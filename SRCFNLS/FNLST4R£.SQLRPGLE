000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200130412      * FNlST4R *----------------------------------------------------*
000300900515      *         - GESTIONE FOGLIO VIAGGIO                            *
000400940915      *--------------------------------------------------------------*
000500900517      * INDICATORI USATI
000600951025      *--------------------------------------------------------------*
000700130315      * 01 = OFF : PER IL VIDEO SONO FNlst4R/51R E NON UN ALTRO
000800951027      * 02 = ON PROTEZIONE CAMPI DEL SFL --> VIDFFV E VIDFLP
000900990514      * 03 = ON DISABILITO F18-RIAPERTURA SE FV DEFLUENZA E LNA ESTERO
001000130419      * 05 = ON filiale gestione in FGW <> da FGVLNP (pera per le poste...)
001100130419      * 06 = IMMETTERE LINEA FINALE DI ARRIVO IN 1° VIDEATA
001200130430      * 08 = visulizzo in sdeconda videata punti di scambio
001300130430      * 10 = se apertura a modo "vecchio", senza copia traino nn abilito
001400130430      *      F6 in prima videata
001500960129      * 15 = SE NON C'E' NIENTE NON VISUALIZZO F8
001600960123      * 16 = ESISTE DESCRIZIONE RITARDO/ANTICIPO: EVIDENZIO IL COMANDO
001700960123      * 17 = INDICA SE C'E' DESCRIZIONE RITARDO NEGLI ARRIVI
001800960123      * 18 = PARTENZA IN ANTICIPO/RITARDO
001900000830      * 22 = USATO IN AGGIUNTA LNA IN SF3
002000000830      * 23 = HI DESCRIZIONE LINEA IN SF3
002100951025      * 28 = EMETTE CAMPO MESSAGGI
002200951026      * 31/34 = DI COMODO
002300011001      * 37 = CHANGE SU VIDIPT
002400110512      * 39 = CHAIN FNFGS
002500010920      * 38 = VIDVIP ERRATA
002600010920      * 39 = CHAIN FNFGW
002700951026      * 40 = VIDHMP ERRATA
002800960318      * 41 = VIDBIS MODIFICATO RISPETTO ALL'ANAGRAFICA
002900960318      * 42 = VIDTTR ERRATA
003000951026      * 43 = ILLUMINO TRAZIONISTA / AUTOMEZZO / E IMPORTO SE MODIFICHE
003100951026      * 44 = VIDDFV ERRATA
003200951026      * 45 = VIDLNA ERRATA
003300951026      * 46 = VIDDTP ERRATA
003400951026      * 47 = VIDPDR ERRATO
003500951026      * 50 = POSIZIONAMENTO SU  VIDFFV
003600951026      * 51 = POSIZIONAMENTO SU  VIDFLP
003700900517      * 52 = RECORD ANNULLATO
003800951026      * 53 = VIDPMB ERRATI
003900951026      * 55 = COPDCN ERRATO
004000910319      * 56 = FOGLIO VIAGGIO CHIUSO
004100951103      * 57 = POSIZIONAMENTO SU VIDTMZ
004200951026      * 58 = VIDIPT ERRATO
004300951103      * 59 = POSIZIONAMENTO SU VIDRTC
004400010621      * 60 = VIDDCN ERRATO
004500000828      * ............. Usati in routine inserimento Terminal ........
004600130502      * 61 = off abilito tasto funzionale F10-ins terminal         .
004700000828      * 62 = ERRORE TERMINAL ARRIVO FINALE                         .
004800000828      * 63 = ERRORE TERMINAL ARRIVO                                .
004900000828      * 64 = SFLEND S06                                            .
005000000828      * 65 = ON SFLCLR S06                                         .
005100000828      * 66 = F6 DI INSERIMENTO TERMINAL + Devo riemettere FOR03    .
005200000828      * 67 = F10 PER EMETTERE VIDEATA DI INSERIMENTO TERMINAL      .
005300000828      * 68 = ERRORE P.O. SCARICO IN SFL TERMINAL ARRIVO            .
005400000828      * 69 = COMODO                                                .
005500000828      * ............................................................
005600000828      * 70 = ON SFLCLR OFF-SFLDSP E SFLDSPCTLM    SFL03
005700951026      * 72/73 = SFLCLR SFLDSP E SFLDSPCTLM        SFL04
005800951025      * 80 = IMMISSIONE   N80 = VARIAZIONE
005900951026      * 81 = PGM RICHIAMATO DA BORDERO'
006000951026      * 82 = OFF - RECORD PRESENTE IN FNFGV46L
006100970526      * 83 = comodo
006200951026      * 89 = ERRORE FOGLIO NON ANNULLABILE PERCHE' PRESENTI BOLLE O SPU
006300951025      * 90 = DEVO RIEMETTERE VIDEATA
006400951026      * 98 = 01 OFF  INTERROGAZIONE  FV
006500951026      * 96 = IMPORTO AL PRIMO ERRORE LA PAGINA DI SFL DA EMETTERE
006600970102      * 98 = INTERROGAZIONE
006700951026      * 99 = PROTEGGO CAMPI DEL VIDEO PERCHE' FV CHIUSO O ANNULLATO
006800900517      *--------------------------------------------------------------*
006900130315     FFNLST4D   CF   E             WORKSTN
007000130315     F                                     SFILE(lst4S03:NRR3)
007100130315     F                                     SFILE(lst4S05:NRR5)
007200130315     F                                     SFILE(lst4S06:NRR6)
007300130412     F                                     SFILE(lst4S08:NRR8)
007400951031     FFNFV401L  UF A E           K DISK
007500951103     FFNFV201L  UF A E           K DISK
007600960130     FFNFV203L  UF   E           K DISK
007700960129     F                                     RENAME(FNFV2000:FNFV2003)
007800951031     FFNFGV01L  UF A E           K DISK
007900940909     F                                     RENAME(FNFGV000:FGV01)
008000961213     FFNFGV02L  IF   E           K DISK    USROPN
008100950901     F                                     RENAME(FNFGV000:FGV02)
008200020805     F                                     prefix(F2V:3)
008300020805     FFNFGV06L  IF   E           K DISK    USROPN
008400020805     F                                     RENAME(FNFGV000:FGV06)
008500020805     F                                     prefix(F6V:3)
008600110512     FFNFGS01L  UF A E           K DISK
008700110512     FFNFGW01L  UF A E           K DISK
008800130318
008900130318     FFNFGT00F  iF   E             DISK
009000020805     FFNBLP79L  IF   E           K DISK
009100070112     FFNBRV02L  UF   E           K DISK
009200951027     FFNFGV46L  UF A E           K DISK    USROPN
009300940912     F                                     RENAME(FNFGV000:FNFGV046)
009400020805     F                                     prefix(FG4:3)
009500951013     F*
009600130604     Ffnfge01l  uF a E           K DISK
009700951013     FAZORG01L  IF   E           K DISK
009800951013     FTABEL00F  IF   E           K DISK
009900951013     F*
010000951013     F*  F I L E   T R A I N I
010100950901     FTNTLT01L  IF   E           K DISK
010200951013     FTNTLR01L  IF   E           K DISK
010300951024     F                                     RENAME(TNTLR000:TNTLR001)
010400951013     FTNTLR02L  IF   E           K DISK
010500951024     FTNTLR03L  IF   E           K DISK
010600951024     F                                     RENAME(TNTLR000:TNTLR003)
010700950901     FTNTLM01L  IF   E           K DISK
010800010920     FTITLL01L  IF   E           K DISK
010900950901     FTNTLZ01L  IF   E           K DISK
011000110511     FTNTLS01L  IF   E           K DISK
011100130424     Faiata01l  IF   E           K DISK
011200130424     Faiata02l  IF   E           K DISK    RENAME(aiata000:aiata002)
011300130424     Faiatm01l  IF   E           K DISK
011400130424
011500130424     FTNPSC01L  IF   E           K DISK
011600970102     FAZCAE02L  IF   E           K DISK
011700970102     F                                     RENAME(AZCAE000:AZCAE02)
011800970102     FAZCAE01L  IF   E           K DISK
011900970102     F                                     RENAME(AZCAE000:AZCAE01)
012000951026     FAZCAE03L  IF   E           K DISK    USROPN
012100030704     FAZCLN01L  IF   E           K DISK
012200080919     ftnfvs01l  if   e           k disk    extfile(Wfvs01) usropn
012300080919     ftntsr04l  if   e           k disk    extfile(wtsr04) usropn
012400080919     ftntst01l  if   e           k disk    extfile(wtst01) usropn
012500130510
012600130510     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
012700020805      *
012800020805      * DEFINIZIONE SCHIERE
012900020805      *
013000080228     D L1              S              3  0 DIM(30)
013100080228     d
013200080228     D LAS             S              3    DIM(301)
013300000420     D LAB             S              3    DIM(301)
013400970102     D*
013500970102     D*
013600000420     D CTL             S              3    DIM(301)
013700000420     D CFS             S              3    DIM(301)
013800000420     D FE0             S              3    DIM(301)
013900970102     D*
014000130606     d pffv            s              3    dim(300)
014100130606     d pflp            s              3    dim(300)
014200000420     D SFV             S              3    DIM(299)
014300000420     D SLP             S              3    DIM(299)
014400000420     D TFV             S              3    DIM(299)
014500000420     D TLP             S              3    DIM(299)
014600950901     D FLA             S              3  0 DIM(300)
014700950901     D TAA             S              3  0 DIM(300)
014800951002     D GSE             S              1    DIM(7)
014900951016     D NFP             S              3  0 DIM(2)
015000951016     D NFA             S              3  0 DIM(2)
015100951024     D K78             S              1    DIM(78)
015200960301     D K35             S              1    DIM(78)
015300951024     D K15             S              1    DIM(15)
015400970102     D*
015500970102     D TFE             S              3  0 DIM(50)
015600100616     D TFEDef          S              3  0 DIM(50)
015700970102     D TFEL            S              3  0 DIM(50)
015800970102     D TFE2            S              3  0 DIM(50)
015900970103     D*
016000970103     D AGG             S              3    DIM(53)
016100970103     D VAR             S              3    DIM(53)
016200970103     D ELM             S              3    DIM(53)
016300960302     D*
016400130729     D MSG             S             78    DIM(114) CTDATA PERRCD(1)
016500110530     D SBI             S             35    DIM(25) CTDATA PERRCD(1)
016600951002     D GFV             S              1    DIM(20) CTDATA PERRCD(20)
016700951026     D TES             S             36    DIM(3) CTDATA PERRCD(1)
016800960302     D GIO             S              9    DIM(7) CTDATA PERRCD(1)
016900130412     D* numeri piombi PER CONTROLLI DI RECORD DOPPIO
017000130417     D PIO2            S              8    DIM(500)
017100020213
017200020805      *
017300020805      * DEFINIZIONE CAMPI
017400020805      *
017500080919     d Wfvs01          s             21    inz('GAITRAGRU /TNFVS01L')
017600080919     d Wtsr04          s             21    inz('GAITRAGRU /TNTSR04L')
017700080919     d Wtst01          s             21    inz('GAITRAGRU /TNTST01L')
017800020805     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
017900020805     D KTLPDR          S                   LIKE(TLZPDR)
018000020805     D LINFE0          S                   LIKE(D55TFA)
018100020805     D LNAFE0          S                   LIKE(D55TFA)
018200020805     D SAVHMP          S                   LIKE(FGVHMP)
018300020805     D SAVDTP          S                   LIKE(FGVDTP)
018400020805     D AMGDTP          S                   LIKE(FGVDTP)
018500020805     D AMGDTT          S                   LIKE(FGVDTP)
018600020805     D AMGDFV          S                   LIKE(FGVDFV)
018700020805     D WSDFV           S                   LIKE(FGVDFV)
018800020805     D CODFIL          S                   LIKE(FGVLNA)
018900020805     D WLNAF           S                   LIKE(FGVLNA)
019000020805     D KTMZ            S                   LIKE(TLTTMZ)
019100020805     D TRNTFA          S                   LIKE(FGVLNA)
019200020805     D KTFP            S                   LIKE(FGVLNA)
019300020805     D KTFA            S                   LIKE(FGVLNA)
019400020805     D LNAPRE          S                   LIKE(FGVLNA)
019500020805     D WDATE           S                   LIKE(FGVDFV)
019600020805     D DATEU           S                   LIKE(FGVDFV)
019700020805     D WFMFES          S                   LIKE(TLRTFM)
019800020805     D WDDE            S                   LIKE(TLRDDE)
019900020805     D WANDDE          S                   LIKE(TLRDDE)
020000150902     D WANtfa          S                   LIKE(TLttfa)
020100020805     D WRIDDE          S                   LIKE(TLRDDE)
020200020805     D KANN            S                   LIKE(CLNANN)
020300020805     D KMES            S                   LIKE(CLNMES)
020400020805     D KNPG            S                   LIKE(BRVNPG)
020500020805     D KFGS            S                   LIKE(BRVFGS)
020600020805     D WNFV            S                   LIKE(BRVNFV)
020700020805     D COD             S                   LIKE(TBLCOD)
020800020805     D KEY             S                   LIKE(TBLKEY)
020900020805     D WTFP            S                   LIKE(CAETFP)
021000020805     D WTFA            S                   LIKE(CAETFA)
021100020805     D KPDR            S                   LIKE(TLLPDR)
021200130424     D savpdr          S                   LIKE(vidPDR)
021300130424     D Kdcn            S                   LIKE(fgtdcn)
021400130424     D savdcn          S                   LIKE(fgtdcn)
021500130424     D savtrm          S                   LIKE(vidtrm)
021600020805     D WTLPDR          S                   LIKE(TLLPDR)
021700110511     D savpsc          S                   LIKE(tlspsc)
021800110531     D oldpsc          S                   LIKE(tlspsc)
021900110530     D savlnar         S                   LIKE(vidlnar)
022000110511     D savtrns         S                   LIKE(tlstrns)
022100020805     D ZERO            S              3  0 INZ
022200110513     D Ctfp            S                   LIKE(tlttfp)
022300110513     D Ctfa            S                   LIKE(tlttfa)
022400110513     D Cdde            S                   LIKE(tltdde)
022500110513     D Cdsc            S                   LIKE(tltdsc)
022600130424     d AlfaSOC         s              3
022700130424     d SocTraini       s              3  0
022800110513     d
022900110513     d wrkgetlista     s           4096    varying
023000020805     D*
023100020805     D* Salvataggio per riprovare i controlli forzati
023200020805     D SAVTTR          S                   LIKE(FGVTTR)
023300020805     D SAVBIS          S                   LIKE(TLTBIS)
023400020805     D SAVDFV          S                   LIKE(FGVDFV)
023500130419     D SAVTRN          S                   LIKE(tltTRN)
023600130419     D COPTRN          S                   LIKE(tltTRN)
023700020805     D SAVIPT          S                   LIKE(VIDIPT)
023800020805     D SAVVIP          S                   LIKE(VIDVIP)
023900020805     D SAVLNA          S                   LIKE(VIDLNA)
024000020805     D* Campi dal file TRAINI: mi servono per vari controlli
024100020805     D TRNUTF          S                   LIKE(§TVUTF)
024200020805     D TRNTMZ          S                   LIKE(FGVTMZ)
024300020805     D TRNVIP          S                   LIKE(FGWVIP)
024400020805     D TRNIPT          S                   LIKE(FGWIPT)
024500020805     D TRNPDR          S                   LIKE(VIDPDR)
024600020805     D* DFV in GG --> mi serve per trovare la Data Traino
024700020805     D WGGDFV          S                   LIKE(G02TGI)
024800020805     D* Data Traino
024900020805     D WDTN            S                   LIKE(TLTDDE)
025000020805     D* Campi vari di Comodo
025100020805     D*** Wlnp            s                   like(FGWlnp) inz
025200020805     D WNFM            S                   LIKE(TLRNFM)
025300020805     D WTFM            S                   LIKE(TLRTFM)
025400020805     D WFIL            S                   LIKE(TLRFIL)
025500020805     D KTRC            S                   LIKE(FV4TRC)
025600020805     D KEPA            S                   LIKE(FV4EPA)
025700020805     D WTRSN1          S                   LIKE(§3BTRS)
025800020805     D WTRSN2          S                   LIKE(§3BTRS)
025900020805     D WTRSC1          S                   LIKE(§3BTRS)
026000020805     D WTRSC2          S                   LIKE(§3BTRS)
026100020805     D WTRSD1          S                   LIKE(§3BTRS)
026200020805     D WTRSD2          S                   LIKE(§3BTRS)
026300020805     D WTRSL1          S                   LIKE(§3BTRS)
026400020805     D WTRSL2          S                   LIKE(§3BTRS)
026500130424     D WTRSP1          S                   LIKE(§3BTRS)
026600130424     D WTRSP2          S                   LIKE(§3BTRS)
026700020805     D WTRAS1          S                   LIKE(§3BTRS)
026800020805     D WTRAS2          S                   LIKE(§3BTRS)
026900020805     D SAVRTC          S                   LIKE(VIDRTC)
027000020805     D KTDH            S                   LIKE(FV2TDH)
027100020805     D WINNFV          S                   LIKE(FGVNFV)
027200020805     D DSTVS           S                   LIKE(DSTV)
027300020805     D SAVFGS          S                   LIKE(VIDFGS)
027400020805     D WGWFGS          S                   LIKE(FGWFGS)
027500020805     D COMLNA          S                   LIKE(BLPLNA)
027600020805      *
027700020805     D SAVDIV          S                   LIKE(VIDVIP)
027800020805     D SAWVIP          S                   LIKE(VIDVIP)
027900130510     D
028000130510     d O_testo         s            132    inz
028100020805      *
028200020213     d esteroedpd      s              1
028300130510     d winviamail      s              1
028400020213     d okestero        s              1
028500050706     d iniziopro       s              3
028600050706
028700050706     d wgiorno         s              2  0
028800030701     d wdtaeur         s               d   datfmt(*eur)
028900030701     d wdtaiso         s               d   datfmt(*iso)
029000100616     d wDEFmeno1       s              8  0
029100100616     d wdtameno1       s              8  0
029200050706     d wdtameno2       s              8  0
029300050706     d woggimeno2      s              8  0
029400030630     d wdtapiu1        s              8  0
029500080919     d Dm30            s              8  0
029600080910     d wfat            s              1
029700110512     d Indx            s              3  0
029800130415     d parx            s              3  0
029900130510     d XI              s              3  0
030000110530     d EmisD06         s              1
030100130424     d WforzD1         s              1
030200130424     d WforzM1         s              1
030300130510     d desBIS          s              3
030400130606
030500130606     d wlna            s                   like(fgvlna)
030600130606     d wsca            s                   like(fgvlna)
030700130606     d wtroF           s              1
030800130606     d wdir            s              1
030900130606     d x_dtp           s                   like(fgvdtp)
031000130606     d x_hmp           s                   like(fgvhmp)
031100130606     d a_dtp           s                   like(fgvdtp)
031200130606     d a_hmp           s                   like(fgvhmp)
031300130607     d a_sca           s                   like(fgvlna)
031400130606     d wdtp            s                   like(fgvdtp)
031500130606     d whmp            s                   like(fgvhmp)
031600130606     d iy              s              3  0
031700130606     d iw              s              3  0
031800130606     d ij              s              3  0
031900020213
032000020805      *
032100020805      * STRUTTURE DATI DEFINITE ESTERNAMENTE
032200020805      *
032300130424     d ds3I          e ds                  inz
032400010920     D DSCV          E DS
032500960208     D DSTC          E DS
032600960208     D DSTV          E DS
032700951031     D DS3B          E DS
032800960122     D DS1Y          E DS
032900960207     D OG148         E DS
033000000405     D OG143         E DS
033100960207     D KPJBA         E DS
033200900517     D CNCR80        E DS
033300020805     D UT§DSE0F      E DS
033400010927      *
033500010927     D DGED          E DS
033600020805     D TIBS02DS      E DS
033700130412     d
033800130412     D Dpscflo       E DS
033900020805      *
034000020805      * DS per TRUL06R - CARICAMENTO £X
034100020805     D TRUL06DS      E DS
034200020805     D  LIN                    1     90  0
034300020805     D                                     DIM(30)
034400020805      *
034500020805      * DS per TNTL17R - Interrogazione Traini
034600020805     D TNTL00DS      E DS
034700110512      * DS per TNTLP0R - rcierca punti scambio
034800110512     D TNTLP0DS      E DS
034900020805      *
035000020805      * DS per FNLV15R - Gestione Interrogazione Ritardo Partenza
035100020805     D FNLV15DS      E DS
035200020805      *
035300020805      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
035400020805     D FNLV55DS      E DS
035500020805      *
035600020805      * DS per TNTL35R - Calcolo Data Ora Tecniche
035700020805     D TNTL35DS      E DS
035800020805      *
035900020805     D TRUL33DS      e ds                  inz
036000020805     D  I33cnu       e                     inz(004)
036100020805     D  I33num       e                     inz(1)
036200010927      *
036300130510      // - Tabella "MRA" = Bart-Mailing -
036400130510     d dMRADAN       e ds                  inz
036500130510     d
036600130510      // - Status
036700130510     d Psds           sds
036800130510     d   SDSpgm          *proc
036900130510     d   JobUser             254    263
037000130510
037100130510      // - Campi per QCMDEXC
037200130510     d Qcmd            s            512    inz
037300130510     d
037400130510      // - Esecuzione comando di sistema
037500130510     d qCmdExc         pr                  extpgm('QCMDEXC')
037600130510     d  Qcmd                        512    const  options(*varsize)
037700130510     d  Qlen                         15  5 const
037800130510
037900020805      * STRUTTURE DATI DEFINITE INTERNAMENTE
038000020805      *
038100910128     D                 DS
038200000420     D  FFV                    1    897
038300000420     D                                     DIM(299)
038400910405    +D  FGVFFV                 1    240
038500910128    +D  FGVFF2               241    447
038600000420    +D  W1FF3                448    687
038700000420    +D  W1FF4                688    897
038800020805      *
038900900522     D                 DS
039000000420    +D  FLP                    1    897
039100000420     D                                     DIM(299)
039200910405    +D  FGVFLP                 1    240
039300910128    +D  FGVFL2               241    447
039400000420    +D  W1FL3                448    687
039500000420    +D  W1FL4                688    897
039600970102     D                 DS
039700000420     D  F2V                    1    900
039800000420     D                                     DIM(300)
039900970102    +D  F2VLNA                 1      3  0
040000970102    +D  F2VFFV                 4    243
040100970102    +D  F2VFF2               244    450
040200000420    +D  W2FF3                451    690
040300000420    +D  W2FF4                691    900
040400130606
040500130610     D                 DS
040600130606     D  F2P                    1    900
040700130606     D                                     DIM(300)
040800130606    +D  F2VFLP                 4    243
040900130606    +D  F2VFL2               244    450
041000130606    +D  W2FL3                451    690
041100130606    +D  W2FL4                691    900
041200970102     D                 DS
041300020805     D  F6V                    1    900
041400020805     D                                     DIM(300)
041500020805    +D  F6VLNA                 1      3  0
041600020805    +D  F6VFFV                 4    243
041700020805    +D  F6VFF2               244    450
041800020805    +D  W6FF3                451    690
041900020805    +D  W6FF4                691    900
042000020805      *
042100020805      *****
042200020805      * Parametri ricevuti
042300130315     D fnlst4ds        DS
042400130315     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
042500020805     D*          B - DA BORDERO'
042600130315     D*          A - apertura foglio
042700020805     D*          C - DA CHIUSURA FV SINGOLA
042800020805     D*          I - DA CHIUSURA DA IMP
042900020805     D*          N - DA INTERROGAZIONE  FV
043000130419     D*          M - Manutenzione FGV
043100130419     D*          P - Manutenzione FGV con videata piombi
043200130502     d
043300130430     D* CAMPI INPUT OBBLIGATORI:
043400130430     D*  GES / FGS
043500130430     d
043600130430     D* CAMPI INPUT PER MANUTENZIONE :
043700130430     D*  NFV
043800130430     D*
043900130430     D* CAMPI INPUT SOLO PER APERTURA :
044000130430     d* DFV /OLD / TRN / NRRP / PDR
044100130430     D
044200130603     d
044300130603     d* solo per un po' N=nuova chiusura
044400130603     D  ilst4NEWC              2      2
044500130319     D  ilst4DFV               3     10
044600130430     D
044700130319     D  ilst4nfv              11     15
044800130315     D  IlsT4GES              16     16
044900130319     D* Se MSG pieno --> c'è stato ERRORE
045000130319     D  olst4msg              17     94
045100130429     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
045200130429     D  iolst4FOR             95     97
045300130430     D
045400130429     D  IlsT4OLD              98     98
045500130430     D* OLST4F12 = 'S'    --> premuto F12
045600130430     D  olst4F12             102    102
045700130430     d
045800130315     D  ilst4fgs             103    105
045900130315     D  ilst4TRN             106    112  0
046000130319     D  ilst4NRRP            113    125
046100130319     D* se non immessa il numero relativo record della proposta
046200130319     d*  si può immettere a modo vecchio il trazionista
046300130319     D  ilst4PDR             126    132
046400130424      ****
046500130424     d* Ricerca AUTISTI di un trazionista
046600130424     D tntl83ds        DS
046700130424     d  i83soc                        3s 0
046800130424     d  i83ksc                        7s 0
046900130424     d* i83mod = 1 --> modalità scelta conducente
047000130424     d  i83mod                        1
047100130424     d* data di riferimento alla quale caricare per ora non usata
047200130424     d  i83Drf                        8
047300130424     d  o83_AUT                      35
047400130424     d  o83_tel                      12
047500130424     d
047600130424     d*****
047700130424     d* Ricerca TARGHE  di un trazionista
047800130424     D tntl84ds        DS
047900130424     d  i84soc                        3s 0
048000130424     d  i84ksc                        7s 0
048100130424     d* i84mod = 1 --> modalità scelta conducente
048200130424     d  i84mod                        1
048300130424     d* data di riferimento alla quale caricare per ora non usata
048400130424     d  i84Drf                        8
048500130424     d  o84_TRM                      10
048600130424     d
048700020805      *****
048800131105      * Parametri per la chiusura FV             - FNLSU2R -
048900951026     D PARAM2          DS                  INZ
049000020805     D* I seguenti 3 campi li passo vuoti perchè servono solo
049100020805     D*  quando devo creare le spunte in automatico nella chiusura FV
049200020805     D*  (solo se chiusura da IMP)
049300951026     D  NPG1                   1      1  0 INZ
049400951026     D  NFV1                   2      4P 0 INZ
049500951026     D  DFV1                   5      9P 0 INZ
049600960209     D  PA2AGS                10     10
049700951026     D  PA2FGV                11     15
049800951026     D  PA2NUM                17     17  0 INZ
049900020805     D* Flag passato dal FNLS93R per dire se file allocati
050000960129     D  WLCK                  18     18    INZ
050100020805     D* Flag tipo chiamata M=Solo invio FV
050200960129     D  PA2FLG                19     19
050300080228     D  L1ela                 78    137P 0
050400951026     D                                     DIM(30)
050500951026     D  PA2RSU               138    157
050600951026     D  PA2FEL               158    160  0
050700020805     D* Vuoto: serve per chiusura IMP
050800951026     D  FGS1                 164    166  0 INZ
050900020805     D* Filiale elaboratore SIMFEL ha CML
051000991020     D  PA2CML               167    167
051100010911     D  PA2AGP               168    168
051200040121     D  PA2spv               169    169
051300040121     D  PA2spp               170    170
051400130603     D  PA2CCM               171    177
051500130603     D  PA2barcode           178    178
051600020805      *****
051700020805      * Parametri per la trasmissione FV in sede - FNLSA4R -
051800960129     D PARAM4          DS
051900051121     D  wainv                  5      7    inz('046')
052000051121     D  PA4MBD                 1     10
052100960129     D  PA4FEL                11     13  0
052200020805     D* Il campo PA4ALL è utilizzato nel pgm FNLSA3R
052300960129     D  PA4ALL                14     14    INZ
052400020805     D* Tipo lancio programma ("C" / "L" / " ")
052500960129     D  PA4TLA                15     15
052600020805     D* PA4FLG = "M" ---> richiamato dalla gestione F.Viaggio
052700960129     D  PA4FLG                16     16    INZ('M')
052800020805     D* Il campo PA4LNP deve essere passato solo se pgm richiamato
052900020805     D*   dalla gestione F.Viaggio
053000960129     D  PA4LNP                17     19  0
053100960129     D*
053200020805      *****
053300020805      * Parametri per la ricerca trazionisti     - TRUL16R -
053400951030     D PARAM3          DS
053500020805     D* Tipologia T_=TRAZIONISTA
053600000807     D  PA3TIP                 1      2
053700020805     D* Ragione sociale trazionista
053800000807     D  PA3RSC                 3     37
053900020805     D* Sistema inform. trazionista
054000000807     D  PA3CSF                38     40
054100020805     D* Codice trazionista
054200000807     D  PA3PDR                41     47  0
054300020805     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
054400020805     D*   non esistono records per la chiave alfabetica richiesta
054500000807     D  PA3FLG                48     48
054600070423     d******
054700070423     D PARAMa3         DS
054800070423     D  PA3NFV                 1      5
054900070423     D* pa3tfp deve contenere il terminal di partenza per poter elaborare e
055000070423     d* trasmettere le bolle per terminal di partenza
055100070423     d* se non passato un valore > '000' il programma elabora e trasmette
055200070423     d* TUTTE le bolle presenti nel file fnblp46f (come faceva prima)
055300070423     D  PA3tfp                 6      8
055400111123     D  PA3num                 9      9
055500130510     d
055600130510      // - Comando di override al PrtF
055700130510     d C_CmdOvrPrtF    c                   const('OVRPRTF +
055800130510     d                                           file(PRTEMAIL) +
055900130510     d                                           pagesize(66 132) +
056000130510     d                                           lpi(6) cpi(10) +
056100130510     d                                           ovrscope(*actgrpdfn) +
056200130510     d                                           ')
056300130510     d C_CmdDltOvr     c                   const('DLTOVR +
056400130510     d                                            file(PRTEMAIL) +
056500130510     d                                            lvl(*actgrpdfn)')
056600130510      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
056700130510     d TRTCM1ds      e ds                  inz
056800130510      //   ·§CM1mitt = Indirizzo e-mail del mittente
056900130513     d   §CM1mitt    e                     inz('Rop001')
057000130510      //   ·§CM1dst  = Indirizzo e-mail del destinatario
057100130510     d   §CM1dst     e                     inz('elisa.sanfelici@brt.it')
057200130510      //   ·§CM1tips = Tipo lettera via e-mail:
057300130510      //    "LET" = testo allegato in corpo con logo
057400130510      //            (richiede righe libere iniziali per il logo)
057500130510      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
057600130510      //            (non consente né UNDERLINE né HIGHLIGHT)
057700130510     d   §CM1tips    e                     inz('COI')
057800130510      //   ·§CM1po   = Filiale
057900130510     d   §CM1po      e                     inz('046')
058000130510      //   ·§CM1var  = Oggetto e-mail
058100130510     d   §CM1var     e                     inz('*OBJM*+
058200130510     d                                     Richiesta di autorizzazione +
058300130510     d                                     al carico')
058400130510      //   ·§CM1sts  = Stato
058500130510     d   §CM1sts     e                     inz(*off)
058600130510      //   ·§CM1sts  = Id processo
058700130510     d   §CM1idp     e                     inz('2')
058800130510
058900020805      *****
059000900515     D WLBDAT          DS
059100940909     D  G02DAT                 1      8  0
059200940909     D  G02INV                 9     16  0
059300940909     D  G02ERR                17     17
059400940909     D  G02TGI                18     22  0
059500910809     D WGIDAT          DS
059600940909     D  GI8DAT                 1      8  0
059700940909     D  GI8INV                 9     16  0
059800940909     D  GI8TGI                17     21  0
059900960122     D                 DS
060000960122     D  CLNPOM                 1     31
060100960122     D  POM                    1     31
060200960122     D                                     DIM(31)
060300040204     D                 DS
060400040204     D  CLNMAT                 1     31
060500040204     D  MAT                    1     31
060600040204     D                                     DIM(31)
060700020805      *
060800900515     D                 DS
060900900515     D  ORA                    1      2  0
061000900515     D  MIN                    3      4  0
061100910809     D  HM                     1      4  0
061200951115     D                 DS
061300951115     D  GG                     1      2  0
061400951115     D  MM                     3      4  0
061500951115     D  AA                     5      8  0
061600951115     D  WDATA                  1      8  0
061700000828     D WDAT8A          DS
061800000828     D  WDAT4A                 1      4
061900000828     D  WDAT2M                 5      6
062000000828     D  WDAT2G                 7      8
062100020805     D*
062200110531     D DStls         E DS                  EXTNAME(tntls00F)
062300110531     D DSFGW         E DS                  EXTNAME(FNFGW00F)
062400040121     D OG150         E DS
062500000605     D SVFGW           DS           928
062600951013     C*---------------------------------------------------------------*
062700130315     c
062800900515     C                   EXSR      PULIZ
062900130319     C*
063000130315     C                   MOVE      ilst4nfv      VIDNFV
063100130315     C                   MOVE      ilst4fgs      VIDFGS
063200130412     C* imposto la data foglio viaggio
063300130319     C                   MOVE      ilst4DFV      G02INV
063400130319     C                   MOVE      *ZEROS        G02DAT
063500130319     C                   MOVEL     '3'           G02ERR
063600130319     C                   CALL      'XSRDA8'
063700130319     C                   PARM                    WLBDAT
063800130319     C                   Z-ADD     G02DAT        VIDDFV
063900951026     C*
064000130315     C* Se richiamo per apertura e c'e' traino --> controllo
064100130315     c*
064200130412    0c                   if        ilst4ges='A'
064300130319
064400130319    1c                   if        ilst4trn>0
064500130315     C                   MOVE      ilst4TRN      copTRN
064600130319     C                   MOVE      ilst4TRN      vidtrn
064700130510     c
064800130430     c                   else
064900130430     c* non devo abolitare F6 in prima videata
065000130430    2c                   if        ilst4old='S'
065100130430     c                   seton                                        10
065200130430    2c                   endif
065300130430    1c                   endif
065400130510     c
065500130510     c* Inserimento passato codice aut controllo
065600130510    2c                   if        ilst4pdr>*zeros
065700130510     c                   move      ilst4pdr      vidpdr
065800130510     c                   exsr      Ctrpar
065900130510    2c                   endif
066000130319     c
066100130315     C                   EXSR      CTRINS
066200130319     c
066300130424    1c                   if        vidmsg<>*blanks
066400130319     c                   movel     vidmsg        olst4msg
066500130412    1c                   endif
066600130412     c
066700130315     C   90              GOTO      fine
066800130412     c
066900130419     c* emetto videata dei piombi  prima di proporre la videata di iserimento
067000130502     c                   if        vidttr=' ' or vidttr='S' or
067100130502     c                              vidttr='H'
067200130419     c                   exsr      GesD09
067300130502     c                   endif
067400130412     c
067500130412   x0c                   else
067600130315    c
067700951026     C* CONTROLLO NUMERO DI FV IMMESSO
067800130319   1ac                   if        vidnfv >0
067900951026     C                   EXSR      CTRVAR
068000130426     c
068100130429    1c                   if        vidmsg<>*blanks
068200130429     c                   movel     vidmsg        olst4msg
068300130429    1c                   endif
068400130429
068500130424     C   90              GOTO      FINE
068600951026     C*
068700951026     C* RIEMPO CAMPI VIDEATA
068800951023     C                   EXSR      RIEMPI
068900130429     c
069000130419     c* emetto videata dei piombi  prima di proporre la videata di iserimento
069100130429     c*  solo se il tipo traino lo prevede
069200130429     c                   if        ilst4ges='P'  and
069300130429     c                             (fgvttr=' ' or fgvttr='S' or
069400130429     c                              fgvttr='H')
069500130419     c                   exsr      GesD09
069600130419     c                   endif
069700130412     c
069800130319   1aC                   ENDIF
069900130319    1C                   ENDIF
070000130412     c
070100130516     c* eseguo posizionamentio cursore
070200130516     c                   if        *in55 or *in54
070300130516     c                   seton                                        90
070400130516     c                   endif
070500130516     c
070600900515      *          =============
070700951024     C     FOR02         TAG
070800900515      *          =============
070900130315     C                   WRITE     lst4T01
071000130315     C                   EXFMT     lst4D02
071100960126     C                   MOVEL     '2'           WRECOR            1
071200951027     C                   SETOFF                                       289089
071300960123     C*
071400960126     C* F8 - INTERROGA ORARI ARRIVO
071500960126    1C     *INKH         IFEQ      *ON
071600960123     C                   EXSR      INTORA
071700960123     C                   GOTO      FOR02
071800960126    1C                   ENDIF
071900130415     C* F13- sfl piombi
072000130415    1C     *INKM         IFEQ      *ON
072100130415     C                   EXSR      gesd09
072200130415     C                   GOTO      FOR02
072300130415    1C                   ENDIF
072400951108     C*
072500951108     C* F12 - RITORNO / F16 - ANNULLAMENTO
072600951108    1C     *INKL         IFEQ      *ON
072700951108     C     *INKQ         OREQ      *ON
072800960123     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO  PARTENZA
072900960123     C                   MOVEL     'P'           KEPA
073000960123     C                   MOVEL     'R'           KTRC
073100960123     C                   CLEAR                   KTFA
073200951108     C                   EXSR      CTRDRA
073300951108     C*
073400951107     C* SE IMMESSA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12
073500951108     C*   O F16 SE NON E' GIA' STATA IMMESSA UNA CAUSALE
073600951108    2C     W001A         IFNE      ' '
073700951108    3C     SAVRTC        IFEQ      ' '
073800951109    4C     VIDRTC        IFEQ      ' '
073900951109     C                   MOVEL     MSG(57)       VIDMSG
074000951109     C                   SETON                                        2859
074100951109     C                   GOTO      FOR02
074200951109   X4C                   ELSE
074300951107     C                   MOVEL     MSG(58)       VIDMSG
074400951108     C                   SETON                                        2859
074500951107     C                   GOTO      FOR02
074600951109    4C                   ENDIF
074700951108    3C                   ENDIF
074800951108     C*
074900951108   X2C                   ELSE
075000951108     C* SE ELIMINATA DESCRIZIONE RITARDO/ANTICIPO NON POSSO PREMERE F12
075100951108     C*   O F16 SE LA CAUSALE CHE E' GIA' STATA IMMESSA E' "G-GENERICA"
075200951108    3C     SAVRTC        IFEQ      'G'
075300951108     C                   MOVEL     MSG(59)       VIDMSG
075400951108     C                   SETON                                        2859
075500951108     C                   GOTO      FOR02
075600951108    3C                   ENDIF
075700951108    2C                   ENDIF
075800960123     C*
075900960123     C* F12 - RITORNO
076000960126     C* SOTTOMETTO LA TRASMISSIONE PER FV4
076100960126     C   KLWAGG          IFNE      ' '
076200960126     C                   EXSR      TRAVAR
076300960126     C                   ENDIF
076400130430
076500130430     C                   if        *inkl
076600130430     C                   MOVEL     'S'           oLST4F12
076700960123     C                   GOTO      FINE
076800960123    2C                   ENDIF
076900960123     C*
077000960123      *
077100960123      * F16 - ANNULLA
077200960123      * NON SI PUO' ANNULLARE SE ESISTONO BOLLE O SPUNTE
077300960126    2C   KQ              DO
077400020805     C     K02BLP79      SETLL     FNBLP000                               89
077500960123     C* ESISTONO BOLLE:  IMPOSSIBILE L'ANNULLAMENTO
077600960123     C   89              MOVEL     MSG(6)        VIDMSG
077700960123     C   89              SETON                                        28
077800960123     C   89              GOTO      FOR02
077900960123     C* ESISTONO SPUNTE: IMPOSSIBILE L'ANNULLAMENTO
078000000202     C                   Z-ADD     VIDNFV        WNFV
078100070112     C     KBRV1         SETLL     FNBRV02L                               89
078200990514     C   89              MOVEL     MSG(3)        VIDMSG
078300960123     C   89              SETON                                        28
078400960123     C   89              GOTO      FOR02
078500120206     c
078600120206     c* Foglio confermato in sede: impossibile annullamento
078700120206     c     kfvs          chain     tntsr04l
078800120206     c                   if        %found(tntsr04l)
078900120206     C                   MOVEL     MSG(105)      VIDMSG
079000120206     C                   SETON                                        2889
079100120206     C                   GOTO      FOR02
079200120206     c                   endif
079300960123     C*
079400020805     C     K02FGV01      CHAIN     FNFGV01L                           80
079500960123     C                   MOVE      '*'           FGVATB
079600960123     C                   UPDATE    FGV01
079700110530     C     K02FGV01      CHAIN     FNFGs01L                           80
079800110530     C                   MOVE      '*'           FGsATB
079900110811     C  n80              UPDATE    fnfgs000
080000110530     c
080100960123     C* VERIFICO SE E' DA MANDARE IN SEDE
080200960123     C                   EXSR      CTRFVA
080300130607     c* aggiorno file di appoggio per stampa etichette
080400130607     c                   exsr      sr_fnfge
080500960123     C*
080600960123     C   20              GOTO      FINE
080700960126    2C                   ENDDO
080800960123     C*
080900951108    1C                   ENDIF
081000951107     C*
081100951023      * F5 - RIATTIVA
081200960123    1C   KE              DO
081300951123     C* SE HA UN NUMERO TRAINO, VERIFICO
081400951123     C     VIDTRN        IFGT      0
081500951123     C                   Z-ADD     FGVDFV        AMGDFV
081600951123     C                   CLEAR                   WFORZ2
081700951123     C* SE LO STO RIPRISTINANDO CONTROLLO SE DOPPIO
081800951123     C                   EXSR      CTRDOP
081900951123     C   90              CLEAR                   VIDTRN
082000951123     C                   ENDIF
082100951123     C*
082200020805     C     K02FGV01      CHAIN     FNFGV01L                           80
082300900516     C                   MOVE      ' '           FGVATB
082400951123     C     VIDTRN        IFEQ      0
082500951123     C                   CLEAR                   FGVTRN
082600951123     C                   CLEAR                   FGVFRQ
082700951123     C                   ENDIF
082800950905     C                   UPDATE    FGV01
082900130712     C     K02FGV01      CHAIN     FNFGs01L                           80
083000130712     C                   MOVE      ' '           FGsATB
083100130712     C  n80              UPDATE    fnfgs000
083200130607
083300130607     c                   exsr      sr_fnfge
083400931116     C*
083500020805     C     K02FGV01      CHAIN     FNFGV01L                           80
083600020805     c                   eval      FGWnfv = VIDnfv
083700020805     c                   eval      FGWlnp = SIMfel
083800020805     c                   exsr      CTR_FGW
083900000427     C                   MOVEL     FGWFF3        W1FF3
084000000427     C                   MOVEL     FGWFF4        W1FF4
084100000427     C                   MOVEL     FGWFL3        W1FL3
084200000427     C                   MOVEL     FGWFL4        W1FL4
084300951123     C                   SETOFF                                       529902
084400951123     C                   EXSR      CARSFL
084500951123     C                   GOTO      FOR02
084600960123    1C                   ENDDO
084700910702     C*
084800951023     C* F18 - RIAPERTURA FOGLIO VIAGGIO
084900960123    1C   KS              DO
085000050706     c* Posso riaprire solo i fogli di oggi - 2 gg lavorativi,
085100050706     c* EDP sempre
085200050706     c                   if        amgdfv<woggimeno2  and iniziopro<>'EDP'
085300050706     C                   MOVEL     MSG(90)       VIDMSG
085400050706     C                   SETON                                        28
085500050706     C                   GOTO      FOR02
085600050706     c                   endif
085700050706     c
085800050706     c*
085900020805     C     K02FGV01      chain     FNFGV01L                           80
086000910702     C                   MOVE      ' '           FGVFCF
086100950905     C                   UPDATE    FGV01
086200910702     C   20              GOTO      FINE
086300960123    1C                   END
086400960126     C**
086500960126     C* F21 - DESCRIZIONE RITARDO/ANTICIPO
086600960126    1C   KV              DO
086700020805     C                   CLEAR                   FNLV15DS
086800960126     C* SE FV ANNULLATO UTILIZZO IL PGM DI DESCRIZIONE RITARDO/ANTICIPO
086900960126     C*   COME INTERROGAZIONE
087000960126     C     *IN52         IFEQ      *OFF
087100130315     C     ilst4ges      ANDNE     'N'
087200960126     C                   MOVEL     'M'           D15TUP
087300960126     C                   ELSE
087400960126     C                   MOVEL     'I'           D15TUP
087500960126     C                   ENDIF
087600960126     C*
087700960126     C* SE PGM RICHIAMATO RICHIAMO CON 'L'
087800960126     C   20              MOVEL     'L'           D15TLA
087900960126     C                   MOVEL     'P'           D15EPA
088000960126     C                   MOVEL     'R'           D15TRC
088100960126     C                   MOVEL     VIDRTC        D15RTC
088200130419     C                   MOVEL     VIDfgs        D15LNP
088300960126     C                   MOVEL     VIDNFV        D15NFV
088400960126     C                   MOVEL     AMGDFV        D15DFV
088500960126     C                   MOVEL     VIDTRN        D15TRN
088600960222     C                   MOVEL     VIDTTR        D15TTR
088700960126     C*
088800020805     C                   MOVEL     FNLV15DS      KPJBU
088900960126     C                   CALL      'FNLV15R'
089000960126     C                   PARM                    KPJBA
089100020805     C                   MOVEL     KPJBU         FNLV15DS
089200960126     C*
089300960126     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
089400960126     C*   EVIDENZIO IL COMANDO
089500960126     C                   SETOFF                                       16
089600960126     C                   MOVEL     'P'           KEPA
089700960126     C                   MOVEL     'R'           KTRC
089800960126     C                   CLEAR                   KTFA
089900960126     C                   EXSR      CTRDRA
090000960126     C*
090100960126     C     W001A         IFNE      ' '
090200960126     C                   SETON                                        16
090300960126     C                   ENDIF
090400960126     C* SE VARIATO QUALCHE COSA IN FV4 SOTTOMETTO IL PGM DI TRASMISSIO
090500960126     C*  SE FV CHIUSO
090600960126     C     D15FLG        IFNE      ' '
090700960126     C                   MOVEL     '1'           WAGG              1
090800960126     C                   ENDIF
090900960126     C*
091000960126     C                   GOTO      FOR02
091100960126    1C                   ENDDO
091200110530     c*
091300110530     c* F2 - gestione punti di scambio
091400110530     c                   if        *inkb=*on
091500110530     c                   eval      EmisD06='1'
091600110530     c                   exsr      GesPSC
091700110531     c                   if        vidpsc<>*blanks
091800110531     c                   seton                                        08
091900110531     c                   endif
092000110531     c
092100110530     C                   GOTO      FOR02
092200110530     c                   endif
092300951107     C*
092400951109     C*  CONTROLLI 2 VIDEATA
092500960123     C* SOLO SE NON SONO IN INTERROGAZIONE
092600130315    1C     ilst4ges      IFNE      'N'
092700951024     C                   EXSR      CTRD02
092800110530    2c                   if        not *in90
092900110530     c                   exsr      ctrPsc
093000110530     c* se trovo errori emetto msg generico di correzione
093100110530    3c                   if        *in90
093200110530     c                   seton                                        2890
093300110530     c                   clear                   vidmsg
093400110530     c                   movel     msg(102)      vidmsg
093500110530    3c                   endif
093600130729
093700130729     c* se immesso orario di partenza deve essere < dell'orario di scambio
093800160714     c* il controllo non lo faccio se abbinato a traino o foglio già
093900160714     c* chiuso
094000160714     c  n56              if        viddtp>0  and viddgs>0 and vidtrn=0
094100130729     c                   if        (amgdgs<amgdtp)or
094200130729     c                             (amgdgs=amgdtp and vidhhs<=vidhmp)
094300130729     C                   MOVEL     MSG(114)      VIDMSG
094400130729     C                   SETON                                        902846
094500130729    2C                   ENDIF
094600130729     c                   endif
094700110530     c
094800110530    2c                   endif
094900110530     c
095000960123    1C                   ENDIF
095100940915     C   KD
095200951024     COR 90              GOTO      FOR02
095300951026     C*
095400951026     C* SE VENGO DA BORDERO' REGISTRO I DATI ED ESCO
095500130315     C   01ilst4GES      IFEQ      'B'
095600951026     C                   EXSR      REGIS
095700960304     C   90              GOTO      FOR02
095800951026     C                   GOTO      FINE
095900951026     C                   ENDIF
096000960123     C**
096100951025     C* SE IMMESSO NUMERO TRAINO RIEMPO I CAMPI SFL COPIANDOLI DAL TRAI
096200960129     C* SOLO LA PRIMA VOLTA O SE HO MODIFICATO LA LINEA DI ARRIVO
096300951026     C   80WCALNA        IFEQ      'S'
096400951026     C*
096500951031     C     VIDTRN        IFGT      0
096600951017     C                   EXSR      RIETRS
096700951025     C                   ELSE
096800951025     C* CARICO SFL VUOTO IN IMMISIONE SENZA COPIA
096900951025     C                   CLEAR                   FFV
097000951025     C                   CLEAR                   FLP
097100951025     C                   EXSR      CARSFL
097200951017     C                   ENDIF
097300951026     C                   CLEAR                   WCALNA
097400951026     C                   ENDIF
097500130430     c* e premuto F6 faccio subito i controllo --> se OK    non emetto
097600130430     c*  videata del SFL
097700130430     c                   if        *inkf and not *in02
097800130430     C                   EXSR      CTRD03
097900130502     c                   endif
098000130502     c
098100130502     c                   if        not *in90 and *inkf
098200130430     C                   EXSR      REGIS
098300130430     C   90              GOTO      FOR02
098400130430     C* SE CHIUSURA FV LANCIO L'ELABORAZIONE
098500130430     C     ilst4GES      IFEQ      'C'
098600130430     C                   EXSR      CHIUFV
098700130430     C                   ENDIF
098800130430     c
098900130430     c                   goto      fine
099000130430     c
099100130430     C                   ENDIF
099200951017     C*
099300900515      *          =============
099400951025     C     FOR03         TAG
099500900515      *          =============
099600000828      *
099700000828      * SE TIPO TRAINO = DEFLUENZA/INOLTRO DISABILITO F10
099800000828      *
099900000828     C     VIDTTR        IFEQ      'I'
100000000828     C     WDEF1A        OREQ      'S'
100100000828     C                   SETON                                        61
100200000828     C                   ENDIF
100300000828      *
100400130315     C                   WRITE     lst4T01
100500130315     C                   WRITE     lst4Z03
100600130315     C                   EXFMT     lst4C03
100700960126     C                   MOVEL     '3'           WRECOR            1
100800970102     C                   SETOFF                                       289050
100900970102     C                   SETOFF                                       51
101000960123     C* F12 - RITORNO
101100951024     C   KL              GOTO      FOR02
101200960123     C**
101300960123     C* SE VENGO DA INTERROGAZIONE FV SINGOLA ESCO
101400130315     C     ilst4GES      IFEQ      'N'
101500960123     C                   GOTO      FINE
101600960123     C                   ENDIF
101700960123     C**
101800951027     C* F7  - MANUTENZIONE LINEE DEL FV
101900951027     C   KG              SETOFF                                       02
102000951027     C   KG              EXSR      CARSFL
102100951027     C   KG              GOTO      FOR03
102200951027      *
102300900515      *  CONTROLLI SFL
102400951027     C* LI ESEGUO SOLO SE POSSO MODIFICARE LE LINEE DEL FV
102500951027     C     *IN02         IFEQ      *OFF
102600951025     C                   EXSR      CTRD03
102700951027     C                   ENDIF
102800000825      *
102900000828      * F10=INSERIMENTO TERMINAL DI ARRIVO
103000000828      *
103100000828     C                   SETOFF                                       66
103200000825     C  N90
103300000825     CAN 67              EXSR      INSTER
103400000825      *
103500951026     C  NKF
103600000828     COR 66
103700000828     COR 90              GOTO      FOR03
103800951026     C*
103900951025      * F6  - REGISTRAZIONE DATI
104000951026     C     *INKF         IFEQ      *ON
104100951026     C                   EXSR      REGIS
104200951027     C   90              GOTO      FOR03
104300951026     C* SE CHIUSURA FV LANCIO L'ELABORAZIONE
104400130315     C     ilst4GES      IFEQ      'C'
104500951026     C                   EXSR      CHIUFV
104600951026     C                   ENDIF
104700951026     C*
104800951026     C                   ENDIF
104900900515      *
105000000000     C     FINE          TAG
105100960123     C* CHIUDO PGM TNTL35
105200960123     C                   MOVEL     'C'           D35TLA
105300020805     C                   MOVEL     TNTL35DS      KPJBU
105400960123     C                   CALL      'TNTL35R'
105500960123     C                   PARM                    KPJBA
105600960129     C*
105700960129     C* SOLO SE NON RICHIAMATO CHIUDO I FILE
105800130430     C***  *IN20         IFEQ      *OFF
105900130430     C**+                MOVEL     'C'           D15TLA
106000130430     C**+                MOVEL     FNLV15DS      KPJBU
106100130430     C***                CALL      'FNLV15R'
106200130430     C***                PARM                    KPJBA
106300130430     C***                ENDIF
106400960129     C*
106500960129     C* SOTTOMETTO TRASMISSIONE FV PER LA SEDE
106600960129     C     WLSA4         IFNE      ' '
106700960129     C                   RESET                   PARAM4
106800960129     C                   MOVEL     'M'           W004A             4
106900051121     C                   MOVE      SIMFEL        w004a
107000960129     C                   MOVEL     W004A         PA4MBD
107100960129     C                   MOVEL     SIMFEL        PA4FEL
107200960129     C                   MOVEL     'L'           PA4TLA
107300130419     C                   MOVEL     VIDfgs        PA4LNP
107400960129     C                   MOVEL     PARAM4        KPJBU
107500960129     C                   MOVEL     'LSA4'        KCOAZ
107600960129     C                   CALL      'BCH10'
107700960129     C                   PARM                    KPJBA
107800960129     C                   ENDIF
107900971203     C*
108000971203     C                   MOVEL     'C'           D55TLA
108100971203     C                   CALL      'FNLV55R'
108200020805     C                   PARM                    FNLV55DS
108300020805     c*
108400020805     c                   clear                   TRUL33DS
108500020805     c                   eval      I33tla = 'C'
108600020805     c                   movel(p)  TRUL33DS      KPJBU
108700020805     c                   call      'TRUL33R'
108800020805     c                   parm                    KPJBA
108900080314
109000080314     C                   CLEAR                   TRUL06DS
109100080314     C                   MOVE      '£1'          D06COD
109200080314     c                   movel     'C'           d06tla
109300080314     C                   MOVEL     SIMFEL        D06KEY
109400080314     C                   MOVEL     TRUL06DS      KPJBU
109500080314     C*
109600080314     C                   CALL      'TRUL06R'
109700080314     C                   PARM                    KPJBA
109800080314     c                   clear                   kpjbu
109900960129     C*
110000130315     C   20              MOVEL     fnlst4ds      KPJBU
110100960129     C*
110200000000     C                   SETON                                        LR
110300960129     C*
110400900515      *-----------------------------------------------------***********
110500900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
110600900515      *-----------------------------------------------------***********
110700900515     C     PULIZ         BEGSR
110800900515      *
110900910703     C                   SETOFF                                       995256
111000951120     C                   SETOFF                                       061618
111100000522     C                   SETOFF                                       430204
111200130503     C                   SETOFF                                       081061
111300951017     C                   CLEAR                   WFORZ
111400951017     C                   CLEAR                   WFORZ2
111500951017     C                   CLEAR                   WFORZ3
111600951024     C                   CLEAR                   WFORZ4
111700951025     C                   CLEAR                   WFORZ5
111800951027     C                   CLEAR                   WFORZ6
111900951027     C                   CLEAR                   WFORZ7
112000951120     C                   CLEAR                   WFORZ8
112100951130     C                   CLEAR                   WFORZ9
112200960122     C                   CLEAR                   WFORZB
112300960126     C                   CLEAR                   WFORZC
112400960130     C                   CLEAR                   WFORZD
112500110512     C                   CLEAR                   WFORzsc
112600110530     C                   CLEAR                   WFORzlnar
112700130424     C                   CLEAR                   WFORzD1
112800130424     C                   CLEAR                   WFORzM1
112900951025     C*
113000130424     C                   CLEAR                   VIDFGS
113100130424     C                   CLEAR                   DESFGS
113200951025     C                   CLEAR                   NFP
113300951025     C                   CLEAR                   NFA
113400951025     C                   CLEAR                   WTFP
113500951025     C                   CLEAR                   WTFA
113600951025     C                   CLEAR                   WDDE
113700951025     C                   CLEAR                   WAN
113800951025     C                   CLEAR                   WRI
113900951025     C                   CLEAR                   WTFM
114000951025     C                   CLEAR                   WNFM
114100951123     C                   CLEAR                   WFMFES
114200951025     C                   CLEAR                   WANDDE
114300150902     C                   CLEAR                   WANtfa
114400951025     C                   CLEAR                   WRIDDE
114500951106     C                   CLEAR                   SAVRTC
114600960126     C                   CLEAR                   SAVDTP
114700960126     C                   CLEAR                   SAVHMP
114800960422     C                   CLEAR                   WINNFV
114900960129     C*
115000960129     C                   CLEAR                   WOBLPD
115100960129     C                   CLEAR                   WTLPDR
115200061116     C                   CLEAR                   okestero
115300130510     C                   CLEAR                   winviamail
115400951026     C* KEY PER LETTURA SULLE SPUNTE
115500951026     C                   MOVEL     1             KNPG
115600951026     C                   MOVEL     SIMFEL        KFGS
115700951017     C*
115800951026     C                   MOVEL     'S'           WCALNA            1
115900900515     C                   Z-ADD     0             VIDNFV
116000951031     C                   CLEAR                   COPTRN
116100951025     C* CAMPI DI SALVATAGGIO
116200951031     C                   CLEAR                   SAVTRN
116300951027     C                   MOVEL     'AAA'         SAVLNA
116400951025     C                   Z-ADD     0             SAVDFV
116500951025     C                   Z-ADD     0             SAVIPT
116600010920     C                   CLEAR                   SAVVIP
116700951027     C                   CLEAR                   SAVTTR
116800940909     C*
116900940912     C                   TIME                    WTIME
117000940912     C                   MOVE      WTIME         WDATE
117100080919     c* data meno 30 gg
117200080919     c     *eur          move      wdate         wdtaiso
117300080919     c                   subdur    30:*days      wdtaiso
117400080919     c                   move      wdtaiso       dm30
117500080919
117600960122     C                   MOVE      WTIME         KANN
117700940912     C                   MOVEL     WDATE         VIDDFV
117800030701     c                   ExSr      Sr_MinMax
117900951024     C                   CLEAR                   VIDTRN
118000050706     c* mi salvo data - 2 che mi serve per eventuale riapertura foglio
118100050706     c                   eval      woggimeno2=wdtameno2
118200940909     C*
118300130419     C                   MOVE      SIMFEL        VIDfgs
118400951024     C*
118500951031     C                   CLEAR                   VIDAN1
118600951031     C                   CLEAR                   VIDAN2
118700960123     C                   CLEAR                   VIDBIS
118800940913     C                   MOVEL     *BLANKS       VIDPDR
118900130424     C                   MOVEL     *BLANKS       SAVPDR
119000010927     C                   MOVEL     §GEDCN        VIDVIP
119100900515     C                   Z-ADD     0             VIDIPT
119200900515     C                   MOVE      *ZEROS        VIDLNA
119300951024     C                   MOVEL     *BLANKS       DESLNA
119400900515     C                   Z-ADD     0             VIDHMP
119500951103     C                   CLEAR                   VIDRTC
119600960208     C                   CLEAR                   DESRT1
119700960208     C                   CLEAR                   DESRT2
119800951025     C                   MOVEL     *BLANKS       VIDDPD
119900900606     C                   MOVEL     *BLANKS       VIDDCN
120000130424     C                   MOVEL     *BLANKS       savDCN
120100010622     C                   MOVEL     *BLANKS       VIDCNC
120200900606     C                   MOVEL     *BLANKS       VIDTMZ
120300950901     C                   MOVEL     *BLANKS       DESTMZ
120400130502     C                   MOVEL     *BLANKS       DESTMZ2
120500950901     C                   MOVEL     *BLANKS       VIDTTR
120600960206     C                   MOVEL     *BLANKS       DESTTR
120700900515     C                   MOVEL     *BLANKS       VIDTRM
120800130424     C                   MOVEL     *BLANKS       savtrm
120900900515     C                   MOVEL     *BLANKS       VIDTRR
121000940909     C                   MOVEL     *BLANKS       VIDPMB
121100900606     C                   MOVEL     *BLANKS       VIDNT1
121200900606     C                   MOVEL     *BLANKS       VIDNT2
121300910405     C                   MOVEL     'N'           VIDSNS
121400951023     C                   CLEAR                   VIDDTP
121500951024     C                   CLEAR                   VIDHMP
121600960122     C                   CLEAR                   VIDDTT
121700960122     C                   CLEAR                   VIDHMT
121800110511     c
121900110531     c                   clear                   oldpsc
122000110531     c                   clear                   savpsc
122100110531     c                   clear                   vidpsc
122200110511     c                   clear                   despsc
122300110512     c                   clear                   viddgs
122400110511     c                   clear                   vidhhs
122500110511     c                   clear                   vidtrns
122600110530     c                   clear                   vidlnar
122700110530     c                   clear                   deslnar
122800110601     c                   clear                   deslnar2
122900110513     c                   clear                   destrns
123000110531     c                   clear                   despind
123100110531     c                   clear                   desploc
123200940913     C*
123300940913     C* PULISCO LE SCHIERE DEI CONTROLLI
123400940913     C                   MOVEA     *BLANKS       LAB
123500940913     C                   MOVEA     *BLANKS       LAS
123600951130     C                   MOVEA     *BLANKS       TFV
123700951130     C                   MOVEA     *BLANKS       TLP
123800960126     C                   CLEAR                   AMGDFV
123900960126     C                   CLEAR                   AMGDTT
124000960126     C                   CLEAR                   AMGDTP
124100951027     C* PROTEZIONE DELLE LINEE DI ARRIVO
124200130315     C     ilst4GES      IFNE      ' '
124300130315     C     ilst4GES      ANDNE     'M'
124400130419     C     ilst4GES      ANDNE     'P'
124500130419     C     ilst4GES      ANDNE     'A'
124600951027     C                   SETON                                        02
124700951027     C                   ENDIF
124800900515      *
124900900514     C                   ENDSR
125000951023      *-----------------------------------------------------***********
125100951023      * CONTROLLI FORMATO 1 PER INSERIMENTO NUOVO FV        *  CTRINS *
125200951023      *-----------------------------------------------------***********
125300951023     C     CTRINS        BEGSR
125400951026     C                   SETON                                        80
125500951023     C*
125600951023     C* SE IMMESSO CONTROLLO NUMERO TRAINO DA CUI COPIARE
125700951031    2C     COPTRN        IFGT      *ZEROS
125800130319     C*
125900130319     C* DATA FOGLIO VIAGGIO  E CALCOLO LA DATA FV -1 E + 5 GG
126000130319     c                   clear                   wd02              1
126100130319     C                   EXSR      CALDAT
126200130319     C   90              GOTO      ENDINS
126300130319     c
126400951023     C                   EXSR      CTRTRN
126500130419     c
126600130419     c* non dovrebbero esserci errori sul traino ma per sicurezza controllo
126700130419     c                   if        *in90
126800130419     C                   GOTO      ENDINS
126900130419     c                   endif
127000951218     C*
127100951218     C* CODICE TRAZIONISTA OBBLIGATORIO SE C'E' LA COPIA DA TRAINO
127200960129     C*  E SE IN ANAGRAFICA C'E' PIU' DI UN PADRONCINO O E' PRESENTE
127300960129     C*  SOLO QUELLO GENERICO - SOLO PER IMMISSIONE
127400130315    3c                   if        ilst4ges='A' and ilst4old='S'
127500130315    4C     VIDPDR        IFLE      *ZEROS
127600960129     C     WOBLPD        ANDEQ     'S'
127700951218     C                   MOVEL     MSG(9)        VIDMSG
127800130419     C                   SETON                                        2890
127900951218     C                   GOTO      ENDINS
128000130315    3c                   ENDIF
128100130315    2c                   ENDIF
128200951023     C*
128300951023     C* SE TUTTO OK RIEMPO I CAMPI DELLA VIDEATA COPIANDOLI DAL TRAINO
128400130315    3c                   if        ilst4ges='A'
128500130315     C                   EXSR      RIETRV
128600130315    3C                   ENDIF
128700130315    2C                   ENDIF
128800951023     C*
128900951023     C* REPERISCO ULTIMO NUMERO FOGLIO VIAGGIO IMMESSO
129000130319    2C***  *INKJ         IFEQ      *ON
129100020805     c                   reset                   TRUL33DS
129200020805     c                   eval      I33po1 = SIMfel
129300020805     c                   movel(p)  TRUL33DS      KPJBU
129400020805     c                   call      'TRUL33R'
129500020805     c                   parm                    KPJBA
129600020805     c                   movel     KPJBU         TRUL33DS
129700020805    3C                   select
129800020805     C* Numeratore FV non trovato
129900020805     C                   when      O33err =  01
130000020805     C                   MOVEL     MSG(43)       VIDMSG
130100130419     C                   SETON                                        2890
130200020805     C                   GOTO      ENDINS
130300020805     C* Numeratore FV allocato
130400020805     C                   when      O33err =  02
130500951120     C                   MOVEL     MSG(60)       VIDMSG
130600130419     C                   SETON                                        2890
130700951120     C                   GOTO      ENDINS
130800020805     C                   when      O33err <> *zeros
130900020805     C                   movel     O33msg        VIDmsg
131000130419     C                   seton                                        2890
131100020805     C                   goto      ENDINS
131200020805    3C                   endsl
131300020805     C                   z-add     O33nrf        VIDnfv
131400960126     C*
131500020805     C     K02FGV01      SETLL     FNFGV01L                               83
131600960126    5C     *IN83         DOWEQ     *ON
131700020805     c                   reset                   TRUL33DS
131800020805     c                   eval      I33po1 = SIMfel
131900020805     c                   movel(p)  TRUL33DS      KPJBU
132000020805     c                   call      'TRUL33R'
132100020805     c                   parm                    KPJBA
132200020805     c                   movel     KPJBU         TRUL33DS
132300020805    6C                   if        O33err <> *zeros
132400020805     c                   movel     O33msg        VIDmsg
132500130419     c                   seton                                        2890
132600020805     c                   goto      ENDINS
132700020805    6c                   endif
132800020805     c                   z-add     O33nrf        VIDnfv
132900020805     c     K02FGV01      SETLL     FNFGV01L                               83
133000960126    5C                   ENDDO
133100951023     C*
133200951023     C* 80 ON  - IMMISSIONE
133300130319     C****               SETON                                        80
133400130319    2C***                ENDIF
133500951023     C*
133600960126     C     ENDINS        ENDSR
133700951003     C*-----------------------------------------------------***********
133800951017     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
133900951003     C*-----------------------------------------------------***********
134000951003     C     CTRTRN        BEGSR
134100951003     C                   SETOFF                                       90
134200951003     C*
134300951024    1C     COPTRN        IFNE      SAVTRN
134400951003     C*
134500951023     C                   CLEAR                   W001
134600951023     C                   CLEAR                   WFORZ
134700951023     C                   CLEAR                   WFORZ2
134800951024     C                   CLEAR                   WFORZ3
134900951120     C                   CLEAR                   WFORZ8
135000951025     C     VIDTRN        SETLL     TNTLT01L
135100951025     C     VIDTRN        READE     TNTLT01L                               83
135200951003     C*
135300951013    2C     *IN83         DOWEQ     *OFF
135400951003    3C     TLTATB        IFEQ      ' '
135500951023     C* CONTROLLO CHE IL TRAINO CHE SI VUOLE COPIARE NON SIA UN TRAINO
135600951023     C*   DI TIPO "A-AFFLUENZA"
135700951023    4C     TLTTTR        IFEQ      'A'
135800951023     C                   MOVEL     MSG(23)       VIDMSG
135900130419     C                   SETON                                        9028
136000951023     C                   GOTO      ENDTRN
136100951023    4C                   ENDIF
136200951023     C*
136300951003     C                   MOVEL     '1'           W001              1
136400951023     C* ALMENO UN RECORD TROVATO --> ESCO DALLA LETTURA
136500951013     C                   SETON                                        83
136600951003    3C                   ENDIF
136700951003     C*
136800951025     C  N83VIDTRN        READE     TNTLT01L                               83
136900951003    2C                   ENDDO
137000951003     C*
137100951003     C* W001 = " " ---> IL N.TRAINO IMMESSO E' INESISTENTE O ANNULLATO
137200951003    2C     W001          IFEQ      ' '
137300951003     C                   MOVEL     MSG(20)       VIDMSG
137400130419     C                   SETON                                        9028
137500951003     C                   GOTO      ENDTRN
137600951003    2C                   ENDIF
137700951003     C*
137800951023     C* DETERMINO IL PROGRESSIVO DECORRENTE DELLA FERMATA
137900951023     C*  VEDO ANCHE SE C'E' A/R OPPURE NO
138000951023     C                   EXSR      CTRDEC
138100951003     C*
138200951023     C* WAN E WRI=" "-> NUMERO TRAINO IMMESSO ERRATO PERCHE':
138300951013     C*                  - SIMFEL NON E' UNA FERMATA DEL TRAINO
138400951013     C*                  - SIMFEL NON E' UNA FERMATA DI CARICO
138500951013     C*                  - IL TRAINO E' DA DECORRERE O SCADUTO
138600951023    2C                   SELECT
138700951023    2C     WAN           WHENEQ    ' '
138800951023     C     WRI           ANDEQ     ' '
138900951006     C                   MOVEL     MSG(21)       VIDMSG
139000130419     C                   SETON                                        9028
139100951006     C                   GOTO      ENDTRN
139200951023     C*
139300951023     C* SE IMMESSI ENTRAMBI --> CHIEDO LA LINEA DI ARRIVO
139400951023    2C     WAN           WHENEQ    'S'
139500951023     C     WRI           ANDEQ     'S'
139600951123     C                   MOVEL     'A'           WFMFES
139700951023     C*
139800951023    3C     *IN06         IFEQ      *OFF
139900951023     C                   SETON                                        0690
140000951024     C                   MOVEL     COPTRN        SAVTRN
140100951023     C                   GOTO      ENDTRN
140200951023    3C                   ENDIF
140300951023     C*
140400951023   X2C                   OTHER
140500951023     C*
140600951023     C* MEMORIZZO IL TIPO FERMATA DA ELABORARE
140700951023    3C     WAN           IFEQ      'S'
140800951123     C                   MOVEL     'A'           WFMFES
140900951025     C                   MOVEL     'A'           WTFM
141000951025     C                   Z-ADD     WANDDE        WDDE
141100951023     C                   ELSE
141200951123     C                   MOVEL     'R'           WFMFES
141300951025     C                   MOVEL     'R'           WTFM
141400951025     C                   Z-ADD     WRIDDE        WDDE
141500951023    3C                   ENDIF
141600951023     C* SALVO IL NUMERO TRAINO IMMESSO
141700951024     C                   MOVEL     COPTRN        SAVTRN
141800951023     C*
141900951023     C* SE E' SOLO A O R --> FACCIO SPARIRE LA LINEA DI ARRIVO SE C'E'
142000951023    3C     *IN06         IFEQ      *ON
142100951023     C                   SETOFF                                       06
142200951023     C                   SETON                                          90
142300951023     C                   GOTO      ENDTRN
142400951023    3C                   ENDIF
142500951023    2C                   ENDSL
142600951023    1C                   ENDIF
142700951005     C*
142800951009     C* 06 ON  - E' RICHIESTA LA LNA FINALE IN 1° VIDEATA PERCHE' SI
142900951009     C*   TRATTA DI UN TRAINO DI TIPO "C-CHIUSO"
143000951009    1C     *IN06         IFEQ      *ON
143100951017     C*
143200951025     C     VIDLNA        IFNE      SAVLNA
143300951025     C                   CLEAR                   WTFM
143400951017     C                   EXSR      CTRLNA
143500980115     C* SALVO LA LINEA DI ARRIVO
143600980115     C                   MOVEL     VIDLNA        SAVLNA
143700980115     C                   MOVEL     'S'           WCALNA            1
143800951017     C   90              GOTO      ENDTRN
143900951025     C                   ENDIF
144000951013     C*
144100951009   X1C                   ELSE
144200951013     C* 06 OFF - NON E' RICHIESTA LA LNA FINALE IN 1° VIDEATA QUINDI IL
144300951003     C*
144400951024     C* CERCO LA LINEA FINALE ARRIVO TRAINO PER SIMFEL
144500951024     C*   UTILIZZANDO I MC: INIZIO CON LA LINEA FINALE TRAINO
144600951027     C                   SETON                                        85
144700951027     C     WTFM          IFEQ      'A'
144800951025     C                   CLEAR                   TRNTFA
144900000522     C                   MOVE      VIDFGS        WTFP
145000150902     C****               MOVEL     TLTTFA        WTFA
145100150902     C                   MOVEL     wantfa        WTFA
145200951024     C                   EXSR      REDTLM
145300951027     C                   ENDIF
145400951024     C*
145500951025     C     *IN85         IFEQ      *OFF
145600150902     C***                Z-ADD     TLTTFA        TRNTFA
145700150902     C                   Z-ADD     wantfa        TRNTFA
145800951025     C                   ELSE
145900951027     C     WTFM          IFEQ      'A'
146000951024     C                   Z-ADD     NFP(1)        WNFM
146100951027     C                   ELSE
146200951027     C                   Z-ADD     NFP(2)        WNFM
146300951027     C                   ENDIF
146400951024     C                   EXSR      RICLNA
146500951024     C                   ENDIF
146600951005     C*
146700951025     C* TRNTFA=0  ---> TRAINO NON UTILIZZABILE PERCHE' NON TROVATI I
146800951024     C*                METRICUBI PER NESSUNA FILIALE FERMATA
146900951025    2C     TRNTFA        IFEQ      0
147000951024     C                   MOVEL     MSG(21)       VIDMSG
147100130419     C                   SETON                                        9028
147200951005     C                   GOTO      ENDTRN
147300951005    2C                   ENDIF
147400951025     C* SE NON IMMESSA LNA FINALE IN 1 VIDEATA CARICO TUTTE LE FERMATE
147500951025     C*  DI SCARICO PER QUEL TIPO FERMATA
147600951025     C                   Z-ADD     999           WNFM
147700951009     C*
147800951009    1C                   ENDIF
147900951027     C* CHAINO LA TESTATA
148000951027     C     KTLR11        CHAIN     TNTLT01L                           83
148100951027     C*
148200951024     C* CONTROLLO SE IL TRAINO SI PUO' APRIRE IN QUESTO GIORNO
148300951024    1C     WFORZ         IFEQ      ' '
148400951024     C* REPERISCO IL GIORNO DELLA SETTIMANA (1=LUN ...)
148500951024     C                   CALL      'XGIOSE1'
148600951024     C                   PARM                    GGMMAA            6
148700951024     C                   PARM                    GIOSET            1
148800951024     C*
148900951024     C                   MOVEA     TLTGSE        GSE
149000951024     C     GIOSET        LOOKUP    GSE                                    83
149100951024    2C     *IN83         IFEQ      *OFF
149200130502     C                   MOVEL     'S'           WFORZ             1
149300130502     c
149400130502    4c                   if        ilst4ges<>'A' or iolst4for<>'F6 '
149500951024     C                   MOVEL     MSG(22)       VIDMSG
149600130419     C                   SETON                                        9028
149700960302     C                   MOVEL     GIOSET        SAVSET            1
149800130502     c                   eval      iolst4for='022'
149900951024     C                   GOTO      ENDTRN
150000130502     c                   endif
150100130502     c
150200951024    2C                   ENDIF
150300951024    1C                   ENDIF
150400951123     C*
150500951123     C* IN UNA DATA SOLO UN FOGLIO VIAGGIO PUO' ESSERE ABBINATO AD
150600951123     C*   UN NUMERO TRAINO A O R : ALTRIMENTI FORZO MA NON ABBINO IL FV
150700951123     C                   EXSR      CTRDOP
150800951123     C   90              GOTO      ENDTRN
150900960129     C*
151000960129     C* VERIFICO SE NEI LISTINI C'E' SOLO UN TRAZIONISTA O PIU':
151100960129     C*  SE CE N'E' UNO SOLO, NON E' OBBLIGATORIO IN 1 VIDEATA
151200960129     C                   CLEAR                   WOBLPD
151300960129     C                   CLEAR                   WTLPDR
151400010920     C     KTLR11        SETLL     TITLL01L
151500010920     C     KTLR11        READE     TITLL01L                               83
151600960129    1C     *IN83         DOWEQ     *OFF
151700960129    2C     TLLATB        IFEQ      ' '
151800960129     C     TLLPDR        ANDNE     9999999
151900960129    3C     WTLPDR        IFEQ      0
152000960129     C                   MOVEL     TLLPDR        WTLPDR
152100960129   X3C                   ELSE
152200960129     C                   MOVEL     'S'           WOBLPD            1
152300960129     C                   SETON                                        83
152400960129    3C                   ENDIF
152500960129    2C                   ENDIF
152600960129     C*
152700010920     C  N83KTLR11        READE     TITLL01L                               83
152800960129    1C                   ENDDO
152900960129     C*
153000960129     C* SE C'E' SOLO CODICE 9999999--> OBBLIGATORIO L'INSERIMENTO
153100960129     C     WTLPDR        IFEQ      0
153200960129     C                   MOVEL     'S'           WOBLPD
153300960129     C                   ENDIF
153400951009     C*
153500951025     C     VIDLNA        IFLE      *ZEROS
153600951025     C                   MOVEL     TRNTFA        VIDLNA
153700951024     C                   ENDIF
153800951009     C     ENDTRN        ENDSR
153900951123     C*---------------------------------------------------------------*
154000951123     C* IN UNA DATA SOLO UN FOGLIO VIAGGIO PUO' ESSERE ABBINATO AD
154100951123     C*   UN NUMERO TRAINO A O R : ALTRIMENTI FORZO MA NON ABBINO IL FV
154200951123     C*---------------------------------------------------------------*
154300951123     C     CTRDOP        BEGSR
154400130429     c* Se apertura fogli, non controllo se ppremuto F6
154500130429     c
154600951123    1C     WFORZ2        IFEQ      ' '
154700130429     c*
154800951123     C     KFGV2         SETLL     FNFGV02L
154900951123     C     KFGV2         READE     FNFGV02L                               83
155000951123     C*
155100951123    2C     *IN83         DOWEQ     *OFF
155200961216    3C     F2VATB        IFEQ      ' '
155300961216     C     F2VTRN        ANDEQ     VIDTRN
155400961216     C     F2VFRQ        ANDEQ     WTFM
155500000522     C                   MOVEL     DSFGW         SVFGW
155600020805     c                   eval      FGWnfv = F2Vnfv
155700020805     c                   eval      FGWlnp = F2Vlnp
155800020805     c                   exsr      CTR_FGW
155900130315     c
156000951123     C                   MOVEL     'S'           WFORZ2            1
156100130429     c
156200130429    4c                   if        ilst4ges<>'A' or iolst4for<>'F6 '
156300130429     C                   SETON                                        9083
156400130429     C                   SETON                                        28
156500951123     C                   MOVEA     MSG(31)       K78
156600961216     C                   MOVEL     F2VNFV        W005A             5
156700951123     C                   MOVEA     W005A         K15(11)
156800951123     C                   Z-ADD     11            Z
156900951123     C                   EXSR      ABBL
157000951123     C                   MOVEA     K15(11)       K78(34)
157100960302     C                   MOVEA     K15(11)       SAV005            5
157200951123     C                   MOVEA     K78           VIDMSG
157300130429     c                   eval      iolst4for='031'
157400130429    4C                   ENDIF
157500130429    3C                   ENDIF
157600000522     C                   MOVEL     SVFGW         DSFGW
157700951123     C*
157800951123     C  N83KFGV2         READE     FNFGV02L                               83
157900951123    2C                   ENDDO
158000130429    1C                   ENDIF
158100951123     C                   ENDSR
158200951023     C*---------------------------------------------------------------*
158300951023     C* CONTROLLO LA DATA DI DECORRENZA DELLA FERMATA E SE C'E' A/R   *
158400951023     C* DETERMINO LA DATA TRAINO
158500951023     C*----------------------------------------------------------------
158600951023     C     CTRDEC        BEGSR
158700951023     C*
158800951024     C                   CLEAR                   NFP
158900951024     C                   CLEAR                   NFA
159000951024     C                   CLEAR                   W001
159100951023     C                   CLEAR                   WANDDE
159200150902     C                   CLEAR                   WANtfa
159300951023     C                   CLEAR                   WAN
159400951023     C                   CLEAR                   WRIDDE
159500951023     C                   CLEAR                   WRI
159600000522     C                   MOVE      VIDFGS        WFIL
159700951025     C                   Z-ADD     1             X
159800951025     C* IN IMMISSIONE FACCIO 2 TENTATIVI SE SERVE
159900960122     C* IN VARIAZIONE CERCO SUBITO CON CIO'CHE HO MEMORIZZATO IN FGVFRQ
160000951025     C     *IN80         IFEQ      *ON
160100951023     C                   MOVEL     'A'           WTFM
160200951025     C                   ENDIF
160300951023     C*
160400951023     C* ESEGUO IL CICLO 2 VOLTE: PRIMA PER "ANDATA" E POI PER "RITORNO"
160500951024    1C                   DO        2             X                 2 0
160600951023     C     KTLR          SETLL     TNTLR02L
160700951023     C     KTLR          READE     TNTLR02L                               83
160800951023     C*
160900951023    2C     *IN83         DOWEQ     *OFF
161000951023     C*
161100951023    3C     TLRATB        IFEQ      ' '
161200951023     C     TLRCAR        ANDEQ     'S'
161300951023     C*
161400951023     C* GIORNO =A ->LA DATA TRAINO CORRISPONDE ALLA DATA FV
161500951023    4C     TLRGFV        IFEQ      'A'
161600951024     C                   Z-ADD     AMGDFV        WDTN
161700951024     C                   MOVEL     VIDDFV        GGMMAA
161800951024     C                   MOVE      VIDDFV        W0020
161900951024     C                   MOVE      W0020         GGMMAA
162000951023   X4C                   ELSE
162100951023     C* GIORNO<>A ->SOTTRAGGO ALLA DATA FOGLIO I GIORNI DI APERTURA FV
162200951026     C                   Z-ADD     1             C                 3 0
162300951027     C     TLRGFV        LOOKUP    GFV(C)                                 31
162400951025     C     WGGDFV        SUB       C             GI8TGI
162500951023     C                   CALL      'XSRGI8'
162600951023     C                   PARM                    WGIDAT
162700951023     C                   Z-ADD     GI8INV        WDTN
162800951024     C                   MOVEL     GI8DAT        GGMMAA
162900951024     C                   MOVE      GI8DAT        W0020             2 0
163000951024     C                   MOVE      W0020         GGMMAA
163100951023    4C                   ENDIF
163200951023     C*
163300951023     C* E' DECORRENTE -- > ESCO DALLA LETTURA
163400951023    4C     WDTN          IFGE      TLRDDE
163500951024     C* CHAIN SULLA TESTA PER VEDERE SE E' ANCHE DENTRO LA SCADENZA
163600951024     C                   Z-ADD     TLRDDE        WDDE
163700951027     C     KTLR11        CHAIN     TNTLT01L                           84
163800951024    5C     *IN84         IFEQ      *OFF
163900951024     C     WDTN          ANDLE     TLTDSC
164000951023     C*                    SETON                     83
164100951023     C* MEMORIZZO SE E' ANDATA + DATA DECORRENZA
164200951024    6C     WTFM          IFEQ      'A'
164300951023     C                   MOVEL     'S'           WAN               1
164400951023     C                   Z-ADD     TLRDDE        WANDDE
164500150902     C                   Z-ADD     TLttfa        WANtfa
164600951024     C                   Z-ADD     TLRNFM        NFP(1)
164700951023     C* SE IL TRAINO E' CHIUSO VERIFICO ANCHE SE E' RITORNO
164800951025     C* SE SONO IN VARIAZIONE ESCO
164900951024    7C     TLTTTR        IFNE      'C'
165000951025     C     *IN80         OREQ      *OFF
165100951023     C                   Z-ADD     3             X
165200951024    7C                   ENDIF
165300951023     C*
165400951024   X6C                   ELSE
165500951023     C* MEMORIZZO SE E' RITORNO
165600951023     C                   MOVEL     'S'           WRI               1
165700951023     C                   Z-ADD     TLRDDE        WRIDDE
165800951024     C                   Z-ADD     TLRNFM        NFP(2)
165900951024    6C                   ENDIF
166000951024    5C                   ENDIF
166100951023    4C                   ENDIF
166200951023    3C                   ENDIF
166300951023     C*
166400951023     C  N83KTLR          READE     TNTLR02L                               83
166500951023    2C                   ENDDO
166600951023     C*
166700951025     C   80              MOVEL     'R'           WTFM
166800951023    1C                   ENDDO
166900951013     C*
167000951023     C                   ENDSR
167100951017     C*-----------------------------------------------------***********
167200951017     C* CONTROLLO LINEA ARRIVO FINALE                       * CTRLNA  *
167300951017     C*-----------------------------------------------------***********
167400951017     C     CTRLNA        BEGSR
167500951017     C*
167600951017     C* RICERCA FILIALE FINALE DI ARRIVO
167700951027     C     '?'           SCAN      VIDLNA                                 45
167800951027    1C     *IN45         IFEQ      *ON
167900951024     C                   EXSR      CASD19
168000951017     C                   MOVEL     §COD          VIDLNA
168100951017     C                   MOVEL     §§DES         DESLNA
168200951017     C                   SETON                                        90
168300951017     C                   GOTO      ENDCTR
168400951017    1C                   ENDIF
168500951017     C*
168600951017     C* CONTROLLO CHE SIA UNA FILIALE DELL'ORGANIGRAMMA
168700951017     C                   MOVEL     *BLANKS       DESLNA
168800020213     c                   clear                   okestero
168900951025     C                   MOVE      VIDLNA        CODFIL
169000951025     C                   EXSR      CTRFIL
169100951017     C*
169200951026    1C     *IN34         IFEQ      *ON
169300951017     C                   MOVEL     *ALL'?'       DESLNA
169400951017     C                   MOVEL     MSG(8)        VIDMSG
169500951026     C                   SETON                                        284590
169600951017     C                   GOTO      ENDCTR
169700951017   X1C                   ELSE
169800951017     C                   MOVEL     ORGDES        DESLNA
169900020213     c                   eval      okestero = esteroedpd
170000020919      *
170100020919      * controllo che coincida con P.O. Gestione
170200020919     c                   clear                   FNLV55DS
170300020919     c                   movel     '6'           D55tpt
170400020919     c                   move      CODfil        D55lin
170500020919     c                   z-add     AMGdfv        D55drf
170600020919     c                   call      'FNLV55R'
170700020919     c                   parm                    FNLV55DS
170800020919if  2c                   if        D55tfa <> CODfil
170900020919     c                   movel     MSG(87)       VIDMSG
171000020919     c                   seton                                        459028
171100020919e   2c                   endif
171200020919      *
171300960226     C* SE NON C'E ECCEZIONE CARICO
171400960226     C                   MOVEL     VIDLNA        KTFA
171500960226     C                   MOVEL     'A'           KEPA
171600970102     C                   EXSR      RICEC2
171700971203     C                   MOVEA     TFE           TFEL
171800970102     C*
171900971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
172000971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
172100020805     C                   CLEAR                   FNLV55DS
172200971203     C                   MOVEL     'A'           D55TPT
172300971203     C                   MOVE      VIDLNA        D55LIN
172400000522     C                   MOVE      VIDFGS        D55LNP
172500971203     C                   MOVE      AMGDFV        D55DRF
172600971203     C                   CALL      'FNLV55R'
172700020805     C                   PARM                    FNLV55DS
172800971203     C                   MOVE      D55TFA        LNAFE0
172900960226     C*
173000951017    1C                   ENDIF
173100951017     C*
173200951017     C* S O L O   S E   P R O V E N G O   D A   C O P I A   T R A I N O
173300951025    1C     VIDTRN        IFGT      0
173400951017     C*
173500951017     C* CONTROLLO CHE LA LINEA FINALE DI ARRIVO IMMESSA ESISTA NEL
173600951024     C*   FILE DEI MC
173700951017     C                   CLEAR                   W001
173800951024     C                   MOVEL     VIDLNA        WFIL
173900000522     C                   MOVE      VIDFGS        WTFP
174000951024     C                   MOVEL     VIDLNA        WTFA
174100951024     C* tento PRIMA COME ANDATA
174200951027    2C     *IN80         IFEQ      *ON
174300951025     C     WTFM          OREQ      'A'
174400951024     C                   MOVEL     'A'           WTFM
174500951024     C                   Z-ADD     WANDDE        WDDE
174600951024     C                   EXSR      LEGFER
174700951025    2C                   ENDIF
174800951024     C*
174900951024     C* POI COME RITORNO
175000951027    2C     *IN80         IFEQ      *ON
175100951025     C     WTFM          OREQ      'R'
175200951024     C                   MOVEL     'R'           WTFM
175300951024     C                   Z-ADD     WRIDDE        WDDE
175400951024     C                   EXSR      LEGFER
175500951025    2C                   ENDIF
175600951017     C*
175700951025    2C                   SELECT
175800951120     C* NON ESISTE --> ERRORE PER IMMISSIONE, X VARIAZ. ERRORE FORZABIL
175900951024     C     W001          WHENEQ    ' '
176000951120     C     *IN80         IFEQ      *ON
176100951017     C                   MOVEL     MSG(28)       VIDMSG
176200951026     C                   SETON                                        904528
176300951017     C                   GOTO      ENDCTR
176400951120     C                   ELSE
176500951120     C     WFORZ8        IFEQ      ' '
176600951120     C                   MOVEL     MSG(61)       VIDMSG
176700951120     C                   SETON                                        904528
176800951120     C                   MOVEL     'S'           WFORZ8            1
176900951120     C                   GOTO      ENDCTR
177000951120     C                   ENDIF
177100951120     C                   ENDIF
177200951024     C* ANDATA
177300951024     C     W001          WHENEQ    'A'
177400951024     C                   MOVEL     'A'           WTFM
177500951024     C                   MOVEL     NFA(1)        WNFM
177600951024     C                   Z-ADD     WANDDE        WDDE
177700951024     C* RITORNO
177800951024     C     W001          WHENEQ    'R'
177900951024     C                   MOVEL     'R'           WTFM
178000951024     C                   MOVEL     NFA(2)        WNFM
178100951024     C                   Z-ADD     WRIDDE        WDDE
178200951024     C*
178300951025   X2C                   OTHER
178400951024     C* LINEA FINALE DI ARRIVO PRESENTE SIA IN "A" CHE "R"
178500951024     C*   --> CONTROLLO QUALE DELLE 2  DAL NUMERO FERMATA
178600951024     C* ENTRAMBI MINORE --> ERRORE
178700951025    3C     NFP(1)        IFLT      NFA(1)
178800951025     C     NFP(2)        ANDLT     NFA(2)
178900951024     C                   MOVEL     MSG(28)       VIDMSG
179000951026     C                   SETON                                        904528
179100951024     C                   GOTO      ENDCTR
179200951025    3C                   ENDIF
179300951024     C* SE ENTRAMBI MAGGIORI --> TELEFONARE IN SEDE!!!
179400951025    3C     NFP(1)        IFGT      NFA(1)
179500951024     C     NFP(2)        ANDGT     NFA(2)
179600951024     C                   MOVEL     MSG(32)       VIDMSG
179700951026     C                   SETON                                        904528
179800951024     C                   GOTO      ENDCTR
179900951025    3C                   ENDIF
180000951024     C*
180100951025    3C     NFP(1)        IFLT      NFA(1)
180200951024     C                   MOVEL     'A'           WTFM
180300951024     C                   MOVEL     NFA(1)        WNFM
180400951024     C                   Z-ADD     WANDDE        WDDE
180500951025   X3C                   ELSE
180600951017     C                   MOVEL     'R'           WTFM
180700951017     C                   MOVEL     NFA(2)        WNFM
180800951024     C                   Z-ADD     WRIDDE        WDDE
180900951025    3C                   ENDIF
181000951025    2C                   ENDSL
181100951017     C*
181200951025     C                   CLEAR                   WFORZ3
181300951025     C*
181400951025     C* CONTROLLO SE LA LINEA IMMESSA E' LA FINALE DEL TRAINO
181500951024    2C     WFORZ3        IFEQ      ' '
181600951025     C                   MOVEL     VIDLNA        TRNTFA
181700951024     C                   EXSR      RICLNA
181800951024     C*
181900951017     C* SE LA LINEA IMMESSA NON CORRISPONDE ALLA LINEA FINALE DI ARRIVO
182000951017     C*   DEL TRAINO EMETTO MESSAGGIO FORZABILE E NON ABBINO
182100951025     C                   MOVEL     VIDLNA        COMLNA
182200951025    3C     TRNTFA        IFNE      COMLNA
182300951120    4C     *IN80         IFEQ      *ON
182400951120     C     WFORZ8        OREQ      ' '
182500951017     C                   MOVEL     MSG(29)       VIDMSG
182600951026     C                   SETON                                        904528
182700951017     C                   MOVEL     'S'           WFORZ3            1
182800951017     C                   GOTO      ENDCTR
182900951120    4C                   ENDIF
183000951120    3C                   ENDIF
183100951120    2C                   ENDIF
183200951024     C*
183300951024    1C                   ENDIF
183400951017     C*
183500951017     C     ENDCTR        ENDSR
183600960226     C*---------------------------------------------------------------*
183700971203     C* CERCO PER LA LNA TUTTI I SUOI TERMINAL DI ARRIVO
183800960226     C*---------------------------------------------------------------*
183900970102     C     RICEC2        BEGSR
184000970102     C                   CLEAR                   TFE
184100100616     C                   CLEAR                   TFEdef
184200971203     C                   Z-ADD     0             XX
184300100616     C                   Z-ADD     0             YY                3 0
184400970102     C*
184500970102     C     KCAE2         CHAIN     AZCAE02L                           31
184600960226     C     *IN31         DOWEQ     *OFF
184700960226     C     AMGDFV        IFGE      CAEDDE
184800960226     C     AMGDFV        ANDLE     CAEDSC
184900960226     C     CAEATB        ANDEQ     ' '
185000970102     C* RIMEPO UNA SCHIERA DI FILIALI TERMINAL DELLA LNA
185100970102     C     CAETFE        LOOKUP    TFE                                    31
185200970102     C  N31              ADD       1             XX                3 0
185300970102     C  N31              Z-ADD     CAETFE        TFE(XX)
185400970102     C                   ENDIF
185500970102     C*
185600970102     C     KCAE2         READE     AZCAE02L                               31
185700960226     C                   ENDDO
185800100616     c
185900100616     c* cerco i terminal alla data -1
186000100616     C     KCAE2         CHAIN     AZCAE02L                           31
186100100616     C     *IN31         DOWEQ     *OFF
186200100616     C     wdefmeno1     IFGE      CAEDDE
186300100616     C     wdefmeno1     ANDLE     CAEDSC
186400100616     C     CAEATB        ANDEQ     ' '
186500100616     C* RIMEPO UNA SCHIERA DI FILIALI TERMINAL DELLA LNA
186600100616     C     CAETFE        LOOKUP    TFEdef                                 31
186700100616     C  N31              ADD       1             yy                3 0
186800100616     C  N31              Z-ADD     CAETFE        TFEdef(yy)
186900100616     C                   ENDIF
187000100616     C*
187100100616     C     KCAE2         READE     AZCAE02L                               31
187200100616     C                   ENDDO
187300100616
187400960226     C                   ENDSR
187500951026     C*---------------------------------------------------------------*
187600951026     C* CONTROLLO IL NUMERO DI FOGLIO VIAGGIO IMMESSO
187700951026     C*---------------------------------------------------------------*
187800951026     C     CTRVAR        BEGSR
187900130502     c                   setoff                                       61
188000961213     C                   SETOFF                                       90
188100951026     C                   CLEAR                   VIDMSG
188200960124     C* SE VENGO DA INTERROGAZIONE NON CHAINO IL RECORD
188300130315     C     ilst4GES      IFNE      'N'
188400020805     C     K02FGV01      CHAIN     FNFGV01L                           8090
188500960124     C                   ELSE
188600020805     C     K02FGV01      CHAIN(N)  FNFGV01L                           80
188700960124     C                   ENDIF
188800960910     C*
188900960910     C* 90 ON - RECORD ALLOCATO
189000960910     C     *IN90         IFEQ      *ON
189100960910     C                   SETON                                        4828
189200130419     C                   MOVEL     MSG(67)       VIDMSG
189300960910     C                   GOTO      ENDVAR
189400960910     C                   ENDIF
189500951026      * FOGLIO INESISTENTE
189600951026     C     *IN80         IFEQ      *ON
189700951026     C                   SETON                                        489028
189800130419     C                   MOVEL     MSG(1)        VIDMSG
189900951026     C                   GOTO      ENDVAR
190000951026     C                   ENDIF
190100000516     C* ESTENSIONE FGV
190200020805     c                   eval      FGWnfv = VIDnfv
190300020805     c                   eval      FGWlnp = SIMfel
190400020805     c                   exsr      CTR_FGW
190500000602     C* POSSO MANUTENZIONARE SOLO SE SONO LA FGS
190600000605     C     FGWFGS        IFEQ      *ZEROS
190700000612     C                   Z-ADD     SIMFEL        WGWFGS
190800000612     C                   ELSE
190900000612     C                   Z-ADD     FGWFGS        WGWFGS
191000000605     C                   ENDIF
191100000612    2C     VIDFGS        IFNE      WGWFGS
191200000516     C     *IN98         ANDEQ     *OFF
191300000516     C                   SETON                                        489028
191400130419     C                   MOVEL     MSG(77)       VIDMSG
191500000516     C                   GOTO      ENDVAR
191600000516    2C                   ENDIF
191700951026     C*
191800951026     C* IN GESTIONE POSSO ANCHE VEDERE FOGLI ANNULLATI O CHIUSI
191900960201     C*  E SE RICHIAMATO COME MANUTENZIONE/INTERROGAZIONE
192000951026     C*  NEGLI ALTRI CASI SEMPRE ERRORE
192100951026     C*
192200951026     C* 52 ON - FOGLIO ANNULLATO
192300951026     C     FGVATB        IFEQ      '*'
192400951027     C                   SETON                                        529902
192500130315     C     ilst4GES      IFNE      ' '
192600130315     C     ilst4GES      ANDNE     'M'
192700130315     C     ilst4GES      ANDNE     'N'
192800951026     C                   SETON                                        904828
192900130419     C                   MOVEL     MSG(40)       VIDMSG
193000951026     C                   ENDIF
193100951026     C                   ENDIF
193200970102     C* 56 ON -  FOGLIO CHIUSO
193300130419    1C     FGVFCF        IFNE      *BLANKS
193400951027     C                   SETON                                        569902
193500130419    2C     ilst4GES      IFNE      ' '
193600130315     C     ilst4GES      ANDNE     'M'
193700130315     C     ilst4GES      ANDNE     'N'
193800951026     C                   SETON                                        904828
193900130419     C                   MOVEL     MSG(2)        VIDMSG
194000130419    2C                   ENDIF
194100080909     c* Se richiesta la manutenzione di un foglio già chiuso verifico che
194200080909     c* il foglio sia ancora presente nel file fogli di sede e che non
194300080909     c* risulti fatturato
194400130502    2c                   if        ilst4ges=' ' or ilst4ges='M'    or
194500130502    2c                             ilst4ges=' ' or ilst4ges='P'
194600080909     c     kfvs          chain     tnfvs01l
194700080910     c* se trovato verifico se fatturato
194800080910     c                   eval      wfat=' '
194900130419    3c                   if        %found(tnfvs01l)
195000080910     c     kfvs          chain     tntsr04l
195100130419    4c                   if        %found(tntsr04l)
195200080910     c     ktst          chain     tntst01l
195300130419    5c                   if        %found(tntst01l) and tstnft>0
195400080910     c                   eval      wfat='S'
195500130419    5c                   endif
195600130419    4c                   endif
195700130419    3c                   endif
195800080919     c* errore se relativo traino già fatturato o se foglio viaggio non
195900080919     c* presente in sede e data foglio più vecchia di 30 gg
196000130419    3c                   if        (Not %found(tnfvs01l) and fgvdfv<Dm30)
196100080910     c                             or wfat='S'
196200080910     C                   SETON                                        904828
196300130419     C                   MOVEL     MSG(91)       VIDMSG
196400130419    3c                   endif
196500130419    3c                   endif
196600130502     c                   seton                                        61
196700130419    2C                   ENDIF
196800990514      *
196900990514      *  NON E' POSSIBILE CHIUDERE UN FV DEFLUENZA SE LNA ESTERO
197000130315     C     ilst4GES      IFEQ      'C'
197100990514     C                   MOVEL     'TV'          COD
197200990514     C                   MOVEL(P)  FGVTTR        KEY
197300990514     C     KTAB          CHAIN     TABEL                              83
197400990514     C  N83              MOVEL     TBLUNI        DSTV
197500990514      *
197600990514     C     §TVDEF        IFEQ      'S'
197700990514     C                   MOVEL     FGVLNA        CODFIL
197800990514     C                   EXSR      CTRFIL
197900990514      *
198000060113     c                   if        esteroedpd = 'S'
198100130419     C                   MOVEL     MSG(73)       VIDMSG
198200990514     C                   SETON                                        489028
198300990514     C                   ENDIF
198400000405     C* Ripeto lo stesso controllo anche per le altre linee del foglio
198500000405     C                   Z-ADD     1             II                3 0
198600000420     C     II            DOWLE     299
198700000405     C     FFV(II)       IFGT      *ZEROS
198800000405     C                   MOVEL     FFV(II)       CODFIL
198900000405     C                   EXSR      CTRFIL
199000000405      *
199100060113     c                   if        esteroedpd = 'S'
199200000405     C                   MOVEL     MSG(73)       VIDMSG
199300000405     C                   SETON                                        489028
199400000405     C                   ENDIF
199500000405     C                   ENDIF
199600000405     C                   ADD       1             II
199700000405     C                   ENDDO
199800990514      *
199900990514     C                   ENDIF
200000990514     C                   CLEAR                   DSTV
200100990514     C                   ENDIF
200200990514      *
200300990514      * ERRORE
200400990514     C   90              GOTO      ENDVAR
200500951026     C*
200600130419     c     endvar        tag
200700130419     c
200800130419     c                   if        *in20 and vidmsg<>*blanks
200900130419     c                   movel     vidmsg        olst4msg
201000130419     c                   endif
201100130419     c
201200130419     C                   ENDSR
201300951024     C*---------------------------------------------------------------*
201400951024     C* CALL AL PGM TNSD19R DI RICERCA LINEE DI ARRIVO
201500951024     C*---------------------------------------------------------------*
201600951024     C     CASD19        BEGSR
201700951024     C                   MOVEL     *BLANKS       §COD
201800951024     C                   MOVEL     *BLANKS       §TIP
201900951024     C                   MOVEL     *BLANKS       §§DES
202000951024     C                   CALL      'TNSD19R'
202100951024     C                   PARM                    §COD              3
202200951024     C                   PARM                    §TIP              1
202300951024     C                   PARM                    §§DES            25
202400951024     C                   ENDSR
202500951024     C*---------------------------------------------------------------*
202600951024     C* RICERCO LA LINEA DI ARRIVO FINALE DEL TRAINO PER SIMFEL
202700951024     C*---------------------------------------------------------------*
202800951024     C     RICLNA        BEGSR
202900951024     C* LEGGO DALLA FERMATA CALCOLATA IN POI
203000951024     C     KTLR1         SETGT     TNTLR01L
203100951024     C     KTLR11        READE     TNTLR01L                               84
203200951024     C*
203300951024    3C     *IN84         DOWEQ     *OFF
203400951024     C     TLRTFM        ANDEQ     WTFM
203500951024     C* LEGGO SOLO SE FILIALE DI SCARICO E NON ANNULLATA
203600951024    4C     TLRATB        IFEQ      ' '
203700951024     C     TLRSCA        ANDEQ     'S'
203800000522     C                   MOVE      VIDFGS        WTFP
203900951024     C                   Z-ADD     TLRFIL        WTFA
204000951024     C                   EXSR      REDTLM
204100951025     C  N85              Z-ADD     TLRFIL        TRNTFA
204200951024    4C                   ENDIF
204300951024     C*
204400951024     C     KTLR11        READE     TNTLR01L                               84
204500951024    3C                   ENDDO
204600951024     C                   ENDSR
204700951024     C*---------------------------------------------------------------*
204800951024     C* LEGGO FERMATE E MC PER VEDERE SE ESISTE LIN.FINALE ARRIVO
204900951024     C*---------------------------------------------------------------*
205000951024     C     LEGFER        BEGSR
205100951024     C     KTLR3         CHAIN     TNTLR03L                           84
205200960202    1C     *IN84         DOWEQ     *OFF
205300960202     C* SE E' FERMATA DI SCARICO ELABORO
205400960202   1AC     TLRSCA        IFEQ      'S'
205500951024     C*
205600951024     C* VERIFICO SE ESISTE ANCHE NEI MC
205700951024     C                   EXSR      REDTLM
205800960202     C                   SETON                                        84
205900951024     C* TROVATO RECORD VALIDO
206000951024    2C  N85WTFM          IFEQ      'A'
206100951024     C                   MOVEL     'A'           W001
206200951024     C                   Z-ADD     TLRNFM        NFA(1)
206300951024     C                   ELSE
206400951024    3C     W001          IFEQ      'A'
206500951024     C                   MOVEL     'E'           W001
206600951024     C                   ELSE
206700951024     C                   MOVEL     'R'           W001
206800951024    3C                   ENDIF
206900951027     C                   Z-ADD     TLRNFM        NFA(2)
207000951024    2C                   ENDIF
207100960202     C*
207200960202  X1AC                   ELSE
207300960202     C* SE NON E' FERMATA DI SCARICO PROVO A RILEGGERE, PER I LOCALI
207400960202     C     KTLR3         READE     TNTLR03L                               84
207500960202   1AC                   ENDIF
207600960202    1C                   ENDDO
207700951024     C                   ENDSR
207800951024     C*-----------------------------------------------------***********
207900951024     C* LETTURA FILE METRI CUBI                             * REDTLM  *
208000951024     C*-----------------------------------------------------***********
208100951024     C     REDTLM        BEGSR
208200951024     C     KTLM          SETLL     TNTLM01L
208300951024     C     KTLM          READE     TNTLM01L                               85
208400951024    2C     *IN85         DOWEQ     *OFF
208500951024     C     TLMATB        ANDNE     ' '
208600951024     C     KTLM          READE     TNTLM01L                               85
208700951024    2C                   ENDDO
208800951024     C                   ENDSR
208900960122     C*-----------------------------------------------------***********
209000010920     C* LETTURA FILE TITLL00F LISTINI
209100960122     C*-----------------------------------------------------***********
209200960122     C     REDTLL        BEGSR
209300010920     C     KTLL1         SETLL     TITLL01L
209400010920     C     KTLL1         READE     TITLL01L                               83
209500960122    2C     *IN83         DOWEQ     *OFF
209600960122     C     TLLATB        ANDNE     ' '
209700010920     C     KTLL1         READE     TITLL01L                               83
209800960122    2C                   ENDDO
209900960122     C                   ENDSR
210000951013     C*-----------------------------------------------------***********
210100951013     C* COPIA CAMPI VIDEATA DA FILE TRAINI                  * RIETRV  *
210200951013     C*-----------------------------------------------------***********
210300951013     C     RIETRV        BEGSR
210400951013     C*
210500951130     C****  DATA FV  ****
210600951130     C                   MOVEL     AMGDFV        WSDFV
210700951130     C****  FILIALE FINALE DI ARRIVO  ****
210800951013     C                   MOVEL     VIDLNA        COMLNA
210900960126     C                   MOVEL     VIDLNA        CODFIL
211000960126     C                   EXSR      CTRFIL
211100960126     C  N34              MOVEL     ORGDES        DESLNA
211200960226     C                   MOVEL     VIDLNA        KTFA
211300960226     C                   MOVEL     'A'           KEPA
211400970102     C*
211500970102     C                   EXSR      RICEC2
211600971203     C  N34              MOVEA     TFE           TFEL
211700970102     C*
211800971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
211900971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
212000020805     C                   CLEAR                   FNLV55DS
212100971203     C                   MOVEL     'A'           D55TPT
212200971203     C                   MOVE      VIDLNA        D55LIN
212300000523     C                   MOVE      VIDFGS        D55LNP
212400971203     C                   MOVE      AMGDFV        D55DRF
212500971203     C                   CALL      'FNLV55R'
212600020805     C                   PARM                    FNLV55DS
212700971203     C                   MOVE      D55TFA        LNAFE0
212800951218     C*
212900960123     C****  BIS/ECCEZIONALE ****
213000960123     C                   MOVEL     TLTBIS        VIDBIS
213100960123     C*
213200960123     C****  TIPO AUTOMEZZO  ****
213300951218     C* SE NON  IMMESSO PRENDO QUELLO DI DEFAULT
213400951218    1C     VIDTMZ        IFEQ      ' '
213500951218     C                   MOVEL     TLTTMZ        VIDTMZ
213600951218     C                   EXSR      CTRTMZ
213700951218    1C                   ENDIF
213800960129     C*
213900960129     C****  TRAZIONISTA *****
214000960129     C     VIDPDR        IFLE      *ZEROS
214100960129     C     WTLPDR        ANDGT     0
214200130502     c     woblpd        andeq     ' '
214300960129     C                   MOVEL     WTLPDR        VIDPDR
214400000807     C                   Z-ADD     WTLPDR        KTLPDR
214500000807     C     KTNTLZ        CHAIN     TNTLZ01L                           83
214600960302     C* SE SI TRATTA DEL TRAZIONISTA FASULLO, SE IMMESSA LA DESCRIZIONE
214700960302     C*  LA TENGO, ALTRIMENTI CI METTO LA DECODIFICA DEL FASULLO
214800960302     C     VIDPDR        IFNE      '0465000'
214900960302     C  N83              MOVEL     TLZRSC        VIDDPD
215000960129     C                   ENDIF
215100960302     C                   ENDIF
215200960129     C*
215300951013     C*
215400951013     C****  TIPO TRAINO  ****
215500951013     C                   MOVEL     TLTTTR        VIDTTR
215600951027     C                   MOVEL     'TV'          COD
215700951027     C                   MOVEL(P)  VIDTTR        KEY
215800951027     C     KTAB          CHAIN     TABEL                              83
215900951027     C     *IN83         IFEQ      *OFF
216000951027     C     TBLFLG        ANDEQ     ' '
216100951027     C                   MOVEL     TBLUNI        DSTV
216200951027     C                   ELSE
216300951027     C                   CLEAR                   DSTV
216400951027     C                   ENDIF
216500960209     C                   MOVEL     §TVDES        DESTTR
216600951027     C* SE NON UTILIZZABILE IN FILIALE O ANNULLATO --> NON VISUALIZZO
216700951027     C     §TVUTF        IFNE      'S'
216800951027     C                   CLEAR                   VIDTTR
216900960209     C                   CLEAR                   DESTTR
217000951027     C                   ENDIF
217100110512     c
217200110511     C*
217300110512     c**** Punto di Scambio  ****
217400110512     c                   exsr      LetTLS
217500110512     c
217600110511    1c                   if        not *in30 and tlsatb=' '
217700110511     c                   eval      vidpsc=tlspsc
217800110530     c                   seton                                        08
217900110511     c     vidpsc        chain     tnpsc01l
218000110511    2c                   if        %found(tnpsc01l)
218100110511     c                   eval      despsc=pscloc
218200110530     c                   eval      despind=pscind
218300110530     c                   eval      desploc=%trim(psccap) + ' ' +
218400110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
218500110530     c
218600110511     c                   else
218700110511     c                   eval      despsc=*all'?'
218800110530     c                   eval      despind=*all'?'
218900110511    2c                   endif
219000130412     c
219100130412     c* Imposto la filiale finale di arrivo, se non c'e' il traino
219200130412     c                   eval      Dpscflo=pscflo
219300130412     c                   if        tlstrns=0  and
219400130412     c                                §pscflr>*zeros
219500130412     c                   movel     §pscflr       vidlnar
219600130412     c                   movel     §pscflr       codfil
219700130412     c                   exsr      ctrfil
219800130412     c  n34              movel     orgdes        deslnar2
219900130412     c  n34              movel     orgdes        deslnar
220000130412     c
220100130412     c                   endif
220200110511     c
220300110511     c                   eval      vidhhs=tlshhs
220400110530     c                   eval      vidtrns=tlstrns
220500110513     c* Decodifico il traino
220600130412     c                   if        vidtrns>0
220700110513     c                   exsr      Ctrtrns
220800130412     c                   endif
220900110511     c
221000110512     C                   MOVE      amgdfv        G02INV
221100110511     C                   MOVE      '3'           G02ERR
221200110511     C                   CALL      'XSRDA8'
221300110511     C                   PARM                    WLBDAT
221400110511     c* data scambio = data traino
221500110511    2c                   if        tlsggs='A'
221600110512     C                   MOVE      G02DAT        viddgs
221700110511    2c                   else
221800110511     c*
221900110512     c                   z-add     1             c
222000110511     C     TLsggs        LOOKUP    GFV(C)                                 31
222100110511    3c                   if        *in31
222200110511     c
222300110511     c     g02tgi        add       c             gi8tgi
222400110511     C                   CALL      'XSRGI8'
222500110511     C                   PARM                    WGIDAT
222600110512     C                   MOVEL     GI8DAT        viddgs
222700110511    3c                   endif
222800110511    2c                   endif
222900110511    1c                   endif
223000130318     c
223100130424     c* Chaino la proposta partenza, se presente per proporre i dati mancanti
223200130318     c                   if        ilst4NRRP>*zeros
223300130318     c                   movel     ilst4nrrp     w0130            13 0
223400130318     c     w0130         chain     fnfgt00f
223500130318     c                   if        %found(fnfgt00f)
223600130318     C****  TRAZIONISTA *****
223700130318     c                   if        fgtpdr>0
223800130318     C                   MOVEL     fgtPDR        VIDPDR
223900130318     C                   Z-ADD     fgtPDR        KTLPDR
224000130318     C     KTNTLZ        CHAIN     TNTLZ01L                           83
224100130318     C* SE SI TRATTA DEL TRAZIONISTA FASULLO, SE IMMESSA LA DESCRIZIONE
224200130318     C*  LA TENGO, ALTRIMENTI CI METTO LA DECODIFICA DEL FASULLO
224300130318     C     VIDPDR        IFNE      '0465000'
224400130318     C  N83              MOVEL     TLZRSC        VIDDPD
224500130318     C                   ENDIF
224600130318     C                   ENDIF
224700130318     c
224800130318     c                   movel     fgtdcn        viddcn
224900130318     c                   movel     fgttrm        vidtrm
225000130318     c                   movel     fgttrr        vidtrr
225100130318     c                   movel     fgtnot        vidnt1
225200130628     c                   movel     fgtnt2        vidnt2
225300130628     c                   movel     fgtan1        vidan1
225400130628     c                   movel     fgtan2        vidan2
225500130424     c* reperisco il cellulare del conducente
225600130424     c                   eval      kdcn=fgtdcn
225700130424     c                   eval      kpdr=fgtpdr
225800130424     c     kata          chain     aiata01l
225900130424     c                   if        %found(aiata01l)
226000130424     c                   eval      vidcnc=atatel
226100130424     c                   endif
226200130318
226300130318     c                   endif
226400130318     c                   endif
226500130510     c
226600130510     c                   if        vidttr<>'L'
226700130510     c                             and vidttr<>'H' and vidttr<>'D'
226800130510     c                             and vidttr<>' '
226900130510
227000130516     c                   if        viddcn=*blanks and vidpdr>*zeros
227100130516     c                   seton                                        55
227200130516     c***                eval      viddcn='?'
227300130510     c                   endif
227400130516     c                   if        vidtrm=*blanks and vidpdr>*zeros and
227500130516     c                             viddcn<>*blanks
227600130516     c***                eval      vidtrm='?'
227700130516     c                   seton                                        54
227800130510     c                   endif
227900130510     c                   endif
228000130502     c
228100130502     C****  IMPORTO PATTUITO  ****
228200130502     C* SE NON E' LA FILIALE CHE DEVE ESPORRE L'IMPORTO NON E' INSERIB
228300130502    1C     VIDFGS        IFEQ      TLTFES
228400130502     C     WTFM          ANDEQ     WFMFES
228500130502     C                   EXSR      LEGLIS
228600130502     C  N83              MOVEL     TLLDIV        VIDVIP
228700130502     C  N83              MOVEL     TLLDIV        SAVDIV
228800130502     C  N83              Z-ADD     TLLILI        VIDIPT
228900130502    1C                   ENDIF
229000951013     C*
229100951017     C* SALVO I CAMPI PER CONTROLLARE SE VIENE MODIFICATO QUALCOSA
229200951025     C                   MOVEL     VIDPDR        TRNPDR
229300951025     C                   MOVEL     VIDTMZ        TRNTMZ
229400010920     C                   MOVEL     VIDVIP        TRNVIP
229500010920     C                   Z-ADD     VIDIPT        TRNIPT
229600951027     C                   MOVEL     §TVUTF        TRNUTF
229700960130     C                   MOVEL     VIDTTR        SAVTTR
229800960130     C                   MOVEL     VIDBIS        SAVBIS
229900110511     C                   MOVEL     VIDpsc        SAVpsc
230000110530     C                   clear                   SAVlnar
230100110511     C                   MOVEL     VIDtrns       SAVtrns
230200130517     c
230300130517     c                   clear                   viddtt
230400130517     c                   clear                   vidhmt
230500130517     c* calcolo data teorica per visualizzarla
230600130517     C                   CLEAR                   TNTL35DS
230700130517     C                   MOVEL     'T'           KTDH
230800130517     C                   MOVEL     'P'           KEPA
230900130517     C                   MOVEL     'P'           D35EPA
231000130517     C                   MOVEL     'D'           D35TUP
231100130517     C                   MOVEL     VIDTRN        D35TRN
231200130517     C                   MOVEL     WDDE          D35DDE
231300130517     C                   MOVEL     WDTN          D35DTN
231400130517     C                   CLEAR                   WTFA
231500130517     C     WTFM          IFEQ      'A'
231600130517     C                   Z-ADD     NFP(1)        d35NFM
231700130517     C                   ELSE
231800130517     C                   Z-ADD     NFP(2)        d35NFM
231900130517     C                   ENDIF
232000130517     C                   MOVEL     TNTL35DS      KPJBU
232100130517     C                   CALL      'TNTL35R'
232200130517     C                   PARM                    KPJBA
232300130517     C                   MOVEL     KPJBU         TNTL35DS
232400130517     C* C'E' ERRORE
232500130517     C     D35ERR        IFeq      *BLANKS
232600130517     C                   MOVEL     D35HTE        vidhmt
232700130517     C                   MOVEL     D35DTE        g02inv
232800130517     C                   MOVE      *ZEROS        G02DAT
232900130517     C                   MOVEL     '3'           G02ERR
233000130517     C                   CALL      'XSRDA8'
233100130517     C                   PARM                    WLBDAT
233200130517     C                   Z-ADD     G02DAT        VIDDTT
233300130517     c                   endif
233400951018     C*
233500951013     C                   ENDSR
233600110512     C*-----------------------------------------------------***********
233700110512     c* Lettura file TLS degi punti di scambio
233800110512     C*-----------------------------------------------------***********
233900110512     c     LetTLS        BEGSR
234000110512     c
234100110512     c**** Punto di Scambio  ****
234200110512     c* Giro la data foglio viaggio per avere il giorno della settimana
234300110512     C                   CLEAR                   WLBDAT
234400110512     C                   MOVE      amgdfv        G02INV
234500110512     C                   MOVE      '3'           G02ERR
234600110512     C                   CALL      'XSRDA8'
234700110512     C                   PARM                    WLBDAT
234800110512     C                   MOVE      G02DAT        W0020             2 0
234900110512     C                   MOVEL     G02DAT        GGMMAA
235000110512     C                   MOVE      W0020         GGMMAA
235100110512     C*
235200110512     C                   CALL      'XGIOSE1'
235300110512     C                   PARM                    GGMMAA            6
235400110512     C                   PARM                    GIOSET            1
235500110512     c
235600110531     c                   clear                   dstls
235700110512     C     KTLS          CHAIN     TNTLS01L                           30
235800110512     C*
235900110512     C* SE NON TROVATO O ANNULLATO, PROVO CON IL GIORNO DELLA
236000110512     C*   SETTIMANA = *BLANKS
236100110512    1C     *IN30         IFEQ      *ON
236200110512     C     TLSATB        ORNE      ' '
236300110512     C                   MOVEL     ' '           GIOSET
236400110531     c                   clear                   dstls
236500110512     C     KTLS          CHAIN     TNTLS01L                           30
236600110512    1C                   ENDIF
236700110512     c                   ENDSR
236800951218     C*-----------------------------------------------------***********
236900951218     C* LEGGO I LISTINI PER PRENDERE IMPORTO FV
237000951218     C*-----------------------------------------------------***********
237100951218     C     LEGLIS        BEGSR
237200951218     C* PER CERCARE L'IMPORTO 4 TENTATIVI:
237300951218     C*  1) CON CODICI SPECIFICI
237400951218     C                   MOVEL     VIDTMZ        KTMZ
237500951218     C                   MOVEL     VIDPDR        KPDR
237600951218     C                   EXSR      REDTLL
237700951218     C*  2) CON TRAZIONISTA SPECIFICO E TIPO AUTOMEZZO = 99
237800951218    1C     *IN83         IFEQ      *ON
237900951218     C                   MOVEL     '99'          KTMZ
238000951218     C                   EXSR      REDTLL
238100951218    1C                   ENDIF
238200951218     C*  3) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO SPECIFICO
238300951218    1C     *IN83         IFEQ      *ON
238400960206     C                   CLEAR                   KTMZ
238500951218     C                   MOVEL     VIDTMZ        KTMZ
238600951218     C                   MOVEL     9999999       KPDR
238700951218     C                   EXSR      REDTLL
238800951218    1C                   ENDIF
238900951218     C*  2) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO = 99
239000951218    1C     *IN83         IFEQ      *ON
239100951218     C                   MOVEL     '99'          KTMZ
239200951218     C                   MOVEL     9999999       KPDR
239300951218     C                   EXSR      REDTLL
239400951218    1C                   ENDIF
239500951218     C* 83 OFF - TROVATO
239600951218     C* ESPONGO IMPORTO SUL FV
239700010718     C* 18/07/01: IMPOSTO SEMPRE FISSO "N" PERCHÈ NON LO VOGLIO PIU'
239800010718     C*            STAMPARE
239900951218    2C  N83TLLIES        IFEQ      0
240000951218     C                   MOVEL     'N'           VIDSNS
240100951218     C                   ELSE
240200010718     C                   MOVEL     'N'           VIDSNS
240300951218    2C                   ENDIF
240400951218     C                   ENDSR
240500951017     C*-----------------------------------------------------***********
240600951017     C* CONTROLLI FORMATO 1                                 *  CTR1   *
240700951017     C*-----------------------------------------------------***********
240800951024     C     CTRD02        BEGSR
240900951027     C                   SETOFF                                       90
241000960318     C                   SETOFF                                       404142
241100951027     C                   SETOFF                                       434445
241200960129     C                   SETOFF                                       464759
241300130419     C                   SETOFF                                       535455
241400000515     C                   SETOFF                                       575849
241500011012     C                   SETOFF                                       6038
241600110512     C                   SETOFF                                       747576
241700951027     C*
241800951027     C****  TIPO TRAINO  ****
241900951027     C* RICERCA
242000951027    1C     VIDTTR        IFEQ      '?'
242100951027     C                   MOVEL     'TV'          COD
242200951027     C                   MOVEL     *BLANKS       VIDTTR
242300951027     C                   MOVEL     *BLANKS       DES              25
242400000316     C                   CLEAR                   KEY
242500951027     C                   CALL      'X§TABER'
242600951027     C                   PARM                    CODUT
242700951027     C                   PARM                    COD
242800951027     C                   PARM                    KEY
242900951027     C                   PARM                    DES
243000951027     C                   MOVEL     KEY           VIDTTR
243100951027     C                   MOVEL     DES           DESTTR
243200951027     C                   SETON                                        9042
243300951027     C                   GOTO      ENDCT2
243400951027    1C                   ENDIF
243500951027     C*
243600000316     C* CONTROLLO VALIDITA' TIPO TRAINO
243700000316     C                   CLEAR                   DSTV
243800000316     C                   CLEAR                   DESTTR
243900000316     C*
244000951027     C                   MOVEL     'TV'          COD
244100951027     C                   MOVEL(P)  VIDTTR        KEY
244200951027     C     KTAB          CHAIN     TABEL                              83
244300951027     C  N83              MOVEL     TBLUNI        DSTV
244400000828     C                   MOVEL     §TVDEF        WDEF1A            1
244500951027    2C     *IN83         IFEQ      *ON
244600951027     C     TBLFLG        ORNE      ' '
244700951027     C     §TVUTF        ORNE      'S'
244800951027     C                   MOVEL     MSG(46)       VIDMSG
244900951027     C                   SETON                                        904228
245000951027     C                   GOTO      ENDCT2
245100951027    2C                   ENDIF
245200960209     C*
245300960209     C                   MOVEL     §TVDES        DESTTR
245400951027     C*
245500951027     C* SE MODIFICA PULISCO LA FORZATURA
245600951027     C     VIDTTR        IFNE      SAVTTR
245700951027     C                   CLEAR                   WFORZ7
245800960202     C                   MOVEL     'S'           WCALNA
245900951027     C                   MOVEL     VIDTTR        SAVTTR
246000951027     C                   ENDIF
246100960130     C* SE MODIFICA PULISCO LA FORZATURA
246200960130     C     VIDBIS        IFNE      SAVBIS
246300960130     C                   CLEAR                   WFORZD
246400960130     C                   MOVEL     VIDBIS        SAVBIS
246500960130     C                   ENDIF
246600100616     c
246700040204     C****  DATA FOGLIO VIAGGIO  ****
246800040204    1C     *IN01         IFEQ      *OFF
246900130318     c                   movel     'S'           wd02              1
247000040204     C                   EXSR      CALDAT
247100040204     C   90              GOTO      ENDCT2
247200040204     C* SE FOGLIO ABBINATO E MODIFICO LA DATA --> SBINO: AVVISO
247300040204    2C     WFORZ9        IFEQ      ' '
247400040204     C     VIDTRN        ANDGT     0
247500040204     C     WSDFV         ANDNE     AMGDFV
247600040204     C                   MOVEL     MSG(5)        VIDMSG
247700040204     C                   SETON                                        904428
247800040204     C                   MOVEL     'S'           WFORZ9            1
247900040204     C                   GOTO      ENDCT2
248000040204    2C                   ENDIF
248100040204    1C                   ENDIF
248200951017     C*
248300951017     C****  FILIALE FINALE D'ARRIVO  ****
248400951026     C* SOLO SE MODIFICATA  E SOLO IN IMMISSIONE/VARIAZIONE
248500951026     C  N01VIDLNA        IFNE      SAVLNA
248600951017     C                   EXSR      CTRLNA
248700060828     C   90              GOTO      ENDCT2
248800060828     C* SALVO LA LINEA DI ARRIVO se tutto ok
248900060828     C                   MOVEL     VIDLNA        SAVLNA
249000060828     C                   MOVEL     'S'           WCALNA            1
249100951025     C                   ENDIF
249200951027     C*
249300951027     C* SE TRAINO DI DEFLUENZA O LOCALE, VERIFICO SE LE LINEE SONO
249400951027     C*  CORRETTE
249500951027     C                   MOVEL     VIDLNA        CODFIL
249600100616     c
249700951027    1C     VIDTTR        IFEQ      'L'
249800951027     C     §TVDEF        OREQ      'S'
249900100616     c                   setoff                                       31
250000100616     c* se terminal di arrivo al giorno prima --> OK lo stesso
250100100616     c                   if        §tvdef='S'
250200100616     C     VIDFGS        LOOKUP    TFEdef                                 31
250300100616     c                   endif
250400970102     C*
250500100616     c                   if        not *in31
250600000612     C     VIDFGS        LOOKUP    TFEL                                   31
250700970102    2C     *IN31         IFEQ      *OFF
250800951027     C                   MOVEL     MSG(47)       VIDMSG
250900951027     C                   SETON                                        284590
251000951027     C                   GOTO      ENDCT2
251100970102    2C                   ENDIF
251200951031    1C                   ENDIF
251300100616     c                   endif
251400100616     c
251500960302     c* se la lna immessa E' UN 2 LIVELLO IN ARRIVO DEVE ESSERE O TRAIN
251600960302     C* DI DEFLUENZA O LOCALE O INOLTRO
251700000612     C     VIDFGS        IFEQ      LNAFE0
251800960302    2C     VIDTTR        IFNE      'L'
251900960302     C     VIDTTR        ANDNE     'I'
252000960302     C     §TVDEF        ANDNE     'S'
252100960302     C                   MOVEL     MSG(66)       VIDMSG
252200960302     C                   SETON                                        284290
252300960302     C                   GOTO      ENDCT2
252400960302    2C                   ENDIF
252500960302    1C                   ENDIF
252600951031     C* PER  UNA DEFLUENZA LNP <> DA LNA
252700000612     C                   MOVE      VIDFGS        W003A             3
252800951031     C     §TVDEF        IFEQ      'S'
252900000612     C     W003A         ANDEQ     VIDLNA
253000951027     C                   MOVEL     MSG(50)       VIDMSG
253100951027     C                   SETON                                        284590
253200951027     C                   GOTO      ENDCT2
253300951031     C                   ENDIF
253400961216     C**
253500961216     C** SOLO PER DEFLUENZE E LNA ESTERA E FV NON ANCORA PARTITO
253600961216     C**
253700961213     C*
253800951218     C* CONTROLLO DEL TIPO AUTOMEZZO E DEL TRAZIONISTA
253900951218     C                   EXSR      CTRPAR
254000951218     C   90              GOTO      ENDCT2
254100951026     C*
254200951026     C**** CONDUCENTE  ****
254300951026     C* OBBLIGATORIO IN BORDERO'/CHIUSURA FV
254400960205     C* NON OBBLIGATORIO SE E' LOCALE
254500951026     C   01VIDDCN        IFEQ      *BLANKS
254600960205     C     VIDTTR        ANDNE     'L'
254700010621     C                   MOVEL     MSG(18)       VIDMSG
254800951026     C                   SETON                                        905528
254900951026     C                   GOTO      ENDCT2
255000951026    2C                   ENDIF
255100130424     c
255200130424     c* Se messo '?' cerco conducente tra quelli del trazionista
255300130424     c                   eval      indx=%scan('?':viddcn)
255400130424    2c                   if        indx>0   and vidpdr>*zeros
255500130424     c                   clear                   tntl83ds
255600130424     c                   eval      i83soc=soctraini
255700130424     c                   eval      i83ksc=%int(vidpdr)
255800130424     c                   eval      i83mod='1'
255900130424     c                   eval      kpjbu=tntl83ds
256000130424     c                   call      'TNTL83R'
256100130424     c                   parm                    kpjba
256200130424     c                   eval      tntl83ds=kpjbu
256300130424    3c                   if        o83_aut<>*blanks
256400130424     c                   eval      viddcn=o83_aut
256500130424     c                   eval      vidcnc=o83_tel
256600130424     c                   seton                                        9055
256700130424     C                   GOTO      ENDCT2
256800130424     c                   else
256900130424     c                   clear                   viddcn
257000130429     c                   seton                                        9055
257100130429     C                   GOTO      ENDCT2
257200130424    3c                   endif
257300130424     c
257400130424   x2c                   else
257500130424    3c                   if        indx>0
257600130424     C                   MOVEL     MSG(106)      VIDMSG
257700130424     C                   SETON                                        905528
257800130424     C                   GOTO      ENDCT2
257900130424    3c                   endif
258000130424    2c                   endif
258100130503     c* se cambiato trazionista ricontrollo conducente  e targa
258200130424     c                   if        savpdr<>vidpdr
258300130424     c                   clear                   wforzD1
258400130424     c                   clear                   wforzM1
258500130424     c                   movel     vidpdr        savpdr
258600130424     c                   endif
258700130424     c                   if        savdcn<>viddcn
258800130424     c                   clear                   wforzD1
258900130424     c                   movel     viddcn        savdcn
259000130424     c                   endif
259100130424     c
259200130424     c* Controllo se accreditato per il trazionista
259300130508     c*  escludo traini H D L
259400130424    2c                   if        viddcn<>*blanks and vidpdr>*zeros
259500130508
259600130424     c                   eval      kdcn=viddcn
259700130424     c                   eval      kpdr=%int(vidpdr)
259800130424     c     kata          chain     aiata01l
259900130508     c
260000130508   2ac                   if        vidttr<>'H' and vidttr<>'D'
260100130508     c                             and vidttr<>'L'
260200130508
260300130424    3c                   if        wforzd1=' ' and (not %found(aiata01l)
260400130424     c                             or   atadtfir>0)
260500130510     c                   if        *in80 or fgvpmb<>viddcn
260600130510     C                   MOVEL     MSG(107)      VIDMSG
260700130510     c                   else
260800130510     C                   MOVEL     MSG(111)      VIDMSG
260900130510     c                   endif
261000130510     c                   eval      WforzD1='A'
261100130424     C                   SETON                                        905528
261200130424     C                   GOTO      ENDCT2
261300130424    3c                   endif
261400130508   2ac                   endif
261500130424     c*
261600130424    3c                   if        %found(aiata01l) and atadtfir=0
261700130514     c                   if        vidcnc=*blanks
261800130424     c                   eval      vidcnc=atatel
261900130514     c                   endif
262000130424     c*
262100130424   x3c                   else
262200130424     c* Se trovo comunque il conducente propongo il suo cellulare
262300130514   3ac                   if        vidcnc=*blanks
262400130424     c     viddcn        setll     aiata02l
262500130424     c     viddcn        reade     aiata02l
262600130424    4c                   dow       not %eof(aiata02l)
262700130424    5c                   if        atadtfir=0
262800130424     c                   eval      vidcnc=atatel
262900130424     c                   leave
263000130424    5c                   endif
263100150916     c     viddcn        reade     aiata02l
263200130424    4c                   enddo
263300130514   3ac                   endif
263400130424     c
263500130424    3c                   endif
263600130424     c
263700130424    2c                   endif
263800130424     c
263900951026     C*
264000010622      **** CELLULARE ****
264100010622     C* OBBLIGATORIO IN BORDERO'/CHIUSURA FV
264200010622     C* NON OBBLIGATORIO SE E' LOCALE
264300130424     C   01VIDCNC        IFEQ      *BLANKS
264400010621     C     VIDTTR        ANDNE     'L'
264500010621     C                   MOVEL     MSG(75)       VIDMSG
264600010621     C                   SETON                                        906028
264700010621     C                   GOTO      ENDCT2
264800010621    2C                   ENDIF
264900130424     C     VIDCNC        IFNE      *BLANKS
265000010704     C     ' '           CHECKR    VIDCNC        CONTA             2 0
265100010704     C     CONTA         IFEQ      0
265200010704     C     CONTA         ORLT      7
265300010704     C                   MOVEL     MSG(76)       VIDMSG
265400010704     C                   SETON                                        902860
265500010704     C                   GOTO      ENDCT2
265600010703     C                   ENDIF
265700010704     C                   ENDIF
265800010621     C*
265900951026     C**** TARGHE ****
266000951026     C* OBBLIGATORIA ALMENO UNA IN BORDERO'/CHIUSURA FV
266100960205     C* NON OBBLIGATORIE SE E' LOCALE
266200951026     C   01VIDTRM        IFEQ      *BLANKS
266300951026     C     VIDTRR        ANDEQ     *BLANKS
266400960205     C     VIDTTR        ANDNE     'L'
266500951026     C                   MOVEL     MSG(11)       VIDMSG
266600951026     C                   SETON                                        549028
266700951026     C                   GOTO      ENDCT2
266800951026     C                   ENDIF
266900130424     c
267000130509     c* Se messo '?' cerco targa tra quelli del trazionista
267100130424     c                   eval      indx=%scan('?':vidTRM)
267200130424    2c                   if        indx>0   and vidpdr>*zeros
267300130424     c                   clear                   tntl84ds
267400130424     c                   eval      i84soc=soctraini
267500130424     c                   eval      i84ksc=%int(vidpdr)
267600130424     c                   eval      i84mod='1'
267700130424     c                   eval      kpjbu=tntl84ds
267800130424     c                   call      'TNTL84R'
267900130424     c                   parm                    kpjba
268000130424     c                   eval      tntl84ds=kpjbu
268100130424    3c                   if        o84_TRM<>*blanks
268200130424     c                   eval      vidtrm=o84_trm
268300130424     c                   seton                                        9054
268400130424     C                   GOTO      ENDCT2
268500130424     c                   else
268600130424     c                   clear                   vidtrm
268700130429     c                   seton                                        9054
268800130429     C                   GOTO      ENDCT2
268900130424    3c                   endif
269000130424     c
269100130424   x2c                   else
269200130424    3c                   if        indx>0
269300130424     C                   MOVEL     MSG(108)      VIDMSG
269400130424     C                   SETON                                        905428
269500130424     C                   GOTO      ENDCT2
269600130424    3c                   endif
269700130424    2c                   endif
269800130424     c
269900130424     c* Controllo se accreditato per il trazionista
270000130510     c* prima conpatto la targa
270100130510      /free
270200130510             if  vidtrm <> *blank;
270300130510             xi=1  ;
270400130510               for  xi = 1  to  %len( %trimr( vidtrm ) );
270500130510                 if  %subst( vidtrm : xi : 1 ) = *blank;
270600130510                   vidtrm = %subst( vidtrm : 1 : xi - 1 ) +
270700130510                            %subst( vidtrm : xi + 1 );
270800130510                   xi -= 1;
270900130510                 endif;
271000130510               endfor;
271100130510             endif;
271200130510             if  vidtrr <> *blank;
271300130510             xi=1  ;
271400130510               for  xi = 1  to  %len( %trimr( vidtrr ) );
271500130510                 if  %subst( vidtrr : xi : 1 ) = *blank;
271600130510                   vidtrr = %subst( vidtrr : 1 : xi - 1 ) +
271700130510                            %subst( vidtrr : xi + 1 );
271800130510                   xi -= 1;
271900130510                 endif;
272000130510               endfor;
272100130510             endif;
272200130510      /end-free
272300130424     c                   if        vidtrm<>savtrm
272400130424     c                   clear                   wforzM1
272500130424     c                   movel     vidtrm        savtrm
272600130424     c                   endif
272700130424     c
272800130424    2c                   if        vidtrm<>*blanks and vidpdr>*zeros
272900130509     c                             and wforzm1=' '  and
273000130509   2ac                             vidttr<>'H' and vidttr<>'D'
273100130509     c                             and vidttr<>'L'
273200130424     c                   eval      kpdr=%int(vidpdr)
273300130424     c     katm          chain     aiatm01l
273400130424    3c                   if        (not %found(aiatm01l)
273500130424     c                             or   atmdtfir>0)
273600130510     c* in immissione o variazione
273700130510     c                   if        *in80 or fgvtrm<>vidtrm
273800130510     C                   MOVEL     MSG(109)      VIDMSG
273900130510     c                   else
274000130510     C                   MOVEL     MSG(112)      VIDMSG
274100130510     c                   endif
274200130510     c                   eval      WforzM1='A'
274300130424     C                   SETON                                        905428
274400130424     C                   GOTO      ENDCT2
274500130509   x3c                   else
274600130509     c
274700130509     c* Se accreditato vedo se ha la data di controllo --> errore forzabile
274800130509    4c                   if        ATMDTCON=0
274900130509     C                   MOVEL     MSG(110)      VIDMSG
275000130510     c                   eval      WforzM1='C'
275100130509     C                   SETON                                        905428
275200130509     C                   GOTO      ENDCT2
275300130509    4c                   endif
275400130509     c
275500130424    3c                   endif
275600130424    2c                   endif
275700130424     c
275800010928      * DIVISA
275900011204     C     VIDIPT        IFNE      *ZEROS
276000011012      * la divisa è obbligatoria
276100011012     C     VIDVIP        IFEQ      *BLANKS
276200011012     C                   SETON                                        903828
276300011012     C                   MOVEL     MSG(86)       VIDMSG
276400011012     C                   GOTO      ENDCT2
276500011012     C                   ENDIF
276600010928      * controllo se divisa valida
276700010928     C                   CLEAR                   DSCV
276800010928     C                   CLEAR                   KEY
276900010928     C                   MOVEL     'CV'          COD
277000010928     C                   MOVEL     VIDVIP        KEY
277100010928     C     KTAB          CHAIN     TABEL00F                           83
277200010928     C     *IN83         IFEQ      *ON
277300010928     C                   SETON                                        903828
277400010928     C                   MOVEL     MSG(79)       VIDMSG
277500010928     C                   GOTO      ENDCT2
277600010928     C                   ENDIF
277700010928     C                   MOVEL     TBLUNI        DSCV
277800011204     C* Se non sono in immissione e non ho variato la divisa
277900011204     C* non faccio questo controllo per non dare errore in caso di
278000011204     C* manutenzione o chiusura in gennaio di un F.V.P. di dicembre
278100011204     C     *IN80         IFEQ      *ON
278200011204     C     VIDVIP        ORNE      SAWVIP
278300010928     C     §CVTRA        IFNE      'S'
278400010928     C                   SETON                                        903828
278500010928     C                   MOVEL     MSG(80)       VIDMSG
278600011012     C                   GOTO      ENDCT2
278700010928     C                   ENDIF
278800011204     C                   ENDIF
278900011001      * se modificata la divisa e non l'importo devo dare msg di errore
279000011001     C     VIDVIP        IFNE      SAVDIV
279100011001     C     *IN37         ANDEQ     *OFF
279200011012     C     VIDVIP        ANDNE     *BLANKS
279300011015     C     SAVDIV        ANDNE     *BLANKS
279400011001     C                   SETON                                        905828
279500011001     C                   MOVEL     MSG(85)       VIDMSG
279600011001     C                   ENDIF
279700011001     C     *IN38         IFEQ      *OFF
279800011015     C     *IN58         ANDEQ     *OFF
279900011001     C                   MOVEL     VIDVIP        SAVDIV
280000011001     C                   ENDIF
280100010928      * controllo se sono ok i decimali nell'importo pattuito
280200010928     C                   MOVE      VIDIPT        DECIMI            2 0
280300010928     C     DECIMI        IFGT      *ZEROS
280400010928     C     §CVFDC        ANDNE     'S'
280500010928     C                   SETON                                        905828
280600010928     C                   MOVEL     MSG(84)       VIDMSG
280700010928     C                   ENDIF
280800010928     C                   ENDIF
280900950901     C*
281000951017     C* S E   P R O V E N G O   D A   C O P I A
281100951026     C* OPPURE SONO IN VARIAZIONE DI UN FV GIA' ABBINATO IN PRECEDENZA
281200960304    1C     VIDTRN        IFGT      0
281300950901     C*
281400950901     C* CODICE TRAZIONISTA OBBLIGATORIO
281500951024     C     VIDPDR        IFLE      *ZEROS
281600951027     C     TRNPDR        ANDGT     *ZEROS
281700951018     C                   MOVEL     MSG(9)        VIDMSG
281800951026     C                   SETON                                        904728
281900951024     C                   GOTO      ENDCT2
282000951005    2C                   ENDIF
282100950901     C* TIPO AUTOMEZZO OBBLIGATORIO
282200951005    2C     VIDTMZ        IFEQ      ' '
282300951027     C     TRNTMZ        ANDNE     ' '
282400951018     C                   MOVEL     MSG(26)       VIDMSG
282500950901     C                   SETON                                        905728
282600951024     C                   GOTO      ENDCT2
282700951005    2C                   ENDIF
282800950901     C*
282900951005     C* SE VENGONO VARIATI TIPO AUTOMEZZO E TRAZIONISTA RIPROPONGO
283000951005     C*   L'IMPORTO LISTINO: SE NON TROVO IL RECORD ERRORE FORZABILE
283100000602     C*   SOLO SE FGS    E' LA PRIMA FILIALE FERMATA
283200000612    2C     VIDFGS        IFEQ      TLTFES
283300951123     C     WTFM          ANDEQ     WFMFES
283400951025    3C     VIDTMZ        IFNE      TRNTMZ
283500951025     C     VIDPDR        ORNE      TRNPDR
283600951218     C                   MOVEL     VIDTMZ        TRNTMZ
283700951218     C                   MOVEL     VIDPDR        TRNPDR
283800951218     C                   EXSR      LEGLIS
283900951024     C*
284000951024     C* 43 ON - ILLUMINO IMPORTO LISTINO PERCHE' CAMBIATO
284100951218    4C     *IN83         IFEQ      *OFF
284200951024    5C     TLLILI        IFNE      VIDIPT
284300010920     C                   MOVEL     TLLDIV        VIDVIP
284400010920     C                   Z-ADD     TLLILI        VIDIPT
284500010920     C                   MOVEL     TLLDIV        TRNVIP
284600010920     C                   Z-ADD     TLLILI        TRNIPT
284700951024     C                   SETON                                        9043
284800951024     C                   GOTO      ENDCT2
284900951024    5C                   ENDIF
285000951024     C*
285100951024   X4C                   ELSE
285200951005     C*
285300951005     C* SE ARRIVO A QUESTO PUNTO SIGNIFICA CHE NON HO TROVATO IL RECORD
285400951005     C*   NEL FILE DEI LISTINI QUINDI AVVISO
285500010920     C                   CLEAR                   TRNVIP
285600951025     C                   CLEAR                   TRNIPT
285700951005     C                   MOVEL     MSG(27)       VIDMSG
285800951005     C                   SETON                                        905828
285900951024     C                   GOTO      ENDCT2
286000951024    4C                   ENDIF
286100951024    3C                   ENDIF
286200951024    2C                   ENDIF
286300951027     C*
286400951027     C* TIPO TRAINO NON SI PUO' MODIFICARE
286500951027    2C     WFORZ7        IFEQ      ' '
286600951027    3C     TRNUTF        IFEQ      'S'
286700951027     C     TLTTTR        ANDNE     VIDTTR
286800951027     C     TRNUTF        ORNE      'S'
286900951027     C     VIDTTR        ANDNE     ' '
287000951027     C                   MOVEL     MSG(25)       VIDMSG
287100951027     C                   MOVEL     'S'           WFORZ7            1
287200951027     C                   SETON                                        904228
287300951027     C                   GOTO      ENDCT2
287400951027    3C                   ENDIF
287500951027    2C                   ENDIF
287600960130     C* CATEGORIA TRAINO NON SI PUO' MODIFICARE
287700000406     C* MA SE DEFLUENZA ESTERO O DPD MI DISINTERESSO
287800960130    2C     WFORZD        IFEQ      ' '
287900960130     C     TLTBIS        ANDNE     VIDBIS
288000000405     C     §TVDEF        ANDNE     'S'
288100000405    2C     WFORZD        OREQ      ' '
288200000405     C     TLTBIS        ANDNE     VIDBIS
288300020213     c                   if        okestero = *blanks
288400960130     C                   MOVEL     MSG(65)       VIDMSG
288500960130     C                   MOVEL     'S'           WFORZD            1
288600960318     C                   SETON                                        904128
288700960130     C                   GOTO      ENDCT2
288800020213     c                   endif
288900960130    2C                   ENDIF
289000110530     c* se traino trasformato in BIS e punto di scambio con traino
289100110530     c*  elimino anche punto di scambio
289200110530     c                   if        tltbis<>vidbis and vidbis<>*blanks
289300110530     c                             and vidpsc<>*blanks and vidtrns>0
289400110530     C                   MOVEL     MSG(103)      VIDMSG
289500110530     C                   SETON                                        904128
289600110530     c                   clear                   vidtrns
289700110601     c                   clear                   destrns
289800110530     c                   clear                   vidpsc
289900130412     c                   clear                   vidlnar
290000130412     c                   clear                   deslnar
290100130412     c                   clear                   deslnar2
290200110601     c* non devo dare errore per punto di scmabio modificato
290300110601     c                   clear                   savpsc
290400110601     c                   eval      WforzSC='S'
290500110601     c                   clear                   despsc
290600110530     c                   clear                   viddgs
290700110530     c                   clear                   vidhhs
290800110530     C                   GOTO      ENDCT2
290900110530     c                   endif
291000110530     c
291100960126     C***** IMPORTO PATTUITO   *****
291200951025     C     VIDIPT        IFNE      SAVIPT
291300010920     C     VIDVIP        ORNE      SAVVIP
291400951025     C                   CLEAR                   WFORZ4
291500951025     C                   CLEAR                   WFORZ5
291600010920     C                   Z-ADD     VIDIPT        SAVIPT
291700010920     C                   MOVEL     VIDVIP        SAVVIP
291800951025     C                   ENDIF
291900060321     C* FILIALE CHE DEVE ESPORRE IL FV  se non è un bis
292000060321     c     vidbis        ifeq      ' '
292100060321    2C     TLTFES        andeq     VIDFGS
292200951123     C     WTFM          ANDEQ     WFMFES
292300951024    3C     VIDIPT        IFEQ      0
292400951018     C                   MOVEL     MSG(24)       VIDMSG
292500950901     C                   SETON                                        905828
292600951024     C                   GOTO      ENDCT2
292700951024    3C                   ENDIF
292800951025     C* SE VARIATO L'IMPORTO DA QUELLO MEMORIZZATO DAL LISTINO--> ERROR
292900951025     C     WFORZ5        IFEQ      ' '
293000951025     C     VIDIPT        ANDNE     TRNIPT
293100951025     C                   MOVEL     MSG(33)       VIDMSG
293200951025     C                   SETON                                        905828
293300951025     C                   MOVEL     'S'           WFORZ5            1
293400951025     C                   GOTO      ENDCT2
293500951025    3C                   ENDIF
293600010920     C* SE VARIATA LA DIVISA DA QUELLA MEMORIZZATA DAL LISTINO--> ERROR
293700010920     C     WFORZ5        IFEQ      ' '
293800010920     C     VIDVIP        ANDNE     TRNVIP
293900010920     C                   MOVEL     MSG(78)       VIDMSG
294000010920     C                   SETON                                        905828
294100010920     C                   MOVEL     'S'           WFORZ5            1
294200010920     C                   GOTO      ENDCT2
294300010920    3C                   ENDIF
294400010920     C*
294500951024   X2C                   ELSE
294600951024     C*
294700951024    3C     WFORZ4        IFEQ      ' '
294800951024    4C     VIDIPT        IFGT      0
294900061116     c     wforzd        andeq     'S'
295000951024     C                   MOVEL     MSG(12)       VIDMSG
295100951024     C                   SETON                                        905828
295200951024     C                   MOVEL     'S'           WFORZ4            1
295300951024     C                   GOTO      ENDCT2
295400951024    4C                   ENDIF
295500950901     C*
295600951024    3C                   ENDIF
295700951024    2C                   ENDIF
295800951024    1C                   ENDIF
295900110512     c
296000960129     C*
296100960129     C**** PIOMBI ****
296200960205     C* OBBLIGATORI IN CHIUSURA FV - NO SE E' LOCALE
296300130315     C   01ilst4ges      IFEQ      'C'
296400130315     C     ilst4ges      OREQ      'I'
296500960129     C     VIDPMB        IFEQ      *BLANKS
296600960205     C     VIDTTR        ANDNE     'L'
296700960129     C                   MOVEL     MSG(39)       VIDMSG
296800960129     C                   SETON                                        539028
296900960129     C                   GOTO      ENDCT2
297000960129     C                   ENDIF
297100960129     C                   ENDIF
297200950901     C*
297300951018     C****  DATA / ORA  PARTENZA EFFETTIVA  ****
297400951026     C** DATA ORA PARTENZA: OBBLIGATORI IN CHIUSURA FV
297500130315    1C   01ilst4ges      IFEQ      'I'
297600130315     C     ilst4ges      OREQ      'C'
297700951026    2C     VIDDTP        IFEQ      0
297800960214     C     VIDTTR        ANDNE     'L'
297900951026     C                   SETON                                        469028
298000951026     C                   MOVEL     MSG(38)       VIDMSG
298100951026     C                   GOTO      ENDCT2
298200951026    2C                   ENDIF
298300951026    1C                   ENDIF
298400951103     C*
298500951103     C* UNA VOLTA IMMESSA LA DATA/ORA PARTENZA NON SI PUO' PIU'
298600951106     C*   TOGLIERE MA SOLO MODIFICARE
298700951106    1C  N80FGVDTP        IFGT      0
298800951103     C     VIDDTP        ANDEQ     0
298900951103     C                   SETON                                        469028
299000960126     C                   MOVEL     MSG(64)       VIDMSG
299100951103     C                   GOTO      ENDCT2
299200951103    1C                   ENDIF
299300951026     C*
299400951025     C                   Z-ADD     *ZERO         AMGDTP
299500951018    1C     VIDDTP        IFNE      *ZERO
299600950324     C     VIDDTP        OREQ      *ZERO
299700950324     C     VIDHMP        ANDNE     *ZERO
299800900515     C                   MOVE      VIDDTP        G02DAT
299900900515     C                   MOVEL     *BLANK        G02ERR
300000940909     C                   CALL      'XSRDA8'
300100900515     C                   PARM                    WLBDAT
300200951018    2C     G02ERR        IFEQ      '1'
300300951026     C                   SETON                                        469028
300400951026     C                   MOVEL     MSG(38)       VIDMSG
300500951024     C                   GOTO      ENDCT2
300600951018    2C                   ENDIF
300700940909     C                   Z-ADD     G02DAT        VIDDTP
300800960122     C                   Z-ADD     G02INV        AMGDTP
300900930225     C*
301000930225     C* CONTROLLO DATA COMPRESA NEL RANGE
301100951027    2C     AMGDTP        IFLT      DTPMIN
301200951027     C     AMGDTP        ORGT      DTPMAX
301300951027     C                   MOVEL     MSG(45)       VIDMSG
301400951026     C                   SETON                                        469028
301500951024     C                   GOTO      ENDCT2
301600951018    2C                   ENDIF
301700951026     C*
301800951026     C* ORA OBBLIGATORIA SE IMMESSA DATA
301900960126    2C     VIDHMP        IFEQ      0
302000951026     C                   MOVEL     MSG(13)       VIDMSG
302100951026     C                   SETON                                        409028
302200951026     C                   GOTO      ENDCT2
302300960126    2C                   ENDIF
302400940913     C*
302500951018    1C                   ENDIF
302600951018     C*
302700951018     C* CONTROLLO ORA PARTENZA EFFETTIVA
302800910809     C                   MOVEL     VIDHMP        HM
302900951018    1C     ORA           IFGT      24
303000900515     C     MIN           ORGT      59
303100951018     C                   MOVEL     MSG(13)       VIDMSG
303200951026     C                   SETON                                        409028
303300951024     C                   GOTO      ENDCT2
303400951018    1C                   ENDIF
303500951027     C*
303600951103     C****  CAUSALE RITARDO/ANTICIPO  ****
303700960126     C* SE INSERITA DATA/ORA PARTENZA EFFETTIVA
303800951120    1C     VIDDTP        IFGT      0
303900960126     C                   MOVEL     '1'           WFORZC
304000951106     C*
304100951106     C* RICERCA CAUSALE RITARDO/ANTICIPO
304200951103    2C     VIDRTC        IFEQ      '?'
304300951103     C                   MOVEL     'TC'          COD
304400951103     C                   MOVEL     *BLANKS       VIDRTC
304500951103     C                   MOVEL     *BLANKS       DES
304600951103     C                   CALL      'X§TABER'
304700951103     C                   PARM                    CODUT
304800951103     C                   PARM                    COD
304900951103     C                   PARM                    KEY
305000951103     C                   PARM                    DES
305100951103     C                   MOVEL     KEY           VIDRTC
305200960208     C                   MOVEL     DES           DESRT1
305300960208     C                   MOVE      DES           W008A             8
305400960208     C                   CLEAR                   DESRT2
305500960208     C                   MOVEL     W008A         DESRT2
305600951103     C                   SETON                                        9059
305700951103     C                   GOTO      ENDCT2
305800951103    2C                   ENDIF
305900951103     C*
306000951103     C* CONTROLLO VALIDITA' CAUSALE
306100951103    2C     VIDRTC        IFNE      ' '
306200951103     C                   MOVEL     'TC'          COD
306300951103     C                   MOVEL(P)  VIDRTC        KEY
306400951103     C     KTAB          CHAIN     TABEL                              83
306500951103    3C     *IN83         IFEQ      *OFF
306600951103     C     TBLFLG        ANDNE     ' '
306700951103     C                   SETON                                        83
306800951103    3C                   ENDIF
306900951103     C*
307000951103    3C     *IN83         IFEQ      *ON
307100951103     C                   MOVEL     MSG(53)       VIDMSG
307200951103     C                   SETON                                        599028
307300951103     C                   GOTO      ENDCT2
307400951103   X3C                   ELSE
307500960208     C                   MOVEL     TBLUNI        DSTC
307600960208     C                   MOVEL     §TCDES        DESRT1
307700960208     C                   MOVE      §TCDES        DESRT2
307800951103    3C                   ENDIF
307900951103     C*
308000951103   X2C                   ELSE
308100960208     C                   CLEAR                   DESRT1
308200960208     C                   CLEAR                   DESRT2
308300951103    2C                   ENDIF
308400951103     C*
308500960122     C* SE DATA/ORA PARTENZA REALE <> DALLA TEORICA, VERIFICO SE DEVO
308600960122     C*  CHIEDERE IL RITARDO
308700960126    2C     VIDTRN        IFGT      0
308800960126     C* SE MANCA LA DATA TEORICA LA CALCOLO
308900960126     C* RICALCOLO SOLO SE HO MODIFICATO LA DATA PARTENZA EFFETTIVA
309000960126    3C     SAVDTP        IFNE      AMGDTP
309100960126     C     SAVHMP        ORNE      VIDHMP
309200960126     C                   CLEAR                   WFORZC
309300960126     C*
309400960126    4C     AMGDTP        IFNE      AMGDTT
309500960126     C     VIDHMP        ORNE      VIDHMT
309600960122     C                   EXSR      CALDIF
309700960126     C                   ELSE
309800960126     C* SONO UGUALI: NON C'E' RITARDO
309900960126     C                   SETOFF                                       18
310000960126    4C                   ENDIF
310100960126     C                   MOVEL     AMGDTP        SAVDTP
310200960126     C                   MOVEL     VIDHMP        SAVHMP
310300960126    3C                   ENDIF
310400960126    2C                   ENDIF
310500951106     C* SE PARTENZA IN RITARDO/ANTICIPO --> CAUSALE OBBLIGATORIA
310600960126    2C   18VIDRTC        IFEQ      ' '
310700951107     C                   MOVEL     MSG(56)       VIDMSG
310800951106     C                   SETON                                        599028
310900951106     C                   GOTO      ENDCT2
311000951106    2C                   ENDIF
311100951106     C*
311200951107     C* SE MODIFICANDO LA DATA/ORA PARTENZA EFFETTIVA IL RITARDO/ANTIC.
311300951107     C*   SCOMPARE, CHIEDERE FORZABILE DI ELIMINARE LA CAUSALE
311400960126    2C  N18WFORZC        IFEQ      ' '
311500951106     C     VIDRTC        ANDNE     ' '
311600951106     C                   MOVEL     MSG(52)       VIDMSG
311700951106     C                   SETON                                        905928
311800960126     C                   MOVEL     '1'           WFORZC            1
311900951106     C                   GOTO      ENDCT2
312000951106    2C                   ENDIF
312100951106     C*
312200951107     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
312300960123     C                   MOVEL     'P'           KEPA
312400960123     C                   MOVEL     'R'           KTRC
312500960123     C                   CLEAR                   KTFA
312600951107     C                   EXSR      CTRDRA
312700951103     C*
312800951103     C* SE IMMESSA LA "G" DI CAUSALE GENERICA, RICHIEDERE OBBLIGATORIA
312900951107     C*   LA DESCRIZIONE RITARDO/ANTICIPO
313000951103    2C     VIDRTC        IFEQ      'G'
313100960126     C* 16 - OFF --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
313200960126     C     *IN16         ANDEQ     *OFF
313300951108     C     *INKV         ANDEQ     *OFF
313400951103     C                   MOVEL     MSG(54)       VIDMSG
313500951103     C                   SETON                                        599028
313600951103     C                   GOTO      ENDCT2
313700951103    2C                   ENDIF
313800951103     C*
313900951120   X1C                   ELSE
314000951120     C* DATA/ORA REALE = 0
314100951120    2C     VIDRTC        IFNE      ' '
314200951120     C                   MOVEL     MSG(62)       VIDMSG
314300951120     C                   SETON                                        469028
314400951120     C                   GOTO      ENDCT2
314500951120    2C                   ENDIF
314600951103    1C                   ENDIF
314700960126     C*
314800960126     C* SE INSERITA LA DESCRIZIONE, INSERIRE ANCHE LA CAUSALE RITARDO
314900960126     C     *IN16         IFEQ      *ON
315000960126     C     VIDRTC        ANDEQ     *BLANKS
315100960126     C                   MOVEL     MSG(57)       VIDMSG
315200960126     C                   SETON                                        599028
315300960126     C                   GOTO      ENDCT2
315400960126     C                   ENDIF
315500940913     C*
315600951024     C     ENDCT2        ENDSR
315700110530     C*-----------------------------------------------------***********
315800130729     C* Gestione videata punti di scambio
315900110530     C*-----------------------------------------------------***********
316000110530     C     GesPsc        BEGSR
316100110530     c
316200110530     c     forPsc        TAG
316300110530     c                   if        Emisd06='1'
316400130315     C                   WRITE     lst4T01
316500130315     C                   WRITE     lst4d06
316600110530     c                   clear                   emisD06
316700110530     c                   endif
316800110530
316900130315     C                   EXFMT     lst4d07
317000110530     c                   setoff                                       747576
317100110530     c                   setoff                                       772890
317200110530     c*
317300130315    1C     ilst4ges      IFNE      'N'
317400110530     c                   exsr      ctrPsc
317500110531     c                   endif
317600110530     c
317700110530     C* F12 - RITORNO ma senza errori
317800110530     c                   if        *inkl and not *in90
317900110530     c                   leavesr
318000110530     c                   endif
318100110530
318200130315    1C     ilst4ges      IFNE      'N'
318300110530     c                   goto      forPSc
318400110531     c                   endif
318500110530     c
318600110530     c                   ENDSR
318700110530     C*-----------------------------------------------------***********
318800110530     C* CONTROLLi per punti di scambio
318900110530     C*-----------------------------------------------------***********
319000110530     C     CTRPsc        BEGSR
319100110530     c*
319200110530     c* controllo codice
319300110530     c                   clear                   despsc
319400110530     c                   clear                   despind
319500110530     c                   clear                   desploc
319600110530     c
319700110530     c                   if        vidtrn>0
319800110530     c                   exsr      LetTLS
319900110530     c                   else
320000110531     c                   clear                   dstls
320100110530     c                   endif
320200110530     c
320300110530    1c                   if        vidpsc<>*blanks
320400110530     c* col '?'  ricerca
320500110530     C                   EVAL      INDX=%scan('?':vidpsc)
320600110530     c
320700110530    2c                   if        indx>0
320800110530     c* imposta la ds di procedura
320900110530     c                   CLEAR                   tntlp0ds
321000110530     c                   MOVE      '01'          dp0op0
321100110530     c                   MOVEL     *blanks       dp0op1
321200110530     c                   MOVEL     '0'           dp0f03
321300110530     c                   MOVEL     '0'           dp0f12
321400110530     c                   MOVEL     '0'           dp0err
321500110530     c                   call      'TNTLP0R'
321600110530     c                   parm                    kpjba
321700110530     c                   parm                    TNTLp0ds
321800110530     c
321900110530     c                   eval      vidpsc=dp0psc
322000110530     c                   eval      despsc=dp0loc
322100110530     c                   eval      despind=dp0ind
322200110530     c                   eval      desploc=%trim(dp0cap) + ' ' +
322300110531     c                             %trim(dp0loc)+'  ' +dp0prv+dp0nar
322400130412     c                   eval      Dpscflo=dp0flo
322500130412     c                   if        §pscflr>*zeros   and vidlnar<=*zeros
322600130412     c                   movel     §pscflr       vidlnar
322700130412     c                   movel     §pscflr       codfil
322800130412     c                   exsr      ctrfil
322900130412     c                   if        not *in34
323000130412     c                   movel     orgdes        deslnar
323100130412     c                   movel     orgdes        deslnar2
323200130412     c                   endif
323300130412     c                   endif
323400110530     c
323500110530     C                   SETON                                        7490
323600110530     c                   eval      emisD06='1'
323700110530     C                   leavesr
323800110530    2c                   endif
323900110530     c
324000110530     c     vidpsc        chain     tnpsc01l
324100110530    2c                   if        not %found(tnpsc01l)
324200110530     C                   MOVEL     MSG(92)       vidmsg
324300110530     C                   SETON                                        749028
324400110530     C                   leavesr
324500110530E   2C                   ENDIF
324600110530     c
324700110530     c                   eval      despsc=pscloc
324800110530     c                   eval      despind=pscind
324900110530     c                   eval      desploc=%trim(psccap) + ' ' +
325000110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
325100130412     c                   eval      Dpscflo=pscflo
325200130412     c                   if        §pscflr>*zeros   and vidlnar<=*zeros
325300130412     c                   movel     §pscflr       vidlnar
325400130412     c                   movel     §pscflr       codfil
325500130412     c                   exsr      ctrfil
325600130412     c                   if        not *in34
325700130412     c                   movel     orgdes        deslnar
325800130412     c                   movel     orgdes        deslnar2
325900130412     c                   endif
326000130412     c                   endif
326100110530     c
326200110530     c
326300110530     c* controllo se scaduto
326400110530    2c                   if        pscdsc>0
326500110530     c                             and amgdfv>pscdsc
326600110530     C                   MOVEL     MSG(92)       vidmsg
326700110530     C                   SETON                                        749028
326800110530     C                   leavesr
326900110530    2c                   endif
327000110530    1c                   endif
327100110530     c
327200110530     c* Se presente data/ora o lna finale --> presente anche punti scam
327300110530     c                   if        vidpsc=*blanks  and
327400110530     c                             (viddgs>0 or vidhhs>0 or (vidlnar<>*blanks
327500110530     c                             and  vidlnar<>*zeros))
327600110530     C                   MOVEL     MSG(93)       vidmsg
327700110530     C                   SETON                                        749028
327800110530     C                   leavesr
327900110530     c                   endif
328000110530     c
328100110530     c                   clear                   amgdGS
328200110607     c* data e ora obbligatori se presente traino di scambio
328300110610     c                   if        vidtrns>0 and viddgs=0  and vidpsc<>*blanks
328400110610     c                             and vidlnar<=*zeros
328500110607     C                   SETON                                        759028
328600110607     C                   MOVEL     MSG(99)       VIDMSG
328700110607     C                   leavesr
328800110607    2C                   ENDIF
328900110607     c
329000110530    1c                   if        viddgs>0   or vidhhs>0
329100110530     C                   MOVE      VIDdgs        G02DAT
329200110530     C                   MOVEL     *BLANK        G02ERR
329300110530     C                   CALL      'XSRDA8'
329400110530     C                   PARM                    WLBDAT
329500110530    2C     G02ERR        IFEQ      '1'
329600110530     C                   SETON                                        759028
329700110530     C                   MOVEL     MSG(94)       VIDMSG
329800110530     C                   leavesr
329900110530    2C                   ENDIF
330000110530    c
330100110530     C                   Z-ADD     G02DAT        VIDdgs
330200110530     C                   Z-ADD     G02INV        AMGDgs            8 0
330300110530     c
330400110530     c* Deve essere >= data foglio viaggio
330500110530    2c                   if        amgdgs<amgdfv
330600110530     C                   SETON                                        759028
330700110530     C                   MOVEL     MSG(95)       VIDMSG
330800110530     C                   leavesr
330900110530    2C                   ENDIF
331000110530     C* CONTROLLO DATA COMPRESA NEL RANGE
331100110530    2C     AMGdgs        IFLT      DTPMIN
331200130729     C     AMGDgs        ORGT      DTPMAX
331300110530     C                   MOVEL     MSG(45)       VIDMSG
331400110530     C                   SETON                                        759028
331500110530     C                   leavesr
331600110530    2C                   ENDIF
331700110530    c
331800110530     C* ORA OBBLIGATORIA SE IMMESSA DATA
331900110530    2C     VIDHhs        IFEQ      0
332000110530     C                   MOVEL     MSG(13)       VIDMSG
332100110530     C                   SETON                                        769028
332200110530     C                   leavesr
332300110530    2C                   ENDIF
332400110530    1C                   ENDIF
332500110530     c
332600110530     c* Ora scambio
332700110530    1c                   if        vidhhs>0
332800110530     C                   MOVEL     VIDhhs        HM
332900110530    2C     ORA           IFGT      24
333000110530     C     MIN           ORGT      59
333100110530     C                   MOVEL     MSG(13)       VIDMSG
333200110530     C                   SETON                                        769028
333300110530     C                   leavesr
333400110530    2C                   ENDIF
333500110530    1C                   ENDIF
333600110530     c*
333700110530     C* Filiale finale arrivo autista
333800110530     C     '?'           SCAN      VIDLNAr                                77
333900110530    1C     *IN77         IFEQ      *ON
334000110530     C                   EXSR      CASD19
334100110530     C                   MOVEL     §COD          VIDLNAr
334200110530     C                   MOVEL     §§DES         DESLNAr
334300110601     C                   MOVEL     §§DES         DESLNAr2
334400110530     C                   SETON                                        90
334500110530     c                   eval      emisD06='1'
334600110530     C                   leavesr
334700110530    1C                   ENDIF
334800110530     C*
334900110530     C* CONTROLLO CHE SIA UNA FILIALE DELL'ORGANIGRAMMA
335000110530     C                   MOVEL     *BLANKS       DESLNAR
335100110601     C                   MOVEL     *BLANKS       DESLNAR2
335200110531    1c                   if        vidlnar<>*blanks and vidlnar<>*zeros
335300110530     c
335400110530    2c                   if        vidlnar>*zeros  and
335500110530     c                             %subst(vidlnar:3:1)>='0'
335600110530     C                   MOVE      VIDLNAr       CODFIL
335700110530     C                   EXSR      CTRFIL
335800110530     C*
335900110530    3C     *IN34         IFEQ      *ON
336000110530     C                   MOVEL     *ALL'?'       DESLNAr
336100110530     C                   MOVEL     MSG(8)        VIDMSG
336200110530     C                   SETON                                        287790
336300110530     C                   leavesr
336400110530     c
336500110530   X3C                   ELSE
336600110530     c
336700110530     C                   MOVEL     ORGDES        DESLNAr
336800110601     C                   MOVEL     ORGDES        DESLNAr2
336900110530     c* non deve essere fil estera
337000110530    4c                   if        esteroedpd='S'
337100110530     C                   MOVEL     MSG(8)        VIDMSG
337200110530     C                   SETON                                        287790
337300110530     c                   leavesr
337400110530    4c                   endif
337500110530    3c                   endif
337600110530     c
337700110530   x2c                   else
337800110530     C                   MOVEL     *ALL'?'       DESLNAr
337900110530     C                   MOVEL     MSG(8)        VIDMSG
338000110530     C                   SETON                                        287790
338100110530     C                   leavesr
338200110530    2c                   endif
338300110530    1c                   endif
338400110530     c
338500110531     c* se c'e' traino di scambio lo decodifico
338600110601     c                   clear                   destrns
338700110531     c                   if        vidtrns>0
338800110531     c                   exsr      CtrTRNS
338900110531     c                   endif
339000110530
339100110530     c* Se cambiato punto di scambio --> sbino
339200110530    1C     VIDTRN        IFGT      0
339300110530     c
339400110530     c                   if        vidpsc<>savpsc
339500110530     C                   CLEAR                   WFORZsc
339600110530     c                   eval      savpsc=vidpsc
339700110530    2c                   endif
339800110530     c
339900110530     c                   if        (vidlnar>*zeros and savlnar<=*zeros) or
340000110607     c                             (vidlnar<=*zeros and savlnar>*zeros)
340100110530     C                   CLEAR                   WFORZlnar
340200110530     c                   eval      savlnar=vidlnar
340300110530    2c                   endif
340400110530     c
340500110530    2c                   if        wforzsc=' ' and vidpsc<>tlspsc
340600110530     C                   MOVEL     MSG(96)       VIDMSG
340700110530     C                   SETON                                        749028
340800110531     c                   clear                   vidtrns
340900110531     c                   clear                   destrns
341000110530     C                   MOVEL     'S'           WFORZsc           1
341100110530     C                   leavesr
341200110530    2c                   endif
341300110531     c
341400110531     c* se rimessi uguali, reimposto il traino di scambio
341500110610     c                   if        vidpsc=tlspsc and vidlnar<=*zeros
341600110531     c                   eval      vidtrns=tlstrns
341700110531     c                   endif
341800110531     c
341900110531     c* Obbligatoria la linea di arrivo finale del mezzo
342000110531    2c                   if        wForzsc='S' and vidlnar<=*zeros
342100110601     c                             and vidpsc<>*blanks
342200110531     C                   MOVEL     MSG(104)      VIDMSG
342300110531     C                   SETON                                        779028
342400110531     C                   leavesr
342500110531   x2c                   else
342600110530     c* Se aggiunta linea arrivo finale --> sbino
342700130412    3c                   if        wforzsc=' '   and vidtrns>0 and
342800110531    3c                             wforzlnar=' ' and vidlnar>*zeros
342900110530     C                   MOVEL     MSG(101)      VIDMSG
343000110531     C                   SETON                                        779028
343100110610     c                   clear                   vidtrns
343200110610     c                   clear                   destrns
343300110530     C                   MOVEL     'S'           WFORZlnar         1
343400110530     C                   leavesr
343500110531    3c                   endif
343600110531    2c                   endif
343700110531     c
343800110531   x1c                   else
343900110531     c* In manutenzione
344000110531     c* se traino già sbinato, cambio  punto di scambio  e c'e'
344100110531     c*  num traino di scambio--> lo tolgo
344200110531    2c  n80              if        oldpsc<>*blanks and oldpsc<>vidpsc and
344300110531     c                             vidtrns>0
344400110531     c                   clear                   vidtrns
344500110610     c                   clear                   destrns
344600110531     c                   seton                                        90
344700110531    2c                   endif
344800110531    c
344900110531    2c  n80              if        oldpsc>*blanks and oldpsc= vidpsc and
345000110531     c                             vidtrns=0 and vidlnar<=*zeros
345100110531     c                             and fgstrns>0
345200110531     c                   eval      vidtrns=fgstrns
345300110531     c                   seton                                        90
345400110531    2c                   endif
345500110531     c
345600110531     c* Se aggiunta linea finale di arrivo --> tolgo il traino di scambio
345700110531    2c                   if        vidtrns>0 and vidlnar>*zeros
345800110531     C                   MOVEL     MSG(98)       VIDMSG
345900110531     C                   SETON                                        779028
346000110610     c                   clear                   destrns
346100110531     c                   clear                   vidtrns
346200110531     C                   leavesr
346300110531    2c                   endif
346400110531    1c                   endif
346500110531     c
346600110531     c* se manca sia traino di scambio che linea di arrivo finale del mezzo -->
346700110531     c*  Errore
346800110531     c                   if        vidtrns=0 and vidlnar<=*zeros
346900110531     c                             and vidpsc<>*blanks
347000110531     C                   MOVEL     MSG(97)       VIDMSG
347100110531     C                   SETON                                        779028
347200110531     C                   leavesr
347300110531     c                   endif
347400110530     c
347500110530     C                   ENDSR
347600110513     C*-----------------------------------------------------***********
347700110513     C* CONTROLLO Traino da cui copiare-uso sql per non sporcarmi i campi
347800110513     C*-----------------------------------------------------***********
347900110513     C     CTRtrns       BEGSR
348000110513     c                   clear                   woktrns           1
348100110513     c                   eval      wrkgetlista =
348200110513     c                             'select tltdde, tltdsc, tlttfp, tlttfa +
348300110513     c                               from tntlt00f where tltatb= '' '' +
348400110513     c                               and tltdde<=' + %editc(amgdfv:'X') +
348500110530     c                               ' and tlttrn= ' + %editc(vidtrns:'X')
348600110513
348700110513     C/EXEC SQL
348800110513     C+ PREPARE s1 FROM :wrkgetlista
348900110513     C/END-EXEC
349000110513     C
349100110513     C/EXEC SQL
349200110513     C+ DECLARE A1 CURSOR FOR S1
349300110513     C/END-EXEC
349400110513     C
349500110513     C/EXEC SQL
349600110513     C+ OPEN a1
349700110513     C/END-EXEC
349800110513
349900110513      *?Leggo il file
350000110513     c                   do        *hival
350100110513     C/EXEC SQL
350200110513     C+ FETCH NEXT FROM a1 INTO : Cdde, :Cdsc, :Ctfp, :Ctfa
350300110513     C/END-EXEC
350400110513
350500110513     c                   select
350600110513
350700110513     c                   when      sqlcod = 100
350800110513     c                   leave
350900110513     c                   when      sqlcod < 0
351000110513     c                   seton                                        H1
351100110513     c                   leave
351200110513     c                   other
351300110513     c
351400110513     c                   if        Cdsc>=amgdfv  and Cdde<=amgdfv
351500110513     c                   eval      woktrns='S'
351600110513     c* Decodico  partenza e arrivo
351700110513     c                   eval      destrns='Da '+%editc(Ctfp:'X') + ' a '+
351800110513     c                             %editc(Ctfa:'X')
351900110513     c                   endif
352000110513     c
352100110513     c                   endsl
352200110513
352300110513     c                   enddo
352400110513     c* errore
352500110513     c                   if        woktrns=' '
352600110513     c                   seton                                        772890
352700110513     c                   eval      vidmsg=msg(100)
352800110513     c                   endif
352900110513
353000110513     C/EXEC SQL
353100110513     C+ CLOSE a1
353200110513     C/END-EXEC
353300110513
353400110513     c                   ENDSR
353500951218     C*-----------------------------------------------------***********
353600951218     C* CONTROLLO DEL TIPO AUTOMEZZO E DEL TRAZIONISTA
353700951218     C*-----------------------------------------------------***********
353800951218     C     CTRPAR        BEGSR
353900951218     C****  TIPO AUTOMEZZO  ****
354000951218     C* OBBLIGATORIO IN COPIA DA TRAINO/BORDERO'/CHIUSURA FV
354100951218     C* RICERCA TIPO AUTOMEZZO
354200951218    1C     VIDTMZ        IFEQ      '?'
354300951218     C                   MOVEL     'TU'          COD
354400951218     C                   MOVEL     *BLANKS       VIDTMZ
354500951218     C                   MOVEL     *BLANKS       DES              25
354600951218     C                   CALL      'X§TABER'
354700951218     C                   PARM                    CODUT
354800951218     C                   PARM                    COD
354900951218     C                   PARM                    KEY
355000951218     C                   PARM                    DES
355100951218     C                   MOVEL     KEY           VIDTMZ
355200951218     C                   MOVEL     DES           DESTMZ
355300130502     C                   MOVEL     DES           DESTMZ2
355400951218     C                   SETON                                        9057
355500951218     C                   GOTO      ENDPAR
355600951218    1C                   ENDIF
355700951218     C*
355800951218     C* SE IMMESSO CONTROLLO VALIDITA' TIPO AUTOMEZZO
355900960126     C* IN 2 VIDEATA SE CHIUSURA FV
356000951218    1C     VIDTMZ        IFNE      ' '
356100951218     C     *IN01         OREQ      *ON
356200960126     C     WRECOR        ANDNE     '1'
356300951218     C                   EXSR      CTRTMZ
356400951218    2C     *IN83         IFEQ      *ON
356500951218     C                   MOVEL     MSG(26)       VIDMSG
356600951218     C                   SETON                                        905728
356700951218     C                   GOTO      ENDPAR
356800951218    2C                   ENDIF
356900951218    1C                   ENDIF
357000951218     C*
357100951218     C****  CODICE TRAZIONISTA  ****
357200951218     C* OBBLIGATORIO IN COPIA DA TRAINO
357300951218     C* IN BORDERO'/CHIUSURA FV SI PUO' ANCHE SOLO INSERIRE LA DESCRIZ
357400951218     C* RICERCA CODICE TRAZIONISTA
357500951218     C     '?'           SCAN      VIDPDR                                 83
357600951218    1C     *IN83         IFEQ      *ON
357700000807     C                   MOVEL     'T '          PA3TIP
357800000807     C                   MOVEL(P)  VIDDPD        PA3RSC
357900000807     C                   CLEAR                   PA3CSF
358000000807     C                   CLEAR                   PA3PDR
358100000807     C                   CLEAR                   PA3FLG
358200951218     C                   MOVEL     PARAM3        KPJBU
358300951218     C                   CALL      'TRUL16R'
358400951218     C                   PARM                    KPJBA
358500951218     C                   MOVEL     KPJBU         PARAM3
358600000807     C                   MOVEL     PA3PDR        VIDPDR
358700000807     C                   MOVEL     PA3RSC        VIDDPD
358800110609     C                   SETON                                        9047
358900110609     C                   GOTO      ENDPAR
359000110609     c
359100000807     C* SE PA3FLG = "3" --> NON ESISTONO RECORDS PER CHIAVE ALFABETICA
359200951218     C*                     RICHIESTA
359300951218     C     PA3FLG        IFEQ      '3'
359400951218     C                   MOVEL     MSG(51)       VIDMSG
359500951219     C                   SETON                                        289047
359600951219     C                   GOTO      ENDPAR
359700951218     C                   ENDIF
359800951218     C*
359900951218    1C                   ENDIF
360000951218     C*
360100951218     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
360200951218    1C     VIDPDR        IFNE      *BLANKS
360300951218     C     VIDPDR        ANDNE     *ZEROS
360400951218     C*
360500951218     C                   TESTN                   VIDPDR               31
360600951218     C  N31              MOVEL     MSG(9)        VIDMSG
360700951218     C  N31              SETON                                        902847
360800951218     C  N31              GOTO      ENDPAR
360900951218     C*
361000951218     C                   MOVE      VIDPDR        KPDR
361100000807     C                   Z-ADD     KPDR          KTLPDR
361200000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
361300951218     C*
361400951218    2C     *IN31         IFEQ      *ON
361500951218     C     TLZATB        ORNE      ' '
361600951218     C                   MOVEL     MSG(9)        VIDMSG
361700951218     C                   SETON                                        904728
361800951218     C                   GOTO      ENDPAR
361900951218   X2C                   ELSE
362000960302     C*
362100960302     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
362200960302    3C     VIDPDR        IFNE      '0465000'
362300960302     C     VIDDPD        OREQ      *BLANKS
362400951218     C                   MOVEL     TLZRSC        VIDDPD
362500960302    3C                   ENDIF
362600960302    2C                   ENDIF
362700951218    1C                   ENDIF
362800951218     C* CODICE O DESCRIZIONE OBBLIGATORIO
362900960126     C   01WRECOR        IFNE      '1'
363000960126     C     VIDPDR        ANDLE     *ZEROS
363100951218     C     VIDDPD        ANDEQ     *BLANKS
363200951218     C                   MOVEL     MSG(10)       VIDMSG
363300951218     C                   SETON                                        904728
363400951218     C                   GOTO      ENDPAR
363500951218    2C                   ENDIF
363600951218     C     ENDPAR        ENDSR
363700951102     C*-----------------------------------------------------***********
363800960122     C* CONTROLLO DIFFERENZA DATA TEORICA E REALE
363900951102     C*-----------------------------------------------------***********
364000960122     C     CALDIF        BEGSR
364100951106     C                   SETOFF                                       18
364200960122     C* CALL PGM PER CALCOLO DIFFERENZA DATA ORA TEORICA CON REALE
364300020805     C                   CLEAR                   TNTL35DS
364400960122     C                   MOVEL     'C'           D35TUP
364500960122     C                   MOVEL     'P'           D35EPA
364600960122     C                   MOVEL     VIDTRN        D35TRN
364700960122     C                   MOVEL     WDDE          D35DDE
364800960122     C                   MOVEL     WDTN          D35DTN
364900960122     C                   MOVEL     AMGDTT        D35DTE
365000960126     C                   MOVEL     VIDHMT        D35HTE
365100960122     C                   MOVEL     AMGDTP        D35DRE
365200960122     C                   MOVEL     VIDHMP        D35HRE
365300020805     C                   MOVEL     TNTL35DS      KPJBU
365400960122     C                   CALL      'TNTL35R'
365500960122     C                   PARM                    KPJBA
365600020805     C                   MOVEL     KPJBU         TNTL35DS
365700960122     C     D35FLG        IFNE      ' '
365800960122     C                   SETON                                        18
365900960122     C                   ENDIF
366000951103     C*
366100951106     C                   ENDSR
366200951103     C*
366300951107     C*-----------------------------------------------------***********
366400951107     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO    * CTRDRA  *
366500951107     C*-----------------------------------------------------***********
366600951107     C     CTRDRA        BEGSR
366700951107     C                   CLEAR                   W001A
366800951107     C*
366900951107     C     KFV4          SETLL     FNFV401L                               83
367000951107    1C     *IN83         IFEQ      *ON
367100951107     C     KFV4          READE(N)  FNFV401L                               83
367200951107    2C     *IN83         DOWEQ     *OFF
367300951107    3C     FV4ATB        IFEQ      ' '
367400951107     C                   MOVEL     '1'           W001A
367500951107     C                   SETON                                        83
367600951107   X3C                   ELSE
367700951107     C     KFV4          READE(N)  FNFV401L                               83
367800951107    3C                   ENDIF
367900951107    2C                   ENDDO
368000951107    1C                   ENDIF
368100951107     C*
368200951107     C                   ENDSR
368300951107     C*
368400900515      *-----------------------------------------------------***********
368500951025      * CONTROLLI SFL                                       *  CTRD03 *
368600900515      *-----------------------------------------------------***********
368700951025     C     CTRD03        BEGSR
368800900515      *
368900951026     C                   SETOFF                                       9096
369000951026     C                   SETOFF                                       5051
369100951025     C                   CLEAR                   CTL
369200970103     C                   CLEAR                   CFS
369300970103     C                   CLEAR                   FE0
369400900529     C                   MOVEL     VIDLNA        CTL(1)
369500970102     C                   MOVEL     VIDLNA        CFS(1)
369600970102     C                   MOVEL     LNAFE0        FE0(1)
369700900515      *
369800080910      * CONTROLLO ALTRE FILIALI DEL FOGLIO
369900900515      *
370000951026     C                   Z-ADD     1             NRR3              3 0
370100000420    1C                   DO        299
370200130315     C     NRR3          CHAIN     lst4S03                            81
370300951025      *
370400951025    2C     VIDFFV        IFEQ      *BLANKS
370500900515     C     VIDFFV        OREQ      *ZEROS
370600900522     C                   MOVEL     *BLANKS       VIDFFV
370700900522     C                   MOVEL     *BLANKS       DESFFV
370800900515     C                   MOVEL     *BLANKS       TIPFFV
370900900515      *
371000951025   X2C                   ELSE
371100900522      *
371200900522      * RICERCA FILIALE
371300900522      *
371400951027     C     '?'           SCAN      VIDFFV                                 31
371500951027    3C   31              DO
371600940915     C                   SETON                                        90
371700951024     C                   EXSR      CASD19
371800900522     C                   MOVEL     §COD          VIDFFV
371900900522     C                   MOVEL     §TIP          TIPFFV
372000900530     C                   MOVEL     *BLANKS       DESFFV
372100900530     C                   MOVEL     §§DES         DESFFV
372200951025    3C                   END
372300900522      *
372400951025     C                   TESTN                   VIDFFV               83
372500951025     C                   MOVE      VIDFFV        W001A             1
372600951025    3C     *IN83         IFEQ      *ON
372700951026     C     W001A         ANDGE     '0'
372800951025     C                   MOVE      VIDFFV        CODFIL
372900951025     C                   EXSR      CTRFIL
373000951025     C                   ELSE
373100951026     C                   SETON                                        34
373200951025    3C                   ENDIF
373300951025     C*
373400900530     C                   MOVEL     *BLANKS       DESFFV
373500951026    3C     *IN34         IFEQ      *OFF
373600971203     C* DETERMINO TER-PART E DI ARRIVO DELLA FILIALE.
373700971203     C* COME TER-ARR, SE ESISTE, PRENDO QUELLO DA ECCEZIONI ALTRIMENTI
373800971203     C* QUELLO STANDARD
373900000602     C*
374000020805     C                   CLEAR                   FNLV55DS
374100971203     C                   MOVE      CODFIL        D55LIN
374200000612     C                   MOVE      VIDFGS        D55LNP
374300971203     C                   MOVE      AMGDFV        D55DRF
374400971203     C                   CALL      'FNLV55R'
374500020805     C                   PARM                    FNLV55DS
374600971203     C*
374700971203     C                   Z-ADD     D55TFA        LINFE0
374800971203     C*
374900951025     C                   MOVEL     ORGDES        DESFFV
375000951025     C                   MOVEL     ORGFAG        TIPFFV
375100971203     C                   MOVEL     D55TFP        SAVTFP
375200960226     C*
375300960226     C                   MOVEL     CODFIL        KTFA
375400960226     C                   MOVEL     'A'           KEPA
375500970102     C                   EXSR      RICEC2
375600971203     C                   MOVEA     TFE           TFE2
375700970102     C*
375800960226     C* RICERCO SE C'E' ECCEZIONE
375900951027     C* SE TRAINO DI DEFLUENZA O LOCALE, VERIFICO SE LE LINEE SONO
376000951027     C*  CORRETTE
376100951027    4C     VIDTTR        IFEQ      'L'
376200951027     C     §TVDEF        OREQ      'S'
376300100616     c                   setoff                                       31
376400100616     c                   if        §tvdef='S'
376500100616     C     VIDFGS        LOOKUP    TFEdef                                 31
376600100616     c                   endif
376700100616     c
376800100616     c                   if        not *in31
376900000612     C     VIDFGS        LOOKUP    TFE2                                   31
377000970102    5C     *IN31         IFEQ      *OFF
377100951027     C                   MOVEL     MSG(47)       VIDMSG
377200951027     C                   SETON                                        905028
377300951027    5C                   ENDIF
377400951027    4C                   ENDIF
377500100616     c                   endif
377600100616     c
377700960302     C* E' ANCHE VERO IL CONTRARIO
377800000612    4C  N90VIDFGS        IFEQ      LINFE0
377900960302     C     VIDTTR        ANDNE     'L'
378000960302     C     VIDTTR        ANDNE     'I'
378100960302     C     §TVDEF        ANDNE     'S'
378200960302     C                   MOVEL     MSG(66)       VIDMSG
378300960302     C                   SETON                                        905028
378400960302    4C                   ENDIF
378500951031     C* SE DEFLUENZA LA LNA <> DA LINEA PARTENZA FV
378600000612     C  N90              MOVE      VIDFGS        W003A
378700960302    4C  N90§TVDEF        IFEQ      'S'
378800000612     C     W003A         ANDEQ     VIDFFV
378900951027     C                   MOVEL     MSG(50)       VIDMSG
379000951027     C                   SETON                                        905028
379100960302    4C                   ENDIF
379200951027     C*
379300951025   X3C                   ELSE
379400951025     C* ERRORE
379500951025     C                   MOVEL     *ALL'?'       DESFFV
379600951025     C                   MOVEL     MSG(8)        VIDMSG
379700951025     C                   SETON                                        905028
379800971203     C                   Z-ADD     0             SAVTFP            3 0
379900951025    3C                   ENDIF
380000951025    2C                   ENDIF
380100900522      *--------------------
380200951025    2C     VIDFLP        IFEQ      *BLANKS
380300900522     C     VIDFLP        OREQ      *ZEROS
380400900522     C                   MOVEL     *BLANKS       VIDFLP
380500900522     C                   MOVEL     *BLANKS       DESFLP
380600900522     C                   MOVEL     *BLANKS       TIPFLP
380700900522      *
380800951025   X2C                   ELSE
380900900522      *
381000910125      * RICERCA FILIALE DI SCARICO
381100900522      *
381200951027     C     '?'           SCAN      VIDFLP                                 31
381300951027    3C   31              DO
381400940915     C                   SETON                                        90
381500951024     C                   EXSR      CASD19
381600900522     C                   MOVEL     §COD          VIDFLP
381700900522     C                   MOVEL     §TIP          TIPFLP
381800900530     C                   MOVEL     *BLANKS       DESFLP
381900900530     C                   MOVEL     §§DES         DESFLP
382000951025    3C                   END
382100900522      *
382200951025     C                   MOVE      VIDFLP        CODFIL
382300951025     C                   EXSR      CTRFIL
382400900530     C                   MOVEL     *BLANKS       DESFLP
382500971203     C*
382600971203     C     *IN34         IFEQ      *OFF
382700020805     C                   CLEAR                   FNLV55DS
382800971203     C                   MOVEL     'P'           D55TPT
382900971203     C                   MOVE      CODFIL        D55LIN
383000971203     C                   MOVE      AMGDFV        D55DRF
383100971203     C                   CALL      'FNLV55R'
383200020805     C                   PARM                    FNLV55DS
383300971203     C                   END
383400951025     C* FLP PUO' ESSERE :
383500951025     C* - SE STESSO
383600040316     C* - UNA SECONDARIA DELLA STESSA FIL.ELABORATORE-non più 16/3/04
383700951025     C* - UNA FIL. DI PRIMO LIVELLO
383800040316    3C**N34D55TFP        IFNE      SAVTFP
383900040316     C**   D55TFP        ANDNE     ORGFIL
384000040316     C**                 SETON                                        34
384100040316     c**                 endif
384200040316     c     d55tfp        ifne      orgfil
384300040316     c     vidflp        andne     vidffv
384400060515     c* Accetto solo se il p.o. di scarico è il terminal di arrivo della
384500060515     c*  linea di arrivo indicata
384600060515     c     linfe0        andne     orgfil
384700040316     C                   SETON                                        34
384800951025    3C                   ENDIF
384900951025     C*
385000951025    3C     *IN34         IFEQ      *OFF
385100951025     C                   MOVEL     ORGDES        DESFLP
385200951025     C                   MOVEL     ORGFAG        TIPFLP
385300951027     C* PER UN TRAINO DEFLUENZA --> LA FILIALE DI SCARICO = ALLA FILIAL
385400951027     C     §TVDEF        IFEQ      'S'
385500951027     C     VIDFLP        ANDNE     VIDFFV
385600951027     C                   MOVEL     MSG(48)       VIDMSG
385700951027     C                   SETON                                        285190
385800951027     C                   ENDIF
385900951027     C* PER UN TRAINO LOCALE    --> LA FILIALE DI SCARICO DELL'AREA
386000951031     C*                             O SE STESSA
386100951027     C     VIDTTR        IFEQ      'L'
386200951027     C     CODFIL        LOOKUP    L1                                     83
386300951027     C     *IN83         IFEQ      *OFF
386400951031     C     VIDFLP        ANDNE     VIDFFV
386500951027     C                   MOVEL     MSG(49)       VIDMSG
386600951027     C                   SETON                                        285190
386700951027     C                   ENDIF
386800951027     C                   ENDIF
386900951027     C*
387000951025   X3C                   ELSE
387100951025     C                   MOVEL     *ALL'?'       DESFLP
387200951025     C                   MOVEL     MSG(14)       VIDMSG
387300951026     C                   SETON                                        285190
387400951025    3C                   ENDIF
387500930329     C*
387600951025     C*   NON SI PUO' VARIARE LA FILIALE DI SCARICO SE ESISTONO
387700930330     C*   BOLLE BORDERIZZATE PER LA LINEA DI ARRIVO
387800951025    3C     *IN90         IFEQ      *OFF
387900951025     C     *IN80         ANDEQ     *OFF
388000951025     C     VIDFFV        LOOKUP    LAB                                    83
388100951025     C*
388200951025    4C     *IN83         IFEQ      *ON
388300951025    5C     SAVFLP        IFLE      *ZEROS
388400951025     C                   Z-ADD     1             I
388500951025     C     VIDFFV        LOOKUP    FFV(I)                                 84
388600951025     C  N84              MOVEL     VIDFLP        SAVFLP
388700951025     C   84              MOVEL     FLP(I)        SAVFLP
388800951025    5C                   ENDIF
388900951025     C*
389000951025    5C     SAVFLP        IFNE      VIDFLP
389100951026     C                   SETON                                        512890
389200951025     C                   MOVEA     MSG(37)       K78
389300951025     C                   MOVEA     SAVFLP        K78(36)
389400951025     C                   MOVEA     VIDFFV        K78(52)
389500951025     C                   MOVEA     K78           VIDMSG
389600951025    5C                   ENDIF
389700951025    4C                   ENDIF
389800951025    3C                   ENDIF
389900951025    2C                   ENDIF
390000951025      *
390100900606      * C'E' LA FILIALE DI TRANSITO SENZA FILIALE
390200951025     C  N90VIDFLP        IFGT      *ZEROS
390300951025     C     VIDFFV        ANDLE     *ZEROS
390400940909     C                   MOVEL     MSG(15)       VIDMSG
390500951025     C                   SETON                                        509028
390600951025     C                   ENDIF
390700951025      *
390800900606      * FILIALE DI TRANSITO OBBLIGATORIA SE C'E' LA FILIALE
390900951025     C  N90VIDFFV        IFGT      *ZEROS
391000951025     C     VIDFLP        ANDLE     *ZEROS
391100940909     C                   MOVEL     MSG(17)       VIDMSG
391200951026     C                   SETON                                        519028
391300951025     C                   ENDIF
391400911105      *
391500900529      * CONTROLLO CHE LA FILIALE NON SIA RIPETUTA
391600951025     C  N90VIDFFV        IFGT      *ZEROS
391700900529     C                   Z-ADD     1             J                 3 0
391800951025     C     VIDFFV        LOOKUP    CTL(J)                                 50
391900911105     C* TROVATO ERRORE FIL DOPPIA
392000970102     C     *IN50         IFEQ      *ON
392100970102     C                   MOVEL     MSG(16)       VIDMSG
392200970102     C                   SETON                                        90  28
392300970102     C                   ELSE
392400940909     C*
392500911105     C* NON TROVATO MEMORIZZO
392600970102     C     '   '         LOOKUP    CTL(J)                                 83
392700970102     C   83              MOVEL     VIDFFV        CTL(J)
392800970102     C* MEMORIZZO IL TERMINAL DI ARRIVO
392900970102     C   83              MOVEL     LINFE0        FE0(J)
393000970102     C   83              MOVEL     VIDFLP        CFS(J)
393100970102     C                   ENDIF
393200951025     C                   ENDIF
393300951025      *
393400000829     C                   SETOFF                                       23
393500130315     C                   UPDATE    lst4S03
393600951026     C                   SETOFF                                       4051
393700951025      *
393800951025     C* POSIZIONO IL CURSORE NELLA PAGIN PER 1 ERRORE
393900951026     C   90
394000951026     CANN96              DO
394100940909     C                   Z-ADD     NRR3          REC3
394200951026     C                   SETON                                        96
394300951025     C                   ENDDO
394400951025     C*
394500951025     C   90              GOTO      ENDCT3
394600900515      *
394700940909     C                   ADD       1             NRR3
394800951025     C                   ENDDO
394900911105      * SE TUTTO OK CONTROLLO CHE NON SIANO STATE ELIMINATE LINEE BORD
395000950905     C                   Z-ADD     1             I
395100951025     C                   Z-ADD     1             REC3
395200950905     C     LAB(I)        DOWGT     *ZEROS
395300951025     C     LAB(I)        LOOKUP    CTL                                    83
395400951025     C     *IN83         IFEQ      *OFF
395500951025     C                   SETON                                        905028
395600951025     C                   MOVEA     MSG(35)       K78
395700951025     C                   MOVEL     LAB(I)        W003A             3
395800951025     C                   MOVEA     W003A         K78(33)
395900951025     C                   MOVEA     K78           VIDMSG
396000000420     C                   Z-ADD     299           I
396100951025     C                   ENDIF
396200911105     C                   ADD       1             I
396300951025     C                   ENDDO
396400911108      * CONTROLLO CHE NON SIANO STATE ELIMINATE LINEE CON SPUNTE
396500950905     C                   Z-ADD     1             I
396600950905     C     LAS(I)        DOWGT     *ZEROS
396700951025     C     LAS(I)        LOOKUP    CTL                                    83
396800951025     C     *IN83         IFEQ      *OFF
396900951025     C                   SETON                                        905028
397000951025     C                   MOVEA     MSG(36)       K78
397100951025     C                   MOVEL     LAS(I)        W003A             3
397200951025     C                   MOVEA     W003A         K78(33)
397300951025     C                   MOVEA     K78           VIDMSG
397400000420     C                   Z-ADD     299           I
397500951025     C                   ENDIF
397600951025     C                   ADD       1             I
397700951025     C                   ENDDO
397800970102     C* CONTROLLO CHE LE FILIALI CON LO STESSO TER ARR
397900970102     C*  ABBIANO LA STESSA FIL DI SCARICO
398000000316     C* (PERMETTO PERO' DI FARE UN DIRETTO --> SCARICO = LINEA)
398100970102     C                   CLEAR                   WEST
398200060116     C*                  CLEAR                   WESTER
398300970102     C                   CLEAR                   WITA
398400970102    0C     VIDTTR        IFEQ      'S'
398500000316    0C     VIDTTR        OREQ      ' '
398600970102     C     §TVDEF        OREQ      'S'
398700970102     C                   Z-ADD     1             I
398800970102     C*
398900970102    1C     I             DOWLE     151
399000970102     C     CTL(I)        ANDGT     *ZEROS
399100970102     C**
399200970102    2C     VIDTTR        IFEQ      'S'
399300000316    2C     VIDTTR        OREQ      ' '
399400000316     C* SE LA LINEA E' UN SECONDO LIVELLO IN ARRIVO E NON SCARICA A
399500000316     C* SE' STESSA DEVE ESSERE PRESENTE COME LINEA ANCHE IL SUO TER-ARR
399600000316   2AC     CTL(I)        IFNE      FE0(I)
399700000316     C     CTL(I)        ANDNE     CFS(I)
399800000316     C     FE0(I)        LOOKUP    CTL                                    31
399900000316   2BC     *IN31         IFEQ      *OFF
400000000316     C* ERRORE
400100000316     C                   SETON                                        905028
400200000316     C                   MOVEA     MSG(74)       K78
400300000316     C                   MOVEL     CTL(I)        W003A
400400000316     C                   MOVEA     W003A         K78(37)
400500000316     C                   MOVEA     K78           VIDMSG
400600000316     C                   Z-ADD     160           I
400700000316   2BC                   ENDIF
400800000316   2AC                   ENDIF
400900970102     C                   Z-ADD     1             XX
401000000316   2AC  N90CTL(I)        IFNE      CFS(I)
401100970102     C                   MOVEL     FE0(I)        W003A
401200970102     C     W003A         LOOKUP    FE0(XX)                                31
401300970102    3C     *IN31         DOWEQ     *ON
401400970102    4C     CFS(XX)       IFNE      CFS(I)
401500000316     C     CTL(XX)       ANDNE     CFS(XX)
401600000316     C     CFS(XX)       ORNE      CFS(I)
401700000316     C     CTL(XX)       ANDEQ     FE0(XX)
401800970102     C* ERRORE
401900970102     C                   SETON                                        905028
402000970102     C                   MOVEA     MSG(70)       K78
402100970102     C                   MOVEL     CTL(I)        W003A
402200970102     C                   MOVEA     W003A         K78(13)
402300970102     C                   MOVEL     CTL(XX)       W003A
402400970102     C                   MOVEA     W003A         K78(21)
402500970102     C                   MOVEA     K78           VIDMSG
402600970102     C                   Z-ADD     160           I
402700970102     C                   SETOFF                                       31
402800970102   X4C                   ELSE
402900970102     C                   ADD       1             XX
403000970102     C*
403100970102     C     W003A         LOOKUP    FE0(XX)                                31
403200970102    4C                   ENDIF
403300970102    3C                   ENDDO
403400000316   2AC                   ENDIF
403500970102     C*
403600970102   X2C                   ELSE
403700990514     C* PER I TIPI TRAINO DEFLUENZA CONTROLLO SE 1 LNA E' ESTERO
403800970102     C*  CHE ANCHE TUTTE LE ALTRE LO SIANO
403900000405     C*  LA DPD LA CONSIDERO ESTERA
404000990514     C                   MOVEL     CTL(I)        CODFIL
404100990514     C                   EXSR      CTRFIL
404200020213     c                   if        esteroedpd = 'S'
404300970102     C                   MOVEL     'S'           WEST              1
404400970102     C                   ELSE
404500970102     C                   MOVEL     'S'           WITA              1
404600970102    3C                   ENDIF
404700970102     C*
404800970102    3C     WEST          IFEQ      'S'
404900970102     C     WITA          ANDEQ     'S'
405000970102     C                   SETON                                        905028
405100970102     C                   MOVEL     MSG(71)       VIDMSG
405200970102     C                   Z-ADD     160           I
405300970102    3C                   ENDIF
405400970102    2C                   ENDIF
405500970102     C*
405600970102     C                   ADD       1             I
405700970102    1C                   ENDDO
405800970102     C*
405900970102    0C                   ENDIF
406000970102     C* CONTROLLO I FV DEFLUENZA DELL'ESTERO
406100970102     C                   EXSR      CTRDEF
406200900515      *
406300951025     C     ENDCT3        ENDSR
406400900515      *-----------------------------------------------------***********
406500900515      * REGISTRAZIONE RECORD                                *  REGIS  *
406600900515      *-----------------------------------------------------***********
406700900515     C     REGIS         BEGSR
406800000605     C*
406900130510     c                   clear                   winviamail
407000020805     c                   eval      FGWnfv = VIDnfv
407100020805     c                   eval      FGWlnp = SIMfel
407200020805     c     K02FGW01      chain     FNFGW000                           39
407300000427     C   39              CLEAR                   FNFGW000
407400110512     c     K02FGv01      chain     FNFGS000                           36
407500110512     C   36              CLEAR                   FNFGS000
407600110512
407700951025     C                   CLEAR                   WABB
407800000405     C                   MOVEA     TFV           SFV
407900951130     C                   MOVEA     TLP           SLP
408000900515      *
408100900515     C                   MOVEL     *BLANK        FGVATB
408200900515     C                   Z-ADD     VIDNFV        FGVNFV
408300900515      *
408400951024     C                   Z-ADD     AMGDFV        FGVDFV
408500900515      *
408600130419     C                   MOVE      VIDfgs        FGVLNP
408700940913     C                   MOVE      VIDPDR        FGVPDR
408800010920     C                   Z-ADD     VIDIPT        FGWIPT
408900010920     C     FGWIPT        IFGT      *ZEROS
409000010920     C                   MOVEL     VIDVIP        FGWVIP
409100010920     C                   ENDIF
409200000202     C  N80              MOVE      FGVLNA        LNAPRE
409300900515     C                   MOVE      VIDLNA        FGVLNA
409400900515      *
409500951025     C                   Z-ADD     AMGDTP        FGVDTP
409600900515     C                   Z-ADD     VIDHMP        FGVHMP
409700130510     C                   MOVEL     VIDTMZ        FGVTMZ
409800900515      *
409900900515     C                   MOVEL     VIDDPD        FGVDPD
410000950913     C                   MOVEL     VIDTTR        FGVTTR
410100900515     C                   MOVEL     VIDTRR        FGVTRR
410200900515      *
410300900606     C                   MOVEL     VIDNT1        FGVNT1
410400900606     C                   MOVEL     VIDNT2        FGVNT2
410500900628     C                   MOVEL     VIDSNS        FGVSNS
410600960123     C                   MOVEL     VIDBIS        FGVSNP
410700900606      *
410800920204     C   80              MOVEL     *BLANKS       FGVFCF
410900940909     C                   MOVEL     *BLANKS       FGVFTR
411000940909     C                   Z-ADD     0             FGVDTR
411100911031     C   80              Z-ADD     0             FGVNFA
411200000202      * SFLAGGO LE SPUNTE SE MODIFICATA LA FILIALE FINALE DI ARRIVO
411300000202     C     *IN80         IFEQ      *OFF
411400000202     C     FGVLNA        ANDNE     LNAPRE
411500000202     C                   MOVEL     ' '           WMOD              1
411600000202     C                   MOVEL     LNAPRE        COMLNA
411700000202     C                   EXSR      LIBSPU
411800000202     C                   ENDIF
411900900515      * LEGGO IL SFL
412000951025     C                   CLEAR                   FLP
412100951025     C                   CLEAR                   FFV
412200900516      *
412300900515     C                   Z-ADD     1             I
412400940909     C                   Z-ADD     1             NRR3
412500951025     C*
412600000420    1C                   DO        299           NRR3
412700130315     C     NRR3          CHAIN     lst4S03                            81
412800951025     C*
412900951025    2C  N81VIDFFV        IFNE      *BLANKS
413000900516     C     VIDFFV        ANDNE     *ZEROS
413100900515     C                   MOVE      VIDFFV        FFV(I)
413200900522      *
413300900522     C                   MOVE      VIDFLP        FLP(I)
413400951025    3C     *IN80         IFEQ      *OFF
413500980303     C* SE MODIFICATO LO SCARICO SFLAGGO LE SPUNTE
413600920428     C                   Z-ADD     1             J
413700951027     C     VIDFFV        LOOKUP    SFV(J)                                 31
413800951027    4C   31VIDFLP        IFNE      SLP(J)
413900980305     C                   MOVEL     ' '           WMOD              1
414000920428     C                   MOVEL     VIDFFV        COMLNA
414100920428     C                   EXSR      LIBSPU
414200951025    4C                   ENDIF
414300980226     C* SE AGGIUNTA UNA LINEA VERIFICO ESISTENZA SPUNTE CON FTR='L'
414400000202     C  N31              MOVE      VIDFFV        W0030             3 0
414500980226     C     *IN31         IFEQ      *OFF
414600000202     C     W0030         ANDNE     LNAPRE
414700980305     C                   MOVEL     VIDFFV        COMLNA
414800980305     C                   MOVEL     'A'           WMOD              1
414900980305     C                   EXSR      LIBSPU
415000980226     C                   END
415100951025    3C                   ENDIF
415200900522      *
415300900516     C                   ADD       1             I
415400951025    2C                   ENDIF
415500951025    1C                   ENDDO
415600960301     C**
415700960301     C* CONTROLLO SE IL FV SI SBINA
415800960301     C                   EXSR      SBIFV
415900960302     C   90              GOTO      ENDREG
416000951027     C*
416100951027     C**
416200950901     C* SE NON E' STATO MODIFICATO NULLA ---> ABBINO
416300951218    1C     WABB          IFEQ      ' '
416400951027     C                   MOVEL     VIDTRN        FGVTRN
416500951018     C                   MOVEL     WTFM          FGVFRQ
416600950901     C                   ELSE
416700950901     C                   CLEAR                   FGVTRN
416800960130     C                   CLEAR                   VIDTRN
416900951018     C                   CLEAR                   FGVFRQ
417000951218    1C                   ENDIF
417100130510     c* Se conducente e/o targa non accreditati e sono in immissione o
417200130510     c*  modificato in manutenzione --> invio mail alla traini
417300130510     c                   if        wforzD1='A' and (*in80 or
417400130510     c                             viddcn<>fgvpmb)
417500130510     c                   eval      winviamail='D'
417600130510     c                   endif
417700130510     c                   if        wforzM1='A' and (*in80 or
417800130510     c                             vidtrm<>fgvtrm)
417900130510
418000130510     c                   if        winviamail=' '
418100130510     c                   eval      winviamail='M'
418200130510     c                   else
418300130510     c                   eval      winviamail='E'
418400130510     c                   endif
418500130510
418600130510     c                   endif
418700130510     c
418800130510     c                   if        winviamail<>' '
418900130510     c                   exsr      Inv_email
419000130510     c                   endif
419100130510     c
419200130510     C*****              MOVEL     VIDDCN        FGVDCN
419300130510     C                   MOVEL     VIDDCN        FGVPMB
419400130510     C                   MOVEL     VIDTRM        FGVTRM
419500951103     C*
419600951031     C* SCRIVO LE ANNOTAZIONI
419700960123     C*  CANCELLO E RISCRIVO
419800960123     C                   MOVEL     'P'           KEPA
419900960123     C                   MOVEL     'N'           KTRC
420000960123     C                   CLEAR                   KTFA
420100960126     C                   MOVEL     WTRSN2        WTRAS2
420200960126     C                   MOVEL     WTRSN1        WTRAS1
420300960126     C*
420400960126    1C     VIDAN1        IFEQ      *BLANKS
420500960126     C     VIDAN2        ANDNE     *BLANKS
420600960126     C                   MOVEL     VIDAN2        VIDAN1
420700960126     C                   CLEAR                   VIDAN2
420800960126    1C                   ENDIF
420900960123     C*
421000000605     C  N80KFV4          SETLL     FNFV401L
421100960126     C  N80KFV4          READE     FNFV401L                               83
421200960201    2C  N80*IN83         DOWEQ     *OFF
421300960126     C* SE ALMENO UNA DELLE 2 NOTE E' RIMASTA CANCELLO TUTTO E
421400960126     C*  RISCRIVO, ALTRIMENTI ANNULLANDO I RECORD UPDATANDO
421500960126     C* SE NON C'E' TRASMISISONE ANNULLO TUTTO
421600960126    3C     VIDAN1        IFNE      *BLANKS
421700960126     C     VIDAN2        ORNE      *BLANKS
421800960126     C     WTRSN1        ORNE      'S'
421900960126     C     WTRSN2        ANDNE     'S'
422000960123     C                   DELETE    FNFV4000
422100960126   X3C                   ELSE
422200960126     C* SOLO SE RECORD NON GIA' ANNULLATO
422300960126    4C     FV4ATB        IFEQ      ' '
422400960126     C     VIDAN1        ANDEQ     *BLANKS
422500960126     C     VIDAN2        ANDEQ     *BLANKS
422600960126     C*
422700960126     C                   MOVEL     'A'           FV4ATB
422800960126     C                   MOVEL     'N'           FV4FT1
422900060711     C                   MOVEL     dateu         FV4DT1
423000960126    5C     WTRSN2        IFNE      'N'
423100960126     C                   CLEAR                   FV4FT2
423200960126     C                   CLEAR                   FV4DT2
423300960126     C                   ELSE
423400960126     C                   MOVEL     'N'           FV4FT2
423500060711     C                   MOVEL     dateu         FV4DT2
423600960126    5C                   ENDIF
423700960126     C                   UPDATE    FNFV4000
423800960126    4C                   ENDIF
423900960126    3C                   ENDIF
424000960123     C*
424100960123     C     KFV4          READE     FNFV401L                               83
424200960123    2C                   ENDDO
424300951122     C*
424400960126     C* NOTE 1
424500951218    1C     VIDAN1        IFNE      *BLANKS
424600951031     C                   MOVEL     VIDAN1        FV4NOT
424700951031     C                   EXSR      WTRFV4
424800960123    1C                   ENDIF
424900951031     C* NOTE 2
425000960123    1C     VIDAN2        IFNE      *BLANKS
425100951031     C                   MOVEL     VIDAN2        FV4NOT
425200951031     C                   EXSR      WTRFV4
425300960123    1C                   ENDIF
425400950901     C*
425500951106     C* SCRIVO CAUSALE RITARDO/ANTICIPO SOLO SE IMMESSA OPPURE SE E'
425600951106     C*   STATA TOLTA MA PRIMA C'ERA (QUESTO PER INVIARE IN SEDE IL
425700951106     C*   RECORD VUOTO)
425800951218    1C     VIDRTC        IFNE      ' '
425900951106     C     VIDRTC        OREQ      ' '
426000951106     C     SAVRTC        ANDNE     ' '
426100951103     C                   EXSR      WTRFV2
426200951218    1C                   ENDIF
426300960126     C*
426400010622     C* N° CELLULARE
426500010622     C                   MOVEL     'P'           KEPA
426600010622     C                   MOVEL     'L'           KTRC
426700010622     C                   CLEAR                   KTFA
426800010622     C                   MOVEL     WTRSL2        WTRAS2
426900010622     C                   MOVEL     WTRSL1        WTRAS1
427000010920     C     KFV4          CHAIN     FNFV401L                           31
427100010920  A  C     *IN31         IFEQ      *OFF
427200010702     C                   MOVEL     FV4NOT        WCELL            16
427300010702  B  C     VIDCNC        IFNE      *BLANKS
427400010702  C  C     WCELL         IFNE      VIDCNC
427500010702     C                   CLEAR                   FV4NOT
427600040519     C                   MOVEL     'N'           FV4FT1
427700040519  D1 C     WTRSN2        IFNE      'N'
427800040519     C                   CLEAR                   FV4FT2
427900040519 XD1 C                   ELSE
428000040519     C                   MOVEL     'N'           FV4FT2
428100040519  D1 C                   ENDIF
428200010702     C                   MOVEL     VIDCNC        FV4NOT
428300010702     C                   UPDATE    FNFV4000
428400040519     c                   else
428500040519     c* disalloco record se cellulare non cambiato
428600040519     c                   unlock    fnfv401l
428700010702  C  C                   ENDIF
428800040519     c* Tolto il cellulare
428900010702 XB  C                   ELSE
429000010702  C  C     WTRSL1        IFNE      'S'
429100010702     C     WTRSL2        ANDNE     'S'
429200010702     C                   DELETE    FNFV401L
429300010702 XC  C                   ELSE
429400010702     C                   MOVEL     'A'           FV4ATB
429500060711     C                   MOVEL     'N'           FV4FT1
429600060711     C                   MOVEL     dateu         FV4DT1
429700010702  D1 C     WTRSN2        IFNE      'N'
429800010702     C                   CLEAR                   FV4FT2
429900010702     C                   CLEAR                   FV4DT2
430000010702 XD1 C                   ELSE
430100010702     C                   MOVEL     'N'           FV4FT2
430200060711     C                   MOVEL     dateu         FV4DT2
430300010702  D1 C                   ENDIF
430400010702     C                   UPDATE    FNFV4000
430500010702  C  C                   ENDIF
430600010702  B  C                   ENDIF
430700010702 XA  C                   ELSE
430800010702  E  C     VIDCNC        IFNE      *BLANKS
430900011017     C                   CLEAR                   FV4NOT
431000010622     C                   MOVEL     VIDCNC        FV4NOT
431100010702     C                   EXSR      WTRFV4
431200010702  E  C                   ENDIF
431300010702  A  C                   ENDIF
431400130424     C*
431500130424     C* PIOMBI
431600130424     C                   MOVEL     'P'           KEPA
431700130424     C                   MOVEL     'P'           KTRC
431800130424     C                   CLEAR                   KTFA
431900130424     C                   MOVEL     WTRSP2        WTRAS2
432000130424     C                   MOVEL     WTRSP1        WTRAS1
432100130424     c
432200130424    1c                   if        not *in80
432300130424     C     KFV4          setll     FNFV401L
432400130424     C     KFV4          reade     FNFV401L
432500130424    2C                   dow       not %eof(fnfv401l)
432600130424    3c                   if        vidpmb<>*blanks
432700130424     c                             or (wtras1<>'S' and wtras2<>'S')
432800130424     c                   delete    fnfv4000
432900130424   x3c                   else
433000130424     C* SOLO SE RECORD NON GIA' ANNULLATO
433100130424    4C                   if        fv4atb=' ' and vidpmb=*blanks
433200130424     C*
433300130424     C                   MOVEL     'A'           FV4ATB
433400130424     C                   MOVEL     'N'           FV4FT1
433500130424     C                   MOVEL     dateu         FV4DT1
433600130424    5C     WTRSP2        IFNE      'N'
433700130424     C                   CLEAR                   FV4FT2
433800130424     C                   CLEAR                   FV4DT2
433900130424     C                   ELSE
434000130424     C                   MOVEL     'N'           FV4FT2
434100130424     C                   MOVEL     dateu         FV4DT2
434200130424    5C                   ENDIF
434300130424     C                   UPDATE    FNFV4000
434400130424    4c                   endif
434500130424    3c                   endif
434600130424     C     KFV4          reade     FNFV401L
434700130424    2c                   enddo
434800130424    1c                   endif
434900130424    c
435000130424    1c                   if        vidpmb<>*blanks
435100130424     c                   movel     vidpmb        fv4not
435200130424     c                   exsr      wtrfv4
435300130424     c* vedo se scivere anche secondo record
435400130424    2c                   if        %subst(vidpmb:36)<>*blanks
435500130424     c                   eval      fv4not=%subst(vidpmb:36)
435600130424     c                   exsr      wtrfv4
435700130424    2c                   endif
435800130424    1c                   endif
435900010622     C*
436000010622     C*
436100960126     C                   CLEAR                   FGVFTR
436200960126     C                   CLEAR                   FGVDTR
436300011011      * Pulisco fgvipt xchè non serve più
436400011011     C                   CLEAR                   FGVIPT
436500951103     C*
436600950905     C   80              WRITE     FGV01
436700950905     C  N80              UPDATE    FGV01
436800000420     C*
436900000420     C* FILE FOGLI VIAGGIO ESTENSIONE
437000000427     C                   MOVEL     W1FF3         FGWFF3
437100000427     C                   MOVEL     W1FF4         FGWFF4
437200000427     C                   MOVEL     W1FL3         FGWFL3
437300000427     C                   MOVEL     W1FL4         FGWFL4
437400130419     C                   CLEAR                   FGWFGS
437500110512     c
437600000427     C     FGWFF3        IFNE      *BLANKS
437700000427     C     FGWFF4        ORNE      *BLANKS
437800000427     C     FGWFGS        ORNE      0
437900011011     C     FGWIPT        ORNE      0
438000000427     C  N39              CLEAR                   FGWATB
438100000427     C  N39              UPDATE    FNFGW000
438200000427     C*
438300000427     C   39              MOVEL     FGVNFV        FGWNFV
438400000427     C   39              MOVEL     FGVLNP        FGWLNP
438500000427     C   39              WRITE     FNFGW000
438600000427     C                   ELSE
438700000427     C  N39              DELETE    FNFGW000
438800000427     C                   ENDIF
438900110512     c
439000110512     c* PUNTI DI SCAMBIO
439100110512     C                   if        vidpsc<>*blanks
439200110512     c                   eval      fgspsc=vidpsc
439300110512     c                   eval      fgsdgs=amgdGS
439400110512     c                   eval      fgshhs=vidhhS
439500110512     c                   if        vidtrns>*zeros
439600110530     c                   eval      fgstrns=vidtrns
439700110512     c                   else
439800110512     c                   clear                   fgstrns
439900110512     c                   endif
440000110530     c* se c'e' fil finale airrov aut ddevo pulire il traino
440100110530     c                   if        vidlnar>*zeros
440200110530     c                   movel     vidlnar       fgslnar
440300110530     c                   clear                   fgstrns
440400110530     c                   endif
440500110513     c
440600110512     c
440700110512     c                   if        not *in36
440800110512     C                   CLEAR                   FGSATB
440900110512     C                   UPDATE    FNFGS000
441000110512     c                   else
441100110512     C                   MOVEL     FGVNFV        FGSNFV
441200110512     C                   MOVEL     FGVLNP        FGSLNP
441300110512     C                   WRITE     FNFGS000
441400110512     c                   endif
441500110512     c
441600110512     C                   ELSE
441700110512     C  N36              DELETE    FNFGS000
441800110512     C                   ENDIF
441900110512     c
442000110512     c
442100960122     C* CALCOLO DELLE DATE TEORICHE PARTENZE E ARRIVO
442200960122     C                   EXSR      DTTEOR
442300130607
442400130607     c* Scrittura/aggiornamento file di appoggio per stampa etichette
442500130607     c                   exsr      sr_fnfge
442600940913     C*
442700960119     C* SE HO MODIFICATO UN F.V. GIA' CHIUSO DEVO TRASMETTERLO IN SEDE
442800960119     C*   E ALLA FILIALE DI ARRIVO (CHE PUO' ESSERE PIU' DI UNA)
442900960126     C                   EXSR      TRAVAR
443000960126     C     ENDREG        ENDSR
443100960301     C*-----------------------------------------------------***********
443200960301     C* CONTROLLO SE IL FV SI SBINA
443300960301     C*-----------------------------------------------------***********
443400960301     C     SBIFV         BEGSR
443500960604     C                   SETOFF                                       90
443600960302     C                   MOVEL     'C'           KTRC
443700960302     C                   MOVEL     WTRSC1        WTRAS1
443800960302     C                   MOVEL     WTRSC2        WTRAS2
443900960301      *
444000960301     C* VERIFICO SE POSSO ABBINARE O MENO IL FV.. NON SI ABBINA SE :
444100960301     C**
444200960301    1C     VIDTRN        IFGT      0
444300960301     C**
444400960301     C* TRAINO IN CAMPO ALFABETICO SENZA 0 NON SIGNIFICATIVI
444500960301     C                   MOVEL     VIDTRN        W007A
444600960301     C                   MOVEA     W007A         K15(9)
444700960301     C                   Z-ADD     9             Z
444800960301     C                   EXSR      ABBL
444900960301     C                   MOVEA     K15(9)        W007A
445000960301     C**
445100960301     C                   MOVEL     VIDLNA        WLNAF
445200960301     C*
445300960301     C* 2) MESSO IMPORTO PATTUITO PER FILIALE CHE NON DEVE PAGARE
445400960301     C* 2)
445500000612    2C     TLTFES        IFNE      VIDFGS
445600960301     C     VIDIPT        ANDGT     0
445700960301     C     WTFM          ORNE      WFMFES
445800960301     C     VIDIPT        ANDGT     0
445900960301     C                   MOVEL     '1'           WABB
446000960301     C*
446100960302     C* SOLO AL 2 GIRO QUANDO HO EMESSO IL MESSAGGIO
446200960302     C     WFORZ6        IFNE      ' '
446300960301     C                   MOVEA     SBI(2)        K35
446400960301     C                   MOVEA     W007A         K35(1)
446500960301     C*
446600960301     C* CALL AL PGM CHE SCRIVE I RECORD DI SBINAMENTO
446700960301     C                   MOVEA     K35           SBI(2)
446800960301     C                   Z-ADD     2             WREC              2 0
446900960301     C                   Z-ADD     1             I
447000960301     C                   EXSR      WRTSBI
447100960301     C                   ENDIF
447200960302     C                   ENDIF
447300960301     C*
447400960301     C* 3) IL TRAINO NON C'E' NEL GIORNO = DATA TRAINO
447500960301     C* 3)
447600960302     C     WFORZ         IFEQ      'S'
447700960301     C                   MOVEL     '1'           WABB
447800960302     C*
447900960302     C     WFORZ6        IFNE      ' '
448000960302     C*
448100960302     C* IMPOSTO IL NUMERO TRAINO
448200960302     C                   MOVEA     SBI(3)        K35
448300960302     C                   MOVEA     W007A         K35(8)
448400960302     C                   MOVEA     K35           SBI(3)
448500960302     c* imposto il giorno della settimana
448600960302     C* REPERISCO IL GIORNO DELLA SETTIMANA (1=LUN ...)
448700960302     C     SAVSET        IFEQ      ' '
448800960302     C                   CALL      'XGIOSE1'
448900960302     C                   PARM                    GGMMAA            6
449000960302     C                   PARM                    GIOSET            1
449100960302     C                   MOVEL     GIOSET        SAVSET
449200960302     C                   ENDIF
449300960302     C*
449400960302     C                   CLEAR                   X
449500960302     C                   MOVE      SAVSET        X
449600960302     C                   MOVEA     SBI(4)        K35
449700960302     C                   MOVEL     GIO(X)        W009A             9
449800960302     C                   MOVEA     W009A         K35(11)
449900960302     C                   MOVEA     K35           SBI(4)
450000960302     c*
450100960302     C                   Z-ADD     2             WREC              2 0
450200960302     C                   Z-ADD     3             I
450300960302     C                   EXSR      WRTSBI
450400960302     C                   ENDIF
450500960301     C                   ENDIF
450600960301     C*
450700960301     C* 4) ESISTE GIA'UN FV ABBINATO IN QUEL GIORNO PER QUEL TRAINO/A-R
450800960301     C* 4)
450900960301     C* SOLO PER IMMISSIONE VALE IL DISCORSO DI ABBINAMENTO DOPPIO
451000960301     C     WFORZ2        IFEQ      'S'
451100960301     C     *IN80         ANDEQ     *ON
451200960301     C                   MOVEL     '1'           WABB
451300960302     C*
451400960302     C     WFORZ6        IFNE      ' '
451500960302     C* IMPOSTO IL NUJMERO TRAINO
451600960302     C                   MOVEA     SBI(6)        K35
451700960302     C                   MOVEA     W007A         K35(8)
451800960302     C                   MOVEA     K35           SBI(6)
451900960302     C* IMPOSTO IL NUMERO DI FV
452000960302     C                   MOVEA     SBI(5)        K35
452100960302     C                   MOVEA     SAV005        K35(14)
452200960302     C                   MOVEA     K35           SBI(5)
452300960302     c*
452400960302     C                   Z-ADD     2             WREC              2 0
452500960302     C                   Z-ADD     5             I
452600960302     C                   EXSR      WRTSBI
452700960301     C                   ENDIF
452800960302     C                   ENDIF
452900960301     C*
453000960301     C* 5) MODIFICATO IL TIPO TRAINO
453100960301     C* 5)
453200960301     C     WFORZ7        IFEQ      'S'
453300960301     C                   MOVEL     '1'           WABB
453400960302     C     WFORZ6        IFNE      ' '
453500960302     C* IMPOSTO IL NUJMERO TRAINO
453600960302     C                   MOVEA     SBI(7)        K35
453700960302     C                   MOVEA     W007A         K35(12)
453800960302     C                   MOVEA     K35           SBI(7)
453900960302     C* IMPOSTO IL TIPO TRAINO
454000960302     C                   MOVEA     SBI(8)        K35
454100960302     C                   MOVEA     TLTTTR        K35(17)
454200960302     C                   MOVEA     VIDTTR        K35(22)
454300960302     C                   MOVEA     DESTTR        K35(24)
454400960302     C                   MOVEA     K35           SBI(8)
454500960302     c*
454600960302     C                   Z-ADD     2             WREC              2 0
454700960302     C                   Z-ADD     7             I
454800960302     C                   EXSR      WRTSBI
454900960302     C                   ENDIF
455000960301     C                   ENDIF
455100960301     C*
455200960301     C* 6) MODIFICATA LA DATA FV
455300960301     C* 6)
455400960301     C     WFORZ9        IFEQ      'S'
455500960301     C                   MOVEL     '1'           WABB
455600960302     C     WFORZ6        IFNE      ' '
455700960302     C* IMPOSTO IL NUMERO TRAINO
455800960302     C                   MOVEA     SBI(9)        K35
455900960302     C                   MOVEA     W007A         K35(19)
456000960302     C                   MOVEA     K35           SBI(9)
456100960302     c*
456200960302     C                   Z-ADD     2             WREC              2 0
456300960302     C                   Z-ADD     9             I
456400960302     C                   EXSR      WRTSBI
456500960302     C                   ENDIF
456600960301     C                   ENDIF
456700960301     C*
456800960301     C* 7) MODIFICATA LA CATEGORIA TRAINO
456900960301     C* 7)
457000110512     C     WFORZD        IFEQ      'S'
457100110512     C                   MOVEL     '1'           WABB
457200110512     C     WFORZ6        IFNE      ' '
457300110512     C* IMPOSTO IL NUMERO TRAINO
457400110512     C                   MOVEA     SBI(11)       K35
457500110512     C                   MOVEA     W007A         K35(12)
457600110512     C                   MOVEA     K35           SBI(11)
457700110512     C* IMPOSTO la categoria traino
457800110512     C                   MOVEA     SBI(12)       K35
457900110512     C     VIDBIS        IFEQ      'B'
458000110512     C                   MOVEA     'BIS'         K35(22)
458100110512     C                   ELSE
458200110512     C                   MOVEA     '   '         K35(22)
458300110512     C                   ENDIF
458400110512     C                   MOVEA     K35           SBI(12)
458500110512     c*
458600110512     C                   Z-ADD     2             WREC              2 0
458700110512     C                   Z-ADD     11            I
458800110512     C                   EXSR      WRTSBI
458900110512     C                   ENDIF
459000110512     C                   ENDIF
459100110512     C*
459200110530     C* 8) aggiunta linea finale ritorno autista per Scambio
459300110512     C* 8)
459400110530     C     WFORZlnar     IFEQ      'S'
459500960301     C                   MOVEL     '1'           WABB
459600960302     C     WFORZ6        IFNE      ' '
459700960302     C* IMPOSTO IL NUMERO TRAINO
459800110530     C                   MOVEA     SBI(22)       K35
459900960302     C                   MOVEA     W007A         K35(12)
460000110530     C                   MOVEA     K35           SBI(22)
460100110530     C* IMPOSTO linea finale riitorno aut aggiunta
460200110530     C                   MOVEA     SBI(23)       K35
460300110530     C                   MOVEA     vidlnar       K35(8)
460400110530     C                   MOVEA     K35           SBI(23)
460500960302     c*
460600960302     C                   Z-ADD     2             WREC              2 0
460700110512     C                   Z-ADD     20            I
460800960302     C                   EXSR      WRTSBI
460900960302     C                   ENDIF
461000960301     C                   ENDIF
461100110530     C* 9) MODIFICATO IL PUNTO DI SCAMBIO
461200110530     C* 9)
461300110530     C     WFORZsc       IFEQ      'S'
461400110530     C                   MOVEL     '1'           WABB
461500110530     C     WFORZ6        IFNE      ' '
461600110530     C* IMPOSTO IL NUMERO TRAINO
461700110530     C                   MOVEA     SBI(20)       K35
461800110530     C                   MOVEA     W007A         K35(12)
461900110530     C                   MOVEA     K35           SBI(20)
462000110530     C* IMPOSTO opunti di scambio modificato
462100110530     C                   MOVEA     SBI(21)       K35
462200110530     C                   MOVEA     TLspsc        K35(13)
462300110530     C                   MOVEA     vidpsc        K35(26)
462400110530     C                   MOVEA     K35           SBI(21)
462500110530     c*
462600110530     C                   Z-ADD     2             WREC              2 0
462700110530     C                   Z-ADD     20            I
462800110530     C                   EXSR      WRTSBI
462900110530     C                   ENDIF
463000110530     C                   ENDIF
463100110512     c
463200960301     C*
463300960301     C* 1) CAMBIATA LNA ARRIVO FINALE
463400110512     C* 9) CAMBIATA UNA LINEA DI ARRIVO O FIL SCARICO
463500110512     C* 9)
463600960301     C* 1)
463700960302     C                   CLEAR                   WMSG
463800970103     C                   CLEAR                   XX
463900970103     C                   CLEAR                   YY
464000970103     C                   CLEAR                   ZZ
464100970103     C                   CLEAR                   VAR
464200970103     C                   CLEAR                   AGG
464300970103     C                   CLEAR                   ELM
464400970103     C*
464500960301    2C     TRNTFA        IFNE      WLNAF
464600960301     C     WFORZ8        ORNE      ' '
464700960301     C                   MOVEL     '1'           WABB
464800970103     C                   MOVEL     'Y'           WMSG              1
464900970103    3C     WFORZ6        IFNE      ' '
465000970103     C* MSG GENERICO DI MODIFICA LINEE
465100970103     C                   EXSR      MSGMOD
465200970103     C*
465300970103     C                   MOVEA     SBI(16)       K35
465400970103     C                   MOVEL     WLNAF         W003A
465500970103     C                   MOVEA     W003A         K35(32)
465600970103     C                   MOVEL     TRNTFA        W003A
465700970103     C                   MOVEA     W003A         K35(26)
465800970103     C                   MOVEA     K35           SBI(16)
465900970103     C*
466000970103     C                   Z-ADD     1             WREC
466100970103     C                   Z-ADD     16            I
466200970103     C                   EXSR      WRTSBI
466300970103    3C                   ENDIF
466400970103    2C                   ENDIF
466500960301     C**
466600960301     C                   Z-ADD     1             I
466700000420    2C     I             DOWLE     299
466800970103   2AC     FFV(I)        IFGT      *ZEROS
466900960301     C                   Z-ADD     1             C
467000960301     C*
467100960301     C     FFV(I)        LOOKUP    SFV(C)                                 31
467200970103     C*
467300970103     C* NON TROVATA : LINEA AGGIUNTA
467400970103    3C     *IN31         IFEQ      *OFF
467500970103     C                   MOVE      '1'           WABB              1
467600970103    4C     WMSG          IFNE      'Y'
467700970103     C                   MOVEL     'S'           WMSG              1
467800970103    4C                   ENDIF
467900970103     C*
468000970103     C     XX            IFLT      53
468100970103     C                   ADD       1             XX
468200970103     C                   MOVEL     FFV(I)        AGG(XX)
468300970103     C                   ENDIF
468400970103     C*
468500970103   X3C                   ELSE
468600970103     C*
468700970103     C* TROVATA: VEDO SE MODIFICATO SCARICO
468800960301     C*
468900970103    4C     FLP(I)        IFNE      SLP(C)
469000960301     C                   MOVE      '1'           WABB
469100970103    5C     WMSG          IFNE      'Y'
469200970103     C                   MOVEL     'S'           WMSG              1
469300970103    5C                   ENDIF
469400970103     C*
469500970103    5C     YY            IFLT      53
469600970103     C                   ADD       1             YY                3 0
469700970103     C                   MOVEL     FFV(I)        VAR(YY)
469800970103    5C                   ENDIF
469900970103    4C                   ENDIF
470000970103     C*
470100960301     C                   CLEAR                   SFV(C)
470200970103    3C                   END
470300970103   2AC                   ENDIF
470400970103     C*
470500960301     C                   ADD       1             I                 3 0
470600970103     C*
470700960301    2C                   ENDDO
470800970103     C* CONTROLLO SE SONO STATE ELIMINATE DELLE LINEE
470900960301     C                   MOVEA     SFV(1)        W225A           225
471000960301     C                   MOVEA     SFV(76)       W225B           225
471100000420     C                   MOVEA     SFV(151)      W225C           225
471200000420     C                   MOVEA     SFV(226)      W225D           225
471300960301     C*
471400970103    2C     W225A         IFNE      *BLANKS
471500960301     C     W225B         ORNE      *BLANKS
471600960301     C                   MOVE      '1'           WABB
471700970103    3C     WMSG          IFNE      'Y'
471800970103     C                   MOVEL     'S'           WMSG              1
471900970103    3C                   ENDIF
472000970103     C* CONTROLLO LE LINEE CHE SONO STATE ELIMINATE
472100000420    3C                   DO        299           I
472200970103    4C     SFV(I)        IFGT      *ZEROS
472300970103     C     ZZ            ANDLT     53
472400970103     C                   ADD       1             ZZ                3 0
472500970103     C                   MOVEL     SFV(I)        ELM(ZZ)
472600970103    4C                   ENDIF
472700970103    3C                   ENDDO
472800970103     C**
472900970103    2C                   ENDIF
473000970103     C*
473100970103     C*
473200970103    2C     WFORZ6        IFNE      ' '
473300970103     C* EMETTO MSG GENERICO DI MODIFICA LINEE
473400970103    3C     WMSG          IFEQ      'S'
473500970103     C                   EXSR      MSGMOD
473600970103    3C                   ENDIF
473700970103     C**
473800970103     C*  L I N E E   A G G I U N T E
473900960302     C**
474000970103    3C     XX            IFGT      0
474100970103     C                   SETON                                        11
474200970103     C     XX            DIV       6             DIVIDI            2 0
474300970103     C                   MVR                     RESTO             2 0
474400970103    4C     RESTO         IFGT      0
474500970103     C                   ADD       1             DIVIDI
474600970103    4C                   ENDIF
474700970103     C**
474800970103     C                   Z-ADD     DIVIDI        WREC
474900970103     C                   Z-ADD     1             XX
475000970103     C                   Z-ADD     17            I
475100970103     C                   EXSR      PREAGG
475200970103     C                   EXSR      WRTSBI
475300970103     C                   SETOFF                                       11
475400970103    3C                   ENDIF
475500970103     C**
475600970103     C*  L I N E E   E L I M I N A T E
475700970103     C**
475800970103    3C     ZZ            IFGT      0
475900970103     C                   SETON                                        12
476000970103     C     ZZ            DIV       6             DIVIDI            2 0
476100970103     C                   MVR                     RESTO             2 0
476200970103    4C     RESTO         IFGT      0
476300970103     C                   ADD       1             DIVIDI
476400970103    4C                   ENDIF
476500970103     C**
476600970103     C                   Z-ADD     DIVIDI        WREC
476700970103     C                   Z-ADD     1             ZZ
476800970103     C                   Z-ADD     18            I
476900970103     C                   EXSR      PREAGG
477000970103     C                   EXSR      WRTSBI
477100970103     C                   SETOFF                                       12
477200970103    3C                   ENDIF
477300970103     C**
477400970103     C*  L I N E E   D I   C U I   V A R I A T O   S C A R I C O
477500970103     C**
477600970103    3C     YY            IFGT      0
477700970103     C                   SETON                                        13
477800970103     C     YY            DIV       4             DIVIDI            2 0
477900970103     C                   MVR                     RESTO             2 0
478000970103    4C     RESTO         IFGT      0
478100970103     C                   ADD       1             DIVIDI
478200970103    4C                   ENDIF
478300970103     C**
478400970103     C                   Z-ADD     DIVIDI        WREC
478500970103     C                   Z-ADD     1             YY
478600970103     C                   Z-ADD     19            I
478700970103     C                   EXSR      PREVAR
478800970103     C                   EXSR      WRTSBI
478900970103     C                   SETOFF                                       13
479000970103    3C                   ENDIF
479100970103     C**
479200960302     C* SE HO SCRITTO I RECORD COME ULTIMO SCRIVO NEL REC "D" IL NUM.
479300960302     C*  DEL TRAINO SBINATO NELLE PRIME 7 POSIZIONI
479400960302     C* IMPOSTO IL NUMERO TRAINO
479500960302     C                   MOVEL     VIDTRN        W007A
479600960302     C                   MOVEA     W007A         SBI(14)
479700960302     C                   MOVEL     'D'           KTRC
479800960302     C                   MOVEL     WTRSD1        WTRAS1
479900960302     C                   MOVEL     WTRSD2        WTRAS2
480000960302     c*
480100960302     C                   Z-ADD     1             WREC              2 0
480200960302     C                   Z-ADD     14            I
480300960302     C                   EXSR      WRTSBI
480400960302    2C                   ENDIF
480500960301     C**
480600960302    2C     WABB          IFEQ      '1'
480700960302     C     WFORZ6        ANDEQ     ' '
480800960302     C                   MOVEL     MSG(44)       VIDMSG
480900960302     C                   SETON                                        2890
481000960302     C                   MOVEL     'S'           WFORZ6            1
481100960302    2C                   ENDIF
481200960302     C*
481300960302   X1C                   ELSE
481400960302     c*
481500960302     C* SE NON E' ABBINATO CONTINUO A PULIRE I 2 CAMPI
481600960302     C                   MOVEL     '1'           WABB
481700960302     C* PER IMMISSIONE SCRIVO IL MESSAGGIO
481800960302    2C     *IN80         IFEQ      *ON
481900960302     C                   Z-ADD     1             WREC              2 0
482000960302     C                   Z-ADD     13            I
482100960302     C                   EXSR      WRTSBI
482200960302    2C                   ENDIF
482300960302    1C                   ENDIF
482400960301     C*
482500960301     C                   ENDSR
482600970103     c*
482700970103     C*-----------------------------------------------------***********
482800970103     C* EMETTO MESSAGGIO DI MODIFICA LINEE
482900970103     C*-----------------------------------------------------***********
483000970103     C     MSGMOD        BEGSR
483100970103     C* IMPOSTO IL NUMERO TRAINO
483200970103     C                   MOVEA     SBI(15)       K35
483300970103     C                   MOVEA     W007A         K35(29)
483400970103     C                   MOVEA     K35           SBI(15)
483500970103     c*
483600970103     C                   Z-ADD     1             WREC              2 0
483700970103     C                   Z-ADD     15            I
483800970103     C                   EXSR      WRTSBI
483900970103     C                   ENDSR
484000970103     C*-----------------------------------------------------***********
484100970114     C* PREPARO MESSAGGIO DI AGGIUNTA/ELIMINAZIONE  LINEE
484200970103     C*-----------------------------------------------------***********
484300970103     C     PREAGG        BEGSR
484400970103     C*
484500970114     C                   Z-ADD     12            KK                3 0
484600970103     C                   MOVEA     SBI(I)        K35
484700970103     C*
484800970103     C     *IN11         IFEQ      *ON
484900970103     C     XX            MULT      6             Y                 3 0
485000970103     C                   ENDIF
485100970103     C*
485200970103     C     *IN12         IFEQ      *ON
485300970103     C     ZZ            MULT      6             Y
485400970103     C                   ENDIF
485500970103     C*
485600970103     C     Y             SUB       5             C
485700970103     C**
485800970103     C     C             DO        Y             C
485900970103     C     C             IFLT      54
486000970103     C   11              MOVEL     AGG(C)        W003A
486100970103     C   12              MOVEL     ELM(C)        W003A
486200970103     C                   ELSE
486300970103     C                   MOVEL     '...'         W003A
486400970103     C                   ENDIF
486500970103     C**
486600970103     C                   MOVEA     W003A         K35(KK)
486700970103     C                   ADD       4             KK
486800970103     C                   ENDDO
486900970103     C**
487000970103     C                   MOVEA     K35           SBI(I)
487100970103     C                   ENDSR
487200970103     C*-----------------------------------------------------***********
487300970103     C* PREPARO MESSAGGIO DI VARIAZIONE FIL DI SCARICO ALLE LINEE
487400970103     C*-----------------------------------------------------***********
487500970103     C     PREVAR        BEGSR
487600970103     C*
487700970103     C                   Z-ADD     21            KK                3 0
487800970103     C                   MOVEA     SBI(I)        K35
487900970103     C*
488000970103     C     YY            MULT      4             Y
488100970103     C     Y             SUB       3             C
488200970103     C**
488300970103     C     C             DO        Y             C
488400970103     C     C             IFLT      54
488500970103     C                   MOVEL     VAR(C)        W003A
488600970103     C                   ELSE
488700970103     C                   MOVEL     '...'         W003A
488800970103     C                   ENDIF
488900970103     C**
489000970103     C                   MOVEA     W003A         K35(KK)
489100970103     C                   ADD       4             KK
489200970103     C                   ENDDO
489300970103     C**
489400970103     C                   MOVEA     K35           SBI(I)
489500970103     C                   ENDSR
489600000825     C*-----------------------------------------------------***********
489700000825     C* INSERIMENTO TERMINAL ARRIVO CON F10
489800000825     C*-----------------------------------------------------***********
489900000825     C     INSTER        BEGSR
490000000828      *
490100000828      * EMETTO SUBFILE VUOTO
490200000828      *
490300000828     C                   SETOFF                                       626368
490400000828     C                   CLEAR                   V06TE2
490500000828     C                   CLEAR                   V06DE2
490600000828     C                   CLEAR                   V06TE3
490700000828     C                   CLEAR                   V06DE3
490800000828     C                   Z-ADD     0             NRR6              3 0
490900000828     C                   SETON                                        65
491000130315     C                   WRITE     lst4C06
491100000828     C                   SETOFF                                       65
491200000829     C     1             DO        75            NRR6
491300130315     C                   WRITE     lst4S06
491400000828     C                   ENDDO
491500000829     C                   Z-ADD     1             REC6
491600130315     C                   WRITE     lst4T01
491700130315     C                   WRITE     lst4Z06
491800130315     C                   WRITE     lst4D06
491900000828     C     INIINT        TAG
492000130315     C                   EXFMT     lst4C06
492100000828     C                   SETOFF                                       626368
492200000828      *
492300000828     C   KL              SETON                                        66
492400000828     C   KL              GOTO      ENDINT
492500000828      *
492600000828      * CONTROLLO DATI IMMESSI:
492700000828      *
492800000829      * STESSI CONTROLLI ANCHE PER T.ARR E P.O. SCARICO DEL SUBFILE
492900000829      *
493000000829     C     1             DO        *HIVAL        WN                5 0
493100130315     C     WN            CHAIN     lst4S06                            69
493200000829     C   69              LEAVE
493300000829      *
493400000829     C                   SETOFF                                       62  68
493500000829      *
493600000829     C                   CLEAR                   V06DE2
493700000829     C                   CLEAR                   V06DE3
493800000829      *
493900000829     C     V06TE2        IFEQ      *ALL'0'
494000000829     C                   MOVE      *BLANKS       V06TE2
494100000829     C                   ENDIF
494200000829      *
494300000829     C     V06TE3        IFEQ      *ALL'0'
494400000829     C                   MOVE      *BLANKS       V06TE3
494500000829     C                   ENDIF
494600000829      *
494700000829     C     '?'           SCAN      V06TE2                                 69
494800000829     C     *IN69         IFEQ      *ON
494900000829     C                   EXSR      CASD19
495000000829     C                   MOVEL     §COD          V06TE2
495100130315     C                   WRITE     lst4T01
495200130315     C                   WRITE     lst4Z06
495300130315     C                   WRITE     lst4D06
495400000829     C                   ENDIF
495500000829      *
495600000829     C     '?'           SCAN      V06TE3                                 69
495700000829     C     *IN69         IFEQ      *ON
495800000829     C                   EXSR      CASD19
495900000829     C                   MOVEL     §COD          V06TE3
496000130315     C                   WRITE     lst4T01
496100130315     C                   WRITE     lst4Z06
496200130315     C                   WRITE     lst4D06
496300000829     C                   ENDIF
496400000829      *
496500000829     C     V06TE2        IFNE      *BLANKS
496600000829     C                   TESTN                   V06TE2               69
496700000829     C                   MOVE      V06TE2        W1ALF             1
496800000829     C     *IN69         IFEQ      *OFF
496900000829     C     W1ALF         ORLT      '0'
497000000829     C                   SETON                                        62
497100000829     C                   ELSE
497200020805     C                   CLEAR                   FNLV55DS
497300000829     C                   MOVEL     'A'           D55TPT
497400000829     C                   MOVE      V06TE2        D55LIN
497500000829     C     VIDFGS        IFGT      0
497600000829     C                   MOVE      VIDFGS        D55LNP
497700000829     C                   ELSE
497800000829     C                   MOVE      SIMFEL        D55LNP
497900000829     C                   ENDIF
498000000829     C                   MOVE      VIDDFV        WDAT4A
498100000829     C                   MOVEL     VIDDFV        WDAT4W            4
498200000829     C                   MOVEL     WDAT4W        WDAT2G
498300000829     C                   MOVE      WDAT4W        WDAT2M
498400000829     C                   MOVE      WDAT8A        D55DRF
498500000829     C                   CALL      'FNLV55R'
498600020805     C                   PARM                    FNLV55DS
498700000829     C     D55ERR        IFNE      ' '
498800000829     C                   SETON                                        62
498900000829     C                   ELSE
499000000829     C     D55LIN        IFNE      D55TFA
499100000829     C                   SETON                                        62
499200000829     C                   ELSE
499300000829     C     VIDTTR        IFEQ      'L'
499400000829     C     D55LIN        ANDNE     D55LNP
499500000829     C                   SETON                                        62
499600000829     C                   ELSE
499700000829     C     D55LIN        CHAIN     AZORG01L                           69
499800000829     C  N69              MOVEL(P)  ORGDES        V06DE2
499900000829     C                   ENDIF
500000000829     C                   ENDIF
500100000829     C                   ENDIF
500200000829     C                   ENDIF
500300000829     C                   ENDIF
500400000829      *
500500000829     C     *IN62         IFEQ      *ON
500600130315     C                   UPDATE    lst4S06
500700000829     C                   LEAVE
500800000829     C                   ENDIF
500900000829      *
501000000829     C     V06TE3        IFNE      *BLANKS
501100000829     C                   TESTN                   V06TE3               69
501200000829     C                   MOVE      V06TE3        W1ALF             1
501300000829     C     *IN69         IFEQ      *OFF
501400000829     C     W1ALF         ORLT      '0'
501500000829     C                   SETON                                        68
501600000829     C                   ELSE
501700020805     C                   CLEAR                   FNLV55DS
501800000829     C                   MOVEL     'A'           D55TPT
501900000829     C                   MOVE      V06TE3        D55LIN
502000000829     C     VIDFGS        IFGT      0
502100000829     C                   MOVE      VIDFGS        D55LNP
502200000829     C                   ELSE
502300000829     C                   MOVE      SIMFEL        D55LNP
502400000829     C                   ENDIF
502500000829     C                   MOVE      VIDDFV        WDAT4A
502600000829     C                   MOVEL     VIDDFV        WDAT4W            4
502700000829     C                   MOVEL     WDAT4W        WDAT2G
502800000829     C                   MOVE      WDAT4W        WDAT2M
502900000829     C                   MOVE      WDAT8A        D55DRF
503000000829     C                   CALL      'FNLV55R'
503100020805     C                   PARM                    FNLV55DS
503200000829     C     D55ERR        IFNE      ' '
503300000829     C                   SETON                                        68
503400000829     C                   ELSE
503500000829     C     D55LIN        IFNE      D55TFA
503600000829     C                   SETON                                        68
503700000829     C                   ELSE
503800000829     C     VIDTTR        IFEQ      'L'
503900000829     C     D55LIN        ANDNE     D55LNP
504000000829     C                   SETON                                        68
504100000829     C                   ELSE
504200000829     C     D55LIN        CHAIN     AZORG01L                           69
504300000829     C  N69              MOVEL(P)  ORGDES        V06DE3
504400000829     C                   ENDIF
504500000829     C                   ENDIF
504600000829     C                   ENDIF
504700000829     C                   ENDIF
504800000829     C                   ENDIF
504900000829      *
505000000829     C     *IN68         IFEQ      *ON
505100130315     C                   UPDATE    lst4S06
505200000829     C                   LEAVE
505300000829     C                   ENDIF
505400000829      *
505500000829     C     V06TE2        IFEQ      *BLANKS
505600000829     C     V06TE3        ANDNE     *BLANKS
505700000829     C                   SETON                                        62
505800130315     C                   UPDATE    lst4S06
505900000829     C                   LEAVE
506000000829     C                   ENDIF
506100000829      *
506200000829     C     V06TE3        IFEQ      *BLANKS
506300000829     C     V06TE2        ANDNE     *BLANKS
506400000829     C                   SETON                                        68
506500130315     C                   UPDATE    lst4S06
506600000829     C                   LEAVE
506700000829     C                   ENDIF
506800000829      *
506900130315     C                   UPDATE    lst4S06
507000000829      *
507100000829     C                   ENDDO
507200000829      *
507300000828      * SE VI SONO ERRORI O NON E' STATO PREMUTO F6 RIEMETTO
507400000828      *
507500000829     C     *IN62         IFEQ      *ON
507600000829     C     *IN68         OREQ      *ON
507700000829     C                   Z-ADD     NRR6          REC6
507800000829     C                   GOTO      INIINT
507900000829     C                   ENDIF
508000000828     C  N66              GOTO      INIINT
508100000828      *
508200000829      * SE E' STATO PREMUTO F6 (INSERIMENTO) E NON VI SONO ERRORI
508300000828      * ESPLODO I P.O. DI ARRIVO PER AGGIUNGERLI NELLA VIDEATA PRECEDENTE
508400000829      *
508500000830      * CARICO TUTTE LE FILIALI DI AZORG00F CON IL RELATIVO TERMINAL
508600000830      * DI ARRIVO
508700000830      *
508800000830     C                   MOVE      WDAT8A        WDATFV            8 0
508900000830     C                   MOVE      ' '           WPOSIZ            1
509000000830     C                   Z-ADD     1             L                 4 0
509100000830     C     *LOVAL        SETLL     AZORG
509200000830     C                   READ      AZORG                                  83
509300000830 --- C     *IN83         DOWEQ     *OFF
509400000830     C     ORGFVA        IFEQ      ' '
509500000830     C     ORGFAG        IFEQ      'F'
509600000830     C     ORGFAG        OREQ      'A'
509700000830      * CARICO SOLO SE NELLA DESCRIZIONE 148 DELL'ORGANIGRAMMA
509800000830      *  C'E' SCRITTO CHE LA POSSO CARICARE PER FV
509900000830     C                   MOVEL     ORGDE8        OG148
510000000830     C     §OGLFV        IFNE      'N'
510100000830     C                   Z-ADD     ORGFIL        FLA(L)
510200020805     C                   CLEAR                   FNLV55DS
510300000830     C                   MOVEL     'A'           D55TPT
510400000830     C                   MOVE      ORGFIL        D55LIN
510500000830     C                   MOVE      WDATFV        D55DRF
510600000830     C                   CALL      'FNLV55R'
510700020805     C                   PARM                    FNLV55DS
510800000830     C                   Z-ADD     D55TFA        TAA(L)
510900000830     C                   ENDIF
511000000830     C                   ADD       1             L
511100000830     C                   ENDIF
511200000830     C                   ENDIF
511300000830     C                   READ      AZORG                                  83
511400000830 --- C                   ENDDO
511500000828      *
511600000829     C     1             DO        *HIVAL        WN
511700130315     C     WN            CHAIN     lst4S06                            69
511800000829     C   69              LEAVE
511900000829     C     V06TE2        IFNE      *BLANKS
512000000829     C                   MOVE      V06TE2        WTE1              3 0
512100000829     C                   MOVE      V06TE3        WTE2              3 0
512200000830     C                   EXSR      AGGTER
512300000829     C                   ENDIF
512400000829     C                   ENDDO
512500000829      *
512600000828     C     ENDINT        ENDSR
512700000829     C*----------------------------------------------------------------
512800000829     C* AGGIORNAMENTO LINEE DI ARRIVO DOPO F6 DI INSERIMENTO TERMINAL
512900000829     C*----------------------------------------------------------------
513000000829     C     AGGTER        BEGSR
513100000829     C                   SETON                                        23
513200001006     C                   CLEAR                   WTROV
513300000830      *
513400001006    1C     VIDFGS        IFGT      *ZEROS
513500000830     C                   MOVEL     VIDFGS        WTFP
513600000830     C                   ELSE
513700000830     C                   MOVEL     SIMFEL        WTFP
513800001006    1C                   ENDIF
513900000830     C                   MOVE      ' '           WTAR              1
514000000830      *
514100000830     C                   Z-ADD     1             L
514200000830     C     WTE1          LOOKUP    TAA(L)                                 31
514300001006    1C     *IN31         DOWEQ     *ON
514400000830     C*
514500000830     C* CONTROLLO SE ESISTE ECCEZIONE
514600000830     C                   MOVE      ' '           W001$             1
514700001006     C                   MOVEL     FLA(L)        W003A
514800001006     C* SELA LINEA TROVATA NON E' IL TERMINAL DI ARRIVO FINALE
514900001006   1AC     VIDLNA        IFNE      W003A
515000001006     C                   MOVEL     FLA(L)        WFIL
515100001006     C                   MOVEL     'A'           KEPA
515200000830     C*
515300000830     C     KCAE          SETLL     AZCAE01L
515400000830     C     KCAE          READE     AZCAE01L                               83
515500001006    2C     *IN83         DOWEQ     *OFF
515600001006    3C     CAEATB        IFEQ      ' '
515700000830     C     WDATFV        ANDGE     CAEDDE
515800000830     C     WDATFV        ANDLE     CAEDSC
515900000830     C*  PER SICUREZZA CONTROLLO CHE IL TERMINAL SIA <> DA ORG.
516000001006    4C     CAETFE        IFNE      WTE1
516100000830     C                   MOVEL     '1'           W001$
516200001006    4C                   ENDIF
516300000830     C                   SETON                                        83
516400001006    3C                   ENDIF
516500000830     C  N83KCAE          READE     AZCAE01L                               83
516600001006    2C                   ENDDO
516700000830     C*
516800000830     C* SE W001$ E' VUOTO AGGIUNGO FLA,L NELLE LNA DEL FOGLIO VIAGGIO
516900001006    2C     W001$         IFEQ      ' '
517000001006     C                   MOVEL     FLA(L)        WTER              3 0
517100000830     C                   EXSR      AGGSF3
517200000830     C                   MOVE      '1'           WTAR
517300001006    2C                   ENDIF
517400001006     C*
517500001006  X1AC                   ELSE
517600001006     C                   MOVEL     'S'           WTROV             1
517700001006   1AC                   ENDIF
517800000830     C*
517900000830     C                   ADD       1             L
518000000830     C     WTE1          LOOKUP    TAA(L)                                 31
518100001006    1C                   ENDDO
518200000830     C*
518300000830     C* CONTROLLO SE ESISTONO ECCEZIONI CON TERMINAL ARRIVO = WTE1
518400000830     C                   MOVEL     WTE1          WFIL
518500000830     C                   MOVEL     'A'           KEPA
518600000830     C     KCAE          SETLL     AZCAE03L
518700000830     C     KCAE          READE     AZCAE03L                               83
518800001006    1C     *IN83         DOWEQ     *OFF
518900001006    2C     CAEATB        IFEQ      ' '
519000000830     C     WDATFV        ANDGE     CAEDDE
519100000830     C     WDATFV        ANDLE     CAEDSC
519200000830     C*
519300001006     C                   MOVEL     CAETFA        W003A
519400000830     C* AGGIUNGO CAETFA NELLE LNA DEL FOGLIO VIAGGIO
519500001006    3C     W003A         IFNE      VIDLNA
519600000830     C     CAETFA        LOOKUP    FLA                                    31
519700001006    4C     *IN31         IFEQ      *ON
519800000830     C                   MOVEL     CAETFA        WTER
519900000830     C                   EXSR      AGGSF3
520000000830     C                   MOVE      '1'           WTAR
520100001006    4C                   ENDIF
520200001006   X3C                   ELSE
520300001006     C                   MOVEL     'S'           WTROV             1
520400001006    3C                   ENDIF
520500001006    2C                   ENDIF
520600000830     C*
520700000830     C     KCAE          READE     AZCAE03L                               83
520800001006    1C                   ENDDO
520900000830     C*
521000000830     C     WTAR          IFEQ      ' '
521100001006     C     WTROV         ANDEQ     ' '
521200000830     C                   MOVEL     WTE1          WTER
521300000830     C                   EXSR      AGGSF3
521400000830     C                   ENDIF
521500000830      *
521600000829     C                   ENDSR
521700000830     C*----------------------------------------------------------------
521800000830     C* AGGIORNAMENTO SUBFILE LNA DA INSERIMENTO TERMINAL
521900000830     C*----------------------------------------------------------------
522000000830     C     AGGSF3        BEGSR
522100000830      *
522200000830     C     1             DO        *HIVAL        LL                4 0
522300130315     C     LL            CHAIN     lst4S03                            22
522400000830     C   22              LEAVE
522500000830     C     VIDFFV        IFEQ      *BLANKS
522600000830     C     VIDFLP        ANDEQ     *BLANKS
522700000830     C                   CLEAR                   TIPFLP
522800000830     C                   CLEAR                   TIPFFV
522900000830     C                   MOVEL(P)  '<-- Ins.'    DESFFV
523000000830     C                   MOVEL(P)  '<-- Ins.'    DESFLP
523100000830     C                   MOVEL     WTER          VIDFFV
523200000830     C                   MOVEL     WTE2          VIDFLP
523300130315     C                   UPDATE    lst4S03
523400000830     C     WPOSIZ        IFEQ      ' '
523500000830     C                   MOVE      'P'           WPOSIZ
523600000830     C                   Z-ADD     NRR3          REC3
523700000830     C                   ENDIF
523800000830     C                   LEAVE
523900000830     C                   ENDIF
524000000830     C                   ENDDO
524100000830      *
524200000830     C                   ENDSR
524300960301     C*-----------------------------------------------------***********
524400960301     C* SCRIVO RECORD DI SBINAMENTO  IN FNFV4
524500960301     C*-----------------------------------------------------***********
524600960301     C     WRTSBI        BEGSR
524700960302     C                   MOVEL     'P'           KEPA
524800960302     C                   CLEAR                   KTFA
524900960302     C*
525000960302     C                   DO        WREC
525100960302     C                   CLEAR                   FNFV4000
525200960302     C                   MOVEL     SBI(I)        FV4NOT
525300970103     C     I             IFLT      15
525400110512     C     I             orge      20
525500960302     C                   ADD       1             I
525600970103     C                   ENDIF
525700960302     C                   EXSR      WTRFV4
525800970103     C* PREPARO ANCORA SBI PER 17 / 18 / 19
525900970103     C     *IN11         IFEQ      *ON
526000970103     C                   ADD       1             XX
526100970103     C                   EXSR      PREAGG
526200970103     C                   ENDIF
526300970103     C     *IN12         IFEQ      *ON
526400970103     C                   ADD       1             ZZ
526500970103     C                   EXSR      PREAGG
526600970103     C                   ENDIF
526700970103     C     *IN13         IFEQ      *ON
526800970103     C                   ADD       1             YY
526900970103     C                   EXSR      PREVAR
527000970103     C                   ENDIF
527100970103     C**
527200960302     C                   ENDDO
527300960301     C                   ENDSR
527400960126     C*-----------------------------------------------------***********
527500960126     C* TRASMISSIONE  DELLE VARIAZIONI APPORTATE AL FV
527600960126     C*-----------------------------------------------------***********
527700960126     C     TRAVAR        BEGSR
527800960126     C* SOLO SE IL FV E' CHIUSO
527900960119    2C  N80FGVFCF        IFNE      ' '
528000960119     C                   MOVEL     'M'           PA2FLG
528100960119     C                   MOVEL     VIDNFV        PA2FGV
528200080228     C* CARICO L1 --> FILIALI GESTITE
528300080228     C                   CLEAR                   TRUL06DS
528400080228     C                   MOVE      '£1'          D06COD
528500080228     C                   MOVEL     SIMFEL        D06KEY
528600080228     C                   MOVE      'S'           D06KEY
528700080228     C                   MOVEL     fgvdfv        D06drf
528800080228     C                   MOVEL     TRUL06DS      KPJBU
528900080228     C*
529000080228     C                   CALL      'TRUL06R'
529100080228     C                   PARM                    KPJBA
529200080228     C                   MOVEL     KPJBU         TRUL06DS
529300080228     C                   MOVEA     LIN           L1ela
529400960119     C                   MOVEL     PARAM2        KPJBU
529500000824     C*
529600000824     C                   CLEAR                   KQEBI
529700000824     C                   CLEAR                   KQIEB
529800130718     C                   MOVEL     'LSU2'        KCOAZ
529900960119     C                   CALL      'BCH10'
530000960119     C                   PARM                    KPJBA
530100960119     C*
530200960119     C                   MOVEL     '1'           WLSA4
530300960126     C                   CLEAR                   WAGG
530400960119    2C                   ENDIF
530500951120     C*
530600960126     C                   ENDSR
530700960122     C*-----------------------------------------------------***********
530800960122     C* CALCOLO DELLE DATE TEORICHE DI PARTENZE E ARRIVO
530900960122     C*-----------------------------------------------------***********
531000960122     C     DTTEOR        BEGSR
531100960122     C* SE TRAINO ABBINATO
531200960130    1C     VIDTRN        IFGT      0
531300020805     C                   CLEAR                   TNTL35DS
531400960122     C* DATA PARTENZA TEORICA
531500960122     C* IN MANUTENZIONE LA CALCOLO SOLO SE NON C'E'
531600960122     C                   MOVEL     'T'           KTDH
531700960129     C                   MOVEL     'P'           KEPA
531800960129     C                   MOVEL     'P'           D35EPA
531900960122     C                   MOVEL     'D'           D35TUP
532000960122     C                   MOVEL     VIDTRN        D35TRN
532100960122     C                   MOVEL     WDDE          D35DDE
532200960122     C                   MOVEL     WDTN          D35DTN
532300960122     C                   CLEAR                   WTFA
532400960122     C*
532500960122     C     KFV22         SETLL     FNFV2000                               31
532600960122    2C     *IN31         IFEQ      *OFF
532700960129     C* PASSO IL NUMERO FERMATA
532800960129     C     WTFM          IFEQ      'A'
532900960129     C                   MOVEL     NFP(1)        D35NFM
533000960129     C                   ELSE
533100960129     C                   MOVEL     NFP(2)        D35NFM
533200960129     C                   ENDIF
533300960122     C                   EXSR      CALT35
533400960122     C   90              GOTO      ENDDTT
533500960122    2C                   ENDIF
533600960122     C*  DATE TEORICHE ARRIVO
533700960122     C*  LETTURA DAI METRI CUBI
533800960122     C                   MOVEL     'A'           D35EPA
533900960129     C                   MOVEL     'A'           KEPA
534000960122     C                   CLEAR                   D35NFM
534100960122     C                   MOVEL     WTFM          D35TFM
534200000612     C                   Z-ADD     VIDFGS        WTFP
534300960122     C     KTLM1         SETLL     TNTLM01L
534400960122     C     KTLM1         READE     TNTLM01L                               83
534500960122     C*
534600960122    2C     *IN83         DOWEQ     *OFF
534700960122    3C     TLMTFA        IFNE      WTFA
534800960122     C     TLMATB        ANDEQ     ' '
534900960122     C                   MOVEL     TLMTFA        WTFA
535000960122     C     KFV22         SETLL     FNFV2000                               31
535100960122    4C     *IN31         IFEQ      *OFF
535200960122     C                   CLEAR                   D35DTE
535300960122     C                   CLEAR                   D35HTE
535400960122     C                   MOVEL     TLMTFA        D35FIL
535500960122     C                   EXSR      CALT35
535600960122     C   90              SETON                                        83
535700960122    4C                   ENDIF
535800960122    3C                   ENDIF
535900960122     C*
536000960122     C  N90KTLM1         READE     TNTLM01L                               83
536100960122    3C                   ENDDO
536200960122     C*
536300960122   X1C                   ELSE
536400960122     C* SE TRAINO NON ABBINATO, E SONO IN MANUTENZIONE
536500960122     C*  CANCELLO LE DATE TEORICHE
536600960122    2C     *IN80         IFEQ      *OFF
536700960122     C                   MOVEL     'T'           KTDH
536800960129     C                   MOVEL     'P'           KEPA
536900960122     C                   CLEAR                   WTFA
537000960122     C     KFV22         DELETE    FNFV2000                           83
537100960122     C* DATE ARRIVO
537200960129     C                   MOVEL     'A'           KEPA
537300960130     C     KFV24         SETLL     FNFV2003
537400960130     C     KFV24         READE     FNFV2003                               83
537500960122    3C     *IN83         DOWEQ     *OFF
537600960130     C                   DELETE    FNFV2003
537700960122     C*
537800960130     C     KFV24         READE     FNFV2003                               83
537900960122    3C                   ENDDO
538000960122     C*
538100960122    2C                   ENDIF
538200960122    1C                   ENDIF
538300960122     C     ENDDTT        ENDSR
538400960122     C*-----------------------------------------------------***********
538500960122     C* CALL PGM TNTL35R E WRITE IN FNFV2
538600960122     C*-----------------------------------------------------***********
538700960122     C     CALT35        BEGSR
538800020805     C                   MOVEL     TNTL35DS      KPJBU
538900960123     C                   CALL      'TNTL35R'
539000960122     C                   PARM                    KPJBA
539100020805     C                   MOVEL     KPJBU         TNTL35DS
539200960122     C* C'E' ERRORE
539300960122     C     D35ERR        IFNE      *BLANKS
539400960122     C                   MOVEL     D35MSG        VIDMSG
539500960122     C                   SETON                                        2890
539600960122     C                   ELSE
539700960123     C* WRITE IN FNFV200F
539800960122     C                   CLEAR                   FNFV2000
539900960122     C                   MOVEL     D35EPA        FV2EPA
540000960122     C     FV2EPA        IFEQ      'A'
540100960122     C                   MOVEL     D35FIL        FV2LAI
540200960122     C                   ENDIF
540300960122     C                   MOVEL     FGVLNP        FV2LNP
540400960122     C                   MOVEL     FGVNFV        FV2NFV
540500960122     C                   MOVEL     'T'           FV2TDH
540600960122     C                   MOVEL     D35DTE        FV2DPA
540700960122     C                   MOVEL     D35HTE        FV2HPA
540800960122     C                   MOVEL     'N'           FV2FTR
540900960122     C                   MOVEL     99999999      FV2DTR
541000960122     C                   WRITE     FNFV2000
541100960122     C                   ENDIF
541200960122     C                   ENDSR
541300951018     C*-----------------------------------------------------***********
541400951031     C* SCRIVO FILE FNFV400F                                * WTRFV4  *
541500951031     C*-----------------------------------------------------***********
541600951031     C     WTRFV4        BEGSR
541700960123     C* SE FV4NOT
541800960126     C                   CLEAR                   FV4ATB
541900951031     C                   Z-ADD     FGVLNP        FV4LNP
542000951031     C                   Z-ADD     FGVNFV        FV4NFV
542100960123     C                   MOVEL     KEPA          FV4EPA
542200960123     C                   MOVEL     KTRC          FV4TRC
542300060711     C***  WTRAS1        IFNE      'S'
542400960126     C                   MOVEL     'N'           FV4FT1
542500060711     C                   MOVEL     dateu         FV4DT1
542600060711     C***                ELSE
542700060711     C***                CLEAR                   FV4FT1
542800060711     C***                CLEAR                   FV4DT1
542900060711     C***                ENDIF
543000960126     C     WTRAS2        IFNE      'S'
543100960126     C                   MOVEL     'N'           FV4FT2
543200060711     C                   MOVEL     dateu         FV4DT2
543300960126     C                   ELSE
543400960126     C                   CLEAR                   FV4FT2
543500960126     C                   CLEAR                   FV4DT2
543600960126     C                   ENDIF
543700951031     C*
543800951031     C                   WRITE     FNFV4000
543900951031     C                   ENDSR
544000951031     C*-----------------------------------------------------***********
544100951018     C* COPIA CAMPI RECORD IN CAMPI FILE VIDEO              * RIEMPI  *
544200951018     C*-----------------------------------------------------***********
544300900515     C     RIEMPI        BEGSR
544400951130     C                   SETOFF                                       16
544500951018     C*
544600911105     C                   Z-ADD     1             J                 3 0
544700911108     C                   Z-ADD     1             Y                 3 0
544800911105     C                   MOVEA     *BLANKS       LAB
544900911108     C                   MOVEA     *BLANKS       LAS
545000940921     C*
545100951018     C* GIRO DATA FOGLIO VIAGGIO
545200951130     C                   MOVE      FGVDFV        WSDFV
545300960126     C                   MOVE      FGVDTP        SAVDTP
545400960126     C                   MOVE      FGVHMP        SAVHMP
545500951130     C*
545600951130     C                   MOVE      FGVDFV        G02INV
545700900515     C                   MOVE      *ZEROS        G02DAT
545800940909     C                   MOVEL     '3'           G02ERR
545900940909     C                   CALL      'XSRDA8'
546000900515     C                   PARM                    WLBDAT
546100900515     C                   Z-ADD     G02DAT        VIDDFV
546200960302     C                   MOVEL     G02DAT        GGMMAA
546300951024     C                   Z-ADD     FGVDFV        AMGDFV
546400951025     C                   Z-ADD     G02TGI        WGGDFV
546500030701     c                   ExSr      Sr_MinMax
546600040204     c                   clear                   wd02
546700951027     C                   EXSR      CALDAT
546800951018     C*
546900130419     C                   MOVE      FGVLNP        VIDfgs
547000130419     C                   MOVE      FGWlnp        CODFIL
547100000516     C                   EXSR      CTRFIL
547200000516     C  N34              MOVEL     ORGDES        DESFGS
547300000516     C   34              CLEAR                   DESFGS
547400130419     C**   FGWFGS        COMP      0                                  0505
547500130419     C** 05              MOVE      FGWFGS        VIDFGS
547600940913     C                   MOVE      FGVPDR        VIDPDR
547700130424     C                   MOVE      FGVPDR        savpdr
547800951025     C                   MOVE      FGVDPD        VIDDPD
547900010920      * se l'importo sul file dell'estensione è > 0 non vado sul file principale
548000010920     C     FGWIPT        IFGT      *ZEROS
548100010920     C                   Z-ADD     FGWIPT        VIDIPT
548200010920     C                   MOVEL     FGWVIP        VIDVIP
548300011015     C                   MOVEL     FGWVIP        SAVDIV
548400011204     C                   MOVEL     FGWVIP        SAWVIP
548500010920     C                   ENDIF
548600010920      *
548700900515     C                   MOVE      FGVLNA        VIDLNA
548800951026     C                   MOVE      FGVNFV        VIDNFV
548900960123     C                   MOVE      FGVSNP        VIDBIS
549000951018     C* SE ESISTONO BOLLE BORDERIZZATE NON POSSO TOGLIERE LA LINEA
549100911105     C                   MOVE      FGVLNA        COMLNA
549200960126     C                   MOVE      FGVLNA        CODFIL
549300960123     C**
549400130315     C     ilst4ges      IFNE      'N'
549500020805     C     KBLP1         SETLL     FNBLP000                               31
549600911105     C   31              MOVEL     FGVLNA        LAB(J)
549700911105     C   31              ADD       1             J
549800951025     C* SE ESISTONO SPUNTE  NON POSSO TOGLIERE LA LINEA
549900000202     C                   Z-ADD     VIDNFV        WNFV
550000070112     C     KFGV1         SETLL     FNBRV02L                               31
550100951025     C   31              MOVEL     FGVLNA        LAS(Y)
550200951026     C   31              ADD       1             Y
550300960123     C                   ENDIF
550400960123     C**
550500951027     C* DECODIFICO LA LINEA FINALE DI ARRIVO
550600061116     c                   clear                   okestero
550700960126     C                   EXSR      CTRFIL
550800960126     C  N34              MOVEL     ORGDES        DESLNA
550900061116     c                   eval      okestero = esteroedpd
551000061116     c
551100960226     C                   MOVEL     'A'           KEPA
551200960226     C                   MOVEL     FGVLNA        KTFA
551300970102     C* CERCO NELLE ECCEZIONI SE LA LNA HA ALTRI TERMINAL DI ARRIVO
551400130419     C*  CHE POSSONO CORRISPONDERE A VIDfgs
551500970102     C                   EXSR      RICEC2
551600971203     C                   MOVEA     TFE           TFEL
551700951018     C*
551800971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
551900971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
552000020805     C                   CLEAR                   FNLV55DS
552100971203     C                   MOVEL     'A'           D55TPT
552200971203     C                   MOVE      FGVLNA        D55LIN
552300000602     C                   MOVE      VIDFGS        D55LNP
552400971203     C                   MOVE      AMGDFV        D55DRF
552500971203     C                   CALL      'FNLV55R'
552600020805     C                   PARM                    FNLV55DS
552700971203     C                   MOVE      D55TFA        LNAFE0
552800970102     C*
552900951018     C* GIRO DATA PARTENZA EFFETTIVA
553000960123    1C     FGVDTP        IFGT      0
553100900515     C                   MOVE      FGVDTP        G02INV
553200900515     C                   MOVE      *ZEROS        G02DAT
553300940909     C                   MOVEL     '3'           G02ERR
553400940909     C                   CALL      'XSRDA8'
553500900515     C                   PARM                    WLBDAT
553600900515     C                   Z-ADD     G02DAT        VIDDTP
553700900515     C                   Z-ADD     FGVHMP        VIDHMP
553800960129     C                   MOVEL     '1'           WFORZC
553900951018     C*
554000951106     C* CAUSALE RITARDO/ANTICIPO PARTENZA FOGLIO
554100951106     C* SE ESISTE GIA' LA CARICO A VIDEO
554200960122     C                   MOVEL     'R'           KTDH
554300960123     C                   MOVEL     'P'           KEPA
554400960123     C     KFV2          CHAIN(N)  FNFV201L                           83
554500951103    2C     *IN83         IFEQ      *OFF
554600951103     C     FV2ATB        ANDEQ     ' '
554700951103     C                   MOVEL     FV2RTC        VIDRTC
554800951106     C                   MOVEL     FV2RTC        SAVRTC
554900951103     C* DECODIFICA
555000951103     C                   MOVEL     'TC'          COD
555100951103     C                   MOVEL(P)  VIDRTC        KEY
555200951103     C     KTAB          CHAIN     TABEL                              83
555300951103    3C     *IN83         IFEQ      *OFF
555400951103     C     TBLFLG        ANDEQ     ' '
555500960208     C                   MOVEL     TBLUNI        DSTC
555600960208     C                   MOVEL     §TCDES        DESRT1
555700960208     C                   MOVE      §TCDES        DESRT2
555800951103    3C                   ENDIF
555900951103    2C                   ENDIF
556000951108     C*
556100951108     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
556200951108     C*   EVIDENZIO IL COMANDO
556300960123     C                   MOVEL     'P'           KEPA
556400960123     C                   MOVEL     'R'           KTRC
556500960123     C                   CLEAR                   KTFA
556600951108     C                   EXSR      CTRDRA
556700951108    2C     W001A         IFNE      ' '
556800951108     C                   SETON                                        16
556900951108    2C                   ENDIF
557000951103    1C                   ENDIF
557100951103     C*
557200000602     C* TIPO AUTOMEZZO + DECODIFICA
557300900606     C                   MOVEL     FGVTMZ        VIDTMZ
557400951025     C                   EXSR      CTRTMZ
557500951025     C*
557600130430     C***                MOVEL     FGVDCN        VIDDCN
557700130430     C***                MOVEL     FGVDCN        savdcn
557800130430     C                   MOVEL     FGVPMB        VIDDCN
557900130430     C                   MOVEL     FGVPMB        savdcn
558000010621      *
558100010621      *REPERISCO IL NUMERO DEL CELLULARE
558200130419     C                   MOVEL     VIDfgs        FGVLNP
558300010621     C                   Z-ADD     VIDNFV        FGVNFV
558400010622     C                   MOVE      'P'           KEPA
558500010622     C                   Z-ADD     0             WTFA
558600010622     C                   MOVE      'L'           KTDH
558700040517     C     KFV22         CHAIN(N)  FNFV401L                           30
558800010622     C  N30              MOVEL     FV4NOT        VIDCNC
558900010621      *
559000950913     C                   MOVEL     FGVTTR        VIDTTR
559100951027     C                   MOVEL     'TV'          COD
559200951027     C                   MOVEL(P)  VIDTTR        KEY
559300951027     C     KTAB          CHAIN     TABEL                              83
559400951027     C  N83              MOVEL     TBLUNI        DESTTR
559500951027     C  N83              MOVEL     TBLUNI        DSTV
559600951027     C   83              CLEAR                   DSTV
559700951027     C*
559800951027     C                   MOVEL     FGVTRM        VIDTRM
559900900515     C                   MOVEL     FGVTRR        VIDTRR
560000951018     C*
560100130424     C* reperisco pimobi da fnfv4
560200130424     C                   MOVEL     'P'           KTRC
560300130424     C                   CLEAR                   KTFA
560400130424     C                   MOVEL     'P'           KEPA
560500130424     C     KFV4          SETLL     FNFV401L
560600130424     C     KFV4          READE(N)  FNFV401L                               83
560700130424     C     *IN83         IFEQ      *OFF
560800130424     C     FV4ATB        ANDEQ     ' '
560900130424     C                   MOVEL     FV4NOT        VIDpmb
561000130424     C     KFV4          READE(N)  FNFV401L                               83
561100130424     C  N83FV4ATB        IFEQ      ' '
561200130424     C                   eval      %subst(vidpmb:36)=fv4not
561300130424     C                   ENDIF
561400130424     C                   ENDIF
561500130424     c
561600900606     C                   MOVEL     FGVNT1        VIDNT1
561700900606     C                   MOVEL     FGVNT2        VIDNT2
561800900628     C                   MOVEL     FGVSNS        VIDSNS
561900960123     C                   MOVEL     FGVFRQ        WTFM
562000960123     C                   Z-ADD     FGVTRN        VIDTRN
562100951025     C* CARICO LINEE NEL SFL
562200000427     C                   MOVEL     FGWFF3        W1FF3
562300000427     C                   MOVEL     FGWFF4        W1FF4
562400000427     C                   MOVEL     FGWFL3        W1FL3
562500000427     C                   MOVEL     FGWFL4        W1FL4
562600951025     C                   EXSR      CARSFL
562700110512     c
562800110512     c* Carico punto di scambio
562900110512     C     K02FGv01      chain(N)  FNFGs000
563000110512if  1c                   if        (not %found(FNFGs01L)
563100110512     c                              or  FGsatb <> *blanks)
563200110512     C                   clear                   FNFGs000
563300110512e   1C                   endif
563400110530     c
563500110530    1c                   if        fgspsc<>*blanks
563600110530     c                   seton                                        08
563700110512     c                   eval      vidpsc=fgspsc
563800110531     c                   eval      OLDpsc=fgspsc
563900110513     c     vidpsc        chain     tnpsc01l
564000110530    2c                   if        %found(tnpsc01l)
564100110513     c                   eval      despsc=pscloc
564200110530     c                   eval      despind=pscind
564300110530     c                   eval      desploc=%trim(psccap) + ' ' +
564400110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
564500110530     c
564600110530   x2c                   else
564700110513     c                   eval      despsc=*all'?'
564800110530     c                   eval      despind=*all'?'
564900110530    2c                   endif
565000110513     c
565100110530    2c                   if        fgstrns>0
565200110530     c                   eval      vidtrns=fgstrns
565300110513     c                   exsr      Ctrtrns
565400110530    2c                   endif
565500110513
565600110512     c                   eval      vidhhs=fgshhs
565700110512     C                   MOVE      FGsdgs        G02INV
565800110512     C                   MOVE      *ZEROS        G02DAT
565900110512     C                   MOVEL     '3'           G02ERR
566000110512     C                   CALL      'XSRDA8'
566100110512     C                   PARM                    WLBDAT
566200110512     C                   Z-ADD     G02DAT        VIDdgs
566300110530     c
566400110530     c* linea finale arrivo autista
566500110530    2c                   if        fgslnar>0
566600110530     c                   move      fgslnar       vidlnar
566700110530     c                   move      fgslnar       codfil
566800110530     c                   exsr      ctrfil
566900110530     c                   movel     orgdes        deslnar
567000110601     c                   movel     orgdes        deslnar2
567100110530    2c                   endif
567200110530    1c                   endif
567300110512      *
567400951025     C*
567500951025     C* FV ABBINATO --> CERCO TRAINO
567600951018    1C     FGVTRN        IFGT      0
567700960123     C*
567800130315   1AC     ilst4ges      IFNE      'N'
567900951018     C* TIPO FERMATA TRAINO: "A-ANDATA" / "R-RITORNO"
568000951025     C* CERCO DECORRENZA TRAINO CORRETTA
568100951025     C                   EXSR      CTRDEC
568200951025     C* TRAINO NON TROVATO
568300951025    2C     WAN           IFEQ      ' '
568400951025     C     WRI           ANDEQ     ' '
568500951025     C                   MOVEL     MSG(34)       VIDMSG
568600951025     C                   MOVEA     MSG(34)       K78
568700951025     C                   MOVEL     FGVTRN        W007A             7
568800951025     C                   MOVEA     W007A         K15(9)
568900951026     C                   Z-ADD     9             Z
569000951025     C                   EXSR      ABBL
569100951025     C                   MOVEA     K15(9)        K78(23)
569200951025     C                   MOVEA     K78           VIDMSG
569300951025     C                   CLEAR                   VIDTRN
569400951027     C                   SETON                                        289048
569500960123   X2C                   ELSE
569600951025     C*
569700951025     C* SALVO I CAMPI PER CONTROLLARE SE VIENE MODIFICATO QUALCOSA
569800951025     C                   MOVEL     VIDLNA        SAVLNA
569900951025     C                   MOVEL     VIDPDR        TRNPDR
570000951025     C                   MOVEL     VIDTMZ        TRNTMZ
570100010920     C                   MOVEL     VIDVIP        TRNVIP
570200010920     C                   Z-ADD     VIDIPT        TRNIPT
570300951025     C                   MOVEL     VIDLNA        TRNTFA
570400951027     C                   MOVEL     VIDTTR        SAVTTR
570500960130     C                   MOVEL     VIDBIS        SAVBIS
570600951027     C                   MOVEL     §TVUTF        TRNUTF
570700110512     C                   MOVEL     VIDpsc        SAVpsc
570800110530     C                   MOVEL     VIDlnar       SAVlnar
570900951130     C* SALVO LE LINEE
571000951130     C                   MOVEA     FFV           TFV
571100951130     C                   MOVEA     FLP           TLP
571200951025     C*
571300951025     C* MEMORIZZO IL TIPO FERMATA DA ELABORARE
571400951025    3C     WAN           IFEQ      'S'
571500951025     C                   MOVEL     'A'           WTFM
571600960122     C                   MOVEL     NFP(1)        WNFM
571700951025     C                   Z-ADD     WANDDE        WDDE
571800951123     C                   MOVEL     'A'           WFMFES
571900960122   X3C                   ELSE
572000951123     C* SE E' UN RITORNO VERIFICO SE C'ERA ANCHE ANDATA
572100951123     C*  PER VEDERE CHE DEVE ESPORRE L'IMPORTO DEL FV
572200951123     C                   MOVEL     'A'           WTFM
572300130419     C***  FGWFGS        IFGT      *ZEROS
572400130419     C***                Z-ADD     FGWFGS        WFIL
572500130419     C***                ELSE
572600130419     C***                Z-ADD     FGVLNP        WFIL
572700130419     C***                ENDIF
572800130419     c
572900130419     C                   Z-ADD     FGVLNP        WFIL
573000951123     C     KTLR3         SETLL     TNTLR03L                               83
573100960122    4C     *IN83         IFEQ      *ON
573200951123     C                   MOVEL     'A'           WFMFES
573300951123     C                   ELSE
573400951123     C                   MOVEL     'R'           WFMFES
573500960122    4C                   ENDIF
573600951123     C**
573700960122     C                   Z-ADD     WRIDDE        WDDE
573800960122     C                   MOVEL     NFP(2)        WNFM
573900951025     C                   MOVEL     'R'           WTFM
574000951025    3C                   ENDIF
574100951025     C* SALVO IL NUMERO TRAINO IMMESSO
574200951025     C                   MOVEL     COPTRN        SAVTRN
574300960123    2C                   ENDIF
574400960123   1AC                   ENDIF
574500150907     c*
574600150907     C* CHAINO LA TESTATA  con la giusta decorrenza
574700150907     C     KTLR11        CHAIN     TNTLT01L                           83
574800960122     C**
574900960122     C* CARICO LA DATA PARTENZA TEORICA
575000960123    2C     VIDTRN        IFGT      0
575100960122     C                   MOVEL     'T'           KTDH
575200960123     C                   MOVEL     'P'           KEPA
575300960123     C     KFV2          CHAIN(N)  FNFV201L                           83
575400960126     C* SE MANCA PROVO A SCRIVERLO
575500960126     C   83              EXSR      DTTEOR
575600960126     C*
575700960126     C     KFV2          CHAIN(N)  FNFV201L                           83
575800960123    3C     *IN83         IFEQ      *OFF
575900960122     C     FV2ATB        ANDEQ     ' '
576000960122     C                   MOVE      FV2DPA        G02INV
576100960122     C                   MOVE      *ZEROS        G02DAT
576200960122     C                   MOVEL     '3'           G02ERR
576300960122     C                   CALL      'XSRDA8'
576400960122     C                   PARM                    WLBDAT
576500960122     C                   Z-ADD     G02DAT        VIDDTT
576600960122     C                   Z-ADD     FV2DPA        AMGDTT
576700960122     C                   MOVEL     FV2HPA        VIDHMT
576800960123    3C                   ENDIF
576900960122     C* SE SONO IN CHIUSURA FV IMPOSTO LA DATA TEORICA NELLA DATA REALE
577000130315    3C     ilst4ges      IFEQ      'I'
577100130315     C     ilst4ges      OREQ      'C'
577200960122     C                   Z-ADD     VIDHMT        VIDHMP
577300960122     C                   Z-ADD     VIDDTT        VIDDTP
577400960123    3C                   ENDIF
577500951019     C*
577600960123    2C                   ENDIF
577700951018    1C                   ENDIF
577800950901     C*
577900960122     C* CHAIN PER PRENDERE IN FNFV400F LE ANNOTAZIONI
578000960123     C                   MOVEL     'N'           KTRC
578100960123     C                   CLEAR                   KTFA
578200960123     C                   MOVEL     'P'           KEPA
578300951031     C     KFV4          SETLL     FNFV401L
578400960124     C     KFV4          READE(N)  FNFV401L                               83
578500951031     C     *IN83         IFEQ      *OFF
578600960126     C     FV4ATB        ANDEQ     ' '
578700951031     C                   MOVEL     FV4NOT        VIDAN1
578800960124     C     KFV4          READE(N)  FNFV401L                               83
578900960126     C  N83FV4ATB        IFEQ      ' '
579000960126     C                   MOVEL     FV4NOT        VIDAN2
579100951031     C                   ENDIF
579200960126     C                   ENDIF
579300960123     C* VERIFICO SE C'E' ALMENO UNA DATA DI ARRIVO
579400960129     C                   MOVEL     'A'           KEPA
579500960123     C     KFV23         SETLL     FNFV2000                               15
579600990514      *
579700990514      * 03 ON --> DISABILITO F18-RIAPERTURA
579800961213     C                   SETOFF                                       03
579900990514     C                   MOVEL     FGVLNA        CODFIL
580000990514     C                   EXSR      CTRFIL
580100060113     c                   if        esteroedpd = 'S' and §tvdef = 'S'
580200961213     C                   SETON                                        03
580300990514     C                   ENDIF
580400130516     c
580500130516     c* posizinamento cursore
580600130516     c                   if        viddcn=*blanks and vidpdr>*zeros
580700130516     c                   seton                                        55
580800130516     c                   endif
580900130516     c                   if        vidtrm=*blanks and vidpdr>*zeros and
581000130516     c                             viddcn<>*blanks
581100130516     c                   seton                                        54
581200130516     c                   endif
581300990514      *
581400900515     C                   ENDSR
581500951025     C*-----------------------------------------------------***********
581600951025     C****  R I E M P I O   I L   S U B F I L E  ****         CARSFL
581700951025     C*-----------------------------------------------------***********
581800951025     C     CARSFL        BEGSR
581900951025     C                   Z-ADD     1             NRR3
582000951026     C                   Z-ADD     1             REC3
582100951025     C                   SETON                                        70
582200130315     C                   WRITE     lst4C03
582300951025     C                   SETOFF                                       70
582400130606     c* In manutenzione memorizzo le linee del foglio e relativi scarichi
582500130606     c* per avere immagine precedente le modifiche
582600130606     c                   if        not *in80
582700130606     c                   eval      pffv(1)=%editc(fgvlna:'X')
582800130606     c                   eval      pflp(1)=%editc(fgvlna:'X')
582900130606     c                   movea     ffv           pffv(2)
583000130606     c                   movea     flp           pflp(2)
583100130606     c                   endif
583200951025     C*
583300951025     C                   Z-ADD     1             I
583400000420    1C                   DO        299           I
583500951025     C*
583600951025     C* FILIALI DI ARRIVO
583700951025     C     FFV(I)        IFGT      *ZEROS
583800951025     C                   MOVE      FFV(I)        VIDFFV
583900951025     C                   MOVE      VIDFFV        CODFIL
584000960126     C                   EXSR      CTRFIL
584100960126    3C     *IN34         IFEQ      *OFF
584200951025     C                   MOVEL     ORGDES        DESFFV
584300951025     C                   MOVEL     ORGFAG        TIPFFV
584400951025    3C                   ENDIF
584500951025     C*
584600951025     C* SE ESISTONO BOLLE BORDERIZZATE NON POSSO TOGLIERE LA LINEA
584700951025     C* SONO IN VARIAZIONE
584800951025     C     *IN80         IFEQ      *OFF
584900951025     C                   MOVE      FFV(I)        COMLNA
585000020805     ***C     KBLP1         SETLL     FNBLP29L                               31
585100020805     C     KBLP1         SETLL     FNBLP000                               31
585200951025     C   31              MOVEL     VIDFFV        LAB(J)
585300951025     C   31              ADD       1             J
585400951025     C* SE ESISTONO SPUNTE NON POSSO TOGLIERE LA LINEA
585500000202     C                   Z-ADD     VIDNFV        WNFV
585600070112     C     KFGV1         SETLL     FNBRV02L                               31
585700951025     C   31              MOVEL     VIDFFV        LAS(Y)
585800951026     C   31              ADD       1             Y
585900951025     C                   ENDIF
586000951025     C*
586100951025   X2C                   ELSE
586200951025     C                   MOVE      *BLANKS       VIDFFV
586300951025     C                   MOVE      *BLANKS       DESFFV
586400951025     C                   MOVE      *BLANKS       TIPFFV
586500951025    2C                   ENDIF
586600951025     C* FILIALI DI SCARICO
586700951025     C     FLP(I)        IFGT      *ZEROS
586800951025     C                   MOVE      FLP(I)        VIDFLP
586900951025     C                   MOVE      FLP(I)        SAVFLP
587000951025     C                   MOVE      VIDFLP        CODFIL
587100960126     C                   EXSR      CTRFIL
587200960126    3C     *IN34         IFEQ      *OFF
587300951025     C                   MOVEL     ORGDES        DESFLP
587400951025     C                   MOVEL     ORGFAG        TIPFLP
587500951025    3C                   ENDIF
587600951025     C*
587700951025   X2C                   ELSE
587800951025     C                   MOVE      *BLANKS       VIDFLP
587900951025     C                   MOVE      *BLANKS       SAVFLP
588000951025     C                   MOVE      *BLANKS       DESFLP
588100951025     C                   MOVE      *BLANKS       TIPFLP
588200951025    2C                   ENDIF
588300951025     C*
588400000829     C                   SETOFF                                       23
588500130315     C                   WRITE     lst4S03
588600951025     C                   ADD       1             NRR3
588700951025    1C                   ENDDO
588800951025     C                   ENDSR
588900951013     C*-----------------------------------------------------***********
589000951013     C* COPIA CAMPI SUBFILE DA FILE TRAINI                  * RIETRS  *
589100951013     C*-----------------------------------------------------***********
589200951013     C     RIETRS        BEGSR
589300971203     C*****
589400971203     C* CARICO TUTTE LE FILIALI DI AZORG00F CON IL RELATIVO TERMINAL
589500971203     C* DI ARRIVO
589600971203     C*****
589700971203     C                   Z-ADD     1             I
589800971203     C     *LOVAL        SETLL     AZORG
589900971203     C                   READ      AZORG                                  83
590000971203     C*
590100971203    1C     *IN83         DOWEQ     *OFF
590200971203    2C     ORGFVA        IFEQ      ' '
590300971203    3C     ORGFAG        IFEQ      'F'
590400971203     C     ORGFAG        OREQ      'A'
590500971203     C* CARICO SOLO SE NELLA DESCRIZIONE 148 DELL'ORGANIGRAMMA
590600971203     C*  C'E' SCRITTO CHE LA POSSO CARICARE
590700971203     C                   MOVEL     ORGDE8        OG148
590800971203     C     §OGLFV        IFNE      'N'
590900971203     C                   Z-ADD     ORGFIL        FLA(I)
591000020805     C                   CLEAR                   FNLV55DS
591100971203     C                   MOVEL     'A'           D55TPT
591200971203     C                   MOVE      ORGFIL        D55LIN
591300971203     C                   MOVE      WDTN          D55DRF
591400971203     C                   CALL      'FNLV55R'
591500020805     C                   PARM                    FNLV55DS
591600971203     C                   Z-ADD     D55TFA        TAA(I)
591700971203    3C                   ENDIF
591800971203     C                   ADD       1             I
591900971203    3C                   ENDIF
592000971203    2C                   ENDIF
592100971203     C*
592200971203     C                   READ      AZORG                                  83
592300971203    1C                   ENDDO
592400971203     C*
592500960202     C* VERIFICO SE STO CARCANDO ANDATA O RITORNO
592600960202     C* MI SERVE PER FVERIFICARE QUANDO ARRIVO ALLA 1 FERMATA
592700960202     C     WTFM          IFEQ      'A'
592800960202     C                   Z-ADD     1             I
592900960202     C                   ELSE
593000960202     C                   Z-ADD     2             I
593100960202     C                   ENDIF
593200960202     C*
593300951026     C                   CLEAR                   FFV
593400951026     C                   CLEAR                   FLP
593500951026     C                   Z-ADD     1             Z                 3 0
593600951025     C                   MOVEL     VIDLNA        WLNAF
593700000602     C     VIDFGS        IFGT      *ZEROS
593800000602     C                   MOVEL     VIDFGS        WTFP
593900000602     C                   ELSE
594000000602     C                   MOVEL     SIMFEL        WTFP
594100000602     C                   ENDIF
594200951025     C     KTLR1         SETGT     TNTLR01L
594300951025     C     KTLR11        READPE    TNTLR01L                               84
594400951009     C*
594500951012    1C     *IN84         DOWEQ     *OFF
594600951012    2C     TLRATB        IFEQ      ' '
594700951025     C* SE HO RAGGIUNTO LA FILIALE SIMFEL --> FINE LETTURA
594800951025     C*  SE MODIFICATO TIPO FERMATA       --> FINE LETTURA
594900960202     C* ESEGUO QUESTO CONTROLLO UTILIZZANDO IL NUMERO FERMATA
595000960202   2AC     TLRNFM        IFGT      NFP(I)
595100960202     C*
595200951025     C* SE NON E' FILIALE DI SCARICO NE LEGGO UN'ALTRA
595300951025   2BC     TLRSCA        IFEQ      'S'
595400951010     C                   MOVEL     TLRFIL        WTFA
595500951025     C* LEGGO I MC
595600951025     C     KTLM          SETLL     TNTLM01L
595700951025     C     KTLM          READE     TNTLM01L                               83
595800950831     C*
595900951012    3C     *IN83         DOWEQ     *OFF
596000951012    4C     TLMATB        IFEQ      ' '
596100951010     C*
596200960202     C* TIPO INOLTRO NON E' 'I': CARICO TUTTE LE FILIALI
596300960202     C*
596400960202   4AC     VIDTTR        IFNE      'I'
596500960202     C**
596600960823     C                   MOVE      ' '           WTAR              1
596700951026     C                   Z-ADD     1             I
596800951027     C     TLMTFF        LOOKUP    TAA(I)                                 31
596900951027    5C     *IN31         DOWEQ     *ON
597000951012     C*
597100951012     C* CONTROLLO SE ESISTE ECCEZIONE
597200951012     C                   CLEAR                   W001
597300951026     C                   MOVEL     FLA(I)        WFIL
597400950831     C*
597500960123     C                   MOVEL     'A'           KEPA
597600951012     C     KCAE          SETLL     AZCAE01L
597700951012     C     KCAE          READE     AZCAE01L                               85
597800951012    6C     *IN85         DOWEQ     *OFF
597900951012    7C     CAEATB        IFEQ      ' '
598000951012     C     WDTN          ANDGE     CAEDDE
598100951012     C     WDTN          ANDLE     CAEDSC
598200951025     C*  PER SICUREZZA CONTROLLO CHE IL TERMINAL TROVATO SIA <> DA ORG.
598300951025     C     CAETFE        IFNE      TLMTFF
598400950831     C                   MOVEL     '1'           W001
598500951025     C                   ENDIF
598600951012     C                   SETON                                        85
598700951012    7C                   ENDIF
598800951012     C  N85KCAE          READE     AZCAE01L                               85
598900951012    6C                   ENDDO
599000950831     C*
599100950831     C* SE W001 E' VUOTO SCRIVO FLA,C NELLE LNA DEL FOGLIO VIAGGIO
599200951012    6C     W001          IFEQ      ' '
599300951026     C     WLNAF         ANDNE     FLA(I)
599400951026     C                   MOVEL     FLA(I)        FFV(Z)
599500950905     C                   MOVEL     TLMTFA        FLP(Z)
599600951012     C                   ADD       1             Z
599700960823     C                   MOVE      '1'           WTAR
599800951012    6C                   ENDIF
599900950831     C*
600000951026     C                   ADD       1             I
600100951027     C     TLMTFF        LOOKUP    TAA(I)                                 31
600200951012    5C                   ENDDO
600300951012     C*
600400951012     C* CONTROLLO SE ESISTONO ECCEZIONI CON TERMINAL ARRIVO = TLMTFF
600500951025     C                   MOVEL     TLMTFF        WFIL
600600960123     C                   MOVEL     'A'           KEPA
600700951012     C     KCAE          SETLL     AZCAE03L
600800951012     C     KCAE          READE     AZCAE03L                               85
600900951012    5C     *IN85         DOWEQ     *OFF
601000950831    6C     CAEATB        IFEQ      ' '
601100951012     C     WDTN          ANDGE     CAEDDE
601200951012     C     WDTN          ANDLE     CAEDSC
601300950831     C*
601400950831     C* SCRIVO CAETFA NELLE LNA DEL FOGLIO VIAGGIO
601500960202     C     CAETFA        ANDNE     WLNAF
601600960208     C     CAETFA        LOOKUP    FLA                                    31
601700960208     C* Carico l'eccezione come filiale del foglio viaggio solo se
601800960208     C* presente in schiera filiali di azorg
601900960208     C     *IN31         IFEQ      *ON
602000950901     C                   MOVEL     CAETFA        FFV(Z)
602100951012     C                   MOVEL     TLMTFA        FLP(Z)
602200950901     C                   ADD       1             Z
602300960823     C                   MOVE      '1'           WTAR
602400960208    3C                   ENDIF
602500960208     C*
602600950831    6C                   ENDIF
602700960208     C*
602800951012     C     KCAE          READE     AZCAE03L                               85
602900950831    5C                   ENDDO
603000950831     C*
603100960202     C*
603200960823   4AC                   ENDIF
603300960202     C*
603400960202     C* TIPO INOLTRO E' "I"  --> CARICO SOLO LA LINEA DI ARRIVO FINALE
603500960823     C* IDEM SE NON E' INOLTRO MA E' SCARICO DIRETTO A FIL. NO TER-ARR
603600960823   4AC     TLTTTR        IFEQ      'I'
603700960823     C     WTAR          OREQ      ' '
603800960202    5C     WLNAF         IFNE      TLMTFF
603900960202     C                   MOVEL     TLMTFF        FFV(Z)
604000960202     C                   MOVEL     TLMTFA        FLP(Z)
604100960202     C                   ADD       1             Z
604200960202    5C                   ENDIF
604300960202   4AC                   ENDIF
604400960202    4C                   ENDIF
604500960202     C**
604600951025     C     KTLM          READE     TNTLM01L                               83
604700950831    3C                   ENDDO
604800951025   2BC                   ENDIF
604900951025     C*
605000951025  X2AC                   ELSE
605100951025     C                   SETON                                        84
605200951025   2AC                   ENDIF
605300950831    2C                   ENDIF
605400950831     C*
605500951025     C  N84KTLR11        READPE    TNTLR01L                               84
605600950831    1C                   ENDDO
605700950831     C*
605800951013     C****  S C R I V O   I L   S U B F I L E  ****
605900951025     C                   EXSR      CARSFL
606000951130     C* SALVO LE LINEE
606100951130     C                   MOVEA     FFV           TFV
606200951130     C                   MOVEA     FLP           TLP
606300950901     C*
606400950830     C                   ENDSR
606500920428      *-----------------------------------------------------***********
606600951026      * SFLAGGO  LE SPUNTE GIA' FLEGGATE SE MODIFICATA FIL DI SCARICO
606700920428      *-----------------------------------------------------***********
606800920428     C     LIBSPU        BEGSR
606900920428      *
607000000202     C                   Z-ADD     VIDNFV        WNFV
607100070112     C     KFGV1         SETLL     FNBRV02L
607200070112     C     KFGV1         READE     FNBRV02L                               31
607300980305     C     *IN31         DOWEQ     '0'
607400980305     C     WMOD          IFEQ      *BLANKS
607500980305     C     BRVFTR        ANDNE     ' '
607600980305     C     WMOD          OREQ      'A'
607700980305     C     BRVFTR        ANDEQ     'L'
607800940912     C                   MOVEL     ' '           BRVFTR
607900940912     C                   MOVEL     *ZEROS        BRVDTR
608000000202     C                   MOVEL     *ZEROS        BRVHTR
608100980305     C     WMOD          IFEQ      'A'
608200980305     C     BRVCAN        ANDEQ     'E'
608300980305     C                   CLEAR                   BRVCAN
608400980305     C                   END
608500070112     C                   UPDATE    FNBRV000
608600951025     C                   ENDIF
608700951025     C*
608800070112     C     KFGV1         READE     FNBRV02L                               31
608900951025     C                   ENDDO
609000920428     C*
609100920428     C                   ENDSR
609200940913      *-----------------------------------------------------***********
609300940913     C*    SCRIVO FILE DI APPOGGIO FNFGV46L                 * WRTFVA  *
609400940913      *-----------------------------------------------------***********
609500931116     C     WRTFVA        BEGSR
609600940913     C*
609700940913     C* VERIFICO SE IL RECORD E' GIA' PRESENTE NEL FILE DI APPOGGIO
609800130419     C                   MOVE      VIDfgs        CODFIL
609900951024     C     KFGV46        CHAIN     FNFGV46L                           82
610000931116     C*
610100940912     C** RIEMPO CAMPI DEL FILE FNFGV46L
610200940913     C                   MOVEL     FGVATB        FG4ATB
610300940913     C                   MOVEL     FGVNFV        FG4NFV
610400940913     C                   MOVEL     FGVDFV        FG4DFV
610500940913     C                   MOVEL     FGVLNP        FG4LNP
610600940913     C                   MOVEL     FGVPDR        FG4PDR
610700940913     C                   MOVEL     FGVDPD        FG4DPD
610800940913     C                   MOVEL     FGVCCN        FG4CCN
610900940913     C                   MOVEL     FGVDCN        FG4DCN
611000940913     C                   MOVEL     FGVTRM        FG4TRM
611100940913     C                   MOVEL     FGVTRR        FG4TRR
611200940913     C                   MOVEL     FGVTMZ        FG4TMZ
611300940913     C                   MOVEL     FGVTTR        FG4TTR
611400940913     C                   MOVEL     FGVFRQ        FG4FRQ
611500940913     C                   MOVEL     FGVIPT        FG4IPT
611600940913     C                   MOVEL     FGVSNS        FG4SNS
611700940913     C                   MOVEL     FGVSNP        FG4SNP
611800940913     C                   MOVEL     FGVLNA        FG4LNA
611900940913     C                   MOVEL     FGVFFV        FG4FFV
612000940913     C                   MOVEL     FGVFF2        FG4FF2
612100940913     C                   MOVEL     FGVFLP        FG4FLP
612200940913     C                   MOVEL     FGVFL2        FG4FL2
612300940913     C                   MOVEL     FGVDTP        FG4DTP
612400940913     C                   MOVEL     FGVHMP        FG4HMP
612500940913     C                   MOVEL     FGVNT1        FG4NT1
612600940913     C                   MOVEL     FGVNT2        FG4NT2
612700940913     C                   MOVEL     FGVPMB        FG4PMB
612800940913     C                   MOVEL     FGVFCF        FG4FCF
612900940913     C                   MOVEL     FGVTRN        FG4TRN
613000940913     C                   MOVEL     FGVNFA        FG4NFA
613100931116     C*
613200940913     C   82              WRITE     FNFGV046
613300940913     C  N82              UPDATE    FNFGV046
613400931116     C*
613500960119     C                   MOVEL     '1'           WLSA4             1
613600940909     C                   ENDSR
613700951103     C*-----------------------------------------------------***********
613800951103     C* SCRIVO FILE FNFV200F                                * WTRFV2  *
613900951103     C*-----------------------------------------------------***********
614000951103     C     WTRFV2        BEGSR
614100951103     C*
614200960123     C                   MOVEL     'R'           KTDH
614300960123     C                   MOVEL     'P'           KEPA
614400951103     C     KFV2          CHAIN     FNFV201L                           83
614500960130    1C     VIDRTC        IFEQ      ' '
614600960130     C  N83              DELETE    FNFV2000
614700960130   X1C                   ELSE
614800951103     C   83              CLEAR                   FNFV2000
614900951103     C*
615000951103     C                   MOVEL     ' '           FV2ATB
615100951106     C                   MOVEL     'R'           FV2TDH
615200951106     C                   MOVEL     FGVDTP        FV2DPA
615300951106     C                   MOVEL     FGVHMP        FV2HPA
615400951103     C                   MOVEL     VIDRTC        FV2RTC
615500951114     C                   CLEAR                   FV2FTR
615600951114     C                   CLEAR                   FV2DTR
615700951106     C*
615800951106     C* 83 OFF - VARIAZIONE
615900960130    2C     *IN83         IFEQ      *OFF
616000951103     C                   UPDATE    FNFV2000
616100951106     C*
616200960130   X2C                   ELSE
616300951106     C* 83 ON  - IMMISSIONE
616400951103     C                   MOVEL     SIMFEL        FV2LNP
616500951103     C                   MOVEL     VIDNFV        FV2NFV
616600951106     C                   MOVEL     'P'           FV2EPA
616700951103     C                   WRITE     FNFV2000
616800960130    2C                   ENDIF
616900960130    1C                   ENDIF
617000960111     C*
617100951103     C                   ENDSR
617200960123     C**************************************************************************
617300960123     C*  INTERROGO GLI ORARI DI ARRIVO
617400960123     C**************************************************************************
617500960123     C     INTORA        BEGSR
617600960129     C* SOLO A CAMBIO FV RICARICO IL SFL
617700960129     C     FGVNFV        IFNE      WINNFV
617800960129     C                   MOVEL     FGVNFV        WINNFV
617900960129     C*
618000960129     C                   Z-ADD     0             NRR5              5 0
618100960129     C                   MOVEL     'A'           KEPA
618200960129     C                   SETOFF                                       17
618300960123     C* PULIZIA SFL
618400960123     C                   SETON                                        70
618500130315     C                   WRITE     lst4C05
618600960123     C                   SETOFF                                       70
618700960129     C*
618800960129     C* SE HA GIA' RILEVATO CHE NON HO NIENTE, SCRIVO 1 REC VUOTO
618900960129    0C     *IN15         IFEQ      *ON
619000960123     C*
619100960129     C* LEGGO PER DATA TEORICA, SE CI SONO, ALTRIMENTI PER DATA REALE
619200960129     C                   MOVEL     'T'           KTDH
619300960129     C     KFV24         SETLL     FNFV2003
619400960130     C     KFV24         READE(N)  FNFV2003                               83
619500960129     C*
619600960129     C* NON CI SONO DATE TEORICHE: LEGGO SOLO LE REALI
619700960129    1C     *IN83         IFEQ      *ON
619800960129     C                   MOVEL     'R'           KTDH
619900960129     C**
620000960130     C     KFV24         SETLL     FNFV2003
620100960130     C     KFV24         READE(N)  FNFV2003                               83
620200960129    2C     *IN83         DOWEQ     *OFF
620300960129     C                   EXSR      DECORA
620400960129     C                   EXSR      WRTSF4
620500960129     C**
620600960130     C     KFV24         READE(N)  FNFV2003                               83
620700960129    2C                   ENDDO
620800960129     C**
620900960129   X1C                   ELSE
621000960129     C**
621100960129    2C     *IN83         DOWEQ     *OFF
621200960129     C                   EXSR      DECORA
621300960129     C*
621400960129     C                   MOVEL     'R'           KTDH
621500960129     C                   MOVEL     FV2LAI        WTFA
621600960129     C     KFV22         CHAIN(N)  FNFV201L                           83
621700960129     C                   EXSR      DECORA
621800960129     C                   EXSR      WRTSF4
621900960129     C**
622000960129     C                   MOVEL     'T'           KTDH
622100960130     C     KFV24         READE(N)  FNFV2003                               83
622200960129    2C                   ENDDO
622300960129    1C                   ENDIF
622400960129     C**
622500960129     C                   ELSE
622600960129     C** NON CI SONO RECORD DA CARICARE
622700130315     C                   CLEAR                   lst4S05
622800960129     C                   EXSR      WRTSF4
622900960129    0C                   ENDIF
623000960129     C*
623100960129     C                   ENDIF
623200960123     C*
623300960123     C     FOR05         TAG
623400130315     C                   WRITE     lst4T01
623500130315     C                   WRITE     lst4Z05
623600130315     C                   EXFMT     lst4C05
623700960126     C                   MOVEL     '5'           WRECOR            1
623800960129     C* F12 - RITORNO
623900960123     C     *INKL         IFEQ      *OFF
624000960123     C                   CLEAR                   W001A
624100130315     C                   READC     lst4S05                                83
624200960123     C     *IN83         DOWEQ     *OFF
624300960123     C     V5CVIS        IFEQ      'V'
624400960123     C                   CLEAR                   V5CVIS
624500960123     C                   SETON                                        17
624600130315     C                   UPDATE    lst4S05
624700960126     C*
624800020805     C                   CLEAR                   FNLV15DS
624900960126     C* SE PGM RICHIAMATO RICHIAMO CON 'L?'
625000960126     C   20              MOVEL     'L'           D15TLA
625100960126     C                   MOVEL     'A'           D15EPA
625200960126     C                   MOVEL     'R'           D15TRC
625300960126     C                   MOVEL     'I'           D15TUP
625400960126     C                   MOVEL     V5CRTC        D15RTC
625500130419     C                   MOVEL     VIDfgs        D15LNP
625600960126     C                   MOVEL     VIDNFV        D15NFV
625700960126     C                   MOVEL     AMGDFV        D15DFV
625800960126     C                   MOVEL     VIDTRN        D15TRN
625900960222     C                   MOVEL     VIDTTR        D15TTR
626000960126     C                   MOVEL     V5CFIL        D15LAI
626100960123     C*
626200020805     C                   MOVEL     FNLV15DS      KPJBU
626300960123     C                   CALL      'FNLV15R'
626400960123     C                   PARM                    KPJBA
626500960123     C                   MOVEL     '1'           W001A
626600960123     C                   ENDIF
626700960123     C*
626800130315     C                   READC     lst4S05                                83
626900960123     C                   ENDDO
627000960123     C*
627100960123     C     W001A         IFEQ      '1'
627200960123     C                   GOTO      FOR05
627300960123     C                   ENDIF
627400960123     C                   ENDIF
627500960123     C*
627600960123     C                   ENDSR
627700960129     C*---------------------------------------------------------------**********
627800960129     C* DECODIFICHE DEL SFL DELGLI ORARI DI ARRIVO
627900960129     C*---------------------------------------------------------------**********
628000960129     C     DECORA        BEGSR
628100960129     C* DATA
628200960129    1C     *IN83         IFEQ      *OFF
628300960129     C                   MOVE      FV2DPA        G02INV
628400960129     C                   MOVE      *ZEROS        G02DAT
628500960129     C                   MOVEL     '3'           G02ERR
628600960129     C                   CALL      'XSRDA8'
628700960129     C                   PARM                    WLBDAT
628800960129    1C                   ENDIF
628900960129     C*
629000960129    1C     KTDH          IFEQ      'T'
629100960129     C                   Z-ADD     G02DAT        V5CDTT
629200960129     C                   MOVEL     FV2HPA        V5CHMT
629300960129   X1C                   ELSE
629400960129     C*
629500960129     C* DECODIFICO ANCHE LA FILIALE DI INVIO
629600960129     C                   Z-ADD     FV2LAI        CODFIL
629700960129     C                   EXSR      CTRFIL
629800960129     C  N34              MOVEL     ORGDES        V5DFIL
629900960129     C   34              MOVEL     *ALL'?'       V5DFIL
630000960129     C                   MOVEL     FV2LAI        V5CFIL
630100960129     C**
630200960129    2C     *IN83         IFEQ      *OFF
630300960129     C                   Z-ADD     G02DAT        V5CDTR
630400960129     C                   MOVEL     FV2HPA        V5CHMR
630500960129     C* VEDO SE C'E' RITARDO
630600960129    3C     FV2RTC        IFNE      *BLANKS
630700960129     C                   MOVEL     FV2RTC        V5CRTC
630800960129     C                   MOVEL     'TC'          COD
630900960129     C                   MOVEL(P)  FV2RTC        KEY
631000960129     C     KTAB          CHAIN     TABEL                              83
631100960129     C  N83              MOVEL     TBLUNI        V5DRTC
631200960129     C   83              MOVEL     *ALL'?'       V5DRTC
631300960129     C* VERIFICO SE C'E' ANCHE LA DESCRIZIONE
631400960129     C                   MOVEL     'A'           KEPA
631500960129     C                   Z-ADD     FV2LAI        KTFA
631600960129     C                   MOVEL     'R'           KTRC
631700960129     C                   EXSR      CTRDRA
631800960129    4C     W001A         IFNE      *BLANKS
631900960129     C                   SETON                                        17
632000960129     C                   ELSE
632100960129     C                   SETOFF                                       17
632200960129    4C                   ENDIF
632300960129    3C                   ENDIF
632400960129    2C                   ENDIF
632500960129    1C                   ENDIF
632600960129     C**
632700960129     C                   ENDSR
632800960129     C*---------------------------------------------------------------**********
632900960129     C* SCRITTUTA DEL SFL 4 E PULIZIA DEI CAMPI
633000960129     C*---------------------------------------------------------------**********
633100960129     C     WRTSF4        BEGSR
633200960129     C                   ADD       1             NRR5
633300130315     C                   WRITE     lst4S05
633400960129     C                   CLEAR                   V5CDTT
633500960129     C                   CLEAR                   V5CHMT
633600960129     C                   CLEAR                   V5CDTR
633700960129     C                   CLEAR                   V5CHMR
633800960129     C                   CLEAR                   V5CRTC
633900960129     C                   CLEAR                   V5DRTC
634000960129     C                   ENDSR
634100940909     C**************************************************************************
634200940909     C*   R O U T I N E    I N I Z I A L E
634300940909     C**************************************************************************
634400940909     C     *INZSR        BEGSR
634500940909     C*
634600960122     C     KCLN1         KLIST
634700960122     C                   KFLD                    KTFP
634800960122     C                   KFLD                    KTFA
634900960122     C                   KFLD                    KANN
635000960122     C                   KFLD                    KMES
635100000202     C     KFGV1         KLIST
635200940909     C                   KFLD                    KNPG
635300000202     C                   KFLD                    WNFV
635400940909     C                   KFLD                    KFGS
635500940909     C                   KFLD                    COMLNA
635600951025     C     KFGV2         KLIST
635700951025     C                   KFLD                    SIMFEL
635800951025     C                   KFLD                    AMGDFV
635900130606     C     KFGV02        KLIST
636000130606     C                   KFLD                    fgvlnp
636100130606     C                   KFLD                    fgvdfv
636200940909     C     KBLP1         KLIST
636300020805     c                   kfld                    SIMFEL
636400940909     C                   KFLD                    VIDNFV
636500020805     C                   KFLD                    COMLNA
636600020805     c     K02BLP79      KLIST
636700020805     c                   kfld                    SIMFEL
636800020805     c                   kfld                    VIDNFV
636900020805     c     K02FGV01      KLIST
637000020805     c                   kfld                    VIDNFV
637100020805     c                   kfld                    SIMFEL
637200020805     c     K02FGW01      KLIST
637300020805     c                   kfld                    FGWnfv
637400020805     c                   kfld                    FGWlnp
637500951024     C     KFGV46        KLIST
637600940909     C                   KFLD                    CODFIL
637700940909     C                   KFLD                    VIDNFV
637800940912     C     KBRV1         KLIST
637900940912     C                   KFLD                    KNPG
638000000202     C                   KFLD                    WNFV
638100941229     C                   KFLD                    KFGS
638200950901     C     KTAB          KLIST
638300950901     C                   KFLD                    CODUT
638400950901     C                   KFLD                    COD
638500950901     C                   KFLD                    KEY
638600951023     C     KTLR          KLIST
638700951025     C                   KFLD                    VIDTRN
638800951016     C                   KFLD                    WTFM
638900951016     C                   KFLD                    WFIL
639000951024     C     KTLR1         KLIST
639100951025     C                   KFLD                    VIDTRN
639200951024     C                   KFLD                    WDDE
639300951024     C                   KFLD                    WNFM
639400951024     C     KTLR11        KLIST
639500951025     C                   KFLD                    VIDTRN
639600951024     C                   KFLD                    WDDE
639700951024     C     KTLL1         KLIST
639800951025     C                   KFLD                    VIDTRN
639900951024     C                   KFLD                    WDDE
640000951024     C                   KFLD                    KTMZ
640100951024     C                   KFLD                    KPDR
640200951024     C     KTLR3         KLIST
640300951025     C                   KFLD                    VIDTRN
640400951024     C                   KFLD                    WDDE
640500951024     C                   KFLD                    WTFM
640600951024     C                   KFLD                    WFIL
640700951024     C     KTLM          KLIST
640800951025     C                   KFLD                    VIDTRN
640900951024     C                   KFLD                    WDDE
641000951024     C                   KFLD                    WTFP
641100951024     C                   KFLD                    WTFA
641200960122     C     KTLM1         KLIST
641300960122     C                   KFLD                    VIDTRN
641400960122     C                   KFLD                    WDDE
641500960122     C                   KFLD                    WTFP
641600110511     C     KTLs          KLIST
641700110511     C                   KFLD                    VIDTRN
641800110511     C                   KFLD                    WDDE
641900110511     C                   KFLD                    gioset
642000950901     C     KCAE          KLIST
642100960123     C                   KFLD                    KEPA
642200951025     C                   KFLD                    WTFP
642300951025     C                   KFLD                    WFIL
642400960226     C     KCAE2         KLIST
642500960226     C                   KFLD                    KEPA
642600960226     C                   KFLD                    KTFA
642700951031     C     KFV4          KLIST
642800951031     C                   KFLD                    SIMFEL
642900951031     C                   KFLD                    VIDNFV
643000960123     C                   KFLD                    KEPA
643100960123     C                   KFLD                    KTFA
643200960123     C                   KFLD                    KTRC
643300951103     C     KFV2          KLIST
643400951103     C                   KFLD                    SIMFEL
643500951103     C                   KFLD                    VIDNFV
643600960123     C                   KFLD                    KEPA
643700960122     C                   KFLD                    ZERO
643800960122     C                   KFLD                    KTDH
643900960122     C     KFV22         KLIST
644000960122     C                   KFLD                    FGVLNP
644100960122     C                   KFLD                    FGVNFV
644200960129     C                   KFLD                    KEPA
644300960122     C                   KFLD                    WTFA
644400960122     C                   KFLD                    KTDH
644500130606     C     KFV22A        KLIST
644600130606     C                   KFLD                    F2VLNP
644700130606     C                   KFLD                    F2VNFV
644800130606     C                   KFLD                    KEPA
644900130606     C                   KFLD                    WTFA
645000130606     C                   KFLD                    KTDH
645100960122     C     KFV23         KLIST
645200960122     C                   KFLD                    FGVLNP
645300960122     C                   KFLD                    FGVNFV
645400960129     C                   KFLD                    KEPA
645500960129     C     KFV24         KLIST
645600960129     C                   KFLD                    FGVLNP
645700960129     C                   KFLD                    FGVNFV
645800960129     C                   KFLD                    KEPA
645900960129     C                   KFLD                    KTDH
646000000807     C     KTNTLZ        KLIST
646100020805     C                   KFLD                    KTLTIP
646200000807     C                   KFLD                    KTLPDR
646300080910     c     kfvs          KLIST
646400080910     c                   kfld                    fgvlnp
646500080910     c                   kfld                    fgvnfv
646600080910     c     ktst          KLIST
646700080910     c                   kfld                    tsrtrn
646800080910     c                   kfld                    tsrdtn
646900130424     c     kata          KLIST
647000130424     c                   kfld                    soctraini
647100130424     c                   kfld                    kpdr
647200130424     c                   kfld                    kdcn
647300130424     c     katm          KLIST
647400130424     c                   kfld                    soctraini
647500130424     c                   kfld                    kpdr
647600130424     c                   kfld                    vidtrm
647700130605
647800130605     c     kfge          klist
647900130605     c                   kfld                    fgvdfv
648000130605     c                   kfld                    fgvlnp
648100130605     c                   kfld                    wlna
648200940909     C*
648300951009     C****  IMPOSTO CAMPI FISSI  ****
648400950905     C                   SETOFF                                       01
648500130417     C                   MOVEL     'FNLST4R '    VIDPGM
648600951026     C***
648700951026     C     *ENTRY        PLIST
648800951026     C                   PARM                    KPJBA
648900130315     C                   MOVEL     KPJBU         fnlst4ds
649000050706     c                   movel     knmus         iniziopro
649100130429     C                   SETON                                        20
649200130502     C                   CLEAR                   olst4msg
649300130502     C                   CLEAR                   OLST4F12
649400130429
649500130429
649600951026     C                   SELECT
649700130315     C     ilst4ges      WHENEQ    ' '
649800951026     C                   MOVE      TES(1)        VIDTES
649900951026     C*
650000130429     C* CHIUSURA FOGLIO VIAGGIO SINGOLA
650100130315     C     ilst4ges      WHENEQ    'C'
650200130502     C     ilst4ges      oreq      'I'
650300130426     C                   MOVE      TES(2)        VIDTES
650400951027     C                   SETON                                        01
650500130429
650600951027     C* MANUTENZIONE FV RICHIAMATA
650700130315     C     ilst4ges      WHENEQ    'M'
650800130315     C     ilst4ges      oreq      'A'
650900130419     C     ilst4ges      oreq      'P'
651000951027     C                   MOVE      TES(1)        VIDTES
651100960123     C* INTERROGAZIONE FV SINGOLA
651200130315     C     ilst4ges      WHENEQ    'N'
651300130430     C                   SETON                                        01
651400960123     C                   SETON                                        9899
651500960123     C                   MOVE      TES(1)        VIDTES
651600960123     C*
651700951027     C                   OTHER
651800130430     C                   SETON                                        01
651900951026     C* VENGO DA BORDERO'
652000130315     C     ilst4ges      IFEQ      'B'
652100951026     C                   SETON                                        81
652200951026     C                   MOVE      TES(3)        VIDTES
652300951026     C                   ENDIF
652400951026     C*
652500951026     C                   ENDSL
652600961213     C*
652700130315     C     ilst4ges      IFNE      'N'
652800130510     c                   if        not %open(fnfgv02l)
652900961213     C                   OPEN      FNFGV02L
653000130510     c                   endif
653100130510     c                   if        not %open(fnfgv06l)
653200020805     C                   OPEN      FNFGV06L
653300130510     C                   endif
653400130510     C                   END
653500080909      *
653600080919     c* In manutenzione apro files che mi serviranno nel controllo del
653700080909     c* numero foglio
653800130315     c                   if        ilst4ges=' '  or ilst4ges='A' or
653900130419     c                             ilst4ges='M'  or ilst4ges='P'
654000080919     c                   Open(e)   TNFVS01L
654100080919     c                   if        not %open(tnfvs01l)
654200080919     c                   eval      %subst(Wfvs01:7:4)='GRPS'
654300080919     c                   Open      TNFVS01L
654400080919     c                   endif
654500080919     c                   Open(e)   tntsr04l
654600080919     c                   if        not %open(tntsr04l)
654700080919     c                   eval      %subst(Wtsr04:7:4)='GRPS'
654800080919     c                   Open      TNtsr04l
654900080919     c                   endif
655000080919     c                   Open(e)   tntst01l
655100080919     c                   if        not %open(tntst01l)
655200080919     c                   eval      %subst(Wtst01:7:4)='GRPS'
655300080919     c                   Open      TNtst01l
655400080919     c                   endif
655500080909     c                   endif
655600951026     C     *IN01         IFEQ      *OFF
655700951026     C* OPEN DI ALCUNI FILE
655800130510     c                   if        not %open(fnfgv46l)
655900951027     C                   OPEN      FNFGV46L
656000130510     c                   endif
656100130510     c                   if        not %open(azcae03l)
656200951026     C                   OPEN      AZCAE03L
656300130510     c                   endif
656400951026     C                   ENDIF
656500951026     C*
656600951023     C                   CALL      'X§PARUT'
656700020805     C                   PARM                    UT§DSE0F
656800020805     C                   MOVEL     REC80         CNCR80
656900951023     C                   MOVEL     RAGUT         VIDRSU
657000951026     C                   MOVEL     RAGUT         PA2RSU
657100951026     C                   Z-ADD     SIMFEL        PA2FEL
657200951023      *
657300030701      * Questo controllo è stato sostituito
657400030701      * ora è - 1 e + 1 dalla data odierna
657500030701      * Ho spostato il giro xchè ora il controllo lo devo fare anche in
657600030701      * manutenzione, -1 + 1 dalla data foglio (se è modificabile)
657700951023     C*
657800951023     C* LA DATA DEL FOGLIO VIAGGIO NON DEVE ANDARE OLTRE ALLA DATA
657900951023     C* CORRENTE MENO 1 GG. E ALLA DATA CORRENTE PIU' 3 GG.
658000951023     C*
658100951023     C* DEVO USARE TIME AL POSTO DI UDATE, POICHE' UDATE NON VIENE
658200951023     C* AGGIORNATA, DOPO LA MEZZANOTTE, SE IL JOB E' GIA' APERTO
658300951023     C*
658400951023     C                   TIME                    WTIME            14 0
658500951023     C                   MOVE      WTIME         WDATE
658600951023     C                   MOVE      WDATE         G02DAT
658700951023     C                   MOVEL     *ZERO         G02INV
658800951023     C                   MOVEL     *BLANKS       G02ERR
658900951023     C                   CALL      'XSRDA8'
659000951023     C                   PARM                    WLBDAT
659100951106     C                   MOVEL     G02INV        DATEU
659200951026     C*
659300130419     C* SE VENGO DA CHIUSURA FV  CARICO LA L1 E VEDO SE SIMFEL HA VDL
659400960126     C                   Z-ADD     SIMFEL        CODFIL
659500960126     C                   EXSR      CTRFIL
659600130419     C  N34              MOVEL     ORGDES        DESfgs
659700991020     C   34              MOVEL     *ZEROS        ORGDF0
659800960209     C   34              MOVEL     *BLANKS       ORGDE8
659900040121     c                   eval      og150=orgdf0
660000951026     C*
660100040121     C                   TESTN                   §ogvol               83
660200951026     C     *IN83         IFEQ      '1'
660300040121     C     §ogvol        ANDGT     *ZEROS
660400960209     C                   MOVEL     'S'           PA2CML            1
660500120914     C                   MOVEL     'S'           PA2spv
660600951026     C                   END
660700991020     C*
660800040121     C                   TESTN                   §ogpes               83
660900991020     C     *IN83         IFEQ      '1'
661000040121     C     §ogpes        ANDGT     *ZEROS
661100991020     C                   MOVEL     'P'           PA2CML
661200120914     C                   MOVEL     'S'           PA2spp
661300991020     C                   END
661400040121     c*
661500960209     C* DALLA DESCRIZIONE 148 VEDO QUALE GENERAZIONE SPUNTE DEVO FARE
661600960209     C*  IN CHIUSURA FV
661700960209     C                   MOVEL     ORGDE8        OG148
661800960209     C     §OGAGS        IFEQ      'E'
661900960209     C                   MOVEL     'S'           PA2AGS
662000010911     C                   MOVEL     §OGAGP        PA2AGP
662100960209     C                   ELSE
662200960209     C                   CLEAR                   PA2AGS
662300010911     C                   CLEAR                   PA2AGP
662400960209     C                   ENDIF
662500960209     C**
662600951027     C* CARICO L1 --> FILIALI GESTITE
662700020805     C                   CLEAR                   TRUL06DS
662800951026     C                   MOVE      '£1'          D06COD
662900951026     C                   MOVEL     SIMFEL        D06KEY
663000020805     C                   MOVEL     TRUL06DS      KPJBU
663100951026     C*
663200951026     C                   CALL      'TRUL06R'
663300951026     C                   PARM                    KPJBA
663400020805     C                   MOVEL     KPJBU         TRUL06DS
663500951026     C                   MOVEA     LIN           L1
663600951106     C* VADO SULLE TABELLE PER VEDERE SE E' UN TIPO RECORD DA
663700951106     C*  TRASMETTERE
663800960123     C                   MOVEL     '3B'          COD
663900960123     C                   MOVEL(P)  'N'           KEY
664000951106     C     KTAB          CHAIN     TABEL                              83
664100951106     C     *IN83         IFEQ      *OFF
664200951106     C                   MOVEL     TBLUNI        DS3B
664300960126     C                   MOVEL     §3BTRS        WTRSN2
664400960126     C                   MOVEL     §3BTRF        WTRSN1
664500951106     C                   ENDIF
664600960302     C*
664700960302     C                   MOVEL(P)  'C'           KEY
664800960302     C     KTAB          CHAIN     TABEL                              83
664900960302     C     *IN83         IFEQ      *OFF
665000960302     C                   MOVEL     TBLUNI        DS3B
665100960302     C                   MOVEL     §3BTRS        WTRSC2
665200960302     C                   MOVEL     §3BTRF        WTRSC1
665300960302     C                   ENDIF
665400960302     C*
665500960302     C                   MOVEL(P)  'D'           KEY
665600960302     C     KTAB          CHAIN     TABEL                              83
665700960302     C     *IN83         IFEQ      *OFF
665800960302     C                   MOVEL     TBLUNI        DS3B
665900960302     C                   MOVEL     §3BTRS        WTRSD2
666000960302     C                   MOVEL     §3BTRF        WTRSD1
666100960302     C                   ENDIF
666200951009     C*
666300010621     C                   MOVEL(P)  'L'           KEY
666400010621     C     KTAB          CHAIN     TABEL                              83
666500010621     C     *IN83         IFEQ      *OFF
666600010621     C                   MOVEL     TBLUNI        DS3B
666700010621     C                   MOVEL     §3BTRS        WTRSL2
666800010621     C                   MOVEL     §3BTRF        WTRSL1
666900010621     C                   ENDIF
667000130424     c*
667100130424     C                   MOVEL(P)  'P'           KEY
667200130424     C     KTAB          CHAIN     TABEL                              83
667300130424     C     *IN83         IFEQ      *OFF
667400130424     C                   MOVEL     TBLUNI        DS3B
667500130424     C                   MOVEL     §3BTRS        WTRSP2
667600130424     C                   MOVEL     §3BTRF        WTRSP1
667700130424     C                   ENDIF
667800010927      *
667900010927      * RECUPERO LA MONETA DI CONTO
668000010927     C                   CLEAR                   DGED
668100020805     C                   CLEAR                   TIBS02DS
668200010927     C                   MOVEL     'L'           T02TLA
668300010927     C                   MOVEL     'C'           T02MOD
668400010927     C                   MOVEL     KNSIF         T02SIF
668500010927     C                   MOVEL     'GED'         T02COD
668600010927     C                   MOVEL     '1'           T02KE1
668700010927     C                   CALL      'TIBS02R'
668800010927     C                   PARM                    KPJBA
668900020805     C                   PARM                    TIBS02DS
669000010927     C                   MOVEL     T02UNI        DGED
669100130424     c
669200130424     c* Reperisco società dei trazionisti
669300130424     C                   MOVEL     '3I'          COD
669400130424     C                   MOVEL(P)  '1'           KEY
669500130424     C     KTAB          CHAIN     TABEL                              83
669600130424     C     *IN83         IFEQ      *OFF
669700130424     c                   movel     tbluni        ds3i
669800130424     c                   eval      alfasoc=%subst(§3ibst:7:3)
669900130424     c                   if        alfasoc>*zeros
670000130424     c                   EVAL      soctraini=%int(alfasoc)
670100130424     c                   endif
670200130424     c                   endif
670300130510     c*
670400130510     c* Reperisco tabella RMA
670500130510     c                   Clear                   tibs02ds
670600130510     c                   Clear                   dmradan
670700130510     c                   Movel     'C'           T02Mod
670800130510     c                   Movel     KNSIF         T02Sif
670900130510     c                   Movel     'MRA'         T02Cod
671000130510     c                   Movel     'FNLST4R'     T02Ke1
671100130510     c                   Call      'TIBS02R'
671200130510     c                   Parm                    Kpjba
671300130510     c                   Parm                    tibs02ds
671400130510    2c                   If        T02Err = *Blanks
671500130510     c                   Movel     T02Uni        Dmradan
671600130510    2c                   endif
671700130510     C*
671800010927      *
671900940909     C                   ENDSR
672000951024     C**************************************************************************
672100951024     C*  ABBLANCA ZERO NON SIGNIFICATIVI
672200951024     C**************************************************************************
672300951024     C     ABBL          BEGSR
672400951024     C*
672500951026     C     K15(Z)        DOWEQ     '0'
672600951026     C     Z             ANDLT     15
672700951026     C                   MOVEL     ' '           K15(Z)
672800951026     C                   ADD       1             Z                 3 0
672900951024     C                   ENDDO
673000951024     C*
673100951024     C                   ENDSR
673200951025     C**************************************************************************
673300951025     C*  CONTROLLO E DEOCIFICO ESISTENZA TIPO AUTOMEZZO
673400951025     C**************************************************************************
673500951025     C     CTRTMZ        BEGSR
673600951025     C                   MOVEL     'TU'          COD
673700951025     C                   MOVEL(P)  VIDTMZ        KEY
673800951025     C     KTAB          CHAIN     TABEL                              83
673900951025    1C  N83TBLFLG        IFEQ      ' '
674000951025     C                   MOVEL     TBLUNI        DESTMZ
674100130502     C                   MOVEL     TBLUNI        DESTMZ2
674200951025     C                   ELSE
674300951025     C                   SETON                                        83
674400951025     C                   CLEAR                   DESTMZ
674500130502     C                   CLEAR                   DESTMZ2
674600951025    1C                   ENDIF
674700951025     C                   ENDSR
674800951025     C**************************************************************************
674900951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
675000951025     C**************************************************************************
675100951025     C     CTRFIL        BEGSR
675200020213
675300020213     c                   clear                   esteroedpd
675400020213
675500951025     C     CODFIL        CHAIN     AZORG01L                           34
675600951025    1C  N34ORGFVA        IFNE      ' '
675700951025     C     ORGFAG        ORNE      'A'
675800951025     C     ORGFAG        ANDNE     'F'
675900951025     C                   SETON                                        34
676000951025    1C                   ENDIF
676100000405     C  N34              MOVEL     ORGDE3        OG143
676200020213
676300020213      * Network esteri + DPD
676400020213     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
676500020213     c                             §ogntw = 'FED' or §ogntw = 'DPD'
676600020213     c                   eval      esteroedpd = 'S'
676700020213     c                   endif
676800951025     C*
676900951025     C                   ENDSR
677000951027     C**************************************************************************
677100951027     C*  CONTROLLA DATA FV E CALCOLO DFV - 1 E DFV + 5
677200951027     C**************************************************************************
677300951027     C     CALDAT        BEGSR
677400951027     C                   MOVE      VIDDFV        G02DAT
677500951027     C                   MOVEL     *BLANK        G02ERR
677600951027     C                   CALL      'XSRDA8'
677700951027     C                   PARM                    WLBDAT
677800951027     C* ERRORE
677900960122    1C     G02ERR        IFEQ      '1'
678000951027     C                   MOVEL     MSG(4)        VIDMSG
678100960122     C                   SETON                                        442890
678200960122   X1C                   ELSE
678300951027     C* NON C'E' STATO ERRORE
678400960122    2C     G02INV        IFNE      AMGDFV
678500951130     C                   CLEAR                   WFORZ9
678600960122     C                   CLEAR                   WFORZB
678700960122    2C                   ENDIF
678800951027     C                   Z-ADD     G02INV        AMGDFV
678900951130     C                   Z-ADD     G02DAT        VIDDFV
679000960122     C                   Z-ADD     G02DAT        KANN
679100960122     C                   Z-ADD     G02DAT        WDATA
679200960122     C                   MOVEL     MM            KMES
679300030630      * non può essere minore o maggiore di 1 gg. lavorativo
679400030630     c                   If        AmgDfv < wdtameno1  or
679500030630     c                             AmgDfv > wdtapiu1
679600030701     c                   Eval      VidMsg = Msg(7)
679700030630     c                   Eval      *in28 = *On
679800030630     c                   Eval      *in44 = *On
679900030630     c                   Eval      *in90 = *On
680000030630     c                   EndIf
680100960122     C* SOLO IN IMMISSIONE --> SE DATA DFV CADE IN GIORNO FESTIVO PER
680200960122     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
680300040204    2C  N90*IN80         IFEQ      *ON
680400040204     C     wd02          IFEQ      'S'
680500960123     C                   CLEAR                   KTFA
680600960123     C                   Z-ADD     68            KTFP
680700960122     C     KCLN1         CHAIN     AZCLN01L                           83
680800960122    3C  N83POM(GG)       IFNE      *BLANKS
680900960122     C                   MOVEL     '1Y'          COD
681000960122     C                   MOVEL(P)  POM(GG)       KEY
681100960122     C     KTAB          CHAIN     TABEL                              83
681200960122     C                   MOVEL     TBLUNI        DS1Y
681300960122     C*
681400960122    4C  N83§1YFET        IFNE      *BLANKS
681500960122     C     WFORZB        ANDEQ     *BLANKS
681600960122     C                   MOVEL     MSG(63)       VIDMSG
681700960122     C                   SETON                                        442890
681800960123     C                   MOVEL     '1'           WFORZB            1
681900960122    4C                   ENDIF
682000960122    3C                   ENDIF
682100040204     c                   endif
682200970526   X2C                   ELSE
682300970526     C* IN MANUTENZIONE
682400970526     C* NON POSSO MODIFICARLA SE HA SPUNTE
682500970526     C  N80AMGDFV        IFNE      FGVDFV
682600000202     C                   Z-ADD     VIDNFV        WNFV
682700070112     C     KBRV1         SETLL     FNBRV02L                               83
682800970526     C   83              MOVEL     MSG(72)       VIDMSG
682900970526     C   83              SETON                                        442890
683000960122    2C                   ENDIF
683100970526    2C                   ENDIF
683200960122     C**
683300960122    2C     *IN90         IFEQ      *OFF
683400951027     C                   Z-ADD     G02TGI        WGGDFV
683500951027     C* DFV -1
683600951027     C     G02TGI        SUB       1             GI8TGI
683700951027     C*
683800951027     C                   CALL      'XSRGI8'
683900951027     C                   PARM                    WGIDAT
684000951027     C                   MOVEL     GI8INV        DTPMIN            8 0
684100951027     C* DFV + 5
684200951027     C     G02TGI        ADD       5             GI8TGI
684300951027     C*
684400951027     C                   CALL      'XSRGI8'
684500951027     C                   PARM                    WGIDAT
684600951027     C                   MOVEL     GI8INV        DTPMAX            8 0
684700960122    2C                   ENDIF
684800960122    1C                   ENDIF
684900100616
685000100616     c* Determino data foglio - 1 gg lavorativo
685100100616     c                   Clear                   Ktfp
685200100616     c                   Z-Add     VidFgs        Ktfa
685300100616     c     *eur          Move      Viddfv        wdtaeur
685400100616      * meno 1 gg. lavorativo
685500100616     c                   Do        *Hival
685600100616     c                   Subdur    1:*d          wdtaeur
685700100616     c                   extrct    wdtaeur:*d    wgiorno
685800100616     c                   extrct    wdtaeur:*m    kmes
685900100616     c                   extrct    wdtaeur:*y    kann
686000100616     c     kcln1         Chain     Azcln01l
686100100616     c                   If        Not %Found(azcln01l)
686200100616     c                   Leave
686300100616     c                   EndIf
686400100616     c                   If        POM(wgiorno) = 'F'
686500100616     c                   Iter
686600100616     c                   EndIf
686700100616     c                   Leave
686800100616     c                   EndDo
686900100616     c
687000100616     c                   Move      wdtaeur       wdtaiso
687100100616     c                   Move      wdtaiso       wDEFmeno1
687200951027     C*
687300951027     C                   ENDSR
687400030630
687500030630      **************************************************************************
687600030630      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
687700030630      **************************************************************************
687800030630     c     Sr_MinMax     BegSr
687900030701
688000030630     c                   Clear                   Ktfa
688100030630     c                   Z-Add     VidFgs        Ktfp
688200030701     c     *eur          Move      Viddfv        wdtaeur
688300030630      * meno 1 gg. lavorativo
688400030630     c                   Do        *Hival
688500030701     c                   Subdur    1:*d          wdtaeur
688600030701     c                   extrct    wdtaeur:*d    wgiorno
688700030701     c                   extrct    wdtaeur:*m    kmes
688800030701     c                   extrct    wdtaeur:*y    kann
688900030630     c     kcln1         Chain     Azcln01l
689000030630     c                   If        Not %Found(azcln01l)
689100030630     c                   Leave
689200030630     c                   EndIf
689300030630     c                   If        POM(wgiorno) = 'F'
689400030630     c                   Iter
689500030630     c                   EndIf
689600030630     c                   Leave
689700030630     c                   EndDo
689800030701     c                   Move      wdtaeur       wdtaiso
689900030701     c                   Move      wdtaiso       wdtameno1
690000030630
690100050706      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
690200050706     c*  con data foglio-1)
690300050706     c                   Do        *Hival
690400050706     c                   Subdur    1:*d          wdtaeur
690500050706     c                   extrct    wdtaeur:*d    wgiorno
690600050706     c                   extrct    wdtaeur:*m    kmes
690700050706     c                   extrct    wdtaeur:*y    kann
690800050706     c     kcln1         Chain     Azcln01l
690900050706     c                   If        Not %Found(azcln01l)
691000050706     c                   Leave
691100050706     c                   EndIf
691200050706     c                   If        POM(wgiorno) = 'F'
691300050706     c                   Iter
691400050706     c                   EndIf
691500050706     c                   Leave
691600050706     c                   EndDo
691700050706     c                   Move      wdtaeur       wdtaiso
691800050706     c                   Move      wdtaiso       wdtameno2
691900030630      * più 1 gg. lavorativo
692000030701     c     *eur          Move      Viddfv        wdtaeur
692100030630     c                   Do        *Hival
692200030701     c                   Adddur    1:*d          wdtaeur
692300030701     c                   extrct    wdtaeur:*d    wgiorno
692400030701     c                   extrct    wdtaeur:*m    kmes
692500030701     c                   extrct    wdtaeur:*y    kann
692600030630     c     kcln1         Chain     Azcln01l
692700030630     c                   If        Not %Found(azcln01l)
692800030630     c                   Leave
692900030630     c                   EndIf
693000030630     c                   If        POM(wgiorno) = 'F'
693100030630     c                   Iter
693200030630     c                   EndIf
693300030630     c                   Leave
693400030630     c                   EndDo
693500030701     c                   Move      wdtaeur       wdtaiso
693600030701     c                   Move      wdtaiso       wdtapiu1
693700030630
693800030630     c                   EndSr
693900030630
694000951026     C**************************************************************************
694100951026     C*  SOTTOMETTO PGM DI CHIUSURA FV
694200951026     C**************************************************************************
694300951026     C     CHIUFV        BEGSR
694400951026     C**
694500951026     C** P R E P A R A Z I O N E   V O L U M I
694600951026     C**
694700120914     C     PA2CML        IFNE      ' '
694800120914     c***  pa2spv        ifne      *blanks
694900120914     c***  pa2spp        orne      *blanks
695000951026     C                   MOVEL     PARAM2        KPJBU
695100951026     C                   MOVEL     'LSB7'        KCOAZ
695200951026     C                   CALL      'BCH10'
695300951026     C                   PARM                    KPJBA
695400951026     C                   END
695500951026     C**
695600951026     C** C H I U S U R A   F O G L I   V I A G G I O
695700951026     C**
695800951026     C                   MOVEL     VIDNFV        PA2FGV
695900130603     c                   movel     'S'           pa2barcode
696000080228     C* CARICO L1 --> FILIALI GESTITE
696100080228     C                   CLEAR                   TRUL06DS
696200080228     C                   MOVE      '£1'          D06COD
696300080228     C                   MOVEL     SIMFEL        D06KEY
696400080228     C                   MOVE      'S'           D06KEY
696500080228     C                   MOVEL     fgvdfv        D06drf
696600080228     C                   MOVEL     TRUL06DS      KPJBU
696700080228     C*
696800080228     C                   CALL      'TRUL06R'
696900080228     C                   PARM                    KPJBA
697000080228     C                   MOVEL     KPJBU         TRUL06DS
697100080228     C                   MOVEA     LIN           L1ela
697200080228     c
697300951026     C                   MOVEL     PARAM2        KPJBU
697400080228     c
697500000824     C                   CLEAR                   KQEBI
697600000824     C                   CLEAR                   KQIEB
697700131105     c****               if        ilst4NEWC=' '
697800131105     C****               CALL      'BCH10'
697900131105     C****               PARM                    KPJBA
698000131105     c****               else
698100130603     C                   MOVEL     'LSU2'        KCOAZ
698200130712     C                   CALL      'BCH10'
698300130603     C                   PARM                    KPJBA
698400131105     c****               endif
698500951026     C*
698600951026     C* PREPARAZIONE FILE LEGAMI BOLLE
698700000824     C                   CLEAR                   KQEBI
698800000824     C                   CLEAR                   KQIEB
698900951026     C*
699000951026     C* LANCIO PGM DI AGGIORNAMENTO BOLLE
699100951026     C                   CLEAR                   KPJBU
699200951026     C*
699300951026     C* INVIO FILE TRASMISSIONE
699400951026     C                   MOVEL     'LSA3'        KCOAZ
699500070423     c                   clear                   parama3
699600070423     c                   move      simfel        pa3tfp
699700070423     c                   movel(p)  parama3       kpjbu
699800951026     C                   CALL      'BCH10'
699900951026     C                   PARM                    KPJBA
700000000824     C**
700100000824     C                   CLEAR                   KQEBI
700200000824     C                   CLEAR                   KQIEB
700300951026     C                   ENDSR
700400951130     C**************************************************************************
700500951130     C* CONTROLLO FV SE E' DA MANDARE IN SEDE
700600951130     C**************************************************************************
700700951130     C     CTRFVA        BEGSR
700800951130     C* SCRIVO FILE DI APPOGGIO DEL FOGLIO VIAGGIO SE:
700900951130     C*   - IL FOGLIO NON E' ABBINATO
701000951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
701100951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
701200951130     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
701300951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
701400951130     C*     C'E' IL RECORD IN TABELLA
701500951130    1C                   SELECT
701600951130     C     FGVTRN        WHENEQ    0
701700951130     C                   EXSR      WRTFVA
701800951130   X1C                   OTHER
701900951130     C*
702000951130     C                   MOVEL     'TV'          COD
702100951130     C                   MOVEL(P)  FGVTTR        KEY
702200951130     C     KTAB          CHAIN     TABEL                              83
702300951130    2C     *IN83         IFEQ      *OFF
702400951130     C     TBLFLG        ANDEQ     ' '
702500951130     C                   MOVEL     TBLUNI        DSTV
702600951130    2C     §TVUTC        IFEQ      'S'
702700951130     C                   EXSR      WRTFVA
702800951130    3C                   ENDIF
702900951130   X2C                   ELSE
703000951130     C                   EXSR      WRTFVA
703100951130    2C                   ENDIF
703200951130    1C                   ENDSL
703300951130     C                   ENDSR
703400970102     C**************************************************************************
703500970102     C* CONTROLLO FV DEFLUENZA PER ESTERO
703600970102     C**************************************************************************
703700970102     C     CTRDEF        BEGSR
703800970102     C* NON DEVE ESISTERE UN ALTRO FV DI DEFL. PER LA STESSA LNA ANCORA
703900970102     C* APERTO O GIA' CHIUSO MA CON LA STESSA DATA. POSSO PERO' ESISTER
704000970102     C*  COME FOGLI BIS
704100970102    1C     §TVDEF        IFEQ      'S'
704200060113     C     WEST          ANDEQ     'S'
704300970102     C     *IN56         ANDEQ     *OFF
704400970102     C     VIDBIS        ANDEQ     ' '
704500970102     C*
704600970102     C                   CLEAR                   WERR
704700970102     C                   CLEAR                   WAPE
704800970102     C* MI SALVO CAMPI DELLA DSTV
704900970102     C                   MOVEL     DSTV          DSTVS
705000970102     C* I° CICLO DI LETTURA:VERIFICO ESISTENZA DI ALTRO FV CON DATA =
705100970102     C     KFGV2         SETLL     FNFGV02L
705200970102     C     KFGV2         READE     FNFGV02L                               32
705300970102     C*
705400970102    2C     *IN32         DOWEQ     *OFF
705500970102     C     WERR          ANDEQ     *BLANKS
705600970102     C*
705700970102    3C     F2VNFV        IFNE      VIDNFV
705800970102     C     F2VSNP        ANDEQ     *BLANKS
705900970102     C     F2VATB        ANDEQ     ' '
706000000420     C*
706100020805     c                   eval      FGWnfv = F2Vnfv
706200020805     c                   eval      FGWlnp = F2Vlnp
706300020805     c                   exsr      CTR_FGW
706400000427     C                   MOVEL     FGWFF3        W2FF3
706500000427     C                   MOVEL     FGWFF4        W2FF4
706600970102     C* CONTROLLO SE ALMENO 1 LINEA DEL FOGLIO IN GESTIONE
706700970102     C*  E' PRESENTE SUL FV APPENA LETTO
706800970102     C                   Z-ADD     1             I
706900970102     C                   SETOFF                                       31
707000970102    4C     *IN31         DOWEQ     *OFF
707100000420     C     I             ANDLE     300
707200970102     C     CTL(I)        ANDGT     *ZEROS
707300000405     C                   MOVEL     CTL(I)        CODFIL
707400000405     C                   EXSR      CTRFIL
707500061027     c**                 if        §ogntw <> 'DPD'
707600970102     C     CTL(I)        LOOKUP    F2V                                    31
707700061027     C**                 ENDIF
707800970102     C  N31              ADD       1             I
707900970102    4C                   ENDDO
708000970102     C*
708100970102    4C     *IN31         IFEQ      *ON
708200970102     C                   MOVEL     'TV'          COD
708300970102     C                   MOVEL(P)  F2VTTR        KEY
708400970102     C     KTAB          CHAIN     TABEL                              32
708500970102     C  N32              MOVEL     TBLUNI        DSTV
708600970102     C   32              CLEAR                   DSTV
708700970102     C* SE DEFLUENZA E NON BIS --> ESCO DAL CICLO E SEGNALO ERRORE
708800970102    5C     §TVDEF        IFEQ      'S'
708900970102     C                   MOVE      '1'           WERR              1
709000970102     C                   MOVEL     CTL(I)        W003A
709100970102    5C                   END
709200970102    4C                   END
709300970102    3C                   END
709400970102     C*
709500970102    3C     WERR          IFEQ      *BLANKS
709600970102     C     KFGV2         READE     FNFGV02L                               32
709700970102    3C                   END
709800970102    2C                   ENDDO
709900970102     C*
710000970102    2C     WERR          IFEQ      *BLANKS
710100970102     C* II° CICLO DI LETTURA: VERIFICO ESISTENZA FV ANCORA APERTO
710200970102     C* (CON DATA DIVERSA, QUELLI CON LA STESSA DATA LI HO GIA' CERCATI
710300970102     C* NEL GIRO PRECEDENTE)
710400020805     C     SIMFEL        SETLL     FNFGV06L
710500020805     C     SIMFEL        READE     FNFGV06L                               32
710600970102     C*
710700970102    3C     *IN32         DOWEQ     *OFF
710800970102     C     WAPE          ANDEQ     *BLANKS
710900970102     C*
711000020805    4C     F6VDFV        IFNE      AMGDFV
711100020805     C     F6VNFV        ANDNE     VIDNFV
711200020805     C     F6VSNP        ANDEQ     ' '
711300000420     C*
711400020805     c                   eval      FGWnfv = F6Vnfv
711500020805     c                   eval      FGWlnp = F6Vlnp
711600020805     c                   exsr      CTR_FGW
711700020805     C                   MOVEL     FGWFF3        W6FF3
711800020805     C                   MOVEL     FGWFF4        W6FF4
711900970102     C* CONTROLLO SE ALMENO 1 LINEA DEL FOGLIO IN GESTIONE
712000970102     C*  E' PRESENTE SUL FV APPENA LETTO
712100970102     C                   Z-ADD     1             I
712200970102     C                   SETOFF                                       31
712300970102    5C     *IN31         DOWEQ     *OFF
712400000420     C     I             ANDLE     300
712500970102     C     CTL(I)        ANDGT     *ZEROS
712600000405     C                   MOVEL     CTL(I)        CODFIL
712700000405     C                   EXSR      CTRFIL
712800020805     C     CTL(I)        LOOKUP    F6V                                    31
712900970102     C  N31              ADD       1             I
713000970102    5C                   ENDDO
713100970102     C*
713200970102    5C     *IN31         IFEQ      *ON
713300970102     C                   MOVEL     'TV'          COD
713400020805     C                   MOVEL(P)  F6VTTR        KEY
713500970102     C     KTAB          CHAIN     TABEL                              32
713600970102     C  N32              MOVEL     TBLUNI        DSTV
713700970102     C   32              CLEAR                   DSTV
713800970102     C* SE DEFLUENZA ESCO DAL CICLO E SEGNALO ERRORE
713900970102    6C     §TVDEF        IFEQ      'S'
714000970102     C                   MOVE      '1'           WAPE              1
714100970102     C                   MOVEL     CTL(I)        W003A
714200970102    6C                   END
714300970102    5C                   END
714400970102    4C                   END
714500970102     C*
714600970102    4C     WAPE          IFEQ      *BLANKS
714700020805     C     SIMFEL        READE     FNFGV06L                               32
714800970102    4C                   ENDIF
714900970102    3C                   ENDDO
715000970102    2C                   ENDIF
715100970102     C* RIPRISTINO CAMPI DELLA DSTV
715200970102     C                   MOVEL     DSTVS         DSTV
715300970102     C* ERRORE
715400970102     C     WERR          IFNE      ' '
715500970102     C                   MOVEA     MSG(69)       K78
715600970102     C                   MOVEA     W003A         K78(58)
715700970102     C                   MOVEA     K78           VIDMSG
715800970102     C                   SETON                                        285090
715900970102     C                   ENDIF
716000970102     C     WAPE          IFNE      ' '
716100970102     C                   MOVEA     MSG(68)       K78
716200970102     C                   MOVEA     W003A         K78(58)
716300970102     C                   MOVEA     K78           VIDMSG
716400970102     C                   SETON                                        285090
716500970102     C                   ENDIF
716600970102     C*
716700970102    1C                   END
716800970102     C                   ENDSR
716900020805      **************************************************************************
717000020805      * AGGANCIO FILE FNFGW01L
717100020805      **************************************************************************
717200020805     C     CTR_FGW       BEGSR
717300020805      *
717400020805     C     K02FGW01      chain(N)  FNFGW000
717500020805      *
717600020910if  1c                   if        (not %found(FNFGW01L)
717700020910     c                              or  FGWatb <> *blanks)
717800020805     C                   clear                   FNFGW000
717900020805e   1C                   endif
718000020805      *
718100020805     C                   ENDSR
718200130412     C**************************************************************************
718300130412     C*  GESTIONE VIDEATA INSERIMENTO piombi
718400130412     C**************************************************************************
718500130412     C     GESD09        BEGSR
718600130415     c* Se il campo piombi è pieno imposto una riga per ogni costante
718700130415     c* spaziata
718800130415     c                   seton                                        35
718900130415     C                   WRITE     LSt4C08
719000130415     c                   setoff                                       3578
719100130415     c
719200130415     C                   Z-ADD     0             NRR8              4 0
719300130430    1c                   if        vidpmb<>*blanks
719400130415     c                   eval      Indx=1
719500130415     c                   eval      Parx=1
719600130430    2c                   dow       Indx>0 and parx<=48
719700130415     c                   eval      Indx=%scan(' ':vidpmb:Parx)
719800130415     C                   clear                   V8CNOT
719900130430    3c                   if        indx>0 and Indx=parx
720000130430     c                   eval      parx=parx+1
720100130430   x3c                   else
720200130430    4c                   if        Indx>0
720300130430     c*****              leave
720400130415     c
720500130415     c                   eval      v8cnot=%subst(vidpmb:parx:(indx-parx))
720600130415     c                   eval      parx=indx+1
720700130415     c                   else
720800130415     c                   eval      indx=48-parx+1
720900130415     c                   eval      v8cnot=%subst(vidpmb:parx:Indx)
721000130415     c                   clear                   indx
721100130430    4c                   endif
721200130415     c
721300130415     C                   ADD       1             NRr8
721400130415     C                   CLEAR                   V8CSCE
721500130415     C                   WRITE     LSt4S08
721600130415     C                   SETON                                        78
721700130430    3c                   endif
721800130430    2c                   enddo
721900130430    1c                   endif
722000130415     C                   Z-ADD     1             V8CNBR
722100130412     c
722200130412     c                   write     lst4T01
722300130412     c                   write     lst4d06
722400130412     c
722500130412     C     EMD09         TAG
722600130412     C                   WRITE     LSt4C08
722700130412     C                   EXFMT     LSt4D09
722800130502     C                   SETOFF                                       28
722900130412     c
723000130412     C* SCRIVO SUBFILE (SE CHI SONO GIA' IMMESSO NON LO RICARICO E NON
723100130412     C* DO ERRORE)
723200130415    1C     V9CNOT        IFNE      *BLANKS
723300130412     C     V9CNOT        LOOKUP    PIO2                                   31
723400130412     c
723500130415    2C     *IN31         IFEQ      *OFF
723600130430     c* Se non è tutto numerico --> errore
723700130430     c                   testn                   v9cnot               33
723800130430     c                   move      v9cnot        alfa1             1
723900130430     c* Errore se non numerico
724000130430     c                   if        not *in33 or alfa1<'0'
724100130430     c                   eval      vidmsg=msg(81)
724200130430     c                   seton                                        28
724300130430     c                   goto      emd09
724400130430     c                   endif
724500130514     c* Errore se è il sesto
724600130514     c                   if        nrr8 >=  5
724700130514     c                   eval      vidmsg=msg(113)
724800130514     c                   seton                                        28
724900130514     c                   goto      emd09
725000130514     c                   endif
725100130430     c
725200130412     C                   ADD       1             NRr8
725300130412     C                   Z-ADD     NRR8          V8CNBR
725400130524     C                   MOVEL(P)  V9CNOT        V8CNOT
725500130412     C                   CLEAR                   V8CSCE
725600130412     C                   WRITE     LSt4S08
725700130412     C                   SETON                                        78
725800130412     C                   ADD       1             I2                3 0
725900130412     C                   MOVE      V9CNOT        PIO2(I2)
726000130415    2C                   ENDIF
726100130412     C                   CLEAR                   V9CNOT
726200130415    1C                   ENDIF
726300130412     c
726400130412     C* PER F6 ESCO DALLA VIDEATA DI IMMISSIONE "CHI SONO"
726500130415    2C     *INKF         IFEQ      *OFF
726600130412     C                   GOTO      EMD09
726700130415    2C                   ENDIF
726800130412     c
726900130412     c* Quando esco scrivo il campo piombi
727000130415    2c                   if        nrr8>0
727100130412     c                   z-add     1             nrr8
727200130415     c                   clear                   vidpmb
727300130412     c     nrr8          chain     lst4s08
727400130412     c
727500130412     c                   dow       %found
727600130415     c                   if        v8cnot<>*blanks and v8csce<>'A'
727700130412     c
727800130412     c                   if        vidpmb<>*blanks
727900130412     c                   eval      vidpmb=%trim(vidpmb) +' '+v8cnot
728000130412     c                   else
728100130412     c                   eval      vidpmb=v8cnot
728200130412     c                   endif
728300130412     c                   endif
728400130412     c
728500130412     c                   add       1             nrr8
728600130412     c     nrr8          chain     lst4s08
728700130412     c                   enddo
728800130607     c                   endif
728900130412     c
729000130412     C                   ENDSR
729100130604     C********************************************************************
729200130604     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
729300130604     C********************************************************************
729400130605     C     sr_fnfge      BEGSR
729500130604
729600130610     c* ignoro se defluenza o inoltro
729700130610     c                   if        fgvttr<>'I' and wdef1a<>'S'
729800130610
729900130605     c* ciclo per ogni lna del foglio viaggio
730000130605     c                   z-add     fgvlna        wlna
730100130605     c                   z-add     fgvlna        wsca
730200130607     c                   select
730300130607     c* annullamento
730400130607     c                   when      *inkq
730500130607     c                   exsr      sr_fnfgeann
730600130607     c* immissione / ripristino
730700130607     c                   when      *in80   or *inke
730800130607     c                   exsr      sr_fnfgeimm
730900130607     c                   other
731000130607     c* manutenzione
731100130607     c                   exsr      sr_fnfgeman
731200130607     c                   endsl
731300130606     c                   z-add     1             iy
731400130606     C     Iy            DOWLE     299
731500130606     C     FFV(Iy)       IFGT      *ZEROS
731600130606     c                   move      ffv(iy)       wlna
731700130606     c                   move      flp(iy)       wsca
731800130607     c                   select
731900130607     c                   when      *inkq
732000130607     c                   exsr      sr_fnfgeann
732100130607     c                   when      *in80 or *inke
732200130607     c                   exsr      sr_fnfgeimm
732300130607     c                   other
732400130607     c                   exsr      sr_fnfgeman
732500130607     c                   endsl
732600130605
732700130605     c                   endif
732800130605
732900130607     c                   add       1             iy
733000130605     c                   enddo
733100130605
733200130605     c* per manutenzione verifico se eliminate lna
733300130607     c                   if        not *in80 and not *inkq and not *inke
733400130606     c                   z-add     1             iy
733500130607     C     Iy            DOWLE     300
733600130606     C     pffv(Iy)      IFGT      *ZEROS
733700130606     c                   move      pffv(iy)      wlna
733800130606     c                   move      pflp(iy)      wsca
733900130606     c                   exsr      sr_fnfgeann
734000130606
734100130606     c                   endif
734200130606
734300130607     c                   add       1             iy
734400130606     c                   enddo
734500130606     c                   endif
734600130610
734700130610     c                   endif
734800130604
734900130604     c                   endsr
735000130605     C********************************************************************
735100130605     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
735200130605     c*  IMMISSIONE
735300130605     C********************************************************************
735400130605     C     sr_fnfgeimm   BEGSR
735500130605     c     kfge          chain     fnfge01l
735600130605    1c                   if        not %found(fnfge01l)
735700130605     c* non trovato: scrivo
735800130605     c                   exsr      sr_wrtfge
735900130605   x1c                   else
736000130605     c* verifico lo scarico
736100130605     c                   exsr      sr_dtp
736200130605
736300130606    2c                   if        fgeflp=wsca
736400130605     c* stesso scarico --> vedo se aggiornare data/ora partenza
736500130610     c                   if        (wdtp>0 and (wdtp*10000)+whmp<(fgedtp*10000)+
736600130610     c                             fgehmp) or (wdtp>0 and fgedtp=0)
736700130605     c                   eval      fgedtp=wdtp
736800130605     c                   eval      fgehmp=whmp
736900130605     c                   update    fnfge000
737000130605    3c                   endif
737100130605   x2c                   else
737200130605     c* scarico diverso:  se diretto updato e sflaggo
737300130605     c                   exsr      sr_tfa
737400130605
737500130605    3c                   if        wlna=wsca or d55tfa=wsca
737600130605     c                   exsr      sr_updfge
737700130605   x3c                   else
737800130607     c* se no diretto updato se data/ora partenza <
737900130610    3c                   if        wlna<>fgeflp and  d55tfa<>fgeflp
738000130610     c                   if        (wdtp>0 and (wdtp*10000)+whmp<(fgedtp*10000)+
738100130610     c                             fgehmp) or (wdtp>0 and fgedtp=0)
738200130605     c                   exsr      sr_updfge
738300130605    4c                   endif
738400130610    4c                   endif
738500130605    3c                   endif
738600130605    2c                   endif
738700130605    1c                   endif
738800130611     c                   unlock    fnfge01l
738900130605     c                   endsr
739000130605     C********************************************************************
739100130605     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
739200130605     c*  MANUTENZIONE
739300130605     C********************************************************************
739400130605     C     sr_fnfgeman   BEGSR
739500130606     c* verifico se lna presente anche prima della modifica
739600130606     c                   eval      iw=%lookup(%EDITC(wlna:'X'):pffv)
739700130606    1c                   if        iw>0
739800130605     c* cambiato lo scarico
739900130606    2c                   if        %editc(wsca:'X')<>pflp(iw)
740000130605     c     kfge          chain     fnfge01l
740100130605     c                   exsr      sr_tfa
740200130605     c* il nuovo scarico e' un diretto
740300130605    3c                   if        wlna=wsca or d55tfa=wsca
740400130606    4c                   if        fgeflp<>wsca
740500130605     c                   exsr      sr_updfge
740600130606    4c                   endif
740700130605   x3c                   else
740800130606     c                   exsr      sr_dtp
740900130605     c* il nuovo scarico non è un diretto
741000130607     c* cerco l'esistenza di altro foglio viaggio partenza nella stessa data
741100130607     c* e per la stessa linea di arrivo dando la precedenza al diretto se presen
741200130607     c* te o in alternativa al foglio con la data/ora partenza inferiore
741300130606     c                   exsr      sr_cerlna
741400130607     c* se trovato altro foglio ed è diretto aggiorno con quello
741500130607     c* se trovato altro foglio e non è diretto aggiorno con quello che parte pr
741600130607     c* ima
741700130607     c* se non trovato altro foglio aggiorno con il nuovo scarico
741800130607     c                   select
741900130607     c                   when      wtroF='S' and wdir='S'
742000130607     c                   eval      wsca=a_sca
742100130607     c                   eval      wdtp=a_dtp
742200130607     c                   eval      whmp=a_hmp
742300130607
742400130610     c                   when      wtroF='S' and  wdtp>0 and
742500130610     c                             (a_dtp=0 or
742600130610     c                             ((Wdtp*10000)+whmp
742700130610     c                             < (a_dtp*10000)+a_hmp))
742800130610
742900130607     c                   when      wtroF='S'
743000130607     c                   eval      wsca=a_sca
743100130607     c                   eval      wdtp=a_dtp
743200130607     c                   eval      whmp=a_hmp
743300130607
743400130607     c                   endsl
743500130607     c                   exsr      sr_updfge
743600130605    3c                   endif
743700130611     c                   unlock    fnfge01l
743800130605    2c                   endif
743900130607     c                   clear                   pffv(iW)
744000130607     c                   clear                   pflp(iW)
744100130605   x1c                   else
744200130605     c* se lna nuova tratto come in immissione
744300130605     c                   exsr      sr_fnfgeimm
744400130605    1c                   endif
744500130607
744600130605     c                   endsr
744700130606     C********************************************************************
744800130606     C*  Aggiornamento file di appoggio per stampa etichette
744900130606     c*  ANNULLAMENTO
745000130606     C********************************************************************
745100130606     C     sr_fnfgeann   BEGSR
745200130606     c     kfge          chain     fnfge01l
745300130606    1c                   if        %found(fnfge01l)
745400130606     c* se stesso scarico
745500130606     c* cerco se esiste altro f.v.p nella stessa data con uguale lna
745600130606     c* dando la precedenza al diretto se presente
745700130606    2c                   if        fgeflp=wsca
745800130606     c                   exsr      sr_cerlna
745900130607     c                   if        wtroF='S'
746000130607     c                   eval      wsca=a_sca
746100130607     c                   eval      wdtp=a_dtp
746200130607     c                   eval      whmp=a_hmp
746300130607     c                   exsr      sr_updfge
746400130607     c                   else
746500130607     c* se non trovato altro foglio deleto
746600130607     c                   delete    fnfge000
746700130607     c                   endif
746800130606    2c                   endif
746900130611     c                   unlock    fnfge01l
747000130606    1c                   endif
747100130606     c                   endsr
747200130605     C********************************************************************
747300130605     C*  write su fnfge
747400130605     C********************************************************************
747500130605     C     sr_updfge     BEGSR
747600130607     c* slfaggo per la ristampa solo se cambiato lo scarico
747700130607     c                   if        fgeflp<>wsca
747800130607     c                   clear                   fgefst
747900130607     c                   endif
748000130606     c                   eval      fgeflp=wsca
748100130605     c                   eval      fgedtp=wdtp
748200130605     c                   eval      fgehmp=whmp
748300130605     c                   update    fnfge000
748400130605     c                   endsr
748500130605     C********************************************************************
748600130605     C*  update su fnfge
748700130605     C********************************************************************
748800130605     C     sr_wrtfge     BEGSR
748900130605     c                   clear                   fnfge000
749000130605     c                   eval      fgelnp=fgvlnp
749100130605     c                   eval      fgedfv=fgvdfv
749200130605     c                   eval      fgelna=wlna
749300130606     c                   eval      fgeflp=wsca
749400130605     c* data/ora partenza reale o teorica
749500130605     c                   if        fgvdtp>0
749600130605     c                   eval      fgedtp=fgvdtp
749700130605     c                   eval      fgehmp=fgvhmp
749800130605     c                   else
749900130605     c                   eval      kepa='P'
750000130605     c                   clear                   wtfa
750100130605     c                   eval      ktdh='T'
750200130605     c     kfv22         chain(n)  fnfv201l
750300130605     c                   if        %found(fnfv201l)
750400130605     c                   z-add     fv2dpa        fgedtp
750500130605     c                   z-add     fv2hpa        fgehmp
750600130605     c                   endif
750700130605     c                   endif
750800130605     c                   write     fnfge000
750900130605     c                   endsr
751000130605     C********************************************************************
751100130605     C*  determino dtat/ora partenza del foglio viaggio
751200130605     C********************************************************************
751300130605     C     sr_dtp        BEGSR
751400130605     c                   clear                   wdtp
751500130605     c                   clear                   whmp
751600130605     c* la data/ora partenza è la reale se presente altrimenti prendo la
751700130605     c* teorica
751800130605     c                   if        fgvdtp>0
751900130605     c                   eval      wdtp=fgvdtp
752000130605     c                   eval      whmp=fgvhmp
752100130605     c                   else
752200130605     c                   eval      kepa='P'
752300130605     c                   clear                   wtfa
752400130605     c                   eval      ktdh='T'
752500130605     c     kfv22         chain(n)  fnfv201l
752600130605     c                   if        %found(fnfv201l)
752700130605     c                   z-add     fv2dpa        wdtp
752800130605     c                   z-add     fv2hpa        whmp
752900130605     c                   endif
753000130605     c                   endif
753100130605     c                   endsr
753200130605     C********************************************************************
753300130605     C*  determino terminal di arrivo della linea di arrivo
753400130605     C********************************************************************
753500130605     C     sr_tfa        BEGSR
753600130605     c                   clear                   fnlv55ds
753700130605     c                   eval      d55lin=wlna
753800130605     c                   eval      d55lnp=fgvlnp
753900130605     c                   eval      d55drf=fgvdfv
754000130605     c                   eval      d55tpt='A'
754100130605     C                   CALL      'FNLV55R'
754200130605     C                   PARM                    FNLV55DS
754300130605     c                   endsr
754400130606     C********************************************************************
754500130606     C*  Ricerca altro foglio
754600130606     C********************************************************************
754700130606     C     sr_cerlna     BEGSR
754800130606     c                   clear                   wtroF
754900130607     c                   clear                   a_sca
755000130607     c                   clear                   a_dtp
755100130606     c                   clear                   a_hmp
755200130606     c                   clear                   x_dtp
755300130606     c                   clear                   x_hmp
755400130606     c                   clear                   wdir
755500130606     c* verifico l'esistenza di un altro foglio viaggio nella stessa data e
755600130607     c* per la stessa lna
755700130606     c     kfgv02        setll     fnfgv02l
755800130606     c     kfgv02        reade     fnfgv02l
755900130606    1c                   dow       not %eof(fnfgv02l) and wdir=' '
756000130606     c* escludo foglio se lo stesso che ho appena manutenzionato
756100130610     c* escludo anche se foglio annullato
756200130610    2c                   if        f2vnfv<>fgvnfv and f2vatb<>'*'
756300130606     c                   eval      FGWnfv = F2Vnfv
756400130606     c                   eval      FGWlnp = F2Vlnp
756500130606     c                   exsr      CTR_FGW
756600130606     C                   MOVEL     FGWFF3        W2FF3
756700130606     C                   MOVEL     FGWFF4        W2FF4
756800130606     C                   MOVEL     FGWFl3        W2Fl3
756900130606     C                   MOVEL     FGWFl4        W2Fl4
757000130606     c                   eval      f2p(1)=%editc(f2vlna:'X')
757100130606     c                   eval      ij=%lookup(%EDITC(wlna:'X'):f2v)
757200130607    3c***                if        ij>0 and f2p(ij)=%editc(fgeflp:'X')
757300130607     c                   if        ij>0
757400130606     c                   eval      wtroF='S'
757500130606    4c                   if        %editc(wlna:'X')=f2p(ij) or
757600130606    4c                             %editc(d55tfa:'X')=f2p(ij)
757700130606     c                   eval      wdir='S'
757800130606    4c                   endif
757900130606    4c                   if        f2vdtp>0
758000130606     c                   eval      x_dtp=f2vdtp
758100130606     c                   eval      x_hmp=f2vhmp
758200130606   x4c                   else
758300130606     c                   eval      kepa='P'
758400130606     c                   clear                   wtfa
758500130606     c                   eval      ktdh='T'
758600130606     c     kfv22a        chain(n)  fnfv201l
758700130606    5c                   if        %found(fnfv201l)
758800130606     c                   z-add     fv2dpa        x_dtp
758900130606     c                   z-add     fv2hpa        x_hmp
759000130606    5c                   endif
759100130606    4c                   endif
759200130606    4c                   if        a_dtp=0 or (x_dtp>0 and (x_dtp<a_dtp or
759300130607     c                             x_dtp=a_dtp and x_hmp<a_dtp)) or wdir='S'
759400130606     c                   eval      a_dtp=x_dtp
759500130606     c                   eval      a_hmp=x_hmp
759600130607     c                   eval      a_sca=%dec(f2p(ij):3:0)
759700130606    4c                   endif
759800130606     c
759900130607     c                   endif
760000130606    2c                   endif
760100130606     c     kfgv02        reade     fnfgv02l
760200130606    1c                   enddo
760300130606     c                   endsr
760400130510     c*-------------------------------------------------------------------
760500130510     c     INV_email     BEGSR
760600130510     c
760700130510     c                   if        not %open(prtemail)
760800130510     C                   EXSR      SR_OPENPRTF
760900130510     C                   endif
761000130510     c
761100130510     c                   exsr      Routend
761200130510     c
761300130510     c                   ENDSR
761400130510
761500130510      /free
761600130510       //--------------------------------------------------------------
761700130510       //?Override al file di stampa per impostarvi i dati per
761800130510       //?  l'invio via e-mail   +   stampa inizio e-mail
761900130510       //--------------------------------------------------------------
762000130510       BEGSR sr_OpenPrtF;
762100130510
762200130510         // Override al file di stampa per impostarvi i dati per
762300130510         // l'invio via e-mail
762400130510           exsr sr_Override;
762500130510
762600130510           open PrtEMAIL;
762700130510
762800130510         // Stampa una testata se NON è richiesta la e-mail
762900130510           IF  §MRAdreg =  *blank;
763000130510             O_testo = JobUser + ' - ' + SDSpgm
763100130510                     + ' - ' + %editc( *date : 'Y' )
763200130510                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
763300130510             except PrtDet;
763400130510             clear O_testo;
763500130510             except PrtDet;
763600130510           ENDIF;
763700130510
763800130510         // Stampa testo iniziale
763900130510
764000130510           if fgvsnp='B'  ;
764100130510           desbis='BIS'  ;
764200130510           else  ;
764300130510           clear desbis  ;
764400130510           endif  ;
764500130510           O_testo = 'Foglio Viaggio :  '+%editc(fgvlnp:'X')+'  '+
764600130510           %editc(fgvnfv:'2')+'  per '+ %editc(fgvlna:'X')+'-'+deslna +
764700130510           '   ' + desbis+ '  del  ' + %editw(viddfv:'  /  /    ') ;
764800130510           except PrtDet;
764900130510
765000130510           clear O_testo;
765100130510           except PrtDet;
765200130510           O_testo = 'Traino: ' +%editc(fgvtrn:'2')   ;
765300130510           except PrtDet;
765400130510
765500130510         // Stampa una riga vuota
765600130510           clear O_testo;
765700130510           except PrtDet;
765800130510
765900130510         // Stampa trazionista
766000130521           KTLPDR=fgvpdr  ;
766100130521           CHAIN  (ktltip: ktlpdr) tntlZ01L   ;
766200130521           if %found(tntlz01l) ;
766300130521            O_testo = 'Trazionista : '+%editc(fgvpdr:'X')+'-'+tlzrsc    ;
766400130521           else  ;
766500130521            O_testo = 'Trazionista : '+%editc(fgvpdr:'X')+'-'+fgvdpd    ;
766600130521           endif  ;
766700130510           except PrtDet;
766800130510
766900130510         // Stampa una riga vuota
767000130510           clear O_testo;
767100130510           except PrtDet;
767200130510
767300130510          if winviamail='D' or winviamail='E'  ;
767400130510          if *in80  ;
767500130510           O_testo = 'CONDUCENTE    non accreditato:  '+ viddcn  ;
767600130510           else  ;
767700130510           O_testo = 'CONDUCENTE    non accreditato modificato da: '+
767800130510           fgvpmb    ;
767900130510           o_testo=%trim(o_testo) + '  a: ' + viddcn  ;
768000130510           endif  ;
768100130510
768200130510           except PrtDet;
768300130510         // Stampa una riga vuota
768400130510           clear O_testo;
768500130510           except PrtDet;
768600130510           endif  ;
768700130510
768800130510          if winviamail='M' or winviamail='E'  ;
768900130510          if *in80  ;
769000130510           O_testo = 'TARGA MOTRICE non accreditata:  '+ vidtrm  ;
769100130510           else  ;
769200130510           O_testo = 'TARGA MOTRICE non accreditata modificata da: '+
769300130510           fgvtrm + '  a: ' + vidtrm  ;
769400130510           endif  ;
769500130510
769600130510           except PrtDet;
769700130510         // Stampa una riga vuota
769800130510           clear O_testo;
769900130510           except PrtDet;
770000130510           endif  ;
770100130510
770200130510       ENDSR;
770300130510
770400130510       //--------------------------------------------------------------
770500130510       //?Override al file di stampa per impostarvi i dati per
770600130510       //?  l'invio via e-mail   +   stampa inizio e-mail
770700130510       //--------------------------------------------------------------
770800130510       BEGSR sr_Override;
770900130510
771000130510         reset TRTCM1ds;
771100130510
771200130510         IF  §MRAdreg <> *blank;
771300130510           §CM1mitt = %trim(§cm1mitt);
771400130510           %subst(§cm1mitt:4:3)=%editc(fgvlnp:'X') ;
771500130510           §CM1dst  = %trim(§MRADDEST) ;
771600130510
771700130510           §CM1tips = §MRAdreg;
771800130510           §CM1po   = %editc(simfel:'X');
771900130510           §CM1var  = '*OBJM*' + 'Richiesta autorizzazione al carico'   ;
772000130510           §CM1idp  = §MRAdidpro;
772100130510           Qcmd = C_CmdOvrPrtF
772200130510                + ' outq(' + %trim(§MRAdoutqi) + ')'
772300130510                + ' usrdfndta(''' + TRTCM1ds + ''')';
772400130510         ELSE;
772500130510           Qcmd = C_CmdOvrPrtF;
772600130510         ENDIF;
772700130510
772800130510         callp(e) qCmdExc(Qcmd : %size(Qcmd));
772900130510
773000130510       ENDSR;
773100130510
773200130510       //--------------------------------------------------------------
773300130510       //?Operazioni finali.
773400130510       //--------------------------------------------------------------
773500130510       BEGSR RoutEnd;
773600130510
773700130510         if %open(PrtEMAIL);
773800130510
773900130510         //?Chiusura dello spool?
774000130510           clear O_testo;
774100130510           except PrtDet;
774200130510           O_testo = '***    Fine    ***';
774300130510           except PrtDet;
774400130510
774500130510           close PrtEMAIL;
774600130510
774700130510         //?Eliminazione overflow?
774800130510           Qcmd = C_CmdDltOvr;
774900130510           callp(e) qCmdExc(Qcmd : %size(Qcmd));
775000130510
775100130510         endif;
775200130510
775300130510       ENDSR;
775400130510
775500130510      /end-free
775600130510     o* ---------------------------------------------------------------
775700130510     oPrtEMAIL  e            PRTdet      1
775800130510     o                       O_testo
775900130510
776000020805      **************************************************************************
776100960302** SCHIERA MSG - MESSAGGI DI ERRORE
776200951023Immettere un foglio di viaggio esistente                                       1
776300951026Il foglio viaggio e' GIA' stato CHIUSO!!!                                      2
776400951026Non e' possibile annullare il foglio viaggio: presenti SPUNTE  !!              6
776500951026Data errata                                                                    4
776600951130Impossibile modificare la data FV se abbinato:enter per forzare e non abbinare 5
776700951023Non e' possibile annullare il foglio viaggio: presenti BOLLE borderizzate      6
776800980430Data non inferiore di 1 gg. e non superiore al limite imposto
776900940912Immettere codice filiale esistente                                             8
777000951026Immettere codice TRAZIONISTA esistente                                         9
777100951023Immettere il codice TRAZIONISTA o la ragione sociale                          10
777200940912Immettere almeno la targa della motrice                                       11
777300951024Impossibile esporre l'importo del FV: se immesso il traino NON SARA' abbinato 12
777400940912Immettere orario valido                                                       13
777500940912Immettere filiale di scarico valida                                           14
777600940912Immettere anche la filiale                                                    15
777700940912Filiale gia' presente                                                         16
777800940912Filiale di scarico obbligatoria                                               17
777900951026Immettere il nome del Conducente                                              18
778000130429
778100951023Immettere un numero traino esistente                                          20
778200951023Traino errato:scaduto, ancora da decorrere o non esiste la filiale come carico21
778300130502Traino non utilizzabile in questo giorno: F6 per forzare e non abbinare       22
778400951017Impossibile effettuare la copia da un traino di tipo "Affluenza"              23
778500950901Immettere l'importo pattuito                                                  24
778600951027Se copia da traino non si puo'modificare il tipo traino:enter x forz.e non abb25
778700950901Immettere tipo automezzo valido                                               26
778800951024Per questo trazionista/tipo automezzo non esiste listino: enter per forzare   27
778900951024Lin.arrivo finale errata:non e' fil.scarico del traino -> IMPOSSIBILE LA COPIA28
779000951025Lin.Arrivo immessa non e'quella finale del traino:copia dati senza abbinamento
779100951017Filiale errata: non c'e' nulla da caricare                                    30
779200130502ATTENZIONE:esiste gia'F.Viaggio  xxxxx  abbinato a questo traino. F6  Forza   31
779300951024Lin.arrivo finale presente 2 volte: come "A" e come "R"->TELEFONARE IN SEDE!  32
779400951025L'importo non corrisponde a quello presente nel listino traino.Enter x forzare33
779500951025FV abbinato al traino xxxxxxx ora inesistente o scaduto: il FV sara' sbinato  34
779600951025Impossibile eliminare la linea  xxx  : PRESENTI BOLLE BORDERIZZATE!!
779700951025Impossibile eliminare la linea  xxx  : PRESENTI SPUNTE!!!
779800951025Impossibile variare la fil.scarico xxx della linea xxx:PRESENTI BOLLE BORDERIZ37
779900960126DATA PARTENZA errata o obbligatoria                                           38
780000951026Immettere la descrizione dei PIOMBI                                           39
780100951026Il foglio viaggio e'  ANNULLATO!!!!                                           40
780200130429
780300130429
780400951027Manca il numeratore dei Fogli Viaggio: TELEFONARE IN SEDE!!!!                 43
780500110531ATTENZIONE!! Il Foglio Viaggio non sarà abbinato al traino. F6 per forzare
780600951027Data non inferiore di 1 gg. e non superiore a 5 rispetto alla data F.Viaggio  45
780700951027Tipo traino errato                                                            46
780800951027Per un foglio DEFLUENZA o LOCALE,la filiale deve essere un 2 livello in arrivo47
780900951027Per un foglio DEFLUENZA lo scarico deve essere = alla filiale indicata        48
781000951031Per un foglio LOCALE, lo scarico deve essere una filiale dell'area o se stessa49
781100951027Per un foglio DEFLUENZA la filiale non puo' essere = alla linea partenza FV   50
781200951030Non trovati trazionisti per chiave alfabetica richiesta                       51
781300951103Non c'e' più ritardo/anticipo: se si vuole si può eliminare la causale        52
781400951103Immettere causale valida                                                      53
781500951108Se immessa causale "G" occorre inserire descrizione ritardo/ant. premendo F21 54
781600960126Per inserire la descrizione ritardo occore immettere la causale               55
781700951107Presente ritardo/anticipo nella partenza del foglio: immettere la causale     56
781800960126Presente descrizione ritardo: immettere la causale o eliminare la descrizione 57
781900960126Presente descrizione ritardo: premere F6 per conferma o eliminare descrizione 58
782000960126Eliminata Descrizione Ritardo: confermare con F6 o reinserire la descrizione  59
782100951120Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli viaggio     60
782200951120Lin.arrivo finale errata:non e' fil.scarico del traino. Enter per forzare     61
782300951120Se inserita la causale ritardo/anticipo occorre immettere la data partenza    62
782400960122La data Foglio Viaggio e' un giorno festivo: enter per forzare                63
782500960201Una volta inserite  data / ora Partenza non possono essere piu' eliminate     64
782600960130Se copia da traino non si puo'modificare la categoria: enter x forz.e non abb.65
782700130430Per questa linea di arrivo TIPO TRAINO deve essere LOCALE DEFLUENZA o INOLTRO 66
782800960910Foglio viaggio attualmente impegnato da un altro lavoro:ATTENDERE e riprovare 67
782900970102Esiste gia'un foglio aperto DEFLUEN.per la linea ESTERA  xxx  :aprire come BIS
783000970102Esiste gia'un foglio DEFL nel giorno per la linea ESTERA xxx :aprire come BIS
783100970102Le filiali  xxx  /  xxx  hanno stesso ter.arrivo ma non la stessa fil.scarico
783200990514Per un foglio DEFLUENZA per l'ESTERO devono essere presenti solo linee ESTERO
783300970526Non e' possibile modificare la data del foglio viaggio: presenti SPUNTE  !!   72
783400990514Non e' possibile chiudere un foglio DEFLUENZA per l'ESTERO                    73
783500000316Per filiale di arrivo di 2° livello XXX inserire anche il suo terminal di arr.74
783600010621Immettere il numero del cellulare del Conducente                              75
783700010704Numero cellulare errato                                                       76
783800000605Foglio in gestione ad altro p.o.                                              77
783900010920La divisa non corrisponde a quella presente nel listino traino.Enter x forzare78
784000010920Divisa errata                                                                 79
784100010920Divisa non ammessa per i traini                                               80
784200130430Il codice del piombo deve essere formato da 8 numeri                          81
784300130429
784400130429
784500010920Non ammessi importi con decimali per la divisa indicata                       84
784600011001Se modificata la divisa ridigitare il relativo importo                        85
784700011012Immettere la divisa                                                           86
784800020919P.O. finale di arrivo errato (deve coincidere con il P.O. gestione)           87
784900130430
785000130430
785100050706Impossibile riaprire il foglio:troppo "vecchio".                              90
785200080910Manutenzione non ammessa per il foglio viaggio richiesto                      91
785300110512Punto di scambio erraro o scaduto                                             92
785400110512Se presenti dati punto di scambio, immettere anche il codice                  93
785500110512Data punto di scambio errata                                                  94
785600110512Data punto di scambio non deve essere minore della data foglio viaggio        95
785700110512Se copia da traino non si può modificare Punto Scambio: ENTER x forz.e non abb96
785800110609Immettere la filiale finale di ritorno autista                                 7
785900110531Aggiunta fil.Ritorno finale Autista: il traino di scambio viene eliminato     98
786000110610Data e ora scambio obbligatori se presente numero traino di scambio           99
786100110513Numero traino di scambio errato o scaduto                                     10
786200110530Aggiunta Fil.Ritorno Finale Autista: il traino verrà sbinato. Enter x forzare 01
786300110530Errori nei dati relativi al punto di Scambio. F2 per correggere               02
786400110530Traino BIS: eliminato punti di scambio copiato da traino                      03
786500110531Se cambiato punto di scambio, immettere la Fil.Finale Ritorno Autista         04
786600120208Impossibile l'annullamento:foglio già elaborato in sede. Telef.ufficio Traini  5
786700130430Per attivare la ricerca del conducente, immettere il trazionista               6
786800130510CONDUCENTE non accreditato per il trazionista:enter per forzare e inviare Mail 7
786900130424Per attivare la ricerca della targa motrice, immettere il trazionista          8
787000130510Targa MOTRICE non accreditata per il trazionista:enter x forzare e inviareMail 9
787100130509La targa motrice è priva della DATA DI CONTROLLO : enter per forzare          10
787200130510CONDUCENTE non accreditato per il trazionista: enter per forzare              11
787300130510Targa MOTRICE non accreditata per il trazionista: enter per forzare           12
787400130514Non è possibile inserire con lettura barcode più di 5 piombi                  13
787500130729L'orario di partenza del traino non può essere maggiore dell'orario di scambio14
787600030701** SCHIERA SBI - MOTIVI DI SBINAMENTO FV
787700960302Immesso l'IMPORTO DEL FV su traino     1
787800960302xxxxxxx,  che  non  deve  esporlo      2
787900960304Traino xxxxxxx non previsto nel  gi    3
788000960304orno  di  xxxxxxxxx                    4
788100960302Esiste il FV xxxxx già abbinato al     5
788200960302traino xxxxxxx                         6
788300960304Nel traino xxxxxxx e' stato modific    7
788400960304ato  il TIPO da x in x-xxxxxxxxxxxx    8
788500960302Abbinato a traino xxxxxxx ma modifi    9
788600960302cata la data foglio viaggio           10
788700960304Nel traino xxxxxxx e' stata modific   11
788800960304ata  la categoria in XXX              12
788900960302FV creato SENZA copia da Traino       13
789000960302XXXXXXX N.TRAINO sbinato dal fv       14
789100960302Variate le LINEE al Traino  XXXXXXX   15
789200970103variato Arrivo FINALE da xxx a xxx    16
789300970114 aggiunte  xxx xxx xxx xxx xxx xxx    17
789400970114 eliminate xxx xxx xxx xxx xxx xxx    18
789500970114 modificato scarico xxx xxx xxx xxx   19
789600110512Nel traino xxxxxxx modificato Punto   20
789700110512 Scambio da xxxxxxxxxx a xxxxxxxxxx   21
789800110530Nel traino xxxxxxx aggiunta FilRito   22
789900110530rnoAut xxx per lo scambio             23
790000951002** SCHIERA GFV
790100951002BCDEFGHILMNOPQRSTUVZ
790200960302** SCHIERA TES - TESTATE
790300130417 ***  FOGLI  VIAGGIO   PARTENZA  ***
790400130628 *CHIUSURA FOGLI VIAGGIO DA SPUNTE *
790500951026 ***   BORDERIZZAZIONE   BOLLE   ***
790600960302** SCHIERA GIO - GIORNI DELLA SETTINANA
790700960302LUNEDI
790800960302MARTEDI
790900960302MERCOLEDI
791000960302GIOVEDI
791100960302VENERDI
791200960302SABATO
791300960302DOMENICA
