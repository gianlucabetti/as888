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
093800130729     c                   if        viddtp>0  and viddgs>0
093900130729     c                   if        (amgdgs<amgdtp)or
094000130729     c                             (amgdgs=amgdtp and vidhhs<=vidhmp)
094100130729     C                   MOVEL     MSG(114)      VIDMSG
094200130729     C                   SETON                                        902846
094300130729    2C                   ENDIF
094400130729     c                   endif
094500110530     c
094600110530    2c                   endif
094700110530     c
094800960123    1C                   ENDIF
094900940915     C   KD
095000951024     COR 90              GOTO      FOR02
095100951026     C*
095200951026     C* SE VENGO DA BORDERO' REGISTRO I DATI ED ESCO
095300130315     C   01ilst4GES      IFEQ      'B'
095400951026     C                   EXSR      REGIS
095500960304     C   90              GOTO      FOR02
095600951026     C                   GOTO      FINE
095700951026     C                   ENDIF
095800960123     C**
095900951025     C* SE IMMESSO NUMERO TRAINO RIEMPO I CAMPI SFL COPIANDOLI DAL TRAI
096000960129     C* SOLO LA PRIMA VOLTA O SE HO MODIFICATO LA LINEA DI ARRIVO
096100951026     C   80WCALNA        IFEQ      'S'
096200951026     C*
096300951031     C     VIDTRN        IFGT      0
096400951017     C                   EXSR      RIETRS
096500951025     C                   ELSE
096600951025     C* CARICO SFL VUOTO IN IMMISIONE SENZA COPIA
096700951025     C                   CLEAR                   FFV
096800951025     C                   CLEAR                   FLP
096900951025     C                   EXSR      CARSFL
097000951017     C                   ENDIF
097100951026     C                   CLEAR                   WCALNA
097200951026     C                   ENDIF
097300130430     c* e premuto F6 faccio subito i controllo --> se OK    non emetto
097400130430     c*  videata del SFL
097500130430     c                   if        *inkf and not *in02
097600130430     C                   EXSR      CTRD03
097700130502     c                   endif
097800130502     c
097900130502     c                   if        not *in90 and *inkf
098000130430     C                   EXSR      REGIS
098100130430     C   90              GOTO      FOR02
098200130430     C* SE CHIUSURA FV LANCIO L'ELABORAZIONE
098300130430     C     ilst4GES      IFEQ      'C'
098400130430     C                   EXSR      CHIUFV
098500130430     C                   ENDIF
098600130430     c
098700130430     c                   goto      fine
098800130430     c
098900130430     C                   ENDIF
099000951017     C*
099100900515      *          =============
099200951025     C     FOR03         TAG
099300900515      *          =============
099400000828      *
099500000828      * SE TIPO TRAINO = DEFLUENZA/INOLTRO DISABILITO F10
099600000828      *
099700000828     C     VIDTTR        IFEQ      'I'
099800000828     C     WDEF1A        OREQ      'S'
099900000828     C                   SETON                                        61
100000000828     C                   ENDIF
100100000828      *
100200130315     C                   WRITE     lst4T01
100300130315     C                   WRITE     lst4Z03
100400130315     C                   EXFMT     lst4C03
100500960126     C                   MOVEL     '3'           WRECOR            1
100600970102     C                   SETOFF                                       289050
100700970102     C                   SETOFF                                       51
100800960123     C* F12 - RITORNO
100900951024     C   KL              GOTO      FOR02
101000960123     C**
101100960123     C* SE VENGO DA INTERROGAZIONE FV SINGOLA ESCO
101200130315     C     ilst4GES      IFEQ      'N'
101300960123     C                   GOTO      FINE
101400960123     C                   ENDIF
101500960123     C**
101600951027     C* F7  - MANUTENZIONE LINEE DEL FV
101700951027     C   KG              SETOFF                                       02
101800951027     C   KG              EXSR      CARSFL
101900951027     C   KG              GOTO      FOR03
102000951027      *
102100900515      *  CONTROLLI SFL
102200951027     C* LI ESEGUO SOLO SE POSSO MODIFICARE LE LINEE DEL FV
102300951027     C     *IN02         IFEQ      *OFF
102400951025     C                   EXSR      CTRD03
102500951027     C                   ENDIF
102600000825      *
102700000828      * F10=INSERIMENTO TERMINAL DI ARRIVO
102800000828      *
102900000828     C                   SETOFF                                       66
103000000825     C  N90
103100000825     CAN 67              EXSR      INSTER
103200000825      *
103300951026     C  NKF
103400000828     COR 66
103500000828     COR 90              GOTO      FOR03
103600951026     C*
103700951025      * F6  - REGISTRAZIONE DATI
103800951026     C     *INKF         IFEQ      *ON
103900951026     C                   EXSR      REGIS
104000951027     C   90              GOTO      FOR03
104100951026     C* SE CHIUSURA FV LANCIO L'ELABORAZIONE
104200130315     C     ilst4GES      IFEQ      'C'
104300951026     C                   EXSR      CHIUFV
104400951026     C                   ENDIF
104500951026     C*
104600951026     C                   ENDIF
104700900515      *
104800000000     C     FINE          TAG
104900960123     C* CHIUDO PGM TNTL35
105000960123     C                   MOVEL     'C'           D35TLA
105100020805     C                   MOVEL     TNTL35DS      KPJBU
105200960123     C                   CALL      'TNTL35R'
105300960123     C                   PARM                    KPJBA
105400960129     C*
105500960129     C* SOLO SE NON RICHIAMATO CHIUDO I FILE
105600130430     C***  *IN20         IFEQ      *OFF
105700130430     C**+                MOVEL     'C'           D15TLA
105800130430     C**+                MOVEL     FNLV15DS      KPJBU
105900130430     C***                CALL      'FNLV15R'
106000130430     C***                PARM                    KPJBA
106100130430     C***                ENDIF
106200960129     C*
106300960129     C* SOTTOMETTO TRASMISSIONE FV PER LA SEDE
106400960129     C     WLSA4         IFNE      ' '
106500960129     C                   RESET                   PARAM4
106600960129     C                   MOVEL     'M'           W004A             4
106700051121     C                   MOVE      SIMFEL        w004a
106800960129     C                   MOVEL     W004A         PA4MBD
106900960129     C                   MOVEL     SIMFEL        PA4FEL
107000960129     C                   MOVEL     'L'           PA4TLA
107100130419     C                   MOVEL     VIDfgs        PA4LNP
107200960129     C                   MOVEL     PARAM4        KPJBU
107300960129     C                   MOVEL     'LSA4'        KCOAZ
107400960129     C                   CALL      'BCH10'
107500960129     C                   PARM                    KPJBA
107600960129     C                   ENDIF
107700971203     C*
107800971203     C                   MOVEL     'C'           D55TLA
107900971203     C                   CALL      'FNLV55R'
108000020805     C                   PARM                    FNLV55DS
108100020805     c*
108200020805     c                   clear                   TRUL33DS
108300020805     c                   eval      I33tla = 'C'
108400020805     c                   movel(p)  TRUL33DS      KPJBU
108500020805     c                   call      'TRUL33R'
108600020805     c                   parm                    KPJBA
108700080314
108800080314     C                   CLEAR                   TRUL06DS
108900080314     C                   MOVE      '£1'          D06COD
109000080314     c                   movel     'C'           d06tla
109100080314     C                   MOVEL     SIMFEL        D06KEY
109200080314     C                   MOVEL     TRUL06DS      KPJBU
109300080314     C*
109400080314     C                   CALL      'TRUL06R'
109500080314     C                   PARM                    KPJBA
109600080314     c                   clear                   kpjbu
109700960129     C*
109800130315     C   20              MOVEL     fnlst4ds      KPJBU
109900960129     C*
110000000000     C                   SETON                                        LR
110100960129     C*
110200900515      *-----------------------------------------------------***********
110300900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
110400900515      *-----------------------------------------------------***********
110500900515     C     PULIZ         BEGSR
110600900515      *
110700910703     C                   SETOFF                                       995256
110800951120     C                   SETOFF                                       061618
110900000522     C                   SETOFF                                       430204
111000130503     C                   SETOFF                                       081061
111100951017     C                   CLEAR                   WFORZ
111200951017     C                   CLEAR                   WFORZ2
111300951017     C                   CLEAR                   WFORZ3
111400951024     C                   CLEAR                   WFORZ4
111500951025     C                   CLEAR                   WFORZ5
111600951027     C                   CLEAR                   WFORZ6
111700951027     C                   CLEAR                   WFORZ7
111800951120     C                   CLEAR                   WFORZ8
111900951130     C                   CLEAR                   WFORZ9
112000960122     C                   CLEAR                   WFORZB
112100960126     C                   CLEAR                   WFORZC
112200960130     C                   CLEAR                   WFORZD
112300110512     C                   CLEAR                   WFORzsc
112400110530     C                   CLEAR                   WFORzlnar
112500130424     C                   CLEAR                   WFORzD1
112600130424     C                   CLEAR                   WFORzM1
112700951025     C*
112800130424     C                   CLEAR                   VIDFGS
112900130424     C                   CLEAR                   DESFGS
113000951025     C                   CLEAR                   NFP
113100951025     C                   CLEAR                   NFA
113200951025     C                   CLEAR                   WTFP
113300951025     C                   CLEAR                   WTFA
113400951025     C                   CLEAR                   WDDE
113500951025     C                   CLEAR                   WAN
113600951025     C                   CLEAR                   WRI
113700951025     C                   CLEAR                   WTFM
113800951025     C                   CLEAR                   WNFM
113900951123     C                   CLEAR                   WFMFES
114000951025     C                   CLEAR                   WANDDE
114100150902     C                   CLEAR                   WANtfa
114200951025     C                   CLEAR                   WRIDDE
114300951106     C                   CLEAR                   SAVRTC
114400960126     C                   CLEAR                   SAVDTP
114500960126     C                   CLEAR                   SAVHMP
114600960422     C                   CLEAR                   WINNFV
114700960129     C*
114800960129     C                   CLEAR                   WOBLPD
114900960129     C                   CLEAR                   WTLPDR
115000061116     C                   CLEAR                   okestero
115100130510     C                   CLEAR                   winviamail
115200951026     C* KEY PER LETTURA SULLE SPUNTE
115300951026     C                   MOVEL     1             KNPG
115400951026     C                   MOVEL     SIMFEL        KFGS
115500951017     C*
115600951026     C                   MOVEL     'S'           WCALNA            1
115700900515     C                   Z-ADD     0             VIDNFV
115800951031     C                   CLEAR                   COPTRN
115900951025     C* CAMPI DI SALVATAGGIO
116000951031     C                   CLEAR                   SAVTRN
116100951027     C                   MOVEL     'AAA'         SAVLNA
116200951025     C                   Z-ADD     0             SAVDFV
116300951025     C                   Z-ADD     0             SAVIPT
116400010920     C                   CLEAR                   SAVVIP
116500951027     C                   CLEAR                   SAVTTR
116600940909     C*
116700940912     C                   TIME                    WTIME
116800940912     C                   MOVE      WTIME         WDATE
116900080919     c* data meno 30 gg
117000080919     c     *eur          move      wdate         wdtaiso
117100080919     c                   subdur    30:*days      wdtaiso
117200080919     c                   move      wdtaiso       dm30
117300080919
117400960122     C                   MOVE      WTIME         KANN
117500940912     C                   MOVEL     WDATE         VIDDFV
117600030701     c                   ExSr      Sr_MinMax
117700951024     C                   CLEAR                   VIDTRN
117800050706     c* mi salvo data - 2 che mi serve per eventuale riapertura foglio
117900050706     c                   eval      woggimeno2=wdtameno2
118000940909     C*
118100130419     C                   MOVE      SIMFEL        VIDfgs
118200951024     C*
118300951031     C                   CLEAR                   VIDAN1
118400951031     C                   CLEAR                   VIDAN2
118500960123     C                   CLEAR                   VIDBIS
118600940913     C                   MOVEL     *BLANKS       VIDPDR
118700130424     C                   MOVEL     *BLANKS       SAVPDR
118800010927     C                   MOVEL     §GEDCN        VIDVIP
118900900515     C                   Z-ADD     0             VIDIPT
119000900515     C                   MOVE      *ZEROS        VIDLNA
119100951024     C                   MOVEL     *BLANKS       DESLNA
119200900515     C                   Z-ADD     0             VIDHMP
119300951103     C                   CLEAR                   VIDRTC
119400960208     C                   CLEAR                   DESRT1
119500960208     C                   CLEAR                   DESRT2
119600951025     C                   MOVEL     *BLANKS       VIDDPD
119700900606     C                   MOVEL     *BLANKS       VIDDCN
119800130424     C                   MOVEL     *BLANKS       savDCN
119900010622     C                   MOVEL     *BLANKS       VIDCNC
120000900606     C                   MOVEL     *BLANKS       VIDTMZ
120100950901     C                   MOVEL     *BLANKS       DESTMZ
120200130502     C                   MOVEL     *BLANKS       DESTMZ2
120300950901     C                   MOVEL     *BLANKS       VIDTTR
120400960206     C                   MOVEL     *BLANKS       DESTTR
120500900515     C                   MOVEL     *BLANKS       VIDTRM
120600130424     C                   MOVEL     *BLANKS       savtrm
120700900515     C                   MOVEL     *BLANKS       VIDTRR
120800940909     C                   MOVEL     *BLANKS       VIDPMB
120900900606     C                   MOVEL     *BLANKS       VIDNT1
121000900606     C                   MOVEL     *BLANKS       VIDNT2
121100910405     C                   MOVEL     'N'           VIDSNS
121200951023     C                   CLEAR                   VIDDTP
121300951024     C                   CLEAR                   VIDHMP
121400960122     C                   CLEAR                   VIDDTT
121500960122     C                   CLEAR                   VIDHMT
121600110511     c
121700110531     c                   clear                   oldpsc
121800110531     c                   clear                   savpsc
121900110531     c                   clear                   vidpsc
122000110511     c                   clear                   despsc
122100110512     c                   clear                   viddgs
122200110511     c                   clear                   vidhhs
122300110511     c                   clear                   vidtrns
122400110530     c                   clear                   vidlnar
122500110530     c                   clear                   deslnar
122600110601     c                   clear                   deslnar2
122700110513     c                   clear                   destrns
122800110531     c                   clear                   despind
122900110531     c                   clear                   desploc
123000940913     C*
123100940913     C* PULISCO LE SCHIERE DEI CONTROLLI
123200940913     C                   MOVEA     *BLANKS       LAB
123300940913     C                   MOVEA     *BLANKS       LAS
123400951130     C                   MOVEA     *BLANKS       TFV
123500951130     C                   MOVEA     *BLANKS       TLP
123600960126     C                   CLEAR                   AMGDFV
123700960126     C                   CLEAR                   AMGDTT
123800960126     C                   CLEAR                   AMGDTP
123900951027     C* PROTEZIONE DELLE LINEE DI ARRIVO
124000130315     C     ilst4GES      IFNE      ' '
124100130315     C     ilst4GES      ANDNE     'M'
124200130419     C     ilst4GES      ANDNE     'P'
124300130419     C     ilst4GES      ANDNE     'A'
124400951027     C                   SETON                                        02
124500951027     C                   ENDIF
124600900515      *
124700900514     C                   ENDSR
124800951023      *-----------------------------------------------------***********
124900951023      * CONTROLLI FORMATO 1 PER INSERIMENTO NUOVO FV        *  CTRINS *
125000951023      *-----------------------------------------------------***********
125100951023     C     CTRINS        BEGSR
125200951026     C                   SETON                                        80
125300951023     C*
125400951023     C* SE IMMESSO CONTROLLO NUMERO TRAINO DA CUI COPIARE
125500951031    2C     COPTRN        IFGT      *ZEROS
125600130319     C*
125700130319     C* DATA FOGLIO VIAGGIO  E CALCOLO LA DATA FV -1 E + 5 GG
125800130319     c                   clear                   wd02              1
125900130319     C                   EXSR      CALDAT
126000130319     C   90              GOTO      ENDINS
126100130319     c
126200951023     C                   EXSR      CTRTRN
126300130419     c
126400130419     c* non dovrebbero esserci errori sul traino ma per sicurezza controllo
126500130419     c                   if        *in90
126600130419     C                   GOTO      ENDINS
126700130419     c                   endif
126800951218     C*
126900951218     C* CODICE TRAZIONISTA OBBLIGATORIO SE C'E' LA COPIA DA TRAINO
127000960129     C*  E SE IN ANAGRAFICA C'E' PIU' DI UN PADRONCINO O E' PRESENTE
127100960129     C*  SOLO QUELLO GENERICO - SOLO PER IMMISSIONE
127200130315    3c                   if        ilst4ges='A' and ilst4old='S'
127300130315    4C     VIDPDR        IFLE      *ZEROS
127400960129     C     WOBLPD        ANDEQ     'S'
127500951218     C                   MOVEL     MSG(9)        VIDMSG
127600130419     C                   SETON                                        2890
127700951218     C                   GOTO      ENDINS
127800130315    3c                   ENDIF
127900130315    2c                   ENDIF
128000951023     C*
128100951023     C* SE TUTTO OK RIEMPO I CAMPI DELLA VIDEATA COPIANDOLI DAL TRAINO
128200130315    3c                   if        ilst4ges='A'
128300130315     C                   EXSR      RIETRV
128400130315    3C                   ENDIF
128500130315    2C                   ENDIF
128600951023     C*
128700951023     C* REPERISCO ULTIMO NUMERO FOGLIO VIAGGIO IMMESSO
128800130319    2C***  *INKJ         IFEQ      *ON
128900020805     c                   reset                   TRUL33DS
129000020805     c                   eval      I33po1 = SIMfel
129100020805     c                   movel(p)  TRUL33DS      KPJBU
129200020805     c                   call      'TRUL33R'
129300020805     c                   parm                    KPJBA
129400020805     c                   movel     KPJBU         TRUL33DS
129500020805    3C                   select
129600020805     C* Numeratore FV non trovato
129700020805     C                   when      O33err =  01
129800020805     C                   MOVEL     MSG(43)       VIDMSG
129900130419     C                   SETON                                        2890
130000020805     C                   GOTO      ENDINS
130100020805     C* Numeratore FV allocato
130200020805     C                   when      O33err =  02
130300951120     C                   MOVEL     MSG(60)       VIDMSG
130400130419     C                   SETON                                        2890
130500951120     C                   GOTO      ENDINS
130600020805     C                   when      O33err <> *zeros
130700020805     C                   movel     O33msg        VIDmsg
130800130419     C                   seton                                        2890
130900020805     C                   goto      ENDINS
131000020805    3C                   endsl
131100020805     C                   z-add     O33nrf        VIDnfv
131200960126     C*
131300020805     C     K02FGV01      SETLL     FNFGV01L                               83
131400960126    5C     *IN83         DOWEQ     *ON
131500020805     c                   reset                   TRUL33DS
131600020805     c                   eval      I33po1 = SIMfel
131700020805     c                   movel(p)  TRUL33DS      KPJBU
131800020805     c                   call      'TRUL33R'
131900020805     c                   parm                    KPJBA
132000020805     c                   movel     KPJBU         TRUL33DS
132100020805    6C                   if        O33err <> *zeros
132200020805     c                   movel     O33msg        VIDmsg
132300130419     c                   seton                                        2890
132400020805     c                   goto      ENDINS
132500020805    6c                   endif
132600020805     c                   z-add     O33nrf        VIDnfv
132700020805     c     K02FGV01      SETLL     FNFGV01L                               83
132800960126    5C                   ENDDO
132900951023     C*
133000951023     C* 80 ON  - IMMISSIONE
133100130319     C****               SETON                                        80
133200130319    2C***                ENDIF
133300951023     C*
133400960126     C     ENDINS        ENDSR
133500951003     C*-----------------------------------------------------***********
133600951017     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
133700951003     C*-----------------------------------------------------***********
133800951003     C     CTRTRN        BEGSR
133900951003     C                   SETOFF                                       90
134000951003     C*
134100951024    1C     COPTRN        IFNE      SAVTRN
134200951003     C*
134300951023     C                   CLEAR                   W001
134400951023     C                   CLEAR                   WFORZ
134500951023     C                   CLEAR                   WFORZ2
134600951024     C                   CLEAR                   WFORZ3
134700951120     C                   CLEAR                   WFORZ8
134800951025     C     VIDTRN        SETLL     TNTLT01L
134900951025     C     VIDTRN        READE     TNTLT01L                               83
135000951003     C*
135100951013    2C     *IN83         DOWEQ     *OFF
135200951003    3C     TLTATB        IFEQ      ' '
135300951023     C* CONTROLLO CHE IL TRAINO CHE SI VUOLE COPIARE NON SIA UN TRAINO
135400951023     C*   DI TIPO "A-AFFLUENZA"
135500951023    4C     TLTTTR        IFEQ      'A'
135600951023     C                   MOVEL     MSG(23)       VIDMSG
135700130419     C                   SETON                                        9028
135800951023     C                   GOTO      ENDTRN
135900951023    4C                   ENDIF
136000951023     C*
136100951003     C                   MOVEL     '1'           W001              1
136200951023     C* ALMENO UN RECORD TROVATO --> ESCO DALLA LETTURA
136300951013     C                   SETON                                        83
136400951003    3C                   ENDIF
136500951003     C*
136600951025     C  N83VIDTRN        READE     TNTLT01L                               83
136700951003    2C                   ENDDO
136800951003     C*
136900951003     C* W001 = " " ---> IL N.TRAINO IMMESSO E' INESISTENTE O ANNULLATO
137000951003    2C     W001          IFEQ      ' '
137100951003     C                   MOVEL     MSG(20)       VIDMSG
137200130419     C                   SETON                                        9028
137300951003     C                   GOTO      ENDTRN
137400951003    2C                   ENDIF
137500951003     C*
137600951023     C* DETERMINO IL PROGRESSIVO DECORRENTE DELLA FERMATA
137700951023     C*  VEDO ANCHE SE C'E' A/R OPPURE NO
137800951023     C                   EXSR      CTRDEC
137900951003     C*
138000951023     C* WAN E WRI=" "-> NUMERO TRAINO IMMESSO ERRATO PERCHE':
138100951013     C*                  - SIMFEL NON E' UNA FERMATA DEL TRAINO
138200951013     C*                  - SIMFEL NON E' UNA FERMATA DI CARICO
138300951013     C*                  - IL TRAINO E' DA DECORRERE O SCADUTO
138400951023    2C                   SELECT
138500951023    2C     WAN           WHENEQ    ' '
138600951023     C     WRI           ANDEQ     ' '
138700951006     C                   MOVEL     MSG(21)       VIDMSG
138800130419     C                   SETON                                        9028
138900951006     C                   GOTO      ENDTRN
139000951023     C*
139100951023     C* SE IMMESSI ENTRAMBI --> CHIEDO LA LINEA DI ARRIVO
139200951023    2C     WAN           WHENEQ    'S'
139300951023     C     WRI           ANDEQ     'S'
139400951123     C                   MOVEL     'A'           WFMFES
139500951023     C*
139600951023    3C     *IN06         IFEQ      *OFF
139700951023     C                   SETON                                        0690
139800951024     C                   MOVEL     COPTRN        SAVTRN
139900951023     C                   GOTO      ENDTRN
140000951023    3C                   ENDIF
140100951023     C*
140200951023   X2C                   OTHER
140300951023     C*
140400951023     C* MEMORIZZO IL TIPO FERMATA DA ELABORARE
140500951023    3C     WAN           IFEQ      'S'
140600951123     C                   MOVEL     'A'           WFMFES
140700951025     C                   MOVEL     'A'           WTFM
140800951025     C                   Z-ADD     WANDDE        WDDE
140900951023     C                   ELSE
141000951123     C                   MOVEL     'R'           WFMFES
141100951025     C                   MOVEL     'R'           WTFM
141200951025     C                   Z-ADD     WRIDDE        WDDE
141300951023    3C                   ENDIF
141400951023     C* SALVO IL NUMERO TRAINO IMMESSO
141500951024     C                   MOVEL     COPTRN        SAVTRN
141600951023     C*
141700951023     C* SE E' SOLO A O R --> FACCIO SPARIRE LA LINEA DI ARRIVO SE C'E'
141800951023    3C     *IN06         IFEQ      *ON
141900951023     C                   SETOFF                                       06
142000951023     C                   SETON                                          90
142100951023     C                   GOTO      ENDTRN
142200951023    3C                   ENDIF
142300951023    2C                   ENDSL
142400951023    1C                   ENDIF
142500951005     C*
142600951009     C* 06 ON  - E' RICHIESTA LA LNA FINALE IN 1° VIDEATA PERCHE' SI
142700951009     C*   TRATTA DI UN TRAINO DI TIPO "C-CHIUSO"
142800951009    1C     *IN06         IFEQ      *ON
142900951017     C*
143000951025     C     VIDLNA        IFNE      SAVLNA
143100951025     C                   CLEAR                   WTFM
143200951017     C                   EXSR      CTRLNA
143300980115     C* SALVO LA LINEA DI ARRIVO
143400980115     C                   MOVEL     VIDLNA        SAVLNA
143500980115     C                   MOVEL     'S'           WCALNA            1
143600951017     C   90              GOTO      ENDTRN
143700951025     C                   ENDIF
143800951013     C*
143900951009   X1C                   ELSE
144000951013     C* 06 OFF - NON E' RICHIESTA LA LNA FINALE IN 1° VIDEATA QUINDI IL
144100951003     C*
144200951024     C* CERCO LA LINEA FINALE ARRIVO TRAINO PER SIMFEL
144300951024     C*   UTILIZZANDO I MC: INIZIO CON LA LINEA FINALE TRAINO
144400951027     C                   SETON                                        85
144500951027     C     WTFM          IFEQ      'A'
144600951025     C                   CLEAR                   TRNTFA
144700000522     C                   MOVE      VIDFGS        WTFP
144800150902     C****               MOVEL     TLTTFA        WTFA
144900150902     C                   MOVEL     wantfa        WTFA
145000951024     C                   EXSR      REDTLM
145100951027     C                   ENDIF
145200951024     C*
145300951025     C     *IN85         IFEQ      *OFF
145400150902     C***                Z-ADD     TLTTFA        TRNTFA
145500150902     C                   Z-ADD     wantfa        TRNTFA
145600951025     C                   ELSE
145700951027     C     WTFM          IFEQ      'A'
145800951024     C                   Z-ADD     NFP(1)        WNFM
145900951027     C                   ELSE
146000951027     C                   Z-ADD     NFP(2)        WNFM
146100951027     C                   ENDIF
146200951024     C                   EXSR      RICLNA
146300951024     C                   ENDIF
146400951005     C*
146500951025     C* TRNTFA=0  ---> TRAINO NON UTILIZZABILE PERCHE' NON TROVATI I
146600951024     C*                METRICUBI PER NESSUNA FILIALE FERMATA
146700951025    2C     TRNTFA        IFEQ      0
146800951024     C                   MOVEL     MSG(21)       VIDMSG
146900130419     C                   SETON                                        9028
147000951005     C                   GOTO      ENDTRN
147100951005    2C                   ENDIF
147200951025     C* SE NON IMMESSA LNA FINALE IN 1 VIDEATA CARICO TUTTE LE FERMATE
147300951025     C*  DI SCARICO PER QUEL TIPO FERMATA
147400951025     C                   Z-ADD     999           WNFM
147500951009     C*
147600951009    1C                   ENDIF
147700951027     C* CHAINO LA TESTATA
147800951027     C     KTLR11        CHAIN     TNTLT01L                           83
147900951027     C*
148000951024     C* CONTROLLO SE IL TRAINO SI PUO' APRIRE IN QUESTO GIORNO
148100951024    1C     WFORZ         IFEQ      ' '
148200951024     C* REPERISCO IL GIORNO DELLA SETTIMANA (1=LUN ...)
148300951024     C                   CALL      'XGIOSE1'
148400951024     C                   PARM                    GGMMAA            6
148500951024     C                   PARM                    GIOSET            1
148600951024     C*
148700951024     C                   MOVEA     TLTGSE        GSE
148800951024     C     GIOSET        LOOKUP    GSE                                    83
148900951024    2C     *IN83         IFEQ      *OFF
149000130502     C                   MOVEL     'S'           WFORZ             1
149100130502     c
149200130502    4c                   if        ilst4ges<>'A' or iolst4for<>'F6 '
149300951024     C                   MOVEL     MSG(22)       VIDMSG
149400130419     C                   SETON                                        9028
149500960302     C                   MOVEL     GIOSET        SAVSET            1
149600130502     c                   eval      iolst4for='022'
149700951024     C                   GOTO      ENDTRN
149800130502     c                   endif
149900130502     c
150000951024    2C                   ENDIF
150100951024    1C                   ENDIF
150200951123     C*
150300951123     C* IN UNA DATA SOLO UN FOGLIO VIAGGIO PUO' ESSERE ABBINATO AD
150400951123     C*   UN NUMERO TRAINO A O R : ALTRIMENTI FORZO MA NON ABBINO IL FV
150500951123     C                   EXSR      CTRDOP
150600951123     C   90              GOTO      ENDTRN
150700960129     C*
150800960129     C* VERIFICO SE NEI LISTINI C'E' SOLO UN TRAZIONISTA O PIU':
150900960129     C*  SE CE N'E' UNO SOLO, NON E' OBBLIGATORIO IN 1 VIDEATA
151000960129     C                   CLEAR                   WOBLPD
151100960129     C                   CLEAR                   WTLPDR
151200010920     C     KTLR11        SETLL     TITLL01L
151300010920     C     KTLR11        READE     TITLL01L                               83
151400960129    1C     *IN83         DOWEQ     *OFF
151500960129    2C     TLLATB        IFEQ      ' '
151600960129     C     TLLPDR        ANDNE     9999999
151700960129    3C     WTLPDR        IFEQ      0
151800960129     C                   MOVEL     TLLPDR        WTLPDR
151900960129   X3C                   ELSE
152000960129     C                   MOVEL     'S'           WOBLPD            1
152100960129     C                   SETON                                        83
152200960129    3C                   ENDIF
152300960129    2C                   ENDIF
152400960129     C*
152500010920     C  N83KTLR11        READE     TITLL01L                               83
152600960129    1C                   ENDDO
152700960129     C*
152800960129     C* SE C'E' SOLO CODICE 9999999--> OBBLIGATORIO L'INSERIMENTO
152900960129     C     WTLPDR        IFEQ      0
153000960129     C                   MOVEL     'S'           WOBLPD
153100960129     C                   ENDIF
153200951009     C*
153300951025     C     VIDLNA        IFLE      *ZEROS
153400951025     C                   MOVEL     TRNTFA        VIDLNA
153500951024     C                   ENDIF
153600951009     C     ENDTRN        ENDSR
153700951123     C*---------------------------------------------------------------*
153800951123     C* IN UNA DATA SOLO UN FOGLIO VIAGGIO PUO' ESSERE ABBINATO AD
153900951123     C*   UN NUMERO TRAINO A O R : ALTRIMENTI FORZO MA NON ABBINO IL FV
154000951123     C*---------------------------------------------------------------*
154100951123     C     CTRDOP        BEGSR
154200130429     c* Se apertura fogli, non controllo se ppremuto F6
154300130429     c
154400951123    1C     WFORZ2        IFEQ      ' '
154500130429     c*
154600951123     C     KFGV2         SETLL     FNFGV02L
154700951123     C     KFGV2         READE     FNFGV02L                               83
154800951123     C*
154900951123    2C     *IN83         DOWEQ     *OFF
155000961216    3C     F2VATB        IFEQ      ' '
155100961216     C     F2VTRN        ANDEQ     VIDTRN
155200961216     C     F2VFRQ        ANDEQ     WTFM
155300000522     C                   MOVEL     DSFGW         SVFGW
155400020805     c                   eval      FGWnfv = F2Vnfv
155500020805     c                   eval      FGWlnp = F2Vlnp
155600020805     c                   exsr      CTR_FGW
155700130315     c
155800951123     C                   MOVEL     'S'           WFORZ2            1
155900130429     c
156000130429    4c                   if        ilst4ges<>'A' or iolst4for<>'F6 '
156100130429     C                   SETON                                        9083
156200130429     C                   SETON                                        28
156300951123     C                   MOVEA     MSG(31)       K78
156400961216     C                   MOVEL     F2VNFV        W005A             5
156500951123     C                   MOVEA     W005A         K15(11)
156600951123     C                   Z-ADD     11            Z
156700951123     C                   EXSR      ABBL
156800951123     C                   MOVEA     K15(11)       K78(34)
156900960302     C                   MOVEA     K15(11)       SAV005            5
157000951123     C                   MOVEA     K78           VIDMSG
157100130429     c                   eval      iolst4for='031'
157200130429    4C                   ENDIF
157300130429    3C                   ENDIF
157400000522     C                   MOVEL     SVFGW         DSFGW
157500951123     C*
157600951123     C  N83KFGV2         READE     FNFGV02L                               83
157700951123    2C                   ENDDO
157800130429    1C                   ENDIF
157900951123     C                   ENDSR
158000951023     C*---------------------------------------------------------------*
158100951023     C* CONTROLLO LA DATA DI DECORRENZA DELLA FERMATA E SE C'E' A/R   *
158200951023     C* DETERMINO LA DATA TRAINO
158300951023     C*----------------------------------------------------------------
158400951023     C     CTRDEC        BEGSR
158500951023     C*
158600951024     C                   CLEAR                   NFP
158700951024     C                   CLEAR                   NFA
158800951024     C                   CLEAR                   W001
158900951023     C                   CLEAR                   WANDDE
159000150902     C                   CLEAR                   WANtfa
159100951023     C                   CLEAR                   WAN
159200951023     C                   CLEAR                   WRIDDE
159300951023     C                   CLEAR                   WRI
159400000522     C                   MOVE      VIDFGS        WFIL
159500951025     C                   Z-ADD     1             X
159600951025     C* IN IMMISSIONE FACCIO 2 TENTATIVI SE SERVE
159700960122     C* IN VARIAZIONE CERCO SUBITO CON CIO'CHE HO MEMORIZZATO IN FGVFRQ
159800951025     C     *IN80         IFEQ      *ON
159900951023     C                   MOVEL     'A'           WTFM
160000951025     C                   ENDIF
160100951023     C*
160200951023     C* ESEGUO IL CICLO 2 VOLTE: PRIMA PER "ANDATA" E POI PER "RITORNO"
160300951024    1C                   DO        2             X                 2 0
160400951023     C     KTLR          SETLL     TNTLR02L
160500951023     C     KTLR          READE     TNTLR02L                               83
160600951023     C*
160700951023    2C     *IN83         DOWEQ     *OFF
160800951023     C*
160900951023    3C     TLRATB        IFEQ      ' '
161000951023     C     TLRCAR        ANDEQ     'S'
161100951023     C*
161200951023     C* GIORNO =A ->LA DATA TRAINO CORRISPONDE ALLA DATA FV
161300951023    4C     TLRGFV        IFEQ      'A'
161400951024     C                   Z-ADD     AMGDFV        WDTN
161500951024     C                   MOVEL     VIDDFV        GGMMAA
161600951024     C                   MOVE      VIDDFV        W0020
161700951024     C                   MOVE      W0020         GGMMAA
161800951023   X4C                   ELSE
161900951023     C* GIORNO<>A ->SOTTRAGGO ALLA DATA FOGLIO I GIORNI DI APERTURA FV
162000951026     C                   Z-ADD     1             C                 3 0
162100951027     C     TLRGFV        LOOKUP    GFV(C)                                 31
162200951025     C     WGGDFV        SUB       C             GI8TGI
162300951023     C                   CALL      'XSRGI8'
162400951023     C                   PARM                    WGIDAT
162500951023     C                   Z-ADD     GI8INV        WDTN
162600951024     C                   MOVEL     GI8DAT        GGMMAA
162700951024     C                   MOVE      GI8DAT        W0020             2 0
162800951024     C                   MOVE      W0020         GGMMAA
162900951023    4C                   ENDIF
163000951023     C*
163100951023     C* E' DECORRENTE -- > ESCO DALLA LETTURA
163200951023    4C     WDTN          IFGE      TLRDDE
163300951024     C* CHAIN SULLA TESTA PER VEDERE SE E' ANCHE DENTRO LA SCADENZA
163400951024     C                   Z-ADD     TLRDDE        WDDE
163500951027     C     KTLR11        CHAIN     TNTLT01L                           84
163600951024    5C     *IN84         IFEQ      *OFF
163700951024     C     WDTN          ANDLE     TLTDSC
163800951023     C*                    SETON                     83
163900951023     C* MEMORIZZO SE E' ANDATA + DATA DECORRENZA
164000951024    6C     WTFM          IFEQ      'A'
164100951023     C                   MOVEL     'S'           WAN               1
164200951023     C                   Z-ADD     TLRDDE        WANDDE
164300150902     C                   Z-ADD     TLttfa        WANtfa
164400951024     C                   Z-ADD     TLRNFM        NFP(1)
164500951023     C* SE IL TRAINO E' CHIUSO VERIFICO ANCHE SE E' RITORNO
164600951025     C* SE SONO IN VARIAZIONE ESCO
164700951024    7C     TLTTTR        IFNE      'C'
164800951025     C     *IN80         OREQ      *OFF
164900951023     C                   Z-ADD     3             X
165000951024    7C                   ENDIF
165100951023     C*
165200951024   X6C                   ELSE
165300951023     C* MEMORIZZO SE E' RITORNO
165400951023     C                   MOVEL     'S'           WRI               1
165500951023     C                   Z-ADD     TLRDDE        WRIDDE
165600951024     C                   Z-ADD     TLRNFM        NFP(2)
165700951024    6C                   ENDIF
165800951024    5C                   ENDIF
165900951023    4C                   ENDIF
166000951023    3C                   ENDIF
166100951023     C*
166200951023     C  N83KTLR          READE     TNTLR02L                               83
166300951023    2C                   ENDDO
166400951023     C*
166500951025     C   80              MOVEL     'R'           WTFM
166600951023    1C                   ENDDO
166700951013     C*
166800951023     C                   ENDSR
166900951017     C*-----------------------------------------------------***********
167000951017     C* CONTROLLO LINEA ARRIVO FINALE                       * CTRLNA  *
167100951017     C*-----------------------------------------------------***********
167200951017     C     CTRLNA        BEGSR
167300951017     C*
167400951017     C* RICERCA FILIALE FINALE DI ARRIVO
167500951027     C     '?'           SCAN      VIDLNA                                 45
167600951027    1C     *IN45         IFEQ      *ON
167700951024     C                   EXSR      CASD19
167800951017     C                   MOVEL     §COD          VIDLNA
167900951017     C                   MOVEL     §§DES         DESLNA
168000951017     C                   SETON                                        90
168100951017     C                   GOTO      ENDCTR
168200951017    1C                   ENDIF
168300951017     C*
168400951017     C* CONTROLLO CHE SIA UNA FILIALE DELL'ORGANIGRAMMA
168500951017     C                   MOVEL     *BLANKS       DESLNA
168600020213     c                   clear                   okestero
168700951025     C                   MOVE      VIDLNA        CODFIL
168800951025     C                   EXSR      CTRFIL
168900951017     C*
169000951026    1C     *IN34         IFEQ      *ON
169100951017     C                   MOVEL     *ALL'?'       DESLNA
169200951017     C                   MOVEL     MSG(8)        VIDMSG
169300951026     C                   SETON                                        284590
169400951017     C                   GOTO      ENDCTR
169500951017   X1C                   ELSE
169600951017     C                   MOVEL     ORGDES        DESLNA
169700020213     c                   eval      okestero = esteroedpd
169800020919      *
169900020919      * controllo che coincida con P.O. Gestione
170000020919     c                   clear                   FNLV55DS
170100020919     c                   movel     '6'           D55tpt
170200020919     c                   move      CODfil        D55lin
170300020919     c                   z-add     AMGdfv        D55drf
170400020919     c                   call      'FNLV55R'
170500020919     c                   parm                    FNLV55DS
170600020919if  2c                   if        D55tfa <> CODfil
170700020919     c                   movel     MSG(87)       VIDMSG
170800020919     c                   seton                                        459028
170900020919e   2c                   endif
171000020919      *
171100960226     C* SE NON C'E ECCEZIONE CARICO
171200960226     C                   MOVEL     VIDLNA        KTFA
171300960226     C                   MOVEL     'A'           KEPA
171400970102     C                   EXSR      RICEC2
171500971203     C                   MOVEA     TFE           TFEL
171600970102     C*
171700971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
171800971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
171900020805     C                   CLEAR                   FNLV55DS
172000971203     C                   MOVEL     'A'           D55TPT
172100971203     C                   MOVE      VIDLNA        D55LIN
172200000522     C                   MOVE      VIDFGS        D55LNP
172300971203     C                   MOVE      AMGDFV        D55DRF
172400971203     C                   CALL      'FNLV55R'
172500020805     C                   PARM                    FNLV55DS
172600971203     C                   MOVE      D55TFA        LNAFE0
172700960226     C*
172800951017    1C                   ENDIF
172900951017     C*
173000951017     C* S O L O   S E   P R O V E N G O   D A   C O P I A   T R A I N O
173100951025    1C     VIDTRN        IFGT      0
173200951017     C*
173300951017     C* CONTROLLO CHE LA LINEA FINALE DI ARRIVO IMMESSA ESISTA NEL
173400951024     C*   FILE DEI MC
173500951017     C                   CLEAR                   W001
173600951024     C                   MOVEL     VIDLNA        WFIL
173700000522     C                   MOVE      VIDFGS        WTFP
173800951024     C                   MOVEL     VIDLNA        WTFA
173900951024     C* tento PRIMA COME ANDATA
174000951027    2C     *IN80         IFEQ      *ON
174100951025     C     WTFM          OREQ      'A'
174200951024     C                   MOVEL     'A'           WTFM
174300951024     C                   Z-ADD     WANDDE        WDDE
174400951024     C                   EXSR      LEGFER
174500951025    2C                   ENDIF
174600951024     C*
174700951024     C* POI COME RITORNO
174800951027    2C     *IN80         IFEQ      *ON
174900951025     C     WTFM          OREQ      'R'
175000951024     C                   MOVEL     'R'           WTFM
175100951024     C                   Z-ADD     WRIDDE        WDDE
175200951024     C                   EXSR      LEGFER
175300951025    2C                   ENDIF
175400951017     C*
175500951025    2C                   SELECT
175600951120     C* NON ESISTE --> ERRORE PER IMMISSIONE, X VARIAZ. ERRORE FORZABIL
175700951024     C     W001          WHENEQ    ' '
175800951120     C     *IN80         IFEQ      *ON
175900951017     C                   MOVEL     MSG(28)       VIDMSG
176000951026     C                   SETON                                        904528
176100951017     C                   GOTO      ENDCTR
176200951120     C                   ELSE
176300951120     C     WFORZ8        IFEQ      ' '
176400951120     C                   MOVEL     MSG(61)       VIDMSG
176500951120     C                   SETON                                        904528
176600951120     C                   MOVEL     'S'           WFORZ8            1
176700951120     C                   GOTO      ENDCTR
176800951120     C                   ENDIF
176900951120     C                   ENDIF
177000951024     C* ANDATA
177100951024     C     W001          WHENEQ    'A'
177200951024     C                   MOVEL     'A'           WTFM
177300951024     C                   MOVEL     NFA(1)        WNFM
177400951024     C                   Z-ADD     WANDDE        WDDE
177500951024     C* RITORNO
177600951024     C     W001          WHENEQ    'R'
177700951024     C                   MOVEL     'R'           WTFM
177800951024     C                   MOVEL     NFA(2)        WNFM
177900951024     C                   Z-ADD     WRIDDE        WDDE
178000951024     C*
178100951025   X2C                   OTHER
178200951024     C* LINEA FINALE DI ARRIVO PRESENTE SIA IN "A" CHE "R"
178300951024     C*   --> CONTROLLO QUALE DELLE 2  DAL NUMERO FERMATA
178400951024     C* ENTRAMBI MINORE --> ERRORE
178500951025    3C     NFP(1)        IFLT      NFA(1)
178600951025     C     NFP(2)        ANDLT     NFA(2)
178700951024     C                   MOVEL     MSG(28)       VIDMSG
178800951026     C                   SETON                                        904528
178900951024     C                   GOTO      ENDCTR
179000951025    3C                   ENDIF
179100951024     C* SE ENTRAMBI MAGGIORI --> TELEFONARE IN SEDE!!!
179200951025    3C     NFP(1)        IFGT      NFA(1)
179300951024     C     NFP(2)        ANDGT     NFA(2)
179400951024     C                   MOVEL     MSG(32)       VIDMSG
179500951026     C                   SETON                                        904528
179600951024     C                   GOTO      ENDCTR
179700951025    3C                   ENDIF
179800951024     C*
179900951025    3C     NFP(1)        IFLT      NFA(1)
180000951024     C                   MOVEL     'A'           WTFM
180100951024     C                   MOVEL     NFA(1)        WNFM
180200951024     C                   Z-ADD     WANDDE        WDDE
180300951025   X3C                   ELSE
180400951017     C                   MOVEL     'R'           WTFM
180500951017     C                   MOVEL     NFA(2)        WNFM
180600951024     C                   Z-ADD     WRIDDE        WDDE
180700951025    3C                   ENDIF
180800951025    2C                   ENDSL
180900951017     C*
181000951025     C                   CLEAR                   WFORZ3
181100951025     C*
181200951025     C* CONTROLLO SE LA LINEA IMMESSA E' LA FINALE DEL TRAINO
181300951024    2C     WFORZ3        IFEQ      ' '
181400951025     C                   MOVEL     VIDLNA        TRNTFA
181500951024     C                   EXSR      RICLNA
181600951024     C*
181700951017     C* SE LA LINEA IMMESSA NON CORRISPONDE ALLA LINEA FINALE DI ARRIVO
181800951017     C*   DEL TRAINO EMETTO MESSAGGIO FORZABILE E NON ABBINO
181900951025     C                   MOVEL     VIDLNA        COMLNA
182000951025    3C     TRNTFA        IFNE      COMLNA
182100951120    4C     *IN80         IFEQ      *ON
182200951120     C     WFORZ8        OREQ      ' '
182300951017     C                   MOVEL     MSG(29)       VIDMSG
182400951026     C                   SETON                                        904528
182500951017     C                   MOVEL     'S'           WFORZ3            1
182600951017     C                   GOTO      ENDCTR
182700951120    4C                   ENDIF
182800951120    3C                   ENDIF
182900951120    2C                   ENDIF
183000951024     C*
183100951024    1C                   ENDIF
183200951017     C*
183300951017     C     ENDCTR        ENDSR
183400960226     C*---------------------------------------------------------------*
183500971203     C* CERCO PER LA LNA TUTTI I SUOI TERMINAL DI ARRIVO
183600960226     C*---------------------------------------------------------------*
183700970102     C     RICEC2        BEGSR
183800970102     C                   CLEAR                   TFE
183900100616     C                   CLEAR                   TFEdef
184000971203     C                   Z-ADD     0             XX
184100100616     C                   Z-ADD     0             YY                3 0
184200970102     C*
184300970102     C     KCAE2         CHAIN     AZCAE02L                           31
184400960226     C     *IN31         DOWEQ     *OFF
184500960226     C     AMGDFV        IFGE      CAEDDE
184600960226     C     AMGDFV        ANDLE     CAEDSC
184700960226     C     CAEATB        ANDEQ     ' '
184800970102     C* RIMEPO UNA SCHIERA DI FILIALI TERMINAL DELLA LNA
184900970102     C     CAETFE        LOOKUP    TFE                                    31
185000970102     C  N31              ADD       1             XX                3 0
185100970102     C  N31              Z-ADD     CAETFE        TFE(XX)
185200970102     C                   ENDIF
185300970102     C*
185400970102     C     KCAE2         READE     AZCAE02L                               31
185500960226     C                   ENDDO
185600100616     c
185700100616     c* cerco i terminal alla data -1
185800100616     C     KCAE2         CHAIN     AZCAE02L                           31
185900100616     C     *IN31         DOWEQ     *OFF
186000100616     C     wdefmeno1     IFGE      CAEDDE
186100100616     C     wdefmeno1     ANDLE     CAEDSC
186200100616     C     CAEATB        ANDEQ     ' '
186300100616     C* RIMEPO UNA SCHIERA DI FILIALI TERMINAL DELLA LNA
186400100616     C     CAETFE        LOOKUP    TFEdef                                 31
186500100616     C  N31              ADD       1             yy                3 0
186600100616     C  N31              Z-ADD     CAETFE        TFEdef(yy)
186700100616     C                   ENDIF
186800100616     C*
186900100616     C     KCAE2         READE     AZCAE02L                               31
187000100616     C                   ENDDO
187100100616
187200960226     C                   ENDSR
187300951026     C*---------------------------------------------------------------*
187400951026     C* CONTROLLO IL NUMERO DI FOGLIO VIAGGIO IMMESSO
187500951026     C*---------------------------------------------------------------*
187600951026     C     CTRVAR        BEGSR
187700130502     c                   setoff                                       61
187800961213     C                   SETOFF                                       90
187900951026     C                   CLEAR                   VIDMSG
188000960124     C* SE VENGO DA INTERROGAZIONE NON CHAINO IL RECORD
188100130315     C     ilst4GES      IFNE      'N'
188200020805     C     K02FGV01      CHAIN     FNFGV01L                           8090
188300960124     C                   ELSE
188400020805     C     K02FGV01      CHAIN(N)  FNFGV01L                           80
188500960124     C                   ENDIF
188600960910     C*
188700960910     C* 90 ON - RECORD ALLOCATO
188800960910     C     *IN90         IFEQ      *ON
188900960910     C                   SETON                                        4828
189000130419     C                   MOVEL     MSG(67)       VIDMSG
189100960910     C                   GOTO      ENDVAR
189200960910     C                   ENDIF
189300951026      * FOGLIO INESISTENTE
189400951026     C     *IN80         IFEQ      *ON
189500951026     C                   SETON                                        489028
189600130419     C                   MOVEL     MSG(1)        VIDMSG
189700951026     C                   GOTO      ENDVAR
189800951026     C                   ENDIF
189900000516     C* ESTENSIONE FGV
190000020805     c                   eval      FGWnfv = VIDnfv
190100020805     c                   eval      FGWlnp = SIMfel
190200020805     c                   exsr      CTR_FGW
190300000602     C* POSSO MANUTENZIONARE SOLO SE SONO LA FGS
190400000605     C     FGWFGS        IFEQ      *ZEROS
190500000612     C                   Z-ADD     SIMFEL        WGWFGS
190600000612     C                   ELSE
190700000612     C                   Z-ADD     FGWFGS        WGWFGS
190800000605     C                   ENDIF
190900000612    2C     VIDFGS        IFNE      WGWFGS
191000000516     C     *IN98         ANDEQ     *OFF
191100000516     C                   SETON                                        489028
191200130419     C                   MOVEL     MSG(77)       VIDMSG
191300000516     C                   GOTO      ENDVAR
191400000516    2C                   ENDIF
191500951026     C*
191600951026     C* IN GESTIONE POSSO ANCHE VEDERE FOGLI ANNULLATI O CHIUSI
191700960201     C*  E SE RICHIAMATO COME MANUTENZIONE/INTERROGAZIONE
191800951026     C*  NEGLI ALTRI CASI SEMPRE ERRORE
191900951026     C*
192000951026     C* 52 ON - FOGLIO ANNULLATO
192100951026     C     FGVATB        IFEQ      '*'
192200951027     C                   SETON                                        529902
192300130315     C     ilst4GES      IFNE      ' '
192400130315     C     ilst4GES      ANDNE     'M'
192500130315     C     ilst4GES      ANDNE     'N'
192600951026     C                   SETON                                        904828
192700130419     C                   MOVEL     MSG(40)       VIDMSG
192800951026     C                   ENDIF
192900951026     C                   ENDIF
193000970102     C* 56 ON -  FOGLIO CHIUSO
193100130419    1C     FGVFCF        IFNE      *BLANKS
193200951027     C                   SETON                                        569902
193300130419    2C     ilst4GES      IFNE      ' '
193400130315     C     ilst4GES      ANDNE     'M'
193500130315     C     ilst4GES      ANDNE     'N'
193600951026     C                   SETON                                        904828
193700130419     C                   MOVEL     MSG(2)        VIDMSG
193800130419    2C                   ENDIF
193900080909     c* Se richiesta la manutenzione di un foglio già chiuso verifico che
194000080909     c* il foglio sia ancora presente nel file fogli di sede e che non
194100080909     c* risulti fatturato
194200130502    2c                   if        ilst4ges=' ' or ilst4ges='M'    or
194300130502    2c                             ilst4ges=' ' or ilst4ges='P'
194400080909     c     kfvs          chain     tnfvs01l
194500080910     c* se trovato verifico se fatturato
194600080910     c                   eval      wfat=' '
194700130419    3c                   if        %found(tnfvs01l)
194800080910     c     kfvs          chain     tntsr04l
194900130419    4c                   if        %found(tntsr04l)
195000080910     c     ktst          chain     tntst01l
195100130419    5c                   if        %found(tntst01l) and tstnft>0
195200080910     c                   eval      wfat='S'
195300130419    5c                   endif
195400130419    4c                   endif
195500130419    3c                   endif
195600080919     c* errore se relativo traino già fatturato o se foglio viaggio non
195700080919     c* presente in sede e data foglio più vecchia di 30 gg
195800130419    3c                   if        (Not %found(tnfvs01l) and fgvdfv<Dm30)
195900080910     c                             or wfat='S'
196000080910     C                   SETON                                        904828
196100130419     C                   MOVEL     MSG(91)       VIDMSG
196200130419    3c                   endif
196300130419    3c                   endif
196400130502     c                   seton                                        61
196500130419    2C                   ENDIF
196600990514      *
196700990514      *  NON E' POSSIBILE CHIUDERE UN FV DEFLUENZA SE LNA ESTERO
196800130315     C     ilst4GES      IFEQ      'C'
196900990514     C                   MOVEL     'TV'          COD
197000990514     C                   MOVEL(P)  FGVTTR        KEY
197100990514     C     KTAB          CHAIN     TABEL                              83
197200990514     C  N83              MOVEL     TBLUNI        DSTV
197300990514      *
197400990514     C     §TVDEF        IFEQ      'S'
197500990514     C                   MOVEL     FGVLNA        CODFIL
197600990514     C                   EXSR      CTRFIL
197700990514      *
197800060113     c                   if        esteroedpd = 'S'
197900130419     C                   MOVEL     MSG(73)       VIDMSG
198000990514     C                   SETON                                        489028
198100990514     C                   ENDIF
198200000405     C* Ripeto lo stesso controllo anche per le altre linee del foglio
198300000405     C                   Z-ADD     1             II                3 0
198400000420     C     II            DOWLE     299
198500000405     C     FFV(II)       IFGT      *ZEROS
198600000405     C                   MOVEL     FFV(II)       CODFIL
198700000405     C                   EXSR      CTRFIL
198800000405      *
198900060113     c                   if        esteroedpd = 'S'
199000000405     C                   MOVEL     MSG(73)       VIDMSG
199100000405     C                   SETON                                        489028
199200000405     C                   ENDIF
199300000405     C                   ENDIF
199400000405     C                   ADD       1             II
199500000405     C                   ENDDO
199600990514      *
199700990514     C                   ENDIF
199800990514     C                   CLEAR                   DSTV
199900990514     C                   ENDIF
200000990514      *
200100990514      * ERRORE
200200990514     C   90              GOTO      ENDVAR
200300951026     C*
200400130419     c     endvar        tag
200500130419     c
200600130419     c                   if        *in20 and vidmsg<>*blanks
200700130419     c                   movel     vidmsg        olst4msg
200800130419     c                   endif
200900130419     c
201000130419     C                   ENDSR
201100951024     C*---------------------------------------------------------------*
201200951024     C* CALL AL PGM TNSD19R DI RICERCA LINEE DI ARRIVO
201300951024     C*---------------------------------------------------------------*
201400951024     C     CASD19        BEGSR
201500951024     C                   MOVEL     *BLANKS       §COD
201600951024     C                   MOVEL     *BLANKS       §TIP
201700951024     C                   MOVEL     *BLANKS       §§DES
201800951024     C                   CALL      'TNSD19R'
201900951024     C                   PARM                    §COD              3
202000951024     C                   PARM                    §TIP              1
202100951024     C                   PARM                    §§DES            25
202200951024     C                   ENDSR
202300951024     C*---------------------------------------------------------------*
202400951024     C* RICERCO LA LINEA DI ARRIVO FINALE DEL TRAINO PER SIMFEL
202500951024     C*---------------------------------------------------------------*
202600951024     C     RICLNA        BEGSR
202700951024     C* LEGGO DALLA FERMATA CALCOLATA IN POI
202800951024     C     KTLR1         SETGT     TNTLR01L
202900951024     C     KTLR11        READE     TNTLR01L                               84
203000951024     C*
203100951024    3C     *IN84         DOWEQ     *OFF
203200951024     C     TLRTFM        ANDEQ     WTFM
203300951024     C* LEGGO SOLO SE FILIALE DI SCARICO E NON ANNULLATA
203400951024    4C     TLRATB        IFEQ      ' '
203500951024     C     TLRSCA        ANDEQ     'S'
203600000522     C                   MOVE      VIDFGS        WTFP
203700951024     C                   Z-ADD     TLRFIL        WTFA
203800951024     C                   EXSR      REDTLM
203900951025     C  N85              Z-ADD     TLRFIL        TRNTFA
204000951024    4C                   ENDIF
204100951024     C*
204200951024     C     KTLR11        READE     TNTLR01L                               84
204300951024    3C                   ENDDO
204400951024     C                   ENDSR
204500951024     C*---------------------------------------------------------------*
204600951024     C* LEGGO FERMATE E MC PER VEDERE SE ESISTE LIN.FINALE ARRIVO
204700951024     C*---------------------------------------------------------------*
204800951024     C     LEGFER        BEGSR
204900951024     C     KTLR3         CHAIN     TNTLR03L                           84
205000960202    1C     *IN84         DOWEQ     *OFF
205100960202     C* SE E' FERMATA DI SCARICO ELABORO
205200960202   1AC     TLRSCA        IFEQ      'S'
205300951024     C*
205400951024     C* VERIFICO SE ESISTE ANCHE NEI MC
205500951024     C                   EXSR      REDTLM
205600960202     C                   SETON                                        84
205700951024     C* TROVATO RECORD VALIDO
205800951024    2C  N85WTFM          IFEQ      'A'
205900951024     C                   MOVEL     'A'           W001
206000951024     C                   Z-ADD     TLRNFM        NFA(1)
206100951024     C                   ELSE
206200951024    3C     W001          IFEQ      'A'
206300951024     C                   MOVEL     'E'           W001
206400951024     C                   ELSE
206500951024     C                   MOVEL     'R'           W001
206600951024    3C                   ENDIF
206700951027     C                   Z-ADD     TLRNFM        NFA(2)
206800951024    2C                   ENDIF
206900960202     C*
207000960202  X1AC                   ELSE
207100960202     C* SE NON E' FERMATA DI SCARICO PROVO A RILEGGERE, PER I LOCALI
207200960202     C     KTLR3         READE     TNTLR03L                               84
207300960202   1AC                   ENDIF
207400960202    1C                   ENDDO
207500951024     C                   ENDSR
207600951024     C*-----------------------------------------------------***********
207700951024     C* LETTURA FILE METRI CUBI                             * REDTLM  *
207800951024     C*-----------------------------------------------------***********
207900951024     C     REDTLM        BEGSR
208000951024     C     KTLM          SETLL     TNTLM01L
208100951024     C     KTLM          READE     TNTLM01L                               85
208200951024    2C     *IN85         DOWEQ     *OFF
208300951024     C     TLMATB        ANDNE     ' '
208400951024     C     KTLM          READE     TNTLM01L                               85
208500951024    2C                   ENDDO
208600951024     C                   ENDSR
208700960122     C*-----------------------------------------------------***********
208800010920     C* LETTURA FILE TITLL00F LISTINI
208900960122     C*-----------------------------------------------------***********
209000960122     C     REDTLL        BEGSR
209100010920     C     KTLL1         SETLL     TITLL01L
209200010920     C     KTLL1         READE     TITLL01L                               83
209300960122    2C     *IN83         DOWEQ     *OFF
209400960122     C     TLLATB        ANDNE     ' '
209500010920     C     KTLL1         READE     TITLL01L                               83
209600960122    2C                   ENDDO
209700960122     C                   ENDSR
209800951013     C*-----------------------------------------------------***********
209900951013     C* COPIA CAMPI VIDEATA DA FILE TRAINI                  * RIETRV  *
210000951013     C*-----------------------------------------------------***********
210100951013     C     RIETRV        BEGSR
210200951013     C*
210300951130     C****  DATA FV  ****
210400951130     C                   MOVEL     AMGDFV        WSDFV
210500951130     C****  FILIALE FINALE DI ARRIVO  ****
210600951013     C                   MOVEL     VIDLNA        COMLNA
210700960126     C                   MOVEL     VIDLNA        CODFIL
210800960126     C                   EXSR      CTRFIL
210900960126     C  N34              MOVEL     ORGDES        DESLNA
211000960226     C                   MOVEL     VIDLNA        KTFA
211100960226     C                   MOVEL     'A'           KEPA
211200970102     C*
211300970102     C                   EXSR      RICEC2
211400971203     C  N34              MOVEA     TFE           TFEL
211500970102     C*
211600971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
211700971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
211800020805     C                   CLEAR                   FNLV55DS
211900971203     C                   MOVEL     'A'           D55TPT
212000971203     C                   MOVE      VIDLNA        D55LIN
212100000523     C                   MOVE      VIDFGS        D55LNP
212200971203     C                   MOVE      AMGDFV        D55DRF
212300971203     C                   CALL      'FNLV55R'
212400020805     C                   PARM                    FNLV55DS
212500971203     C                   MOVE      D55TFA        LNAFE0
212600951218     C*
212700960123     C****  BIS/ECCEZIONALE ****
212800960123     C                   MOVEL     TLTBIS        VIDBIS
212900960123     C*
213000960123     C****  TIPO AUTOMEZZO  ****
213100951218     C* SE NON  IMMESSO PRENDO QUELLO DI DEFAULT
213200951218    1C     VIDTMZ        IFEQ      ' '
213300951218     C                   MOVEL     TLTTMZ        VIDTMZ
213400951218     C                   EXSR      CTRTMZ
213500951218    1C                   ENDIF
213600960129     C*
213700960129     C****  TRAZIONISTA *****
213800960129     C     VIDPDR        IFLE      *ZEROS
213900960129     C     WTLPDR        ANDGT     0
214000130502     c     woblpd        andeq     ' '
214100960129     C                   MOVEL     WTLPDR        VIDPDR
214200000807     C                   Z-ADD     WTLPDR        KTLPDR
214300000807     C     KTNTLZ        CHAIN     TNTLZ01L                           83
214400960302     C* SE SI TRATTA DEL TRAZIONISTA FASULLO, SE IMMESSA LA DESCRIZIONE
214500960302     C*  LA TENGO, ALTRIMENTI CI METTO LA DECODIFICA DEL FASULLO
214600960302     C     VIDPDR        IFNE      '0465000'
214700960302     C  N83              MOVEL     TLZRSC        VIDDPD
214800960129     C                   ENDIF
214900960302     C                   ENDIF
215000960129     C*
215100951013     C*
215200951013     C****  TIPO TRAINO  ****
215300951013     C                   MOVEL     TLTTTR        VIDTTR
215400951027     C                   MOVEL     'TV'          COD
215500951027     C                   MOVEL(P)  VIDTTR        KEY
215600951027     C     KTAB          CHAIN     TABEL                              83
215700951027     C     *IN83         IFEQ      *OFF
215800951027     C     TBLFLG        ANDEQ     ' '
215900951027     C                   MOVEL     TBLUNI        DSTV
216000951027     C                   ELSE
216100951027     C                   CLEAR                   DSTV
216200951027     C                   ENDIF
216300960209     C                   MOVEL     §TVDES        DESTTR
216400951027     C* SE NON UTILIZZABILE IN FILIALE O ANNULLATO --> NON VISUALIZZO
216500951027     C     §TVUTF        IFNE      'S'
216600951027     C                   CLEAR                   VIDTTR
216700960209     C                   CLEAR                   DESTTR
216800951027     C                   ENDIF
216900110512     c
217000110511     C*
217100110512     c**** Punto di Scambio  ****
217200110512     c                   exsr      LetTLS
217300110512     c
217400110511    1c                   if        not *in30 and tlsatb=' '
217500110511     c                   eval      vidpsc=tlspsc
217600110530     c                   seton                                        08
217700110511     c     vidpsc        chain     tnpsc01l
217800110511    2c                   if        %found(tnpsc01l)
217900110511     c                   eval      despsc=pscloc
218000110530     c                   eval      despind=pscind
218100110530     c                   eval      desploc=%trim(psccap) + ' ' +
218200110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
218300110530     c
218400110511     c                   else
218500110511     c                   eval      despsc=*all'?'
218600110530     c                   eval      despind=*all'?'
218700110511    2c                   endif
218800130412     c
218900130412     c* Imposto la filiale finale di arrivo, se non c'e' il traino
219000130412     c                   eval      Dpscflo=pscflo
219100130412     c                   if        tlstrns=0  and
219200130412     c                                §pscflr>*zeros
219300130412     c                   movel     §pscflr       vidlnar
219400130412     c                   movel     §pscflr       codfil
219500130412     c                   exsr      ctrfil
219600130412     c  n34              movel     orgdes        deslnar2
219700130412     c  n34              movel     orgdes        deslnar
219800130412     c
219900130412     c                   endif
220000110511     c
220100110511     c                   eval      vidhhs=tlshhs
220200110530     c                   eval      vidtrns=tlstrns
220300110513     c* Decodifico il traino
220400130412     c                   if        vidtrns>0
220500110513     c                   exsr      Ctrtrns
220600130412     c                   endif
220700110511     c
220800110512     C                   MOVE      amgdfv        G02INV
220900110511     C                   MOVE      '3'           G02ERR
221000110511     C                   CALL      'XSRDA8'
221100110511     C                   PARM                    WLBDAT
221200110511     c* data scambio = data traino
221300110511    2c                   if        tlsggs='A'
221400110512     C                   MOVE      G02DAT        viddgs
221500110511    2c                   else
221600110511     c*
221700110512     c                   z-add     1             c
221800110511     C     TLsggs        LOOKUP    GFV(C)                                 31
221900110511    3c                   if        *in31
222000110511     c
222100110511     c     g02tgi        add       c             gi8tgi
222200110511     C                   CALL      'XSRGI8'
222300110511     C                   PARM                    WGIDAT
222400110512     C                   MOVEL     GI8DAT        viddgs
222500110511    3c                   endif
222600110511    2c                   endif
222700110511    1c                   endif
222800130318     c
222900130424     c* Chaino la proposta partenza, se presente per proporre i dati mancanti
223000130318     c                   if        ilst4NRRP>*zeros
223100130318     c                   movel     ilst4nrrp     w0130            13 0
223200130318     c     w0130         chain     fnfgt00f
223300130318     c                   if        %found(fnfgt00f)
223400130318     C****  TRAZIONISTA *****
223500130318     c                   if        fgtpdr>0
223600130318     C                   MOVEL     fgtPDR        VIDPDR
223700130318     C                   Z-ADD     fgtPDR        KTLPDR
223800130318     C     KTNTLZ        CHAIN     TNTLZ01L                           83
223900130318     C* SE SI TRATTA DEL TRAZIONISTA FASULLO, SE IMMESSA LA DESCRIZIONE
224000130318     C*  LA TENGO, ALTRIMENTI CI METTO LA DECODIFICA DEL FASULLO
224100130318     C     VIDPDR        IFNE      '0465000'
224200130318     C  N83              MOVEL     TLZRSC        VIDDPD
224300130318     C                   ENDIF
224400130318     C                   ENDIF
224500130318     c
224600130318     c                   movel     fgtdcn        viddcn
224700130318     c                   movel     fgttrm        vidtrm
224800130318     c                   movel     fgttrr        vidtrr
224900130318     c                   movel     fgtnot        vidnt1
225000130628     c                   movel     fgtnt2        vidnt2
225100130628     c                   movel     fgtan1        vidan1
225200130628     c                   movel     fgtan2        vidan2
225300130424     c* reperisco il cellulare del conducente
225400130424     c                   eval      kdcn=fgtdcn
225500130424     c                   eval      kpdr=fgtpdr
225600130424     c     kata          chain     aiata01l
225700130424     c                   if        %found(aiata01l)
225800130424     c                   eval      vidcnc=atatel
225900130424     c                   endif
226000130318
226100130318     c                   endif
226200130318     c                   endif
226300130510     c
226400130510     c                   if        vidttr<>'L'
226500130510     c                             and vidttr<>'H' and vidttr<>'D'
226600130510     c                             and vidttr<>' '
226700130510
226800130516     c                   if        viddcn=*blanks and vidpdr>*zeros
226900130516     c                   seton                                        55
227000130516     c***                eval      viddcn='?'
227100130510     c                   endif
227200130516     c                   if        vidtrm=*blanks and vidpdr>*zeros and
227300130516     c                             viddcn<>*blanks
227400130516     c***                eval      vidtrm='?'
227500130516     c                   seton                                        54
227600130510     c                   endif
227700130510     c                   endif
227800130502     c
227900130502     C****  IMPORTO PATTUITO  ****
228000130502     C* SE NON E' LA FILIALE CHE DEVE ESPORRE L'IMPORTO NON E' INSERIB
228100130502    1C     VIDFGS        IFEQ      TLTFES
228200130502     C     WTFM          ANDEQ     WFMFES
228300130502     C                   EXSR      LEGLIS
228400130502     C  N83              MOVEL     TLLDIV        VIDVIP
228500130502     C  N83              MOVEL     TLLDIV        SAVDIV
228600130502     C  N83              Z-ADD     TLLILI        VIDIPT
228700130502    1C                   ENDIF
228800951013     C*
228900951017     C* SALVO I CAMPI PER CONTROLLARE SE VIENE MODIFICATO QUALCOSA
229000951025     C                   MOVEL     VIDPDR        TRNPDR
229100951025     C                   MOVEL     VIDTMZ        TRNTMZ
229200010920     C                   MOVEL     VIDVIP        TRNVIP
229300010920     C                   Z-ADD     VIDIPT        TRNIPT
229400951027     C                   MOVEL     §TVUTF        TRNUTF
229500960130     C                   MOVEL     VIDTTR        SAVTTR
229600960130     C                   MOVEL     VIDBIS        SAVBIS
229700110511     C                   MOVEL     VIDpsc        SAVpsc
229800110530     C                   clear                   SAVlnar
229900110511     C                   MOVEL     VIDtrns       SAVtrns
230000130517     c
230100130517     c                   clear                   viddtt
230200130517     c                   clear                   vidhmt
230300130517     c* calcolo data teorica per visualizzarla
230400130517     C                   CLEAR                   TNTL35DS
230500130517     C                   MOVEL     'T'           KTDH
230600130517     C                   MOVEL     'P'           KEPA
230700130517     C                   MOVEL     'P'           D35EPA
230800130517     C                   MOVEL     'D'           D35TUP
230900130517     C                   MOVEL     VIDTRN        D35TRN
231000130517     C                   MOVEL     WDDE          D35DDE
231100130517     C                   MOVEL     WDTN          D35DTN
231200130517     C                   CLEAR                   WTFA
231300130517     C     WTFM          IFEQ      'A'
231400130517     C                   Z-ADD     NFP(1)        d35NFM
231500130517     C                   ELSE
231600130517     C                   Z-ADD     NFP(2)        d35NFM
231700130517     C                   ENDIF
231800130517     C                   MOVEL     TNTL35DS      KPJBU
231900130517     C                   CALL      'TNTL35R'
232000130517     C                   PARM                    KPJBA
232100130517     C                   MOVEL     KPJBU         TNTL35DS
232200130517     C* C'E' ERRORE
232300130517     C     D35ERR        IFeq      *BLANKS
232400130517     C                   MOVEL     D35HTE        vidhmt
232500130517     C                   MOVEL     D35DTE        g02inv
232600130517     C                   MOVE      *ZEROS        G02DAT
232700130517     C                   MOVEL     '3'           G02ERR
232800130517     C                   CALL      'XSRDA8'
232900130517     C                   PARM                    WLBDAT
233000130517     C                   Z-ADD     G02DAT        VIDDTT
233100130517     c                   endif
233200951018     C*
233300951013     C                   ENDSR
233400110512     C*-----------------------------------------------------***********
233500110512     c* Lettura file TLS degi punti di scambio
233600110512     C*-----------------------------------------------------***********
233700110512     c     LetTLS        BEGSR
233800110512     c
233900110512     c**** Punto di Scambio  ****
234000110512     c* Giro la data foglio viaggio per avere il giorno della settimana
234100110512     C                   CLEAR                   WLBDAT
234200110512     C                   MOVE      amgdfv        G02INV
234300110512     C                   MOVE      '3'           G02ERR
234400110512     C                   CALL      'XSRDA8'
234500110512     C                   PARM                    WLBDAT
234600110512     C                   MOVE      G02DAT        W0020             2 0
234700110512     C                   MOVEL     G02DAT        GGMMAA
234800110512     C                   MOVE      W0020         GGMMAA
234900110512     C*
235000110512     C                   CALL      'XGIOSE1'
235100110512     C                   PARM                    GGMMAA            6
235200110512     C                   PARM                    GIOSET            1
235300110512     c
235400110531     c                   clear                   dstls
235500110512     C     KTLS          CHAIN     TNTLS01L                           30
235600110512     C*
235700110512     C* SE NON TROVATO O ANNULLATO, PROVO CON IL GIORNO DELLA
235800110512     C*   SETTIMANA = *BLANKS
235900110512    1C     *IN30         IFEQ      *ON
236000110512     C     TLSATB        ORNE      ' '
236100110512     C                   MOVEL     ' '           GIOSET
236200110531     c                   clear                   dstls
236300110512     C     KTLS          CHAIN     TNTLS01L                           30
236400110512    1C                   ENDIF
236500110512     c                   ENDSR
236600951218     C*-----------------------------------------------------***********
236700951218     C* LEGGO I LISTINI PER PRENDERE IMPORTO FV
236800951218     C*-----------------------------------------------------***********
236900951218     C     LEGLIS        BEGSR
237000951218     C* PER CERCARE L'IMPORTO 4 TENTATIVI:
237100951218     C*  1) CON CODICI SPECIFICI
237200951218     C                   MOVEL     VIDTMZ        KTMZ
237300951218     C                   MOVEL     VIDPDR        KPDR
237400951218     C                   EXSR      REDTLL
237500951218     C*  2) CON TRAZIONISTA SPECIFICO E TIPO AUTOMEZZO = 99
237600951218    1C     *IN83         IFEQ      *ON
237700951218     C                   MOVEL     '99'          KTMZ
237800951218     C                   EXSR      REDTLL
237900951218    1C                   ENDIF
238000951218     C*  3) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO SPECIFICO
238100951218    1C     *IN83         IFEQ      *ON
238200960206     C                   CLEAR                   KTMZ
238300951218     C                   MOVEL     VIDTMZ        KTMZ
238400951218     C                   MOVEL     9999999       KPDR
238500951218     C                   EXSR      REDTLL
238600951218    1C                   ENDIF
238700951218     C*  2) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO = 99
238800951218    1C     *IN83         IFEQ      *ON
238900951218     C                   MOVEL     '99'          KTMZ
239000951218     C                   MOVEL     9999999       KPDR
239100951218     C                   EXSR      REDTLL
239200951218    1C                   ENDIF
239300951218     C* 83 OFF - TROVATO
239400951218     C* ESPONGO IMPORTO SUL FV
239500010718     C* 18/07/01: IMPOSTO SEMPRE FISSO "N" PERCHÈ NON LO VOGLIO PIU'
239600010718     C*            STAMPARE
239700951218    2C  N83TLLIES        IFEQ      0
239800951218     C                   MOVEL     'N'           VIDSNS
239900951218     C                   ELSE
240000010718     C                   MOVEL     'N'           VIDSNS
240100951218    2C                   ENDIF
240200951218     C                   ENDSR
240300951017     C*-----------------------------------------------------***********
240400951017     C* CONTROLLI FORMATO 1                                 *  CTR1   *
240500951017     C*-----------------------------------------------------***********
240600951024     C     CTRD02        BEGSR
240700951027     C                   SETOFF                                       90
240800960318     C                   SETOFF                                       404142
240900951027     C                   SETOFF                                       434445
241000960129     C                   SETOFF                                       464759
241100130419     C                   SETOFF                                       535455
241200000515     C                   SETOFF                                       575849
241300011012     C                   SETOFF                                       6038
241400110512     C                   SETOFF                                       747576
241500951027     C*
241600951027     C****  TIPO TRAINO  ****
241700951027     C* RICERCA
241800951027    1C     VIDTTR        IFEQ      '?'
241900951027     C                   MOVEL     'TV'          COD
242000951027     C                   MOVEL     *BLANKS       VIDTTR
242100951027     C                   MOVEL     *BLANKS       DES              25
242200000316     C                   CLEAR                   KEY
242300951027     C                   CALL      'X§TABER'
242400951027     C                   PARM                    CODUT
242500951027     C                   PARM                    COD
242600951027     C                   PARM                    KEY
242700951027     C                   PARM                    DES
242800951027     C                   MOVEL     KEY           VIDTTR
242900951027     C                   MOVEL     DES           DESTTR
243000951027     C                   SETON                                        9042
243100951027     C                   GOTO      ENDCT2
243200951027    1C                   ENDIF
243300951027     C*
243400000316     C* CONTROLLO VALIDITA' TIPO TRAINO
243500000316     C                   CLEAR                   DSTV
243600000316     C                   CLEAR                   DESTTR
243700000316     C*
243800951027     C                   MOVEL     'TV'          COD
243900951027     C                   MOVEL(P)  VIDTTR        KEY
244000951027     C     KTAB          CHAIN     TABEL                              83
244100951027     C  N83              MOVEL     TBLUNI        DSTV
244200000828     C                   MOVEL     §TVDEF        WDEF1A            1
244300951027    2C     *IN83         IFEQ      *ON
244400951027     C     TBLFLG        ORNE      ' '
244500951027     C     §TVUTF        ORNE      'S'
244600951027     C                   MOVEL     MSG(46)       VIDMSG
244700951027     C                   SETON                                        904228
244800951027     C                   GOTO      ENDCT2
244900951027    2C                   ENDIF
245000960209     C*
245100960209     C                   MOVEL     §TVDES        DESTTR
245200951027     C*
245300951027     C* SE MODIFICA PULISCO LA FORZATURA
245400951027     C     VIDTTR        IFNE      SAVTTR
245500951027     C                   CLEAR                   WFORZ7
245600960202     C                   MOVEL     'S'           WCALNA
245700951027     C                   MOVEL     VIDTTR        SAVTTR
245800951027     C                   ENDIF
245900960130     C* SE MODIFICA PULISCO LA FORZATURA
246000960130     C     VIDBIS        IFNE      SAVBIS
246100960130     C                   CLEAR                   WFORZD
246200960130     C                   MOVEL     VIDBIS        SAVBIS
246300960130     C                   ENDIF
246400100616     c
246500040204     C****  DATA FOGLIO VIAGGIO  ****
246600040204    1C     *IN01         IFEQ      *OFF
246700130318     c                   movel     'S'           wd02              1
246800040204     C                   EXSR      CALDAT
246900040204     C   90              GOTO      ENDCT2
247000040204     C* SE FOGLIO ABBINATO E MODIFICO LA DATA --> SBINO: AVVISO
247100040204    2C     WFORZ9        IFEQ      ' '
247200040204     C     VIDTRN        ANDGT     0
247300040204     C     WSDFV         ANDNE     AMGDFV
247400040204     C                   MOVEL     MSG(5)        VIDMSG
247500040204     C                   SETON                                        904428
247600040204     C                   MOVEL     'S'           WFORZ9            1
247700040204     C                   GOTO      ENDCT2
247800040204    2C                   ENDIF
247900040204    1C                   ENDIF
248000951017     C*
248100951017     C****  FILIALE FINALE D'ARRIVO  ****
248200951026     C* SOLO SE MODIFICATA  E SOLO IN IMMISSIONE/VARIAZIONE
248300951026     C  N01VIDLNA        IFNE      SAVLNA
248400951017     C                   EXSR      CTRLNA
248500060828     C   90              GOTO      ENDCT2
248600060828     C* SALVO LA LINEA DI ARRIVO se tutto ok
248700060828     C                   MOVEL     VIDLNA        SAVLNA
248800060828     C                   MOVEL     'S'           WCALNA            1
248900951025     C                   ENDIF
249000951027     C*
249100951027     C* SE TRAINO DI DEFLUENZA O LOCALE, VERIFICO SE LE LINEE SONO
249200951027     C*  CORRETTE
249300951027     C                   MOVEL     VIDLNA        CODFIL
249400100616     c
249500951027    1C     VIDTTR        IFEQ      'L'
249600951027     C     §TVDEF        OREQ      'S'
249700100616     c                   setoff                                       31
249800100616     c* se terminal di arrivo al giorno prima --> OK lo stesso
249900100616     c                   if        §tvdef='S'
250000100616     C     VIDFGS        LOOKUP    TFEdef                                 31
250100100616     c                   endif
250200970102     C*
250300100616     c                   if        not *in31
250400000612     C     VIDFGS        LOOKUP    TFEL                                   31
250500970102    2C     *IN31         IFEQ      *OFF
250600951027     C                   MOVEL     MSG(47)       VIDMSG
250700951027     C                   SETON                                        284590
250800951027     C                   GOTO      ENDCT2
250900970102    2C                   ENDIF
251000951031    1C                   ENDIF
251100100616     c                   endif
251200100616     c
251300960302     c* se la lna immessa E' UN 2 LIVELLO IN ARRIVO DEVE ESSERE O TRAIN
251400960302     C* DI DEFLUENZA O LOCALE O INOLTRO
251500000612     C     VIDFGS        IFEQ      LNAFE0
251600960302    2C     VIDTTR        IFNE      'L'
251700960302     C     VIDTTR        ANDNE     'I'
251800960302     C     §TVDEF        ANDNE     'S'
251900960302     C                   MOVEL     MSG(66)       VIDMSG
252000960302     C                   SETON                                        284290
252100960302     C                   GOTO      ENDCT2
252200960302    2C                   ENDIF
252300960302    1C                   ENDIF
252400951031     C* PER  UNA DEFLUENZA LNP <> DA LNA
252500000612     C                   MOVE      VIDFGS        W003A             3
252600951031     C     §TVDEF        IFEQ      'S'
252700000612     C     W003A         ANDEQ     VIDLNA
252800951027     C                   MOVEL     MSG(50)       VIDMSG
252900951027     C                   SETON                                        284590
253000951027     C                   GOTO      ENDCT2
253100951031     C                   ENDIF
253200961216     C**
253300961216     C** SOLO PER DEFLUENZE E LNA ESTERA E FV NON ANCORA PARTITO
253400961216     C**
253500961213     C*
253600951218     C* CONTROLLO DEL TIPO AUTOMEZZO E DEL TRAZIONISTA
253700951218     C                   EXSR      CTRPAR
253800951218     C   90              GOTO      ENDCT2
253900951026     C*
254000951026     C**** CONDUCENTE  ****
254100951026     C* OBBLIGATORIO IN BORDERO'/CHIUSURA FV
254200960205     C* NON OBBLIGATORIO SE E' LOCALE
254300951026     C   01VIDDCN        IFEQ      *BLANKS
254400960205     C     VIDTTR        ANDNE     'L'
254500010621     C                   MOVEL     MSG(18)       VIDMSG
254600951026     C                   SETON                                        905528
254700951026     C                   GOTO      ENDCT2
254800951026    2C                   ENDIF
254900130424     c
255000130424     c* Se messo '?' cerco conducente tra quelli del trazionista
255100130424     c                   eval      indx=%scan('?':viddcn)
255200130424    2c                   if        indx>0   and vidpdr>*zeros
255300130424     c                   clear                   tntl83ds
255400130424     c                   eval      i83soc=soctraini
255500130424     c                   eval      i83ksc=%int(vidpdr)
255600130424     c                   eval      i83mod='1'
255700130424     c                   eval      kpjbu=tntl83ds
255800130424     c                   call      'TNTL83R'
255900130424     c                   parm                    kpjba
256000130424     c                   eval      tntl83ds=kpjbu
256100130424    3c                   if        o83_aut<>*blanks
256200130424     c                   eval      viddcn=o83_aut
256300130424     c                   eval      vidcnc=o83_tel
256400130424     c                   seton                                        9055
256500130424     C                   GOTO      ENDCT2
256600130424     c                   else
256700130424     c                   clear                   viddcn
256800130429     c                   seton                                        9055
256900130429     C                   GOTO      ENDCT2
257000130424    3c                   endif
257100130424     c
257200130424   x2c                   else
257300130424    3c                   if        indx>0
257400130424     C                   MOVEL     MSG(106)      VIDMSG
257500130424     C                   SETON                                        905528
257600130424     C                   GOTO      ENDCT2
257700130424    3c                   endif
257800130424    2c                   endif
257900130503     c* se cambiato trazionista ricontrollo conducente  e targa
258000130424     c                   if        savpdr<>vidpdr
258100130424     c                   clear                   wforzD1
258200130424     c                   clear                   wforzM1
258300130424     c                   movel     vidpdr        savpdr
258400130424     c                   endif
258500130424     c                   if        savdcn<>viddcn
258600130424     c                   clear                   wforzD1
258700130424     c                   movel     viddcn        savdcn
258800130424     c                   endif
258900130424     c
259000130424     c* Controllo se accreditato per il trazionista
259100130508     c*  escludo traini H D L
259200130424    2c                   if        viddcn<>*blanks and vidpdr>*zeros
259300130508
259400130424     c                   eval      kdcn=viddcn
259500130424     c                   eval      kpdr=%int(vidpdr)
259600130424     c     kata          chain     aiata01l
259700130508     c
259800130508   2ac                   if        vidttr<>'H' and vidttr<>'D'
259900130508     c                             and vidttr<>'L'
260000130508
260100130424    3c                   if        wforzd1=' ' and (not %found(aiata01l)
260200130424     c                             or   atadtfir>0)
260300130510     c                   if        *in80 or fgvpmb<>viddcn
260400130510     C                   MOVEL     MSG(107)      VIDMSG
260500130510     c                   else
260600130510     C                   MOVEL     MSG(111)      VIDMSG
260700130510     c                   endif
260800130510     c                   eval      WforzD1='A'
260900130424     C                   SETON                                        905528
261000130424     C                   GOTO      ENDCT2
261100130424    3c                   endif
261200130508   2ac                   endif
261300130424     c*
261400130424    3c                   if        %found(aiata01l) and atadtfir=0
261500130514     c                   if        vidcnc=*blanks
261600130424     c                   eval      vidcnc=atatel
261700130514     c                   endif
261800130424     c*
261900130424   x3c                   else
262000130424     c* Se trovo comunque il conducente propongo il suo cellulare
262100130514   3ac                   if        vidcnc=*blanks
262200130424     c     viddcn        setll     aiata02l
262300130424     c     viddcn        reade     aiata02l
262400130424    4c                   dow       not %eof(aiata02l)
262500130424    5c                   if        atadtfir=0
262600130424     c                   eval      vidcnc=atatel
262700130424     c                   leave
262800130424    5c                   endif
262900150916     c     viddcn        reade     aiata02l
263000130424    4c                   enddo
263100130514   3ac                   endif
263200130424     c
263300130424    3c                   endif
263400130424     c
263500130424    2c                   endif
263600130424     c
263700951026     C*
263800010622      **** CELLULARE ****
263900010622     C* OBBLIGATORIO IN BORDERO'/CHIUSURA FV
264000010622     C* NON OBBLIGATORIO SE E' LOCALE
264100130424     C   01VIDCNC        IFEQ      *BLANKS
264200010621     C     VIDTTR        ANDNE     'L'
264300010621     C                   MOVEL     MSG(75)       VIDMSG
264400010621     C                   SETON                                        906028
264500010621     C                   GOTO      ENDCT2
264600010621    2C                   ENDIF
264700130424     C     VIDCNC        IFNE      *BLANKS
264800010704     C     ' '           CHECKR    VIDCNC        CONTA             2 0
264900010704     C     CONTA         IFEQ      0
265000010704     C     CONTA         ORLT      7
265100010704     C                   MOVEL     MSG(76)       VIDMSG
265200010704     C                   SETON                                        902860
265300010704     C                   GOTO      ENDCT2
265400010703     C                   ENDIF
265500010704     C                   ENDIF
265600010621     C*
265700951026     C**** TARGHE ****
265800951026     C* OBBLIGATORIA ALMENO UNA IN BORDERO'/CHIUSURA FV
265900960205     C* NON OBBLIGATORIE SE E' LOCALE
266000951026     C   01VIDTRM        IFEQ      *BLANKS
266100951026     C     VIDTRR        ANDEQ     *BLANKS
266200960205     C     VIDTTR        ANDNE     'L'
266300951026     C                   MOVEL     MSG(11)       VIDMSG
266400951026     C                   SETON                                        549028
266500951026     C                   GOTO      ENDCT2
266600951026     C                   ENDIF
266700130424     c
266800130509     c* Se messo '?' cerco targa tra quelli del trazionista
266900130424     c                   eval      indx=%scan('?':vidTRM)
267000130424    2c                   if        indx>0   and vidpdr>*zeros
267100130424     c                   clear                   tntl84ds
267200130424     c                   eval      i84soc=soctraini
267300130424     c                   eval      i84ksc=%int(vidpdr)
267400130424     c                   eval      i84mod='1'
267500130424     c                   eval      kpjbu=tntl84ds
267600130424     c                   call      'TNTL84R'
267700130424     c                   parm                    kpjba
267800130424     c                   eval      tntl84ds=kpjbu
267900130424    3c                   if        o84_TRM<>*blanks
268000130424     c                   eval      vidtrm=o84_trm
268100130424     c                   seton                                        9054
268200130424     C                   GOTO      ENDCT2
268300130424     c                   else
268400130424     c                   clear                   vidtrm
268500130429     c                   seton                                        9054
268600130429     C                   GOTO      ENDCT2
268700130424    3c                   endif
268800130424     c
268900130424   x2c                   else
269000130424    3c                   if        indx>0
269100130424     C                   MOVEL     MSG(108)      VIDMSG
269200130424     C                   SETON                                        905428
269300130424     C                   GOTO      ENDCT2
269400130424    3c                   endif
269500130424    2c                   endif
269600130424     c
269700130424     c* Controllo se accreditato per il trazionista
269800130510     c* prima conpatto la targa
269900130510      /free
270000130510             if  vidtrm <> *blank;
270100130510             xi=1  ;
270200130510               for  xi = 1  to  %len( %trimr( vidtrm ) );
270300130510                 if  %subst( vidtrm : xi : 1 ) = *blank;
270400130510                   vidtrm = %subst( vidtrm : 1 : xi - 1 ) +
270500130510                            %subst( vidtrm : xi + 1 );
270600130510                   xi -= 1;
270700130510                 endif;
270800130510               endfor;
270900130510             endif;
271000130510             if  vidtrr <> *blank;
271100130510             xi=1  ;
271200130510               for  xi = 1  to  %len( %trimr( vidtrr ) );
271300130510                 if  %subst( vidtrr : xi : 1 ) = *blank;
271400130510                   vidtrr = %subst( vidtrr : 1 : xi - 1 ) +
271500130510                            %subst( vidtrr : xi + 1 );
271600130510                   xi -= 1;
271700130510                 endif;
271800130510               endfor;
271900130510             endif;
272000130510      /end-free
272100130424     c                   if        vidtrm<>savtrm
272200130424     c                   clear                   wforzM1
272300130424     c                   movel     vidtrm        savtrm
272400130424     c                   endif
272500130424     c
272600130424    2c                   if        vidtrm<>*blanks and vidpdr>*zeros
272700130509     c                             and wforzm1=' '  and
272800130509   2ac                             vidttr<>'H' and vidttr<>'D'
272900130509     c                             and vidttr<>'L'
273000130424     c                   eval      kpdr=%int(vidpdr)
273100130424     c     katm          chain     aiatm01l
273200130424    3c                   if        (not %found(aiatm01l)
273300130424     c                             or   atmdtfir>0)
273400130510     c* in immissione o variazione
273500130510     c                   if        *in80 or fgvtrm<>vidtrm
273600130510     C                   MOVEL     MSG(109)      VIDMSG
273700130510     c                   else
273800130510     C                   MOVEL     MSG(112)      VIDMSG
273900130510     c                   endif
274000130510     c                   eval      WforzM1='A'
274100130424     C                   SETON                                        905428
274200130424     C                   GOTO      ENDCT2
274300130509   x3c                   else
274400130509     c
274500130509     c* Se accreditato vedo se ha la data di controllo --> errore forzabile
274600130509    4c                   if        ATMDTCON=0
274700130509     C                   MOVEL     MSG(110)      VIDMSG
274800130510     c                   eval      WforzM1='C'
274900130509     C                   SETON                                        905428
275000130509     C                   GOTO      ENDCT2
275100130509    4c                   endif
275200130509     c
275300130424    3c                   endif
275400130424    2c                   endif
275500130424     c
275600010928      * DIVISA
275700011204     C     VIDIPT        IFNE      *ZEROS
275800011012      * la divisa è obbligatoria
275900011012     C     VIDVIP        IFEQ      *BLANKS
276000011012     C                   SETON                                        903828
276100011012     C                   MOVEL     MSG(86)       VIDMSG
276200011012     C                   GOTO      ENDCT2
276300011012     C                   ENDIF
276400010928      * controllo se divisa valida
276500010928     C                   CLEAR                   DSCV
276600010928     C                   CLEAR                   KEY
276700010928     C                   MOVEL     'CV'          COD
276800010928     C                   MOVEL     VIDVIP        KEY
276900010928     C     KTAB          CHAIN     TABEL00F                           83
277000010928     C     *IN83         IFEQ      *ON
277100010928     C                   SETON                                        903828
277200010928     C                   MOVEL     MSG(79)       VIDMSG
277300010928     C                   GOTO      ENDCT2
277400010928     C                   ENDIF
277500010928     C                   MOVEL     TBLUNI        DSCV
277600011204     C* Se non sono in immissione e non ho variato la divisa
277700011204     C* non faccio questo controllo per non dare errore in caso di
277800011204     C* manutenzione o chiusura in gennaio di un F.V.P. di dicembre
277900011204     C     *IN80         IFEQ      *ON
278000011204     C     VIDVIP        ORNE      SAWVIP
278100010928     C     §CVTRA        IFNE      'S'
278200010928     C                   SETON                                        903828
278300010928     C                   MOVEL     MSG(80)       VIDMSG
278400011012     C                   GOTO      ENDCT2
278500010928     C                   ENDIF
278600011204     C                   ENDIF
278700011001      * se modificata la divisa e non l'importo devo dare msg di errore
278800011001     C     VIDVIP        IFNE      SAVDIV
278900011001     C     *IN37         ANDEQ     *OFF
279000011012     C     VIDVIP        ANDNE     *BLANKS
279100011015     C     SAVDIV        ANDNE     *BLANKS
279200011001     C                   SETON                                        905828
279300011001     C                   MOVEL     MSG(85)       VIDMSG
279400011001     C                   ENDIF
279500011001     C     *IN38         IFEQ      *OFF
279600011015     C     *IN58         ANDEQ     *OFF
279700011001     C                   MOVEL     VIDVIP        SAVDIV
279800011001     C                   ENDIF
279900010928      * controllo se sono ok i decimali nell'importo pattuito
280000010928     C                   MOVE      VIDIPT        DECIMI            2 0
280100010928     C     DECIMI        IFGT      *ZEROS
280200010928     C     §CVFDC        ANDNE     'S'
280300010928     C                   SETON                                        905828
280400010928     C                   MOVEL     MSG(84)       VIDMSG
280500010928     C                   ENDIF
280600010928     C                   ENDIF
280700950901     C*
280800951017     C* S E   P R O V E N G O   D A   C O P I A
280900951026     C* OPPURE SONO IN VARIAZIONE DI UN FV GIA' ABBINATO IN PRECEDENZA
281000960304    1C     VIDTRN        IFGT      0
281100950901     C*
281200950901     C* CODICE TRAZIONISTA OBBLIGATORIO
281300951024     C     VIDPDR        IFLE      *ZEROS
281400951027     C     TRNPDR        ANDGT     *ZEROS
281500951018     C                   MOVEL     MSG(9)        VIDMSG
281600951026     C                   SETON                                        904728
281700951024     C                   GOTO      ENDCT2
281800951005    2C                   ENDIF
281900950901     C* TIPO AUTOMEZZO OBBLIGATORIO
282000951005    2C     VIDTMZ        IFEQ      ' '
282100951027     C     TRNTMZ        ANDNE     ' '
282200951018     C                   MOVEL     MSG(26)       VIDMSG
282300950901     C                   SETON                                        905728
282400951024     C                   GOTO      ENDCT2
282500951005    2C                   ENDIF
282600950901     C*
282700951005     C* SE VENGONO VARIATI TIPO AUTOMEZZO E TRAZIONISTA RIPROPONGO
282800951005     C*   L'IMPORTO LISTINO: SE NON TROVO IL RECORD ERRORE FORZABILE
282900000602     C*   SOLO SE FGS    E' LA PRIMA FILIALE FERMATA
283000000612    2C     VIDFGS        IFEQ      TLTFES
283100951123     C     WTFM          ANDEQ     WFMFES
283200951025    3C     VIDTMZ        IFNE      TRNTMZ
283300951025     C     VIDPDR        ORNE      TRNPDR
283400951218     C                   MOVEL     VIDTMZ        TRNTMZ
283500951218     C                   MOVEL     VIDPDR        TRNPDR
283600951218     C                   EXSR      LEGLIS
283700951024     C*
283800951024     C* 43 ON - ILLUMINO IMPORTO LISTINO PERCHE' CAMBIATO
283900951218    4C     *IN83         IFEQ      *OFF
284000951024    5C     TLLILI        IFNE      VIDIPT
284100010920     C                   MOVEL     TLLDIV        VIDVIP
284200010920     C                   Z-ADD     TLLILI        VIDIPT
284300010920     C                   MOVEL     TLLDIV        TRNVIP
284400010920     C                   Z-ADD     TLLILI        TRNIPT
284500951024     C                   SETON                                        9043
284600951024     C                   GOTO      ENDCT2
284700951024    5C                   ENDIF
284800951024     C*
284900951024   X4C                   ELSE
285000951005     C*
285100951005     C* SE ARRIVO A QUESTO PUNTO SIGNIFICA CHE NON HO TROVATO IL RECORD
285200951005     C*   NEL FILE DEI LISTINI QUINDI AVVISO
285300010920     C                   CLEAR                   TRNVIP
285400951025     C                   CLEAR                   TRNIPT
285500951005     C                   MOVEL     MSG(27)       VIDMSG
285600951005     C                   SETON                                        905828
285700951024     C                   GOTO      ENDCT2
285800951024    4C                   ENDIF
285900951024    3C                   ENDIF
286000951024    2C                   ENDIF
286100951027     C*
286200951027     C* TIPO TRAINO NON SI PUO' MODIFICARE
286300951027    2C     WFORZ7        IFEQ      ' '
286400951027    3C     TRNUTF        IFEQ      'S'
286500951027     C     TLTTTR        ANDNE     VIDTTR
286600951027     C     TRNUTF        ORNE      'S'
286700951027     C     VIDTTR        ANDNE     ' '
286800951027     C                   MOVEL     MSG(25)       VIDMSG
286900951027     C                   MOVEL     'S'           WFORZ7            1
287000951027     C                   SETON                                        904228
287100951027     C                   GOTO      ENDCT2
287200951027    3C                   ENDIF
287300951027    2C                   ENDIF
287400960130     C* CATEGORIA TRAINO NON SI PUO' MODIFICARE
287500000406     C* MA SE DEFLUENZA ESTERO O DPD MI DISINTERESSO
287600960130    2C     WFORZD        IFEQ      ' '
287700960130     C     TLTBIS        ANDNE     VIDBIS
287800000405     C     §TVDEF        ANDNE     'S'
287900000405    2C     WFORZD        OREQ      ' '
288000000405     C     TLTBIS        ANDNE     VIDBIS
288100020213     c                   if        okestero = *blanks
288200960130     C                   MOVEL     MSG(65)       VIDMSG
288300960130     C                   MOVEL     'S'           WFORZD            1
288400960318     C                   SETON                                        904128
288500960130     C                   GOTO      ENDCT2
288600020213     c                   endif
288700960130    2C                   ENDIF
288800110530     c* se traino trasformato in BIS e punto di scambio con traino
288900110530     c*  elimino anche punto di scambio
289000110530     c                   if        tltbis<>vidbis and vidbis<>*blanks
289100110530     c                             and vidpsc<>*blanks and vidtrns>0
289200110530     C                   MOVEL     MSG(103)      VIDMSG
289300110530     C                   SETON                                        904128
289400110530     c                   clear                   vidtrns
289500110601     c                   clear                   destrns
289600110530     c                   clear                   vidpsc
289700130412     c                   clear                   vidlnar
289800130412     c                   clear                   deslnar
289900130412     c                   clear                   deslnar2
290000110601     c* non devo dare errore per punto di scmabio modificato
290100110601     c                   clear                   savpsc
290200110601     c                   eval      WforzSC='S'
290300110601     c                   clear                   despsc
290400110530     c                   clear                   viddgs
290500110530     c                   clear                   vidhhs
290600110530     C                   GOTO      ENDCT2
290700110530     c                   endif
290800110530     c
290900960126     C***** IMPORTO PATTUITO   *****
291000951025     C     VIDIPT        IFNE      SAVIPT
291100010920     C     VIDVIP        ORNE      SAVVIP
291200951025     C                   CLEAR                   WFORZ4
291300951025     C                   CLEAR                   WFORZ5
291400010920     C                   Z-ADD     VIDIPT        SAVIPT
291500010920     C                   MOVEL     VIDVIP        SAVVIP
291600951025     C                   ENDIF
291700060321     C* FILIALE CHE DEVE ESPORRE IL FV  se non è un bis
291800060321     c     vidbis        ifeq      ' '
291900060321    2C     TLTFES        andeq     VIDFGS
292000951123     C     WTFM          ANDEQ     WFMFES
292100951024    3C     VIDIPT        IFEQ      0
292200951018     C                   MOVEL     MSG(24)       VIDMSG
292300950901     C                   SETON                                        905828
292400951024     C                   GOTO      ENDCT2
292500951024    3C                   ENDIF
292600951025     C* SE VARIATO L'IMPORTO DA QUELLO MEMORIZZATO DAL LISTINO--> ERROR
292700951025     C     WFORZ5        IFEQ      ' '
292800951025     C     VIDIPT        ANDNE     TRNIPT
292900951025     C                   MOVEL     MSG(33)       VIDMSG
293000951025     C                   SETON                                        905828
293100951025     C                   MOVEL     'S'           WFORZ5            1
293200951025     C                   GOTO      ENDCT2
293300951025    3C                   ENDIF
293400010920     C* SE VARIATA LA DIVISA DA QUELLA MEMORIZZATA DAL LISTINO--> ERROR
293500010920     C     WFORZ5        IFEQ      ' '
293600010920     C     VIDVIP        ANDNE     TRNVIP
293700010920     C                   MOVEL     MSG(78)       VIDMSG
293800010920     C                   SETON                                        905828
293900010920     C                   MOVEL     'S'           WFORZ5            1
294000010920     C                   GOTO      ENDCT2
294100010920    3C                   ENDIF
294200010920     C*
294300951024   X2C                   ELSE
294400951024     C*
294500951024    3C     WFORZ4        IFEQ      ' '
294600951024    4C     VIDIPT        IFGT      0
294700061116     c     wforzd        andeq     'S'
294800951024     C                   MOVEL     MSG(12)       VIDMSG
294900951024     C                   SETON                                        905828
295000951024     C                   MOVEL     'S'           WFORZ4            1
295100951024     C                   GOTO      ENDCT2
295200951024    4C                   ENDIF
295300950901     C*
295400951024    3C                   ENDIF
295500951024    2C                   ENDIF
295600951024    1C                   ENDIF
295700110512     c
295800960129     C*
295900960129     C**** PIOMBI ****
296000960205     C* OBBLIGATORI IN CHIUSURA FV - NO SE E' LOCALE
296100130315     C   01ilst4ges      IFEQ      'C'
296200130315     C     ilst4ges      OREQ      'I'
296300960129     C     VIDPMB        IFEQ      *BLANKS
296400960205     C     VIDTTR        ANDNE     'L'
296500960129     C                   MOVEL     MSG(39)       VIDMSG
296600960129     C                   SETON                                        539028
296700960129     C                   GOTO      ENDCT2
296800960129     C                   ENDIF
296900960129     C                   ENDIF
297000950901     C*
297100951018     C****  DATA / ORA  PARTENZA EFFETTIVA  ****
297200951026     C** DATA ORA PARTENZA: OBBLIGATORI IN CHIUSURA FV
297300130315    1C   01ilst4ges      IFEQ      'I'
297400130315     C     ilst4ges      OREQ      'C'
297500951026    2C     VIDDTP        IFEQ      0
297600960214     C     VIDTTR        ANDNE     'L'
297700951026     C                   SETON                                        469028
297800951026     C                   MOVEL     MSG(38)       VIDMSG
297900951026     C                   GOTO      ENDCT2
298000951026    2C                   ENDIF
298100951026    1C                   ENDIF
298200951103     C*
298300951103     C* UNA VOLTA IMMESSA LA DATA/ORA PARTENZA NON SI PUO' PIU'
298400951106     C*   TOGLIERE MA SOLO MODIFICARE
298500951106    1C  N80FGVDTP        IFGT      0
298600951103     C     VIDDTP        ANDEQ     0
298700951103     C                   SETON                                        469028
298800960126     C                   MOVEL     MSG(64)       VIDMSG
298900951103     C                   GOTO      ENDCT2
299000951103    1C                   ENDIF
299100951026     C*
299200951025     C                   Z-ADD     *ZERO         AMGDTP
299300951018    1C     VIDDTP        IFNE      *ZERO
299400950324     C     VIDDTP        OREQ      *ZERO
299500950324     C     VIDHMP        ANDNE     *ZERO
299600900515     C                   MOVE      VIDDTP        G02DAT
299700900515     C                   MOVEL     *BLANK        G02ERR
299800940909     C                   CALL      'XSRDA8'
299900900515     C                   PARM                    WLBDAT
300000951018    2C     G02ERR        IFEQ      '1'
300100951026     C                   SETON                                        469028
300200951026     C                   MOVEL     MSG(38)       VIDMSG
300300951024     C                   GOTO      ENDCT2
300400951018    2C                   ENDIF
300500940909     C                   Z-ADD     G02DAT        VIDDTP
300600960122     C                   Z-ADD     G02INV        AMGDTP
300700930225     C*
300800930225     C* CONTROLLO DATA COMPRESA NEL RANGE
300900951027    2C     AMGDTP        IFLT      DTPMIN
301000951027     C     AMGDTP        ORGT      DTPMAX
301100951027     C                   MOVEL     MSG(45)       VIDMSG
301200951026     C                   SETON                                        469028
301300951024     C                   GOTO      ENDCT2
301400951018    2C                   ENDIF
301500951026     C*
301600951026     C* ORA OBBLIGATORIA SE IMMESSA DATA
301700960126    2C     VIDHMP        IFEQ      0
301800951026     C                   MOVEL     MSG(13)       VIDMSG
301900951026     C                   SETON                                        409028
302000951026     C                   GOTO      ENDCT2
302100960126    2C                   ENDIF
302200940913     C*
302300951018    1C                   ENDIF
302400951018     C*
302500951018     C* CONTROLLO ORA PARTENZA EFFETTIVA
302600910809     C                   MOVEL     VIDHMP        HM
302700951018    1C     ORA           IFGT      24
302800900515     C     MIN           ORGT      59
302900951018     C                   MOVEL     MSG(13)       VIDMSG
303000951026     C                   SETON                                        409028
303100951024     C                   GOTO      ENDCT2
303200951018    1C                   ENDIF
303300951027     C*
303400951103     C****  CAUSALE RITARDO/ANTICIPO  ****
303500960126     C* SE INSERITA DATA/ORA PARTENZA EFFETTIVA
303600951120    1C     VIDDTP        IFGT      0
303700960126     C                   MOVEL     '1'           WFORZC
303800951106     C*
303900951106     C* RICERCA CAUSALE RITARDO/ANTICIPO
304000951103    2C     VIDRTC        IFEQ      '?'
304100951103     C                   MOVEL     'TC'          COD
304200951103     C                   MOVEL     *BLANKS       VIDRTC
304300951103     C                   MOVEL     *BLANKS       DES
304400951103     C                   CALL      'X§TABER'
304500951103     C                   PARM                    CODUT
304600951103     C                   PARM                    COD
304700951103     C                   PARM                    KEY
304800951103     C                   PARM                    DES
304900951103     C                   MOVEL     KEY           VIDRTC
305000960208     C                   MOVEL     DES           DESRT1
305100960208     C                   MOVE      DES           W008A             8
305200960208     C                   CLEAR                   DESRT2
305300960208     C                   MOVEL     W008A         DESRT2
305400951103     C                   SETON                                        9059
305500951103     C                   GOTO      ENDCT2
305600951103    2C                   ENDIF
305700951103     C*
305800951103     C* CONTROLLO VALIDITA' CAUSALE
305900951103    2C     VIDRTC        IFNE      ' '
306000951103     C                   MOVEL     'TC'          COD
306100951103     C                   MOVEL(P)  VIDRTC        KEY
306200951103     C     KTAB          CHAIN     TABEL                              83
306300951103    3C     *IN83         IFEQ      *OFF
306400951103     C     TBLFLG        ANDNE     ' '
306500951103     C                   SETON                                        83
306600951103    3C                   ENDIF
306700951103     C*
306800951103    3C     *IN83         IFEQ      *ON
306900951103     C                   MOVEL     MSG(53)       VIDMSG
307000951103     C                   SETON                                        599028
307100951103     C                   GOTO      ENDCT2
307200951103   X3C                   ELSE
307300960208     C                   MOVEL     TBLUNI        DSTC
307400960208     C                   MOVEL     §TCDES        DESRT1
307500960208     C                   MOVE      §TCDES        DESRT2
307600951103    3C                   ENDIF
307700951103     C*
307800951103   X2C                   ELSE
307900960208     C                   CLEAR                   DESRT1
308000960208     C                   CLEAR                   DESRT2
308100951103    2C                   ENDIF
308200951103     C*
308300960122     C* SE DATA/ORA PARTENZA REALE <> DALLA TEORICA, VERIFICO SE DEVO
308400960122     C*  CHIEDERE IL RITARDO
308500960126    2C     VIDTRN        IFGT      0
308600960126     C* SE MANCA LA DATA TEORICA LA CALCOLO
308700960126     C* RICALCOLO SOLO SE HO MODIFICATO LA DATA PARTENZA EFFETTIVA
308800960126    3C     SAVDTP        IFNE      AMGDTP
308900960126     C     SAVHMP        ORNE      VIDHMP
309000960126     C                   CLEAR                   WFORZC
309100960126     C*
309200960126    4C     AMGDTP        IFNE      AMGDTT
309300960126     C     VIDHMP        ORNE      VIDHMT
309400960122     C                   EXSR      CALDIF
309500960126     C                   ELSE
309600960126     C* SONO UGUALI: NON C'E' RITARDO
309700960126     C                   SETOFF                                       18
309800960126    4C                   ENDIF
309900960126     C                   MOVEL     AMGDTP        SAVDTP
310000960126     C                   MOVEL     VIDHMP        SAVHMP
310100960126    3C                   ENDIF
310200960126    2C                   ENDIF
310300951106     C* SE PARTENZA IN RITARDO/ANTICIPO --> CAUSALE OBBLIGATORIA
310400960126    2C   18VIDRTC        IFEQ      ' '
310500951107     C                   MOVEL     MSG(56)       VIDMSG
310600951106     C                   SETON                                        599028
310700951106     C                   GOTO      ENDCT2
310800951106    2C                   ENDIF
310900951106     C*
311000951107     C* SE MODIFICANDO LA DATA/ORA PARTENZA EFFETTIVA IL RITARDO/ANTIC.
311100951107     C*   SCOMPARE, CHIEDERE FORZABILE DI ELIMINARE LA CAUSALE
311200960126    2C  N18WFORZC        IFEQ      ' '
311300951106     C     VIDRTC        ANDNE     ' '
311400951106     C                   MOVEL     MSG(52)       VIDMSG
311500951106     C                   SETON                                        905928
311600960126     C                   MOVEL     '1'           WFORZC            1
311700951106     C                   GOTO      ENDCT2
311800951106    2C                   ENDIF
311900951106     C*
312000951107     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
312100960123     C                   MOVEL     'P'           KEPA
312200960123     C                   MOVEL     'R'           KTRC
312300960123     C                   CLEAR                   KTFA
312400951107     C                   EXSR      CTRDRA
312500951103     C*
312600951103     C* SE IMMESSA LA "G" DI CAUSALE GENERICA, RICHIEDERE OBBLIGATORIA
312700951107     C*   LA DESCRIZIONE RITARDO/ANTICIPO
312800951103    2C     VIDRTC        IFEQ      'G'
312900960126     C* 16 - OFF --> NON ESISTE LA DESCRIZIONE RITARDO/ANTICIPO
313000960126     C     *IN16         ANDEQ     *OFF
313100951108     C     *INKV         ANDEQ     *OFF
313200951103     C                   MOVEL     MSG(54)       VIDMSG
313300951103     C                   SETON                                        599028
313400951103     C                   GOTO      ENDCT2
313500951103    2C                   ENDIF
313600951103     C*
313700951120   X1C                   ELSE
313800951120     C* DATA/ORA REALE = 0
313900951120    2C     VIDRTC        IFNE      ' '
314000951120     C                   MOVEL     MSG(62)       VIDMSG
314100951120     C                   SETON                                        469028
314200951120     C                   GOTO      ENDCT2
314300951120    2C                   ENDIF
314400951103    1C                   ENDIF
314500960126     C*
314600960126     C* SE INSERITA LA DESCRIZIONE, INSERIRE ANCHE LA CAUSALE RITARDO
314700960126     C     *IN16         IFEQ      *ON
314800960126     C     VIDRTC        ANDEQ     *BLANKS
314900960126     C                   MOVEL     MSG(57)       VIDMSG
315000960126     C                   SETON                                        599028
315100960126     C                   GOTO      ENDCT2
315200960126     C                   ENDIF
315300940913     C*
315400951024     C     ENDCT2        ENDSR
315500110530     C*-----------------------------------------------------***********
315600130729     C* Gestione videata punti di scambio
315700110530     C*-----------------------------------------------------***********
315800110530     C     GesPsc        BEGSR
315900110530     c
316000110530     c     forPsc        TAG
316100110530     c                   if        Emisd06='1'
316200130315     C                   WRITE     lst4T01
316300130315     C                   WRITE     lst4d06
316400110530     c                   clear                   emisD06
316500110530     c                   endif
316600110530
316700130315     C                   EXFMT     lst4d07
316800110530     c                   setoff                                       747576
316900110530     c                   setoff                                       772890
317000110530     c*
317100130315    1C     ilst4ges      IFNE      'N'
317200110530     c                   exsr      ctrPsc
317300110531     c                   endif
317400110530     c
317500110530     C* F12 - RITORNO ma senza errori
317600110530     c                   if        *inkl and not *in90
317700110530     c                   leavesr
317800110530     c                   endif
317900110530
318000130315    1C     ilst4ges      IFNE      'N'
318100110530     c                   goto      forPSc
318200110531     c                   endif
318300110530     c
318400110530     c                   ENDSR
318500110530     C*-----------------------------------------------------***********
318600110530     C* CONTROLLi per punti di scambio
318700110530     C*-----------------------------------------------------***********
318800110530     C     CTRPsc        BEGSR
318900110530     c*
319000110530     c* controllo codice
319100110530     c                   clear                   despsc
319200110530     c                   clear                   despind
319300110530     c                   clear                   desploc
319400110530     c
319500110530     c                   if        vidtrn>0
319600110530     c                   exsr      LetTLS
319700110530     c                   else
319800110531     c                   clear                   dstls
319900110530     c                   endif
320000110530     c
320100110530    1c                   if        vidpsc<>*blanks
320200110530     c* col '?'  ricerca
320300110530     C                   EVAL      INDX=%scan('?':vidpsc)
320400110530     c
320500110530    2c                   if        indx>0
320600110530     c* imposta la ds di procedura
320700110530     c                   CLEAR                   tntlp0ds
320800110530     c                   MOVE      '01'          dp0op0
320900110530     c                   MOVEL     *blanks       dp0op1
321000110530     c                   MOVEL     '0'           dp0f03
321100110530     c                   MOVEL     '0'           dp0f12
321200110530     c                   MOVEL     '0'           dp0err
321300110530     c                   call      'TNTLP0R'
321400110530     c                   parm                    kpjba
321500110530     c                   parm                    TNTLp0ds
321600110530     c
321700110530     c                   eval      vidpsc=dp0psc
321800110530     c                   eval      despsc=dp0loc
321900110530     c                   eval      despind=dp0ind
322000110530     c                   eval      desploc=%trim(dp0cap) + ' ' +
322100110531     c                             %trim(dp0loc)+'  ' +dp0prv+dp0nar
322200130412     c                   eval      Dpscflo=dp0flo
322300130412     c                   if        §pscflr>*zeros   and vidlnar<=*zeros
322400130412     c                   movel     §pscflr       vidlnar
322500130412     c                   movel     §pscflr       codfil
322600130412     c                   exsr      ctrfil
322700130412     c                   if        not *in34
322800130412     c                   movel     orgdes        deslnar
322900130412     c                   movel     orgdes        deslnar2
323000130412     c                   endif
323100130412     c                   endif
323200110530     c
323300110530     C                   SETON                                        7490
323400110530     c                   eval      emisD06='1'
323500110530     C                   leavesr
323600110530    2c                   endif
323700110530     c
323800110530     c     vidpsc        chain     tnpsc01l
323900110530    2c                   if        not %found(tnpsc01l)
324000110530     C                   MOVEL     MSG(92)       vidmsg
324100110530     C                   SETON                                        749028
324200110530     C                   leavesr
324300110530E   2C                   ENDIF
324400110530     c
324500110530     c                   eval      despsc=pscloc
324600110530     c                   eval      despind=pscind
324700110530     c                   eval      desploc=%trim(psccap) + ' ' +
324800110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
324900130412     c                   eval      Dpscflo=pscflo
325000130412     c                   if        §pscflr>*zeros   and vidlnar<=*zeros
325100130412     c                   movel     §pscflr       vidlnar
325200130412     c                   movel     §pscflr       codfil
325300130412     c                   exsr      ctrfil
325400130412     c                   if        not *in34
325500130412     c                   movel     orgdes        deslnar
325600130412     c                   movel     orgdes        deslnar2
325700130412     c                   endif
325800130412     c                   endif
325900110530     c
326000110530     c
326100110530     c* controllo se scaduto
326200110530    2c                   if        pscdsc>0
326300110530     c                             and amgdfv>pscdsc
326400110530     C                   MOVEL     MSG(92)       vidmsg
326500110530     C                   SETON                                        749028
326600110530     C                   leavesr
326700110530    2c                   endif
326800110530    1c                   endif
326900110530     c
327000110530     c* Se presente data/ora o lna finale --> presente anche punti scam
327100110530     c                   if        vidpsc=*blanks  and
327200110530     c                             (viddgs>0 or vidhhs>0 or (vidlnar<>*blanks
327300110530     c                             and  vidlnar<>*zeros))
327400110530     C                   MOVEL     MSG(93)       vidmsg
327500110530     C                   SETON                                        749028
327600110530     C                   leavesr
327700110530     c                   endif
327800110530     c
327900110530     c                   clear                   amgdGS
328000110607     c* data e ora obbligatori se presente traino di scambio
328100110610     c                   if        vidtrns>0 and viddgs=0  and vidpsc<>*blanks
328200110610     c                             and vidlnar<=*zeros
328300110607     C                   SETON                                        759028
328400110607     C                   MOVEL     MSG(99)       VIDMSG
328500110607     C                   leavesr
328600110607    2C                   ENDIF
328700110607     c
328800110530    1c                   if        viddgs>0   or vidhhs>0
328900110530     C                   MOVE      VIDdgs        G02DAT
329000110530     C                   MOVEL     *BLANK        G02ERR
329100110530     C                   CALL      'XSRDA8'
329200110530     C                   PARM                    WLBDAT
329300110530    2C     G02ERR        IFEQ      '1'
329400110530     C                   SETON                                        759028
329500110530     C                   MOVEL     MSG(94)       VIDMSG
329600110530     C                   leavesr
329700110530    2C                   ENDIF
329800110530    c
329900110530     C                   Z-ADD     G02DAT        VIDdgs
330000110530     C                   Z-ADD     G02INV        AMGDgs            8 0
330100110530     c
330200110530     c* Deve essere >= data foglio viaggio
330300110530    2c                   if        amgdgs<amgdfv
330400110530     C                   SETON                                        759028
330500110530     C                   MOVEL     MSG(95)       VIDMSG
330600110530     C                   leavesr
330700110530    2C                   ENDIF
330800110530     C* CONTROLLO DATA COMPRESA NEL RANGE
330900110530    2C     AMGdgs        IFLT      DTPMIN
331000130729     C     AMGDgs        ORGT      DTPMAX
331100110530     C                   MOVEL     MSG(45)       VIDMSG
331200110530     C                   SETON                                        759028
331300110530     C                   leavesr
331400110530    2C                   ENDIF
331500110530    c
331600110530     C* ORA OBBLIGATORIA SE IMMESSA DATA
331700110530    2C     VIDHhs        IFEQ      0
331800110530     C                   MOVEL     MSG(13)       VIDMSG
331900110530     C                   SETON                                        769028
332000110530     C                   leavesr
332100110530    2C                   ENDIF
332200110530    1C                   ENDIF
332300110530     c
332400110530     c* Ora scambio
332500110530    1c                   if        vidhhs>0
332600110530     C                   MOVEL     VIDhhs        HM
332700110530    2C     ORA           IFGT      24
332800110530     C     MIN           ORGT      59
332900110530     C                   MOVEL     MSG(13)       VIDMSG
333000110530     C                   SETON                                        769028
333100110530     C                   leavesr
333200110530    2C                   ENDIF
333300110530    1C                   ENDIF
333400110530     c*
333500110530     C* Filiale finale arrivo autista
333600110530     C     '?'           SCAN      VIDLNAr                                77
333700110530    1C     *IN77         IFEQ      *ON
333800110530     C                   EXSR      CASD19
333900110530     C                   MOVEL     §COD          VIDLNAr
334000110530     C                   MOVEL     §§DES         DESLNAr
334100110601     C                   MOVEL     §§DES         DESLNAr2
334200110530     C                   SETON                                        90
334300110530     c                   eval      emisD06='1'
334400110530     C                   leavesr
334500110530    1C                   ENDIF
334600110530     C*
334700110530     C* CONTROLLO CHE SIA UNA FILIALE DELL'ORGANIGRAMMA
334800110530     C                   MOVEL     *BLANKS       DESLNAR
334900110601     C                   MOVEL     *BLANKS       DESLNAR2
335000110531    1c                   if        vidlnar<>*blanks and vidlnar<>*zeros
335100110530     c
335200110530    2c                   if        vidlnar>*zeros  and
335300110530     c                             %subst(vidlnar:3:1)>='0'
335400110530     C                   MOVE      VIDLNAr       CODFIL
335500110530     C                   EXSR      CTRFIL
335600110530     C*
335700110530    3C     *IN34         IFEQ      *ON
335800110530     C                   MOVEL     *ALL'?'       DESLNAr
335900110530     C                   MOVEL     MSG(8)        VIDMSG
336000110530     C                   SETON                                        287790
336100110530     C                   leavesr
336200110530     c
336300110530   X3C                   ELSE
336400110530     c
336500110530     C                   MOVEL     ORGDES        DESLNAr
336600110601     C                   MOVEL     ORGDES        DESLNAr2
336700110530     c* non deve essere fil estera
336800110530    4c                   if        esteroedpd='S'
336900110530     C                   MOVEL     MSG(8)        VIDMSG
337000110530     C                   SETON                                        287790
337100110530     c                   leavesr
337200110530    4c                   endif
337300110530    3c                   endif
337400110530     c
337500110530   x2c                   else
337600110530     C                   MOVEL     *ALL'?'       DESLNAr
337700110530     C                   MOVEL     MSG(8)        VIDMSG
337800110530     C                   SETON                                        287790
337900110530     C                   leavesr
338000110530    2c                   endif
338100110530    1c                   endif
338200110530     c
338300110531     c* se c'e' traino di scambio lo decodifico
338400110601     c                   clear                   destrns
338500110531     c                   if        vidtrns>0
338600110531     c                   exsr      CtrTRNS
338700110531     c                   endif
338800110530
338900110530     c* Se cambiato punto di scambio --> sbino
339000110530    1C     VIDTRN        IFGT      0
339100110530     c
339200110530     c                   if        vidpsc<>savpsc
339300110530     C                   CLEAR                   WFORZsc
339400110530     c                   eval      savpsc=vidpsc
339500110530    2c                   endif
339600110530     c
339700110530     c                   if        (vidlnar>*zeros and savlnar<=*zeros) or
339800110607     c                             (vidlnar<=*zeros and savlnar>*zeros)
339900110530     C                   CLEAR                   WFORZlnar
340000110530     c                   eval      savlnar=vidlnar
340100110530    2c                   endif
340200110530     c
340300110530    2c                   if        wforzsc=' ' and vidpsc<>tlspsc
340400110530     C                   MOVEL     MSG(96)       VIDMSG
340500110530     C                   SETON                                        749028
340600110531     c                   clear                   vidtrns
340700110531     c                   clear                   destrns
340800110530     C                   MOVEL     'S'           WFORZsc           1
340900110530     C                   leavesr
341000110530    2c                   endif
341100110531     c
341200110531     c* se rimessi uguali, reimposto il traino di scambio
341300110610     c                   if        vidpsc=tlspsc and vidlnar<=*zeros
341400110531     c                   eval      vidtrns=tlstrns
341500110531     c                   endif
341600110531     c
341700110531     c* Obbligatoria la linea di arrivo finale del mezzo
341800110531    2c                   if        wForzsc='S' and vidlnar<=*zeros
341900110601     c                             and vidpsc<>*blanks
342000110531     C                   MOVEL     MSG(104)      VIDMSG
342100110531     C                   SETON                                        779028
342200110531     C                   leavesr
342300110531   x2c                   else
342400110530     c* Se aggiunta linea arrivo finale --> sbino
342500130412    3c                   if        wforzsc=' '   and vidtrns>0 and
342600110531    3c                             wforzlnar=' ' and vidlnar>*zeros
342700110530     C                   MOVEL     MSG(101)      VIDMSG
342800110531     C                   SETON                                        779028
342900110610     c                   clear                   vidtrns
343000110610     c                   clear                   destrns
343100110530     C                   MOVEL     'S'           WFORZlnar         1
343200110530     C                   leavesr
343300110531    3c                   endif
343400110531    2c                   endif
343500110531     c
343600110531   x1c                   else
343700110531     c* In manutenzione
343800110531     c* se traino già sbinato, cambio  punto di scambio  e c'e'
343900110531     c*  num traino di scambio--> lo tolgo
344000110531    2c  n80              if        oldpsc<>*blanks and oldpsc<>vidpsc and
344100110531     c                             vidtrns>0
344200110531     c                   clear                   vidtrns
344300110610     c                   clear                   destrns
344400110531     c                   seton                                        90
344500110531    2c                   endif
344600110531    c
344700110531    2c  n80              if        oldpsc>*blanks and oldpsc= vidpsc and
344800110531     c                             vidtrns=0 and vidlnar<=*zeros
344900110531     c                             and fgstrns>0
345000110531     c                   eval      vidtrns=fgstrns
345100110531     c                   seton                                        90
345200110531    2c                   endif
345300110531     c
345400110531     c* Se aggiunta linea finale di arrivo --> tolgo il traino di scambio
345500110531    2c                   if        vidtrns>0 and vidlnar>*zeros
345600110531     C                   MOVEL     MSG(98)       VIDMSG
345700110531     C                   SETON                                        779028
345800110610     c                   clear                   destrns
345900110531     c                   clear                   vidtrns
346000110531     C                   leavesr
346100110531    2c                   endif
346200110531    1c                   endif
346300110531     c
346400110531     c* se manca sia traino di scambio che linea di arrivo finale del mezzo -->
346500110531     c*  Errore
346600110531     c                   if        vidtrns=0 and vidlnar<=*zeros
346700110531     c                             and vidpsc<>*blanks
346800110531     C                   MOVEL     MSG(97)       VIDMSG
346900110531     C                   SETON                                        779028
347000110531     C                   leavesr
347100110531     c                   endif
347200110530     c
347300110530     C                   ENDSR
347400110513     C*-----------------------------------------------------***********
347500110513     C* CONTROLLO Traino da cui copiare-uso sql per non sporcarmi i campi
347600110513     C*-----------------------------------------------------***********
347700110513     C     CTRtrns       BEGSR
347800110513     c                   clear                   woktrns           1
347900110513     c                   eval      wrkgetlista =
348000110513     c                             'select tltdde, tltdsc, tlttfp, tlttfa +
348100110513     c                               from tntlt00f where tltatb= '' '' +
348200110513     c                               and tltdde<=' + %editc(amgdfv:'X') +
348300110530     c                               ' and tlttrn= ' + %editc(vidtrns:'X')
348400110513
348500110513     C/EXEC SQL
348600110513     C+ PREPARE s1 FROM :wrkgetlista
348700110513     C/END-EXEC
348800110513     C
348900110513     C/EXEC SQL
349000110513     C+ DECLARE A1 CURSOR FOR S1
349100110513     C/END-EXEC
349200110513     C
349300110513     C/EXEC SQL
349400110513     C+ OPEN a1
349500110513     C/END-EXEC
349600110513
349700110513      *?Leggo il file
349800110513     c                   do        *hival
349900110513     C/EXEC SQL
350000110513     C+ FETCH NEXT FROM a1 INTO : Cdde, :Cdsc, :Ctfp, :Ctfa
350100110513     C/END-EXEC
350200110513
350300110513     c                   select
350400110513
350500110513     c                   when      sqlcod = 100
350600110513     c                   leave
350700110513     c                   when      sqlcod < 0
350800110513     c                   seton                                        H1
350900110513     c                   leave
351000110513     c                   other
351100110513     c
351200110513     c                   if        Cdsc>=amgdfv  and Cdde<=amgdfv
351300110513     c                   eval      woktrns='S'
351400110513     c* Decodico  partenza e arrivo
351500110513     c                   eval      destrns='Da '+%editc(Ctfp:'X') + ' a '+
351600110513     c                             %editc(Ctfa:'X')
351700110513     c                   endif
351800110513     c
351900110513     c                   endsl
352000110513
352100110513     c                   enddo
352200110513     c* errore
352300110513     c                   if        woktrns=' '
352400110513     c                   seton                                        772890
352500110513     c                   eval      vidmsg=msg(100)
352600110513     c                   endif
352700110513
352800110513     C/EXEC SQL
352900110513     C+ CLOSE a1
353000110513     C/END-EXEC
353100110513
353200110513     c                   ENDSR
353300951218     C*-----------------------------------------------------***********
353400951218     C* CONTROLLO DEL TIPO AUTOMEZZO E DEL TRAZIONISTA
353500951218     C*-----------------------------------------------------***********
353600951218     C     CTRPAR        BEGSR
353700951218     C****  TIPO AUTOMEZZO  ****
353800951218     C* OBBLIGATORIO IN COPIA DA TRAINO/BORDERO'/CHIUSURA FV
353900951218     C* RICERCA TIPO AUTOMEZZO
354000951218    1C     VIDTMZ        IFEQ      '?'
354100951218     C                   MOVEL     'TU'          COD
354200951218     C                   MOVEL     *BLANKS       VIDTMZ
354300951218     C                   MOVEL     *BLANKS       DES              25
354400951218     C                   CALL      'X§TABER'
354500951218     C                   PARM                    CODUT
354600951218     C                   PARM                    COD
354700951218     C                   PARM                    KEY
354800951218     C                   PARM                    DES
354900951218     C                   MOVEL     KEY           VIDTMZ
355000951218     C                   MOVEL     DES           DESTMZ
355100130502     C                   MOVEL     DES           DESTMZ2
355200951218     C                   SETON                                        9057
355300951218     C                   GOTO      ENDPAR
355400951218    1C                   ENDIF
355500951218     C*
355600951218     C* SE IMMESSO CONTROLLO VALIDITA' TIPO AUTOMEZZO
355700960126     C* IN 2 VIDEATA SE CHIUSURA FV
355800951218    1C     VIDTMZ        IFNE      ' '
355900951218     C     *IN01         OREQ      *ON
356000960126     C     WRECOR        ANDNE     '1'
356100951218     C                   EXSR      CTRTMZ
356200951218    2C     *IN83         IFEQ      *ON
356300951218     C                   MOVEL     MSG(26)       VIDMSG
356400951218     C                   SETON                                        905728
356500951218     C                   GOTO      ENDPAR
356600951218    2C                   ENDIF
356700951218    1C                   ENDIF
356800951218     C*
356900951218     C****  CODICE TRAZIONISTA  ****
357000951218     C* OBBLIGATORIO IN COPIA DA TRAINO
357100951218     C* IN BORDERO'/CHIUSURA FV SI PUO' ANCHE SOLO INSERIRE LA DESCRIZ
357200951218     C* RICERCA CODICE TRAZIONISTA
357300951218     C     '?'           SCAN      VIDPDR                                 83
357400951218    1C     *IN83         IFEQ      *ON
357500000807     C                   MOVEL     'T '          PA3TIP
357600000807     C                   MOVEL(P)  VIDDPD        PA3RSC
357700000807     C                   CLEAR                   PA3CSF
357800000807     C                   CLEAR                   PA3PDR
357900000807     C                   CLEAR                   PA3FLG
358000951218     C                   MOVEL     PARAM3        KPJBU
358100951218     C                   CALL      'TRUL16R'
358200951218     C                   PARM                    KPJBA
358300951218     C                   MOVEL     KPJBU         PARAM3
358400000807     C                   MOVEL     PA3PDR        VIDPDR
358500000807     C                   MOVEL     PA3RSC        VIDDPD
358600110609     C                   SETON                                        9047
358700110609     C                   GOTO      ENDPAR
358800110609     c
358900000807     C* SE PA3FLG = "3" --> NON ESISTONO RECORDS PER CHIAVE ALFABETICA
359000951218     C*                     RICHIESTA
359100951218     C     PA3FLG        IFEQ      '3'
359200951218     C                   MOVEL     MSG(51)       VIDMSG
359300951219     C                   SETON                                        289047
359400951219     C                   GOTO      ENDPAR
359500951218     C                   ENDIF
359600951218     C*
359700951218    1C                   ENDIF
359800951218     C*
359900951218     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
360000951218    1C     VIDPDR        IFNE      *BLANKS
360100951218     C     VIDPDR        ANDNE     *ZEROS
360200951218     C*
360300951218     C                   TESTN                   VIDPDR               31
360400951218     C  N31              MOVEL     MSG(9)        VIDMSG
360500951218     C  N31              SETON                                        902847
360600951218     C  N31              GOTO      ENDPAR
360700951218     C*
360800951218     C                   MOVE      VIDPDR        KPDR
360900000807     C                   Z-ADD     KPDR          KTLPDR
361000000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
361100951218     C*
361200951218    2C     *IN31         IFEQ      *ON
361300951218     C     TLZATB        ORNE      ' '
361400951218     C                   MOVEL     MSG(9)        VIDMSG
361500951218     C                   SETON                                        904728
361600951218     C                   GOTO      ENDPAR
361700951218   X2C                   ELSE
361800960302     C*
361900960302     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
362000960302    3C     VIDPDR        IFNE      '0465000'
362100960302     C     VIDDPD        OREQ      *BLANKS
362200951218     C                   MOVEL     TLZRSC        VIDDPD
362300960302    3C                   ENDIF
362400960302    2C                   ENDIF
362500951218    1C                   ENDIF
362600951218     C* CODICE O DESCRIZIONE OBBLIGATORIO
362700960126     C   01WRECOR        IFNE      '1'
362800960126     C     VIDPDR        ANDLE     *ZEROS
362900951218     C     VIDDPD        ANDEQ     *BLANKS
363000951218     C                   MOVEL     MSG(10)       VIDMSG
363100951218     C                   SETON                                        904728
363200951218     C                   GOTO      ENDPAR
363300951218    2C                   ENDIF
363400951218     C     ENDPAR        ENDSR
363500951102     C*-----------------------------------------------------***********
363600960122     C* CONTROLLO DIFFERENZA DATA TEORICA E REALE
363700951102     C*-----------------------------------------------------***********
363800960122     C     CALDIF        BEGSR
363900951106     C                   SETOFF                                       18
364000960122     C* CALL PGM PER CALCOLO DIFFERENZA DATA ORA TEORICA CON REALE
364100020805     C                   CLEAR                   TNTL35DS
364200960122     C                   MOVEL     'C'           D35TUP
364300960122     C                   MOVEL     'P'           D35EPA
364400960122     C                   MOVEL     VIDTRN        D35TRN
364500960122     C                   MOVEL     WDDE          D35DDE
364600960122     C                   MOVEL     WDTN          D35DTN
364700960122     C                   MOVEL     AMGDTT        D35DTE
364800960126     C                   MOVEL     VIDHMT        D35HTE
364900960122     C                   MOVEL     AMGDTP        D35DRE
365000960122     C                   MOVEL     VIDHMP        D35HRE
365100020805     C                   MOVEL     TNTL35DS      KPJBU
365200960122     C                   CALL      'TNTL35R'
365300960122     C                   PARM                    KPJBA
365400020805     C                   MOVEL     KPJBU         TNTL35DS
365500960122     C     D35FLG        IFNE      ' '
365600960122     C                   SETON                                        18
365700960122     C                   ENDIF
365800951103     C*
365900951106     C                   ENDSR
366000951103     C*
366100951107     C*-----------------------------------------------------***********
366200951107     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO    * CTRDRA  *
366300951107     C*-----------------------------------------------------***********
366400951107     C     CTRDRA        BEGSR
366500951107     C                   CLEAR                   W001A
366600951107     C*
366700951107     C     KFV4          SETLL     FNFV401L                               83
366800951107    1C     *IN83         IFEQ      *ON
366900951107     C     KFV4          READE(N)  FNFV401L                               83
367000951107    2C     *IN83         DOWEQ     *OFF
367100951107    3C     FV4ATB        IFEQ      ' '
367200951107     C                   MOVEL     '1'           W001A
367300951107     C                   SETON                                        83
367400951107   X3C                   ELSE
367500951107     C     KFV4          READE(N)  FNFV401L                               83
367600951107    3C                   ENDIF
367700951107    2C                   ENDDO
367800951107    1C                   ENDIF
367900951107     C*
368000951107     C                   ENDSR
368100951107     C*
368200900515      *-----------------------------------------------------***********
368300951025      * CONTROLLI SFL                                       *  CTRD03 *
368400900515      *-----------------------------------------------------***********
368500951025     C     CTRD03        BEGSR
368600900515      *
368700951026     C                   SETOFF                                       9096
368800951026     C                   SETOFF                                       5051
368900951025     C                   CLEAR                   CTL
369000970103     C                   CLEAR                   CFS
369100970103     C                   CLEAR                   FE0
369200900529     C                   MOVEL     VIDLNA        CTL(1)
369300970102     C                   MOVEL     VIDLNA        CFS(1)
369400970102     C                   MOVEL     LNAFE0        FE0(1)
369500900515      *
369600080910      * CONTROLLO ALTRE FILIALI DEL FOGLIO
369700900515      *
369800951026     C                   Z-ADD     1             NRR3              3 0
369900000420    1C                   DO        299
370000130315     C     NRR3          CHAIN     lst4S03                            81
370100951025      *
370200951025    2C     VIDFFV        IFEQ      *BLANKS
370300900515     C     VIDFFV        OREQ      *ZEROS
370400900522     C                   MOVEL     *BLANKS       VIDFFV
370500900522     C                   MOVEL     *BLANKS       DESFFV
370600900515     C                   MOVEL     *BLANKS       TIPFFV
370700900515      *
370800951025   X2C                   ELSE
370900900522      *
371000900522      * RICERCA FILIALE
371100900522      *
371200951027     C     '?'           SCAN      VIDFFV                                 31
371300951027    3C   31              DO
371400940915     C                   SETON                                        90
371500951024     C                   EXSR      CASD19
371600900522     C                   MOVEL     §COD          VIDFFV
371700900522     C                   MOVEL     §TIP          TIPFFV
371800900530     C                   MOVEL     *BLANKS       DESFFV
371900900530     C                   MOVEL     §§DES         DESFFV
372000951025    3C                   END
372100900522      *
372200951025     C                   TESTN                   VIDFFV               83
372300951025     C                   MOVE      VIDFFV        W001A             1
372400951025    3C     *IN83         IFEQ      *ON
372500951026     C     W001A         ANDGE     '0'
372600951025     C                   MOVE      VIDFFV        CODFIL
372700951025     C                   EXSR      CTRFIL
372800951025     C                   ELSE
372900951026     C                   SETON                                        34
373000951025    3C                   ENDIF
373100951025     C*
373200900530     C                   MOVEL     *BLANKS       DESFFV
373300951026    3C     *IN34         IFEQ      *OFF
373400971203     C* DETERMINO TER-PART E DI ARRIVO DELLA FILIALE.
373500971203     C* COME TER-ARR, SE ESISTE, PRENDO QUELLO DA ECCEZIONI ALTRIMENTI
373600971203     C* QUELLO STANDARD
373700000602     C*
373800020805     C                   CLEAR                   FNLV55DS
373900971203     C                   MOVE      CODFIL        D55LIN
374000000612     C                   MOVE      VIDFGS        D55LNP
374100971203     C                   MOVE      AMGDFV        D55DRF
374200971203     C                   CALL      'FNLV55R'
374300020805     C                   PARM                    FNLV55DS
374400971203     C*
374500971203     C                   Z-ADD     D55TFA        LINFE0
374600971203     C*
374700951025     C                   MOVEL     ORGDES        DESFFV
374800951025     C                   MOVEL     ORGFAG        TIPFFV
374900971203     C                   MOVEL     D55TFP        SAVTFP
375000960226     C*
375100960226     C                   MOVEL     CODFIL        KTFA
375200960226     C                   MOVEL     'A'           KEPA
375300970102     C                   EXSR      RICEC2
375400971203     C                   MOVEA     TFE           TFE2
375500970102     C*
375600960226     C* RICERCO SE C'E' ECCEZIONE
375700951027     C* SE TRAINO DI DEFLUENZA O LOCALE, VERIFICO SE LE LINEE SONO
375800951027     C*  CORRETTE
375900951027    4C     VIDTTR        IFEQ      'L'
376000951027     C     §TVDEF        OREQ      'S'
376100100616     c                   setoff                                       31
376200100616     c                   if        §tvdef='S'
376300100616     C     VIDFGS        LOOKUP    TFEdef                                 31
376400100616     c                   endif
376500100616     c
376600100616     c                   if        not *in31
376700000612     C     VIDFGS        LOOKUP    TFE2                                   31
376800970102    5C     *IN31         IFEQ      *OFF
376900951027     C                   MOVEL     MSG(47)       VIDMSG
377000951027     C                   SETON                                        905028
377100951027    5C                   ENDIF
377200951027    4C                   ENDIF
377300100616     c                   endif
377400100616     c
377500960302     C* E' ANCHE VERO IL CONTRARIO
377600000612    4C  N90VIDFGS        IFEQ      LINFE0
377700960302     C     VIDTTR        ANDNE     'L'
377800960302     C     VIDTTR        ANDNE     'I'
377900960302     C     §TVDEF        ANDNE     'S'
378000960302     C                   MOVEL     MSG(66)       VIDMSG
378100960302     C                   SETON                                        905028
378200960302    4C                   ENDIF
378300951031     C* SE DEFLUENZA LA LNA <> DA LINEA PARTENZA FV
378400000612     C  N90              MOVE      VIDFGS        W003A
378500960302    4C  N90§TVDEF        IFEQ      'S'
378600000612     C     W003A         ANDEQ     VIDFFV
378700951027     C                   MOVEL     MSG(50)       VIDMSG
378800951027     C                   SETON                                        905028
378900960302    4C                   ENDIF
379000951027     C*
379100951025   X3C                   ELSE
379200951025     C* ERRORE
379300951025     C                   MOVEL     *ALL'?'       DESFFV
379400951025     C                   MOVEL     MSG(8)        VIDMSG
379500951025     C                   SETON                                        905028
379600971203     C                   Z-ADD     0             SAVTFP            3 0
379700951025    3C                   ENDIF
379800951025    2C                   ENDIF
379900900522      *--------------------
380000951025    2C     VIDFLP        IFEQ      *BLANKS
380100900522     C     VIDFLP        OREQ      *ZEROS
380200900522     C                   MOVEL     *BLANKS       VIDFLP
380300900522     C                   MOVEL     *BLANKS       DESFLP
380400900522     C                   MOVEL     *BLANKS       TIPFLP
380500900522      *
380600951025   X2C                   ELSE
380700900522      *
380800910125      * RICERCA FILIALE DI SCARICO
380900900522      *
381000951027     C     '?'           SCAN      VIDFLP                                 31
381100951027    3C   31              DO
381200940915     C                   SETON                                        90
381300951024     C                   EXSR      CASD19
381400900522     C                   MOVEL     §COD          VIDFLP
381500900522     C                   MOVEL     §TIP          TIPFLP
381600900530     C                   MOVEL     *BLANKS       DESFLP
381700900530     C                   MOVEL     §§DES         DESFLP
381800951025    3C                   END
381900900522      *
382000951025     C                   MOVE      VIDFLP        CODFIL
382100951025     C                   EXSR      CTRFIL
382200900530     C                   MOVEL     *BLANKS       DESFLP
382300971203     C*
382400971203     C     *IN34         IFEQ      *OFF
382500020805     C                   CLEAR                   FNLV55DS
382600971203     C                   MOVEL     'P'           D55TPT
382700971203     C                   MOVE      CODFIL        D55LIN
382800971203     C                   MOVE      AMGDFV        D55DRF
382900971203     C                   CALL      'FNLV55R'
383000020805     C                   PARM                    FNLV55DS
383100971203     C                   END
383200951025     C* FLP PUO' ESSERE :
383300951025     C* - SE STESSO
383400040316     C* - UNA SECONDARIA DELLA STESSA FIL.ELABORATORE-non più 16/3/04
383500951025     C* - UNA FIL. DI PRIMO LIVELLO
383600040316    3C**N34D55TFP        IFNE      SAVTFP
383700040316     C**   D55TFP        ANDNE     ORGFIL
383800040316     C**                 SETON                                        34
383900040316     c**                 endif
384000040316     c     d55tfp        ifne      orgfil
384100040316     c     vidflp        andne     vidffv
384200060515     c* Accetto solo se il p.o. di scarico è il terminal di arrivo della
384300060515     c*  linea di arrivo indicata
384400060515     c     linfe0        andne     orgfil
384500040316     C                   SETON                                        34
384600951025    3C                   ENDIF
384700951025     C*
384800951025    3C     *IN34         IFEQ      *OFF
384900951025     C                   MOVEL     ORGDES        DESFLP
385000951025     C                   MOVEL     ORGFAG        TIPFLP
385100951027     C* PER UN TRAINO DEFLUENZA --> LA FILIALE DI SCARICO = ALLA FILIAL
385200951027     C     §TVDEF        IFEQ      'S'
385300951027     C     VIDFLP        ANDNE     VIDFFV
385400951027     C                   MOVEL     MSG(48)       VIDMSG
385500951027     C                   SETON                                        285190
385600951027     C                   ENDIF
385700951027     C* PER UN TRAINO LOCALE    --> LA FILIALE DI SCARICO DELL'AREA
385800951031     C*                             O SE STESSA
385900951027     C     VIDTTR        IFEQ      'L'
386000951027     C     CODFIL        LOOKUP    L1                                     83
386100951027     C     *IN83         IFEQ      *OFF
386200951031     C     VIDFLP        ANDNE     VIDFFV
386300951027     C                   MOVEL     MSG(49)       VIDMSG
386400951027     C                   SETON                                        285190
386500951027     C                   ENDIF
386600951027     C                   ENDIF
386700951027     C*
386800951025   X3C                   ELSE
386900951025     C                   MOVEL     *ALL'?'       DESFLP
387000951025     C                   MOVEL     MSG(14)       VIDMSG
387100951026     C                   SETON                                        285190
387200951025    3C                   ENDIF
387300930329     C*
387400951025     C*   NON SI PUO' VARIARE LA FILIALE DI SCARICO SE ESISTONO
387500930330     C*   BOLLE BORDERIZZATE PER LA LINEA DI ARRIVO
387600951025    3C     *IN90         IFEQ      *OFF
387700951025     C     *IN80         ANDEQ     *OFF
387800951025     C     VIDFFV        LOOKUP    LAB                                    83
387900951025     C*
388000951025    4C     *IN83         IFEQ      *ON
388100951025    5C     SAVFLP        IFLE      *ZEROS
388200951025     C                   Z-ADD     1             I
388300951025     C     VIDFFV        LOOKUP    FFV(I)                                 84
388400951025     C  N84              MOVEL     VIDFLP        SAVFLP
388500951025     C   84              MOVEL     FLP(I)        SAVFLP
388600951025    5C                   ENDIF
388700951025     C*
388800951025    5C     SAVFLP        IFNE      VIDFLP
388900951026     C                   SETON                                        512890
389000951025     C                   MOVEA     MSG(37)       K78
389100951025     C                   MOVEA     SAVFLP        K78(36)
389200951025     C                   MOVEA     VIDFFV        K78(52)
389300951025     C                   MOVEA     K78           VIDMSG
389400951025    5C                   ENDIF
389500951025    4C                   ENDIF
389600951025    3C                   ENDIF
389700951025    2C                   ENDIF
389800951025      *
389900900606      * C'E' LA FILIALE DI TRANSITO SENZA FILIALE
390000951025     C  N90VIDFLP        IFGT      *ZEROS
390100951025     C     VIDFFV        ANDLE     *ZEROS
390200940909     C                   MOVEL     MSG(15)       VIDMSG
390300951025     C                   SETON                                        509028
390400951025     C                   ENDIF
390500951025      *
390600900606      * FILIALE DI TRANSITO OBBLIGATORIA SE C'E' LA FILIALE
390700951025     C  N90VIDFFV        IFGT      *ZEROS
390800951025     C     VIDFLP        ANDLE     *ZEROS
390900940909     C                   MOVEL     MSG(17)       VIDMSG
391000951026     C                   SETON                                        519028
391100951025     C                   ENDIF
391200911105      *
391300900529      * CONTROLLO CHE LA FILIALE NON SIA RIPETUTA
391400951025     C  N90VIDFFV        IFGT      *ZEROS
391500900529     C                   Z-ADD     1             J                 3 0
391600951025     C     VIDFFV        LOOKUP    CTL(J)                                 50
391700911105     C* TROVATO ERRORE FIL DOPPIA
391800970102     C     *IN50         IFEQ      *ON
391900970102     C                   MOVEL     MSG(16)       VIDMSG
392000970102     C                   SETON                                        90  28
392100970102     C                   ELSE
392200940909     C*
392300911105     C* NON TROVATO MEMORIZZO
392400970102     C     '   '         LOOKUP    CTL(J)                                 83
392500970102     C   83              MOVEL     VIDFFV        CTL(J)
392600970102     C* MEMORIZZO IL TERMINAL DI ARRIVO
392700970102     C   83              MOVEL     LINFE0        FE0(J)
392800970102     C   83              MOVEL     VIDFLP        CFS(J)
392900970102     C                   ENDIF
393000951025     C                   ENDIF
393100951025      *
393200000829     C                   SETOFF                                       23
393300130315     C                   UPDATE    lst4S03
393400951026     C                   SETOFF                                       4051
393500951025      *
393600951025     C* POSIZIONO IL CURSORE NELLA PAGIN PER 1 ERRORE
393700951026     C   90
393800951026     CANN96              DO
393900940909     C                   Z-ADD     NRR3          REC3
394000951026     C                   SETON                                        96
394100951025     C                   ENDDO
394200951025     C*
394300951025     C   90              GOTO      ENDCT3
394400900515      *
394500940909     C                   ADD       1             NRR3
394600951025     C                   ENDDO
394700911105      * SE TUTTO OK CONTROLLO CHE NON SIANO STATE ELIMINATE LINEE BORD
394800950905     C                   Z-ADD     1             I
394900951025     C                   Z-ADD     1             REC3
395000950905     C     LAB(I)        DOWGT     *ZEROS
395100951025     C     LAB(I)        LOOKUP    CTL                                    83
395200951025     C     *IN83         IFEQ      *OFF
395300951025     C                   SETON                                        905028
395400951025     C                   MOVEA     MSG(35)       K78
395500951025     C                   MOVEL     LAB(I)        W003A             3
395600951025     C                   MOVEA     W003A         K78(33)
395700951025     C                   MOVEA     K78           VIDMSG
395800000420     C                   Z-ADD     299           I
395900951025     C                   ENDIF
396000911105     C                   ADD       1             I
396100951025     C                   ENDDO
396200911108      * CONTROLLO CHE NON SIANO STATE ELIMINATE LINEE CON SPUNTE
396300950905     C                   Z-ADD     1             I
396400950905     C     LAS(I)        DOWGT     *ZEROS
396500951025     C     LAS(I)        LOOKUP    CTL                                    83
396600951025     C     *IN83         IFEQ      *OFF
396700951025     C                   SETON                                        905028
396800951025     C                   MOVEA     MSG(36)       K78
396900951025     C                   MOVEL     LAS(I)        W003A             3
397000951025     C                   MOVEA     W003A         K78(33)
397100951025     C                   MOVEA     K78           VIDMSG
397200000420     C                   Z-ADD     299           I
397300951025     C                   ENDIF
397400951025     C                   ADD       1             I
397500951025     C                   ENDDO
397600970102     C* CONTROLLO CHE LE FILIALI CON LO STESSO TER ARR
397700970102     C*  ABBIANO LA STESSA FIL DI SCARICO
397800000316     C* (PERMETTO PERO' DI FARE UN DIRETTO --> SCARICO = LINEA)
397900970102     C                   CLEAR                   WEST
398000060116     C*                  CLEAR                   WESTER
398100970102     C                   CLEAR                   WITA
398200970102    0C     VIDTTR        IFEQ      'S'
398300000316    0C     VIDTTR        OREQ      ' '
398400970102     C     §TVDEF        OREQ      'S'
398500970102     C                   Z-ADD     1             I
398600970102     C*
398700970102    1C     I             DOWLE     151
398800970102     C     CTL(I)        ANDGT     *ZEROS
398900970102     C**
399000970102    2C     VIDTTR        IFEQ      'S'
399100000316    2C     VIDTTR        OREQ      ' '
399200000316     C* SE LA LINEA E' UN SECONDO LIVELLO IN ARRIVO E NON SCARICA A
399300000316     C* SE' STESSA DEVE ESSERE PRESENTE COME LINEA ANCHE IL SUO TER-ARR
399400000316   2AC     CTL(I)        IFNE      FE0(I)
399500000316     C     CTL(I)        ANDNE     CFS(I)
399600000316     C     FE0(I)        LOOKUP    CTL                                    31
399700000316   2BC     *IN31         IFEQ      *OFF
399800000316     C* ERRORE
399900000316     C                   SETON                                        905028
400000000316     C                   MOVEA     MSG(74)       K78
400100000316     C                   MOVEL     CTL(I)        W003A
400200000316     C                   MOVEA     W003A         K78(37)
400300000316     C                   MOVEA     K78           VIDMSG
400400000316     C                   Z-ADD     160           I
400500000316   2BC                   ENDIF
400600000316   2AC                   ENDIF
400700970102     C                   Z-ADD     1             XX
400800000316   2AC  N90CTL(I)        IFNE      CFS(I)
400900970102     C                   MOVEL     FE0(I)        W003A
401000970102     C     W003A         LOOKUP    FE0(XX)                                31
401100970102    3C     *IN31         DOWEQ     *ON
401200970102    4C     CFS(XX)       IFNE      CFS(I)
401300000316     C     CTL(XX)       ANDNE     CFS(XX)
401400000316     C     CFS(XX)       ORNE      CFS(I)
401500000316     C     CTL(XX)       ANDEQ     FE0(XX)
401600970102     C* ERRORE
401700970102     C                   SETON                                        905028
401800970102     C                   MOVEA     MSG(70)       K78
401900970102     C                   MOVEL     CTL(I)        W003A
402000970102     C                   MOVEA     W003A         K78(13)
402100970102     C                   MOVEL     CTL(XX)       W003A
402200970102     C                   MOVEA     W003A         K78(21)
402300970102     C                   MOVEA     K78           VIDMSG
402400970102     C                   Z-ADD     160           I
402500970102     C                   SETOFF                                       31
402600970102   X4C                   ELSE
402700970102     C                   ADD       1             XX
402800970102     C*
402900970102     C     W003A         LOOKUP    FE0(XX)                                31
403000970102    4C                   ENDIF
403100970102    3C                   ENDDO
403200000316   2AC                   ENDIF
403300970102     C*
403400970102   X2C                   ELSE
403500990514     C* PER I TIPI TRAINO DEFLUENZA CONTROLLO SE 1 LNA E' ESTERO
403600970102     C*  CHE ANCHE TUTTE LE ALTRE LO SIANO
403700000405     C*  LA DPD LA CONSIDERO ESTERA
403800990514     C                   MOVEL     CTL(I)        CODFIL
403900990514     C                   EXSR      CTRFIL
404000020213     c                   if        esteroedpd = 'S'
404100970102     C                   MOVEL     'S'           WEST              1
404200970102     C                   ELSE
404300970102     C                   MOVEL     'S'           WITA              1
404400970102    3C                   ENDIF
404500970102     C*
404600970102    3C     WEST          IFEQ      'S'
404700970102     C     WITA          ANDEQ     'S'
404800970102     C                   SETON                                        905028
404900970102     C                   MOVEL     MSG(71)       VIDMSG
405000970102     C                   Z-ADD     160           I
405100970102    3C                   ENDIF
405200970102    2C                   ENDIF
405300970102     C*
405400970102     C                   ADD       1             I
405500970102    1C                   ENDDO
405600970102     C*
405700970102    0C                   ENDIF
405800970102     C* CONTROLLO I FV DEFLUENZA DELL'ESTERO
405900970102     C                   EXSR      CTRDEF
406000900515      *
406100951025     C     ENDCT3        ENDSR
406200900515      *-----------------------------------------------------***********
406300900515      * REGISTRAZIONE RECORD                                *  REGIS  *
406400900515      *-----------------------------------------------------***********
406500900515     C     REGIS         BEGSR
406600000605     C*
406700130510     c                   clear                   winviamail
406800020805     c                   eval      FGWnfv = VIDnfv
406900020805     c                   eval      FGWlnp = SIMfel
407000020805     c     K02FGW01      chain     FNFGW000                           39
407100000427     C   39              CLEAR                   FNFGW000
407200110512     c     K02FGv01      chain     FNFGS000                           36
407300110512     C   36              CLEAR                   FNFGS000
407400110512
407500951025     C                   CLEAR                   WABB
407600000405     C                   MOVEA     TFV           SFV
407700951130     C                   MOVEA     TLP           SLP
407800900515      *
407900900515     C                   MOVEL     *BLANK        FGVATB
408000900515     C                   Z-ADD     VIDNFV        FGVNFV
408100900515      *
408200951024     C                   Z-ADD     AMGDFV        FGVDFV
408300900515      *
408400130419     C                   MOVE      VIDfgs        FGVLNP
408500940913     C                   MOVE      VIDPDR        FGVPDR
408600010920     C                   Z-ADD     VIDIPT        FGWIPT
408700010920     C     FGWIPT        IFGT      *ZEROS
408800010920     C                   MOVEL     VIDVIP        FGWVIP
408900010920     C                   ENDIF
409000000202     C  N80              MOVE      FGVLNA        LNAPRE
409100900515     C                   MOVE      VIDLNA        FGVLNA
409200900515      *
409300951025     C                   Z-ADD     AMGDTP        FGVDTP
409400900515     C                   Z-ADD     VIDHMP        FGVHMP
409500130510     C                   MOVEL     VIDTMZ        FGVTMZ
409600900515      *
409700900515     C                   MOVEL     VIDDPD        FGVDPD
409800950913     C                   MOVEL     VIDTTR        FGVTTR
409900900515     C                   MOVEL     VIDTRR        FGVTRR
410000900515      *
410100900606     C                   MOVEL     VIDNT1        FGVNT1
410200900606     C                   MOVEL     VIDNT2        FGVNT2
410300900628     C                   MOVEL     VIDSNS        FGVSNS
410400960123     C                   MOVEL     VIDBIS        FGVSNP
410500900606      *
410600920204     C   80              MOVEL     *BLANKS       FGVFCF
410700940909     C                   MOVEL     *BLANKS       FGVFTR
410800940909     C                   Z-ADD     0             FGVDTR
410900911031     C   80              Z-ADD     0             FGVNFA
411000000202      * SFLAGGO LE SPUNTE SE MODIFICATA LA FILIALE FINALE DI ARRIVO
411100000202     C     *IN80         IFEQ      *OFF
411200000202     C     FGVLNA        ANDNE     LNAPRE
411300000202     C                   MOVEL     ' '           WMOD              1
411400000202     C                   MOVEL     LNAPRE        COMLNA
411500000202     C                   EXSR      LIBSPU
411600000202     C                   ENDIF
411700900515      * LEGGO IL SFL
411800951025     C                   CLEAR                   FLP
411900951025     C                   CLEAR                   FFV
412000900516      *
412100900515     C                   Z-ADD     1             I
412200940909     C                   Z-ADD     1             NRR3
412300951025     C*
412400000420    1C                   DO        299           NRR3
412500130315     C     NRR3          CHAIN     lst4S03                            81
412600951025     C*
412700951025    2C  N81VIDFFV        IFNE      *BLANKS
412800900516     C     VIDFFV        ANDNE     *ZEROS
412900900515     C                   MOVE      VIDFFV        FFV(I)
413000900522      *
413100900522     C                   MOVE      VIDFLP        FLP(I)
413200951025    3C     *IN80         IFEQ      *OFF
413300980303     C* SE MODIFICATO LO SCARICO SFLAGGO LE SPUNTE
413400920428     C                   Z-ADD     1             J
413500951027     C     VIDFFV        LOOKUP    SFV(J)                                 31
413600951027    4C   31VIDFLP        IFNE      SLP(J)
413700980305     C                   MOVEL     ' '           WMOD              1
413800920428     C                   MOVEL     VIDFFV        COMLNA
413900920428     C                   EXSR      LIBSPU
414000951025    4C                   ENDIF
414100980226     C* SE AGGIUNTA UNA LINEA VERIFICO ESISTENZA SPUNTE CON FTR='L'
414200000202     C  N31              MOVE      VIDFFV        W0030             3 0
414300980226     C     *IN31         IFEQ      *OFF
414400000202     C     W0030         ANDNE     LNAPRE
414500980305     C                   MOVEL     VIDFFV        COMLNA
414600980305     C                   MOVEL     'A'           WMOD              1
414700980305     C                   EXSR      LIBSPU
414800980226     C                   END
414900951025    3C                   ENDIF
415000900522      *
415100900516     C                   ADD       1             I
415200951025    2C                   ENDIF
415300951025    1C                   ENDDO
415400960301     C**
415500960301     C* CONTROLLO SE IL FV SI SBINA
415600960301     C                   EXSR      SBIFV
415700960302     C   90              GOTO      ENDREG
415800951027     C*
415900951027     C**
416000950901     C* SE NON E' STATO MODIFICATO NULLA ---> ABBINO
416100951218    1C     WABB          IFEQ      ' '
416200951027     C                   MOVEL     VIDTRN        FGVTRN
416300951018     C                   MOVEL     WTFM          FGVFRQ
416400950901     C                   ELSE
416500950901     C                   CLEAR                   FGVTRN
416600960130     C                   CLEAR                   VIDTRN
416700951018     C                   CLEAR                   FGVFRQ
416800951218    1C                   ENDIF
416900130510     c* Se conducente e/o targa non accreditati e sono in immissione o
417000130510     c*  modificato in manutenzione --> invio mail alla traini
417100130510     c                   if        wforzD1='A' and (*in80 or
417200130510     c                             viddcn<>fgvpmb)
417300130510     c                   eval      winviamail='D'
417400130510     c                   endif
417500130510     c                   if        wforzM1='A' and (*in80 or
417600130510     c                             vidtrm<>fgvtrm)
417700130510
417800130510     c                   if        winviamail=' '
417900130510     c                   eval      winviamail='M'
418000130510     c                   else
418100130510     c                   eval      winviamail='E'
418200130510     c                   endif
418300130510
418400130510     c                   endif
418500130510     c
418600130510     c                   if        winviamail<>' '
418700130510     c                   exsr      Inv_email
418800130510     c                   endif
418900130510     c
419000130510     C*****              MOVEL     VIDDCN        FGVDCN
419100130510     C                   MOVEL     VIDDCN        FGVPMB
419200130510     C                   MOVEL     VIDTRM        FGVTRM
419300951103     C*
419400951031     C* SCRIVO LE ANNOTAZIONI
419500960123     C*  CANCELLO E RISCRIVO
419600960123     C                   MOVEL     'P'           KEPA
419700960123     C                   MOVEL     'N'           KTRC
419800960123     C                   CLEAR                   KTFA
419900960126     C                   MOVEL     WTRSN2        WTRAS2
420000960126     C                   MOVEL     WTRSN1        WTRAS1
420100960126     C*
420200960126    1C     VIDAN1        IFEQ      *BLANKS
420300960126     C     VIDAN2        ANDNE     *BLANKS
420400960126     C                   MOVEL     VIDAN2        VIDAN1
420500960126     C                   CLEAR                   VIDAN2
420600960126    1C                   ENDIF
420700960123     C*
420800000605     C  N80KFV4          SETLL     FNFV401L
420900960126     C  N80KFV4          READE     FNFV401L                               83
421000960201    2C  N80*IN83         DOWEQ     *OFF
421100960126     C* SE ALMENO UNA DELLE 2 NOTE E' RIMASTA CANCELLO TUTTO E
421200960126     C*  RISCRIVO, ALTRIMENTI ANNULLANDO I RECORD UPDATANDO
421300960126     C* SE NON C'E' TRASMISISONE ANNULLO TUTTO
421400960126    3C     VIDAN1        IFNE      *BLANKS
421500960126     C     VIDAN2        ORNE      *BLANKS
421600960126     C     WTRSN1        ORNE      'S'
421700960126     C     WTRSN2        ANDNE     'S'
421800960123     C                   DELETE    FNFV4000
421900960126   X3C                   ELSE
422000960126     C* SOLO SE RECORD NON GIA' ANNULLATO
422100960126    4C     FV4ATB        IFEQ      ' '
422200960126     C     VIDAN1        ANDEQ     *BLANKS
422300960126     C     VIDAN2        ANDEQ     *BLANKS
422400960126     C*
422500960126     C                   MOVEL     'A'           FV4ATB
422600960126     C                   MOVEL     'N'           FV4FT1
422700060711     C                   MOVEL     dateu         FV4DT1
422800960126    5C     WTRSN2        IFNE      'N'
422900960126     C                   CLEAR                   FV4FT2
423000960126     C                   CLEAR                   FV4DT2
423100960126     C                   ELSE
423200960126     C                   MOVEL     'N'           FV4FT2
423300060711     C                   MOVEL     dateu         FV4DT2
423400960126    5C                   ENDIF
423500960126     C                   UPDATE    FNFV4000
423600960126    4C                   ENDIF
423700960126    3C                   ENDIF
423800960123     C*
423900960123     C     KFV4          READE     FNFV401L                               83
424000960123    2C                   ENDDO
424100951122     C*
424200960126     C* NOTE 1
424300951218    1C     VIDAN1        IFNE      *BLANKS
424400951031     C                   MOVEL     VIDAN1        FV4NOT
424500951031     C                   EXSR      WTRFV4
424600960123    1C                   ENDIF
424700951031     C* NOTE 2
424800960123    1C     VIDAN2        IFNE      *BLANKS
424900951031     C                   MOVEL     VIDAN2        FV4NOT
425000951031     C                   EXSR      WTRFV4
425100960123    1C                   ENDIF
425200950901     C*
425300951106     C* SCRIVO CAUSALE RITARDO/ANTICIPO SOLO SE IMMESSA OPPURE SE E'
425400951106     C*   STATA TOLTA MA PRIMA C'ERA (QUESTO PER INVIARE IN SEDE IL
425500951106     C*   RECORD VUOTO)
425600951218    1C     VIDRTC        IFNE      ' '
425700951106     C     VIDRTC        OREQ      ' '
425800951106     C     SAVRTC        ANDNE     ' '
425900951103     C                   EXSR      WTRFV2
426000951218    1C                   ENDIF
426100960126     C*
426200010622     C* N° CELLULARE
426300010622     C                   MOVEL     'P'           KEPA
426400010622     C                   MOVEL     'L'           KTRC
426500010622     C                   CLEAR                   KTFA
426600010622     C                   MOVEL     WTRSL2        WTRAS2
426700010622     C                   MOVEL     WTRSL1        WTRAS1
426800010920     C     KFV4          CHAIN     FNFV401L                           31
426900010920  A  C     *IN31         IFEQ      *OFF
427000010702     C                   MOVEL     FV4NOT        WCELL            16
427100010702  B  C     VIDCNC        IFNE      *BLANKS
427200010702  C  C     WCELL         IFNE      VIDCNC
427300010702     C                   CLEAR                   FV4NOT
427400040519     C                   MOVEL     'N'           FV4FT1
427500040519  D1 C     WTRSN2        IFNE      'N'
427600040519     C                   CLEAR                   FV4FT2
427700040519 XD1 C                   ELSE
427800040519     C                   MOVEL     'N'           FV4FT2
427900040519  D1 C                   ENDIF
428000010702     C                   MOVEL     VIDCNC        FV4NOT
428100010702     C                   UPDATE    FNFV4000
428200040519     c                   else
428300040519     c* disalloco record se cellulare non cambiato
428400040519     c                   unlock    fnfv401l
428500010702  C  C                   ENDIF
428600040519     c* Tolto il cellulare
428700010702 XB  C                   ELSE
428800010702  C  C     WTRSL1        IFNE      'S'
428900010702     C     WTRSL2        ANDNE     'S'
429000010702     C                   DELETE    FNFV401L
429100010702 XC  C                   ELSE
429200010702     C                   MOVEL     'A'           FV4ATB
429300060711     C                   MOVEL     'N'           FV4FT1
429400060711     C                   MOVEL     dateu         FV4DT1
429500010702  D1 C     WTRSN2        IFNE      'N'
429600010702     C                   CLEAR                   FV4FT2
429700010702     C                   CLEAR                   FV4DT2
429800010702 XD1 C                   ELSE
429900010702     C                   MOVEL     'N'           FV4FT2
430000060711     C                   MOVEL     dateu         FV4DT2
430100010702  D1 C                   ENDIF
430200010702     C                   UPDATE    FNFV4000
430300010702  C  C                   ENDIF
430400010702  B  C                   ENDIF
430500010702 XA  C                   ELSE
430600010702  E  C     VIDCNC        IFNE      *BLANKS
430700011017     C                   CLEAR                   FV4NOT
430800010622     C                   MOVEL     VIDCNC        FV4NOT
430900010702     C                   EXSR      WTRFV4
431000010702  E  C                   ENDIF
431100010702  A  C                   ENDIF
431200130424     C*
431300130424     C* PIOMBI
431400130424     C                   MOVEL     'P'           KEPA
431500130424     C                   MOVEL     'P'           KTRC
431600130424     C                   CLEAR                   KTFA
431700130424     C                   MOVEL     WTRSP2        WTRAS2
431800130424     C                   MOVEL     WTRSP1        WTRAS1
431900130424     c
432000130424    1c                   if        not *in80
432100130424     C     KFV4          setll     FNFV401L
432200130424     C     KFV4          reade     FNFV401L
432300130424    2C                   dow       not %eof(fnfv401l)
432400130424    3c                   if        vidpmb<>*blanks
432500130424     c                             or (wtras1<>'S' and wtras2<>'S')
432600130424     c                   delete    fnfv4000
432700130424   x3c                   else
432800130424     C* SOLO SE RECORD NON GIA' ANNULLATO
432900130424    4C                   if        fv4atb=' ' and vidpmb=*blanks
433000130424     C*
433100130424     C                   MOVEL     'A'           FV4ATB
433200130424     C                   MOVEL     'N'           FV4FT1
433300130424     C                   MOVEL     dateu         FV4DT1
433400130424    5C     WTRSP2        IFNE      'N'
433500130424     C                   CLEAR                   FV4FT2
433600130424     C                   CLEAR                   FV4DT2
433700130424     C                   ELSE
433800130424     C                   MOVEL     'N'           FV4FT2
433900130424     C                   MOVEL     dateu         FV4DT2
434000130424    5C                   ENDIF
434100130424     C                   UPDATE    FNFV4000
434200130424    4c                   endif
434300130424    3c                   endif
434400130424     C     KFV4          reade     FNFV401L
434500130424    2c                   enddo
434600130424    1c                   endif
434700130424    c
434800130424    1c                   if        vidpmb<>*blanks
434900130424     c                   movel     vidpmb        fv4not
435000130424     c                   exsr      wtrfv4
435100130424     c* vedo se scivere anche secondo record
435200130424    2c                   if        %subst(vidpmb:36)<>*blanks
435300130424     c                   eval      fv4not=%subst(vidpmb:36)
435400130424     c                   exsr      wtrfv4
435500130424    2c                   endif
435600130424    1c                   endif
435700010622     C*
435800010622     C*
435900960126     C                   CLEAR                   FGVFTR
436000960126     C                   CLEAR                   FGVDTR
436100011011      * Pulisco fgvipt xchè non serve più
436200011011     C                   CLEAR                   FGVIPT
436300951103     C*
436400950905     C   80              WRITE     FGV01
436500950905     C  N80              UPDATE    FGV01
436600000420     C*
436700000420     C* FILE FOGLI VIAGGIO ESTENSIONE
436800000427     C                   MOVEL     W1FF3         FGWFF3
436900000427     C                   MOVEL     W1FF4         FGWFF4
437000000427     C                   MOVEL     W1FL3         FGWFL3
437100000427     C                   MOVEL     W1FL4         FGWFL4
437200130419     C                   CLEAR                   FGWFGS
437300110512     c
437400000427     C     FGWFF3        IFNE      *BLANKS
437500000427     C     FGWFF4        ORNE      *BLANKS
437600000427     C     FGWFGS        ORNE      0
437700011011     C     FGWIPT        ORNE      0
437800000427     C  N39              CLEAR                   FGWATB
437900000427     C  N39              UPDATE    FNFGW000
438000000427     C*
438100000427     C   39              MOVEL     FGVNFV        FGWNFV
438200000427     C   39              MOVEL     FGVLNP        FGWLNP
438300000427     C   39              WRITE     FNFGW000
438400000427     C                   ELSE
438500000427     C  N39              DELETE    FNFGW000
438600000427     C                   ENDIF
438700110512     c
438800110512     c* PUNTI DI SCAMBIO
438900110512     C                   if        vidpsc<>*blanks
439000110512     c                   eval      fgspsc=vidpsc
439100110512     c                   eval      fgsdgs=amgdGS
439200110512     c                   eval      fgshhs=vidhhS
439300110512     c                   if        vidtrns>*zeros
439400110530     c                   eval      fgstrns=vidtrns
439500110512     c                   else
439600110512     c                   clear                   fgstrns
439700110512     c                   endif
439800110530     c* se c'e' fil finale airrov aut ddevo pulire il traino
439900110530     c                   if        vidlnar>*zeros
440000110530     c                   movel     vidlnar       fgslnar
440100110530     c                   clear                   fgstrns
440200110530     c                   endif
440300110513     c
440400110512     c
440500110512     c                   if        not *in36
440600110512     C                   CLEAR                   FGSATB
440700110512     C                   UPDATE    FNFGS000
440800110512     c                   else
440900110512     C                   MOVEL     FGVNFV        FGSNFV
441000110512     C                   MOVEL     FGVLNP        FGSLNP
441100110512     C                   WRITE     FNFGS000
441200110512     c                   endif
441300110512     c
441400110512     C                   ELSE
441500110512     C  N36              DELETE    FNFGS000
441600110512     C                   ENDIF
441700110512     c
441800110512     c
441900960122     C* CALCOLO DELLE DATE TEORICHE PARTENZE E ARRIVO
442000960122     C                   EXSR      DTTEOR
442100130607
442200130607     c* Scrittura/aggiornamento file di appoggio per stampa etichette
442300130607     c                   exsr      sr_fnfge
442400940913     C*
442500960119     C* SE HO MODIFICATO UN F.V. GIA' CHIUSO DEVO TRASMETTERLO IN SEDE
442600960119     C*   E ALLA FILIALE DI ARRIVO (CHE PUO' ESSERE PIU' DI UNA)
442700960126     C                   EXSR      TRAVAR
442800960126     C     ENDREG        ENDSR
442900960301     C*-----------------------------------------------------***********
443000960301     C* CONTROLLO SE IL FV SI SBINA
443100960301     C*-----------------------------------------------------***********
443200960301     C     SBIFV         BEGSR
443300960604     C                   SETOFF                                       90
443400960302     C                   MOVEL     'C'           KTRC
443500960302     C                   MOVEL     WTRSC1        WTRAS1
443600960302     C                   MOVEL     WTRSC2        WTRAS2
443700960301      *
443800960301     C* VERIFICO SE POSSO ABBINARE O MENO IL FV.. NON SI ABBINA SE :
443900960301     C**
444000960301    1C     VIDTRN        IFGT      0
444100960301     C**
444200960301     C* TRAINO IN CAMPO ALFABETICO SENZA 0 NON SIGNIFICATIVI
444300960301     C                   MOVEL     VIDTRN        W007A
444400960301     C                   MOVEA     W007A         K15(9)
444500960301     C                   Z-ADD     9             Z
444600960301     C                   EXSR      ABBL
444700960301     C                   MOVEA     K15(9)        W007A
444800960301     C**
444900960301     C                   MOVEL     VIDLNA        WLNAF
445000960301     C*
445100960301     C* 2) MESSO IMPORTO PATTUITO PER FILIALE CHE NON DEVE PAGARE
445200960301     C* 2)
445300000612    2C     TLTFES        IFNE      VIDFGS
445400960301     C     VIDIPT        ANDGT     0
445500960301     C     WTFM          ORNE      WFMFES
445600960301     C     VIDIPT        ANDGT     0
445700960301     C                   MOVEL     '1'           WABB
445800960301     C*
445900960302     C* SOLO AL 2 GIRO QUANDO HO EMESSO IL MESSAGGIO
446000960302     C     WFORZ6        IFNE      ' '
446100960301     C                   MOVEA     SBI(2)        K35
446200960301     C                   MOVEA     W007A         K35(1)
446300960301     C*
446400960301     C* CALL AL PGM CHE SCRIVE I RECORD DI SBINAMENTO
446500960301     C                   MOVEA     K35           SBI(2)
446600960301     C                   Z-ADD     2             WREC              2 0
446700960301     C                   Z-ADD     1             I
446800960301     C                   EXSR      WRTSBI
446900960301     C                   ENDIF
447000960302     C                   ENDIF
447100960301     C*
447200960301     C* 3) IL TRAINO NON C'E' NEL GIORNO = DATA TRAINO
447300960301     C* 3)
447400960302     C     WFORZ         IFEQ      'S'
447500960301     C                   MOVEL     '1'           WABB
447600960302     C*
447700960302     C     WFORZ6        IFNE      ' '
447800960302     C*
447900960302     C* IMPOSTO IL NUMERO TRAINO
448000960302     C                   MOVEA     SBI(3)        K35
448100960302     C                   MOVEA     W007A         K35(8)
448200960302     C                   MOVEA     K35           SBI(3)
448300960302     c* imposto il giorno della settimana
448400960302     C* REPERISCO IL GIORNO DELLA SETTIMANA (1=LUN ...)
448500960302     C     SAVSET        IFEQ      ' '
448600960302     C                   CALL      'XGIOSE1'
448700960302     C                   PARM                    GGMMAA            6
448800960302     C                   PARM                    GIOSET            1
448900960302     C                   MOVEL     GIOSET        SAVSET
449000960302     C                   ENDIF
449100960302     C*
449200960302     C                   CLEAR                   X
449300960302     C                   MOVE      SAVSET        X
449400960302     C                   MOVEA     SBI(4)        K35
449500960302     C                   MOVEL     GIO(X)        W009A             9
449600960302     C                   MOVEA     W009A         K35(11)
449700960302     C                   MOVEA     K35           SBI(4)
449800960302     c*
449900960302     C                   Z-ADD     2             WREC              2 0
450000960302     C                   Z-ADD     3             I
450100960302     C                   EXSR      WRTSBI
450200960302     C                   ENDIF
450300960301     C                   ENDIF
450400960301     C*
450500960301     C* 4) ESISTE GIA'UN FV ABBINATO IN QUEL GIORNO PER QUEL TRAINO/A-R
450600960301     C* 4)
450700960301     C* SOLO PER IMMISSIONE VALE IL DISCORSO DI ABBINAMENTO DOPPIO
450800960301     C     WFORZ2        IFEQ      'S'
450900960301     C     *IN80         ANDEQ     *ON
451000960301     C                   MOVEL     '1'           WABB
451100960302     C*
451200960302     C     WFORZ6        IFNE      ' '
451300960302     C* IMPOSTO IL NUJMERO TRAINO
451400960302     C                   MOVEA     SBI(6)        K35
451500960302     C                   MOVEA     W007A         K35(8)
451600960302     C                   MOVEA     K35           SBI(6)
451700960302     C* IMPOSTO IL NUMERO DI FV
451800960302     C                   MOVEA     SBI(5)        K35
451900960302     C                   MOVEA     SAV005        K35(14)
452000960302     C                   MOVEA     K35           SBI(5)
452100960302     c*
452200960302     C                   Z-ADD     2             WREC              2 0
452300960302     C                   Z-ADD     5             I
452400960302     C                   EXSR      WRTSBI
452500960301     C                   ENDIF
452600960302     C                   ENDIF
452700960301     C*
452800960301     C* 5) MODIFICATO IL TIPO TRAINO
452900960301     C* 5)
453000960301     C     WFORZ7        IFEQ      'S'
453100960301     C                   MOVEL     '1'           WABB
453200960302     C     WFORZ6        IFNE      ' '
453300960302     C* IMPOSTO IL NUJMERO TRAINO
453400960302     C                   MOVEA     SBI(7)        K35
453500960302     C                   MOVEA     W007A         K35(12)
453600960302     C                   MOVEA     K35           SBI(7)
453700960302     C* IMPOSTO IL TIPO TRAINO
453800960302     C                   MOVEA     SBI(8)        K35
453900960302     C                   MOVEA     TLTTTR        K35(17)
454000960302     C                   MOVEA     VIDTTR        K35(22)
454100960302     C                   MOVEA     DESTTR        K35(24)
454200960302     C                   MOVEA     K35           SBI(8)
454300960302     c*
454400960302     C                   Z-ADD     2             WREC              2 0
454500960302     C                   Z-ADD     7             I
454600960302     C                   EXSR      WRTSBI
454700960302     C                   ENDIF
454800960301     C                   ENDIF
454900960301     C*
455000960301     C* 6) MODIFICATA LA DATA FV
455100960301     C* 6)
455200960301     C     WFORZ9        IFEQ      'S'
455300960301     C                   MOVEL     '1'           WABB
455400960302     C     WFORZ6        IFNE      ' '
455500960302     C* IMPOSTO IL NUMERO TRAINO
455600960302     C                   MOVEA     SBI(9)        K35
455700960302     C                   MOVEA     W007A         K35(19)
455800960302     C                   MOVEA     K35           SBI(9)
455900960302     c*
456000960302     C                   Z-ADD     2             WREC              2 0
456100960302     C                   Z-ADD     9             I
456200960302     C                   EXSR      WRTSBI
456300960302     C                   ENDIF
456400960301     C                   ENDIF
456500960301     C*
456600960301     C* 7) MODIFICATA LA CATEGORIA TRAINO
456700960301     C* 7)
456800110512     C     WFORZD        IFEQ      'S'
456900110512     C                   MOVEL     '1'           WABB
457000110512     C     WFORZ6        IFNE      ' '
457100110512     C* IMPOSTO IL NUMERO TRAINO
457200110512     C                   MOVEA     SBI(11)       K35
457300110512     C                   MOVEA     W007A         K35(12)
457400110512     C                   MOVEA     K35           SBI(11)
457500110512     C* IMPOSTO la categoria traino
457600110512     C                   MOVEA     SBI(12)       K35
457700110512     C     VIDBIS        IFEQ      'B'
457800110512     C                   MOVEA     'BIS'         K35(22)
457900110512     C                   ELSE
458000110512     C                   MOVEA     '   '         K35(22)
458100110512     C                   ENDIF
458200110512     C                   MOVEA     K35           SBI(12)
458300110512     c*
458400110512     C                   Z-ADD     2             WREC              2 0
458500110512     C                   Z-ADD     11            I
458600110512     C                   EXSR      WRTSBI
458700110512     C                   ENDIF
458800110512     C                   ENDIF
458900110512     C*
459000110530     C* 8) aggiunta linea finale ritorno autista per Scambio
459100110512     C* 8)
459200110530     C     WFORZlnar     IFEQ      'S'
459300960301     C                   MOVEL     '1'           WABB
459400960302     C     WFORZ6        IFNE      ' '
459500960302     C* IMPOSTO IL NUMERO TRAINO
459600110530     C                   MOVEA     SBI(22)       K35
459700960302     C                   MOVEA     W007A         K35(12)
459800110530     C                   MOVEA     K35           SBI(22)
459900110530     C* IMPOSTO linea finale riitorno aut aggiunta
460000110530     C                   MOVEA     SBI(23)       K35
460100110530     C                   MOVEA     vidlnar       K35(8)
460200110530     C                   MOVEA     K35           SBI(23)
460300960302     c*
460400960302     C                   Z-ADD     2             WREC              2 0
460500110512     C                   Z-ADD     20            I
460600960302     C                   EXSR      WRTSBI
460700960302     C                   ENDIF
460800960301     C                   ENDIF
460900110530     C* 9) MODIFICATO IL PUNTO DI SCAMBIO
461000110530     C* 9)
461100110530     C     WFORZsc       IFEQ      'S'
461200110530     C                   MOVEL     '1'           WABB
461300110530     C     WFORZ6        IFNE      ' '
461400110530     C* IMPOSTO IL NUMERO TRAINO
461500110530     C                   MOVEA     SBI(20)       K35
461600110530     C                   MOVEA     W007A         K35(12)
461700110530     C                   MOVEA     K35           SBI(20)
461800110530     C* IMPOSTO opunti di scambio modificato
461900110530     C                   MOVEA     SBI(21)       K35
462000110530     C                   MOVEA     TLspsc        K35(13)
462100110530     C                   MOVEA     vidpsc        K35(26)
462200110530     C                   MOVEA     K35           SBI(21)
462300110530     c*
462400110530     C                   Z-ADD     2             WREC              2 0
462500110530     C                   Z-ADD     20            I
462600110530     C                   EXSR      WRTSBI
462700110530     C                   ENDIF
462800110530     C                   ENDIF
462900110512     c
463000960301     C*
463100960301     C* 1) CAMBIATA LNA ARRIVO FINALE
463200110512     C* 9) CAMBIATA UNA LINEA DI ARRIVO O FIL SCARICO
463300110512     C* 9)
463400960301     C* 1)
463500960302     C                   CLEAR                   WMSG
463600970103     C                   CLEAR                   XX
463700970103     C                   CLEAR                   YY
463800970103     C                   CLEAR                   ZZ
463900970103     C                   CLEAR                   VAR
464000970103     C                   CLEAR                   AGG
464100970103     C                   CLEAR                   ELM
464200970103     C*
464300960301    2C     TRNTFA        IFNE      WLNAF
464400960301     C     WFORZ8        ORNE      ' '
464500960301     C                   MOVEL     '1'           WABB
464600970103     C                   MOVEL     'Y'           WMSG              1
464700970103    3C     WFORZ6        IFNE      ' '
464800970103     C* MSG GENERICO DI MODIFICA LINEE
464900970103     C                   EXSR      MSGMOD
465000970103     C*
465100970103     C                   MOVEA     SBI(16)       K35
465200970103     C                   MOVEL     WLNAF         W003A
465300970103     C                   MOVEA     W003A         K35(32)
465400970103     C                   MOVEL     TRNTFA        W003A
465500970103     C                   MOVEA     W003A         K35(26)
465600970103     C                   MOVEA     K35           SBI(16)
465700970103     C*
465800970103     C                   Z-ADD     1             WREC
465900970103     C                   Z-ADD     16            I
466000970103     C                   EXSR      WRTSBI
466100970103    3C                   ENDIF
466200970103    2C                   ENDIF
466300960301     C**
466400960301     C                   Z-ADD     1             I
466500000420    2C     I             DOWLE     299
466600970103   2AC     FFV(I)        IFGT      *ZEROS
466700960301     C                   Z-ADD     1             C
466800960301     C*
466900960301     C     FFV(I)        LOOKUP    SFV(C)                                 31
467000970103     C*
467100970103     C* NON TROVATA : LINEA AGGIUNTA
467200970103    3C     *IN31         IFEQ      *OFF
467300970103     C                   MOVE      '1'           WABB              1
467400970103    4C     WMSG          IFNE      'Y'
467500970103     C                   MOVEL     'S'           WMSG              1
467600970103    4C                   ENDIF
467700970103     C*
467800970103     C     XX            IFLT      53
467900970103     C                   ADD       1             XX
468000970103     C                   MOVEL     FFV(I)        AGG(XX)
468100970103     C                   ENDIF
468200970103     C*
468300970103   X3C                   ELSE
468400970103     C*
468500970103     C* TROVATA: VEDO SE MODIFICATO SCARICO
468600960301     C*
468700970103    4C     FLP(I)        IFNE      SLP(C)
468800960301     C                   MOVE      '1'           WABB
468900970103    5C     WMSG          IFNE      'Y'
469000970103     C                   MOVEL     'S'           WMSG              1
469100970103    5C                   ENDIF
469200970103     C*
469300970103    5C     YY            IFLT      53
469400970103     C                   ADD       1             YY                3 0
469500970103     C                   MOVEL     FFV(I)        VAR(YY)
469600970103    5C                   ENDIF
469700970103    4C                   ENDIF
469800970103     C*
469900960301     C                   CLEAR                   SFV(C)
470000970103    3C                   END
470100970103   2AC                   ENDIF
470200970103     C*
470300960301     C                   ADD       1             I                 3 0
470400970103     C*
470500960301    2C                   ENDDO
470600970103     C* CONTROLLO SE SONO STATE ELIMINATE DELLE LINEE
470700960301     C                   MOVEA     SFV(1)        W225A           225
470800960301     C                   MOVEA     SFV(76)       W225B           225
470900000420     C                   MOVEA     SFV(151)      W225C           225
471000000420     C                   MOVEA     SFV(226)      W225D           225
471100960301     C*
471200970103    2C     W225A         IFNE      *BLANKS
471300960301     C     W225B         ORNE      *BLANKS
471400960301     C                   MOVE      '1'           WABB
471500970103    3C     WMSG          IFNE      'Y'
471600970103     C                   MOVEL     'S'           WMSG              1
471700970103    3C                   ENDIF
471800970103     C* CONTROLLO LE LINEE CHE SONO STATE ELIMINATE
471900000420    3C                   DO        299           I
472000970103    4C     SFV(I)        IFGT      *ZEROS
472100970103     C     ZZ            ANDLT     53
472200970103     C                   ADD       1             ZZ                3 0
472300970103     C                   MOVEL     SFV(I)        ELM(ZZ)
472400970103    4C                   ENDIF
472500970103    3C                   ENDDO
472600970103     C**
472700970103    2C                   ENDIF
472800970103     C*
472900970103     C*
473000970103    2C     WFORZ6        IFNE      ' '
473100970103     C* EMETTO MSG GENERICO DI MODIFICA LINEE
473200970103    3C     WMSG          IFEQ      'S'
473300970103     C                   EXSR      MSGMOD
473400970103    3C                   ENDIF
473500970103     C**
473600970103     C*  L I N E E   A G G I U N T E
473700960302     C**
473800970103    3C     XX            IFGT      0
473900970103     C                   SETON                                        11
474000970103     C     XX            DIV       6             DIVIDI            2 0
474100970103     C                   MVR                     RESTO             2 0
474200970103    4C     RESTO         IFGT      0
474300970103     C                   ADD       1             DIVIDI
474400970103    4C                   ENDIF
474500970103     C**
474600970103     C                   Z-ADD     DIVIDI        WREC
474700970103     C                   Z-ADD     1             XX
474800970103     C                   Z-ADD     17            I
474900970103     C                   EXSR      PREAGG
475000970103     C                   EXSR      WRTSBI
475100970103     C                   SETOFF                                       11
475200970103    3C                   ENDIF
475300970103     C**
475400970103     C*  L I N E E   E L I M I N A T E
475500970103     C**
475600970103    3C     ZZ            IFGT      0
475700970103     C                   SETON                                        12
475800970103     C     ZZ            DIV       6             DIVIDI            2 0
475900970103     C                   MVR                     RESTO             2 0
476000970103    4C     RESTO         IFGT      0
476100970103     C                   ADD       1             DIVIDI
476200970103    4C                   ENDIF
476300970103     C**
476400970103     C                   Z-ADD     DIVIDI        WREC
476500970103     C                   Z-ADD     1             ZZ
476600970103     C                   Z-ADD     18            I
476700970103     C                   EXSR      PREAGG
476800970103     C                   EXSR      WRTSBI
476900970103     C                   SETOFF                                       12
477000970103    3C                   ENDIF
477100970103     C**
477200970103     C*  L I N E E   D I   C U I   V A R I A T O   S C A R I C O
477300970103     C**
477400970103    3C     YY            IFGT      0
477500970103     C                   SETON                                        13
477600970103     C     YY            DIV       4             DIVIDI            2 0
477700970103     C                   MVR                     RESTO             2 0
477800970103    4C     RESTO         IFGT      0
477900970103     C                   ADD       1             DIVIDI
478000970103    4C                   ENDIF
478100970103     C**
478200970103     C                   Z-ADD     DIVIDI        WREC
478300970103     C                   Z-ADD     1             YY
478400970103     C                   Z-ADD     19            I
478500970103     C                   EXSR      PREVAR
478600970103     C                   EXSR      WRTSBI
478700970103     C                   SETOFF                                       13
478800970103    3C                   ENDIF
478900970103     C**
479000960302     C* SE HO SCRITTO I RECORD COME ULTIMO SCRIVO NEL REC "D" IL NUM.
479100960302     C*  DEL TRAINO SBINATO NELLE PRIME 7 POSIZIONI
479200960302     C* IMPOSTO IL NUMERO TRAINO
479300960302     C                   MOVEL     VIDTRN        W007A
479400960302     C                   MOVEA     W007A         SBI(14)
479500960302     C                   MOVEL     'D'           KTRC
479600960302     C                   MOVEL     WTRSD1        WTRAS1
479700960302     C                   MOVEL     WTRSD2        WTRAS2
479800960302     c*
479900960302     C                   Z-ADD     1             WREC              2 0
480000960302     C                   Z-ADD     14            I
480100960302     C                   EXSR      WRTSBI
480200960302    2C                   ENDIF
480300960301     C**
480400960302    2C     WABB          IFEQ      '1'
480500960302     C     WFORZ6        ANDEQ     ' '
480600960302     C                   MOVEL     MSG(44)       VIDMSG
480700960302     C                   SETON                                        2890
480800960302     C                   MOVEL     'S'           WFORZ6            1
480900960302    2C                   ENDIF
481000960302     C*
481100960302   X1C                   ELSE
481200960302     c*
481300960302     C* SE NON E' ABBINATO CONTINUO A PULIRE I 2 CAMPI
481400960302     C                   MOVEL     '1'           WABB
481500960302     C* PER IMMISSIONE SCRIVO IL MESSAGGIO
481600960302    2C     *IN80         IFEQ      *ON
481700960302     C                   Z-ADD     1             WREC              2 0
481800960302     C                   Z-ADD     13            I
481900960302     C                   EXSR      WRTSBI
482000960302    2C                   ENDIF
482100960302    1C                   ENDIF
482200960301     C*
482300960301     C                   ENDSR
482400970103     c*
482500970103     C*-----------------------------------------------------***********
482600970103     C* EMETTO MESSAGGIO DI MODIFICA LINEE
482700970103     C*-----------------------------------------------------***********
482800970103     C     MSGMOD        BEGSR
482900970103     C* IMPOSTO IL NUMERO TRAINO
483000970103     C                   MOVEA     SBI(15)       K35
483100970103     C                   MOVEA     W007A         K35(29)
483200970103     C                   MOVEA     K35           SBI(15)
483300970103     c*
483400970103     C                   Z-ADD     1             WREC              2 0
483500970103     C                   Z-ADD     15            I
483600970103     C                   EXSR      WRTSBI
483700970103     C                   ENDSR
483800970103     C*-----------------------------------------------------***********
483900970114     C* PREPARO MESSAGGIO DI AGGIUNTA/ELIMINAZIONE  LINEE
484000970103     C*-----------------------------------------------------***********
484100970103     C     PREAGG        BEGSR
484200970103     C*
484300970114     C                   Z-ADD     12            KK                3 0
484400970103     C                   MOVEA     SBI(I)        K35
484500970103     C*
484600970103     C     *IN11         IFEQ      *ON
484700970103     C     XX            MULT      6             Y                 3 0
484800970103     C                   ENDIF
484900970103     C*
485000970103     C     *IN12         IFEQ      *ON
485100970103     C     ZZ            MULT      6             Y
485200970103     C                   ENDIF
485300970103     C*
485400970103     C     Y             SUB       5             C
485500970103     C**
485600970103     C     C             DO        Y             C
485700970103     C     C             IFLT      54
485800970103     C   11              MOVEL     AGG(C)        W003A
485900970103     C   12              MOVEL     ELM(C)        W003A
486000970103     C                   ELSE
486100970103     C                   MOVEL     '...'         W003A
486200970103     C                   ENDIF
486300970103     C**
486400970103     C                   MOVEA     W003A         K35(KK)
486500970103     C                   ADD       4             KK
486600970103     C                   ENDDO
486700970103     C**
486800970103     C                   MOVEA     K35           SBI(I)
486900970103     C                   ENDSR
487000970103     C*-----------------------------------------------------***********
487100970103     C* PREPARO MESSAGGIO DI VARIAZIONE FIL DI SCARICO ALLE LINEE
487200970103     C*-----------------------------------------------------***********
487300970103     C     PREVAR        BEGSR
487400970103     C*
487500970103     C                   Z-ADD     21            KK                3 0
487600970103     C                   MOVEA     SBI(I)        K35
487700970103     C*
487800970103     C     YY            MULT      4             Y
487900970103     C     Y             SUB       3             C
488000970103     C**
488100970103     C     C             DO        Y             C
488200970103     C     C             IFLT      54
488300970103     C                   MOVEL     VAR(C)        W003A
488400970103     C                   ELSE
488500970103     C                   MOVEL     '...'         W003A
488600970103     C                   ENDIF
488700970103     C**
488800970103     C                   MOVEA     W003A         K35(KK)
488900970103     C                   ADD       4             KK
489000970103     C                   ENDDO
489100970103     C**
489200970103     C                   MOVEA     K35           SBI(I)
489300970103     C                   ENDSR
489400000825     C*-----------------------------------------------------***********
489500000825     C* INSERIMENTO TERMINAL ARRIVO CON F10
489600000825     C*-----------------------------------------------------***********
489700000825     C     INSTER        BEGSR
489800000828      *
489900000828      * EMETTO SUBFILE VUOTO
490000000828      *
490100000828     C                   SETOFF                                       626368
490200000828     C                   CLEAR                   V06TE2
490300000828     C                   CLEAR                   V06DE2
490400000828     C                   CLEAR                   V06TE3
490500000828     C                   CLEAR                   V06DE3
490600000828     C                   Z-ADD     0             NRR6              3 0
490700000828     C                   SETON                                        65
490800130315     C                   WRITE     lst4C06
490900000828     C                   SETOFF                                       65
491000000829     C     1             DO        75            NRR6
491100130315     C                   WRITE     lst4S06
491200000828     C                   ENDDO
491300000829     C                   Z-ADD     1             REC6
491400130315     C                   WRITE     lst4T01
491500130315     C                   WRITE     lst4Z06
491600130315     C                   WRITE     lst4D06
491700000828     C     INIINT        TAG
491800130315     C                   EXFMT     lst4C06
491900000828     C                   SETOFF                                       626368
492000000828      *
492100000828     C   KL              SETON                                        66
492200000828     C   KL              GOTO      ENDINT
492300000828      *
492400000828      * CONTROLLO DATI IMMESSI:
492500000828      *
492600000829      * STESSI CONTROLLI ANCHE PER T.ARR E P.O. SCARICO DEL SUBFILE
492700000829      *
492800000829     C     1             DO        *HIVAL        WN                5 0
492900130315     C     WN            CHAIN     lst4S06                            69
493000000829     C   69              LEAVE
493100000829      *
493200000829     C                   SETOFF                                       62  68
493300000829      *
493400000829     C                   CLEAR                   V06DE2
493500000829     C                   CLEAR                   V06DE3
493600000829      *
493700000829     C     V06TE2        IFEQ      *ALL'0'
493800000829     C                   MOVE      *BLANKS       V06TE2
493900000829     C                   ENDIF
494000000829      *
494100000829     C     V06TE3        IFEQ      *ALL'0'
494200000829     C                   MOVE      *BLANKS       V06TE3
494300000829     C                   ENDIF
494400000829      *
494500000829     C     '?'           SCAN      V06TE2                                 69
494600000829     C     *IN69         IFEQ      *ON
494700000829     C                   EXSR      CASD19
494800000829     C                   MOVEL     §COD          V06TE2
494900130315     C                   WRITE     lst4T01
495000130315     C                   WRITE     lst4Z06
495100130315     C                   WRITE     lst4D06
495200000829     C                   ENDIF
495300000829      *
495400000829     C     '?'           SCAN      V06TE3                                 69
495500000829     C     *IN69         IFEQ      *ON
495600000829     C                   EXSR      CASD19
495700000829     C                   MOVEL     §COD          V06TE3
495800130315     C                   WRITE     lst4T01
495900130315     C                   WRITE     lst4Z06
496000130315     C                   WRITE     lst4D06
496100000829     C                   ENDIF
496200000829      *
496300000829     C     V06TE2        IFNE      *BLANKS
496400000829     C                   TESTN                   V06TE2               69
496500000829     C                   MOVE      V06TE2        W1ALF             1
496600000829     C     *IN69         IFEQ      *OFF
496700000829     C     W1ALF         ORLT      '0'
496800000829     C                   SETON                                        62
496900000829     C                   ELSE
497000020805     C                   CLEAR                   FNLV55DS
497100000829     C                   MOVEL     'A'           D55TPT
497200000829     C                   MOVE      V06TE2        D55LIN
497300000829     C     VIDFGS        IFGT      0
497400000829     C                   MOVE      VIDFGS        D55LNP
497500000829     C                   ELSE
497600000829     C                   MOVE      SIMFEL        D55LNP
497700000829     C                   ENDIF
497800000829     C                   MOVE      VIDDFV        WDAT4A
497900000829     C                   MOVEL     VIDDFV        WDAT4W            4
498000000829     C                   MOVEL     WDAT4W        WDAT2G
498100000829     C                   MOVE      WDAT4W        WDAT2M
498200000829     C                   MOVE      WDAT8A        D55DRF
498300000829     C                   CALL      'FNLV55R'
498400020805     C                   PARM                    FNLV55DS
498500000829     C     D55ERR        IFNE      ' '
498600000829     C                   SETON                                        62
498700000829     C                   ELSE
498800000829     C     D55LIN        IFNE      D55TFA
498900000829     C                   SETON                                        62
499000000829     C                   ELSE
499100000829     C     VIDTTR        IFEQ      'L'
499200000829     C     D55LIN        ANDNE     D55LNP
499300000829     C                   SETON                                        62
499400000829     C                   ELSE
499500000829     C     D55LIN        CHAIN     AZORG01L                           69
499600000829     C  N69              MOVEL(P)  ORGDES        V06DE2
499700000829     C                   ENDIF
499800000829     C                   ENDIF
499900000829     C                   ENDIF
500000000829     C                   ENDIF
500100000829     C                   ENDIF
500200000829      *
500300000829     C     *IN62         IFEQ      *ON
500400130315     C                   UPDATE    lst4S06
500500000829     C                   LEAVE
500600000829     C                   ENDIF
500700000829      *
500800000829     C     V06TE3        IFNE      *BLANKS
500900000829     C                   TESTN                   V06TE3               69
501000000829     C                   MOVE      V06TE3        W1ALF             1
501100000829     C     *IN69         IFEQ      *OFF
501200000829     C     W1ALF         ORLT      '0'
501300000829     C                   SETON                                        68
501400000829     C                   ELSE
501500020805     C                   CLEAR                   FNLV55DS
501600000829     C                   MOVEL     'A'           D55TPT
501700000829     C                   MOVE      V06TE3        D55LIN
501800000829     C     VIDFGS        IFGT      0
501900000829     C                   MOVE      VIDFGS        D55LNP
502000000829     C                   ELSE
502100000829     C                   MOVE      SIMFEL        D55LNP
502200000829     C                   ENDIF
502300000829     C                   MOVE      VIDDFV        WDAT4A
502400000829     C                   MOVEL     VIDDFV        WDAT4W            4
502500000829     C                   MOVEL     WDAT4W        WDAT2G
502600000829     C                   MOVE      WDAT4W        WDAT2M
502700000829     C                   MOVE      WDAT8A        D55DRF
502800000829     C                   CALL      'FNLV55R'
502900020805     C                   PARM                    FNLV55DS
503000000829     C     D55ERR        IFNE      ' '
503100000829     C                   SETON                                        68
503200000829     C                   ELSE
503300000829     C     D55LIN        IFNE      D55TFA
503400000829     C                   SETON                                        68
503500000829     C                   ELSE
503600000829     C     VIDTTR        IFEQ      'L'
503700000829     C     D55LIN        ANDNE     D55LNP
503800000829     C                   SETON                                        68
503900000829     C                   ELSE
504000000829     C     D55LIN        CHAIN     AZORG01L                           69
504100000829     C  N69              MOVEL(P)  ORGDES        V06DE3
504200000829     C                   ENDIF
504300000829     C                   ENDIF
504400000829     C                   ENDIF
504500000829     C                   ENDIF
504600000829     C                   ENDIF
504700000829      *
504800000829     C     *IN68         IFEQ      *ON
504900130315     C                   UPDATE    lst4S06
505000000829     C                   LEAVE
505100000829     C                   ENDIF
505200000829      *
505300000829     C     V06TE2        IFEQ      *BLANKS
505400000829     C     V06TE3        ANDNE     *BLANKS
505500000829     C                   SETON                                        62
505600130315     C                   UPDATE    lst4S06
505700000829     C                   LEAVE
505800000829     C                   ENDIF
505900000829      *
506000000829     C     V06TE3        IFEQ      *BLANKS
506100000829     C     V06TE2        ANDNE     *BLANKS
506200000829     C                   SETON                                        68
506300130315     C                   UPDATE    lst4S06
506400000829     C                   LEAVE
506500000829     C                   ENDIF
506600000829      *
506700130315     C                   UPDATE    lst4S06
506800000829      *
506900000829     C                   ENDDO
507000000829      *
507100000828      * SE VI SONO ERRORI O NON E' STATO PREMUTO F6 RIEMETTO
507200000828      *
507300000829     C     *IN62         IFEQ      *ON
507400000829     C     *IN68         OREQ      *ON
507500000829     C                   Z-ADD     NRR6          REC6
507600000829     C                   GOTO      INIINT
507700000829     C                   ENDIF
507800000828     C  N66              GOTO      INIINT
507900000828      *
508000000829      * SE E' STATO PREMUTO F6 (INSERIMENTO) E NON VI SONO ERRORI
508100000828      * ESPLODO I P.O. DI ARRIVO PER AGGIUNGERLI NELLA VIDEATA PRECEDENTE
508200000829      *
508300000830      * CARICO TUTTE LE FILIALI DI AZORG00F CON IL RELATIVO TERMINAL
508400000830      * DI ARRIVO
508500000830      *
508600000830     C                   MOVE      WDAT8A        WDATFV            8 0
508700000830     C                   MOVE      ' '           WPOSIZ            1
508800000830     C                   Z-ADD     1             L                 4 0
508900000830     C     *LOVAL        SETLL     AZORG
509000000830     C                   READ      AZORG                                  83
509100000830 --- C     *IN83         DOWEQ     *OFF
509200000830     C     ORGFVA        IFEQ      ' '
509300000830     C     ORGFAG        IFEQ      'F'
509400000830     C     ORGFAG        OREQ      'A'
509500000830      * CARICO SOLO SE NELLA DESCRIZIONE 148 DELL'ORGANIGRAMMA
509600000830      *  C'E' SCRITTO CHE LA POSSO CARICARE PER FV
509700000830     C                   MOVEL     ORGDE8        OG148
509800000830     C     §OGLFV        IFNE      'N'
509900000830     C                   Z-ADD     ORGFIL        FLA(L)
510000020805     C                   CLEAR                   FNLV55DS
510100000830     C                   MOVEL     'A'           D55TPT
510200000830     C                   MOVE      ORGFIL        D55LIN
510300000830     C                   MOVE      WDATFV        D55DRF
510400000830     C                   CALL      'FNLV55R'
510500020805     C                   PARM                    FNLV55DS
510600000830     C                   Z-ADD     D55TFA        TAA(L)
510700000830     C                   ENDIF
510800000830     C                   ADD       1             L
510900000830     C                   ENDIF
511000000830     C                   ENDIF
511100000830     C                   READ      AZORG                                  83
511200000830 --- C                   ENDDO
511300000828      *
511400000829     C     1             DO        *HIVAL        WN
511500130315     C     WN            CHAIN     lst4S06                            69
511600000829     C   69              LEAVE
511700000829     C     V06TE2        IFNE      *BLANKS
511800000829     C                   MOVE      V06TE2        WTE1              3 0
511900000829     C                   MOVE      V06TE3        WTE2              3 0
512000000830     C                   EXSR      AGGTER
512100000829     C                   ENDIF
512200000829     C                   ENDDO
512300000829      *
512400000828     C     ENDINT        ENDSR
512500000829     C*----------------------------------------------------------------
512600000829     C* AGGIORNAMENTO LINEE DI ARRIVO DOPO F6 DI INSERIMENTO TERMINAL
512700000829     C*----------------------------------------------------------------
512800000829     C     AGGTER        BEGSR
512900000829     C                   SETON                                        23
513000001006     C                   CLEAR                   WTROV
513100000830      *
513200001006    1C     VIDFGS        IFGT      *ZEROS
513300000830     C                   MOVEL     VIDFGS        WTFP
513400000830     C                   ELSE
513500000830     C                   MOVEL     SIMFEL        WTFP
513600001006    1C                   ENDIF
513700000830     C                   MOVE      ' '           WTAR              1
513800000830      *
513900000830     C                   Z-ADD     1             L
514000000830     C     WTE1          LOOKUP    TAA(L)                                 31
514100001006    1C     *IN31         DOWEQ     *ON
514200000830     C*
514300000830     C* CONTROLLO SE ESISTE ECCEZIONE
514400000830     C                   MOVE      ' '           W001$             1
514500001006     C                   MOVEL     FLA(L)        W003A
514600001006     C* SELA LINEA TROVATA NON E' IL TERMINAL DI ARRIVO FINALE
514700001006   1AC     VIDLNA        IFNE      W003A
514800001006     C                   MOVEL     FLA(L)        WFIL
514900001006     C                   MOVEL     'A'           KEPA
515000000830     C*
515100000830     C     KCAE          SETLL     AZCAE01L
515200000830     C     KCAE          READE     AZCAE01L                               83
515300001006    2C     *IN83         DOWEQ     *OFF
515400001006    3C     CAEATB        IFEQ      ' '
515500000830     C     WDATFV        ANDGE     CAEDDE
515600000830     C     WDATFV        ANDLE     CAEDSC
515700000830     C*  PER SICUREZZA CONTROLLO CHE IL TERMINAL SIA <> DA ORG.
515800001006    4C     CAETFE        IFNE      WTE1
515900000830     C                   MOVEL     '1'           W001$
516000001006    4C                   ENDIF
516100000830     C                   SETON                                        83
516200001006    3C                   ENDIF
516300000830     C  N83KCAE          READE     AZCAE01L                               83
516400001006    2C                   ENDDO
516500000830     C*
516600000830     C* SE W001$ E' VUOTO AGGIUNGO FLA,L NELLE LNA DEL FOGLIO VIAGGIO
516700001006    2C     W001$         IFEQ      ' '
516800001006     C                   MOVEL     FLA(L)        WTER              3 0
516900000830     C                   EXSR      AGGSF3
517000000830     C                   MOVE      '1'           WTAR
517100001006    2C                   ENDIF
517200001006     C*
517300001006  X1AC                   ELSE
517400001006     C                   MOVEL     'S'           WTROV             1
517500001006   1AC                   ENDIF
517600000830     C*
517700000830     C                   ADD       1             L
517800000830     C     WTE1          LOOKUP    TAA(L)                                 31
517900001006    1C                   ENDDO
518000000830     C*
518100000830     C* CONTROLLO SE ESISTONO ECCEZIONI CON TERMINAL ARRIVO = WTE1
518200000830     C                   MOVEL     WTE1          WFIL
518300000830     C                   MOVEL     'A'           KEPA
518400000830     C     KCAE          SETLL     AZCAE03L
518500000830     C     KCAE          READE     AZCAE03L                               83
518600001006    1C     *IN83         DOWEQ     *OFF
518700001006    2C     CAEATB        IFEQ      ' '
518800000830     C     WDATFV        ANDGE     CAEDDE
518900000830     C     WDATFV        ANDLE     CAEDSC
519000000830     C*
519100001006     C                   MOVEL     CAETFA        W003A
519200000830     C* AGGIUNGO CAETFA NELLE LNA DEL FOGLIO VIAGGIO
519300001006    3C     W003A         IFNE      VIDLNA
519400000830     C     CAETFA        LOOKUP    FLA                                    31
519500001006    4C     *IN31         IFEQ      *ON
519600000830     C                   MOVEL     CAETFA        WTER
519700000830     C                   EXSR      AGGSF3
519800000830     C                   MOVE      '1'           WTAR
519900001006    4C                   ENDIF
520000001006   X3C                   ELSE
520100001006     C                   MOVEL     'S'           WTROV             1
520200001006    3C                   ENDIF
520300001006    2C                   ENDIF
520400000830     C*
520500000830     C     KCAE          READE     AZCAE03L                               83
520600001006    1C                   ENDDO
520700000830     C*
520800000830     C     WTAR          IFEQ      ' '
520900001006     C     WTROV         ANDEQ     ' '
521000000830     C                   MOVEL     WTE1          WTER
521100000830     C                   EXSR      AGGSF3
521200000830     C                   ENDIF
521300000830      *
521400000829     C                   ENDSR
521500000830     C*----------------------------------------------------------------
521600000830     C* AGGIORNAMENTO SUBFILE LNA DA INSERIMENTO TERMINAL
521700000830     C*----------------------------------------------------------------
521800000830     C     AGGSF3        BEGSR
521900000830      *
522000000830     C     1             DO        *HIVAL        LL                4 0
522100130315     C     LL            CHAIN     lst4S03                            22
522200000830     C   22              LEAVE
522300000830     C     VIDFFV        IFEQ      *BLANKS
522400000830     C     VIDFLP        ANDEQ     *BLANKS
522500000830     C                   CLEAR                   TIPFLP
522600000830     C                   CLEAR                   TIPFFV
522700000830     C                   MOVEL(P)  '<-- Ins.'    DESFFV
522800000830     C                   MOVEL(P)  '<-- Ins.'    DESFLP
522900000830     C                   MOVEL     WTER          VIDFFV
523000000830     C                   MOVEL     WTE2          VIDFLP
523100130315     C                   UPDATE    lst4S03
523200000830     C     WPOSIZ        IFEQ      ' '
523300000830     C                   MOVE      'P'           WPOSIZ
523400000830     C                   Z-ADD     NRR3          REC3
523500000830     C                   ENDIF
523600000830     C                   LEAVE
523700000830     C                   ENDIF
523800000830     C                   ENDDO
523900000830      *
524000000830     C                   ENDSR
524100960301     C*-----------------------------------------------------***********
524200960301     C* SCRIVO RECORD DI SBINAMENTO  IN FNFV4
524300960301     C*-----------------------------------------------------***********
524400960301     C     WRTSBI        BEGSR
524500960302     C                   MOVEL     'P'           KEPA
524600960302     C                   CLEAR                   KTFA
524700960302     C*
524800960302     C                   DO        WREC
524900960302     C                   CLEAR                   FNFV4000
525000960302     C                   MOVEL     SBI(I)        FV4NOT
525100970103     C     I             IFLT      15
525200110512     C     I             orge      20
525300960302     C                   ADD       1             I
525400970103     C                   ENDIF
525500960302     C                   EXSR      WTRFV4
525600970103     C* PREPARO ANCORA SBI PER 17 / 18 / 19
525700970103     C     *IN11         IFEQ      *ON
525800970103     C                   ADD       1             XX
525900970103     C                   EXSR      PREAGG
526000970103     C                   ENDIF
526100970103     C     *IN12         IFEQ      *ON
526200970103     C                   ADD       1             ZZ
526300970103     C                   EXSR      PREAGG
526400970103     C                   ENDIF
526500970103     C     *IN13         IFEQ      *ON
526600970103     C                   ADD       1             YY
526700970103     C                   EXSR      PREVAR
526800970103     C                   ENDIF
526900970103     C**
527000960302     C                   ENDDO
527100960301     C                   ENDSR
527200960126     C*-----------------------------------------------------***********
527300960126     C* TRASMISSIONE  DELLE VARIAZIONI APPORTATE AL FV
527400960126     C*-----------------------------------------------------***********
527500960126     C     TRAVAR        BEGSR
527600960126     C* SOLO SE IL FV E' CHIUSO
527700960119    2C  N80FGVFCF        IFNE      ' '
527800960119     C                   MOVEL     'M'           PA2FLG
527900960119     C                   MOVEL     VIDNFV        PA2FGV
528000080228     C* CARICO L1 --> FILIALI GESTITE
528100080228     C                   CLEAR                   TRUL06DS
528200080228     C                   MOVE      '£1'          D06COD
528300080228     C                   MOVEL     SIMFEL        D06KEY
528400080228     C                   MOVE      'S'           D06KEY
528500080228     C                   MOVEL     fgvdfv        D06drf
528600080228     C                   MOVEL     TRUL06DS      KPJBU
528700080228     C*
528800080228     C                   CALL      'TRUL06R'
528900080228     C                   PARM                    KPJBA
529000080228     C                   MOVEL     KPJBU         TRUL06DS
529100080228     C                   MOVEA     LIN           L1ela
529200960119     C                   MOVEL     PARAM2        KPJBU
529300000824     C*
529400000824     C                   CLEAR                   KQEBI
529500000824     C                   CLEAR                   KQIEB
529600130718     C                   MOVEL     'LSU2'        KCOAZ
529700960119     C                   CALL      'BCH10'
529800960119     C                   PARM                    KPJBA
529900960119     C*
530000960119     C                   MOVEL     '1'           WLSA4
530100960126     C                   CLEAR                   WAGG
530200960119    2C                   ENDIF
530300951120     C*
530400960126     C                   ENDSR
530500960122     C*-----------------------------------------------------***********
530600960122     C* CALCOLO DELLE DATE TEORICHE DI PARTENZE E ARRIVO
530700960122     C*-----------------------------------------------------***********
530800960122     C     DTTEOR        BEGSR
530900960122     C* SE TRAINO ABBINATO
531000960130    1C     VIDTRN        IFGT      0
531100020805     C                   CLEAR                   TNTL35DS
531200960122     C* DATA PARTENZA TEORICA
531300960122     C* IN MANUTENZIONE LA CALCOLO SOLO SE NON C'E'
531400960122     C                   MOVEL     'T'           KTDH
531500960129     C                   MOVEL     'P'           KEPA
531600960129     C                   MOVEL     'P'           D35EPA
531700960122     C                   MOVEL     'D'           D35TUP
531800960122     C                   MOVEL     VIDTRN        D35TRN
531900960122     C                   MOVEL     WDDE          D35DDE
532000960122     C                   MOVEL     WDTN          D35DTN
532100960122     C                   CLEAR                   WTFA
532200960122     C*
532300960122     C     KFV22         SETLL     FNFV2000                               31
532400960122    2C     *IN31         IFEQ      *OFF
532500960129     C* PASSO IL NUMERO FERMATA
532600960129     C     WTFM          IFEQ      'A'
532700960129     C                   MOVEL     NFP(1)        D35NFM
532800960129     C                   ELSE
532900960129     C                   MOVEL     NFP(2)        D35NFM
533000960129     C                   ENDIF
533100960122     C                   EXSR      CALT35
533200960122     C   90              GOTO      ENDDTT
533300960122    2C                   ENDIF
533400960122     C*  DATE TEORICHE ARRIVO
533500960122     C*  LETTURA DAI METRI CUBI
533600960122     C                   MOVEL     'A'           D35EPA
533700960129     C                   MOVEL     'A'           KEPA
533800960122     C                   CLEAR                   D35NFM
533900960122     C                   MOVEL     WTFM          D35TFM
534000000612     C                   Z-ADD     VIDFGS        WTFP
534100960122     C     KTLM1         SETLL     TNTLM01L
534200960122     C     KTLM1         READE     TNTLM01L                               83
534300960122     C*
534400960122    2C     *IN83         DOWEQ     *OFF
534500960122    3C     TLMTFA        IFNE      WTFA
534600960122     C     TLMATB        ANDEQ     ' '
534700960122     C                   MOVEL     TLMTFA        WTFA
534800960122     C     KFV22         SETLL     FNFV2000                               31
534900960122    4C     *IN31         IFEQ      *OFF
535000960122     C                   CLEAR                   D35DTE
535100960122     C                   CLEAR                   D35HTE
535200960122     C                   MOVEL     TLMTFA        D35FIL
535300960122     C                   EXSR      CALT35
535400960122     C   90              SETON                                        83
535500960122    4C                   ENDIF
535600960122    3C                   ENDIF
535700960122     C*
535800960122     C  N90KTLM1         READE     TNTLM01L                               83
535900960122    3C                   ENDDO
536000960122     C*
536100960122   X1C                   ELSE
536200960122     C* SE TRAINO NON ABBINATO, E SONO IN MANUTENZIONE
536300960122     C*  CANCELLO LE DATE TEORICHE
536400960122    2C     *IN80         IFEQ      *OFF
536500960122     C                   MOVEL     'T'           KTDH
536600960129     C                   MOVEL     'P'           KEPA
536700960122     C                   CLEAR                   WTFA
536800960122     C     KFV22         DELETE    FNFV2000                           83
536900960122     C* DATE ARRIVO
537000960129     C                   MOVEL     'A'           KEPA
537100960130     C     KFV24         SETLL     FNFV2003
537200960130     C     KFV24         READE     FNFV2003                               83
537300960122    3C     *IN83         DOWEQ     *OFF
537400960130     C                   DELETE    FNFV2003
537500960122     C*
537600960130     C     KFV24         READE     FNFV2003                               83
537700960122    3C                   ENDDO
537800960122     C*
537900960122    2C                   ENDIF
538000960122    1C                   ENDIF
538100960122     C     ENDDTT        ENDSR
538200960122     C*-----------------------------------------------------***********
538300960122     C* CALL PGM TNTL35R E WRITE IN FNFV2
538400960122     C*-----------------------------------------------------***********
538500960122     C     CALT35        BEGSR
538600020805     C                   MOVEL     TNTL35DS      KPJBU
538700960123     C                   CALL      'TNTL35R'
538800960122     C                   PARM                    KPJBA
538900020805     C                   MOVEL     KPJBU         TNTL35DS
539000960122     C* C'E' ERRORE
539100960122     C     D35ERR        IFNE      *BLANKS
539200960122     C                   MOVEL     D35MSG        VIDMSG
539300960122     C                   SETON                                        2890
539400960122     C                   ELSE
539500960123     C* WRITE IN FNFV200F
539600960122     C                   CLEAR                   FNFV2000
539700960122     C                   MOVEL     D35EPA        FV2EPA
539800960122     C     FV2EPA        IFEQ      'A'
539900960122     C                   MOVEL     D35FIL        FV2LAI
540000960122     C                   ENDIF
540100960122     C                   MOVEL     FGVLNP        FV2LNP
540200960122     C                   MOVEL     FGVNFV        FV2NFV
540300960122     C                   MOVEL     'T'           FV2TDH
540400960122     C                   MOVEL     D35DTE        FV2DPA
540500960122     C                   MOVEL     D35HTE        FV2HPA
540600960122     C                   MOVEL     'N'           FV2FTR
540700960122     C                   MOVEL     99999999      FV2DTR
540800960122     C                   WRITE     FNFV2000
540900960122     C                   ENDIF
541000960122     C                   ENDSR
541100951018     C*-----------------------------------------------------***********
541200951031     C* SCRIVO FILE FNFV400F                                * WTRFV4  *
541300951031     C*-----------------------------------------------------***********
541400951031     C     WTRFV4        BEGSR
541500960123     C* SE FV4NOT
541600960126     C                   CLEAR                   FV4ATB
541700951031     C                   Z-ADD     FGVLNP        FV4LNP
541800951031     C                   Z-ADD     FGVNFV        FV4NFV
541900960123     C                   MOVEL     KEPA          FV4EPA
542000960123     C                   MOVEL     KTRC          FV4TRC
542100060711     C***  WTRAS1        IFNE      'S'
542200960126     C                   MOVEL     'N'           FV4FT1
542300060711     C                   MOVEL     dateu         FV4DT1
542400060711     C***                ELSE
542500060711     C***                CLEAR                   FV4FT1
542600060711     C***                CLEAR                   FV4DT1
542700060711     C***                ENDIF
542800960126     C     WTRAS2        IFNE      'S'
542900960126     C                   MOVEL     'N'           FV4FT2
543000060711     C                   MOVEL     dateu         FV4DT2
543100960126     C                   ELSE
543200960126     C                   CLEAR                   FV4FT2
543300960126     C                   CLEAR                   FV4DT2
543400960126     C                   ENDIF
543500951031     C*
543600951031     C                   WRITE     FNFV4000
543700951031     C                   ENDSR
543800951031     C*-----------------------------------------------------***********
543900951018     C* COPIA CAMPI RECORD IN CAMPI FILE VIDEO              * RIEMPI  *
544000951018     C*-----------------------------------------------------***********
544100900515     C     RIEMPI        BEGSR
544200951130     C                   SETOFF                                       16
544300951018     C*
544400911105     C                   Z-ADD     1             J                 3 0
544500911108     C                   Z-ADD     1             Y                 3 0
544600911105     C                   MOVEA     *BLANKS       LAB
544700911108     C                   MOVEA     *BLANKS       LAS
544800940921     C*
544900951018     C* GIRO DATA FOGLIO VIAGGIO
545000951130     C                   MOVE      FGVDFV        WSDFV
545100960126     C                   MOVE      FGVDTP        SAVDTP
545200960126     C                   MOVE      FGVHMP        SAVHMP
545300951130     C*
545400951130     C                   MOVE      FGVDFV        G02INV
545500900515     C                   MOVE      *ZEROS        G02DAT
545600940909     C                   MOVEL     '3'           G02ERR
545700940909     C                   CALL      'XSRDA8'
545800900515     C                   PARM                    WLBDAT
545900900515     C                   Z-ADD     G02DAT        VIDDFV
546000960302     C                   MOVEL     G02DAT        GGMMAA
546100951024     C                   Z-ADD     FGVDFV        AMGDFV
546200951025     C                   Z-ADD     G02TGI        WGGDFV
546300030701     c                   ExSr      Sr_MinMax
546400040204     c                   clear                   wd02
546500951027     C                   EXSR      CALDAT
546600951018     C*
546700130419     C                   MOVE      FGVLNP        VIDfgs
546800130419     C                   MOVE      FGWlnp        CODFIL
546900000516     C                   EXSR      CTRFIL
547000000516     C  N34              MOVEL     ORGDES        DESFGS
547100000516     C   34              CLEAR                   DESFGS
547200130419     C**   FGWFGS        COMP      0                                  0505
547300130419     C** 05              MOVE      FGWFGS        VIDFGS
547400940913     C                   MOVE      FGVPDR        VIDPDR
547500130424     C                   MOVE      FGVPDR        savpdr
547600951025     C                   MOVE      FGVDPD        VIDDPD
547700010920      * se l'importo sul file dell'estensione è > 0 non vado sul file principale
547800010920     C     FGWIPT        IFGT      *ZEROS
547900010920     C                   Z-ADD     FGWIPT        VIDIPT
548000010920     C                   MOVEL     FGWVIP        VIDVIP
548100011015     C                   MOVEL     FGWVIP        SAVDIV
548200011204     C                   MOVEL     FGWVIP        SAWVIP
548300010920     C                   ENDIF
548400010920      *
548500900515     C                   MOVE      FGVLNA        VIDLNA
548600951026     C                   MOVE      FGVNFV        VIDNFV
548700960123     C                   MOVE      FGVSNP        VIDBIS
548800951018     C* SE ESISTONO BOLLE BORDERIZZATE NON POSSO TOGLIERE LA LINEA
548900911105     C                   MOVE      FGVLNA        COMLNA
549000960126     C                   MOVE      FGVLNA        CODFIL
549100960123     C**
549200130315     C     ilst4ges      IFNE      'N'
549300020805     C     KBLP1         SETLL     FNBLP000                               31
549400911105     C   31              MOVEL     FGVLNA        LAB(J)
549500911105     C   31              ADD       1             J
549600951025     C* SE ESISTONO SPUNTE  NON POSSO TOGLIERE LA LINEA
549700000202     C                   Z-ADD     VIDNFV        WNFV
549800070112     C     KFGV1         SETLL     FNBRV02L                               31
549900951025     C   31              MOVEL     FGVLNA        LAS(Y)
550000951026     C   31              ADD       1             Y
550100960123     C                   ENDIF
550200960123     C**
550300951027     C* DECODIFICO LA LINEA FINALE DI ARRIVO
550400061116     c                   clear                   okestero
550500960126     C                   EXSR      CTRFIL
550600960126     C  N34              MOVEL     ORGDES        DESLNA
550700061116     c                   eval      okestero = esteroedpd
550800061116     c
550900960226     C                   MOVEL     'A'           KEPA
551000960226     C                   MOVEL     FGVLNA        KTFA
551100970102     C* CERCO NELLE ECCEZIONI SE LA LNA HA ALTRI TERMINAL DI ARRIVO
551200130419     C*  CHE POSSONO CORRISPONDERE A VIDfgs
551300970102     C                   EXSR      RICEC2
551400971203     C                   MOVEA     TFE           TFEL
551500951018     C*
551600971203     C* DETERMINO TER-ARR DELLA FILIALE PRENDENDO, SE ESISTE, QUELLO
551700971203     C* DA ECCEZIONI ALTRIMENTI QUELLO STANDARD
551800020805     C                   CLEAR                   FNLV55DS
551900971203     C                   MOVEL     'A'           D55TPT
552000971203     C                   MOVE      FGVLNA        D55LIN
552100000602     C                   MOVE      VIDFGS        D55LNP
552200971203     C                   MOVE      AMGDFV        D55DRF
552300971203     C                   CALL      'FNLV55R'
552400020805     C                   PARM                    FNLV55DS
552500971203     C                   MOVE      D55TFA        LNAFE0
552600970102     C*
552700951018     C* GIRO DATA PARTENZA EFFETTIVA
552800960123    1C     FGVDTP        IFGT      0
552900900515     C                   MOVE      FGVDTP        G02INV
553000900515     C                   MOVE      *ZEROS        G02DAT
553100940909     C                   MOVEL     '3'           G02ERR
553200940909     C                   CALL      'XSRDA8'
553300900515     C                   PARM                    WLBDAT
553400900515     C                   Z-ADD     G02DAT        VIDDTP
553500900515     C                   Z-ADD     FGVHMP        VIDHMP
553600960129     C                   MOVEL     '1'           WFORZC
553700951018     C*
553800951106     C* CAUSALE RITARDO/ANTICIPO PARTENZA FOGLIO
553900951106     C* SE ESISTE GIA' LA CARICO A VIDEO
554000960122     C                   MOVEL     'R'           KTDH
554100960123     C                   MOVEL     'P'           KEPA
554200960123     C     KFV2          CHAIN(N)  FNFV201L                           83
554300951103    2C     *IN83         IFEQ      *OFF
554400951103     C     FV2ATB        ANDEQ     ' '
554500951103     C                   MOVEL     FV2RTC        VIDRTC
554600951106     C                   MOVEL     FV2RTC        SAVRTC
554700951103     C* DECODIFICA
554800951103     C                   MOVEL     'TC'          COD
554900951103     C                   MOVEL(P)  VIDRTC        KEY
555000951103     C     KTAB          CHAIN     TABEL                              83
555100951103    3C     *IN83         IFEQ      *OFF
555200951103     C     TBLFLG        ANDEQ     ' '
555300960208     C                   MOVEL     TBLUNI        DSTC
555400960208     C                   MOVEL     §TCDES        DESRT1
555500960208     C                   MOVE      §TCDES        DESRT2
555600951103    3C                   ENDIF
555700951103    2C                   ENDIF
555800951108     C*
555900951108     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
556000951108     C*   EVIDENZIO IL COMANDO
556100960123     C                   MOVEL     'P'           KEPA
556200960123     C                   MOVEL     'R'           KTRC
556300960123     C                   CLEAR                   KTFA
556400951108     C                   EXSR      CTRDRA
556500951108    2C     W001A         IFNE      ' '
556600951108     C                   SETON                                        16
556700951108    2C                   ENDIF
556800951103    1C                   ENDIF
556900951103     C*
557000000602     C* TIPO AUTOMEZZO + DECODIFICA
557100900606     C                   MOVEL     FGVTMZ        VIDTMZ
557200951025     C                   EXSR      CTRTMZ
557300951025     C*
557400130430     C***                MOVEL     FGVDCN        VIDDCN
557500130430     C***                MOVEL     FGVDCN        savdcn
557600130430     C                   MOVEL     FGVPMB        VIDDCN
557700130430     C                   MOVEL     FGVPMB        savdcn
557800010621      *
557900010621      *REPERISCO IL NUMERO DEL CELLULARE
558000130419     C                   MOVEL     VIDfgs        FGVLNP
558100010621     C                   Z-ADD     VIDNFV        FGVNFV
558200010622     C                   MOVE      'P'           KEPA
558300010622     C                   Z-ADD     0             WTFA
558400010622     C                   MOVE      'L'           KTDH
558500040517     C     KFV22         CHAIN(N)  FNFV401L                           30
558600010622     C  N30              MOVEL     FV4NOT        VIDCNC
558700010621      *
558800950913     C                   MOVEL     FGVTTR        VIDTTR
558900951027     C                   MOVEL     'TV'          COD
559000951027     C                   MOVEL(P)  VIDTTR        KEY
559100951027     C     KTAB          CHAIN     TABEL                              83
559200951027     C  N83              MOVEL     TBLUNI        DESTTR
559300951027     C  N83              MOVEL     TBLUNI        DSTV
559400951027     C   83              CLEAR                   DSTV
559500951027     C*
559600951027     C                   MOVEL     FGVTRM        VIDTRM
559700900515     C                   MOVEL     FGVTRR        VIDTRR
559800951018     C*
559900130424     C* reperisco pimobi da fnfv4
560000130424     C                   MOVEL     'P'           KTRC
560100130424     C                   CLEAR                   KTFA
560200130424     C                   MOVEL     'P'           KEPA
560300130424     C     KFV4          SETLL     FNFV401L
560400130424     C     KFV4          READE(N)  FNFV401L                               83
560500130424     C     *IN83         IFEQ      *OFF
560600130424     C     FV4ATB        ANDEQ     ' '
560700130424     C                   MOVEL     FV4NOT        VIDpmb
560800130424     C     KFV4          READE(N)  FNFV401L                               83
560900130424     C  N83FV4ATB        IFEQ      ' '
561000130424     C                   eval      %subst(vidpmb:36)=fv4not
561100130424     C                   ENDIF
561200130424     C                   ENDIF
561300130424     c
561400900606     C                   MOVEL     FGVNT1        VIDNT1
561500900606     C                   MOVEL     FGVNT2        VIDNT2
561600900628     C                   MOVEL     FGVSNS        VIDSNS
561700960123     C                   MOVEL     FGVFRQ        WTFM
561800960123     C                   Z-ADD     FGVTRN        VIDTRN
561900951025     C* CARICO LINEE NEL SFL
562000000427     C                   MOVEL     FGWFF3        W1FF3
562100000427     C                   MOVEL     FGWFF4        W1FF4
562200000427     C                   MOVEL     FGWFL3        W1FL3
562300000427     C                   MOVEL     FGWFL4        W1FL4
562400951025     C                   EXSR      CARSFL
562500110512     c
562600110512     c* Carico punto di scambio
562700110512     C     K02FGv01      chain(N)  FNFGs000
562800110512if  1c                   if        (not %found(FNFGs01L)
562900110512     c                              or  FGsatb <> *blanks)
563000110512     C                   clear                   FNFGs000
563100110512e   1C                   endif
563200110530     c
563300110530    1c                   if        fgspsc<>*blanks
563400110530     c                   seton                                        08
563500110512     c                   eval      vidpsc=fgspsc
563600110531     c                   eval      OLDpsc=fgspsc
563700110513     c     vidpsc        chain     tnpsc01l
563800110530    2c                   if        %found(tnpsc01l)
563900110513     c                   eval      despsc=pscloc
564000110530     c                   eval      despind=pscind
564100110530     c                   eval      desploc=%trim(psccap) + ' ' +
564200110531     c                             %trim(pscloc)+'  ' +pscprv+pscnar
564300110530     c
564400110530   x2c                   else
564500110513     c                   eval      despsc=*all'?'
564600110530     c                   eval      despind=*all'?'
564700110530    2c                   endif
564800110513     c
564900110530    2c                   if        fgstrns>0
565000110530     c                   eval      vidtrns=fgstrns
565100110513     c                   exsr      Ctrtrns
565200110530    2c                   endif
565300110513
565400110512     c                   eval      vidhhs=fgshhs
565500110512     C                   MOVE      FGsdgs        G02INV
565600110512     C                   MOVE      *ZEROS        G02DAT
565700110512     C                   MOVEL     '3'           G02ERR
565800110512     C                   CALL      'XSRDA8'
565900110512     C                   PARM                    WLBDAT
566000110512     C                   Z-ADD     G02DAT        VIDdgs
566100110530     c
566200110530     c* linea finale arrivo autista
566300110530    2c                   if        fgslnar>0
566400110530     c                   move      fgslnar       vidlnar
566500110530     c                   move      fgslnar       codfil
566600110530     c                   exsr      ctrfil
566700110530     c                   movel     orgdes        deslnar
566800110601     c                   movel     orgdes        deslnar2
566900110530    2c                   endif
567000110530    1c                   endif
567100110512      *
567200951025     C*
567300951025     C* FV ABBINATO --> CERCO TRAINO
567400951018    1C     FGVTRN        IFGT      0
567500960123     C*
567600130315   1AC     ilst4ges      IFNE      'N'
567700951018     C* TIPO FERMATA TRAINO: "A-ANDATA" / "R-RITORNO"
567800951025     C* CERCO DECORRENZA TRAINO CORRETTA
567900951025     C                   EXSR      CTRDEC
568000951025     C* TRAINO NON TROVATO
568100951025    2C     WAN           IFEQ      ' '
568200951025     C     WRI           ANDEQ     ' '
568300951025     C                   MOVEL     MSG(34)       VIDMSG
568400951025     C                   MOVEA     MSG(34)       K78
568500951025     C                   MOVEL     FGVTRN        W007A             7
568600951025     C                   MOVEA     W007A         K15(9)
568700951026     C                   Z-ADD     9             Z
568800951025     C                   EXSR      ABBL
568900951025     C                   MOVEA     K15(9)        K78(23)
569000951025     C                   MOVEA     K78           VIDMSG
569100951025     C                   CLEAR                   VIDTRN
569200951027     C                   SETON                                        289048
569300960123   X2C                   ELSE
569400951025     C*
569500951025     C* SALVO I CAMPI PER CONTROLLARE SE VIENE MODIFICATO QUALCOSA
569600951025     C                   MOVEL     VIDLNA        SAVLNA
569700951025     C                   MOVEL     VIDPDR        TRNPDR
569800951025     C                   MOVEL     VIDTMZ        TRNTMZ
569900010920     C                   MOVEL     VIDVIP        TRNVIP
570000010920     C                   Z-ADD     VIDIPT        TRNIPT
570100951025     C                   MOVEL     VIDLNA        TRNTFA
570200951027     C                   MOVEL     VIDTTR        SAVTTR
570300960130     C                   MOVEL     VIDBIS        SAVBIS
570400951027     C                   MOVEL     §TVUTF        TRNUTF
570500110512     C                   MOVEL     VIDpsc        SAVpsc
570600110530     C                   MOVEL     VIDlnar       SAVlnar
570700951130     C* SALVO LE LINEE
570800951130     C                   MOVEA     FFV           TFV
570900951130     C                   MOVEA     FLP           TLP
571000951025     C*
571100951025     C* MEMORIZZO IL TIPO FERMATA DA ELABORARE
571200951025    3C     WAN           IFEQ      'S'
571300951025     C                   MOVEL     'A'           WTFM
571400960122     C                   MOVEL     NFP(1)        WNFM
571500951025     C                   Z-ADD     WANDDE        WDDE
571600951123     C                   MOVEL     'A'           WFMFES
571700960122   X3C                   ELSE
571800951123     C* SE E' UN RITORNO VERIFICO SE C'ERA ANCHE ANDATA
571900951123     C*  PER VEDERE CHE DEVE ESPORRE L'IMPORTO DEL FV
572000951123     C                   MOVEL     'A'           WTFM
572100130419     C***  FGWFGS        IFGT      *ZEROS
572200130419     C***                Z-ADD     FGWFGS        WFIL
572300130419     C***                ELSE
572400130419     C***                Z-ADD     FGVLNP        WFIL
572500130419     C***                ENDIF
572600130419     c
572700130419     C                   Z-ADD     FGVLNP        WFIL
572800951123     C     KTLR3         SETLL     TNTLR03L                               83
572900960122    4C     *IN83         IFEQ      *ON
573000951123     C                   MOVEL     'A'           WFMFES
573100951123     C                   ELSE
573200951123     C                   MOVEL     'R'           WFMFES
573300960122    4C                   ENDIF
573400951123     C**
573500960122     C                   Z-ADD     WRIDDE        WDDE
573600960122     C                   MOVEL     NFP(2)        WNFM
573700951025     C                   MOVEL     'R'           WTFM
573800951025    3C                   ENDIF
573900951025     C* SALVO IL NUMERO TRAINO IMMESSO
574000951025     C                   MOVEL     COPTRN        SAVTRN
574100960123    2C                   ENDIF
574200960123   1AC                   ENDIF
574300150907     c*
574400150907     C* CHAINO LA TESTATA  con la giusta decorrenza
574500150907     C     KTLR11        CHAIN     TNTLT01L                           83
574600960122     C**
574700960122     C* CARICO LA DATA PARTENZA TEORICA
574800960123    2C     VIDTRN        IFGT      0
574900960122     C                   MOVEL     'T'           KTDH
575000960123     C                   MOVEL     'P'           KEPA
575100960123     C     KFV2          CHAIN(N)  FNFV201L                           83
575200960126     C* SE MANCA PROVO A SCRIVERLO
575300960126     C   83              EXSR      DTTEOR
575400960126     C*
575500960126     C     KFV2          CHAIN(N)  FNFV201L                           83
575600960123    3C     *IN83         IFEQ      *OFF
575700960122     C     FV2ATB        ANDEQ     ' '
575800960122     C                   MOVE      FV2DPA        G02INV
575900960122     C                   MOVE      *ZEROS        G02DAT
576000960122     C                   MOVEL     '3'           G02ERR
576100960122     C                   CALL      'XSRDA8'
576200960122     C                   PARM                    WLBDAT
576300960122     C                   Z-ADD     G02DAT        VIDDTT
576400960122     C                   Z-ADD     FV2DPA        AMGDTT
576500960122     C                   MOVEL     FV2HPA        VIDHMT
576600960123    3C                   ENDIF
576700960122     C* SE SONO IN CHIUSURA FV IMPOSTO LA DATA TEORICA NELLA DATA REALE
576800130315    3C     ilst4ges      IFEQ      'I'
576900130315     C     ilst4ges      OREQ      'C'
577000960122     C                   Z-ADD     VIDHMT        VIDHMP
577100960122     C                   Z-ADD     VIDDTT        VIDDTP
577200960123    3C                   ENDIF
577300951019     C*
577400960123    2C                   ENDIF
577500951018    1C                   ENDIF
577600950901     C*
577700960122     C* CHAIN PER PRENDERE IN FNFV400F LE ANNOTAZIONI
577800960123     C                   MOVEL     'N'           KTRC
577900960123     C                   CLEAR                   KTFA
578000960123     C                   MOVEL     'P'           KEPA
578100951031     C     KFV4          SETLL     FNFV401L
578200960124     C     KFV4          READE(N)  FNFV401L                               83
578300951031     C     *IN83         IFEQ      *OFF
578400960126     C     FV4ATB        ANDEQ     ' '
578500951031     C                   MOVEL     FV4NOT        VIDAN1
578600960124     C     KFV4          READE(N)  FNFV401L                               83
578700960126     C  N83FV4ATB        IFEQ      ' '
578800960126     C                   MOVEL     FV4NOT        VIDAN2
578900951031     C                   ENDIF
579000960126     C                   ENDIF
579100960123     C* VERIFICO SE C'E' ALMENO UNA DATA DI ARRIVO
579200960129     C                   MOVEL     'A'           KEPA
579300960123     C     KFV23         SETLL     FNFV2000                               15
579400990514      *
579500990514      * 03 ON --> DISABILITO F18-RIAPERTURA
579600961213     C                   SETOFF                                       03
579700990514     C                   MOVEL     FGVLNA        CODFIL
579800990514     C                   EXSR      CTRFIL
579900060113     c                   if        esteroedpd = 'S' and §tvdef = 'S'
580000961213     C                   SETON                                        03
580100990514     C                   ENDIF
580200130516     c
580300130516     c* posizinamento cursore
580400130516     c                   if        viddcn=*blanks and vidpdr>*zeros
580500130516     c                   seton                                        55
580600130516     c                   endif
580700130516     c                   if        vidtrm=*blanks and vidpdr>*zeros and
580800130516     c                             viddcn<>*blanks
580900130516     c                   seton                                        54
581000130516     c                   endif
581100990514      *
581200900515     C                   ENDSR
581300951025     C*-----------------------------------------------------***********
581400951025     C****  R I E M P I O   I L   S U B F I L E  ****         CARSFL
581500951025     C*-----------------------------------------------------***********
581600951025     C     CARSFL        BEGSR
581700951025     C                   Z-ADD     1             NRR3
581800951026     C                   Z-ADD     1             REC3
581900951025     C                   SETON                                        70
582000130315     C                   WRITE     lst4C03
582100951025     C                   SETOFF                                       70
582200130606     c* In manutenzione memorizzo le linee del foglio e relativi scarichi
582300130606     c* per avere immagine precedente le modifiche
582400130606     c                   if        not *in80
582500130606     c                   eval      pffv(1)=%editc(fgvlna:'X')
582600130606     c                   eval      pflp(1)=%editc(fgvlna:'X')
582700130606     c                   movea     ffv           pffv(2)
582800130606     c                   movea     flp           pflp(2)
582900130606     c                   endif
583000951025     C*
583100951025     C                   Z-ADD     1             I
583200000420    1C                   DO        299           I
583300951025     C*
583400951025     C* FILIALI DI ARRIVO
583500951025     C     FFV(I)        IFGT      *ZEROS
583600951025     C                   MOVE      FFV(I)        VIDFFV
583700951025     C                   MOVE      VIDFFV        CODFIL
583800960126     C                   EXSR      CTRFIL
583900960126    3C     *IN34         IFEQ      *OFF
584000951025     C                   MOVEL     ORGDES        DESFFV
584100951025     C                   MOVEL     ORGFAG        TIPFFV
584200951025    3C                   ENDIF
584300951025     C*
584400951025     C* SE ESISTONO BOLLE BORDERIZZATE NON POSSO TOGLIERE LA LINEA
584500951025     C* SONO IN VARIAZIONE
584600951025     C     *IN80         IFEQ      *OFF
584700951025     C                   MOVE      FFV(I)        COMLNA
584800020805     ***C     KBLP1         SETLL     FNBLP29L                               31
584900020805     C     KBLP1         SETLL     FNBLP000                               31
585000951025     C   31              MOVEL     VIDFFV        LAB(J)
585100951025     C   31              ADD       1             J
585200951025     C* SE ESISTONO SPUNTE NON POSSO TOGLIERE LA LINEA
585300000202     C                   Z-ADD     VIDNFV        WNFV
585400070112     C     KFGV1         SETLL     FNBRV02L                               31
585500951025     C   31              MOVEL     VIDFFV        LAS(Y)
585600951026     C   31              ADD       1             Y
585700951025     C                   ENDIF
585800951025     C*
585900951025   X2C                   ELSE
586000951025     C                   MOVE      *BLANKS       VIDFFV
586100951025     C                   MOVE      *BLANKS       DESFFV
586200951025     C                   MOVE      *BLANKS       TIPFFV
586300951025    2C                   ENDIF
586400951025     C* FILIALI DI SCARICO
586500951025     C     FLP(I)        IFGT      *ZEROS
586600951025     C                   MOVE      FLP(I)        VIDFLP
586700951025     C                   MOVE      FLP(I)        SAVFLP
586800951025     C                   MOVE      VIDFLP        CODFIL
586900960126     C                   EXSR      CTRFIL
587000960126    3C     *IN34         IFEQ      *OFF
587100951025     C                   MOVEL     ORGDES        DESFLP
587200951025     C                   MOVEL     ORGFAG        TIPFLP
587300951025    3C                   ENDIF
587400951025     C*
587500951025   X2C                   ELSE
587600951025     C                   MOVE      *BLANKS       VIDFLP
587700951025     C                   MOVE      *BLANKS       SAVFLP
587800951025     C                   MOVE      *BLANKS       DESFLP
587900951025     C                   MOVE      *BLANKS       TIPFLP
588000951025    2C                   ENDIF
588100951025     C*
588200000829     C                   SETOFF                                       23
588300130315     C                   WRITE     lst4S03
588400951025     C                   ADD       1             NRR3
588500951025    1C                   ENDDO
588600951025     C                   ENDSR
588700951013     C*-----------------------------------------------------***********
588800951013     C* COPIA CAMPI SUBFILE DA FILE TRAINI                  * RIETRS  *
588900951013     C*-----------------------------------------------------***********
589000951013     C     RIETRS        BEGSR
589100971203     C*****
589200971203     C* CARICO TUTTE LE FILIALI DI AZORG00F CON IL RELATIVO TERMINAL
589300971203     C* DI ARRIVO
589400971203     C*****
589500971203     C                   Z-ADD     1             I
589600971203     C     *LOVAL        SETLL     AZORG
589700971203     C                   READ      AZORG                                  83
589800971203     C*
589900971203    1C     *IN83         DOWEQ     *OFF
590000971203    2C     ORGFVA        IFEQ      ' '
590100971203    3C     ORGFAG        IFEQ      'F'
590200971203     C     ORGFAG        OREQ      'A'
590300971203     C* CARICO SOLO SE NELLA DESCRIZIONE 148 DELL'ORGANIGRAMMA
590400971203     C*  C'E' SCRITTO CHE LA POSSO CARICARE
590500971203     C                   MOVEL     ORGDE8        OG148
590600971203     C     §OGLFV        IFNE      'N'
590700971203     C                   Z-ADD     ORGFIL        FLA(I)
590800020805     C                   CLEAR                   FNLV55DS
590900971203     C                   MOVEL     'A'           D55TPT
591000971203     C                   MOVE      ORGFIL        D55LIN
591100971203     C                   MOVE      WDTN          D55DRF
591200971203     C                   CALL      'FNLV55R'
591300020805     C                   PARM                    FNLV55DS
591400971203     C                   Z-ADD     D55TFA        TAA(I)
591500971203    3C                   ENDIF
591600971203     C                   ADD       1             I
591700971203    3C                   ENDIF
591800971203    2C                   ENDIF
591900971203     C*
592000971203     C                   READ      AZORG                                  83
592100971203    1C                   ENDDO
592200971203     C*
592300960202     C* VERIFICO SE STO CARCANDO ANDATA O RITORNO
592400960202     C* MI SERVE PER FVERIFICARE QUANDO ARRIVO ALLA 1 FERMATA
592500960202     C     WTFM          IFEQ      'A'
592600960202     C                   Z-ADD     1             I
592700960202     C                   ELSE
592800960202     C                   Z-ADD     2             I
592900960202     C                   ENDIF
593000960202     C*
593100951026     C                   CLEAR                   FFV
593200951026     C                   CLEAR                   FLP
593300951026     C                   Z-ADD     1             Z                 3 0
593400951025     C                   MOVEL     VIDLNA        WLNAF
593500000602     C     VIDFGS        IFGT      *ZEROS
593600000602     C                   MOVEL     VIDFGS        WTFP
593700000602     C                   ELSE
593800000602     C                   MOVEL     SIMFEL        WTFP
593900000602     C                   ENDIF
594000951025     C     KTLR1         SETGT     TNTLR01L
594100951025     C     KTLR11        READPE    TNTLR01L                               84
594200951009     C*
594300951012    1C     *IN84         DOWEQ     *OFF
594400951012    2C     TLRATB        IFEQ      ' '
594500951025     C* SE HO RAGGIUNTO LA FILIALE SIMFEL --> FINE LETTURA
594600951025     C*  SE MODIFICATO TIPO FERMATA       --> FINE LETTURA
594700960202     C* ESEGUO QUESTO CONTROLLO UTILIZZANDO IL NUMERO FERMATA
594800960202   2AC     TLRNFM        IFGT      NFP(I)
594900960202     C*
595000951025     C* SE NON E' FILIALE DI SCARICO NE LEGGO UN'ALTRA
595100951025   2BC     TLRSCA        IFEQ      'S'
595200951010     C                   MOVEL     TLRFIL        WTFA
595300951025     C* LEGGO I MC
595400951025     C     KTLM          SETLL     TNTLM01L
595500951025     C     KTLM          READE     TNTLM01L                               83
595600950831     C*
595700951012    3C     *IN83         DOWEQ     *OFF
595800951012    4C     TLMATB        IFEQ      ' '
595900951010     C*
596000960202     C* TIPO INOLTRO NON E' 'I': CARICO TUTTE LE FILIALI
596100960202     C*
596200960202   4AC     VIDTTR        IFNE      'I'
596300960202     C**
596400960823     C                   MOVE      ' '           WTAR              1
596500951026     C                   Z-ADD     1             I
596600951027     C     TLMTFF        LOOKUP    TAA(I)                                 31
596700951027    5C     *IN31         DOWEQ     *ON
596800951012     C*
596900951012     C* CONTROLLO SE ESISTE ECCEZIONE
597000951012     C                   CLEAR                   W001
597100951026     C                   MOVEL     FLA(I)        WFIL
597200950831     C*
597300960123     C                   MOVEL     'A'           KEPA
597400951012     C     KCAE          SETLL     AZCAE01L
597500951012     C     KCAE          READE     AZCAE01L                               85
597600951012    6C     *IN85         DOWEQ     *OFF
597700951012    7C     CAEATB        IFEQ      ' '
597800951012     C     WDTN          ANDGE     CAEDDE
597900951012     C     WDTN          ANDLE     CAEDSC
598000951025     C*  PER SICUREZZA CONTROLLO CHE IL TERMINAL TROVATO SIA <> DA ORG.
598100951025     C     CAETFE        IFNE      TLMTFF
598200950831     C                   MOVEL     '1'           W001
598300951025     C                   ENDIF
598400951012     C                   SETON                                        85
598500951012    7C                   ENDIF
598600951012     C  N85KCAE          READE     AZCAE01L                               85
598700951012    6C                   ENDDO
598800950831     C*
598900950831     C* SE W001 E' VUOTO SCRIVO FLA,C NELLE LNA DEL FOGLIO VIAGGIO
599000951012    6C     W001          IFEQ      ' '
599100951026     C     WLNAF         ANDNE     FLA(I)
599200951026     C                   MOVEL     FLA(I)        FFV(Z)
599300950905     C                   MOVEL     TLMTFA        FLP(Z)
599400951012     C                   ADD       1             Z
599500960823     C                   MOVE      '1'           WTAR
599600951012    6C                   ENDIF
599700950831     C*
599800951026     C                   ADD       1             I
599900951027     C     TLMTFF        LOOKUP    TAA(I)                                 31
600000951012    5C                   ENDDO
600100951012     C*
600200951012     C* CONTROLLO SE ESISTONO ECCEZIONI CON TERMINAL ARRIVO = TLMTFF
600300951025     C                   MOVEL     TLMTFF        WFIL
600400960123     C                   MOVEL     'A'           KEPA
600500951012     C     KCAE          SETLL     AZCAE03L
600600951012     C     KCAE          READE     AZCAE03L                               85
600700951012    5C     *IN85         DOWEQ     *OFF
600800950831    6C     CAEATB        IFEQ      ' '
600900951012     C     WDTN          ANDGE     CAEDDE
601000951012     C     WDTN          ANDLE     CAEDSC
601100950831     C*
601200950831     C* SCRIVO CAETFA NELLE LNA DEL FOGLIO VIAGGIO
601300960202     C     CAETFA        ANDNE     WLNAF
601400960208     C     CAETFA        LOOKUP    FLA                                    31
601500960208     C* Carico l'eccezione come filiale del foglio viaggio solo se
601600960208     C* presente in schiera filiali di azorg
601700960208     C     *IN31         IFEQ      *ON
601800950901     C                   MOVEL     CAETFA        FFV(Z)
601900951012     C                   MOVEL     TLMTFA        FLP(Z)
602000950901     C                   ADD       1             Z
602100960823     C                   MOVE      '1'           WTAR
602200960208    3C                   ENDIF
602300960208     C*
602400950831    6C                   ENDIF
602500960208     C*
602600951012     C     KCAE          READE     AZCAE03L                               85
602700950831    5C                   ENDDO
602800950831     C*
602900960202     C*
603000960823   4AC                   ENDIF
603100960202     C*
603200960202     C* TIPO INOLTRO E' "I"  --> CARICO SOLO LA LINEA DI ARRIVO FINALE
603300960823     C* IDEM SE NON E' INOLTRO MA E' SCARICO DIRETTO A FIL. NO TER-ARR
603400960823   4AC     TLTTTR        IFEQ      'I'
603500960823     C     WTAR          OREQ      ' '
603600960202    5C     WLNAF         IFNE      TLMTFF
603700960202     C                   MOVEL     TLMTFF        FFV(Z)
603800960202     C                   MOVEL     TLMTFA        FLP(Z)
603900960202     C                   ADD       1             Z
604000960202    5C                   ENDIF
604100960202   4AC                   ENDIF
604200960202    4C                   ENDIF
604300960202     C**
604400951025     C     KTLM          READE     TNTLM01L                               83
604500950831    3C                   ENDDO
604600951025   2BC                   ENDIF
604700951025     C*
604800951025  X2AC                   ELSE
604900951025     C                   SETON                                        84
605000951025   2AC                   ENDIF
605100950831    2C                   ENDIF
605200950831     C*
605300951025     C  N84KTLR11        READPE    TNTLR01L                               84
605400950831    1C                   ENDDO
605500950831     C*
605600951013     C****  S C R I V O   I L   S U B F I L E  ****
605700951025     C                   EXSR      CARSFL
605800951130     C* SALVO LE LINEE
605900951130     C                   MOVEA     FFV           TFV
606000951130     C                   MOVEA     FLP           TLP
606100950901     C*
606200950830     C                   ENDSR
606300920428      *-----------------------------------------------------***********
606400951026      * SFLAGGO  LE SPUNTE GIA' FLEGGATE SE MODIFICATA FIL DI SCARICO
606500920428      *-----------------------------------------------------***********
606600920428     C     LIBSPU        BEGSR
606700920428      *
606800000202     C                   Z-ADD     VIDNFV        WNFV
606900070112     C     KFGV1         SETLL     FNBRV02L
607000070112     C     KFGV1         READE     FNBRV02L                               31
607100980305     C     *IN31         DOWEQ     '0'
607200980305     C     WMOD          IFEQ      *BLANKS
607300980305     C     BRVFTR        ANDNE     ' '
607400980305     C     WMOD          OREQ      'A'
607500980305     C     BRVFTR        ANDEQ     'L'
607600940912     C                   MOVEL     ' '           BRVFTR
607700940912     C                   MOVEL     *ZEROS        BRVDTR
607800000202     C                   MOVEL     *ZEROS        BRVHTR
607900980305     C     WMOD          IFEQ      'A'
608000980305     C     BRVCAN        ANDEQ     'E'
608100980305     C                   CLEAR                   BRVCAN
608200980305     C                   END
608300070112     C                   UPDATE    FNBRV000
608400951025     C                   ENDIF
608500951025     C*
608600070112     C     KFGV1         READE     FNBRV02L                               31
608700951025     C                   ENDDO
608800920428     C*
608900920428     C                   ENDSR
609000940913      *-----------------------------------------------------***********
609100940913     C*    SCRIVO FILE DI APPOGGIO FNFGV46L                 * WRTFVA  *
609200940913      *-----------------------------------------------------***********
609300931116     C     WRTFVA        BEGSR
609400940913     C*
609500940913     C* VERIFICO SE IL RECORD E' GIA' PRESENTE NEL FILE DI APPOGGIO
609600130419     C                   MOVE      VIDfgs        CODFIL
609700951024     C     KFGV46        CHAIN     FNFGV46L                           82
609800931116     C*
609900940912     C** RIEMPO CAMPI DEL FILE FNFGV46L
610000940913     C                   MOVEL     FGVATB        FG4ATB
610100940913     C                   MOVEL     FGVNFV        FG4NFV
610200940913     C                   MOVEL     FGVDFV        FG4DFV
610300940913     C                   MOVEL     FGVLNP        FG4LNP
610400940913     C                   MOVEL     FGVPDR        FG4PDR
610500940913     C                   MOVEL     FGVDPD        FG4DPD
610600940913     C                   MOVEL     FGVCCN        FG4CCN
610700940913     C                   MOVEL     FGVDCN        FG4DCN
610800940913     C                   MOVEL     FGVTRM        FG4TRM
610900940913     C                   MOVEL     FGVTRR        FG4TRR
611000940913     C                   MOVEL     FGVTMZ        FG4TMZ
611100940913     C                   MOVEL     FGVTTR        FG4TTR
611200940913     C                   MOVEL     FGVFRQ        FG4FRQ
611300940913     C                   MOVEL     FGVIPT        FG4IPT
611400940913     C                   MOVEL     FGVSNS        FG4SNS
611500940913     C                   MOVEL     FGVSNP        FG4SNP
611600940913     C                   MOVEL     FGVLNA        FG4LNA
611700940913     C                   MOVEL     FGVFFV        FG4FFV
611800940913     C                   MOVEL     FGVFF2        FG4FF2
611900940913     C                   MOVEL     FGVFLP        FG4FLP
612000940913     C                   MOVEL     FGVFL2        FG4FL2
612100940913     C                   MOVEL     FGVDTP        FG4DTP
612200940913     C                   MOVEL     FGVHMP        FG4HMP
612300940913     C                   MOVEL     FGVNT1        FG4NT1
612400940913     C                   MOVEL     FGVNT2        FG4NT2
612500940913     C                   MOVEL     FGVPMB        FG4PMB
612600940913     C                   MOVEL     FGVFCF        FG4FCF
612700940913     C                   MOVEL     FGVTRN        FG4TRN
612800940913     C                   MOVEL     FGVNFA        FG4NFA
612900931116     C*
613000940913     C   82              WRITE     FNFGV046
613100940913     C  N82              UPDATE    FNFGV046
613200931116     C*
613300960119     C                   MOVEL     '1'           WLSA4             1
613400940909     C                   ENDSR
613500951103     C*-----------------------------------------------------***********
613600951103     C* SCRIVO FILE FNFV200F                                * WTRFV2  *
613700951103     C*-----------------------------------------------------***********
613800951103     C     WTRFV2        BEGSR
613900951103     C*
614000960123     C                   MOVEL     'R'           KTDH
614100960123     C                   MOVEL     'P'           KEPA
614200951103     C     KFV2          CHAIN     FNFV201L                           83
614300960130    1C     VIDRTC        IFEQ      ' '
614400960130     C  N83              DELETE    FNFV2000
614500960130   X1C                   ELSE
614600951103     C   83              CLEAR                   FNFV2000
614700951103     C*
614800951103     C                   MOVEL     ' '           FV2ATB
614900951106     C                   MOVEL     'R'           FV2TDH
615000951106     C                   MOVEL     FGVDTP        FV2DPA
615100951106     C                   MOVEL     FGVHMP        FV2HPA
615200951103     C                   MOVEL     VIDRTC        FV2RTC
615300951114     C                   CLEAR                   FV2FTR
615400951114     C                   CLEAR                   FV2DTR
615500951106     C*
615600951106     C* 83 OFF - VARIAZIONE
615700960130    2C     *IN83         IFEQ      *OFF
615800951103     C                   UPDATE    FNFV2000
615900951106     C*
616000960130   X2C                   ELSE
616100951106     C* 83 ON  - IMMISSIONE
616200951103     C                   MOVEL     SIMFEL        FV2LNP
616300951103     C                   MOVEL     VIDNFV        FV2NFV
616400951106     C                   MOVEL     'P'           FV2EPA
616500951103     C                   WRITE     FNFV2000
616600960130    2C                   ENDIF
616700960130    1C                   ENDIF
616800960111     C*
616900951103     C                   ENDSR
617000960123     C**************************************************************************
617100960123     C*  INTERROGO GLI ORARI DI ARRIVO
617200960123     C**************************************************************************
617300960123     C     INTORA        BEGSR
617400960129     C* SOLO A CAMBIO FV RICARICO IL SFL
617500960129     C     FGVNFV        IFNE      WINNFV
617600960129     C                   MOVEL     FGVNFV        WINNFV
617700960129     C*
617800960129     C                   Z-ADD     0             NRR5              5 0
617900960129     C                   MOVEL     'A'           KEPA
618000960129     C                   SETOFF                                       17
618100960123     C* PULIZIA SFL
618200960123     C                   SETON                                        70
618300130315     C                   WRITE     lst4C05
618400960123     C                   SETOFF                                       70
618500960129     C*
618600960129     C* SE HA GIA' RILEVATO CHE NON HO NIENTE, SCRIVO 1 REC VUOTO
618700960129    0C     *IN15         IFEQ      *ON
618800960123     C*
618900960129     C* LEGGO PER DATA TEORICA, SE CI SONO, ALTRIMENTI PER DATA REALE
619000960129     C                   MOVEL     'T'           KTDH
619100960129     C     KFV24         SETLL     FNFV2003
619200960130     C     KFV24         READE(N)  FNFV2003                               83
619300960129     C*
619400960129     C* NON CI SONO DATE TEORICHE: LEGGO SOLO LE REALI
619500960129    1C     *IN83         IFEQ      *ON
619600960129     C                   MOVEL     'R'           KTDH
619700960129     C**
619800960130     C     KFV24         SETLL     FNFV2003
619900960130     C     KFV24         READE(N)  FNFV2003                               83
620000960129    2C     *IN83         DOWEQ     *OFF
620100960129     C                   EXSR      DECORA
620200960129     C                   EXSR      WRTSF4
620300960129     C**
620400960130     C     KFV24         READE(N)  FNFV2003                               83
620500960129    2C                   ENDDO
620600960129     C**
620700960129   X1C                   ELSE
620800960129     C**
620900960129    2C     *IN83         DOWEQ     *OFF
621000960129     C                   EXSR      DECORA
621100960129     C*
621200960129     C                   MOVEL     'R'           KTDH
621300960129     C                   MOVEL     FV2LAI        WTFA
621400960129     C     KFV22         CHAIN(N)  FNFV201L                           83
621500960129     C                   EXSR      DECORA
621600960129     C                   EXSR      WRTSF4
621700960129     C**
621800960129     C                   MOVEL     'T'           KTDH
621900960130     C     KFV24         READE(N)  FNFV2003                               83
622000960129    2C                   ENDDO
622100960129    1C                   ENDIF
622200960129     C**
622300960129     C                   ELSE
622400960129     C** NON CI SONO RECORD DA CARICARE
622500130315     C                   CLEAR                   lst4S05
622600960129     C                   EXSR      WRTSF4
622700960129    0C                   ENDIF
622800960129     C*
622900960129     C                   ENDIF
623000960123     C*
623100960123     C     FOR05         TAG
623200130315     C                   WRITE     lst4T01
623300130315     C                   WRITE     lst4Z05
623400130315     C                   EXFMT     lst4C05
623500960126     C                   MOVEL     '5'           WRECOR            1
623600960129     C* F12 - RITORNO
623700960123     C     *INKL         IFEQ      *OFF
623800960123     C                   CLEAR                   W001A
623900130315     C                   READC     lst4S05                                83
624000960123     C     *IN83         DOWEQ     *OFF
624100960123     C     V5CVIS        IFEQ      'V'
624200960123     C                   CLEAR                   V5CVIS
624300960123     C                   SETON                                        17
624400130315     C                   UPDATE    lst4S05
624500960126     C*
624600020805     C                   CLEAR                   FNLV15DS
624700960126     C* SE PGM RICHIAMATO RICHIAMO CON 'L?'
624800960126     C   20              MOVEL     'L'           D15TLA
624900960126     C                   MOVEL     'A'           D15EPA
625000960126     C                   MOVEL     'R'           D15TRC
625100960126     C                   MOVEL     'I'           D15TUP
625200960126     C                   MOVEL     V5CRTC        D15RTC
625300130419     C                   MOVEL     VIDfgs        D15LNP
625400960126     C                   MOVEL     VIDNFV        D15NFV
625500960126     C                   MOVEL     AMGDFV        D15DFV
625600960126     C                   MOVEL     VIDTRN        D15TRN
625700960222     C                   MOVEL     VIDTTR        D15TTR
625800960126     C                   MOVEL     V5CFIL        D15LAI
625900960123     C*
626000020805     C                   MOVEL     FNLV15DS      KPJBU
626100960123     C                   CALL      'FNLV15R'
626200960123     C                   PARM                    KPJBA
626300960123     C                   MOVEL     '1'           W001A
626400960123     C                   ENDIF
626500960123     C*
626600130315     C                   READC     lst4S05                                83
626700960123     C                   ENDDO
626800960123     C*
626900960123     C     W001A         IFEQ      '1'
627000960123     C                   GOTO      FOR05
627100960123     C                   ENDIF
627200960123     C                   ENDIF
627300960123     C*
627400960123     C                   ENDSR
627500960129     C*---------------------------------------------------------------**********
627600960129     C* DECODIFICHE DEL SFL DELGLI ORARI DI ARRIVO
627700960129     C*---------------------------------------------------------------**********
627800960129     C     DECORA        BEGSR
627900960129     C* DATA
628000960129    1C     *IN83         IFEQ      *OFF
628100960129     C                   MOVE      FV2DPA        G02INV
628200960129     C                   MOVE      *ZEROS        G02DAT
628300960129     C                   MOVEL     '3'           G02ERR
628400960129     C                   CALL      'XSRDA8'
628500960129     C                   PARM                    WLBDAT
628600960129    1C                   ENDIF
628700960129     C*
628800960129    1C     KTDH          IFEQ      'T'
628900960129     C                   Z-ADD     G02DAT        V5CDTT
629000960129     C                   MOVEL     FV2HPA        V5CHMT
629100960129   X1C                   ELSE
629200960129     C*
629300960129     C* DECODIFICO ANCHE LA FILIALE DI INVIO
629400960129     C                   Z-ADD     FV2LAI        CODFIL
629500960129     C                   EXSR      CTRFIL
629600960129     C  N34              MOVEL     ORGDES        V5DFIL
629700960129     C   34              MOVEL     *ALL'?'       V5DFIL
629800960129     C                   MOVEL     FV2LAI        V5CFIL
629900960129     C**
630000960129    2C     *IN83         IFEQ      *OFF
630100960129     C                   Z-ADD     G02DAT        V5CDTR
630200960129     C                   MOVEL     FV2HPA        V5CHMR
630300960129     C* VEDO SE C'E' RITARDO
630400960129    3C     FV2RTC        IFNE      *BLANKS
630500960129     C                   MOVEL     FV2RTC        V5CRTC
630600960129     C                   MOVEL     'TC'          COD
630700960129     C                   MOVEL(P)  FV2RTC        KEY
630800960129     C     KTAB          CHAIN     TABEL                              83
630900960129     C  N83              MOVEL     TBLUNI        V5DRTC
631000960129     C   83              MOVEL     *ALL'?'       V5DRTC
631100960129     C* VERIFICO SE C'E' ANCHE LA DESCRIZIONE
631200960129     C                   MOVEL     'A'           KEPA
631300960129     C                   Z-ADD     FV2LAI        KTFA
631400960129     C                   MOVEL     'R'           KTRC
631500960129     C                   EXSR      CTRDRA
631600960129    4C     W001A         IFNE      *BLANKS
631700960129     C                   SETON                                        17
631800960129     C                   ELSE
631900960129     C                   SETOFF                                       17
632000960129    4C                   ENDIF
632100960129    3C                   ENDIF
632200960129    2C                   ENDIF
632300960129    1C                   ENDIF
632400960129     C**
632500960129     C                   ENDSR
632600960129     C*---------------------------------------------------------------**********
632700960129     C* SCRITTUTA DEL SFL 4 E PULIZIA DEI CAMPI
632800960129     C*---------------------------------------------------------------**********
632900960129     C     WRTSF4        BEGSR
633000960129     C                   ADD       1             NRR5
633100130315     C                   WRITE     lst4S05
633200960129     C                   CLEAR                   V5CDTT
633300960129     C                   CLEAR                   V5CHMT
633400960129     C                   CLEAR                   V5CDTR
633500960129     C                   CLEAR                   V5CHMR
633600960129     C                   CLEAR                   V5CRTC
633700960129     C                   CLEAR                   V5DRTC
633800960129     C                   ENDSR
633900940909     C**************************************************************************
634000940909     C*   R O U T I N E    I N I Z I A L E
634100940909     C**************************************************************************
634200940909     C     *INZSR        BEGSR
634300940909     C*
634400960122     C     KCLN1         KLIST
634500960122     C                   KFLD                    KTFP
634600960122     C                   KFLD                    KTFA
634700960122     C                   KFLD                    KANN
634800960122     C                   KFLD                    KMES
634900000202     C     KFGV1         KLIST
635000940909     C                   KFLD                    KNPG
635100000202     C                   KFLD                    WNFV
635200940909     C                   KFLD                    KFGS
635300940909     C                   KFLD                    COMLNA
635400951025     C     KFGV2         KLIST
635500951025     C                   KFLD                    SIMFEL
635600951025     C                   KFLD                    AMGDFV
635700130606     C     KFGV02        KLIST
635800130606     C                   KFLD                    fgvlnp
635900130606     C                   KFLD                    fgvdfv
636000940909     C     KBLP1         KLIST
636100020805     c                   kfld                    SIMFEL
636200940909     C                   KFLD                    VIDNFV
636300020805     C                   KFLD                    COMLNA
636400020805     c     K02BLP79      KLIST
636500020805     c                   kfld                    SIMFEL
636600020805     c                   kfld                    VIDNFV
636700020805     c     K02FGV01      KLIST
636800020805     c                   kfld                    VIDNFV
636900020805     c                   kfld                    SIMFEL
637000020805     c     K02FGW01      KLIST
637100020805     c                   kfld                    FGWnfv
637200020805     c                   kfld                    FGWlnp
637300951024     C     KFGV46        KLIST
637400940909     C                   KFLD                    CODFIL
637500940909     C                   KFLD                    VIDNFV
637600940912     C     KBRV1         KLIST
637700940912     C                   KFLD                    KNPG
637800000202     C                   KFLD                    WNFV
637900941229     C                   KFLD                    KFGS
638000950901     C     KTAB          KLIST
638100950901     C                   KFLD                    CODUT
638200950901     C                   KFLD                    COD
638300950901     C                   KFLD                    KEY
638400951023     C     KTLR          KLIST
638500951025     C                   KFLD                    VIDTRN
638600951016     C                   KFLD                    WTFM
638700951016     C                   KFLD                    WFIL
638800951024     C     KTLR1         KLIST
638900951025     C                   KFLD                    VIDTRN
639000951024     C                   KFLD                    WDDE
639100951024     C                   KFLD                    WNFM
639200951024     C     KTLR11        KLIST
639300951025     C                   KFLD                    VIDTRN
639400951024     C                   KFLD                    WDDE
639500951024     C     KTLL1         KLIST
639600951025     C                   KFLD                    VIDTRN
639700951024     C                   KFLD                    WDDE
639800951024     C                   KFLD                    KTMZ
639900951024     C                   KFLD                    KPDR
640000951024     C     KTLR3         KLIST
640100951025     C                   KFLD                    VIDTRN
640200951024     C                   KFLD                    WDDE
640300951024     C                   KFLD                    WTFM
640400951024     C                   KFLD                    WFIL
640500951024     C     KTLM          KLIST
640600951025     C                   KFLD                    VIDTRN
640700951024     C                   KFLD                    WDDE
640800951024     C                   KFLD                    WTFP
640900951024     C                   KFLD                    WTFA
641000960122     C     KTLM1         KLIST
641100960122     C                   KFLD                    VIDTRN
641200960122     C                   KFLD                    WDDE
641300960122     C                   KFLD                    WTFP
641400110511     C     KTLs          KLIST
641500110511     C                   KFLD                    VIDTRN
641600110511     C                   KFLD                    WDDE
641700110511     C                   KFLD                    gioset
641800950901     C     KCAE          KLIST
641900960123     C                   KFLD                    KEPA
642000951025     C                   KFLD                    WTFP
642100951025     C                   KFLD                    WFIL
642200960226     C     KCAE2         KLIST
642300960226     C                   KFLD                    KEPA
642400960226     C                   KFLD                    KTFA
642500951031     C     KFV4          KLIST
642600951031     C                   KFLD                    SIMFEL
642700951031     C                   KFLD                    VIDNFV
642800960123     C                   KFLD                    KEPA
642900960123     C                   KFLD                    KTFA
643000960123     C                   KFLD                    KTRC
643100951103     C     KFV2          KLIST
643200951103     C                   KFLD                    SIMFEL
643300951103     C                   KFLD                    VIDNFV
643400960123     C                   KFLD                    KEPA
643500960122     C                   KFLD                    ZERO
643600960122     C                   KFLD                    KTDH
643700960122     C     KFV22         KLIST
643800960122     C                   KFLD                    FGVLNP
643900960122     C                   KFLD                    FGVNFV
644000960129     C                   KFLD                    KEPA
644100960122     C                   KFLD                    WTFA
644200960122     C                   KFLD                    KTDH
644300130606     C     KFV22A        KLIST
644400130606     C                   KFLD                    F2VLNP
644500130606     C                   KFLD                    F2VNFV
644600130606     C                   KFLD                    KEPA
644700130606     C                   KFLD                    WTFA
644800130606     C                   KFLD                    KTDH
644900960122     C     KFV23         KLIST
645000960122     C                   KFLD                    FGVLNP
645100960122     C                   KFLD                    FGVNFV
645200960129     C                   KFLD                    KEPA
645300960129     C     KFV24         KLIST
645400960129     C                   KFLD                    FGVLNP
645500960129     C                   KFLD                    FGVNFV
645600960129     C                   KFLD                    KEPA
645700960129     C                   KFLD                    KTDH
645800000807     C     KTNTLZ        KLIST
645900020805     C                   KFLD                    KTLTIP
646000000807     C                   KFLD                    KTLPDR
646100080910     c     kfvs          KLIST
646200080910     c                   kfld                    fgvlnp
646300080910     c                   kfld                    fgvnfv
646400080910     c     ktst          KLIST
646500080910     c                   kfld                    tsrtrn
646600080910     c                   kfld                    tsrdtn
646700130424     c     kata          KLIST
646800130424     c                   kfld                    soctraini
646900130424     c                   kfld                    kpdr
647000130424     c                   kfld                    kdcn
647100130424     c     katm          KLIST
647200130424     c                   kfld                    soctraini
647300130424     c                   kfld                    kpdr
647400130424     c                   kfld                    vidtrm
647500130605
647600130605     c     kfge          klist
647700130605     c                   kfld                    fgvdfv
647800130605     c                   kfld                    fgvlnp
647900130605     c                   kfld                    wlna
648000940909     C*
648100951009     C****  IMPOSTO CAMPI FISSI  ****
648200950905     C                   SETOFF                                       01
648300130417     C                   MOVEL     'FNLST4R '    VIDPGM
648400951026     C***
648500951026     C     *ENTRY        PLIST
648600951026     C                   PARM                    KPJBA
648700130315     C                   MOVEL     KPJBU         fnlst4ds
648800050706     c                   movel     knmus         iniziopro
648900130429     C                   SETON                                        20
649000130502     C                   CLEAR                   olst4msg
649100130502     C                   CLEAR                   OLST4F12
649200130429
649300130429
649400951026     C                   SELECT
649500130315     C     ilst4ges      WHENEQ    ' '
649600951026     C                   MOVE      TES(1)        VIDTES
649700951026     C*
649800130429     C* CHIUSURA FOGLIO VIAGGIO SINGOLA
649900130315     C     ilst4ges      WHENEQ    'C'
650000130502     C     ilst4ges      oreq      'I'
650100130426     C                   MOVE      TES(2)        VIDTES
650200951027     C                   SETON                                        01
650300130429
650400951027     C* MANUTENZIONE FV RICHIAMATA
650500130315     C     ilst4ges      WHENEQ    'M'
650600130315     C     ilst4ges      oreq      'A'
650700130419     C     ilst4ges      oreq      'P'
650800951027     C                   MOVE      TES(1)        VIDTES
650900960123     C* INTERROGAZIONE FV SINGOLA
651000130315     C     ilst4ges      WHENEQ    'N'
651100130430     C                   SETON                                        01
651200960123     C                   SETON                                        9899
651300960123     C                   MOVE      TES(1)        VIDTES
651400960123     C*
651500951027     C                   OTHER
651600130430     C                   SETON                                        01
651700951026     C* VENGO DA BORDERO'
651800130315     C     ilst4ges      IFEQ      'B'
651900951026     C                   SETON                                        81
652000951026     C                   MOVE      TES(3)        VIDTES
652100951026     C                   ENDIF
652200951026     C*
652300951026     C                   ENDSL
652400961213     C*
652500130315     C     ilst4ges      IFNE      'N'
652600130510     c                   if        not %open(fnfgv02l)
652700961213     C                   OPEN      FNFGV02L
652800130510     c                   endif
652900130510     c                   if        not %open(fnfgv06l)
653000020805     C                   OPEN      FNFGV06L
653100130510     C                   endif
653200130510     C                   END
653300080909      *
653400080919     c* In manutenzione apro files che mi serviranno nel controllo del
653500080909     c* numero foglio
653600130315     c                   if        ilst4ges=' '  or ilst4ges='A' or
653700130419     c                             ilst4ges='M'  or ilst4ges='P'
653800080919     c                   Open(e)   TNFVS01L
653900080919     c                   if        not %open(tnfvs01l)
654000080919     c                   eval      %subst(Wfvs01:7:4)='GRPS'
654100080919     c                   Open      TNFVS01L
654200080919     c                   endif
654300080919     c                   Open(e)   tntsr04l
654400080919     c                   if        not %open(tntsr04l)
654500080919     c                   eval      %subst(Wtsr04:7:4)='GRPS'
654600080919     c                   Open      TNtsr04l
654700080919     c                   endif
654800080919     c                   Open(e)   tntst01l
654900080919     c                   if        not %open(tntst01l)
655000080919     c                   eval      %subst(Wtst01:7:4)='GRPS'
655100080919     c                   Open      TNtst01l
655200080919     c                   endif
655300080909     c                   endif
655400951026     C     *IN01         IFEQ      *OFF
655500951026     C* OPEN DI ALCUNI FILE
655600130510     c                   if        not %open(fnfgv46l)
655700951027     C                   OPEN      FNFGV46L
655800130510     c                   endif
655900130510     c                   if        not %open(azcae03l)
656000951026     C                   OPEN      AZCAE03L
656100130510     c                   endif
656200951026     C                   ENDIF
656300951026     C*
656400951023     C                   CALL      'X§PARUT'
656500020805     C                   PARM                    UT§DSE0F
656600020805     C                   MOVEL     REC80         CNCR80
656700951023     C                   MOVEL     RAGUT         VIDRSU
656800951026     C                   MOVEL     RAGUT         PA2RSU
656900951026     C                   Z-ADD     SIMFEL        PA2FEL
657000951023      *
657100030701      * Questo controllo è stato sostituito
657200030701      * ora è - 1 e + 1 dalla data odierna
657300030701      * Ho spostato il giro xchè ora il controllo lo devo fare anche in
657400030701      * manutenzione, -1 + 1 dalla data foglio (se è modificabile)
657500951023     C*
657600951023     C* LA DATA DEL FOGLIO VIAGGIO NON DEVE ANDARE OLTRE ALLA DATA
657700951023     C* CORRENTE MENO 1 GG. E ALLA DATA CORRENTE PIU' 3 GG.
657800951023     C*
657900951023     C* DEVO USARE TIME AL POSTO DI UDATE, POICHE' UDATE NON VIENE
658000951023     C* AGGIORNATA, DOPO LA MEZZANOTTE, SE IL JOB E' GIA' APERTO
658100951023     C*
658200951023     C                   TIME                    WTIME            14 0
658300951023     C                   MOVE      WTIME         WDATE
658400951023     C                   MOVE      WDATE         G02DAT
658500951023     C                   MOVEL     *ZERO         G02INV
658600951023     C                   MOVEL     *BLANKS       G02ERR
658700951023     C                   CALL      'XSRDA8'
658800951023     C                   PARM                    WLBDAT
658900951106     C                   MOVEL     G02INV        DATEU
659000951026     C*
659100130419     C* SE VENGO DA CHIUSURA FV  CARICO LA L1 E VEDO SE SIMFEL HA VDL
659200960126     C                   Z-ADD     SIMFEL        CODFIL
659300960126     C                   EXSR      CTRFIL
659400130419     C  N34              MOVEL     ORGDES        DESfgs
659500991020     C   34              MOVEL     *ZEROS        ORGDF0
659600960209     C   34              MOVEL     *BLANKS       ORGDE8
659700040121     c                   eval      og150=orgdf0
659800951026     C*
659900040121     C                   TESTN                   §ogvol               83
660000951026     C     *IN83         IFEQ      '1'
660100040121     C     §ogvol        ANDGT     *ZEROS
660200960209     C                   MOVEL     'S'           PA2CML            1
660300120914     C                   MOVEL     'S'           PA2spv
660400951026     C                   END
660500991020     C*
660600040121     C                   TESTN                   §ogpes               83
660700991020     C     *IN83         IFEQ      '1'
660800040121     C     §ogpes        ANDGT     *ZEROS
660900991020     C                   MOVEL     'P'           PA2CML
661000120914     C                   MOVEL     'S'           PA2spp
661100991020     C                   END
661200040121     c*
661300960209     C* DALLA DESCRIZIONE 148 VEDO QUALE GENERAZIONE SPUNTE DEVO FARE
661400960209     C*  IN CHIUSURA FV
661500960209     C                   MOVEL     ORGDE8        OG148
661600960209     C     §OGAGS        IFEQ      'E'
661700960209     C                   MOVEL     'S'           PA2AGS
661800010911     C                   MOVEL     §OGAGP        PA2AGP
661900960209     C                   ELSE
662000960209     C                   CLEAR                   PA2AGS
662100010911     C                   CLEAR                   PA2AGP
662200960209     C                   ENDIF
662300960209     C**
662400951027     C* CARICO L1 --> FILIALI GESTITE
662500020805     C                   CLEAR                   TRUL06DS
662600951026     C                   MOVE      '£1'          D06COD
662700951026     C                   MOVEL     SIMFEL        D06KEY
662800020805     C                   MOVEL     TRUL06DS      KPJBU
662900951026     C*
663000951026     C                   CALL      'TRUL06R'
663100951026     C                   PARM                    KPJBA
663200020805     C                   MOVEL     KPJBU         TRUL06DS
663300951026     C                   MOVEA     LIN           L1
663400951106     C* VADO SULLE TABELLE PER VEDERE SE E' UN TIPO RECORD DA
663500951106     C*  TRASMETTERE
663600960123     C                   MOVEL     '3B'          COD
663700960123     C                   MOVEL(P)  'N'           KEY
663800951106     C     KTAB          CHAIN     TABEL                              83
663900951106     C     *IN83         IFEQ      *OFF
664000951106     C                   MOVEL     TBLUNI        DS3B
664100960126     C                   MOVEL     §3BTRS        WTRSN2
664200960126     C                   MOVEL     §3BTRF        WTRSN1
664300951106     C                   ENDIF
664400960302     C*
664500960302     C                   MOVEL(P)  'C'           KEY
664600960302     C     KTAB          CHAIN     TABEL                              83
664700960302     C     *IN83         IFEQ      *OFF
664800960302     C                   MOVEL     TBLUNI        DS3B
664900960302     C                   MOVEL     §3BTRS        WTRSC2
665000960302     C                   MOVEL     §3BTRF        WTRSC1
665100960302     C                   ENDIF
665200960302     C*
665300960302     C                   MOVEL(P)  'D'           KEY
665400960302     C     KTAB          CHAIN     TABEL                              83
665500960302     C     *IN83         IFEQ      *OFF
665600960302     C                   MOVEL     TBLUNI        DS3B
665700960302     C                   MOVEL     §3BTRS        WTRSD2
665800960302     C                   MOVEL     §3BTRF        WTRSD1
665900960302     C                   ENDIF
666000951009     C*
666100010621     C                   MOVEL(P)  'L'           KEY
666200010621     C     KTAB          CHAIN     TABEL                              83
666300010621     C     *IN83         IFEQ      *OFF
666400010621     C                   MOVEL     TBLUNI        DS3B
666500010621     C                   MOVEL     §3BTRS        WTRSL2
666600010621     C                   MOVEL     §3BTRF        WTRSL1
666700010621     C                   ENDIF
666800130424     c*
666900130424     C                   MOVEL(P)  'P'           KEY
667000130424     C     KTAB          CHAIN     TABEL                              83
667100130424     C     *IN83         IFEQ      *OFF
667200130424     C                   MOVEL     TBLUNI        DS3B
667300130424     C                   MOVEL     §3BTRS        WTRSP2
667400130424     C                   MOVEL     §3BTRF        WTRSP1
667500130424     C                   ENDIF
667600010927      *
667700010927      * RECUPERO LA MONETA DI CONTO
667800010927     C                   CLEAR                   DGED
667900020805     C                   CLEAR                   TIBS02DS
668000010927     C                   MOVEL     'L'           T02TLA
668100010927     C                   MOVEL     'C'           T02MOD
668200010927     C                   MOVEL     KNSIF         T02SIF
668300010927     C                   MOVEL     'GED'         T02COD
668400010927     C                   MOVEL     '1'           T02KE1
668500010927     C                   CALL      'TIBS02R'
668600010927     C                   PARM                    KPJBA
668700020805     C                   PARM                    TIBS02DS
668800010927     C                   MOVEL     T02UNI        DGED
668900130424     c
669000130424     c* Reperisco società dei trazionisti
669100130424     C                   MOVEL     '3I'          COD
669200130424     C                   MOVEL(P)  '1'           KEY
669300130424     C     KTAB          CHAIN     TABEL                              83
669400130424     C     *IN83         IFEQ      *OFF
669500130424     c                   movel     tbluni        ds3i
669600130424     c                   eval      alfasoc=%subst(§3ibst:7:3)
669700130424     c                   if        alfasoc>*zeros
669800130424     c                   EVAL      soctraini=%int(alfasoc)
669900130424     c                   endif
670000130424     c                   endif
670100130510     c*
670200130510     c* Reperisco tabella RMA
670300130510     c                   Clear                   tibs02ds
670400130510     c                   Clear                   dmradan
670500130510     c                   Movel     'C'           T02Mod
670600130510     c                   Movel     KNSIF         T02Sif
670700130510     c                   Movel     'MRA'         T02Cod
670800130510     c                   Movel     'FNLST4R'     T02Ke1
670900130510     c                   Call      'TIBS02R'
671000130510     c                   Parm                    Kpjba
671100130510     c                   Parm                    tibs02ds
671200130510    2c                   If        T02Err = *Blanks
671300130510     c                   Movel     T02Uni        Dmradan
671400130510    2c                   endif
671500130510     C*
671600010927      *
671700940909     C                   ENDSR
671800951024     C**************************************************************************
671900951024     C*  ABBLANCA ZERO NON SIGNIFICATIVI
672000951024     C**************************************************************************
672100951024     C     ABBL          BEGSR
672200951024     C*
672300951026     C     K15(Z)        DOWEQ     '0'
672400951026     C     Z             ANDLT     15
672500951026     C                   MOVEL     ' '           K15(Z)
672600951026     C                   ADD       1             Z                 3 0
672700951024     C                   ENDDO
672800951024     C*
672900951024     C                   ENDSR
673000951025     C**************************************************************************
673100951025     C*  CONTROLLO E DEOCIFICO ESISTENZA TIPO AUTOMEZZO
673200951025     C**************************************************************************
673300951025     C     CTRTMZ        BEGSR
673400951025     C                   MOVEL     'TU'          COD
673500951025     C                   MOVEL(P)  VIDTMZ        KEY
673600951025     C     KTAB          CHAIN     TABEL                              83
673700951025    1C  N83TBLFLG        IFEQ      ' '
673800951025     C                   MOVEL     TBLUNI        DESTMZ
673900130502     C                   MOVEL     TBLUNI        DESTMZ2
674000951025     C                   ELSE
674100951025     C                   SETON                                        83
674200951025     C                   CLEAR                   DESTMZ
674300130502     C                   CLEAR                   DESTMZ2
674400951025    1C                   ENDIF
674500951025     C                   ENDSR
674600951025     C**************************************************************************
674700951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
674800951025     C**************************************************************************
674900951025     C     CTRFIL        BEGSR
675000020213
675100020213     c                   clear                   esteroedpd
675200020213
675300951025     C     CODFIL        CHAIN     AZORG01L                           34
675400951025    1C  N34ORGFVA        IFNE      ' '
675500951025     C     ORGFAG        ORNE      'A'
675600951025     C     ORGFAG        ANDNE     'F'
675700951025     C                   SETON                                        34
675800951025    1C                   ENDIF
675900000405     C  N34              MOVEL     ORGDE3        OG143
676000020213
676100020213      * Network esteri + DPD
676200020213     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
676300020213     c                             §ogntw = 'FED' or §ogntw = 'DPD'
676400020213     c                   eval      esteroedpd = 'S'
676500020213     c                   endif
676600951025     C*
676700951025     C                   ENDSR
676800951027     C**************************************************************************
676900951027     C*  CONTROLLA DATA FV E CALCOLO DFV - 1 E DFV + 5
677000951027     C**************************************************************************
677100951027     C     CALDAT        BEGSR
677200951027     C                   MOVE      VIDDFV        G02DAT
677300951027     C                   MOVEL     *BLANK        G02ERR
677400951027     C                   CALL      'XSRDA8'
677500951027     C                   PARM                    WLBDAT
677600951027     C* ERRORE
677700960122    1C     G02ERR        IFEQ      '1'
677800951027     C                   MOVEL     MSG(4)        VIDMSG
677900960122     C                   SETON                                        442890
678000960122   X1C                   ELSE
678100951027     C* NON C'E' STATO ERRORE
678200960122    2C     G02INV        IFNE      AMGDFV
678300951130     C                   CLEAR                   WFORZ9
678400960122     C                   CLEAR                   WFORZB
678500960122    2C                   ENDIF
678600951027     C                   Z-ADD     G02INV        AMGDFV
678700951130     C                   Z-ADD     G02DAT        VIDDFV
678800960122     C                   Z-ADD     G02DAT        KANN
678900960122     C                   Z-ADD     G02DAT        WDATA
679000960122     C                   MOVEL     MM            KMES
679100030630      * non può essere minore o maggiore di 1 gg. lavorativo
679200030630     c                   If        AmgDfv < wdtameno1  or
679300030630     c                             AmgDfv > wdtapiu1
679400030701     c                   Eval      VidMsg = Msg(7)
679500030630     c                   Eval      *in28 = *On
679600030630     c                   Eval      *in44 = *On
679700030630     c                   Eval      *in90 = *On
679800030630     c                   EndIf
679900960122     C* SOLO IN IMMISSIONE --> SE DATA DFV CADE IN GIORNO FESTIVO PER
680000960122     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
680100040204    2C  N90*IN80         IFEQ      *ON
680200040204     C     wd02          IFEQ      'S'
680300960123     C                   CLEAR                   KTFA
680400960123     C                   Z-ADD     68            KTFP
680500960122     C     KCLN1         CHAIN     AZCLN01L                           83
680600960122    3C  N83POM(GG)       IFNE      *BLANKS
680700960122     C                   MOVEL     '1Y'          COD
680800960122     C                   MOVEL(P)  POM(GG)       KEY
680900960122     C     KTAB          CHAIN     TABEL                              83
681000960122     C                   MOVEL     TBLUNI        DS1Y
681100960122     C*
681200960122    4C  N83§1YFET        IFNE      *BLANKS
681300960122     C     WFORZB        ANDEQ     *BLANKS
681400960122     C                   MOVEL     MSG(63)       VIDMSG
681500960122     C                   SETON                                        442890
681600960123     C                   MOVEL     '1'           WFORZB            1
681700960122    4C                   ENDIF
681800960122    3C                   ENDIF
681900040204     c                   endif
682000970526   X2C                   ELSE
682100970526     C* IN MANUTENZIONE
682200970526     C* NON POSSO MODIFICARLA SE HA SPUNTE
682300970526     C  N80AMGDFV        IFNE      FGVDFV
682400000202     C                   Z-ADD     VIDNFV        WNFV
682500070112     C     KBRV1         SETLL     FNBRV02L                               83
682600970526     C   83              MOVEL     MSG(72)       VIDMSG
682700970526     C   83              SETON                                        442890
682800960122    2C                   ENDIF
682900970526    2C                   ENDIF
683000960122     C**
683100960122    2C     *IN90         IFEQ      *OFF
683200951027     C                   Z-ADD     G02TGI        WGGDFV
683300951027     C* DFV -1
683400951027     C     G02TGI        SUB       1             GI8TGI
683500951027     C*
683600951027     C                   CALL      'XSRGI8'
683700951027     C                   PARM                    WGIDAT
683800951027     C                   MOVEL     GI8INV        DTPMIN            8 0
683900951027     C* DFV + 5
684000951027     C     G02TGI        ADD       5             GI8TGI
684100951027     C*
684200951027     C                   CALL      'XSRGI8'
684300951027     C                   PARM                    WGIDAT
684400951027     C                   MOVEL     GI8INV        DTPMAX            8 0
684500960122    2C                   ENDIF
684600960122    1C                   ENDIF
684700100616
684800100616     c* Determino data foglio - 1 gg lavorativo
684900100616     c                   Clear                   Ktfp
685000100616     c                   Z-Add     VidFgs        Ktfa
685100100616     c     *eur          Move      Viddfv        wdtaeur
685200100616      * meno 1 gg. lavorativo
685300100616     c                   Do        *Hival
685400100616     c                   Subdur    1:*d          wdtaeur
685500100616     c                   extrct    wdtaeur:*d    wgiorno
685600100616     c                   extrct    wdtaeur:*m    kmes
685700100616     c                   extrct    wdtaeur:*y    kann
685800100616     c     kcln1         Chain     Azcln01l
685900100616     c                   If        Not %Found(azcln01l)
686000100616     c                   Leave
686100100616     c                   EndIf
686200100616     c                   If        POM(wgiorno) = 'F'
686300100616     c                   Iter
686400100616     c                   EndIf
686500100616     c                   Leave
686600100616     c                   EndDo
686700100616     c
686800100616     c                   Move      wdtaeur       wdtaiso
686900100616     c                   Move      wdtaiso       wDEFmeno1
687000951027     C*
687100951027     C                   ENDSR
687200030630
687300030630      **************************************************************************
687400030630      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
687500030630      **************************************************************************
687600030630     c     Sr_MinMax     BegSr
687700030701
687800030630     c                   Clear                   Ktfa
687900030630     c                   Z-Add     VidFgs        Ktfp
688000030701     c     *eur          Move      Viddfv        wdtaeur
688100030630      * meno 1 gg. lavorativo
688200030630     c                   Do        *Hival
688300030701     c                   Subdur    1:*d          wdtaeur
688400030701     c                   extrct    wdtaeur:*d    wgiorno
688500030701     c                   extrct    wdtaeur:*m    kmes
688600030701     c                   extrct    wdtaeur:*y    kann
688700030630     c     kcln1         Chain     Azcln01l
688800030630     c                   If        Not %Found(azcln01l)
688900030630     c                   Leave
689000030630     c                   EndIf
689100030630     c                   If        POM(wgiorno) = 'F'
689200030630     c                   Iter
689300030630     c                   EndIf
689400030630     c                   Leave
689500030630     c                   EndDo
689600030701     c                   Move      wdtaeur       wdtaiso
689700030701     c                   Move      wdtaiso       wdtameno1
689800030630
689900050706      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
690000050706     c*  con data foglio-1)
690100050706     c                   Do        *Hival
690200050706     c                   Subdur    1:*d          wdtaeur
690300050706     c                   extrct    wdtaeur:*d    wgiorno
690400050706     c                   extrct    wdtaeur:*m    kmes
690500050706     c                   extrct    wdtaeur:*y    kann
690600050706     c     kcln1         Chain     Azcln01l
690700050706     c                   If        Not %Found(azcln01l)
690800050706     c                   Leave
690900050706     c                   EndIf
691000050706     c                   If        POM(wgiorno) = 'F'
691100050706     c                   Iter
691200050706     c                   EndIf
691300050706     c                   Leave
691400050706     c                   EndDo
691500050706     c                   Move      wdtaeur       wdtaiso
691600050706     c                   Move      wdtaiso       wdtameno2
691700030630      * più 1 gg. lavorativo
691800030701     c     *eur          Move      Viddfv        wdtaeur
691900030630     c                   Do        *Hival
692000030701     c                   Adddur    1:*d          wdtaeur
692100030701     c                   extrct    wdtaeur:*d    wgiorno
692200030701     c                   extrct    wdtaeur:*m    kmes
692300030701     c                   extrct    wdtaeur:*y    kann
692400030630     c     kcln1         Chain     Azcln01l
692500030630     c                   If        Not %Found(azcln01l)
692600030630     c                   Leave
692700030630     c                   EndIf
692800030630     c                   If        POM(wgiorno) = 'F'
692900030630     c                   Iter
693000030630     c                   EndIf
693100030630     c                   Leave
693200030630     c                   EndDo
693300030701     c                   Move      wdtaeur       wdtaiso
693400030701     c                   Move      wdtaiso       wdtapiu1
693500030630
693600030630     c                   EndSr
693700030630
693800951026     C**************************************************************************
693900951026     C*  SOTTOMETTO PGM DI CHIUSURA FV
694000951026     C**************************************************************************
694100951026     C     CHIUFV        BEGSR
694200951026     C**
694300951026     C** P R E P A R A Z I O N E   V O L U M I
694400951026     C**
694500120914     C     PA2CML        IFNE      ' '
694600120914     c***  pa2spv        ifne      *blanks
694700120914     c***  pa2spp        orne      *blanks
694800951026     C                   MOVEL     PARAM2        KPJBU
694900951026     C                   MOVEL     'LSB7'        KCOAZ
695000951026     C                   CALL      'BCH10'
695100951026     C                   PARM                    KPJBA
695200951026     C                   END
695300951026     C**
695400951026     C** C H I U S U R A   F O G L I   V I A G G I O
695500951026     C**
695600951026     C                   MOVEL     VIDNFV        PA2FGV
695700130603     c                   movel     'S'           pa2barcode
695800080228     C* CARICO L1 --> FILIALI GESTITE
695900080228     C                   CLEAR                   TRUL06DS
696000080228     C                   MOVE      '£1'          D06COD
696100080228     C                   MOVEL     SIMFEL        D06KEY
696200080228     C                   MOVE      'S'           D06KEY
696300080228     C                   MOVEL     fgvdfv        D06drf
696400080228     C                   MOVEL     TRUL06DS      KPJBU
696500080228     C*
696600080228     C                   CALL      'TRUL06R'
696700080228     C                   PARM                    KPJBA
696800080228     C                   MOVEL     KPJBU         TRUL06DS
696900080228     C                   MOVEA     LIN           L1ela
697000080228     c
697100951026     C                   MOVEL     PARAM2        KPJBU
697200080228     c
697300000824     C                   CLEAR                   KQEBI
697400000824     C                   CLEAR                   KQIEB
697500131105     c****               if        ilst4NEWC=' '
697600131105     C****               CALL      'BCH10'
697700131105     C****               PARM                    KPJBA
697800131105     c****               else
697900130603     C                   MOVEL     'LSU2'        KCOAZ
698000130712     C                   CALL      'BCH10'
698100130603     C                   PARM                    KPJBA
698200131105     c****               endif
698300951026     C*
698400951026     C* PREPARAZIONE FILE LEGAMI BOLLE
698500000824     C                   CLEAR                   KQEBI
698600000824     C                   CLEAR                   KQIEB
698700951026     C*
698800951026     C* LANCIO PGM DI AGGIORNAMENTO BOLLE
698900951026     C                   CLEAR                   KPJBU
699000951026     C*
699100951026     C* INVIO FILE TRASMISSIONE
699200951026     C                   MOVEL     'LSA3'        KCOAZ
699300070423     c                   clear                   parama3
699400070423     c                   move      simfel        pa3tfp
699500070423     c                   movel(p)  parama3       kpjbu
699600951026     C                   CALL      'BCH10'
699700951026     C                   PARM                    KPJBA
699800000824     C**
699900000824     C                   CLEAR                   KQEBI
700000000824     C                   CLEAR                   KQIEB
700100951026     C                   ENDSR
700200951130     C**************************************************************************
700300951130     C* CONTROLLO FV SE E' DA MANDARE IN SEDE
700400951130     C**************************************************************************
700500951130     C     CTRFVA        BEGSR
700600951130     C* SCRIVO FILE DI APPOGGIO DEL FOGLIO VIAGGIO SE:
700700951130     C*   - IL FOGLIO NON E' ABBINATO
700800951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
700900951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
701000951130     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTS = "S")
701100951130     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
701200951130     C*     C'E' IL RECORD IN TABELLA
701300951130    1C                   SELECT
701400951130     C     FGVTRN        WHENEQ    0
701500951130     C                   EXSR      WRTFVA
701600951130   X1C                   OTHER
701700951130     C*
701800951130     C                   MOVEL     'TV'          COD
701900951130     C                   MOVEL(P)  FGVTTR        KEY
702000951130     C     KTAB          CHAIN     TABEL                              83
702100951130    2C     *IN83         IFEQ      *OFF
702200951130     C     TBLFLG        ANDEQ     ' '
702300951130     C                   MOVEL     TBLUNI        DSTV
702400951130    2C     §TVUTC        IFEQ      'S'
702500951130     C                   EXSR      WRTFVA
702600951130    3C                   ENDIF
702700951130   X2C                   ELSE
702800951130     C                   EXSR      WRTFVA
702900951130    2C                   ENDIF
703000951130    1C                   ENDSL
703100951130     C                   ENDSR
703200970102     C**************************************************************************
703300970102     C* CONTROLLO FV DEFLUENZA PER ESTERO
703400970102     C**************************************************************************
703500970102     C     CTRDEF        BEGSR
703600970102     C* NON DEVE ESISTERE UN ALTRO FV DI DEFL. PER LA STESSA LNA ANCORA
703700970102     C* APERTO O GIA' CHIUSO MA CON LA STESSA DATA. POSSO PERO' ESISTER
703800970102     C*  COME FOGLI BIS
703900970102    1C     §TVDEF        IFEQ      'S'
704000060113     C     WEST          ANDEQ     'S'
704100970102     C     *IN56         ANDEQ     *OFF
704200970102     C     VIDBIS        ANDEQ     ' '
704300970102     C*
704400970102     C                   CLEAR                   WERR
704500970102     C                   CLEAR                   WAPE
704600970102     C* MI SALVO CAMPI DELLA DSTV
704700970102     C                   MOVEL     DSTV          DSTVS
704800970102     C* I° CICLO DI LETTURA:VERIFICO ESISTENZA DI ALTRO FV CON DATA =
704900970102     C     KFGV2         SETLL     FNFGV02L
705000970102     C     KFGV2         READE     FNFGV02L                               32
705100970102     C*
705200970102    2C     *IN32         DOWEQ     *OFF
705300970102     C     WERR          ANDEQ     *BLANKS
705400970102     C*
705500970102    3C     F2VNFV        IFNE      VIDNFV
705600970102     C     F2VSNP        ANDEQ     *BLANKS
705700970102     C     F2VATB        ANDEQ     ' '
705800000420     C*
705900020805     c                   eval      FGWnfv = F2Vnfv
706000020805     c                   eval      FGWlnp = F2Vlnp
706100020805     c                   exsr      CTR_FGW
706200000427     C                   MOVEL     FGWFF3        W2FF3
706300000427     C                   MOVEL     FGWFF4        W2FF4
706400970102     C* CONTROLLO SE ALMENO 1 LINEA DEL FOGLIO IN GESTIONE
706500970102     C*  E' PRESENTE SUL FV APPENA LETTO
706600970102     C                   Z-ADD     1             I
706700970102     C                   SETOFF                                       31
706800970102    4C     *IN31         DOWEQ     *OFF
706900000420     C     I             ANDLE     300
707000970102     C     CTL(I)        ANDGT     *ZEROS
707100000405     C                   MOVEL     CTL(I)        CODFIL
707200000405     C                   EXSR      CTRFIL
707300061027     c**                 if        §ogntw <> 'DPD'
707400970102     C     CTL(I)        LOOKUP    F2V                                    31
707500061027     C**                 ENDIF
707600970102     C  N31              ADD       1             I
707700970102    4C                   ENDDO
707800970102     C*
707900970102    4C     *IN31         IFEQ      *ON
708000970102     C                   MOVEL     'TV'          COD
708100970102     C                   MOVEL(P)  F2VTTR        KEY
708200970102     C     KTAB          CHAIN     TABEL                              32
708300970102     C  N32              MOVEL     TBLUNI        DSTV
708400970102     C   32              CLEAR                   DSTV
708500970102     C* SE DEFLUENZA E NON BIS --> ESCO DAL CICLO E SEGNALO ERRORE
708600970102    5C     §TVDEF        IFEQ      'S'
708700970102     C                   MOVE      '1'           WERR              1
708800970102     C                   MOVEL     CTL(I)        W003A
708900970102    5C                   END
709000970102    4C                   END
709100970102    3C                   END
709200970102     C*
709300970102    3C     WERR          IFEQ      *BLANKS
709400970102     C     KFGV2         READE     FNFGV02L                               32
709500970102    3C                   END
709600970102    2C                   ENDDO
709700970102     C*
709800970102    2C     WERR          IFEQ      *BLANKS
709900970102     C* II° CICLO DI LETTURA: VERIFICO ESISTENZA FV ANCORA APERTO
710000970102     C* (CON DATA DIVERSA, QUELLI CON LA STESSA DATA LI HO GIA' CERCATI
710100970102     C* NEL GIRO PRECEDENTE)
710200020805     C     SIMFEL        SETLL     FNFGV06L
710300020805     C     SIMFEL        READE     FNFGV06L                               32
710400970102     C*
710500970102    3C     *IN32         DOWEQ     *OFF
710600970102     C     WAPE          ANDEQ     *BLANKS
710700970102     C*
710800020805    4C     F6VDFV        IFNE      AMGDFV
710900020805     C     F6VNFV        ANDNE     VIDNFV
711000020805     C     F6VSNP        ANDEQ     ' '
711100000420     C*
711200020805     c                   eval      FGWnfv = F6Vnfv
711300020805     c                   eval      FGWlnp = F6Vlnp
711400020805     c                   exsr      CTR_FGW
711500020805     C                   MOVEL     FGWFF3        W6FF3
711600020805     C                   MOVEL     FGWFF4        W6FF4
711700970102     C* CONTROLLO SE ALMENO 1 LINEA DEL FOGLIO IN GESTIONE
711800970102     C*  E' PRESENTE SUL FV APPENA LETTO
711900970102     C                   Z-ADD     1             I
712000970102     C                   SETOFF                                       31
712100970102    5C     *IN31         DOWEQ     *OFF
712200000420     C     I             ANDLE     300
712300970102     C     CTL(I)        ANDGT     *ZEROS
712400000405     C                   MOVEL     CTL(I)        CODFIL
712500000405     C                   EXSR      CTRFIL
712600020805     C     CTL(I)        LOOKUP    F6V                                    31
712700970102     C  N31              ADD       1             I
712800970102    5C                   ENDDO
712900970102     C*
713000970102    5C     *IN31         IFEQ      *ON
713100970102     C                   MOVEL     'TV'          COD
713200020805     C                   MOVEL(P)  F6VTTR        KEY
713300970102     C     KTAB          CHAIN     TABEL                              32
713400970102     C  N32              MOVEL     TBLUNI        DSTV
713500970102     C   32              CLEAR                   DSTV
713600970102     C* SE DEFLUENZA ESCO DAL CICLO E SEGNALO ERRORE
713700970102    6C     §TVDEF        IFEQ      'S'
713800970102     C                   MOVE      '1'           WAPE              1
713900970102     C                   MOVEL     CTL(I)        W003A
714000970102    6C                   END
714100970102    5C                   END
714200970102    4C                   END
714300970102     C*
714400970102    4C     WAPE          IFEQ      *BLANKS
714500020805     C     SIMFEL        READE     FNFGV06L                               32
714600970102    4C                   ENDIF
714700970102    3C                   ENDDO
714800970102    2C                   ENDIF
714900970102     C* RIPRISTINO CAMPI DELLA DSTV
715000970102     C                   MOVEL     DSTVS         DSTV
715100970102     C* ERRORE
715200970102     C     WERR          IFNE      ' '
715300970102     C                   MOVEA     MSG(69)       K78
715400970102     C                   MOVEA     W003A         K78(58)
715500970102     C                   MOVEA     K78           VIDMSG
715600970102     C                   SETON                                        285090
715700970102     C                   ENDIF
715800970102     C     WAPE          IFNE      ' '
715900970102     C                   MOVEA     MSG(68)       K78
716000970102     C                   MOVEA     W003A         K78(58)
716100970102     C                   MOVEA     K78           VIDMSG
716200970102     C                   SETON                                        285090
716300970102     C                   ENDIF
716400970102     C*
716500970102    1C                   END
716600970102     C                   ENDSR
716700020805      **************************************************************************
716800020805      * AGGANCIO FILE FNFGW01L
716900020805      **************************************************************************
717000020805     C     CTR_FGW       BEGSR
717100020805      *
717200020805     C     K02FGW01      chain(N)  FNFGW000
717300020805      *
717400020910if  1c                   if        (not %found(FNFGW01L)
717500020910     c                              or  FGWatb <> *blanks)
717600020805     C                   clear                   FNFGW000
717700020805e   1C                   endif
717800020805      *
717900020805     C                   ENDSR
718000130412     C**************************************************************************
718100130412     C*  GESTIONE VIDEATA INSERIMENTO piombi
718200130412     C**************************************************************************
718300130412     C     GESD09        BEGSR
718400130415     c* Se il campo piombi è pieno imposto una riga per ogni costante
718500130415     c* spaziata
718600130415     c                   seton                                        35
718700130415     C                   WRITE     LSt4C08
718800130415     c                   setoff                                       3578
718900130415     c
719000130415     C                   Z-ADD     0             NRR8              4 0
719100130430    1c                   if        vidpmb<>*blanks
719200130415     c                   eval      Indx=1
719300130415     c                   eval      Parx=1
719400130430    2c                   dow       Indx>0 and parx<=48
719500130415     c                   eval      Indx=%scan(' ':vidpmb:Parx)
719600130415     C                   clear                   V8CNOT
719700130430    3c                   if        indx>0 and Indx=parx
719800130430     c                   eval      parx=parx+1
719900130430   x3c                   else
720000130430    4c                   if        Indx>0
720100130430     c*****              leave
720200130415     c
720300130415     c                   eval      v8cnot=%subst(vidpmb:parx:(indx-parx))
720400130415     c                   eval      parx=indx+1
720500130415     c                   else
720600130415     c                   eval      indx=48-parx+1
720700130415     c                   eval      v8cnot=%subst(vidpmb:parx:Indx)
720800130415     c                   clear                   indx
720900130430    4c                   endif
721000130415     c
721100130415     C                   ADD       1             NRr8
721200130415     C                   CLEAR                   V8CSCE
721300130415     C                   WRITE     LSt4S08
721400130415     C                   SETON                                        78
721500130430    3c                   endif
721600130430    2c                   enddo
721700130430    1c                   endif
721800130415     C                   Z-ADD     1             V8CNBR
721900130412     c
722000130412     c                   write     lst4T01
722100130412     c                   write     lst4d06
722200130412     c
722300130412     C     EMD09         TAG
722400130412     C                   WRITE     LSt4C08
722500130412     C                   EXFMT     LSt4D09
722600130502     C                   SETOFF                                       28
722700130412     c
722800130412     C* SCRIVO SUBFILE (SE CHI SONO GIA' IMMESSO NON LO RICARICO E NON
722900130412     C* DO ERRORE)
723000130415    1C     V9CNOT        IFNE      *BLANKS
723100130412     C     V9CNOT        LOOKUP    PIO2                                   31
723200130412     c
723300130415    2C     *IN31         IFEQ      *OFF
723400130430     c* Se non è tutto numerico --> errore
723500130430     c                   testn                   v9cnot               33
723600130430     c                   move      v9cnot        alfa1             1
723700130430     c* Errore se non numerico
723800130430     c                   if        not *in33 or alfa1<'0'
723900130430     c                   eval      vidmsg=msg(81)
724000130430     c                   seton                                        28
724100130430     c                   goto      emd09
724200130430     c                   endif
724300130514     c* Errore se è il sesto
724400130514     c                   if        nrr8 >=  5
724500130514     c                   eval      vidmsg=msg(113)
724600130514     c                   seton                                        28
724700130514     c                   goto      emd09
724800130514     c                   endif
724900130430     c
725000130412     C                   ADD       1             NRr8
725100130412     C                   Z-ADD     NRR8          V8CNBR
725200130524     C                   MOVEL(P)  V9CNOT        V8CNOT
725300130412     C                   CLEAR                   V8CSCE
725400130412     C                   WRITE     LSt4S08
725500130412     C                   SETON                                        78
725600130412     C                   ADD       1             I2                3 0
725700130412     C                   MOVE      V9CNOT        PIO2(I2)
725800130415    2C                   ENDIF
725900130412     C                   CLEAR                   V9CNOT
726000130415    1C                   ENDIF
726100130412     c
726200130412     C* PER F6 ESCO DALLA VIDEATA DI IMMISSIONE "CHI SONO"
726300130415    2C     *INKF         IFEQ      *OFF
726400130412     C                   GOTO      EMD09
726500130415    2C                   ENDIF
726600130412     c
726700130412     c* Quando esco scrivo il campo piombi
726800130415    2c                   if        nrr8>0
726900130412     c                   z-add     1             nrr8
727000130415     c                   clear                   vidpmb
727100130412     c     nrr8          chain     lst4s08
727200130412     c
727300130412     c                   dow       %found
727400130415     c                   if        v8cnot<>*blanks and v8csce<>'A'
727500130412     c
727600130412     c                   if        vidpmb<>*blanks
727700130412     c                   eval      vidpmb=%trim(vidpmb) +' '+v8cnot
727800130412     c                   else
727900130412     c                   eval      vidpmb=v8cnot
728000130412     c                   endif
728100130412     c                   endif
728200130412     c
728300130412     c                   add       1             nrr8
728400130412     c     nrr8          chain     lst4s08
728500130412     c                   enddo
728600130607     c                   endif
728700130412     c
728800130412     C                   ENDSR
728900130604     C********************************************************************
729000130604     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
729100130604     C********************************************************************
729200130605     C     sr_fnfge      BEGSR
729300130604
729400130610     c* ignoro se defluenza o inoltro
729500130610     c                   if        fgvttr<>'I' and wdef1a<>'S'
729600130610
729700130605     c* ciclo per ogni lna del foglio viaggio
729800130605     c                   z-add     fgvlna        wlna
729900130605     c                   z-add     fgvlna        wsca
730000130607     c                   select
730100130607     c* annullamento
730200130607     c                   when      *inkq
730300130607     c                   exsr      sr_fnfgeann
730400130607     c* immissione / ripristino
730500130607     c                   when      *in80   or *inke
730600130607     c                   exsr      sr_fnfgeimm
730700130607     c                   other
730800130607     c* manutenzione
730900130607     c                   exsr      sr_fnfgeman
731000130607     c                   endsl
731100130606     c                   z-add     1             iy
731200130606     C     Iy            DOWLE     299
731300130606     C     FFV(Iy)       IFGT      *ZEROS
731400130606     c                   move      ffv(iy)       wlna
731500130606     c                   move      flp(iy)       wsca
731600130607     c                   select
731700130607     c                   when      *inkq
731800130607     c                   exsr      sr_fnfgeann
731900130607     c                   when      *in80 or *inke
732000130607     c                   exsr      sr_fnfgeimm
732100130607     c                   other
732200130607     c                   exsr      sr_fnfgeman
732300130607     c                   endsl
732400130605
732500130605     c                   endif
732600130605
732700130607     c                   add       1             iy
732800130605     c                   enddo
732900130605
733000130605     c* per manutenzione verifico se eliminate lna
733100130607     c                   if        not *in80 and not *inkq and not *inke
733200130606     c                   z-add     1             iy
733300130607     C     Iy            DOWLE     300
733400130606     C     pffv(Iy)      IFGT      *ZEROS
733500130606     c                   move      pffv(iy)      wlna
733600130606     c                   move      pflp(iy)      wsca
733700130606     c                   exsr      sr_fnfgeann
733800130606
733900130606     c                   endif
734000130606
734100130607     c                   add       1             iy
734200130606     c                   enddo
734300130606     c                   endif
734400130610
734500130610     c                   endif
734600130604
734700130604     c                   endsr
734800130605     C********************************************************************
734900130605     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
735000130605     c*  IMMISSIONE
735100130605     C********************************************************************
735200130605     C     sr_fnfgeimm   BEGSR
735300130605     c     kfge          chain     fnfge01l
735400130605    1c                   if        not %found(fnfge01l)
735500130605     c* non trovato: scrivo
735600130605     c                   exsr      sr_wrtfge
735700130605   x1c                   else
735800130605     c* verifico lo scarico
735900130605     c                   exsr      sr_dtp
736000130605
736100130606    2c                   if        fgeflp=wsca
736200130605     c* stesso scarico --> vedo se aggiornare data/ora partenza
736300130610     c                   if        (wdtp>0 and (wdtp*10000)+whmp<(fgedtp*10000)+
736400130610     c                             fgehmp) or (wdtp>0 and fgedtp=0)
736500130605     c                   eval      fgedtp=wdtp
736600130605     c                   eval      fgehmp=whmp
736700130605     c                   update    fnfge000
736800130605    3c                   endif
736900130605   x2c                   else
737000130605     c* scarico diverso:  se diretto updato e sflaggo
737100130605     c                   exsr      sr_tfa
737200130605
737300130605    3c                   if        wlna=wsca or d55tfa=wsca
737400130605     c                   exsr      sr_updfge
737500130605   x3c                   else
737600130607     c* se no diretto updato se data/ora partenza <
737700130610    3c                   if        wlna<>fgeflp and  d55tfa<>fgeflp
737800130610     c                   if        (wdtp>0 and (wdtp*10000)+whmp<(fgedtp*10000)+
737900130610     c                             fgehmp) or (wdtp>0 and fgedtp=0)
738000130605     c                   exsr      sr_updfge
738100130605    4c                   endif
738200130610    4c                   endif
738300130605    3c                   endif
738400130605    2c                   endif
738500130605    1c                   endif
738600130611     c                   unlock    fnfge01l
738700130605     c                   endsr
738800130605     C********************************************************************
738900130605     C*  Aggiornamento/scrittura file di appoggio per stampa etichette
739000130605     c*  MANUTENZIONE
739100130605     C********************************************************************
739200130605     C     sr_fnfgeman   BEGSR
739300130606     c* verifico se lna presente anche prima della modifica
739400130606     c                   eval      iw=%lookup(%EDITC(wlna:'X'):pffv)
739500130606    1c                   if        iw>0
739600130605     c* cambiato lo scarico
739700130606    2c                   if        %editc(wsca:'X')<>pflp(iw)
739800130605     c     kfge          chain     fnfge01l
739900130605     c                   exsr      sr_tfa
740000130605     c* il nuovo scarico e' un diretto
740100130605    3c                   if        wlna=wsca or d55tfa=wsca
740200130606    4c                   if        fgeflp<>wsca
740300130605     c                   exsr      sr_updfge
740400130606    4c                   endif
740500130605   x3c                   else
740600130606     c                   exsr      sr_dtp
740700130605     c* il nuovo scarico non è un diretto
740800130607     c* cerco l'esistenza di altro foglio viaggio partenza nella stessa data
740900130607     c* e per la stessa linea di arrivo dando la precedenza al diretto se presen
741000130607     c* te o in alternativa al foglio con la data/ora partenza inferiore
741100130606     c                   exsr      sr_cerlna
741200130607     c* se trovato altro foglio ed è diretto aggiorno con quello
741300130607     c* se trovato altro foglio e non è diretto aggiorno con quello che parte pr
741400130607     c* ima
741500130607     c* se non trovato altro foglio aggiorno con il nuovo scarico
741600130607     c                   select
741700130607     c                   when      wtroF='S' and wdir='S'
741800130607     c                   eval      wsca=a_sca
741900130607     c                   eval      wdtp=a_dtp
742000130607     c                   eval      whmp=a_hmp
742100130607
742200130610     c                   when      wtroF='S' and  wdtp>0 and
742300130610     c                             (a_dtp=0 or
742400130610     c                             ((Wdtp*10000)+whmp
742500130610     c                             < (a_dtp*10000)+a_hmp))
742600130610
742700130607     c                   when      wtroF='S'
742800130607     c                   eval      wsca=a_sca
742900130607     c                   eval      wdtp=a_dtp
743000130607     c                   eval      whmp=a_hmp
743100130607
743200130607     c                   endsl
743300130607     c                   exsr      sr_updfge
743400130605    3c                   endif
743500130611     c                   unlock    fnfge01l
743600130605    2c                   endif
743700130607     c                   clear                   pffv(iW)
743800130607     c                   clear                   pflp(iW)
743900130605   x1c                   else
744000130605     c* se lna nuova tratto come in immissione
744100130605     c                   exsr      sr_fnfgeimm
744200130605    1c                   endif
744300130607
744400130605     c                   endsr
744500130606     C********************************************************************
744600130606     C*  Aggiornamento file di appoggio per stampa etichette
744700130606     c*  ANNULLAMENTO
744800130606     C********************************************************************
744900130606     C     sr_fnfgeann   BEGSR
745000130606     c     kfge          chain     fnfge01l
745100130606    1c                   if        %found(fnfge01l)
745200130606     c* se stesso scarico
745300130606     c* cerco se esiste altro f.v.p nella stessa data con uguale lna
745400130606     c* dando la precedenza al diretto se presente
745500130606    2c                   if        fgeflp=wsca
745600130606     c                   exsr      sr_cerlna
745700130607     c                   if        wtroF='S'
745800130607     c                   eval      wsca=a_sca
745900130607     c                   eval      wdtp=a_dtp
746000130607     c                   eval      whmp=a_hmp
746100130607     c                   exsr      sr_updfge
746200130607     c                   else
746300130607     c* se non trovato altro foglio deleto
746400130607     c                   delete    fnfge000
746500130607     c                   endif
746600130606    2c                   endif
746700130611     c                   unlock    fnfge01l
746800130606    1c                   endif
746900130606     c                   endsr
747000130605     C********************************************************************
747100130605     C*  write su fnfge
747200130605     C********************************************************************
747300130605     C     sr_updfge     BEGSR
747400130607     c* slfaggo per la ristampa solo se cambiato lo scarico
747500130607     c                   if        fgeflp<>wsca
747600130607     c                   clear                   fgefst
747700130607     c                   endif
747800130606     c                   eval      fgeflp=wsca
747900130605     c                   eval      fgedtp=wdtp
748000130605     c                   eval      fgehmp=whmp
748100130605     c                   update    fnfge000
748200130605     c                   endsr
748300130605     C********************************************************************
748400130605     C*  update su fnfge
748500130605     C********************************************************************
748600130605     C     sr_wrtfge     BEGSR
748700130605     c                   clear                   fnfge000
748800130605     c                   eval      fgelnp=fgvlnp
748900130605     c                   eval      fgedfv=fgvdfv
749000130605     c                   eval      fgelna=wlna
749100130606     c                   eval      fgeflp=wsca
749200130605     c* data/ora partenza reale o teorica
749300130605     c                   if        fgvdtp>0
749400130605     c                   eval      fgedtp=fgvdtp
749500130605     c                   eval      fgehmp=fgvhmp
749600130605     c                   else
749700130605     c                   eval      kepa='P'
749800130605     c                   clear                   wtfa
749900130605     c                   eval      ktdh='T'
750000130605     c     kfv22         chain(n)  fnfv201l
750100130605     c                   if        %found(fnfv201l)
750200130605     c                   z-add     fv2dpa        fgedtp
750300130605     c                   z-add     fv2hpa        fgehmp
750400130605     c                   endif
750500130605     c                   endif
750600130605     c                   write     fnfge000
750700130605     c                   endsr
750800130605     C********************************************************************
750900130605     C*  determino dtat/ora partenza del foglio viaggio
751000130605     C********************************************************************
751100130605     C     sr_dtp        BEGSR
751200130605     c                   clear                   wdtp
751300130605     c                   clear                   whmp
751400130605     c* la data/ora partenza è la reale se presente altrimenti prendo la
751500130605     c* teorica
751600130605     c                   if        fgvdtp>0
751700130605     c                   eval      wdtp=fgvdtp
751800130605     c                   eval      whmp=fgvhmp
751900130605     c                   else
752000130605     c                   eval      kepa='P'
752100130605     c                   clear                   wtfa
752200130605     c                   eval      ktdh='T'
752300130605     c     kfv22         chain(n)  fnfv201l
752400130605     c                   if        %found(fnfv201l)
752500130605     c                   z-add     fv2dpa        wdtp
752600130605     c                   z-add     fv2hpa        whmp
752700130605     c                   endif
752800130605     c                   endif
752900130605     c                   endsr
753000130605     C********************************************************************
753100130605     C*  determino terminal di arrivo della linea di arrivo
753200130605     C********************************************************************
753300130605     C     sr_tfa        BEGSR
753400130605     c                   clear                   fnlv55ds
753500130605     c                   eval      d55lin=wlna
753600130605     c                   eval      d55lnp=fgvlnp
753700130605     c                   eval      d55drf=fgvdfv
753800130605     c                   eval      d55tpt='A'
753900130605     C                   CALL      'FNLV55R'
754000130605     C                   PARM                    FNLV55DS
754100130605     c                   endsr
754200130606     C********************************************************************
754300130606     C*  Ricerca altro foglio
754400130606     C********************************************************************
754500130606     C     sr_cerlna     BEGSR
754600130606     c                   clear                   wtroF
754700130607     c                   clear                   a_sca
754800130607     c                   clear                   a_dtp
754900130606     c                   clear                   a_hmp
755000130606     c                   clear                   x_dtp
755100130606     c                   clear                   x_hmp
755200130606     c                   clear                   wdir
755300130606     c* verifico l'esistenza di un altro foglio viaggio nella stessa data e
755400130607     c* per la stessa lna
755500130606     c     kfgv02        setll     fnfgv02l
755600130606     c     kfgv02        reade     fnfgv02l
755700130606    1c                   dow       not %eof(fnfgv02l) and wdir=' '
755800130606     c* escludo foglio se lo stesso che ho appena manutenzionato
755900130610     c* escludo anche se foglio annullato
756000130610    2c                   if        f2vnfv<>fgvnfv and f2vatb<>'*'
756100130606     c                   eval      FGWnfv = F2Vnfv
756200130606     c                   eval      FGWlnp = F2Vlnp
756300130606     c                   exsr      CTR_FGW
756400130606     C                   MOVEL     FGWFF3        W2FF3
756500130606     C                   MOVEL     FGWFF4        W2FF4
756600130606     C                   MOVEL     FGWFl3        W2Fl3
756700130606     C                   MOVEL     FGWFl4        W2Fl4
756800130606     c                   eval      f2p(1)=%editc(f2vlna:'X')
756900130606     c                   eval      ij=%lookup(%EDITC(wlna:'X'):f2v)
757000130607    3c***                if        ij>0 and f2p(ij)=%editc(fgeflp:'X')
757100130607     c                   if        ij>0
757200130606     c                   eval      wtroF='S'
757300130606    4c                   if        %editc(wlna:'X')=f2p(ij) or
757400130606    4c                             %editc(d55tfa:'X')=f2p(ij)
757500130606     c                   eval      wdir='S'
757600130606    4c                   endif
757700130606    4c                   if        f2vdtp>0
757800130606     c                   eval      x_dtp=f2vdtp
757900130606     c                   eval      x_hmp=f2vhmp
758000130606   x4c                   else
758100130606     c                   eval      kepa='P'
758200130606     c                   clear                   wtfa
758300130606     c                   eval      ktdh='T'
758400130606     c     kfv22a        chain(n)  fnfv201l
758500130606    5c                   if        %found(fnfv201l)
758600130606     c                   z-add     fv2dpa        x_dtp
758700130606     c                   z-add     fv2hpa        x_hmp
758800130606    5c                   endif
758900130606    4c                   endif
759000130606    4c                   if        a_dtp=0 or (x_dtp>0 and (x_dtp<a_dtp or
759100130607     c                             x_dtp=a_dtp and x_hmp<a_dtp)) or wdir='S'
759200130606     c                   eval      a_dtp=x_dtp
759300130606     c                   eval      a_hmp=x_hmp
759400130607     c                   eval      a_sca=%dec(f2p(ij):3:0)
759500130606    4c                   endif
759600130606     c
759700130607     c                   endif
759800130606    2c                   endif
759900130606     c     kfgv02        reade     fnfgv02l
760000130606    1c                   enddo
760100130606     c                   endsr
760200130510     c*-------------------------------------------------------------------
760300130510     c     INV_email     BEGSR
760400130510     c
760500130510     c                   if        not %open(prtemail)
760600130510     C                   EXSR      SR_OPENPRTF
760700130510     C                   endif
760800130510     c
760900130510     c                   exsr      Routend
761000130510     c
761100130510     c                   ENDSR
761200130510
761300130510      /free
761400130510       //--------------------------------------------------------------
761500130510       //?Override al file di stampa per impostarvi i dati per
761600130510       //?  l'invio via e-mail   +   stampa inizio e-mail
761700130510       //--------------------------------------------------------------
761800130510       BEGSR sr_OpenPrtF;
761900130510
762000130510         // Override al file di stampa per impostarvi i dati per
762100130510         // l'invio via e-mail
762200130510           exsr sr_Override;
762300130510
762400130510           open PrtEMAIL;
762500130510
762600130510         // Stampa una testata se NON è richiesta la e-mail
762700130510           IF  §MRAdreg =  *blank;
762800130510             O_testo = JobUser + ' - ' + SDSpgm
762900130510                     + ' - ' + %editc( *date : 'Y' )
763000130510                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
763100130510             except PrtDet;
763200130510             clear O_testo;
763300130510             except PrtDet;
763400130510           ENDIF;
763500130510
763600130510         // Stampa testo iniziale
763700130510
763800130510           if fgvsnp='B'  ;
763900130510           desbis='BIS'  ;
764000130510           else  ;
764100130510           clear desbis  ;
764200130510           endif  ;
764300130510           O_testo = 'Foglio Viaggio :  '+%editc(fgvlnp:'X')+'  '+
764400130510           %editc(fgvnfv:'2')+'  per '+ %editc(fgvlna:'X')+'-'+deslna +
764500130510           '   ' + desbis+ '  del  ' + %editw(viddfv:'  /  /    ') ;
764600130510           except PrtDet;
764700130510
764800130510           clear O_testo;
764900130510           except PrtDet;
765000130510           O_testo = 'Traino: ' +%editc(fgvtrn:'2')   ;
765100130510           except PrtDet;
765200130510
765300130510         // Stampa una riga vuota
765400130510           clear O_testo;
765500130510           except PrtDet;
765600130510
765700130510         // Stampa trazionista
765800130521           KTLPDR=fgvpdr  ;
765900130521           CHAIN  (ktltip: ktlpdr) tntlZ01L   ;
766000130521           if %found(tntlz01l) ;
766100130521            O_testo = 'Trazionista : '+%editc(fgvpdr:'X')+'-'+tlzrsc    ;
766200130521           else  ;
766300130521            O_testo = 'Trazionista : '+%editc(fgvpdr:'X')+'-'+fgvdpd    ;
766400130521           endif  ;
766500130510           except PrtDet;
766600130510
766700130510         // Stampa una riga vuota
766800130510           clear O_testo;
766900130510           except PrtDet;
767000130510
767100130510          if winviamail='D' or winviamail='E'  ;
767200130510          if *in80  ;
767300130510           O_testo = 'CONDUCENTE    non accreditato:  '+ viddcn  ;
767400130510           else  ;
767500130510           O_testo = 'CONDUCENTE    non accreditato modificato da: '+
767600130510           fgvpmb    ;
767700130510           o_testo=%trim(o_testo) + '  a: ' + viddcn  ;
767800130510           endif  ;
767900130510
768000130510           except PrtDet;
768100130510         // Stampa una riga vuota
768200130510           clear O_testo;
768300130510           except PrtDet;
768400130510           endif  ;
768500130510
768600130510          if winviamail='M' or winviamail='E'  ;
768700130510          if *in80  ;
768800130510           O_testo = 'TARGA MOTRICE non accreditata:  '+ vidtrm  ;
768900130510           else  ;
769000130510           O_testo = 'TARGA MOTRICE non accreditata modificata da: '+
769100130510           fgvtrm + '  a: ' + vidtrm  ;
769200130510           endif  ;
769300130510
769400130510           except PrtDet;
769500130510         // Stampa una riga vuota
769600130510           clear O_testo;
769700130510           except PrtDet;
769800130510           endif  ;
769900130510
770000130510       ENDSR;
770100130510
770200130510       //--------------------------------------------------------------
770300130510       //?Override al file di stampa per impostarvi i dati per
770400130510       //?  l'invio via e-mail   +   stampa inizio e-mail
770500130510       //--------------------------------------------------------------
770600130510       BEGSR sr_Override;
770700130510
770800130510         reset TRTCM1ds;
770900130510
771000130510         IF  §MRAdreg <> *blank;
771100130510           §CM1mitt = %trim(§cm1mitt);
771200130510           %subst(§cm1mitt:4:3)=%editc(fgvlnp:'X') ;
771300130510           §CM1dst  = %trim(§MRADDEST) ;
771400130510
771500130510           §CM1tips = §MRAdreg;
771600130510           §CM1po   = %editc(simfel:'X');
771700130510           §CM1var  = '*OBJM*' + 'Richiesta autorizzazione al carico'   ;
771800130510           §CM1idp  = §MRAdidpro;
771900130510           Qcmd = C_CmdOvrPrtF
772000130510                + ' outq(' + %trim(§MRAdoutqi) + ')'
772100130510                + ' usrdfndta(''' + TRTCM1ds + ''')';
772200130510         ELSE;
772300130510           Qcmd = C_CmdOvrPrtF;
772400130510         ENDIF;
772500130510
772600130510         callp(e) qCmdExc(Qcmd : %size(Qcmd));
772700130510
772800130510       ENDSR;
772900130510
773000130510       //--------------------------------------------------------------
773100130510       //?Operazioni finali.
773200130510       //--------------------------------------------------------------
773300130510       BEGSR RoutEnd;
773400130510
773500130510         if %open(PrtEMAIL);
773600130510
773700130510         //?Chiusura dello spool?
773800130510           clear O_testo;
773900130510           except PrtDet;
774000130510           O_testo = '***    Fine    ***';
774100130510           except PrtDet;
774200130510
774300130510           close PrtEMAIL;
774400130510
774500130510         //?Eliminazione overflow?
774600130510           Qcmd = C_CmdDltOvr;
774700130510           callp(e) qCmdExc(Qcmd : %size(Qcmd));
774800130510
774900130510         endif;
775000130510
775100130510       ENDSR;
775200130510
775300130510      /end-free
775400130510     o* ---------------------------------------------------------------
775500130510     oPrtEMAIL  e            PRTdet      1
775600130510     o                       O_testo
775700130510
775800020805      **************************************************************************
775900960302** SCHIERA MSG - MESSAGGI DI ERRORE
776000951023Immettere un foglio di viaggio esistente                                       1
776100951026Il foglio viaggio e' GIA' stato CHIUSO!!!                                      2
776200951026Non e' possibile annullare il foglio viaggio: presenti SPUNTE  !!              6
776300951026Data errata                                                                    4
776400951130Impossibile modificare la data FV se abbinato:enter per forzare e non abbinare 5
776500951023Non e' possibile annullare il foglio viaggio: presenti BOLLE borderizzate      6
776600980430Data non inferiore di 1 gg. e non superiore al limite imposto
776700940912Immettere codice filiale esistente                                             8
776800951026Immettere codice TRAZIONISTA esistente                                         9
776900951023Immettere il codice TRAZIONISTA o la ragione sociale                          10
777000940912Immettere almeno la targa della motrice                                       11
777100951024Impossibile esporre l'importo del FV: se immesso il traino NON SARA' abbinato 12
777200940912Immettere orario valido                                                       13
777300940912Immettere filiale di scarico valida                                           14
777400940912Immettere anche la filiale                                                    15
777500940912Filiale gia' presente                                                         16
777600940912Filiale di scarico obbligatoria                                               17
777700951026Immettere il nome del Conducente                                              18
777800130429
777900951023Immettere un numero traino esistente                                          20
778000951023Traino errato:scaduto, ancora da decorrere o non esiste la filiale come carico21
778100130502Traino non utilizzabile in questo giorno: F6 per forzare e non abbinare       22
778200951017Impossibile effettuare la copia da un traino di tipo "Affluenza"              23
778300950901Immettere l'importo pattuito                                                  24
778400951027Se copia da traino non si puo'modificare il tipo traino:enter x forz.e non abb25
778500950901Immettere tipo automezzo valido                                               26
778600951024Per questo trazionista/tipo automezzo non esiste listino: enter per forzare   27
778700951024Lin.arrivo finale errata:non e' fil.scarico del traino -> IMPOSSIBILE LA COPIA28
778800951025Lin.Arrivo immessa non e'quella finale del traino:copia dati senza abbinamento
778900951017Filiale errata: non c'e' nulla da caricare                                    30
779000130502ATTENZIONE:esiste gia'F.Viaggio  xxxxx  abbinato a questo traino. F6  Forza   31
779100951024Lin.arrivo finale presente 2 volte: come "A" e come "R"->TELEFONARE IN SEDE!  32
779200951025L'importo non corrisponde a quello presente nel listino traino.Enter x forzare33
779300951025FV abbinato al traino xxxxxxx ora inesistente o scaduto: il FV sara' sbinato  34
779400951025Impossibile eliminare la linea  xxx  : PRESENTI BOLLE BORDERIZZATE!!
779500951025Impossibile eliminare la linea  xxx  : PRESENTI SPUNTE!!!
779600951025Impossibile variare la fil.scarico xxx della linea xxx:PRESENTI BOLLE BORDERIZ37
779700960126DATA PARTENZA errata o obbligatoria                                           38
779800951026Immettere la descrizione dei PIOMBI                                           39
779900951026Il foglio viaggio e'  ANNULLATO!!!!                                           40
780000130429
780100130429
780200951027Manca il numeratore dei Fogli Viaggio: TELEFONARE IN SEDE!!!!                 43
780300110531ATTENZIONE!! Il Foglio Viaggio non sarà abbinato al traino. F6 per forzare
780400951027Data non inferiore di 1 gg. e non superiore a 5 rispetto alla data F.Viaggio  45
780500951027Tipo traino errato                                                            46
780600951027Per un foglio DEFLUENZA o LOCALE,la filiale deve essere un 2 livello in arrivo47
780700951027Per un foglio DEFLUENZA lo scarico deve essere = alla filiale indicata        48
780800951031Per un foglio LOCALE, lo scarico deve essere una filiale dell'area o se stessa49
780900951027Per un foglio DEFLUENZA la filiale non puo' essere = alla linea partenza FV   50
781000951030Non trovati trazionisti per chiave alfabetica richiesta                       51
781100951103Non c'e' più ritardo/anticipo: se si vuole si può eliminare la causale        52
781200951103Immettere causale valida                                                      53
781300951108Se immessa causale "G" occorre inserire descrizione ritardo/ant. premendo F21 54
781400960126Per inserire la descrizione ritardo occore immettere la causale               55
781500951107Presente ritardo/anticipo nella partenza del foglio: immettere la causale     56
781600960126Presente descrizione ritardo: immettere la causale o eliminare la descrizione 57
781700960126Presente descrizione ritardo: premere F6 per conferma o eliminare descrizione 58
781800960126Eliminata Descrizione Ritardo: confermare con F6 o reinserire la descrizione  59
781900951120Inserimento non consentito: qualcuno sta gia' inserendo dei Fogli viaggio     60
782000951120Lin.arrivo finale errata:non e' fil.scarico del traino. Enter per forzare     61
782100951120Se inserita la causale ritardo/anticipo occorre immettere la data partenza    62
782200960122La data Foglio Viaggio e' un giorno festivo: enter per forzare                63
782300960201Una volta inserite  data / ora Partenza non possono essere piu' eliminate     64
782400960130Se copia da traino non si puo'modificare la categoria: enter x forz.e non abb.65
782500130430Per questa linea di arrivo TIPO TRAINO deve essere LOCALE DEFLUENZA o INOLTRO 66
782600960910Foglio viaggio attualmente impegnato da un altro lavoro:ATTENDERE e riprovare 67
782700970102Esiste gia'un foglio aperto DEFLUEN.per la linea ESTERA  xxx  :aprire come BIS
782800970102Esiste gia'un foglio DEFL nel giorno per la linea ESTERA xxx :aprire come BIS
782900970102Le filiali  xxx  /  xxx  hanno stesso ter.arrivo ma non la stessa fil.scarico
783000990514Per un foglio DEFLUENZA per l'ESTERO devono essere presenti solo linee ESTERO
783100970526Non e' possibile modificare la data del foglio viaggio: presenti SPUNTE  !!   72
783200990514Non e' possibile chiudere un foglio DEFLUENZA per l'ESTERO                    73
783300000316Per filiale di arrivo di 2° livello XXX inserire anche il suo terminal di arr.74
783400010621Immettere il numero del cellulare del Conducente                              75
783500010704Numero cellulare errato                                                       76
783600000605Foglio in gestione ad altro p.o.                                              77
783700010920La divisa non corrisponde a quella presente nel listino traino.Enter x forzare78
783800010920Divisa errata                                                                 79
783900010920Divisa non ammessa per i traini                                               80
784000130430Il codice del piombo deve essere formato da 8 numeri                          81
784100130429
784200130429
784300010920Non ammessi importi con decimali per la divisa indicata                       84
784400011001Se modificata la divisa ridigitare il relativo importo                        85
784500011012Immettere la divisa                                                           86
784600020919P.O. finale di arrivo errato (deve coincidere con il P.O. gestione)           87
784700130430
784800130430
784900050706Impossibile riaprire il foglio:troppo "vecchio".                              90
785000080910Manutenzione non ammessa per il foglio viaggio richiesto                      91
785100110512Punto di scambio erraro o scaduto                                             92
785200110512Se presenti dati punto di scambio, immettere anche il codice                  93
785300110512Data punto di scambio errata                                                  94
785400110512Data punto di scambio non deve essere minore della data foglio viaggio        95
785500110512Se copia da traino non si può modificare Punto Scambio: ENTER x forz.e non abb96
785600110609Immettere la filiale finale di ritorno autista                                 7
785700110531Aggiunta fil.Ritorno finale Autista: il traino di scambio viene eliminato     98
785800110610Data e ora scambio obbligatori se presente numero traino di scambio           99
785900110513Numero traino di scambio errato o scaduto                                     10
786000110530Aggiunta Fil.Ritorno Finale Autista: il traino verrà sbinato. Enter x forzare 01
786100110530Errori nei dati relativi al punto di Scambio. F2 per correggere               02
786200110530Traino BIS: eliminato punti di scambio copiato da traino                      03
786300110531Se cambiato punto di scambio, immettere la Fil.Finale Ritorno Autista         04
786400120208Impossibile l'annullamento:foglio già elaborato in sede. Telef.ufficio Traini  5
786500130430Per attivare la ricerca del conducente, immettere il trazionista               6
786600130510CONDUCENTE non accreditato per il trazionista:enter per forzare e inviare Mail 7
786700130424Per attivare la ricerca della targa motrice, immettere il trazionista          8
786800130510Targa MOTRICE non accreditata per il trazionista:enter x forzare e inviareMail 9
786900130509La targa motrice è priva della DATA DI CONTROLLO : enter per forzare          10
787000130510CONDUCENTE non accreditato per il trazionista: enter per forzare              11
787100130510Targa MOTRICE non accreditata per il trazionista: enter per forzare           12
787200130514Non è possibile inserire con lettura barcode più di 5 piombi                  13
787300130729L'orario di partenza del traino non può essere maggiore dell'orario di scambio14
787400030701** SCHIERA SBI - MOTIVI DI SBINAMENTO FV
787500960302Immesso l'IMPORTO DEL FV su traino     1
787600960302xxxxxxx,  che  non  deve  esporlo      2
787700960304Traino xxxxxxx non previsto nel  gi    3
787800960304orno  di  xxxxxxxxx                    4
787900960302Esiste il FV xxxxx già abbinato al     5
788000960302traino xxxxxxx                         6
788100960304Nel traino xxxxxxx e' stato modific    7
788200960304ato  il TIPO da x in x-xxxxxxxxxxxx    8
788300960302Abbinato a traino xxxxxxx ma modifi    9
788400960302cata la data foglio viaggio           10
788500960304Nel traino xxxxxxx e' stata modific   11
788600960304ata  la categoria in XXX              12
788700960302FV creato SENZA copia da Traino       13
788800960302XXXXXXX N.TRAINO sbinato dal fv       14
788900960302Variate le LINEE al Traino  XXXXXXX   15
789000970103variato Arrivo FINALE da xxx a xxx    16
789100970114 aggiunte  xxx xxx xxx xxx xxx xxx    17
789200970114 eliminate xxx xxx xxx xxx xxx xxx    18
789300970114 modificato scarico xxx xxx xxx xxx   19
789400110512Nel traino xxxxxxx modificato Punto   20
789500110512 Scambio da xxxxxxxxxx a xxxxxxxxxx   21
789600110530Nel traino xxxxxxx aggiunta FilRito   22
789700110530rnoAut xxx per lo scambio             23
789800951002** SCHIERA GFV
789900951002BCDEFGHILMNOPQRSTUVZ
790000960302** SCHIERA TES - TESTATE
790100130417 ***  FOGLI  VIAGGIO   PARTENZA  ***
790200130628 *CHIUSURA FOGLI VIAGGIO DA SPUNTE *
790300951026 ***   BORDERIZZAZIONE   BOLLE   ***
790400960302** SCHIERA GIO - GIORNI DELLA SETTINANA
790500960302LUNEDI
790600960302MARTEDI
790700960302MERCOLEDI
790800960302GIOVEDI
790900960302VENERDI
791000960302SABATO
791100960302DOMENICA
