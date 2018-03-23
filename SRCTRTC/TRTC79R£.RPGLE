000100980618      ***********************************************************************
000200070108     C*?  IMPORTA BOLLE DA PARTNER ESTERI   vers.96A   E.D.I.   ESTERO      *
000300960329      ***********************************************************************
000400070108      * A livello di testata, se non è riuscito l'aggancio standard alla PT
000500070108      * (ID+Qualificatore), aggancio la PT ignorando il qualificatore.
000600070108      * Se il 2° aggancio va a buon fine a livello di dettaglio riaggancio
000700070108      * la PT utilizzando la nazione del primo mittente di dettaglio come
000800070108      * qualificatore.
000900070108      * ?------------------------------------------------------------------ */
001000070108      * Le ricerca dei "sottocodici cliente per partner sono state spostate
001100070108      * a dopo l'aggancio della PT a dettaglio.
001200070108      * ?------------------------------------------------------------------ */
001300960329     H DECEDIT('0,') DATEDIT(*DMY.)
001400070108      * ?------------------------------------------------------------------ */
001500960524     FEDIFCSUM  UF   E           K DISK
001600040301     F                                     INFDS(SRECDS)
001700960528     FEDRDE01L  UF A E           K DISK
001800960329     FFLNUF01L  UF   E           K DISK
001900960329     FTABEL00F  IF   E           K DISK
002000990922     FCNACO00F  IF   E           K DISK
002100990922     FCNIND00F  IF   E           K DISK
002200960524     FEDTAB01L  IF   E           K DISK
002300020909      *
002400020909     FEDiVAB1L  UF A E           K DISK
002500020909     FEDiVAD0F  O  A E           K DISK
002600020909     FEDiVAT0F  O  A E           K DISK
002700020909      *
002800000207     FEDSUM00F  O  A E           K DISK
002900960527     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000000120     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)   USROPN
003100070108      * ?------------------------------------------------------------------ */
003200000120      * Schiere
003300070108      * ?------------------------------------------------------------------ */
003400970624     D SGN             S              7  0 DIM(5000)
003500980219     D SGE             S             35    DIM(5000)                            EUROEXPRESS
003600000120      * Stringhe per conversione alfanumerico/numerico N.Documento
003700960725     D CA              S             78    DIM(1) CTDATA PERRCD(1)
003800960725     D CN              S             78    DIM(1) CTDATA PERRCD(1)
003900000120      * Nr. documento alfanumerico da decodificare
004000000120     D NDA             S              1    DIM(35)
004100000120      * Nr. documento alfanumerico da già decodificato
004200960329     D NDN             S              1    DIM(15)
004300000120      * Messaggi di errore
004400961122     D MSG             S             60    DIM(32) CTDATA PERRCD(1)
004500000120      * Tabella codici trattamento merce (1B)
004600960528     D CTM             S              2    DIM(200)
004700040227     D DTM             S             67    DIM(200)
004800000120      * Tabella codici valuta
004900960527     D CCV             S              3    DIM(200)
005000960527     D DCV             S             89    DIM(200)
005100000120      * Tabella codici nazioni
005200011217     D C15             S              3    DIM(500)
005300011217     D ISO             S              3    DIM(500)
005400011217     D CPF             S              2  0 DIM(500)
005500070123      *
005600000120      * Tabella partner
005700121113     D CPT             S             35    DIM(200)
005800121105     D CPTparz         S             35    DIM(%elem(CPT))
005900121105     D KSC_UNB         S              7    DIM(%elem(CPT))
006000121105     D DPT             S             90    DIM(%elem(CPT))
006100121105     D DPU             S             90    DIM(%elem(CPT))
006200121105     D DPV             S             90    DIM(%elem(CPT))
006300070219     d Ptn_parziale    s              1    inz('N')
006400070219     d UNB_Parz        s             35    inz(' ')
006500070219     d UNB_Generico    s             35    inz(' ')
006600070219     d sav_CPTx        s              3  0 inz(0)
006700070123      *
006800000120      * Tabella CL --> codici clienti - tariffa
006900961122     D CCL             S             35    DIM(300)
007000961122     D DCL             S             90    DIM(300)
007100000120      * Schiere x caricamento cod.cliente  <>
007200961204     D KCL             S              7  0 DIM(300)
007300000120      * Priorità trasporto
007400061128     D PTR35           S             35    DIM(100)
007500960528     D PTR             S              3    DIM(100)
007600960527     D TSP             S              1    DIM(100)
007700000120      * Termini di consegna
007800061128     D CTB35           S             35    DIM(100)
007900061128     D CTB             S              3    DIM(100)
008000960527     D POR             S              1    DIM(100)
008100000120      * Tipo pagamento C/Assegno
008200061128     D CTC35           S             35    DIM(100)
008300960716     D CTC             S              2    DIM(100)
008400960716     D TPI             S              2    DIM(100)
008500000120      * Decodifiche servizi speciali
008600061128     D SS35            S             35    DIM(100)
008700061128     D SS              S              3    DIM(100)
008800960911     D DSS             S             90    DIM(100)
008900000120      * Schiere x routine di conversione CAP
009000960828     D CAP5            S              1    DIM(5)
009100960828     D CAP9            S              1    DIM(9)
009200991007     D CAPM            S              1    DIM(9)
009300000120      * Schiera per memorizzazione FTX ricevuti
009400970520     D QUA             S              3    DIM(100)
009500970520     D FTX             S             70    DIM(100)
009600000120      * Tabelle capiconto
009700080115      * Schiere x inidirizzo/località
009800080115     D F35             S              1    DIM(35)
009900070104      * ?---------------------------------------------------------------------*
010000070104     C*?    DS versione 96A del flat FILE
010100070104      * ?---------------------------------------------------------------------*
010200000120      * .. per scompozione dati ricevuti a seconda del tipo record
010300070108     D ED6T00DS      E DS
010400070108     D ED6T01DS      E DS
010500070108     D ED6T05DS      E DS
010600000120     D  T05                   13    362    DIM(5)
010700070108     D ED6T10DS      E DS
010800000120     D  T10                   10     53    DIM(2)
010900070108     D ED6T15DS      E DS
011000070108     D ED6T20DS      E DS
011100000120     D  T20                  106    249    DIM(3)
011200070109     D  T20D                 250    411    DIM(9)
011300070108     D ED6T21DS      E DS
011400070108     D ED6T22DS      E DS
011500070108     D ED6T25DS      E DS
011600000120     D  T25                   68    123    DIM(2)
011700070108     D ED6T26DS      E DS
011800070108     D ED6T27DS      E DS
011900070108     D ED6T28DS      E DS
012000070108     D ED6T30DS      E DS
012100070125     D  W6350                 33     47                                         EQN
012200070108     D ED6T31DS      E DS
012300070108      *---
012400070108     D ED6D00DS      E DS
012500000120     D  D00                   77    142    DIM(3)
012600070108     D ED6D01DS      E DS
012700070108     D ED6D05DS      E DS
012800000120     D  D05                   13    362    DIM(5)
012900000120     D  D05A                 366    715    DIM(5)
013000070108     D ED6D10DS      E DS
013100070108     D ED6D12DS      E DS
013200070108     D ED6D13DS      E DS
013300070108     D ED6D15DS      E DS
013400070108     D ED6D16DS      E DS
013500070108     D ED6D17DS      E DS
013600070108     D ED6D18DS      E DS
013700070108     D ED6D19DS      E DS
013800070108     D ED6D1ADS      E DS
013900070108     D ED6D20DS      E DS
014000960711     D  W1496                 10     14
014100960711     D  W7224                 15     22
014200070108     D ED6D22DS      E DS
014300070108     D ED6D25DS      E DS
014400960711     D  WPESO                 22     37
014500070108     D ED6D26DS      E DS
014600070108     D ED6D27DS      E DS
014700070108     D ED6D28DS      E DS
014800070108     D ED6D30DS      E DS
014900070216     D  D30_1                 10    359    DIM(10)
015000070216     D  D30_2                360    709    DIM(10)
015100070205     D  D30ski                10    709
015200070216      **
015300070216     D  X30            S             35    DIM(20)                              generico segnacolli
015400070216     D  X30_1          S             35    DIM(10)                              generico segnacolli
015500070216     D  X30_2          S             35    DIM(10)                              generico segnacolli
015600070216      **
015700070108     D ED6D31DS      E DS
015800070108     D ED6D32DS      E DS
015900070216     D  D32_1                 13    362    DIM(10)
016000070205     D  D32ski                13    362
016100070216      **
016200070108     D ED6D35DS      E DS
016300070108     D ED6D36DS      E DS
016400070108     D ED6D37DS      E DS
016500070108     D ED6D40DS      E DS
016600070108     D ED6D41DS      E DS
016700070108     D ED6D42DS      E DS
016800070108     D ED6D43DS      E DS
016900070104      * ?---------------------------------------------------------------------*
017000050112      * Numeratore Bolle (302)
017100050112     D trul33ds      E DS
017200050112      *
017300050112     D Ds3C          E DS
017400040301      *
017500040301      * numero relativo di record
017600040301     D SRECDS          DS
017700040301     D  NRREC                397    400B 0
017800040301      *
017900000120      *  Ds per reperimento dati peso/Volume intera spedizione
018000960524     D WTT10           DS
018100960524     D  WCTR                   1      3
018200070213     D  WVLU                   4     21  3
018300070213     D  WTPM                  22     24
018400000120      *  Ds per reperimento località
018500960524     D WTT20           DS
018600960524     D  WTPL                   1      3
018700960524     D  WLOC                   4     28
018800960725     D  WTP1                  29     31
018900960725     D  WLO1                  32     48
019000000120      *  Ds per reperimento date partenza/arrivo...
019100960524     D WTT20D          DS
019200960524     D  WTPD                   1      3
019300960524     D  WDTA                   4     15
019400960524     D  WFOR                  16     18
019500000120      *  Ds per reperimento dati comunicazioni
019600960524     D WTT25           DS
019700960524     D  WTPC                   1      3
019800960524     D  WCOM                   4     28
019900000120      *  Ds per reperimento dati importi
020000960524     D WTD00           DS
020100960524     D  WTPI                   1      3
020200960524     D  WIMP                   4     19  3
020300960524     D  WVAL                  20     22
020400000120      *  Ds per reperimento riferimeti singola spedizione
020500960524     D WTD10           DS
020600960524     D  WTPR                   1      3
020700960524     D  WRIF                   4     38
020800000120      * DS per reperimento destinatario comunicazione
020900960524     D WTD15           DS
021000960524     D  WTCD                   1      3
021100960524     D  WCMD                   4     28
021200070104      * ?---------------------------------------------------------------------*
021300000120      * .. per reperimento tipo record di EDIFTMIN letto
021400000120      *                    .. 5 - 8  : tipo record letto
021500960524     D WDAT            DS           587
021600960524     D  SUMTPR                 5      8
021700070104      * ?---------------------------------------------------------------------*
021800980617     D KPJBA         E DS
021900000120     D FNLV55DS      E DS
022000020909     D TIBS55DS      E DS
022100000120     D TISI95DS      E DS
022200000120     D UTEDSE0F      E DS
022300000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
022400000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
022500000120      * Ds scomposzione tipo capoconti
022600960531     D TCUDS           DS
022700960531     D  F34                    3      4
022800960531     D  F56                    5      6
022900961028     D DS1B          E DS
023000960822     D DS15          E DS
023100960527     D DSCV          E DS
023200070109      **
023300000120     D TRTC86DS      E DS
023400070109      **
023500000120     D EDIDSPT       E DS
023600000120     D EDIDSPU       E DS
023700070123     D EDIDSPV       E DS
023800070123      **
023900000120     D EDIDSCL       E DS
024000000120     D EDIDSSS       E DS
024100070109      **
024200020909      *  DS x salvataggio dati impostati in EDiVAB
024300020909     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
024400960924     D SALVA           DS           545
024500960528     D WLBDA8          DS
024600960528     D  G02DAT                 1      8  0
024700960528     D  G02INV                 9     16  0
024800960528     D  G02ERR                17     17
024900960528     D  G02TGI                18     22  0
025000000120      *  DS x scomposizione numero spedizione
025100960530     D DSSPE           DS
025200960530     D  SPEAAS                 1      4  0
025300960530     D  SPELNP                 5      7  0
025400960530     D  SPENRS                 8      9  0
025500960530     D  SPENSP                10     16  0
025600000120      *  DS x scomposizione segnacollo
025700960528     D DSSGN           DS
025800960528     D  SGNLNP                 1      3  0
025900960528     D  SGNLNA                 4      6  0
026000960528     D  SGNNRS                 7      8  0
026100960528     D  SGNNSC                 9     15  0
026200960829     D  SGNZNC                16     17  0
026300000120      *  DS x stampa segnacollo
026400960628     D                 DS
026500960628     D  SGN1                   1     35
026600960628     D  SGN2                  36     70
026700960628     D  SGN3                  71    105
026800000120     D  SGT                    1    105    DIM(3)
026900000120      *---------------                                                      *
027000000120     D W035A           s             35
027100070123     D Wvabdcr         s                   LIKE(vabdcr)
027200000125     D Wvablnp         s                   LIKE(vablnp)
027300040227     D Wvabnrs         s                   LIKE(vabnrs)
027400040227     D Wvabctm         s                   LIKE(vabctm)
027500060612     D Wvabtic         s                   LIKE(vabtic)
027600070125     D Wvabpkb         s                   LIKE(vabpkb)
027700070125     D Wvabvlb         s                   LIKE(vabvlb)
027800070125     D Wvabncl         s                   LIKE(vabncl)
027900050112     D NUM_Sped        s                   LIKE(vabnsp)
028000041214     D SvKPJBU         s                   LIKE(KPJBU)
028100070131     D WvabRSD         s                   LIKE(vabRSD)
028200070131     D WvabRD2         s                   LIKE(vabRD2)
028300070131     D WvabIND         s                   LIKE(vabIND)
028400070131     D WvabLOD         s                   LIKE(vabLOD)
028500070131     D WvabNZD         s                   LIKE(vabNZD)
028600070131     D WvabRMO         s                   LIKE(vabRMO)
028700070206     D SUvabRMO        s                   LIKE(vabRMO)
028800070131     D WvabCMO         s                   LIKE(vabCMO)
028900070131     D WvabNMO         s                   LIKE(vabNMO)
029000070205     D Wvatrde         s                   LIKE(watrde)
029100070205     D Wvattel         s                   LIKE(wattel)
029200071128     D wCNIsalvatoRMN  s                   LIKE(vabRMN)
029300070122     C*
029400070122     C* Definizione campi di work
029500070122     D pVLB            s                   LIKE(VABVLB)
029600070122     D kKUT            s                   LIKE(TBLKUT)
029700070122     D kCOD            s                   LIKE(TBLCOD)
029800070122     D kKEY            s                   LIKE(TBLKEY)
029900070122     D kKCC            s                   LIKE(ACOKCC)
030000070122     D kKSC            s                   LIKE(ACOKSC)
030100070122     D kAAA            s                   LIKE(NUFAAA)
030200070122     D kCNU            s                   LIKE(NUFCNU)
030300070122     D kFIL            s                   LIKE(NUFFIL)
030400070122     D kAAS            s                   LIKE(VABAAS)
030500070122     D kLNP            s                   LIKE(VABLNP)
030600070122     D kNRS            s                   LIKE(VABNRS)
030700070122     D kNSP            s                   LIKE(VABNSP)
030800070122     D kCMR            s                   LIKE(VABCMR)
030900070122     D kCCM            s                   LIKE(VABCCM)
031000070122     D kNCD            s                   LIKE(VADNCD)
031100070122     D kCNT            s                   LIKE(VADCNT)
031200070122     D kNMC            s                   LIKE(RDENMC)
031300070122     D kNRP            s                   LIKE(RDENRP)
031400070122     D kLNP1           s                   LIKE(RDELNP)
031500070122     D WCODPT          s                   LIKE(T000004)
031600070122     D WQUAPT          s                   LIKE(T000007)
031700080716     D trovato_TPI     s              1
031800070122      *
031900070123      *  Invio E-mail
032000070202     D Indirizzi       s            253
032100070122     D Oggetto         s             44
032200070122     D Messaggio       s           5000
032300070123      *
032400070205     d   DSmail        ds
032500070205     D Mail_Ind                      44
032600070202     d   IND_char                     1    dim(44)
032700070205      *
032800070202     D Lun_Ind         s              5u 0
032900070123      *
033000070205     D Dac             s              5u 0
033100070205     D Luc             s              5u 0
033200070205      *
033300070104      * ?---------------------------------------------------------------------*
033400040227     D MsgDSP          S            256                                         Testo generico
033500070104      * ?---------------------------------------------------------------------*
033600040227     D SndMg01         S             10                                         Message type
033700040227     D                                     INZ('*INFO')
033800040227     D SndMg02         S             10                                         Deluvery mode
033900040227     D                                     INZ('*BREAK')
034000040227     D SndMg03         S            256                                         Message text
034100040227     D SndMg04         S             10I 0                                      Length of text
034200040227     D                                     INZ(%SIZE(SndMg03))
034300040227     D SndMg05         S             10                                         User profile
034400040227     D                                     DIM(299)
034500040227     D SndMg06         S             10I 0                                      Number of user
034600040227     D SndMg07         S             10I 0                                      Message sent indic.
034700040227     D SndMg08         S             10I 0                                      Function requested
034800040227     D SndMg10         S              1                                         Show display
034900040227     D                                     INZ('N')
035000040227     D SndMg11         S             20                                         Qualified MSGQ name
035100040227     D SndMg12         S              4                                         Name type indicator
035200040227     D                                     INZ('*USR')
035300040227      * indice di scihera
035400040227     D Jx              S              5I 0
035500040227      *
035600040227     D/COPY QSYSINC/QRPGLESRC,QUSEC
035700070123      *
035800070202     d bart_it         C                   CONST('@Bartolini.it;')
035900070202     d CED_Bart        C                   CONST('CED@Bartolini.it;')
036000000120      *---------------------------------------------------------------------*
036100960711     IRRIFCSUM
036200960524     I              RRIFCSUM                    SUMDAT
036300000120      *---------------------------------------------------------------------*
036400000120      * Ciclo principale
036500000120      *---------------------------------------------------------------------*
036600061116     **
036700960329      * Posizionamento iniziale sul file
036800960329     C                   EXSR      REDFIL
036900040227      *
037000961016      * Controllo che primo record letto sia TT00
037100961016     C     SUMTPR        IFNE      'TT00'
037200961212     C     WFINE         ANDNE     'S'                                          - Fine file -
037300070122      *
037400070122      * Msg di allerta Traduzione
037500070202     C                   EVAL      Indirizzi = CED_BART
037600070122     C                   EVAL      Oggetto ='Problemi ricez. EDI Bolle import -
037700070122     C                             Vers.D96A'
037800070122     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
037900070122     C                             IFCSUM Bolle import in filiale - messaggio -
038000070122     C                             incompleto. Verificare la ricezione sul serv-
038100070122     C                             er EDI e rieseguire la traduzione'
038200070122     c                   exsr      invio_MAIL
038300961016     C                   GOTO      FINE
038400961016     C                   END
038500980609      *
038600960329      * Lettura fino a fine file
038700960329     C     WFINE         DOWNE     'S'                                          - Fine file -
038800070123      *
038900070123      * ?Pulisco campi di importazione dati - tutte le DS
039000960527     C                   EXSR      AZZCMP
039100980609      *
039200980609      * Controllo tipo record letto per decodifica solo se trovata la tabella PT
039300980609     C                   SELECT
039400070123      * ?Testata
039500070123      * ? UNB-BGM
039600980609     C     SUMTPR        WHENEQ    'TT00'
039700070123     C                   EXSR      DRETT0
039800070123      * ? DTM
039900070108     C     SUMTPR        WHENEQ    'TT01'
040000070115     C                   EXSR      DRET01
040100070123      * ? FTX
040200980609     C     SUMTPR        WHENEQ    'TT05'
040300040227     C     PT_ok         ANDEQ     'S'
040400980609     C                   EXSR      DRET05
040500070123      * ? CNT
040600980609     C     SUMTPR        WHENEQ    'TT10'
040700040227     C     PT_ok         ANDEQ     'S'
040800980609     C                   EXSR      DRET10
040900070123      * ? RFF-DTM
041000980609     C     SUMTPR        WHENEQ    'TT15'
041100040227     C     PT_ok         ANDEQ     'S'
041200980609     C                   EXSR      DRET15
041300070123      * ? TDT-LOC-DTM
041400980609     C     SUMTPR        WHENEQ    'TT20'
041500040227     C     PT_ok         ANDEQ     'S'
041600980609     C                   EXSR      DRET20
041700070123      * ? TSR
041800070108     C     SUMTPR        WHENEQ    'TT21'
041900070115     C                   EXSR      DRET21
042000070123      * ? SEL
042100070108     C     SUMTPR        WHENEQ    'TT22'
042200070115     C                   EXSR      DRET22
042300070123      * ? NAD-CTA-COM
042400980609     C     SUMTPR        WHENEQ    'TT25'
042500040227     C     PT_ok         ANDEQ     'S'
042600980609     C                   EXSR      DRET25
042700070123      * ? DOC-DTM
042800070108     C     SUMTPR        WHENEQ    'TT26'
042900070115     C                   EXSR      DRET26
043000070123      * ? TCC
043100070108     C     SUMTPR        WHENEQ    'TT27'
043200070115     C                   EXSR      DRET27
043300070123      * ? MOA
043400070108     C     SUMTPR        WHENEQ    'TT28'
043500070115     C                   EXSR      DRET28
043600070123      * ? EQD-EQN-SEL
043700980609     C     SUMTPR        WHENEQ    'TT30'
043800040227     C     PT_ok         ANDEQ     'S'
043900980609     C                   EXSR      DRET30
044000070123      * ? NAD
044100070108     C     SUMTPR        WHENEQ    'TT31'
044200070115     C                   EXSR      DRET31
044300070123      * ?Dettaglio
044400070123      * ? CNI-DTM-TSR-MOA
044500980609     C     SUMTPR        WHENEQ    'TD00'
044600040227     C     PT_ok         ANDEQ     'S'
044700980609     C                   EXSR      DRED00
044800070123      * ? CNT
044900070108     C     SUMTPR        WHENEQ    'TD01'
045000070115     C                   EXSR      DRED01
045100070123      * ? FTX
045200980609     C     SUMTPR        WHENEQ    'TD05'
045300040227     C     PT_ok         ANDEQ     'S'
045400980609     C                   EXSR      DRED05
045500070123      * ? TOD-LOC-RFF
045600980609     C     SUMTPR        WHENEQ    'TD10'
045700040227     C     PT_ok         ANDEQ     'S'
045800980609     C                   EXSR      DRED10
045900070123      * ? GOR
046000070108     C     SUMTPR        WHENEQ    'TD12'
046100070115     C                   EXSR      DRED12
046200070123      * ? DOC-DTM
046300070108     C     SUMTPR        WHENEQ    'TD13'
046400070115     C                   EXSR      DRED13
046500070123      * ? NAD-CTA-COM
046600980609     C     SUMTPR        WHENEQ    'TD15'
046700040227     C     PT_ok         ANDEQ     'S'
046800980609     C                   EXSR      DRED15
046900070123      * ? LOC
047000070108     C     SUMTPR        WHENEQ    'TD16'
047100070115     C                   EXSR      DRED16
047200070123      * ? DOC
047300070108     C     SUMTPR        WHENEQ    'TD17'
047400070115     C                   EXSR      DRED17
047500070123      * ? TCC
047600070108     C     SUMTPR        WHENEQ    'TD18'
047700070115     C                   EXSR      DRED18
047800070123      * ? MOA
047900070108     C     SUMTPR        WHENEQ    'TD19'
048000070115     C                   EXSR      DRED19
048100070123      * ? RFF
048200070108     C     SUMTPR        WHENEQ    'TD1A'
048300070115     C                   EXSR      DRED1A
048400070123      * ? GID-TMP-PIA-FTX
048500980609     C     SUMTPR        WHENEQ    'TD20'
048600040227     C     PT_ok         ANDEQ     'S'
048700980609     C                   EXSR      DRED20
048800070123      * ? HAN-RNG
048900070108     C     SUMTPR        WHENEQ    'TD22'
049000070115     C                   EXSR      DRED22
049100070123      * ? MEA
049200980609     C     SUMTPR        WHENEQ    'TD25'
049300040227     C     PT_ok         ANDEQ     'S'
049400980609     C                   EXSR      DRED25
049500070123      * ? DIM
049600070108     C     SUMTPR        WHENEQ    'TD26'
049700070115     C                   EXSR      DRED26
049800070123      * ? RFF
049900070108     C     SUMTPR        WHENEQ    'TD27'
050000070115     C                   EXSR      DRED27
050100070123      * ? DTM
050200070108     C     SUMTPR        WHENEQ    'TD28'
050300070115     C                   EXSR      DRED28
050400070123      * ? PCI
050500980609     C     SUMTPR        WHENEQ    'TD30'
050600040227     C     PT_ok         ANDEQ     'S'
050700980609     C                   EXSR      DRED30
050800070123      * ? RFF
050900070108     C     SUMTPR        WHENEQ    'TD31'
051000070115     C                   EXSR      DRED31
051100070123      * ? GIN
051200070108     C     SUMTPR        WHENEQ    'TD32'
051300070115     C                   EXSR      DRED32
051400070123      * ? DGS-FTX-MEA
051500070108     C     SUMTPR        WHENEQ    'TD35'
051600070115     C                   EXSR      DRED35
051700070123      * ? CTA
051800070108     C     SUMTPR        WHENEQ    'TD36'
051900070115     C                   EXSR      DRED36
052000070123      * ? COM
052100070108     C     SUMTPR        WHENEQ    'TD37'
052200070115     C                   EXSR      DRED37
052300070123      * ? EQD-EQN
052400070108     C     SUMTPR        WHENEQ    'TD40'
052500070115     C                   EXSR      DRED40
052600070123      * ? TMD
052700070108     C     SUMTPR        WHENEQ    'TD41'
052800070115     C                   EXSR      DRED41
052900070123      * ? MEA
053000070108     C     SUMTPR        WHENEQ    'TD42'
053100070115     C                   EXSR      DRED42
053200070123      * ? NAD
053300070108     C     SUMTPR        WHENEQ    'TD43'
053400070115     C                   EXSR      DRED43
053500980609      *
053600980609     C                   ENDSL
053700980609      *
053800980609      * Cancello record, tranne per tabella PT non trovata
053900040227     C     PT_ok         IFeq      'S'
054000960524     C                   DELETE    EDIFCSUM
054100980609     C                   END
054200980609      *
054300960329      * Lettura successiva
054400960329     C                   EXSR      REDFIL
054500070122     C                   ENDdo                                                  - Fine file -
054600980609      *
054700980609      * Se tabella PT non trovata vado a fine
054800040227     C     PT_ok         CABEQ     'N'           FINE
054900960329     C*
055000070215     C* se abilitato
055100070215     C     WRTVAB        IFEQ      'S'
055200070214      * ?- Scrivo Ultima bolla     - *
055300070214      * ?- e aggiorna i Files      ------------ *
055400070214     c                   exsr      Aggiorna_DB
055500070214      * ?            * ------------------------ *
055600070215     c                   end
055700040227      *
055800960924     C*  Se non è la prima volta
055900960925     C     WPRIMA        IFEQ      'N'
056000070215      *
056100070215     C*  ULTIMA CLOSE SYSPRT
056200070215      *   a Fine chiude e basta
056300070215     C                   CLOSE     SYSPRT
056400070215     C*
056500070215      * ?- Passa i records da EDIVA  a FIVAB - *
056600070215      * ?- e aggiorna i Files      ------------ *
056700070215     c                   exsr      Write_FIVAB
056800070215      * ?            * ------------------------ *
056900070215      *
057000961204     C                   END
057100070215     C*
057200960703     C*  Controllo quadratura:
057300960703     C     WTOTCO        IFNE      WTOCOC
057400960703     C     WTOTCO        ANDNE     0
057500960703     C     WTOTSP        ORNE      WTOSPC
057600960703     C     WTOTSP        ANDNE     0
057700960926     C                   Z-ADD     WTOTCO        WCOL              6 0
057800960926     C                   Z-ADD     WTOCOC        WCOC              6 0
057900960926     C                   Z-ADD     WTOTSP        WSPM              6 0
058000960926     C                   Z-ADD     WTOSPC        WSPC              6 0
058100960926     C     §PTCMR        IFEQ      'S'
058200960926     C                   MOVEL     WNRCMR        WNRDOC           35
058300960926     C                   ELSE
058400960926     C                   MOVEL     WNUMFV        WNRDOC           35
058500960926     C                   END
058600960926     C   OF              EXCEPT    TESTA
058700960926     C                   EXCEPT    ERRQUA
058800040416     c                   move      'S'           errori_prtf
058900960703     C                   END
059000070109
059100960531     C*  Stampa riga finale
059200960731     C     FINE          TAG
059300040416     c                   if        errori_prtf = 'S'
059400960531     C   OF              EXCEPT    TESTA
059500960527     C                   EXCEPT    STAFIN
059600040416     c                   end
059700070215      *
059800980114     C* Imposto i dati in LV55
059900000120     C                   CLEAR                   FNLV55DS
060000980114     C                   MOVEL     'L'           D55TLA
060100980114     C                   CALL      'FNLV55R'
060200000120     C                   PARM                    FNLV55DS
060300070109
060400960329     C                   SETON                                        LR
060500960524     C*----------------------------------------------------------------
060600960524     C*? REDFIL - LETTURA FILE
060700960524     C*----------------------------------------------------------------
060800960524     C     REDFIL        BEGSR
060900960524     C*
061000960524     C* Lettura file
061100960528     C                   READ      EDIFCSUM                               30
061200960527     C                   MOVEL     SUMDAT        WDAT
061300960524     C     *IN30         IFEQ      '1'
061400960524     C                   MOVEL     'S'           WFINE
061500960524     C                   END
061600960524     C*
061700960524     C                   ENDSR
061800960524     C*----------------------------------------------------------------
061900070214     C*? Aggiorna i files
062000070214     C*----------------------------------------------------------------
062100070214     C     Aggiorna_DB   BEGSR
062200070214     C*
062300070214      * ?- Scrivo bolla precedente - *
062400070214      *
062500070214     C*  Stampo dati colli
062600070214     C     XW            IFLT      3                                            --- 02 ---
062700070214     C     XW            ANDGT     0                                            --- 02 ---
062800070214     c                   If        In_Stampa ='S'
062900070214     C                   EXCEPT    DATSGN
063000070214     c                   end
063100070214     C                   Z-ADD     0             XW
063200070214     C                   CLEAR                   SGT
063300070214     C                   END                                                    --- 02 ---
063400070214     c                   If        In_Stampa ='S'
063500070214     C   OA              EXCEPT    TEST1
063600070214     C                   EXCEPT    DATCOL
063700070214     c                   end
063800070214      *
063900070215     C*  Prima di passare ad esaminare il dettaglio succ. scrivo dati
064000070214     C*  VAB/VAD in sospeso
064100070214     C                   EXSR      UPDVAB
064200070214      *
064300070214     C                   EXSR      AZZBOL
064400070214      *
064500070214      * ?- Fine bolla precedente -*
064600070214     C*
064700070214     C                   ENDSR
064800070215     C*----------------------------------------------------------------
064900070215     C*? DRETT0 - DECODIFICA TIPO RECORD TTT0
065000070214     C*----------------------------------------------------------------
065100070215     C     Write_FIVAB   BEGSR
065200070215     C*
065300070215     C*  Richiamo pgm per scrittura immediata VAB
065400070215     C     §PUWRK        IFEQ      ' '
065500070215      *
065600070215     C*  Richiamo pgm per esecuzione stampa
065700070215     C     §PTCMR        IFEQ      'S'
065800070215     C                   MOVEL     WNRCMR        D86CMR
065900070215     C                   ELSE
066000070215     C                   MOVEL     WNUMFV        D86CMR
066100070215     C                   END
066200070215      *
066300070215     C                   MOVEL     ' '           D86RIE
066400070215     C                   MOVEL     §PUDTA        D86DTA
066500070215     C                   Z-ADD     WPROG         D86CNT
066600070215     C                   Z-ADD     §PTKSC        D86CCM
066700070215     C                   MOVEL     *BLANKS       D86STA
066800070215     C                   MOVEL     'E'           D86MOD
066900070215      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
067000070215      * dove poi imposta la DS dei parametri passati
067100070215     C                   MOVEL     'LR'          D86CHI
067200070215     C                   MOVEL     'N'           D86CTL
067300070215      *
067400070215     c                   move      kpjbu         SvKpjbu
067500070215     c                   clear                   kpjbu
067600070215     C                   MOVEL     TRTC86DS      KPJBU
067700070215      * ?- Chiamata la TRTC86R2 -*
067800070215     C                   CALL      'TRTC86R2'
067900070215     C                   PARM                    KPJBA
068000070215     c                   move      Svkpjbu       Kpjbu
068100070215      *
068200070215     C*  Controllo pgm per scrittura immediata VAB
068300070215     C     WW            IFNE      0
068400070215     C                   DO        WW            WX                3 0
068500070215     C                   Z-ADD     KCL(WX)       D86CCM
068600070215      *
068700070215     c                   move      kpjbu         SvKpjbu
068800070215     c                   clear                   kpjbu
068900070215     C                   MOVEL     TRTC86DS      KPJBU
069000070215      * ?- Chiamata la TRTC86R2 -*
069100070215     C                   CALL      'TRTC86R2'
069200070215     C                   PARM                    KPJBA
069300070215     c                   move      Svkpjbu       Kpjbu
069400070215     C                   END
069500070215     C                   END
069600070215      *
069700070215     C                   END
069800070215     C*
069900070215     C                   ENDSR
070000070215     C*----------------------------------------------------------------
070100960524     C*? DRETT0 - DECODIFICA TIPO RECORD TTT0
070200960524     C*----------------------------------------------------------------
070300960524     C     DRETT0        BEGSR
070400960524     C*
070500070108     C                   MOVEL     SUMDAT        ED6T00DS
070600070215     C* se abilitato
070700070215     C     WRTVAB        IFEQ      'S'
070800070214      * ?- Scrivo bolla precedente - *
070900070214      * ?- e aggiorna i Files      ------------ *
071000070214     c                   exsr      Aggiorna_DB
071100070214      * ?            * ------------------------ *
071200070215     C                   MOVEL     'N'           WRTVAB            1
071300070215     c                   END
071400070108      *
071500960924     C*  Se non è la prima volta
071600960925     C     WPRIMA        IFEQ      'N'
071700070215      *
071800070215      *   a Cambio di Partner chiude e riapre
071900070215     C     WCODPT        IFNE      T000004
072000070215     C     WQUAPT        ORNE      T000007
072100070215     C                   CLOSE     SYSPRT
072200070215     C                   OPEN      SYSPRT
072300070215     C                   END
072400070215     C*
072500070215      * ?- Passa i records da EDIVA  a FIVAB - *
072600070215      * ?- e aggiorna i Files      ------------ *
072700070215     c                   exsr      Write_FIVAB
072800070215      * ?            * ------------------------ *
072900070215      *
073000960924     C*  Se è la prima volta modifico variabile
073100960924     C                   ELSE
073200960924     C                   MOVEL     'N'           WPRIMA            1
073300960924     C                   END
073400070109
073500960628     C*  Inizio messaggio: azzero dati
073600960628     C                   EXSR      AZZMSG
073700070115
073800000120      *  Se è stato modificato il codice partner controllo se è tabellato
073900000120     C                   eval      *in04 = *on
074000040301      *
074100070108     C                   IF        WCODPT <> T000004 or  WQUAPT <> T000007
074200070108     C                   MOVEL     T000004       WCODPT
074300070108     C                   MOVEL     T000007       WQUAPT
074400000120      * Primo tentativo rep. da codice + qualificatore
074500070108     C                   MOVEL     T000004       WCOD             35
074600070108     C                   MOVE      T000007       WCOD
074700000224     C                   Z-ADD     1             XX                3 0
074800961021     C     WCOD          LOOKUP    CPT(XX)                                32
074900040301      *
075000040311     C                   If        *IN32 = *off
075100000224     C                   eval      XX = 1
075200070219     c                   clear                   w035a
075300070108     C                   eval      W035A = %subst(T000004:1:31)
075400000224     C     W035A         lookup    CPTparz(XX)                            32
075500040301      *
075600070219      * ?se con il codice parziale è stato trovato un codice Partner occorre tramite
075700070219      * ?routine esterna reperire la nazione del primo mittente di dettaglio valido
075800070219      * ?per cercare di individuare con certezza il codice della tabella PT in base
075900070219      * ?all'UNB specifico. (es.: A02YR    ES/PT)
076000040301     C                   If        *IN32 = *on
076100070219      *
076200070219      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
076300070219      * ?a decodificare il mittente per attribuire la giusta linea/cliente
076400070219      * ?  su Partner che hanno + linee con + nazioni.
076500070219     c                   eval      Ptn_parziale = 'S'
076600070219      *
076700070219      *  Salva il codice generico se è considerato Parziale
076800070219     c                   eval      UNB_Generico = W035A
076900070219      *
077000040301     c                   z-add     nrrec         rec_pos           9 0
077100040301     c                   move      *blank        Naz_mitt          3
077200040302     c                   move      'N'           OK_Naz            1
077300040302      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
077400070104     c                   call      'TRTC79R1'
077500040302     c                   parm                    rec_pos
077600040302     c                   parm                    Naz_mitt
077700040302     c                   parm                    Ok_Naz
077800040322     c                   parm                    User_msg
077900040310      *  Re-inizializza l'indicatore di trovato
078000040310     c                   setoff                                       32
078100040310      *
078200040302      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
078300040302      *   da decodificare come UNB.
078400040302     c                   if        Ok_Naz = 'S'
078500040302     c                   clear                   W035A
078600070108     C                   eval      W035A = %subst(T000004:1:31) + Naz_mitt
078700040302     C                   eval      XX = 1
078800040302     C     W035a         lookup    CPT(XX)                                32
078900070219      *
079000070219      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
079100070219      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
079200070219      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
079300070219      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
079400070219     c   32              if        Ptn_parziale = 'S'
079500070219      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
079600070219     c                   movel     w035a         UNB_Parz
079700070219     C                   z-add     XX            savXX             3 0
079800070219     c                   End
079900070219      *
080000040310     c                   Endif
080100040301      *
080200040301     c                   Endif
080300040301     c                   Endif
080400980707      *
080500000120      *  OPERAZIONI PER TABELLA PARTNER TROVATA
080600000120     C     *IN32         IFEQ      '1'
080700040227     C                   move      'S'           PT_ok
080800070215      * ?           *------------------------------*
080900000120     C                   EXSR      PARTNEROK
081000070215      * ?           *------------------------------*
081100960524     C                   ELSE
081200040227      *
081300040227      * Non ha trovato la PT ???????
081400000120     C                   clear                   EDIDSPT
081500000120     C                   clear                   EDIDSPU
081600070123     C                   clear                   EDIDSPV
081700040227     C                   move      'N'           PT_ok             1
081800980609     C                   Z-ADD     1             WINDER            3 0
081900070108     C                   MOVEL(P)  T000004       WVALOR           35
082000960731     C                   SETOFF                                       05
082100960524     C                   EXSR      STPERR
082200070123      *
082300070123      * Msg di allerta Traduzione
082400070202     C                   EVAL      Indirizzi = CED_Bart
082500070123     C                   EVAL      Oggetto ='Problemi ricez. EDI Bolle import -
082600070123     C                             Vers.D96A'
082700070123     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
082800070123     C                             IFCSUM Bolle import in filiale - messaggio -
082900070123     C                             con UNB non presente in tabella PT controlla-
083000070123     C                             re EDI ricevuto.'
083100070123     c                   exsr      invio_mail
083200070123     C                   GOTO      End_TT00
083300960524     C                   END
083400040227      *
083500960524     C                   END
083600040227      *
083700960702     C*  Stampo dati inizio messaggio
083800040607     c                   If        In_Stampa ='S'
083900960702     C   OA              EXCEPT    TEST1
084000040607     c                   end
084100070108     C                   MOVEL     T000017       WAA4              4 0
084200960725     C                   MOVE      WAA4          WAA2              2 0
084300070108     C                   MOVE      T000017       WGG               2 0
084400070108     C                   MOVE      T000017       WDTPRE            8 0
084500070108     C                   MOVE      T000019       WHMPRE            4 0
084600070109     C                   MOVE      T000017       WDTMSG            6 0
084700960712     C                   MOVE      WAA2          WDTMSG
084800960712     C                   MOVEL     WGG           WDTMSG
084900040607     c                   If        In_Stampa ='S'
085000960702     C                   EXCEPT    DATMSG
085100040607     c                   end
085200980707      *
085300010503      * Pulizia flag di decodifica in testata se arrivato RFF+FF in testata
085400010503     C                   clear                   WFFT             35
085500010503      *
085600070123     C     End_TT00      ENDSR
085700000120     C*----------------------------------------------------------------
085800000120      *? OPERAZIONI PER TABELLA PARTNER TROVATA
085900000120     C*----------------------------------------------------------------
086000000125     C     PARTNEROK     BEGSR
086100000120      *
086200000120     C                   MOVEL     DPT(XX)       EDIDSPT
086300000120     C                   MOVEL     DPU(XX)       EDIDSPU
086400070123     C                   MOVEL     DPV(XX)       EDIDSPV
086500070201      *  Campi impostati come default:
086600070201      *  Se ho AGE/ON.... prendo i riferimenti definito su tab.PT il tipo di
086700070201      *  qualificatore da utilizzare come facciamo x RFF+AGE con EEX.
086800070201      *  Per default se non impostato diventa automaticamente un AGE.
086900070201     c                   if        §PVRFF = *blank
087000070201     c                   eval      §PVRFF = 'AGE'
087100070201     c                   end
087200070201      *  Anche per il numerico prendo come default "CU"
087300070201     c                   if        §PVRCU = *blank
087400070201     c                   eval      §PVRCU = 'CU '
087500070201     c                   end
087600050301      *
087700070206      *  Default per i campi Free Text nel dettaglio 'SIN' e 'ICN'
087800070206     c                   if        §PVsin = *blank
087900070206     c                   eval      §PVsin = 'SIN'
088000070206     c                   end
088100070206     c                   if        §PVicn = *blank
088200070206     c                   eval      §PVicn = 'ICN'
088300070206     c                   end
088400070206      *
088500070202      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
088600070123      *  ricevuti:
088700070123     c                   movel     §PVema        mail_ind         44
088800070123      *
088900070202      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
089000070123     c                   if        mail_ind <> *blank
089100070123     c                   exsr      imp_ADDR_mail
089200070123     c                   end
089300070123      *
089400050301      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
089500050301      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
089600050301      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
089700050301     C                   if        §PULUN = *blank
089800050301     C                   eval      §PULUN = '00'
089900050301     c                   end
090000070123      *
090100070213      * ?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ------ */
090200050301      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
090300050301     C                   move      §puLUN        WVATlun           2 0          *0/>0
090400050301      *
090500020221     C                   Z-ADD     §PTKSC        WKSC              7 0
090600000125     C                   MOVEL     §PTLNP        WVABlnp
090700040227     C                   MOVEL     §PTNRS        WVABnrs
090800040227     C                   MOVEL     §PTCTM        WVABctm
090900050223      *
091000050223      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
091100050223      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
091200050223      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
091300050223      *  i records trasmessi).
091400050223     C                   MOVEL     §puNSP        WVABfnsp          1            *blk/S
091500000125      *
091600000125      *  In base al cod.trattamento merce reperisco tipo tratt.
091700000120     C                   CLEAR                   DS1B
091800000120     C                   Z-ADD     1             Y                 3 0
091900040227     C     Wvabctm       LOOKUP    CTM(Y)                                 32
092000000120     C     *IN32         IFEQ      '1'
092100000120     C                   MOVEL     DTM(Y)        DS1B
092200000125     C                   MOVEL     DS1B          WSVTRM           38
092300000120     C                   END
092400000125      *
092500000125      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
092600000125     C                   clear                   ACORAG
092700000125     C                   clear                   INDCAE
092800000125     C                   clear                   INDCAP
092900000125     C                   clear                   INDSTA
093000000125      *
093100000125      * Decodifico partner
093200000120     C                   Z-ADD     1             KKUT
093300000120     C                   Z-ADD     KCI           KKCC
093400000120     C                   MOVE      §PTKSC        KKSC
093500000120     C     KACO          CHAIN     CNACO00F                           31
093600000120     C     KIND          CHAIN     CNIND00F                           31
093700000120      *
093800000120     C                   ENDSR
093900960524     C*----------------------------------------------------------------
094000070115     C*? DRET01 - DECODIFICA TIPO RECORD TT01
094100960524     C*----------------------------------------------------------------
094200070115     C     DRET01        BEGSR
094300960524     C*
094400070115     C                   MOVEL     SUMDAT        ED6T01DS
094500070115      *
094600070123      * Data Prevista Consegna per le spedizioni all'interno dei dettagli
094700070115     C     T152005       IFEQ      '2  '
094800070115     C                   MOVEL     T152380       WDATA            35            Data
094900070115     C                   MOVEL     T152379       WFORM             3            Formato
095000070115     C                   EXSR      CNVDAT
095100070115      *     Data Ora Documento
095200070123     C                   MOVEL     WCAMG         WvabDCR           8 0          Data Cons.Rich.
095300070123     C                   MOVEL     WHMS          WvabHCR           4 0          Ora  Cons.Rich.
095400070115      *
095500070115     C     WERRO         IFEQ      'S'
095600070115     C                   Z-ADD     5             WINDER
095700070115     C                   MOVEL     *BLANKS       WVALOR
095800070115     C                   MOVEL     T152380       WVALOR
095900070115     C                   SETOFF                                       05
096000070115     C                   EXSR      STPERR
096100070115     C                   END
096200070123      *
096300070115     C                   END
096400070115      *
096500070115     C                   ENDSR
096600070115     C*----------------------------------------------------------------
096700070115     C*? DRETT5 - DECODIFICA TIPO RECORD TT05
096800070115     C*----------------------------------------------------------------
096900070115     C     DRET05        BEGSR
097000070115     C*
097100070108     C                   MOVEL     SUMDAT        ED6T05DS
097200070125     C*
097300960628     C*  Stampo dati note intero messaggio
097400040607     c                   If        In_Stampa ='S'
097500960628     C   OA              EXCEPT    TEST1
097600960628     C                   EXCEPT    NOTMSG
097700040607     c                   end
097800070125     C*
097900960628     C                   DO        5             X
098000960702     C     T05(X)        IFNE      *BLANKS
098100960628     C                   MOVEL     T05(X)        WNOT
098200040607     c                   If        In_Stampa ='S'
098300960628     C                   EXCEPT    NOTMS1
098400040607     c                   end
098500960628     C                   END
098600960628     C                   END
098700960524     C*
098800960524     C                   ENDSR
098900960524     C*----------------------------------------------------------------
099000960524     C*? DRET10 - DECODIFICA TIPO RECORD TT10
099100960524     C*----------------------------------------------------------------
099200960524     C     DRET10        BEGSR
099300960524     C*
099400070108     C                   MOVEL     SUMDAT        ED6T10DS
099500070125     C*
099600960627     C*  Memorizzo totali x quadratura
099700960627     C                   DO        2             X
099800960702     C                   MOVEL     T10(X)        WTT10
099900960627     C     WCTR          IFEQ      '10 '
100000070215     C                   Z-ADD     WVLU          WTOTSP           16 0          Tot.sped.
100100960627     C                   END
100200960627     C     WCTR          IFEQ      '11 '
100300070215     C                   Z-ADD     WVLU          WTOTCO           16 0          Tot. colli
100400960627     C                   END
100500960627     C     WCTR          IFEQ      '7  '
100600070215     C                   Z-ADD     WVLU          WTOTPL           16 3
100700960627     C                   END
100800960702     C     WCTR          IFEQ      'ZCW'
100900070215     C                   Z-ADD     WVLU          WTOTPN           16 3
101000960627     C                   END
101100960627     C                   END
101200960524     C*
101300960524     C                   ENDSR
101400960524     C*----------------------------------------------------------------
101500960524     C*? DRET15 - DECODIFICA TIPO RECORD TT15
101600960524     C*----------------------------------------------------------------
101700960524     C     DRET15        BEGSR
101800960628     C*
101900070108     C                   MOVEL     SUMDAT        ED6T15DS
102000070125     C*
102100960624     C*  Se tipo documento CMR ('CMR') memorizzo numero e data
102200070108     C     T151153       IFEQ      'CMR'
102300070108     C                   MOVEL     T151154       WNRCMR           35
102400070109     C     T152005       IFEQ      '137'
102500070109     C                   MOVEL     T152380       WDATA            35            Data
102600070109     C                   MOVEL     T152379       WFORM             3            Formato
102700960524     C                   EXSR      CNVDAT
102800960624     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
102900960628     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
103000960628     C     WERRO         IFEQ      'S'
103100960628     C                   Z-ADD     2             WINDER
103200960628     C                   MOVEL     *BLANKS       WVALOR
103300070109     C                   MOVEL     T152380       WVALOR
103400960731     C                   SETOFF                                       05
103500960628     C                   EXSR      STPERR
103600960628     C                   END
103700960628     C                   END
103800960628     C*  Stampo dati CMR
103900960712     C                   MOVE      WDTCMR        G02INV
104000960712     C                   MOVEL     '3'           G02ERR
104100960712     C                   CALL      'XSRDA8'
104200960712     C                   PARM                    WLBDA8
104300960712     C                   Z-ADD     G02DAT        WSTCMR            8 0
104400040607     c                   If        In_Stampa ='S'
104500960628     C   OA              EXCEPT    TEST1
104600960628     C                   EXCEPT    CMRDOC
104700040607     c                   end
104800960524     C                   END
104900960628     C*
105000960628     C*  Se tipo documento AFB (Foglio viaggio) stampo numero e data
105100070109     C     T151153       IFEQ      'AFB'
105200070109     C                   MOVEL     T151154       WNUMFV           35
105300070109     C     T152005       IFEQ      '137'
105400070109     C                   MOVEL     T152380       WDATA            35            Data
105500070109     C                   MOVEL     T152379       WFORM             3            Formato
105600960628     C                   EXSR      CNVDAT
105700960702     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
105800960628     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
105900960628     C     WERRO         IFEQ      'S'
106000960702     C                   Z-ADD     21            WINDER
106100960628     C                   MOVEL     *BLANKS       WVALOR
106200070109     C                   MOVEL     T152380       WVALOR
106300960731     C                   SETOFF                                       05
106400960628     C                   EXSR      STPERR
106500960628     C                   END
106600960628     C                   END
106700960628     C*  Stampo dati AFB
106800960712     C                   MOVE      WDATFV        G02INV
106900960712     C                   MOVEL     '3'           G02ERR
107000960712     C                   CALL      'XSRDA8'
107100960712     C                   PARM                    WLBDA8
107200960712     C                   Z-ADD     G02DAT        WSTDFV            8 0
107300040607     c                   If        In_Stampa ='S'
107400960628     C   OA              EXCEPT    TEST1
107500960702     C                   EXCEPT    FVVDOC
107600040607     c                   end
107700960628     C                   END
107800980911      *
107900070125      * ?------------------------*
108000070219      *  Se tipo documento FF decodifico il codice cliente x tutto il messaggio
108100070109     C                   If        T151153 = 'FF '  and T151154 <> *blanks
108200070109     C                   MOVEL     T151154       WFFT             35
108300000125     C                   Endif
108400010503      *
108500010503     C                   IF        WFFT <> *BLANKS
108600010503     C                   z-add     1             XX
108700010503     C                   movel     §PTKSC        WCCL             35
108800010503     C                   movel     WFFT          WCOM28           28
108900010503     C                   move      WCOM28        WCCL
109000010503     C     WCCL          LOOKUP    CCL(XX)                                34
109100010503      *
109200070219      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
109300070219      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
109400070219      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
109500070219      * ?con i primi 31 caratteri dell'UNB.
109600070219     C  n34              if        PTN_parziale = 'S' and §puPLN = 'S'
109700070219     c                   do        sav_CPTx      x
109800070219      *
109900070219     c                   if        CPTparz(x) = UNB_generico
110000070219     C                   movel     KSC_UNB(x)    WCCL             35
110100070219     C                   movel     WFFT          WCOM28           28
110200070219     C                   move      WCOM28        WCCL
110300070219     C                   z-add     1             XX
110400070219     C     WCCL          LOOKUP    CCL(XX)                                34
110500070219      * se trovato esce immediatamente
110600070219     c   34              leave
110700070219     c                   end
110800070219      *
110900070219     C                   endDO
111000070219     c                   endIF
111100070219      *   Se trovato vale x tutto il messaggio
111200070219      *
111300010503     C     *IN34         IFEQ      '1'
111400010503     C                   MOVEL     DCL(XX)       EDIDSCL
111500040311     C                   z-add     §CLKSC        $CLKSC
111600040311     C                   movel     §CLTAR        $CLTAR
111700040311     C                   z-add     §CLLNP        $CLLNP
111800040311     C                   z-add     §CLNRS        $CLNRS
111900040311     C                   move      §CLCTM        $CLCTM
112000040311     C                   move      §CLBS1        $CLBS1
112100050223     C                   move      §CLnsp        $CLfnsp                        *blk/S
112200040311      * LNP NRS e CTM da FF
112300040311     c                   eval      wvablnp = §CLlnp
112400040311     c                   eval      wvabnrs = §CLnrs
112500040311     c                   eval      wvabctm = §CLctm
112600050223     c                   eval      WVABfnsp = §CLnsp                            *blk/S
112700070215      *
112800040311      *  In base al cod.trattamento merce reperisco tipo tratt.
112900040311     C                   CLEAR                   DS1B
113000040311     C                   Z-ADD     1             Y                 3 0
113100040311     C     Wvabctm       LOOKUP    CTM(Y)                                 32
113200040311     C     *IN32         IFEQ      '1'
113300040311     C                   MOVEL     DTM(Y)        DS1B
113400040311     C                   MOVEL     DS1B          WSVTRM           38
113500040311     C                   END
113600040311      *
113700010503     C     §CLKSC        LOOKUP    KCL                                    39
113800040311     c                   If        *in39 = *off
113900010503     C                   add       1             WW
114000010503     C                   z-add     §CLKSC        KCL(WW)
114100010503     C                   Endif
114200040311      *
114300010503     C                   ENDIF
114400010503      *
114500010503     C                   ENDIF
114600010503     C*--------------------------
114700980911      *
114800960524     C                   ENDSR
114900960524     C*----------------------------------------------------------------
115000960524     C*? DRET20 - DECODIFICA TIPO RECORD TT20
115100960524     C*----------------------------------------------------------------
115200960524     C     DRET20        BEGSR
115300960524     C*
115400070108     C                   MOVEL     SUMDAT        ED6T20DS
115500070125     C*
115600960628     C*  Stampo dati trasporto
115700040607     c                   If        In_Stampa ='S'
115800960628     C   OA              EXCEPT    TEST1
115900960628     C                   EXCEPT    DATTRA
116000040607     c                   end
116100070125     C*
116200960628     C*  Controllo se ci sono loc.partenza/arrivo
116300960628     C                   Z-ADD     1             X
116400960628     C                   DO        3             X
116500960712     C     T20(X)        IFNE      *BLANKS
116600960702     C                   MOVEL     T20(X)        WTT20
116700960628     C     WTPL          IFEQ      '5  '
116800960628     C                   MOVEL     WLOC          WLOCPA
116900960628     C                   END
117000960628     C     WTPL          IFEQ      '8  '
117100960628     C                   MOVEL     WLOC          WLOCAR
117200960628     C                   END
117300960628     C                   END
117400960712     C                   END
117500960628     C*  Data partenza
117600960628     C                   Z-ADD     1             X
117700070109     C                   DO        9             X
117800960712     C     T20D(X)       IFNE      *BLANKS
117900960702     C                   MOVEL     T20D(X)       WTT20D
118000960628     C     WTPD          IFEQ      '136'
118100960628     C                   MOVEL     WDTA          WDATA            35            Data
118200960628     C                   MOVEL     WFOR          WFORM             3            Formato
118300960628     C                   EXSR      CNVDAT
118400960628     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.
118500960913     C                   Z-ADD     WDATPA        G02INV
118600960913     C                   MOVEL     '3'           G02ERR
118700960913     C                   CALL      'XSRDA8'
118800960913     C                   PARM                    WLBDA8
118900960913     C                   Z-ADD     G02DAT        WSTDPA            8 0
119000960628     C                   MOVEL     WHMS          WHHPA             4 0          Ora  Part.
119100960628     C     WERRO         IFEQ      'S'
119200960702     C                   Z-ADD     9             WINDER
119300960628     C                   MOVEL     *BLANKS       WVALOR
119400070109     C                   MOVEL     T152380       WVALOR
119500960731     C                   SETOFF                                       05
119600960628     C                   EXSR      STPERR
119700960628     C                   END
119800960628     C                   END
119900960712     C                   END
120000960524     C*
120100960628     C                   END
120200960628     C*  Stampo dati luogo partenza/arrivo
120300960702     C     WTT20         IFNE      *BLANKS
120400960702     C     WTT20D        ORNE      *BLANKS
120500040607     c                   If        In_Stampa ='S'
120600960628     C   OA              EXCEPT    TEST1
120700960628     C                   EXCEPT    DATLOC
120800040607     c                   end
120900960628     C                   END
121000960628     C*
121100960524     C                   ENDSR
121200070115     C*----------------------------------------------------------------
121300070115     C*? DRET21 - DECODIFICA TIPO RECORD TT21
121400070115     C*-----------------------------------
121500070115     C     DRET21        BEGSR
121600070115     C*
121700070115     C                   MOVEL     SUMDAT        ED6T21DS
121800070125     C*
121900070125     C*  Reperisco priorità spedizione:
122000070125     C                   Z-ADD     1             Y
122100070125      ***
122200070125      *** attenzione se il cliente non codifica come gli standard prevedono,
122300070125      ***  occorre sulla PT indicarlo gestendo il codice in modo particolare
122400070125      ***
122500070125     c                   clear                   prior_testras     1
122600070125     c                   if        D004219 <> *blank
122700070125     c                   movel(p)  D004219       fld35a           35
122800070125     c                   move      §puTS         fld35a
122900070125     C     fld35a        LOOKUP    PTR35(Y)                               32
123000070125      *
123100070125     C     *IN32         IFEQ      '1'
123200070125     C                   MOVEL     TSP(Y)        WvabTSP
123300070125     c                   move      'S'           prior_testras
123400070125     C                   ELSE
123500070125     C                   Z-ADD     6             WINDER
123600070125     C                   MOVEL     *BLANKS       WVALOR
123700070125     C                   MOVEL     D004219       WVALOR
123800070125     C                   SETON                                        05
123900070125     C                   EXSR      STPERR
124000070125     C                   END
124100070125     c                   end
124200070115      *
124300070115     C                   ENDSR
124400070115     C*----------------------------------------------------------------
124500070115     C*? DRET22 - DECODIFICA TIPO RECORD TT22
124600070115     C*-----------------------------------
124700070115     C     DRET22        BEGSR
124800070115     C*
124900070115     C                   MOVEL     SUMDAT        ED6T22DS
125000070115      *
125100070125      *  Numero Sigillo non utilizzato: SEAL NUMBER
125200070125      *
125300070115     C                   ENDSR
125400960524     C*----------------------------------------------------------------
125500960524     C*? DRET25 - DECODIFICA TIPO RECORD TT25
125600960524     C*----------------------------------------------------------------
125700960524     C     DRET25        BEGSR
125800960524     C*
125900960926     C*  Reperisco ultimo numero progrressivo CMR x cliente/nr.cmr
126000960926     C                   Z-ADD     §PTKSC        KCCM
126100960926     C     §PTCMR        IFEQ      'S'
126200960926     C                   MOVEL     WNRCMR        KCMR
126300960926     C                   ELSE
126400960926     C                   MOVEL     WNUMFV        KCMR
126500960926     C                   END
126600960926     C                   Z-ADD     1             WPROG             5 0
126700960927     C                   MOVEL     SAVBOL        SALVA
126800020909     C     KVAB2         SETGT     EDiVAB1L
126900020909     C     KVAB2         READPE    EDiVAB1L                               30
127000960926     C     *IN30         IFEQ      '0'
127100960926     C     VABCNT        ADD       1             WPROG
127200960926     C                   END
127300960926     C*
127400960927     C                   MOVEL     SALVA         SAVBOL
127500960628     C* Se tot. spedizioni > 0  stampo dati (rec.TT25 obblig.)
127600960628     C     WTOTSP        IFNE      0
127700960628     C     WTOTCO        ORNE      0
127800960628     C     WTOTPL        ORNE      0
127900960628     C     WTOTPN        ORNE      0
128000960712     C     WSTPTO        IFEQ      'N'
128100040607     c                   If        In_Stampa ='S'
128200960628     C   OA              EXCEPT    TEST1
128300960628     C                   EXCEPT    TOTDOC
128400040607     c                   end
128500960712     C                   MOVEL     'S'           WSTPTO
128600960628     C                   END
128700960712     C                   END
128800070125      *
128900960628     C*  Imposto dati TT25
129000070108     C                   MOVEL     SUMDAT        ED6T25DS
129100960628     C*  Stampa
129200040607     c                   If        In_Stampa ='S'
129300960628     C   OA              EXCEPT    TEST1
129400070213     C     T253035       IFEQ      'SF '
129500960628     C                   EXCEPT    DATPAM
129600960628     C                   END
129700070213     C     T253035       IFEQ      'ST '
129800960628     C                   EXCEPT    DATPAD
129900960628     C                   END
130000040607     c                   end
130100070202      *
130200070126      * ?------------------------*
130300070205     C*  Name and Address di testata:
130400070126      * ?------------------------*
130500070202     c                   eval      *in04 = *off
130600070202     c                   eval      *in07 = *off
130700070202      * ?------------------------*
130800070202     C*  Name and Adderss di testata: se non impostato un destinatario in dettaglio
130900070202      * ?------------------------*
131000070202      * Dati destinatario
131100070213     c                   movel     T253035       Qua3035           2
131200070213     C                   IF        Qua3035= 'CN'   or
131300070213     C                             (§PTDES <> *BLANKS  and  §PTDES = Qua3035)
131400070202      *
131500070202     c                   move      'S'           NAD_CN_testa      1
131600070202     c                   eval      *in04 = *on
131700070202     c                   eval      *in07 = *on
131800070202      *
131900070202     C* Reperisco nazione corrispondente destinatario
132000070202     C                   Z-ADD     1             YY                3 0
132100070202     c                   eval      *in32 = *off
132200070202     C                   if        T253207 <> *BLANKS
132300070202     C     T253207       LOOKUP    ISO(YY)                                32
132400070202     C                   endif
132500070202      *
132600070202     C* Imposto dati destinatario
132700070202     C                   MOVEL     T253036       wVABRSD
132800070202     C                   if        T2530361 <> *LOVAL
132900070202     C                   MOVEL     T2530361      wVABRD2
133000070202     C                   endif
133100070202      *
133200070202     C                   MOVEL     T253042       wVABIND
133300070202     C                   MOVEL     T253164       wVABLOD
133400070202      *
133500070202     C* Se DD3042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
133600070202     c                   if        T2530421 <> *blanks  and T2530421 <> *loval
133700070206      *
133800070206      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
133900070206      *  altrimenti vado in coda alla località
134000070206     C     '    '        SCAN      wVABIND       WI                4 0
134100070206      *
134200070206     C* Verifico se c'è dello spazio per metterlo nella località
134300070206      *  oppure    accodato al primo indirizzo
134400070206      *
134500070206     c                   if        §pv2IN = 'S' and WI < 5
134600070206      *
134700070206     C     '    '        SCAN      wVABIND       WI                4 0
134800080115     C                   MOVEA     wVABIND       F35
134900070206     C                   ADD       1             WI
135000080115     C                   MOVEA     T2530421      F35(WI)
135100080115     C                   MOVEA     F35           wVABIND
135200070206      *
135300070206     c                   else
135400070206      *
135500070206     C     '    '        SCAN      wVABLOD       WI                4 0
135600080115     C                   MOVEA     wVABLOD       F35
135700070206     C                   ADD       1             WI
135800080115     C                   MOVEA     T2530421      F35(WI)
135900080115     C                   MOVEA     F35           wVABLOD
136000070206      *
136100070206     c                   end
136200070206      *
136300070202     C                   ENDif
136400070202      *
136500070202     C                   If        *in32 = *on
136600070202     C                   MOVEL     C15(YY)       wVABNZD
136700070202     C                   MOVEL     CPF(YY)       WFLCPF            2 0
136800070202     c                   Else
136900070202     C                   MOVEL     *BLANKS       wVABNZD
137000070202     C                   Z-ADD     0             WFLCPF
137100070202     c                   Endif
137200070202      * Converto CAP
137300070202     c                   movel(p)  T253251       CAP_9ALFA         9
137400070202     C                   EXSR      CNVCAP
137500070202     C                   ENDIF
137600070202      *
137700070202      * ?------------------------*
137800070205     C*  Name and Address di testata: se non impostato un mittente in dettaglio
137900070202      * ?------------------------*
138000070202      * Dati mittente
138100070213     C                   IF        Qua3035 = 'CZ'
138200070202      *
138300070202     c                   move      'S'           NAD_CZ_testa      1
138400070202      *
138500070202     C                   movel     T253036       wVABRMO
138600070202      *
138700070202      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
138800070202     C                   movel     §PTNAZ        wVABNMO
138900070202      *
139000070202     C                   IF        T253207 <> *blanks
139100070202     C                   eval      YY = 1
139200070202     C     T253207       LOOKUP    ISO(YY)                                32
139300070202     C                   if         *in32 = *on
139400070202     C                   MOVEL     C15(YY)       wVABNMO
139500070202     C                   endif
139600070202     C                   ENDIF
139700070202      *
139800070202      * Normalizzo CAP mittente originale
139900070202     C                   clear                   T
140000070202     C                   clear                   S
140100070202     C                   MOVEA     *BLANKS       CAPM
140200070202     C                   MOVEA     T253251       CAP9
140300070202     C                   DO        9             T
140400070202     C     CAP9(T)       IFNE      *BLANKS
140500070202     C                   ADD       1             S
140600070202     C                   MOVE      CAP9(T)       CAPM(S)
140700070202     C                   ENDIF
140800070202     C                   ENDDO
140900070202     C*
141000070202      *     Controllo validità CAP
141100070202     C                   MOVEA     CAPM          wVABCMO
141200070202      *
141300070202     C                   CLEAR                   TISI95DS
141400070202     C                   MOVEL     wVABCMO       I95CAP
141500070202     C                   MOVEL     wVABNMO       I95NAR
141600070202     C                   MOVEL     WOGGI         I95DAT
141700070202     C                   MOVEL     '3'           I95TCN
141800070202     C                   CALL      'TISI95R'
141900070202     C                   PARM                    TISI95DS
142000070202      *     Errore
142100070202     C     O95ERR        IFNE      *BLANKS
142200070202     C                   MOVEL     INDCAE        wVABCMO
142300070202     C                   MOVEL     §PTNAZ        wVABNMO
142400070202     C                   END
142500070202      *
142600070202     C                   END
142700070202      *
142800070202      *  Stampo dati soggetti transazione
142900070202     c                   If        In_Stampa ='S'
143000070202     C   OA              EXCEPT    TEST1
143100070202     C                   EXCEPT    SOGTRT
143200070202     c                   end
143300070202      *
143400070202      *  Controllo se inserito rivolgersi a ..
143500070202     C     T253139       IFNE      *BLANKS
143600070202     C     T253412       ORNE      *BLANKS
143700070202     c                   If        In_Stampa ='S'
143800070202     C   OA              EXCEPT    TEST1
143900070202     C                   EXCEPT    SOGRIT
144000070202     c                   end
144100070202     C                   END
144200960524     C*
144300070126      * ?------------------------*
144400070125     C*  Contatto e Comunicazione mittente: (Non utilizzato)
144500070126      * ?------------------------*
144600070202      *
144700070202      * Se ne prende uno solo poichè sul nostro D.B. ce n'è uno solo
144800070202      *  memorizza il primo riferimento di consegna x il destinatario
144900070202     C     T253412       ifne      *BLANKS
145000070528     c     wvatRDE       andeq     *blank
145100070205     c                   movel     T253412       wvatrde
145200070202     c                   end
145300070202      *
145400070202      * Se ne prende uno solo poichè sul nostro D.B. ce n'è uno solo
145500070202      *  memorizza il primo riferimento di consegna nr.telefonico
145600070202     C     T253148       ifne      *BLANKS
145700070528     c     wvatTEL       andeq     *blank
145800070205     c                   movel     T253148       wvattel
145900070202     c                   end
146000070125     C*
146100960524     C                   ENDSR
146200070115     C*----------------------------------------------------------------
146300070115     C*? DRET26 - DECODIFICA TIPO RECORD TT26
146400070115     C*-----------------------------------
146500070115     C     DRET26        BEGSR
146600070115     C*
146700070115     C                   MOVEL     SUMDAT        ED6T26DS
146800070125      *    Documento e Data : (Non utilizzato)
146900070125      *
147000070115     C                   ENDSR
147100070115     C*----------------------------------------------------------------
147200070115     C*? DRET27 - DECODIFICA TIPO RECORD TT27
147300070115     C*-----------------------------------
147400070115     C     DRET27        BEGSR
147500070115     C*
147600070115     C                   MOVEL     SUMDAT        ED6T27DS
147700070125      *    Spese di Trasporto : (Non utilizzato)
147800070115      *
147900070115     C                   ENDSR
148000070115     C*----------------------------------------------------------------
148100070115     C*? DRET28 - DECODIFICA TIPO RECORD TT28
148200070115     C*-----------------------------------
148300070115     C     DRET28        BEGSR
148400070115     C*
148500070115     C                   MOVEL     SUMDAT        ED6T28DS
148600070125      *    Importo Totale : (Non utilizzato)
148700070115      *
148800070115     C                   ENDSR
148900960524     C*----------------------------------------------------------------
149000960524     C*? DRET30 - DECODIFICA TIPO RECORD TT30
149100960524     C*----------------------------------------------------------------
149200960524     C     DRET30        BEGSR
149300960524     C*
149400070108     C                   MOVEL     SUMDAT        ED6T30DS
149500070125     C*  EQD: (Non utilizzato)
149600070125     C*
149700070125     C*  EQN:   Controllo se numero pallet numerico
149800960822     C                   TESTN                   W6350                98
149900960822     C     *IN98         IFEQ      '0'
150000960822     C                   MOVEL     *ZEROS        W6350
150100960822     C                   END
150200960628     C*  Stampa
150300040607     c                   If        In_Stampa ='S'
150400960628     C   OA              EXCEPT    TEST1
150500960628     C                   EXCEPT    DATAUT
150600040607     c                   end
150700960524     C*
150800960524     C                   ENDSR
150900070115     C*----------------------------------------------------------------
151000070115     C*? DRET31 - DECODIFICA TIPO RECORD TT31
151100070115     C*-----------------------------------
151200070115     C     DRET31        BEGSR
151300070115     C*
151400070115     C                   MOVEL     SUMDAT        ED6T31DS
151500070125      *    Name & Address : (Non utilizzato)
151600070115      *
151700070115     C                   ENDSR
151800070125      * ?----------------------------------------------------------------
151900960527     C*? DRED00 - DECODIFICA TIPO RECORD TD00
152000070125      * ?----------------------------------------------------------------
152100960527     C     DRED00        BEGSR
152200960527     C*
152300070215     C******             MOVEL     'N'           WNSPER            1
152400070215     C******             MOVEL     'S'           WPRSGN            1
152500040227     C*
152600070125      * ?Scrivo vecchio record EDiVAB e azzero dati per nuovo
152700960527     C     WRTVAB        IFEQ      'S'
152800070215     C*
152900070215      * ?- Scrivo bolla precedente - *
153000070215      * ?- e aggiorna i Files      ------------ *
153100070215     c                   exsr      Aggiorna_DB
153200070215      * ?            * ------------------------ *
153300070213      ***
153400070219      * ?- Attenzione solo se abilitato in tabella il Partner ad avere + linee   -*/
153500070219      * ?- di un Partner che ha realmente più linee                              -*/
153600070219     c                   if        Ptn_parziale = 'S' and §puPLN = 'S'
153700070219     c                   z-add     savXX         XX
153800070219     C                   exsr      PARTNEROK
153900070219     c                   end
154000070219      ***
154100960527     C                   END
154200000125      *
154300070219      * ?--------------->
154400070219      * ? Se stiamo trattando un Ptn con + linee ad ogni dettaglio cerco la
154500070219      * ? Nazione del mittente altrimenti prende quella di testata.
154600070219      * ?- Attenzione solo se abilitato in tabella il Partner ad avere + linee   -*/
154700070219      * ?- di un Partner che ha realmente più linee                              -*/
154800070219     c                   if        Ptn_parziale = 'S' and §puPLN = 'S'
154900070219     C                   exsr      DRED00_mitt
155000070219     c                   end
155100070219      * ?--------------->
155200070219      *
155300040302      * Abilita la scrittura
155400960527     C                   MOVEL     'S'           WRTVAB
155500070215      *
155600070215      *   Conta le spedizioni ossia i dettagli
155700070215     C                   ADD       1             WTOSPC           16 0
155800040302      *
155900040302      * Imposta qui i campi presi dalla PT oppure dall'RFF+FF di testata
156000040302      *  validi su tutti i dettagli del messaggio. Potranno essere reimpostati
156100040302      *  in presenza di valori specifici RFF+FF a livello di dettaglio su TD10
156200040302      *  e decodificati in seguito con tabella CL particolare.
156300040302     C                   MOVEL     Wvabnrs       VABNRS
156400040302     C                   MOVEL     Wvablnp       VABLNP
156500040302     C                   MOVEL     Wvabctm       VABCTM
156600040302      *
156700070213      * ?--------------->
156800070108     C                   MOVEL     SUMDAT        ED6D00DS
156900070125      *
157000070125      * ?Reperisco loro nr.spedizione e lo metto nel rif.mittente
157100070206     C                   clear                   WidSPEp          35
157200070220     c                   eval      WidSPEp = 'Rif.:' + %Trim(D001004)
157300070206     C                   MOVEL     D001004       WidSPE           35
157400070206     C                   MOVEL     D001004       WNSP             35
157500070109     C                   MOVEL     D001004       WNUM             35
157600070125      * ?                                       --------
157700070109     C                   MOVEL     D001004       VABRMA
157800070125      * ?                                       --------
157900071128     c                   clear                   wCNIsalvatoRMN
158000071128      * ?                                       --------
158100070109     C     ' '           SCAN      D001004       WLUNGH            3 0
158200960725     C                   SUB       1             WLUNGH
158300960627     C                   EXSR      TSTNUM
158400960627     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
158500070125      * ?                                       --------
158600960627     C                   MOVEL     WNUMOK        VABRMN
158700070125      * ?                                       --------
158800071128     c                   z-add     vabRMN        wCNIsalvatoRMN
158900071128      * ?                                       --------
159000960627     C                   ELSE
159100070215     C*******            MOVEL     'S'           WNSPER
159200960627     C                   Z-ADD     4             WINDER
159300960627     C                   MOVEL     *BLANKS       WVALOR
159400070109     C                   MOVEL     D002380       WVALOR
159500960731     C                   SETON                                        05
159600960627     C                   EXSR      STPERR
159700960627     C                   END
159800070125      *
159900070125      * ?Se impostata in testata la Data Consegna richiesta
160000070125      * ?  imposta come Default Data Ora
160100070125      * ?   prima di un eventuale data particolare a dettaglio
160200070125     C                   eval      vabDCR = WvabDCR
160300070125     C                   eval      vabHCR = WvabHCR
160400070125      *
160500960527     C*  Controllo se ho memorizzato data consegna richiesta
160600070109     C     D002005       IFEQ      'Z73'
160700070125     C     D002005       oreq      '2  '
160800960527     C                   MOVEL     *BLANKS       WDATA                          Data
160900070109     C                   MOVEL     D002380       WDATA                          Data
161000070109     C                   MOVEL     D002379       WFORM                          Formato
161100960527     C                   EXSR      CNVDAT
161200960527     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
161300960527     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
161400960527     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
161500960527     C     WERRO         IFEQ      'N'
161600960527     C                   Z-ADD     5             WINDER
161700960527     C                   MOVEL     *BLANKS       WVALOR
161800070109     C                   MOVEL     D002380       WVALOR
161900960731     C                   SETON                                        05
162000960527     C                   EXSR      STPERR
162100960527     C                   END
162200960527     C                   END
162300070125      ***
162400070125      * ?Reperisco priorità spedizione:
162500070125      *** attenzione se si tratta di cliente che gestisce fuori dallo standard tabellato
162600070125      *** (su tab"TS") occorre impostarlo sulla tab.PT. in modo da recepirlo come
162700070125      *** eccezione.
162800070125      *** Se viene passato in testata comunque viene impostato come Default.
162900070125     c                   if        wvabTSP <> *blank
163000070125     C                   movel     wvabTSP       VABTSP
163100070125     c                   end
163200070125      *
163300070125     C                   Z-ADD     1             Y
163400070109     c                   movel(p)  D004219       fld35a           35
163500061128     c                   move      §puTS         fld35a
163600061128     C     fld35a        LOOKUP    PTR35(Y)                               32
163700061128      *
163800960527     C     *IN32         IFEQ      '1'
163900960528     C                   MOVEL     TSP(Y)        VABTSP
164000960527     C                   ELSE
164100070125      *   Se non c'era nemmeno in testata..... stampa errore
164200070125     c                   IF        prior_testras = *blank
164300960527     C                   Z-ADD     6             WINDER
164400960527     C                   MOVEL     *BLANKS       WVALOR
164500070109     C                   MOVEL     D004219       WVALOR
164600960731     C                   SETON                                        05
164700960527     C                   EXSR      STPERR
164800070125     c                   end
164900960527     C                   END
165000070125      *
165100070125      * ?Controllo se c'è il c.assegno/imp.assicurato/valore merce
165200960527     C                   DO        3             X
165300960527     C* Controllo se campo importo numerico
165400960712     C     D00(X)        IFNE      *BLANKS
165500960527     C                   MOVEL     D00(X)        WTD00
165600960527     C                   MOVEL     D00(X)        WCOM19           19
165700960527     C                   MOVE      WCOM19        WCOM16           16
165800960527     C                   TESTN                   WCOM16               98
165900960527     C*  Se importo numerico decodifico tipo
166000960531     C     *IN98         IFEQ      '1'
166100960527     C                   EXSR      CTRVAL                                       Ctrl valuta
166200960527     C     WTPI          IFEQ      '157'
166300971117     C                   Z-ADD     WIMP          VABIAS                         IMPORTO
166400971117     C                   MOVEL     WVAL          VABVAS                         ASSICURATO
166500960527     C                   END
166600960527     C     WTPI          IFEQ      '77 '                                        Valore
166700971117     C                   Z-ADD     WIMP          VABVMD                         MERCE
166800971117     C                   MOVEL     WVAL          VABVAD                         DICHIARATO
166900960527     C                   END
167000960527     C     WTPI          IFEQ      '22 '                                        C/Assegno
167100981009     C     §PUCAS        IFEQ      'S'                                          C/Assegno
167200960527     C                   Z-ADD     WIMP          VABCAS
167300960527     C                   MOVEL     WVAL          VABVCA
167400981012      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
167500981012      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
167600981012     C     §PUTPC        IFEQ      'P'
167700981012     C     WIMP          ANDGT     *ZEROS
167800981012     C                   MOVEL     'OM'          VABTIC
167900060612     C                   MOVEL     'OM'          WVABTIC
168000981012     C                   ENDIF
168100981009     C                   END
168200960926     C                   END
168300960527     C*  Campo importo non numerico
168400960527     C                   ELSE
168500960527     C                   Z-ADD     12            WINDER
168600960527     C                   MOVEL     *BLANKS       WVALOR
168700960527     C                   MOVEL     WCOM16        WVALOR
168800960731     C                   SETON                                        05
168900960527     C                   EXSR      STPERR
169000960527     C                   END                                                    Imp.non numer.
169100960712     C                   END                                                    Imp.non numer.
169200960527     C*
169300960527     C                   END                                                    Do 3
169400960911     C*
169500070125      * ?Decodifico servizi speciali
169600070109     C     D007085       IFNE      *BLANKS
169700070109     C     D007085       ANDNE     *LOVAL
169800960911     C                   Z-ADD     1             X
169900070109     c                   movel(p)  D007085       fld35a           35
170000061128     c                   move      §puSS         fld35a
170100061128     C     fld35a        LOOKUP    SS35(X)                                32
170200960911     C     *IN32         IFEQ      '0'
170300960911     C                   Z-ADD     24            WINDER            3 0
170400960911     C                   MOVEL     *BLANKS       WVALOR           35
170500070109     C                   MOVEL     D007085       WVALOR
170600960912     C                   SETON                                        05
170700960911     C                   EXSR      STPERR
170800960911     C                   ELSE
170900000120     C                   MOVEL     DSS(X)        EDIDSSS
171000961129     C     §SSNOT        IFEQ      'S'
171100960911     C                   MOVEL     §SSDES        VABNOT
171200961129     C                   END
171300070125      *
171400070125      * ?x Espresso     "Pre-noon"
171500960911     C     §SSTPS        IFEQ      'S'
171600960911     C                   MOVEL     'E'           VABTSP
171700960911     C                   END
171800070125      * ?x Appuntamento "Booking ins"
171900031209     C     §SSTPS        IFEQ      'A'
172000031209     C                   MOVEL     'A'           VABTC1
172100031209     C                   END
172200070125      *
172300960911     C                   END
172400960911     C                   END
172500960628     C*  Stampa
172600040607     c                   If        In_Stampa ='S'
172700960628     C   OA              EXCEPT    TEST1
172800960628     C                   EXCEPT    DATSPE
172900040607     c                   end
173000980701      *
173100960527     C                   ENDSR
173200070125      * ?----------------------------------------------------------------
173300070219     C*? DRED00_MITT - DECODIFICA il mittente x Partner con più linee
173400070219      * ?----------------------------------------------------------------
173500070219     C     DRED00_mitt   BEGSR
173600070219     C*
173700070219     c                   z-add     nrrec         rec_pos           9 0
173800070219     c                   move      *blank        Naz_mitt          3
173900070219     c                   move      'N'           OK_Naz            1
174000070219     C*
174100070219      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
174200070219     c                   call      'TRTC79R4'
174300070219     c                   parm                    rec_pos
174400070219     c                   parm                    Naz_mitt
174500070219     c                   parm                    Ok_Naz
174600070219     c                   parm                    User_msg
174700070219     c                   parm                    UNB_Parz
174800070219      *
174900070219      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
175000070219      *   da decodificare come UNB.
175100070219     c                   if        Ok_Naz = 'S'
175200070219     c                   clear                   W035A
175300070219     C                   eval      W035A = %subst(UNB_Parz:1:31) + Naz_mitt
175400070219     C                   eval      XX = 1
175500070219     C     W035a         lookup    CPT(XX)                                32
175600070219      *
175700070219      *  Reimposta i dati del Partner Mittente Tabelle PT/PU/PV
175800070219     c                   if        %Found
175900070219      *
176000070219      *             *------------------------------*
176100070219     C                   EXSR      PARTNEROK
176200070219      *             *------------------------------*
176300070219      *
176400070219      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
176500070219      *   questa schiera è utilizzata anche per i clienti.
176600070219     C     §ptKSC        LOOKUP    KCL                                    39
176700070219     c                   If        *in39 = *off
176800070219     C                   add       1             WW
176900070219     C                   z-add     §ptKSC        KCL(WW)
177000070219     C                   Endif
177100070219      *
177200070219     c                   Endif
177300070219     c                   End
177400070219      *
177500070219     C                   ENDSR
177600070213      * ?----------------------------------------------------------------
177700070213     C*? DRED01 - DECODIFICA TIPO RECORD TD01
177800070213      * ?----------------------------------------------------------------
177900070213     C     DRED01        BEGSR
178000070213     C*
178100070125     C* Totali della spedizione:
178200070115     C                   MOVEL     SUMDAT        ED6D01DS
178300070125     C*  totale peso
178400070205     c                   if        §pvPEs  = 'S'
178500070205     C*
178600070125     c                   if        D016069 = '7  '
178700070125      *          Kilogrammi
178800070125     c                   if        D016411 = 'KGM'
178900070125     C                   Z-ADD     D016066       WvabPKB                         Peso
179000070205     C                   Z-ADD     WvabPKB       vabPKB                          Peso
179100070125     c                   end
179200070125      *          Grammi
179300070125     c                   if        D016411 = '163'
179400070125     C     D016066       div       1000          WvabPKB                         Peso
179500070205     C                   Z-ADD     WvabPKB       vabPKB                          Peso
179600070125     c                   end
179700070125     c                   end
179800070205      **
179900070205     c                   end
180000070125     C*  totale colli
180100070205     c                   if        §pvCOl  = 'S'
180200070205     C*
180300070125     c                   if        D016069 = '11 '
180400070125     C                   Z-ADD     D016066       WvabNCL                         N.Colli
180500070205     C                   Z-ADD     WvabNCL       vabNCL                          N.Colli
180600070205     C                   ADD       VABNCL        WTOCOC           16 0
180700070125     c                   end
180800070205      **
180900070205     c                   end
181000070205      *
181100070205     c                   If        In_Stampa ='S'
181200070205     C   OA              EXCEPT    TEST1
181300070205     C                   EXCEPT    SPECOL
181400070205     c                   end
181500070125     C*
181600070115     C                   ENDSR
181700070125      * ?----------------------------------------------------------------
181800070115     C*? DRED05 - DECODIFICA TIPO RECORD TD05
181900070125      * ?----------------------------------------------------------------
182000070115     C     DRED05        BEGSR
182100070115     C*
182200070108     C                   MOVEL     SUMDAT        ED6D05DS
182300070125     C*
182400960627     C*  Se ci sono delle note le memorizzo (note partenza)
182500070125      * ? solo se si tratta di note SIN ICN o PAI non altro
182600070206     C     D054451       IFEQ      §pvSIN                                       -- 01 --
182700070206     C     D054451       OREQ      §pvICN
182800070125     c                   clear                   tipNOTE          15
182900070125      *
183000070125      * Special Instruction
183100070206     C     D054451       IFEQ      §pvSIN
183200070125     c                   eval      tipNOTE = 'ISTRUZIONI ...:'
183300070125     C                   END
183400070125      * Instruction for Consignee
183500070206     C     D054451       IFEQ      §pvICN
183600070125     c                   eval      tipNOTE = 'X DESTINATARIO:'
183700960628     C                   END
183800070125      *
183900040607     c                   If        In_Stampa ='S'
184000960628     C   OA              EXCEPT    TEST1
184100960628     C                   EXCEPT    NOTSPE
184200040607     c                   end
184300070125      *
184400960628     C                   DO        4             X                              -- 02 --
184500960628     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
184600970520     C                   ADD       1             FF
184700070109     C                   MOVEL     D054451       QUA(FF)
184800970520     C                   MOVEL     D05(X)        FTX(FF)
184900960628     C     WNOT1         IFEQ      *BLANKS                                      -- 04 --
185000960627     C                   MOVEL     D05(X)        WNOT1
185100961231     C                   MOVE      D05(X)        WNOT2
185200960628     C                   ELSE                                                   -- 04 --
185300960628     C     WNOT2         IFEQ      *BLANKS                                      -- 05 --
185400960628     C                   MOVEL     D05(X)        WNOT2
185500960628     C                   END                                                    -- 05 --
185600960628     C                   END                                                    -- 04 --
185700960628     C*  Stampo dati note
185800960702     C                   MOVEL     D05(X)        WNOTSP           70
185900040607     c                   If        In_Stampa ='S'
186000960628     C   OA              EXCEPT    TEST1
186100960628     C                   EXCEPT    NOTSP1
186200040607     c                   end
186300960628     C                   END                                                    -- 03 --
186400960628     C                   END                                                    -- 02 --
186500960527     C*
186600960628     C                   END                                                    -- 01 --
186700960628     C*
186800960628     C*  Se ci sono delle note le memorizzo (note partenza)
186900070206     C     D0544511      IFEQ      §pvSIN                                       -- 01 --
187000070206     C     D0544511      OREQ      §pvICN
187100070125     c                   clear                   tipNOTE          15
187200070125     C*
187300070109     C     D0544511      IFNE      D054451
187400070206     C     D0544511      IFEQ      §pvSIN
187500070125     c                   eval      tipNOTE = 'ISTRUZIONI ...:'
187600960628     C                   END
187700070206     C     D0544511      IFEQ      §pvICN
187800070125     c                   eval      tipNOTE = 'X DESTINATARIO:'
187900960628     C                   END
188000070125     C*
188100040607     c                   If        In_Stampa ='S'
188200960628     C   OA              EXCEPT    TEST1
188300960628     C                   EXCEPT    NOTSPE
188400040607     c                   end
188500960628     C                   END
188600960628     C*
188700960628     C                   DO        4             X                              -- 02 --
188800960628     C     D05A(X)       IFNE      *BLANKS                                      -- 03 --
188900970623     C                   ADD       1             FF
189000070109     C                   MOVEL     D0544511      QUA(FF)
189100970623     C                   MOVEL     D05A(X)       FTX(FF)
189200960628     C     WNOT1         IFEQ      *BLANKS                                      -- 04 --
189300960628     C                   MOVEL     D05A(X)       WNOT1
189400961231     C                   MOVE      D05A(X)       WNOT2
189500960628     C                   ELSE                                                   -- 04 --
189600960628     C     WNOT2         IFEQ      *BLANKS                                      -- 05 --
189700960628     C                   MOVEL     D05A(X)       WNOT2
189800960628     C                   END                                                    -- 05 --
189900960628     C                   END                                                    -- 04 --
190000960628     C*  Stampo dati note
190100040607     c                   If        In_Stampa ='S'
190200960628     C   OA              EXCEPT    TEST1
190300960628     C                   EXCEPT    NOTSP1
190400040607     c                   end
190500960628     C                   END                                                    -- 03 --
190600960628     C                   END                                                    -- 02 --
190700960628     C*
190800960628     C                   END                                                    -- 01 --
190900060317     C*------>>
191000070125      * ?  Decodifico tipo pagamento C/Assegno
191100060317      *      lo pulisco solo se non l'ho decodificato e non l'ho
191200060317      *       ancora scritto (Problema di pulizia in presenza di + righe TD05)
191300060317      *       --> succedeva che al primo giro aveva letto una TD05 con le informazioni
191400060317      *           x il tipo incasso poi arrivava una seconda riga TD05 con altro tipo
191500060317      *           di informazioni, la riga del VAB non era stata ancora scritta e qui
191600060317      *           abblankava il VABTIC togliendo il Tipo Incasso inviato.
191700060317     C                   if        wri_vabtic = *blank
191800960716     C                   MOVEL     *BLANKS       VABTIC
191900060317     c                   end
192000060612      ****
192100080716     c                   clear                   trovato_TPI
192200070125      * ?Se c'è il tipo Incasso
192300070125     C     D054451       IFEQ      'PAI'                                        -- 01 --
192400070125      ****
192500070125     C                   do        4             X                              -- 02 --
192600960628     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
192700960716     C                   MOVEL     D05(X)        WCTC              2
192800960716     C                   Z-ADD     1             Y
192900061128     c                   movel(p)  Wctc          fld35a           35
193000061128     c                   move      §puTC         fld35a
193100061128     C     fld35a        LOOKUP    CTC35(Y)                               33
193200070125      * ?Se Partner IMPOSTO 'OM'
193300070125     C     §PUTPC        IFEQ      'P'                                          -- 04 --
193400060928     c     wctc          andne     'TO'
193500981009     C                   MOVEL     'OM'          VABTIC
193600070125     C                   ELSE                                                   -- 04 --
193700981009     C   33              MOVEL     TPI(Y)        VABTIC
193800080716     c   33              move      'S'           trovato_TPI
193900070125     C                   END                                                    -- 04 --
194000981009     C                   END                                                    -- 03 --
194100070125     C                   ENDdo                                                  -- 02 --
194200981009      *
194300960628     C                   ELSE                                                   -- 01 --
194400981009      *
194500070125      * ?Se NON c'è il tipo Incasso Contrassegno particolare
194600070125      *    Deve essere impostato comunque da testata
194700080716     c                   if        vabtic = *blank
194800080716      *
194900070125     C                   MOVEL     wVabTIC       VABTIC
195000080716      *
195100080716     c                   end
195200070125      *
195300080716      *
195400070125      * ?Se ci sono istruzioni di pagamento
195500070109     C     D0544511      IFEQ      'PAI'                                        -- 02 --
195600960628     C                   DO        4             X                              -- 03 --
195700070125     C     D05A(X)       IFNE      *BLANKS                                      -- 04 --
195800070125     C                   MOVEL     D05A(X)       WCTC              2
195900981009     C                   Z-ADD     1             Y
196000061128     c                   movel(p)  Wctc          fld35a           35
196100061128     c                   move      §puTC         fld35a
196200061128     C     fld35a        LOOKUP    CTC35(Y)                               33
196300070125      * ?Se Partner IMPOSTO 'OM'
196400981009     C     §PUTPC        IFEQ      'P'
196500060928     c     wctc          andne     'TO'
196600981009     C                   MOVEL     'OM'          VABTIC
196700981009     C                   ELSE
196800981009     C   33              MOVEL     TPI(Y)        VABTIC
196900080716     c   33              move      'S'           trovato_TPI
197000981009     C                   END                                                    -- 03 --
197100960628     C                   END                                                    -- 04 --
197200960628     C                   END                                                    -- 03 --
197300960628     C                   END                                                    -- 02 --
197400981009      *
197500960628     C                   END                                                    -- 01 --
197600981009      *
197700060317      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
197800060317      *    essere presenti altri record Flat TD05 con altre informazioni
197900060317      *    che potrebbero abblancare il Tipo incasso precedentemente
198000060317      *    decodificato
198100080716     c********           if        vabtic <> *blank and wri_vabtic = *blank
198200080716     c                   if        Trovato_TPI = 'S' and wri_vabtic = *blank
198300060317     c                   eval      wri_vabtic = 'S'
198400060317     c                   end
198500060317      *
198600960527     C                   ENDSR
198700070125      * ?----------------------------------------------------------------
198800000125      *? DRED10 - DECODIFICA TIPO RECORD TD10
198900070125      * ?----------------------------------------------------------------
199000960527     C     DRED10        BEGSR
199100000125      *
199200070108     C                   MOVEL     SUMDAT        ED6D10DS
199300061128      *
199400000125      *  Trascodifico tipo trasporto
199500000125     C     VABCBO        IFEQ      *BLANKS
199600070129      *
199700070131     c                   clear                   TOD_char3         3
199800070129      *   Imposta o il codice o il metodo
199900070129      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
200000070129     c                   if        D104053  <> *blank
200100070131     c                   movel(p)  D104053       TOD_char3
200200070129     c                   else
200300070130     c                   if        D104215  <> *blank
200400070129     c                   movel(p)  D104215       TOD_char3
200500070129     c                   end
200600070130     c                   end
200700000125      *
200800070130     c                   if        TOD_char3 <> *blank
200900070129     C                   eval      Y = 1
201000070129     c                   movel(p)  TOD_char3     fld35a           35
201100061128     c                   move      §puTB         fld35a
201200061128     C     fld35a        LOOKUP    CTB35(Y)                               32
201300070129      *
201400070221      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
201500070221      *   nell'altro campo 4215 quindi comunque ci si riprova:
201600070221      *
201700070221     C                   if        *in32 = *Off and D104215  <> *blank
201800070221     c                   movel(p)  D104215       TOD_char3
201900070221     c                   movel(p)  TOD_char3     fld35a           35
202000070221     c                   move      §puTB         fld35a
202100070221     C                   eval      Y = 1
202200070221     C     fld35a        LOOKUP    CTB35(Y)                               32
202300070221     c                   end
202400070221      *
202500070221     c                   end
202600070221      *
202700070221      *  se anche a questo punto non ha decoficato nulla prende il default se c'è
202800070221     C     *IN32         IFEQ      *off
202900070221     C     §puPFA        andne     *blank
203000070221      *
203100070221     c                   SELECT
203200070221      *  Porto Franco
203300070221     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
203400070221     C                   movel     '4'           VABCBO                         + C/Ass
203500070221     c                   WHEN      §puPFA = 'F'
203600070221     C                   movel     '1'           VABCBO                         Senza C/Ass.
203700070221      *  Porto Assegnato
203800070221     c                   WHEN      §puPFA = 'A'  and  VABcas > *zeros
203900070221     C                   movel     '6'           VABCBO                         + C/Ass
204000070221     C                   WHEN      §puPFA = 'A'
204100070221     C                   movel     '2'           VABCBO                         Senza C/Ass
204200070221     C                   ENDSL
204300070221      *
204400070221     c                   end
204500070221      *
204600000125     C     *IN32         IFEQ      '1'
204700000125     c                   SELECT
204800000125     c                   WHEN      POR(Y) = 'F'  and  VABcas > *zeros
204900000125     C                   movel     '4'           VABCBO                         + C/Ass
205000000125     c                   WHEN      POR(Y) = 'F'
205100000125     C                   movel     '1'           VABCBO                         Senza C/Ass.
205200000125      *  Porto Assegnato
205300000125     c                   WHEN      VABCAS > *zeros
205400000125     C                   movel     '6'           VABCBO                         + C/Ass
205500000125     C                   OTHER
205600000125     C                   movel     '2'           VABCBO                         Senza C/Ass
205700000125     C                   ENDSL
205800070130     C                   ENDIF
205900070130      *
206000000125      *  No decodifica termini di consegna: errore
206100070130     c                   if        TOD_char3 =  *blank  or *in32 =*off
206200070221     c                   if        §pupfa = *blank
206300000125     C                   z-add     8             WINDER
206400000125     C                   clear                   WVALOR
206500000125     C                   eval      *in05 = *on
206600960527     C                   EXSR      STPERR
206700070221     c                   end
206800070131     C                   End
206900070129      *
207000000125     C                   ENDIF
207100980701      *
207200070129      * ?--------------------------*
207300070131      *  Quello numerico solo se non ho FF o CU
207400000125     C                   SELECT
207500070131     c                   WHEN      D101153  = §PVRFF and D101154  <> *BLANKS
207600070109     C                   movel     D101154       WAGE
207700070131     c                   WHEN      D1011531 = §PVRFF and D1011541 <> *BLANKS
207800070109     C                   movel     D1011541      WAGE
207900070131     c                   WHEN      D1011532 = §PVRFF and D1011542 <> *BLANKS
208000070109     C                   movel     D1011542      WAGE
208100000125     C                   ENDSL
208200980701      *
208300000125     C                   IF         WAGE <> *BLANKS
208400000125     C                   movel     WAGE          WNSP             35
208500000125     C                   movel     WAGE          WNUM             35
208600000125     C     ' '           scan      WAGE          WLUNGH
208700000125     C                   sub       1             WLUNGH
208800980701     C                   EXSR      TSTNUM
208900000125     C                   IF         WOKNUM = 'S'                                Ctrl numerico
209000000125     C                   movel     WAGE          VABRMA
209100070131     C                   IF         WRMN = §PVRFF or WRMN = *blanks
209200000125     C                   movel     WNUMOK        VABRMN
209300070131     C                   movel     §PVRFF        WRMN
209400000125     C                   ENDIF
209500000125     C                   ENDIF
209600000125     C                   ENDIF
209700980701      *
209800980701      *  Se ho FF e trovo il cod. cliente corrispondente prendo il rif. numerico
209900980715      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
210000000125      **********************************************************************
210100000125      *  In questa fase memorizzo i campi necessari al controllo che ho
210200000125      *  spostato nella sbr  DRED15 dopo aver riagganciato la PT
210300000125      **********************************************************************
210400000125     C                   SELECT
210500070109     C                   WHEN      D101153  = 'FF '  and D101154  <> *BLANKS
210600070109     C                   movel     D101154       WFF
210700070109     C                   WHEN      D1011531 = 'FF '  and D1011541 <> *BLANKS
210800070109     C                   movel     D1011541      WFF
210900070109     C                   WHEN      D1011532 = 'FF '  and D1011542 <> *BLANKS
211000070109     C                   movel     D1011542      WFF
211100000125     c                   ENDSL
211200980701      *
211300980701      *  Se ho CU prendo il riferimento numerico
211400980715      *  ECCEZIONE: se ho già impostato FF obbligatorio
211500980715     C     WRMN          IFNE      'FFS'
211600980715      *
211700000125     C                   SELECT
211800070131     C                   WHEN      D101153  = §pvRCU and D101154 <> *blanks
211900070109     C                   movel     D101154       WCU
212000070131     C                   WHEN      D1011531 = §pvRCU and D1011541<> *blanks
212100070109     C                   movel     D1011541      WCU
212200070131     C                   WHEN      D1011532 = §pvRCU and D1011542<> *blanks
212300070109     C                   movel     D1011542      WCU
212400000125     C                   ENDSL
212500980701      *
212600980701     C     WCU           IFNE      *BLANKS
212700000125     C                   movel     WCU           WNUM             35
212800000125     C     ' '           scan      WCU           WLUNGH
212900000125     C                   sub       1             WLUNGH
213000980701     C                   EXSR      TSTNUM
213100000125     C                   If          WOKNUM = 'S'                               Ctrl numerico
213200000125     C                   movel     WNUMOK        VABRMN
213300070131     C                   movel     §pvRCU        WRMN
213400000125     C                   Endif
213500000125     C                   ENDIF
213600980701      *
213700000125     C                   ENDIF
213800980715      *
213900000125      *  Stampo dati riferimenti consegna
214000970429     C     STPD10        TAG
214100040607     c                   If        In_Stampa ='S'
214200960628     C   OA              EXCEPT    TEST1
214300960628     C                   EXCEPT    RIFCON
214400040607     c                   end
214500010502      *
214600010502     C*----------------------------------------------------------------
214700010502      *  La routine finiva qui ma per E.Arden che tratta il segmento
214800010502      *  NAD+DP nel TD15 senza avere il NAD+CZ occorre decodificare in
214900010502      *  anticipo il codice particolare del Cliente (CL)
215000010502     C*----------------------------------------------------------------
215100010502      * Se ho FF e trovo il cod. cliente prendo il rif. numerico  ex DRED15
215200010502      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
215300010502     C     WFF           IFNE      *BLANKS
215400010502     C                   eval      XX = 1
215500010502     C                   movel     §PTKSC        WCCL             35
215600010502     C                   movel     WFF           WCOM28           28
215700010502     C                   move      WCOM28        WCCL
215800010502     C     WCCL          lookup    CCL(XX)                                34
215900010502      *
216000010502     C     *IN34         IFEQ      '1'
216100010502     C                   movel     DCL(XX)       EDIDSCL
216200040311     C                   z-add     §CLKSC        WCLKSC            7 0
216300040311     C                   movel     §CLTAR        WCLTAR            3
216400040311     C                   z-add     §CLLNP        WCLLNP            3 0
216500040311     C                   z-add     §CLNRS        WCLNRS            2 0
216600040311     C                   movel     §CLCTM        WCLCTM            2
216700040311     C                   movel     §CLBS1        WCLBS1            1
216800050223     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
216900040311      *
217000040311      * imposta la LNP   x il dettaglio specifco
217100040311     c                   eval      VABlnp = §CLlnp
217200040311      *
217300040311      * imposta la Serie x il dettaglio specifco
217400040311     c                   eval      VABnrs = §CLnrs
217500040311      *
217600040311      *  In base al cod.trattamento merce reperisco tipo tratt.
217700040311      *    per un dettaglio specifico.
217800040311     C                   CLEAR                   DS1B
217900040311     C                   Z-ADD     1             Y                 3 0
218000040311     C     §CLctm        LOOKUP    CTM(Y)                                 32
218100040311     C     *IN32         IFEQ      '1'
218200040311     C                   MOVEL     DTM(Y)        DS1B
218300040311     C                   END
218400040311      *
218500040311      * Se il cliente non era sulla schiera lo aggiunge
218600010502     C     §CLKSC        lookup    KCL                                    39
218700010502     C                   if        *in39 = *off
218800010502     C                   add       1             WW
218900010502     C                   z-add     §CLKSC        KCL(WW)
219000010502     c                   endif
219100010502      *
219200010502      * Controllo se devo memorizzare un altro riferimento
219300010502     C     §CLFTX        IFNE      *BLANKS
219400010502     C                   eval      FF = 1
219500010502     C     §CLFTX        lookup    QUA(FF)                                34
219600010502      *
219700010502     C     *IN34         IFEQ      '1'
219800010502     C                   movel     FTX(FF)       WNUM             35
219900010502     C     ' '           scan      FTX(FF)       WLUNGH
220000010502     C                   sub       1             WLUNGH
220100010502     C                   EXSR      TSTNUM
220200010502      *
220300010502     C                   SELECT
220400010502     C                   WHEN      WOKNUM <> 'S'                                Ctrl numerico
220500010502     C                   WHEN      §CLROB  = 'S'
220600010502     C                   movel     WNUMOK        VABRMN
220700010502     C                   movel     'FFS'         WRMN
220800070131     C                   WHEN      WRMN   <> §pvRCU
220900010502     C                   movel     WNUMOK        VABRMN
221000010502     C                   movel     'FF '         WRMN
221100010502     C                   ENDSL
221200010502      *
221300010502     C                   ENDIF
221400010502     C                   ENDIF
221500010502      *
221600010502     C                   ENDIF
221700010502     C                   ENDIF
221800010502     C*----------------------------------------------------------------
221900000125      *
222000960527     C                   ENDSR
222100070115     C*----------------------------------------------------------------
222200070115     C*? DRED12 - DECODIFICA TIPO RECORD TD12
222300070115     C*----------------------------------------------------------------
222400070115     C     DRED12        BEGSR
222500070115     C*
222600070131     C*  Esigenze Governative : Non gestito
222700070115     C                   MOVEL     SUMDAT        ED6D12DS
222800070115      *
222900070115     C                   ENDSR
223000070115     C*----------------------------------------------------------------
223100070115     C*? DRED13 - DECODIFICA TIPO RECORD TD13
223200070115     C*----------------------------------------------------------------
223300070115     C     DRED13        BEGSR
223400070115     C*
223500070131     C*  Dettaglio Documenti  : Non gestito
223600070115     C                   MOVEL     SUMDAT        ED6D13DS
223700070115      *
223800070115     C                   ENDSR
223900010502     C*----------------------------------------------------------------
224000960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
224100960527     C*----------------------------------------------------------------
224200960527     C     DRED15        BEGSR
224300070131      *
224400070108     C                   MOVEL     SUMDAT        ED6D15DS
224500000120     c                   eval      *in04 = *off
224600000120     c                   eval      *in07 = *off
224700070131      * ?------------------------*
224800070131     C*  Name and Adderss di testata: se non impostato un destinatario in dettaglio
224900070131      * ?------------------------*
225000000120      * Dati destinatario
225100070213     c                   movel     D153035       Qua3035           2
225200070213     C                   IF        Qua3035= 'CN'  or
225300070213     C                             (§PTDES <> *BLANKS  and  §PTDES = Qua3035)
225400070131      *
225500070131     c                   move      'S'           NAD_CN_detta      1
225600070131      *
225700000120     c                   eval      *in04 = *on
225800000120     c                   eval      *in07 = *on
225900960822     C* Reperisco nazione corrispondente destinatario
226000960822     C                   Z-ADD     1             YY                3 0
226100000120     c                   eval      *in32 = *off
226200070109     C                   if        D153207 <> *BLANKS
226300070109     C     D153207       LOOKUP    ISO(YY)                                32
226400000120     C                   endif
226500080115      *
226600000125     C* Imposto dati destinatario
226700070109     C                   MOVEL     D153036       VABRSD
226800080115     C                   MOVEL     D153164       VABLOD
226900080115      *
227000070109     C                   if        D1530361<> *LOVAL
227100070109     C                   MOVEL     D1530361      VABRD2
227200000120     C                   endif
227300080115      *
227400080115      *  Se richiesto in tabella PT di accodare alla Ragione sociale
227500080115      *  l'informazione di indirizzo invertita nel segmento...
227600080115      *  Esigenza nata da Dior che mette l'indirizzo vero nel 2°campo del Indirizzo NAD
227700080115      *  ossia dopo i 2 punti (:) ed invece mette un'informazione sul destinatario/
227800080115      *  indirizzo nel 1° campo dell'Inidirizzo NAD prima dei 2 punti (:).
227900080115      *   Per nostra comodità mettiamo quindi l'informazione aggiuntiva attaccata alla
228000080115      *   Ragione Sociale e non alla Località poichè altrimenti avremmo continui
228100080115      *   problemi di decodifica Località/Cappario durante la conferma bolle.
228200080115     c                   if        §pv2IN = 'S'
228300080115      *
228400080115     c                   if        D1530421 <> *blanks  and D1530421 <> *loval
228500080115      *
228600080115      * Nell'indirizzo il 2°campo ricevuto
228700080115     C                   MOVEL     D1530421      VABIND
228800080115      * Mentre appiccica all'estensione della ragione sociale le info di consegna
228900080115      *  messe nel 1° campo dell'indirizzo (DIOR)
229000080115      *
229100080115      *  Verifico se c'è spazio sul campo estensione altrimenti vado in coda
229200080115      *    prima all'indirizzo e poi alla località
229300080115      *
229400080115     C* Verifico se c'è dello spazio per metterlo nell'estensione rag.soc.
229500080115     C     '    '        SCAN      VABRD2        WI                4 0
229600080115     c                   if        WI < 10
229700080115     C     '    '        SCAN      VABrd2        WI                4 0
229800080115     C                   MOVEA     VABrd2        F35
229900080115     C                   ADD       1             WI
230000080115     C                   MOVEA     D153042       F35(WI)
230100080115     C                   MOVEA     F35           VABrd2
230200080115      *
230300080115     c                   else
230400080115      *  Verifico se c'è spazio sull'indirizzo altrimenti vado in coda alla località
230500080115     C     '    '        SCAN      VABIND        WI                4 0
230600080115     c                   if        WI < 10
230700080115     C     '    '        SCAN      VABIND        WI                4 0
230800080115     C                   MOVEA     VABIND        F35
230900080115     C                   ADD       1             WI
231000080115     C                   MOVEA     D153042       F35(WI)
231100080115     C                   MOVEA     F35           VABIND
231200080115      *
231300080115     c                   else
231400080115      *  oppure    accodato nella località
231500080115     C     '    '        SCAN      VABLOD        WI                4 0
231600080115     C                   MOVEA     VABLOD        F35
231700080115     C                   ADD       1             WI
231800080115     C                   MOVEA     D153042       F35(WI)
231900080115     C                   MOVEA     F35           VABLOD
232000080115     c                   end
232100080115     c                   end
232200080115      *-----------
232300080115     c                   else
232400080115      *
232500080115     C                   MOVEL     D153042       VABIND
232600080115     c                   end
232700080115      *
232800080115      *  segue lo standard
232900080115     c                   else
233000080115      *
233100080115     C                   MOVEL     D153042       VABIND
233200080115      *
233300000120     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
233400070109     c                   if        D1530421 <> *blanks  and D1530421 <> *loval
233500070206      *
233600070206      *  Verifico se c'è spazio sul primo indirizzo x accodare se richiesto
233700070206      *  altrimenti vado in coda alla località
233800070206     C     '    '        SCAN      VABIND        WI                4 0
233900070206      *
234000070206     C* Verifico se c'è dello spazio per metterlo nella località
234100070206      *  oppure    accodato al primo indirizzo
234200070206      *
234300080115     c                   if        WI < 15
234400070206      *
234500070206     C     '    '        SCAN      VABind        WI                4 0
234600080115     C                   MOVEA     VABind        F35
234700070206     C                   ADD       1             WI
234800080115     C                   MOVEA     D1530421      F35(WI)
234900080115     C                   MOVEA     F35           VABind
235000070206      *
235100070206     c                   else
235200070206      *
235300070206     C     '    '        SCAN      VABLOD        WI                4 0
235400080115     C                   MOVEA     VABLOD        F35
235500070206     C                   ADD       1             WI
235600080115     C                   MOVEA     D1530421      F35(WI)
235700080115     C                   MOVEA     F35           VABLOD
235800070206      *
235900070206     c                   end
236000080115      *
236100080115      *
236200080115     c                   end
236300080115      *
236400070206      *
236500000125     C                   ENDif
236600080115      * ---------
236700000120     C                   If        *in32 = *on
236800000120     C                   MOVEL     C15(YY)       VABNZD
236900000120     C                   MOVEL     CPF(YY)       WFLCPF            2 0
237000000120     c                   Else
237100000120     C                   MOVEL     *BLANKS       VABNZD
237200000120     C                   Z-ADD     0             WFLCPF
237300000120     c                   Endif
237400000125      * Converto CAP
237500070202     c                   movel(p)  D153251       CAP_9ALFA         9
237600960828     C                   EXSR      CNVCAP
237700000120     C                   ENDIF
237800000120      *
237900070131      * ?------------------------*
238000070131     C*  Name and Adderss di testata: se non impostato un mittente in dettaglio
238100070131      * ?------------------------*
238200980930      * Dati mittente
238300070213     C                   IF        Qua3035 = 'CZ'
238400070131      *
238500070131     c                   move      'S'           NAD_CZ_detta      1
238600070131      *
238700070109     C                   movel     D153036       VABRMO
238800000125      *
238900000120      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
239000000125     C                   movel     §PTNAZ        VABNMO
239100040302      *
239200070109     C                   IF        D153207 <> *blanks
239300000125     C                   eval      YY = 1
239400070109     C     D153207       LOOKUP    ISO(YY)                                32
239500000125     C                   if         *in32 = *on
239600000125     C                   MOVEL     C15(YY)       VABNMO
239700000125     C                   endif
239800000125     C                   ENDIF
239900000125      *
240000991007      * Normalizzo CAP mittente originale
240100000120     C                   clear                   T
240200000120     C                   clear                   S
240300991007     C                   MOVEA     *BLANKS       CAPM
240400070109     C                   MOVEA     D153251       CAP9
240500991007     C                   DO        9             T
240600991007     C     CAP9(T)       IFNE      *BLANKS
240700991007     C                   ADD       1             S
240800991007     C                   MOVE      CAP9(T)       CAPM(S)
240900991007     C                   ENDIF
241000991007     C                   ENDDO
241100991007     C*
241200980930      *     Controllo validità CAP
241300991007     C                   MOVEA     CAPM          VABCMO
241400980930      *
241500000120     C                   CLEAR                   TISI95DS
241600980930     C                   MOVEL     VABCMO        I95CAP
241700980930     C                   MOVEL     VABNMO        I95NAR
241800980930     C                   MOVEL     WOGGI         I95DAT
241900980930     C                   MOVEL     '3'           I95TCN
242000980930     C                   CALL      'TISI95R'
242100000120     C                   PARM                    TISI95DS
242200980930      *     Errore
242300980930     C     O95ERR        IFNE      *BLANKS
242400980930     C                   MOVEL     INDCAE        VABCMO
242500980930     C                   MOVEL     §PTNAZ        VABNMO
242600961024     C                   END
242700980929      *
242800960527     C                   END
242900980929      *
243000980929      *  Stampo dati soggetti transazione
243100040607     c                   If        In_Stampa ='S'
243200960628     C   OA              EXCEPT    TEST1
243300960628     C                   EXCEPT    SOGTRA
243400040607     c                   end
243500000125      *
243600000125      *  Controllo se inserito rivolgersi a ..
243700070109     C     D153139       IFNE      *BLANKS
243800070109     C     D153412       ORNE      *BLANKS
243900040607     c                   If        In_Stampa ='S'
244000960628     C   OA              EXCEPT    TEST1
244100960628     C                   EXCEPT    SOGRIV
244200040607     c                   end
244300960628     C                   END
244400000125      *
244500070131      * Se ne prende uno solo poichè sul nostro D.B. ce n'è uno solo
244600070131      *  memorizza il primo riferimento di consegna x il destinatario
244700070109     C     D153412       ifne      *BLANKS
244800070529     c     watRDE        andeq     *blank
244900070109     c                   movel     D153412       watrde
245000031211     c                   end
245100070131      *
245200070131      * Se ne prende uno solo poichè sul nostro D.B. ce n'è uno solo
245300070131      *  memorizza il primo riferimento di consegna nr.telefonico
245400070109     C     D153148       ifne      *BLANKS
245500070529     c     watTEL        andeq     *blank
245600070109     c                   movel     D153148       wattel
245700031211     c                   end
245800031211      *
245900960527     C                   ENDSR
246000070115     C*----------------------------------------------------------------
246100070115     C*? DRED16 - DECODIFICA TIPO RECORD TD16
246200070115     C*----------------------------------------------------------------
246300070115     C     DRED16        BEGSR
246400070115     C*
246500070131     C*  Località/Sito        : Non gestito
246600070115     C                   MOVEL     SUMDAT        ED6D16DS
246700070115      *
246800070115     C                   ENDSR
246900070115     C*----------------------------------------------------------------
247000070115     C*? DRED17 - DECODIFICA TIPO RECORD TD17
247100070115     C*----------------------------------------------------------------
247200070115     C     DRED17        BEGSR
247300070115     C*
247400070131     C*  Dettagli Documento   : Non gestito
247500070115     C                   MOVEL     SUMDAT        ED6D17DS
247600070115      *
247700070115     C                   ENDSR
247800070115     C*----------------------------------------------------------------
247900070115     C*? DRED18 - DECODIFICA TIPO RECORD TD18
248000070115     C*----------------------------------------------------------------
248100070115     C     DRED18        BEGSR
248200070115     C*
248300070131     C*  Spese di trasporto   : Non gestito
248400070115     C                   MOVEL     SUMDAT        ED6D18DS
248500070115      *
248600070115     C                   ENDSR
248700070115     C*----------------------------------------------------------------
248800070115     C*? DRED19 - DECODIFICA TIPO RECORD TD19
248900070115     C*----------------------------------------------------------------
249000070115     C     DRED19        BEGSR
249100070115     C*
249200070131     C*  Importo              : Non gestito
249300070115     C                   MOVEL     SUMDAT        ED6D19DS
249400070115      *
249500070115     C                   ENDSR
249600070115     C*----------------------------------------------------------------
249700070115     C*? DRED1A - DECODIFICA TIPO RECORD TD1A
249800070115     C*----------------------------------------------------------------
249900070115     C     DRED1A        BEGSR
250000070115     C*
250100070131     C*  Riferimenti          :
250200070115     C                   MOVEL     SUMDAT        ED6D1ADS
250300070131     C*
250400070131     C*  Il riferimento potrebbe essere non precedente al NAD
250500070131     C*   ma successivo in tal caso deve essere preso il primo
250600070131      * ?--------------------------*
250700070131      *  Prende il riferimento se non era stato precedentemente caricato
250800070131     c                   if        D1A1153 = §PVRFF and D1A1154 <> *BLANKS and
250900070131     c                             wAGE = *blank
251000070131     C                   movel     D1A1154       WAGE
251100070131      *
251200070131     C                   IF         WAGE <> *BLANKS
251300070131     C                   movel     WAGE          WNSP             35
251400070131     C                   movel     WAGE          WNUM             35
251500070131     C     ' '           scan      WAGE          WLUNGH
251600070131     C                   sub       1             WLUNGH
251700070131     C                   EXSR      TSTNUM
251800070131     C                   IF         WOKNUM = 'S'                                Ctrl numerico
251900070131     C                   movel     WAGE          VABRMA
252000070131     C                   IF         WRMN = §PVRFF or WRMN = *blanks
252100070131     C                   movel     WNUMOK        VABRMN
252200070131     C                   movel     §PVRFF        WRMN
252300070131     C                   EndIF
252400070131     C                   ENDIF
252500070131      *
252600070131     C                   ENDIF
252700070131     C                   End
252800070131      *
252900070201      *  Se ho FF e trovo il cod. cliente corrispondente prendo il rif. numerico
253000070201      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
253100070201      **********************************************************************
253200070201      *  In questa fase memorizzo i campi necessari al controllo che ho
253300070201      *  spostato nella sbr  DRED15 dopo aver riagganciato la PT
253400070201      **********************************************************************
253500070201     C                   if        D1A1153 = 'FF ' and D1A1154 <> *BLANKS and
253600070201     c                             wff = *blank
253700070201     C                   movel     D1A1154       WFF
253800070201      *
253900070131     C*----------------------------------------------------------------
254000070131      * Se ho FF e trovo il cod. cliente prendo il rif. numerico  ex DRED15
254100070131      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
254200070131     C     WFF           IFNE      *BLANKS
254300070131     C                   eval      XX = 1
254400070131     C                   movel     §PTKSC        WCCL             35
254500070131     C                   movel     WFF           WCOM28           28
254600070131     C                   move      WCOM28        WCCL
254700070131     C     WCCL          lookup    CCL(XX)                                34
254800070131      *
254900070131     C     *IN34         IFEQ      '1'
255000070131     C                   movel     DCL(XX)       EDIDSCL
255100070131     C                   z-add     §CLKSC        WCLKSC            7 0
255200070131     C                   movel     §CLTAR        WCLTAR            3
255300070131     C                   z-add     §CLLNP        WCLLNP            3 0
255400070131     C                   z-add     §CLNRS        WCLNRS            2 0
255500070131     C                   movel     §CLCTM        WCLCTM            2
255600070131     C                   movel     §CLBS1        WCLBS1            1
255700070131     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
255800070131      *
255900070131      * imposta la LNP   x il dettaglio specifco
256000070131     c                   eval      VABlnp = §CLlnp
256100070131      *
256200070131      * imposta la Serie x il dettaglio specifco
256300070131     c                   eval      VABnrs = §CLnrs
256400070131      *
256500070131      *  In base al cod.trattamento merce reperisco tipo tratt.
256600070131      *    per un dettaglio specifico.
256700070131     C                   CLEAR                   DS1B
256800070131     C                   Z-ADD     1             Y                 3 0
256900070131     C     §CLctm        LOOKUP    CTM(Y)                                 32
257000070131     C     *IN32         IFEQ      '1'
257100070131     C                   MOVEL     DTM(Y)        DS1B
257200070131     C                   END
257300070131      *
257400070131      * Se il cliente non era sulla schiera lo aggiunge
257500070131     C     §CLKSC        lookup    KCL                                    39
257600070131     C                   if        *in39 = *off
257700070131     C                   add       1             WW
257800070131     C                   z-add     §CLKSC        KCL(WW)
257900070131     c                   endif
258000070131      *
258100070131      * Controllo se devo memorizzare un altro riferimento
258200070131     C     §CLFTX        IFNE      *BLANKS
258300070131     C                   eval      FF = 1
258400070131     C     §CLFTX        lookup    QUA(FF)                                34
258500070131      *
258600070131     C     *IN34         IFEQ      '1'
258700070131     C                   movel     FTX(FF)       WNUM             35
258800070131     C     ' '           scan      FTX(FF)       WLUNGH
258900070131     C                   sub       1             WLUNGH
259000070131     C                   EXSR      TSTNUM
259100070131      *
259200070131     C                   SELECT
259300070131     C                   WHEN      WOKNUM <> 'S'                                Ctrl numerico
259400070201      *
259500070131     C                   WHEN      §CLROB  = 'S'
259600070131     C                   movel     WNUMOK        VABRMN
259700070131     C                   movel     'FFS'         WRMN
259800070201      *
259900070131     C                   WHEN      WRMN   <> §pvRCU
260000070131     C                   movel     WNUMOK        VABRMN
260100070131     C                   movel     'FF '         WRMN
260200070131     C                   ENDSL
260300070131      *
260400070131     C                   ENDIF
260500070131     C                   ENDIF
260600070131      *
260700070131     C                   ENDIF
260800070131     C                   ENDIF
260900070201      *
261000070201     c                   end
261100070131     C*----------------------------------------------------------------
261200070201      *  Se ho CU prendo il riferimento numerico
261300070201      *  ECCEZIONE: se ho già impostato FF obbligatorio
261400070201     C     WRMN          IFNE      'FFS'
261500070201      *
261600070201     C                   if        D1A1153  = §pvRCU and D1A1154 <> *blanks and
261700070201     c                             wcu = *blank
261800070201     C                   movel     D1A1154       WCU
261900070201      *
262000070201     C     WCU           IFNE      *BLANKS
262100070201     C                   movel     WCU           WNUM             35
262200070201     C     ' '           scan      WCU           WLUNGH
262300070201     C                   sub       1             WLUNGH
262400070201     C                   EXSR      TSTNUM
262500070201     C                   If          WOKNUM = 'S'                               Ctrl numerico
262600070201     C                   movel     WNUMOK        VABRMN
262700070201     C                   movel     §pvRCU        WRMN
262800070201     C                   Endif
262900070201     C                   ENDIF
263000070201      *
263100070201     C                   END
263200070201      *
263300070201     C                   ENDIF
263400070201      *
263500070201      *  Stampo dati riferimenti consegna
263600070201     c                   If        In_Stampa ='S'
263700070201     C   OA              EXCEPT    TEST1
263800070201     C                   EXCEPT    RIFCOA
263900070201     c                   end
264000070115      *
264100070115     C                   ENDSR
264200960527     C*----------------------------------------------------------------
264300960527     C*? DRED20 - DECODIFICA TIPO RECORD TD20
264400960527     C*----------------------------------------------------------------
264500960527     C     DRED20        BEGSR
264600960527     C*
264700070108     C                   MOVEL     SUMDAT        ED6D20DS
264800070131     C*
264900960527     C*  Controllo validità campi numerici
265000070214     c                   if        w1496 =  *blank
265100070214     c                   move      *zeros        w1496
265200070214     c                   end
265300070206     c                   if        w1496 <> *blank
265400960712     C                   TESTN                   W1496                98
265500960712     C                   MOVEL     W1496         WS1496
265600960531     C     *IN98         IFEQ      '0'
265700960527     C                   Z-ADD     13            WINDER
265800960527     C                   MOVEL     *BLANKS       WVALOR
265900960527     C                   MOVEL     W1496         WVALOR
266000960731     C                   SETON                                        05
266100960527     C                   EXSR      STPERR
266200960527     C                   GOTO      FIND20
266300960527     C                   END                                                    Imp.non numer.
266400070214     c                   end
266500070131      *
266600960527     C* Totale colli numerico
266700960527     C                   TESTN                   W7224                98
266800960531     C     *IN98         IFEQ      '0'
266900960527     C                   Z-ADD     14            WINDER
267000960527     C                   MOVEL     *BLANKS       WVALOR
267100960527     C                   MOVEL     W7224         WVALOR
267200960731     C                   SETON                                        05
267300960527     C                   EXSR      STPERR
267400960527     C                   GOTO      FIND20
267500960527     C                   END
267600070131      *
267700070205      *  Se non richiesto quello sulla testata dettaglio
267800070205     c                   if        §pvCOl  <> 'S'  or WvabPKB =  0
267900070205     C*
268000960527     C*  Se importo numerico decodifico tipo
268100960603     C     *IN98         IFEQ      '1'
268200960527     C* Se ho totali per spedizione metto in campi VAB
268300070109     C     D201496       IFEQ      99999
268400070109     C     D201496       orEQ      09999
268500970612     C                   MOVEL     'S'           WGID99
268600960712     C                   MOVE      W7224         VABNCL
268700960527     C                   ELSE
268800970612     C     WGID99        IFEQ      'N'
268900960528     C                   MOVE      W7224         WTNCL             5 0
269000960528     C                   ADD       WTNCL         VABNCL
269100960527     C                   END
269200970612     C                   END
269300960712     C                   ADD       VABNCL        WTOCOC           16 0
269400960527     C                   END
269500960527     C*
269600070205     c                   endIF
269700070205     C*
269800050520     C*  Natura Merce  (Se abilitato alla trascodifica)
269900050520     c                   if        §PUnas = 'S'
270000070109     C                   If        D204451  ='AAA'
270100070109     C                   Movel     D204440A      VABnas
270200050520     C                   EnD
270300050520     c                   end
270400070206     C*
270500070206     C*  Note generiche non caricate precedentemente
270600070206     c                   if        Wnot1 = *blank
270700070604     C*
270800070206     c                   if        D204451 = §pvSIN or D204451 = §pvICN
270900070206     c                   Movel     D204440A      Wnot1
271000070206     c                   end
271100070206     C*
271200070604     c                   if        Wnot2 = *blank and
271300070604     c                             (D204451 = §pvSIN or D204451 = §pvICN)
271400070206     c                   Move      D204440A      Wnot2
271500070206     c                   end
271600070604     C*
271700070604     C* Se invece il primo campo note era già stato caricato in precedenza
271800070604     c                   else
271900070604     C*   controllo se ci sono altre note da aggiungere nel secondo campo note
272000070604     C*
272100070604     c                   if        Wnot2 = *blank and
272200070604     c                             (D204451 = §pvSIN or D204451 = §pvICN)
272300070604     c                   MoveL     D204440A      Wnot2
272400070604     c                   end
272500070604     C*
272600070206     c                   end
272700070206     C*
272800960527     C     FIND20        ENDSR
272900070115     C*----------------------------------------------------------------
273000070115     C*? DRED22 - DECODIFICA TIPO RECORD TD22
273100070115     C*----------------------------------------------------------------
273200070115     C     DRED22        BEGSR
273300070115     C*
273400070201     C*  Istruzioni x trattar la merce  : Non gestito
273500070115     C                   MOVEL     SUMDAT        ED6D22DS
273600070115      *
273700070115     C                   ENDSR
273800960527     C*----------------------------------------------------------------
273900960527     C*? DRED25 - DECODIFICA TIPO RECORD TD25
274000960527     C*----------------------------------------------------------------
274100960527     C     DRED25        BEGSR
274200960527     C*
274300070108     C                   MOVEL     SUMDAT        ED6D25DS
274400960527     C*  Controllo validità campi numerici
274500960711     C                   TESTN                   WPESO                98
274600960527     C*  Peso
274700070109     C     D256311       IFEQ      'WT '
274800070109     C     D256411       ANDEQ     'KGM'
274900070205      *
275000070205     c     §pvPEs        IFne      'S'
275100070205     c     WvabPkb       oreq      0
275200070205      *
275300960531     C     *IN98         IFEQ      '0'
275400960527     C                   Z-ADD     15            WINDER
275500960527     C                   MOVEL     *BLANKS       WVALOR
275600960711     C                   MOVEL     WPESO         WVALOR
275700960731     C                   SETON                                        05
275800960527     C                   EXSR      STPERR
275900960527     C                   ELSE
276000960527     C*  Controllo se ho valore totale o parziale
276100960712     C     WS1496        IFEQ      99999
276200020807     C     WS1496        orEQ      09999
276300070109     C                   Z-ADD     D256314       VABPKB                         Peso
276400960527     C                   ELSE
276500970612     C     WGID99        IFEQ      'N'
276600070109     C                   ADD       D256314       VABPKB                         Peso
276700960527     C                   END
276800970612     C                   END
276900070109     C     D256313       IFEQ      'AAG'
277000070109     C                   ADD       D256314       WTOPLC           16 0          ???
277100960703     C                   END
277200070109     C     D256313       IFEQ      'AAL'
277300070109     C                   ADD       D256314       WTOPNC           16 0          ???
277400960703     C                   END
277500960527     C                   END
277600070205      *
277700070205     c                   EndIF
277800070205      *
277900960527     C                   END
278000061127      ***
278100061128     C*  Peso in Grammi  se abilitato a utilizzarlo
278200061128      ***
278300061128     C     §puGR         IFEQ      'S'
278400070109     C     D256311       IFEQ      'WT '
278500070109     C     D256411       ANDEQ     '163'
278600070205      *
278700070205     c     §pvPEs        IFne      'S'
278800070205     c     WvabPkb       oreq      0
278900070205      *
279000061127     C     *IN98         IFEQ      '0'
279100061127     C                   Z-ADD     15            WINDER
279200061127     C                   MOVEL     *BLANKS       WVALOR
279300061127     C                   MOVEL     WPESO         WVALOR
279400061127     C                   SETON                                        05
279500061127     C                   EXSR      STPERR
279600061127     C                   ELSE
279700061127     C*  Controllo se ho valore totale o parziale
279800061127     C     WS1496        IFEQ      99999
279900061127     C     WS1496        orEQ      09999
280000070109     C     D256314       div       1000          VABPKB                         Peso
280100061127     C                   ELSE
280200061127     C     WGID99        IFEQ      'N'
280300070109     C     D256314       div       1000          Peso_inKG        12 3          Peso
280400061127     C                   add       Peso_inKG     VABPKB                         Peso
280500061127     C                   END
280600061127     C                   END
280700070109     C     D256313       IFEQ      'AAG'
280800070109     C     D256314       div       1000          Peso_inKG        12 3          Peso
280900061127     C                   ADD       Peso_inKG     WTOPLC
281000061127     C                   END
281100070109     C     D256313       IFEQ      'AAL'
281200070109     C     D256314       div       1000          Peso_inKG        12 3          Peso
281300061127     C                   ADD       Peso_inKG     WTOPNC
281400061127     C                   END
281500061127     C                   END
281600070205      *
281700070205     c                   endIF
281800070205      *
281900061127     C                   END
282000061128     c                   endIF
282100990616      *  Volume
282200070109     C     D256311       IFEQ      'VOL'
282300070109     C     D256411       ANDEQ     'MTQ'
282400990616     C     *IN98         IFEQ      '0'
282500960527     C                   Z-ADD     16            WINDER
282600960527     C                   MOVEL     *BLANKS       WVALOR
282700960527     C                   MOVEL     WCOM16        WVALOR
282800960731     C                   SETON                                        05
282900960527     C                   EXSR      STPERR
283000990616     C                   ELSE
283100990616      *  Controllo se ho valore totale o parziale
283200990616     C     WS1496        IFEQ      99999
283300020807     C     WS1496        orEQ      09999
283400070109     C                   Z-ADD     D256314       VABVLB                         Volume
283500070109     C                   Z-ADD     D256314       PVLB
283600990616     C                   ELSE
283700990616     C     WGID99        IFEQ      'N'
283800070109     C                   ADD       D256314       VABVLB                         Volume
283900070109     C                   ADD       D256314       PVLB
284000990616     C                   END
284100990616     C                   END
284200990616     C                   END
284300990616     C                   END
284400990616      *
284500960628     C*  Stampo dati articolo
284600070109     C     D207140A      IFNE      *BLANKS
284700070109     C     D207143A      ORNE      *BLANKS
284800070109     C     D204440A      ORNE      *BLANKS
284900040607     c                   If        In_Stampa ='S'
285000960628     C   OA              EXCEPT    TEST1
285100960628     C                   EXCEPT    DATART
285200040607     c                   end
285300960628     C                   END
285400960829     C* Ricerco CAP
285500961104     C     WCAP          IFNE      *BLANKS
285600070109     C     D256311       IFEQ      'WT '
285700040302      *
285800040302      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
285900040302     C                   CLEAR                   FNLV55DS
286000040302     C                   MOVEL     'P'           D55TPT
286100040302     C                   MOVEL     WOGGI         D55DRF
286200040302     C                   MOVEL     VABLNP        D55LNP
286300040302     C                   MOVEL     VABLNP        D55LIN
286400040302     C                   CALL      'FNLV55R'
286500040302     C                   PARM                    FNLV55DS
286600040302     C                   MOVE      D55TFP        COMTFP            3 0
286700040302      *
286800040302     C*  PASSO IL TERMINAL PARTENZA
286900000120     C                   CLEAR                   TISI95DS
287000970918     C                   Z-ADD     COMTFP        I95TFP
287100980602     C                   MOVEL     VABTSP        I95TSP
287200980602     C                   MOVEL     'S'           I95FRE
287300970710     C                   MOVEL     '3'           I95TCN
287400970710     C                   MOVEL     WCAP          I95CAP
287500970710     C                   MOVEL     VABNZD        I95NAR
287600970710     C                   MOVEL     VABLOD        I95LOC
287700970710     C                   MOVEL     VABIND        I95IND
287800970710     C* Se gestita seconda linea di arrivo passo peso - volume
287900970710     C* e numero colli effettivo
288000970710     C     §PULNA        IFEQ      'S'
288100970710     C                   Z-ADD     VABPKB        I95LKG
288200970710     C                   Z-ADD     VABVLB        I95LMC
288300970710     C                   END
288400971202     C*
288500971202     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
288600971202     C                   SELECT
288700971202     C     VABCBO        WHENEQ    '1 '
288800971202     C                   MOVEL     *BLANKS       I95FCA
288900971202     C                   MOVEL     'F'           I95TPO
289000971202     C     VABCBO        WHENEQ    '2 '
289100971202     C                   MOVEL     *BLANKS       I95FCA
289200971202     C                   MOVEL     'A'           I95TPO
289300971202     C     VABCBO        WHENEQ    '4 '
289400971202     C                   MOVEL     'S'           I95FCA
289500971202     C                   MOVEL     'F'           I95TPO
289600971202     C     VABCBO        WHENEQ    '6 '
289700971202     C                   MOVEL     'S'           I95FCA
289800971202     C                   MOVEL     'A'           I95TPO
289900971202     C                   OTHER
290000971202     C                   MOVEL     'F'           I95TPO
290100971202     C     VABCAS        IFGT      0
290200971202     C                   MOVEL     'S'           I95FCA
290300971202     C                   ELSE
290400971202     C                   MOVEL     *BLANKS       I95FCA
290500971202     C                   END
290600971202     C                   ENDSL
290700971202     C*
290800970710     C                   CALL      'TISI95R'
290900000120     C                   PARM                    TISI95DS
291000970710     C* Reperisco i dati da CAP
291100110615     C                   move      'S'           Instradamento
291200970710     C                   MOVEL     O95PRV        VABPRD
291300971202     C                   MOVEL     O95LNA        VABLNA
291400970918     C                   MOVEL     O95ZNC        VABZNC
291500970710     C                   MOVEL     WCAP          VABCAD
291600970710     C*
291700970710     C     O95ERR        IFNE      *BLANKS
291800970710     C     O95FIT        ANDEQ     'S'
291900960829     C*  Cap non trovato
292000960829     C                   Z-ADD     10            WINDER
292100960829     C                   MOVEL     *BLANKS       WVALOR
292200960829     C                   MOVEL     WCAP          WVALOR
292300960829     C                   SETON                                        05
292400960829     C                   EXSR      STPERR
292500960829     C                   END
292600961017     C                   END
292700961104     C                   ELSE
292800961104     C                   CLEAR                   VABPRD
292900961104     C                   CLEAR                   VABLNA
293000961104     C                   CLEAR                   VABZNC
293100961104     C                   CLEAR                   VABCAD
293200961104     C*  Cap non trovato
293300961104     C                   Z-ADD     10            WINDER
293400961104     C                   MOVEL     *BLANKS       WVALOR
293500961104     C                   MOVEL     WCAP          WVALOR
293600961104     C                   SETON                                        05
293700961104     C                   EXSR      STPERR
293800961104     C                   END
293900960527     C*
294000960527     C                   ENDSR
294100070115     C*----------------------------------------------------------------
294200070115     C*? DRED26 - DECODIFICA TIPO RECORD TD26
294300070115     C*----------------------------------------------------------------
294400070115     C     DRED26        BEGSR
294500070115     C*
294600070201     C*  Dimensioni   : Non gestito
294700070115     C                   MOVEL     SUMDAT        ED6D26DS
294800070115      *
294900070115     C                   ENDSR
295000070115     C*----------------------------------------------------------------
295100070115     C*? DRED27 - DECODIFICA TIPO RECORD TD27
295200070115     C*----------------------------------------------------------------
295300070115     C     DRED27        BEGSR
295400070115     C*
295500070201     C*  Riferimento a livello di segnacollo se abilitato sulla tabella PT
295600070201     C*     deve prendere questo oppure se non è stato inserito precedentemente
295700070115     C                   MOVEL     SUMDAT        ED6D27DS
295800070201     C*
295900070201      * ?--------------------------*
296000070201      *  Prende il riferimento se non era stato precedentemente caricato
296100070201      *     o se sulla PT c'è la forzatura a livello segnacollo.
296200070201     c                   if        Primo_RFFcollo = *blank
296300070201      **
296400070201     c                   if        wAGE = *blank or §PVRFS = 'S'
296500070201     c                   if        D271153 = §PVRFF and D271154 <> *BLANKS
296600070201     C                   movel     D271154       WAGE
296700070201      *
296800070201     C                   IF         WAGE <> *BLANKS
296900070201     C                   movel     WAGE          WNSP             35
297000070201     C                   movel     WAGE          WNUM             35
297100070201     C     ' '           scan      WAGE          WLUNGH
297200070201     C                   sub       1             WLUNGH
297300070201     C                   EXSR      TSTNUM
297400070201     C                   IF         WOKNUM = 'S'                                Ctrl numerico
297500070201     C                   movel     WAGE          VABRMA
297600070201     c                   eval      Primo_RFFcollo = 'S'
297700070201     C                   IF         WRMN = §PVRFF or WRMN = *blanks
297800070201     C                   movel     WNUMOK        VABRMN
297900070201     C                   movel     §PVRFF        WRMN
298000070201     C                   EndIF
298100070201     C                   ENDIF
298200070201      *
298300070201     C                   ENDIF
298400070201     C                   End
298500070201      *
298600070201     C                   End
298700070201      *
298800070201     c                   else
298900070201      * Se ci sono ulteriori Riferimenti e sono da riportare su VABRMO
299000070201     c                   if        §PVRFS = 'S' and §PVRSE ='S' and
299100070201     c                             D271153 = §PVRFF and D271154 <> *BLANKS and
299200070201     c                             D271154 <> wAGE
299300070201      *
299400070201     c                   eval      conta_rif = conta_rif + 1
299500070201      *
299600070201      *  al max possono essere 2 riferimenti differenti dal principale
299700070201     c                   if        conta_rif <= 2
299800070201     C                   movel     D271154       WAGE2
299900070201      *
300000070201     C                   IF         WAGE2 <> *BLANKS
300100070201     C     ' '           scan      WAGE2         WLUNGH
300200070201     C                   sub       1             WLUNGH
300300070206      *
300400070206     c                   if        conta_rif = 1
300500070206     c                   clear                   SUvabrmo
300600070206     c                   end
300700070206      *
300800070206     C                   eval      SUvabRMo = %Trim(suVABRMo) + '.' +
300900070201     C                             %Subst(Wage2:1:WLUNGH)
301000070201     C                   ENDIF
301100070201     c                   endIF
301200070201      *
301300070201     c                   end
301400070201     c                   end
301500070201     C*----------------------------------------------------------------
301600070201      *  Se ho CU prendo il riferimento numerico
301700070201      *  ECCEZIONE: se ho già impostato FF obbligatorio
301800070201     c                   if        Primo_RCUcollo = *blank
301900070201      *
302000070201     c                   if        wCU  = *blank or §PVRFS = 'S'
302100070201     C                   if        D271153  = §pvRCU and D271154 <> *blanks
302200070201     C                   movel     D271154       WCU
302300070201      *
302400070201     C     WCU           IFNE      *BLANKS
302500070201     C                   movel     WCU           WNUM             35
302600070201     C     ' '           scan      WCU           WLUNGH
302700070201     C                   sub       1             WLUNGH
302800070201     C                   EXSR      TSTNUM
302900070201     C                   If          WOKNUM = 'S'                               Ctrl numerico
303000070201     c                   eval      Primo_RCUcollo = 'S'
303100070201     C                   movel     WNUMOK        VABRMN
303200070201     C                   movel     §pvRCU        WRMN
303300070201     C                   Endif
303400070201     C                   ENDIF
303500070201      *
303600070201     C                   END
303700070201      *
303800070201     C                   End
303900070201      *
304000070201     C                   Else
304100070201      * Se ci sono ulteriori Riferimenti Numerici non vengono considerati
304200070201     c                   if        §PVRFS = 'S' and §PVRSE ='S' and
304300070201     c                             D271153 = §PVRFF and D271154 <> *BLANKS and
304400070201     c                             D271154 <> wCU
304500070201      *    ??????
304600070201     c                   end
304700070201     C                   End
304800070201      *
304900070201      *  Stampo dati riferimenti consegna
305000070201     c                   If        In_Stampa ='S'
305100070201     C   OA              EXCEPT    TEST1
305200070201     C                   EXCEPT    RIFSCO
305300070201     c                   end
305400070115      *
305500070115     C                   ENDSR
305600070115     C*----------------------------------------------------------------
305700070115     C*? DRED28 - DECODIFICA TIPO RECORD TD28
305800070115     C*----------------------------------------------------------------
305900070115     C     DRED28        BEGSR
306000070115     C*
306100070202     C*  Data         : Non gestito
306200070115     C                   MOVEL     SUMDAT        ED6D28DS
306300070115      *
306400070115     C                   ENDSR
306500960527     C*----------------------------------------------------------------
306600960527     C*? DRED30 - DECODIFICA TIPO RECORD TD30
306700960527     C*----------------------------------------------------------------
306800960527     C     DRED30        BEGSR
306900980218      *
307000070108     C                   MOVEL     SUMDAT        ED6D30DS
307100040607     c                   If        In_Stampa ='S'
307200960628     C   OA              EXCEPT    TEST1
307300040607     c                   end
307400980218      *
307500980408      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
307600001031      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
307700001031     C     §1BF12        IFEQ      'E'
307800070205     c     d30ski        andne     *blank
307900070216     c                   movea     D30_1         X30_1
308000070216     c                   movea     D30_2         X30_2
308100980218     C                   EXSR      SEGNEE
308200980218     C                   ELSE
308300980218      *
308400040227      *  La prima volta controllo congruità numero segnacollo
308500040227     C     §PUBS1        IFNE      *BLANKS
308600040227     C     VABnrs        ANDNE     0
308700980218      *  Se il codice trattamento merce prevede che segnacollino i
308800980218      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
308900980218     C     §1BFG7        IFEQ      'S'
309000960731     C     §1BFG1        OREQ      'S'
309100980218      *  calcolo segnacollo
309200070205     c     d30ski        ifne      *blank
309300070216     c                   movea     D30_1         X30_1
309400070216     c                   movea     D30_2         X30_2
309500960725     C                   EXSR      SGNELA
309600070202     c                   end
309700960725     C                   END
309800960702     C                   END
309900980218      *
310000980218     C                   END
310100980218      *
310200960527     C                   ENDSR
310300070115     C*----------------------------------------------------------------
310400070115     C*? DRED31 - DECODIFICA TIPO RECORD TD31
310500070115     C*----------------------------------------------------------------
310600070115     C     DRED31        BEGSR
310700070115     C*
310800070202     C*  Riferimenti  : Non gestito
310900070115     C                   MOVEL     SUMDAT        ED6D31DS
311000070115      *
311100070115     C                   ENDSR
311200070115     C*----------------------------------------------------------------
311300070115     C*? DRED32 - DECODIFICA TIPO RECORD TD32
311400070115     C*----------------------------------------------------------------
311500070115     C     DRED32        BEGSR
311600070115     C*
311700070115     C                   MOVEL     SUMDAT        ED6D32DS
311800070202     C*
311900070202     c                   If        In_Stampa ='S'
312000070202     C   OA              EXCEPT    TEST1
312100070202     c                   end
312200070202      *
312300070202      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
312400070202      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
312500070202     C     §1BF12        IFEQ      'E'
312600070205     c     d32ski        andne     *blank
312700070216     c                   movea     D32_1         X30_1
312800070216     c                   clear                   X30_2
312900070202     C                   EXSR      SEGNEE
313000070202     C                   ELSE
313100070202      *
313200070202      *  La prima volta controllo congruità numero segnacollo
313300070202     C     §PUBS1        IFNE      *BLANKS
313400070202     C     VABnrs        ANDNE     0
313500070202      *  Se il codice trattamento merce prevede che segnacollino i
313600070202      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
313700070202     C     §1BFG7        IFEQ      'S'
313800070202     C     §1BFG1        OREQ      'S'
313900070202      *  calcolo segnacollo
314000070205     c     d32ski        ifne      *blank
314100070216     c                   movea     D32_1         X30_1
314200070216     c                   clear                   X30_2
314300070202     C                   EXSR      SGNELA
314400070202     c                   end
314500070202     C                   END
314600070202     C                   END
314700070202      *
314800070202     C                   END
314900070115      *
315000070115     C                   ENDSR
315100070115     C*----------------------------------------------------------------
315200070115     C*? DRED35 - DECODIFICA TIPO RECORD TD35
315300070115     C*----------------------------------------------------------------
315400070115     C     DRED35        BEGSR
315500070115     C*
315600070202     C* Gestione merci pericolose : Non gestito
315700070115     C                   MOVEL     SUMDAT        ED6D35DS
315800070115      *
315900070115     C                   ENDSR
316000070115     C*----------------------------------------------------------------
316100070115     C*? DRED36 - DECODIFICA TIPO RECORD TD36
316200070115     C*----------------------------------------------------------------
316300070115     C     DRED36        BEGSR
316400070115     C*
316500070202     C* Contatti                  : Non gestito
316600070115     C                   MOVEL     SUMDAT        ED6D36DS
316700070115      *
316800070115     C                   ENDSR
316900070115     C*----------------------------------------------------------------
317000070115     C*? DRED37 - DECODIFICA TIPO RECORD TD37
317100070115     C*----------------------------------------------------------------
317200070115     C     DRED37        BEGSR
317300070115     C*
317400070202     C* Comunicazioni contatti    : Non gestito
317500070115     C                   MOVEL     SUMDAT        ED6D37DS
317600070115      *
317700070115     C                   ENDSR
317800070115     C*----------------------------------------------------------------
317900070115     C*? DRED40 - DECODIFICA TIPO RECORD TD40
318000070115     C*----------------------------------------------------------------
318100070115     C     DRED40        BEGSR
318200070115     C*
318300070202     C* Note Pallet non gestito   : Non gestito
318400070115     C                   MOVEL     SUMDAT        ED6D40DS
318500070115      *
318600070115     C                   ENDSR
318700070115     C*----------------------------------------------------------------
318800070115     C*? DRED41 - DECODIFICA TIPO RECORD TD41
318900070115     C*----------------------------------------------------------------
319000070115     C     DRED41        BEGSR
319100070115     C*
319200070202     C* Dettagli Movimentazione   : Non gestito
319300070115     C                   MOVEL     SUMDAT        ED6D41DS
319400070115      *
319500070115     C                   ENDSR
319600070115     C*----------------------------------------------------------------
319700070115     C*? DRED42 - DECODIFICA TIPO RECORD TD42
319800070115     C*----------------------------------------------------------------
319900070115     C     DRED42        BEGSR
320000070115     C*
320100070202     C* Misure                    : Non gestito
320200070115     C                   MOVEL     SUMDAT        ED6D42DS
320300070115      *
320400070115     C                   ENDSR
320500070115     C*----------------------------------------------------------------
320600070115     C*? DRED43 - DECODIFICA TIPO RECORD TD43
320700070115     C*----------------------------------------------------------------
320800070115     C     DRED43        BEGSR
320900070115     C*
321000070202     C* Nome ed indirizzo         : Non gestito
321100070115     C                   MOVEL     SUMDAT        ED6D43DS
321200070115      *
321300070115     C                   ENDSR
321400070216      *----------------------------------------------------------------
321500070216      *? Input segnacolli su schiera di 20 elementi
321600070216      *----------------------------------------------------------------
321700070216     C     INP_Segnacol  BEGSR
321800070216      *
321900070216     c                   clear                   quanti_PCI        3 0
322000070216     c                   if        §puEL1 = 0
322100070216     c                   z-add     10            quanti_PCI
322200070216     c                   else
322300070216     c                   z-add     §puEL1        quanti_PCI
322400070216     c                   end
322500070216      *
322600070216      *  Pulisce la schiera dei segnacolli da elaborare
322700070216     c                   clear                   x30
322800070216     c                   z-add     0             Nr_x30            3 0
322900070216      *
323000070216      * 2 schiere per ogni TD30: x30_1 e x30_2
323100070216      *   riporto tutto sulla schiera generica X30
323200070216      *
323300070216      *  ciclo sui primi 10
323400070216     c                   do        10            limite            3 0
323500070216      * Limita quanti elementi passati in ogni PCI
323600070216      *   devono essere considerati
323700070216     c                   if        limite > quanti_PCI
323800070216     c                   Leave
323900070216     c                   end
324000070216      *
324100070216      *  Carica la schiera generale di tutti i segnacolli
324200070216     c                   if        x30_1(limite) <> *blank
324300070216     c                   add       1             Nr_x30
324400070216     c                   eval      X30(Nr_x30) = x30_1(limite)
324500070216     c                   end
324600070216     c                   enddo
324700070216      *
324800070216      *  ciclo sui secondi 10
324900070216     c                   do        10            limite
325000070216      * Limita quanti elementi passati in ogni PCI
325100070216      *   devono essere considerati
325200070216     c                   if        limite > quanti_PCI
325300070216     c                   Leave
325400070216     c                   end
325500070216      *
325600070216      *  Carica la schiera generale di tutti i segnacolli
325700070216     c                   if        x30_2(limite) <> *blank
325800070216     c                   add       1             Nr_x30
325900070216     c                   eval      X30(Nr_x30) = x30_2(limite)
326000070216     c                   end
326100070216     c                   enddo
326200070216      *
326300070216     C                   ENDSR
326400980218      *----------------------------------------------------------------
326500980218      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
326600980218      *----------------------------------------------------------------
326700980218     C     SEGNEE        BEGSR
326800070216      *
326900070216     c                   exsr      INP_Segnacol
327000070215      *
327100980218      *  Loop lettura PCI: fino a che non arrivo a fine schiera (20 elementi)
327200070216     C                   DO        20            x                              --- 01 ---
327300980218      *
327400070202     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
327500070202     C     x30(X)        ANDNE     *LOVAL                                       --- 02 ---
327600020909      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
327700980219     C                   ADD       1             §§
327800070202     C                   MOVEL     x30(X)        SGE(§§)
327900980219     C     XW            IFLT      3                                            --- 03 ---
328000980218     C                   ADD       1             XW                3 0
328100980218     C                   MOVEL     DSSGN         SGT(XW)
328200980219     C                   END                                                    --- 03 ---
328300980219     C     XW            IFEQ      3                                            --- 03 ---
328400040607     c                   If        In_Stampa ='S'
328500980218     C                   EXCEPT    DATSGN
328600040607     c                   end
328700980218     C                   Z-ADD     *ZEROS        XW
328800980218     C                   CLEAR                   SGT
328900980219     C                   END                                                    --- 03 ---
329000980219      *
329100980219     C                   END                                                    --- 02 ---
329200070216      *
329300980219     C                   END                                                    --- 01 ---
329400980218      *
329500980218     C                   ENDSR
329600960725     C*----------------------------------------------------------------
329700960912     C*? SGNELA - DECODIFICA elabora nr.segnacollo
329800960725     C*----------------------------------------------------------------
329900960725     C     SGNELA        BEGSR
330000070216      *
330100070216     c                   exsr      INP_Segnacol
330200070216     C*
330300070216     C*  Loop lettura PCI: fino a che non arrivo a fine sch.20 elem.
330400070216     C*  in base al numero elementi validi carico sgn.
330500070216     C                   DO        20            X                              --- 01 ---
330600070216     C*
330700070202     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
330800070202     C     x30(X)        ANDNE     *LOVAL
330900960912     C                   CLEAR                   DSSGN
331000960911     C*  Controllo se devo ricevere la linea di partenza
331100960912     C                   MOVEL     'N'           WERRO             1
331200960912     C     §PULP1        IFGT      0                                            --- 03 ---
331300960911     C                   EXSR      REPLNP
331400960912     C                   END                                                    --- 03 ---
331500960911     C*  Controllo se devo ricevere la linea di arrivo
331600960912     C     §PULA1        IFGT      0                                            --- 03 ---
331700960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
331800960911     C                   EXSR      REPLNA
331900960912     C                   END                                                    --- 03 ---
332000960911     C*  Controllo se devo ricevere il numero di serie
332100960912     C     §PUNR1        IFGT      0                                            --- 03 ---
332200960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
332300960911     C                   EXSR      REPNRS
332400960912     C                   END                                                    --- 03 ---
332500960911     C*  Controllo se devo ricevere il numero segnacollo
332600960912     C     §PUSG1        IFGT      0                                            --- 03 ---
332700960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
332800960911     C                   EXSR      REPSGN
332900960912     C                   END                                                    --- 03 ---
333000960911     C*  Controllo se devo ricevere la zona di consegna
333100960912     C     §PUZN1        IFGT      0                                            --- 03 ---
333200960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
333300960911     C                   EXSR      REPZNC
333400960912     C                   END                                                    --- 03 ---
333500960912     C*  Imposto da VAB i dati a zero se mi è stato passato
333600960912     C*  numero segnacollo valido
333700960912     C     WERRO         IFEQ      'N'                                          --- 03 ---
333800960912     C     SGNLNP        IFEQ      0                                            --- 04 ---
333900040302     C                   Z-ADD     VABLNP        SGNLNP
334000960912     C                   END                                                    --- 04 ---
334100960912     C     SGNLNA        IFEQ      0                                            --- 04 ---
334200960911     C                   Z-ADD     VABLNA        SGNLNA
334300960912     C                   END                                                    --- 04 ---
334400960912     C     SGNNRS        IFEQ      0                                            --- 04 ---
334500960911     C                   Z-ADD     VABNRS        SGNNRS
334600960912     C                   END                                                    --- 04 ---
334700960912     C     SGNZNC        IFEQ      0                                            --- 04 ---
334800960911     C                   Z-ADD     VABZNC        SGNZNC
334900960912     C                   END                                                    --- 04 ---
335000960912     C*  mi reimposto LNA e zona da segnacollo
335100960912     C                   Z-ADD     SGNLNA        VABLNA
335200960912     C                   Z-ADD     SGNZNC        VABZNC
335300020909     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
335400960912     C                   ADD       1             XY
335500960912     C                   MOVE      SGNNSC        SGN(XY)
335600960912     C     XW            IFLT      3                                            --- 04 ---
335700960912     C                   ADD       1             XW                3 0
335800960912     C                   MOVEL     DSSGN         SGT(XW)
335900960912     C                   END                                                    --- 04 ---
336000960912     C     XW            IFEQ      3                                            --- 04 ---
336100040607     c                   If        In_Stampa ='S'
336200960912     C                   EXCEPT    DATSGN
336300040607     c                   end
336400960912     C                   Z-ADD     0             XW
336500960912     C                   CLEAR                   SGT
336600960912     C                   END                                                    --- 04 ---
336700960912     C                   END                                                    --- 03 ---
336800070216     C*
336900960912     C                   END                                                    --- 02 ---
337000070216     C*
337100960912     C                   END                                                    --- 01 ---
337200990715      *
337300990715      * SE NON IMPOSTATO SEGNACOLLO FORZO 1 E PULISCO IL 1° ELEMENTO
337400990715     C     XY            IFEQ      *ZEROS
337500990715     C                   Z-ADD     1             XY
337600990715     C                   CLEAR                   SGN(XY)
337700990715     C                   END
337800990715      *
337900960912     C                   ENDSR
338000960912     C*----------------------------------------------------------------
338100960912     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
338200960912     C*----------------------------------------------------------------
338300960912     C     REPLNP        BEGSR
338400960912     C*
338500960911     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
338600960911     C*  (a partire dalla posizione iniziale indicata in tabella)
338700960911     C*  dal numero segnacollo passatomi dal partner
338800960911     C                   Z-ADD     §PULP1        WP                2 0
338900070202     C     3             SUBST     x30(X):WP     WLNP              3
339000960911     C*  controllo che la lnp calcolata sia numerica
339100960911     C                   TESTN                   WLNP                 98
339200960911     C*  se è numerica la carico
339300960911     C     *IN98         IFEQ      '1'
339400960911     C                   MOVE      WLNP          SGNLNP
339500960911     C                   ELSE
339600960911     C*  se non è numerica stampo errore
339700960912     C                   MOVEL     'S'           WERRO             1
339800960911     C                   Z-ADD     25            WINDER            3 0
339900960912     C                   MOVEL     WLNP          WVALOR           35
340000960912     C                   SETON                                        05
340100960911     C                   EXSR      STPERR
340200960912     C                   GOTO      FINLNP
340300960911     C                   END
340400020909     C*  se la lnp non è uguale a quella calcolata per EDiVAB
340500020909     C*  lo segnalo e prendo per buona la lnp del EDiVAB
340600040302     C     SGNLNP        IFNE      VABLNP
340700960912     C                   MOVEL     'S'           WERRO             1
340800960912     C                   Z-ADD     26            WINDER            3 0
340900960912     C                   MOVEL     SGNLNP        WCOMO5            5
341000960912     C                   MOVE      '-'           WCOMO5
341100040302     C                   MOVE      VABLNP        WCOMO9            9
341200960912     C                   MOVEL     WCOMO5        WCOMO9            9
341300960912     C                   MOVEL     WCOMO9        WVALOR           35
341400960912     C                   SETON                                        05
341500960912     C                   EXSR      STPERR
341600960912     C                   END
341700960725     C*
341800960912     C     FINLNP        ENDSR
341900960912     C*----------------------------------------------------------------
342000960912     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
342100960912     C*----------------------------------------------------------------
342200960912     C     REPLNA        BEGSR
342300960912     C*
342400960912     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
342500960912     C*  (a partire dalla posizione iniziale indicata in tabella)
342600960912     C*  dal numero segnacollo passatomi dal partner
342700960912     C                   Z-ADD     §PULA1        WP                2 0
342800070202     C     3             SUBST     x30(X):WP     WLNA              3
342900960912     C*  controllo che la lna calcolata sia numerica
343000960912     C                   TESTN                   WLNA                 98
343100960912     C*  se è numerica la carico
343200960912     C     *IN98         IFEQ      '1'
343300960912     C                   MOVE      WLNA          SGNLNA
343400960912     C                   ELSE
343500960912     C*  se non è numerica stampo errore
343600960912     C                   Z-ADD     27            WINDER            3 0
343700960912     C                   MOVEL     WLNA          WVALOR           35
343800960912     C                   SETON                                        05
343900960912     C                   EXSR      STPERR
344000960912     C                   MOVEL     'S'           WERRO             1
344100960912     C                   GOTO      FINLNA
344200960912     C                   END
344300960912     C*
344400960912     C     FINLNA        ENDSR
344500960912     C*----------------------------------------------------------------
344600960912     C*? REPNRS - Reperisce numero serie
344700960912     C*----------------------------------------------------------------
344800960912     C     REPNRS        BEGSR
344900960912     C*
345000960912     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
345100960912     C*  (a partire dalla posizione iniziale indicata in tabella)
345200960912     C*  dal numero segnacollo passatomi dal partner
345300960912     C                   Z-ADD     §PUNR1        WP                2 0
345400070202     C     2             SUBST     x30(X):WP     WNRS              2
345500960912     C*  controllo che il nrs calcolata sia numerico
345600960912     C                   TESTN                   WNRS                 98
345700960912     C*  se è numerica la carico
345800960912     C     *IN98         IFEQ      '1'
345900960912     C                   MOVE      WNRS          SGNNRS
346000960912     C                   ELSE
346100960912     C*  se non è numerico stampo errore
346200960912     C                   Z-ADD     28            WINDER            3 0
346300960912     C                   MOVEL     WNRS          WVALOR           35
346400960912     C                   SETON                                        05
346500960912     C                   EXSR      STPERR
346600960912     C                   MOVEL     'S'           WERRO             1
346700960912     C                   GOTO      FINNRS
346800960912     C                   END
346900020909     C*  se il nrs non è uguale a quella calcolata per EDiVAB
347000960912     C*  lo segnalo e prendo per buona il nrs del segnacollo
347100960912     C     SGNNRS        IFNE      VABNRS
347200960912     C                   Z-ADD     29            WINDER            3 0
347300960912     C                   MOVEL     SGNNRS        WCOMO4            4
347400960912     C                   MOVE      '-'           WCOMO5
347500960912     C                   MOVE      VABNRS        WCOMO7            8
347600960912     C                   MOVEL     WCOMO4        WCOMO7            8
347700960912     C                   MOVEL     WCOMO7        WVALOR           35
347800960912     C                   MOVEL     'S'           WERRO             1
347900960912     C                   SETON                                        05
348000960912     C                   EXSR      STPERR
348100960912     C                   END
348200960912     C*
348300960912     C     FINNRS        ENDSR
348400960912     C*----------------------------------------------------------------
348500960912     C*? REPZNC - Reperisce zona consegna
348600960912     C*----------------------------------------------------------------
348700960912     C     REPZNC        BEGSR
348800960912     C*
348900960912     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
349000960912     C*  (a partire dalla posizione iniziale indicata in tabella)
349100960912     C*  dal numero segnacollo passatomi dal partner
349200960912     C                   Z-ADD     §PUZN1        WP                2 0
349300070202     C     2             SUBST     x30(X):WP     WZNC              2
349400960912     C*  controllo che la zona calcolata sia numerica
349500960912     C                   TESTN                   WZNC                 98
349600960912     C*  se è numerica la carico
349700960912     C     *IN98         IFEQ      '1'
349800960912     C                   MOVE      WZNC          SGNZNC
349900960912     C                   ELSE
350000960912     C*  se non è numerica stampo errore
350100960912     C                   MOVEL     'S'           WERRO             1
350200960912     C                   Z-ADD     30            WINDER            3 0
350300960912     C                   MOVEL     WZNC          WVALOR           35
350400960912     C                   SETON                                        05
350500960912     C                   EXSR      STPERR
350600960912     C                   GOTO      FINZNC
350700960912     C                   END
350800960912     C*
350900960912     C     FINZNC        ENDSR
351000960912     C*----------------------------------------------------------------
351100960912     C*? REPSGN - Reperisce numero segnacollo
351200960912     C*----------------------------------------------------------------
351300960912     C     REPSGN        BEGSR
351400960912     C*
351500960912     C*  se viene passata nr.segnacollo estraggo sottostringa
351600960912     C*  lunga 7 chr (a partire dalla posizione iniziale
351700960912     C*  indicata in tabella)
351800960912     C*  dal numero segnacollo passatomi dal partner
351900960912     C                   Z-ADD     §PUSG1        WP                2 0
352000070202     C     7             SUBST     x30(X):WP     WSGN              7
352100960912     C*  controllo che il nr.segnacollo calcolato sia numerico
352200960912     C                   TESTN                   WSGN                 98
352300960912     C*  se è numerico lo carico
352400960912     C     *IN98         IFEQ      '1'
352500960912     C                   MOVE      WSGN          SGNNSC
352600960912     C                   ELSE
352700960912     C*  se non è numerica stampo errore
352800960912     C                   Z-ADD     31            WINDER            3 0
352900960912     C                   MOVEL     WSGN          WVALOR           35
353000960912     C                   SETON                                        05
353100960912     C                   MOVEL     'S'           WERRO             1
353200960912     C                   EXSR      STPERR
353300960912     C                   END
353400960912     C*
353500960912     C                   ENDSR
353600960828     C*----------------------------------------------------------------
353700960828     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
353800960828     C*----------------------------------------------------------------
353900960828     C     CNVCAP        BEGSR
354000960828     C*
354100960828     C                   MOVEL     *BLANKS       WCAP              9
354200070202     C     CAP_9ALFA     IFNE      '0'
354300070202     C     '  '          SCAN      CAP_9ALFA     LENGHT            2 0
354400960828     C                   SUB       1             LENGHT
354500960828     C     LENGHT        IFLE      5
354600960904     C     LENGHT        ANDGT     0
354700960828     C                   Z-ADD     LENGHT        T                 2 0
354800960828     C                   Z-ADD     5             S                 2 0
354900070202     C                   MOVEA     CAP_9ALFA     CAP9
355000960828     C                   MOVEL     *ZEROS        CAP5
355100960828     C                   DO        LENGHT
355200960828     C                   MOVE      CAP9(T)       CAP5(S)
355300960828     C                   SUB       1             S
355400960828     C                   SUB       1             T
355500960828     C                   END
355600960926     C                   MOVEA     CAP5          WCAP5             5
355700961017     C     WCAP5         IFEQ      '0'
355800960926     C                   MOVEL     *BLANKS       WCAP
355900960926     C                   ELSE
356000960926     C                   MOVEA     CAP5          WCAP
356100960926     C                   END
356200960828     C*
356300960828     C                   ELSE
356400070202     C                   MOVEL     CAP_9ALFA     WCAP
356500960828     C                   END
356600961017     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
356700961017     C     WCAP          IFNE      *BLANKS
356800961017     C     WFLCPF        ANDNE     0
356900961017     C                   MOVEL     WFLCPF        WELE              1 0
357000961017     C                   MOVE      WFLCPF        WPOS              1 0
357100961017     C                   MOVEL     *BLANK        WCPF
357200961017     C     WELE          SUBST     WCAP:WPOS     WCPF              9
357300961017     C                   ELSE
357400961017     C                   MOVEL     *BLANKS       WCPF
357500961017     C                   END
357600961104     C                   END
357700960828     C*
357800960828     C                   ENDSR
357900000125      *----------------------------------------------------------------
358000000125      *? AZZCMP - Azzero campi di importazione dati
358100000125      *----------------------------------------------------------------
358200960527     C     AZZCMP        BEGSR
358300000125      *
358400000125      * Pulisco ds impostazione dati
358500070108     C                   CLEAR                   ED6T00DS
358600070108     C                   CLEAR                   ED6T01DS
358700070108     C                   CLEAR                   ED6T05DS
358800070108     C                   CLEAR                   ED6T10DS
358900070108     C                   CLEAR                   ED6T15DS
359000070108     C                   CLEAR                   ED6T20DS
359100070108     C                   CLEAR                   ED6T21DS
359200070108     C                   CLEAR                   ED6T22DS
359300070108     C                   CLEAR                   ED6T25DS
359400070108     C                   CLEAR                   ED6T26DS
359500070108     C                   CLEAR                   ED6T27DS
359600070108     C                   CLEAR                   ED6T28DS
359700070108     C                   CLEAR                   ED6T30DS
359800070108     C                   CLEAR                   ED6T31DS
359900070108      *---
360000070108     C                   CLEAR                   ED6D00DS
360100070108     C                   CLEAR                   ED6D01DS
360200070108     C                   CLEAR                   ED6D05DS
360300070108     C                   CLEAR                   ED6D1ADS
360400070108     C                   CLEAR                   ED6D10DS
360500070108     C                   CLEAR                   ED6D12DS
360600070108     C                   CLEAR                   ED6D13DS
360700070108     C                   CLEAR                   ED6D15DS
360800070108     C                   CLEAR                   ED6D16DS
360900070108     C                   CLEAR                   ED6D17DS
361000070108     C                   CLEAR                   ED6D18DS
361100070108     C                   CLEAR                   ED6D19DS
361200070108     C                   CLEAR                   ED6D20DS
361300070108     C                   CLEAR                   ED6D22DS
361400070108     C                   CLEAR                   ED6D25DS
361500070108     C                   CLEAR                   ED6D26DS
361600070108     C                   CLEAR                   ED6D27DS
361700070108     C                   CLEAR                   ED6D28DS
361800070108     C                   CLEAR                   ED6D30DS
361900070108     C                   CLEAR                   ED6D31DS
362000070108     C                   CLEAR                   ED6D32DS
362100070108     C                   CLEAR                   ED6D35DS
362200070108     C                   CLEAR                   ED6D36DS
362300070108     C                   CLEAR                   ED6D37DS
362400070108     C                   CLEAR                   ED6D40DS
362500070108     C                   CLEAR                   ED6D41DS
362600070108     C                   CLEAR                   ED6D42DS
362700070108     C                   CLEAR                   ED6D43DS
362800960527     C                   CLEAR                   WTT10
362900960527     C                   CLEAR                   WTT20
363000960527     C                   CLEAR                   WTT20D
363100960527     C                   CLEAR                   WTT25
363200960527     C                   CLEAR                   WTD00
363300960527     C                   CLEAR                   WTD10
363400960527     C                   CLEAR                   WTD15
363500960527     C* Pulisco schiere
363600960527     C                   CLEAR                   T05
363700960527     C                   CLEAR                   T10
363800960527     C                   CLEAR                   T20
363900960527     C                   CLEAR                   T20D
364000960527     C                   CLEAR                   T25
364100960527     C                   CLEAR                   D00
364200960527     C                   CLEAR                   D05
364300960527     C                   CLEAR                   D05A
364400070216     C                   CLEAR                   D30_1
364500070216     C                   CLEAR                   D30_2
364600070216     C                   CLEAR                   D32_1
364700070202     C                   CLEAR                   X30
364800070216     C                   CLEAR                   X30_1
364900070216     C                   CLEAR                   X30_2
365000031211     C*
365100960527     C                   ENDSR
365200000125      *----------------------------------------------------------------
365300000125      *? AZZBOL - Azzera dati bolla
365400000125      *----------------------------------------------------------------
365500960524     C     AZZBOL        BEGSR
365600000125      *
365700000125      * Pulisco campi del file
365800020909     C                   CLEAR                   EDiVAB00
365900020909     C                   CLEAR                   EDiVAD00
366000070214     C********           MOVEL     'S'           WRTVAB
366100961010     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
366200970612     C                   MOVEL     'N'           WGID99            1
366300000125     C                   clear                   XY                5 0
366400000125     C                   clear                   §§                5 0
366500000125     C                   clear                   WS1496            5 0
366600000125     C                   clear                   WNSP             35
366700000125     C                   clear                   WNOT1            35
366800000125     C                   clear                   WNOT2            35
366900070215     C                   Z-ADD     *ZEROS        PVLB
367000960726     C                   MOVEA     *HIVAL        SGN
367100960809     C                   CLEAR                   SGT
367200960809     C                   Z-ADD     0             XW
367300961122     C*  Azzero codice cliente - tariffa
367400000125     C                   clear                   WCLKSC
367500000125     C                   clear                   WCLTAR
367600040224     C                   clear                   WCLLNP
367700040303     C                   clear                   WCLNRS
367800040224     C                   clear                   WCLCTM
367900040224     C                   clear                   WCLBS1
368000050223     C                   clear                   WCLfnsp                        *blk/S
368100070125     C*  Campi di work
368200070125     c                   clear                   wVabTIC
368300070125     c                   clear                   wVabPKB
368400070205     c                   clear                   wVabNCL
368500970520     C* Azzero schiera per memorizzazione FTX + qualificatore
368600970520     C                   Z-ADD     0             FF                3 0
368700970520     C                   MOVEA     *BLANKS       FTX
368800970520     C                   MOVEA     *BLANKS       QUA
368900070131      *
369000070131     C* pulisce campi di work
369100110615     C                   clear                   Instradamento     1
369200070131     c                   clear                   watrde           35
369300070131     c                   clear                   wattel           25
369400070131     C                   clear                   WAGE             35
369500070131     C                   clear                   WFF              35
369600070131     C                   clear                   WCU              35
369700070201     C                   clear                   WAGE2            35
369800070201     C                   clear                   WCU2             35
369900070131     C                   clear                   WRMN              3
370000960528     C*
370100070131     C* Flag x Destinatario/Mittente in dettaglio
370200070131     c                   clear                   NAD_CZ_detta      1
370300070206     c                   clear                   NAD_CN_detta      1
370400070201     c                   clear                   Primo_RFFcollo    1
370500070201     c                   clear                   Primo_RCUcollo    1
370600070201     c                   z-add     0             conta_rif         1 0
370700070206     c                   clear                   suVABrmo
370800070215      *
370900070131      *
371000960527     C                   ENDSR
371100960628     C*----------------------------------------------------------------
371200960628     C*? AZZMSG - Azzera dati messaggio
371300960628     C*----------------------------------------------------------------
371400960628     C     AZZMSG        BEGSR
371500960628     C*
371600960628     C* Pulisco dati di work x foglio viaggio
371700960712     C                   MOVEL     'N'           WSTPTO            1
371800960628     C                   MOVEL     *BLANKS       WNOT             70
371900960628     C                   MOVEL     *BLANKS       WNUMFV           35
372000960628     C                   MOVEL     *BLANKS       WNRCMR           35
372100960628     C                   MOVEL     *BLANKS       WLOCPA           25
372200960628     C                   MOVEL     *BLANKS       WLOCAR           25
372300960628     C                   Z-ADD     0             WDATPA            8 0
372400960628     C                   Z-ADD     0             WHHPA             4 0
372500960628     C                   Z-ADD     0             WDATFV            8 0
372600960628     C                   Z-ADD     0             WHHNFV            4 0
372700960628     C                   Z-ADD     0             WDTCMR            8 0
372800960628     C                   Z-ADD     0             WHHCMR            4 0
372900070123     C                   Z-ADD     0             WvabDCR           8 0
373000070123     C                   Z-ADD     0             WvabHCR           4 0
373100070205     C                   movel     *blank        WvabTSP           1
373200070215     C                   Z-ADD     0             WTOTSP
373300070215     C                   Z-ADD     0             WTOTCO
373400070215     C                   Z-ADD     0             WTOTPL
373500070215     C                   Z-ADD     0             WTOTPN
373600070215      *
373700960703     C                   Z-ADD     0             WTOSPC           16 0
373800960703     C                   Z-ADD     0             WTOCOC           16 0
373900960703     C                   Z-ADD     0             WTOPLC           16 0          ???
374000960703     C                   Z-ADD     0             WTOPNC           16 0          ???
374100070131     C* Flag x Destinatario/Mittente in testata
374200070131     c                   clear                   NAD_CZ_testa      1
374300070131     c                   clear                   NAD_CN_testa      1
374400070131     c                   clear                   WvabRSD
374500070131     c                   clear                   WvabRD2
374600070131     c                   clear                   WvabIND
374700070131     c                   clear                   WvabLOD
374800070131     c                   clear                   WvabNZD
374900070131     c                   clear                   WvabRMO
375000070131     c                   clear                   WvabCMO
375100070131     c                   clear                   WvabNMO
375200070205     c                   clear                   Wvattel
375300070205     c                   clear                   Wvatrde
375400070215      * pulisce gli RFF+FF di testata
375500070215     C                   clear                   $CLKSC            7 0
375600070215     C                   clear                   $CLTAR            3
375700070215     C                   clear                   $CLLNP            3 0
375800070215     C                   clear                   $CLNRS            2 0
375900070215     C                   clear                   $CLCTM            2
376000070215     C                   clear                   $CLBS1            1
376100070215     C                   clear                   $CLfnsp           1            *blk/S
376200960927     C*  Inizializzo variabile new documento
376300961204     C                   MOVEL     'S'           WSTPTE            1
376400961204     C                   Z-ADD     0             KCL
376500961204     C                   Z-ADD     0             WW                3 0
376600070219     ** Inizializza campo di work
376700070219     c                   move      'N'           WGID99
376800070220     c                   clear                   UNB_Generico
376900070220     c                   clear                   UNB_Parz
377000960628     C*
377100960628     C                   ENDSR
377200960527     C*----------------------------------------------------------------
377300020909     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
377400960527     C*----------------------------------------------------------------
377500960527     C     UPDVAB        BEGSR
377600070215      *
377700070215     C*  Se non è stato impostato il codice bolla lo imposto io
377800070215     C     VABCBO        IFEQ      *BLANKS
377900070215     C     VABCAS        IFGT      0
378000070215     C                   MOVEL     '4'           VABCBO                         + C/Ass
378100070215     C                   ELSE
378200070215     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
378300070215     C                   END
378400070215     C                   END
378500990714      *
378600070205      *   Destinatario (in testata o in dettaglio)
378700070205     c                   if        NAD_CN_testa  =  'S' and
378800070205     c                             NAD_CN_detta  <> 'S'
378900070205     c                   eval        vabRSD  =   WvabRSD
379000070205     c                   eval        vabRD2  =   WvabRD2
379100070205     c                   eval        vabIND  =   WvabIND
379200070205     c                   eval        vabLOD  =   WvabLOD
379300070205     c                   eval        vabNZD  =   WvabNZD
379400070205     c                   end
379500070205      *   Mittente (in testata o in dettaglio)
379600070205     c                   if        NAD_CZ_testa  =  'S' and
379700070205     c                             NAD_CZ_detta  <> 'S'
379800070205     c                   eval        vabRMO  =   WvabRMO
379900070205     c                   eval        vabCMO  =   WvabCMO
380000070205     c                   eval        vabNMO  =   WvabNMO
380100070205     c                   end
380200070205      *
380300961104     C     VABNOT        IFEQ      *BLANKS
380400961104     C                   MOVEL     WNOT1         VABNOT
380500961104     C                   MOVEL     WNOT2         VABNT2
380600961104     C                   ELSE
380700961107     C                   MOVEL     WNOT1         VABNT2
380800961104     C                   END
380900960530     C*  Attribuisco anno spedizione
381000960530     C     WDATPA        IFGT      0                                            anno sped.
381100960530     C                   MOVEL     WDATPA        VABAAS
381200960530     C                   ELSE
381300960530     C     WDATFV        IFGT      0
381400960530     C                   MOVEL     WDATFV        VABAAS                         anno sped.
381500960530     C                   END
381600960530     C                   END
381700980219      *
381800980219      *  Controllo dettaglio segnacolli
381900001031     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
382000980219     C                   EXSR      CTRSGE
382100980219     C                   ELSE
382200980219     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
382300960731     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
382400960731     C                   EXSR      CTRSGN
382500960731     C                   END
382600980219     C                   END
382700980219      *
382800000125      *  Imposto linea partenza
382900000125     C                   eval      VABLNP = WVABLNP
383000040224      *
383100040224      *  Controllo se devo variare il codice trattamento merce
383200040227     C                   eval      VABCTM = WVABctm
383300050223      *
383400050223      * Controllo se deve mantenere il numero bolla passato dal messaggio
383500050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
383600050224     c                   clear                   mantiene_nsp      1            *blk/'S'
383700050224     c                   eval      mantiene_nsp = WVABfnsp                      *blk/'S'
383800040224      *
383900050224      * (Se esistono delle Particolarità sul cliente prioritariamente prende
384000050224      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
384100050224      *
384200000125      *  Imposto codice cliente
384300000125     c                   Select
384400040224      *
384500000125      *  Specificato codice da qualificatore FF (TD15)
384600000125     C                   When        WCLKSC <> *zeros
384700000125     C                   Z-ADD     WCLKSC        VABCCM
384800000125     C                   MOVEL     WCLTAR        VABCTR
384900040303      * forza LNP/CTM/NRS
385000040224     c                   if        WclLNP <> 0
385100040224     C                   eval      VABLNP = WclLNP
385200040224     c                   end
385300040303     c                   if        WclNRS <> 0
385400040303     C                   eval      VABNRS = WclNRS
385500040227     c                   end
385600040224     c                   if        WclCTM <> *blank
385700040224     C                   eval      VABCTM = WclCTM
385800040224     c                   end
385900050223      *
386000050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
386100050224     c                   if        WCLfnsp <> *blank                            *blk/'S'
386200050224     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
386300050223     c                   end
386400050223      *
386500000125      *  Specificato codice da qualificatore FF (TT15)
386600000125     C                   When        $CLKSC <> *zeros
386700000125     C                   Z-ADD     $CLKSC        VABCCM
386800000125     C                   MOVEL     $CLTAR        VABCTR
386900040303      * forza LNP/CTM/NRS
387000040224     c                   if        $clLNP <> 0
387100040224     C                   eval      VABLNP = $clLNP
387200040224     c                   end
387300040303     c                   if        $clNRS <> 0
387400040303     C                   eval      VABNRS = $clNRS
387500040227     c                   end
387600040224     c                   if        $clCTM <> *blank
387700040224     C                   eval      VABCTM = $clCTM
387800040224     c                   end
387900050223      *
388000050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
388100050224     c                   if        $CLfnsp <> *blank                            *blk/'S'
388200050224     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
388300050223     c                   end
388400040224      *
388500000125      *  Imposto dati da tabella - PT-
388600000125     C                   Other
388700040224      *
388800000125     C                   MOVEL     §PTKSC        VABCCM
388900960527     C                   MOVEL     §PTCTR        VABCTR
389000000125      *
389100000125     C                   Endsl
389200040225      *
389300040225      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
389400040225      *    o dalla relativa CL
389500040225     C                   MOVEL     VABlnp        VABfgs
389600000126      *
389700050223      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
389800050223      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
389900050224      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
390000050224      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
390100050224      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
390200050224      *                A T T E N Z I O N E
390300050224      * << Se però il numero è più grande di 7 cifre non deve eseguirlo e forza blank >>
390400050309     c******             if        VABRMN > 9999999
390500050309     c******             eval      mantiene_nsp = *blank                        *blk/'S'
390600050309     c******             end
390700050224      *
390800050224     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
390900050224     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
391000050224     c                   else
391100050112      *
391200050112     C*  Se non è previsto che il cliente attribuisca nr.sped.
391300050112     C*  lo attribuisco io
391400050112      **              **------------------**
391500050112     c                   exsr      repnum
391600050112      **              **------------------**
391700050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
391800050112      ***********
391900050224     c                   end
392000050224      *
392100960527     C*  Altrimenti attribuisco data del giorno
392200960527     C     VABMGS        IFEQ      0
392300960527     C                   MOVEL     WOGGI         VABAAS                         data sped.
392400960527     C                   MOVE      WOGGI         VABMGS                         = oggi
392500960527     C                   END
392600020909     C*  Eseguo posizionamento su EDiVAB
392700960528     C                   Z-ADD     VABAAS        KAAS
392800040224     C                   Z-ADD     VABlnp        KLNP
392900960528     C                   Z-ADD     VABNRS        KNRS
393000960528     C                   Z-ADD     VABNSP        KNSP
393100960528     C                   MOVEL     SAVBOL        SALVA
393200020909     C     KVAB1         CHAIN     EDiVAB1L                           30
393300960528     C                   MOVEL     SALVA         SAVBOL
393400960924     C*  Imposto dati CMR
393500960924     C                   Z-ADD     WPROG         VABCNT
393600960926     C                   Z-ADD     WDTPRE        VABDTS
393700960926     C                   Z-ADD     WHMPRE        VABHMS
393800960926     C     §PTCMR        IFEQ      'S'
393900960924     C                   Z-ADD     WDTCMR        VABDCM
394000960924     C                   MOVEL     WNRCMR        VABCMR
394100960926     C                   ELSE
394200960926     C                   Z-ADD     WDATFV        VABDCM
394300960926     C                   MOVEL     WNUMFV        VABCMR
394400960926     C                   END
394500960531     C*  Se non è indicato il nome del mittente = Partner mittente
394600960531     C     VABRMO        IFEQ      *BLANKS
394700960531     C                   MOVEL     ACORAG        VABRMO
394800960531     C                   MOVEL     INDCAP        VABCMO
394900960531     C     INDCAE        IFNE      *BLANKS
395000960531     C                   MOVEL     INDCAE        VABCMO
395100960531     C                   END
395200960820     C     INDSTA        IFNE      *BLANKS
395300960820     C                   MOVEL     INDSTA        VABNMO
395400960820     C                   END
395500960531     C                   END
395600070206      *
395700070206      * attenzione alla particolarità attivata sul vabrmo tabellata su PT
395800070206     c                   if        SUvabRMO <> *blank
395900070206     c                   eval        vabRMO  =   SUvabRMO
396000070206     c                   end
396100070206      *
396200960527     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
396300960527     C     §1BFG1        IFEQ      'N'
396400960731     C     §1BFG7        ANDEQ     'N'
396500960527     C     §1BFG2        ANDEQ     'N'
396600960731     C***                  Z-ADD0         VABNRS
396700960527     C                   END
396800960527     C*  Imposto segancollo da - a
396900960731     C     §1BFG1        IFEQ      'S'
397000960731     C     §1BFG7        ANDEQ     'N'
397100001031     C     §1BF12        ANDNE     'E'
397200990714      *
397300990810     C     XY            IFEQ      *ZEROS
397400990810     C                   CLEAR                   VABNCD
397500990810     C                   CLEAR                   VABNCA
397600990810     C                   ELSE
397700990714     C                   Z-ADD     SGN(1)        VABNCD
397800990714     C                   Z-ADD     SGN(XY)       VABNCA
397900990810     C                   ENDIF
398000990810     C*
398100990714     C                   END
398200960531     C*
398300960904     C*  Se cap mittente originale a blank abblenco nazione
398400960904     C     VABCMO        IFEQ      *BLANKS
398500960904     C                   MOVEL     *BLANKS       VABNMO
398600960904     C                   END
398700071128      *
398800071128      * ? Forza il numero passato nel CNI nel riferimento Numerico
398900071128      *
399000071128     c                   if        §PVCNIRMN = 'S'
399100071128     c                   z-add     wCNIsalvatoRMNvabRMN
399200071128     c                   end
399300071128      *
399400110615      *
399500110615      * Non ha calcolato l'instradamento poichè non aveva segmenti MEA
399600110615     C                   if         Instradamento = *blank
399700110615     c                               and vabCAD = *blank and vabpkb >0
399800110615     c                   exsr      Instrada_sped
399900110615     c                   end
400000110615      *
400100050301     C*
400200050301      *? ------------------------------------------------------------------------
400300050301      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
400400050301      *? ------------------------------------------------------------------------
400500050301     C*
400600050301     c                   exsr      Chiodi_VAB
400700050301     C*
400800050224      *? ------------------------------------------------------------------------
400900010621     C*
401000020909     C   30              WRITE     EDiVAB00
401100020909     C  N30              UPDATE    EDiVAB00
401200060317     c                   clear                   wri_vabtic        1
401300010621     C*
401400031211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
401500031211     C                   EXSR      WRTVAT_Rif
401600031211     C*
401700980219      * Stampo ns.numero spedizione
401800040607     c                   If        In_Stampa ='S'
401900960712     C   OA              EXCEPT    TEST1
402000960712     C                   EXCEPT    LETVET
402100040607     c                   end
402200980219      *
402300020909      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
402400001031     C     §1BF12        IFEQ      'E'
402500980219     C                   EXSR      WRTVAT
402600980219     C                   ELSE
402700020909      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
402800960731     C     §1BFG7        IFEQ      'S'
402900960731     C     §1BFG1        OREQ      'S'
403000960527     C                   EXSR      WRTVAD
403100960527     C                   END
403200980219     C                   END
403300960527     C* Reimposto codice trattamento merce cliente
403400960527     C                   MOVEL     WSVTRM        DS1B
403500031219     C**
403600960626     C* Do errore se previsto CMR ma non c'è
403700960827     C     §PTCMR        IFNE      ' '
403800960731     C                   EXSR      MEMCMR
403900960731     C                   END
404000960731     C* Do errore se previsto AFB ma non c'è
404100960827     C     §PTNFV        IFNE      ' '
404200960731     C                   EXSR      MEMAFB
404300960731     C                   END
404400961010     C*  Se il cliente vuole il ritorno delle date di consegna
404500961010     C*  è c'è una squdratura fra i segnacolli e il numero
404600961010     C*  dei colli che ci ha passato scrivo EDSUM
404700961010     C     §PUDTA        IFEQ      'S'
404800961010     C     WSQUAD        ANDEQ     'S'
404900961010     C                   EXSR      WRTSUM
405000961010     C                   END
405100960626     C*
405200960527     C                   ENDSR
405300960731     C*----------------------------------------------------------------
405400050301     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
405500050301     C*----------------------------------------------------------------
405600050301     C     Chiodi_VAB    BEGSR
405700050307      *
405800080410     C*  Per non bloccare la bollettazione
405900080410     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
406000080410     C*  o se il peso non è presente... imposto fisso il minimo 0,1
406100080410     c                   if        vabpkb = 0
406200080410     c                   z-add     0,1           vabpkb
406300080410     c                   end
406400050301     C*
406500050301     C                   ENDSR
406600050301     C*----------------------------------------------------------------
406700960731     C*? MEMCMR - MEMORIZZO NUMERO CMR
406800960731     C*----------------------------------------------------------------
406900960731     C     MEMCMR        BEGSR
407000960731     C*
407100960731     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
407200960827     C     §PTCM1        IFEQ      'S'
407300960819     C                   CLEAR                   EDRDE000
407400070109     C                   MOVEL     'T51154'      KNMC
407500960731     C                   Z-ADD     1             KNRP
407600961029     C                   MOVEL     WKSC          KLNP1
407700960731     C     KRDE          CHAIN     EDRDE01L                           31
407800960731     C     *IN31         IFEQ      '1'
407900960731     C                   Z-ADD     VABAAS        RDEAAS
408000040302     C                   Z-ADD     VABLNP        RDELNP
408100960731     C                   Z-ADD     VABNRS        RDENRS
408200960731     C                   Z-ADD     VABNSP        RDENSP
408300070109     C                   MOVEL     'T51154'      RDENMC
408400960731     C                   Z-ADD     1             RDENRP
408500960731     C                   MOVEL     WNRCMR        RDEVAL
408600960731     C                   WRITE     EDRDE000
408700960731     C                   END
408800960731     C*  Memorizzo qualificatore CMR
408900960819     C                   CLEAR                   EDRDE000
409000070109     C                   MOVEL     'T51153'      KNMC
409100960731     C                   Z-ADD     1             KNRP
409200961029     C                   MOVEL     WKSC          KLNP1
409300960731     C     KRDE          CHAIN     EDRDE01L                           31
409400960731     C     *IN31         IFEQ      '1'
409500960731     C                   Z-ADD     VABAAS        RDEAAS
409600040302     C                   Z-ADD     VABLNP        RDELNP
409700960731     C                   Z-ADD     VABNRS        RDENRS
409800960731     C                   Z-ADD     VABNSP        RDENSP
409900960731     C                   Z-ADD     1             RDENRP
410000070109     C                   MOVEL     'T51153'      RDENMC
410100960731     C                   MOVEL     'CMR'         RDEVAL
410200960731     C                   WRITE     EDRDE000
410300960731     C                   END
410400960731     C*  Memorizzo la data
410500960819     C                   CLEAR                   EDRDE000
410600070109     C                   MOVEL     'T52380'      KNMC
410700960731     C                   Z-ADD     1             KNRP
410800961029     C                   MOVEL     WKSC          KLNP1
410900960731     C     KRDE          CHAIN     EDRDE01L                           31
411000960731     C     *IN31         IFEQ      '1'
411100960731     C                   Z-ADD     VABAAS        RDEAAS
411200040302     C                   Z-ADD     VABLNP        RDELNP
411300960731     C                   Z-ADD     VABNRS        RDENRS
411400960731     C                   Z-ADD     VABNSP        RDENSP
411500960731     C                   Z-ADD     1             RDENRP
411600070109     C                   MOVEL     'T52380'      RDENMC
411700960731     C                   MOVEL     WDTCMR        RDEVAL
411800960731     C                   WRITE     EDRDE000
411900960731     C                   END
412000960731     C*
412100960827     C                   END                                                    §PTCM1 = 'S'
412200960731     C*
412300960731     C                   ENDSR
412400960731     C*----------------------------------------------------------------
412500960731     C*? MEMAFB - MEMORIZZO NUMERO AFB
412600960731     C*----------------------------------------------------------------
412700960731     C     MEMAFB        BEGSR
412800960731     C*
412900960731     C     WNUMFV        IFEQ      *BLANKS
413000960731     C                   Z-ADD     23            WINDER
413100960731     C                   MOVEL     *BLANKS       WVALOR
413200960731     C                   SETON                                        05
413300960731     C                   EXSR      STPERR
413400960731     C                   END
413500960731     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
413600960827     C     §PTNF1        IFEQ      'S'
413700960819     C                   CLEAR                   EDRDE000
413800961029     C                   MOVEL     WKSC          KLNP1
413900070109     C                   MOVEL     'T51154'      KNMC
414000960731     C                   Z-ADD     1             KNRP
414100960731     C     KRDE          CHAIN     EDRDE01L                           31
414200960731     C     *IN31         IFEQ      '1'
414300960731     C                   Z-ADD     VABAAS        RDEAAS
414400040302     C                   Z-ADD     VABLNP        RDELNP
414500960731     C                   Z-ADD     VABNRS        RDENRS
414600960731     C                   Z-ADD     VABNSP        RDENSP
414700070109     C                   MOVEL     'T51154'      RDENMC
414800960827     C     §PTCM1        IFNE      'S'
414900960731     C     WNRCMR        OREQ      *BLANKS
415000960731     C                   Z-ADD     1             RDENRP
415100960731     C                   ELSE
415200960731     C                   Z-ADD     2             RDENRP
415300960731     C                   END
415400960731     C                   MOVEL     WNUMFV        RDEVAL
415500960731     C                   WRITE     EDRDE000
415600960731     C                   END
415700960731     C*  Memorizzo qualificatore CMR
415800960819     C                   CLEAR                   EDRDE000
415900961029     C                   MOVEL     WKSC          KLNP1
416000070109     C                   MOVEL     'T51153'      KNMC
416100960731     C                   Z-ADD     1             KNRP
416200960731     C     KRDE          CHAIN     EDRDE01L                           31
416300960731     C     *IN31         IFEQ      '1'
416400960731     C                   Z-ADD     VABAAS        RDEAAS
416500040302     C                   Z-ADD     VABLNP        RDELNP
416600960731     C                   Z-ADD     VABNRS        RDENRS
416700960731     C                   Z-ADD     VABNSP        RDENSP
416800960827     C     §PTCM1        IFNE      'S'
416900960731     C     WNRCMR        OREQ      *BLANKS
417000960731     C                   Z-ADD     1             RDENRP
417100960731     C                   ELSE
417200960731     C                   Z-ADD     2             RDENRP
417300960731     C                   END
417400070109     C                   MOVEL     'T51153'      RDENMC
417500960731     C                   MOVEL     'AFB'         RDEVAL
417600960731     C                   WRITE     EDRDE000
417700960731     C                   END
417800960731     C*  Memorizzo la data
417900960819     C                   CLEAR                   EDRDE000
418000961029     C                   MOVEL     WKSC          KLNP1
418100070109     C                   MOVEL     'T52380'      KNMC
418200960731     C                   Z-ADD     1             KNRP
418300960731     C     KRDE          CHAIN     EDRDE01L                           31
418400960731     C     *IN31         IFEQ      '1'
418500960731     C                   Z-ADD     VABAAS        RDEAAS
418600040302     C                   Z-ADD     VABLNP        RDELNP
418700960731     C                   Z-ADD     VABNRS        RDENRS
418800960731     C                   Z-ADD     VABNSP        RDENSP
418900960827     C     §PTCM1        IFNE      'S'
419000960731     C     WNRCMR        OREQ      *BLANKS
419100960731     C                   Z-ADD     1             RDENRP
419200960731     C                   ELSE
419300960731     C                   Z-ADD     2             RDENRP
419400960731     C                   END
419500070109     C                   MOVEL     'T52380'      RDENMC
419600960731     C                   MOVEL     WDATFV        RDEVAL
419700960731     C                   WRITE     EDRDE000
419800960731     C                   END
419900960731     C*
420000960827     C                   END                                                    §PTCM1 = 'S'
420100960731     C*
420200960731     C                   ENDSR
420300960527     C*----------------------------------------------------------------
420400960527     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
420500960527     C*----------------------------------------------------------------
420600960528     C     CTRSGN        BEGSR
420700960527     C*
420800960528     C                   MOVEL     'N'           WNSGER            1
420900960527     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
421000960527     C     VABNCL        IFNE      XY
421100961010     C                   MOVEL     'S'           WNSGER
421200961010     C                   MOVEL     'S'           WSQUAD
421300960527     C                   Z-ADD     17            WINDER
421400960927     C                   MOVEL     *BLANKS       WCOMO8            8
421500960927     C                   MOVEL     XY            WCOMO8            8
421600960926     C                   MOVE      '<>'          WCOMO8
421700960926     C                   MOVEL     WCOMO8        WCOM14           14
421800960926     C                   MOVE      VABNCL        WCOM14           14
421900960926     C                   Z-ADD     17            WINDER
422000960926     C                   MOVEL     *BLANKS       WVALOR
422100960926     C                   MOVEL     WCOM14        WVALOR
422200960731     C                   SETON                                        05
422300960528     C                   EXSR      STPERR
422400960527     C                   GOTO      FINSGN
422500960527     C                   END
422600960527     C*  Riordino i segnacolli
422700960731     C     §1BFG7        IFEQ      'N'
422800960527     C                   SORTA     SGN
422900960527     C*  Controllo se sono sequenziali
423000960528     C                   MOVEL     'N'           WNOSEQ            1
423100960528     C     SGN(1)        ADD       1             WEXSGN            7 0
423200960527     C     2             DO        XY            X
423300960527     C     SGN(X)        IFNE      WEXSGN
423400960527     C                   MOVEL     'S'           WNOSEQ
423500960527     C                   END
423600960527     C     SGN(X)        ADD       1             WEXSGN
423700960527     C                   END
423800960731     C                   END
423900960527     C*
424000960527     C     FINSGN        ENDSR
424100980219     C*----------------------------------------------------------------
424200980219     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
424300980219     C*----------------------------------------------------------------
424400980219     C     CTRSGE        BEGSR
424500980219     C*
424600980219     C                   MOVEL     'N'           WNSGER            1
424700980219      *
424800980219      *  Controllo se il nr.segnacolli corrisponde coi bollettati
424900980219      *
425000980219     C     VABNCL        IFNE      §§
425100980219     C                   MOVEL     'S'           WNSGER
425200980219     C                   MOVEL     'S'           WSQUAD
425300980219     C                   Z-ADD     17            WINDER
425400980219     C                   MOVEL     *BLANKS       WCOMO8            8
425500980219     C                   MOVEL     §§            WCOMO8            8
425600980219     C                   MOVE      '<>'          WCOMO8
425700980219     C                   MOVEL     WCOMO8        WCOM14           14
425800980219     C                   MOVE      VABNCL        WCOM14           14
425900980219     C                   Z-ADD     17            WINDER
426000980219     C                   MOVEL     *BLANKS       WVALOR
426100980219     C                   MOVEL     WCOM14        WVALOR
426200980219     C                   SETON                                        05
426300980219     C                   EXSR      STPERR
426400980219     C                   END
426500980219      *
426600980219     C                   ENDSR
426700960527     C*----------------------------------------------------------------
426800020909     C*? ESEGUO SCRITTURA EDiVAD
426900960527     C*----------------------------------------------------------------
427000960527     C     WRTVAD        BEGSR
427100960527     C*
427200960527     C* Se non seq. scrivo xy record
427300960731     C     §1BFG1        IFEQ      'S'
427400990810     C     XY            ANDNE     *ZEROS
427500960731     C*
427600960731     C     §1BFG7        IFEQ      'S'
427700960527     C                   DO        XY            X
427800960528     C                   Z-ADD     VABCCM        KCCM
427900960528     C                   Z-ADD     SGN(X)        KNCD
428000960924     C                   Z-ADD     VABCNT        VADCNT
428100960924     C                   MOVEL     VABCMR        VADCMR
428200960527     C                   Z-ADD     VABAAS        VADAAS
428300040302     C                   Z-ADD     VABLNP        VADLNP
428400960527     C                   Z-ADD     VABNRS        VADNRS
428500960527     C                   Z-ADD     VABNSP        VADNSP
428600960731     C                   Z-ADD     1             VADNCL
428700960911     C                   MOVEL     SGN(X)        VADCDP
428800960527     C                   Z-ADD     SGN(X)        VADNCD
428900960527     C                   Z-ADD     *ZEROS        VADNCA
429000960528     C                   Z-ADD     VABCCM        VADCCM
429100020909     C                   movel     VABfgs        VADfgs
429200020909     C                   WRITE     EDiVAD00
429300960527     C                   END
429400960527     C* Se sequenziali scrivo un solo record
429500960527     C                   ELSE
429600960731     C                   Z-ADD     VABAAS        VADAAS
429700040302     C                   Z-ADD     VABLNP        VADLNP
429800960731     C                   Z-ADD     VABNRS        VADNRS
429900960731     C                   Z-ADD     VABNSP        VADNSP
430000960731     C                   Z-ADD     VABNCL        VADNCL
430100960731     C                   Z-ADD     SGN(1)        VADNCD
430200960731     C                   Z-ADD     SGN(XY)       VADNCA
430300960731     C                   Z-ADD     VABCCM        VADCCM
430400020909     C                   movel     VABfgs        VADfgs
430500020909     C                   WRITE     EDiVAD00
430600960527     C                   END
430700960731     C*
430800960731     C                   ELSE
430900960731     C                   DO        XY            X
431000960731     C                   Z-ADD     VABCCM        KCCM
431100960731     C                   Z-ADD     SGN(X)        KNCD
431200960731     C                   Z-ADD     VABAAS        VADAAS
431300040302     C                   Z-ADD     VABLNP        VADLNP
431400960731     C                   Z-ADD     VABNRS        VADNRS
431500960731     C                   Z-ADD     VABNSP        VADNSP
431600960731     C                   MOVEL     SGN(X)        VADCDP
431700960731     C                   Z-ADD     1             VADNCL
431800960731     C                   Z-ADD     *ZEROS        VADNCD
431900960731     C                   Z-ADD     *ZEROS        VADNCA
432000960731     C                   Z-ADD     VABCCM        VADCCM
432100020909     C                   movel     VABfgs        VADfgs
432200020909     C                   WRITE     EDiVAD00
432300960731     C                   END
432400960527     C*
432500960731     C                   END
432600960731     C*
432700960527     C                   ENDSR
432800031211     C*----------------------------------------------------------------
432900031211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
433000031211     C*----------------------------------------------------------------
433100031211     C     WRTVAT_Rif    BEGSR
433200031211      *
433300031211      *  riferimento di consegna destinatario
433400070205     c                   if        WatRDE <> *blank or WvatRDE <> *blank
433500070205     C                   MOVEL     WvatRDE       VATNOT
433600070205      * prende quello del dettaglio
433700070205     c                   if          WatRDE <> *blank
433800070205     C                   MOVEL     WatRDE        VATNOT
433900070205     c                   end
434000031211     C                   Z-ADD     VABCNT        VATCNT
434100031211     C                   MOVEL     VABCMR        VATCMR
434200031211     C                   Z-ADD     VABCCM        VATCCM
434300031211     C                   movel     VABfgs        VATfgs
434400040302     C                   Z-ADD     VABLNP        VATLNP
434500031211     C                   Z-ADD     VABAAS        VATAAS
434600031211     C                   Z-ADD     VABNRS        VATNRS
434700031211     C                   Z-ADD     VABNSP        VATNSP
434800031211     C                   MOVEL     'A'           VATTRC
434900031211     C                   WRITE     EDiVAT00
435000031211     c                   end
435100031211      *
435200031211      *  riferimento di consegna telefonico
435300070205     c                   if        WatTEL <> *blank or WvatTEL <> *blank
435400070205     C                   MOVEL     WvatTEL       VATNOT
435500070205      * prende quello del dettaglio
435600070205     c                   if          WatTEL <> *blank
435700070205     C                   MOVEL     WatTEL        VATNOT
435800070205     c                   end
435900031211     C                   Z-ADD     VABCNT        VATCNT
436000031211     C                   MOVEL     VABCMR        VATCMR
436100031211     C                   Z-ADD     VABCCM        VATCCM
436200031211     C                   movel     VABfgs        VATfgs
436300040302     C                   Z-ADD     VABLNP        VATLNP
436400031211     C                   Z-ADD     VABAAS        VATAAS
436500031211     C                   Z-ADD     VABNRS        VATNRS
436600031211     C                   Z-ADD     VABNSP        VATNSP
436700031211     C                   MOVEL     'B'           VATTRC
436800031211     C                   WRITE     EDiVAT00
436900031211     c                   end
437000031211      *
437100031219      * riferimento del Partner che una volta veniva riportato sul BL4
437200031219     c                   if        Wnsp   <> *blank
437300031219     C                   Z-ADD     VABCNT        VATCNT
437400031219     C                   MOVEL     VABCMR        VATCMR
437500031219     C                   Z-ADD     VABCCM        VATCCM
437600031219     C                   movel     VABfgs        VATfgs
437700040302     C                   Z-ADD     VABLNP        VATLNP
437800031219     C                   Z-ADD     VABAAS        VATAAS
437900031219     C                   Z-ADD     VABNRS        VATNRS
438000031219     C                   Z-ADD     VABNSP        VATNSP
438100031219     C                   MOVEL     'C'           VATTRC
438200031219     C                   MOVEL     Wnsp          VATNOT
438300031219     C                   WRITE     EDiVAT00
438400031219     c                   end
438500031219      *
438600031211     C                   ENDSR
438700980219     C*----------------------------------------------------------------
438800020909     C*? ESEGUO SCRITTURA EDiVAT
438900980219     C*----------------------------------------------------------------
439000980219     C     WRTVAT        BEGSR
439100980219      *
439200980219     C                   DO        §§            X
439300031121     C                   Z-ADD     VABCNT        VATCNT
439400031121     C                   MOVEL     VABCMR        VATCMR
439500980219     C                   Z-ADD     VABCCM        VATCCM
439600020909     C                   movel     VABfgs        VATfgs
439700040302     C                   Z-ADD     VABLNP        VATLNP
439800980219     C                   Z-ADD     VABAAS        VATAAS
439900980219     C                   Z-ADD     VABNRS        VATNRS
440000980219     C                   Z-ADD     VABNSP        VATNSP
440100980219     C                   MOVEL     'E'           VATTRC
440200050301      *
440300050301      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
440400050301      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
440500050301      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
440600050301     c                   if        WVATlun > 0
440700050301      *
440800050301      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
440900050301     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
441000050301      *
441100050301     c                   else
441200050301      *
441300050301      * Se non è impostato nulla prende il segnacollo passato così com'è
441400980219     C                   MOVEL     SGE(X)        VATNOT
441500050301     c                   end
441600050301      *
441700020909     C                   WRITE     EDiVAT00
441800980219     C                   END
441900980219      *
442000980219     C                   ENDSR
442100961010     C*----------------------------------------------------------------
442200961010     C*? ESEGUO SCRITTURA EDSUM
442300961010     C*----------------------------------------------------------------
442400961010     C     WRTSUM        BEGSR
442500961010     C*
442600961010     C* Se non seq. scrivo xy record
442700961010     C                   DO        XY            X
442800961010     C                   Z-ADD     VABAAS        SUMAAS
442900040302     C                   Z-ADD     VABLNP        SUMFLS
443000961104     C                   MOVEL     WKSC          SUMLNP
443100961010     C                   Z-ADD     VABLNA        SUMLNA
443200961010     C                   Z-ADD     VABZNC        SUMZNC
443300961010     C                   Z-ADD     VABNRS        SUMNRS
443400961010     C                   Z-ADD     VABNSP        SUMNSP
443500961010     C                   Z-ADD     SGN(X)        SUMNSC
443600961028     C                   Z-ADD     WKSC          SUMCCM
443700961010     C                   MOVEL     *BLANKS       SUMSTS
443800961010     C                   MOVEL     *BLANKS       SUMFLG
443900070125     C                   MOVEL     WidSPE        SUMCNI
444000961010     C                   MOVEL     VABCMR        SUMCMR
444100961010     C                   Z-ADD     0             SUMNFE
444200961010     C                   Z-ADD     VABCCM        VADCCM
444300961010     C                   WRITE     EDSUM000
444400961010     C                   END
444500961010     C*
444600961010     C                   ENDSR
444700960527     C*----------------------------------------------------------------
444800960527     C*? STPERR: STAMPA ERRORE
444900960527     C*----------------------------------------------------------------
445000960527     C     STPERR        BEGSR
445100040227     C*
445200040227     C*  Prepara messaggio di errore da mandare ad un utente specifico
445300040227     C*   a pro-memoria da controllare gli errori elencati in stampa
445400040227     c                   clear                   msgDSP
445500070104     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC79R traduzione E-
445600040227     C                             DI IFCSUM Controllare Stampa errori QSYSPRT -
445700040227     C                             con il comando: << WRKSPLF SELECT(*ALL *ALL -
445800070104     C                             *ALL TRTC79R) >> x il Codice:'
445900040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' +
446000040303     C                             %trim(%Editc(§ptksc:'X')) + ' ' +
446100040303     C                             %trim(ACORAG)
446200040227     C*
446300960527     C                   Z-ADD     WINDER        I                 2 0
446400960731     C                   MOVEL     MSG(I)        SERR             60
446500960927     C     *INOF         IFEQ      '1'
446600960927     C                   EXCEPT    TESTA
446700040227     C*
446800960927     C                   MOVEL     'S'           WSTPTE
446900960927     C                   END
447000960927     C*
447100960927     C*  Controllo intestazione errori da stampare
447200960927     C     WSTPTE        IFEQ      'S'
447300040227     C*
447400960927     C*  Controllo se devo stampare nr.documento
447500960927     C     §PTCMR        IFEQ      'S'
447600960927     C     WNRCMR        ANDNE     *BLANKS
447700960927     C                   MOVEL     'CMR'         TIPDOC            3
447800960927     C                   MOVEL     WNRCMR        WDOCUM           35
447900960927     C                   EXCEPT    ERRDOC
448000040227     C*
448100040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' + %trim(TIPDOC)
448200040227     C                                       + ' ' + %trim(Wdocum)
448300960927     C                   ELSE
448400040227     C*
448500960927     C     §PTNFV        IFEQ      'S'
448600960927     C     WNUMFV        ANDNE     *BLANKS
448700960927     C                   MOVEL     'AFB'         TIPDOC            3
448800960927     C                   MOVEL     WNUMFV        WDOCUM           35
448900960927     C                   EXCEPT    ERRDOC
449000040227     C*
449100040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' + %trim(TIPDOC)
449200040227     C                                       + ' ' + %trim(Wdocum)
449300040227     C*
449400960927     C                   ELSE
449500960927     C                   EXCEPT    ERRCLI
449600041006      *
449700041006     c                   goto      no_invio_Xora
449800040227     c                   exsr      SEND_MSG_EDP
449900041006     c     no_invio_Xora tag
450000041006      *
450100040310     c                   move      'N'           Snd_Msg_alert     1
450200040227     C*
450300960927     C                   END
450400960927     C                   END
450500960927     C                   MOVEL     'N'           WSTPTE            1
450600960927     C                   END
450700960927     C*
450800960527     C                   EXCEPT    DETER
450900040416     c                   move      'S'           errori_prtf
451000960527     C*
451100960527     C                   ENDSR
451200960329     C*----------------------------------------------------------------
451300960527     C*? CNVDAT - DECODIFICA LA DATA
451400960527     C* INPUT:  - WFORM  (FORMATO DATA : 102)
451500960527     C*         - WDATA  (DATA ALFANUMERICA)
451600960527     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
451700960527     C*         - WHMS   (ORA NUMERICA)
451800960329     C*----------------------------------------------------------------
451900960527     C     CNVDAT        BEGSR
452000960329     C*
452100960528     C                   MOVEL     ' '           WERRO             1
452200960527     C                   Z-ADD     *ZEROS        WCAMG             8 0
452300960527     C                   Z-ADD     *ZEROS        WHMS              6 0
452400960329     C*
452500960527     C     WFORM         IFEQ      '102'                                        CCYMMDD
452600960527     C                   MOVEL     WDATA         WCOMO8            8
452700960527     C                   TESTN                   WCOMO8               99
452800960527     C   99              MOVE      WCOMO8        WCAMG                          *DATA
452900960528     C  N99              MOVEL     'S'           WERRO             1
453000960329     C                   END
453100960329     C*
453200960329     C                   ENDSR
453300960725     C*----------------------------------------------------------------
453400960725     C*? TSTNUM - CONTROLLO NUMERICO
453500960725     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
453600960725     C*         - WLUNGH (Lunghezza campo output)
453700960725     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
453800960725     C*----------------------------------------------------------------
453900960725     C     TSTNUM        BEGSR
454000960725     C*
454100960725     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
454200960725     C                   MOVEL     'N'           WOKNUM            1
454300960725     C                   MOVEL     *BLANKS       WNUMOK           15
454400960725     C                   Z-ADD     15            IN                3 0
454500960725     C                   Z-ADD     WLUNGH        IX                3 0
454600960725     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
454700960725     C                   MOVEA     WNUM          NDA
454800960725     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
454900960725     C                   MOVEA     *ALL'0'       NDN
455000960725     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
455100960725     C                   Z-ADD     WLUNGH        I
455200960725     C     I             DOWGT     0                                            --- 1 -->
455300960725     C* ESTRAE IL CARATTERE DA ESAMINARE
455400960725     C     1             SUBST     WNUM:I        WTEM01            1
455500960725     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
455600960725     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
455700960725     C     WTEM01        SCAN      WCA                                    99
455800960725     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
455900960725     C     *IN99         IFEQ      '1'                                          --- 2 -->
456000960725     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
456100960725     C     IN            IFGT      *ZEROS                                       --- 3 -->
456200960725     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
456300960725     C* DA DESTRA VERSO SINISTRA
456400960725     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
456500960725     C                   MOVEL     WTEMP1        NDN(IN)
456600960725     C                   SUB       1             IN
456700960725     C                   END                                                    <-- 3 ---
456800960725     C                   END                                                    <-- 2 ---
456900960725     C                   SUB       1             I
457000960725     C                   END                                                    <-- 1 ---
457100960725     C*
457200960725     C                   MOVEA     NDN           WNDN             15
457300960725     C     WNDN          IFNE      *ZEROS
457400960725     C                   MOVEA     NDN           WNUMOK           15
457500960725     C                   MOVEL     'S'           WOKNUM            1
457600960725     C                   END
457700960725     C*
457800960725     C                   ENDSR
457900960527     C*----------------------------------------------------------------
458000960527     C*? CTRVAL - CONTROLLO VALUTA
458100960527     C*----------------------------------------------------------------
458200960527     C     CTRVAL        BEGSR
458300960527     C*
458400960527     C                   Z-ADD     1             Y
458500960527     C     WVAL          LOOKUP    CCV(Y)                                 32
458600960527     C     *IN32         IFEQ      '1'
458700960527     C                   MOVEL     DCV(Y)        DSCV
458800960527     C*  Se valuta italiana abblenco divisa
458900960527     C     §CVITA        IFEQ      'I'
459000010928     C                   MOVEL     'ITL'         WVAL
459100960527     C                   END
459200960527     C                   ELSE
459300960527     C                   Z-ADD     20            WINDER
459400960527     C                   MOVEL     *BLANKS       WVALOR
459500960527     C                   MOVEL     WVAL          WVALOR
459600960731     C                   SETON                                        05
459700960527     C                   EXSR      STPERR
459800960527     C                   MOVEL     *BLANKS       WVAL
459900960527     C                   MOVEL     *BLANKS       WTPI
460000960527     C                   END
460100960527     C*
460200960527     C                   ENDSR
460300960401     C*----------------------------------------------------------------
460400960401     C*? *INZSR - OPERAZIONI INIZIALI
460500960401     C*----------------------------------------------------------------
460600960401     C     *INZSR        BEGSR
460700960401     C*
460800040322     c     *entry        plist
460900040322     c                   parm                    User_msg         10
461000040322     C*
461100960527     C* Definisco chiavi di accesso
461200960527     C     KTAB1         KLIST
461300960401     C                   KFLD                    KKUT
461400960401     C                   KFLD                    KCOD
461500070123     C*
461600050112     C     KTAB          KLIST
461700050112     C                   KFLD                    KKUT
461800050112     C                   KFLD                    KCOD
461900050112     C                   KFLD                    KKEY
462000070123     C*
462100960401     C     KNUF          KLIST
462200960401     C                   KFLD                    KAAA
462300960401     C                   KFLD                    KCNU
462400960401     C                   KFLD                    KFIL
462500070123     C*
462600960924     C     KVAB1         KLIST
462700960925     C                   KFLD                    KCCM
462800960925     C                   KFLD                    KCMR
462900960925     C                   KFLD                    KCNT
463000960527     C                   KFLD                    KAAS
463100960527     C                   KFLD                    KLNP
463200960527     C                   KFLD                    KNRS
463300960527     C                   KFLD                    KNSP
463400070123     C*
463500960924     C     KVAB2         KLIST
463600960924     C                   KFLD                    KCCM
463700960924     C                   KFLD                    KCMR
463800070123     C*
463900960528     C     KRDE          KLIST
464000960528     C                   KFLD                    KAAS
464100961029     C                   KFLD                    KLNP1
464200960528     C                   KFLD                    KNRS
464300960528     C                   KFLD                    KNSP
464400960528     C                   KFLD                    KNMC
464500960528     C                   KFLD                    KNRP
464600070123     C*
464700960531     C     KACO          KLIST
464800960531     C                   KFLD                    KKUT
464900960531     C                   KFLD                    KKCC
465000960531     C                   KFLD                    KKSC
465100070123     C*
465200960531     C     KIND          KLIST
465300960531     C                   KFLD                    KKUT
465400960531     C                   KFLD                    KKCC
465500960531     C                   KFLD                    KKSC
465600980617      *
465700980617      * DETERMINO SE SONO BARTOLINI O SDI
465800020909     C                   CLEAR                   TIBS55DS
465900980617     C                   MOVEL     'L'           I50TLA
466000020909     C                   CALL      'TIBS55R'
466100020909     C                   PARM                    TIBS55DS
466200980617      *
466300960401     C* RECUPERO DATI DELL'UTENTE
466400960527     C                   Z-ADD     1             CODUT             1 0
466500960401     C                   CALL      'XPARUT'
466600000120     C                   PARM                    UTEDSE0F
466700980617     C                   MOVEL     RAGUT         RSUT             20
466800960531     C*
466900960531     C* RICERCA CAPOCONTI
467000960531     C                   DO        50            X
467100960531     C                   MOVE      TCU(X)        TCUDS
467200960531     C     F56           IFEQ      'CG'
467300960531     C     F34           ANDEQ     '01'
467400960531     C                   Z-ADD     KCU(X)        KCI               4 0
467500960531     C                   END
467600960531     C                   END
467700960402     C*
467800960402     C* IMPOSTO A ZERO VARIABILI DI PGM
467900040310     c                   move      *blank        Snd_Msg_alert     1
468000040607     c                   move      'N'           In_Stampa         1
468100960402     C                   MOVEL     'N'           WFINE             1
468200960527     C                   MOVEL     CA(1)         WCA              78
468300960527     C                   MOVEL     CN(1)         WCN              78
468400070215      *
468500960402     C* RECUPERO DATA E ORA
468600960527     C                   TIME                    WHHDAT           14 0
468700960527     C                   MOVE      WHHDAT        G02DAT
468800960913     C                   MOVE      WHHDAT        WDATA8            8 0
468900960913     C                   MOVE      WDATA8        WAA2              2 0
469000960913     C                   MOVEL     WDATA8        DATA              6 0
469100960913     C                   MOVE      WAA2          DATA
469200960527     C                   MOVEL     *BLANK        G02ERR
469300960527     C                   CALL      'XSRDA8'
469400960527     C                   PARM                    WLBDA8
469500960711     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
469600960527     C                   TIME                    WORA              6 0
469700070123     C*
469800960527     C* Caricamento Tabella 1B
469900970624     C                   Z-ADD     0             X                 4 0
470000960402     C                   Z-ADD     CODUT         KKUT
470100960402     C                   MOVEL     '1B'          KCOD
470200960527     C     KTAB1         CHAIN     TABEL00F                           30
470300960528     C     *IN30         DOWEQ     '0'
470400960527     C     TBLFLG        IFEQ      *BLANKS
470500960527     C                   ADD       1             X
470600960527     C                   MOVEL     TBLKEY        CTM(X)
470700960527     C                   MOVEL     TBLUNI        DTM(X)
470800960527     C                   END
470900960527     C     KTAB1         READE     TABEL00F                               30
471000960527     C                   END
471100020221      *
471200960822     C* Caricamento Tabella 15
471300020221     C                   Z-ADD     0             X                 4 0
471400960822     C                   Z-ADD     CODUT         KKUT
471500960822     C                   MOVEL     '15'          KCOD
471600960822     C     KTAB1         CHAIN     TABEL00F                           30
471700960822     C     *IN30         DOWEQ     '0'
471800960822     C     TBLFLG        IFEQ      *BLANKS
471900960822     C                   ADD       1             X
472000960822     C                   MOVEL     TBLKEY        C15(X)
472100960822     C                   MOVEL     TBLUNI        DS15
472200960822     C                   MOVEL     §15COD        ISO(X)
472300961017     C                   MOVEL     §15ECF        CPF(X)
472400961017     C                   MOVE      §15PCF        CPF(X)
472500960822     C                   END
472600960822     C     KTAB1         READE     TABEL00F                               30
472700960822     C                   END
472800070123      *
472900960527     C* Caricamento Tabella CV
473000970624     C                   Z-ADD     0             X
473100960527     C                   Z-ADD     CODUT         KKUT
473200960527     C                   MOVEL     'CV'          KCOD
473300960527     C     KTAB1         CHAIN     TABEL00F                           30
473400960528     C     *IN30         DOWEQ     '0'
473500960527     C     TBLFLG        IFEQ      *BLANKS
473600960527     C                   ADD       1             X
473700960527     C                   MOVEL     TBLKEY        CCV(X)
473800960527     C                   MOVEL     TBLUNI        DCV(X)
473900960527     C                   END
474000960527     C     KTAB1         READE     TABEL00F                               30
474100960527     C                   END
474200070123      *
474300960527     C* Caricamento Tabella Priorità trasporto
474400970624     C                   Z-ADD     0             X
474500960527     C                   MOVEL     'TS'          TABCOD
474600960528     C     TABCOD        CHAIN     EDTAB01L                           30
474700960528     C     *IN30         DOWEQ     '0'
474800960527     C     TABFLG        IFEQ      *BLANKS
474900960527     C                   ADD       1             X
475000960528     C                   MOVEL     TABKEY        PTR(X)
475100061128     C                   MOVEL     TABKEY        PTR35(X)
475200960527     C                   MOVEL     TABUNI        TSP(X)
475300960527     C                   END
475400960528     C     TABCOD        READE     EDTAB01L                               30
475500960527     C                   END
475600070123      *
475700960716     C* Caricamento Tabella Tipo Incasso C/Assegno
475800970624     C                   Z-ADD     0             X
475900960716     C                   MOVEL     'TC'          TABCOD
476000960716     C     TABCOD        CHAIN     EDTAB01L                           30
476100960716     C     *IN30         DOWEQ     '0'
476200960716     C     TABFLG        IFEQ      *BLANKS
476300960716     C                   ADD       1             X
476400061128     C                   MOVEL     TABKEY        CTC35(X)
476500061128     C                   MOVEL     TABKEY        CTC(X)
476600960716     C                   MOVEL     TABUNI        TPI(X)
476700960716     C                   END
476800960716     C     TABCOD        READE     EDTAB01L                               30
476900960716     C                   END
477000980618      *
477100980618      * Caricamento Tabella Partner esteri
477200970624     C                   Z-ADD     0             X
477300960527     C                   MOVEL     'PT'          TABCOD
477400960528     C     TABCOD        CHAIN     EDTAB01L                           30
477500960528     C     *IN30         DOWEQ     '0'
477600980618      *
477700980618     C                   SELECT
477800980618     C     TABFLG        WHENNE    *BLANKS
477900980618      *
478000980618     C                   OTHER
478100960527     C                   ADD       1             X
478200960527     C                   MOVEL     TABKEY        CPT(X)
478300070219     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
478400070219     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
478500960527     C                   MOVEL     TABUNI        DPT(X)
478600980618     C                   ENDSL
478700980617      *
478800960528     C     TABCOD        READE     EDTAB01L                               30
478900960527     C                   END
479000070219      * salva quanti Codici UNB ha caricato
479100070219     c                   z-add     x             sav_CPTx
479200980618      *
479300960912     C                   MOVEL     'PU'          TABCOD
479400960912     C     TABCOD        CHAIN     EDTAB01L                           30
479500960911     C     *IN30         DOWEQ     '0'
479600980618      *
479700980618     C                   SELECT
479800980618     C     TABFLG        WHENNE    *BLANKS
479900980618      *
480000980618     C                   OTHER
480100960911     C                   MOVEL     TABKEY        CODPU            35
480200960911     C                   Z-ADD     1             X
480300960911     C     CODPU         LOOKUP    CPT(X)                                 32
480400980618     C   32              MOVEL     TABUNI        DPU(X)
480500980618     C                   ENDSL
480600980618      *
480700960912     C     TABCOD        READE     EDTAB01L                               30
480800960911     C                   END
480900980618      *
481000070123      * Prosegue tab.PT e PU
481100070123     C                   MOVEL     'PV'          TABCOD
481200070123     C     TABCOD        CHAIN     EDTAB01L                           30
481300070123     C     *IN30         DOWEQ     '0'
481400070123      *
481500070123     C                   SELECT
481600070123     C     TABFLG        WHENNE    *BLANKS
481700070123      *
481800070123     C                   OTHER
481900070123     C                   MOVEL     TABKEY        CODPV            35
482000070123     C                   Z-ADD     1             X
482100070123     C     CODPV         LOOKUP    CPT(X)                                 32
482200070123     C   32              MOVEL     TABUNI        DPV(X)
482300070123     C                   ENDSL
482400070123      *
482500070123     C     TABCOD        READE     EDTAB01L                               30
482600070123     C                   END
482700070123      *
482800980618      * Caricamento Tabella termini consegna
482900970624     C                   Z-ADD     0             X
483000960527     C                   MOVEL     'TB'          TABCOD
483100960528     C     TABCOD        CHAIN     EDTAB01L                           30
483200960528     C     *IN30         DOWEQ     '0'
483300960527     C     TABFLG        IFEQ      *BLANKS
483400960527     C                   ADD       1             X
483500061128     C                   MOVEL     TABKEY        CTB35(X)
483600061128     C                   MOVEL     TABKEY        CTB(X)
483700960527     C                   MOVEL     TABUNI        POR(X)
483800960527     C                   END
483900960528     C     TABCOD        READE     EDTAB01L                               30
484000960527     C                   END
484100070123      *
484200961122     C* Caricamento Tabella codici clienti - tariffe
484300970624     C                   Z-ADD     0             X
484400961122     C                   MOVEL     'CL'          TABCOD
484500961122     C     TABCOD        CHAIN     EDTAB01L                           30
484600961122     C     *IN30         DOWEQ     '0'
484700961122     C     TABFLG        IFEQ      *BLANKS
484800961122     C                   ADD       1             X
484900961122     C                   MOVEL     TABKEY        CCL(X)
485000961122     C                   MOVEL     TABUNI        DCL(X)
485100961122     C                   END
485200961122     C     TABCOD        READE     EDTAB01L                               30
485300961122     C                   END
485400070123      *
485500960911     C* Caricamento Serivizi speciali
485600970624     C                   Z-ADD     0             X
485700960926     C                   MOVEL     'SS'          TABCOD
485800960911     C     TABCOD        CHAIN     EDTAB01L                           30
485900960911     C     *IN30         DOWEQ     '0'
486000960911     C     TABFLG        IFEQ      *BLANKS
486100960911     C                   ADD       1             X
486200960926     C                   MOVEL     TABKEY        SS(X)
486300061128     C                   MOVEL     TABKEY        SS35(X)
486400960926     C                   MOVEL     TABUNI        DSS(X)
486500960911     C                   END
486600960911     C     TABCOD        READE     EDTAB01L                               30
486700960911     C                   END
486800000125      *
486900960712     C                   MOVEL     *ALL'-'       CMP132          132
487000000125     C                   eval      *inOA = *ON
487100000125     C                   eval      *inOF = *ON
487200020909     C                   CLEAR                   EDiVAB00
487300000125     C                   clear                   Wvablnp
487400040301     C                   clear                   Wvabnrs
487500040301     C                   clear                   Wvabctm
487600050223     C                   clear                   WVABfnsp
487700960726     C                   MOVEA     *HIVAL        SGN
487800960927     C*  PRIMA OPEN SYSPRT
487900960927     C                   OPEN      SYSPRT
488000960402     C*
488100040416     c                   clear                   errori_prtf       1
488200040416     C*
488300960402     C                   ENDSR
488400040227     c*==================================================================*
488500070123      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
488600040227     c*==================================================================*
488700070123     c     Imp_ADDR_mail begsr
488800040227      *
488900070202     c                   clear                   Indirizzi
489000070202      *
489100070202      *  Lunghezza indirizzi presenti
489200070202     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
489300070202     c                   z-add     1             al_char           3 0
489400070202     c                   z-add     1             dal_char          3 0
489500070202     c                   z-add     1             tot_char          3 0
489600070202      *
489700070202     c     successivo    tag
489800070202      *
489900070202      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
490000070202      *   e aggiunge il dominio Bartolini + il (;) separatore
490100070202     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
490200070206     c                   eval      tot_char = al_char - dal_char
490300070205     c                   z-add     dal_char      Dac
490400070205     c                   z-add     tot_char      Luc
490500070202      *
490600070202     c                   clear                   Indir_Bartol     40
490700070205     c                   clear                   Indir_Parz       20
490800070205     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
490900070205     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
491000070205     c                             %Trim(bart_it)
491100070202     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
491200070202      *
491300070202     c                   if        al_char = Lun_Ind
491400070202     c                   goto      fine_indmail
491500070202     c                   else
491600070202     c                   eval      dal_char = ( al_char + 1 )
491700070202     c                   goto      successivo
491800070202     c                   end
491900070202      *
492000070202     C     fine_indmail  TAG
492100070202      *
492200070202     C                   ENDSR
492300070123     c*==================================================================*
492400070123      * Manda un Msg x E-mail
492500070123     c*==================================================================*
492600070123     c     Invio_Mail    begsr
492700070123      *
492800070122     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
492900070122     C* Inizializzo variabili
493000070122     C                   movel     *blanks       wrkEml          253
493100070122     C                   movel     *blanks       wrkMsg         5000
493200070122     C                   movel     *blanks       wrkOgg           44
493300070122      *
493400070122     C* Valorizzo i campi della e-m@ail - indirizzo
493500070202     C                   eval      wrkEml = Indirizzi
493600070123     C                   eval      wrkOgg = Oggetto
493700070122     C                   eval      wrkMsg = Messaggio
493800110203      ********
493900110203     C********           call(e)   'TIS701C'
494000110203     C********           parm                    wrkEml
494100110203     C********           parm                    wrkOgg
494200110203     C********           parm                    wrkMsg
494300110203     C*
494400110203      ** *****
494500110203     C                   call(e)   'TRTCT00R2'
494600110203     C                   parm                    wrkEml
494700110203     C                   parm                    wrkOgg
494800110203     C                   parm                    wrkMsg
494900110203     C*
495000070122
495100070122     C*
495200070122     C                   ENDSR
495300070122     c*==================================================================*
495400070122      * Manda un Msg in Posta AS Sede a EDP*
495500070122     c*==================================================================*
495600070122     c     SEND_MSG_EDP  begsr
495700070122      *
495800040227      * INVIA MESSAGGIO ad un Utente --> EDPAB
495900040310      *   Lo manda una sola volta per lavoro
496000040322     c                   IF        Snd_Msg_alert  <> 'N' and
496100040322     c                             User_msg <> *blank
496200040227     c                   z-add     1             Jx
496300040227     C                   exsr      CalQEZSNDMG
496400040310     c                   end
496500040227      *
496600040227     c                   endsr
496700040227     ***********************************************************************
496800040227     **
496900040227     ** Send Message (QEZSNDMG) API
497000040227     **
497100040227     ***********************************************************************
497200040227     C     CalQEZSNDMG   BEGSR
497300040227      *
497400040227     ** Invio messaggio all'utente.
497500040227     C                   EVAL      SndMg03 = msgDSP
497600040322     C                   EVAL      SndMg05(Jx) = User_msg
497700040227     C                   EVAL      SndMg06 = Jx
497800040227     C                   CLEAR                   QUSEC
497900040227     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
498000040227
498100040227     C                   CALL      'QEZSNDMG'
498200040227     C                   PARM                    SndMg01
498300040227     C                   PARM                    SndMg02
498400040227     C                   PARM                    SndMg03
498500040227     C                   PARM                    SndMg04
498600040227     C                   PARM                    SndMg05
498700040227     C                   PARM                    SndMg06
498800040227     C                   PARM                    SndMg07
498900040227     C                   PARM                    SndMg08
499000040227     C                   PARM                    Qusec
499100040227     C                   PARM                    SndMg10
499200040227     C                   PARM                    SndMg11
499300040227     C                   PARM                    SndMg12
499400040227      *
499500040227     C                   ENDSR
499600050112     c**********************************************************************
499700050112     c* reperimento numero Spedizione
499800050112     c**********************************************************************
499900050112     C     repnum        BEGSR
500000050112      *
500100050112      *    deve controllare sulla tabella "3C" se il numero spedizione
500200050112      *     deve essere mantenuto oppure no
500300050112     C*
500400050112     C                   clear                   Ds3C
500500050112     C                   Z-ADD     CODUT         KKUT
500600050112     C                   MOVEL     '3C'          KCOD
500700050112     C                   movel(p)  VABccm        KKey
500800050112     C     KTAB          CHAIN     TABEL00F                           30
500900050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
501000050112     C                   MOVEL     TBLUNI        Ds3C
501100050112     C                   END
501200050112      *
501300050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
501400050112     c                   if        §3CFsp <> 'S'
501500050112      *
501600050112     C* NSP => Stacco un numeratore da AZNUM
501700050112     c                   movel     kpjbu         svkpjbu
501800050112     c                   clear                   kpjbu
501900050112     C                   clear                   TRUL33DS
502000050112     C                   eval      I33OPE = *zeros
502100050112     C                   eval      I33CNU = 302
502200050112     C                   eval      I33NUM = 1
502300050112     C                   movel     TRUL33DS      KPJBU
502400050112     C                   call      'TRUL33R'
502500050112     C                   parm                    KPJBA
502600050112     C                   movel     KPJBU         TRUL33DS
502700050112     c                   movel     svkpjbu       kpjbu
502800050112      ****
502900050112     C                   if        O33ERR = *zeros
503000050112     c                   z-add     o33nrf        NUM_SPED
503100050112     C                   else
503200050112     C                   Z-ADD     19            WINDER
503300050112     C                   MOVEL     *BLANKS       WVALOR
503400050112     C                   SETON                                        05
503500050112     C                   EXSR      STPERR
503600050112     C                   endif
503700050112      ****
503800050112     c                   ELSE
503900050112      *   se si vuole mantenere il numero spedizione
504000050112      ****
504100050112     C                   MOVEL     WOGGI         WANNO             4 0
504200050112     C                   MOVE      WANNO         KAAA
504300050112     C                   Z-ADD     3             KCNU
504400050112     C                   Z-ADD     VABlnp        KFIL
504500050112     C     KNUF          CHAIN     FLNUF                              9999
504600050112     C     *IN99         IFEQ      '0'
504700050112     C                   ADD       1             NUFNUM
504800050112     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
504900050112     C                   UPDATE    FLNUF
505000050112     C                   ELSE
505100050112     C                   Z-ADD     19            WINDER
505200050112     C                   MOVEL     *BLANKS       WVALOR
505300050112     C                   SETON                                        05
505400050112     C                   EXSR      STPERR
505500050112     C                   END
505600070205      *****
505700050112     c                   EndIF
505800050112      **
505900050112     C                   ENDSR
506000110615      * ?------------------------------------------------------------------ */
506100110615      *
506200110615      * ?------------------------------------------------------------------ */
506300110615     c     Instrada_sped begsr
506400110615      *
506500110615      * -------------
506600110615     C* Ricerco CAP in base alle dimensioni
506700110615     C     Wcap          IFne      *BLANKS
506800110615      *
506900110615      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
507000110615     C                   CLEAR                   FNLV55DS
507100110615     C                   MOVEL     'P'           D55TPT
507200110615     C                   MOVEL     WOGGI         D55DRF
507300110615     C                   MOVEL     VABLNP        D55LNP
507400110615     C                   MOVEL     VABLNP        D55LIN
507500110615     C                   CALL      'FNLV55R'
507600110615     C                   PARM                    FNLV55DS
507700110615     C                   MOVE      D55TFP        COMTFP            3 0
507800110615      *
507900110615     C*  PASSO IL TERMINAL PARTENZA
508000110615     C                   CLEAR                   TISI95DS
508100110615     C                   Z-ADD     COMTFP        I95TFP
508200110615     C                   MOVEL     VABTSP        I95TSP
508300110615     C                   MOVEL     'S'           I95FRE
508400110615     C                   MOVEL     '3'           I95TCN
508500110615     C                   MOVEL     WCAP          I95CAP
508600110615     C                   MOVEL     VABNZD        I95NAR
508700110615     C                   MOVEL     VABLOD        I95LOC
508800110615     C                   MOVEL     VABIND        I95IND
508900110615      *
509000110615     C* Se gestita seconda linea di arrivo passo peso - volume
509100110615     C* e numero colli effettivo
509200110615     C     §PULNA        IFEQ      'S'
509300110615     C                   Z-ADD     VABPKB        I95LKG
509400110615     C                   Z-ADD     VABVLB        I95LMC
509500110615     C                   END
509600110615     C*
509700110615     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
509800110615     C                   SELECT
509900110615     C     VABCBO        WHENEQ    '1 '
510000110615     C                   MOVEL     *BLANKS       I95FCA
510100110615     C                   MOVEL     'F'           I95TPO
510200110615     C     VABCBO        WHENEQ    '2 '
510300110615     C                   MOVEL     *BLANKS       I95FCA
510400110615     C                   MOVEL     'A'           I95TPO
510500110615     C     VABCBO        WHENEQ    '4 '
510600110615     C                   MOVEL     'S'           I95FCA
510700110615     C                   MOVEL     'F'           I95TPO
510800110615     C     VABCBO        WHENEQ    '6 '
510900110615     C                   MOVEL     'S'           I95FCA
511000110615     C                   MOVEL     'A'           I95TPO
511100110615     C                   OTHER
511200110615     C                   MOVEL     'F'           I95TPO
511300110615     C     VABCAS        IFGT      0
511400110615     C                   MOVEL     'S'           I95FCA
511500110615     C                   ELSE
511600110615     C                   MOVEL     *BLANKS       I95FCA
511700110615     C                   END
511800110615     C                   ENDSL
511900110615     C*
512000110615     C                   CALL      'TISI95R'
512100110615     C                   PARM                    TISI95DS
512200110615     C*
512300110615     C* Reperisco i dati da CAP
512400110615     C                   MOVEL     O95PRV        VABPRD
512500110615     C                   MOVEL     WCAP          VABCAD
512600110615      ***
512700110615     c                   MOVEL     O95LNA        VABLNA
512800110615     C                   MOVEL     O95ZNC        VABZNC
512900110615     C*
513000110615     C     O95ERR        IFNE      *BLANKS
513100110615     C     O95FIT        ANDEQ     'S'
513200110615     C*  Cap non trovato
513300110615     C                   END
513400110615      *
513500110615     C                   ELSE
513600110615      *
513700110615     C                   CLEAR                   VABPRD
513800110615     C                   CLEAR                   VABLNA
513900110615     C                   CLEAR                   VABZNC
514000110615     C                   CLEAR                   VABCAD
514100110615     C                   END
514200110615      *
514300110615     c                   endSR
514400110615      * ?------------------------------------------------------------------ */
514500960401     O*****************************************************************
514600960528     OQSYSPRT   E            TESTA            01
514700960527     O                       RSUT                20
514800960527     O                                           50 '** STAMPA ERRORI RICEZI'
514900960527     O                                              'ONE BOLLE DA PARTNER'
515000960527     O                                              ' ESTERI **'
515100960527     O                       DATA               121 '  /  /  '
515200960527     O                                          127 'Pag.:'
515300960527     O                       PAGE          Z    132
515400970206     O          E            TESTA          1 02
515500960527     O                       WORA               121 '  :  :  '
515600960927     O*------------------------------------
515700960927     O          E            ERRCLI         1
515800960927     O                                           15 'CODICE CLIENTE:'
515900960927     O                       §PTKSC        Z     23
516000960927     O                       ACORAG              72
516100960927     O*------------------------------------
516200960927     O          E            ERRDOC         1
516300960927     O                                           15 'CODICE CLIENTE:'
516400960927     O                       §PTKSC        Z     23
516500960927     O                       ACORAG              72
516600960927     O                                           78 'NUMERO'
516700960927     O                       TIPDOC              82
516800960927     O                                           83 ':'
516900960927     O                       WDOCUM             120
517000960401     O*------------------------------------
517100960528     O          E            DETER          1
517200960731     O                       SERR                60
517300960731     O                       WVALOR              96
517400070206     O               05      WidSPEp            132
517500960926     O*------------------------------------
517600960926     O          E            ERRQUA         1
517700960926     O                                         +  0 'Totale colli/spediz. '
517800960926     O                                         +  0 'msg.:'
517900960926     O                       WCOC          4   +  1
518000960926     O                                         +  0 '/'
518100960926     O                       WSPC          4   +  0
518200960926     O                                         +  1 '<> da quello indica'
518300960926     O                                         +  0 'to:'
518400040607     O                       WCOL          4   +  0
518500960926     O                                         +  0 '/'
518600040607     O                       WSPM          4   +  1
518700960926     O                       WNRDOC             132
518800961016     O*------------------------------------
518900960527     O          E            STAFIN           60
519000960401     O                                           76 '*** FINE STAMPA ***'
519100960401     O*----------------------------------------------------------------
519200960628     O*  Stampa dati ricevuti dal partner
519300960628     O*----------------------------------------------------------------
519400960628     OSYSPRT    E            TEST1            01
519500960628     O                       RSUT                20
519600960628     O                                           48 '** STAMPA DATI RICEVUTI'
519700960917     O                                         +  1 'IN FASE DI IMPORTAZIONE'
519800960628     O                                              ' BOLLE DA PARTNER ESTER'
519900960628     O                                              'I **'
520000960628     O                       DATA               121 '  /  /  '
520100960628     O                                          127 'Pag.:'
520200960628     O                       PAGE          Z    132
520300960628     O          E            TEST1          2 02
520400960628     O                       WORA               121 '  :  :  '
520500960628     O*  Dati messaggio
520600960725     O          E            DATMSG         1
520700960725     O                       CMP132             132
520800960628     O          E            DATMSG         1
520900960628     O                                           18 'DATI MESSAG: MITT.'
521000070109     O                       T000004           +  1
521100070109     O                       T000007           +  1
521200960628     O                                         +  1 'DESTIN.'
521300070109     O                       T000010           +  1
521400070109     O                       T0000071          +  1
521500960628     O                                         +  1 'DATA'
521600960712     O                       WDTMSG        Y   +  1
521700960628     O                                         +  1 'ORA'
521800070109     O                       T000019           +  1 '  :  '
521900960628     O          E            DATMSG         1
522000960712     O                                           18 'DOCUMENTO..: NOME:'
522100070109     O                       T001000           +  1
522200960712     O                                         +  6 'NUMERO:'
522300070109     O                       T001004           +  1
522400960628     O                                         +  1 'COD.'
522500070109     O                       T001001           +  1
522600960712     O                                         +  1 'FUNZIONE'
522700070109     O                       T001225           +  1
522800960628     O*  Note intero documento
522900960628     O          E            NOTMSG      1
523000960628     O                                           18 'NOTE DOCUMENTO...:'
523100070109     O                       T054451           +  1
523200960628     O          E            NOTMS1         1
523300960628     O                       WNOT              +  1
523400960628     O*  Totali colli/spedizioni ....
523500960628     O          E            TOTDOC      1  1
523600960712     O                                           12 'TOT.SPEDIZ.:'
523700070215     O                       WTOTSP            +  1 '          .   .   '
523800960628     O                                         +  1 'TOT.COLLI:'
523900070215     O                       WTOTCO            +  1 '          .   .   '
524000960628     O                                         +  1 'TOT.PESO NETTO:'
524100070215     O                       WTOTPN            +  1 '          .   ,   '
524200960628     O                                         +  1 'TOT.PESO LORDO:'
524300070215     O                       WTOTPL            +  1 '          .   ,   '
524400960628     O*  Nr.Documento/data
524500960712     O          E            CMRDOC         1
524600960712     O                                           12 'NUMERO CMR.:'
524700070109     O                       T151154           +  1
524800960712     O                                           67 'DATA..:'
524900960712     O                       WSTCMR        8   +  1
525000960712     O                                          108 'ORA:'
525100960628     O                       WHHCMR            +  1 '  :  '
525200960628     O          E            FVVDOC         1
525300960725     O                                           12 'F. VIAGGIO.:'
525400070109     O                       T151154           +  1
525500960712     O                                           67 'DATA..:'
525600960712     O                       WSTDFV        8   +  1
525700960712     O                                          108 'ORA:'
525800960702     O                       WHHNFV            +  1 '  :  '
525900960628     O*  Dati trasporto
526000960712     O          E            DATTRA         1
526100960712     O                                           12 'TIPO TRASP.:'
526200070109     O                       T208051           +  1
526300960628     O                                         +  1 'MODAL.:'
526400070109     O                       T208067           +  1
526500960628     O                                         +  2 'VETTORE:'
526600070109     O                       T203128           +  1
526700960628     O                                         +  1 'ID.TRASPORTO:'
526800070109     O                       T208212           +  1
526900960628     O*  Luogo di partenza/arrivo
527000960628     O          E            DATLOC      1
527100960712     O                                           14 'LOC.PARTENZA.:'
527200960628     O                       WLOCPA            +  1
527300960628     O                                         +  1 'DATA'
527400960913     O                       WSTDPA        8   +  1
527500960628     O                                         +  1 'ORA'
527600960712     O                       WHHPA             +  1 '  :  '
527700960628     O                                         +  1 'LOC.ARRIVO:'
527800960628     O                       WLOCAR            +  1
527900960628     O*  Dati Partner
528000960712     O          E            DATPAM         1
528100960712     O                                           14 'PARTNER MITT.:'
528200070109     O                       T253039           +  1
528300960628     O          E            DATPAD         1
528400960712     O                                           14 'PARTNER DEST.:'
528500070109     O                       T253039           +  1
528600960628     O*  Dati Automezzo
528700960628     O          E            DATAUT         1
528800960628     O                                           13 'TP.AUTOMEZZO:'
528900070109     O                       T308260           +  1
529000960628     O                                         +  1 'NR.PALLETS:'
529100070109     O                       T306350       4   +  1
529200960628     O                                         +  1 'NR.PIOMBO:'
529300070109     O                       T309308           +  1
529400960628     O*  Spedizione
529500960913     O          E            DATSPE         1
529600960712     O                       CMP132             132
529700960712     O          E            DATSPE         1
529800040310     O                                           13 'NR.SPEDIZ...:'
529900070109     O                       D001004           +  1
530000960628     O*  Istruzioni/Note Mittente
530100960712     O          E            NOTSPE         1
530200070125     O                       TIPNOTE             15
530300960628     O               04                          13 'ISTRUZIONI..:'
530400070125     O               05                          13 'x DESTINATARI'
530500960628     O          E            NOTSP1         1
530600070125     O                       WNOTSP              87
530700070205     O*  Dati colli    .
530800070205     O          E            SPECOL         1
530900070205     O                                           14 'NR.COLLI SPED:'
531000070205     O                       WVABNCL       4   +  1
531100070205     O                                         +  3 'PESO SPED:'
531200070205     O                       WVABPKB       2   +  1
531300960628     O*  Riferimenti consegna: AGE,CMR....
531400960712     O          E            RIFCON         1
531500960702     O                                           12 'RF.CONSEGNA:'
531600070109     O                       D101153           +  1
531700070109     O                       D101154           +  1
531800070109     O                       D1011531          +  1
531900070109     O                       D1011541          +  1
532000070109     O                       D1011532          +  1
532100070109     O                       D1011542          +  1
532200070201     O*  Riferimenti consegna: AGE,CMR....
532300070201     O          E            RIFCOA         1
532400070201     O                                           12 'RF.CONSEGNA:'
532500070201     O                       D1A1153           +  1
532600070201     O                       D1A1154           +  1
532700070201     O*  Riferimenti consegna: AGE,CMR....
532800070201     O          E            RIFSCO         1
532900070201     O                                           14 'RF.CONS.COLLO:'
533000070201     O                       D271153           +  1
533100070201     O                       D271154           +  1
533200960628     O*  Soggetti transazione: CZ, CN, SF, ...
533300070202     O          E            SOGTRT         1
533400960913     O               07                          14 'DESTINATARIO.:'
533500960913     O              N07                          14 'MITTENTE.....:'
533600070202     O                       T253035           +  1
533700070202     O                       T253055           +  2
533800070202     O              N04      T253207           +  1
533900070202     O              N04      T253039           +  1
534000070202     O                       T253036           +  5
534100070202     O              N04      T2530361          +  1
534200070202     O          E   N04      SOGTRT         1
534300070202     O                       T253042             50
534400070202     O                       T2530421          +  1
534500070202     O                       T253164           +  1
534600070202     O                       T253251           +  1
534700070202     O*  Soggetti transazione: CZ, CN, SF, ...
534800070202     O          E            SOGTRA         1
534900070202     O               07                          14 'DESTINATARIO.:'
535000070202     O              N07                          14 'MITTENTE.....:'
535100070202     O                       D153035           +  1
535200070202     O                       D153055           +  2
535300070202     O              N04      D153207           +  1
535400070202     O              N04      D153039           +  1
535500070202     O                       D153036           +  5
535600070202     O              N04      D1530361          +  1
535700070202     O          E   N04      SOGTRA         1
535800070202     O                       D153042             50
535900070202     O                       D1530421          +  1
536000070202     O                       D153164           +  1
536100070202     O                       D153251           +  1
536200960628     O*  Rivolgersi a...
536300070202     O          E            SOGRIT         1
536400960628     O                                           14 'RIVOLGERSI A.:'
536500070202     O                       T253412           +  1
536600070202     O                       T253155           +  1
536700070202     O                       T253148           +  1
536800070202     O                       T2531551          +  1
536900070202     O                       T2531481          +  1
537000070202     O*  Rivolgersi a...
537100070202     O          E            SOGRIV         1
537200070202     O                                           14 'RIVOLGERSI A.:'
537300070202     O                       D153412           +  1
537400070202     O                       D153155           +  1
537500070202     O                       D153148           +  1
537600070202     O                       D1531551          +  1
537700070202     O                       D1531481          +  1
537800960628     O*  Dati articolo
537900960628     O          E            DATART         1
538000960628     O                                           14 'NR.ARTICOLO..:'
538100070109     O                       D207140A          +  1
538200070109     O                       D207143A          +  1
538300070109     O                       D204440A          +  1
538400960917     O*  Dati colli    .
538500960917     O          E            DATCOL         1
538600960917     O                                           14 'NUMERO COLLI.:'
538700960917     O                       VABNCL        4   +  1
538800960917     O                                         +  3 'PESO.:'
538900960917     O                       VABPKB        2   +  1
539000960917     O                                         +  2 'VOLUME:'
539100990616     O                       PVLB          2   +  1
539200960712     O          E            LETVET         1
539300960712     O                                           14 'NS LETT.VETT.:'
539400960712     O                       VABAAS        4   +  1
539500040302     O                       VABlnp        4   +  1
539600960712     O                       VABNRS        4   +  1
539700960712     O                       VABNSP        4   +  1
539800960918     O                                         +  2 'CONTRASSEGNO.:'
539900960918     O                       VABCAS        2   +  1
540000960918     O                       VABVCA            +  1
540100960628     O*  Nr.segnacolli
540200960725     O          E            DATSGN
540300960628     O                                           14 'NR.SEGNACOLLI:'
540400960628     O                       SGN1                50
540500960628     O                       SGN2              +  1
540600960628     O                       SGN3              +  1
540700961122** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
540800960725ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
540900960527** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
541000960725987654321098765432109876540123456789987654321098765432109876540999999999999999 1
541100960527** CN
541200960731Rif.mittente msg. non codificato in tabella Partner Esteri                     1
541300070115Data documento non valida CMR o AFB                                            2
541400960626Numero CMR obbligatorio ma non immesso                                         3
541500960530Numero spedizione passato dal cliente non valido                               4
541600960527Data consegna richiesta non valida                                             5
541700960527Priorità spedizione non codificata in tabella TS                               6
541800960527Termine consegna non codificato in tabella TB                                  7
541900070131Non indicato alcun termine di consegna TOD                                     8
542000960702Data partenza non valida                                                       9
542100960527C.A.P. destinatario non valido                                                10
542200960527C.A.P. mittente originale non valido                                          11
542300960527Campo importo non numerico                                                    12
542400960527Qualificatore numero colli non numerico                                       13
542500960527Numero colli non numerico                                                     14
542600960527Peso della spedizione non numerico                                            15
542700960527Volume della spedizione non numerico                                          16
542800980219Numero segnacolli passati è diverso da quello indicato                        17
542900960527Non trovato codice trattamento merce corrispondente                           18
543000960527Non trovato numeratore per attribuzione nr.spedizione                         19
543100960527Valuta importo non codificata in tabella CV                                   20
543200960702Data foglio viaggio non valida                                                21
543300960926Totale colli/spedizioni msg. <> da indicato                                   22
543400960731Numero AFB obbligatorio ma non immesso                                        23
543500960911Codice servizi speciali inesistente                                           24
543600960912Ricevuta da partner lnp non numerica. Impostata da tab.PT                     25
543700960912Linea di partenza del segnacollo diversa da quella tabella PT                 26
543800960912Ricevuta da partner lna non numerica. Ricalcolata da CAP                      27
543900960912Ricevuto numero serie non numerico. Ricalcolata da tab.PT                     28
544000960912Numero di serie del segnacollo diversa da quella tab.PT.                      29
544100990715Ricevuta da partner zona consega non numerica: SEGNACOLLO IGNORATO            30
544200990715Ricevuta da partner nr.segnacollo non numerico: SEGNACOLLO IGNORATO           31
544300961122Codice inesistente nella tabella dei clienti - tariffe                        32
