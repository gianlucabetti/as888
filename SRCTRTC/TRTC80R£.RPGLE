000100980618      ***********************************************************************
000200960524      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960329      ***********************************************************************
000400960329     H DECEDIT('0,') DATEDIT(*DMY.)
000500000120      *---------------------------------------------------------------------*
000600960524     FEDIFCSUM  UF   E           K DISK
000700040301     F                                     INFDS(SRECDS)
000800960528     FEDRDE01L  UF A E           K DISK
000900960329     FFLNUF01L  UF   E           K DISK
001000960329     FTABEL00F  IF   E           K DISK
001100990922     FCNACO00F  IF   E           K DISK
001200990922     FCNIND00F  IF   E           K DISK
001300960524     FEDTAB01L  IF   E           K DISK
001400020909      *
001500020909     FEDiVAB1L  UF A E           K DISK
001600020909     FEDiVAD0F  O  A E           K DISK
001700020909     FEDiVAT0F  O  A E           K DISK
001800020909      *
001900000207     FEDSUM00F  O  A E           K DISK
002000960527     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002100000120     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)   USROPN
002200000120      *---------------------------------------------------------------------*
002300000120      * Schiere
002400000120      *---------------------------------------------------------------------*
002500000120      * Schiera per ricezione testi liberi record TT05
002600000120      * Schiera per reperimento totali peso/volume
002700000120      * Schiera per reperimento località record TT20
002800000120      * Schiera per reperimento nomi ed indirizzi spedizionieri
002900000120      * Schiera per reperimento importo/valuta tipo/qualif.imp.
003000000120      * Schiera per reperimento testo libero singola sped.
003100000120      * Schiera per reperimento nr.riferimento spedizione
003200000120      * Schiera per reperimento persona a cui è diretta comunic.
003300000120      * Schiera per reperimento nr.seganacollo
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
005500000120      * Tabella partner
005600121105     D CPT             S             35    DIM(200)
005700121105     D CPTparz         S             35    DIM(%elem(CPT))
005800121105     D KSC_UNB         S              7    DIM(%elem(CPT))
005900121105     D DPT             S             90    DIM(%elem(CPT))
006000121105     D DPU             S             90    DIM(%elem(CPT))
006100121105     D DPV             S             90    DIM(%elem(CPT))
006200070219     d Ptn_parziale    s              1    inz('N')
006300070219     d UNB_Parz        s             35    inz(' ')
006400070219     d UNB_Generico    s             35    inz(' ')
006500070219     d sav_CPTx        s              3  0 inz(0)
006600070219      *
006700000120      * Tabella CL --> codici clienti - tariffa
006800961122     D CCL             S             35    DIM(300)
006900961122     D DCL             S             90    DIM(300)
007000000120      * Schiere x caricamento cod.cliente  <>
007100961204     D KCL             S              7  0 DIM(300)
007200000120      * Priorità trasporto
007300061128     D PTR35           S             35    DIM(100)
007400960528     D PTR             S              3    DIM(100)
007500960527     D TSP             S              1    DIM(100)
007600000120      * Termini di consegna
007700061128     D CTB35           S             35    DIM(100)
007800061128     D CTB             S              3    DIM(100)
007900960527     D POR             S              1    DIM(100)
008000000120      * Tipo pagamento C/Assegno
008100061128     D CTC35           S             35    DIM(100)
008200960716     D CTC             S              2    DIM(100)
008300960716     D TPI             S              2    DIM(100)
008400000120      * Decodifiche servizi speciali
008500061128     D SS35            S             35    DIM(100)
008600061128     D SS              S              3    DIM(100)
008700960911     D DSS             S             90    DIM(100)
008800000120      * Schiere x routine di conversione CAP
008900960828     D CAP5            S              1    DIM(5)
009000960828     D CAP9            S              1    DIM(9)
009100991007     D CAPM            S              1    DIM(9)
009200000120      * Schiera per memorizzazione FTX ricevuti
009300970520     D QUA             S              3    DIM(100)
009400970520     D FTX             S             70    DIM(100)
009500000120      * Tabelle capiconto
009600000120      * Schiere x località
009700970710     D LOD             S              1    DIM(35)
009800000120      *---------------------------------------------------------------------*
009900000120      * DS
010000000120      *---------------------------------------------------------------------*
010100000120      * .. per scompozione dati ricevuti a seconda del tipo record
010200000120     D EDTT00DS      E DS
010300000120     D EDTT05DS      E DS
010400000120     D  T05                   13    362    DIM(5)
010500000120     D EDTT10DS      E DS
010600000120     D  T10                   10     53    DIM(2)
010700000120     D EDTT15DS      E DS
010800000120     D EDTT20DS      E DS
010900000120     D  T20                  106    249    DIM(3)
011000000120     D  T20D                 250    303    DIM(3)
011100000120     D EDTT25DS      E DS
011200000120     D  T25                   68    123    DIM(2)
011300000120     D EDTT30DS      E DS
011400960822     D  W6350                 33     47
011500000120     D EDTD00DS      E DS
011600000120     D  D00                   77    142    DIM(3)
011700000120     D EDTD05DS      E DS
011800000120     D  D05                   13    362    DIM(5)
011900000120     D  D05A                 366    715    DIM(5)
012000000120     D EDTD10DS      E DS
012100000120     D  D10                   73    186    DIM(3)
012200000120     D EDTD15DS      E DS
012300000120     D  D15                  445    500    DIM(2)
012400000120     D EDTD20DS      E DS
012500960711     D  W1496                 10     14
012600960711     D  W7224                 15     22
012700000120     D EDTD25DS      E DS
012800960711     D  WPESO                 22     37
012900000120     D EDTD30DS      E DS
013000070216     D  D30_1                 10    359    DIM(10)
013100070216     D  D30_2                360    709    DIM(10)
013200070216      **
013300070216     D  X30            S             35    DIM(20)                              generico segnacolli
013400070216      **
013500000120     D EDTD35DS      E DS
013600000120     D EDTD40DS      E DS
013700050112      *
013800050112      * Numeratore Bolle (302)
013900050112     D trul33ds      E DS
014000050112      *
014100050112     D Ds3C          E DS
014200040301      *
014300040301      * numero relativo di record
014400040301     D SRECDS          DS
014500040301     D  NRREC                397    400B 0
014600040301      *
014700000120      *  Ds per reperimento dati peso/Volume intera spedizione
014800960524     D WTT10           DS
014900960524     D  WCTR                   1      3
015000960527     D  WVLU                   4     19  0
015100960524     D  WTPM                  20     22
015200000120      *  Ds per reperimento località
015300960524     D WTT20           DS
015400960524     D  WTPL                   1      3
015500960524     D  WLOC                   4     28
015600960725     D  WTP1                  29     31
015700960725     D  WLO1                  32     48
015800000120      *  Ds per reperimento date partenza/arrivo...
015900960524     D WTT20D          DS
016000960524     D  WTPD                   1      3
016100960524     D  WDTA                   4     15
016200960524     D  WFOR                  16     18
016300000120      *  Ds per reperimento dati comunicazioni
016400960524     D WTT25           DS
016500960524     D  WTPC                   1      3
016600960524     D  WCOM                   4     28
016700000120      *  Ds per reperimento dati importi
016800960524     D WTD00           DS
016900960524     D  WTPI                   1      3
017000960524     D  WIMP                   4     19  3
017100960524     D  WVAL                  20     22
017200000120      *  Ds per reperimento riferimeti singola spedizione
017300960524     D WTD10           DS
017400960524     D  WTPR                   1      3
017500960524     D  WRIF                   4     38
017600000120      * DS per reperimento destinatario comunicazione
017700960524     D WTD15           DS
017800960524     D  WTCD                   1      3
017900960524     D  WCMD                   4     28
018000000120      *---------------------------------------------------------------------*
018100000120      * .. per reperimento tipo record di EDIFTMIN letto
018200000120      *                    .. 5 - 8  : tipo record letto
018300960524     D WDAT            DS           587
018400960524     D  SUMTPR                 5      8
018500000120      *---------------------------------------------------------------------*
018600980617     D KPJBA         E DS
018700000120     D FNLV55DS      E DS
018800020909     D TIBS55DS      E DS
018900000120     D TISI95DS      E DS
019000000120     D UTEDSE0F      E DS
019100000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
019200000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
019300000120      * Ds scomposzione tipo capoconti
019400960531     D TCUDS           DS
019500960531     D  F34                    3      4
019600960531     D  F56                    5      6
019700961028     D DS1B          E DS
019800960822     D DS15          E DS
019900960527     D DSCV          E DS
020000000120     D TRTC86DS      E DS
020100000120     D EDIDSPT       E DS
020200000120     D EDIDSPU       E DS
020300070219     D EDIDSPV       E DS
020400070219      **
020500000120     D EDIDSCL       E DS
020600000120     D EDIDSSS       E DS
020700020909      *  DS x salvataggio dati impostati in EDiVAB
020800020909     D SAVBOL        E DS                  EXTNAME(EDiVAB1L)
020900960924     D SALVA           DS           545
021000960528     D WLBDA8          DS
021100960528     D  G02DAT                 1      8  0
021200960528     D  G02INV                 9     16  0
021300960528     D  G02ERR                17     17
021400960528     D  G02TGI                18     22  0
021500000120      *  DS x scomposizione numero spedizione
021600960530     D DSSPE           DS
021700960530     D  SPEAAS                 1      4  0
021800960530     D  SPELNP                 5      7  0
021900960530     D  SPENRS                 8      9  0
022000960530     D  SPENSP                10     16  0
022100000120      *  DS x scomposizione segnacollo
022200960528     D DSSGN           DS
022300960528     D  SGNLNP                 1      3  0
022400960528     D  SGNLNA                 4      6  0
022500960528     D  SGNNRS                 7      8  0
022600960528     D  SGNNSC                 9     15  0
022700960829     D  SGNZNC                16     17  0
022800000120      *  DS x stampa segnacollo
022900960628     D                 DS
023000960628     D  SGN1                   1     35
023100960628     D  SGN2                  36     70
023200960628     D  SGN3                  71    105
023300000120     D  SGT                    1    105    DIM(3)
023400000120      *---------------                                                      *
023500000120     D W035A           s             35
023600000125     D Wvablnp         s                   LIKE(vablnp)
023700040227     D Wvabnrs         s                   LIKE(vabnrs)
023800040227     D Wvabctm         s                   LIKE(vabctm)
023900060612     D Wvabtic         s                   LIKE(vabtic)
024000050112     D NUM_Sped        s                   LIKE(vabnsp)
024100041214     D SvKPJBU         s                   LIKE(KPJBU)
024200070219     C*
024300070219     C* Definizione campi di work
024400070219     D pVLB            s                   LIKE(VABVLB)
024500070219     D kKUT            s                   LIKE(TBLKUT)
024600070219     D kCOD            s                   LIKE(TBLCOD)
024700070219     D kKEY            s                   LIKE(TBLKEY)
024800070219     D kKCC            s                   LIKE(ACOKCC)
024900070219     D kKSC            s                   LIKE(ACOKSC)
025000070219     D kAAA            s                   LIKE(NUFAAA)
025100070219     D kCNU            s                   LIKE(NUFCNU)
025200070219     D kFIL            s                   LIKE(NUFFIL)
025300070219     D kAAS            s                   LIKE(VABAAS)
025400070219     D kLNP            s                   LIKE(VABLNP)
025500070219     D kNRS            s                   LIKE(VABNRS)
025600070219     D kNSP            s                   LIKE(VABNSP)
025700070219     D kCMR            s                   LIKE(VABCMR)
025800070219     D kCCM            s                   LIKE(VABCCM)
025900070219     D kNCD            s                   LIKE(VADNCD)
026000070219     D kCNT            s                   LIKE(VADCNT)
026100070219     D kNMC            s                   LIKE(RDENMC)
026200070219     D kNRP            s                   LIKE(RDENRP)
026300070219     D kLNP1           s                   LIKE(RDELNP)
026400070219     D WCODPT          s                   LIKE(TA0004)
026500070219     D WQUAPT          s                   LIKE(TA0007)
026600080716     D trovato_TPI     s              1
026700070219      *
026800070219      *  Invio E-mail
026900070219     D Indirizzi       s            253
027000070219     D Oggetto         s             44
027100070219     D Messaggio       s           5000
027200070219      *
027300070219     d   DSmail        ds
027400070219     D Mail_Ind                      44
027500070219     d   IND_char                     1    dim(44)
027600070219      *
027700070219     D Lun_Ind         s              5u 0
027800070219      *
027900070219     D Dac             s              5u 0
028000070219     D Luc             s              5u 0
028100070219      *
028200040227     C*---------------------------------------------------------------*
028300040227     D MsgDSP          S            256                                         Testo generico
028400040227     C*---------------------------------------------------------------*
028500040227     D SndMg01         S             10                                         Message type
028600040227     D                                     INZ('*INFO')
028700040227     D SndMg02         S             10                                         Deluvery mode
028800040227     D                                     INZ('*BREAK')
028900040227     D SndMg03         S            256                                         Message text
029000040227     D SndMg04         S             10I 0                                      Length of text
029100040227     D                                     INZ(%SIZE(SndMg03))
029200040227     D SndMg05         S             10                                         User profile
029300040227     D                                     DIM(299)
029400040227     D SndMg06         S             10I 0                                      Number of user
029500040227     D SndMg07         S             10I 0                                      Message sent indic.
029600040227     D SndMg08         S             10I 0                                      Function requested
029700040227     D SndMg10         S              1                                         Show display
029800040227     D                                     INZ('N')
029900040227     D SndMg11         S             20                                         Qualified MSGQ name
030000040227     D SndMg12         S              4                                         Name type indicator
030100040227     D                                     INZ('*USR')
030200040227      * indice di scihera
030300040227     D Jx              S              5I 0
030400040227      *
030500040227     D/COPY QSYSINC/QRPGLESRC,QUSEC
030600070219      *
030700070219     d bart_it         C                   CONST('@Bartolini.it;')
030800070219     d CED_Bart        C                   CONST('CED@Bartolini.it;')
030900000120      *---------------------------------------------------------------------*
031000960711     IRRIFCSUM
031100960524     I              RRIFCSUM                    SUMDAT
031200000120      *---------------------------------------------------------------------*
031300000120      * Ciclo principale
031400000120      *---------------------------------------------------------------------*
031500061116     **
031600960329      * Posizionamento iniziale sul file
031700960329     C                   EXSR      REDFIL
031800040227      *
031900961016      * Controllo che primo record letto sia TT00
032000961016     C     SUMTPR        IFNE      'TT00'
032100961212     C     WFINE         ANDNE     'S'                                          - Fine file -
032200961016     C                   EXCEPT    TESTA
032300961016     C                   EXCEPT    AVVCED
032400040416     c                   move      'S'           errori_prtf
032500040227      *
032600040227      * Msg di allerta Traduzione da Flat file non finita bene
032700040227      *
032800040227     c                   clear                   msgDSP
032900070220     C                   EVAL      msgDSP = 'ATTENZIONE TRTC80R: traduzione EDI-
033000070220     C                              IFCSUM Bolle import in filiale messaggio -
033100070220     C                             incompleto. Verificare la ricezione sul serv-
033200070220     C                             er EDI e rieseguire la traduzione'
033300040227     c                   exsr      SEND_MSG_EDP
033400070219      *
033500070219      * Msg di allerta Traduzione
033600070219     C                   EVAL      Indirizzi = CED_BART
033700070219     C                   EVAL      Oggetto ='Problemi ricez. EDI Bolle import -
033800070219     C                             Vers.D96A'
033900070219     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
034000070219     C                             IFCSUM Bolle import in filiale - messaggio -
034100070219     C                             incompleto. Verificare la ricezione sul serv-
034200070219     C                             er EDI e rieseguire la traduzione'
034300070219     c                   exsr      invio_MAIL
034400961016     C                   GOTO      FINE
034500961016     C                   END
034600980609      *
034700960329      * Lettura fino a fine file
034800960329     C     WFINE         DOWNE     'S'                                          - Fine file -
034900070219      *
035000960527      * Pulisco campi di importazione dati
035100960527     C                   EXSR      AZZCMP
035200980609      *
035300980609      * Controllo tipo record letto per decodifica solo se trovata la tabella PT
035400980609     C                   SELECT
035500980609      *
035600980609     C     SUMTPR        WHENEQ    'TT00'
035700980609     C                   EXSR      DRETT0
035800980609      *
035900980609     C     SUMTPR        WHENEQ    'TT05'
036000040227     C     PT_ok         ANDEQ     'S'
036100980609     C                   EXSR      DRET05
036200980609      *
036300980609     C     SUMTPR        WHENEQ    'TT10'
036400040227     C     PT_ok         ANDEQ     'S'
036500980609     C                   EXSR      DRET10
036600980609      *
036700980609     C     SUMTPR        WHENEQ    'TT15'
036800040227     C     PT_ok         ANDEQ     'S'
036900980609     C                   EXSR      DRET15
037000980609      *
037100980609     C     SUMTPR        WHENEQ    'TT20'
037200040227     C     PT_ok         ANDEQ     'S'
037300980609     C                   EXSR      DRET20
037400980609      *
037500980609     C     SUMTPR        WHENEQ    'TT25'
037600040227     C     PT_ok         ANDEQ     'S'
037700980609     C                   EXSR      DRET25
037800980609      *
037900980609     C     SUMTPR        WHENEQ    'TT30'
038000040227     C     PT_ok         ANDEQ     'S'
038100980609     C                   EXSR      DRET30
038200980609      *
038300980609     C     SUMTPR        WHENEQ    'TD00'
038400040227     C     PT_ok         ANDEQ     'S'
038500980609     C                   EXSR      DRED00
038600980609      *
038700980609     C     SUMTPR        WHENEQ    'TD05'
038800040227     C     PT_ok         ANDEQ     'S'
038900980609     C                   EXSR      DRED05
039000980609      *
039100980609     C     SUMTPR        WHENEQ    'TD10'
039200040227     C     PT_ok         ANDEQ     'S'
039300980609     C                   EXSR      DRED10
039400980609      *
039500980609     C     SUMTPR        WHENEQ    'TD15'
039600040227     C     PT_ok         ANDEQ     'S'
039700980609     C                   EXSR      DRED15
039800980609      *
039900980609     C     SUMTPR        WHENEQ    'TD20'
040000040227     C     PT_ok         ANDEQ     'S'
040100980609     C                   EXSR      DRED20
040200980609      *
040300980609     C     SUMTPR        WHENEQ    'TD25'
040400040227     C     PT_ok         ANDEQ     'S'
040500980609     C                   EXSR      DRED25
040600980609      *
040700980609     C     SUMTPR        WHENEQ    'TD30'
040800040227     C     PT_ok         ANDEQ     'S'
040900980609     C                   EXSR      DRED30
041000980609      *
041100980609     C                   ENDSL
041200980609      *
041300980609      * Cancello record, tranne per tabella PT non trovata
041400040227     C     PT_ok         IFeq      'S'
041500960524     C                   DELETE    EDIFCSUM
041600980609     C                   END
041700980609      *
041800960329      * Lettura successiva
041900960329     C                   EXSR      REDFIL
042000960329     C                   END                                                    - Fine file -
042100980609      *
042200980609      * Se tabella PT non trovata vado a fine
042300040227     C     PT_ok         CABEQ     'N'           FINE
042400070219     C*
042500070219     C* se abilitato
042600070219     C     WRTVAB        IFEQ      'S'
042700070219      * ?- Scrivo Ultima bolla     - *
042800070219      * ?- e aggiorna i Files      ------------ *
042900070219     c                   exsr      Aggiorna_DB
043000070219      * ?            * ------------------------ *
043100070219     c                   end
043200040227      *
043300960924     C*  Se non è la prima volta
043400960925     C     WPRIMA        IFEQ      'N'
043500070219      *
043600070219     C*  ULTIMA CLOSE SYSPRT
043700070219      *   a Fine chiude e basta
043800070219     C                   CLOSE     SYSPRT
043900070219     C*
044000070219      * ?- Passa i records da EDIVA  a FIVAB - *
044100070219      * ?- e aggiorna i Files      ------------ *
044200070219     c                   exsr      Write_FIVAB
044300070219      * ?            * ------------------------ *
044400070219      *
044500070219     C                   END
044600070219      *
044700960703     C*  Controllo quadratura:
044800960703     C     WTOTCO        IFNE      WTOCOC
044900960703     C     WTOTCO        ANDNE     0
045000960703     C     WTOTSP        ORNE      WTOSPC
045100960703     C     WTOTSP        ANDNE     0
045200960926     C                   Z-ADD     WTOTCO        WCOL              6 0
045300960926     C                   Z-ADD     WTOCOC        WCOC              6 0
045400960926     C                   Z-ADD     WTOTSP        WSPM              6 0
045500960926     C                   Z-ADD     WTOSPC        WSPC              6 0
045600960926     C     §PTCMR        IFEQ      'S'
045700960926     C                   MOVEL     WNRCMR        WNRDOC           35
045800960926     C                   ELSE
045900960926     C                   MOVEL     WNUMFV        WNRDOC           35
046000960926     C                   END
046100960926     C   OF              EXCEPT    TESTA
046200960926     C                   EXCEPT    ERRQUA
046300040416     c                   move      'S'           errori_prtf
046400960703     C                   END
046500070219      *
046600960531     C*  Stampa riga finale
046700960731     C     FINE          TAG
046800040416     c                   if        errori_prtf = 'S'
046900960531     C   OF              EXCEPT    TESTA
047000960527     C                   EXCEPT    STAFIN
047100040416     c                   end
047200070219      *
047300980114     C* Imposto i dati in LV55
047400000120     C                   CLEAR                   FNLV55DS
047500980114     C                   MOVEL     'L'           D55TLA
047600980114     C                   CALL      'FNLV55R'
047700000120     C                   PARM                    FNLV55DS
047800070219      *
047900960329     C                   SETON                                        LR
048000960524     C*----------------------------------------------------------------
048100960524     C*? REDFIL - LETTURA FILE
048200960524     C*----------------------------------------------------------------
048300960524     C     REDFIL        BEGSR
048400960524     C*
048500960524     C* Lettura file
048600960528     C                   READ      EDIFCSUM                               30
048700960527     C                   MOVEL     SUMDAT        WDAT
048800960524     C     *IN30         IFEQ      '1'
048900960524     C                   MOVEL     'S'           WFINE
049000960524     C                   END
049100960524     C*
049200960524     C                   ENDSR
049300070219     C*----------------------------------------------------------------
049400070219     C*? Aggiorna i files
049500070219     C*----------------------------------------------------------------
049600070219     C     Aggiorna_DB   BEGSR
049700070219     C*
049800070219      * ?- Scrivo bolla precedente - *
049900070219      *
050000070219     C*  Stampo dati colli
050100070219     C     XW            IFLT      3                                            --- 02 ---
050200070219     C     XW            ANDGT     0                                            --- 02 ---
050300070219     c                   If        In_Stampa ='S'
050400070219     C                   EXCEPT    DATSGN
050500070219     c                   end
050600070219     C                   Z-ADD     0             XW
050700070219     C                   CLEAR                   SGT
050800070219     C                   END                                                    --- 02 ---
050900070219     c                   If        In_Stampa ='S'
051000070219     C   OA              EXCEPT    TEST1
051100070219     C                   EXCEPT    DATCOL
051200070219     c                   end
051300070219      *
051400070219     C*  Prima di passare ad esaminare il dettaglio succ. scrivo dati
051500070220     C*  VAB/VAD/VAT in sospeso
051600070219     C                   EXSR      UPDVAB
051700070220      *
051800070220      * ?- Fine bolla precedente -*
051900070220      * ?- e azzera i dati Bolla Successiva -
052000070219     C                   EXSR      AZZBOL
052100070219     C*
052200070219     C                   ENDSR
052300070219     C*----------------------------------------------------------------
052400070219     C*? Scrive i dati da EDIVAB a FIVAB
052500070219     C*----------------------------------------------------------------
052600070219     C     Write_FIVAB   BEGSR
052700070219     C*
052800070219     C*  Richiamo pgm per scrittura immediata VAB
052900070219     C     §PUWRK        IFEQ      ' '
053000070219      *
053100070219     C*  Richiamo pgm per esecuzione stampa
053200070219     C     §PTCMR        IFEQ      'S'
053300070219     C                   MOVEL     WNRCMR        D86CMR
053400070219     C                   ELSE
053500070219     C                   MOVEL     WNUMFV        D86CMR
053600070219     C                   END
053700070219      *
053800070219     C                   MOVEL     ' '           D86RIE
053900070219     C                   MOVEL     §PUDTA        D86DTA
054000070219     C                   Z-ADD     WPROG         D86CNT
054100070219     C                   Z-ADD     §PTKSC        D86CCM
054200070219     C                   MOVEL     *BLANKS       D86STA
054300070219     C                   MOVEL     'E'           D86MOD
054400070219      * deve essere chiuso in LR altrimenti l'*INZSR non viene rieseguita
054500070219      * dove poi imposta la DS dei parametri passati
054600070219     C                   MOVEL     'LR'          D86CHI
054700070219     C                   MOVEL     'N'           D86CTL
054800070219      *
054900070219     c                   move      kpjbu         SvKpjbu
055000070219     c                   clear                   kpjbu
055100070219     C                   MOVEL     TRTC86DS      KPJBU
055200070219      * ?- Chiamata la TRTC86R2 -*
055300070219     C                   CALL      'TRTC86R2'
055400070219     C                   PARM                    KPJBA
055500070219     c                   move      Svkpjbu       Kpjbu
055600070219      *
055700070219     C*  Controllo pgm per scrittura immediata VAB
055800070219     C     WW            IFNE      0
055900070219     C                   DO        WW            WX                3 0
056000070219     C                   Z-ADD     KCL(WX)       D86CCM
056100070219      *
056200070219     c                   move      kpjbu         SvKpjbu
056300070219     c                   clear                   kpjbu
056400070219     C                   MOVEL     TRTC86DS      KPJBU
056500070219      * ?- Chiamata la TRTC86R2 -*
056600070219     C                   CALL      'TRTC86R2'
056700070219     C                   PARM                    KPJBA
056800070219     c                   move      Svkpjbu       Kpjbu
056900070219     C                   END
057000070219     C                   END
057100070219      *
057200070219     C                   END
057300070219     C*
057400070219     C                   ENDSR
057500960524     C*----------------------------------------------------------------
057600960524     C*? DRETT0 - DECODIFICA TIPO RECORD TTT0
057700960524     C*----------------------------------------------------------------
057800960524     C     DRETT0        BEGSR
057900960524     C*
058000000120     C                   MOVEL     SUMDAT        EDTT00DS
058100960726     C*  Scrivo ex bolla
058200960726     C     WRTVAB        IFEQ      'S'
058300070219      * ?- Scrivo bolla precedente - *
058400070219      * ?- e aggiorna i Files      ------------ *
058500070219     c                   exsr      Aggiorna_DB
058600070219      * ?            * ------------------------ *
058700070219      *
058800960726     C                   MOVEL     'N'           WRTVAB            1
058900960726     C                   END
059000960924     C*  Se non è la prima volta
059100960925     C     WPRIMA        IFEQ      'N'
059200070219      *
059300960926     C     WCODPT        IFNE      TA0004
059400961021     C     WQUAPT        ORNE      TA0007
059500960927     C                   CLOSE     SYSPRT
059600960927     C                   OPEN      SYSPRT
059700960926     C                   END
059800070219     C*
059900070219      * ?- Passa i records da EDIVA  a FIVAB - *
060000070219      * ?- e aggiorna i Files      ------------ *
060100070219     c                   exsr      Write_FIVAB
060200070219      * ?            * ------------------------ *
060300070219      *
060400960924     C*  Se è la prima volta modifico variabile
060500960924     C                   ELSE
060600960924     C                   MOVEL     'N'           WPRIMA            1
060700960924     C                   END
060800070219      *
060900960628     C*  Inizio messaggio: azzero dati
061000960628     C                   EXSR      AZZMSG
061100070219      *
061200000120      *  Se è stato modificato il codice partner controllo se è tabellato
061300000120     C                   eval      *in04 = *on
061400040301      *
061500000120     C                   IF        WCODPT <> TA0004  or  WQUAPT <> TA0007
061600960528     C                   MOVEL     TA0004        WCODPT
061700961021     C                   MOVEL     TA0007        WQUAPT
061800000120      * Primo tentativo rep. da codice + qualificatore
061900961021     C                   MOVEL     TA0004        WCOD             35
062000961021     C                   MOVE      TA0007        WCOD
062100000224     C                   Z-ADD     1             XX                3 0
062200961021     C     WCOD          LOOKUP    CPT(XX)                                32
062300040301      *
062400040311     C                   If        *IN32 = *off
062500000224     C                   eval      XX = 1
062600070219     c                   clear                   w035a
062700000120     C                   eval      W035A = %subst(TA0004:1:31)
062800000224     C     W035A         lookup    CPTparz(XX)                            32
062900040301      *
063000040301      *  se con il codice parziale è stato trovato un codice Partner occorre tramite
063100040301      *  routine esterna reperire la nazione del primo mittente di dettaglio valido
063200040301      *  per cercare di individuare con certezza il codice della tabella PT in base
063300070219      *  all'UNB specifico. (es.: A02YR    ES/PT)
063400040301     C                   If        *IN32 = *on
063500070219      *
063600070219      * ?Serve per eseguire le routines che dettaglio x dettaglio vanno
063700070219      * ?a decodificare il mittente per attribuire la giusta linea/cliente
063800070219      * ?  su Partner che hanno + linee con + nazioni.
063900070219     c                   eval      Ptn_parziale = 'S'
064000070219      *
064100070219      *  Salva il codice generico se è considerato Parziale
064200070219     c                   eval      UNB_Generico = W035A
064300070219      *
064400040301     c                   z-add     nrrec         rec_pos           9 0
064500040301     c                   move      *blank        Naz_mitt          3
064600040302     c                   move      'N'           OK_Naz            1
064700040302      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
064800040302     c                   call      'TRTC80R1'
064900040302     c                   parm                    rec_pos
065000040302     c                   parm                    Naz_mitt
065100040302     c                   parm                    Ok_Naz
065200040322     c                   parm                    User_msg
065300040310      *  Re-inizializza l'indicatore di trovato
065400040310     c                   setoff                                       32
065500040310      *
065600040302      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
065700040302      *   da decodificare come UNB.
065800040302     c                   if        Ok_Naz = 'S'
065900040302     c                   clear                   W035A
066000040302     C                   eval      W035A = %subst(TA0004:1:31) + Naz_mitt
066100040302     C                   eval      XX = 1
066200040302     C     W035a         lookup    CPT(XX)                                32
066300070219      *
066400070219      * ?- Attenzione, a questo livello posso sapere se il partner ha più nazioni
066500070219      * ?  da gestire (es.:Spagna-Portogallo ... Olanda-Belgio)
066600070219      * ?- in seguito, agganciata la tabella PU, controllo se il programma è abilitato
066700070219      * ?  a gestire tramite il dettaglio Naz.mittente la relativa tabella PT/PU/PV.
066800070219     c   32              if        Ptn_parziale = 'S'
066900070219      * ?- Salva UNB e l'indice delle schiere per riaggancarla in seguito  ------ */
067000070219     c                   movel     w035a         UNB_Parz
067100070219     C                   z-add     XX            savXX             3 0
067200070219     c                   End
067300070219      *
067400040310     c                   Endif
067500040301      *
067600040301     c                   Endif
067700040301     c                   Endif
067800980707      *
067900000120      *  OPERAZIONI PER TABELLA PARTNER TROVATA
068000000120     C     *IN32         IFEQ      '1'
068100040227     C                   move      'S'           PT_ok
068200040227      *
068300040302      *             *------------------------------*
068400000120     C                   EXSR      PARTNEROK
068500040302      *             *------------------------------*
068600000120      *
068700960524     C                   ELSE
068800040227      *
068900040227      * Non ha trovato la PT ???????
069000000120     C                   clear                   EDIDSPT
069100000120     C                   clear                   EDIDSPU
069200070219     C                   clear                   EDIDSPV
069300040227     C                   move      'N'           PT_ok             1
069400980609     C                   Z-ADD     1             WINDER            3 0
069500000120     C                   MOVEL(P)  TA0004        WVALOR           35
069600960731     C                   SETOFF                                       05
069700960524     C                   EXSR      STPERR
069800980609     C**                   GOTO FINE
069900070219      *
070000070219      * Msg di allerta Traduzione
070100070219     C                   EVAL      Indirizzi = CED_Bart
070200070219     C                   EVAL      Oggetto ='Problemi ricez. EDI Bolle import -
070300070219     C                             Vers.D96A'
070400070219     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
070500070219     C                             IFCSUM Bolle import in filiale - messaggio -
070600070219     C                             con UNB non presente in tabella PT controlla-
070700070219     C                             re EDI ricevuto.'
070800070219     c                   exsr      invio_mail
070900980609     C                   GOTO      ETT00
071000960524     C                   END
071100040227      *
071200960524     C                   END
071300040227      *
071400960702     C*  Stampo dati inizio messaggio
071500040607     c                   If        In_Stampa ='S'
071600960702     C   OA              EXCEPT    TEST1
071700040607     c                   end
071800960725     C                   MOVEL     TA0017        WAA4              4 0
071900960725     C                   MOVE      WAA4          WAA2              2 0
072000960712     C                   MOVE      TA0017        WGG               2 0
072100960924     C                   MOVE      TA0017        WDTPRE            8 0
072200960924     C                   MOVE      TA0019        WHMPRE            4 0
072300960924     C                   MOVE      TA0017        WDTMSG            6 0
072400960712     C                   MOVE      WAA2          WDTMSG
072500960712     C                   MOVEL     WGG           WDTMSG
072600040607     c                   If        In_Stampa ='S'
072700960702     C                   EXCEPT    DATMSG
072800040607     c                   end
072900980707      *
073000010503      * Pulizia flag di decodifica in testata se arrivato RFF+FF in testata
073100010503     C                   clear                   WFFT             35
073200010503      *
073300980609     C     ETT00         ENDSR
073400000120     C*----------------------------------------------------------------
073500000120      *? OPERAZIONI PER TABELLA PARTNER TROVATA
073600000120     C*----------------------------------------------------------------
073700000125     C     PARTNEROK     BEGSR
073800000120      *
073900000120     C                   MOVEL     DPT(XX)       EDIDSPT
074000000120     C                   MOVEL     DPU(XX)       EDIDSPU
074100070219     C                   MOVEL     DPV(XX)       EDIDSPV
074200050301      *
074300070219      * ?Indirizzi Mail particolari per comunicazioni sulla traduzione dei dati
074400070219      *  ricevuti:
074500070219     c                   movel     §PVema        mail_ind         44
074600070219      *
074700070219      * ?imposta gli indirizzi mail se interni a Bartolini o esterni
074800070219     c                   if        mail_ind <> *blank
074900070219     c                   exsr      imp_ADDR_mail
075000070219     c                   end
075100070219      *
075200050301      * campo da gestire come numerico (lunghezza max. Barcode) x diskC se
075300050301      * ricevuto un PCI + lungo di quello che dobbiamo riportare su FIVAT
075400050301      *  il campo è stato aggiunto alfanumerico su tabella quindi >Blank<
075500050301     C                   if        §PULUN = *blank
075600050301     C                   eval      §PULUN = '00'
075700050301     c                   end
075800050301      * Lunghezza max segnacollo da prendere su VAT solo se diskC e se > 0
075900050301     C                   move      §puLUN        WVATlun           2 0          *0/>0
076000050301      *
076100020221     C                   Z-ADD     §PTKSC        WKSC              7 0
076200000125     C                   MOVEL     §PTLNP        WVABlnp
076300040227     C                   MOVEL     §PTNRS        WVABnrs
076400040227     C                   MOVEL     §PTCTM        WVABctm
076500050223      *
076600050223      *  Per gestire legame tramite Nr.Sped.passato da EDI e non reperito da
076700050223      *  numeratore VAB. (Questo perchè se viene trasmesso l'EDI e un file
076800050223      *  di carattere BARTOLINI come FIVAX00F serve per poter mantenere legati
076900050223      *  i records trasmessi).
077000050223     C                   MOVEL     §puNSP        WVABfnsp          1            *blk/S
077100000125      *
077200000125      *  In base al cod.trattamento merce reperisco tipo tratt.
077300000120     C                   CLEAR                   DS1B
077400000120     C                   Z-ADD     1             Y                 3 0
077500040227     C     Wvabctm       LOOKUP    CTM(Y)                                 32
077600000120     C     *IN32         IFEQ      '1'
077700000120     C                   MOVEL     DTM(Y)        DS1B
077800000125     C                   MOVEL     DS1B          WSVTRM           38
077900000120     C                   END
078000000125      *
078100000125      * CLEARIZZO CAMPI DI CNACO E CNIND UTILIZZATI DAL PGM
078200000125     C                   clear                   ACORAG
078300000125     C                   clear                   INDCAE
078400000125     C                   clear                   INDCAP
078500000125     C                   clear                   INDSTA
078600000125      *
078700000125      * Decodifico partner
078800000120     C                   Z-ADD     1             KKUT
078900000120     C                   Z-ADD     KCI           KKCC
079000000120     C                   MOVE      §PTKSC        KKSC
079100000120     C     KACO          CHAIN     CNACO00F                           31
079200000120     C     KIND          CHAIN     CNIND00F                           31
079300000120      *
079400000120     C                   ENDSR
079500960524     C*----------------------------------------------------------------
079600960524     C*? DRETT5 - DECODIFICA TIPO RECORD TT05
079700960524     C*----------------------------------------------------------------
079800960524     C     DRET05        BEGSR
079900960524     C*
080000000120     C                   MOVEL     SUMDAT        EDTT05DS
080100960628     C*  Stampo dati note intero messaggio
080200040607     c                   If        In_Stampa ='S'
080300960628     C   OA              EXCEPT    TEST1
080400960628     C                   EXCEPT    NOTMSG
080500040607     c                   end
080600960628     C                   DO        5             X
080700960702     C     T05(X)        IFNE      *BLANKS
080800960628     C                   MOVEL     T05(X)        WNOT
080900040607     c                   If        In_Stampa ='S'
081000960628     C                   EXCEPT    NOTMS1
081100040607     c                   end
081200960628     C                   END
081300960628     C                   END
081400960524     C*
081500960524     C                   ENDSR
081600960524     C*----------------------------------------------------------------
081700960524     C*? DRET10 - DECODIFICA TIPO RECORD TT10
081800960524     C*----------------------------------------------------------------
081900960524     C     DRET10        BEGSR
082000960524     C*
082100000120     C                   MOVEL     SUMDAT        EDTT10DS
082200960627     C*  Memorizzo totali x quadratura
082300960627     C                   DO        2             X
082400960702     C                   MOVEL     T10(X)        WTT10
082500960627     C     WCTR          IFEQ      '10 '
082600960627     C                   Z-ADD     WVLU          WTOTSP           16 0          Tot.sped.
082700960627     C                   END
082800960627     C     WCTR          IFEQ      '11 '
082900960627     C                   Z-ADD     WVLU          WTOTCO           16 0          Tot. colli
083000960627     C                   END
083100960627     C     WCTR          IFEQ      '7  '
083200960627     C                   Z-ADD     WVLU          WTOTPL           16 0
083300960627     C                   END
083400960702     C     WCTR          IFEQ      'ZCW'
083500960627     C                   Z-ADD     WVLU          WTOTPN           16 0
083600960627     C                   END
083700960627     C                   END
083800960524     C*
083900960524     C                   ENDSR
084000960524     C*----------------------------------------------------------------
084100960524     C*? DRET15 - DECODIFICA TIPO RECORD TT15
084200960524     C*----------------------------------------------------------------
084300960524     C     DRET15        BEGSR
084400960628     C*
084500000120     C                   MOVEL     SUMDAT        EDTT15DS
084600960624     C*  Se tipo documento CMR ('CMR') memorizzo numero e data
084700960624     C     TD1153        IFEQ      'CMR'
084800960624     C                   MOVEL     TD1154        WNRCMR           35
084900960524     C     TD2005        IFEQ      '137'
085000960528     C                   MOVEL     TD2380        WDATA            35            Data
085100960712     C                   MOVEL     TD2379        WFORM             3            Formato
085200960524     C                   EXSR      CNVDAT
085300960624     C                   MOVEL     WCAMG         WDTCMR            8 0          Data CMR
085400960628     C                   MOVEL     WHMS          WHHCMR            4 0          Ora  CMR
085500960628     C     WERRO         IFEQ      'S'
085600960628     C                   Z-ADD     2             WINDER
085700960628     C                   MOVEL     *BLANKS       WVALOR
085800960628     C                   MOVEL     TD2380        WVALOR
085900960731     C                   SETOFF                                       05
086000960628     C                   EXSR      STPERR
086100960628     C                   END
086200960628     C                   END
086300960628     C*  Stampo dati CMR
086400960712     C                   MOVE      WDTCMR        G02INV
086500960712     C                   MOVEL     '3'           G02ERR
086600960712     C                   CALL      'XSRDA8'
086700960712     C                   PARM                    WLBDA8
086800960712     C                   Z-ADD     G02DAT        WSTCMR            8 0
086900040607     c                   If        In_Stampa ='S'
087000960628     C   OA              EXCEPT    TEST1
087100960628     C                   EXCEPT    CMRDOC
087200040607     c                   end
087300960524     C                   END
087400960628     C*
087500960628     C*  Se tipo documento AFB (Foglio viaggio) stampo numero e data
087600960628     C     TD1153        IFEQ      'AFB'
087700960628     C                   MOVEL     TD1154        WNUMFV           35
087800960628     C     TD2005        IFEQ      '137'
087900960628     C                   MOVEL     TD2380        WDATA            35            Data
088000960712     C                   MOVEL     TD2379        WFORM             3            Formato
088100960628     C                   EXSR      CNVDAT
088200960702     C                   MOVEL     WCAMG         WDATFV            8 0          Data CMR
088300960628     C                   MOVEL     WHMS          WHHNFV            4 0          Ora  CMR
088400960628     C     WERRO         IFEQ      'S'
088500960702     C                   Z-ADD     21            WINDER
088600960628     C                   MOVEL     *BLANKS       WVALOR
088700960628     C                   MOVEL     TD2380        WVALOR
088800960731     C                   SETOFF                                       05
088900960628     C                   EXSR      STPERR
089000960628     C                   END
089100960628     C                   END
089200960628     C*  Stampo dati AFB
089300960712     C                   MOVE      WDATFV        G02INV
089400960712     C                   MOVEL     '3'           G02ERR
089500960712     C                   CALL      'XSRDA8'
089600960712     C                   PARM                    WLBDA8
089700960712     C                   Z-ADD     G02DAT        WSTDFV            8 0
089800040607     c                   If        In_Stampa ='S'
089900960628     C   OA              EXCEPT    TEST1
090000960702     C                   EXCEPT    FVVDOC
090100040607     c                   end
090200960628     C                   END
090300980911      *
090400980911      *  Se tipo documento FF memorizzo il codice cliente
090500010503      *******************************************
090600000125      *  In questa fase memorizzo i campi necessari al controllo che ho
090700000125      *  spostato nella sbr  DRED15 dopo aver riagganciato la PT
090800000125      **********************************************************************
090900090630      **  GEODIS CALBERSON per mandare il riferimento cliente usa RFF+ADE al posto di RFF+FF
091000090630     C                   If        (TD1153 = 'FF '  and  TD1154 <> *blanks)  or
091100090630     C                             (TD1153 = 'ADE'  and  TD1154 <> *blanks)
091200010502     C                   MOVEL     TD1154        WFFT             35
091300000125     C                   Endif
091400010503      *
091500010503     C*----------------------------------------------------------------
091600010503      *  La routine finiva qui ma per E.Arden che tratta il segmento
091700010503      *  NAD+DP nel TD15 senza avere il NAD+CZ occorre decodificare in
091800010503      *  anticipo il codice particolare del Cliente (CL)
091900040303     C*----------------------------------------------------------------
092000040303      *  Non in anticipo ma nel momento giusto durante la lettura del dato
092100010503     C*----------------------------------------------------------------
092200040302      *  Con l'introduzione del pgm TRTC80R1 il Partner verrà sempre decodificato
092300040302      *  anche quando è dubbio come NLLYN  NL/BE quindi un RFF+FF a livello di testata
092400040302      *  è giusto che sia decodificato in questo momento.
092500040302      *  ma......
092600040302      *  ATTENZIONE se il partner è DUBBIO come NLLYN con più Linee di PARTENZA e
092700040302      *  NON MANDA il NAD+CZ (Nazione Mittente) poichè il gioco non funziona più.
092800040302     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
092900010503     C                   IF        WFFT <> *BLANKS
093000010503     C                   z-add     1             XX
093100010503     C                   movel     §PTKSC        WCCL             35
093200010503     C                   movel     WFFT          WCOM28           28
093300010503     C                   move      WCOM28        WCCL
093400010503     C     WCCL          LOOKUP    CCL(XX)                                34
093500070219      *
093600070219      * ?Se il Partner è uno di quelli che gestisce + linee quindi ha un UNB parziale
093700070219      * ?potrebbe non agganciare correttamente il cliente quindi occorre decodificare
093800070219      * ?il dato cercandolo anche con gli altri codici KSC del Partner Generico ossia
093900070219      * ?con i primi 31 caratteri dell'UNB.
094000070219     C  n34              if        PTN_parziale = 'S' and §puPLN = 'S'
094100070219     c                   do        sav_CPTx      x
094200070219      *
094300070219     c                   if        CPTparz(x) = UNB_generico
094400070219     C                   movel     KSC_UNB(x)    WCCL             35
094500070219     C                   movel     WFFT          WCOM28           28
094600070219     C                   move      WCOM28        WCCL
094700070219     C                   z-add     1             XX
094800070219     C     WCCL          LOOKUP    CCL(XX)                                34
094900070219      * se trovato esce immediatamente
095000070219     c   34              leave
095100070219     c                   end
095200070219      *
095300070219     C                   endDO
095400070219     c                   endIF
095500070219      *   Se trovato vale x tutto il messaggio
095600010503      *
095700010503     C     *IN34         IFEQ      '1'
095800010503     C                   MOVEL     DCL(XX)       EDIDSCL
095900040311     C                   z-add     §CLKSC        $CLKSC
096000040311     C                   movel     §CLTAR        $CLTAR
096100040311     C                   z-add     §CLLNP        $CLLNP
096200040311     C                   z-add     §CLNRS        $CLNRS
096300040311     C                   move      §CLCTM        $CLCTM
096400040311     C                   move      §CLBS1        $CLBS1
096500050223     C                   move      §CLnsp        $CLfnsp                        *blk/S
096600040311      * LNP NRS e CTM da FF
096700040311     c                   eval      wvablnp = §CLlnp
096800040311     c                   eval      wvabnrs = §CLnrs
096900040311     c                   eval      wvabctm = §CLctm
097000050223     c                   eval      WVABfnsp = §CLnsp                            *blk/S
097100070219      *
097200040311      *  In base al cod.trattamento merce reperisco tipo tratt.
097300040311     C                   CLEAR                   DS1B
097400040311     C                   Z-ADD     1             Y                 3 0
097500040311     C     Wvabctm       LOOKUP    CTM(Y)                                 32
097600040311     C     *IN32         IFEQ      '1'
097700040311     C                   MOVEL     DTM(Y)        DS1B
097800040311     C                   MOVEL     DS1B          WSVTRM           38
097900040311     C                   END
098000040311      *
098100010503     C     §CLKSC        LOOKUP    KCL                                    39
098200040311     c                   If        *in39 = *off
098300010503     C                   add       1             WW
098400010503     C                   z-add     §CLKSC        KCL(WW)
098500010503     C                   Endif
098600040311      *
098700010503     C                   ENDIF
098800010503      *
098900010503     C                   ENDIF
099000010503     C*--------------------------
099100980911      *
099200960524     C                   ENDSR
099300960524     C*----------------------------------------------------------------
099400960524     C*? DRET20 - DECODIFICA TIPO RECORD TT20
099500960524     C*----------------------------------------------------------------
099600960524     C     DRET20        BEGSR
099700960524     C*
099800000120     C                   MOVEL     SUMDAT        EDTT20DS
099900960628     C*  Stampo dati trasporto
100000040607     c                   If        In_Stampa ='S'
100100960628     C   OA              EXCEPT    TEST1
100200960628     C                   EXCEPT    DATTRA
100300040607     c                   end
100400960628     C*  Controllo se ci sono loc.partenza/arrivo
100500960628     C                   Z-ADD     1             X
100600960628     C                   DO        3             X
100700960712     C     T20(X)        IFNE      *BLANKS
100800960702     C                   MOVEL     T20(X)        WTT20
100900960628     C     WTPL          IFEQ      '5  '
101000960628     C                   MOVEL     WLOC          WLOCPA
101100960628     C                   END
101200960628     C     WTPL          IFEQ      '8  '
101300960628     C                   MOVEL     WLOC          WLOCAR
101400960628     C                   END
101500960628     C                   END
101600960712     C                   END
101700960628     C*  Data partenza
101800960628     C                   Z-ADD     1             X
101900960628     C                   DO        3             X
102000960712     C     T20D(X)       IFNE      *BLANKS
102100960702     C                   MOVEL     T20D(X)       WTT20D
102200960628     C     WTPD          IFEQ      '136'
102300960628     C                   MOVEL     WDTA          WDATA            35            Data
102400960628     C                   MOVEL     WFOR          WFORM             3            Formato
102500960628     C                   EXSR      CNVDAT
102600960628     C                   MOVEL     WCAMG         WDATPA            8 0          Data Part.
102700960913     C                   Z-ADD     WDATPA        G02INV
102800960913     C                   MOVEL     '3'           G02ERR
102900960913     C                   CALL      'XSRDA8'
103000960913     C                   PARM                    WLBDA8
103100960913     C                   Z-ADD     G02DAT        WSTDPA            8 0
103200960628     C                   MOVEL     WHMS          WHHPA             4 0          Ora  Part.
103300960628     C     WERRO         IFEQ      'S'
103400960702     C                   Z-ADD     9             WINDER
103500960628     C                   MOVEL     *BLANKS       WVALOR
103600960628     C                   MOVEL     TD2380        WVALOR
103700960731     C                   SETOFF                                       05
103800960628     C                   EXSR      STPERR
103900960628     C                   END
104000960628     C                   END
104100960712     C                   END
104200960524     C*
104300960628     C                   END
104400960628     C*  Stampo dati luogo partenza/arrivo
104500960702     C     WTT20         IFNE      *BLANKS
104600960702     C     WTT20D        ORNE      *BLANKS
104700040607     c                   If        In_Stampa ='S'
104800960628     C   OA              EXCEPT    TEST1
104900960628     C                   EXCEPT    DATLOC
105000040607     c                   end
105100960628     C                   END
105200960628     C*
105300960524     C                   ENDSR
105400960524     C*----------------------------------------------------------------
105500960524     C*? DRET25 - DECODIFICA TIPO RECORD TT25
105600960524     C*----------------------------------------------------------------
105700960524     C     DRET25        BEGSR
105800960524     C*
105900960926     C*  Reperisco ultimo numero progrressivo CMR x cliente/nr.cmr
106000960926     C                   Z-ADD     §PTKSC        KCCM
106100960926     C     §PTCMR        IFEQ      'S'
106200960926     C                   MOVEL     WNRCMR        KCMR
106300960926     C                   ELSE
106400960926     C                   MOVEL     WNUMFV        KCMR
106500960926     C                   END
106600960926     C                   Z-ADD     1             WPROG             5 0
106700960927     C                   MOVEL     SAVBOL        SALVA
106800020909     C     KVAB2         SETGT     EDiVAB1L
106900020909     C     KVAB2         READPE    EDiVAB1L                               30
107000960926     C     *IN30         IFEQ      '0'
107100960926     C     VABCNT        ADD       1             WPROG
107200960926     C                   END
107300960926     C*
107400960927     C                   MOVEL     SALVA         SAVBOL
107500960628     C* Se tot. spedizioni > 0  stampo dati (rec.TT25 obblig.)
107600960628     C     WTOTSP        IFNE      0
107700960628     C     WTOTCO        ORNE      0
107800960628     C     WTOTPL        ORNE      0
107900960628     C     WTOTPN        ORNE      0
108000960712     C     WSTPTO        IFEQ      'N'
108100040607     c                   If        In_Stampa ='S'
108200960628     C   OA              EXCEPT    TEST1
108300960628     C                   EXCEPT    TOTDOC
108400040607     c                   end
108500960712     C                   MOVEL     'S'           WSTPTO
108600960628     C                   END
108700960712     C                   END
108800070219      *
108900960628     C*  Imposto dati TT25
109000000120     C                   MOVEL     SUMDAT        EDTT25DS
109100960628     C*  Stampa
109200040607     c                   If        In_Stampa ='S'
109300960628     C   OA              EXCEPT    TEST1
109400960628     C     TF3035        IFEQ      'SF'
109500960628     C                   EXCEPT    DATPAM
109600960628     C                   END
109700960628     C     TF3035        IFEQ      'ST'
109800960628     C                   EXCEPT    DATPAD
109900960628     C                   END
110000040607     c                   end
110100960524     C*
110200960524     C                   ENDSR
110300960524     C*----------------------------------------------------------------
110400960524     C*? DRET30 - DECODIFICA TIPO RECORD TT30
110500960524     C*----------------------------------------------------------------
110600960524     C     DRET30        BEGSR
110700960524     C*
110800000120     C                   MOVEL     SUMDAT        EDTT30DS
110900960822     C*  Controllo se numero pallet numerico
111000960822     C                   TESTN                   W6350                98
111100960822     C     *IN98         IFEQ      '0'
111200960822     C                   MOVEL     *ZEROS        W6350
111300960822     C                   END
111400960628     C*  Stampa
111500040607     c                   If        In_Stampa ='S'
111600960628     C   OA              EXCEPT    TEST1
111700960628     C                   EXCEPT    DATAUT
111800040607     c                   end
111900960524     C*
112000960524     C                   ENDSR
112100960527     C*----------------------------------------------------------------
112200960527     C*? DRED00 - DECODIFICA TIPO RECORD TD00
112300960527     C*----------------------------------------------------------------
112400960527     C     DRED00        BEGSR
112500960527     C*
112600960703     C                   ADD       1             WTOSPC           16 0
112700040227     C*
112800020909     C*  Scrivo vecchio record EDiVAB e azzero dati per nuovo
112900960527     C     WRTVAB        IFEQ      'S'
113000070219     C*
113100070219      * ?- Scrivo bolla precedente - *
113200070219      * ?- e aggiorna i Files      ------------ *
113300070219     c                   exsr      Aggiorna_DB
113400070219      * ?            * ------------------------ *
113500070219      ***
113600070219      * ?- Attenzione solo se abilitato in tabella il Partner ad avere + linee   -*/
113700070219      * ?- di un Partner che ha realmente più linee                              -*/
113800070219     c                   if        Ptn_parziale = 'S' and §puPLN = 'S'
113900070219     c                   z-add     savXX         XX
114000070219     C                   exsr      PARTNEROK
114100070219     c                   end
114200070219      ***
114300960527     C                   END
114400000125      *
114500040302     C** Spostato la ricerca del Terminal di partenza nel dettaglio TD25
114600070219      *
114700070219      * ?--------------->
114800070219      * ? Se stiamo trattando un Ptn con + linee ad ogni dettaglio cerco la
114900070219      * ? Nazione del mittente altrimenti prende quella di testata.
115000070219      * ?- Attenzione solo se abilitato in tabella il Partner ad avere + linee   -*/
115100070219      * ?- di un Partner che ha realmente più linee                              -*/
115200070219     c                   if        Ptn_parziale = 'S' and §puPLN = 'S'
115300070219     C                   exsr      DRED00_mitt
115400070219     c                   end
115500070219      * ?--------------->
115600000125      *
115700040302      * Abilita la scrittura
115800960527     C                   MOVEL     'S'           WRTVAB
115900040302      *
116000040302      * Imposta qui i campi presi dalla PT oppure dall'RFF+FF di testata
116100040302      *  validi su tutti i dettagli del messaggio. Potranno essere reimpostati
116200040302      *  in presenza di valori specifici RFF+FF a livello di dettaglio su TD10
116300040302      *  e decodificati in seguito con tabella CL particolare.
116400040302     C                   MOVEL     Wvabnrs       VABNRS
116500040302     C                   MOVEL     Wvablnp       VABLNP
116600040302     C                   MOVEL     Wvabctm       VABCTM
116700040302      *
116800000120     C                   MOVEL     SUMDAT        EDTD00DS
116900070219      *
117000070219      * ?Reperisco loro nr.spedizione e lo metto nel rif.mittente
117100070219     C                   clear                   WidSPEp          35
117200070220     c                   eval      WidSPEp = 'Rif.:' + %Trim(DA1004)
117300960731     C                   MOVEL     DA1004        WPRGSP           35
117400960627     C*  Reperisco loro nr.spedizione e lo metto nel rif.mittente
117500960626     C                   MOVEL     DA1004        WNSP             35
117600960702     C                   MOVEL     DA1004        WNUM             35
117700070219      * ?                                       --------
117800960627     C                   MOVEL     DA1004        VABRMA
117900070219      * ?                                       --------
118000960725     C     ' '           SCAN      DA1004        WLUNGH            3 0
118100960725     C                   SUB       1             WLUNGH
118200960627     C                   EXSR      TSTNUM
118300960627     C     WOKNUM        IFEQ      'S'                                          Ctrl numerico
118400070219      * ?                                       --------
118500960627     C                   MOVEL     WNUMOK        VABRMN
118600070219      * ?                                       --------
118700960627     C                   ELSE
118800960627     C                   Z-ADD     4             WINDER
118900960627     C                   MOVEL     *BLANKS       WVALOR
119000960627     C                   MOVEL     DA2380        WVALOR
119100960731     C                   SETON                                        05
119200960627     C                   EXSR      STPERR
119300960627     C                   END
119400070219      *
119500960527     C*  Controllo se ho memorizzato data consegna richiesta
119600960527     C     DA2005        IFEQ      'Z73'
119700960527     C                   MOVEL     *BLANKS       WDATA                          Data
119800060728     C                   MOVEL     DA2380        WDATA                          Data
119900960527     C                   MOVEL     DA2379        WFORM                          Formato
120000960527     C                   EXSR      CNVDAT
120100960527     C                   MOVEL     WCAMG         VABDCR                         Data cons.rich.
120200960527     C                   MOVEL     *BLANKS       VABTCR                         Tipo cons.rich.
120300960527     C                   MOVEL     WHMS          VABHCR                         Ora  cons.rich.
120400960527     C     WERRO         IFEQ      'N'
120500960527     C                   Z-ADD     5             WINDER
120600960527     C                   MOVEL     *BLANKS       WVALOR
120700960528     C                   MOVEL     DA2380        WVALOR
120800960731     C                   SETON                                        05
120900960527     C                   EXSR      STPERR
121000960527     C                   END
121100960527     C                   END
121200070219      ***
121300960527     C*  Reperisco priorità spedizione:
121400960527     C                   Z-ADD     1             Y
121500061127      ***
121600061127      *** attenzione se si tratta di YSL --> l'espresso è indicato con il "2"
121700061127      ***  mentre in tabella EDI EEX è fatto con "1"
121800061127      ***
121900061128     c                   movel(p)  DA4219        fld35a           35
122000061128     c                   move      §puTS         fld35a
122100061128     C     fld35a        LOOKUP    PTR35(Y)                               32
122200061128      *
122300960527     C     *IN32         IFEQ      '1'
122400960528     C                   MOVEL     TSP(Y)        VABTSP
122500960527     C                   ELSE
122600960527     C                   Z-ADD     6             WINDER
122700960527     C                   MOVEL     *BLANKS       WVALOR
122800960527     C                   MOVEL     DA4219        WVALOR
122900960731     C                   SETON                                        05
123000960527     C                   EXSR      STPERR
123100960527     C                   END
123200960527     C*  Controllo se c'è il c.assegno/imp.assicurato/valore merce
123300960527     C                   DO        3             X
123400960527     C* Controllo se campo importo numerico
123500960712     C     D00(X)        IFNE      *BLANKS
123600960527     C                   MOVEL     D00(X)        WTD00
123700960527     C                   MOVEL     D00(X)        WCOM19           19
123800960527     C                   MOVE      WCOM19        WCOM16           16
123900960527     C                   TESTN                   WCOM16               98
124000960527     C*  Se importo numerico decodifico tipo
124100960531     C     *IN98         IFEQ      '1'
124200960527     C                   EXSR      CTRVAL                                       Ctrl valuta
124300960527     C     WTPI          IFEQ      '157'
124400971117     C                   Z-ADD     WIMP          VABIAS                         IMPORTO
124500971117     C                   MOVEL     WVAL          VABVAS                         ASSICURATO
124600960527     C                   END
124700960527     C     WTPI          IFEQ      '77 '                                        Valore
124800971117     C                   Z-ADD     WIMP          VABVMD                         MERCE
124900971117     C                   MOVEL     WVAL          VABVAD                         DICHIARATO
125000960527     C                   END
125100960527     C     WTPI          IFEQ      '22 '                                        C/Assegno
125200981009     C     §PUCAS        IFEQ      'S'                                          C/Assegno
125300960527     C                   Z-ADD     WIMP          VABCAS
125400960527     C                   MOVEL     WVAL          VABVCA
125500981012      * SE PARTNER IMPOSTO 'OM' COME TIPO INCASSO
125600981012      *  forzo fuori dalla routine perchè a volte i Partner non lo impostano
125700981012     C     §PUTPC        IFEQ      'P'
125800981012     C     WIMP          ANDGT     *ZEROS
125900981012     C                   MOVEL     'OM'          VABTIC
126000060612     C                   MOVEL     'OM'          WVABTIC
126100981012     C                   ENDIF
126200981009     C                   END
126300960926     C                   END
126400960527     C*  Campo importo non numerico
126500960527     C                   ELSE
126600960527     C                   Z-ADD     12            WINDER
126700960527     C                   MOVEL     *BLANKS       WVALOR
126800960527     C                   MOVEL     WCOM16        WVALOR
126900960731     C                   SETON                                        05
127000960527     C                   EXSR      STPERR
127100960527     C                   END                                                    Imp.non numer.
127200960712     C                   END                                                    Imp.non numer.
127300960527     C*
127400960527     C                   END                                                    Do 3
127500960911     C*
127600960911     C*  Decodifico servizi speciali
127700960911     C     DA7085        IFNE      *BLANKS
127800960913     C     DA7085        ANDNE     *LOVAL
127900960911     C                   Z-ADD     1             X
128000061128     c                   movel(p)  DA7085        fld35a           35
128100061128     c                   move      §puSS         fld35a
128200061128     C     fld35a        LOOKUP    SS35(X)                                32
128300960911     C     *IN32         IFEQ      '0'
128400960911     C                   Z-ADD     24            WINDER            3 0
128500960911     C                   MOVEL     *BLANKS       WVALOR           35
128600960911     C                   MOVEL     DA7085        WVALOR
128700960912     C                   SETON                                        05
128800960911     C                   EXSR      STPERR
128900960911     C                   ELSE
129000000120     C                   MOVEL     DSS(X)        EDIDSSS
129100961129     C     §SSNOT        IFEQ      'S'
129200960911     C                   MOVEL     §SSDES        VABNOT
129300961129     C                   END
129400070219      *
129500031209      * x Espresso     "Pre-noon"
129600960911     C     §SSTPS        IFEQ      'S'
129700960911     C                   MOVEL     'E'           VABTSP
129800960911     C                   END
129900031209      * x Appuntamento "Booking ins"
130000031209     C     §SSTPS        IFEQ      'A'
130100031209     C                   MOVEL     'A'           VABTC1
130200031209     C                   END
130300960911     C                   END
130400960911     C                   END
130500960628     C*  Stampa
130600040607     c                   If        In_Stampa ='S'
130700960628     C   OA              EXCEPT    TEST1
130800960628     C                   EXCEPT    DATSPE
130900040607     c                   end
131000980701      *
131100960527     C                   ENDSR
131200070219      * ?----------------------------------------------------------------
131300070219     C*? DRED00_MITT - DECODIFICA il mittente x Partner con più linee
131400070219      * ?----------------------------------------------------------------
131500070219     C     DRED00_mitt   BEGSR
131600070219     C*
131700070219     c                   z-add     nrrec         rec_pos           9 0
131800070219     c                   move      *blank        Naz_mitt          3
131900070219     c                   move      'N'           OK_Naz            1
132000070219     C*
132100070219      *  Restituisce la Nazione con cui agganciare l'UNB su tab.:PT
132200070219     c                   call      'TRTC80R4'
132300070219     c                   parm                    rec_pos
132400070219     c                   parm                    Naz_mitt
132500070219     c                   parm                    Ok_Naz
132600070219     c                   parm                    User_msg
132700070219     c                   parm                    UNB_Parz
132800070219      *
132900070219      *  se esito ricerca è OK --> ('S') imposta la stringa completa del Mittente
133000070219      *   da decodificare come UNB.
133100070219     c                   if        Ok_Naz = 'S'
133200070219     c                   clear                   W035A
133300070219     C                   eval      W035A = %subst(UNB_Parz:1:31) + Naz_mitt
133400070219     C                   eval      XX = 1
133500070219     C     W035a         lookup    CPT(XX)                                32
133600070219      *
133700070219      *  Reimposta i dati del Partner Mittente Tabelle PT/PU/PV
133800070219     c                   if        %Found
133900070219      *
134000070219      *             *------------------------------*
134100070219     C                   EXSR      PARTNEROK
134200070219      *             *------------------------------*
134300070219      *
134400070219      *  Carica in schiera per permettere di trascrivere da EDIVAB a FIVAB
134500070219      *   questa schiera è utilizzata anche per i clienti.
134600070219     C     §ptKSC        LOOKUP    KCL                                    39
134700070219     c                   If        *in39 = *off
134800070219     C                   add       1             WW
134900070219     C                   z-add     §ptKSC        KCL(WW)
135000070219     C                   Endif
135100070219      *
135200070219     c                   Endif
135300070219     c                   End
135400070219      *
135500070219     C                   ENDSR
135600960527     C*----------------------------------------------------------------
135700960527     C*? DRED05 - DECODIFICA TIPO RECORD TD05
135800960527     C*----------------------------------------------------------------
135900960527     C     DRED05        BEGSR
136000960527     C*
136100000120     C                   MOVEL     SUMDAT        EDTD05DS
136200070219     C*
136300960627     C*  Se ci sono delle note le memorizzo (note partenza)
136400070219      * ? solo se si tratta di note SIN ICN o PAI non altro
136500960628     C     DB4451        IFEQ      'SIN'                                        -- 01 --
136600960627     C     DB4451        OREQ      'ICN'
136700960628     C                   SETOFF                                       0405
136800960628     C     DB4451        IFEQ      'SIN'
136900960628     C                   SETON                                        04
137000960628     C                   END
137100960628     C     DB4451        IFEQ      'ICN'
137200960628     C                   SETON                                        05
137300960628     C                   END
137400070219      *
137500040607     c                   If        In_Stampa ='S'
137600960628     C   OA              EXCEPT    TEST1
137700960628     C                   EXCEPT    NOTSPE
137800040607     c                   end
137900070219      *
138000960628     C                   DO        4             X                              -- 02 --
138100960628     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
138200970520     C                   ADD       1             FF
138300970520     C                   MOVEL     DB4451        QUA(FF)
138400970520     C                   MOVEL     D05(X)        FTX(FF)
138500960628     C     WNOT1         IFEQ      *BLANKS                                      -- 04 --
138600960627     C                   MOVEL     D05(X)        WNOT1
138700961231     C                   MOVE      D05(X)        WNOT2
138800960628     C                   ELSE                                                   -- 04 --
138900960628     C     WNOT2         IFEQ      *BLANKS                                      -- 05 --
139000960628     C                   MOVEL     D05(X)        WNOT2
139100960628     C                   END                                                    -- 05 --
139200960628     C                   END                                                    -- 04 --
139300960628     C*  Stampo dati note
139400960702     C                   MOVEL     D05(X)        WNOTSP           70
139500040607     c                   If        In_Stampa ='S'
139600960628     C   OA              EXCEPT    TEST1
139700960628     C                   EXCEPT    NOTSP1
139800040607     c                   end
139900960628     C                   END                                                    -- 03 --
140000960628     C                   END                                                    -- 02 --
140100960527     C*
140200960628     C                   END                                                    -- 01 --
140300960628     C*
140400960628     C*  Se ci sono delle note le memorizzo (note partenza)
140500960702     C     DBE451        IFEQ      'SIN'                                        -- 01 --
140600960702     C     DBE451        OREQ      'ICN'
140700960702     C     DBE451        IFNE      DB4451
140800960628     C                   SETOFF                                       0405
140900960702     C     DBE451        IFEQ      'SIN'
141000960628     C                   SETON                                        04
141100960628     C                   END
141200960702     C     DBE451        IFEQ      'ICN'
141300960628     C                   SETON                                        05
141400960628     C                   END
141500070219      *
141600040607     c                   If        In_Stampa ='S'
141700960628     C   OA              EXCEPT    TEST1
141800960628     C                   EXCEPT    NOTSPE
141900040607     c                   end
142000960628     C                   END
142100960628     C*
142200960628     C                   DO        4             X                              -- 02 --
142300960628     C     D05A(X)       IFNE      *BLANKS                                      -- 03 --
142400970623     C                   ADD       1             FF
142500970623     C                   MOVEL     DBE451        QUA(FF)
142600970623     C                   MOVEL     D05A(X)       FTX(FF)
142700960628     C     WNOT1         IFEQ      *BLANKS                                      -- 04 --
142800960628     C                   MOVEL     D05A(X)       WNOT1
142900961231     C                   MOVE      D05A(X)       WNOT2
143000960628     C                   ELSE                                                   -- 04 --
143100960628     C     WNOT2         IFEQ      *BLANKS                                      -- 05 --
143200960628     C                   MOVEL     D05A(X)       WNOT2
143300960628     C                   END                                                    -- 05 --
143400960628     C                   END                                                    -- 04 --
143500960628     C*  Stampo dati note
143600040607     c                   If        In_Stampa ='S'
143700960628     C   OA              EXCEPT    TEST1
143800960628     C                   EXCEPT    NOTSP1
143900040607     c                   end
144000960628     C                   END                                                    -- 03 --
144100960628     C                   END                                                    -- 02 --
144200960628     C*
144300960628     C                   END                                                    -- 01 --
144400060317     C*------>>
144500960628     C*  Decodifico tipo pagamento C/Assegno
144600060317      *      lo pulisco solo se non l'ho decodificato e non l'ho
144700060317      *       ancora scritto (Problema di pulizia in presenza di + righe TD05)
144800060317      *       --> succedeva che al primo giro aveva letto una TD05 con le informazioni
144900060317      *           x il tipo incasso poi arrivava una seconda riga TD05 con altro tipo
145000060317      *           di informazioni, la riga del VAB non era stata ancora scritta e qui
145100060317      *           abblankava il VABTIC togliendo il Tipo Incasso inviato.
145200060317     C                   if        wri_vabtic = *blank
145300960716     C                   MOVEL     *BLANKS       VABTIC
145400060317     c                   end
145500060317      *
145600080716     c                   clear                   trovato_TPI
145700070219      * ?Se c'è il tipo Incasso
145800960628     C     DB4451        IFEQ      'PAI'                                        -- 01 --
145900981009      *
146000960628     C                   DO        4             X                              -- 02 --
146100960628     C     D05(X)        IFNE      *BLANKS                                      -- 03 --
146200960716     C                   MOVEL     D05(X)        WCTC              2
146300960716     C                   Z-ADD     1             Y
146400061128     c                   movel(p)  Wctc          fld35a           35
146500061128     c                   move      §puTC         fld35a
146600061128     C     fld35a        LOOKUP    CTC35(Y)                               33
146700981009      * SE PARTNER IMPOSTO 'OM'
146800981009     C     §PUTPC        IFEQ      'P'
146900060928     c     wctc          andne     'TO'
147000981009     C                   MOVEL     'OM'          VABTIC
147100981009     C                   ELSE
147200981009     C   33              MOVEL     TPI(Y)        VABTIC
147300080716     c   33              move      'S'           trovato_TPI
147400960628     C                   END                                                    -- 03 --
147500981009     C                   END                                                    -- 03 --
147600070219     C                   ENDdo                                                  -- 02 --
147700981009      *
147800960628     C                   ELSE                                                   -- 01 --
147900070219      *
148000070219      * ?Se NON c'è il tipo Incasso Contrassegno particolare
148100070219      *    Deve essere impostato comunque da testata
148200080716     c                   if        vabtic = *blank
148300070219     C                   MOVEL     wVabTIC       VABTIC
148400080716     c                   end
148500981009      *
148600960702     C     DBE451        IFEQ      'PAI'                                        -- 02 --
148700960628     C                   DO        4             X                              -- 03 --
148800070125     C     D05A(X)       IFNE      *BLANKS                                      -- 04 --
148900070125     C                   MOVEL     D05A(X)       WCTC              2
149000981009     C                   Z-ADD     1             Y
149100061128     c                   movel(p)  Wctc          fld35a           35
149200061128     c                   move      §puTC         fld35a
149300061128     C     fld35a        LOOKUP    CTC35(Y)                               33
149400981009      * SE PARTNER IMPOSTO 'OM'
149500981009     C     §PUTPC        IFEQ      'P'
149600060928     c     wctc          andne     'TO'
149700981009     C                   MOVEL     'OM'          VABTIC
149800981009     C                   ELSE
149900981009     C   33              MOVEL     TPI(Y)        VABTIC
150000080716     c   33              move      'S'           trovato_TPI
150100981009     C                   END                                                    -- 03 --
150200960628     C                   END                                                    -- 04 --
150300960628     C                   END                                                    -- 03 --
150400960628     C                   END                                                    -- 02 --
150500981009      *
150600960628     C                   END                                                    -- 01 --
150700981009      *
150800060317      *   Memorizza il Tipo Incasso se decodificato poichè potrebbero
150900060317      *    essere presenti altri record Flat TD05 con altre informazioni
151000060317      *    che potrebbero abblancare il Tipo incasso precedentemente
151100060317      *    decodificato
151200080716     c                   if        Trovato_TPI = 'S' and wri_vabtic = *blank
151300060317     c                   eval      wri_vabtic = 'S'
151400060317     c                   end
151500060317      *
151600960527     C                   ENDSR
151700000125      *----------------------------------------------------------------
151800000125      *? DRED10 - DECODIFICA TIPO RECORD TD10
151900000125      *----------------------------------------------------------------
152000960527     C     DRED10        BEGSR
152100000125      *
152200000120     C                   MOVEL     SUMDAT        EDTD10DS
152300061128      *
152400000125      *  Trascodifico tipo trasporto
152500000125     C     VABCBO        IFEQ      *BLANKS
152600000125      *
152700070219     c                   clear                   TOD_char3         3
152800070219      *   Imposta o il codice o il metodo
152900070219      *     a seconda di chi invia metta l'uno o l'altro campo nel segmento
153000070219     c                   if        DC4053  <> *blank
153100070219     c                   movel(p)  DC4053        TOD_char3
153200070221     c                   else
153300070219     c                   if        DC4215  <> *blank
153400070219     c                   movel(p)  DC4215        TOD_char3
153500070219     c                   end
153600070219     c                   end
153700070219      ****************
153800070219      *****  In questo nuovo modo vale sia x EEX che per clienti che trasmettono
153900070219      *****    seguendo gli standard EDIFACT es.: PP (PrePaied)
154000070219      ****************
154100070219     c                   if              TOD_char3 <> *blank
154200070219     c                   movel(p)  TOD_char3     fld35a           35
154300061128     c                   move      §puTB         fld35a
154400070219     C                   eval      Y = 1
154500061128     C     fld35a        LOOKUP    CTB35(Y)                               32
154600000125      *
154700070221      *  Se non ha trova con il campo 4053 è possibile che abbiano codificato
154800070221      *   nell'altro campo 4215 quindi comunque ci si riprova:
154900070221      *
155000070221     C                   if        *in32 = *Off and DC4215  <> *blank
155100070221      *
155200070221     c                   movel(p)  DC4215        TOD_char3
155300070221     c                   movel(p)  TOD_char3     fld35a           35
155400070221     c                   move      §puTB         fld35a
155500070221     C                   eval      Y = 1
155600070221     C     fld35a        LOOKUP    CTB35(Y)                               32
155700070221      *
155800070221     c                   end
155900070221      *
156000070221     c                   end
156100070221      *
156200070221      *  se anche a questo punto non ha decoficato nulla prende il default se c'è
156300070221     C     *IN32         IFEQ      *off
156400070221     C     §puPFA        andne     *blank
156500070221     c                   SELECT
156600070221      *  Porto Franco
156700070221     c                   WHEN      §puPFA = 'F'  and  VABcas > *zeros
156800070221     C                   movel     '4'           VABCBO                         + C/Ass
156900070221     c                   WHEN      §puPFA = 'F'
157000070221     C                   movel     '1'           VABCBO                         Senza C/Ass.
157100070221      *  Porto Assegnato
157200070221     c                   WHEN      §puPFA = 'A'  and  VABcas > *zeros
157300070221     C                   movel     '6'           VABCBO                         + C/Ass
157400070221     C                   WHEN      §puPFA = 'A'
157500070221     C                   movel     '2'           VABCBO                         Senza C/Ass
157600070221     C                   ENDSL
157700070221      *
157800070221     c                   end
157900070221      *
158000070221      *  SE invece ha decodificato dalla tabella e dal dato passato dal Ptn
158100000125     C     *IN32         IFEQ      '1'
158200000125     c                   SELECT
158300000125     c                   WHEN      POR(Y) = 'F'  and  VABcas > *zeros
158400000125     C                   movel     '4'           VABCBO                         + C/Ass
158500000125     c                   WHEN      POR(Y) = 'F'
158600000125     C                   movel     '1'           VABCBO                         Senza C/Ass.
158700000125      *  Porto Assegnato
158800000125     c                   WHEN      VABCAS > *zeros
158900000125     C                   movel     '6'           VABCBO                         + C/Ass
159000000125     C                   OTHER
159100000125     C                   movel     '2'           VABCBO                         Senza C/Ass
159200000125     C                   ENDSL
159300000125      *
159400000125     C                   ENDIF
159500000125      *
159600061128      *
159700070219      *  No decodifica termini di consegna: errore
159800070219     c                   if        TOD_char3 =  *blank  or *in32 =*off
159900070221     c                   if        §pupfa = *blank
160000000125     C                   z-add     8             WINDER
160100000125     C                   clear                   WVALOR
160200000125     C                   eval      *in05 = *on
160300960527     C                   EXSR      STPERR
160400070221     c                   end
160500070219     c                   end
160600061128      *
160700000125     C                   ENDIF
160800980701      *
160900000125      *  Se ho AGE prendo i riferimenti.
161000000125      *  Quello numerico solo se non ho FF o CU
161100000125     C                   SELECT
161200000125     c                   WHEN       DC1153 = 'AGE'  and  DC1154 <> *BLANKS
161300000125     C                   movel     DC1154        WAGE
161400000125     c                   WHEN       DCA153 = 'AGE'  and  DCA154 <> *BLANKS
161500000125     C                   movel     DCA154        WAGE
161600000125     c                   WHEN       DCB153 = 'AGE'  and  DCB154 <> *BLANKS
161700000125     C                   movel     DCB154        WAGE
161800000125     C                   ENDSL
161900980701      *
162000000125     C                   IF         WAGE <> *BLANKS
162100000125     C                   movel     WAGE          WNSP             35
162200000125     C                   movel     WAGE          WNUM             35
162300000125     C     ' '           scan      WAGE          WLUNGH
162400000125     C                   sub       1             WLUNGH
162500980701     C                   EXSR      TSTNUM
162600000125     C                   IF         WOKNUM = 'S'                                Ctrl numerico
162700000125     C                   movel     WAGE          VABRMA
162800000125     C                   IF         WRMN = 'AGE'  or  WRMN = *blanks
162900000125     C                   movel     WNUMOK        VABRMN
163000000125     C                   movel     'AGE'         WRMN
163100000125     C                   ENDIF
163200000125     C                   ENDIF
163300000125     C                   ENDIF
163400980701      *
163500980701      *  Se ho FF e trovo il cod. cliente corrispondente prendo il rif. numerico
163600980715      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
163700000125      **********************************************************************
163800000125      *  In questa fase memorizzo i campi necessari al controllo che ho
163900000125      *  spostato nella sbr  DRED15 dopo aver riagganciato la PT
164000000125      **********************************************************************
164100090630      **  GEODIS CALBERSON per mandare il riferimento cliente usa RFF+ADE al posto di RFF+FF
164200000125     C                   SELECT
164300090630     C                   WHEN      (DC1153 = 'FF '  and  DC1154 <> *BLANKS) or
164400090630     C                             (DC1153 = 'ADE'  and  DC1154 <> *BLANKS)
164500000125     C                   movel     DC1154        WFF
164600090630     C                   WHEN      (DCA153 = 'FF '  and  DCA154 <> *BLANKS) or
164700090630     C                             (DCA153 = 'ADE'  and  DCA154 <> *BLANKS)
164800000125     C                   movel     DCA154        WFF
164900090630     C                   WHEN      (DCB153 = 'FF '  and  DCB154 <> *BLANKS) or
165000090630     C                             (DCB153 = 'ADE'  and  DCB154 <> *BLANKS)
165100000125     C                   movel     DCB154        WFF
165200000125     c                   ENDSL
165300980701      *
165400980701      *  Se ho CU prendo il riferimento numerico
165500980715      *  ECCEZIONE: se ho già impostato FF obbligatorio
165600980715     C     WRMN          IFNE      'FFS'
165700980715      *
165800000125     C                   SELECT
165900000125     C                   WHEN        DC1153 = 'CU '  and  DC1154 <> *blanks
166000000125     C                   movel     DC1154        WCU
166100000125     C                   WHEN        DCA153 = 'CU '  and  DCA154 <> *blanks
166200000125     C                   movel     DCA154        WCU
166300000125     C                   WHEN        DCB153 = 'CU '  and  DCB154 <> *blanks
166400000125     C                   movel     DCB154        WCU
166500000125     C                   ENDSL
166600980701      *
166700980701     C     WCU           IFNE      *BLANKS
166800000125     C                   movel     WCU           WNUM             35
166900000125     C     ' '           scan      WCU           WLUNGH
167000000125     C                   sub       1             WLUNGH
167100980701     C                   EXSR      TSTNUM
167200000125     C                   If          WOKNUM = 'S'                               Ctrl numerico
167300000125     C                   movel     WNUMOK        VABRMN
167400000125     C                   movel     'CU '         WRMN
167500000125     C                   Endif
167600000125     C                   ENDIF
167700980701      *
167800000125     C                   ENDIF
167900980715      *
168000000125      *  Stampo dati riferimenti consegna
168100970429     C     STPD10        TAG
168200040607     c                   If        In_Stampa ='S'
168300960628     C   OA              EXCEPT    TEST1
168400960628     C                   EXCEPT    RIFCON
168500040607     c                   end
168600010502      *
168700010502     C*----------------------------------------------------------------
168800010502      *  La routine finiva qui ma per E.Arden che tratta il segmento
168900010502      *  NAD+DP nel TD15 senza avere il NAD+CZ occorre decodificare in
169000010502      *  anticipo il codice particolare del Cliente (CL)
169100010502     C*----------------------------------------------------------------
169200010502      * Se ho FF e trovo il cod. cliente prendo il rif. numerico  ex DRED15
169300010502      *  ECCEZIONE: se ho già impostato CU ed FF non è obbligatorio
169400010502     C     WFF           IFNE      *BLANKS
169500010502     C                   eval      XX = 1
169600010502     C                   movel     §PTKSC        WCCL             35
169700010502     C                   movel     WFF           WCOM28           28
169800010502     C                   move      WCOM28        WCCL
169900010502     C     WCCL          lookup    CCL(XX)                                34
170000010502      *
170100010502     C     *IN34         IFEQ      '1'
170200010502     C                   movel     DCL(XX)       EDIDSCL
170300040311     C                   z-add     §CLKSC        WCLKSC            7 0
170400040311     C                   movel     §CLTAR        WCLTAR            3
170500040311     C                   z-add     §CLLNP        WCLLNP            3 0
170600040311     C                   z-add     §CLNRS        WCLNRS            2 0
170700040311     C                   movel     §CLCTM        WCLCTM            2
170800040311     C                   movel     §CLBS1        WCLBS1            1
170900050223     C                   movel     §CLnsp        WCLfnsp           1            *blk/S
171000040311      *
171100040311      * imposta la LNP   x il dettaglio specifco
171200040311     c                   eval      VABlnp = §CLlnp
171300040311      *
171400040311      * imposta la Serie x il dettaglio specifco
171500040311     c                   eval      VABnrs = §CLnrs
171600040311      *
171700040311      *  In base al cod.trattamento merce reperisco tipo tratt.
171800040311      *    per un dettaglio specifico.
171900040311     C                   CLEAR                   DS1B
172000040311     C                   Z-ADD     1             Y                 3 0
172100040311     C     §CLctm        LOOKUP    CTM(Y)                                 32
172200040311     C     *IN32         IFEQ      '1'
172300040311     C                   MOVEL     DTM(Y)        DS1B
172400040311     C                   END
172500040311      *
172600040311      * Se il cliente non era sulla schiera lo aggiunge
172700010502     C     §CLKSC        lookup    KCL                                    39
172800010502     C                   if        *in39 = *off
172900010502     C                   add       1             WW
173000010502     C                   z-add     §CLKSC        KCL(WW)
173100010502     c                   endif
173200010502      *
173300010502      * Controllo se devo memorizzare un altro riferimento
173400010502     C     §CLFTX        IFNE      *BLANKS
173500010502     C                   eval      FF = 1
173600010502     C     §CLFTX        lookup    QUA(FF)                                34
173700010502      *
173800010502     C     *IN34         IFEQ      '1'
173900010502     C                   movel     FTX(FF)       WNUM             35
174000010502     C     ' '           scan      FTX(FF)       WLUNGH
174100010502     C                   sub       1             WLUNGH
174200010502     C                   EXSR      TSTNUM
174300010502      *
174400010502     C                   SELECT
174500010502     C                   WHEN      WOKNUM <> 'S'                                Ctrl numerico
174600010502     C                   WHEN      §CLROB  = 'S'
174700010502     C                   movel     WNUMOK        VABRMN
174800010502     C                   movel     'FFS'         WRMN
174900010502     C                   WHEN      WRMN   <> 'CU'
175000010502     C                   movel     WNUMOK        VABRMN
175100010502     C                   movel     'FF '         WRMN
175200010502     C                   ENDSL
175300010502      *
175400010502     C                   ENDIF
175500010502     C                   ENDIF
175600010502      *
175700010502     C                   ENDIF
175800010502     C                   ENDIF
175900010502     C*----------------------------------------------------------------
176000000125      *
176100960527     C                   ENDSR
176200010502     C*----------------------------------------------------------------
176300960527     C*? DRED15 - DECODIFICA TIPO RECORD TD15
176400960527     C*----------------------------------------------------------------
176500960527     C     DRED15        BEGSR
176600000120      *
176700000120     C                   MOVEL     SUMDAT        EDTD15DS
176800000120     c                   eval      *in04 = *off
176900000120     c                   eval      *in07 = *off
177000000120      * Dati destinatario
177100000120     C                   IF        DD3035 = 'CN'  or
177200000120     C                             (§PTDES <> *BLANKS  and  §PTDES = DD3035)
177300070219      *
177400000120     c                   eval      *in04 = *on
177500000120     c                   eval      *in07 = *on
177600960822     C* Reperisco nazione corrispondente destinatario
177700960822     C                   Z-ADD     1             YY                3 0
177800000120     c                   eval      *in32 = *off
177900000120     C                   if         DD3207 <> *BLANKS
178000960822     C     DD3207        LOOKUP    ISO(YY)                                32
178100000120     C                   endif
178200000125     C* Imposto dati destinatario
178300960527     C                   MOVEL     DD3036        VABRSD
178400000120     C                   if         DDA036 <> *LOVAL
178500960527     C                   MOVEL     DDA036        VABRD2
178600000120     C                   endif
178700960527     C                   MOVEL     DD3042        VABIND
178800960527     C                   MOVEL     DD3164        VABLOD
178900000120     C* Se DDA042 non è vuoto lo imposto dalla posizione WIND+1 in VABLOD
179000000120     c                   if        DDA042 <> *blanks  and  DDA042 <> *loval
179100070219      *
179200970710     C* Verifico se c'è dello spazio per nella località
179300970710     C     '    '        SCAN      VABLOD        WI                4 0
179400070219      *
179500970710     C                   MOVEA     VABLOD        LOD
179600970710     C                   ADD       1             WI
179700970711     C                   MOVEA     DDA042        LOD(WI)
179800970711     C                   MOVEA     LOD           VABLOD
179900000125     C                   ENDif
180000000120      *
180100000120     C                   If        *in32 = *on
180200000120     C                   MOVEL     C15(YY)       VABNZD
180300000120     C                   MOVEL     CPF(YY)       WFLCPF            2 0
180400000120     c                   Else
180500000120     C                   MOVEL     *BLANKS       VABNZD
180600000120     C                   Z-ADD     0             WFLCPF
180700000120     c                   Endif
180800000125      * Converto CAP
180900960828     C                   EXSR      CNVCAP
181000000120     C                   ENDIF
181100000120      *
181200980930      * Dati mittente
181300000120     C                   IF         DD3035 = 'CZ'
181400000120     C                   movel     DD3036        VABRMO
181500000125      *
181600000120      *  Reperisco nazione mittente se ERRORE utilizzo PARTNER
181700000125     C                   movel     §PTNAZ        VABNMO
181800040302      *
181900000125     C                   IF         DD3207 <> *blanks
182000000125     C                   eval      YY = 1
182100960822     C     DD3207        LOOKUP    ISO(YY)                                32
182200000125     C                   if         *in32 = *on
182300000125     C                   MOVEL     C15(YY)       VABNMO
182400000125     C                   endif
182500000125     C                   ENDIF
182600000125      *
182700991007      * Normalizzo CAP mittente originale
182800000120     C                   clear                   T
182900000120     C                   clear                   S
183000991007     C                   MOVEA     *BLANKS       CAPM
183100991007     C                   MOVEA     DD3251        CAP9
183200991007     C                   DO        9             T
183300991007     C     CAP9(T)       IFNE      *BLANKS
183400991007     C                   ADD       1             S
183500991007     C                   MOVE      CAP9(T)       CAPM(S)
183600991007     C                   ENDIF
183700991007     C                   ENDDO
183800991007     C*
183900980930      *     Controllo validità CAP
184000991007     C                   MOVEA     CAPM          VABCMO
184100980930      *
184200000120     C                   CLEAR                   TISI95DS
184300980930     C                   MOVEL     VABCMO        I95CAP
184400980930     C                   MOVEL     VABNMO        I95NAR
184500980930     C                   MOVEL     WOGGI         I95DAT
184600980930     C                   MOVEL     '3'           I95TCN
184700980930     C                   CALL      'TISI95R'
184800000120     C                   PARM                    TISI95DS
184900980930      *     Errore
185000980930     C     O95ERR        IFNE      *BLANKS
185100980930     C                   MOVEL     INDCAE        VABCMO
185200980930     C                   MOVEL     §PTNAZ        VABNMO
185300961024     C                   END
185400980929      *
185500960527     C                   END
185600980929      *
185700980929      *  Stampo dati soggetti transazione
185800040607     c                   If        In_Stampa ='S'
185900960628     C   OA              EXCEPT    TEST1
186000960628     C                   EXCEPT    SOGTRA
186100040607     c                   end
186200000125      *
186300000125      *  Controllo se inserito rivolgersi a ..
186400960628     C     DD3139        IFNE      *BLANKS
186500960628     C     DD3412        ORNE      *BLANKS
186600040607     c                   If        In_Stampa ='S'
186700960628     C   OA              EXCEPT    TEST1
186800960628     C                   EXCEPT    SOGRIV
186900040607     c                   end
187000960628     C                   END
187100000125      *
187200031211      *  memorizza il riferimento di consegna x il destinatario
187300031211      *  memorizza il riferimento di consegna nr.telefonico
187400031211     C     DD3412        ifne      *BLANKS
187500031211     c                   movel     DD3412        watrde
187600031211     c                   end
187700031211     C     DD3148        ifne      *BLANKS
187800031211     c                   movel     DD3148        wattel
187900031211     c                   end
188000031211      *
188100960527     C                   ENDSR
188200960527     C*----------------------------------------------------------------
188300960527     C*? DRED20 - DECODIFICA TIPO RECORD TD20
188400960527     C*----------------------------------------------------------------
188500960527     C     DRED20        BEGSR
188600960527     C*
188700000120     C                   MOVEL     SUMDAT        EDTD20DS
188800070219      *
188900960527     C*  Controllo validità campi numerici
189000070219     c                   if        w1496 =  *blank
189100070219     c                   move      *zeros        w1496
189200070219     c                   end
189300070219      *
189400960712     C                   TESTN                   W1496                98
189500960712     C                   MOVEL     W1496         WS1496
189600960531     C     *IN98         IFEQ      '0'
189700960527     C                   Z-ADD     13            WINDER
189800960527     C                   MOVEL     *BLANKS       WVALOR
189900960527     C                   MOVEL     W1496         WVALOR
190000960731     C                   SETON                                        05
190100960527     C                   EXSR      STPERR
190200960527     C                   GOTO      FIND20
190300960527     C                   END                                                    Imp.non numer.
190400070219      *
190500960527     C* Totale colli numerico
190600960527     C                   TESTN                   W7224                98
190700960531     C     *IN98         IFEQ      '0'
190800960527     C                   Z-ADD     14            WINDER
190900960527     C                   MOVEL     *BLANKS       WVALOR
191000960527     C                   MOVEL     W7224         WVALOR
191100960731     C                   SETON                                        05
191200960527     C                   EXSR      STPERR
191300960527     C                   GOTO      FIND20
191400960527     C                   END
191500070219      *
191600960527     C*  Se importo numerico decodifico tipo
191700960603     C     *IN98         IFEQ      '1'
191800960527     C* Se ho totali per spedizione metto in campi VAB
191900960528     C     DE1496        IFEQ      99999
192000020807     C     DE1496        orEQ      09999
192100970612     C                   MOVEL     'S'           WGID99
192200960712     C                   MOVE      W7224         VABNCL
192300960527     C                   ELSE
192400970612     C     WGID99        IFEQ      'N'
192500960528     C                   MOVE      W7224         WTNCL             5 0
192600960528     C                   ADD       WTNCL         VABNCL
192700960527     C                   END
192800970612     C                   END
192900960712     C                   ADD       VABNCL        WTOCOC           16 0
193000960527     C                   END
193100960527     C*
193200050520     C*  Natura Merce  (Se abilitato alla trascodifica)
193300050520     c                   if        §PUnas = 'S'
193400050520     C                   If        De4451 ='AAA'
193500050520     C                   Movel     De4440        VABnas
193600050520     C                   EnD
193700050520     c                   end
193800050520     C*
193900960527     C     FIND20        ENDSR
194000960527     C*----------------------------------------------------------------
194100960527     C*? DRED25 - DECODIFICA TIPO RECORD TD25
194200960527     C*----------------------------------------------------------------
194300960527     C     DRED25        BEGSR
194400960527     C*
194500000120     C                   MOVEL     SUMDAT        EDTD25DS
194600960527     C*  Controllo validità campi numerici
194700960711     C                   TESTN                   WPESO                98
194800960527     C*  Peso
194900960527     C     DF6311        IFEQ      'WT '
195000960527     C     DF6411        ANDEQ     'KGM'
195100960531     C     *IN98         IFEQ      '0'
195200960527     C                   Z-ADD     15            WINDER
195300960527     C                   MOVEL     *BLANKS       WVALOR
195400960711     C                   MOVEL     WPESO         WVALOR
195500960731     C                   SETON                                        05
195600960527     C                   EXSR      STPERR
195700960527     C                   ELSE
195800960527     C*  Controllo se ho valore totale o parziale
195900960712     C     WS1496        IFEQ      99999
196000020807     C     WS1496        orEQ      09999
196100960527     C                   Z-ADD     DF6314        VABPKB                         Peso
196200960527     C                   ELSE
196300970612     C     WGID99        IFEQ      'N'
196400960527     C                   ADD       DF6314        VABPKB                         Peso
196500960527     C                   END
196600970612     C                   END
196700960703     C     DF6313        IFEQ      'AAG'
196800960703     C                   ADD       DF6314        WTOPLC           16 0          ???
196900960703     C                   END
197000960703     C     DF6313        IFEQ      'AAL'
197100960703     C                   ADD       DF6314        WTOPNC           16 0          ???
197200960703     C                   END
197300960527     C                   END
197400960527     C                   END
197500061127      ***
197600061128     C*  Peso in Grammi  se abilitato a utilizzarlo
197700061128      ***
197800061128     C     §puGR         IFEQ      'S'
197900061128     C     DF6311        IFEQ      'WT '
198000061127     C     DF6411        ANDEQ     '163'
198100070219      *
198200061127     C     *IN98         IFEQ      '0'
198300061127     C                   Z-ADD     15            WINDER
198400061127     C                   MOVEL     *BLANKS       WVALOR
198500061127     C                   MOVEL     WPESO         WVALOR
198600061127     C                   SETON                                        05
198700061127     C                   EXSR      STPERR
198800061127     C                   ELSE
198900061127     C*  Controllo se ho valore totale o parziale
199000061127     C     WS1496        IFEQ      99999
199100061127     C     WS1496        orEQ      09999
199200061127     C     DF6314        div       1000          VABPKB                         Peso
199300061127     C                   ELSE
199400061127     C     WGID99        IFEQ      'N'
199500061127     C     DF6314        div       1000          Peso_inKG        12 3          Peso
199600061127     C                   add       Peso_inKG     VABPKB                         Peso
199700061127     C                   END
199800061127     C                   END
199900061127     C     DF6313        IFEQ      'AAG'
200000061127     C     DF6314        div       1000          Peso_inKG        12 3          Peso
200100061127     C                   ADD       Peso_inKG     WTOPLC
200200061127     C                   END
200300061127     C     DF6313        IFEQ      'AAL'
200400061127     C     DF6314        div       1000          Peso_inKG        12 3          Peso
200500061127     C                   ADD       Peso_inKG     WTOPNC
200600061127     C                   END
200700061127     C                   END
200800061127     C                   END
200900061128     c                   endIF
201000990616      *  Volume
201100990616     C     DF6311        IFEQ      'VOL'
201200960917     C     DF6411        ANDEQ     'MTQ'
201300990616     C     *IN98         IFEQ      '0'
201400960527     C                   Z-ADD     16            WINDER
201500960527     C                   MOVEL     *BLANKS       WVALOR
201600960527     C                   MOVEL     WCOM16        WVALOR
201700960731     C                   SETON                                        05
201800960527     C                   EXSR      STPERR
201900990616     C                   ELSE
202000990616      *  Controllo se ho valore totale o parziale
202100990616     C     WS1496        IFEQ      99999
202200020807     C     WS1496        orEQ      09999
202300960527     C                   Z-ADD     DF6314        VABVLB                         Volume
202400990616     C                   Z-ADD     DF6314        PVLB
202500990616     C                   ELSE
202600990616     C     WGID99        IFEQ      'N'
202700960527     C                   ADD       DF6314        VABVLB                         Volume
202800990616     C                   ADD       DF6314        PVLB
202900990616     C                   END
203000990616     C                   END
203100990616     C                   END
203200990616     C                   END
203300990616      *
203400960628     C*  Stampo dati articolo
203500960628     C     DE7140        IFNE      *BLANKS
203600960628     C     DE7143        ORNE      *BLANKS
203700960628     C     DE4440        ORNE      *BLANKS
203800040607     c                   If        In_Stampa ='S'
203900960628     C   OA              EXCEPT    TEST1
204000960628     C                   EXCEPT    DATART
204100040607     c                   end
204200960628     C                   END
204300960829     C* Ricerco CAP
204400961104     C     WCAP          IFNE      *BLANKS
204500961017     C     DF6311        IFEQ      'WT '
204600040302      *
204700040302      * PRENDO TERMINAL PARTENZA LINEA DI PARTENZA
204800040302     C                   CLEAR                   FNLV55DS
204900040302     C                   MOVEL     'P'           D55TPT
205000040302     C                   MOVEL     WOGGI         D55DRF
205100040302     C                   MOVEL     VABLNP        D55LNP
205200040302     C                   MOVEL     VABLNP        D55LIN
205300040302     C                   CALL      'FNLV55R'
205400040302     C                   PARM                    FNLV55DS
205500040302     C                   MOVE      D55TFP        COMTFP            3 0
205600040302      *
205700040302     C*  PASSO IL TERMINAL PARTENZA
205800000120     C                   CLEAR                   TISI95DS
205900970918     C                   Z-ADD     COMTFP        I95TFP
206000980602     C                   MOVEL     VABTSP        I95TSP
206100980602     C                   MOVEL     'S'           I95FRE
206200970710     C                   MOVEL     '3'           I95TCN
206300970710     C                   MOVEL     WCAP          I95CAP
206400970710     C                   MOVEL     VABNZD        I95NAR
206500970710     C                   MOVEL     VABLOD        I95LOC
206600970710     C                   MOVEL     VABIND        I95IND
206700970710     C* Se gestita seconda linea di arrivo passo peso - volume
206800970710     C* e numero colli effettivo
206900970710     C     §PULNA        IFEQ      'S'
207000970710     C                   Z-ADD     VABPKB        I95LKG
207100970710     C                   Z-ADD     VABVLB        I95LMC
207200970710     C                   END
207300971202     C*
207400971202     C* Imposto flag tipo bolla Franco/Assegnato e C/Assegno
207500971202     C                   SELECT
207600971202     C     VABCBO        WHENEQ    '1 '
207700971202     C                   MOVEL     *BLANKS       I95FCA
207800971202     C                   MOVEL     'F'           I95TPO
207900971202     C     VABCBO        WHENEQ    '2 '
208000971202     C                   MOVEL     *BLANKS       I95FCA
208100971202     C                   MOVEL     'A'           I95TPO
208200971202     C     VABCBO        WHENEQ    '4 '
208300971202     C                   MOVEL     'S'           I95FCA
208400971202     C                   MOVEL     'F'           I95TPO
208500971202     C     VABCBO        WHENEQ    '6 '
208600971202     C                   MOVEL     'S'           I95FCA
208700971202     C                   MOVEL     'A'           I95TPO
208800971202     C                   OTHER
208900971202     C                   MOVEL     'F'           I95TPO
209000971202     C     VABCAS        IFGT      0
209100971202     C                   MOVEL     'S'           I95FCA
209200971202     C                   ELSE
209300971202     C                   MOVEL     *BLANKS       I95FCA
209400971202     C                   END
209500971202     C                   ENDSL
209600971202     C*
209700970710     C                   CALL      'TISI95R'
209800000120     C                   PARM                    TISI95DS
209900970710     C* Reperisco i dati da CAP
210000970710     C                   MOVEL     O95PRV        VABPRD
210100971202     C                   MOVEL     O95LNA        VABLNA
210200970918     C                   MOVEL     O95ZNC        VABZNC
210300970710     C                   MOVEL     WCAP          VABCAD
210400970710     C*
210500970710     C     O95ERR        IFNE      *BLANKS
210600970710     C     O95FIT        ANDEQ     'S'
210700960829     C*  Cap non trovato
210800960829     C                   Z-ADD     10            WINDER
210900960829     C                   MOVEL     *BLANKS       WVALOR
211000960829     C                   MOVEL     WCAP          WVALOR
211100960829     C                   SETON                                        05
211200960829     C                   EXSR      STPERR
211300960829     C                   END
211400961017     C                   END
211500961104     C                   ELSE
211600961104     C                   CLEAR                   VABPRD
211700961104     C                   CLEAR                   VABLNA
211800961104     C                   CLEAR                   VABZNC
211900961104     C                   CLEAR                   VABCAD
212000961104     C*  Cap non trovato
212100961104     C                   Z-ADD     10            WINDER
212200961104     C                   MOVEL     *BLANKS       WVALOR
212300961104     C                   MOVEL     WCAP          WVALOR
212400961104     C                   SETON                                        05
212500961104     C                   EXSR      STPERR
212600961104     C                   END
212700960527     C*
212800960527     C                   ENDSR
212900960527     C*----------------------------------------------------------------
213000960527     C*? DRED30 - DECODIFICA TIPO RECORD TD30
213100960527     C*----------------------------------------------------------------
213200960527     C     DRED30        BEGSR
213300980218      *
213400000120     C                   MOVEL     SUMDAT        EDTD30DS
213500040607     c                   If        In_Stampa ='S'
213600960628     C   OA              EXCEPT    TEST1
213700040607     c                   end
213800980218      *
213900980408      *  Se il trattamento merce prevede il segnacollo EUROEXPRESS li memorizzo
214000001031      *   --> prima era <S> adesso è <E> per il funzionamento del Disk C
214100001031     C     §1BF12        IFEQ      'E'
214200980218     C                   EXSR      SEGNEE
214300980218     C                   ELSE
214400980218      *
214500040227      *  La prima volta controllo congruità numero segnacollo
214600040227     C     §PUBS1        IFNE      *BLANKS
214700040227     C     VABnrs        ANDNE     0
214800980218      *  Se il codice trattamento merce prevede che segnacollino i
214900980218      *  partner e il nr. serie > 0. Controllo nr.segnacollo OK
215000980218     C     §1BFG7        IFEQ      'S'
215100960731     C     §1BFG1        OREQ      'S'
215200980218      *  calcolo segnacollo
215300960725     C                   EXSR      SGNELA
215400960725     C                   END
215500960702     C                   END
215600980218      *
215700980218     C                   END
215800980218      *
215900960527     C                   ENDSR
216000980218      *----------------------------------------------------------------
216100070216      *? Input segnacolli su schiera di 20 elementi
216200980218      *----------------------------------------------------------------
216300070216     C     INP_Segnacol  BEGSR
216400980218      *
216500070216     c                   clear                   quanti_PCI        3 0
216600070216     c                   if        §puEL1 = 0
216700070216     c                   z-add     10            quanti_PCI
216800070216     c                   else
216900070216     c                   z-add     §puEL1        quanti_PCI
217000070216     c                   end
217100070216      *
217200070216      *  Pulisce la schiera dei segnacolli da elaborare
217300070216     c                   clear                   x30
217400070216     c                   z-add     0             Nr_x30            3 0
217500070216      *
217600070216      * 2 schiere per ogni TD30: D30_1 e D30_2
217700070216      *   riporto tutto sulla schiera generica X30
217800070216      *
217900070216      *  ciclo sui primi 10
218000070216     c                   do        10            limite            3 0
218100070216      * Limita quanti elementi passati in ogni PCI
218200070216      *   devono essere considerati
218300070216     c                   if        limite > quanti_PCI
218400070216     c                   Leave
218500070216     c                   end
218600070216      *
218700070216      *  Carica la schiera generale di tutti i segnacolli
218800070216     c                   if        D30_1(limite) <> *blank
218900070216     c                   add       1             Nr_x30
219000070216     c                   eval      X30(Nr_x30) = D30_1(limite)
219100070216     c                   end
219200070216     c                   enddo
219300070216      *
219400070216      *  ciclo sui secondi 10
219500070216     c                   do        10            limite
219600070216      * Limita quanti elementi passati in ogni PCI
219700070216      *   devono essere considerati
219800070216     c                   if        limite > quanti_PCI
219900070216     c                   Leave
220000070216     c                   end
220100070216      *
220200070216      *  Carica la schiera generale di tutti i segnacolli
220300070216     c                   if        D30_2(limite) <> *blank
220400070216     c                   add       1             Nr_x30
220500070216     c                   eval      X30(Nr_x30) = D30_2(limite)
220600070216     c                   end
220700070216     c                   enddo
220800070216      *
220900070216     C                   ENDSR
221000070216      *----------------------------------------------------------------
221100070216      *? SEGNEE - ELABORO SEGNACOLLO EUROEXPRESS
221200070216      *----------------------------------------------------------------
221300070216     C     SEGNEE        BEGSR
221400070216      *
221500070216     c                   exsr      INP_Segnacol
221600070216      *
221700980218      *  Loop lettura PCI: fino a che non arrivo a fine schiera (20 elementi)
221800070216     C                   DO        20            x                              --- 01 ---
221900980218      *
222000070216     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
222100070216     C     x30(X)        ANDNE     *LOVAL
222200070216      *
222300020909      *  Carico il segnacollo in schiera per poter scirvere EDiVAT
222400980219     C                   ADD       1             §§
222500070216     C                   MOVEL     x30(X)        SGE(§§)
222600980219     C     XW            IFLT      3                                            --- 03 ---
222700980218     C                   ADD       1             XW                3 0
222800980218     C                   MOVEL     DSSGN         SGT(XW)
222900980219     C                   END                                                    --- 03 ---
223000980219     C     XW            IFEQ      3                                            --- 03 ---
223100040607     c                   If        In_Stampa ='S'
223200980218     C                   EXCEPT    DATSGN
223300040607     c                   end
223400980218     C                   Z-ADD     *ZEROS        XW
223500980218     C                   CLEAR                   SGT
223600980219     C                   END                                                    --- 03 ---
223700980219      *
223800980219     C                   END                                                    --- 02 ---
223900070216      *
224000980219     C                   END                                                    --- 01 ---
224100980218      *
224200980218     C                   ENDSR
224300960725     C*----------------------------------------------------------------
224400960912     C*? SGNELA - DECODIFICA elabora nr.segnacollo
224500960725     C*----------------------------------------------------------------
224600960725     C     SGNELA        BEGSR
224700070216      *
224800070216     c                   exsr      INP_Segnacol
224900960725     C*
225000960911     C*  Loop lettura PCI: fino a che non arrivo a fine sch.20 elem.
225100960911     C*  in base al numero elementi validi carico sgn.
225200070216     C                   DO        20            X                              --- 01 ---
225300070216     C*
225400070216     C     x30(X)        IFNE      *BLANKS                                      --- 02 ---
225500070216     C     x30(X)        ANDNE     *LOVAL
225600070216     C*
225700960912     C                   CLEAR                   DSSGN
225800960911     C*  Controllo se devo ricevere la linea di partenza
225900960912     C                   MOVEL     'N'           WERRO             1
226000960912     C     §PULP1        IFGT      0                                            --- 03 ---
226100960911     C                   EXSR      REPLNP
226200960912     C                   END                                                    --- 03 ---
226300960911     C*  Controllo se devo ricevere la linea di arrivo
226400960912     C     §PULA1        IFGT      0                                            --- 03 ---
226500960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
226600960911     C                   EXSR      REPLNA
226700960912     C                   END                                                    --- 03 ---
226800960911     C*  Controllo se devo ricevere il numero di serie
226900960912     C     §PUNR1        IFGT      0                                            --- 03 ---
227000960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
227100960911     C                   EXSR      REPNRS
227200960912     C                   END                                                    --- 03 ---
227300960911     C*  Controllo se devo ricevere il numero segnacollo
227400960912     C     §PUSG1        IFGT      0                                            --- 03 ---
227500960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
227600960911     C                   EXSR      REPSGN
227700960912     C                   END                                                    --- 03 ---
227800960911     C*  Controllo se devo ricevere la zona di consegna
227900960912     C     §PUZN1        IFGT      0                                            --- 03 ---
228000960912     C     WERRO         ANDEQ     'N'                                          --- 03 ---
228100960911     C                   EXSR      REPZNC
228200960912     C                   END                                                    --- 03 ---
228300960912     C*  Imposto da VAB i dati a zero se mi è stato passato
228400960912     C*  numero segnacollo valido
228500960912     C     WERRO         IFEQ      'N'                                          --- 03 ---
228600960912     C     SGNLNP        IFEQ      0                                            --- 04 ---
228700040302     C                   Z-ADD     VABLNP        SGNLNP
228800960912     C                   END                                                    --- 04 ---
228900960912     C     SGNLNA        IFEQ      0                                            --- 04 ---
229000960911     C                   Z-ADD     VABLNA        SGNLNA
229100960912     C                   END                                                    --- 04 ---
229200960912     C     SGNNRS        IFEQ      0                                            --- 04 ---
229300960911     C                   Z-ADD     VABNRS        SGNNRS
229400960912     C                   END                                                    --- 04 ---
229500960912     C     SGNZNC        IFEQ      0                                            --- 04 ---
229600960911     C                   Z-ADD     VABZNC        SGNZNC
229700960912     C                   END                                                    --- 04 ---
229800960912     C*  mi reimposto LNA e zona da segnacollo
229900960912     C                   Z-ADD     SGNLNA        VABLNA
230000960912     C                   Z-ADD     SGNZNC        VABZNC
230100020909     C*  Carico il segnacollo in schiera per poter scirvere EDiVAD
230200960912     C                   ADD       1             XY
230300960912     C                   MOVE      SGNNSC        SGN(XY)
230400960912     C     XW            IFLT      3                                            --- 04 ---
230500960912     C                   ADD       1             XW                3 0
230600960912     C                   MOVEL     DSSGN         SGT(XW)
230700960912     C                   END                                                    --- 04 ---
230800960912     C     XW            IFEQ      3                                            --- 04 ---
230900040607     c                   If        In_Stampa ='S'
231000960912     C                   EXCEPT    DATSGN
231100040607     c                   end
231200960912     C                   Z-ADD     0             XW
231300960912     C                   CLEAR                   SGT
231400960912     C                   END                                                    --- 04 ---
231500960912     C                   END                                                    --- 03 ---
231600070216     C*
231700960912     C                   END                                                    --- 02 ---
231800070216     C*
231900960912     C                   END                                                    --- 01 ---
232000990715      *
232100990715      * SE NON IMPOSTATO SEGNACOLLO FORZO 1 E PULISCO IL 1° ELEMENTO
232200990715     C     XY            IFEQ      *ZEROS
232300990715     C                   Z-ADD     1             XY
232400990715     C                   CLEAR                   SGN(XY)
232500990715     C                   END
232600990715      *
232700960912     C                   ENDSR
232800960912     C*----------------------------------------------------------------
232900960912     C*? REPLNP - Reperisce lnp dal nr.segnacollo passato
233000960912     C*----------------------------------------------------------------
233100960912     C     REPLNP        BEGSR
233200960912     C*
233300960911     C*  se viene passata lnp. estraggo lo sottostringa lunga 3 chr
233400960911     C*  (a partire dalla posizione iniziale indicata in tabella)
233500960911     C*  dal numero segnacollo passatomi dal partner
233600960911     C                   Z-ADD     §PULP1        WP                2 0
233700070216     C     3             SUBST     x30(X):WP     WLNP              3
233800960911     C*  controllo che la lnp calcolata sia numerica
233900960911     C                   TESTN                   WLNP                 98
234000960911     C*  se è numerica la carico
234100960911     C     *IN98         IFEQ      '1'
234200960911     C                   MOVE      WLNP          SGNLNP
234300960911     C                   ELSE
234400960911     C*  se non è numerica stampo errore
234500960912     C                   MOVEL     'S'           WERRO             1
234600960911     C                   Z-ADD     25            WINDER            3 0
234700960912     C                   MOVEL     WLNP          WVALOR           35
234800960912     C                   SETON                                        05
234900960911     C                   EXSR      STPERR
235000960912     C                   GOTO      FINLNP
235100960911     C                   END
235200020909     C*  se la lnp non è uguale a quella calcolata per EDiVAB
235300020909     C*  lo segnalo e prendo per buona la lnp del EDiVAB
235400040302     C     SGNLNP        IFNE      VABLNP
235500960912     C                   MOVEL     'S'           WERRO             1
235600960912     C                   Z-ADD     26            WINDER            3 0
235700960912     C                   MOVEL     SGNLNP        WCOMO5            5
235800960912     C                   MOVE      '-'           WCOMO5
235900040302     C                   MOVE      VABLNP        WCOMO9            9
236000960912     C                   MOVEL     WCOMO5        WCOMO9            9
236100960912     C                   MOVEL     WCOMO9        WVALOR           35
236200960912     C                   SETON                                        05
236300960912     C                   EXSR      STPERR
236400960912     C                   END
236500960725     C*
236600960912     C     FINLNP        ENDSR
236700960912     C*----------------------------------------------------------------
236800960912     C*? REPLNA - Reperisce lna dal nr.segnacollo passato
236900960912     C*----------------------------------------------------------------
237000960912     C     REPLNA        BEGSR
237100960912     C*
237200960912     C*  se viene passata lna. estraggo lo sottostringa lunga 3 chr
237300960912     C*  (a partire dalla posizione iniziale indicata in tabella)
237400960912     C*  dal numero segnacollo passatomi dal partner
237500960912     C                   Z-ADD     §PULA1        WP                2 0
237600070216     C     3             SUBST     x30(X):WP     WLNA              3
237700960912     C*  controllo che la lna calcolata sia numerica
237800960912     C                   TESTN                   WLNA                 98
237900960912     C*  se è numerica la carico
238000960912     C     *IN98         IFEQ      '1'
238100960912     C                   MOVE      WLNA          SGNLNA
238200960912     C                   ELSE
238300960912     C*  se non è numerica stampo errore
238400960912     C                   Z-ADD     27            WINDER            3 0
238500960912     C                   MOVEL     WLNA          WVALOR           35
238600960912     C                   SETON                                        05
238700960912     C                   EXSR      STPERR
238800960912     C                   MOVEL     'S'           WERRO             1
238900960912     C                   GOTO      FINLNA
239000960912     C                   END
239100960912     C*
239200960912     C     FINLNA        ENDSR
239300960912     C*----------------------------------------------------------------
239400960912     C*? REPNRS - Reperisce numero serie
239500960912     C*----------------------------------------------------------------
239600960912     C     REPNRS        BEGSR
239700960912     C*
239800960912     C*  se viene passata nrs. estraggo lo sottostringa lunga 2 chr
239900960912     C*  (a partire dalla posizione iniziale indicata in tabella)
240000960912     C*  dal numero segnacollo passatomi dal partner
240100960912     C                   Z-ADD     §PUNR1        WP                2 0
240200070216     C     2             SUBST     x30(X):WP     WNRS              2
240300960912     C*  controllo che il nrs calcolata sia numerico
240400960912     C                   TESTN                   WNRS                 98
240500960912     C*  se è numerica la carico
240600960912     C     *IN98         IFEQ      '1'
240700960912     C                   MOVE      WNRS          SGNNRS
240800960912     C                   ELSE
240900960912     C*  se non è numerico stampo errore
241000960912     C                   Z-ADD     28            WINDER            3 0
241100960912     C                   MOVEL     WNRS          WVALOR           35
241200960912     C                   SETON                                        05
241300960912     C                   EXSR      STPERR
241400960912     C                   MOVEL     'S'           WERRO             1
241500960912     C                   GOTO      FINNRS
241600960912     C                   END
241700020909     C*  se il nrs non è uguale a quella calcolata per EDiVAB
241800960912     C*  lo segnalo e prendo per buona il nrs del segnacollo
241900960912     C     SGNNRS        IFNE      VABNRS
242000960912     C                   Z-ADD     29            WINDER            3 0
242100960912     C                   MOVEL     SGNNRS        WCOMO4            4
242200960912     C                   MOVE      '-'           WCOMO5
242300960912     C                   MOVE      VABNRS        WCOMO7            8
242400960912     C                   MOVEL     WCOMO4        WCOMO7            8
242500960912     C                   MOVEL     WCOMO7        WVALOR           35
242600960912     C                   MOVEL     'S'           WERRO             1
242700960912     C                   SETON                                        05
242800960912     C                   EXSR      STPERR
242900960912     C                   END
243000960912     C*
243100960912     C     FINNRS        ENDSR
243200960912     C*----------------------------------------------------------------
243300960912     C*? REPZNC - Reperisce zona consegna
243400960912     C*----------------------------------------------------------------
243500960912     C     REPZNC        BEGSR
243600960912     C*
243700960912     C*  se viene passata la zona estraggo lo sottostringa di 2 chr
243800960912     C*  (a partire dalla posizione iniziale indicata in tabella)
243900960912     C*  dal numero segnacollo passatomi dal partner
244000960912     C                   Z-ADD     §PUZN1        WP                2 0
244100070216     C     2             SUBST     x30(X):WP     WZNC              2
244200960912     C*  controllo che la zona calcolata sia numerica
244300960912     C                   TESTN                   WZNC                 98
244400960912     C*  se è numerica la carico
244500960912     C     *IN98         IFEQ      '1'
244600960912     C                   MOVE      WZNC          SGNZNC
244700960912     C                   ELSE
244800960912     C*  se non è numerica stampo errore
244900960912     C                   MOVEL     'S'           WERRO             1
245000960912     C                   Z-ADD     30            WINDER            3 0
245100960912     C                   MOVEL     WZNC          WVALOR           35
245200960912     C                   SETON                                        05
245300960912     C                   EXSR      STPERR
245400960912     C                   GOTO      FINZNC
245500960912     C                   END
245600960912     C*
245700960912     C     FINZNC        ENDSR
245800960912     C*----------------------------------------------------------------
245900960912     C*? REPSGN - Reperisce numero segnacollo
246000960912     C*----------------------------------------------------------------
246100960912     C     REPSGN        BEGSR
246200960912     C*
246300960912     C*  se viene passata nr.segnacollo estraggo sottostringa
246400960912     C*  lunga 7 chr (a partire dalla posizione iniziale
246500960912     C*  indicata in tabella)
246600960912     C*  dal numero segnacollo passatomi dal partner
246700960912     C                   Z-ADD     §PUSG1        WP                2 0
246800070216     C     7             SUBST     x30(X):WP     WSGN              7
246900960912     C*  controllo che il nr.segnacollo calcolato sia numerico
247000960912     C                   TESTN                   WSGN                 98
247100960912     C*  se è numerico lo carico
247200960912     C     *IN98         IFEQ      '1'
247300960912     C                   MOVE      WSGN          SGNNSC
247400960912     C                   ELSE
247500960912     C*  se non è numerica stampo errore
247600960912     C                   Z-ADD     31            WINDER            3 0
247700960912     C                   MOVEL     WSGN          WVALOR           35
247800960912     C                   SETON                                        05
247900960912     C                   MOVEL     'S'           WERRO             1
248000960912     C                   EXSR      STPERR
248100960912     C                   END
248200960912     C*
248300960912     C                   ENDSR
248400960828     C*----------------------------------------------------------------
248500960828     C*? CNVCAP - Routine x allineamento a destra CAP destinatario
248600960828     C*----------------------------------------------------------------
248700960828     C     CNVCAP        BEGSR
248800960828     C*
248900960828     C                   MOVEL     *BLANKS       WCAP              9
249000961104     C     DD3251        IFNE      '0'
249100960828     C     '  '          SCAN      DD3251        LENGHT            2 0
249200960828     C                   SUB       1             LENGHT
249300960828     C     LENGHT        IFLE      5
249400960904     C     LENGHT        ANDGT     0
249500960828     C                   Z-ADD     LENGHT        T                 2 0
249600960828     C                   Z-ADD     5             S                 2 0
249700960828     C                   MOVEA     DD3251        CAP9
249800960828     C                   MOVEL     *ZEROS        CAP5
249900960828     C                   DO        LENGHT
250000960828     C                   MOVE      CAP9(T)       CAP5(S)
250100960828     C                   SUB       1             S
250200960828     C                   SUB       1             T
250300960828     C                   END
250400960926     C                   MOVEA     CAP5          WCAP5             5
250500961017     C     WCAP5         IFEQ      '0'
250600960926     C                   MOVEL     *BLANKS       WCAP
250700960926     C                   ELSE
250800960926     C                   MOVEA     CAP5          WCAP
250900960926     C                   END
251000960828     C*
251100960828     C                   ELSE
251200960828     C                   MOVEL     DD3251        WCAP
251300960828     C                   END
251400961017     C*  Se il CAP è diverso da blanks calcolo anche cap fittizio
251500961017     C     WCAP          IFNE      *BLANKS
251600961017     C     WFLCPF        ANDNE     0
251700961017     C                   MOVEL     WFLCPF        WELE              1 0
251800961017     C                   MOVE      WFLCPF        WPOS              1 0
251900961017     C                   MOVEL     *BLANK        WCPF
252000961017     C     WELE          SUBST     WCAP:WPOS     WCPF              9
252100961017     C                   ELSE
252200961017     C                   MOVEL     *BLANKS       WCPF
252300961017     C                   END
252400961104     C                   END
252500960828     C*
252600960828     C                   ENDSR
252700000125      *----------------------------------------------------------------
252800000125      *? AZZCMP - Azzero campi di importazione dati
252900000125      *----------------------------------------------------------------
253000960527     C     AZZCMP        BEGSR
253100000125      *
253200000125      * Pulisco ds impostazione dati
253300000120     C                   CLEAR                   EDTT00DS
253400000120     C                   CLEAR                   EDTT05DS
253500000120     C                   CLEAR                   EDTT10DS
253600000120     C                   CLEAR                   EDTT15DS
253700000120     C                   CLEAR                   EDTT20DS
253800000120     C                   CLEAR                   EDTT25DS
253900000120     C                   CLEAR                   EDTT30DS
254000000120     C                   CLEAR                   EDTD00DS
254100000120     C                   CLEAR                   EDTD05DS
254200000120     C                   CLEAR                   EDTD10DS
254300000120     C                   CLEAR                   EDTD15DS
254400000120     C                   CLEAR                   EDTD20DS
254500000120     C                   CLEAR                   EDTD25DS
254600000120     C                   CLEAR                   EDTD30DS
254700960527     C                   CLEAR                   WTT10
254800960527     C                   CLEAR                   WTT20
254900960527     C                   CLEAR                   WTT20D
255000960527     C                   CLEAR                   WTT25
255100960527     C                   CLEAR                   WTD00
255200960527     C                   CLEAR                   WTD10
255300960527     C                   CLEAR                   WTD15
255400960527     C* Pulisco schiere
255500960527     C                   CLEAR                   T05
255600960527     C                   CLEAR                   T10
255700960527     C                   CLEAR                   T20
255800960527     C                   CLEAR                   T20D
255900960527     C                   CLEAR                   T25
256000960527     C                   CLEAR                   D00
256100960527     C                   CLEAR                   D05
256200960527     C                   CLEAR                   D05A
256300960527     C                   CLEAR                   D10
256400960527     C                   CLEAR                   D15
256500070216     C                   CLEAR                   D30_1
256600070216     C                   CLEAR                   D30_2
256700070216     C                   CLEAR                   X30
256800031211     C*
256900960527     C                   ENDSR
257000000125      *----------------------------------------------------------------
257100000125      *? AZZBOL - Azzera dati bolla
257200000125      *----------------------------------------------------------------
257300960524     C     AZZBOL        BEGSR
257400000125      *
257500000125      * Pulisco campi del file
257600020909     C                   CLEAR                   EDiVAB00
257700020909     C                   CLEAR                   EDiVAD00
257800961010     C                   MOVEL     'N'           WSQUAD            1            Squad.nr.colli
257900970612     C                   MOVEL     'N'           WGID99            1
258000000125     C                   clear                   XY                5 0
258100000125     C                   clear                   §§                5 0
258200000125     C                   clear                   WS1496            5 0
258300000125     C                   clear                   WNSP             35
258400000125     C                   clear                   WNOT1            35
258500000125     C                   clear                   WNOT2            35
258600070219     C                   Z-ADD     *ZEROS        PVLB
258700960726     C                   MOVEA     *HIVAL        SGN
258800960809     C                   CLEAR                   SGT
258900960809     C                   Z-ADD     0             XW
259000961122     C*  Azzero codice cliente - tariffa
259100000125     C                   clear                   WCLKSC
259200000125     C                   clear                   WCLTAR
259300040224     C                   clear                   WCLLNP
259400040303     C                   clear                   WCLNRS
259500040224     C                   clear                   WCLCTM
259600040224     C                   clear                   WCLBS1
259700050223     C                   clear                   WCLfnsp                        *blk/S
259800070219     C*  Campi di work
259900070219     c                   clear                   wVabTIC
260000970520     C* Azzero schiera per memorizzazione FTX + qualificatore
260100970520     C                   Z-ADD     0             FF                3 0
260200970520     C                   MOVEA     *BLANKS       FTX
260300970520     C                   MOVEA     *BLANKS       QUA
260400070219      *
260500070219     C* pulisce campi di work
260600070219     c                   clear                   watrde           35
260700070219     c                   clear                   wattel           25
260800070219     C                   clear                   WAGE             35
260900070219     C                   clear                   WFF              35
261000070219     C                   clear                   WCU              35
261100070219     C                   clear                   WRMN              3
261200960528     C*
261300960527     C                   ENDSR
261400960628     C*----------------------------------------------------------------
261500960628     C*? AZZMSG - Azzera dati messaggio
261600960628     C*----------------------------------------------------------------
261700960628     C     AZZMSG        BEGSR
261800960628     C*
261900960628     C* Pulisco dati di work x foglio viaggio
262000960712     C                   MOVEL     'N'           WSTPTO            1
262100960628     C                   MOVEL     *BLANKS       WNOT             70
262200960628     C                   MOVEL     *BLANKS       WNUMFV           35
262300960628     C                   MOVEL     *BLANKS       WNRCMR           35
262400960628     C                   MOVEL     *BLANKS       WLOCPA           25
262500960628     C                   MOVEL     *BLANKS       WLOCAR           25
262600960628     C                   Z-ADD     0             WDATPA            8 0
262700960628     C                   Z-ADD     0             WHHPA             4 0
262800960628     C                   Z-ADD     0             WDATFV            8 0
262900960628     C                   Z-ADD     0             WHHNFV            4 0
263000960628     C                   Z-ADD     0             WDTCMR            8 0
263100960628     C                   Z-ADD     0             WHHCMR            4 0
263200960628     C                   Z-ADD     0             WTOTSP           16 0
263300960628     C                   Z-ADD     0             WTOTCO           16 0
263400960628     C                   Z-ADD     0             WTOTPL           16 0
263500960628     C                   Z-ADD     0             WTOTPN           16 0
263600070219      *
263700960703     C                   Z-ADD     0             WTOSPC           16 0
263800960703     C                   Z-ADD     0             WTOCOC           16 0
263900960703     C                   Z-ADD     0             WTOPLC           16 0          ???
264000960703     C                   Z-ADD     0             WTOPNC           16 0          ???
264100070219      * pulisce gli RFF+FF di testata
264200070219     C                   clear                   $CLKSC            7 0
264300070219     C                   clear                   $CLTAR            3
264400070219     C                   clear                   $CLLNP            3 0
264500070219     C                   clear                   $CLNRS            2 0
264600070219     C                   clear                   $CLCTM            2
264700070219     C                   clear                   $CLBS1            1
264800070219     C                   clear                   $CLfnsp           1            *blk/S
264900960927     C*  Inizializzo variabile new documento
265000961204     C                   MOVEL     'S'           WSTPTE            1
265100961204     C                   Z-ADD     0             KCL
265200961204     C                   Z-ADD     0             WW                3 0
265300070219     ** Inizializza campo di work
265400070219     c                   move      'N'           WGID99
265500070220     c                   clear                   UNB_Generico
265600070220     c                   clear                   UNB_Parz
265700960628     C*
265800960628     C                   ENDSR
265900960527     C*----------------------------------------------------------------
266000020909     C*? UPDVAB - AGGIORNO EDiVAB: Scittura dati
266100960527     C*----------------------------------------------------------------
266200960527     C     UPDVAB        BEGSR
266300070219      *
266400070219     C*  Se non è stato impostato il codice bolla lo imposto io
266500070219     C     VABCBO        IFEQ      *BLANKS
266600070219     C     VABCAS        IFGT      0
266700070219     C                   MOVEL     '4'           VABCBO                         + C/Ass
266800070219     C                   ELSE
266900070219     C                   MOVEL     '1'           VABCBO                         Senza C/Ass.
267000070219     C                   END
267100070219     C                   END
267200990714      *
267300961104     C     VABNOT        IFEQ      *BLANKS
267400961104     C                   MOVEL     WNOT1         VABNOT
267500961104     C                   MOVEL     WNOT2         VABNT2
267600961104     C                   ELSE
267700961107     C                   MOVEL     WNOT1         VABNT2
267800961104     C                   END
267900960530     C*  Attribuisco anno spedizione
268000960530     C     WDATPA        IFGT      0                                            anno sped.
268100960530     C                   MOVEL     WDATPA        VABAAS
268200960530     C                   ELSE
268300960530     C     WDATFV        IFGT      0
268400960530     C                   MOVEL     WDATFV        VABAAS                         anno sped.
268500960530     C                   END
268600960530     C                   END
268700980219      *
268800980219      *  Controllo dettaglio segnacolli
268900001031     C     §1BF12        IFEQ      'E'                                          Segnac. EUROEXPRESS
269000980219     C                   EXSR      CTRSGE
269100980219     C                   ELSE
269200980219     C     §1BFG1        IFEQ      'S'                                          Cli.segnacolla
269300960731     C     §1BFG7        OREQ      'S'                                          Cli.segnacolla
269400960725     C***        WNSGER    ANDEQ'S'                        nr.sgn. errato
269500960731     C                   EXSR      CTRSGN
269600960731     C                   END
269700980219     C                   END
269800980219      *
269900000125      *  Imposto linea partenza
270000000125     C                   eval      VABLNP = WVABLNP
270100040224      *
270200040224      *  Controllo se devo variare il codice trattamento merce
270300040227     C                   eval      VABCTM = WVABctm
270400050223      *
270500050223      * Controllo se deve mantenere il numero bolla passato dal messaggio
270600050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
270700050224     c                   clear                   mantiene_nsp      1            *blk/'S'
270800050224     c                   eval      mantiene_nsp = WVABfnsp                      *blk/'S'
270900000125      *
271000050224      * (Se esistono delle Particolarità sul cliente prioritariamente prende
271100050224      *  quelle del dettaglio e se non ci sono prende quelle di testata se ci sono.)
271200050224      *
271300000125      *  Imposto codice cliente
271400000125     c                   Select
271500040224      *
271600000125      *  Specificato codice da qualificatore FF (TD15)
271700000125     C                   When        WCLKSC <> *zeros
271800000125     C                   Z-ADD     WCLKSC        VABCCM
271900000125     C                   MOVEL     WCLTAR        VABCTR
272000040303      * forza LNP/CTM/NRS
272100040224     c                   if        WclLNP <> 0
272200040224     C                   eval      VABLNP = WclLNP
272300040224     c                   end
272400040303     c                   if        WclNRS <> 0
272500040303     C                   eval      VABNRS = WclNRS
272600040227     c                   end
272700040224     c                   if        WclCTM <> *blank
272800040224     C                   eval      VABCTM = WclCTM
272900040224     c                   end
273000050223      *
273100050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
273200050224     c                   if        WCLfnsp <> *blank                            *blk/'S'
273300050224     c                   eval      mantiene_nsp = WCLfnsp                       *blk/'S'
273400050223     c                   end
273500050223      *
273600000125      *  Specificato codice da qualificatore FF (TT15)
273700000125     C                   When        $CLKSC <> *zeros
273800000125     C                   Z-ADD     $CLKSC        VABCCM
273900000125     C                   MOVEL     $CLTAR        VABCTR
274000040303      * forza LNP/CTM/NRS
274100040224     c                   if        $clLNP <> 0
274200040224     C                   eval      VABLNP = $clLNP
274300040224     c                   end
274400040303     c                   if        $clNRS <> 0
274500040303     C                   eval      VABNRS = $clNRS
274600040227     c                   end
274700040224     c                   if        $clCTM <> *blank
274800040224     C                   eval      VABCTM = $clCTM
274900040224     c                   end
275000050223      *
275100050223      * Deve mantenere il nr.sped.del Partner/Cliente ?  (*blk/S)
275200050224     c                   if        $CLfnsp <> *blank                            *blk/'S'
275300050224     c                   eval      mantiene_nsp = $CLfnsp                       *blk/'S'
275400050223     c                   end
275500040224      *
275600000125      *  Imposto dati da tabella - PT-
275700000125     C                   Other
275800040224      *
275900000125     C                   MOVEL     §PTKSC        VABCCM
276000960527     C                   MOVEL     §PTCTR        VABCTR
276100000125      *
276200000125     C                   Endsl
276300040225      *
276400040225      *  Prendo comunque dalla PT la LNP come flag di gestione P.O.
276500040225      *    o dalla relativa CL
276600040225     C                   MOVEL     VABlnp        VABfgs
276700000126      *
276800050223      *  Deve Controllare se mantenere o meno il numero Spedizione passato da EDI
276900050223      *   se richiesto in tabella PT/CL e se veramente lungo 7 e numerico
277000050224      *       CONTROLLO di 7 numerico........ è stato fatto precedentemente
277100050224      *   caricando il VABRMN anche prendendolo eventualmente dall'AGE quindi se
277200050224      *   VABRMN contiene un numero questo sarà il numero della spedizione passato.
277300050224      *                A T T E N Z I O N E
277400050224      * << Se però il numero è più grande di 7 cifre non deve eseguirlo e forza blank >>
277500050224     c                   if        mantiene_nsp = 'S' and VABRMN > 0            *blk/'S' - 1234567
277600050224     c                   z-add     vabRMN        VABNSP                         *NUM.SPED.da Cliente
277700050224     c                   else
277800050112      *
277900050112     C*  Se non è previsto che il cliente attribuisca nr.sped.
278000050112     C*  lo attribuisco io
278100050112      **              **------------------**
278200050112     c                   exsr      repnum
278300050112      **              **------------------**
278400050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
278500050224     c                   end
278600050224      *
278700960527     C*  Altrimenti attribuisco data del giorno
278800960527     C     VABMGS        IFEQ      0
278900960527     C                   MOVEL     WOGGI         VABAAS                         data sped.
279000960527     C                   MOVE      WOGGI         VABMGS                         = oggi
279100960527     C                   END
279200020909     C*  Eseguo posizionamento su EDiVAB
279300960528     C                   Z-ADD     VABAAS        KAAS
279400040224     C                   Z-ADD     VABlnp        KLNP
279500960528     C                   Z-ADD     VABNRS        KNRS
279600960528     C                   Z-ADD     VABNSP        KNSP
279700960528     C                   MOVEL     SAVBOL        SALVA
279800020909     C     KVAB1         CHAIN     EDiVAB1L                           30
279900960528     C                   MOVEL     SALVA         SAVBOL
280000960924     C*  Imposto dati CMR
280100960924     C                   Z-ADD     WPROG         VABCNT
280200090204     **
280300090204      *** Attenzione se si deve trattare il VAX con il CMR
280400090204      *** il campo VABCNT deve essere sempre impostato a (1)
280500090204     C     §puWRK        ifEQ      'S'
280600090204     C     §puNSP        andEQ     'S'
280700090204     C                   Z-ADD     1             VABCNT
280800090204     c                   end
280900090204     **
281000960926     C                   Z-ADD     WDTPRE        VABDTS
281100960926     C                   Z-ADD     WHMPRE        VABHMS
281200960926     C     §PTCMR        IFEQ      'S'
281300960924     C                   Z-ADD     WDTCMR        VABDCM
281400960924     C                   MOVEL     WNRCMR        VABCMR
281500960926     C                   ELSE
281600960926     C                   Z-ADD     WDATFV        VABDCM
281700960926     C                   MOVEL     WNUMFV        VABCMR
281800960926     C                   END
281900960531     C*  Se non è indicato il nome del mittente = Partner mittente
282000960531     C     VABRMO        IFEQ      *BLANKS
282100960531     C                   MOVEL     ACORAG        VABRMO
282200960531     C                   MOVEL     INDCAP        VABCMO
282300960531     C     INDCAE        IFNE      *BLANKS
282400960531     C                   MOVEL     INDCAE        VABCMO
282500960531     C                   END
282600960820     C     INDSTA        IFNE      *BLANKS
282700960820     C                   MOVEL     INDSTA        VABNMO
282800960820     C                   END
282900960531     C                   END
283000960527     C*  Se non viene attribuito ne segnacolli ne nr.sped. serie = 00
283100960527     C     §1BFG1        IFEQ      'N'
283200960731     C     §1BFG7        ANDEQ     'N'
283300960527     C     §1BFG2        ANDEQ     'N'
283400960527     C                   END
283500960527     C*  Imposto segancollo da - a
283600960731     C     §1BFG1        IFEQ      'S'
283700960731     C     §1BFG7        ANDEQ     'N'
283800001031     C     §1BF12        ANDNE     'E'
283900990714      *
284000990810     C     XY            IFEQ      *ZEROS
284100990810     C                   CLEAR                   VABNCD
284200990810     C                   CLEAR                   VABNCA
284300990810     C                   ELSE
284400990714     C                   Z-ADD     SGN(1)        VABNCD
284500990714     C                   Z-ADD     SGN(XY)       VABNCA
284600990810     C                   ENDIF
284700990810     C*
284800990714     C                   END
284900960531     C*
285000960904     C*  Se cap mittente originale a blank abblenco nazione
285100960904     C     VABCMO        IFEQ      *BLANKS
285200960904     C                   MOVEL     *BLANKS       VABNMO
285300960904     C                   END
285400050301     C*
285500050301      *? ------------------------------------------------------------------------
285600050301      *?  PARTICOLARITA' sui Clienti Partners x scrittura VAB  (C H I O D I)
285700050301      *? ------------------------------------------------------------------------
285800050301     C*
285900050301     c                   exsr      Chiodi_VAB
286000050301     C*
286100050224      *? ------------------------------------------------------------------------
286200010621     C*
286300020909     C   30              WRITE     EDiVAB00
286400020909     C  N30              UPDATE    EDiVAB00
286500060317     c                   clear                   wri_vabtic        1
286600010621     C*
286700031211     C*  Scrive il riferimento Telefonico e il nome x la consegna al destinatario
286800031211     C                   EXSR      WRTVAT_Rif
286900031211     C*
287000980219      * Stampo ns.numero spedizione
287100040607     c                   If        In_Stampa ='S'
287200960712     C   OA              EXCEPT    TEST1
287300960712     C                   EXCEPT    LETVET
287400040607     c                   end
287500980219      *
287600020909      *  Se previsti segnacolli EUROEXPRESS scrivo EDiVAT
287700001031     C     §1BF12        IFEQ      'E'
287800980219     C                   EXSR      WRTVAT
287900980219     C                   ELSE
288000020909      *  Se il trattamento merce prevede che il cliente segnacolli scrivo EDiVAD
288100960731     C     §1BFG7        IFEQ      'S'
288200960731     C     §1BFG1        OREQ      'S'
288300960527     C                   EXSR      WRTVAD
288400960527     C                   END
288500980219     C                   END
288600960527     C* Reimposto codice trattamento merce cliente
288700960527     C                   MOVEL     WSVTRM        DS1B
288800031219     C**
288900960626     C* Do errore se previsto CMR ma non c'è
289000960827     C     §PTCMR        IFNE      ' '
289100960731     C                   EXSR      MEMCMR
289200960731     C                   END
289300960731     C* Do errore se previsto AFB ma non c'è
289400960827     C     §PTNFV        IFNE      ' '
289500960731     C                   EXSR      MEMAFB
289600960731     C                   END
289700961010     C*  Se il cliente vuole il ritorno delle date di consegna
289800961010     C*  è c'è una squdratura fra i segnacolli e il numero
289900961010     C*  dei colli che ci ha passato scrivo EDSUM
290000961010     C     §PUDTA        IFEQ      'S'
290100961010     C     WSQUAD        ANDEQ     'S'
290200961010     C                   EXSR      WRTSUM
290300961010     C                   END
290400960626     C*
290500960527     C                   ENDSR
290600960731     C*----------------------------------------------------------------
290700050301     C*? CHIODI VAB  prima della scrittura del VAB Particolarità su Clienti/Partners
290800050301     C*----------------------------------------------------------------
290900050301     C     Chiodi_VAB    BEGSR
291000050307      *
291100050301      *? ------------------------------------------------------------------------
291200050301      *? ATTENZIONE: (Chiodo E.Arden)
291300050301      *?  x E.Arden o Unilever           501790 e 501791
291400050301      *? Se è un cliente e non un Partner deve togliere la Ragione Sociale
291500050301      *? originale e mettere un punto al posto della RagSoc poichè non
291600050301      *?  deve essere vuoto il campo.
291700050301      *? ------------------------------------------------------------------------
291800050301     C     VABRMO        IFne      *blank
291900050301     C     §PUTPC        andeq     'C'
292000050301     C     §PTKSC        ifeq      501790
292100050301     C     §PTKSC        oreq      501791
292200050301     C                   MOVEL     *BLANKS       VABRMO
292300050301     C                   MOVEL     '.'           VABRMO
292400050301     c                   end
292500050301     C                   END
292600050301      *
292700050301      *? ------------------------------------------------------------------------
292800050301      *? Chiodo da togliere a gennaio 2005 :
292900050301      *? attenzione solo x ARVATO non deve essere importato l'IMPORTO ASSICURATO
293000050301      *? come da accordi commerciali anche se lo inviano sul tracciato EDI
293100050301      *? ED ANCHE IL VOLUME dal 1/3/2005
293200050301      *? ------------------------------------------------------------------------
293300050301     C     §PTKSC        ifeq      932337
293400050301     C     VABCCM        oreq      52981
293500050301     C     VABCCM        oreq      932353
293600050301     c     VABCCM        oreq      932461
293700050301     C                   clear                   VABIAS                         IMPORTO
293800050301     C                   clear                   VABVAS                         ASSICURATO
293900050301     C                   clear                   VABVLB                         VOLUME BOLLETTATO
294000050301     c                   end
294100050301     C*
294200070528      *? ------------------------------------------------------------------------
294300070528      *? Chiodo x SFS come da richiesta del 25/5/2007:
294400070528      *? mettere sui 2 campi note:
294500070528      *?      not: DDT SUL COLLO
294600070528      *?      nt2: NON CONSEGNARE IN CASO DI AVARIA
294700070528      *? ------------------------------------------------------------------------
294800070528     C     §PTKSC        ifeq      1160950
294900070528     C     VABNOT        andeq     *blank
295000070528     C     VABNT2        andeq     *blank
295100070528     C                   eval      vabNOT = 'DDT SUL COLLO'
295200070528     C                   eval      vabNT2 = 'NON CONSEGNARE IN CASO DI AVARIA'
295300070528     c                   end
295400070528     C*
295500070924      *? ------------------------------------------------------------------------
295600070924      *? Chiodo x SEUR cliente REBELIO  del 24/9/2007:
295700070924      *?  il cliente se ha il COD vuole sempre in contanti mentre
295800070924      *?   su IFCSUM manda sempre il FTX+PAI+++CO' che noi tradurremmo come OM.
295900070924      *? Chiodo Richiesto dalla Villa con Mail il 24/9/2007
296000070924      *? ------------------------------------------------------------------------
296100070924     C     §PTKSC        ifeq      433048
296200070924     C     VABCCM        andeq     433972
296300070924     C     VABTIC        andne     *blank
296400070924     C                   eval      vabTIC = '  '
296500070924     c                   end
296600070924     C*
296700080410     C*  Per non bloccare la bollettazione
296800080410     C*  a fronte di pesi troppo piccoli da lasciare a 0 il peso
296900080410     C*  o se il peso non è presente... imposto fisso il minimo 0,1
297000080410     c                   if        vabpkb = 0
297100080410     c                   z-add     0,1           vabpkb
297200080410     c                   end
297300080410     C*
297400090401     C*
297500090401      *? ------------------------------------------------------------------------
297600090401      *? Chiodo x AZKAR BISA richiesta  del 01/4/2009: dalla VILLA con mail di Azkar
297700090401      *?  tutti i contrassegni devono essere intestati come "OM"
297800090401      *? Chiodo Richiesto dalla Villa con Mail il 01/4/2009
297900090401      *? ------------------------------------------------------------------------
298000090401     C     §PTKSC        ifeq      22349
298100090401     C     VABCCM        andeq     22349
298200090401     C     VABTIC        ifne      *blank
298300090401     C     VABCAS        orgt      *zeros
298400090401     C                   eval      vabTIC = 'OM'
298500090401     c                   end
298600090401     c                   end
298700080410     C*
298800050301     C                   ENDSR
298900050301     C*----------------------------------------------------------------
299000960731     C*? MEMCMR - MEMORIZZO NUMERO CMR
299100960731     C*----------------------------------------------------------------
299200960731     C     MEMCMR        BEGSR
299300960731     C*
299400960731     C     WNRCMR        IFEQ      *BLANKS
299500960731     C                   END
299600960731     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
299700960827     C     §PTCM1        IFEQ      'S'
299800960819     C                   CLEAR                   EDRDE000
299900960731     C                   MOVEL     'TD1154'      KNMC
300000960731     C                   Z-ADD     1             KNRP
300100961029     C                   MOVEL     WKSC          KLNP1
300200960731     C     KRDE          CHAIN     EDRDE01L                           31
300300960731     C     *IN31         IFEQ      '1'
300400960731     C                   Z-ADD     VABAAS        RDEAAS
300500040302     C                   Z-ADD     VABLNP        RDELNP
300600960731     C                   Z-ADD     VABNRS        RDENRS
300700960731     C                   Z-ADD     VABNSP        RDENSP
300800960731     C                   MOVEL     'TD1154'      RDENMC
300900960731     C                   Z-ADD     1             RDENRP
301000960731     C                   MOVEL     WNRCMR        RDEVAL
301100960731     C                   WRITE     EDRDE000
301200960731     C                   END
301300960731     C*  Memorizzo qualificatore CMR
301400960819     C                   CLEAR                   EDRDE000
301500960731     C                   MOVEL     'TD1153'      KNMC
301600960731     C                   Z-ADD     1             KNRP
301700961029     C                   MOVEL     WKSC          KLNP1
301800960731     C     KRDE          CHAIN     EDRDE01L                           31
301900960731     C     *IN31         IFEQ      '1'
302000960731     C                   Z-ADD     VABAAS        RDEAAS
302100040302     C                   Z-ADD     VABLNP        RDELNP
302200960731     C                   Z-ADD     VABNRS        RDENRS
302300960731     C                   Z-ADD     VABNSP        RDENSP
302400960731     C                   Z-ADD     1             RDENRP
302500960731     C                   MOVEL     'TD1153'      RDENMC
302600960731     C                   MOVEL     'CMR'         RDEVAL
302700960731     C                   WRITE     EDRDE000
302800960731     C                   END
302900960731     C*  Memorizzo la data
303000960819     C                   CLEAR                   EDRDE000
303100960731     C                   MOVEL     'TD2380'      KNMC
303200960731     C                   Z-ADD     1             KNRP
303300961029     C                   MOVEL     WKSC          KLNP1
303400960731     C     KRDE          CHAIN     EDRDE01L                           31
303500960731     C     *IN31         IFEQ      '1'
303600960731     C                   Z-ADD     VABAAS        RDEAAS
303700040302     C                   Z-ADD     VABLNP        RDELNP
303800960731     C                   Z-ADD     VABNRS        RDENRS
303900960731     C                   Z-ADD     VABNSP        RDENSP
304000960731     C                   Z-ADD     1             RDENRP
304100960731     C                   MOVEL     'TD2380'      RDENMC
304200960731     C                   MOVEL     WDTCMR        RDEVAL
304300960731     C                   WRITE     EDRDE000
304400960731     C                   END
304500960731     C*
304600960827     C                   END                                                    §PTCM1 = 'S'
304700960731     C*
304800960731     C                   ENDSR
304900960731     C*----------------------------------------------------------------
305000960731     C*? MEMAFB - MEMORIZZO NUMERO AFB
305100960731     C*----------------------------------------------------------------
305200960731     C     MEMAFB        BEGSR
305300960731     C*
305400960731     C     WNUMFV        IFEQ      *BLANKS
305500960731     C                   Z-ADD     23            WINDER
305600960731     C                   MOVEL     *BLANKS       WVALOR
305700960731     C                   SETON                                        05
305800960731     C                   EXSR      STPERR
305900960731     C                   END
306000960731     C* Eseguo scrittura EDRDE del numero CMR se previsto da tab.PT
306100960827     C     §PTNF1        IFEQ      'S'
306200960819     C                   CLEAR                   EDRDE000
306300961029     C                   MOVEL     WKSC          KLNP1
306400960731     C                   MOVEL     'TD1154'      KNMC
306500960731     C                   Z-ADD     1             KNRP
306600960731     C     KRDE          CHAIN     EDRDE01L                           31
306700960731     C     *IN31         IFEQ      '1'
306800960731     C                   Z-ADD     VABAAS        RDEAAS
306900040302     C                   Z-ADD     VABLNP        RDELNP
307000960731     C                   Z-ADD     VABNRS        RDENRS
307100960731     C                   Z-ADD     VABNSP        RDENSP
307200960731     C                   MOVEL     'TD1154'      RDENMC
307300960827     C     §PTCM1        IFNE      'S'
307400960731     C     WNRCMR        OREQ      *BLANKS
307500960731     C                   Z-ADD     1             RDENRP
307600960731     C                   ELSE
307700960731     C                   Z-ADD     2             RDENRP
307800960731     C                   END
307900960731     C                   MOVEL     WNUMFV        RDEVAL
308000960731     C                   WRITE     EDRDE000
308100960731     C                   END
308200960731     C*  Memorizzo qualificatore CMR
308300960819     C                   CLEAR                   EDRDE000
308400961029     C                   MOVEL     WKSC          KLNP1
308500960731     C                   MOVEL     'TD1153'      KNMC
308600960731     C                   Z-ADD     1             KNRP
308700960731     C     KRDE          CHAIN     EDRDE01L                           31
308800960731     C     *IN31         IFEQ      '1'
308900960731     C                   Z-ADD     VABAAS        RDEAAS
309000040302     C                   Z-ADD     VABLNP        RDELNP
309100960731     C                   Z-ADD     VABNRS        RDENRS
309200960731     C                   Z-ADD     VABNSP        RDENSP
309300960827     C     §PTCM1        IFNE      'S'
309400960731     C     WNRCMR        OREQ      *BLANKS
309500960731     C                   Z-ADD     1             RDENRP
309600960731     C                   ELSE
309700960731     C                   Z-ADD     2             RDENRP
309800960731     C                   END
309900960731     C                   MOVEL     'TD1153'      RDENMC
310000960731     C                   MOVEL     'AFB'         RDEVAL
310100960731     C                   WRITE     EDRDE000
310200960731     C                   END
310300960731     C*  Memorizzo la data
310400960819     C                   CLEAR                   EDRDE000
310500961029     C                   MOVEL     WKSC          KLNP1
310600960731     C                   MOVEL     'TD2380'      KNMC
310700960731     C                   Z-ADD     1             KNRP
310800960731     C     KRDE          CHAIN     EDRDE01L                           31
310900960731     C     *IN31         IFEQ      '1'
311000960731     C                   Z-ADD     VABAAS        RDEAAS
311100040302     C                   Z-ADD     VABLNP        RDELNP
311200960731     C                   Z-ADD     VABNRS        RDENRS
311300960731     C                   Z-ADD     VABNSP        RDENSP
311400960827     C     §PTCM1        IFNE      'S'
311500960731     C     WNRCMR        OREQ      *BLANKS
311600960731     C                   Z-ADD     1             RDENRP
311700960731     C                   ELSE
311800960731     C                   Z-ADD     2             RDENRP
311900960731     C                   END
312000960731     C                   MOVEL     'TD2380'      RDENMC
312100960731     C                   MOVEL     WDATFV        RDEVAL
312200960731     C                   WRITE     EDRDE000
312300960731     C                   END
312400960731     C*
312500960827     C                   END                                                    §PTCM1 = 'S'
312600960731     C*
312700960731     C                   ENDSR
312800960527     C*----------------------------------------------------------------
312900960527     C*? CTRSGN - CONTROLLO DETTAGLIO SEGNACOLLI
313000960527     C*----------------------------------------------------------------
313100960528     C     CTRSGN        BEGSR
313200960527     C*
313300960528     C                   MOVEL     'N'           WNSGER            1
313400960527     C*  Controllo se il nr.segnacolli corrisponde coi bollettati
313500960527     C     VABNCL        IFNE      XY
313600961010     C                   MOVEL     'S'           WNSGER
313700961010     C                   MOVEL     'S'           WSQUAD
313800960527     C                   Z-ADD     17            WINDER
313900960927     C                   MOVEL     *BLANKS       WCOMO8            8
314000960927     C                   MOVEL     XY            WCOMO8            8
314100960926     C                   MOVE      '<>'          WCOMO8
314200960926     C                   MOVEL     WCOMO8        WCOM14           14
314300960926     C                   MOVE      VABNCL        WCOM14           14
314400960926     C                   Z-ADD     17            WINDER
314500960926     C                   MOVEL     *BLANKS       WVALOR
314600960926     C                   MOVEL     WCOM14        WVALOR
314700960731     C                   SETON                                        05
314800960528     C                   EXSR      STPERR
314900960527     C                   GOTO      FINSGN
315000960527     C                   END
315100960527     C*  Riordino i segnacolli
315200960731     C     §1BFG7        IFEQ      'N'
315300960527     C                   SORTA     SGN
315400960527     C*  Controllo se sono sequenziali
315500960528     C                   MOVEL     'N'           WNOSEQ            1
315600960528     C     SGN(1)        ADD       1             WEXSGN            7 0
315700960527     C     2             DO        XY            X
315800960527     C     SGN(X)        IFNE      WEXSGN
315900960527     C                   MOVEL     'S'           WNOSEQ
316000960527     C                   END
316100960527     C     SGN(X)        ADD       1             WEXSGN
316200960527     C                   END
316300960731     C                   END
316400960527     C*
316500960527     C     FINSGN        ENDSR
316600980219     C*----------------------------------------------------------------
316700980219     C*? CTRSGE - CONTROLLO DETTAGLIO SEGNACOLLI EUROEXPRESS
316800980219     C*----------------------------------------------------------------
316900980219     C     CTRSGE        BEGSR
317000980219     C*
317100980219     C                   MOVEL     'N'           WNSGER            1
317200980219      *
317300980219      *  Controllo se il nr.segnacolli corrisponde coi bollettati
317400980219      *
317500980219     C     VABNCL        IFNE      §§
317600980219     C                   MOVEL     'S'           WNSGER
317700980219     C                   MOVEL     'S'           WSQUAD
317800980219     C                   Z-ADD     17            WINDER
317900980219     C                   MOVEL     *BLANKS       WCOMO8            8
318000980219     C                   MOVEL     §§            WCOMO8            8
318100980219     C                   MOVE      '<>'          WCOMO8
318200980219     C                   MOVEL     WCOMO8        WCOM14           14
318300980219     C                   MOVE      VABNCL        WCOM14           14
318400980219     C                   Z-ADD     17            WINDER
318500980219     C                   MOVEL     *BLANKS       WVALOR
318600980219     C                   MOVEL     WCOM14        WVALOR
318700980219     C                   SETON                                        05
318800980219     C                   EXSR      STPERR
318900980219     C                   END
319000980219      *
319100980219     C                   ENDSR
319200960527     C*----------------------------------------------------------------
319300020909     C*? ESEGUO SCRITTURA EDiVAD
319400960527     C*----------------------------------------------------------------
319500960527     C     WRTVAD        BEGSR
319600960527     C*
319700960527     C* Se non seq. scrivo xy record
319800960731     C     §1BFG1        IFEQ      'S'
319900990810     C     XY            ANDNE     *ZEROS
320000960731     C*
320100960731     C     §1BFG7        IFEQ      'S'
320200960527     C                   DO        XY            X
320300960528     C                   Z-ADD     VABCCM        KCCM
320400960528     C                   Z-ADD     SGN(X)        KNCD
320500960924     C                   Z-ADD     VABCNT        VADCNT
320600960924     C                   MOVEL     VABCMR        VADCMR
320700960527     C                   Z-ADD     VABAAS        VADAAS
320800040302     C                   Z-ADD     VABLNP        VADLNP
320900960527     C                   Z-ADD     VABNRS        VADNRS
321000960527     C                   Z-ADD     VABNSP        VADNSP
321100960731     C                   Z-ADD     1             VADNCL
321200960911     C                   MOVEL     SGN(X)        VADCDP
321300960527     C                   Z-ADD     SGN(X)        VADNCD
321400960527     C                   Z-ADD     *ZEROS        VADNCA
321500960528     C                   Z-ADD     VABCCM        VADCCM
321600020909     C                   movel     VABfgs        VADfgs
321700020909     C                   WRITE     EDiVAD00
321800960527     C                   END
321900960527     C* Se sequenziali scrivo un solo record
322000960527     C                   ELSE
322100960731     C                   Z-ADD     VABAAS        VADAAS
322200040302     C                   Z-ADD     VABLNP        VADLNP
322300960731     C                   Z-ADD     VABNRS        VADNRS
322400960731     C                   Z-ADD     VABNSP        VADNSP
322500960731     C                   Z-ADD     VABNCL        VADNCL
322600960731     C                   Z-ADD     SGN(1)        VADNCD
322700960731     C                   Z-ADD     SGN(XY)       VADNCA
322800960731     C                   Z-ADD     VABCCM        VADCCM
322900020909     C                   movel     VABfgs        VADfgs
323000020909     C                   WRITE     EDiVAD00
323100960527     C                   END
323200960731     C*
323300960731     C                   ELSE
323400960731     C                   DO        XY            X
323500960731     C                   Z-ADD     VABCCM        KCCM
323600960731     C                   Z-ADD     SGN(X)        KNCD
323700960731     C                   Z-ADD     VABAAS        VADAAS
323800040302     C                   Z-ADD     VABLNP        VADLNP
323900960731     C                   Z-ADD     VABNRS        VADNRS
324000960731     C                   Z-ADD     VABNSP        VADNSP
324100960731     C                   MOVEL     SGN(X)        VADCDP
324200960731     C                   Z-ADD     1             VADNCL
324300960731     C                   Z-ADD     *ZEROS        VADNCD
324400960731     C                   Z-ADD     *ZEROS        VADNCA
324500960731     C                   Z-ADD     VABCCM        VADCCM
324600020909     C                   movel     VABfgs        VADfgs
324700020909     C                   WRITE     EDiVAD00
324800960731     C                   END
324900960527     C*
325000960731     C                   END
325100960731     C*
325200960527     C                   ENDSR
325300031211     C*----------------------------------------------------------------
325400031211     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
325500031211     C*----------------------------------------------------------------
325600031211     C     WRTVAT_Rif    BEGSR
325700031211      *
325800031211      *  riferimento di consegna destinatario
325900031211     c                   if        WatRDE <> *blank
326000031211     C                   Z-ADD     VABCNT        VATCNT
326100031211     C                   MOVEL     VABCMR        VATCMR
326200031211     C                   Z-ADD     VABCCM        VATCCM
326300031211     C                   movel     VABfgs        VATfgs
326400040302     C                   Z-ADD     VABLNP        VATLNP
326500031211     C                   Z-ADD     VABAAS        VATAAS
326600031211     C                   Z-ADD     VABNRS        VATNRS
326700031211     C                   Z-ADD     VABNSP        VATNSP
326800031211     C                   MOVEL     'A'           VATTRC
326900031211     C                   MOVEL     WatRDE        VATNOT
327000031211     C                   WRITE     EDiVAT00
327100031211     c                   end
327200031211      *
327300031211      *  riferimento di consegna telefonico
327400031211     c                   if        WatTEL <> *blank
327500031211     C                   Z-ADD     VABCNT        VATCNT
327600031211     C                   MOVEL     VABCMR        VATCMR
327700031211     C                   Z-ADD     VABCCM        VATCCM
327800031211     C                   movel     VABfgs        VATfgs
327900040302     C                   Z-ADD     VABLNP        VATLNP
328000031211     C                   Z-ADD     VABAAS        VATAAS
328100031211     C                   Z-ADD     VABNRS        VATNRS
328200031211     C                   Z-ADD     VABNSP        VATNSP
328300031211     C                   MOVEL     'B'           VATTRC
328400031211     C                   MOVEL     WatTEL        VATNOT
328500031211     C                   WRITE     EDiVAT00
328600031211     c                   end
328700031211      *
328800031219      * riferimento del Partner che una volta veniva riportato sul BL4
328900031219     c                   if        Wnsp   <> *blank
329000031219     C                   Z-ADD     VABCNT        VATCNT
329100031219     C                   MOVEL     VABCMR        VATCMR
329200031219     C                   Z-ADD     VABCCM        VATCCM
329300031219     C                   movel     VABfgs        VATfgs
329400040302     C                   Z-ADD     VABLNP        VATLNP
329500031219     C                   Z-ADD     VABAAS        VATAAS
329600031219     C                   Z-ADD     VABNRS        VATNRS
329700031219     C                   Z-ADD     VABNSP        VATNSP
329800031219     C                   MOVEL     'C'           VATTRC
329900031219     C                   MOVEL     Wnsp          VATNOT
330000031219     C                   WRITE     EDiVAT00
330100031219     c                   end
330200031219      *
330300031211     C                   ENDSR
330400980219     C*----------------------------------------------------------------
330500020909     C*? ESEGUO SCRITTURA EDiVAT
330600980219     C*----------------------------------------------------------------
330700980219     C     WRTVAT        BEGSR
330800980219      *
330900980219     C                   DO        §§            X
331000031121     C                   Z-ADD     VABCNT        VATCNT
331100031121     C                   MOVEL     VABCMR        VATCMR
331200980219     C                   Z-ADD     VABCCM        VATCCM
331300020909     C                   movel     VABfgs        VATfgs
331400040302     C                   Z-ADD     VABLNP        VATLNP
331500980219     C                   Z-ADD     VABAAS        VATAAS
331600980219     C                   Z-ADD     VABNRS        VATNRS
331700980219     C                   Z-ADD     VABNSP        VATNSP
331800980219     C                   MOVEL     'E'           VATTRC
331900050301      *
332000050301      * se riceviamo dal Partner/Cliente un segnacollo troppo lungo possiamo riportare
332100050301      * una parte di esso e questo è definito nella tab.PU Lunghezza max. segnacollo
332200050301      * es.SERNAM manda 32 caratteri ma noi vogliamo prenderne solo i primi 30
332300050301     c                   if        WVATlun > 0
332400050301      *
332500050301      *    prende una parte del segnacollo pari alla lunghezza definita da sinistra
332600050301     C                   eval      VATnot = %subst(SGE(X):1:WVATlun)
332700050301      *
332800050301     c                   else
332900050301      *
333000050301      * Se non è impostato nulla prende il segnacollo passato così com'è
333100980219     C                   MOVEL     SGE(X)        VATNOT
333200050301     c                   end
333300050301      *
333400020909     C                   WRITE     EDiVAT00
333500980219     C                   END
333600980219      *
333700980219     C                   ENDSR
333800961010     C*----------------------------------------------------------------
333900961010     C*? ESEGUO SCRITTURA EDSUM
334000961010     C*----------------------------------------------------------------
334100961010     C     WRTSUM        BEGSR
334200961010     C*
334300961010     C* Se non seq. scrivo xy record
334400961010     C                   DO        XY            X
334500961010     C                   Z-ADD     VABAAS        SUMAAS
334600040302     C                   Z-ADD     VABLNP        SUMFLS
334700961104     C                   MOVEL     WKSC          SUMLNP
334800961010     C                   Z-ADD     VABLNA        SUMLNA
334900961010     C                   Z-ADD     VABZNC        SUMZNC
335000961010     C                   Z-ADD     VABNRS        SUMNRS
335100961010     C                   Z-ADD     VABNSP        SUMNSP
335200961010     C                   Z-ADD     SGN(X)        SUMNSC
335300961028     C                   Z-ADD     WKSC          SUMCCM
335400961010     C                   MOVEL     *BLANKS       SUMSTS
335500961010     C                   MOVEL     *BLANKS       SUMFLG
335600961010     C                   MOVEL     WPRGSP        SUMCNI
335700961010     C                   MOVEL     VABCMR        SUMCMR
335800961010     C                   Z-ADD     0             SUMNFE
335900961010     C                   Z-ADD     VABCCM        VADCCM
336000961010     C                   WRITE     EDSUM000
336100961010     C                   END
336200961010     C*
336300961010     C                   ENDSR
336400960527     C*----------------------------------------------------------------
336500960527     C*? STPERR: STAMPA ERRORE
336600960527     C*----------------------------------------------------------------
336700960527     C     STPERR        BEGSR
336800040227     C*
336900040227     C*  Prepara messaggio di errore da mandare ad un utente specifico
337000040227     C*   a pro-memoria da controllare gli errori elencati in stampa
337100040227     c                   clear                   msgDSP
337200040227     C                   EVAL      msgDSP  = 'ATTENZIONE:  TRTC80R traduzione E-
337300040227     C                             DI IFCSUM Controllare Stampa errori QSYSPRT -
337400040227     C                             con il comando: << WRKSPLF SELECT(*ALL *ALL -
337500040227     C                             *ALL TRTC80R) >> x il Codice:'
337600040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' +
337700040303     C                             %trim(%Editc(§ptksc:'X')) + ' ' +
337800040303     C                             %trim(ACORAG)
337900040227     C*
338000960527     C                   Z-ADD     WINDER        I                 2 0
338100960731     C                   MOVEL     MSG(I)        SERR             60
338200960927     C     *INOF         IFEQ      '1'
338300960927     C                   EXCEPT    TESTA
338400040227     C*
338500960927     C                   MOVEL     'S'           WSTPTE
338600960927     C                   END
338700960927     C*
338800960927     C*  Controllo intestazione errori da stampare
338900960927     C     WSTPTE        IFEQ      'S'
339000040227     C*
339100960927     C*  Controllo se devo stampare nr.documento
339200960927     C     §PTCMR        IFEQ      'S'
339300960927     C     WNRCMR        ANDNE     *BLANKS
339400960927     C                   MOVEL     'CMR'         TIPDOC            3
339500960927     C                   MOVEL     WNRCMR        WDOCUM           35
339600960927     C                   EXCEPT    ERRDOC
339700040227     C*
339800040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' + %trim(TIPDOC)
339900040227     C                                       + ' ' + %trim(Wdocum)
340000040227     C*
340100960927     C                   ELSE
340200040227     C*
340300960927     C     §PTNFV        IFEQ      'S'
340400960927     C     WNUMFV        ANDNE     *BLANKS
340500960927     C                   MOVEL     'AFB'         TIPDOC            3
340600960927     C                   MOVEL     WNUMFV        WDOCUM           35
340700960927     C                   EXCEPT    ERRDOC
340800040227     C*
340900040227     C                   EVAL      msgDSP  = %trim(msgDSP) + ' ' + %trim(TIPDOC)
341000040227     C                                       + ' ' + %trim(Wdocum)
341100040227     C*
341200960927     C                   ELSE
341300960927     C                   EXCEPT    ERRCLI
341400041006      *
341500041006     c                   goto      no_invio_Xora
341600040227     c                   exsr      SEND_MSG_EDP
341700041006     c     no_invio_Xora tag
341800041006      *
341900040310     c                   move      'N'           Snd_Msg_alert     1
342000040227     C*
342100960927     C                   END
342200960927     C                   END
342300960927     C                   MOVEL     'N'           WSTPTE            1
342400960927     C                   END
342500960927     C*
342600960527     C                   EXCEPT    DETER
342700040416     c                   move      'S'           errori_prtf
342800960527     C*
342900960527     C                   ENDSR
343000960329     C*----------------------------------------------------------------
343100960527     C*? CNVDAT - DECODIFICA LA DATA
343200960527     C* INPUT:  - WFORM  (FORMATO DATA : 102)
343300960527     C*         - WDATA  (DATA ALFANUMERICA)
343400960527     C* OUTPUT: - WCAMG  (DATA NUMERICA) (8)
343500960527     C*         - WHMS   (ORA NUMERICA)
343600960329     C*----------------------------------------------------------------
343700960527     C     CNVDAT        BEGSR
343800960329     C*
343900960528     C                   MOVEL     ' '           WERRO             1
344000960527     C                   Z-ADD     *ZEROS        WCAMG             8 0
344100960527     C                   Z-ADD     *ZEROS        WHMS              6 0
344200960329     C*
344300960527     C     WFORM         IFEQ      '102'                                        CCYMMDD
344400960527     C                   MOVEL     WDATA         WCOMO8            8
344500960527     C                   TESTN                   WCOMO8               99
344600960527     C   99              MOVE      WCOMO8        WCAMG                          *DATA
344700960528     C  N99              MOVEL     'S'           WERRO             1
344800960329     C                   END
344900960329     C*
345000960329     C                   ENDSR
345100960725     C*----------------------------------------------------------------
345200960725     C*? TSTNUM - CONTROLLO NUMERICO
345300960725     C* INPUT:  - WNUM   (Numero ALFABETICO) (35)
345400960725     C*         - WLUNGH (Lunghezza campo output)
345500960725     C* OUTPUT: - WNUMOK (Numero NUMERICO) (15)
345600960725     C*----------------------------------------------------------------
345700960725     C     TSTNUM        BEGSR
345800960725     C*
345900960725     C* IMPOSTA L'INDICE DELLA SCHIERA NUMERICA SULL'ULTIMO NUM. A DESTRA
346000960725     C                   MOVEL     'N'           WOKNUM            1
346100960725     C                   MOVEL     *BLANKS       WNUMOK           15
346200960725     C                   Z-ADD     15            IN                3 0
346300960725     C                   Z-ADD     WLUNGH        IX                3 0
346400960725     C* PORTA IL N.DOCUM. IN INPUT NELLA SCHIERA ALFANUMERICA
346500960725     C                   MOVEA     WNUM          NDA
346600960725     C* IMPOSTA A ZERO LA SCHIERA IN OUTPUT NUMERICA
346700960725     C                   MOVEA     *ALL'0'       NDN
346800960725     C* LOOP CARATTERE PER CARATTERE SCHIERA IN INPUT DAL 35° CAR. AL 1°
346900960725     C                   Z-ADD     WLUNGH        I
347000960725     C     I             DOWGT     0                                            --- 1 -->
347100960725     C* ESTRAE IL CARATTERE DA ESAMINARE
347200960725     C     1             SUBST     WNUM:I        WTEM01            1
347300960725     C* CONTROLLA SE IL CAR.E' PRESENTE NELLA DS CARATTERI CONVERTIBILI
347400960725     C* (SPAZIO NON DEVE ESSERE CONVERTIBILE)
347500960725     C     WTEM01        SCAN      WCA                                    99
347600960725     C* CARATT.TROVATO PROCEDO CON LA CONVERSIONE
347700960725     C     *IN99         IFEQ      '1'                                          --- 2 -->
347800960725     C* SE LA SCHIERA IN OUTPUT E' GIA' PIENA NON CONVERTO
347900960725     C     IN            IFGT      *ZEROS                                       --- 3 -->
348000960725     C* ATTRAVERSO LE DS ALFAB/NUM CONVERTE E METTE IL NUMERO NELLA SCHIERA OUT
348100960725     C* DA DESTRA VERSO SINISTRA
348200960725     C     WCA:WCN       XLATE     WTEM01        WTEMP1            1
348300960725     C                   MOVEL     WTEMP1        NDN(IN)
348400960725     C                   SUB       1             IN
348500960725     C                   END                                                    <-- 3 ---
348600960725     C                   END                                                    <-- 2 ---
348700960725     C                   SUB       1             I
348800960725     C                   END                                                    <-- 1 ---
348900960725     C*
349000960725     C                   MOVEA     NDN           WNDN             15
349100960725     C     WNDN          IFNE      *ZEROS
349200960725     C                   MOVEA     NDN           WNUMOK           15
349300960725     C                   MOVEL     'S'           WOKNUM            1
349400960725     C                   END
349500960725     C*
349600960725     C                   ENDSR
349700960527     C*----------------------------------------------------------------
349800960527     C*? CTRVAL - CONTROLLO VALUTA
349900960527     C*----------------------------------------------------------------
350000960527     C     CTRVAL        BEGSR
350100960527     C*
350200960527     C                   Z-ADD     1             Y
350300960527     C     WVAL          LOOKUP    CCV(Y)                                 32
350400960527     C     *IN32         IFEQ      '1'
350500960527     C                   MOVEL     DCV(Y)        DSCV
350600960527     C*  Se valuta italiana abblenco divisa
350700960527     C     §CVITA        IFEQ      'I'
350800010928     C                   MOVEL     'ITL'         WVAL
350900960527     C                   END
351000960527     C                   ELSE
351100960527     C                   Z-ADD     20            WINDER
351200960527     C                   MOVEL     *BLANKS       WVALOR
351300960527     C                   MOVEL     WVAL          WVALOR
351400960731     C                   SETON                                        05
351500960527     C                   EXSR      STPERR
351600960527     C                   MOVEL     *BLANKS       WVAL
351700960527     C                   MOVEL     *BLANKS       WTPI
351800960527     C                   END
351900960527     C*
352000960527     C                   ENDSR
352100960401     C*----------------------------------------------------------------
352200960401     C*? *INZSR - OPERAZIONI INIZIALI
352300960401     C*----------------------------------------------------------------
352400960401     C     *INZSR        BEGSR
352500960401     C*
352600040322     c     *entry        plist
352700040322     c                   parm                    User_msg         10
352800040322     C*
352900960527     C* Definisco chiavi di accesso
353000960527     C     KTAB1         KLIST
353100960401     C                   KFLD                    KKUT
353200960401     C                   KFLD                    KCOD
353300070219     C*
353400050112     C     KTAB          KLIST
353500050112     C                   KFLD                    KKUT
353600050112     C                   KFLD                    KCOD
353700050112     C                   KFLD                    KKEY
353800070219      *
353900960401     C     KNUF          KLIST
354000960401     C                   KFLD                    KAAA
354100960401     C                   KFLD                    KCNU
354200960401     C                   KFLD                    KFIL
354300070219      *
354400960924     C     KVAB1         KLIST
354500960925     C                   KFLD                    KCCM
354600960925     C                   KFLD                    KCMR
354700960925     C                   KFLD                    KCNT
354800960527     C                   KFLD                    KAAS
354900960527     C                   KFLD                    KLNP
355000960527     C                   KFLD                    KNRS
355100960527     C                   KFLD                    KNSP
355200070219      *
355300960924     C     KVAB2         KLIST
355400960924     C                   KFLD                    KCCM
355500960924     C                   KFLD                    KCMR
355600070219      *
355700960528     C     KRDE          KLIST
355800960528     C                   KFLD                    KAAS
355900961029     C                   KFLD                    KLNP1
356000960528     C                   KFLD                    KNRS
356100960528     C                   KFLD                    KNSP
356200960528     C                   KFLD                    KNMC
356300960528     C                   KFLD                    KNRP
356400070219      *
356500960531     C     KACO          KLIST
356600960531     C                   KFLD                    KKUT
356700960531     C                   KFLD                    KKCC
356800960531     C                   KFLD                    KKSC
356900070219      *
357000960531     C     KIND          KLIST
357100960531     C                   KFLD                    KKUT
357200960531     C                   KFLD                    KKCC
357300960531     C                   KFLD                    KKSC
357400980617      *
357500980617      * DETERMINO SE SONO BARTOLINI O SDI
357600020909     C                   CLEAR                   TIBS55DS
357700980617     C                   MOVEL     'L'           I50TLA
357800020909     C                   CALL      'TIBS55R'
357900020909     C                   PARM                    TIBS55DS
358000980617      *
358100960401     C* RECUPERO DATI DELL'UTENTE
358200960527     C                   Z-ADD     1             CODUT             1 0
358300960401     C                   CALL      'XPARUT'
358400000120     C                   PARM                    UTEDSE0F
358500980617     C                   MOVEL     RAGUT         RSUT             20
358600960531     C*
358700960531     C* RICERCA CAPOCONTI
358800960531     C                   DO        50            X
358900960531     C                   MOVE      TCU(X)        TCUDS
359000960531     C     F56           IFEQ      'CG'
359100960531     C     F34           ANDEQ     '01'
359200960531     C                   Z-ADD     KCU(X)        KCI               4 0
359300960531     C                   END
359400960531     C                   END
359500960402     C*
359600960402     C* IMPOSTO A ZERO VARIABILI DI PGM
359700040310     c                   move      *blank        Snd_Msg_alert     1
359800040607     c                   move      'N'           In_Stampa         1
359900960402     C                   MOVEL     'N'           WFINE             1
360000960527     C                   MOVEL     CA(1)         WCA              78
360100960527     C                   MOVEL     CN(1)         WCN              78
360200070219      *
360300960402     C* RECUPERO DATA E ORA
360400960527     C                   TIME                    WHHDAT           14 0
360500960527     C                   MOVE      WHHDAT        G02DAT
360600960913     C                   MOVE      WHHDAT        WDATA8            8 0
360700960913     C                   MOVE      WDATA8        WAA2              2 0
360800960913     C                   MOVEL     WDATA8        DATA              6 0
360900960913     C                   MOVE      WAA2          DATA
361000960527     C                   MOVEL     *BLANK        G02ERR
361100960527     C                   CALL      'XSRDA8'
361200960527     C                   PARM                    WLBDA8
361300960711     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
361400960527     C                   TIME                    WORA              6 0
361500070219      *
361600960527     C* Caricamento Tabella 1B
361700970624     C                   Z-ADD     0             X                 4 0
361800960402     C                   Z-ADD     CODUT         KKUT
361900960402     C                   MOVEL     '1B'          KCOD
362000960527     C     KTAB1         CHAIN     TABEL00F                           30
362100960528     C     *IN30         DOWEQ     '0'
362200960527     C     TBLFLG        IFEQ      *BLANKS
362300960527     C                   ADD       1             X
362400960527     C                   MOVEL     TBLKEY        CTM(X)
362500960527     C                   MOVEL     TBLUNI        DTM(X)
362600960527     C                   END
362700960527     C     KTAB1         READE     TABEL00F                               30
362800960527     C                   END
362900020221      *
363000960822     C* Caricamento Tabella 15
363100020221     C                   Z-ADD     0             X                 4 0
363200960822     C                   Z-ADD     CODUT         KKUT
363300960822     C                   MOVEL     '15'          KCOD
363400960822     C     KTAB1         CHAIN     TABEL00F                           30
363500960822     C     *IN30         DOWEQ     '0'
363600960822     C     TBLFLG        IFEQ      *BLANKS
363700960822     C                   ADD       1             X
363800960822     C                   MOVEL     TBLKEY        C15(X)
363900960822     C                   MOVEL     TBLUNI        DS15
364000960822     C                   MOVEL     §15COD        ISO(X)
364100961017     C                   MOVEL     §15ECF        CPF(X)
364200961017     C                   MOVE      §15PCF        CPF(X)
364300960822     C                   END
364400960822     C     KTAB1         READE     TABEL00F                               30
364500960822     C                   END
364600070219      *
364700960527     C* Caricamento Tabella CV
364800970624     C                   Z-ADD     0             X
364900960527     C                   Z-ADD     CODUT         KKUT
365000960527     C                   MOVEL     'CV'          KCOD
365100960527     C     KTAB1         CHAIN     TABEL00F                           30
365200960528     C     *IN30         DOWEQ     '0'
365300960527     C     TBLFLG        IFEQ      *BLANKS
365400960527     C                   ADD       1             X
365500960527     C                   MOVEL     TBLKEY        CCV(X)
365600960527     C                   MOVEL     TBLUNI        DCV(X)
365700960527     C                   END
365800960527     C     KTAB1         READE     TABEL00F                               30
365900960527     C                   END
366000070219      *
366100960527     C* Caricamento Tabella Priorità trasporto
366200970624     C                   Z-ADD     0             X
366300960527     C                   MOVEL     'TS'          TABCOD
366400960528     C     TABCOD        CHAIN     EDTAB01L                           30
366500960528     C     *IN30         DOWEQ     '0'
366600960527     C     TABFLG        IFEQ      *BLANKS
366700960527     C                   ADD       1             X
366800960528     C                   MOVEL     TABKEY        PTR(X)
366900061128     C                   MOVEL     TABKEY        PTR35(X)
367000960527     C                   MOVEL     TABUNI        TSP(X)
367100960527     C                   END
367200960528     C     TABCOD        READE     EDTAB01L                               30
367300960527     C                   END
367400070219      *
367500960716     C* Caricamento Tabella Tipo Incasso C/Assegno
367600970624     C                   Z-ADD     0             X
367700960716     C                   MOVEL     'TC'          TABCOD
367800960716     C     TABCOD        CHAIN     EDTAB01L                           30
367900960716     C     *IN30         DOWEQ     '0'
368000960716     C     TABFLG        IFEQ      *BLANKS
368100960716     C                   ADD       1             X
368200061128     C                   MOVEL     TABKEY        CTC35(X)
368300061128     C                   MOVEL     TABKEY        CTC(X)
368400960716     C                   MOVEL     TABUNI        TPI(X)
368500960716     C                   END
368600960716     C     TABCOD        READE     EDTAB01L                               30
368700960716     C                   END
368800980618      *
368900980618      * Caricamento Tabella Partner esteri
369000970624     C                   Z-ADD     0             X
369100960527     C                   MOVEL     'PT'          TABCOD
369200960528     C     TABCOD        CHAIN     EDTAB01L                           30
369300960528     C     *IN30         DOWEQ     '0'
369400980618      *
369500980618     C                   SELECT
369600980618     C     TABFLG        WHENNE    *BLANKS
369700980618      *
369800980618     C                   OTHER
369900960527     C                   ADD       1             X
370000960527     C                   MOVEL     TABKEY        CPT(X)
370100070219     C                   eval      CPTparz(X) = %subst(TABKEY:1:31)
370200070219     C                   eval      KSC_UNB(X) = %subst(TABuni:1:7)
370300960527     C                   MOVEL     TABUNI        DPT(X)
370400980618     C                   ENDSL
370500980617      *
370600960528     C     TABCOD        READE     EDTAB01L                               30
370700960527     C                   END
370800070219      * salva quanti Codici UNB ha caricato
370900070219     c                   z-add     x             sav_CPTx
371000980618      *
371100960912     C                   MOVEL     'PU'          TABCOD
371200960912     C     TABCOD        CHAIN     EDTAB01L                           30
371300960911     C     *IN30         DOWEQ     '0'
371400980618      *
371500980618     C                   SELECT
371600980618     C     TABFLG        WHENNE    *BLANKS
371700980618      *
371800980618     C                   OTHER
371900960911     C                   MOVEL     TABKEY        CODPU            35
372000960911     C                   Z-ADD     1             X
372100960911     C     CODPU         LOOKUP    CPT(X)                                 32
372200980618     C   32              MOVEL     TABUNI        DPU(X)
372300980618     C                   ENDSL
372400980618      *
372500960912     C     TABCOD        READE     EDTAB01L                               30
372600960911     C                   END
372700980618      *
372800070219      * Prosegue tab.PT e PU
372900070219     C                   MOVEL     'PV'          TABCOD
373000070219     C     TABCOD        CHAIN     EDTAB01L                           30
373100070219     C     *IN30         DOWEQ     '0'
373200070219      *
373300070219     C                   SELECT
373400070219     C     TABFLG        WHENNE    *BLANKS
373500070219      *
373600070219     C                   OTHER
373700070219     C                   MOVEL     TABKEY        CODPV            35
373800070219     C                   Z-ADD     1             X
373900070219     C     CODPV         LOOKUP    CPT(X)                                 32
374000070219     C   32              MOVEL     TABUNI        DPV(X)
374100070219     C                   ENDSL
374200070219      *
374300070219     C     TABCOD        READE     EDTAB01L                               30
374400070219     C                   END
374500070219      *
374600980618      * Caricamento Tabella termini consegna
374700970624     C                   Z-ADD     0             X
374800960527     C                   MOVEL     'TB'          TABCOD
374900960528     C     TABCOD        CHAIN     EDTAB01L                           30
375000960528     C     *IN30         DOWEQ     '0'
375100960527     C     TABFLG        IFEQ      *BLANKS
375200960527     C                   ADD       1             X
375300061128     C                   MOVEL     TABKEY        CTB35(X)
375400061128     C                   MOVEL     TABKEY        CTB(X)
375500960527     C                   MOVEL     TABUNI        POR(X)
375600960527     C                   END
375700960528     C     TABCOD        READE     EDTAB01L                               30
375800960527     C                   END
375900070219      *
376000961122     C* Caricamento Tabella codici clienti - tariffe
376100970624     C                   Z-ADD     0             X
376200961122     C                   MOVEL     'CL'          TABCOD
376300961122     C     TABCOD        CHAIN     EDTAB01L                           30
376400961122     C     *IN30         DOWEQ     '0'
376500961122     C     TABFLG        IFEQ      *BLANKS
376600961122     C                   ADD       1             X
376700961122     C                   MOVEL     TABKEY        CCL(X)
376800961122     C                   MOVEL     TABUNI        DCL(X)
376900961122     C                   END
377000961122     C     TABCOD        READE     EDTAB01L                               30
377100961122     C                   END
377200070219      *
377300960911     C* Caricamento Serivizi speciali
377400970624     C                   Z-ADD     0             X
377500960926     C                   MOVEL     'SS'          TABCOD
377600960911     C     TABCOD        CHAIN     EDTAB01L                           30
377700960911     C     *IN30         DOWEQ     '0'
377800960911     C     TABFLG        IFEQ      *BLANKS
377900960911     C                   ADD       1             X
378000960926     C                   MOVEL     TABKEY        SS(X)
378100061128     C                   MOVEL     TABKEY        SS35(X)
378200960926     C                   MOVEL     TABUNI        DSS(X)
378300960911     C                   END
378400960911     C     TABCOD        READE     EDTAB01L                               30
378500960911     C                   END
378600000125      *
378700960712     C                   MOVEL     *ALL'-'       CMP132          132
378800000125     C                   eval      *inOA = *ON
378900000125     C                   eval      *inOF = *ON
379000020909     C                   CLEAR                   EDiVAB00
379100000125     C                   clear                   Wvablnp
379200040301     C                   clear                   Wvabnrs
379300040301     C                   clear                   Wvabctm
379400050223     C                   clear                   WVABfnsp
379500960726     C                   MOVEA     *HIVAL        SGN
379600960927     C*  PRIMA OPEN SYSPRT
379700960927     C                   OPEN      SYSPRT
379800960402     C*
379900040416     c                   clear                   errori_prtf       1
380000040416     C*
380100960402     C                   ENDSR
380200070219     c*==================================================================*
380300070219      * Imposta gli indirizzi mail a cui inviare segnalazione di errori
380400070219     c*==================================================================*
380500070219     c     Imp_ADDR_mail begsr
380600070219      *
380700070219     c                   clear                   Indirizzi
380800070219      *
380900070219      *  Lunghezza indirizzi presenti
381000070219     c                   eval      Lun_Ind = %Len(%TrimR(Mail_Ind))
381100070219     c                   z-add     1             al_char           3 0
381200070219     c                   z-add     1             dal_char          3 0
381300070219     c                   z-add     1             tot_char          3 0
381400070219      *
381500070219     c     successivo    tag
381600070219      *
381700070219      * prende il (;) più prossimo per dividere gli indirizzi a cui spedire
381800070219      *   e aggiunge il dominio Bartolini + il (;) separatore
381900070219     c                   eval      al_char = %Scan(';' :  Mail_Ind : dal_char)
382000070219     c                   eval      tot_char = al_char - dal_char
382100070219     c                   z-add     dal_char      Dac
382200070219     c                   z-add     tot_char      Luc
382300070219      *
382400070219     c                   clear                   Indir_Bartol     40
382500070219     c                   clear                   Indir_Parz       20
382600070219     c                   eval      Indir_Parz = %Subst(Mail_Ind:dac:luc)
382700070219     c                   eval      Indir_Bartol = %Trim(Indir_Parz) +
382800070219     c                             %Trim(bart_it)
382900070219     c                   eval      Indirizzi = %Trim(Indirizzi) + Indir_Bartol
383000070219      *
383100070219     c                   if        al_char = Lun_Ind
383200070219     c                   goto      fine_indmail
383300070219     c                   else
383400070219     c                   eval      dal_char = ( al_char + 1 )
383500070219     c                   goto      successivo
383600070219     c                   end
383700070219      *
383800070219     C     fine_indmail  TAG
383900070219      *
384000070219     C                   ENDSR
384100070219     c*==================================================================*
384200070219      * Manda un Msg x E-mail
384300070219     c*==================================================================*
384400070219     c     Invio_Mail    begsr
384500070219      *
384600070219     c                   goto      non_inviare
384700070219      *
384800070219     C*   Alert_mail : invio Mail a CED@Bartolini.it                  *
384900070219     C* Inizializzo variabili
385000070219     C                   movel     *blanks       wrkEml          253
385100070219     C                   movel     *blanks       wrkMsg         5000
385200070219     C                   movel     *blanks       wrkOgg           44
385300070219      *
385400070219     C* Valorizzo i campi della e-m@ail - indirizzo
385500070219     C                   eval      wrkEml = Indirizzi
385600070219     C                   eval      wrkOgg = Oggetto
385700070219     C                   eval      wrkMsg = Messaggio
385800110203     C*
385900110203     C                   call(e)   'TRTCT00R2'
386000110203     C                   parm                    wrkEml
386100110203     C                   parm                    wrkOgg
386200110203     C                   parm                    wrkMsg
386300110203     C*
386400070219
386500070219     c     non_inviare   tag
386600070219     C*
386700070219     C                   ENDSR
386800040227     c*==================================================================*
386900040227      * Manda un Msg in Posta AS Sede a EDP*
387000040227     c*==================================================================*
387100040227     c     SEND_MSG_EDP  begsr
387200040227      *
387300040227      * INVIA MESSAGGIO ad un Utente --> EDPAB
387400040310      *   Lo manda una sola volta per lavoro
387500040322     c                   IF        Snd_Msg_alert  <> 'N' and
387600040322     c                             User_msg <> *blank
387700040227     c                   z-add     1             Jx
387800040227     C                   exsr      CalQEZSNDMG
387900040310     c                   end
388000040227      *
388100040227     c                   endsr
388200040227     ***********************************************************************
388300040227     **
388400040227     ** Send Message (QEZSNDMG) API
388500040227     **
388600040227     ***********************************************************************
388700040227     C     CalQEZSNDMG   BEGSR
388800040227      *
388900040227     ** Invio messaggio all'utente.
389000040227     C                   EVAL      SndMg03 = msgDSP
389100040322     C                   EVAL      SndMg05(Jx) = User_msg
389200040227     C                   EVAL      SndMg06 = Jx
389300040227     C                   CLEAR                   QUSEC
389400040227     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
389500040227
389600040227     C                   CALL      'QEZSNDMG'
389700040227     C                   PARM                    SndMg01
389800040227     C                   PARM                    SndMg02
389900040227     C                   PARM                    SndMg03
390000040227     C                   PARM                    SndMg04
390100040227     C                   PARM                    SndMg05
390200040227     C                   PARM                    SndMg06
390300040227     C                   PARM                    SndMg07
390400040227     C                   PARM                    SndMg08
390500040227     C                   PARM                    Qusec
390600040227     C                   PARM                    SndMg10
390700040227     C                   PARM                    SndMg11
390800040227     C                   PARM                    SndMg12
390900040227      *
391000040227     C                   ENDSR
391100050112     c**********************************************************************
391200050112     c* reperimento numero Spedizione
391300050112     c**********************************************************************
391400050112     C     repnum        BEGSR
391500050112      *
391600050112      *    deve controllare sulla tabella "3C" se il numero spedizione
391700050112      *     deve essere mantenuto oppure no
391800050112     C*
391900050112     C                   clear                   Ds3C
392000050112     C                   Z-ADD     CODUT         KKUT
392100050112     C                   MOVEL     '3C'          KCOD
392200050112     C                   movel(p)  VABccm        KKey
392300050112     C     KTAB          CHAIN     TABEL00F                           30
392400050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
392500050112     C                   MOVEL     TBLUNI        Ds3C
392600050112     C                   END
392700050112      *
392800050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
392900050112     c                   if        §3CFsp <> 'S'
393000050112      *
393100050112     C* NSP => Stacco un numeratore da AZNUM
393200050112     c                   movel     kpjbu         svkpjbu
393300050112     c                   clear                   kpjbu
393400050112     C                   clear                   TRUL33DS
393500050112     C                   eval      I33OPE = *zeros
393600050112     C                   eval      I33CNU = 302
393700050112     C                   eval      I33NUM = 1
393800050112     C                   movel     TRUL33DS      KPJBU
393900050112     C                   call      'TRUL33R'
394000050112     C                   parm                    KPJBA
394100050112     C                   movel     KPJBU         TRUL33DS
394200050112     c                   movel     svkpjbu       kpjbu
394300050112      ****
394400050112     C                   if        O33ERR = *zeros
394500050112     c                   z-add     o33nrf        NUM_SPED
394600050112     C                   else
394700050112     C                   Z-ADD     19            WINDER
394800050112     C                   MOVEL     *BLANKS       WVALOR
394900050112     C                   SETON                                        05
395000050112     C                   EXSR      STPERR
395100050112     C                   endif
395200050112      ****
395300050112     c                   ELSE
395400050112      *   se si vuole mantenere il numero spedizione
395500050112      ****
395600050112     C                   MOVEL     WOGGI         WANNO             4 0
395700050112     C                   MOVE      WANNO         KAAA
395800050112     C                   Z-ADD     3             KCNU
395900050112     C                   Z-ADD     VABlnp        KFIL
396000050112     C     KNUF          CHAIN     FLNUF                              9999
396100050112     C     *IN99         IFEQ      '0'
396200050112     C                   ADD       1             NUFNUM
396300050112     C                   Z-ADD     NUFNUM        NUM_SPED                       *NUM.SPEDIZIONE
396400050112     C                   UPDATE    FLNUF
396500050112     C                   ELSE
396600050112     C                   Z-ADD     19            WINDER
396700050112     C                   MOVEL     *BLANKS       WVALOR
396800050112     C                   SETON                                        05
396900050112     C                   EXSR      STPERR
397000050112     C                   END
397100050112     C***********
397200050112     c                   EndIF
397300050112      **
397400050112     C                   ENDSR
397500960401     O*****************************************************************
397600960528     OQSYSPRT   E            TESTA            01
397700960527     O                       RSUT                20
397800960527     O                                           50 '** STAMPA ERRORI RICEZI'
397900960527     O                                              'ONE BOLLE DA PARTNER'
398000960527     O                                              ' ESTERI **'
398100960527     O                       DATA               121 '  /  /  '
398200960527     O                                          127 'Pag.:'
398300960527     O                       PAGE          Z    132
398400970206     O          E            TESTA          1 02
398500960527     O                       WORA               121 '  :  :  '
398600960927     O*------------------------------------
398700960927     O          E            ERRCLI         1
398800960927     O                                           15 'CODICE CLIENTE:'
398900960927     O                       §PTKSC        Z     23
399000960927     O                       ACORAG              72
399100960927     O*------------------------------------
399200960927     O          E            ERRDOC         1
399300960927     O                                           15 'CODICE CLIENTE:'
399400960927     O                       §PTKSC        Z     23
399500960927     O                       ACORAG              72
399600960927     O                                           78 'NUMERO'
399700960927     O                       TIPDOC              82
399800960927     O                                           83 ':'
399900960927     O                       WDOCUM             120
400000960401     O*------------------------------------
400100960528     O          E            DETER          1
400200960731     O                       SERR                60
400300960731     O                       WVALOR              96
400400070219     O               05      WidSPEp            132
400500960926     O*------------------------------------
400600960926     O          E            ERRQUA         1
400700960926     O                                         +  0 'Totale colli/spediz. '
400800960926     O                                         +  0 'msg.:'
400900960926     O                       WCOC          4   +  1
401000960926     O                                         +  0 '/'
401100960926     O                       WSPC          4   +  0
401200960926     O                                         +  1 '<> da quello indica'
401300960926     O                                         +  0 'to:'
401400040607     O                       WCOL          4   +  0
401500960926     O                                         +  0 '/'
401600040607     O                       WSPM          4   +  1
401700960926     O                       WNRDOC             132
401800961016     O*------------------------------------
401900961016     O          E            AVVCED         1
402000961016     O                                           15 'ATTENZIONE !!!!'
402100961016     O                                         +  1 'AVVISARE CED DI SEDE'
402200961016     O                                         +  2 'PRECEDENTE ELABORAZIONE '
402300961016     O                                              'TERMINATA IN MODO ANOMAL'
402400961016     O                                              'O !!!!'
402500040227     O          E            AVVCED         1
402600040227     O                                           15 'ATTENZIONE !!!!'
402700040227     o                                         +  1 'Controllare i membri'
402800040227     o                                         +  1 'del TRIFCSUM nella'
402900040227     o                                         +  1 'libreria di gruppo.'
403000960527     O          E            STAFIN           60
403100960401     O                                           76 '*** FINE STAMPA ***'
403200960401     O*----------------------------------------------------------------
403300960628     O*  Stampa dati ricevuti dal partner
403400960628     O*----------------------------------------------------------------
403500960628     OSYSPRT    E            TEST1            01
403600960628     O                       RSUT                20
403700960628     O                                           48 '** STAMPA DATI RICEVUTI'
403800960917     O                                         +  1 'IN FASE DI IMPORTAZIONE'
403900960628     O                                              ' BOLLE DA PARTNER ESTER'
404000960628     O                                              'I **'
404100960628     O                       DATA               121 '  /  /  '
404200960628     O                                          127 'Pag.:'
404300960628     O                       PAGE          Z    132
404400960628     O          E            TEST1          2 02
404500960628     O                       WORA               121 '  :  :  '
404600960628     O*  Dati messaggio
404700960725     O          E            DATMSG         1
404800960725     O                       CMP132             132
404900960628     O          E            DATMSG         1
405000960628     O                                           18 'DATI MESSAG: MITT.'
405100960628     O                       TA0004            +  1
405200960628     O                       TA0007            +  1
405300960628     O                                         +  1 'DESTIN.'
405400960628     O                       TA0010            +  1
405500960628     O                       TAA007            +  1
405600960628     O                                         +  1 'DATA'
405700960712     O                       WDTMSG        Y   +  1
405800960628     O                                         +  1 'ORA'
405900960628     O                       TA0019            +  1 '  :  '
406000960628     O          E            DATMSG         1
406100960712     O                                           18 'DOCUMENTO..: NOME:'
406200960628     O                       TA1000            +  1
406300960712     O                                         +  6 'NUMERO:'
406400960628     O                       TA1004            +  1
406500960628     O                                         +  1 'COD.'
406600960628     O                       TA1001            +  1
406700960712     O                                         +  1 'FUNZIONE'
406800960628     O                       TA1225            +  1
406900960628     O*  Note intero documento
407000960628     O          E            NOTMSG      1
407100960628     O                                           18 'NOTE DOCUMENTO...:'
407200960628     O                       TB4451            +  1
407300960628     O          E            NOTMS1         1
407400960628     O                       WNOT              +  1
407500960628     O*  Totali colli/spedizioni ....
407600960628     O          E            TOTDOC      1  1
407700960712     O                                           12 'TOT.SPEDIZ.:'
407800960628     O                       WTOTSP            +  1 '          .   .   '
407900960628     O                                         +  1 'TOT.COLLI:'
408000960628     O                       WTOTCO            +  1 '          .   .   '
408100960628     O                                         +  1 'TOT.PESO NETTO:'
408200960628     O                       WTOTPN            +  1 '          .   .   '
408300960628     O                                         +  1 'TOT.PESO LORDO:'
408400960628     O                       WTOTPL            +  1 '          .   .   '
408500960628     O*  Nr.Documento/data
408600960712     O          E            CMRDOC         1
408700960712     O                                           12 'NUMERO CMR.:'
408800960628     O                       TD1154            +  1
408900960712     O                                           67 'DATA..:'
409000960712     O                       WSTCMR        8   +  1
409100960712     O                                          108 'ORA:'
409200960628     O                       WHHCMR            +  1 '  :  '
409300960628     O          E            FVVDOC         1
409400960725     O                                           12 'F. VIAGGIO.:'
409500960628     O                       TD1154            +  1
409600960712     O                                           67 'DATA..:'
409700960712     O                       WSTDFV        8   +  1
409800960712     O                                          108 'ORA:'
409900960702     O                       WHHNFV            +  1 '  :  '
410000960628     O*  Dati trasporto
410100960712     O          E            DATTRA         1
410200960712     O                                           12 'TIPO TRASP.:'
410300960628     O                       TE8051            +  1
410400960628     O                                         +  1 'MODAL.:'
410500960628     O                       TE8067            +  1
410600960628     O                                         +  2 'VETTORE:'
410700960628     O                       TE3128            +  1
410800960628     O                                         +  1 'ID.TRASPORTO:'
410900960628     O                       TE8212            +  1
411000960628     O*  Luogo di partenza/arrivo
411100960628     O          E            DATLOC      1
411200960712     O                                           14 'LOC.PARTENZA.:'
411300960628     O                       WLOCPA            +  1
411400960628     O                                         +  1 'DATA'
411500960913     O                       WSTDPA        8   +  1
411600960628     O                                         +  1 'ORA'
411700960712     O                       WHHPA             +  1 '  :  '
411800960628     O                                         +  1 'LOC.ARRIVO:'
411900960628     O                       WLOCAR            +  1
412000960628     O*  Dati Partner
412100960712     O          E            DATPAM         1
412200960712     O                                           14 'PARTNER MITT.:'
412300960628     O                       TF3039            +  1
412400960628     O          E            DATPAD         1
412500960712     O                                           14 'PARTNER DEST.:'
412600960628     O                       TF3039            +  1
412700960628     O*  Dati Automezzo
412800960628     O          E            DATAUT         1
412900960628     O                                           13 'TP.AUTOMEZZO:'
413000960628     O                       TG8260            +  1
413100960628     O                                         +  1 'NR.PALLETS:'
413200960712     O                       TG6350        4   +  1
413300960628     O                                         +  1 'NR.PIOMBO:'
413400960702     O                       TG9308            +  1
413500960628     O*  Spedizione
413600960913     O          E            DATSPE         1
413700960712     O                       CMP132             132
413800960712     O          E            DATSPE         1
413900040310     O                                           13 'NR.SPEDIZ...:'
414000960628     O                       DA1004            +  1
414100960628     O*  Istruzioni/Note Mittente
414200960712     O          E            NOTSPE         1
414300960628     O               04                          13 'ISTRUZIONI..:'
414400960628     O               05                          13 'NOTE MITTENTE'
414500960628     O          E            NOTSP1         1
414600960628     O                       WNOTSP              85
414700960628     O*  Riferimenti consegna: AGE,CMR....
414800960712     O          E            RIFCON         1
414900960702     O                                           12 'RF.CONSEGNA:'
415000960628     O                       DC1153            +  1
415100960628     O                       DC1154            +  1
415200960628     O                       DCA153            +  1
415300960628     O                       DCA154            +  1
415400960628     O                       DCB153            +  1
415500960628     O                       DCB154            +  1
415600960628     O*  Soggetti transazione: CZ, CN, SF, ...
415700960712     O          E            SOGTRA         1
415800960913     O               07                          14 'DESTINATARIO.:'
415900960913     O              N07                          14 'MITTENTE.....:'
416000960702     O                       DD3035            +  1
416100960712     O                       DD3055            +  2
416200960712     O              N04      DD3207            +  1
416300960712     O              N04      DD3039            +  1
416400960712     O                       DD3036            +  5
416500960712     O              N04      DDA036            +  1
416600960712     O          E   N04      SOGTRA         1
416700960628     O                       DD3042              50
416800960628     O                       DDA042            +  1
416900960628     O                       DD3164            +  1
417000960628     O                       DD3251            +  1
417100960628     O*  Rivolgersi a...
417200960628     O          E            SOGRIV         1
417300960628     O                                           14 'RIVOLGERSI A.:'
417400960628     O                       DD3412            +  1
417500960628     O                       DD3155            +  1
417600960628     O                       DD3148            +  1
417700960702     O                       DDA155            +  1
417800960702     O                       DDA148            +  1
417900960628     O*  Dati articolo
418000960628     O          E            DATART         1
418100960628     O                                           14 'NR.ARTICOLO..:'
418200960628     O                       DE7140            +  1
418300960628     O                       DE7143            +  1
418400960628     O                       DE4440            +  1
418500960917     O*  Dati colli    .
418600960917     O          E            DATCOL         1
418700960917     O                                           14 'NUMERO COLLI.:'
418800960917     O                       VABNCL        4   +  1
418900960917     O                                         +  3 'PESO.:'
419000960917     O                       VABPKB        2   +  1
419100960917     O                                         +  2 'VOLUME:'
419200990616     O                       PVLB          2   +  1
419300960712     O          E            LETVET         1
419400960712     O                                           14 'NS LETT.VETT.:'
419500960712     O                       VABAAS        4   +  1
419600040302     O                       VABlnp        4   +  1
419700960712     O                       VABNRS        4   +  1
419800960712     O                       VABNSP        4   +  1
419900960918     O                                         +  2 'CONTRASSEGNO.:'
420000960918     O                       VABCAS        2   +  1
420100960918     O                       VABVCA            +  1
420200960628     O*  Nr.segnacolli
420300960725     O          E            DATSGN
420400960628     O                                           14 'NR.SEGNACOLLI:'
420500960628     O                       SGN1                50
420600960628     O                       SGN2              +  1
420700960628     O                       SGN3              +  1
420800961122** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
420900960725ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
421000960527** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
421100960725987654321098765432109876540123456789987654321098765432109876540999999999999999 1
421200960527** CN
421300960731Rif.mittente msg. non codificato in tabella Partner Esteri                     1
421400960702Data documento CMR non valida                                                  2
421500960626Numero CMR obbligatorio ma non immesso                                         3
421600960530Numero spedizione passato dal cliente non valido                               4
421700960527Data consegna richiesta non valida                                             5
421800960527Priorità spedizione non codificata in tabella TS                               6
421900960527Termine consegna non codificato in tabella TB                                  7
422000960527Non indicato alcun termine di consegna                                         8
422100960702Data partenza non valida                                                       9
422200960527C.A.P. destinatario non valido                                                10
422300960527C.A.P. mittente originale non valido                                          11
422400960527Campo importo non numerico                                                    12
422500960527Qualificatore numero colli non numerico                                       13
422600960527Numero colli non numerico                                                     14
422700960527Peso della spedizione non numerico                                            15
422800960527Volume della spedizione non numerico                                          16
422900980219Numero segnacolli passati è diverso da quello indicato                        17
423000960527Non trovato codice trattamento merce corrispondente                           18
423100960527Non trovato numeratore per attribuzione nr.spedizione                         19
423200960527Valuta importo non codificata in tabella CV                                   20
423300960702Data foglio viaggio non valida                                                21
423400960926Totale colli/spedizioni msg. <> da indicato                                   22
423500960731Numero AFB obbligatorio ma non immesso                                        23
423600960911Codice servizi speciali inesistente                                           24
423700960912Ricevuta da partner lnp non numerica. Impostata da tab.PT                     25
423800960912Linea di partenza del segnacollo diversa da quella tabella PT                 26
423900960912Ricevuta da partner lna non numerica. Ricalcolata da CAP                      27
424000960912Ricevuto numero serie non numerico. Ricalcolata da tab.PT                     28
424100960912Numero di serie del segnacollo diversa da quella tab.PT.                      29
424200990715Ricevuta da partner zona consega non numerica: SEGNACOLLO IGNORATO            30
424300990715Ricevuta da partner nr.segnacollo non numerico: SEGNACOLLO IGNORATO           31
424400961122Codice inesistente nella tabella dei clienti - tariffe                        32
