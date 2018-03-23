000100170609      *-------------------------------------------------------------------------*
000200990628      *    INTERROGAZIONE BOLLE SEDE - VISUALIZZAZIONE SPEDIZIONE               *
000300980618      *-------------------------------------------------------------------------*
000400980715
000500990618      ****************************************************************
000600990618      *  RIEPILOGO INDICATORI
000700990618      ****************************************************************
000800041007      * 01 - ABILITO  F1=Immissione Richiesta Assistenza Clienti
000900990702      * 02 - ABILITO  F2=Comunicazione di Anomalia
001000990630      * 03 -
001100010221      * 04 - ABILITO  F4=ORM
001200990702      * 05 - ABILITO  F5=Eventi
001300990630      * 06 -
001400170515      * 07 -
001500000901      * 08 - ABILITO  F8=Cod."CHI SONO"
001600040603      * 09 - ABILITO  F9=Transiti
001700990702      * 10 - ABILITO  F10=Variazioni
001800030120      * 11 - ABILITO  F11=Note Supermercato
001900990630      * 12 -
002000991117      * 13 - ABILITO  F13=Tassazione
002100990702      * 14 - ABILITO  F14=Giacenza
002200040416      * 15 - Abilito  F15=Gestione reclamo
002300990702      * 16 - ABILITO  F16=Competenze di C/E
002400000417      * 17 - ABILITO  F17=Richiesta rigenerazione bolla
002500990702      * 18 - ABILITO  F18=Legami
002600040119      * 19 - ABILITO  F19=Immagine LdV
002700010221      * 20 -
002800990702      * 21 - ABILITO  F21=Dettaglio Colli
002900990702      * 22 - ABILITO  F22=C/Assegno
003000020225      * 23 - Abilito  F23=AWB FedEx
003100020225      * 24 - Abilito  F24=Altri Tasti
003200011029      **
003300990630      * 25 - GESTIONE SUBFILE
003400990630      * 26 - GESTIONE SUBFILE
003500990706      * 27 - PREMUTO UN TASTO FUNZIONALE
003600990618      * 28 - ERRORE GENERICO DSPF
003700990630      * 29 -
003800990618      * 30 - COMODO
003900990618      * 31 - COMODO
004000030120      * 32 - Comodo
004100030402      * 33 - Comodo
004200990630      * 34 -
004300991117      * 35 - PROFILO NON ABILITATO A: F13, F16 e Totale Fattura(nel DSPF)
004400990630      * 36 -
004500990630      * 37 -
004600990630      * 39 -
004700990701      ***  Video
004800990628      * 40 - PRESENTE ora consegna reale
004900990708      * 41 - SPENTO Fattura Immediata, ACCESO Fattura fine mese
005000990701      * 42 - PRESENTE ora arrivo ultimo collo
005100990702      * 43 - PRESENTE ora consegna richiesta
005200990705      * 44 - PRESENTE ora consegna teorica
005300990707      * 45 - ABILITO decodifica particolarità giacenza
005400990707      * 46 - ABILITO decodifica particolarità consegna
005500990707      * 47 - ABILITO decodifica particolarità varie
005600040608      * 48 - non visualizzo data entrata transito
005700040608      * 49 - non visualizzo data uscita transito
005800990720      * 50 - EMETTO tipo inserimento per importo da assicurare
005900991123      * 51 - EMETTO mittente originale
006000000222      * 52 - EMETTO dati arrivo DPD
006100030402      * 53 - R.I. x bolla legata precedente richiesta dall'utente
006200030402      * 54 - N.D. x bolla legata seguente quando c'è l'originale
006300030402      * 55 - R.I. x bolla legata seguente richiesta dall'utente
006400030402      * 56 - N.D. x bolla legata campo scelta
006500030408      * 57 - Visualizzo 2° intestazione
006600090402      * 58 - Visualizzo note partenza record 8 e 9 di tita4
006700090402      * 59 - Visualizzo data manifest per bolle export
006800990623      ***  Video 2
006900070524      ***  Video 13 Tassazionne
007000070524      * 61 - R.I. porto tassato prima della fatturazione
007100070524      * 62 - R.I. imponibile tassato prima della fatturazione
007200070524      * 63 - R.I. IVA calcolata prima della tassazione
007300070524      * 64 - R.I. subfile delle varie
007400120330      * 65 -
007500120330      * 66 -
007600120330      * 67 - visualizza dati DDT al mittente
007700121022      * 69 - Visualizza errore doc. al mittente SB51W02
007800151016      * 70 - ABILITA visualizzazione Statistica Pesi/Volumi x cliente
007900121217      * 71 - Abilita visualizzazione bolla recupero/originale per variazioni post-fatturazione
008000160621      * 72 - Visualizza '?' per Ottico fattura
008100170522      * 73 - Visualizza Franco Frontiera se presente
008200170522      * 74 - Visualizza i bancali se presenti
008300180126      * 75 - segnacolli non sequenziali
008400180126      * 76 - bolla con colli incompatibili
008500990618      ***
008600990618      * 90 - riemissione videata
008700990618      ****************************************************************
008800990618
008900990628     H DECEDIT('0,') DATEDIT(*DMY/)
009000990618
009100990705     FTNSB51D   CF   E             WORKSTN  infds(INFDS51D)
009200120302     F                                      maxdev(*file)
009300040603     f                                      sfile(sb51s09:nrr09)
009400990705     F                                      sfile(SB51S11:NRR11)
009500990705     F                                      sfile(SB51S13:NRR13)
009600990705     F                                      sfile(SB51S21:NRR21)
009700000901     F                                      sfile(SB51S08:NRR08)
009800020226     f                                      sfile(sb51s23:nrr23)
009900030402     f                                      sfile(sb51s18:nrr18)
010000180126     F                                      sfile(SB51S24:NRR24)
010100990623     FTITAS30C  IF   E           K DISK
010200990628     FTABEL00F  IF   E           K DISK
010300990629     FTITA430C  IF   E           K DISK
010400171023     Ffipnd01l  IF   E           K DISK
010500990622     FTITAA30C  IF   E           K DISK
010600990630     FTITAT30C  IF   E           K DISK
010700990702     FTITA730C  IF   E           K DISK
010800000901     FTITAH30C  IF   E           K DISK
010900170118     F*TITASV1L  IF   E           K DISK     prefix(V)
011000161006     Ffiarbf2c  IF   E           K DISK
011100990630     FTNCSB03L  IF   E           K DISK
011200110428     F*TNBLA01L  IF   E           K DISK
011300010312     FECCET30C  IF   E           K DISK    usropn
011400040809     F**""*FNDCT02L  IF   E           K DISK
011500040809     FFNDCT01L  IF   E           K DISK
011600990705     FFNDFA01L  IF   E           K DISK
011700990813     FFNEVB01L  IF   E           K DISK
011800050401     FTigcp51L  IF   E           K DISK
011900990618     fAZORG01L  IF   E           K DISK
012000021203     Ffiapd01l  if   e           k disk
012100020419     ffnorm01l  if   e           k disk    usropn
012200040603     fFiar531c  if   e           k Disk
012300030402     fFnlbl01l  if   e           k Disk
012400030402     fFnlbl02l  if   e           k Disk    Rename(fnlbl000:fnlbl02)
012500160404     fFITGD32L  if   e           k disk
012600070524     fTitai01L  if   e           k disk                                         Immagine bolla tass
012700120404     ftntbe01l  if   e           k disk    USROPN extfile(Wtntbe)
012800990618      *------------------------------------------------------------------------*
012900020516
013000020516     D KKUT            S                   LIKE(TBLkut) inz(1)
013100990630     D KCOD            S                   LIKE(TBLcod)
013200990630     D KKEY            S                   LIKE(TBLkey)
013300990630     D KTRC            S                   LIKE(TAAtrc)
013400990630     D Wtbluni         S                   LIKE(TBLuni)
013500990628     D Wlinea          S                   LIKE(TASlnp)
013600990628     D Wtstdec         S                   LIKE(§CVfdc)
013700990813     D Wtasdcp         S                   LIKE(TASDCP)
013800010404     D Wta4not         S                   LIKE(TA4NOT)
013900020225     d wtahtrc         s                   like(tahtrc)
014000030120     d wAr5Trd         s                   Like(Ar5Trd)
014100030703     d wtba            s                   Like(§Ar5Tba)
014200030703     d wdestba         s                   Like(V0dTba)
014300020419
014400020419     d kormpoe         s                   like(ormpoe)
014500020419     d kormnsr         s                   like(ormnsr)
014600020419     d kormnor         s                   like(ormnor)
014700020419     d kormnrv         s                   like(ormnrv)
014800021203     d kpdpdr          S                   LIKE(APDpdr)
014900021203     d kpdtip          S                   LIKE(APDtip)
015000030402     d SavAas          s                   Like(I51Aas)
015100030402     d SavLnp          s                   Like(I51Lnp)
015200030402     d SavNrs          s                   Like(I51Nrs)
015300030402     d SavNsp          s                   Like(I51Nsp)
015400030402     d SavTbl          s                   Like(I51Tbl)
015500121217     d waas_leg        s                   Like(I51Aas)
015600121217     d wlnp_leg        s                   Like(I51Lnp)
015700121217     d wnrs_leg        s                   Like(I51Nrs)
015800121217     d wnsp_leg        s                   Like(I51Nsp)
015900030402     d kLblAao         s                   Like(LblAao)
016000030402     d kLblLpo         s                   Like(LblLpo)
016100030402     d kLblNro         s                   Like(LblNro)
016200030402     d kLblNso         s                   Like(LblNso)
016300030402     d kTasAas         s                   Like(TasAas)
016400030402     d kTasLnp         s                   Like(TasLnp)
016500030402     d kTasNrs         s                   Like(TasNrs)
016600030402     d kTasNsp         s                   Like(TasNsp)
016700070524     d ksvt            s                   Like(TaiSvt)
016800120330     D KCODE           S                   LIKE(TBecod)
016900120330     D Kke1            S                   LIKE(TBeke1)
017000120330     D Kke2            S                   LIKE(TBeke2)
017100990628
017200000309     D callricors      s              1
017300000309     D wtibs69r        s              1
017400990707     D wsb51d01        s              1
017500990705     D wsb51d02        s              1
017600990705     D wsb51d07        s              1
017700990702     D wsb51d13        s              1
017800990702     D wsb51d20        s              1
017900990705     D wsb51d21        s              1
018000000901     D wsb51d08        s              1
018100030121     d wsb51z00        s              1  0 Inz(1)
018200020226     d wsb51d23        s              1
018300180126     d wsb51d24        s              1
018400030402     d wsb51d18        s              1
018500040603     d wsb51d09        s              1
018600990706     D wunaca          s              1
018700990702     D W001A           s              1
018800030402     d w001a18         s              1
018900121217     d w001aleg        s              1
019000990702     D W017A           s             17
019100990629     D W020A           s             20
019200990628     D IMPeditato      s             13
019300991021     D  wfatimmed      s              1
019400020516     d wPOa            s              3    inz
019500020516     d wKSCa           S              7    inz
019600020516     d wabi            s                   like(UTEaut)  inz
019700170515     d wabim           s                   like(UTEaut)  inz
019800030120     d wIn             s              1
019900030121     d wOnOff          s              2  0
020000030121     d yy              s              2  0
020100030121     d wTara           s                   Like(§QtTpc)
020200030121     d wNtara          s                   Like(TasPkf)
020300990628
020400990618     D xx              S              3  0
020500990628     D W0080           S              8  0
020600990629     D w0100           s             10  0
020700990629     D w0130           s             13  0
020800990629     D w0133           s             13  3
020900990630     D w0140           s             14  0
021000990618     D Wdtgio          S              8  0
021100990618     D dateu           S              8  0
021200990705     D nrr11           S              4  0
021300990705     D nrr13           S              4  0
021400990705     D nrr21           S              4  0
021500180126     D nrr24           S              4  0
021600000901     D nrr08           S              4  0
021700020226     d nrr23           s              4  0
021800030402     d nrr18           s              4  0
021900040603     d nrr09           S              4  0
022000990628     D Wksc4           S              4  0
022100990628     D Wccm4           S              4  0
022200021119     d LnaNtw          s                   Like(§ogNtw)
022300030402     d wa              s              3  0
022400030402     d wb              s              3  0
022500040604     d woktrs          s              1    inz(*Off)
022600120829     d wtrovl          s              1    inz(*Off)
022700161209     D oStato          S              5I 0
022800170621     D ch_titas        S              1
022900990618
023000990618     D DATA_eur        S               D   DATFMT(*eur)
023100120330     D DATAiso         S               D   DATFMT(*iso)
023200120330     D DATAdmy         S               D   DATFMT(*dmy)
023300110927
023400110927     d c_tibs20        s              1
023500040119
023600040119      * Parametri per chiamata a Laguna
023700040122     d Soc670          s              3    inz('201')
023800040119     d Cmd670          s             15    inz('VIEW           ')
023900040119A0976d Parm670         s             44    inz   dim(30)
024000040119     d Err670          s              1    inz
024100040119     d Ret670          s             32A   inz
024200990618      *
024300990618      *   S C H I E R E
024400120709     D MSG             S             78    DIM(23) CTDATA PERRCD(1)             MSG VIDEO
024500170515     d tf              s             11    dim(14) ctdata perrcd(1)             tasti funzione
024600030121     d SkInd           s              2  0 Dim(15)
024700030121     d SkOnOff         s              1    Dim(15)
024800170515     d Vtf             s             11    Dim(5)
024900040119      * P.O. gestibili dall'utente
025000170109     D*$POG            s              3    dim(250) inz(*zeros)
025100030402
025200030402     d Aan             S              4  0 DIM(30)                              ANNO BOLLA SEGUENTE
025300030402     d Lpn             S              3  0 DIM(30)                              LNP BOLLA SEGUENTE
025400030402     d Nrn             S              2  0 DIM(30)                              SERIE BOLLA SEGUENTE
025500030402     d Nsn             S              7  0 DIM(30)                              SPEDIZ. BOLLA SEGUEN
025600990618      *
025700990618      *   D S   I N T E R N E / E S T E R N E
025800990618      *
025900990630     D INFDS51D        DS
026000990630     D  INFDSFMT             261    270
026100990618      *
026200010221     D* VISUALIZZAZIONE ORM - FIOR07R
026300010221     D Parm01          DS
026400010221     D  ppoe                          3  0
026500010221     D  pnor                          7  0
026600010221     D  pnsr                          2  0
026700010221     D  pnrv                          2  0
026800010221     D  psce                          1
026900010221     D  pfgs                          3  0
027000010221     D  ppor                          3  0
027100010221     D  pdtr                          8  0
027200010221     D  pmdb                         10
027300010221     D  pprb                         10
027400010221     D  pdts                          8  0
027500010221     D  prmp                          1
027600010221     D  pbrc                          1
027700010221     D  pref                          2
027800010221     D  pmio                          1
027900990813      *  DS per richiamo PGM visualizzazione legami bolla
028000990813     d LRB5DS          DS
028100990813     d  pb5aas                14     17  0
028200990813     d  pb5lnp                18     20  0
028300990813     d  pb5nrs                21     22  0
028400990813     d  pb5nsp                23     29  0
028500990813     d  pb5f03                30     30
028600990813     d  pb5flg                31     31
028700990813     d  pb5pgm                32     41
028800990813     d  pb5dsa                42     61
028900990813     d  pb5dsp                63     70  0
029000990813     d  pb5rmn                71     85  0
029100990813     d  pb5lna                86     88  0
029200990813     d  pb5lnad               89    108
029300990813     d  pb5rsd               109    143
029400990813     d  pb5dcp               144    151  0
029500990813     d  pb5tmc               152    152
029600160930      *  DS per richiamo PGM interrogazione sotorico variazioni unico
029700160930     d LR75DS          DS
029800160930     d  p75aas                14     17  0
029900160930     d  p75lnp                18     20  0
030000160930     d  p75nrs                21     22  0
030100160930     d  p75nsp                23     29  0
030200160930     d  p75f03                30     30
030300160930     D  P75FLG                31     31
030400160930     D  P75PGM                32     41
030500160930     D  p75DSA                42     61
030600160930     D  P75TBO               148    148
030700160930     d  p75tbl               149    150
030800040119      *
030900040416     d ds_Spedizione   ds
031000040119     d   Waas20                       2  0
031100040119     d   TASlnp
031200040119     d   TASnrs
031300040119     d   TASnsp
031400040416     d   TASaas
031500040416     d  ds_KEYsped             1     14
031600040416     d  ds_OggSped             3     18
031700990701      *
031800990623     D KPJBA         E DS
031900040416     d FIDNA1ds      e ds
032000040416      *
032100040416     d FIDNA2ds      e ds                  inz
032200041011     d*****************************************  IA2mod       e                     inz('I')
032300040416     d  IA2tor       e                     inz('S')
032400040416      *
032500000418     D TIBS20DS      E DS                  INZ
032600990701     D TNSB01DS      E DS                  INZ
032700990701     D TNSB51DS      E DS                  INZ
032800990707     D TNSC40DS      E DS
032900990707     D FIDN00DS      E DS
033000990714     D FNLV16DS      E DS                  INZ
033100170609     D FNLV79DS      E DS                  INZ
033200051129     D FNLV80DS      E DS                  INZ
033300990729     D DTASV         E DS                  INZ   prefix(§)
033400990707     D DS7PQRS       E DS
033500990621     d TIBS69DS      E DS                  INZ
033600990618     d DS_cnaco      E DS                  extname(CNACO00F)
033700990618     d DS_cnind      E DS                  extname(CNIND00F)
033800990618     d DS_cnclp      E DS                  extname(CNCLP00F)
033900990618     d DS_fncls      E DS                  extname(FNCLS00F)
034000990901     D Fnlg30ds      E DS
034100170726     D*DSQC2         E DS
034200990714     D DSTB          E DS
034300990629     D DSCV          E DS
034400990630     D WDSCV         E DS                  extname(DSCV)  prefix(W)
034500990629     D DTA4A         E DS
034600090402     D DTA4k         E DS
034700070927     D DSBL4E        E DS
034800070927     D*DSBL4F        E DS
034900171023     D*DSBL4i        E DS
035000171023     D dpnddp4       E DS
035100010220     D DSBL4M        E DS
035200990702     D DS$2          E DS
035300020516     D TIBS02DS      E DS                  inz
035400020516     D TIBS34DS      E DS
035500020516     D DDATIUTE      E DS
035600020516     D AZUTEDS       E DS                  extname(AZUTE00F)
035700020516     D DLAT          E DS                  inz
035800020516     D DUTE01        E DS                  inz
035900170109     D*TRUL31DS      e ds                  inz
036000040119     d dTASflo       e ds                  inz
036100020516      *
036200020328     D DSCC          E DS
036300021119     d Og143         e ds
036400030121
036500030121     d Fnlrb3Ds      e ds
036600030121     d dAr5Ban       e ds
036700030703     d dAr5Bnb       e ds
036800031119     d dAr5Gen       e ds
036900120711     d dAr5doc       e ds
037000040603     d dAr5Trs       e ds
037100151016     d dAr5Fat       e ds                  qualified   inz
037200030121     d dTba          e ds
037300030121     d dsQt1         e ds
037400030624     d ds3a          e ds
037500061108     d dta4q         e ds
037600090605     d ds5e          e ds
037700180129     d darsj         e ds
037800040809
037900040809      * ds per il controllo della presenza di CA per la spedizione richiesta
038000040809     d FIDN12DS      e ds
038100040809     d  skKey                 26    305    dim(20)
038200040809     d  skAnn                306    325    dim(20)
038300040809     d  skDca                326    485  0 dim(20)
038400040809     d  skFca                486    545  0 dim(20)
038500040809     d  skDch                546    705  0 dim(20)
038600040809     d  skCch                706    745    dim(20)
038700151016
038800151020      *?Visualizzazione Pesi Usati in Tassazione?
038900151020     d TNSB56ds      e ds                  inz
039000161229
039100170109     d* Controllo abilitazioni utente
039200170109     d tntaa1ds      e ds
039300160621
039400160621     d pcmdpc          s            128a   inz
039500160621     d ppause          s              4a   inz
039600160621     d puid            s             24a   inz
039700160621     d pnfat           s              9a   inz
039800151016
039900040809     d*
040000040809     d dsKey           ds
040100040809     d  dsaac                         4  0
040200040809     d  dsfil                         3  0
040300040809     d  dsnca                         7  0
040400020226
040500990618     D                SDS
040600990628     D  VTCPGM                 1     10
040700040416     d  SDSprm           *parms
040800990618
040900030120      * Costanti
041000170517     d titolo_I        c                   const('* VISUALIZZAZIONE BOLLA SEDE -
041100170517     d                                     *')
041200170518     d titolo_M        c                   const('*    GESTIONE BOLLE DI SEDE  -
041300170517     d                                     *')
041400030120     D CF2413          C                   CONST('F24=AlTasti(1/3)')
041500030120     D CF2423          C                   CONST('F24=AlTasti(2/3)')
041600030120     D CF2433          C                   CONST('F24=AlTasti(3/3)')
041700030120     D CF2412          C                   CONST('F24=AlTasti(1/2)')
041800030120     D CF2422          C                   CONST('F24=AlTasti(2/2)')
041900120404
042000120404     d Wtntbe          s             21    inz('FILTRAGRU /TNTBE01L')
042100160621
042200160621     d comandpc        c                   const('https://ecm.brt.it/b/b.asp?Pa-
042300160621     d                                     rams=SYN_INVO_UNIQUEID=')
042400170517     D* CAMPO PER DSPATR "RI"
042500170517     D RI              C                   CONST(X'21')
042600170517
042700170517     d Atrnorm         c                   const(x'20')
042800160621
042900160621      //---------------------------------------------------------------
043000160621      //?Definizione procedure usate.
043100160621      //---------------------------------------------------------------
043200160621      // - Interrogazione fattura Docflow
043300160621     d TNSB51C1        pr                  extpgm('TNSB51C1')
043400160621     d  pcmdpc                      128
043500160621     d  ppause                        4
043600030120
043700990618      *------------------------------------------------------------------------*
043800990618      *---------------  C A L C O L O  ----------------------------------------*
043900990618      *------------------------------------------------------------------------*
044000030402
044100030402     c     Inizio        Tag
044200030402      * Provengo da interrogazione legami bolla
044300121217     c                   If        w001a18 = 'S'  or w001aleg='S'
044400030402     c     Keyspe        Setll     titas30c
044500030402     c     Keyspe        Reade     Titas30c
044600030402     c                   Eval      i51Tbl = tastbl
044700030408     c                   Eval      *In57 = *On
044800030418      * devo pulire tutte le videate
044900030418     c                   Exsr      Sr_Clear
045000030402     c                   EndIf
045100990618
045200990628     c     KEYSPE1       CHAIN     TITAS30C                           30
045300120709     c                   if        not %found(titas30c)
045400120709     c     KEYSPE        CHAIN     TITAS30C                           30
045500120709     c                   if        %found(titas30c)
045600120709     c                   eval      o51err='1'
045700120709     c                   eval      o51msg=msg(23)
045800120709     c                   endif
045900120709     c                   else
046000120709     c******             IF        %FOUND
046100990630
046200991117      * Verifico profilo per abilitazioni
046300170621     c                   if        ch_titas<>'S'
046400991117     c                   EXSR      ABIPROFILO
046500170621     c                   endif
046600991117
046700991117      * Abilito tasti funzionali
046800991117     c                   EXSR      ABITASFUN
046900170621     c                   if        ch_titas='S'
047000170621     c                   eval      *inkl=*off
047100170621     c                   eval      wsb51d01=*blanks
047200170621     c                   endif
047300170621
047400170621     c                   clear                   ch_titas
047500990628
047600990630     C                   DO        *HIVAL
047700990630
047800990630      * Gestione tasti funzionali
047900990630     c                   SELECT
048000990630      * F3 = Fine - ESCO DAL PGM
048100990630     c                   WHEN      *INKC = *ON
048200990630     c                   eval      O51F03 = '1'
048300990630     c                   leave
048400030403      * F12 = Ritorno - se premuto non dalla prima bolla
048500030418      *                 ricarico la prima bolla
048600121217     c                   WHEN      *INKL=*ON and (w001a18 = 'S' or w001aleg='S')
048700121217     c                             and INFDSFMT = 'SB51D01'
048800030403     c                   Eval      I51Aas = SavAas
048900030403     c                   Eval      I51Lnp = SavLnp
049000030403     c                   Eval      I51Nrs = SavNrs
049100030403     c                   Eval      I51Nsp = SavNsp
049200030403     c                   Eval      I51Tbl = SavTbl
049300030403     c                   Clear                   w001a18
049400121217     c                   Clear                   w001aleg
049500030403     c                   Eval      *Inkl = *Off
049600030408     c                   Eval      *In57 = *Off
049700030418      * devo pulire tutte le videate
049800030418     c                   Exsr      Sr_Clear
049900030403     c                   GoTo      Inizio
050000990630      * F12 = Ritorno - se premuto nella videata di entrata ESCO DAL PGM
050100990630     c                   WHEN      *INKL = *ON  and  INFDSFMT = 'SB51D01'
050200990630     c                   eval      O51F12 = '1'
050300990630     c                   leave
050400990630      * F12 = Ritorno - se non premuto nella videata di entrata ritorno alla videata di entrata
050500990630     c                   WHEN      *INKL = *ON
050600170621     c                   if        ch_titas='S'
050700170621     c                   goto      inizio
050800170621     c                   endif
050900990630     c                   exsr      SUB_FMT001
051000041007      *?F01 = Aggiunta Richiesta Assistenza clienti
051100041007     c                   WHEN      *inKA = *on
051200041007     c                   exsr      Sub_F01
051300041007     c                   leave
051400990630      * F2 = C.A.
051500990630     c                   WHEN      *INKB = *ON
051600990630     c                   exsr      SUB_F02
051700121121     c                   IF        *in28
051800121121     c                   eval      *inkb = '0'
051900121121     c                   ENDIF
052000010221      * F4 = ORM
052100010221     c                   WHEN      *INKD = *ON
052200010221     c                   exsr      SUB_F04
052300990630      * F5 = Eventi
052400990630     c                   WHEN      *INKE = *ON
052500990630     c                   exsr      SUB_F05
052600170515      * F6 = Manutenzione bolla
052700170515     c                   WHEN      *INKF = *ON
052800170515     c                   exsr      SUB_F06
052900990630      * F7 = Altri Dati
053000990630     c                   WHEN      *INKG = *ON
053100990630     c                   exsr      SUB_F07
053200000901      * F8 = Cod."CHI SONO"
053300000901     c                   WHEN      *INKH = *ON
053400000901     c                   exsr      SUB_F08
053500040603      * F9 = Transiti
053600040603     c                   WHEN      *INKI = *ON
053700040603     c                   exsr      SUB_F09
053800990630      * F10 = Variazioni
053900990630     c                   WHEN      *INKJ = *ON
054000990630     c                   exsr      SUB_F10
054100030120      * F11 = Note Supermercato
054200030120     c                   WHEN      *INKK = *ON
054300030120     c                   exsr      SUB_F11
054400990630      * F13 = Tassazione
054500990630     c                   WHEN      *INKM = *ON
054600990630     c                   exsr      SUB_F13
054700990630      * F14 = Giacenze
054800990630     c                   WHEN      *INKN = *ON
054900990630     c                   exsr      SUB_F14
055000040416      *?F15 = Gestione reclami?
055100040416     c                   WHEN      *inKP = *on
055200040416     c                   exsr      Sub_F15
055300990630      * F16 = Competenze Conto Economico
055400990630     c                   WHEN      *INKQ = *ON
055500990630     c                   exsr      SUB_F16
055600000417      * F17 = Richiesta rigenerazione spedizione
055700000417     c                   WHEN      *INKR = *ON
055800000417     c                   exsr      SUB_F17
055900990630      * F18 = Legami
056000990630     c                   WHEN      *INKS = *ON
056100990630     c                   exsr      SUB_F18
056200040119      * F19 = Immagine LdV
056300110926     c                   WHEN      *INKT = *ON  or
056400111129     c****                         (*inkf=*on and %subst(knmus:1:3)= 'EDP')
056500111129     c                             *inkf
056600040119     c                   exsr      SUB_F19
056700990630      * F20 = Affidabilità
056800990630     c                   WHEN      *INKU = *ON
056900990630     c                   exsr      SUB_F20
057000990630      * F21 = Dettaglio Colli
057100990630     c                   WHEN      *INKV = *ON
057200990630     c                   exsr      SUB_F21
057300990630      * F22 = Contrassegni
057400990630     c                   WHEN      *INKW = *ON
057500990630     c                   exsr      SUB_F22
057600020225      * F23 = AWB FedEx
057700020225     c                   WHEN      *INKX = *ON
057800020225     c                   exsr      SUB_F23
057900020225      * F24 = Altri Tasti
058000020225     c                   WHEN      *INKY = *ON
058100020225     c                   exsr      SUB_F24
058200020226     c                   eval      *inky = *off
058300121217     c* ? su campo per visualizzazione legame bolla originale/di recupero variaz.
058400121217     c                   when      *in71 and v1cleg='?'
058500121217     c                   clear                   v1cleg
058600121217     c                   Eval      I51Aas = waas_leg
058700121217     c                   Eval      I51Lnp = wlnp_leg
058800121217     c                   Eval      I51Nrs = wnrs_leg
058900121217     c                   Eval      I51Nsp = wnsp_leg
059000121217     c                   Eval      w001aleg = 'S'
059100121217     c                   clear                   infdsfmt
059200121217     c                   goto      inizio
059300151016      *?? per visualizzare i pesi usati in tassazione?
059400151016     c                   when      *in70 and V1PesTas = '?'
059500151016     c                   clear                   V1PesTas
059600151020     c                   clear                   TNSB56ds
059700151020     c                   eval      D56aas = TASaas
059800151020     c                   eval      D56lnp = TASlnp
059900151020     c                   eval      D56nrs = TASnrs
060000151020     c                   eval      D56nsp = TASnsp
060100151020     c                   eval      D56tbl = TAStbl
060200151022     c                   eval      D56ksc = TASksc
060300151022     c*//                eval      D56rsc = TAS...
060400151022     c                   eval      D56ctr = TASctr
060500151022     c*//                eval      D56dtr = §TRdes
060600151020     c                   eval      KPJBU  = TNSB56ds
060700151020     c                   call      'TNSB56R'
060800151020     c                   parm                    KPJBA
060900151020     c                   eval      TNSB56ds = KPJBU
061000151016     c                   clear                   InfDsFmt
061100151016     c                   goto      Inizio
061200151016     c*//                eval      *inKL = *on
061300180126      *?? per visualizzare i colli incompatibili%
061400180126     c                   when      *in76 and V1cinc   = '?'
061500180129     c                   clear                   v1cinc
061600180129     c                   exsr      sub_inc
061700180126
061800990630      * Emissione Videata di entrata
061900990630     c                   OTHER
062000990630     c                   exsr      SUB_FMT001
062100990630     c                   ENDSL
062200990618
062300990630     c                   ENDDO
062400990630     c                   ENDIF
062500990618
062600990618      * Chiudo pgm aperti
062700990618     C                   IF        Wtibs69r  <>  *BLANKS
062800990618     C                   eval      I69TLA  = 'C'
062900990618     C                   call      'TIBS69R'
063000990618     C                   parm                    TIBS69DS
063100990624     c                   ENDIF
063200040810     C*
063300040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
063400040810     c                   clear                   fidn12ds
063500040810     c                   eval      i12tla = 'C'
063600040810     c                   call      'FIDN12R'
063700040810     c                   parm                    fidn12ds
063800170109
063900170109     C                   clear                   tntaa1ds
064000170109     C                   movel     'C'           Itaa1Tla
064100170109     C                   movel(p)  tntaa1ds      kpjbu
064200170109     C                   CALL      'TNTAA1C'
064300170109     C                   PARM                    KPJBA
064400170314
064500170314     C                   MOVEL     'C'           P75FLG
064600170314     C                   MOVEL     lr75ds        KPJBU
064700170314     C                   CALL      'FNLR75R'                            99
064800170314     C                   PARM                    KPJBA
064900040810     c*
065000990618
065100990624      * Imposto campi per uscita con F03
065200990630     c                   MOVEL(P)  TNSB51DS      KPJBU
065300990630
065400990618     c                   EVAL      *INLR = *ON
065500991117      **********************************************************************
065600991117      * VERIFICO PROFILO PER ABILITAZIONI
065700991117      **********************************************************************
065800991117     C     ABIPROFILO    BEGSR
065900991117
066000020516     c                   eval      *in35  = *off
066100170515     c                   eval      *in06  = *off
066200020516     c                   clear                   wABI
066300170515     c                   clear                   wABIm
066400020516     c                   clear                   DLAT
066500020516
066600020516      *-- Verifica errori e autorità profilo
066700020516sel 1C                   SELECT
066800020516      *-- controllo se ho errori nei dati utente
066900020516      *--   nel qual caso NON risulta un profilo abilitato
067000020516w   1C                   WHEN      DUTERR = 'E'
067100020516     C                   eval      *IN35  = *on
067200020516
067300020516      *-- CONTROLLO AUTORITA'
067400020516      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
067500020516      *-- se il chiamante non richiede autorità specifica verificare
067600020516      *--   quella del profilo
067700020516      *-- se il chiamante richiede autorità specifica verificarla,
067800020516      *--  se è blank verificare quella del profilo
067900020516
068000020516      * se UTEAUT = *BLANK non siamo in sede
068100020516      *--     nel qual caso saranno attuate le protezioni del caso
068200020516w   1C                   WHEN      UTEAUT = *blank
068300020516     C                   eval      *in35  = *on
068400020516
068500020516x   1C                   OTHER
068600020516      *-----------------------------------------------------------------
068700020516     C                   movel     UTEFAF        DUTE01
068800020516      *-- Autorizzazione alla interrogazione delle bolle di sede
068900020516if  2C                   IF        §UTEibs <> *blanks
069000020516      *--   Con AUTORITÀ SPECIFICA
069100020516     C                   eval      wabi = §UTEibs
069200020516x   2C                   ELSE
069300020516      *--   Senza AUTORITÀ SPECIFICA
069400020516     C                   eval      wabi = UTEAUT
069500020516e   2C                   ENDIF
069600020516      *
069700020516     c                   clear                   tibs02ds
069800020516     c                   movel     'C'           t02mod
069900020516     c                   movel     knsif         t02sif
070000020516     c                   movel     'LAT'         t02cod
070100020516     c                   movel(p)  wabi          t02ke1
070200020516     c                   call      'TIBS02R'
070300020516     c                   parm                    kpjba
070400020516     c                   parm                    tibs02ds
070500020516if  2c                   if        t02err = *blanks
070600020516     c                   movel     t02uni        DLAT
070700020516e   2c                   endif
070800020516if  2c                   if        T02err <> *blanks   or
070900020516     c                             §LATabi = 'S'
071000020516     C                   eval      *in35  = *on
071100020516e   2c                   endif
071200170515     c* verifico anche l'autorizzazione alla manutenzione bolle di sede
071300170517     c                   if        %subst(i51op0:2:2)='02' and i51op1<>' 05'
071400170515     c                   seton                                        06
071500170515if  2C                   IF        §UTEmbs <> *blanks
071600170515      *--   Con AUTORITÀ SPECIFICA
071700170515     C                   eval      wabim= §UTEmbs
071800170515x   2C                   ELSE
071900170515      *--   Senza AUTORITÀ SPECIFICA
072000170515     C                   eval      wabim= UTEAUT
072100170515e   2C                   ENDIF
072200170515      *
072300170515     c                   clear                   tibs02ds
072400170515     c                   movel     'C'           t02mod
072500170515     c                   movel     knsif         t02sif
072600170515     c                   movel     'LAT'         t02cod
072700170515     c                   movel(p)  wabim         t02ke1
072800170515     c                   call      'TIBS02R'
072900170515     c                   parm                    kpjba
073000170515     c                   parm                    tibs02ds
073100170515if  2c                   if        t02err = *blanks
073200170515     c                   movel     t02uni        DLAT
073300170515e   2c                   endif
073400170515if  2c                   if        T02err <> *blanks   or
073500170515     c                             §LATabi = 'S'
073600170515     C                   eval      *in06  = *off
073700170515e   2c                   endif
073800170515     c                   endif
073900020516      *
074000020516e   1c                   ENDSL
074100020516      *
074200020516      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
074300020516      *
074400020516      * Utente con abilitazione valida e diversa da "AZ"
074500020516if  1c                   if        wABI  <> 'AZ' and not *in35
074600020516      *
074700170109if  2c**                 if        wABI  <> I31abi
074800170109     c**                 reset                   $POG
074900020516      * Reperimento dei P.O. gestibili dall'utente
075000170109     c**                 clear                   TRUL31DS
075100170109     c**                 eval      I31abi = wabi
075200170109     c**                 eval      I31cdi = DUTdis
075300170109     c**                 eval      I31car = DUTare
075400170109     c**                 eval      I31cpo = DUTpou
075500170109     c**                 call      'TRUL31R'
075600170109     c**                 parm                    KPJBA
075700170109     c**                 parm                    TRUL31DS
075800170109if  3c**                 if        O31pog > *zeros
075900170109     c**                 movea     O31pog        $POG
076000170109e   3c**                 endif
076100170109e   2c**                 endif
076200020516      *
076300020516      *  controllo se il P.O. di competenza per il cliente è compreso
076400020516      *    tra quelli gestibili dall'utente
076500170109     c**                 movel     TASksc        wPOa
076600170109     c**   wPOa          lookup    $POG                                   30
076700170109if  2c**                 if        not *in30
076800170109     c                   clear                   tntaa1ds
076900170109     c                   eval      itaa1caut='§uteibs'
077000170109     c                   eval      itaa1ksc=%int(tasksc)
077100170109     c                   clear                   kpjbu
077200170109     c                   movel     tntaa1ds      kpjbu
077300170109     c                   call      'TNTAA1C'
077400170109     c                   parm                    kpjba
077500170109     c                   movel     kpjbu         tntaa1ds
077600170109    2c                   if        otaa1fabi='N'
077700020516     c                   eval      *in35  = *on
077800020516e   2c                   endif
077900020516      *
078000020516e   1c                   endif
078100170515
078200170515      * Per la manutenzione:
078300170515      * Utente con abilitazione valida e diversa da "AZ"
078400170515if  1c                   if        wABIm <> 'AZ' and *in06
078500170515      *
078600170515      *
078700170515      *  controllo se il P.O. di competenza per il cliente è compreso
078800170515      *    tra quelli gestibili dall'utente
078900170515     c                   clear                   tntaa1ds
079000170515     c                   eval      itaa1caut='§utembs'
079100170515     c                   eval      itaa1ksc=%int(tasksc)
079200170515     c                   clear                   kpjbu
079300170515     c                   movel     tntaa1ds      kpjbu
079400170515     c                   call      'TNTAA1C'
079500170515     c                   parm                    kpjba
079600170515     c                   movel     kpjbu         tntaa1ds
079700170515    2c                   if        otaa1fabi='N'
079800170515     c                   eval      *in06  = *off
079900170515e   2c                   endif
080000170515      *
080100170515e   1c                   endif
080200020516
080300020516     C                   ENDSR
080400990630      **********************************************************************
080500990630      * ABILITO TASTI FUNZIONALI
080600990630      **********************************************************************
080700990630     C     ABITASFUN     BEGSR
080800040119
080900040119     c                   movel     TASflo        dTASflo
081000990630
081100000209      *  F2= Comunicazione di Anomalia, non abilitato se arrivo dalle C.A.
081200000209     c                   EVAL      *IN02 = (TASfda = 'S'
081300000209     c                                      and  %SUBST(I51OP0:1:1) <> 'D')
081400010221
081500010221      *  F4= ORM
081600111103     c                   clear                   wta4not
081700010221     c                   eval      ktrc = 'M'
081800010221     c     KTAStrc       CHAIN     TITA430C
081900020419 b1  C                   if        %found(tita430C)
082000020419 b2  C     ta4not        ifne      *blanks
082100020419     c                   eval      Wta4not = ta4not
082200020419     c                   open      fnorm01l
082300020419     c                   movel     wta4not       dsbl4m
082400020419     c                   move      §b4poe        kormpoe
082500020419     c                   move      §b4nsr        kormnsr
082600020419     c                   move      §b4nor        kormnor
082700020419     c                   move      §b4nrv        kormnrv
082800020419     c     kfnorm        setll     fnorm01l
082900020419 b3  c                   if        %equal(fnorm01l)
083000010221     C                   seton                                        04
083100020419 x3  C                   else
083200010221     C                   setoff                                       04
083300111103     c*****              clear                   Wta4not
083400020419 e3  C                   endif
083500020419     c                   close     fnorm01l
083600020419 e2  c                   endif
083700020419 x1  C                   else
083800010404     c                   clear                   Wta4not
083900010221     C                   setoff                                       04
084000020419 e1  C                   endif
084100990630
084200990630      *  F5= Eventi
084300990813     C     KEYspe        SETLL     FNEVB01L                               05
084400170515
084500000901
084600000901      *  F8= Cod."CHI SONO"
084700020225     c                   eval      wtahtrc = 'C'
084800020225     C     KEYspeh       SETLL     TITAH30C                               08
084900040603
085000040603      *  F9= Transiti
085100040604     c                   Eval      woktrs = *Off
085200040603     c                   Eval      *In09 = *Off
085300040603     c                   Eval      wAr5Trd = 'TRS'
085400040604     c     kSpeAr5       Setgt     Fiar531c
085500040604     c                   Do        *Hival
085600040604     c     kSpeAr5       Readpe    Fiar531c
085700040604     c                   If        %Eof(Fiar531c)
085800040604     c                   Leave
085900040604     c                   EndIf
086000040604     c                   Eval      woktrs = *On
086100040603     c                   Eval      dAr5Trs = Ar5Uni
086200040603     c                   If        §Ar5Flp > *Zeros or §ar5Fl2 > *Zeros or
086300040603     c                             §Ar5Fl3 > *Zeros or §ar5Fl4 > *Zeros or
086400040603     c                             §Ar5Fl5 > *Zeros
086500040603     c                   Eval      *In09 = *On
086600040603     c                   EndIf
086700040604     c                   Leave
086800040604     c                   EndDo
086900040604     c                   If        woktrs = *Off and
087000040604     c                             (TasFlp > *Zeros or TasFl2 > *Zeros)
087100040603     c                   Eval      *In09 = *On
087200040603     c                   EndIf
087300040603
087400990630      *  F10= Variazioni
087500170118     c***                if        knmus<>'EDPRM'
087600170118     C***  KEYspe        SETLL     TITASV1L                               10
087700170118     c***                else
087800161006     C     KEYspe        SETLL     fiarbf2c                               10
087900170118     c***                endif
088000991117
088100030122      *  F11= Note Supermercato/Bancali
088200030121     c                   Eval      wAr5Trd = 'SCR'
088300040603     c     kSpeAr5       Setll     Fiar531c                               11
088400030703     c                   If        Not *In11
088500030703     c                   Eval      wAr5Trd = 'BNB'
088600040603     c     kSpeAr5       Setll     Fiar531c                               11
088700030703     c                   EndIf
088800030122     c                   If        Not *In11
088900030122     c                   Eval      wAr5Trd = 'BAN'
089000040603     c     kSpeAr5       Chain     Fiar531c
089100040603     c                   If        %Found(Fiar531c)
089200030122     c                   Movel     Ar5Uni        dAr5Ban
089300030122     c                   If        §Ar5Rb1 <> *Zeros or §Ar5Rb2 <> *Zeros
089400030122     c                   Eval      *In11 = *On
089500030122     c                   EndIf
089600030122     c                   EndIf
089700030127     c                   EndIf
089800030120
089900991117      *  F13= Tassazione
090000991117     c                   EVAL      *IN13 = (*IN35 = *off)
090100990630
090200000209      *  F14= Giacenza, non abilitato se arrivo dalle giacenze
090300000209     c                   IF        %SUBST(I51OP0:1:1) = 'G'
090400000209     c                   eval      *in14 = *off
090500000209     c                   ELSE
090600050401     C     KEYspe        setll     Tigcp51l                               14
090700000209     c                   ENDIF
090800041007      *
090900041007      *?Abilito F1=Aggiunta in Reclami?
091000041007      * (se richiamato da pgm. FIDNA1R...)
091100041007     c                   if            SDSprm >  1
091200041007     c                             and IA1ins = ' '
091300041007     c                   eval      *in01  = *on
091400041007     c                   endif
091500040416      *
091600040416      *?Abilito F15=Visualizzazione Reclami?
091700040416      * (se richiamato da pgm. FIDNA1R...)
091800040416     c                   eval      TGDtor  = 'S'
091900040416     c                   eval      TGDogg  = ds_OggSped
092000040416     c     K02TGD02      setll     FITGD000
092100160404     c                   eval      *in15   = (%equal(FITGD32L)   and
092200040416     c                                           (SDSprm =  1
092300040416     c                                        or (SDSprm >  1
092400040416     c                                        and IA1ins <> 'N')))
092500990630
092600990630      *  F16= Competenze di C/E
092700991117     c                   if         *in35 = *off
092800010312     c                   open      eccet30c
092900990630     C     KEYspe1       SETLL     ECCET30C                               16
093000010312     c                   close     eccet30c
093100991117     c                   endif
093200080118     c
093300080118      *  F17= Richiesta rigenerazione bolla se non vengo dalla manutenzione
093400080118     c                   if                 %SUBST(I51OP0:1:1) <> 'M'
093500030624     c                   Eval      kCod = '3A'
093600030624     c                   Movel(P)  TasCbo        kKey
093700030624     c                   ExSr      ChkTab
093800030624     c  n30              Movel     Wtbluni       ds3a
093900030627     c                   If        §3agen <> 'N'
094000000418     c                   EVAL      *IN17 = *on
094100030624     c                   EndIf
094200080118     c                   EndIf
094300990630
094400990630      *  F18= Legami
094500990630     c                   EVAL      *IN18 = (TASfa1 = 'S')
094600040119
094700040119      *  F19= Immagine LdV
094800040331     ***c                   eval      *in19 = (§FLOiml = 'S')
094900040331     c                   eval      *in19 = (§FLOiml <> *blanks)
095000990630
095100990630      *  F21= Dettaglio Colli
095200990630     C     KEYspe        SETLL     TITAT30C                               21
095300990630
095400000209      *  F22= C/Assegno, non abilitato se arrivo dai C/Assegni
095500000209     c                   EVAL      *IN22 = (TASfca = 'S'
095600000209     c                                      and  %SUBST(I51OP0:1:1) <> 'C' )
095700020225      *  F23= AWB FedEx
095800020225     c                   eval      wtahtrc = 'F'
095900020225     C     KEYspeh       SETLL     TITAH30C                               23
096000030121
096100030121      * Imposto le sk dei tasti funzione che verranno emessi nella riga variabile
096200030121      * a video (5 tasti funzione alla volta + tasto F24)
096300030121      * le sk devono essere caricate in base all'ordine di emissione dei tasti
096400030121      * funzione
096500030121      * 1 sk con il n.indicatore e 1 sk con lo stato dell'indicatore (acceso/spento)
096600040416     c                   Eval      SkInd(1) = 15
096700040416     c                   Eval      SkOnOff(1) = *In15
096800170515     c                   Eval      SkInd(2) = 06
096900170515     c                   Eval      SkOnOff(2) = *In06
097000170515     c                   Eval      SkInd(3) = 5
097100170515     c                   Eval      SkOnOff(3) = *In05
097200170515     c                   Eval      SkInd(4) = 22
097300170515     c                   Eval      SkOnOff(4) = *In22
097400170515     c                   Eval      SkInd(5) = 2
097500170515     c                   Eval      SkOnOff(5) = *In02
097600170515     c                   Eval      SkInd(6) = 14
097700170515     c                   Eval      SkOnOff(6) = *In14
097800170515     c                   Eval      SkInd(7) = 10
097900170515     c                   Eval      SkOnOff(7) = *In10
098000170515     c                   Eval      SkInd(8) = 18
098100170515     c                   Eval      SkOnOff(8) = *In18
098200170515     c                   Eval      SkInd(9) = 11
098300170515     c                   Eval      SkOnOff(9) = *In11
098400170515     c                   Eval      SkInd(10) = 04
098500170515     c                   Eval      SkOnOff(10) = *In04
098600170515     c                   Eval      SkInd(11) = 08
098700170515     c                   Eval      SkOnOff(11) = *In08
098800170515     c                   Eval      SkInd(12) = 21
098900170515     c                   Eval      SkOnOff(12) = *In21
099000170515     c                   Eval      SkInd(13) = 23
099100170515     c                   Eval      SkOnOff(13) = *In23
099200170515     c                   Eval      SkInd(14) = 01
099300170515     c                   Eval      SkOnOff(14) = *In01
099400030121
099500030403     c                   Clear                   wOnOff
099600030121      * Conto quanti tasti funzione sono attivi
099700120711     c                   Do        13            xx
099800030121     c                   If        SkOnOff(xx) = '1'
099900030121     c                   Add       1             wOnOff
100000030121     c                   EndIf
100100030121     c                   EndDo
100200020225
100300020225      *  F24= Altri tasti
100400020226     c                   exsr      sub_f24
100500990630
100600990630     C                   ENDSR
100700990630      **********************************************************************
100800990630      * EMETTO VIDEATA DI ENTRATA
100900990630      **********************************************************************
101000990630     C     SUB_FMT001    BEGSR
101100990705
101200990705      * Per il primo richiamo imposto i campi della videata di entrata, i campi comuni a più videate
101300990708      *  e i dati del mittente e del destinatario per tutte le videate
101400990705     c                   IF        WSB51D01 = *blanks
101500990705     c                   EXSR      RIED01
101600990705     c                   eval      WSB51D01 = 'S'
101700990705     c                   ENDIF
101800990705
101900990630     c                   write     SB51T01
102000990630     c                   write     SB51Z00
102100120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
102200120302     c                   exfmt     SB51D01
102300120302     c                   else
102400120302     c                   write     sb51d01
102500120302     c                   read(E)   tnsb51d
102600120302     c                   if        %error
102700120302     c                   eval      *inkc='1'
102800120302     c                   endif
102900120302     c                   endif
103000990630
103100990630     c                   ENDSR
103200990618      **********************************************************************
103300990630      * IMPOSTO CAMPI VIDEO 1 E QUELLI COMUNI A PIU' VIDEATE
103400990618      **********************************************************************
103500990628     C     RIED01        BEGSR
103600171124     c*******            eval      *in70  = *off
103700170517     c                   eval      v1ctit=titolo_I
103800170517     c                   if        i51op1<>' 05'
103900170517     c                   eval      v1ctit=titolo_M
104000170517     c                   endif
104100151020     c                   clear                   dAR5fat
104200990618
104300990630      * I campi con prefisso V0C o V0D sono presenti in più videate
104400990630
104500990630     c                   z-add     taslnp        V0Clnp
104600990630     c                   z-add     tasnrs        V0Cnrs
104700990630     c                   z-add     tasnsp        V0Cnsp
104800990630     c                   z-add     TASaas        V0Cdsp
104900990630     c                   movel     TASmgs        V0Cdsp
105000990630     c     *usa          move      V0Cdsp        DATA_eur
105100990630     c     *eur          move      DATA_eur      V0Cdsp
105200990630      *
105300990630     c                   move      TASccm        Wccm4
105400990630     c                   move      TASksc        Wksc4
105500990628      * Tipo servizio
105600990630     C                   eval      KCOD = '5E'
105700990628     C                   movel(P)  tastsp        KKEY
105800990628     C                   EXSR      CHKTAB
105900990629     c                   if        *in30 = *off
106000090605     C                   movel     Wtbluni       ds5e
106100090605     c                   eval      V0Dtsp = §5ed08
106200990629     c                   endif
106300990628      * Stato bolla
106400990628     c                   SELECT
106500990628     c                   WHEN      TASFIC <> *blanks
106600990630     c                   eval      V0Dsts = 'CONTABILIZZ.'
106700990628     c                   WHEN      TASDFT > *zeros
106800990630     c                   eval      V0Dsts = 'FATTURATA   '
106900990628     c                   OTHER
107000990630     c                   eval      V0Dsts = 'DA FATTURARE'
107100990628     c                   ENDSL
107200990628      * P.O. Arrivo
107300021119     c                   Clear                   Og143
107400021119     c                   Clear                   LnaNtw
107500990630     c                   z-add     taslna        V0Clna
107600990628     c                   z-add     taslna        Wlinea
107700990628     C                   EXSR      CHKORG
107800990630     C                   movel     orgdes        V0Dlna
107900021119     c                   Movel     Orgde3        Og143
108000021119     c                   Movel     §OgNtw        LnaNtw
108100990630      * Tipo bolla
108200990630     c                   movel     tastbl        V0Ctbl
108300990630     C                   eval      KCOD = 'TB'
108400990628     C                   movel(P)  tastbl        KKEY
108500990628     C                   EXSR      CHKTAB
108600990629     c                   if        *in30 = *off
108700990629     C                   movel     Wtbluni       DSTB
108800990630     C                   movel     §TBdes        V0Dtbl
108900990726     C                   movel     §TBdes        V0Dtb1
109000990629     c                   endif
109100151020     c
109200151020     c                   clear                   ds3a
109300151020     c                   Eval      kCod = '3A'
109400151020     c                   Movel(P)  TasCbo        kKey
109500151020     c                   ExSr      ChkTab
109600151020     c  n30              Movel     Wtbluni       ds3a
109700990628      * Data 1a Consegna Parziale
109800990628     c                   IF        TASdcp > *zeros
109900990630     c     *iso          move      TASdcp        DATA_eur
110000990813     c     *eur          move      DATA_eur      Wtasdcp
110100990813     C                   eval      V0Cdcp = %EDITC(Wtasdcp:'Y')
110200990628     c                   ENDIF
110300990628      * Codice consegna anomala
110400990702     c                   IF        TAScca <> *blanks
110500990701     c                   movel     tascca        v0ccca
110600990630     C                   eval      KCOD = '7O'
110700990628     C                   movel(P)  tascca        KKEY
110800990628     C                   EXSR      CHKTAB
110900990702     C  N30              movel     Wtbluni       V0Dcca
111000990702     c                   ENDIF
111100990628      * Data Consegna Reale
111200990628     c                   IF        TASdcm > *zeros
111300990630     c     *iso          move      TASdcm        DATA_eur
111400990630     c     *eur          move      DATA_eur      W0080
111500990701     C                   eval      V0Cdcm = %EDITC(W0080:'Y')
111600990628     c                   ENDIF
111700990628      * Ora Consegna Reale
111800990628     c                   IF        TASHmc > *zeros
111900990628     c                   eval      *IN40 = *on
112000990701     c                   z-add     tashmc        v0chmc
112100990628     c                   ENDIF
112200990628      * Numero e data distinta consegna
112300990701     c                   z-add     tasndc        v0cndc
112400990628     c                   IF        TASddc > *zeros
112500990630     c     *iso          move      TASddc        DATA_eur
112600990630     c     *eur          move      DATA_eur      W0080
112700990701     C                   eval      V0Cddc = %EDITC(W0080:'Y')
112800990628     c                   ENDIF
112900990628      * Dati Fattura
113000990629      *
113100990629      *    Decodifico Divisa e memorizzo dati in DS di comodo
113200990630     C                   eval      KCOD = 'CV'
113300990628     C                   movel(P)  tasdiv        KKEY
113400990628     C                   EXSR      CHKTAB
113500990629     c                   if        *in30 = *off
113600990628     C                   movel     Wtbluni       WDSCV
113700990629     c                   endif
113800991021      *    Se ho il libro IVA determino se fattura immediata
113900991021     c                   clear                   Wfatimmed
114000991021     c                   IF        TASfiv > *zeros
114100991021     c                   EXSR      FATIMMED
114200991021     c                   ENDIF
114300991021      *
114400991021      *    Se Fattura immediata  scrivo IMPORTO altrimenti IMPONIBILE
114500991021     c                   IF        Wfatimmed = 'S'
114600990716      *        se fatturata calcolo l'importo
114700990813     c                   if        TASfts = '1'
114800990714     c                   eval      D17tla = 'L'
114900990825     C
115000991021     c                   if         Wfatimmed = 'S'  and  §TBTPO = 'F'
115100990825     c                   eval      D17PRP = 'S'
115200990813     c                   endif
115300990825     c                   eval      D17tbo = '0'
115400990714     c                   eval      D17crb = 'S'
115500990714     c                   eval      D17aas = TASaas
115600990714     c                   eval      D17lns = TASlnp
115700990714     c                   eval      D17nrs = TASnrs
115800990714     c                   eval      D17nsp = TASnsp
115900990714     c                   eval      D17tbs = TAStbl
116000990714     c                   eval      D17fim = 'F'
116100990714     c                   CALL      'FNLV16R'
116200990714     c                   PARM                    FNLV16DS
116300990714     c                   PARM                    DTASV
116400990716     c                   endif
116500990714     c                   eval      *in41 = *off
116600990714     c                   z-add     O17IFT        W0133
116700990813      *
116800990628     c                   ELSE
116900990702     c                   z-add     TASimv        W0133
117000990708     c                   eval      *in41 = *on
117100990628     c                   ENDIF
117200990628      *
117300990629     c                   IF         W0133 > *zeros
117400990628     c                   movel     W§cvfdc       Wtstdec
117500990628     c                   exsr      EDTimporti
117600990628     c                   movel     IMPeditato    v1cimp
117700990628     c                   ENDIF
117800011029      *    Divisa
117900011029     c                   IF        v1cimp <>*zeros
118000011029     c                   movel     tasdiv        v1cdiv
118100011029     c                   endif
118200990629      *    P.O. IVA - Numero Fattura - Data fattura
118300990702     c                   z-add     TASfiv        V0Cfiv
118400990702     c                   z-add     TASnft        V0Cnft
118500990629     c                   IF        TASdft > *zeros
118600990630     c     *iso          move      TASdft        DATA_eur
118700990630     c     *eur          move      DATA_eur      W0080
118800990702     C                   eval      V0Cdft = %EDITC(W0080:'Y')
118900990702      *    Divisa
119000990702     c                   ENDIF
119100990701      * Dati Mittente per tutte le videate
119200990701     c                   z-add     TASccm        V0Cccm
119300990701      *    se non codificato aggancio TITAA
119400990707     c                   IF        Wccm4 = *zero  or  Wccm4 = 8888
119500990629     c                   eval      ktrc = 'M'
119600990701     c     KTAStrc       CHAIN     TITAA30C
119700990701      *
119800990701     c                   IF        %FOUND
119900990629     c                   movel     TAArsc        V1Crsm
120000990701     c                   movel     TAAind        V1Cinm
120100990701     c                   movel     TAAloc        V1Clom
120200990701     c                   if        TAAnaz <> *blanks
120300990701     c                   movel     TAAnaz        V1Cpnm
120400990701     c                   else
120500990701     c                   movel     TAAprv        V1Cpnm
120600990701     c                   endif
120700990701     c                   movel     TAAcap        V0Ccam
120800990701     c                   movel     TAArsc        V7Crsm
120900990701     c                   movel     TAAind        V7Cinm
121000990701     c                   movel     TAAloc        V7Clom
121100990702     c                   movel     TAAprv        V7Cprm
121200990702     c                   movel     TAAnaz        V7Cnzm
121300990701     c                   movel     TAAcpi        V7Cpim
121400990701     c                   ENDIF
121500990629      *    se codificato aggancio anagrafico
121600990629     c                   ELSE
121700990701     c                   z-add     V0Cccm        I69kac
121800990701     c                   z-add     V0Cccm        I69kin
121900990701     c                   z-add     V0Cccm        I69kcs
122000990630     c                   exsr      CTR_anagra
122100990701      *
122200990630     c                   movel     ACOrag        V1Crsm
122300990630     c                   movel     INDvia        V1Cinm
122400990630     c                   movel     INDcit        V1Clom
122500990630     c                   if        INDsta <> *blanks
122600990630     c                   movel     INDsta        V1Cpnm
122700990630     c                   else
122800990630     c                   movel     INDprv        V1Cpnm
122900990630     c                   endif
123000990701     c                   movel     INDcae        V0Ccam
123100990701     c                   movel     ACOrag        V7Crsm
123200990701     c                   movel     INDvia        V7Cinm
123300990701     c                   movel     INDcit        V7Clom
123400990701     c                   movel     INDprv        V7Cprm
123500990701     c                   movel     INDsta        V7Cnzm
123600990701     c                   movel     INDiva        V7Cpim
123700990629     c                   ENDIF
123800990629      * Destinatario
123900990701      *    se assegnato utilizzo codice di tassazione come codice destinatario
124000990629     c                   IF        §TBtpo = 'A'
124100990701     c                   z-add     TASksc        V0Cccd
124200990629     c                   ENDIF
124300990701      *
124400111011     c* cerco 2 parte della ragione sociale
124500111011     c                   eval      ktrc = 'D'
124600111011     c                   clear                   rsd2             35
124700111011     c     ktastrc       chain     tita430c
124800111011 b1  c                   if        %found(tita430C)
124900111011     c                   eval      rsd2=ta4not
125000111011     c                   endif
125100111011     c
125200990629     c                   movel     TASrsd        V1Crsd
125300111011     c                   if        %subst(tasrsd:31:5)= *blanks and rsd2<>*blanks
125400111011     c                   eval      v1crsd=%trim(v1crsd) + ' ' +rsd2
125500111011     c                   endif
125600990629     c                   movel     TASind        V1Cind
125700990629     c                   movel     TASlod        V1Clod
125800990629     c                   if        TASnzd <> *blanks
125900990629     c                   movel     TASnzd        V1Cpnd
126000990629     c                   else
126100990629     c                   movel     TASprd        V1Cpnd
126200990629     c                   endif
126300990701     c                   movel     TAScad        V0Ccad
126400990702     c                   movel     TASnzd        V0Cnzd
126500111011     c                   clear                   v7crsd
126600990701     c                   movel     TASrsd        V7Crsd
126700111011     c*
126800111011 b1  c                   if        rsd2<>*blanks
126900111011     c                   if        %subst(tasrsd:35:1)<>' '
127000111011     c                   eval      v7crsd=tasrsd +ta4not
127100111011     c                   else
127200111011     c                   eval      v7crsd=%trim(v7crsd) + ' ' +rsd2
127300111011     c                   endif
127400111011     c
127500111011     c                   endif
127600990701     c                   movel     TASind        V7Cind
127700990701     c                   movel     TASlod        V7Clod
127800990701     c                   movel     TASprd        V7Cprd
127900990701     c                   movel     TAScpd        V7Cpid
128000061108      * Codice fiscale mittente/destinatario da Tita4 rcd Q
128100061108     c                   eval      ktrc = 'Q'
128200061108     c                   clear                   dta4q
128300061108     c     ktastrc       chain     tita430c
128400061108 b1  c                   if        %found(tita430C)
128500061108     c                   eval      dta4q = ta4not
128600061108     c                   endif
128700061108     c                   eval      v7cdfm = §ta4qcfm
128800061108     c                   eval      v7cdfd = §ta4qcfd
128900990726      *
129000990701      * Cliente Fatturazione
129100990701     c                   z-add     TASksc        VTCksc
129200990701     c                   SELECT
129300990701      *     assegnato non codificato  (Da TITAS)
129400990708     c                   WHEN      §TBtpo = 'A' and  *IN41 = *off
129500990726     c                   movel     TASrsd        VTDksc
129600990726      *     assegnato codificato (Decodifico la ragione sociale)
129700990701     c                   WHEN      §TBtpo = 'A'
129800990702     c                   z-add     tasksc        I69kac
129900990701     c                   exsr      CTR_anagra
130000990726     c                   movel     ACOrag        VTDksc
130100990701      *     porto franco
130200990701     c                   WHEN      §TBtpo = 'F'
130300990701     c                   movel     V7Crsm        VTDksc
130400990701     c                   ENDSL
130500991123      * Mittente Originale
130600991123     c                   eval      ktrc = 'O'
130700991123     c     KTAStrc       CHAIN     TITAA30C
130800991123     c                   If        %FOUND
130900991123     c                   eval      *in51 = *on
131000991123     c                   eval      v7crmo = TAArsc
131100991123     c                   Endif
131200990629      * Riferimento mittente numerico
131300990701     c                   z-add     TASrmn        V0Crmn
131400990629      * Riferimento mittente alfanumerico e riferimento partner
131500990629     c                   eval      ktrc = 'A'
131600990629     c     KTAStrc       CHAIN     TITA430C
131700990629     c                   If        %FOUND
131800990629     c                   movel     TA4not        DTA4A
131900990630     c                   movel     §TA4ARMA      V1Crma
132000070927     c*********+         movel     §TA4ARPT      V1Crpt
132100160201     c                   movel     §TA4Anas      V1Cnas
132200990629     c                   ENDIF
132300070927      * Riferimento  partner nel record "E"
132400070927     c                   eval      ktrc = 'E'
132500070927     c     KTAStrc       CHAIN     TITA430C
132600070927     c                   If        %FOUND
132700070927     c                   movel     TA4not        dsbl4E
132800070927     c                   movel     §b4ERP        V1Crpt
132900070927     c                   ENDIF
133000000222      * Numero parcel DPD
133100060518     c***                eval      ktrc = 'F'
133200171023     c***                eval      ktrc = 'I'
133300171023     c***  KTAStrc       CHAIN     TITA430C
133400171023     c****               If        %FOUND
133500060518     c**                 movel     TA4not        DSBL4F
133600060518     c**                 move(P)   §B4fnp        V1Crpt
133700060518     c**                 movel     §B4rtn        V1Crtn
133800060518     c**                 movel     §B4zon        V1Czon
133900171023     c***                movel     TA4not        DSBL4i
134000171023     c***                movel(P)  §B4ipn        V1Crpt
134100171023     c**                 movel     §B4ida        V1Crtn
134200171023     c**                 movel     §B4iza        V1Czon
134300171023     C***                eval      *in52 = (V1Crtn <> *blanks or
134400171023     C***                                   V1Czon <> *blanks)
134500171023     c***                ENDIF
134600171023         chain (tasaas:taslnp:tasnrs:tasnsp) fipnd01l;
134700171023         if %found(fipnd01l);
134800171023            v1crpt=pndipn;
134900171023            select;
135000171023            when pnddpe='DP4';
135100171023                dpnddp4=pndetiu;
135200171023                v1cida7=§b4ida;
135300171023            when pnddpe='DP5';
135400171023                v1cida7=pndida7 ;
135500171023            endsl;
135600171023            *in52 = v1cida7<>*blanks;
135700171023         endif;
135800990629      * Natura merce
135900160201     c****               movel     TASnas        V1Cnas
136000990629      * Segnacolli
136100170601     C                   eval      *in75=*off
136200990630     c                   z-add     TASfls        V1Cfls
136300990629     c                   z-add     TASnrs        V1Cnrss
136400990629     c                   z-add     TASncd        V1Cncd
136500990629     c                   z-add     TASnca        V1Cnca
136600990629     c                   if        TASfns = 'S'
136700990629     c                   eval      V1Dfns = 'NON SEQUENZIALI'
136800170601     c                   eval      *in75=*on
136900990629     c                   endif
137000180126      *  Colli incompatibili
137100180126     c                   eval      wtahtrc = 'J'
137200180126     C     KEYspeh       SETLL     TITAH30C                               76
137300180129     c                   clear                   v1cinc
137400180126
137500990629     c                   z-add     TASznc        V1Cznc
137600050113     c* flag raggruppamento spedizioni
137700050113     c                   movel     TASxco        V1Cxco
137800990629      * Contrassegno
137900160412     c                   setoff                                       39
138000990630     c                   IF        TASfca = 'S'
138100990630     c     Keyspe        chain     TNCSB03L                           30
138200160419     c****               If        %FOUND  and  CSBsta <> 9
138300160419     c                   If        %FOUND
138400990630     c                   z-add     CSBcas        V1Ccas
138500990630     c                   If         CSBvca <> *blanks
138600990630     c                   movel     CSBvca        V1Cvca
138700990630     c                   Else
138800990630     c                   movel     'ITL'         V1Cvca
138900990630     c                   Endif
139000990630     c                   Endif
139100160412     c                   If        %found and   CSBsta= 9
139200160412     c                   seton                                        39
139300160412     c                   endif
139400990630     c                   ENDIF
139500031119
139600031119      * Referente e Telefono del destinatario
139700110610      * e profilo utente/terminale che ha immesso la bolla
139800031119     c                   Clear                   V7cRef
139900031119     c                   Clear                   V7cTeld
140000110610     c                   Clear                   V7cpru
140100110610     c                   Clear                   V7cnmtd
140200120712     c                   clear                   W02DRDT
140300120712     c                   clear                   W02MTVDT
140400120712     c                   clear                   W02DDTIL
140500120712     c                   clear                   W02DIMTV
140600120712     c                   clear                   W02DMTVDT
140700120712     c                   clear                   W02DRID
140800120829     c                   clear                   W02lbl
140900110610     c                   setoff                                       66
141000120404     c                   setoff                                       6768
141100031119      * li prendo da Fiar500f record 'GEN'
141200031119     c                   Clear                   dAr5Gen
141300031119     c                   Eval      war5Trd = 'GEN'
141400040603     c     kSpeAr5       Chain     Fiar531c
141500040603If  1c                   If        %Found(Fiar531c)
141600031119     c                   Movel     Ar5Uni        Dar5Gen
141700031119     c                   Eval      V7cRef = §Ar5Ref
141800031119     c                   Eval      V7cTeld = §Ar5Teld
141900110610     c                   Eval      V7cpru  = §Ar5bpru
142000110610     c                   Eval      V7cnmtd = §Ar5bnmtd
142100120404    2c                   if        v7cpru<>*blanks
142200110610     c                   seton                                        66
142300120404    2c                   endif
142400120330     c* restituzione DDt al mittente
142500151119    2c***                if        §ar5drdt<>*blanks
142600151119     c****               seton                                        67
142700120712     c* data ricezione
142800151119    3c***                if        §ar5drdt<>'99999999'
142900151119     c***                movel     §ar5drdt      w0080             8 0
143000151119     c***  *iso          movel     w0080         dataiso
143100151119     c***                eval      data_eur=dataiso
143200151119     c***                movel     data_eur      w02drdt
143300151119    3c***                endif
143400120712     c* "Elaborata per il mittente"
143500151119     c***                clear                   w02dfstd
143600151119     c***                if        §ar5fstd='S'
143700151119     c***                eval      w02dfstd='Elaborata per invio al Mittente'
143800151119     c***                endif
143900120712     c* motivo
144000151119    3c***                if        §ar5mtvdt<>*blanks
144100120712     c* apro file TNTBE di filiale
144200151119     c***                if        not %open(tntbe01l)
144300151119     c***                open(e)   tntbe01l
144400151119     c***                if        %error
144500151119     c***                eval      %subst(Wtntbe:7:4)='GRPF'
144600151119     c***                open(e)   tntbe01l
144700151119     c***                endif
144800151119     c***                endif
144900150828     c***
145000151119     c***                eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
145100151119     c***                                   %subst(§ar5mtvdt:4:5)
145200151119     c***                eval      kcode='MDE'
145300151119     c***                clear                   kke1
145400151119     c***                clear                   kke2
145500151119     c***                eval      kke1=%subst(§ar5mtvdt:1:3)
145600151119     c***                eval      kke2=%subst(§ar5mtvdt:4:5)
145700151119     c***  ktbe          chain     tntbe01l
145800151119    4c***                if        %found(tntbe01l)
145900151119     c***                eval      w02dmtvdt=tbeuni
146000151119    4c***                endif
146100151119    4c***                if        §ar5dimtv >*zeros
146200151119     c***                movel     §ar5dimtv     w0080             8 0
146300151119     c***  *iso          movel     w0080         dataiso
146400151119     c***                eval      data_eur=dataiso
146500151119     c***                movel     data_eur      w02dimtv
146600151119     c***                eval      w02ddtil='Inserito il'
146700151119    4c***                endif
146800151119    3c***                endif
146900151119    2c***                endif
147000031119    1c                   EndIf
147100120712     c* DATA RICEZIONE IMM.DOC.
147200120712     c                   Clear                   dAr5doc
147300120712     c                   Eval      war5Trd = 'DOC'
147400120712     c     kSpeAr5       Chain     Fiar531c
147500120712If  1c                   If        %Found(Fiar531c)
147600120712     c                   Movel     Ar5Uni        Dar5doc
147700120712    4c                   if        §ar5jdrid >*zeros
147800120712     c                   seton                                        67
147900120712     c                   movel     §ar5jdrid     w0080             8 0
148000120712     c     *iso          movel     w0080         dataiso
148100120712     c                   eval      data_eur=dataiso
148200120712     c                   movel     data_eur      w02drid
148300120712    4c                   endif
148400150828     c* restituzione DDt al mittente
148500151119    2c                   if        §ar5drdt<>*blanks
148600151119     c                   seton                                        67
148700150828     c* data ricezione
148800151119    3c                   if        §ar5drdt<>'99999999'
148900151119     c                   movel     §ar5drdt      w0080             8 0
149000151119     c     *iso          movel     w0080         dataiso
149100151119     c                   eval      data_eur=dataiso
149200151119     c                   movel     data_eur      w02drdt
149300151119    3c                   endif
149400150828     c* "Elaborata per il mittente"
149500151119     c                   clear                   w02dfstd
149600151119     c                   if        §ar5fstd='S'
149700151119     c                   eval      w02dfstd='Elaborata per invio al Mittente'
149800151119     c                   endif
149900150828     c* motivo
150000151119    3c                   if        §ar5mtvdt<>*blanks
150100150828     c* apro file TNTBE di filiale
150200151119     c                   if        not %open(tntbe01l)
150300151119     c                   open(e)   tntbe01l
150400151119     c                   if        %error
150500151119     c                   eval      %subst(Wtntbe:7:4)='GRPF'
150600151119     c                   open(e)   tntbe01l
150700151119     c                   endif
150800151119     c                   endif
150900150828     c
151000151119     c                   eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
151100151119     c                                      %subst(§ar5mtvdt:4:5)
151200151119     c                   eval      kcode='MDE'
151300151119     c                   clear                   kke1
151400151119     c                   clear                   kke2
151500151119     c                   eval      kke1=%subst(§ar5mtvdt:1:3)
151600151119     c                   eval      kke2=%subst(§ar5mtvdt:4:5)
151700151119     c     ktbe          chain     tntbe01l
151800151119    4c                   if        %found(tntbe01l)
151900151119     c                   eval      w02dmtvdt=tbeuni
152000151119    4c                   endif
152100151119    4c                   if        §ar5dimtv >*zeros
152200151119     c                   movel     §ar5dimtv     w0080             8 0
152300151119     c     *iso          movel     w0080         dataiso
152400151119     c                   eval      data_eur=dataiso
152500151119     c                   movel     data_eur      w02dimtv
152600151119     c                   eval      w02ddtil='Inserito il'
152700151119    4c                   endif
152800151119    3c                   endif
152900151119    2c                   endif
153000120712    1c                   endif
153100120829     c* se non presente data scannerizzazione visualizzo quella della figlia eventuale
153200120829     c                   if        w02drid=*zeros
153300120829     c                   exsr      replbl
153400120829     c                   if        wtrovl=*on
153500120829     c                   seton                                        67
153600120829     c                   movel     §ar5jdrid     w0080             8 0
153700120829     c     *iso          movel     w0080         dataiso
153800120829     c                   eval      data_eur=dataiso
153900120829     c                   movel     data_eur      w02drid
154000120829     c                   eval      w02lbl='Bolla legata'
154100120829     c                   endif
154200120829     c                   endif
154300121217     c* bolla orignale / di recupero per variazioni post fatturazione
154400121217     c                   if        w001aleg=*blanks
154500121217     c                   setoff                                       71
154600121217     c                   Eval      war5Trd = 'LEG'
154700121217     c     kSpeAr5       Chain     Fiar531c
154800121217If  1c                   If        %Found(Fiar531c)
154900121217     c                   seton                                        71
155000121217     c                   eval      waas_leg=%dec(%subst(ar5uni:1:4):4:0)
155100121217     c                   eval      wlnp_leg=%dec(%subst(ar5uni:5:3):3:0)
155200121217     c                   eval      wnrs_leg=%dec(%subst(ar5uni:8:2):2:0)
155300121217     c                   eval      wnsp_leg=%dec(%subst(ar5uni:10:7):7:0)
155400121217     c                   clear                   v1cleg
155500121217     c                   if        §tbrbl='R'
155600170601     c                   eval      v1dleg='Bolla Originale  (?)'
155700121217     c                   else
155800170601     c                   eval      v1dleg='Recupero Variaz. (?)'
155900121217     c                   endif
156000121217    1c                   endif
156100121217     c                   endif
156200040301
156300040301      * Se colli di valore visualizzo "V" nell'apposito campo
156400040301      *  con lo specifico attributo di visualizzazione
156500040603if  1c**!!!              if        §AR5bva = *blanks
156600040603     c**!!!              eval      V1Cbva  = *blanks
156700040603x   1c**!!!              else
156800040603     c**!!!              eval      V1Cbva  = 'V'
156900040603e   1c**!!!              endif
157000040603     c                   Eval      V1cBva = §FloTba
157100040301
157200990701      * Colli, Peso e Volume
157300990702     c                   z-add     TASncl        V0Cncl
157400990630      *
157500990629     c                   movel     TASfpb        V1Cfpb
157600990629     c                   z-add     TASpkb        V1Cpkb
157700990629     c                   z-add     TASpkc        V1Cpkc
157800990630     c                   z-add     TASncp        V1Cncp
157900151016     c*//                movel     TASfpf        V0Cfpf
158000990702     c                   z-add     TASpkf        V0Cpkf
158100160608      *
158200160608     c                   movel     TASfvb        V1Cfvb
158300160608     c                   z-add     TASvlb        V1Cvlb
158400160608     c                   z-add     TASvlc        V1Cvlc
158500160608     c                   z-add     TASncr        V1Cncr
158600170523     c                   movel     TASfvf        V0Cfvf
158700160608     c                   z-add     TASvlf        V0Cvlf
158800030121      * Peso fatturato solo se la bolla è fatturarata
158900151016     c                   Clear                   V0Cfpf
159000030121     c                   Clear                   V0cPeso
159100170523     c                   Clear                   V0Cfvolu
159200170522     c                   Clear                   V0cVolu
159300151124If  1c***********        If        TasDft > *Zeros  or
159400151124     c                   if        TASimv > *Zeros
159500151020      *?Si reperiscono i dati da FIAR5/"FAT"?
159600151020      * non se è la seconda bolla di bolla doppia
159700151020     c                   clear                   dAR5fat
159800151020     c                   if        §3atb2=*blanks or
159900151020     c                             (%subst(§3atb1:1:1)=%subst(tastbl:1:1))
160000151020     c                   Eval      wAR5trd = 'FAT'
160100151020     c     kSpeAr5       Chain     Fiar531c
160200151020if  2c                   if        %Found(Fiar531c)
160300151020     c                   Movel     Ar5Uni        dAr5Fat
160400151125     c                   eval      v0cfpf=dar5fat.§ar5fptas
160500151125     c                   if        v0cfpf='Z' or v0cfpf='T'
160600151125     c                   eval      v0cfpf='V'
160700151126     c                   endif
160800151125     c                   eval      V0Cpeso = dAR5fat.§AR5pkTas
160900151020     c                   endif
161000170522     c                   eval      v0cVolu = dar5fat.§ar5vltas
161100170524     c                   eval      v0cfVolu = dar5fat.§ar5fvtas
161200170525     c                   if        v0cfVolu='Z' or v0cfVolu='T' or
161300170525     c                             v0cfVolu='D'
161400170525     c                   eval      v0cfVolu='V'
161500170525     c                   endif
161600170524     c***                eval      v0cvlf = dar5fat.§ar5vltas
161700170523     c***                eval      v0cfvf = dar5fat.§ar5fvtas
161800160608      * volume desunto visualizzo come rilevato totale da VDL
161900160608     c                   if        dar5fat.§ar5fvtas = 'D'
162000160608     c                   eval      v1cvlc = dar5fat.§ar5vltas
162100170525     c****               eval      v0cvlf = dar5fat.§ar5vltas
162200170525     c****               eval      v0cfvf = 'T'
162300160608     c                   eval      v1cncr = v0cncl
162400160608     c                   endif
162500160608
162600151020     c                   endif
162700151125
162800151125     c                   if        v0cfpf=*blanks
162900151125     c                   movel     TASfpf        V0Cfpf
163000151125     c                   clear                   v0cpeso
163100151125     c                   endif
163200170525
163300170525     c                   if        v0cfVolu=*blanks
163400170525     c                   movel     TASfvf        V0CfVolu
163500170525     c                   eval      v0cVolu=tasvlf
163600170525     c                   if        v0cfVolu='Z' or v0cfVolu='T'
163700170525     c                   eval      v0cfVolu='V'
163800170525     c                   endif
163900170525     c                   endif
164000151020     c
164100151125      * -?Abilitazione Visualizzazione Pesi usati in Tassazione?
164200171124      * se utente abilitato
164300171124      * e peso o volume desunto
164400171124     c*********          eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
164500171124       // imposto se utente abilitato alla visualizzazione del peso/volume desunto
164600171124         *in70 = ((§UTEdesun = 'S') and (v0cfpf = 'D' or
164700171124                                         dar5fat.§ar5fvtas = 'D'));
164800171124
164900030121      * Prendo il peso fatturato
165000151125     c***                If        TasFpf = 'R' or ( tasfpf='D' and
165100151125     c***                          dar5fat.§AR5PKTAS=0)
165200151125     c***                Z-Add     TasPkf        V0cPeso
165300151125     c***                movel     'R'           V0Cfpf
165400151125     c***                EndIf
165500151125      * VDL
165600030121      * Prendo il peso da cml - la tara
165700151126If  2c                   If        v0cfpf = 'V' and v0cpeso=0
165800030121     c                   Clear                   dsQt1
165900030121     c                   Movel     'QT'          Kcod
166000030121     c                   Movel     1             Kkey
166100030121     c     KeyTab        Chain     Tabel00f
166200030121     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
166300030121     c                   Movel     TblUni        dsQt1
166400030121     c                   EndIf
166500150717     c* se la data fattura è >0 e minore uguale  alla data in tabella,
166600150717     c*  sottraggo il campo TARA da TCO, altrimenti da TPC
166700150717     c                   if        tasdft>0 and §qtdst>0 and tasdft<=§qtdst
166800150717     c     §QtTco        Mult      TasNcp        wTara
166900150717     c                   else
167000030121     c     §QtTpc        Mult      TasNcp        wTara
167100150717     c                   endif
167200150717     c
167300030121     c     TasPkc        Sub(h)    wTara         wNtara
167400030121     c                   Z-Add     wNtara        V0cPeso
167500030121    2c                   EndIf
167600151125      * DESUNTO
167700151125     c                   if        v0cfpf='D'
167800151125     c                   if        v0cpeso=0
167900151125     c                   Z-Add     TasPkf        V0cPeso
168000151125     c                   movel     'R'           V0Cfpf
168100151125     c                   else
168200151125      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
168300151125      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
168400151125      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
168500151125      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
168600151125     c                   eval      V0Cfpf  = 'V'
168700151125     c                   eval      V1Cpkc  = V0Cpeso
168800151125     c                   eval      V1Cncp  = V0Cncl
168900151125     c                   endif
169000151125     c                   endif
169100151125      * REALE imposto il peso se non presente su record FAT
169200151125     c                   if        v0cfpf='R'
169300151125     c                             and v0cpeso=0
169400151125     c                   z-add     taspkf        v0cpeso
169500151125     c                   endif
169600151020     c
169700151020
169800151020      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
169900151020      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
170000151125if  2c*                  if        dar5fat.§AR5PKTAS>0
170100151125     c*
170200151125if  3c*                  IF        dAR5fat.§AR5FPTAS='D'
170300151125     c*                  eval      V0Cfpf  = 'V'
170400151016      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
170500151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
170600151125     c*                  eval      V1Cpkc  = V0Cpeso
170700151016      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
170800151125     c*                  eval      V1Cncp  = V0Cncl
170900151125   x3c*                  else
171000151020     c* se non è desunto--> imposto il fatturato da record FAT
171100151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
171200151125    3c*                  endif
171300151125e   2c*                  endif
171400151020     c
171500151125     c****               eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
171600151125     c****                                    and     TASfpf = 'D' )
171700030121    1c                   EndIf
171800990701      * Merce Portata a Magazzino
171900990701     c                   IF        TASfdn = 'S'
172000990701     C                   eval      V0Dfdn = 'SI'
172100990701     c                   ENDIF
172200990701      * Fermo Deposito
172300990702     c                   IF        TASffd = 'S'
172400051107     C                   eval      V0Dffd = 'SI '
172500990701     c                   ENDIF
172600051107     c                   IF        TASffd = 'M'
172700051107     C                   eval      V0Dffd = 'SAB'
172800051107     c                   ENDIF
172900990716      * Anteporto
173000990716     C                   movel     tasfap        V0Cfap
173100990716      * Inoltro
173200990716     C                   movel     tasfin        V0Cfin
173300010221      * ORM
173400010221     C                   clear                   v12poe
173500010221     C                   clear                   v12nsr
173600010221     C                   clear                   v12nor
173700010221     C                   clear                   v12nrv
173800111103     c                   if        wta4not<>*blanks
173900111103     C                   movel     Wta4not       dsbl4m
174000111103     C                   move      §b4poe        v12poe
174100111103     C                   move      §b4nsr        v12nsr
174200111103     C                   move      §b4nor        v12nor
174300111103     C                   move      §b4nrv        v12nrv
174400111103     c                   endif
174500990630      *
174600990630     c                   ENDSR
174700990618      *****************************************************************
174800990630      * GESTIONE F2 = Comunicazione Anomalia
174900990618      *****************************************************************
175000990630     C     SUB_F02       BEGSR
175100990618      *
175200990705     c                   SELECT
175300990705      * Prima volta carico subfile
175400990705     c                   WHEN      WSB51D02 = *blanks
175500990705     c                   exsr      RIED02
175600990706     c                   eval      WSB51D02 = 'S'
175700990706      * Nesima volta con una sola C.A.: entrata diretta
175800990706     c                   WHEN      Wunaca = 'S'
175900990706     c                   exsr      RIED02_3
176000990706      * Nesima volta con più C.A.: gestione subfile
176100990705     c                   OTHER
176200990706     c                   exsr      RIED02_2
176300990705     c                   ENDSL
176400990705      *
176500990630     c                   ENDSR
176600990705      **********************************************************************
176700990705      * IMPOSTO CAMPI VIDEO INTERROGAZIONE C.A.
176800990705      **********************************************************************
176900990705     C     RIED02        BEGSR
177000990705      *
177100990705      * Pulizia subfile
177200990705     C                   eval      *in26 = *off
177300990705     C                   WRITE     SB51C11
177400990705     C                   eval      *in25 = *on
177500990705     C                   eval      *in26 = *on
177600990705     C                   CLEAR                   nrr11
177700990705      *
177800990705      * Carico il subfile
177900040809     C**""*Keyspe        setll     FNDCT02L
178000040809     C**""*Keyspe        READE     FNDCT02L
178100040809     C**""*              DOW       %EOF = *off
178200040809     C**""*              IF        DCTATB = *blanks
178300040809     C**""*              Z-ADD     DCTAAC        V11AAC
178400040809     C**""*              Z-ADD     DCTFIL        V11FIL
178500040809     C**""*              Z-ADD     DCTNCA        V11NCA
178600040809     C**""*              z-add     DCTaac        W0080
178700040809     C**""*              movel     DCTmgc        W0080
178800040809     C**""**usa          MOVEL     W0080         DATA_EUR
178900040809     C**""**eur          MOVEL     DATA_EUR      W0080
179000040809     C**""*              eval      V11dca = %EDITC(W0080:'Y')
179100040809     C**""*              Z-ADD     DCTFCA        V11FCA
179200040809     C**""*DCTfca        CHAIN     FNDFA000
179300040809     C**""*              if        %FOUND
179400040809     C**""*              movel     DFADES        V11DFA
179500040809     c**""*              endif
179600990705      *
179700040809     C**""*              add       1             NRR11
179800040809     C**""*              WRITE     SB51S11
179900040809     C**""*              ENDIF
180000990705      *
180100040809     C**""*Keyspe        READE     FNDCT02L
180200040809     C**""*              ENDDO
180300040809      *
180400040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
180500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
180600040809      * legate alla spedizione che passo
180700040809     c                   clear                   fidn12ds
180800040809     c                   eval      i12aas = I51AAS
180900040809     c                   eval      i12lnp = I51LNP
181000040809     c                   eval      i12nrs = I51NRS
181100040809     c                   eval      i12nsp = I51NSP
181200040809     c                   eval      i12fel = 'E'
181300040809     c                   eval      i12fan = 'E'
181400040809     c*
181500040809     c                   call      'FIDN12R'
181600040809     c                   parm                    fidn12ds
181700040809     c*
181800040809     c* se non ci sono errori
181900040809     c                   if        o12err = ' '
182000040809     c* se numero di CA trovate maggiore di zero
182100040809     c                   if        o12nca > 0
182200040809     c                   z-add     1             ii                2 0
182300040809     c                   dow       ii <= o12nca
182400040809     c                   movea     skKey(ii)     dsKey
182500040809     C                   Z-ADD     dsaac         V11AAC
182600040809     C                   Z-ADD     dsfil         V11FIL
182700040809     C                   Z-ADD     dsnca         V11NCA
182800040809     c*
182900040809     c* Aggancio il FNDCT01L x reperire alcuni campi
183000040809     c     KDCT1_C       chain     FNDCT01L
183100040809     c                   if        %found(FNDCT01L)
183200040809     C                   z-add     DCTaac        W0080
183300040809     C                   movel     DCTmgc        W0080
183400040809     C     *usa          MOVEL     W0080         DATA_EUR
183500040809     C     *eur          MOVEL     DATA_EUR      W0080
183600040809     C                   eval      V11dca = %EDITC(W0080:'Y')
183700040809     c                   endif
183800040809     c*
183900040809     C                   Z-ADD     skfca(ii)     V11FCA
184000040809     C     skfca(ii)     CHAIN     FNDFA000
184100040809     C                   if        %FOUND
184200040809     C                   movel     DFADES        V11DFA
184300040809     c                   endif
184400040809      *
184500040809     C                   add       1             NRR11
184600040809     C                   WRITE     SB51S11
184700040809     c*
184800040809     c                   add       1             ii
184900040809     c                   enddo
185000040809     c*
185100040809     c                   endif
185200040809     c                   endif
185300040809     c**
185400121121      * se NRR11 = 0 vuol dire che non ho più la c.a. è già stata archiviata
185500121121     c                   IF        NRR11 = 0
185600121121     c                   eval      *in28 = *on
185700121121     c                   eval      V1Cmsg = 'C.A. già archiviata'
185800121121     c                   leavesr
185900121121     c                   ENDIF
186000990705      *
186100990705      * Gestione subfile o se ho una sola C.A. entrata diretta
186200990705     c                   IF        NRR11 = 1
186300990705     c                   clear                   fidn00ds
186400990706     c                   eval      Wunaca = 'S'
186500990705     c                   exsr      RIED02_1
186600990705     c                   ELSE
186700990706     c                   exsr      RIED02_2
186800990705     C                   ENDIF
186900990705      *
187000990705     c                   ENDSR
187100990705      **********************************************************************
187200990705      * RICHIAMO PGM DI VISUALIZZAZIONE C.A.
187300990705      **********************************************************************
187400990705     C     RIED02_1      BEGSR
187500990705      *
187600990705      * Clearizzo la DS fuori dalla routine per conservarla in caso di richiamo per una sola C.A.
187700990705     C                   Z-ADD     v11aac        i00aac
187800990705     C                   Z-ADD     V11FIL        I00fil
187900990705     C                   Z-ADD     V11NCA        I00NCA
188000990705     C                   Z-ADD     tasAAS        I00AAS
188100990705     C                   Z-ADD     tasNRS        I00NRS
188200990705     C                   Z-ADD     tasNSP        I00NSP
188300990705     C                   Z-ADD     tasLNP        I00LNP
188400990705     C                   MOVEL     'S'           I00TPB
188500990706      *
188600990706     c                   exsr      RIED02_3
188700990706      *
188800990706     c                   ENDSR
188900990705      *****************************************************************
189000990705      * GESTIONE SUBFILE PER INTERROGAZIONE C.A.
189100990705      **********************************************************************
189200990705     C     RIED02_2      BEGSR
189300990705
189400990705     c                   DO        *hival
189500990706     C                   eval      nrr11 = 1
189600990707     C                   clear                   W001A
189700990705      * Emetto video
189800990706     C                   WRITE     SB51T10
189900990706     C                   WRITE     SB51Z00
190000120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
190100990705     C                   EXFMT     SB51C11
190200120302     c                   else
190300120302     c                   write     sb51C11
190400120302     c                   read(E)   tnsb51d
190500120302     c                   if        %error
190600120302     c                   eval      *inkc='1'
190700120302     c                   leave
190800120302     c                   endif
190900120302     c                   endif
191000990706      * Premuto un tasto funzionale
191100990706     C                   IF         *IN27 = *ON
191200990706     c                   leave
191300990706     C                   ENDIF
191400990705      *
191500990705     C                   READC     SB51S11
191600990705     C                   DOW       %EOF = *off
191700990705      *
191800990705     C                   IF        V11SCE = '5'
191900990707     c                   eval      W001A = 'S'
192000990705     C                   clear                   fidn00ds
192100990705     C                   exsr      ried02_1
192200990705     C                   clear                   fidn00ds
192300990705     C                   clear                   V11SCE
192400990705     C                   update    SB51S11
192500990706      *   se premuto F3 esco dal programma
192600990706     c   KC              leave
192700990705     C                   ENDIF
192800990705      *
192900990705     C                   READC     SB51S11
193000990705     C                   ENDDO
193100990706      *   se non effettuate scelte forzo F12
193200990707     c                   If        W001A = 'S'
193300990706     c                   eval       *inkl = *on
193400990706     c                   clear                   INFDSFMT
193500990706     c                   Endif
193600120302
193700990706      *
193800990706     C                   ENDDO
193900990705      *
194000990630     c                   ENDSR
194100990706      *****************************************************************
194200990706      * RICHIAMO E GESTIONE DEL PGM DI INTERROGAZIONE C.A.
194300990706      *****************************************************************
194400990706     C     RIED02_3      BEGSR
194500990706      *
194600990706     C                   movel(P)  fidn00ds      kpjbu
194700990706      *
194800990706     C                   CALL      'FIDN01R'
194900990706     C                   PARM                    KPJBA
195000000309     C                   PARM                    callricors
195100990706      *
195200990706     C                   movel     Kpjbu         fidn00ds
195300990706      *
195400990706      * Se ritorna premuto F3 accendo *INKC
195500990706      * Altrimenti se ho richiamato direttamente la CA ritorno alla videata di apertura
195600990706     C                   SELECT
195700990707     C                   WHEN      O00F03 <> *blanks
195800990706     c                   eval       *inkc = *on
195900990706     c                   WHEN      Wunaca = 'S'
196000990706     c                   eval       *inkl = *on
196100990706     c                   clear                   INFDSFMT
196200990706     c                   OTHER
196300990706     c                   eval       *inkl = *on
196400990707     C                   ENDSL
196500990706      *
196600990706     c                   ENDSR
196700010221      *****************************************************************
196800010221      * GESTIONE F4 = ORM
196900010221      *****************************************************************
197000010221     C     SUB_F04       BEGSR
197100010221      *
197200010221     C                   clear                   PARM01
197300010221     C                   move      v12poe        ppoe
197400010221     C                   move      v12nor        pnor
197500010221     C                   move      v12nsr        pnsr
197600010221     C                   move      v12nrv        pnrv
197700010221     C                   move      '5'           psce
197800010221     C                   move      v0clnp        pfgs
197900010221     C                   movel(p)  PARM01        KPJBU
198000010221
198100010221     C                   call      'FIOR07R'
198200010221     C                   parm                    KPJBA
198300010221     c                   clear                   INFDSFMT
198400010221     c                   eval      *inkl = *on
198500010221      *
198600010221     c                   ENDSR
198700990630      *****************************************************************
198800990705      * GESTIONE F5 = Eventi
198900990630      *****************************************************************
199000990705     C     SUB_F05       BEGSR
199100990705      *
199200990813     c                   clear                   LRB5DS
199300990813     C                   Z-ADD     tasAAS        pb5AAS
199400990813     C                   Z-ADD     tasLNP        pb5LNP
199500990813     C                   Z-ADD     tasNRS        pb5NRS
199600990813     C                   Z-ADD     tasNSP        pb5NSP
199700990813     c                   movel     RSUT          pb5DSA
199800990813     c                   z-add     V0CDSP        pb5DSP
199900990813     c                   z-add     tasRMN        pb5RMN
200000990813     c                   z-add     tasLNA        pb5LNA
200100990813     c                   movel     V0DLNA        pb5LNAD
200200990813     c                   movel     tasRSD        pb5RSD
200300990813     c                   z-add     Wtasdcp       pb5DCP
200400990813      *
200500990813     C                   MOVEL(P)  LRB5DS        KPJBU
200600990813     C                   CALL      'FNLRB5R'
200700990813     C                   PARM                    KPJBA
200800990813     C                   MOVEL     KPJBU         LRB5DS
200900990813      *
201000990813      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
201100990813     C                   IF        pb5F03 = '1'
201200990813     c                   eval      *inkc = *on
201300990813     c                   ELSE
201400990813     c                   clear                   INFDSFMT
201500990813     c                   eval      *inkl = *on
201600990813     C                   ENDIF
201700990705      *
201800990705     c                   ENDSR
201900170515      *****************************************************************
202000170515      * GESTIONE F6 = Manutenzione bolla
202100170515      *****************************************************************
202200170515     C     SUB_F06       BEGSR
202300170515     c                   clear                   TNSB01DS
202400170515      *
202500170515     c                   movel     ' 02'         D01OP0
202600170515     c                   z-add     tasaas        D01aas
202700170515     c                   z-add     taslnp        D01lnp
202800170515     c                   z-add     tasnrs        D01nrs
202900170515     c                   z-add     tasnsp        D01nsp
203000170515     c                   movel     tastbl        D01tbl
203100170515      *
203200170515     C                   movel(P)  TNSB01DS      Kpjbu
203300170515      *
203400170515     c                   call      'TNSB52R'
203500170515     c                   parm                    KPJBA
203600170515      *
203700170515     C                   movel     KPJBU         TNSB01DS
203800170515      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
203900170515     C                   IF        D01F03 = '1'
204000170515     c                   eval      *inkc = *on
204100170515     c                   ELSE
204200170515     c                   clear                   INFDSFMT
204300170515     c                   eval      *inkl = *on
204400170621     c                   eval      *inkf = *off
204500170621     c* memorizzo di richainare titas
204600170621     c                   eval      ch_titas='S'
204700170621     c                   if        i51tbl<>d01tbl
204800170621     c                   eval      i51tbl=d01tbl
204900170621     c                   endif
205000170515     C                   ENDIF
205100170515     c                   endsr
205200990705      *****************************************************************
205300990705      * GESTIONE F7 = Altri dati
205400990705      *****************************************************************
205500990705     C     SUB_F07       BEGSR
205600990705
205700990630      * Per il primo richiamo imposto i campi del video
205800990630     c                   IF        WSB51D07 = *blanks
205900990630     c                   exsr      ried07
206000990702     c                   eval      WSB51D07 = 'S'
206100990630     c                   ENDIF
206200990630
206300990707     c                   DO        *hival
206400990707
206500990630     c                   write     SB51T01
206600990630     c                   write     SB51Z00
206700120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
206800990630     c                   exfmt     SB51D07
206900120302     c                   else
207000120302     c                   write     sb51d07
207100120302     c                   read(E)   tnsb51d
207200120302     c                   if        %error
207300120302     c                   eval      *inkc='1'
207400120302     c                   leave
207500120302     c                   endif
207600120302     c                   endif
207700990707
207800990707     C                   clear                   W001A
207900120711      *
208000120711      * Interrogazione restituzione documenti al mittente
208100120711     c                   IF        *in67 and V7Idoc = '?'
208200120711     c                   exsr      sr_w02
208300120711     C                   clear                   V7Idoc
208400120711     C                   eval      W001A = 'S'
208500120711     C                   ENDIF
208600990630
208700170609      * Interrogazione contatti destinatario
208800170609     c                   IF        *in65 and V7Iemd = '?'
208900170609     C                   clear                   V7iemd
209000170609     c                   clear                   fnlv79ds
209100170609     c                   eval      ilv79TLA='L'
209200170609     c                   eval      ILV79AAS=tasaas
209300170609     c                   eval      ILV79LNP=taslnp
209400170609     c                   eval      ILV79NRS=tasnrs
209500170609     c                   eval      ILV79NSP=tasnsp
209600170609     c                   call      'FNLV79R'
209700170609     c                   parm                    fnlv79ds
209800170609     C                   eval      W001A = 'S'
209900170609     C                   ENDIF
210000990707      * Decodifica particolarità Giacenza
210100990707     c                   IF        V7Igga = '?'
210200990707     C                   clear                   ds7pqrs
210300990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
210400990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
210500990707     C                   movel     V7Cgga        DS7COD                         CODICE
210600990707     C                   movel(P)  DS7PQRS       KPJBU
210700990707     C                   CALL      'TRTB69R'
210800990707     C                   PARM                    KPJBA
210900990707     C                   clear                   V7Igga
211000990707     C                   eval      W001A = 'S'
211100990707     C                   ENDIF
211200990707
211300990707      * Decodifica particolarità Consegna
211400990707     c                   IF        V7Igma = '?'
211500990707     C                   clear                   ds7pqrs
211600990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
211700990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
211800990707     C                   movel     V7Cgma        DS7COD                         CODICE
211900990707     C                   movel(P)  DS7PQRS       KPJBU
212000990707     C                   CALL      'TRTB70R'
212100990707     C                   PARM                    KPJBA
212200990707     C                   clear                   V7Igma
212300990707     C                   eval      W001A = 'S'
212400990707     C                   ENDIF
212500990707
212600990707      * Decodifica particolarità Varie
212700990707     c                   IF        V7Igva = '?'
212800990707     C                   clear                   ds7pqrs
212900990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
213000990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
213100990707     C                   movel     V7Cgva        DS7COD                         CODICE
213200990707     C                   movel(P)  DS7PQRS       KPJBU
213300990707     C                   CALL      'TRTB73R'
213400990707     C                   PARM                    KPJBA
213500990707     C                   clear                   V7Igva
213600990707     C                   eval      W001A = 'S'
213700990707     C                   ENDIF
213800990707
213900990707     c                   IF        W001A = *blanks
214000990707     c                   leave
214100990707     C                   ENDIF
214200990707
214300990707     c                   ENDDO
214400990707
214500990630     c                   ENDSR
214600990630      **********************************************************************
214700990630      * IMPOSTO CAMPI VIDEO ALTRI DATI
214800990630      **********************************************************************
214900990630     C     RIED07        BEGSR
215000990708      *
215100990630      * Codice bolla
215200990702     c                   IF        TASCBO <> *blanks
215300990630     c                   movel     tascbo        V7Ccbo
215400990630     C                   eval      KCOD = '3A'
215500990630     C                   movel(P)  tascbo        KKEY
215600990630     C                   EXSR      CHKTAB
215700990702     C  n30              movel     Wtbluni       V7Dcbo
215800990702     c                   ENDIF
215900990708      * P.O. Partenza
216000990708     c                   z-add     V0Clnp        V7Clnp
216100990708     c                   z-add     V0Clnp        Wlinea
216200990708     C                   EXSR      CHKORG
216300990708     C                   movel     orgdes        V7Dlnp
216400990708      * Terminal Partenza
216500990708     c                   IF        TAStfp <> *zeros
216600990708     c                   z-add     TAStfp        V7Ctfp
216700990708     c                   z-add     tastfp        Wlinea
216800990708     C                   EXSR      CHKORG
216900990708     C                   movel     orgdes        V7Dtfp
217000990708     c                   ENDIF
217100990708      * Terminal Arrivo
217200990708     c                   IF        TAStfa <> *zeros
217300990708     c                   z-add     TAStfa        V7Ctfa
217400990708     c                   z-add     tastfa        Wlinea
217500990708     C                   EXSR      CHKORG
217600990708     C                   movel     orgdes        V7Dtfa
217700990708     c                   ENDIF
217800990708      * 1o P.O. Transito
217900040603     c**!!!              IF        TASFLP <> *zeros
218000040603     c**!!!              eval      *in48 = *ON
218100040603     c**!!!              z-add     TASflp        V7Cflp
218200040603     c**!!!              z-add     tasflp        Wlinea
218300040603     C**!!!              EXSR      CHKORG
218400040603     C**!!!              movel     orgdes        V7Dflp
218500040603     c**!!!              ENDIF
218600990708      * 2o P.O. Transito, se non presente non emetto scritta
218700040603     c**!!!              IF        TASFL2 <> *zeros
218800040603     c**!!!              eval      *in49 = *ON
218900040603     c**!!!              z-add     TASfl2        V7Cfl2
219000040603     c**!!!              z-add     tasfl2        Wlinea
219100040603     C**!!!              EXSR      CHKORG
219200040603     C**!!!              movel     orgdes        V7Dfl2
219300040603     c**!!!              ENDIF
219400990630      * Sigla Operatore
219500990630     C                   movel     tassop        V7Csop
219600990630      * Trattamento merce
219700990630     C                   movel     tasctm        V7Cctm
219800990630      * Stampa lettera di vettura
219900170522     C***                SELECT
220000170522     C***                WHEN      TASLL1 = *blanks
220100170522     C***                WHEN      TASLL1 = 'C'  or  TASLL1 = 'K'
220200170522     C***                eval      V7Dll1 = 'CLI'
220300170522     C***                WHEN      TASLL1 = 'S'  or  TASLL1 = 'Y'
220400170522     C***                eval      V7Dll1 = 'PAR'
220500170522     c***                OTHER
220600170522     C***                eval      V7Dll1 = 'ARR'
220700170522     C***                ENDSL
220800110610      * email destinatario
220900110610     c                   setoff                                       65
221000110610      *
221100130926If  1c                   If        §floemd<>*blanks
221200110610     C                   SETON                                        65
221300170612     c                   eval      v7cemd='SI'
221400170612     c***                select
221500170612     c***                when      §floemd='E'
221600170612     c***                Eval      V7cemd = 'MAIL+SMS'
221700170612     c***                when      §floemd='S'
221800170612     c***                Eval      V7cemd = 'MAIL    '
221900170612     c***                when      §floemd='X'
222000170612     c***                Eval      V7cemd = 'SMS     '
222100170612     c***                endsl
222200110610    1c                   EndIf
222300990630      * Bolla rientro
222400990630     C                   IF        TASfbr = 'S'
222500990630     C                   eval      V7Dfbr = 'SI'
222600990630     C                   ENDIF
222700990630      * Franco Frontiera
222800170522     c                   setoff                                       73
222900990630     C                   IF        TASfbr = 'F'
223000990630     C                   eval      V7Dffr = 'SI'
223100170522     C                   seton                                        73
223200990630     C                   ENDIF
223300170522      * Bancali
223400170522     c                   Clear                   V0cNba
223500170522     c                   Clear                   V0cNb2
223600170522     c                   Clear                   V0dTba
223700170522     c                   Clear                   V0dTb2
223800170522     c                   setoff                                       74
223900170522      * Bancali - part.varie "B"
224000170522If  0c                   If        %subst(TasGva:1:1) = 'B'
224100170522     c                   Clear                   dAr5Ban
224200170522     c                   Eval      wAr5Trd = 'BAN'
224300170522     c     kSpeAr5       Chain     Fiar531c
224400170522If  1c                   If        %Found(Fiar531c)
224500170522     c                   seton                                        74
224600170522     c                   Movel     Ar5Uni        Dar5Ban
224700170522     c                   Eval      V0cNba = §Ar5Nba
224800170522     c                   Eval      V0cNb2 = §Ar5Nb2
224900170522If  2c                   If        §Ar5Tba <> *Zeros
225000170522     c                   Eval      wtba = §Ar5Tba
225100170522     c                   ExSr      Sr_DecTba
225200170522     c                   Eval      V0dTba = wdestba
225300170522    2c                   EndIf
225400170522If  2c                   If        §Ar5Tb2 <> *Zeros
225500170522     c                   Eval      wtba = §Ar5Tb2
225600170522     c                   ExSr      Sr_DecTba
225700170522     c                   Eval      V0dTb2 = wdestba
225800170522    2c                   EndIf
225900170522    1c                   EndIf
226000170522    0c                   EndIf
226100170522      * Bancali - part.varie "O"
226200170522If  0c                   If        %subst(TasGva:1:1) = 'O'
226300170522     c                   Clear                   dAr5Bnb
226400170522     c                   Eval      wAr5Trd = 'BNB'
226500170522     c     kSpeAr5       Chain     Fiar531c
226600170522If  1c                   If        %Found(Fiar531c)
226700170522     c                   seton                                        74
226800170522     c                   Movel     Ar5Uni        Dar5Bnb
226900170522     c                   Eval      V0cNba = §Ar5bNba
227000170522     c                   Eval      V0cNb2 = §Ar5bNb2
227100170522If  2c                   If        §Ar5bTba <> *Zeros
227200170522     c                   Eval      wtba = §Ar5bTba
227300170522     c                   ExSr      Sr_DecTba
227400170522     c                   Eval      V0dTba = wdestba
227500170522    2c                   EndIf
227600170522If  2c                   If        §Ar5bTb2 <> *Zeros
227700170522     c                   Eval      wtba = §Ar5bTb2
227800170522     c                   ExSr      Sr_DecTba
227900170522     c                   Eval      V0dTb2 = wdestba
228000170522    2c                   EndIf
228100170522    1c                   EndIf
228200170522    0c                   EndIf
228300990630      * BAM si/no
228400991123     C                   SELECT
228500991123     C                   WHEN      TASLL1 = *blanks
228600991123     C                   WHEN      TASLL1 = 'C'  or  TASLL1 = 'S'
228700990630     C                   eval      V7Dddt = 'SI'
228800991123     C                   OTHER
228900990630     C                   eval      V7Dddt = 'NO'
229000991123     C                   ENDSL
229100990630      * Emessa Nota Accredito
229200990630     C                   IF        TASfa2 = 'S'
229300990630     C                   eval      V7Dfa2 = 'SI'
229400990630     C                   ENDIF
229500050620      * note partenza
229600050621     c                   setoff                                       58
229700050620     c                   eval      ktrc = '8'
229800050620     c                   clear                   v7dnt1
229900050620     c     KTAStrc       CHAIN     TITA430C
230000050620     c                   If        %FOUND
230100050620     c                   movel     TA4not        v7dnt1
230200050621     c                   seton                                        58
230300050620     c                   ENDIF
230400050620     c                   eval      ktrc = '9'
230500050620     c                   clear                   v7dnt2
230600050620     c     KTAStrc       CHAIN     TITA430C
230700050620     c                   If        %FOUND
230800050620     c                   movel     TA4not        v7dnt2
230900050621     c                   seton                                        58
231000050620     c                   ENDIF
231100990630      * Importo da assicurare e flag tipo inserimento
231200990630     c                   IF        TASias > *zeros
231300990630     c                   z-add     TASias        V7Cias
231400990630     c                   If        TASvas = *blanks
231500990630     c                   movel     'ITL'         V7Cvas
231600990630     c                   else
231700990630     c                   movel     TASvas        V7Cvas
231800990630     C                   Endif
231900990720     c                   eval      *IN50 = (TASfcm <> *blanks)
232000990630     C                   ENDIF
232100990630      * Particolarità
232200990630     c                   movel     TASgga        V7Cgga
232300990707     c                   eval      *in45 = (V7Cgga <> *blanks)
232400990630     c                   movel     TASgma        V7Cgma
232500990707     c                   eval      *in46 = (V7Cgma <> *blanks)
232600990630     c                   movel     TASgva        V7Cgva
232700990712     c                   move      ' '           V7Cgva
232800990707     c                   eval      *in47 = (V7Cgva <> *blanks)
232900990630      *
233000120330      * decopdifico motivo DDt errat
233100990630     c                   ENDSR
233200990630      *****************************************************************
233300000901      * GESTIONE F08 = Cod."CHI SONO"
233400990630      *****************************************************************
233500000901     C     SUB_F08       BEGSR
233600000901
233700000901      * Per il primo richiamo imposto i campi del video
233800000901     c                   IF        WSB51D08 = *blanks
233900000901     c                   exsr      ried08
234000000901     c                   eval      WSB51D08 = 'S'
234100000901     c                   ENDIF
234200000901
234300000901     c                   z-add     1             nrr08
234400000901
234500000901     C                   WRITE     SB51T10
234600000901     C                   WRITE     SB51Z00
234700120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
234800000901     C                   EXFMT     SB51C08
234900120302     c                   else
235000120302     c                   write     sb51c08
235100120302     c                   read(E)   tnsb51d
235200120302     c                   if        %error
235300120302     c                   eval      *inkc='1'
235400120302     c                   endif
235500120302     c                   endif
235600000901
235700000901     C                   ENDSR
235800000901      *****************************************************************
235900000901      * IMPOSTO CAMPI VIDEO CODICE "CHI SONO"
236000000901      *****************************************************************
236100000901     C     RIED08        BEGSR
236200000901
236300000901      *
236400000901      * Pulizia subfile
236500000901     C                   eval      *in26 = *off
236600000901     C                   WRITE     SB51C08
236700000901     C                   eval      *in25 = *on
236800000901     C                   eval      *in26 = *on
236900000901     C                   clear                   nrr08
237000000901      *
237100020226     c                   eval      wtahtrc = 'C'
237200020226     C     KEYspeh       SETLL     TITAH30C
237300020226     C     KEYspeh       READE     TITAH30C
237400000901      *
237500000901     C                   DOW       %EOF = *off
237600000901     c                   move      tahnot        V08not
237700010524     c                   z-add     tahnsc        v08nsc
237800000901     C                   move      '7H'          kcod
237900000901     C                   movel(p)  tahtrc        kkey
238000000901     C     KEYTAB        CHAIN     TABEL00F                           30
238100000901     c                   IF        *IN30 = *ON
238200000901     C                   else
238300000901     C                   ENDIF
238400000901      *
238500000901     c                   ADD       1             nrr08
238600000901     c                   write     SB51s08
238700000901      *
238800020226     C     KEYspeh       READE     TITAH30C
238900000901     c                   ENDDO
239000000901
239100000901     c                   ENDSR
239200040603      *****************************************************************
239300040603      * GESTIONE F09 = Transiti
239400040603      *****************************************************************
239500040603     C     SUB_F09       BEGSR
239600040603
239700040603      * Per il primo richiamo imposto i campi del video
239800040603     c                   IF        WSB51D09 = *blanks
239900040603     c                   exsr      ried09
240000040603     c                   eval      WSB51D09 = 'S'
240100040603     c                   ENDIF
240200040603
240300040603     c                   z-add     1             nrr09
240400040603
240500040603     C                   WRITE     SB51T10
240600040603     C                   WRITE     SB51Z00
240700120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
240800040603     C                   EXFMT     SB51C09
240900120302     c                   else
241000120302     c                   write     sb51c09
241100120302     c                   read(E)   tnsb51d
241200120302     c                   if        %error
241300120302     c                   eval      *inkc='1'
241400120302     c                   endif
241500120302     c                   endif
241600040603
241700040603     C                   ENDSR
241800040603      *****************************************************************
241900040603      * IMPOSTO CAMPI VIDEO TRANSITI
242000040603      *****************************************************************
242100040603     C     RIED09        BEGSR
242200040603
242300040603      * Pulizia subfile
242400040603     c                   eval      *in26 = *off
242500040603     c                   WRITE     SB51C09
242600040603     c                   eval      *in25 = *on
242700040603     c                   eval      *in26 = *on
242800040603     c                   clear                   nrr09
242900040603
243000040603      * Transiti da Fiar5 rcd TRS
243100040604     c                   Eval      woktrs = *Off
243200040603     c                   Eval      wAr5Trd = 'TRS'
243300040604     c     kSpeAr5       Setgt     Fiar531c
243400040604     c                   Do        *Hival
243500040604     c     kSpeAr5       Readpe    Fiar531c
243600040604     c                   If        %Eof(Fiar531c)
243700040604     c                   Leave
243800040604     c                   EndIf
243900040604     c                   Eval      woktrs = *On
244000040603     c                   Eval      dAr5Trs = Ar5Uni
244100040603      * 1° p.o. Transito
244200040603     c                   If        §Ar5Flp > *Zeros
244300040603     c                   Eval      V09Fil = §Ar5Flp
244400040603     c                   Z-add     §Ar5Flp       Wlinea
244500040603     c                   ExSr      CHKORG
244600040603     c                   Eval      V09Dfl = OrgDes
244700040603     c                   If        §Ar5Det > *Zeros
244800040604     c     *iso          Move      §Ar5Det       DATA_eur
244900040603     c     *eur          Move      DATA_eur      V09Det
245000040608     c                   Eval      *In48 = *Off
245100040603     c                   Else
245200040603     c                   Clear                   V09Det
245300040608     c                   Eval      *In48 = *On
245400040603     c                   EndIf
245500040603     c                   If        §Ar5Dut > *Zeros
245600040604     c     *iso          Move      §Ar5Dut       DATA_eur
245700040603     c     *eur          Move      DATA_eur      V09Dut
245800040608     c                   Eval      *In49 = *Off
245900040603     c                   Else
246000040603     c                   Clear                   V09Dut
246100040608     c                   Eval      *In49 = *On
246200040603     c                   EndIf
246300040603     c                   Add       1             nrr09
246400040603     c                   Write     SB51s09
246500040603     c                   EndIf
246600040603      * 2° p.o. Transito
246700040603     c                   If        §Ar5Fl2 > *Zeros
246800040603     c                   Eval      V09Fil = §Ar5Fl2
246900040603     c                   Z-add     §Ar5Fl2       Wlinea
247000040603     c                   ExSr      CHKORG
247100040603     c                   Eval      V09Dfl = OrgDes
247200040727     c                   If        §Ar5De2 > *Zeros
247300040604     c     *iso          Move      §Ar5De2       DATA_eur
247400040603     c     *eur          Move      DATA_eur      V09Det
247500040608     c                   Eval      *In48 = *Off
247600040603     c                   Else
247700040603     c                   Clear                   V09Det
247800040608     c                   Eval      *In48 = *On
247900040603     c                   EndIf
248000040727     c                   If        §Ar5Du2 > *Zeros
248100040604     c     *iso          Move      §Ar5Du2       DATA_eur
248200040603     c     *eur          Move      DATA_eur      V09Dut
248300040608     c                   Eval      *In49 = *Off
248400040603     c                   Else
248500040603     c                   Clear                   V09Dut
248600040608     c                   Eval      *In49 = *On
248700040603     c                   EndIf
248800040603     c                   Add       1             nrr09
248900040603     c                   Write     SB51s09
249000040603     c                   EndIf
249100040603      * 3° p.o. Transito
249200040603     c                   If        §Ar5Fl3 > *Zeros
249300040603     c                   Eval      V09Fil = §Ar5Fl3
249400040603     c                   Z-add     §Ar5Fl3       Wlinea
249500040603     c                   ExSr      CHKORG
249600040603     c                   Eval      V09Dfl = OrgDes
249700040727     c                   If        §Ar5De3 > *Zeros
249800040604     c     *iso          Move      §Ar5De3       DATA_eur
249900040603     c     *eur          Move      DATA_eur      V09Det
250000040608     c                   Eval      *In48 = *Off
250100040603     c                   Else
250200040603     c                   Clear                   V09Det
250300040608     c                   Eval      *In48 = *On
250400040603     c                   EndIf
250500040727     c                   If        §Ar5Du3 > *Zeros
250600040604     c     *iso          Move      §Ar5Du3       DATA_eur
250700040603     c     *eur          Move      DATA_eur      V09Dut
250800040608     c                   Eval      *In49 = *Off
250900040603     c                   Else
251000040603     c                   Clear                   V09Dut
251100040608     c                   Eval      *In49 = *On
251200040603     c                   EndIf
251300040608     c                   Add       1             nrr09
251400040603     c                   Write     SB51s09
251500040603     c                   EndIf
251600040603      * 4° p.o. Transito
251700040603     c                   If        §Ar5Fl4 > *Zeros
251800040603     c                   Eval      V09Fil = §Ar5Fl4
251900040603     c                   Z-add     §Ar5Fl4       Wlinea
252000040603     c                   ExSr      CHKORG
252100040603     c                   Eval      V09Dfl = OrgDes
252200040727     c                   If        §Ar5De4 > *Zeros
252300040604     c     *iso          Move      §Ar5De4       DATA_eur
252400040603     c     *eur          Move      DATA_eur      V09Det
252500040608     c                   Eval      *In48 = *Off
252600040603     c                   Else
252700040603     c                   Clear                   V09Det
252800040608     c                   Eval      *In48 = *On
252900040603     c                   EndIf
253000040727     c                   If        §Ar5Du4 > *Zeros
253100040604     c     *iso          Move      §Ar5Du4       DATA_eur
253200040603     c     *eur          Move      DATA_eur      V09Dut
253300040608     c                   Eval      *In49 = *Off
253400040603     c                   Else
253500040603     c                   Clear                   V09Dut
253600040608     c                   Eval      *In49 = *On
253700040603     c                   EndIf
253800040603     c                   Add       1             nrr09
253900040603     c                   Write     SB51s09
254000040603     c                   EndIf
254100040603      * 5° p.o. Transito
254200040603     c                   If        §Ar5Fl5 > *Zeros
254300040603     c                   Eval      V09Fil = §Ar5Fl5
254400040603     c                   Z-add     §Ar5Fl5       Wlinea
254500040603     c                   ExSr      CHKORG
254600040603     c                   Eval      V09Dfl = OrgDes
254700040727     c                   If        §Ar5De5 > *Zeros
254800040604     c     *iso          Move      §Ar5De5       DATA_eur
254900040603     c     *eur          Move      DATA_eur      V09Det
255000040608     c                   Eval      *In48 = *Off
255100040603     c                   Else
255200040603     c                   Clear                   V09Det
255300040608     c                   Eval      *In48 = *On
255400040603     c                   EndIf
255500040727     c                   If        §Ar5Du5 > *Zeros
255600040604     c     *iso          Move      §Ar5Du5       DATA_eur
255700040603     c     *eur          Move      DATA_eur      V09Dut
255800040608     c                   Eval      *In49 = *Off
255900040603     c                   Else
256000040603     c                   Clear                   V09Dut
256100040608     c                   Eval      *In48 = *On
256200040603     c                   EndIf
256300040603     c                   Add       1             nrr09
256400040603     c                   Write     SB51s09
256500040603     c                   EndIf
256600040604     c                   Leave
256700040604     c                   EndDo
256800040603
256900040603      * Transiti da TITAS
257000040604     c                   If        woktrs = *Off
257100040603      * 1° p.o. Transito
257200040603     c                   If        TasFlp > *Zeros
257300040603     c                   Eval      V09Fil = TasFlp
257400040603     c                   Z-add     TasFlp        Wlinea
257500040603     c                   ExSr      CHKORG
257600040603     c                   Eval      V09Dfl = OrgDes
257700040603     c                   Clear                   V09Det
257800040603     c                   Clear                   V09Dut
257900040608     c                   Eval      *In48 = *On
258000040608     c                   Eval      *In49 = *On
258100040603     c                   Add       1             nrr09
258200040603     c                   Write     SB51s09
258300040603     c                   EndIf
258400040603      * 2° p.o. Transito
258500040603     c                   If        TasFl2 > *Zeros
258600040603     c                   Eval      V09Fil = TasFl2
258700040603     c                   Z-add     TasFl2        Wlinea
258800040603     c                   ExSr      CHKORG
258900040603     c                   Eval      V09Dfl = OrgDes
259000040603     c                   Clear                   V09Det
259100040603     c                   Clear                   V09Dut
259200040608     c                   Eval      *In48 = *On
259300040608     c                   Eval      *In49 = *On
259400040603     c                   Add       1             nrr09
259500040603     c                   Write     SB51s09
259600040603     c                   EndIf
259700040603     c                   EndIf
259800040603
259900040603     C                   ENDSR
260000000901      *****************************************************************
260100000901      * GESTIONE F10 = Variazioni
260200000901      *****************************************************************
260300000901     C     SUB_F10       BEGSR
260400990804      *
260500990804     c                   clear                   TNSB01DS
260600990804     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
260700990804     C                   MOVEL     'F10'         D01OP1                         *LIVELLO PROGRAMMA
260800990804     C                   Z-ADD     tasAAS        d01AAS                         *ANNO
260900990804     C                   Z-ADD     tasLNP        d01LNP                         *LNP
261000020402     C                   Z-ADD     tasLNA        d01LNA                         *LNA
261100990804     C                   Z-ADD     tasNRS        d01NRS                         *SERIE
261200990804     C                   Z-ADD     tasNSP        d01NSP                         *SPEDIZIONE
261300990804     C                   Z-ADD     V0CDSP        D01DSP                         * GG/MM/AAAA
261400990804     C                   MOVEL     '0'           D01F03                         *USCITA PGM
261500990804     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
261600990804     C                   MOVEL     '0'           D01ERR                         *ERRORE
261700990804      *
261800990804     C                   MOVEL     TNSB01DS      KPJBU
261900990804      *
262000170118     c***                if        knmus<>'EDPRM'
262100170118     C***                CALL      'TNSB53R'
262200170118     C***                PARM                    KPJBA
262300990804      *
262400170118     C***                MOVEL     KPJBU         TNSB01DS
262500990804      *
262600990804      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
262700170118     C***                IF        D01F03 = '1'
262800170118     c***                eval      *inkc = *on
262900170118     c***                ELSE
263000170118     c***                clear                   INFDSFMT
263100170118     c***                eval      *inkl = *on
263200170118     C***                ENDIF
263300170118     c***                else
263400160930     c                   clear                   lr75ds
263500160930     c                   eval      p75aas=tasaas
263600160930     c                   eval      p75lnp=taslnp
263700160930     c                   eval      p75nrs=tasnrs
263800160930     c                   eval      p75nsp=tasnsp
263900160930     c                   eval      p75DSA=rsut
264000160930     c                   eval      P75TBO='S'
264100160930     c                   eval      p75tbl=tastbl
264200160930     c                   eval      kpjbu=lr75ds
264300160930     c                   call      'FNLR75R'
264400160930     C                   PARM                    KPJBA
264500160930      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
264600160930     C                   IF        p75F03 = '1'
264700160930     c                   eval      *inkc = *on
264800160930     c                   ELSE
264900160930     c                   clear                   INFDSFMT
265000160930     c                   eval      *inkl = *on
265100160930     C                   ENDIF
265200170118     c***                endif
265300990630      *
265400990630     c                   ENDSR
265500030120      *****************************************************************
265600030120      * GESTIONE F11 = Note Supermercato
265700030120      *****************************************************************
265800030120     C     SUB_F11       BEGSR
265900030120      *
266000030121     c                   Clear                   FnLrb3Ds
266100030122     c                   Movel     RSUT          Rb3Rsu
266200030121     c                   Eval      Rb3Fil = Simfel
266300030121     c                   Eval      Rb3Aas = TasAas
266400030121     c                   Eval      Rb3Lnp = TasLnp
266500030121     c                   Eval      Rb3Nrs = TasNrs
266600030121     c                   Eval      Rb3Nsp = TasNsp
266700030127     c                   Z-Add     TasAas        Rb3Dsp
266800030127     c                   movel     TasMgs        Rb3Dsp
266900030127     c     *usa          move      Rb3Dsp        DATA_eur
267000030127     c     *eur          move      DATA_eur      Rb3Dsp
267100030121     c                   Eval      Rb3Rmn = TasRmn
267200030121     c                   Eval      Rb3Lna = TasLna
267300030121     c                   Eval      Rb3Rsd = TasRsd
267400030121     c                   Eval      Rb3Pkf = TasPkf
267500030121     c                   Eval      Rb3Ncl = TasNcl
267600030121     c                   Eval      Rb3Vlb = TasVlb
267700030703      * bancali - part.varie "B"
267800030703If  0c                   If        %subst(TasGva:1:1) = 'B'
267900030121     c                   Eval      Rb3Tba = §Ar5Tba
268000030121     c                   Eval      Rb3Nba = V0cNba
268100030121     c                   Eval      Rb3Tb2 = §Ar5Tb2
268200030121     c                   Eval      Rb3Nb2 = V0cNb2
268300030703     c                   Eval      Rb3Gva = TasGva
268400030703    0c                   EndIf
268500030703If  0c                   If        %subst(TasGva:1:1) = 'O'
268600030703     c                   Eval      Rb3Tba = §Ar5bTba
268700030703     c                   Eval      Rb3Nba = V0cNba
268800030703     c                   Eval      Rb3Tb2 = §Ar5bTb2
268900030703     c                   Eval      Rb3Nb2 = V0cNb2
269000030703     c                   Eval      Rb3Gva = TasGva
269100030703     c                   Eval      Rb3Csf = §Ar5bCsf
269200030703     c                   Eval      Rb3Cor = §Ar5bCor
269300030703    0c                   EndIf
269400030121      * richiamo PGM per note supermercati
269500030121     c                   Call      'FNLRB3R'
269600030122     c                   Parm                    Kpjba
269700030121     c                   Parm                    Fnlrb3Ds
269800030121
269900030121     c                   clear                   INFDSFMT
270000030121     c                   eval      *inkl = *on
270100030121
270200030121     c                   EndSr
270300990630      *****************************************************************
270400990630      * GESTIONE F13 = Tassazione
270500990630      *****************************************************************
270600990630     C     SUB_F13       BEGSR
270700990701
270800990701      * Per il primo richiamo imposto i campi del video
270900990701     c                   IF        WSB51D13 = *blanks
271000990701     c                   exsr      ried13
271100990701     c                   eval      WSB51D13 = 'S'
271200990701     c                   ENDIF
271300990701
271400990705     c                   z-add     1             nrr13
271500160621
271600160621     c                   DO        *hival
271700990702
271800990701     c                   write     SB51T10
271900990701     c                   write     SB51Z00
272000120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
272100990701     c                   exfmt     SB51C13
272200120302     c                   else
272300120302     c                   write     sb51c13
272400120302     c                   read(E)   tnsb51d
272500120302     c                   if        %error
272600120302     c                   eval      *inkc='1'
272700160621     c                   leave
272800120302     c                   endif
272900120302     c                   endif
273000160621
273100160621     c                   clear                   w001a
273200160621
273300160621      * '?' su Ottico
273400160621     c                   IF        *in72 and V13ottico = '?'
273500160621     c                   exsr      VisFattura
273600160621     c                   clear                   V13ottico
273700160621     c                   eval      w001a = 'S'
273800160621     c                   ENDIF
273900160621
274000160621     c                   IF        w001a = *blanks
274100160621     c                   leave
274200160621     c                   ENDIF
274300160621
274400160621     c                   ENDDO
274500990701
274600990630     c                   ENDSR
274700990701      **********************************************************************
274800990701      * IMPOSTO CAMPI VIDEO TASSAZIONE
274900990701      **********************************************************************
275000990701     C     RIED13        BEGSR
275100070524
275200070524     c                   eval      *in61 = *off
275300070524     c                   eval      *in62 = *off
275400070524     c                   eval      *in63 = *off
275500151020     c                   clear                   v13peso
275600160621     c                   eval      *in72 = *off
275700990701      *
275800990702      * Fatturata con tariffa Mittente
275900990702     c                   IF        TASftm = 'S'
276000990702     c                   eval      V13ftm = 'SI'
276100990702     c                   ENDIF
276200051103      * Peso fatturato
276300051103      * se bolla fatturata imposto il peso da fatturare che visualizzo in prima videata
276400151016     c                   If        TasDft > *Zeros  or
276500151016     c                             TASimv > *Zeros
276600051103     c                   Eval      v13peso = v0cpeso
276700151016     c*//                Else
276800151016      *//* se bolla da fatturare imposto taspkf
276900151016     c*//                Eval      v13peso = taspkf
277000051103     c                   EndIf
277100990702      * Peso tassabile
277200990702     c                   z-add     Taspvl        v13pvl
277300990702      * Quantità da fatturare
277400990702     c                   z-add     Tasqft        v13qft
277500990702      * Nazione e CAP fatturazione Mittente
277600990702     c                   movel     tasnzm        V13nzm
277700990702     c                   movel     tascam        V13cam
277800990702      * Codice tassazione Mittente
277900990702     c                   IF        TASmct <> *blanks
278000990702     c                   movel     tasmct        V13mct
278100990702     C                   eval      KCOD = 'CT'
278200990702     C                   movel(P)  tasmct        KKEY
278300990702     C                   EXSR      CHKTAB
278400990702     C  n30              movel     Wtbluni       V13dmt
278500990702     c                   ENDIF
278600990702      * Codice tassazione Destinatario
278700990702     c                   IF        TAScts <> *blanks
278800990702     c                   movel     tascts        V13cts
278900990702     C                   eval      KCOD = 'CT'
279000990702     C                   movel(P)  tascts        KKEY
279100990702     C                   EXSR      CHKTAB
279200990702     C  n30              movel     Wtbluni       V13dct
279300990702     c                   ENDIF
279400990702      * Codice tariffa
279500990702     c                   z-add     tasctr        v13ctr
279600990702     C                   eval      KCOD = 'TR'
279700990702     C                   movel     tasctr        w001a
279800990702     C                   movel(P)  W001A         KKEY
279900990702     C                   EXSR      CHKTAB
280000990702     C  n30              movel     Wtbluni       V13dtr
280100160621      * Ottico
280200160830      * per ora SOLO EDP (per tutti dal 1° Settembre 2016)
280300160621      * solo se bolla contabilizzata
280400160621      * se c'è fattura
280500160621      * se non è una distinta
280600160621      * e se tipo bolla che genera fattura
280700160621     c                   IF        TASfic <> *blanks and
280800160621     c                             TASnft > 0 and TASdft > 0 and
280900160621     c                             TASnft <> *all'9' and
281000160830     c                             §TBfcb = '1'
281100160830     c***                          %subst(knmus:1:3) = 'EDP'
281200160621     c                   eval      *in72 = *on
281300160621     c                   ENDIF
281400990702      * Divisa
281500990702     c                   movel     tasdiv        v13div
281600990708      * Importo fattura se fattura immediata
281700990708     c                   IF        *IN41 = *off
281800990708     c                   movel     V1Cimp        v13ift
281900990708     C                   ENDIF
282000990702      * Porto
282100990702     c                   IF        TASpor > *zeros
282200990702     c                   z-add     TASpor        W0133
282300990702     c                   movel     W§cvfdc       Wtstdec
282400990702     c                   exsr      EDTimporti
282500990702     c                   movel     IMPeditato    v13por
282600070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
282700070524     c                   eval      Ksvt = ' '
282800070524     c     Ktitai        chain     titai01l
282900070524     c                   if        %found(titai01l)
283000070524     c                   eval      *in61 = *on
283100070524     c                   endif
283200070524
283300990702     c                   ENDIF
283400990702      * Imponibile
283500990702     c                   IF        TASimv > *zeros
283600990702     c                   z-add     TASimv        W0133
283700990702     c                   movel     W§cvfdc       Wtstdec
283800990702     c                   exsr      EDTimporti
283900990702     c                   movel     IMPeditato    v13imv
284000070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
284100070524     c                   eval      Ksvt = '£'
284200070524     c     Ktitai        chain     titai01l
284300070524     c                   if        %found(titai01l)
284400070524     c                   eval      *in62 = *on
284500070524     c                   endif
284600990702     c                   ENDIF
284700990702      * Esenzione IVA
284800990702     c                   IF        TASfei <> *blanks
284900990702     c                   movel     tasfei        v13fei
285000990702     C                   eval      KCOD = 'EI'
285100990702     C                   movel(P)  tasfei        KKEY
285200990702     C                   EXSR      CHKTAB
285300990702     C  n30              movel     Wtbluni       V13dei
285400990702     c                   ENDIF
285500990702      * Recupero codice varia IVA
285600990702     C                   eval      KCOD = '$2'
285700990702     C                   movel(P)  1             KKEY
285800990702     C                   EXSR      CHKTAB
285900990702     C  n30              movel     Wtbluni       DS$2
286000990702      *
286100990702      * VARIE
286200990702      *    pulizia subfile
286300990702     C                   eval         *IN26 = *OFF
286400990702     C                   WRITE     SB51C13
286500990702     C                   eval         *IN25 = *ON
286600990702     C                   eval         *IN26 = *ON
286700990702      *    varia 1 TAS
286800990702     c                   IF        tassv1 <> *blanks
286900990702     c                   eval      v13svn = tassv1
287000990702     c                   z-add     TASva1        W0133
287100990702     c                   exsr      AGGIOSBF
287200990702     c                   ENDIF
287300990702      *    varia 2 TAS
287400990702     c                   IF        tassv2 <> *blanks
287500990702     c                   eval      v13svn = tassv2
287600990702     c                   z-add     TASva2        W0133
287700990702     c                   exsr      AGGIOSBF
287800990702     c                   ENDIF
287900990702      *    varia 3 TAS
288000990702     c                   IF        tassv3 <> *blanks
288100990702     c                   eval      v13svn = tassv3
288200990702     c                   z-add     TASva3        W0133
288300990702     c                   exsr      AGGIOSBF
288400990702      *    carico ulteriori varie
288500990702     C     keyspe1       setll     tita730c
288600990702     C     keyspe1       reade     tita730c
288700990705     C                   DOW       %EOF = *off
288800990702     c                   IF        TA7svn <> *blanks
288900990702     c                   eval      v13svn = ta7svn
289000990702     c                   z-add     TA7van        W0133
289100990702     c                   exsr      AGGIOSBF
289200990702     c                   ENDIF
289300990702     C     keyspe1       reade     tita730c
289400990702     c                   ENDDO
289500990702      *
289600990702     c                   ENDIF
289700990705      *    se non ci sono varie carico 1 rcd vuoto
289800990705     c                   IF        NRR13 = *ZEROS
289900990708     C                   clear                   v13svn
290000990708     C                   clear                   V13dsv
290100990708     c                   clear                   v13van
290200990705     c                   add       1             nrr13
290300990705     c                   write     sb51s13
290400990705     c                   ENDIF
290500990705      *
290600990702     c                   ENDSR
290700990702      *****************************************************************
290800990702      * CARICO SUBFILE VARIE
290900990702      *****************************************************************
291000990702     C     AGGIOSBF      BEGSR
291100990702      *
291200990702     c                   movel     W§cvfdc       Wtstdec
291300990702     c                   exsr      EDTimporti
291400990702      *
291500990702      * La varia IVA la scrivo nel control e non nel subfile
291600990702     C                   IF        §$2iva = v13SVN
291700990702     c                   movel     IMPeditato    v13iva
291800990702     c                   ELSE
291900990705     c                   movel     IMPeditato    v13van
292000990702      *
292100990702     C                   eval      KCOD = 'CC'
292200990702     C                   movel(P)  'VARIE'       KKEY
292300990702     C                   move      v13svn        KKEY
292400990702     C                   EXSR      CHKTAB
292500020328     C                   movel     TBLuni        DSCC
292600020328     C     §ccdfe        ifeq      'S'
292700020328      *
292800020328      * Verifico se si tratta di una bolla Fedex
292900021119     c                   if        LnaNtw = 'FED'
293000020328      * recupero decodifica da tabella CB
293100020328     C                   eval      KCOD = 'CB'
293200020328     C                   movel(P)  'VARIE'       KKEY
293300020328     C                   move      V13svn        KKEY
293400020328     C                   EXSR      CHKTAB
293500020328     c                   endif
293600020328     c                   endif
293700020328      *
293800990702     C  n30              movel     Wtbluni       V13dsv
293900070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
294000070524     c                   eval      Ksvt = v13svn
294100070524     c     Ktitai        chain     titai01l
294200070524     c                   if        %found(titai01l)
294300070524
294400070524     c                   if        §$2iva = v13svn
294500070524     c                   eval      *in63 = *on
294600070524     c                   else
294700070524     c                   eval      *in64 = *on
294800070524     c                   endif
294900070524
295000070524     c                   else
295100070524     c                   eval      *in64 = *off
295200070524     c                   endif
295300070524
295400990702      *
295500990705     c                   add       1             nrr13
295600990702     c                   write     sb51s13
295700990702      *
295800990702     c                   ENDIF
295900990702      *
296000990701     c                   ENDSR
296100041007      *****************************************************************
296200041007      * GESTIONE F01 = Aggiunta Richiesta Assistenza
296300041007      *****************************************************************
296400041007     C     SUB_F01       BEGSR
296500041007      *
296600041007     c                   clear                   FIDNA1ds
296700041007     c                   eval      IA1ins  = 'S'
296800041007     c                   eval      IA1tor  = 'S'
296900041007     c                   eval      IA1ogg  = ds_OggSped
297000041007      *
297100041007     C                   ENDSR
297200990630      *****************************************************************
297300990630      * GESTIONE F14 = Giacenze
297400990630      *****************************************************************
297500990630     C     SUB_F14       BEGSR
297600990630      *
297700990901     c                   clear                   FNLG30DS
297800990901     C                   MOVEL     'S'           D30POA
297900990901     C                   MOVEL     tasAAS        D30AAS
298000990901     C                   MOVEL     tasLNP        D30LNP
298100990901     C                   MOVEL     tasNRS        D30NRS
298200990901     C                   MOVEL     tasNSP        D30NSP
298300990901     C                   MOVEL     FNLG30DS      KPJBU
298400990901     C                   SETON                                        31
298500990901     C                   CALL      'FNLG30C'
298600990901     C                   PARM                    KPJBA
298700990901      *
298800990901      *  ritorno alla videata di apertura
298900990901     c                   clear                   INFDSFMT
299000990701     c                   EVAL      *inkl = *on
299100990630      *
299200990630     c                   ENDSR
299300040416      *****************************************************************
299400040416      * GESTIONE F15 = Gestione Reclami
299500040416      *****************************************************************
299600040416     C     SUB_F15       BEGSR
299700040416      *
299800040416     c                   reset                   FIDNA2ds
299900040416     c                   eval      IA2ogg  = ds_OggSped
300000040416     c                   call      'FIDNA2R'
300100040416     c                   parm                    KPJBA
300200040416     c                   parm                    FIDNA2ds
300300040416      *
300400040416      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
300500040416     c                   if        OA2f03 = *on
300600040416     c                   eval      *inKC  = *on
300700040416     c                   else
300800040416      *  ritorno alla videata di apertura
300900040416     c                   clear                   InfDSfmt
301000040416     c                   eval      *inKL  = *on
301100040416     c                   endif
301200040416      *
301300040416     c                   ENDSR
301400990630      *****************************************************************
301500990630      * GESTIONE F16 = Competenze C/E
301600990630      *****************************************************************
301700990630     C     SUB_F16       BEGSR
301800990701      *
301900990701     c                   clear                   TNSB01DS
302000990701     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
302100990701     C                   MOVEL     'F16'         D01OP1                         *LIVELLO PROGRAMMA
302200990701     C                   MOVEL     '0'           D01F03                         *USCITA PGM
302300990701     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
302400990701     C                   MOVEL     '0'           D01ERR                         *ERRORE
302500990701     C                   Z-ADD     TASaas        D01AAS                         *ANNO
302600990701     C                   Z-ADD     taslnp        D01LNP                         *LNP
302700990701     C                   Z-ADD     tasnrs        D01NRS                         *SERIE
302800990701     C                   Z-ADD     tasnsp        D01NSP                        *SPEDIZIONE
302900990701     C                   MOVEL     tastbl        D01TBL                        *TIPO BOLLA
303000990701     C                   Z-ADD     V0Cdsp        D01DSP                        *DATA SPEDIZIONE
303100990701      *
303200990705     C                   MOVEL(P)  TNSB01DS      KPJBU
303300990701     C                   CALL      'TNSB08R'
303400990701     C                   PARM                    KPJBA
303500990701     C                   MOVEL     KPJBU         TNSB01DS
303600990701      *
303700990701      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
303800990707     C                   IF        D01F03 = '1'
303900990701     c                   eval      *inkc = *on
304000990707     c                   ELSE
304100990701     c                   clear                   INFDSFMT
304200990707     c                   eval      *inkl = *on
304300990707     C                   ENDIF
304400990701      *
304500990630     c                   ENDSR
304600000417      *****************************************************************
304700000417      * GESTIONE F17 = Richiesta rigenerazione bolle
304800000417      *****************************************************************
304900000417     C     SUB_F17       BEGSR
305000000417      *
305100000418     c                   clear                   tibs20ds
305200000418     C                   MOVEL     'W'           i20VID
305300000418     C                   Z-ADD     tasAAS        i20AAS
305400000418     C                   Z-ADD     tasLNP        i20LNP
305500000418     C                   Z-ADD     tasNRS        i20NRS
305600000418     C                   Z-ADD     tasNSP        i20NSP
305700000418     C                   Z-ADD     tasLNA        i20LNA
305800000418     C                   MOVEL(P)  tibs20ds      KPJBU
305900000417      *
306000000418     C                   CALL      'TIBS20R'
306100000418     C                   PARM                    KPJBA
306200000418     C                   MOVEL     KPJBU         tibs20ds
306300000418      *
306400000418      * Ritorno alla videata di apertura
306500000418     c                   clear                   INFDSFMT
306600000418     c                   eval      *inkl = *on
306700000418      *
306800000417     c                   ENDSR
306900990630      *****************************************************************
307000990630      * GESTIONE F18 = Legami
307100990630      *****************************************************************
307200990630     C     SUB_F18       BEGSR
307300030402
307400030402      * Per il primo richiamo imposto i campi del video
307500030402     c                   If        Wsb51d18 = *Blanks
307600030402     c                   ExSr      Ried18
307700030402     c                   Eval      Wsb51d18 = 'S'
307800030402     c                   EndIf
307900030402
308000030402      * Gestione subfile
308100030402     c                   If        NRR18 > 0
308200030402     c                   ExSr      Ried18_1
308300040518     c                   Else
308400040518     c                   Eval      *Inks = *Off
308500030402     c                   EndIf
308600990630      *
308700990630     c                   ENDSR
308800030402      *****************************************************************
308900030402      * IMPOSTO CAMPI VIDEO LEGAMI BOLLA
309000030402      *****************************************************************
309100030402     c     Ried18        BegSr
309200030402
309300030402      * Pulizia subfile
309400030402     c                   Eval      *In26 = *Off
309500030402     c                   Write     Sb51c18
309600030402     c                   Eval      *In25 = *On
309700030402     c                   Eval      *In26 = *On
309800030402     c                   Clear                   Nrr18
309900030402
310000030402      * Carico il subfile
310100030402      * uso la bolla come figlia o come mamma per cercare la bolla originale
310200030402     c     Keyspe        Chain     Fnlbl01l                           33
310300030402     c   33Keyspe        Chain     Fnlbl02l                           33
310400040517     c                   If        *In33
310500040517     c                   Eval      *Inks = *Off
310600040517     c                   EndIf
310700030402If  1c                   If        Not *In33
310800030402
310900030402      * con la bolla originale leggo la prima bolla della lista
311000031029     c**!!!              Eval      kLblAao = LblAao
311100031029     c**!!!              Eval      KlblLpo = LblLpo
311200031029     c**!!!              Eval      kLblNro = LblNro
311300031029     c**!!!              Eval      kLblNso = LblNso
311400031029     c**!!!keyspelbl     Chain     Fnlbl02l                           33
311500030402
311600030402      * Imposto subito nel sbfl la bolla originale
311700031029If  2c**!!!              If        Not *In33
311800030402     c                   Eval      v18Aap = LblAao
311900030402     c                   Eval      v18Lpp = LblLpo
312000030402     c                   Eval      v18Lap = LblLao
312100030402     c                   Eval      v18Nrp = LblNro
312200030402     c                   Eval      v18Nsp = LblNso
312300030402     c                   Clear                   v18Aan
312400030402     c                   Clear                   v18Lpn
312500030402     c                   Clear                   v18Lan
312600030402     c                   Clear                   v18Nrn
312700030402     c                   Clear                   v18Nsn
312800030402     c     'ORIGINAL'    CAT       'E':0         v18Pre
312900030402     c                   Clear                   v18Flg
313000030402     c                   Clear                   v18Npre
313100030402     c                   Clear                   v18Sce
313200030402     c                   Eval      *In53 = *Off
313300030402     c                   Eval      *In54 = *On
313400030402     c                   Eval      *In56 = *Off
313500030402
313600030402      * Controllo se la bolla trovata è quella richiesta dall'utente
313700030402If  3c                   If        LblAao = I51Aas and LblLpo = I51Lnp and
313800030402     c                             LblNro = I51Nrs and LblNso = I51Nsp
313900030402     c                   Eval      *In53 = *On
314000030402     c                   Eval      *In56 = *On
314100030402    3c                   EndIf
314200030402      * Controllo se abilitare o meno la scelta di visualizzazione
314300030402      * la bolla originale non è quella richiesta dall'utente
314400030402      * verifico se esiste in archivio
314500030403If  3c                   If        Not *In53
314600030402     c                   Eval      v18Flg = 'O'
314700030402     c                   Eval      kTasAas = LblAao
314800030402     c                   Eval      KTasLnp = LblLpo
314900030402     c                   Eval      kTasNrs = LblNro
315000030402     c                   Eval      kTasNsp = LblNso
315100030402     c     keyspe2       Setll     Titas30c
315200030402     c                   If        Not %Equal(Titas30c)
315300030402     c                   Eval      v18Npre = 'Non Presente'
315400030402     c                   Eval      *In56 = *On
315500030402     c                   EndIf
315600030403      * e non è la prima bolla richiesta
315700030403If  4c                   If        Not *In56 and
315800030403     c                             LblAao = SavAas and LblLpo = SavLnp and
315900030403     c                             LblNro = SavNrs and LblNso = SavNsp
316000030403     c                   Eval      *In56 = *On
316100030403    4c                   EndIf
316200030402    3c                   EndIf
316300030402
316400030402     c                   Eval      Nrr18 = 1
316500030402     c                   Write     Sb51s18
316600030402
316700030402     c                   Eval      *In54 = *Off
316800030402     c                   Eval      *In33 = *Off
316900031029    2c**!!!              EndIf
317000031029
317100031029      * con la bolla originale leggo la prima bolla della lista
317200031029     c                   Eval      kLblAao = LblAao
317300031029     c                   Eval      KlblLpo = LblLpo
317400031029     c                   Eval      kLblNro = LblNro
317500031029     c                   Eval      kLblNso = LblNso
317600031029     c     keyspelbl     Chain     Fnlbl02l                           33
317700030402      * Ciclo fino all'ultimo legame
317800030402      * lavorando solo sui campi "BOLLA SEGUENTE"
317900030402     c                   Clear                   wa
318000030402     c                   Clear                   wb
318100030402     c                   Clear                   Aan
318200030402Do  2c                   Dow       Not *In33
318300030402     c                   Eval      v18Aap = LblAap
318400030402     c                   Eval      v18Lpp = LblLpp
318500030402     c                   Eval      v18Lap = LblLap
318600030402     c                   Eval      v18Nrp = LblNrp
318700030402     c                   Eval      v18Nsp = LblNsp
318800030402     c                   Eval      v18Aan = LblAan
318900030402     c                   Eval      v18Lpn = LblLpn
319000030402     c                   Eval      v18Lan = LblLan
319100030402     c                   Eval      v18Nrn = LblNrn
319200030402     c                   Eval      v18Nsn = LblNsn
319300030402     c                   Clear                   v18pre
319400030402     c                   Clear                   v18Flg
319500030402     c                   Clear                   v18npre
319600030402     c                   Clear                   v18Sce
319700030402     c                   Eval      *In53 = *Off
319800030402     c                   Eval      *In55 = *Off
319900030402     c                   Eval      *In56 = *Off
320000030402
320100030402      * Controllo se la bolla precedente trovata è quella richiesta dall'utente
320200030402If  3c                   If        LblAap = I51Aas and LblLpp = I51Lnp and
320300030402     c                             LblNrp = I51Nrs and LblNsp = I51Nsp
320400030402     c                   Eval      *In53 = *On
320500030402     c                   Eval      v18Flg = 'P'
320600030402    3c                   EndIf
320700030402      * Controllo se la bolla seguente trovata è quella richiesta dall'utente
320800030402If  3c                   If        LblAan = I51Aas and LblLpn = I51Lnp and
320900030402     c                             LblNrn = I51Nrs and LblNsn = I51Nsp
321000030402     c                   Eval      *In55 = *On
321100030402     c                   Eval      v18Flg = 'S'
321200030403     c                   Eval      *In56 = *On
321300030402    3c                   EndIf
321400030402      * Controllo se abilitare o meno la scelta di visualizzazione
321500030403      * la bolla seguente non è quella che l'utente ha richiesto
321600030403If  3c                   If        Not *In55
321700030403     c                   Eval      kTasAas = LblAan
321800030403     c                   Eval      KTasLnp = LblLpn
321900030403     c                   Eval      kTasNrs = LblNrn
322000030403     c                   Eval      kTasNsp = LblNsn
322100030403      * verifico se esiste in archivio
322200030402     c     keyspe2       Setll     Titas30c
322300030402     c                   If        Not %Equal(Titas30c)
322400030402     c                   Eval      v18Npre = 'Non Presente'
322500030402     c                   Eval      *In56 = *On
322600030402     c                   EndIf
322700030403      * e se non è la prima bolla richiesta
322800030403If  4c                   If        Not *In56 and
322900030403     c                             LblAan = SavAas and LblLpn = SavLnp and
323000030403     c                             LblNrn = SavNrs and LblNsn = SavNsp
323100030403     c                   Eval      *In56 = *On
323200030403    4c                   EndIf
323300030402    3c                   EndIf
323400030402      * Scrivo il record nel sbfl
323500030402     c                   Add       1             Nrr18
323600030402     c                   Write     sb51s18
323700030402      * Aggiorno la schiera con le figlie
323800030402     c                   Add       1             wa                             INDICE SCHIERA
323900030402     c                   Eval      Aan(wa) = LblAan
324000030402     c                   Eval      Lpn(wa) = LblLpn
324100030402     c                   Eval      Nrn(wa) = LblNrn
324200030402     c                   Eval      Nsn(wa) = LblNsn
324300030402
324400030402     C     keyspelbl     Reade     Fnlbl02l                               33
324500030402
324600030402      * Se non trovo più bolle con la reade
324700030402      * utilizzo la figlia per leggere il legame successivo
324800030402Do  3c                   Dow       wb < wa  and %Eof(Fnlbl02l) = *On
324900030402     c                   Add       1             wb
325000030402     c                   Eval      kLblAao = Aan(wb)
325100030402     c                   Eval      kLblLpo = Lpn(wb)
325200030402     c                   Eval      kLblNro = Nrn(wb)
325300030402     c                   Eval      kLblNso = Nsn(wb)
325400030402     c     keyspelbl     Setll     Fnlbl02l
325500030402     c     keyspelbl     Reade     Fnlbl02l                               33
325600030402    3c                   EndDo
325700030402
325800030402    2c                   EndDo
325900030402    1C                   EndIf
326000030402
326100030402     c                   EndSr
326200030402      **********************************************************************
326300030402      * RICHIAMO PGM DI VISUALIZZAZIONE BOLLA LEGATA
326400030402      **********************************************************************
326500030402     c     Ried18_1      BegSr
326600030402
326700030402Do  1c                   Do        *Hival
326800030402     c                   Eval      Nrr18 = 1
326900030402     c                   Clear                   w001a18
327000030402      * Emetto Subfile
327100030402     c                   Write     Sb51t10
327200030402     c                   Write     Sb51z00
327300120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
327400030402     c                   Exfmt     Sb51c18
327500120302     c                   else
327600120302     c                   write     sb51c18
327700120302     c                   read(E)   tnsb51d
327800120302     c                   if        %error
327900120302     c                   eval      *inkc='1'
328000120302     c                   leave
328100120302     c                   endif
328200120302     c                   endif
328300030402      * Premuto un tasto funzionale
328400030402     c                   If         *In27 = *On
328500030402     c                   Leave
328600030402     c                   EndIf
328700030402      * Leggo il Subfile
328800030402     c                   Readc     Sb51s18
328900030402Do  2c                   Dow       %Eof = *Off
329000030402If  3c                   If        v18Sce = '5'
329100030402     c                   Eval      w001a18 = 'S'
329200030402      * passo il n. di spedizione da visualizzare
329300030403If  4c                   If        v18Flg = 'O'
329400030402     c                   Eval      I51Aas = v18Aap
329500030402     c                   Eval      I51Lnp = v18Lpp
329600030402     c                   Eval      I51Nrs = v18Nrp
329700030402     c                   Eval      I51Nsp = v18Nsp
329800030403   x4c                   Else
329900030402     c                   Eval      I51Aas = v18Aan
330000030402     c                   Eval      I51Lnp = v18Lpn
330100030402     c                   Eval      I51Nrs = v18Nrn
330200030402     c                   Eval      I51Nsp = v18Nsn
330300030402    4c                   EndIf
330400030402     c                   Clear                   v18Sce
330500030402     c                   Update    Sb51s18
330600030403     c                   Clear                   INFDSFMT
330700030402     c                   GoTo      Inizio
330800030402      * se premuto F3 esco dal programma
330900030402     c   kc              Leave
331000030402    3c                   EndIf
331100030402
331200030402     c                   Readc     Sb51s18
331300030402    2c                   EndDo
331400030402      * se non effettuate scelte forzo F12
331500030402     c                   If        w001a18 = *Blanks
331600030402     c                   Eval      *Inkl = *On
331700030402     c                   Clear                   INFDSFMT
331800030408     c                   Eval      w001a18 = 'S'
331900030402     c                   Leave
332000030402     c                   Endif
332100030402      *
332200030402    1c                   EndDo
332300030403
332400030418      * se premuto f12 ritorno alla prima bolla richiesta
332500030418     c                   If        *Inkl
332600030418     c                   Clear                   INFDSFMT
332700030418     c                   Eval      w001a18 = 'S'
332800030418     c                   EndIf
332900030402
333000030402     c                   EndSr
333100040119
333200040119      *****************************************************************
333300040119      * GESTIONE F19 = Immagine LdV
333400040119      *****************************************************************
333500040119     c     SUB_F19       BEGSR
333600040119      *
333700110927     c                   eval      c_tibs20=' '
333800110926     c                   select
333900040119      * Richiamo pgm. ANA670R per visualizzare la LdV
334000111129     c***                when      *inkt
334100170515     c                   when      *inkf and not *inkf
334200040119     c                   z-add     TASaas        Waas20
334300040119     c                   clear                   Parm670
334400040128     c                   movel(p)  'LDV'         Parm670(1)
334500040119     c                   movel(p)  ds_KEYsped    Parm670(2)
334600040119     c                   movel(p)  KNSif         Parm670(3)
334700040119      *
334800040331     ***c                   call      'ANA670R'
334900040331     c                   call      'YNA670R'
335000040119     c                   parm      '201'         Soc670
335100040122     c                   parm      'VIEW'        Cmd670
335200040122     c                   parm                    Parm670
335300040119     c                   parm      *off          Err670
335400040119     c                   parm      *blanks       Ret670
335500110927     c                   If        Ret670 = 'NULL '
335600110927     c                   eval      c_tibs20='S'
335700110927     c                   endif
335800111129     c****               when      *inkf
335900111129     c                   when      *inkt
336000161209     c                   z-add     TASaas        Waas20
336100161209     C                   CALL      'UBJDOC01R'
336200161209     C                   PARM                    ds_KEYsped
336300161209     C                   PARM      *zeros        oStato
336400161209     c                   clear                   w01des
336500161209     C                   SELECT
336600161209     C                   WHEN      oStato = *zeros
336700161209     c                   eval      c_tibs20='S'
336800161209     C                   WHEN      oStato = -1
336900161209     C                   EVAL      w01des = 'Errore generico'
337000161209     C                   WHEN      oStato = -2
337100161209     C                   EVAL      w01des = 'Errore: Tabelle mancanti'
337200161209     C                   WHEN      oStato = -3
337300161209     C                   EVAL      w01des = 'Errore in risposta da JDOC'
337400161209     C                   WHEN      oStato = -4
337500161209     C                   EVAL      w01des = 'Errore: POD non trovata'
337600161209     C                   WHEN      oStato = -5
337700161209     C                   EVAL      w01des = 'Errore visualizzazione su PC'
337800161209     C                   ENDSL
337900161209     c                   if        oStato<>*zeros
338000161209     c                   if        %SUBST(I51OP0:1:1)<>'M'
338100161209     c                   exfmt     sb51w01
338200161209     c                   else
338300161209     c                   write     sb51w01
338400161209     c                   read(E)   tnsb51d
338500161209     c                   if        %error
338600161209     c                   eval      *inkc='1'
338700161209     c                   leavesr
338800161209     c                   endif
338900161209     c                   endif
339000161209     c                   endif
339100161209
339200110926     c                   endsl
339300040511      * se la chiamata al pgm è andata a buon fine richiamo la rigenerazione bolla firmata
339400110927     c****               If        Ret670 = 'NULL '
339500110927     c                   if        c_tibs20='S'
339600040511     c                   Clear                   Tibs20ds
339700040511     c                   Eval      i20vid = 'S'
339800040511     c                   Eval      i20car = 'S'
339900040511     c                   Eval      i20aas = TasAas
340000040511     c                   Eval      i20lnp = TasLnp
340100040511     c                   Eval      i20nrs = TasNrs
340200040511     c                   Eval      i20nsp = TasNsp
340300040511     c                   Eval      i20lna = TasLna
340400040511     c                   Movel(p)  Tibs20ds      kpjbu
340500040511     c                   Call      'TIBS20R'
340600040511     c                   Parm                    kpjba
340700040511     c                   Eval      Tibs20ds = kpjbu
340800040511     c                   EndIf
340900040121      *
341000040128      *  ritorno alla videata di apertura
341100040128     c                   clear                   INFDSFMT
341200040121     c                   eval      *inKL = *on
341300040119      *
341400040119     c                   ENDSR
341500040119
341600990630      *****************************************************************
341700990630      * GESTIONE F20 = Affidabilità
341800990630      *****************************************************************
341900990630     C     SUB_F20       BEGSR
342000990701
342100990701      * Per il primo richiamo imposto i campi del video
342200990701     c                   IF        WSB51D20 = *blanks
342300990701     c                   exsr      ried12
342400990701     c                   eval      WSB51D20 = 'S'
342500990701     c                   ENDIF
342600990701
342700990701     c                   write     SB51T10
342800990701     c                   write     SB51Z00
342900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
343000990701     c                   exfmt     SB51D12
343100120302     c                   else
343200120302     c                   write     sb51d12
343300120302     c                   read(E)   tnsb51d
343400120302     c                   if        %error
343500120302     c                   eval      *inkc='1'
343600120302     c                   endif
343700120302     c                   endif
343800990701
343900990701     c                   ENDSR
344000990701      **********************************************************************
344100990701      * IMPOSTO CAMPI VIDEO AFFIDABILITA'
344200990701      **********************************************************************
344300990701     C     RIED12        BEGSR
344400091211     c                   setoff                                       5960
344500990701      *
344600990701      * Data ritiro
344700990701     c                   IF        TASdrt > *zeros
344800990701     c     *iso          move      TASdrt        DATA_eur
344900990701     c     *eur          move      DATA_eur      W0080
345000990701     C                   eval      V12drt = %EDITC(W0080:'Y')
345100990701     c                   ENDIF
345200990701      * Numero ritiro
345300990701     c                   z-add     tasnrt        v12nrt
345400990701      * Prestazione Autotrasportatore
345500990701     c                   SELECT
345600990701     c                   WHEN      TASfpp = 'M'
345700990701     C                   eval      V12fpp = 'MATTINO'
345800990701     c                   WHEN      TASfpp = 'P'
345900990701     C                   eval      V12fpp = 'POMERIGGIO'
346000990701     c                   ENDSL
346100990701      * Stop Autotrasportatore
346200990701     c                   z-add     tasstr        v12str
346300990701      * Prestazione Autotrasportatore
346400990701     c                   z-add     taspdr        v12pdr
346500021203     C                   movel     tasprd        kpdpdr
346600021203     C                   move      'A'           kpdtip
346700021203     C     kfapd         chain     fiapd01L
346800000727     C                   if        %found
346900000727     C                   movel     apdrsc        v12rs1
347000000727     C                   else
347100000727     C                   clear                   v12rs1
347200000727     C                   endif
347300990701      * Data Immissione Bolla
347400990701     c                   IF        TASdim > *zeros
347500990701     c     *iso          move      TASdim        DATA_eur
347600990701     c     *eur          move      DATA_eur      W0080
347700990701     C                   eval      V12dim = %EDITC(W0080:'Y')
347800990701     c                   ENDIF
347900990701      * Data Borderò
348000990701     c                   IF        TASdbr > *zeros
348100990701     c     *iso          move      TASdbr        DATA_eur
348200990701     c     *eur          move      DATA_eur      W0080
348300990701     C                   eval      V12dbr = %EDITC(W0080:'Y')
348400990701     c                   ENDIF
348500990701      * Data Partenza Ultimo Collo
348600990701     c                   IF        TASduc > *zeros
348700990701     c     *iso          move      TASduc        DATA_eur
348800990701     c     *eur          move      DATA_eur      W0080
348900990701     C                   eval      V12duc = %EDITC(W0080:'Y')
349000990701     c                   ENDIF
349100091210      * Data Arrivo collo al terminal di arrivo--> ARBDAM
349200990701     c                   IF        TASdti > *zeros
349300990701     c     *iso          move      TASdti        DATA_eur
349400990701     c     *eur          move      DATA_eur      W0080
349500990701     C                   eval      V12dti = %EDITC(W0080:'Y')
349600990701     c                   ENDIF
349700090402      * Ora Arrivo Ultimo Collo . Es non la inviamo più in ttitas
349800090402     c****               IF        TAShti > *zeros
349900090402     c****               eval      *in42 = *on
350000090402     c****               z-add     TAShti        V12hti
350100090402     c****               ENDIF
350200091210     c
350300091210      * Data Arrivo all filiale di arrivo finale
350400091210      *           da Fiar500f record 'GEN'
350500091210     c                   Clear                   dAr5Gen
350600091210     c                   Eval      war5Trd = 'GEN'
350700091210     c     kSpeAr5       Chain     Fiar531c
350800091210If  1c                   If        %Found(Fiar531c)
350900091210     c                   Movel     Ar5Uni        Dar5Gen
351000091210     c                   IF        §ar5arrdt> *zeros
351100091210     c                   movel     §ar5arrdt     w0080
351200091210     c     *iso          move      w0080         DATA_eur
351300091210     c     *eur          move      DATA_eur      W0080
351400091210     C                   eval      V12arred = %EDITC(W0080:'Y')
351500091210     c* Ora arrivo reale
351600091210     c                   IF        §ar5arrhm> *zeros
351700091211     c                   eval      *in60 = *on
351800091210     c                   movel     §ar5arrhm     v12arreh
351900091210     c                   ENDIF
352000091210     c
352100091210     c                   ENDIF
352200091210     c                   ENDIF
352300091210     c
352400090402      * Data manifest per bolla export
352500090402     c                   clear                   v12dmf
352600090402     c                   if        lnantw='FED'  or lnantw='EEX' or
352700090402     c                             lnantw='DPD'
352800090402     c                   eval      ktrc = 'K'
352900090402     c     KTAStrc       CHAIN     TITA430C
353000090402     c                   If        %FOUND
353100090402     c                   movel     TA4not        dta4K
353200090402     c                   if        §ta4kdmf>0
353300090402     c     *iso          move      §ta4kdmf      DATA_eur
353400090402     c     *eur          move      DATA_eur      W0080
353500090402     C                   eval      V12dmf = %EDITC(W0080:'Y')
353600090402     c                   seton                                        59
353700090402     c                   ENDIF
353800090402     c                   ENDIF
353900090402     c
354000090402     c* ES: Dal 13 aprile 09 visulizzo data anche se vuota
354100090402     c                   if        not *in59  and tasdbr>20090413
354200090402     c                   seton                                        59
354300090402     c                   ENDIF
354400090402     c                   ENDIF
354500990701      * Consegne particolari
354600990701     c                   IF        TASFTC <> *BLANKS
354700990701     c                   movel     TASftc        V12ftc
354800990701     C                   eval      KCOD = '1P'
354900990701     C                   movel(P)  tasftc        KKEY
355000990701     C                   EXSR      CHKTAB
355100990701     c                   if        *in30 = *off
355200990702     C                   movel     Wtbluni       V12dtc
355300990701     c                   endif
355400990701     c                   ENDIF
355500990701      *
355600990701     c                   IF        TAStc2 <> *BLANKS
355700990701     c                   movel     TAStc2        V12tc2
355800990701     C                   eval      KCOD = '1P'
355900990701     C                   movel(P)  tastc2        KKEY
356000990701     C                   EXSR      CHKTAB
356100990701     c                   if        *in30 = *off
356200990701     C                   movel     Wtbluni       V12dc2
356300990701     c                   endif
356400990701     c                   ENDIF
356500990701      * Giorni chiusura
356600990701     c                   movel     TASgc1        V12gc1
356700990701     c                   movel     TASgc2        V12gc2
356800990701      * Data Consegna Teorica
356900990705     c                   IF        TASdce > *zeros
357000990705     c     *iso          move      TASdce        DATA_eur
357100990701     c     *eur          move      DATA_eur      W0080
357200990705     C                   eval      V12dce = %EDITC(W0080:'Y')
357300990701     c                   ENDIF
357400990705      * Ora Consegna Teorica
357500990705     c                   IF        TAShce > *zeros
357600990705     c                   eval      *in44 = *on
357700990705     c                   z-add     TAShce        V12hce
357800990705     c                   ENDIF
357900990702      * Tipo Data Consegna Richiesta
358000051129     c                   clear                   v12tcr
358100051129     c                   clear                   v12dcr
358200051129     c                   clear                   v12hcr
358300051129     c                   if        tasdcr>0 or tashcr>0
358400051129     c                   clear                   fnlv80ds
358500051129     c                   eval      ilv80tla='L'
358600051129     c                   eval      ilv80aas=tasaas
358700051129     c                   eval      ilv80lnp=taslnp
358800051129     c                   eval      ilv80nrs=tasnrs
358900051129     c                   eval      ilv80nsp=tasnsp
359000051129     c                   eval      ilv80mgs=tasmgs
359100051129     c                   eval      ilv80tc1=tasftc
359200051129     c                   eval      ilv80tc2=tastc2
359300051129     c                   eval      ilv80dcr=tasdcr
359400051129     c                   eval      ilv80hcr=tashcr
359500051129     c                   eval      ilv80tcr=tastcr
359600051129     c                   call      'FNLV80R'
359700051129     c                   parm                    kpjba
359800051129     c                   parm                    fnlv80ds
359900051129     c
360000051129     c                   eval      v12tcr=olv80tcrd
360100990702      * Data Consegna Richiesta
360200990702     c                   IF        TASdcr > *zeros
360300051129     C                   eval      V12dcr = %EDITC(olv80dcr8:'Y')
360400990702     c                   ENDIF
360500990702      * Ora Consegna Richiesta
360600990705     c                   IF        TAShcr > *zeros
360700990705     c                   eval      *in43 = *on
360800990705     c                   z-add     TAShcr        V12hcr
360900990705     c                   ENDIF
361000051129     c                   ENDIF
361100990702      * Stop consegna
361200990702     c                   z-add     tasstp        V12stp
361300990702      * Autotrasportatore
361400990702     c                   z-add     taspdc        V12pdc
361500021203     C                   movel     taspdc        kpdpdr
361600021203     C                   move      'A'           kpdtip
361700021203     C     kfapd         chain     fiapd01L
361800000727     C                   if        %found
361900000727     C                   movel     apdrsc        v12rs2
362000000727     C                   else
362100000727     C                   clear                   v12rs2
362200000727     C                   endif
362300990702      * Affidabilità
362400990702     c                   z-add     tasnpi        V12npi
362500990702     c                   z-add     tasnci        V12nci
362600990702     c                   z-add     tasnrc        V12nrc
362700010220      *
362800990630     c                   ENDSR
362900990630      *****************************************************************
363000990630      * GESTIONE F21 = Dettaglio Colli
363100990630      *****************************************************************
363200990630     C     SUB_F21       BEGSR
363300990705
363400990705      * Per il primo richiamo imposto i campi del video
363500990705     c                   IF        WSB51D21 = *blanks
363600990705     c                   exsr      ried21
363700990705     c                   eval      WSB51D21 = 'S'
363800990705     c                   ENDIF
363900990705
364000990705     c                   z-add     1             nrr21
364100990705
364200990705     C                   WRITE     SB51T10
364300990706     C                   WRITE     SB51Z00
364400120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
364500990705     C                   EXFMT     SB51C21
364600120302     c                   else
364700120302     c                   write     sb51c21
364800120302     c                   read(E)   tnsb51d
364900120302     c                   if        %error
365000120302     c                   eval      *inkc='1'
365100120302     c                   endif
365200120302     c                   endif
365300990705
365400990705     c                   ENDSR
365500180126      *****************************************************************
365600180126      * IMPOSTO CAMPI VIDEO DETTAGLIO COLLI
365700180126      *****************************************************************
365800180126     C     RIED21        BEGSR
365900180126
366000180126      *
366100180126      * Pulizia subfile
366200180126     C                   eval      *in26 = *off
366300180126     C                   WRITE     SB51C21
366400180126     C                   eval      *in25 = *on
366500180126     C                   eval      *in26 = *on
366600180126     C                   clear                   nrr21
366700180126      *
366800180126     C     KEYspe        SETLL     TITAT30C
366900180126     C     KEYspe        READE     TITAT30C
367000180126      *
367100180126     C                   DOW       %EOF = *off
367200180126     c                   z-add     tatfls        v21fls
367300180126     c                   z-add     tatnsc        v21nsc
367400180126     c                   If        TATdcm > *zeros
367500180126     c     *iso          move      TATdcm        DATA_eur
367600180126     c     *eur          move      DATA_eur      W0080
367700180126     C                   eval      V21dcm = %EDITC(W0080:'Y')
367800180126     c                   Else
367900180126     c                   movel     V0Cdcm        v21dcm
368000180126     c                   Endif
368100180126      *
368200180126     c                   ADD       1             nrr21
368300180126     c                   write     SB51s21
368400180126      *
368500180126     C     KEYspe        READE     TITAT30C
368600180126     c                   ENDDO
368700180126
368800180126     c                   ENDSR
368900180126      *****************************************************************
369000180126      * GESTIONE    Dettaglio Colli incompatibili
369100180126      *****************************************************************
369200180129     C     SUB_INC       BEGSR
369300180126
369400180126      * Per il primo richiamo imposto i campi del video
369500180126     c                   IF        WSB51D24 = *blanks
369600180126     c                   exsr      ried24
369700180126     c                   eval      WSB51D24 = 'S'
369800180126     c                   ENDIF
369900180126
370000180126     c                   z-add     1             nrr24
370100180126
370200180126     C                   WRITE     SB51T10
370300180126     C                   WRITE     SB51Z00
370400180126     c                   if        %SUBST(I51OP0:1:1)<>'M'
370500180126     C                   EXFMT     SB51C24
370600180126     c                   else
370700180126     c                   write     sb51c24
370800180126     c                   read(E)   tnsb51d
370900180126     c                   if        %error
371000180126     c                   eval      *inkc='1'
371100180126     c                   endif
371200180126     c                   endif
371300180126
371400180126     c                   ENDSR
371500990705      *****************************************************************
371600180126      * IMPOSTO CAMPI VIDEO COLLI incompatibili
371700990705      *****************************************************************
371800180126     C     RIED24        BEGSR
371900990630      *
372000990705      * Pulizia subfile
372100990705     C                   eval      *in26 = *off
372200180126     C                   WRITE     SB51C24
372300990705     C                   eval      *in25 = *on
372400990705     C                   eval      *in26 = *on
372500180126     C                   clear                   nrr24
372600990705      *
372700180126     c                   eval      wtahtrc = 'J'
372800180126     C     KEYspeh       SETLL     TITAh30C
372900180126     C     KEYspeh       READE     TITAh30C
373000990705      *
373100990705     C                   DOW       %EOF = *off
373200180129     c                   z-add     tahnsc        v24nsc
373300180129     c                   eval      darsj=tahnot
373400180129     c                   eval      v24pest= §ARSJPEST
373500180129     c                   eval      v24npgt= §ARSJNPGT
373600180129     c                   eval      v24npst= §ARSJNPST
373700180129     c                   if        §ARSJTIPO='S'
373800180129     c                   eval      v24dest='Da anomalia "S"'
373900180129     c                   else
374000180129     c                   eval      v24dest='Da foglio "INC"'
374100180129     c                   endif
374200180129     c
374300180129     c                   If        §ARSJDFVT>*zeros
374400180129     c     *iso          move      §ARSJDFVT     DATA_eur
374500180129     c     *eur          move      DATA_eur      v24dfvt
374600990705     c                   Else
374700180129     c                   clear                   v24dfvt
374800990705     c                   Endif
374900990630      *
375000180129     c                   ADD       1             nrr24
375100180129     c                   write     SB51s24
375200990705      *
375300180129     C     KEYspeh       READE     TITAh30C
375400990705     c                   ENDDO
375500990705
375600990630     c                   ENDSR
375700990630      *****************************************************************
375800990630      * GESTIONE F22 = Contrassegno
375900990630      *****************************************************************
376000990630     C     SUB_F22       BEGSR
376100990630      *
376200990705     C                   clear                   tnsc40ds
376300990705     C                   Z-ADD     1             D40INT
376400990705     C                   Z-ADD     tasAAS        D40AAS
376500990705     C                   Z-ADD     tasLNP        D40LNP
376600990705     C                   Z-ADD     tasNRS        D40NRS
376700990705     C                   Z-ADD     tasNSP        D40NSP
376800990705     C                   MOVEL(P)  TNSC40DS      KPJBU
376900990705      *
377000990705     C                   CALL      'TNSC43R'
377100990705     C                   PARM                    KPJBA
377200990706      *  ritorno alla videata di apertura
377300990706     c                   clear                   INFDSFMT
377400990706     c                   eval      *inkl = *on
377500990706      *
377600990630     c                   ENDSR
377700020226      *****************************************************************
377800020226      * GESTIONE F23 = AWB FedEx
377900020226      *****************************************************************
378000020226     C     SUB_F23       BEGSR
378100020226      *
378200020226
378300020226      * Per il primo richiamo imposto i campi del video
378400020226     c                   IF        WSB51D23 = *blanks
378500020226     c                   exsr      ried23
378600020226     c                   eval      WSB51D23 = 'S'
378700020226     c                   ENDIF
378800020226
378900020226     c                   z-add     1             nrr23
379000020226
379100020226     C                   WRITE     SB51T10
379200020226     C                   WRITE     SB51Z00
379300120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
379400020226     C                   EXFMT     SB51C23
379500120302     c                   else
379600120302     c                   write     sb51c23
379700120302     c                   read(E)   tnsb51d
379800120302     c                   if        %error
379900120302     c                   eval      *inkc='1'
380000120302     c                   endif
380100120302     c                   endif
380200020226      *
380300020226     c                   ENDSR
380400020226      *****************************************************************
380500020226      * IMPOSTO CAMPI VIDEO AWB FEDEX
380600020226      *****************************************************************
380700020226     C     RIED23        BEGSR
380800020226
380900020226      *
381000020226      * Pulizia subfile
381100020226     C                   eval      *in26 = *off
381200020226     C                   WRITE     SB51C23
381300020226     C                   eval      *in25 = *on
381400020226     C                   eval      *in26 = *on
381500020226     C                   clear                   nrr23
381600020226      *
381700020226     c                   eval      wtahtrc = 'F'
381800020226     C     KEYspeh       SETLL     TITAH30C
381900020226     C     KEYspeh       READE     TITAH30C
382000020226      *
382100020226     C                   DOW       %EOF = *off
382200020226     c                   move      tahnot        V23not
382300020226     c                   z-add     tahnsc        v23nsc
382400020226     C                   move      '7H'          kcod
382500020226     C                   movel(p)  tahtrc        kkey
382600020226     C     KEYTAB        CHAIN     TABEL00F                           30
382700020226     c                   IF        *IN30 = *ON
382800020226     C                   else
382900020226     C                   ENDIF
383000020226      *
383100020226     c                   ADD       1             nrr23
383200020226     c                   write     SB51s23
383300020226      *
383400020226     C     KEYspeh       READE     TITAH30C
383500020226     c                   ENDDO
383600020226
383700020226     c                   ENDSR
383800020226      *****************************************************************
383900020226      * GESTIONE F24 = Altri dati
384000020226      *****************************************************************
384100020226     C     SUB_F24       BEGSR
384200020226
384300030121     c                   Eval      *In24 = *Off
384400030121     c                   Clear                   Vtf
384500020226     c                   clear                   vtf01
384600020226     c                   clear                   vtf02
384700020226     c                   clear                   vtf03
384800020226     c                   clear                   vtf04
384900020226     c                   clear                   vtf05
385000030121     c                   Clear                   Vtf06
385100020226
385200030121      * prima emissione tasti funzione
385300030121  1b c                   if        wsb51z00 = 1
385400030121
385500030121     c                   Clear                   xx
385600030121     c                   Do        5             yy
385700030121     c                   Add       1             xx
385800030121     c     '1'           Lookup    SkOnOff(xx)                            32
385900030121     c                   If        *In32
386000030121     c                   Eval      Vtf(yy) = Tf(xx)
386100170517     c                   if        %scan('F6=':tf(xx))>0
386200170517     c                   eval      %subst(vtf(yy):1:1)=ri
386300170517     c                   eval      %subst(vtf(yy):11:1)=atrnorm
386400170517     c                   endif
386500030121     c                   Else
386600030121     c                   Leave
386700030121     c                   EndIf
386800030121     c                   EndDo
386900030121
387000030121     c                   Eval      Vtf01 = vtf(01)
387100030121     c                   Eval      Vtf02 = vtf(02)
387200030121     c                   Eval      Vtf03 = vtf(03)
387300030121     c                   Eval      Vtf04 = vtf(04)
387400030121     c                   Eval      Vtf05 = vtf(05)
387500030121
387600030121     c                   If        wOnOff > 5
387700030121     c                   Eval      Vtf06 = Cf2412
387800030121     c                   If        wOnOff > 10
387900030121     c                   Eval      Vtf06 = Cf2413
388000030121     c                   EndIf
388100030121     c                   Eval      *In24 = *On
388200030121     c                   Eval      wsb51z00 = 2
388300030121     c                   Goto      EndF24
388400030121     c                   EndIf
388500030121
388600030121     c                   EndIf
388700020226
388800030121      * seconda emissione tasti funzione
388900030121  1b c                   if        wsb51z00 = 2
389000030121
389100030121     c                   Do        5             yy
389200030121     c                   Add       1             xx
389300030121     c     '1'           Lookup    SkOnOff(xx)                            32
389400030121     c                   If        *In32
389500030121     c                   Eval      Vtf(yy) = Tf(xx)
389600170517     c                   if        %scan('F6=':tf(xx))>0
389700170517     c                   eval      %subst(vtf(yy):1:1)=ri
389800170517     c                   eval      %subst(vtf(yy):11:1)=atrnorm
389900170517     c                   endif
390000030121     c                   Else
390100030121     c                   Leave
390200030121     c                   EndIf
390300030121     c                   EndDo
390400030121
390500030121     c                   Eval      Vtf01 = vtf(01)
390600030121     c                   Eval      Vtf02 = vtf(02)
390700030121     c                   Eval      Vtf03 = vtf(03)
390800030121     c                   Eval      Vtf04 = vtf(04)
390900030121     c                   Eval      Vtf05 = vtf(05)
391000030121
391100030121     c                   If        wOnOff > 10
391200030121     c                   Eval      *In24 = *On
391300030121     c                   Eval      Vtf06 = Cf2423
391400030121     c                   Eval      wsb51z00 = 3
391500030121     c                   Goto      EndF24
391600030121     c                   EndIf
391700030121     c                   If        wOnOff > 5
391800030121     c                   Eval      *In24 = *On
391900030121     c                   Eval      Vtf06 = Cf2422
392000030121     c                   Eval      wsb51z00 = 1
392100030121     c                   Goto      EndF24
392200030121     c                   EndIf
392300030121
392400030121     c                   EndIf
392500030121
392600030121      * terza emissione tasti funzione
392700030121  1b c                   if        wsb51z00 = 3
392800030121
392900030121     c                   Do        5             yy
393000030121     c                   Add       1             xx
393100030121     c     '1'           Lookup    SkOnOff(xx)                            32
393200030121     c                   If        *In32
393300030121     c                   Eval      Vtf(yy) = Tf(xx)
393400170517     c                   if        %scan('F6=':tf(xx))>0
393500170517     c                   eval      %subst(vtf(yy):1:1)=ri
393600170517     c                   eval      %subst(vtf(yy):11:1)=atrnorm
393700170517     c                   endif
393800030121     c                   Else
393900030121     c                   Leave
394000030121     c                   EndIf
394100030121     c                   EndDo
394200030121
394300030121     c                   Eval      Vtf01 = vtf(01)
394400030121     c                   Eval      Vtf02 = vtf(02)
394500030121     c                   Eval      Vtf03 = vtf(03)
394600030121     c                   Eval      Vtf04 = vtf(04)
394700030121     c                   Eval      Vtf05 = vtf(05)
394800030121
394900030121     c                   Eval      *In24 = *On
395000030121     c                   Eval      Vtf06 = Cf2433
395100030121     c                   Eval      wsb51z00 = 1
395200030121     c                   Goto      EndF24
395300030121
395400030121     c                   EndIf
395500030121
395600030121     c     EndF24        ENDSR
395700990813      *****************************************************************
395800991021      * DETERMINO SE SPEDIZIONE CON FATTURA IMMEDIATA
395900990813      *****************************************************************
396000991021     C     FATIMMED      BEGSR
396100990813      *
396200170726     C***                eval      KCOD = 'QC'
396300170726     C***                movel(P)  '2'           KKEY
396400170726     C****               EXSR      CHKTAB
396500990813      *
396600170726     c****               if        *in30 = *off
396700170726     C****               MOVEL     TBLUNI        DSQC2
396800990813      *
396900170726     c****               IF        TASfiv <> §QCfiv  and  TASfiv <> §QCfi2  and
397000170726     c****                         TASfiv <> §QCfi3  and  TASfiv <> §QCfi4  and
397100170726     c****                         TASfiv <> §QCfi5  and  TASfiv <> §QCfi6
397200170726     c                   if        tasfiv < 900
397300991021     c                   eval        Wfatimmed = 'S'
397400991021     c                   ENDIF
397500990813      *
397600170726     c****               endif
397700990813      *
397800990813     C                   ENDSR
397900990621      *****************************************************************
398000990621      *  AGGANCIO ANAGRAFICHE
398100990621      *****************************************************************
398200990621     C     CTR_ANAGRA    BEGSR
398300990621      *
398400990629     C                   clear                   DS_cnaco
398500990629     C                   clear                   DS_cnind
398600990629     C                   clear                   DS_cnclp
398700990629     C                   clear                   DS_fncls
398800990629      *
398900990621     C                   CALL      'TIBS69R'
399000990621     C                   PARM                    tibs69DS
399100990621     C                   PARM                    DS_cnaco
399200990621     C                   PARM                    DS_cnind
399300990621     C                   PARM                    DS_cnclp
399400990621     C                   PARM                    DS_fncls
399500990621      *
399600990621     C                   eval      wtibs69r = 'S'
399700990621      * per il 1° giro è inizializzata nelle specifiche "D"
399800990621     C                   clear                   TIBS69DS
399900990621      *
400000990621     C                   ENDSR
400100990628      **********************************************************************
400200990629      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
400300990628      **********************************************************************
400400990628     C     EDTIMPORTI    BEGSR
400500990628
400600990628      * -----------------------------------------------------------------------------------
400700990629      * W0133      contiene il numero da editare per il video
400800990629      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il numero ha decimali
400900990629      * IMPEDITATO ritorna il valore editato per il video lungo 13 (troncate cifre oltre 10 mld)
401000990628      * -----------------------------------------------------------------------------------
401100990628
401200990628     c                   clear                   IMPeditato
401300990629     c                   clear                   W017A
401400990628
401500990628     c                   if        Wtstdec = 'S'
401600990629     C                   eval      W017a = %EDITC(W0133:'2')
401700990628     c                   else
401800990629     c                   z-add     W0133         W0130
401900990629     C                   eval      W017a = %EDITC(W0130:'2')
402000990628     c                   endif
402100990629
402200990629     c                   move      W017a         IMPeditato
402300990628
402400990628     c                   ENDSR
402500990628      **********************************************************************
402600990628      * AGGANCIO FILE ORGANIGRAMMA
402700990628      **********************************************************************
402800990628     C     CHKORG        BEGSR
402900990628      *
403000990630     C                   clear                   ORGDES
403100990630     C     Wlinea        CHAIN     AZORG                              30
403200990628      *
403300990628     c                   ENDSR
403400990628      **********************************************************************
403500990628      * AGGANCIO FILE TABELLE
403600990628      **********************************************************************
403700990628     C     CHKTAB        BEGSR
403800990628      *
403900990628     C                   clear                   Wtbluni
404000990628      *
404100990630     C     KEYTAB        CHAIN     TABEL00F                           30
404200990630     c                   IF        *IN30 = *ON  or  TBLflg <> *blanks
404300990628     C                   eval         *in30 = *on
404400990628     C                   else
404500990628     C                   movel     TBLUNI        WTBLUNI
404600990628     C                   ENDIF
404700990628      *
404800990628     c                   ENDSR
404900030703      *-------------------------------------------------------------------------
405000030703      *  Decoditifa tipo bancali
405100030703      *-------------------------------------------------------------------------
405200030703     c     Sr_DecTba     BegSr
405300030703
405400030703     c                   Clear                   Tibs02Ds
405500030703     c                   Clear                   dTba
405600030703     c                   Movel     'C'           T02Mod
405700030703     c                   Movel     Knsif         T02Sif
405800030703     c                   Movel     'TBA'         T02Cod
405900030703     c                   Movel(p)  wtba          T02Ke1
406000030703     c                   Call      'TIBS02R'
406100030703     c                   Parm                    Kpjba
406200030703     c                   Parm                    Tibs02Ds
406300030703     c                   If        T02Err = *Blanks
406400030703     c                   Movel     T02Uni        dTba
406500030703     c                   EndIf
406600030703     c                   Eval      wdestba = §TbaDesc
406700030703
406800030703     c                   EndSr
406900030418      *****************************************************************
407000030418      * Pulisco tutti i record del file video
407100030418      *****************************************************************
407200030418     c     Sr_Clear      BegSr
407300030418
407400030418     c                   Clear                   wsb51d01
407500030418     c                   Clear                   wsb51d02
407600030418     c                   Clear                   wsb51d07
407700030418     c                   Clear                   wsb51d08
407800030418     c                   Clear                   wsb51d13
407900030418     c                   Clear                   wsb51d18
408000030418     c                   Clear                   wsb51d20
408100030418     c                   Clear                   wsb51d21
408200030418     c                   Clear                   wsb51d23
408300180126     c                   Clear                   wsb51d24
408400030418
408500030418     c                   Clear                   Sb51D01
408600030418     c                   Clear                   Sb51D07
408700030418     c                   Clear                   Sb51D12
408800030418
408900030418     c                   Clear                   Sb51C08
409000040603     c                   Clear                   Sb51C09
409100030418     c                   Clear                   Sb51C11
409200030418     c                   Clear                   Sb51C13
409300030418     c                   Clear                   Sb51C18
409400030418     c                   Clear                   Sb51C21
409500030418     c                   Clear                   Sb51C23
409600030418
409700030418     c                   Eval      *In26 = *Off
409800030418     c                   Write     Sb51C08
409900030418     c                   Eval      *In25 = *On
410000030418     c                   Eval      *In26 = *On
410100030418     c                   Clear                   nrr08
410200030418
410300030418     c                   Eval      *In26 = *Off
410400030418     c                   Write     Sb51C11
410500030418     c                   Eval      *In25 = *On
410600030418     c                   Eval      *In26 = *On
410700030418     c                   Clear                   nrr11
410800030418
410900030418     c                   Eval      *In26 = *Off
411000030418     c                   Write     Sb51C13
411100030418     c                   Eval      *In25 = *On
411200030418     c                   Eval      *In26 = *On
411300030418     c                   Clear                   nrr13
411400030418
411500030418     c                   Eval      *In26 = *Off
411600030418     c                   Write     Sb51C18
411700030418     c                   Eval      *In25 = *On
411800030418     c                   Eval      *In26 = *On
411900030418     c                   Clear                   nrr18
412000030418
412100030418     c                   Eval      *In26 = *Off
412200030418     c                   Write     Sb51C21
412300030418     c                   Eval      *In25 = *On
412400030418     c                   Eval      *In26 = *On
412500030418     c                   Clear                   nrr21
412600030418
412700030418     c                   Eval      *In26 = *Off
412800030418     c                   Write     Sb51C23
412900030418     c                   Eval      *In25 = *On
413000030418     c                   Eval      *In26 = *On
413100030418     c                   Clear                   nrr23
413200040512
413300040512     c                   Eval      wsb51z00 = 1
413400030418
413500030418     c                   EndSr
413600120711      *****************************************************************
413700120712      * Window Interrogazione Restituzione documenti al mitt.
413800120711      *****************************************************************
413900120711     c     Sr_w02        BegSr
414000121022
414100121022      * se non ho errore non visualizzo la riga
414200121022     c                   eval      *in69 = *off
414300121022     c                   IF        W02mtvdt <> *blanks
414400121022     c                   eval      *in69 = *on
414500121022     c                   ENDIF
414600120711     c*
414700120711     c                   if        %SUBST(I51OP0:1:1)<>'M'
414800120711     c                   exfmt     sb51w02
414900120711     c                   else
415000120712     c                   write     sb51w02
415100120711     c                   read(E)   tnsb51d
415200120711     c                   if        %error
415300120712     c                   leavesr
415400120711     c                   endif
415500120711     c                   endif
415600120711     c                   endsr
415700120829      *****************************************************************
415800120829      * Reperimento data scannerizzazione della figlia
415900120829      *****************************************************************
416000120829     c     Replbl        BegSr
416100120829     c                   eval      wtrovl=*off
416200120829     c                   eval      LBLAAN=I51AAS
416300120829     c                   eval      LBLLPN=I51LNP
416400120829     c                   eval      LBLNRN=I51NRS
416500120829     c                   eval      LBLNSN=I51NSP
416600120829     c     keylblN       chain     fnlbl02l
416700120830     c                   dow       %found(fnlbl02l)
416800120829     c
416900120829     c                   Eval      war5Trd = 'DOC'
417000120829     c     kspeAr5l      Chain     Fiar531c
417100120829If  1c                   If        %Found(Fiar531c)
417200120829     c                   Movel     Ar5Uni        Dar5doc
417300120829    4c                   if        §ar5jdrid >*zeros
417400120829     c                   eval      wtrovl=*on
417500120830     c                   leave
417600120829     c                   endif
417700120829     c                   endif
417800120829     c     keylblN       chain     fnlbl02l
417900120829     c                   enddo
418000120829     c                   endsr
418100160621
418200160621      /free
418300160621       //--------------------------------------------------------------
418400160621       //?Richiamo CL per comando visualizza fattura.
418500160621       //--------------------------------------------------------------
418600160621       BEGSR VisFattura;
418700160621
418800160621         clear pcmdpc;
418900160621         ppause = '*NO';
419000160621         clear puid;
419100160621         pnfat = *all'0';
419200160621         %subst(pnfat:4:6) = %editc(TASnft:'X');
419300160621
419400170726       //  IF  TASfiv = 996 or TASfiv = 998;
419500170726         IF  TASfiv >= 900;
419600160621           puid = 'FATA' + '201' + %subst(%editc(TASdft:'X'):1:4) +
419700160621                  '2' + %editc(TASfiv:'X') + pnfat;
419800160621         ELSE;
419900160621           puid = 'FAT9' + '201' + %subst(%editc(TASdft:'X'):1:4) +
420000160621                  '2' + %editc(TASfiv:'X') + pnfat;
420100160621         ENDIF;
420200160621
420300160621
420400160621         pcmdpc = '"start ' + comandpc + puid + '"';
420500160621
420600160621         tnsb51c1 (pcmdpc:ppause);
420700160621
420800160621       ENDSR;
420900160621
421000160621      /end-free
421100990618      *****************************************************************
421200990618      * ROUTINE INIZIALE
421300990618      *****************************************************************
421400990618     C     *INZSR        BEGSR
421500990618      *
421600990618     C     *ENTRY        PLIST
421700990618     C                   PARM                    KPJBA
421800040416     c                   parm                    FIDNA1ds
421900990623      *
422000990628     c                   movel     KPJBU         TNSB51DS
422100030402
422200030402      * Mi salvo la bolla iniziale richiesta dall'utente
422300030402     c                   Eval      SavAas = I51Aas
422400030402     c                   Eval      SavLnp = I51Lnp
422500030402     c                   Eval      SavNrs = I51Nrs
422600030402     c                   Eval      SavNsp = I51Nsp
422700030402     c                   Eval      SavTbl = I51Tbl
422800020516
422900020516      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
423000020516     C     *dtaara       define    §azute        azuteds
423100020516     C     *dtaara       define    §datiute      ddatiute
423200020516
423300020516     C                   clear                   AzUteDs
423400020516     C                   clear                   DDatiUte
423500020516     C                   clear                   Tibs34Ds
423600020516     C                   in(E)     *dtaara
423700020516if  1C                   IF        %Error  or  RSUT = *blanks
423800020516     C                   call      'TIBS34R'
423900020516     C                   parm                    Tibs34Ds
424000020516     C                   in        *dtaara
424100020516e   1C                   ENDIF
424200990618
424300990618      * reperisco data e ora
424400990618     C                   TIME                    W0140
424500990618      * UDATE IN GGMMAAAA
424600990618     C                   MOVE      W0140         WDTGIO
424700990618      * UDATE IN AAAAMMGG
424800990805     C     *eur          MOVEL     WDTGIO        DATA_eur
424900990805     C     *iso          MOVEL     DATA_eur      dateu
425000110927     c* accendo in06 se utente edp (mi serve per l'abilitazione temporanea del tasto funz F6
425100110927     c* per l'interrogazione immagine a modo nuovo
425200111129     c****               if        %subst(knmus:1:3)= 'EDP'
425300111223     c****               seton                                        06
425400111129     c****               else
425500111129     c****               setoff                                       06
425600111129     c****               endif
425700110927     c
425800990618
425900990628     C     keyspe        KLIST
426000990628     C                   KFLD                    I51AAS
426100990628     C                   KFLD                    I51LNP
426200990628     C                   KFLD                    I51NRS
426300990628     C                   KFLD                    I51NSP
426400990618      *
426500990628     C     keyspe1       KLIST
426600990628     C                   KFLD                    I51AAS
426700990628     C                   KFLD                    I51LNP
426800990628     C                   KFLD                    I51NRS
426900990628     C                   KFLD                    I51NSP
427000990628     C                   KFLD                    I51TBL
427100030402
427200030402     c     keyspe2       Klist
427300030402     c                   Kfld                    kTasAas
427400030402     c                   Kfld                    kTasLnp
427500030402     c                   Kfld                    kTasNrs
427600030402     c                   Kfld                    kTasNsp
427700020225
427800020225     C     keyspeh       KLIST
427900020225     C                   KFLD                    I51AAS
428000020225     C                   KFLD                    I51LNP
428100020225     C                   KFLD                    I51NRS
428200020225     C                   KFLD                    I51NSP
428300020225     c                   kfld                    wtahtrc
428400030120
428500030120     c     kSpeAr5       Klist
428600030120     c                   Kfld                    I51AAS
428700030120     c                   Kfld                    I51LNP
428800030120     c                   Kfld                    I51NRS
428900030120     c                   Kfld                    I51NSP
429000030121     c                   kfld                    wAr5Trd
429100120829      *
429200120829     c     kSpeAr5l      Klist
429300120829     c                   Kfld                    LBLAAN
429400120829     c                   Kfld                    LBLLPN
429500120829     c                   Kfld                    LBLNRN
429600120829     c                   Kfld                    LBLNSN
429700120829     c                   kfld                    wAr5Trd
429800120829      *
429900120829     c     keylblN       Klist
430000120829     c                   Kfld                    LBLAAN
430100120829     c                   Kfld                    LBLLPN
430200120829     c                   Kfld                    LBLNRN
430300120829     c                   Kfld                    LBLNSN
430400030402
430500030402     c     keyspelbl     Klist
430600030402     c                   Kfld                    kLblAao
430700030402     c                   Kfld                    kLblLpo
430800030402     c                   Kfld                    kLblNro
430900030402     c                   Kfld                    kLblNso
431000990622      *
431100990630     C     keytab        KLIST
431200020516     C                   KFLD                    KKUT
431300990630     C                   KFLD                    KCOD
431400990630     C                   KFLD                    KKEY
431500990622      *
431600990629     C     ktastrc       KLIST
431700990629     C                   KFLD                    tasAAS
431800990629     C                   KFLD                    tasLNP
431900990629     C                   KFLD                    tasNRS
432000990629     C                   KFLD                    tasNSP
432100990629     C                   KFLD                    Ktrc
432200070524      *
432300070524     C     ktitai        KLIST
432400070524     C                   KFLD                    tasAAS
432500070524     C                   KFLD                    tasLNP
432600070524     C                   KFLD                    tasNRS
432700070524     C                   KFLD                    tasNSP
432800070524     C                   KFLD                    tasTBL
432900070524     C                   KFLD                    Ksvt                           Sigla varia tassata
433000020419
433100020419     c     kfnorm        klist
433200020419     c                   kfld                    kormpoe
433300020419     c                   kfld                    kormnsr
433400020419     c                   kfld                    kormnor
433500020419     c                   kfld                    kormnrv
433600021203
433700021203     C     Kfapd         klist
433800021203     C                   kfld                    kpdtip
433900021203     C                   kfld                    kpdpdr
434000040416      *
434100160404      * File FITGD32L
434200040416     c     K02TGD02      klist
434300040416     c                   kfld                    TGDtor
434400040416     c                   kfld                    TGDogg
434500040809
434600040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
434700040809     C     KDCT1_C       KLIST
434800040809     C                   KFLD                    dsaac
434900040809     C                   KFLD                    dsfil
435000040809     C                   KFLD                    dsnca
435100120330     C     Ktbe          KLIST
435200120330     C                   KFLD                    kcode
435300120330     C                   KFLD                    kke1
435400120330     C                   KFLD                    kke2
435500990623      *
435600990618     C                   ENDSR
435700990618      *---------------------------------------------------------------------------------------------
435800990618** MSG  Lungh. 78                                                            *
435900990623TNSB50R- Opzione non valida                                                     1
436000990623TNSB50R- Codice cliente errato                                                  2
436100990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
436200990623TNSB50R- Impostate selezioni incongruenti                                       4
436300990623TNSB50R- Data fattura errata                                                    5
436400990623TNSB50R- Immettere la linea                                                     6
436500990623TNSB50R- Linea inesistente o annullata                                          7
436600990623TNSB50R- Non richiedere la data fattura e le spedizioni non fatturate           8
436700990623TNSB50R- Immettere l'anno spedizione                                            9
436800990623TNSB50R- Non richiedere il numero fattura se non si è immessa la data fattura   10
436900990623TNSB50R- Non richiedere data fattura se non si è immesso il cod. fatturazione   11
437000990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
437100990623TNSB50R- Data spedizione errata                                                 13
437200990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
437300990623TNSB50R- Immettere almeno una selezione valida                                  15
437400990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
437500990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
437600990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
437700990623TNSB50R- Bolla esistente ma annullata                                           19
437800990623TNSB50R- Opzione non valida                                                     20
437900990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
438000990624TNSB50R- Non trovata la spedizione richiesta                                    22
438100120709TNSB50R- Bolla non trovata: e' stato modificato il tipo bolla                   23
438200020225** TF   Lungh. 11                                                            *
438300041018F15=Ric.Ass                                                                     1
438400170519 F6=Manut.                                                                      2
438500170515F5=Eventi                                                                       3
438600170515F22=C/Ass.                                                                      4
438700170515F2=C.A.                                                                         5
438800170515F14=Giac.                                                                       6
438900170515F10=Variaz.                                                                     7
439000170515F18=Legami                                                                      8
439100170515F11=NoteS/B                                                                     9
439200170515F4=ORM                                                                          10
439300170515F8=CHI SONO                                                                     11
439400170515F21=D.Colli                                                                     12
439500170515F23=AWB Fed                                                                     13
439600170515F1=Agg.Rich                                                                     14
