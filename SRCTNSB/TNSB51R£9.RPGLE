000100980618      *-------------------------------------------------------------------------*
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
001400120711      * 07 -
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
008100990618      ***
008200990618      * 90 - riemissione videata
008300990618      ****************************************************************
008400990618
008500990628     H DECEDIT('0,') DATEDIT(*DMY/)
008600990618
008700990705     FTNSB51D   CF   E             WORKSTN  infds(INFDS51D)
008800120302     F                                      maxdev(*file)
008900040603     f                                      sfile(sb51s09:nrr09)
009000990705     F                                      sfile(SB51S11:NRR11)
009100990705     F                                      sfile(SB51S13:NRR13)
009200990705     F                                      sfile(SB51S21:NRR21)
009300000901     F                                      sfile(SB51S08:NRR08)
009400020226     f                                      sfile(sb51s23:nrr23)
009500030402     f                                      sfile(sb51s18:nrr18)
009600990623     FTITAS30C  IF   E           K DISK
009700990628     FTABEL00F  IF   E           K DISK
009800990629     FTITA430C  IF   E           K DISK
009900990622     FTITAA30C  IF   E           K DISK
010000990630     FTITAT30C  IF   E           K DISK
010100990702     FTITA730C  IF   E           K DISK
010200000901     FTITAH30C  IF   E           K DISK
010300170118     F*TITASV1L  IF   E           K DISK     prefix(V)
010301161006     Ffiarbf2c  IF   E           K DISK
010400990630     FTNCSB03L  IF   E           K DISK
010500110428     F*TNBLA01L  IF   E           K DISK
010600010312     FECCET30C  IF   E           K DISK    usropn
010700040809     F**""*FNDCT02L  IF   E           K DISK
010800040809     FFNDCT01L  IF   E           K DISK
010900990705     FFNDFA01L  IF   E           K DISK
011000990813     FFNEVB01L  IF   E           K DISK
011100050401     FTigcp51L  IF   E           K DISK
011200990618     fAZORG01L  IF   E           K DISK
011300021203     Ffiapd01l  if   e           k disk
011400020419     ffnorm01l  if   e           k disk    usropn
011500040603     fFiar531c  if   e           k Disk
011600030402     fFnlbl01l  if   e           k Disk
011700030402     fFnlbl02l  if   e           k Disk    Rename(fnlbl000:fnlbl02)
011800160404     fFITGD32L  if   e           k disk
011900070524     fTitai01L  if   e           k disk                                         Immagine bolla tass
012000120404     ftntbe01l  if   e           k disk    USROPN extfile(Wtntbe)
012100990618      *------------------------------------------------------------------------*
012200020516
012300020516     D KKUT            S                   LIKE(TBLkut) inz(1)
012400990630     D KCOD            S                   LIKE(TBLcod)
012500990630     D KKEY            S                   LIKE(TBLkey)
012600990630     D KTRC            S                   LIKE(TAAtrc)
012700990630     D Wtbluni         S                   LIKE(TBLuni)
012800990628     D Wlinea          S                   LIKE(TASlnp)
012900990628     D Wtstdec         S                   LIKE(§CVfdc)
013000990813     D Wtasdcp         S                   LIKE(TASDCP)
013100010404     D Wta4not         S                   LIKE(TA4NOT)
013200020225     d wtahtrc         s                   like(tahtrc)
013300030120     d wAr5Trd         s                   Like(Ar5Trd)
013400030703     d wtba            s                   Like(§Ar5Tba)
013500030703     d wdestba         s                   Like(V0dTba)
013600020419
013700020419     d kormpoe         s                   like(ormpoe)
013800020419     d kormnsr         s                   like(ormnsr)
013900020419     d kormnor         s                   like(ormnor)
014000020419     d kormnrv         s                   like(ormnrv)
014100021203     d kpdpdr          S                   LIKE(APDpdr)
014200021203     d kpdtip          S                   LIKE(APDtip)
014300030402     d SavAas          s                   Like(I51Aas)
014400030402     d SavLnp          s                   Like(I51Lnp)
014500030402     d SavNrs          s                   Like(I51Nrs)
014600030402     d SavNsp          s                   Like(I51Nsp)
014700030402     d SavTbl          s                   Like(I51Tbl)
014800121217     d waas_leg        s                   Like(I51Aas)
014900121217     d wlnp_leg        s                   Like(I51Lnp)
015000121217     d wnrs_leg        s                   Like(I51Nrs)
015100121217     d wnsp_leg        s                   Like(I51Nsp)
015200030402     d kLblAao         s                   Like(LblAao)
015300030402     d kLblLpo         s                   Like(LblLpo)
015400030402     d kLblNro         s                   Like(LblNro)
015500030402     d kLblNso         s                   Like(LblNso)
015600030402     d kTasAas         s                   Like(TasAas)
015700030402     d kTasLnp         s                   Like(TasLnp)
015800030402     d kTasNrs         s                   Like(TasNrs)
015900030402     d kTasNsp         s                   Like(TasNsp)
016000070524     d ksvt            s                   Like(TaiSvt)
016100120330     D KCODE           S                   LIKE(TBecod)
016200120330     D Kke1            S                   LIKE(TBeke1)
016300120330     D Kke2            S                   LIKE(TBeke2)
016400990628
016500000309     D callricors      s              1
016600000309     D wtibs69r        s              1
016700990707     D wsb51d01        s              1
016800990705     D wsb51d02        s              1
016900990705     D wsb51d07        s              1
017000990702     D wsb51d13        s              1
017100990702     D wsb51d20        s              1
017200990705     D wsb51d21        s              1
017300000901     D wsb51d08        s              1
017400030121     d wsb51z00        s              1  0 Inz(1)
017500020226     d wsb51d23        s              1
017600030402     d wsb51d18        s              1
017700040603     d wsb51d09        s              1
017800990706     D wunaca          s              1
017900990702     D W001A           s              1
018000030402     d w001a18         s              1
018100121217     d w001aleg        s              1
018200990702     D W017A           s             17
018300990629     D W020A           s             20
018400990628     D IMPeditato      s             13
018500991021     D  wfatimmed      s              1
018600020516     d wPOa            s              3    inz
018700020516     d wKSCa           S              7    inz
018800020516     d wabi            s                   like(UTEaut)  inz
018900030120     d wIn             s              1
019000030121     d wOnOff          s              2  0
019100030121     d yy              s              2  0
019200030121     d wTara           s                   Like(§QtTpc)
019300030121     d wNtara          s                   Like(TasPkf)
019400990628
019500990618     D xx              S              3  0
019600990628     D W0080           S              8  0
019700990629     D w0100           s             10  0
019800990629     D w0130           s             13  0
019900990629     D w0133           s             13  3
020000990630     D w0140           s             14  0
020100990618     D Wdtgio          S              8  0
020200990618     D dateu           S              8  0
020300990705     D nrr11           S              4  0
020400990705     D nrr13           S              4  0
020500990705     D nrr21           S              4  0
020600000901     D nrr08           S              4  0
020700020226     d nrr23           s              4  0
020800030402     d nrr18           s              4  0
020900040603     d nrr09           S              4  0
021000990628     D Wksc4           S              4  0
021100990628     D Wccm4           S              4  0
021200021119     d LnaNtw          s                   Like(§ogNtw)
021300030402     d wa              s              3  0
021400030402     d wb              s              3  0
021500040604     d woktrs          s              1    inz(*Off)
021600120829     d wtrovl          s              1    inz(*Off)
021601161209     D oStato          S              5I 0
021700990618
021800990618     D DATA_eur        S               D   DATFMT(*eur)
021900120330     D DATAiso         S               D   DATFMT(*iso)
022000120330     D DATAdmy         S               D   DATFMT(*dmy)
022100110927
022200110927     d c_tibs20        s              1
022600040119
022700040119      * Parametri per chiamata a Laguna
022800040122     d Soc670          s              3    inz('201')
022900040119     d Cmd670          s             15    inz('VIEW           ')
023000040119A0976d Parm670         s             44    inz   dim(30)
023100040119     d Err670          s              1    inz
023200040119     d Ret670          s             32A   inz
023300990618      *
023400990618      *   S C H I E R E
023500120709     D MSG             S             78    DIM(23) CTDATA PERRCD(1)             MSG VIDEO
023600120711     d tf              s             11    dim(13) ctdata perrcd(1)             tasti funzione
023700030121     d SkInd           s              2  0 Dim(15)
023800030121     d SkOnOff         s              1    Dim(15)
023900030121     d Vtf             s             11    Dim(5)
024000040119      * P.O. gestibili dall'utente
024100170109     D*$POG            s              3    dim(250) inz(*zeros)
024200030402
024300030402     d Aan             S              4  0 DIM(30)                              ANNO BOLLA SEGUENTE
024400030402     d Lpn             S              3  0 DIM(30)                              LNP BOLLA SEGUENTE
024500030402     d Nrn             S              2  0 DIM(30)                              SERIE BOLLA SEGUENTE
024600030402     d Nsn             S              7  0 DIM(30)                              SPEDIZ. BOLLA SEGUEN
024700990618      *
024800990618      *   D S   I N T E R N E / E S T E R N E
024900990618      *
025000990630     D INFDS51D        DS
025100990630     D  INFDSFMT             261    270
025200990618      *
025300010221     D* VISUALIZZAZIONE ORM - FIOR07R
025400010221     D Parm01          DS
025500010221     D  ppoe                          3  0
025600010221     D  pnor                          7  0
025700010221     D  pnsr                          2  0
025800010221     D  pnrv                          2  0
025900010221     D  psce                          1
026000010221     D  pfgs                          3  0
026100010221     D  ppor                          3  0
026200010221     D  pdtr                          8  0
026300010221     D  pmdb                         10
026400010221     D  pprb                         10
026500010221     D  pdts                          8  0
026600010221     D  prmp                          1
026700010221     D  pbrc                          1
026800010221     D  pref                          2
026900010221     D  pmio                          1
027000990813      *  DS per richiamo PGM visualizzazione legami bolla
027100990813     d LRB5DS          DS
027200990813     d  pb5aas                14     17  0
027300990813     d  pb5lnp                18     20  0
027400990813     d  pb5nrs                21     22  0
027500990813     d  pb5nsp                23     29  0
027600990813     d  pb5f03                30     30
027700990813     d  pb5flg                31     31
027800990813     d  pb5pgm                32     41
027900990813     d  pb5dsa                42     61
028000990813     d  pb5dsp                63     70  0
028100990813     d  pb5rmn                71     85  0
028200990813     d  pb5lna                86     88  0
028300990813     d  pb5lnad               89    108
028400990813     d  pb5rsd               109    143
028500990813     d  pb5dcp               144    151  0
028600990813     d  pb5tmc               152    152
028601160930      *  DS per richiamo PGM interrogazione sotorico variazioni unico
028602160930     d LR75DS          DS
028603160930     d  p75aas                14     17  0
028604160930     d  p75lnp                18     20  0
028605160930     d  p75nrs                21     22  0
028606160930     d  p75nsp                23     29  0
028607160930     d  p75f03                30     30
028608160930     D  P75FLG                31     31
028609160930     D  P75PGM                32     41
028610160930     D  p75DSA                42     61
028611160930     D  P75TBO               148    148
028612160930     d  p75tbl               149    150
028700040119      *
028800040416     d ds_Spedizione   ds
028900040119     d   Waas20                       2  0
029000040119     d   TASlnp
029100040119     d   TASnrs
029200040119     d   TASnsp
029300040416     d   TASaas
029400040416     d  ds_KEYsped             1     14
029500040416     d  ds_OggSped             3     18
029600990701      *
029700990623     D KPJBA         E DS
029800040416     d FIDNA1ds      e ds
029900040416      *
030000040416     d FIDNA2ds      e ds                  inz
030100041011     d*****************************************  IA2mod       e                     inz('I')
030200040416     d  IA2tor       e                     inz('S')
030300040416      *
030400000418     D TIBS20DS      E DS                  INZ
030500990701     D TNSB01DS      E DS                  INZ
030600990701     D TNSB51DS      E DS                  INZ
030700990707     D TNSC40DS      E DS
030800990707     D FIDN00DS      E DS
030900990714     D FNLV16DS      E DS                  INZ
031000051129     D FNLV80DS      E DS                  INZ
031100990729     D DTASV         E DS                  INZ   prefix(§)
031200990707     D DS7PQRS       E DS
031300990621     d TIBS69DS      E DS                  INZ
031400990618     d DS_cnaco      E DS                  extname(CNACO00F)
031500990618     d DS_cnind      E DS                  extname(CNIND00F)
031600990618     d DS_cnclp      E DS                  extname(CNCLP00F)
031700990618     d DS_fncls      E DS                  extname(FNCLS00F)
031800990901     D Fnlg30ds      E DS
031900990813     D DSQC2         E DS
032000990714     D DSTB          E DS
032100990629     D DSCV          E DS
032200990630     D WDSCV         E DS                  extname(DSCV)  prefix(W)
032300990629     D DTA4A         E DS
032400090402     D DTA4k         E DS
032500070927     D DSBL4E        E DS
032600070927     D*DSBL4F        E DS
032700060518     D DSBL4i        E DS
032800010220     D DSBL4M        E DS
032900990702     D DS$2          E DS
033000020516     D TIBS02DS      E DS                  inz
033100020516     D TIBS34DS      E DS
033200020516     D DDATIUTE      E DS
033300020516     D AZUTEDS       E DS                  extname(AZUTE00F)
033400020516     D DLAT          E DS                  inz
033500020516     D DUTE01        E DS                  inz
033600170109     D*TRUL31DS      e ds                  inz
033700040119     d dTASflo       e ds                  inz
033800020516      *
033900020328     D DSCC          E DS
034000021119     d Og143         e ds
034100030121
034200030121     d Fnlrb3Ds      e ds
034300030121     d dAr5Ban       e ds
034400030703     d dAr5Bnb       e ds
034500031119     d dAr5Gen       e ds
034600120711     d dAr5doc       e ds
034700040603     d dAr5Trs       e ds
034800151016     d dAr5Fat       e ds                  qualified   inz
034900030121     d dTba          e ds
035000030121     d dsQt1         e ds
035100030624     d ds3a          e ds
035200061108     d dta4q         e ds
035300090605     d ds5e          e ds
035400040809
035500040809      * ds per il controllo della presenza di CA per la spedizione richiesta
035600040809     d FIDN12DS      e ds
035700040809     d  skKey                 26    305    dim(20)
035800040809     d  skAnn                306    325    dim(20)
035900040809     d  skDca                326    485  0 dim(20)
036000040809     d  skFca                486    545  0 dim(20)
036100040809     d  skDch                546    705  0 dim(20)
036200040809     d  skCch                706    745    dim(20)
036300151016
036400151020      *?Visualizzazione Pesi Usati in Tassazione?
036500151020     d TNSB56ds      e ds                  inz
036501161229
036502170109     d* Controllo abilitazioni utente
036503170109     d tntaa1ds      e ds
036600160621
036700160621     d pcmdpc          s            128a   inz
036800160621     d ppause          s              4a   inz
036900160621     d puid            s             24a   inz
037000160621     d pnfat           s              9a   inz
037100151016
037200040809     d*
037300040809     d dsKey           ds
037400040809     d  dsaac                         4  0
037500040809     d  dsfil                         3  0
037600040809     d  dsnca                         7  0
037700020226
037800990618     D                SDS
037900990628     D  VTCPGM                 1     10
038000040416     d  SDSprm           *parms
038100990618
038200030120      * Costanti
038300030120     D CF2413          C                   CONST('F24=AlTasti(1/3)')
038400030120     D CF2423          C                   CONST('F24=AlTasti(2/3)')
038500030120     D CF2433          C                   CONST('F24=AlTasti(3/3)')
038600030120     D CF2412          C                   CONST('F24=AlTasti(1/2)')
038700030120     D CF2422          C                   CONST('F24=AlTasti(2/2)')
038800120404
038900120404     d Wtntbe          s             21    inz('FILTRAGRU /TNTBE01L')
039000160621
039100160621     d comandpc        c                   const('https://ecm.brt.it/b/b.asp?Pa-
039200160621     d                                     rams=SYN_INVO_UNIQUEID=')
039300160621
039400160621      //---------------------------------------------------------------
039500160621      //?Definizione procedure usate.
039600160621      //---------------------------------------------------------------
039700160621      // - Interrogazione fattura Docflow
039800160621     d TNSB51C1        pr                  extpgm('TNSB51C1')
039900160621     d  pcmdpc                      128
040000160621     d  ppause                        4
040100030120
040200990618      *------------------------------------------------------------------------*
040300990618      *---------------  C A L C O L O  ----------------------------------------*
040400990618      *------------------------------------------------------------------------*
040500030402
040600030402     c     Inizio        Tag
040700030402      * Provengo da interrogazione legami bolla
040800121217     c                   If        w001a18 = 'S'  or w001aleg='S'
040900030402     c     Keyspe        Setll     titas30c
041000030402     c     Keyspe        Reade     Titas30c
041100030402     c                   Eval      i51Tbl = tastbl
041200030408     c                   Eval      *In57 = *On
041300030418      * devo pulire tutte le videate
041400030418     c                   Exsr      Sr_Clear
041500030402     c                   EndIf
041600990618
041700990628     c     KEYSPE1       CHAIN     TITAS30C                           30
041800120709     c                   if        not %found(titas30c)
041900120709     c     KEYSPE        CHAIN     TITAS30C                           30
042000120709     c                   if        %found(titas30c)
042100120709     c                   eval      o51err='1'
042200120709     c                   eval      o51msg=msg(23)
042300120709     c                   endif
042400120709     c                   else
042500120709     c******             IF        %FOUND
042600990630
042700991117      * Verifico profilo per abilitazioni
042800991117     c                   EXSR      ABIPROFILO
042900991117
043000991117      * Abilito tasti funzionali
043100991117     c                   EXSR      ABITASFUN
043200990628
043300990630     C                   DO        *HIVAL
043400990630
043500990630      * Gestione tasti funzionali
043600990630     c                   SELECT
043700990630      * F3 = Fine - ESCO DAL PGM
043800990630     c                   WHEN      *INKC = *ON
043900990630     c                   eval      O51F03 = '1'
044000990630     c                   leave
044100030403      * F12 = Ritorno - se premuto non dalla prima bolla
044200030418      *                 ricarico la prima bolla
044300121217     c                   WHEN      *INKL=*ON and (w001a18 = 'S' or w001aleg='S')
044400121217     c                             and INFDSFMT = 'SB51D01'
044500030403     c                   Eval      I51Aas = SavAas
044600030403     c                   Eval      I51Lnp = SavLnp
044700030403     c                   Eval      I51Nrs = SavNrs
044800030403     c                   Eval      I51Nsp = SavNsp
044900030403     c                   Eval      I51Tbl = SavTbl
045000030403     c                   Clear                   w001a18
045100121217     c                   Clear                   w001aleg
045200030403     c                   Eval      *Inkl = *Off
045300030408     c                   Eval      *In57 = *Off
045400030418      * devo pulire tutte le videate
045500030418     c                   Exsr      Sr_Clear
045600030403     c                   GoTo      Inizio
045700990630      * F12 = Ritorno - se premuto nella videata di entrata ESCO DAL PGM
045800990630     c                   WHEN      *INKL = *ON  and  INFDSFMT = 'SB51D01'
045900990630     c                   eval      O51F12 = '1'
046000990630     c                   leave
046100990630      * F12 = Ritorno - se non premuto nella videata di entrata ritorno alla videata di entrata
046200990630     c                   WHEN      *INKL = *ON
046300990630     c                   exsr      SUB_FMT001
046400041007      *?F01 = Aggiunta Richiesta Assistenza clienti
046500041007     c                   WHEN      *inKA = *on
046600041007     c                   exsr      Sub_F01
046700041007     c                   leave
046800990630      * F2 = C.A.
046900990630     c                   WHEN      *INKB = *ON
047000990630     c                   exsr      SUB_F02
047100121121     c                   IF        *in28
047200121121     c                   eval      *inkb = '0'
047300121121     c                   ENDIF
047400010221      * F4 = ORM
047500010221     c                   WHEN      *INKD = *ON
047600010221     c                   exsr      SUB_F04
047700990630      * F5 = Eventi
047800990630     c                   WHEN      *INKE = *ON
047900990630     c                   exsr      SUB_F05
048000990630      * F7 = Altri Dati
048100990630     c                   WHEN      *INKG = *ON
048200990630     c                   exsr      SUB_F07
048300000901      * F8 = Cod."CHI SONO"
048400000901     c                   WHEN      *INKH = *ON
048500000901     c                   exsr      SUB_F08
048600040603      * F9 = Transiti
048700040603     c                   WHEN      *INKI = *ON
048800040603     c                   exsr      SUB_F09
048900990630      * F10 = Variazioni
049000990630     c                   WHEN      *INKJ = *ON
049100990630     c                   exsr      SUB_F10
049200030120      * F11 = Note Supermercato
049300030120     c                   WHEN      *INKK = *ON
049400030120     c                   exsr      SUB_F11
049500990630      * F13 = Tassazione
049600990630     c                   WHEN      *INKM = *ON
049700990630     c                   exsr      SUB_F13
049800990630      * F14 = Giacenze
049900990630     c                   WHEN      *INKN = *ON
050000990630     c                   exsr      SUB_F14
050100040416      *?F15 = Gestione reclami?
050200040416     c                   WHEN      *inKP = *on
050300040416     c                   exsr      Sub_F15
050400990630      * F16 = Competenze Conto Economico
050500990630     c                   WHEN      *INKQ = *ON
050600990630     c                   exsr      SUB_F16
050700000417      * F17 = Richiesta rigenerazione spedizione
050800000417     c                   WHEN      *INKR = *ON
050900000417     c                   exsr      SUB_F17
051000990630      * F18 = Legami
051100990630     c                   WHEN      *INKS = *ON
051200990630     c                   exsr      SUB_F18
051300040119      * F19 = Immagine LdV
051400110926     c                   WHEN      *INKT = *ON  or
051500111129     c****                         (*inkf=*on and %subst(knmus:1:3)= 'EDP')
051600111129     c                             *inkf
051700040119     c                   exsr      SUB_F19
051800990630      * F20 = Affidabilità
051900990630     c                   WHEN      *INKU = *ON
052000990630     c                   exsr      SUB_F20
052100990630      * F21 = Dettaglio Colli
052200990630     c                   WHEN      *INKV = *ON
052300990630     c                   exsr      SUB_F21
052400990630      * F22 = Contrassegni
052500990630     c                   WHEN      *INKW = *ON
052600990630     c                   exsr      SUB_F22
052700020225      * F23 = AWB FedEx
052800020225     c                   WHEN      *INKX = *ON
052900020225     c                   exsr      SUB_F23
053000020225      * F24 = Altri Tasti
053100020225     c                   WHEN      *INKY = *ON
053200020225     c                   exsr      SUB_F24
053300020226     c                   eval      *inky = *off
053400121217     c* ? su campo per visualizzazione legame bolla originale/di recupero variaz.
053500121217     c                   when      *in71 and v1cleg='?'
053600121217     c                   clear                   v1cleg
053700121217     c                   Eval      I51Aas = waas_leg
053800121217     c                   Eval      I51Lnp = wlnp_leg
053900121217     c                   Eval      I51Nrs = wnrs_leg
054000121217     c                   Eval      I51Nsp = wnsp_leg
054100121217     c                   Eval      w001aleg = 'S'
054200121217     c                   clear                   infdsfmt
054300121217     c                   goto      inizio
054400151016      *?? per visualizzare i pesi usati in tassazione?
054500151016     c                   when      *in70 and V1PesTas = '?'
054600151016     c                   clear                   V1PesTas
054700151020     c                   clear                   TNSB56ds
054800151020     c                   eval      D56aas = TASaas
054900151020     c                   eval      D56lnp = TASlnp
055000151020     c                   eval      D56nrs = TASnrs
055100151020     c                   eval      D56nsp = TASnsp
055200151020     c                   eval      D56tbl = TAStbl
055300151022     c                   eval      D56ksc = TASksc
055400151022     c*//                eval      D56rsc = TAS...
055500151022     c                   eval      D56ctr = TASctr
055600151022     c*//                eval      D56dtr = §TRdes
055700151020     c                   eval      KPJBU  = TNSB56ds
055800151020     c                   call      'TNSB56R'
055900151020     c                   parm                    KPJBA
056000151020     c                   eval      TNSB56ds = KPJBU
056100151016     c                   clear                   InfDsFmt
056200151016     c                   goto      Inizio
056300151016     c*//                eval      *inKL = *on
056400990630      * Emissione Videata di entrata
056500990630     c                   OTHER
056600990630     c                   exsr      SUB_FMT001
056700990630     c                   ENDSL
056800990618
056900990630     c                   ENDDO
057000990630     c                   ENDIF
057100990618
057200990618      * Chiudo pgm aperti
057300990618     C                   IF        Wtibs69r  <>  *BLANKS
057400990618     C                   eval      I69TLA  = 'C'
057500990618     C                   call      'TIBS69R'
057600990618     C                   parm                    TIBS69DS
057700990624     c                   ENDIF
057800040810     C*
057900040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
058000040810     c                   clear                   fidn12ds
058100040810     c                   eval      i12tla = 'C'
058200040810     c                   call      'FIDN12R'
058300040810     c                   parm                    fidn12ds
058301170109
058302170109     C                   clear                   tntaa1ds
058303170109     C                   movel     'C'           Itaa1Tla
058304170109     C                   movel(p)  tntaa1ds      kpjbu
058305170109     C                   CALL      'TNTAA1C'
058306170109     C                   PARM                    KPJBA
058900040810     c*
059000990618
059100990624      * Imposto campi per uscita con F03
059200990630     c                   MOVEL(P)  TNSB51DS      KPJBU
059300990630
059400990618     c                   EVAL      *INLR = *ON
059500991117      **********************************************************************
059600991117      * VERIFICO PROFILO PER ABILITAZIONI
059700991117      **********************************************************************
059800991117     C     ABIPROFILO    BEGSR
059900991117
060000020516     c                   eval      *in35  = *off
060100020516     c                   clear                   wABI
060200020516     c                   clear                   DLAT
060300020516
060400020516      *-- Verifica errori e autorità profilo
060500020516sel 1C                   SELECT
060600020516      *-- controllo se ho errori nei dati utente
060700020516      *--   nel qual caso NON risulta un profilo abilitato
060800020516w   1C                   WHEN      DUTERR = 'E'
060900020516     C                   eval      *IN35  = *on
061000020516
061100020516      *-- CONTROLLO AUTORITA'
061200020516      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
061300020516      *-- se il chiamante non richiede autorità specifica verificare
061400020516      *--   quella del profilo
061500020516      *-- se il chiamante richiede autorità specifica verificarla,
061600020516      *--  se è blank verificare quella del profilo
061700020516
061800020516      * se UTEAUT = *BLANK non siamo in sede
061900020516      *--     nel qual caso saranno attuate le protezioni del caso
062000020516w   1C                   WHEN      UTEAUT = *blank
062100020516     C                   eval      *in35  = *on
062200020516
062300020516x   1C                   OTHER
062400020516      *-----------------------------------------------------------------
062500020516     C                   movel     UTEFAF        DUTE01
062600020516      *-- Autorizzazione alla interrogazione delle bolle di sede
062700020516if  2C                   IF        §UTEibs <> *blanks
062800020516      *--   Con AUTORITÀ SPECIFICA
062900020516     C                   eval      wabi = §UTEibs
063000020516x   2C                   ELSE
063100020516      *--   Senza AUTORITÀ SPECIFICA
063200020516     C                   eval      wabi = UTEAUT
063300020516e   2C                   ENDIF
063400020516      *
063500020516     c                   clear                   tibs02ds
063600020516     c                   movel     'C'           t02mod
063700020516     c                   movel     knsif         t02sif
063800020516     c                   movel     'LAT'         t02cod
063900020516     c                   movel(p)  wabi          t02ke1
064000020516     c                   call      'TIBS02R'
064100020516     c                   parm                    kpjba
064200020516     c                   parm                    tibs02ds
064300020516if  2c                   if        t02err = *blanks
064400020516     c                   movel     t02uni        DLAT
064500020516e   2c                   endif
064600020516if  2c                   if        T02err <> *blanks   or
064700020516     c                             §LATabi = 'S'
064800020516     C                   eval      *in35  = *on
064900020516e   2c                   endif
065000020516      *
065100020516e   1c                   ENDSL
065200020516      *
065300020516      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
065400020516      *
065500020516      * Utente con abilitazione valida e diversa da "AZ"
065600020516if  1c                   if        wABI  <> 'AZ' and not *in35
065700020516      *
065800170109if  2c**                 if        wABI  <> I31abi
065900170109     c**                 reset                   $POG
066000020516      * Reperimento dei P.O. gestibili dall'utente
066100170109     c**                 clear                   TRUL31DS
066200170109     c**                 eval      I31abi = wabi
066300170109     c**                 eval      I31cdi = DUTdis
066400170109     c**                 eval      I31car = DUTare
066500170109     c**                 eval      I31cpo = DUTpou
066600170109     c**                 call      'TRUL31R'
066700170109     c**                 parm                    KPJBA
066800170109     c**                 parm                    TRUL31DS
066900170109if  3c**                 if        O31pog > *zeros
067000170109     c**                 movea     O31pog        $POG
067100170109e   3c**                 endif
067200170109e   2c**                 endif
067300020516      *
067400020516      *  controllo se il P.O. di competenza per il cliente è compreso
067500020516      *    tra quelli gestibili dall'utente
067600170109     c**                 movel     TASksc        wPOa
067700170109     c**   wPOa          lookup    $POG                                   30
067800170109if  2c**                 if        not *in30
067801170109     c                   clear                   tntaa1ds
067802170109     c                   eval      itaa1caut='§uteibs'
067803170109     c                   eval      itaa1ksc=%int(tasksc)
067804170109     c                   clear                   kpjbu
067805170109     c                   movel     tntaa1ds      kpjbu
067806170109     c                   call      'TNTAA1C'
067807170109     c                   parm                    kpjba
067808170109     c                   movel     kpjbu         tntaa1ds
067809170109    2c                   if        otaa1fabi='N'
067900020516     c                   eval      *in35  = *on
068000020516e   2c                   endif
068100020516      *
068200020516e   1c                   endif
068300020516
068400020516     C                   ENDSR
068500990630      **********************************************************************
068600990630      * ABILITO TASTI FUNZIONALI
068700990630      **********************************************************************
068800990630     C     ABITASFUN     BEGSR
068900040119
069000040119     c                   movel     TASflo        dTASflo
069100990630
069200000209      *  F2= Comunicazione di Anomalia, non abilitato se arrivo dalle C.A.
069300000209     c                   EVAL      *IN02 = (TASfda = 'S'
069400000209     c                                      and  %SUBST(I51OP0:1:1) <> 'D')
069500010221
069600010221      *  F4= ORM
069700111103     c                   clear                   wta4not
069800010221     c                   eval      ktrc = 'M'
069900010221     c     KTAStrc       CHAIN     TITA430C
070000020419 b1  C                   if        %found(tita430C)
070100020419 b2  C     ta4not        ifne      *blanks
070200020419     c                   eval      Wta4not = ta4not
070300020419     c                   open      fnorm01l
070400020419     c                   movel     wta4not       dsbl4m
070500020419     c                   move      §b4poe        kormpoe
070600020419     c                   move      §b4nsr        kormnsr
070700020419     c                   move      §b4nor        kormnor
070800020419     c                   move      §b4nrv        kormnrv
070900020419     c     kfnorm        setll     fnorm01l
071000020419 b3  c                   if        %equal(fnorm01l)
071100010221     C                   seton                                        04
071200020419 x3  C                   else
071300010221     C                   setoff                                       04
071400111103     c*****              clear                   Wta4not
071500020419 e3  C                   endif
071600020419     c                   close     fnorm01l
071700020419 e2  c                   endif
071800020419 x1  C                   else
071900010404     c                   clear                   Wta4not
072000010221     C                   setoff                                       04
072100020419 e1  C                   endif
072200990630
072300990630      *  F5= Eventi
072400990813     C     KEYspe        SETLL     FNEVB01L                               05
072500000901
072600000901      *  F8= Cod."CHI SONO"
072700020225     c                   eval      wtahtrc = 'C'
072800020225     C     KEYspeh       SETLL     TITAH30C                               08
072900040603
073000040603      *  F9= Transiti
073100040604     c                   Eval      woktrs = *Off
073200040603     c                   Eval      *In09 = *Off
073300040603     c                   Eval      wAr5Trd = 'TRS'
073400040604     c     kSpeAr5       Setgt     Fiar531c
073500040604     c                   Do        *Hival
073600040604     c     kSpeAr5       Readpe    Fiar531c
073700040604     c                   If        %Eof(Fiar531c)
073800040604     c                   Leave
073900040604     c                   EndIf
074000040604     c                   Eval      woktrs = *On
074100040603     c                   Eval      dAr5Trs = Ar5Uni
074200040603     c                   If        §Ar5Flp > *Zeros or §ar5Fl2 > *Zeros or
074300040603     c                             §Ar5Fl3 > *Zeros or §ar5Fl4 > *Zeros or
074400040603     c                             §Ar5Fl5 > *Zeros
074500040603     c                   Eval      *In09 = *On
074600040603     c                   EndIf
074700040604     c                   Leave
074800040604     c                   EndDo
074900040604     c                   If        woktrs = *Off and
075000040604     c                             (TasFlp > *Zeros or TasFl2 > *Zeros)
075100040603     c                   Eval      *In09 = *On
075200040603     c                   EndIf
075300040603
075400990630      *  F10= Variazioni
075401170118     c***                if        knmus<>'EDPRM'
075500170118     C***  KEYspe        SETLL     TITASV1L                               10
075501170118     c***                else
075502161006     C     KEYspe        SETLL     fiarbf2c                               10
075503170118     c***                endif
075600991117
075700030122      *  F11= Note Supermercato/Bancali
075800030121     c                   Eval      wAr5Trd = 'SCR'
075900040603     c     kSpeAr5       Setll     Fiar531c                               11
076000030703     c                   If        Not *In11
076100030703     c                   Eval      wAr5Trd = 'BNB'
076200040603     c     kSpeAr5       Setll     Fiar531c                               11
076300030703     c                   EndIf
076400030122     c                   If        Not *In11
076500030122     c                   Eval      wAr5Trd = 'BAN'
076600040603     c     kSpeAr5       Chain     Fiar531c
076700040603     c                   If        %Found(Fiar531c)
076800030122     c                   Movel     Ar5Uni        dAr5Ban
076900030122     c                   If        §Ar5Rb1 <> *Zeros or §Ar5Rb2 <> *Zeros
077000030122     c                   Eval      *In11 = *On
077100030122     c                   EndIf
077200030122     c                   EndIf
077300030127     c                   EndIf
077400030120
077500991117      *  F13= Tassazione
077600991117     c                   EVAL      *IN13 = (*IN35 = *off)
077700990630
077800000209      *  F14= Giacenza, non abilitato se arrivo dalle giacenze
077900000209     c                   IF        %SUBST(I51OP0:1:1) = 'G'
078000000209     c                   eval      *in14 = *off
078100000209     c                   ELSE
078200050401     C     KEYspe        setll     Tigcp51l                               14
078300000209     c                   ENDIF
078400041007      *
078500041007      *?Abilito F1=Aggiunta in Reclami?
078600041007      * (se richiamato da pgm. FIDNA1R...)
078700041007     c                   if            SDSprm >  1
078800041007     c                             and IA1ins = ' '
078900041007     c                   eval      *in01  = *on
079000041007     c                   endif
079100040416      *
079200040416      *?Abilito F15=Visualizzazione Reclami?
079300040416      * (se richiamato da pgm. FIDNA1R...)
079400040416     c                   eval      TGDtor  = 'S'
079500040416     c                   eval      TGDogg  = ds_OggSped
079600040416     c     K02TGD02      setll     FITGD000
079700160404     c                   eval      *in15   = (%equal(FITGD32L)   and
079800040416     c                                           (SDSprm =  1
079900040416     c                                        or (SDSprm >  1
080000040416     c                                        and IA1ins <> 'N')))
080100990630
080200990630      *  F16= Competenze di C/E
080300991117     c                   if         *in35 = *off
080400010312     c                   open      eccet30c
080500990630     C     KEYspe1       SETLL     ECCET30C                               16
080600010312     c                   close     eccet30c
080700991117     c                   endif
080800080118     c
080900080118      *  F17= Richiesta rigenerazione bolla se non vengo dalla manutenzione
081000080118     c                   if                 %SUBST(I51OP0:1:1) <> 'M'
081100030624     c                   Eval      kCod = '3A'
081200030624     c                   Movel(P)  TasCbo        kKey
081300030624     c                   ExSr      ChkTab
081400030624     c  n30              Movel     Wtbluni       ds3a
081500030627     c                   If        §3agen <> 'N'
081600000418     c                   EVAL      *IN17 = *on
081700030624     c                   EndIf
081800080118     c                   EndIf
081900990630
082000990630      *  F18= Legami
082100990630     c                   EVAL      *IN18 = (TASfa1 = 'S')
082200040119
082300040119      *  F19= Immagine LdV
082400040331     ***c                   eval      *in19 = (§FLOiml = 'S')
082500040331     c                   eval      *in19 = (§FLOiml <> *blanks)
082600990630
082700990630      *  F21= Dettaglio Colli
082800990630     C     KEYspe        SETLL     TITAT30C                               21
082900990630
083000000209      *  F22= C/Assegno, non abilitato se arrivo dai C/Assegni
083100000209     c                   EVAL      *IN22 = (TASfca = 'S'
083200000209     c                                      and  %SUBST(I51OP0:1:1) <> 'C' )
083300020225      *  F23= AWB FedEx
083400020225     c                   eval      wtahtrc = 'F'
083500020225     C     KEYspeh       SETLL     TITAH30C                               23
083600030121
083700030121      * Imposto le sk dei tasti funzione che verranno emessi nella riga variabile
083800030121      * a video (5 tasti funzione alla volta + tasto F24)
083900030121      * le sk devono essere caricate in base all'ordine di emissione dei tasti
084000030121      * funzione
084100030121      * 1 sk con il n.indicatore e 1 sk con lo stato dell'indicatore (acceso/spento)
084200040416     c                   Eval      SkInd(1) = 15
084300040416     c                   Eval      SkOnOff(1) = *In15
084400040416     c                   Eval      SkInd(2) = 5
084500040416     c                   Eval      SkOnOff(2) = *In05
084600040416     c                   Eval      SkInd(3) = 22
084700040416     c                   Eval      SkOnOff(3) = *In22
084800040416     c                   Eval      SkInd(4) = 2
084900040416     c                   Eval      SkOnOff(4) = *In02
085000040416     c                   Eval      SkInd(5) = 14
085100040416     c                   Eval      SkOnOff(5) = *In14
085200040416     c                   Eval      SkInd(6) = 10
085300040416     c                   Eval      SkOnOff(6) = *In10
085400040416     c                   Eval      SkInd(7) = 18
085500040416     c                   Eval      SkOnOff(7) = *In18
085600040416     c                   Eval      SkInd(8) = 11
085700040416     c                   Eval      SkOnOff(8) = *In11
085800040416     c                   Eval      SkInd(9) = 04
085900040416     c                   Eval      SkOnOff(9) = *In04
086000040416     c                   Eval      SkInd(10) = 08
086100040416     c                   Eval      SkOnOff(10) = *In08
086200040416     c                   Eval      SkInd(11) = 21
086300040416     c                   Eval      SkOnOff(11) = *In21
086400040416     c                   Eval      SkInd(12) = 23
086500040416     c                   Eval      SkOnOff(12) = *In23
086600041008     c                   Eval      SkInd(13) = 01
086700041008     c                   Eval      SkOnOff(13) = *In01
086800030121
086900030403     c                   Clear                   wOnOff
087000030121      * Conto quanti tasti funzione sono attivi
087100120711     c                   Do        13            xx
087200030121     c                   If        SkOnOff(xx) = '1'
087300030121     c                   Add       1             wOnOff
087400030121     c                   EndIf
087500030121     c                   EndDo
087600020225
087700020225      *  F24= Altri tasti
087800020226     c                   exsr      sub_f24
087900990630
088000990630     C                   ENDSR
088100990630      **********************************************************************
088200990630      * EMETTO VIDEATA DI ENTRATA
088300990630      **********************************************************************
088400990630     C     SUB_FMT001    BEGSR
088500990705
088600990705      * Per il primo richiamo imposto i campi della videata di entrata, i campi comuni a più videate
088700990708      *  e i dati del mittente e del destinatario per tutte le videate
088800990705     c                   IF        WSB51D01 = *blanks
088900990705     c                   EXSR      RIED01
089000990705     c                   eval      WSB51D01 = 'S'
089100990705     c                   ENDIF
089200990705
089300990630     c                   write     SB51T01
089400990630     c                   write     SB51Z00
089500120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
089600120302     c                   exfmt     SB51D01
089700120302     c                   else
089800120302     c                   write     sb51d01
089900120302     c                   read(E)   tnsb51d
090000120302     c                   if        %error
090100120302     c                   eval      *inkc='1'
090200120302     c                   endif
090300120302     c                   endif
090400990630
090500990630     c                   ENDSR
090600990618      **********************************************************************
090700990630      * IMPOSTO CAMPI VIDEO 1 E QUELLI COMUNI A PIU' VIDEATE
090800990618      **********************************************************************
090900990628     C     RIED01        BEGSR
091000151020     c                   eval      *in70  = *off
091100151020     c                   clear                   dAR5fat
091200990618
091300990630      * I campi con prefisso V0C o V0D sono presenti in più videate
091400990630
091500990630     c                   z-add     taslnp        V0Clnp
091600990630     c                   z-add     tasnrs        V0Cnrs
091700990630     c                   z-add     tasnsp        V0Cnsp
091800990630     c                   z-add     TASaas        V0Cdsp
091900990630     c                   movel     TASmgs        V0Cdsp
092000990630     c     *usa          move      V0Cdsp        DATA_eur
092100990630     c     *eur          move      DATA_eur      V0Cdsp
092200990630      *
092300990630     c                   move      TASccm        Wccm4
092400990630     c                   move      TASksc        Wksc4
092500990628      * Tipo servizio
092600990630     C                   eval      KCOD = '5E'
092700990628     C                   movel(P)  tastsp        KKEY
092800990628     C                   EXSR      CHKTAB
092900990629     c                   if        *in30 = *off
093000090605     C                   movel     Wtbluni       ds5e
093100090605     c                   eval      V0Dtsp = §5ed08
093200990629     c                   endif
093300990628      * Stato bolla
093400990628     c                   SELECT
093500990628     c                   WHEN      TASFIC <> *blanks
093600990630     c                   eval      V0Dsts = 'CONTABILIZZ.'
093700990628     c                   WHEN      TASDFT > *zeros
093800990630     c                   eval      V0Dsts = 'FATTURATA   '
093900990628     c                   OTHER
094000990630     c                   eval      V0Dsts = 'DA FATTURARE'
094100990628     c                   ENDSL
094200990628      * P.O. Arrivo
094300021119     c                   Clear                   Og143
094400021119     c                   Clear                   LnaNtw
094500990630     c                   z-add     taslna        V0Clna
094600990628     c                   z-add     taslna        Wlinea
094700990628     C                   EXSR      CHKORG
094800990630     C                   movel     orgdes        V0Dlna
094900021119     c                   Movel     Orgde3        Og143
095000021119     c                   Movel     §OgNtw        LnaNtw
095100990630      * Tipo bolla
095200990630     c                   movel     tastbl        V0Ctbl
095300990630     C                   eval      KCOD = 'TB'
095400990628     C                   movel(P)  tastbl        KKEY
095500990628     C                   EXSR      CHKTAB
095600990629     c                   if        *in30 = *off
095700990629     C                   movel     Wtbluni       DSTB
095800990630     C                   movel     §TBdes        V0Dtbl
095900990726     C                   movel     §TBdes        V0Dtb1
096000990629     c                   endif
096100151020     c
096200151020     c                   clear                   ds3a
096300151020     c                   Eval      kCod = '3A'
096400151020     c                   Movel(P)  TasCbo        kKey
096500151020     c                   ExSr      ChkTab
096600151020     c  n30              Movel     Wtbluni       ds3a
096700990628      * Data 1a Consegna Parziale
096800990628     c                   IF        TASdcp > *zeros
096900990630     c     *iso          move      TASdcp        DATA_eur
097000990813     c     *eur          move      DATA_eur      Wtasdcp
097100990813     C                   eval      V0Cdcp = %EDITC(Wtasdcp:'Y')
097200990628     c                   ENDIF
097300990628      * Codice consegna anomala
097400990702     c                   IF        TAScca <> *blanks
097500990701     c                   movel     tascca        v0ccca
097600990630     C                   eval      KCOD = '7O'
097700990628     C                   movel(P)  tascca        KKEY
097800990628     C                   EXSR      CHKTAB
097900990702     C  N30              movel     Wtbluni       V0Dcca
098000990702     c                   ENDIF
098100990628      * Data Consegna Reale
098200990628     c                   IF        TASdcm > *zeros
098300990630     c     *iso          move      TASdcm        DATA_eur
098400990630     c     *eur          move      DATA_eur      W0080
098500990701     C                   eval      V0Cdcm = %EDITC(W0080:'Y')
098600990628     c                   ENDIF
098700990628      * Ora Consegna Reale
098800990628     c                   IF        TASHmc > *zeros
098900990628     c                   eval      *IN40 = *on
099000990701     c                   z-add     tashmc        v0chmc
099100990628     c                   ENDIF
099200990628      * Numero e data distinta consegna
099300990701     c                   z-add     tasndc        v0cndc
099400990628     c                   IF        TASddc > *zeros
099500990630     c     *iso          move      TASddc        DATA_eur
099600990630     c     *eur          move      DATA_eur      W0080
099700990701     C                   eval      V0Cddc = %EDITC(W0080:'Y')
099800990628     c                   ENDIF
099900990628      * Dati Fattura
100000990629      *
100100990629      *    Decodifico Divisa e memorizzo dati in DS di comodo
100200990630     C                   eval      KCOD = 'CV'
100300990628     C                   movel(P)  tasdiv        KKEY
100400990628     C                   EXSR      CHKTAB
100500990629     c                   if        *in30 = *off
100600990628     C                   movel     Wtbluni       WDSCV
100700990629     c                   endif
100800991021      *    Se ho il libro IVA determino se fattura immediata
100900991021     c                   clear                   Wfatimmed
101000991021     c                   IF        TASfiv > *zeros
101100991021     c                   EXSR      FATIMMED
101200991021     c                   ENDIF
101300991021      *
101400991021      *    Se Fattura immediata  scrivo IMPORTO altrimenti IMPONIBILE
101500991021     c                   IF        Wfatimmed = 'S'
101600990716      *        se fatturata calcolo l'importo
101700990813     c                   if        TASfts = '1'
101800990714     c                   eval      D17tla = 'L'
101900990825     C
102000991021     c                   if         Wfatimmed = 'S'  and  §TBTPO = 'F'
102100990825     c                   eval      D17PRP = 'S'
102200990813     c                   endif
102300990825     c                   eval      D17tbo = '0'
102400990714     c                   eval      D17crb = 'S'
102500990714     c                   eval      D17aas = TASaas
102600990714     c                   eval      D17lns = TASlnp
102700990714     c                   eval      D17nrs = TASnrs
102800990714     c                   eval      D17nsp = TASnsp
102900990714     c                   eval      D17tbs = TAStbl
103000990714     c                   eval      D17fim = 'F'
103100990714     c                   CALL      'FNLV16R'
103200990714     c                   PARM                    FNLV16DS
103300990714     c                   PARM                    DTASV
103400990716     c                   endif
103500990714     c                   eval      *in41 = *off
103600990714     c                   z-add     O17IFT        W0133
103700990813      *
103800990628     c                   ELSE
103900990702     c                   z-add     TASimv        W0133
104000990708     c                   eval      *in41 = *on
104100990628     c                   ENDIF
104200990628      *
104300990629     c                   IF         W0133 > *zeros
104400990628     c                   movel     W§cvfdc       Wtstdec
104500990628     c                   exsr      EDTimporti
104600990628     c                   movel     IMPeditato    v1cimp
104700990628     c                   ENDIF
104800011029      *    Divisa
104900011029     c                   IF        v1cimp <>*zeros
105000011029     c                   movel     tasdiv        v1cdiv
105100011029     c                   endif
105200990629      *    P.O. IVA - Numero Fattura - Data fattura
105300990702     c                   z-add     TASfiv        V0Cfiv
105400990702     c                   z-add     TASnft        V0Cnft
105500990629     c                   IF        TASdft > *zeros
105600990630     c     *iso          move      TASdft        DATA_eur
105700990630     c     *eur          move      DATA_eur      W0080
105800990702     C                   eval      V0Cdft = %EDITC(W0080:'Y')
105900990702      *    Divisa
106000990702     c                   ENDIF
106100990701      * Dati Mittente per tutte le videate
106200990701     c                   z-add     TASccm        V0Cccm
106300990701      *    se non codificato aggancio TITAA
106400990707     c                   IF        Wccm4 = *zero  or  Wccm4 = 8888
106500990629     c                   eval      ktrc = 'M'
106600990701     c     KTAStrc       CHAIN     TITAA30C
106700990701      *
106800990701     c                   IF        %FOUND
106900990629     c                   movel     TAArsc        V1Crsm
107000990701     c                   movel     TAAind        V1Cinm
107100990701     c                   movel     TAAloc        V1Clom
107200990701     c                   if        TAAnaz <> *blanks
107300990701     c                   movel     TAAnaz        V1Cpnm
107400990701     c                   else
107500990701     c                   movel     TAAprv        V1Cpnm
107600990701     c                   endif
107700990701     c                   movel     TAAcap        V0Ccam
107800990701     c                   movel     TAArsc        V7Crsm
107900990701     c                   movel     TAAind        V7Cinm
108000990701     c                   movel     TAAloc        V7Clom
108100990702     c                   movel     TAAprv        V7Cprm
108200990702     c                   movel     TAAnaz        V7Cnzm
108300990701     c                   movel     TAAcpi        V7Cpim
108400990701     c                   ENDIF
108500990629      *    se codificato aggancio anagrafico
108600990629     c                   ELSE
108700990701     c                   z-add     V0Cccm        I69kac
108800990701     c                   z-add     V0Cccm        I69kin
108900990701     c                   z-add     V0Cccm        I69kcs
109000990630     c                   exsr      CTR_anagra
109100990701      *
109200990630     c                   movel     ACOrag        V1Crsm
109300990630     c                   movel     INDvia        V1Cinm
109400990630     c                   movel     INDcit        V1Clom
109500990630     c                   if        INDsta <> *blanks
109600990630     c                   movel     INDsta        V1Cpnm
109700990630     c                   else
109800990630     c                   movel     INDprv        V1Cpnm
109900990630     c                   endif
110000990701     c                   movel     INDcae        V0Ccam
110100990701     c                   movel     ACOrag        V7Crsm
110200990701     c                   movel     INDvia        V7Cinm
110300990701     c                   movel     INDcit        V7Clom
110400990701     c                   movel     INDprv        V7Cprm
110500990701     c                   movel     INDsta        V7Cnzm
110600990701     c                   movel     INDiva        V7Cpim
110700990629     c                   ENDIF
110800990629      * Destinatario
110900990701      *    se assegnato utilizzo codice di tassazione come codice destinatario
111000990629     c                   IF        §TBtpo = 'A'
111100990701     c                   z-add     TASksc        V0Cccd
111200990629     c                   ENDIF
111300990701      *
111400111011     c* cerco 2 parte della ragione sociale
111500111011     c                   eval      ktrc = 'D'
111600111011     c                   clear                   rsd2             35
111700111011     c     ktastrc       chain     tita430c
111800111011 b1  c                   if        %found(tita430C)
111900111011     c                   eval      rsd2=ta4not
112000111011     c                   endif
112100111011     c
112200990629     c                   movel     TASrsd        V1Crsd
112300111011     c                   if        %subst(tasrsd:31:5)= *blanks and rsd2<>*blanks
112400111011     c                   eval      v1crsd=%trim(v1crsd) + ' ' +rsd2
112500111011     c                   endif
112600990629     c                   movel     TASind        V1Cind
112700990629     c                   movel     TASlod        V1Clod
112800990629     c                   if        TASnzd <> *blanks
112900990629     c                   movel     TASnzd        V1Cpnd
113000990629     c                   else
113100990629     c                   movel     TASprd        V1Cpnd
113200990629     c                   endif
113300990701     c                   movel     TAScad        V0Ccad
113400990702     c                   movel     TASnzd        V0Cnzd
113500111011     c                   clear                   v7crsd
113600990701     c                   movel     TASrsd        V7Crsd
113700111011     c*
113800111011 b1  c                   if        rsd2<>*blanks
113900111011     c                   if        %subst(tasrsd:35:1)<>' '
114000111011     c                   eval      v7crsd=tasrsd +ta4not
114100111011     c                   else
114200111011     c                   eval      v7crsd=%trim(v7crsd) + ' ' +rsd2
114300111011     c                   endif
114400111011     c
114500111011     c                   endif
114600990701     c                   movel     TASind        V7Cind
114700990701     c                   movel     TASlod        V7Clod
114800990701     c                   movel     TASprd        V7Cprd
114900990701     c                   movel     TAScpd        V7Cpid
115000061108      * Codice fiscale mittente/destinatario da Tita4 rcd Q
115100061108     c                   eval      ktrc = 'Q'
115200061108     c                   clear                   dta4q
115300061108     c     ktastrc       chain     tita430c
115400061108 b1  c                   if        %found(tita430C)
115500061108     c                   eval      dta4q = ta4not
115600061108     c                   endif
115700061108     c                   eval      v7cdfm = §ta4qcfm
115800061108     c                   eval      v7cdfd = §ta4qcfd
115900990726      *
116000990701      * Cliente Fatturazione
116100990701     c                   z-add     TASksc        VTCksc
116200990701     c                   SELECT
116300990701      *     assegnato non codificato  (Da TITAS)
116400990708     c                   WHEN      §TBtpo = 'A' and  *IN41 = *off
116500990726     c                   movel     TASrsd        VTDksc
116600990726      *     assegnato codificato (Decodifico la ragione sociale)
116700990701     c                   WHEN      §TBtpo = 'A'
116800990702     c                   z-add     tasksc        I69kac
116900990701     c                   exsr      CTR_anagra
117000990726     c                   movel     ACOrag        VTDksc
117100990701      *     porto franco
117200990701     c                   WHEN      §TBtpo = 'F'
117300990701     c                   movel     V7Crsm        VTDksc
117400990701     c                   ENDSL
117500991123      * Mittente Originale
117600991123     c                   eval      ktrc = 'O'
117700991123     c     KTAStrc       CHAIN     TITAA30C
117800991123     c                   If        %FOUND
117900991123     c                   eval      *in51 = *on
118000991123     c                   eval      v7crmo = TAArsc
118100991123     c                   Endif
118200990629      * Riferimento mittente numerico
118300990701     c                   z-add     TASrmn        V0Crmn
118400990629      * Riferimento mittente alfanumerico e riferimento partner
118500990629     c                   eval      ktrc = 'A'
118600990629     c     KTAStrc       CHAIN     TITA430C
118700990629     c                   If        %FOUND
118800990629     c                   movel     TA4not        DTA4A
118900990630     c                   movel     §TA4ARMA      V1Crma
119000070927     c*********+         movel     §TA4ARPT      V1Crpt
119100160201     c                   movel     §TA4Anas      V1Cnas
119200990629     c                   ENDIF
119300070927      * Riferimento  partner nel record "E"
119400070927     c                   eval      ktrc = 'E'
119500070927     c     KTAStrc       CHAIN     TITA430C
119600070927     c                   If        %FOUND
119700070927     c                   movel     TA4not        dsbl4E
119800070927     c                   movel     §b4ERP        V1Crpt
119900070927     c                   ENDIF
120000000222      * Numero parcel DPD
120100060518     c***                eval      ktrc = 'F'
120200060518     c                   eval      ktrc = 'I'
120300000222     c     KTAStrc       CHAIN     TITA430C
120400000222     c                   If        %FOUND
120500060518     c**                 movel     TA4not        DSBL4F
120600060518     c**                 move(P)   §B4fnp        V1Crpt
120700060518     c**                 movel     §B4rtn        V1Crtn
120800060518     c**                 movel     §B4zon        V1Czon
120900060518     c                   movel     TA4not        DSBL4i
121000060518     c                   movel(P)  §B4ipn        V1Crpt
121100060518     c                   movel     §B4ida        V1Crtn
121200060518     c                   movel     §B4iza        V1Czon
121300060518     C                   eval      *in52 = (V1Crtn <> *blanks or
121400060518     C                                      V1Czon <> *blanks)
121500000222     c                   ENDIF
121600990629      * Natura merce
121700160201     c****               movel     TASnas        V1Cnas
121800990629      * Segnacolli
121900990630     c                   z-add     TASfls        V1Cfls
122000990629     c                   z-add     TASnrs        V1Cnrss
122100990629     c                   z-add     TASncd        V1Cncd
122200990629     c                   z-add     TASnca        V1Cnca
122300990629     c                   if        TASfns = 'S'
122400990629     c                   eval      V1Dfns = 'NON SEQUENZIALI'
122500990629     c                   endif
122600990629     c                   z-add     TASznc        V1Cznc
122700050113     c* flag raggruppamento spedizioni
122800050113     c                   movel     TASxco        V1Cxco
122900990629      * Contrassegno
123000160412     c                   setoff                                       39
123100990630     c                   IF        TASfca = 'S'
123200990630     c     Keyspe        chain     TNCSB03L                           30
123300160419     c****               If        %FOUND  and  CSBsta <> 9
123400160419     c                   If        %FOUND
123500990630     c                   z-add     CSBcas        V1Ccas
123600990630     c                   If         CSBvca <> *blanks
123700990630     c                   movel     CSBvca        V1Cvca
123800990630     c                   Else
123900990630     c                   movel     'ITL'         V1Cvca
124000990630     c                   Endif
124100990630     c                   Endif
124200160412     c                   If        %found and   CSBsta= 9
124300160412     c                   seton                                        39
124400160412     c                   endif
124500990630     c                   ENDIF
124600031119
124700031119      * Referente e Telefono del destinatario
124800110610      * e profilo utente/terminale che ha immesso la bolla
124900031119     c                   Clear                   V7cRef
125000031119     c                   Clear                   V7cTeld
125100110610     c                   Clear                   V7cpru
125200110610     c                   Clear                   V7cnmtd
125300120712     c                   clear                   W02DRDT
125400120712     c                   clear                   W02MTVDT
125500120712     c                   clear                   W02DDTIL
125600120712     c                   clear                   W02DIMTV
125700120712     c                   clear                   W02DMTVDT
125800120712     c                   clear                   W02DRID
125900120829     c                   clear                   W02lbl
126000110610     c                   setoff                                       66
126100120404     c                   setoff                                       6768
126200031119      * li prendo da Fiar500f record 'GEN'
126300031119     c                   Clear                   dAr5Gen
126400031119     c                   Eval      war5Trd = 'GEN'
126500040603     c     kSpeAr5       Chain     Fiar531c
126600040603If  1c                   If        %Found(Fiar531c)
126700031119     c                   Movel     Ar5Uni        Dar5Gen
126800031119     c                   Eval      V7cRef = §Ar5Ref
126900031119     c                   Eval      V7cTeld = §Ar5Teld
127000110610     c                   Eval      V7cpru  = §Ar5bpru
127100110610     c                   Eval      V7cnmtd = §Ar5bnmtd
127200120404    2c                   if        v7cpru<>*blanks
127300110610     c                   seton                                        66
127400120404    2c                   endif
127500120330     c* restituzione DDt al mittente
127600151119    2c***                if        §ar5drdt<>*blanks
127700151119     c****               seton                                        67
127800120712     c* data ricezione
127900151119    3c***                if        §ar5drdt<>'99999999'
128000151119     c***                movel     §ar5drdt      w0080             8 0
128100151119     c***  *iso          movel     w0080         dataiso
128200151119     c***                eval      data_eur=dataiso
128300151119     c***                movel     data_eur      w02drdt
128400151119    3c***                endif
128500120712     c* "Elaborata per il mittente"
128600151119     c***                clear                   w02dfstd
128700151119     c***                if        §ar5fstd='S'
128800151119     c***                eval      w02dfstd='Elaborata per invio al Mittente'
128900151119     c***                endif
129000120712     c* motivo
129100151119    3c***                if        §ar5mtvdt<>*blanks
129200120712     c* apro file TNTBE di filiale
129300151119     c***                if        not %open(tntbe01l)
129400151119     c***                open(e)   tntbe01l
129500151119     c***                if        %error
129600151119     c***                eval      %subst(Wtntbe:7:4)='GRPF'
129700151119     c***                open(e)   tntbe01l
129800151119     c***                endif
129900151119     c***                endif
130000150828     c***
130100151119     c***                eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
130200151119     c***                                   %subst(§ar5mtvdt:4:5)
130300151119     c***                eval      kcode='MDE'
130400151119     c***                clear                   kke1
130500151119     c***                clear                   kke2
130600151119     c***                eval      kke1=%subst(§ar5mtvdt:1:3)
130700151119     c***                eval      kke2=%subst(§ar5mtvdt:4:5)
130800151119     c***  ktbe          chain     tntbe01l
130900151119    4c***                if        %found(tntbe01l)
131000151119     c***                eval      w02dmtvdt=tbeuni
131100151119    4c***                endif
131200151119    4c***                if        §ar5dimtv >*zeros
131300151119     c***                movel     §ar5dimtv     w0080             8 0
131400151119     c***  *iso          movel     w0080         dataiso
131500151119     c***                eval      data_eur=dataiso
131600151119     c***                movel     data_eur      w02dimtv
131700151119     c***                eval      w02ddtil='Inserito il'
131800151119    4c***                endif
131900151119    3c***                endif
132000151119    2c***                endif
132100031119    1c                   EndIf
132200120712     c* DATA RICEZIONE IMM.DOC.
132300120712     c                   Clear                   dAr5doc
132400120712     c                   Eval      war5Trd = 'DOC'
132500120712     c     kSpeAr5       Chain     Fiar531c
132600120712If  1c                   If        %Found(Fiar531c)
132700120712     c                   Movel     Ar5Uni        Dar5doc
132800120712    4c                   if        §ar5jdrid >*zeros
132900120712     c                   seton                                        67
133000120712     c                   movel     §ar5jdrid     w0080             8 0
133100120712     c     *iso          movel     w0080         dataiso
133200120712     c                   eval      data_eur=dataiso
133300120712     c                   movel     data_eur      w02drid
133400120712    4c                   endif
133500150828     c* restituzione DDt al mittente
133600151119    2c                   if        §ar5drdt<>*blanks
133700151119     c                   seton                                        67
133800150828     c* data ricezione
133900151119    3c                   if        §ar5drdt<>'99999999'
134000151119     c                   movel     §ar5drdt      w0080             8 0
134100151119     c     *iso          movel     w0080         dataiso
134200151119     c                   eval      data_eur=dataiso
134300151119     c                   movel     data_eur      w02drdt
134400151119    3c                   endif
134500150828     c* "Elaborata per il mittente"
134600151119     c                   clear                   w02dfstd
134700151119     c                   if        §ar5fstd='S'
134800151119     c                   eval      w02dfstd='Elaborata per invio al Mittente'
134900151119     c                   endif
135000150828     c* motivo
135100151119    3c                   if        §ar5mtvdt<>*blanks
135200150828     c* apro file TNTBE di filiale
135300151119     c                   if        not %open(tntbe01l)
135400151119     c                   open(e)   tntbe01l
135500151119     c                   if        %error
135600151119     c                   eval      %subst(Wtntbe:7:4)='GRPF'
135700151119     c                   open(e)   tntbe01l
135800151119     c                   endif
135900151119     c                   endif
136000150828     c
136100151119     c                   eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
136200151119     c                                      %subst(§ar5mtvdt:4:5)
136300151119     c                   eval      kcode='MDE'
136400151119     c                   clear                   kke1
136500151119     c                   clear                   kke2
136600151119     c                   eval      kke1=%subst(§ar5mtvdt:1:3)
136700151119     c                   eval      kke2=%subst(§ar5mtvdt:4:5)
136800151119     c     ktbe          chain     tntbe01l
136900151119    4c                   if        %found(tntbe01l)
137000151119     c                   eval      w02dmtvdt=tbeuni
137100151119    4c                   endif
137200151119    4c                   if        §ar5dimtv >*zeros
137300151119     c                   movel     §ar5dimtv     w0080             8 0
137400151119     c     *iso          movel     w0080         dataiso
137500151119     c                   eval      data_eur=dataiso
137600151119     c                   movel     data_eur      w02dimtv
137700151119     c                   eval      w02ddtil='Inserito il'
137800151119    4c                   endif
137900151119    3c                   endif
138000151119    2c                   endif
138100120712    1c                   endif
138200120829     c* se non presente data scannerizzazione visualizzo quella della figlia eventuale
138300120829     c                   if        w02drid=*zeros
138400120829     c                   exsr      replbl
138500120829     c                   if        wtrovl=*on
138600120829     c                   seton                                        67
138700120829     c                   movel     §ar5jdrid     w0080             8 0
138800120829     c     *iso          movel     w0080         dataiso
138900120829     c                   eval      data_eur=dataiso
139000120829     c                   movel     data_eur      w02drid
139100120829     c                   eval      w02lbl='Bolla legata'
139200120829     c                   endif
139300120829     c                   endif
139400121217     c* bolla orignale / di recupero per variazioni post fatturazione
139500121217     c                   if        w001aleg=*blanks
139600121217     c                   setoff                                       71
139700121217     c                   Eval      war5Trd = 'LEG'
139800121217     c     kSpeAr5       Chain     Fiar531c
139900121217If  1c                   If        %Found(Fiar531c)
140000121217     c                   seton                                        71
140100121217     c                   eval      waas_leg=%dec(%subst(ar5uni:1:4):4:0)
140200121217     c                   eval      wlnp_leg=%dec(%subst(ar5uni:5:3):3:0)
140300121217     c                   eval      wnrs_leg=%dec(%subst(ar5uni:8:2):2:0)
140400121217     c                   eval      wnsp_leg=%dec(%subst(ar5uni:10:7):7:0)
140500121217     c                   clear                   v1cleg
140600121217     c                   if        §tbrbl='R'
140700121217     c                   eval      v1dleg='Bolla Originale     (?)'
140800121217     c                   else
140900121217     c                   eval      v1dleg='Recupero Variazioni (?)'
141000121217     c                   endif
141100121217    1c                   endif
141200121217     c                   endif
141300040301
141400040301      * Se colli di valore visualizzo "V" nell'apposito campo
141500040301      *  con lo specifico attributo di visualizzazione
141600040603if  1c**!!!              if        §AR5bva = *blanks
141700040603     c**!!!              eval      V1Cbva  = *blanks
141800040603x   1c**!!!              else
141900040603     c**!!!              eval      V1Cbva  = 'V'
142000040603e   1c**!!!              endif
142100040603     c                   Eval      V1cBva = §FloTba
142200040301
142300030121      * Bancali
142400030703     c                   Clear                   V0cNba
142500030703     c                   Clear                   V0cNb2
142600030121     c                   Clear                   V0dTba
142700030121     c                   Clear                   V0dTb2
142800030703      * Bancali - part.varie "B"
142900030703If  0c                   If        %subst(TasGva:1:1) = 'B'
143000030121     c                   Clear                   dAr5Ban
143100030121     c                   Eval      wAr5Trd = 'BAN'
143200040603     c     kSpeAr5       Chain     Fiar531c
143300040603If  1c                   If        %Found(Fiar531c)
143400030121     c                   Movel     Ar5Uni        Dar5Ban
143500030121     c                   Eval      V0cNba = §Ar5Nba
143600030121     c                   Eval      V0cNb2 = §Ar5Nb2
143700030121If  2c                   If        §Ar5Tba <> *Zeros
143800030703     c                   Eval      wtba = §Ar5Tba
143900030703     c                   ExSr      Sr_DecTba
144000030703     c                   Eval      V0dTba = wdestba
144100030121    2c                   EndIf
144200030121If  2c                   If        §Ar5Tb2 <> *Zeros
144300030703     c                   Eval      wtba = §Ar5Tb2
144400030703     c                   ExSr      Sr_DecTba
144500030703     c                   Eval      V0dTb2 = wdestba
144600030121    2c                   EndIf
144700030121    1c                   EndIf
144800030703    0c                   EndIf
144900030703      * Bancali - part.varie "O"
145000030703If  0c                   If        %subst(TasGva:1:1) = 'O'
145100030703     c                   Clear                   dAr5Bnb
145200030703     c                   Eval      wAr5Trd = 'BNB'
145300040603     c     kSpeAr5       Chain     Fiar531c
145400040603If  1c                   If        %Found(Fiar531c)
145500030703     c                   Movel     Ar5Uni        Dar5Bnb
145600030703     c                   Eval      V0cNba = §Ar5bNba
145700030703     c                   Eval      V0cNb2 = §Ar5bNb2
145800030703If  2c                   If        §Ar5bTba <> *Zeros
145900030703     c                   Eval      wtba = §Ar5bTba
146000030703     c                   ExSr      Sr_DecTba
146100030703     c                   Eval      V0dTba = wdestba
146200030703    2c                   EndIf
146300030703If  2c                   If        §Ar5bTb2 <> *Zeros
146400030703     c                   Eval      wtba = §Ar5bTb2
146500030703     c                   ExSr      Sr_DecTba
146600030703     c                   Eval      V0dTb2 = wdestba
146700030703    2c                   EndIf
146800030703    1c                   EndIf
146900030703    0c                   EndIf
147000990701      * Colli, Peso e Volume
147100990702     c                   z-add     TASncl        V0Cncl
147200990630      *
147300990629     c                   movel     TASfpb        V1Cfpb
147400990629     c                   z-add     TASpkb        V1Cpkb
147500990629     c                   z-add     TASpkc        V1Cpkc
147600990630     c                   z-add     TASncp        V1Cncp
147700151016     c*//                movel     TASfpf        V0Cfpf
147800990702     c                   z-add     TASpkf        V0Cpkf
147900160608      *
148000160608     c                   movel     TASfvb        V1Cfvb
148100160608     c                   z-add     TASvlb        V1Cvlb
148200160608     c                   z-add     TASvlc        V1Cvlc
148300160608     c                   z-add     TASncr        V1Cncr
148400160608     c                   movel     TASfvf        V0Cfvf
148500160608     c                   z-add     TASvlf        V0Cvlf
148600030121      * Peso fatturato solo se la bolla è fatturarata
148700151016     c                   Clear                   V0Cfpf
148800030121     c                   Clear                   V0cPeso
148900151124If  1c***********        If        TasDft > *Zeros  or
149000151124     c                   if        TASimv > *Zeros
149100151020      *?Si reperiscono i dati da FIAR5/"FAT"?
149200151020      * non se è la seconda bolla di bolla doppia
149300151020     c                   clear                   dAR5fat
149400151020     c                   if        §3atb2=*blanks or
149500151020     c                             (%subst(§3atb1:1:1)=%subst(tastbl:1:1))
149600151020     c                   Eval      wAR5trd = 'FAT'
149700151020     c     kSpeAr5       Chain     Fiar531c
149800151020if  2c                   if        %Found(Fiar531c)
149900151020     c                   Movel     Ar5Uni        dAr5Fat
150000151125     c                   eval      v0cfpf=dar5fat.§ar5fptas
150100151125     c                   if        v0cfpf='Z' or v0cfpf='T'
150200151125     c                   eval      v0cfpf='V'
150300151126     c                   endif
150400151125     c                   eval      V0Cpeso = dAR5fat.§AR5pkTas
150500151020     c                   endif
150600160616     c                   eval      v0cvlf = dar5fat.§ar5vltas
150700160616     c                   eval      v0cfvf = dar5fat.§ar5fvtas
150800160608      * volume desunto visualizzo come rilevato totale da VDL
150900160608     c                   if        dar5fat.§ar5fvtas = 'D'
151000160608     c                   eval      v1cvlc = dar5fat.§ar5vltas
151100160608     c                   eval      v0cvlf = dar5fat.§ar5vltas
151200160608     c                   eval      v0cfvf = 'T'
151300160608     c                   eval      v1cncr = v0cncl
151400160608     c                   endif
151500160608
151600151020     c                   endif
151700151125
151800151125     c                   if        v0cfpf=*blanks
151900151125     c                   movel     TASfpf        V0Cfpf
152000151125     c                   clear                   v0cpeso
152100151125     c                   endif
152200151020     c
152300151125      * -?Abilitazione Visualizzazione Pesi usati in Tassazione?
152400160505      * se utente EDP e peso o volume desunto
152500151125     c                   eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
152600160505     c                                        and   ( v0cfpf = 'D' or
152700160505     c                                        dar5fat.§ar5fvtas = 'D'))
152800030121      * Prendo il peso fatturato
152900151125     c***                If        TasFpf = 'R' or ( tasfpf='D' and
153000151125     c***                          dar5fat.§AR5PKTAS=0)
153100151125     c***                Z-Add     TasPkf        V0cPeso
153200151125     c***                movel     'R'           V0Cfpf
153300151125     c***                EndIf
153400151125      * VDL
153500030121      * Prendo il peso da cml - la tara
153600151126If  2c                   If        v0cfpf = 'V' and v0cpeso=0
153700030121     c                   Clear                   dsQt1
153800030121     c                   Movel     'QT'          Kcod
153900030121     c                   Movel     1             Kkey
154000030121     c     KeyTab        Chain     Tabel00f
154100030121     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
154200030121     c                   Movel     TblUni        dsQt1
154300030121     c                   EndIf
154400150717     c* se la data fattura è >0 e minore uguale  alla data in tabella,
154500150717     c*  sottraggo il campo TARA da TCO, altrimenti da TPC
154600150717     c                   if        tasdft>0 and §qtdst>0 and tasdft<=§qtdst
154700150717     c     §QtTco        Mult      TasNcp        wTara
154800150717     c                   else
154900030121     c     §QtTpc        Mult      TasNcp        wTara
155000150717     c                   endif
155100150717     c
155200030121     c     TasPkc        Sub(h)    wTara         wNtara
155300030121     c                   Z-Add     wNtara        V0cPeso
155400030121    2c                   EndIf
155500151125      * DESUNTO
155600151125     c                   if        v0cfpf='D'
155700151125     c                   if        v0cpeso=0
155800151125     c                   Z-Add     TasPkf        V0cPeso
155900151125     c                   movel     'R'           V0Cfpf
156000151125     c                   else
156100151125      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
156200151125      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
156300151125      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
156400151125      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
156500151125     c                   eval      V0Cfpf  = 'V'
156600151125     c                   eval      V1Cpkc  = V0Cpeso
156700151125     c                   eval      V1Cncp  = V0Cncl
156800151125     c                   endif
156900151125     c                   endif
157000151125      * REALE imposto il peso se non presente su record FAT
157100151125     c                   if        v0cfpf='R'
157200151125     c                             and v0cpeso=0
157300151125     c                   z-add     taspkf        v0cpeso
157400151125     c                   endif
157500151020     c
157600151020
157700151020      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
157800151020      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
157900151125if  2c*                  if        dar5fat.§AR5PKTAS>0
158000151125     c*
158100151125if  3c*                  IF        dAR5fat.§AR5FPTAS='D'
158200151125     c*                  eval      V0Cfpf  = 'V'
158300151016      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
158400151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
158500151125     c*                  eval      V1Cpkc  = V0Cpeso
158600151016      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
158700151125     c*                  eval      V1Cncp  = V0Cncl
158800151125   x3c*                  else
158900151020     c* se non è desunto--> imposto il fatturato da record FAT
159000151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
159100151125    3c*                  endif
159200151125e   2c*                  endif
159300151020     c
159400151125     c****               eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
159500151125     c****                                    and     TASfpf = 'D' )
159600030121    1c                   EndIf
159700990701      * Merce Portata a Magazzino
159800990701     c                   IF        TASfdn = 'S'
159900990701     C                   eval      V0Dfdn = 'SI'
160000990701     c                   ENDIF
160100990701      * Fermo Deposito
160200990702     c                   IF        TASffd = 'S'
160300051107     C                   eval      V0Dffd = 'SI '
160400990701     c                   ENDIF
160500051107     c                   IF        TASffd = 'M'
160600051107     C                   eval      V0Dffd = 'SAB'
160700051107     c                   ENDIF
160800990716      * Anteporto
160900990716     C                   movel     tasfap        V0Cfap
161000990716      * Inoltro
161100990716     C                   movel     tasfin        V0Cfin
161200010221      * ORM
161300010221     C                   clear                   v12poe
161400010221     C                   clear                   v12nsr
161500010221     C                   clear                   v12nor
161600010221     C                   clear                   v12nrv
161700111103     c                   if        wta4not<>*blanks
161800111103     C                   movel     Wta4not       dsbl4m
161900111103     C                   move      §b4poe        v12poe
162000111103     C                   move      §b4nsr        v12nsr
162100111103     C                   move      §b4nor        v12nor
162200111103     C                   move      §b4nrv        v12nrv
162300111103     c                   endif
162400990630      *
162500990630     c                   ENDSR
162600990618      *****************************************************************
162700990630      * GESTIONE F2 = Comunicazione Anomalia
162800990618      *****************************************************************
162900990630     C     SUB_F02       BEGSR
163000990618      *
163100990705     c                   SELECT
163200990705      * Prima volta carico subfile
163300990705     c                   WHEN      WSB51D02 = *blanks
163400990705     c                   exsr      RIED02
163500990706     c                   eval      WSB51D02 = 'S'
163600990706      * Nesima volta con una sola C.A.: entrata diretta
163700990706     c                   WHEN      Wunaca = 'S'
163800990706     c                   exsr      RIED02_3
163900990706      * Nesima volta con più C.A.: gestione subfile
164000990705     c                   OTHER
164100990706     c                   exsr      RIED02_2
164200990705     c                   ENDSL
164300990705      *
164400990630     c                   ENDSR
164500990705      **********************************************************************
164600990705      * IMPOSTO CAMPI VIDEO INTERROGAZIONE C.A.
164700990705      **********************************************************************
164800990705     C     RIED02        BEGSR
164900990705      *
165000990705      * Pulizia subfile
165100990705     C                   eval      *in26 = *off
165200990705     C                   WRITE     SB51C11
165300990705     C                   eval      *in25 = *on
165400990705     C                   eval      *in26 = *on
165500990705     C                   CLEAR                   nrr11
165600990705      *
165700990705      * Carico il subfile
165800040809     C**""*Keyspe        setll     FNDCT02L
165900040809     C**""*Keyspe        READE     FNDCT02L
166000040809     C**""*              DOW       %EOF = *off
166100040809     C**""*              IF        DCTATB = *blanks
166200040809     C**""*              Z-ADD     DCTAAC        V11AAC
166300040809     C**""*              Z-ADD     DCTFIL        V11FIL
166400040809     C**""*              Z-ADD     DCTNCA        V11NCA
166500040809     C**""*              z-add     DCTaac        W0080
166600040809     C**""*              movel     DCTmgc        W0080
166700040809     C**""**usa          MOVEL     W0080         DATA_EUR
166800040809     C**""**eur          MOVEL     DATA_EUR      W0080
166900040809     C**""*              eval      V11dca = %EDITC(W0080:'Y')
167000040809     C**""*              Z-ADD     DCTFCA        V11FCA
167100040809     C**""*DCTfca        CHAIN     FNDFA000
167200040809     C**""*              if        %FOUND
167300040809     C**""*              movel     DFADES        V11DFA
167400040809     c**""*              endif
167500990705      *
167600040809     C**""*              add       1             NRR11
167700040809     C**""*              WRITE     SB51S11
167800040809     C**""*              ENDIF
167900990705      *
168000040809     C**""*Keyspe        READE     FNDCT02L
168100040809     C**""*              ENDDO
168200040809      *
168300040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
168400040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
168500040809      * legate alla spedizione che passo
168600040809     c                   clear                   fidn12ds
168700040809     c                   eval      i12aas = I51AAS
168800040809     c                   eval      i12lnp = I51LNP
168900040809     c                   eval      i12nrs = I51NRS
169000040809     c                   eval      i12nsp = I51NSP
169100040809     c                   eval      i12fel = 'E'
169200040809     c                   eval      i12fan = 'E'
169300040809     c*
169400040809     c                   call      'FIDN12R'
169500040809     c                   parm                    fidn12ds
169600040809     c*
169700040809     c* se non ci sono errori
169800040809     c                   if        o12err = ' '
169900040809     c* se numero di CA trovate maggiore di zero
170000040809     c                   if        o12nca > 0
170100040809     c                   z-add     1             ii                2 0
170200040809     c                   dow       ii <= o12nca
170300040809     c                   movea     skKey(ii)     dsKey
170400040809     C                   Z-ADD     dsaac         V11AAC
170500040809     C                   Z-ADD     dsfil         V11FIL
170600040809     C                   Z-ADD     dsnca         V11NCA
170700040809     c*
170800040809     c* Aggancio il FNDCT01L x reperire alcuni campi
170900040809     c     KDCT1_C       chain     FNDCT01L
171000040809     c                   if        %found(FNDCT01L)
171100040809     C                   z-add     DCTaac        W0080
171200040809     C                   movel     DCTmgc        W0080
171300040809     C     *usa          MOVEL     W0080         DATA_EUR
171400040809     C     *eur          MOVEL     DATA_EUR      W0080
171500040809     C                   eval      V11dca = %EDITC(W0080:'Y')
171600040809     c                   endif
171700040809     c*
171800040809     C                   Z-ADD     skfca(ii)     V11FCA
171900040809     C     skfca(ii)     CHAIN     FNDFA000
172000040809     C                   if        %FOUND
172100040809     C                   movel     DFADES        V11DFA
172200040809     c                   endif
172300040809      *
172400040809     C                   add       1             NRR11
172500040809     C                   WRITE     SB51S11
172600040809     c*
172700040809     c                   add       1             ii
172800040809     c                   enddo
172900040809     c*
173000040809     c                   endif
173100040809     c                   endif
173200040809     c**
173300121121      * se NRR11 = 0 vuol dire che non ho più la c.a. è già stata archiviata
173400121121     c                   IF        NRR11 = 0
173500121121     c                   eval      *in28 = *on
173600121121     c                   eval      V1Cmsg = 'C.A. già archiviata'
173700121121     c                   leavesr
173800121121     c                   ENDIF
173900990705      *
174000990705      * Gestione subfile o se ho una sola C.A. entrata diretta
174100990705     c                   IF        NRR11 = 1
174200990705     c                   clear                   fidn00ds
174300990706     c                   eval      Wunaca = 'S'
174400990705     c                   exsr      RIED02_1
174500990705     c                   ELSE
174600990706     c                   exsr      RIED02_2
174700990705     C                   ENDIF
174800990705      *
174900990705     c                   ENDSR
175000990705      **********************************************************************
175100990705      * RICHIAMO PGM DI VISUALIZZAZIONE C.A.
175200990705      **********************************************************************
175300990705     C     RIED02_1      BEGSR
175400990705      *
175500990705      * Clearizzo la DS fuori dalla routine per conservarla in caso di richiamo per una sola C.A.
175600990705     C                   Z-ADD     v11aac        i00aac
175700990705     C                   Z-ADD     V11FIL        I00fil
175800990705     C                   Z-ADD     V11NCA        I00NCA
175900990705     C                   Z-ADD     tasAAS        I00AAS
176000990705     C                   Z-ADD     tasNRS        I00NRS
176100990705     C                   Z-ADD     tasNSP        I00NSP
176200990705     C                   Z-ADD     tasLNP        I00LNP
176300990705     C                   MOVEL     'S'           I00TPB
176400990706      *
176500990706     c                   exsr      RIED02_3
176600990706      *
176700990706     c                   ENDSR
176800990705      *****************************************************************
176900990705      * GESTIONE SUBFILE PER INTERROGAZIONE C.A.
177000990705      **********************************************************************
177100990705     C     RIED02_2      BEGSR
177200990705
177300990705     c                   DO        *hival
177400990706     C                   eval      nrr11 = 1
177500990707     C                   clear                   W001A
177600990705      * Emetto video
177700990706     C                   WRITE     SB51T10
177800990706     C                   WRITE     SB51Z00
177900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
178000990705     C                   EXFMT     SB51C11
178100120302     c                   else
178200120302     c                   write     sb51C11
178300120302     c                   read(E)   tnsb51d
178400120302     c                   if        %error
178500120302     c                   eval      *inkc='1'
178600120302     c                   leave
178700120302     c                   endif
178800120302     c                   endif
178900990706      * Premuto un tasto funzionale
179000990706     C                   IF         *IN27 = *ON
179100990706     c                   leave
179200990706     C                   ENDIF
179300990705      *
179400990705     C                   READC     SB51S11
179500990705     C                   DOW       %EOF = *off
179600990705      *
179700990705     C                   IF        V11SCE = '5'
179800990707     c                   eval      W001A = 'S'
179900990705     C                   clear                   fidn00ds
180000990705     C                   exsr      ried02_1
180100990705     C                   clear                   fidn00ds
180200990705     C                   clear                   V11SCE
180300990705     C                   update    SB51S11
180400990706      *   se premuto F3 esco dal programma
180500990706     c   KC              leave
180600990705     C                   ENDIF
180700990705      *
180800990705     C                   READC     SB51S11
180900990705     C                   ENDDO
181000990706      *   se non effettuate scelte forzo F12
181100990707     c                   If        W001A = 'S'
181200990706     c                   eval       *inkl = *on
181300990706     c                   clear                   INFDSFMT
181400990706     c                   Endif
181500120302
181600990706      *
181700990706     C                   ENDDO
181800990705      *
181900990630     c                   ENDSR
182000990706      *****************************************************************
182100990706      * RICHIAMO E GESTIONE DEL PGM DI INTERROGAZIONE C.A.
182200990706      *****************************************************************
182300990706     C     RIED02_3      BEGSR
182400990706      *
182500990706     C                   movel(P)  fidn00ds      kpjbu
182600990706      *
182700990706     C                   CALL      'FIDN01R'
182800990706     C                   PARM                    KPJBA
182900000309     C                   PARM                    callricors
183000990706      *
183100990706     C                   movel     Kpjbu         fidn00ds
183200990706      *
183300990706      * Se ritorna premuto F3 accendo *INKC
183400990706      * Altrimenti se ho richiamato direttamente la CA ritorno alla videata di apertura
183500990706     C                   SELECT
183600990707     C                   WHEN      O00F03 <> *blanks
183700990706     c                   eval       *inkc = *on
183800990706     c                   WHEN      Wunaca = 'S'
183900990706     c                   eval       *inkl = *on
184000990706     c                   clear                   INFDSFMT
184100990706     c                   OTHER
184200990706     c                   eval       *inkl = *on
184300990707     C                   ENDSL
184400990706      *
184500990706     c                   ENDSR
184600010221      *****************************************************************
184700010221      * GESTIONE F4 = ORM
184800010221      *****************************************************************
184900010221     C     SUB_F04       BEGSR
185000010221      *
185100010221     C                   clear                   PARM01
185200010221     C                   move      v12poe        ppoe
185300010221     C                   move      v12nor        pnor
185400010221     C                   move      v12nsr        pnsr
185500010221     C                   move      v12nrv        pnrv
185600010221     C                   move      '5'           psce
185700010221     C                   move      v0clnp        pfgs
185800010221     C                   movel(p)  PARM01        KPJBU
185900010221
186000010221     C                   call      'FIOR07R'
186100010221     C                   parm                    KPJBA
186200010221     c                   clear                   INFDSFMT
186300010221     c                   eval      *inkl = *on
186400010221      *
186500010221     c                   ENDSR
186600990630      *****************************************************************
186700990705      * GESTIONE F5 = Eventi
186800990630      *****************************************************************
186900990705     C     SUB_F05       BEGSR
187000990705      *
187100990813     c                   clear                   LRB5DS
187200990813     C                   Z-ADD     tasAAS        pb5AAS
187300990813     C                   Z-ADD     tasLNP        pb5LNP
187400990813     C                   Z-ADD     tasNRS        pb5NRS
187500990813     C                   Z-ADD     tasNSP        pb5NSP
187600990813     c                   movel     RSUT          pb5DSA
187700990813     c                   z-add     V0CDSP        pb5DSP
187800990813     c                   z-add     tasRMN        pb5RMN
187900990813     c                   z-add     tasLNA        pb5LNA
188000990813     c                   movel     V0DLNA        pb5LNAD
188100990813     c                   movel     tasRSD        pb5RSD
188200990813     c                   z-add     Wtasdcp       pb5DCP
188300990813      *
188400990813     C                   MOVEL(P)  LRB5DS        KPJBU
188500990813     C                   CALL      'FNLRB5R'
188600990813     C                   PARM                    KPJBA
188700990813     C                   MOVEL     KPJBU         LRB5DS
188800990813      *
188900990813      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
189000990813     C                   IF        pb5F03 = '1'
189100990813     c                   eval      *inkc = *on
189200990813     c                   ELSE
189300990813     c                   clear                   INFDSFMT
189400990813     c                   eval      *inkl = *on
189500990813     C                   ENDIF
189600990705      *
189700990705     c                   ENDSR
189800990705      *****************************************************************
189900990705      * GESTIONE F7 = Altri dati
190000990705      *****************************************************************
190100990705     C     SUB_F07       BEGSR
190200990705
190300990630      * Per il primo richiamo imposto i campi del video
190400990630     c                   IF        WSB51D07 = *blanks
190500990630     c                   exsr      ried07
190600990702     c                   eval      WSB51D07 = 'S'
190700990630     c                   ENDIF
190800990630
190900990707     c                   DO        *hival
191000990707
191100990630     c                   write     SB51T01
191200990630     c                   write     SB51Z00
191300120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
191400990630     c                   exfmt     SB51D07
191500120302     c                   else
191600120302     c                   write     sb51d07
191700120302     c                   read(E)   tnsb51d
191800120302     c                   if        %error
191900120302     c                   eval      *inkc='1'
192000120302     c                   leave
192100120302     c                   endif
192200120302     c                   endif
192300990707
192400990707     C                   clear                   W001A
192500120711      *
192600120711      * Interrogazione restituzione documenti al mittente
192700120711     c                   IF        *in67 and V7Idoc = '?'
192800120711     c                   exsr      sr_w02
192900120711     C                   clear                   V7Idoc
193000120711     C                   eval      W001A = 'S'
193100120711     C                   ENDIF
193200990630
193300990707      * Decodifica particolarità Giacenza
193400990707     c                   IF        V7Igga = '?'
193500990707     C                   clear                   ds7pqrs
193600990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
193700990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
193800990707     C                   movel     V7Cgga        DS7COD                         CODICE
193900990707     C                   movel(P)  DS7PQRS       KPJBU
194000990707     C                   CALL      'TRTB69R'
194100990707     C                   PARM                    KPJBA
194200990707     C                   clear                   V7Igga
194300990707     C                   eval      W001A = 'S'
194400990707     C                   ENDIF
194500990707
194600990707      * Decodifica particolarità Consegna
194700990707     c                   IF        V7Igma = '?'
194800990707     C                   clear                   ds7pqrs
194900990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
195000990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
195100990707     C                   movel     V7Cgma        DS7COD                         CODICE
195200990707     C                   movel(P)  DS7PQRS       KPJBU
195300990707     C                   CALL      'TRTB70R'
195400990707     C                   PARM                    KPJBA
195500990707     C                   clear                   V7Igma
195600990707     C                   eval      W001A = 'S'
195700990707     C                   ENDIF
195800990707
195900990707      * Decodifica particolarità Varie
196000990707     c                   IF        V7Igva = '?'
196100990707     C                   clear                   ds7pqrs
196200990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
196300990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
196400990707     C                   movel     V7Cgva        DS7COD                         CODICE
196500990707     C                   movel(P)  DS7PQRS       KPJBU
196600990707     C                   CALL      'TRTB73R'
196700990707     C                   PARM                    KPJBA
196800990707     C                   clear                   V7Igva
196900990707     C                   eval      W001A = 'S'
197000990707     C                   ENDIF
197100990707
197200990707     c                   IF        W001A = *blanks
197300990707     c                   leave
197400990707     C                   ENDIF
197500990707
197600990707     c                   ENDDO
197700990707
197800990630     c                   ENDSR
197900990630      **********************************************************************
198000990630      * IMPOSTO CAMPI VIDEO ALTRI DATI
198100990630      **********************************************************************
198200990630     C     RIED07        BEGSR
198300990708      *
198400990630      * Codice bolla
198500990702     c                   IF        TASCBO <> *blanks
198600990630     c                   movel     tascbo        V7Ccbo
198700990630     C                   eval      KCOD = '3A'
198800990630     C                   movel(P)  tascbo        KKEY
198900990630     C                   EXSR      CHKTAB
199000990702     C  n30              movel     Wtbluni       V7Dcbo
199100990702     c                   ENDIF
199200990708      * P.O. Partenza
199300990708     c                   z-add     V0Clnp        V7Clnp
199400990708     c                   z-add     V0Clnp        Wlinea
199500990708     C                   EXSR      CHKORG
199600990708     C                   movel     orgdes        V7Dlnp
199700990708      * Terminal Partenza
199800990708     c                   IF        TAStfp <> *zeros
199900990708     c                   z-add     TAStfp        V7Ctfp
200000990708     c                   z-add     tastfp        Wlinea
200100990708     C                   EXSR      CHKORG
200200990708     C                   movel     orgdes        V7Dtfp
200300990708     c                   ENDIF
200400990708      * Terminal Arrivo
200500990708     c                   IF        TAStfa <> *zeros
200600990708     c                   z-add     TAStfa        V7Ctfa
200700990708     c                   z-add     tastfa        Wlinea
200800990708     C                   EXSR      CHKORG
200900990708     C                   movel     orgdes        V7Dtfa
201000990708     c                   ENDIF
201100990708      * 1o P.O. Transito
201200040603     c**!!!              IF        TASFLP <> *zeros
201300040603     c**!!!              eval      *in48 = *ON
201400040603     c**!!!              z-add     TASflp        V7Cflp
201500040603     c**!!!              z-add     tasflp        Wlinea
201600040603     C**!!!              EXSR      CHKORG
201700040603     C**!!!              movel     orgdes        V7Dflp
201800040603     c**!!!              ENDIF
201900990708      * 2o P.O. Transito, se non presente non emetto scritta
202000040603     c**!!!              IF        TASFL2 <> *zeros
202100040603     c**!!!              eval      *in49 = *ON
202200040603     c**!!!              z-add     TASfl2        V7Cfl2
202300040603     c**!!!              z-add     tasfl2        Wlinea
202400040603     C**!!!              EXSR      CHKORG
202500040603     C**!!!              movel     orgdes        V7Dfl2
202600040603     c**!!!              ENDIF
202700990630      * Sigla Operatore
202800990630     C                   movel     tassop        V7Csop
202900990630      * Trattamento merce
203000990630     C                   movel     tasctm        V7Cctm
203100990630      * Stampa lettera di vettura
203200990630     C                   SELECT
203300991123     C                   WHEN      TASLL1 = *blanks
203400990630     C                   WHEN      TASLL1 = 'C'  or  TASLL1 = 'K'
203500990630     C                   eval      V7Dll1 = 'CLI'
203600990630     C                   WHEN      TASLL1 = 'S'  or  TASLL1 = 'Y'
203700990630     C                   eval      V7Dll1 = 'PAR'
203800990630     c                   OTHER
203900990630     C                   eval      V7Dll1 = 'ARR'
204000990630     C                   ENDSL
204100110610      * email destinatario
204200110610     c                   setoff                                       65
204300110610      *
204400130926If  1c                   If        §floemd<>*blanks
204500110610     C                   SETON                                        65
204600130927     c                   select
204700130927     c                   when      §floemd='E'
204800130927     c                   Eval      V7cemd = 'MAIL+SMS'
204900130927     c                   when      §floemd='S'
205000130927     c                   Eval      V7cemd = 'MAIL    '
205100130927     c                   when      §floemd='X'
205200130927     c                   Eval      V7cemd = 'SMS     '
205300130927     c                   endsl
205400110610    1c                   EndIf
205500990630      * Bolla rientro
205600990630     C                   IF        TASfbr = 'S'
205700990630     C                   eval      V7Dfbr = 'SI'
205800990630     C                   ENDIF
205900990630      * Franco Frontiera
206000990630     C                   IF        TASfbr = 'F'
206100990630     C                   eval      V7Dffr = 'SI'
206200990630     C                   ENDIF
206300990630      * BAM si/no
206400991123     C                   SELECT
206500991123     C                   WHEN      TASLL1 = *blanks
206600991123     C                   WHEN      TASLL1 = 'C'  or  TASLL1 = 'S'
206700990630     C                   eval      V7Dddt = 'SI'
206800991123     C                   OTHER
206900990630     C                   eval      V7Dddt = 'NO'
207000991123     C                   ENDSL
207100990630      * Emessa Nota Accredito
207200990630     C                   IF        TASfa2 = 'S'
207300990630     C                   eval      V7Dfa2 = 'SI'
207400990630     C                   ENDIF
207500050620      * note partenza
207600050621     c                   setoff                                       58
207700050620     c                   eval      ktrc = '8'
207800050620     c                   clear                   v7dnt1
207900050620     c     KTAStrc       CHAIN     TITA430C
208000050620     c                   If        %FOUND
208100050620     c                   movel     TA4not        v7dnt1
208200050621     c                   seton                                        58
208300050620     c                   ENDIF
208400050620     c                   eval      ktrc = '9'
208500050620     c                   clear                   v7dnt2
208600050620     c     KTAStrc       CHAIN     TITA430C
208700050620     c                   If        %FOUND
208800050620     c                   movel     TA4not        v7dnt2
208900050621     c                   seton                                        58
209000050620     c                   ENDIF
209100990630      * Importo da assicurare e flag tipo inserimento
209200990630     c                   IF        TASias > *zeros
209300990630     c                   z-add     TASias        V7Cias
209400990630     c                   If        TASvas = *blanks
209500990630     c                   movel     'ITL'         V7Cvas
209600990630     c                   else
209700990630     c                   movel     TASvas        V7Cvas
209800990630     C                   Endif
209900990720     c                   eval      *IN50 = (TASfcm <> *blanks)
210000990630     C                   ENDIF
210100990630      * Particolarità
210200990630     c                   movel     TASgga        V7Cgga
210300990707     c                   eval      *in45 = (V7Cgga <> *blanks)
210400990630     c                   movel     TASgma        V7Cgma
210500990707     c                   eval      *in46 = (V7Cgma <> *blanks)
210600990630     c                   movel     TASgva        V7Cgva
210700990712     c                   move      ' '           V7Cgva
210800990707     c                   eval      *in47 = (V7Cgva <> *blanks)
210900990630      *
211000120330      * decopdifico motivo DDt errat
211100990630     c                   ENDSR
211200990630      *****************************************************************
211300000901      * GESTIONE F08 = Cod."CHI SONO"
211400990630      *****************************************************************
211500000901     C     SUB_F08       BEGSR
211600000901
211700000901      * Per il primo richiamo imposto i campi del video
211800000901     c                   IF        WSB51D08 = *blanks
211900000901     c                   exsr      ried08
212000000901     c                   eval      WSB51D08 = 'S'
212100000901     c                   ENDIF
212200000901
212300000901     c                   z-add     1             nrr08
212400000901
212500000901     C                   WRITE     SB51T10
212600000901     C                   WRITE     SB51Z00
212700120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
212800000901     C                   EXFMT     SB51C08
212900120302     c                   else
213000120302     c                   write     sb51c08
213100120302     c                   read(E)   tnsb51d
213200120302     c                   if        %error
213300120302     c                   eval      *inkc='1'
213400120302     c                   endif
213500120302     c                   endif
213600000901
213700000901     C                   ENDSR
213800000901      *****************************************************************
213900000901      * IMPOSTO CAMPI VIDEO CODICE "CHI SONO"
214000000901      *****************************************************************
214100000901     C     RIED08        BEGSR
214200000901
214300000901      *
214400000901      * Pulizia subfile
214500000901     C                   eval      *in26 = *off
214600000901     C                   WRITE     SB51C08
214700000901     C                   eval      *in25 = *on
214800000901     C                   eval      *in26 = *on
214900000901     C                   clear                   nrr08
215000000901      *
215100020226     c                   eval      wtahtrc = 'C'
215200020226     C     KEYspeh       SETLL     TITAH30C
215300020226     C     KEYspeh       READE     TITAH30C
215400000901      *
215500000901     C                   DOW       %EOF = *off
215600000901     c                   move      tahnot        V08not
215700010524     c                   z-add     tahnsc        v08nsc
215800000901     C                   move      '7H'          kcod
215900000901     C                   movel(p)  tahtrc        kkey
216000000901     C     KEYTAB        CHAIN     TABEL00F                           30
216100000901     c                   IF        *IN30 = *ON
216200000901     C                   else
216300000901     C                   ENDIF
216400000901      *
216500000901     c                   ADD       1             nrr08
216600000901     c                   write     SB51s08
216700000901      *
216800020226     C     KEYspeh       READE     TITAH30C
216900000901     c                   ENDDO
217000000901
217100000901     c                   ENDSR
217200040603      *****************************************************************
217300040603      * GESTIONE F09 = Transiti
217400040603      *****************************************************************
217500040603     C     SUB_F09       BEGSR
217600040603
217700040603      * Per il primo richiamo imposto i campi del video
217800040603     c                   IF        WSB51D09 = *blanks
217900040603     c                   exsr      ried09
218000040603     c                   eval      WSB51D09 = 'S'
218100040603     c                   ENDIF
218200040603
218300040603     c                   z-add     1             nrr09
218400040603
218500040603     C                   WRITE     SB51T10
218600040603     C                   WRITE     SB51Z00
218700120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
218800040603     C                   EXFMT     SB51C09
218900120302     c                   else
219000120302     c                   write     sb51c09
219100120302     c                   read(E)   tnsb51d
219200120302     c                   if        %error
219300120302     c                   eval      *inkc='1'
219400120302     c                   endif
219500120302     c                   endif
219600040603
219700040603     C                   ENDSR
219800040603      *****************************************************************
219900040603      * IMPOSTO CAMPI VIDEO TRANSITI
220000040603      *****************************************************************
220100040603     C     RIED09        BEGSR
220200040603
220300040603      * Pulizia subfile
220400040603     c                   eval      *in26 = *off
220500040603     c                   WRITE     SB51C09
220600040603     c                   eval      *in25 = *on
220700040603     c                   eval      *in26 = *on
220800040603     c                   clear                   nrr09
220900040603
221000040603      * Transiti da Fiar5 rcd TRS
221100040604     c                   Eval      woktrs = *Off
221200040603     c                   Eval      wAr5Trd = 'TRS'
221300040604     c     kSpeAr5       Setgt     Fiar531c
221400040604     c                   Do        *Hival
221500040604     c     kSpeAr5       Readpe    Fiar531c
221600040604     c                   If        %Eof(Fiar531c)
221700040604     c                   Leave
221800040604     c                   EndIf
221900040604     c                   Eval      woktrs = *On
222000040603     c                   Eval      dAr5Trs = Ar5Uni
222100040603      * 1° p.o. Transito
222200040603     c                   If        §Ar5Flp > *Zeros
222300040603     c                   Eval      V09Fil = §Ar5Flp
222400040603     c                   Z-add     §Ar5Flp       Wlinea
222500040603     c                   ExSr      CHKORG
222600040603     c                   Eval      V09Dfl = OrgDes
222700040603     c                   If        §Ar5Det > *Zeros
222800040604     c     *iso          Move      §Ar5Det       DATA_eur
222900040603     c     *eur          Move      DATA_eur      V09Det
223000040608     c                   Eval      *In48 = *Off
223100040603     c                   Else
223200040603     c                   Clear                   V09Det
223300040608     c                   Eval      *In48 = *On
223400040603     c                   EndIf
223500040603     c                   If        §Ar5Dut > *Zeros
223600040604     c     *iso          Move      §Ar5Dut       DATA_eur
223700040603     c     *eur          Move      DATA_eur      V09Dut
223800040608     c                   Eval      *In49 = *Off
223900040603     c                   Else
224000040603     c                   Clear                   V09Dut
224100040608     c                   Eval      *In49 = *On
224200040603     c                   EndIf
224300040603     c                   Add       1             nrr09
224400040603     c                   Write     SB51s09
224500040603     c                   EndIf
224600040603      * 2° p.o. Transito
224700040603     c                   If        §Ar5Fl2 > *Zeros
224800040603     c                   Eval      V09Fil = §Ar5Fl2
224900040603     c                   Z-add     §Ar5Fl2       Wlinea
225000040603     c                   ExSr      CHKORG
225100040603     c                   Eval      V09Dfl = OrgDes
225200040727     c                   If        §Ar5De2 > *Zeros
225300040604     c     *iso          Move      §Ar5De2       DATA_eur
225400040603     c     *eur          Move      DATA_eur      V09Det
225500040608     c                   Eval      *In48 = *Off
225600040603     c                   Else
225700040603     c                   Clear                   V09Det
225800040608     c                   Eval      *In48 = *On
225900040603     c                   EndIf
226000040727     c                   If        §Ar5Du2 > *Zeros
226100040604     c     *iso          Move      §Ar5Du2       DATA_eur
226200040603     c     *eur          Move      DATA_eur      V09Dut
226300040608     c                   Eval      *In49 = *Off
226400040603     c                   Else
226500040603     c                   Clear                   V09Dut
226600040608     c                   Eval      *In49 = *On
226700040603     c                   EndIf
226800040603     c                   Add       1             nrr09
226900040603     c                   Write     SB51s09
227000040603     c                   EndIf
227100040603      * 3° p.o. Transito
227200040603     c                   If        §Ar5Fl3 > *Zeros
227300040603     c                   Eval      V09Fil = §Ar5Fl3
227400040603     c                   Z-add     §Ar5Fl3       Wlinea
227500040603     c                   ExSr      CHKORG
227600040603     c                   Eval      V09Dfl = OrgDes
227700040727     c                   If        §Ar5De3 > *Zeros
227800040604     c     *iso          Move      §Ar5De3       DATA_eur
227900040603     c     *eur          Move      DATA_eur      V09Det
228000040608     c                   Eval      *In48 = *Off
228100040603     c                   Else
228200040603     c                   Clear                   V09Det
228300040608     c                   Eval      *In48 = *On
228400040603     c                   EndIf
228500040727     c                   If        §Ar5Du3 > *Zeros
228600040604     c     *iso          Move      §Ar5Du3       DATA_eur
228700040603     c     *eur          Move      DATA_eur      V09Dut
228800040608     c                   Eval      *In49 = *Off
228900040603     c                   Else
229000040603     c                   Clear                   V09Dut
229100040608     c                   Eval      *In49 = *On
229200040603     c                   EndIf
229300040608     c                   Add       1             nrr09
229400040603     c                   Write     SB51s09
229500040603     c                   EndIf
229600040603      * 4° p.o. Transito
229700040603     c                   If        §Ar5Fl4 > *Zeros
229800040603     c                   Eval      V09Fil = §Ar5Fl4
229900040603     c                   Z-add     §Ar5Fl4       Wlinea
230000040603     c                   ExSr      CHKORG
230100040603     c                   Eval      V09Dfl = OrgDes
230200040727     c                   If        §Ar5De4 > *Zeros
230300040604     c     *iso          Move      §Ar5De4       DATA_eur
230400040603     c     *eur          Move      DATA_eur      V09Det
230500040608     c                   Eval      *In48 = *Off
230600040603     c                   Else
230700040603     c                   Clear                   V09Det
230800040608     c                   Eval      *In48 = *On
230900040603     c                   EndIf
231000040727     c                   If        §Ar5Du4 > *Zeros
231100040604     c     *iso          Move      §Ar5Du4       DATA_eur
231200040603     c     *eur          Move      DATA_eur      V09Dut
231300040608     c                   Eval      *In49 = *Off
231400040603     c                   Else
231500040603     c                   Clear                   V09Dut
231600040608     c                   Eval      *In49 = *On
231700040603     c                   EndIf
231800040603     c                   Add       1             nrr09
231900040603     c                   Write     SB51s09
232000040603     c                   EndIf
232100040603      * 5° p.o. Transito
232200040603     c                   If        §Ar5Fl5 > *Zeros
232300040603     c                   Eval      V09Fil = §Ar5Fl5
232400040603     c                   Z-add     §Ar5Fl5       Wlinea
232500040603     c                   ExSr      CHKORG
232600040603     c                   Eval      V09Dfl = OrgDes
232700040727     c                   If        §Ar5De5 > *Zeros
232800040604     c     *iso          Move      §Ar5De5       DATA_eur
232900040603     c     *eur          Move      DATA_eur      V09Det
233000040608     c                   Eval      *In48 = *Off
233100040603     c                   Else
233200040603     c                   Clear                   V09Det
233300040608     c                   Eval      *In48 = *On
233400040603     c                   EndIf
233500040727     c                   If        §Ar5Du5 > *Zeros
233600040604     c     *iso          Move      §Ar5Du5       DATA_eur
233700040603     c     *eur          Move      DATA_eur      V09Dut
233800040608     c                   Eval      *In49 = *Off
233900040603     c                   Else
234000040603     c                   Clear                   V09Dut
234100040608     c                   Eval      *In48 = *On
234200040603     c                   EndIf
234300040603     c                   Add       1             nrr09
234400040603     c                   Write     SB51s09
234500040603     c                   EndIf
234600040604     c                   Leave
234700040604     c                   EndDo
234800040603
234900040603      * Transiti da TITAS
235000040604     c                   If        woktrs = *Off
235100040603      * 1° p.o. Transito
235200040603     c                   If        TasFlp > *Zeros
235300040603     c                   Eval      V09Fil = TasFlp
235400040603     c                   Z-add     TasFlp        Wlinea
235500040603     c                   ExSr      CHKORG
235600040603     c                   Eval      V09Dfl = OrgDes
235700040603     c                   Clear                   V09Det
235800040603     c                   Clear                   V09Dut
235900040608     c                   Eval      *In48 = *On
236000040608     c                   Eval      *In49 = *On
236100040603     c                   Add       1             nrr09
236200040603     c                   Write     SB51s09
236300040603     c                   EndIf
236400040603      * 2° p.o. Transito
236500040603     c                   If        TasFl2 > *Zeros
236600040603     c                   Eval      V09Fil = TasFl2
236700040603     c                   Z-add     TasFl2        Wlinea
236800040603     c                   ExSr      CHKORG
236900040603     c                   Eval      V09Dfl = OrgDes
237000040603     c                   Clear                   V09Det
237100040603     c                   Clear                   V09Dut
237200040608     c                   Eval      *In48 = *On
237300040608     c                   Eval      *In49 = *On
237400040603     c                   Add       1             nrr09
237500040603     c                   Write     SB51s09
237600040603     c                   EndIf
237700040603     c                   EndIf
237800040603
237900040603     C                   ENDSR
238000000901      *****************************************************************
238100000901      * GESTIONE F10 = Variazioni
238200000901      *****************************************************************
238300000901     C     SUB_F10       BEGSR
238400990804      *
238500990804     c                   clear                   TNSB01DS
238600990804     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
238700990804     C                   MOVEL     'F10'         D01OP1                         *LIVELLO PROGRAMMA
238800990804     C                   Z-ADD     tasAAS        d01AAS                         *ANNO
238900990804     C                   Z-ADD     tasLNP        d01LNP                         *LNP
239000020402     C                   Z-ADD     tasLNA        d01LNA                         *LNA
239100990804     C                   Z-ADD     tasNRS        d01NRS                         *SERIE
239200990804     C                   Z-ADD     tasNSP        d01NSP                         *SPEDIZIONE
239300990804     C                   Z-ADD     V0CDSP        D01DSP                         * GG/MM/AAAA
239400990804     C                   MOVEL     '0'           D01F03                         *USCITA PGM
239500990804     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
239600990804     C                   MOVEL     '0'           D01ERR                         *ERRORE
239700990804      *
239800990804     C                   MOVEL     TNSB01DS      KPJBU
239900990804      *
239901170118     c***                if        knmus<>'EDPRM'
240000170118     C***                CALL      'TNSB53R'
240100170118     C***                PARM                    KPJBA
240200990804      *
240300170118     C***                MOVEL     KPJBU         TNSB01DS
240400990804      *
240500990804      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
240600170118     C***                IF        D01F03 = '1'
240700170118     c***                eval      *inkc = *on
240800170118     c***                ELSE
240900170118     c***                clear                   INFDSFMT
241000170118     c***                eval      *inkl = *on
241100170118     C***                ENDIF
241101170118     c***                else
241102160930     c                   clear                   lr75ds
241104160930     c                   eval      p75aas=tasaas
241105160930     c                   eval      p75lnp=taslnp
241106160930     c                   eval      p75nrs=tasnrs
241107160930     c                   eval      p75nsp=tasnsp
241111160930     c                   eval      p75DSA=rsut
241112160930     c                   eval      P75TBO='S'
241113160930     c                   eval      p75tbl=tastbl
241114160930     c                   eval      kpjbu=lr75ds
241117160930     c                   call      'FNLR75R'
241118160930     C                   PARM                    KPJBA
241119160930      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
241120160930     C                   IF        p75F03 = '1'
241121160930     c                   eval      *inkc = *on
241122160930     c                   ELSE
241123160930     c                   clear                   INFDSFMT
241124160930     c                   eval      *inkl = *on
241125160930     C                   ENDIF
241126170118     c***                endif
241200990630      *
241300990630     c                   ENDSR
241400030120      *****************************************************************
241500030120      * GESTIONE F11 = Note Supermercato
241600030120      *****************************************************************
241700030120     C     SUB_F11       BEGSR
241800030120      *
241900030121     c                   Clear                   FnLrb3Ds
242000030122     c                   Movel     RSUT          Rb3Rsu
242100030121     c                   Eval      Rb3Fil = Simfel
242200030121     c                   Eval      Rb3Aas = TasAas
242300030121     c                   Eval      Rb3Lnp = TasLnp
242400030121     c                   Eval      Rb3Nrs = TasNrs
242500030121     c                   Eval      Rb3Nsp = TasNsp
242600030127     c                   Z-Add     TasAas        Rb3Dsp
242700030127     c                   movel     TasMgs        Rb3Dsp
242800030127     c     *usa          move      Rb3Dsp        DATA_eur
242900030127     c     *eur          move      DATA_eur      Rb3Dsp
243000030121     c                   Eval      Rb3Rmn = TasRmn
243100030121     c                   Eval      Rb3Lna = TasLna
243200030121     c                   Eval      Rb3Rsd = TasRsd
243300030121     c                   Eval      Rb3Pkf = TasPkf
243400030121     c                   Eval      Rb3Ncl = TasNcl
243500030121     c                   Eval      Rb3Vlb = TasVlb
243600030703      * bancali - part.varie "B"
243700030703If  0c                   If        %subst(TasGva:1:1) = 'B'
243800030121     c                   Eval      Rb3Tba = §Ar5Tba
243900030121     c                   Eval      Rb3Nba = V0cNba
244000030121     c                   Eval      Rb3Tb2 = §Ar5Tb2
244100030121     c                   Eval      Rb3Nb2 = V0cNb2
244200030703     c                   Eval      Rb3Gva = TasGva
244300030703    0c                   EndIf
244400030703If  0c                   If        %subst(TasGva:1:1) = 'O'
244500030703     c                   Eval      Rb3Tba = §Ar5bTba
244600030703     c                   Eval      Rb3Nba = V0cNba
244700030703     c                   Eval      Rb3Tb2 = §Ar5bTb2
244800030703     c                   Eval      Rb3Nb2 = V0cNb2
244900030703     c                   Eval      Rb3Gva = TasGva
245000030703     c                   Eval      Rb3Csf = §Ar5bCsf
245100030703     c                   Eval      Rb3Cor = §Ar5bCor
245200030703    0c                   EndIf
245300030121      * richiamo PGM per note supermercati
245400030121     c                   Call      'FNLRB3R'
245500030122     c                   Parm                    Kpjba
245600030121     c                   Parm                    Fnlrb3Ds
245700030121
245800030121     c                   clear                   INFDSFMT
245900030121     c                   eval      *inkl = *on
246000030121
246100030121     c                   EndSr
246200990630      *****************************************************************
246300990630      * GESTIONE F13 = Tassazione
246400990630      *****************************************************************
246500990630     C     SUB_F13       BEGSR
246600990701
246700990701      * Per il primo richiamo imposto i campi del video
246800990701     c                   IF        WSB51D13 = *blanks
246900990701     c                   exsr      ried13
247000990701     c                   eval      WSB51D13 = 'S'
247100990701     c                   ENDIF
247200990701
247300990705     c                   z-add     1             nrr13
247400160621
247500160621     c                   DO        *hival
247600990702
247700990701     c                   write     SB51T10
247800990701     c                   write     SB51Z00
247900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
248000990701     c                   exfmt     SB51C13
248100120302     c                   else
248200120302     c                   write     sb51c13
248300120302     c                   read(E)   tnsb51d
248400120302     c                   if        %error
248500120302     c                   eval      *inkc='1'
248600160621     c                   leave
248700120302     c                   endif
248800120302     c                   endif
248900160621
249000160621     c                   clear                   w001a
249100160621
249200160621      * '?' su Ottico
249300160621     c                   IF        *in72 and V13ottico = '?'
249400160621     c                   exsr      VisFattura
249500160621     c                   clear                   V13ottico
249600160621     c                   eval      w001a = 'S'
249700160621     c                   ENDIF
249800160621
249900160621     c                   IF        w001a = *blanks
250000160621     c                   leave
250100160621     c                   ENDIF
250200160621
250300160621     c                   ENDDO
250400990701
250500990630     c                   ENDSR
250600990701      **********************************************************************
250700990701      * IMPOSTO CAMPI VIDEO TASSAZIONE
250800990701      **********************************************************************
250900990701     C     RIED13        BEGSR
251000070524
251100070524     c                   eval      *in61 = *off
251200070524     c                   eval      *in62 = *off
251300070524     c                   eval      *in63 = *off
251400151020     c                   clear                   v13peso
251500160621     c                   eval      *in72 = *off
251600990701      *
251700990702      * Fatturata con tariffa Mittente
251800990702     c                   IF        TASftm = 'S'
251900990702     c                   eval      V13ftm = 'SI'
252000990702     c                   ENDIF
252100051103      * Peso fatturato
252200051103      * se bolla fatturata imposto il peso da fatturare che visualizzo in prima videata
252300151016     c                   If        TasDft > *Zeros  or
252400151016     c                             TASimv > *Zeros
252500051103     c                   Eval      v13peso = v0cpeso
252600151016     c*//                Else
252700151016      *//* se bolla da fatturare imposto taspkf
252800151016     c*//                Eval      v13peso = taspkf
252900051103     c                   EndIf
253000990702      * Peso tassabile
253100990702     c                   z-add     Taspvl        v13pvl
253200990702      * Quantità da fatturare
253300990702     c                   z-add     Tasqft        v13qft
253400990702      * Nazione e CAP fatturazione Mittente
253500990702     c                   movel     tasnzm        V13nzm
253600990702     c                   movel     tascam        V13cam
253700990702      * Codice tassazione Mittente
253800990702     c                   IF        TASmct <> *blanks
253900990702     c                   movel     tasmct        V13mct
254000990702     C                   eval      KCOD = 'CT'
254100990702     C                   movel(P)  tasmct        KKEY
254200990702     C                   EXSR      CHKTAB
254300990702     C  n30              movel     Wtbluni       V13dmt
254400990702     c                   ENDIF
254500990702      * Codice tassazione Destinatario
254600990702     c                   IF        TAScts <> *blanks
254700990702     c                   movel     tascts        V13cts
254800990702     C                   eval      KCOD = 'CT'
254900990702     C                   movel(P)  tascts        KKEY
255000990702     C                   EXSR      CHKTAB
255100990702     C  n30              movel     Wtbluni       V13dct
255200990702     c                   ENDIF
255300990702      * Codice tariffa
255400990702     c                   z-add     tasctr        v13ctr
255500990702     C                   eval      KCOD = 'TR'
255600990702     C                   movel     tasctr        w001a
255700990702     C                   movel(P)  W001A         KKEY
255800990702     C                   EXSR      CHKTAB
255900990702     C  n30              movel     Wtbluni       V13dtr
256000160621      * Ottico
256100160830      * per ora SOLO EDP (per tutti dal 1° Settembre 2016)
256200160621      * solo se bolla contabilizzata
256300160621      * se c'è fattura
256400160621      * se non è una distinta
256500160621      * e se tipo bolla che genera fattura
256600160621     c                   IF        TASfic <> *blanks and
256700160621     c                             TASnft > 0 and TASdft > 0 and
256800160621     c                             TASnft <> *all'9' and
256900160830     c                             §TBfcb = '1'
257000160830     c***                          %subst(knmus:1:3) = 'EDP'
257100160621     c                   eval      *in72 = *on
257200160621     c                   ENDIF
257300990702      * Divisa
257400990702     c                   movel     tasdiv        v13div
257500990708      * Importo fattura se fattura immediata
257600990708     c                   IF        *IN41 = *off
257700990708     c                   movel     V1Cimp        v13ift
257800990708     C                   ENDIF
257900990702      * Porto
258000990702     c                   IF        TASpor > *zeros
258100990702     c                   z-add     TASpor        W0133
258200990702     c                   movel     W§cvfdc       Wtstdec
258300990702     c                   exsr      EDTimporti
258400990702     c                   movel     IMPeditato    v13por
258500070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
258600070524     c                   eval      Ksvt = ' '
258700070524     c     Ktitai        chain     titai01l
258800070524     c                   if        %found(titai01l)
258900070524     c                   eval      *in61 = *on
259000070524     c                   endif
259100070524
259200990702     c                   ENDIF
259300990702      * Imponibile
259400990702     c                   IF        TASimv > *zeros
259500990702     c                   z-add     TASimv        W0133
259600990702     c                   movel     W§cvfdc       Wtstdec
259700990702     c                   exsr      EDTimporti
259800990702     c                   movel     IMPeditato    v13imv
259900070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
260000070524     c                   eval      Ksvt = '£'
260100070524     c     Ktitai        chain     titai01l
260200070524     c                   if        %found(titai01l)
260300070524     c                   eval      *in62 = *on
260400070524     c                   endif
260500990702     c                   ENDIF
260600990702      * Esenzione IVA
260700990702     c                   IF        TASfei <> *blanks
260800990702     c                   movel     tasfei        v13fei
260900990702     C                   eval      KCOD = 'EI'
261000990702     C                   movel(P)  tasfei        KKEY
261100990702     C                   EXSR      CHKTAB
261200990702     C  n30              movel     Wtbluni       V13dei
261300990702     c                   ENDIF
261400990702      * Recupero codice varia IVA
261500990702     C                   eval      KCOD = '$2'
261600990702     C                   movel(P)  1             KKEY
261700990702     C                   EXSR      CHKTAB
261800990702     C  n30              movel     Wtbluni       DS$2
261900990702      *
262000990702      * VARIE
262100990702      *    pulizia subfile
262200990702     C                   eval         *IN26 = *OFF
262300990702     C                   WRITE     SB51C13
262400990702     C                   eval         *IN25 = *ON
262500990702     C                   eval         *IN26 = *ON
262600990702      *    varia 1 TAS
262700990702     c                   IF        tassv1 <> *blanks
262800990702     c                   eval      v13svn = tassv1
262900990702     c                   z-add     TASva1        W0133
263000990702     c                   exsr      AGGIOSBF
263100990702     c                   ENDIF
263200990702      *    varia 2 TAS
263300990702     c                   IF        tassv2 <> *blanks
263400990702     c                   eval      v13svn = tassv2
263500990702     c                   z-add     TASva2        W0133
263600990702     c                   exsr      AGGIOSBF
263700990702     c                   ENDIF
263800990702      *    varia 3 TAS
263900990702     c                   IF        tassv3 <> *blanks
264000990702     c                   eval      v13svn = tassv3
264100990702     c                   z-add     TASva3        W0133
264200990702     c                   exsr      AGGIOSBF
264300990702      *    carico ulteriori varie
264400990702     C     keyspe1       setll     tita730c
264500990702     C     keyspe1       reade     tita730c
264600990705     C                   DOW       %EOF = *off
264700990702     c                   IF        TA7svn <> *blanks
264800990702     c                   eval      v13svn = ta7svn
264900990702     c                   z-add     TA7van        W0133
265000990702     c                   exsr      AGGIOSBF
265100990702     c                   ENDIF
265200990702     C     keyspe1       reade     tita730c
265300990702     c                   ENDDO
265400990702      *
265500990702     c                   ENDIF
265600990705      *    se non ci sono varie carico 1 rcd vuoto
265700990705     c                   IF        NRR13 = *ZEROS
265800990708     C                   clear                   v13svn
265900990708     C                   clear                   V13dsv
266000990708     c                   clear                   v13van
266100990705     c                   add       1             nrr13
266200990705     c                   write     sb51s13
266300990705     c                   ENDIF
266400990705      *
266500990702     c                   ENDSR
266600990702      *****************************************************************
266700990702      * CARICO SUBFILE VARIE
266800990702      *****************************************************************
266900990702     C     AGGIOSBF      BEGSR
267000990702      *
267100990702     c                   movel     W§cvfdc       Wtstdec
267200990702     c                   exsr      EDTimporti
267300990702      *
267400990702      * La varia IVA la scrivo nel control e non nel subfile
267500990702     C                   IF        §$2iva = v13SVN
267600990702     c                   movel     IMPeditato    v13iva
267700990702     c                   ELSE
267800990705     c                   movel     IMPeditato    v13van
267900990702      *
268000990702     C                   eval      KCOD = 'CC'
268100990702     C                   movel(P)  'VARIE'       KKEY
268200990702     C                   move      v13svn        KKEY
268300990702     C                   EXSR      CHKTAB
268400020328     C                   movel     TBLuni        DSCC
268500020328     C     §ccdfe        ifeq      'S'
268600020328      *
268700020328      * Verifico se si tratta di una bolla Fedex
268800021119     c                   if        LnaNtw = 'FED'
268900020328      * recupero decodifica da tabella CB
269000020328     C                   eval      KCOD = 'CB'
269100020328     C                   movel(P)  'VARIE'       KKEY
269200020328     C                   move      V13svn        KKEY
269300020328     C                   EXSR      CHKTAB
269400020328     c                   endif
269500020328     c                   endif
269600020328      *
269700990702     C  n30              movel     Wtbluni       V13dsv
269800070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
269900070524     c                   eval      Ksvt = v13svn
270000070524     c     Ktitai        chain     titai01l
270100070524     c                   if        %found(titai01l)
270200070524
270300070524     c                   if        §$2iva = v13svn
270400070524     c                   eval      *in63 = *on
270500070524     c                   else
270600070524     c                   eval      *in64 = *on
270700070524     c                   endif
270800070524
270900070524     c                   else
271000070524     c                   eval      *in64 = *off
271100070524     c                   endif
271200070524
271300990702      *
271400990705     c                   add       1             nrr13
271500990702     c                   write     sb51s13
271600990702      *
271700990702     c                   ENDIF
271800990702      *
271900990701     c                   ENDSR
272000041007      *****************************************************************
272100041007      * GESTIONE F01 = Aggiunta Richiesta Assistenza
272200041007      *****************************************************************
272300041007     C     SUB_F01       BEGSR
272400041007      *
272500041007     c                   clear                   FIDNA1ds
272600041007     c                   eval      IA1ins  = 'S'
272700041007     c                   eval      IA1tor  = 'S'
272800041007     c                   eval      IA1ogg  = ds_OggSped
272900041007      *
273000041007     C                   ENDSR
273100990630      *****************************************************************
273200990630      * GESTIONE F14 = Giacenze
273300990630      *****************************************************************
273400990630     C     SUB_F14       BEGSR
273500990630      *
273600990901     c                   clear                   FNLG30DS
273700990901     C                   MOVEL     'S'           D30POA
273800990901     C                   MOVEL     tasAAS        D30AAS
273900990901     C                   MOVEL     tasLNP        D30LNP
274000990901     C                   MOVEL     tasNRS        D30NRS
274100990901     C                   MOVEL     tasNSP        D30NSP
274200990901     C                   MOVEL     FNLG30DS      KPJBU
274300990901     C                   SETON                                        31
274400990901     C                   CALL      'FNLG30C'
274500990901     C                   PARM                    KPJBA
274600990901      *
274700990901      *  ritorno alla videata di apertura
274800990901     c                   clear                   INFDSFMT
274900990701     c                   EVAL      *inkl = *on
275000990630      *
275100990630     c                   ENDSR
275200040416      *****************************************************************
275300040416      * GESTIONE F15 = Gestione Reclami
275400040416      *****************************************************************
275500040416     C     SUB_F15       BEGSR
275600040416      *
275700040416     c                   reset                   FIDNA2ds
275800040416     c                   eval      IA2ogg  = ds_OggSped
275900040416     c                   call      'FIDNA2R'
276000040416     c                   parm                    KPJBA
276100040416     c                   parm                    FIDNA2ds
276200040416      *
276300040416      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
276400040416     c                   if        OA2f03 = *on
276500040416     c                   eval      *inKC  = *on
276600040416     c                   else
276700040416      *  ritorno alla videata di apertura
276800040416     c                   clear                   InfDSfmt
276900040416     c                   eval      *inKL  = *on
277000040416     c                   endif
277100040416      *
277200040416     c                   ENDSR
277300990630      *****************************************************************
277400990630      * GESTIONE F16 = Competenze C/E
277500990630      *****************************************************************
277600990630     C     SUB_F16       BEGSR
277700990701      *
277800990701     c                   clear                   TNSB01DS
277900990701     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
278000990701     C                   MOVEL     'F16'         D01OP1                         *LIVELLO PROGRAMMA
278100990701     C                   MOVEL     '0'           D01F03                         *USCITA PGM
278200990701     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
278300990701     C                   MOVEL     '0'           D01ERR                         *ERRORE
278400990701     C                   Z-ADD     TASaas        D01AAS                         *ANNO
278500990701     C                   Z-ADD     taslnp        D01LNP                         *LNP
278600990701     C                   Z-ADD     tasnrs        D01NRS                         *SERIE
278700990701     C                   Z-ADD     tasnsp        D01NSP                        *SPEDIZIONE
278800990701     C                   MOVEL     tastbl        D01TBL                        *TIPO BOLLA
278900990701     C                   Z-ADD     V0Cdsp        D01DSP                        *DATA SPEDIZIONE
279000990701      *
279100990705     C                   MOVEL(P)  TNSB01DS      KPJBU
279200990701     C                   CALL      'TNSB08R'
279300990701     C                   PARM                    KPJBA
279400990701     C                   MOVEL     KPJBU         TNSB01DS
279500990701      *
279600990701      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
279700990707     C                   IF        D01F03 = '1'
279800990701     c                   eval      *inkc = *on
279900990707     c                   ELSE
280000990701     c                   clear                   INFDSFMT
280100990707     c                   eval      *inkl = *on
280200990707     C                   ENDIF
280300990701      *
280400990630     c                   ENDSR
280500000417      *****************************************************************
280600000417      * GESTIONE F17 = Richiesta rigenerazione bolle
280700000417      *****************************************************************
280800000417     C     SUB_F17       BEGSR
280900000417      *
281000000418     c                   clear                   tibs20ds
281100000418     C                   MOVEL     'W'           i20VID
281200000418     C                   Z-ADD     tasAAS        i20AAS
281300000418     C                   Z-ADD     tasLNP        i20LNP
281400000418     C                   Z-ADD     tasNRS        i20NRS
281500000418     C                   Z-ADD     tasNSP        i20NSP
281600000418     C                   Z-ADD     tasLNA        i20LNA
281700000418     C                   MOVEL(P)  tibs20ds      KPJBU
281800000417      *
281900000418     C                   CALL      'TIBS20R'
282000000418     C                   PARM                    KPJBA
282100000418     C                   MOVEL     KPJBU         tibs20ds
282200000418      *
282300000418      * Ritorno alla videata di apertura
282400000418     c                   clear                   INFDSFMT
282500000418     c                   eval      *inkl = *on
282600000418      *
282700000417     c                   ENDSR
282800990630      *****************************************************************
282900990630      * GESTIONE F18 = Legami
283000990630      *****************************************************************
283100990630     C     SUB_F18       BEGSR
283200030402
283300030402      * Per il primo richiamo imposto i campi del video
283400030402     c                   If        Wsb51d18 = *Blanks
283500030402     c                   ExSr      Ried18
283600030402     c                   Eval      Wsb51d18 = 'S'
283700030402     c                   EndIf
283800030402
283900030402      * Gestione subfile
284000030402     c                   If        NRR18 > 0
284100030402     c                   ExSr      Ried18_1
284200040518     c                   Else
284300040518     c                   Eval      *Inks = *Off
284400030402     c                   EndIf
284500990630      *
284600990630     c                   ENDSR
284700030402      *****************************************************************
284800030402      * IMPOSTO CAMPI VIDEO LEGAMI BOLLA
284900030402      *****************************************************************
285000030402     c     Ried18        BegSr
285100030402
285200030402      * Pulizia subfile
285300030402     c                   Eval      *In26 = *Off
285400030402     c                   Write     Sb51c18
285500030402     c                   Eval      *In25 = *On
285600030402     c                   Eval      *In26 = *On
285700030402     c                   Clear                   Nrr18
285800030402
285900030402      * Carico il subfile
286000030402      * uso la bolla come figlia o come mamma per cercare la bolla originale
286100030402     c     Keyspe        Chain     Fnlbl01l                           33
286200030402     c   33Keyspe        Chain     Fnlbl02l                           33
286300040517     c                   If        *In33
286400040517     c                   Eval      *Inks = *Off
286500040517     c                   EndIf
286600030402If  1c                   If        Not *In33
286700030402
286800030402      * con la bolla originale leggo la prima bolla della lista
286900031029     c**!!!              Eval      kLblAao = LblAao
287000031029     c**!!!              Eval      KlblLpo = LblLpo
287100031029     c**!!!              Eval      kLblNro = LblNro
287200031029     c**!!!              Eval      kLblNso = LblNso
287300031029     c**!!!keyspelbl     Chain     Fnlbl02l                           33
287400030402
287500030402      * Imposto subito nel sbfl la bolla originale
287600031029If  2c**!!!              If        Not *In33
287700030402     c                   Eval      v18Aap = LblAao
287800030402     c                   Eval      v18Lpp = LblLpo
287900030402     c                   Eval      v18Lap = LblLao
288000030402     c                   Eval      v18Nrp = LblNro
288100030402     c                   Eval      v18Nsp = LblNso
288200030402     c                   Clear                   v18Aan
288300030402     c                   Clear                   v18Lpn
288400030402     c                   Clear                   v18Lan
288500030402     c                   Clear                   v18Nrn
288600030402     c                   Clear                   v18Nsn
288700030402     c     'ORIGINAL'    CAT       'E':0         v18Pre
288800030402     c                   Clear                   v18Flg
288900030402     c                   Clear                   v18Npre
289000030402     c                   Clear                   v18Sce
289100030402     c                   Eval      *In53 = *Off
289200030402     c                   Eval      *In54 = *On
289300030402     c                   Eval      *In56 = *Off
289400030402
289500030402      * Controllo se la bolla trovata è quella richiesta dall'utente
289600030402If  3c                   If        LblAao = I51Aas and LblLpo = I51Lnp and
289700030402     c                             LblNro = I51Nrs and LblNso = I51Nsp
289800030402     c                   Eval      *In53 = *On
289900030402     c                   Eval      *In56 = *On
290000030402    3c                   EndIf
290100030402      * Controllo se abilitare o meno la scelta di visualizzazione
290200030402      * la bolla originale non è quella richiesta dall'utente
290300030402      * verifico se esiste in archivio
290400030403If  3c                   If        Not *In53
290500030402     c                   Eval      v18Flg = 'O'
290600030402     c                   Eval      kTasAas = LblAao
290700030402     c                   Eval      KTasLnp = LblLpo
290800030402     c                   Eval      kTasNrs = LblNro
290900030402     c                   Eval      kTasNsp = LblNso
291000030402     c     keyspe2       Setll     Titas30c
291100030402     c                   If        Not %Equal(Titas30c)
291200030402     c                   Eval      v18Npre = 'Non Presente'
291300030402     c                   Eval      *In56 = *On
291400030402     c                   EndIf
291500030403      * e non è la prima bolla richiesta
291600030403If  4c                   If        Not *In56 and
291700030403     c                             LblAao = SavAas and LblLpo = SavLnp and
291800030403     c                             LblNro = SavNrs and LblNso = SavNsp
291900030403     c                   Eval      *In56 = *On
292000030403    4c                   EndIf
292100030402    3c                   EndIf
292200030402
292300030402     c                   Eval      Nrr18 = 1
292400030402     c                   Write     Sb51s18
292500030402
292600030402     c                   Eval      *In54 = *Off
292700030402     c                   Eval      *In33 = *Off
292800031029    2c**!!!              EndIf
292900031029
293000031029      * con la bolla originale leggo la prima bolla della lista
293100031029     c                   Eval      kLblAao = LblAao
293200031029     c                   Eval      KlblLpo = LblLpo
293300031029     c                   Eval      kLblNro = LblNro
293400031029     c                   Eval      kLblNso = LblNso
293500031029     c     keyspelbl     Chain     Fnlbl02l                           33
293600030402      * Ciclo fino all'ultimo legame
293700030402      * lavorando solo sui campi "BOLLA SEGUENTE"
293800030402     c                   Clear                   wa
293900030402     c                   Clear                   wb
294000030402     c                   Clear                   Aan
294100030402Do  2c                   Dow       Not *In33
294200030402     c                   Eval      v18Aap = LblAap
294300030402     c                   Eval      v18Lpp = LblLpp
294400030402     c                   Eval      v18Lap = LblLap
294500030402     c                   Eval      v18Nrp = LblNrp
294600030402     c                   Eval      v18Nsp = LblNsp
294700030402     c                   Eval      v18Aan = LblAan
294800030402     c                   Eval      v18Lpn = LblLpn
294900030402     c                   Eval      v18Lan = LblLan
295000030402     c                   Eval      v18Nrn = LblNrn
295100030402     c                   Eval      v18Nsn = LblNsn
295200030402     c                   Clear                   v18pre
295300030402     c                   Clear                   v18Flg
295400030402     c                   Clear                   v18npre
295500030402     c                   Clear                   v18Sce
295600030402     c                   Eval      *In53 = *Off
295700030402     c                   Eval      *In55 = *Off
295800030402     c                   Eval      *In56 = *Off
295900030402
296000030402      * Controllo se la bolla precedente trovata è quella richiesta dall'utente
296100030402If  3c                   If        LblAap = I51Aas and LblLpp = I51Lnp and
296200030402     c                             LblNrp = I51Nrs and LblNsp = I51Nsp
296300030402     c                   Eval      *In53 = *On
296400030402     c                   Eval      v18Flg = 'P'
296500030402    3c                   EndIf
296600030402      * Controllo se la bolla seguente trovata è quella richiesta dall'utente
296700030402If  3c                   If        LblAan = I51Aas and LblLpn = I51Lnp and
296800030402     c                             LblNrn = I51Nrs and LblNsn = I51Nsp
296900030402     c                   Eval      *In55 = *On
297000030402     c                   Eval      v18Flg = 'S'
297100030403     c                   Eval      *In56 = *On
297200030402    3c                   EndIf
297300030402      * Controllo se abilitare o meno la scelta di visualizzazione
297400030403      * la bolla seguente non è quella che l'utente ha richiesto
297500030403If  3c                   If        Not *In55
297600030403     c                   Eval      kTasAas = LblAan
297700030403     c                   Eval      KTasLnp = LblLpn
297800030403     c                   Eval      kTasNrs = LblNrn
297900030403     c                   Eval      kTasNsp = LblNsn
298000030403      * verifico se esiste in archivio
298100030402     c     keyspe2       Setll     Titas30c
298200030402     c                   If        Not %Equal(Titas30c)
298300030402     c                   Eval      v18Npre = 'Non Presente'
298400030402     c                   Eval      *In56 = *On
298500030402     c                   EndIf
298600030403      * e se non è la prima bolla richiesta
298700030403If  4c                   If        Not *In56 and
298800030403     c                             LblAan = SavAas and LblLpn = SavLnp and
298900030403     c                             LblNrn = SavNrs and LblNsn = SavNsp
299000030403     c                   Eval      *In56 = *On
299100030403    4c                   EndIf
299200030402    3c                   EndIf
299300030402      * Scrivo il record nel sbfl
299400030402     c                   Add       1             Nrr18
299500030402     c                   Write     sb51s18
299600030402      * Aggiorno la schiera con le figlie
299700030402     c                   Add       1             wa                             INDICE SCHIERA
299800030402     c                   Eval      Aan(wa) = LblAan
299900030402     c                   Eval      Lpn(wa) = LblLpn
300000030402     c                   Eval      Nrn(wa) = LblNrn
300100030402     c                   Eval      Nsn(wa) = LblNsn
300200030402
300300030402     C     keyspelbl     Reade     Fnlbl02l                               33
300400030402
300500030402      * Se non trovo più bolle con la reade
300600030402      * utilizzo la figlia per leggere il legame successivo
300700030402Do  3c                   Dow       wb < wa  and %Eof(Fnlbl02l) = *On
300800030402     c                   Add       1             wb
300900030402     c                   Eval      kLblAao = Aan(wb)
301000030402     c                   Eval      kLblLpo = Lpn(wb)
301100030402     c                   Eval      kLblNro = Nrn(wb)
301200030402     c                   Eval      kLblNso = Nsn(wb)
301300030402     c     keyspelbl     Setll     Fnlbl02l
301400030402     c     keyspelbl     Reade     Fnlbl02l                               33
301500030402    3c                   EndDo
301600030402
301700030402    2c                   EndDo
301800030402    1C                   EndIf
301900030402
302000030402     c                   EndSr
302100030402      **********************************************************************
302200030402      * RICHIAMO PGM DI VISUALIZZAZIONE BOLLA LEGATA
302300030402      **********************************************************************
302400030402     c     Ried18_1      BegSr
302500030402
302600030402Do  1c                   Do        *Hival
302700030402     c                   Eval      Nrr18 = 1
302800030402     c                   Clear                   w001a18
302900030402      * Emetto Subfile
303000030402     c                   Write     Sb51t10
303100030402     c                   Write     Sb51z00
303200120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
303300030402     c                   Exfmt     Sb51c18
303400120302     c                   else
303500120302     c                   write     sb51c18
303600120302     c                   read(E)   tnsb51d
303700120302     c                   if        %error
303800120302     c                   eval      *inkc='1'
303900120302     c                   leave
304000120302     c                   endif
304100120302     c                   endif
304200030402      * Premuto un tasto funzionale
304300030402     c                   If         *In27 = *On
304400030402     c                   Leave
304500030402     c                   EndIf
304600030402      * Leggo il Subfile
304700030402     c                   Readc     Sb51s18
304800030402Do  2c                   Dow       %Eof = *Off
304900030402If  3c                   If        v18Sce = '5'
305000030402     c                   Eval      w001a18 = 'S'
305100030402      * passo il n. di spedizione da visualizzare
305200030403If  4c                   If        v18Flg = 'O'
305300030402     c                   Eval      I51Aas = v18Aap
305400030402     c                   Eval      I51Lnp = v18Lpp
305500030402     c                   Eval      I51Nrs = v18Nrp
305600030402     c                   Eval      I51Nsp = v18Nsp
305700030403   x4c                   Else
305800030402     c                   Eval      I51Aas = v18Aan
305900030402     c                   Eval      I51Lnp = v18Lpn
306000030402     c                   Eval      I51Nrs = v18Nrn
306100030402     c                   Eval      I51Nsp = v18Nsn
306200030402    4c                   EndIf
306300030402     c                   Clear                   v18Sce
306400030402     c                   Update    Sb51s18
306500030403     c                   Clear                   INFDSFMT
306600030402     c                   GoTo      Inizio
306700030402      * se premuto F3 esco dal programma
306800030402     c   kc              Leave
306900030402    3c                   EndIf
307000030402
307100030402     c                   Readc     Sb51s18
307200030402    2c                   EndDo
307300030402      * se non effettuate scelte forzo F12
307400030402     c                   If        w001a18 = *Blanks
307500030402     c                   Eval      *Inkl = *On
307600030402     c                   Clear                   INFDSFMT
307700030408     c                   Eval      w001a18 = 'S'
307800030402     c                   Leave
307900030402     c                   Endif
308000030402      *
308100030402    1c                   EndDo
308200030403
308300030418      * se premuto f12 ritorno alla prima bolla richiesta
308400030418     c                   If        *Inkl
308500030418     c                   Clear                   INFDSFMT
308600030418     c                   Eval      w001a18 = 'S'
308700030418     c                   EndIf
308800030402
308900030402     c                   EndSr
309000040119
309100040119      *****************************************************************
309200040119      * GESTIONE F19 = Immagine LdV
309300040119      *****************************************************************
309400040119     c     SUB_F19       BEGSR
309500040119      *
309600110927     c                   eval      c_tibs20=' '
309700110926     c                   select
309800040119      * Richiamo pgm. ANA670R per visualizzare la LdV
309900111129     c***                when      *inkt
310000111129     c                   when      *inkf
310100040119     c                   z-add     TASaas        Waas20
310200040119     c                   clear                   Parm670
310300040128     c                   movel(p)  'LDV'         Parm670(1)
310400040119     c                   movel(p)  ds_KEYsped    Parm670(2)
310500040119     c                   movel(p)  KNSif         Parm670(3)
310600040119      *
310700040331     ***c                   call      'ANA670R'
310800040331     c                   call      'YNA670R'
310900040119     c                   parm      '201'         Soc670
311000040122     c                   parm      'VIEW'        Cmd670
311100040122     c                   parm                    Parm670
311200040119     c                   parm      *off          Err670
311300040119     c                   parm      *blanks       Ret670
311400110927     c                   If        Ret670 = 'NULL '
311500110927     c                   eval      c_tibs20='S'
311600110927     c                   endif
311800111129     c****               when      *inkf
311900111129     c                   when      *inkt
311902161209     c                   z-add     TASaas        Waas20
311903161209     C                   CALL      'UBJDOC01R'
311904161209     C                   PARM                    ds_KEYsped
311905161209     C                   PARM      *zeros        oStato
311906161209     c                   clear                   w01des
311907161209     C                   SELECT
311908161209     C                   WHEN      oStato = *zeros
311909161209     c                   eval      c_tibs20='S'
311910161209     C                   WHEN      oStato = -1
311911161209     C                   EVAL      w01des = 'Errore generico'
311912161209     C                   WHEN      oStato = -2
311913161209     C                   EVAL      w01des = 'Errore: Tabelle mancanti'
311914161209     C                   WHEN      oStato = -3
311915161209     C                   EVAL      w01des = 'Errore in risposta da JDOC'
311916161209     C                   WHEN      oStato = -4
311917161209     C                   EVAL      w01des = 'Errore: POD non trovata'
311918161209     C                   WHEN      oStato = -5
311919161209     C                   EVAL      w01des = 'Errore visualizzazione su PC'
311920161209     C                   ENDSL
311921161209     c                   if        oStato<>*zeros
311922161209     c                   if        %SUBST(I51OP0:1:1)<>'M'
311923161209     c                   exfmt     sb51w01
311924161209     c                   else
311925161209     c                   write     sb51w01
311926161209     c                   read(E)   tnsb51d
311927161209     c                   if        %error
311928161209     c                   eval      *inkc='1'
311929161209     c                   leavesr
311930161209     c                   endif
311931161209     c                   endif
311932161209     c                   endif
315001161209
315100110926     c                   endsl
315200040511      * se la chiamata al pgm è andata a buon fine richiamo la rigenerazione bolla firmata
315300110927     c****               If        Ret670 = 'NULL '
315400110927     c                   if        c_tibs20='S'
315500040511     c                   Clear                   Tibs20ds
315600040511     c                   Eval      i20vid = 'S'
315700040511     c                   Eval      i20car = 'S'
315800040511     c                   Eval      i20aas = TasAas
315900040511     c                   Eval      i20lnp = TasLnp
316000040511     c                   Eval      i20nrs = TasNrs
316100040511     c                   Eval      i20nsp = TasNsp
316200040511     c                   Eval      i20lna = TasLna
316300040511     c                   Movel(p)  Tibs20ds      kpjbu
316400040511     c                   Call      'TIBS20R'
316500040511     c                   Parm                    kpjba
316600040511     c                   Eval      Tibs20ds = kpjbu
316700040511     c                   EndIf
316800040121      *
316900040128      *  ritorno alla videata di apertura
317000040128     c                   clear                   INFDSFMT
317100040121     c                   eval      *inKL = *on
317200040119      *
317300040119     c                   ENDSR
317400040119
317500990630      *****************************************************************
317600990630      * GESTIONE F20 = Affidabilità
317700990630      *****************************************************************
317800990630     C     SUB_F20       BEGSR
317900990701
318000990701      * Per il primo richiamo imposto i campi del video
318100990701     c                   IF        WSB51D20 = *blanks
318200990701     c                   exsr      ried12
318300990701     c                   eval      WSB51D20 = 'S'
318400990701     c                   ENDIF
318500990701
318600990701     c                   write     SB51T10
318700990701     c                   write     SB51Z00
318800120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
318900990701     c                   exfmt     SB51D12
319000120302     c                   else
319100120302     c                   write     sb51d12
319200120302     c                   read(E)   tnsb51d
319300120302     c                   if        %error
319400120302     c                   eval      *inkc='1'
319500120302     c                   endif
319600120302     c                   endif
319700990701
319800990701     c                   ENDSR
319900990701      **********************************************************************
320000990701      * IMPOSTO CAMPI VIDEO AFFIDABILITA'
320100990701      **********************************************************************
320200990701     C     RIED12        BEGSR
320300091211     c                   setoff                                       5960
320400990701      *
320500990701      * Data ritiro
320600990701     c                   IF        TASdrt > *zeros
320700990701     c     *iso          move      TASdrt        DATA_eur
320800990701     c     *eur          move      DATA_eur      W0080
320900990701     C                   eval      V12drt = %EDITC(W0080:'Y')
321000990701     c                   ENDIF
321100990701      * Numero ritiro
321200990701     c                   z-add     tasnrt        v12nrt
321300990701      * Prestazione Autotrasportatore
321400990701     c                   SELECT
321500990701     c                   WHEN      TASfpp = 'M'
321600990701     C                   eval      V12fpp = 'MATTINO'
321700990701     c                   WHEN      TASfpp = 'P'
321800990701     C                   eval      V12fpp = 'POMERIGGIO'
321900990701     c                   ENDSL
322000990701      * Stop Autotrasportatore
322100990701     c                   z-add     tasstr        v12str
322200990701      * Prestazione Autotrasportatore
322300990701     c                   z-add     taspdr        v12pdr
322400021203     C                   movel     tasprd        kpdpdr
322500021203     C                   move      'A'           kpdtip
322600021203     C     kfapd         chain     fiapd01L
322700000727     C                   if        %found
322800000727     C                   movel     apdrsc        v12rs1
322900000727     C                   else
323000000727     C                   clear                   v12rs1
323100000727     C                   endif
323200990701      * Data Immissione Bolla
323300990701     c                   IF        TASdim > *zeros
323400990701     c     *iso          move      TASdim        DATA_eur
323500990701     c     *eur          move      DATA_eur      W0080
323600990701     C                   eval      V12dim = %EDITC(W0080:'Y')
323700990701     c                   ENDIF
323800990701      * Data Borderò
323900990701     c                   IF        TASdbr > *zeros
324000990701     c     *iso          move      TASdbr        DATA_eur
324100990701     c     *eur          move      DATA_eur      W0080
324200990701     C                   eval      V12dbr = %EDITC(W0080:'Y')
324300990701     c                   ENDIF
324400990701      * Data Partenza Ultimo Collo
324500990701     c                   IF        TASduc > *zeros
324600990701     c     *iso          move      TASduc        DATA_eur
324700990701     c     *eur          move      DATA_eur      W0080
324800990701     C                   eval      V12duc = %EDITC(W0080:'Y')
324900990701     c                   ENDIF
325000091210      * Data Arrivo collo al terminal di arrivo--> ARBDAM
325100990701     c                   IF        TASdti > *zeros
325200990701     c     *iso          move      TASdti        DATA_eur
325300990701     c     *eur          move      DATA_eur      W0080
325400990701     C                   eval      V12dti = %EDITC(W0080:'Y')
325500990701     c                   ENDIF
325600090402      * Ora Arrivo Ultimo Collo . Es non la inviamo più in ttitas
325700090402     c****               IF        TAShti > *zeros
325800090402     c****               eval      *in42 = *on
325900090402     c****               z-add     TAShti        V12hti
326000090402     c****               ENDIF
326100091210     c
326200091210      * Data Arrivo all filiale di arrivo finale
326300091210      *           da Fiar500f record 'GEN'
326400091210     c                   Clear                   dAr5Gen
326500091210     c                   Eval      war5Trd = 'GEN'
326600091210     c     kSpeAr5       Chain     Fiar531c
326700091210If  1c                   If        %Found(Fiar531c)
326800091210     c                   Movel     Ar5Uni        Dar5Gen
326900091210     c                   IF        §ar5arrdt> *zeros
327000091210     c                   movel     §ar5arrdt     w0080
327100091210     c     *iso          move      w0080         DATA_eur
327200091210     c     *eur          move      DATA_eur      W0080
327300091210     C                   eval      V12arred = %EDITC(W0080:'Y')
327400091210     c* Ora arrivo reale
327500091210     c                   IF        §ar5arrhm> *zeros
327600091211     c                   eval      *in60 = *on
327700091210     c                   movel     §ar5arrhm     v12arreh
327800091210     c                   ENDIF
327900091210     c
328000091210     c                   ENDIF
328100091210     c                   ENDIF
328200091210     c
328300090402      * Data manifest per bolla export
328400090402     c                   clear                   v12dmf
328500090402     c                   if        lnantw='FED'  or lnantw='EEX' or
328600090402     c                             lnantw='DPD'
328700090402     c                   eval      ktrc = 'K'
328800090402     c     KTAStrc       CHAIN     TITA430C
328900090402     c                   If        %FOUND
329000090402     c                   movel     TA4not        dta4K
329100090402     c                   if        §ta4kdmf>0
329200090402     c     *iso          move      §ta4kdmf      DATA_eur
329300090402     c     *eur          move      DATA_eur      W0080
329400090402     C                   eval      V12dmf = %EDITC(W0080:'Y')
329500090402     c                   seton                                        59
329600090402     c                   ENDIF
329700090402     c                   ENDIF
329800090402     c
329900090402     c* ES: Dal 13 aprile 09 visulizzo data anche se vuota
330000090402     c                   if        not *in59  and tasdbr>20090413
330100090402     c                   seton                                        59
330200090402     c                   ENDIF
330300090402     c                   ENDIF
330400990701      * Consegne particolari
330500990701     c                   IF        TASFTC <> *BLANKS
330600990701     c                   movel     TASftc        V12ftc
330700990701     C                   eval      KCOD = '1P'
330800990701     C                   movel(P)  tasftc        KKEY
330900990701     C                   EXSR      CHKTAB
331000990701     c                   if        *in30 = *off
331100990702     C                   movel     Wtbluni       V12dtc
331200990701     c                   endif
331300990701     c                   ENDIF
331400990701      *
331500990701     c                   IF        TAStc2 <> *BLANKS
331600990701     c                   movel     TAStc2        V12tc2
331700990701     C                   eval      KCOD = '1P'
331800990701     C                   movel(P)  tastc2        KKEY
331900990701     C                   EXSR      CHKTAB
332000990701     c                   if        *in30 = *off
332100990701     C                   movel     Wtbluni       V12dc2
332200990701     c                   endif
332300990701     c                   ENDIF
332400990701      * Giorni chiusura
332500990701     c                   movel     TASgc1        V12gc1
332600990701     c                   movel     TASgc2        V12gc2
332700990701      * Data Consegna Teorica
332800990705     c                   IF        TASdce > *zeros
332900990705     c     *iso          move      TASdce        DATA_eur
333000990701     c     *eur          move      DATA_eur      W0080
333100990705     C                   eval      V12dce = %EDITC(W0080:'Y')
333200990701     c                   ENDIF
333300990705      * Ora Consegna Teorica
333400990705     c                   IF        TAShce > *zeros
333500990705     c                   eval      *in44 = *on
333600990705     c                   z-add     TAShce        V12hce
333700990705     c                   ENDIF
333800990702      * Tipo Data Consegna Richiesta
333900051129     c                   clear                   v12tcr
334000051129     c                   clear                   v12dcr
334100051129     c                   clear                   v12hcr
334200051129     c                   if        tasdcr>0 or tashcr>0
334300051129     c                   clear                   fnlv80ds
334400051129     c                   eval      ilv80tla='L'
334500051129     c                   eval      ilv80aas=tasaas
334600051129     c                   eval      ilv80lnp=taslnp
334700051129     c                   eval      ilv80nrs=tasnrs
334800051129     c                   eval      ilv80nsp=tasnsp
334900051129     c                   eval      ilv80mgs=tasmgs
335000051129     c                   eval      ilv80tc1=tasftc
335100051129     c                   eval      ilv80tc2=tastc2
335200051129     c                   eval      ilv80dcr=tasdcr
335300051129     c                   eval      ilv80hcr=tashcr
335400051129     c                   eval      ilv80tcr=tastcr
335500051129     c                   call      'FNLV80R'
335600051129     c                   parm                    kpjba
335700051129     c                   parm                    fnlv80ds
335800051129     c
335900051129     c                   eval      v12tcr=olv80tcrd
336000990702      * Data Consegna Richiesta
336100990702     c                   IF        TASdcr > *zeros
336200051129     C                   eval      V12dcr = %EDITC(olv80dcr8:'Y')
336300990702     c                   ENDIF
336400990702      * Ora Consegna Richiesta
336500990705     c                   IF        TAShcr > *zeros
336600990705     c                   eval      *in43 = *on
336700990705     c                   z-add     TAShcr        V12hcr
336800990705     c                   ENDIF
336900051129     c                   ENDIF
337000990702      * Stop consegna
337100990702     c                   z-add     tasstp        V12stp
337200990702      * Autotrasportatore
337300990702     c                   z-add     taspdc        V12pdc
337400021203     C                   movel     taspdc        kpdpdr
337500021203     C                   move      'A'           kpdtip
337600021203     C     kfapd         chain     fiapd01L
337700000727     C                   if        %found
337800000727     C                   movel     apdrsc        v12rs2
337900000727     C                   else
338000000727     C                   clear                   v12rs2
338100000727     C                   endif
338200990702      * Affidabilità
338300990702     c                   z-add     tasnpi        V12npi
338400990702     c                   z-add     tasnci        V12nci
338500990702     c                   z-add     tasnrc        V12nrc
338600010220      *
338700990630     c                   ENDSR
338800990630      *****************************************************************
338900990630      * GESTIONE F21 = Dettaglio Colli
339000990630      *****************************************************************
339100990630     C     SUB_F21       BEGSR
339200990705
339300990705      * Per il primo richiamo imposto i campi del video
339400990705     c                   IF        WSB51D21 = *blanks
339500990705     c                   exsr      ried21
339600990705     c                   eval      WSB51D21 = 'S'
339700990705     c                   ENDIF
339800990705
339900990705     c                   z-add     1             nrr21
340000990705
340100990705     C                   WRITE     SB51T10
340200990706     C                   WRITE     SB51Z00
340300120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
340400990705     C                   EXFMT     SB51C21
340500120302     c                   else
340600120302     c                   write     sb51c21
340700120302     c                   read(E)   tnsb51d
340800120302     c                   if        %error
340900120302     c                   eval      *inkc='1'
341000120302     c                   endif
341100120302     c                   endif
341200990705
341300990705     c                   ENDSR
341400990705      *****************************************************************
341500990705      * IMPOSTO CAMPI VIDEO DETTAGLIO COLLI
341600990705      *****************************************************************
341700990705     C     RIED21        BEGSR
341800990705
341900990630      *
342000990705      * Pulizia subfile
342100990705     C                   eval      *in26 = *off
342200990705     C                   WRITE     SB51C21
342300990705     C                   eval      *in25 = *on
342400990705     C                   eval      *in26 = *on
342500990705     C                   clear                   nrr21
342600990705      *
342700990705     C     KEYspe        SETLL     TITAT30C
342800990705     C     KEYspe        READE     TITAT30C
342900990705      *
343000990705     C                   DOW       %EOF = *off
343100990705     c                   z-add     tatfls        v21fls
343200990705     c                   z-add     tatnsc        v21nsc
343300990705     c                   If        TATdcm > *zeros
343400990705     c     *iso          move      TATdcm        DATA_eur
343500990705     c     *eur          move      DATA_eur      W0080
343600990705     C                   eval      V21dcm = %EDITC(W0080:'Y')
343700990705     c                   Else
343800990705     c                   movel     V0Cdcm        v21dcm
343900990705     c                   Endif
344000990630      *
344100990705     c                   ADD       1             nrr21
344200990705     c                   write     SB51s21
344300990705      *
344400990705     C     KEYspe        READE     TITAT30C
344500990705     c                   ENDDO
344600990705
344700990630     c                   ENDSR
344800990630      *****************************************************************
344900990630      * GESTIONE F22 = Contrassegno
345000990630      *****************************************************************
345100990630     C     SUB_F22       BEGSR
345200990630      *
345300990705     C                   clear                   tnsc40ds
345400990705     C                   Z-ADD     1             D40INT
345500990705     C                   Z-ADD     tasAAS        D40AAS
345600990705     C                   Z-ADD     tasLNP        D40LNP
345700990705     C                   Z-ADD     tasNRS        D40NRS
345800990705     C                   Z-ADD     tasNSP        D40NSP
345900990705     C                   MOVEL(P)  TNSC40DS      KPJBU
346000990705      *
346100990705     C                   CALL      'TNSC43R'
346200990705     C                   PARM                    KPJBA
346300990706      *  ritorno alla videata di apertura
346400990706     c                   clear                   INFDSFMT
346500990706     c                   eval      *inkl = *on
346600990706      *
346700990630     c                   ENDSR
346800020226      *****************************************************************
346900020226      * GESTIONE F23 = AWB FedEx
347000020226      *****************************************************************
347100020226     C     SUB_F23       BEGSR
347200020226      *
347300020226
347400020226      * Per il primo richiamo imposto i campi del video
347500020226     c                   IF        WSB51D23 = *blanks
347600020226     c                   exsr      ried23
347700020226     c                   eval      WSB51D23 = 'S'
347800020226     c                   ENDIF
347900020226
348000020226     c                   z-add     1             nrr23
348100020226
348200020226     C                   WRITE     SB51T10
348300020226     C                   WRITE     SB51Z00
348400120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
348500020226     C                   EXFMT     SB51C23
348600120302     c                   else
348700120302     c                   write     sb51c23
348800120302     c                   read(E)   tnsb51d
348900120302     c                   if        %error
349000120302     c                   eval      *inkc='1'
349100120302     c                   endif
349200120302     c                   endif
349300020226      *
349400020226     c                   ENDSR
349500020226      *****************************************************************
349600020226      * IMPOSTO CAMPI VIDEO AWB FEDEX
349700020226      *****************************************************************
349800020226     C     RIED23        BEGSR
349900020226
350000020226      *
350100020226      * Pulizia subfile
350200020226     C                   eval      *in26 = *off
350300020226     C                   WRITE     SB51C23
350400020226     C                   eval      *in25 = *on
350500020226     C                   eval      *in26 = *on
350600020226     C                   clear                   nrr23
350700020226      *
350800020226     c                   eval      wtahtrc = 'F'
350900020226     C     KEYspeh       SETLL     TITAH30C
351000020226     C     KEYspeh       READE     TITAH30C
351100020226      *
351200020226     C                   DOW       %EOF = *off
351300020226     c                   move      tahnot        V23not
351400020226     c                   z-add     tahnsc        v23nsc
351500020226     C                   move      '7H'          kcod
351600020226     C                   movel(p)  tahtrc        kkey
351700020226     C     KEYTAB        CHAIN     TABEL00F                           30
351800020226     c                   IF        *IN30 = *ON
351900020226     C                   else
352000020226     C                   ENDIF
352100020226      *
352200020226     c                   ADD       1             nrr23
352300020226     c                   write     SB51s23
352400020226      *
352500020226     C     KEYspeh       READE     TITAH30C
352600020226     c                   ENDDO
352700020226
352800020226     c                   ENDSR
352900020226      *****************************************************************
353000020226      * GESTIONE F24 = Altri dati
353100020226      *****************************************************************
353200020226     C     SUB_F24       BEGSR
353300020226
353400030121     c                   Eval      *In24 = *Off
353500030121     c                   Clear                   Vtf
353600020226     c                   clear                   vtf01
353700020226     c                   clear                   vtf02
353800020226     c                   clear                   vtf03
353900020226     c                   clear                   vtf04
354000020226     c                   clear                   vtf05
354100030121     c                   Clear                   Vtf06
354200020226
354300030121      * prima emissione tasti funzione
354400030121  1b c                   if        wsb51z00 = 1
354500030121
354600030121     c                   Clear                   xx
354700030121     c                   Do        5             yy
354800030121     c                   Add       1             xx
354900030121     c     '1'           Lookup    SkOnOff(xx)                            32
355000030121     c                   If        *In32
355100030121     c                   Eval      Vtf(yy) = Tf(xx)
355200030121     c                   Else
355300030121     c                   Leave
355400030121     c                   EndIf
355500030121     c                   EndDo
355600030121
355700030121     c                   Eval      Vtf01 = vtf(01)
355800030121     c                   Eval      Vtf02 = vtf(02)
355900030121     c                   Eval      Vtf03 = vtf(03)
356000030121     c                   Eval      Vtf04 = vtf(04)
356100030121     c                   Eval      Vtf05 = vtf(05)
356200030121
356300030121     c                   If        wOnOff > 5
356400030121     c                   Eval      Vtf06 = Cf2412
356500030121     c                   If        wOnOff > 10
356600030121     c                   Eval      Vtf06 = Cf2413
356700030121     c                   EndIf
356800030121     c                   Eval      *In24 = *On
356900030121     c                   Eval      wsb51z00 = 2
357000030121     c                   Goto      EndF24
357100030121     c                   EndIf
357200030121
357300030121     c                   EndIf
357400020226
357500030121      * seconda emissione tasti funzione
357600030121  1b c                   if        wsb51z00 = 2
357700030121
357800030121     c                   Do        5             yy
357900030121     c                   Add       1             xx
358000030121     c     '1'           Lookup    SkOnOff(xx)                            32
358100030121     c                   If        *In32
358200030121     c                   Eval      Vtf(yy) = Tf(xx)
358300030121     c                   Else
358400030121     c                   Leave
358500030121     c                   EndIf
358600030121     c                   EndDo
358700030121
358800030121     c                   Eval      Vtf01 = vtf(01)
358900030121     c                   Eval      Vtf02 = vtf(02)
359000030121     c                   Eval      Vtf03 = vtf(03)
359100030121     c                   Eval      Vtf04 = vtf(04)
359200030121     c                   Eval      Vtf05 = vtf(05)
359300030121
359400030121     c                   If        wOnOff > 10
359500030121     c                   Eval      *In24 = *On
359600030121     c                   Eval      Vtf06 = Cf2423
359700030121     c                   Eval      wsb51z00 = 3
359800030121     c                   Goto      EndF24
359900030121     c                   EndIf
360000030121     c                   If        wOnOff > 5
360100030121     c                   Eval      *In24 = *On
360200030121     c                   Eval      Vtf06 = Cf2422
360300030121     c                   Eval      wsb51z00 = 1
360400030121     c                   Goto      EndF24
360500030121     c                   EndIf
360600030121
360700030121     c                   EndIf
360800030121
360900030121      * terza emissione tasti funzione
361000030121  1b c                   if        wsb51z00 = 3
361100030121
361200030121     c                   Do        5             yy
361300030121     c                   Add       1             xx
361400030121     c     '1'           Lookup    SkOnOff(xx)                            32
361500030121     c                   If        *In32
361600030121     c                   Eval      Vtf(yy) = Tf(xx)
361700030121     c                   Else
361800030121     c                   Leave
361900030121     c                   EndIf
362000030121     c                   EndDo
362100030121
362200030121     c                   Eval      Vtf01 = vtf(01)
362300030121     c                   Eval      Vtf02 = vtf(02)
362400030121     c                   Eval      Vtf03 = vtf(03)
362500030121     c                   Eval      Vtf04 = vtf(04)
362600030121     c                   Eval      Vtf05 = vtf(05)
362700030121
362800030121     c                   Eval      *In24 = *On
362900030121     c                   Eval      Vtf06 = Cf2433
363000030121     c                   Eval      wsb51z00 = 1
363100030121     c                   Goto      EndF24
363200030121
363300030121     c                   EndIf
363400030121
363500030121     c     EndF24        ENDSR
363600990813      *****************************************************************
363700991021      * DETERMINO SE SPEDIZIONE CON FATTURA IMMEDIATA
363800990813      *****************************************************************
363900991021     C     FATIMMED      BEGSR
364000990813      *
364100990813     C                   eval      KCOD = 'QC'
364200990813     C                   movel(P)  '2'           KKEY
364300990813     C                   EXSR      CHKTAB
364400990813      *
364500990813     c                   if        *in30 = *off
364600990813     C                   MOVEL     TBLUNI        DSQC2
364700990813      *
364800991021     c                   IF        TASfiv <> §QCfiv  and  TASfiv <> §QCfi2  and
364900990813     c                             TASfiv <> §QCfi3  and  TASfiv <> §QCfi4  and
365000990813     c                             TASfiv <> §QCfi5  and  TASfiv <> §QCfi6
365100991021     c                   eval        Wfatimmed = 'S'
365200991021     c                   ENDIF
365300990813      *
365400990813     c                   endif
365500990813      *
365600990813     C                   ENDSR
365700990621      *****************************************************************
365800990621      *  AGGANCIO ANAGRAFICHE
365900990621      *****************************************************************
366000990621     C     CTR_ANAGRA    BEGSR
366100990621      *
366200990629     C                   clear                   DS_cnaco
366300990629     C                   clear                   DS_cnind
366400990629     C                   clear                   DS_cnclp
366500990629     C                   clear                   DS_fncls
366600990629      *
366700990621     C                   CALL      'TIBS69R'
366800990621     C                   PARM                    tibs69DS
366900990621     C                   PARM                    DS_cnaco
367000990621     C                   PARM                    DS_cnind
367100990621     C                   PARM                    DS_cnclp
367200990621     C                   PARM                    DS_fncls
367300990621      *
367400990621     C                   eval      wtibs69r = 'S'
367500990621      * per il 1° giro è inizializzata nelle specifiche "D"
367600990621     C                   clear                   TIBS69DS
367700990621      *
367800990621     C                   ENDSR
367900990628      **********************************************************************
368000990629      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
368100990628      **********************************************************************
368200990628     C     EDTIMPORTI    BEGSR
368300990628
368400990628      * -----------------------------------------------------------------------------------
368500990629      * W0133      contiene il numero da editare per il video
368600990629      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il numero ha decimali
368700990629      * IMPEDITATO ritorna il valore editato per il video lungo 13 (troncate cifre oltre 10 mld)
368800990628      * -----------------------------------------------------------------------------------
368900990628
369000990628     c                   clear                   IMPeditato
369100990629     c                   clear                   W017A
369200990628
369300990628     c                   if        Wtstdec = 'S'
369400990629     C                   eval      W017a = %EDITC(W0133:'2')
369500990628     c                   else
369600990629     c                   z-add     W0133         W0130
369700990629     C                   eval      W017a = %EDITC(W0130:'2')
369800990628     c                   endif
369900990629
370000990629     c                   move      W017a         IMPeditato
370100990628
370200990628     c                   ENDSR
370300990628      **********************************************************************
370400990628      * AGGANCIO FILE ORGANIGRAMMA
370500990628      **********************************************************************
370600990628     C     CHKORG        BEGSR
370700990628      *
370800990630     C                   clear                   ORGDES
370900990630     C     Wlinea        CHAIN     AZORG                              30
371000990628      *
371100990628     c                   ENDSR
371200990628      **********************************************************************
371300990628      * AGGANCIO FILE TABELLE
371400990628      **********************************************************************
371500990628     C     CHKTAB        BEGSR
371600990628      *
371700990628     C                   clear                   Wtbluni
371800990628      *
371900990630     C     KEYTAB        CHAIN     TABEL00F                           30
372000990630     c                   IF        *IN30 = *ON  or  TBLflg <> *blanks
372100990628     C                   eval         *in30 = *on
372200990628     C                   else
372300990628     C                   movel     TBLUNI        WTBLUNI
372400990628     C                   ENDIF
372500990628      *
372600990628     c                   ENDSR
372700030703      *-------------------------------------------------------------------------
372800030703      *  Decoditifa tipo bancali
372900030703      *-------------------------------------------------------------------------
373000030703     c     Sr_DecTba     BegSr
373100030703
373200030703     c                   Clear                   Tibs02Ds
373300030703     c                   Clear                   dTba
373400030703     c                   Movel     'C'           T02Mod
373500030703     c                   Movel     Knsif         T02Sif
373600030703     c                   Movel     'TBA'         T02Cod
373700030703     c                   Movel(p)  wtba          T02Ke1
373800030703     c                   Call      'TIBS02R'
373900030703     c                   Parm                    Kpjba
374000030703     c                   Parm                    Tibs02Ds
374100030703     c                   If        T02Err = *Blanks
374200030703     c                   Movel     T02Uni        dTba
374300030703     c                   EndIf
374400030703     c                   Eval      wdestba = §TbaDesc
374500030703
374600030703     c                   EndSr
374700030418      *****************************************************************
374800030418      * Pulisco tutti i record del file video
374900030418      *****************************************************************
375000030418     c     Sr_Clear      BegSr
375100030418
375200030418     c                   Clear                   wsb51d01
375300030418     c                   Clear                   wsb51d02
375400030418     c                   Clear                   wsb51d07
375500030418     c                   Clear                   wsb51d08
375600030418     c                   Clear                   wsb51d13
375700030418     c                   Clear                   wsb51d18
375800030418     c                   Clear                   wsb51d20
375900030418     c                   Clear                   wsb51d21
376000030418     c                   Clear                   wsb51d23
376100030418
376200030418     c                   Clear                   Sb51D01
376300030418     c                   Clear                   Sb51D07
376400030418     c                   Clear                   Sb51D12
376500030418
376600030418     c                   Clear                   Sb51C08
376700040603     c                   Clear                   Sb51C09
376800030418     c                   Clear                   Sb51C11
376900030418     c                   Clear                   Sb51C13
377000030418     c                   Clear                   Sb51C18
377100030418     c                   Clear                   Sb51C21
377200030418     c                   Clear                   Sb51C23
377300030418
377400030418     c                   Eval      *In26 = *Off
377500030418     c                   Write     Sb51C08
377600030418     c                   Eval      *In25 = *On
377700030418     c                   Eval      *In26 = *On
377800030418     c                   Clear                   nrr08
377900030418
378000030418     c                   Eval      *In26 = *Off
378100030418     c                   Write     Sb51C11
378200030418     c                   Eval      *In25 = *On
378300030418     c                   Eval      *In26 = *On
378400030418     c                   Clear                   nrr11
378500030418
378600030418     c                   Eval      *In26 = *Off
378700030418     c                   Write     Sb51C13
378800030418     c                   Eval      *In25 = *On
378900030418     c                   Eval      *In26 = *On
379000030418     c                   Clear                   nrr13
379100030418
379200030418     c                   Eval      *In26 = *Off
379300030418     c                   Write     Sb51C18
379400030418     c                   Eval      *In25 = *On
379500030418     c                   Eval      *In26 = *On
379600030418     c                   Clear                   nrr18
379700030418
379800030418     c                   Eval      *In26 = *Off
379900030418     c                   Write     Sb51C21
380000030418     c                   Eval      *In25 = *On
380100030418     c                   Eval      *In26 = *On
380200030418     c                   Clear                   nrr21
380300030418
380400030418     c                   Eval      *In26 = *Off
380500030418     c                   Write     Sb51C23
380600030418     c                   Eval      *In25 = *On
380700030418     c                   Eval      *In26 = *On
380800030418     c                   Clear                   nrr23
380900040512
381000040512     c                   Eval      wsb51z00 = 1
381100030418
381200030418     c                   EndSr
381300120711      *****************************************************************
381400120712      * Window Interrogazione Restituzione documenti al mitt.
381500120711      *****************************************************************
381600120711     c     Sr_w02        BegSr
381700121022
381800121022      * se non ho errore non visualizzo la riga
381900121022     c                   eval      *in69 = *off
382000121022     c                   IF        W02mtvdt <> *blanks
382100121022     c                   eval      *in69 = *on
382200121022     c                   ENDIF
382300120711     c*
382400120711     c                   if        %SUBST(I51OP0:1:1)<>'M'
382500120711     c                   exfmt     sb51w02
382600120711     c                   else
382700120712     c                   write     sb51w02
382800120711     c                   read(E)   tnsb51d
382900120711     c                   if        %error
383000120712     c                   leavesr
383100120711     c                   endif
383200120711     c                   endif
383300120711     c                   endsr
383400120829      *****************************************************************
383500120829      * Reperimento data scannerizzazione della figlia
383600120829      *****************************************************************
383700120829     c     Replbl        BegSr
383800120829     c                   eval      wtrovl=*off
383900120829     c                   eval      LBLAAN=I51AAS
384000120829     c                   eval      LBLLPN=I51LNP
384100120829     c                   eval      LBLNRN=I51NRS
384200120829     c                   eval      LBLNSN=I51NSP
384300120829     c     keylblN       chain     fnlbl02l
384400120830     c                   dow       %found(fnlbl02l)
384500120829     c
384600120829     c                   Eval      war5Trd = 'DOC'
384700120829     c     kspeAr5l      Chain     Fiar531c
384800120829If  1c                   If        %Found(Fiar531c)
384900120829     c                   Movel     Ar5Uni        Dar5doc
385000120829    4c                   if        §ar5jdrid >*zeros
385100120829     c                   eval      wtrovl=*on
385200120830     c                   leave
385300120829     c                   endif
385400120829     c                   endif
385500120829     c     keylblN       chain     fnlbl02l
385600120829     c                   enddo
385700120829     c                   endsr
385800160621
385900160621      /free
386000160621       //--------------------------------------------------------------
386100160621       //?Richiamo CL per comando visualizza fattura.
386200160621       //--------------------------------------------------------------
386300160621       BEGSR VisFattura;
386400160621
386500160621         clear pcmdpc;
386600160621         ppause = '*NO';
386700160621         clear puid;
386800160621         pnfat = *all'0';
386900160621         %subst(pnfat:4:6) = %editc(TASnft:'X');
387000160621
387100160621         IF  TASfiv = 996 or TASfiv = 998;
387200160621           puid = 'FATA' + '201' + %subst(%editc(TASdft:'X'):1:4) +
387300160621                  '2' + %editc(TASfiv:'X') + pnfat;
387400160621         ELSE;
387500160621           puid = 'FAT9' + '201' + %subst(%editc(TASdft:'X'):1:4) +
387600160621                  '2' + %editc(TASfiv:'X') + pnfat;
387700160621         ENDIF;
387800160621
387900160621
388000160621         pcmdpc = '"start ' + comandpc + puid + '"';
388100160621
388200160621         tnsb51c1 (pcmdpc:ppause);
388300160621
388400160621       ENDSR;
388500160621
388600160621      /end-free
388700990618      *****************************************************************
388800990618      * ROUTINE INIZIALE
388900990618      *****************************************************************
389000990618     C     *INZSR        BEGSR
389100990618      *
389200990618     C     *ENTRY        PLIST
389300990618     C                   PARM                    KPJBA
389400040416     c                   parm                    FIDNA1ds
389500990623      *
389600990628     c                   movel     KPJBU         TNSB51DS
389700030402
389800030402      * Mi salvo la bolla iniziale richiesta dall'utente
389900030402     c                   Eval      SavAas = I51Aas
390000030402     c                   Eval      SavLnp = I51Lnp
390100030402     c                   Eval      SavNrs = I51Nrs
390200030402     c                   Eval      SavNsp = I51Nsp
390300030402     c                   Eval      SavTbl = I51Tbl
390400020516
390500020516      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
390600020516     C     *dtaara       define    §azute        azuteds
390700020516     C     *dtaara       define    §datiute      ddatiute
390800020516
390900020516     C                   clear                   AzUteDs
391000020516     C                   clear                   DDatiUte
391100020516     C                   clear                   Tibs34Ds
391200020516     C                   in(E)     *dtaara
391300020516if  1C                   IF        %Error  or  RSUT = *blanks
391400020516     C                   call      'TIBS34R'
391500020516     C                   parm                    Tibs34Ds
391600020516     C                   in        *dtaara
391700020516e   1C                   ENDIF
391800990618
391900990618      * reperisco data e ora
392000990618     C                   TIME                    W0140
392100990618      * UDATE IN GGMMAAAA
392200990618     C                   MOVE      W0140         WDTGIO
392300990618      * UDATE IN AAAAMMGG
392400990805     C     *eur          MOVEL     WDTGIO        DATA_eur
392500990805     C     *iso          MOVEL     DATA_eur      dateu
392600110927     c* accendo in06 se utente edp (mi serve per l'abilitazione temporanea del tasto funz F6
392700110927     c* per l'interrogazione immagine a modo nuovo
392800111129     c****               if        %subst(knmus:1:3)= 'EDP'
392900111223     c****               seton                                        06
393000111129     c****               else
393100111129     c****               setoff                                       06
393200111129     c****               endif
393300110927     c
393400990618
393500990628     C     keyspe        KLIST
393600990628     C                   KFLD                    I51AAS
393700990628     C                   KFLD                    I51LNP
393800990628     C                   KFLD                    I51NRS
393900990628     C                   KFLD                    I51NSP
394000990618      *
394100990628     C     keyspe1       KLIST
394200990628     C                   KFLD                    I51AAS
394300990628     C                   KFLD                    I51LNP
394400990628     C                   KFLD                    I51NRS
394500990628     C                   KFLD                    I51NSP
394600990628     C                   KFLD                    I51TBL
394700030402
394800030402     c     keyspe2       Klist
394900030402     c                   Kfld                    kTasAas
395000030402     c                   Kfld                    kTasLnp
395100030402     c                   Kfld                    kTasNrs
395200030402     c                   Kfld                    kTasNsp
395300020225
395400020225     C     keyspeh       KLIST
395500020225     C                   KFLD                    I51AAS
395600020225     C                   KFLD                    I51LNP
395700020225     C                   KFLD                    I51NRS
395800020225     C                   KFLD                    I51NSP
395900020225     c                   kfld                    wtahtrc
396000030120
396100030120     c     kSpeAr5       Klist
396200030120     c                   Kfld                    I51AAS
396300030120     c                   Kfld                    I51LNP
396400030120     c                   Kfld                    I51NRS
396500030120     c                   Kfld                    I51NSP
396600030121     c                   kfld                    wAr5Trd
396700120829      *
396800120829     c     kSpeAr5l      Klist
396900120829     c                   Kfld                    LBLAAN
397000120829     c                   Kfld                    LBLLPN
397100120829     c                   Kfld                    LBLNRN
397200120829     c                   Kfld                    LBLNSN
397300120829     c                   kfld                    wAr5Trd
397400120829      *
397500120829     c     keylblN       Klist
397600120829     c                   Kfld                    LBLAAN
397700120829     c                   Kfld                    LBLLPN
397800120829     c                   Kfld                    LBLNRN
397900120829     c                   Kfld                    LBLNSN
398000030402
398100030402     c     keyspelbl     Klist
398200030402     c                   Kfld                    kLblAao
398300030402     c                   Kfld                    kLblLpo
398400030402     c                   Kfld                    kLblNro
398500030402     c                   Kfld                    kLblNso
398600990622      *
398700990630     C     keytab        KLIST
398800020516     C                   KFLD                    KKUT
398900990630     C                   KFLD                    KCOD
399000990630     C                   KFLD                    KKEY
399100990622      *
399200990629     C     ktastrc       KLIST
399300990629     C                   KFLD                    tasAAS
399400990629     C                   KFLD                    tasLNP
399500990629     C                   KFLD                    tasNRS
399600990629     C                   KFLD                    tasNSP
399700990629     C                   KFLD                    Ktrc
399800070524      *
399900070524     C     ktitai        KLIST
400000070524     C                   KFLD                    tasAAS
400100070524     C                   KFLD                    tasLNP
400200070524     C                   KFLD                    tasNRS
400300070524     C                   KFLD                    tasNSP
400400070524     C                   KFLD                    tasTBL
400500070524     C                   KFLD                    Ksvt                           Sigla varia tassata
400600020419
400700020419     c     kfnorm        klist
400800020419     c                   kfld                    kormpoe
400900020419     c                   kfld                    kormnsr
401000020419     c                   kfld                    kormnor
401100020419     c                   kfld                    kormnrv
401200021203
401300021203     C     Kfapd         klist
401400021203     C                   kfld                    kpdtip
401500021203     C                   kfld                    kpdpdr
401600040416      *
401700160404      * File FITGD32L
401800040416     c     K02TGD02      klist
401900040416     c                   kfld                    TGDtor
402000040416     c                   kfld                    TGDogg
402100040809
402200040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
402300040809     C     KDCT1_C       KLIST
402400040809     C                   KFLD                    dsaac
402500040809     C                   KFLD                    dsfil
402600040809     C                   KFLD                    dsnca
402700120330     C     Ktbe          KLIST
402800120330     C                   KFLD                    kcode
402900120330     C                   KFLD                    kke1
403000120330     C                   KFLD                    kke2
403100990623      *
403200990618     C                   ENDSR
403300990618      *---------------------------------------------------------------------------------------------
403400990618** MSG  Lungh. 78                                                            *
403500990623TNSB50R- Opzione non valida                                                     1
403600990623TNSB50R- Codice cliente errato                                                  2
403700990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
403800990623TNSB50R- Impostate selezioni incongruenti                                       4
403900990623TNSB50R- Data fattura errata                                                    5
404000990623TNSB50R- Immettere la linea                                                     6
404100990623TNSB50R- Linea inesistente o annullata                                          7
404200990623TNSB50R- Non richiedere la data fattura e le spedizioni non fatturate           8
404300990623TNSB50R- Immettere l'anno spedizione                                            9
404400990623TNSB50R- Non richiedere il numero fattura se non si è immessa la data fattura   10
404500990623TNSB50R- Non richiedere data fattura se non si è immesso il cod. fatturazione   11
404600990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
404700990623TNSB50R- Data spedizione errata                                                 13
404800990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
404900990623TNSB50R- Immettere almeno una selezione valida                                  15
405000990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
405100990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
405200990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
405300990623TNSB50R- Bolla esistente ma annullata                                           19
405400990623TNSB50R- Opzione non valida                                                     20
405500990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
405600990624TNSB50R- Non trovata la spedizione richiesta                                    22
405700120709TNSB50R- Bolla non trovata: e' stato modificato il tipo bolla                   23
405800020225** TF   Lungh. 11                                                            *
405900041018F15=Ric.Ass                                                                     1
406000041008F5=Eventi                                                                       2
406100041008F22=C/Ass.                                                                      3
406200041008F2=C.A.                                                                         4
406300041008F14=Giac.                                                                       5
406400041008F10=Variaz.                                                                     6
406500041008F18=Legami                                                                      7
406600041008F11=NoteS/B                                                                     8
406700041008F4=ORM                                                                          9
406800041008F8=CHI SONO                                                                     10
406900041008F21=D.Colli                                                                     11
407000041008F23=AWB Fed                                                                     12
407100041008F1=Agg.Rich                                                                     13
