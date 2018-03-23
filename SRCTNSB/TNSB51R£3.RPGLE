000100151016      */*END
000200980618      *-------------------------------------------------------------------------*
000300990628      *    INTERROGAZIONE BOLLE SEDE - VISUALIZZAZIONE SPEDIZIONE               *
000400980618      *-------------------------------------------------------------------------*
000500980715
000600990618      ****************************************************************
000700990618      *  RIEPILOGO INDICATORI
000800990618      ****************************************************************
000900041007      * 01 - ABILITO  F1=Immissione Richiesta Assistenza Clienti
001000990702      * 02 - ABILITO  F2=Comunicazione di Anomalia
001100990630      * 03 -
001200010221      * 04 - ABILITO  F4=ORM
001300990702      * 05 - ABILITO  F5=Eventi
001400990630      * 06 -
001500120711      * 07 -
001600000901      * 08 - ABILITO  F8=Cod."CHI SONO"
001700040603      * 09 - ABILITO  F9=Transiti
001800990702      * 10 - ABILITO  F10=Variazioni
001900030120      * 11 - ABILITO  F11=Note Supermercato
002000990630      * 12 -
002100991117      * 13 - ABILITO  F13=Tassazione
002200990702      * 14 - ABILITO  F14=Giacenza
002300040416      * 15 - Abilito  F15=Gestione reclamo
002400990702      * 16 - ABILITO  F16=Competenze di C/E
002500000417      * 17 - ABILITO  F17=Richiesta rigenerazione bolla
002600990702      * 18 - ABILITO  F18=Legami
002700040119      * 19 - ABILITO  F19=Immagine LdV
002800010221      * 20 -
002900990702      * 21 - ABILITO  F21=Dettaglio Colli
003000990702      * 22 - ABILITO  F22=C/Assegno
003100020225      * 23 - Abilito  F23=AWB FedEx
003200020225      * 24 - Abilito  F24=Altri Tasti
003300011029      **
003400990630      * 25 - GESTIONE SUBFILE
003500990630      * 26 - GESTIONE SUBFILE
003600990706      * 27 - PREMUTO UN TASTO FUNZIONALE
003700990618      * 28 - ERRORE GENERICO DSPF
003800990630      * 29 -
003900990618      * 30 - COMODO
004000990618      * 31 - COMODO
004100030120      * 32 - Comodo
004200030402      * 33 - Comodo
004300990630      * 34 -
004400991117      * 35 - PROFILO NON ABILITATO A: F13, F16 e Totale Fattura(nel DSPF)
004500990630      * 36 -
004600990630      * 37 -
004700990630      * 39 -
004800990701      ***  Video
004900990628      * 40 - PRESENTE ora consegna reale
005000990708      * 41 - SPENTO Fattura Immediata, ACCESO Fattura fine mese
005100990701      * 42 - PRESENTE ora arrivo ultimo collo
005200990702      * 43 - PRESENTE ora consegna richiesta
005300990705      * 44 - PRESENTE ora consegna teorica
005400990707      * 45 - ABILITO decodifica particolarità giacenza
005500990707      * 46 - ABILITO decodifica particolarità consegna
005600990707      * 47 - ABILITO decodifica particolarità varie
005700040608      * 48 - non visualizzo data entrata transito
005800040608      * 49 - non visualizzo data uscita transito
005900990720      * 50 - EMETTO tipo inserimento per importo da assicurare
006000991123      * 51 - EMETTO mittente originale
006100000222      * 52 - EMETTO dati arrivo DPD
006200030402      * 53 - R.I. x bolla legata precedente richiesta dall'utente
006300030402      * 54 - N.D. x bolla legata seguente quando c'è l'originale
006400030402      * 55 - R.I. x bolla legata seguente richiesta dall'utente
006500030402      * 56 - N.D. x bolla legata campo scelta
006600030408      * 57 - Visualizzo 2° intestazione
006700090402      * 58 - Visualizzo note partenza record 8 e 9 di tita4
006800090402      * 59 - Visualizzo data manifest per bolle export
006900990623      ***  Video 2
007000070524      ***  Video 13 Tassazionne
007100070524      * 61 - R.I. porto tassato prima della fatturazione
007200070524      * 62 - R.I. imponibile tassato prima della fatturazione
007300070524      * 63 - R.I. IVA calcolata prima della tassazione
007400070524      * 64 - R.I. subfile delle varie
007500120330      * 65 -
007600120330      * 66 -
007700120330      * 67 - visualizza dati DDT al mittente
007800121022      * 69 - Visualizza errore doc. al mittente SB51W02
007900151016      * 70 - ABILITA visualizzazione Statistica Pesi/Volumi x cliente
008000121217      * 71 - Abilita visualizzazione bolla recupero/originale per variazioni post-fatturazione
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
010300990720     FTITASV1L  IF   E           K DISK     prefix(V)
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
021700990618
021800990618     D DATA_eur        S               D   DATFMT(*eur)
021900120330     D DATAiso         S               D   DATFMT(*iso)
022000120330     D DATAdmy         S               D   DATFMT(*dmy)
022100110927
022200110927     d c_tibs20        s              1
022300110927     D CmdExc01        S             80
022400110927     D CmdExc02        S             15  5
022500110927     D                                     INZ(%SIZE(CmdExc01))
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
024100040119     D $POG            s              3    dim(250) inz(*zeros)
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
033600020516     D TRUL31DS      e ds                  inz
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
036600151016
036700040809     d*
036800040809     d dsKey           ds
036900040809     d  dsaac                         4  0
037000040809     d  dsfil                         3  0
037100040809     d  dsnca                         7  0
037200020226
037300990618     D                SDS
037400990628     D  VTCPGM                 1     10
037500040416     d  SDSprm           *parms
037600990618
037700030120      * Costanti
037800030120     D CF2413          C                   CONST('F24=AlTasti(1/3)')
037900030120     D CF2423          C                   CONST('F24=AlTasti(2/3)')
038000030120     D CF2433          C                   CONST('F24=AlTasti(3/3)')
038100030120     D CF2412          C                   CONST('F24=AlTasti(1/2)')
038200030120     D CF2422          C                   CONST('F24=AlTasti(2/2)')
038300120404
038400120404     d Wtntbe          s             21    inz('FILTRAGRU /TNTBE01L')
038500030120
038600990618      *------------------------------------------------------------------------*
038700990618      *---------------  C A L C O L O  ----------------------------------------*
038800990618      *------------------------------------------------------------------------*
038900030402
039000030402     c     Inizio        Tag
039100030402      * Provengo da interrogazione legami bolla
039200121217     c                   If        w001a18 = 'S'  or w001aleg='S'
039300030402     c     Keyspe        Setll     titas30c
039400030402     c     Keyspe        Reade     Titas30c
039500030402     c                   Eval      i51Tbl = tastbl
039600030408     c                   Eval      *In57 = *On
039700030418      * devo pulire tutte le videate
039800030418     c                   Exsr      Sr_Clear
039900030402     c                   EndIf
040000990618
040100990628     c     KEYSPE1       CHAIN     TITAS30C                           30
040200120709     c                   if        not %found(titas30c)
040300120709     c     KEYSPE        CHAIN     TITAS30C                           30
040400120709     c                   if        %found(titas30c)
040500120709     c                   eval      o51err='1'
040600120709     c                   eval      o51msg=msg(23)
040700120709     c                   endif
040800120709     c                   else
040900120709     c******             IF        %FOUND
041000990630
041100991117      * Verifico profilo per abilitazioni
041200991117     c                   EXSR      ABIPROFILO
041300991117
041400991117      * Abilito tasti funzionali
041500991117     c                   EXSR      ABITASFUN
041600990628
041700990630     C                   DO        *HIVAL
041800990630
041900990630      * Gestione tasti funzionali
042000990630     c                   SELECT
042100990630      * F3 = Fine - ESCO DAL PGM
042200990630     c                   WHEN      *INKC = *ON
042300990630     c                   eval      O51F03 = '1'
042400990630     c                   leave
042500030403      * F12 = Ritorno - se premuto non dalla prima bolla
042600030418      *                 ricarico la prima bolla
042700121217     c                   WHEN      *INKL=*ON and (w001a18 = 'S' or w001aleg='S')
042800121217     c                             and INFDSFMT = 'SB51D01'
042900030403     c                   Eval      I51Aas = SavAas
043000030403     c                   Eval      I51Lnp = SavLnp
043100030403     c                   Eval      I51Nrs = SavNrs
043200030403     c                   Eval      I51Nsp = SavNsp
043300030403     c                   Eval      I51Tbl = SavTbl
043400030403     c                   Clear                   w001a18
043500121217     c                   Clear                   w001aleg
043600030403     c                   Eval      *Inkl = *Off
043700030408     c                   Eval      *In57 = *Off
043800030418      * devo pulire tutte le videate
043900030418     c                   Exsr      Sr_Clear
044000030403     c                   GoTo      Inizio
044100990630      * F12 = Ritorno - se premuto nella videata di entrata ESCO DAL PGM
044200990630     c                   WHEN      *INKL = *ON  and  INFDSFMT = 'SB51D01'
044300990630     c                   eval      O51F12 = '1'
044400990630     c                   leave
044500990630      * F12 = Ritorno - se non premuto nella videata di entrata ritorno alla videata di entrata
044600990630     c                   WHEN      *INKL = *ON
044700990630     c                   exsr      SUB_FMT001
044800041007      *?F01 = Aggiunta Richiesta Assistenza clienti
044900041007     c                   WHEN      *inKA = *on
045000041007     c                   exsr      Sub_F01
045100041007     c                   leave
045200990630      * F2 = C.A.
045300990630     c                   WHEN      *INKB = *ON
045400990630     c                   exsr      SUB_F02
045500121121     c                   IF        *in28
045600121121     c                   eval      *inkb = '0'
045700121121     c                   ENDIF
045800010221      * F4 = ORM
045900010221     c                   WHEN      *INKD = *ON
046000010221     c                   exsr      SUB_F04
046100990630      * F5 = Eventi
046200990630     c                   WHEN      *INKE = *ON
046300990630     c                   exsr      SUB_F05
046400990630      * F7 = Altri Dati
046500990630     c                   WHEN      *INKG = *ON
046600990630     c                   exsr      SUB_F07
046700000901      * F8 = Cod."CHI SONO"
046800000901     c                   WHEN      *INKH = *ON
046900000901     c                   exsr      SUB_F08
047000040603      * F9 = Transiti
047100040603     c                   WHEN      *INKI = *ON
047200040603     c                   exsr      SUB_F09
047300990630      * F10 = Variazioni
047400990630     c                   WHEN      *INKJ = *ON
047500990630     c                   exsr      SUB_F10
047600030120      * F11 = Note Supermercato
047700030120     c                   WHEN      *INKK = *ON
047800030120     c                   exsr      SUB_F11
047900990630      * F13 = Tassazione
048000990630     c                   WHEN      *INKM = *ON
048100990630     c                   exsr      SUB_F13
048200990630      * F14 = Giacenze
048300990630     c                   WHEN      *INKN = *ON
048400990630     c                   exsr      SUB_F14
048500040416      *?F15 = Gestione reclami?
048600040416     c                   WHEN      *inKP = *on
048700040416     c                   exsr      Sub_F15
048800990630      * F16 = Competenze Conto Economico
048900990630     c                   WHEN      *INKQ = *ON
049000990630     c                   exsr      SUB_F16
049100000417      * F17 = Richiesta rigenerazione spedizione
049200000417     c                   WHEN      *INKR = *ON
049300000417     c                   exsr      SUB_F17
049400990630      * F18 = Legami
049500990630     c                   WHEN      *INKS = *ON
049600990630     c                   exsr      SUB_F18
049700040119      * F19 = Immagine LdV
049800110926     c                   WHEN      *INKT = *ON  or
049900111129     c****                         (*inkf=*on and %subst(knmus:1:3)= 'EDP')
050000111129     c                             *inkf
050100040119     c                   exsr      SUB_F19
050200990630      * F20 = Affidabilità
050300990630     c                   WHEN      *INKU = *ON
050400990630     c                   exsr      SUB_F20
050500990630      * F21 = Dettaglio Colli
050600990630     c                   WHEN      *INKV = *ON
050700990630     c                   exsr      SUB_F21
050800990630      * F22 = Contrassegni
050900990630     c                   WHEN      *INKW = *ON
051000990630     c                   exsr      SUB_F22
051100020225      * F23 = AWB FedEx
051200020225     c                   WHEN      *INKX = *ON
051300020225     c                   exsr      SUB_F23
051400020225      * F24 = Altri Tasti
051500020225     c                   WHEN      *INKY = *ON
051600020225     c                   exsr      SUB_F24
051700020226     c                   eval      *inky = *off
051800121217     c* ? su campo per visualizzazione legame bolla originale/di recupero variaz.
051900121217     c                   when      *in71 and v1cleg='?'
052000121217     c                   clear                   v1cleg
052100121217     c                   Eval      I51Aas = waas_leg
052200121217     c                   Eval      I51Lnp = wlnp_leg
052300121217     c                   Eval      I51Nrs = wnrs_leg
052400121217     c                   Eval      I51Nsp = wnsp_leg
052500121217     c                   Eval      w001aleg = 'S'
052600121217     c                   clear                   infdsfmt
052700121217     c                   goto      inizio
052800151016      *?? per visualizzare i pesi usati in tassazione?
052900151016     c                   when      *in70 and V1PesTas = '?'
053000151016     c                   clear                   V1PesTas
053100151020     c                   clear                   TNSB56ds
053200151020     c                   eval      D56aas = TASaas
053300151020     c                   eval      D56lnp = TASlnp
053400151020     c                   eval      D56nrs = TASnrs
053500151020     c                   eval      D56nsp = TASnsp
053600151020     c                   eval      D56tbl = TAStbl
053700151022     c                   eval      D56ksc = TASksc
053800151022     c*//                eval      D56rsc = TAS...
053900151022     c                   eval      D56ctr = TASctr
054000151022     c*//                eval      D56dtr = §TRdes
054100151020     c                   eval      KPJBU  = TNSB56ds
054200151020     c                   call      'TNSB56R'
054300151020     c                   parm                    KPJBA
054400151020     c                   eval      TNSB56ds = KPJBU
054500151016     c                   clear                   InfDsFmt
054600151016     c                   goto      Inizio
054700151016     c*//                eval      *inKL = *on
054800990630      * Emissione Videata di entrata
054900990630     c                   OTHER
055000990630     c                   exsr      SUB_FMT001
055100990630     c                   ENDSL
055200990618
055300990630     c                   ENDDO
055400990630     c                   ENDIF
055500990618
055600990618      * Chiudo pgm aperti
055700990618     C                   IF        Wtibs69r  <>  *BLANKS
055800990618     C                   eval      I69TLA  = 'C'
055900990618     C                   call      'TIBS69R'
056000990618     C                   parm                    TIBS69DS
056100990624     c                   ENDIF
056200040810     C*
056300040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
056400040810     c                   clear                   fidn12ds
056500040810     c                   eval      i12tla = 'C'
056600040810     c                   call      'FIDN12R'
056700040810     c                   parm                    fidn12ds
056800110927      *
056900110927     C                   EVAL      CmdExc01 = 'RMVLIBLE LIB(SKGLIB)'
057000110927     C                   CALL(E)   'QCMDEXC'
057100110927     C                   PARM                    CmdExc01
057200110927     C                   PARM                    CmdExc02
057300040810     c*
057400990618
057500990624      * Imposto campi per uscita con F03
057600990630     c                   MOVEL(P)  TNSB51DS      KPJBU
057700990630
057800990618     c                   EVAL      *INLR = *ON
057900991117      **********************************************************************
058000991117      * VERIFICO PROFILO PER ABILITAZIONI
058100991117      **********************************************************************
058200991117     C     ABIPROFILO    BEGSR
058300991117
058400020516     c                   eval      *in35  = *off
058500020516     c                   clear                   wABI
058600020516     c                   clear                   DLAT
058700020516
058800020516      *-- Verifica errori e autorità profilo
058900020516sel 1C                   SELECT
059000020516      *-- controllo se ho errori nei dati utente
059100020516      *--   nel qual caso NON risulta un profilo abilitato
059200020516w   1C                   WHEN      DUTERR = 'E'
059300020516     C                   eval      *IN35  = *on
059400020516
059500020516      *-- CONTROLLO AUTORITA'
059600020516      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
059700020516      *-- se il chiamante non richiede autorità specifica verificare
059800020516      *--   quella del profilo
059900020516      *-- se il chiamante richiede autorità specifica verificarla,
060000020516      *--  se è blank verificare quella del profilo
060100020516
060200020516      * se UTEAUT = *BLANK non siamo in sede
060300020516      *--     nel qual caso saranno attuate le protezioni del caso
060400020516w   1C                   WHEN      UTEAUT = *blank
060500020516     C                   eval      *in35  = *on
060600020516
060700020516x   1C                   OTHER
060800020516      *-----------------------------------------------------------------
060900020516     C                   movel     UTEFAF        DUTE01
061000020516      *-- Autorizzazione alla interrogazione delle bolle di sede
061100020516if  2C                   IF        §UTEibs <> *blanks
061200020516      *--   Con AUTORITÀ SPECIFICA
061300020516     C                   eval      wabi = §UTEibs
061400020516x   2C                   ELSE
061500020516      *--   Senza AUTORITÀ SPECIFICA
061600020516     C                   eval      wabi = UTEAUT
061700020516e   2C                   ENDIF
061800020516      *
061900020516     c                   clear                   tibs02ds
062000020516     c                   movel     'C'           t02mod
062100020516     c                   movel     knsif         t02sif
062200020516     c                   movel     'LAT'         t02cod
062300020516     c                   movel(p)  wabi          t02ke1
062400020516     c                   call      'TIBS02R'
062500020516     c                   parm                    kpjba
062600020516     c                   parm                    tibs02ds
062700020516if  2c                   if        t02err = *blanks
062800020516     c                   movel     t02uni        DLAT
062900020516e   2c                   endif
063000020516if  2c                   if        T02err <> *blanks   or
063100020516     c                             §LATabi = 'S'
063200020516     C                   eval      *in35  = *on
063300020516e   2c                   endif
063400020516      *
063500020516e   1c                   ENDSL
063600020516      *
063700020516      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
063800020516      *
063900020516      * Utente con abilitazione valida e diversa da "AZ"
064000020516if  1c                   if        wABI  <> 'AZ' and not *in35
064100020516      *
064200020516if  2c                   if        wABI  <> I31abi
064300020516     c                   reset                   $POG
064400020516      * Reperimento dei P.O. gestibili dall'utente
064500020516     c                   clear                   TRUL31DS
064600020516     c                   eval      I31abi = wabi
064700020516     c                   eval      I31cdi = DUTdis
064800020516     c                   eval      I31car = DUTare
064900020516     c                   eval      I31cpo = DUTpou
065000020516     c                   call      'TRUL31R'
065100020516     c                   parm                    KPJBA
065200020516     c                   parm                    TRUL31DS
065300020516if  3c                   if        O31pog > *zeros
065400020516     c                   movea     O31pog        $POG
065500020516e   3c                   endif
065600020516e   2c                   endif
065700020516      *
065800020516      *  controllo se il P.O. di competenza per il cliente è compreso
065900020516      *    tra quelli gestibili dall'utente
066000020516     c                   movel     TASksc        wPOa
066100020516     c     wPOa          lookup    $POG                                   30
066200020516if  2c                   if        not *in30
066300020516     c                   eval      *in35  = *on
066400020516e   2c                   endif
066500020516      *
066600020516e   1c                   endif
066700020516
066800020516     C                   ENDSR
066900990630      **********************************************************************
067000990630      * ABILITO TASTI FUNZIONALI
067100990630      **********************************************************************
067200990630     C     ABITASFUN     BEGSR
067300040119
067400040119     c                   movel     TASflo        dTASflo
067500990630
067600000209      *  F2= Comunicazione di Anomalia, non abilitato se arrivo dalle C.A.
067700000209     c                   EVAL      *IN02 = (TASfda = 'S'
067800000209     c                                      and  %SUBST(I51OP0:1:1) <> 'D')
067900010221
068000010221      *  F4= ORM
068100111103     c                   clear                   wta4not
068200010221     c                   eval      ktrc = 'M'
068300010221     c     KTAStrc       CHAIN     TITA430C
068400020419 b1  C                   if        %found(tita430C)
068500020419 b2  C     ta4not        ifne      *blanks
068600020419     c                   eval      Wta4not = ta4not
068700020419     c                   open      fnorm01l
068800020419     c                   movel     wta4not       dsbl4m
068900020419     c                   move      §b4poe        kormpoe
069000020419     c                   move      §b4nsr        kormnsr
069100020419     c                   move      §b4nor        kormnor
069200020419     c                   move      §b4nrv        kormnrv
069300020419     c     kfnorm        setll     fnorm01l
069400020419 b3  c                   if        %equal(fnorm01l)
069500010221     C                   seton                                        04
069600020419 x3  C                   else
069700010221     C                   setoff                                       04
069800111103     c*****              clear                   Wta4not
069900020419 e3  C                   endif
070000020419     c                   close     fnorm01l
070100020419 e2  c                   endif
070200020419 x1  C                   else
070300010404     c                   clear                   Wta4not
070400010221     C                   setoff                                       04
070500020419 e1  C                   endif
070600990630
070700990630      *  F5= Eventi
070800990813     C     KEYspe        SETLL     FNEVB01L                               05
070900000901
071000000901      *  F8= Cod."CHI SONO"
071100020225     c                   eval      wtahtrc = 'C'
071200020225     C     KEYspeh       SETLL     TITAH30C                               08
071300040603
071400040603      *  F9= Transiti
071500040604     c                   Eval      woktrs = *Off
071600040603     c                   Eval      *In09 = *Off
071700040603     c                   Eval      wAr5Trd = 'TRS'
071800040604     c     kSpeAr5       Setgt     Fiar531c
071900040604     c                   Do        *Hival
072000040604     c     kSpeAr5       Readpe    Fiar531c
072100040604     c                   If        %Eof(Fiar531c)
072200040604     c                   Leave
072300040604     c                   EndIf
072400040604     c                   Eval      woktrs = *On
072500040603     c                   Eval      dAr5Trs = Ar5Uni
072600040603     c                   If        §Ar5Flp > *Zeros or §ar5Fl2 > *Zeros or
072700040603     c                             §Ar5Fl3 > *Zeros or §ar5Fl4 > *Zeros or
072800040603     c                             §Ar5Fl5 > *Zeros
072900040603     c                   Eval      *In09 = *On
073000040603     c                   EndIf
073100040604     c                   Leave
073200040604     c                   EndDo
073300040604     c                   If        woktrs = *Off and
073400040604     c                             (TasFlp > *Zeros or TasFl2 > *Zeros)
073500040603     c                   Eval      *In09 = *On
073600040603     c                   EndIf
073700040603
073800990630      *  F10= Variazioni
073900990720     C     KEYspe        SETLL     TITASV1L                               10
074000991117
074100030122      *  F11= Note Supermercato/Bancali
074200030121     c                   Eval      wAr5Trd = 'SCR'
074300040603     c     kSpeAr5       Setll     Fiar531c                               11
074400030703     c                   If        Not *In11
074500030703     c                   Eval      wAr5Trd = 'BNB'
074600040603     c     kSpeAr5       Setll     Fiar531c                               11
074700030703     c                   EndIf
074800030122     c                   If        Not *In11
074900030122     c                   Eval      wAr5Trd = 'BAN'
075000040603     c     kSpeAr5       Chain     Fiar531c
075100040603     c                   If        %Found(Fiar531c)
075200030122     c                   Movel     Ar5Uni        dAr5Ban
075300030122     c                   If        §Ar5Rb1 <> *Zeros or §Ar5Rb2 <> *Zeros
075400030122     c                   Eval      *In11 = *On
075500030122     c                   EndIf
075600030122     c                   EndIf
075700030127     c                   EndIf
075800030120
075900991117      *  F13= Tassazione
076000991117     c                   EVAL      *IN13 = (*IN35 = *off)
076100990630
076200000209      *  F14= Giacenza, non abilitato se arrivo dalle giacenze
076300000209     c                   IF        %SUBST(I51OP0:1:1) = 'G'
076400000209     c                   eval      *in14 = *off
076500000209     c                   ELSE
076600050401     C     KEYspe        setll     Tigcp51l                               14
076700000209     c                   ENDIF
076800041007      *
076900041007      *?Abilito F1=Aggiunta in Reclami?
077000041007      * (se richiamato da pgm. FIDNA1R...)
077100041007     c                   if            SDSprm >  1
077200041007     c                             and IA1ins = ' '
077300041007     c                   eval      *in01  = *on
077400041007     c                   endif
077500040416      *
077600040416      *?Abilito F15=Visualizzazione Reclami?
077700040416      * (se richiamato da pgm. FIDNA1R...)
077800040416     c                   eval      TGDtor  = 'S'
077900040416     c                   eval      TGDogg  = ds_OggSped
078000040416     c     K02TGD02      setll     FITGD000
078100160404     c                   eval      *in15   = (%equal(FITGD32L)   and
078200040416     c                                           (SDSprm =  1
078300040416     c                                        or (SDSprm >  1
078400040416     c                                        and IA1ins <> 'N')))
078500990630
078600990630      *  F16= Competenze di C/E
078700991117     c                   if         *in35 = *off
078800010312     c                   open      eccet30c
078900990630     C     KEYspe1       SETLL     ECCET30C                               16
079000010312     c                   close     eccet30c
079100991117     c                   endif
079200080118     c
079300080118      *  F17= Richiesta rigenerazione bolla se non vengo dalla manutenzione
079400080118     c                   if                 %SUBST(I51OP0:1:1) <> 'M'
079500030624     c                   Eval      kCod = '3A'
079600030624     c                   Movel(P)  TasCbo        kKey
079700030624     c                   ExSr      ChkTab
079800030624     c  n30              Movel     Wtbluni       ds3a
079900030627     c                   If        §3agen <> 'N'
080000000418     c                   EVAL      *IN17 = *on
080100030624     c                   EndIf
080200080118     c                   EndIf
080300990630
080400990630      *  F18= Legami
080500990630     c                   EVAL      *IN18 = (TASfa1 = 'S')
080600040119
080700040119      *  F19= Immagine LdV
080800040331     ***c                   eval      *in19 = (§FLOiml = 'S')
080900040331     c                   eval      *in19 = (§FLOiml <> *blanks)
081000990630
081100990630      *  F21= Dettaglio Colli
081200990630     C     KEYspe        SETLL     TITAT30C                               21
081300990630
081400000209      *  F22= C/Assegno, non abilitato se arrivo dai C/Assegni
081500000209     c                   EVAL      *IN22 = (TASfca = 'S'
081600000209     c                                      and  %SUBST(I51OP0:1:1) <> 'C' )
081700020225      *  F23= AWB FedEx
081800020225     c                   eval      wtahtrc = 'F'
081900020225     C     KEYspeh       SETLL     TITAH30C                               23
082000030121
082100030121      * Imposto le sk dei tasti funzione che verranno emessi nella riga variabile
082200030121      * a video (5 tasti funzione alla volta + tasto F24)
082300030121      * le sk devono essere caricate in base all'ordine di emissione dei tasti
082400030121      * funzione
082500030121      * 1 sk con il n.indicatore e 1 sk con lo stato dell'indicatore (acceso/spento)
082600040416     c                   Eval      SkInd(1) = 15
082700040416     c                   Eval      SkOnOff(1) = *In15
082800040416     c                   Eval      SkInd(2) = 5
082900040416     c                   Eval      SkOnOff(2) = *In05
083000040416     c                   Eval      SkInd(3) = 22
083100040416     c                   Eval      SkOnOff(3) = *In22
083200040416     c                   Eval      SkInd(4) = 2
083300040416     c                   Eval      SkOnOff(4) = *In02
083400040416     c                   Eval      SkInd(5) = 14
083500040416     c                   Eval      SkOnOff(5) = *In14
083600040416     c                   Eval      SkInd(6) = 10
083700040416     c                   Eval      SkOnOff(6) = *In10
083800040416     c                   Eval      SkInd(7) = 18
083900040416     c                   Eval      SkOnOff(7) = *In18
084000040416     c                   Eval      SkInd(8) = 11
084100040416     c                   Eval      SkOnOff(8) = *In11
084200040416     c                   Eval      SkInd(9) = 04
084300040416     c                   Eval      SkOnOff(9) = *In04
084400040416     c                   Eval      SkInd(10) = 08
084500040416     c                   Eval      SkOnOff(10) = *In08
084600040416     c                   Eval      SkInd(11) = 21
084700040416     c                   Eval      SkOnOff(11) = *In21
084800040416     c                   Eval      SkInd(12) = 23
084900040416     c                   Eval      SkOnOff(12) = *In23
085000041008     c                   Eval      SkInd(13) = 01
085100041008     c                   Eval      SkOnOff(13) = *In01
085200030121
085300030403     c                   Clear                   wOnOff
085400030121      * Conto quanti tasti funzione sono attivi
085500120711     c                   Do        13            xx
085600030121     c                   If        SkOnOff(xx) = '1'
085700030121     c                   Add       1             wOnOff
085800030121     c                   EndIf
085900030121     c                   EndDo
086000020225
086100020225      *  F24= Altri tasti
086200020226     c                   exsr      sub_f24
086300990630
086400990630     C                   ENDSR
086500990630      **********************************************************************
086600990630      * EMETTO VIDEATA DI ENTRATA
086700990630      **********************************************************************
086800990630     C     SUB_FMT001    BEGSR
086900990705
087000990705      * Per il primo richiamo imposto i campi della videata di entrata, i campi comuni a più videate
087100990708      *  e i dati del mittente e del destinatario per tutte le videate
087200990705     c                   IF        WSB51D01 = *blanks
087300990705     c                   EXSR      RIED01
087400990705     c                   eval      WSB51D01 = 'S'
087500990705     c                   ENDIF
087600990705
087700990630     c                   write     SB51T01
087800990630     c                   write     SB51Z00
087900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
088000120302     c                   exfmt     SB51D01
088100120302     c                   else
088200120302     c                   write     sb51d01
088300120302     c                   read(E)   tnsb51d
088400120302     c                   if        %error
088500120302     c                   eval      *inkc='1'
088600120302     c                   endif
088700120302     c                   endif
088800990630
088900990630     c                   ENDSR
089000990618      **********************************************************************
089100990630      * IMPOSTO CAMPI VIDEO 1 E QUELLI COMUNI A PIU' VIDEATE
089200990618      **********************************************************************
089300990628     C     RIED01        BEGSR
089400151020     c                   eval      *in70  = *off
089500151020     c                   clear                   dAR5fat
089600990618
089700990630      * I campi con prefisso V0C o V0D sono presenti in più videate
089800990630
089900990630     c                   z-add     taslnp        V0Clnp
090000990630     c                   z-add     tasnrs        V0Cnrs
090100990630     c                   z-add     tasnsp        V0Cnsp
090200990630     c                   z-add     TASaas        V0Cdsp
090300990630     c                   movel     TASmgs        V0Cdsp
090400990630     c     *usa          move      V0Cdsp        DATA_eur
090500990630     c     *eur          move      DATA_eur      V0Cdsp
090600990630      *
090700990630     c                   move      TASccm        Wccm4
090800990630     c                   move      TASksc        Wksc4
090900990628      * Tipo servizio
091000990630     C                   eval      KCOD = '5E'
091100990628     C                   movel(P)  tastsp        KKEY
091200990628     C                   EXSR      CHKTAB
091300990629     c                   if        *in30 = *off
091400090605     C                   movel     Wtbluni       ds5e
091500090605     c                   eval      V0Dtsp = §5ed08
091600990629     c                   endif
091700990628      * Stato bolla
091800990628     c                   SELECT
091900990628     c                   WHEN      TASFIC <> *blanks
092000990630     c                   eval      V0Dsts = 'CONTABILIZZ.'
092100990628     c                   WHEN      TASDFT > *zeros
092200990630     c                   eval      V0Dsts = 'FATTURATA   '
092300990628     c                   OTHER
092400990630     c                   eval      V0Dsts = 'DA FATTURARE'
092500990628     c                   ENDSL
092600990628      * P.O. Arrivo
092700021119     c                   Clear                   Og143
092800021119     c                   Clear                   LnaNtw
092900990630     c                   z-add     taslna        V0Clna
093000990628     c                   z-add     taslna        Wlinea
093100990628     C                   EXSR      CHKORG
093200990630     C                   movel     orgdes        V0Dlna
093300021119     c                   Movel     Orgde3        Og143
093400021119     c                   Movel     §OgNtw        LnaNtw
093500990630      * Tipo bolla
093600990630     c                   movel     tastbl        V0Ctbl
093700990630     C                   eval      KCOD = 'TB'
093800990628     C                   movel(P)  tastbl        KKEY
093900990628     C                   EXSR      CHKTAB
094000990629     c                   if        *in30 = *off
094100990629     C                   movel     Wtbluni       DSTB
094200990630     C                   movel     §TBdes        V0Dtbl
094300990726     C                   movel     §TBdes        V0Dtb1
094400990629     c                   endif
094500151020     c
094600151020     c                   clear                   ds3a
094700151020     c                   Eval      kCod = '3A'
094800151020     c                   Movel(P)  TasCbo        kKey
094900151020     c                   ExSr      ChkTab
095000151020     c  n30              Movel     Wtbluni       ds3a
095100990628      * Data 1a Consegna Parziale
095200990628     c                   IF        TASdcp > *zeros
095300990630     c     *iso          move      TASdcp        DATA_eur
095400990813     c     *eur          move      DATA_eur      Wtasdcp
095500990813     C                   eval      V0Cdcp = %EDITC(Wtasdcp:'Y')
095600990628     c                   ENDIF
095700990628      * Codice consegna anomala
095800990702     c                   IF        TAScca <> *blanks
095900990701     c                   movel     tascca        v0ccca
096000990630     C                   eval      KCOD = '7O'
096100990628     C                   movel(P)  tascca        KKEY
096200990628     C                   EXSR      CHKTAB
096300990702     C  N30              movel     Wtbluni       V0Dcca
096400990702     c                   ENDIF
096500990628      * Data Consegna Reale
096600990628     c                   IF        TASdcm > *zeros
096700990630     c     *iso          move      TASdcm        DATA_eur
096800990630     c     *eur          move      DATA_eur      W0080
096900990701     C                   eval      V0Cdcm = %EDITC(W0080:'Y')
097000990628     c                   ENDIF
097100990628      * Ora Consegna Reale
097200990628     c                   IF        TASHmc > *zeros
097300990628     c                   eval      *IN40 = *on
097400990701     c                   z-add     tashmc        v0chmc
097500990628     c                   ENDIF
097600990628      * Numero e data distinta consegna
097700990701     c                   z-add     tasndc        v0cndc
097800990628     c                   IF        TASddc > *zeros
097900990630     c     *iso          move      TASddc        DATA_eur
098000990630     c     *eur          move      DATA_eur      W0080
098100990701     C                   eval      V0Cddc = %EDITC(W0080:'Y')
098200990628     c                   ENDIF
098300990628      * Dati Fattura
098400990629      *
098500990629      *    Decodifico Divisa e memorizzo dati in DS di comodo
098600990630     C                   eval      KCOD = 'CV'
098700990628     C                   movel(P)  tasdiv        KKEY
098800990628     C                   EXSR      CHKTAB
098900990629     c                   if        *in30 = *off
099000990628     C                   movel     Wtbluni       WDSCV
099100990629     c                   endif
099200991021      *    Se ho il libro IVA determino se fattura immediata
099300991021     c                   clear                   Wfatimmed
099400991021     c                   IF        TASfiv > *zeros
099500991021     c                   EXSR      FATIMMED
099600991021     c                   ENDIF
099700991021      *
099800991021      *    Se Fattura immediata  scrivo IMPORTO altrimenti IMPONIBILE
099900991021     c                   IF        Wfatimmed = 'S'
100000990716      *        se fatturata calcolo l'importo
100100990813     c                   if        TASfts = '1'
100200990714     c                   eval      D17tla = 'L'
100300990825     C
100400991021     c                   if         Wfatimmed = 'S'  and  §TBTPO = 'F'
100500990825     c                   eval      D17PRP = 'S'
100600990813     c                   endif
100700990825     c                   eval      D17tbo = '0'
100800990714     c                   eval      D17crb = 'S'
100900990714     c                   eval      D17aas = TASaas
101000990714     c                   eval      D17lns = TASlnp
101100990714     c                   eval      D17nrs = TASnrs
101200990714     c                   eval      D17nsp = TASnsp
101300990714     c                   eval      D17tbs = TAStbl
101400990714     c                   eval      D17fim = 'F'
101500990714     c                   CALL      'FNLV16R'
101600990714     c                   PARM                    FNLV16DS
101700990714     c                   PARM                    DTASV
101800990716     c                   endif
101900990714     c                   eval      *in41 = *off
102000990714     c                   z-add     O17IFT        W0133
102100990813      *
102200990628     c                   ELSE
102300990702     c                   z-add     TASimv        W0133
102400990708     c                   eval      *in41 = *on
102500990628     c                   ENDIF
102600990628      *
102700990629     c                   IF         W0133 > *zeros
102800990628     c                   movel     W§cvfdc       Wtstdec
102900990628     c                   exsr      EDTimporti
103000990628     c                   movel     IMPeditato    v1cimp
103100990628     c                   ENDIF
103200011029      *    Divisa
103300011029     c                   IF        v1cimp <>*zeros
103400011029     c                   movel     tasdiv        v1cdiv
103500011029     c                   endif
103600990629      *    P.O. IVA - Numero Fattura - Data fattura
103700990702     c                   z-add     TASfiv        V0Cfiv
103800990702     c                   z-add     TASnft        V0Cnft
103900990629     c                   IF        TASdft > *zeros
104000990630     c     *iso          move      TASdft        DATA_eur
104100990630     c     *eur          move      DATA_eur      W0080
104200990702     C                   eval      V0Cdft = %EDITC(W0080:'Y')
104300990702      *    Divisa
104400990702     c                   ENDIF
104500990701      * Dati Mittente per tutte le videate
104600990701     c                   z-add     TASccm        V0Cccm
104700990701      *    se non codificato aggancio TITAA
104800990707     c                   IF        Wccm4 = *zero  or  Wccm4 = 8888
104900990629     c                   eval      ktrc = 'M'
105000990701     c     KTAStrc       CHAIN     TITAA30C
105100990701      *
105200990701     c                   IF        %FOUND
105300990629     c                   movel     TAArsc        V1Crsm
105400990701     c                   movel     TAAind        V1Cinm
105500990701     c                   movel     TAAloc        V1Clom
105600990701     c                   if        TAAnaz <> *blanks
105700990701     c                   movel     TAAnaz        V1Cpnm
105800990701     c                   else
105900990701     c                   movel     TAAprv        V1Cpnm
106000990701     c                   endif
106100990701     c                   movel     TAAcap        V0Ccam
106200990701     c                   movel     TAArsc        V7Crsm
106300990701     c                   movel     TAAind        V7Cinm
106400990701     c                   movel     TAAloc        V7Clom
106500990702     c                   movel     TAAprv        V7Cprm
106600990702     c                   movel     TAAnaz        V7Cnzm
106700990701     c                   movel     TAAcpi        V7Cpim
106800990701     c                   ENDIF
106900990629      *    se codificato aggancio anagrafico
107000990629     c                   ELSE
107100990701     c                   z-add     V0Cccm        I69kac
107200990701     c                   z-add     V0Cccm        I69kin
107300990701     c                   z-add     V0Cccm        I69kcs
107400990630     c                   exsr      CTR_anagra
107500990701      *
107600990630     c                   movel     ACOrag        V1Crsm
107700990630     c                   movel     INDvia        V1Cinm
107800990630     c                   movel     INDcit        V1Clom
107900990630     c                   if        INDsta <> *blanks
108000990630     c                   movel     INDsta        V1Cpnm
108100990630     c                   else
108200990630     c                   movel     INDprv        V1Cpnm
108300990630     c                   endif
108400990701     c                   movel     INDcae        V0Ccam
108500990701     c                   movel     ACOrag        V7Crsm
108600990701     c                   movel     INDvia        V7Cinm
108700990701     c                   movel     INDcit        V7Clom
108800990701     c                   movel     INDprv        V7Cprm
108900990701     c                   movel     INDsta        V7Cnzm
109000990701     c                   movel     INDiva        V7Cpim
109100990629     c                   ENDIF
109200990629      * Destinatario
109300990701      *    se assegnato utilizzo codice di tassazione come codice destinatario
109400990629     c                   IF        §TBtpo = 'A'
109500990701     c                   z-add     TASksc        V0Cccd
109600990629     c                   ENDIF
109700990701      *
109800111011     c* cerco 2 parte della ragione sociale
109900111011     c                   eval      ktrc = 'D'
110000111011     c                   clear                   rsd2             35
110100111011     c     ktastrc       chain     tita430c
110200111011 b1  c                   if        %found(tita430C)
110300111011     c                   eval      rsd2=ta4not
110400111011     c                   endif
110500111011     c
110600990629     c                   movel     TASrsd        V1Crsd
110700111011     c                   if        %subst(tasrsd:31:5)= *blanks and rsd2<>*blanks
110800111011     c                   eval      v1crsd=%trim(v1crsd) + ' ' +rsd2
110900111011     c                   endif
111000990629     c                   movel     TASind        V1Cind
111100990629     c                   movel     TASlod        V1Clod
111200990629     c                   if        TASnzd <> *blanks
111300990629     c                   movel     TASnzd        V1Cpnd
111400990629     c                   else
111500990629     c                   movel     TASprd        V1Cpnd
111600990629     c                   endif
111700990701     c                   movel     TAScad        V0Ccad
111800990702     c                   movel     TASnzd        V0Cnzd
111900111011     c                   clear                   v7crsd
112000990701     c                   movel     TASrsd        V7Crsd
112100111011     c*
112200111011 b1  c                   if        rsd2<>*blanks
112300111011     c                   if        %subst(tasrsd:35:1)<>' '
112400111011     c                   eval      v7crsd=tasrsd +ta4not
112500111011     c                   else
112600111011     c                   eval      v7crsd=%trim(v7crsd) + ' ' +rsd2
112700111011     c                   endif
112800111011     c
112900111011     c                   endif
113000990701     c                   movel     TASind        V7Cind
113100990701     c                   movel     TASlod        V7Clod
113200990701     c                   movel     TASprd        V7Cprd
113300990701     c                   movel     TAScpd        V7Cpid
113400061108      * Codice fiscale mittente/destinatario da Tita4 rcd Q
113500061108     c                   eval      ktrc = 'Q'
113600061108     c                   clear                   dta4q
113700061108     c     ktastrc       chain     tita430c
113800061108 b1  c                   if        %found(tita430C)
113900061108     c                   eval      dta4q = ta4not
114000061108     c                   endif
114100061108     c                   eval      v7cdfm = §ta4qcfm
114200061108     c                   eval      v7cdfd = §ta4qcfd
114300990726      *
114400990701      * Cliente Fatturazione
114500990701     c                   z-add     TASksc        VTCksc
114600990701     c                   SELECT
114700990701      *     assegnato non codificato  (Da TITAS)
114800990708     c                   WHEN      §TBtpo = 'A' and  *IN41 = *off
114900990726     c                   movel     TASrsd        VTDksc
115000990726      *     assegnato codificato (Decodifico la ragione sociale)
115100990701     c                   WHEN      §TBtpo = 'A'
115200990702     c                   z-add     tasksc        I69kac
115300990701     c                   exsr      CTR_anagra
115400990726     c                   movel     ACOrag        VTDksc
115500990701      *     porto franco
115600990701     c                   WHEN      §TBtpo = 'F'
115700990701     c                   movel     V7Crsm        VTDksc
115800990701     c                   ENDSL
115900991123      * Mittente Originale
116000991123     c                   eval      ktrc = 'O'
116100991123     c     KTAStrc       CHAIN     TITAA30C
116200991123     c                   If        %FOUND
116300991123     c                   eval      *in51 = *on
116400991123     c                   eval      v7crmo = TAArsc
116500991123     c                   Endif
116600990629      * Riferimento mittente numerico
116700990701     c                   z-add     TASrmn        V0Crmn
116800990629      * Riferimento mittente alfanumerico e riferimento partner
116900990629     c                   eval      ktrc = 'A'
117000990629     c     KTAStrc       CHAIN     TITA430C
117100990629     c                   If        %FOUND
117200990629     c                   movel     TA4not        DTA4A
117300990630     c                   movel     §TA4ARMA      V1Crma
117400070927     c*********+         movel     §TA4ARPT      V1Crpt
117500160201     c                   movel     §TA4Anas      V1Cnas
117600990629     c                   ENDIF
117700070927      * Riferimento  partner nel record "E"
117800070927     c                   eval      ktrc = 'E'
117900070927     c     KTAStrc       CHAIN     TITA430C
118000070927     c                   If        %FOUND
118100070927     c                   movel     TA4not        dsbl4E
118200070927     c                   movel     §b4ERP        V1Crpt
118300070927     c                   ENDIF
118400000222      * Numero parcel DPD
118500060518     c***                eval      ktrc = 'F'
118600060518     c                   eval      ktrc = 'I'
118700000222     c     KTAStrc       CHAIN     TITA430C
118800000222     c                   If        %FOUND
118900060518     c**                 movel     TA4not        DSBL4F
119000060518     c**                 move(P)   §B4fnp        V1Crpt
119100060518     c**                 movel     §B4rtn        V1Crtn
119200060518     c**                 movel     §B4zon        V1Czon
119300060518     c                   movel     TA4not        DSBL4i
119400060518     c                   movel(P)  §B4ipn        V1Crpt
119500060518     c                   movel     §B4ida        V1Crtn
119600060518     c                   movel     §B4iza        V1Czon
119700060518     C                   eval      *in52 = (V1Crtn <> *blanks or
119800060518     C                                      V1Czon <> *blanks)
119900000222     c                   ENDIF
120000990629      * Natura merce
120100160201     c****               movel     TASnas        V1Cnas
120200990629      * Segnacolli
120300990630     c                   z-add     TASfls        V1Cfls
120400990629     c                   z-add     TASnrs        V1Cnrss
120500990629     c                   z-add     TASncd        V1Cncd
120600990629     c                   z-add     TASnca        V1Cnca
120700990629     c                   if        TASfns = 'S'
120800990629     c                   eval      V1Dfns = 'NON SEQUENZIALI'
120900990629     c                   endif
121000990629     c                   z-add     TASznc        V1Cznc
121100050113     c* flag raggruppamento spedizioni
121200050113     c                   movel     TASxco        V1Cxco
121300990629      * Contrassegno
121400160412     c                   setoff                                       39
121500990630     c                   IF        TASfca = 'S'
121600990630     c     Keyspe        chain     TNCSB03L                           30
121700160419     c****               If        %FOUND  and  CSBsta <> 9
121800160419     c                   If        %FOUND
121900990630     c                   z-add     CSBcas        V1Ccas
122000990630     c                   If         CSBvca <> *blanks
122100990630     c                   movel     CSBvca        V1Cvca
122200990630     c                   Else
122300990630     c                   movel     'ITL'         V1Cvca
122400990630     c                   Endif
122500990630     c                   Endif
122600160412     c                   If        %found and   CSBsta= 9
122700160412     c                   seton                                        39
122800160412     c                   endif
122900990630     c                   ENDIF
123000031119
123100031119      * Referente e Telefono del destinatario
123200110610      * e profilo utente/terminale che ha immesso la bolla
123300031119     c                   Clear                   V7cRef
123400031119     c                   Clear                   V7cTeld
123500110610     c                   Clear                   V7cpru
123600110610     c                   Clear                   V7cnmtd
123700120712     c                   clear                   W02DRDT
123800120712     c                   clear                   W02MTVDT
123900120712     c                   clear                   W02DDTIL
124000120712     c                   clear                   W02DIMTV
124100120712     c                   clear                   W02DMTVDT
124200120712     c                   clear                   W02DRID
124300120829     c                   clear                   W02lbl
124400110610     c                   setoff                                       66
124500120404     c                   setoff                                       6768
124600031119      * li prendo da Fiar500f record 'GEN'
124700031119     c                   Clear                   dAr5Gen
124800031119     c                   Eval      war5Trd = 'GEN'
124900040603     c     kSpeAr5       Chain     Fiar531c
125000040603If  1c                   If        %Found(Fiar531c)
125100031119     c                   Movel     Ar5Uni        Dar5Gen
125200031119     c                   Eval      V7cRef = §Ar5Ref
125300031119     c                   Eval      V7cTeld = §Ar5Teld
125400110610     c                   Eval      V7cpru  = §Ar5bpru
125500110610     c                   Eval      V7cnmtd = §Ar5bnmtd
125600120404    2c                   if        v7cpru<>*blanks
125700110610     c                   seton                                        66
125800120404    2c                   endif
125900120330     c* restituzione DDt al mittente
126000151119    2c***                if        §ar5drdt<>*blanks
126100151119     c****               seton                                        67
126200120712     c* data ricezione
126300151119    3c***                if        §ar5drdt<>'99999999'
126400151119     c***                movel     §ar5drdt      w0080             8 0
126500151119     c***  *iso          movel     w0080         dataiso
126600151119     c***                eval      data_eur=dataiso
126700151119     c***                movel     data_eur      w02drdt
126800151119    3c***                endif
126900120712     c* "Elaborata per il mittente"
127000151119     c***                clear                   w02dfstd
127100151119     c***                if        §ar5fstd='S'
127200151119     c***                eval      w02dfstd='Elaborata per invio al Mittente'
127300151119     c***                endif
127400120712     c* motivo
127500151119    3c***                if        §ar5mtvdt<>*blanks
127600120712     c* apro file TNTBE di filiale
127700151119     c***                if        not %open(tntbe01l)
127800151119     c***                open(e)   tntbe01l
127900151119     c***                if        %error
128000151119     c***                eval      %subst(Wtntbe:7:4)='GRPF'
128100151119     c***                open(e)   tntbe01l
128200151119     c***                endif
128300151119     c***                endif
128400150828     c***
128500151119     c***                eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
128600151119     c***                                   %subst(§ar5mtvdt:4:5)
128700151119     c***                eval      kcode='MDE'
128800151119     c***                clear                   kke1
128900151119     c***                clear                   kke2
129000151119     c***                eval      kke1=%subst(§ar5mtvdt:1:3)
129100151119     c***                eval      kke2=%subst(§ar5mtvdt:4:5)
129200151119     c***  ktbe          chain     tntbe01l
129300151119    4c***                if        %found(tntbe01l)
129400151119     c***                eval      w02dmtvdt=tbeuni
129500151119    4c***                endif
129600151119    4c***                if        §ar5dimtv >*zeros
129700151119     c***                movel     §ar5dimtv     w0080             8 0
129800151119     c***  *iso          movel     w0080         dataiso
129900151119     c***                eval      data_eur=dataiso
130000151119     c***                movel     data_eur      w02dimtv
130100151119     c***                eval      w02ddtil='Inserito il'
130200151119    4c***                endif
130300151119    3c***                endif
130400151119    2c***                endif
130500031119    1c                   EndIf
130600120712     c* DATA RICEZIONE IMM.DOC.
130700120712     c                   Clear                   dAr5doc
130800120712     c                   Eval      war5Trd = 'DOC'
130900120712     c     kSpeAr5       Chain     Fiar531c
131000120712If  1c                   If        %Found(Fiar531c)
131100120712     c                   Movel     Ar5Uni        Dar5doc
131200120712    4c                   if        §ar5jdrid >*zeros
131300120712     c                   seton                                        67
131400120712     c                   movel     §ar5jdrid     w0080             8 0
131500120712     c     *iso          movel     w0080         dataiso
131600120712     c                   eval      data_eur=dataiso
131700120712     c                   movel     data_eur      w02drid
131800120712    4c                   endif
131900150828     c* restituzione DDt al mittente
132000151119    2c                   if        §ar5drdt<>*blanks
132100151119     c                   seton                                        67
132200150828     c* data ricezione
132300151119    3c                   if        §ar5drdt<>'99999999'
132400151119     c                   movel     §ar5drdt      w0080             8 0
132500151119     c     *iso          movel     w0080         dataiso
132600151119     c                   eval      data_eur=dataiso
132700151119     c                   movel     data_eur      w02drdt
132800151119    3c                   endif
132900150828     c* "Elaborata per il mittente"
133000151119     c                   clear                   w02dfstd
133100151119     c                   if        §ar5fstd='S'
133200151119     c                   eval      w02dfstd='Elaborata per invio al Mittente'
133300151119     c                   endif
133400150828     c* motivo
133500151119    3c                   if        §ar5mtvdt<>*blanks
133600150828     c* apro file TNTBE di filiale
133700151119     c                   if        not %open(tntbe01l)
133800151119     c                   open(e)   tntbe01l
133900151119     c                   if        %error
134000151119     c                   eval      %subst(Wtntbe:7:4)='GRPF'
134100151119     c                   open(e)   tntbe01l
134200151119     c                   endif
134300151119     c                   endif
134400150828     c
134500151119     c                   eval      w02mtvdt=%subst(§ar5mtvdt:1:3) +'/'+
134600151119     c                                      %subst(§ar5mtvdt:4:5)
134700151119     c                   eval      kcode='MDE'
134800151119     c                   clear                   kke1
134900151119     c                   clear                   kke2
135000151119     c                   eval      kke1=%subst(§ar5mtvdt:1:3)
135100151119     c                   eval      kke2=%subst(§ar5mtvdt:4:5)
135200151119     c     ktbe          chain     tntbe01l
135300151119    4c                   if        %found(tntbe01l)
135400151119     c                   eval      w02dmtvdt=tbeuni
135500151119    4c                   endif
135600151119    4c                   if        §ar5dimtv >*zeros
135700151119     c                   movel     §ar5dimtv     w0080             8 0
135800151119     c     *iso          movel     w0080         dataiso
135900151119     c                   eval      data_eur=dataiso
136000151119     c                   movel     data_eur      w02dimtv
136100151119     c                   eval      w02ddtil='Inserito il'
136200151119    4c                   endif
136300151119    3c                   endif
136400151119    2c                   endif
136500120712    1c                   endif
136600120829     c* se non presente data scannerizzazione visualizzo quella della figlia eventuale
136700120829     c                   if        w02drid=*zeros
136800120829     c                   exsr      replbl
136900120829     c                   if        wtrovl=*on
137000120829     c                   seton                                        67
137100120829     c                   movel     §ar5jdrid     w0080             8 0
137200120829     c     *iso          movel     w0080         dataiso
137300120829     c                   eval      data_eur=dataiso
137400120829     c                   movel     data_eur      w02drid
137500120829     c                   eval      w02lbl='Bolla legata'
137600120829     c                   endif
137700120829     c                   endif
137800121217     c* bolla orignale / di recupero per variazioni post fatturazione
137900121217     c                   if        w001aleg=*blanks
138000121217     c                   setoff                                       71
138100121217     c                   Eval      war5Trd = 'LEG'
138200121217     c     kSpeAr5       Chain     Fiar531c
138300121217If  1c                   If        %Found(Fiar531c)
138400121217     c                   seton                                        71
138500121217     c                   eval      waas_leg=%dec(%subst(ar5uni:1:4):4:0)
138600121217     c                   eval      wlnp_leg=%dec(%subst(ar5uni:5:3):3:0)
138700121217     c                   eval      wnrs_leg=%dec(%subst(ar5uni:8:2):2:0)
138800121217     c                   eval      wnsp_leg=%dec(%subst(ar5uni:10:7):7:0)
138900121217     c                   clear                   v1cleg
139000121217     c                   if        §tbrbl='R'
139100121217     c                   eval      v1dleg='Bolla Originale     (?)'
139200121217     c                   else
139300121217     c                   eval      v1dleg='Recupero Variazioni (?)'
139400121217     c                   endif
139500121217    1c                   endif
139600121217     c                   endif
139700040301
139800040301      * Se colli di valore visualizzo "V" nell'apposito campo
139900040301      *  con lo specifico attributo di visualizzazione
140000040603if  1c**!!!              if        §AR5bva = *blanks
140100040603     c**!!!              eval      V1Cbva  = *blanks
140200040603x   1c**!!!              else
140300040603     c**!!!              eval      V1Cbva  = 'V'
140400040603e   1c**!!!              endif
140500040603     c                   Eval      V1cBva = §FloTba
140600040301
140700030121      * Bancali
140800030703     c                   Clear                   V0cNba
140900030703     c                   Clear                   V0cNb2
141000030121     c                   Clear                   V0dTba
141100030121     c                   Clear                   V0dTb2
141200030703      * Bancali - part.varie "B"
141300030703If  0c                   If        %subst(TasGva:1:1) = 'B'
141400030121     c                   Clear                   dAr5Ban
141500030121     c                   Eval      wAr5Trd = 'BAN'
141600040603     c     kSpeAr5       Chain     Fiar531c
141700040603If  1c                   If        %Found(Fiar531c)
141800030121     c                   Movel     Ar5Uni        Dar5Ban
141900030121     c                   Eval      V0cNba = §Ar5Nba
142000030121     c                   Eval      V0cNb2 = §Ar5Nb2
142100030121If  2c                   If        §Ar5Tba <> *Zeros
142200030703     c                   Eval      wtba = §Ar5Tba
142300030703     c                   ExSr      Sr_DecTba
142400030703     c                   Eval      V0dTba = wdestba
142500030121    2c                   EndIf
142600030121If  2c                   If        §Ar5Tb2 <> *Zeros
142700030703     c                   Eval      wtba = §Ar5Tb2
142800030703     c                   ExSr      Sr_DecTba
142900030703     c                   Eval      V0dTb2 = wdestba
143000030121    2c                   EndIf
143100030121    1c                   EndIf
143200030703    0c                   EndIf
143300030703      * Bancali - part.varie "O"
143400030703If  0c                   If        %subst(TasGva:1:1) = 'O'
143500030703     c                   Clear                   dAr5Bnb
143600030703     c                   Eval      wAr5Trd = 'BNB'
143700040603     c     kSpeAr5       Chain     Fiar531c
143800040603If  1c                   If        %Found(Fiar531c)
143900030703     c                   Movel     Ar5Uni        Dar5Bnb
144000030703     c                   Eval      V0cNba = §Ar5bNba
144100030703     c                   Eval      V0cNb2 = §Ar5bNb2
144200030703If  2c                   If        §Ar5bTba <> *Zeros
144300030703     c                   Eval      wtba = §Ar5bTba
144400030703     c                   ExSr      Sr_DecTba
144500030703     c                   Eval      V0dTba = wdestba
144600030703    2c                   EndIf
144700030703If  2c                   If        §Ar5bTb2 <> *Zeros
144800030703     c                   Eval      wtba = §Ar5bTb2
144900030703     c                   ExSr      Sr_DecTba
145000030703     c                   Eval      V0dTb2 = wdestba
145100030703    2c                   EndIf
145200030703    1c                   EndIf
145300030703    0c                   EndIf
145400990701      * Colli, Peso e Volume
145500990702     c                   z-add     TASncl        V0Cncl
145600990630      *
145700990629     c                   movel     TASfpb        V1Cfpb
145800990629     c                   z-add     TASpkb        V1Cpkb
145900990629     c                   z-add     TASpkc        V1Cpkc
146000990630     c                   z-add     TASncp        V1Cncp
146100151016     c*//                movel     TASfpf        V0Cfpf
146200990702     c                   z-add     TASpkf        V0Cpkf
146201160608      *
146202160608     c                   movel     TASfvb        V1Cfvb
146203160608     c                   z-add     TASvlb        V1Cvlb
146204160608     c                   z-add     TASvlc        V1Cvlc
146205160608     c                   z-add     TASncr        V1Cncr
146206160608     c                   movel     TASfvf        V0Cfvf
146207160608     c                   z-add     TASvlf        V0Cvlf
146300030121      * Peso fatturato solo se la bolla è fatturarata
146400151016     c                   Clear                   V0Cfpf
146500030121     c                   Clear                   V0cPeso
146600151124If  1c***********        If        TasDft > *Zeros  or
146700151124     c                   if        TASimv > *Zeros
146800151020      *?Si reperiscono i dati da FIAR5/"FAT"?
146900151020      * non se è la seconda bolla di bolla doppia
147000151020     c                   clear                   dAR5fat
147100151020     c                   if        §3atb2=*blanks or
147200151020     c                             (%subst(§3atb1:1:1)=%subst(tastbl:1:1))
147300151020     c                   Eval      wAR5trd = 'FAT'
147400151020     c     kSpeAr5       Chain     Fiar531c
147500151020if  2c                   if        %Found(Fiar531c)
147600151020     c                   Movel     Ar5Uni        dAr5Fat
147700151125     c                   eval      v0cfpf=dar5fat.§ar5fptas
147800151125     c                   if        v0cfpf='Z' or v0cfpf='T'
147900151125     c                   eval      v0cfpf='V'
148000151126     c                   endif
148100151125     c                   eval      V0Cpeso = dAR5fat.§AR5pkTas
148200151020     c                   endif
148201160608      * volume desunto visualizzo come rilevato totale da VDL
148202160608     c                   if        dar5fat.§ar5fvtas = 'D'
148203160608     c                   eval      v1cvlc = dar5fat.§ar5vltas
148204160608     c                   eval      v0cvlf = dar5fat.§ar5vltas
148205160608     c                   eval      v0cfvf = 'T'
148206160608     c                   eval      v1cncr = v0cncl
148207160608     c                   endif
148208160608
148300151020     c                   endif
148400151125
148500151125     c                   if        v0cfpf=*blanks
148600151125     c                   movel     TASfpf        V0Cfpf
148700151125     c                   clear                   v0cpeso
148800151125     c                   endif
148900151020     c
149000151125      * -?Abilitazione Visualizzazione Pesi usati in Tassazione?
149001160505      * se utente EDP e peso o volume desunto
149100151125     c                   eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
149200160505     c                                        and   ( v0cfpf = 'D' or
149201160505     c                                        dar5fat.§ar5fvtas = 'D'))
149300030121      * Prendo il peso fatturato
149400151125     c***                If        TasFpf = 'R' or ( tasfpf='D' and
149500151125     c***                          dar5fat.§AR5PKTAS=0)
149600151125     c***                Z-Add     TasPkf        V0cPeso
149700151125     c***                movel     'R'           V0Cfpf
149800151125     c***                EndIf
149900151125      * VDL
150000030121      * Prendo il peso da cml - la tara
150100151126If  2c                   If        v0cfpf = 'V' and v0cpeso=0
150200030121     c                   Clear                   dsQt1
150300030121     c                   Movel     'QT'          Kcod
150400030121     c                   Movel     1             Kkey
150500030121     c     KeyTab        Chain     Tabel00f
150600030121     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
150700030121     c                   Movel     TblUni        dsQt1
150800030121     c                   EndIf
150900150717     c* se la data fattura è >0 e minore uguale  alla data in tabella,
151000150717     c*  sottraggo il campo TARA da TCO, altrimenti da TPC
151100150717     c                   if        tasdft>0 and §qtdst>0 and tasdft<=§qtdst
151200150717     c     §QtTco        Mult      TasNcp        wTara
151300150717     c                   else
151400030121     c     §QtTpc        Mult      TasNcp        wTara
151500150717     c                   endif
151600150717     c
151700030121     c     TasPkc        Sub(h)    wTara         wNtara
151800030121     c                   Z-Add     wNtara        V0cPeso
151900030121    2c                   EndIf
152000151125      * DESUNTO
152100151125     c                   if        v0cfpf='D'
152200151125     c                   if        v0cpeso=0
152300151125     c                   Z-Add     TasPkf        V0cPeso
152400151125     c                   movel     'R'           V0Cfpf
152500151125     c                   else
152600151125      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
152700151125      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
152800151125      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
152900151125      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
153000151125     c                   eval      V0Cfpf  = 'V'
153100151125     c                   eval      V1Cpkc  = V0Cpeso
153200151125     c                   eval      V1Cncp  = V0Cncl
153300151125     c                   endif
153400151125     c                   endif
153500151125      * REALE imposto il peso se non presente su record FAT
153600151125     c                   if        v0cfpf='R'
153700151125     c                             and v0cpeso=0
153800151125     c                   z-add     taspkf        v0cpeso
153900151125     c                   endif
154000151020     c
154100151020
154200151020      *?SE sped. tassata fino all'imponibile con Peso Desunto:?
154300151020      * -?Il Flag Fatturato = "D" (Desunto) diventa "V" (VDL)?
154400151125if  2c*                  if        dar5fat.§AR5PKTAS>0
154500151125     c*
154600151125if  3c*                  IF        dAR5fat.§AR5FPTAS='D'
154700151125     c*                  eval      V0Cfpf  = 'V'
154800151016      * -?Il Peso (§AR5pkTas) si mette sia come Fatturato che come Da VDL?
154900151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
155000151125     c*                  eval      V1Cpkc  = V0Cpeso
155100151016      * -?Il n° Colli da VDL diventa = n° Colli della bolla?
155200151125     c*                  eval      V1Cncp  = V0Cncl
155300151125   x3c*                  else
155400151020     c* se non è desunto--> imposto il fatturato da record FAT
155500151125     c*                  eval      V0Cpeso = dAR5fat.§AR5pkTas
155600151125    3c*                  endif
155700151125e   2c*                  endif
155800151020     c
155900151125     c****               eval      *in70  = ( %subst( KNMUS : 1 : 3 ) = 'EDP'
156000151125     c****                                    and     TASfpf = 'D' )
156100030121    1c                   EndIf
156900990701      * Merce Portata a Magazzino
157000990701     c                   IF        TASfdn = 'S'
157100990701     C                   eval      V0Dfdn = 'SI'
157200990701     c                   ENDIF
157300990701      * Fermo Deposito
157400990702     c                   IF        TASffd = 'S'
157500051107     C                   eval      V0Dffd = 'SI '
157600990701     c                   ENDIF
157700051107     c                   IF        TASffd = 'M'
157800051107     C                   eval      V0Dffd = 'SAB'
157900051107     c                   ENDIF
158000990716      * Anteporto
158100990716     C                   movel     tasfap        V0Cfap
158200990716      * Inoltro
158300990716     C                   movel     tasfin        V0Cfin
158400010221      * ORM
158500010221     C                   clear                   v12poe
158600010221     C                   clear                   v12nsr
158700010221     C                   clear                   v12nor
158800010221     C                   clear                   v12nrv
158900111103     c                   if        wta4not<>*blanks
159000111103     C                   movel     Wta4not       dsbl4m
159100111103     C                   move      §b4poe        v12poe
159200111103     C                   move      §b4nsr        v12nsr
159300111103     C                   move      §b4nor        v12nor
159400111103     C                   move      §b4nrv        v12nrv
159500111103     c                   endif
159600990630      *
159700990630     c                   ENDSR
159800990618      *****************************************************************
159900990630      * GESTIONE F2 = Comunicazione Anomalia
160000990618      *****************************************************************
160100990630     C     SUB_F02       BEGSR
160200990618      *
160300990705     c                   SELECT
160400990705      * Prima volta carico subfile
160500990705     c                   WHEN      WSB51D02 = *blanks
160600990705     c                   exsr      RIED02
160700990706     c                   eval      WSB51D02 = 'S'
160800990706      * Nesima volta con una sola C.A.: entrata diretta
160900990706     c                   WHEN      Wunaca = 'S'
161000990706     c                   exsr      RIED02_3
161100990706      * Nesima volta con più C.A.: gestione subfile
161200990705     c                   OTHER
161300990706     c                   exsr      RIED02_2
161400990705     c                   ENDSL
161500990705      *
161600990630     c                   ENDSR
161700990705      **********************************************************************
161800990705      * IMPOSTO CAMPI VIDEO INTERROGAZIONE C.A.
161900990705      **********************************************************************
162000990705     C     RIED02        BEGSR
162100990705      *
162200990705      * Pulizia subfile
162300990705     C                   eval      *in26 = *off
162400990705     C                   WRITE     SB51C11
162500990705     C                   eval      *in25 = *on
162600990705     C                   eval      *in26 = *on
162700990705     C                   CLEAR                   nrr11
162800990705      *
162900990705      * Carico il subfile
163000040809     C**""*Keyspe        setll     FNDCT02L
163100040809     C**""*Keyspe        READE     FNDCT02L
163200040809     C**""*              DOW       %EOF = *off
163300040809     C**""*              IF        DCTATB = *blanks
163400040809     C**""*              Z-ADD     DCTAAC        V11AAC
163500040809     C**""*              Z-ADD     DCTFIL        V11FIL
163600040809     C**""*              Z-ADD     DCTNCA        V11NCA
163700040809     C**""*              z-add     DCTaac        W0080
163800040809     C**""*              movel     DCTmgc        W0080
163900040809     C**""**usa          MOVEL     W0080         DATA_EUR
164000040809     C**""**eur          MOVEL     DATA_EUR      W0080
164100040809     C**""*              eval      V11dca = %EDITC(W0080:'Y')
164200040809     C**""*              Z-ADD     DCTFCA        V11FCA
164300040809     C**""*DCTfca        CHAIN     FNDFA000
164400040809     C**""*              if        %FOUND
164500040809     C**""*              movel     DFADES        V11DFA
164600040809     c**""*              endif
164700990705      *
164800040809     C**""*              add       1             NRR11
164900040809     C**""*              WRITE     SB51S11
165000040809     C**""*              ENDIF
165100990705      *
165200040809     C**""*Keyspe        READE     FNDCT02L
165300040809     C**""*              ENDDO
165400040809      *
165500040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
165600040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
165700040809      * legate alla spedizione che passo
165800040809     c                   clear                   fidn12ds
165900040809     c                   eval      i12aas = I51AAS
166000040809     c                   eval      i12lnp = I51LNP
166100040809     c                   eval      i12nrs = I51NRS
166200040809     c                   eval      i12nsp = I51NSP
166300040809     c                   eval      i12fel = 'E'
166400040809     c                   eval      i12fan = 'E'
166500040809     c*
166600040809     c                   call      'FIDN12R'
166700040809     c                   parm                    fidn12ds
166800040809     c*
166900040809     c* se non ci sono errori
167000040809     c                   if        o12err = ' '
167100040809     c* se numero di CA trovate maggiore di zero
167200040809     c                   if        o12nca > 0
167300040809     c                   z-add     1             ii                2 0
167400040809     c                   dow       ii <= o12nca
167500040809     c                   movea     skKey(ii)     dsKey
167600040809     C                   Z-ADD     dsaac         V11AAC
167700040809     C                   Z-ADD     dsfil         V11FIL
167800040809     C                   Z-ADD     dsnca         V11NCA
167900040809     c*
168000040809     c* Aggancio il FNDCT01L x reperire alcuni campi
168100040809     c     KDCT1_C       chain     FNDCT01L
168200040809     c                   if        %found(FNDCT01L)
168300040809     C                   z-add     DCTaac        W0080
168400040809     C                   movel     DCTmgc        W0080
168500040809     C     *usa          MOVEL     W0080         DATA_EUR
168600040809     C     *eur          MOVEL     DATA_EUR      W0080
168700040809     C                   eval      V11dca = %EDITC(W0080:'Y')
168800040809     c                   endif
168900040809     c*
169000040809     C                   Z-ADD     skfca(ii)     V11FCA
169100040809     C     skfca(ii)     CHAIN     FNDFA000
169200040809     C                   if        %FOUND
169300040809     C                   movel     DFADES        V11DFA
169400040809     c                   endif
169500040809      *
169600040809     C                   add       1             NRR11
169700040809     C                   WRITE     SB51S11
169800040809     c*
169900040809     c                   add       1             ii
170000040809     c                   enddo
170100040809     c*
170200040809     c                   endif
170300040809     c                   endif
170400040809     c**
170500121121      * se NRR11 = 0 vuol dire che non ho più la c.a. è già stata archiviata
170600121121     c                   IF        NRR11 = 0
170700121121     c                   eval      *in28 = *on
170800121121     c                   eval      V1Cmsg = 'C.A. già archiviata'
170900121121     c                   leavesr
171000121121     c                   ENDIF
171100990705      *
171200990705      * Gestione subfile o se ho una sola C.A. entrata diretta
171300990705     c                   IF        NRR11 = 1
171400990705     c                   clear                   fidn00ds
171500990706     c                   eval      Wunaca = 'S'
171600990705     c                   exsr      RIED02_1
171700990705     c                   ELSE
171800990706     c                   exsr      RIED02_2
171900990705     C                   ENDIF
172000990705      *
172100990705     c                   ENDSR
172200990705      **********************************************************************
172300990705      * RICHIAMO PGM DI VISUALIZZAZIONE C.A.
172400990705      **********************************************************************
172500990705     C     RIED02_1      BEGSR
172600990705      *
172700990705      * Clearizzo la DS fuori dalla routine per conservarla in caso di richiamo per una sola C.A.
172800990705     C                   Z-ADD     v11aac        i00aac
172900990705     C                   Z-ADD     V11FIL        I00fil
173000990705     C                   Z-ADD     V11NCA        I00NCA
173100990705     C                   Z-ADD     tasAAS        I00AAS
173200990705     C                   Z-ADD     tasNRS        I00NRS
173300990705     C                   Z-ADD     tasNSP        I00NSP
173400990705     C                   Z-ADD     tasLNP        I00LNP
173500990705     C                   MOVEL     'S'           I00TPB
173600990706      *
173700990706     c                   exsr      RIED02_3
173800990706      *
173900990706     c                   ENDSR
174000990705      *****************************************************************
174100990705      * GESTIONE SUBFILE PER INTERROGAZIONE C.A.
174200990705      **********************************************************************
174300990705     C     RIED02_2      BEGSR
174400990705
174500990705     c                   DO        *hival
174600990706     C                   eval      nrr11 = 1
174700990707     C                   clear                   W001A
174800990705      * Emetto video
174900990706     C                   WRITE     SB51T10
175000990706     C                   WRITE     SB51Z00
175100120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
175200990705     C                   EXFMT     SB51C11
175300120302     c                   else
175400120302     c                   write     sb51C11
175500120302     c                   read(E)   tnsb51d
175600120302     c                   if        %error
175700120302     c                   eval      *inkc='1'
175800120302     c                   leave
175900120302     c                   endif
176000120302     c                   endif
176100990706      * Premuto un tasto funzionale
176200990706     C                   IF         *IN27 = *ON
176300990706     c                   leave
176400990706     C                   ENDIF
176500990705      *
176600990705     C                   READC     SB51S11
176700990705     C                   DOW       %EOF = *off
176800990705      *
176900990705     C                   IF        V11SCE = '5'
177000990707     c                   eval      W001A = 'S'
177100990705     C                   clear                   fidn00ds
177200990705     C                   exsr      ried02_1
177300990705     C                   clear                   fidn00ds
177400990705     C                   clear                   V11SCE
177500990705     C                   update    SB51S11
177600990706      *   se premuto F3 esco dal programma
177700990706     c   KC              leave
177800990705     C                   ENDIF
177900990705      *
178000990705     C                   READC     SB51S11
178100990705     C                   ENDDO
178200990706      *   se non effettuate scelte forzo F12
178300990707     c                   If        W001A = 'S'
178400990706     c                   eval       *inkl = *on
178500990706     c                   clear                   INFDSFMT
178600990706     c                   Endif
178700120302
178800990706      *
178900990706     C                   ENDDO
179000990705      *
179100990630     c                   ENDSR
179200990706      *****************************************************************
179300990706      * RICHIAMO E GESTIONE DEL PGM DI INTERROGAZIONE C.A.
179400990706      *****************************************************************
179500990706     C     RIED02_3      BEGSR
179600990706      *
179700990706     C                   movel(P)  fidn00ds      kpjbu
179800990706      *
179900990706     C                   CALL      'FIDN01R'
180000990706     C                   PARM                    KPJBA
180100000309     C                   PARM                    callricors
180200990706      *
180300990706     C                   movel     Kpjbu         fidn00ds
180400990706      *
180500990706      * Se ritorna premuto F3 accendo *INKC
180600990706      * Altrimenti se ho richiamato direttamente la CA ritorno alla videata di apertura
180700990706     C                   SELECT
180800990707     C                   WHEN      O00F03 <> *blanks
180900990706     c                   eval       *inkc = *on
181000990706     c                   WHEN      Wunaca = 'S'
181100990706     c                   eval       *inkl = *on
181200990706     c                   clear                   INFDSFMT
181300990706     c                   OTHER
181400990706     c                   eval       *inkl = *on
181500990707     C                   ENDSL
181600990706      *
181700990706     c                   ENDSR
181800010221      *****************************************************************
181900010221      * GESTIONE F4 = ORM
182000010221      *****************************************************************
182100010221     C     SUB_F04       BEGSR
182200010221      *
182300010221     C                   clear                   PARM01
182400010221     C                   move      v12poe        ppoe
182500010221     C                   move      v12nor        pnor
182600010221     C                   move      v12nsr        pnsr
182700010221     C                   move      v12nrv        pnrv
182800010221     C                   move      '5'           psce
182900010221     C                   move      v0clnp        pfgs
183000010221     C                   movel(p)  PARM01        KPJBU
183100010221
183200010221     C                   call      'FIOR07R'
183300010221     C                   parm                    KPJBA
183400010221     c                   clear                   INFDSFMT
183500010221     c                   eval      *inkl = *on
183600010221      *
183700010221     c                   ENDSR
183800990630      *****************************************************************
183900990705      * GESTIONE F5 = Eventi
184000990630      *****************************************************************
184100990705     C     SUB_F05       BEGSR
184200990705      *
184300990813     c                   clear                   LRB5DS
184400990813     C                   Z-ADD     tasAAS        pb5AAS
184500990813     C                   Z-ADD     tasLNP        pb5LNP
184600990813     C                   Z-ADD     tasNRS        pb5NRS
184700990813     C                   Z-ADD     tasNSP        pb5NSP
184800990813     c                   movel     RSUT          pb5DSA
184900990813     c                   z-add     V0CDSP        pb5DSP
185000990813     c                   z-add     tasRMN        pb5RMN
185100990813     c                   z-add     tasLNA        pb5LNA
185200990813     c                   movel     V0DLNA        pb5LNAD
185300990813     c                   movel     tasRSD        pb5RSD
185400990813     c                   z-add     Wtasdcp       pb5DCP
185500990813      *
185600990813     C                   MOVEL(P)  LRB5DS        KPJBU
185700990813     C                   CALL      'FNLRB5R'
185800990813     C                   PARM                    KPJBA
185900990813     C                   MOVEL     KPJBU         LRB5DS
186000990813      *
186100990813      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
186200990813     C                   IF        pb5F03 = '1'
186300990813     c                   eval      *inkc = *on
186400990813     c                   ELSE
186500990813     c                   clear                   INFDSFMT
186600990813     c                   eval      *inkl = *on
186700990813     C                   ENDIF
186800990705      *
186900990705     c                   ENDSR
187000990705      *****************************************************************
187100990705      * GESTIONE F7 = Altri dati
187200990705      *****************************************************************
187300990705     C     SUB_F07       BEGSR
187400990705
187500990630      * Per il primo richiamo imposto i campi del video
187600990630     c                   IF        WSB51D07 = *blanks
187700990630     c                   exsr      ried07
187800990702     c                   eval      WSB51D07 = 'S'
187900990630     c                   ENDIF
188000990630
188100990707     c                   DO        *hival
188200990707
188300990630     c                   write     SB51T01
188400990630     c                   write     SB51Z00
188500120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
188600990630     c                   exfmt     SB51D07
188700120302     c                   else
188800120302     c                   write     sb51d07
188900120302     c                   read(E)   tnsb51d
189000120302     c                   if        %error
189100120302     c                   eval      *inkc='1'
189200120302     c                   leave
189300120302     c                   endif
189400120302     c                   endif
189500990707
189600990707     C                   clear                   W001A
189700120711      *
189800120711      * Interrogazione restituzione documenti al mittente
189900120711     c                   IF        *in67 and V7Idoc = '?'
190000120711     c                   exsr      sr_w02
190100120711     C                   clear                   V7Idoc
190200120711     C                   eval      W001A = 'S'
190300120711     C                   ENDIF
190400990630
190500990707      * Decodifica particolarità Giacenza
190600990707     c                   IF        V7Igga = '?'
190700990707     C                   clear                   ds7pqrs
190800990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
190900990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
191000990707     C                   movel     V7Cgga        DS7COD                         CODICE
191100990707     C                   movel(P)  DS7PQRS       KPJBU
191200990707     C                   CALL      'TRTB69R'
191300990707     C                   PARM                    KPJBA
191400990707     C                   clear                   V7Igga
191500990707     C                   eval      W001A = 'S'
191600990707     C                   ENDIF
191700990707
191800990707      * Decodifica particolarità Consegna
191900990707     c                   IF        V7Igma = '?'
192000990707     C                   clear                   ds7pqrs
192100990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
192200990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
192300990707     C                   movel     V7Cgma        DS7COD                         CODICE
192400990707     C                   movel(P)  DS7PQRS       KPJBU
192500990707     C                   CALL      'TRTB70R'
192600990707     C                   PARM                    KPJBA
192700990707     C                   clear                   V7Igma
192800990707     C                   eval      W001A = 'S'
192900990707     C                   ENDIF
193000990707
193100990707      * Decodifica particolarità Varie
193200990707     c                   IF        V7Igva = '?'
193300990707     C                   clear                   ds7pqrs
193400990707     C                   movel     'S'           DS7RIC                         'S'=RICHIAMATO
193500990707     C                   movel     'V'           DS7GES                         'V'=VISUALIZZA
193600990707     C                   movel     V7Cgva        DS7COD                         CODICE
193700990707     C                   movel(P)  DS7PQRS       KPJBU
193800990707     C                   CALL      'TRTB73R'
193900990707     C                   PARM                    KPJBA
194000990707     C                   clear                   V7Igva
194100990707     C                   eval      W001A = 'S'
194200990707     C                   ENDIF
194300990707
194400990707     c                   IF        W001A = *blanks
194500990707     c                   leave
194600990707     C                   ENDIF
194700990707
194800990707     c                   ENDDO
194900990707
195000990630     c                   ENDSR
195100990630      **********************************************************************
195200990630      * IMPOSTO CAMPI VIDEO ALTRI DATI
195300990630      **********************************************************************
195400990630     C     RIED07        BEGSR
195500990708      *
195600990630      * Codice bolla
195700990702     c                   IF        TASCBO <> *blanks
195800990630     c                   movel     tascbo        V7Ccbo
195900990630     C                   eval      KCOD = '3A'
196000990630     C                   movel(P)  tascbo        KKEY
196100990630     C                   EXSR      CHKTAB
196200990702     C  n30              movel     Wtbluni       V7Dcbo
196300990702     c                   ENDIF
196400990708      * P.O. Partenza
196500990708     c                   z-add     V0Clnp        V7Clnp
196600990708     c                   z-add     V0Clnp        Wlinea
196700990708     C                   EXSR      CHKORG
196800990708     C                   movel     orgdes        V7Dlnp
196900990708      * Terminal Partenza
197000990708     c                   IF        TAStfp <> *zeros
197100990708     c                   z-add     TAStfp        V7Ctfp
197200990708     c                   z-add     tastfp        Wlinea
197300990708     C                   EXSR      CHKORG
197400990708     C                   movel     orgdes        V7Dtfp
197500990708     c                   ENDIF
197600990708      * Terminal Arrivo
197700990708     c                   IF        TAStfa <> *zeros
197800990708     c                   z-add     TAStfa        V7Ctfa
197900990708     c                   z-add     tastfa        Wlinea
198000990708     C                   EXSR      CHKORG
198100990708     C                   movel     orgdes        V7Dtfa
198200990708     c                   ENDIF
198300990708      * 1o P.O. Transito
198400040603     c**!!!              IF        TASFLP <> *zeros
198500040603     c**!!!              eval      *in48 = *ON
198600040603     c**!!!              z-add     TASflp        V7Cflp
198700040603     c**!!!              z-add     tasflp        Wlinea
198800040603     C**!!!              EXSR      CHKORG
198900040603     C**!!!              movel     orgdes        V7Dflp
199000040603     c**!!!              ENDIF
199100990708      * 2o P.O. Transito, se non presente non emetto scritta
199200040603     c**!!!              IF        TASFL2 <> *zeros
199300040603     c**!!!              eval      *in49 = *ON
199400040603     c**!!!              z-add     TASfl2        V7Cfl2
199500040603     c**!!!              z-add     tasfl2        Wlinea
199600040603     C**!!!              EXSR      CHKORG
199700040603     C**!!!              movel     orgdes        V7Dfl2
199800040603     c**!!!              ENDIF
199900990630      * Sigla Operatore
200000990630     C                   movel     tassop        V7Csop
200100990630      * Trattamento merce
200200990630     C                   movel     tasctm        V7Cctm
200300990630      * Stampa lettera di vettura
200400990630     C                   SELECT
200500991123     C                   WHEN      TASLL1 = *blanks
200600990630     C                   WHEN      TASLL1 = 'C'  or  TASLL1 = 'K'
200700990630     C                   eval      V7Dll1 = 'CLI'
200800990630     C                   WHEN      TASLL1 = 'S'  or  TASLL1 = 'Y'
200900990630     C                   eval      V7Dll1 = 'PAR'
201000990630     c                   OTHER
201100990630     C                   eval      V7Dll1 = 'ARR'
201200990630     C                   ENDSL
201300110610      * email destinatario
201400110610     c                   setoff                                       65
201500110610      *
201600130926If  1c                   If        §floemd<>*blanks
201700110610     C                   SETON                                        65
201800130927     c                   select
201900130927     c                   when      §floemd='E'
202000130927     c                   Eval      V7cemd = 'MAIL+SMS'
202100130927     c                   when      §floemd='S'
202200130927     c                   Eval      V7cemd = 'MAIL    '
202300130927     c                   when      §floemd='X'
202400130927     c                   Eval      V7cemd = 'SMS     '
202500130927     c                   endsl
202600110610    1c                   EndIf
202700990630      * Bolla rientro
202800990630     C                   IF        TASfbr = 'S'
202900990630     C                   eval      V7Dfbr = 'SI'
203000990630     C                   ENDIF
203100990630      * Franco Frontiera
203200990630     C                   IF        TASfbr = 'F'
203300990630     C                   eval      V7Dffr = 'SI'
203400990630     C                   ENDIF
203500990630      * BAM si/no
203600991123     C                   SELECT
203700991123     C                   WHEN      TASLL1 = *blanks
203800991123     C                   WHEN      TASLL1 = 'C'  or  TASLL1 = 'S'
203900990630     C                   eval      V7Dddt = 'SI'
204000991123     C                   OTHER
204100990630     C                   eval      V7Dddt = 'NO'
204200991123     C                   ENDSL
204300990630      * Emessa Nota Accredito
204400990630     C                   IF        TASfa2 = 'S'
204500990630     C                   eval      V7Dfa2 = 'SI'
204600990630     C                   ENDIF
204700050620      * note partenza
204800050621     c                   setoff                                       58
204900050620     c                   eval      ktrc = '8'
205000050620     c                   clear                   v7dnt1
205100050620     c     KTAStrc       CHAIN     TITA430C
205200050620     c                   If        %FOUND
205300050620     c                   movel     TA4not        v7dnt1
205400050621     c                   seton                                        58
205500050620     c                   ENDIF
205600050620     c                   eval      ktrc = '9'
205700050620     c                   clear                   v7dnt2
205800050620     c     KTAStrc       CHAIN     TITA430C
205900050620     c                   If        %FOUND
206000050620     c                   movel     TA4not        v7dnt2
206100050621     c                   seton                                        58
206200050620     c                   ENDIF
206300990630      * Importo da assicurare e flag tipo inserimento
206400990630     c                   IF        TASias > *zeros
206500990630     c                   z-add     TASias        V7Cias
206600990630     c                   If        TASvas = *blanks
206700990630     c                   movel     'ITL'         V7Cvas
206800990630     c                   else
206900990630     c                   movel     TASvas        V7Cvas
207000990630     C                   Endif
207100990720     c                   eval      *IN50 = (TASfcm <> *blanks)
207200990630     C                   ENDIF
207300990630      * Particolarità
207400990630     c                   movel     TASgga        V7Cgga
207500990707     c                   eval      *in45 = (V7Cgga <> *blanks)
207600990630     c                   movel     TASgma        V7Cgma
207700990707     c                   eval      *in46 = (V7Cgma <> *blanks)
207800990630     c                   movel     TASgva        V7Cgva
207900990712     c                   move      ' '           V7Cgva
208000990707     c                   eval      *in47 = (V7Cgva <> *blanks)
208100990630      *
208200120330      * decopdifico motivo DDt errat
208300990630     c                   ENDSR
208400990630      *****************************************************************
208500000901      * GESTIONE F08 = Cod."CHI SONO"
208600990630      *****************************************************************
208700000901     C     SUB_F08       BEGSR
208800000901
208900000901      * Per il primo richiamo imposto i campi del video
209000000901     c                   IF        WSB51D08 = *blanks
209100000901     c                   exsr      ried08
209200000901     c                   eval      WSB51D08 = 'S'
209300000901     c                   ENDIF
209400000901
209500000901     c                   z-add     1             nrr08
209600000901
209700000901     C                   WRITE     SB51T10
209800000901     C                   WRITE     SB51Z00
209900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
210000000901     C                   EXFMT     SB51C08
210100120302     c                   else
210200120302     c                   write     sb51c08
210300120302     c                   read(E)   tnsb51d
210400120302     c                   if        %error
210500120302     c                   eval      *inkc='1'
210600120302     c                   endif
210700120302     c                   endif
210800000901
210900000901     C                   ENDSR
211000000901      *****************************************************************
211100000901      * IMPOSTO CAMPI VIDEO CODICE "CHI SONO"
211200000901      *****************************************************************
211300000901     C     RIED08        BEGSR
211400000901
211500000901      *
211600000901      * Pulizia subfile
211700000901     C                   eval      *in26 = *off
211800000901     C                   WRITE     SB51C08
211900000901     C                   eval      *in25 = *on
212000000901     C                   eval      *in26 = *on
212100000901     C                   clear                   nrr08
212200000901      *
212300020226     c                   eval      wtahtrc = 'C'
212400020226     C     KEYspeh       SETLL     TITAH30C
212500020226     C     KEYspeh       READE     TITAH30C
212600000901      *
212700000901     C                   DOW       %EOF = *off
212800000901     c                   move      tahnot        V08not
212900010524     c                   z-add     tahnsc        v08nsc
213000000901     C                   move      '7H'          kcod
213100000901     C                   movel(p)  tahtrc        kkey
213200000901     C     KEYTAB        CHAIN     TABEL00F                           30
213300000901     c                   IF        *IN30 = *ON
213400000901     C                   else
213500000901     C                   ENDIF
213600000901      *
213700000901     c                   ADD       1             nrr08
213800000901     c                   write     SB51s08
213900000901      *
214000020226     C     KEYspeh       READE     TITAH30C
214100000901     c                   ENDDO
214200000901
214300000901     c                   ENDSR
214400040603      *****************************************************************
214500040603      * GESTIONE F09 = Transiti
214600040603      *****************************************************************
214700040603     C     SUB_F09       BEGSR
214800040603
214900040603      * Per il primo richiamo imposto i campi del video
215000040603     c                   IF        WSB51D09 = *blanks
215100040603     c                   exsr      ried09
215200040603     c                   eval      WSB51D09 = 'S'
215300040603     c                   ENDIF
215400040603
215500040603     c                   z-add     1             nrr09
215600040603
215700040603     C                   WRITE     SB51T10
215800040603     C                   WRITE     SB51Z00
215900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
216000040603     C                   EXFMT     SB51C09
216100120302     c                   else
216200120302     c                   write     sb51c09
216300120302     c                   read(E)   tnsb51d
216400120302     c                   if        %error
216500120302     c                   eval      *inkc='1'
216600120302     c                   endif
216700120302     c                   endif
216800040603
216900040603     C                   ENDSR
217000040603      *****************************************************************
217100040603      * IMPOSTO CAMPI VIDEO TRANSITI
217200040603      *****************************************************************
217300040603     C     RIED09        BEGSR
217400040603
217500040603      * Pulizia subfile
217600040603     c                   eval      *in26 = *off
217700040603     c                   WRITE     SB51C09
217800040603     c                   eval      *in25 = *on
217900040603     c                   eval      *in26 = *on
218000040603     c                   clear                   nrr09
218100040603
218200040603      * Transiti da Fiar5 rcd TRS
218300040604     c                   Eval      woktrs = *Off
218400040603     c                   Eval      wAr5Trd = 'TRS'
218500040604     c     kSpeAr5       Setgt     Fiar531c
218600040604     c                   Do        *Hival
218700040604     c     kSpeAr5       Readpe    Fiar531c
218800040604     c                   If        %Eof(Fiar531c)
218900040604     c                   Leave
219000040604     c                   EndIf
219100040604     c                   Eval      woktrs = *On
219200040603     c                   Eval      dAr5Trs = Ar5Uni
219300040603      * 1° p.o. Transito
219400040603     c                   If        §Ar5Flp > *Zeros
219500040603     c                   Eval      V09Fil = §Ar5Flp
219600040603     c                   Z-add     §Ar5Flp       Wlinea
219700040603     c                   ExSr      CHKORG
219800040603     c                   Eval      V09Dfl = OrgDes
219900040603     c                   If        §Ar5Det > *Zeros
220000040604     c     *iso          Move      §Ar5Det       DATA_eur
220100040603     c     *eur          Move      DATA_eur      V09Det
220200040608     c                   Eval      *In48 = *Off
220300040603     c                   Else
220400040603     c                   Clear                   V09Det
220500040608     c                   Eval      *In48 = *On
220600040603     c                   EndIf
220700040603     c                   If        §Ar5Dut > *Zeros
220800040604     c     *iso          Move      §Ar5Dut       DATA_eur
220900040603     c     *eur          Move      DATA_eur      V09Dut
221000040608     c                   Eval      *In49 = *Off
221100040603     c                   Else
221200040603     c                   Clear                   V09Dut
221300040608     c                   Eval      *In49 = *On
221400040603     c                   EndIf
221500040603     c                   Add       1             nrr09
221600040603     c                   Write     SB51s09
221700040603     c                   EndIf
221800040603      * 2° p.o. Transito
221900040603     c                   If        §Ar5Fl2 > *Zeros
222000040603     c                   Eval      V09Fil = §Ar5Fl2
222100040603     c                   Z-add     §Ar5Fl2       Wlinea
222200040603     c                   ExSr      CHKORG
222300040603     c                   Eval      V09Dfl = OrgDes
222400040727     c                   If        §Ar5De2 > *Zeros
222500040604     c     *iso          Move      §Ar5De2       DATA_eur
222600040603     c     *eur          Move      DATA_eur      V09Det
222700040608     c                   Eval      *In48 = *Off
222800040603     c                   Else
222900040603     c                   Clear                   V09Det
223000040608     c                   Eval      *In48 = *On
223100040603     c                   EndIf
223200040727     c                   If        §Ar5Du2 > *Zeros
223300040604     c     *iso          Move      §Ar5Du2       DATA_eur
223400040603     c     *eur          Move      DATA_eur      V09Dut
223500040608     c                   Eval      *In49 = *Off
223600040603     c                   Else
223700040603     c                   Clear                   V09Dut
223800040608     c                   Eval      *In49 = *On
223900040603     c                   EndIf
224000040603     c                   Add       1             nrr09
224100040603     c                   Write     SB51s09
224200040603     c                   EndIf
224300040603      * 3° p.o. Transito
224400040603     c                   If        §Ar5Fl3 > *Zeros
224500040603     c                   Eval      V09Fil = §Ar5Fl3
224600040603     c                   Z-add     §Ar5Fl3       Wlinea
224700040603     c                   ExSr      CHKORG
224800040603     c                   Eval      V09Dfl = OrgDes
224900040727     c                   If        §Ar5De3 > *Zeros
225000040604     c     *iso          Move      §Ar5De3       DATA_eur
225100040603     c     *eur          Move      DATA_eur      V09Det
225200040608     c                   Eval      *In48 = *Off
225300040603     c                   Else
225400040603     c                   Clear                   V09Det
225500040608     c                   Eval      *In48 = *On
225600040603     c                   EndIf
225700040727     c                   If        §Ar5Du3 > *Zeros
225800040604     c     *iso          Move      §Ar5Du3       DATA_eur
225900040603     c     *eur          Move      DATA_eur      V09Dut
226000040608     c                   Eval      *In49 = *Off
226100040603     c                   Else
226200040603     c                   Clear                   V09Dut
226300040608     c                   Eval      *In49 = *On
226400040603     c                   EndIf
226500040608     c                   Add       1             nrr09
226600040603     c                   Write     SB51s09
226700040603     c                   EndIf
226800040603      * 4° p.o. Transito
226900040603     c                   If        §Ar5Fl4 > *Zeros
227000040603     c                   Eval      V09Fil = §Ar5Fl4
227100040603     c                   Z-add     §Ar5Fl4       Wlinea
227200040603     c                   ExSr      CHKORG
227300040603     c                   Eval      V09Dfl = OrgDes
227400040727     c                   If        §Ar5De4 > *Zeros
227500040604     c     *iso          Move      §Ar5De4       DATA_eur
227600040603     c     *eur          Move      DATA_eur      V09Det
227700040608     c                   Eval      *In48 = *Off
227800040603     c                   Else
227900040603     c                   Clear                   V09Det
228000040608     c                   Eval      *In48 = *On
228100040603     c                   EndIf
228200040727     c                   If        §Ar5Du4 > *Zeros
228300040604     c     *iso          Move      §Ar5Du4       DATA_eur
228400040603     c     *eur          Move      DATA_eur      V09Dut
228500040608     c                   Eval      *In49 = *Off
228600040603     c                   Else
228700040603     c                   Clear                   V09Dut
228800040608     c                   Eval      *In49 = *On
228900040603     c                   EndIf
229000040603     c                   Add       1             nrr09
229100040603     c                   Write     SB51s09
229200040603     c                   EndIf
229300040603      * 5° p.o. Transito
229400040603     c                   If        §Ar5Fl5 > *Zeros
229500040603     c                   Eval      V09Fil = §Ar5Fl5
229600040603     c                   Z-add     §Ar5Fl5       Wlinea
229700040603     c                   ExSr      CHKORG
229800040603     c                   Eval      V09Dfl = OrgDes
229900040727     c                   If        §Ar5De5 > *Zeros
230000040604     c     *iso          Move      §Ar5De5       DATA_eur
230100040603     c     *eur          Move      DATA_eur      V09Det
230200040608     c                   Eval      *In48 = *Off
230300040603     c                   Else
230400040603     c                   Clear                   V09Det
230500040608     c                   Eval      *In48 = *On
230600040603     c                   EndIf
230700040727     c                   If        §Ar5Du5 > *Zeros
230800040604     c     *iso          Move      §Ar5Du5       DATA_eur
230900040603     c     *eur          Move      DATA_eur      V09Dut
231000040608     c                   Eval      *In49 = *Off
231100040603     c                   Else
231200040603     c                   Clear                   V09Dut
231300040608     c                   Eval      *In48 = *On
231400040603     c                   EndIf
231500040603     c                   Add       1             nrr09
231600040603     c                   Write     SB51s09
231700040603     c                   EndIf
231800040604     c                   Leave
231900040604     c                   EndDo
232000040603
232100040603      * Transiti da TITAS
232200040604     c                   If        woktrs = *Off
232300040603      * 1° p.o. Transito
232400040603     c                   If        TasFlp > *Zeros
232500040603     c                   Eval      V09Fil = TasFlp
232600040603     c                   Z-add     TasFlp        Wlinea
232700040603     c                   ExSr      CHKORG
232800040603     c                   Eval      V09Dfl = OrgDes
232900040603     c                   Clear                   V09Det
233000040603     c                   Clear                   V09Dut
233100040608     c                   Eval      *In48 = *On
233200040608     c                   Eval      *In49 = *On
233300040603     c                   Add       1             nrr09
233400040603     c                   Write     SB51s09
233500040603     c                   EndIf
233600040603      * 2° p.o. Transito
233700040603     c                   If        TasFl2 > *Zeros
233800040603     c                   Eval      V09Fil = TasFl2
233900040603     c                   Z-add     TasFl2        Wlinea
234000040603     c                   ExSr      CHKORG
234100040603     c                   Eval      V09Dfl = OrgDes
234200040603     c                   Clear                   V09Det
234300040603     c                   Clear                   V09Dut
234400040608     c                   Eval      *In48 = *On
234500040608     c                   Eval      *In49 = *On
234600040603     c                   Add       1             nrr09
234700040603     c                   Write     SB51s09
234800040603     c                   EndIf
234900040603     c                   EndIf
235000040603
235100040603     C                   ENDSR
235200000901      *****************************************************************
235300000901      * GESTIONE F10 = Variazioni
235400000901      *****************************************************************
235500000901     C     SUB_F10       BEGSR
235600990804      *
235700990804     c                   clear                   TNSB01DS
235800990804     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
235900990804     C                   MOVEL     'F10'         D01OP1                         *LIVELLO PROGRAMMA
236000990804     C                   Z-ADD     tasAAS        d01AAS                         *ANNO
236100990804     C                   Z-ADD     tasLNP        d01LNP                         *LNP
236200020402     C                   Z-ADD     tasLNA        d01LNA                         *LNA
236300990804     C                   Z-ADD     tasNRS        d01NRS                         *SERIE
236400990804     C                   Z-ADD     tasNSP        d01NSP                         *SPEDIZIONE
236500990804     C                   Z-ADD     V0CDSP        D01DSP                         * GG/MM/AAAA
236600990804     C                   MOVEL     '0'           D01F03                         *USCITA PGM
236700990804     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
236800990804     C                   MOVEL     '0'           D01ERR                         *ERRORE
236900990804      *
237000990804     C                   MOVEL     TNSB01DS      KPJBU
237100990804      *
237200990804     C                   CALL      'TNSB53R'
237300990804     C                   PARM                    KPJBA
237400990804      *
237500990804     C                   MOVEL     KPJBU         TNSB01DS
237600990804      *
237700990804      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
237800990804     C                   IF        D01F03 = '1'
237900990804     c                   eval      *inkc = *on
238000990804     c                   ELSE
238100990804     c                   clear                   INFDSFMT
238200990804     c                   eval      *inkl = *on
238300990804     C                   ENDIF
238400990630      *
238500990630     c                   ENDSR
238600030120      *****************************************************************
238700030120      * GESTIONE F11 = Note Supermercato
238800030120      *****************************************************************
238900030120     C     SUB_F11       BEGSR
239000030120      *
239100030121     c                   Clear                   FnLrb3Ds
239200030122     c                   Movel     RSUT          Rb3Rsu
239300030121     c                   Eval      Rb3Fil = Simfel
239400030121     c                   Eval      Rb3Aas = TasAas
239500030121     c                   Eval      Rb3Lnp = TasLnp
239600030121     c                   Eval      Rb3Nrs = TasNrs
239700030121     c                   Eval      Rb3Nsp = TasNsp
239800030127     c                   Z-Add     TasAas        Rb3Dsp
239900030127     c                   movel     TasMgs        Rb3Dsp
240000030127     c     *usa          move      Rb3Dsp        DATA_eur
240100030127     c     *eur          move      DATA_eur      Rb3Dsp
240200030121     c                   Eval      Rb3Rmn = TasRmn
240300030121     c                   Eval      Rb3Lna = TasLna
240400030121     c                   Eval      Rb3Rsd = TasRsd
240500030121     c                   Eval      Rb3Pkf = TasPkf
240600030121     c                   Eval      Rb3Ncl = TasNcl
240700030121     c                   Eval      Rb3Vlb = TasVlb
240800030703      * bancali - part.varie "B"
240900030703If  0c                   If        %subst(TasGva:1:1) = 'B'
241000030121     c                   Eval      Rb3Tba = §Ar5Tba
241100030121     c                   Eval      Rb3Nba = V0cNba
241200030121     c                   Eval      Rb3Tb2 = §Ar5Tb2
241300030121     c                   Eval      Rb3Nb2 = V0cNb2
241400030703     c                   Eval      Rb3Gva = TasGva
241500030703    0c                   EndIf
241600030703If  0c                   If        %subst(TasGva:1:1) = 'O'
241700030703     c                   Eval      Rb3Tba = §Ar5bTba
241800030703     c                   Eval      Rb3Nba = V0cNba
241900030703     c                   Eval      Rb3Tb2 = §Ar5bTb2
242000030703     c                   Eval      Rb3Nb2 = V0cNb2
242100030703     c                   Eval      Rb3Gva = TasGva
242200030703     c                   Eval      Rb3Csf = §Ar5bCsf
242300030703     c                   Eval      Rb3Cor = §Ar5bCor
242400030703    0c                   EndIf
242500030121      * richiamo PGM per note supermercati
242600030121     c                   Call      'FNLRB3R'
242700030122     c                   Parm                    Kpjba
242800030121     c                   Parm                    Fnlrb3Ds
242900030121
243000030121     c                   clear                   INFDSFMT
243100030121     c                   eval      *inkl = *on
243200030121
243300030121     c                   EndSr
243400990630      *****************************************************************
243500990630      * GESTIONE F13 = Tassazione
243600990630      *****************************************************************
243700990630     C     SUB_F13       BEGSR
243800990701
243900990701      * Per il primo richiamo imposto i campi del video
244000990701     c                   IF        WSB51D13 = *blanks
244100990701     c                   exsr      ried13
244200990701     c                   eval      WSB51D13 = 'S'
244300990701     c                   ENDIF
244400990701
244500990705     c                   z-add     1             nrr13
244600990702
244700990701     c                   write     SB51T10
244800990701     c                   write     SB51Z00
244900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
245000990701     c                   exfmt     SB51C13
245100120302     c                   else
245200120302     c                   write     sb51c13
245300120302     c                   read(E)   tnsb51d
245400120302     c                   if        %error
245500120302     c                   eval      *inkc='1'
245600120302     c                   endif
245700120302     c                   endif
245800990701
245900990630     c                   ENDSR
246000990701      **********************************************************************
246100990701      * IMPOSTO CAMPI VIDEO TASSAZIONE
246200990701      **********************************************************************
246300990701     C     RIED13        BEGSR
246400070524
246500070524     c                   eval      *in61 = *off
246600070524     c                   eval      *in62 = *off
246700070524     c                   eval      *in63 = *off
246800151020     c                   clear                   v13peso
246900990701      *
247000990702      * Fatturata con tariffa Mittente
247100990702     c                   IF        TASftm = 'S'
247200990702     c                   eval      V13ftm = 'SI'
247300990702     c                   ENDIF
247400051103      * Peso fatturato
247500051103      * se bolla fatturata imposto il peso da fatturare che visualizzo in prima videata
247600151016     c                   If        TasDft > *Zeros  or
247700151016     c                             TASimv > *Zeros
247800051103     c                   Eval      v13peso = v0cpeso
247900151016     c*//                Else
248000151016      *//* se bolla da fatturare imposto taspkf
248100151016     c*//                Eval      v13peso = taspkf
248200051103     c                   EndIf
248300990702      * Peso tassabile
248400990702     c                   z-add     Taspvl        v13pvl
248500990702      * Quantità da fatturare
248600990702     c                   z-add     Tasqft        v13qft
248700990702      * Nazione e CAP fatturazione Mittente
248800990702     c                   movel     tasnzm        V13nzm
248900990702     c                   movel     tascam        V13cam
249000990702      * Codice tassazione Mittente
249100990702     c                   IF        TASmct <> *blanks
249200990702     c                   movel     tasmct        V13mct
249300990702     C                   eval      KCOD = 'CT'
249400990702     C                   movel(P)  tasmct        KKEY
249500990702     C                   EXSR      CHKTAB
249600990702     C  n30              movel     Wtbluni       V13dmt
249700990702     c                   ENDIF
249800990702      * Codice tassazione Destinatario
249900990702     c                   IF        TAScts <> *blanks
250000990702     c                   movel     tascts        V13cts
250100990702     C                   eval      KCOD = 'CT'
250200990702     C                   movel(P)  tascts        KKEY
250300990702     C                   EXSR      CHKTAB
250400990702     C  n30              movel     Wtbluni       V13dct
250500990702     c                   ENDIF
250600990702      * Codice tariffa
250700990702     c                   z-add     tasctr        v13ctr
250800990702     C                   eval      KCOD = 'TR'
250900990702     C                   movel     tasctr        w001a
251000990702     C                   movel(P)  W001A         KKEY
251100990702     C                   EXSR      CHKTAB
251200990702     C  n30              movel     Wtbluni       V13dtr
251300990702      * Divisa
251400990702     c                   movel     tasdiv        v13div
251500990708      * Importo fattura se fattura immediata
251600990708     c                   IF        *IN41 = *off
251700990708     c                   movel     V1Cimp        v13ift
251800990708     C                   ENDIF
251900990702      * Porto
252000990702     c                   IF        TASpor > *zeros
252100990702     c                   z-add     TASpor        W0133
252200990702     c                   movel     W§cvfdc       Wtstdec
252300990702     c                   exsr      EDTimporti
252400990702     c                   movel     IMPeditato    v13por
252500070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
252600070524     c                   eval      Ksvt = ' '
252700070524     c     Ktitai        chain     titai01l
252800070524     c                   if        %found(titai01l)
252900070524     c                   eval      *in61 = *on
253000070524     c                   endif
253100070524
253200990702     c                   ENDIF
253300990702      * Imponibile
253400990702     c                   IF        TASimv > *zeros
253500990702     c                   z-add     TASimv        W0133
253600990702     c                   movel     W§cvfdc       Wtstdec
253700990702     c                   exsr      EDTimporti
253800990702     c                   movel     IMPeditato    v13imv
253900070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
254000070524     c                   eval      Ksvt = '£'
254100070524     c     Ktitai        chain     titai01l
254200070524     c                   if        %found(titai01l)
254300070524     c                   eval      *in62 = *on
254400070524     c                   endif
254500990702     c                   ENDIF
254600990702      * Esenzione IVA
254700990702     c                   IF        TASfei <> *blanks
254800990702     c                   movel     tasfei        v13fei
254900990702     C                   eval      KCOD = 'EI'
255000990702     C                   movel(P)  tasfei        KKEY
255100990702     C                   EXSR      CHKTAB
255200990702     C  n30              movel     Wtbluni       V13dei
255300990702     c                   ENDIF
255400990702      * Recupero codice varia IVA
255500990702     C                   eval      KCOD = '$2'
255600990702     C                   movel(P)  1             KKEY
255700990702     C                   EXSR      CHKTAB
255800990702     C  n30              movel     Wtbluni       DS$2
255900990702      *
256000990702      * VARIE
256100990702      *    pulizia subfile
256200990702     C                   eval         *IN26 = *OFF
256300990702     C                   WRITE     SB51C13
256400990702     C                   eval         *IN25 = *ON
256500990702     C                   eval         *IN26 = *ON
256600990702      *    varia 1 TAS
256700990702     c                   IF        tassv1 <> *blanks
256800990702     c                   eval      v13svn = tassv1
256900990702     c                   z-add     TASva1        W0133
257000990702     c                   exsr      AGGIOSBF
257100990702     c                   ENDIF
257200990702      *    varia 2 TAS
257300990702     c                   IF        tassv2 <> *blanks
257400990702     c                   eval      v13svn = tassv2
257500990702     c                   z-add     TASva2        W0133
257600990702     c                   exsr      AGGIOSBF
257700990702     c                   ENDIF
257800990702      *    varia 3 TAS
257900990702     c                   IF        tassv3 <> *blanks
258000990702     c                   eval      v13svn = tassv3
258100990702     c                   z-add     TASva3        W0133
258200990702     c                   exsr      AGGIOSBF
258300990702      *    carico ulteriori varie
258400990702     C     keyspe1       setll     tita730c
258500990702     C     keyspe1       reade     tita730c
258600990705     C                   DOW       %EOF = *off
258700990702     c                   IF        TA7svn <> *blanks
258800990702     c                   eval      v13svn = ta7svn
258900990702     c                   z-add     TA7van        W0133
259000990702     c                   exsr      AGGIOSBF
259100990702     c                   ENDIF
259200990702     C     keyspe1       reade     tita730c
259300990702     c                   ENDDO
259400990702      *
259500990702     c                   ENDIF
259600990705      *    se non ci sono varie carico 1 rcd vuoto
259700990705     c                   IF        NRR13 = *ZEROS
259800990708     C                   clear                   v13svn
259900990708     C                   clear                   V13dsv
260000990708     c                   clear                   v13van
260100990705     c                   add       1             nrr13
260200990705     c                   write     sb51s13
260300990705     c                   ENDIF
260400990705      *
260500990702     c                   ENDSR
260600990702      *****************************************************************
260700990702      * CARICO SUBFILE VARIE
260800990702      *****************************************************************
260900990702     C     AGGIOSBF      BEGSR
261000990702      *
261100990702     c                   movel     W§cvfdc       Wtstdec
261200990702     c                   exsr      EDTimporti
261300990702      *
261400990702      * La varia IVA la scrivo nel control e non nel subfile
261500990702     C                   IF        §$2iva = v13SVN
261600990702     c                   movel     IMPeditato    v13iva
261700990702     c                   ELSE
261800990705     c                   movel     IMPeditato    v13van
261900990702      *
262000990702     C                   eval      KCOD = 'CC'
262100990702     C                   movel(P)  'VARIE'       KKEY
262200990702     C                   move      v13svn        KKEY
262300990702     C                   EXSR      CHKTAB
262400020328     C                   movel     TBLuni        DSCC
262500020328     C     §ccdfe        ifeq      'S'
262600020328      *
262700020328      * Verifico se si tratta di una bolla Fedex
262800021119     c                   if        LnaNtw = 'FED'
262900020328      * recupero decodifica da tabella CB
263000020328     C                   eval      KCOD = 'CB'
263100020328     C                   movel(P)  'VARIE'       KKEY
263200020328     C                   move      V13svn        KKEY
263300020328     C                   EXSR      CHKTAB
263400020328     c                   endif
263500020328     c                   endif
263600020328      *
263700990702     C  n30              movel     Wtbluni       V13dsv
263800070524      * verifico se c'è importo su titai .. tassato prima della fatturazione
263900070524     c                   eval      Ksvt = v13svn
264000070524     c     Ktitai        chain     titai01l
264100070524     c                   if        %found(titai01l)
264200070524
264300070524     c                   if        §$2iva = v13svn
264400070524     c                   eval      *in63 = *on
264500070524     c                   else
264600070524     c                   eval      *in64 = *on
264700070524     c                   endif
264800070524
264900070524     c                   else
265000070524     c                   eval      *in64 = *off
265100070524     c                   endif
265200070524
265300990702      *
265400990705     c                   add       1             nrr13
265500990702     c                   write     sb51s13
265600990702      *
265700990702     c                   ENDIF
265800990702      *
265900990701     c                   ENDSR
266000041007      *****************************************************************
266100041007      * GESTIONE F01 = Aggiunta Richiesta Assistenza
266200041007      *****************************************************************
266300041007     C     SUB_F01       BEGSR
266400041007      *
266500041007     c                   clear                   FIDNA1ds
266600041007     c                   eval      IA1ins  = 'S'
266700041007     c                   eval      IA1tor  = 'S'
266800041007     c                   eval      IA1ogg  = ds_OggSped
266900041007      *
267000041007     C                   ENDSR
267100990630      *****************************************************************
267200990630      * GESTIONE F14 = Giacenze
267300990630      *****************************************************************
267400990630     C     SUB_F14       BEGSR
267500990630      *
267600990901     c                   clear                   FNLG30DS
267700990901     C                   MOVEL     'S'           D30POA
267800990901     C                   MOVEL     tasAAS        D30AAS
267900990901     C                   MOVEL     tasLNP        D30LNP
268000990901     C                   MOVEL     tasNRS        D30NRS
268100990901     C                   MOVEL     tasNSP        D30NSP
268200990901     C                   MOVEL     FNLG30DS      KPJBU
268300990901     C                   SETON                                        31
268400990901     C                   CALL      'FNLG30C'
268500990901     C                   PARM                    KPJBA
268600990901      *
268700990901      *  ritorno alla videata di apertura
268800990901     c                   clear                   INFDSFMT
268900990701     c                   EVAL      *inkl = *on
269000990630      *
269100990630     c                   ENDSR
269200040416      *****************************************************************
269300040416      * GESTIONE F15 = Gestione Reclami
269400040416      *****************************************************************
269500040416     C     SUB_F15       BEGSR
269600040416      *
269700040416     c                   reset                   FIDNA2ds
269800040416     c                   eval      IA2ogg  = ds_OggSped
269900040416     c                   call      'FIDNA2R'
270000040416     c                   parm                    KPJBA
270100040416     c                   parm                    FIDNA2ds
270200040416      *
270300040416      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
270400040416     c                   if        OA2f03 = *on
270500040416     c                   eval      *inKC  = *on
270600040416     c                   else
270700040416      *  ritorno alla videata di apertura
270800040416     c                   clear                   InfDSfmt
270900040416     c                   eval      *inKL  = *on
271000040416     c                   endif
271100040416      *
271200040416     c                   ENDSR
271300990630      *****************************************************************
271400990630      * GESTIONE F16 = Competenze C/E
271500990630      *****************************************************************
271600990630     C     SUB_F16       BEGSR
271700990701      *
271800990701     c                   clear                   TNSB01DS
271900990701     C                   MOVEL     ' 05'         D01OP0                         *LIVELLO PROCEDURA
272000990701     C                   MOVEL     'F16'         D01OP1                         *LIVELLO PROGRAMMA
272100990701     C                   MOVEL     '0'           D01F03                         *USCITA PGM
272200990701     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
272300990701     C                   MOVEL     '0'           D01ERR                         *ERRORE
272400990701     C                   Z-ADD     TASaas        D01AAS                         *ANNO
272500990701     C                   Z-ADD     taslnp        D01LNP                         *LNP
272600990701     C                   Z-ADD     tasnrs        D01NRS                         *SERIE
272700990701     C                   Z-ADD     tasnsp        D01NSP                        *SPEDIZIONE
272800990701     C                   MOVEL     tastbl        D01TBL                        *TIPO BOLLA
272900990701     C                   Z-ADD     V0Cdsp        D01DSP                        *DATA SPEDIZIONE
273000990701      *
273100990705     C                   MOVEL(P)  TNSB01DS      KPJBU
273200990701     C                   CALL      'TNSB08R'
273300990701     C                   PARM                    KPJBA
273400990701     C                   MOVEL     KPJBU         TNSB01DS
273500990701      *
273600990701      * Se ritorna premuto F3 vado a fine programma  altrimenti ritorno alla videata di apertura
273700990707     C                   IF        D01F03 = '1'
273800990701     c                   eval      *inkc = *on
273900990707     c                   ELSE
274000990701     c                   clear                   INFDSFMT
274100990707     c                   eval      *inkl = *on
274200990707     C                   ENDIF
274300990701      *
274400990630     c                   ENDSR
274500000417      *****************************************************************
274600000417      * GESTIONE F17 = Richiesta rigenerazione bolle
274700000417      *****************************************************************
274800000417     C     SUB_F17       BEGSR
274900000417      *
275000000418     c                   clear                   tibs20ds
275100000418     C                   MOVEL     'W'           i20VID
275200000418     C                   Z-ADD     tasAAS        i20AAS
275300000418     C                   Z-ADD     tasLNP        i20LNP
275400000418     C                   Z-ADD     tasNRS        i20NRS
275500000418     C                   Z-ADD     tasNSP        i20NSP
275600000418     C                   Z-ADD     tasLNA        i20LNA
275700000418     C                   MOVEL(P)  tibs20ds      KPJBU
275800000417      *
275900000418     C                   CALL      'TIBS20R'
276000000418     C                   PARM                    KPJBA
276100000418     C                   MOVEL     KPJBU         tibs20ds
276200000418      *
276300000418      * Ritorno alla videata di apertura
276400000418     c                   clear                   INFDSFMT
276500000418     c                   eval      *inkl = *on
276600000418      *
276700000417     c                   ENDSR
276800990630      *****************************************************************
276900990630      * GESTIONE F18 = Legami
277000990630      *****************************************************************
277100990630     C     SUB_F18       BEGSR
277200030402
277300030402      * Per il primo richiamo imposto i campi del video
277400030402     c                   If        Wsb51d18 = *Blanks
277500030402     c                   ExSr      Ried18
277600030402     c                   Eval      Wsb51d18 = 'S'
277700030402     c                   EndIf
277800030402
277900030402      * Gestione subfile
278000030402     c                   If        NRR18 > 0
278100030402     c                   ExSr      Ried18_1
278200040518     c                   Else
278300040518     c                   Eval      *Inks = *Off
278400030402     c                   EndIf
278500990630      *
278600990630     c                   ENDSR
278700030402      *****************************************************************
278800030402      * IMPOSTO CAMPI VIDEO LEGAMI BOLLA
278900030402      *****************************************************************
279000030402     c     Ried18        BegSr
279100030402
279200030402      * Pulizia subfile
279300030402     c                   Eval      *In26 = *Off
279400030402     c                   Write     Sb51c18
279500030402     c                   Eval      *In25 = *On
279600030402     c                   Eval      *In26 = *On
279700030402     c                   Clear                   Nrr18
279800030402
279900030402      * Carico il subfile
280000030402      * uso la bolla come figlia o come mamma per cercare la bolla originale
280100030402     c     Keyspe        Chain     Fnlbl01l                           33
280200030402     c   33Keyspe        Chain     Fnlbl02l                           33
280300040517     c                   If        *In33
280400040517     c                   Eval      *Inks = *Off
280500040517     c                   EndIf
280600030402If  1c                   If        Not *In33
280700030402
280800030402      * con la bolla originale leggo la prima bolla della lista
280900031029     c**!!!              Eval      kLblAao = LblAao
281000031029     c**!!!              Eval      KlblLpo = LblLpo
281100031029     c**!!!              Eval      kLblNro = LblNro
281200031029     c**!!!              Eval      kLblNso = LblNso
281300031029     c**!!!keyspelbl     Chain     Fnlbl02l                           33
281400030402
281500030402      * Imposto subito nel sbfl la bolla originale
281600031029If  2c**!!!              If        Not *In33
281700030402     c                   Eval      v18Aap = LblAao
281800030402     c                   Eval      v18Lpp = LblLpo
281900030402     c                   Eval      v18Lap = LblLao
282000030402     c                   Eval      v18Nrp = LblNro
282100030402     c                   Eval      v18Nsp = LblNso
282200030402     c                   Clear                   v18Aan
282300030402     c                   Clear                   v18Lpn
282400030402     c                   Clear                   v18Lan
282500030402     c                   Clear                   v18Nrn
282600030402     c                   Clear                   v18Nsn
282700030402     c     'ORIGINAL'    CAT       'E':0         v18Pre
282800030402     c                   Clear                   v18Flg
282900030402     c                   Clear                   v18Npre
283000030402     c                   Clear                   v18Sce
283100030402     c                   Eval      *In53 = *Off
283200030402     c                   Eval      *In54 = *On
283300030402     c                   Eval      *In56 = *Off
283400030402
283500030402      * Controllo se la bolla trovata è quella richiesta dall'utente
283600030402If  3c                   If        LblAao = I51Aas and LblLpo = I51Lnp and
283700030402     c                             LblNro = I51Nrs and LblNso = I51Nsp
283800030402     c                   Eval      *In53 = *On
283900030402     c                   Eval      *In56 = *On
284000030402    3c                   EndIf
284100030402      * Controllo se abilitare o meno la scelta di visualizzazione
284200030402      * la bolla originale non è quella richiesta dall'utente
284300030402      * verifico se esiste in archivio
284400030403If  3c                   If        Not *In53
284500030402     c                   Eval      v18Flg = 'O'
284600030402     c                   Eval      kTasAas = LblAao
284700030402     c                   Eval      KTasLnp = LblLpo
284800030402     c                   Eval      kTasNrs = LblNro
284900030402     c                   Eval      kTasNsp = LblNso
285000030402     c     keyspe2       Setll     Titas30c
285100030402     c                   If        Not %Equal(Titas30c)
285200030402     c                   Eval      v18Npre = 'Non Presente'
285300030402     c                   Eval      *In56 = *On
285400030402     c                   EndIf
285500030403      * e non è la prima bolla richiesta
285600030403If  4c                   If        Not *In56 and
285700030403     c                             LblAao = SavAas and LblLpo = SavLnp and
285800030403     c                             LblNro = SavNrs and LblNso = SavNsp
285900030403     c                   Eval      *In56 = *On
286000030403    4c                   EndIf
286100030402    3c                   EndIf
286200030402
286300030402     c                   Eval      Nrr18 = 1
286400030402     c                   Write     Sb51s18
286500030402
286600030402     c                   Eval      *In54 = *Off
286700030402     c                   Eval      *In33 = *Off
286800031029    2c**!!!              EndIf
286900031029
287000031029      * con la bolla originale leggo la prima bolla della lista
287100031029     c                   Eval      kLblAao = LblAao
287200031029     c                   Eval      KlblLpo = LblLpo
287300031029     c                   Eval      kLblNro = LblNro
287400031029     c                   Eval      kLblNso = LblNso
287500031029     c     keyspelbl     Chain     Fnlbl02l                           33
287600030402      * Ciclo fino all'ultimo legame
287700030402      * lavorando solo sui campi "BOLLA SEGUENTE"
287800030402     c                   Clear                   wa
287900030402     c                   Clear                   wb
288000030402     c                   Clear                   Aan
288100030402Do  2c                   Dow       Not *In33
288200030402     c                   Eval      v18Aap = LblAap
288300030402     c                   Eval      v18Lpp = LblLpp
288400030402     c                   Eval      v18Lap = LblLap
288500030402     c                   Eval      v18Nrp = LblNrp
288600030402     c                   Eval      v18Nsp = LblNsp
288700030402     c                   Eval      v18Aan = LblAan
288800030402     c                   Eval      v18Lpn = LblLpn
288900030402     c                   Eval      v18Lan = LblLan
289000030402     c                   Eval      v18Nrn = LblNrn
289100030402     c                   Eval      v18Nsn = LblNsn
289200030402     c                   Clear                   v18pre
289300030402     c                   Clear                   v18Flg
289400030402     c                   Clear                   v18npre
289500030402     c                   Clear                   v18Sce
289600030402     c                   Eval      *In53 = *Off
289700030402     c                   Eval      *In55 = *Off
289800030402     c                   Eval      *In56 = *Off
289900030402
290000030402      * Controllo se la bolla precedente trovata è quella richiesta dall'utente
290100030402If  3c                   If        LblAap = I51Aas and LblLpp = I51Lnp and
290200030402     c                             LblNrp = I51Nrs and LblNsp = I51Nsp
290300030402     c                   Eval      *In53 = *On
290400030402     c                   Eval      v18Flg = 'P'
290500030402    3c                   EndIf
290600030402      * Controllo se la bolla seguente trovata è quella richiesta dall'utente
290700030402If  3c                   If        LblAan = I51Aas and LblLpn = I51Lnp and
290800030402     c                             LblNrn = I51Nrs and LblNsn = I51Nsp
290900030402     c                   Eval      *In55 = *On
291000030402     c                   Eval      v18Flg = 'S'
291100030403     c                   Eval      *In56 = *On
291200030402    3c                   EndIf
291300030402      * Controllo se abilitare o meno la scelta di visualizzazione
291400030403      * la bolla seguente non è quella che l'utente ha richiesto
291500030403If  3c                   If        Not *In55
291600030403     c                   Eval      kTasAas = LblAan
291700030403     c                   Eval      KTasLnp = LblLpn
291800030403     c                   Eval      kTasNrs = LblNrn
291900030403     c                   Eval      kTasNsp = LblNsn
292000030403      * verifico se esiste in archivio
292100030402     c     keyspe2       Setll     Titas30c
292200030402     c                   If        Not %Equal(Titas30c)
292300030402     c                   Eval      v18Npre = 'Non Presente'
292400030402     c                   Eval      *In56 = *On
292500030402     c                   EndIf
292600030403      * e se non è la prima bolla richiesta
292700030403If  4c                   If        Not *In56 and
292800030403     c                             LblAan = SavAas and LblLpn = SavLnp and
292900030403     c                             LblNrn = SavNrs and LblNsn = SavNsp
293000030403     c                   Eval      *In56 = *On
293100030403    4c                   EndIf
293200030402    3c                   EndIf
293300030402      * Scrivo il record nel sbfl
293400030402     c                   Add       1             Nrr18
293500030402     c                   Write     sb51s18
293600030402      * Aggiorno la schiera con le figlie
293700030402     c                   Add       1             wa                             INDICE SCHIERA
293800030402     c                   Eval      Aan(wa) = LblAan
293900030402     c                   Eval      Lpn(wa) = LblLpn
294000030402     c                   Eval      Nrn(wa) = LblNrn
294100030402     c                   Eval      Nsn(wa) = LblNsn
294200030402
294300030402     C     keyspelbl     Reade     Fnlbl02l                               33
294400030402
294500030402      * Se non trovo più bolle con la reade
294600030402      * utilizzo la figlia per leggere il legame successivo
294700030402Do  3c                   Dow       wb < wa  and %Eof(Fnlbl02l) = *On
294800030402     c                   Add       1             wb
294900030402     c                   Eval      kLblAao = Aan(wb)
295000030402     c                   Eval      kLblLpo = Lpn(wb)
295100030402     c                   Eval      kLblNro = Nrn(wb)
295200030402     c                   Eval      kLblNso = Nsn(wb)
295300030402     c     keyspelbl     Setll     Fnlbl02l
295400030402     c     keyspelbl     Reade     Fnlbl02l                               33
295500030402    3c                   EndDo
295600030402
295700030402    2c                   EndDo
295800030402    1C                   EndIf
295900030402
296000030402     c                   EndSr
296100030402      **********************************************************************
296200030402      * RICHIAMO PGM DI VISUALIZZAZIONE BOLLA LEGATA
296300030402      **********************************************************************
296400030402     c     Ried18_1      BegSr
296500030402
296600030402Do  1c                   Do        *Hival
296700030402     c                   Eval      Nrr18 = 1
296800030402     c                   Clear                   w001a18
296900030402      * Emetto Subfile
297000030402     c                   Write     Sb51t10
297100030402     c                   Write     Sb51z00
297200120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
297300030402     c                   Exfmt     Sb51c18
297400120302     c                   else
297500120302     c                   write     sb51c18
297600120302     c                   read(E)   tnsb51d
297700120302     c                   if        %error
297800120302     c                   eval      *inkc='1'
297900120302     c                   leave
298000120302     c                   endif
298100120302     c                   endif
298200030402      * Premuto un tasto funzionale
298300030402     c                   If         *In27 = *On
298400030402     c                   Leave
298500030402     c                   EndIf
298600030402      * Leggo il Subfile
298700030402     c                   Readc     Sb51s18
298800030402Do  2c                   Dow       %Eof = *Off
298900030402If  3c                   If        v18Sce = '5'
299000030402     c                   Eval      w001a18 = 'S'
299100030402      * passo il n. di spedizione da visualizzare
299200030403If  4c                   If        v18Flg = 'O'
299300030402     c                   Eval      I51Aas = v18Aap
299400030402     c                   Eval      I51Lnp = v18Lpp
299500030402     c                   Eval      I51Nrs = v18Nrp
299600030402     c                   Eval      I51Nsp = v18Nsp
299700030403   x4c                   Else
299800030402     c                   Eval      I51Aas = v18Aan
299900030402     c                   Eval      I51Lnp = v18Lpn
300000030402     c                   Eval      I51Nrs = v18Nrn
300100030402     c                   Eval      I51Nsp = v18Nsn
300200030402    4c                   EndIf
300300030402     c                   Clear                   v18Sce
300400030402     c                   Update    Sb51s18
300500030403     c                   Clear                   INFDSFMT
300600030402     c                   GoTo      Inizio
300700030402      * se premuto F3 esco dal programma
300800030402     c   kc              Leave
300900030402    3c                   EndIf
301000030402
301100030402     c                   Readc     Sb51s18
301200030402    2c                   EndDo
301300030402      * se non effettuate scelte forzo F12
301400030402     c                   If        w001a18 = *Blanks
301500030402     c                   Eval      *Inkl = *On
301600030402     c                   Clear                   INFDSFMT
301700030408     c                   Eval      w001a18 = 'S'
301800030402     c                   Leave
301900030402     c                   Endif
302000030402      *
302100030402    1c                   EndDo
302200030403
302300030418      * se premuto f12 ritorno alla prima bolla richiesta
302400030418     c                   If        *Inkl
302500030418     c                   Clear                   INFDSFMT
302600030418     c                   Eval      w001a18 = 'S'
302700030418     c                   EndIf
302800030402
302900030402     c                   EndSr
303000040119
303100040119      *****************************************************************
303200040119      * GESTIONE F19 = Immagine LdV
303300040119      *****************************************************************
303400040119     c     SUB_F19       BEGSR
303500040119      *
303600110927     c                   eval      c_tibs20=' '
303700110926     c                   select
303800040119      * Richiamo pgm. ANA670R per visualizzare la LdV
303900111129     c***                when      *inkt
304000111129     c                   when      *inkf
304100040119     c                   z-add     TASaas        Waas20
304200040119     c                   clear                   Parm670
304300040128     c                   movel(p)  'LDV'         Parm670(1)
304400040119     c                   movel(p)  ds_KEYsped    Parm670(2)
304500040119     c                   movel(p)  KNSif         Parm670(3)
304600040119      *
304700040331     ***c                   call      'ANA670R'
304800040331     c                   call      'YNA670R'
304900040119     c                   parm      '201'         Soc670
305000040122     c                   parm      'VIEW'        Cmd670
305100040122     c                   parm                    Parm670
305200040119     c                   parm      *off          Err670
305300040119     c                   parm      *blanks       Ret670
305400110927     c                   If        Ret670 = 'NULL '
305500110927     c                   eval      c_tibs20='S'
305600110927     c                   endif
305700110926      * Nuovo pgm di visualizzazione per il momento solo per profilo "EDP"
305800111129     c****               when      *inkf
305900111129     c                   when      *inkt
306000110927     C                   EVAL      CmdExc01 = 'ADDLIBLE LIB(SKGLIB) POSITION(*L+
306100110927     C                             AST)'
306200110927     C                   CALL(E)   'QCMDEXC'
306300110927     C                   PARM                    CmdExc01
306400110927     C                   PARM                    CmdExc02
306500110928     c                   z-add     TASaas        Waas20
306600110926     C                   EVAL      Barcode=ds_KEYsped
306700110928     c                   clear                   Errore
306800110928     c                   clear                   Descrizione
306900110926     C                   CALL(e)   'SKG670_BRT'
307000110926     C                   PARM                    Barcode          50
307100110926     C                   PARM                    Errore            1
307200110926     C                   PARM                    Descrizione      32
307300111006     c                   if        not %error and errore = '0'
307400110927     c                   eval      c_tibs20='S'
307500110928     c                   else
307600110928     c* Se errore di "Immagine non reperita" emetto window di avviso
307700111006     c                   if        errore='1'
307800111007     c                   eval      w01des=descrizione
307900120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
308000110928     c                   exfmt     sb51w01
308100120302     c                   else
308200120302     c                   write     sb51w01
308300120302     c                   read(E)   tnsb51d
308400120302     c                   if        %error
308500120302     c                   eval      *inkc='1'
308600120302     c                   leavesr
308700120302     c                   endif
308800120302     c                   endif
308900110928     c                   endif
309000110926     c                   endif
309100110926     c                   endsl
309200040511      * se la chiamata al pgm è andata a buon fine richiamo la rigenerazione bolla firmata
309300110927     c****               If        Ret670 = 'NULL '
309400110927     c                   if        c_tibs20='S'
309500040511     c                   Clear                   Tibs20ds
309600040511     c                   Eval      i20vid = 'S'
309700040511     c                   Eval      i20car = 'S'
309800040511     c                   Eval      i20aas = TasAas
309900040511     c                   Eval      i20lnp = TasLnp
310000040511     c                   Eval      i20nrs = TasNrs
310100040511     c                   Eval      i20nsp = TasNsp
310200040511     c                   Eval      i20lna = TasLna
310300040511     c                   Movel(p)  Tibs20ds      kpjbu
310400040511     c                   Call      'TIBS20R'
310500040511     c                   Parm                    kpjba
310600040511     c                   Eval      Tibs20ds = kpjbu
310700040511     c                   EndIf
310800040121      *
310900040128      *  ritorno alla videata di apertura
311000040128     c                   clear                   INFDSFMT
311100040121     c                   eval      *inKL = *on
311200040119      *
311300040119     c                   ENDSR
311400040119
311500990630      *****************************************************************
311600990630      * GESTIONE F20 = Affidabilità
311700990630      *****************************************************************
311800990630     C     SUB_F20       BEGSR
311900990701
312000990701      * Per il primo richiamo imposto i campi del video
312100990701     c                   IF        WSB51D20 = *blanks
312200990701     c                   exsr      ried12
312300990701     c                   eval      WSB51D20 = 'S'
312400990701     c                   ENDIF
312500990701
312600990701     c                   write     SB51T10
312700990701     c                   write     SB51Z00
312800120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
312900990701     c                   exfmt     SB51D12
313000120302     c                   else
313100120302     c                   write     sb51d12
313200120302     c                   read(E)   tnsb51d
313300120302     c                   if        %error
313400120302     c                   eval      *inkc='1'
313500120302     c                   endif
313600120302     c                   endif
313700990701
313800990701     c                   ENDSR
313900990701      **********************************************************************
314000990701      * IMPOSTO CAMPI VIDEO AFFIDABILITA'
314100990701      **********************************************************************
314200990701     C     RIED12        BEGSR
314300091211     c                   setoff                                       5960
314400990701      *
314500990701      * Data ritiro
314600990701     c                   IF        TASdrt > *zeros
314700990701     c     *iso          move      TASdrt        DATA_eur
314800990701     c     *eur          move      DATA_eur      W0080
314900990701     C                   eval      V12drt = %EDITC(W0080:'Y')
315000990701     c                   ENDIF
315100990701      * Numero ritiro
315200990701     c                   z-add     tasnrt        v12nrt
315300990701      * Prestazione Autotrasportatore
315400990701     c                   SELECT
315500990701     c                   WHEN      TASfpp = 'M'
315600990701     C                   eval      V12fpp = 'MATTINO'
315700990701     c                   WHEN      TASfpp = 'P'
315800990701     C                   eval      V12fpp = 'POMERIGGIO'
315900990701     c                   ENDSL
316000990701      * Stop Autotrasportatore
316100990701     c                   z-add     tasstr        v12str
316200990701      * Prestazione Autotrasportatore
316300990701     c                   z-add     taspdr        v12pdr
316400021203     C                   movel     tasprd        kpdpdr
316500021203     C                   move      'A'           kpdtip
316600021203     C     kfapd         chain     fiapd01L
316700000727     C                   if        %found
316800000727     C                   movel     apdrsc        v12rs1
316900000727     C                   else
317000000727     C                   clear                   v12rs1
317100000727     C                   endif
317200990701      * Data Immissione Bolla
317300990701     c                   IF        TASdim > *zeros
317400990701     c     *iso          move      TASdim        DATA_eur
317500990701     c     *eur          move      DATA_eur      W0080
317600990701     C                   eval      V12dim = %EDITC(W0080:'Y')
317700990701     c                   ENDIF
317800990701      * Data Borderò
317900990701     c                   IF        TASdbr > *zeros
318000990701     c     *iso          move      TASdbr        DATA_eur
318100990701     c     *eur          move      DATA_eur      W0080
318200990701     C                   eval      V12dbr = %EDITC(W0080:'Y')
318300990701     c                   ENDIF
318400990701      * Data Partenza Ultimo Collo
318500990701     c                   IF        TASduc > *zeros
318600990701     c     *iso          move      TASduc        DATA_eur
318700990701     c     *eur          move      DATA_eur      W0080
318800990701     C                   eval      V12duc = %EDITC(W0080:'Y')
318900990701     c                   ENDIF
319000091210      * Data Arrivo collo al terminal di arrivo--> ARBDAM
319100990701     c                   IF        TASdti > *zeros
319200990701     c     *iso          move      TASdti        DATA_eur
319300990701     c     *eur          move      DATA_eur      W0080
319400990701     C                   eval      V12dti = %EDITC(W0080:'Y')
319500990701     c                   ENDIF
319600090402      * Ora Arrivo Ultimo Collo . Es non la inviamo più in ttitas
319700090402     c****               IF        TAShti > *zeros
319800090402     c****               eval      *in42 = *on
319900090402     c****               z-add     TAShti        V12hti
320000090402     c****               ENDIF
320100091210     c
320200091210      * Data Arrivo all filiale di arrivo finale
320300091210      *           da Fiar500f record 'GEN'
320400091210     c                   Clear                   dAr5Gen
320500091210     c                   Eval      war5Trd = 'GEN'
320600091210     c     kSpeAr5       Chain     Fiar531c
320700091210If  1c                   If        %Found(Fiar531c)
320800091210     c                   Movel     Ar5Uni        Dar5Gen
320900091210     c                   IF        §ar5arrdt> *zeros
321000091210     c                   movel     §ar5arrdt     w0080
321100091210     c     *iso          move      w0080         DATA_eur
321200091210     c     *eur          move      DATA_eur      W0080
321300091210     C                   eval      V12arred = %EDITC(W0080:'Y')
321400091210     c* Ora arrivo reale
321500091210     c                   IF        §ar5arrhm> *zeros
321600091211     c                   eval      *in60 = *on
321700091210     c                   movel     §ar5arrhm     v12arreh
321800091210     c                   ENDIF
321900091210     c
322000091210     c                   ENDIF
322100091210     c                   ENDIF
322200091210     c
322300090402      * Data manifest per bolla export
322400090402     c                   clear                   v12dmf
322500090402     c                   if        lnantw='FED'  or lnantw='EEX' or
322600090402     c                             lnantw='DPD'
322700090402     c                   eval      ktrc = 'K'
322800090402     c     KTAStrc       CHAIN     TITA430C
322900090402     c                   If        %FOUND
323000090402     c                   movel     TA4not        dta4K
323100090402     c                   if        §ta4kdmf>0
323200090402     c     *iso          move      §ta4kdmf      DATA_eur
323300090402     c     *eur          move      DATA_eur      W0080
323400090402     C                   eval      V12dmf = %EDITC(W0080:'Y')
323500090402     c                   seton                                        59
323600090402     c                   ENDIF
323700090402     c                   ENDIF
323800090402     c
323900090402     c* ES: Dal 13 aprile 09 visulizzo data anche se vuota
324000090402     c                   if        not *in59  and tasdbr>20090413
324100090402     c                   seton                                        59
324200090402     c                   ENDIF
324300090402     c                   ENDIF
324400990701      * Consegne particolari
324500990701     c                   IF        TASFTC <> *BLANKS
324600990701     c                   movel     TASftc        V12ftc
324700990701     C                   eval      KCOD = '1P'
324800990701     C                   movel(P)  tasftc        KKEY
324900990701     C                   EXSR      CHKTAB
325000990701     c                   if        *in30 = *off
325100990702     C                   movel     Wtbluni       V12dtc
325200990701     c                   endif
325300990701     c                   ENDIF
325400990701      *
325500990701     c                   IF        TAStc2 <> *BLANKS
325600990701     c                   movel     TAStc2        V12tc2
325700990701     C                   eval      KCOD = '1P'
325800990701     C                   movel(P)  tastc2        KKEY
325900990701     C                   EXSR      CHKTAB
326000990701     c                   if        *in30 = *off
326100990701     C                   movel     Wtbluni       V12dc2
326200990701     c                   endif
326300990701     c                   ENDIF
326400990701      * Giorni chiusura
326500990701     c                   movel     TASgc1        V12gc1
326600990701     c                   movel     TASgc2        V12gc2
326700990701      * Data Consegna Teorica
326800990705     c                   IF        TASdce > *zeros
326900990705     c     *iso          move      TASdce        DATA_eur
327000990701     c     *eur          move      DATA_eur      W0080
327100990705     C                   eval      V12dce = %EDITC(W0080:'Y')
327200990701     c                   ENDIF
327300990705      * Ora Consegna Teorica
327400990705     c                   IF        TAShce > *zeros
327500990705     c                   eval      *in44 = *on
327600990705     c                   z-add     TAShce        V12hce
327700990705     c                   ENDIF
327800990702      * Tipo Data Consegna Richiesta
327900051129     c                   clear                   v12tcr
328000051129     c                   clear                   v12dcr
328100051129     c                   clear                   v12hcr
328200051129     c                   if        tasdcr>0 or tashcr>0
328300051129     c                   clear                   fnlv80ds
328400051129     c                   eval      ilv80tla='L'
328500051129     c                   eval      ilv80aas=tasaas
328600051129     c                   eval      ilv80lnp=taslnp
328700051129     c                   eval      ilv80nrs=tasnrs
328800051129     c                   eval      ilv80nsp=tasnsp
328900051129     c                   eval      ilv80mgs=tasmgs
329000051129     c                   eval      ilv80tc1=tasftc
329100051129     c                   eval      ilv80tc2=tastc2
329200051129     c                   eval      ilv80dcr=tasdcr
329300051129     c                   eval      ilv80hcr=tashcr
329400051129     c                   eval      ilv80tcr=tastcr
329500051129     c                   call      'FNLV80R'
329600051129     c                   parm                    kpjba
329700051129     c                   parm                    fnlv80ds
329800051129     c
329900051129     c                   eval      v12tcr=olv80tcrd
330000990702      * Data Consegna Richiesta
330100990702     c                   IF        TASdcr > *zeros
330200051129     C                   eval      V12dcr = %EDITC(olv80dcr8:'Y')
330300990702     c                   ENDIF
330400990702      * Ora Consegna Richiesta
330500990705     c                   IF        TAShcr > *zeros
330600990705     c                   eval      *in43 = *on
330700990705     c                   z-add     TAShcr        V12hcr
330800990705     c                   ENDIF
330900051129     c                   ENDIF
331000990702      * Stop consegna
331100990702     c                   z-add     tasstp        V12stp
331200990702      * Autotrasportatore
331300990702     c                   z-add     taspdc        V12pdc
331400021203     C                   movel     taspdc        kpdpdr
331500021203     C                   move      'A'           kpdtip
331600021203     C     kfapd         chain     fiapd01L
331700000727     C                   if        %found
331800000727     C                   movel     apdrsc        v12rs2
331900000727     C                   else
332000000727     C                   clear                   v12rs2
332100000727     C                   endif
332200990702      * Affidabilità
332300990702     c                   z-add     tasnpi        V12npi
332400990702     c                   z-add     tasnci        V12nci
332500990702     c                   z-add     tasnrc        V12nrc
332600010220      *
332700990630     c                   ENDSR
332800990630      *****************************************************************
332900990630      * GESTIONE F21 = Dettaglio Colli
333000990630      *****************************************************************
333100990630     C     SUB_F21       BEGSR
333200990705
333300990705      * Per il primo richiamo imposto i campi del video
333400990705     c                   IF        WSB51D21 = *blanks
333500990705     c                   exsr      ried21
333600990705     c                   eval      WSB51D21 = 'S'
333700990705     c                   ENDIF
333800990705
333900990705     c                   z-add     1             nrr21
334000990705
334100990705     C                   WRITE     SB51T10
334200990706     C                   WRITE     SB51Z00
334300120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
334400990705     C                   EXFMT     SB51C21
334500120302     c                   else
334600120302     c                   write     sb51c21
334700120302     c                   read(E)   tnsb51d
334800120302     c                   if        %error
334900120302     c                   eval      *inkc='1'
335000120302     c                   endif
335100120302     c                   endif
335200990705
335300990705     c                   ENDSR
335400990705      *****************************************************************
335500990705      * IMPOSTO CAMPI VIDEO DETTAGLIO COLLI
335600990705      *****************************************************************
335700990705     C     RIED21        BEGSR
335800990705
335900990630      *
336000990705      * Pulizia subfile
336100990705     C                   eval      *in26 = *off
336200990705     C                   WRITE     SB51C21
336300990705     C                   eval      *in25 = *on
336400990705     C                   eval      *in26 = *on
336500990705     C                   clear                   nrr21
336600990705      *
336700990705     C     KEYspe        SETLL     TITAT30C
336800990705     C     KEYspe        READE     TITAT30C
336900990705      *
337000990705     C                   DOW       %EOF = *off
337100990705     c                   z-add     tatfls        v21fls
337200990705     c                   z-add     tatnsc        v21nsc
337300990705     c                   If        TATdcm > *zeros
337400990705     c     *iso          move      TATdcm        DATA_eur
337500990705     c     *eur          move      DATA_eur      W0080
337600990705     C                   eval      V21dcm = %EDITC(W0080:'Y')
337700990705     c                   Else
337800990705     c                   movel     V0Cdcm        v21dcm
337900990705     c                   Endif
338000990630      *
338100990705     c                   ADD       1             nrr21
338200990705     c                   write     SB51s21
338300990705      *
338400990705     C     KEYspe        READE     TITAT30C
338500990705     c                   ENDDO
338600990705
338700990630     c                   ENDSR
338800990630      *****************************************************************
338900990630      * GESTIONE F22 = Contrassegno
339000990630      *****************************************************************
339100990630     C     SUB_F22       BEGSR
339200990630      *
339300990705     C                   clear                   tnsc40ds
339400990705     C                   Z-ADD     1             D40INT
339500990705     C                   Z-ADD     tasAAS        D40AAS
339600990705     C                   Z-ADD     tasLNP        D40LNP
339700990705     C                   Z-ADD     tasNRS        D40NRS
339800990705     C                   Z-ADD     tasNSP        D40NSP
339900990705     C                   MOVEL(P)  TNSC40DS      KPJBU
340000990705      *
340100990705     C                   CALL      'TNSC43R'
340200990705     C                   PARM                    KPJBA
340300990706      *  ritorno alla videata di apertura
340400990706     c                   clear                   INFDSFMT
340500990706     c                   eval      *inkl = *on
340600990706      *
340700990630     c                   ENDSR
340800020226      *****************************************************************
340900020226      * GESTIONE F23 = AWB FedEx
341000020226      *****************************************************************
341100020226     C     SUB_F23       BEGSR
341200020226      *
341300020226
341400020226      * Per il primo richiamo imposto i campi del video
341500020226     c                   IF        WSB51D23 = *blanks
341600020226     c                   exsr      ried23
341700020226     c                   eval      WSB51D23 = 'S'
341800020226     c                   ENDIF
341900020226
342000020226     c                   z-add     1             nrr23
342100020226
342200020226     C                   WRITE     SB51T10
342300020226     C                   WRITE     SB51Z00
342400120302     c                   if        %SUBST(I51OP0:1:1)<>'M'
342500020226     C                   EXFMT     SB51C23
342600120302     c                   else
342700120302     c                   write     sb51c23
342800120302     c                   read(E)   tnsb51d
342900120302     c                   if        %error
343000120302     c                   eval      *inkc='1'
343100120302     c                   endif
343200120302     c                   endif
343300020226      *
343400020226     c                   ENDSR
343500020226      *****************************************************************
343600020226      * IMPOSTO CAMPI VIDEO AWB FEDEX
343700020226      *****************************************************************
343800020226     C     RIED23        BEGSR
343900020226
344000020226      *
344100020226      * Pulizia subfile
344200020226     C                   eval      *in26 = *off
344300020226     C                   WRITE     SB51C23
344400020226     C                   eval      *in25 = *on
344500020226     C                   eval      *in26 = *on
344600020226     C                   clear                   nrr23
344700020226      *
344800020226     c                   eval      wtahtrc = 'F'
344900020226     C     KEYspeh       SETLL     TITAH30C
345000020226     C     KEYspeh       READE     TITAH30C
345100020226      *
345200020226     C                   DOW       %EOF = *off
345300020226     c                   move      tahnot        V23not
345400020226     c                   z-add     tahnsc        v23nsc
345500020226     C                   move      '7H'          kcod
345600020226     C                   movel(p)  tahtrc        kkey
345700020226     C     KEYTAB        CHAIN     TABEL00F                           30
345800020226     c                   IF        *IN30 = *ON
345900020226     C                   else
346000020226     C                   ENDIF
346100020226      *
346200020226     c                   ADD       1             nrr23
346300020226     c                   write     SB51s23
346400020226      *
346500020226     C     KEYspeh       READE     TITAH30C
346600020226     c                   ENDDO
346700020226
346800020226     c                   ENDSR
346900020226      *****************************************************************
347000020226      * GESTIONE F24 = Altri dati
347100020226      *****************************************************************
347200020226     C     SUB_F24       BEGSR
347300020226
347400030121     c                   Eval      *In24 = *Off
347500030121     c                   Clear                   Vtf
347600020226     c                   clear                   vtf01
347700020226     c                   clear                   vtf02
347800020226     c                   clear                   vtf03
347900020226     c                   clear                   vtf04
348000020226     c                   clear                   vtf05
348100030121     c                   Clear                   Vtf06
348200020226
348300030121      * prima emissione tasti funzione
348400030121  1b c                   if        wsb51z00 = 1
348500030121
348600030121     c                   Clear                   xx
348700030121     c                   Do        5             yy
348800030121     c                   Add       1             xx
348900030121     c     '1'           Lookup    SkOnOff(xx)                            32
349000030121     c                   If        *In32
349100030121     c                   Eval      Vtf(yy) = Tf(xx)
349200030121     c                   Else
349300030121     c                   Leave
349400030121     c                   EndIf
349500030121     c                   EndDo
349600030121
349700030121     c                   Eval      Vtf01 = vtf(01)
349800030121     c                   Eval      Vtf02 = vtf(02)
349900030121     c                   Eval      Vtf03 = vtf(03)
350000030121     c                   Eval      Vtf04 = vtf(04)
350100030121     c                   Eval      Vtf05 = vtf(05)
350200030121
350300030121     c                   If        wOnOff > 5
350400030121     c                   Eval      Vtf06 = Cf2412
350500030121     c                   If        wOnOff > 10
350600030121     c                   Eval      Vtf06 = Cf2413
350700030121     c                   EndIf
350800030121     c                   Eval      *In24 = *On
350900030121     c                   Eval      wsb51z00 = 2
351000030121     c                   Goto      EndF24
351100030121     c                   EndIf
351200030121
351300030121     c                   EndIf
351400020226
351500030121      * seconda emissione tasti funzione
351600030121  1b c                   if        wsb51z00 = 2
351700030121
351800030121     c                   Do        5             yy
351900030121     c                   Add       1             xx
352000030121     c     '1'           Lookup    SkOnOff(xx)                            32
352100030121     c                   If        *In32
352200030121     c                   Eval      Vtf(yy) = Tf(xx)
352300030121     c                   Else
352400030121     c                   Leave
352500030121     c                   EndIf
352600030121     c                   EndDo
352700030121
352800030121     c                   Eval      Vtf01 = vtf(01)
352900030121     c                   Eval      Vtf02 = vtf(02)
353000030121     c                   Eval      Vtf03 = vtf(03)
353100030121     c                   Eval      Vtf04 = vtf(04)
353200030121     c                   Eval      Vtf05 = vtf(05)
353300030121
353400030121     c                   If        wOnOff > 10
353500030121     c                   Eval      *In24 = *On
353600030121     c                   Eval      Vtf06 = Cf2423
353700030121     c                   Eval      wsb51z00 = 3
353800030121     c                   Goto      EndF24
353900030121     c                   EndIf
354000030121     c                   If        wOnOff > 5
354100030121     c                   Eval      *In24 = *On
354200030121     c                   Eval      Vtf06 = Cf2422
354300030121     c                   Eval      wsb51z00 = 1
354400030121     c                   Goto      EndF24
354500030121     c                   EndIf
354600030121
354700030121     c                   EndIf
354800030121
354900030121      * terza emissione tasti funzione
355000030121  1b c                   if        wsb51z00 = 3
355100030121
355200030121     c                   Do        5             yy
355300030121     c                   Add       1             xx
355400030121     c     '1'           Lookup    SkOnOff(xx)                            32
355500030121     c                   If        *In32
355600030121     c                   Eval      Vtf(yy) = Tf(xx)
355700030121     c                   Else
355800030121     c                   Leave
355900030121     c                   EndIf
356000030121     c                   EndDo
356100030121
356200030121     c                   Eval      Vtf01 = vtf(01)
356300030121     c                   Eval      Vtf02 = vtf(02)
356400030121     c                   Eval      Vtf03 = vtf(03)
356500030121     c                   Eval      Vtf04 = vtf(04)
356600030121     c                   Eval      Vtf05 = vtf(05)
356700030121
356800030121     c                   Eval      *In24 = *On
356900030121     c                   Eval      Vtf06 = Cf2433
357000030121     c                   Eval      wsb51z00 = 1
357100030121     c                   Goto      EndF24
357200030121
357300030121     c                   EndIf
357400030121
357500030121     c     EndF24        ENDSR
357600990813      *****************************************************************
357700991021      * DETERMINO SE SPEDIZIONE CON FATTURA IMMEDIATA
357800990813      *****************************************************************
357900991021     C     FATIMMED      BEGSR
358000990813      *
358100990813     C                   eval      KCOD = 'QC'
358200990813     C                   movel(P)  '2'           KKEY
358300990813     C                   EXSR      CHKTAB
358400990813      *
358500990813     c                   if        *in30 = *off
358600990813     C                   MOVEL     TBLUNI        DSQC2
358700990813      *
358800991021     c                   IF        TASfiv <> §QCfiv  and  TASfiv <> §QCfi2  and
358900990813     c                             TASfiv <> §QCfi3  and  TASfiv <> §QCfi4  and
359000990813     c                             TASfiv <> §QCfi5  and  TASfiv <> §QCfi6
359100991021     c                   eval        Wfatimmed = 'S'
359200991021     c                   ENDIF
359300990813      *
359400990813     c                   endif
359500990813      *
359600990813     C                   ENDSR
359700990621      *****************************************************************
359800990621      *  AGGANCIO ANAGRAFICHE
359900990621      *****************************************************************
360000990621     C     CTR_ANAGRA    BEGSR
360100990621      *
360200990629     C                   clear                   DS_cnaco
360300990629     C                   clear                   DS_cnind
360400990629     C                   clear                   DS_cnclp
360500990629     C                   clear                   DS_fncls
360600990629      *
360700990621     C                   CALL      'TIBS69R'
360800990621     C                   PARM                    tibs69DS
360900990621     C                   PARM                    DS_cnaco
361000990621     C                   PARM                    DS_cnind
361100990621     C                   PARM                    DS_cnclp
361200990621     C                   PARM                    DS_fncls
361300990621      *
361400990621     C                   eval      wtibs69r = 'S'
361500990621      * per il 1° giro è inizializzata nelle specifiche "D"
361600990621     C                   clear                   TIBS69DS
361700990621      *
361800990621     C                   ENDSR
361900990628      **********************************************************************
362000990629      * EDITO IMPORTI PER CAMPI A VIDEO lunghi 13 char
362100990628      **********************************************************************
362200990628     C     EDTIMPORTI    BEGSR
362300990628
362400990628      * -----------------------------------------------------------------------------------
362500990629      * W0133      contiene il numero da editare per il video
362600990629      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il numero ha decimali
362700990629      * IMPEDITATO ritorna il valore editato per il video lungo 13 (troncate cifre oltre 10 mld)
362800990628      * -----------------------------------------------------------------------------------
362900990628
363000990628     c                   clear                   IMPeditato
363100990629     c                   clear                   W017A
363200990628
363300990628     c                   if        Wtstdec = 'S'
363400990629     C                   eval      W017a = %EDITC(W0133:'2')
363500990628     c                   else
363600990629     c                   z-add     W0133         W0130
363700990629     C                   eval      W017a = %EDITC(W0130:'2')
363800990628     c                   endif
363900990629
364000990629     c                   move      W017a         IMPeditato
364100990628
364200990628     c                   ENDSR
364300990628      **********************************************************************
364400990628      * AGGANCIO FILE ORGANIGRAMMA
364500990628      **********************************************************************
364600990628     C     CHKORG        BEGSR
364700990628      *
364800990630     C                   clear                   ORGDES
364900990630     C     Wlinea        CHAIN     AZORG                              30
365000990628      *
365100990628     c                   ENDSR
365200990628      **********************************************************************
365300990628      * AGGANCIO FILE TABELLE
365400990628      **********************************************************************
365500990628     C     CHKTAB        BEGSR
365600990628      *
365700990628     C                   clear                   Wtbluni
365800990628      *
365900990630     C     KEYTAB        CHAIN     TABEL00F                           30
366000990630     c                   IF        *IN30 = *ON  or  TBLflg <> *blanks
366100990628     C                   eval         *in30 = *on
366200990628     C                   else
366300990628     C                   movel     TBLUNI        WTBLUNI
366400990628     C                   ENDIF
366500990628      *
366600990628     c                   ENDSR
366700030703      *-------------------------------------------------------------------------
366800030703      *  Decoditifa tipo bancali
366900030703      *-------------------------------------------------------------------------
367000030703     c     Sr_DecTba     BegSr
367100030703
367200030703     c                   Clear                   Tibs02Ds
367300030703     c                   Clear                   dTba
367400030703     c                   Movel     'C'           T02Mod
367500030703     c                   Movel     Knsif         T02Sif
367600030703     c                   Movel     'TBA'         T02Cod
367700030703     c                   Movel(p)  wtba          T02Ke1
367800030703     c                   Call      'TIBS02R'
367900030703     c                   Parm                    Kpjba
368000030703     c                   Parm                    Tibs02Ds
368100030703     c                   If        T02Err = *Blanks
368200030703     c                   Movel     T02Uni        dTba
368300030703     c                   EndIf
368400030703     c                   Eval      wdestba = §TbaDesc
368500030703
368600030703     c                   EndSr
368700030418      *****************************************************************
368800030418      * Pulisco tutti i record del file video
368900030418      *****************************************************************
369000030418     c     Sr_Clear      BegSr
369100030418
369200030418     c                   Clear                   wsb51d01
369300030418     c                   Clear                   wsb51d02
369400030418     c                   Clear                   wsb51d07
369500030418     c                   Clear                   wsb51d08
369600030418     c                   Clear                   wsb51d13
369700030418     c                   Clear                   wsb51d18
369800030418     c                   Clear                   wsb51d20
369900030418     c                   Clear                   wsb51d21
370000030418     c                   Clear                   wsb51d23
370100030418
370200030418     c                   Clear                   Sb51D01
370300030418     c                   Clear                   Sb51D07
370400030418     c                   Clear                   Sb51D12
370500030418
370600030418     c                   Clear                   Sb51C08
370700040603     c                   Clear                   Sb51C09
370800030418     c                   Clear                   Sb51C11
370900030418     c                   Clear                   Sb51C13
371000030418     c                   Clear                   Sb51C18
371100030418     c                   Clear                   Sb51C21
371200030418     c                   Clear                   Sb51C23
371300030418
371400030418     c                   Eval      *In26 = *Off
371500030418     c                   Write     Sb51C08
371600030418     c                   Eval      *In25 = *On
371700030418     c                   Eval      *In26 = *On
371800030418     c                   Clear                   nrr08
371900030418
372000030418     c                   Eval      *In26 = *Off
372100030418     c                   Write     Sb51C11
372200030418     c                   Eval      *In25 = *On
372300030418     c                   Eval      *In26 = *On
372400030418     c                   Clear                   nrr11
372500030418
372600030418     c                   Eval      *In26 = *Off
372700030418     c                   Write     Sb51C13
372800030418     c                   Eval      *In25 = *On
372900030418     c                   Eval      *In26 = *On
373000030418     c                   Clear                   nrr13
373100030418
373200030418     c                   Eval      *In26 = *Off
373300030418     c                   Write     Sb51C18
373400030418     c                   Eval      *In25 = *On
373500030418     c                   Eval      *In26 = *On
373600030418     c                   Clear                   nrr18
373700030418
373800030418     c                   Eval      *In26 = *Off
373900030418     c                   Write     Sb51C21
374000030418     c                   Eval      *In25 = *On
374100030418     c                   Eval      *In26 = *On
374200030418     c                   Clear                   nrr21
374300030418
374400030418     c                   Eval      *In26 = *Off
374500030418     c                   Write     Sb51C23
374600030418     c                   Eval      *In25 = *On
374700030418     c                   Eval      *In26 = *On
374800030418     c                   Clear                   nrr23
374900040512
375000040512     c                   Eval      wsb51z00 = 1
375100030418
375200030418     c                   EndSr
375300120711      *****************************************************************
375400120712      * Window Interrogazione Restituzione documenti al mitt.
375500120711      *****************************************************************
375600120711     c     Sr_w02        BegSr
375700121022
375800121022      * se non ho errore non visualizzo la riga
375900121022     c                   eval      *in69 = *off
376000121022     c                   IF        W02mtvdt <> *blanks
376100121022     c                   eval      *in69 = *on
376200121022     c                   ENDIF
376300120711     c*
376400120711     c                   if        %SUBST(I51OP0:1:1)<>'M'
376500120711     c                   exfmt     sb51w02
376600120711     c                   else
376700120712     c                   write     sb51w02
376800120711     c                   read(E)   tnsb51d
376900120711     c                   if        %error
377000120712     c                   leavesr
377100120711     c                   endif
377200120711     c                   endif
377300120711     c                   endsr
377400120829      *****************************************************************
377500120829      * Reperimento data scannerizzazione della figlia
377600120829      *****************************************************************
377700120829     c     Replbl        BegSr
377800120829     c                   eval      wtrovl=*off
377900120829     c                   eval      LBLAAN=I51AAS
378000120829     c                   eval      LBLLPN=I51LNP
378100120829     c                   eval      LBLNRN=I51NRS
378200120829     c                   eval      LBLNSN=I51NSP
378300120829     c     keylblN       chain     fnlbl02l
378400120830     c                   dow       %found(fnlbl02l)
378500120829     c
378600120829     c                   Eval      war5Trd = 'DOC'
378700120829     c     kspeAr5l      Chain     Fiar531c
378800120829If  1c                   If        %Found(Fiar531c)
378900120829     c                   Movel     Ar5Uni        Dar5doc
379000120829    4c                   if        §ar5jdrid >*zeros
379100120829     c                   eval      wtrovl=*on
379200120830     c                   leave
379300120829     c                   endif
379400120829     c                   endif
379500120829     c     keylblN       chain     fnlbl02l
379600120829     c                   enddo
379700120829     c                   endsr
379800990618      *****************************************************************
379900990618      * ROUTINE INIZIALE
380000990618      *****************************************************************
380100990618     C     *INZSR        BEGSR
380200990618      *
380300990618     C     *ENTRY        PLIST
380400990618     C                   PARM                    KPJBA
380500040416     c                   parm                    FIDNA1ds
380600990623      *
380700990628     c                   movel     KPJBU         TNSB51DS
380800030402
380900030402      * Mi salvo la bolla iniziale richiesta dall'utente
381000030402     c                   Eval      SavAas = I51Aas
381100030402     c                   Eval      SavLnp = I51Lnp
381200030402     c                   Eval      SavNrs = I51Nrs
381300030402     c                   Eval      SavNsp = I51Nsp
381400030402     c                   Eval      SavTbl = I51Tbl
381500020516
381600020516      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
381700020516     C     *dtaara       define    §azute        azuteds
381800020516     C     *dtaara       define    §datiute      ddatiute
381900020516
382000020516     C                   clear                   AzUteDs
382100020516     C                   clear                   DDatiUte
382200020516     C                   clear                   Tibs34Ds
382300020516     C                   in(E)     *dtaara
382400020516if  1C                   IF        %Error  or  RSUT = *blanks
382500020516     C                   call      'TIBS34R'
382600020516     C                   parm                    Tibs34Ds
382700020516     C                   in        *dtaara
382800020516e   1C                   ENDIF
382900990618
383000990618      * reperisco data e ora
383100990618     C                   TIME                    W0140
383200990618      * UDATE IN GGMMAAAA
383300990618     C                   MOVE      W0140         WDTGIO
383400990618      * UDATE IN AAAAMMGG
383500990805     C     *eur          MOVEL     WDTGIO        DATA_eur
383600990805     C     *iso          MOVEL     DATA_eur      dateu
383700110927     c* accendo in06 se utente edp (mi serve per l'abilitazione temporanea del tasto funz F6
383800110927     c* per l'interrogazione immagine a modo nuovo
383900111129     c****               if        %subst(knmus:1:3)= 'EDP'
384000111223     c****               seton                                        06
384100111129     c****               else
384200111129     c****               setoff                                       06
384300111129     c****               endif
384400110927     c
384500990618
384600990628     C     keyspe        KLIST
384700990628     C                   KFLD                    I51AAS
384800990628     C                   KFLD                    I51LNP
384900990628     C                   KFLD                    I51NRS
385000990628     C                   KFLD                    I51NSP
385100990618      *
385200990628     C     keyspe1       KLIST
385300990628     C                   KFLD                    I51AAS
385400990628     C                   KFLD                    I51LNP
385500990628     C                   KFLD                    I51NRS
385600990628     C                   KFLD                    I51NSP
385700990628     C                   KFLD                    I51TBL
385800030402
385900030402     c     keyspe2       Klist
386000030402     c                   Kfld                    kTasAas
386100030402     c                   Kfld                    kTasLnp
386200030402     c                   Kfld                    kTasNrs
386300030402     c                   Kfld                    kTasNsp
386400020225
386500020225     C     keyspeh       KLIST
386600020225     C                   KFLD                    I51AAS
386700020225     C                   KFLD                    I51LNP
386800020225     C                   KFLD                    I51NRS
386900020225     C                   KFLD                    I51NSP
387000020225     c                   kfld                    wtahtrc
387100030120
387200030120     c     kSpeAr5       Klist
387300030120     c                   Kfld                    I51AAS
387400030120     c                   Kfld                    I51LNP
387500030120     c                   Kfld                    I51NRS
387600030120     c                   Kfld                    I51NSP
387700030121     c                   kfld                    wAr5Trd
387800120829      *
387900120829     c     kSpeAr5l      Klist
388000120829     c                   Kfld                    LBLAAN
388100120829     c                   Kfld                    LBLLPN
388200120829     c                   Kfld                    LBLNRN
388300120829     c                   Kfld                    LBLNSN
388400120829     c                   kfld                    wAr5Trd
388500120829      *
388600120829     c     keylblN       Klist
388700120829     c                   Kfld                    LBLAAN
388800120829     c                   Kfld                    LBLLPN
388900120829     c                   Kfld                    LBLNRN
389000120829     c                   Kfld                    LBLNSN
389100030402
389200030402     c     keyspelbl     Klist
389300030402     c                   Kfld                    kLblAao
389400030402     c                   Kfld                    kLblLpo
389500030402     c                   Kfld                    kLblNro
389600030402     c                   Kfld                    kLblNso
389700990622      *
389800990630     C     keytab        KLIST
389900020516     C                   KFLD                    KKUT
390000990630     C                   KFLD                    KCOD
390100990630     C                   KFLD                    KKEY
390200990622      *
390300990629     C     ktastrc       KLIST
390400990629     C                   KFLD                    tasAAS
390500990629     C                   KFLD                    tasLNP
390600990629     C                   KFLD                    tasNRS
390700990629     C                   KFLD                    tasNSP
390800990629     C                   KFLD                    Ktrc
390900070524      *
391000070524     C     ktitai        KLIST
391100070524     C                   KFLD                    tasAAS
391200070524     C                   KFLD                    tasLNP
391300070524     C                   KFLD                    tasNRS
391400070524     C                   KFLD                    tasNSP
391500070524     C                   KFLD                    tasTBL
391600070524     C                   KFLD                    Ksvt                           Sigla varia tassata
391700020419
391800020419     c     kfnorm        klist
391900020419     c                   kfld                    kormpoe
392000020419     c                   kfld                    kormnsr
392100020419     c                   kfld                    kormnor
392200020419     c                   kfld                    kormnrv
392300021203
392400021203     C     Kfapd         klist
392500021203     C                   kfld                    kpdtip
392600021203     C                   kfld                    kpdpdr
392700040416      *
392800160404      * File FITGD32L
392900040416     c     K02TGD02      klist
393000040416     c                   kfld                    TGDtor
393100040416     c                   kfld                    TGDogg
393200040809
393300040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
393400040809     C     KDCT1_C       KLIST
393500040809     C                   KFLD                    dsaac
393600040809     C                   KFLD                    dsfil
393700040809     C                   KFLD                    dsnca
393800120330     C     Ktbe          KLIST
393900120330     C                   KFLD                    kcode
394000120330     C                   KFLD                    kke1
394100120330     C                   KFLD                    kke2
394200990623      *
394300990618     C                   ENDSR
394400990618      *---------------------------------------------------------------------------------------------
394500990618** MSG  Lungh. 78                                                            *
394600990623TNSB50R- Opzione non valida                                                     1
394700990623TNSB50R- Codice cliente errato                                                  2
394800990623TNSB50R- Non richiedere il cod. mittente se non si è immesso il rif. mittente   3
394900990623TNSB50R- Impostate selezioni incongruenti                                       4
395000990623TNSB50R- Data fattura errata                                                    5
395100990623TNSB50R- Immettere la linea                                                     6
395200990623TNSB50R- Linea inesistente o annullata                                          7
395300990623TNSB50R- Non richiedere la data fattura e le spedizioni non fatturate           8
395400990623TNSB50R- Immettere l'anno spedizione                                            9
395500990623TNSB50R- Non richiedere il numero fattura se non si è immessa la data fattura   10
395600990623TNSB50R- Non richiedere data fattura se non si è immesso il cod. fatturazione   11
395700990623TNSB50R- Non richiedere sped. non fatturate se non è immesso il cod. fatturaz.  12
395800990623TNSB50R- Data spedizione errata                                                 13
395900990623TNSB50R- Immettere data spedizione DAL minore o uguale a data spedizione AL     14
396000990623TNSB50R- Immettere almeno una selezione valida                                  15
396100990623TNSB50R- Non richiedere la serie se non immessi anche P.O. e numero segnacollo  16
396200990623TNSB50R- Non richiedere il P.O. segnacollo se non immesso il numero segnacollo  17
396300990623TNSB50R- Non richiedere il numero segnacollo se non immesso il P.O. segnacollo  18
396400990623TNSB50R- Bolla esistente ma annullata                                           19
396500990623TNSB50R- Opzione non valida                                                     20
396600990623TNSB50R- Scorrimento indietro o in avanti oltre il primo o l'ultimo record      21
396700990624TNSB50R- Non trovata la spedizione richiesta                                    22
396800120709TNSB50R- Bolla non trovata: e' stato modificato il tipo bolla                   23
396900020225** TF   Lungh. 11                                                            *
397000041018F15=Ric.Ass                                                                     1
397100041008F5=Eventi                                                                       2
397200041008F22=C/Ass.                                                                      3
397300041008F2=C.A.                                                                         4
397400041008F14=Giac.                                                                       5
397500041008F10=Variaz.                                                                     6
397600041008F18=Legami                                                                      7
397700041008F11=NoteS/B                                                                     8
397800041008F4=ORM                                                                          9
397900041008F8=CHI SONO                                                                     10
398000041008F21=D.Colli                                                                     11
398100041008F23=AWB Fed                                                                     12
398200041008F1=Agg.Rich                                                                     13
