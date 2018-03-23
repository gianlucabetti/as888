000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200120821     h dftactgrp(*no) actgrp(*caller)
000300120821ab   H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000400020125     H* FICNB3R *----------------------------------------------------*
000500021219     H*         - CONTEGGI PADRONCINI: CONFERMA
000600950127     H*--------------------------------------------------------------*
000700020125     FFICNB3D   CF   E             WORKSTN
000800950127     F                                     SFILE(FRC0SF2:NRR)
000900020408     F                                     infds(info2)
001000011113     F                                     SFILE(FRC0WS6:NRW6)
001100090512     Faitra03L  IF   E           K DISK
001200090512     Faitrs01L  IF   E           K DISK
001300000711     FAZORG01L  IF   E           K DISK
001400000711     FTABEL00F  IF   E           K DISK
001500020329     FTntbe01L  IF   E           K DISK
001600090507     Ffitgt01L  IF   E           K DISK
001700021202     FFIAPD01L  IF   E           K DISK
001800120517     FFiDST01L  IF   E           K DISK
001900020918     FFNFVV02L  IF   E           K DISK
002000950127     FFNBLP34L  IF   E           K DISK
002100020419     FFNBLP01L  IF   E           K DISK
002200020419     F                                     RENAME(FNBLP000:FNBLP001)
002300020918     FFNARB78L  IF   E           K DISK
002400950221     FFNARB01L  IF   E           K DISK
002500950221     F                                     RENAME(FNARB000:FNARB1)
002600030123     Ffifre02L  IF   E           K DISK
002700020611      *                                                                         <-------
002800020611     Ffifcew0S  o  a e           k disk                                         <---- Spia
002900020611      *                                                                         <-------
003000020408     Ffifce01L  UF A E           K DISK    commit
003100050208     FfifceW3L  UF A E           K DISK    usropn
003200040406     FfifceW1L  UF A E           K DISK    usropn
003300020329     F                                     RENAME(FIFCW000:FIFCW001)
003400011113     FFIFTT01L  IF   E           K DISK
003500060119     FFIFTT22L  UF A E           K DISK    commit
003600011113     F                                     RENAME(FIFTT000:FIFTT002)
003700011113     FFIFTD01L  IF   E           K DISK
003800020329     Ffiftd06l  IF   E           K DISK    RENAME(FIFTD000:FIFTD006)
003900970923     F* File di appoggio per trasmissione stop in partenza
004000040311     FFNSTP10F  O  A E             DISK
004100020408     FFNWLRC0F  UF A E           K DISK
004200020408     FFNWLRC1L  UF   E           K DISK
004300951011     F                                     RENAME(FNWLRC00:FNWC01)
004400030428     F*---------------------------------------------------------------------------------
004500030428      * Per Controllare che non ci siano le trasmissioni in sede in corso
004600030428      *  durante l'apertura di questo programma si scrive un record su questo file.
004700030430     Ffiwalc1l  UF A E           K DISK    commit
004800030428     F*---------------------------------------------------------------------------------
004900120821ab   ***************************************************************************
005000120821ab   ** Prototipi.
005100120821ab   ***************************************************************************
005200120821ab    **
005300120821ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
005400120821ab    /DEFINE DFTACTGRP_NO
005500120821ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
005600120821ab    /UNDEFINE DFTACTGRP_NO
005700120821ab   **
005800120821ab   ***************************************************************************
005900120821ab   **       Campi
006000120821ab   ***************************************************************************
006100120821ab   D TibsSof_esito...
006200120821ab   D                 S             10I 0 IMPORT
006300120821ab   D idSocieta...
006400120821EDPDCD                 S              3A
006500120821ab   **
006600120821EDPDCd Filiale         s              3S 0
006700120821EDPDCd dtVALIDITA      s               D
006800120821EDPDCd dtVALIDITAiniz  s               D
006900120821EDPDCd dataISO         s               D
007000120821EDPDCd VALdatFINE      s               D
007100120821EDPDCd VALdatINIZ      s               D
007200120821EDPDCd Data_FINE       s               D
007300120821EDPDCd Data_INIZIO     s               D
007400120821ab   ***************************************************************************
007500100527      *
007600100527     D ddatiute      e ds                  prefix(UT_)
007700100527     D azuteds       e ds                  extname(AZUTE00F)
007800100527     D tibs34ds      E DS                  inz
007900130321     D Dtgtflr       E DS
008000020408      *
008100020408      * record del sfl per gestire correttamente il posizionamento del sfl
008200020408     D info2           DS
008300020408     D  nrult2               378    379B 0
008400020408      *
008500970415     D RSC             S             35    DIM(50)                              RSC CON STP =
008600970415     D KSC             S              7  0 DIM(50)                              KSC CON STP =
008700970415     D*
008800950127     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
008900950127     D DTS             S             15    DIM(30)                              DESCR.TIPI PREST
009000950127     D CTS             S              3    DIM(30)                              CDTAR TIPI PREST
009100950127     D T2R             S              1    DIM(30)                              TIPI PRESTAZ2
009200950127     D DT2             S             15    DIM(30)                              DESCR.TIPI PREST
009300950127     D CT2             S              3    DIM(30)                              CDTAR TIPI PREST
009400950127     D K18             S              1    DIM(18)                              SKI 18 ALFABETICA
009500950127     D K08             S              1    DIM(8)                               SKI 8  ALFABETICA
009600040306     D CMC             S              6    DIM(9999)                            MANC.CONS DA TASS
009700040306     D CBL             S              6    DIM(9999)                            CD.BOL DA NO TASS
009800970923     D*
009900120821     D ERR             S             55    DIM(15) CTDATA PERRCD(1)             MSG ERRORE
010000950531     D MSG             S             70    DIM(2) CTDATA PERRCD(1)              MSG PER SEDE
010100020328     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              COMANDI da eseguire
010200020320      *
010300020320     d si_B3R1         S              1
010400020403     d udatiso         s               d   Datfmt(*iso)
010500020320     d si_CN40         S              1
010600020918     d wfgs            S                   like(arbifp)
010700130321     d §t3pdrsav       S                   like(§t3pdr)
010800020328      *---------------------------------------------------------------------*
010900020328     D  MSGDS          S             80
011000020328     D  LENGH          S             15  5
011100020328      *---------------------------------------------------------------------*
011200121017     D ficng5ds      E DS
011300131008     D tibs42ds      E DS
011400131008     D uteautds      E DS
011500091106     D trmz70s1ds    E DS                  prefix(S1_)
011600130404     D trmz70s4ds    E DS                  prefix(S4_)
011700130404     D parm_s4CDF      s                   like(tgtCDF)
011800130404     D parm_s4SOC      s                   like(tgtSOC)
011900130404     D parm_s4FIL      s                   like(tgtFIL)
012000130404     D TAR_PIVA        s                   like(trsIVA)
012100130404     D TAR_Forn        s                   like(tgtCDF)
012200130404     D TAR_Socie       s                   like(tgtSOC)
012300130404     D TRS_PIVA        s                   like(trsIVA)
012400130404     D TRS_Forn        s                   like(tgtCDF)
012500130404     D TRS_Socie       s                   like(tgtSOC)
012600020516     D Dcre          E DS
012700020329     D DS8A          E DS
012800090507     D ds5Aaut       E DS
012900130318     D ficn5Ads      E DS
013000081008     D ficn74ds      E DS
013100020320      *
013200950127     D KPJBA         E DS
013300020701     D* REM                    1      3
013400020701     D* REMFIL                 4      6
013500950127     D PARAM           DS
013600950127     D  PARSML               256    256
013700020125     D* PASSAGGIO DATI  A CONTEGGI PADRONCINI            - FICNB1R -
013800950127     D PARAM1          DS
013900950127     D  PA1PDR                 1      7  0
014000950127     D  PA1TSR                 8      8
014100950127     D  PA1DDC                 9     16  0
014200950127     D  PA1SML               256    256
014300011113     D* PASSAGGIO DATI  A SVALORIZZAZ. FILE SIMULAZIONE  - FICND9R -
014400950127     D PARAM2          DS
014500950127     D  PA2PDR                 1      7  0
014600950127     D  PA2TSR                11     11
014700950127     D  PA2DDC                12     19  0
014800950127     D WLBDAT          DS
014900950127     D  G02DAT                 1      8  0
015000950127     D  G02INV                 9     16  0
015100950127     D  G02ERR                17     17
015200950127     D  G02TGI                18     22  0
015300950127     D                 DS
015400950127     D  AA                     1      2
015500950127     D  MM                     3      4
015600950127     D  GG                     5      6
015700950127     D  DATA                   1      6  0
015800950127     D                 DS
015900950127     D  VI1PF1                 1      3  0
016000950127     D  VI1PD1                 4      7  0
016100950127     D  VI1P1                  1      7  0
016200950127     D                 DS
016300950127     D  VI1PF2                 1      3  0
016400950127     D  VI1PD2                 4      7  0
016500950127     D  VI1P2                  1      7  0
016600020423     D                 DS
016700020423     D  ftdTSR                 1      1
016800020423     D  ARBCBO                 2      3
016900020423     D  W018C                  1      3
017000020423      *
017100020318     D ficn40ds      E DS
017200020423     D DS2Z          E DS
017300000711     D OG143         E DS
017400000711     D DS1Z          E DS
017500950221     D DS3A          E DS
017600020201     D FICNB2        E DS                  EXTNAME(FICNB2DS)
017700020320     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
017800020315     D FIcnB6        E DS                  EXTNAME(FICNB6DS)
017900950127     D CNCR80        E DS
018000950127     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018100950531     D                 DS
018200950531     D  MSGDET                 1    198
018300950531     D  MSPDR                 43     49
018400950531     D  MSDDC                 61     68
018500950801     D                 DS
018600950801     D  W01KSC                 1      7  0
018700950801     D  W01COD                 4      7  0
018800120821      *
018900120821      **------------------------------------------------------------**
019000120821EDPDCd SOC_errata      s              1A
019100120821     D NO              C                   CONST('N')
019200120821     D SI              C                   CONST('S')
019300120821      **------------------------------------------------------------**
019400021219      *
019500950127     D***
019600950127     D* DEFINIZIONE CONSTANTI
019700950127     D***
019800011113     D CMINIM          C                   CONST('M i n i m o:')
019900011113     D CTOTAL          C                   CONST('Totale:    ')
020000020520     D CIMPOR          C                   CONST('Var Corrisp')
020100950127     I/SPACE 3
020200950127     C****************************************************************
020300950127     C*  RIEPILOGO INDICATORI
020400950127     C***************************************************************
020500950127     C* 01    - IMMESSA SOLO DATA DAL
020600950127     C* 02    - IMMESSA DATA AL
020700950127     C* 03    - IMMESSA FILIALE DI APPARTENENZA
020800950127     C* 06    - FASE DI SIMULAZIONE
020900951025     C* 09    - ERRORE IN CONFERMA CONTEGGI:RECORD DI TOTALE ESISTE GIà
021000950127     C* 12    - EMISSIONE RECORD A GIORNATA IN SFL
021100950127     C* 15    - EMISSIONE A VIDEO BASE ACCESSORI PIK E BONUS
021200950127     C* 16    - RECORD DEL MINIMO
021300950127     C* 17    - EMISSIONE CAMPO TARIFFA GIORNATA
021400950127     C* 19    - EMETTO TOTALE BONUS
021500950127     C* 20    - RECROD DI TOTALE
021600950127     C* 30/32 - DI COMODO
021700950127     C* 35    - PULIZIA ES EMISSIONE SFL
021800001130     C* 37    - DI COMODO
021900020402     C* 38/45 - ERRORI
022000011207     C* 47    - ERRORE DIVISA
022100950406     C* 50    - ERRORE SFL:PRESENTE IMPORTO TOTALE = 0
022200020318     C* 51    - ERRORE SFL:manca tariffa o tariffa non confermata con data stampa
022300951009     C* 52/57 - ERRORI
022400950127     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
022500950127     C* 91    - RECORD DI FTT DA NON CARICARE
022600950127     C*****************************************************************
022700950127     C     INIZIO        TAG
022800950127     C* PULIZIA FORMATO 1
022900950127     C                   EXSR      PUL01
023000950127     C*
023100950127     C     FOR01         TAG
023200950127     C                   EXFMT     FRC0D01
023300950127     C                   SETOFF                                       444546
023400950615     C                   SETOFF                                       565554
023500951009     C                   SETOFF                                       535257
023600950127     C** CMD3 - FINE LAVORO
023700950127     C   KC              GOTO      FINE
023800950127     C* CONTROLLI FORMATO1
023900950127     C                   EXSR      CONTR1
024000950127     C   90              GOTO      FOR01
024100121017     C*  esegue controlli sulle FIRME
024200121017     C                   EXSR      Ctrl_FIRME
024300121017     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
024400121017     c                   if          devo_bloccare = 'S'
024500121017     C                   GOTO      FOR01
024600121017     c                   end
024700950127     C*
024800950127     C* CARICO DISTINTE SELEZIONATE
024900950127     C                   EXSR      CARNDC
025000020523     C   90              EXSR      DEL_FIFCW
025100951009     C   90              EXSR      SRWC01
025200950127     C   90              GOTO      FOR01
025300950127     C*
025400950127     C                   WRITE     FRC0D03
025500950127     C*
025600950127     C     FORCT2        TAG
025700950127     C                   EXFMT     FRC0CT2
025800020408     c                   z-add     nrult2        wrc2
025900970924     C                   SETOFF                                       57
026000950127     C** CMD03 - FINE
026100020327     C   KC              EXSR      DEL_FIFCW
026200020327     C   KC              EXSR      SRWC01
026300950127     C   KC              GOTO      FINE
026400950127     C*
026500950127     C** CMD12 - RITORNO
026600020327     C   KL              EXSR      DEL_FIFCW
026700950912     C   KL              EXSR      SRWC01
026800950127     C   KL              GOTO      FOR01
026900020318     C*
027000950127     C* SE PRESENTI MANCA TARIFFA NON POSSO FARE NULLA: USCIRE E CORREG
027100950127     C     WMTAR         IFEQ      '1'
027200950127     C                   SETON                                        40
027300950127     C                   GOTO      FORCT2
027400950127     C                   ENDIF
027500040506     C*
027600950127     C                   EXSR      CONTR2
027700011207     C   47
027800011207     Cor 50              GOTO      FORCT2
027900950127     C*
028000950127     C* CMD6 - CONFERMA VALORIZZAZIONE
028100950127     C     *INKF         IFEQ      *ON
028200950127     C* SOLO IN SIMULAZIONE CHIEDO SE SI VUOLE RIEFFETTUARE LA SIMULAZ.
028300950127     C     PARSML        IFNE      ' '
028400950127     C                   MOVEL     'NO'          VIDSIM
028500950127     C                   EXFMT     FRC0D04
028600950127     C                   ENDIF
028700950127     C*
028800020329     C*  Conferma i Conteggi e crea il Workfile per il Conto Economico
028900020329     C*   se la Conferma ha ulteriori valori da ripartire.
029000950127     C                   EXSR      CONVAL
029100020329     C* Dopo aver convalidato e creato il workfile con i valori di C/Econ
029200020329     C*  le scrive aggiornando le VOCI sul FIFCE01L
029300020408     C     PARSML        IFeq      ' '
029400020329     C                   EXSR      AGG_VOCI
029500070523     c                   commit
029600020408     c                   end
029700020329     C*  quindi pulisce il Workfile utilizzato
029800020718     c                   move      'S'           F6Fine            1
029900020329     C                   EXSR      DEL_FIFCW
030000020718     c                   move      ' '           F6Fine
030100020329     C*  e libera il programma
030200950912     C                   EXSR      SRWC01
030300950127     C                   GOTO      INIZIO
030400950127     C                   ENDIF
030500020408      *
030600020408     c                   commit
030700950127     C* ENTER
030800950127     C                   GOTO      FORCT2
030900950127     C*
031000950127     C     FINE          TAG
031100030430     C*
031200030430     C*  deve cancellare il record di allocazione x impedire le trasmissioni
031300030430     C*   in sede durante l'utilizzo di questo programma.
031400030513     c     parsml        ifeq      *blank
031500030430     C     Kwalc         setll     fiwalc1l
031600030430     C     Kwalc         reade     fiwalc1l
031700030430     c                   dow       not %eof(fiwalc1l)
031800030430     c                   delete    fiwalc00
031900030430     C     Kwalc         reade     fiwalc1l
032000030430     c                   end
032100030513     c                   end
032200020408      *
032300020408     c                   commit
032400020320     C*
032500020320     c                   if        si_B3R1='S'
032600020320     C                   clear                   ficnb3
032700020320     C                   movel     'C'           §t3tla
032800020320     C                   CALL      'FICNB3R1'
032900020320     C                   PARM                    ficnb3
033000020320     c                   end
033100020320     C*
033200020320     c                   if        si_CN40='S'
033300020320     C                   clear                   ficn40ds
033400030124     C                   MOVEL     'A'           §40tip
033500020320     C                   MOVEL     'C'           §40tla
033600020320     C                   MOVEL     kpjbu         savpjbu
033700020320     C                   clear                   kpjbu
033800020320     C                   MOVEL     ficn40ds      kpjbu
033900020320     C                   call      'FICN40R'
034000020320     C                   parm                    kpjba
034100020320     C                   clear                   kpjbu
034200020320     C                   MOVEL     savpjbu       kpjbu
034300020320     c                   end
034400020328     C*
034500020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
034600020328     C*   economico.
034700020328     C                   clear                   FICNB2
034800020328     C                   move      'C'           §taflg
034900020328     C                   CALL      'FICNB2R'
035000020328     C                   PARM                    FICNB2
035100020328     C*
035200020523     c     parsml        ifeq      *blank
035300050208     c                   close     fifcew3l
035400020329     c                   close     fifcew1l
035500020523     c                   end
035600020328     C*
035700020523     c                   goto      no_rgz
035800020328     C                   clear                   MSGDS
035900020328     C                   MOVEL     CMD(1)        MSGDS
036000020328     C                   Z-ADD     80            LENGH
036100020328     C                   CALL      'QCMDEXC'                            99
036200020328     C                   PARM                    MSGDS
036300020328     C                   PARM                    LENGH
036400020523     c     no_rgz        tag
036500030428     C*
036600950127     C                   SETON                                        LR
036700950127     C****************************************************************
036800950127     C     *INZSR        BEGSR
036900950127     C     *ENTRY        PLIST
037000950127     C                   PARM                    KPJBA
037100950127     C                   MOVEL     KPJBU         PARAM
037200100527      *
037300100527     c     *dtaara       define    §azute        azuteds
037400100527     c     *dtaara       define    §datiute      ddatiute
037500100527     C                   in(E)     *dtaara
037600100527     C                   IF        %Error  or  RSUT = *blanks
037700100527     C                   call      'TIBS34R'
037800100527     C                   parm                    Tibs34Ds
037900100527     C                   in        *dtaara
038000100527     c                   ENDIF
038100100527      *
038200950127     C                   Z-ADD     1             CODUT
038300950127     C                   CALL      'X§PARUT'
038400950127     C                   PARM                    UT§DSE
038500950127     C                   MOVEL     RAGUT         RSUT             20
038600950127     C                   MOVEL     REC80         CNCR80
038700020320     C                   eval      si_B3R1 = 'N'
038800020320     c                   eval      si_CN40 = 'N'
038900020328     C*
039000030428     C* solo se non simulato
039100020523     c     parsml        ifeq      *blank
039200050208     c                   open      fifcew3l
039300020329     c                   open      fifcew1l
039400020523     c                   end
039500120821ab   C*     Inizializza
039600120821ab    /FREE
039700120821ab     TibsSof_Init();
039800120821ab    /END-FREE
039900950127     C*---------------------------------------------------------------*
040000030428     C     Kwalc         KLIST
040100030428     C                   KFLD                    WalPGM
040200030428     C                   KFLD                    WalNOJ
040300030428     C                   KFLD                    WalNUS
040400030428     C                   KFLD                    WalNRJ
040500950127     C* ACCESSO   TABEL
040600950127     C     KTAB2         KLIST
040700950127     C                   KFLD                    CODUT
040800950127     C                   KFLD                    COD               2
040900950127     C     KTAB          KLIST
041000950127     C                   KFLD                    CODUT
041100950127     C                   KFLD                    COD               2
041200950127     C                   KFLD                    KEY               8
041300000307     C     KFTT          KLIST
041400000307     C                   KFLD                    VH2PDR
041500000307     C                   KFLD                    KTSR
041600000307     C                   KFLD                    KNDC
041700000307     C                   KFLD                    FTTDDC
041800950127     C     KFTT2         KLIST
041900950127     C                   KFLD                    W01PDR
042000950127     C                   KFLD                    W01FVL
042100950127     C     KFTT3         KLIST
042200950127     C                   KFLD                    VH2PDR
042300950127     C                   KFLD                    W02FVL
042400950127     C                   KFLD                    W02DDC
042500950127     C                   KFLD                    TSR(C)
042600950127     C     KFTD          KLIST
042700950127     C                   KFLD                    VH2PDR
042800950127     C                   KFLD                    W01TSR
042900950127     C                   KFLD                    W01NDC
043000950127     C     KFTDS         KLIST
043100950127     C                   KFLD                    FTTPDR
043200950127     C                   KFLD                    FTTTSR
043300950127     C                   KFLD                    FTTNDC
043400950127     C                   KFLD                    FTTDDC
043500950127     C                   KFLD                    W01STP
043600950127     C     KFTDR         KLIST
043700950127     C                   KFLD                    FTTPDR
043800950127     C                   KFLD                    FTTTSR
043900950127     C                   KFLD                    FTTNDC
044000950127     C                   KFLD                    FTTDDC
044100020423      *
044200020423     C     kftd3         KLIST
044300020423     C                   KFLD                    ftdAAS
044400020423     C                   KFLD                    ftdLNP
044500020423     C                   KFLD                    ftdNRS
044600020423     C                   KFLD                    ftdNSP
044700950127     C     KFVV          KLIST
044800950127     C                   KFLD                    W01NPG
044900950127     C                   KFLD                    FTTDDC
045000020918     C                   KFLD                    vi1pf1
045100120517     C     KiDST         KLIST
045200120517     C                   KFLD                    dstNPG
045300120517     C                   KFLD                    dstNFV
045400120517     C                   KFLD                    dstFGS
045500001130     C     KBLP          KLIST
045600001130     C                   KFLD                    W01CPR
045700001130     C                   KFLD                    FTTDDC
045800950127     C     KBLP1         KLIST
045900950127     C                   KFLD                    FTDPDR
046000950127     C                   KFLD                    FTDDDC
046100950127     C                   KFLD                    FTDRSC
046200950127     C                   KFLD                    W01KSC
046300950127     C                   KFLD                    W01CCM
046400970924     C                   KFLD                    FTTFPP
046500950127     C                   KFLD                    W01NRT
046600950221     C     KARB          KLIST
046700950221     C                   KFLD                    FTDAAS
046800950221     C                   KFLD                    FTDLNP
046900950221     C                   KFLD                    FTDNRS
047000950221     C                   KFLD                    FTDNSP
047100020918     C     KARB78        KLIST
047200020918     C                   KFLD                    wfgs
047300020918     C                   KFLD                    wnum7
047400951011     C     KWC0          KLIST
047500951011     C                   KFLD                    PARSML
047600950912     C                   KFLD                    FTTPDR
047700950912     C                   KFLD                    FTTDDC
047800951011     C     KWC1          KLIST
047900951011     C                   KFLD                    KNMEB                          NOME JOB
048000951011     C                   KFLD                    KNMUS                          NOME USER
048100951011     C                   KFLD                    KNRJO                          NR.JOB
048200020423     C*
048300020423     C     Kfcw2T        KLIST
048400020423     c                   KFLD                    kfgsw
048500020423     c                   KFLD                    kpdrw
048600020423     c                   KFLD                    kddcw
048700020329     C*
048800020329     C     Kftd6         KLIST
048900020329     c                   KFLD                    fcwpdr
049000020329     c                   KFLD                    fcwddc
049100020329     c                   KFLD                    fcwtsr
049200020329     C*
049300020329     C     Kftd6_1       KLIST
049400020329     c                   KFLD                    fcwpdr
049500020329     c                   KFLD                    fcwddc
049600040426     C*
049700040426     C     Kftd6_zero    KLIST
049800040426     c                   KFLD                    vh2pdr
049900040426     c                   KFLD                    vh2ddc
050000020329     C*
050100020329     C     Kfre          KLIST
050200030123     c                   KFLD                    ktip
050300020329     c                   KFLD                    frepdr
050400020329     c                   KFLD                    freddc
050500030123     c                   move      'A'           ktip              1
050600020329      *
050700020329     C     Kfce          KLIST
050800020329     C                   KFLD                    fceFGS
050900020329     C                   KFLD                    fcePDR
051000020329     C                   KFLD                    fceTSR
051100020329     C                   KFLD                    fceNDC
051200020329     C                   KFLD                    fceDDC
051300020329     C                   KFLD                    fceAAS
051400020329     C                   KFLD                    fceLNP
051500020329     C                   KFLD                    fceNRS
051600020329     C                   KFLD                    fceNSP
051700020329     C                   KFLD                    fceCCE
051800020329      *
051900020329     C     KTbe          KLIST
052000020329     C                   KFLD                    tbeCOD
052100020329     C                   KFLD                    tbeKE1
052200020329     C                   KFLD                    tbeKE2
052300020329      *
052400020329     C     Kfcw1         KLIST
052500020329     C                   KFLD                    fcwFGS
052600020329     C                   KFLD                    fcwPDR
052700020329     C                   KFLD                    fcwTSR
052800020329     C                   KFLD                    fcwNDC
052900020329     C                   KFLD                    fcwDDC
053000020329     C                   KFLD                    fcwAAS
053100020329     C                   KFLD                    fcwLNP
053200020329     C                   KFLD                    fcwNRS
053300020329     C                   KFLD                    fcwNSP
053400020329     C                   KFLD                    fcwCCE
053500021202     c     Kapdv         Klist
053600021202     C                   kfld                    apdtip
053700021202     C                   kfld                    vi1p1
053800021202     c     Kapdf         Klist
053900021202     C                   kfld                    apdtip
054000021202     C                   kfld                    fttpdr
054100021202     c                   eval      apdtip = 'A'
054200020327     C*
054300090507     c     Ktgt          Klist
054400090507     C                   kfld                    fttpdr
054500090507     C                   kfld                    parsml
054600090512      *
054700090512     c     KTRA03        Klist
054800120911     C                   kfld                    fttPDR
054900090512     C                   kfld                    DATA_a0           8 0
055000090512     C                   z-add     0             DATA_a0
055100950127     C* CARICO TABELLE
055200950127     C***
055300950127     C* 06 ON - FASE DI SIMULAZIONE
055400950127     C***
055500950127     C     PARSML        IFEQ      'S'
055600950127     C                   SETON                                        06
055700950127     C                   ENDIF
055800950127     C* VEDO SE SONO SIMFEL O REMOTO
055900950127     C***
056000020701     C*                  MOVEL     SIMFEL        CDU               3 0
056100950127     C*
056200020701     C*    REM           IFEQ      'REM'
056300020701     C*    REMFIL        ANDGT     *ZEROS
056400020701     C*                  MOVEL     REMFIL        CDU
056500020701     C*                  END
056600020402     C*
056700950127     C* CARICO TABELLA DEI TIPI PRESTAZIONE
056800950127     C***
056900950127     C                   Z-ADD     1             C
057000950127     C                   Z-ADD     1             B
057100950127     C                   MOVEL     '1Z'          COD
057200950127     C     KTAB2         SETLL     TABEL
057300950127     C     KTAB2         READE     TABEL                                  30
057400950127     C*
057500950127     C     *IN30         DOWEQ     *OFF
057600950127     C     TBLFLG        IFEQ      ' '
057700950127     C                   MOVEL     TBLUNI        DS1Z
057800950127     C     §1ZUPD        IFEQ      'S'
057900950127     C                   MOVEL     TBLKEY        TSR(C)
058000950127     C                   MOVEL     §1ZDES        DTS(C)
058100950127     C                   MOVEL     §1ZCTR        CTS(C)
058200950127     C                   ADD       1             C
058300950127     C                   ENDIF
058400950127     C                   MOVEL     TBLKEY        T2R(B)
058500950127     C                   MOVEL     §1ZDES        DT2(B)
058600950127     C                   MOVEL     §1ZCTR        CT2(B)
058700950127     C                   ADD       1             B
058800950127     C                   ENDIF
058900950127     C     KTAB2         READE     TABEL                                  30
059000950127     C                   ENDDO
059100020423     C***---------------------------
059200020423     C* CARICO MANCATE CONSEGNE DA TASSARE
059300020423     C***---------------------------
059400020423     C                   Z-ADD     1             CC
059500020423     C                   MOVEL     '8B'          COD
059600020423     C     KTAB2         SETLL     TABEL
059700020423     C     KTAB2         READE     TABEL                                  30
059800020423     C*
059900020423     C     *IN30         DOWEQ     *OFF
060000020423     C     TBLFLG        IFEQ      ' '
060100020423     C                   MOVEL     TBLKEY        CMC(CC)
060200020423     C                   MOVE      TBLKEY        WALF3             3
060300020423     C                   MOVE      WALF3         CMC(CC)
060400020423     C                   ADD       1             CC
060500020423     C                   ENDIF
060600020423     C     KTAB2         READE     TABEL                                  30
060700020423     C                   ENDDO
060800020403      *
060900020423     C***---------------------------
061000020423     C* CARICO I TIPI BOLLA DA NON TASSARE
061100020423     C***
061200040306     C                   Z-ADD     1             CC                5 0
061300020423     C                   MOVEL     '8C'          COD
061400020423     C     KTAB2         SETLL     TABEL
061500020423     C     KTAB2         READE     TABEL                                  30
061600020423     C*
061700020423     C     *IN30         DOWEQ     *OFF
061800020423     C     TBLFLG        IFEQ      ' '
061900020423     C                   MOVEL     TBLKEY        CBL(CC)
062000020423     C                   MOVE      TBLKEY        WALF3             3
062100020423     C                   MOVE      WALF3         CBL(CC)
062200020423     C                   ADD       1             CC
062300020423     C                   ENDIF
062400020423     C     KTAB2         READE     TABEL                                  30
062500020423     C                   ENDDO
062600020423     C*
062700020423     C***---------------------------
062800950127     C* GIRO DATA ODIERNA
062900950127     C***
063000950127     C                   TIME                    W0140            14 0
063100950127     C                   MOVE      W0140         UDATE8            8 0
063200950127     C                   Z-ADD     UDATE8        G02DAT
063300950127     C                   MOVE      *ZEROS        G02INV
063400950127     C                   MOVE      *BLANKS       G02ERR
063500950127     C                   CALL      'XSRDA8'
063600950127     C                   PARM                    WLBDAT
063700950127     C                   Z-ADD     G02INV        DATEU8            8 0
063800020403     C                   MOVE      g02inv        UDATiso
063900030428     C*
064000030428     C* scrive il record di allocazione x trasmissione
064100030428     c     parsml        ifeq      *blank
064200030428     C                   clear                   Fiwalc00
064300030428     C                   movel     w0140         walOrc
064400030428     C                   movel     g02inv        waldac
064500030428     C                   eval      WalSIF = KNSIF
064600030428     C                   eval      WalPGM = 'FICNB3R   '
064700030428     C                   eval      WalNOJ = KNMEB
064800030428     C                   eval      WalNUS = KNMUS
064900030428     C                   eval      WalNRJ = KNRJO
065000100527     C                   eval      WalFLD = 'Filiale   ' + %editc(UTEFIL:'X') +
065100100527     c                                      ' *'
065200030428     C                   write     Fiwalc00
065300030429     c                   feod      Fiwalc1L
065400030428     c                   end
065500950127     C***
065600950127     C* DEFINIZIONE CAMPI
065700950127     C***
065800020320     C     *like         define    kpjbu         savpjbu
065900011113     c     *like         define    vi2itt        vi2itx
066000000711     C     *LIKE         DEFINE    BLPLNP        SAVLNP
066100000711     C     *LIKE         DEFINE    TBLKEY        §KEY
066200950127     C     *LIKE         DEFINE    TBLCOD        §COD
066300950127     C     *LIKE         DEFINE    TBLKUT        §KUT
066400950127     C* AAMMGG DATA IMMESSA A VIDEO
066500950127     C     *LIKE         DEFINE    FVVNPG        W01NPG
066600950127     C     *LIKE         DEFINE    BLPPDR        W01CPR                          PADR LUNG 3
066700950127     C     *LIKE         DEFINE    FTTPDR        W01PDR                          PADR LUNG 7
066800950127     C     *LIKE         DEFINE    FTTPDR        W02PDR                          PADR LUNG 7
066900950127     C     *LIKE         DEFINE    FTTDDC        W01DDC
067000950127     C     *LIKE         DEFINE    FTTDDC        W02DDC
067100950127     C     *LIKE         DEFINE    FTTNDC        W01NDC
067200000307     C     *LIKE         DEFINE    FTTNDC        KNDC
067300000307     C     *LIKE         DEFINE    FTTTSR        KTSR
067400950127     C     *LIKE         DEFINE    FTTTSR        W01TSR
067500021217     C     *LIKE         DEFINE    fttTBS        w01TBS
067600950127     C     *LIKE         DEFINE    FTTTBN        W01TBN
067700950127     C     *LIKE         DEFINE    FTTTPE        W01TPE
067800950127     C     *LIKE         DEFINE    FTTITA        W01ITA
067900950127     C     *LIKE         DEFINE    FTTITA        DIFITA
068000950127     C     *LIKE         DEFINE    FTTITT        W01ITT
068100021218     C     *LIKE         define    fttITT        w01ran
068200950127     C     *LIKE         DEFINE    FTTITT        DIFITT
068300950127     C     *LIKE         DEFINE    FTTMNT        WMNT
068400020329     C     *LIKE         DEFINE    FTTMNT        WGIO
068500020329     C     *LIKE         DEFINE    vi2tpv        wret
068600950127     C     *LIKE         DEFINE    FTTSNA        W01SNA
068700950127     C     *LIKE         DEFINE    FTTKFA        W01KFA
068800950127     C     *LIKE         DEFINE    FTTCLA        W01CLA
068900950127     C     *LIKE         DEFINE    FTTCPE        W01CPE
069000950127     C     *LIKE         DEFINE    FTTTVL        W01TVL
069100950127     C     *LIKE         DEFINE    FTTSET        W01SET
069200950127     C     *LIKE         DEFINE    FTTSNT        W01SNT
069300950127     C     *LIKE         DEFINE    FTTSNP        W01SNP
069400950127     C     *LIKE         DEFINE    FTTSNE        W01SNE
069500950127     C     *LIKE         DEFINE    FTTSNP        W02SNP
069600950127     C     *LIKE         DEFINE    FTTSET        W02SET
069700950127     C     *LIKE         DEFINE    FTTSNT        W02SNT
069800950127     C     *LIKE         DEFINE    FTTSNE        W02SNE
069900950127     C     *LIKE         DEFINE    FTTFVL        W01FVL
070000950127     C     *LIKE         DEFINE    FTTFVL        W02FVL
070100950127     C     *LIKE         DEFINE    FTDSTP        W01STP
070200950127     C     *LIKE         DEFINE    FTTPEP        W01PPE
070300950127     C     *LIKE         DEFINE    FTTPEP        W02PPE
070400950127     C     *LIKE         DEFINE    NRR           W01NRR
070500950127     C     *LIKE         DEFINE    NRR           W02NRR
070600950127     C     *LIKE         DEFINE    BLPCCM        W01CCM
070700950127     C     *LIKE         DEFINE    BLPNRT        W01NRT
070800950912     C     *LIKE         DEFINE    FTTPDR        COMPDR
070900950912     C     *LIKE         DEFINE    FTTDDC        COMDDC
071000970415     C     *LIKE         DEFINE    FTDRSC        SAVRSC
071100970415     C     *LIKE         DEFINE    FTDKSC        SAVKSC
071200970415     C     *LIKE         DEFINE    FTDSET        SAVSET
071300020419     C     *LIKE         DEFINE    FTDSET        Sv1SET
071400970415     C     *LIKE         DEFINE    FTDSTP        SAVSTP
071500970924     C     *LIKE         DEFINE    FTDSTP        SAVSTO
071600970930     C     *LIKE         DEFINE    FTTSET        COMSET
071700020327     c     *like         define    fcwpdr        kpdrw
071800020327     c     *like         define    fcwfgs        kfgsw
071900020327     c     *like         define    fcwddc        kddcw
072000020329     C     *LIKE         DEFINE    fcwice        val_ice
072100020329     C     *LIKE         DEFINE    fcwice        resto_ice
072200950127     C                   Z-ADD     0             B                 2 0
072300950127     C                   Z-ADD     0             C                 2 0
072400950127     C                   Z-ADD     4             W01NPG
072500950127     C                   ENDSR
072600950127     C*
072700950127     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
072800020402     C     PUL01         BEGSR
072900950127     C                   CLEAR                   VI1DDC
073000950127     C                   CLEAR                   VI1P1
073100950127     C                   CLEAR                   VI1P2
073200020701     C*                  Z-ADD     CDU           VI1PF1
073300020701     C                   Z-ADD     SimPOU        VI1PF1
073400950127     C                   CLEAR                   W02NRR
073500950127     C                   ENDSR
073600121017     C* --------------------------------------------------------------
073700121017     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
073800121017     C* --------------------------------------------------------------
073900121017     C     CTRL_FIRME    BEGSR
074000121017     C*
074100121017     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
074200121017     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
074300121017     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
074400121017     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
074500121017     c                   move      'N'           devo_bloccare     1
074600121017     C                   MOVEL     kpjbu         savpjbu
074700121017     C*
074800121017      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
074900121017      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
075000121017      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
075100121017      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
075200121017      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
075300121017     C*
075400121017     C                   clear                   kpjbu
075500121017     C                   clear                   ficng5ds
075600121017     c                   eval      §G5CNVL  = 'V'
075700121017     c                   eval      §G5FIL   = VI1PF1
075800121017     C                   MOVEL     ficng5ds      kpjbu
075900121017     c                   call      'FICNG5R'
076000121017     C                   parm                    kpjba
076100121017     C                   MOVEL     kpjbu         ficng5ds
076200121017     C                   clear                   kpjbu
076300121017     C                   MOVEL     savpjbu       kpjbu
076400121017     C*
076500121017     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
076600121017     C*    e devono firmare prima tutti i documenti in sospeso
076700121017     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
076800121017     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
076900121017     c                   if        §G5ESITO ='B'
077000121017     c                   eval        devo_bloccare = 'S'
077100121017     c                   end
077200121017     C*
077300121017     C                   ENDSR
077400121017     C* --------------------------------------------------------------
077500121017     C*--- CONTROLLI FORMATO1 ----------------------------------------*
077600121017     C* --------------------------------------------------------------
077700121017     C     CONTR1        BEGSR
077800121017     C*
077900950127     C                   SETOFF                                       90
078000950127     C                   CLEAR                   W01DDC
078100950127     C*
078200950127     C* F I L I A L E   P A D R O N C I N O
078300950127     C     VI1PF1        IFEQ      0
078400950127     C                   SETON                                        3990
078500950127     C                   GOTO      ENDCT1
078600131022     C                   endif
078700020402      *
078800020402     C* Controllo autorizzazione all'utilizzo del programma
078900020402     c* Controllo se abilitato
079000131008     c                   clear                   tibs42ds
079100131008     c                   movel     vi1pf1        i42pge
079200131008     c                   call      'TIBS42R'
079300131008     c                   parm                    tibs42ds
079400131008     c                   movel     o42uni        uteautds
079500020402     c* 98 indicatore che protegge tutto
079600020402     c     §autcon       ifne      'S'
079700020402     C                   SETON                                        3890
079800020402     C                   GOTO      ENDCT1
079900020402     C                   endif
080000020402      *
080100950127     C* P A D R O N C I N O   DAL AL
080200950127     C*
080300950127    1C     VI1PD2        IFGT      0
080400950127    2C     VI1PD2        IFLT      VI1PD1
080500950127     C                   SETON                                        4190
080600950127     C                   GOTO      ENDCT1
080700950127    2C                   ENDIF
080800950127     C*
080900950127   X1C                   ELSE
081000950127     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
081100950127    2C     VI1PD1        IFGT      0
081200950127     C*
081300021202     C     kapdv         CHAIN     FIAPD01L                           30
081400950127     C     *IN30         IFEQ      *ON
081500950127     C                   SETON                                        4290
081600950127     C                   GOTO      ENDCT1
081700950127    3C                   ENDIF
081800950127     C*é
081900950127    2C                   ENDIF
082000950127    1C                   ENDIF
082100950127     C*
082200950127     C* D A T A   D I S T I N T A
082300950127    1C     VI1DDC        IFGT      0
082400950127     C                   MOVE      VI1DDC        G02DAT
082500950127     C                   MOVEL     *BLANK        G02ERR
082600950127     C                   CALL      'XSRDA8'
082700950127     C                   PARM                    WLBDAT
082800950127    2C     G02ERR        IFEQ      '1'
082900950127     C                   SETON                                        43  90
083000950127     C                   GOTO      ENDCT1
083100950127    2C                   END
083200950127     C                   Z-ADD     G02INV        W01DDC
083300950127    1C                   ENDIF
083400950127     C*
083500950127     C  N90              MOVEL     VI1PF1        VI1PF2
083600950127     C*
083700950127     C     ENDCT1        ENDSR
083800950127     C*
083900950127     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
084000950127     C     CARNDC        BEGSR
084100950127     C                   SETOFF                                       0102
084200951009     C                   SETOFF                                       3157
084300020408     c                   z-add     1             wrc2
084400950127     C                   Z-ADD     0             NRR               5 0
084500950127     C                   Z-ADD     0             W01NRR
084600950127     C                   MOVEL     'V'           W01FVL
084700950127     C* PULIZIA CAMPI DI TOTALE
084800950127     C                   CLEAR                   WMTAR
084900950127     C                   CLEAR                   W01SNA
085000950127     C                   CLEAR                   W01KFA
085100950127     C                   CLEAR                   W01TVL
085200950127     C                   CLEAR                   W01CLA
085300950127     C                   CLEAR                   W01CPE
085400950127     C                   CLEAR                   W01ITA
085500950127     C                   CLEAR                   W01ITT
085600950127     C                   CLEAR                   W01SNP
085700950127     C                   CLEAR                   W01SET
085800950127     C                   CLEAR                   W01SNT
085900950127     C                   CLEAR                   W01SNE
086000950127     C                   CLEAR                   W01TBN
086100950127     C                   CLEAR                   W01TOT
086200950127     C                   CLEAR                   W01IGR
086300950127     C                   CLEAR                   W02SNP
086400950127     C                   CLEAR                   W02SET
086500950127     C                   CLEAR                   W02SNT
086600950127     C                   CLEAR                   W02SNE
086700950127     C                   CLEAR                   W01PPE
086800950127     C                   CLEAR                   W02PPE
086900021218     C                   clear                   w01TBS
087000021219     C                   clear                   w01ran
087100130404      *
087200130404     C                   clear                   §5aAUTISTA
087300950127     C* PULIZIA SUBFILE
087400950127     C                   SETON                                        35
087500950127     C                   WRITE     FRC0CT2
087600950127     C                   SETOFF                                       35
087700950127     C*
087800950127     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
087900950127     C* 02 ON - IMMESSI PADRONCINI DAL AL
088000950127     C*
088100950127     C* PADRONCINO
088200950127     C     VI1PD2        IFGT      0
088300950127     C     VI1P2         ANDNE     VI1P1
088400950127     C                   SETON                                        02
088500950127     C                   ELSE
088600950127     C*
088700950127     C     VI1PD1        IFGT      0
088800950127     C                   SETON                                        01
088900950127     C                   END
089000950127     C                   ENDIF
089100950127     C                   Z-ADD     VI1P1         W01PDR
089200950127     C**
089300950127     C* POSIZIONAMENTO PER LETTURA
089400950127     C**
089500950127     C     VI1PD1        IFGT      *ZEROS
089600011113     C     KFTT2         SETLL     FIFTT002
089700950127     C                   ELSE
089800950127     C                   EXSR      IMPO1
089900950127     C                   END
090000950127     C*
090100950127     C* LETTURA DISTINTE/RITIRI
090200950127     C     *IN31         IFEQ      *OFF
090300950127     C                   EXSR      LEGNDC
090400950127     C                   ELSE
090500950127     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
090600950127     C                   SETON                                        4490
090700950127     C                   MOVEL     ERR(4)        VI1ERR
090800950127     C                   ENDIF
090900950127     C*
091000950127     C                   ENDSR
091100950127     C*
091200950127     C* IMPOSTAZIONE LETTURA PER PADRONCINO DAL = 0 ------------------*
091300950127     C     IMPO1         BEGSR
091400011113     C     W01PDR        SETGT     FIFTT002
091500011113     C                   READ(N)   FIFTT002                               31
091600950202     C*
091700950202     C     *IN31         IFEQ      *OFF
091800950202     C                   MOVEL     FTTPDR        W03               3 0
091900950202     C     VI1PF1        IFNE      W03
092000950202     C                   SETON                                        31
092100950202     C                   END
092200950202     C                   END
092300950127     C*
092400950127     C     *IN31         IFEQ      *OFF
092500950127     C                   Z-ADD     FTTPDR        W01PDR
092600011113     C     KFTT2         SETLL     FIFTT002
092700950127     C                   ENDIF
092800950127     C                   ENDSR
092900950127     C*
093000950127     C* LETTURA DITINTE DA CARICARE ----------------------------------*
093100950127     C     LEGNDC        BEGSR
093200950127     C                   CLEAR                   W01TSR
093300950127     C                   CLEAR                   W02PDR
093400950127     C                   CLEAR                   W02DDC
093500950127     C                   CLEAR                   W02NRR
093600950912     C                   CLEAR                   COMPDR
093700950912     C                   CLEAR                   COMDDC
093800020328     C                   CLEAR                   comkgc            8 1
093900020328     C                   CLEAR                   comkgr            8 1
094000950127     C* L E T T U R A
094100950127     C     *IN31         DOWEQ     *OFF
094200950127     C                   SETOFF                                       9132
094300950127     C                   SELECT
094400950127     C** LEGGO PER PADRONCINO/FLAG VALORIZZ
094500950127     C     *IN01         WHENEQ    *ON
094600950127     C*
094700011113     C     KFTT2         READE(N)  FIFTT002                               31
094800950127     C*
094900950127     C*  LEGGO I PADRONCINI (TUTTI O NEL DAL/AL)
095000950127     C                   OTHER
095100950127     C*
095200950127     C     *IN32         DOWEQ     *OFF
095300011113     C     KFTT2         READE(N)  FIFTT002                               31
095400950127     C*
095500950127     C     *IN31         IFEQ      *ON
095600950127     C                   EXSR      IMPO1
095700950127     C   31              SETON                                        32
095800950127     C                   ELSE
095900950127     C                   SETON                                        32
096000950127     C                   ENDIF
096100950127     C                   ENDDO
096200950127     C*
096300950127     C                   ENDSL
096400950127     C*
096500950127     C* C O N F R O N T O   L  E   S E L E Z I O N I
096600950127     C  N31              EXSR      CONFR
096700950127     C*
096800950127     C* C A R I C A M E N T O   S F L
096900950127     C* 91 ON - RECORD DA NON SELEZIONARE
097000950127     C     *IN31         IFEQ      *OFF
097100950127     C     *IN91         ANDEQ     *OFF
097200950127     C                   EXSR      CARSF2
097300950127     C   90              SETON                                        31
097400950127     C                   ENDIF
097500950127     C                   ENDDO
097600950127     C*
097700950127     C* ULTIMA ROTTURA DI PADRONCINO
097800950509     C  N90W02PDR        IFNE      0
097900950127     C                   EXSR      ROTPAD
098000950127     C                   ENDIF
098100950127     C*
098200950127     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
098300950127     C  N90NRR           IFEQ      0
098400951009     C                   SETON                                        4490
098500951009     C                   MOVEL     ERR(4)        VI1ERR
098600950127     C                   ENDIF
098700020620     C* comunque deve impostare il numero di records  anche se c'è un errore
098800020620     C* altrimenti non esegue la DEL_FIFCW
098900020620     C                   Z-ADD     NRR           W01NRR
099000950127     C*
099100950127     C                   ENDSR
099200950127     C*
099300950127     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
099400950127     C     CONFR         BEGSR
099500950127     C* FLAG VALORIZZAZIONE
099600950127     C     FTTFVL        IFNE      W01FVL
099700950127     C                   SETON                                        91
099800950127     C                   GOTO      ENDCON
099900950127     C                   ENDIF
100000950127     C*
100100950127     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
100200950127     C   02FTTPDR        IFGT      VI1P2
100300950127     C                   SETON                                        3191
100400950127     C                   GOTO      ENDCON
100500950127     C                   ENDIF
100600950127     C* DATA DISTINTA
100700950127     C     VI1DDC        IFGT      0
100800950127     C     FTTDDC        ANDGT     W01DDC
100900950127     C                   SETON                                        91
101000950127     C                   GOTO      ENDCON
101100950127     C                   ENDIF
101200950127     C* SE TUTTO OK MA HA MANCA TARIFFA ERRORE
101300950127     C     FTTFLA        IFEQ      'S'
101400950127     C                   SETON                                        469031
101500020618     C                   SETON                                        45
101600950127     C                   MOVEL     FTTPDR        VI1PDE
101700020618     C                   move      fttddc        data
101800020618     C                   clear                   data6             6
101900020618     c                   eval      data6=gg+mm+aa
102000020618     C                   move      data6         vi1dde
102100950127     C                   MOVEL     ERR(5)        VI1ERR
102200950911     C                   GOTO      ENDCON
102300950127     C                   ENDIF
102400950911     C* SE TUTTO OK CONTROLLO CHE QUALCUN ALTRO NON STIA CONFERMANDO
102500950911     C* LO STESSSO PADRONCINO/DATA DISTINTA
102600950912     C     FTTPDR        IFNE      COMPDR
102700950912     C     FTTDDC        ORNE      COMDDC
102800951011     C                   MOVE      PARSML        WLRSML
102900951011     C                   MOVE      FTTPDR        WLRPDR
103000951011     C                   MOVE      FTTDDC        WLRDDC
103100951011     C                   MOVE      DATEU8        WLRIMM
103200951011     C                   MOVEL     KNMEB         WLRNMB
103300951011     C                   MOVEL     KNMUS         WLRNUS
103400951011     C                   Z-ADD     KNRJO         WLRNJO
103500951011     C                   WRITE     FNWLRC00                             30
103600950911     C  N30              MOVE      'N'           WEXIST            1
103700950911     C     *IN30         IFEQ      *ON
103800951011     C     KWC0          CHAIN     FNWLRC0F                           30
103900950911     C     *IN30         IFEQ      *OFF
104000951011     C     WLRIMM        ANDLT     DATEU8
104100951011     C                   Z-ADD     DATEU8        WLRIMM
104200951011     C                   MOVEL     KNMEB         WLRNMB
104300951011     C                   MOVEL     KNMUS         WLRNUS
104400951011     C                   Z-ADD     KNRJO         WLRNJO
104500951011     C                   UPDATE    FNWLRC00
104600950911     C                   MOVE      'N'           WEXIST
104700950911     C                   ELSE
104800000307     C* SE STESSO UTENTE/TERMINALE DO X SCONTATO CHE SI TRATTI DI UN
104900000307     C* LAVORO TERMINATO IN MODO ANOMALO
105000020701     C*    KNMUS         IFNE      WLRNUS
105100020701     C*    KNMTD         ORNE      WLRNMB
105200020701     C*    REM           OREQ      'EDP'
105300020701     C                   IF        SimUte <> WLRNUS
105400020701     C                             OR
105500020701     C                             KNmTD <> WLRNMB
105600020701     C                             OR
105700020701     C                             %SUBST(SimUte:1:3) = 'EDP'
105800950911     C                   MOVE      'E'           WEXIST
105900951011     C                   UNLOCK    FNWLRC0F
106000960930     C                   ELSE
106100960930     C                   Z-ADD     DATEU8        WLRIMM
106200960930     C                   MOVEL     KNMEB         WLRNMB
106300960930     C                   MOVEL     KNMUS         WLRNUS
106400960930     C                   Z-ADD     KNRJO         WLRNJO
106500960930     C                   UPDATE    FNWLRC00
106600960930     C                   MOVE      'N'           WEXIST
106700960930     C                   END
106800950911     C                   END
106900950911     C                   END
107000950912     C                   MOVE      FTTPDR        COMPDR
107100950912     C                   MOVE      FTTDDC        COMDDC
107200950912     C                   END
107300950912     C     WEXIST        IFEQ      'E'
107400951009     C  N57              MOVEL     ERR(6)        VCZMSG
107500951009     C  N57              MOVEL     ERR(6)        VI2ERR
107600951009     C                   SETON                                        9157
107700950912     C                   END
107800950127     C*
107900950127     C     ENDCON        ENDSR
108000950127     C*
108100950127     C* CARICO SUBFILE -----------------------------------------------*
108200950127     C     CARSF2        BEGSR
108300950127     C* TIPI RECORD DEL SUBFILE: "P" - PADRONCINO
108400950127     C*                          "S" - TIPO PRESTRAZIONE R O C
108500950127     C*                          "G" - TIPO PRESTAZIONE A GIORNATA
108600950127     C*                          "M" - MINIMO
108700950127     C*                          "T" - TOTALE
108800950127     C* CAMBIATO PADRONCINO
108900950127     C     FTTPDR        IFNE      W02PDR
109000120910      **
109100950127     C     W02PDR        IFNE      0
109200950127     C                   EXSR      ROTPAD
109300050620     c   90              goto      endcar
109400950127     C                   CLEAR                   W01TSR
109500950127     C                   CLEAR                   W02DDC
109600950127     C                   ENDIF
109700950127     C*
109800950127     C                   CLEAR                   FRC0SF2
109900950127     C*
110000021202     C     kapdf         CHAIN     FIAPD01L                           30
110100100223     C   30              CLEAR                   APDRSF
110200120911     C*******
110300120911     C**  controlli sulla Tariffa/Soc.Fornitore
110400120911     c                   exsr      SocFRN_Tariffa
110500120911     c   90              goto      endcar
110600120821     C*
110700090512     C*  se NON è accreditato l'autista e la tariffa è stata confermata
110800090512     C* occorre controllare il limite di giorni (preso dalla tabella 5aAUT)
110900090512     C*  entro i quali è possibile confermare altrimenti bloccare la Conferma
111000090512     C*
111100950127     C                   CLEAR                   K18
111200950127     C                   MOVEL     FTTPDR        WALF7             7
111300011113     c                   movea     walf7         k08
111400011113     c                   exsr      alfab
111500011113     c                   movea     k08           k18(1)
111600011113     c                   move      ' '           k18(8)
111700950127     C                   MOVEA     K18           VI2DES
111800950127     C                   MOVEL     FTTPDR        W02PDR
111900950127     C                   MOVEL     FTTPDR        VH2PDR
112000011113     c                   movel     fttdiv        vh2div
112100090507      *
112200950127     C* "P" TIPO RECORD PADRONCINO
112300950127     C                   MOVEL     'P'           VH2TRC
112400950127     C                   ADD       1             NRR
112500950127     C                   WRITE     FRC0SF2
112600011113      *
112700011113     C                   CLEAR                   K18
112800100223     C                   MOVEA     APDRSF        K18(1)
112900011113     C                   MOVEA     K18           VI2DES
113000011113     C* "P" TIPO RECORD PADRONCINO
113100020513     C*
113200020513     C                   CLEAR                   WflgR             1
113300020513     C                   CLEAR                   WflgC             1
113400020513     C*
113500011113     C                   MOVEL     'P'           VH2TRC
113600011113     C                   ADD       1             NRR
113700011113     C                   WRITE     FRC0SF2
113800011113      *
113900950127     C                   ENDIF
114000950127     C* DATA DISTINTA
114100950127     C     FTTDDC        IFNE      W02DDC
114200950127     C     W02DDC        IFNE      0
114300950127     C                   EXSR      ROTPAD
114400950127     C                   CLEAR                   W01TSR
114500950127     C                   ENDIF
114600120911     C*
114700120911     C**  controlli sulla Tariffa/Soc.Fornitore
114800120911     c                   exsr      SocFRN_Tariffa
114900120911     c   90              goto      endcar
115000950127     C*
115100950127     C* CONTROLLO SE ESISTONO ANCORA DELLE DISTINTE/RITIR DA VALORIZZAR
115200950127     C                   MOVEL     FTTDDC        W02DDC
115300950127     C                   EXSR      CTRDIS
115400950127     C                   ENDIF
115500950127     C* TIPO PRESTAZIONE
115600950127     C     FTTTSR        IFNE      W01TSR
115700950127     C     W01TSR        ANDNE     ' '
115800950127     C                   EXSR      ROTTSR
115900950127     C                   ENDIF
116000950127     C*
116100950127     C* SE C'E' SOMMA PESI A GIORNATA LA CALCOLO
116200950127     C     FTTCPE        IFGT      0
116300950127     C     FTTKFA        ORGT      0
116400950127     C                   Z-ADD     FTTNDC        W01NDC
116500950127     C                   ADD       FTTSNA        W01SNA                         TOT.STOP
116600950127     C                   ADD       FTTKFA        W01KFA                         TOT.PESO
116700020328     c     ftttsr        ifeq      'C'
116800020328     C                   add       FTTKFA        comkgc                         TOT.PESO
116900020328     C                   add       FTTKFA        vh2kgc
117000020507      * s/n pagare picking
117100020513     c                   if        vh2pgp <> 'S'
117200020507     C                   MOVE      fttpep        vh2pgp                         x picking
117300020513     c                   end
117400020328     c                   else
117500020328     C                   add       FTTKFA        comkgr                         TOT.PESO
117600020328     C                   add       FTTKFA        vh2kgr
117700020507      * s/n pagare etichettatura
117800020513     c                   if        vh2pge <> 'S'
117900020507     C                   MOVE      fttpep        vh2pge                         x etichetta
118000020513     c                   end
118100020328     c                   end
118200950127     C                   ADD       FTTTVL        W01TVL                         TOT.VOLUME
118300950127     C* Sommo tot.colli solo se pag.pik/etich.='S'
118400950127     C     FTTPEP        IFEQ      'S'
118500950127     C                   ADD       FTTCLA        W01CLA                         TOT.COLLI
118600950127     C                   ADD       FTTCPE        W01CPE                         T.COLLI PIK/ET.
118700950127     C                   ENDIF
118800950127     C                   ENDIF
118900030326     C*
119000030326     C* A prestazione: anche se solo la mattina o solo il pomeriggio
119100030326     C     FTTPEP        IFNE      'N'
119200030326     C                   MOVE      'S'           W01PPE
119300030326     C                   ENDIF
119400950127     C*
119500030326     C* A giornata: invece deve avere sia mattina che pomeriggio
119600950127     C     FTTPEP        IFNE      'S'
119700950127     C                   MOVE      'N'           W02PPE
119800950127     C                   ENDIF
119900950127     C*
120000950127     C                   ADD       FTTSET        W01SET
120100950127     C                   ADD       FTTSNT        W01SNT
120200950127     C                   ADD       FTTSNP        W01SNP
120300950127     C                   ADD       FTTSNE        W01SNE
120400950127     C                   ADD       FTTITT        W01ITT
120500950127     C                   ADD       FTTITA        W01ITA
120600950127     C                   MOVEL     FTTTSR        W01TSR
120700021218     c                   add       fttTBS        w01TBS
120800950127     C*
120900020329     C     endcar        ENDSR
121000950127     C* --------------------------------------------------------------*
121100120911     C*  Controllo da Tariffa la Società ed il Fornitore
121200120911     C* --------------------------------------------------------------*
121300120911     C     SocFRN_TariffaBEGSR
121400120911     C*
121500120911     c                   clear                   supertestata      1
121600120911     c                   clear                   accreditato       1
121700130404     c                   move      *all'0'       TAR_Forn
121800130404     c                   move      *all'0'       TAR_Socie
121900130404     c                   move      *blank        TAR_Piva
122000120911     C*
122100120911      *  ma li deve prendere assolutamente dalla Supertestata tariffe
122200120911     C     ktgt          setll     FItgt01L
122300120911     C     ktgt          reade     FItgt01L
122400120911     c                   dow       not %EoF(FItgt01L)
122500120911     C*  interno al periodo
122600120911     c                   if        FTTDDC >= tgtddt and FTTDDC<=tgtdst and
122700120911     c                             tgtATB  = ' '
122800120911     c                   move      'S'           supertestata
122900120911     c                   move      tgtcdf        vh2cdf
123000130404     c                   move      tgtcdf        TAR_Forn
123100120911     c                   move      tgtsoc        vh2soc
123200130404     c                   move      tgtsoc        TAR_Socie
123300130404      **  Recupera la P.IVA
123400130404      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
123500130404      *   a quella del contratto
123600130404     C                   move (p)  tgtCDF        parm_s4CDF                        Input
123700130404     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
123800130404     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
123900130404     c                   exsr      DECOD_P_IVA
124000130404      * se la P.IVA coincide
124100130404     c                   if        s4_errore ='0'
124200130404     c                   eval      TAR_Piva = s4_PARTITAIVA
124300130404     c                   end
124400130404      *
124500120911     c                   leave
124600120911     c                   end
124700120911     C     ktgt          reade     FItgt01L
124800120911     c                   enddo
124900120911      **
125000120911      **  Se Società o Fornitore non presenti in tariffa
125100120911      **    BLOCCARE ASSOLUTAMENTE la Valorizzazione !!
125200120911      **
125300120911     C* Controlla che ci sia il Codice Fornitore e il Sistema Inf.
125400120911     C*  altrimenti segala l'errore
125500120911     c                   if        (*in30=*Off and apdksc= 0 and
125600120911     c                             supertestata = ' ')
125700120911     c                             or
125800120911     c                             (*in30=*Off and vh2CDF= 0
125900120911     c                             and parsml=' ' and
126000120911     c                             supertestata = 'S')
126100120911     C                   SETON                                        4690
126200120911     C                   MOVEL     fttPDR        VI1PDE
126300120911     C                   MOVEL     ERR(8)        VI1ERR
126400120911     c                   goto      endsocfrn
126500120911     c                   end
126600120911     **
126700120911     c                   if        (*in30=*Off and apdcsf= *blanks and
126800120911     c                             supertestata = ' ')
126900120911     c                             or
127000120911     c                             (*in30=*Off and vh2SOC= *blanks
127100120911     c                             and parsml=' ' and
127200120911     c                             supertestata = 'S')
127300120911     C                   SETON                                        4690
127400120911     C                   MOVEL     fttPDR        VI1PDE
127500120911     C                   MOVEL     ERR(9)        VI1ERR
127600120911     c                   goto      endsocfrn
127700120911     c                   end
127800120911     C*
127900120911     C* Controlla se l'autista è stato disaccreditato ad una certa data.
128000120911     C*  Per cui deve valorizzare fino alla data disaccreditamento oltre
128100120911     C*  la quale invece deve bloccare la valorizzazione dando una
128200120911     C*  segnalazione a video.
128300120911     c                   clear                   vh2DFI
128400120911     c     Ktra03        chain     aitra03l
128500130325     C* ================:
128600120911     c                   if        %Found(aitra03l)
128700130325     C* ================:
128800120911     C* Se è accreditato:
128900120911     c                   eval      accreditato = 'S'
129000130325     C*
129100130325      * Se disaccreditato controlla se era sullo stesso fornitore della tariffa
129200130325     c     tranrc        chain     aitrs01l
129300130325     c                   if        %Found(aitrs01l)
129400130325      *
129500130325     C                   exsr      Decod_Rag_IVA
129600130325      *
129700130325     C*  Controlla se la P.IVA del contratto è quella della tariffa non Coincidono
129800130325     C*    altrimenti se le P.IVA non coincidono l'autista NON è accreditato.
129900130404     c                   if            TRS_Piva  <> TAR_Piva
130000130327      *
130100130404      *  Esegue il controllo su accreditamento e Ritorno Copia Firmata della tariffa
130200130404      *   solo se NON in simulazione e se è cambiato il codice del cliente trattato
130300130404     c                   exsr      CTRacrcf_TGT
130400130325      *
130500130325     c                   endIF
130600130325     c                   endIF
130700130325      *
130800130325     C* ================:
130900120911     c                   Else
131000130325     C* ================:
131100120911     C* Se non l'ha trovato o non esiste l'aut su AITRA oppure è disaccreditato
131200120911     c                   eval      accreditato = 'N'
131300120911     C*
131400120911     C* Quindi cerca con l'AUT se disaccreditato e....
131500120911     C* Se disaccreditato deve vedere l'ultimo disaccreditamento a quando risale
131600120911     C*  per controllare fino a quando può essere abilitato alla valorizzazione.
131700120911     c     fttpdr        setgt     aitra03l
131800120911     c     fttpdr        readpe    aitra03l
131900120911     C*
132000120911     C*  Posso valorizzare fino al giorno
132100120911     C*     della data disaccreditamento
132200120911     c                   if        not %EoF(aitra03l)
132300130325      *  Lo reimposta come default xchè lo ha trovato con DFI>0
132400130325     c                   eval      accreditato = 'S'
132500120911      *
132600130325      *  Se la DATA FINE accreditamento è INFERIORE del DOCUMENTO da valorizzare
132700120911     C                   if        traDFI < fttddc
132800130325      *
132900120911      * Solo se disaccreditato sullo stesso fornitore della tariffa
133000120911     c     tranrc        chain     aitrs01l
133100120911     c                   if        %Found(aitrs01l)
133200120911      *
133300120911     C                   exsr      Decod_Rag_IVA
133400130325      *
133500130325      *  Se la P.IVA corrisponde ALLORA è stato disaccreditato in data X
133600130404     c                   IF            TRS_Piva = TAR_Piva
133700130325      *
133800130325     c                   eval      accreditato = 'N'
133900130325     c                   eval      vh2DFI = traDFI
134000120911      *
134100130325      *  Se non corrisponde alla P.IVA del Fornitore in Tariffa
134200130404     c                   elseIF        TRS_Piva  <> TAR_Piva
134300130404      *
134400130404      *  Esegue il controllo su accreditamento e Ritorno Copia Firmata della tariffa
134500130404      *   solo se NON in simulazione e se è cambiato il codice del cliente trattato
134600130404     c                   exsr      CTRacrcf_TGT
134700130325      *
134800120911     c                   endIF
134900130327     c                   endIF
135000120911      *
135100120911     c                   end
135200120911     c                   end
135300130325     C* ================:
135400120911     c                   endIF
135500130325     C* ================:
135600120911     C*
135700120911     c                   setoFF                                       48
135800120911     c                   If         vh2DFI > 0
135900120911     C                   SETON                                        4890
136000120911     C                   MOVEL     fttPDR        VI1PDE
136100120911     C                   move      vh2DFI        data
136200120911     C                   clear                   data6             6
136300120911     c                   eval      data6=gg+mm+aa
136400120911     C                   move      data6         vi1dde
136500120911     C                   MOVEL     ERR(13)       VI1ERR
136600120911     c                   goto      endsocfrn
136700120911     c                   endIf
136800120911     C*
136900120911     C*  solo se NON simulate
137000120911     C                   if        parSML = ' '
137100120911     C* Controlla se la società Operativa è corretta per la valorizzazione
137200120911     C*   alla data della valorizzazione.
137300120911     C     *iso          move      FTTDDC        dtVALIDITA
137400120911     C                   MOVEL     FTTPDR        Filiale
137500120911     c                   exsr      chk_SOC_ope
137600120911      *
137700120911     C                   if        soc_errata = SI  or
137800120911     C                             TAR_socie  <> idSocieta
137900120911     C                   SETON                                        4890
138000120911     C                   move      FTTDDC        data
138100120911     C                   clear                   data6             6
138200120911     c                   eval      data6=gg+mm+aa
138300120911     C                   move      data6         vi1dde
138400120911     C                   MOVEL     fttPDR        VI1PDE
138500120911     C                   MOVEL     ERR(15)       VI1ERR
138600120911     c                   goto      endsocfrn
138700120911     C                   end
138800120911     C                   endIF
138900120911     C*
139000120911     C     endsocfrn     ENDSR
139100120911     C* --------------------------------------------------------------*
139200130404     C*   Controlla Accreditamento e Ritorno Copia Firmata da TGT
139300120911     C* --------------------------------------------------------------*
139400130404     C     CTRacrcf_TGT  BEGSR
139500130404      *
139600130404      * ci entra solo se è cambiato l'autista da testare
139700130404     c                   if        tgtPDR <> §5Aautista
139800130404     C                             AND parSML = ' '
139900130404      *
140000130404     c                   clear                   ficn5ads
140100130404     C                   MOVEL     kpjbu         savpjbu
140200130404     c                   z-add     tgtPDR        §5Aautista
140300130404     c                   z-add     tgtPRG        §5Aprgtar
140400130404     c                   movel     tgtFLR        DtgtFLR
140500130404     c                   movel     §tgtRIS       §5ARISTAMP
140600130404     C                   z-add     fttddc        §5ADATA1
140700130404     C                   z-add     tgtdts        §5ADATA2
140800130404     c                   eval      §5AWINDOW  = 'F'
140900130404     C                   clear                   kpjbu
141000130404     C                   MOVEL     ficn5ads      kpjbu
141100130404     C                   call      'FICN5AR'
141200130404     C                   parm                    kpjba
141300130404     C                   eval      ficn5ads = kpjbu
141400130404     C                   eval      ds5aAUT  = §5AUNIDS
141500130404     C                   clear                   kpjbu
141600130404     C                   MOVEL     savpjbu       kpjbu
141700130404     C*
141800130404     C*  Se non è stato ancora accreditato l'autista occorre bloccare dopo
141900130404     C*   "x" giorni --> presi nella tabella 5aAUT.
142000130404      ***    Solo se NON è stato forzato
142100130404     c                   IF        §5Aesito ='1'
142200130404      ***
142300130404     c                   IF        §5AGGDIFF  > 0   and
142400130404     c                             §5ATIPOGG  = '§5AGGVALDA'
142500130404      *  Non è accreditato
142600130404     c                   eval      accreditato = 'N'
142700130404     c                   eval      vh2DFI = traDFI
142800130404      *
142900130404     C                   SETON                                          90
143000130404      *** oppure
143100130404      ***   se è stata Ricevuta la tariffa FIRMATA dopo "x" giorni.
143200130404     c                   elseIF    §5AGGDIFF  > 0   and
143300130404     c                             §5ATIPOGG  = '§5AGGVALDR'
143400130404     c                             and §t3cnv = 0
143500130404     C                   SETON                                          90
143600130404     c                   endif
143700130404      ***
143800130404     c                   endif
143900130404      ***
144000130404     c                   endif
144100130404      *
144200130404     C                   ENDSR
144300130404     C* --------------------------------------------------------------*
144400130404     C*   Controlli su Distinte Aperte
144500130404     C* --------------------------------------------------------------*
144600130404     C     CTRDIS        BEGSR
144700950127     C                   MOVE      VH2PDR        W01CPR
144800950509     C                   SETOFF                                       90
144900950127     C*
145000950127     C* CONTROLLO SE ESISTONO -NELLA DATA- IN ARRIVO:
145100950127     C* - DELLE DISTINTE ANCORA DA CHIUDERE
145200120517     C* - DELLE DISTINTE SU FiDST CON PADRONCINO = A QUELLO
145300001130     C*   DELLA CONFERMA
145400020515      * e non Annullate
145500950127     C     KFVV          SETLL     FNFVV000
145600950127     C     KFVV          READE     FNFVV000                               30
145700950127     C*
145800950127    1C     *IN30         DOWEQ     *OFF
145900950127     C* CONTROLLO SOLTANTO GLI APERTI
146000950127    2C     FVVFCF        IFNE      'S'
146100020515    2C     FVVatb        andeq     *blank
146200001130      *
146300120517ab    *  sostituito il vecchio file delle distinte
146400120517ab   C                   z-add     fvvNPG        dstNPG
146500120517ab   C                   z-add     fvvNFV        dstNFV
146600120517ab   C                   z-add     fvvFGS        dstFGS
146700120517ab   C     KiDST         CHAIN     FiDST01L                           37
146800120517      *
146900001130     C     *IN37         IFEQ      *OFF
147000001130     C     DSTATB        ANDEQ     ' '
147100001130     C     DSTPDR        ANDEQ     VH2PDR
147200001130     C                   SETON                                        453090
147300001130     C                   MOVEL     ERR(1)        VI1ERR
147400110928     c                   eval      %subst(VI1ERR:4:5) = %editc(FVVNFV:'X')
147500001130     C                   ENDIF
147600001130      *
147700001130     C  N30              Z-ADD     FVVNFV        WNUM7             7 0
147800020918     C  N30              Z-ADD     FVVfgs        Wfgs
147900020918     C  N30karb78        CHAIN     FNARB000                           30
148000950207     C*
148100950207     C     *IN30         IFEQ      *OFF
148200950207     C     ARBPDC        ANDEQ     W01CPR
148300950207     C*
148400950127     C                   SETON                                        453090
148500950127     C                   MOVEL     ERR(1)        VI1ERR
148600110928     c                   eval      %subst(VI1ERR:4:5) = %editc(FVVNFV:'X')
148700950127    3C                   ENDIF
148800950127    2C                   ENDIF
148900950127     C*
149000950127     C  N90KFVV          READE     FNFVV000                               30
149100950127    1C                   ENDDO
149200950127     C*
149300950127     C*
149400950127    1C     *IN90         IFEQ      *OFF
149500950127     C* - DELLE DISTINTE CHIUSE MA NON VALORIZZATE
149600950127     C* DA VALORIZZARE
149700950127     C                   Z-ADD     1             C
149800950127    2C     TSR(C)        DOWNE     ' '
149900950127     C*
150000950127     C                   MOVEL     ' '           W02FVL
150100011113     C     KFTT3         SETLL     FIFTT002                               90
150200950127     C* BLOCCATE
150300950127     C  N90              MOVEL     'B'           W02FVL
150400011113     C  N90KFTT3         SETLL     FIFTT002                               90
150500950127    3C     *IN90         IFEQ      *ON
150600950127     C                   SETON                                            45
150700950127     C                   MOVEL     ERR(2)        VI1ERR
150800950127     C                   Z-ADD     19            C
150900950127    3C                   ENDIF
151000950127     C*
151100950127     C                   ADD       1             C
151200950127    2C                   ENDDO
151300950127     C*
151400950127    2C     *IN90         IFEQ      *OFF
151500950127     C* MI RIPOSIZIONO
151600950127     C                   MOVEL     'V'           W02FVL
151700950127     C                   Z-ADD     1             C
151800950127     C     FTTTSR        LOOKUP    TSR(C)                                 30
151900950127     C  N30              Z-ADD     1             C
152000011113     C     KFTT3         CHAIN(N)  FIFTT002                           30
152100950127     C*
152200950127     C* CONTROLLO SE ESISTONO -NELLA DATA- IN PARTENZA:
152300950127     C* - SPEDIZ.CON DATA RITIRO=DATA MA NON ANCORA PARTITE(CHIUS FV)
152400000711     C*  QUESTO CONTROLLO NON LO FACCIO PER BOL POSTE PERCHE'CE NE SONO
152500000711     C*  SEMPRE DI BOLLE MA QUANDO PARTONO VARIAMO LA DATA SPEDIZIONE
152600000711     C*
152700000711     C                   CLEAR                   SAVLNP
152800950127     C     KBLP          SETLL     FNBLP000
152900950127     C     KBLP          READE     FNBLP000                               30
153000950127    3C     *IN30         DOWEQ     *OFF
153100000711     C*
153200000711     C     BLPLNP        IFNE      SAVLNP
153300000711     C     BLPLNP        CHAIN     AZORG01L                           33
153400000711     C  N33              MOVEL     ORGDE3        OG143
153500000711     C   33              CLEAR                   OG143
153600000711     C                   MOVEL     BLPLNP        SAVLNP
153700000711     C                   ENDIF
153800000711     C**
153900020729     C     §OgNTW        IFne      'PPT'
154000950127     C     BLPFT1        IFEQ      ' '
154100950130     C                   SETON                                        459030
154200950127     C                   MOVEL     ERR(3)        VI1ERR
154300950127     C                   ELSE
154400950127     C     KBLP          READE     FNBLP000                               30
154500950127     C                   ENDIF
154600000711     C                   ELSE
154700000711     C* E ALMENO UNA BOLLA E POSTE, ESCO DAL GIRO
154800000711     C*
154900000711     C                   SETON                                            30
155000000711     C                   ENDIF
155100000711     C**
155200950127    3C                   ENDDO
155300950127    2C                   ENDIF
155400000307     C* CONTROLLO SE PER IL PADRONCINO/DATA DISTINTA ESISTE GIA' RECORD
155500000307     C* DI TOTALE
155600000307    2C     *IN90         IFEQ      *OFF
155700000307     C                   CLEAR                   KTSR
155800000307     C                   CLEAR                   KNDC
155900011113     C     KFTT          SETLL     FIFTT01L                               90
156000000307     C   90              SETON                                        45
156100000307     C   90              MOVEL     ERR(7)        VI1ERR
156200000307    2C                   ENDIF
156300950127    1C                   ENDIF
156400950127     C* GIRO LA DATA
156500950127     C     *IN90         IFEQ      *ON
156600950127     C                   MOVE      FTTDDC        DATA
156700950127     C                   MOVE      FTTDDC        VI1DDE
156800950127     C                   MOVEL     GG            VI1DDE
156900950127     C                   MOVE      AA            VI1DDE
157000950127     C                   MOVEL     VH2PDR        VI1PDE
157100950127     C                   ENDIF
157200950127     C*
157300950127     C                   ENDSR
157400950127     C*
157500950127     C* ROTTURA PER PADRONCINO O DATA --------------------------------*
157600950127     C     ROTPAD        BEGSR
157700950127     C                   CLEAR                   WMNT
157800020513     C*
157900950127     C* CALCOLO IL TIPO PRESTAZIONE
158000950127     C                   EXSR      ROTTSR
158100950127     C*
158200950127     C* CALCOLO IL TIPO PRESTAZ "G" GIORNATA
158300950127     C                   MOVEL     'G'           W01TSR
158400950127     C                   SETON                                        12
158500950127     C* TOTALE SPEDIZIONI NON FATTE DELLA GIORNATA
158600950127     C                   Z-ADD     W02SNP        W01SNP
158700950127     C* TOTALE SPEDIZIONI RITIR./CONS. DELLA GIORNATA
158800950127     C                   Z-ADD     W02SET        W01SET
158900950127     C* TOTALE SPEDIZIONI NON CONSEGNATE DELLA GIORNATA
159000950127     C                   Z-ADD     W02SNT        W01SNT
159100950127     C* TOTALE SPEDIZIONI NON CONS/NON PAG DELLA GIORNATA
159200950127     C                   Z-ADD     W02SNE        W01SNE
159300950127     C*
159400950127     C                   EXSR      ROTTSR
159500950127     C*
159600950127     C                   CLEAR                   W02SNP
159700950127     C                   CLEAR                   W02SET
159800950127     C                   CLEAR                   W02SNT
159900950127     C                   CLEAR                   W02SNE
160000950127     C*
160100950127     C*  T O T P A D = TOTALI TARIFFA + TARIFFA A GIORNATA
160200011207     C     W01TOT        ADD       W01IGR        TOTPAD           12 3
160300950127     C*
160400950127     C* ESPOSIZIONE DEL   M I N I M O   A   G I O R N A T A
160500950127     C                   EXSR      PULSFL
160600950127     C     §TAMNT        IFGT      0
160700950127     C                   Z-ADD     §TAMNT        WMNT
160800950127     C                   EXSR      PULSFL
160900950127     C                   Z-ADD     §TAMNT        VI2IGR                          MINIMO
161000950127     C                   MOVE      CMINIM        VI2DES
161100950127     C* "M"= TIPO RECORD DEL MINIMO
161200950127     C                   MOVEL     'M'           VH2TRC
161300950127     C                   ADD       1             NRR
161400950127     C                   SETON                                        1716
161500950127     C* ESPONGO IL TOTALE BONUS
161600950127     C     W01TBN        IFGT      0
161700950127     C                   Z-ADD     W01TBN        VI2TBV                          TOT BONUS
161800950127     C                   SETON                                        19
161900950127     C                   ENDIF
162000950127     C*
162100950127     C                   WRITE     FRC0SF2
162200950127     C                   SETOFF                                       171916
162300950127     C                   ENDIF
162400950127     C*
162500950127     C* ESPOSIZIONE  T O T A L E
162600950127     C                   EXSR      PULSFL
162700950127     C*
162800950127     C                   MOVEL     'T'           VH2TRC                          TIPO RECORD
162900021211     C     apdhpa        add       apdhar        vi2HLV
163000950127     C                   MOVE      CTOTAL        VI2DES
163100011113     c                   move      vh2div        VI2DES
163200950127     C                   MOVEL     W02DDC        VH2DDC                          DATA DISTIN
163300021219     c                   z-add     w01ran        vh2ran
163400030107     c                   z-add     w01TBS        vh2TBS
163500950127     C* TOTALE PADRONCINO
163600950127     C                   Z-ADD     TOTPAD        VI2IGR
163700950127     C*
163800950127     C* SALVO "TOTALE TARIFFA"     IN VI2ITT
163900950127     C                   Z-ADD     W01TOT        VI2ITT
164000950127     C*
164100950127     C* SALVO "TARIFFA GIORNATA"   IN VI2ITA
164200950127     C                   Z-ADD     W01IGR        VI2ITA
164300950127     C*
164400950127     C* SALVO "TOTALE BONUS"       IN VI2TVB
164500950127     C                   Z-ADD     W01TBN        VI2TBV                          TOT BONUS
164600950127     C* IMPORTO AGGIUNTIVO
164700950127     C                   MOVEL     CIMPOR        VI2ATT
164800950127     C                   Z-ADD     WIAD          VI2TPV
164900020318     C                   Z-ADD     WIAD          vh2IAD
165000950127     C*
165100020318     C* aggiunge all'importo addizionale eventuali altre rettifiche
165200030123     C* apportate con apposito file FIFRE02L
165300020408     C     PARSML        IFeq      ' '
165400020320     c                   eval      si_CN40='S'
165500020320     C                   clear                   ficn40ds
165600030128     C                   MOVEL     'A'           §40tip
165700020320     C                   MOVEL     'I'           §40FLG
165800020320     C                   MOVEL     VH2PDR        §40PDR
165900020320     C                   MOVEL     VH2DDC        §40DDC
166000020320     C                   MOVEL     kpjbu         savpjbu
166100020320     C                   clear                   kpjbu
166200020320     C                   MOVEL     ficn40ds      kpjbu
166300020320     C                   call      'FICN40R'
166400020320     C                   parm                    kpjba
166500020320     C                   MOVEL     kpjbu         ficn40ds
166600020320     C                   clear                   kpjbu
166700020320     C                   MOVEL     savpjbu       kpjbu
166800020320     C                   add       §40tim        VI2TPV
166900020320     C                   z-add     §40tim        vh2RET
167000020408     c                   endif
167100020318     C*
167200950127     C* PREPARA DATI X VISUALIZZARE RECROD TOTALE
167300950127     C                   EXSR      VISTOT
167400950127     C*
167500950127     C                   ADD       1             NRR
167600950127     C                   WRITE     FRC0SF2
167700950127     C                   SETOFF                                       151720
167800950406     C                   SETOFF                                       1922
167900950127     C*
168000020328     C                   clear                   comkgc
168100020328     C                   clear                   comkgr
168200020328     C                   clear                   vh2kgc
168300020328     C                   clear                   vh2kgr
168400020507     C                   clear                   vh2pge
168500020507     C                   clear                   vh2pgp
168600020328     C*
168700950127     C                   CLEAR                   W01TOT
168800950127     C                   CLEAR                   W01IGR
168900950127     C                   CLEAR                   W01TBN
169000021219     C                   clear                   w01ran
169100020322     C                   CLEAR                   WIAD             10 3
169200950127     C                   ENDSR
169300950127     C*
169400950127     C* PULIZIA SUBFILE ----------------------------------------------*
169500950127     C     PULSFL        BEGSR
169600950127     C* NON PULISCO:
169700950127     C* 1)  VH2PDR/VH2APP PERCHE' RIMANGONO SEMPRE IMPOSTATI
169800950127     C* 2)  VH2TRC/VH2FGR PERCHE' LI REIMPOSTO SEMPRE
169900950127     C                   CLEAR                   VI2FGR
170000950127     C                   CLEAR                   VI2IGR
170100950127     C                   CLEAR                   VI2ITT
170200950127     C                   CLEAR                   VI2ITA
170300950127     C                   CLEAR                   VI2TPV
170400020318     C                   clear                   vh2IAD
170500020507     C                   clear                   vh2TGP
170600020507     C                   clear                   vh2TGE
170700020318     C                   clear                   vh2RET
170800950127     C                   CLEAR                   VI2TBV
170900950127     C                   CLEAR                   VI2TOT
171000950127     C                   CLEAR                   VI2DES
171100950127     C                   CLEAR                   VH2DDC
171200950127     C                   CLEAR                   VI2SCE
171300021217     C                   clear                   vh2TBS
171400021219     C                   clear                   vh2RAN
171500130403      *
171600950127     C                   ENDSR
171700950127     C*
171800950127     C* ROTTURA PER TIPO PRESTAZIONE ---------------------------------*
171900950127     C     ROTTSR        BEGSR
172000950127     C*
172100950127    1C     W02NRR        IFEQ      0
172200950127     C                   EXSR      PULSFL
172300950127    1C                   ENDIF
172400020513     C*
172500020513    1C     W01tsr        IFeq      'R'
172600020513     c                   move      'S'           WflgR
172700020513     c                   end
172800020513    1C     W01tsr        IFeq      'C'
172900020513     c                   move      'S'           WflgC
173000020513     c                   end
173100020513     C*
173200950127     C* CALCOLO LA   S O M M A   P E S I   A   G I O R N A T A  SE C'E'
173300950127    1C     W01CPE        IFGT      0
173400950127    1C     W01KFA        ORGT      0
173500020201     C                   CLEAR                   FICNB2
173600020426     C                   MOVEL     'CONFERM'     §TApgm
173700020326     C                   MOVEL     VH2PDR        §TAfgs
173800020326     C                   MOVEL     VH2PDR        §TAPDR
173900011113     c                   movel     vh2div        §tadiv
174000950127     C                   MOVEL     W02DDC        §TADDC
174100020326     C                   z-add     w01ndc        §tandc
174200950127     C                   MOVEL     W01SNA        §TASPP
174300950127     C                   MOVEL     W01KFA        §TAKFA
174400950127     C     W01KFA        IFGT      *ZEROS
174500950127     C                   MOVEL     'S'           §TAPAG
174600950127     C                   ENDIF
174700950127     C                   Z-ADD     W01CLA        §TANCL
174800950127     C                   Z-ADD     W01CPE        §TACPE
174900950127     C                   MOVEL     'S'           §TAPPE
175000950127     C                   MOVEL     W01TVL        §TATVL
175100950127     C                   MOVEL     'IT100'       §TACAP
175200950127     C                   MOVEL     W01TSR        §TATSR
175300950127     C*
175400020320     C* Routine esterna per restituire il codice tariffa del documento
175500020320     C                   clear                   ficnb3
175600020320     C                   eval      si_B3R1 = 'S'
175700020320     C                   movel     §tapdr        §t3pdr
175800020603     C                   movel     parsml        §t3sml
175900020320     C                   movel     'V'           §t3fvl
176000020320     C                   movel     §tatsr        §t3tsr
176100020320     C                   z-add     w01ndc        §t3ndc
176200020320     C                   z-add     §taddc        §t3ddc
176300020320     C                   movel     §tadiv        §t3div
176400020320     C                   movel     *blank        §t3ctr
176500020320     C                   CALL      'FICNB3R1'
176600020320     C                   PARM                    ficnb3
176700020406      *
176800020406      *  Attenzione se non c'è la tariffa deve saltare subito alla
176900020406      *   chiamata per prestazione 999 .
177000020406      *   *-----------------------------------------------*
177100020406     c     §t3ctr        cabeq     *all'?'       a_prestaz
177200020406      *   *-----------------------------------------------*
177300020406      *
177400020320     C                   movel     §t3ctr        §TACTR
177500020403     C*
177600130318     -* ******** se questa data non è stata convalidata possono passare
177700130318     -* ********           (20)giorni prima erano (7) oltre i quali
177800130318     -* ********           NON è più possibile eseguire il conteggio.
177900130318      *** --
178000130318      ***  aggancia tabella 5aAUT di controllo giorni limite oltre i quali
178100130318      ***   NON si può andare x eseguire i Conteggi x l'Autista.
178200130318      **  x  la Ricezione Tariffa Firmata
178300130318      **     non passa la prima data poichè la UDATE da cui calcolare
178400130318      *
178500130321      * Solo se è cambiato l'autista
178600130318     c                   if        §t3sta > 0 and PARSML =' '
178700130321     c                             and §t3pdr <> §t3pdrsav
178800130404      *
178900130404      * ci entra solo se è cambiato l'autista da testare
179000130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
179100130404      ****  della Tariffa Firmata
179200130404     c                   exsr      CTRacrcf_§T3
179300130321      *
179400020403     c                   end
179500090513     C*
179600020402     c                   z-add     comkgc        §TAkgc
179700020402     c                   z-add     comkgr        §TAkgr
179800020402     C*
179900950127     C                   MOVEL     *HIVAL        §TAITA
180000950127     C                   MOVEL     ' '           §TAFLG
180100020311     C                   MOVEL     'P'           §TATSM
180200021217      * Ritiri Annullati
180300021217     C                   z-add     vh2TBS        §taTBS
180400021217     C*
180500950127     C                   MOVEL     PARSML        §TASML
180600950127     C*
180700020201     C                   CALL      'FICNB2R'
180800020201     C                   PARM                    FICNB2
180900950127     C*
181000950127    2C     §TAERR        IFEQ      ' '
181100950127     C                   ADD       §TAITT        W01ITT
181200950127     C                   ADD       §TATPE        W01ITT
181300950127     C                   ADD       §TATST        W01ITT
181400020403     C                   ADD       vh2iad        w01itt
181500950127    3C     W02NRR        IFGT      0
181600950127     C     W01ITT        SUB       VI2ITT        DIFITT
181700950127    3C                   ENDIF
181800021219     C                   ADD       vh2ran        w01ran
181900950127     C*
182000950127   X2C                   ELSE
182100950127     C*
182200950127     C* MANCA TARIFFA --> NON POSSO CONFERMARE I CONTEGGI
182300950127     C                   MOVEL     'E'           VI2SCE
182400950127     C                   MOVEL     '1'           WMTAR             1
182500950127    2C                   ENDIF
182600950127     C                   CLEAR                   W01SNA
182700950127     C                   CLEAR                   W01KFA
182800950127     C                   CLEAR                   W01TVL
182900950127     C                   CLEAR                   W01CLA
183000950127     C                   CLEAR                   W01CPE
183100950127    1C                   ENDIF
183200950127     C*
183300020406     C*  *----------------------*
183400020406     c     a_prestaz     tag
183500020406     C*  *----------------------*
183600020406     C*    occorre fare il controllo della tariffa se esiste e se è stata
183700020406     C*     convalidata .
183800020406     C*
183900950127     C* T A S S A Z I O N E   A LIVELLO DI  T I P O   P R E S T A Z
184000950127     C*
184100950127     C* SE NON E' UN RICALCOLO (W02NRR=0) ESEGUO TASSAZIONE
184200950127    1C     W02NRR        IFEQ      0
184300020201     C                   CLEAR                   FICNB2
184400950127     C*
184500020426     C                   MOVEL     'CONFERM'     §TApgm
184600020402     c                   z-add     comkgc        §TAkgc
184700020402     c                   z-add     comkgr        §TAkgr
184800020312     c                   if        w01tsr = 'G'
184900020312     c                   movel     'G'           §TATSM
185000020312     c                   else
185100020312     C                   MOVEL     'P'           §TATSM
185200020312     c                   end
185300020311     C                   MOVEL     *all'9'       §TACTR
185400020311     C*
185500950127     C                   MOVEL     W02PDR        §TAPDR
185600020326     C                   MOVEL     W02PDR        §TAfgs
185700011113     c                   movel     vh2div        §tadiv
185800950127     C                   MOVEL     W02DDC        §TADDC
185900950127     C                   MOVEL     W01SET        §TASET
186000950127     C                   MOVEL     W01SNP        §TASNP
186100950127     C                   MOVEL     W01SNT        §TASNT
186200950127     C                   MOVEL     W01SNE        §TASNE
186300950127     C                   MOVEL     W01TSR        §TATSR
186400950127     C                   MOVEL     'S'           §TAFLG
186500950127     C                   MOVEL     PARSML        §TASML
186600021217     C*
186700021217     C                   z-add     w01TBS        §taTBS
186800020408     C*
186900020408     C* Routine esterna per controllare i giorni per poter confermare
187000020408     C                   clear                   ficnb3
187100020408     C                   eval      si_B3R1 = 'S'
187200020408     C                   movel     §tapdr        §t3pdr
187300020603     C                   movel     parsml        §t3sml
187400020408     C                   movel     'V'           §t3fvl
187500020408     C                   movel     §tatsr        §t3tsr
187600020408     C                   z-add     0             §t3ndc
187700020408     C                   z-add     §taddc        §t3ddc
187800020408     C                   movel     §tadiv        §t3div
187900020408     C                   movel     §tactr        §t3ctr
188000020408     C                   CALL      'FICNB3R1'
188100020408     C                   PARM                    ficnb3
188200020507      *
188300020507      *  Attenzione c'è la tariffa ma senza data di stampa
188400020507     c     §t3ctr        ifeq      *all'?'
188500020507     c     §t3err        andeq     'E'
188600020507     C                   SETON                                        4690
188700020507     C                   MOVEL     apdPDR        VI1PDE
188800020507     C                   MOVEL     ERR(11)       VI1ERR
188900020507     c                   end
189000020408      *
189100020408      *  Attenzione se non c'è la tariffa non deve fare il controllo
189200020408      *   *-----------------------------------------------*
189300020408     c     §t3ctr        ifne      *all'?'
189400020408      *   *-----------------------------------------------*
189500130318      *** --
189600130318      ***  aggancia tabella 5aAUT di controllo giorni limite oltre i quali
189700130318      ***   NON si può andare x eseguire i Conteggi x l'Autista.
189800130318      **  x  la Ricezione Tariffa Firmata
189900130318      **     non passa la prima data poichè la UDATE da cui calcolare
190000130318      *
190100130318     c                   if        §t3sta > 0 and PARSML =' '
190200130321     c                             and §t3pdr <> §t3pdrsav
190300130404      *
190400130404      * ci entra solo se è cambiato l'autista da testare
190500130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
190600130404      ****  della Tariffa Firmata
190700130404     c                   exsr      CTRacrcf_§T3
190800130404      *
190900130321     c                   end
191000130318     c                   end
191100130318     C*
191200030326     C* TROVATA UNA DISTINTA CON FTTPEP = 'S' => IL PIK/ETICH E'
191300030326     C* DA PAGARE poichè
191400030326     C*  o di mattina o pomeriggio è stata fatta la prestazione
191500030326     C     W01PPE        IFEQ      'S'
191600030326     C                   MOVEL     'S'           §TAPPE
191700950127     C                   ELSE
191800030326     C                   MOVEL     ' '           §TAPPE
191900950127     C                   END
192000950127     C                   CLEAR                   W01PPE
192100020507      * a giornata
192200950127     C     *IN12         IFEQ      '1'
192300020507      *
192400020507     C* SE GIORNATA IMPOSTO IL FLAG
192500020507     C                   MOVEL     APDFGR        §TAFGR
192600020507      *
192700950127     C* TROVATA UNA DISTINTA CON FTTPEP = ' ' => IL PIK/ETICH NON E'
192800950127     C* DA PAGARE
192900950127     C     W02PPE        IFEQ      'N'
193000950127     C                   MOVEL     ' '           §TAPPE
193100950127     C                   ELSE
193200950127     C                   MOVEL     'S'           §TAPPE
193300950127     C                   END
193400020506      *
193500020506      * nella nuova logica di pck/eti a giornata occorre dividere
193600020506      *  il concetto fra ritiri e consegne in quanto la tariffa
193700020506      * a giornata è distinta fra pick ed etich. indipendentemente.
193800020507      * se da pagare il picking o l'etichettatura è impostato nei
193900020507      * 2 flags hidden.
194000020513     c                   if        WflgR ='S'
194100020507     C                   movel     vh2pge        §tapge
194200020513     c                   end
194300020513      *
194400020513     c                   if        WflgC ='S'
194500020507     C                   movel     vh2pgp        §tapgp
194600020513     c                   end
194700020506      *
194800950127     C                   CLEAR                   W02PPE
194900020506      *
195000950127     C                   END
195100950127     C*
195200020201     C                   CALL      'FICNB2R'
195300020201     C                   PARM                    FICNB2
195400950127     C*
195500950127     C* NON C'E' ERRORE IN TASSAZIONE
195600950127    2C     §TAERR        IFEQ      ' '
195700020318     c                   z-add     0             vh2IAD
195800021219     c                   z-add     0             vh2RAN
195900020318     c                   z-add     0             vh2RET
196000950127     C                   Z-ADD     §TATPE        VI2TPV                          PIKIN PRES
196100950127     C                   Z-ADD     §TATBN        VI2TBV                          BONUS PRES
196200950127     C*
196300950127     C* SE "G" C'E' L'IMPORTO DELLA TARIFFA A GIORNATA
196400950127     C   12              Z-ADD     §TATIG        VI2IGR                          TAR GIORN
196500020507     C   12              Z-ADD     §TATGP        vh2tgp
196600020507     C   12              Z-ADD     §TATGE        vh2tge
196700950127     C* MEMORIZZO L'IMPORTO ADDIZIONALE
196800020322     C**N12******        ADD       §TAIAD        WIAD             10 3
196900020322     C  N12              ADD       §TAIAD        w01itt
197000020403     C  N12              ADD       §taiad        vh2iad
197100021218     C  N12              ADD       §TARAN        w01itt
197200021218     C  N12              ADD       §TARAN        w01ran
197300021219     C  N12              ADD       §TARAN        vh2ran
197400020319     C*
197500020319    2C                   Else
197600020319     C* errore nella ricerca delle tariffe
197700040506     c                   if        §taTER = 'A'
197800040506     C                   MOVEL     'E'           VI2SCE
197900040506     C                   MOVEL     §taTER        WMTAR
198000040506     c                   end
198100020319     C*
198200020319    2C                   ENDIF
198300020319     C*
198400950127     C* DATA
198500950127     C                   CLEAR                   K18
198600950127     C                   MOVE      W02DDC        DATA
198700950127     C                   MOVEA     GG            K18(1)
198800950127     C                   MOVEA     '/'           K18(3)
198900950127     C                   MOVEA     MM            K18(4)
199000950127     C* DECODIFICA TIPO PRESTAZIONE
199100950127     C                   Z-ADD     1             C
199200950127     C     W01TSR        LOOKUP    T2R(C)                                 30
199300011113     C   30              MOVEA     DT2(C)        K18(7)
199400950127     C                   MOVEA     K18           VI2DES
199500950127    1C                   ENDIF
199600950127     C* DATA DISTINTA
199700950127     C                   MOVE      W02DDC        VH2DDC
199800950127     C*
199900950127     C* TIPO PRESTAZ "R" O "C"
200000950127     C     *IN12         IFEQ      *OFF
200100950127     C* "S"= TIPO RECORD DELLA PRESTAZIONE
200200950127     C                   MOVEL     'S'           VH2TRC
200300950127     C*
200400950127     C* INDICATORI DI VISUALIZZAZIONE
200500950127     C                   SETON                                        15
200600950127     C* MEMORIZZO IL TIPO PRESTAZ  IN VH2FGR
200700950127     C                   MOVEL     W01TSR        VH2FGR                          TIPO PREST.
200800950127     C*
200900950127     C                   Z-ADD     W01ITA        VI2ITA                          ACCES SPED
201000950127     C                   Z-ADD     W01ITT        VI2ITT                          BASE  SPED
201100011113      *
201200021217      * Ritiri Annullati
201300021217     C                   z-add     w01TBS        vh2TBS
201400021219     C                   z-add     w01RAN        vh2RAN
201500021217      *
201600950127     C* ESPONGO ANCHE NEI CAMPI ALFABETICI I 2
201700011113     c     vi2itt        add       vi2ita        vi2itx
201800011113     C                   move      vi2itx        fld4              4
201900011113     C                   movel     ','           fld4
202000011113     C                   movel     vi2itx        vi2att
202100011113     C                   MOVEA     VI2ATT        K08
202200011113     C                   EXSR      ALFAB
202300020320     c                   if        K08(7) = ' '
202400020320     c                   move      '0'           K08(7)
202500020320     c                   end
202600011113     c                   movea     k08           vi2att
202700011113     c                   move      fld4          vi2att
202800950127     C*
202900950127     C                   ELSE
203000950127     C*
203100950127     C* "G"= TIPO RECORD DELLA GIORNATA
203200950127     C                   MOVEL     'G'           VH2TRC
203300950127     C* INDICATORI DI VISUALIZZAZIONE
203400950127     C                   SETON                                        1715
203500950127     C* MEMORIZZO IL FLAG GIORNATA IN VH2FGR
203600950127     C                   MOVEL     §TAFGR        VI2FGR                          FLG GIORNAT
203700950127     C                   MOVEL     §TAFGR        VH2FGR                           "    "
203800950127     C                   ENDIF
203900950127     C*
204000950127     C     W02NRR        IFEQ      0
204100950127     C                   ADD       1             NRR
204200950127     C                   WRITE     FRC0SF2
204300950127     C*
204400950127     C                   ADD       W01SNP        W02SNP
204500950127     C                   ADD       W01SET        W02SET
204600950127     C                   ADD       W01SNT        W02SNT
204700950127     C                   ADD       W01SNE        W02SNE
204800950127     C*
204900950127     C*                    DA PAGARGLI
205000011207     C                   ADD       W01ITT        W01TOT           12 3
205100950127     C                   ADD       W01ITA        W01TOT
205200950127     C                   ADD       VI2TPV        W01TOT
205300950127     C* TOTALE BONUS FUORI DAL MINIMO QUINDI DAL TOTALE A VIDEO
205400950127     C                   ADD       VI2TBV        W01TBN
205500950127     C* IMPORTO TARIFFA A GIORNATA
205600011207     C   12              ADD       VI2IGR        W01IGR           10 3
205700950127     C*
205800950127     C                   ELSE
205900950127     C*
206000950127     C                   UPDATE    FRC0SF2
206100950127     C                   ENDIF
206200950127     C*
206300950127     C                   CLEAR                   W01ITA
206400950127     C                   CLEAR                   W01ITT
206500950127     C                   CLEAR                   W01SNP
206600950127     C                   CLEAR                   W01SET
206700950127     C                   CLEAR                   W01SNT
206800950127     C                   CLEAR                   W01SNE
206900030107     C                   clear                   w01TBS
207000950127     C                   SETOFF                                       151217
207100950127     C*
207200020403     C     endtsr        ENDSR
207300130404     C* --------------------------------------------------------------*
207400130404     C*   Controlla Accreditamento e Ritorno Copia Firmata da §T3
207500130404     C* --------------------------------------------------------------*
207600130404     C     CTRacrcf_§T3  BEGSR
207700130404      *
207800130404     c                   eval       §t3pdrsav = §t3pdr
207900130404      *
208000130404      * Deve controllare solo se manca la data di Ricezione Tariffa firmata
208100130404      *  o se l'Autista non è ancora stato accreditato
208200130404     c                   if            §t3cnv = 0 or Accreditato <>'S'
208300130404      *
208400130404      * ci entra solo se è cambiato l'autista da testare
208500130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
208600130404      ****  della Tariffa Firmata
208700130404     c                   if        §T3PDR <> §5Aautista
208800130404     C                             AND parSML = ' '
208900130404      *
209000130404      * ci entra solo se è cambiato l'autista da testare
209100130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
209200130404      ****  della Tariffa Firmata
209300130404      *
209400130404     c                   clear                   ficn5ads
209500130404     C                   MOVEL     kpjbu         savpjbu
209600130404     c                   movel     §T3FLR        DtgtFLR
209700130404     c                   z-add     §T3PDR        §5Aautista
209800130404     c                   z-add     §T3PRG        §5Aprgtar
209900130404      *
210000130404     c                   if               Accreditato = 'S'
210100130404     c                   eval      §5ATIPOCHK = '§5AGGVALDR'
210200130404     c                   end
210300130404      *
210400130404     c                   movel     §tgtRIS       §5ARISTAMP
210500130404     C                   z-add     §t3sta        §5ADATA2
210600130404     c                   eval      §5AWINDOW  = 'F'
210700130404     C                   clear                   kpjbu
210800130404     C                   MOVEL     ficn5ads      kpjbu
210900130404     C                   call      'FICN5AR'
211000130404     C                   parm                    kpjba
211100130404     C                   eval      ficn5ads = kpjbu
211200130404     C                   eval      ds5aAUT  = §5AUNIDS
211300130404     C                   clear                   kpjbu
211400130404     C                   MOVEL     savpjbu       kpjbu
211500130404     C*
211600130404     C*  Se non è stato ancora accreditato l'autista occorre bloccare dopo
211700130404     C*   "x" giorni --> presi nella tabella 5aAUT.
211800130404      ***    Solo se NON è stato forzato
211900130404     c                   IF        §5Aesito ='1'
212000130404      ***
212100130404     c                   IF        §5AGGDIFF  > 0   and
212200130404     c                             §5ATIPOGG  = '§5AGGVALDA'
212300130404     c                               and  accreditato = 'N'
212400130404     C                   SETON                                        4690
212500130404     C                   MOVEL     apdPDR        VI1PDE
212600130404     C                   MOVEL     ERR(14)       VI1ERR
212700130404      *** oppure
212800130404      ***   se è stata Ricevuta la tariffa FIRMATA dopo "x" giorni.
212900130404     c                   elseIF    §5AGGDIFF  > 0   and
213000130404     c                             §5ATIPOGG  = '§5AGGVALDR'
213100130404     c                             and §t3cnv = 0
213200130404     C                   SETON                                        4690
213300130404     C                   MOVEL     apdPDR        VI1PDE
213400130404     C                   MOVEL     ERR(10)       VI1ERR
213500130404     c                   endif
213600130404      ***
213700130404     c                   endif
213800130404     c                   endif
213900130404     c                   endif
214000130404     C*
214100130404     C                   ENDSR
214200130404     C* --------------------------------------------------------------*
214300950127     C* PREPARA DATI PER VISUALIZZARE RECORD TOTALE -----------------*
214400130404     C* --------------------------------------------------------------*
214500950127     C     VISTOT        BEGSR
214600020318     C                   SETON                                        171520
214700040420     C*  controllo spostato qui ....
214800090828     C                   SEToff                                       50  62
214900950127     C*
215000950127     C* TOTPAD > MINIMO --> ESPONGO IL TOTALE + BONUS
215100950127    5C     VI2IGR        IFGT      WMNT
215200950127     C     VI2IGR        ADD       VI2TBV        VI2TOT
215300011207     C  N47              ADD       VI2TPV        VI2TOT
215400950127     C                   ELSE
215500950127     C*                 --> ESPONGO MINIMO    + BONUS
215600950127     C     WMNT          ADD       VI2TBV        VI2TOT
215700011207     C  N47              ADD       VI2TPV        VI2TOT
215800950127     C*
215900950127    6C     VI2TBV        IFGT      0
216000950127     C     WMNT          ANDEQ     0
216100950127     C                   SETON                                        19
216200950127    6C                   ENDIF
216300950127    5C                   ENDIF
216400090828      *
216500090828      * Il totale non può essere 0
216600950406     C     VI2TOT        IFEQ      *ZEROS
216700950406     C                   SETON                                        22
216800950406     C                   END
216900040420      *
217000040420     C*  rimesso in modo diverso il controllo ossia se presente solo tariffe
217100040420     C*   a giornata x il padroncino quindi sia ritiri e consegne = (0)
217200040420     C*   e l'importo finale è = (0) e non ho nemmeno applicata una tariffa
217300040420     C*   allora non è possibile effettuare la conferma.
217400040420     C                   IF        VI2TOT = 0 and VI2IGR = 0 and
217500040420     C                             totCON = 0 and totRIT = 0 and
217600040420     C                             VI2TPV = 0 and flgFGR = *Blank
217700040426     c                             and parSML=' ' and Vi2sce <> 'F'
217800040420     C                   SETON                                        50
217900040420     C                   END
218000090828      *
218100090828      * Per evitare di utilizzare solo rettifiche e nessuna Tariffa
218200090828      *  ciò comporta in seguito problemi nella scrittura del FIFCE..
218300090828      * Il totale non può essere identico all'importo della rettifica
218400090828     C     VI2TOT        IFEQ      vh2RET
218500090828     C                   SETON                                        2262
218600090828     C                   END
218700090828      *
218800950127     C                   ENDSR
218900950127     C* CONTROLLO SFL2 -----------------------------------------------*
219000950127     C     CONTR2        BEGSR
219100040420     c                   clear                   totCON           10 3
219200040420     c                   clear                   totRIT           10 3
219300950127     C                   Z-ADD     1             NRR
219400040420     C                   SETOFF                                       90  47
219500950127     C                   READC     FRC0SF2                                32
219600950127     C*
219700950127    1C     *IN32         DOWEQ     *OFF
219800040426     c                   clear                   flgFGR            1
219900950127     C                   CLEAR                   WMNT
220000950127     C*
220100950127     C                   SELECT
220200950127     C* TIPO RECROD -->  P R E S T A Z I O N E
220300950127    2C     VH2TRC        WHENEQ    'S'
220400040420     c                   if        vh2fgr ='R'
220500040420     c     vi2itt        add       vi2ita        totRIT
220600040420     c                   end
220700040420     c                   if        vh2fgr ='C'
220800040420     c     vi2itt        add       vi2ita        totCON
220900040420     c                   end
221000020318     C*
221100020318    3c     vi2sce        ifeq      'R'
221200040426    3c     vi2sce        oreq      'F'
221300020318     c                   clear                   vi2sce
221400020318     c                   seton                                        15
221500020318     c                   update    frc0sf2
221600020318     c                   setoff                                       15
221700020318     c                   end
221800011113     C*
221900011113      * visualizza dettaglio importi
222000011113    3c     vi2sce        ifeq      'D'
222100011113     c                   clear                   vi2sce
222200011113     c                   exsr      WndwSfl
222300011113     c*
222400011113     c                   seton                                        15
222500011113     c                   update    frc0sf2
222600011113     c                   setoff                                       15
222700011113     c*
222800011113     c                   endif
222900011113      *
223000950127     C* MANUTENZIONE TARIFFE PADRONCINI
223100950127    3C     VI2SCE        IFEQ      'M'
223200950127     C                   MOVEL     VH2PDR        PA1PDR
223300950127     C                   MOVEL     VH2FGR        PA1TSR
223400950127     C                   MOVEL     VH2DDC        PA1DDC
223500950127     C                   MOVEL     PARSML        PA1SML
223600950127     C                   MOVEL     PARAM1        KPJBU
223700020125     C                   CALL      'FICNB1R'
223800950127     C                   PARM                    KPJBA
223900950127     C                   WRITE     FRC0D03
224000950127     C                   CLEAR                   VI2SCE
224100950127     C* RICALCOLO IL TIPO PRESTAZIONE
224200950127     C                   EXSR      CALTSR
224300950127    3C                   ENDIF
224400950127     C*
224500950127     C* TIPO RECORD -->  G I O R N A T A
224600950127    2C     VH2TRC        WHENEQ    'G'
224700040419     c                   move      vi2fgr        flgfgr
224800950127     C*
224900950127     C* VARIATO FLAG A GIORNATA
225000950127    3C     VI2FGR        IFNE      VH2FGR
225100950127    4C     VI2FGR        IFNE      ' '
225200950127     C* RICALCOLO LA TARIFFA A GIORNATA
225300020201     C                   CLEAR                   FICNB2
225400020426     C                   MOVEL     'CONFERM'     §TApgm
225500950127     C                   Z-ADD     VH2PDR        §TAPDR
225600020326     C                   movel     VH2PDR        §TAfgs
225700011113     c                   movel     vh2div        §tadiv
225800950127     C                   MOVEL     'G'           §TATSR
225900950127     C                   MOVEL     VI2FGR        §TAFGR
226000950127     C                   MOVEL     PARSML        §TASML
226100950127     C                   MOVEL     VH2DDC        §TADDC
226200950127     C*
226300950127     C                   MOVEL     'S'           §TAFLG
226400020311     C                   MOVEL     'G'           §TATSM
226500020311     C                   MOVEL     *all'9'       §TActr
226600950127     C                   MOVEL     PARSML        §TASML
226700020201     C                   CALL      'FICNB2R'
226800020201     C                   PARM                    FICNB2
226900950127     C*
227000950127    5C     §TAERR        IFEQ      ' '
227100950127     C                   Z-ADD     §TATIG        VI2IGR
227200950127   X5C                   ELSE
227300020318     c                   seton                                        51
227400950127     C                   CLEAR                   VI2IGR
227500950127    5C                   ENDIF
227600950127     C*
227700950127     C* SE TOLTO IL FLAG AZZERO IL CAMPO
227800950127   X4C                   ELSE
227900950127     C                   CLEAR                   VI2IGR
228000950127    4C                   ENDIF
228100950127     C* SALVO NUOVO FLAG
228200950127     C                   MOVEL     VI2FGR        VH2FGR
228300950127     C*
228400950127     C                   SETON                                        121715
228500950127     C                   UPDATE    FRC0SF2
228600950127     C                   SETOFF                                       121715
228700950127     C*
228800950127     C* TARIFFA GIORNATA
228900950127     C                   Z-ADD     VI2IGR        W01IGR
229000950127     C*
229100950127     C* CHAIN SUL RECORD DI TOTALE PER AGGIORNARE IL NUOVO TOTALE
229200950127     C                   ADD       1             NRR
229300950127     C     NRR           CHAIN     FRC0SF2                            30
229400950127     C* SE TROVO RECROD MINIMO SALVO CAMPO E RICHAIN
229500950127    4C     VH2TRC        IFEQ      'M'
229600950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
229700950127     C*
229800950127     C                   ADD       1             NRR
229900950127     C     NRR           CHAIN     FRC0SF2                            30
230000950127    4C                   ENDIF
230100950127     C*
230200950127    4C     *IN30         IFEQ      *OFF
230300950127     C     VH2TRC        ANDEQ     'T'
230400950127     C*
230500950127     C                   Z-ADD     W01IGR        VI2ITA                          TAR GIORNAT
230600950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                          ADD GIORNAT
230700011207     C*
230800011207     C*  nel campo aggiuntivo non posso mettere i decimali se sono in lire
230900011207     C                   move      vi2tpv        trebytes          3 0
231000011207     C                   if        vh2div ='ITL' and
231100011207     c                             trebytes > 0
231200011207     c                   seton                                        47
231300011207     c                   end
231400020319     C*
231500020319     c* se selezionato "R" di rettifica occorre attivare il SFLNXTCHG
231600020319     C*  per poter leggere il record di totale altrimenti con questo
231700020319     C*   update non lo leggerebbe più.
231800020319     C*  Richiamo programma di rettifiche con Note
231900020319     C*    da portare nell'importo aggiuntivo
232000020319     c     vi2sce        ifeq      'R'
232100020408     c     parsml        andeq     ' '
232200020320     c                   eval      si_CN40='S'
232300020319     C                   clear                   ficn40ds
232400030128     C                   MOVEL     'A'           §40tip
232500020319     C                   MOVEL     'C'           §40FLG
232600020319     C                   MOVEL     VH2PDR        §40PDR
232700020319     C                   MOVEL     VH2DDC        §40DDC
232800020320     C                   MOVEL     kpjbu         savpjbu
232900020320     C                   clear                   kpjbu
233000020320     C                   MOVEL     ficn40ds      kpjbu
233100020319     C                   call      'FICN40R'
233200020319     C                   parm                    kpjba
233300020319     C                   MOVEL     kpjbu         ficn40ds
233400020320     C                   clear                   kpjbu
233500020320     C                   MOVEL     savpjbu       kpjbu
233600020319     C                   z-add     §40tim        vh2RET
233700020319     C                   z-add     §40tim        VI2TPV
233800020319     C                   add       vh2IAD        VI2TPV
233900020319     C                   clear                   VI2SCE
234000020319     c                   end
234100011207      *
234200950127     C* PREPARA ALTRI DATI DA VISUALIZZARE
234300011207     C                   EXSR      VISTOT
234400020319     C*
234500950127     C                   UPDATE    FRC0SF2
234600950127     C                   SETOFF                                       171520
234700950406     C                   SETOFF                                       1922
234800950127     C*
234900950127    4C                   ENDIF
235000950127    3C                   ENDIF
235100950127     C*
235200950127     C* TIPO RECORD -->  T O T A L E
235300950127    2C     VH2TRC        WHENEQ    'T'
235400950127     C*
235500020318     C*  Richiamo programma di rettifiche con Note
235600020318     C*    da portare nell'importo aggiuntivo
235700020318    3C     vi2sce        IFEQ      'R'
235800020408    3C     parsml        andeq     ' '
235900020320     c                   eval      si_CN40='S'
236000020318     C                   clear                   ficn40ds
236100030128     C                   MOVEL     'A'           §40tip
236200020318     C                   MOVEL     'C'           §40FLG
236300020318     C                   MOVEL     VH2PDR        §40PDR
236400020318     C                   MOVEL     VH2DDC        §40DDC
236500121016     C                   MOVEL     kpjbu         savpjbu
236600121016     C                   clear                   kpjbu
236700020318     C                   MOVEL     ficn40ds      kpjbu
236800020318     C                   call      'FICN40R'
236900020318     C                   parm                    kpjba
237000020318     C                   MOVEL     kpjbu         ficn40ds
237100121016     C                   clear                   kpjbu
237200121016     C                   eval      kpjbu = savpjbu
237300020318     C                   z-add     §40tim        vh2RET
237400020318     C                   z-add     §40tim        VI2TPV
237500020318     C                   add       vh2IAD        VI2TPV
237600020318     C                   clear                   VI2SCE
237700020318     C                   UPDATE    FRC0SF2
237800020318     C                   WRITE     FRC0D03
237900020318    3C                   end
238000020318     C*
238100950127     C* CHAIN SUL RECORD DEL MINIMO
238200950127     C                   SUB       1             NRR
238300950127     C     NRR           CHAIN     FRC0SF2                            30
238400950127     C* SE NON TROVATO RECORD O NON E' 'T' RICHAIN
238500950127    3C     VH2TRC        IFEQ      'M'
238600950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
238700950127    3C                   ENDIF
238800950127     C*
238900950127     C                   ADD       1             NRR
239000950127     C     NRR           CHAIN     FRC0SF2                            30
239100950127     C*
239200950127    3C     *IN30         IFEQ      *OFF
239300950127     C     VH2TRC        ANDEQ     'T'
239400950127     C*
239500950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                          ADD GIORNAT
239600950127     C*
239700011207     C*  nel campo aggiuntivo non posso mettere i decimali se sono in lire
239800011207     C                   move      vi2tpv        trebytes          3 0
239900011207     C                   if        vh2div ='ITL' and
240000011207     c                             trebytes > 0
240100011207     c                   seton                                        47
240200011207     c                   end
240300011207      *
240400011207     C                   EXSR      VISTOT
240500040419     C*
240600950127     C                   UPDATE    FRC0SF2
240700950127     C                   SETOFF                                       171520
240800950406     C                   SETOFF                                       1922
240900950127     C*
241000950127    3C                   ENDIF
241100950127    2C                   ENDSL
241200950127     C*
241300950127     C                   READC     FRC0SF2                                32
241400950127    1C                   ENDDO
241500950127     C*
241600950127     C                   ENDSR
241700950127     C*
241800950127     C* RICALCOLO TIPO PRESTAZ DI CUI SONO ANDATA IN MODIFICA ------*
241900950127     C     CALTSR        BEGSR
242000950127     C                   CLEAR                   W01ITT
242100950127     C                   CLEAR                   W01ITA
242200950127     C* FLAG VALORIZZAZIONE
242300950127     C                   MOVEL     'V'           W02FVL
242400950127     C* TIPO PRESTAZIONE
242500950127     C                   Z-ADD     1             C
242600950127     C     PA1TSR        LOOKUP    TSR(C)                                 30
242700950127     C  N30              Z-ADD     1             C
242800950127     C* DATA
242900950127     C                   MOVEL     PA1DDC        W02DDC
243000950127     C                   MOVEL     PA1TSR        W01TSR
243100950127     C*
243200011113     C     KFTT3         SETLL     FIFTT002
243300011113     C     KFTT3         READE(N)  FIFTT002                               30
243400950127     C*
243500950127    1C     *IN30         DOWEQ     *OFF
243600950127     C* SOMMA PESI A GIORNATA
243700950127    2C     FTTCPE        IFGT      0
243800950127     C                   Z-ADD     FTTNDC        W01NDC
243900950127     C                   ADD       FTTSNA        W01SNA
244000950127     C                   ADD       FTTKFA        W01KFA
244100950127     C                   ADD       FTTTVL        W01TVL
244200950127     C* Sommo tot.colli solo se pag.pik/etich.='S'
244300950127     C     FTTPEP        IFEQ      'S'
244400950127     C                   ADD       FTTCLA        W01CLA
244500950127     C                   ADD       FTTCPE        W01CPE
244600950127     C                   END
244700950127    2C                   ENDIF
244800950127     C*
244900950127     C                   ADD       FTTITT        W01ITT
245000950127     C                   ADD       FTTITA        W01ITA
245100950127     C*
245200011113     C     KFTT3         READE(N)  FIFTT002                               30
245300950127    1C                   ENDDO
245400950127     C*
245500950127     C* LA DIFFERENZA RISCONTRATA: SE 0 NON FACCIO NULLA
245600950127     C     W01ITT        SUB       VI2ITT        DIFITT
245700950127     C     W01ITA        SUB       VI2ITA        DIFITA
245800950127     C*
245900950127    1C     DIFITT        IFNE      0
246000950127     C     DIFITA        ORNE      0
246100950127     C                   Z-ADD     NRR           W02NRR
246200950127     C*
246300950127     C                   EXSR      ROTTSR
246400950127     C*
246500950127     C                   CLEAR                   WMNT
246600950127     C                   ADD       1             NRR
246700950127    2C     NRR           DOWLE     W01NRR
246800950127     C     VH2PDR        ANDEQ     PA1PDR
246900950127     C     NRR           CHAIN     FRC0SF2                            30
247000950127     C*
247100950127    3C     *IN30         IFEQ      *OFF
247200950127     C*
247300950127     C* RECORD DI TOTALE
247400950127    4C     VH2TRC        IFEQ      'T'
247500950127     C                   ADD       DIFITT        VI2ITT                         BASE+ACC+PIK
247600950127     C                   ADD       DIFITA        VI2ITT
247700950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                         ADD GIORNAT
247800950127     C*
247900950127     C                   EXSR      VISTOT
248000950127     C*
248100950127     C                   UPDATE    FRC0SF2
248200950127     C                   SETOFF                                       171520
248300950406     C                   SETOFF                                       1922
248400950127     C                   Z-ADD     W01NRR        NRR
248500950127     C*
248600950127   X4C                   ELSE
248700950127     C*
248800950127    5C     VH2TRC        IFEQ      'M'
248900950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
249000950127    5C                   ENDIF
249100950127    4C                   ENDIF
249200950127    3C                   ENDIF
249300950127     C                   ADD       1             NRR
249400950127    2C                   ENDDO
249500950127     C*
249600950127   X1C                   ELSE
249700950127     C                   SETON                                        15
249800950127     C                   UPDATE    FRC0SF2
249900950127     C                   SETOFF                                       15
250000950127    1C                   ENDIF
250100950127     C*
250200950127     C                   ENDSR
250300950127     C*
250400950127     C* CONFERMA VALORIZZAZIONE -------------------------------------*
250500950127     C     CONVAL        BEGSR
250600020329     C*
250700950127     C* PULIZIA CAMPI DI TOTALE
250800950127     C                   CLEAR                   W01SET
250900950127     C                   CLEAR                   W01SNT
251000950127     C                   CLEAR                   W01SNP
251100950127     C                   CLEAR                   W01SNE
251200950127     C                   CLEAR                   W01ITT
251300950127     C                   CLEAR                   W01ITA
251400950127     C                   CLEAR                   W01TPE
251500021218     C                   clear                   w01TBS
251600021219     C                   clear                   w01RAN
251700950127     C                   CLEAR                   WMNT
251800020329     C                   CLEAR                   WGIO
251900020329     C                   CLEAR                   Wret
252000020329     C                   CLEAR                   WTMI              1
252100950615     C                   Z-ADD     1             WW                1 0
252200950127     C*
252300950127     C                   Z-ADD     1             NRR
252400950127    1C     NRR           DOWLE     W01NRR
252500950127     C*
252600950127     C     NRR           CHAIN     FRC0SF2                            30
252700950127     C*
252800950127    2C     *IN30         IFEQ      *OFF
252900950127     C*
253000950127    3C                   SELECT
253100950127     C*
253200011113     C* RECORD TOTALE --> SCRIVO NUOVO RECORD IN FIFTT
253300950127     C     VH2TRC        WHENEQ    'T'
253400950127     C*
253500950616     C* Verifico se è possibile chiudere le distinte/ritiri del padronc
253600950615     C                   EXSR      CTRCHI
253700950615     C     *IN90         IFEQ      '0'
253800950615     C*
253900950127     C* CHIUDO LE DISTINTE/RITIRI DEL PADRONCINO
254000950127     C                   EXSR      CHIUDC
254100950615     C*
254200011113     C                   CLEAR                   FIFTT002
254300950127     C                   MOVEL     VH2PDR        FTTPDR
254400020424     C                   MOVEL     VH2PDR        FTTfgs
254500011113     c                   movel     vh2div        fttdiv
254600950127     C                   MOVEL     VH2DDC        FTTDDC
254700950127     C                   MOVEL     'C'           FTTFVL
254800950127     C                   MOVEL     W01SET        FTTSET
254900950127     C                   MOVEL     W01SNT        FTTSNT
255000950127     C                   MOVEL     W01SNP        FTTSNP
255100950127     C                   MOVEL     W01SNE        FTTSNE
255200950127     C     VI2ITA        ADD       W01ITT        FTTITT
255300000307     C                   Z-ADD     W01ITA        FTTITA
255400950127     C                   Z-ADD     W01TPE        FTTTPE
255500950127     C                   Z-ADD     VI2TBV        FTTTBN
255600020318     C                   Z-ADD     vh2RET        FTTTIM
255700020318     C                   ADD       vh2IAD        FTTITT
255800020328     C                   move      *zero         fttcdf
255900020328     C                   move      vh2cdf        fttcdf
256000020328     C                   move      vh2soc        fttsoc
256100020329     C                   z-add     vi2tpv        wret
256200021211     C                   z-add     vi2hlv        ftthlv
256300030107      *
256400030107      * Memorizza il valore dei ritiri annullati
256500030107     C     vh2ran        mult      1000          FttPDU
256600081008     c                   z-add     vh2ran        fttvRAN
256700030107ab   C                   z-add     w01tbs        fttTBS
256800020507      *
256900950127     C* SE IL TOTALE E' < DEL MINIMO METTO IL DELTA PER ARRIVARE AL MIN
257000950127     C     VI2IGR        IFLT      WMNT
257100950127     C     WMNT          SUB       VI2IGR        FTTMNT
257200950127     C                   ENDIF
257300950127     C                   Z-ADD     DATEU8        FTTDCV
257400081008      *
257500081008      * richiamo routine per verifica se esiste sovrapprezzo carburante
257600081008     c                   exsr      caricofuel
257700081008     c                   if        o74err = *blank
257800081008     c                   z-add     o74impco      fttpcar
257900081008     c                   z-add     o74mcp        fttmcp
258000081008     c                   endif
258100950127     C*
258200011113     C                   WRITE     FIFTT002                             09
258300060411     C*
258400950531     C* Se errore scrittura invio messaggio alla sede
258500951025     C     *IN09         IFEQ      *ON
258600100906     C                   MOVEL     MSG(1)        MSGTES           55
258700100906     C                   MOVEL(P)  MSG(2)        MSGDET          198
258800950531     C                   MOVE      FTTPDR        MSPDR
258900950531     C                   MOVE      FTTDDC        MSDDC
259000100906      *
259100100906      * invia segnalazione via mail
259200100906     c                   eval      emlto = 'CedAlert@bartolini.it'
259300100906     c                   eval      emlcc = *blank
259400100906     c                   eval      emogg = 'ERRORE conferma Valorizza-
259500100906     c                             zioni AUTISTI CITTÀ'
259600100906     C                   eval      emmsg = msgtes + ' - ' + msgdet
259700100906     c
259800100906     C                   CALL      'TIS701C1'
259900100906     C                   PARM                    emlto           253
260000100906     C                   PARM                    emlcc           253
260100100906     C                   PARM                    emogg            44
260200100906     C                   PARM                    emmsg          5000
260300100906     C                   PARM                    emesito           1
260400950531     C                   END
260500040426     **
260600040426     ** Scrive forzatamente l'FIFCE con le righe a zero se il valore
260700040426     **  della giornata doveva essere nullo
260800040426     c                   if        PARSML=*blank
260900040426     C                   IF        VI2TOT = 0 and VI2IGR = 0 and
261000040426     C                             totCON = 0 and totRIT = 0 and
261100040426     C                             VI2TPV = 0 and flgFGR = *Blank and
261200040426     c                             Vi2sce = 'F'
261300040426     c                   exsr      wrt0_Voci
261400040426     C                   End
261500040426     C                   End
261600950127     C*
261700020329     C* aggiunge informazioni sulla Gionrata al Workfile che poi servirà
261800020329     C*  per attribuire le voci di conto economico secondo i pesi fra
261900020329     C*   le spedizioni dei ritiri e delle consegne.
262000020605     c                   if        PARSML=*blank
262100020329     C                   EXSR      ADD_FIFCW
262200020605     c                   end
262300020329     C*
262400950127     C                   CLEAR                   W01SET
262500950127     C                   CLEAR                   W01SNT
262600950127     C                   CLEAR                   W01SNP
262700950127     C                   CLEAR                   W01SNE
262800950127     C                   CLEAR                   W01ITT
262900950127     C                   CLEAR                   W01ITA
263000950127     C                   CLEAR                   W01TPE
263100021218     C                   clear                   w01TBS
263200021219     C                   clear                   w01RAN
263300950127     C                   CLEAR                   WMNT
263400020329     C                   CLEAR                   WGIO
263500020329     C                   CLEAR                   WTMI
263600020329     C                   CLEAR                   Wret
263700950127     C*
263800950406     C* RICHIAMO PGM DI STAMPA DEL PADRONCINO
263900950127     C                   MOVE      VH2PDR        DD0PDR
264000950127     C                   MOVEL     ' '           DD0TSR
264100950127     C                   MOVEL     PARSML        DD0SIM
264200950127     C                   MOVEL     VH2DDC        DD0DDT
264300951025     C* Se non è simulazione e errore in conferma lo segnalo al pgm di
264400951025     C* stampa affinchè gli sia possibile stampare un messaggio di
264500951025     C* dati non corretti.
264600951025     C     PARSML        IFEQ      ' '
264700951025     C     *IN09         ANDEQ     *ON
264800951025     C                   MOVE      'S'           DD0ERR
264900951025     C                   ELSE
265000951025     C                   CLEAR                   DD0ERR
265100951025     C                   END
265200020315     C                   MOVEL     FICNB6        KPJBU
265300950127     C* STAMPA DISTINTA DI CONSEGNA EFFETTIVA
265400950127     C     PARSML        IFEQ      ' '
265500020315     C                   CALL      'FICNB6R'
265600950406     C                   PARM                    KPJBA
265700950127     C                   ELSE
265800950127     C* STAMPA DISTINTA DI CONSEGNA DI SIMULAZIONE
265900020315     C                   CALL      'FICNB7C'
266000950406     C                   PARM                    KPJBA
266100950127     C                   ENDIF
266200950127     C*
266300950127     C* SOTTOMETTO LA SVALORIZZAZIONE DEI RECORD DI SIMULAZIONE
266400950127     C     VIDSIM        IFEQ      'SI'
266500950127     C                   MOVEL     VH2PDR        PA2PDR
266600950127     C                   MOVEL     ' '           PA2TSR
266700950127     C                   MOVEL     VH2DDC        PA2DDC
266800950127     C                   MOVEL     PARAM2        KPJBU
266900011115     c     knmus         ifeq      *all'1'
267000011115     c                   call      'FICND9R'
267100011115     c                   parm                    kpjba
267200011115     c                   else
267300011116     C                   MOVEL     'FND9'        KCOAZ
267400950127     C                   CALL      'BCH10'
267500950127     C                   PARM                    KPJBA
267600011115     c                   end
267700950127     C                   ENDIF
267800950615     C*
267900060411      *  impostato a livello di singolo Padroncino data
268000060411     c                   commit
268100060411     C*
268200950615     C                   END
268300950127     C*
268400950127   X3C                   OTHER
268500020503     C
268600020503     C* A Giornata    --> MI SALVO IL VALORE e il tipo Intera o Mezza
268700020503     C     VH2TRC        ifeq      'G'
268800020503     c                   if        vi2igr > 0
268900020503     C                   z-add     vi2igr        Wgio
269000020503     C                   move      vi2fgr        Wtmi
269100020503     c                   end
269200020503     c                   end
269300020503     C*
269400020503     C* RECORD MINIMO --> MI SALVO IL VALORE
269500020503     C     VH2TRC        ifeq      'M'
269600020503     C                   Z-ADD     VI2IGR        WMNT
269700020503     c                   end
269800950127     C*
269900950127     C* ESCLUDO RECORD PADRONCINO
270000950127    4C     VH2TRC        IFNE      'P'
270100950127     C                   ADD       VI2ITT        W01ITT
270200950127     C                   ADD       VI2ITA        W01ITA
270300950127     C                   ADD       VI2TPV        W01TPE
270400030107ab   C                   add       vh2tbs        w01tbs
270500950127    4C                   ENDIF
270600950127    3C                   ENDSL
270700950127    2C                   ENDIF
270800950127     C*
270900950127     C                   ADD       1             NRR
271000950127     C                   ENDDO
271100020329     C*
271200020329     C                   ENDSR
271300020329     C*--------------------------------------------------------------*
271400020329     C* Aggiunge informazioni a livello di giornata sul WorkFile     *
271500020329     C*--------------------------------------------------------------*
271600020329     C     ADD_FIFCW     BEGSR
271700950127     C*
271800020329     C* scrive un record TSR='G' per tariffa TIG o TMG senza voce
271900020329     C*  da ripartire
272000020329     c                   if        WGIO >0
272100020329     C                   clear                   fifcW000
272200020329     C                   movel     vh2pdr        fcWFGS
272300020329     C                   z-add     vh2pdr        fcWPDR
272400020329     C                   move      'G'           fcWTSR
272500020329     C                   z-add     vh2ddc        fcWDDC
272600020329     C                   z-add     1             fcWnDC
272700020329     C                   z-add     Wgio          fcWice
272800020329     c                   if        Wtmi = 'I'
272900020329     c                   MOVEL     'TIG'         fcWt8a
273000020329     c                   else
273100020329     c                   MOVEL     'TMG'         fcWt8a
273200020329     c                   end
273300020329     C                   z-add     vh2kgc        fcWKGC
273400020329     C                   z-add     vh2kgr        fcWKGR
273500020329     C                   write     fifcW000
273600020329     c                   end
273700020329     C*
273800020329     C* scrive un record TSR='G' per il delta al Minimo Garantito
273900020329     C*  con tariffa MGA da ripartire.
274000020329     c                   if        Vi2igr < Wmnt
274100020329     C                   clear                   fifcW000
274200020329     C                   movel     vh2pdr        fcWFGS
274300020329     C                   z-add     vh2pdr        fcWPDR
274400020329     C                   move      'G'           fcWTSR
274500020329     C                   z-add     vh2ddc        fcWDDC
274600020329     C                   z-add     2             fcWnDC
274700020329     C     Wmnt          sub       vi2igr        fcWice
274800020329     c                   MOVEL     'MGA'         fcWt8a
274900020329     C                   z-add     vh2kgc        fcWKGC
275000020329     C                   z-add     vh2kgr        fcWKGR
275100020329     C                   write     fifcW000
275200020329     c                   end
275300020329      * Rettifiche  --------------------
275400020423     c                   if        Wret <>0
275500020329      *
275600020329     c                   z-add     vh2pdr        frepdr
275700020329     c                   z-add     vh2ddc        freddc
275800030123     C     Kfre          setll     fifre02l
275900030123     C     Kfre          reade     fifre02l
276000020329      * ------1->
276100030123     c                   dow       not %eof(fifre02l)
276200020329      *
276300020329     c                   clear                   fifcw001
276400020329     C                   movel     vh2pdr        fcWFGS
276500020329     C                   z-add     vh2pdr        fcWPDR
276600020329     C                   z-add     vh2ddc        fcWDDC
276700020329     C                   movel(P)  'CRE'         tbeCOD
276800020329     C                   movel(P)  freCRE        tbeKE1
276900020329     C                   movel(P)  *blank        tbeKE2
277000020329     C     KTbe          chain     tntbe01l
277100020329      * consegne/ritiri
277200020329     c                   if        %found(tntbe01l)
277300020516     C                   movel     tbeuni        dCRE
277400020329     C                   movel     §CRtsr        fcWTSR
277500020402     c                   move      §CRvoc        fcWcce
277600050208     C                   z-add     10            fcWnDC
277700020329      *
277800020329     C     Kfcw1         chain     fifceW1l
277900020329      *
278000020329     c                   if        %found(fifceW1l)
278100020329      *
278200020329     C*  somma algebrica del valore
278300020329     C                   add       FreTim        fcwice
278400020329     C                   update    fifcW001
278500020329     C*
278600020329     c                   else
278700020329     C*
278800020329     C                   clear                   fifcW001
278900020329     C                   movel     vh2pdr        fcWFGS
279000020329     C                   z-add     vh2pdr        fcWPDR
279100020329     C                   z-add     vh2ddc        fcWDDC
279200050208     C                   z-add     10            fcWnDC
279300020617     c                   MOVEL     'RET'         fcWt8a
279400020329      *
279500020329     C                   movel     §CRtsr        fcWTSR
279600020402     c                   move      §CRvoc        fcWcce
279700020329      *
279800020329     C*  somma algebrica del valore
279900020402     C                   z-add     vh2kgc        fcwkgc
280000020402     C                   z-add     vh2kgr        fcwkgr
280100020402     C                   z-add     freTim        fcWice
280200020329     C                   write     fifcW001
280300020329     c                   end
280400020329     C*
280500020329     c                   endIF                                                  %found
280600020329      *
280700030123     C     Kfre          reade     fifre02l
280800020329     c                   enddo
280900020329      * ------1->
281000020329     c                   end
281100020329     C*
281200950127     C                   ENDSR
281300020329     C*--------------------------------------------------------------*
281400950127     C* CHIUDO LE DISTINTE/RITIR DEL PADRONCINO ---------------------*
281500950127     C     CHIUDC        BEGSR
281600950127     C                   MOVEL     VH2DDC        W02DDC
281700950127     C                   MOVEL     'V'           W02FVL
281800950127     C                   Z-ADD     1             C
281900950127     C     TSR(C)        DOWNE     ' '
282000011113     C     KFTT3         SETLL     FIFTT002
282100011113     C     KFTT3         READE     FIFTT002                               30
282200950127     C*
282300950127     C     *IN30         DOWEQ     *OFF
282400950127     C                   ADD       FTTSET        W01SET
282500950127     C                   ADD       FTTSNT        W01SNT
282600950127     C                   ADD       FTTSNP        W01SNP
282700950127     C                   ADD       FTTSNE        W01SNE
282800020328     C                   move      *zero         fttcdf
282900020328     C                   move      vh2cdf        fttcdf
283000020328     C                   move      vh2soc        fttsoc
283100950127     C                   MOVEL     'C'           FTTFVL
283200950127     C                   MOVEL     DATEU8        FTTDCV
283300020422     C                   MOVEL     *blank        FTTftr
283400011113     C                   UPDATE    FIFTT002
283500970924     C* elaborazione stop padroncino per trasmissione a sede e filiale
283600950127     C     PARSML        IFEQ      ' '
283700950127     C                   EXSR      SRFTDS
283800950127     C                   END
283900950127     C*
284000011113     C     KFTT3         READE     FIFTT002                               30
284100950127     C                   ENDDO
284200950127     C*
284300950127     C                   ADD       1             C
284400950127     C                   ENDDO
284500950615     C                   ENDSR
284600950615     C*****************************************************************
284700950616     C* VERIFICA POSSIBILITA' DI CHIUDERE LE DISTINTE/RITIRI DEL PADR.
284800950615     C*****************************************************************
284900950615     C     CTRCHI        BEGSR
285000950615     C*
285100950615     C                   MOVEL     VH2DDC        W02DDC
285200950616     C                   MOVE      VH2DDC        FTTDDC
285300950615     C*
285400950615     C                   EXSR      CTRDIS
285500950616     C* Trovato errore: memorizzo fino a 6 errori
285600950615     C     *IN90         IFEQ      '1'
285700950615     C     WW            IFLE      5
285800950615     C                   SELECT
285900950615     C     WW            WHENEQ    1
286000950615     C                   SETON                                        56
286100950615     C                   MOVEL     VI1ERR        VI6ERR
286200950615     C                   MOVE      VI1DDE        VI6DDE
286300950615     C                   MOVE      VI1PDE        VI6PDE
286400950615     C     WW            WHENEQ    2
286500950615     C                   SETON                                        55
286600950615     C                   MOVEL     VI1ERR        VI5ERR
286700950615     C                   MOVE      VI1DDE        VI5DDE
286800950615     C                   MOVE      VI1PDE        VI5PDE
286900950615     C     WW            WHENEQ    3
287000950615     C                   SETON                                        54
287100950615     C                   MOVEL     VI1ERR        VI4ERR
287200950615     C                   MOVE      VI1DDE        VI4DDE
287300950615     C                   MOVE      VI1PDE        VI4PDE
287400950615     C     WW            WHENEQ    4
287500950615     C                   SETON                                        53
287600950615     C                   MOVEL     VI1ERR        VI3ERR
287700950615     C                   MOVE      VI1DDE        VI3DDE
287800950615     C                   MOVE      VI1PDE        VI3PDE
287900950615     C     WW            WHENEQ    5
288000950615     C                   SETON                                        52
288100950615     C                   MOVEL     VI1ERR        VI2ERR
288200950615     C                   MOVE      VI1DDE        VI2DDE
288300950615     C                   MOVE      VI1PDE        VI2PDE
288400950615     C                   ENDSL
288500950615     C                   ADD       1             WW
288600950615     C                   SETOFF                                       45
288700950615     C                   END
288800950615     C                   CLEAR                   W01SET
288900950615     C                   CLEAR                   W01SNT
289000950615     C                   CLEAR                   W01SNP
289100950615     C                   CLEAR                   W01SNE
289200950615     C                   CLEAR                   W01ITT
289300950615     C                   CLEAR                   W01ITA
289400950615     C                   CLEAR                   W01TPE
289500021218     C                   clear                   w01TBS
289600021219     C                   clear                   w01RAN
289700950615     C                   CLEAR                   WMNT
289800950616     C                   END
289900950615     C*
290000950616     C                   ENDSR
290100950127      **************************************************************************
290200950127     C* TRASMISSIONE NUMERI STOP IN SEDE
290300950127      **************************************************************************
290400950127     C     SRFTDS        BEGSR
290500970923     C                   CLEAR                   SAVSET
290600970415     C                   Z-ADD     1             K
290700970923     C                   CLEAR                   SAVSTP
290800970924     C                   CLEAR                   SAVSTO
290900970930     C                   CLEAR                   COMSET
291000970923     C                   CLEAR                   W01STP
291100970923     C* P O S I Z I O N A M E N T O:
291200970922     C* DISTINTA DI CONSEGNA: considero solo i records con n. stop>0
291300970924    1C     FTTTSR        IFEQ      'C'
291400011113     C     KFTDS         SETGT     FIFTD000
291500950411     C                   ELSE
291600011113     C* DISTINTA DI RITIRO: visto che ad un record di FIFTD possono
291700011113     C* corrispondere più spedizioni considero anche i redords di FIFTD
291800950411     C* con numero stop = 0.
291900011113     C     KFTDS         SETLL     FIFTD000
292000970924    1C                   END
292100970922     C* L E T T U R A :
292200011113     C     KFTDR         READE     FIFTD000                               30
292300950127     C*
292400950221    1C     *IN30         DOWEQ     *OFF
292500950127     C*
292600970922     C* CONSEGNA
292700020314      *  o nuovi ritiri
292800950221    2C     FTDTSR        IFEQ      'C'
292900020314      * per gestire il nuovo modo di scrivere i ritiri con le spedizioni
293000020314     c     ftdtsr        oreq      'R'
293100020314     c     ftdnsp        andgt     0
293200020314      *
293300970923     C* A rottura numero stop memorizzo numero spedizioni per stop
293400970923     C* scrivendo il record di trasmissione
293500970924    3C     FTDSTP        IFNE      SAVSTO
293600970924    4C     SAVSTO        IFGT      0
293700970924     C                   Z-ADD     SAVSTO        W01STP
293800011113     C     KFTDS         SETLL     FIFTD000
293900020419      *
294000970923     C                   EXSR      WRTSTP
294100020419      *
294200970923    4C                   END
294300020419      *
294400970924     C                   Z-ADD     FTDSTP        SAVSTO
294500020419     C                   Z-ADD     ftdksc        savksc
294600020419     C                   MOVEL     ftdrsc        savRSC
294700970930     C                   MOVE      *ZEROS        COMSET
294800020419     C                   MOVE      *ZEROS        sv1SET
294900970923    3C                   END
295000970923     C* Controllo se record da elaborare
295100020419    2C     FTDTSR        ifeq      'R'
295200020419     C                   ADD       1             COMSET
295300020419     C                   ADD       ftdset        sv1SET
295400020419    3C                   else
295500020419      * x consegne
295600970923     C                   EXSR      CHKFTD
295700970923    3C     WELAB         IFEQ      'S'
295800970930     C                   ADD       1             COMSET
295900970923    3C                   END
296000020419    3C                   endif
296100020419      *
296200950221   X2C                   ELSE
296300950127     C* RITIRO: PRENDO DATI DA BOLLE PARTENZE
296400950411     C* Escludo se si tratta di un ritiro di una sola spedizione con
296500950411     C* numero stop = 0
296600950221    3C     FTDTSR        IFEQ      'R'
296700950411     C     FTDSTP        ANDGT     0
296800950411     C     FTDTSR        OREQ      'R'
296900950411     C     FTDSTP        ANDEQ     0
297000950411     C     FTDSET        ANDGT     1
297100970924     C* Scrittura records di trasmissione a rottura numero stop per
297200970924     C* records con numero stop > 0
297300970924    4C     FTDSTP        IFNE      SAVSTO
297400970924    5C     SAVSTO        IFGT      *ZEROS
297500970924     C                   Z-ADD     SAVSTO        W01STP
297600011113     C     KFTDS         SETLL     FIFTD000
297700011113     C     KFTDR         READE     FIFTD000                               30
297800970924    6C     *IN30         DOWEQ     *OFF
297900970924     C     FTDSTP        ANDEQ     W01STP
298000970924     C                   EXSR      ELABLP
298100011113     C     KFTDR         READE     FIFTD000                               30
298200970924    6C                   ENDDO
298300970930     C                   MOVE      *ZEROS        COMSET
298400970924    5C                   END
298500970924     C                   Z-ADD     FTDSTP        SAVSTO
298600970924    4C                   END
298700970924     C* SE NUMERO DI STOP = 0 POSSO GIA SCRIVERE I RECORDS DI TRASMISS.
298800970924    4C     FTDSTP        IFEQ      *ZEROS
298900970924     C                   EXSR      ELABLP
299000970924   X4C                   ELSE
299100970924     C* ALTRIMENTI MEMORIZZO IL NUMERO DELLE SPEDIZIONI
299200970930     C                   ADD       FTDSET        COMSET
299300970924    4C                   END
299400950801     C*
299500950221    3C                   END
299600970415     C* SCRIVO IL NUMERO DEI RITIRI PER IL DELTA
299700970415     C                   EXSR      RITDEL
299800950221    2C                   END
299900950127     C*
300000011113     C     KFTDR         READE     FIFTD000                               30
300100950221    1C                   ENDDO
300200970923     C*
300300970923     C* MEMORIZZAZIONE DELL'ULTIMO STOP DI CONSEGNA A FINE LETTURA
300400970924    1C     SAVSTO        IFGT      0
300500970924     C                   Z-ADD     SAVSTO        W01STP
300600011113     C     KFTDS         SETLL     FIFTD000
300700020314      *
300800020314     C* CONSEGNA
300900020314      *  o nuovi ritiri
301000970924    2C     FTTTSR        IFEQ      'C'
301100020314      * per gestire il nuovo modo di scrivere i ritiri con le spedizioni
301200020314     c     ftdtsr        oreq      'R'
301300020314     c     ftdnsp        andgt     0
301400020314      *
301500970923     C                   EXSR      WRTSTP
301600020314      *
301700970924   X2C                   ELSE
301800020419      *
301900970923     C* MEMORIZZAZIONE DEGLI STOP DI RITIRO
302000011113     C     KFTDR         READE     FIFTD000                               30
302100970924    3C     *IN30         DOWEQ     *OFF
302200970924     C     FTDSTP        ANDEQ     W01STP
302300970924     C                   EXSR      ELABLP
302400011113     C     KFTDR         READE     FIFTD000                               30
302500970924    3C                   ENDDO
302600970924    2C                   END
302700970415     C*
302800970415     C* SE DEVO MEMORIZZARE L'ULTIMO STOP LO FACCIO ALLA FINE DELLA
302900970415     C*  LETTURA
303000970924    1C     SAVSTP        IFGT      0
303100970415     C                   EXSR      WRISBP
303200970924    1C                   ENDIF
303300020419     C*
303400020419    1C                   END
303500020419     C*
303600950127     C                   ENDSR
303700030321      **************************************************************************
303800030321     C* controlla se ritiro o consegna per recuperare codice bolla x fcecbo
303900030321      **************************************************************************
304000030321     C     Cod_bolla     BEGSR
304100030321     c                   clear                   wfcecbo           2
304200030321     C*
304300030321     C* Verifico se è un ritiro
304400030321     c                   if        fcetsr ='R'
304500030321     C     KARB          CHAIN     Fnblp01L                           31
304600030321    2C     *IN31         IFEQ      *OFF
304700030321     C                   MOVEL(P)  blpCBO        wfcecbo
304800030321     C                   ENDif
304900030321     c                   else
305000030321     C     KARB          CHAIN     Fnarb01L                           31
305100030321    2C     *IN31         IFEQ      *OFF
305200030321     C                   MOVEL(P)  arbCBO        wfcecbo
305300030321     C                   ENDif
305400030321     c                   endif
305500030321     C*
305600030321     C                   ENDSR
305700970924      **************************************************************************
305800020419     C* prende in tab.3A del tipo bolla  per i ritiri i flags di invio
305900970924      **************************************************************************
306000020419     C     CHKblp        BEGSR
306100020419     C*
306200020419     C* Verifico se è un recupero
306300020419     C     KARB          CHAIN     Fnblp01L                           31
306400020419    2C     *IN31         IFEQ      *OFF
306500020419     C                   MOVEL     '3A'          COD
306600020419     C                   MOVEL(P)  blpCBO        KEY
306700020419     C                   CLEAR                   DS3A
306800020419     C     KTAB          CHAIN     TABEL00F                           32
306900020419     C  N32              MOVEL     TBLUNI        DS3A
307000020419     C                   ENDif
307100020419     C*
307200020419     C                   ENDSR
307300020419      **************************************************************************
307400020419     C* VERIFICA SE RECORD DI FIFTD00F E' DA ELABORARE
307500020419      **************************************************************************
307600020419     C     CHKFTD        BEGSR
307700970924     C                   CLEAR                   WELAB
307800970924     C* Memorizzo spedizione solo se consegnata
307900970924     C* e se non è una consegna parziale
308000970924     C* e se non è un ritorno all'incasso
308100970924    1C     FTDTMC        IFEQ      *BLANKS
308200970924     C     FTDCMC        ANDEQ     *BLANKS
308300970924     C     FTDSIC        ANDNE     'S'
308400970924     C* Verifico se è un recupero
308500970924     C     KARB          CHAIN     FNARB01L                           31
308600970924    2C     *IN31         IFEQ      *OFF
308700970924     C                   MOVEL     '3A'          COD
308800970924     C                   MOVEL(P)  ARBCBO        KEY
308900970924     C                   CLEAR                   DS3A
309000970924     C     KTAB          CHAIN     TABEL00F                           32
309100970924     C  N32              MOVEL     TBLUNI        DS3A
309200970924     C* Scarto se è un recupero dal momento che in C.E. non ci sono
309300970924     C* competenze di stop sulle spedizioni di recupero
309400970924     C     §3ARBL        IFNE      'R'
309500970924     C                   MOVE      'S'           WELAB             1
309600970924    3C                   END
309700970924    2C                   END
309800970924    1C                   END
309900970924     C                   ENDSR
310000970923      **************************************************************************
310100970924     C* SCRITTURA NUMERO DI SPEDIZIONI PER STOP DI CONSEGNA (FNSTP10F)
310200970923      **************************************************************************
310300970923     C     WRTSTP        BEGSR
310400011113     C     KFTDR         READE     FIFTD000                               30
310500970923    1C     *IN30         DOWEQ     *OFF
310600970923     C     FTDSTP        ANDEQ     W01STP
310700970923     C* Verifico se record da elaborare
310800020419     c     ftdtsr        ifeq      'C'
310900970923     C                   EXSR      CHKFTD
311000020419     c                   else
311100020419     C                   EXSR      CHKblp
311200020419     c                   move      'S'           welab
311300020419     c                   endif
311400970923    2C     WELAB         IFEQ      'S'
311500970923     C     §3ABSD        ANDEQ     *BLANKS
311600970924     C     WELAB         OREQ      'S'
311700970923     C     §3ABFI        ANDEQ     *BLANKS
311800970924     C                   CLEAR                   FNSTP100
311900970923     C                   MOVE      FTDTSR        STPTSR
312000970923     C                   MOVE      FTDAAS        STPAAS
312100970923     C                   MOVE      FTDLNP        STPLNP
312200970923     C                   MOVE      FTDNRS        STPNRS
312300970923     C                   MOVE      FTDNSP        STPNSP
312400000615     C     COMSET        IFEQ      99999
312500000615     C                   Z-ADD     99998         STPSET
312600971028     C                   ELSE
312700970930     C                   Z-ADD     COMSET        STPSET
312800971028     C                   END
312900970924     C* Imposto flag a chi inviare il record (sede,filiale o entrambi)
313000970924    3C                   SELECT
313100970924     C     §3ABSD        WHENEQ    *BLANKS
313200970924     C     §3ABFI        ANDEQ     *BLANKS
313300970924     C                   MOVEL     'E'           STPINV
313400970924     C     §3ABSD        WHENEQ    *BLANKS
313500970924     C                   MOVEL     'S'           STPINV
313600970924     C     §3ABFI        WHENEQ    *BLANKS
313700970924     C                   MOVEL     'F'           STPINV
313800970924    3C                   ENDSL
313900970923     C* scrivo record per trasmissione a sede
314000970924     C                   WRITE     FNSTP100
314100970923    2C                   END
314200011113     C     KFTDR         READE     FIFTD000                               30
314300970923    1C                   ENDDO
314400970924     C                   ENDSR
314500970924      **************************************************************************
314600970924     C* LETTURA ARCHIVIO BOLLE PARTENZA PER STOP DI RITIRO
314700970924      **************************************************************************
314800970924     C     ELABLP        BEGSR
314900970924     C* IMPOSTO CHIAVE PER LETTURA ARCHIVIO BOLLE PARTENZA
315000970924     C                   MOVE      FTDKSC        W01KSC
315100970924     C                   CLEAR                   W01CCM
315200970924     C                   MOVE      FTDNDC        W01NRT
315300970924     C     KBLP1         SETLL     FNBLP34L                               31
315400970924     C* Assegnato non codificato
315500970924    1C     *IN31         IFEQ      *OFF
315600970924     C                   MOVE      9999          W01KSC
315700970924     C                   MOVE      FTDKSC        W01CCM
315800970924     C     KBLP1         SETLL     FNBLP34L
315900970924    1C                   END
316000970924     C                   EXSR      WRTSTR
316100970924     C                   MOVE      FTDKSC        W0040             4 0
316200971013    1C                   SELECT
316300971013     C* Se franco non codificato ripeto WRTSTR con KSC = 9999
316400971013     C* (Caso di ritiro di alcune spedizioni in franco e di altre in
316500971013     C*  assegnato presso lo stesso cliente non codificato)
316600971013   WHC     W0040         WHENEQ    8888
316700970924     C                   MOVE      9999          W01KSC
316800970924     C                   CLEAR                   W01CCM
316900971013     C     KBLP1         SETLL     FNBLP34L
317000971013     C                   EXSR      WRTSTR
317100971013     C* Se franco codificato ripeto WRTSTR con KSC=9999 E CCM=KSC
317200971013     C* (Caso di ritiro di alcune spedizioni in franco e di altre in
317300971013     C*  assegnato presso lo stesso cliente codificato)
317400971013   WHC     W0040         WHENNE    9999
317500971013     C                   MOVE      9999          W01KSC
317600971013     C                   MOVE      FTDKSC        W01CCM
317700970924     C     KBLP1         SETLL     FNBLP34L
317800970924     C                   EXSR      WRTSTR
317900971013    1C                   ENDSL
318000970924     C                   ENDSR
318100950801      **************************************************************************
318200970923     C* MEMORIZZAZIONE STOP DI RITIRO
318300950801      **************************************************************************
318400970924     C     WRTSTR        BEGSR
318500950801     C*
318600950801     C     KBLP1         READE     FNBLP34L                               31
318700950801     C*
318800950801    1C     *IN31         DOWEQ     *OFF
318900970923     C* Escludo i recuperi in quanto non possono esistere dei ritiri
319000970923     C* a fronte di recuperi
319100950801     C                   MOVEL     '3A'          COD
319200950801     C                   MOVEL(P)  BLPCBO        KEY
319300950801     C                   CLEAR                   DS3A
319400950801     C     KTAB          CHAIN     TABEL00F                           32
319500950801     C  N32              MOVEL     TBLUNI        DS3A
319600970923    2C     §3ARBL        IFNE      'R'
319700970924     C     §3ABSD        ANDEQ     *BLANKS
319800970924     C     §3ARBL        ORNE      'R'
319900970924     C     §3ABFI        ANDEQ     *BLANKS
320000970924     C                   CLEAR                   FNSTP100
320100970924     C                   MOVE      FTDTSR        STPTSR
320200970924     C                   MOVE      BLPAAS        STPAAS
320300970924     C                   MOVE      BLPLNP        STPLNP
320400970924     C                   MOVE      BLPNRS        STPNRS
320500970924     C                   MOVE      BLPNSP        STPNSP
320600970924    3C     FTDSTP        IFEQ      *ZEROS
320700000615     C     FTDSET        IFEQ      99999
320800000615     C                   Z-ADD     99998         STPSET
320900971028     C                   ELSE
321000970924     C                   Z-ADD     FTDSET        STPSET
321100971028     C                   END
321200970924   X3C                   ELSE
321300000615     C     COMSET        IFEQ      99999
321400000615     C                   Z-ADD     99998         STPSET
321500971028     C                   ELSE
321600970930     C                   Z-ADD     COMSET        STPSET
321700971028     C                   END
321800970924    3C                   END
321900970924    3C                   SELECT
322000970924     C     §3ABSD        WHENEQ    *BLANKS
322100970924     C     §3ABFI        ANDEQ     *BLANKS
322200970924     C                   MOVEL     'E'           STPINV
322300970924     C     §3ABSD        WHENEQ    *BLANKS
322400970924     C                   MOVEL     'S'           STPINV
322500970924     C     §3ABFI        WHENEQ    *BLANKS
322600970924     C                   MOVEL     'F'           STPINV
322700970924    3C                   ENDSL
322800970924     C                   WRITE     FNSTP100
322900950801    2C                   END
323000950801     C     KBLP1         READE     FNBLP34L                               31
323100950801    1C                   ENDDO
323200950801     C*
323300950801     C                   ENDSR
323400970415      **************************************************************************
323500970415     C* SCRITTURA NUMERI STOP DI RITIRO PER IL DELTA DI FILIALE
323600970415      **************************************************************************
323700970415     C     RITDEL        BEGSR
323800970415     C* SE NON C'E' IL NUMERO DI STOP SCRIVO SUBITO
323900970415     C*  SE C'E' DEVO TENERE MEMORIZZATA LA KEY PER SOMMARE INSIEME
324000970415     C*  I RECORD CON LO STESSO NUMERO DI STOP
324100970415     C**
324200970415     C                   MOVE      FTDKSC        W0040             4 0
324300970415     C*  ESCLUDO I CLIENTI GENERICI 8888 E 9999
324400970415    1C     W0040         IFNE      8888
324500970415     C*
324600970415    2C     FTDSTP        IFNE      SAVSTP
324700970415    3C     SAVSTP        IFGT      0
324800970415     C                   EXSR      WRISBP
324900970415    3C                   ENDIF
325000970415     C                   Z-ADD     FTDSTP        SAVSTP
325100970415    2C                   ENDIF
325200970415     C*
325300970415     C                   MOVEL     FTDKSC        KSC(K)
325400970415     C                   MOVEL     FTDRSC        RSC(K)
325500970415     C                   ADD       1             K
325600970415     C                   ADD       FTDSET        SAVSET
325700970415     C*
325800970415    2C     FTDSTP        IFEQ      0
325900970415     C                   EXSR      WRISBP
326000970415    2C                   ENDIF
326100970415    1C                   ENDIF
326200970415     C*
326300970415     C                   ENDSR
326400970415      **************************************************************************
326500970415     C* SCRITTURA FILE FNSBP00F
326600970415      **************************************************************************
326700970415     C     WRISBP        BEGSR
326800970415     C                   Z-ADD     1             K
326900970415     C     KSC(K)        DOWGT     0
327000970415     C                   Z-ADD     KSC(K)        SAVKSC
327100970415     C                   MOVEL     RSC(K)        SAVRSC
327200970415     C*
327300970415     C                   ADD       1             K                 3 0
327400970415     C                   ENDDO
327500020419     C*
327600970415     C                   CLEAR                   KSC
327700970415     C                   CLEAR                   RSC
327800970415     C                   CLEAR                   SAVSET
327900970415     C                   Z-ADD     1             K
328000970415     C                   ENDSR
328100020419     C*----------------------------------------------------------------
328200950127     C* ESPONGO I CAMPI ITT E ITA IN CAMPI ALFABETICI DEL VIDEO ------*
328300020419     C*----------------------------------------------------------------
328400011113     C     ALFAB         BEGSR
328500950127     C* ELIMINO GLI 0 NON SIGNIFICATIVI
328600011113     c                   movel     ','           k08(8)
328700011113     C                   Z-ADD     1             X
328800011113     C     K08(X)        DOWEQ     '0'
328900011113     c     X             andlt     8
329000011113     C                   MOVEL     ' '           K08(X)
329100011113     C                   ADD       1             X                 3 0
329200011113     C                   ENDDO
329300011113      *
329400011113     C                   ENDSR
329500950912      **************************************************************************
329600950912     C* DELET DA FILE DI LAVORO DEI RECORDS DI QUESTO LAVORO
329700950912      **************************************************************************
329800950912     C     SRWC01        BEGSR
329900951011     C     KWC1          SETLL     FNWLRC1L
330000951011     C     KWC1          READE     FNWLRC1L                               30
330100950912     C     *IN30         DOWEQ     *OFF
330200950912     C                   DELETE    FNWC01
330300951011     C     KWC1          READE     FNWLRC1L                               30
330400950912     C                   ENDDO
330500950912     C                   ENDSR
330600950912     C*
330700020327      **************************************************************************
330800020327     C* DELET Del WorkFile voci C/Economico da Valorizzazione Conteggi
330900020327      **************************************************************************
331000020327     C     DEL_FIFCW     BEGSR
331100020327     C*
331200020521     C* solo se non in simulazione
331300020522     C     PARSML        IFeq      ' '
331400020723     C     w01nrr        andgt     0
331500020521     C*
331600020327     C* deve cancellare i records creati e non confermati dal programma
331700020327     c                   z-add     1             nrr
331800020327     c     nrr           chain     FRC0SF2                            21
331900020327     C*
332000020327     c     *in21         doweq     *off
332100020327     C*
332200020423     C* se sono rimasti per errore dei records
332300020423     C*  quando è sui totali elimina tutto ciò che riguarda il padroncino
332400020619     c     vh2trc        ifeq      'T'
332500020423     c                   movel     vh2pdr        kfgsw
332600020423     c                   z-add     vh2pdr        kpdrw
332700020423     c                   z-add     vh2ddc        kddcw
332800020423     C*  Elimina tutto
332900050208     C     Kfcw2T        setll     fifcew3l
333000050208     C     Kfcw2T        reade     fifcew3l
333100050208     c                   dow       not %eof(fifcew3l)
333200020718     c                   if        F6Fine = 'S'
333300020611     c                   exsr      Spia_Records                                 <-------
333400020718     c                   end
333500020423     C                   delete    fifcw000
333600050208     C     Kfcw2T        reade     fifcew3l
333700020423     c                   end
333800020423     C*
333900020619     c                   endif
334000020327     C*
334100020327     c                   add       1             nrr
334200020327     c     nrr           chain     FRC0SF2                            21
334300020327     c                   enddo
334400020327     C*
334500020522     C                   ENDif
334600020521     C*
334700020521     C                   ENDSR
334800020329      **************************************************************************
334900020329      * Emissione Window con dettaglio importi della riga Base e Accessori
335000020329      **************************************************************************
335100020329     C* Emette la finestra con il dettaglio degli importi tariffe ----*
335200020329     c     WndwSfl       begsr
335300020329     c*
335400011113     c                   seton                                        35
335500011113     c                   write     FRC0wc6
335600011113     c                   setoff                                       35
335700011113     c*
335800011113     c                   clear                   nrw6              5 0
335900011113     c                   z-add     1             wrc6
336000011113     c                   move      vh2div        w6cdiv
336100011113     c                   movel     vi2des        w6cdes
336200020128     c                   z-add     vi2itt        w6cbas
336300020128     c                   z-add     vi2ita        w6cacc
336400020128     c                   z-add     vi2tpv        w6cpck
336500020128     c                   z-add     vi2tbv        w6cbon
336600011113     c*------->
336700011113     c* Carica le  righe di dettaglio importi
336800011113     c                   do        *hival
336900011113     c* riga del sfl
337000011113     c                   add       1             nrw6
337100011113     c*  deve caricare solo 7 righe max.
337200011113     c                   if        nrw6 > 7
337300011113     c                   leave
337400011113     c                   endif
337500011113     c*
337600011113     c                   z-add     nrw6          w1htrc
337700011113     c                   z-add     0             w1valr
337800011113     c                   movel     *blank        w1tipo
337900011113     c                   setoff                                       60
338000011113     c*
338100011113     c                   select
338200011113     c*
338300011113     c     nrw6          wheneq    1
338400011113     c                   seton                                        60
338500011113     c                   movel     'SPEDIZIONI'  w1tipo
338600011113     c*
338700011113     c     nrw6          wheneq    2
338800011113     c                   movel     'Base'        w1tipo
338900011113     c                   z-add     vi2itt        w1valr
339000011113     c*
339100011113     c     nrw6          wheneq    3
339200011113     c                   movel     'Accessori'   w1tipo
339300011113     c                   z-add     vi2ita        w1valr
339400011113     c*
339500011113     c     nrw6          wheneq    5
339600011113     c                   seton                                        60
339700011113     c                   movel     'PRESTAZIONE' w1tipo
339800011113     c*
339900011113     c     nrw6          wheneq    6
340000011113     c                   movel     'Pick/Etich'  w1tipo
340100011113     c                   z-add     vi2tpv        w1valr
340200011113     c*
340300011113     c     nrw6          wheneq    7
340400011113     c                   movel     'Bonus'       w1tipo
340500011113     c                   z-add     vi2tbv        w1valr
340600011113     c*
340700011113     c                   endsl
340800011113     c*
340900011113     c                   write     FRC0ws6
341000011113     c                   enddo
341100011113     c*-------> Emissione Window
341200011113     c     resta_wdw     tag
341300011113      *                *----------------------*
341400011113     c                   write     FRC0wd6
341500011113     c                   exfmt     FRC0wc6
341600011113      *                *----------------------*
341700011113     c*  Controlli
341800011113     c* Cntrl le righe x importi modificati
341900011113     c                   z-add     0             nrw6
342000011113      *------------        ---------------
342100011113     c* resta sulla window se non ha premuto dei cmd
342200011113     c     *inkl         ifeq      *off
342300011113     c                   goto      resta_wdw
342400011113     c                   end
342500011113      *
342600011113     c                   endsr
342700020329      **************************************************************************
342800040426      * Scrive forzatamente le Voci di FCE della giornata = 0 x ogni bolla
342900020329      **************************************************************************
343000040426     c     Wrt0_VOCI     begsr
343100040426     **
343200040426     C     Kftd6_zero    setll     fiftd06l
343300040426     C     Kftd6_zero    reade     fiftd06l
343400040426     **
343500040426     c                   dow       not %eof(fiftd06l)
343600040426     **
343700040426     **  scrive i records bolla con valore (0)
343800040426     C                   z-add     ftdfgs        fceFGS
343900040426     c                   if        fceFGS=0
344000040426     C                   movel     ftdpdr        fceFGS
344100040426     c                   end
344200040426     C                   z-add     ftdpdr        fcePDR
344300040426     C                   move      ftdtsr        fceTSR
344400040426     C                   z-add     ftdndc        fceNDC
344500040426     C                   z-add     ftdddc        fceDDC
344600040426     C                   z-add     ftdaas        fceAAS
344700040426     C                   z-add     ftdlnp        fceLNP
344800040426     C                   z-add     ftdnrs        fceNRS
344900040426     C                   z-add     ftdnsp        fceNSP
345000040426      *
345100040426      *  chiodo x impostare il codice Consegne
345200040426     C                   if        ftdtsr ='C'
345300040426     c                   move      '005'         fcecce
345400040426     c                   end
345500040426      *  chiodo x impostare il codice Ritiri
345600040426     C                   if        ftdtsr ='R'
345700040426     c                   move      '001'         fcecce
345800040426     c                   end
345900040426     C*
346000040426     c                   exsr      Cod_bolla
346100040426
346200040426     C*  somma algebrica del valore
346300040426     C                   move      wfcecbo       fcecbo
346400040426     C                   z-add     0             fceice
346500040426     C                   write     fifce000
346600040426     **
346700040426     C     Kftd6_zero    reade     fiftd06l
346800040426     c                   enddo
346900040426     **
347000040426     c                   endsr
347100040426      **************************************************************************
347200040426      * Aggiorna le Voci di conto Economico tramite il Workfile
347300040426      **************************************************************************
347400040426     c     AGG_VOCI      begsr
347500040426     c*
347600020329     c*  legge in sequenza il file di work con il quale poi, agganciando tutti
347700020329     c*  i relativi dettagli, in base al peso di ciascuno sul peso totale
347800020329     c*  attribuisce il valore proporzionale alla singola bolla per voce di
347900020329     c*  conto economico.
348000050208     c     *loval        setll     fifcew3l
348100050208     c                   read      fifcew3l
348200020329     C* ------1->
348300050208     c                   dow       not %eof(fifcew3l)
348400020524     C* flag di forzatura
348500020524     c                   move      *blank        forza_pag         1
348600040107     c                   move      *blank        fatto_uno         1
348700040407     c                   z-add     0             al_max            3 0
348800020524     C*
348900020515     C* se non aveva impostato i pesi totali sul record
349000020515     C*   il caso può essere se il documento aveva tutte tariffe poste
349100030404     c                   if        fcwkgc = 0  and fcwtsr ='C' or
349200030404     c                             fcwkgr = 0  and fcwtsr ='R' or
349300031219     c* ricalcolo sempre x "Giornata" x sicurezza poichè il FICNB1R non sempre
349400031219     c*  passa il totale pesi x la giornata (vedi spedizioni ISOLE)
349500040726     c                             fcwtsr ='G'
349600020515     C                   exsr      CALC_KG
349700020515     c                   endif
349800050222     C*
349900050222     C*  ATTENZIONE: potrei avere ancora i kg = 0 poichè esiste una ECCEZIONE.
350000050222     C*   Eccezione: x RETTIFICHE MANUALI (FIFRE--> "RET") fatte su tipi servizi
350100050222     C*     non trattati su quella giornata.
350200050222     C*  Esempio: se il padroncino in quella giornata ha fatto solo Consegne e gli
350300050222     C*  viene fatta una rettifica sui Ritiri ..... I kg dei ritiri sono (0) e non
350400050222     C*  riesce ad eseguire questa routine. A questo punto occorre ribaltare sulle
350500050222     C*  Consegne la rettifica altrimenti il dato verrebbe perso.
350600020515     C*
350700020329     C* Imposta il totale dei kg da riproporzionare
350800020329     c                   move      *zeros        totkg             9 1
350900050222      *
351000050224     C                   Select
351100050224      *
351200050224     C                   WHEN      fcwtsr ='C'
351300020329     c                   z-add     fcwkgc        totkg             9 1
351400050222      * Eccezione:RET Consegne --> ribalta su Ritiri
351500050222     c                   if        fcWt8a = 'RET     ' and totkg = 0
351600050222     c                   z-add     fcwkgr        totkg
351700050222     C                   eval      fcwtsr ='R'
351800050222     c                   end
351900050224      * Eccezione:BON Consegne --> ribalta su Ritiri (Il Bonus è solo x Consegne)
352000050224     c                   if        fcWt8a = 'G999 BON' and totkg = 0  OR
352100050224     c                             fcWt8a = 'C999 BON' and totkg = 0
352200050224     c                   z-add     fcwkgr        totkg
352300050224     C                   eval      fcwtsr ='R'
352400050224     c                   end
352500050222      *
352600050224     C                   WHEN      fcwtsr ='R'
352700020329     c                   z-add     fcwkgr        totkg
352800050222      * Eccezione:RET Ritiri   --> ribalta su Consegne
352900050222     c                   if        fcWt8a = 'RET     ' and totkg = 0
353000050222     c                   z-add     fcwkgc        totkg
353100050222     C                   eval      fcwtsr ='C'
353200050222     c                   end
353300050224      * Eccezione:RAN Ritiri   --> ribalta su Consegne (Il Rit.Ann. è solo x Ritiri)
353400050224     c                   if        fcWt8a = 'R999 RAN' and totkg = 0
353500050224     c                   z-add     fcwkgc        totkg
353600050224     C                   eval      fcwtsr ='C'
353700050224     c                   end
353800050222      *
353900050224     C                   WHEN      fcwtsr ='G'
354000020329     c     fcwkgc        add       fcwkgr        totkg
354100050222      *
354200020329     C                   endsl
354300020329     C*
354400050222     C*
354500050222     C*    ECCEZIONE all'ECCEZIONE:   -->  ( N O N    G E S T I B I L E )
354600050222     C*  Esiste però il caso di rettifiche senza kg nè su Consegne nè su Ritiri
354700050222     C*  poichè il padroncino è pagato a Giornata quindi non si riesce a ribaltare
354800050222     C*  il peso fra Ritiri e Consegne.....
354900050222     C*
355000020417     C* A T T E N Z I O N E : esegue solo se il totale Kg è > 0
355100020417     c                   if        totkg > 0
355200020417      *
355300020329     C* imposta il campo utilizzato come resto del valore totale da attribuire
355400020329     c                   z-add     fcwice        resto_ice
355500020329     C*
355600031218     c     rispalma      tag
355700040726     **
355800040726     **     Conteggia quante bolle ci sono e quante sono da Non Pagare
355900040726     **      se dovesse coincidere il totale Bolle con quelle da non Pagare
356000040726     **       allora successivamente nella Routine xResto devo comunque
356100040726     **      spalmare eventuali valori ad esempio Rettifiche.
356200040726     c                   z-add     0             num_bolle         5 0
356300040726     c                   z-add     0             num_bol_nonPa     5 0
356400140115     c                   add       1             al_max
356500020329     C*  quindi legge il dettaglio delle bolle per data documento
356600020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
356700020329     C     Kftd6         setll     fiftd06l
356800020329     C     Kftd6         reade     fiftd06l
356900020329     c                   else
357000020329     C     Kftd6_1       setll     fiftd06l
357100020329     C     Kftd6_1       reade     fiftd06l
357200020329     c                   end
357300020329     C* ----2->
357400020329     c                   dow       not %eof(fiftd06l)
357500140115     c**********         add       1             al_max
357600040726     c                   add       1             num_bolle         5 0
357700020329      *
357800020329      * non deve avere un cod.giacenza/manc.consegna
357900020524      *  ma se tutte le bolle nel documento erano non da pagare
358000020524      *  allora occorre spalmare comunque.
358100020524     c     forza_pag     ifeq      'S'
358200020524     c                   move      'S'           W01pag
358300020524     c                   else
358400020423     C                   exsr      Se_daPagare
358500020524     c                   end
358600020524      *
358700020423     c     W01pag        ifeq      'S'
358800031216      *
358900031216     c***  Attenzione : solo se il totale generale è superiore al singolo dettaglio
359000031216     c                   if        ftdpkl <= totkg
359100040107     c                   move      'S'           fatto_uno
359200020329     C*               *--------------------*
359300020329     c                   exsr      xBolla
359400020329     C*               *--------------------*
359500031216     c                   end
359600031216      *
359700040726     c                   else
359800040726      **
359900040726      ** conta quelle da NON PAGARE per spalmare successivamente
360000040726     c                   add       1             num_bol_nonPa
360100020329     c                   end
360200020329      *
360300020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
360400020329     C     Kftd6         reade     fiftd06l
360500020329     c                   else
360600020329     C     Kftd6_1       reade     fiftd06l
360700020329     c                   end
360800020329     c                   enddo
360900040726     **
361000040726     **   Se coincide il totale bolle di dettaglio con le bolle da non Pagare
361100040726     **    anche in questo caso deve forzare il pagamento
361200040726     **     ha fatto il giro (al_max=1) la prima volta a vuoto poichè solo dopo aver letto
361300040726     **      il dettaglio può conoscere che tipo di bolle ci sono all'interno del documento.
361400040726     c                   if        num_bolle = num_bol_nonPa and al_max = 1
361500040726     c                   move      'S'           forza_pag
361600040726     c                   end
361700031218      *
361800031218      * Se c'è un resto oltre l'euro prova spalmare nuovamente il rimanente
361900031218      *  sull bolle che aveva precedentemente calcolato
362000040407      *  ma non deve continuare se il resto rimane sempre quello e può tentare
362100040407      *  al max. 3 volte dopodichè lo somma all'ultima spedizione.
362200040726      *  oppure
362300040726      *    ATTENZIONE:
362400040726      *  se la totalità delle bolle era da non pagare e comunque devo spalmare
362500040726      *  il valore (es.una rettifica di una Giornata) e al_max è = 1 (poichè è già passato
362600040726      *  una volta dentro al ciclo di lettura) prova a spalmare e se rimane del resto
362700040726      *  prova a rispalmare altre 3 volte prima di passare alla routine RESTO.
362800040726      *  (Il test è fatto con "4" poichè il contatore al_max parte da 1 e non da 0)
362900040407     c                   if        resto_ICE > 1 and fatto_uno ='S' and
363000040407     c                             resto_ice <> fcwice and al_max <= 3
363100040726     ****
363200040726     c                             or
363300040726     c                             num_bolle = num_bol_nonPa and al_max = 1
363400040726     c                             or
363500040726     c                             num_bolle = num_bol_nonPa and
363600040726     c                             resto_ice <> fcwice and al_max <= 4
363700040726     ****
363800031218     c                   z-add     resto_ice     fcwice
363900031218     c                   goto      rispalma
364000031218     c                   end
364100020329      *
364200020329      * se c'è del resto lo attribuisce sull'ultimo dettaglio
364300020423     c                   if        resto_ICE <> 0
364400040726     **
364500020329     C*               *--------------------*
364600020329     c                   exsr      xResto
364700020329     C*               *--------------------*
364800020329     c                   end
364900020329     C* ----2->
365000020417     c                   endIF
365100020417      *
365200050208     c                   read      fifcew3l
365300020329     c                   enddo
365400020329     C* ------1->
365500020329      *
365600020329     c                   endsr
365700020329      **************************************************************************
365800020515      * Deve calcolare i kg per documento sia ritiri che consegne
365900020329      **************************************************************************
366000020515     c     CALC_KG       begsr
366100020515      *
366200020524     c                   move      'SI'          piucheck          2
366300020603     c                   z-add     0             contaC            5 0
366400020603     c                   z-add     0             contaR            5 0
366500030404     c*
366600030404     c                   z-add     0             fcwkgc
366700030404     c                   z-add     0             fcwkgr
366800020524      *
366900020524     c     rifai         tag
367000020524      *
367100020515     C*  quindi legge il dettaglio delle bolle per data documento
367200020515     C     Kftd6_1       setll     fiftd06l
367300020515     C     Kftd6_1       reade     fiftd06l
367400020515     C* ----2->
367500020515     c                   dow       not %eof(fiftd06l)
367600020515      *
367700020515      * non deve avere un cod.giacenza/manc.consegna
367800020524     c     piucheck      ifeq      'SI'
367900020515     C                   exsr      Se_daPagare
368000020524     c                   else
368100020524     c                   move      'S'           W01pag
368200020524     c                   end
368300020524      *
368400020515     c     W01pag        ifeq      'S'
368500020515     c     ftdtsr        ifeq      'C'
368600020515     c                   add       ftdPKL        fcwkgc
368700020603     c                   add       1             contaC
368800020515     c                   end
368900020515     c     ftdtsr        ifeq      'R'
369000020515     c                   add       ftdPKL        fcwkgr
369100020603     c                   add       1             contaR
369200020515     c                   end
369300020515     c                   end
369400020515      *
369500020515     C     Kftd6_1       reade     fiftd06l
369600020515     c                   enddo
369700020524      *
369800020524      *  se sono tutte bolle da non pagare allora riesegue la
369900020524      *  routine di calcolo kg escludendo il controllo del da_pagare
370000020603     c                   if        fcwkgc = 0 and fcwkgr = 0 and
370100020603     c                             piucheck = 'SI'
370200020524     c                   move      'NO'          piucheck          2
370300020524     c                   move      'S'           forza_pag
370400020524     c                   goto      rifai
370500020524     c                   end
370600020515      *
370700020515     c                   endsr
370800020515      **************************************************************************
370900020515      * Letta la Bolla calcola e scrive il record di Voce di Conto Economico
371000020515      **************************************************************************
371100020515     c     xBolla        begsr
371200020515     c*
371300020329     C* Crea il valore da ripartire:
371400020329     C*  proporziona la PARte relativa al peso della spedizione e si
371500020329     C*   memorizza il resto
371600020417     C*
371700020329     c                   eval      val_ice = fcwice * ftdpkl / totkg
371800020329     c                   eval      resto_ice = resto_ice - val_ice
371900020329     C*
372000020329     C                   z-add     ftdfgs        fceFGS
372100020329     c                   if        fceFGS=0
372200020329     C                   movel     ftdpdr        fceFGS
372300020329     c                   end
372400020329     C                   z-add     ftdpdr        fcePDR
372500020329     C                   move      ftdtsr        fceTSR
372600020329     C                   z-add     ftdndc        fceNDC
372700020329     C                   z-add     ftdddc        fceDDC
372800020329     C                   z-add     ftdaas        fceAAS
372900020329     C                   z-add     ftdlnp        fceLNP
373000020329     C                   z-add     ftdnrs        fceNRS
373100020329     C                   z-add     ftdnsp        fceNSP
373200020403      *
373300020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
373400020329     c                   move      fcwcce        fcecce
373500020329     c                   else
373600020403     c                   exsr      CCE_da8A
373700020329     c                   end
373800020329     C*
373900030321     c                   exsr      Cod_bolla
374000030321
374100020329     C     Kfce          chain     fifce01l
374200020329     c                   if        %found(fifce01l)
374300020329     C*  somma algebrica del valore
374400030403     C                   move      wfcecbo       fcecbo
374500020329     C                   add       val_ice       fceice
374600020329     C                   update    fifce000
374700020329     C*
374800020329     c                   else
374900020329     C*
375000020329     C                   clear                   fifce000
375100020329     C                   z-add     ftdfgs        fceFGS
375200020329     c                   if        fceFGS=0
375300020329     C                   movel     ftdpdr        fceFGS
375400020329     c                   end
375500020329     C                   z-add     ftdpdr        fcePDR
375600020329     C                   move      ftdtsr        fceTSR
375700020329     C                   z-add     ftdndc        fceNDC
375800020329     C                   z-add     ftdddc        fceDDC
375900020329     C                   z-add     ftdaas        fceAAS
376000020329     C                   z-add     ftdlnp        fceLNP
376100020329     C                   z-add     ftdnrs        fceNRS
376200020329     C                   z-add     ftdnsp        fceNSP
376300030321     C                   move      wfcecbo       fcecbo
376400020403      *
376500020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
376600020329     c                   move      fcwcce        fcecce
376700020329     c                   else
376800020403     c                   exsr      CCE_da8A
376900020329     c                   end
377000020403      *
377100020329     C*  somma algebrica del valore
377200020329     C                   add       val_ice       fceice
377300020329     C                   write     fifce000
377400020329     c                   end
377500020417      *
377600020329     c                   endsr
377700020329      **************************************************************************
377800020329      * se è rimasto del resto da attribuire lo mette sull'ultima bolla
377900020329      **************************************************************************
378000020329     c     xResto        begsr
378100020329     c*
378200020329     C*  se c'è un resto ancora da attribuire lo ripartiamo sull'ultima
378300020329     C*  bolla letta.
378400020329     C*  quindi legge il dettaglio delle bolle per data documento
378500020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
378600020329     C     Kftd6         setgt     fiftd06l
378700020329     C     Kftd6         readpe    fiftd06l
378800020329     c                   else
378900020329     C     Kftd6_1       setgt     fiftd06l
379000020329     C     Kftd6_1       readpe    fiftd06l
379100020329     c                   end
379200020423      *
379300020423     c                   dow       not %eof(fiftd06l)
379400020329     C*
379500020423      * non deve avere un cod.giacenza/manc.consegna
379600020524      *  ma se tutte le bolle nel documento erano non da pagare
379700020524      *  allora occorre spalmare comunque.
379800020524     c     forza_pag     ifeq      'S'
379900020524     c                   move      'S'           W01pag
380000020524     c                   else
380100020423     C                   exsr      Se_daPagare
380200020524     c                   end
380300020524      *
380400020423     c     W01pag        ifeq      'S'
380500020329     C*
380600020329     C                   z-add     ftdfgs        fceFGS
380700020329     c                   if        fceFGS=0
380800020329     C                   movel     ftdpdr        fceFGS
380900020329     c                   end
381000020329     C                   z-add     ftdpdr        fcePDR
381100020329     C                   move      ftdtsr        fceTSR
381200020329     C                   z-add     ftdndc        fceNDC
381300020329     C                   z-add     ftdddc        fceDDC
381400020329     C                   z-add     ftdaas        fceAAS
381500020329     C                   z-add     ftdlnp        fceLNP
381600020329     C                   z-add     ftdnrs        fceNRS
381700020329     C                   z-add     ftdnsp        fceNSP
381800020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
381900020329     c                   move      fcwcce        fcecce
382000020329     c                   else
382100020403     c                   exsr      CCE_da8A
382200020329     c                   end
382300020329     C*
382400020329     C     Kfce          chain     fifce01l
382500020329     c                   if        %found(fifce01l)
382600020329     C*  somma algebrica del valore
382700020329     C                   add       resto_ice     fceice
382800020329     C                   update    fifce000
382900020423     C* quindi dopo aver aggiornato può uscire forzatamente
383000020423     c                   leave
383100020329     c                   end
383200020329     C*
383300020329     c                   end
383400020423     C*
383500020423     C                   if        fcwtsr ='C' or fcwtsr ='R'
383600020423     C     Kftd6         readpe    fiftd06l
383700020423     c                   else
383800020423     C     Kftd6_1       readpe    fiftd06l
383900020423     c                   end
384000020423      *
384100020423     c                   enddo
384200020329     C*
384300020329     C                   endsr
384400020319     c*****************************************************************
384500020403     C*  Prende dalla 8A la voce di conto
384600020403     c*****************************************************************
384700020403     C     CCE_da8A      begsr
384800020403      *------
384900020403     C* deve prendere la voce di C/E dalla tabella 8A
385000020403     C                   clear                   ds8A
385100020403     C                   movel     '8A'          cod
385200020403     C                   MOVEL(P)  fcwt8A        KEY
385300020403     C     KTAB          CHAIN     TABEL00F                           32
385400020403     C  n32              movel     tbluni        ds8A
385500020403     C  n32              if        ftdtsr ='C'
385600020403     C                   if        §8ACE1 <> *zeros and §8ACE1 <> *blank
385700020403     C                   move      §8ACE1        fcecce
385800020403     c                   else
385900020403     C                   move      §8ACE3        fcecce
386000020403     c                   end
386100020403     c                   else
386200020403     C                   if        §8ACE2 <> *zeros and §8ACE2 <> *blank
386300020403     C                   move      §8ACE2        fcecce
386400020403     c                   else
386500020403     C                   move      §8ACE4        fcecce
386600020403     c                   end
386700020403     c                   end
386800020403      *------
386900020403     C                   endsr
387000020403     c*****************************************************************
387100020423      * controlla se la bolla è da pagare
387200020423     c*****************************************************************
387300020423     C     Se_daPagare   Begsr
387400020524      *------
387500020423     C                   movel     ' '           W01PAG
387600020423     C                   seton                                        70
387700020423      *------
387800020423     C* se bolla posta oppure no su azorg per linea di partenza
387900020423     C     ftdLNP        chain     AZORG01L                           74
388000020423     C   74              movel     *BLANKS       OG143
388100020423     C  N74              movel     ORGDE3        OG143
388200020423     C*
388300020423     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
388400020423      *         *--------------------------------------*
388500020423     c                   if        ftdtsr ='C'
388600020423     C* T I P O   B O L L A   D A   T A S S A R E
388700020423     C                   clear                   ARBCBO
388800020423     C     kftd3         chain     FNARB01L                           72
388900020423     C                   movel     W018C         WALF6             6
389000020423     C                   move      ftdfgs        WALF6
389100020423     C     WALF6         lookup    CBL                                    71
389200020423     C* SE INESISTENTE CERCO CON FILIALE A 0
389300020423     C                   if        *in71 = *off
389400020423     C                   move      000           WALF6
389500020423     C     WALF6         lookup    CBL                                    71
389600020423     C                   endif
389700020423     C   71              setoff                                       70
389800020423      *         *------------------*
389900020423     C* MANCATA  CONSEGNA  DA  TASSARE   (SE NON CONSEGNATA)
390000020423     C   70              if        ftddcm = 0
390100020423     C*         *------*
390200020423     C     ftdCMC        ifne      *BLANKS
390300020423     C                   movel     ftdCMC        WALF6
390400140115      *** NON esiste immissione di un codice per TIPOLOGIA MANCATA CONSEGNA
390500140115      **   ma solo per un codice determinato in tabella di MANCATA CONSEGNA
390600140115     C*****              else
390700140115     C****               movel     ftdTMC        WALF6
390800140115     C***                end
390900020423     C*         *------*
391000020423     C                   MOVE      ftdfgs        WALF6
391100020423     C     WALF6         lookup    CMC                                    70
391200020423     C* SE INESISTENTE CERCO CON FILIALE A 0
391300020423     C                   if        *in70 = *off
391400020423     C                   move      000           WALF6
391500020423     C     WALF6         lookup    CMC                                    70
391600020423     C                   endif
391700020423     C*         *------*
391800140115     C                   end
391900140115     C*         *------*
392000020423     C                   end
392100020423      *         *------------------*
392200020423     C                   endif
392300020423      *         *--------------------------------------*
392400020423     C*  P.O. POSTE
392500020729     C     §OgNTW        IFEQ      'PPT'
392600020423     C                   setoff                                       70
392700020423      *
392800020423     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE
392900020423      * SIGNIFICA CHE LA BOLLA SI DEVE PAGARE COME CONSEGNATA
393000020423     C                   MOVEL     '2Z'          COD
393100020423     C                   MOVEL     *BLANKS       KEY
393200020423     C                   MOVEL     FTDCMC        KEY
393300020423     C     KTAB          CHAIN     TABEL                              70
393400020423     C                   clear                   DS2Z
393500020423     C  N70              movel     TBLUNI        DS2Z
393600020423     C* LA BOLLA VIENE PAGATA
393700020423     C     §2ZTPP        ifne      *BLANKS
393800020423     C                   seton                                        70
393900020423     C                   endif
394000020423     C                   end
394100020423     C*
394200020423     C*  Bolla da pagare
394300020423     c                   if        ftdtsr ='R' or *in70=*on
394400020423     C                   MOVEL     'S'           W01PAG            1
394500020423     C                   end
394600020423      *------
394700020611     C                   endsr
394800020611     c*****************************************************************         <-------
394900020611      * prima di cancellare salva i records di work per FCE a giornata          <-------
395000020611     c*****************************************************************         <-------
395100020611     C     Spia_Records  Begsr                                                  <-------
395200020612      *------                                                                   <-------
395300020612     c                   eval      fcspdr = fcwpdr                              <-------
395400020612     c                   eval      fcstsr = fcwtsr                              <-------
395500020612     c                   eval      fcsfgs = fcwfgs                              <-------
395600020612     c                   eval      fcsndc = fcwndc                              <-------
395700020612     c                   eval      fcsddc = fcwddc                              <-------
395800020612     c                   eval      fcscce = fcwcce                              <-------
395900020612     c                   eval      fcsice = fcwice                              <-------
396000020612     c                   eval      fcst8a = fcwt8a                              <-------
396100020612     c                   eval      fcskgc = fcwkgc                              <-------
396200020612     c                   eval      fcskgr = fcwkgr                              <-------
396300020612     c                   eval      fcsaas = fcwaas                              <-------
396400020612     c                   eval      fcslnp = fcwlnp                              <-------
396500020612     c                   eval      fcsnrs = fcwnrs                              <-------
396600020612     c                   eval      fcsnsp = fcwnsp                              <-------
396700020611     C                   write     fifcsw00                                     <-------
396800020612      *------                                                                   <-------
396900020611     C                   endsr                                                  <-------
397000081008      *****************************************************************
397100081008      * valore presunto costo carburante
397200081008      **********************************************************************
397300081008     C     caricofuel    BEGSR
397400130404      *
397500081008     c                   clear                   ficn74ds
397600081008      * totale importo Costo Autista x la giornata
397700081008     c                   eval      i74imp = fttitt + fttita + ftttpe +
397800081008     c                                      ftttbn + ftttim + fttmnt
397900081008     c                   z-add     Fttpdr        i74pdr
398000081008     c                   z-add     Fttddc        i74dtpre
398100081008     c                   move      'A'           i74tip
398200081008     c                   call      'FICN74R'
398300081008     c                   parm                    ficn74ds
398400081008      *
398500081008     c                   endsr
398600091106     c*--------------------------------------------------------------
398700091106     c* tramite Società e Unità decodifica P.IVA su PROJ
398800130404     c*--------------------------------------------------------------
398900091106     C     Decod_Rag_IVA BegSR
399000091106      **
399100091106      **  Routine x Reperire dati Fornitore:
399200091106      **    La routine in base alla sottonatura può funzionare
399300091106      **     x F=Fornitore/C=Cliente
399400091106     C                   clear                   trmz70s1ds                     Input
399500091106     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
399600091106     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
399700091106     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
399800091106     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
399900091106     c                   call      'TRMZ70SR1'
400000091106     C                   PARM                    trmz70s1ds                     Input
400100091106      *
400200130404     c                   clear                   TRS_Forn
400300130404     c                   clear                   TRS_Socie
400400130404     c                   clear                   TRS_Piva
400500091106     c                   if          S1_Trovato = *On and
400600091106     c                                S1_Errore = '0'
400700091106     c                   move      s1_KeyKSC     TRS_Forn
400800091106     c                   move      s1_Societa    TRS_Socie
400900130404     c                   movel     s1_PartitaIVA TRS_Piva
401000091106     c                   end
401100091106      *
401200091106     C                   ENDSR
401300091106     C/EJECT
401400130404     c*--------------------------------------------------------------
401500130404     c* tramite Società e Unità decodifica P.IVA su PROJ
401600130404     c*--------------------------------------------------------------
401700130404     C     Decod_P_IVA   BegSR
401800130404      **
401900130404      **  Routine x Reperire dati PIVA:
402000130404      **    La routine in base alla sottonatura può funzionare
402100130404      **     x F=Fornitore/C=Cliente
402200130404     C                   clear                   trmz70s4ds                     Input
402300130404     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
402400130404     C                   move      parm_S4CDF    s4_keyKSC                      Input
402500130404     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
402600130404     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
402700130404     c                   if        parm_S4FIL  > 0
402800130404     C                   movel(p)  parm_S4FIL    s4_Unita                       Input/Output
402900130404     c                   end
403000130404     c                   call      'TRMZ70SR4'
403100130404     c                   parm                    trmz70s4ds
403200130404      *
403300130404      *  se torna con l'errore riprova e aveva passato l'unità
403400130404      *   riprova passandogli l'unità vuota
403500130404     c                   if        s4_errore <> '0'
403600130404     C                   clear                   trmz70s4ds                     Input
403700130404     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
403800130404     C                   move      parm_S4CDF    s4_keyKSC                      Input
403900130404     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
404000130404     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
404100130404     c                   call      'TRMZ70SR4'
404200130404     c                   parm                    trmz70s4ds
404300130404     c                   end
404400130404      *
404500130404     C                   ENDSR
404600130404     C/EJECT
404700120821ab   C**************************************************************************
404800120821ab    * Controlla la Società operativa della Filiale
404900120821ab   C**************************************************************************
405000120821ab   C     CHK_SOC_OPE   begsr
405100120821ab   **
405200120821     c                   eval      soc_errata = NO
405300120821ab    /FREE
405400120821ab
405500120821EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
405600120821EDPDC                                             : dtVALIDITA
405700120821EDPDC                                             : 'O'
405800120821EDPDC                                             : valDatIniz
405900120821EDPDC                                             : valDatFine
406000120821ab                                                );
406100120821ab
406200120821EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
406300120821EDPDC    // Gestire l'errore.
406400120821
406500120821         eval  soc_errata = SI;
406600120821
406700120821ab     ENDIF;
406800120821ab
406900120821ab    /END-FREE
407000120821ab   C                   ENDSR
407100120821     C**************************************************************************
407200950531**
407300110928Nr.XXXXX  Distinta  ancora  da chiudere  con  data
407400950127Esistono ancora distinte chiuse da valorizzare con data
407500950127Esistono ancora spedizioni non partite con data ritiro
407600020322Non esistono autotrasp. da confermare nella selez.fatta
407700020618Attenzione: Presenti MANCA TARIFFA su distinte in data
407800951009Alcuni conteggi non caricati:in conferma da altri utent
407900000307Il totale conteggi è già stato calcolato:data distinta
408000090507Manca il Codice Fornitore sulle Tariffe dell'Autista
408100090507Manca il Codice Società   sulle Tariffe dell'Autista
408200020403Senza la tariffa firmata non si può fare il conteggio
408300020507Tariffe a prestazione/giornata senza data stampa sul
408400081008In Anagr.Aut. manca il peso della "Massa complessiva"
408500090512Attenzione: l'autista è stato disaccreditato.   In data
408600090513Senza accreditamento dell'autista non si può Confermare
408700120821Soc.Operativa non corrispondente in Tariffa  alla data
408800950531** MSG
408900020322ERRORE CONFERMA CONTEGGI AUTOTRASPORTATORI
409000020322Rilevato errore in conferma conteggi Padr.1234567 data dist.12345678
409100020328**
409200020328RGZPFM FILE(FIFCEW0F)
