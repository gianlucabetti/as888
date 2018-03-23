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
008800970415     D L1              S              3  0 DIM(30)                              FIL GESTITE
008900950127     D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
009000950127     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
009100950127     D DTS             S             15    DIM(30)                              DESCR.TIPI PREST
009200950127     D CTS             S              3    DIM(30)                              CDTAR TIPI PREST
009300950127     D T2R             S              1    DIM(30)                              TIPI PRESTAZ2
009400950127     D DT2             S             15    DIM(30)                              DESCR.TIPI PREST
009500950127     D CT2             S              3    DIM(30)                              CDTAR TIPI PREST
009600950127     D K18             S              1    DIM(18)                              SKI 18 ALFABETICA
009700950127     D K08             S              1    DIM(8)                               SKI 8  ALFABETICA
009800040306     D CMC             S              6    DIM(9999)                            MANC.CONS DA TASS
009900040306     D CBL             S              6    DIM(9999)                            CD.BOL DA NO TASS
010000970923     D*
010100120821     D ERR             S             55    DIM(15) CTDATA PERRCD(1)             MSG ERRORE
010200950531     D MSG             S             70    DIM(2) CTDATA PERRCD(1)              MSG PER SEDE
010300020328     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              COMANDI da eseguire
010400020320      *
010500020320     d si_B3R1         S              1
010600020403     d udatiso         s               d   Datfmt(*iso)
010700020320     d si_CN40         S              1
010800020918     d wfgs            S                   like(arbifp)
010900130321     d §t3pdrsav       S                   like(§t3pdr)
011000020328      *---------------------------------------------------------------------*
011100020328     D  MSGDS          S             80
011200020328     D  LENGH          S             15  5
011300020328      *---------------------------------------------------------------------*
011400121017     D ficng5ds      E DS
011500020402     D ficn70ds      E DS
011600091106     D trmz70s1ds    E DS                  prefix(S1_)
011700130404     D trmz70s4ds    E DS                  prefix(S4_)
011800130404     D parm_s4CDF      s                   like(tgtCDF)
011900130404     D parm_s4SOC      s                   like(tgtSOC)
012000130404     D parm_s4FIL      s                   like(tgtFIL)
012100130404     D TAR_PIVA        s                   like(trsIVA)
012200130404     D TAR_Forn        s                   like(tgtCDF)
012300130404     D TAR_Socie       s                   like(tgtSOC)
012400130404     D TRS_PIVA        s                   like(trsIVA)
012500130404     D TRS_Forn        s                   like(tgtCDF)
012600130404     D TRS_Socie       s                   like(tgtSOC)
012700020402     D daut          E DS
012800020516     D Dcre          E DS
012900020329     D DS8A          E DS
013000090507     D ds5Aaut       E DS
013100130318     D ficn5Ads      E DS
013200081008     D ficn74ds      E DS
013300020320      *
013400950127     D KPJBA         E DS
013500020701     D* REM                    1      3
013600020701     D* REMFIL                 4      6
013700950127     D PARAM           DS
013800950127     D  PARSML               256    256
013900020125     D* PASSAGGIO DATI  A CONTEGGI PADRONCINI            - FICNB1R -
014000950127     D PARAM1          DS
014100950127     D  PA1PDR                 1      7  0
014200950127     D  PA1TSR                 8      8
014300950127     D  PA1DDC                 9     16  0
014400950127     D  PA1SML               256    256
014500011113     D* PASSAGGIO DATI  A SVALORIZZAZ. FILE SIMULAZIONE  - FICND9R -
014600950127     D PARAM2          DS
014700950127     D  PA2PDR                 1      7  0
014800950127     D  PA2TSR                11     11
014900950127     D  PA2DDC                12     19  0
015000950127     D* DS PER TRUL06R - CARICAMENTO £X
015100950127     D DSUL06        E DS                  EXTNAME(TRUL06DS)
015200950127     D  LIN                    1     90  0
015300950127     D                                     DIM(30)                              FIL. COMODO
015400950127     D WLBDAT          DS
015500950127     D  G02DAT                 1      8  0
015600950127     D  G02INV                 9     16  0
015700950127     D  G02ERR                17     17
015800950127     D  G02TGI                18     22  0
015900950127     D                 DS
016000950127     D  AA                     1      2
016100950127     D  MM                     3      4
016200950127     D  GG                     5      6
016300950127     D  DATA                   1      6  0
016400950127     D                 DS
016500950127     D  VI1PF1                 1      3  0
016600950127     D  VI1PD1                 4      7  0
016700950127     D  VI1P1                  1      7  0
016800950127     D                 DS
016900950127     D  VI1PF2                 1      3  0
017000950127     D  VI1PD2                 4      7  0
017100950127     D  VI1P2                  1      7  0
017200020423     D                 DS
017300020423     D  ftdTSR                 1      1
017400020423     D  ARBCBO                 2      3
017500020423     D  W018C                  1      3
017600020423      *
017700020318     D ficn40ds      E DS
017800020423     D DS2Z          E DS
017900000711     D OG143         E DS
018000000711     D DS1Z          E DS
018100950221     D DS3A          E DS
018200020201     D FICNB2        E DS                  EXTNAME(FICNB2DS)
018300020320     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
018400020315     D FIcnB6        E DS                  EXTNAME(FICNB6DS)
018500950127     D CNCR80        E DS
018600950127     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018700950531     D                 DS
018800950531     D  MSGDET                 1    198
018900950531     D  MSPDR                 43     49
019000950531     D  MSDDC                 61     68
019100950801     D                 DS
019200950801     D  W01KSC                 1      7  0
019300950801     D  W01COD                 4      7  0
019400120821      *
019500120821      **------------------------------------------------------------**
019600120821EDPDCd SOC_errata      s              1A
019700120821     D NO              C                   CONST('N')
019800120821     D SI              C                   CONST('S')
019900120821      **------------------------------------------------------------**
020000021219      *
020100950127     D***
020200950127     D* DEFINIZIONE CONSTANTI
020300950127     D***
020400011113     D CMINIM          C                   CONST('M i n i m o:')
020500011113     D CTOTAL          C                   CONST('Totale:    ')
020600020520     D CIMPOR          C                   CONST('Var Corrisp')
020700950127     I/SPACE 3
020800950127     C****************************************************************
020900950127     C*  RIEPILOGO INDICATORI
021000950127     C***************************************************************
021100950127     C* 01    - IMMESSA SOLO DATA DAL
021200950127     C* 02    - IMMESSA DATA AL
021300950127     C* 03    - IMMESSA FILIALE DI APPARTENENZA
021400950127     C* 06    - FASE DI SIMULAZIONE
021500951025     C* 09    - ERRORE IN CONFERMA CONTEGGI:RECORD DI TOTALE ESISTE GIà
021600950127     C* 12    - EMISSIONE RECORD A GIORNATA IN SFL
021700950127     C* 15    - EMISSIONE A VIDEO BASE ACCESSORI PIK E BONUS
021800950127     C* 16    - RECORD DEL MINIMO
021900950127     C* 17    - EMISSIONE CAMPO TARIFFA GIORNATA
022000950127     C* 19    - EMETTO TOTALE BONUS
022100950127     C* 20    - RECROD DI TOTALE
022200950127     C* 30/32 - DI COMODO
022300950127     C* 35    - PULIZIA ES EMISSIONE SFL
022400001130     C* 37    - DI COMODO
022500020402     C* 38/45 - ERRORI
022600011207     C* 47    - ERRORE DIVISA
022700950406     C* 50    - ERRORE SFL:PRESENTE IMPORTO TOTALE = 0
022800020318     C* 51    - ERRORE SFL:manca tariffa o tariffa non confermata con data stampa
022900951009     C* 52/57 - ERRORI
023000950127     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
023100950127     C* 91    - RECORD DI FTT DA NON CARICARE
023200950127     C*****************************************************************
023300950127     C     INIZIO        TAG
023400950127     C* PULIZIA FORMATO 1
023500950127     C                   EXSR      PUL01
023600950127     C*
023700950127     C     FOR01         TAG
023800950127     C                   EXFMT     FRC0D01
023900950127     C                   SETOFF                                       444546
024000950615     C                   SETOFF                                       565554
024100951009     C                   SETOFF                                       535257
024200950127     C** CMD3 - FINE LAVORO
024300950127     C   KC              GOTO      FINE
024400950127     C* CONTROLLI FORMATO1
024500950127     C                   EXSR      CONTR1
024600950127     C   90              GOTO      FOR01
024700121017     C*  esegue controlli sulle FIRME
024800121017     C                   EXSR      Ctrl_FIRME
024900121017     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
025000121017     c                   if          devo_bloccare = 'S'
025100121017     C                   GOTO      FOR01
025200121017     c                   end
025300950127     C*
025400950127     C* CARICO DISTINTE SELEZIONATE
025500950127     C                   EXSR      CARNDC
025600020523     C   90              EXSR      DEL_FIFCW
025700951009     C   90              EXSR      SRWC01
025800950127     C   90              GOTO      FOR01
025900950127     C*
026000950127     C                   WRITE     FRC0D03
026100950127     C*
026200950127     C     FORCT2        TAG
026300950127     C                   EXFMT     FRC0CT2
026400020408     c                   z-add     nrult2        wrc2
026500970924     C                   SETOFF                                       57
026600950127     C** CMD03 - FINE
026700020327     C   KC              EXSR      DEL_FIFCW
026800020327     C   KC              EXSR      SRWC01
026900950127     C   KC              GOTO      FINE
027000950127     C*
027100950127     C** CMD12 - RITORNO
027200020327     C   KL              EXSR      DEL_FIFCW
027300950912     C   KL              EXSR      SRWC01
027400950127     C   KL              GOTO      FOR01
027500020318     C*
027600950127     C* SE PRESENTI MANCA TARIFFA NON POSSO FARE NULLA: USCIRE E CORREG
027700950127     C     WMTAR         IFEQ      '1'
027800950127     C                   SETON                                        40
027900950127     C                   GOTO      FORCT2
028000950127     C                   ENDIF
028100040506     C*
028200950127     C                   EXSR      CONTR2
028300011207     C   47
028400011207     Cor 50              GOTO      FORCT2
028500950127     C*
028600950127     C* CMD6 - CONFERMA VALORIZZAZIONE
028700950127     C     *INKF         IFEQ      *ON
028800950127     C* SOLO IN SIMULAZIONE CHIEDO SE SI VUOLE RIEFFETTUARE LA SIMULAZ.
028900950127     C     PARSML        IFNE      ' '
029000950127     C                   MOVEL     'NO'          VIDSIM
029100950127     C                   EXFMT     FRC0D04
029200950127     C                   ENDIF
029300950127     C*
029400020329     C*  Conferma i Conteggi e crea il Workfile per il Conto Economico
029500020329     C*   se la Conferma ha ulteriori valori da ripartire.
029600950127     C                   EXSR      CONVAL
029700020329     C* Dopo aver convalidato e creato il workfile con i valori di C/Econ
029800020329     C*  le scrive aggiornando le VOCI sul FIFCE01L
029900020408     C     PARSML        IFeq      ' '
030000020329     C                   EXSR      AGG_VOCI
030100070523     c                   commit
030200020408     c                   end
030300020329     C*  quindi pulisce il Workfile utilizzato
030400020718     c                   move      'S'           F6Fine            1
030500020329     C                   EXSR      DEL_FIFCW
030600020718     c                   move      ' '           F6Fine
030700020329     C*  e libera il programma
030800950912     C                   EXSR      SRWC01
030900950127     C                   GOTO      INIZIO
031000950127     C                   ENDIF
031100020408      *
031200020408     c                   commit
031300950127     C* ENTER
031400950127     C                   GOTO      FORCT2
031500950127     C*
031600950127     C     FINE          TAG
031700030430     C*
031800030430     C*  deve cancellare il record di allocazione x impedire le trasmissioni
031900030430     C*   in sede durante l'utilizzo di questo programma.
032000030513     c     parsml        ifeq      *blank
032100030430     C     Kwalc         setll     fiwalc1l
032200030430     C     Kwalc         reade     fiwalc1l
032300030430     c                   dow       not %eof(fiwalc1l)
032400030430     c                   delete    fiwalc00
032500030430     C     Kwalc         reade     fiwalc1l
032600030430     c                   end
032700030513     c                   end
032800020408      *
032900020408     c                   commit
033000020320     C*
033100020320     c                   if        si_B3R1='S'
033200020320     C                   clear                   ficnb3
033300020320     C                   movel     'C'           §t3tla
033400020320     C                   CALL      'FICNB3R1'
033500020320     C                   PARM                    ficnb3
033600020320     c                   end
033700020320     C*
033800020320     c                   if        si_CN40='S'
033900020320     C                   clear                   ficn40ds
034000030124     C                   MOVEL     'A'           §40tip
034100020320     C                   MOVEL     'C'           §40tla
034200020320     C                   MOVEL     kpjbu         savpjbu
034300020320     C                   clear                   kpjbu
034400020320     C                   MOVEL     ficn40ds      kpjbu
034500020320     C                   call      'FICN40R'
034600020320     C                   parm                    kpjba
034700020320     C                   clear                   kpjbu
034800020320     C                   MOVEL     savpjbu       kpjbu
034900020320     c                   end
035000020328     C*
035100020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
035200020328     C*   economico.
035300020328     C                   clear                   FICNB2
035400020328     C                   move      'C'           §taflg
035500020328     C                   CALL      'FICNB2R'
035600020328     C                   PARM                    FICNB2
035700020328     C*
035800020523     c     parsml        ifeq      *blank
035900050208     c                   close     fifcew3l
036000020329     c                   close     fifcew1l
036100020523     c                   end
036200020328     C*
036300020523     c                   goto      no_rgz
036400020328     C                   clear                   MSGDS
036500020328     C                   MOVEL     CMD(1)        MSGDS
036600020328     C                   Z-ADD     80            LENGH
036700020328     C                   CALL      'QCMDEXC'                            99
036800020328     C                   PARM                    MSGDS
036900020328     C                   PARM                    LENGH
037000020523     c     no_rgz        tag
037100030428     C*
037200950127     C                   SETON                                        LR
037300950127     C****************************************************************
037400950127     C     *INZSR        BEGSR
037500950127     C     *ENTRY        PLIST
037600950127     C                   PARM                    KPJBA
037700950127     C                   MOVEL     KPJBU         PARAM
037800100527      *
037900100527     c     *dtaara       define    §azute        azuteds
038000100527     c     *dtaara       define    §datiute      ddatiute
038100100527     C                   in(E)     *dtaara
038200100527     C                   IF        %Error  or  RSUT = *blanks
038300100527     C                   call      'TIBS34R'
038400100527     C                   parm                    Tibs34Ds
038500100527     C                   in        *dtaara
038600100527     c                   ENDIF
038700100527      *
038800950127     C                   Z-ADD     1             CODUT
038900950127     C                   CALL      'X§PARUT'
039000950127     C                   PARM                    UT§DSE
039100950127     C                   MOVEL     RAGUT         RSUT             20
039200950127     C                   MOVEL     REC80         CNCR80
039300020320     C                   eval      si_B3R1 = 'N'
039400020320     c                   eval      si_CN40 = 'N'
039500020328     C*
039600030428     C* solo se non simulato
039700020523     c     parsml        ifeq      *blank
039800050208     c                   open      fifcew3l
039900020329     c                   open      fifcew1l
040000020523     c                   end
040100120821ab   C*     Inizializza
040200120821ab    /FREE
040300120821ab     TibsSof_Init();
040400120821ab    /END-FREE
040500950127     C*---------------------------------------------------------------*
040600030428     C     Kwalc         KLIST
040700030428     C                   KFLD                    WalPGM
040800030428     C                   KFLD                    WalNOJ
040900030428     C                   KFLD                    WalNUS
041000030428     C                   KFLD                    WalNRJ
041100950127     C* ACCESSO   TABEL
041200950127     C     KTAB2         KLIST
041300950127     C                   KFLD                    CODUT
041400950127     C                   KFLD                    COD               2
041500950127     C     KTAB          KLIST
041600950127     C                   KFLD                    CODUT
041700950127     C                   KFLD                    COD               2
041800950127     C                   KFLD                    KEY               8
041900000307     C     KFTT          KLIST
042000000307     C                   KFLD                    VH2PDR
042100000307     C                   KFLD                    KTSR
042200000307     C                   KFLD                    KNDC
042300000307     C                   KFLD                    FTTDDC
042400950127     C     KFTT2         KLIST
042500950127     C                   KFLD                    W01PDR
042600950127     C                   KFLD                    W01FVL
042700950127     C     KFTT3         KLIST
042800950127     C                   KFLD                    VH2PDR
042900950127     C                   KFLD                    W02FVL
043000950127     C                   KFLD                    W02DDC
043100950127     C                   KFLD                    TSR(C)
043200950127     C     KFTD          KLIST
043300950127     C                   KFLD                    VH2PDR
043400950127     C                   KFLD                    W01TSR
043500950127     C                   KFLD                    W01NDC
043600950127     C     KFTDS         KLIST
043700950127     C                   KFLD                    FTTPDR
043800950127     C                   KFLD                    FTTTSR
043900950127     C                   KFLD                    FTTNDC
044000950127     C                   KFLD                    FTTDDC
044100950127     C                   KFLD                    W01STP
044200950127     C     KFTDR         KLIST
044300950127     C                   KFLD                    FTTPDR
044400950127     C                   KFLD                    FTTTSR
044500950127     C                   KFLD                    FTTNDC
044600950127     C                   KFLD                    FTTDDC
044700020423      *
044800020423     C     kftd3         KLIST
044900020423     C                   KFLD                    ftdAAS
045000020423     C                   KFLD                    ftdLNP
045100020423     C                   KFLD                    ftdNRS
045200020423     C                   KFLD                    ftdNSP
045300950127     C     KFVV          KLIST
045400950127     C                   KFLD                    W01NPG
045500950127     C                   KFLD                    FTTDDC
045600020918     C                   KFLD                    vi1pf1
045700120517     C     KiDST         KLIST
045800120517     C                   KFLD                    dstNPG
045900120517     C                   KFLD                    dstNFV
046000120517     C                   KFLD                    dstFGS
046100001130     C     KBLP          KLIST
046200001130     C                   KFLD                    W01CPR
046300001130     C                   KFLD                    FTTDDC
046400950127     C     KBLP1         KLIST
046500950127     C                   KFLD                    FTDPDR
046600950127     C                   KFLD                    FTDDDC
046700950127     C                   KFLD                    FTDRSC
046800950127     C                   KFLD                    W01KSC
046900950127     C                   KFLD                    W01CCM
047000970924     C                   KFLD                    FTTFPP
047100950127     C                   KFLD                    W01NRT
047200950221     C     KARB          KLIST
047300950221     C                   KFLD                    FTDAAS
047400950221     C                   KFLD                    FTDLNP
047500950221     C                   KFLD                    FTDNRS
047600950221     C                   KFLD                    FTDNSP
047700020918     C     KARB78        KLIST
047800020918     C                   KFLD                    wfgs
047900020918     C                   KFLD                    wnum7
048000951011     C     KWC0          KLIST
048100951011     C                   KFLD                    PARSML
048200950912     C                   KFLD                    FTTPDR
048300950912     C                   KFLD                    FTTDDC
048400951011     C     KWC1          KLIST
048500951011     C                   KFLD                    KNMEB                          NOME JOB
048600951011     C                   KFLD                    KNMUS                          NOME USER
048700951011     C                   KFLD                    KNRJO                          NR.JOB
048800020423     C*
048900020423     C     Kfcw2T        KLIST
049000020423     c                   KFLD                    kfgsw
049100020423     c                   KFLD                    kpdrw
049200020423     c                   KFLD                    kddcw
049300020329     C*
049400020329     C     Kftd6         KLIST
049500020329     c                   KFLD                    fcwpdr
049600020329     c                   KFLD                    fcwddc
049700020329     c                   KFLD                    fcwtsr
049800020329     C*
049900020329     C     Kftd6_1       KLIST
050000020329     c                   KFLD                    fcwpdr
050100020329     c                   KFLD                    fcwddc
050200040426     C*
050300040426     C     Kftd6_zero    KLIST
050400040426     c                   KFLD                    vh2pdr
050500040426     c                   KFLD                    vh2ddc
050600020329     C*
050700020329     C     Kfre          KLIST
050800030123     c                   KFLD                    ktip
050900020329     c                   KFLD                    frepdr
051000020329     c                   KFLD                    freddc
051100030123     c                   move      'A'           ktip              1
051200020329      *
051300020329     C     Kfce          KLIST
051400020329     C                   KFLD                    fceFGS
051500020329     C                   KFLD                    fcePDR
051600020329     C                   KFLD                    fceTSR
051700020329     C                   KFLD                    fceNDC
051800020329     C                   KFLD                    fceDDC
051900020329     C                   KFLD                    fceAAS
052000020329     C                   KFLD                    fceLNP
052100020329     C                   KFLD                    fceNRS
052200020329     C                   KFLD                    fceNSP
052300020329     C                   KFLD                    fceCCE
052400020329      *
052500020329     C     KTbe          KLIST
052600020329     C                   KFLD                    tbeCOD
052700020329     C                   KFLD                    tbeKE1
052800020329     C                   KFLD                    tbeKE2
052900020329      *
053000020329     C     Kfcw1         KLIST
053100020329     C                   KFLD                    fcwFGS
053200020329     C                   KFLD                    fcwPDR
053300020329     C                   KFLD                    fcwTSR
053400020329     C                   KFLD                    fcwNDC
053500020329     C                   KFLD                    fcwDDC
053600020329     C                   KFLD                    fcwAAS
053700020329     C                   KFLD                    fcwLNP
053800020329     C                   KFLD                    fcwNRS
053900020329     C                   KFLD                    fcwNSP
054000020329     C                   KFLD                    fcwCCE
054100021202     c     Kapdv         Klist
054200021202     C                   kfld                    apdtip
054300021202     C                   kfld                    vi1p1
054400021202     c     Kapdf         Klist
054500021202     C                   kfld                    apdtip
054600021202     C                   kfld                    fttpdr
054700021202     c                   eval      apdtip = 'A'
054800020327     C*
054900090507     c     Ktgt          Klist
055000090507     C                   kfld                    fttpdr
055100090507     C                   kfld                    parsml
055200090512      *
055300090512     c     KTRA03        Klist
055400120911     C                   kfld                    fttPDR
055500090512     C                   kfld                    DATA_a0           8 0
055600090512     C                   z-add     0             DATA_a0
055700950127     C* CARICO TABELLE
055800950127     C***
055900950127     C* 06 ON - FASE DI SIMULAZIONE
056000950127     C***
056100950127     C     PARSML        IFEQ      'S'
056200950127     C                   SETON                                        06
056300950127     C                   ENDIF
056400950127     C***
056500950127     C* CARICO TABELLA FILIALI GESTITE £1
056600950127     C***
056700950127     C                   CLEAR                   DSUL06
056800950127     C                   MOVE      '£1'          D06COD
056900950127     C                   MOVEL     SIMFEL        D06KEY
057000950127     C                   MOVEL     ' '           D06TLA
057100950127     C                   MOVEL     DSUL06        KPJBU
057200950127     C                   CALL      'TRUL06R'
057300950127     C                   PARM                    KPJBA
057400950127     C                   MOVEL     KPJBU         DSUL06
057500950127     C                   MOVEA     LIN           L1
057600950127     C***
057700950127     C* VEDO SE SONO SIMFEL O REMOTO
057800950127     C***
057900020701     C*                  MOVEL     SIMFEL        CDU               3 0
058000950127     C*
058100020701     C*    REM           IFEQ      'REM'
058200020701     C*    REMFIL        ANDGT     *ZEROS
058300020701     C*                  MOVEL     REMFIL        CDU
058400020701     C*                  END
058500950127     C***
058600950127     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
058700950127     C***
058800950127     C                   CLEAR                   DSUL06
058900950127     C                   MOVE      '£6'          D06COD
059000020701     C*                  MOVEL     CDU           D06KEY
059100020701     C                   MOVEL     SimPOU        D06KEY
059200950127     C                   MOVEL     'L'           D06TLA
059300950127     C                   MOVEL     DSUL06        KPJBU
059400950127     C                   CALL      'TRUL06R'
059500950127     C                   PARM                    KPJBA
059600950127     C                   MOVEL     KPJBU         DSUL06
059700950127     C                   MOVEA     LIN           L6
059800020402     C*
059900950127     C* CARICO TABELLA DEI TIPI PRESTAZIONE
060000950127     C***
060100950127     C                   Z-ADD     1             C
060200950127     C                   Z-ADD     1             B
060300950127     C                   MOVEL     '1Z'          COD
060400950127     C     KTAB2         SETLL     TABEL
060500950127     C     KTAB2         READE     TABEL                                  30
060600950127     C*
060700950127     C     *IN30         DOWEQ     *OFF
060800950127     C     TBLFLG        IFEQ      ' '
060900950127     C                   MOVEL     TBLUNI        DS1Z
061000950127     C     §1ZUPD        IFEQ      'S'
061100950127     C                   MOVEL     TBLKEY        TSR(C)
061200950127     C                   MOVEL     §1ZDES        DTS(C)
061300950127     C                   MOVEL     §1ZCTR        CTS(C)
061400950127     C                   ADD       1             C
061500950127     C                   ENDIF
061600950127     C                   MOVEL     TBLKEY        T2R(B)
061700950127     C                   MOVEL     §1ZDES        DT2(B)
061800950127     C                   MOVEL     §1ZCTR        CT2(B)
061900950127     C                   ADD       1             B
062000950127     C                   ENDIF
062100950127     C     KTAB2         READE     TABEL                                  30
062200950127     C                   ENDDO
062300020423     C***---------------------------
062400020423     C* CARICO MANCATE CONSEGNE DA TASSARE
062500020423     C***---------------------------
062600020423     C                   Z-ADD     1             CC
062700020423     C                   MOVEL     '8B'          COD
062800020423     C     KTAB2         SETLL     TABEL
062900020423     C     KTAB2         READE     TABEL                                  30
063000020423     C*
063100020423     C     *IN30         DOWEQ     *OFF
063200020423     C     TBLFLG        IFEQ      ' '
063300020423     C                   MOVEL     TBLKEY        CMC(CC)
063400020423     C                   MOVE      TBLKEY        WALF3             3
063500020423     C                   MOVE      WALF3         CMC(CC)
063600020423     C                   ADD       1             CC
063700020423     C                   ENDIF
063800020423     C     KTAB2         READE     TABEL                                  30
063900020423     C                   ENDDO
064000020403      *
064100020423     C***---------------------------
064200020423     C* CARICO I TIPI BOLLA DA NON TASSARE
064300020423     C***
064400040306     C                   Z-ADD     1             CC                5 0
064500020423     C                   MOVEL     '8C'          COD
064600020423     C     KTAB2         SETLL     TABEL
064700020423     C     KTAB2         READE     TABEL                                  30
064800020423     C*
064900020423     C     *IN30         DOWEQ     *OFF
065000020423     C     TBLFLG        IFEQ      ' '
065100020423     C                   MOVEL     TBLKEY        CBL(CC)
065200020423     C                   MOVE      TBLKEY        WALF3             3
065300020423     C                   MOVE      WALF3         CBL(CC)
065400020423     C                   ADD       1             CC
065500020423     C                   ENDIF
065600020423     C     KTAB2         READE     TABEL                                  30
065700020423     C                   ENDDO
065800020423     C*
065900020423     C***---------------------------
066000950127     C* GIRO DATA ODIERNA
066100950127     C***
066200950127     C                   TIME                    W0140            14 0
066300950127     C                   MOVE      W0140         UDATE8            8 0
066400950127     C                   Z-ADD     UDATE8        G02DAT
066500950127     C                   MOVE      *ZEROS        G02INV
066600950127     C                   MOVE      *BLANKS       G02ERR
066700950127     C                   CALL      'XSRDA8'
066800950127     C                   PARM                    WLBDAT
066900950127     C                   Z-ADD     G02INV        DATEU8            8 0
067000020403     C                   MOVE      g02inv        UDATiso
067100030428     C*
067200030428     C* scrive il record di allocazione x trasmissione
067300030428     c     parsml        ifeq      *blank
067400030428     C                   clear                   Fiwalc00
067500030428     C                   movel     w0140         walOrc
067600030428     C                   movel     g02inv        waldac
067700030428     C                   eval      WalSIF = KNSIF
067800030428     C                   eval      WalPGM = 'FICNB3R   '
067900030428     C                   eval      WalNOJ = KNMEB
068000030428     C                   eval      WalNUS = KNMUS
068100030428     C                   eval      WalNRJ = KNRJO
068200100527     C                   eval      WalFLD = 'Filiale   ' + %editc(UTEFIL:'X') +
068300100527     c                                      ' *'
068400030428     C                   write     Fiwalc00
068500030429     c                   feod      Fiwalc1L
068600030428     c                   end
068700950127     C***
068800950127     C* DEFINIZIONE CAMPI
068900950127     C***
069000020320     C     *like         define    kpjbu         savpjbu
069100011113     c     *like         define    vi2itt        vi2itx
069200000711     C     *LIKE         DEFINE    BLPLNP        SAVLNP
069300000711     C     *LIKE         DEFINE    TBLKEY        §KEY
069400950127     C     *LIKE         DEFINE    TBLCOD        §COD
069500950127     C     *LIKE         DEFINE    TBLKUT        §KUT
069600950127     C* AAMMGG DATA IMMESSA A VIDEO
069700950127     C     *LIKE         DEFINE    FVVNPG        W01NPG
069800950127     C     *LIKE         DEFINE    BLPPDR        W01CPR                          PADR LUNG 3
069900950127     C     *LIKE         DEFINE    FTTPDR        W01PDR                          PADR LUNG 7
070000950127     C     *LIKE         DEFINE    FTTPDR        W02PDR                          PADR LUNG 7
070100950127     C     *LIKE         DEFINE    FTTDDC        W01DDC
070200950127     C     *LIKE         DEFINE    FTTDDC        W02DDC
070300950127     C     *LIKE         DEFINE    FTTNDC        W01NDC
070400000307     C     *LIKE         DEFINE    FTTNDC        KNDC
070500000307     C     *LIKE         DEFINE    FTTTSR        KTSR
070600950127     C     *LIKE         DEFINE    FTTTSR        W01TSR
070700021217     C     *LIKE         DEFINE    fttTBS        w01TBS
070800950127     C     *LIKE         DEFINE    FTTTBN        W01TBN
070900950127     C     *LIKE         DEFINE    FTTTPE        W01TPE
071000950127     C     *LIKE         DEFINE    FTTITA        W01ITA
071100950127     C     *LIKE         DEFINE    FTTITA        DIFITA
071200950127     C     *LIKE         DEFINE    FTTITT        W01ITT
071300021218     C     *LIKE         define    fttITT        w01ran
071400950127     C     *LIKE         DEFINE    FTTITT        DIFITT
071500950127     C     *LIKE         DEFINE    FTTMNT        WMNT
071600020329     C     *LIKE         DEFINE    FTTMNT        WGIO
071700020329     C     *LIKE         DEFINE    vi2tpv        wret
071800950127     C     *LIKE         DEFINE    FTTSNA        W01SNA
071900950127     C     *LIKE         DEFINE    FTTKFA        W01KFA
072000950127     C     *LIKE         DEFINE    FTTCLA        W01CLA
072100950127     C     *LIKE         DEFINE    FTTCPE        W01CPE
072200950127     C     *LIKE         DEFINE    FTTTVL        W01TVL
072300950127     C     *LIKE         DEFINE    FTTSET        W01SET
072400950127     C     *LIKE         DEFINE    FTTSNT        W01SNT
072500950127     C     *LIKE         DEFINE    FTTSNP        W01SNP
072600950127     C     *LIKE         DEFINE    FTTSNE        W01SNE
072700950127     C     *LIKE         DEFINE    FTTSNP        W02SNP
072800950127     C     *LIKE         DEFINE    FTTSET        W02SET
072900950127     C     *LIKE         DEFINE    FTTSNT        W02SNT
073000950127     C     *LIKE         DEFINE    FTTSNE        W02SNE
073100950127     C     *LIKE         DEFINE    FTTFVL        W01FVL
073200950127     C     *LIKE         DEFINE    FTTFVL        W02FVL
073300950127     C     *LIKE         DEFINE    FTDSTP        W01STP
073400950127     C     *LIKE         DEFINE    FTTPEP        W01PPE
073500950127     C     *LIKE         DEFINE    FTTPEP        W02PPE
073600950127     C     *LIKE         DEFINE    NRR           W01NRR
073700950127     C     *LIKE         DEFINE    NRR           W02NRR
073800950127     C     *LIKE         DEFINE    BLPCCM        W01CCM
073900950127     C     *LIKE         DEFINE    BLPNRT        W01NRT
074000950912     C     *LIKE         DEFINE    FTTPDR        COMPDR
074100950912     C     *LIKE         DEFINE    FTTDDC        COMDDC
074200970415     C     *LIKE         DEFINE    FTDRSC        SAVRSC
074300970415     C     *LIKE         DEFINE    FTDKSC        SAVKSC
074400970415     C     *LIKE         DEFINE    FTDSET        SAVSET
074500020419     C     *LIKE         DEFINE    FTDSET        Sv1SET
074600970415     C     *LIKE         DEFINE    FTDSTP        SAVSTP
074700970924     C     *LIKE         DEFINE    FTDSTP        SAVSTO
074800970930     C     *LIKE         DEFINE    FTTSET        COMSET
074900020327     c     *like         define    fcwpdr        kpdrw
075000020327     c     *like         define    fcwfgs        kfgsw
075100020327     c     *like         define    fcwddc        kddcw
075200020329     C     *LIKE         DEFINE    fcwice        val_ice
075300020329     C     *LIKE         DEFINE    fcwice        resto_ice
075400950127     C                   Z-ADD     0             B                 2 0
075500950127     C                   Z-ADD     0             C                 2 0
075600950127     C                   Z-ADD     4             W01NPG
075700950127     C                   ENDSR
075800950127     C*
075900950127     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
076000020402     C     PUL01         BEGSR
076100950127     C                   CLEAR                   VI1DDC
076200950127     C                   CLEAR                   VI1P1
076300950127     C                   CLEAR                   VI1P2
076400020701     C*                  Z-ADD     CDU           VI1PF1
076500020701     C                   Z-ADD     SimPOU        VI1PF1
076600950127     C                   CLEAR                   W02NRR
076700950127     C                   ENDSR
076800121017     C* --------------------------------------------------------------
076900121017     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
077000121017     C* --------------------------------------------------------------
077100121017     C     CTRL_FIRME    BEGSR
077200121017     C*
077300121017     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
077400121017     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
077500121017     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
077600121017     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
077700121017     c                   move      'N'           devo_bloccare     1
077800121017     C                   MOVEL     kpjbu         savpjbu
077900121017     C*
078000121017      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
078100121017      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
078200121017      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
078300121017      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
078400121017      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
078500121017     C*
078600121017     C                   clear                   kpjbu
078700121017     C                   clear                   ficng5ds
078800121017     c                   eval      §G5CNVL  = 'V'
078900121017     c                   eval      §G5FIL   = VI1PF1
079000121017     C                   MOVEL     ficng5ds      kpjbu
079100121017     c                   call      'FICNG5R'
079200121017     C                   parm                    kpjba
079300121017     C                   MOVEL     kpjbu         ficng5ds
079400121017     C                   clear                   kpjbu
079500121017     C                   MOVEL     savpjbu       kpjbu
079600121017     C*
079700121017     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
079800121017     C*    e devono firmare prima tutti i documenti in sospeso
079900121017     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
080000121017     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
080100121017     c                   if        §G5ESITO ='B'
080200121017     c                   eval        devo_bloccare = 'S'
080300121017     c                   end
080400121017     C*
080500121017     C                   ENDSR
080600121017     C* --------------------------------------------------------------
080700121017     C*--- CONTROLLI FORMATO1 ----------------------------------------*
080800121017     C* --------------------------------------------------------------
080900121017     C     CONTR1        BEGSR
081000121017     C*
081100950127     C                   SETOFF                                       90
081200950127     C                   CLEAR                   W01DDC
081300950127     C*
081400950127     C* F I L I A L E   P A D R O N C I N O
081500950127     C     VI1PF1        IFEQ      0
081600950127     C                   SETON                                        3990
081700950127     C                   GOTO      ENDCT1
081800950127     C                   ELSE
081900950127     C* Verifico esistenza filiale padroncino: se REM su £6, se SIMFEL
082000950127     C* su £1
082100020701     C*    REM           IFEQ      'REM'
082200020701     C*    REMFIL        ANDGT     *ZEROS
082300020701     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
082400950127     C     VI1PF1        LOOKUP    L6                                     30
082500950127     C  N30              SETON                                        3990
082600950127     C  N30              GOTO      ENDCT1
082700950127     C                   END
082800950127     C                   END
082900020402      *
083000020402     C* Controllo autorizzazione all'utilizzo del programma
083100020402     c* Controllo se abilitato
083200020701     c*                  movel     cdu           i70pog
083300020701     c                   Z-ADD     SimPOU        i70pog
083400020701     c                   movel     vi1pf1        i70pge
083500020402     c                   movel     knmus         i70prf
083600020402     c                   call      'FICN70R'
083700020402     c                   parm                    ficn70ds
083800020402     c                   movel     o70uni        daut
083900020402     c* 98 indicatore che protegge tutto
084000020402     c     §autcon       ifne      'S'
084100020402     C                   SETON                                        3890
084200020402     C                   GOTO      ENDCT1
084300020402     C                   endif
084400020402      *
084500950127     C* P A D R O N C I N O   DAL AL
084600950127     C*
084700950127    1C     VI1PD2        IFGT      0
084800950127    2C     VI1PD2        IFLT      VI1PD1
084900950127     C                   SETON                                        4190
085000950127     C                   GOTO      ENDCT1
085100950127    2C                   ENDIF
085200950127     C*
085300950127   X1C                   ELSE
085400950127     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
085500950127    2C     VI1PD1        IFGT      0
085600950127     C*
085700021202     C     kapdv         CHAIN     FIAPD01L                           30
085800950127     C     *IN30         IFEQ      *ON
085900950127     C                   SETON                                        4290
086000950127     C                   GOTO      ENDCT1
086100950127    3C                   ENDIF
086200950127     C*é
086300950127    2C                   ENDIF
086400950127    1C                   ENDIF
086500950127     C*
086600950127     C* D A T A   D I S T I N T A
086700950127    1C     VI1DDC        IFGT      0
086800950127     C                   MOVE      VI1DDC        G02DAT
086900950127     C                   MOVEL     *BLANK        G02ERR
087000950127     C                   CALL      'XSRDA8'
087100950127     C                   PARM                    WLBDAT
087200950127    2C     G02ERR        IFEQ      '1'
087300950127     C                   SETON                                        43  90
087400950127     C                   GOTO      ENDCT1
087500950127    2C                   END
087600950127     C                   Z-ADD     G02INV        W01DDC
087700950127    1C                   ENDIF
087800950127     C*
087900950127     C  N90              MOVEL     VI1PF1        VI1PF2
088000950127     C*
088100950127     C     ENDCT1        ENDSR
088200950127     C*
088300950127     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
088400950127     C     CARNDC        BEGSR
088500950127     C                   SETOFF                                       0102
088600951009     C                   SETOFF                                       3157
088700020408     c                   z-add     1             wrc2
088800950127     C                   Z-ADD     0             NRR               5 0
088900950127     C                   Z-ADD     0             W01NRR
089000950127     C                   MOVEL     'V'           W01FVL
089100950127     C* PULIZIA CAMPI DI TOTALE
089200950127     C                   CLEAR                   WMTAR
089300950127     C                   CLEAR                   W01SNA
089400950127     C                   CLEAR                   W01KFA
089500950127     C                   CLEAR                   W01TVL
089600950127     C                   CLEAR                   W01CLA
089700950127     C                   CLEAR                   W01CPE
089800950127     C                   CLEAR                   W01ITA
089900950127     C                   CLEAR                   W01ITT
090000950127     C                   CLEAR                   W01SNP
090100950127     C                   CLEAR                   W01SET
090200950127     C                   CLEAR                   W01SNT
090300950127     C                   CLEAR                   W01SNE
090400950127     C                   CLEAR                   W01TBN
090500950127     C                   CLEAR                   W01TOT
090600950127     C                   CLEAR                   W01IGR
090700950127     C                   CLEAR                   W02SNP
090800950127     C                   CLEAR                   W02SET
090900950127     C                   CLEAR                   W02SNT
091000950127     C                   CLEAR                   W02SNE
091100950127     C                   CLEAR                   W01PPE
091200950127     C                   CLEAR                   W02PPE
091300021218     C                   clear                   w01TBS
091400021219     C                   clear                   w01ran
091500130404      *
091600130404     C                   clear                   §5aAUTISTA
091700950127     C* PULIZIA SUBFILE
091800950127     C                   SETON                                        35
091900950127     C                   WRITE     FRC0CT2
092000950127     C                   SETOFF                                       35
092100950127     C*
092200950127     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
092300950127     C* 02 ON - IMMESSI PADRONCINI DAL AL
092400950127     C*
092500950127     C* PADRONCINO
092600950127     C     VI1PD2        IFGT      0
092700950127     C     VI1P2         ANDNE     VI1P1
092800950127     C                   SETON                                        02
092900950127     C                   ELSE
093000950127     C*
093100950127     C     VI1PD1        IFGT      0
093200950127     C                   SETON                                        01
093300950127     C                   END
093400950127     C                   ENDIF
093500950127     C                   Z-ADD     VI1P1         W01PDR
093600950127     C**
093700950127     C* POSIZIONAMENTO PER LETTURA
093800950127     C**
093900950127     C     VI1PD1        IFGT      *ZEROS
094000011113     C     KFTT2         SETLL     FIFTT002
094100950127     C                   ELSE
094200950127     C                   EXSR      IMPO1
094300950127     C                   END
094400950127     C*
094500950127     C* LETTURA DISTINTE/RITIRI
094600950127     C     *IN31         IFEQ      *OFF
094700950127     C                   EXSR      LEGNDC
094800950127     C                   ELSE
094900950127     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
095000950127     C                   SETON                                        4490
095100950127     C                   MOVEL     ERR(4)        VI1ERR
095200950127     C                   ENDIF
095300950127     C*
095400950127     C                   ENDSR
095500950127     C*
095600950127     C* IMPOSTAZIONE LETTURA PER PADRONCINO DAL = 0 ------------------*
095700950127     C     IMPO1         BEGSR
095800011113     C     W01PDR        SETGT     FIFTT002
095900011113     C                   READ(N)   FIFTT002                               31
096000950202     C*
096100950202     C     *IN31         IFEQ      *OFF
096200950202     C                   MOVEL     FTTPDR        W03               3 0
096300950202     C     VI1PF1        IFNE      W03
096400950202     C                   SETON                                        31
096500950202     C                   END
096600950202     C                   END
096700950127     C*
096800950127     C     *IN31         IFEQ      *OFF
096900950127     C                   Z-ADD     FTTPDR        W01PDR
097000011113     C     KFTT2         SETLL     FIFTT002
097100950127     C                   ENDIF
097200950127     C                   ENDSR
097300950127     C*
097400950127     C* LETTURA DITINTE DA CARICARE ----------------------------------*
097500950127     C     LEGNDC        BEGSR
097600950127     C                   CLEAR                   W01TSR
097700950127     C                   CLEAR                   W02PDR
097800950127     C                   CLEAR                   W02DDC
097900950127     C                   CLEAR                   W02NRR
098000950912     C                   CLEAR                   COMPDR
098100950912     C                   CLEAR                   COMDDC
098200020328     C                   CLEAR                   comkgc            8 1
098300020328     C                   CLEAR                   comkgr            8 1
098400950127     C* L E T T U R A
098500950127     C     *IN31         DOWEQ     *OFF
098600950127     C                   SETOFF                                       9132
098700950127     C                   SELECT
098800950127     C** LEGGO PER PADRONCINO/FLAG VALORIZZ
098900950127     C     *IN01         WHENEQ    *ON
099000950127     C*
099100011113     C     KFTT2         READE(N)  FIFTT002                               31
099200950127     C*
099300950127     C*  LEGGO I PADRONCINI (TUTTI O NEL DAL/AL)
099400950127     C                   OTHER
099500950127     C*
099600950127     C     *IN32         DOWEQ     *OFF
099700011113     C     KFTT2         READE(N)  FIFTT002                               31
099800950127     C*
099900950127     C     *IN31         IFEQ      *ON
100000950127     C                   EXSR      IMPO1
100100950127     C   31              SETON                                        32
100200950127     C                   ELSE
100300950127     C                   SETON                                        32
100400950127     C                   ENDIF
100500950127     C                   ENDDO
100600950127     C*
100700950127     C                   ENDSL
100800950127     C*
100900950127     C* C O N F R O N T O   L  E   S E L E Z I O N I
101000950127     C  N31              EXSR      CONFR
101100950127     C*
101200950127     C* C A R I C A M E N T O   S F L
101300950127     C* 91 ON - RECORD DA NON SELEZIONARE
101400950127     C     *IN31         IFEQ      *OFF
101500950127     C     *IN91         ANDEQ     *OFF
101600950127     C                   EXSR      CARSF2
101700950127     C   90              SETON                                        31
101800950127     C                   ENDIF
101900950127     C                   ENDDO
102000950127     C*
102100950127     C* ULTIMA ROTTURA DI PADRONCINO
102200950509     C  N90W02PDR        IFNE      0
102300950127     C                   EXSR      ROTPAD
102400950127     C                   ENDIF
102500950127     C*
102600950127     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
102700950127     C  N90NRR           IFEQ      0
102800951009     C                   SETON                                        4490
102900951009     C                   MOVEL     ERR(4)        VI1ERR
103000950127     C                   ENDIF
103100020620     C* comunque deve impostare il numero di records  anche se c'è un errore
103200020620     C* altrimenti non esegue la DEL_FIFCW
103300020620     C                   Z-ADD     NRR           W01NRR
103400950127     C*
103500950127     C                   ENDSR
103600950127     C*
103700950127     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
103800950127     C     CONFR         BEGSR
103900950127     C* FLAG VALORIZZAZIONE
104000950127     C     FTTFVL        IFNE      W01FVL
104100950127     C                   SETON                                        91
104200950127     C                   GOTO      ENDCON
104300950127     C                   ENDIF
104400950127     C*
104500950127     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
104600950127     C   02FTTPDR        IFGT      VI1P2
104700950127     C                   SETON                                        3191
104800950127     C                   GOTO      ENDCON
104900950127     C                   ENDIF
105000950127     C* DATA DISTINTA
105100950127     C     VI1DDC        IFGT      0
105200950127     C     FTTDDC        ANDGT     W01DDC
105300950127     C                   SETON                                        91
105400950127     C                   GOTO      ENDCON
105500950127     C                   ENDIF
105600950127     C* SE TUTTO OK MA HA MANCA TARIFFA ERRORE
105700950127     C     FTTFLA        IFEQ      'S'
105800950127     C                   SETON                                        469031
105900020618     C                   SETON                                        45
106000950127     C                   MOVEL     FTTPDR        VI1PDE
106100020618     C                   move      fttddc        data
106200020618     C                   clear                   data6             6
106300020618     c                   eval      data6=gg+mm+aa
106400020618     C                   move      data6         vi1dde
106500950127     C                   MOVEL     ERR(5)        VI1ERR
106600950911     C                   GOTO      ENDCON
106700950127     C                   ENDIF
106800950911     C* SE TUTTO OK CONTROLLO CHE QUALCUN ALTRO NON STIA CONFERMANDO
106900950911     C* LO STESSSO PADRONCINO/DATA DISTINTA
107000950912     C     FTTPDR        IFNE      COMPDR
107100950912     C     FTTDDC        ORNE      COMDDC
107200951011     C                   MOVE      PARSML        WLRSML
107300951011     C                   MOVE      FTTPDR        WLRPDR
107400951011     C                   MOVE      FTTDDC        WLRDDC
107500951011     C                   MOVE      DATEU8        WLRIMM
107600951011     C                   MOVEL     KNMEB         WLRNMB
107700951011     C                   MOVEL     KNMUS         WLRNUS
107800951011     C                   Z-ADD     KNRJO         WLRNJO
107900951011     C                   WRITE     FNWLRC00                             30
108000950911     C  N30              MOVE      'N'           WEXIST            1
108100950911     C     *IN30         IFEQ      *ON
108200951011     C     KWC0          CHAIN     FNWLRC0F                           30
108300950911     C     *IN30         IFEQ      *OFF
108400951011     C     WLRIMM        ANDLT     DATEU8
108500951011     C                   Z-ADD     DATEU8        WLRIMM
108600951011     C                   MOVEL     KNMEB         WLRNMB
108700951011     C                   MOVEL     KNMUS         WLRNUS
108800951011     C                   Z-ADD     KNRJO         WLRNJO
108900951011     C                   UPDATE    FNWLRC00
109000950911     C                   MOVE      'N'           WEXIST
109100950911     C                   ELSE
109200000307     C* SE STESSO UTENTE/TERMINALE DO X SCONTATO CHE SI TRATTI DI UN
109300000307     C* LAVORO TERMINATO IN MODO ANOMALO
109400020701     C*    KNMUS         IFNE      WLRNUS
109500020701     C*    KNMTD         ORNE      WLRNMB
109600020701     C*    REM           OREQ      'EDP'
109700020701     C                   IF        SimUte <> WLRNUS
109800020701     C                             OR
109900020701     C                             KNmTD <> WLRNMB
110000020701     C                             OR
110100020701     C                             %SUBST(SimUte:1:3) = 'EDP'
110200950911     C                   MOVE      'E'           WEXIST
110300951011     C                   UNLOCK    FNWLRC0F
110400960930     C                   ELSE
110500960930     C                   Z-ADD     DATEU8        WLRIMM
110600960930     C                   MOVEL     KNMEB         WLRNMB
110700960930     C                   MOVEL     KNMUS         WLRNUS
110800960930     C                   Z-ADD     KNRJO         WLRNJO
110900960930     C                   UPDATE    FNWLRC00
111000960930     C                   MOVE      'N'           WEXIST
111100960930     C                   END
111200950911     C                   END
111300950911     C                   END
111400950912     C                   MOVE      FTTPDR        COMPDR
111500950912     C                   MOVE      FTTDDC        COMDDC
111600950912     C                   END
111700950912     C     WEXIST        IFEQ      'E'
111800951009     C  N57              MOVEL     ERR(6)        VCZMSG
111900951009     C  N57              MOVEL     ERR(6)        VI2ERR
112000951009     C                   SETON                                        9157
112100950912     C                   END
112200950127     C*
112300950127     C     ENDCON        ENDSR
112400950127     C*
112500950127     C* CARICO SUBFILE -----------------------------------------------*
112600950127     C     CARSF2        BEGSR
112700950127     C* TIPI RECORD DEL SUBFILE: "P" - PADRONCINO
112800950127     C*                          "S" - TIPO PRESTRAZIONE R O C
112900950127     C*                          "G" - TIPO PRESTAZIONE A GIORNATA
113000950127     C*                          "M" - MINIMO
113100950127     C*                          "T" - TOTALE
113200950127     C* CAMBIATO PADRONCINO
113300950127     C     FTTPDR        IFNE      W02PDR
113400120910      **
113500950127     C     W02PDR        IFNE      0
113600950127     C                   EXSR      ROTPAD
113700050620     c   90              goto      endcar
113800950127     C                   CLEAR                   W01TSR
113900950127     C                   CLEAR                   W02DDC
114000950127     C                   ENDIF
114100950127     C*
114200950127     C                   CLEAR                   FRC0SF2
114300950127     C*
114400021202     C     kapdf         CHAIN     FIAPD01L                           30
114500100223     C   30              CLEAR                   APDRSF
114600120911     C*******
114700120911     C**  controlli sulla Tariffa/Soc.Fornitore
114800120911     c                   exsr      SocFRN_Tariffa
114900120911     c   90              goto      endcar
115000120821     C*
115100090512     C*  se NON è accreditato l'autista e la tariffa è stata confermata
115200090512     C* occorre controllare il limite di giorni (preso dalla tabella 5aAUT)
115300090512     C*  entro i quali è possibile confermare altrimenti bloccare la Conferma
115400090512     C*
115500950127     C                   CLEAR                   K18
115600950127     C                   MOVEL     FTTPDR        WALF7             7
115700011113     c                   movea     walf7         k08
115800011113     c                   exsr      alfab
115900011113     c                   movea     k08           k18(1)
116000011113     c                   move      ' '           k18(8)
116100950127     C                   MOVEA     K18           VI2DES
116200950127     C                   MOVEL     FTTPDR        W02PDR
116300950127     C                   MOVEL     FTTPDR        VH2PDR
116400011113     c                   movel     fttdiv        vh2div
116500090507      *
116600950127     C* "P" TIPO RECORD PADRONCINO
116700950127     C                   MOVEL     'P'           VH2TRC
116800950127     C                   ADD       1             NRR
116900950127     C                   WRITE     FRC0SF2
117000011113      *
117100011113     C                   CLEAR                   K18
117200100223     C                   MOVEA     APDRSF        K18(1)
117300011113     C                   MOVEA     K18           VI2DES
117400011113     C* "P" TIPO RECORD PADRONCINO
117500020513     C*
117600020513     C                   CLEAR                   WflgR             1
117700020513     C                   CLEAR                   WflgC             1
117800020513     C*
117900011113     C                   MOVEL     'P'           VH2TRC
118000011113     C                   ADD       1             NRR
118100011113     C                   WRITE     FRC0SF2
118200011113      *
118300950127     C                   ENDIF
118400950127     C* DATA DISTINTA
118500950127     C     FTTDDC        IFNE      W02DDC
118600950127     C     W02DDC        IFNE      0
118700950127     C                   EXSR      ROTPAD
118800950127     C                   CLEAR                   W01TSR
118900950127     C                   ENDIF
119000120911     C*
119100120911     C**  controlli sulla Tariffa/Soc.Fornitore
119200120911     c                   exsr      SocFRN_Tariffa
119300120911     c   90              goto      endcar
119400950127     C*
119500950127     C* CONTROLLO SE ESISTONO ANCORA DELLE DISTINTE/RITIR DA VALORIZZAR
119600950127     C                   MOVEL     FTTDDC        W02DDC
119700950127     C                   EXSR      CTRDIS
119800950127     C                   ENDIF
119900950127     C* TIPO PRESTAZIONE
120000950127     C     FTTTSR        IFNE      W01TSR
120100950127     C     W01TSR        ANDNE     ' '
120200950127     C                   EXSR      ROTTSR
120300950127     C                   ENDIF
120400950127     C*
120500950127     C* SE C'E' SOMMA PESI A GIORNATA LA CALCOLO
120600950127     C     FTTCPE        IFGT      0
120700950127     C     FTTKFA        ORGT      0
120800950127     C                   Z-ADD     FTTNDC        W01NDC
120900950127     C                   ADD       FTTSNA        W01SNA                         TOT.STOP
121000950127     C                   ADD       FTTKFA        W01KFA                         TOT.PESO
121100020328     c     ftttsr        ifeq      'C'
121200020328     C                   add       FTTKFA        comkgc                         TOT.PESO
121300020328     C                   add       FTTKFA        vh2kgc
121400020507      * s/n pagare picking
121500020513     c                   if        vh2pgp <> 'S'
121600020507     C                   MOVE      fttpep        vh2pgp                         x picking
121700020513     c                   end
121800020328     c                   else
121900020328     C                   add       FTTKFA        comkgr                         TOT.PESO
122000020328     C                   add       FTTKFA        vh2kgr
122100020507      * s/n pagare etichettatura
122200020513     c                   if        vh2pge <> 'S'
122300020507     C                   MOVE      fttpep        vh2pge                         x etichetta
122400020513     c                   end
122500020328     c                   end
122600950127     C                   ADD       FTTTVL        W01TVL                         TOT.VOLUME
122700950127     C* Sommo tot.colli solo se pag.pik/etich.='S'
122800950127     C     FTTPEP        IFEQ      'S'
122900950127     C                   ADD       FTTCLA        W01CLA                         TOT.COLLI
123000950127     C                   ADD       FTTCPE        W01CPE                         T.COLLI PIK/ET.
123100950127     C                   ENDIF
123200950127     C                   ENDIF
123300030326     C*
123400030326     C* A prestazione: anche se solo la mattina o solo il pomeriggio
123500030326     C     FTTPEP        IFNE      'N'
123600030326     C                   MOVE      'S'           W01PPE
123700030326     C                   ENDIF
123800950127     C*
123900030326     C* A giornata: invece deve avere sia mattina che pomeriggio
124000950127     C     FTTPEP        IFNE      'S'
124100950127     C                   MOVE      'N'           W02PPE
124200950127     C                   ENDIF
124300950127     C*
124400950127     C                   ADD       FTTSET        W01SET
124500950127     C                   ADD       FTTSNT        W01SNT
124600950127     C                   ADD       FTTSNP        W01SNP
124700950127     C                   ADD       FTTSNE        W01SNE
124800950127     C                   ADD       FTTITT        W01ITT
124900950127     C                   ADD       FTTITA        W01ITA
125000950127     C                   MOVEL     FTTTSR        W01TSR
125100021218     c                   add       fttTBS        w01TBS
125200950127     C*
125300020329     C     endcar        ENDSR
125400950127     C* --------------------------------------------------------------*
125500120911     C*  Controllo da Tariffa la Società ed il Fornitore
125600120911     C* --------------------------------------------------------------*
125700120911     C     SocFRN_TariffaBEGSR
125800120911     C*
125900120911     c                   clear                   supertestata      1
126000120911     c                   clear                   accreditato       1
126100130404     c                   move      *all'0'       TAR_Forn
126200130404     c                   move      *all'0'       TAR_Socie
126300130404     c                   move      *blank        TAR_Piva
126400120911     C*
126500120911      *  ma li deve prendere assolutamente dalla Supertestata tariffe
126600120911     C     ktgt          setll     FItgt01L
126700120911     C     ktgt          reade     FItgt01L
126800120911     c                   dow       not %EoF(FItgt01L)
126900120911     C*  interno al periodo
127000120911     c                   if        FTTDDC >= tgtddt and FTTDDC<=tgtdst and
127100120911     c                             tgtATB  = ' '
127200120911     c                   move      'S'           supertestata
127300120911     c                   move      tgtcdf        vh2cdf
127400130404     c                   move      tgtcdf        TAR_Forn
127500120911     c                   move      tgtsoc        vh2soc
127600130404     c                   move      tgtsoc        TAR_Socie
127700130404      **  Recupera la P.IVA
127800130404      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
127900130404      *   a quella del contratto
128000130404     C                   move (p)  tgtCDF        parm_s4CDF                        Input
128100130404     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
128200130404     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
128300130404     c                   exsr      DECOD_P_IVA
128400130404      * se la P.IVA coincide
128500130404     c                   if        s4_errore ='0'
128600130404     c                   eval      TAR_Piva = s4_PARTITAIVA
128700130404     c                   end
128800130404      *
128900120911     c                   leave
129000120911     c                   end
129100120911     C     ktgt          reade     FItgt01L
129200120911     c                   enddo
129300120911      **
129400120911      **  Se Società o Fornitore non presenti in tariffa
129500120911      **    BLOCCARE ASSOLUTAMENTE la Valorizzazione !!
129600120911      **
129700120911     C* Controlla che ci sia il Codice Fornitore e il Sistema Inf.
129800120911     C*  altrimenti segala l'errore
129900120911     c                   if        (*in30=*Off and apdksc= 0 and
130000120911     c                             supertestata = ' ')
130100120911     c                             or
130200120911     c                             (*in30=*Off and vh2CDF= 0
130300120911     c                             and parsml=' ' and
130400120911     c                             supertestata = 'S')
130500120911     C                   SETON                                        4690
130600120911     C                   MOVEL     fttPDR        VI1PDE
130700120911     C                   MOVEL     ERR(8)        VI1ERR
130800120911     c                   goto      endsocfrn
130900120911     c                   end
131000120911     **
131100120911     c                   if        (*in30=*Off and apdcsf= *blanks and
131200120911     c                             supertestata = ' ')
131300120911     c                             or
131400120911     c                             (*in30=*Off and vh2SOC= *blanks
131500120911     c                             and parsml=' ' and
131600120911     c                             supertestata = 'S')
131700120911     C                   SETON                                        4690
131800120911     C                   MOVEL     fttPDR        VI1PDE
131900120911     C                   MOVEL     ERR(9)        VI1ERR
132000120911     c                   goto      endsocfrn
132100120911     c                   end
132200120911     C*
132300120911     C* Controlla se l'autista è stato disaccreditato ad una certa data.
132400120911     C*  Per cui deve valorizzare fino alla data disaccreditamento oltre
132500120911     C*  la quale invece deve bloccare la valorizzazione dando una
132600120911     C*  segnalazione a video.
132700120911     c                   clear                   vh2DFI
132800120911     c     Ktra03        chain     aitra03l
132900130325     C* ================:
133000120911     c                   if        %Found(aitra03l)
133100130325     C* ================:
133200120911     C* Se è accreditato:
133300120911     c                   eval      accreditato = 'S'
133400130325     C*
133500130325      * Se disaccreditato controlla se era sullo stesso fornitore della tariffa
133600130325     c     tranrc        chain     aitrs01l
133700130325     c                   if        %Found(aitrs01l)
133800130325      *
133900130325     C                   exsr      Decod_Rag_IVA
134000130325      *
134100130325     C*  Controlla se la P.IVA del contratto è quella della tariffa non Coincidono
134200130325     C*    altrimenti se le P.IVA non coincidono l'autista NON è accreditato.
134300130404     c                   if            TRS_Piva  <> TAR_Piva
134400130327      *
134500130404      *  Esegue il controllo su accreditamento e Ritorno Copia Firmata della tariffa
134600130404      *   solo se NON in simulazione e se è cambiato il codice del cliente trattato
134700130404     c                   exsr      CTRacrcf_TGT
134800130325      *
134900130325     c                   endIF
135000130325     c                   endIF
135100130325      *
135200130325     C* ================:
135300120911     c                   Else
135400130325     C* ================:
135500120911     C* Se non l'ha trovato o non esiste l'aut su AITRA oppure è disaccreditato
135600120911     c                   eval      accreditato = 'N'
135700120911     C*
135800120911     C* Quindi cerca con l'AUT se disaccreditato e....
135900120911     C* Se disaccreditato deve vedere l'ultimo disaccreditamento a quando risale
136000120911     C*  per controllare fino a quando può essere abilitato alla valorizzazione.
136100120911     c     fttpdr        setgt     aitra03l
136200120911     c     fttpdr        readpe    aitra03l
136300120911     C*
136400120911     C*  Posso valorizzare fino al giorno
136500120911     C*     della data disaccreditamento
136600120911     c                   if        not %EoF(aitra03l)
136700130325      *  Lo reimposta come default xchè lo ha trovato con DFI>0
136800130325     c                   eval      accreditato = 'S'
136900120911      *
137000130325      *  Se la DATA FINE accreditamento è INFERIORE del DOCUMENTO da valorizzare
137100120911     C                   if        traDFI < fttddc
137200130325      *
137300120911      * Solo se disaccreditato sullo stesso fornitore della tariffa
137400120911     c     tranrc        chain     aitrs01l
137500120911     c                   if        %Found(aitrs01l)
137600120911      *
137700120911     C                   exsr      Decod_Rag_IVA
137800130325      *
137900130325      *  Se la P.IVA corrisponde ALLORA è stato disaccreditato in data X
138000130404     c                   IF            TRS_Piva = TAR_Piva
138100130325      *
138200130325     c                   eval      accreditato = 'N'
138300130325     c                   eval      vh2DFI = traDFI
138400120911      *
138500130325      *  Se non corrisponde alla P.IVA del Fornitore in Tariffa
138600130404     c                   elseIF        TRS_Piva  <> TAR_Piva
138700130404      *
138800130404      *  Esegue il controllo su accreditamento e Ritorno Copia Firmata della tariffa
138900130404      *   solo se NON in simulazione e se è cambiato il codice del cliente trattato
139000130404     c                   exsr      CTRacrcf_TGT
139100130325      *
139200120911     c                   endIF
139300130327     c                   endIF
139400120911      *
139500120911     c                   end
139600120911     c                   end
139700130325     C* ================:
139800120911     c                   endIF
139900130325     C* ================:
140000120911     C*
140100120911     c                   setoFF                                       48
140200120911     c                   If         vh2DFI > 0
140300120911     C                   SETON                                        4890
140400120911     C                   MOVEL     fttPDR        VI1PDE
140500120911     C                   move      vh2DFI        data
140600120911     C                   clear                   data6             6
140700120911     c                   eval      data6=gg+mm+aa
140800120911     C                   move      data6         vi1dde
140900120911     C                   MOVEL     ERR(13)       VI1ERR
141000120911     c                   goto      endsocfrn
141100120911     c                   endIf
141200120911     C*
141300120911     C*  solo se NON simulate
141400120911     C                   if        parSML = ' '
141500120911     C* Controlla se la società Operativa è corretta per la valorizzazione
141600120911     C*   alla data della valorizzazione.
141700120911     C     *iso          move      FTTDDC        dtVALIDITA
141800120911     C                   MOVEL     FTTPDR        Filiale
141900120911     c                   exsr      chk_SOC_ope
142000120911      *
142100120911     C                   if        soc_errata = SI  or
142200120911     C                             TAR_socie  <> idSocieta
142300120911     C                   SETON                                        4890
142400120911     C                   move      FTTDDC        data
142500120911     C                   clear                   data6             6
142600120911     c                   eval      data6=gg+mm+aa
142700120911     C                   move      data6         vi1dde
142800120911     C                   MOVEL     fttPDR        VI1PDE
142900120911     C                   MOVEL     ERR(15)       VI1ERR
143000120911     c                   goto      endsocfrn
143100120911     C                   end
143200120911     C                   endIF
143300120911     C*
143400120911     C     endsocfrn     ENDSR
143500120911     C* --------------------------------------------------------------*
143600130404     C*   Controlla Accreditamento e Ritorno Copia Firmata da TGT
143700120911     C* --------------------------------------------------------------*
143800130404     C     CTRacrcf_TGT  BEGSR
143900130404      *
144000130404      * ci entra solo se è cambiato l'autista da testare
144100130404     c                   if        tgtPDR <> §5Aautista
144200130404     C                             AND parSML = ' '
144300130404      *
144400130404     c                   clear                   ficn5ads
144500130404     C                   MOVEL     kpjbu         savpjbu
144600130404     c                   z-add     tgtPDR        §5Aautista
144700130404     c                   z-add     tgtPRG        §5Aprgtar
144800130404     c                   movel     tgtFLR        DtgtFLR
144900130404     c                   movel     §tgtRIS       §5ARISTAMP
145000130404     C                   z-add     fttddc        §5ADATA1
145100130404     C                   z-add     tgtdts        §5ADATA2
145200130404     c                   eval      §5AWINDOW  = 'F'
145300130404     C                   clear                   kpjbu
145400130404     C                   MOVEL     ficn5ads      kpjbu
145500130404     C                   call      'FICN5AR'
145600130404     C                   parm                    kpjba
145700130404     C                   eval      ficn5ads = kpjbu
145800130404     C                   eval      ds5aAUT  = §5AUNIDS
145900130404     C                   clear                   kpjbu
146000130404     C                   MOVEL     savpjbu       kpjbu
146100130404     C*
146200130404     C*  Se non è stato ancora accreditato l'autista occorre bloccare dopo
146300130404     C*   "x" giorni --> presi nella tabella 5aAUT.
146400130404      ***    Solo se NON è stato forzato
146500130404     c                   IF        §5Aesito ='1'
146600130404      ***
146700130404     c                   IF        §5AGGDIFF  > 0   and
146800130404     c                             §5ATIPOGG  = '§5AGGVALDA'
146900130404      *  Non è accreditato
147000130404     c                   eval      accreditato = 'N'
147100130404     c                   eval      vh2DFI = traDFI
147200130404      *
147300130404     C                   SETON                                          90
147400130404      *** oppure
147500130404      ***   se è stata Ricevuta la tariffa FIRMATA dopo "x" giorni.
147600130404     c                   elseIF    §5AGGDIFF  > 0   and
147700130404     c                             §5ATIPOGG  = '§5AGGVALDR'
147800130404     c                             and §t3cnv = 0
147900130404     C                   SETON                                          90
148000130404     c                   endif
148100130404      ***
148200130404     c                   endif
148300130404      ***
148400130404     c                   endif
148500130404      *
148600130404     C                   ENDSR
148700130404     C* --------------------------------------------------------------*
148800130404     C*   Controlli su Distinte Aperte
148900130404     C* --------------------------------------------------------------*
149000130404     C     CTRDIS        BEGSR
149100950127     C                   MOVE      VH2PDR        W01CPR
149200950509     C                   SETOFF                                       90
149300950127     C*
149400950127     C* CONTROLLO SE ESISTONO -NELLA DATA- IN ARRIVO:
149500950127     C* - DELLE DISTINTE ANCORA DA CHIUDERE
149600120517     C* - DELLE DISTINTE SU FiDST CON PADRONCINO = A QUELLO
149700001130     C*   DELLA CONFERMA
149800020515      * e non Annullate
149900950127     C     KFVV          SETLL     FNFVV000
150000950127     C     KFVV          READE     FNFVV000                               30
150100950127     C*
150200950127    1C     *IN30         DOWEQ     *OFF
150300950127     C* CONTROLLO SOLTANTO GLI APERTI
150400950127    2C     FVVFCF        IFNE      'S'
150500020515    2C     FVVatb        andeq     *blank
150600001130      *
150700120517ab    *  sostituito il vecchio file delle distinte
150800120517ab   C                   z-add     fvvNPG        dstNPG
150900120517ab   C                   z-add     fvvNFV        dstNFV
151000120517ab   C                   z-add     fvvFGS        dstFGS
151100120517ab   C     KiDST         CHAIN     FiDST01L                           37
151200120517      *
151300001130     C     *IN37         IFEQ      *OFF
151400001130     C     DSTATB        ANDEQ     ' '
151500001130     C     DSTPDR        ANDEQ     VH2PDR
151600001130     C                   SETON                                        453090
151700001130     C                   MOVEL     ERR(1)        VI1ERR
151800110928     c                   eval      %subst(VI1ERR:4:5) = %editc(FVVNFV:'X')
151900001130     C                   ENDIF
152000001130      *
152100001130     C  N30              Z-ADD     FVVNFV        WNUM7             7 0
152200020918     C  N30              Z-ADD     FVVfgs        Wfgs
152300020918     C  N30karb78        CHAIN     FNARB000                           30
152400950207     C*
152500950207     C     *IN30         IFEQ      *OFF
152600950207     C     ARBPDC        ANDEQ     W01CPR
152700950207     C*
152800950127     C                   SETON                                        453090
152900950127     C                   MOVEL     ERR(1)        VI1ERR
153000110928     c                   eval      %subst(VI1ERR:4:5) = %editc(FVVNFV:'X')
153100950127    3C                   ENDIF
153200950127    2C                   ENDIF
153300950127     C*
153400950127     C  N90KFVV          READE     FNFVV000                               30
153500950127    1C                   ENDDO
153600950127     C*
153700950127     C*
153800950127    1C     *IN90         IFEQ      *OFF
153900950127     C* - DELLE DISTINTE CHIUSE MA NON VALORIZZATE
154000950127     C* DA VALORIZZARE
154100950127     C                   Z-ADD     1             C
154200950127    2C     TSR(C)        DOWNE     ' '
154300950127     C*
154400950127     C                   MOVEL     ' '           W02FVL
154500011113     C     KFTT3         SETLL     FIFTT002                               90
154600950127     C* BLOCCATE
154700950127     C  N90              MOVEL     'B'           W02FVL
154800011113     C  N90KFTT3         SETLL     FIFTT002                               90
154900950127    3C     *IN90         IFEQ      *ON
155000950127     C                   SETON                                            45
155100950127     C                   MOVEL     ERR(2)        VI1ERR
155200950127     C                   Z-ADD     19            C
155300950127    3C                   ENDIF
155400950127     C*
155500950127     C                   ADD       1             C
155600950127    2C                   ENDDO
155700950127     C*
155800950127    2C     *IN90         IFEQ      *OFF
155900950127     C* MI RIPOSIZIONO
156000950127     C                   MOVEL     'V'           W02FVL
156100950127     C                   Z-ADD     1             C
156200950127     C     FTTTSR        LOOKUP    TSR(C)                                 30
156300950127     C  N30              Z-ADD     1             C
156400011113     C     KFTT3         CHAIN(N)  FIFTT002                           30
156500950127     C*
156600950127     C* CONTROLLO SE ESISTONO -NELLA DATA- IN PARTENZA:
156700950127     C* - SPEDIZ.CON DATA RITIRO=DATA MA NON ANCORA PARTITE(CHIUS FV)
156800000711     C*  QUESTO CONTROLLO NON LO FACCIO PER BOL POSTE PERCHE'CE NE SONO
156900000711     C*  SEMPRE DI BOLLE MA QUANDO PARTONO VARIAMO LA DATA SPEDIZIONE
157000000711     C*
157100000711     C                   CLEAR                   SAVLNP
157200950127     C     KBLP          SETLL     FNBLP000
157300950127     C     KBLP          READE     FNBLP000                               30
157400950127    3C     *IN30         DOWEQ     *OFF
157500000711     C*
157600000711     C     BLPLNP        IFNE      SAVLNP
157700000711     C     BLPLNP        CHAIN     AZORG01L                           33
157800000711     C  N33              MOVEL     ORGDE3        OG143
157900000711     C   33              CLEAR                   OG143
158000000711     C                   MOVEL     BLPLNP        SAVLNP
158100000711     C                   ENDIF
158200000711     C**
158300020729     C     §OgNTW        IFne      'PPT'
158400950127     C     BLPFT1        IFEQ      ' '
158500950130     C                   SETON                                        459030
158600950127     C                   MOVEL     ERR(3)        VI1ERR
158700950127     C                   ELSE
158800950127     C     KBLP          READE     FNBLP000                               30
158900950127     C                   ENDIF
159000000711     C                   ELSE
159100000711     C* E ALMENO UNA BOLLA E POSTE, ESCO DAL GIRO
159200000711     C*
159300000711     C                   SETON                                            30
159400000711     C                   ENDIF
159500000711     C**
159600950127    3C                   ENDDO
159700950127    2C                   ENDIF
159800000307     C* CONTROLLO SE PER IL PADRONCINO/DATA DISTINTA ESISTE GIA' RECORD
159900000307     C* DI TOTALE
160000000307    2C     *IN90         IFEQ      *OFF
160100000307     C                   CLEAR                   KTSR
160200000307     C                   CLEAR                   KNDC
160300011113     C     KFTT          SETLL     FIFTT01L                               90
160400000307     C   90              SETON                                        45
160500000307     C   90              MOVEL     ERR(7)        VI1ERR
160600000307    2C                   ENDIF
160700950127    1C                   ENDIF
160800950127     C* GIRO LA DATA
160900950127     C     *IN90         IFEQ      *ON
161000950127     C                   MOVE      FTTDDC        DATA
161100950127     C                   MOVE      FTTDDC        VI1DDE
161200950127     C                   MOVEL     GG            VI1DDE
161300950127     C                   MOVE      AA            VI1DDE
161400950127     C                   MOVEL     VH2PDR        VI1PDE
161500950127     C                   ENDIF
161600950127     C*
161700950127     C                   ENDSR
161800950127     C*
161900950127     C* ROTTURA PER PADRONCINO O DATA --------------------------------*
162000950127     C     ROTPAD        BEGSR
162100950127     C                   CLEAR                   WMNT
162200020513     C*
162300950127     C* CALCOLO IL TIPO PRESTAZIONE
162400950127     C                   EXSR      ROTTSR
162500950127     C*
162600950127     C* CALCOLO IL TIPO PRESTAZ "G" GIORNATA
162700950127     C                   MOVEL     'G'           W01TSR
162800950127     C                   SETON                                        12
162900950127     C* TOTALE SPEDIZIONI NON FATTE DELLA GIORNATA
163000950127     C                   Z-ADD     W02SNP        W01SNP
163100950127     C* TOTALE SPEDIZIONI RITIR./CONS. DELLA GIORNATA
163200950127     C                   Z-ADD     W02SET        W01SET
163300950127     C* TOTALE SPEDIZIONI NON CONSEGNATE DELLA GIORNATA
163400950127     C                   Z-ADD     W02SNT        W01SNT
163500950127     C* TOTALE SPEDIZIONI NON CONS/NON PAG DELLA GIORNATA
163600950127     C                   Z-ADD     W02SNE        W01SNE
163700950127     C*
163800950127     C                   EXSR      ROTTSR
163900950127     C*
164000950127     C                   CLEAR                   W02SNP
164100950127     C                   CLEAR                   W02SET
164200950127     C                   CLEAR                   W02SNT
164300950127     C                   CLEAR                   W02SNE
164400950127     C*
164500950127     C*  T O T P A D = TOTALI TARIFFA + TARIFFA A GIORNATA
164600011207     C     W01TOT        ADD       W01IGR        TOTPAD           12 3
164700950127     C*
164800950127     C* ESPOSIZIONE DEL   M I N I M O   A   G I O R N A T A
164900950127     C                   EXSR      PULSFL
165000950127     C     §TAMNT        IFGT      0
165100950127     C                   Z-ADD     §TAMNT        WMNT
165200950127     C                   EXSR      PULSFL
165300950127     C                   Z-ADD     §TAMNT        VI2IGR                          MINIMO
165400950127     C                   MOVE      CMINIM        VI2DES
165500950127     C* "M"= TIPO RECORD DEL MINIMO
165600950127     C                   MOVEL     'M'           VH2TRC
165700950127     C                   ADD       1             NRR
165800950127     C                   SETON                                        1716
165900950127     C* ESPONGO IL TOTALE BONUS
166000950127     C     W01TBN        IFGT      0
166100950127     C                   Z-ADD     W01TBN        VI2TBV                          TOT BONUS
166200950127     C                   SETON                                        19
166300950127     C                   ENDIF
166400950127     C*
166500950127     C                   WRITE     FRC0SF2
166600950127     C                   SETOFF                                       171916
166700950127     C                   ENDIF
166800950127     C*
166900950127     C* ESPOSIZIONE  T O T A L E
167000950127     C                   EXSR      PULSFL
167100950127     C*
167200950127     C                   MOVEL     'T'           VH2TRC                          TIPO RECORD
167300021211     C     apdhpa        add       apdhar        vi2HLV
167400950127     C                   MOVE      CTOTAL        VI2DES
167500011113     c                   move      vh2div        VI2DES
167600950127     C                   MOVEL     W02DDC        VH2DDC                          DATA DISTIN
167700021219     c                   z-add     w01ran        vh2ran
167800030107     c                   z-add     w01TBS        vh2TBS
167900950127     C* TOTALE PADRONCINO
168000950127     C                   Z-ADD     TOTPAD        VI2IGR
168100950127     C*
168200950127     C* SALVO "TOTALE TARIFFA"     IN VI2ITT
168300950127     C                   Z-ADD     W01TOT        VI2ITT
168400950127     C*
168500950127     C* SALVO "TARIFFA GIORNATA"   IN VI2ITA
168600950127     C                   Z-ADD     W01IGR        VI2ITA
168700950127     C*
168800950127     C* SALVO "TOTALE BONUS"       IN VI2TVB
168900950127     C                   Z-ADD     W01TBN        VI2TBV                          TOT BONUS
169000950127     C* IMPORTO AGGIUNTIVO
169100950127     C                   MOVEL     CIMPOR        VI2ATT
169200950127     C                   Z-ADD     WIAD          VI2TPV
169300020318     C                   Z-ADD     WIAD          vh2IAD
169400950127     C*
169500020318     C* aggiunge all'importo addizionale eventuali altre rettifiche
169600030123     C* apportate con apposito file FIFRE02L
169700020408     C     PARSML        IFeq      ' '
169800020320     c                   eval      si_CN40='S'
169900020320     C                   clear                   ficn40ds
170000030128     C                   MOVEL     'A'           §40tip
170100020320     C                   MOVEL     'I'           §40FLG
170200020320     C                   MOVEL     VH2PDR        §40PDR
170300020320     C                   MOVEL     VH2DDC        §40DDC
170400020320     C                   MOVEL     kpjbu         savpjbu
170500020320     C                   clear                   kpjbu
170600020320     C                   MOVEL     ficn40ds      kpjbu
170700020320     C                   call      'FICN40R'
170800020320     C                   parm                    kpjba
170900020320     C                   MOVEL     kpjbu         ficn40ds
171000020320     C                   clear                   kpjbu
171100020320     C                   MOVEL     savpjbu       kpjbu
171200020320     C                   add       §40tim        VI2TPV
171300020320     C                   z-add     §40tim        vh2RET
171400020408     c                   endif
171500020318     C*
171600950127     C* PREPARA DATI X VISUALIZZARE RECROD TOTALE
171700950127     C                   EXSR      VISTOT
171800950127     C*
171900950127     C                   ADD       1             NRR
172000950127     C                   WRITE     FRC0SF2
172100950127     C                   SETOFF                                       151720
172200950406     C                   SETOFF                                       1922
172300950127     C*
172400020328     C                   clear                   comkgc
172500020328     C                   clear                   comkgr
172600020328     C                   clear                   vh2kgc
172700020328     C                   clear                   vh2kgr
172800020507     C                   clear                   vh2pge
172900020507     C                   clear                   vh2pgp
173000020328     C*
173100950127     C                   CLEAR                   W01TOT
173200950127     C                   CLEAR                   W01IGR
173300950127     C                   CLEAR                   W01TBN
173400021219     C                   clear                   w01ran
173500020322     C                   CLEAR                   WIAD             10 3
173600950127     C                   ENDSR
173700950127     C*
173800950127     C* PULIZIA SUBFILE ----------------------------------------------*
173900950127     C     PULSFL        BEGSR
174000950127     C* NON PULISCO:
174100950127     C* 1)  VH2PDR/VH2APP PERCHE' RIMANGONO SEMPRE IMPOSTATI
174200950127     C* 2)  VH2TRC/VH2FGR PERCHE' LI REIMPOSTO SEMPRE
174300950127     C                   CLEAR                   VI2FGR
174400950127     C                   CLEAR                   VI2IGR
174500950127     C                   CLEAR                   VI2ITT
174600950127     C                   CLEAR                   VI2ITA
174700950127     C                   CLEAR                   VI2TPV
174800020318     C                   clear                   vh2IAD
174900020507     C                   clear                   vh2TGP
175000020507     C                   clear                   vh2TGE
175100020318     C                   clear                   vh2RET
175200950127     C                   CLEAR                   VI2TBV
175300950127     C                   CLEAR                   VI2TOT
175400950127     C                   CLEAR                   VI2DES
175500950127     C                   CLEAR                   VH2DDC
175600950127     C                   CLEAR                   VI2SCE
175700021217     C                   clear                   vh2TBS
175800021219     C                   clear                   vh2RAN
175900130403      *
176000950127     C                   ENDSR
176100950127     C*
176200950127     C* ROTTURA PER TIPO PRESTAZIONE ---------------------------------*
176300950127     C     ROTTSR        BEGSR
176400950127     C*
176500950127    1C     W02NRR        IFEQ      0
176600950127     C                   EXSR      PULSFL
176700950127    1C                   ENDIF
176800020513     C*
176900020513    1C     W01tsr        IFeq      'R'
177000020513     c                   move      'S'           WflgR
177100020513     c                   end
177200020513    1C     W01tsr        IFeq      'C'
177300020513     c                   move      'S'           WflgC
177400020513     c                   end
177500020513     C*
177600950127     C* CALCOLO LA   S O M M A   P E S I   A   G I O R N A T A  SE C'E'
177700950127    1C     W01CPE        IFGT      0
177800950127    1C     W01KFA        ORGT      0
177900020201     C                   CLEAR                   FICNB2
178000020426     C                   MOVEL     'CONFERM'     §TApgm
178100020326     C                   MOVEL     VH2PDR        §TAfgs
178200020326     C                   MOVEL     VH2PDR        §TAPDR
178300011113     c                   movel     vh2div        §tadiv
178400950127     C                   MOVEL     W02DDC        §TADDC
178500020326     C                   z-add     w01ndc        §tandc
178600950127     C                   MOVEL     W01SNA        §TASPP
178700950127     C                   MOVEL     W01KFA        §TAKFA
178800950127     C     W01KFA        IFGT      *ZEROS
178900950127     C                   MOVEL     'S'           §TAPAG
179000950127     C                   ENDIF
179100950127     C                   Z-ADD     W01CLA        §TANCL
179200950127     C                   Z-ADD     W01CPE        §TACPE
179300950127     C                   MOVEL     'S'           §TAPPE
179400950127     C                   MOVEL     W01TVL        §TATVL
179500950127     C                   MOVEL     'IT100'       §TACAP
179600950127     C                   MOVEL     W01TSR        §TATSR
179700950127     C*
179800020320     C* Routine esterna per restituire il codice tariffa del documento
179900020320     C                   clear                   ficnb3
180000020320     C                   eval      si_B3R1 = 'S'
180100020320     C                   movel     §tapdr        §t3pdr
180200020603     C                   movel     parsml        §t3sml
180300020320     C                   movel     'V'           §t3fvl
180400020320     C                   movel     §tatsr        §t3tsr
180500020320     C                   z-add     w01ndc        §t3ndc
180600020320     C                   z-add     §taddc        §t3ddc
180700020320     C                   movel     §tadiv        §t3div
180800020320     C                   movel     *blank        §t3ctr
180900020320     C                   CALL      'FICNB3R1'
181000020320     C                   PARM                    ficnb3
181100020406      *
181200020406      *  Attenzione se non c'è la tariffa deve saltare subito alla
181300020406      *   chiamata per prestazione 999 .
181400020406      *   *-----------------------------------------------*
181500020406     c     §t3ctr        cabeq     *all'?'       a_prestaz
181600020406      *   *-----------------------------------------------*
181700020406      *
181800020320     C                   movel     §t3ctr        §TACTR
181900020403     C*
182000130318     -* ******** se questa data non è stata convalidata possono passare
182100130318     -* ********           (20)giorni prima erano (7) oltre i quali
182200130318     -* ********           NON è più possibile eseguire il conteggio.
182300130318      *** --
182400130318      ***  aggancia tabella 5aAUT di controllo giorni limite oltre i quali
182500130318      ***   NON si può andare x eseguire i Conteggi x l'Autista.
182600130318      **  x  la Ricezione Tariffa Firmata
182700130318      **     non passa la prima data poichè la UDATE da cui calcolare
182800130318      *
182900130321      * Solo se è cambiato l'autista
183000130318     c                   if        §t3sta > 0 and PARSML =' '
183100130321     c                             and §t3pdr <> §t3pdrsav
183200130404      *
183300130404      * ci entra solo se è cambiato l'autista da testare
183400130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
183500130404      ****  della Tariffa Firmata
183600130404     c                   exsr      CTRacrcf_§T3
183700130321      *
183800020403     c                   end
183900090513     C*
184000020402     c                   z-add     comkgc        §TAkgc
184100020402     c                   z-add     comkgr        §TAkgr
184200020402     C*
184300950127     C                   MOVEL     *HIVAL        §TAITA
184400950127     C                   MOVEL     ' '           §TAFLG
184500020311     C                   MOVEL     'P'           §TATSM
184600021217      * Ritiri Annullati
184700021217     C                   z-add     vh2TBS        §taTBS
184800021217     C*
184900950127     C                   MOVEL     PARSML        §TASML
185000950127     C*
185100020201     C                   CALL      'FICNB2R'
185200020201     C                   PARM                    FICNB2
185300950127     C*
185400950127    2C     §TAERR        IFEQ      ' '
185500950127     C                   ADD       §TAITT        W01ITT
185600950127     C                   ADD       §TATPE        W01ITT
185700950127     C                   ADD       §TATST        W01ITT
185800020403     C                   ADD       vh2iad        w01itt
185900950127    3C     W02NRR        IFGT      0
186000950127     C     W01ITT        SUB       VI2ITT        DIFITT
186100950127    3C                   ENDIF
186200021219     C                   ADD       vh2ran        w01ran
186300950127     C*
186400950127   X2C                   ELSE
186500950127     C*
186600950127     C* MANCA TARIFFA --> NON POSSO CONFERMARE I CONTEGGI
186700950127     C                   MOVEL     'E'           VI2SCE
186800950127     C                   MOVEL     '1'           WMTAR             1
186900950127    2C                   ENDIF
187000950127     C                   CLEAR                   W01SNA
187100950127     C                   CLEAR                   W01KFA
187200950127     C                   CLEAR                   W01TVL
187300950127     C                   CLEAR                   W01CLA
187400950127     C                   CLEAR                   W01CPE
187500950127    1C                   ENDIF
187600950127     C*
187700020406     C*  *----------------------*
187800020406     c     a_prestaz     tag
187900020406     C*  *----------------------*
188000020406     C*    occorre fare il controllo della tariffa se esiste e se è stata
188100020406     C*     convalidata .
188200020406     C*
188300950127     C* T A S S A Z I O N E   A LIVELLO DI  T I P O   P R E S T A Z
188400950127     C*
188500950127     C* SE NON E' UN RICALCOLO (W02NRR=0) ESEGUO TASSAZIONE
188600950127    1C     W02NRR        IFEQ      0
188700020201     C                   CLEAR                   FICNB2
188800950127     C*
188900020426     C                   MOVEL     'CONFERM'     §TApgm
189000020402     c                   z-add     comkgc        §TAkgc
189100020402     c                   z-add     comkgr        §TAkgr
189200020312     c                   if        w01tsr = 'G'
189300020312     c                   movel     'G'           §TATSM
189400020312     c                   else
189500020312     C                   MOVEL     'P'           §TATSM
189600020312     c                   end
189700020311     C                   MOVEL     *all'9'       §TACTR
189800020311     C*
189900950127     C                   MOVEL     W02PDR        §TAPDR
190000020326     C                   MOVEL     W02PDR        §TAfgs
190100011113     c                   movel     vh2div        §tadiv
190200950127     C                   MOVEL     W02DDC        §TADDC
190300950127     C                   MOVEL     W01SET        §TASET
190400950127     C                   MOVEL     W01SNP        §TASNP
190500950127     C                   MOVEL     W01SNT        §TASNT
190600950127     C                   MOVEL     W01SNE        §TASNE
190700950127     C                   MOVEL     W01TSR        §TATSR
190800950127     C                   MOVEL     'S'           §TAFLG
190900950127     C                   MOVEL     PARSML        §TASML
191000021217     C*
191100021217     C                   z-add     w01TBS        §taTBS
191200020408     C*
191300020408     C* Routine esterna per controllare i giorni per poter confermare
191400020408     C                   clear                   ficnb3
191500020408     C                   eval      si_B3R1 = 'S'
191600020408     C                   movel     §tapdr        §t3pdr
191700020603     C                   movel     parsml        §t3sml
191800020408     C                   movel     'V'           §t3fvl
191900020408     C                   movel     §tatsr        §t3tsr
192000020408     C                   z-add     0             §t3ndc
192100020408     C                   z-add     §taddc        §t3ddc
192200020408     C                   movel     §tadiv        §t3div
192300020408     C                   movel     §tactr        §t3ctr
192400020408     C                   CALL      'FICNB3R1'
192500020408     C                   PARM                    ficnb3
192600020507      *
192700020507      *  Attenzione c'è la tariffa ma senza data di stampa
192800020507     c     §t3ctr        ifeq      *all'?'
192900020507     c     §t3err        andeq     'E'
193000020507     C                   SETON                                        4690
193100020507     C                   MOVEL     apdPDR        VI1PDE
193200020507     C                   MOVEL     ERR(11)       VI1ERR
193300020507     c                   end
193400020408      *
193500020408      *  Attenzione se non c'è la tariffa non deve fare il controllo
193600020408      *   *-----------------------------------------------*
193700020408     c     §t3ctr        ifne      *all'?'
193800020408      *   *-----------------------------------------------*
193900130318      *** --
194000130318      ***  aggancia tabella 5aAUT di controllo giorni limite oltre i quali
194100130318      ***   NON si può andare x eseguire i Conteggi x l'Autista.
194200130318      **  x  la Ricezione Tariffa Firmata
194300130318      **     non passa la prima data poichè la UDATE da cui calcolare
194400130318      *
194500130318     c                   if        §t3sta > 0 and PARSML =' '
194600130321     c                             and §t3pdr <> §t3pdrsav
194700130404      *
194800130404      * ci entra solo se è cambiato l'autista da testare
194900130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
195000130404      ****  della Tariffa Firmata
195100130404     c                   exsr      CTRacrcf_§T3
195200130404      *
195300130321     c                   end
195400130318     c                   end
195500130318     C*
195600030326     C* TROVATA UNA DISTINTA CON FTTPEP = 'S' => IL PIK/ETICH E'
195700030326     C* DA PAGARE poichè
195800030326     C*  o di mattina o pomeriggio è stata fatta la prestazione
195900030326     C     W01PPE        IFEQ      'S'
196000030326     C                   MOVEL     'S'           §TAPPE
196100950127     C                   ELSE
196200030326     C                   MOVEL     ' '           §TAPPE
196300950127     C                   END
196400950127     C                   CLEAR                   W01PPE
196500020507      * a giornata
196600950127     C     *IN12         IFEQ      '1'
196700020507      *
196800020507     C* SE GIORNATA IMPOSTO IL FLAG
196900020507     C                   MOVEL     APDFGR        §TAFGR
197000020507      *
197100950127     C* TROVATA UNA DISTINTA CON FTTPEP = ' ' => IL PIK/ETICH NON E'
197200950127     C* DA PAGARE
197300950127     C     W02PPE        IFEQ      'N'
197400950127     C                   MOVEL     ' '           §TAPPE
197500950127     C                   ELSE
197600950127     C                   MOVEL     'S'           §TAPPE
197700950127     C                   END
197800020506      *
197900020506      * nella nuova logica di pck/eti a giornata occorre dividere
198000020506      *  il concetto fra ritiri e consegne in quanto la tariffa
198100020506      * a giornata è distinta fra pick ed etich. indipendentemente.
198200020507      * se da pagare il picking o l'etichettatura è impostato nei
198300020507      * 2 flags hidden.
198400020513     c                   if        WflgR ='S'
198500020507     C                   movel     vh2pge        §tapge
198600020513     c                   end
198700020513      *
198800020513     c                   if        WflgC ='S'
198900020507     C                   movel     vh2pgp        §tapgp
199000020513     c                   end
199100020506      *
199200950127     C                   CLEAR                   W02PPE
199300020506      *
199400950127     C                   END
199500950127     C*
199600020201     C                   CALL      'FICNB2R'
199700020201     C                   PARM                    FICNB2
199800950127     C*
199900950127     C* NON C'E' ERRORE IN TASSAZIONE
200000950127    2C     §TAERR        IFEQ      ' '
200100020318     c                   z-add     0             vh2IAD
200200021219     c                   z-add     0             vh2RAN
200300020318     c                   z-add     0             vh2RET
200400950127     C                   Z-ADD     §TATPE        VI2TPV                          PIKIN PRES
200500950127     C                   Z-ADD     §TATBN        VI2TBV                          BONUS PRES
200600950127     C*
200700950127     C* SE "G" C'E' L'IMPORTO DELLA TARIFFA A GIORNATA
200800950127     C   12              Z-ADD     §TATIG        VI2IGR                          TAR GIORN
200900020507     C   12              Z-ADD     §TATGP        vh2tgp
201000020507     C   12              Z-ADD     §TATGE        vh2tge
201100950127     C* MEMORIZZO L'IMPORTO ADDIZIONALE
201200020322     C**N12******        ADD       §TAIAD        WIAD             10 3
201300020322     C  N12              ADD       §TAIAD        w01itt
201400020403     C  N12              ADD       §taiad        vh2iad
201500021218     C  N12              ADD       §TARAN        w01itt
201600021218     C  N12              ADD       §TARAN        w01ran
201700021219     C  N12              ADD       §TARAN        vh2ran
201800020319     C*
201900020319    2C                   Else
202000020319     C* errore nella ricerca delle tariffe
202100040506     c                   if        §taTER = 'A'
202200040506     C                   MOVEL     'E'           VI2SCE
202300040506     C                   MOVEL     §taTER        WMTAR
202400040506     c                   end
202500020319     C*
202600020319    2C                   ENDIF
202700020319     C*
202800950127     C* DATA
202900950127     C                   CLEAR                   K18
203000950127     C                   MOVE      W02DDC        DATA
203100950127     C                   MOVEA     GG            K18(1)
203200950127     C                   MOVEA     '/'           K18(3)
203300950127     C                   MOVEA     MM            K18(4)
203400950127     C* DECODIFICA TIPO PRESTAZIONE
203500950127     C                   Z-ADD     1             C
203600950127     C     W01TSR        LOOKUP    T2R(C)                                 30
203700011113     C   30              MOVEA     DT2(C)        K18(7)
203800950127     C                   MOVEA     K18           VI2DES
203900950127    1C                   ENDIF
204000950127     C* DATA DISTINTA
204100950127     C                   MOVE      W02DDC        VH2DDC
204200950127     C*
204300950127     C* TIPO PRESTAZ "R" O "C"
204400950127     C     *IN12         IFEQ      *OFF
204500950127     C* "S"= TIPO RECORD DELLA PRESTAZIONE
204600950127     C                   MOVEL     'S'           VH2TRC
204700950127     C*
204800950127     C* INDICATORI DI VISUALIZZAZIONE
204900950127     C                   SETON                                        15
205000950127     C* MEMORIZZO IL TIPO PRESTAZ  IN VH2FGR
205100950127     C                   MOVEL     W01TSR        VH2FGR                          TIPO PREST.
205200950127     C*
205300950127     C                   Z-ADD     W01ITA        VI2ITA                          ACCES SPED
205400950127     C                   Z-ADD     W01ITT        VI2ITT                          BASE  SPED
205500011113      *
205600021217      * Ritiri Annullati
205700021217     C                   z-add     w01TBS        vh2TBS
205800021219     C                   z-add     w01RAN        vh2RAN
205900021217      *
206000950127     C* ESPONGO ANCHE NEI CAMPI ALFABETICI I 2
206100011113     c     vi2itt        add       vi2ita        vi2itx
206200011113     C                   move      vi2itx        fld4              4
206300011113     C                   movel     ','           fld4
206400011113     C                   movel     vi2itx        vi2att
206500011113     C                   MOVEA     VI2ATT        K08
206600011113     C                   EXSR      ALFAB
206700020320     c                   if        K08(7) = ' '
206800020320     c                   move      '0'           K08(7)
206900020320     c                   end
207000011113     c                   movea     k08           vi2att
207100011113     c                   move      fld4          vi2att
207200950127     C*
207300950127     C                   ELSE
207400950127     C*
207500950127     C* "G"= TIPO RECORD DELLA GIORNATA
207600950127     C                   MOVEL     'G'           VH2TRC
207700950127     C* INDICATORI DI VISUALIZZAZIONE
207800950127     C                   SETON                                        1715
207900950127     C* MEMORIZZO IL FLAG GIORNATA IN VH2FGR
208000950127     C                   MOVEL     §TAFGR        VI2FGR                          FLG GIORNAT
208100950127     C                   MOVEL     §TAFGR        VH2FGR                           "    "
208200950127     C                   ENDIF
208300950127     C*
208400950127     C     W02NRR        IFEQ      0
208500950127     C                   ADD       1             NRR
208600950127     C                   WRITE     FRC0SF2
208700950127     C*
208800950127     C                   ADD       W01SNP        W02SNP
208900950127     C                   ADD       W01SET        W02SET
209000950127     C                   ADD       W01SNT        W02SNT
209100950127     C                   ADD       W01SNE        W02SNE
209200950127     C*
209300950127     C*                    DA PAGARGLI
209400011207     C                   ADD       W01ITT        W01TOT           12 3
209500950127     C                   ADD       W01ITA        W01TOT
209600950127     C                   ADD       VI2TPV        W01TOT
209700950127     C* TOTALE BONUS FUORI DAL MINIMO QUINDI DAL TOTALE A VIDEO
209800950127     C                   ADD       VI2TBV        W01TBN
209900950127     C* IMPORTO TARIFFA A GIORNATA
210000011207     C   12              ADD       VI2IGR        W01IGR           10 3
210100950127     C*
210200950127     C                   ELSE
210300950127     C*
210400950127     C                   UPDATE    FRC0SF2
210500950127     C                   ENDIF
210600950127     C*
210700950127     C                   CLEAR                   W01ITA
210800950127     C                   CLEAR                   W01ITT
210900950127     C                   CLEAR                   W01SNP
211000950127     C                   CLEAR                   W01SET
211100950127     C                   CLEAR                   W01SNT
211200950127     C                   CLEAR                   W01SNE
211300030107     C                   clear                   w01TBS
211400950127     C                   SETOFF                                       151217
211500950127     C*
211600020403     C     endtsr        ENDSR
211700130404     C* --------------------------------------------------------------*
211800130404     C*   Controlla Accreditamento e Ritorno Copia Firmata da §T3
211900130404     C* --------------------------------------------------------------*
212000130404     C     CTRacrcf_§T3  BEGSR
212100130404      *
212200130404     c                   eval       §t3pdrsav = §t3pdr
212300130404      *
212400130404      * Deve controllare solo se manca la data di Ricezione Tariffa firmata
212500130404      *  o se l'Autista non è ancora stato accreditato
212600130404     c                   if            §t3cnv = 0 or Accreditato <>'S'
212700130404      *
212800130404      * ci entra solo se è cambiato l'autista da testare
212900130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
213000130404      ****  della Tariffa Firmata
213100130404     c                   if        §T3PDR <> §5Aautista
213200130404     C                             AND parSML = ' '
213300130404      *
213400130404      * ci entra solo se è cambiato l'autista da testare
213500130404      **** e se era accreditato allora il controllo deve basarsi sulla ricezione
213600130404      ****  della Tariffa Firmata
213700130404      *
213800130404     c                   clear                   ficn5ads
213900130404     C                   MOVEL     kpjbu         savpjbu
214000130404     c                   movel     §T3FLR        DtgtFLR
214100130404     c                   z-add     §T3PDR        §5Aautista
214200130404     c                   z-add     §T3PRG        §5Aprgtar
214300130404      *
214400130404     c                   if               Accreditato = 'S'
214500130404     c                   eval      §5ATIPOCHK = '§5AGGVALDR'
214600130404     c                   end
214700130404      *
214800130404     c                   movel     §tgtRIS       §5ARISTAMP
214900130404     C                   z-add     §t3sta        §5ADATA2
215000130404     c                   eval      §5AWINDOW  = 'F'
215100130404     C                   clear                   kpjbu
215200130404     C                   MOVEL     ficn5ads      kpjbu
215300130404     C                   call      'FICN5AR'
215400130404     C                   parm                    kpjba
215500130404     C                   eval      ficn5ads = kpjbu
215600130404     C                   eval      ds5aAUT  = §5AUNIDS
215700130404     C                   clear                   kpjbu
215800130404     C                   MOVEL     savpjbu       kpjbu
215900130404     C*
216000130404     C*  Se non è stato ancora accreditato l'autista occorre bloccare dopo
216100130404     C*   "x" giorni --> presi nella tabella 5aAUT.
216200130404      ***    Solo se NON è stato forzato
216300130404     c                   IF        §5Aesito ='1'
216400130404      ***
216500130404     c                   IF        §5AGGDIFF  > 0   and
216600130404     c                             §5ATIPOGG  = '§5AGGVALDA'
216700130404     c                               and  accreditato = 'N'
216800130404     C                   SETON                                        4690
216900130404     C                   MOVEL     apdPDR        VI1PDE
217000130404     C                   MOVEL     ERR(14)       VI1ERR
217100130404      *** oppure
217200130404      ***   se è stata Ricevuta la tariffa FIRMATA dopo "x" giorni.
217300130404     c                   elseIF    §5AGGDIFF  > 0   and
217400130404     c                             §5ATIPOGG  = '§5AGGVALDR'
217500130404     c                             and §t3cnv = 0
217600130404     C                   SETON                                        4690
217700130404     C                   MOVEL     apdPDR        VI1PDE
217800130404     C                   MOVEL     ERR(10)       VI1ERR
217900130404     c                   endif
218000130404      ***
218100130404     c                   endif
218200130404     c                   endif
218300130404     c                   endif
218400130404     C*
218500130404     C                   ENDSR
218600130404     C* --------------------------------------------------------------*
218700950127     C* PREPARA DATI PER VISUALIZZARE RECORD TOTALE -----------------*
218800130404     C* --------------------------------------------------------------*
218900950127     C     VISTOT        BEGSR
219000020318     C                   SETON                                        171520
219100040420     C*  controllo spostato qui ....
219200090828     C                   SEToff                                       50  62
219300950127     C*
219400950127     C* TOTPAD > MINIMO --> ESPONGO IL TOTALE + BONUS
219500950127    5C     VI2IGR        IFGT      WMNT
219600950127     C     VI2IGR        ADD       VI2TBV        VI2TOT
219700011207     C  N47              ADD       VI2TPV        VI2TOT
219800950127     C                   ELSE
219900950127     C*                 --> ESPONGO MINIMO    + BONUS
220000950127     C     WMNT          ADD       VI2TBV        VI2TOT
220100011207     C  N47              ADD       VI2TPV        VI2TOT
220200950127     C*
220300950127    6C     VI2TBV        IFGT      0
220400950127     C     WMNT          ANDEQ     0
220500950127     C                   SETON                                        19
220600950127    6C                   ENDIF
220700950127    5C                   ENDIF
220800090828      *
220900090828      * Il totale non può essere 0
221000950406     C     VI2TOT        IFEQ      *ZEROS
221100950406     C                   SETON                                        22
221200950406     C                   END
221300040420      *
221400040420     C*  rimesso in modo diverso il controllo ossia se presente solo tariffe
221500040420     C*   a giornata x il padroncino quindi sia ritiri e consegne = (0)
221600040420     C*   e l'importo finale è = (0) e non ho nemmeno applicata una tariffa
221700040420     C*   allora non è possibile effettuare la conferma.
221800040420     C                   IF        VI2TOT = 0 and VI2IGR = 0 and
221900040420     C                             totCON = 0 and totRIT = 0 and
222000040420     C                             VI2TPV = 0 and flgFGR = *Blank
222100040426     c                             and parSML=' ' and Vi2sce <> 'F'
222200040420     C                   SETON                                        50
222300040420     C                   END
222400090828      *
222500090828      * Per evitare di utilizzare solo rettifiche e nessuna Tariffa
222600090828      *  ciò comporta in seguito problemi nella scrittura del FIFCE..
222700090828      * Il totale non può essere identico all'importo della rettifica
222800090828     C     VI2TOT        IFEQ      vh2RET
222900090828     C                   SETON                                        2262
223000090828     C                   END
223100090828      *
223200950127     C                   ENDSR
223300950127     C* CONTROLLO SFL2 -----------------------------------------------*
223400950127     C     CONTR2        BEGSR
223500040420     c                   clear                   totCON           10 3
223600040420     c                   clear                   totRIT           10 3
223700950127     C                   Z-ADD     1             NRR
223800040420     C                   SETOFF                                       90  47
223900950127     C                   READC     FRC0SF2                                32
224000950127     C*
224100950127    1C     *IN32         DOWEQ     *OFF
224200040426     c                   clear                   flgFGR            1
224300950127     C                   CLEAR                   WMNT
224400950127     C*
224500950127     C                   SELECT
224600950127     C* TIPO RECROD -->  P R E S T A Z I O N E
224700950127    2C     VH2TRC        WHENEQ    'S'
224800040420     c                   if        vh2fgr ='R'
224900040420     c     vi2itt        add       vi2ita        totRIT
225000040420     c                   end
225100040420     c                   if        vh2fgr ='C'
225200040420     c     vi2itt        add       vi2ita        totCON
225300040420     c                   end
225400020318     C*
225500020318    3c     vi2sce        ifeq      'R'
225600040426    3c     vi2sce        oreq      'F'
225700020318     c                   clear                   vi2sce
225800020318     c                   seton                                        15
225900020318     c                   update    frc0sf2
226000020318     c                   setoff                                       15
226100020318     c                   end
226200011113     C*
226300011113      * visualizza dettaglio importi
226400011113    3c     vi2sce        ifeq      'D'
226500011113     c                   clear                   vi2sce
226600011113     c                   exsr      WndwSfl
226700011113     c*
226800011113     c                   seton                                        15
226900011113     c                   update    frc0sf2
227000011113     c                   setoff                                       15
227100011113     c*
227200011113     c                   endif
227300011113      *
227400950127     C* MANUTENZIONE TARIFFE PADRONCINI
227500950127    3C     VI2SCE        IFEQ      'M'
227600950127     C                   MOVEL     VH2PDR        PA1PDR
227700950127     C                   MOVEL     VH2FGR        PA1TSR
227800950127     C                   MOVEL     VH2DDC        PA1DDC
227900950127     C                   MOVEL     PARSML        PA1SML
228000950127     C                   MOVEL     PARAM1        KPJBU
228100020125     C                   CALL      'FICNB1R'
228200950127     C                   PARM                    KPJBA
228300950127     C                   WRITE     FRC0D03
228400950127     C                   CLEAR                   VI2SCE
228500950127     C* RICALCOLO IL TIPO PRESTAZIONE
228600950127     C                   EXSR      CALTSR
228700950127    3C                   ENDIF
228800950127     C*
228900950127     C* TIPO RECORD -->  G I O R N A T A
229000950127    2C     VH2TRC        WHENEQ    'G'
229100040419     c                   move      vi2fgr        flgfgr
229200950127     C*
229300950127     C* VARIATO FLAG A GIORNATA
229400950127    3C     VI2FGR        IFNE      VH2FGR
229500950127    4C     VI2FGR        IFNE      ' '
229600950127     C* RICALCOLO LA TARIFFA A GIORNATA
229700020201     C                   CLEAR                   FICNB2
229800020426     C                   MOVEL     'CONFERM'     §TApgm
229900950127     C                   Z-ADD     VH2PDR        §TAPDR
230000020326     C                   movel     VH2PDR        §TAfgs
230100011113     c                   movel     vh2div        §tadiv
230200950127     C                   MOVEL     'G'           §TATSR
230300950127     C                   MOVEL     VI2FGR        §TAFGR
230400950127     C                   MOVEL     PARSML        §TASML
230500950127     C                   MOVEL     VH2DDC        §TADDC
230600950127     C*
230700950127     C                   MOVEL     'S'           §TAFLG
230800020311     C                   MOVEL     'G'           §TATSM
230900020311     C                   MOVEL     *all'9'       §TActr
231000950127     C                   MOVEL     PARSML        §TASML
231100020201     C                   CALL      'FICNB2R'
231200020201     C                   PARM                    FICNB2
231300950127     C*
231400950127    5C     §TAERR        IFEQ      ' '
231500950127     C                   Z-ADD     §TATIG        VI2IGR
231600950127   X5C                   ELSE
231700020318     c                   seton                                        51
231800950127     C                   CLEAR                   VI2IGR
231900950127    5C                   ENDIF
232000950127     C*
232100950127     C* SE TOLTO IL FLAG AZZERO IL CAMPO
232200950127   X4C                   ELSE
232300950127     C                   CLEAR                   VI2IGR
232400950127    4C                   ENDIF
232500950127     C* SALVO NUOVO FLAG
232600950127     C                   MOVEL     VI2FGR        VH2FGR
232700950127     C*
232800950127     C                   SETON                                        121715
232900950127     C                   UPDATE    FRC0SF2
233000950127     C                   SETOFF                                       121715
233100950127     C*
233200950127     C* TARIFFA GIORNATA
233300950127     C                   Z-ADD     VI2IGR        W01IGR
233400950127     C*
233500950127     C* CHAIN SUL RECORD DI TOTALE PER AGGIORNARE IL NUOVO TOTALE
233600950127     C                   ADD       1             NRR
233700950127     C     NRR           CHAIN     FRC0SF2                            30
233800950127     C* SE TROVO RECROD MINIMO SALVO CAMPO E RICHAIN
233900950127    4C     VH2TRC        IFEQ      'M'
234000950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
234100950127     C*
234200950127     C                   ADD       1             NRR
234300950127     C     NRR           CHAIN     FRC0SF2                            30
234400950127    4C                   ENDIF
234500950127     C*
234600950127    4C     *IN30         IFEQ      *OFF
234700950127     C     VH2TRC        ANDEQ     'T'
234800950127     C*
234900950127     C                   Z-ADD     W01IGR        VI2ITA                          TAR GIORNAT
235000950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                          ADD GIORNAT
235100011207     C*
235200011207     C*  nel campo aggiuntivo non posso mettere i decimali se sono in lire
235300011207     C                   move      vi2tpv        trebytes          3 0
235400011207     C                   if        vh2div ='ITL' and
235500011207     c                             trebytes > 0
235600011207     c                   seton                                        47
235700011207     c                   end
235800020319     C*
235900020319     c* se selezionato "R" di rettifica occorre attivare il SFLNXTCHG
236000020319     C*  per poter leggere il record di totale altrimenti con questo
236100020319     C*   update non lo leggerebbe più.
236200020319     C*  Richiamo programma di rettifiche con Note
236300020319     C*    da portare nell'importo aggiuntivo
236400020319     c     vi2sce        ifeq      'R'
236500020408     c     parsml        andeq     ' '
236600020320     c                   eval      si_CN40='S'
236700020319     C                   clear                   ficn40ds
236800030128     C                   MOVEL     'A'           §40tip
236900020319     C                   MOVEL     'C'           §40FLG
237000020319     C                   MOVEL     VH2PDR        §40PDR
237100020319     C                   MOVEL     VH2DDC        §40DDC
237200020320     C                   MOVEL     kpjbu         savpjbu
237300020320     C                   clear                   kpjbu
237400020320     C                   MOVEL     ficn40ds      kpjbu
237500020319     C                   call      'FICN40R'
237600020319     C                   parm                    kpjba
237700020319     C                   MOVEL     kpjbu         ficn40ds
237800020320     C                   clear                   kpjbu
237900020320     C                   MOVEL     savpjbu       kpjbu
238000020319     C                   z-add     §40tim        vh2RET
238100020319     C                   z-add     §40tim        VI2TPV
238200020319     C                   add       vh2IAD        VI2TPV
238300020319     C                   clear                   VI2SCE
238400020319     c                   end
238500011207      *
238600950127     C* PREPARA ALTRI DATI DA VISUALIZZARE
238700011207     C                   EXSR      VISTOT
238800020319     C*
238900950127     C                   UPDATE    FRC0SF2
239000950127     C                   SETOFF                                       171520
239100950406     C                   SETOFF                                       1922
239200950127     C*
239300950127    4C                   ENDIF
239400950127    3C                   ENDIF
239500950127     C*
239600950127     C* TIPO RECORD -->  T O T A L E
239700950127    2C     VH2TRC        WHENEQ    'T'
239800950127     C*
239900020318     C*  Richiamo programma di rettifiche con Note
240000020318     C*    da portare nell'importo aggiuntivo
240100020318    3C     vi2sce        IFEQ      'R'
240200020408    3C     parsml        andeq     ' '
240300020320     c                   eval      si_CN40='S'
240400020318     C                   clear                   ficn40ds
240500030128     C                   MOVEL     'A'           §40tip
240600020318     C                   MOVEL     'C'           §40FLG
240700020318     C                   MOVEL     VH2PDR        §40PDR
240800020318     C                   MOVEL     VH2DDC        §40DDC
240900121016     C                   MOVEL     kpjbu         savpjbu
241000121016     C                   clear                   kpjbu
241100020318     C                   MOVEL     ficn40ds      kpjbu
241200020318     C                   call      'FICN40R'
241300020318     C                   parm                    kpjba
241400020318     C                   MOVEL     kpjbu         ficn40ds
241500121016     C                   clear                   kpjbu
241600121016     C                   eval      kpjbu = savpjbu
241700020318     C                   z-add     §40tim        vh2RET
241800020318     C                   z-add     §40tim        VI2TPV
241900020318     C                   add       vh2IAD        VI2TPV
242000020318     C                   clear                   VI2SCE
242100020318     C                   UPDATE    FRC0SF2
242200020318     C                   WRITE     FRC0D03
242300020318    3C                   end
242400020318     C*
242500950127     C* CHAIN SUL RECORD DEL MINIMO
242600950127     C                   SUB       1             NRR
242700950127     C     NRR           CHAIN     FRC0SF2                            30
242800950127     C* SE NON TROVATO RECORD O NON E' 'T' RICHAIN
242900950127    3C     VH2TRC        IFEQ      'M'
243000950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
243100950127    3C                   ENDIF
243200950127     C*
243300950127     C                   ADD       1             NRR
243400950127     C     NRR           CHAIN     FRC0SF2                            30
243500950127     C*
243600950127    3C     *IN30         IFEQ      *OFF
243700950127     C     VH2TRC        ANDEQ     'T'
243800950127     C*
243900950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                          ADD GIORNAT
244000950127     C*
244100011207     C*  nel campo aggiuntivo non posso mettere i decimali se sono in lire
244200011207     C                   move      vi2tpv        trebytes          3 0
244300011207     C                   if        vh2div ='ITL' and
244400011207     c                             trebytes > 0
244500011207     c                   seton                                        47
244600011207     c                   end
244700011207      *
244800011207     C                   EXSR      VISTOT
244900040419     C*
245000950127     C                   UPDATE    FRC0SF2
245100950127     C                   SETOFF                                       171520
245200950406     C                   SETOFF                                       1922
245300950127     C*
245400950127    3C                   ENDIF
245500950127    2C                   ENDSL
245600950127     C*
245700950127     C                   READC     FRC0SF2                                32
245800950127    1C                   ENDDO
245900950127     C*
246000950127     C                   ENDSR
246100950127     C*
246200950127     C* RICALCOLO TIPO PRESTAZ DI CUI SONO ANDATA IN MODIFICA ------*
246300950127     C     CALTSR        BEGSR
246400950127     C                   CLEAR                   W01ITT
246500950127     C                   CLEAR                   W01ITA
246600950127     C* FLAG VALORIZZAZIONE
246700950127     C                   MOVEL     'V'           W02FVL
246800950127     C* TIPO PRESTAZIONE
246900950127     C                   Z-ADD     1             C
247000950127     C     PA1TSR        LOOKUP    TSR(C)                                 30
247100950127     C  N30              Z-ADD     1             C
247200950127     C* DATA
247300950127     C                   MOVEL     PA1DDC        W02DDC
247400950127     C                   MOVEL     PA1TSR        W01TSR
247500950127     C*
247600011113     C     KFTT3         SETLL     FIFTT002
247700011113     C     KFTT3         READE(N)  FIFTT002                               30
247800950127     C*
247900950127    1C     *IN30         DOWEQ     *OFF
248000950127     C* SOMMA PESI A GIORNATA
248100950127    2C     FTTCPE        IFGT      0
248200950127     C                   Z-ADD     FTTNDC        W01NDC
248300950127     C                   ADD       FTTSNA        W01SNA
248400950127     C                   ADD       FTTKFA        W01KFA
248500950127     C                   ADD       FTTTVL        W01TVL
248600950127     C* Sommo tot.colli solo se pag.pik/etich.='S'
248700950127     C     FTTPEP        IFEQ      'S'
248800950127     C                   ADD       FTTCLA        W01CLA
248900950127     C                   ADD       FTTCPE        W01CPE
249000950127     C                   END
249100950127    2C                   ENDIF
249200950127     C*
249300950127     C                   ADD       FTTITT        W01ITT
249400950127     C                   ADD       FTTITA        W01ITA
249500950127     C*
249600011113     C     KFTT3         READE(N)  FIFTT002                               30
249700950127    1C                   ENDDO
249800950127     C*
249900950127     C* LA DIFFERENZA RISCONTRATA: SE 0 NON FACCIO NULLA
250000950127     C     W01ITT        SUB       VI2ITT        DIFITT
250100950127     C     W01ITA        SUB       VI2ITA        DIFITA
250200950127     C*
250300950127    1C     DIFITT        IFNE      0
250400950127     C     DIFITA        ORNE      0
250500950127     C                   Z-ADD     NRR           W02NRR
250600950127     C*
250700950127     C                   EXSR      ROTTSR
250800950127     C*
250900950127     C                   CLEAR                   WMNT
251000950127     C                   ADD       1             NRR
251100950127    2C     NRR           DOWLE     W01NRR
251200950127     C     VH2PDR        ANDEQ     PA1PDR
251300950127     C     NRR           CHAIN     FRC0SF2                            30
251400950127     C*
251500950127    3C     *IN30         IFEQ      *OFF
251600950127     C*
251700950127     C* RECORD DI TOTALE
251800950127    4C     VH2TRC        IFEQ      'T'
251900950127     C                   ADD       DIFITT        VI2ITT                         BASE+ACC+PIK
252000950127     C                   ADD       DIFITA        VI2ITT
252100950127     C     VI2ITT        ADD       VI2ITA        VI2IGR                         ADD GIORNAT
252200950127     C*
252300950127     C                   EXSR      VISTOT
252400950127     C*
252500950127     C                   UPDATE    FRC0SF2
252600950127     C                   SETOFF                                       171520
252700950406     C                   SETOFF                                       1922
252800950127     C                   Z-ADD     W01NRR        NRR
252900950127     C*
253000950127   X4C                   ELSE
253100950127     C*
253200950127    5C     VH2TRC        IFEQ      'M'
253300950127     C                   Z-ADD     VI2IGR        WMNT                            MINIMO
253400950127    5C                   ENDIF
253500950127    4C                   ENDIF
253600950127    3C                   ENDIF
253700950127     C                   ADD       1             NRR
253800950127    2C                   ENDDO
253900950127     C*
254000950127   X1C                   ELSE
254100950127     C                   SETON                                        15
254200950127     C                   UPDATE    FRC0SF2
254300950127     C                   SETOFF                                       15
254400950127    1C                   ENDIF
254500950127     C*
254600950127     C                   ENDSR
254700950127     C*
254800950127     C* CONFERMA VALORIZZAZIONE -------------------------------------*
254900950127     C     CONVAL        BEGSR
255000020329     C*
255100950127     C* PULIZIA CAMPI DI TOTALE
255200950127     C                   CLEAR                   W01SET
255300950127     C                   CLEAR                   W01SNT
255400950127     C                   CLEAR                   W01SNP
255500950127     C                   CLEAR                   W01SNE
255600950127     C                   CLEAR                   W01ITT
255700950127     C                   CLEAR                   W01ITA
255800950127     C                   CLEAR                   W01TPE
255900021218     C                   clear                   w01TBS
256000021219     C                   clear                   w01RAN
256100950127     C                   CLEAR                   WMNT
256200020329     C                   CLEAR                   WGIO
256300020329     C                   CLEAR                   Wret
256400020329     C                   CLEAR                   WTMI              1
256500950615     C                   Z-ADD     1             WW                1 0
256600950127     C*
256700950127     C                   Z-ADD     1             NRR
256800950127    1C     NRR           DOWLE     W01NRR
256900950127     C*
257000950127     C     NRR           CHAIN     FRC0SF2                            30
257100950127     C*
257200950127    2C     *IN30         IFEQ      *OFF
257300950127     C*
257400950127    3C                   SELECT
257500950127     C*
257600011113     C* RECORD TOTALE --> SCRIVO NUOVO RECORD IN FIFTT
257700950127     C     VH2TRC        WHENEQ    'T'
257800950127     C*
257900950616     C* Verifico se è possibile chiudere le distinte/ritiri del padronc
258000950615     C                   EXSR      CTRCHI
258100950615     C     *IN90         IFEQ      '0'
258200950615     C*
258300950127     C* CHIUDO LE DISTINTE/RITIRI DEL PADRONCINO
258400950127     C                   EXSR      CHIUDC
258500950615     C*
258600011113     C                   CLEAR                   FIFTT002
258700950127     C                   MOVEL     VH2PDR        FTTPDR
258800020424     C                   MOVEL     VH2PDR        FTTfgs
258900011113     c                   movel     vh2div        fttdiv
259000950127     C                   MOVEL     VH2DDC        FTTDDC
259100950127     C                   MOVEL     'C'           FTTFVL
259200950127     C                   MOVEL     W01SET        FTTSET
259300950127     C                   MOVEL     W01SNT        FTTSNT
259400950127     C                   MOVEL     W01SNP        FTTSNP
259500950127     C                   MOVEL     W01SNE        FTTSNE
259600950127     C     VI2ITA        ADD       W01ITT        FTTITT
259700000307     C                   Z-ADD     W01ITA        FTTITA
259800950127     C                   Z-ADD     W01TPE        FTTTPE
259900950127     C                   Z-ADD     VI2TBV        FTTTBN
260000020318     C                   Z-ADD     vh2RET        FTTTIM
260100020318     C                   ADD       vh2IAD        FTTITT
260200020328     C                   move      *zero         fttcdf
260300020328     C                   move      vh2cdf        fttcdf
260400020328     C                   move      vh2soc        fttsoc
260500020329     C                   z-add     vi2tpv        wret
260600021211     C                   z-add     vi2hlv        ftthlv
260700030107      *
260800030107      * Memorizza il valore dei ritiri annullati
260900030107     C     vh2ran        mult      1000          FttPDU
261000081008     c                   z-add     vh2ran        fttvRAN
261100030107ab   C                   z-add     w01tbs        fttTBS
261200020507      *
261300950127     C* SE IL TOTALE E' < DEL MINIMO METTO IL DELTA PER ARRIVARE AL MIN
261400950127     C     VI2IGR        IFLT      WMNT
261500950127     C     WMNT          SUB       VI2IGR        FTTMNT
261600950127     C                   ENDIF
261700950127     C                   Z-ADD     DATEU8        FTTDCV
261800081008      *
261900081008      * richiamo routine per verifica se esiste sovrapprezzo carburante
262000081008     c                   exsr      caricofuel
262100081008     c                   if        o74err = *blank
262200081008     c                   z-add     o74impco      fttpcar
262300081008     c                   z-add     o74mcp        fttmcp
262400081008     c                   endif
262500950127     C*
262600011113     C                   WRITE     FIFTT002                             09
262700060411     C*
262800950531     C* Se errore scrittura invio messaggio alla sede
262900951025     C     *IN09         IFEQ      *ON
263000100906     C                   MOVEL     MSG(1)        MSGTES           55
263100100906     C                   MOVEL(P)  MSG(2)        MSGDET          198
263200950531     C                   MOVE      FTTPDR        MSPDR
263300950531     C                   MOVE      FTTDDC        MSDDC
263400100906      *
263500100906      * invia segnalazione via mail
263600100906     c                   eval      emlto = 'CedAlert@bartolini.it'
263700100906     c                   eval      emlcc = *blank
263800100906     c                   eval      emogg = 'ERRORE conferma Valorizza-
263900100906     c                             zioni AUTISTI CITTÀ'
264000100906     C                   eval      emmsg = msgtes + ' - ' + msgdet
264100100906     c
264200100906     C                   CALL      'TIS701C1'
264300100906     C                   PARM                    emlto           253
264400100906     C                   PARM                    emlcc           253
264500100906     C                   PARM                    emogg            44
264600100906     C                   PARM                    emmsg          5000
264700100906     C                   PARM                    emesito           1
264800950531     C                   END
264900040426     **
265000040426     ** Scrive forzatamente l'FIFCE con le righe a zero se il valore
265100040426     **  della giornata doveva essere nullo
265200040426     c                   if        PARSML=*blank
265300040426     C                   IF        VI2TOT = 0 and VI2IGR = 0 and
265400040426     C                             totCON = 0 and totRIT = 0 and
265500040426     C                             VI2TPV = 0 and flgFGR = *Blank and
265600040426     c                             Vi2sce = 'F'
265700040426     c                   exsr      wrt0_Voci
265800040426     C                   End
265900040426     C                   End
266000950127     C*
266100020329     C* aggiunge informazioni sulla Gionrata al Workfile che poi servirà
266200020329     C*  per attribuire le voci di conto economico secondo i pesi fra
266300020329     C*   le spedizioni dei ritiri e delle consegne.
266400020605     c                   if        PARSML=*blank
266500020329     C                   EXSR      ADD_FIFCW
266600020605     c                   end
266700020329     C*
266800950127     C                   CLEAR                   W01SET
266900950127     C                   CLEAR                   W01SNT
267000950127     C                   CLEAR                   W01SNP
267100950127     C                   CLEAR                   W01SNE
267200950127     C                   CLEAR                   W01ITT
267300950127     C                   CLEAR                   W01ITA
267400950127     C                   CLEAR                   W01TPE
267500021218     C                   clear                   w01TBS
267600021219     C                   clear                   w01RAN
267700950127     C                   CLEAR                   WMNT
267800020329     C                   CLEAR                   WGIO
267900020329     C                   CLEAR                   WTMI
268000020329     C                   CLEAR                   Wret
268100950127     C*
268200950406     C* RICHIAMO PGM DI STAMPA DEL PADRONCINO
268300950127     C                   MOVE      VH2PDR        DD0PDR
268400950127     C                   MOVEL     ' '           DD0TSR
268500950127     C                   MOVEL     PARSML        DD0SIM
268600950127     C                   MOVEL     VH2DDC        DD0DDT
268700951025     C* Se non è simulazione e errore in conferma lo segnalo al pgm di
268800951025     C* stampa affinchè gli sia possibile stampare un messaggio di
268900951025     C* dati non corretti.
269000951025     C     PARSML        IFEQ      ' '
269100951025     C     *IN09         ANDEQ     *ON
269200951025     C                   MOVE      'S'           DD0ERR
269300951025     C                   ELSE
269400951025     C                   CLEAR                   DD0ERR
269500951025     C                   END
269600020315     C                   MOVEL     FICNB6        KPJBU
269700950127     C* STAMPA DISTINTA DI CONSEGNA EFFETTIVA
269800950127     C     PARSML        IFEQ      ' '
269900020315     C                   CALL      'FICNB6R'
270000950406     C                   PARM                    KPJBA
270100950127     C                   ELSE
270200950127     C* STAMPA DISTINTA DI CONSEGNA DI SIMULAZIONE
270300020315     C                   CALL      'FICNB7C'
270400950406     C                   PARM                    KPJBA
270500950127     C                   ENDIF
270600950127     C*
270700950127     C* SOTTOMETTO LA SVALORIZZAZIONE DEI RECORD DI SIMULAZIONE
270800950127     C     VIDSIM        IFEQ      'SI'
270900950127     C                   MOVEL     VH2PDR        PA2PDR
271000950127     C                   MOVEL     ' '           PA2TSR
271100950127     C                   MOVEL     VH2DDC        PA2DDC
271200950127     C                   MOVEL     PARAM2        KPJBU
271300011115     c     knmus         ifeq      *all'1'
271400011115     c                   call      'FICND9R'
271500011115     c                   parm                    kpjba
271600011115     c                   else
271700011116     C                   MOVEL     'FND9'        KCOAZ
271800950127     C                   CALL      'BCH10'
271900950127     C                   PARM                    KPJBA
272000011115     c                   end
272100950127     C                   ENDIF
272200950615     C*
272300060411      *  impostato a livello di singolo Padroncino data
272400060411     c                   commit
272500060411     C*
272600950615     C                   END
272700950127     C*
272800950127   X3C                   OTHER
272900020503     C
273000020503     C* A Giornata    --> MI SALVO IL VALORE e il tipo Intera o Mezza
273100020503     C     VH2TRC        ifeq      'G'
273200020503     c                   if        vi2igr > 0
273300020503     C                   z-add     vi2igr        Wgio
273400020503     C                   move      vi2fgr        Wtmi
273500020503     c                   end
273600020503     c                   end
273700020503     C*
273800020503     C* RECORD MINIMO --> MI SALVO IL VALORE
273900020503     C     VH2TRC        ifeq      'M'
274000020503     C                   Z-ADD     VI2IGR        WMNT
274100020503     c                   end
274200950127     C*
274300950127     C* ESCLUDO RECORD PADRONCINO
274400950127    4C     VH2TRC        IFNE      'P'
274500950127     C                   ADD       VI2ITT        W01ITT
274600950127     C                   ADD       VI2ITA        W01ITA
274700950127     C                   ADD       VI2TPV        W01TPE
274800030107ab   C                   add       vh2tbs        w01tbs
274900950127    4C                   ENDIF
275000950127    3C                   ENDSL
275100950127    2C                   ENDIF
275200950127     C*
275300950127     C                   ADD       1             NRR
275400950127     C                   ENDDO
275500020329     C*
275600020329     C                   ENDSR
275700020329     C*--------------------------------------------------------------*
275800020329     C* Aggiunge informazioni a livello di giornata sul WorkFile     *
275900020329     C*--------------------------------------------------------------*
276000020329     C     ADD_FIFCW     BEGSR
276100950127     C*
276200020329     C* scrive un record TSR='G' per tariffa TIG o TMG senza voce
276300020329     C*  da ripartire
276400020329     c                   if        WGIO >0
276500020329     C                   clear                   fifcW000
276600020329     C                   movel     vh2pdr        fcWFGS
276700020329     C                   z-add     vh2pdr        fcWPDR
276800020329     C                   move      'G'           fcWTSR
276900020329     C                   z-add     vh2ddc        fcWDDC
277000020329     C                   z-add     1             fcWnDC
277100020329     C                   z-add     Wgio          fcWice
277200020329     c                   if        Wtmi = 'I'
277300020329     c                   MOVEL     'TIG'         fcWt8a
277400020329     c                   else
277500020329     c                   MOVEL     'TMG'         fcWt8a
277600020329     c                   end
277700020329     C                   z-add     vh2kgc        fcWKGC
277800020329     C                   z-add     vh2kgr        fcWKGR
277900020329     C                   write     fifcW000
278000020329     c                   end
278100020329     C*
278200020329     C* scrive un record TSR='G' per il delta al Minimo Garantito
278300020329     C*  con tariffa MGA da ripartire.
278400020329     c                   if        Vi2igr < Wmnt
278500020329     C                   clear                   fifcW000
278600020329     C                   movel     vh2pdr        fcWFGS
278700020329     C                   z-add     vh2pdr        fcWPDR
278800020329     C                   move      'G'           fcWTSR
278900020329     C                   z-add     vh2ddc        fcWDDC
279000020329     C                   z-add     2             fcWnDC
279100020329     C     Wmnt          sub       vi2igr        fcWice
279200020329     c                   MOVEL     'MGA'         fcWt8a
279300020329     C                   z-add     vh2kgc        fcWKGC
279400020329     C                   z-add     vh2kgr        fcWKGR
279500020329     C                   write     fifcW000
279600020329     c                   end
279700020329      * Rettifiche  --------------------
279800020423     c                   if        Wret <>0
279900020329      *
280000020329     c                   z-add     vh2pdr        frepdr
280100020329     c                   z-add     vh2ddc        freddc
280200030123     C     Kfre          setll     fifre02l
280300030123     C     Kfre          reade     fifre02l
280400020329      * ------1->
280500030123     c                   dow       not %eof(fifre02l)
280600020329      *
280700020329     c                   clear                   fifcw001
280800020329     C                   movel     vh2pdr        fcWFGS
280900020329     C                   z-add     vh2pdr        fcWPDR
281000020329     C                   z-add     vh2ddc        fcWDDC
281100020329     C                   movel(P)  'CRE'         tbeCOD
281200020329     C                   movel(P)  freCRE        tbeKE1
281300020329     C                   movel(P)  *blank        tbeKE2
281400020329     C     KTbe          chain     tntbe01l
281500020329      * consegne/ritiri
281600020329     c                   if        %found(tntbe01l)
281700020516     C                   movel     tbeuni        dCRE
281800020329     C                   movel     §CRtsr        fcWTSR
281900020402     c                   move      §CRvoc        fcWcce
282000050208     C                   z-add     10            fcWnDC
282100020329      *
282200020329     C     Kfcw1         chain     fifceW1l
282300020329      *
282400020329     c                   if        %found(fifceW1l)
282500020329      *
282600020329     C*  somma algebrica del valore
282700020329     C                   add       FreTim        fcwice
282800020329     C                   update    fifcW001
282900020329     C*
283000020329     c                   else
283100020329     C*
283200020329     C                   clear                   fifcW001
283300020329     C                   movel     vh2pdr        fcWFGS
283400020329     C                   z-add     vh2pdr        fcWPDR
283500020329     C                   z-add     vh2ddc        fcWDDC
283600050208     C                   z-add     10            fcWnDC
283700020617     c                   MOVEL     'RET'         fcWt8a
283800020329      *
283900020329     C                   movel     §CRtsr        fcWTSR
284000020402     c                   move      §CRvoc        fcWcce
284100020329      *
284200020329     C*  somma algebrica del valore
284300020402     C                   z-add     vh2kgc        fcwkgc
284400020402     C                   z-add     vh2kgr        fcwkgr
284500020402     C                   z-add     freTim        fcWice
284600020329     C                   write     fifcW001
284700020329     c                   end
284800020329     C*
284900020329     c                   endIF                                                  %found
285000020329      *
285100030123     C     Kfre          reade     fifre02l
285200020329     c                   enddo
285300020329      * ------1->
285400020329     c                   end
285500020329     C*
285600950127     C                   ENDSR
285700020329     C*--------------------------------------------------------------*
285800950127     C* CHIUDO LE DISTINTE/RITIR DEL PADRONCINO ---------------------*
285900950127     C     CHIUDC        BEGSR
286000950127     C                   MOVEL     VH2DDC        W02DDC
286100950127     C                   MOVEL     'V'           W02FVL
286200950127     C                   Z-ADD     1             C
286300950127     C     TSR(C)        DOWNE     ' '
286400011113     C     KFTT3         SETLL     FIFTT002
286500011113     C     KFTT3         READE     FIFTT002                               30
286600950127     C*
286700950127     C     *IN30         DOWEQ     *OFF
286800950127     C                   ADD       FTTSET        W01SET
286900950127     C                   ADD       FTTSNT        W01SNT
287000950127     C                   ADD       FTTSNP        W01SNP
287100950127     C                   ADD       FTTSNE        W01SNE
287200020328     C                   move      *zero         fttcdf
287300020328     C                   move      vh2cdf        fttcdf
287400020328     C                   move      vh2soc        fttsoc
287500950127     C                   MOVEL     'C'           FTTFVL
287600950127     C                   MOVEL     DATEU8        FTTDCV
287700020422     C                   MOVEL     *blank        FTTftr
287800011113     C                   UPDATE    FIFTT002
287900970924     C* elaborazione stop padroncino per trasmissione a sede e filiale
288000950127     C     PARSML        IFEQ      ' '
288100950127     C                   EXSR      SRFTDS
288200950127     C                   END
288300950127     C*
288400011113     C     KFTT3         READE     FIFTT002                               30
288500950127     C                   ENDDO
288600950127     C*
288700950127     C                   ADD       1             C
288800950127     C                   ENDDO
288900950615     C                   ENDSR
289000950615     C*****************************************************************
289100950616     C* VERIFICA POSSIBILITA' DI CHIUDERE LE DISTINTE/RITIRI DEL PADR.
289200950615     C*****************************************************************
289300950615     C     CTRCHI        BEGSR
289400950615     C*
289500950615     C                   MOVEL     VH2DDC        W02DDC
289600950616     C                   MOVE      VH2DDC        FTTDDC
289700950615     C*
289800950615     C                   EXSR      CTRDIS
289900950616     C* Trovato errore: memorizzo fino a 6 errori
290000950615     C     *IN90         IFEQ      '1'
290100950615     C     WW            IFLE      5
290200950615     C                   SELECT
290300950615     C     WW            WHENEQ    1
290400950615     C                   SETON                                        56
290500950615     C                   MOVEL     VI1ERR        VI6ERR
290600950615     C                   MOVE      VI1DDE        VI6DDE
290700950615     C                   MOVE      VI1PDE        VI6PDE
290800950615     C     WW            WHENEQ    2
290900950615     C                   SETON                                        55
291000950615     C                   MOVEL     VI1ERR        VI5ERR
291100950615     C                   MOVE      VI1DDE        VI5DDE
291200950615     C                   MOVE      VI1PDE        VI5PDE
291300950615     C     WW            WHENEQ    3
291400950615     C                   SETON                                        54
291500950615     C                   MOVEL     VI1ERR        VI4ERR
291600950615     C                   MOVE      VI1DDE        VI4DDE
291700950615     C                   MOVE      VI1PDE        VI4PDE
291800950615     C     WW            WHENEQ    4
291900950615     C                   SETON                                        53
292000950615     C                   MOVEL     VI1ERR        VI3ERR
292100950615     C                   MOVE      VI1DDE        VI3DDE
292200950615     C                   MOVE      VI1PDE        VI3PDE
292300950615     C     WW            WHENEQ    5
292400950615     C                   SETON                                        52
292500950615     C                   MOVEL     VI1ERR        VI2ERR
292600950615     C                   MOVE      VI1DDE        VI2DDE
292700950615     C                   MOVE      VI1PDE        VI2PDE
292800950615     C                   ENDSL
292900950615     C                   ADD       1             WW
293000950615     C                   SETOFF                                       45
293100950615     C                   END
293200950615     C                   CLEAR                   W01SET
293300950615     C                   CLEAR                   W01SNT
293400950615     C                   CLEAR                   W01SNP
293500950615     C                   CLEAR                   W01SNE
293600950615     C                   CLEAR                   W01ITT
293700950615     C                   CLEAR                   W01ITA
293800950615     C                   CLEAR                   W01TPE
293900021218     C                   clear                   w01TBS
294000021219     C                   clear                   w01RAN
294100950615     C                   CLEAR                   WMNT
294200950616     C                   END
294300950615     C*
294400950616     C                   ENDSR
294500950127      **************************************************************************
294600950127     C* TRASMISSIONE NUMERI STOP IN SEDE
294700950127      **************************************************************************
294800950127     C     SRFTDS        BEGSR
294900970923     C                   CLEAR                   SAVSET
295000970415     C                   Z-ADD     1             K
295100970923     C                   CLEAR                   SAVSTP
295200970924     C                   CLEAR                   SAVSTO
295300970930     C                   CLEAR                   COMSET
295400970923     C                   CLEAR                   W01STP
295500970923     C* P O S I Z I O N A M E N T O:
295600970922     C* DISTINTA DI CONSEGNA: considero solo i records con n. stop>0
295700970924    1C     FTTTSR        IFEQ      'C'
295800011113     C     KFTDS         SETGT     FIFTD000
295900950411     C                   ELSE
296000011113     C* DISTINTA DI RITIRO: visto che ad un record di FIFTD possono
296100011113     C* corrispondere più spedizioni considero anche i redords di FIFTD
296200950411     C* con numero stop = 0.
296300011113     C     KFTDS         SETLL     FIFTD000
296400970924    1C                   END
296500970922     C* L E T T U R A :
296600011113     C     KFTDR         READE     FIFTD000                               30
296700950127     C*
296800950221    1C     *IN30         DOWEQ     *OFF
296900950127     C*
297000970922     C* CONSEGNA
297100020314      *  o nuovi ritiri
297200950221    2C     FTDTSR        IFEQ      'C'
297300020314      * per gestire il nuovo modo di scrivere i ritiri con le spedizioni
297400020314     c     ftdtsr        oreq      'R'
297500020314     c     ftdnsp        andgt     0
297600020314      *
297700970923     C* A rottura numero stop memorizzo numero spedizioni per stop
297800970923     C* scrivendo il record di trasmissione
297900970924    3C     FTDSTP        IFNE      SAVSTO
298000970924    4C     SAVSTO        IFGT      0
298100970924     C                   Z-ADD     SAVSTO        W01STP
298200011113     C     KFTDS         SETLL     FIFTD000
298300020419      *
298400970923     C                   EXSR      WRTSTP
298500020419      *
298600970923    4C                   END
298700020419      *
298800970924     C                   Z-ADD     FTDSTP        SAVSTO
298900020419     C                   Z-ADD     ftdksc        savksc
299000020419     C                   MOVEL     ftdrsc        savRSC
299100970930     C                   MOVE      *ZEROS        COMSET
299200020419     C                   MOVE      *ZEROS        sv1SET
299300970923    3C                   END
299400970923     C* Controllo se record da elaborare
299500020419    2C     FTDTSR        ifeq      'R'
299600020419     C                   ADD       1             COMSET
299700020419     C                   ADD       ftdset        sv1SET
299800020419    3C                   else
299900020419      * x consegne
300000970923     C                   EXSR      CHKFTD
300100970923    3C     WELAB         IFEQ      'S'
300200970930     C                   ADD       1             COMSET
300300970923    3C                   END
300400020419    3C                   endif
300500020419      *
300600950221   X2C                   ELSE
300700950127     C* RITIRO: PRENDO DATI DA BOLLE PARTENZE
300800950411     C* Escludo se si tratta di un ritiro di una sola spedizione con
300900950411     C* numero stop = 0
301000950221    3C     FTDTSR        IFEQ      'R'
301100950411     C     FTDSTP        ANDGT     0
301200950411     C     FTDTSR        OREQ      'R'
301300950411     C     FTDSTP        ANDEQ     0
301400950411     C     FTDSET        ANDGT     1
301500970924     C* Scrittura records di trasmissione a rottura numero stop per
301600970924     C* records con numero stop > 0
301700970924    4C     FTDSTP        IFNE      SAVSTO
301800970924    5C     SAVSTO        IFGT      *ZEROS
301900970924     C                   Z-ADD     SAVSTO        W01STP
302000011113     C     KFTDS         SETLL     FIFTD000
302100011113     C     KFTDR         READE     FIFTD000                               30
302200970924    6C     *IN30         DOWEQ     *OFF
302300970924     C     FTDSTP        ANDEQ     W01STP
302400970924     C                   EXSR      ELABLP
302500011113     C     KFTDR         READE     FIFTD000                               30
302600970924    6C                   ENDDO
302700970930     C                   MOVE      *ZEROS        COMSET
302800970924    5C                   END
302900970924     C                   Z-ADD     FTDSTP        SAVSTO
303000970924    4C                   END
303100970924     C* SE NUMERO DI STOP = 0 POSSO GIA SCRIVERE I RECORDS DI TRASMISS.
303200970924    4C     FTDSTP        IFEQ      *ZEROS
303300970924     C                   EXSR      ELABLP
303400970924   X4C                   ELSE
303500970924     C* ALTRIMENTI MEMORIZZO IL NUMERO DELLE SPEDIZIONI
303600970930     C                   ADD       FTDSET        COMSET
303700970924    4C                   END
303800950801     C*
303900950221    3C                   END
304000970415     C* SCRIVO IL NUMERO DEI RITIRI PER IL DELTA
304100970415     C                   EXSR      RITDEL
304200950221    2C                   END
304300950127     C*
304400011113     C     KFTDR         READE     FIFTD000                               30
304500950221    1C                   ENDDO
304600970923     C*
304700970923     C* MEMORIZZAZIONE DELL'ULTIMO STOP DI CONSEGNA A FINE LETTURA
304800970924    1C     SAVSTO        IFGT      0
304900970924     C                   Z-ADD     SAVSTO        W01STP
305000011113     C     KFTDS         SETLL     FIFTD000
305100020314      *
305200020314     C* CONSEGNA
305300020314      *  o nuovi ritiri
305400970924    2C     FTTTSR        IFEQ      'C'
305500020314      * per gestire il nuovo modo di scrivere i ritiri con le spedizioni
305600020314     c     ftdtsr        oreq      'R'
305700020314     c     ftdnsp        andgt     0
305800020314      *
305900970923     C                   EXSR      WRTSTP
306000020314      *
306100970924   X2C                   ELSE
306200020419      *
306300970923     C* MEMORIZZAZIONE DEGLI STOP DI RITIRO
306400011113     C     KFTDR         READE     FIFTD000                               30
306500970924    3C     *IN30         DOWEQ     *OFF
306600970924     C     FTDSTP        ANDEQ     W01STP
306700970924     C                   EXSR      ELABLP
306800011113     C     KFTDR         READE     FIFTD000                               30
306900970924    3C                   ENDDO
307000970924    2C                   END
307100970415     C*
307200970415     C* SE DEVO MEMORIZZARE L'ULTIMO STOP LO FACCIO ALLA FINE DELLA
307300970415     C*  LETTURA
307400970924    1C     SAVSTP        IFGT      0
307500970415     C                   EXSR      WRISBP
307600970924    1C                   ENDIF
307700020419     C*
307800020419    1C                   END
307900020419     C*
308000950127     C                   ENDSR
308100030321      **************************************************************************
308200030321     C* controlla se ritiro o consegna per recuperare codice bolla x fcecbo
308300030321      **************************************************************************
308400030321     C     Cod_bolla     BEGSR
308500030321     c                   clear                   wfcecbo           2
308600030321     C*
308700030321     C* Verifico se è un ritiro
308800030321     c                   if        fcetsr ='R'
308900030321     C     KARB          CHAIN     Fnblp01L                           31
309000030321    2C     *IN31         IFEQ      *OFF
309100030321     C                   MOVEL(P)  blpCBO        wfcecbo
309200030321     C                   ENDif
309300030321     c                   else
309400030321     C     KARB          CHAIN     Fnarb01L                           31
309500030321    2C     *IN31         IFEQ      *OFF
309600030321     C                   MOVEL(P)  arbCBO        wfcecbo
309700030321     C                   ENDif
309800030321     c                   endif
309900030321     C*
310000030321     C                   ENDSR
310100970924      **************************************************************************
310200020419     C* prende in tab.3A del tipo bolla  per i ritiri i flags di invio
310300970924      **************************************************************************
310400020419     C     CHKblp        BEGSR
310500020419     C*
310600020419     C* Verifico se è un recupero
310700020419     C     KARB          CHAIN     Fnblp01L                           31
310800020419    2C     *IN31         IFEQ      *OFF
310900020419     C                   MOVEL     '3A'          COD
311000020419     C                   MOVEL(P)  blpCBO        KEY
311100020419     C                   CLEAR                   DS3A
311200020419     C     KTAB          CHAIN     TABEL00F                           32
311300020419     C  N32              MOVEL     TBLUNI        DS3A
311400020419     C                   ENDif
311500020419     C*
311600020419     C                   ENDSR
311700020419      **************************************************************************
311800020419     C* VERIFICA SE RECORD DI FIFTD00F E' DA ELABORARE
311900020419      **************************************************************************
312000020419     C     CHKFTD        BEGSR
312100970924     C                   CLEAR                   WELAB
312200970924     C* Memorizzo spedizione solo se consegnata
312300970924     C* e se non è una consegna parziale
312400970924     C* e se non è un ritorno all'incasso
312500970924    1C     FTDTMC        IFEQ      *BLANKS
312600970924     C     FTDCMC        ANDEQ     *BLANKS
312700970924     C     FTDSIC        ANDNE     'S'
312800970924     C* Verifico se è un recupero
312900970924     C     KARB          CHAIN     FNARB01L                           31
313000970924    2C     *IN31         IFEQ      *OFF
313100970924     C                   MOVEL     '3A'          COD
313200970924     C                   MOVEL(P)  ARBCBO        KEY
313300970924     C                   CLEAR                   DS3A
313400970924     C     KTAB          CHAIN     TABEL00F                           32
313500970924     C  N32              MOVEL     TBLUNI        DS3A
313600970924     C* Scarto se è un recupero dal momento che in C.E. non ci sono
313700970924     C* competenze di stop sulle spedizioni di recupero
313800970924     C     §3ARBL        IFNE      'R'
313900970924     C                   MOVE      'S'           WELAB             1
314000970924    3C                   END
314100970924    2C                   END
314200970924    1C                   END
314300970924     C                   ENDSR
314400970923      **************************************************************************
314500970924     C* SCRITTURA NUMERO DI SPEDIZIONI PER STOP DI CONSEGNA (FNSTP10F)
314600970923      **************************************************************************
314700970923     C     WRTSTP        BEGSR
314800011113     C     KFTDR         READE     FIFTD000                               30
314900970923    1C     *IN30         DOWEQ     *OFF
315000970923     C     FTDSTP        ANDEQ     W01STP
315100970923     C* Verifico se record da elaborare
315200020419     c     ftdtsr        ifeq      'C'
315300970923     C                   EXSR      CHKFTD
315400020419     c                   else
315500020419     C                   EXSR      CHKblp
315600020419     c                   move      'S'           welab
315700020419     c                   endif
315800970923    2C     WELAB         IFEQ      'S'
315900970923     C     §3ABSD        ANDEQ     *BLANKS
316000970924     C     WELAB         OREQ      'S'
316100970923     C     §3ABFI        ANDEQ     *BLANKS
316200970924     C                   CLEAR                   FNSTP100
316300970923     C                   MOVE      FTDTSR        STPTSR
316400970923     C                   MOVE      FTDAAS        STPAAS
316500970923     C                   MOVE      FTDLNP        STPLNP
316600970923     C                   MOVE      FTDNRS        STPNRS
316700970923     C                   MOVE      FTDNSP        STPNSP
316800000615     C     COMSET        IFEQ      99999
316900000615     C                   Z-ADD     99998         STPSET
317000971028     C                   ELSE
317100970930     C                   Z-ADD     COMSET        STPSET
317200971028     C                   END
317300970924     C* Imposto flag a chi inviare il record (sede,filiale o entrambi)
317400970924    3C                   SELECT
317500970924     C     §3ABSD        WHENEQ    *BLANKS
317600970924     C     §3ABFI        ANDEQ     *BLANKS
317700970924     C                   MOVEL     'E'           STPINV
317800970924     C     §3ABSD        WHENEQ    *BLANKS
317900970924     C                   MOVEL     'S'           STPINV
318000970924     C     §3ABFI        WHENEQ    *BLANKS
318100970924     C                   MOVEL     'F'           STPINV
318200970924    3C                   ENDSL
318300970923     C* scrivo record per trasmissione a sede
318400970924     C                   WRITE     FNSTP100
318500970923    2C                   END
318600011113     C     KFTDR         READE     FIFTD000                               30
318700970923    1C                   ENDDO
318800970924     C                   ENDSR
318900970924      **************************************************************************
319000970924     C* LETTURA ARCHIVIO BOLLE PARTENZA PER STOP DI RITIRO
319100970924      **************************************************************************
319200970924     C     ELABLP        BEGSR
319300970924     C* IMPOSTO CHIAVE PER LETTURA ARCHIVIO BOLLE PARTENZA
319400970924     C                   MOVE      FTDKSC        W01KSC
319500970924     C                   CLEAR                   W01CCM
319600970924     C                   MOVE      FTDNDC        W01NRT
319700970924     C     KBLP1         SETLL     FNBLP34L                               31
319800970924     C* Assegnato non codificato
319900970924    1C     *IN31         IFEQ      *OFF
320000970924     C                   MOVE      9999          W01KSC
320100970924     C                   MOVE      FTDKSC        W01CCM
320200970924     C     KBLP1         SETLL     FNBLP34L
320300970924    1C                   END
320400970924     C                   EXSR      WRTSTR
320500970924     C                   MOVE      FTDKSC        W0040             4 0
320600971013    1C                   SELECT
320700971013     C* Se franco non codificato ripeto WRTSTR con KSC = 9999
320800971013     C* (Caso di ritiro di alcune spedizioni in franco e di altre in
320900971013     C*  assegnato presso lo stesso cliente non codificato)
321000971013   WHC     W0040         WHENEQ    8888
321100970924     C                   MOVE      9999          W01KSC
321200970924     C                   CLEAR                   W01CCM
321300971013     C     KBLP1         SETLL     FNBLP34L
321400971013     C                   EXSR      WRTSTR
321500971013     C* Se franco codificato ripeto WRTSTR con KSC=9999 E CCM=KSC
321600971013     C* (Caso di ritiro di alcune spedizioni in franco e di altre in
321700971013     C*  assegnato presso lo stesso cliente codificato)
321800971013   WHC     W0040         WHENNE    9999
321900971013     C                   MOVE      9999          W01KSC
322000971013     C                   MOVE      FTDKSC        W01CCM
322100970924     C     KBLP1         SETLL     FNBLP34L
322200970924     C                   EXSR      WRTSTR
322300971013    1C                   ENDSL
322400970924     C                   ENDSR
322500950801      **************************************************************************
322600970923     C* MEMORIZZAZIONE STOP DI RITIRO
322700950801      **************************************************************************
322800970924     C     WRTSTR        BEGSR
322900950801     C*
323000950801     C     KBLP1         READE     FNBLP34L                               31
323100950801     C*
323200950801    1C     *IN31         DOWEQ     *OFF
323300970923     C* Escludo i recuperi in quanto non possono esistere dei ritiri
323400970923     C* a fronte di recuperi
323500950801     C                   MOVEL     '3A'          COD
323600950801     C                   MOVEL(P)  BLPCBO        KEY
323700950801     C                   CLEAR                   DS3A
323800950801     C     KTAB          CHAIN     TABEL00F                           32
323900950801     C  N32              MOVEL     TBLUNI        DS3A
324000970923    2C     §3ARBL        IFNE      'R'
324100970924     C     §3ABSD        ANDEQ     *BLANKS
324200970924     C     §3ARBL        ORNE      'R'
324300970924     C     §3ABFI        ANDEQ     *BLANKS
324400970924     C                   CLEAR                   FNSTP100
324500970924     C                   MOVE      FTDTSR        STPTSR
324600970924     C                   MOVE      BLPAAS        STPAAS
324700970924     C                   MOVE      BLPLNP        STPLNP
324800970924     C                   MOVE      BLPNRS        STPNRS
324900970924     C                   MOVE      BLPNSP        STPNSP
325000970924    3C     FTDSTP        IFEQ      *ZEROS
325100000615     C     FTDSET        IFEQ      99999
325200000615     C                   Z-ADD     99998         STPSET
325300971028     C                   ELSE
325400970924     C                   Z-ADD     FTDSET        STPSET
325500971028     C                   END
325600970924   X3C                   ELSE
325700000615     C     COMSET        IFEQ      99999
325800000615     C                   Z-ADD     99998         STPSET
325900971028     C                   ELSE
326000970930     C                   Z-ADD     COMSET        STPSET
326100971028     C                   END
326200970924    3C                   END
326300970924    3C                   SELECT
326400970924     C     §3ABSD        WHENEQ    *BLANKS
326500970924     C     §3ABFI        ANDEQ     *BLANKS
326600970924     C                   MOVEL     'E'           STPINV
326700970924     C     §3ABSD        WHENEQ    *BLANKS
326800970924     C                   MOVEL     'S'           STPINV
326900970924     C     §3ABFI        WHENEQ    *BLANKS
327000970924     C                   MOVEL     'F'           STPINV
327100970924    3C                   ENDSL
327200970924     C                   WRITE     FNSTP100
327300950801    2C                   END
327400950801     C     KBLP1         READE     FNBLP34L                               31
327500950801    1C                   ENDDO
327600950801     C*
327700950801     C                   ENDSR
327800970415      **************************************************************************
327900970415     C* SCRITTURA NUMERI STOP DI RITIRO PER IL DELTA DI FILIALE
328000970415      **************************************************************************
328100970415     C     RITDEL        BEGSR
328200970415     C* SE NON C'E' IL NUMERO DI STOP SCRIVO SUBITO
328300970415     C*  SE C'E' DEVO TENERE MEMORIZZATA LA KEY PER SOMMARE INSIEME
328400970415     C*  I RECORD CON LO STESSO NUMERO DI STOP
328500970415     C**
328600970415     C                   MOVE      FTDKSC        W0040             4 0
328700970415     C*  ESCLUDO I CLIENTI GENERICI 8888 E 9999
328800970415    1C     W0040         IFNE      8888
328900970415     C*
329000970415    2C     FTDSTP        IFNE      SAVSTP
329100970415    3C     SAVSTP        IFGT      0
329200970415     C                   EXSR      WRISBP
329300970415    3C                   ENDIF
329400970415     C                   Z-ADD     FTDSTP        SAVSTP
329500970415    2C                   ENDIF
329600970415     C*
329700970415     C                   MOVEL     FTDKSC        KSC(K)
329800970415     C                   MOVEL     FTDRSC        RSC(K)
329900970415     C                   ADD       1             K
330000970415     C                   ADD       FTDSET        SAVSET
330100970415     C*
330200970415    2C     FTDSTP        IFEQ      0
330300970415     C                   EXSR      WRISBP
330400970415    2C                   ENDIF
330500970415    1C                   ENDIF
330600970415     C*
330700970415     C                   ENDSR
330800970415      **************************************************************************
330900970415     C* SCRITTURA FILE FNSBP00F
331000970415      **************************************************************************
331100970415     C     WRISBP        BEGSR
331200970415     C                   Z-ADD     1             K
331300970415     C     KSC(K)        DOWGT     0
331400970415     C                   Z-ADD     KSC(K)        SAVKSC
331500970415     C                   MOVEL     RSC(K)        SAVRSC
331600970415     C*
331700970415     C                   ADD       1             K                 3 0
331800970415     C                   ENDDO
331900020419     C*
332000970415     C                   CLEAR                   KSC
332100970415     C                   CLEAR                   RSC
332200970415     C                   CLEAR                   SAVSET
332300970415     C                   Z-ADD     1             K
332400970415     C                   ENDSR
332500020419     C*----------------------------------------------------------------
332600950127     C* ESPONGO I CAMPI ITT E ITA IN CAMPI ALFABETICI DEL VIDEO ------*
332700020419     C*----------------------------------------------------------------
332800011113     C     ALFAB         BEGSR
332900950127     C* ELIMINO GLI 0 NON SIGNIFICATIVI
333000011113     c                   movel     ','           k08(8)
333100011113     C                   Z-ADD     1             X
333200011113     C     K08(X)        DOWEQ     '0'
333300011113     c     X             andlt     8
333400011113     C                   MOVEL     ' '           K08(X)
333500011113     C                   ADD       1             X                 3 0
333600011113     C                   ENDDO
333700011113      *
333800011113     C                   ENDSR
333900950912      **************************************************************************
334000950912     C* DELET DA FILE DI LAVORO DEI RECORDS DI QUESTO LAVORO
334100950912      **************************************************************************
334200950912     C     SRWC01        BEGSR
334300951011     C     KWC1          SETLL     FNWLRC1L
334400951011     C     KWC1          READE     FNWLRC1L                               30
334500950912     C     *IN30         DOWEQ     *OFF
334600950912     C                   DELETE    FNWC01
334700951011     C     KWC1          READE     FNWLRC1L                               30
334800950912     C                   ENDDO
334900950912     C                   ENDSR
335000950912     C*
335100020327      **************************************************************************
335200020327     C* DELET Del WorkFile voci C/Economico da Valorizzazione Conteggi
335300020327      **************************************************************************
335400020327     C     DEL_FIFCW     BEGSR
335500020327     C*
335600020521     C* solo se non in simulazione
335700020522     C     PARSML        IFeq      ' '
335800020723     C     w01nrr        andgt     0
335900020521     C*
336000020327     C* deve cancellare i records creati e non confermati dal programma
336100020327     c                   z-add     1             nrr
336200020327     c     nrr           chain     FRC0SF2                            21
336300020327     C*
336400020327     c     *in21         doweq     *off
336500020327     C*
336600020423     C* se sono rimasti per errore dei records
336700020423     C*  quando è sui totali elimina tutto ciò che riguarda il padroncino
336800020619     c     vh2trc        ifeq      'T'
336900020423     c                   movel     vh2pdr        kfgsw
337000020423     c                   z-add     vh2pdr        kpdrw
337100020423     c                   z-add     vh2ddc        kddcw
337200020423     C*  Elimina tutto
337300050208     C     Kfcw2T        setll     fifcew3l
337400050208     C     Kfcw2T        reade     fifcew3l
337500050208     c                   dow       not %eof(fifcew3l)
337600020718     c                   if        F6Fine = 'S'
337700020611     c                   exsr      Spia_Records                                 <-------
337800020718     c                   end
337900020423     C                   delete    fifcw000
338000050208     C     Kfcw2T        reade     fifcew3l
338100020423     c                   end
338200020423     C*
338300020619     c                   endif
338400020327     C*
338500020327     c                   add       1             nrr
338600020327     c     nrr           chain     FRC0SF2                            21
338700020327     c                   enddo
338800020327     C*
338900020522     C                   ENDif
339000020521     C*
339100020521     C                   ENDSR
339200020329      **************************************************************************
339300020329      * Emissione Window con dettaglio importi della riga Base e Accessori
339400020329      **************************************************************************
339500020329     C* Emette la finestra con il dettaglio degli importi tariffe ----*
339600020329     c     WndwSfl       begsr
339700020329     c*
339800011113     c                   seton                                        35
339900011113     c                   write     FRC0wc6
340000011113     c                   setoff                                       35
340100011113     c*
340200011113     c                   clear                   nrw6              5 0
340300011113     c                   z-add     1             wrc6
340400011113     c                   move      vh2div        w6cdiv
340500011113     c                   movel     vi2des        w6cdes
340600020128     c                   z-add     vi2itt        w6cbas
340700020128     c                   z-add     vi2ita        w6cacc
340800020128     c                   z-add     vi2tpv        w6cpck
340900020128     c                   z-add     vi2tbv        w6cbon
341000011113     c*------->
341100011113     c* Carica le  righe di dettaglio importi
341200011113     c                   do        *hival
341300011113     c* riga del sfl
341400011113     c                   add       1             nrw6
341500011113     c*  deve caricare solo 7 righe max.
341600011113     c                   if        nrw6 > 7
341700011113     c                   leave
341800011113     c                   endif
341900011113     c*
342000011113     c                   z-add     nrw6          w1htrc
342100011113     c                   z-add     0             w1valr
342200011113     c                   movel     *blank        w1tipo
342300011113     c                   setoff                                       60
342400011113     c*
342500011113     c                   select
342600011113     c*
342700011113     c     nrw6          wheneq    1
342800011113     c                   seton                                        60
342900011113     c                   movel     'SPEDIZIONI'  w1tipo
343000011113     c*
343100011113     c     nrw6          wheneq    2
343200011113     c                   movel     'Base'        w1tipo
343300011113     c                   z-add     vi2itt        w1valr
343400011113     c*
343500011113     c     nrw6          wheneq    3
343600011113     c                   movel     'Accessori'   w1tipo
343700011113     c                   z-add     vi2ita        w1valr
343800011113     c*
343900011113     c     nrw6          wheneq    5
344000011113     c                   seton                                        60
344100011113     c                   movel     'PRESTAZIONE' w1tipo
344200011113     c*
344300011113     c     nrw6          wheneq    6
344400011113     c                   movel     'Pick/Etich'  w1tipo
344500011113     c                   z-add     vi2tpv        w1valr
344600011113     c*
344700011113     c     nrw6          wheneq    7
344800011113     c                   movel     'Bonus'       w1tipo
344900011113     c                   z-add     vi2tbv        w1valr
345000011113     c*
345100011113     c                   endsl
345200011113     c*
345300011113     c                   write     FRC0ws6
345400011113     c                   enddo
345500011113     c*-------> Emissione Window
345600011113     c     resta_wdw     tag
345700011113      *                *----------------------*
345800011113     c                   write     FRC0wd6
345900011113     c                   exfmt     FRC0wc6
346000011113      *                *----------------------*
346100011113     c*  Controlli
346200011113     c* Cntrl le righe x importi modificati
346300011113     c                   z-add     0             nrw6
346400011113      *------------        ---------------
346500011113     c* resta sulla window se non ha premuto dei cmd
346600011113     c     *inkl         ifeq      *off
346700011113     c                   goto      resta_wdw
346800011113     c                   end
346900011113      *
347000011113     c                   endsr
347100020329      **************************************************************************
347200040426      * Scrive forzatamente le Voci di FCE della giornata = 0 x ogni bolla
347300020329      **************************************************************************
347400040426     c     Wrt0_VOCI     begsr
347500040426     **
347600040426     C     Kftd6_zero    setll     fiftd06l
347700040426     C     Kftd6_zero    reade     fiftd06l
347800040426     **
347900040426     c                   dow       not %eof(fiftd06l)
348000040426     **
348100040426     **  scrive i records bolla con valore (0)
348200040426     C                   z-add     ftdfgs        fceFGS
348300040426     c                   if        fceFGS=0
348400040426     C                   movel     ftdpdr        fceFGS
348500040426     c                   end
348600040426     C                   z-add     ftdpdr        fcePDR
348700040426     C                   move      ftdtsr        fceTSR
348800040426     C                   z-add     ftdndc        fceNDC
348900040426     C                   z-add     ftdddc        fceDDC
349000040426     C                   z-add     ftdaas        fceAAS
349100040426     C                   z-add     ftdlnp        fceLNP
349200040426     C                   z-add     ftdnrs        fceNRS
349300040426     C                   z-add     ftdnsp        fceNSP
349400040426      *
349500040426      *  chiodo x impostare il codice Consegne
349600040426     C                   if        ftdtsr ='C'
349700040426     c                   move      '005'         fcecce
349800040426     c                   end
349900040426      *  chiodo x impostare il codice Ritiri
350000040426     C                   if        ftdtsr ='R'
350100040426     c                   move      '001'         fcecce
350200040426     c                   end
350300040426     C*
350400040426     c                   exsr      Cod_bolla
350500040426
350600040426     C*  somma algebrica del valore
350700040426     C                   move      wfcecbo       fcecbo
350800040426     C                   z-add     0             fceice
350900040426     C                   write     fifce000
351000040426     **
351100040426     C     Kftd6_zero    reade     fiftd06l
351200040426     c                   enddo
351300040426     **
351400040426     c                   endsr
351500040426      **************************************************************************
351600040426      * Aggiorna le Voci di conto Economico tramite il Workfile
351700040426      **************************************************************************
351800040426     c     AGG_VOCI      begsr
351900040426     c*
352000020329     c*  legge in sequenza il file di work con il quale poi, agganciando tutti
352100020329     c*  i relativi dettagli, in base al peso di ciascuno sul peso totale
352200020329     c*  attribuisce il valore proporzionale alla singola bolla per voce di
352300020329     c*  conto economico.
352400050208     c     *loval        setll     fifcew3l
352500050208     c                   read      fifcew3l
352600020329     C* ------1->
352700050208     c                   dow       not %eof(fifcew3l)
352800020524     C* flag di forzatura
352900020524     c                   move      *blank        forza_pag         1
353000040107     c                   move      *blank        fatto_uno         1
353100040407     c                   z-add     0             al_max            3 0
353200020524     C*
353300020515     C* se non aveva impostato i pesi totali sul record
353400020515     C*   il caso può essere se il documento aveva tutte tariffe poste
353500030404     c                   if        fcwkgc = 0  and fcwtsr ='C' or
353600030404     c                             fcwkgr = 0  and fcwtsr ='R' or
353700031219     c* ricalcolo sempre x "Giornata" x sicurezza poichè il FICNB1R non sempre
353800031219     c*  passa il totale pesi x la giornata (vedi spedizioni ISOLE)
353900040726     c                             fcwtsr ='G'
354000020515     C                   exsr      CALC_KG
354100020515     c                   endif
354200050222     C*
354300050222     C*  ATTENZIONE: potrei avere ancora i kg = 0 poichè esiste una ECCEZIONE.
354400050222     C*   Eccezione: x RETTIFICHE MANUALI (FIFRE--> "RET") fatte su tipi servizi
354500050222     C*     non trattati su quella giornata.
354600050222     C*  Esempio: se il padroncino in quella giornata ha fatto solo Consegne e gli
354700050222     C*  viene fatta una rettifica sui Ritiri ..... I kg dei ritiri sono (0) e non
354800050222     C*  riesce ad eseguire questa routine. A questo punto occorre ribaltare sulle
354900050222     C*  Consegne la rettifica altrimenti il dato verrebbe perso.
355000020515     C*
355100020329     C* Imposta il totale dei kg da riproporzionare
355200020329     c                   move      *zeros        totkg             9 1
355300050222      *
355400050224     C                   Select
355500050224      *
355600050224     C                   WHEN      fcwtsr ='C'
355700020329     c                   z-add     fcwkgc        totkg             9 1
355800050222      * Eccezione:RET Consegne --> ribalta su Ritiri
355900050222     c                   if        fcWt8a = 'RET     ' and totkg = 0
356000050222     c                   z-add     fcwkgr        totkg
356100050222     C                   eval      fcwtsr ='R'
356200050222     c                   end
356300050224      * Eccezione:BON Consegne --> ribalta su Ritiri (Il Bonus è solo x Consegne)
356400050224     c                   if        fcWt8a = 'G999 BON' and totkg = 0  OR
356500050224     c                             fcWt8a = 'C999 BON' and totkg = 0
356600050224     c                   z-add     fcwkgr        totkg
356700050224     C                   eval      fcwtsr ='R'
356800050224     c                   end
356900050222      *
357000050224     C                   WHEN      fcwtsr ='R'
357100020329     c                   z-add     fcwkgr        totkg
357200050222      * Eccezione:RET Ritiri   --> ribalta su Consegne
357300050222     c                   if        fcWt8a = 'RET     ' and totkg = 0
357400050222     c                   z-add     fcwkgc        totkg
357500050222     C                   eval      fcwtsr ='C'
357600050222     c                   end
357700050224      * Eccezione:RAN Ritiri   --> ribalta su Consegne (Il Rit.Ann. è solo x Ritiri)
357800050224     c                   if        fcWt8a = 'R999 RAN' and totkg = 0
357900050224     c                   z-add     fcwkgc        totkg
358000050224     C                   eval      fcwtsr ='C'
358100050224     c                   end
358200050222      *
358300050224     C                   WHEN      fcwtsr ='G'
358400020329     c     fcwkgc        add       fcwkgr        totkg
358500050222      *
358600020329     C                   endsl
358700020329     C*
358800050222     C*
358900050222     C*    ECCEZIONE all'ECCEZIONE:   -->  ( N O N    G E S T I B I L E )
359000050222     C*  Esiste però il caso di rettifiche senza kg nè su Consegne nè su Ritiri
359100050222     C*  poichè il padroncino è pagato a Giornata quindi non si riesce a ribaltare
359200050222     C*  il peso fra Ritiri e Consegne.....
359300050222     C*
359400020417     C* A T T E N Z I O N E : esegue solo se il totale Kg è > 0
359500020417     c                   if        totkg > 0
359600020417      *
359700020329     C* imposta il campo utilizzato come resto del valore totale da attribuire
359800020329     c                   z-add     fcwice        resto_ice
359900020329     C*
360000031218     c     rispalma      tag
360100040726     **
360200040726     **     Conteggia quante bolle ci sono e quante sono da Non Pagare
360300040726     **      se dovesse coincidere il totale Bolle con quelle da non Pagare
360400040726     **       allora successivamente nella Routine xResto devo comunque
360500040726     **      spalmare eventuali valori ad esempio Rettifiche.
360600040726     c                   z-add     0             num_bolle         5 0
360700040726     c                   z-add     0             num_bol_nonPa     5 0
360800031218     C*
360900020329     C*  quindi legge il dettaglio delle bolle per data documento
361000020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
361100020329     C     Kftd6         setll     fiftd06l
361200020329     C     Kftd6         reade     fiftd06l
361300020329     c                   else
361400020329     C     Kftd6_1       setll     fiftd06l
361500020329     C     Kftd6_1       reade     fiftd06l
361600020329     c                   end
361700020329     C* ----2->
361800020329     c                   dow       not %eof(fiftd06l)
361900040407     c                   add       1             al_max
362000040726     c                   add       1             num_bolle         5 0
362100020329      *
362200020329      * non deve avere un cod.giacenza/manc.consegna
362300020524      *  ma se tutte le bolle nel documento erano non da pagare
362400020524      *  allora occorre spalmare comunque.
362500020524     c     forza_pag     ifeq      'S'
362600020524     c                   move      'S'           W01pag
362700020524     c                   else
362800020423     C                   exsr      Se_daPagare
362900020524     c                   end
363000020524      *
363100020423     c     W01pag        ifeq      'S'
363200031216      *
363300031216     c***  Attenzione : solo se il totale generale è superiore al singolo dettaglio
363400031216     c                   if        ftdpkl <= totkg
363500040107     c                   move      'S'           fatto_uno
363600020329     C*               *--------------------*
363700020329     c                   exsr      xBolla
363800020329     C*               *--------------------*
363900031216     c                   end
364000031216      *
364100040726     c                   else
364200040726      **
364300040726      ** conta quelle da NON PAGARE per spalmare successivamente
364400040726     c                   add       1             num_bol_nonPa
364500020329     c                   end
364600020329      *
364700020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
364800020329     C     Kftd6         reade     fiftd06l
364900020329     c                   else
365000020329     C     Kftd6_1       reade     fiftd06l
365100020329     c                   end
365200020329     c                   enddo
365300040726     **
365400040726     **   Se coincide il totale bolle di dettaglio con le bolle da non Pagare
365500040726     **    anche in questo caso deve forzare il pagamento
365600040726     **     ha fatto il giro (al_max=1) la prima volta a vuoto poichè solo dopo aver letto
365700040726     **      il dettaglio può conoscere che tipo di bolle ci sono all'interno del documento.
365800040726     c                   if        num_bolle = num_bol_nonPa and al_max = 1
365900040726     c                   move      'S'           forza_pag
366000040726     c                   end
366100031218      *
366200031218      * Se c'è un resto oltre l'euro prova spalmare nuovamente il rimanente
366300031218      *  sull bolle che aveva precedentemente calcolato
366400040407      *  ma non deve continuare se il resto rimane sempre quello e può tentare
366500040407      *  al max. 3 volte dopodichè lo somma all'ultima spedizione.
366600040726      *  oppure
366700040726      *    ATTENZIONE:
366800040726      *  se la totalità delle bolle era da non pagare e comunque devo spalmare
366900040726      *  il valore (es.una rettifica di una Giornata) e al_max è = 1 (poichè è già passato
367000040726      *  una volta dentro al ciclo di lettura) prova a spalmare e se rimane del resto
367100040726      *  prova a rispalmare altre 3 volte prima di passare alla routine RESTO.
367200040726      *  (Il test è fatto con "4" poichè il contatore al_max parte da 1 e non da 0)
367300040407     c                   if        resto_ICE > 1 and fatto_uno ='S' and
367400040407     c                             resto_ice <> fcwice and al_max <= 3
367500040726     ****
367600040726     c                             or
367700040726     c                             num_bolle = num_bol_nonPa and al_max = 1
367800040726     c                             or
367900040726     c                             num_bolle = num_bol_nonPa and
368000040726     c                             resto_ice <> fcwice and al_max <= 4
368100040726     ****
368200031218     c                   z-add     resto_ice     fcwice
368300031218     c                   goto      rispalma
368400031218     c                   end
368500020329      *
368600020329      * se c'è del resto lo attribuisce sull'ultimo dettaglio
368700020423     c                   if        resto_ICE <> 0
368800040726     **
368900020329     C*               *--------------------*
369000020329     c                   exsr      xResto
369100020329     C*               *--------------------*
369200020329     c                   end
369300020329     C* ----2->
369400020417     c                   endIF
369500020417      *
369600050208     c                   read      fifcew3l
369700020329     c                   enddo
369800020329     C* ------1->
369900020329      *
370000020329     c                   endsr
370100020329      **************************************************************************
370200020515      * Deve calcolare i kg per documento sia ritiri che consegne
370300020329      **************************************************************************
370400020515     c     CALC_KG       begsr
370500020515      *
370600020524     c                   move      'SI'          piucheck          2
370700020603     c                   z-add     0             contaC            5 0
370800020603     c                   z-add     0             contaR            5 0
370900030404     c*
371000030404     c                   z-add     0             fcwkgc
371100030404     c                   z-add     0             fcwkgr
371200020524      *
371300020524     c     rifai         tag
371400020524      *
371500020515     C*  quindi legge il dettaglio delle bolle per data documento
371600020515     C     Kftd6_1       setll     fiftd06l
371700020515     C     Kftd6_1       reade     fiftd06l
371800020515     C* ----2->
371900020515     c                   dow       not %eof(fiftd06l)
372000020515      *
372100020515      * non deve avere un cod.giacenza/manc.consegna
372200020524     c     piucheck      ifeq      'SI'
372300020515     C                   exsr      Se_daPagare
372400020524     c                   else
372500020524     c                   move      'S'           W01pag
372600020524     c                   end
372700020524      *
372800020515     c     W01pag        ifeq      'S'
372900020515     c     ftdtsr        ifeq      'C'
373000020515     c                   add       ftdPKL        fcwkgc
373100020603     c                   add       1             contaC
373200020515     c                   end
373300020515     c     ftdtsr        ifeq      'R'
373400020515     c                   add       ftdPKL        fcwkgr
373500020603     c                   add       1             contaR
373600020515     c                   end
373700020515     c                   end
373800020515      *
373900020515     C     Kftd6_1       reade     fiftd06l
374000020515     c                   enddo
374100020524      *
374200020524      *  se sono tutte bolle da non pagare allora riesegue la
374300020524      *  routine di calcolo kg escludendo il controllo del da_pagare
374400020603     c                   if        fcwkgc = 0 and fcwkgr = 0 and
374500020603     c                             piucheck = 'SI'
374600020524     c                   move      'NO'          piucheck          2
374700020524     c                   move      'S'           forza_pag
374800020524     c                   goto      rifai
374900020524     c                   end
375000020515      *
375100020515     c                   endsr
375200020515      **************************************************************************
375300020515      * Letta la Bolla calcola e scrive il record di Voce di Conto Economico
375400020515      **************************************************************************
375500020515     c     xBolla        begsr
375600020515     c*
375700020329     C* Crea il valore da ripartire:
375800020329     C*  proporziona la PARte relativa al peso della spedizione e si
375900020329     C*   memorizza il resto
376000020417     C*
376100020329     c                   eval      val_ice = fcwice * ftdpkl / totkg
376200020329     c                   eval      resto_ice = resto_ice - val_ice
376300020329     C*
376400020329     C                   z-add     ftdfgs        fceFGS
376500020329     c                   if        fceFGS=0
376600020329     C                   movel     ftdpdr        fceFGS
376700020329     c                   end
376800020329     C                   z-add     ftdpdr        fcePDR
376900020329     C                   move      ftdtsr        fceTSR
377000020329     C                   z-add     ftdndc        fceNDC
377100020329     C                   z-add     ftdddc        fceDDC
377200020329     C                   z-add     ftdaas        fceAAS
377300020329     C                   z-add     ftdlnp        fceLNP
377400020329     C                   z-add     ftdnrs        fceNRS
377500020329     C                   z-add     ftdnsp        fceNSP
377600020403      *
377700020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
377800020329     c                   move      fcwcce        fcecce
377900020329     c                   else
378000020403     c                   exsr      CCE_da8A
378100020329     c                   end
378200020329     C*
378300030321     c                   exsr      Cod_bolla
378400030321
378500020329     C     Kfce          chain     fifce01l
378600020329     c                   if        %found(fifce01l)
378700020329     C*  somma algebrica del valore
378800030403     C                   move      wfcecbo       fcecbo
378900020329     C                   add       val_ice       fceice
379000020329     C                   update    fifce000
379100020329     C*
379200020329     c                   else
379300020329     C*
379400020329     C                   clear                   fifce000
379500020329     C                   z-add     ftdfgs        fceFGS
379600020329     c                   if        fceFGS=0
379700020329     C                   movel     ftdpdr        fceFGS
379800020329     c                   end
379900020329     C                   z-add     ftdpdr        fcePDR
380000020329     C                   move      ftdtsr        fceTSR
380100020329     C                   z-add     ftdndc        fceNDC
380200020329     C                   z-add     ftdddc        fceDDC
380300020329     C                   z-add     ftdaas        fceAAS
380400020329     C                   z-add     ftdlnp        fceLNP
380500020329     C                   z-add     ftdnrs        fceNRS
380600020329     C                   z-add     ftdnsp        fceNSP
380700030321     C                   move      wfcecbo       fcecbo
380800020403      *
380900020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
381000020329     c                   move      fcwcce        fcecce
381100020329     c                   else
381200020403     c                   exsr      CCE_da8A
381300020329     c                   end
381400020403      *
381500020329     C*  somma algebrica del valore
381600020329     C                   add       val_ice       fceice
381700020329     C                   write     fifce000
381800020329     c                   end
381900020417      *
382000020329     c                   endsr
382100020329      **************************************************************************
382200020329      * se è rimasto del resto da attribuire lo mette sull'ultima bolla
382300020329      **************************************************************************
382400020329     c     xResto        begsr
382500020329     c*
382600020329     C*  se c'è un resto ancora da attribuire lo ripartiamo sull'ultima
382700020329     C*  bolla letta.
382800020329     C*  quindi legge il dettaglio delle bolle per data documento
382900020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
383000020329     C     Kftd6         setgt     fiftd06l
383100020329     C     Kftd6         readpe    fiftd06l
383200020329     c                   else
383300020329     C     Kftd6_1       setgt     fiftd06l
383400020329     C     Kftd6_1       readpe    fiftd06l
383500020329     c                   end
383600020423      *
383700020423     c                   dow       not %eof(fiftd06l)
383800020329     C*
383900020423      * non deve avere un cod.giacenza/manc.consegna
384000020524      *  ma se tutte le bolle nel documento erano non da pagare
384100020524      *  allora occorre spalmare comunque.
384200020524     c     forza_pag     ifeq      'S'
384300020524     c                   move      'S'           W01pag
384400020524     c                   else
384500020423     C                   exsr      Se_daPagare
384600020524     c                   end
384700020524      *
384800020423     c     W01pag        ifeq      'S'
384900020329     C*
385000020329     C                   z-add     ftdfgs        fceFGS
385100020329     c                   if        fceFGS=0
385200020329     C                   movel     ftdpdr        fceFGS
385300020329     c                   end
385400020329     C                   z-add     ftdpdr        fcePDR
385500020329     C                   move      ftdtsr        fceTSR
385600020329     C                   z-add     ftdndc        fceNDC
385700020329     C                   z-add     ftdddc        fceDDC
385800020329     C                   z-add     ftdaas        fceAAS
385900020329     C                   z-add     ftdlnp        fceLNP
386000020329     C                   z-add     ftdnrs        fceNRS
386100020329     C                   z-add     ftdnsp        fceNSP
386200020329     C                   if        fcwtsr ='C' or fcwtsr ='R'
386300020329     c                   move      fcwcce        fcecce
386400020329     c                   else
386500020403     c                   exsr      CCE_da8A
386600020329     c                   end
386700020329     C*
386800020329     C     Kfce          chain     fifce01l
386900020329     c                   if        %found(fifce01l)
387000020329     C*  somma algebrica del valore
387100020329     C                   add       resto_ice     fceice
387200020329     C                   update    fifce000
387300020423     C* quindi dopo aver aggiornato può uscire forzatamente
387400020423     c                   leave
387500020329     c                   end
387600020329     C*
387700020329     c                   end
387800020423     C*
387900020423     C                   if        fcwtsr ='C' or fcwtsr ='R'
388000020423     C     Kftd6         readpe    fiftd06l
388100020423     c                   else
388200020423     C     Kftd6_1       readpe    fiftd06l
388300020423     c                   end
388400020423      *
388500020423     c                   enddo
388600020329     C*
388700020329     C                   endsr
388800020319     c*****************************************************************
388900020403     C*  Prende dalla 8A la voce di conto
389000020403     c*****************************************************************
389100020403     C     CCE_da8A      begsr
389200020403      *------
389300020403     C* deve prendere la voce di C/E dalla tabella 8A
389400020403     C                   clear                   ds8A
389500020403     C                   movel     '8A'          cod
389600020403     C                   MOVEL(P)  fcwt8A        KEY
389700020403     C     KTAB          CHAIN     TABEL00F                           32
389800020403     C  n32              movel     tbluni        ds8A
389900020403     C  n32              if        ftdtsr ='C'
390000020403     C                   if        §8ACE1 <> *zeros and §8ACE1 <> *blank
390100020403     C                   move      §8ACE1        fcecce
390200020403     c                   else
390300020403     C                   move      §8ACE3        fcecce
390400020403     c                   end
390500020403     c                   else
390600020403     C                   if        §8ACE2 <> *zeros and §8ACE2 <> *blank
390700020403     C                   move      §8ACE2        fcecce
390800020403     c                   else
390900020403     C                   move      §8ACE4        fcecce
391000020403     c                   end
391100020403     c                   end
391200020403      *------
391300020403     C                   endsr
391400020403     c*****************************************************************
391500020423      * controlla se la bolla è da pagare
391600020423     c*****************************************************************
391700020423     C     Se_daPagare   Begsr
391800020524      *------
391900020423     C                   movel     ' '           W01PAG
392000020423     C                   seton                                        70
392100020423      *------
392200020423     C* se bolla posta oppure no su azorg per linea di partenza
392300020423     C     ftdLNP        chain     AZORG01L                           74
392400020423     C   74              movel     *BLANKS       OG143
392500020423     C  N74              movel     ORGDE3        OG143
392600020423     C*
392700020423     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
392800020423      *         *--------------------------------------*
392900020423     c                   if        ftdtsr ='C'
393000020423     C* T I P O   B O L L A   D A   T A S S A R E
393100020423     C                   clear                   ARBCBO
393200020423     C     kftd3         chain     FNARB01L                           72
393300020423     C                   movel     W018C         WALF6             6
393400020423     C                   move      ftdfgs        WALF6
393500020423     C     WALF6         lookup    CBL                                    71
393600020423     C* SE INESISTENTE CERCO CON FILIALE A 0
393700020423     C                   if        *in71 = *off
393800020423     C                   move      000           WALF6
393900020423     C     WALF6         lookup    CBL                                    71
394000020423     C                   endif
394100020423     C   71              setoff                                       70
394200020423      *         *------------------*
394300020423     C* MANCATA  CONSEGNA  DA  TASSARE   (SE NON CONSEGNATA)
394400020423     C   70              if        ftddcm = 0
394500020423     C*         *------*
394600020423     C     ftdCMC        ifne      *BLANKS
394700020423     C                   movel     ftdCMC        WALF6
394800020423     C                   else
394900020423     C                   movel     ftdTMC        WALF6
395000020423     C                   end
395100020423     C*         *------*
395200020423     C                   MOVE      ftdfgs        WALF6
395300020423     C     WALF6         lookup    CMC                                    70
395400020423     C* SE INESISTENTE CERCO CON FILIALE A 0
395500020423     C                   if        *in70 = *off
395600020423     C                   move      000           WALF6
395700020423     C     WALF6         lookup    CMC                                    70
395800020423     C                   endif
395900020423     C*         *------*
396000020423     C                   end
396100020423      *         *------------------*
396200020423     C                   endif
396300020423      *         *--------------------------------------*
396400020423     C*  P.O. POSTE
396500020729     C     §OgNTW        IFEQ      'PPT'
396600020423     C                   setoff                                       70
396700020423      *
396800020423     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE
396900020423      * SIGNIFICA CHE LA BOLLA SI DEVE PAGARE COME CONSEGNATA
397000020423     C                   MOVEL     '2Z'          COD
397100020423     C                   MOVEL     *BLANKS       KEY
397200020423     C                   MOVEL     FTDCMC        KEY
397300020423     C     KTAB          CHAIN     TABEL                              70
397400020423     C                   clear                   DS2Z
397500020423     C  N70              movel     TBLUNI        DS2Z
397600020423     C* LA BOLLA VIENE PAGATA
397700020423     C     §2ZTPP        ifne      *BLANKS
397800020423     C                   seton                                        70
397900020423     C                   endif
398000020423     C                   end
398100020423     C*
398200020423     C*  Bolla da pagare
398300020423     c                   if        ftdtsr ='R' or *in70=*on
398400020423     C                   MOVEL     'S'           W01PAG            1
398500020423     C                   end
398600020423      *------
398700020611     C                   endsr
398800020611     c*****************************************************************         <-------
398900020611      * prima di cancellare salva i records di work per FCE a giornata          <-------
399000020611     c*****************************************************************         <-------
399100020611     C     Spia_Records  Begsr                                                  <-------
399200020612      *------                                                                   <-------
399300020612     c                   eval      fcspdr = fcwpdr                              <-------
399400020612     c                   eval      fcstsr = fcwtsr                              <-------
399500020612     c                   eval      fcsfgs = fcwfgs                              <-------
399600020612     c                   eval      fcsndc = fcwndc                              <-------
399700020612     c                   eval      fcsddc = fcwddc                              <-------
399800020612     c                   eval      fcscce = fcwcce                              <-------
399900020612     c                   eval      fcsice = fcwice                              <-------
400000020612     c                   eval      fcst8a = fcwt8a                              <-------
400100020612     c                   eval      fcskgc = fcwkgc                              <-------
400200020612     c                   eval      fcskgr = fcwkgr                              <-------
400300020612     c                   eval      fcsaas = fcwaas                              <-------
400400020612     c                   eval      fcslnp = fcwlnp                              <-------
400500020612     c                   eval      fcsnrs = fcwnrs                              <-------
400600020612     c                   eval      fcsnsp = fcwnsp                              <-------
400700020611     C                   write     fifcsw00                                     <-------
400800020612      *------                                                                   <-------
400900020611     C                   endsr                                                  <-------
401000081008      *****************************************************************
401100081008      * valore presunto costo carburante
401200081008      **********************************************************************
401300081008     C     caricofuel    BEGSR
401400130404      *
401500081008     c                   clear                   ficn74ds
401600081008      * totale importo Costo Autista x la giornata
401700081008     c                   eval      i74imp = fttitt + fttita + ftttpe +
401800081008     c                                      ftttbn + ftttim + fttmnt
401900081008     c                   z-add     Fttpdr        i74pdr
402000081008     c                   z-add     Fttddc        i74dtpre
402100081008     c                   move      'A'           i74tip
402200081008     c                   call      'FICN74R'
402300081008     c                   parm                    ficn74ds
402400081008      *
402500081008     c                   endsr
402600091106     c*--------------------------------------------------------------
402700091106     c* tramite Società e Unità decodifica P.IVA su PROJ
402800130404     c*--------------------------------------------------------------
402900091106     C     Decod_Rag_IVA BegSR
403000091106      **
403100091106      **  Routine x Reperire dati Fornitore:
403200091106      **    La routine in base alla sottonatura può funzionare
403300091106      **     x F=Fornitore/C=Cliente
403400091106     C                   clear                   trmz70s1ds                     Input
403500091106     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
403600091106     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
403700091106     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
403800091106     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
403900091106     c                   call      'TRMZ70SR1'
404000091106     C                   PARM                    trmz70s1ds                     Input
404100091106      *
404200130404     c                   clear                   TRS_Forn
404300130404     c                   clear                   TRS_Socie
404400130404     c                   clear                   TRS_Piva
404500091106     c                   if          S1_Trovato = *On and
404600091106     c                                S1_Errore = '0'
404700091106     c                   move      s1_KeyKSC     TRS_Forn
404800091106     c                   move      s1_Societa    TRS_Socie
404900130404     c                   movel     s1_PartitaIVA TRS_Piva
405000091106     c                   end
405100091106      *
405200091106     C                   ENDSR
405300091106     C/EJECT
405400130404     c*--------------------------------------------------------------
405500130404     c* tramite Società e Unità decodifica P.IVA su PROJ
405600130404     c*--------------------------------------------------------------
405700130404     C     Decod_P_IVA   BegSR
405800130404      **
405900130404      **  Routine x Reperire dati PIVA:
406000130404      **    La routine in base alla sottonatura può funzionare
406100130404      **     x F=Fornitore/C=Cliente
406200130404     C                   clear                   trmz70s4ds                     Input
406300130404     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
406400130404     C                   move      parm_S4CDF    s4_keyKSC                      Input
406500130404     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
406600130404     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
406700130404     c                   if        parm_S4FIL  > 0
406800130404     C                   movel(p)  parm_S4FIL    s4_Unita                       Input/Output
406900130404     c                   end
407000130404     c                   call      'TRMZ70SR4'
407100130404     c                   parm                    trmz70s4ds
407200130404      *
407300130404      *  se torna con l'errore riprova e aveva passato l'unità
407400130404      *   riprova passandogli l'unità vuota
407500130404     c                   if        s4_errore <> '0'
407600130404     C                   clear                   trmz70s4ds                     Input
407700130404     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
407800130404     C                   move      parm_S4CDF    s4_keyKSC                      Input
407900130404     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
408000130404     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
408100130404     c                   call      'TRMZ70SR4'
408200130404     c                   parm                    trmz70s4ds
408300130404     c                   end
408400130404      *
408500130404     C                   ENDSR
408600130404     C/EJECT
408700120821ab   C**************************************************************************
408800120821ab    * Controlla la Società operativa della Filiale
408900120821ab   C**************************************************************************
409000120821ab   C     CHK_SOC_OPE   begsr
409100120821ab   **
409200120821     c                   eval      soc_errata = NO
409300120821ab    /FREE
409400120821ab
409500120821EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
409600120821EDPDC                                             : dtVALIDITA
409700120821EDPDC                                             : 'O'
409800120821EDPDC                                             : valDatIniz
409900120821EDPDC                                             : valDatFine
410000120821ab                                                );
410100120821ab
410200120821EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
410300120821EDPDC    // Gestire l'errore.
410400120821
410500120821         eval  soc_errata = SI;
410600120821
410700120821ab     ENDIF;
410800120821ab
410900120821ab    /END-FREE
411000120821ab   C                   ENDSR
411100120821     C**************************************************************************
411200950531**
411300110928Nr.XXXXX  Distinta  ancora  da chiudere  con  data
411400950127Esistono ancora distinte chiuse da valorizzare con data
411500950127Esistono ancora spedizioni non partite con data ritiro
411600020322Non esistono autotrasp. da confermare nella selez.fatta
411700020618Attenzione: Presenti MANCA TARIFFA su distinte in data
411800951009Alcuni conteggi non caricati:in conferma da altri utent
411900000307Il totale conteggi è già stato calcolato:data distinta
412000090507Manca il Codice Fornitore sulle Tariffe dell'Autista
412100090507Manca il Codice Società   sulle Tariffe dell'Autista
412200020403Senza la tariffa firmata non si può fare il conteggio
412300020507Tariffe a prestazione/giornata senza data stampa sul
412400081008In Anagr.Aut. manca il peso della "Massa complessiva"
412500090512Attenzione: l'autista è stato disaccreditato.   In data
412600090513Senza accreditamento dell'autista non si può Confermare
412700120821Soc.Operativa non corrispondente in Tariffa  alla data
412800950531** MSG
412900020322ERRORE CONFERMA CONTEGGI AUTOTRASPORTATORI
413000020322Rilevato errore in conferma conteggi Padr.1234567 data dist.12345678
413100020328**
413200020328RGZPFM FILE(FIFCEW0F)
