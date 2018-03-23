000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020122     H* ficnb1R *----------------------------------------------------*
000300931015     H*         - CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500020122     Fficnb1D   CF   E             WORKSTN
000600931015     F                                     SFILE(FRB8SF2:NRR)
000700931021     F                                     SFILE(FRB8SF4:NRR4)
000800931021     F                                     SFILE(FRB8SF6:NRR6)
000900011105     F                                     SFILE(FRB8WS6:NRW6)
001000020408     F                                     infds(info)
001100100513     F*---------------------------------------------------------------------------------
001200100513      * Per Controllare che non ci siano le trasmissioni in sede in corso
001300100513      *  durante l'apertura di questo programma si scrive un record su questo file.
001400100513     Ffiwalc1l  UF A E           K DISK    commit
001500100513     F*---------------------------------------------------------------------------------
001600021212     FFNBLP01L  IF   E           K DISK
001700931015     FTABEL00F  IF   E           K DISK
001800090908     FTntbe01L  IF   E           K DISK
001900140115     FFIARBF2C  IF   E           K DISK
002000030418     f                                     prefix(K)
002100950116     F                                     IGNORE(FNARBD00)
002200991104     F                                     IGNORE(FIARBT00)
002300950116     F                                     IGNORE(FNARBG00)
002400950116     F                                     IGNORE(FNARBM00)
002500950116     F                                     IGNORE(FNARBV00)
002600991104     F                                     IGNORE(FNARBP00)
002700140115     F                                     IGNORE(FNARBN00)
002800950116     FFNLBL01L  IF   E           K DISK
002900020408     FFifce01L  UF   E           K DISK    commit
003000020408     FFIFTT01L  UF   E           K DISK    commit
003100060118     Ffiftt21L  iF   E           K DISK    RENAME(FIFTT000:FIFTT021)
003200011105     FFIFTT02L  IF   E           K DISK
003300011105     F                                     RENAME(FIFTT000:FIFTT002)
003400020328      *
003500020328      * work file per attribuire a stop i valori sulle voci di C/E
003600020328     Ffiftdw1L  UF A E           K DISK    usropn
003700020328      *
003800931202     F* DETTAGLIO CONSEGNE E RITIRI
003900020408     Ffiftd01L  UF   E           K DISK    commit
004000011105     F                                     RENAME(fiftd000:fiftd001)
004100931102     F                                     INFDS(TARDS)
004200931202     F* DETTAGLIO DELLE CONSEGNE
004300060126     Ffiftd22L  UF   E           K DISK    commit
004400011105     F                                     RENAME(fiftd000:fiftd002)
004500020315     F* DETTAGLIO DEI nuovi RITIRI
004600020408     Ffiftd05L  UF   E           K DISK    commit
004700020315     F                                     RENAME(fiftd000:fiftd005)
004800020315     F* DETTAGLIO DEI RITIRI
004900020408     Ffiftd04L  UF   E           K DISK    commit
005000011105     F                                     RENAME(fiftd000:fiftd004)
005100020408     Ffiftd00F  UF   E             DISK    commit
005200011112     FFIFGT01L  IF   E           K DISK
005300091112     FFITGT01L  IF   E           K DISK
005400021202     Ffiapd01L  IF   E           K DISK
005500950113     FFNARB01L  IF   E           K DISK
005600090918     Ffiar601L  IF   E           K DISK
005700000622     FAZORG01L  IF   E           K DISK
005800931103     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005900930707     F*
006000020408      * record del sfl per gestire correttamente il posizionamento del sfl
006100020408     D info            DS
006200020408     D  nrrult               378    379B 0
006300020408     F*
006400911017     D L1              S              3  0 DIM(30)                              FIL GESTITE
006500950112     D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
006600931108     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
006700931202     D DTS             S             15    DIM(30)                              DESCR.TIP PRESTAZ
006800931202     D CTS             S              3  0 DIM(30)                              CDTAR TIP PRESTAZ
006900040308     D CMC             S              6    DIM(9999)                            MANC.CONS DA TASS
007000040308     D CBL             S              6    DIM(9999)                            CD.BOL DA NO TASS
007100931018     D K17             S              1    DIM(17)                              SKI 17 ALFABETICA
007200931104     D*
007300090908     DKCliPar          S              7s 0 DIM(9999)                            Tar.Part.sul cliente
007400090909     DKtutteBolle      S              1A
007500090908     D*
007600030519     D ERR             S             72    DIM(3) CTDATA PERRCD(1)              MSG DI ERR VID
007700931103     D STA             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE STAMPA
007800931104     D SER             S             25    DIM(2) CTDATA PERRCD(1)              MSG DI ERR STA
007900020328     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              COMANDI da eseguire
008000950125     D* PASSAGGIO DATI DA CONFERMA CONTEGGI              - FNLRC0R -
008100931028     D PARAM           DS
008200931028     D  PARPDR                 1      7  0
008300931028     D  PARTSR                 8      8
008400950119     D  PARDDC                 9     16  0
008500931028     D  PARSML               256    256
008600950113     D* PASSAGGIO DATI A INTERROGAZIONE BOLLE ARRIVI     - FNLR36R -
008700931021     D PARAM2          DS
008800931021     D  GIA                    1     12  0 INZ
008900931021     D  RICH                  13     13    INZ
009000950113     D  PA2AAS                14     17  0
009100950113     D  PA2LNP                18     20  0
009200950113     D  PA2NRS                21     22  0
009300950113     D  PA2NSP                23     29  0
009400950113     D  PA2FLG                31     31
009500950116     D* PASSAGGIO DATI A RICERCA CODICE TARIFFA PADRONCINO-TNTA86R -
009600931028     D PARAM3          DS
009700931108     D  PA3PDR                 1      7  0
009800931108     D  PA3SML                 8      8
009900931108     D  PA3TSR                 9      9
010000931108     D  PA3CTR                11     13  0
010100931108     D  PA3PRG                14     16  0
010200931108     D  PA3SEL                17     17
010300020326      *
010400020326     D DSLS05          DS
010500020326     D  PA0AAS                 1      4  0
010600020326     D  PA0LNP                 5      7  0
010700020326     D  PA0NRS                 8      9  0
010800020326     D  PA0NSP                10     16  0
010900020326     D  PA0F03                17     17
011000020326     D  PA0FLG                18     18
011100020326     D  PA0A9                 19     19
011200020326     D  PA0B5                 20     20
011300020326     D  PA075                 21     21
011400020326     D  PA098                 22     22
011500020326     D  PA0RSU                23     42
011600020326     D  PA0CML                43     43
011700020326     D  PA0SIM                44     46  0
011800020326      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
011900020326     D  PA0CLI                47     47
012000020326      * PA0GIA indica se il pgm e' richiamato dalle giacenze
012100020326     D  PA0GIA                48     48
012200020326      *
012300950112     D* DS PER TRUL06R - CARICAMENTO £X
012400950112     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012500950112     D  LIN                    1     90  0
012600950112     D                                     DIM(30)                              FIL. COMODO
012700920810     D KPJBA         E DS
012800100513     d   KPJBUS        s                   like(KPJBU)
012900910423     D WLBDAT          DS
013000950112     D  G02DAT                 1      8  0
013100950112     D  G02INV                 9     16  0
013200950112     D  G02ERR                17     17
013300950112     D  G02TGI                18     22  0
013400910423     D WGIDAT          DS
013500910423     D  GIODAT                 1      6  0
013600910423     D  GIOINV                 7     12  0
013700910423     D  GIOTGI                13     17  0
013800931104     D TCUDS           DS
013900931104     D  F1                     1      1
014000931104     D  F3                     3      3
014100931104     D  F2                     2      2
014200931104     D  F4                     4      4
014300931104     D  F56                    5      6
014400931102     D TARDS           DS
014500931102     D  FTDNRR               397    400B 0
014600931018     D                 DS
014700931019     D  AA                     1      2
014800931019     D  MM                     3      4
014900931019     D  GG                     5      6
015000931018     D  DATA                   1      6  0
015100931028     D                 DS
015200931028     D  VH4TSR                 1      1
015300950116     D  ARBCBO                 2      3
015400950224     D  W018C                  1      3
015500931015     D                 DS
015600931015     D  VI1PF1                 1      3  0
015700931015     D  VI1PD1                 4      7  0
015800931103     D  VI1P1                  1      7  0
015900931015     D                 DS
016000931015     D  VI1PF2                 1      3  0
016100931015     D  VI1PD2                 4      7  0
016200931103     D  VI1P2                  1      7  0
016300931018     D                 DS
016400931018     D  VI1ND0                 1      7  0
016500931018     D  VI1ND1                 8     14  0
016600931018     D  VI1ND2                15     21  0
016700931018     D  VI1ND3                22     28  0
016800931018     D  VI1ND4                29     35  0
016900931018     D  VI1ND5                36     42  0
017000931018     D  VI1ND6                43     49  0
017100931018     D  VI1ND7                50     56  0
017200950112     D  VI1ND8                57     63  0
017300931018     D  VI1ND9                64     70  0
017400931018     D  NDC                    1     70  0
017500931018     D                                     DIM(10)                              NUM DISTINT SELEZ
017600931102     D                 DS
017700950116     D  FTDAAS                 1      4  0
017800950116     D  FTDNRS                 5      6  0
017900950116     D  FTDNSP                 7     13  0
018000950116     D  FTDLNP                14     16  0
018100950116     D  FTDSPC                 1     16
018200931202     D                 DS
018300931202     D  FTDKSC                 1      7  0
018400950116     D  FTDRSC                 8     42
018500950116     D  FTDSPR                 1     42
018600950116     D                 DS
018700950116     D  VI4PDR                 1      7  0
018800950116     D  VI4FIL                 1      3  0
018900931021     D DS1Z          E DS
019000000622     D DS3A          E DS
019100000623     D DS2Z          E DS
019200090909     D   DCTP        E DS
019300000622     D OG143         E DS
019400021212     D DS1B          E DS
019500170130     D FNLVF3DS      E DS
019600030821     D FICNB1        E DS                  EXTNAME(FICNB1DS)
019700020122     D FICNB2        E DS                  EXTNAME(FICNB2DS)
019800020603     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
019900931021     D CNCR80        E DS
020000900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
020100100513      *
020200100513     D ddatiute      e ds                  prefix(UT_)
020300100513     D azuteds       e ds                  extname(AZUTE00F)
020400100513     D tibs34ds      E DS                  inz
020500100513      *---------------------------------------------------------------------*
020600100513     D FICNB1ds0     E DS
020700040622      *---------------------------------------------------------------------*
020800040825     d   tar_valida    S              1    INZ('N')
020900040825     d rifPDR          s                   like(fgtPDR)
021000040825     d rifCTR          s                   like(fgtCTR)
021100040825     d ptarv_rap       s                   like(fgtrap)
021200040825     d ptarv_rct       s                   like(fgtrct)
021300040622     d dataoggi        s               d   datfmt(*iso)
021400040622     d finoadata       s               d   datfmt(*iso) inz(d'2004-08-10')
021500090911     d SAV_TarPartCLI  s                   like(W01_TarPartCLI)
021600090911     d SAV_UnoOpiuCLI  s                   like(W01_UnoOpiuCLI)
021700020328      *---------------------------------------------------------------------*
021800020328     D  MSGDS          S            100
021900020328     D  LENGH          S             15  5
022000020328      *---------------------------------------------------------------------*
022100931018     D***
022200950118     D* DEFINIZIONE COSTANTI
022300931018     D***
022400931018     D CDAVAL          C                   CONST('DA VALORIZZARE')
022500931018     D CVALOR          C                   CONST('VALORIZZATE   ')
022600931123     D CMITT           C                   CONST('--M I T T E N T E-- ')
022700931202     D CSPED           C                   CONST('---- SPEDIZIONE ----')
022800950119     D COS1            C                   CONST('Uscita: MAT')
022900950119     D COS2            C                   CONST('Uscita: POM')
023000950119     D COS3            C                   CONST('  Piking  ')
023100950119     D COS4            C                   CONST('  Etich.  ')
023200021211     D COS5            C                   CONST('Caric.')
023300021211     D COS6            C                   CONST(' Scar.')
023400011105     IFIFTT002
023500000714     I              FTTATB                      FT2ATB
023600011105     I              FTTpdu                      FT2pdu
023700000714     I              FTTPDR                      FT2PDR
023800000714     I              FTTTSR                      FT2TSR
023900021211     I              FTTlvl                      FT2FCS
024000011105     I              FTTdiv                      FT2div
024100011105     I              FTTfgs                      FT2fgs
024200000714     I              FTTNDC                      FT2NDC
024300000714     I              FTTDDC                      FT2DDC
024400000714     I              FTTFVL                      FT2FVL
024500000714     I              FTTFLA                      FT2FLA
024600000714     I              FTTSET                      FT2SET
024700000714     I              FTTSNT                      FT2SNT
024800000714     I              FTTSNP                      FT2SNP
024900000714     I              FTTSNE                      FT2SNE
025000000714     I              FTTSNA                      FT2SNA
025100000714     I              FTTCLA                      FT2CLA
025200000714     I              FTTCPE                      FT2CPE
025300000714     I              FTTKFA                      FT2KFA
025400000714     I              FTTTVL                      FT2TVL
025500000714     I              FTTITT                      FT2ITT
025600000714     I              FTTFIT                      FT2FIT
025700000714     I              FTTITA                      FT2ITA
025800000714     I              FTTFIA                      FT2FIA
025900000714     I              FTTTPE                      FT2TPE
026000000714     I              FTTTBN                      FT2TBN
026100000714     I              FTTTIM                      FT2TIM
026200000714     I              FTTMNT                      FT2MNT
026300000714     I              FTTDFT                      FT2DFT
026400011105     I              FTTNFF                      FT2NFT
026500000714     I              FTTTBS                      FT2TBS
026600000714     I              FTTCUC                      FT2CUC
026700000714     I              FTTFPP                      FT2FPP
026800000714     I              FTTPEP                      FT2PEP
026900000714     I              FTTDCV                      FT2DCV
027000011105     I              FTThlv                      FT2hlv
027100011105     I              FTTctg                      FT2ctg
027200011105     I              FTTdcn                      FT2dcn
027300011105     I              FTTsoc                      FT2soc
027400011105     I              FTTflr                      FT2flr
027500011105     I              FTTftr                      FT2ftr
027600011105     I              FTTdtr                      FT2dtr
027700000000     I/SPACE 3
027800900521     C****************************************************************
027900900521     C*  RIEPILOGO INDICATORI
028000900521     C***************************************************************
028100931018     C* 01    - IMMESSA SOLO DATA DAL
028200931018     C* 02    - IMMESSA DATA AL
028300950112     C* 03    -
028400931108     C* 04    - IMMESSO TIPO PRESTAZIONE
028500950116     C* 05    - IMMESSO UN SOLO NUMERO DISTINTA (SOLO PER CONSEGNE)
028600931201     C* 06    - FASE DI SIMULAZIONE
028700931124     C* 09    - LEGGO FLARBF1C CON I CAMPI DEL LEGAME BOLLA
028800931124     C* 11    - PROTEGGO COD PADRONCINO IN SFL4 PER NON RIPETERLO
028900931103     C* 12    - PROTEGGO COD PADRONCINO IN SFL2 PER NON RIPETERLO
029000950117     C* 13    - PROTEGGO IMPORTO ACCESSORI IN SFL6 PER NON INSERIRLI
029100931022     C* 14    - ANNULLO TASSAZIONI SINGOLE DI BOLLE SE DISTINTA TASSATA
029200931022     C*         IN MODO COMPLETO DA VIDEO
029300931022     C* 15    - IMMESSO IMPORTO BASE      A LIVELLO DI DISTINTA
029400931022     C* 16    - IMMESSO IMPORTO ACCESSORI A LIVELLO DI DISTINTA
029500931022     C* 17    - ON SPEDIZIONE DA NON TASSARE
029600931102     C* 18    - ON MANCA TARIFFA PER MANCANZA SOMMA PESI
029700931104     C* 19    - ON E' RITIRO   OFF E' CONSEGNA
029800931028     C* 20    - ON PROGRAMMA RICHIAMATO
029900931105     C* 21    - ON SVALORIZZATE DISTINTE: RIEMETTO SFL2
030000931110     C* 22    - ON OBBLIGO CMD6 SE AGGIORNO DISTINTA CON MANCA TARIFFA
030100950117     C* 23    - RITORNO ALL'INCASSO E PROTEZIONE IMPORTO BASE
030200950131     C* 24    - DISTINTA VALORIZZATA
030300000626     C* 25    - NON SI PAGA LA SPEDIZIONE PER BOLLE POSTE NON CONSEGNATE
030400011109     C* 26    - PROTECT DIVISA
030500931022     C* 30/32 - DI COMODO
030600000622     C* 34    - DI COMODO
030700931018     C* 35    - PULIZIA ES EMISSIONE SFL
030800931018     C* 36    - SFLNXTCHG
030900931110     C* 37    - EMISSIONE SFL6
031000000622     C* 40/48 - ERRORI
031100000626     C* 49    - PROTEGGO IL CAMPO NUMERO STOÈP PER BOLLE POSTE
031200000622     C* 50    - EMISSIONE ERRORE TARIFFA ERRATA BARTOLINI INESISTENTI
031300000622     C* 51    - EMISSIONE ERRORE TARIFFA ERRATA POSTE INESISTENTI
031400000622     C* 52/53 - TARIFFA ERRATA
031500011108     C* 60    - ON protegge le righe del windowsfl
031600011108     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
031700900521     C*****************************************************************
031800100513     C                   clear                   ficnb1ds0
031900100513     c                   eval      FN0PGM  = 'FICNB1R'
032000100513     c                   eval      FN0TIPO = 'FILIALE'
032100100513     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
032200100513     C                   MOVEL     KPJBU         kpjbus
032300100513     C                   clear                   kpjbu
032400100513     C                   MOVEL     ficnb1ds0     kpjbu
032500100513     c                   call      'FICNB1R0'
032600100513     c                   parm                    kpjba
032700100513     C                   MOVEL     kpjbu         ficnb1ds0
032800100513     C*   se già utilizzata da altro utente della stessa filiale fa uscire
032900100513     C*   forzatamente.
033000100513     c                   if        Fn0FLG <> *blank
033100100513     c                   goto      FINE
033200100513     c                   end
033300100513     C*
033400100513     C                   MOVEL     KPJBUs        kpjbu
033500100513     C*
033600931015     C     INIZIO        TAG
033700931015     C* PULIZIA FORMATO 1
033800931103     C  N20              EXSR      PUL01
033900930426     C*
034000931015     C     FOR01         TAG
034100931028     C*
034200931028     C* PGM NON RICHIAMO - EMETTO 1 VIDEATA ALTRIMENTI VADO ALLA 2A
034300931105     C* SE SVALORIZZATE SPEDIZIONI EMETTO SUBITO LA 2A (21 ON)
034400931028     C     *IN20         IFEQ      *OFF
034500931105     C     *IN21         ANDEQ     *OFF
034600931028     C*
034700931015     C                   EXFMT     FRB8D01
034800170130     C*
034900020409     c                   z-add     0             savnr2
035000020409     c                   z-add     0             savnr4
035100020409     c                   z-add     0             savnr6
035200020604     c                   move      'N'           seF6              1
035300170130     C*
035400020604      * se richieste le distinte già Valorizzate non deve permettere di rivalorizzare
035500020604      * quindi imposto la memoria del flag SeF6 come se fosse stato dato l'F6 di
035600020604      * valorizzazione e impedire un nuovo F6 che raddoppia i conteggi.
035700020604     c                   if        vi1fvl ='V'
035800020604     c                   move      'S'           seF6
035900020604     c                   end
036000170130     C*
036100931103     C                   SETOFF                                       4746
036200920323     C** CMD3 - FINE LAVORO
036300931028     C   KC              GOTO      FINE
036400170130     C*
036500170130      **--  F8= Gestione Distinte Assenze in presenza di Anomalie
036600170130     C   KH              exsr      gesDISTASS
036700170130      **--
036800931015     C* CONTROLLI FORMATO1
036900900528     C                   EXSR      CONTR1
037000170130      **--
037100900528     C   90              GOTO      FOR01
037200931028     C                   ENDIF
037300931018     C*
037400931018     C* CARICO DISTINTE SELEZIONATE
037500931018     C                   EXSR      CARNDC
037600950616     C   90
037700950616     CAN 20              GOTO      FINE
037800950616     C   90              GOTO      FOR01
037900900611     C*
038000931119     C                   Z-ADD     1             REC2
038100900518     C*
038200931018     C     FORCT2        TAG
038300931103     C  N90              WRITE     FRB8D03
038400020603     c                   if        savnr2 > 0 and savnr2 <= rec2
038500020409     c                   z-add     savnr2        rec2
038600020409     c                   end
038700931018     C                   EXFMT     FRB8CT2
038800020409     c                   z-add     nrrult        savnr2            5 0
038900020411     c                   z-add     0             savnr4
039000931028     C   KC              GOTO      FINE
039100931028     C   KL
039200931028     CANN20              GOTO      FOR01
039300931028     C   KL
039400931028     CAN 20              GOTO      FINE
039500931018     C*
039600931018     C** CMD1 O ENTER - SELEZIONE DISTINTE
039700931018     C                   EXSR      SELNDC
039800931018     C   90              GOTO      FORCT2
039900931018     C*
040000931103     C                   Z-ADD     1             REC4
040100931018     C     FORCT4        TAG
040200020604      *
040300020604      * se ha già eseguito la valorizzazione non deve permettere un secondo
040400020604      * F6 a causa di un manca tariffa
040500020604     c     seF6          comp      'N'                                    88
040600020604      *
040700931103     C  N90              WRITE     FRB8D05
040800020409     c                   if        savnr4 > 0
040900020409     c                   z-add     savnr4        rec4
041000020409     c                   end
041100931018     C                   EXFMT     FRB8CT4
041200020604      *
041300020409     c                   z-add     nrrult        savnr4            5 0
041400020411     c                   z-add     0             savnr6
041500960906     C                   SETOFF                                       90
041600931110     C* SE AGGIORNATO DISTINTA CON DEI MANCA TARIFFA OBBLIGO IN CMD6
041700931110     C     *INKL         IFEQ      *ON
041800931110     C     *INKC         OREQ      *ON
041900931110     C   22              SETON                                        4490
042000931110     C   22              GOTO      FORCT4
042100931110     C                   ENDIF
042200130305     C*
042300931105     C   KC              GOTO      FINE
042400931110     C   KL
042500931110     CANN20              GOTO      FORCT2
042600931110     C   KL
042700931110     CAN 20              GOTO      FINE
042800931018     C* CONTROLLO SFL4
042900931018     C                   EXSR      CONTR4
043000931103     C* CMD3 - DALLA VIDEATA DI MANUTENZIONE SPEDIZIONI
043100931019     C   LR              GOTO      FINE
043200931019     C* ERRORI
043300931105     C   90              GOTO      FORCT4
043400931105     C* RIEMETTO IL SFL2 PERCHE' HO ANNULLATO DELLE VALORIZZAZIONI
043500931105     C   21              GOTO      FOR01
043600931105     C* CMD6 - ESEGUO VALORIZZAZIONE
043700020604     c   kf              move      'S'           seF6
043800931105     C   KF              EXSR      VALZZ
043900020408     C   kf              commit
044000931105     C  NKF
044100931105     COR KF
044200931105     CAN 90              GOTO      FORCT4
044300931028     C  N20              GOTO      INIZIO
044400900611     C*
044500000000     C     FINE          TAG
044600100513     C*
044700100513     C*  deve cancellare il record di allocazione x impedire le trasmissioni
044800100513     C*   in sede durante l'utilizzo di questo programma.
044900100513     c     parsml        ifeq      *blank
045000100513     C     Kwalc         setll     fiwalc1l
045100100513     C     Kwalc         reade     fiwalc1l
045200100513     c                   dow       not %eof(fiwalc1l)
045300100513     c                   delete    fiwalc00
045400100513     C     Kwalc         reade     fiwalc1l
045500100513     c                   end
045600100513     c                   end
045700100513      *
045800020408     c                   commit
045900020408     C*
046000020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
046100020328     C*   economico.
046200020328     C                   clear                   FICNB2
046300020328     C                   move      'C'           §taflg
046400020328     C                   CALL      'FICNB2R'
046500020328     C                   PARM                    FICNB2
046600020328     C*
046700020328     c                   close     fiftdw1l
046800020328     C*
046900020422     c                   if        parsml=' '
047000051109     c                   goto      norgzpfm
047100020328     C                   clear                   MSGDS
047200020328     C                   MOVEL     CMD(1)        MSGDS
047300020328     C                   Z-ADD     80            LENGH
047400020328     C                   CALL      'QCMDEXC'                            99
047500020328     C                   PARM                    MSGDS
047600020328     C                   PARM                    LENGH
047700051109     c     norgzpfm      tag
047800051109     c                   end
047900020328     C*
048000931028     C                   SETON                                        LR
048100950112     C****************************************************************
048200950112     C     *INZSR        BEGSR
048300950112     C*
048400950112     C     *ENTRY        PLIST
048500950112     C                   PARM                    KPJBA
048600950112     C                   MOVEL     KPJBU         PARAM
048700100513      *
048800100513     c     *dtaara       define    §azute        azuteds
048900100513     c     *dtaara       define    §datiute      ddatiute
049000100513     C                   in(E)     *dtaara
049100100513     C                   IF        %Error  or  RSUT = *blanks
049200100513     C                   call      'TIBS34R'
049300100513     C                   parm                    Tibs34Ds
049400100513     C                   in        *dtaara
049500100513     c                   ENDIF
049600950112     C*
049700950112     C                   Z-ADD     1             CODUT
049800950112     C                   CALL      'X§PARUT'
049900950112     C                   PARM                    UT§DSE
050000950112     C                   MOVEL     RAGUT         RSUT             20
050100950112     C                   MOVEL     REC80         CNCR80
050200020328     C*
050300020328     C* apertura workf per ripartizione voci di conto economico
050400020328     c                   open      fiftdw1l
050500950112     C*
050600950112     C* DEFINIZIONE CHIAVI DI ACCESSO
050700100513     C     Kwalc         KLIST
050800100513     C                   KFLD                    WalPGM
050900100513     C                   KFLD                    WalNOJ
051000100513     C                   KFLD                    WalNUS
051100100513     C                   KFLD                    WalNRJ
051200090908      *
051300090908     C     KTbe          KLIST
051400090908     C                   KFLD                    tbeCOD
051500090908     C                   KFLD                    tbeKE1
051600090908     C                   KFLD                    tbeKE2
051700090908      *
051800950112     C     KTAB2         KLIST
051900950112     C                   KFLD                    CODUT
052000950112     C                   KFLD                    COD               2
052100950112     C     KTAB          KLIST
052200950112     C                   KFLD                    CODUT
052300950112     C                   KFLD                    COD               2
052400950112     C                   KFLD                    KEY               8
052500950113     C     KFTT          KLIST
052600950113     C                   KFLD                    VI4PDR
052700950113     C                   KFLD                    VH4TSR
052800950113     C                   KFLD                    VI4NDC
052900950116     C                   KFLD                    VH4DDC
053000950112     C     KFTT1         KLIST
053100950112     C                   KFLD                    W01PDR
053200950112     C                   KFLD                    VI1TSR
053300950112     C                   KFLD                    W01NDC
053400950112     C     KFTT3         KLIST
053500950112     C                   KFLD                    W01PDR
053600950112     C                   KFLD                    VI1TSR
053700060118     C     K21pdr        KLIST
053800060118     C                   KFLD                    VI1fvl
053900060118     C                   KFLD                    W01PDR
054000060118     C     K21pdr1       KLIST
054100060118     C                   KFLD                    VI1fvl
054200060118     C                   KFLD                    W01PDR
054300060118     C                   KFLD                    VI1TSR
054400060118     C                   KFLD                    W01NDC
054500060118     C     K21pdr3       KLIST
054600060118     C                   KFLD                    VI1fvl
054700060118     C                   KFLD                    W01PDR
054800060118     C                   KFLD                    VI1TSR
054900091112     C     Ktgt          KLIST
055000091112     C                   KFLD                    fgtPDR
055100091112     C                   KFLD                    fgtSML
055200091112     C                   KFLD                    fgtPRG
055300950112     C     KFGT          KLIST
055400950112     C                   KFLD                    VI4PDR
055500950112     C                   KFLD                    PARSML
055600950112     C                   KFLD                    VH4TSR
055700950112     C     KFGT2         KLIST
055800950112     C                   KFLD                    VI4PDR
055900950112     C                   KFLD                    PARSML
056000950112     C                   KFLD                    VH4TSR
056100950112     C                   KFLD                    W01CTR
056200040825      *
056300040825     C     KFGT_rif      KLIST
056400040825     C                   KFLD                    rifPDR
056500040825     C                   KFLD                    PARSML
056600040825     C                   KFLD                    VH4TSR
056700040825     C                   KFLD                    rifCTR
056800950112     C     KFTD          KLIST
056900950112     C                   KFLD                    VI4PDR
057000950112     C                   KFLD                    VH4TSR
057100950112     C                   KFLD                    VI4NDC
057200950116     C                   KFLD                    VH4DDC
057300950112     C     KFTD2         KLIST
057400950116     C                   KFLD                    VH6AAS
057500950116     C                   KFLD                    FTDLNP
057600950116     C                   KFLD                    FTDNRS
057700950116     C                   KFLD                    VI6NSP
057800950116     C                   KFLD                    VH4TSR
057900950112     C                   KFLD                    VI4PDR
058000950112     C                   KFLD                    VI4NDC
058100950116     C                   KFLD                    VH4DDC
058200020315     C     KFTD5         KLIST
058300020315     C                   KFLD                    VI4PDR
058400020315     C                   KFLD                    VH4TSR
058500020315     C                   KFLD                    VI4NDC
058600020315     C                   KFLD                    VH4DDC
058700020315     C                   KFLD                    VH6AAS
058800020315     C                   KFLD                    FTDLNP
058900020315     C                   KFLD                    FTDNRS
059000020315     C                   KFLD                    VI6NSP
059100091012     C     Bolla_FTD     KLIST
059200950112     C                   KFLD                    FTDAAS
059300950112     C                   KFLD                    FTDLNP
059400950112     C                   KFLD                    FTDNRS
059500950112     C                   KFLD                    FTDNSP
059600091012      *
059700950112     C     KFTD4         KLIST
059800950112     C                   KFLD                    VI4PDR
059900950112     C                   KFLD                    VH4TSR
060000950112     C                   KFLD                    VI4NDC
060100950116     C                   KFLD                    VH4DDC
060200950112     C                   KFLD                    VI6NSP
060300950116     C                   KFLD                    FTDRSC
060400000622     C     KFTDT         KLIST
060500000622     C                   KFLD                    VI4PDR
060600000622     C                   KFLD                    VH4TSR
060700000714     C                   KFLD                    FT2NDC
060800000622     C                   KFLD                    VH2DDC
060900000714     C     KFTTT         KLIST
061000000714     C                   KFLD                    VI4PDR
061100000714     C                   KFLD                    VI1FVL
061200000714     C                   KFLD                    VH2DDC
061300000714     C                   KFLD                    VH4TSR
061400950112     C     KLBL          KLIST
061500950112     C                   KFLD                    LBLAAN
061600950112     C                   KFLD                    LBLLPN
061700950112     C                   KFLD                    LBLNRN
061800950112     C                   KFLD                    LBLNSN
061900041124      *
062000041124     C     Del_FCE       KLIST
062100041124     C                   KFLD                    fgs_vi4pdr        3 0
062200041124     C                   KFLD                    VI4PDR
062300041124     C                   KFLD                    VH4TSR
062400041124     C                   KFLD                    VI4NDC
062500041124     C                   KFLD                    VH4DDC
062600020326      *
062700020326     c     Kfce          Klist
062800020326     C                   kfld                    ftdfgs
062900020326     C                   kfld                    ftdpdr
063000020326     C                   kfld                    ftdtsr
063100020326     C                   kfld                    ftdndc
063200020326     C                   kfld                    ftdddc
063300020326     C                   kfld                    ftdaas
063400020326     C                   kfld                    ftdlnp
063500020326     C                   kfld                    ftdnrs
063600020326     C                   kfld                    ftdnsp
063700020326      *
063800020326     c     KfceD         Klist
063900020326     C                   kfld                    vh6fgs
064000020326     C                   kfld                    vi4pdr
064100020326     C                   kfld                    vh6tsr
064200020326     C                   kfld                    vi4ndc
064300020326     C                   kfld                    vh4ddc
064400020326     C                   kfld                    vh6aas
064500020326     C                   kfld                    ftdlnp
064600020326     C                   kfld                    ftdnrs
064700020326     C                   kfld                    vh6nsp
064800020328     C*
064900020422     c     KftdW0        Klist
065000020328     C                   kfld                    kftwfgs
065100020328     C                   kfld                    kftwpdr
065200020328     C                   kfld                    kftwtsr
065300020328     C                   kfld                    kftwndc
065400020328     C                   kfld                    kftwddc
065500020422     C*
065600020422     c     KftdW         Klist
065700020422     C                   kfld                    kftwfgs
065800020422     C                   kfld                    kftwpdr
065900020422     C                   kfld                    kftwtsr
066000020422     C                   kfld                    kftwndc
066100020422     C                   kfld                    kftwddc
066200020328     C                   kfld                    kftwstp
066300021202     c     Kapdv         Klist
066400021202     C                   kfld                    apdtip
066500021202     C                   kfld                    vi1p1
066600021202     c     Kapdf         Klist
066700021202     C                   kfld                    apdtip
066800021202     C                   kfld                    fttpdr
066900021202     c                   eval      apdtip = 'A'
067000950112     C***
067100950112     C* 20 ON - PROGRAMMA RICHIAMATO
067200950112     C     PARTSR        IFNE      ' '
067300950112     C                   EXSR      PUL01
067400950112     C                   SETON                                        20
067500950112     C                   MOVEL     'V'           VI1FVL
067600950112     C                   MOVEL     PARPDR        VI1P1
067700950112     C                   MOVEL     PARTSR        VI1TSR
067800950112     C                   ENDIF
067900950112     C***
068000950112     C* 06 ON - FASE DI SIMULAZIONE
068100950112     C     PARSML        IFEQ      'S'
068200950112     C                   SETON                                        06
068300950112     C                   ENDIF
068400950112     C***
068500950112     C* CARICO TABELLA FILIALI GESTITE £1
068600950112     C                   CLEAR                   DSUL06
068700950112     C                   MOVE      '£1'          D06COD
068800020717     C                   MOVEL     SIMPOU        D06KEY
068900950112     C                   MOVEL     ' '           D06TLA
069000100513     C                   MOVEL     KPJBU         kpjbus
069100100513     C                   clear                   kpjbu
069200950112     C                   MOVEL     DSUL06        KPJBU
069300950112     C                   CALL      'TRUL06R'
069400950112     C                   PARM                    KPJBA
069500950112     C                   MOVEL     KPJBU         DSUL06
069600950112     C                   MOVEA     LIN           L1
069700100513     C                   MOVEL     KPJBUs        kpjbu
069800950112     C***
069900950112     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
070000950112     C                   CLEAR                   DSUL06
070100950112     C                   MOVE      '£6'          D06COD
070200020717     C                   MOVEL     SIMPOU        D06KEY
070300950112     C                   MOVEL     'L'           D06TLA
070400100513     C                   MOVEL     KPJBU         kpjbus
070500100513     C                   clear                   kpjbu
070600950112     C                   MOVEL     DSUL06        KPJBU
070700950112     C                   CALL      'TRUL06R'
070800950112     C                   PARM                    KPJBA
070900950112     C                   MOVEL     KPJBU         DSUL06
071000950112     C                   MOVEA     LIN           L6
071100100513     C                   MOVEL     KPJBUs        kpjbu
071200950112     C*
071300950112     C***
071400950112     C* CARICO TABELLA DEI TIPI PRESTAZIONE
071500950112     C                   Z-ADD     1             C
071600950112     C                   MOVEL     '1Z'          COD
071700950112     C     KTAB2         SETLL     TABEL
071800950112     C     KTAB2         READE     TABEL                                  30
071900950112     C*
072000950112     C     *IN30         DOWEQ     *OFF
072100950112     C     TBLFLG        IFEQ      ' '
072200950112     C                   MOVEL     TBLUNI        DS1Z
072300950112     C     §1ZUPD        IFEQ      'S'
072400950112     C                   MOVEL     TBLKEY        TSR(C)
072500950112     C                   MOVEL     §1ZDES        DTS(C)
072600950112     C                   MOVEL     §1ZCTR        CTS(C)
072700950112     C                   ADD       1             C
072800950112     C                   ENDIF
072900950112     C                   ENDIF
073000950112     C     KTAB2         READE     TABEL                                  30
073100950112     C                   ENDDO
073200950112     C***
073300950112     C* CARICO MANCATE CONSEGNE DA TASSARE
073400950112     C                   Z-ADD     1             C
073500950112     C                   MOVEL     '8B'          COD
073600950112     C     KTAB2         SETLL     TABEL
073700950112     C     KTAB2         READE     TABEL                                  30
073800950112     C*
073900950112     C     *IN30         DOWEQ     *OFF
074000950112     C     TBLFLG        IFEQ      ' '
074100950112     C                   MOVEL     TBLKEY        CMC(C)
074200950112     C                   MOVE      TBLKEY        WALF3             3
074300950112     C                   MOVE      WALF3         CMC(C)
074400950112     C                   ADD       1             C
074500950112     C                   ENDIF
074600950112     C     KTAB2         READE     TABEL                                  30
074700950112     C                   ENDDO
074800950112     C***
074900950112     C* CARICO I TIPI BOLLA DA NON TASSARE
075000950112     C                   Z-ADD     1             C
075100950112     C                   MOVEL     '8C'          COD
075200950112     C     KTAB2         SETLL     TABEL
075300950112     C     KTAB2         READE     TABEL                                  30
075400950112     C*
075500950112     C     *IN30         DOWEQ     *OFF
075600950112     C     TBLFLG        IFEQ      ' '
075700950112     C                   MOVEL     TBLKEY        CBL(C)
075800950112     C                   MOVE      TBLKEY        WALF3             3
075900950112     C                   MOVE      WALF3         CBL(C)
076000950112     C                   ADD       1             C
076100950112     C                   ENDIF
076200950112     C     KTAB2         READE     TABEL                                  30
076300950112     C                   ENDDO
076400090908      *----
076500090909     C                   Z-ADD     0             C
076600090908     C                   movel(P)  'CTP'         tbeCOD
076700090908     c                   clear                   KCliPar
076800090908     c                   clear                   KtutteBolle
076900090908     C     tbeCOD        setll     tntbe01l
077000090908     C     tbeCOD        reade     tntbe01l
077100090908      * consegne/ritiri
077200090908     c                   dow       not %eof(tntbe01l)
077300090909     C                   ADD       1             C
077400090908     C                   movel     tbeuni        dCTP
077500090908     C                   MOVEL     TBeKE1        KCliPar(C)
077600090908     C     tbeCOD        reade     tntbe01l
077700090908     c                   enddo
077800950112     C***
077900950112     C* DEFINIZIONE CAMPI
078000950112     C     *LIKE         DEFINE    TBLKEY        §KEY
078100950112     C     *LIKE         DEFINE    TBLCOD        §COD
078200950112     C     *LIKE         DEFINE    TBLKUT        §KUT
078300950112     C* AAMMGG DATA IMMESSA A VIDEO
078400950112     C     *LIKE         DEFINE    FTTDDC        W01DDC
078500950127     C     *LIKE         DEFINE    VI4DDC        WM1DDC
078600950112     C* DATA DI ELABORAZIONE (PUO' ESSERECENE SOLO UNA)
078700950112     C     *LIKE         DEFINE    FTTDDC        W02DDC
078800020129     C     *like         define    fttndc        w02ndc
078900011112     C     *like         define    fttdiv        w02div
079000950112     C     *LIKE         DEFINE    FTTNDC        W01NDC
079100950112     C     *LIKE         DEFINE    FTTPDR        W01PDR
079200011105      *
079300950112     C     *LIKE         DEFINE    FTTPDR        W02PDR
079400950112     C     *LIKE         DEFINE    FTTTSR        W01TSR
079500950112     C     *LIKE         DEFINE    FTTSNA        W01SNA
079600020212     C     *LIKE         DEFINE    FTTSNA        g01SNA
079700950112     C     *LIKE         DEFINE    FTTKFA        W01KFA
079800020212     C     *LIKE         DEFINE    FTTKFA        g01KFA
079900950112     C     *LIKE         DEFINE    FTTCLA        W01CLA
080000020212     C     *LIKE         DEFINE    FTTCLA        g01CLA
080100950118     C     *LIKE         DEFINE    FTTCPE        W01CPE
080200020212     C     *LIKE         DEFINE    FTTCPE        g01CPE
080300950112     C     *LIKE         DEFINE    FTTTVL        W01TVL
080400020212     C     *LIKE         DEFINE    FTTTVL        g01TVL
080500950112     C     *LIKE         DEFINE    FTTITA        W01ITA
080600950112     C     *LIKE         DEFINE    FTTITT        W01ITT
080700950112     C     *LIKE         DEFINE    FTTFIT        W01FIT
080800950112     C     *LIKE         DEFINE    FTTFIA        W01FIA
080900950119     C     *LIKE         DEFINE    FTTSNE        W01SNE
081000950112     C     *LIKE         DEFINE    FTTSNP        W01SNP
081100950112     C     *LIKE         DEFINE    FTDNCL        W01NCL
081200950112     C     *LIKE         DEFINE    FTDSTP        W01STP
081300950112     C     *LIKE         DEFINE    FTDSTP        W02STP
081400950112     C     *LIKE         DEFINE    FTDSPC        W01SPC
081500950112     C     *LIKE         DEFINE    FTDSPR        W01SPR
081600950112     C     *LIKE         DEFINE    NRR           W01NRR
081700950112     C     *LIKE         DEFINE    NRR           W04NRR
081800950112     C     *LIKE         DEFINE    NRR           W06NRR
081900950112     C     *LIKE         DEFINE    NRR           NRR4
082000950112     C     *LIKE         DEFINE    NRR           NRR6
082100011105     C     *LIKE         DEFINE    NRR           NRW6
082200950112     C     *LIKE         DEFINE    FGTCTR        W01CTR
082300950112     C     *LIKE         DEFINE    FTDCTP        W01CTP
082400020325     C     *like         define    ftdFIN        w01isola
082500020325     C     *like         define    arbtsp        w01xpres
082600020328     C     *like         define    ftwfgs        kftwfgs
082700020328     C     *like         define    ftwpdr        kftwpdr
082800020328     C     *like         define    ftwtsr        kftwtsr
082900020328     C     *like         define    ftwndc        kftwndc
083000020328     C     *like         define    ftwddc        kftwddc
083100020328     C     *like         define    ftwstp        kftwstp
083200020422     C*
083300020422     C     *like         define    ftdfgs        sv1fgs
083400020422     C     *like         define    ftdpdr        sv1pdr
083500020422     C     *like         define    ftdtsr        sv1tsr
083600020422     C     *like         define    ftdndc        sv1ndc
083700020422     C     *like         define    ftdddc        sv1ddc
083800020328     C*
083900030821     C     *like         define    §t1NSS        W01nss
084000030821     C     *like         define    §t1FW         W01fw
084100030821     C     *like         define    §t1RCC        W01rcc
084200030821     C     *like         define    ftdSTP        savSTP
084300030821     c     *like         define    W01nss        SAVnss
084400030821     c     *like         define    W01fw         SAVfw
084500030821     c     *like         define    W01rcc        SAVrcc
084600030821     C*
084700950112     C                   Z-ADD     0             B                 2 0
084800040112     C                   Z-ADD     0             C                 5 0
084900950112     C                   TIME                    UTIME             6 0
085000040622     C***
085100040622     C                   TIME                    W0140            14 0
085200040622     C                   MOVEl     W0140         Wora              6 0
085300040622     C                   MOVE      W0140         WDAT              8 0
085400040622     C*
085500040622     C                   Z-ADD     Wdat          G02DAT
085600040622     C                   MOVEL     *BLANK        G02ERR
085700040622     C                   CALL      'XSRDA8'
085800040622     C                   PARM                    WLBDAT
085900040622     C* UDATE A 8 IN AAAA/MM/GG
086000040622     C                   move      G02INV        dataoggi
086100090511      **
086200100513     C* scrive il record di allocazione x trasmissione
086300100513     c     parsml        ifeq      *blank
086400100513     C                   clear                   Fiwalc00
086500100513     C                   movel     w0140         walOrc
086600100513     C                   movel     g02inv        waldac
086700100513     C                   eval      WalSIF = KNSIF
086800100513     C                   eval      WalPGM = 'FICNB1R   '
086900100513     C                   eval      WalNOJ = KNMEB
087000100513     C                   eval      WalNUS = KNMUS
087100100513     C                   eval      WalNRJ = KNRJO
087200100513     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
087300100513     C                   write     Fiwalc00
087400100513     c                   feod      Fiwalc1L
087500100513     c                   end
087600090511     C***
087700950112     C                   ENDSR
087800020212     C*---------------------------------------------------------------*
087900931103     C* PULISCO 1 FORMATO --------------------------------------------*
088000020212     C*---------------------------------------------------------------*
088100931103     C     PUL01         BEGSR
088200931103     C                   CLEAR                   NDC
088300931103     C                   CLEAR                   VI1P1
088400931103     C                   CLEAR                   VI1P2
088500931103     C                   CLEAR                   VI1DDC
088600931103     C                   CLEAR                   VI1FVL
088700931103     C                   CLEAR                   VI1TSR
088800020717     C                   Z-ADD     SIMPOU        VI1PF1
088900950112     C                   ENDSR
089000020212     C*---------------------------------------------------------------*
089100900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
089200020212     C*---------------------------------------------------------------*
089300900528     C     CONTR1        BEGSR
089400170130      *--
089500931103     C                   SETOFF                                       90
089600170130      *- ind.: 48 particolare (oltre al messaggio di errore gestisce la visualizzazione
089700170130      *-     del tasto funzionale F8 x risolvere il problema)
089800170130     C                   SETOFF                                       48
089900170130      *--
090000931103     C                   CLEAR                   W01DDC
090100931103     C                   CLEAR                   W02DDC
090200020129     C                   clear                   w02ndc
090300011112     C                   clear                   w02div
090400931119     C* T I P O   P R E S T A Z I O N E
090500931021    1C     VI1TSR        IFNE      ' '
090600931021    2C     VI1TSR        IFEQ      '?'
090700931015     C                   MOVEL     CODUT         §KUT
090800931015     C                   MOVEL     '1Z'          §COD
090900931015     C                   MOVE      ' '           VI1TSR
091000931015     C                   MOVEL     *BLANKS       §KEY
091100931015     C                   CALL      'X§TABER'
091200931015     C                   PARM                    §KUT
091300931015     C                   PARM                    §COD
091400931015     C                   PARM                    §KEY
091500931015     C                   PARM                    §DES             30
091600931015     C                   MOVEL     §KEY          VI1TSR
091700931015     C                   SETON                                        90
091800931015     C                   GOTO      ENDCT1
091900931021    2C                   ENDIF
092000931018     C*
092100931015     C* 30 ON - INESISTENTE O ANNULLATO
092200931018     C     VI1TSR        LOOKUP    TSR                                    30
092300931018     C  N30              SETON                                        4090
092400931018     C  N30              GOTO      ENDCT1
092500931021    1C                   ENDIF
092600931015     C*
092700950112     C* F I L I A L E   P A D R O N C I N O
092800950112     C     VI1PF1        IFEQ      0
092900950112     C                   SETON                                        4190
093000950119     C                   GOTO      ENDCT1
093100950112     C                   ELSE
093200020717     C* Verifico esistenza filiale padroncino: se 2° LIV. su £6, se 1° LIV.
093300950112     C* su £1
093400020717     C                   IF        SIMTPP = '2'
093500950112     C     VI1PF1        LOOKUP    L6                                     30
093600950112     C  N30              SETON                                        4190
093700950119     C  N30              GOTO      ENDCT1
093800950112     C                   ELSE
093900950112     C     VI1PF1        LOOKUP    L1                                     30
094000950112     C  N30              SETON                                        4190
094100950119     C  N30              GOTO      ENDCT1
094200950112     C                   END
094300950112     C                   END
094400931018     C*
094500931018     C* P A D R O N C I N O   DAL AL
094600931021    1C     VI1PD2        IFGT      0
094700931119     C     VI1PD2        ANDNE     VI1PD1
094800931119     C*
094900931021    2C     VI1PD2        IFLT      VI1PD1
095000931015     C                   SETON                                        4290
095100090508     C                   GOTO      ENDCT1
095200931021    2C                   ENDIF
095300931015     C*
095400931021   X1C                   ELSE
095500931018     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
095600931021    2C     VI1PD1        IFGT      0
095700931110     C*
095800021202     C     kapdv         CHAIN     fiapd01L                           30
095900950123     C     *IN30         IFEQ      *ON
096000931015     C                   SETON                                        30
096100931021    3C                   ENDIF
096200931015     C*
096300931015     C   30              SETON                                        4390
096400931015     C   30              GOTO      ENDCT1
096500931021    2C                   ENDIF
096600931021    1C                   ENDIF
096700931018     C*
096800931018     C* D A T A   D I S T I N T A
096900931021    1C     VI1DDC        IFGT      0
097000950112     C                   Z-ADD     VI1DDC        G02DAT
097100931018     C                   MOVEL     *BLANK        G02ERR
097200950112     C                   CALL      'XSRDA8'
097300931018     C                   PARM                    WLBDAT
097400931021    2C     G02ERR        IFEQ      '1'
097500931018     C                   SETON                                        44  90
097600931018     C                   GOTO      ENDCT1
097700931021    2C                   END
097800931018     C                   Z-ADD     G02INV        W01DDC
097900931026    1C                   ENDIF
098000950113     C* SE IMMESSO UN SOLO NUMERO DISTINTA IMMETTERE ANCHE IL TIPO SERV
098100931018     C                   SORTA     NDC
098200931026    1C     NDC(10)       IFGT      0
098300931018     C     NDC(9)        ANDEQ     0
098400931018     C     VI1TSR        ANDEQ     ' '
098500931018     C                   SETON                                        45  90
098600931018     C                   GOTO      ENDCT1
098700931026    1C                   ENDIF
098800910320     C*
098900950112     C  N90              MOVEL     VI1PF1        VI1PF2
099000950112     C*
099100170130      *- Controllo sulle assenze autisti per la Filiale prescelta
099200170130     c                   clear                   FNLVF3DS
099300170130     c                   move      'C'           ILVF3TRC
099400170130     c                   move      VI1PF1        ILVF3fgs
099500170130      **
099600170130     c                   call      'FNLVF3R'
099700170130     c                   parm                    kpjba
099800170130     c                   parm                    FNLVF3DS
099900170130      **
100000170130      *   SE ritorna con "1" discordanza emette segnalazione
100100170130     c                   if        OLVF3DISCO = 'S'
100200170130     C                   SETON                                        4890
100300170130     C                   GOTO      ENDCT1
100400170130     c                   end
100500170130     C*
100600170130     C     ENDCT1        ENDSR
100700170130     C*---------------------------------------------------------------*
100800170130     C*--- gestione DISTINTE ASSENZE in filiale ----------------------*
100900170130     C*---------------------------------------------------------------*
101000170130     C     gesDISTASS    BEGSR
101100170130     C*
101200170130      *- Gestione Distinte/Assenze autisti sulla Filiale prescelta
101300170130     c                   clear                   FNLVF3DS
101400170130     c                   move      'G'           ILVF3TRC
101500170130     c                   move      VI1PF1        ILVF3fgs
101600170130      **
101700170130     c                   call      'FNLVF3R'
101800170130     c                   parm                    kpjba
101900170130     c                   parm                    FNLVF3DS
102000170130      **
102100170130      *   SE ritorna con "1" discordanza emette segnalazione
102200170130     c                   if        OLVF3DISCO = 'S' or
102300170130     c                             OLVF3ERR   = '1'
102400170130     C                   SETON                                        4890
102500170130     c                   end
102600170130     C*
102700170130     C                   ENDSR
102800020212     C*---------------------------------------------------------------*
102900931018     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
103000020212     C*---------------------------------------------------------------*
103100931018     C     CARNDC        BEGSR
103200950113     C                   SETOFF                                       0102
103300931018     C                   SETOFF                                       040531
103400931105     C                   SETOFF                                       21
103500931018     C                   Z-ADD     0             NRR               5 0
103600931018     C                   Z-ADD     0             W01NRR
103700931018     C                   CLEAR                   VI1VAL
103800931119     C                   CLEAR                   WDATA
103900931018     C* PULIZIA SUBFILE
104000931018     C                   SETON                                        35
104100931018     C                   WRITE     FRB8CT2
104200931018     C                   SETOFF                                       35
104300931018     C*
104400931018     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
104500931018     C* 02 ON - IMMESSI PADRONCINI DAL AL
104600931108     C* 04 ON - IMMESSO TIPO PRESTAZIONE
104700950113     C* 05 ON - IMMESSO SOLO UN NUMERO DISTINTA (CONSEGNE)
104800931018     C*
104900931108     C* TIPO PRESTAZ SELEZIONATO
105000931018     C     VI1TSR        IFNE      ' '
105100931018     C                   SETON                                        04
105200931018     C                   ENDIF
105300931018     C* PADRONCINO
105400931018     C     VI1PD2        IFGT      0
105500931018     C     VI1P2         ANDNE     VI1P1
105600931018     C                   SETON                                        02
105700931018     C                   ELSE
105800931018     C*
105900931018     C     VI1PD1        IFGT      0
106000931018     C                   SETON                                        01
106100950113     C* IMMESSO SOLO 1 NUMERO DISTINTA E TIPO SERIVZIO = 'C'
106200931018     C     NDC(10)       IFGT      0
106300931018     C     NDC(9)        ANDEQ     0
106400950113     C     VI1TSR        ANDEQ     'C'
106500931018     C                   SETON                                        05
106600931018     C                   MOVEL     NDC(10)       W01NDC
106700931018     C                   ENDIF
106800931018     C*
106900931018     C                   ENDIF
107000931018     C                   ENDIF
107100931018     C                   Z-ADD     VI1P1         W01PDR
107200931018     C* NUMERI DISTINTA
107300931018     C                   XFOOT     NDC           WNUM9             9 0
107400931018     C* FLAG VALORIZZAZIONE
107500931018     C     VI1FVL        IFEQ      ' '
107600931018     C                   MOVEL     CDAVAL        VI1VAL
107700931018     C                   ELSE
107800931018     C                   MOVEL     CVALOR        VI1VAL
107900931018     C                   ENDIF
108000921229     C**
108100931018     C* POSIZIONAMENTO PER LETTURA
108200931018     C**
108300931018     C                   SELECT
108400060118      **
108500931108     C** NON IMMESSO TIPO PRESTAZ: LEGGO SOLO PER PADRONCINO
108600931018     C     *IN04         WHENEQ    *OFF
108700931018     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
108800931018     C     VI1PD1        IFGT      0
108900060118     C     k21pdr        SETLL     fiftt21l
109000931018     C                   ELSE
109100931018     C                   EXSR      IMPO1
109200931018     C                   ENDIF
109300931018     C*
109400931018     C                   OTHER
109500950113     C** 01 ON  - LEGGO PER PADRONCINO/TIPO SERVIZIO/NUMERO/DATA DISTIN
109600950113     C**              O PER PADRONCINO/TIPO SERVIZIO
109700950113     C** 01 OFF - LEGGO PER PADRONCINO (TUTTI O NEL DAL-AL)/TIPO SERVIZ
109800060118      **
109900931018     C     *IN01         IFEQ      *ON
110000060118     C   05K21pdr1       CHAIN(N)  fiftt21l                           31
110100060118     C  N05K21pdr3       SETLL     fiftt21l
110200060118      **
110300931018     C                   ELSE
110400950113     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
110500950113     C     VI1PD1        IFGT      0
110600060118     C     k21PDR        SETLL     fiftt21l
110700950113     C                   ELSE
110800931018     C                   EXSR      IMPO1
110900931018     C                   ENDIF
111000060118      **
111100950113     C                   ENDIF
111200060118      **
111300931018     C                   ENDSL
111400931018     C*
111500931018     C* LETTURA DISTINTE/RITIRI
111600931018     C     *IN31         IFEQ      *OFF
111700931018     C                   EXSR      LEGNDC
111800931018     C                   ELSE
111900931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
112000931018     C                   SETON                                        4690
112100931119     C                   MOVEL     ERR(1)        VI1ERR
112200030519      * solo in simulazione: (eccezione)
112300030519      *  se non c'è la tariffa non carica le distinte
112400030519      *  quindi non è valido il messaggio precedente
112500030519     c     parsml        ifeq      'S'
112600030519     c     §t3ctr        andeq     '???'
112700030519     C                   clear                   VI1ERR
112800030519     c     §t3tsr        ifeq      'C'
112900030519     C                   MOVEL     ERR(2)        VI1ERR
113000030519     c                   end
113100030519     c     §t3tsr        ifeq      'R'
113200030519     C                   MOVEL     ERR(3)        VI1ERR
113300030519     c                   end
113400030519     c                   end
113500931018     C                   ENDIF
113600931018     C*
113700900608     C                   ENDSR
113800020212     C*---------------------------------------------------------------*
113900931018     C* IMPOSTAZIONE LETTURA PER 04 OFF ------------------------------*
114000020212     C*---------------------------------------------------------------*
114100931018     C     IMPO1         BEGSR
114200060118      **
114300060118     C     k21PDR        setgt     fiftt21l
114400060118     C                   read(N)   fiftt21l                               31
114500950202     C*
114600950202     C     *IN31         IFEQ      *OFF
114700950202     C                   MOVEL     FTTPDR        W03               3 0
114800950202     C     VI1PF1        IFNE      W03
114900950202     C                   SETON                                        31
115000950202     C                   END
115100950202     C                   END
115200931018     C*
115300931018     C     *IN31         IFEQ      *OFF
115400931018     C                   Z-ADD     FTTPDR        W01PDR
115500060118     C  N04k21pdr        setll     fiftt21l
115600060118     C   04K21pdr3       setll     fiftt21l
115700931018     C                   ENDIF
115800060118      **
115900931018     C                   ENDSR
116000020212     C*---------------------------------------------------------------*
116100931018     C* LETTURA DITINTE DA CARICARE ----------------------------------*
116200020212     C*---------------------------------------------------------------*
116300931018     C     LEGNDC        BEGSR
116400931018     C                   CLEAR                   W01TSR
116500931018     C                   CLEAR                   W02PDR
116600931018     C* L E T T U R A
116700940325    1C     *IN31         DOWEQ     *OFF
116800931018     C                   SETOFF                                       9032
116900931018     C                   SELECT
117000931018     C** LEGGO PER PADRONCINO (TUTTO O NEL DAL-AL)
117100940325    2C     *IN04         WHENEQ    *OFF
117200931018     C*
117300940325    3C     *IN32         DOWEQ     *OFF
117400060118     C     k21pdr        READE(N)  fiftt21l                               31
117500931018     C*
117600940325    4C     *IN31         IFEQ      *ON
117700931018     C     *IN01         ANDEQ     *OFF
117800931018     C                   EXSR      IMPO1
117900931018     C   31              SETON                                        32
118000931018     C                   ELSE
118100931018     C                   SETON                                        32
118200940325    4C                   ENDIF
118300940325    3C                   ENDDO
118400931108     C** LEGGO TUTTI I PADRONCINI(O NEL DAL-AL) E TIPO PRESTAZIONE
118500940325    2C     *IN01         WHENEQ    *OFF
118600931018     C*
118700940325    3C     *IN32         DOWEQ     *OFF
118800060118     C     K21pdr3       READE(N)  fiftt21l                               31
118900931018     C*
119000940325    4C     *IN31         IFEQ      *ON
119100931018     C                   EXSR      IMPO1
119200931018     C   31              SETON                                        32
119300931018     C                   ELSE
119400931018     C                   SETON                                        32
119500940325    4C                   ENDIF
119600940325    3C                   ENDDO
119700060118      *
119800931108     C** LEGGO PER PADRONCINO TIPO PRESTAZIONE
119900940325    2C     *IN01         WHENEQ    *ON
120000931018     C     *IN05         ANDEQ     *OFF
120100060118     C     K21pdr3       READE(N)  fiftt21l                               31
120200931018     C*
120300931018     C                   ENDSL
120400931018     C*
120500931018     C* C O N F R O N T O   L  E   S E L E Z I O N I
120600020606     C     *IN31         IFEQ      *OFF
120700020606     C                   EXSR      CONFR
120800931018     C*
120900931018     C* C A R I C A M E N T O   S F L
121000900608     C* 90 ON - RECORD DA NON SELEZIONARE
121100020606     C     *IN90         ifeq      *OFF
121200931018     C*
121300931018     C                   CLEAR                   VI2SCE
121400011105     C                   clear                   vh2div
121500931028     C                   CLEAR                   VH2ITT
121600931028     C                   CLEAR                   VH2ITA
121700931018     C*
121800931018     C     FTTPDR        IFNE      W02PDR
121900020524     C                   CLEAR                   saltasf2          1
122000931103     C                   SETOFF                                       12
122100931103     C                   MOVEL     '0'           VH2I12
122200021202     C     kapdf         CHAIN     fiapd01L                           30
122300950117     C     *IN30         IFEQ      *OFF
122400100223     C                   MOVEL     APDRSf        VI2DPD
122500950117     C                   MOVEL     APDETM        VH2ETM
122600950117     C                   MOVEL     APDETP        VH2ETP
122700950117     C                   MOVEL     APDPKM        VH2PKM
122800950117     C                   MOVEL     APDPKP        VH2PKP
122900021211     C                   eval      vh2cam = apdCAM
123000021211     C                   eval      vh2cap = apdCAP
123100021211     C                   eval      vh2scm = apdSCM
123200021211     C                   eval      vh2scp = apdSCP
123300021211     C                   z-add     apdHAR        vh2HAR
123400021211     C                   z-add     apdHPA        vh2HPA
123500020524     C                   movel     APDpdd        saltasf2
123600950117     C                   ELSE
123700950117     C                   CLEAR                   VI2DPD
123800950117     C                   CLEAR                   VH2ETM
123900950117     C                   CLEAR                   VH2ETP
124000950117     C                   CLEAR                   VH2PKM
124100950117     C                   CLEAR                   VH2PKP
124200021211     C                   clear                   vh2CAM
124300021211     C                   clear                   vh2CAP
124400021211     C                   clear                   vh2SCM
124500021211     C                   clear                   vh2HAR
124600021211     C                   clear                   vh2HPA
124700021211     C                   clear                   vh2SCP
124800950117     C                   END
124900931018     C                   MOVEL     FTTPDR        VI2PDR
125000931018     C                   MOVEL     FTTPDR        W02PDR
125100931018     C                   ELSE
125200931103     C                   SETON                                        12
125300931103     C                   MOVEL     '1'           VH2I12
125400931018     C                   ENDIF
125500931108     C* TIPO PRESTAZIONE
125600931018     C     FTTTSR        IFNE      W01TSR
125700931018     C                   MOVEL     FTTTSR        K17(1)
125800931018     C                   MOVEL     '-'           K17(2)
125900931018     C                   Z-ADD     1             C
126000931018     C     FTTTSR        LOOKUP    TSR(C)                                 30
126100931018     C   30              MOVEA     DTS(C)        K17(3)
126200931018     C  N30              MOVEA     *BLANKS       K17(3)
126300931018     C*
126400931018     C                   MOVEA     K17           VI2TSR
126500931018     C                   MOVEL     FTTTSR        W01TSR
126600931018     C                   ENDIF
126700931018     C*
126800931119     C                   MOVEL     FTTNDC        VI2NDC                          NUMERO
126900950113     C                   MOVE      FTTDDC        DATA
127000950113     C                   MOVE      FTTDDC        VI2DDC                         DATA
127100931119     C                   MOVE      AA            VI2DDC
127200950113     C                   MOVEL     GG            VI2DDC
127300931119     C                   MOVEL     FTTDDC        VH2DDC
127400931027     C     FTTFIT        IFNE      ' '
127500011105     C                   Z-ADD     FTTITT        VH2ITT
127600931027     C                   ENDIF
127700931027     C     FTTFIA        IFNE      ' '
127800931027     C                   Z-ADD     FTTITA        VH2ITA
127900931027     C                   ENDIF
128000011105     C                   movel     fttdiv        vh2div
128100950117     C                   MOVEL     FTTFPP        VH2FPP
128200950117     C                   MOVEL     FTTPEP        VH2PEP
128300021211     C                   eval      vh2FCS = fttLVL
128400021217     C                   z-add     fttTBS        vh2TBS
128500900608     C*
128600020603     C*  Prova a cercare se in simulazione la tariffa legata al documento
128700020603     c     parsml        ifeq      'S'
128800030516     C*
128900020603     C* Routine esterna per restituire il codice tariffa del documento
129000020603     C                   clear                   ficnb3
129100030516     C*
129200030516     C* Attenzione se ci sono dei Ritiri Annullati, deve impostare il parametro
129300030516     C*  RAN con (S)
129400030516     c                   if        ftttsr ='R' and ftttbs > 0
129500030516     C                   movel     'S'           §t3RAN
129600030516     c                   end
129700020603     C                   movel     fttpdr        §t3pdr
129800020603     C                   movel     parsml        §t3sml
129900020603     C                   movel     ftttsr        §t3tsr
130000020603     C                   z-add     fttndc        §t3ndc
130100020603     C                   z-add     fttddc        §t3ddc
130200020603     C                   movel     fttdiv        §t3div
130300020603     C                   movel     *blank        §t3ctr
130400020605     C                   movel     vi1fvl        §t3fvl
130500020603     C                   CALL      'FICNB3R1'
130600020603     C                   PARM                    ficnb3
130700020603      *
130800020603     c                   endif
130900020603     C*
131000020524     C* se il padroncino è stato escluso dalla valorizzazione
131100020524     C* comunque in simulazione deve sempre permettere la valorizzazione
131200020603     c                   move      'S'           nowrite           1
131300020603      *
131400020603     c     saltasf2      ifeq      'S'
131500020603     c     parsml        andeq     *blank
131600020603      *
131700020524     c     parsml        oreq      'S'
131800020603     c     §t3ctr        andeq     '???'
131900020603     c     saltasf2      andeq     'S'
132000020603     c                   move      'N'           nowrite
132100020603     c                   endif
132200020524      *
132300020524     C* questo da quando si è voluto attivare le simulazioni per quei
132400020524     C* padroncini che non devono avere l'autofatturazione ma che occorre
132500020524     C* simulare comunque.
132600020603     c                   if        nowrite ='S'
132700020606     C*
132800020606     C* CONTROLLO QUANTE DATE CARICO
132900020606    1C     W02DDC        IFEQ      0
133000020606     C                   Z-ADD     FTTDDC        W02DDC
133100020606     C                   z-add     fttndc        w02ndc
133200020606     C                   movel     fttdiv        w02div
133300020606   X1C                   ELSE
133400020606     C* PRESENTI ALMENO 2 DATE
133500020606    2C     FTTDDC        IFNE      W02DDC
133600020606     C                   MOVEL     '1'           WDATA             1
133700020606    2C                   ENDIF
133800020606    1C                   ENDIF
133900020606     C*
134000900608     C                   ADD       1             NRR
134100931018     C                   WRITE     FRB8SF2
134200020603     c                   endif
134300020524      *
134400931018     C                   ENDIF
134500020606      *
134600020606     C                   ENDIF
134700940325     C*
134800940325     C* ESSENDO CHAIN SECCA, DOPO AVER LETTO UNA VOLTA ESCO
134900940325     C   01
135000940325     CAN 05              SETON                                        31
135100931018     C                   ENDDO
135200900521     C*
135300931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
135400931119     C     NRR           IFEQ      0
135500931018     C                   SETON                                        4690
135600931119     C                   MOVEL     ERR(1)        VI1ERR
135700030519      * solo in simulazione: (eccezione)
135800030519      *  se non c'è la tariffa non carica le distinte
135900030519      *  quindi non è valido il messaggio precedente
136000030519     c     parsml        ifeq      'S'
136100030519     c     §t3ctr        andeq     '???'
136200030519     C                   clear                   VI1ERR
136300030519     c     §t3tsr        ifeq      'C'
136400030519     C                   MOVEL     ERR(2)        VI1ERR
136500030519     c                   end
136600030519     c     §t3tsr        ifeq      'R'
136700030519     C                   MOVEL     ERR(3)        VI1ERR
136800030519     c                   end
136900030519     c                   end
137000931018     C                   ELSE
137100931018     C                   Z-ADD     NRR           W01NRR
137200931119     C                   SETOFF                                       90
137300931018     C                   ENDIF
137400900509     C*
137500900518     C                   ENDSR
137600020212     C*---------------------------------------------------------------*
137700931018     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
137800020212     C*---------------------------------------------------------------*
137900931018     C     CONFR         BEGSR
138000931108     C* TIPO PRESTAZIONE
138100931105     C   04FTTTSR        IFNE      VI1TSR
138200931105     C                   SETON                                        90
138300931105     C                   GOTO      ENDCON
138400931105     C                   ENDIF
138500931105     C* FLAG VALORIZZAZIONE
138600931105     C     FTTFVL        IFNE      VI1FVL
138700931105     C                   SETON                                        90
138800931105     C                   GOTO      ENDCON
138900931105     C                   ENDIF
139000931018     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
139100931018     C   02FTTPDR        IFGT      VI1P2
139200931018     C                   SETON                                        3190
139300931018     C                   GOTO      ENDCON
139400931018     C                   ENDIF
139500931018     C* DATA DISTINTA
139600931018     C     VI1DDC        IFGT      0
139700931018     C     FTTDDC        ANDGT     W01DDC
139800931018     C                   SETON                                        90
139900931018     C                   GOTO      ENDCON
140000931018     C                   ENDIF
140100931028     C*
140200931028     C* PGM RICHIAMATO: CONTROLLO DATA DISTINTA
140300931028     C   20FTTDDC        IFNE      PARDDC
140400931028     C                   SETON                                        90
140500931028     C                   GOTO      ENDCON
140600931028     C                   ENDIF
140700950113     C* NUMERI DISTINTA O RITIRO E SCELTO UN SOLO NUMERO DISTINTA
140800931018     C     WNUM9         IFGT      0
140900950113     C     NDC(10)       ORGT      0
141000950113     C     NDC(9)        ANDEQ     0
141100950113     C     VI1TSR        ANDEQ     'R'
141200931018     C     FTTNDC        LOOKUP    NDC                                    30
141300931018     C  N30              SETON                                        90
141400931018     C  N30              GOTO      ENDCON
141500931018     C                   ENDIF
141600931018     C*
141700931018     C     ENDCON        ENDSR
141800020212     C*---------------------------------------------------------------*
141900931018     C* SELEZIONE DISTINTE DA VALORIZZARE ----------------------------*
142000020212     C*---------------------------------------------------------------*
142100931018     C     SELNDC        BEGSR
142200931105     C                   SETOFF                                       90
142300931018     C                   Z-ADD     1             NRR
142400931018     C                   Z-ADD     0             NRR4
142500931018     C                   Z-ADD     0             W04NRR
142600931018     C                   CLEAR                   W02PDR
142700931018     C                   CLEAR                   VI4SCE
142800931119     C* PRESENTI PIU' DATE
142900931119    1C     WDATA         IFNE      ' '
143000931119     C* SE CMD1 ERRORE
143100931119    2C     *INKA         IFEQ      *ON
143200931119     C                   Z-ADD     1             REC2
143300931119     C                   SETON                                        4090
143400931119     C                   GOTO      ENDSEL
143500931119     C*
143600931119   X2C                   ELSE
143700931119     C*
143800931119     C* SE ENTER CONTROLLO LE SELEZIONI FATTE
143900931123     C                   CLEAR                   W02DDC
144000020129     C                   clear                   w02ndc
144100011112     C                   clear                   w02div
144200931119     C                   SETON                                        36
144300931119     C                   READC     FRB8SF2                                30
144400931119     C*
144500931119    3C     *IN30         DOWEQ     *OFF
144600931122     C*
144700931122    4C     VI2SCE        IFEQ      '1'
144800931122    5C     W02DDC        IFEQ      0
144900931122     C                   MOVEL     VH2DDC        W02DDC
145000020129     C                   z-add     vi2ndc        w02ndc
145100011112     C                   movel     vh2div        w02div
145200931122   X5C                   ELSE
145300931122    6C     VH2DDC        IFNE      W02DDC
145400931119     C                   Z-ADD     NRR           REC2
145500931119     C                   SETON                                        409030
145600931122    6C                   ENDIF
145700931122    5C                   ENDIF
145800931122    4C                   ENDIF
145900931119     C*
146000931119    4C     VH2I12        IFEQ      '1'
146100931119     C                   SETON                                        12
146200931119   X4C                   ELSE
146300931119     C                   SETOFF                                       12
146400931119    4C                   ENDIF
146500931119     C*
146600931119     C                   UPDATE    FRB8SF2
146700931119     C*
146800931119     C  N30              READC     FRB8SF2                                30
146900931119    3C                   ENDDO
147000931119     C*
147100931119     C                   SETOFF                                       36
147200931119     C   90              GOTO      ENDSEL
147300931119    2C                   ENDIF
147400931119    1C                   ENDIF
147500931018     C* PULIZIA SUBFILE
147600931018     C                   SETON                                        35
147700931018     C                   WRITE     FRB8CT4
147800931018     C                   SETOFF                                       35
147900931119     C*
148000931119     C                   Z-ADD     1             NRR
148100950131     C* Indicatore protezione codice tariffa e Pik/Etich. S/N per
148200950131     C* distinte gia' valorizzate
148300950131     C     VI1FVL        COMP      'V'                                    24
148400931018     C*
148500931028    1C     NRR           DOWLE     W01NRR
148600931103     C*
148700931111     C   KANRR           CHAIN     FRB8SF2                            31
148800931111     C  NKA              READC     FRB8SF2                                31
148900931018     C*
149000931018     C* SE ENTER --> SOLO QUELLI CON L'"1"
149100931111    2C  N31*INKA         IFEQ      *OFF
149200931103     C     VI2SCE        ANDEQ     '1'
149300931018     C* SE CMD1 --> TUTTI
149400931018     C     *INKA         OREQ      *ON
149500931018     C*
149600931018     C* C O D I C E   P A D R O N C I N O
149700931028    3C     VI2PDR        IFNE      W02PDR
149800931018     C*
149900931018     C                   MOVEL     VI2PDR        W02PDR                          SALVATAGGIO
150000931019     C                   MOVEL     VI2PDR        VI4PDR                          VIDEO COD
150100931019     C                   MOVEL     VI2DPD        VI4DPD                          VIDEO DES
150200931018     C                   MOVEL     VI2DPD        VH4DPD                          HIDDEN X SLF6
150300931018     C*
150400931108     C* 11 OFF - EMETTO PADRONCINO E TIPO PRESTAZ A VIDEO
150500931019     C                   SETOFF                                       11
150600931028    3C                   ENDIF
150700931018     C*
150800931108     C* T I P O   P R E S T A Z I O N E
150900931028    3C     *IN11         IFEQ      *OFF
151000931019     C     VI2TSR        ORNE      W17TSR
151100931019     C                   SETOFF                                       11
151200021211     C                   MOVEL     *blank        vi4TCS
151300931018     C                   MOVEL     VI2TSR        W17TSR           17             CONFRONTO
151400931018     C                   MOVEL     VI2TSR        VI4TSR                          VIDEO
151500950120     C                   MOVEL     VI2TSR        VH4TSH                         T.SERV.X PRINT
151600931019     C                   MOVEL     VI2TSR        VH4TSR                          HIDDEN
151700950119     C                   MOVEL     VI2TSR        VH4TSD                          HIDDEN
151800950119     C                   MOVEL     VI2DPD        VI4DPD
151900090508     C                   MOVEL     VI2DPD        VH4DPD
152000090508      *
152100931123     C* RICERCA CODICE TARIFFA PER PADRONCINO/TIPO PRESTAZ
152200011112     C                   clear                   vi4ctd
152300931021     C                   EXSR      RICTAR
152400040728     c                   move      tar_poste     vh4TPO
152500090507     C*
152600931018     C                   CLEAR                   VI4NDC
152700931018     C                   CLEAR                   VI4DDC
152800950116     C                   CLEAR                   VH4DDC
152900931019     C                   CLEAR                   VH4NRR
153000931027     C                   CLEAR                   VI4ITT
153100931027     C                   CLEAR                   VI4ITA
153200011106     C                   z-add     1             vh4tpr
153300931018     C                   ADD       1             NRR4
153400011109     c     vh4tpr        comp      2                                      26
153500931018     C                   WRITE     FRB8SF4
153600931019     C                   SETON                                        11
153700000703     C                   SETOFF                                       36
153800931019     C                   Z-ADD     NRR4          VH4NRR
153900931028    3C                   ENDIF
154000931018     C*
154100950117     C* PRESTAZIONE PADRONCINO M o P
154200950119     C     VH2FPP        IFEQ      'M'
154300950117     C                   MOVEL     COS1          VI4DPD
154400950119     C                   ELSE
154500950119     C     VH2FPP        IFEQ      'P'
154600950119     C                   MOVEL     COS2          VI4DPD
154700950119     C                   END
154800950119     C                   END
154900021211      *
155000950117     C                   MOVE      VH2FPP        VH4FPP
155100021211      *
155200950117     C* PAGAMENTO TARIFFA ETICH/PIKING
155300950117     C                   SELECT
155400021211      *
155500950117     C* Se Consegna memorizza flag piking da anagrafico padroncino
155600021211      *  e memorizza anche carico/scarico
155700950117     C     VH4TSR        WHENEQ    'C'
155800021211      * Carico
155900950119     C                   MOVEL     COS3          VI4TSR
156000021211     C                   MOVEL     cos5          vi4TCS
156100950117     C     VH4FPP        IFEQ      'M'
156200950127     C                   MOVEL     VH2PKM        VH4PEA
156300021211     c                   eval      vh4ACS = vh2CAM
156400950117     C                   ELSE
156500950127     C                   MOVEL     VH2PKP        VH4PEA
156600021211     c                   eval      vh4ACS = vh2CAP
156700950117     C                   END
156800021211      *
156900950117     C* Se Ritiro memorizza flag etichettatura da anagrafico
157000950117     C     VH4TSR        WHENEQ    'R'
157100021211      * Scarico
157200950119     C                   MOVEL     COS4          VI4TSR
157300021211     C                   MOVEL     cos6          vi4TCS
157400950117     C     VH4FPP        IFEQ      'M'
157500950127     C                   MOVEL     VH2ETM        VH4PEA
157600021211     c                   eval      vh4ACS = vh2SCM
157700950117     C                   ELSE
157800950127     C                   MOVEL     VH2ETP        VH4PEA
157900021211     c                   eval      vh4ACS = vh2SCP
158000950117     C                   END
158100950117     C                   ENDSL
158200950127     C*
158300950127     C                   MOVEL     VH2PEP        VI4PEP
158400021211     c                   eval      vi4FCS = vh2FCS
158500021211      *
158600950127     C     VI4PEP        IFEQ      *BLANKS
158700950127     C                   MOVEL     VH4PEA        VI4PEP
158800950127     C                   END
158900021211      *
159000021211     C     vi4FCS        IFEQ      *BLANKS
159100021211     C                   eval      vi4FCS = vh4ACS
159200021211     C                   END
159300021211      *
159400931018     C* DISTINTA: NUMERO E DATA
159500931018     C                   MOVEL     VI2NDC        VI4NDC
159600931018     C                   MOVEL     VI2DDC        VI4DDC
159700950116     C                   MOVEL     VH2DDC        VH4DDC
159800011105     C                   MOVEL     VH2ITT        VI4ITT
159900931027     C                   MOVEL     VH2ITA        VI4ITA
160000021217     C                   z-add     vh2TBS        vh4TBS
160100030127      * espone se ci sono dei ritiri annullati
160200030127     C                   z-add     vh2TBS        vi4ran
160300011105     C                   CLEAR                   vi4ctd
160400011105     C                   MOVEL     vh2div        vi4ctd
160500011106     C                   z-add     2             vh4tpr
160600931018     C                   ADD       1             NRR4
160700011109     c     vh4tpr        comp      2                                      26
160800931018     C                   WRITE     FRB8SF4
160900931103     C*
161000931103     C* PULISCO IL CAMPO DI SCELTA
161100931103     C                   MOVEL     ' '           VI2SCE
161200931103     C     VH2I12        IFEQ      '0'
161300931103     C                   SETOFF                                       12
161400931103     C                   ELSE
161500931103     C                   SETON                                        12
161600931103     C                   ENDIF
161700931103     C*
161800931103     C                   UPDATE    FRB8SF2
161900931028    2C                   ENDIF
162000931103     C*
162100931103     C* FINE LETTURA SE NO CMD1
162200931111     C  NKA*IN31         IFEQ      *ON
162300931103     C                   Z-ADD     W01NRR        NRR
162400931103     C                   ENDIF
162500931018     C*
162600931018     C                   ADD       1             NRR
162700931028    1C                   ENDDO
162800931018     C*
162900931018     C* NON EFFETTUATA NESSUNA SELEZIONE--> RIEMETTO SFL 2
163000931028    1C     NRR4          IFEQ      0
163100931018     C                   SETON                                        90
163200931018     C                   ELSE
163300931018     C                   Z-ADD     NRR4          W04NRR
163400931028    1C                   ENDIF
163500931018     C*
163600931119     C     ENDSEL        ENDSR
163700020212     C*---------------------------------------------------------------*
163800931108     C* RICERCO TARIFFA PREFERENZIALE PER TIPO PRESTAZ/PADRONCINO ----*
163900020212     C*---------------------------------------------------------------*
164000931018     C     RICTAR        BEGSR
164100000629     C*
164200000629     C                   SETOFF                                       5051
164300000703     C                   SETON                                        36
164400000622     C* RICHIAMO LA ROUTINE CHE VERIFICA SE LE BOLLE CHE SONO IN DISTINTA SONO TUTTE BARTOLINI
164500000622     C* OPPURE TUTTE POSTE OPPURE MISTE PER FARE LA PROPOSTA DELLA TARIFFA GIUSTA
164600000622     C                   EXSR      RICBOL
164700931104     C* SE LA SOMMA PESI NON C'E' VIENE ASSUNTO SPEDIZIONE
164800931104     C                   MOVEL     'S'           VH4TSM
164900931028     C                   CLEAR                   WTARV
165000000622     C* CAMPI COMODO TARIFFE  POSTE
165100000622     C                   CLEAR                   PTARV
165200040825     C                   clear                   Ptarv_rap
165300040825     C                   clear                   Ptarv_rct
165400000622     C*
165500011112     C     KFGT          CHAIN     FIFGT000                           30
165600931018     C*
165700931021    1C     *IN30         DOWEQ     *OFF
165800931018     C*
165900011112     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
166000011112     C*  e quelle diverse dalla divisa del documento
166100020128      ****   e quelle senza data di stampa (quindi non convalidate)
166200020315      ****   ma solo se non in simulazione.
166300020315      ****   se in simulazione non deve controllare la data di stampa.
166400091112    2C     FGTATB        IFEQ      ' '
166500091112     C     w02div        andeq     fgtdiv
166600091112     C*
166700091112     C* Se tariffa del periodo
166800091112     C     Ktgt          CHAIN     FItgt01l
166900091112 2/3 c                   if        %Found(FItgt01l) and
167000091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
167100091112     **
167200091112    3C     parsml        IFeq      *blank
167300091112     C     tgtDTS        aNDgt     0
167400091112     C     parsml        OReq      'S'
167500011112      *
167600931108     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
167700931105     C                   Z-ADD     1             C
167800931105     C     FGTTSR        LOOKUP    TSR(C)                                 32
167900091112    4C     FGTCTR        IFNE      CTS(C)
168000931018     C* TARIFFA VALIDA
168100931021     C*
168200931021     C* EMETTO LA PREFERENZIALE SE C'E'
168300931021    5C     FGTFTP        IFEQ      'P'
168400931021     C                   MOVEL     FGTCTR        WTARV             3
168500931021     C                   SETON                                        30
168600931021     C*
168700931021   X5C                   ELSE
168800931021     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
168900931022    6C     WTARV         IFEQ      *BLANKS
169000000622     C     FGTFTP        ANDNE     'O'
169100091112     C     WTARV         orne      *blank
169200040628     C     fgtFTP        ANDNE     'O'
169300091112     C     fgtCTR        andeq     sav_FTDCTB
169400091112     C     sav_FlgCTB    andeq     'S'
169500931022     C                   MOVEL     FGTCTR        WTARV
169600931022    6C                   ENDIF
169700091112      *
169800000622    6C     PTARV         IFEQ      *BLANKS
169900000622     C     FGTFTP        ANDEQ     'O'
170000091112     C     ptarv         Orne      *BLANKS
170100091112     C     fgtCTR        andeq     sav_FTDCTP
170200091112     C     sav_FlgCTP    andeq     'S'
170300040628     C     FGTFTP        ANDEQ     'O'
170400000622     C                   MOVEL     FGTCTR        PTARV             3
170500040825     C                   z-add     fgtrap        Ptarv_rap
170600040825     C                   z-add     fgtrct        Ptarv_rct
170700000622    6C                   ENDIF
170800091112      *
170900931022    5C                   ENDIF
171000091112     C*
171100091112    4C                   ENDIF
171200091112     C*
171300091112    3C                   ENDIF
171400091112 2/3 C                   ENDIF
171500091112    2C                   ENDIF
171600931018     C*
171700011112     C  N30KFGT          READE     FIFGT000                               30
171800931021    1C                   ENDDO
171900931021     C*
172000000714     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
172100000622    0C     VH4BAR        IFEQ      'S'
172200931028    1C     WTARV         IFNE      *BLANKS
172300011030     C                   MOVEL     WTARV         VI4CTD                          VALIDA
172400000703     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
172500011030     C                   MOVEL     VI4CTD        VH4CTB
172600931028    1C                   ENDIF
172700000622    0C                   ENDIF
172800000622     C*
172900000714     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
173000000622    0C     VH4POS        IFEQ      'S'
173100040825      *
173200040825      *  Deve controllare se però c'è una tariffa di riferimento poste che non esiste +
173300091112      **** NON DEVE ESSERE PIU' utilizzata poichè comunque manca il progressivo in chiave
173400040825     C                   if        Ptarv_rap <> 0 and Ptarv_rct <> 0
173500040825     c                   exsr      Ric_Tar_RIF
173600040825     c                   end
173700040825      *
173800000622    1C     PTARV         IFNE      *BLANKS
173900011030     C                   MOVEL     PTARV         VI4CTD                          VALIDA
174000000703     C* SALVO LA TARIFFA POSTE     PER IL DETTAGLIO SPEDIZIONE
174100011030     C                   MOVEL     VI4CTD        VH4CTP
174200040726     c                   else
174300040726     **
174400040726     ** se non ci sono tariffe poste, dalla data specificata in FINOADATA
174500040726     ** deve prendere come tariffa poste la tariffa bartolini
174600040726     c                   if        dataoggi >= finoadata
174700040726     C                   MOVEL     WTARV         VI4CTD                          VALIDA
174800040726     C                   MOVEL     VI4CTD        VH4CTB
174900040726     c                   end
175000040726     **
175100000622    1C                   ENDIF
175200000622    0C                   ENDIF
175300931021     C*
175400931021     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
175500011030     C     VI4CTD        IFEQ      *BLANKS
175600011030     C                   MOVEL     '???'         VI4CTD
175700000629     C                   SETOFF                                       5150
175800000626     C                   ELSE
175900000626     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
176000000626     C     VH4CTB        IFNE      *BLANKS
176100011030     C                   MOVEL     VH4CTB        VI4CTD
176200000626     C                   ENDIF
176300000626     C*
176400931021     C                   ENDIF
176500931021     C*
176600040727     ** Si memorizza se ha trovato una tariffa poste per poter
176700040727     **  gestire variazioni tariffe impostate a video su sfl su tariffe Bartolini
176800040728     c                   if        vh4TSR='C'
176900040727     c                   clear                   tar_poste         1
177000040727     c                   if        vh4CTP <> *blank
177100040727     c                   move      'S'           tar_poste         1
177200040727     c                   end
177300040728     c                   endIF
177400040727     **
177500931119     C                   ENDSR
177600040825     C*================================================================
177700040825     C* cerca eventuale tariffa di riferimento x Poste            ----*
177800040825     C*================================================================
177900040825     C     Ric_Tar_RIF   BEGSR
178000040825     **
178100040825     C                   Z-ADD     Ptarv_rap     rifPDR
178200040825     C                   Z-ADD     Ptarv_rct     rifCTR
178300040825     C*
178400040825     C                   move      'N'           tar_valida
178500040825     C*
178600040825     C     KFGT_rif      setll     fifgt01l
178700040825     C     KFGT_rif      reade     fifgt01l
178800040825      *
178900040825    1C                   DOW       not %eof(fifgt01l)
179000040825     C*
179100040825     C* TARIFFA VALIDA di riferimento POSTE
179200040825     C*  Non deve essere annullata e deve avere la divisa richiesta
179300040825      **   Data Documento deve essere compresa fra le date di attivazione e
179400040825      ***   scadenza e deve essere stata stampata (quindi convalidata)
179500040825      ****   >altrimenti è un Manca Tariffa<
179600040825    2C     FGTATB        ifEQ      ' '
179700091112     c     w02div        andeq     fgtdiv
179800091112     C*
179900091112     C* Se tariffa del periodo
180000091112     C     Ktgt          CHAIN     FItgt01l
180100091112 2/3 c                   if        %Found(FItgt01l) and
180200091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
180300091112      *
180400091112    3C     parsml        IFeq      *blank
180500091112     C     tgtDTS        andgt     0
180600091112     C     fgtatb        oreq      ' '
180700091112     c     parsml        andeq     'S'
180800040825     C*  se trovata ok deve uscire dal giro
180900040825     C                   move      'S'           tar_valida
181000040825     C                   leave
181100091112    3C                   end
181200091112      *
181300091112 2/3 C                   end
181400040825    2C                   end
181500040825     C*
181600040825     C     KFGT_rif      reade     fifgt01l
181700040825    1C                   ENDDO
181800040825     C*
181900040825    2C     Tar_valida    ifeq      'N'
182000040825     C                   clear                   PTARV
182100040825    2c                   end
182200040825     **
182300040825     C                   ENDSR
182400020212     C*---------------------------------------------------------------*
182500000622     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
182600020212     C*---------------------------------------------------------------*
182700000622     C     RICBOL        BEGSR
182800000622     C*
182900000622     C                   MOVEL     ' '           VH4POS
183000000622     C                   MOVEL     ' '           VH4BAR
183100000703     C                   MOVEL     *BLANKS       VH4CTP
183200000703     C                   MOVEL     *BLANKS       VH4CTB
183300021217     c                   z-add     0             conta             5 0
183400040628     c                   z-add     0             sav_FTDctb        3 0
183500040628     c                   z-add     0             sav_FTDctp        3 0
183600040727     C* Oltre  a salvare la tariffa vera e propria deve anche impostare un flag
183700040727     C* parallelamente poichè potrebbero esistere tariffe '000' e quindi potrebbe
183800040727     C* non essere comprensibile solo il dato sav_FTDct*
183900040727     c                   move      *blank        sav_FLGctb        1
184000040727     c                   move      *blank        sav_FLGctp        1
184100000622     C*
184200000622     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
184300000622     C* UTILE PER RICONOSCERE SE BOLLA POSTA
184400011105     C     KFTTT         SETLL     FIFTT02L
184500011105     C     KFTTT         READE     FIFTT02L                               39
184600000714     C**
184700000714     C     *IN39         DOWEQ     *OFF
184800000714     C**
184900011105     C     KFTDT         SETLL     fiftd001
185000000622     C                   DO        *HIVAL
185100011105     C     KFTDT         READE     fiftd001                               32
185200000622     C*
185300000622     C   32              LEAVE
185400021217     c                   add       1             conta
185500000714     C**
185600000622     C* CONTROLLO IN AZORG SE PO POSTE
185700000622     C                   EXSR      CTRORG
185800000622     C*  P.O. POSTE
185900020729     C     §OgNTW        IFEQ      'PPT'
186000000622     C                   MOVEL     'S'           VH4POS
186100040729     c                   if        ftdctp <> *blank and sav_FLGctp = ' '
186200040628     c                   move      ftdctp        sav_FTDctp
186300040727     c                   move      'S'           sav_FLGctp
186400040628     c                   end
186500000622     C                   ELSE
186600000622     C                   MOVEL     'S'           VH4BAR
186700040729     c                   if        ftdctp <> *blank and sav_FLGctb = ' '
186800040628     c                   move      ftdctp        sav_FTDctb
186900040727     c                   move      'S'           sav_FLGctb
187000040628     c                   end
187100000622     C                   ENDIF
187200000622     C*
187300000622     C                   ENDDO
187400000714     C**
187500011105     C     KFTTT         READE     FIFTT02L                               39
187600000714     C                   ENDDO
187700040727     **
187800040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
187900040727     C                   if        VH4BAR = ' ' and VH4POS = 'S'
188000040727     c                   move      sav_FTDctp    sav_FTDctb
188100040727     c                   end
188200040727     **
188300021217     C* Attenzione: Occorre gestire una eccezione.
188400021217     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
188500021217     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
188600021217     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
188700021217     C*  il VH4BAR come se avessi trovato solo delle bolle Bartolini.
188800021217     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
188900021217     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
189000021217     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
189100021217     C* su Bartolini. (Per Default)
189200030127     c                   if        vh4TSR='R' and conta=0 and vh2TBS > 0
189300021217     C                   MOVEL     'S'           VH4BAR
189400021217     C                   EndIF
189500021217     C*
189600000622     C                   ENDSR
189700020521     C*---------------------------------------------------------------*
189800020521     C* CONTROLLO IN AZORG SE PO POSTE
189900020212     C*---------------------------------------------------------------*
190000000622     C     CTRORG        BEGSR
190100020212     C*---------------------------------------------------------------*
190200000622     C* AGGANCIO AZORG
190300000622     C     FTDLNP        CHAIN     AZORG01L                           34
190400000622     C   34              MOVEL     *BLANKS       OG143
190500000622     C  N34              MOVEL     ORGDE3        OG143
190600000622     C*
190700000622     C                   ENDSR
190800020212     C*---------------------------------------------------------------*
190900931018     C* CONTROLLO DATI SUBFILE 4 -------------------------------------*
191000020212     C*---------------------------------------------------------------*
191100931018     C     CONTR4        BEGSR
191200931018     C                   Z-ADD     1             NRR4
191300931018     C                   SETOFF                                       90
191400911014     C*
191500931019     C                   READC     FRB8SF4                                32
191600930707     C*
191700931019    1C     *IN32         DOWEQ     *OFF
191800931019     C*
191900950117     C* RECORD DEL TIPO SERVIZIO
192000931019    2C     VH4NRR        IFEQ      0
192100931019     C*
192200931018     C* C O D I C E   T A R I F F A
192300011030    3C     VI4CTD        IFNE      '???'
192400931019     C* RICERCA
192500011112     C     '?'           SCAN      VI4CTD                                 30
192600011106     C*
192700011106     C* Se si tratta della 1° riga (significa Codice Tariffa)
192800011106     c     vh4tpr        ifeq      1
192900931018     C*
193000011112I   4C     *IN30         IFEQ      *ON
193100931028     C                   CLEAR                   PARAM3
193200931028     C                   MOVEL     VI4PDR        PA3PDR
193300931028     C                   MOVEL     PARSML        PA3SML
193400931028     C                   MOVEL     VH4TSR        PA3TSR
193500100513     C                   MOVEL     KPJBU         kpjbus
193600100513     C                   clear                   kpjbu
193700931028     C                   MOVEL     PARAM3        KPJBU
193800011112     C                   CALL      'FICN86R'
193900931028     C                   PARM                    KPJBA
194000931108     C                   MOVEL     KPJBU         PARAM3
194100100513     C                   MOVEL     KPJBUs        kpjbu
194200931028     C* SELEZIONE EFFETTUATA
194300931028     C     PA3SEL        IFEQ      ' '
194400011030     C                   MOVEL     PA3CTR        VI4CTD
194500931028     C                   ELSE
194600011030     C                   MOVEL     '???'         VI4CTD
194700931028     C                   ENDIF
194800931028     C*
194900931105     C                   SETON                                        36
195000931018     C                   WRITE     FRB8D05
195100000622    4C                   ENDIF
195200931018     C* CONTROLLO
195300011030     C                   TESTN                   VI4CTD               30
195400931105    5C     *IN30         IFEQ      *OFF
195500931105     C                   SETON                                        90
195600931105   X5C                   ELSE
195700931105     C*
195800011030     C                   MOVEL     VI4CTD        W01CTR
195900931105     C                   EXSR      CTRTAR
196000931105    5C                   ENDIF
196100931019     C   90              SETON                                        4036
196200000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
196300000622     C* POSTE O BARTOLINI
196400000622     C     *IN90         IFEQ      *OFF
196500000630     C* IN CASO DI DISTINTA GIA' VALORIZZATA NON SI FA
196600000630     C     *IN24         ANDEQ     *OFF
196700000622     C*
196800000622     C     VH4POS        IFEQ      'S'
196900000622     C     VH4BAR        ANDNE     'S'
197000000622     C* E LA TARIFFA È BARTOLINI ERRORE
197100000622     C     FGTFTP        ANDNE     'O'
197200040726     c                   if        dataoggi < finoadata
197300000622     C                   SETON                                        905236
197400040726     c                   end
197500000622     C                   ENDIF
197600000622     C*
197700000622     C     VH4POS        IFNE      'S'
197800000622     C     VH4BAR        ANDEQ     'S'
197900040622     C*
198000000622     C* E LA TARIFFA È BARTOLINI ERRORE
198100000622     C     FGTFTP        ANDEQ     'O'
198200000622     C                   SETON                                        905336
198300000622     C                   ENDIF
198400040622     C*
198500000703     C* SE CI SONO SPEDIZIONI MISTE MA MANCA ALMENO UNA DELLE DUE TARIFFE DO ERRORE
198600000703     C* LA PRIMA VOLTA CHE CONTROLLO
198700000703     C     VH4POS        IFEQ      'S'
198800000703     C     VH4BAR        ANDEQ     'S'
198900040622     C*
199000000703     C* MANCA TARIFFA POSTA
199100000703     C     VH4CTP        IFEQ      *BLANK
199200040727     ** oppure se in un secondo tempo non essendoci tariffe poste è stata impostata
199300040727     ** un'altra tariffa bartolini rispetto a quella standard proposta
199400040728     c     vh4TPO        oreq      *blank
199500040727     C     vh4CTP        andne     vi4CTD
199600040622     C*
199700040622     C*  fino al giorno 2004/08/10 il controllo è attivato
199800040726     C*   comunque imposta in Hidden il valore espresso a video
199900040726     C*    ma non da + l'errore
200000040622     c                   if        dataoggi < finoadata
200100000703     C                   SETON                                        905136
200200040726     c                   end
200300011030     C                   MOVEL     VI4CTD        VH4CTP
200400040622     C*
200500000703     C                   ENDIF
200600000703     C*
200700000703     C     VH4CTB        IFEQ      *BLANK
200800000703     C                   SETON                                        905036
200900011030     C                   MOVEL     VI4CTD        VH4CTB
201000000703     C                   ENDIF
201100000703     C*
201200000703     C                   ENDIF
201300000622     C*
201400000622     C* SE NON E' ACCESO IL 90 VALORIZZO I CAMPI TARIFFA HIDDEN SE CAMBIATI
201500000622     C     *IN90         IFEQ      *OFF
201600000622     C*+
201700000622     C     FGTFTP        IFEQ      'O'
201800011030     C                   MOVEL     VI4CTD        VH4CTP
201900000622     C                   ELSE
202000011030     C                   MOVEL     VI4CTD        VH4CTB
202100000622     C                   ENDIF
202200000622     C*
202300000622     C                   ENDIF
202400000622     C                   ENDIF
202500011106     C*
202600011106    4C                   end
202700011106     C*
202800931021    3C                   ENDIF
202900931019     C*
203000931108     C* NON E' POSSIBILE ANDARE IN MANUTENZIONE DI UN TIPO PRESTAZIONE
203100931019     C  N90VI4SCE        IFEQ      'M'
203200931019     C                   SETON                                        419036
203300931019     C                   ENDIF
203400931019     C*
203500931108     C* ANNULLAMENTO VALORIZZAZIONE DI TUTTO IL TIPO PRESTAZIONE
203600931019     C*  LO ESEGUO DOPO
203700931019     C  N90VI4SCE        IFEQ      'A'
203800931112     C   20              SETON                                        459036
203900931112     C  N20              SETON                                        36
204000931019     C                   ENDIF
204100931019     C*
204200931019   X2C                   ELSE
204300011107     C*
204400931019     C* RECORD DI DISTINTA/RITIRO
204500011107     C*
204600931019     C* MANUTENZIONE SPEDIZIONI DELLA DISTINTA
204700931022    3C     VI4SCE        IFEQ      'M'
204800931022    4C     VI4ITT        IFGT      0
204900931022     C     VI4ITA        ANDGT     0
205000931022     C                   SETON                                        439036
205100931022   X4C                   ELSE
205200931019     C                   EXSR      MANSPE
205300931019     C                   SETON                                        90
205400931028     C                   WRITE     FRB8D05
205500931105     C*
205600931105     C* RIAGGANCIO IL RECORD DI SUBFILE
205700931105     C     NRR4          CHAIN     FRB8SF4                            32
205800931022    4C                   ENDIF
205900931022    3C                   ENDIF
206000931019     C*
206100931019     C* ANNULLO VALORIZZAZIONE PRECEDENTE DELLA DISTINTA
206200931019     C     VI4SCE        IFEQ      'A'
206300931105     C     VI1FVL        ANDEQ     'V'
206400931112     C   20              SETON                                        459036
206500931112     C  N20              EXSR      ANNSPE
206600931112     C  N20              SETON                                        21
206700931105     C                   ENDIF
206800931019     C*
206900950117     C* CONTROLLO PIKING/ETICHETTATURA S/N
207000950117     C* Se = Blank imposto quello specificato su anagrafico padroncini
207100950117     C     VI4PEP        IFEQ      *BLANKS
207200950127     C                   MOVEL     VH4PEA        VI4PEP
207300950117     C                   END
207400021211      *
207500021211     C* Se = Blank imposto quello specificato su anagrafico padroncini
207600021211      *   per carico/scarico
207700021211     C     vi4FCS        IFEQ      *BLANKS
207800021211     C                   eval      vi4FCS = vh4ACS
207900021211     C                   END
208000950117     C*
208100931019    2C                   ENDIF
208200921228     C*
208300931019    2C     VH4NRR        IFEQ      0
208400931018     C                   SETOFF                                       11
208500931018     C                   ELSE
208600931018     C                   SETON                                        11
208700931019    2C                   ENDIF
208800911017     C*
208900931019     C  N36              MOVEL     ' '           VI4SCE
209000931018     C                   Z-ADD     NRR4          REC4
209100011109     c     vh4tpr        comp      2                                      26
209200931019     C                   UPDATE    FRB8SF4
209300931018     C                   SETOFF                                       36
209400930707     C*
209500931019     C   90              SETON                                        32
209600931019     C  N90              READC     FRB8SF4                                32
209700931018    1C                   ENDDO
209800931019     C**
209900931019     C* SE TUTTO OK CONTROLLO SE HO ANNULLAMENTI DI VALORIZZAZIONE
210000931108     C*  DI TIPO PRESTAZ DA FARE
210100931108    1C     *IN90         IFEQ      *OFF
210200931019     C                   READC     FRB8SF4                                32
210300931019     C*
210400931108    2C     *IN32         DOWEQ     *OFF
210500931105     C*
210600931108    3C     VI4SCE        IFEQ      'A'
210700931108     C* SE NON SONO GIA' VALORIZZATE PULISCO SOLO IL CAMPO DI SCELTA SF
210800931108    4C     VI1FVL        IFEQ      'V'
210900931019     C                   EXSR      ANNTSR
211000931105     C*
211100931105     C* AL TERMINE RIEMETTO IL SFL2 (21 ON)
211200931105     C                   SETON                                        21
211300931108   X4C                   ELSE
211400931108     C                   MOVEL     ' '           VI4SCE
211500011109     c     vh4tpr        comp      2                                      26
211600931108     C                   UPDATE    FRB8SF4
211700931108    4C                   ENDIF
211800931105     C*
211900931108    3C                   ENDIF
212000931019     C*
212100931019     C                   READC     FRB8SF4                                32
212200931108    2C                   ENDDO
212300931108    1C                   ENDIF
212400911017     C*
212500900614     C                   ENDSR
212600020212     C*---------------------------------------------------------------*
212700931019     C* MANUTENZIONE SPEDIZIONI DISTINTA SELEZIONATA -----------------*
212800020212     C*---------------------------------------------------------------*
212900931019     C     MANSPE        BEGSR
213000931110     C                   SETOFF                                       151637
213100931108     C                   CLEAR                   FRB8SF6
213200931028     C     VI4ITT        IFGT      0
213300931028     C                   SETON                                        15
213400931028     C                   ENDIF
213500931028     C     VI4ITA        IFGT      0
213600931028     C                   SETON                                        16
213700931028     C                   ENDIF
213800931028     C*
213900931019     C* CARICAMENTO SPEDIZIONI
214000931019     C                   EXSR      CARSPE
214100130305     C*
214200931110     C* SE NON CE NE SONO NON EMETTO IL SUBFILE
214300030127     c                   move      'N'           Dett6_a0          1
214400931110     C     NRR6          IFEQ      0
214500931110     C                   SETON                                        37
214600030127     c                   move      'S'           Dett6_a0
214700030127     c                   z-add     vi4ran        d8ran
214800931110     C                   ENDIF
214900931019     C*
215000931103     C                   Z-ADD     1             REC6
215100931019     C                   WRITE     FRB8D07
215200030127     C   37              WRITE     FRB8D08
215300931019     C     FORCT6        TAG
215400020409     c                   if        savnr6 > 0
215500020409     c                   z-add     savnr6        rec6
215600020409     c                   end
215700931019     C                   EXFMT     FRB8CT6
215800020409     c                   z-add     nrrult        savnr6            5 0
215900931019     C* CMD3 - FINE LAVORO
216000931019     C     *INKC         IFEQ      *ON
216100931111     C   22              SETON                                        4490
216200931111     C   22              GOTO      FORCT6
216300931019     C                   SETON                                        LR
216400931019     C                   GOTO      ENDMAN
216500931019     C                   ENDIF
216600931019     C* CMD12 - RITORNO
216700931019     C   KL              GOTO      ENDMAN
216800030127      *
216900030127      * CONTROLLI solo se ci sono delle righe e non sono solo
217000030127      *   dei ritiri annullati
217100030127     c                   if        Dett6_a0 = 'N'
217200931019     C                   EXSR      CONTR6
217300030127     c                   end
217400931202     C   90              GOTO      FORCT6
217500020418     C*
217600020418     c                   if        VI1FVL<>'V'
217700931019     C* CMD6 - AGGIORNAMENTO
217800931202     C   KE
217900030127     cOR KF              if        Dett6_a0 = 'N'
218000030127     C                   EXSR      AGGIO6
218100020408     C   ke
218200020408     Cor kf              commit
218300020418     c                   end
218400030127     c                   end
218500931202     C* CMD5 RIVISUALIZZA
218600931202     C   KE              EXSR      CARSPE
218700931202     C   KE              Z-ADD     1             REC6
218800931202     C* RIEMETTO VIDEATA PER ENTER O CMD5
218900931202     C  NKF              GOTO      FORCT6
219000931019     C*
219100931021     C     ENDMAN        ENDSR
219200020212     C*---------------------------------------------------------------*
219300950116     C* CARICO LE SPEDIZIONI DELLA DISTINTA A VIDEO ------------------*
219400020212     C*---------------------------------------------------------------*
219500931019     C     CARSPE        BEGSR
219600931021     C                   CLEAR                   NRR6
219700931021     C                   CLEAR                   W06NRR
219800931019     C*
219900931019     C                   SETON                                        35
220000931019     C                   WRITE     FRB8CT6
220100931019     C                   SETOFF                                       35
220200931019     C*
220300931019     C* DECODIFICA C A M P I   T E S T A T A
220400931019     C*
220500931019     C                   MOVEL     '1N'          COD
220600931019     C                   MOVEL     *BLANKS       KEY
220700931103     C                   MOVEL     VH4TSM        KEY
220800931019     C     KTAB          CHAIN     TABEL                              30
220900931019     C  N30              MOVEA     TBLUNI        K17(3)
221000931019     C   30              MOVEA     *BLANKS       K17(3)
221100931103     C                   MOVEL     VH4TSM        K17(1)
221200931019     C                   MOVEL     '-'           K17(2)
221300931019     C                   MOVEA     K17           VI6TSM
221400931019     C* MITTENTE/DESTINATARIO
221500931022    1C     VH4TSR        IFEQ      'R'
221600931104     C                   SETON                                        19
221700020311     C                   MOVEL     'In'          VI6IA
221800020311     C                   MOVEL     CSPED         VI6MD
221900931108     C*
222000931108   X1C                   ELSE
222100931108     C*
222200931104     C                   SETOFF                                       19
222300931029     C                   MOVEL     'In'          VI6IA
222400931202     C                   MOVEL     CSPED         VI6MD
222500931019    1C                   ENDIF
222600931021     C*
222700931021     C* REPERISCO CODICE TARIFFA DALLA RIGA IN CUI SI TROVA
222800121221     c                   move      'N'           h6targ            1
222900931103     C                   Z-ADD     NRR4          W05NRR
223000931021     C     VH4NRR        CHAIN     FRB8SF4                            30
223100931022     C   30              MOVEL     '000'         W01CTR
223200011106     c                   if        vh4tpr = 1
223300011030     C  N30VI4CTD        IFEQ      '???'
223400121221     c                   move      'S'           h6targ
223500931022     C                   MOVEL     000           W01CTR
223600931022     C                   ELSE
223700011030     C                   MOVEL     VI4CTD        W01CTR
223800931021     C                   ENDIF
223900011105     c                   endif
224000011105      *
224100000703     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
224200000703     C     VH4CTP        IFNE      *BLANKS
224300000703     C     VH4CTB        ANDNE     *BLANKS
224400000703     C                   MOVEL     VH4CTP        WCTP              3
224500000703     C                   MOVEL     VH4CTB        WCTB              3
224600000703     C                   ELSE
224700000703     C                   MOVEL     *BLANKS       WCTP
224800000703     C                   MOVEL     *BLANKS       WCTB
224900000703     C                   ENDIF
225000931103     C                   Z-ADD     W05NRR        NRR4
225100931028     C     NRR4          CHAIN     FRB8SF4                            30
225200931019     C*
225300931019     C* DECODIFICA  C A M P I   D E T T A G L I O
225400931019     C                   CLEAR                   K17
225500931203     C                   CLEAR                   WFTD
225600931108     C*
225700011105     C     KFTD          SETLL     fiftd001
225800011105     C     KFTD          READE     fiftd001                               32
225900931019     C*
226000931019    1C     *IN32         DOWEQ     *OFF
226100931203     C                   ADD       1             WFTD              5 0
226200000626     C                   SETOFF                                       132325
226300000626     C                   SETOFF                                       49
226400950118     C*
226500950119     C                   MOVE      *BLANKS       VI6FTC
226600950119     C                   MOVE      *BLANKS       VI6FT2
226700950119     C                   MOVE      *BLANKS       VI6TSP
226800030320     C                   MOVE      *BLANKS       Vh6cbo
226900950119     C                   MOVE      *BLANKS       VH6SIC
227000950119     C                   MOVE      *BLANKS       VH6GVA
227100950119     C                   MOVE      *ZEROS        VH6NCE
227200000705     C                   MOVE      *ZEROS        VH6FGC
227300000623     C*
227400000623     C* CONTROLLO SE BOLLA POSTA O BARTOLINI
227500000623     C                   EXSR      CTRORG
227600020729     C     §OgNTW        IFEQ      'PPT'
227700020729     c                   movel     'S'           VH6POS
227800020729     c                   else
227900020729     C                   MOVEL     *blank        VH6POS
228000020729     c                   end
228100000626     C* SE BOLLA POSTA PROTEGGO IL CAMPO NUMERO STOP
228200020729     C     §OgNTW        IFEQ      'PPT'
228300000626     C                   SETON                                        49
228400000626     C                   ELSE
228500000626     C                   SETOFF                                       49
228600000626     C                   ENDIF
228700950119     C*
228800011123     C                   z-add     FTDSTP        VI6STP                          STOP
228900020313     C* ----------
229000020313     C* servono questi 2 campi hidden per poter gestire la vista logica FIFTD04L
229100020313     C* in alternativa alla FIFTD02L
229200020313     C                   z-add     FTDksc        vh6ksc                          cliente
229300020313     C                   z-add     FTDnsp        vh6nsp                          spedizione
229400020326     C                   move      FTDtsr        vh6tsr
229500020326     C                   z-add     FTDfgs        vh6fgs
229600020326     c                   if        vh6fgs=0
229700020326     C                   movel     FTDpdr        vh6fgs
229800020326     c                   end
229900020313     C* ----------
230000950116     C                   MOVEL     FTDRSC        VI6RSB                          RAG SOCIALE
230100950113     C                   MOVEL     FTDCAP        VI6CAP
230200950118     C                   Z-ADD     FTDCAS        VH6CAS                          CONTRASSEGNO
230300950131     C                   MOVEL     FTDVCA        VH6VCA                          DIVISA
230400011106     c                   if        vh6vca=*blank
230500011106     c                   eval      vh6vca='ITL'
230600011106     c                   end
230700950118     C                   Z-ADD     FTDIFP        VH6IFP                          IMP.INCASS.
230800011109      ***>>>>>>>>>>>>>>
230900011109     c                   if        vh4tpr = 2
231000011109     c                   movel     vi4ctd        vi6div
231100011109     c                   end
231200950118     C                   Z-ADD     FTDSET        VH6SET
231300931202     C* RITIRI
231400931202    2C     VH4TSR        IFEQ      'R'
231500020311     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
231600020311    2C     ftdnsp        ifeq      0
231700020311    2C     ftdksc        andgt     0
231800020311     C                   z-add     ftdksc        vi6nsp                          COD CLIENTE
231900020311     C                   end
232000950118     C                   Z-ADD     FTDNCE        VH6NCE                          COLLI ETICH
232100931202     C                   ELSE
232200931202     C* CONSEGNE
232300091012     C     Bolla_FTD     CHAIN     FNARB01L                           30
232400090918     C                   Z-ADD     arbKSC        Vh6ksc                          Cliente
232500090921      *
232600960208     C                   MOVEL     ARBGVA        VH6GVA                          PARTICOLARITA'
232700960208     C                   MOVE      ' '           VH6GVA
232800950118     C                   MOVEL     ARBTSP        VI6TSP                          C/E/D
232900030320     C                   MOVEL     ARBcbo        Vh6cbo                          C/E/D
233000931202     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
233100950113     C                   MOVE      ARBMGS        DATA                            DATA SPEDIZ
233200931019     C                   MOVEL     GG            VI6MGS
233300931019     C                   MOVE      MM            VI6MGS
233400931028     C* SPEDIZIONE CONSEGNATA
233500931105    3C     FTDDCM        IFGT      0
233600950113     C                   MOVE      FTDDCM        DATA
233700931019     C                   MOVEA     GG            K17(1)
233800931019     C                   MOVEL     '/'           K17(3)
233900931019     C                   MOVEA     MM            K17(4)
234000931019     C                   MOVEL     '/'           K17(6)
234100931019     C                   MOVEA     AA            K17(7)
234200931019     C                   MOVEA     K17           VI6CON
234300950117     C* FLAG RITORNO ALL'INCASSO
234400950117     C     FTDSIC        COMP      'S'                                    23
234500950117     C                   MOVEL     FTDSIC        VH6SIC
234600931029     C*
234700950113     C* CONSEGNA PARTICOLARE 1 (SOLO SE E' CONSEGNATA)
234800950113    3C     ARBTC1        IFNE      ' '
234900931029     C                   MOVEL     '1P'          COD
235000931029     C                   MOVEL     *BLANKS       KEY
235100950113     C                   MOVEL     ARBTC1        KEY
235200931029     C     KTAB          CHAIN     TABEL                              30
235300931029     C*
235400931029     C  N30              MOVEA     TBLUNI        K17(3)
235500931029     C   30              MOVEA     *BLANKS       K17(3)
235600950113     C                   MOVEL     ARBTC1        K17(1)
235700931029     C                   MOVEL     '-'           K17(2)
235800931029     C                   MOVEA     K17           VI6FTC
235900931029     C                   ENDIF
236000950113     C* CONSEGNA PARTICOLARE 2 (SOLO SE E' CONSEGNATA)
236100950113    3C     ARBTC2        IFNE      ' '
236200950113     C                   MOVEL     '1P'          COD
236300950113     C                   MOVEL     *BLANKS       KEY
236400950113     C                   MOVEL     ARBTC2        KEY
236500950113     C     KTAB          CHAIN     TABEL                              30
236600950113     C*
236700950113     C  N30              MOVEA     TBLUNI        K17(3)
236800950113     C   30              MOVEA     *BLANKS       K17(3)
236900950113     C                   MOVEL     ARBTC2        K17(1)
237000950113     C                   MOVEL     '-'           K17(2)
237100950113     C                   MOVEA     K17           VI6FT2
237200950113     C                   ENDIF
237300931019     C*
237400931019   X3C                   ELSE
237500931019     C*
237600931019     C* C'E' CODICE GIACENZA/MANCATA CONSEGNA
237700931019    4C     FTDCMC        IFNE      ' '
237800931019     C                   MOVEA     FTDCMC        K17(1)
237900931019     C                   MOVEL     '-'           K17(4)
238000931019     C                   MOVEL     '2A'          COD
238100931019     C                   MOVEL     *BLANKS       KEY
238200931019     C                   MOVEL     FTDCMC        KEY
238300931019     C     KTAB          CHAIN     TABEL                              30
238400931019     C  N30              MOVEA     TBLUNI        K17(5)
238500931021     C   30              MOVEA     *BLANKS       K17(5)
238600931019     C                   MOVEA     K17           VI6CON
238700000705     C*
238800000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
238900000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
239000000623     C     VH6POS        IFEQ      'S'
239100000623     C                   MOVEL     '2Z'          COD
239200000623     C                   MOVEL     *BLANKS       KEY
239300000623     C                   MOVEL     FTDCMC        KEY
239400000623     C     KTAB          CHAIN     TABEL                              30
239500000623     C  N30              MOVEL     TBLUNI        DS2Z
239600000623     C   30              MOVEL     *BLANKS       DS2Z
239700000626     C     §2ZTPP        IFEQ      ' '
239800000626     C* NON PAGO LA SPEDIZIONE
239900000630     C                   SETON                                        2513
240000000626     C*
240100000626     C                   ENDIF
240200000623     C                   MOVEL     §2ZTPP        VH6FGC
240300000623     C                   MOVEL     §2ZTPP        FTDFGC
240400000623     C                   ENDIF
240500931019     C*
240600931019   X4C                   ELSE
240700931019     C* C'E' MANCATA CONSEGNA
240800931019     C                   MOVEA     FTDTMC        K17(1)
240900931019     C                   MOVEL     '-'           K17(2)
241000931019     C                   MOVEL     '2A'          COD
241100931019     C                   MOVEL     *BLANKS       KEY
241200931019     C                   MOVEL     FTDTMC        KEY
241300931019     C     KTAB          CHAIN     TABEL                              30
241400931019     C  N30              MOVEA     TBLUNI        K17(3)
241500931021     C   30              MOVEA     *BLANKS       K17(3)
241600931019     C                   MOVEA     K17           VI6CON
241700931019     C*
241800931028     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
241900931019     C* PRIMA CERCO CON FILIALE DI APPARTENENZA
242000931021     C                   MOVEL     KEY           WALF6             6
242100931111     C                   EXSR      CTRCMC
242200000630    4C                   ENDIF
242300931019     C*
242400950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
242500931028     C*                                    (SE NON L'HO TROVATA)
242600931028     C     *IN30         IFEQ      *OFF
242700931019     C                   SETON                                        13
242800931019    4C                   ENDIF
242900931019    3C                   ENDIF
243000931028     C*
243100950119     C* CODICE BOLLA
243200030320     C                   MOVEL     ARBCBO        VI6CBO
243300950119     C                   MOVEL     '3A'          COD
243400950119     C                   MOVEL     *BLANKS       KEY
243500950119     C                   MOVEL     ARBCBO        KEY
243600950119     C     KTAB          CHAIN     TABEL                              30
243700950119     C  N30              MOVEL     TBLUNI        DS3A
243800950119     C  N30              MOVEL     §3ADES        VI6CBD
243900950119     C   30              MOVEL     *BLANKS       VI6CBD
244000950119     C*
244100931028     C* C O D I C E   B O L L A   D A   T A S S A R E
244200931028    3C     *IN13         IFEQ      *OFF
244300931111     C                   EXSR      CTRCBL
244400931028     C*
244500950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
244600931028     C*                                    (SE L'HO TROVATO)
244700931111    4C     *IN31         IFEQ      *ON
244800931028     C                   SETON                                        13
244900931028    4C                   ENDIF
245000931028    3C                   ENDIF
245100931028    2C                   ENDIF
245200931202     C*
245300931202     C* AGGIORNO PERCHE' POTREI AVER IMPOSTATO IL FLAG DI RICONS.GIAC
245400011105     C                   UPDATE    fiftd001
245500931019     C*
245600931022     C     FTDCTP        IFNE      *BLANKS
245700931022     C                   MOVEL     FTDCTP        VI6CTR
245800931022     C                   ELSE
245900000622     C* VERIFICO SE ESISTONO SIA BOLLE POSTE CHE BOLLE BARTOLINI
246000000703     C     WCTB          IFNE      '   '
246100000703     C     WCTP          ANDNE     '   '
246200000622     C* SE E' UN PO POSTE
246300000622     C     VH6POS        IFEQ      'S'
246400000622     C* MUOVO TARIFFA POSTA
246500000703     C                   MOVEL     WCTP          VI6CTR
246600000622     C                   ELSE
246700000622     C* MUOVO TARIFFA BARTOLINI
246800000703     C                   MOVEL     WCTB          VI6CTR
246900000622     C                   ENDIF
247000000622     C                   ELSE
247100121221     C                   MOVEL     h6targ        Vh6targ
247200121221     C                   MOVEL     W01CTR        VI6CTR
247300000622     C                   ENDIF
247400000622     C*
247500931022     C                   ENDIF
247600011105      *
247700011105      *  pulito il campo di totale
247800011108     C                   clear                   vi6tot
247900011115     c                   move      '0'           vp6ita
248000011115     c                   move      '0'           vp6itt
248100011115      *
248200011115      * imposta il flag di protezione in memoria sulla riga
248300011115     c                   if        *in13 = *on or *in16 = *on
248400011115     c                   eval      vp6ita = '1'
248500011115     c                   end
248600011115      *
248700011115      * imposta il flag di protezione in memoria sulla riga
248800011115     c                   if        *in15 = *on or *in23 = *on or *in25 = *on
248900011115     c                   eval      vp6itt = '1'
249000011115     c                   end
249100011115      * pulisce i campi
249200011115     C                   z-add     0             vh6ita
249300011115     C                   z-add     0             vi6ita
249400011115     C                   z-add     0             vh6itt
249500011115     C                   z-add     0             vi6itt
249600011105      *
249700950117     C* ACCESSORI NON VISUALIZZABILI: NON CARICO NEMMENO L'IMPORTO
249800931105     C     *IN13         IFEQ      *ON
249900950119     C                   MOVEL     'B'           VH6FIA
250000931105     C                   ELSE
250100011109     C                   MOVEL     FTDITA        VI6ITA
250200011115     C  N16              add       ftdita        vi6tot
250300931022     C                   MOVEL     FTDITA        VH6ITA
250400931021     C                   MOVEL     FTDFIA        VH6FIA
250500931105     C                   ENDIF
250600950117     C* NON CARICO IMPORTO BASE SE SI TRATTA DI UN RITORNO ALL'INCASSO
250700950118     C     FTDSIC        IFNE      'S'
250800011109     C                   MOVEL     FTDITT        VI6ITT
250900011115     C  N15              add       ftditt        vi6tot
251000950117     C                   MOVEL     FTDITT        VH6ITT
251100950117     C                   MOVEL     FTDFIT        VH6FIT
251200950119     C                   ELSE
251300950119     C                   MOVEL     'B'           VH6FIT
251400950117     C                   ENDIF
251500931105     C                   MOVEL     FTDAAS        VH6AAS
251600931019     C                   ADD       1             NRR6
251700020313     C*
251800020418     c     vi1fvl        comp      'V'                                    57
251900931019     C                   WRITE     FRB8SF6
252000931019     C*
252100011105     C     KFTD          READE     fiftd001                               32
252200931019    1C                   ENDDO
252300931203     C*
252400931203     C                   MOVEL     W01CTR        W01CTP
252500931019     C*
252600931019     C                   ENDSR
252700020212     C*---------------------------------------------------------------*
252800931019     C* ANNULLAMENTO VALORIZZAZIONE DI UNA DISTINTA ------------------*
252900020212     C*---------------------------------------------------------------*
253000931019     C     ANNSPE        BEGSR
253100931108     C*
253200011105     C     KFTD          SETLL     fiftd001
253300011105     C     KFTD          READE     fiftd001                               32
253400931019     C*
253500931022    1C     *IN32         DOWEQ     *OFF
253600931022     C* 14 ON  - ANNULLO TUTTO
253700931022    2C     *IN14         IFEQ      *ON
253800931022     C                   CLEAR                   FTDFIT                          FLG TAR BAS
253900931022     C                   CLEAR                   FTDITT                          TAR BASE
254000931022     C                   CLEAR                   FTDFIA                          FLG TAR ACC
254100931022     C                   CLEAR                   FTDITA                          TAR ACCESS
254200931022     C*
254300931022   X2C                   ELSE
254400931022     C* 14 OFF - ANNULLO SOLO LE VALORIZZAZIONI AUTOMATICHE
254500931022     C*
254600931022    3C     FTDFIT        IFEQ      ' '
254700931022     C                   CLEAR                   FTDITT                          TAR BASE
254800931022    3C                   ENDIF
254900931022    3C     FTDFIA        IFEQ      ' '
255000931022     C                   CLEAR                   FTDITA                          TAR ACCESS
255100931022    3C                   ENDIF
255200931022    2C                   ENDIF
255300931022     C*
255400020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
255500020326     C* i primi tre caratteri
255600020326     c                   if        ftdfgs = 0
255700020326     c                   movel     ftdpdr        ftdfgs
255800020326     c                   end
255900020326     C*
256000931022     C                   CLEAR                   FTDCTP                          COD TAR
256100011105     C                   UPDATE    fiftd001
256200931019     C*
256300020326     C*          *-------------------------------------*
256400020408     c     parsml        ifeq      ' '
256500020326     C*  deve annullare/cancellare (se c'è) anche la voce di C/E
256600020326     c     Kfce          setll     FiFCE01L
256700020326     c     Kfce          reade     FiFCE01L
256800020326     c                   dow       not %Eof(FiFCE01L)
256900020326     c                   delete    FIFCE000
257000020326     c     Kfce          reade     FiFCE01L
257100020326     C                   enddo
257200020408     C                   endif
257300020326     C*          *-------------------------------------*
257400011105     C     KFTD          READE     fiftd001                               32
257500931022    1C                   ENDDO
257600931019     C*
257700931022     C* 14 OFF - SVALORIZZO ANCHE LA TESTATA
257800931022    1C     *IN14         IFEQ      *OFF
257900931022     C*
258000011105     C     KFTT          CHAIN     FIFTT000                           32
258100931022    2C     *IN32         IFEQ      *OFF
258200931022     C*
258300931022    3C     FTTFIT        IFEQ      ' '
258400931022     C                   CLEAR                   FTTITT
258500931022    3C                   ENDIF
258600940121     C*
258700931022    3C     FTTFIA        IFEQ      ' '
258800931022     C                   CLEAR                   FTTITA
258900931022    3C                   ENDIF
259000931022     C*
259100940121     C                   Z-ADD     VI4ITT        FTTITT
259200940121     C     VI4ITT        IFGT      0
259300940121     C                   MOVEL     'M'           FTTFIT
259400940121     C                   ENDIF
259500940121     C                   Z-ADD     VI4ITA        FTTITA
259600940121     C     VI4ITA        IFGT      0
259700940121     C                   MOVEL     'M'           FTTFIA
259800940121     C                   ENDIF
259900020326     C*
260000020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
260100020326     C* i primi tre caratteri
260200020326     c                   if        fttfgs = 0
260300020326     c                   movel     fttpdr        fttfgs
260400020326     c                   end
260500940121     C*
260600940121     C                   MOVEL     ' '           FTTFVL                          FLG VALORIZ
260700940121     C                   MOVEL     ' '           FTTFLA                          FLG MAN TAR
260800931123     C                   CLEAR                   FTTSNA
260900931125     C                   CLEAR                   FTTCLA
261000950119     C                   CLEAR                   FTTCPE
261100931123     C                   CLEAR                   FTTKFA
261200931123     C                   CLEAR                   FTTTVL
261300950119     C                   CLEAR                   FTTSNE
261400931202     C                   CLEAR                   FTTSNP
261500011105     C                   UPDATE    FIFTT000
261600931022    2C                   ENDIF
261700931022    1C                   ENDIF
261800931019     C*
261900931019     C                   ENDSR
262000020212     C*---------------------------------------------------------------*
262100931108     C* ANNULLAMENTO VALORIZZAZIONE DI UN TIPO PRESTAZ ---------------*
262200020212     C*---------------------------------------------------------------*
262300931019     C     ANNTSR        BEGSR
262400931021     C     VI4SCE        DOWEQ     'A'
262500931021     C* ELIMINO IL FLAG
262600931021     C                   MOVEL     ' '           VI4SCE
262700011109     c     vh4tpr        comp      2                                      26
262800931021     C                   UPDATE    FRB8SF4
262900931021     C*
263000931019     C                   Z-ADD     VI4PDR        W01PDR
263100931019     C                   MOVEL     VH4TSR        W01TSR
263200931108     C*
263300931019     C                   ADD       1             NRR4
263400931019     C     NRR4          CHAIN     FRB8SF4                            32
263500931019     C*
263600931108     C* LEGGO TUTTE LE DISTINTE O I RITIRI DI QUEL PADR/TIPO PRESTAZ
263700931019     C     *IN32         DOWEQ     *OFF
263800931019     C     VI4PDR        ANDEQ     W01PDR
263900931019     C     VH4TSR        ANDEQ     W01TSR
264000931019     C*
264100931019     C                   EXSR      ANNSPE
264200931019     C*
264300931103     C                   ADD       1             NRR4
264400931019     C     NRR4          CHAIN     FRB8SF4                            32
264500931019     C                   ENDDO
264600931021     C                   ENDDO
264700931019     C*
264800931019     C                   ENDSR
264900020212     C*---------------------------------------------------------------*
265000931019     C* CONTROLLO SFL6 -----------------------------------------------*
265100020212     C*---------------------------------------------------------------*
265200931019     C     CONTR6        BEGSR
265300011105     C*
265400931019     C                   Z-ADD     1             NRR6
265500931019     C                   SETOFF                                       90
265600931103     C                   CLEAR                   WALF3
265700931203     C                   CLEAR                   WREAD
265800931021     C                   SETON                                        36
265900931019     C                   READC     FRB8SF6                                32
266000931019     C*
266100931019    1C     *IN32         DOWEQ     *OFF
266200931203     C                   ADD       1             WREAD             5 0
266300931021     C*
266400931123     C* CONTROLLO SE HO INSERITO DEI VALORI MANUALMENTE
266500931123     C* T A R I F F A   B A S E
266600011109      *>>>>>>>>>>>>>>>
266700011109    2C     VI6ITT        IFNE      VH6ITT
266800931022     C*
266900011109    3C     VI6ITT        IFGT      0
267000931021     C                   MOVEL     'M'           VH6FIT
267100931022   X3C                   ELSE
267200931021     C                   MOVEL     ' '           VH6FIT
267300931022    3C                   ENDIF
267400011105     C*
267500011109     C                   MOVEL     VI6ITT        VH6ITT
267600011108     C*>>>>>>>>
267700931022    2C                   ENDIF
267800931021     C*
267900931022     C* T A R I F F A   A C C E S S O R I
268000011108      *>>>>>>>>>>>>>>>
268100011109    2C     VI6ITA        IFNE      VH6ITA
268200931022     C*
268300011109    3C     VI6ITA        IFGT      0
268400931206     C                   MOVEL     'M'           VH6FIA
268500931022   X3C                   ELSE
268600931206     C                   MOVEL     ' '           VH6FIA
268700931022    3C                   ENDIF
268800011105     C*
268900011109     C                   MOVEL     VI6ITA        VH6ITA
269000011108     C*>>>>>>>>
269100931022    2C                   ENDIF
269200931019     C*
269300931019     C* C O D I C E   T A R I F F A
269400931019     C* RICERCA
269500121221     c                   if        vh6targ <> 'S'
269600931019     C     '?'           SCAN      VI6CTR                                 90
269700931019     C*
269800931021    2C     *IN90         IFEQ      *ON
269900931028     C                   CLEAR                   PARAM3
270000931028     C                   MOVEL     VI4PDR        PA3PDR
270100931028     C                   MOVEL     PARSML        PA3SML
270200931028     C                   MOVEL     VH4TSR        PA3TSR
270300100513     C                   MOVEL     KPJBU         kpjbus
270400100513     C                   clear                   kpjbu
270500931028     C                   MOVEL     PARAM3        KPJBU
270600011112     C                   CALL      'FICN86R'
270700931028     C                   PARM                    KPJBA
270800931110     C                   MOVEL     KPJBU         PARAM3
270900100513     C                   MOVEL     KPJBUs        kpjbu
271000931028     C* SELEZIONE EFFETTUATA
271100931028     C     PA3SEL        IFEQ      ' '
271200931028     C                   MOVEL     PA3CTR        VI6CTR
271300931028     C                   ELSE
271400931028     C                   MOVEL     '000'         VI6CTR
271500931028     C                   ENDIF
271600121221      *
271700931028     C* RIEMETTO I PIEDI DEL VIDEO
271800931019     C                   WRITE     FRB8D07
271900931028     C*
272000000622   X2C                   ELSE
272100931019     C* CONTROLLO
272200931105     C                   TESTN                   VI6CTR               30
272300931105     C     *IN30         IFEQ      *OFF
272400931105     C                   SETON                                        4290
272500931105     C                   GOTO      ENDCT6
272600931105     C                   ENDIF
272700931105     C*
272800931019     C                   MOVEL     VI6CTR        W01CTR
272900121221      *
273000121221      * Controlla la Tariffa
273100931019     C                   EXSR      CTRTAR
273200121221      *
273300000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
273400000622     C* POSTE O BARTOLINI
273500000622     C     *IN90         IFEQ      *OFF
273600000622     C*
273700000622     C     VH6POS        IFEQ      'S'
273800000622     C* E LA TARIFFA È BARTOLINI ERRORE
273900000622     C     FGTFTP        ANDNE     'O'
274000000623     C* ED ESISTE UNA TARIFFA BARTOLINI
274100000623     C     VH4CTB        ANDNE     '   '
274200040622     C*
274300040622     C*  fino al giorno 2004/08/10 il controllo è attivato
274400040622     c                   if        dataoggi < finoadata
274500000622     C                   SETON                                        9054
274600000622     C                   GOTO      ENDCT6
274700040622     c                   end
274800040622     C*
274900000622     C                   ENDIF
275000000622     C*
275100000622     C     VH6POS        IFNE      'S'
275200000622     C* E LA TARIFFA È BARTOLINI ERRORE
275300000622     C     FGTFTP        ANDEQ     'O'
275400000623     C* ED ESISTE UNA TARIFFA POSTE
275500000623     C     VH4CTP        ANDNE     '   '
275600000622     C                   SETON                                        9055
275700000622     C                   GOTO      ENDCT6
275800000622     C                   ENDIF
275900000622     C*
276000000622     C                   ENDIF
276100931021     C*
276200931021    3C     *IN90         IFEQ      *ON
276300931021    4C     *IN30         IFEQ      *OFF
276400000622     C                   SETON                                        40
276500931021   X4C                   ELSE
276600931021     C*
276700931021     C                   MOVEL     VI6CTR        WALF1             1
276800931021     C                   MOVEL     'TR'          COD
276900931021     C                   MOVEL     *BLANKS       KEY
277000931021     C                   MOVEL     WALF1         KEY
277100931021     C     KTAB          CHAIN     TABEL                              30
277200931021     C*
277300931021    5C     *IN30         IFEQ      *ON
277400931021     C     *IN30         OREQ      *OFF
277500931021     C     TBLFLG        ANDNE     ' '
277600931021     C                   SETON                                        42
277700931021   X5C                   ELSE
277800931021     C                   SETOFF                                       90
277900931021    5C                   ENDIF
278000931021    4C                   ENDIF
278100931021    3C                   ENDIF
278200121221     C*
278300000622    2C                   ENDIF
278400121221      *-----
278500121221     c                   endIF
278600121221      *-----
278700931022     C   90              GOTO      ENDCT6
278800020315     C* CONTROLLO NUMERO STOP: NON DEVE ESSERE > O = 9000
278900020315      * attenzione 99999 ha un significato ben preciso come STOP in quanto significa
279000020315      *  consegnato a magazzino.
279100020315     C     VI6STP        IFGE      9000
279200020419     C     VI6STP        oreq      0
279300950711     C                   SETON                                        4890
279400950711     C                   GOTO      ENDCT6
279500950711     C                   END
279600011109     C* >>>>>>>>>
279700011112      *
279800011109     c     tass_boldet   tag
279900011112      *
280000011112      *  flag per determinare da dove è richiamata la tassazione bolle
280100011112      *   se con la "T" oppure con l'"F2" dalla windowsfl in tal caso
280200011112      *    deve preparare i campi della window a ricevere i valori dalla
280300011112      *     tassazione automatica.
280400011112     c                   movel     ' '           bol_tassa         1
280500011109     C* >>>>>>>>>
280600011107     c* D e t t a g l i o    V a l o r i    s u    W i n d o w S f l
280700011107    2c     vi6sce        ifeq      'D'
280800011107     c                   movel     ' '           vi6sce
280900011108     c                   exsr      WndwSfl
281000011107     c                   endif
281100011107     C* >>>>>>>>>
281200931019     C*
281300931123     C* T A S S A Z I O N E   S I N G O L A   B O L L A
281400931022    2C     VI6SCE        IFEQ      'T'
281500931123     C                   MOVEL     ' '           VI6SCE
281600011108      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
281700931029     C* SOLO SE UNO DEI 2 IMPORTI E' A 0(E NON INSERITO IN PREC VIDEO)
281800011107      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
281900011105     C*
282000011109    3C     VI6ITT        IFEQ      0
282100011109     C     *IN15         ANDEQ     *OFF
282200011109     C     VI6ITA        OREQ      0
282300011109     C     *IN16         ANDEQ     *OFF
282400931029     C*
282500020122     C                   CLEAR                   FICNB2
282600020426     C                   MOVEL     'CALCVAL'     §TApgm
282700020213     C                   MOVEL     'S'           §TAtsm
282800020326     C                   MOVEL     vi4pdr        §TAfgs
282900020326     C                   MOVEL     VI4PDR        §TAPDR
283000950117     C                   MOVEL     VH4TSR        §TATSR                         CONSEGNA/RITIRO
283100931029     C                   MOVEL     PARSML        §TASML
283200020326     C                   MOVEL     vh4DDC        §TADDC
283300020326     C                   z-add     vi4ndc        §tandc
283400931202     C                   Z-ADD     1             §TASPP
283500950118     C                   Z-ADD     VH6SET        §TASET
283600950118     C                   MOVE      'S'           §TAPAG
283700020129     C                   z-add     vh6aas        §taaas
283800020129     C                   z-add     ftdlnp        §talnp
283900020129     C                   z-add     ftdnrs        §tanrs
284000020129     C                   z-add     vi6nsp        §tansp
284100950118     C* Tot.colli per piking
284200950118     C     VH4TSR        IFEQ      'C'
284300950118     C* Spedizione da non pagare
284400950119     C     VH6FIA        IFEQ      'B'
284500950118     C                   MOVE      *ZEROS        §TANCL
284600950118     C                   MOVE      *BLANKS       §TAPAG
284700950118     C                   ELSE
284800950118     C                   Z-ADD     FTDNCL        §TANCL
284900950118     C                   END
285000950118     C                   Z-ADD     FTDNCL        §TACPE
285100950118     C                   ELSE
285200950118     C* Tot.colli per etichettatura
285300950118     C                   Z-ADD     FTDNCL        §TANCL
285400950118     C                   Z-ADD     VH6NCE        §TACPE
285500950118     C                   END
285600950127     C                   MOVEL     VI4PEP        §TAPPE
285700021211     C                   movel     vi4FCS        §taFCS
285800950116     C                   Z-ADD     FTDPKL        §TAKFA
285900950116     C                   Z-ADD     FTDVLU        §TATVL
286000931029     C*
286100950118     C                   Z-ADD     VH6IFP        §TAIFP
286200011107     c                   movel     vi6div        §tadiv
286300950118     C                   MOVEL     VI6TSP        §TATSP
286400030320     C                   MOVEL     Vh6cbo        §TAcbo
286500950116     C                   MOVEL     VI6FTC        §TATC1
286600950116     C                   MOVEL     VI6FT2        §TATC2
286700950131     C                   MOVEL     VI6CAP        §TACAP
286800950118     C                   MOVEL     FTDFIN        §TAFIN
286900950118     C                   MOVEL     VH6FGC        §TAFGC
287000950118     C                   MOVEL     VH6GVA        §TAGVA
287100950118     C                   MOVEL     VH6SIC        §TASIC
287200931029     C                   MOVEL     VI6CTR        §TACTR
287300931029    5C     *IN15         IFEQ      *ON
287400950118     C     VH6SIC        OREQ      'S'
287500931029     C                   MOVEL     *HIVAL        §TAITT
287600931029     C                   ELSE
287700011109     C                   Z-ADD     VI6ITT        §TAITT
287800931029    5C                   ENDIF
287900931029     C*
288000931029    5C     *IN16         IFEQ      *ON
288100950119     C     VH6FIA        OREQ      'B'
288200931029     C                   MOVEL     *HIVAL        §TAITA
288300931029     C                   ELSE
288400011109     C                   Z-ADD     VI6ITA        §TAITA
288500931029    5C                   ENDIF
288600021212      *
288700050909     C* Comunque aggancia sempre la bolla in partenza x vedere
288800050909      *  il codice trattamento merce
288900050909     C                   clear                   DS1B
289000091012     c     bolla_ftd     chain     fnblp01l
289100050909     c                   if        %found(fnblp01l)
289200050909     C                   MOVE      blpCbo        §tacbo
289300050909     C                   MOVEL     '1B'          COD
289400050909     C                   MOVEL     *BLANKS       KEY
289500050909     C                   MOVEL     blpCTM        KEY
289600050909     C     KTAB          CHAIN     TABEL00F                           30
289700050909     c                   if        %found(TABEL00F)
289800050909     C                   MOVEL     TBLUNI        DS1B
289900021212      * ------------
290000050909      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
290100050909      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
290200050909      *   la tariffa di Etichettatura in generale.
290300050909      *
290400050909      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
290500050909      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
290600050909     c                   iF        ftdTSR = 'R'
290700050909     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
290800050909     c                   move      'N'           §taPPE
290900050909     c                   end
291000050909      * Se è un DISKC Bartolini la regola dice che deve essere comunque
291100050909      * pagata la spedizione al Padroncino
291200050909     c                   iF        §1Bdkc ='B' and ftdnrs = 0
291300050909     c                   eval      §taPPE = 'S'
291400050909     c                   eval      §tadkc = 'S'
291500050909     c                   end
291600050909     c                   end
291700050909      **
291800050909     c                   endiF
291900050909     c                   endiF
292000021212      * ------------
292100950131     C*
292200931124     C     §TAITA        IFEQ      0
292300931124     C     VH4TSR        ANDEQ     'C'
292400950131     C                   Z-ADD     VH6CAS        §TACAS
292500950131     C                   MOVEL     VH6VCA        §TAVCA
292600950131     C                   Z-ADD     VH6AAS        FTDAAS
292700950131     C                   Z-ADD     VI6NSP        FTDNSP
292800020129     C*
292900950131     C* CONTROLLO SE E' C/A PRONTAMENTE RECUPERATO
293000931124     C                   EXSR      CTRASS
293100931124     C                   ENDIF
293200090909     C*
293300090909     C*   Deve passare il codice del cliente solo se è un cliente particolare
293400090909     c     vh6KSC        lookup    KcliPAR                                30
293500090909     c                   if        *in30
293600090909     C                   Z-ADD     vh6KSC        §TAcliPAR
293700090909     C                   end
293800931029     C*
293900020122     C                   CALL      'FICNB2R'
294000020122     C                   PARM                    FICNB2
294100931029     C*
294200011107     C* di ritorno dall'elaborazione:
294300011107     C*
294400931029    5C     §TAERR        IFEQ      ' '
294500011107     c                   z-add     0             vi6tot
294600931108     C     *IN15         IFEQ      *OFF
294700950118     C     VH6SIC        ANDNE     'S'
294800011112      *
294900931108     C                   MOVEL     §TAITT        VH6ITT
295000931108     C                   ADD       §TATPE        VH6ITT
295100931108     C                   ADD       §TATST        VH6ITT
295200011107      * >>>>>>> su campo SFLWDW 1° riga
295300011109     C                   Z-ADD     VH6ITT        VI6ITT
295400011107     c                   add       vh6itt        vi6tot
295500011112      *
295600931108     C                   ENDIF
295700011112      *
295800950118     C     *IN16         IFEQ      *OFF
295900950119     C     VH6FIA        ANDNE     'B'
296000011112      *
296100011112     C                   MOVEL     §TAITA        VH6ITA
296200011112     C                   MOVEL     §TAITA        VI6ITA
296300011112      * >>>>>>> su campo SFLWDW 2° riga
296400011112     c                   add       vh6ita        vi6tot
296500011112      *
296600950118     C                   END
296700011112      *
296800931029   X5C                   ELSE
296900931029     C* ERRORE IN TASSAZIONE
297000931019     C                   SETON                                        4190
297100931022     C                   GOTO      ENDCT6
297200931029    5C                   ENDIF
297300931029    3C                   ENDIF
297400931029    2C                   ENDIF
297500011109     c*>>>>>>>>>>>>>
297600011109     c* se richiesta a tassazione dal dettaglio nella windsfl
297700011109     c*  deve tornare sul dettaglio della windowsfl.
297800011109     c                   if        bol_tassa = 'D'
297900011109     c                   eval      vi6sce = 'D'
298000011109     c                   goto      tass_boldet
298100011109     c                   endif
298200011109     c*>>>>>>>>>>>>>
298300011109     C*
298400931123     C* SE  S O M M A   P E S I   A   G I O R N A T A
298500931123    2C     VH4TSM        IFEQ      'G'
298600011109     C     VI6ITT        ANDEQ     0
298700950119     C     VH6FIA        ANDNE     'B'
298800950118     C     VH6SIC        ANDNE     'S'
298900931123     C                   MOVEL     VI6CTR        WALF1
299000931123     C* NON ACCETTO PIU' DI 1 COD TAR A KILO O A QUINTALE
299100931123     C*  OPPURE DEVO VALORIZZARE SUBITO LA BOLLA
299200931123    3C     WALF1         IFEQ      '0'
299300931123     C     WALF1         OREQ      '3'
299400931123     C*
299500931123    4C     WALF3         IFEQ      *BLANKS
299600931123     C                   MOVEL     VI6CTR        WALF3
299700931123   X4C                   ELSE
299800931123    5C     VI6CTR        IFNE      WALF3
299900931123     C                   SETON                                        4390
300000931123     C                   GOTO      ENDCT6
300100931123    5C                   ENDIF
300200931123    4C                   ENDIF
300300931123     C*
300400931123    3C                   ENDIF
300500931123    2C                   ENDIF
300600931019     C*
300700931123     C* V I S U A L I Z Z A Z I O N E   S I N G O L A   B O L L A
300800931022    2C     VI6SCE        IFEQ      'V'
300900931123     C                   MOVEL     ' '           VI6SCE
301000931123    3C     VH4TSR        IFEQ      'C'                                           CONSEGNE
301100931021     C                   MOVEL     VH6AAS        PA2AAS
301200931021     C                   MOVEL     FTDLNP        PA2LNP
301300931021     C                   MOVEL     FTDNRS        PA2NRS
301400931207     C                   MOVEL     VI6NSP        PA2NSP
301500931105     C                   MOVEL     '1'           PA2FLG
301600100513     C                   MOVEL     KPJBU         kpjbus
301700100513     C                   clear                   kpjbu
301800931021     C                   MOVEL     PARAM2        KPJBU
301900950116     C                   CALL      'FNLR36R'
302000931021     C                   PARM                    KPJBA
302100100513     C                   MOVEL     KPJBUs        kpjbu
302200931021     C*
302300931021     C                   WRITE     FRB8D07
302400931022     C                   SETON                                        90
302500931123     C                   GOTO      ENDCT6
302600931123    3C                   ENDIF
302700020326     C*
302800020326    3C     VH4TSR        ifeq      'R'                                           ritiri nuovi
302900020326    3C     vh6nsp        andgt     0
303000020326     C                   CLEAR                   DSLS05
303100020326     C                   MOVE      VH6AAS        PA0AAS
303200020326     C                   MOVE      FTDLNP        PA0LNP
303300020326     C                   MOVE      FTDNRS        PA0NRS
303400020326     C                   MOVE      VI6NSP        PA0NSP
303500020326     C                   MOVEL     RAGUT         PA0RSU
303600020326     C                   MOVE      SIMFEL        PA0SIM
303700100513     C                   MOVEL     KPJBU         kpjbus
303800100513     C                   clear                   kpjbu
303900020326     C                   MOVEL     DSLS05        KPJBU
304000020326     C                   CALL      'FNLS05R'
304100020326     C                   PARM                    KPJBA
304200100513     C                   MOVEL     KPJBUs        kpjbu
304300020326     C*
304400020326     C                   WRITE     FRB8D07
304500020326     C                   SETON                                        90
304600020326     C                   GOTO      ENDCT6
304700020326    3C                   end
304800020326     C*
304900931123    2C                   ENDIF
305000931019     C*
305100931022     C     ENDCT6        TAG
305200931105     C* BLOCCATO
305300950119     C     VH6FIA        IFEQ      'B'
305400931105     C                   SETON                                        13
305500931105     C                   ELSE
305600931105     C                   SETOFF                                       13
305700931105     C                   ENDIF
305800950117     C* RITORNO ALL'INCASSO
305900950117     C     VH6SIC        IFEQ      'S'
306000950117     C                   SETON                                        23
306100950117     C                   ELSE
306200950117     C                   SETOFF                                       23
306300950117     C                   ENDIF
306400931105     C*
306500931019     C                   Z-ADD     NRR6          REC6
306600931019     C                   UPDATE    FRB8SF6
306700931019     C*
306800931019     C   90              SETON                                        32
306900931019     C  N90              READC     FRB8SF6                                32
307000931019    1C                   ENDDO
307100931203     C*
307200931203     C*  SE: 90 OFF - NO ERRORE
307300931203     C*             - SE TROVATO ALMENO UN CODICE TARIFFA 0XX O 3XX
307400931203     C*             - TIPO SOMMA PESI A GIORNATA
307500931203     C*             - NO VARIATO TUTTE LE RIGHE DELLA DISTINTA
307600931203    1C     *IN90         IFEQ      *OFF
307700931203     C     WALF3         ANDNE     *BLANKS
307800931203     C     VH4TSM        ANDEQ     'G'
307900931203     C     WREAD         ANDNE     WFTD
308000931203     C*
308100931203     C                   MOVEL     W01CTP        WALF1
308200931203    2C     WALF1         IFEQ      '0'
308300931203     C     WALF1         OREQ      '3'
308400931203     C*
308500931203    3C     W01CTP        IFNE      WALF3
308600931203     C                   SETON                                        4390
308700931203     C                   CLEAR                   WALF3
308800931203     C     1             CHAIN     FRB8SF6                            30
308900931203     C                   GOTO      ENDCT6
309000931203    3C                   ENDIF
309100931203    2C                   ENDIF
309200931203    1C                   ENDIF
309300931203     C*
309400931021     C                   SETOFF                                       36
309500931019     C*
309600931021     C                   ENDSR
309700020212     C*---------------------------------------------------------------*
309800011108     C* Emette la finestra con il dettaglio degli importi tariffe ----*
309900020212     C*---------------------------------------------------------------*
310000011108     c     WndwSfl       begsr
310100011108     c*
310200011108     c     *like         define    w6ctot        wwctot
310300011108     c*
310400011108     c                   seton                                        35
310500011108     c                   write     frb8wc6
310600011123     c                   setoff                                       3561
310700011108     c*
310800011108     c                   clear                   nrw6
310900011108     c                   z-add     1             wrc6
311000011108     c                   z-add     ftdlnp        w6clnp
311100011108     c                   z-add     ftdnrs        w6cnrs
311200011218     c                   z-add     vi6nsp        w6cnsp
311300011108     c                   move      vi6ftc        w6cftc
311400011108     c                   move      vi6ft2        w6cft2
311500011108     c                   move      vi6con        w6ccon
311600011108     c                   move      vi6ia         w6cin
311700011108     c                   move      vi6div        w6cdiv
311800011108     c                   move      vi6cap        w6ccap
311900011108     c                   move      ftdfin        w6cfin
312000011108     c                   z-add     ftdncl        w6cncl
312100011108     c                   z-add     ftdpkl        w6cpkl
312200011108     c                   z-add     ftdvlu        w6cvlu
312300011108     c                   move      vi6tot        w6ctot
312400020128     c                   z-add     vi6itt        w6cbas
312500020128     c                   z-add     vi6ita        w6cacc
312600011109     c*------->
312700020327     c* Carica le 2 righe x importo Base e Accessori
312800020327     c                   do        *hival
312900020327     c* protect/no display
313000020327     c                   setoff                                       60
313100020327     c* riga del sfl
313200020327     c                   add       1             nrw6
313300020327     c*  deve caricare solo 2 righe
313400020327     c                   if        nrw6 > 2
313500020327     c                   leave
313600020327     c                   endif
313700020327     c*
313800020327     c                   z-add     nrw6          w1htrc
313900020327     c*
314000020327     c                   select
314100020327     c     nrw6          wheneq    1
314200020327     c     vp6itt        comp      '1'                                    60
314300020327     c                   movel     *blank        w1tipo
314400020327     c                   movel     'Base'        w1tipo
314500020327     c                   z-add     vi6itt        w1valr
314600020327     c                   z-add     vi6itt        w1hval
314700020327     c*
314800020327     c     nrw6          wheneq    2
314900020327     c     vp6ita        comp      '1'                                    60
315000020327     c                   movel     *blank        w1tipo
315100020327     c                   movel     'Accessori'   w1tipo
315200020327     c                   z-add     vi6ita        w1valr
315300020327     c                   z-add     vi6ita        w1hval
315400020327     c                   endsl
315500020327     c*
315600020327     c                   write     frb8ws6
315700020327     c                   enddo
315800011109     c*-------> Emissione Window
315900011108     c     resta_wdw     tag
316000011109      *                *----------------------*
316100011108     c                   write     frb8wd6
316200011108     c                   exfmt     frb8wc6
316300011109      *                *----------------------*
316400011123     C                   setoff                                       61
316500011108     c*  Controlli
316600011109      * ---------         ----------
316700020327     c* Cntrl le righe x importi modificati
316800020327     c                   z-add     0             nrw6
316900020327     c                   z-add     0             wwctot
317000020327      *------------        ---------------
317100020327     c* cicla sulle righe
317200020327     c                   do        *hival
317300020327     c                   add       1             nrw6
317400020327     c*
317500020327     c*  deve caricare solo 2 righe
317600020327     c                   if        nrw6 > 2
317700020327     c                   leave
317800020327     c                   endif
317900020327     c*
318000020327     c     nrw6          chain     frb8ws6
318100020327      *  totalizza le righe
318200020327     c                   add       w1valr        wwctot
318300020327     c* protect/no display
318400020327     c                   setoff                                       60
318500020327      *  campi protetti
318600020327     c     nrw6          ifeq      1
318700020327     c     vp6itt        comp      '1'                                    60
318800020327     c                   end
318900020327     c     nrw6          ifeq      2
319000020327     c     vp6ita        comp      '1'                                    60
319100020327     c                   end
319200020327      *
319300020327      *  x F8 -> Conferma
319400020327      *  valori modificati manualmente
319500020327     c   KB
319600020327     cor KH              IF        w1hval <>  w1valr
319700020327      *
319800020327     c                   MOVEL     ' '           flgfi
319900020327    3c                   if        w1valr > 0
320000020327     c                   MOVEL     'M'           flgfi             1
320100020327    3c                   end
320200020327      *
320300020327     c                   Select
320400020327      * Base
320500020327     c     nrw6          wheneq    1
320600020327     C                   movel     flgfi         VH6FIT
320700020327     C                   MOVEL     w1valr        vi6itt
320800020327     C                   MOVEL     vi6itt        VH6ITT
320900020327      * Accessori
321000020327     c     nrw6          wheneq    2
321100020327     C                   movel     flgfi         VH6FIA
321200020327     C                   MOVEL     w1valr        vi6ita
321300020327     C                   MOVEL     vi6ita        VH6ITA
321400020327      *
321500020327     c                   endsl
321600020327      *
321700020327     C                   z-add     w1valr        w1hval
321800020327     c                   endif
321900020327      *
322000020327     C                   move      w1valr        trebytes          3 0
322100020327     C                   if        w6cdiv ='ITL' and
322200020327     c                             trebytes > 0
322300020327     c                   seton                                        61
322400020327     c                   end
322500020327      *
322600020327     c                   update    frb8ws6
322700020327     c   61              leave
322800020327     c                   enddo
322900020327      * ---------         ----------
323000020327     c* resta sulla window se non ha premuto dei cmd
323100020327     c     *inkh         ifeq      *off
323200020327     c     *inkl         andeq     *off
323300020327     c     *inkb         andeq     *off
323400020327     c     *in61         oreq      *on
323500020327     c     *inkl         andeq     *off
323600020327     c                   goto      resta_wdw
323700020327     c                   end
323800020327      *
323900020327      * reimposta il totale delle singole righe
324000020327     c     *inkh         ifeq      *on
324100020327     c                   if        wwctot <>  vi6tot
324200020327     c                   z-add     wwctot        vi6tot
324300020327     c                   endif
324400020327     c                   end
324500020327     c*
324600020327     c*  Se richiesta la tassazione bolla in automatico
324700020327     c*   come se avesse chiesto "T" sulla scelta del SFL principale
324800020327     c*    e successivamente "D" di dettaglio.
324900020327     c     *inkb         ifeq      *on
325000020327     c                   move      'T'           vi6sce
325100020327     c                   move      'D'           bol_tassa
325200020327     c                   end
325300020327     c*
325400011109     c*
325500011108     c                   endsr
325600020212     c*---------------------------------------------------------------*
325700011108     C* VEDO SE E' C/A PRONTAMENTE RECUPERATO ------------------------*
325800020212     C*---------------------------------------------------------------*
325900011108     C     CTRASS        BEGSR
326000011108     C*
326100950131     C                   MOVE      *ZEROS        §TACA1
326200950131     C                   MOVE      *BLANKS       WTACPC            1
326300931124     C* SOLO SE INCASSATO IL C/A
326400950131    1C     §TACAS        IFGT      0
326500931124     C*
326600931124     C                   CLEAR                   WNUM3
326700931124     C                   SETOFF                                       09
326800931124     C*
326900931124     C* PER ESEGUIRLO EVENTUALEMTE 2 VOLTE
327000931124    2C     WNUM3         DOWLE     2
327100931124     C                   ADD       1             WNUM3
327200931124     C*
327300931124    3C     *IN09         IFEQ      *OFF                                          CON FTD
327400091012     C     Bolla_FTD     SETLL     FNARBK00
327500091012     C     Bolla_FTD     READE     FNARBK00                               30
327600931124   X3C                   ELSE
327700950116     C     KLBL          SETLL     FNARBK00                                      CON MAMMA
327800950116     C     KLBL          READE     FNARBK00                               30
327900931124    3C                   ENDIF
328000931124     C*
328100931124    3C     *IN30         DOWEQ     *OFF
328200931124     C*
328300931124     C* SE INSERITO COMPLETAMENTE OK
328400030418    4C     kARBCVB       IFEQ      'K5'
328500950131     C                   Z-ADD     §TACAS        §TACA1
328600950131     C                   MOVEL     'S'           WTACPC
328700931124     C                   SETON                                        30
328800931124   X4C                   ELSE
328900931124     C*
329000931124     C* SE MODIFICATO IMPORTO L'ASSEGNO SOLTANTO SE > 0 E PER LA DIFFER
329100030418    5C     kARBCVB       IFEQ      'KP'
329200931124     C                   SETON                                        30
329300030418     C     §TACAS        SUB       kARBCAS       §TACA1
329400950131    6C     §TACA1        IFLE      0
329500950131     C                   Z-ADD     0             §TACA1
329600931124   X6C                   ELSE
329700950131     C                   MOVEL     'S'           WTACPC
329800931124    6C                   ENDIF
329900931124    5C                   ENDIF
330000931124    4C                   ENDIF
330100931124     C*
330200950116     C  N30
330300091012     CANN09Bolla_FTD     READE     FNARBK00                               30
330400950116     C  N30
330500950116     CAN 09KLBL          READE     FNARBK00                               30
330600931124    3C                   ENDDO
330700931124     C*
330800931124     C* SE NON TROVATO NULLA GUARDO SE HA UNA MAMMA E CERCO SULLA MAM
330900950131    3C     WTACPC        IFEQ      ' '
331000931124     C     WNUM3         ANDEQ     1
331100931124     C*
331200091012     C     Bolla_FTD     SETLL     FNLBL000                               30
331300931124    4C     *IN30         IFEQ      *OFF
331400030418     C* per uscire dal ciclo
331500950119     C                   Z-ADD     3             WNUM3             3 0
331600931124   X4C                   ELSE
331700931124     C                   ADD       1             WNUM3
331800091012     C     Bolla_FTD     CHAIN     FNLBL000                           30
331900030418     C* deve eseguire solo se le linee di arrivo sono uguali fra madre e figlia
332000030418     C*  altrimenti forza l'uscita dal ciclo
332100030418    4c  n30              if        lbllan <> lbllap
332200030418     c                   leave
332300030418     c                   end
332400931124     C                   SETON                                        09
332500931124    4C                   ENDIF
332600931124     C*
332700931124     C                   ELSE
332800030418     C* per uscire dal ciclo
332900931124     C                   Z-ADD     3             WNUM3
333000931124    3C                   ENDIF
333100931124    2C                   ENDDO
333200931124    1C                   ENDIF
333300931029     C                   ENDSR
333400020212     C*---------------------------------------------------------------*
333500950116     C* AGGIORNAMENTO BOLLE DISTINTA/RITIRO --------------------------*
333600020212     C*---------------------------------------------------------------*
333700931021     C     AGGIO6        BEGSR
333800931021     C                   Z-ADD     1             NRR6
333900931021     C                   READC     FRB8SF6                                32
334000931109     C*
334100931109     C* SE GIA' VALORIZZATE AGGANCIO LA TESTATA
334200931112     C     VI1FVL        IFEQ      'V'
334300931123     C* SE SOMMA PESI A GIORNATA DEVO RICONFERMARE LA VALUTAZIONE
334400931123     C  N32VH4TSM        IFEQ      'G'
334500931123     C                   SETON                                        22
334600931123     C                   ENDIF
334700011105     C     KFTD          CHAIN     FIFTT000                           30
334800931109     C                   ENDIF
334900931021     C*
335000931021    1C     *IN32         DOWEQ     *OFF
335100020313      *
335200020313      * per distinguere il vecchio modo di lettura dei ritiri non è più possibile
335300020313      * utilizzare l'ind.19 ma il campo numero spedizione.
335400020313     c                   if        vh6nsp>0
335500020315     c                   if        vh4tsr='R'
335600020315      * vista per i nuovi ritiri x spedizione
335700020404     C     KFTD5         CHAIN     fiftd005                           32
335800020315     c                   else
335900020315      * seleziona solo i consegnati
336000020313     C     KFTD2         CHAIN     fiftd002                           30
336100020315     c                   end
336200020313     c                   else
336300020313     C     KFTD4         CHAIN     fiftd004                           30
336400020313     c                   end
336500931112     C*
336600020313    2C     *IN30         IFEQ      *OFF
336700931109     C*
336800931109     C* SE GIA' VALORIZZATE DEVO AGGIORNARE I CAMPI DI TOTALE
336900931109     C     VI1FVL        IFEQ      'V'
337000950118     C  N15VH6SIC        IFNE      'S'
337100931109     C                   SUB       FTDITT        FTTITT
337200011109     C                   ADD       VI6ITT        FTTITT
337300931110     C                   SETON                                        22
337400931109     C                   ENDIF
337500931110     C*
337600950119     C  N16VH6FIA        IFNE      'B'
337700931109     C                   SUB       FTDITA        FTTITA
337800011109     C                   ADD       VI6ITA        FTTITA
337900950118     C                   SETON                                        22
338000931109     C                   ENDIF
338100931109     C                   ENDIF
338200931109     C*
338300931029     C* SE IMPOSTATO VALORE DISTINTA BASE AZZERO SULLA BOLLA
338400931029     C     *IN15         IFEQ      *OFF
338500011109     C                   MOVEL     VI6ITT        FTDITT
338600931021     C                   MOVEL     VH6FIT        FTDFIT
338700931029     C                   ENDIF
338800931029     C* SE IMPOSTATO VALORE DISTINTA ACCESSORI AZZERO SULLA BOLLA
338900931029     C     *IN16         IFEQ      *OFF
339000011109     C                   MOVEL     VI6ITA        FTDITA
339100931021     C                   MOVEL     VH6FIA        FTDFIA
339200931029     C                   ENDIF
339300011030     C     VI6CTR        IFNE      VI4CTD
339400011106     c     vh4tpr        andeq     1
339500931022     C                   MOVEL     VI6CTR        FTDCTP
339600931109     C                   ENDIF
339700011123     C                   z-add     VI6STP        FTDSTP                          STOP
339800020326     C*
339900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
340000020326     C* i primi tre caratteri
340100020326     c                   if        ftdfgs = 0
340200020326     c                   movel     ftdpdr        ftdfgs
340300020326     c                   end
340400020326     C*
340500020313     c                   if        vh6nsp > 0
340600020315     c                   if        vh4tsr='R'
340700020315     C                   UPDATE    fiftd005
340800020315     c                   else
340900020313     C                   UPDATE    fiftd002
341000020315     c                   end
341100020313     c                   else
341200020313     C                   UPDATE    fiftd004
341300020313     c                   end
341400020313     C*
341500931021    2C                   ENDIF
341600931021     C                   READC     FRB8SF6                                32
341700931021    1C                   ENDDO
341800931021     C*
341900931123     C* RIAGGIORNO LA TESTATA (PER EVITARE CHE NON SIA AGGIORNATA SE
342000931109     C*  ESCONO CON CMD12)
342100931109     C     VI1FVL        IFEQ      'V'
342200020326     C*
342300020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
342400020326     C* i primi tre caratteri
342500020326     c                   if        fttfgs = 0
342600020326     c                   movel     fttpdr        fttfgs
342700020326     c                   end
342800020326     C*
342900011105     C                   UPDATE    FIFTT000
343000931109     C                   ENDIF
343100931109     C*
343200931021     C                   ENDSR
343300020212     C*---------------------------------------------------------------*
343400931019     C* CONTROLLO CODICE TARIFFA -------------------------------------*
343500020212     C*---------------------------------------------------------------*
343600931019     C     CTRTAR        BEGSR
343700091112     C*
343800931123     C                   Z-ADD     1             C
343900931123     C     VH4TSR        LOOKUP    TSR(C)                                 30
344000931123     C  N30              Z-ADD     1             C
344100931123     C*
344200931123     C* SE E' IL CODICE TARIFFA GENERICO --> ERRORE
344300931123    1C     W01CTR        IFEQ      CTS(C)
344400931123     C                   SETON                                        90
344500931123   X1C                   ELSE
344600931123     C*
344700011112     C     KFGT2         CHAIN     FIFGT000                           30
344800011112     C*
344900011112     C* Modificato con le nuove regole di data scadenza e valuta
345000011112     C*  il modo di reperire il record valido.
345100011112    2C     *in30         doweq     *off
345200091112      *
345300011112     C     fgtatb        ifne      'A'
345400011112     C     w02div        andeq     fgtdiv
345500091112      *
345600091112     C* Se tariffa del periodo
345700091112     C     Ktgt          CHAIN     FItgt01l
345800091112 2/3 c                   if        %Found(FItgt01l) and
345900091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
346000091112      *
346100091112    3c     parsml        IFeq      *blank
346200091112     C     tgtDTS        aNDgt     0
346300091112     c     parsml        OReq      'S'
346400091112     C                   leave
346500091112    3C                   end
346600091112      *
346700091112 2/3 C                   end
346800091112      *
346900091112    2C                   end
347000091112      *
347100011112     C     kfgt2         reade     fifgt000                               30
347200011112    2C                   enddo
347300931019     C*
347400091112     C*  ????????????  strano test se trovato ritesta la data se minore della decorrenza ?????
347500931123    2C     *IN30         IFEQ      *ON
347600931019     C     *IN30         OREQ      *OFF
347700091112     C     W02DDC        ANDLT     tgtDDT
347800931019     C                   SETON                                        90
347900931123    2C                   ENDIF
348000091112     C*
348100931123    1C                   ENDIF
348200931021     C*
348300931021     C* 90 ON - TARIFFA INESISTENTE
348400931028     C*         VEDO SE HA ALMENO UN CODICE TARIFFA VALIDO
348500931021    1C     *IN90         IFEQ      *ON
348600931108     C* CERCO IL CODICE TARIFFA GENERICO DEL PRESTAZ DA ESCLUDERE
348700931028     C                   SETOFF                                       31
348800931028     C*
348900011112     C     KFGT          CHAIN     FIFGT000                           30
349000091112      *
349100091112     C*   in testata di periodo
349200091112     C  n30Ktgt          CHAIN     FItgt01l
349300091112 1/2 c  n30              if        %Found(FItgt01l)
349400931028     C*
349500931028     C* SE ANNULLATO O E' IL CODICE TARIFFA GENERICO RILEGGO
349600011112      * o se non è della stessa divisa o se scaduto
349700931022    2C  N30FGTATB        IFEQ      'A'
349800931028     C     FGTCTR        OREQ      CTS(C)
349900011112     C     fgtdiv        ORne      w02div
350000091112     C     tgtdst        ORgt      w02ddc
350100091112     C     tgtDTS        oreq      0
350200091112     c     parsml        andeq     *blank
350300931021     C                   SETON                                        31
350400931028    2C                   ENDIF
350500091112 1/2 c                   end
350600931021     C*
350700931021    2C     *IN30         DOWEQ     *OFF
350800931021     C     *IN31         ANDEQ     *ON
350900931021     C*
351000011112     C     KFGT          READE     FIFGT000                               30
351100091112      *
351200091112     C*   in testata di periodo
351300091112     C  n30Ktgt          CHAIN     FItgt01l
351400091112 2/3 c  n30              if        %Found(FItgt01l)
351500091112     C*
351600931022    3C  N30FGTATB        IFEQ      'A'
351700931028     C     FGTCTR        OREQ      CTS(C)
351800011112     C     fgtdiv        ORne      w02div
351900091112     C     tgtdst        ORgt      w02ddc
352000091112    3C     tgtDTS        oreq      0
352100020315    3c     parsml        andeq     *blank
352200931021     C                   SETON                                        31
352300931028     C                   ELSE
352400931028     C                   SETOFF                                       31
352500931021    3C                   ENDIF
352600931021     C*
352700091112 2/3 c                   end
352800091112     C*
352900931021    2C                   ENDDO
353000931021    1C                   ENDIF
353100931028     C*
353200931028     C* 90 ON 30 ON - NON ESISTONO TARIFFE PER IL CLIENTE
353300931021     C*
353400931019     C                   ENDSR
353500020212     C*---------------------------------------------------------------*
353600931111     C* CONTROLLO SE MANCATA CONSEGNA DA TASSARE ---------------------*
353700020212     C*---------------------------------------------------------------*
353800931111     C     CTRCMC        BEGSR
353900950116     C                   MOVE      VI4FIL        WALF6
354000931111     C     WALF6         LOOKUP    CMC                                    30
354100931111     C* SE INESISTENTE CERCO CON FILIALE A 0
354200931111     C     *IN30         IFEQ      *OFF
354300931111     C                   MOVE      000           WALF6
354400931111     C     WALF6         LOOKUP    CMC                                    30
354500931111     C                   ENDIF
354600931111     C                   ENDSR
354700020212     C*---------------------------------------------------------------*
354800931111     C* CONTROLLO CODICE BOLLA DA NON TASSARE ------------------------*
354900020212     C*---------------------------------------------------------------*
355000931111     C     CTRCBL        BEGSR
355100950116     C                   MOVEL     W018C         WALF6             6
355200950116     C                   MOVE      VI4FIL        WALF6
355300950116     C     WALF6         LOOKUP    CBL                                    31
355400931111     C* SE INESISTENTE CERCO CON FILIALE A 0
355500931111     C     *IN31         IFEQ      *OFF
355600950116     C                   MOVE      000           WALF6
355700950116     C     WALF6         LOOKUP    CBL                                    31
355800931111     C                   ENDIF
355900931111     C                   ENDSR
356000020212     C*---------------------------------------------------------------*
356100931021     C* ESEGUO VALORIZZAZIONE ----------------------------------------*
356200020212     C*---------------------------------------------------------------*
356300931021     C     VALZZ         BEGSR
356400931110     C* SPENGO IL 22 CHE OBBLIGA IL CMD6
356500931110     C                   SETOFF                                       22
356600931021     C                   Z-ADD     1             NRR4
356700931021    0C     NRR4          DOWLE     W04NRR
356800931021     C     NRR4          CHAIN     FRB8SF4                            32
356900931021     C*
357000931021    1C     *IN32         IFEQ      *OFF
357100931021     C*
357200931123     C* R E C O R D   D E L   T I P O   P R E S T A Z I O N E:
357300931029     C* ESCLUDO (MEMORIZZO IL COD.TARIFFA)
357400931021    2C     VH4NRR        IFEQ      0
357500011106     c                   if        vh4tpr = 1
357600011030     C                   MOVEL     VI4CTD        WALF3
357700011105     c                   end
357800000623     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
357900000623     C     VH4CTP        IFNE      *BLANKS
358000000623     C     VH4CTB        ANDNE     *BLANKS
358100000623     C                   MOVEL     VH4CTP        WCTP              3
358200000623     C                   MOVEL     VH4CTB        WCTB              3
358300000623     C                   ELSE
358400000623     C                   MOVEL     *BLANKS       WCTP
358500000623     C                   MOVEL     *BLANKS       WCTB
358600000623     C                   ENDIF
358700931029     C*
358800931021   X2C                   ELSE
358900931022     C*
359000931029     C* V A L O R I   B A S E   E   A C C E S S O R I   D I S T I N TA:
359100931029     C*  SE CI SONO ENTRAMBI NON FACCIO NULLA
359200931022    3C     VI4ITT        IFEQ      0
359300931022     C     VI4ITA        OREQ      0
359400931103     C*
359500931103     C* 15 ON - IMMESSA TARIFFA BASE
359600931103     C* 16 ON - IMMESSA TARIFFA ACCESSORI
359700931103     C     VI4ITT        COMP      0                                  15
359800931103     C     VI4ITA        COMP      0                                  16
359900931105     C* PULIZIA CAMPI DI TOTALE
360000931125     C                   CLEAR                   W01CLA
360100020212     C                   CLEAR                   g01CLA
360200950118     C                   CLEAR                   W01CPE
360300020212     C                   CLEAR                   g01CPE
360400931125     C                   CLEAR                   W01KFA
360500020212     C                   CLEAR                   g01KFA
360600931105     C                   CLEAR                   W01NCL
360700931105     C                   CLEAR                   W01TVL
360800020212     C                   CLEAR                   g01TVL
360900931105     C                   CLEAR                   W01SNA
361000020212     C                   CLEAR                   g01SNA
361100931105     C                   CLEAR                   W01STP
361200931105     C                   CLEAR                   W02STP
361300931123     C                   CLEAR                   W01CTP
361400931105     C                   CLEAR                   W01FIA
361500931105     C                   CLEAR                   W01FIT
361600931105     C                   CLEAR                   W01ITT
361700931105     C                   CLEAR                   W01ITA
361800950119     C                   CLEAR                   W01SNE
361900931202     C                   CLEAR                   W01SNP
362000931103     C*
362100931103     C* C O D I C E   T A R I F F A   I N E S I S T E N T E :
362200931103    4C     WALF3         IFEQ      '???'
362300931105     C*  VEDO SE DARE MANCA TARIFFA SOLO SE NON E' IMPOSTATA LA BASE
362400931105     C     *IN15         IFEQ      *OFF
362500931104     C                   MOVEL     SER(2)        W01ERR           25
362600931103     C                   EXSR      MANTAR
362700931105     C                   ENDIF
362800931103     C*
362900931103   X4C                   ELSE
363000931029     C*
363100931108     C     VH4TSR        IFEQ      'R'                                           RITIRI
363200931108     C                   SETON                                        19
363300931108     C                   ELSE
363400931108     C                   SETOFF                                       19         CONSEGNE
363500931108     C                   ENDIF
363600020325     C*
363700020325     C*  prima del ciclo di lettura di dettaglio resetta
363800020325     C*   i flags o i contatori
363900020325     c                   move      *blank        w01isola
364000020325     c                   move      *blank        w01xpres
364100030109     c                   move      *blank        w01disagiato      1
364200020325     c                   z-add     0             almuno_xpr
364300020325     c                   z-add     0             almuno_iso
364400030109     c                   z-add     0             almuno_disag
364500030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
364600030821     C*  lo stop.
364700030821     c                   z-add     0             W01nss
364800030821     c                   z-add     0             W01fw
364900030821     c                   z-add     0             W01rcc
365000030821     c                   move      *loval        savSTP
365100041124     C*
365200041124     C*   Comunque prima di valorizzare per sicurezza pulisce eventuali
365300041124     C*   records del FIFCE      *-------------*
365400041124     c                   exsr      Annulla_FCE
365500041124     C*                          *-------------*
365600931108     C*
365700011105     C     KFTD          SETLL     fiftd001
365800011105     C     KFTD          READE     fiftd001                               32
365900931021     C*
366000931104    5C     *IN32         DOWEQ     *OFF
366100020328     C*
366200030821     C*  a rottura di STOP conta quante spedizioni ci sono
366300030821     C*   all'interno dello stesso STOP
366400030821     c                   if        ftdSTP <> savSTP
366500030821     C*                          *-------------*
366600030821     c                   exsr      Sped_x_Stop
366700030821     C*                          *-------------*
366800030821     c                   eval         savSTP = ftdSTP
366900030821     c                   end
367000030821     C*
367100020422     C*               *--------------------*
367200950118     C                   EXSR      CTRTSM
367300020422     C*               *--------------------*
367400020328     C*
367500020328     C* scrive record x record sul workF
367600020423     C*  soo se hanno lo stop e se hanno un peso.
367700020620     c                   if        ftdstp <>0  and
367800020620     c                             ftdpkl <>0  and
367900020422    3C                             parsml = ' '
368000020328     C                   EXSR      wrt_FTDW
368100020419     c                   end
368200931021     C*
368300011105     C     KFTD          READE     fiftd001                               32
368400931104    5C                   ENDDO
368500931102     C*
368600931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA SOMMARE PER "G"
368700931102     C*                                 O DA TASSARE PER "T"
368800931102     C                   EXSR      CTRSTP
368900931103    4C                   ENDIF
369000931022     C*
369100950116     C* BASE  GIA' VALORIZZATA
369200931103    4C     *IN15         IFEQ      *ON
369300931105     C                   MOVEL     'M'           W01FIT
369400931105     C                   Z-ADD     VI4ITT        W01ITT
369500931103    4C                   ENDIF
369600950116     C* ACCESSORI  GIA' VALORIZZATI
369700931103    4C     *IN16         IFEQ      *ON
369800931105     C                   MOVEL     'M'           W01FIA
369900931105     C                   Z-ADD     VI4ITA        W01ITA
370000931103    4C                   ENDIF
370100931021     C*
370200931103   X3C                   ELSE
370300931022     C*
370400931022     C* BASE E ACCESSORI GIA' VALORIZZATI
370500931022     C                   Z-ADD     VI4ITT        W01ITT
370600931022     C                   Z-ADD     VI4ITA        W01ITA
370700931021     C                   MOVEL     'M'           W01FIT
370800931021     C                   MOVEL     'M'           W01FIA
370900931022     C*
371000931022     C* ANNULLO TUTTE LE EVENTUALI MEMORIZZAZIONI SULLE BOLLE
371100931022     C                   SETON                                        14
371200931022     C                   EXSR      ANNSPE
371300931022     C                   SETOFF                                       14
371400931022     C*
371500931021    3C                   ENDIF
371600931021     C* AGGIORNO TESTATA
371700011105     C     KFTD          CHAIN     FIFTT000                           30
371800020212     C                   z-add     g01CLA        FTTCLA
371900020212     C                   z-add     g01CPE        FTTCPE
372000020212     C                   z-add     g01KFA        FTTKFA
372100020212     C                   z-add     g01TVL        FTTTVL
372200020212     C                   z-add     g01SNA        FTTSNA
372300931022     C*
372400931022     C* SE TROVATI ERRORI IMPOSTO IL FLAG DI PRESENZA MANCA TARIFFA
372500931103    3C     VI4SCE        IFEQ      'E'
372600931022     C                   MOVEL     'S'           FTTFLA
372700931103   X3C                   ELSE
372800931022     C                   MOVEL     ' '           FTTFLA
372900931103    3C                   ENDIF
373000931022     C                   MOVEL     W01ITT        FTTITT
373100931022     C                   MOVEL     W01ITA        FTTITA
373200931021     C                   MOVEL     W01FIT        FTTFIT
373300931021     C                   MOVEL     W01FIA        FTTFIA
373400931202     C                   MOVEL     W01SNP        FTTSNP
373500950119     C                   MOVE      W01SNE        FTTSNE
373600950127     C                   MOVEL     VI4PEP        FTTPEP
373700021211     C                   movel     vi4FCS        fttLVL
373800931021     C                   MOVEL     'V'           FTTFVL
373900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
374000020326     C* i primi tre caratteri
374100020326     c                   if        fttfgs = 0
374200020326     c                   movel     fttpdr        fttfgs
374300020326     c                   end
374400020326     C*
374500011105     C  N30              UPDATE    FIFTT000
374600020423     c*
374700020423     c* pulisce eventuali records del file di work rimasti in sospeso
374800020423     c*               *-----------------------*
374900020423     c  N30              exsr      del_FTDW0
375000020423     c*               *-----------------------*
375100931021    2C                   ENDIF
375200931021    1C                   ENDIF
375300931021     C*
375400931021     C                   ADD       1             NRR4
375500931103    0C                   ENDDO
375600931110     C* IMPOSTO IL FLAG DI VALORIZZATE
375700931110     C     *IN90         IFEQ      *ON
375800931110     C                   MOVEL     'V'           VI1FVL
375900931110     C                   MOVEL     CVALOR        VI1VAL
376000931110     C                   ENDIF
376100931021     C                   ENDSR
376200030821     C*---------------------------------------------------------------*
376300041124     C*  Ciclo preventivo di annullamento FIFCE se per problemi era
376400041124     C*  rimasto sporco. --> Non deve esserci nulla prima della Valorizzazione
376500030821     C*---------------------------------------------------------------*
376600041124     C     Annulla_FCE   BEGSR
376700041124      *
376800041124     c                   movel     vi4pdr        FGS_vi4pdr        3 0
376900041124      *          *-------------------------------------*
377000041124     c     parsml        ifeq      ' '
377100041124     c     Del_FCE       setll     fifce01l
377200041124     c     Del_FCE       reade     fifce01l
377300041124     c                   dow       not %Eof(fifce01l)
377400041124     c                   delete    fifce000
377500041124     c     Del_FCE       reade     fifce01l
377600041124     c                   enddo
377700041124     c                   end
377800041124      *          *-------------------------------------*
377900041124      *
378000041124     C                   ENDSR
378100041124     C*---------------------------------------------------------------*
378200041124     C* Conta quante Spedizioni ci sono all'interno dello Stesso Stop
378300041124     C*---------------------------------------------------------------*
378400041124     C     Sped_x_Stop   BEGSR
378500030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
378600030821     C*  lo stop.
378700030821     c                   z-add     0             W01nss
378800030821     c                   z-add     0             W01fw
378900030821     c                   z-add     0             W01rcc
379000090911     c                   clear                   W01_TarPartCLI    1
379100090911     c                   clear                   W01_UnoOpiuCLI    7 0
379200030821      *
379300030821     C*  pgm x contare le spedizioni in STOP
379400030821     c                   clear                   FICNB1
379500030821     C                   eval      §t1PDR  =  ftdPDR
379600030821     C                   eval      §t1TSR  =  ftdTSR
379700030821     C                   eval      §t1NDC  =  ftdNDC
379800030821     C                   eval      §t1DDC  =  ftdDDC
379900030821     C                   eval      §t1STP  =  ftdSTP
380000030821     C*
380100030821     C                   CALL      'FICNB1R1'
380200030821     C                   PARM                    ficnb1
380300030821     C* Nr.Sped.totali in STOP
380400030821     c                   z-add     §T1nss        W01nss
380500030821     c                   z-add     §T1fw         W01fw
380600030821     c                   z-add     §T1rcc        W01rcc
380700090911     c                   eval       W01_TarPartCLI = §T1PARTIC
380800090911     c                   eval       W01_UnoOpiuCLI = §T1CLIPAR
380900030821      *
381000030821     C                   ENDSR
381100020212     C*---------------------------------------------------------------*
381200020328     C* Scrittura del file di work record x record
381300020212     C*---------------------------------------------------------------*
381400020328     C     wrt_FTDW      BEGSR
381500020328      *
381600020328     C                   z-add     ftdfgs        ftwfgs
381700020328     C                   z-add     ftdpdr        ftwpdr
381800020328     C                   move      ftdtsr        ftwtsr
381900020328     C                   z-add     ftdndc        ftwndc
382000020328     C                   z-add     ftdddc        ftwddc
382100020328     C                   z-add     ftdstp        ftwstp
382200020328     C                   z-add     ftdaas        ftwaas
382300020328     C                   z-add     ftdlnp        ftwlnp
382400020328     C                   z-add     ftdnrs        ftwnrs
382500020328     C                   z-add     ftdnsp        ftwnsp
382600020328     C                   z-add     ftdpkl        ftwpkl
382700020328     C                   z-add     ftdvlu        ftwvlu
382800020328     c                   write     fiftdw00
382900020328      *
383000020328     C                   ENDSR
383100020328     C*---------------------------------------------------------------*
383200020328     C* Cancella i record del work dopo aver eseguito lo stop
383300020328     C*---------------------------------------------------------------*
383400020328     C     del_FTDW      BEGSR
383500020328      *
383600020328     C                   z-add     ftdfgs        kftwfgs
383700020328     C                   z-add     ftdpdr        kftwpdr
383800020328     C                   move      ftdtsr        kftwtsr
383900020328     C                   z-add     ftdndc        kftwndc
384000020328     C                   z-add     ftdddc        kftwddc
384100020328     C                   z-add     ftdstp        kftwstp
384200020328      *
384300020328     c     kftdw         setll     fiftdw1l
384400020328     c     kftdw         reade     fiftdw1l
384500020328     c                   dow       not %eof(fiftdw1l)
384600020328     c                   delete    fiftdw00
384700020328     c     kftdw         reade     fiftdw1l
384800020328     C                   ENDdo
384900020328      *
385000020328     C                   ENDSR
385100020422     C*---------------------------------------------------------------*
385200020422     C* Cancella i record del work dopo aver eseguito lo stop
385300020422     C*---------------------------------------------------------------*
385400020422     C     del_FTDW0     BEGSR
385500020422      *
385600020423     C                   z-add     fttfgs        kftwfgs
385700020423     C                   z-add     fttpdr        kftwpdr
385800020423     C                   move      ftttsr        kftwtsr
385900020423     C                   z-add     fttndc        kftwndc
386000020423     C                   z-add     fttddc        kftwddc
386100020423      *
386200020422     c     kftdw0        setll     fiftdw1l
386300020422     c     kftdw0        reade     fiftdw1l
386400020422     c                   dow       not %eof(fiftdw1l)
386500020422     c                   delete    fiftdw00
386600020422     c     kftdw0        reade     fiftdw1l
386700020422     C                   ENDdo
386800020422      *
386900020422     C                   ENDSR
387000020328     C*---------------------------------------------------------------*
387100020328     C* CONTROLLO LA SOMMA PESI -------------------------------------*
387200020328     C*---------------------------------------------------------------*
387300020328     C     CTRTSM        BEGSR
387400931029     C                   SETOFF                                       17
387500020122     C                   CLEAR                   FICNB2
387600000626     C* VERIFICO SE BOLLA POSTA OPPURE NO
387700000626     C* CONTROLLO IN AZORG SE PO POSTE
387800000626     C                   EXSR      CTRORG
387900931123     C* MEMORIZZO CODICE TARIFFA
388000931123    1C     FTDCTP        IFEQ      *BLANKS
388100000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
388200000623    2C     WCTP          IFEQ      *BLANKS
388300000623     C     WCTB          ANDEQ     *BLANKS
388400931123     C                   MOVEL     WALF3         FTDCTP
388500000623     C                   ELSE
388600000623     C*  P.O. POSTE
388700020729     C     §OgNTW        IFEQ      'PPT'
388800000623     C                   MOVEL     WCTP          FTDCTP
388900000623     C                   ELSE
389000000623     C                   MOVEL     WCTB          FTDCTP
389100000623    3C                   ENDIF
389200000623     C*
389300000623    2C                   ENDIF
389400000623     C*
389500931123    1C                   ENDIF
389600931029     C*
389700950118     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
389800950118     C                   SETON                                        30
389900950118    6C     *IN19         IFEQ      *OFF
390000950118     C* T I P O   B O L L A   D A   T A S S A R E
390100950224     C                   CLEAR                   ARBCBO
390200091012     C     Bolla_FTD     CHAIN     FNARB01L                           32
390300950118     C                   EXSR      CTRCBL
390400030109      *
390500950118     C   31              SETOFF                                       30
390600950118     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
390700950118     C*  (SE NON CONSEGNATA)
390800950118    7C   30FTDDCM        IFEQ      0
390900950118     C*
391000950118    8C     FTDCMC        IFNE      *BLANKS
391100950118     C                   MOVEL     FTDCMC        WALF6
391200950118   X8C                   ELSE
391300950118     C                   MOVEL     FTDTMC        WALF6
391400950118    8C                   ENDIF
391500950118     C                   EXSR      CTRCMC
391600950118    7C                   ENDIF
391700950118    6C                   ENDIF
391800000626     C*  P.O. POSTE
391900050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
392000020729     C     §OgNTW        IFEQ      'PPT'
392100050426     C     vh4tpo        andeq     'S'
392200000626     C                   SETOFF                                       30
392300130305     C*
392400000626     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE SIGNIFICA CHE LA BOLLA
392500000626     C* SI DEVE PAGARE COME CONSEGNATA
392600000626     C                   MOVEL     '2Z'          COD
392700000626     C                   MOVEL     *BLANKS       KEY
392800000626     C                   MOVEL     FTDCMC        KEY
392900000626     C     KTAB          CHAIN     TABEL                              30
393000000626     C  N30              MOVEL     TBLUNI        DS2Z
393100000626     C   30              MOVEL     *BLANKS       DS2Z
393200000626     C     §2ZTPP        IFNE      *BLANKS
393300000626     C* LA BOLLA VIENE PAGATA
393400000626     C                   SETON                                        30
393500000626     C                   ENDIF
393600000626     C                   MOVEL     §2ZTPP        FTDFGC
393700000626    3C                   ENDIF
393800950118     C*
393900950118    6C     *IN30         IFEQ      *ON
394000950119     C                   MOVEL     'S'           W01PAG            1
394100950118     C                   ELSE
394200950119     C                   ADD       1             W01SNP                          no pagare
394300950118     C                   MOVEL     ' '           W01PAG
394400950118    6C                   ENDIF
394500950119     C*
394600950119     C     *IN19         IFEQ      *OFF
394700950119     C     W01PAG        ANDEQ     *BLANKS
394800950119     C     FTDDCM        ANDEQ     *ZEROS
394900950119     C                   ADD       1             W01SNE                          no cons.no pag
395000950119     C                   END
395100950118     C*
395200950118     C     W01PAG        IFEQ      'S'
395300950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
395400950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
395500931029     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
395600950123     C                   MOVE      'S'           §TAPAG
395700950118     C                   ELSE
395800950118     C                   MOVE      *ZEROS        §TAKFA                          PESO
395900950118     C                   MOVE      *ZEROS        §TATVL                          VOLUME
396000950118     C                   MOVE      *ZEROS        §TANCL                          COLLI
396100950123     C                   MOVE      ' '           §TAPAG
396200950118     C                   END
396300950118     C*
396400950118     C     *IN19         IFEQ      *ON
396500950118     C                   Z-ADD     FTDNCE        §TACPE                          COLLI ETICH
396600950118     C                   ELSE
396700950118     C                   Z-ADD     FTDNCL        §TACPE                          COLLI PIK.
396800950118     C                   END
396900950118     C*
397000950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
397100021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
397200950118     C*
397300950130     C                   Z-ADD     FTDSET        §TASET
397400931029     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
397500931029     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
397600020325     C*
397700020325     C* memorizza se c'è anteporto per Isola
397800020325     C*  ma attenzione,se almeno uno all'interno dello stesso STOP
397900020325     C* è diverso da isola, resetta il flag da passare al pgm di calcolo x stop
398000020325     C*
398100020325     C* Se almeno una delle spedizioni all'interno dello stop non è
398200020325     C*  un'isola ...lo memorizza.
398300020325     c                   if        almuno_iso = 1 and ftdFIN <> 'I'             Anteporto
398400020325     C                   move      *blank        w01isola                       se Isola
398500020325     c                   end
398600020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
398700020325     C*   all'interno dello stesso stop non sia gestita ad Isola che deve
398800020325     C*    segnalarlo.
398900020325     c                   if        almuno_iso = 0
399000020325     c                   z-add     1             almuno_iso        1 0
399100020325     C                   move      *blank        w01isola                       Anteporto
399200020325     c                   if        ftdFIN = 'I'                                 per indicare
399300020325     C                   move      'S'           w01isola                       se Isola
399400020325     c                   end
399500020325     c                   end
399600030109     C*
399700931111     C* SOMMA PESI A GIORNATA O A STOP:
399800931111     C* ELABORO COMUNQUE GLI ACCESSORI BOLLA PER BOLLA
399900931111     C*
400000020325      *  *-----------------------*
400100931111     C***   S T O P
400200020325      *  *-----------------------*
400300931111     C* SE NON C'E' IMPORTO MANUALE E NON E' VALORIZZATA BASE DISTINTA
400400931116     C* E NON E' SOLO UN RITORNO ALL'INCASSO
400500950126     C* E BASE SPEDIZIONE NON VALORIZZATA
400600931111     C*  ENTRO NEL CALCOLO DELLO "STOP"
400700931111    2C     FTDFIT        IFNE      'M'
400800931111     C     *IN15         ANDEQ     *OFF
400900931116     C     FTDSIC        ANDNE     'S'
401000960905    3C     FTDITT        IFEQ      *ZEROS
401100931111     C                   EXSR      CALSTP
401200960905   X3C                   ELSE
401300960906     C     FTDSTP        IFGT      *ZEROS
401400960906    4C     FTDSTP        ANDEQ     W01STP
401500960906     C                   CLEAR                   W01STP
401600960905     C                   CLEAR                   W01KFA
401700960905     C                   CLEAR                   W01NCL
401800960905     C                   CLEAR                   W01TVL
401900960905     C                   CLEAR                   W01CPE
402000960905    4C                   END
402100960905     C* SE ANCHE ACCESSORI PIENI NON TASSO
402200960905    4C     FTDITA        IFGT      0
402300960905     C     *IN16         OREQ      *ON
402400960905     C                   SETON                                        17
402500960905    4C                   ENDIF
402600960905    3C                   END
402700931111   X2C                   ELSE
402800931111     C* SE ANCHE ACCESSORI PIENI NON TASSO
402900931111    3C     FTDITA        IFGT      0
403000931111     C     *IN16         OREQ      *ON
403100931111     C                   SETON                                        17
403200931111    3C                   ENDIF
403300931111    2C                   ENDIF
403400931111     C*
403500931029     C* SOLO PER TARIFFA A KILO O QUINTALE
403600931123     C                   MOVEL     FTDCTP        WALF1
403700931111    2C     WALF1         IFEQ      '0'
403800931029     C     WALF1         OREQ      '3'
403900931029     C*
404000020325      *  *-----------------------*
404100931111     C***   G I O R N A T A
404200020325      *  *-----------------------*
404300931029     C* SOLO SE NON HO GIA' VALORIZZATO LA BASE
404400931111    4C     *IN15         IFEQ      *OFF
404500931029     C     FTDITT        ANDEQ     0
404600950118     C     W01PAG        IFEQ      'S'
404700931125     C                   ADD       FTDNCL        W01CLA                          COLL GIORN
404800020304     C                   ADD       FTDPKL        W01KFA                          PESO GIORN
404900020325     C                   ADD       FTDVLU        W01TVL                          VOL  GIORN
405000020325      *
405100020325      * non deve essere l'anteporto per Isole
405200020325      *  in quanto per le spedizioni Isole non si deve calcolare nè la
405300020325      *   base nè lo stop. Quindi escludo dalla somma della giornata i valori
405400020325      * per il calcolo .
405500151124     c**** ftdfin        ifne      'I'
405600020325     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
405700020304     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
405800020304     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
405900151124     c***********        end
406000020325      *
406100950118     C                   END
406200020320     C     *IN19         IFEQ      *on
406300020304     C                   ADD       FTDNCE        W01CPE                          COLLI ETICH
406400020304     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
406500950118     C                   ELSE
406600020304     C                   ADD       FTDNCL        W01CPE                          COLLI PIK.
406700020304     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
406800950118     C                   END
406900931029     C                   MOVEL     *HIVAL        §TAITT
407000931029     C*
407100950214     C     W01PAG        IFEQ      'S'
407200931111    5C     W02STP        IFNE      FTDSTP                                        STOP GIORN
407300931111    6C     W02STP        IFGT      0
407400931029     C                   ADD       1             W01SNA
407500020212     C                   ADD       1             g01SNA
407600931111    6C                   ENDIF
407700931102     C                   MOVEL     FTDSTP        W02STP
407800931111    5C                   ENDIF
407900931029     C*
408000931111    5C     W02STP        IFEQ      FTDSTP
408100931102     C     W02STP        ANDEQ     0
408200931029     C                   ADD       1             W01SNA
408300020212     C                   ADD       1             g01SNA
408400931111    5C                   ENDIF
408500931111    4C                   ENDIF
408600950214     C                   ENDIF
408700931029     C*
408800931111    4C     FTDITA        IFGT      0
408900931029     C     *IN16         OREQ      *ON
409000931029     C                   SETON                                        17
409100931111    4C                   ENDIF
409200931029     C*
409300020304    2C                   ENDIF
409400931111     C*
409500931111     C* T A S S A Z I O N E   B O L L A
409600931111     C*
409700950118     C* SE CI SONO ENTRAMBI GLI IMPORTI NON TASSO
409800931112    1C     FTDITT        IFGT      0
409900931112     C     *IN15         OREQ      *ON
410000950118     C     FTDSIC        OREQ      'S'                                          RITORNO INCASSO
410100931112     C     FTDITA        IFGT      0
410200931112     C     *IN16         OREQ      *ON
410300950118     C     W01PAG        OREQ      *BLANKS                                      NON PAGARE
410400931112     C                   SETON                                        17
410500931111    1C                   ENDIF
410600931112    1C                   ENDIF
410700931112     C*
410800020325     C                   movel     *blanks       §taISO
410900020213     C                   MOVEL     'S'           §TAtsm
411000931112     C                   EXSR      TASSAZ
411100931102     C*
411200020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
411300020326     C* i primi tre caratteri
411400020326     c                   if        ftdfgs = 0
411500020326     c                   movel     ftdpdr        ftdfgs
411600020326     c                   end
411700020326     C*
411800011105     C                   UPDATE    fiftd001
411900931029     C                   ENDSR
412000020212     C*---------------------------------------------------------------*
412100931022     C* CALCOLO LE SOMME PESI A STOP ---------------------------------*
412200020212     C*---------------------------------------------------------------*
412300931022     C     CALSTP        BEGSR
412400931102     C*
412500931102     C* SE TROVATO STOP ED E' CAMBIATO --> TASSO LO STOP
412600931102    1C     W01STP        IFNE      0
412700931102     C     FTDSTP        ANDNE     W01STP
412800931102     C     W01STP        ORNE      0
412900931123     C     FTDCTP        ANDNE     W01CTP
413000020212     C*
413100000626     C* SOLO PER DEBUG
413200000626     C     FTDSTP        IFEQ      3
413300000626     C     W01STP        ANDEQ     2
413400000626     C                   MOVE      2             W01STP
413500000626     C                   ENDIF
413600931102     C*
413700020313      *  nuovo modo di salvare la chiave della spedizione in quanto i ritiri
413800020313      *   nella nuova forma hanno la riga con la spedizione.
413900020315     c                   clear                   w01spc
414000020315     c                   clear                   w01spr
414100020315     c                   IF        ftdnsp > 0
414200020313     C                   MOVEL     FTDSPC        W01SPC
414300020313     c                   else
414400020313     C                   MOVEL     FTDSPR        W01SPR
414500020313     c                   end
414600020325     C*
414700931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
414800011105     C     W05NRR        CHAIN     fiftd000                           32
414900931102     C*
415000931102     C                   Z-ADD     W01KFA        §TAKFA
415100931102     C                   Z-ADD     W01TVL        §TATVL
415200931102     C                   Z-ADD     W01NCL        §TANCL
415300950118     C                   Z-ADD     W01CPE        §TACPE
415400950123     C                   MOVEL     'S'           §TAPAG
415500950118     C     §TAKFA        IFEQ      0
415600950118     C                   MOVE      *BLANKS       §TAPAG
415700950118     C                   END
415800950118     C*
415900931102     C                   MOVEL     *HIVAL        §TAITA
416000020325     C                   movel     w01isola      §taISO
416100931102     C*
416200020213     C                   MOVEL     'T'           §TAtsm
416300931102     C                   EXSR      TASSAZ
416400020328      *
416500020325     C*  resetta i contatori per le isole e per l'espresso.
416600020325     c                   z-add     0             almuno_iso        1 0
416700020325     c                   z-add     0             almuno_xpr        1 0
416800030109     C*  resetta a livello di stop il destinatario disagiato
416900030109     c                   z-add     0             almuno_disag      1 0
417000931102     C*
417100020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
417200020326     C* i primi tre caratteri
417300020326     c  n32              if        ftdfgs = 0
417400020326     c                   movel     ftdpdr        ftdfgs
417500020326     c                   end
417600020326     C*
417700011105     C  N32              UPDATE    fiftd000
417800020325     C*
417900931102     C* RILEGGO IL RECORD
418000020313      * prima testava l'indicatore adesso non è più possibile distinguere i ritiri
418100020313      * allo stesso modo
418200020315     c                   IF        w01spc <> *blank
418300931202     C                   MOVEL     W01SPC        FTDSPC
418400931202     C                   MOVEL     FTDAAS        VH6AAS
418500931202     C                   MOVEL     FTDNSP        VI6NSP
418600020315     c                   if        vh4tsr='R'
418700020315      * vista per i nuovi ritiri x spedizione
418800020315     C     KFTD5         CHAIN(N)  fiftd005                           32
418900020315     c                   else
419000020315      * seleziona solo i consegnati
419100011105     C     KFTD2         CHAIN(N)  fiftd002                           32
419200020315     c                   end
419300940415   X2C                   ELSE
419400931202     C                   MOVEL     W01SPR        FTDSPR
419500931202     C                   MOVEL     FTDKSC        VI6NSP
419600011105     C     KFTD4         CHAIN(N)  fiftd004                           32
419700940415    2C                   ENDIF
419800931102     C*
419900020122     C                   CLEAR                   FICNB2
420000931102     C                   CLEAR                   W01KFA
420100931102     C                   CLEAR                   W01NCL
420200931102     C                   CLEAR                   W01TVL
420300950118     C                   CLEAR                   W01CPE
420400931102     C* REIMPOSTO I CAMPI
420500950118     C     W01PAG        IFEQ      'S'
420600950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
420700950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
420800931102     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
420900950125     C                   MOVEL     'S'           §TAPAG
421000950118     C                   ELSE
421100950118     C                   MOVE      *ZEROS        §TAKFA
421200950118     C                   MOVE      *ZEROS        §TATVL
421300950118     C                   MOVE      *ZEROS        §TANCL
421400950125     C                   MOVEL     ' '           §TAPAG
421500950118     C                   END
421600020313      *
421700020320     C     *in19         ifeq      *on
421800950118     C                   Z-ADD     FTDNCE        §TACPE
421900950118     C                   ELSE
422000950118     C                   Z-ADD     FTDNCL        §TACPE
422100950118     C                   END
422200020313      *
422300950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
422400021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
422500931102     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
422600931102     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
422700931110     C                   MOVEL     FTDSTP        W01STP
422800940415     C* MEMORIZZO CODICE TARIFFA
422900940415    2C     FTDCTP        IFEQ      *BLANKS
423000940415     C                   MOVEL     WALF3         FTDCTP
423100000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
423200000623    3C     WCTP          IFEQ      *BLANKS
423300000623     C     WCTB          ANDEQ     *BLANKS
423400000623     C                   MOVEL     WALF3         FTDCTP
423500000623     C                   ELSE
423600000623     C* VERIFICO SE BOLLA POSTA OPPURE NO
423700000623     C* CONTROLLO IN AZORG SE PO POSTE
423800000623     C                   EXSR      CTRORG
423900000623     C*  P.O. POSTE
424000020729     C     §OgNTW        IFEQ      'PPT'
424100000623     C                   MOVEL     WCTP          FTDCTP
424200000623     C                   ELSE
424300000623     C                   MOVEL     WCTB          FTDCTP
424400000623    4C                   ENDIF
424500000623     C*
424600000623    3C                   ENDIF
424700000623     C*
424800940415    2C                   ENDIF
424900931102    1C                   ENDIF
425000931102     C*
425100931022     C* C'E' LO STOP E LA BASE NON E' TASSATA(NE' DI BOLLA NE' DI DIST)
425200931102    1C     FTDSTP        IFGT      0
425300931022     C                   MOVEL     FTDSTP        W01STP                          SALVO STOP
425400931123     C                   MOVEL     FTDCTP        W01CTP                          SALVO CTR
425500931102     C                   Z-ADD     FTDNRR        W05NRR            9 0
425600950118     C     W01PAG        IFEQ      'S'
425700931022     C                   ADD       FTDNCL        W01NCL
425800950119     C                   END
425900950119     C*
426000931022     C                   MOVEL     *HIVAL        §TAITT
426100931102    2C     FTDITA        IFGT      0
426200931022     C     *IN16         OREQ      *ON
426300931022     C                   SETON                                        17
426400931102    2C                   ENDIF
426500931102    1C                   ENDIF
426600931022     C*
426700931022     C                   ENDSR
426800020212     C*---------------------------------------------------------------*
426900931102     C* TASSAZIONE BOLLA ---------------------------------------------*
427000020212     C*---------------------------------------------------------------*
427100931102     C     TASSAZ        BEGSR
427200091012      *
427300931111     C* 17 OFF - D A   T A S S A R E
427400931111    0C     *IN17         IFEQ      *OFF
427500931123     C                   MOVEL     FTDCAP        §TACAP                          CAP MIT/DES
427600020326     C                   MOVEL     ftdfgs        §TAfgs
427700091012      *
427800020326     c                   if        §tafgs=0
427900020326     C                   MOVEL     FTDPDR        §TAfgs                          P.O. di gestione
428000020326     c                   end
428100091012      *
428200931102     C                   MOVEL     FTDPDR        §TAPDR                          PADRONCINO
428300931102     C                   MOVEL     PARSML        §TASML                          SIMULAZ
428400020326     C                   MOVEL     vh4DDC        §TADDC                          DATA DISTIN
428500020326     C                   z-add     vi4ndc        §tandc
428600020129     C                   z-add     ftdaas        §taaas
428700020129     C                   z-add     ftdlnp        §talnp
428800020129     C                   z-add     ftdnrs        §tanrs
428900020129     C                   z-add     ftdnsp        §tansp
429000020328     C                   z-add     ftdstp        §tastp
429100091012      *
429200020225     c                   if        §tansp = 0 and ftdtsr='R'
429300020225     C                   z-add     ftdksc        §tansp
429400020326     C                   z-add     ftdksc        §taksc
429500020225     c                   end
429600050909      *
429700050909     C* Comunque aggancia sempre la bolla in partenza x vedere
429800050909      *  il codice trattamento merce
429900091012     c                   clear                   trovato_arb       1
430000091012     c                   clear                   trovato_blp       1
430100050909     C                   clear                   DS1B
430200091012     c     bolla_ftd     chain     fnblp01l
430300050909     c                   if        %found(fnblp01l)
430400091012      * ------------
430500091012     c                   move      'S'           trovato_blp       1
430600091012     C                   MOVE      blpCbo        §tacbo
430700091012     C                   z-add     blpKSC        CodCliente        7 0
430800130305     C                   MOVEL     blpTSP        §TATSP                          TIPO SPEDIZ
430900091012      *
431000091012     C                   MOVEL     '3A'          COD
431100091012     C                   MOVEL     *BLANKS       KEY
431200091012     C                   MOVEL     blpCBO        KEY
431300091012     C     KTAB          CHAIN     TABEL                              30
431400091012     C  N30              MOVEL     TBLUNI        DS3A
431500091012     c                   If        %subst(§3ATB1:1:1) ='A'
431600091012     C     Bolla_FTD     CHAIN     FiAR601L
431700091012     c                   if        %Found(Fiar601L)
431800091012     C                   Z-ADD     ar6KSC        CodCliente                      Cliente
431900091012     c                   end
432000091012      **   SOLO X RITIRI
432100091012     c                   iF        ftdtsr = 'R'
432200091012     C                   MOVEL     '1B'          COD
432300091012     C                   MOVEL     *BLANKS       KEY
432400091012     C                   MOVEL     blpCTM        KEY
432500091012     C     KTAB          CHAIN     TABEL                              30
432600091012     c                   if        %found(TABEL00F)
432700091012     C                   MOVEL     TBLUNI        DS1B
432800091012      * deve controllare se abilitata a tariffa Disk C
432900091012      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
433000091012      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
433100091012      *   la tariffa di Etichettatura in generale.
433200091012      *
433300091012      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
433400091012      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
433500091012     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
433600091012     c                   move      'N'           §taPPE
433700091012     c                   end
433800091012      * Se è un DISKC Bartolini la regola dice che deve essere comunque
433900091012      * pagata la spedizione al Padroncino
434000091012     c                   iF        §1Bdkc ='B' and ftdnrs = 0
434100091012     c                   eval      §taPPE = 'S'
434200091012     c                   eval      §tadkc = 'S'
434300091012     c                   end
434400091012     c                   end
434500091012      **
434600091012     c                   endiF
434700091012      *
434800091012     c                   endiF
434900091012      * ------------
435000091012     c                   end
435100091012      * Se non ha trovato il BLP
435200091012      *  oppure
435300091012      * Comunque se si tratta di consegne
435400091012     c                   if        NOT %found(fnblp01l) or FTDTSR = 'C'
435500091012     c     bolla_ftd     chain     fnarb01l
435600091012     c                   if        %found(fnarb01l)
435700091012     c                   move      'S'           trovato_arb       1
435800091012     C                   MOVE      arbCbo        §tacbo
435900091012     C                   z-add     arbKSC        CodCliente        7 0
436000130305     C                   MOVEL     ARBTSP        §TATSP                          TIPO SPEDIZ
436100130305     C* SOLO SE CONSEGNATA
436200130305    2C     FTDDCM        IFGT      0
436300130305     C                   MOVEL     ARBTC1        §TATC1                          CONS PARTIC
436400130305     C                   MOVEL     ARBTC2        §TATC2                          CONS PARTIC
436500130305    2C                   ENDIF
436600130305     C* GESTIONE PARTICOLARITA' VARIE
436700130305     C                   MOVEL     ARBGVA        §TAGVA
436800130305     C                   MOVE      ' '           §TAGVA
436900130305     C*
437000091012     c                   end
437100091012     c                   end
437200091012      *
437300091012      *  Carica i campi di passaggio
437400931202     C                   Z-ADD     1             §TASPP                          NUM STOP
437500931123     C                   MOVEL     FTDTSR        §TATSR                          TIPO PREST
437600931108     C                   MOVEL     FTDFIN        §TAFIN                          INOLTRO/ANT
437700931124     C                   MOVEL     FTDCTP        §TACTR                          COD TARIFFA
437800931102     C*
437900931102     C* SE GIA' VALORIZZATA IMPOSTO MAX VALORE PER NON CALCOLARLA
438000931102     C   15              MOVEL     *HIVAL        §TAITT                          BASE
438100931102     C   16              MOVEL     *HIVAL        §TAITA                          ACCESSORI
438200950118     C*
438300950118     C* IMPORTO  FATTURA ASSEGNATO INCASSATO
438400950118     C                   Z-ADD     FTDIFP        §TAIFP
438500011109      *  Divisa
438600011109     c                   if        vh4tpr = 2
438700011109     c                   movel     vi4ctd        §tadiv
438800011109     c                   end
438900950118     C* FLAG SOLO INCASSO
439000950118     C                   MOVEL     FTDSIC        §TASIC
439100130305     C* *******
439200020325     C                   MOVEL     *blank        §taxpr
439300020325     C* Se a spedizione
439400020325     c     §tatsm        ifeq      'S'
439500130305      **
439600130305     c     trovato_arb   IFEQ      'S'
439700030109     C*----------
439800020325     C* memorizza se all'interno dello stop sono tutte spedizioni espresso
439900020325     C* Se almeno una delle spedizioni all'interno dello stop non è
440000020325     C*  espresso ...lo segnala.
440100130305     c                   if        almuno_xpr = 1 and arbtsp <> 'E'
440200130305     c                             and arbtsp <> 'H'
440300020325     C                   move      *blank        w01xpres
440400020325     c                   end
440500020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
440600020325     C*   all'interno dello stesso stop non sia un espresso che deve segnalarlo
440700020325     c                   if        almuno_xpr = 0
440800020325     c                   z-add     1             almuno_xpr        1 0
440900020325     C                   move      *blank        w01xpres                       Espresso
441000130305     c                   if        arbtsp = 'E' or arbtsp = 'H'
441100020325     C                   move      'S'           w01xpres
441200020325     c                   end
441300020325     c                   end
441400030109     C*----------
441500030109     C* Se almeno una delle spedizioni all'interno dello stop non ha
441600030109     C*  il destinatario disagiato lo memorizza per non pagare il disagio.
441700030109     c                   if        almuno_disag = 1
441800030109     c                   if        arbtc1 = ' ' and arbtc2 = ' ' or
441900030109     c                             arbdcm = 0
442000030109     C                   move      *blank        w01disagiato                   non disagiato
442100030109     c                   end
442200030109     c                   end
442300030109     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
442400030109     C*   all'interno dello stesso stop non sia segnalata come destinatario
442500030109     C*    disagiato che deve tenerlo presente per tutte
442600030109     c                   if        almuno_disag = 0
442700030109     c                   z-add     1             almuno_disag      1 0
442800030109     C                   move      *blank        w01disagiato
442900030109     c                   if        arbtc1 = 'X' or arbtc2 = 'X'                 per indicare
443000030109     c                   if        arbdcm > 0                                   se nella spediz.
443100030109     C                   move      'S'           w01disagiato                   il destinatario
443200030109     c                   end                                                    è disagiato
443300030109     c                   end                                                    è disagiato
443400030109     c                   end
443500020325     C*
443600130305     c                   END
443700130305      **
443800020325     c                   ElsE
443900130305      **   S T O P
444000020325     C* solo a STOP passa il flag valorizzato per il calcolo
444100020325     C                   move      w01xpres      §taxpr
444200030109     C                   move      w01disagiato  §taDED
444300020325     c                   endIF
444400931124     C*
444500931124    2C     §TAITA        IFEQ      0
444600950131     C* CONTRASSEGNO
444700950131     C                   Z-ADD     FTDCAS        §TACAS
444800950131     C                   MOVEL     FTDVCA        §TAVCA
444900011106      *   con l'EURO non è più vero che la divisa lira=*blank
445000011106     c                   if        §tavca=*blank
445100011106     c                   eval      §tavca='ITL'
445200011106     c                   end
445300011106      *
445400011106      *   con l'EURO è tolto ogni tipo di cambio dai files
445500011106      *    quindi adesso occorre passare a (0) sempre il cambio
445600011106      *     sarà il programma FNLRB9R che dovrà eseguire il
445700011106      *      ragionamwento di conversione DIVISA/EURO.
445800020129     C*
445900950131     C* VEDO SE C/A PRONTAMENTE RECUPERATO
446000931124     C                   EXSR      CTRASS
446100931124    2C                   ENDIF
446200000705     C*
446300130305     C* SE IL CODICE MANCATA CONSEGNA È VALORIZZATO
446400000705     C                   MOVEL     *BLANK        §TAFGC
446500000623    2C     FTDCMC        IFNE      *BLANK
446600000623     C* VERIFICO SE BOLLA POSTA
446700000623     C                   EXSR      CTRORG
446800000623     C*  P.O. POSTE
446900050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
447000020729     C     §OgNTW        IFEQ      'PPT'
447100050426     C     vh4tpo        andeq     'S'
447200130305     C*
447300000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
447400000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
447500000623     C                   MOVEL     '2Z'          COD
447600000623     C                   MOVEL     *BLANKS       KEY
447700000623     C                   MOVEL     FTDCMC        KEY
447800000623     C     KTAB          CHAIN     TABEL                              30
447900000623     C  N30              MOVEL     TBLUNI        DS2Z
448000000623     C   30              MOVEL     *BLANKS       DS2Z
448100000623     C                   MOVEL     §2ZTPP        §TAFGC
448200000623     C                   MOVEL     §2ZTPP        FTDFGC
448300000623    3C                   ENDIF
448400000623    2C                   ENDIF
448500091012     C*----------
448600950125     C     §TAPAG        IFEQ      ' '
448700950125     C                   MOVE      *HIVAL        §TAITA
448800950125     C                   END
448900950125     C     §TASIC        IFEQ      'S'
449000950125     C                   MOVE      *HIVAL        §TAITT
449100950125     C                   END
449200030821     C*----------
449300030821     C* contatore delle spedizioni presenti nello stesso STOP
449400030821     C* di quante "FW" e di quante "RCC".
449500030821     c     §tatsm        ifeq      'S'
449600130305      **
449700030821     c                   z-add     W01nss        §taNSS
449800030821     c                   z-add     W01fw         §taFW
449900030821     c                   z-add     W01rcc        §taRCC
450000030821     c                   z-add     W01nss        SAVnss
450100030821     c                   z-add     W01fw         SAVfw
450200030821     c                   z-add     W01rcc        SAVrcc
450300090911      *
450400090911      *  sullo stop tariffe particolari x cliente
450500090914     c                   eval       SAV_TarPartCLI = W01_TarPartCLI
450600090914     c                   eval       SAV_UnoOpiuCLI = W01_UnoOpiuCLI
450700090918     C*
450800090918      * se valorizza a Spedizione
450900090918     C*   Deve passare il codice del cliente solo se è un cliente particolare
451000090918     c     CodCliente    lookup    KcliPAR                                30
451100090918     c                   if        *in30
451200130307     C                   Z-ADD     CodCliente    §TAcliPAR
451300090918     C                   end
451400090911      *
451500030821     c                   else
451600090918      * se valorizza a Stop
451700090918     C*
451800030821      * quando è a stop imposta i campi precedentemente salvati altrimenti
451900030821      * sarebbero impostati per il giro successivo del nuovo STOP
452000030821     c                   z-add     SAVnss        §taNSS
452100030821     c                   z-add     SAVfw         §taFW
452200030821     c                   z-add     SAVrcc        §taRCC
452300090911      *
452400090911      *  sullo stop tariffe particolari x cliente
452500090911     c                   eval       §TACLIPAR      = SAV_UnoOpiuCLI
452600090911     c                   eval       §TATUTTE       = SAV_TarPartCLI
452700090911      *
452800030821     c                   end
452900030821     C*----------
453000020426     C                   MOVEL     'CALCVAL'     §TApgm
453100020205     C*
453200020122     C                   CALL      'FICNB2R'
453300020122     C                   PARM                    FICNB2
453400020328      *
453500020328      * cancella i records utilizzati nello STOP dal file di work
453600020328     C     §TAtsm        IFEQ      'T'
453700020422    3C     parsml        andeq     *blank
453800020328     c                   exsr      del_FTDW
453900020328     C                   END
454000020328      *
454100931102    0C                   ENDIF
454200931102     C*
454300931102     C* E R R ="E"  M A N C A   T A R I F F A
454400931103    1C     §TAERR        IFEQ      'E'
454500931102     C                   SETON                                        4290
454600931102     C* ERRORE: SEGNALO LA DISTINTA
454700931112     C     VI4SCE        IFNE      'E'
454800931102     C                   MOVEL     'E'           VI4SCE
454900011109     c     vh4tpr        comp      2                                      26
455000931102     C                   UPDATE    FRB8SF4
455100931112     C                   ENDIF
455200931102     C* STAMPA MANCA TARIFFA
455300931102     C                   EXSR      STAERR
455400931102     C*
455500931103   X1C                   ELSE
455600931102     C*
455700931102     C* N O N   E '   M A N C A   T A R I F F A
455800931102     C* SE BASE      DISTINTA VALORIZZATA AZZERO IL CAMPO IN BOLLA
455900931103    2C     *IN15         IFEQ      *ON
456000931102     C                   CLEAR                   FTDITT
456100931102     C                   CLEAR                   FTDFIT
456200931103   X2C                   ELSE
456300931102     C*
456400931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
456500020226    3C     §TAITT        IFNE      *ALL'9'
456600020214     C                   add       §TAITT        FTDITT                          BASE
456700931109     C                   ADD       §TATPE        FTDITT                          PIK/ETICH
456800931109     C                   ADD       §TATST        FTDITT                          STOP
456900020320     C                   add       §taITT        w01ITT                          BASE
457000020320     C                   add       §taTPE        w01ITT                          PIK/ETICH
457100020320     C                   add       §taTST        w01ITT                          STOP
457200020226    3C                   ENDIF
457300931103    2C                   ENDIF
457400931102     C* SE ACCESSORI DISTINTA VALORIZZATI AZZERO IL CAMPO IN BOLLA
457500931103    2C     *IN16         IFEQ      *ON
457600931102     C                   CLEAR                   FTDITA
457700931102     C                   CLEAR                   FTDFIA
457800931103   X2C                   ELSE
457900931102     C*
458000931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
458100020226    3C     §TAITA        IFNE      *ALL'9'
458200931102     C                   ADD       §TAITA        W01ITA
458300020214     C                   add       §TAITA        FTDITA
458400020226    3C                   ENDIF
458500931103    2C                   ENDIF
458600931102     C*
458700931103    1C                   ENDIF
458800931102     C*
458900931102     C* SE VALORIZZATA SIA BASE CHE ACCESSORI
459000931102     C*  CLEAR SE IMPOSTATA A LIVELLO DI TESTATA
459100931102     C   15              CLEAR                   FTDITT
459200931102     C   15              CLEAR                   FTDFIT
459300931102     C   16              CLEAR                   FTDITA
459400931102     C   16              CLEAR                   FTDFIA
459500130305      *
459600931102     C                   ENDSR
459700020212     C*---------------------------------------------------------------*
459800931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA TASSARE/SOMMARE ---------*
459900020212     C*---------------------------------------------------------------*
460000931102     C     CTRSTP        BEGSR
460100940606     C                   SETOFF                                       17
460200130305     C*
460300931102     C* S O M M A   P E S I   A   S T O P
460400931102     C     W01STP        IFNE      0
460500931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
460600011105     C     W05NRR        CHAIN     fiftd000                           32
460700931102     C*
460800020122     C                   CLEAR                   FICNB2
460900931102     C                   Z-ADD     W01KFA        §TAKFA
461000931102     C                   Z-ADD     W01TVL        §TATVL
461100931102     C                   Z-ADD     W01NCL        §TANCL
461200950118     C                   Z-ADD     W01CPE        §TACPE
461300950123     C                   MOVEL     'S'           §TAPAG
461400950118     C     §TAKFA        IFEQ      0
461500950118     C                   MOVE      *BLANKS       §TAPAG
461600950118     C                   END
461700931102     C                   MOVEL     *HIVAL        §TAITA
461800931123     C     FTDCTP        IFEQ      *BLANKS
461900931123     C                   MOVEL     WALF3         FTDCTP
462000931123     C                   ENDIF
462100950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
462200021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
462300020325     C                   movel     w01isola      §taISO
462400931102     C*
462500020213     C                   MOVEL     'T'           §TAtsm
462600931102     C                   EXSR      TASSAZ
462700931102     C*
462800020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
462900020326     C* i primi tre caratteri
463000020326     c  n32              if        ftdfgs = 0
463100020326     c                   movel     ftdpdr        ftdfgs
463200020326     c                   end
463300020326     C*
463400011105     C  N32              UPDATE    fiftd000
463500931102     C                   ENDIF
463600931102     C*
463700931102     C* S O M M A   P E S I   A   G I O R N A T A
463800950214     C     W02STP        IFGT      0
463900931102     C                   ADD       1             W01SNA
464000020212     C                   ADD       1             g01SNA
464100931102     C                   ENDIF
464200130305     C*
464300931102     C                   ENDSR
464400020212     C*---------------------------------------------------------------*
464500931103     C* SOMMO LE VALORIZZAZIONI CHE CI SONO NEL DETTAGLIO -----------*
464600020212     C*---------------------------------------------------------------*
464700931103     C     AGGFTD        BEGSR
464800130305     C*
464900011105     C     KFTD          SETLL     fiftd001
465000011105     C     KFTD          READE     fiftd001                               32
465100931103     C*
465200931103     C     *IN32         DOWEQ     *OFF
465300020405     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
465400020405     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
465500020405     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
465600020405     C     *IN19         IFEQ      *on
465700020405     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
465800020405     C                   ELSE
465900020405     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
466000020405     C                   END
466100931103     C* TESTATA
466200931103     C     *IN15         IFEQ      *ON
466300931103     C                   CLEAR                   FTDITT
466400931103     C                   CLEAR                   FTTFIT
466500931103     C                   ELSE
466600931103     C                   ADD       FTDITT        W01ITT
466700931103     C                   ENDIF
466800931103     C* DETTAGLIO
466900931103     C     *IN16         IFEQ      *ON
467000931103     C                   CLEAR                   FTDITA
467100931103     C                   CLEAR                   FTTFIA
467200931103     C                   ELSE
467300931103     C                   ADD       FTDITA        W01ITA
467400931103     C                   ENDIF
467500020326     C*
467600020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
467700020326     C* i primi tre caratteri
467800020326     c                   if        ftdfgs = 0
467900020326     c                   movel     ftdpdr        ftdfgs
468000020326     c                   end
468100020326     C*
468200011105     C                   UPDATE    fiftd001
468300931103     C*
468400011105     C     KFTD          READE     fiftd001                               32
468500931103     C                   ENDDO
468600931103     C*
468700931103     C                   ENDSR
468800020212     C*---------------------------------------------------------------*
468900931103     C* STAMPA MANCA TARIFFA -----------------------------------------*
469000020212     C*---------------------------------------------------------------*
469100950116     C     STAERR        BEGSR
469200950116     C                   MOVEL     FTDRSC        W01RSB           15
469300931103     C  N18VH4TSR        IFEQ      'R'
469400931104     C                   SETON                                        19
469500931103     C                   ELSE
469600931104     C                   SETOFF                                       19
469700931103     C                   ENDIF
469800931103     C* TESTATA
469900931103     C     *INOF         IFEQ      *ON
470000950127     C     VI4DDC        ORNE      WM1DDC
470100931103     C                   EXCEPT    TES
470200931103     C                   SETOFF                                       OF
470300950127     C                   Z-ADD     VI4DDC        WM1DDC
470400931103     C                   ENDIF
470500950127     C  N18              MOVE      FTDAAS        AASPRT            2 0
470600950127     C  N18              MOVEL     FTDCAP        CAPPRT            5
470700931103     C* DETTAGLIO
470800931103     C   18              EXCEPT    DET1
470900950116     C  N18              EXCEPT    DET2
471000931103     C*
471100931103     C                   ENDSR
471200020212     C*---------------------------------------------------------------*
471300931103     C*  VEDO SE DARE MANCA TARIFFA ---------------------------------*
471400020212     C*---------------------------------------------------------------*
471500931103     C     MANTAR        BEGSR
471600020122     C                   CLEAR                   FICNB2
471700020426     C                   MOVEL     'CALCVAL'     §TApgm
471800931209     C                   MOVEL     PARSML        §TASML
471900931103     C                   MOVEL     VI4PDR        §TAPDR
472000020326     C                   MOVEL     vi4pdr        §TAfgs
472100011205     c                   movel     vi4ctd        §tadiv
472200020326     C                   MOVEL     vh4DDC        §TADDC
472300020326     C                   z-add     vi4ndc        §tandc
472400931103     C                   MOVEL     'M'           §TAFLG
472500020122     C                   CALL      'FICNB2R'
472600020122     C                   PARM                    FICNB2
472700931103     C*
472800931103     C* ERR=E DEVO DARE MANCA TARIFFA
472900931103    1C     §TAERR        IFEQ      'E'
473000931202     C                   SETON                                        184290
473100931103     C                   EXSR      STAERR
473200931202     C                   SETOFF                                       18
473300931103     C* SEGNALO LA DISTINTA
473400931103     C                   MOVEL     'E'           VI4SCE
473500011109     c     vh4tpr        comp      2                                      26
473600931103     C                   UPDATE    FRB8SF4
473700931103     C*
473800931103   X1C                   ELSE
473900931103     C* SOMMO LE SINGOLE VALORIZZAZIONI EVENTUALMENTE FATTE IN TESTATA
474000931103     C*  SENZA DARE ERRORE
474100931103     C                   EXSR      AGGFTD
474200931103    1C                   ENDIF
474300130305      *
474400931103     C                   ENDSR
474500020212     C*---------------------------------------------------------------*
474600931103     OPRTF198   E            TES              02
474700931102     O                       STA(1)              66
474800931102     O                       STA(2)             132
474900931103     O                       STA(3)             198
475000931102     O                       RSUT                20
475100931102     O                       SIMFEL              24
475200950127     O                       VI4DDC              99 '  /  /  '
475300931103     O                       UDATE              160 '  .  .  '
475400931103     O                       PAGE          Z    198
475500931103     O          E            TES              03
475600931103     O                       UTIME              160 '  :  :  '
475700931103     O          E            TES              04
475800931103     O                       STA(4)              66
475900931103     O                       STA(7)             132
476000931103     O                       STA(10)            198
476100931103     O          E            TES            1 05
476200931103     O                       STA(5)              66
476300931103     O                       STA(8)             132
476400931103     O                       STA(11)            198
476500931103     O          E            DET1           1
476600931103     O                       STA(13)             66
476700931103     O                       STA(14)            132
476800931103     O                       STA(15)            198
476900931103     O                       VI4PDR               8
477000931103     O                                              '-'
477100931103     O                       VH4DPD
477200950120     O                       VH4TSH            +  1
477300931110     O                       VI4NDC        Z   +  2
477400931110     O                       W01ERR             170
477500931103     O          E            DET2           1
477600931103     O                       STA(13)             66
477700931103     O                       STA(14)            132
477800931103     O                       STA(15)            198
477900931103     O                       VI4PDR               8
478000931103     O                                              '-'
478100931103     O                       VH4DPD
478200950120     O                       VH4TSH            +  1
478300931110     O                       VI4NDC        Z   +  2
478400950127     O                       AASPRT        Z   +  1
478500931103     O                       FTDLNP        Z   +  1
478600931103     O                       FTDNRS        Z   +  1
478700931103     O                       FTDNSP        Z   +  1
478800931123     O              N19      W01RSB            +  1
478900931123     O              N19      FTDFIN            +  2
479000931123     O               19      W01RSB            +  2
479100931123     O               19      FTDFIN            +  2
479200950120     O                       FTDPKL        4   +  1
479300931103     O                       FTDNCL        Z   +  1
479400950116     O                       FTDVLU        4   +  1
479500950120     O              N19      CAPPRT            +  2
479600950120     O               19      CAPPRT            +  3
479700931110     O                       §TACTR            +  3
479800931110     O                       SER(1)            +  3
479900931103**
480000931028NON ESISTONO DISTINTE/RITIRI da visualizzare per la selezione effettuata
480100030519Inserire Tariffe Valide per Distinte di Consegna -> Mancano le Tariffe
480200030519Inserire Tariffe Valide per Distinte di Ritiro   -> Mancano le Tariffe
480300931103**
480400931103XXXXXXXXXXXXXXXXXXXX/XXX                ***  CONTROLLO  TASSAZIONE   1
480500020322  AUTOTRASP.  DEL GIORNO XX/XX/XX  ***                     FICNB1R   2
480600931103                    XX.XX.XX                             PAG. XXXX   3
480700931110'      C O D I C E      '   TIPO   ' NUMERO '  SPEDIZIONE     '      4
480800020322'   AUTOTRASPORTATORE   ' PRESTAZ. 'DISTINTA'                 'RAG   5
480900931103'XXXXXXX-XXXXXXXXXXXXXXX'XXXXXXXXXX' XXXXXXX'XX XXX XX XXXXXXX'XXX   6
481000931110DESTINATARIO   '     MITTENTE      ' PESO   'COLLI'VOLUME'  CAP  '
481100931110.SOCIALE     IN' RAG.SOCIALE     AN'        '     '      ' DEST. '
481200950120XXXXXXXXXXXX  X' XXXXXXXXXXXXXXX  X'XXXXXX,X'XXXXX'XX,XXX' XXXXX '
481300931110  CAP  ' COD '     A N O M A L I A      '
481400931110 MITT. ' TAR '                          '
481500931110 XXXXX ' XXX ' XXXXXXXXXXXXXXXXXXXXXXXXX'
481600931110'                       '          '        '                 '     13
481700931110               '                   '        '     '      '       '  14
481800931110       '     '                          '                           15
481900931103**
482000931103MANCA TARIFFA
482100020322L'AUTOTRASP.NON HA TARIFFE
482200020328**
482300020328RGZPFM FILE(FIFTDW0F)
