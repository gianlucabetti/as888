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
001900991104     FFIARBF1C  IF   E           K DISK
002000030418     f                                     prefix(K)
002100950116     F                                     IGNORE(FNARBD00)
002200991104     F                                     IGNORE(FIARBT00)
002300950116     F                                     IGNORE(FNARBG00)
002400950116     F                                     IGNORE(FNARBM00)
002500950116     F                                     IGNORE(FNARBV00)
002600991104     F                                     IGNORE(FNARBP00)
002700950116     FFNLBL01L  IF   E           K DISK
002800020408     FFifce01L  UF   E           K DISK    commit
002900020408     FFIFTT01L  UF   E           K DISK    commit
003000060118     Ffiftt21L  iF   E           K DISK    RENAME(FIFTT000:FIFTT021)
003100011105     FFIFTT02L  IF   E           K DISK
003200011105     F                                     RENAME(FIFTT000:FIFTT002)
003300020328      *
003400020328      * work file per attribuire a stop i valori sulle voci di C/E
003500020328     Ffiftdw1L  UF A E           K DISK    usropn
003600051107     F******                               commit
003700020328      *
003800931202     F* DETTAGLIO CONSEGNE E RITIRI
003900020408     Ffiftd01L  UF   E           K DISK    commit
004000011105     F                                     RENAME(fiftd000:fiftd001)
004100931102     F                                     INFDS(TARDS)
004200931202     F* DETTAGLIO DELLE CONSEGNE
004300060126     F****fiftd02L  UF   E           K DISK    commit
004400060126     Ffiftd22L  UF   E           K DISK    commit
004500011105     F                                     RENAME(fiftd000:fiftd002)
004600020315     F* DETTAGLIO DEI nuovi RITIRI
004700020408     Ffiftd05L  UF   E           K DISK    commit
004800020315     F                                     RENAME(fiftd000:fiftd005)
004900020315     F* DETTAGLIO DEI RITIRI
005000020408     Ffiftd04L  UF   E           K DISK    commit
005100011105     F                                     RENAME(fiftd000:fiftd004)
005200020408     Ffiftd00F  UF   E             DISK    commit
005300011112     FFIFGT01L  IF   E           K DISK
005400091112     FFITGT01L  IF   E           K DISK
005500021202     Ffiapd01L  IF   E           K DISK
005600950113     FFNARB01L  IF   E           K DISK
005700090918     Ffiar601L  IF   E           K DISK
005800000622     FAZORG01L  IF   E           K DISK
005900931103     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
006000930707     F*
006100020408      * record del sfl per gestire correttamente il posizionamento del sfl
006200020408     D info            DS
006300020408     D  nrrult               378    379B 0
006400020408     F*
006500911017     D L1              S              3  0 DIM(30)                              FIL GESTITE
006600950112     D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
006700931108     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
006800931202     D DTS             S             15    DIM(30)                              DESCR.TIP PRESTAZ
006900931202     D CTS             S              3  0 DIM(30)                              CDTAR TIP PRESTAZ
007000040308     D CMC             S              6    DIM(9999)                            MANC.CONS DA TASS
007100040308     D CBL             S              6    DIM(9999)                            CD.BOL DA NO TASS
007200931018     D K17             S              1    DIM(17)                              SKI 17 ALFABETICA
007300931104     D*
007400090908     DKCliPar          S              7s 0 DIM(9999)                            Tar.Part.sul cliente
007500090909     DKtutteBolle      S              1A
007600090908     D*
007700030519     D ERR             S             72    DIM(3) CTDATA PERRCD(1)              MSG DI ERR VID
007800931103     D STA             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE STAMPA
007900931104     D SER             S             25    DIM(2) CTDATA PERRCD(1)              MSG DI ERR STA
008000020328     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              COMANDI da eseguire
008100950125     D* PASSAGGIO DATI DA CONFERMA CONTEGGI              - FNLRC0R -
008200931028     D PARAM           DS
008300931028     D  PARPDR                 1      7  0
008400931028     D  PARTSR                 8      8
008500950119     D  PARDDC                 9     16  0
008600931028     D  PARSML               256    256
008700950113     D* PASSAGGIO DATI A INTERROGAZIONE BOLLE ARRIVI     - FNLR36R -
008800931021     D PARAM2          DS
008900931021     D  GIA                    1     12  0 INZ
009000931021     D  RICH                  13     13    INZ
009100950113     D  PA2AAS                14     17  0
009200950113     D  PA2LNP                18     20  0
009300950113     D  PA2NRS                21     22  0
009400950113     D  PA2NSP                23     29  0
009500950113     D  PA2FLG                31     31
009600950116     D* PASSAGGIO DATI A RICERCA CODICE TARIFFA PADRONCINO-TNTA86R -
009700931028     D PARAM3          DS
009800931108     D  PA3PDR                 1      7  0
009900931108     D  PA3SML                 8      8
010000931108     D  PA3TSR                 9      9
010100931108     D  PA3CTR                11     13  0
010200931108     D  PA3PRG                14     16  0
010300931108     D  PA3SEL                17     17
010400020326      *
010500020326     D DSLS05          DS
010600020326     D  PA0AAS                 1      4  0
010700020326     D  PA0LNP                 5      7  0
010800020326     D  PA0NRS                 8      9  0
010900020326     D  PA0NSP                10     16  0
011000020326     D  PA0F03                17     17
011100020326     D  PA0FLG                18     18
011200020326     D  PA0A9                 19     19
011300020326     D  PA0B5                 20     20
011400020326     D  PA075                 21     21
011500020326     D  PA098                 22     22
011600020326     D  PA0RSU                23     42
011700020326     D  PA0CML                43     43
011800020326     D  PA0SIM                44     46  0
011900020326      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
012000020326     D  PA0CLI                47     47
012100020326      * PA0GIA indica se il pgm e' richiamato dalle giacenze
012200020326     D  PA0GIA                48     48
012300020326      *
012400950112     D* DS PER TRUL06R - CARICAMENTO £X
012500950112     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012600950112     D  LIN                    1     90  0
012700950112     D                                     DIM(30)                              FIL. COMODO
012800920810     D KPJBA         E DS
012900100513     d   KPJBUS        s                   like(KPJBU)
013000910423     D WLBDAT          DS
013100950112     D  G02DAT                 1      8  0
013200950112     D  G02INV                 9     16  0
013300950112     D  G02ERR                17     17
013400950112     D  G02TGI                18     22  0
013500910423     D WGIDAT          DS
013600910423     D  GIODAT                 1      6  0
013700910423     D  GIOINV                 7     12  0
013800910423     D  GIOTGI                13     17  0
013900931104     D TCUDS           DS
014000931104     D  F1                     1      1
014100931104     D  F3                     3      3
014200931104     D  F2                     2      2
014300931104     D  F4                     4      4
014400931104     D  F56                    5      6
014500931102     D TARDS           DS
014600931102     D  FTDNRR               397    400B 0
014700931018     D                 DS
014800931019     D  AA                     1      2
014900931019     D  MM                     3      4
015000931019     D  GG                     5      6
015100931018     D  DATA                   1      6  0
015200931028     D                 DS
015300931028     D  VH4TSR                 1      1
015400950116     D  ARBCBO                 2      3
015500950224     D  W018C                  1      3
015600931015     D                 DS
015700931015     D  VI1PF1                 1      3  0
015800931015     D  VI1PD1                 4      7  0
015900931103     D  VI1P1                  1      7  0
016000931015     D                 DS
016100931015     D  VI1PF2                 1      3  0
016200931015     D  VI1PD2                 4      7  0
016300931103     D  VI1P2                  1      7  0
016400931018     D                 DS
016500931018     D  VI1ND0                 1      7  0
016600931018     D  VI1ND1                 8     14  0
016700931018     D  VI1ND2                15     21  0
016800931018     D  VI1ND3                22     28  0
016900931018     D  VI1ND4                29     35  0
017000931018     D  VI1ND5                36     42  0
017100931018     D  VI1ND6                43     49  0
017200931018     D  VI1ND7                50     56  0
017300950112     D  VI1ND8                57     63  0
017400931018     D  VI1ND9                64     70  0
017500931018     D  NDC                    1     70  0
017600931018     D                                     DIM(10)                              NUM DISTINT SELEZ
017700931102     D                 DS
017800950116     D  FTDAAS                 1      4  0
017900950116     D  FTDNRS                 5      6  0
018000950116     D  FTDNSP                 7     13  0
018100950116     D  FTDLNP                14     16  0
018200950116     D  FTDSPC                 1     16
018300931202     D                 DS
018400931202     D  FTDKSC                 1      7  0
018500950116     D  FTDRSC                 8     42
018600950116     D  FTDSPR                 1     42
018700950116     D                 DS
018800950116     D  VI4PDR                 1      7  0
018900950116     D  VI4FIL                 1      3  0
019000931021     D DS1Z          E DS
019100000622     D DS3A          E DS
019200000623     D DS2Z          E DS
019300090909     D   DCTP        E DS
019400000622     D OG143         E DS
019500021212     D DS1B          E DS
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
031800910419     C*
031900100513     C                   clear                   ficnb1ds0
032000100513     c                   eval      FN0PGM  = 'FICNB1R'
032100100513     c                   eval      FN0TIPO = 'FILIALE'
032200100513     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
032300100513     C                   MOVEL     KPJBU         kpjbus
032400100513     C                   clear                   kpjbu
032500100513     C                   MOVEL     ficnb1ds0     kpjbu
032600100513     c                   call      'FICNB1R0'
032700100513     c                   parm                    kpjba
032800100513     C                   MOVEL     kpjbu         ficnb1ds0
032900100513     C*   se già utilizzata da altro utente della stessa filiale fa uscire
033000100513     C*   forzatamente.
033100100513     c                   if        Fn0FLG <> *blank
033200100513     c                   goto      FINE
033300100513     c                   end
033400100513     C*
033500100513     C                   MOVEL     KPJBUs        kpjbu
033600100513     C*
033700931015     C     INIZIO        TAG
033800931015     C* PULIZIA FORMATO 1
033900931103     C  N20              EXSR      PUL01
034000930426     C*
034100931015     C     FOR01         TAG
034200931028     C*
034300931028     C* PGM NON RICHIAMO - EMETTO 1 VIDEATA ALTRIMENTI VADO ALLA 2A
034400931105     C* SE SVALORIZZATE SPEDIZIONI EMETTO SUBITO LA 2A (21 ON)
034500931028     C     *IN20         IFEQ      *OFF
034600931105     C     *IN21         ANDEQ     *OFF
034700931028     C*
034800931015     C                   EXFMT     FRB8D01
034900020409     c                   z-add     0             savnr2
035000020409     c                   z-add     0             savnr4
035100020409     c                   z-add     0             savnr6
035200020604     c                   move      'N'           seF6              1
035300020604      * se richieste le distinte già Valorizzate non deve permettere di rivalorizzare
035400020604      * quindi imposto la memoria del flag SeF6 come se fosse stato dato l'F6 di
035500020604      * valorizzazione e impedire un nuovo F6 che raddoppia i conteggi.
035600020604     c                   if        vi1fvl ='V'
035700020604     c                   move      'S'           seF6
035800020604     c                   end
035900931103     C                   SETOFF                                       4746
036000920323     C** CMD3 - FINE LAVORO
036100931028     C   KC              GOTO      FINE
036200931015     C* CONTROLLI FORMATO1
036300900528     C                   EXSR      CONTR1
036400900528     C   90              GOTO      FOR01
036500931028     C                   ENDIF
036600931018     C*
036700931018     C* CARICO DISTINTE SELEZIONATE
036800931018     C                   EXSR      CARNDC
036900950616     C   90
037000950616     CAN 20              GOTO      FINE
037100950616     C   90              GOTO      FOR01
037200900611     C*
037300931119     C                   Z-ADD     1             REC2
037400900518     C*
037500931018     C     FORCT2        TAG
037600931103     C  N90              WRITE     FRB8D03
037700020603     c                   if        savnr2 > 0 and savnr2 <= rec2
037800020409     c                   z-add     savnr2        rec2
037900020409     c                   end
038000931018     C                   EXFMT     FRB8CT2
038100020409     c                   z-add     nrrult        savnr2            5 0
038200020411     c                   z-add     0             savnr4
038300920323     C** CMD3 - FINE LAVORO
038400931028     C   KC              GOTO      FINE
038500900518     C** CMD12 - RITORNO
038600931028     C   KL
038700931028     CANN20              GOTO      FOR01
038800931028     C   KL
038900931028     CAN 20              GOTO      FINE
039000931018     C*
039100931018     C** CMD1 O ENTER - SELEZIONE DISTINTE
039200931018     C                   EXSR      SELNDC
039300931018     C   90              GOTO      FORCT2
039400931018     C*
039500931103     C                   Z-ADD     1             REC4
039600931018     C     FORCT4        TAG
039700020604      *
039800020604      * se ha già eseguito la valorizzazione non deve permettere un secondo
039900020604      * F6 a causa di un manca tariffa
040000020604     c     seF6          comp      'N'                                    88
040100020604      *
040200931103     C  N90              WRITE     FRB8D05
040300020409     c                   if        savnr4 > 0
040400020409     c                   z-add     savnr4        rec4
040500020409     c                   end
040600931018     C                   EXFMT     FRB8CT4
040700020604      *
040800020409     c                   z-add     nrrult        savnr4            5 0
040900020411     c                   z-add     0             savnr6
041000960906     C                   SETOFF                                       90
041100931110     C* SE AGGIORNATO DISTINTA CON DEI MANCA TARIFFA OBBLIGO IN CMD6
041200931110     C     *INKL         IFEQ      *ON
041300931110     C     *INKC         OREQ      *ON
041400931110     C   22              SETON                                        4490
041500931110     C   22              GOTO      FORCT4
041600931110     C                   ENDIF
041700931105     C** CMD3 - FINE LAVORO
041800931105     C   KC              GOTO      FINE
041900931103     C** CMD12 - RITORNO
042000931110     C*
042100931110     C   KL
042200931110     CANN20              GOTO      FORCT2
042300931110     C   KL
042400931110     CAN 20              GOTO      FINE
042500931018     C* CONTROLLO SFL4
042600931018     C                   EXSR      CONTR4
042700931103     C* CMD3 - DALLA VIDEATA DI MANUTENZIONE SPEDIZIONI
042800931019     C   LR              GOTO      FINE
042900931019     C* ERRORI
043000931105     C   90              GOTO      FORCT4
043100931105     C* RIEMETTO IL SFL2 PERCHE' HO ANNULLATO DELLE VALORIZZAZIONI
043200931105     C   21              GOTO      FOR01
043300931105     C* CMD6 - ESEGUO VALORIZZAZIONE
043400020604     c   kf              move      'S'           seF6
043500931105     C   KF              EXSR      VALZZ
043600020408     C   kf              commit
043700931105     C  NKF
043800931105     COR KF
043900931105     CAN 90              GOTO      FORCT4
044000931028     C  N20              GOTO      INIZIO
044100900611     C*
044200000000     C     FINE          TAG
044300100513     C*
044400100513     C*  deve cancellare il record di allocazione x impedire le trasmissioni
044500100513     C*   in sede durante l'utilizzo di questo programma.
044600100513     c     parsml        ifeq      *blank
044700100513     C     Kwalc         setll     fiwalc1l
044800100513     C     Kwalc         reade     fiwalc1l
044900100513     c                   dow       not %eof(fiwalc1l)
045000100513     c                   delete    fiwalc00
045100100513     C     Kwalc         reade     fiwalc1l
045200100513     c                   end
045300100513     c                   end
045400100513      *
045500020408     c                   commit
045600020408     C*
045700020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
045800020328     C*   economico.
045900020328     C                   clear                   FICNB2
046000020328     C                   move      'C'           §taflg
046100020328     C                   CALL      'FICNB2R'
046200020328     C                   PARM                    FICNB2
046300020328     C*
046400020328     c                   close     fiftdw1l
046500020328     C*
046600020422     c                   if        parsml=' '
046700051109     c                   goto      norgzpfm
046800020328     C                   clear                   MSGDS
046900020328     C                   MOVEL     CMD(1)        MSGDS
047000020328     C                   Z-ADD     80            LENGH
047100020328     C                   CALL      'QCMDEXC'                            99
047200020328     C                   PARM                    MSGDS
047300020328     C                   PARM                    LENGH
047400051109     c     norgzpfm      tag
047500051109     c                   end
047600020328     C*
047700931028     C                   SETON                                        LR
047800950112     C****************************************************************
047900950112     C     *INZSR        BEGSR
048000950112     C*
048100950112     C     *ENTRY        PLIST
048200950112     C                   PARM                    KPJBA
048300950112     C                   MOVEL     KPJBU         PARAM
048400100513      *
048500100513     c     *dtaara       define    §azute        azuteds
048600100513     c     *dtaara       define    §datiute      ddatiute
048700100513     C                   in(E)     *dtaara
048800100513     C                   IF        %Error  or  RSUT = *blanks
048900100513     C                   call      'TIBS34R'
049000100513     C                   parm                    Tibs34Ds
049100100513     C                   in        *dtaara
049200100513     c                   ENDIF
049300950112     C*
049400950112     C                   Z-ADD     1             CODUT
049500950112     C                   CALL      'X§PARUT'
049600950112     C                   PARM                    UT§DSE
049700950112     C                   MOVEL     RAGUT         RSUT             20
049800950112     C                   MOVEL     REC80         CNCR80
049900020328     C*
050000020328     C* apertura workf per ripartizione voci di conto economico
050100020328     c                   open      fiftdw1l
050200950112     C*
050300950112     C* DEFINIZIONE CHIAVI DI ACCESSO
050400100513     C     Kwalc         KLIST
050500100513     C                   KFLD                    WalPGM
050600100513     C                   KFLD                    WalNOJ
050700100513     C                   KFLD                    WalNUS
050800100513     C                   KFLD                    WalNRJ
050900090908      *
051000090908     C     KTbe          KLIST
051100090908     C                   KFLD                    tbeCOD
051200090908     C                   KFLD                    tbeKE1
051300090908     C                   KFLD                    tbeKE2
051400090908      *
051500950112     C     KTAB2         KLIST
051600950112     C                   KFLD                    CODUT
051700950112     C                   KFLD                    COD               2
051800950112     C     KTAB          KLIST
051900950112     C                   KFLD                    CODUT
052000950112     C                   KFLD                    COD               2
052100950112     C                   KFLD                    KEY               8
052200950113     C     KFTT          KLIST
052300950113     C                   KFLD                    VI4PDR
052400950113     C                   KFLD                    VH4TSR
052500950113     C                   KFLD                    VI4NDC
052600950116     C                   KFLD                    VH4DDC
052700950112     C     KFTT1         KLIST
052800950112     C                   KFLD                    W01PDR
052900950112     C                   KFLD                    VI1TSR
053000950112     C                   KFLD                    W01NDC
053100950112     C     KFTT3         KLIST
053200950112     C                   KFLD                    W01PDR
053300950112     C                   KFLD                    VI1TSR
053400060118     C     K21pdr        KLIST
053500060118     C                   KFLD                    VI1fvl
053600060118     C                   KFLD                    W01PDR
053700060118     C     K21pdr1       KLIST
053800060118     C                   KFLD                    VI1fvl
053900060118     C                   KFLD                    W01PDR
054000060118     C                   KFLD                    VI1TSR
054100060118     C                   KFLD                    W01NDC
054200060118     C     K21pdr3       KLIST
054300060118     C                   KFLD                    VI1fvl
054400060118     C                   KFLD                    W01PDR
054500060118     C                   KFLD                    VI1TSR
054600091112     C     Ktgt          KLIST
054700091112     C                   KFLD                    fgtPDR
054800091112     C                   KFLD                    fgtSML
054900091112     C                   KFLD                    fgtPRG
055000950112     C     KFGT          KLIST
055100950112     C                   KFLD                    VI4PDR
055200950112     C                   KFLD                    PARSML
055300950112     C                   KFLD                    VH4TSR
055400950112     C     KFGT2         KLIST
055500950112     C                   KFLD                    VI4PDR
055600950112     C                   KFLD                    PARSML
055700950112     C                   KFLD                    VH4TSR
055800950112     C                   KFLD                    W01CTR
055900040825      *
056000040825     C     KFGT_rif      KLIST
056100040825     C                   KFLD                    rifPDR
056200040825     C                   KFLD                    PARSML
056300040825     C                   KFLD                    VH4TSR
056400040825     C                   KFLD                    rifCTR
056500950112     C     KFTD          KLIST
056600950112     C                   KFLD                    VI4PDR
056700950112     C                   KFLD                    VH4TSR
056800950112     C                   KFLD                    VI4NDC
056900950116     C                   KFLD                    VH4DDC
057000950112     C     KFTD2         KLIST
057100950116     C                   KFLD                    VH6AAS
057200950116     C                   KFLD                    FTDLNP
057300950116     C                   KFLD                    FTDNRS
057400950116     C                   KFLD                    VI6NSP
057500950116     C                   KFLD                    VH4TSR
057600950112     C                   KFLD                    VI4PDR
057700950112     C                   KFLD                    VI4NDC
057800950116     C                   KFLD                    VH4DDC
057900020315     C     KFTD5         KLIST
058000020315     C                   KFLD                    VI4PDR
058100020315     C                   KFLD                    VH4TSR
058200020315     C                   KFLD                    VI4NDC
058300020315     C                   KFLD                    VH4DDC
058400020315     C                   KFLD                    VH6AAS
058500020315     C                   KFLD                    FTDLNP
058600020315     C                   KFLD                    FTDNRS
058700020315     C                   KFLD                    VI6NSP
058800091012     C     Bolla_FTD     KLIST
058900950112     C                   KFLD                    FTDAAS
059000950112     C                   KFLD                    FTDLNP
059100950112     C                   KFLD                    FTDNRS
059200950112     C                   KFLD                    FTDNSP
059300091012      *
059400950112     C     KFTD4         KLIST
059500950112     C                   KFLD                    VI4PDR
059600950112     C                   KFLD                    VH4TSR
059700950112     C                   KFLD                    VI4NDC
059800950116     C                   KFLD                    VH4DDC
059900950112     C                   KFLD                    VI6NSP
060000950116     C                   KFLD                    FTDRSC
060100000622     C     KFTDT         KLIST
060200000622     C                   KFLD                    VI4PDR
060300000622     C                   KFLD                    VH4TSR
060400000714     C                   KFLD                    FT2NDC
060500000622     C                   KFLD                    VH2DDC
060600000714     C     KFTTT         KLIST
060700000714     C                   KFLD                    VI4PDR
060800000714     C                   KFLD                    VI1FVL
060900000714     C                   KFLD                    VH2DDC
061000000714     C                   KFLD                    VH4TSR
061100950112     C     KLBL          KLIST
061200950112     C                   KFLD                    LBLAAN
061300950112     C                   KFLD                    LBLLPN
061400950112     C                   KFLD                    LBLNRN
061500950112     C                   KFLD                    LBLNSN
061600041124      *
061700041124     C     Del_FCE       KLIST
061800041124     C                   KFLD                    fgs_vi4pdr        3 0
061900041124     C                   KFLD                    VI4PDR
062000041124     C                   KFLD                    VH4TSR
062100041124     C                   KFLD                    VI4NDC
062200041124     C                   KFLD                    VH4DDC
062300020326      *
062400020326     c     Kfce          Klist
062500020326     C                   kfld                    ftdfgs
062600020326     C                   kfld                    ftdpdr
062700020326     C                   kfld                    ftdtsr
062800020326     C                   kfld                    ftdndc
062900020326     C                   kfld                    ftdddc
063000020326     C                   kfld                    ftdaas
063100020326     C                   kfld                    ftdlnp
063200020326     C                   kfld                    ftdnrs
063300020326     C                   kfld                    ftdnsp
063400020326      *
063500020326     c     KfceD         Klist
063600020326     C                   kfld                    vh6fgs
063700020326     C                   kfld                    vi4pdr
063800020326     C                   kfld                    vh6tsr
063900020326     C                   kfld                    vi4ndc
064000020326     C                   kfld                    vh4ddc
064100020326     C                   kfld                    vh6aas
064200020326     C                   kfld                    ftdlnp
064300020326     C                   kfld                    ftdnrs
064400020326     C                   kfld                    vh6nsp
064500020328     C*
064600020422     c     KftdW0        Klist
064700020328     C                   kfld                    kftwfgs
064800020328     C                   kfld                    kftwpdr
064900020328     C                   kfld                    kftwtsr
065000020328     C                   kfld                    kftwndc
065100020328     C                   kfld                    kftwddc
065200020422     C*
065300020422     c     KftdW         Klist
065400020422     C                   kfld                    kftwfgs
065500020422     C                   kfld                    kftwpdr
065600020422     C                   kfld                    kftwtsr
065700020422     C                   kfld                    kftwndc
065800020422     C                   kfld                    kftwddc
065900020328     C                   kfld                    kftwstp
066000021202     c     Kapdv         Klist
066100021202     C                   kfld                    apdtip
066200021202     C                   kfld                    vi1p1
066300021202     c     Kapdf         Klist
066400021202     C                   kfld                    apdtip
066500021202     C                   kfld                    fttpdr
066600021202     c                   eval      apdtip = 'A'
066700950112     C***
066800950112     C* 20 ON - PROGRAMMA RICHIAMATO
066900950112     C***
067000950112     C     PARTSR        IFNE      ' '
067100950112     C                   EXSR      PUL01
067200950112     C                   SETON                                        20
067300950112     C                   MOVEL     'V'           VI1FVL
067400950112     C                   MOVEL     PARPDR        VI1P1
067500950112     C                   MOVEL     PARTSR        VI1TSR
067600950112     C                   ENDIF
067700950112     C***
067800950112     C* 06 ON - FASE DI SIMULAZIONE
067900950112     C***
068000950112     C     PARSML        IFEQ      'S'
068100950112     C                   SETON                                        06
068200950112     C                   ENDIF
068300950112     C*
068400950112     C***
068500950112     C* CARICO TABELLA FILIALI GESTITE £1
068600950112     C***
068700950112     C                   CLEAR                   DSUL06
068800950112     C                   MOVE      '£1'          D06COD
068900020717     C                   MOVEL     SIMPOU        D06KEY
069000950112     C                   MOVEL     ' '           D06TLA
069100100513     C                   MOVEL     KPJBU         kpjbus
069200100513     C                   clear                   kpjbu
069300950112     C                   MOVEL     DSUL06        KPJBU
069400950112     C                   CALL      'TRUL06R'
069500950112     C                   PARM                    KPJBA
069600950112     C                   MOVEL     KPJBU         DSUL06
069700950112     C                   MOVEA     LIN           L1
069800100513     C                   MOVEL     KPJBUs        kpjbu
069900950112     C***
070000950112     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
070100950112     C***
070200950112     C                   CLEAR                   DSUL06
070300950112     C                   MOVE      '£6'          D06COD
070400020717     C                   MOVEL     SIMPOU        D06KEY
070500950112     C                   MOVEL     'L'           D06TLA
070600100513     C                   MOVEL     KPJBU         kpjbus
070700100513     C                   clear                   kpjbu
070800950112     C                   MOVEL     DSUL06        KPJBU
070900950112     C                   CALL      'TRUL06R'
071000950112     C                   PARM                    KPJBA
071100950112     C                   MOVEL     KPJBU         DSUL06
071200950112     C                   MOVEA     LIN           L6
071300100513     C                   MOVEL     KPJBUs        kpjbu
071400950112     C*
071500950112     C***
071600950112     C* CARICO TABELLA DEI TIPI PRESTAZIONE
071700950112     C***
071800950112     C                   Z-ADD     1             C
071900950112     C                   MOVEL     '1Z'          COD
072000950112     C     KTAB2         SETLL     TABEL
072100950112     C     KTAB2         READE     TABEL                                  30
072200950112     C*
072300950112     C     *IN30         DOWEQ     *OFF
072400950112     C     TBLFLG        IFEQ      ' '
072500950112     C                   MOVEL     TBLUNI        DS1Z
072600950112     C     §1ZUPD        IFEQ      'S'
072700950112     C                   MOVEL     TBLKEY        TSR(C)
072800950112     C                   MOVEL     §1ZDES        DTS(C)
072900950112     C                   MOVEL     §1ZCTR        CTS(C)
073000950112     C                   ADD       1             C
073100950112     C                   ENDIF
073200950112     C                   ENDIF
073300950112     C     KTAB2         READE     TABEL                                  30
073400950112     C                   ENDDO
073500950112     C***
073600950112     C* CARICO MANCATE CONSEGNE DA TASSARE
073700950112     C***
073800950112     C                   Z-ADD     1             C
073900950112     C                   MOVEL     '8B'          COD
074000950112     C     KTAB2         SETLL     TABEL
074100950112     C     KTAB2         READE     TABEL                                  30
074200950112     C*
074300950112     C     *IN30         DOWEQ     *OFF
074400950112     C     TBLFLG        IFEQ      ' '
074500950112     C                   MOVEL     TBLKEY        CMC(C)
074600950112     C                   MOVE      TBLKEY        WALF3             3
074700950112     C                   MOVE      WALF3         CMC(C)
074800950112     C                   ADD       1             C
074900950112     C                   ENDIF
075000950112     C     KTAB2         READE     TABEL                                  30
075100950112     C                   ENDDO
075200950112     C***
075300950112     C* CARICO I TIPI BOLLA DA NON TASSARE
075400950112     C***
075500950112     C                   Z-ADD     1             C
075600950112     C                   MOVEL     '8C'          COD
075700950112     C     KTAB2         SETLL     TABEL
075800950112     C     KTAB2         READE     TABEL                                  30
075900950112     C*
076000950112     C     *IN30         DOWEQ     *OFF
076100950112     C     TBLFLG        IFEQ      ' '
076200950112     C                   MOVEL     TBLKEY        CBL(C)
076300950112     C                   MOVE      TBLKEY        WALF3             3
076400950112     C                   MOVE      WALF3         CBL(C)
076500950112     C                   ADD       1             C
076600950112     C                   ENDIF
076700950112     C     KTAB2         READE     TABEL                                  30
076800950112     C                   ENDDO
076900090908      *----
077000090909     C                   Z-ADD     0             C
077100090908     C                   movel(P)  'CTP'         tbeCOD
077200090908     c                   clear                   KCliPar
077300090908     c                   clear                   KtutteBolle
077400090908     C     tbeCOD        setll     tntbe01l
077500090908     C     tbeCOD        reade     tntbe01l
077600090908      * consegne/ritiri
077700090908     c                   dow       not %eof(tntbe01l)
077800090909     C                   ADD       1             C
077900090908     C                   movel     tbeuni        dCTP
078000090908     C                   MOVEL     TBeKE1        KCliPar(C)
078100090908     C     tbeCOD        reade     tntbe01l
078200090908     c                   enddo
078300950112     C***
078400950112     C* DEFINIZIONE CAMPI
078500950112     C***
078600950112     C     *LIKE         DEFINE    TBLKEY        §KEY
078700950112     C     *LIKE         DEFINE    TBLCOD        §COD
078800950112     C     *LIKE         DEFINE    TBLKUT        §KUT
078900950112     C* AAMMGG DATA IMMESSA A VIDEO
079000950112     C     *LIKE         DEFINE    FTTDDC        W01DDC
079100950127     C     *LIKE         DEFINE    VI4DDC        WM1DDC
079200950112     C* DATA DI ELABORAZIONE (PUO' ESSERECENE SOLO UNA)
079300950112     C     *LIKE         DEFINE    FTTDDC        W02DDC
079400020129     C     *like         define    fttndc        w02ndc
079500011112     C     *like         define    fttdiv        w02div
079600950112     C     *LIKE         DEFINE    FTTNDC        W01NDC
079700950112     C     *LIKE         DEFINE    FTTPDR        W01PDR
079800011105      *
079900950112     C     *LIKE         DEFINE    FTTPDR        W02PDR
080000950112     C     *LIKE         DEFINE    FTTTSR        W01TSR
080100950112     C     *LIKE         DEFINE    FTTSNA        W01SNA
080200020212     C     *LIKE         DEFINE    FTTSNA        g01SNA
080300950112     C     *LIKE         DEFINE    FTTKFA        W01KFA
080400020212     C     *LIKE         DEFINE    FTTKFA        g01KFA
080500950112     C     *LIKE         DEFINE    FTTCLA        W01CLA
080600020212     C     *LIKE         DEFINE    FTTCLA        g01CLA
080700950118     C     *LIKE         DEFINE    FTTCPE        W01CPE
080800020212     C     *LIKE         DEFINE    FTTCPE        g01CPE
080900950112     C     *LIKE         DEFINE    FTTTVL        W01TVL
081000020212     C     *LIKE         DEFINE    FTTTVL        g01TVL
081100950112     C     *LIKE         DEFINE    FTTITA        W01ITA
081200950112     C     *LIKE         DEFINE    FTTITT        W01ITT
081300950112     C     *LIKE         DEFINE    FTTFIT        W01FIT
081400950112     C     *LIKE         DEFINE    FTTFIA        W01FIA
081500950119     C     *LIKE         DEFINE    FTTSNE        W01SNE
081600950112     C     *LIKE         DEFINE    FTTSNP        W01SNP
081700950112     C     *LIKE         DEFINE    FTDNCL        W01NCL
081800950112     C     *LIKE         DEFINE    FTDSTP        W01STP
081900950112     C     *LIKE         DEFINE    FTDSTP        W02STP
082000950112     C     *LIKE         DEFINE    FTDSPC        W01SPC
082100950112     C     *LIKE         DEFINE    FTDSPR        W01SPR
082200950112     C     *LIKE         DEFINE    NRR           W01NRR
082300950112     C     *LIKE         DEFINE    NRR           W04NRR
082400950112     C     *LIKE         DEFINE    NRR           W06NRR
082500950112     C     *LIKE         DEFINE    NRR           NRR4
082600950112     C     *LIKE         DEFINE    NRR           NRR6
082700011105     C     *LIKE         DEFINE    NRR           NRW6
082800950112     C     *LIKE         DEFINE    FGTCTR        W01CTR
082900950112     C     *LIKE         DEFINE    FTDCTP        W01CTP
083000020325     C     *like         define    ftdFIN        w01isola
083100020325     C     *like         define    arbtsp        w01xpres
083200020328     C     *like         define    ftwfgs        kftwfgs
083300020328     C     *like         define    ftwpdr        kftwpdr
083400020328     C     *like         define    ftwtsr        kftwtsr
083500020328     C     *like         define    ftwndc        kftwndc
083600020328     C     *like         define    ftwddc        kftwddc
083700020328     C     *like         define    ftwstp        kftwstp
083800020422     C*
083900020422     C     *like         define    ftdfgs        sv1fgs
084000020422     C     *like         define    ftdpdr        sv1pdr
084100020422     C     *like         define    ftdtsr        sv1tsr
084200020422     C     *like         define    ftdndc        sv1ndc
084300020422     C     *like         define    ftdddc        sv1ddc
084400020328     C*
084500030821     C     *like         define    §t1NSS        W01nss
084600030821     C     *like         define    §t1FW         W01fw
084700030821     C     *like         define    §t1RCC        W01rcc
084800030821     C     *like         define    ftdSTP        savSTP
084900030821     c     *like         define    W01nss        SAVnss
085000030821     c     *like         define    W01fw         SAVfw
085100030821     c     *like         define    W01rcc        SAVrcc
085200030821     C*
085300950112     C                   Z-ADD     0             B                 2 0
085400040112     C                   Z-ADD     0             C                 5 0
085500950112     C                   TIME                    UTIME             6 0
085600090511     C
085700040622     C***
085800040622     C                   TIME                    W0140            14 0
085900040622     C                   MOVEl     W0140         Wora              6 0
086000040622     C                   MOVE      W0140         WDAT              8 0
086100040622     C*
086200040622     C                   Z-ADD     Wdat          G02DAT
086300040622     C                   MOVEL     *BLANK        G02ERR
086400040622     C                   CALL      'XSRDA8'
086500040622     C                   PARM                    WLBDAT
086600040622     C* UDATE A 8 IN AAAA/MM/GG
086700040622     C                   move      G02INV        dataoggi
086800090511      **
086900100513     C* scrive il record di allocazione x trasmissione
087000100513     c     parsml        ifeq      *blank
087100100513     C                   clear                   Fiwalc00
087200100513     C                   movel     w0140         walOrc
087300100513     C                   movel     g02inv        waldac
087400100513     C                   eval      WalSIF = KNSIF
087500100513     C                   eval      WalPGM = 'FICNB1R   '
087600100513     C                   eval      WalNOJ = KNMEB
087700100513     C                   eval      WalNUS = KNMUS
087800100513     C                   eval      WalNRJ = KNRJO
087900100513     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
088000100513     C                   write     Fiwalc00
088100100513     c                   feod      Fiwalc1L
088200100513     c                   end
088300090511     C***
088400950112     C                   ENDSR
088500020212     C*---------------------------------------------------------------*
088600931103     C* PULISCO 1 FORMATO --------------------------------------------*
088700020212     C*---------------------------------------------------------------*
088800931103     C     PUL01         BEGSR
088900931103     C                   CLEAR                   NDC
089000931103     C                   CLEAR                   VI1P1
089100931103     C                   CLEAR                   VI1P2
089200931103     C                   CLEAR                   VI1DDC
089300931103     C                   CLEAR                   VI1FVL
089400931103     C                   CLEAR                   VI1TSR
089500020717     C                   Z-ADD     SIMPOU        VI1PF1
089600950112     C                   ENDSR
089700020212     C*---------------------------------------------------------------*
089800900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
089900020212     C*---------------------------------------------------------------*
090000900528     C     CONTR1        BEGSR
090100931103     C                   SETOFF                                       90
090200931103     C                   CLEAR                   W01DDC
090300931103     C                   CLEAR                   W02DDC
090400020129     C                   clear                   w02ndc
090500011112     C                   clear                   w02div
090600931119     C* T I P O   P R E S T A Z I O N E
090700931021    1C     VI1TSR        IFNE      ' '
090800931021    2C     VI1TSR        IFEQ      '?'
090900931015     C                   MOVEL     CODUT         §KUT
091000931015     C                   MOVEL     '1Z'          §COD
091100931015     C                   MOVE      ' '           VI1TSR
091200931015     C                   MOVEL     *BLANKS       §KEY
091300931015     C                   CALL      'X§TABER'
091400931015     C                   PARM                    §KUT
091500931015     C                   PARM                    §COD
091600931015     C                   PARM                    §KEY
091700931015     C                   PARM                    §DES             30
091800931015     C                   MOVEL     §KEY          VI1TSR
091900931015     C                   SETON                                        90
092000931015     C                   GOTO      ENDCT1
092100931021    2C                   ENDIF
092200931018     C*
092300931015     C* 30 ON - INESISTENTE O ANNULLATO
092400931018     C     VI1TSR        LOOKUP    TSR                                    30
092500931018     C  N30              SETON                                        4090
092600931018     C  N30              GOTO      ENDCT1
092700931021    1C                   ENDIF
092800931015     C*
092900950112     C* F I L I A L E   P A D R O N C I N O
093000950112     C     VI1PF1        IFEQ      0
093100950112     C                   SETON                                        4190
093200950119     C                   GOTO      ENDCT1
093300950112     C                   ELSE
093400020717     C* Verifico esistenza filiale padroncino: se 2° LIV. su £6, se 1° LIV.
093500950112     C* su £1
093600020717     C                   IF        SIMTPP = '2'
093700950112     C     VI1PF1        LOOKUP    L6                                     30
093800950112     C  N30              SETON                                        4190
093900950119     C  N30              GOTO      ENDCT1
094000950112     C                   ELSE
094100950112     C     VI1PF1        LOOKUP    L1                                     30
094200950112     C  N30              SETON                                        4190
094300950119     C  N30              GOTO      ENDCT1
094400950112     C                   END
094500950112     C                   END
094600931018     C*
094700931018     C* P A D R O N C I N O   DAL AL
094800931110     C*
094900931021    1C     VI1PD2        IFGT      0
095000931119     C     VI1PD2        ANDNE     VI1PD1
095100931119     C*
095200931021    2C     VI1PD2        IFLT      VI1PD1
095300931015     C                   SETON                                        4290
095400090508     C                   GOTO      ENDCT1
095500931021    2C                   ENDIF
095600931015     C*
095700931021   X1C                   ELSE
095800931018     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
095900931021    2C     VI1PD1        IFGT      0
096000931110     C*
096100021202     C     kapdv         CHAIN     fiapd01L                           30
096200950123     C     *IN30         IFEQ      *ON
096300931015     C                   SETON                                        30
096400931021    3C                   ENDIF
096500931015     C*
096600931015     C   30              SETON                                        4390
096700931015     C   30              GOTO      ENDCT1
096800931021    2C                   ENDIF
096900931021    1C                   ENDIF
097000931018     C*
097100931018     C* D A T A   D I S T I N T A
097200931021    1C     VI1DDC        IFGT      0
097300950112     C                   Z-ADD     VI1DDC        G02DAT
097400931018     C                   MOVEL     *BLANK        G02ERR
097500950112     C                   CALL      'XSRDA8'
097600931018     C                   PARM                    WLBDAT
097700931021    2C     G02ERR        IFEQ      '1'
097800931018     C                   SETON                                        44  90
097900931018     C                   GOTO      ENDCT1
098000931021    2C                   END
098100931018     C                   Z-ADD     G02INV        W01DDC
098200931026    1C                   ENDIF
098300950113     C* SE IMMESSO UN SOLO NUMERO DISTINTA IMMETTERE ANCHE IL TIPO SERV
098400931018     C                   SORTA     NDC
098500931026    1C     NDC(10)       IFGT      0
098600931018     C     NDC(9)        ANDEQ     0
098700931018     C     VI1TSR        ANDEQ     ' '
098800931018     C                   SETON                                        45  90
098900931018     C                   GOTO      ENDCT1
099000931026    1C                   ENDIF
099100910320     C*
099200950112     C  N90              MOVEL     VI1PF1        VI1PF2
099300950112     C*
099400900614     C     ENDCT1        ENDSR
099500020212     C*---------------------------------------------------------------*
099600931018     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
099700020212     C*---------------------------------------------------------------*
099800931018     C     CARNDC        BEGSR
099900950113     C                   SETOFF                                       0102
100000931018     C                   SETOFF                                       040531
100100931105     C                   SETOFF                                       21
100200931018     C                   Z-ADD     0             NRR               5 0
100300931018     C                   Z-ADD     0             W01NRR
100400931018     C                   CLEAR                   VI1VAL
100500931119     C                   CLEAR                   WDATA
100600931018     C* PULIZIA SUBFILE
100700931018     C                   SETON                                        35
100800931018     C                   WRITE     FRB8CT2
100900931018     C                   SETOFF                                       35
101000931018     C*
101100931018     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
101200931018     C* 02 ON - IMMESSI PADRONCINI DAL AL
101300931108     C* 04 ON - IMMESSO TIPO PRESTAZIONE
101400950113     C* 05 ON - IMMESSO SOLO UN NUMERO DISTINTA (CONSEGNE)
101500931018     C*
101600931108     C* TIPO PRESTAZ SELEZIONATO
101700931018     C     VI1TSR        IFNE      ' '
101800931018     C                   SETON                                        04
101900931018     C                   ENDIF
102000931018     C* PADRONCINO
102100931018     C     VI1PD2        IFGT      0
102200931018     C     VI1P2         ANDNE     VI1P1
102300931018     C                   SETON                                        02
102400931018     C                   ELSE
102500931018     C*
102600931018     C     VI1PD1        IFGT      0
102700931018     C                   SETON                                        01
102800950113     C* IMMESSO SOLO 1 NUMERO DISTINTA E TIPO SERIVZIO = 'C'
102900931018     C     NDC(10)       IFGT      0
103000931018     C     NDC(9)        ANDEQ     0
103100950113     C     VI1TSR        ANDEQ     'C'
103200931018     C                   SETON                                        05
103300931018     C                   MOVEL     NDC(10)       W01NDC
103400931018     C                   ENDIF
103500931018     C*
103600931018     C                   ENDIF
103700931018     C                   ENDIF
103800931018     C                   Z-ADD     VI1P1         W01PDR
103900931018     C* NUMERI DISTINTA
104000931018     C                   XFOOT     NDC           WNUM9             9 0
104100931018     C* FLAG VALORIZZAZIONE
104200931018     C     VI1FVL        IFEQ      ' '
104300931018     C                   MOVEL     CDAVAL        VI1VAL
104400931018     C                   ELSE
104500931018     C                   MOVEL     CVALOR        VI1VAL
104600931018     C                   ENDIF
104700921229     C**
104800931018     C* POSIZIONAMENTO PER LETTURA
104900931018     C**
105000931018     C                   SELECT
105100060118      **
105200931108     C** NON IMMESSO TIPO PRESTAZ: LEGGO SOLO PER PADRONCINO
105300931018     C     *IN04         WHENEQ    *OFF
105400931018     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
105500931018     C     VI1PD1        IFGT      0
105600060118     C     k21pdr        SETLL     fiftt21l
105700060118     C**** W01PDR        SETLL     FIFTT000
105800931018     C                   ELSE
105900931018     C                   EXSR      IMPO1
106000931018     C                   ENDIF
106100931018     C*
106200931018     C                   OTHER
106300950113     C** 01 ON  - LEGGO PER PADRONCINO/TIPO SERVIZIO/NUMERO/DATA DISTIN
106400950113     C**              O PER PADRONCINO/TIPO SERVIZIO
106500950113     C** 01 OFF - LEGGO PER PADRONCINO (TUTTI O NEL DAL-AL)/TIPO SERVIZ
106600060118      **
106700931018     C     *IN01         IFEQ      *ON
106800060118     C   05K21pdr1       CHAIN(N)  fiftt21l                           31
106900060118     C** 05KFTT1         CHAIN(N)  FIFTT000                           31
107000060118     C  N05K21pdr3       SETLL     fiftt21l
107100060118     C**N05KFTT3         SETLL     FIFTT000
107200060118      **
107300931018     C                   ELSE
107400950113     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
107500950113     C     VI1PD1        IFGT      0
107600060118     C     k21PDR        SETLL     fiftt21l
107700060118     C**** W01PDR        SETLL     FIFTT000
107800950113     C                   ELSE
107900931018     C                   EXSR      IMPO1
108000931018     C                   ENDIF
108100060118      **
108200950113     C                   ENDIF
108300060118      **
108400931018     C                   ENDSL
108500931018     C*
108600931018     C* LETTURA DISTINTE/RITIRI
108700931018     C     *IN31         IFEQ      *OFF
108800931018     C                   EXSR      LEGNDC
108900931018     C                   ELSE
109000931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
109100931018     C                   SETON                                        4690
109200931119     C                   MOVEL     ERR(1)        VI1ERR
109300030519      * solo in simulazione: (eccezione)
109400030519      *  se non c'è la tariffa non carica le distinte
109500030519      *  quindi non è valido il messaggio precedente
109600030519     c     parsml        ifeq      'S'
109700030519     c     §t3ctr        andeq     '???'
109800030519     C                   clear                   VI1ERR
109900030519     c     §t3tsr        ifeq      'C'
110000030519     C                   MOVEL     ERR(2)        VI1ERR
110100030519     c                   end
110200030519     c     §t3tsr        ifeq      'R'
110300030519     C                   MOVEL     ERR(3)        VI1ERR
110400030519     c                   end
110500030519     c                   end
110600931018     C                   ENDIF
110700931018     C*
110800900608     C                   ENDSR
110900020212     C*---------------------------------------------------------------*
111000931018     C* IMPOSTAZIONE LETTURA PER 04 OFF ------------------------------*
111100020212     C*---------------------------------------------------------------*
111200931018     C     IMPO1         BEGSR
111300060118      **
111400060118     C     k21PDR        setgt     fiftt21l
111500060118     C**** W01PDR        SETGT     FIFTT000
111600060118     C                   read(N)   fiftt21l                               31
111700060118     C*****              READ(N)   FIFTT000                               31
111800950202     C*
111900950202     C     *IN31         IFEQ      *OFF
112000950202     C                   MOVEL     FTTPDR        W03               3 0
112100950202     C     VI1PF1        IFNE      W03
112200950202     C                   SETON                                        31
112300950202     C                   END
112400950202     C                   END
112500931018     C*
112600931018     C     *IN31         IFEQ      *OFF
112700931018     C                   Z-ADD     FTTPDR        W01PDR
112800060118     C  N04k21pdr        setll     fiftt21l
112900060118     C**N04W01PDR        SETLL     FIFTT000
113000060118     C   04K21pdr3       setll     fiftt21l
113100060118     C** 04KFTT3         SETLL     FIFTT000
113200931018     C                   ENDIF
113300060118      **
113400931018     C                   ENDSR
113500020212     C*---------------------------------------------------------------*
113600931018     C* LETTURA DITINTE DA CARICARE ----------------------------------*
113700020212     C*---------------------------------------------------------------*
113800931018     C     LEGNDC        BEGSR
113900931018     C                   CLEAR                   W01TSR
114000931018     C                   CLEAR                   W02PDR
114100931018     C* L E T T U R A
114200940325    1C     *IN31         DOWEQ     *OFF
114300931018     C                   SETOFF                                       9032
114400931018     C                   SELECT
114500931018     C** LEGGO PER PADRONCINO (TUTTO O NEL DAL-AL)
114600940325    2C     *IN04         WHENEQ    *OFF
114700931018     C*
114800940325    3C     *IN32         DOWEQ     *OFF
114900060118     C     k21pdr        READE(N)  fiftt21l                               31
115000060118     C**** W01PDR        READE(N)  FIFTT000                               31
115100931018     C*
115200940325    4C     *IN31         IFEQ      *ON
115300931018     C     *IN01         ANDEQ     *OFF
115400931018     C                   EXSR      IMPO1
115500931018     C   31              SETON                                        32
115600931018     C                   ELSE
115700931018     C                   SETON                                        32
115800940325    4C                   ENDIF
115900940325    3C                   ENDDO
116000931108     C** LEGGO TUTTI I PADRONCINI(O NEL DAL-AL) E TIPO PRESTAZIONE
116100940325    2C     *IN01         WHENEQ    *OFF
116200931018     C*
116300940325    3C     *IN32         DOWEQ     *OFF
116400060118     C     K21pdr3       READE(N)  fiftt21l                               31
116500060118     C**** KFTT3         READE(N)  FIFTT000                               31
116600931018     C*
116700940325    4C     *IN31         IFEQ      *ON
116800931018     C                   EXSR      IMPO1
116900931018     C   31              SETON                                        32
117000931018     C                   ELSE
117100931018     C                   SETON                                        32
117200940325    4C                   ENDIF
117300940325    3C                   ENDDO
117400060118      *
117500931108     C** LEGGO PER PADRONCINO TIPO PRESTAZIONE
117600940325    2C     *IN01         WHENEQ    *ON
117700931018     C     *IN05         ANDEQ     *OFF
117800060118     C     K21pdr3       READE(N)  fiftt21l                               31
117900060118     C**** KFTT3         READE(N)  FIFTT000                               31
118000931018     C*
118100931018     C                   ENDSL
118200931018     C*
118300931018     C* C O N F R O N T O   L  E   S E L E Z I O N I
118400020606     C     *IN31         IFEQ      *OFF
118500020606     C                   EXSR      CONFR
118600931018     C*
118700931018     C* C A R I C A M E N T O   S F L
118800900608     C* 90 ON - RECORD DA NON SELEZIONARE
118900020606     C     *IN90         ifeq      *OFF
119000931018     C*
119100931018     C                   CLEAR                   VI2SCE
119200011105     C                   clear                   vh2div
119300931028     C                   CLEAR                   VH2ITT
119400931028     C                   CLEAR                   VH2ITA
119500931018     C*
119600931018     C     FTTPDR        IFNE      W02PDR
119700020524     C                   CLEAR                   saltasf2          1
119800931103     C                   SETOFF                                       12
119900931103     C                   MOVEL     '0'           VH2I12
120000021202     C     kapdf         CHAIN     fiapd01L                           30
120100950117     C     *IN30         IFEQ      *OFF
120200100223     C                   MOVEL     APDRSf        VI2DPD
120300950117     C                   MOVEL     APDETM        VH2ETM
120400950117     C                   MOVEL     APDETP        VH2ETP
120500950117     C                   MOVEL     APDPKM        VH2PKM
120600950117     C                   MOVEL     APDPKP        VH2PKP
120700021211     C                   eval      vh2cam = apdCAM
120800021211     C                   eval      vh2cap = apdCAP
120900021211     C                   eval      vh2scm = apdSCM
121000021211     C                   eval      vh2scp = apdSCP
121100021211     C                   z-add     apdHAR        vh2HAR
121200021211     C                   z-add     apdHPA        vh2HPA
121300020524     C                   movel     APDpdd        saltasf2
121400950117     C                   ELSE
121500950117     C                   CLEAR                   VI2DPD
121600950117     C                   CLEAR                   VH2ETM
121700950117     C                   CLEAR                   VH2ETP
121800950117     C                   CLEAR                   VH2PKM
121900950117     C                   CLEAR                   VH2PKP
122000021211     C                   clear                   vh2CAM
122100021211     C                   clear                   vh2CAP
122200021211     C                   clear                   vh2SCM
122300021211     C                   clear                   vh2HAR
122400021211     C                   clear                   vh2HPA
122500021211     C                   clear                   vh2SCP
122600950117     C                   END
122700931018     C                   MOVEL     FTTPDR        VI2PDR
122800931018     C                   MOVEL     FTTPDR        W02PDR
122900931018     C                   ELSE
123000931103     C                   SETON                                        12
123100931103     C                   MOVEL     '1'           VH2I12
123200931018     C                   ENDIF
123300931108     C* TIPO PRESTAZIONE
123400931018     C     FTTTSR        IFNE      W01TSR
123500931018     C                   MOVEL     FTTTSR        K17(1)
123600931018     C                   MOVEL     '-'           K17(2)
123700931018     C                   Z-ADD     1             C
123800931018     C     FTTTSR        LOOKUP    TSR(C)                                 30
123900931018     C   30              MOVEA     DTS(C)        K17(3)
124000931018     C  N30              MOVEA     *BLANKS       K17(3)
124100931018     C*
124200931018     C                   MOVEA     K17           VI2TSR
124300931018     C                   MOVEL     FTTTSR        W01TSR
124400931018     C                   ENDIF
124500931018     C*
124600931119     C                   MOVEL     FTTNDC        VI2NDC                          NUMERO
124700950113     C                   MOVE      FTTDDC        DATA
124800950113     C                   MOVE      FTTDDC        VI2DDC                         DATA
124900931119     C                   MOVE      AA            VI2DDC
125000950113     C                   MOVEL     GG            VI2DDC
125100931119     C                   MOVEL     FTTDDC        VH2DDC
125200931027     C     FTTFIT        IFNE      ' '
125300011105     C                   Z-ADD     FTTITT        VH2ITT
125400931027     C                   ENDIF
125500931027     C     FTTFIA        IFNE      ' '
125600931027     C                   Z-ADD     FTTITA        VH2ITA
125700931027     C                   ENDIF
125800011105     C                   movel     fttdiv        vh2div
125900950117     C                   MOVEL     FTTFPP        VH2FPP
126000950117     C                   MOVEL     FTTPEP        VH2PEP
126100021211     C                   eval      vh2FCS = fttLVL
126200021217     C                   z-add     fttTBS        vh2TBS
126300900608     C*
126400020603     C*  Prova a cercare se in simulazione la tariffa legata al documento
126500020603     c     parsml        ifeq      'S'
126600030516     C*
126700020603     C* Routine esterna per restituire il codice tariffa del documento
126800020603     C                   clear                   ficnb3
126900030516     C*
127000030516     C* Attenzione se ci sono dei Ritiri Annullati, deve impostare il parametro
127100030516     C*  RAN con (S)
127200030516     c                   if        ftttsr ='R' and ftttbs > 0
127300030516     C                   movel     'S'           §t3RAN
127400030516     c                   end
127500020603     C                   movel     fttpdr        §t3pdr
127600020603     C                   movel     parsml        §t3sml
127700020603     C                   movel     ftttsr        §t3tsr
127800020603     C                   z-add     fttndc        §t3ndc
127900020603     C                   z-add     fttddc        §t3ddc
128000020603     C                   movel     fttdiv        §t3div
128100020603     C                   movel     *blank        §t3ctr
128200020605     C                   movel     vi1fvl        §t3fvl
128300020603     C                   CALL      'FICNB3R1'
128400020603     C                   PARM                    ficnb3
128500020603      *
128600020603     c                   endif
128700020603     C*
128800020524     C* se il padroncino è stato escluso dalla valorizzazione
128900020524     C* comunque in simulazione deve sempre permettere la valorizzazione
129000020603     c                   move      'S'           nowrite           1
129100020603      *
129200020603     c     saltasf2      ifeq      'S'
129300020603     c     parsml        andeq     *blank
129400020603      *
129500020524     c     parsml        oreq      'S'
129600020603     c     §t3ctr        andeq     '???'
129700020603     c     saltasf2      andeq     'S'
129800020603     c                   move      'N'           nowrite
129900020603     c                   endif
130000020524      *
130100020524     C* questo da quando si è voluto attivare le simulazioni per quei
130200020524     C* padroncini che non devono avere l'autofatturazione ma che occorre
130300020524     C* simulare comunque.
130400020524      *
130500020603     c                   if        nowrite ='S'
130600020606     C*
130700020606     C* CONTROLLO QUANTE DATE CARICO
130800020606    1C     W02DDC        IFEQ      0
130900020606     C                   Z-ADD     FTTDDC        W02DDC
131000020606     C                   z-add     fttndc        w02ndc
131100020606     C                   movel     fttdiv        w02div
131200020606   X1C                   ELSE
131300020606     C* PRESENTI ALMENO 2 DATE
131400020606    2C     FTTDDC        IFNE      W02DDC
131500020606     C                   MOVEL     '1'           WDATA             1
131600020606    2C                   ENDIF
131700020606    1C                   ENDIF
131800020606     C*
131900900608     C                   ADD       1             NRR
132000931018     C                   WRITE     FRB8SF2
132100020603     c                   endif
132200020524      *
132300931018     C                   ENDIF
132400020606      *
132500020606     C                   ENDIF
132600940325     C*
132700940325     C* ESSENDO CHAIN SECCA, DOPO AVER LETTO UNA VOLTA ESCO
132800940325     C   01
132900940325     CAN 05              SETON                                        31
133000931018     C                   ENDDO
133100900521     C*
133200931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
133300931119     C     NRR           IFEQ      0
133400931018     C                   SETON                                        4690
133500931119     C                   MOVEL     ERR(1)        VI1ERR
133600030519      * solo in simulazione: (eccezione)
133700030519      *  se non c'è la tariffa non carica le distinte
133800030519      *  quindi non è valido il messaggio precedente
133900030519     c     parsml        ifeq      'S'
134000030519     c     §t3ctr        andeq     '???'
134100030519     C                   clear                   VI1ERR
134200030519     c     §t3tsr        ifeq      'C'
134300030519     C                   MOVEL     ERR(2)        VI1ERR
134400030519     c                   end
134500030519     c     §t3tsr        ifeq      'R'
134600030519     C                   MOVEL     ERR(3)        VI1ERR
134700030519     c                   end
134800030519     c                   end
134900931018     C                   ELSE
135000931018     C                   Z-ADD     NRR           W01NRR
135100931119     C                   SETOFF                                       90
135200931018     C                   ENDIF
135300900509     C*
135400900518     C                   ENDSR
135500020212     C*---------------------------------------------------------------*
135600931018     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
135700020212     C*---------------------------------------------------------------*
135800931018     C     CONFR         BEGSR
135900931108     C* TIPO PRESTAZIONE
136000931105     C   04FTTTSR        IFNE      VI1TSR
136100931105     C                   SETON                                        90
136200931105     C                   GOTO      ENDCON
136300931105     C                   ENDIF
136400931105     C* FLAG VALORIZZAZIONE
136500931105     C     FTTFVL        IFNE      VI1FVL
136600931105     C                   SETON                                        90
136700931105     C                   GOTO      ENDCON
136800931105     C                   ENDIF
136900931018     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
137000931018     C   02FTTPDR        IFGT      VI1P2
137100931018     C                   SETON                                        3190
137200931018     C                   GOTO      ENDCON
137300931018     C                   ENDIF
137400931018     C* DATA DISTINTA
137500931018     C     VI1DDC        IFGT      0
137600931018     C     FTTDDC        ANDGT     W01DDC
137700931018     C                   SETON                                        90
137800931018     C                   GOTO      ENDCON
137900931018     C                   ENDIF
138000931028     C*
138100931028     C* PGM RICHIAMATO: CONTROLLO DATA DISTINTA
138200931028     C   20FTTDDC        IFNE      PARDDC
138300931028     C                   SETON                                        90
138400931028     C                   GOTO      ENDCON
138500931028     C                   ENDIF
138600950113     C* NUMERI DISTINTA O RITIRO E SCELTO UN SOLO NUMERO DISTINTA
138700931018     C     WNUM9         IFGT      0
138800950113     C     NDC(10)       ORGT      0
138900950113     C     NDC(9)        ANDEQ     0
139000950113     C     VI1TSR        ANDEQ     'R'
139100931018     C     FTTNDC        LOOKUP    NDC                                    30
139200931018     C  N30              SETON                                        90
139300931018     C  N30              GOTO      ENDCON
139400931018     C                   ENDIF
139500931018     C*
139600931018     C     ENDCON        ENDSR
139700020212     C*---------------------------------------------------------------*
139800931018     C* SELEZIONE DISTINTE DA VALORIZZARE ----------------------------*
139900020212     C*---------------------------------------------------------------*
140000931018     C     SELNDC        BEGSR
140100931105     C                   SETOFF                                       90
140200931018     C                   Z-ADD     1             NRR
140300931018     C                   Z-ADD     0             NRR4
140400931018     C                   Z-ADD     0             W04NRR
140500931018     C                   CLEAR                   W02PDR
140600931018     C                   CLEAR                   VI4SCE
140700931119     C* PRESENTI PIU' DATE
140800931119    1C     WDATA         IFNE      ' '
140900931119     C* SE CMD1 ERRORE
141000931119    2C     *INKA         IFEQ      *ON
141100931119     C                   Z-ADD     1             REC2
141200931119     C                   SETON                                        4090
141300931119     C                   GOTO      ENDSEL
141400931119     C*
141500931119   X2C                   ELSE
141600931119     C*
141700931119     C* SE ENTER CONTROLLO LE SELEZIONI FATTE
141800931123     C                   CLEAR                   W02DDC
141900020129     C                   clear                   w02ndc
142000011112     C                   clear                   w02div
142100931119     C                   SETON                                        36
142200931119     C                   READC     FRB8SF2                                30
142300931119     C*
142400931119    3C     *IN30         DOWEQ     *OFF
142500931122     C*
142600931122    4C     VI2SCE        IFEQ      '1'
142700931122    5C     W02DDC        IFEQ      0
142800931122     C                   MOVEL     VH2DDC        W02DDC
142900020129     C                   z-add     vi2ndc        w02ndc
143000011112     C                   movel     vh2div        w02div
143100931122   X5C                   ELSE
143200931122    6C     VH2DDC        IFNE      W02DDC
143300931119     C                   Z-ADD     NRR           REC2
143400931119     C                   SETON                                        409030
143500931122    6C                   ENDIF
143600931122    5C                   ENDIF
143700931122    4C                   ENDIF
143800931119     C*
143900931119    4C     VH2I12        IFEQ      '1'
144000931119     C                   SETON                                        12
144100931119   X4C                   ELSE
144200931119     C                   SETOFF                                       12
144300931119    4C                   ENDIF
144400931119     C*
144500931119     C                   UPDATE    FRB8SF2
144600931119     C*
144700931119     C  N30              READC     FRB8SF2                                30
144800931119    3C                   ENDDO
144900931119     C*
145000931119     C                   SETOFF                                       36
145100931119     C   90              GOTO      ENDSEL
145200931119    2C                   ENDIF
145300931119    1C                   ENDIF
145400931018     C* PULIZIA SUBFILE
145500931018     C                   SETON                                        35
145600931018     C                   WRITE     FRB8CT4
145700931018     C                   SETOFF                                       35
145800931119     C*
145900931119     C                   Z-ADD     1             NRR
146000950131     C* Indicatore protezione codice tariffa e Pik/Etich. S/N per
146100950131     C* distinte gia' valorizzate
146200950131     C     VI1FVL        COMP      'V'                                    24
146300931018     C*
146400931028    1C     NRR           DOWLE     W01NRR
146500931103     C*
146600931111     C   KANRR           CHAIN     FRB8SF2                            31
146700931111     C  NKA              READC     FRB8SF2                                31
146800931018     C*
146900931018     C* SE ENTER --> SOLO QUELLI CON L'"1"
147000931111    2C  N31*INKA         IFEQ      *OFF
147100931103     C     VI2SCE        ANDEQ     '1'
147200931018     C* SE CMD1 --> TUTTI
147300931018     C     *INKA         OREQ      *ON
147400931018     C*
147500931018     C* C O D I C E   P A D R O N C I N O
147600931028    3C     VI2PDR        IFNE      W02PDR
147700931018     C*
147800931018     C                   MOVEL     VI2PDR        W02PDR                          SALVATAGGIO
147900931019     C                   MOVEL     VI2PDR        VI4PDR                          VIDEO COD
148000931019     C                   MOVEL     VI2DPD        VI4DPD                          VIDEO DES
148100931018     C                   MOVEL     VI2DPD        VH4DPD                          HIDDEN X SLF6
148200931018     C*
148300931108     C* 11 OFF - EMETTO PADRONCINO E TIPO PRESTAZ A VIDEO
148400931019     C                   SETOFF                                       11
148500931028    3C                   ENDIF
148600931018     C*
148700931108     C* T I P O   P R E S T A Z I O N E
148800931028    3C     *IN11         IFEQ      *OFF
148900931019     C     VI2TSR        ORNE      W17TSR
149000931019     C                   SETOFF                                       11
149100021211     C                   MOVEL     *blank        vi4TCS
149200931018     C                   MOVEL     VI2TSR        W17TSR           17             CONFRONTO
149300931018     C                   MOVEL     VI2TSR        VI4TSR                          VIDEO
149400950120     C                   MOVEL     VI2TSR        VH4TSH                         T.SERV.X PRINT
149500931019     C                   MOVEL     VI2TSR        VH4TSR                          HIDDEN
149600950119     C                   MOVEL     VI2TSR        VH4TSD                          HIDDEN
149700950119     C                   MOVEL     VI2DPD        VI4DPD
149800090508     C                   MOVEL     VI2DPD        VH4DPD
149900090508      *
150000931123     C* RICERCA CODICE TARIFFA PER PADRONCINO/TIPO PRESTAZ
150100011112     C                   clear                   vi4ctd
150200931021     C                   EXSR      RICTAR
150300040728     c                   move      tar_poste     vh4TPO
150400090507     C*
150500931018     C                   CLEAR                   VI4NDC
150600931018     C                   CLEAR                   VI4DDC
150700950116     C                   CLEAR                   VH4DDC
150800931019     C                   CLEAR                   VH4NRR
150900931027     C                   CLEAR                   VI4ITT
151000931027     C                   CLEAR                   VI4ITA
151100011106     C                   z-add     1             vh4tpr
151200931018     C                   ADD       1             NRR4
151300011109     c     vh4tpr        comp      2                                      26
151400931018     C                   WRITE     FRB8SF4
151500931019     C                   SETON                                        11
151600000703     C                   SETOFF                                       36
151700931019     C                   Z-ADD     NRR4          VH4NRR
151800931028    3C                   ENDIF
151900931018     C*
152000950117     C* PRESTAZIONE PADRONCINO M o P
152100950119     C     VH2FPP        IFEQ      'M'
152200950117     C                   MOVEL     COS1          VI4DPD
152300950119     C                   ELSE
152400950119     C     VH2FPP        IFEQ      'P'
152500950119     C                   MOVEL     COS2          VI4DPD
152600950119     C                   END
152700950119     C                   END
152800021211      *
152900950117     C                   MOVE      VH2FPP        VH4FPP
153000021211      *
153100950117     C* PAGAMENTO TARIFFA ETICH/PIKING
153200950117     C                   SELECT
153300021211      *
153400950117     C* Se Consegna memorizza flag piking da anagrafico padroncino
153500021211      *  e memorizza anche carico/scarico
153600950117     C     VH4TSR        WHENEQ    'C'
153700021211      * Carico
153800950119     C                   MOVEL     COS3          VI4TSR
153900021211     C                   MOVEL     cos5          vi4TCS
154000950117     C     VH4FPP        IFEQ      'M'
154100950127     C                   MOVEL     VH2PKM        VH4PEA
154200021211     c                   eval      vh4ACS = vh2CAM
154300950117     C                   ELSE
154400950127     C                   MOVEL     VH2PKP        VH4PEA
154500021211     c                   eval      vh4ACS = vh2CAP
154600950117     C                   END
154700021211      *
154800950117     C* Se Ritiro memorizza flag etichettatura da anagrafico
154900950117     C     VH4TSR        WHENEQ    'R'
155000021211      * Scarico
155100950119     C                   MOVEL     COS4          VI4TSR
155200021211     C                   MOVEL     cos6          vi4TCS
155300950117     C     VH4FPP        IFEQ      'M'
155400950127     C                   MOVEL     VH2ETM        VH4PEA
155500021211     c                   eval      vh4ACS = vh2SCM
155600950117     C                   ELSE
155700950127     C                   MOVEL     VH2ETP        VH4PEA
155800021211     c                   eval      vh4ACS = vh2SCP
155900950117     C                   END
156000950117     C                   ENDSL
156100950127     C*
156200950127     C                   MOVEL     VH2PEP        VI4PEP
156300021211     c                   eval      vi4FCS = vh2FCS
156400021211      *
156500950127     C     VI4PEP        IFEQ      *BLANKS
156600950127     C                   MOVEL     VH4PEA        VI4PEP
156700950127     C                   END
156800021211      *
156900021211     C     vi4FCS        IFEQ      *BLANKS
157000021211     C                   eval      vi4FCS = vh4ACS
157100021211     C                   END
157200021211      *
157300931018     C* DISTINTA: NUMERO E DATA
157400931018     C                   MOVEL     VI2NDC        VI4NDC
157500931018     C                   MOVEL     VI2DDC        VI4DDC
157600950116     C                   MOVEL     VH2DDC        VH4DDC
157700011105     C                   MOVEL     VH2ITT        VI4ITT
157800931027     C                   MOVEL     VH2ITA        VI4ITA
157900021217     C                   z-add     vh2TBS        vh4TBS
158000030127      * espone se ci sono dei ritiri annullati
158100030127     C                   z-add     vh2TBS        vi4ran
158200011105     C                   CLEAR                   vi4ctd
158300011105     C                   MOVEL     vh2div        vi4ctd
158400011106     C                   z-add     2             vh4tpr
158500931018     C                   ADD       1             NRR4
158600011109     c     vh4tpr        comp      2                                      26
158700931018     C                   WRITE     FRB8SF4
158800931103     C*
158900931103     C* PULISCO IL CAMPO DI SCELTA
159000931103     C                   MOVEL     ' '           VI2SCE
159100931103     C     VH2I12        IFEQ      '0'
159200931103     C                   SETOFF                                       12
159300931103     C                   ELSE
159400931103     C                   SETON                                        12
159500931103     C                   ENDIF
159600931103     C*
159700931103     C                   UPDATE    FRB8SF2
159800931028    2C                   ENDIF
159900931103     C*
160000931103     C* FINE LETTURA SE NO CMD1
160100931111     C  NKA*IN31         IFEQ      *ON
160200931103     C                   Z-ADD     W01NRR        NRR
160300931103     C                   ENDIF
160400931018     C*
160500931018     C                   ADD       1             NRR
160600931028    1C                   ENDDO
160700931018     C*
160800931018     C* NON EFFETTUATA NESSUNA SELEZIONE--> RIEMETTO SFL 2
160900931028    1C     NRR4          IFEQ      0
161000931018     C                   SETON                                        90
161100931018     C                   ELSE
161200931018     C                   Z-ADD     NRR4          W04NRR
161300931028    1C                   ENDIF
161400931018     C*
161500931119     C     ENDSEL        ENDSR
161600020212     C*---------------------------------------------------------------*
161700931108     C* RICERCO TARIFFA PREFERENZIALE PER TIPO PRESTAZ/PADRONCINO ----*
161800020212     C*---------------------------------------------------------------*
161900931018     C     RICTAR        BEGSR
162000000629     C*
162100000629     C                   SETOFF                                       5051
162200000703     C                   SETON                                        36
162300000622     C* RICHIAMO LA ROUTINE CHE VERIFICA SE LE BOLLE CHE SONO IN DISTINTA SONO TUTTE BARTOLINI
162400000622     C* OPPURE TUTTE POSTE OPPURE MISTE PER FARE LA PROPOSTA DELLA TARIFFA GIUSTA
162500000622     C                   EXSR      RICBOL
162600931104     C* SE LA SOMMA PESI NON C'E' VIENE ASSUNTO SPEDIZIONE
162700931104     C                   MOVEL     'S'           VH4TSM
162800931028     C                   CLEAR                   WTARV
162900000622     C* CAMPI COMODO TARIFFE  POSTE
163000000622     C                   CLEAR                   PTARV
163100040825     C                   clear                   Ptarv_rap
163200040825     C                   clear                   Ptarv_rct
163300000622     C*
163400011112     C     KFGT          CHAIN     FIFGT000                           30
163500931018     C*
163600931021    1C     *IN30         DOWEQ     *OFF
163700931018     C*
163800011112     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
163900011112     C*  e quelle diverse dalla divisa del documento
164000020128      ****   e quelle senza data di stampa (quindi non convalidate)
164100020315      ****   ma solo se non in simulazione.
164200020315      ****   se in simulazione non deve controllare la data di stampa.
164300091112    2C     FGTATB        IFEQ      ' '
164400091112     C     w02div        andeq     fgtdiv
164500091112     C***  W02DDC        ANDGE     FGTDDT
164600091112     C***  W02DDC        andle     FGTDST
164700091112     C*
164800091112     C* Se tariffa del periodo
164900091112     C     Ktgt          CHAIN     FItgt01l
165000091112 2/3 c                   if        %Found(FItgt01l) and
165100091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
165200091112     **
165300091112    3C     parsml        IFeq      *blank
165400091112     C     tgtDTS        aNDgt     0
165500091112     C**** fgtDTS        aNDgt     0
165600091112     C     parsml        OReq      'S'
165700011112      *
165800931108     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
165900931105     C                   Z-ADD     1             C
166000931105     C     FGTTSR        LOOKUP    TSR(C)                                 32
166100091112    4C     FGTCTR        IFNE      CTS(C)
166200931018     C* TARIFFA VALIDA
166300931021     C*
166400931021     C* EMETTO LA PREFERENZIALE SE C'E'
166500931021    5C     FGTFTP        IFEQ      'P'
166600931021     C                   MOVEL     FGTCTR        WTARV             3
166700931021     C                   SETON                                        30
166800931021     C*
166900931021   X5C                   ELSE
167000931021     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
167100931022    6C     WTARV         IFEQ      *BLANKS
167200000622     C     FGTFTP        ANDNE     'O'
167300091112     C     WTARV         orne      *blank
167400040628     C     fgtFTP        ANDNE     'O'
167500091112     C     fgtCTR        andeq     sav_FTDCTB
167600091112     C     sav_FlgCTB    andeq     'S'
167700931022     C                   MOVEL     FGTCTR        WTARV
167800931022    6C                   ENDIF
167900091112      *
168000000622    6C     PTARV         IFEQ      *BLANKS
168100000622     C     FGTFTP        ANDEQ     'O'
168200091112     C     ptarv         Orne      *BLANKS
168300091112     C     fgtCTR        andeq     sav_FTDCTP
168400091112     C     sav_FlgCTP    andeq     'S'
168500040628     C     FGTFTP        ANDEQ     'O'
168600000622     C                   MOVEL     FGTCTR        PTARV             3
168700040825     C                   z-add     fgtrap        Ptarv_rap
168800040825     C                   z-add     fgtrct        Ptarv_rct
168900000622    6C                   ENDIF
169000091112      *
169100931022    5C                   ENDIF
169200091112     C*
169300091112    4C                   ENDIF
169400091112     C*
169500091112    3C                   ENDIF
169600091112 2/3 C                   ENDIF
169700091112    2C                   ENDIF
169800931018     C*
169900011112     C  N30KFGT          READE     FIFGT000                               30
170000931021    1C                   ENDDO
170100931021     C*
170200000714     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
170300000622    0C     VH4BAR        IFEQ      'S'
170400931028    1C     WTARV         IFNE      *BLANKS
170500011030     C                   MOVEL     WTARV         VI4CTD                          VALIDA
170600000703     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
170700011030     C                   MOVEL     VI4CTD        VH4CTB
170800931028    1C                   ENDIF
170900000622    0C                   ENDIF
171000000622     C*
171100000714     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
171200000622    0C     VH4POS        IFEQ      'S'
171300040825      *
171400040825      *  Deve controllare se però c'è una tariffa di riferimento poste che non esiste +
171500091112      **** NON DEVE ESSERE PIU' utilizzata poichè comunque manca il progressivo in chiave
171600040825     C                   if        Ptarv_rap <> 0 and Ptarv_rct <> 0
171700040825     c                   exsr      Ric_Tar_RIF
171800040825     c                   end
171900040825      *
172000000622    1C     PTARV         IFNE      *BLANKS
172100011030     C                   MOVEL     PTARV         VI4CTD                          VALIDA
172200000703     C* SALVO LA TARIFFA POSTE     PER IL DETTAGLIO SPEDIZIONE
172300011030     C                   MOVEL     VI4CTD        VH4CTP
172400040726     c                   else
172500040726     **
172600040726     ** se non ci sono tariffe poste, dalla data specificata in FINOADATA
172700040726     ** deve prendere come tariffa poste la tariffa bartolini
172800040726     c                   if        dataoggi >= finoadata
172900040726     C                   MOVEL     WTARV         VI4CTD                          VALIDA
173000040726     C                   MOVEL     VI4CTD        VH4CTB
173100040726     c                   end
173200040726     **
173300000622    1C                   ENDIF
173400000622    0C                   ENDIF
173500931021     C*
173600931021     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
173700011030     C     VI4CTD        IFEQ      *BLANKS
173800011030     C                   MOVEL     '???'         VI4CTD
173900000629     C                   SETOFF                                       5150
174000000626     C                   ELSE
174100000626     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
174200000626     C     VH4CTB        IFNE      *BLANKS
174300011030     C                   MOVEL     VH4CTB        VI4CTD
174400000626     C                   ENDIF
174500000626     C*
174600931021     C                   ENDIF
174700931021     C*
174800040727     ** Si memorizza se ha trovato una tariffa poste per poter
174900040727     **  gestire variazioni tariffe impostate a video su sfl su tariffe Bartolini
175000040728     c                   if        vh4TSR='C'
175100040727     c                   clear                   tar_poste         1
175200040727     c                   if        vh4CTP <> *blank
175300040727     c                   move      'S'           tar_poste         1
175400040727     c                   end
175500040728     c                   endIF
175600040727     **
175700931119     C                   ENDSR
175800040825     C*================================================================
175900040825     C* cerca eventuale tariffa di riferimento x Poste            ----*
176000040825     C*================================================================
176100040825     C     Ric_Tar_RIF   BEGSR
176200040825     **
176300040825     C                   Z-ADD     Ptarv_rap     rifPDR
176400040825     C                   Z-ADD     Ptarv_rct     rifCTR
176500040825     C*
176600040825     C                   move      'N'           tar_valida
176700040825     C*
176800040825     C     KFGT_rif      setll     fifgt01l
176900040825     C     KFGT_rif      reade     fifgt01l
177000040825      *
177100040825    1C                   DOW       not %eof(fifgt01l)
177200040825     C*
177300040825     C* TARIFFA VALIDA di riferimento POSTE
177400040825     C*  Non deve essere annullata e deve avere la divisa richiesta
177500040825      **   Data Documento deve essere compresa fra le date di attivazione e
177600040825      ***   scadenza e deve essere stata stampata (quindi convalidata)
177700040825      ****   >altrimenti è un Manca Tariffa<
177800040825    2C     FGTATB        ifEQ      ' '
177900091112     c     w02div        andeq     fgtdiv
178000091112     C**** w02DDC        andge     FGTDDT
178100091112     C*
178200091112     C* Se tariffa del periodo
178300091112     C     Ktgt          CHAIN     FItgt01l
178400091112 2/3 c                   if        %Found(FItgt01l) and
178500091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
178600091112      *
178700091112    3C     parsml        IFeq      *blank
178800091112     C     tgtDTS        andgt     0
178900091112     C**** fgtDTS        andgt     0
179000091112     C     fgtatb        oreq      ' '
179100091112     c     parsml        andeq     'S'
179200091112     c*****§tasml        andeq     'S'
179300040825     C*  se trovata ok deve uscire dal giro
179400040825     C                   move      'S'           tar_valida
179500040825     C                   leave
179600091112    3C                   end
179700091112      *
179800091112 2/3 C                   end
179900040825    2C                   end
180000040825     C*
180100040825     C     KFGT_rif      reade     fifgt01l
180200040825    1C                   ENDDO
180300040825     C*
180400040825    2C     Tar_valida    ifeq      'N'
180500040825     C                   clear                   PTARV
180600040825    2c                   end
180700040825     **
180800040825     C                   ENDSR
180900040825     C*================================================================
181000020212     C*---------------------------------------------------------------*
181100000622     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
181200020212     C*---------------------------------------------------------------*
181300000622     C     RICBOL        BEGSR
181400000622     C*
181500000622     C                   MOVEL     ' '           VH4POS
181600000622     C                   MOVEL     ' '           VH4BAR
181700000703     C                   MOVEL     *BLANKS       VH4CTP
181800000703     C                   MOVEL     *BLANKS       VH4CTB
181900021217     c                   z-add     0             conta             5 0
182000040628     c                   z-add     0             sav_FTDctb        3 0
182100040628     c                   z-add     0             sav_FTDctp        3 0
182200040727     C* Oltre  a salvare la tariffa vera e propria deve anche impostare un flag
182300040727     C* parallelamente poichè potrebbero esistere tariffe '000' e quindi potrebbe
182400040727     C* non essere comprensibile solo il dato sav_FTDct*
182500040727     c                   move      *blank        sav_FLGctb        1
182600040727     c                   move      *blank        sav_FLGctp        1
182700000622     C*
182800000622     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
182900000622     C* UTILE PER RICONOSCERE SE BOLLA POSTA
183000011105     C     KFTTT         SETLL     FIFTT02L
183100011105     C     KFTTT         READE     FIFTT02L                               39
183200000714     C**
183300000714     C     *IN39         DOWEQ     *OFF
183400000714     C**
183500011105     C     KFTDT         SETLL     fiftd001
183600000622     C                   DO        *HIVAL
183700011105     C     KFTDT         READE     fiftd001                               32
183800000622     C*
183900000622     C   32              LEAVE
184000021217     c                   add       1             conta
184100000714     C**
184200000622     C* CONTROLLO IN AZORG SE PO POSTE
184300000622     C                   EXSR      CTRORG
184400000622     C*  P.O. POSTE
184500020729     C     §OgNTW        IFEQ      'PPT'
184600000622     C                   MOVEL     'S'           VH4POS
184700040729     c                   if        ftdctp <> *blank and sav_FLGctp = ' '
184800040628     c                   move      ftdctp        sav_FTDctp
184900040727     c                   move      'S'           sav_FLGctp
185000040628     c                   end
185100000622     C                   ELSE
185200000622     C                   MOVEL     'S'           VH4BAR
185300040729     c                   if        ftdctp <> *blank and sav_FLGctb = ' '
185400040628     c                   move      ftdctp        sav_FTDctb
185500040727     c                   move      'S'           sav_FLGctb
185600040628     c                   end
185700000622     C                   ENDIF
185800000622     C*
185900000622     C                   ENDDO
186000000714     C**
186100011105     C     KFTTT         READE     FIFTT02L                               39
186200000714     C                   ENDDO
186300040727     **
186400040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
186500040727     C                   if        VH4BAR = ' ' and VH4POS = 'S'
186600040727     c                   move      sav_FTDctp    sav_FTDctb
186700040727     c                   end
186800040727     **
186900021217     C* Attenzione: Occorre gestire una eccezione.
187000021217     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
187100021217     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
187200021217     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
187300021217     C*  il VH4BAR come se avessi trovato solo delle bolle Bartolini.
187400021217     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
187500021217     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
187600021217     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
187700021217     C* su Bartolini. (Per Default)
187800030127     c                   if        vh4TSR='R' and conta=0 and vh2TBS > 0
187900021217     C                   MOVEL     'S'           VH4BAR
188000021217     C                   EndIF
188100021217     C*
188200000622     C                   ENDSR
188300020521     C*---------------------------------------------------------------*
188400020521     C* CONTROLLO IN AZORG SE PO POSTE
188500020212     C*---------------------------------------------------------------*
188600000622     C     CTRORG        BEGSR
188700020212     C*---------------------------------------------------------------*
188800000622     C* AGGANCIO AZORG
188900000622     C     FTDLNP        CHAIN     AZORG01L                           34
189000000622     C   34              MOVEL     *BLANKS       OG143
189100000622     C  N34              MOVEL     ORGDE3        OG143
189200000622     C*
189300000622     C                   ENDSR
189400020212     C*---------------------------------------------------------------*
189500931018     C* CONTROLLO DATI SUBFILE 4 -------------------------------------*
189600020212     C*---------------------------------------------------------------*
189700931018     C     CONTR4        BEGSR
189800931018     C                   Z-ADD     1             NRR4
189900931018     C                   SETOFF                                       90
190000911014     C*
190100931019     C                   READC     FRB8SF4                                32
190200930707     C*
190300931019    1C     *IN32         DOWEQ     *OFF
190400931019     C*
190500950117     C* RECORD DEL TIPO SERVIZIO
190600931019    2C     VH4NRR        IFEQ      0
190700931019     C*
190800931018     C* C O D I C E   T A R I F F A
190900011030    3C     VI4CTD        IFNE      '???'
191000931019     C* RICERCA
191100011112     C     '?'           SCAN      VI4CTD                                 30
191200011106     C*
191300011106     C* Se si tratta della 1° riga (significa Codice Tariffa)
191400011106     c     vh4tpr        ifeq      1
191500931018     C*
191600011112I   4C     *IN30         IFEQ      *ON
191700931028     C                   CLEAR                   PARAM3
191800931028     C                   MOVEL     VI4PDR        PA3PDR
191900931028     C                   MOVEL     PARSML        PA3SML
192000931028     C                   MOVEL     VH4TSR        PA3TSR
192100100513     C                   MOVEL     KPJBU         kpjbus
192200100513     C                   clear                   kpjbu
192300931028     C                   MOVEL     PARAM3        KPJBU
192400011112     C                   CALL      'FICN86R'
192500931028     C                   PARM                    KPJBA
192600931108     C                   MOVEL     KPJBU         PARAM3
192700100513     C                   MOVEL     KPJBUs        kpjbu
192800931028     C* SELEZIONE EFFETTUATA
192900931028     C     PA3SEL        IFEQ      ' '
193000011030     C                   MOVEL     PA3CTR        VI4CTD
193100931028     C                   ELSE
193200011030     C                   MOVEL     '???'         VI4CTD
193300931028     C                   ENDIF
193400931028     C*
193500931105     C                   SETON                                        36
193600931018     C                   WRITE     FRB8D05
193700000622    4C                   ENDIF
193800931018     C* CONTROLLO
193900011030     C                   TESTN                   VI4CTD               30
194000931105    5C     *IN30         IFEQ      *OFF
194100931105     C                   SETON                                        90
194200931105   X5C                   ELSE
194300931105     C*
194400011030     C                   MOVEL     VI4CTD        W01CTR
194500931105     C                   EXSR      CTRTAR
194600931105    5C                   ENDIF
194700931019     C   90              SETON                                        4036
194800000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
194900000622     C* POSTE O BARTOLINI
195000000622     C     *IN90         IFEQ      *OFF
195100000630     C* IN CASO DI DISTINTA GIA' VALORIZZATA NON SI FA
195200000630     C     *IN24         ANDEQ     *OFF
195300000622     C*
195400000622     C     VH4POS        IFEQ      'S'
195500000622     C     VH4BAR        ANDNE     'S'
195600000622     C* E LA TARIFFA È BARTOLINI ERRORE
195700000622     C     FGTFTP        ANDNE     'O'
195800040726     c                   if        dataoggi < finoadata
195900000622     C                   SETON                                        905236
196000040726     c                   end
196100000622     C                   ENDIF
196200000622     C*
196300000622     C     VH4POS        IFNE      'S'
196400000622     C     VH4BAR        ANDEQ     'S'
196500040622     C*
196600000622     C* E LA TARIFFA È BARTOLINI ERRORE
196700000622     C     FGTFTP        ANDEQ     'O'
196800000622     C                   SETON                                        905336
196900000622     C                   ENDIF
197000040622     C*
197100000703     C* SE CI SONO SPEDIZIONI MISTE MA MANCA ALMENO UNA DELLE DUE TARIFFE DO ERRORE
197200000703     C* LA PRIMA VOLTA CHE CONTROLLO
197300000703     C     VH4POS        IFEQ      'S'
197400000703     C     VH4BAR        ANDEQ     'S'
197500040622     C*
197600000703     C* MANCA TARIFFA POSTA
197700000703     C     VH4CTP        IFEQ      *BLANK
197800040727     ** oppure se in un secondo tempo non essendoci tariffe poste è stata impostata
197900040727     ** un'altra tariffa bartolini rispetto a quella standard proposta
198000040728     c     vh4TPO        oreq      *blank
198100040727     C     vh4CTP        andne     vi4CTD
198200040622     C*
198300040622     C*  fino al giorno 2004/08/10 il controllo è attivato
198400040726     C*   comunque imposta in Hidden il valore espresso a video
198500040726     C*    ma non da + l'errore
198600040622     c                   if        dataoggi < finoadata
198700000703     C                   SETON                                        905136
198800040726     c                   end
198900011030     C                   MOVEL     VI4CTD        VH4CTP
199000040622     C*
199100000703     C                   ENDIF
199200000703     C*
199300000703     C     VH4CTB        IFEQ      *BLANK
199400000703     C                   SETON                                        905036
199500011030     C                   MOVEL     VI4CTD        VH4CTB
199600000703     C                   ENDIF
199700000703     C*
199800000703     C                   ENDIF
199900000622     C*
200000000622     C* SE NON E' ACCESO IL 90 VALORIZZO I CAMPI TARIFFA HIDDEN SE CAMBIATI
200100000622     C     *IN90         IFEQ      *OFF
200200000622     C*+
200300000622     C     FGTFTP        IFEQ      'O'
200400011030     C                   MOVEL     VI4CTD        VH4CTP
200500000622     C                   ELSE
200600011030     C                   MOVEL     VI4CTD        VH4CTB
200700000622     C                   ENDIF
200800000622     C*
200900000622     C                   ENDIF
201000000622     C*
201100000622     C                   ENDIF
201200011106     C*
201300011106    4C                   end
201400011106     C*
201500931021    3C                   ENDIF
201600931019     C*
201700931108     C* NON E' POSSIBILE ANDARE IN MANUTENZIONE DI UN TIPO PRESTAZIONE
201800931019     C  N90VI4SCE        IFEQ      'M'
201900931019     C                   SETON                                        419036
202000931019     C                   ENDIF
202100931019     C*
202200931108     C* ANNULLAMENTO VALORIZZAZIONE DI TUTTO IL TIPO PRESTAZIONE
202300931019     C*  LO ESEGUO DOPO
202400931019     C  N90VI4SCE        IFEQ      'A'
202500931112     C   20              SETON                                        459036
202600931112     C  N20              SETON                                        36
202700931019     C                   ENDIF
202800931019     C*
202900931019   X2C                   ELSE
203000011107     C*
203100931019     C* RECORD DI DISTINTA/RITIRO
203200011107     C*
203300931019     C* MANUTENZIONE SPEDIZIONI DELLA DISTINTA
203400931022    3C     VI4SCE        IFEQ      'M'
203500931022    4C     VI4ITT        IFGT      0
203600931022     C     VI4ITA        ANDGT     0
203700931022     C                   SETON                                        439036
203800931022   X4C                   ELSE
203900931019     C                   EXSR      MANSPE
204000931019     C                   SETON                                        90
204100931028     C                   WRITE     FRB8D05
204200931105     C*
204300931105     C* RIAGGANCIO IL RECORD DI SUBFILE
204400931105     C     NRR4          CHAIN     FRB8SF4                            32
204500931022    4C                   ENDIF
204600931022    3C                   ENDIF
204700931019     C*
204800931019     C* ANNULLO VALORIZZAZIONE PRECEDENTE DELLA DISTINTA
204900931019     C     VI4SCE        IFEQ      'A'
205000931105     C     VI1FVL        ANDEQ     'V'
205100931112     C   20              SETON                                        459036
205200931112     C  N20              EXSR      ANNSPE
205300931112     C  N20              SETON                                        21
205400931105     C                   ENDIF
205500931019     C*
205600950117     C* CONTROLLO PIKING/ETICHETTATURA S/N
205700950117     C* Se = Blank imposto quello specificato su anagrafico padroncini
205800950117     C     VI4PEP        IFEQ      *BLANKS
205900950127     C                   MOVEL     VH4PEA        VI4PEP
206000950117     C                   END
206100021211      *
206200021211     C* Se = Blank imposto quello specificato su anagrafico padroncini
206300021211      *   per carico/scarico
206400021211     C     vi4FCS        IFEQ      *BLANKS
206500021211     C                   eval      vi4FCS = vh4ACS
206600021211     C                   END
206700950117     C*
206800931019    2C                   ENDIF
206900921228     C*
207000931019    2C     VH4NRR        IFEQ      0
207100931018     C                   SETOFF                                       11
207200931018     C                   ELSE
207300931018     C                   SETON                                        11
207400931019    2C                   ENDIF
207500911017     C*
207600931019     C  N36              MOVEL     ' '           VI4SCE
207700931018     C                   Z-ADD     NRR4          REC4
207800011109     c     vh4tpr        comp      2                                      26
207900931019     C                   UPDATE    FRB8SF4
208000931018     C                   SETOFF                                       36
208100930707     C*
208200931019     C   90              SETON                                        32
208300931019     C  N90              READC     FRB8SF4                                32
208400931018    1C                   ENDDO
208500931019     C**
208600931019     C* SE TUTTO OK CONTROLLO SE HO ANNULLAMENTI DI VALORIZZAZIONE
208700931108     C*  DI TIPO PRESTAZ DA FARE
208800931108    1C     *IN90         IFEQ      *OFF
208900931105     C*
209000931019     C                   READC     FRB8SF4                                32
209100931019     C*
209200931108    2C     *IN32         DOWEQ     *OFF
209300931105     C*
209400931108    3C     VI4SCE        IFEQ      'A'
209500931108     C* SE NON SONO GIA' VALORIZZATE PULISCO SOLO IL CAMPO DI SCELTA SF
209600931108    4C     VI1FVL        IFEQ      'V'
209700931019     C                   EXSR      ANNTSR
209800931105     C*
209900931105     C* AL TERMINE RIEMETTO IL SFL2 (21 ON)
210000931105     C                   SETON                                        21
210100931108   X4C                   ELSE
210200931108     C                   MOVEL     ' '           VI4SCE
210300011109     c     vh4tpr        comp      2                                      26
210400931108     C                   UPDATE    FRB8SF4
210500931108    4C                   ENDIF
210600931105     C*
210700931108    3C                   ENDIF
210800931019     C*
210900931019     C                   READC     FRB8SF4                                32
211000931108    2C                   ENDDO
211100931108    1C                   ENDIF
211200911017     C*
211300900614     C                   ENDSR
211400020212     C*---------------------------------------------------------------*
211500931019     C* MANUTENZIONE SPEDIZIONI DISTINTA SELEZIONATA -----------------*
211600020212     C*---------------------------------------------------------------*
211700931019     C     MANSPE        BEGSR
211800931110     C                   SETOFF                                       151637
211900931108     C                   CLEAR                   FRB8SF6
212000931028     C     VI4ITT        IFGT      0
212100931028     C                   SETON                                        15
212200931028     C                   ENDIF
212300931028     C     VI4ITA        IFGT      0
212400931028     C                   SETON                                        16
212500931028     C                   ENDIF
212600931028     C*
212700931019     C* CARICAMENTO SPEDIZIONI
212800931019     C                   EXSR      CARSPE
212900931110     C* SE NON CE NE SONO NON EMETTO IL SUBFILE
213000030127     c                   move      'N'           Dett6_a0          1
213100931110     C     NRR6          IFEQ      0
213200931110     C                   SETON                                        37
213300030127     c                   move      'S'           Dett6_a0
213400030127     c                   z-add     vi4ran        d8ran
213500931110     C                   ENDIF
213600931019     C*
213700931103     C                   Z-ADD     1             REC6
213800931019     C                   WRITE     FRB8D07
213900030127     C   37              WRITE     FRB8D08
214000931019     C     FORCT6        TAG
214100020409     c                   if        savnr6 > 0
214200020409     c                   z-add     savnr6        rec6
214300020409     c                   end
214400931019     C                   EXFMT     FRB8CT6
214500020409     c                   z-add     nrrult        savnr6            5 0
214600931019     C* CMD3 - FINE LAVORO
214700931019     C     *INKC         IFEQ      *ON
214800931111     C   22              SETON                                        4490
214900931111     C   22              GOTO      FORCT6
215000931019     C                   SETON                                        LR
215100931019     C                   GOTO      ENDMAN
215200931019     C                   ENDIF
215300931019     C* CMD12 - RITORNO
215400931019     C   KL              GOTO      ENDMAN
215500030127      *
215600030127      * CONTROLLI solo se ci sono delle righe e non sono solo
215700030127      *   dei ritiri annullati
215800030127     c                   if        Dett6_a0 = 'N'
215900931019     C                   EXSR      CONTR6
216000030127     c                   end
216100931202     C   90              GOTO      FORCT6
216200020418     C*
216300020418     c                   if        VI1FVL<>'V'
216400931019     C* CMD6 - AGGIORNAMENTO
216500931202     C   KE
216600030127     cOR KF              if        Dett6_a0 = 'N'
216700030127     C                   EXSR      AGGIO6
216800020408     C   ke
216900020408     Cor kf              commit
217000020418     c                   end
217100030127     c                   end
217200931202     C* CMD5 RIVISUALIZZA
217300931202     C   KE              EXSR      CARSPE
217400931202     C   KE              Z-ADD     1             REC6
217500931202     C* RIEMETTO VIDEATA PER ENTER O CMD5
217600931202     C  NKF              GOTO      FORCT6
217700931019     C*
217800931021     C     ENDMAN        ENDSR
217900020212     C*---------------------------------------------------------------*
218000950116     C* CARICO LE SPEDIZIONI DELLA DISTINTA A VIDEO ------------------*
218100020212     C*---------------------------------------------------------------*
218200931019     C     CARSPE        BEGSR
218300931021     C                   CLEAR                   NRR6
218400931021     C                   CLEAR                   W06NRR
218500931019     C*
218600931019     C                   SETON                                        35
218700931019     C                   WRITE     FRB8CT6
218800931019     C                   SETOFF                                       35
218900931019     C*
219000931019     C* DECODIFICA C A M P I   T E S T A T A
219100931019     C*
219200931019     C                   MOVEL     '1N'          COD
219300931019     C                   MOVEL     *BLANKS       KEY
219400931103     C                   MOVEL     VH4TSM        KEY
219500931019     C     KTAB          CHAIN     TABEL                              30
219600931019     C  N30              MOVEA     TBLUNI        K17(3)
219700931019     C   30              MOVEA     *BLANKS       K17(3)
219800931103     C                   MOVEL     VH4TSM        K17(1)
219900931019     C                   MOVEL     '-'           K17(2)
220000931019     C                   MOVEA     K17           VI6TSM
220100931019     C* MITTENTE/DESTINATARIO
220200931022    1C     VH4TSR        IFEQ      'R'
220300931104     C                   SETON                                        19
220400020311     C*******            MOVEL     'An'          VI6IA
220500020311     C*******            MOVEL     CMITT         VI6MD
220600020311     C                   MOVEL     'In'          VI6IA
220700020311     C                   MOVEL     CSPED         VI6MD
220800931108     C*
220900931108   X1C                   ELSE
221000931108     C*
221100931104     C                   SETOFF                                       19
221200931029     C                   MOVEL     'In'          VI6IA
221300931202     C                   MOVEL     CSPED         VI6MD
221400931019    1C                   ENDIF
221500931021     C*
221600931021     C* REPERISCO CODICE TARIFFA DALLA RIGA IN CUI SI TROVA
221700121221     c                   move      'N'           h6targ            1
221800931103     C                   Z-ADD     NRR4          W05NRR
221900931021     C     VH4NRR        CHAIN     FRB8SF4                            30
222000931022     C   30              MOVEL     '000'         W01CTR
222100011030     C**N30VI4CTR        IFEQ      '???'
222200011106     c                   if        vh4tpr = 1
222300011030     C  N30VI4CTD        IFEQ      '???'
222400121221     c                   move      'S'           h6targ
222500931022     C                   MOVEL     000           W01CTR
222600931022     C                   ELSE
222700011030     C                   MOVEL     VI4CTD        W01CTR
222800931021     C                   ENDIF
222900011105     c                   endif
223000011105      *
223100000703     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
223200000703     C     VH4CTP        IFNE      *BLANKS
223300000703     C     VH4CTB        ANDNE     *BLANKS
223400000703     C                   MOVEL     VH4CTP        WCTP              3
223500000703     C                   MOVEL     VH4CTB        WCTB              3
223600000703     C                   ELSE
223700000703     C                   MOVEL     *BLANKS       WCTP
223800000703     C                   MOVEL     *BLANKS       WCTB
223900000703     C                   ENDIF
224000931103     C                   Z-ADD     W05NRR        NRR4
224100931028     C     NRR4          CHAIN     FRB8SF4                            30
224200931019     C*
224300931019     C* DECODIFICA  C A M P I   D E T T A G L I O
224400931019     C*
224500931019     C                   CLEAR                   K17
224600931203     C                   CLEAR                   WFTD
224700931108     C*
224800011105     C     KFTD          SETLL     fiftd001
224900011105     C     KFTD          READE     fiftd001                               32
225000931019     C*
225100931019    1C     *IN32         DOWEQ     *OFF
225200931203     C                   ADD       1             WFTD              5 0
225300000626     C                   SETOFF                                       132325
225400000626     C                   SETOFF                                       49
225500950118     C*
225600950119     C                   MOVE      *BLANKS       VI6FTC
225700950119     C                   MOVE      *BLANKS       VI6FT2
225800950119     C                   MOVE      *BLANKS       VI6TSP
225900030320     C                   MOVE      *BLANKS       Vh6cbo
226000950119     C                   MOVE      *BLANKS       VH6SIC
226100950119     C                   MOVE      *BLANKS       VH6GVA
226200950119     C                   MOVE      *ZEROS        VH6NCE
226300000705     C                   MOVE      *ZEROS        VH6FGC
226400000623     C*
226500000623     C* CONTROLLO SE BOLLA POSTA O BARTOLINI
226600000623     C                   EXSR      CTRORG
226700020729     C     §OgNTW        IFEQ      'PPT'
226800020729     c                   movel     'S'           VH6POS
226900020729     c                   else
227000020729     C                   MOVEL     *blank        VH6POS
227100020729     c                   end
227200000626     C* SE BOLLA POSTA PROTEGGO IL CAMPO NUMERO STOP
227300020729     C     §OgNTW        IFEQ      'PPT'
227400000626     C                   SETON                                        49
227500000626     C                   ELSE
227600000626     C                   SETOFF                                       49
227700000626     C                   ENDIF
227800950119     C*
227900011123     C                   z-add     FTDSTP        VI6STP                          STOP
228000020313     C* ----------
228100020313     C* servono questi 2 campi hidden per poter gestire la vista logica FIFTD04L
228200020313     C* in alternativa alla FIFTD02L
228300020313     C                   z-add     FTDksc        vh6ksc                          cliente
228400020313     C                   z-add     FTDnsp        vh6nsp                          spedizione
228500020326     C                   move      FTDtsr        vh6tsr
228600020326     C                   z-add     FTDfgs        vh6fgs
228700020326     c                   if        vh6fgs=0
228800020326     C                   movel     FTDpdr        vh6fgs
228900020326     c                   end
229000020313     C* ----------
229100950116     C                   MOVEL     FTDRSC        VI6RSB                          RAG SOCIALE
229200950113     C                   MOVEL     FTDCAP        VI6CAP
229300950118     C                   Z-ADD     FTDCAS        VH6CAS                          CONTRASSEGNO
229400950131     C                   MOVEL     FTDVCA        VH6VCA                          DIVISA
229500011106     c                   if        vh6vca=*blank
229600011106     c                   eval      vh6vca='ITL'
229700011106     c                   end
229800950118     C                   Z-ADD     FTDIFP        VH6IFP                          IMP.INCASS.
229900011109      ***>>>>>>>>>>>>>>
230000011109     c                   if        vh4tpr = 2
230100011109     c                   movel     vi4ctd        vi6div
230200011109     c                   end
230300950118     C                   Z-ADD     FTDSET        VH6SET
230400931202     C* RITIRI
230500931202    2C     VH4TSR        IFEQ      'R'
230600020311     C*******            Z-ADD     FTDKSC        VI6NSP                          COD CLIENTE
230700020311     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
230800020311    2C     ftdnsp        ifeq      0
230900020311    2C     ftdksc        andgt     0
231000020311     C                   z-add     ftdksc        vi6nsp                          COD CLIENTE
231100020311     C                   end
231200950118     C                   Z-ADD     FTDNCE        VH6NCE                          COLLI ETICH
231300931202     C                   ELSE
231400931202     C* CONSEGNE
231500091012     C     Bolla_FTD     CHAIN     FNARB01L                           30
231600090918     C                   Z-ADD     arbKSC        Vh6ksc                          Cliente
231700090921      *
231800960208     C                   MOVEL     ARBGVA        VH6GVA                          PARTICOLARITA'
231900960208     C                   MOVE      ' '           VH6GVA
232000950118     C                   MOVEL     ARBTSP        VI6TSP                          C/E/D
232100030320     C                   MOVEL     ARBcbo        Vh6cbo                          C/E/D
232200931202     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
232300950113     C                   MOVE      ARBMGS        DATA                            DATA SPEDIZ
232400931019     C                   MOVEL     GG            VI6MGS
232500931019     C                   MOVE      MM            VI6MGS
232600931028     C* SPEDIZIONE CONSEGNATA
232700931105    3C     FTDDCM        IFGT      0
232800950113     C                   MOVE      FTDDCM        DATA
232900931019     C                   MOVEA     GG            K17(1)
233000931019     C                   MOVEL     '/'           K17(3)
233100931019     C                   MOVEA     MM            K17(4)
233200931019     C                   MOVEL     '/'           K17(6)
233300931019     C                   MOVEA     AA            K17(7)
233400931019     C                   MOVEA     K17           VI6CON
233500950117     C* FLAG RITORNO ALL'INCASSO
233600950117     C     FTDSIC        COMP      'S'                                    23
233700950117     C                   MOVEL     FTDSIC        VH6SIC
233800931029     C*
233900950113     C* CONSEGNA PARTICOLARE 1 (SOLO SE E' CONSEGNATA)
234000950113    3C     ARBTC1        IFNE      ' '
234100931029     C                   MOVEL     '1P'          COD
234200931029     C                   MOVEL     *BLANKS       KEY
234300950113     C                   MOVEL     ARBTC1        KEY
234400931029     C     KTAB          CHAIN     TABEL                              30
234500931029     C*
234600931029     C  N30              MOVEA     TBLUNI        K17(3)
234700931029     C   30              MOVEA     *BLANKS       K17(3)
234800950113     C                   MOVEL     ARBTC1        K17(1)
234900931029     C                   MOVEL     '-'           K17(2)
235000931029     C                   MOVEA     K17           VI6FTC
235100931029     C                   ENDIF
235200950113     C* CONSEGNA PARTICOLARE 2 (SOLO SE E' CONSEGNATA)
235300950113    3C     ARBTC2        IFNE      ' '
235400950113     C                   MOVEL     '1P'          COD
235500950113     C                   MOVEL     *BLANKS       KEY
235600950113     C                   MOVEL     ARBTC2        KEY
235700950113     C     KTAB          CHAIN     TABEL                              30
235800950113     C*
235900950113     C  N30              MOVEA     TBLUNI        K17(3)
236000950113     C   30              MOVEA     *BLANKS       K17(3)
236100950113     C                   MOVEL     ARBTC2        K17(1)
236200950113     C                   MOVEL     '-'           K17(2)
236300950113     C                   MOVEA     K17           VI6FT2
236400950113     C                   ENDIF
236500931029     C*
236600931029     C* CONTROLLO SE RICONSEGNA GIACENZA (SOLO SE CONSEGNATA)
236700000623     C*****      FTDFGC    IFEQ ' '
236800000623     C*****                EXSR CTRGIA
236900000623     C*****                ENDIF
237000000623     C*****                MOVELFTDFGC    VH6FGC
237100931019     C*
237200931019   X3C                   ELSE
237300931019     C*
237400931019     C* C'E' CODICE GIACENZA/MANCATA CONSEGNA
237500931019    4C     FTDCMC        IFNE      ' '
237600931019     C                   MOVEA     FTDCMC        K17(1)
237700931019     C                   MOVEL     '-'           K17(4)
237800931019     C                   MOVEL     '2A'          COD
237900931019     C                   MOVEL     *BLANKS       KEY
238000931019     C                   MOVEL     FTDCMC        KEY
238100931019     C     KTAB          CHAIN     TABEL                              30
238200931019     C  N30              MOVEA     TBLUNI        K17(5)
238300931021     C   30              MOVEA     *BLANKS       K17(5)
238400931019     C                   MOVEA     K17           VI6CON
238500000705     C*
238600000705     C*
238700000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
238800000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
238900000623     C     VH6POS        IFEQ      'S'
239000000623     C*
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
241700000630    4C*****!!!!!****       ENDIF          SPOSTATA SOTTO PERCHÈ IL CONTROLLO MANCATA CONSEGNA SI FA
241800000630     C*****!!!!!****                      SOLO SE LA SPEDIZIONE NON E' POSTA
241900931019     C*
242000931028     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
242100931019     C* PRIMA CERCO CON FILIALE DI APPARTENENZA
242200931021     C                   MOVEL     KEY           WALF6             6
242300931111     C                   EXSR      CTRCMC
242400000630    4C                   ENDIF
242500931019     C*
242600950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
242700931028     C*                                    (SE NON L'HO TROVATA)
242800931028     C     *IN30         IFEQ      *OFF
242900931019     C                   SETON                                        13
243000931019    4C                   ENDIF
243100931019    3C                   ENDIF
243200931028     C*
243300950119     C* CODICE BOLLA
243400030320     C                   MOVEL     ARBCBO        VI6CBO
243500950119     C                   MOVEL     '3A'          COD
243600950119     C                   MOVEL     *BLANKS       KEY
243700950119     C                   MOVEL     ARBCBO        KEY
243800950119     C     KTAB          CHAIN     TABEL                              30
243900950119     C  N30              MOVEL     TBLUNI        DS3A
244000950119     C  N30              MOVEL     §3ADES        VI6CBD
244100950119     C   30              MOVEL     *BLANKS       VI6CBD
244200950119     C*
244300931028     C* C O D I C E   B O L L A   D A   T A S S A R E
244400931028    3C     *IN13         IFEQ      *OFF
244500931111     C                   EXSR      CTRCBL
244600931028     C*
244700950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
244800931028     C*                                    (SE L'HO TROVATO)
244900931111    4C     *IN31         IFEQ      *ON
245000931028     C                   SETON                                        13
245100931028    4C                   ENDIF
245200931028    3C                   ENDIF
245300931028    2C                   ENDIF
245400931202     C*
245500931202     C* AGGIORNO PERCHE' POTREI AVER IMPOSTATO IL FLAG DI RICONS.GIAC
245600011105     C                   UPDATE    fiftd001
245700931019     C*
245800931022     C     FTDCTP        IFNE      *BLANKS
245900931022     C                   MOVEL     FTDCTP        VI6CTR
246000931022     C                   ELSE
246100000622     C* VERIFICO SE ESISTONO SIA BOLLE POSTE CHE BOLLE BARTOLINI
246200000703     C     WCTB          IFNE      '   '
246300000703     C     WCTP          ANDNE     '   '
246400000622     C* SE E' UN PO POSTE
246500000622     C     VH6POS        IFEQ      'S'
246600000622     C* MUOVO TARIFFA POSTA
246700000703     C                   MOVEL     WCTP          VI6CTR
246800000622     C                   ELSE
246900000622     C* MUOVO TARIFFA BARTOLINI
247000000703     C                   MOVEL     WCTB          VI6CTR
247100000622     C                   ENDIF
247200000622     C                   ELSE
247300121221     C                   MOVEL     h6targ        Vh6targ
247400121221     C                   MOVEL     W01CTR        VI6CTR
247500000622     C                   ENDIF
247600000622     C*
247700931022     C                   ENDIF
247800011105      *
247900011105      *  pulito il campo di totale
248000011108     C                   clear                   vi6tot
248100011115     C******             clear                   wh6tot
248200011115     c                   move      '0'           vp6ita
248300011115     c                   move      '0'           vp6itt
248400011115      *
248500011115      * imposta il flag di protezione in memoria sulla riga
248600011115     c                   if        *in13 = *on or *in16 = *on
248700011115     c                   eval      vp6ita = '1'
248800011115     c                   end
248900011115      *
249000011115      * imposta il flag di protezione in memoria sulla riga
249100011115     c                   if        *in15 = *on or *in23 = *on or *in25 = *on
249200011115     c                   eval      vp6itt = '1'
249300011115     c                   end
249400011115      * pulisce i campi
249500011115     C                   z-add     0             vh6ita
249600011115     C                   z-add     0             vi6ita
249700011115     C                   z-add     0             vh6itt
249800011115     C                   z-add     0             vi6itt
249900011105      *
250000950117     C* ACCESSORI NON VISUALIZZABILI: NON CARICO NEMMENO L'IMPORTO
250100931105     C     *IN13         IFEQ      *ON
250200950119     C                   MOVEL     'B'           VH6FIA
250300931105     C                   ELSE
250400011109     C                   MOVEL     FTDITA        VI6ITA
250500011115     C  N16              add       ftdita        vi6tot
250600011115     C**N16****          add       ftdita        wh6tot
250700931022     C                   MOVEL     FTDITA        VH6ITA
250800931021     C                   MOVEL     FTDFIA        VH6FIA
250900931105     C                   ENDIF
251000950117     C* NON CARICO IMPORTO BASE SE SI TRATTA DI UN RITORNO ALL'INCASSO
251100950118     C     FTDSIC        IFNE      'S'
251200011109     C                   MOVEL     FTDITT        VI6ITT
251300011115     C  N15              add       ftditt        vi6tot
251400011115     C**N15****          add       ftditt        wh6tot
251500950117     C                   MOVEL     FTDITT        VH6ITT
251600950117     C                   MOVEL     FTDFIT        VH6FIT
251700950119     C                   ELSE
251800950119     C                   MOVEL     'B'           VH6FIT
251900950117     C                   ENDIF
252000931105     C                   MOVEL     FTDAAS        VH6AAS
252100931019     C                   ADD       1             NRR6
252200020313     C*
252300020418     c     vi1fvl        comp      'V'                                    57
252400931019     C                   WRITE     FRB8SF6
252500931019     C*
252600011105     C     KFTD          READE     fiftd001                               32
252700931019    1C                   ENDDO
252800931203     C*
252900931203     C                   MOVEL     W01CTR        W01CTP
253000931019     C*
253100931019     C                   ENDSR
253200020212     C*---------------------------------------------------------------*
253300931019     C* ANNULLAMENTO VALORIZZAZIONE DI UNA DISTINTA ------------------*
253400020212     C*---------------------------------------------------------------*
253500931019     C     ANNSPE        BEGSR
253600931108     C*
253700011105     C     KFTD          SETLL     fiftd001
253800011105     C     KFTD          READE     fiftd001                               32
253900931019     C*
254000931022    1C     *IN32         DOWEQ     *OFF
254100931022     C* 14 ON  - ANNULLO TUTTO
254200931022    2C     *IN14         IFEQ      *ON
254300931022     C                   CLEAR                   FTDFIT                          FLG TAR BAS
254400931022     C                   CLEAR                   FTDITT                          TAR BASE
254500931022     C                   CLEAR                   FTDFIA                          FLG TAR ACC
254600931022     C                   CLEAR                   FTDITA                          TAR ACCESS
254700931022     C*
254800931022   X2C                   ELSE
254900931022     C* 14 OFF - ANNULLO SOLO LE VALORIZZAZIONI AUTOMATICHE
255000931022     C*
255100931022    3C     FTDFIT        IFEQ      ' '
255200931022     C                   CLEAR                   FTDITT                          TAR BASE
255300931022    3C                   ENDIF
255400931022    3C     FTDFIA        IFEQ      ' '
255500931022     C                   CLEAR                   FTDITA                          TAR ACCESS
255600931022    3C                   ENDIF
255700931022    2C                   ENDIF
255800931022     C*
255900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
256000020326     C* i primi tre caratteri
256100020326     c                   if        ftdfgs = 0
256200020326     c                   movel     ftdpdr        ftdfgs
256300020326     c                   end
256400020326     C*
256500931022     C                   CLEAR                   FTDCTP                          COD TAR
256600011105     C                   UPDATE    fiftd001
256700931019     C*
256800020326     C*          *-------------------------------------*
256900020408     c     parsml        ifeq      ' '
257000020326     C*  deve annullare/cancellare (se c'è) anche la voce di C/E
257100020326     c     Kfce          setll     FiFCE01L
257200020326     c     Kfce          reade     FiFCE01L
257300020326     c                   dow       not %Eof(FiFCE01L)
257400020326     c                   delete    FIFCE000
257500020326     c     Kfce          reade     FiFCE01L
257600020326     C                   enddo
257700020408     C                   endif
257800020326     C*          *-------------------------------------*
257900020326     C*
258000011105     C     KFTD          READE     fiftd001                               32
258100931022    1C                   ENDDO
258200931019     C*
258300931022     C* 14 OFF - SVALORIZZO ANCHE LA TESTATA
258400931022    1C     *IN14         IFEQ      *OFF
258500931022     C*
258600011105     C     KFTT          CHAIN     FIFTT000                           32
258700931022    2C     *IN32         IFEQ      *OFF
258800931022     C*
258900931022    3C     FTTFIT        IFEQ      ' '
259000931022     C                   CLEAR                   FTTITT
259100931022    3C                   ENDIF
259200940121     C*
259300931022    3C     FTTFIA        IFEQ      ' '
259400931022     C                   CLEAR                   FTTITA
259500931022    3C                   ENDIF
259600931022     C*
259700940121     C                   Z-ADD     VI4ITT        FTTITT
259800940121     C     VI4ITT        IFGT      0
259900940121     C                   MOVEL     'M'           FTTFIT
260000940121     C                   ENDIF
260100940121     C                   Z-ADD     VI4ITA        FTTITA
260200940121     C     VI4ITA        IFGT      0
260300940121     C                   MOVEL     'M'           FTTFIA
260400940121     C                   ENDIF
260500020326     C*
260600020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
260700020326     C* i primi tre caratteri
260800020326     c                   if        fttfgs = 0
260900020326     c                   movel     fttpdr        fttfgs
261000020326     c                   end
261100940121     C*
261200940121     C                   MOVEL     ' '           FTTFVL                          FLG VALORIZ
261300940121     C                   MOVEL     ' '           FTTFLA                          FLG MAN TAR
261400931123     C                   CLEAR                   FTTSNA
261500931125     C                   CLEAR                   FTTCLA
261600950119     C                   CLEAR                   FTTCPE
261700931123     C                   CLEAR                   FTTKFA
261800931123     C                   CLEAR                   FTTTVL
261900950119     C                   CLEAR                   FTTSNE
262000931202     C                   CLEAR                   FTTSNP
262100011105     C                   UPDATE    FIFTT000
262200931022    2C                   ENDIF
262300931022    1C                   ENDIF
262400931019     C*
262500931019     C                   ENDSR
262600020212     C*---------------------------------------------------------------*
262700931108     C* ANNULLAMENTO VALORIZZAZIONE DI UN TIPO PRESTAZ ---------------*
262800020212     C*---------------------------------------------------------------*
262900931019     C     ANNTSR        BEGSR
263000931021     C     VI4SCE        DOWEQ     'A'
263100931021     C* ELIMINO IL FLAG
263200931021     C                   MOVEL     ' '           VI4SCE
263300011109     c     vh4tpr        comp      2                                      26
263400931021     C                   UPDATE    FRB8SF4
263500931021     C*
263600931019     C                   Z-ADD     VI4PDR        W01PDR
263700931019     C                   MOVEL     VH4TSR        W01TSR
263800931108     C*
263900931019     C                   ADD       1             NRR4
264000931019     C     NRR4          CHAIN     FRB8SF4                            32
264100931019     C*
264200931108     C* LEGGO TUTTE LE DISTINTE O I RITIRI DI QUEL PADR/TIPO PRESTAZ
264300931019     C     *IN32         DOWEQ     *OFF
264400931019     C     VI4PDR        ANDEQ     W01PDR
264500931019     C     VH4TSR        ANDEQ     W01TSR
264600931019     C*
264700931019     C                   EXSR      ANNSPE
264800931019     C*
264900931103     C                   ADD       1             NRR4
265000931019     C     NRR4          CHAIN     FRB8SF4                            32
265100931019     C                   ENDDO
265200931021     C                   ENDDO
265300931019     C*
265400931019     C                   ENDSR
265500020212     C*---------------------------------------------------------------*
265600931019     C* CONTROLLO SFL6 -----------------------------------------------*
265700020212     C*---------------------------------------------------------------*
265800931019     C     CONTR6        BEGSR
265900011105     C*
266000931019     C                   Z-ADD     1             NRR6
266100931019     C                   SETOFF                                       90
266200931103     C                   CLEAR                   WALF3
266300931203     C                   CLEAR                   WREAD
266400931021     C                   SETON                                        36
266500931019     C                   READC     FRB8SF6                                32
266600931019     C*
266700931019    1C     *IN32         DOWEQ     *OFF
266800931203     C                   ADD       1             WREAD             5 0
266900931021     C*
267000931123     C* CONTROLLO SE HO INSERITO DEI VALORI MANUALMENTE
267100931123     C* T A R I F F A   B A S E
267200011109      *>>>>>>>>>>>>>>>
267300931105     C*
267400011109    2C     VI6ITT        IFNE      VH6ITT
267500931022     C*
267600011109    3C     VI6ITT        IFGT      0
267700931021     C                   MOVEL     'M'           VH6FIT
267800931022   X3C                   ELSE
267900931021     C                   MOVEL     ' '           VH6FIT
268000931022    3C                   ENDIF
268100011105     C*
268200011109     C                   MOVEL     VI6ITT        VH6ITT
268300011108     C*>>>>>>>>
268400931022    2C                   ENDIF
268500931021     C*
268600931022     C* T A R I F F A   A C C E S S O R I
268700011108      *>>>>>>>>>>>>>>>
268800011109    2C     VI6ITA        IFNE      VH6ITA
268900931022     C*
269000011109    3C     VI6ITA        IFGT      0
269100931206     C                   MOVEL     'M'           VH6FIA
269200931022   X3C                   ELSE
269300931206     C                   MOVEL     ' '           VH6FIA
269400931022    3C                   ENDIF
269500011105     C*
269600011109     C                   MOVEL     VI6ITA        VH6ITA
269700011108     C*>>>>>>>>
269800931022    2C                   ENDIF
269900931019     C*
270000931019     C* C O D I C E   T A R I F F A
270100931019     C* RICERCA
270200121221     c                   if        vh6targ <> 'S'
270300931019     C     '?'           SCAN      VI6CTR                                 90
270400931019     C*
270500931021    2C     *IN90         IFEQ      *ON
270600931028     C                   CLEAR                   PARAM3
270700931028     C                   MOVEL     VI4PDR        PA3PDR
270800931028     C                   MOVEL     PARSML        PA3SML
270900931028     C                   MOVEL     VH4TSR        PA3TSR
271000100513     C                   MOVEL     KPJBU         kpjbus
271100100513     C                   clear                   kpjbu
271200931028     C                   MOVEL     PARAM3        KPJBU
271300011112     C                   CALL      'FICN86R'
271400931028     C                   PARM                    KPJBA
271500931110     C                   MOVEL     KPJBU         PARAM3
271600100513     C                   MOVEL     KPJBUs        kpjbu
271700931028     C* SELEZIONE EFFETTUATA
271800931028     C     PA3SEL        IFEQ      ' '
271900931028     C                   MOVEL     PA3CTR        VI6CTR
272000931028     C                   ELSE
272100931028     C                   MOVEL     '000'         VI6CTR
272200931028     C                   ENDIF
272300121221      *
272400931028     C* RIEMETTO I PIEDI DEL VIDEO
272500931019     C                   WRITE     FRB8D07
272600931028     C*
272700000622   X2C                   ELSE
272800931019     C* CONTROLLO
272900931105     C                   TESTN                   VI6CTR               30
273000931105     C     *IN30         IFEQ      *OFF
273100931105     C                   SETON                                        4290
273200931105     C                   GOTO      ENDCT6
273300931105     C                   ENDIF
273400931105     C*
273500931019     C                   MOVEL     VI6CTR        W01CTR
273600121221      *
273700121221      * Controlla la Tariffa
273800931019     C                   EXSR      CTRTAR
273900121221      *
274000000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
274100000622     C* POSTE O BARTOLINI
274200000622     C     *IN90         IFEQ      *OFF
274300000622     C*
274400000622     C     VH6POS        IFEQ      'S'
274500000622     C* E LA TARIFFA È BARTOLINI ERRORE
274600000622     C     FGTFTP        ANDNE     'O'
274700000623     C* ED ESISTE UNA TARIFFA BARTOLINI
274800000623     C     VH4CTB        ANDNE     '   '
274900040622     C*
275000040622     C*  fino al giorno 2004/08/10 il controllo è attivato
275100040622     c                   if        dataoggi < finoadata
275200000622     C                   SETON                                        9054
275300000622     C                   GOTO      ENDCT6
275400040622     c                   end
275500040622     C*
275600000622     C                   ENDIF
275700000622     C*
275800000622     C     VH6POS        IFNE      'S'
275900000622     C* E LA TARIFFA È BARTOLINI ERRORE
276000000622     C     FGTFTP        ANDEQ     'O'
276100000623     C* ED ESISTE UNA TARIFFA POSTE
276200000623     C     VH4CTP        ANDNE     '   '
276300000622     C                   SETON                                        9055
276400000622     C                   GOTO      ENDCT6
276500000622     C                   ENDIF
276600000622     C*
276700000622     C                   ENDIF
276800931021     C*
276900931021    3C     *IN90         IFEQ      *ON
277000931021    4C     *IN30         IFEQ      *OFF
277100000622     C                   SETON                                        40
277200931021   X4C                   ELSE
277300931021     C*
277400931021     C                   MOVEL     VI6CTR        WALF1             1
277500931021     C                   MOVEL     'TR'          COD
277600931021     C                   MOVEL     *BLANKS       KEY
277700931021     C                   MOVEL     WALF1         KEY
277800931021     C     KTAB          CHAIN     TABEL                              30
277900931021     C*
278000931021    5C     *IN30         IFEQ      *ON
278100931021     C     *IN30         OREQ      *OFF
278200931021     C     TBLFLG        ANDNE     ' '
278300931021     C                   SETON                                        42
278400931021   X5C                   ELSE
278500931021     C                   SETOFF                                       90
278600931021    5C                   ENDIF
278700931021    4C                   ENDIF
278800931021    3C                   ENDIF
278900121221     C*
279000000622    2C                   ENDIF
279100121221      *-----
279200121221     c                   endIF
279300121221      *-----
279400931022     C   90              GOTO      ENDCT6
279500020315     C* CONTROLLO NUMERO STOP: NON DEVE ESSERE > O = 9000
279600020315      * attenzione 99999 ha un significato ben preciso come STOP in quanto significa
279700020315      *  consegnato a magazzino.
279800020315     C**** VI6STP        IFGE      800
279900020315     C     VI6STP        IFGE      9000
280000020419     C     VI6STP        oreq      0
280100950711     C                   SETON                                        4890
280200950711     C                   GOTO      ENDCT6
280300950711     C                   END
280400011109     C* >>>>>>>>>
280500011112      *
280600011109     c     tass_boldet   tag
280700011112      *
280800011112      *  flag per determinare da dove è richiamata la tassazione bolle
280900011112      *   se con la "T" oppure con l'"F2" dalla windowsfl in tal caso
281000011112      *    deve preparare i campi della window a ricevere i valori dalla
281100011112      *     tassazione automatica.
281200011112     c                   movel     ' '           bol_tassa         1
281300011109     C* >>>>>>>>>
281400011107     c* D e t t a g l i o    V a l o r i    s u    W i n d o w S f l
281500011107    2c     vi6sce        ifeq      'D'
281600011109    2c**** vi6tot        orne      wh6tot
281700011109    2c**** vi6sce        andne     'T'
281800011107     c                   movel     ' '           vi6sce
281900011108     c                   exsr      WndwSfl
282000011107     c                   endif
282100011107     C* >>>>>>>>>
282200931019     C*
282300931123     C* T A S S A Z I O N E   S I N G O L A   B O L L A
282400931022    2C     VI6SCE        IFEQ      'T'
282500931123     C                   MOVEL     ' '           VI6SCE
282600011108      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
282700011109    3c**** vi6tot        ifeq      0
282800011109    3C****               eval      vi6itt = 0
282900011109    3C****               eval      vi6ita = 0
283000011109     c**********         endif
283100011108      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
283200011105     C*
283300931029     C* SOLO SE UNO DEI 2 IMPORTI E' A 0(E NON INSERITO IN PREC VIDEO)
283400011105      *
283500011107      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
283600011105     C*
283700011109    3C     VI6ITT        IFEQ      0
283800011109     C     *IN15         ANDEQ     *OFF
283900011109     C     VI6ITA        OREQ      0
284000011109     C     *IN16         ANDEQ     *OFF
284100931029     C*
284200020122     C                   CLEAR                   FICNB2
284300020426     C                   MOVEL     'CALCVAL'     §TApgm
284400020213     C                   MOVEL     'S'           §TAtsm
284500020326     C                   MOVEL     vi4pdr        §TAfgs
284600020326     C                   MOVEL     VI4PDR        §TAPDR
284700950117     C                   MOVEL     VH4TSR        §TATSR                         CONSEGNA/RITIRO
284800931029     C                   MOVEL     PARSML        §TASML
284900020326     C                   MOVEL     vh4DDC        §TADDC
285000020326     C                   z-add     vi4ndc        §tandc
285100931202     C                   Z-ADD     1             §TASPP
285200950118     C                   Z-ADD     VH6SET        §TASET
285300950118     C                   MOVE      'S'           §TAPAG
285400020129     C                   z-add     vh6aas        §taaas
285500020129     C                   z-add     ftdlnp        §talnp
285600020129     C                   z-add     ftdnrs        §tanrs
285700020129     C                   z-add     vi6nsp        §tansp
285800950118     C* Tot.colli per piking
285900950118     C     VH4TSR        IFEQ      'C'
286000950118     C* Spedizione da non pagare
286100950119     C     VH6FIA        IFEQ      'B'
286200950118     C                   MOVE      *ZEROS        §TANCL
286300950118     C                   MOVE      *BLANKS       §TAPAG
286400950118     C                   ELSE
286500950118     C                   Z-ADD     FTDNCL        §TANCL
286600950118     C                   END
286700950118     C                   Z-ADD     FTDNCL        §TACPE
286800950118     C                   ELSE
286900950118     C* Tot.colli per etichettatura
287000950118     C                   Z-ADD     FTDNCL        §TANCL
287100950118     C                   Z-ADD     VH6NCE        §TACPE
287200950118     C                   END
287300950127     C                   MOVEL     VI4PEP        §TAPPE
287400021211     C                   movel     vi4FCS        §taFCS
287500950116     C                   Z-ADD     FTDPKL        §TAKFA
287600950116     C                   Z-ADD     FTDVLU        §TATVL
287700931029     C*
287800950118     C                   Z-ADD     VH6IFP        §TAIFP
287900011107     c                   movel     vi6div        §tadiv
288000950118     C                   MOVEL     VI6TSP        §TATSP
288100030320     C                   MOVEL     Vh6cbo        §TAcbo
288200950116     C                   MOVEL     VI6FTC        §TATC1
288300950116     C                   MOVEL     VI6FT2        §TATC2
288400950131     C                   MOVEL     VI6CAP        §TACAP
288500950118     C                   MOVEL     FTDFIN        §TAFIN
288600950118     C                   MOVEL     VH6FGC        §TAFGC
288700950118     C                   MOVEL     VH6GVA        §TAGVA
288800950118     C                   MOVEL     VH6SIC        §TASIC
288900931029     C                   MOVEL     VI6CTR        §TACTR
289000931029    5C     *IN15         IFEQ      *ON
289100950118     C     VH6SIC        OREQ      'S'
289200931029     C                   MOVEL     *HIVAL        §TAITT
289300931029     C                   ELSE
289400011109     C                   Z-ADD     VI6ITT        §TAITT
289500931029    5C                   ENDIF
289600931029     C*
289700931029    5C     *IN16         IFEQ      *ON
289800950119     C     VH6FIA        OREQ      'B'
289900931029     C                   MOVEL     *HIVAL        §TAITA
290000931029     C                   ELSE
290100011109     C                   Z-ADD     VI6ITA        §TAITA
290200931029    5C                   ENDIF
290300021212      *
290400050909     C* Comunque aggancia sempre la bolla in partenza x vedere
290500050909      *  il codice trattamento merce
290600050909     C                   clear                   DS1B
290700091012     c     bolla_ftd     chain     fnblp01l
290800050909     c                   if        %found(fnblp01l)
290900050909     C                   MOVE      blpCbo        §tacbo
291000050909     C                   MOVEL     '1B'          COD
291100050909     C                   MOVEL     *BLANKS       KEY
291200050909     C                   MOVEL     blpCTM        KEY
291300050909     C     KTAB          CHAIN     TABEL00F                           30
291400050909     c                   if        %found(TABEL00F)
291500050909     C                   MOVEL     TBLUNI        DS1B
291600021212      * ------------
291700021212      * deve controllare se abilitata a tariffa Disk C
291800050909     c*******            iF        ftdnrs = 0 and ftdtsr='R' and
291900050909     C*******                      §1BF12 = 'S' and §1Bdkc ='B'
292000050909     c*******            eval      §tadkc = 'S'
292100050909     c*******            end
292200050909      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
292300050909      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
292400050909      *   la tariffa di Etichettatura in generale.
292500050909      *
292600050909      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
292700050909      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
292800050909     c                   iF        ftdTSR = 'R'
292900050909     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
293000050909     c                   move      'N'           §taPPE
293100050909     c                   end
293200050909      * Se è un DISKC Bartolini la regola dice che deve essere comunque
293300050909      * pagata la spedizione al Padroncino
293400050909     c                   iF        §1Bdkc ='B' and ftdnrs = 0
293500050909     c                   eval      §taPPE = 'S'
293600050909     c                   eval      §tadkc = 'S'
293700050909     c                   end
293800050909     c                   end
293900050909      **
294000050909     c                   endiF
294100050909     c                   endiF
294200021212      * ------------
294300950131     C*
294400931124     C     §TAITA        IFEQ      0
294500931124     C     VH4TSR        ANDEQ     'C'
294600950131     C                   Z-ADD     VH6CAS        §TACAS
294700950131     C                   MOVEL     VH6VCA        §TAVCA
294800950131     C                   Z-ADD     VH6AAS        FTDAAS
294900950131     C                   Z-ADD     VI6NSP        FTDNSP
295000020129     C*
295100950131     C* CONTROLLO SE E' C/A PRONTAMENTE RECUPERATO
295200931124     C                   EXSR      CTRASS
295300931124     C                   ENDIF
295400090909     C*
295500090909     C*   Deve passare il codice del cliente solo se è un cliente particolare
295600090909     c     vh6KSC        lookup    KcliPAR                                30
295700090909     c                   if        *in30
295800090909     C                   Z-ADD     vh6KSC        §TAcliPAR
295900090909     C                   end
296000931029     C*
296100020122     C                   CALL      'FICNB2R'
296200020122     C                   PARM                    FICNB2
296300931029     C*
296400011107     C* di ritorno dall'elaborazione:
296500011107     C*
296600931029    5C     §TAERR        IFEQ      ' '
296700011107     c                   z-add     0             vi6tot
296800011109     c******             z-add     0             wh6tot
296900931108     C     *IN15         IFEQ      *OFF
297000950118     C     VH6SIC        ANDNE     'S'
297100011112      *
297200931108     C                   MOVEL     §TAITT        VH6ITT
297300931108     C                   ADD       §TATPE        VH6ITT
297400931108     C                   ADD       §TATST        VH6ITT
297500011107      * >>>>>>> su campo SFLWDW 1° riga
297600011109     C                   Z-ADD     VH6ITT        VI6ITT
297700011107     c                   add       vh6itt        vi6tot
297800011109     c******             add       vh6itt        wh6tot
297900011112      *
298000931108     C                   ENDIF
298100011112      *
298200950118     C     *IN16         IFEQ      *OFF
298300950119     C     VH6FIA        ANDNE     'B'
298400011112      *
298500011112     C                   MOVEL     §TAITA        VH6ITA
298600011112     C                   MOVEL     §TAITA        VI6ITA
298700011112      * >>>>>>> su campo SFLWDW 2° riga
298800011112     c                   add       vh6ita        vi6tot
298900011112     c******             add       vh6ita        wh6tot
299000011112      *
299100950118     C                   END
299200011112      *
299300931029   X5C                   ELSE
299400931029     C* ERRORE IN TASSAZIONE
299500931019     C                   SETON                                        4190
299600931022     C                   GOTO      ENDCT6
299700931029    5C                   ENDIF
299800931029    3C                   ENDIF
299900931029    2C                   ENDIF
300000011109     c*>>>>>>>>>>>>>
300100011109     c* se richiesta a tassazione dal dettaglio nella windsfl
300200011109     c*  deve tornare sul dettaglio della windowsfl.
300300011109     c                   if        bol_tassa = 'D'
300400011109     c                   eval      vi6sce = 'D'
300500011109     c                   goto      tass_boldet
300600011109     c                   endif
300700011109     c*>>>>>>>>>>>>>
300800011109     C*
300900931123     C* SE  S O M M A   P E S I   A   G I O R N A T A
301000931123    2C     VH4TSM        IFEQ      'G'
301100011109     C     VI6ITT        ANDEQ     0
301200950119     C     VH6FIA        ANDNE     'B'
301300950118     C     VH6SIC        ANDNE     'S'
301400931123     C                   MOVEL     VI6CTR        WALF1
301500931123     C* NON ACCETTO PIU' DI 1 COD TAR A KILO O A QUINTALE
301600931123     C*  OPPURE DEVO VALORIZZARE SUBITO LA BOLLA
301700931123    3C     WALF1         IFEQ      '0'
301800931123     C     WALF1         OREQ      '3'
301900931123     C*
302000931123    4C     WALF3         IFEQ      *BLANKS
302100931123     C                   MOVEL     VI6CTR        WALF3
302200931123   X4C                   ELSE
302300931123    5C     VI6CTR        IFNE      WALF3
302400931123     C                   SETON                                        4390
302500931123     C                   GOTO      ENDCT6
302600931123    5C                   ENDIF
302700931123    4C                   ENDIF
302800931123     C*
302900931123    3C                   ENDIF
303000931123    2C                   ENDIF
303100931019     C*
303200931123     C* V I S U A L I Z Z A Z I O N E   S I N G O L A   B O L L A
303300931022    2C     VI6SCE        IFEQ      'V'
303400931123     C                   MOVEL     ' '           VI6SCE
303500931123    3C     VH4TSR        IFEQ      'C'                                           CONSEGNE
303600931021     C                   MOVEL     VH6AAS        PA2AAS
303700931021     C                   MOVEL     FTDLNP        PA2LNP
303800931021     C                   MOVEL     FTDNRS        PA2NRS
303900931207     C                   MOVEL     VI6NSP        PA2NSP
304000931105     C                   MOVEL     '1'           PA2FLG
304100100513     C                   MOVEL     KPJBU         kpjbus
304200100513     C                   clear                   kpjbu
304300931021     C                   MOVEL     PARAM2        KPJBU
304400950116     C                   CALL      'FNLR36R'
304500931021     C                   PARM                    KPJBA
304600100513     C                   MOVEL     KPJBUs        kpjbu
304700931021     C*
304800931021     C                   WRITE     FRB8D07
304900931022     C                   SETON                                        90
305000931123     C                   GOTO      ENDCT6
305100931123    3C                   ENDIF
305200020326     C*
305300020326    3C     VH4TSR        ifeq      'R'                                           ritiri nuovi
305400020326    3C     vh6nsp        andgt     0
305500020326     C                   CLEAR                   DSLS05
305600020326     C                   MOVE      VH6AAS        PA0AAS
305700020326     C                   MOVE      FTDLNP        PA0LNP
305800020326     C                   MOVE      FTDNRS        PA0NRS
305900020326     C                   MOVE      VI6NSP        PA0NSP
306000020326     C                   MOVEL     RAGUT         PA0RSU
306100020326     C                   MOVE      SIMFEL        PA0SIM
306200020326     C********           MOVE      WCML          PA0CML
306300100513     C                   MOVEL     KPJBU         kpjbus
306400100513     C                   clear                   kpjbu
306500020326     C                   MOVEL     DSLS05        KPJBU
306600020326     C                   CALL      'FNLS05R'
306700020326     C                   PARM                    KPJBA
306800100513     C                   MOVEL     KPJBUs        kpjbu
306900020326     C*
307000020326     C                   WRITE     FRB8D07
307100020326     C                   SETON                                        90
307200020326     C                   GOTO      ENDCT6
307300020326    3C                   end
307400020326     C*
307500931123    2C                   ENDIF
307600931019     C*
307700931022     C     ENDCT6        TAG
307800931105     C* BLOCCATO
307900950119     C     VH6FIA        IFEQ      'B'
308000931105     C                   SETON                                        13
308100931105     C                   ELSE
308200931105     C                   SETOFF                                       13
308300931105     C                   ENDIF
308400950117     C* RITORNO ALL'INCASSO
308500950117     C     VH6SIC        IFEQ      'S'
308600950117     C                   SETON                                        23
308700950117     C                   ELSE
308800950117     C                   SETOFF                                       23
308900950117     C                   ENDIF
309000931105     C*
309100931019     C                   Z-ADD     NRR6          REC6
309200931019     C                   UPDATE    FRB8SF6
309300931019     C*
309400931019     C   90              SETON                                        32
309500931019     C  N90              READC     FRB8SF6                                32
309600931019    1C                   ENDDO
309700931203     C*
309800931203     C*  SE: 90 OFF - NO ERRORE
309900931203     C*             - SE TROVATO ALMENO UN CODICE TARIFFA 0XX O 3XX
310000931203     C*             - TIPO SOMMA PESI A GIORNATA
310100931203     C*             - NO VARIATO TUTTE LE RIGHE DELLA DISTINTA
310200931203    1C     *IN90         IFEQ      *OFF
310300931203     C     WALF3         ANDNE     *BLANKS
310400931203     C     VH4TSM        ANDEQ     'G'
310500931203     C     WREAD         ANDNE     WFTD
310600931203     C*
310700931203     C                   MOVEL     W01CTP        WALF1
310800931203    2C     WALF1         IFEQ      '0'
310900931203     C     WALF1         OREQ      '3'
311000931203     C*
311100931203    3C     W01CTP        IFNE      WALF3
311200931203     C                   SETON                                        4390
311300931203     C                   CLEAR                   WALF3
311400931203     C     1             CHAIN     FRB8SF6                            30
311500931203     C                   GOTO      ENDCT6
311600931203    3C                   ENDIF
311700931203    2C                   ENDIF
311800931203    1C                   ENDIF
311900931203     C*
312000931021     C                   SETOFF                                       36
312100931019     C*
312200931021     C                   ENDSR
312300020212     C*---------------------------------------------------------------*
312400011108     C* Emette la finestra con il dettaglio degli importi tariffe ----*
312500020212     C*---------------------------------------------------------------*
312600011108     c     WndwSfl       begsr
312700011108     c*
312800011108     c     *like         define    w6ctot        wwctot
312900011108     c*
313000011108     c                   seton                                        35
313100011108     c                   write     frb8wc6
313200011123     c                   setoff                                       3561
313300011108     c*
313400011108     c                   clear                   nrw6
313500011108     c                   z-add     1             wrc6
313600011108     c                   z-add     ftdlnp        w6clnp
313700011108     c                   z-add     ftdnrs        w6cnrs
313800011218     c                   z-add     vi6nsp        w6cnsp
313900011108     c                   move      vi6ftc        w6cftc
314000011108     c                   move      vi6ft2        w6cft2
314100011108     c                   move      vi6con        w6ccon
314200011108     c                   move      vi6ia         w6cin
314300011108     c                   move      vi6div        w6cdiv
314400011108     c                   move      vi6cap        w6ccap
314500011108     c                   move      ftdfin        w6cfin
314600011108     c                   z-add     ftdncl        w6cncl
314700011108     c                   z-add     ftdpkl        w6cpkl
314800011108     c                   z-add     ftdvlu        w6cvlu
314900011108     c                   move      vi6tot        w6ctot
315000020128     c                   z-add     vi6itt        w6cbas
315100020128     c                   z-add     vi6ita        w6cacc
315200011109     c*------->
315300020327     c* Carica le 2 righe x importo Base e Accessori
315400020327     c                   do        *hival
315500020327     c* protect/no display
315600020327     c                   setoff                                       60
315700020327     c* riga del sfl
315800020327     c                   add       1             nrw6
315900020327     c*  deve caricare solo 2 righe
316000020327     c                   if        nrw6 > 2
316100020327     c                   leave
316200020327     c                   endif
316300020327     c*
316400020327     c                   z-add     nrw6          w1htrc
316500020327     c*
316600020327     c                   select
316700020327     c     nrw6          wheneq    1
316800020327     c     vp6itt        comp      '1'                                    60
316900020327     c                   movel     *blank        w1tipo
317000020327     c                   movel     'Base'        w1tipo
317100020327     c                   z-add     vi6itt        w1valr
317200020327     c                   z-add     vi6itt        w1hval
317300020327     c*
317400020327     c     nrw6          wheneq    2
317500020327     c     vp6ita        comp      '1'                                    60
317600020327     c                   movel     *blank        w1tipo
317700020327     c                   movel     'Accessori'   w1tipo
317800020327     c                   z-add     vi6ita        w1valr
317900020327     c                   z-add     vi6ita        w1hval
318000020327     c                   endsl
318100020327     c*
318200020327     c                   write     frb8ws6
318300020327     c                   enddo
318400011109     c*-------> Emissione Window
318500011108     c     resta_wdw     tag
318600011109      *                *----------------------*
318700011108     c                   write     frb8wd6
318800011108     c                   exfmt     frb8wc6
318900011109      *                *----------------------*
319000011123     C                   setoff                                       61
319100011108     c*  Controlli
319200011109      * ---------         ----------
319300020327     c* Cntrl le righe x importi modificati
319400020327     c                   z-add     0             nrw6
319500020327     c                   z-add     0             wwctot
319600020327      *------------        ---------------
319700020327     c* cicla sulle righe
319800020327     c                   do        *hival
319900020327     c                   add       1             nrw6
320000020327     c*
320100020327     c*  deve caricare solo 2 righe
320200020327     c                   if        nrw6 > 2
320300020327     c                   leave
320400020327     c                   endif
320500020327     c*
320600020327     c     nrw6          chain     frb8ws6
320700020327      *  totalizza le righe
320800020327     c                   add       w1valr        wwctot
320900020327     c* protect/no display
321000020327     c                   setoff                                       60
321100020327      *  campi protetti
321200020327     c     nrw6          ifeq      1
321300020327     c     vp6itt        comp      '1'                                    60
321400020327     c                   end
321500020327     c     nrw6          ifeq      2
321600020327     c     vp6ita        comp      '1'                                    60
321700020327     c                   end
321800020327      *
321900020327      *  x F8 -> Conferma
322000020327      *  valori modificati manualmente
322100020327     c   KB
322200020327     cor KH              IF        w1hval <>  w1valr
322300020327      *
322400020327     c                   MOVEL     ' '           flgfi
322500020327    3c                   if        w1valr > 0
322600020327     c                   MOVEL     'M'           flgfi             1
322700020327    3c                   end
322800020327      *
322900020327     c                   Select
323000020327      * Base
323100020327     c     nrw6          wheneq    1
323200020327     C                   movel     flgfi         VH6FIT
323300020327     C                   MOVEL     w1valr        vi6itt
323400020327     C                   MOVEL     vi6itt        VH6ITT
323500020327      * Accessori
323600020327     c     nrw6          wheneq    2
323700020327     C                   movel     flgfi         VH6FIA
323800020327     C                   MOVEL     w1valr        vi6ita
323900020327     C                   MOVEL     vi6ita        VH6ITA
324000020327      *
324100020327     c                   endsl
324200020327      *
324300020327     C                   z-add     w1valr        w1hval
324400020327     c                   endif
324500020327      *
324600020327     C                   move      w1valr        trebytes          3 0
324700020327     C                   if        w6cdiv ='ITL' and
324800020327     c                             trebytes > 0
324900020327     c                   seton                                        61
325000020327     c                   end
325100020327      *
325200020327     c                   update    frb8ws6
325300020327     c   61              leave
325400020327     c                   enddo
325500020327      * ---------         ----------
325600020327     c* resta sulla window se non ha premuto dei cmd
325700020327     c     *inkh         ifeq      *off
325800020327     c     *inkl         andeq     *off
325900020327     c     *inkb         andeq     *off
326000020327     c     *in61         oreq      *on
326100020327     c     *inkl         andeq     *off
326200020327     c                   goto      resta_wdw
326300020327     c                   end
326400020327      *
326500020327      * reimposta il totale delle singole righe
326600020327     c     *inkh         ifeq      *on
326700020327     c                   if        wwctot <>  vi6tot
326800020327     c*******                      or wwctot <>  wh6tot
326900020327     c                   z-add     wwctot        vi6tot
327000020327     c*******            z-add     wwctot        wh6tot
327100020327     c                   endif
327200020327     c                   end
327300020327     c*
327400020327     c*  Se richiesta la tassazione bolla in automatico
327500020327     c*   come se avesse chiesto "T" sulla scelta del SFL principale
327600020327     c*    e successivamente "D" di dettaglio.
327700020327     c     *inkb         ifeq      *on
327800020327     c                   move      'T'           vi6sce
327900020327     c                   move      'D'           bol_tassa
328000020327     c                   end
328100020327     c*
328200011109     c*
328300011108     c                   endsr
328400020212     c*---------------------------------------------------------------*
328500011108     C* VEDO SE E' C/A PRONTAMENTE RECUPERATO ------------------------*
328600020212     C*---------------------------------------------------------------*
328700011108     C     CTRASS        BEGSR
328800011108     C*
328900950131     C                   MOVE      *ZEROS        §TACA1
329000950131     C                   MOVE      *BLANKS       WTACPC            1
329100931124     C* SOLO SE INCASSATO IL C/A
329200950131    1C     §TACAS        IFGT      0
329300931124     C*
329400931124     C                   CLEAR                   WNUM3
329500931124     C                   SETOFF                                       09
329600931124     C*
329700931124     C* PER ESEGUIRLO EVENTUALEMTE 2 VOLTE
329800931124    2C     WNUM3         DOWLE     2
329900931124     C                   ADD       1             WNUM3
330000931124     C*
330100931124    3C     *IN09         IFEQ      *OFF                                          CON FTD
330200091012     C     Bolla_FTD     SETLL     FNARBK00
330300091012     C     Bolla_FTD     READE     FNARBK00                               30
330400931124   X3C                   ELSE
330500950116     C     KLBL          SETLL     FNARBK00                                      CON MAMMA
330600950116     C     KLBL          READE     FNARBK00                               30
330700931124    3C                   ENDIF
330800931124     C*
330900931124    3C     *IN30         DOWEQ     *OFF
331000931124     C*
331100931124     C* SE INSERITO COMPLETAMENTE OK
331200030418    4C     kARBCVB       IFEQ      'K5'
331300950131     C                   Z-ADD     §TACAS        §TACA1
331400950131     C                   MOVEL     'S'           WTACPC
331500931124     C                   SETON                                        30
331600931124   X4C                   ELSE
331700931124     C*
331800931124     C* SE MODIFICATO IMPORTO L'ASSEGNO SOLTANTO SE > 0 E PER LA DIFFER
331900030418    5C     kARBCVB       IFEQ      'KP'
332000931124     C                   SETON                                        30
332100030418     C     §TACAS        SUB       kARBCAS       §TACA1
332200950131    6C     §TACA1        IFLE      0
332300950131     C                   Z-ADD     0             §TACA1
332400931124   X6C                   ELSE
332500950131     C                   MOVEL     'S'           WTACPC
332600931124    6C                   ENDIF
332700931124    5C                   ENDIF
332800931124    4C                   ENDIF
332900931124     C*
333000950116     C  N30
333100091012     CANN09Bolla_FTD     READE     FNARBK00                               30
333200950116     C  N30
333300950116     CAN 09KLBL          READE     FNARBK00                               30
333400931124    3C                   ENDDO
333500931124     C*
333600931124     C* SE NON TROVATO NULLA GUARDO SE HA UNA MAMMA E CERCO SULLA MAM
333700950131    3C     WTACPC        IFEQ      ' '
333800931124     C     WNUM3         ANDEQ     1
333900931124     C*
334000091012     C     Bolla_FTD     SETLL     FNLBL000                               30
334100931124    4C     *IN30         IFEQ      *OFF
334200030418     C* per uscire dal ciclo
334300950119     C                   Z-ADD     3             WNUM3             3 0
334400931124   X4C                   ELSE
334500931124     C                   ADD       1             WNUM3
334600091012     C     Bolla_FTD     CHAIN     FNLBL000                           30
334700030418     C* deve eseguire solo se le linee di arrivo sono uguali fra madre e figlia
334800030418     C*  altrimenti forza l'uscita dal ciclo
334900030418    4c  n30              if        lbllan <> lbllap
335000030418     c                   leave
335100030418     c                   end
335200931124     C                   SETON                                        09
335300931124    4C                   ENDIF
335400931124     C*
335500931124     C                   ELSE
335600030418     C* per uscire dal ciclo
335700931124     C                   Z-ADD     3             WNUM3
335800931124    3C                   ENDIF
335900931124    2C                   ENDDO
336000931124    1C                   ENDIF
336100931029     C                   ENDSR
336200020212     C*---------------------------------------------------------------*
336300931029     C* VEDO SE E' RICONSEGNA GIACENZA -------------------------------*
336400020212     c*---------------------------------------------------------------*
336500000623     C***        CTRGIA    BEGSR
336600091012     C*          Bolla_FTD CHAINFIGCP000             30
336700000623     C*          *IN30     IFEQ *OFF
336800000623     C*          GCPATB    ANDEQ' '
336900000623     C*                    MOVEL'S'       FTDFGC
337000000623     C*                    ELSE
337100000623     C*                    MOVEL'N'       FTDFGC
337200000623     C*                    ENDIF
337300000623     C*                    ENDSR
337400020212     C*---------------------------------------------------------------*
337500950116     C* AGGIORNAMENTO BOLLE DISTINTA/RITIRO --------------------------*
337600020212     C*---------------------------------------------------------------*
337700931021     C     AGGIO6        BEGSR
337800931021     C                   Z-ADD     1             NRR6
337900931021     C                   READC     FRB8SF6                                32
338000931109     C*
338100931109     C* SE GIA' VALORIZZATE AGGANCIO LA TESTATA
338200931112     C     VI1FVL        IFEQ      'V'
338300931123     C* SE SOMMA PESI A GIORNATA DEVO RICONFERMARE LA VALUTAZIONE
338400931123     C  N32VH4TSM        IFEQ      'G'
338500931123     C                   SETON                                        22
338600931123     C                   ENDIF
338700011105     C     KFTD          CHAIN     FIFTT000                           30
338800931109     C                   ENDIF
338900931021     C*
339000931021    1C     *IN32         DOWEQ     *OFF
339100020313     C***19KFTD4*****    CHAIN     fiftd004                           30
339200020313     C**N19KFTD2*****    CHAIN     fiftd002                           30
339300020313      *
339400020313      * per distinguere il vecchio modo di lettura dei ritiri non è più possibile
339500020313      * utilizzare l'ind.19 ma il campo numero spedizione.
339600020313     c                   if        vh6nsp>0
339700020315     c                   if        vh4tsr='R'
339800020315      * vista per i nuovi ritiri x spedizione
339900020404     C     KFTD5         CHAIN     fiftd005                           32
340000020315     c                   else
340100020315      * seleziona solo i consegnati
340200020313     C     KFTD2         CHAIN     fiftd002                           30
340300020315     c                   end
340400020313     c                   else
340500020313     C     KFTD4         CHAIN     fiftd004                           30
340600020313     c                   end
340700931112     C*
340800020313    2C     *IN30         IFEQ      *OFF
340900931109     C*
341000931109     C* SE GIA' VALORIZZATE DEVO AGGIORNARE I CAMPI DI TOTALE
341100931109     C     VI1FVL        IFEQ      'V'
341200950118     C  N15VH6SIC        IFNE      'S'
341300931109     C                   SUB       FTDITT        FTTITT
341400011109     C                   ADD       VI6ITT        FTTITT
341500931110     C                   SETON                                        22
341600931109     C                   ENDIF
341700931110     C*
341800950119     C  N16VH6FIA        IFNE      'B'
341900931109     C                   SUB       FTDITA        FTTITA
342000011109     C                   ADD       VI6ITA        FTTITA
342100950118     C                   SETON                                        22
342200931109     C                   ENDIF
342300931109     C                   ENDIF
342400931109     C*
342500931029     C* SE IMPOSTATO VALORE DISTINTA BASE AZZERO SULLA BOLLA
342600931029     C     *IN15         IFEQ      *OFF
342700011109     C                   MOVEL     VI6ITT        FTDITT
342800931021     C                   MOVEL     VH6FIT        FTDFIT
342900931029     C                   ENDIF
343000931029     C* SE IMPOSTATO VALORE DISTINTA ACCESSORI AZZERO SULLA BOLLA
343100931029     C     *IN16         IFEQ      *OFF
343200011109     C                   MOVEL     VI6ITA        FTDITA
343300931021     C                   MOVEL     VH6FIA        FTDFIA
343400931029     C                   ENDIF
343500011030     C*****VI6CTR        IFNE      VI4CTR
343600011030     C     VI6CTR        IFNE      VI4CTD
343700011106     c     vh4tpr        andeq     1
343800931022     C                   MOVEL     VI6CTR        FTDCTP
343900931109     C                   ENDIF
344000011123     C                   z-add     VI6STP        FTDSTP                          STOP
344100020326     C*
344200020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
344300020326     C* i primi tre caratteri
344400020326     c                   if        ftdfgs = 0
344500020326     c                   movel     ftdpdr        ftdfgs
344600020326     c                   end
344700020326     C*
344800931202     C*
344900020313     C***19******        UPDATE    fiftd004
345000020313     C**N19******        UPDATE    fiftd002
345100020313     c                   if        vh6nsp > 0
345200020315     c                   if        vh4tsr='R'
345300020315     C                   UPDATE    fiftd005
345400020315     c                   else
345500020313     C                   UPDATE    fiftd002
345600020315     c                   end
345700020313     c                   else
345800020313     C                   UPDATE    fiftd004
345900020313     c                   end
346000020313     C*
346100931021    2C                   ENDIF
346200931021     C                   READC     FRB8SF6                                32
346300931021    1C                   ENDDO
346400931021     C*
346500931123     C* RIAGGIORNO LA TESTATA (PER EVITARE CHE NON SIA AGGIORNATA SE
346600931109     C*  ESCONO CON CMD12)
346700931109     C     VI1FVL        IFEQ      'V'
346800020326     C*
346900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
347000020326     C* i primi tre caratteri
347100020326     c                   if        fttfgs = 0
347200020326     c                   movel     fttpdr        fttfgs
347300020326     c                   end
347400020326     C*
347500011105     C                   UPDATE    FIFTT000
347600931109     C                   ENDIF
347700931109     C*
347800931021     C                   ENDSR
347900020212     C*---------------------------------------------------------------*
348000931019     C* CONTROLLO CODICE TARIFFA -------------------------------------*
348100020212     C*---------------------------------------------------------------*
348200931019     C     CTRTAR        BEGSR
348300091112     C*
348400931123     C                   Z-ADD     1             C
348500931123     C     VH4TSR        LOOKUP    TSR(C)                                 30
348600931123     C  N30              Z-ADD     1             C
348700931123     C*
348800931123     C* SE E' IL CODICE TARIFFA GENERICO --> ERRORE
348900931123    1C     W01CTR        IFEQ      CTS(C)
349000931123     C                   SETON                                        90
349100931123   X1C                   ELSE
349200931123     C*
349300011112     C     KFGT2         CHAIN     FIFGT000                           30
349400011112     C*
349500011112     C* Modificato con le nuove regole di data scadenza e valuta
349600011112     C*  il modo di reperire il record valido.
349700011112    2C     *in30         doweq     *off
349800091112      *
349900011112     C     fgtatb        ifne      'A'
350000011112     C     w02div        andeq     fgtdiv
350100091112     C**** w02ddc        andle     fgtdst
350200091112     C**** w02ddc        andge     fgtddt
350300091112      *
350400091112     C* Se tariffa del periodo
350500091112     C     Ktgt          CHAIN     FItgt01l
350600091112 2/3 c                   if        %Found(FItgt01l) and
350700091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
350800091112      *
350900091112    3c     parsml        IFeq      *blank
351000091112     C     tgtDTS        aNDgt     0
351100091112     C**** fgtDTS        aNDgt     0
351200091112     c     parsml        OReq      'S'
351300091112     C                   leave
351400091112    3C                   end
351500091112      *
351600091112 2/3 C                   end
351700091112      *
351800091112    2C                   end
351900091112      *
352000011112     C     kfgt2         reade     fifgt000                               30
352100011112    2C                   enddo
352200931019     C*
352300091112     C*  ????????????  strano test se trovato ritesta la data se minore della decorrenza ?????
352400931123    2C     *IN30         IFEQ      *ON
352500931019     C     *IN30         OREQ      *OFF
352600091112     C     W02DDC        ANDLT     tgtDDT
352700091112     C**** W02DDC        ANDLT     FGTDDT
352800931019     C                   SETON                                        90
352900931123    2C                   ENDIF
353000091112     C*
353100931123    1C                   ENDIF
353200931021     C*
353300931021     C* 90 ON - TARIFFA INESISTENTE
353400931028     C*         VEDO SE HA ALMENO UN CODICE TARIFFA VALIDO
353500931021    1C     *IN90         IFEQ      *ON
353600931108     C* CERCO IL CODICE TARIFFA GENERICO DEL PRESTAZ DA ESCLUDERE
353700931028     C                   SETOFF                                       31
353800931028     C*
353900011112     C     KFGT          CHAIN     FIFGT000                           30
354000091112      *
354100091112     C*   in testata di periodo
354200091112     C  n30Ktgt          CHAIN     FItgt01l
354300091112 1/2 c  n30              if        %Found(FItgt01l)
354400931028     C*
354500931028     C* SE ANNULLATO O E' IL CODICE TARIFFA GENERICO RILEGGO
354600011112      * o se non è della stessa divisa o se scaduto
354700931022    2C  N30FGTATB        IFEQ      'A'
354800931028     C     FGTCTR        OREQ      CTS(C)
354900011112     C     fgtdiv        ORne      w02div
355000091112     C**** fgtdst        ORgt      w02ddc
355100091112     C**** fgtDTS        oreq      0
355200091112     C     tgtdst        ORgt      w02ddc
355300091112     C     tgtDTS        oreq      0
355400091112     c     parsml        andeq     *blank
355500931021     C                   SETON                                        31
355600931028    2C                   ENDIF
355700091112 1/2 c                   end
355800931021     C*
355900931021    2C     *IN30         DOWEQ     *OFF
356000931021     C     *IN31         ANDEQ     *ON
356100931021     C*
356200011112     C     KFGT          READE     FIFGT000                               30
356300091112      *
356400091112     C*   in testata di periodo
356500091112     C  n30Ktgt          CHAIN     FItgt01l
356600091112 2/3 c  n30              if        %Found(FItgt01l)
356700091112     C*
356800931022    3C  N30FGTATB        IFEQ      'A'
356900931028     C     FGTCTR        OREQ      CTS(C)
357000011112     C     fgtdiv        ORne      w02div
357100091112     C***  fgtdst        ORgt      w02ddc
357200091112    3C***  fgtDTS        oreq      0
357300091112     C     tgtdst        ORgt      w02ddc
357400091112    3C     tgtDTS        oreq      0
357500020315    3c     parsml        andeq     *blank
357600931021     C                   SETON                                        31
357700931028     C                   ELSE
357800931028     C                   SETOFF                                       31
357900931021    3C                   ENDIF
358000931021     C*
358100091112 2/3 c                   end
358200091112     C*
358300931021    2C                   ENDDO
358400931021    1C                   ENDIF
358500931028     C*
358600931028     C* 90 ON 30 ON - NON ESISTONO TARIFFE PER IL CLIENTE
358700931021     C*
358800931019     C                   ENDSR
358900020212     C*---------------------------------------------------------------*
359000931111     C* CONTROLLO SE MANCATA CONSEGNA DA TASSARE ---------------------*
359100020212     C*---------------------------------------------------------------*
359200931111     C     CTRCMC        BEGSR
359300950116     C                   MOVE      VI4FIL        WALF6
359400931111     C     WALF6         LOOKUP    CMC                                    30
359500931111     C* SE INESISTENTE CERCO CON FILIALE A 0
359600931111     C     *IN30         IFEQ      *OFF
359700931111     C                   MOVE      000           WALF6
359800931111     C     WALF6         LOOKUP    CMC                                    30
359900931111     C                   ENDIF
360000931111     C                   ENDSR
360100020212     C*---------------------------------------------------------------*
360200931111     C* CONTROLLO CODICE BOLLA DA NON TASSARE ------------------------*
360300020212     C*---------------------------------------------------------------*
360400931111     C     CTRCBL        BEGSR
360500950116     C                   MOVEL     W018C         WALF6             6
360600950116     C                   MOVE      VI4FIL        WALF6
360700950116     C     WALF6         LOOKUP    CBL                                    31
360800931111     C* SE INESISTENTE CERCO CON FILIALE A 0
360900931111     C     *IN31         IFEQ      *OFF
361000950116     C                   MOVE      000           WALF6
361100950116     C     WALF6         LOOKUP    CBL                                    31
361200931111     C                   ENDIF
361300931111     C                   ENDSR
361400020212     C*---------------------------------------------------------------*
361500931021     C* ESEGUO VALORIZZAZIONE ----------------------------------------*
361600020212     C*---------------------------------------------------------------*
361700931021     C     VALZZ         BEGSR
361800931110     C* SPENGO IL 22 CHE OBBLIGA IL CMD6
361900931110     C                   SETOFF                                       22
362000931021     C                   Z-ADD     1             NRR4
362100931021    0C     NRR4          DOWLE     W04NRR
362200931021     C     NRR4          CHAIN     FRB8SF4                            32
362300931021     C*
362400931021    1C     *IN32         IFEQ      *OFF
362500931021     C*
362600931123     C* R E C O R D   D E L   T I P O   P R E S T A Z I O N E:
362700931029     C* ESCLUDO (MEMORIZZO IL COD.TARIFFA)
362800931021    2C     VH4NRR        IFEQ      0
362900011106     c                   if        vh4tpr = 1
363000011030     C                   MOVEL     VI4CTD        WALF3
363100011105     c                   end
363200000623     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
363300000623     C     VH4CTP        IFNE      *BLANKS
363400000623     C     VH4CTB        ANDNE     *BLANKS
363500000623     C                   MOVEL     VH4CTP        WCTP              3
363600000623     C                   MOVEL     VH4CTB        WCTB              3
363700000623     C                   ELSE
363800000623     C                   MOVEL     *BLANKS       WCTP
363900000623     C                   MOVEL     *BLANKS       WCTB
364000000623     C                   ENDIF
364100931029     C*
364200931021   X2C                   ELSE
364300931022     C*
364400931029     C* V A L O R I   B A S E   E   A C C E S S O R I   D I S T I N TA:
364500931029     C*  SE CI SONO ENTRAMBI NON FACCIO NULLA
364600931022    3C     VI4ITT        IFEQ      0
364700931022     C     VI4ITA        OREQ      0
364800931103     C*
364900931103     C* 15 ON - IMMESSA TARIFFA BASE
365000931103     C* 16 ON - IMMESSA TARIFFA ACCESSORI
365100931103     C     VI4ITT        COMP      0                                  15
365200931103     C     VI4ITA        COMP      0                                  16
365300931105     C* PULIZIA CAMPI DI TOTALE
365400931125     C                   CLEAR                   W01CLA
365500020212     C                   CLEAR                   g01CLA
365600950118     C                   CLEAR                   W01CPE
365700020212     C                   CLEAR                   g01CPE
365800931125     C                   CLEAR                   W01KFA
365900020212     C                   CLEAR                   g01KFA
366000931105     C                   CLEAR                   W01NCL
366100931105     C                   CLEAR                   W01TVL
366200020212     C                   CLEAR                   g01TVL
366300931105     C                   CLEAR                   W01SNA
366400020212     C                   CLEAR                   g01SNA
366500931105     C                   CLEAR                   W01STP
366600931105     C                   CLEAR                   W02STP
366700931123     C                   CLEAR                   W01CTP
366800931105     C                   CLEAR                   W01FIA
366900931105     C                   CLEAR                   W01FIT
367000931105     C                   CLEAR                   W01ITT
367100931105     C                   CLEAR                   W01ITA
367200950119     C                   CLEAR                   W01SNE
367300931202     C                   CLEAR                   W01SNP
367400931103     C*
367500931103     C* C O D I C E   T A R I F F A   I N E S I S T E N T E :
367600931103    4C     WALF3         IFEQ      '???'
367700931105     C*  VEDO SE DARE MANCA TARIFFA SOLO SE NON E' IMPOSTATA LA BASE
367800931105     C     *IN15         IFEQ      *OFF
367900931104     C                   MOVEL     SER(2)        W01ERR           25
368000931103     C                   EXSR      MANTAR
368100931105     C                   ENDIF
368200931103     C*
368300931103   X4C                   ELSE
368400931029     C*
368500931108     C     VH4TSR        IFEQ      'R'                                           RITIRI
368600931108     C                   SETON                                        19
368700931108     C                   ELSE
368800931108     C                   SETOFF                                       19         CONSEGNE
368900931108     C                   ENDIF
369000020325     C*
369100020325     C*  prima del ciclo di lettura di dettaglio resetta
369200020325     C*   i flags o i contatori
369300020325     c                   move      *blank        w01isola
369400020325     c                   move      *blank        w01xpres
369500030109     c                   move      *blank        w01disagiato      1
369600020325     c                   z-add     0             almuno_xpr
369700020325     c                   z-add     0             almuno_iso
369800030109     c                   z-add     0             almuno_disag
369900030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
370000030821     C*  lo stop.
370100030821     c                   z-add     0             W01nss
370200030821     c                   z-add     0             W01fw
370300030821     c                   z-add     0             W01rcc
370400030821     c                   move      *loval        savSTP
370500041124     C*
370600041124     C*   Comunque prima di valorizzare per sicurezza pulisce eventuali
370700041124     C*   records del FIFCE      *-------------*
370800041124     c                   exsr      Annulla_FCE
370900041124     C*                          *-------------*
371000931108     C*
371100011105     C     KFTD          SETLL     fiftd001
371200011105     C     KFTD          READE     fiftd001                               32
371300931021     C*
371400931104    5C     *IN32         DOWEQ     *OFF
371500020328     C*
371600030821     C*  a rottura di STOP conta quante spedizioni ci sono
371700030821     C*   all'interno dello stesso STOP
371800030821     c                   if        ftdSTP <> savSTP
371900030821     C*                          *-------------*
372000030821     c                   exsr      Sped_x_Stop
372100030821     C*                          *-------------*
372200030821     c                   eval         savSTP = ftdSTP
372300030821     c                   end
372400030821     C*
372500020422     C*               *--------------------*
372600950118     C                   EXSR      CTRTSM
372700020422     C*               *--------------------*
372800020328     C*
372900020328     C* scrive record x record sul workF
373000020423     C*  soo se hanno lo stop e se hanno un peso.
373100020620     c                   if        ftdstp <>0  and
373200020620     c                             ftdpkl <>0  and
373300020422    3C                             parsml = ' '
373400020328     C                   EXSR      wrt_FTDW
373500020419     c                   end
373600931021     C*
373700011105     C     KFTD          READE     fiftd001                               32
373800931104    5C                   ENDDO
373900931102     C*
374000931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA SOMMARE PER "G"
374100931102     C*                                 O DA TASSARE PER "T"
374200931102     C                   EXSR      CTRSTP
374300931103    4C                   ENDIF
374400931022     C*
374500950116     C* BASE  GIA' VALORIZZATA
374600931103    4C     *IN15         IFEQ      *ON
374700931105     C                   MOVEL     'M'           W01FIT
374800931105     C                   Z-ADD     VI4ITT        W01ITT
374900931103    4C                   ENDIF
375000950116     C* ACCESSORI  GIA' VALORIZZATI
375100931103    4C     *IN16         IFEQ      *ON
375200931105     C                   MOVEL     'M'           W01FIA
375300931105     C                   Z-ADD     VI4ITA        W01ITA
375400931103    4C                   ENDIF
375500931021     C*
375600931103   X3C                   ELSE
375700931022     C*
375800931022     C* BASE E ACCESSORI GIA' VALORIZZATI
375900931022     C                   Z-ADD     VI4ITT        W01ITT
376000931022     C                   Z-ADD     VI4ITA        W01ITA
376100931021     C                   MOVEL     'M'           W01FIT
376200931021     C                   MOVEL     'M'           W01FIA
376300931022     C*
376400931022     C* ANNULLO TUTTE LE EVENTUALI MEMORIZZAZIONI SULLE BOLLE
376500931022     C                   SETON                                        14
376600931022     C                   EXSR      ANNSPE
376700931022     C                   SETOFF                                       14
376800931022     C*
376900931021    3C                   ENDIF
377000931021     C* AGGIORNO TESTATA
377100011105     C     KFTD          CHAIN     FIFTT000                           30
377200020212    3C**** sempre imposta i campi per calcolare a giornata nella Conferma
377300020212    3C**** VH4TSM        IFEQ      'G'
377400020212     C***                Z-ADD     W01CLA        FTTCLA
377500020212     C***                Z-ADD     W01CPE        FTTCPE
377600020212     C***                Z-ADD     W01KFA        FTTKFA
377700020212     C***                Z-ADD     W01TVL        FTTTVL
377800020212     C***                Z-ADD     W01SNA        FTTSNA
377900020212     C                   z-add     g01CLA        FTTCLA
378000020212     C                   z-add     g01CPE        FTTCPE
378100020212     C                   z-add     g01KFA        FTTKFA
378200020212     C                   z-add     g01TVL        FTTTVL
378300020212     C                   z-add     g01SNA        FTTSNA
378400020212    3C********           ENDIF
378500931022     C*
378600931022     C* SE TROVATI ERRORI IMPOSTO IL FLAG DI PRESENZA MANCA TARIFFA
378700931103    3C     VI4SCE        IFEQ      'E'
378800931022     C                   MOVEL     'S'           FTTFLA
378900931103   X3C                   ELSE
379000931022     C                   MOVEL     ' '           FTTFLA
379100931103    3C                   ENDIF
379200931022     C                   MOVEL     W01ITT        FTTITT
379300931022     C                   MOVEL     W01ITA        FTTITA
379400931021     C                   MOVEL     W01FIT        FTTFIT
379500931021     C                   MOVEL     W01FIA        FTTFIA
379600931202     C                   MOVEL     W01SNP        FTTSNP
379700950119     C                   MOVE      W01SNE        FTTSNE
379800950127     C                   MOVEL     VI4PEP        FTTPEP
379900021211     C                   movel     vi4FCS        fttLVL
380000931021     C                   MOVEL     'V'           FTTFVL
380100020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
380200020326     C* i primi tre caratteri
380300020326     c                   if        fttfgs = 0
380400020326     c                   movel     fttpdr        fttfgs
380500020326     c                   end
380600020326     C*
380700011105     C  N30              UPDATE    FIFTT000
380800020423     c*
380900020423     c* pulisce eventuali records del file di work rimasti in sospeso
381000020423     c*               *-----------------------*
381100020423     c  N30              exsr      del_FTDW0
381200020423     c*               *-----------------------*
381300020423     c*
381400931021    2C                   ENDIF
381500931021    1C                   ENDIF
381600931021     C*
381700931021     C                   ADD       1             NRR4
381800931103    0C                   ENDDO
381900931110     C* IMPOSTO IL FLAG DI VALORIZZATE
382000931110     C     *IN90         IFEQ      *ON
382100931110     C                   MOVEL     'V'           VI1FVL
382200931110     C                   MOVEL     CVALOR        VI1VAL
382300931110     C                   ENDIF
382400931021     C                   ENDSR
382500030821     C*---------------------------------------------------------------*
382600041124     C*  Ciclo preventivo di annullamento FIFCE se per problemi era
382700041124     C*  rimasto sporco. --> Non deve esserci nulla prima della Valorizzazione
382800030821     C*---------------------------------------------------------------*
382900041124     C     Annulla_FCE   BEGSR
383000041124      *
383100041124     c                   movel     vi4pdr        FGS_vi4pdr        3 0
383200041124      *          *-------------------------------------*
383300041124     c     parsml        ifeq      ' '
383400041124     c     Del_FCE       setll     fifce01l
383500041124     c     Del_FCE       reade     fifce01l
383600041124     c                   dow       not %Eof(fifce01l)
383700041124     c                   delete    fifce000
383800041124     c     Del_FCE       reade     fifce01l
383900041124     c                   enddo
384000041124     c                   end
384100041124      *          *-------------------------------------*
384200041124      *
384300041124     C                   ENDSR
384400041124     C*---------------------------------------------------------------*
384500041124     C* Conta quante Spedizioni ci sono all'interno dello Stesso Stop
384600041124     C*---------------------------------------------------------------*
384700041124     C     Sped_x_Stop   BEGSR
384800030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
384900030821     C*  lo stop.
385000030821     c                   z-add     0             W01nss
385100030821     c                   z-add     0             W01fw
385200030821     c                   z-add     0             W01rcc
385300090911     c                   clear                   W01_TarPartCLI    1
385400090911     c                   clear                   W01_UnoOpiuCLI    7 0
385500030821      *
385600030821     C*  pgm x contare le spedizioni in STOP
385700030821     c                   clear                   FICNB1
385800030821     C                   eval      §t1PDR  =  ftdPDR
385900030821     C                   eval      §t1TSR  =  ftdTSR
386000030821     C                   eval      §t1NDC  =  ftdNDC
386100030821     C                   eval      §t1DDC  =  ftdDDC
386200030821     C                   eval      §t1STP  =  ftdSTP
386300030821     C*
386400030821     C                   CALL      'FICNB1R1'
386500030821     C                   PARM                    ficnb1
386600030821     C* Nr.Sped.totali in STOP
386700030821     c                   z-add     §T1nss        W01nss
386800030821     c                   z-add     §T1fw         W01fw
386900030821     c                   z-add     §T1rcc        W01rcc
387000090911     c                   eval       W01_TarPartCLI = §T1PARTIC
387100090911     c                   eval       W01_UnoOpiuCLI = §T1CLIPAR
387200030821      *
387300030821     C                   ENDSR
387400020212     C*---------------------------------------------------------------*
387500020328     C* Scrittura del file di work record x record
387600020212     C*---------------------------------------------------------------*
387700020328     C     wrt_FTDW      BEGSR
387800020328      *
387900020328     C                   z-add     ftdfgs        ftwfgs
388000020328     C                   z-add     ftdpdr        ftwpdr
388100020328     C                   move      ftdtsr        ftwtsr
388200020328     C                   z-add     ftdndc        ftwndc
388300020328     C                   z-add     ftdddc        ftwddc
388400020328     C                   z-add     ftdstp        ftwstp
388500020328     C                   z-add     ftdaas        ftwaas
388600020328     C                   z-add     ftdlnp        ftwlnp
388700020328     C                   z-add     ftdnrs        ftwnrs
388800020328     C                   z-add     ftdnsp        ftwnsp
388900020328     C                   z-add     ftdpkl        ftwpkl
389000020328     C                   z-add     ftdvlu        ftwvlu
389100020328      *
389200020328     c                   write     fiftdw00
389300020328      *
389400020328     C                   ENDSR
389500020328     C*---------------------------------------------------------------*
389600020328     C* Cancella i record del work dopo aver eseguito lo stop
389700020328     C*---------------------------------------------------------------*
389800020328     C     del_FTDW      BEGSR
389900020328      *
390000020328     C                   z-add     ftdfgs        kftwfgs
390100020328     C                   z-add     ftdpdr        kftwpdr
390200020328     C                   move      ftdtsr        kftwtsr
390300020328     C                   z-add     ftdndc        kftwndc
390400020328     C                   z-add     ftdddc        kftwddc
390500020328     C                   z-add     ftdstp        kftwstp
390600020328      *
390700020328     c     kftdw         setll     fiftdw1l
390800020328     c     kftdw         reade     fiftdw1l
390900020328     c                   dow       not %eof(fiftdw1l)
391000020328     c                   delete    fiftdw00
391100020328     c     kftdw         reade     fiftdw1l
391200020328     C                   ENDdo
391300020328      *
391400020328     C                   ENDSR
391500020422     C*---------------------------------------------------------------*
391600020422     C* Cancella i record del work dopo aver eseguito lo stop
391700020422     C*---------------------------------------------------------------*
391800020422     C     del_FTDW0     BEGSR
391900020422      *
392000020423     C                   z-add     fttfgs        kftwfgs
392100020423     C                   z-add     fttpdr        kftwpdr
392200020423     C                   move      ftttsr        kftwtsr
392300020423     C                   z-add     fttndc        kftwndc
392400020423     C                   z-add     fttddc        kftwddc
392500020423      *
392600020422     c     kftdw0        setll     fiftdw1l
392700020422     c     kftdw0        reade     fiftdw1l
392800020422     c                   dow       not %eof(fiftdw1l)
392900020422     c                   delete    fiftdw00
393000020422     c     kftdw0        reade     fiftdw1l
393100020422     C                   ENDdo
393200020422      *
393300020422     C                   ENDSR
393400020328     C*---------------------------------------------------------------*
393500020328     C* CONTROLLO LA SOMMA PESI -------------------------------------*
393600020328     C*---------------------------------------------------------------*
393700020328     C     CTRTSM        BEGSR
393800931029     C                   SETOFF                                       17
393900020122     C                   CLEAR                   FICNB2
394000000626     C* VERIFICO SE BOLLA POSTA OPPURE NO
394100000626     C* CONTROLLO IN AZORG SE PO POSTE
394200000626     C                   EXSR      CTRORG
394300931123     C* MEMORIZZO CODICE TARIFFA
394400931123    1C     FTDCTP        IFEQ      *BLANKS
394500000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
394600000623    2C     WCTP          IFEQ      *BLANKS
394700000623     C     WCTB          ANDEQ     *BLANKS
394800931123     C                   MOVEL     WALF3         FTDCTP
394900000623     C                   ELSE
395000000623     C*  P.O. POSTE
395100020729     C     §OgNTW        IFEQ      'PPT'
395200000623     C                   MOVEL     WCTP          FTDCTP
395300000623     C                   ELSE
395400000623     C                   MOVEL     WCTB          FTDCTP
395500000623    3C                   ENDIF
395600000623     C*
395700000623    2C                   ENDIF
395800000623     C*
395900931123    1C                   ENDIF
396000931029     C*
396100950118     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
396200950118     C                   SETON                                        30
396300950118    6C     *IN19         IFEQ      *OFF
396400950118     C* T I P O   B O L L A   D A   T A S S A R E
396500950224     C                   CLEAR                   ARBCBO
396600091012     C     Bolla_FTD     CHAIN     FNARB01L                           32
396700950118     C                   EXSR      CTRCBL
396800030109      *
396900950118     C   31              SETOFF                                       30
397000950118     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
397100950118     C*  (SE NON CONSEGNATA)
397200950118    7C   30FTDDCM        IFEQ      0
397300950118     C*
397400950118    8C     FTDCMC        IFNE      *BLANKS
397500950118     C                   MOVEL     FTDCMC        WALF6
397600950118   X8C                   ELSE
397700950118     C                   MOVEL     FTDTMC        WALF6
397800950118    8C                   ENDIF
397900950118     C                   EXSR      CTRCMC
398000950118    7C                   ENDIF
398100950118    6C                   ENDIF
398200000626     C*  P.O. POSTE
398300050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
398400020729     C     §OgNTW        IFEQ      'PPT'
398500050426     C     vh4tpo        andeq     'S'
398600000626     C                   SETOFF                                       30
398700000626     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE SIGNIFICA CHE LA BOLLA
398800000626     C* SI DEVE PAGARE COME CONSEGNATA
398900000626     C*
399000000626     C                   MOVEL     '2Z'          COD
399100000626     C                   MOVEL     *BLANKS       KEY
399200000626     C                   MOVEL     FTDCMC        KEY
399300000626     C     KTAB          CHAIN     TABEL                              30
399400000626     C  N30              MOVEL     TBLUNI        DS2Z
399500000626     C   30              MOVEL     *BLANKS       DS2Z
399600000626     C     §2ZTPP        IFNE      *BLANKS
399700000626     C* LA BOLLA VIENE PAGATA
399800000626     C                   SETON                                        30
399900000626     C                   ENDIF
400000000626     C                   MOVEL     §2ZTPP        FTDFGC
400100000626    3C                   ENDIF
400200950118     C*
400300950118    6C     *IN30         IFEQ      *ON
400400950119     C                   MOVEL     'S'           W01PAG            1
400500950118     C                   ELSE
400600950119     C                   ADD       1             W01SNP                          no pagare
400700950118     C                   MOVEL     ' '           W01PAG
400800950118    6C                   ENDIF
400900950119     C*
401000950119     C     *IN19         IFEQ      *OFF
401100950119     C     W01PAG        ANDEQ     *BLANKS
401200950119     C     FTDDCM        ANDEQ     *ZEROS
401300950119     C                   ADD       1             W01SNE                          no cons.no pag
401400950119     C                   END
401500950118     C*
401600950118     C     W01PAG        IFEQ      'S'
401700950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
401800950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
401900931029     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
402000950123     C                   MOVE      'S'           §TAPAG
402100950118     C                   ELSE
402200950118     C                   MOVE      *ZEROS        §TAKFA                          PESO
402300950118     C                   MOVE      *ZEROS        §TATVL                          VOLUME
402400950118     C                   MOVE      *ZEROS        §TANCL                          COLLI
402500950123     C                   MOVE      ' '           §TAPAG
402600950118     C                   END
402700950118     C*
402800950118     C     *IN19         IFEQ      *ON
402900950118     C                   Z-ADD     FTDNCE        §TACPE                          COLLI ETICH
403000950118     C                   ELSE
403100950118     C                   Z-ADD     FTDNCL        §TACPE                          COLLI PIK.
403200950118     C                   END
403300950118     C*
403400950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
403500021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
403600950118     C*
403700950130     C                   Z-ADD     FTDSET        §TASET
403800931029     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
403900931029     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
404000020325     C*
404100020325     C* memorizza se c'è anteporto per Isola
404200020325     C*  ma attenzione,se almeno uno all'interno dello stesso STOP
404300020325     C* è diverso da isola, resetta il flag da passare al pgm di calcolo x stop
404400020325     C*
404500020325     C* Se almeno una delle spedizioni all'interno dello stop non è
404600020325     C*  un'isola ...lo memorizza.
404700020325     c                   if        almuno_iso = 1 and ftdFIN <> 'I'             Anteporto
404800020325     C                   move      *blank        w01isola                       se Isola
404900020325     c                   end
405000020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
405100020325     C*   all'interno dello stesso stop non sia gestita ad Isola che deve
405200020325     C*    segnalarlo.
405300020325     c                   if        almuno_iso = 0
405400020325     c                   z-add     1             almuno_iso        1 0
405500020325     C                   move      *blank        w01isola                       Anteporto
405600020325     c                   if        ftdFIN = 'I'                                 per indicare
405700020325     C                   move      'S'           w01isola                       se Isola
405800020325     c                   end
405900020325     c                   end
406000030109     C*
406100931111     C* SOMMA PESI A GIORNATA O A STOP:
406200931111     C* ELABORO COMUNQUE GLI ACCESSORI BOLLA PER BOLLA
406300931111     C*
406400020325      *  *-----------------------*
406500931111     C***   S T O P
406600020325      *  *-----------------------*
406700020301    1C*****VH4TSM        IFEQ      'T'
406800931111     C* SE NON C'E' IMPORTO MANUALE E NON E' VALORIZZATA BASE DISTINTA
406900931116     C* E NON E' SOLO UN RITORNO ALL'INCASSO
407000950126     C* E BASE SPEDIZIONE NON VALORIZZATA
407100931111     C*  ENTRO NEL CALCOLO DELLO "STOP"
407200931111    2C     FTDFIT        IFNE      'M'
407300931111     C     *IN15         ANDEQ     *OFF
407400931116     C     FTDSIC        ANDNE     'S'
407500960905    3C     FTDITT        IFEQ      *ZEROS
407600931111     C                   EXSR      CALSTP
407700960905   X3C                   ELSE
407800960906     C     FTDSTP        IFGT      *ZEROS
407900960906    4C     FTDSTP        ANDEQ     W01STP
408000960906     C                   CLEAR                   W01STP
408100960905     C                   CLEAR                   W01KFA
408200960905     C                   CLEAR                   W01NCL
408300960905     C                   CLEAR                   W01TVL
408400960905     C                   CLEAR                   W01CPE
408500020301     C******             CLEAR                   g01cpe
408600020301     C******             CLEAR                   g01tvl
408700020301     C******             CLEAR                   g01kfa
408800960905    4C                   END
408900960905     C* SE ANCHE ACCESSORI PIENI NON TASSO
409000960905    4C     FTDITA        IFGT      0
409100960905     C     *IN16         OREQ      *ON
409200960905     C                   SETON                                        17
409300960905    4C                   ENDIF
409400960905    3C                   END
409500931111   X2C                   ELSE
409600931111     C* SE ANCHE ACCESSORI PIENI NON TASSO
409700931111    3C     FTDITA        IFGT      0
409800931111     C     *IN16         OREQ      *ON
409900931111     C                   SETON                                        17
410000931111    3C                   ENDIF
410100931111    2C                   ENDIF
410200931111     C*
410300020214   X1C*************      ELSE
410400931029     C*
410500931029     C* SOLO PER TARIFFA A KILO O QUINTALE
410600931123     C                   MOVEL     FTDCTP        WALF1
410700931111    2C     WALF1         IFEQ      '0'
410800931029     C     WALF1         OREQ      '3'
410900931029     C*
411000020325      *  *-----------------------*
411100931111     C***   G I O R N A T A
411200020325      *  *-----------------------*
411300020304    3C*****VH4TSM        IFEQ      'G'********************************
411400931029     C* SOLO SE NON HO GIA' VALORIZZATO LA BASE
411500931111    4C     *IN15         IFEQ      *OFF
411600931029     C     FTDITT        ANDEQ     0
411700950118     C     W01PAG        IFEQ      'S'
411800931125     C                   ADD       FTDNCL        W01CLA                          COLL GIORN
411900020304     C                   ADD       FTDPKL        W01KFA                          PESO GIORN
412000020325     C                   ADD       FTDVLU        W01TVL                          VOL  GIORN
412100020325      *
412200020325      * non deve essere l'anteporto per Isole
412300020325      *  in quanto per le spedizioni Isole non si deve calcolare nè la
412400020325      *   base nè lo stop. Quindi escludo dalla somma della giornata i valori
412500020325      * per il calcolo .
412600020325     c     ftdfin        ifne      'I'
412700020325     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
412800020304     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
412900020304     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
413000020325     c                   end
413100020325      *
413200950118     C                   END
413300020320     C**** *IN19         IFEQ      *OFF
413400020320     C     *IN19         IFEQ      *on
413500020304     C                   ADD       FTDNCE        W01CPE                          COLLI ETICH
413600020304     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
413700950118     C                   ELSE
413800020304     C                   ADD       FTDNCL        W01CPE                          COLLI PIK.
413900020304     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
414000950118     C                   END
414100931029     C                   MOVEL     *HIVAL        §TAITT
414200931029     C*
414300950214     C     W01PAG        IFEQ      'S'
414400931111    5C     W02STP        IFNE      FTDSTP                                        STOP GIORN
414500931111    6C     W02STP        IFGT      0
414600931029     C                   ADD       1             W01SNA
414700020212     C                   ADD       1             g01SNA
414800931111    6C                   ENDIF
414900931102     C                   MOVEL     FTDSTP        W02STP
415000931111    5C                   ENDIF
415100931029     C*
415200931111    5C     W02STP        IFEQ      FTDSTP
415300931102     C     W02STP        ANDEQ     0
415400931029     C                   ADD       1             W01SNA
415500020212     C                   ADD       1             g01SNA
415600931111    5C                   ENDIF
415700931111    4C                   ENDIF
415800950214     C                   ENDIF
415900931029     C*
416000931111    4C     FTDITA        IFGT      0
416100931029     C     *IN16         OREQ      *ON
416200931029     C                   SETON                                        17
416300931111    4C                   ENDIF
416400020304    3C****************** ENDIF*****************************************
416500931029     C*
416600020304    2C                   ENDIF
416700020214    1C***********        ENDIF
416800931111     C*
416900931111     C* T A S S A Z I O N E   B O L L A
417000931111     C*
417100950118     C* SE CI SONO ENTRAMBI GLI IMPORTI NON TASSO
417200931112    1C     FTDITT        IFGT      0
417300931112     C     *IN15         OREQ      *ON
417400950118     C     FTDSIC        OREQ      'S'                                          RITORNO INCASSO
417500931112     C     FTDITA        IFGT      0
417600931112     C     *IN16         OREQ      *ON
417700950118     C     W01PAG        OREQ      *BLANKS                                      NON PAGARE
417800931112     C                   SETON                                        17
417900931111    1C                   ENDIF
418000931112    1C                   ENDIF
418100931112     C*
418200020325     C                   movel     *blanks       §taISO
418300020213     C                   MOVEL     'S'           §TAtsm
418400931112     C                   EXSR      TASSAZ
418500931102     C*
418600020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
418700020326     C* i primi tre caratteri
418800020326     c                   if        ftdfgs = 0
418900020326     c                   movel     ftdpdr        ftdfgs
419000020326     c                   end
419100020326     C*
419200011105     C                   UPDATE    fiftd001
419300931029     C                   ENDSR
419400020212     C*---------------------------------------------------------------*
419500931022     C* CALCOLO LE SOMME PESI A STOP ---------------------------------*
419600020212     C*---------------------------------------------------------------*
419700931022     C     CALSTP        BEGSR
419800931102     C*
419900931102     C* SE TROVATO STOP ED E' CAMBIATO --> TASSO LO STOP
420000931102    1C     W01STP        IFNE      0
420100931102     C     FTDSTP        ANDNE     W01STP
420200931102     C     W01STP        ORNE      0
420300931123     C     FTDCTP        ANDNE     W01CTP
420400020212     C*
420500000626     C* SOLO PER DEBUG
420600000626     C     FTDSTP        IFEQ      3
420700000626     C     W01STP        ANDEQ     2
420800000626     C                   MOVE      2             W01STP
420900000626     C                   ENDIF
421000931102     C*
421100931102     C* SALVO LA CHIAVE DELLA SPEDIZIONE
421200020313     C**N19********      MOVEL     FTDSPC        W01SPC
421300020313     C***19********      MOVEL     FTDSPR        W01SPR
421400020313      *  nuovo modo di salvare la chiave della spedizione in quanto i ritiri
421500020313      *   nella nuova forma hanno la riga con la spedizione.
421600020315     c                   clear                   w01spc
421700020315     c                   clear                   w01spr
421800020315     c**********         if        vh6nsp>0
421900020315     c                   IF        ftdnsp > 0
422000020313     C                   MOVEL     FTDSPC        W01SPC
422100020313     c                   else
422200020313     C                   MOVEL     FTDSPR        W01SPR
422300020313     c                   end
422400020325     C*
422500931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
422600011105     C     W05NRR        CHAIN     fiftd000                           32
422700931102     C*
422800931102     C                   Z-ADD     W01KFA        §TAKFA
422900931102     C                   Z-ADD     W01TVL        §TATVL
423000931102     C                   Z-ADD     W01NCL        §TANCL
423100950118     C                   Z-ADD     W01CPE        §TACPE
423200950123     C                   MOVEL     'S'           §TAPAG
423300950118     C     §TAKFA        IFEQ      0
423400950118     C                   MOVE      *BLANKS       §TAPAG
423500950118     C                   END
423600950118     C*
423700931102     C                   MOVEL     *HIVAL        §TAITA
423800020325     C                   movel     w01isola      §taISO
423900931102     C*
424000020213     C                   MOVEL     'T'           §TAtsm
424100931102     C                   EXSR      TASSAZ
424200020328      *
424300020325     C*  resetta i contatori per le isole e per l'espresso.
424400020325     c                   z-add     0             almuno_iso        1 0
424500020325     c                   z-add     0             almuno_xpr        1 0
424600030109     C*  resetta a livello di stop il destinatario disagiato
424700030109     c                   z-add     0             almuno_disag      1 0
424800931102     C*
424900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
425000020326     C* i primi tre caratteri
425100020326     c  n32              if        ftdfgs = 0
425200020326     c                   movel     ftdpdr        ftdfgs
425300020326     c                   end
425400020326     C*
425500011105     C  N32              UPDATE    fiftd000
425600020325     C*
425700931102     C* RILEGGO IL RECORD
425800020313    2C**** *IN19 *****   IFEQ      *OFF
425900020313      * prima testava l'indicatore adesso non è più possibile distinguere i ritiri
426000020313      * allo stesso modo
426100020315     c***********        IF        vh6nsp>0
426200020315     c                   IF        w01spc <> *blank
426300931202     C                   MOVEL     W01SPC        FTDSPC
426400931202     C                   MOVEL     FTDAAS        VH6AAS
426500931202     C                   MOVEL     FTDNSP        VI6NSP
426600020315     c                   if        vh4tsr='R'
426700020315      * vista per i nuovi ritiri x spedizione
426800020315     C     KFTD5         CHAIN(N)  fiftd005                           32
426900020315     c                   else
427000020315      * seleziona solo i consegnati
427100011105     C     KFTD2         CHAIN(N)  fiftd002                           32
427200020315     c                   end
427300940415   X2C                   ELSE
427400931202     C                   MOVEL     W01SPR        FTDSPR
427500931202     C                   MOVEL     FTDKSC        VI6NSP
427600011105     C     KFTD4         CHAIN(N)  fiftd004                           32
427700940415    2C                   ENDIF
427800931102     C*
427900020122     C                   CLEAR                   FICNB2
428000931102     C                   CLEAR                   W01KFA
428100931102     C                   CLEAR                   W01NCL
428200931102     C                   CLEAR                   W01TVL
428300950118     C                   CLEAR                   W01CPE
428400020301     C******             CLEAR                   g01KFA
428500020301     C******             CLEAR                   g01TVL
428600020301     C******             CLEAR                   g01CPE
428700931102     C* REIMPOSTO I CAMPI
428800950118     C     W01PAG        IFEQ      'S'
428900950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
429000950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
429100931102     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
429200950125     C                   MOVEL     'S'           §TAPAG
429300950118     C                   ELSE
429400950118     C                   MOVE      *ZEROS        §TAKFA
429500950118     C                   MOVE      *ZEROS        §TATVL
429600950118     C                   MOVE      *ZEROS        §TANCL
429700950125     C                   MOVEL     ' '           §TAPAG
429800950118     C                   END
429900020313      *
430000020320     C**** *IN19         IFEQ      *OFF
430100020320     C     *in19         ifeq      *on
430200950118     C                   Z-ADD     FTDNCE        §TACPE
430300950118     C                   ELSE
430400950118     C                   Z-ADD     FTDNCL        §TACPE
430500950118     C                   END
430600020313      *
430700950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
430800021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
430900931102     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
431000931102     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
431100931110     C                   MOVEL     FTDSTP        W01STP
431200940415     C* MEMORIZZO CODICE TARIFFA
431300940415    2C     FTDCTP        IFEQ      *BLANKS
431400940415     C                   MOVEL     WALF3         FTDCTP
431500000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
431600000623    3C     WCTP          IFEQ      *BLANKS
431700000623     C     WCTB          ANDEQ     *BLANKS
431800000623     C                   MOVEL     WALF3         FTDCTP
431900000623     C                   ELSE
432000000623     C* VERIFICO SE BOLLA POSTA OPPURE NO
432100000623     C* CONTROLLO IN AZORG SE PO POSTE
432200000623     C                   EXSR      CTRORG
432300000623     C*  P.O. POSTE
432400020729     C     §OgNTW        IFEQ      'PPT'
432500000623     C                   MOVEL     WCTP          FTDCTP
432600000623     C                   ELSE
432700000623     C                   MOVEL     WCTB          FTDCTP
432800000623    4C                   ENDIF
432900000623     C*
433000000623    3C                   ENDIF
433100000623     C*
433200940415    2C                   ENDIF
433300931102    1C                   ENDIF
433400931102     C*
433500931022     C* C'E' LO STOP E LA BASE NON E' TASSATA(NE' DI BOLLA NE' DI DIST)
433600931102    1C     FTDSTP        IFGT      0
433700931022     C                   MOVEL     FTDSTP        W01STP                          SALVO STOP
433800931123     C                   MOVEL     FTDCTP        W01CTP                          SALVO CTR
433900931102     C                   Z-ADD     FTDNRR        W05NRR            9 0
434000950118     C     W01PAG        IFEQ      'S'
434100020304     C***********        ADD       FTDPKL        W01KFA
434200020304     C***********        ADD       FTDPKL        g01KFA
434300020304     C***********        ADD       FTDVLU        W01TVL
434400020304     C***********        ADD       FTDVLU        g01TVL
434500931022     C                   ADD       FTDNCL        W01NCL
434600950119     C                   END
434700950119     C*
434800950119     C     *IN19         IFEQ      *OFF
434900020304     C**********         ADD       FTDNCL        W01CPE                         PIK.
435000020304     C**********         ADD       FTDNCL        g01CPE                         PIK.
435100950118     C                   ELSE
435200020304     C**********         ADD       FTDNCE        W01CPE                         ETICH.
435300020304     C**********         ADD       FTDNCE        g01CPE                         ETICH.
435400950118     C                   END
435500950119     C*
435600931022     C                   MOVEL     *HIVAL        §TAITT
435700931102    2C     FTDITA        IFGT      0
435800931022     C     *IN16         OREQ      *ON
435900931022     C                   SETON                                        17
436000931102    2C                   ENDIF
436100931102    1C                   ENDIF
436200931022     C*
436300931022     C                   ENDSR
436400020212     C*---------------------------------------------------------------*
436500931102     C* TASSAZIONE BOLLA ---------------------------------------------*
436600020212     C*---------------------------------------------------------------*
436700931102     C     TASSAZ        BEGSR
436800091012      *
436900931111     C* 17 OFF - D A   T A S S A R E
437000931111    0C     *IN17         IFEQ      *OFF
437100091012      *
437200931123     C                   MOVEL     FTDCAP        §TACAP                          CAP MIT/DES
437300020326     C                   MOVEL     ftdfgs        §TAfgs
437400091012      *
437500020326     c                   if        §tafgs=0
437600020326     C                   MOVEL     FTDPDR        §TAfgs                          P.O. di gestione
437700020326     c                   end
437800091012      *
437900931102     C                   MOVEL     FTDPDR        §TAPDR                          PADRONCINO
438000931102     C                   MOVEL     PARSML        §TASML                          SIMULAZ
438100020326     C                   MOVEL     vh4DDC        §TADDC                          DATA DISTIN
438200020326     C                   z-add     vi4ndc        §tandc
438300020129     C                   z-add     ftdaas        §taaas
438400020129     C                   z-add     ftdlnp        §talnp
438500020129     C                   z-add     ftdnrs        §tanrs
438600020129     C                   z-add     ftdnsp        §tansp
438700020328     C                   z-add     ftdstp        §tastp
438800091012      *
438900020225     c                   if        §tansp = 0 and ftdtsr='R'
439000020225     C                   z-add     ftdksc        §tansp
439100020326     C                   z-add     ftdksc        §taksc
439200020225     c                   end
439300050909      *
439400050909     C* Comunque aggancia sempre la bolla in partenza x vedere
439500050909      *  il codice trattamento merce
439600091012     c                   clear                   trovato_arb       1
439700091012     c                   clear                   trovato_blp       1
439800050909     C                   clear                   DS1B
439900091012     c     bolla_ftd     chain     fnblp01l
440000050909     c                   if        %found(fnblp01l)
440100091012      * ------------
440200091012     c                   move      'S'           trovato_blp       1
440300091012     C                   MOVE      blpCbo        §tacbo
440400091012     C                   z-add     blpKSC        CodCliente        7 0
440500091012      *
440600091012     C                   MOVEL     '3A'          COD
440700091012     C                   MOVEL     *BLANKS       KEY
440800091012     C                   MOVEL     blpCBO        KEY
440900091012     C     KTAB          CHAIN     TABEL                              30
441000091012     C  N30              MOVEL     TBLUNI        DS3A
441100091012     c                   If        %subst(§3ATB1:1:1) ='A'
441200091012     C     Bolla_FTD     CHAIN     FiAR601L
441300091012     c                   if        %Found(Fiar601L)
441400091012     C                   Z-ADD     ar6KSC        CodCliente                      Cliente
441500091012     c                   end
441600091012      **
441700091012      **   SOLO X RITIRI
441800091012     c                   iF        ftdtsr = 'R'
441900091012     C                   MOVEL     '1B'          COD
442000091012     C                   MOVEL     *BLANKS       KEY
442100091012     C                   MOVEL     blpCTM        KEY
442200091012     C     KTAB          CHAIN     TABEL                              30
442300091012     c                   if        %found(TABEL00F)
442400091012     C                   MOVEL     TBLUNI        DS1B
442500091012      * deve controllare se abilitata a tariffa Disk C
442600091012      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
442700091012      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
442800091012      *   la tariffa di Etichettatura in generale.
442900091012      *
443000091012      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
443100091012      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
443200091012     c*******            iF        ftdtsr = 'R'
443300091012     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
443400091012     c                   move      'N'           §taPPE
443500091012     c                   end
443600091012      * Se è un DISKC Bartolini la regola dice che deve essere comunque
443700091012      * pagata la spedizione al Padroncino
443800091012     c                   iF        §1Bdkc ='B' and ftdnrs = 0
443900091012     c                   eval      §taPPE = 'S'
444000091012     c                   eval      §tadkc = 'S'
444100091012     c                   end
444200091012     c                   end
444300091012      **
444400091012     c                   endiF
444500091012      *
444600091012     c                   endiF
444700091012      * ------------
444800091012     c                   end
444900091012      * Se non ha trovato il BLP
445000091012      *  oppure
445100091012      * Comunque se si tratta di consegne
445200091012     c                   if        NOT %found(fnblp01l) or FTDTSR = 'C'
445300091012     c     bolla_ftd     chain     fnarb01l
445400091012     c                   if        %found(fnarb01l)
445500091012     c                   move      'S'           trovato_arb       1
445600091012     C                   MOVE      arbCbo        §tacbo
445700091012     C                   z-add     arbKSC        CodCliente        7 0
445800091012     c                   end
445900091012     c                   end
446000091012      *
446100091012      *  Carica i campi di passaggio
446200931202     C                   Z-ADD     1             §TASPP                          NUM STOP
446300931123     C                   MOVEL     FTDTSR        §TATSR                          TIPO PREST
446400931108     C                   MOVEL     FTDFIN        §TAFIN                          INOLTRO/ANT
446500931124     C                   MOVEL     FTDCTP        §TACTR                          COD TARIFFA
446600931102     C*
446700931102     C* SE GIA' VALORIZZATA IMPOSTO MAX VALORE PER NON CALCOLARLA
446800931102     C   15              MOVEL     *HIVAL        §TAITT                          BASE
446900931102     C* IDEM
447000931102     C   16              MOVEL     *HIVAL        §TAITA                          ACCESSORI
447100950118     C*
447200950118     C* IMPORTO  FATTURA ASSEGNATO INCASSATO
447300950118     C                   Z-ADD     FTDIFP        §TAIFP
447400011109      *  Divisa
447500011109     c                   if        vh4tpr = 2
447600011109     c                   movel     vi4ctd        §tadiv
447700011109     c                   end
447800011109      *
447900950118     C* FLAG SOLO INCASSO
448000950118     C                   MOVEL     FTDSIC        §TASIC
448100931124     C*
448200931124     C* C O N S E G N E
448300091012    1C**** FTDTSR        IFEQ      'C'
448400091012     C**** Bolla_FTD     CHAIN     FNARB01L                           30
448500091012     C****               z-add     arbKSC        CodCliente        7 0
448600091012     c*******            end
448700090918      *
448800950116     C                   MOVEL     ARBTSP        §TATSP                          TIPO SPEDIZ
448900020325     C                   MOVEL     *blank        §taxpr
449000030320     C                   MOVEL     ARBcbo        §TAcbo                          TIPO SPEDIZ
449100020325     C*
449200020325     C* Se a spedizione
449300020325     c     §tatsm        ifeq      'S'
449400030109     C*----------
449500020325     C* memorizza se all'interno dello stop sono tutte spedizioni espresso
449600020325     C* Se almeno una delle spedizioni all'interno dello stop non è
449700020325     C*  espresso ...lo segnala.
449800020325     c                   if        almuno_xpr = 1 and arbtsp <> 'E'
449900091008     c                             and arbtsp <> 'H'
450000020325     C                   move      *blank        w01xpres
450100020325     c                   end
450200020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
450300020325     C*   all'interno dello stesso stop non sia un espresso che deve segnalarlo
450400020325     c                   if        almuno_xpr = 0
450500020325     c                   z-add     1             almuno_xpr        1 0
450600020325     C                   move      *blank        w01xpres                       Espresso
450700091008     c                   if        arbtsp = 'E' or arbtsp = 'H'
450800020325     C                   move      'S'           w01xpres
450900020325     c                   end
451000020325     c                   end
451100030109     C*----------
451200030109     C* Se almeno una delle spedizioni all'interno dello stop non ha
451300030109     C*  il destinatario disagiato lo memorizza per non pagare il disagio.
451400030109     c                   if        almuno_disag = 1
451500030109     c                   if        arbtc1 = ' ' and arbtc2 = ' ' or
451600030109     c                             arbdcm = 0
451700030109     C                   move      *blank        w01disagiato                   non disagiato
451800030109     c                   end
451900030109     c                   end
452000030109     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
452100030109     C*   all'interno dello stesso stop non sia segnalata come destinatario
452200030109     C*    disagiato che deve tenerlo presente per tutte
452300030109     c                   if        almuno_disag = 0
452400030109     c                   z-add     1             almuno_disag      1 0
452500030109     C                   move      *blank        w01disagiato
452600030109     c                   if        arbtc1 = 'X' or arbtc2 = 'X'                 per indicare
452700030109     c                   if        arbdcm > 0                                   se nella spediz.
452800030109     C                   move      'S'           w01disagiato                   il destinatario
452900030109     c                   end                                                    è disagiato
453000030109     c                   end                                                    è disagiato
453100030109     c                   end
453200020325     C*
453300020325     c                   ElsE
453400020325     C* solo a STOP passa il flag valorizzato per il calcolo
453500020325     C                   move      w01xpres      §taxpr
453600030109     C                   move      w01disagiato  §taDED
453700020325     c                   endIF
453800931124     C*
453900931124    2C     §TAITA        IFEQ      0
454000950131     C* CONTRASSEGNO
454100950131     C                   Z-ADD     FTDCAS        §TACAS
454200950131     C                   MOVEL     FTDVCA        §TAVCA
454300011106      *   con l'EURO non è più vero che la divisa lira=*blank
454400011106     c                   if        §tavca=*blank
454500011106     c                   eval      §tavca='ITL'
454600011106     c                   end
454700011106      *
454800011106      *   con l'EURO è tolto ogni tipo di cambio dai files
454900011106      *    quindi adesso occorre passare a (0) sempre il cambio
455000011106      *     sarà il programma FNLRB9R che dovrà eseguire il
455100011106      *      ragionamwento di conversione DIVISA/EURO.
455200020129     C*
455300950131     C* VEDO SE C/A PRONTAMENTE RECUPERATO
455400931124     C                   EXSR      CTRASS
455500931124    2C                   ENDIF
455600000623     C* SE IL CODICE MANCATA CONSEGNA È VALORIZZATO
455700000705     C*
455800000705     C                   MOVEL     *BLANK        §TAFGC
455900000623    2C     FTDCMC        IFNE      *BLANK
456000000623     C* VERIFICO SE BOLLA POSTA
456100000623     C                   EXSR      CTRORG
456200000623     C*  P.O. POSTE
456300050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
456400020729     C     §OgNTW        IFEQ      'PPT'
456500050426     C     vh4tpo        andeq     'S'
456600000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
456700000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
456800000623     C*
456900000623     C                   MOVEL     '2Z'          COD
457000000623     C                   MOVEL     *BLANKS       KEY
457100000623     C                   MOVEL     FTDCMC        KEY
457200000623     C     KTAB          CHAIN     TABEL                              30
457300000623     C  N30              MOVEL     TBLUNI        DS2Z
457400000623     C   30              MOVEL     *BLANKS       DS2Z
457500000623     C                   MOVEL     §2ZTPP        §TAFGC
457600000623     C                   MOVEL     §2ZTPP        FTDFGC
457700000623    3C                   ENDIF
457800000623    2C                   ENDIF
457900931124     C* SOLO SE CONSEGNATA
458000931124    2C     FTDDCM        IFGT      0
458100950116     C                   MOVEL     ARBTC1        §TATC1                          CONS PARTIC
458200950116     C                   MOVEL     ARBTC2        §TATC2                          CONS PARTIC
458300000623    2C                   ENDIF
458400950118     C* GESTIONE PARTICOLARITA' VARIE
458500960208     C                   MOVEL     ARBGVA        §TAGVA
458600960208     C                   MOVE      ' '           §TAGVA
458700091012     C*
458800091012    1C*********          ENDIF
458900091012     C** END chiudeva BLP in partenza
459000091012    1C*********
459100091012     C*----------
459200091012     C*
459300950125     C     §TAPAG        IFEQ      ' '
459400950125     C                   MOVE      *HIVAL        §TAITA
459500950125     C                   END
459600950125     C     §TASIC        IFEQ      'S'
459700950125     C                   MOVE      *HIVAL        §TAITT
459800950125     C                   END
459900030821     C*----------
460000030821     C* contatore delle spedizioni presenti nello stesso STOP
460100030821     C* di quante "FW" e di quante "RCC".
460200030821     c     §tatsm        ifeq      'S'
460300090918     C*
460400030821     c                   z-add     W01nss        §taNSS
460500030821     c                   z-add     W01fw         §taFW
460600030821     c                   z-add     W01rcc        §taRCC
460700030821     c                   z-add     W01nss        SAVnss
460800030821     c                   z-add     W01fw         SAVfw
460900030821     c                   z-add     W01rcc        SAVrcc
461000090911      *
461100090911      *  sullo stop tariffe particolari x cliente
461200090914     c                   eval       SAV_TarPartCLI = W01_TarPartCLI
461300090914     c                   eval       SAV_UnoOpiuCLI = W01_UnoOpiuCLI
461400090918     C*
461500090918      * se valorizza a Spedizione
461600090918     C*   Deve passare il codice del cliente solo se è un cliente particolare
461700090918     c     CodCliente    lookup    KcliPAR                                30
461800090918     c                   if        *in30
461900090918     C                   Z-ADD     arbKSC        §TAcliPAR
462000090918     C                   end
462100090918     C*
462200090911      *
462300030821     c                   else
462400090918      * se valorizza a Stop
462500090918     C*
462600030821      * quando è a stop imposta i campi precedentemente salvati altrimenti
462700030821      * sarebbero impostati per il giro successivo del nuovo STOP
462800030821     c                   z-add     SAVnss        §taNSS
462900030821     c                   z-add     SAVfw         §taFW
463000030821     c                   z-add     SAVrcc        §taRCC
463100090911      *
463200090911      *  sullo stop tariffe particolari x cliente
463300090911     c                   eval       §TACLIPAR      = SAV_UnoOpiuCLI
463400090911     c                   eval       §TATUTTE       = SAV_TarPartCLI
463500090911      *
463600030821     c                   end
463700030821     C*----------
463800020426     C                   MOVEL     'CALCVAL'     §TApgm
463900020205     C*
464000020122     C                   CALL      'FICNB2R'
464100020122     C                   PARM                    FICNB2
464200020328      *
464300020328      * cancella i records utilizzati nello STOP dal file di work
464400020328     C     §TAtsm        IFEQ      'T'
464500020422    3C     parsml        andeq     *blank
464600020328     c                   exsr      del_FTDW
464700020328     C                   END
464800020328      *
464900931102    0C                   ENDIF
465000931102     C*
465100931102     C* E R R ="E"  M A N C A   T A R I F F A
465200931103    1C     §TAERR        IFEQ      'E'
465300931102     C                   SETON                                        4290
465400931102     C* ERRORE: SEGNALO LA DISTINTA
465500931112     C     VI4SCE        IFNE      'E'
465600931102     C                   MOVEL     'E'           VI4SCE
465700011109     c     vh4tpr        comp      2                                      26
465800931102     C                   UPDATE    FRB8SF4
465900931112     C                   ENDIF
466000931102     C* STAMPA MANCA TARIFFA
466100931102     C                   EXSR      STAERR
466200931102     C*
466300931103   X1C                   ELSE
466400931102     C*
466500931102     C* N O N   E '   M A N C A   T A R I F F A
466600931102     C* SE BASE      DISTINTA VALORIZZATA AZZERO IL CAMPO IN BOLLA
466700931103    2C     *IN15         IFEQ      *ON
466800931102     C                   CLEAR                   FTDITT
466900931102     C                   CLEAR                   FTDFIT
467000931103   X2C                   ELSE
467100931102     C*
467200931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
467300020226    3C     §TAITT        IFNE      *ALL'9'
467400020320     C************       Z-ADD     §TAITT        FTDITT                          BASE
467500020214     C                   add       §TAITT        FTDITT                          BASE
467600931109     C                   ADD       §TATPE        FTDITT                          PIK/ETICH
467700931109     C                   ADD       §TATST        FTDITT                          STOP
467800020320     C************       ADD       FTDITT        W01ITT
467900020320     C                   add       §taITT        w01ITT                          BASE
468000020320     C                   add       §taTPE        w01ITT                          PIK/ETICH
468100020320     C                   add       §taTST        w01ITT                          STOP
468200020226    3C                   ENDIF
468300931103    2C                   ENDIF
468400931102     C* SE ACCESSORI DISTINTA VALORIZZATI AZZERO IL CAMPO IN BOLLA
468500931103    2C     *IN16         IFEQ      *ON
468600931102     C                   CLEAR                   FTDITA
468700931102     C                   CLEAR                   FTDFIA
468800931103   X2C                   ELSE
468900931102     C*
469000931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
469100020226    3C     §TAITA        IFNE      *ALL'9'
469200931102     C                   ADD       §TAITA        W01ITA
469300020214     C*****              Z-ADD     §TAITA        FTDITA
469400020214     C                   add       §TAITA        FTDITA
469500020226    3C                   ENDIF
469600931103    2C                   ENDIF
469700931102     C*
469800931103    1C                   ENDIF
469900931102     C*
470000931102     C* SE VALORIZZATA SIA BASE CHE ACCESSORI
470100931102     C*  CLEAR SE IMPOSTATA A LIVELLO DI TESTATA
470200931102     C   15              CLEAR                   FTDITT
470300931102     C   15              CLEAR                   FTDFIT
470400931102     C   16              CLEAR                   FTDITA
470500931102     C   16              CLEAR                   FTDFIA
470600931102     C                   ENDSR
470700020212     C*---------------------------------------------------------------*
470800931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA TASSARE/SOMMARE ---------*
470900020212     C*---------------------------------------------------------------*
471000931102     C     CTRSTP        BEGSR
471100940606     C                   SETOFF                                       17
471200931102     C* S O M M A   P E S I   A   S T O P
471300931102     C     W01STP        IFNE      0
471400931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
471500011105     C     W05NRR        CHAIN     fiftd000                           32
471600931102     C*
471700020122     C                   CLEAR                   FICNB2
471800931102     C                   Z-ADD     W01KFA        §TAKFA
471900931102     C                   Z-ADD     W01TVL        §TATVL
472000931102     C                   Z-ADD     W01NCL        §TANCL
472100950118     C                   Z-ADD     W01CPE        §TACPE
472200950123     C                   MOVEL     'S'           §TAPAG
472300950118     C     §TAKFA        IFEQ      0
472400950118     C                   MOVE      *BLANKS       §TAPAG
472500950118     C                   END
472600931102     C                   MOVEL     *HIVAL        §TAITA
472700931123     C     FTDCTP        IFEQ      *BLANKS
472800931123     C                   MOVEL     WALF3         FTDCTP
472900931123     C                   ENDIF
473000950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
473100021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
473200020325     C                   movel     w01isola      §taISO
473300931102     C*
473400020213     C                   MOVEL     'T'           §TAtsm
473500931102     C                   EXSR      TASSAZ
473600931102     C*
473700020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
473800020326     C* i primi tre caratteri
473900020326     c  n32              if        ftdfgs = 0
474000020326     c                   movel     ftdpdr        ftdfgs
474100020326     c                   end
474200020326     C*
474300011105     C  N32              UPDATE    fiftd000
474400931102     C                   ENDIF
474500931102     C*
474600931102     C* S O M M A   P E S I   A   G I O R N A T A
474700950214     C     W02STP        IFGT      0
474800931102     C                   ADD       1             W01SNA
474900020212     C                   ADD       1             g01SNA
475000931102     C                   ENDIF
475100931102     C                   ENDSR
475200020212     C*---------------------------------------------------------------*
475300931103     C* SOMMO LE VALORIZZAZIONI CHE CI SONO NEL DETTAGLIO -----------*
475400020212     C*---------------------------------------------------------------*
475500931103     C     AGGFTD        BEGSR
475600011105     C     KFTD          SETLL     fiftd001
475700011105     C     KFTD          READE     fiftd001                               32
475800931103     C*
475900931103     C     *IN32         DOWEQ     *OFF
476000020405     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
476100020405     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
476200020405     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
476300020405     C     *IN19         IFEQ      *on
476400020405     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
476500020405     C                   ELSE
476600020405     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
476700020405     C                   END
476800931103     C* TESTATA
476900931103     C     *IN15         IFEQ      *ON
477000931103     C                   CLEAR                   FTDITT
477100931103     C                   CLEAR                   FTTFIT
477200931103     C                   ELSE
477300931103     C                   ADD       FTDITT        W01ITT
477400931103     C                   ENDIF
477500931103     C* DETTAGLIO
477600931103     C     *IN16         IFEQ      *ON
477700931103     C                   CLEAR                   FTDITA
477800931103     C                   CLEAR                   FTTFIA
477900931103     C                   ELSE
478000931103     C                   ADD       FTDITA        W01ITA
478100931103     C                   ENDIF
478200020326     C*
478300020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
478400020326     C* i primi tre caratteri
478500020326     c                   if        ftdfgs = 0
478600020326     c                   movel     ftdpdr        ftdfgs
478700020326     c                   end
478800020326     C*
478900011105     C                   UPDATE    fiftd001
479000931103     C*
479100011105     C     KFTD          READE     fiftd001                               32
479200931103     C                   ENDDO
479300931103     C*
479400931103     C                   ENDSR
479500020212     C*---------------------------------------------------------------*
479600931103     C* STAMPA MANCA TARIFFA -----------------------------------------*
479700020212     C*---------------------------------------------------------------*
479800950116     C     STAERR        BEGSR
479900950116     C                   MOVEL     FTDRSC        W01RSB           15
480000931103     C  N18VH4TSR        IFEQ      'R'
480100931104     C                   SETON                                        19
480200931103     C                   ELSE
480300931104     C                   SETOFF                                       19
480400931103     C                   ENDIF
480500931103     C* TESTATA
480600931103     C     *INOF         IFEQ      *ON
480700950127     C     VI4DDC        ORNE      WM1DDC
480800931103     C                   EXCEPT    TES
480900931103     C                   SETOFF                                       OF
481000950127     C                   Z-ADD     VI4DDC        WM1DDC
481100931103     C                   ENDIF
481200950127     C  N18              MOVE      FTDAAS        AASPRT            2 0
481300950127     C  N18              MOVEL     FTDCAP        CAPPRT            5
481400931103     C* DETTAGLIO
481500931103     C   18              EXCEPT    DET1
481600950116     C  N18              EXCEPT    DET2
481700931103     C*
481800931103     C                   ENDSR
481900020212     C*---------------------------------------------------------------*
482000931103     C*  VEDO SE DARE MANCA TARIFFA ---------------------------------*
482100020212     C*---------------------------------------------------------------*
482200931103     C     MANTAR        BEGSR
482300020122     C                   CLEAR                   FICNB2
482400020426     C                   MOVEL     'CALCVAL'     §TApgm
482500931209     C                   MOVEL     PARSML        §TASML
482600931103     C                   MOVEL     VI4PDR        §TAPDR
482700020326     C                   MOVEL     vi4pdr        §TAfgs
482800011205     c                   movel     vi4ctd        §tadiv
482900020326     C                   MOVEL     vh4DDC        §TADDC
483000020326     C                   z-add     vi4ndc        §tandc
483100931103     C                   MOVEL     'M'           §TAFLG
483200020122     C                   CALL      'FICNB2R'
483300020122     C                   PARM                    FICNB2
483400931103     C*
483500931103     C* ERR=E DEVO DARE MANCA TARIFFA
483600931103    1C     §TAERR        IFEQ      'E'
483700931202     C                   SETON                                        184290
483800931103     C                   EXSR      STAERR
483900931202     C                   SETOFF                                       18
484000931103     C* SEGNALO LA DISTINTA
484100931103     C                   MOVEL     'E'           VI4SCE
484200011109     c     vh4tpr        comp      2                                      26
484300931103     C                   UPDATE    FRB8SF4
484400931103     C*
484500931103   X1C                   ELSE
484600931103     C* SOMMO LE SINGOLE VALORIZZAZIONI EVENTUALMENTE FATTE IN TESTATA
484700931103     C*  SENZA DARE ERRORE
484800931103     C                   EXSR      AGGFTD
484900931103    1C                   ENDIF
485000931103     C                   ENDSR
485100020212     C*---------------------------------------------------------------*
485200931103     OPRTF198   E            TES              02
485300931102     O                       STA(1)              66
485400931102     O                       STA(2)             132
485500931103     O                       STA(3)             198
485600931102     O                       RSUT                20
485700931102     O                       SIMFEL              24
485800950127     O                       VI4DDC              99 '  /  /  '
485900931103     O                       UDATE              160 '  .  .  '
486000931103     O                       PAGE          Z    198
486100931103     O          E            TES              03
486200931103     O                       UTIME              160 '  :  :  '
486300931103     O          E            TES              04
486400931103     O                       STA(4)              66
486500931103     O                       STA(7)             132
486600931103     O                       STA(10)            198
486700931103     O          E            TES            1 05
486800931103     O                       STA(5)              66
486900931103     O                       STA(8)             132
487000931103     O                       STA(11)            198
487100931103     O          E            DET1           1
487200931103     O                       STA(13)             66
487300931103     O                       STA(14)            132
487400931103     O                       STA(15)            198
487500931103     O                       VI4PDR               8
487600931103     O                                              '-'
487700931103     O                       VH4DPD
487800950120     O                       VH4TSH            +  1
487900931110     O                       VI4NDC        Z   +  2
488000931110     O                       W01ERR             170
488100931103     O          E            DET2           1
488200931103     O                       STA(13)             66
488300931103     O                       STA(14)            132
488400931103     O                       STA(15)            198
488500931103     O                       VI4PDR               8
488600931103     O                                              '-'
488700931103     O                       VH4DPD
488800950120     O                       VH4TSH            +  1
488900931110     O                       VI4NDC        Z   +  2
489000950127     O                       AASPRT        Z   +  1
489100931103     O                       FTDLNP        Z   +  1
489200931103     O                       FTDNRS        Z   +  1
489300931103     O                       FTDNSP        Z   +  1
489400931123     O              N19      W01RSB            +  1
489500931123     O              N19      FTDFIN            +  2
489600931123     O               19      W01RSB            +  2
489700931123     O               19      FTDFIN            +  2
489800950120     O                       FTDPKL        4   +  1
489900931103     O                       FTDNCL        Z   +  1
490000950116     O                       FTDVLU        4   +  1
490100950120     O              N19      CAPPRT            +  2
490200950120     O               19      CAPPRT            +  3
490300931110     O                       §TACTR            +  3
490400931110     O                       SER(1)            +  3
490500020418     O****fiftd001  E            UPDFTD
490600020418     O****                       FTDSTP
490700931103**
490800931028NON ESISTONO DISTINTE/RITIRI da visualizzare per la selezione effettuata
490900030519Inserire Tariffe Valide per Distinte di Consegna -> Mancano le Tariffe
491000030519Inserire Tariffe Valide per Distinte di Ritiro   -> Mancano le Tariffe
491100931103**
491200931103XXXXXXXXXXXXXXXXXXXX/XXX                ***  CONTROLLO  TASSAZIONE   1
491300020322  AUTOTRASP.  DEL GIORNO XX/XX/XX  ***                     FICNB1R   2
491400931103                    XX.XX.XX                             PAG. XXXX   3
491500931110'      C O D I C E      '   TIPO   ' NUMERO '  SPEDIZIONE     '      4
491600020322'   AUTOTRASPORTATORE   ' PRESTAZ. 'DISTINTA'                 'RAG   5
491700931103'XXXXXXX-XXXXXXXXXXXXXXX'XXXXXXXXXX' XXXXXXX'XX XXX XX XXXXXXX'XXX   6
491800931110DESTINATARIO   '     MITTENTE      ' PESO   'COLLI'VOLUME'  CAP  '
491900931110.SOCIALE     IN' RAG.SOCIALE     AN'        '     '      ' DEST. '
492000950120XXXXXXXXXXXX  X' XXXXXXXXXXXXXXX  X'XXXXXX,X'XXXXX'XX,XXX' XXXXX '
492100931110  CAP  ' COD '     A N O M A L I A      '
492200931110 MITT. ' TAR '                          '
492300931110 XXXXX ' XXX ' XXXXXXXXXXXXXXXXXXXXXXXXX'
492400931110'                       '          '        '                 '     13
492500931110               '                   '        '     '      '       '  14
492600931110       '     '                          '                           15
492700931103**
492800931103MANCA TARIFFA
492900020322L'AUTOTRASP.NON HA TARIFFE
493000020328**
493100020328RGZPFM FILE(FIFTDW0F)
