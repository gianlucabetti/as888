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
019500030821     D FICNB1        E DS                  EXTNAME(FICNB1DS)
019600020122     D FICNB2        E DS                  EXTNAME(FICNB2DS)
019700020603     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
019800931021     D CNCR80        E DS
019900900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
020000100513      *
020100100513     D ddatiute      e ds                  prefix(UT_)
020200100513     D azuteds       e ds                  extname(AZUTE00F)
020300100513     D tibs34ds      E DS                  inz
020400100513      *---------------------------------------------------------------------*
020500100513     D FICNB1ds0     E DS
020600040622      *---------------------------------------------------------------------*
020700040825     d   tar_valida    S              1    INZ('N')
020800040825     d rifPDR          s                   like(fgtPDR)
020900040825     d rifCTR          s                   like(fgtCTR)
021000040825     d ptarv_rap       s                   like(fgtrap)
021100040825     d ptarv_rct       s                   like(fgtrct)
021200040622     d dataoggi        s               d   datfmt(*iso)
021300040622     d finoadata       s               d   datfmt(*iso) inz(d'2004-08-10')
021400090911     d SAV_TarPartCLI  s                   like(W01_TarPartCLI)
021500090911     d SAV_UnoOpiuCLI  s                   like(W01_UnoOpiuCLI)
021600020328      *---------------------------------------------------------------------*
021700020328     D  MSGDS          S            100
021800020328     D  LENGH          S             15  5
021900020328      *---------------------------------------------------------------------*
022000931018     D***
022100950118     D* DEFINIZIONE COSTANTI
022200931018     D***
022300931018     D CDAVAL          C                   CONST('DA VALORIZZARE')
022400931018     D CVALOR          C                   CONST('VALORIZZATE   ')
022500931123     D CMITT           C                   CONST('--M I T T E N T E-- ')
022600931202     D CSPED           C                   CONST('---- SPEDIZIONE ----')
022700950119     D COS1            C                   CONST('Uscita: MAT')
022800950119     D COS2            C                   CONST('Uscita: POM')
022900950119     D COS3            C                   CONST('  Piking  ')
023000950119     D COS4            C                   CONST('  Etich.  ')
023100021211     D COS5            C                   CONST('Caric.')
023200021211     D COS6            C                   CONST(' Scar.')
023300011105     IFIFTT002
023400000714     I              FTTATB                      FT2ATB
023500011105     I              FTTpdu                      FT2pdu
023600000714     I              FTTPDR                      FT2PDR
023700000714     I              FTTTSR                      FT2TSR
023800021211     I              FTTlvl                      FT2FCS
023900011105     I              FTTdiv                      FT2div
024000011105     I              FTTfgs                      FT2fgs
024100000714     I              FTTNDC                      FT2NDC
024200000714     I              FTTDDC                      FT2DDC
024300000714     I              FTTFVL                      FT2FVL
024400000714     I              FTTFLA                      FT2FLA
024500000714     I              FTTSET                      FT2SET
024600000714     I              FTTSNT                      FT2SNT
024700000714     I              FTTSNP                      FT2SNP
024800000714     I              FTTSNE                      FT2SNE
024900000714     I              FTTSNA                      FT2SNA
025000000714     I              FTTCLA                      FT2CLA
025100000714     I              FTTCPE                      FT2CPE
025200000714     I              FTTKFA                      FT2KFA
025300000714     I              FTTTVL                      FT2TVL
025400000714     I              FTTITT                      FT2ITT
025500000714     I              FTTFIT                      FT2FIT
025600000714     I              FTTITA                      FT2ITA
025700000714     I              FTTFIA                      FT2FIA
025800000714     I              FTTTPE                      FT2TPE
025900000714     I              FTTTBN                      FT2TBN
026000000714     I              FTTTIM                      FT2TIM
026100000714     I              FTTMNT                      FT2MNT
026200000714     I              FTTDFT                      FT2DFT
026300011105     I              FTTNFF                      FT2NFT
026400000714     I              FTTTBS                      FT2TBS
026500000714     I              FTTCUC                      FT2CUC
026600000714     I              FTTFPP                      FT2FPP
026700000714     I              FTTPEP                      FT2PEP
026800000714     I              FTTDCV                      FT2DCV
026900011105     I              FTThlv                      FT2hlv
027000011105     I              FTTctg                      FT2ctg
027100011105     I              FTTdcn                      FT2dcn
027200011105     I              FTTsoc                      FT2soc
027300011105     I              FTTflr                      FT2flr
027400011105     I              FTTftr                      FT2ftr
027500011105     I              FTTdtr                      FT2dtr
027600000000     I/SPACE 3
027700900521     C****************************************************************
027800900521     C*  RIEPILOGO INDICATORI
027900900521     C***************************************************************
028000931018     C* 01    - IMMESSA SOLO DATA DAL
028100931018     C* 02    - IMMESSA DATA AL
028200950112     C* 03    -
028300931108     C* 04    - IMMESSO TIPO PRESTAZIONE
028400950116     C* 05    - IMMESSO UN SOLO NUMERO DISTINTA (SOLO PER CONSEGNE)
028500931201     C* 06    - FASE DI SIMULAZIONE
028600931124     C* 09    - LEGGO FLARBF1C CON I CAMPI DEL LEGAME BOLLA
028700931124     C* 11    - PROTEGGO COD PADRONCINO IN SFL4 PER NON RIPETERLO
028800931103     C* 12    - PROTEGGO COD PADRONCINO IN SFL2 PER NON RIPETERLO
028900950117     C* 13    - PROTEGGO IMPORTO ACCESSORI IN SFL6 PER NON INSERIRLI
029000931022     C* 14    - ANNULLO TASSAZIONI SINGOLE DI BOLLE SE DISTINTA TASSATA
029100931022     C*         IN MODO COMPLETO DA VIDEO
029200931022     C* 15    - IMMESSO IMPORTO BASE      A LIVELLO DI DISTINTA
029300931022     C* 16    - IMMESSO IMPORTO ACCESSORI A LIVELLO DI DISTINTA
029400931022     C* 17    - ON SPEDIZIONE DA NON TASSARE
029500931102     C* 18    - ON MANCA TARIFFA PER MANCANZA SOMMA PESI
029600931104     C* 19    - ON E' RITIRO   OFF E' CONSEGNA
029700931028     C* 20    - ON PROGRAMMA RICHIAMATO
029800931105     C* 21    - ON SVALORIZZATE DISTINTE: RIEMETTO SFL2
029900931110     C* 22    - ON OBBLIGO CMD6 SE AGGIORNO DISTINTA CON MANCA TARIFFA
030000950117     C* 23    - RITORNO ALL'INCASSO E PROTEZIONE IMPORTO BASE
030100950131     C* 24    - DISTINTA VALORIZZATA
030200000626     C* 25    - NON SI PAGA LA SPEDIZIONE PER BOLLE POSTE NON CONSEGNATE
030300011109     C* 26    - PROTECT DIVISA
030400931022     C* 30/32 - DI COMODO
030500000622     C* 34    - DI COMODO
030600931018     C* 35    - PULIZIA ES EMISSIONE SFL
030700931018     C* 36    - SFLNXTCHG
030800931110     C* 37    - EMISSIONE SFL6
030900000622     C* 40/48 - ERRORI
031000000626     C* 49    - PROTEGGO IL CAMPO NUMERO STOÈP PER BOLLE POSTE
031100000622     C* 50    - EMISSIONE ERRORE TARIFFA ERRATA BARTOLINI INESISTENTI
031200000622     C* 51    - EMISSIONE ERRORE TARIFFA ERRATA POSTE INESISTENTI
031300000622     C* 52/53 - TARIFFA ERRATA
031400011108     C* 60    - ON protegge le righe del windowsfl
031500011108     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
031600900521     C*****************************************************************
031700100513     C                   clear                   ficnb1ds0
031800100513     c                   eval      FN0PGM  = 'FICNB1R'
031900100513     c                   eval      FN0TIPO = 'FILIALE'
032000100513     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
032100100513     C                   MOVEL     KPJBU         kpjbus
032200100513     C                   clear                   kpjbu
032300100513     C                   MOVEL     ficnb1ds0     kpjbu
032400100513     c                   call      'FICNB1R0'
032500100513     c                   parm                    kpjba
032600100513     C                   MOVEL     kpjbu         ficnb1ds0
032700100513     C*   se già utilizzata da altro utente della stessa filiale fa uscire
032800100513     C*   forzatamente.
032900100513     c                   if        Fn0FLG <> *blank
033000100513     c                   goto      FINE
033100100513     c                   end
033200100513     C*
033300100513     C                   MOVEL     KPJBUs        kpjbu
033400100513     C*
033500931015     C     INIZIO        TAG
033600931015     C* PULIZIA FORMATO 1
033700931103     C  N20              EXSR      PUL01
033800930426     C*
033900931015     C     FOR01         TAG
034000931028     C*
034100931028     C* PGM NON RICHIAMO - EMETTO 1 VIDEATA ALTRIMENTI VADO ALLA 2A
034200931105     C* SE SVALORIZZATE SPEDIZIONI EMETTO SUBITO LA 2A (21 ON)
034300931028     C     *IN20         IFEQ      *OFF
034400931105     C     *IN21         ANDEQ     *OFF
034500931028     C*
034600931015     C                   EXFMT     FRB8D01
034700020409     c                   z-add     0             savnr2
034800020409     c                   z-add     0             savnr4
034900020409     c                   z-add     0             savnr6
035000020604     c                   move      'N'           seF6              1
035100020604      * se richieste le distinte già Valorizzate non deve permettere di rivalorizzare
035200020604      * quindi imposto la memoria del flag SeF6 come se fosse stato dato l'F6 di
035300020604      * valorizzazione e impedire un nuovo F6 che raddoppia i conteggi.
035400020604     c                   if        vi1fvl ='V'
035500020604     c                   move      'S'           seF6
035600020604     c                   end
035700931103     C                   SETOFF                                       4746
035800920323     C** CMD3 - FINE LAVORO
035900931028     C   KC              GOTO      FINE
036000931015     C* CONTROLLI FORMATO1
036100900528     C                   EXSR      CONTR1
036200900528     C   90              GOTO      FOR01
036300931028     C                   ENDIF
036400931018     C*
036500931018     C* CARICO DISTINTE SELEZIONATE
036600931018     C                   EXSR      CARNDC
036700950616     C   90
036800950616     CAN 20              GOTO      FINE
036900950616     C   90              GOTO      FOR01
037000900611     C*
037100931119     C                   Z-ADD     1             REC2
037200900518     C*
037300931018     C     FORCT2        TAG
037400931103     C  N90              WRITE     FRB8D03
037500020603     c                   if        savnr2 > 0 and savnr2 <= rec2
037600020409     c                   z-add     savnr2        rec2
037700020409     c                   end
037800931018     C                   EXFMT     FRB8CT2
037900020409     c                   z-add     nrrult        savnr2            5 0
038000020411     c                   z-add     0             savnr4
038100931028     C   KC              GOTO      FINE
038200931028     C   KL
038300931028     CANN20              GOTO      FOR01
038400931028     C   KL
038500931028     CAN 20              GOTO      FINE
038600931018     C*
038700931018     C** CMD1 O ENTER - SELEZIONE DISTINTE
038800931018     C                   EXSR      SELNDC
038900931018     C   90              GOTO      FORCT2
039000931018     C*
039100931103     C                   Z-ADD     1             REC4
039200931018     C     FORCT4        TAG
039300020604      *
039400020604      * se ha già eseguito la valorizzazione non deve permettere un secondo
039500020604      * F6 a causa di un manca tariffa
039600020604     c     seF6          comp      'N'                                    88
039700020604      *
039800931103     C  N90              WRITE     FRB8D05
039900020409     c                   if        savnr4 > 0
040000020409     c                   z-add     savnr4        rec4
040100020409     c                   end
040200931018     C                   EXFMT     FRB8CT4
040300020604      *
040400020409     c                   z-add     nrrult        savnr4            5 0
040500020411     c                   z-add     0             savnr6
040600960906     C                   SETOFF                                       90
040700931110     C* SE AGGIORNATO DISTINTA CON DEI MANCA TARIFFA OBBLIGO IN CMD6
040800931110     C     *INKL         IFEQ      *ON
040900931110     C     *INKC         OREQ      *ON
041000931110     C   22              SETON                                        4490
041100931110     C   22              GOTO      FORCT4
041200931110     C                   ENDIF
041300130305     C*
041400931105     C   KC              GOTO      FINE
041500931110     C   KL
041600931110     CANN20              GOTO      FORCT2
041700931110     C   KL
041800931110     CAN 20              GOTO      FINE
041900931018     C* CONTROLLO SFL4
042000931018     C                   EXSR      CONTR4
042100931103     C* CMD3 - DALLA VIDEATA DI MANUTENZIONE SPEDIZIONI
042200931019     C   LR              GOTO      FINE
042300931019     C* ERRORI
042400931105     C   90              GOTO      FORCT4
042500931105     C* RIEMETTO IL SFL2 PERCHE' HO ANNULLATO DELLE VALORIZZAZIONI
042600931105     C   21              GOTO      FOR01
042700931105     C* CMD6 - ESEGUO VALORIZZAZIONE
042800020604     c   kf              move      'S'           seF6
042900931105     C   KF              EXSR      VALZZ
043000020408     C   kf              commit
043100931105     C  NKF
043200931105     COR KF
043300931105     CAN 90              GOTO      FORCT4
043400931028     C  N20              GOTO      INIZIO
043500900611     C*
043600000000     C     FINE          TAG
043700100513     C*
043800100513     C*  deve cancellare il record di allocazione x impedire le trasmissioni
043900100513     C*   in sede durante l'utilizzo di questo programma.
044000100513     c     parsml        ifeq      *blank
044100100513     C     Kwalc         setll     fiwalc1l
044200100513     C     Kwalc         reade     fiwalc1l
044300100513     c                   dow       not %eof(fiwalc1l)
044400100513     c                   delete    fiwalc00
044500100513     C     Kwalc         reade     fiwalc1l
044600100513     c                   end
044700100513     c                   end
044800100513      *
044900020408     c                   commit
045000020408     C*
045100020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
045200020328     C*   economico.
045300020328     C                   clear                   FICNB2
045400020328     C                   move      'C'           §taflg
045500020328     C                   CALL      'FICNB2R'
045600020328     C                   PARM                    FICNB2
045700020328     C*
045800020328     c                   close     fiftdw1l
045900020328     C*
046000020422     c                   if        parsml=' '
046100051109     c                   goto      norgzpfm
046200020328     C                   clear                   MSGDS
046300020328     C                   MOVEL     CMD(1)        MSGDS
046400020328     C                   Z-ADD     80            LENGH
046500020328     C                   CALL      'QCMDEXC'                            99
046600020328     C                   PARM                    MSGDS
046700020328     C                   PARM                    LENGH
046800051109     c     norgzpfm      tag
046900051109     c                   end
047000020328     C*
047100931028     C                   SETON                                        LR
047200950112     C****************************************************************
047300950112     C     *INZSR        BEGSR
047400950112     C*
047500950112     C     *ENTRY        PLIST
047600950112     C                   PARM                    KPJBA
047700950112     C                   MOVEL     KPJBU         PARAM
047800100513      *
047900100513     c     *dtaara       define    §azute        azuteds
048000100513     c     *dtaara       define    §datiute      ddatiute
048100100513     C                   in(E)     *dtaara
048200100513     C                   IF        %Error  or  RSUT = *blanks
048300100513     C                   call      'TIBS34R'
048400100513     C                   parm                    Tibs34Ds
048500100513     C                   in        *dtaara
048600100513     c                   ENDIF
048700950112     C*
048800950112     C                   Z-ADD     1             CODUT
048900950112     C                   CALL      'X§PARUT'
049000950112     C                   PARM                    UT§DSE
049100950112     C                   MOVEL     RAGUT         RSUT             20
049200950112     C                   MOVEL     REC80         CNCR80
049300020328     C*
049400020328     C* apertura workf per ripartizione voci di conto economico
049500020328     c                   open      fiftdw1l
049600950112     C*
049700950112     C* DEFINIZIONE CHIAVI DI ACCESSO
049800100513     C     Kwalc         KLIST
049900100513     C                   KFLD                    WalPGM
050000100513     C                   KFLD                    WalNOJ
050100100513     C                   KFLD                    WalNUS
050200100513     C                   KFLD                    WalNRJ
050300090908      *
050400090908     C     KTbe          KLIST
050500090908     C                   KFLD                    tbeCOD
050600090908     C                   KFLD                    tbeKE1
050700090908     C                   KFLD                    tbeKE2
050800090908      *
050900950112     C     KTAB2         KLIST
051000950112     C                   KFLD                    CODUT
051100950112     C                   KFLD                    COD               2
051200950112     C     KTAB          KLIST
051300950112     C                   KFLD                    CODUT
051400950112     C                   KFLD                    COD               2
051500950112     C                   KFLD                    KEY               8
051600950113     C     KFTT          KLIST
051700950113     C                   KFLD                    VI4PDR
051800950113     C                   KFLD                    VH4TSR
051900950113     C                   KFLD                    VI4NDC
052000950116     C                   KFLD                    VH4DDC
052100950112     C     KFTT1         KLIST
052200950112     C                   KFLD                    W01PDR
052300950112     C                   KFLD                    VI1TSR
052400950112     C                   KFLD                    W01NDC
052500950112     C     KFTT3         KLIST
052600950112     C                   KFLD                    W01PDR
052700950112     C                   KFLD                    VI1TSR
052800060118     C     K21pdr        KLIST
052900060118     C                   KFLD                    VI1fvl
053000060118     C                   KFLD                    W01PDR
053100060118     C     K21pdr1       KLIST
053200060118     C                   KFLD                    VI1fvl
053300060118     C                   KFLD                    W01PDR
053400060118     C                   KFLD                    VI1TSR
053500060118     C                   KFLD                    W01NDC
053600060118     C     K21pdr3       KLIST
053700060118     C                   KFLD                    VI1fvl
053800060118     C                   KFLD                    W01PDR
053900060118     C                   KFLD                    VI1TSR
054000091112     C     Ktgt          KLIST
054100091112     C                   KFLD                    fgtPDR
054200091112     C                   KFLD                    fgtSML
054300091112     C                   KFLD                    fgtPRG
054400950112     C     KFGT          KLIST
054500950112     C                   KFLD                    VI4PDR
054600950112     C                   KFLD                    PARSML
054700950112     C                   KFLD                    VH4TSR
054800950112     C     KFGT2         KLIST
054900950112     C                   KFLD                    VI4PDR
055000950112     C                   KFLD                    PARSML
055100950112     C                   KFLD                    VH4TSR
055200950112     C                   KFLD                    W01CTR
055300040825      *
055400040825     C     KFGT_rif      KLIST
055500040825     C                   KFLD                    rifPDR
055600040825     C                   KFLD                    PARSML
055700040825     C                   KFLD                    VH4TSR
055800040825     C                   KFLD                    rifCTR
055900950112     C     KFTD          KLIST
056000950112     C                   KFLD                    VI4PDR
056100950112     C                   KFLD                    VH4TSR
056200950112     C                   KFLD                    VI4NDC
056300950116     C                   KFLD                    VH4DDC
056400950112     C     KFTD2         KLIST
056500950116     C                   KFLD                    VH6AAS
056600950116     C                   KFLD                    FTDLNP
056700950116     C                   KFLD                    FTDNRS
056800950116     C                   KFLD                    VI6NSP
056900950116     C                   KFLD                    VH4TSR
057000950112     C                   KFLD                    VI4PDR
057100950112     C                   KFLD                    VI4NDC
057200950116     C                   KFLD                    VH4DDC
057300020315     C     KFTD5         KLIST
057400020315     C                   KFLD                    VI4PDR
057500020315     C                   KFLD                    VH4TSR
057600020315     C                   KFLD                    VI4NDC
057700020315     C                   KFLD                    VH4DDC
057800020315     C                   KFLD                    VH6AAS
057900020315     C                   KFLD                    FTDLNP
058000020315     C                   KFLD                    FTDNRS
058100020315     C                   KFLD                    VI6NSP
058200091012     C     Bolla_FTD     KLIST
058300950112     C                   KFLD                    FTDAAS
058400950112     C                   KFLD                    FTDLNP
058500950112     C                   KFLD                    FTDNRS
058600950112     C                   KFLD                    FTDNSP
058700091012      *
058800950112     C     KFTD4         KLIST
058900950112     C                   KFLD                    VI4PDR
059000950112     C                   KFLD                    VH4TSR
059100950112     C                   KFLD                    VI4NDC
059200950116     C                   KFLD                    VH4DDC
059300950112     C                   KFLD                    VI6NSP
059400950116     C                   KFLD                    FTDRSC
059500000622     C     KFTDT         KLIST
059600000622     C                   KFLD                    VI4PDR
059700000622     C                   KFLD                    VH4TSR
059800000714     C                   KFLD                    FT2NDC
059900000622     C                   KFLD                    VH2DDC
060000000714     C     KFTTT         KLIST
060100000714     C                   KFLD                    VI4PDR
060200000714     C                   KFLD                    VI1FVL
060300000714     C                   KFLD                    VH2DDC
060400000714     C                   KFLD                    VH4TSR
060500950112     C     KLBL          KLIST
060600950112     C                   KFLD                    LBLAAN
060700950112     C                   KFLD                    LBLLPN
060800950112     C                   KFLD                    LBLNRN
060900950112     C                   KFLD                    LBLNSN
061000041124      *
061100041124     C     Del_FCE       KLIST
061200041124     C                   KFLD                    fgs_vi4pdr        3 0
061300041124     C                   KFLD                    VI4PDR
061400041124     C                   KFLD                    VH4TSR
061500041124     C                   KFLD                    VI4NDC
061600041124     C                   KFLD                    VH4DDC
061700020326      *
061800020326     c     Kfce          Klist
061900020326     C                   kfld                    ftdfgs
062000020326     C                   kfld                    ftdpdr
062100020326     C                   kfld                    ftdtsr
062200020326     C                   kfld                    ftdndc
062300020326     C                   kfld                    ftdddc
062400020326     C                   kfld                    ftdaas
062500020326     C                   kfld                    ftdlnp
062600020326     C                   kfld                    ftdnrs
062700020326     C                   kfld                    ftdnsp
062800020326      *
062900020326     c     KfceD         Klist
063000020326     C                   kfld                    vh6fgs
063100020326     C                   kfld                    vi4pdr
063200020326     C                   kfld                    vh6tsr
063300020326     C                   kfld                    vi4ndc
063400020326     C                   kfld                    vh4ddc
063500020326     C                   kfld                    vh6aas
063600020326     C                   kfld                    ftdlnp
063700020326     C                   kfld                    ftdnrs
063800020326     C                   kfld                    vh6nsp
063900020328     C*
064000020422     c     KftdW0        Klist
064100020328     C                   kfld                    kftwfgs
064200020328     C                   kfld                    kftwpdr
064300020328     C                   kfld                    kftwtsr
064400020328     C                   kfld                    kftwndc
064500020328     C                   kfld                    kftwddc
064600020422     C*
064700020422     c     KftdW         Klist
064800020422     C                   kfld                    kftwfgs
064900020422     C                   kfld                    kftwpdr
065000020422     C                   kfld                    kftwtsr
065100020422     C                   kfld                    kftwndc
065200020422     C                   kfld                    kftwddc
065300020328     C                   kfld                    kftwstp
065400021202     c     Kapdv         Klist
065500021202     C                   kfld                    apdtip
065600021202     C                   kfld                    vi1p1
065700021202     c     Kapdf         Klist
065800021202     C                   kfld                    apdtip
065900021202     C                   kfld                    fttpdr
066000021202     c                   eval      apdtip = 'A'
066100950112     C***
066200950112     C* 20 ON - PROGRAMMA RICHIAMATO
066300950112     C     PARTSR        IFNE      ' '
066400950112     C                   EXSR      PUL01
066500950112     C                   SETON                                        20
066600950112     C                   MOVEL     'V'           VI1FVL
066700950112     C                   MOVEL     PARPDR        VI1P1
066800950112     C                   MOVEL     PARTSR        VI1TSR
066900950112     C                   ENDIF
067000950112     C***
067100950112     C* 06 ON - FASE DI SIMULAZIONE
067200950112     C     PARSML        IFEQ      'S'
067300950112     C                   SETON                                        06
067400950112     C                   ENDIF
067500950112     C***
067600950112     C* CARICO TABELLA FILIALI GESTITE £1
067700950112     C                   CLEAR                   DSUL06
067800950112     C                   MOVE      '£1'          D06COD
067900020717     C                   MOVEL     SIMPOU        D06KEY
068000950112     C                   MOVEL     ' '           D06TLA
068100100513     C                   MOVEL     KPJBU         kpjbus
068200100513     C                   clear                   kpjbu
068300950112     C                   MOVEL     DSUL06        KPJBU
068400950112     C                   CALL      'TRUL06R'
068500950112     C                   PARM                    KPJBA
068600950112     C                   MOVEL     KPJBU         DSUL06
068700950112     C                   MOVEA     LIN           L1
068800100513     C                   MOVEL     KPJBUs        kpjbu
068900950112     C***
069000950112     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
069100950112     C                   CLEAR                   DSUL06
069200950112     C                   MOVE      '£6'          D06COD
069300020717     C                   MOVEL     SIMPOU        D06KEY
069400950112     C                   MOVEL     'L'           D06TLA
069500100513     C                   MOVEL     KPJBU         kpjbus
069600100513     C                   clear                   kpjbu
069700950112     C                   MOVEL     DSUL06        KPJBU
069800950112     C                   CALL      'TRUL06R'
069900950112     C                   PARM                    KPJBA
070000950112     C                   MOVEL     KPJBU         DSUL06
070100950112     C                   MOVEA     LIN           L6
070200100513     C                   MOVEL     KPJBUs        kpjbu
070300950112     C*
070400950112     C***
070500950112     C* CARICO TABELLA DEI TIPI PRESTAZIONE
070600950112     C                   Z-ADD     1             C
070700950112     C                   MOVEL     '1Z'          COD
070800950112     C     KTAB2         SETLL     TABEL
070900950112     C     KTAB2         READE     TABEL                                  30
071000950112     C*
071100950112     C     *IN30         DOWEQ     *OFF
071200950112     C     TBLFLG        IFEQ      ' '
071300950112     C                   MOVEL     TBLUNI        DS1Z
071400950112     C     §1ZUPD        IFEQ      'S'
071500950112     C                   MOVEL     TBLKEY        TSR(C)
071600950112     C                   MOVEL     §1ZDES        DTS(C)
071700950112     C                   MOVEL     §1ZCTR        CTS(C)
071800950112     C                   ADD       1             C
071900950112     C                   ENDIF
072000950112     C                   ENDIF
072100950112     C     KTAB2         READE     TABEL                                  30
072200950112     C                   ENDDO
072300950112     C***
072400950112     C* CARICO MANCATE CONSEGNE DA TASSARE
072500950112     C                   Z-ADD     1             C
072600950112     C                   MOVEL     '8B'          COD
072700950112     C     KTAB2         SETLL     TABEL
072800950112     C     KTAB2         READE     TABEL                                  30
072900950112     C*
073000950112     C     *IN30         DOWEQ     *OFF
073100950112     C     TBLFLG        IFEQ      ' '
073200950112     C                   MOVEL     TBLKEY        CMC(C)
073300950112     C                   MOVE      TBLKEY        WALF3             3
073400950112     C                   MOVE      WALF3         CMC(C)
073500950112     C                   ADD       1             C
073600950112     C                   ENDIF
073700950112     C     KTAB2         READE     TABEL                                  30
073800950112     C                   ENDDO
073900950112     C***
074000950112     C* CARICO I TIPI BOLLA DA NON TASSARE
074100950112     C                   Z-ADD     1             C
074200950112     C                   MOVEL     '8C'          COD
074300950112     C     KTAB2         SETLL     TABEL
074400950112     C     KTAB2         READE     TABEL                                  30
074500950112     C*
074600950112     C     *IN30         DOWEQ     *OFF
074700950112     C     TBLFLG        IFEQ      ' '
074800950112     C                   MOVEL     TBLKEY        CBL(C)
074900950112     C                   MOVE      TBLKEY        WALF3             3
075000950112     C                   MOVE      WALF3         CBL(C)
075100950112     C                   ADD       1             C
075200950112     C                   ENDIF
075300950112     C     KTAB2         READE     TABEL                                  30
075400950112     C                   ENDDO
075500090908      *----
075600090909     C                   Z-ADD     0             C
075700090908     C                   movel(P)  'CTP'         tbeCOD
075800090908     c                   clear                   KCliPar
075900090908     c                   clear                   KtutteBolle
076000090908     C     tbeCOD        setll     tntbe01l
076100090908     C     tbeCOD        reade     tntbe01l
076200090908      * consegne/ritiri
076300090908     c                   dow       not %eof(tntbe01l)
076400090909     C                   ADD       1             C
076500090908     C                   movel     tbeuni        dCTP
076600090908     C                   MOVEL     TBeKE1        KCliPar(C)
076700090908     C     tbeCOD        reade     tntbe01l
076800090908     c                   enddo
076900950112     C***
077000950112     C* DEFINIZIONE CAMPI
077100950112     C     *LIKE         DEFINE    TBLKEY        §KEY
077200950112     C     *LIKE         DEFINE    TBLCOD        §COD
077300950112     C     *LIKE         DEFINE    TBLKUT        §KUT
077400950112     C* AAMMGG DATA IMMESSA A VIDEO
077500950112     C     *LIKE         DEFINE    FTTDDC        W01DDC
077600950127     C     *LIKE         DEFINE    VI4DDC        WM1DDC
077700950112     C* DATA DI ELABORAZIONE (PUO' ESSERECENE SOLO UNA)
077800950112     C     *LIKE         DEFINE    FTTDDC        W02DDC
077900020129     C     *like         define    fttndc        w02ndc
078000011112     C     *like         define    fttdiv        w02div
078100950112     C     *LIKE         DEFINE    FTTNDC        W01NDC
078200950112     C     *LIKE         DEFINE    FTTPDR        W01PDR
078300011105      *
078400950112     C     *LIKE         DEFINE    FTTPDR        W02PDR
078500950112     C     *LIKE         DEFINE    FTTTSR        W01TSR
078600950112     C     *LIKE         DEFINE    FTTSNA        W01SNA
078700020212     C     *LIKE         DEFINE    FTTSNA        g01SNA
078800950112     C     *LIKE         DEFINE    FTTKFA        W01KFA
078900020212     C     *LIKE         DEFINE    FTTKFA        g01KFA
079000950112     C     *LIKE         DEFINE    FTTCLA        W01CLA
079100020212     C     *LIKE         DEFINE    FTTCLA        g01CLA
079200950118     C     *LIKE         DEFINE    FTTCPE        W01CPE
079300020212     C     *LIKE         DEFINE    FTTCPE        g01CPE
079400950112     C     *LIKE         DEFINE    FTTTVL        W01TVL
079500020212     C     *LIKE         DEFINE    FTTTVL        g01TVL
079600950112     C     *LIKE         DEFINE    FTTITA        W01ITA
079700950112     C     *LIKE         DEFINE    FTTITT        W01ITT
079800950112     C     *LIKE         DEFINE    FTTFIT        W01FIT
079900950112     C     *LIKE         DEFINE    FTTFIA        W01FIA
080000950119     C     *LIKE         DEFINE    FTTSNE        W01SNE
080100950112     C     *LIKE         DEFINE    FTTSNP        W01SNP
080200950112     C     *LIKE         DEFINE    FTDNCL        W01NCL
080300950112     C     *LIKE         DEFINE    FTDSTP        W01STP
080400950112     C     *LIKE         DEFINE    FTDSTP        W02STP
080500950112     C     *LIKE         DEFINE    FTDSPC        W01SPC
080600950112     C     *LIKE         DEFINE    FTDSPR        W01SPR
080700950112     C     *LIKE         DEFINE    NRR           W01NRR
080800950112     C     *LIKE         DEFINE    NRR           W04NRR
080900950112     C     *LIKE         DEFINE    NRR           W06NRR
081000950112     C     *LIKE         DEFINE    NRR           NRR4
081100950112     C     *LIKE         DEFINE    NRR           NRR6
081200011105     C     *LIKE         DEFINE    NRR           NRW6
081300950112     C     *LIKE         DEFINE    FGTCTR        W01CTR
081400950112     C     *LIKE         DEFINE    FTDCTP        W01CTP
081500020325     C     *like         define    ftdFIN        w01isola
081600020325     C     *like         define    arbtsp        w01xpres
081700020328     C     *like         define    ftwfgs        kftwfgs
081800020328     C     *like         define    ftwpdr        kftwpdr
081900020328     C     *like         define    ftwtsr        kftwtsr
082000020328     C     *like         define    ftwndc        kftwndc
082100020328     C     *like         define    ftwddc        kftwddc
082200020328     C     *like         define    ftwstp        kftwstp
082300020422     C*
082400020422     C     *like         define    ftdfgs        sv1fgs
082500020422     C     *like         define    ftdpdr        sv1pdr
082600020422     C     *like         define    ftdtsr        sv1tsr
082700020422     C     *like         define    ftdndc        sv1ndc
082800020422     C     *like         define    ftdddc        sv1ddc
082900020328     C*
083000030821     C     *like         define    §t1NSS        W01nss
083100030821     C     *like         define    §t1FW         W01fw
083200030821     C     *like         define    §t1RCC        W01rcc
083300030821     C     *like         define    ftdSTP        savSTP
083400030821     c     *like         define    W01nss        SAVnss
083500030821     c     *like         define    W01fw         SAVfw
083600030821     c     *like         define    W01rcc        SAVrcc
083700030821     C*
083800950112     C                   Z-ADD     0             B                 2 0
083900040112     C                   Z-ADD     0             C                 5 0
084000950112     C                   TIME                    UTIME             6 0
084100040622     C***
084200040622     C                   TIME                    W0140            14 0
084300040622     C                   MOVEl     W0140         Wora              6 0
084400040622     C                   MOVE      W0140         WDAT              8 0
084500040622     C*
084600040622     C                   Z-ADD     Wdat          G02DAT
084700040622     C                   MOVEL     *BLANK        G02ERR
084800040622     C                   CALL      'XSRDA8'
084900040622     C                   PARM                    WLBDAT
085000040622     C* UDATE A 8 IN AAAA/MM/GG
085100040622     C                   move      G02INV        dataoggi
085200090511      **
085300100513     C* scrive il record di allocazione x trasmissione
085400100513     c     parsml        ifeq      *blank
085500100513     C                   clear                   Fiwalc00
085600100513     C                   movel     w0140         walOrc
085700100513     C                   movel     g02inv        waldac
085800100513     C                   eval      WalSIF = KNSIF
085900100513     C                   eval      WalPGM = 'FICNB1R   '
086000100513     C                   eval      WalNOJ = KNMEB
086100100513     C                   eval      WalNUS = KNMUS
086200100513     C                   eval      WalNRJ = KNRJO
086300100513     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
086400100513     C                   write     Fiwalc00
086500100513     c                   feod      Fiwalc1L
086600100513     c                   end
086700090511     C***
086800950112     C                   ENDSR
086900020212     C*---------------------------------------------------------------*
087000931103     C* PULISCO 1 FORMATO --------------------------------------------*
087100020212     C*---------------------------------------------------------------*
087200931103     C     PUL01         BEGSR
087300931103     C                   CLEAR                   NDC
087400931103     C                   CLEAR                   VI1P1
087500931103     C                   CLEAR                   VI1P2
087600931103     C                   CLEAR                   VI1DDC
087700931103     C                   CLEAR                   VI1FVL
087800931103     C                   CLEAR                   VI1TSR
087900020717     C                   Z-ADD     SIMPOU        VI1PF1
088000950112     C                   ENDSR
088100020212     C*---------------------------------------------------------------*
088200900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
088300020212     C*---------------------------------------------------------------*
088400900528     C     CONTR1        BEGSR
088500931103     C                   SETOFF                                       90
088600931103     C                   CLEAR                   W01DDC
088700931103     C                   CLEAR                   W02DDC
088800020129     C                   clear                   w02ndc
088900011112     C                   clear                   w02div
089000931119     C* T I P O   P R E S T A Z I O N E
089100931021    1C     VI1TSR        IFNE      ' '
089200931021    2C     VI1TSR        IFEQ      '?'
089300931015     C                   MOVEL     CODUT         §KUT
089400931015     C                   MOVEL     '1Z'          §COD
089500931015     C                   MOVE      ' '           VI1TSR
089600931015     C                   MOVEL     *BLANKS       §KEY
089700931015     C                   CALL      'X§TABER'
089800931015     C                   PARM                    §KUT
089900931015     C                   PARM                    §COD
090000931015     C                   PARM                    §KEY
090100931015     C                   PARM                    §DES             30
090200931015     C                   MOVEL     §KEY          VI1TSR
090300931015     C                   SETON                                        90
090400931015     C                   GOTO      ENDCT1
090500931021    2C                   ENDIF
090600931018     C*
090700931015     C* 30 ON - INESISTENTE O ANNULLATO
090800931018     C     VI1TSR        LOOKUP    TSR                                    30
090900931018     C  N30              SETON                                        4090
091000931018     C  N30              GOTO      ENDCT1
091100931021    1C                   ENDIF
091200931015     C*
091300950112     C* F I L I A L E   P A D R O N C I N O
091400950112     C     VI1PF1        IFEQ      0
091500950112     C                   SETON                                        4190
091600950119     C                   GOTO      ENDCT1
091700950112     C                   ELSE
091800020717     C* Verifico esistenza filiale padroncino: se 2° LIV. su £6, se 1° LIV.
091900950112     C* su £1
092000020717     C                   IF        SIMTPP = '2'
092100950112     C     VI1PF1        LOOKUP    L6                                     30
092200950112     C  N30              SETON                                        4190
092300950119     C  N30              GOTO      ENDCT1
092400950112     C                   ELSE
092500950112     C     VI1PF1        LOOKUP    L1                                     30
092600950112     C  N30              SETON                                        4190
092700950119     C  N30              GOTO      ENDCT1
092800950112     C                   END
092900950112     C                   END
093000931018     C*
093100931018     C* P A D R O N C I N O   DAL AL
093200931021    1C     VI1PD2        IFGT      0
093300931119     C     VI1PD2        ANDNE     VI1PD1
093400931119     C*
093500931021    2C     VI1PD2        IFLT      VI1PD1
093600931015     C                   SETON                                        4290
093700090508     C                   GOTO      ENDCT1
093800931021    2C                   ENDIF
093900931015     C*
094000931021   X1C                   ELSE
094100931018     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
094200931021    2C     VI1PD1        IFGT      0
094300931110     C*
094400021202     C     kapdv         CHAIN     fiapd01L                           30
094500950123     C     *IN30         IFEQ      *ON
094600931015     C                   SETON                                        30
094700931021    3C                   ENDIF
094800931015     C*
094900931015     C   30              SETON                                        4390
095000931015     C   30              GOTO      ENDCT1
095100931021    2C                   ENDIF
095200931021    1C                   ENDIF
095300931018     C*
095400931018     C* D A T A   D I S T I N T A
095500931021    1C     VI1DDC        IFGT      0
095600950112     C                   Z-ADD     VI1DDC        G02DAT
095700931018     C                   MOVEL     *BLANK        G02ERR
095800950112     C                   CALL      'XSRDA8'
095900931018     C                   PARM                    WLBDAT
096000931021    2C     G02ERR        IFEQ      '1'
096100931018     C                   SETON                                        44  90
096200931018     C                   GOTO      ENDCT1
096300931021    2C                   END
096400931018     C                   Z-ADD     G02INV        W01DDC
096500931026    1C                   ENDIF
096600950113     C* SE IMMESSO UN SOLO NUMERO DISTINTA IMMETTERE ANCHE IL TIPO SERV
096700931018     C                   SORTA     NDC
096800931026    1C     NDC(10)       IFGT      0
096900931018     C     NDC(9)        ANDEQ     0
097000931018     C     VI1TSR        ANDEQ     ' '
097100931018     C                   SETON                                        45  90
097200931018     C                   GOTO      ENDCT1
097300931026    1C                   ENDIF
097400910320     C*
097500950112     C  N90              MOVEL     VI1PF1        VI1PF2
097600950112     C*
097700900614     C     ENDCT1        ENDSR
097800020212     C*---------------------------------------------------------------*
097900931018     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
098000020212     C*---------------------------------------------------------------*
098100931018     C     CARNDC        BEGSR
098200950113     C                   SETOFF                                       0102
098300931018     C                   SETOFF                                       040531
098400931105     C                   SETOFF                                       21
098500931018     C                   Z-ADD     0             NRR               5 0
098600931018     C                   Z-ADD     0             W01NRR
098700931018     C                   CLEAR                   VI1VAL
098800931119     C                   CLEAR                   WDATA
098900931018     C* PULIZIA SUBFILE
099000931018     C                   SETON                                        35
099100931018     C                   WRITE     FRB8CT2
099200931018     C                   SETOFF                                       35
099300931018     C*
099400931018     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
099500931018     C* 02 ON - IMMESSI PADRONCINI DAL AL
099600931108     C* 04 ON - IMMESSO TIPO PRESTAZIONE
099700950113     C* 05 ON - IMMESSO SOLO UN NUMERO DISTINTA (CONSEGNE)
099800931018     C*
099900931108     C* TIPO PRESTAZ SELEZIONATO
100000931018     C     VI1TSR        IFNE      ' '
100100931018     C                   SETON                                        04
100200931018     C                   ENDIF
100300931018     C* PADRONCINO
100400931018     C     VI1PD2        IFGT      0
100500931018     C     VI1P2         ANDNE     VI1P1
100600931018     C                   SETON                                        02
100700931018     C                   ELSE
100800931018     C*
100900931018     C     VI1PD1        IFGT      0
101000931018     C                   SETON                                        01
101100950113     C* IMMESSO SOLO 1 NUMERO DISTINTA E TIPO SERIVZIO = 'C'
101200931018     C     NDC(10)       IFGT      0
101300931018     C     NDC(9)        ANDEQ     0
101400950113     C     VI1TSR        ANDEQ     'C'
101500931018     C                   SETON                                        05
101600931018     C                   MOVEL     NDC(10)       W01NDC
101700931018     C                   ENDIF
101800931018     C*
101900931018     C                   ENDIF
102000931018     C                   ENDIF
102100931018     C                   Z-ADD     VI1P1         W01PDR
102200931018     C* NUMERI DISTINTA
102300931018     C                   XFOOT     NDC           WNUM9             9 0
102400931018     C* FLAG VALORIZZAZIONE
102500931018     C     VI1FVL        IFEQ      ' '
102600931018     C                   MOVEL     CDAVAL        VI1VAL
102700931018     C                   ELSE
102800931018     C                   MOVEL     CVALOR        VI1VAL
102900931018     C                   ENDIF
103000921229     C**
103100931018     C* POSIZIONAMENTO PER LETTURA
103200931018     C**
103300931018     C                   SELECT
103400060118      **
103500931108     C** NON IMMESSO TIPO PRESTAZ: LEGGO SOLO PER PADRONCINO
103600931018     C     *IN04         WHENEQ    *OFF
103700931018     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
103800931018     C     VI1PD1        IFGT      0
103900060118     C     k21pdr        SETLL     fiftt21l
104000931018     C                   ELSE
104100931018     C                   EXSR      IMPO1
104200931018     C                   ENDIF
104300931018     C*
104400931018     C                   OTHER
104500950113     C** 01 ON  - LEGGO PER PADRONCINO/TIPO SERVIZIO/NUMERO/DATA DISTIN
104600950113     C**              O PER PADRONCINO/TIPO SERVIZIO
104700950113     C** 01 OFF - LEGGO PER PADRONCINO (TUTTI O NEL DAL-AL)/TIPO SERVIZ
104800060118      **
104900931018     C     *IN01         IFEQ      *ON
105000060118     C   05K21pdr1       CHAIN(N)  fiftt21l                           31
105100060118     C  N05K21pdr3       SETLL     fiftt21l
105200060118      **
105300931018     C                   ELSE
105400950113     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
105500950113     C     VI1PD1        IFGT      0
105600060118     C     k21PDR        SETLL     fiftt21l
105700950113     C                   ELSE
105800931018     C                   EXSR      IMPO1
105900931018     C                   ENDIF
106000060118      **
106100950113     C                   ENDIF
106200060118      **
106300931018     C                   ENDSL
106400931018     C*
106500931018     C* LETTURA DISTINTE/RITIRI
106600931018     C     *IN31         IFEQ      *OFF
106700931018     C                   EXSR      LEGNDC
106800931018     C                   ELSE
106900931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
107000931018     C                   SETON                                        4690
107100931119     C                   MOVEL     ERR(1)        VI1ERR
107200030519      * solo in simulazione: (eccezione)
107300030519      *  se non c'è la tariffa non carica le distinte
107400030519      *  quindi non è valido il messaggio precedente
107500030519     c     parsml        ifeq      'S'
107600030519     c     §t3ctr        andeq     '???'
107700030519     C                   clear                   VI1ERR
107800030519     c     §t3tsr        ifeq      'C'
107900030519     C                   MOVEL     ERR(2)        VI1ERR
108000030519     c                   end
108100030519     c     §t3tsr        ifeq      'R'
108200030519     C                   MOVEL     ERR(3)        VI1ERR
108300030519     c                   end
108400030519     c                   end
108500931018     C                   ENDIF
108600931018     C*
108700900608     C                   ENDSR
108800020212     C*---------------------------------------------------------------*
108900931018     C* IMPOSTAZIONE LETTURA PER 04 OFF ------------------------------*
109000020212     C*---------------------------------------------------------------*
109100931018     C     IMPO1         BEGSR
109200060118      **
109300060118     C     k21PDR        setgt     fiftt21l
109400060118     C                   read(N)   fiftt21l                               31
109500950202     C*
109600950202     C     *IN31         IFEQ      *OFF
109700950202     C                   MOVEL     FTTPDR        W03               3 0
109800950202     C     VI1PF1        IFNE      W03
109900950202     C                   SETON                                        31
110000950202     C                   END
110100950202     C                   END
110200931018     C*
110300931018     C     *IN31         IFEQ      *OFF
110400931018     C                   Z-ADD     FTTPDR        W01PDR
110500060118     C  N04k21pdr        setll     fiftt21l
110600060118     C   04K21pdr3       setll     fiftt21l
110700931018     C                   ENDIF
110800060118      **
110900931018     C                   ENDSR
111000020212     C*---------------------------------------------------------------*
111100931018     C* LETTURA DITINTE DA CARICARE ----------------------------------*
111200020212     C*---------------------------------------------------------------*
111300931018     C     LEGNDC        BEGSR
111400931018     C                   CLEAR                   W01TSR
111500931018     C                   CLEAR                   W02PDR
111600931018     C* L E T T U R A
111700940325    1C     *IN31         DOWEQ     *OFF
111800931018     C                   SETOFF                                       9032
111900931018     C                   SELECT
112000931018     C** LEGGO PER PADRONCINO (TUTTO O NEL DAL-AL)
112100940325    2C     *IN04         WHENEQ    *OFF
112200931018     C*
112300940325    3C     *IN32         DOWEQ     *OFF
112400060118     C     k21pdr        READE(N)  fiftt21l                               31
112500931018     C*
112600940325    4C     *IN31         IFEQ      *ON
112700931018     C     *IN01         ANDEQ     *OFF
112800931018     C                   EXSR      IMPO1
112900931018     C   31              SETON                                        32
113000931018     C                   ELSE
113100931018     C                   SETON                                        32
113200940325    4C                   ENDIF
113300940325    3C                   ENDDO
113400931108     C** LEGGO TUTTI I PADRONCINI(O NEL DAL-AL) E TIPO PRESTAZIONE
113500940325    2C     *IN01         WHENEQ    *OFF
113600931018     C*
113700940325    3C     *IN32         DOWEQ     *OFF
113800060118     C     K21pdr3       READE(N)  fiftt21l                               31
113900931018     C*
114000940325    4C     *IN31         IFEQ      *ON
114100931018     C                   EXSR      IMPO1
114200931018     C   31              SETON                                        32
114300931018     C                   ELSE
114400931018     C                   SETON                                        32
114500940325    4C                   ENDIF
114600940325    3C                   ENDDO
114700060118      *
114800931108     C** LEGGO PER PADRONCINO TIPO PRESTAZIONE
114900940325    2C     *IN01         WHENEQ    *ON
115000931018     C     *IN05         ANDEQ     *OFF
115100060118     C     K21pdr3       READE(N)  fiftt21l                               31
115200931018     C*
115300931018     C                   ENDSL
115400931018     C*
115500931018     C* C O N F R O N T O   L  E   S E L E Z I O N I
115600020606     C     *IN31         IFEQ      *OFF
115700020606     C                   EXSR      CONFR
115800931018     C*
115900931018     C* C A R I C A M E N T O   S F L
116000900608     C* 90 ON - RECORD DA NON SELEZIONARE
116100020606     C     *IN90         ifeq      *OFF
116200931018     C*
116300931018     C                   CLEAR                   VI2SCE
116400011105     C                   clear                   vh2div
116500931028     C                   CLEAR                   VH2ITT
116600931028     C                   CLEAR                   VH2ITA
116700931018     C*
116800931018     C     FTTPDR        IFNE      W02PDR
116900020524     C                   CLEAR                   saltasf2          1
117000931103     C                   SETOFF                                       12
117100931103     C                   MOVEL     '0'           VH2I12
117200021202     C     kapdf         CHAIN     fiapd01L                           30
117300950117     C     *IN30         IFEQ      *OFF
117400100223     C                   MOVEL     APDRSf        VI2DPD
117500950117     C                   MOVEL     APDETM        VH2ETM
117600950117     C                   MOVEL     APDETP        VH2ETP
117700950117     C                   MOVEL     APDPKM        VH2PKM
117800950117     C                   MOVEL     APDPKP        VH2PKP
117900021211     C                   eval      vh2cam = apdCAM
118000021211     C                   eval      vh2cap = apdCAP
118100021211     C                   eval      vh2scm = apdSCM
118200021211     C                   eval      vh2scp = apdSCP
118300021211     C                   z-add     apdHAR        vh2HAR
118400021211     C                   z-add     apdHPA        vh2HPA
118500020524     C                   movel     APDpdd        saltasf2
118600950117     C                   ELSE
118700950117     C                   CLEAR                   VI2DPD
118800950117     C                   CLEAR                   VH2ETM
118900950117     C                   CLEAR                   VH2ETP
119000950117     C                   CLEAR                   VH2PKM
119100950117     C                   CLEAR                   VH2PKP
119200021211     C                   clear                   vh2CAM
119300021211     C                   clear                   vh2CAP
119400021211     C                   clear                   vh2SCM
119500021211     C                   clear                   vh2HAR
119600021211     C                   clear                   vh2HPA
119700021211     C                   clear                   vh2SCP
119800950117     C                   END
119900931018     C                   MOVEL     FTTPDR        VI2PDR
120000931018     C                   MOVEL     FTTPDR        W02PDR
120100931018     C                   ELSE
120200931103     C                   SETON                                        12
120300931103     C                   MOVEL     '1'           VH2I12
120400931018     C                   ENDIF
120500931108     C* TIPO PRESTAZIONE
120600931018     C     FTTTSR        IFNE      W01TSR
120700931018     C                   MOVEL     FTTTSR        K17(1)
120800931018     C                   MOVEL     '-'           K17(2)
120900931018     C                   Z-ADD     1             C
121000931018     C     FTTTSR        LOOKUP    TSR(C)                                 30
121100931018     C   30              MOVEA     DTS(C)        K17(3)
121200931018     C  N30              MOVEA     *BLANKS       K17(3)
121300931018     C*
121400931018     C                   MOVEA     K17           VI2TSR
121500931018     C                   MOVEL     FTTTSR        W01TSR
121600931018     C                   ENDIF
121700931018     C*
121800931119     C                   MOVEL     FTTNDC        VI2NDC                          NUMERO
121900950113     C                   MOVE      FTTDDC        DATA
122000950113     C                   MOVE      FTTDDC        VI2DDC                         DATA
122100931119     C                   MOVE      AA            VI2DDC
122200950113     C                   MOVEL     GG            VI2DDC
122300931119     C                   MOVEL     FTTDDC        VH2DDC
122400931027     C     FTTFIT        IFNE      ' '
122500011105     C                   Z-ADD     FTTITT        VH2ITT
122600931027     C                   ENDIF
122700931027     C     FTTFIA        IFNE      ' '
122800931027     C                   Z-ADD     FTTITA        VH2ITA
122900931027     C                   ENDIF
123000011105     C                   movel     fttdiv        vh2div
123100950117     C                   MOVEL     FTTFPP        VH2FPP
123200950117     C                   MOVEL     FTTPEP        VH2PEP
123300021211     C                   eval      vh2FCS = fttLVL
123400021217     C                   z-add     fttTBS        vh2TBS
123500900608     C*
123600020603     C*  Prova a cercare se in simulazione la tariffa legata al documento
123700020603     c     parsml        ifeq      'S'
123800030516     C*
123900020603     C* Routine esterna per restituire il codice tariffa del documento
124000020603     C                   clear                   ficnb3
124100030516     C*
124200030516     C* Attenzione se ci sono dei Ritiri Annullati, deve impostare il parametro
124300030516     C*  RAN con (S)
124400030516     c                   if        ftttsr ='R' and ftttbs > 0
124500030516     C                   movel     'S'           §t3RAN
124600030516     c                   end
124700020603     C                   movel     fttpdr        §t3pdr
124800020603     C                   movel     parsml        §t3sml
124900020603     C                   movel     ftttsr        §t3tsr
125000020603     C                   z-add     fttndc        §t3ndc
125100020603     C                   z-add     fttddc        §t3ddc
125200020603     C                   movel     fttdiv        §t3div
125300020603     C                   movel     *blank        §t3ctr
125400020605     C                   movel     vi1fvl        §t3fvl
125500020603     C                   CALL      'FICNB3R1'
125600020603     C                   PARM                    ficnb3
125700020603      *
125800020603     c                   endif
125900020603     C*
126000020524     C* se il padroncino è stato escluso dalla valorizzazione
126100020524     C* comunque in simulazione deve sempre permettere la valorizzazione
126200020603     c                   move      'S'           nowrite           1
126300020603      *
126400020603     c     saltasf2      ifeq      'S'
126500020603     c     parsml        andeq     *blank
126600020603      *
126700020524     c     parsml        oreq      'S'
126800020603     c     §t3ctr        andeq     '???'
126900020603     c     saltasf2      andeq     'S'
127000020603     c                   move      'N'           nowrite
127100020603     c                   endif
127200020524      *
127300020524     C* questo da quando si è voluto attivare le simulazioni per quei
127400020524     C* padroncini che non devono avere l'autofatturazione ma che occorre
127500020524     C* simulare comunque.
127600020603     c                   if        nowrite ='S'
127700020606     C*
127800020606     C* CONTROLLO QUANTE DATE CARICO
127900020606    1C     W02DDC        IFEQ      0
128000020606     C                   Z-ADD     FTTDDC        W02DDC
128100020606     C                   z-add     fttndc        w02ndc
128200020606     C                   movel     fttdiv        w02div
128300020606   X1C                   ELSE
128400020606     C* PRESENTI ALMENO 2 DATE
128500020606    2C     FTTDDC        IFNE      W02DDC
128600020606     C                   MOVEL     '1'           WDATA             1
128700020606    2C                   ENDIF
128800020606    1C                   ENDIF
128900020606     C*
129000900608     C                   ADD       1             NRR
129100931018     C                   WRITE     FRB8SF2
129200020603     c                   endif
129300020524      *
129400931018     C                   ENDIF
129500020606      *
129600020606     C                   ENDIF
129700940325     C*
129800940325     C* ESSENDO CHAIN SECCA, DOPO AVER LETTO UNA VOLTA ESCO
129900940325     C   01
130000940325     CAN 05              SETON                                        31
130100931018     C                   ENDDO
130200900521     C*
130300931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
130400931119     C     NRR           IFEQ      0
130500931018     C                   SETON                                        4690
130600931119     C                   MOVEL     ERR(1)        VI1ERR
130700030519      * solo in simulazione: (eccezione)
130800030519      *  se non c'è la tariffa non carica le distinte
130900030519      *  quindi non è valido il messaggio precedente
131000030519     c     parsml        ifeq      'S'
131100030519     c     §t3ctr        andeq     '???'
131200030519     C                   clear                   VI1ERR
131300030519     c     §t3tsr        ifeq      'C'
131400030519     C                   MOVEL     ERR(2)        VI1ERR
131500030519     c                   end
131600030519     c     §t3tsr        ifeq      'R'
131700030519     C                   MOVEL     ERR(3)        VI1ERR
131800030519     c                   end
131900030519     c                   end
132000931018     C                   ELSE
132100931018     C                   Z-ADD     NRR           W01NRR
132200931119     C                   SETOFF                                       90
132300931018     C                   ENDIF
132400900509     C*
132500900518     C                   ENDSR
132600020212     C*---------------------------------------------------------------*
132700931018     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
132800020212     C*---------------------------------------------------------------*
132900931018     C     CONFR         BEGSR
133000931108     C* TIPO PRESTAZIONE
133100931105     C   04FTTTSR        IFNE      VI1TSR
133200931105     C                   SETON                                        90
133300931105     C                   GOTO      ENDCON
133400931105     C                   ENDIF
133500931105     C* FLAG VALORIZZAZIONE
133600931105     C     FTTFVL        IFNE      VI1FVL
133700931105     C                   SETON                                        90
133800931105     C                   GOTO      ENDCON
133900931105     C                   ENDIF
134000931018     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
134100931018     C   02FTTPDR        IFGT      VI1P2
134200931018     C                   SETON                                        3190
134300931018     C                   GOTO      ENDCON
134400931018     C                   ENDIF
134500931018     C* DATA DISTINTA
134600931018     C     VI1DDC        IFGT      0
134700931018     C     FTTDDC        ANDGT     W01DDC
134800931018     C                   SETON                                        90
134900931018     C                   GOTO      ENDCON
135000931018     C                   ENDIF
135100931028     C*
135200931028     C* PGM RICHIAMATO: CONTROLLO DATA DISTINTA
135300931028     C   20FTTDDC        IFNE      PARDDC
135400931028     C                   SETON                                        90
135500931028     C                   GOTO      ENDCON
135600931028     C                   ENDIF
135700950113     C* NUMERI DISTINTA O RITIRO E SCELTO UN SOLO NUMERO DISTINTA
135800931018     C     WNUM9         IFGT      0
135900950113     C     NDC(10)       ORGT      0
136000950113     C     NDC(9)        ANDEQ     0
136100950113     C     VI1TSR        ANDEQ     'R'
136200931018     C     FTTNDC        LOOKUP    NDC                                    30
136300931018     C  N30              SETON                                        90
136400931018     C  N30              GOTO      ENDCON
136500931018     C                   ENDIF
136600931018     C*
136700931018     C     ENDCON        ENDSR
136800020212     C*---------------------------------------------------------------*
136900931018     C* SELEZIONE DISTINTE DA VALORIZZARE ----------------------------*
137000020212     C*---------------------------------------------------------------*
137100931018     C     SELNDC        BEGSR
137200931105     C                   SETOFF                                       90
137300931018     C                   Z-ADD     1             NRR
137400931018     C                   Z-ADD     0             NRR4
137500931018     C                   Z-ADD     0             W04NRR
137600931018     C                   CLEAR                   W02PDR
137700931018     C                   CLEAR                   VI4SCE
137800931119     C* PRESENTI PIU' DATE
137900931119    1C     WDATA         IFNE      ' '
138000931119     C* SE CMD1 ERRORE
138100931119    2C     *INKA         IFEQ      *ON
138200931119     C                   Z-ADD     1             REC2
138300931119     C                   SETON                                        4090
138400931119     C                   GOTO      ENDSEL
138500931119     C*
138600931119   X2C                   ELSE
138700931119     C*
138800931119     C* SE ENTER CONTROLLO LE SELEZIONI FATTE
138900931123     C                   CLEAR                   W02DDC
139000020129     C                   clear                   w02ndc
139100011112     C                   clear                   w02div
139200931119     C                   SETON                                        36
139300931119     C                   READC     FRB8SF2                                30
139400931119     C*
139500931119    3C     *IN30         DOWEQ     *OFF
139600931122     C*
139700931122    4C     VI2SCE        IFEQ      '1'
139800931122    5C     W02DDC        IFEQ      0
139900931122     C                   MOVEL     VH2DDC        W02DDC
140000020129     C                   z-add     vi2ndc        w02ndc
140100011112     C                   movel     vh2div        w02div
140200931122   X5C                   ELSE
140300931122    6C     VH2DDC        IFNE      W02DDC
140400931119     C                   Z-ADD     NRR           REC2
140500931119     C                   SETON                                        409030
140600931122    6C                   ENDIF
140700931122    5C                   ENDIF
140800931122    4C                   ENDIF
140900931119     C*
141000931119    4C     VH2I12        IFEQ      '1'
141100931119     C                   SETON                                        12
141200931119   X4C                   ELSE
141300931119     C                   SETOFF                                       12
141400931119    4C                   ENDIF
141500931119     C*
141600931119     C                   UPDATE    FRB8SF2
141700931119     C*
141800931119     C  N30              READC     FRB8SF2                                30
141900931119    3C                   ENDDO
142000931119     C*
142100931119     C                   SETOFF                                       36
142200931119     C   90              GOTO      ENDSEL
142300931119    2C                   ENDIF
142400931119    1C                   ENDIF
142500931018     C* PULIZIA SUBFILE
142600931018     C                   SETON                                        35
142700931018     C                   WRITE     FRB8CT4
142800931018     C                   SETOFF                                       35
142900931119     C*
143000931119     C                   Z-ADD     1             NRR
143100950131     C* Indicatore protezione codice tariffa e Pik/Etich. S/N per
143200950131     C* distinte gia' valorizzate
143300950131     C     VI1FVL        COMP      'V'                                    24
143400931018     C*
143500931028    1C     NRR           DOWLE     W01NRR
143600931103     C*
143700931111     C   KANRR           CHAIN     FRB8SF2                            31
143800931111     C  NKA              READC     FRB8SF2                                31
143900931018     C*
144000931018     C* SE ENTER --> SOLO QUELLI CON L'"1"
144100931111    2C  N31*INKA         IFEQ      *OFF
144200931103     C     VI2SCE        ANDEQ     '1'
144300931018     C* SE CMD1 --> TUTTI
144400931018     C     *INKA         OREQ      *ON
144500931018     C*
144600931018     C* C O D I C E   P A D R O N C I N O
144700931028    3C     VI2PDR        IFNE      W02PDR
144800931018     C*
144900931018     C                   MOVEL     VI2PDR        W02PDR                          SALVATAGGIO
145000931019     C                   MOVEL     VI2PDR        VI4PDR                          VIDEO COD
145100931019     C                   MOVEL     VI2DPD        VI4DPD                          VIDEO DES
145200931018     C                   MOVEL     VI2DPD        VH4DPD                          HIDDEN X SLF6
145300931018     C*
145400931108     C* 11 OFF - EMETTO PADRONCINO E TIPO PRESTAZ A VIDEO
145500931019     C                   SETOFF                                       11
145600931028    3C                   ENDIF
145700931018     C*
145800931108     C* T I P O   P R E S T A Z I O N E
145900931028    3C     *IN11         IFEQ      *OFF
146000931019     C     VI2TSR        ORNE      W17TSR
146100931019     C                   SETOFF                                       11
146200021211     C                   MOVEL     *blank        vi4TCS
146300931018     C                   MOVEL     VI2TSR        W17TSR           17             CONFRONTO
146400931018     C                   MOVEL     VI2TSR        VI4TSR                          VIDEO
146500950120     C                   MOVEL     VI2TSR        VH4TSH                         T.SERV.X PRINT
146600931019     C                   MOVEL     VI2TSR        VH4TSR                          HIDDEN
146700950119     C                   MOVEL     VI2TSR        VH4TSD                          HIDDEN
146800950119     C                   MOVEL     VI2DPD        VI4DPD
146900090508     C                   MOVEL     VI2DPD        VH4DPD
147000090508      *
147100931123     C* RICERCA CODICE TARIFFA PER PADRONCINO/TIPO PRESTAZ
147200011112     C                   clear                   vi4ctd
147300931021     C                   EXSR      RICTAR
147400040728     c                   move      tar_poste     vh4TPO
147500090507     C*
147600931018     C                   CLEAR                   VI4NDC
147700931018     C                   CLEAR                   VI4DDC
147800950116     C                   CLEAR                   VH4DDC
147900931019     C                   CLEAR                   VH4NRR
148000931027     C                   CLEAR                   VI4ITT
148100931027     C                   CLEAR                   VI4ITA
148200011106     C                   z-add     1             vh4tpr
148300931018     C                   ADD       1             NRR4
148400011109     c     vh4tpr        comp      2                                      26
148500931018     C                   WRITE     FRB8SF4
148600931019     C                   SETON                                        11
148700000703     C                   SETOFF                                       36
148800931019     C                   Z-ADD     NRR4          VH4NRR
148900931028    3C                   ENDIF
149000931018     C*
149100950117     C* PRESTAZIONE PADRONCINO M o P
149200950119     C     VH2FPP        IFEQ      'M'
149300950117     C                   MOVEL     COS1          VI4DPD
149400950119     C                   ELSE
149500950119     C     VH2FPP        IFEQ      'P'
149600950119     C                   MOVEL     COS2          VI4DPD
149700950119     C                   END
149800950119     C                   END
149900021211      *
150000950117     C                   MOVE      VH2FPP        VH4FPP
150100021211      *
150200950117     C* PAGAMENTO TARIFFA ETICH/PIKING
150300950117     C                   SELECT
150400021211      *
150500950117     C* Se Consegna memorizza flag piking da anagrafico padroncino
150600021211      *  e memorizza anche carico/scarico
150700950117     C     VH4TSR        WHENEQ    'C'
150800021211      * Carico
150900950119     C                   MOVEL     COS3          VI4TSR
151000021211     C                   MOVEL     cos5          vi4TCS
151100950117     C     VH4FPP        IFEQ      'M'
151200950127     C                   MOVEL     VH2PKM        VH4PEA
151300021211     c                   eval      vh4ACS = vh2CAM
151400950117     C                   ELSE
151500950127     C                   MOVEL     VH2PKP        VH4PEA
151600021211     c                   eval      vh4ACS = vh2CAP
151700950117     C                   END
151800021211      *
151900950117     C* Se Ritiro memorizza flag etichettatura da anagrafico
152000950117     C     VH4TSR        WHENEQ    'R'
152100021211      * Scarico
152200950119     C                   MOVEL     COS4          VI4TSR
152300021211     C                   MOVEL     cos6          vi4TCS
152400950117     C     VH4FPP        IFEQ      'M'
152500950127     C                   MOVEL     VH2ETM        VH4PEA
152600021211     c                   eval      vh4ACS = vh2SCM
152700950117     C                   ELSE
152800950127     C                   MOVEL     VH2ETP        VH4PEA
152900021211     c                   eval      vh4ACS = vh2SCP
153000950117     C                   END
153100950117     C                   ENDSL
153200950127     C*
153300950127     C                   MOVEL     VH2PEP        VI4PEP
153400021211     c                   eval      vi4FCS = vh2FCS
153500021211      *
153600950127     C     VI4PEP        IFEQ      *BLANKS
153700950127     C                   MOVEL     VH4PEA        VI4PEP
153800950127     C                   END
153900021211      *
154000021211     C     vi4FCS        IFEQ      *BLANKS
154100021211     C                   eval      vi4FCS = vh4ACS
154200021211     C                   END
154300021211      *
154400931018     C* DISTINTA: NUMERO E DATA
154500931018     C                   MOVEL     VI2NDC        VI4NDC
154600931018     C                   MOVEL     VI2DDC        VI4DDC
154700950116     C                   MOVEL     VH2DDC        VH4DDC
154800011105     C                   MOVEL     VH2ITT        VI4ITT
154900931027     C                   MOVEL     VH2ITA        VI4ITA
155000021217     C                   z-add     vh2TBS        vh4TBS
155100030127      * espone se ci sono dei ritiri annullati
155200030127     C                   z-add     vh2TBS        vi4ran
155300011105     C                   CLEAR                   vi4ctd
155400011105     C                   MOVEL     vh2div        vi4ctd
155500011106     C                   z-add     2             vh4tpr
155600931018     C                   ADD       1             NRR4
155700011109     c     vh4tpr        comp      2                                      26
155800931018     C                   WRITE     FRB8SF4
155900931103     C*
156000931103     C* PULISCO IL CAMPO DI SCELTA
156100931103     C                   MOVEL     ' '           VI2SCE
156200931103     C     VH2I12        IFEQ      '0'
156300931103     C                   SETOFF                                       12
156400931103     C                   ELSE
156500931103     C                   SETON                                        12
156600931103     C                   ENDIF
156700931103     C*
156800931103     C                   UPDATE    FRB8SF2
156900931028    2C                   ENDIF
157000931103     C*
157100931103     C* FINE LETTURA SE NO CMD1
157200931111     C  NKA*IN31         IFEQ      *ON
157300931103     C                   Z-ADD     W01NRR        NRR
157400931103     C                   ENDIF
157500931018     C*
157600931018     C                   ADD       1             NRR
157700931028    1C                   ENDDO
157800931018     C*
157900931018     C* NON EFFETTUATA NESSUNA SELEZIONE--> RIEMETTO SFL 2
158000931028    1C     NRR4          IFEQ      0
158100931018     C                   SETON                                        90
158200931018     C                   ELSE
158300931018     C                   Z-ADD     NRR4          W04NRR
158400931028    1C                   ENDIF
158500931018     C*
158600931119     C     ENDSEL        ENDSR
158700020212     C*---------------------------------------------------------------*
158800931108     C* RICERCO TARIFFA PREFERENZIALE PER TIPO PRESTAZ/PADRONCINO ----*
158900020212     C*---------------------------------------------------------------*
159000931018     C     RICTAR        BEGSR
159100000629     C*
159200000629     C                   SETOFF                                       5051
159300000703     C                   SETON                                        36
159400000622     C* RICHIAMO LA ROUTINE CHE VERIFICA SE LE BOLLE CHE SONO IN DISTINTA SONO TUTTE BARTOLINI
159500000622     C* OPPURE TUTTE POSTE OPPURE MISTE PER FARE LA PROPOSTA DELLA TARIFFA GIUSTA
159600000622     C                   EXSR      RICBOL
159700931104     C* SE LA SOMMA PESI NON C'E' VIENE ASSUNTO SPEDIZIONE
159800931104     C                   MOVEL     'S'           VH4TSM
159900931028     C                   CLEAR                   WTARV
160000000622     C* CAMPI COMODO TARIFFE  POSTE
160100000622     C                   CLEAR                   PTARV
160200040825     C                   clear                   Ptarv_rap
160300040825     C                   clear                   Ptarv_rct
160400000622     C*
160500011112     C     KFGT          CHAIN     FIFGT000                           30
160600931018     C*
160700931021    1C     *IN30         DOWEQ     *OFF
160800931018     C*
160900011112     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
161000011112     C*  e quelle diverse dalla divisa del documento
161100020128      ****   e quelle senza data di stampa (quindi non convalidate)
161200020315      ****   ma solo se non in simulazione.
161300020315      ****   se in simulazione non deve controllare la data di stampa.
161400091112    2C     FGTATB        IFEQ      ' '
161500091112     C     w02div        andeq     fgtdiv
161600091112     C*
161700091112     C* Se tariffa del periodo
161800091112     C     Ktgt          CHAIN     FItgt01l
161900091112 2/3 c                   if        %Found(FItgt01l) and
162000091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
162100091112     **
162200091112    3C     parsml        IFeq      *blank
162300091112     C     tgtDTS        aNDgt     0
162400091112     C     parsml        OReq      'S'
162500011112      *
162600931108     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
162700931105     C                   Z-ADD     1             C
162800931105     C     FGTTSR        LOOKUP    TSR(C)                                 32
162900091112    4C     FGTCTR        IFNE      CTS(C)
163000931018     C* TARIFFA VALIDA
163100931021     C*
163200931021     C* EMETTO LA PREFERENZIALE SE C'E'
163300931021    5C     FGTFTP        IFEQ      'P'
163400931021     C                   MOVEL     FGTCTR        WTARV             3
163500931021     C                   SETON                                        30
163600931021     C*
163700931021   X5C                   ELSE
163800931021     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
163900931022    6C     WTARV         IFEQ      *BLANKS
164000000622     C     FGTFTP        ANDNE     'O'
164100091112     C     WTARV         orne      *blank
164200040628     C     fgtFTP        ANDNE     'O'
164300091112     C     fgtCTR        andeq     sav_FTDCTB
164400091112     C     sav_FlgCTB    andeq     'S'
164500931022     C                   MOVEL     FGTCTR        WTARV
164600931022    6C                   ENDIF
164700091112      *
164800000622    6C     PTARV         IFEQ      *BLANKS
164900000622     C     FGTFTP        ANDEQ     'O'
165000091112     C     ptarv         Orne      *BLANKS
165100091112     C     fgtCTR        andeq     sav_FTDCTP
165200091112     C     sav_FlgCTP    andeq     'S'
165300040628     C     FGTFTP        ANDEQ     'O'
165400000622     C                   MOVEL     FGTCTR        PTARV             3
165500040825     C                   z-add     fgtrap        Ptarv_rap
165600040825     C                   z-add     fgtrct        Ptarv_rct
165700000622    6C                   ENDIF
165800091112      *
165900931022    5C                   ENDIF
166000091112     C*
166100091112    4C                   ENDIF
166200091112     C*
166300091112    3C                   ENDIF
166400091112 2/3 C                   ENDIF
166500091112    2C                   ENDIF
166600931018     C*
166700011112     C  N30KFGT          READE     FIFGT000                               30
166800931021    1C                   ENDDO
166900931021     C*
167000000714     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
167100000622    0C     VH4BAR        IFEQ      'S'
167200931028    1C     WTARV         IFNE      *BLANKS
167300011030     C                   MOVEL     WTARV         VI4CTD                          VALIDA
167400000703     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
167500011030     C                   MOVEL     VI4CTD        VH4CTB
167600931028    1C                   ENDIF
167700000622    0C                   ENDIF
167800000622     C*
167900000714     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
168000000622    0C     VH4POS        IFEQ      'S'
168100040825      *
168200040825      *  Deve controllare se però c'è una tariffa di riferimento poste che non esiste +
168300091112      **** NON DEVE ESSERE PIU' utilizzata poichè comunque manca il progressivo in chiave
168400040825     C                   if        Ptarv_rap <> 0 and Ptarv_rct <> 0
168500040825     c                   exsr      Ric_Tar_RIF
168600040825     c                   end
168700040825      *
168800000622    1C     PTARV         IFNE      *BLANKS
168900011030     C                   MOVEL     PTARV         VI4CTD                          VALIDA
169000000703     C* SALVO LA TARIFFA POSTE     PER IL DETTAGLIO SPEDIZIONE
169100011030     C                   MOVEL     VI4CTD        VH4CTP
169200040726     c                   else
169300040726     **
169400040726     ** se non ci sono tariffe poste, dalla data specificata in FINOADATA
169500040726     ** deve prendere come tariffa poste la tariffa bartolini
169600040726     c                   if        dataoggi >= finoadata
169700040726     C                   MOVEL     WTARV         VI4CTD                          VALIDA
169800040726     C                   MOVEL     VI4CTD        VH4CTB
169900040726     c                   end
170000040726     **
170100000622    1C                   ENDIF
170200000622    0C                   ENDIF
170300931021     C*
170400931021     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
170500011030     C     VI4CTD        IFEQ      *BLANKS
170600011030     C                   MOVEL     '???'         VI4CTD
170700000629     C                   SETOFF                                       5150
170800000626     C                   ELSE
170900000626     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
171000000626     C     VH4CTB        IFNE      *BLANKS
171100011030     C                   MOVEL     VH4CTB        VI4CTD
171200000626     C                   ENDIF
171300000626     C*
171400931021     C                   ENDIF
171500931021     C*
171600040727     ** Si memorizza se ha trovato una tariffa poste per poter
171700040727     **  gestire variazioni tariffe impostate a video su sfl su tariffe Bartolini
171800040728     c                   if        vh4TSR='C'
171900040727     c                   clear                   tar_poste         1
172000040727     c                   if        vh4CTP <> *blank
172100040727     c                   move      'S'           tar_poste         1
172200040727     c                   end
172300040728     c                   endIF
172400040727     **
172500931119     C                   ENDSR
172600040825     C*================================================================
172700040825     C* cerca eventuale tariffa di riferimento x Poste            ----*
172800040825     C*================================================================
172900040825     C     Ric_Tar_RIF   BEGSR
173000040825     **
173100040825     C                   Z-ADD     Ptarv_rap     rifPDR
173200040825     C                   Z-ADD     Ptarv_rct     rifCTR
173300040825     C*
173400040825     C                   move      'N'           tar_valida
173500040825     C*
173600040825     C     KFGT_rif      setll     fifgt01l
173700040825     C     KFGT_rif      reade     fifgt01l
173800040825      *
173900040825    1C                   DOW       not %eof(fifgt01l)
174000040825     C*
174100040825     C* TARIFFA VALIDA di riferimento POSTE
174200040825     C*  Non deve essere annullata e deve avere la divisa richiesta
174300040825      **   Data Documento deve essere compresa fra le date di attivazione e
174400040825      ***   scadenza e deve essere stata stampata (quindi convalidata)
174500040825      ****   >altrimenti è un Manca Tariffa<
174600040825    2C     FGTATB        ifEQ      ' '
174700091112     c     w02div        andeq     fgtdiv
174800091112     C*
174900091112     C* Se tariffa del periodo
175000091112     C     Ktgt          CHAIN     FItgt01l
175100091112 2/3 c                   if        %Found(FItgt01l) and
175200091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
175300091112      *
175400091112    3C     parsml        IFeq      *blank
175500091112     C     tgtDTS        andgt     0
175600091112     C     fgtatb        oreq      ' '
175700091112     c     parsml        andeq     'S'
175800040825     C*  se trovata ok deve uscire dal giro
175900040825     C                   move      'S'           tar_valida
176000040825     C                   leave
176100091112    3C                   end
176200091112      *
176300091112 2/3 C                   end
176400040825    2C                   end
176500040825     C*
176600040825     C     KFGT_rif      reade     fifgt01l
176700040825    1C                   ENDDO
176800040825     C*
176900040825    2C     Tar_valida    ifeq      'N'
177000040825     C                   clear                   PTARV
177100040825    2c                   end
177200040825     **
177300040825     C                   ENDSR
177400020212     C*---------------------------------------------------------------*
177500000622     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
177600020212     C*---------------------------------------------------------------*
177700000622     C     RICBOL        BEGSR
177800000622     C*
177900000622     C                   MOVEL     ' '           VH4POS
178000000622     C                   MOVEL     ' '           VH4BAR
178100000703     C                   MOVEL     *BLANKS       VH4CTP
178200000703     C                   MOVEL     *BLANKS       VH4CTB
178300021217     c                   z-add     0             conta             5 0
178400040628     c                   z-add     0             sav_FTDctb        3 0
178500040628     c                   z-add     0             sav_FTDctp        3 0
178600040727     C* Oltre  a salvare la tariffa vera e propria deve anche impostare un flag
178700040727     C* parallelamente poichè potrebbero esistere tariffe '000' e quindi potrebbe
178800040727     C* non essere comprensibile solo il dato sav_FTDct*
178900040727     c                   move      *blank        sav_FLGctb        1
179000040727     c                   move      *blank        sav_FLGctp        1
179100000622     C*
179200000622     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
179300000622     C* UTILE PER RICONOSCERE SE BOLLA POSTA
179400011105     C     KFTTT         SETLL     FIFTT02L
179500011105     C     KFTTT         READE     FIFTT02L                               39
179600000714     C**
179700000714     C     *IN39         DOWEQ     *OFF
179800000714     C**
179900011105     C     KFTDT         SETLL     fiftd001
180000000622     C                   DO        *HIVAL
180100011105     C     KFTDT         READE     fiftd001                               32
180200000622     C*
180300000622     C   32              LEAVE
180400021217     c                   add       1             conta
180500000714     C**
180600000622     C* CONTROLLO IN AZORG SE PO POSTE
180700000622     C                   EXSR      CTRORG
180800000622     C*  P.O. POSTE
180900020729     C     §OgNTW        IFEQ      'PPT'
181000000622     C                   MOVEL     'S'           VH4POS
181100040729     c                   if        ftdctp <> *blank and sav_FLGctp = ' '
181200040628     c                   move      ftdctp        sav_FTDctp
181300040727     c                   move      'S'           sav_FLGctp
181400040628     c                   end
181500000622     C                   ELSE
181600000622     C                   MOVEL     'S'           VH4BAR
181700040729     c                   if        ftdctp <> *blank and sav_FLGctb = ' '
181800040628     c                   move      ftdctp        sav_FTDctb
181900040727     c                   move      'S'           sav_FLGctb
182000040628     c                   end
182100000622     C                   ENDIF
182200000622     C*
182300000622     C                   ENDDO
182400000714     C**
182500011105     C     KFTTT         READE     FIFTT02L                               39
182600000714     C                   ENDDO
182700040727     **
182800040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
182900040727     C                   if        VH4BAR = ' ' and VH4POS = 'S'
183000040727     c                   move      sav_FTDctp    sav_FTDctb
183100040727     c                   end
183200040727     **
183300021217     C* Attenzione: Occorre gestire una eccezione.
183400021217     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
183500021217     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
183600021217     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
183700021217     C*  il VH4BAR come se avessi trovato solo delle bolle Bartolini.
183800021217     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
183900021217     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
184000021217     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
184100021217     C* su Bartolini. (Per Default)
184200030127     c                   if        vh4TSR='R' and conta=0 and vh2TBS > 0
184300021217     C                   MOVEL     'S'           VH4BAR
184400021217     C                   EndIF
184500021217     C*
184600000622     C                   ENDSR
184700020521     C*---------------------------------------------------------------*
184800020521     C* CONTROLLO IN AZORG SE PO POSTE
184900020212     C*---------------------------------------------------------------*
185000000622     C     CTRORG        BEGSR
185100020212     C*---------------------------------------------------------------*
185200000622     C* AGGANCIO AZORG
185300000622     C     FTDLNP        CHAIN     AZORG01L                           34
185400000622     C   34              MOVEL     *BLANKS       OG143
185500000622     C  N34              MOVEL     ORGDE3        OG143
185600000622     C*
185700000622     C                   ENDSR
185800020212     C*---------------------------------------------------------------*
185900931018     C* CONTROLLO DATI SUBFILE 4 -------------------------------------*
186000020212     C*---------------------------------------------------------------*
186100931018     C     CONTR4        BEGSR
186200931018     C                   Z-ADD     1             NRR4
186300931018     C                   SETOFF                                       90
186400911014     C*
186500931019     C                   READC     FRB8SF4                                32
186600930707     C*
186700931019    1C     *IN32         DOWEQ     *OFF
186800931019     C*
186900950117     C* RECORD DEL TIPO SERVIZIO
187000931019    2C     VH4NRR        IFEQ      0
187100931019     C*
187200931018     C* C O D I C E   T A R I F F A
187300011030    3C     VI4CTD        IFNE      '???'
187400931019     C* RICERCA
187500011112     C     '?'           SCAN      VI4CTD                                 30
187600011106     C*
187700011106     C* Se si tratta della 1° riga (significa Codice Tariffa)
187800011106     c     vh4tpr        ifeq      1
187900931018     C*
188000011112I   4C     *IN30         IFEQ      *ON
188100931028     C                   CLEAR                   PARAM3
188200931028     C                   MOVEL     VI4PDR        PA3PDR
188300931028     C                   MOVEL     PARSML        PA3SML
188400931028     C                   MOVEL     VH4TSR        PA3TSR
188500100513     C                   MOVEL     KPJBU         kpjbus
188600100513     C                   clear                   kpjbu
188700931028     C                   MOVEL     PARAM3        KPJBU
188800011112     C                   CALL      'FICN86R'
188900931028     C                   PARM                    KPJBA
189000931108     C                   MOVEL     KPJBU         PARAM3
189100100513     C                   MOVEL     KPJBUs        kpjbu
189200931028     C* SELEZIONE EFFETTUATA
189300931028     C     PA3SEL        IFEQ      ' '
189400011030     C                   MOVEL     PA3CTR        VI4CTD
189500931028     C                   ELSE
189600011030     C                   MOVEL     '???'         VI4CTD
189700931028     C                   ENDIF
189800931028     C*
189900931105     C                   SETON                                        36
190000931018     C                   WRITE     FRB8D05
190100000622    4C                   ENDIF
190200931018     C* CONTROLLO
190300011030     C                   TESTN                   VI4CTD               30
190400931105    5C     *IN30         IFEQ      *OFF
190500931105     C                   SETON                                        90
190600931105   X5C                   ELSE
190700931105     C*
190800011030     C                   MOVEL     VI4CTD        W01CTR
190900931105     C                   EXSR      CTRTAR
191000931105    5C                   ENDIF
191100931019     C   90              SETON                                        4036
191200000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
191300000622     C* POSTE O BARTOLINI
191400000622     C     *IN90         IFEQ      *OFF
191500000630     C* IN CASO DI DISTINTA GIA' VALORIZZATA NON SI FA
191600000630     C     *IN24         ANDEQ     *OFF
191700000622     C*
191800000622     C     VH4POS        IFEQ      'S'
191900000622     C     VH4BAR        ANDNE     'S'
192000000622     C* E LA TARIFFA È BARTOLINI ERRORE
192100000622     C     FGTFTP        ANDNE     'O'
192200040726     c                   if        dataoggi < finoadata
192300000622     C                   SETON                                        905236
192400040726     c                   end
192500000622     C                   ENDIF
192600000622     C*
192700000622     C     VH4POS        IFNE      'S'
192800000622     C     VH4BAR        ANDEQ     'S'
192900040622     C*
193000000622     C* E LA TARIFFA È BARTOLINI ERRORE
193100000622     C     FGTFTP        ANDEQ     'O'
193200000622     C                   SETON                                        905336
193300000622     C                   ENDIF
193400040622     C*
193500000703     C* SE CI SONO SPEDIZIONI MISTE MA MANCA ALMENO UNA DELLE DUE TARIFFE DO ERRORE
193600000703     C* LA PRIMA VOLTA CHE CONTROLLO
193700000703     C     VH4POS        IFEQ      'S'
193800000703     C     VH4BAR        ANDEQ     'S'
193900040622     C*
194000000703     C* MANCA TARIFFA POSTA
194100000703     C     VH4CTP        IFEQ      *BLANK
194200040727     ** oppure se in un secondo tempo non essendoci tariffe poste è stata impostata
194300040727     ** un'altra tariffa bartolini rispetto a quella standard proposta
194400040728     c     vh4TPO        oreq      *blank
194500040727     C     vh4CTP        andne     vi4CTD
194600040622     C*
194700040622     C*  fino al giorno 2004/08/10 il controllo è attivato
194800040726     C*   comunque imposta in Hidden il valore espresso a video
194900040726     C*    ma non da + l'errore
195000040622     c                   if        dataoggi < finoadata
195100000703     C                   SETON                                        905136
195200040726     c                   end
195300011030     C                   MOVEL     VI4CTD        VH4CTP
195400040622     C*
195500000703     C                   ENDIF
195600000703     C*
195700000703     C     VH4CTB        IFEQ      *BLANK
195800000703     C                   SETON                                        905036
195900011030     C                   MOVEL     VI4CTD        VH4CTB
196000000703     C                   ENDIF
196100000703     C*
196200000703     C                   ENDIF
196300000622     C*
196400000622     C* SE NON E' ACCESO IL 90 VALORIZZO I CAMPI TARIFFA HIDDEN SE CAMBIATI
196500000622     C     *IN90         IFEQ      *OFF
196600000622     C*+
196700000622     C     FGTFTP        IFEQ      'O'
196800011030     C                   MOVEL     VI4CTD        VH4CTP
196900000622     C                   ELSE
197000011030     C                   MOVEL     VI4CTD        VH4CTB
197100000622     C                   ENDIF
197200000622     C*
197300000622     C                   ENDIF
197400000622     C                   ENDIF
197500011106     C*
197600011106    4C                   end
197700011106     C*
197800931021    3C                   ENDIF
197900931019     C*
198000931108     C* NON E' POSSIBILE ANDARE IN MANUTENZIONE DI UN TIPO PRESTAZIONE
198100931019     C  N90VI4SCE        IFEQ      'M'
198200931019     C                   SETON                                        419036
198300931019     C                   ENDIF
198400931019     C*
198500931108     C* ANNULLAMENTO VALORIZZAZIONE DI TUTTO IL TIPO PRESTAZIONE
198600931019     C*  LO ESEGUO DOPO
198700931019     C  N90VI4SCE        IFEQ      'A'
198800931112     C   20              SETON                                        459036
198900931112     C  N20              SETON                                        36
199000931019     C                   ENDIF
199100931019     C*
199200931019   X2C                   ELSE
199300011107     C*
199400931019     C* RECORD DI DISTINTA/RITIRO
199500011107     C*
199600931019     C* MANUTENZIONE SPEDIZIONI DELLA DISTINTA
199700931022    3C     VI4SCE        IFEQ      'M'
199800931022    4C     VI4ITT        IFGT      0
199900931022     C     VI4ITA        ANDGT     0
200000931022     C                   SETON                                        439036
200100931022   X4C                   ELSE
200200931019     C                   EXSR      MANSPE
200300931019     C                   SETON                                        90
200400931028     C                   WRITE     FRB8D05
200500931105     C*
200600931105     C* RIAGGANCIO IL RECORD DI SUBFILE
200700931105     C     NRR4          CHAIN     FRB8SF4                            32
200800931022    4C                   ENDIF
200900931022    3C                   ENDIF
201000931019     C*
201100931019     C* ANNULLO VALORIZZAZIONE PRECEDENTE DELLA DISTINTA
201200931019     C     VI4SCE        IFEQ      'A'
201300931105     C     VI1FVL        ANDEQ     'V'
201400931112     C   20              SETON                                        459036
201500931112     C  N20              EXSR      ANNSPE
201600931112     C  N20              SETON                                        21
201700931105     C                   ENDIF
201800931019     C*
201900950117     C* CONTROLLO PIKING/ETICHETTATURA S/N
202000950117     C* Se = Blank imposto quello specificato su anagrafico padroncini
202100950117     C     VI4PEP        IFEQ      *BLANKS
202200950127     C                   MOVEL     VH4PEA        VI4PEP
202300950117     C                   END
202400021211      *
202500021211     C* Se = Blank imposto quello specificato su anagrafico padroncini
202600021211      *   per carico/scarico
202700021211     C     vi4FCS        IFEQ      *BLANKS
202800021211     C                   eval      vi4FCS = vh4ACS
202900021211     C                   END
203000950117     C*
203100931019    2C                   ENDIF
203200921228     C*
203300931019    2C     VH4NRR        IFEQ      0
203400931018     C                   SETOFF                                       11
203500931018     C                   ELSE
203600931018     C                   SETON                                        11
203700931019    2C                   ENDIF
203800911017     C*
203900931019     C  N36              MOVEL     ' '           VI4SCE
204000931018     C                   Z-ADD     NRR4          REC4
204100011109     c     vh4tpr        comp      2                                      26
204200931019     C                   UPDATE    FRB8SF4
204300931018     C                   SETOFF                                       36
204400930707     C*
204500931019     C   90              SETON                                        32
204600931019     C  N90              READC     FRB8SF4                                32
204700931018    1C                   ENDDO
204800931019     C**
204900931019     C* SE TUTTO OK CONTROLLO SE HO ANNULLAMENTI DI VALORIZZAZIONE
205000931108     C*  DI TIPO PRESTAZ DA FARE
205100931108    1C     *IN90         IFEQ      *OFF
205200931019     C                   READC     FRB8SF4                                32
205300931019     C*
205400931108    2C     *IN32         DOWEQ     *OFF
205500931105     C*
205600931108    3C     VI4SCE        IFEQ      'A'
205700931108     C* SE NON SONO GIA' VALORIZZATE PULISCO SOLO IL CAMPO DI SCELTA SF
205800931108    4C     VI1FVL        IFEQ      'V'
205900931019     C                   EXSR      ANNTSR
206000931105     C*
206100931105     C* AL TERMINE RIEMETTO IL SFL2 (21 ON)
206200931105     C                   SETON                                        21
206300931108   X4C                   ELSE
206400931108     C                   MOVEL     ' '           VI4SCE
206500011109     c     vh4tpr        comp      2                                      26
206600931108     C                   UPDATE    FRB8SF4
206700931108    4C                   ENDIF
206800931105     C*
206900931108    3C                   ENDIF
207000931019     C*
207100931019     C                   READC     FRB8SF4                                32
207200931108    2C                   ENDDO
207300931108    1C                   ENDIF
207400911017     C*
207500900614     C                   ENDSR
207600020212     C*---------------------------------------------------------------*
207700931019     C* MANUTENZIONE SPEDIZIONI DISTINTA SELEZIONATA -----------------*
207800020212     C*---------------------------------------------------------------*
207900931019     C     MANSPE        BEGSR
208000931110     C                   SETOFF                                       151637
208100931108     C                   CLEAR                   FRB8SF6
208200931028     C     VI4ITT        IFGT      0
208300931028     C                   SETON                                        15
208400931028     C                   ENDIF
208500931028     C     VI4ITA        IFGT      0
208600931028     C                   SETON                                        16
208700931028     C                   ENDIF
208800931028     C*
208900931019     C* CARICAMENTO SPEDIZIONI
209000931019     C                   EXSR      CARSPE
209100130305     C*
209200931110     C* SE NON CE NE SONO NON EMETTO IL SUBFILE
209300030127     c                   move      'N'           Dett6_a0          1
209400931110     C     NRR6          IFEQ      0
209500931110     C                   SETON                                        37
209600030127     c                   move      'S'           Dett6_a0
209700030127     c                   z-add     vi4ran        d8ran
209800931110     C                   ENDIF
209900931019     C*
210000931103     C                   Z-ADD     1             REC6
210100931019     C                   WRITE     FRB8D07
210200030127     C   37              WRITE     FRB8D08
210300931019     C     FORCT6        TAG
210400020409     c                   if        savnr6 > 0
210500020409     c                   z-add     savnr6        rec6
210600020409     c                   end
210700931019     C                   EXFMT     FRB8CT6
210800020409     c                   z-add     nrrult        savnr6            5 0
210900931019     C* CMD3 - FINE LAVORO
211000931019     C     *INKC         IFEQ      *ON
211100931111     C   22              SETON                                        4490
211200931111     C   22              GOTO      FORCT6
211300931019     C                   SETON                                        LR
211400931019     C                   GOTO      ENDMAN
211500931019     C                   ENDIF
211600931019     C* CMD12 - RITORNO
211700931019     C   KL              GOTO      ENDMAN
211800030127      *
211900030127      * CONTROLLI solo se ci sono delle righe e non sono solo
212000030127      *   dei ritiri annullati
212100030127     c                   if        Dett6_a0 = 'N'
212200931019     C                   EXSR      CONTR6
212300030127     c                   end
212400931202     C   90              GOTO      FORCT6
212500020418     C*
212600020418     c                   if        VI1FVL<>'V'
212700931019     C* CMD6 - AGGIORNAMENTO
212800931202     C   KE
212900030127     cOR KF              if        Dett6_a0 = 'N'
213000030127     C                   EXSR      AGGIO6
213100020408     C   ke
213200020408     Cor kf              commit
213300020418     c                   end
213400030127     c                   end
213500931202     C* CMD5 RIVISUALIZZA
213600931202     C   KE              EXSR      CARSPE
213700931202     C   KE              Z-ADD     1             REC6
213800931202     C* RIEMETTO VIDEATA PER ENTER O CMD5
213900931202     C  NKF              GOTO      FORCT6
214000931019     C*
214100931021     C     ENDMAN        ENDSR
214200020212     C*---------------------------------------------------------------*
214300950116     C* CARICO LE SPEDIZIONI DELLA DISTINTA A VIDEO ------------------*
214400020212     C*---------------------------------------------------------------*
214500931019     C     CARSPE        BEGSR
214600931021     C                   CLEAR                   NRR6
214700931021     C                   CLEAR                   W06NRR
214800931019     C*
214900931019     C                   SETON                                        35
215000931019     C                   WRITE     FRB8CT6
215100931019     C                   SETOFF                                       35
215200931019     C*
215300931019     C* DECODIFICA C A M P I   T E S T A T A
215400931019     C*
215500931019     C                   MOVEL     '1N'          COD
215600931019     C                   MOVEL     *BLANKS       KEY
215700931103     C                   MOVEL     VH4TSM        KEY
215800931019     C     KTAB          CHAIN     TABEL                              30
215900931019     C  N30              MOVEA     TBLUNI        K17(3)
216000931019     C   30              MOVEA     *BLANKS       K17(3)
216100931103     C                   MOVEL     VH4TSM        K17(1)
216200931019     C                   MOVEL     '-'           K17(2)
216300931019     C                   MOVEA     K17           VI6TSM
216400931019     C* MITTENTE/DESTINATARIO
216500931022    1C     VH4TSR        IFEQ      'R'
216600931104     C                   SETON                                        19
216700020311     C                   MOVEL     'In'          VI6IA
216800020311     C                   MOVEL     CSPED         VI6MD
216900931108     C*
217000931108   X1C                   ELSE
217100931108     C*
217200931104     C                   SETOFF                                       19
217300931029     C                   MOVEL     'In'          VI6IA
217400931202     C                   MOVEL     CSPED         VI6MD
217500931019    1C                   ENDIF
217600931021     C*
217700931021     C* REPERISCO CODICE TARIFFA DALLA RIGA IN CUI SI TROVA
217800121221     c                   move      'N'           h6targ            1
217900931103     C                   Z-ADD     NRR4          W05NRR
218000931021     C     VH4NRR        CHAIN     FRB8SF4                            30
218100931022     C   30              MOVEL     '000'         W01CTR
218200011106     c                   if        vh4tpr = 1
218300011030     C  N30VI4CTD        IFEQ      '???'
218400121221     c                   move      'S'           h6targ
218500931022     C                   MOVEL     000           W01CTR
218600931022     C                   ELSE
218700011030     C                   MOVEL     VI4CTD        W01CTR
218800931021     C                   ENDIF
218900011105     c                   endif
219000011105      *
219100000703     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
219200000703     C     VH4CTP        IFNE      *BLANKS
219300000703     C     VH4CTB        ANDNE     *BLANKS
219400000703     C                   MOVEL     VH4CTP        WCTP              3
219500000703     C                   MOVEL     VH4CTB        WCTB              3
219600000703     C                   ELSE
219700000703     C                   MOVEL     *BLANKS       WCTP
219800000703     C                   MOVEL     *BLANKS       WCTB
219900000703     C                   ENDIF
220000931103     C                   Z-ADD     W05NRR        NRR4
220100931028     C     NRR4          CHAIN     FRB8SF4                            30
220200931019     C*
220300931019     C* DECODIFICA  C A M P I   D E T T A G L I O
220400931019     C                   CLEAR                   K17
220500931203     C                   CLEAR                   WFTD
220600931108     C*
220700011105     C     KFTD          SETLL     fiftd001
220800011105     C     KFTD          READE     fiftd001                               32
220900931019     C*
221000931019    1C     *IN32         DOWEQ     *OFF
221100931203     C                   ADD       1             WFTD              5 0
221200000626     C                   SETOFF                                       132325
221300000626     C                   SETOFF                                       49
221400950118     C*
221500950119     C                   MOVE      *BLANKS       VI6FTC
221600950119     C                   MOVE      *BLANKS       VI6FT2
221700950119     C                   MOVE      *BLANKS       VI6TSP
221800030320     C                   MOVE      *BLANKS       Vh6cbo
221900950119     C                   MOVE      *BLANKS       VH6SIC
222000950119     C                   MOVE      *BLANKS       VH6GVA
222100950119     C                   MOVE      *ZEROS        VH6NCE
222200000705     C                   MOVE      *ZEROS        VH6FGC
222300000623     C*
222400000623     C* CONTROLLO SE BOLLA POSTA O BARTOLINI
222500000623     C                   EXSR      CTRORG
222600020729     C     §OgNTW        IFEQ      'PPT'
222700020729     c                   movel     'S'           VH6POS
222800020729     c                   else
222900020729     C                   MOVEL     *blank        VH6POS
223000020729     c                   end
223100000626     C* SE BOLLA POSTA PROTEGGO IL CAMPO NUMERO STOP
223200020729     C     §OgNTW        IFEQ      'PPT'
223300000626     C                   SETON                                        49
223400000626     C                   ELSE
223500000626     C                   SETOFF                                       49
223600000626     C                   ENDIF
223700950119     C*
223800011123     C                   z-add     FTDSTP        VI6STP                          STOP
223900020313     C* ----------
224000020313     C* servono questi 2 campi hidden per poter gestire la vista logica FIFTD04L
224100020313     C* in alternativa alla FIFTD02L
224200020313     C                   z-add     FTDksc        vh6ksc                          cliente
224300020313     C                   z-add     FTDnsp        vh6nsp                          spedizione
224400020326     C                   move      FTDtsr        vh6tsr
224500020326     C                   z-add     FTDfgs        vh6fgs
224600020326     c                   if        vh6fgs=0
224700020326     C                   movel     FTDpdr        vh6fgs
224800020326     c                   end
224900020313     C* ----------
225000950116     C                   MOVEL     FTDRSC        VI6RSB                          RAG SOCIALE
225100950113     C                   MOVEL     FTDCAP        VI6CAP
225200950118     C                   Z-ADD     FTDCAS        VH6CAS                          CONTRASSEGNO
225300950131     C                   MOVEL     FTDVCA        VH6VCA                          DIVISA
225400011106     c                   if        vh6vca=*blank
225500011106     c                   eval      vh6vca='ITL'
225600011106     c                   end
225700950118     C                   Z-ADD     FTDIFP        VH6IFP                          IMP.INCASS.
225800011109      ***>>>>>>>>>>>>>>
225900011109     c                   if        vh4tpr = 2
226000011109     c                   movel     vi4ctd        vi6div
226100011109     c                   end
226200950118     C                   Z-ADD     FTDSET        VH6SET
226300931202     C* RITIRI
226400931202    2C     VH4TSR        IFEQ      'R'
226500020311     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
226600020311    2C     ftdnsp        ifeq      0
226700020311    2C     ftdksc        andgt     0
226800020311     C                   z-add     ftdksc        vi6nsp                          COD CLIENTE
226900020311     C                   end
227000950118     C                   Z-ADD     FTDNCE        VH6NCE                          COLLI ETICH
227100931202     C                   ELSE
227200931202     C* CONSEGNE
227300091012     C     Bolla_FTD     CHAIN     FNARB01L                           30
227400090918     C                   Z-ADD     arbKSC        Vh6ksc                          Cliente
227500090921      *
227600960208     C                   MOVEL     ARBGVA        VH6GVA                          PARTICOLARITA'
227700960208     C                   MOVE      ' '           VH6GVA
227800950118     C                   MOVEL     ARBTSP        VI6TSP                          C/E/D
227900030320     C                   MOVEL     ARBcbo        Vh6cbo                          C/E/D
228000931202     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
228100950113     C                   MOVE      ARBMGS        DATA                            DATA SPEDIZ
228200931019     C                   MOVEL     GG            VI6MGS
228300931019     C                   MOVE      MM            VI6MGS
228400931028     C* SPEDIZIONE CONSEGNATA
228500931105    3C     FTDDCM        IFGT      0
228600950113     C                   MOVE      FTDDCM        DATA
228700931019     C                   MOVEA     GG            K17(1)
228800931019     C                   MOVEL     '/'           K17(3)
228900931019     C                   MOVEA     MM            K17(4)
229000931019     C                   MOVEL     '/'           K17(6)
229100931019     C                   MOVEA     AA            K17(7)
229200931019     C                   MOVEA     K17           VI6CON
229300950117     C* FLAG RITORNO ALL'INCASSO
229400950117     C     FTDSIC        COMP      'S'                                    23
229500950117     C                   MOVEL     FTDSIC        VH6SIC
229600931029     C*
229700950113     C* CONSEGNA PARTICOLARE 1 (SOLO SE E' CONSEGNATA)
229800950113    3C     ARBTC1        IFNE      ' '
229900931029     C                   MOVEL     '1P'          COD
230000931029     C                   MOVEL     *BLANKS       KEY
230100950113     C                   MOVEL     ARBTC1        KEY
230200931029     C     KTAB          CHAIN     TABEL                              30
230300931029     C*
230400931029     C  N30              MOVEA     TBLUNI        K17(3)
230500931029     C   30              MOVEA     *BLANKS       K17(3)
230600950113     C                   MOVEL     ARBTC1        K17(1)
230700931029     C                   MOVEL     '-'           K17(2)
230800931029     C                   MOVEA     K17           VI6FTC
230900931029     C                   ENDIF
231000950113     C* CONSEGNA PARTICOLARE 2 (SOLO SE E' CONSEGNATA)
231100950113    3C     ARBTC2        IFNE      ' '
231200950113     C                   MOVEL     '1P'          COD
231300950113     C                   MOVEL     *BLANKS       KEY
231400950113     C                   MOVEL     ARBTC2        KEY
231500950113     C     KTAB          CHAIN     TABEL                              30
231600950113     C*
231700950113     C  N30              MOVEA     TBLUNI        K17(3)
231800950113     C   30              MOVEA     *BLANKS       K17(3)
231900950113     C                   MOVEL     ARBTC2        K17(1)
232000950113     C                   MOVEL     '-'           K17(2)
232100950113     C                   MOVEA     K17           VI6FT2
232200950113     C                   ENDIF
232300931019     C*
232400931019   X3C                   ELSE
232500931019     C*
232600931019     C* C'E' CODICE GIACENZA/MANCATA CONSEGNA
232700931019    4C     FTDCMC        IFNE      ' '
232800931019     C                   MOVEA     FTDCMC        K17(1)
232900931019     C                   MOVEL     '-'           K17(4)
233000931019     C                   MOVEL     '2A'          COD
233100931019     C                   MOVEL     *BLANKS       KEY
233200931019     C                   MOVEL     FTDCMC        KEY
233300931019     C     KTAB          CHAIN     TABEL                              30
233400931019     C  N30              MOVEA     TBLUNI        K17(5)
233500931021     C   30              MOVEA     *BLANKS       K17(5)
233600931019     C                   MOVEA     K17           VI6CON
233700000705     C*
233800000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
233900000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
234000000623     C     VH6POS        IFEQ      'S'
234100000623     C                   MOVEL     '2Z'          COD
234200000623     C                   MOVEL     *BLANKS       KEY
234300000623     C                   MOVEL     FTDCMC        KEY
234400000623     C     KTAB          CHAIN     TABEL                              30
234500000623     C  N30              MOVEL     TBLUNI        DS2Z
234600000623     C   30              MOVEL     *BLANKS       DS2Z
234700000626     C     §2ZTPP        IFEQ      ' '
234800000626     C* NON PAGO LA SPEDIZIONE
234900000630     C                   SETON                                        2513
235000000626     C*
235100000626     C                   ENDIF
235200000623     C                   MOVEL     §2ZTPP        VH6FGC
235300000623     C                   MOVEL     §2ZTPP        FTDFGC
235400000623     C                   ENDIF
235500931019     C*
235600931019   X4C                   ELSE
235700931019     C* C'E' MANCATA CONSEGNA
235800931019     C                   MOVEA     FTDTMC        K17(1)
235900931019     C                   MOVEL     '-'           K17(2)
236000931019     C                   MOVEL     '2A'          COD
236100931019     C                   MOVEL     *BLANKS       KEY
236200931019     C                   MOVEL     FTDTMC        KEY
236300931019     C     KTAB          CHAIN     TABEL                              30
236400931019     C  N30              MOVEA     TBLUNI        K17(3)
236500931021     C   30              MOVEA     *BLANKS       K17(3)
236600931019     C                   MOVEA     K17           VI6CON
236700931019     C*
236800931028     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
236900931019     C* PRIMA CERCO CON FILIALE DI APPARTENENZA
237000931021     C                   MOVEL     KEY           WALF6             6
237100931111     C                   EXSR      CTRCMC
237200000630    4C                   ENDIF
237300931019     C*
237400950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
237500931028     C*                                    (SE NON L'HO TROVATA)
237600931028     C     *IN30         IFEQ      *OFF
237700931019     C                   SETON                                        13
237800931019    4C                   ENDIF
237900931019    3C                   ENDIF
238000931028     C*
238100950119     C* CODICE BOLLA
238200030320     C                   MOVEL     ARBCBO        VI6CBO
238300950119     C                   MOVEL     '3A'          COD
238400950119     C                   MOVEL     *BLANKS       KEY
238500950119     C                   MOVEL     ARBCBO        KEY
238600950119     C     KTAB          CHAIN     TABEL                              30
238700950119     C  N30              MOVEL     TBLUNI        DS3A
238800950119     C  N30              MOVEL     §3ADES        VI6CBD
238900950119     C   30              MOVEL     *BLANKS       VI6CBD
239000950119     C*
239100931028     C* C O D I C E   B O L L A   D A   T A S S A R E
239200931028    3C     *IN13         IFEQ      *OFF
239300931111     C                   EXSR      CTRCBL
239400931028     C*
239500950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
239600931028     C*                                    (SE L'HO TROVATO)
239700931111    4C     *IN31         IFEQ      *ON
239800931028     C                   SETON                                        13
239900931028    4C                   ENDIF
240000931028    3C                   ENDIF
240100931028    2C                   ENDIF
240200931202     C*
240300931202     C* AGGIORNO PERCHE' POTREI AVER IMPOSTATO IL FLAG DI RICONS.GIAC
240400011105     C                   UPDATE    fiftd001
240500931019     C*
240600931022     C     FTDCTP        IFNE      *BLANKS
240700931022     C                   MOVEL     FTDCTP        VI6CTR
240800931022     C                   ELSE
240900000622     C* VERIFICO SE ESISTONO SIA BOLLE POSTE CHE BOLLE BARTOLINI
241000000703     C     WCTB          IFNE      '   '
241100000703     C     WCTP          ANDNE     '   '
241200000622     C* SE E' UN PO POSTE
241300000622     C     VH6POS        IFEQ      'S'
241400000622     C* MUOVO TARIFFA POSTA
241500000703     C                   MOVEL     WCTP          VI6CTR
241600000622     C                   ELSE
241700000622     C* MUOVO TARIFFA BARTOLINI
241800000703     C                   MOVEL     WCTB          VI6CTR
241900000622     C                   ENDIF
242000000622     C                   ELSE
242100121221     C                   MOVEL     h6targ        Vh6targ
242200121221     C                   MOVEL     W01CTR        VI6CTR
242300000622     C                   ENDIF
242400000622     C*
242500931022     C                   ENDIF
242600011105      *
242700011105      *  pulito il campo di totale
242800011108     C                   clear                   vi6tot
242900011115     c                   move      '0'           vp6ita
243000011115     c                   move      '0'           vp6itt
243100011115      *
243200011115      * imposta il flag di protezione in memoria sulla riga
243300011115     c                   if        *in13 = *on or *in16 = *on
243400011115     c                   eval      vp6ita = '1'
243500011115     c                   end
243600011115      *
243700011115      * imposta il flag di protezione in memoria sulla riga
243800011115     c                   if        *in15 = *on or *in23 = *on or *in25 = *on
243900011115     c                   eval      vp6itt = '1'
244000011115     c                   end
244100011115      * pulisce i campi
244200011115     C                   z-add     0             vh6ita
244300011115     C                   z-add     0             vi6ita
244400011115     C                   z-add     0             vh6itt
244500011115     C                   z-add     0             vi6itt
244600011105      *
244700950117     C* ACCESSORI NON VISUALIZZABILI: NON CARICO NEMMENO L'IMPORTO
244800931105     C     *IN13         IFEQ      *ON
244900950119     C                   MOVEL     'B'           VH6FIA
245000931105     C                   ELSE
245100011109     C                   MOVEL     FTDITA        VI6ITA
245200011115     C  N16              add       ftdita        vi6tot
245300931022     C                   MOVEL     FTDITA        VH6ITA
245400931021     C                   MOVEL     FTDFIA        VH6FIA
245500931105     C                   ENDIF
245600950117     C* NON CARICO IMPORTO BASE SE SI TRATTA DI UN RITORNO ALL'INCASSO
245700950118     C     FTDSIC        IFNE      'S'
245800011109     C                   MOVEL     FTDITT        VI6ITT
245900011115     C  N15              add       ftditt        vi6tot
246000950117     C                   MOVEL     FTDITT        VH6ITT
246100950117     C                   MOVEL     FTDFIT        VH6FIT
246200950119     C                   ELSE
246300950119     C                   MOVEL     'B'           VH6FIT
246400950117     C                   ENDIF
246500931105     C                   MOVEL     FTDAAS        VH6AAS
246600931019     C                   ADD       1             NRR6
246700020313     C*
246800020418     c     vi1fvl        comp      'V'                                    57
246900931019     C                   WRITE     FRB8SF6
247000931019     C*
247100011105     C     KFTD          READE     fiftd001                               32
247200931019    1C                   ENDDO
247300931203     C*
247400931203     C                   MOVEL     W01CTR        W01CTP
247500931019     C*
247600931019     C                   ENDSR
247700020212     C*---------------------------------------------------------------*
247800931019     C* ANNULLAMENTO VALORIZZAZIONE DI UNA DISTINTA ------------------*
247900020212     C*---------------------------------------------------------------*
248000931019     C     ANNSPE        BEGSR
248100931108     C*
248200011105     C     KFTD          SETLL     fiftd001
248300011105     C     KFTD          READE     fiftd001                               32
248400931019     C*
248500931022    1C     *IN32         DOWEQ     *OFF
248600931022     C* 14 ON  - ANNULLO TUTTO
248700931022    2C     *IN14         IFEQ      *ON
248800931022     C                   CLEAR                   FTDFIT                          FLG TAR BAS
248900931022     C                   CLEAR                   FTDITT                          TAR BASE
249000931022     C                   CLEAR                   FTDFIA                          FLG TAR ACC
249100931022     C                   CLEAR                   FTDITA                          TAR ACCESS
249200931022     C*
249300931022   X2C                   ELSE
249400931022     C* 14 OFF - ANNULLO SOLO LE VALORIZZAZIONI AUTOMATICHE
249500931022     C*
249600931022    3C     FTDFIT        IFEQ      ' '
249700931022     C                   CLEAR                   FTDITT                          TAR BASE
249800931022    3C                   ENDIF
249900931022    3C     FTDFIA        IFEQ      ' '
250000931022     C                   CLEAR                   FTDITA                          TAR ACCESS
250100931022    3C                   ENDIF
250200931022    2C                   ENDIF
250300931022     C*
250400020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
250500020326     C* i primi tre caratteri
250600020326     c                   if        ftdfgs = 0
250700020326     c                   movel     ftdpdr        ftdfgs
250800020326     c                   end
250900020326     C*
251000931022     C                   CLEAR                   FTDCTP                          COD TAR
251100011105     C                   UPDATE    fiftd001
251200931019     C*
251300020326     C*          *-------------------------------------*
251400020408     c     parsml        ifeq      ' '
251500020326     C*  deve annullare/cancellare (se c'è) anche la voce di C/E
251600020326     c     Kfce          setll     FiFCE01L
251700020326     c     Kfce          reade     FiFCE01L
251800020326     c                   dow       not %Eof(FiFCE01L)
251900020326     c                   delete    FIFCE000
252000020326     c     Kfce          reade     FiFCE01L
252100020326     C                   enddo
252200020408     C                   endif
252300020326     C*          *-------------------------------------*
252400011105     C     KFTD          READE     fiftd001                               32
252500931022    1C                   ENDDO
252600931019     C*
252700931022     C* 14 OFF - SVALORIZZO ANCHE LA TESTATA
252800931022    1C     *IN14         IFEQ      *OFF
252900931022     C*
253000011105     C     KFTT          CHAIN     FIFTT000                           32
253100931022    2C     *IN32         IFEQ      *OFF
253200931022     C*
253300931022    3C     FTTFIT        IFEQ      ' '
253400931022     C                   CLEAR                   FTTITT
253500931022    3C                   ENDIF
253600940121     C*
253700931022    3C     FTTFIA        IFEQ      ' '
253800931022     C                   CLEAR                   FTTITA
253900931022    3C                   ENDIF
254000931022     C*
254100940121     C                   Z-ADD     VI4ITT        FTTITT
254200940121     C     VI4ITT        IFGT      0
254300940121     C                   MOVEL     'M'           FTTFIT
254400940121     C                   ENDIF
254500940121     C                   Z-ADD     VI4ITA        FTTITA
254600940121     C     VI4ITA        IFGT      0
254700940121     C                   MOVEL     'M'           FTTFIA
254800940121     C                   ENDIF
254900020326     C*
255000020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
255100020326     C* i primi tre caratteri
255200020326     c                   if        fttfgs = 0
255300020326     c                   movel     fttpdr        fttfgs
255400020326     c                   end
255500940121     C*
255600940121     C                   MOVEL     ' '           FTTFVL                          FLG VALORIZ
255700940121     C                   MOVEL     ' '           FTTFLA                          FLG MAN TAR
255800931123     C                   CLEAR                   FTTSNA
255900931125     C                   CLEAR                   FTTCLA
256000950119     C                   CLEAR                   FTTCPE
256100931123     C                   CLEAR                   FTTKFA
256200931123     C                   CLEAR                   FTTTVL
256300950119     C                   CLEAR                   FTTSNE
256400931202     C                   CLEAR                   FTTSNP
256500011105     C                   UPDATE    FIFTT000
256600931022    2C                   ENDIF
256700931022    1C                   ENDIF
256800931019     C*
256900931019     C                   ENDSR
257000020212     C*---------------------------------------------------------------*
257100931108     C* ANNULLAMENTO VALORIZZAZIONE DI UN TIPO PRESTAZ ---------------*
257200020212     C*---------------------------------------------------------------*
257300931019     C     ANNTSR        BEGSR
257400931021     C     VI4SCE        DOWEQ     'A'
257500931021     C* ELIMINO IL FLAG
257600931021     C                   MOVEL     ' '           VI4SCE
257700011109     c     vh4tpr        comp      2                                      26
257800931021     C                   UPDATE    FRB8SF4
257900931021     C*
258000931019     C                   Z-ADD     VI4PDR        W01PDR
258100931019     C                   MOVEL     VH4TSR        W01TSR
258200931108     C*
258300931019     C                   ADD       1             NRR4
258400931019     C     NRR4          CHAIN     FRB8SF4                            32
258500931019     C*
258600931108     C* LEGGO TUTTE LE DISTINTE O I RITIRI DI QUEL PADR/TIPO PRESTAZ
258700931019     C     *IN32         DOWEQ     *OFF
258800931019     C     VI4PDR        ANDEQ     W01PDR
258900931019     C     VH4TSR        ANDEQ     W01TSR
259000931019     C*
259100931019     C                   EXSR      ANNSPE
259200931019     C*
259300931103     C                   ADD       1             NRR4
259400931019     C     NRR4          CHAIN     FRB8SF4                            32
259500931019     C                   ENDDO
259600931021     C                   ENDDO
259700931019     C*
259800931019     C                   ENDSR
259900020212     C*---------------------------------------------------------------*
260000931019     C* CONTROLLO SFL6 -----------------------------------------------*
260100020212     C*---------------------------------------------------------------*
260200931019     C     CONTR6        BEGSR
260300011105     C*
260400931019     C                   Z-ADD     1             NRR6
260500931019     C                   SETOFF                                       90
260600931103     C                   CLEAR                   WALF3
260700931203     C                   CLEAR                   WREAD
260800931021     C                   SETON                                        36
260900931019     C                   READC     FRB8SF6                                32
261000931019     C*
261100931019    1C     *IN32         DOWEQ     *OFF
261200931203     C                   ADD       1             WREAD             5 0
261300931021     C*
261400931123     C* CONTROLLO SE HO INSERITO DEI VALORI MANUALMENTE
261500931123     C* T A R I F F A   B A S E
261600011109      *>>>>>>>>>>>>>>>
261700011109    2C     VI6ITT        IFNE      VH6ITT
261800931022     C*
261900011109    3C     VI6ITT        IFGT      0
262000931021     C                   MOVEL     'M'           VH6FIT
262100931022   X3C                   ELSE
262200931021     C                   MOVEL     ' '           VH6FIT
262300931022    3C                   ENDIF
262400011105     C*
262500011109     C                   MOVEL     VI6ITT        VH6ITT
262600011108     C*>>>>>>>>
262700931022    2C                   ENDIF
262800931021     C*
262900931022     C* T A R I F F A   A C C E S S O R I
263000011108      *>>>>>>>>>>>>>>>
263100011109    2C     VI6ITA        IFNE      VH6ITA
263200931022     C*
263300011109    3C     VI6ITA        IFGT      0
263400931206     C                   MOVEL     'M'           VH6FIA
263500931022   X3C                   ELSE
263600931206     C                   MOVEL     ' '           VH6FIA
263700931022    3C                   ENDIF
263800011105     C*
263900011109     C                   MOVEL     VI6ITA        VH6ITA
264000011108     C*>>>>>>>>
264100931022    2C                   ENDIF
264200931019     C*
264300931019     C* C O D I C E   T A R I F F A
264400931019     C* RICERCA
264500121221     c                   if        vh6targ <> 'S'
264600931019     C     '?'           SCAN      VI6CTR                                 90
264700931019     C*
264800931021    2C     *IN90         IFEQ      *ON
264900931028     C                   CLEAR                   PARAM3
265000931028     C                   MOVEL     VI4PDR        PA3PDR
265100931028     C                   MOVEL     PARSML        PA3SML
265200931028     C                   MOVEL     VH4TSR        PA3TSR
265300100513     C                   MOVEL     KPJBU         kpjbus
265400100513     C                   clear                   kpjbu
265500931028     C                   MOVEL     PARAM3        KPJBU
265600011112     C                   CALL      'FICN86R'
265700931028     C                   PARM                    KPJBA
265800931110     C                   MOVEL     KPJBU         PARAM3
265900100513     C                   MOVEL     KPJBUs        kpjbu
266000931028     C* SELEZIONE EFFETTUATA
266100931028     C     PA3SEL        IFEQ      ' '
266200931028     C                   MOVEL     PA3CTR        VI6CTR
266300931028     C                   ELSE
266400931028     C                   MOVEL     '000'         VI6CTR
266500931028     C                   ENDIF
266600121221      *
266700931028     C* RIEMETTO I PIEDI DEL VIDEO
266800931019     C                   WRITE     FRB8D07
266900931028     C*
267000000622   X2C                   ELSE
267100931019     C* CONTROLLO
267200931105     C                   TESTN                   VI6CTR               30
267300931105     C     *IN30         IFEQ      *OFF
267400931105     C                   SETON                                        4290
267500931105     C                   GOTO      ENDCT6
267600931105     C                   ENDIF
267700931105     C*
267800931019     C                   MOVEL     VI6CTR        W01CTR
267900121221      *
268000121221      * Controlla la Tariffa
268100931019     C                   EXSR      CTRTAR
268200121221      *
268300000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
268400000622     C* POSTE O BARTOLINI
268500000622     C     *IN90         IFEQ      *OFF
268600000622     C*
268700000622     C     VH6POS        IFEQ      'S'
268800000622     C* E LA TARIFFA È BARTOLINI ERRORE
268900000622     C     FGTFTP        ANDNE     'O'
269000000623     C* ED ESISTE UNA TARIFFA BARTOLINI
269100000623     C     VH4CTB        ANDNE     '   '
269200040622     C*
269300040622     C*  fino al giorno 2004/08/10 il controllo è attivato
269400040622     c                   if        dataoggi < finoadata
269500000622     C                   SETON                                        9054
269600000622     C                   GOTO      ENDCT6
269700040622     c                   end
269800040622     C*
269900000622     C                   ENDIF
270000000622     C*
270100000622     C     VH6POS        IFNE      'S'
270200000622     C* E LA TARIFFA È BARTOLINI ERRORE
270300000622     C     FGTFTP        ANDEQ     'O'
270400000623     C* ED ESISTE UNA TARIFFA POSTE
270500000623     C     VH4CTP        ANDNE     '   '
270600000622     C                   SETON                                        9055
270700000622     C                   GOTO      ENDCT6
270800000622     C                   ENDIF
270900000622     C*
271000000622     C                   ENDIF
271100931021     C*
271200931021    3C     *IN90         IFEQ      *ON
271300931021    4C     *IN30         IFEQ      *OFF
271400000622     C                   SETON                                        40
271500931021   X4C                   ELSE
271600931021     C*
271700931021     C                   MOVEL     VI6CTR        WALF1             1
271800931021     C                   MOVEL     'TR'          COD
271900931021     C                   MOVEL     *BLANKS       KEY
272000931021     C                   MOVEL     WALF1         KEY
272100931021     C     KTAB          CHAIN     TABEL                              30
272200931021     C*
272300931021    5C     *IN30         IFEQ      *ON
272400931021     C     *IN30         OREQ      *OFF
272500931021     C     TBLFLG        ANDNE     ' '
272600931021     C                   SETON                                        42
272700931021   X5C                   ELSE
272800931021     C                   SETOFF                                       90
272900931021    5C                   ENDIF
273000931021    4C                   ENDIF
273100931021    3C                   ENDIF
273200121221     C*
273300000622    2C                   ENDIF
273400121221      *-----
273500121221     c                   endIF
273600121221      *-----
273700931022     C   90              GOTO      ENDCT6
273800020315     C* CONTROLLO NUMERO STOP: NON DEVE ESSERE > O = 9000
273900020315      * attenzione 99999 ha un significato ben preciso come STOP in quanto significa
274000020315      *  consegnato a magazzino.
274100020315     C     VI6STP        IFGE      9000
274200020419     C     VI6STP        oreq      0
274300950711     C                   SETON                                        4890
274400950711     C                   GOTO      ENDCT6
274500950711     C                   END
274600011109     C* >>>>>>>>>
274700011112      *
274800011109     c     tass_boldet   tag
274900011112      *
275000011112      *  flag per determinare da dove è richiamata la tassazione bolle
275100011112      *   se con la "T" oppure con l'"F2" dalla windowsfl in tal caso
275200011112      *    deve preparare i campi della window a ricevere i valori dalla
275300011112      *     tassazione automatica.
275400011112     c                   movel     ' '           bol_tassa         1
275500011109     C* >>>>>>>>>
275600011107     c* D e t t a g l i o    V a l o r i    s u    W i n d o w S f l
275700011107    2c     vi6sce        ifeq      'D'
275800011107     c                   movel     ' '           vi6sce
275900011108     c                   exsr      WndwSfl
276000011107     c                   endif
276100011107     C* >>>>>>>>>
276200931019     C*
276300931123     C* T A S S A Z I O N E   S I N G O L A   B O L L A
276400931022    2C     VI6SCE        IFEQ      'T'
276500931123     C                   MOVEL     ' '           VI6SCE
276600011108      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
276700931029     C* SOLO SE UNO DEI 2 IMPORTI E' A 0(E NON INSERITO IN PREC VIDEO)
276800011107      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
276900011105     C*
277000011109    3C     VI6ITT        IFEQ      0
277100011109     C     *IN15         ANDEQ     *OFF
277200011109     C     VI6ITA        OREQ      0
277300011109     C     *IN16         ANDEQ     *OFF
277400931029     C*
277500020122     C                   CLEAR                   FICNB2
277600020426     C                   MOVEL     'CALCVAL'     §TApgm
277700020213     C                   MOVEL     'S'           §TAtsm
277800020326     C                   MOVEL     vi4pdr        §TAfgs
277900020326     C                   MOVEL     VI4PDR        §TAPDR
278000950117     C                   MOVEL     VH4TSR        §TATSR                         CONSEGNA/RITIRO
278100931029     C                   MOVEL     PARSML        §TASML
278200020326     C                   MOVEL     vh4DDC        §TADDC
278300020326     C                   z-add     vi4ndc        §tandc
278400931202     C                   Z-ADD     1             §TASPP
278500950118     C                   Z-ADD     VH6SET        §TASET
278600950118     C                   MOVE      'S'           §TAPAG
278700020129     C                   z-add     vh6aas        §taaas
278800020129     C                   z-add     ftdlnp        §talnp
278900020129     C                   z-add     ftdnrs        §tanrs
279000020129     C                   z-add     vi6nsp        §tansp
279100950118     C* Tot.colli per piking
279200950118     C     VH4TSR        IFEQ      'C'
279300950118     C* Spedizione da non pagare
279400950119     C     VH6FIA        IFEQ      'B'
279500950118     C                   MOVE      *ZEROS        §TANCL
279600950118     C                   MOVE      *BLANKS       §TAPAG
279700950118     C                   ELSE
279800950118     C                   Z-ADD     FTDNCL        §TANCL
279900950118     C                   END
280000950118     C                   Z-ADD     FTDNCL        §TACPE
280100950118     C                   ELSE
280200950118     C* Tot.colli per etichettatura
280300950118     C                   Z-ADD     FTDNCL        §TANCL
280400950118     C                   Z-ADD     VH6NCE        §TACPE
280500950118     C                   END
280600950127     C                   MOVEL     VI4PEP        §TAPPE
280700021211     C                   movel     vi4FCS        §taFCS
280800950116     C                   Z-ADD     FTDPKL        §TAKFA
280900950116     C                   Z-ADD     FTDVLU        §TATVL
281000931029     C*
281100950118     C                   Z-ADD     VH6IFP        §TAIFP
281200011107     c                   movel     vi6div        §tadiv
281300950118     C                   MOVEL     VI6TSP        §TATSP
281400030320     C                   MOVEL     Vh6cbo        §TAcbo
281500950116     C                   MOVEL     VI6FTC        §TATC1
281600950116     C                   MOVEL     VI6FT2        §TATC2
281700950131     C                   MOVEL     VI6CAP        §TACAP
281800950118     C                   MOVEL     FTDFIN        §TAFIN
281900950118     C                   MOVEL     VH6FGC        §TAFGC
282000950118     C                   MOVEL     VH6GVA        §TAGVA
282100950118     C                   MOVEL     VH6SIC        §TASIC
282200931029     C                   MOVEL     VI6CTR        §TACTR
282300931029    5C     *IN15         IFEQ      *ON
282400950118     C     VH6SIC        OREQ      'S'
282500931029     C                   MOVEL     *HIVAL        §TAITT
282600931029     C                   ELSE
282700011109     C                   Z-ADD     VI6ITT        §TAITT
282800931029    5C                   ENDIF
282900931029     C*
283000931029    5C     *IN16         IFEQ      *ON
283100950119     C     VH6FIA        OREQ      'B'
283200931029     C                   MOVEL     *HIVAL        §TAITA
283300931029     C                   ELSE
283400011109     C                   Z-ADD     VI6ITA        §TAITA
283500931029    5C                   ENDIF
283600021212      *
283700050909     C* Comunque aggancia sempre la bolla in partenza x vedere
283800050909      *  il codice trattamento merce
283900050909     C                   clear                   DS1B
284000091012     c     bolla_ftd     chain     fnblp01l
284100050909     c                   if        %found(fnblp01l)
284200050909     C                   MOVE      blpCbo        §tacbo
284300050909     C                   MOVEL     '1B'          COD
284400050909     C                   MOVEL     *BLANKS       KEY
284500050909     C                   MOVEL     blpCTM        KEY
284600050909     C     KTAB          CHAIN     TABEL00F                           30
284700050909     c                   if        %found(TABEL00F)
284800050909     C                   MOVEL     TBLUNI        DS1B
284900021212      * ------------
285000050909      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
285100050909      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
285200050909      *   la tariffa di Etichettatura in generale.
285300050909      *
285400050909      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
285500050909      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
285600050909     c                   iF        ftdTSR = 'R'
285700050909     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
285800050909     c                   move      'N'           §taPPE
285900050909     c                   end
286000050909      * Se è un DISKC Bartolini la regola dice che deve essere comunque
286100050909      * pagata la spedizione al Padroncino
286200050909     c                   iF        §1Bdkc ='B' and ftdnrs = 0
286300050909     c                   eval      §taPPE = 'S'
286400050909     c                   eval      §tadkc = 'S'
286500050909     c                   end
286600050909     c                   end
286700050909      **
286800050909     c                   endiF
286900050909     c                   endiF
287000021212      * ------------
287100950131     C*
287200931124     C     §TAITA        IFEQ      0
287300931124     C     VH4TSR        ANDEQ     'C'
287400950131     C                   Z-ADD     VH6CAS        §TACAS
287500950131     C                   MOVEL     VH6VCA        §TAVCA
287600950131     C                   Z-ADD     VH6AAS        FTDAAS
287700950131     C                   Z-ADD     VI6NSP        FTDNSP
287800020129     C*
287900950131     C* CONTROLLO SE E' C/A PRONTAMENTE RECUPERATO
288000931124     C                   EXSR      CTRASS
288100931124     C                   ENDIF
288200090909     C*
288300090909     C*   Deve passare il codice del cliente solo se è un cliente particolare
288400090909     c     vh6KSC        lookup    KcliPAR                                30
288500090909     c                   if        *in30
288600090909     C                   Z-ADD     vh6KSC        §TAcliPAR
288700090909     C                   end
288800931029     C*
288900020122     C                   CALL      'FICNB2R'
289000020122     C                   PARM                    FICNB2
289100931029     C*
289200011107     C* di ritorno dall'elaborazione:
289300011107     C*
289400931029    5C     §TAERR        IFEQ      ' '
289500011107     c                   z-add     0             vi6tot
289600931108     C     *IN15         IFEQ      *OFF
289700950118     C     VH6SIC        ANDNE     'S'
289800011112      *
289900931108     C                   MOVEL     §TAITT        VH6ITT
290000931108     C                   ADD       §TATPE        VH6ITT
290100931108     C                   ADD       §TATST        VH6ITT
290200011107      * >>>>>>> su campo SFLWDW 1° riga
290300011109     C                   Z-ADD     VH6ITT        VI6ITT
290400011107     c                   add       vh6itt        vi6tot
290500011112      *
290600931108     C                   ENDIF
290700011112      *
290800950118     C     *IN16         IFEQ      *OFF
290900950119     C     VH6FIA        ANDNE     'B'
291000011112      *
291100011112     C                   MOVEL     §TAITA        VH6ITA
291200011112     C                   MOVEL     §TAITA        VI6ITA
291300011112      * >>>>>>> su campo SFLWDW 2° riga
291400011112     c                   add       vh6ita        vi6tot
291500011112      *
291600950118     C                   END
291700011112      *
291800931029   X5C                   ELSE
291900931029     C* ERRORE IN TASSAZIONE
292000931019     C                   SETON                                        4190
292100931022     C                   GOTO      ENDCT6
292200931029    5C                   ENDIF
292300931029    3C                   ENDIF
292400931029    2C                   ENDIF
292500011109     c*>>>>>>>>>>>>>
292600011109     c* se richiesta a tassazione dal dettaglio nella windsfl
292700011109     c*  deve tornare sul dettaglio della windowsfl.
292800011109     c                   if        bol_tassa = 'D'
292900011109     c                   eval      vi6sce = 'D'
293000011109     c                   goto      tass_boldet
293100011109     c                   endif
293200011109     c*>>>>>>>>>>>>>
293300011109     C*
293400931123     C* SE  S O M M A   P E S I   A   G I O R N A T A
293500931123    2C     VH4TSM        IFEQ      'G'
293600011109     C     VI6ITT        ANDEQ     0
293700950119     C     VH6FIA        ANDNE     'B'
293800950118     C     VH6SIC        ANDNE     'S'
293900931123     C                   MOVEL     VI6CTR        WALF1
294000931123     C* NON ACCETTO PIU' DI 1 COD TAR A KILO O A QUINTALE
294100931123     C*  OPPURE DEVO VALORIZZARE SUBITO LA BOLLA
294200931123    3C     WALF1         IFEQ      '0'
294300931123     C     WALF1         OREQ      '3'
294400931123     C*
294500931123    4C     WALF3         IFEQ      *BLANKS
294600931123     C                   MOVEL     VI6CTR        WALF3
294700931123   X4C                   ELSE
294800931123    5C     VI6CTR        IFNE      WALF3
294900931123     C                   SETON                                        4390
295000931123     C                   GOTO      ENDCT6
295100931123    5C                   ENDIF
295200931123    4C                   ENDIF
295300931123     C*
295400931123    3C                   ENDIF
295500931123    2C                   ENDIF
295600931019     C*
295700931123     C* V I S U A L I Z Z A Z I O N E   S I N G O L A   B O L L A
295800931022    2C     VI6SCE        IFEQ      'V'
295900931123     C                   MOVEL     ' '           VI6SCE
296000931123    3C     VH4TSR        IFEQ      'C'                                           CONSEGNE
296100931021     C                   MOVEL     VH6AAS        PA2AAS
296200931021     C                   MOVEL     FTDLNP        PA2LNP
296300931021     C                   MOVEL     FTDNRS        PA2NRS
296400931207     C                   MOVEL     VI6NSP        PA2NSP
296500931105     C                   MOVEL     '1'           PA2FLG
296600100513     C                   MOVEL     KPJBU         kpjbus
296700100513     C                   clear                   kpjbu
296800931021     C                   MOVEL     PARAM2        KPJBU
296900950116     C                   CALL      'FNLR36R'
297000931021     C                   PARM                    KPJBA
297100100513     C                   MOVEL     KPJBUs        kpjbu
297200931021     C*
297300931021     C                   WRITE     FRB8D07
297400931022     C                   SETON                                        90
297500931123     C                   GOTO      ENDCT6
297600931123    3C                   ENDIF
297700020326     C*
297800020326    3C     VH4TSR        ifeq      'R'                                           ritiri nuovi
297900020326    3C     vh6nsp        andgt     0
298000020326     C                   CLEAR                   DSLS05
298100020326     C                   MOVE      VH6AAS        PA0AAS
298200020326     C                   MOVE      FTDLNP        PA0LNP
298300020326     C                   MOVE      FTDNRS        PA0NRS
298400020326     C                   MOVE      VI6NSP        PA0NSP
298500020326     C                   MOVEL     RAGUT         PA0RSU
298600020326     C                   MOVE      SIMFEL        PA0SIM
298700100513     C                   MOVEL     KPJBU         kpjbus
298800100513     C                   clear                   kpjbu
298900020326     C                   MOVEL     DSLS05        KPJBU
299000020326     C                   CALL      'FNLS05R'
299100020326     C                   PARM                    KPJBA
299200100513     C                   MOVEL     KPJBUs        kpjbu
299300020326     C*
299400020326     C                   WRITE     FRB8D07
299500020326     C                   SETON                                        90
299600020326     C                   GOTO      ENDCT6
299700020326    3C                   end
299800020326     C*
299900931123    2C                   ENDIF
300000931019     C*
300100931022     C     ENDCT6        TAG
300200931105     C* BLOCCATO
300300950119     C     VH6FIA        IFEQ      'B'
300400931105     C                   SETON                                        13
300500931105     C                   ELSE
300600931105     C                   SETOFF                                       13
300700931105     C                   ENDIF
300800950117     C* RITORNO ALL'INCASSO
300900950117     C     VH6SIC        IFEQ      'S'
301000950117     C                   SETON                                        23
301100950117     C                   ELSE
301200950117     C                   SETOFF                                       23
301300950117     C                   ENDIF
301400931105     C*
301500931019     C                   Z-ADD     NRR6          REC6
301600931019     C                   UPDATE    FRB8SF6
301700931019     C*
301800931019     C   90              SETON                                        32
301900931019     C  N90              READC     FRB8SF6                                32
302000931019    1C                   ENDDO
302100931203     C*
302200931203     C*  SE: 90 OFF - NO ERRORE
302300931203     C*             - SE TROVATO ALMENO UN CODICE TARIFFA 0XX O 3XX
302400931203     C*             - TIPO SOMMA PESI A GIORNATA
302500931203     C*             - NO VARIATO TUTTE LE RIGHE DELLA DISTINTA
302600931203    1C     *IN90         IFEQ      *OFF
302700931203     C     WALF3         ANDNE     *BLANKS
302800931203     C     VH4TSM        ANDEQ     'G'
302900931203     C     WREAD         ANDNE     WFTD
303000931203     C*
303100931203     C                   MOVEL     W01CTP        WALF1
303200931203    2C     WALF1         IFEQ      '0'
303300931203     C     WALF1         OREQ      '3'
303400931203     C*
303500931203    3C     W01CTP        IFNE      WALF3
303600931203     C                   SETON                                        4390
303700931203     C                   CLEAR                   WALF3
303800931203     C     1             CHAIN     FRB8SF6                            30
303900931203     C                   GOTO      ENDCT6
304000931203    3C                   ENDIF
304100931203    2C                   ENDIF
304200931203    1C                   ENDIF
304300931203     C*
304400931021     C                   SETOFF                                       36
304500931019     C*
304600931021     C                   ENDSR
304700020212     C*---------------------------------------------------------------*
304800011108     C* Emette la finestra con il dettaglio degli importi tariffe ----*
304900020212     C*---------------------------------------------------------------*
305000011108     c     WndwSfl       begsr
305100011108     c*
305200011108     c     *like         define    w6ctot        wwctot
305300011108     c*
305400011108     c                   seton                                        35
305500011108     c                   write     frb8wc6
305600011123     c                   setoff                                       3561
305700011108     c*
305800011108     c                   clear                   nrw6
305900011108     c                   z-add     1             wrc6
306000011108     c                   z-add     ftdlnp        w6clnp
306100011108     c                   z-add     ftdnrs        w6cnrs
306200011218     c                   z-add     vi6nsp        w6cnsp
306300011108     c                   move      vi6ftc        w6cftc
306400011108     c                   move      vi6ft2        w6cft2
306500011108     c                   move      vi6con        w6ccon
306600011108     c                   move      vi6ia         w6cin
306700011108     c                   move      vi6div        w6cdiv
306800011108     c                   move      vi6cap        w6ccap
306900011108     c                   move      ftdfin        w6cfin
307000011108     c                   z-add     ftdncl        w6cncl
307100011108     c                   z-add     ftdpkl        w6cpkl
307200011108     c                   z-add     ftdvlu        w6cvlu
307300011108     c                   move      vi6tot        w6ctot
307400020128     c                   z-add     vi6itt        w6cbas
307500020128     c                   z-add     vi6ita        w6cacc
307600011109     c*------->
307700020327     c* Carica le 2 righe x importo Base e Accessori
307800020327     c                   do        *hival
307900020327     c* protect/no display
308000020327     c                   setoff                                       60
308100020327     c* riga del sfl
308200020327     c                   add       1             nrw6
308300020327     c*  deve caricare solo 2 righe
308400020327     c                   if        nrw6 > 2
308500020327     c                   leave
308600020327     c                   endif
308700020327     c*
308800020327     c                   z-add     nrw6          w1htrc
308900020327     c*
309000020327     c                   select
309100020327     c     nrw6          wheneq    1
309200020327     c     vp6itt        comp      '1'                                    60
309300020327     c                   movel     *blank        w1tipo
309400020327     c                   movel     'Base'        w1tipo
309500020327     c                   z-add     vi6itt        w1valr
309600020327     c                   z-add     vi6itt        w1hval
309700020327     c*
309800020327     c     nrw6          wheneq    2
309900020327     c     vp6ita        comp      '1'                                    60
310000020327     c                   movel     *blank        w1tipo
310100020327     c                   movel     'Accessori'   w1tipo
310200020327     c                   z-add     vi6ita        w1valr
310300020327     c                   z-add     vi6ita        w1hval
310400020327     c                   endsl
310500020327     c*
310600020327     c                   write     frb8ws6
310700020327     c                   enddo
310800011109     c*-------> Emissione Window
310900011108     c     resta_wdw     tag
311000011109      *                *----------------------*
311100011108     c                   write     frb8wd6
311200011108     c                   exfmt     frb8wc6
311300011109      *                *----------------------*
311400011123     C                   setoff                                       61
311500011108     c*  Controlli
311600011109      * ---------         ----------
311700020327     c* Cntrl le righe x importi modificati
311800020327     c                   z-add     0             nrw6
311900020327     c                   z-add     0             wwctot
312000020327      *------------        ---------------
312100020327     c* cicla sulle righe
312200020327     c                   do        *hival
312300020327     c                   add       1             nrw6
312400020327     c*
312500020327     c*  deve caricare solo 2 righe
312600020327     c                   if        nrw6 > 2
312700020327     c                   leave
312800020327     c                   endif
312900020327     c*
313000020327     c     nrw6          chain     frb8ws6
313100020327      *  totalizza le righe
313200020327     c                   add       w1valr        wwctot
313300020327     c* protect/no display
313400020327     c                   setoff                                       60
313500020327      *  campi protetti
313600020327     c     nrw6          ifeq      1
313700020327     c     vp6itt        comp      '1'                                    60
313800020327     c                   end
313900020327     c     nrw6          ifeq      2
314000020327     c     vp6ita        comp      '1'                                    60
314100020327     c                   end
314200020327      *
314300020327      *  x F8 -> Conferma
314400020327      *  valori modificati manualmente
314500020327     c   KB
314600020327     cor KH              IF        w1hval <>  w1valr
314700020327      *
314800020327     c                   MOVEL     ' '           flgfi
314900020327    3c                   if        w1valr > 0
315000020327     c                   MOVEL     'M'           flgfi             1
315100020327    3c                   end
315200020327      *
315300020327     c                   Select
315400020327      * Base
315500020327     c     nrw6          wheneq    1
315600020327     C                   movel     flgfi         VH6FIT
315700020327     C                   MOVEL     w1valr        vi6itt
315800020327     C                   MOVEL     vi6itt        VH6ITT
315900020327      * Accessori
316000020327     c     nrw6          wheneq    2
316100020327     C                   movel     flgfi         VH6FIA
316200020327     C                   MOVEL     w1valr        vi6ita
316300020327     C                   MOVEL     vi6ita        VH6ITA
316400020327      *
316500020327     c                   endsl
316600020327      *
316700020327     C                   z-add     w1valr        w1hval
316800020327     c                   endif
316900020327      *
317000020327     C                   move      w1valr        trebytes          3 0
317100020327     C                   if        w6cdiv ='ITL' and
317200020327     c                             trebytes > 0
317300020327     c                   seton                                        61
317400020327     c                   end
317500020327      *
317600020327     c                   update    frb8ws6
317700020327     c   61              leave
317800020327     c                   enddo
317900020327      * ---------         ----------
318000020327     c* resta sulla window se non ha premuto dei cmd
318100020327     c     *inkh         ifeq      *off
318200020327     c     *inkl         andeq     *off
318300020327     c     *inkb         andeq     *off
318400020327     c     *in61         oreq      *on
318500020327     c     *inkl         andeq     *off
318600020327     c                   goto      resta_wdw
318700020327     c                   end
318800020327      *
318900020327      * reimposta il totale delle singole righe
319000020327     c     *inkh         ifeq      *on
319100020327     c                   if        wwctot <>  vi6tot
319200020327     c                   z-add     wwctot        vi6tot
319300020327     c                   endif
319400020327     c                   end
319500020327     c*
319600020327     c*  Se richiesta la tassazione bolla in automatico
319700020327     c*   come se avesse chiesto "T" sulla scelta del SFL principale
319800020327     c*    e successivamente "D" di dettaglio.
319900020327     c     *inkb         ifeq      *on
320000020327     c                   move      'T'           vi6sce
320100020327     c                   move      'D'           bol_tassa
320200020327     c                   end
320300020327     c*
320400011109     c*
320500011108     c                   endsr
320600020212     c*---------------------------------------------------------------*
320700011108     C* VEDO SE E' C/A PRONTAMENTE RECUPERATO ------------------------*
320800020212     C*---------------------------------------------------------------*
320900011108     C     CTRASS        BEGSR
321000011108     C*
321100950131     C                   MOVE      *ZEROS        §TACA1
321200950131     C                   MOVE      *BLANKS       WTACPC            1
321300931124     C* SOLO SE INCASSATO IL C/A
321400950131    1C     §TACAS        IFGT      0
321500931124     C*
321600931124     C                   CLEAR                   WNUM3
321700931124     C                   SETOFF                                       09
321800931124     C*
321900931124     C* PER ESEGUIRLO EVENTUALEMTE 2 VOLTE
322000931124    2C     WNUM3         DOWLE     2
322100931124     C                   ADD       1             WNUM3
322200931124     C*
322300931124    3C     *IN09         IFEQ      *OFF                                          CON FTD
322400091012     C     Bolla_FTD     SETLL     FNARBK00
322500091012     C     Bolla_FTD     READE     FNARBK00                               30
322600931124   X3C                   ELSE
322700950116     C     KLBL          SETLL     FNARBK00                                      CON MAMMA
322800950116     C     KLBL          READE     FNARBK00                               30
322900931124    3C                   ENDIF
323000931124     C*
323100931124    3C     *IN30         DOWEQ     *OFF
323200931124     C*
323300931124     C* SE INSERITO COMPLETAMENTE OK
323400030418    4C     kARBCVB       IFEQ      'K5'
323500950131     C                   Z-ADD     §TACAS        §TACA1
323600950131     C                   MOVEL     'S'           WTACPC
323700931124     C                   SETON                                        30
323800931124   X4C                   ELSE
323900931124     C*
324000931124     C* SE MODIFICATO IMPORTO L'ASSEGNO SOLTANTO SE > 0 E PER LA DIFFER
324100030418    5C     kARBCVB       IFEQ      'KP'
324200931124     C                   SETON                                        30
324300030418     C     §TACAS        SUB       kARBCAS       §TACA1
324400950131    6C     §TACA1        IFLE      0
324500950131     C                   Z-ADD     0             §TACA1
324600931124   X6C                   ELSE
324700950131     C                   MOVEL     'S'           WTACPC
324800931124    6C                   ENDIF
324900931124    5C                   ENDIF
325000931124    4C                   ENDIF
325100931124     C*
325200950116     C  N30
325300091012     CANN09Bolla_FTD     READE     FNARBK00                               30
325400950116     C  N30
325500950116     CAN 09KLBL          READE     FNARBK00                               30
325600931124    3C                   ENDDO
325700931124     C*
325800931124     C* SE NON TROVATO NULLA GUARDO SE HA UNA MAMMA E CERCO SULLA MAM
325900950131    3C     WTACPC        IFEQ      ' '
326000931124     C     WNUM3         ANDEQ     1
326100931124     C*
326200091012     C     Bolla_FTD     SETLL     FNLBL000                               30
326300931124    4C     *IN30         IFEQ      *OFF
326400030418     C* per uscire dal ciclo
326500950119     C                   Z-ADD     3             WNUM3             3 0
326600931124   X4C                   ELSE
326700931124     C                   ADD       1             WNUM3
326800091012     C     Bolla_FTD     CHAIN     FNLBL000                           30
326900030418     C* deve eseguire solo se le linee di arrivo sono uguali fra madre e figlia
327000030418     C*  altrimenti forza l'uscita dal ciclo
327100030418    4c  n30              if        lbllan <> lbllap
327200030418     c                   leave
327300030418     c                   end
327400931124     C                   SETON                                        09
327500931124    4C                   ENDIF
327600931124     C*
327700931124     C                   ELSE
327800030418     C* per uscire dal ciclo
327900931124     C                   Z-ADD     3             WNUM3
328000931124    3C                   ENDIF
328100931124    2C                   ENDDO
328200931124    1C                   ENDIF
328300931029     C                   ENDSR
328400020212     C*---------------------------------------------------------------*
328500950116     C* AGGIORNAMENTO BOLLE DISTINTA/RITIRO --------------------------*
328600020212     C*---------------------------------------------------------------*
328700931021     C     AGGIO6        BEGSR
328800931021     C                   Z-ADD     1             NRR6
328900931021     C                   READC     FRB8SF6                                32
329000931109     C*
329100931109     C* SE GIA' VALORIZZATE AGGANCIO LA TESTATA
329200931112     C     VI1FVL        IFEQ      'V'
329300931123     C* SE SOMMA PESI A GIORNATA DEVO RICONFERMARE LA VALUTAZIONE
329400931123     C  N32VH4TSM        IFEQ      'G'
329500931123     C                   SETON                                        22
329600931123     C                   ENDIF
329700011105     C     KFTD          CHAIN     FIFTT000                           30
329800931109     C                   ENDIF
329900931021     C*
330000931021    1C     *IN32         DOWEQ     *OFF
330100020313      *
330200020313      * per distinguere il vecchio modo di lettura dei ritiri non è più possibile
330300020313      * utilizzare l'ind.19 ma il campo numero spedizione.
330400020313     c                   if        vh6nsp>0
330500020315     c                   if        vh4tsr='R'
330600020315      * vista per i nuovi ritiri x spedizione
330700020404     C     KFTD5         CHAIN     fiftd005                           32
330800020315     c                   else
330900020315      * seleziona solo i consegnati
331000020313     C     KFTD2         CHAIN     fiftd002                           30
331100020315     c                   end
331200020313     c                   else
331300020313     C     KFTD4         CHAIN     fiftd004                           30
331400020313     c                   end
331500931112     C*
331600020313    2C     *IN30         IFEQ      *OFF
331700931109     C*
331800931109     C* SE GIA' VALORIZZATE DEVO AGGIORNARE I CAMPI DI TOTALE
331900931109     C     VI1FVL        IFEQ      'V'
332000950118     C  N15VH6SIC        IFNE      'S'
332100931109     C                   SUB       FTDITT        FTTITT
332200011109     C                   ADD       VI6ITT        FTTITT
332300931110     C                   SETON                                        22
332400931109     C                   ENDIF
332500931110     C*
332600950119     C  N16VH6FIA        IFNE      'B'
332700931109     C                   SUB       FTDITA        FTTITA
332800011109     C                   ADD       VI6ITA        FTTITA
332900950118     C                   SETON                                        22
333000931109     C                   ENDIF
333100931109     C                   ENDIF
333200931109     C*
333300931029     C* SE IMPOSTATO VALORE DISTINTA BASE AZZERO SULLA BOLLA
333400931029     C     *IN15         IFEQ      *OFF
333500011109     C                   MOVEL     VI6ITT        FTDITT
333600931021     C                   MOVEL     VH6FIT        FTDFIT
333700931029     C                   ENDIF
333800931029     C* SE IMPOSTATO VALORE DISTINTA ACCESSORI AZZERO SULLA BOLLA
333900931029     C     *IN16         IFEQ      *OFF
334000011109     C                   MOVEL     VI6ITA        FTDITA
334100931021     C                   MOVEL     VH6FIA        FTDFIA
334200931029     C                   ENDIF
334300011030     C     VI6CTR        IFNE      VI4CTD
334400011106     c     vh4tpr        andeq     1
334500931022     C                   MOVEL     VI6CTR        FTDCTP
334600931109     C                   ENDIF
334700011123     C                   z-add     VI6STP        FTDSTP                          STOP
334800020326     C*
334900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
335000020326     C* i primi tre caratteri
335100020326     c                   if        ftdfgs = 0
335200020326     c                   movel     ftdpdr        ftdfgs
335300020326     c                   end
335400020326     C*
335500020313     c                   if        vh6nsp > 0
335600020315     c                   if        vh4tsr='R'
335700020315     C                   UPDATE    fiftd005
335800020315     c                   else
335900020313     C                   UPDATE    fiftd002
336000020315     c                   end
336100020313     c                   else
336200020313     C                   UPDATE    fiftd004
336300020313     c                   end
336400020313     C*
336500931021    2C                   ENDIF
336600931021     C                   READC     FRB8SF6                                32
336700931021    1C                   ENDDO
336800931021     C*
336900931123     C* RIAGGIORNO LA TESTATA (PER EVITARE CHE NON SIA AGGIORNATA SE
337000931109     C*  ESCONO CON CMD12)
337100931109     C     VI1FVL        IFEQ      'V'
337200020326     C*
337300020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
337400020326     C* i primi tre caratteri
337500020326     c                   if        fttfgs = 0
337600020326     c                   movel     fttpdr        fttfgs
337700020326     c                   end
337800020326     C*
337900011105     C                   UPDATE    FIFTT000
338000931109     C                   ENDIF
338100931109     C*
338200931021     C                   ENDSR
338300020212     C*---------------------------------------------------------------*
338400931019     C* CONTROLLO CODICE TARIFFA -------------------------------------*
338500020212     C*---------------------------------------------------------------*
338600931019     C     CTRTAR        BEGSR
338700091112     C*
338800931123     C                   Z-ADD     1             C
338900931123     C     VH4TSR        LOOKUP    TSR(C)                                 30
339000931123     C  N30              Z-ADD     1             C
339100931123     C*
339200931123     C* SE E' IL CODICE TARIFFA GENERICO --> ERRORE
339300931123    1C     W01CTR        IFEQ      CTS(C)
339400931123     C                   SETON                                        90
339500931123   X1C                   ELSE
339600931123     C*
339700011112     C     KFGT2         CHAIN     FIFGT000                           30
339800011112     C*
339900011112     C* Modificato con le nuove regole di data scadenza e valuta
340000011112     C*  il modo di reperire il record valido.
340100011112    2C     *in30         doweq     *off
340200091112      *
340300011112     C     fgtatb        ifne      'A'
340400011112     C     w02div        andeq     fgtdiv
340500091112      *
340600091112     C* Se tariffa del periodo
340700091112     C     Ktgt          CHAIN     FItgt01l
340800091112 2/3 c                   if        %Found(FItgt01l) and
340900091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
341000091112      *
341100091112    3c     parsml        IFeq      *blank
341200091112     C     tgtDTS        aNDgt     0
341300091112     c     parsml        OReq      'S'
341400091112     C                   leave
341500091112    3C                   end
341600091112      *
341700091112 2/3 C                   end
341800091112      *
341900091112    2C                   end
342000091112      *
342100011112     C     kfgt2         reade     fifgt000                               30
342200011112    2C                   enddo
342300931019     C*
342400091112     C*  ????????????  strano test se trovato ritesta la data se minore della decorrenza ?????
342500931123    2C     *IN30         IFEQ      *ON
342600931019     C     *IN30         OREQ      *OFF
342700091112     C     W02DDC        ANDLT     tgtDDT
342800931019     C                   SETON                                        90
342900931123    2C                   ENDIF
343000091112     C*
343100931123    1C                   ENDIF
343200931021     C*
343300931021     C* 90 ON - TARIFFA INESISTENTE
343400931028     C*         VEDO SE HA ALMENO UN CODICE TARIFFA VALIDO
343500931021    1C     *IN90         IFEQ      *ON
343600931108     C* CERCO IL CODICE TARIFFA GENERICO DEL PRESTAZ DA ESCLUDERE
343700931028     C                   SETOFF                                       31
343800931028     C*
343900011112     C     KFGT          CHAIN     FIFGT000                           30
344000091112      *
344100091112     C*   in testata di periodo
344200091112     C  n30Ktgt          CHAIN     FItgt01l
344300091112 1/2 c  n30              if        %Found(FItgt01l)
344400931028     C*
344500931028     C* SE ANNULLATO O E' IL CODICE TARIFFA GENERICO RILEGGO
344600011112      * o se non è della stessa divisa o se scaduto
344700931022    2C  N30FGTATB        IFEQ      'A'
344800931028     C     FGTCTR        OREQ      CTS(C)
344900011112     C     fgtdiv        ORne      w02div
345000091112     C     tgtdst        ORgt      w02ddc
345100091112     C     tgtDTS        oreq      0
345200091112     c     parsml        andeq     *blank
345300931021     C                   SETON                                        31
345400931028    2C                   ENDIF
345500091112 1/2 c                   end
345600931021     C*
345700931021    2C     *IN30         DOWEQ     *OFF
345800931021     C     *IN31         ANDEQ     *ON
345900931021     C*
346000011112     C     KFGT          READE     FIFGT000                               30
346100091112      *
346200091112     C*   in testata di periodo
346300091112     C  n30Ktgt          CHAIN     FItgt01l
346400091112 2/3 c  n30              if        %Found(FItgt01l)
346500091112     C*
346600931022    3C  N30FGTATB        IFEQ      'A'
346700931028     C     FGTCTR        OREQ      CTS(C)
346800011112     C     fgtdiv        ORne      w02div
346900091112     C     tgtdst        ORgt      w02ddc
347000091112    3C     tgtDTS        oreq      0
347100020315    3c     parsml        andeq     *blank
347200931021     C                   SETON                                        31
347300931028     C                   ELSE
347400931028     C                   SETOFF                                       31
347500931021    3C                   ENDIF
347600931021     C*
347700091112 2/3 c                   end
347800091112     C*
347900931021    2C                   ENDDO
348000931021    1C                   ENDIF
348100931028     C*
348200931028     C* 90 ON 30 ON - NON ESISTONO TARIFFE PER IL CLIENTE
348300931021     C*
348400931019     C                   ENDSR
348500020212     C*---------------------------------------------------------------*
348600931111     C* CONTROLLO SE MANCATA CONSEGNA DA TASSARE ---------------------*
348700020212     C*---------------------------------------------------------------*
348800931111     C     CTRCMC        BEGSR
348900950116     C                   MOVE      VI4FIL        WALF6
349000931111     C     WALF6         LOOKUP    CMC                                    30
349100931111     C* SE INESISTENTE CERCO CON FILIALE A 0
349200931111     C     *IN30         IFEQ      *OFF
349300931111     C                   MOVE      000           WALF6
349400931111     C     WALF6         LOOKUP    CMC                                    30
349500931111     C                   ENDIF
349600931111     C                   ENDSR
349700020212     C*---------------------------------------------------------------*
349800931111     C* CONTROLLO CODICE BOLLA DA NON TASSARE ------------------------*
349900020212     C*---------------------------------------------------------------*
350000931111     C     CTRCBL        BEGSR
350100950116     C                   MOVEL     W018C         WALF6             6
350200950116     C                   MOVE      VI4FIL        WALF6
350300950116     C     WALF6         LOOKUP    CBL                                    31
350400931111     C* SE INESISTENTE CERCO CON FILIALE A 0
350500931111     C     *IN31         IFEQ      *OFF
350600950116     C                   MOVE      000           WALF6
350700950116     C     WALF6         LOOKUP    CBL                                    31
350800931111     C                   ENDIF
350900931111     C                   ENDSR
351000020212     C*---------------------------------------------------------------*
351100931021     C* ESEGUO VALORIZZAZIONE ----------------------------------------*
351200020212     C*---------------------------------------------------------------*
351300931021     C     VALZZ         BEGSR
351400931110     C* SPENGO IL 22 CHE OBBLIGA IL CMD6
351500931110     C                   SETOFF                                       22
351600931021     C                   Z-ADD     1             NRR4
351700931021    0C     NRR4          DOWLE     W04NRR
351800931021     C     NRR4          CHAIN     FRB8SF4                            32
351900931021     C*
352000931021    1C     *IN32         IFEQ      *OFF
352100931021     C*
352200931123     C* R E C O R D   D E L   T I P O   P R E S T A Z I O N E:
352300931029     C* ESCLUDO (MEMORIZZO IL COD.TARIFFA)
352400931021    2C     VH4NRR        IFEQ      0
352500011106     c                   if        vh4tpr = 1
352600011030     C                   MOVEL     VI4CTD        WALF3
352700011105     c                   end
352800000623     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
352900000623     C     VH4CTP        IFNE      *BLANKS
353000000623     C     VH4CTB        ANDNE     *BLANKS
353100000623     C                   MOVEL     VH4CTP        WCTP              3
353200000623     C                   MOVEL     VH4CTB        WCTB              3
353300000623     C                   ELSE
353400000623     C                   MOVEL     *BLANKS       WCTP
353500000623     C                   MOVEL     *BLANKS       WCTB
353600000623     C                   ENDIF
353700931029     C*
353800931021   X2C                   ELSE
353900931022     C*
354000931029     C* V A L O R I   B A S E   E   A C C E S S O R I   D I S T I N TA:
354100931029     C*  SE CI SONO ENTRAMBI NON FACCIO NULLA
354200931022    3C     VI4ITT        IFEQ      0
354300931022     C     VI4ITA        OREQ      0
354400931103     C*
354500931103     C* 15 ON - IMMESSA TARIFFA BASE
354600931103     C* 16 ON - IMMESSA TARIFFA ACCESSORI
354700931103     C     VI4ITT        COMP      0                                  15
354800931103     C     VI4ITA        COMP      0                                  16
354900931105     C* PULIZIA CAMPI DI TOTALE
355000931125     C                   CLEAR                   W01CLA
355100020212     C                   CLEAR                   g01CLA
355200950118     C                   CLEAR                   W01CPE
355300020212     C                   CLEAR                   g01CPE
355400931125     C                   CLEAR                   W01KFA
355500020212     C                   CLEAR                   g01KFA
355600931105     C                   CLEAR                   W01NCL
355700931105     C                   CLEAR                   W01TVL
355800020212     C                   CLEAR                   g01TVL
355900931105     C                   CLEAR                   W01SNA
356000020212     C                   CLEAR                   g01SNA
356100931105     C                   CLEAR                   W01STP
356200931105     C                   CLEAR                   W02STP
356300931123     C                   CLEAR                   W01CTP
356400931105     C                   CLEAR                   W01FIA
356500931105     C                   CLEAR                   W01FIT
356600931105     C                   CLEAR                   W01ITT
356700931105     C                   CLEAR                   W01ITA
356800950119     C                   CLEAR                   W01SNE
356900931202     C                   CLEAR                   W01SNP
357000931103     C*
357100931103     C* C O D I C E   T A R I F F A   I N E S I S T E N T E :
357200931103    4C     WALF3         IFEQ      '???'
357300931105     C*  VEDO SE DARE MANCA TARIFFA SOLO SE NON E' IMPOSTATA LA BASE
357400931105     C     *IN15         IFEQ      *OFF
357500931104     C                   MOVEL     SER(2)        W01ERR           25
357600931103     C                   EXSR      MANTAR
357700931105     C                   ENDIF
357800931103     C*
357900931103   X4C                   ELSE
358000931029     C*
358100931108     C     VH4TSR        IFEQ      'R'                                           RITIRI
358200931108     C                   SETON                                        19
358300931108     C                   ELSE
358400931108     C                   SETOFF                                       19         CONSEGNE
358500931108     C                   ENDIF
358600020325     C*
358700020325     C*  prima del ciclo di lettura di dettaglio resetta
358800020325     C*   i flags o i contatori
358900020325     c                   move      *blank        w01isola
359000020325     c                   move      *blank        w01xpres
359100030109     c                   move      *blank        w01disagiato      1
359200020325     c                   z-add     0             almuno_xpr
359300020325     c                   z-add     0             almuno_iso
359400030109     c                   z-add     0             almuno_disag
359500030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
359600030821     C*  lo stop.
359700030821     c                   z-add     0             W01nss
359800030821     c                   z-add     0             W01fw
359900030821     c                   z-add     0             W01rcc
360000030821     c                   move      *loval        savSTP
360100041124     C*
360200041124     C*   Comunque prima di valorizzare per sicurezza pulisce eventuali
360300041124     C*   records del FIFCE      *-------------*
360400041124     c                   exsr      Annulla_FCE
360500041124     C*                          *-------------*
360600931108     C*
360700011105     C     KFTD          SETLL     fiftd001
360800011105     C     KFTD          READE     fiftd001                               32
360900931021     C*
361000931104    5C     *IN32         DOWEQ     *OFF
361100020328     C*
361200030821     C*  a rottura di STOP conta quante spedizioni ci sono
361300030821     C*   all'interno dello stesso STOP
361400030821     c                   if        ftdSTP <> savSTP
361500030821     C*                          *-------------*
361600030821     c                   exsr      Sped_x_Stop
361700030821     C*                          *-------------*
361800030821     c                   eval         savSTP = ftdSTP
361900030821     c                   end
362000030821     C*
362100020422     C*               *--------------------*
362200950118     C                   EXSR      CTRTSM
362300020422     C*               *--------------------*
362400020328     C*
362500020328     C* scrive record x record sul workF
362600020423     C*  soo se hanno lo stop e se hanno un peso.
362700020620     c                   if        ftdstp <>0  and
362800020620     c                             ftdpkl <>0  and
362900020422    3C                             parsml = ' '
363000020328     C                   EXSR      wrt_FTDW
363100020419     c                   end
363200931021     C*
363300011105     C     KFTD          READE     fiftd001                               32
363400931104    5C                   ENDDO
363500931102     C*
363600931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA SOMMARE PER "G"
363700931102     C*                                 O DA TASSARE PER "T"
363800931102     C                   EXSR      CTRSTP
363900931103    4C                   ENDIF
364000931022     C*
364100950116     C* BASE  GIA' VALORIZZATA
364200931103    4C     *IN15         IFEQ      *ON
364300931105     C                   MOVEL     'M'           W01FIT
364400931105     C                   Z-ADD     VI4ITT        W01ITT
364500931103    4C                   ENDIF
364600950116     C* ACCESSORI  GIA' VALORIZZATI
364700931103    4C     *IN16         IFEQ      *ON
364800931105     C                   MOVEL     'M'           W01FIA
364900931105     C                   Z-ADD     VI4ITA        W01ITA
365000931103    4C                   ENDIF
365100931021     C*
365200931103   X3C                   ELSE
365300931022     C*
365400931022     C* BASE E ACCESSORI GIA' VALORIZZATI
365500931022     C                   Z-ADD     VI4ITT        W01ITT
365600931022     C                   Z-ADD     VI4ITA        W01ITA
365700931021     C                   MOVEL     'M'           W01FIT
365800931021     C                   MOVEL     'M'           W01FIA
365900931022     C*
366000931022     C* ANNULLO TUTTE LE EVENTUALI MEMORIZZAZIONI SULLE BOLLE
366100931022     C                   SETON                                        14
366200931022     C                   EXSR      ANNSPE
366300931022     C                   SETOFF                                       14
366400931022     C*
366500931021    3C                   ENDIF
366600931021     C* AGGIORNO TESTATA
366700011105     C     KFTD          CHAIN     FIFTT000                           30
366800020212     C                   z-add     g01CLA        FTTCLA
366900020212     C                   z-add     g01CPE        FTTCPE
367000020212     C                   z-add     g01KFA        FTTKFA
367100020212     C                   z-add     g01TVL        FTTTVL
367200020212     C                   z-add     g01SNA        FTTSNA
367300931022     C*
367400931022     C* SE TROVATI ERRORI IMPOSTO IL FLAG DI PRESENZA MANCA TARIFFA
367500931103    3C     VI4SCE        IFEQ      'E'
367600931022     C                   MOVEL     'S'           FTTFLA
367700931103   X3C                   ELSE
367800931022     C                   MOVEL     ' '           FTTFLA
367900931103    3C                   ENDIF
368000931022     C                   MOVEL     W01ITT        FTTITT
368100931022     C                   MOVEL     W01ITA        FTTITA
368200931021     C                   MOVEL     W01FIT        FTTFIT
368300931021     C                   MOVEL     W01FIA        FTTFIA
368400931202     C                   MOVEL     W01SNP        FTTSNP
368500950119     C                   MOVE      W01SNE        FTTSNE
368600950127     C                   MOVEL     VI4PEP        FTTPEP
368700021211     C                   movel     vi4FCS        fttLVL
368800931021     C                   MOVEL     'V'           FTTFVL
368900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
369000020326     C* i primi tre caratteri
369100020326     c                   if        fttfgs = 0
369200020326     c                   movel     fttpdr        fttfgs
369300020326     c                   end
369400020326     C*
369500011105     C  N30              UPDATE    FIFTT000
369600020423     c*
369700020423     c* pulisce eventuali records del file di work rimasti in sospeso
369800020423     c*               *-----------------------*
369900020423     c  N30              exsr      del_FTDW0
370000020423     c*               *-----------------------*
370100931021    2C                   ENDIF
370200931021    1C                   ENDIF
370300931021     C*
370400931021     C                   ADD       1             NRR4
370500931103    0C                   ENDDO
370600931110     C* IMPOSTO IL FLAG DI VALORIZZATE
370700931110     C     *IN90         IFEQ      *ON
370800931110     C                   MOVEL     'V'           VI1FVL
370900931110     C                   MOVEL     CVALOR        VI1VAL
371000931110     C                   ENDIF
371100931021     C                   ENDSR
371200030821     C*---------------------------------------------------------------*
371300041124     C*  Ciclo preventivo di annullamento FIFCE se per problemi era
371400041124     C*  rimasto sporco. --> Non deve esserci nulla prima della Valorizzazione
371500030821     C*---------------------------------------------------------------*
371600041124     C     Annulla_FCE   BEGSR
371700041124      *
371800041124     c                   movel     vi4pdr        FGS_vi4pdr        3 0
371900041124      *          *-------------------------------------*
372000041124     c     parsml        ifeq      ' '
372100041124     c     Del_FCE       setll     fifce01l
372200041124     c     Del_FCE       reade     fifce01l
372300041124     c                   dow       not %Eof(fifce01l)
372400041124     c                   delete    fifce000
372500041124     c     Del_FCE       reade     fifce01l
372600041124     c                   enddo
372700041124     c                   end
372800041124      *          *-------------------------------------*
372900041124      *
373000041124     C                   ENDSR
373100041124     C*---------------------------------------------------------------*
373200041124     C* Conta quante Spedizioni ci sono all'interno dello Stesso Stop
373300041124     C*---------------------------------------------------------------*
373400041124     C     Sped_x_Stop   BEGSR
373500030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
373600030821     C*  lo stop.
373700030821     c                   z-add     0             W01nss
373800030821     c                   z-add     0             W01fw
373900030821     c                   z-add     0             W01rcc
374000090911     c                   clear                   W01_TarPartCLI    1
374100090911     c                   clear                   W01_UnoOpiuCLI    7 0
374200030821      *
374300030821     C*  pgm x contare le spedizioni in STOP
374400030821     c                   clear                   FICNB1
374500030821     C                   eval      §t1PDR  =  ftdPDR
374600030821     C                   eval      §t1TSR  =  ftdTSR
374700030821     C                   eval      §t1NDC  =  ftdNDC
374800030821     C                   eval      §t1DDC  =  ftdDDC
374900030821     C                   eval      §t1STP  =  ftdSTP
375000030821     C*
375100030821     C                   CALL      'FICNB1R1'
375200030821     C                   PARM                    ficnb1
375300030821     C* Nr.Sped.totali in STOP
375400030821     c                   z-add     §T1nss        W01nss
375500030821     c                   z-add     §T1fw         W01fw
375600030821     c                   z-add     §T1rcc        W01rcc
375700090911     c                   eval       W01_TarPartCLI = §T1PARTIC
375800090911     c                   eval       W01_UnoOpiuCLI = §T1CLIPAR
375900030821      *
376000030821     C                   ENDSR
376100020212     C*---------------------------------------------------------------*
376200020328     C* Scrittura del file di work record x record
376300020212     C*---------------------------------------------------------------*
376400020328     C     wrt_FTDW      BEGSR
376500020328      *
376600020328     C                   z-add     ftdfgs        ftwfgs
376700020328     C                   z-add     ftdpdr        ftwpdr
376800020328     C                   move      ftdtsr        ftwtsr
376900020328     C                   z-add     ftdndc        ftwndc
377000020328     C                   z-add     ftdddc        ftwddc
377100020328     C                   z-add     ftdstp        ftwstp
377200020328     C                   z-add     ftdaas        ftwaas
377300020328     C                   z-add     ftdlnp        ftwlnp
377400020328     C                   z-add     ftdnrs        ftwnrs
377500020328     C                   z-add     ftdnsp        ftwnsp
377600020328     C                   z-add     ftdpkl        ftwpkl
377700020328     C                   z-add     ftdvlu        ftwvlu
377800020328     c                   write     fiftdw00
377900020328      *
378000020328     C                   ENDSR
378100020328     C*---------------------------------------------------------------*
378200020328     C* Cancella i record del work dopo aver eseguito lo stop
378300020328     C*---------------------------------------------------------------*
378400020328     C     del_FTDW      BEGSR
378500020328      *
378600020328     C                   z-add     ftdfgs        kftwfgs
378700020328     C                   z-add     ftdpdr        kftwpdr
378800020328     C                   move      ftdtsr        kftwtsr
378900020328     C                   z-add     ftdndc        kftwndc
379000020328     C                   z-add     ftdddc        kftwddc
379100020328     C                   z-add     ftdstp        kftwstp
379200020328      *
379300020328     c     kftdw         setll     fiftdw1l
379400020328     c     kftdw         reade     fiftdw1l
379500020328     c                   dow       not %eof(fiftdw1l)
379600020328     c                   delete    fiftdw00
379700020328     c     kftdw         reade     fiftdw1l
379800020328     C                   ENDdo
379900020328      *
380000020328     C                   ENDSR
380100020422     C*---------------------------------------------------------------*
380200020422     C* Cancella i record del work dopo aver eseguito lo stop
380300020422     C*---------------------------------------------------------------*
380400020422     C     del_FTDW0     BEGSR
380500020422      *
380600020423     C                   z-add     fttfgs        kftwfgs
380700020423     C                   z-add     fttpdr        kftwpdr
380800020423     C                   move      ftttsr        kftwtsr
380900020423     C                   z-add     fttndc        kftwndc
381000020423     C                   z-add     fttddc        kftwddc
381100020423      *
381200020422     c     kftdw0        setll     fiftdw1l
381300020422     c     kftdw0        reade     fiftdw1l
381400020422     c                   dow       not %eof(fiftdw1l)
381500020422     c                   delete    fiftdw00
381600020422     c     kftdw0        reade     fiftdw1l
381700020422     C                   ENDdo
381800020422      *
381900020422     C                   ENDSR
382000020328     C*---------------------------------------------------------------*
382100020328     C* CONTROLLO LA SOMMA PESI -------------------------------------*
382200020328     C*---------------------------------------------------------------*
382300020328     C     CTRTSM        BEGSR
382400931029     C                   SETOFF                                       17
382500020122     C                   CLEAR                   FICNB2
382600000626     C* VERIFICO SE BOLLA POSTA OPPURE NO
382700000626     C* CONTROLLO IN AZORG SE PO POSTE
382800000626     C                   EXSR      CTRORG
382900931123     C* MEMORIZZO CODICE TARIFFA
383000931123    1C     FTDCTP        IFEQ      *BLANKS
383100000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
383200000623    2C     WCTP          IFEQ      *BLANKS
383300000623     C     WCTB          ANDEQ     *BLANKS
383400931123     C                   MOVEL     WALF3         FTDCTP
383500000623     C                   ELSE
383600000623     C*  P.O. POSTE
383700020729     C     §OgNTW        IFEQ      'PPT'
383800000623     C                   MOVEL     WCTP          FTDCTP
383900000623     C                   ELSE
384000000623     C                   MOVEL     WCTB          FTDCTP
384100000623    3C                   ENDIF
384200000623     C*
384300000623    2C                   ENDIF
384400000623     C*
384500931123    1C                   ENDIF
384600931029     C*
384700950118     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
384800950118     C                   SETON                                        30
384900950118    6C     *IN19         IFEQ      *OFF
385000950118     C* T I P O   B O L L A   D A   T A S S A R E
385100950224     C                   CLEAR                   ARBCBO
385200091012     C     Bolla_FTD     CHAIN     FNARB01L                           32
385300950118     C                   EXSR      CTRCBL
385400030109      *
385500950118     C   31              SETOFF                                       30
385600950118     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
385700950118     C*  (SE NON CONSEGNATA)
385800950118    7C   30FTDDCM        IFEQ      0
385900950118     C*
386000950118    8C     FTDCMC        IFNE      *BLANKS
386100950118     C                   MOVEL     FTDCMC        WALF6
386200950118   X8C                   ELSE
386300950118     C                   MOVEL     FTDTMC        WALF6
386400950118    8C                   ENDIF
386500950118     C                   EXSR      CTRCMC
386600950118    7C                   ENDIF
386700950118    6C                   ENDIF
386800000626     C*  P.O. POSTE
386900050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
387000020729     C     §OgNTW        IFEQ      'PPT'
387100050426     C     vh4tpo        andeq     'S'
387200000626     C                   SETOFF                                       30
387300130305     C*
387400000626     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE SIGNIFICA CHE LA BOLLA
387500000626     C* SI DEVE PAGARE COME CONSEGNATA
387600000626     C                   MOVEL     '2Z'          COD
387700000626     C                   MOVEL     *BLANKS       KEY
387800000626     C                   MOVEL     FTDCMC        KEY
387900000626     C     KTAB          CHAIN     TABEL                              30
388000000626     C  N30              MOVEL     TBLUNI        DS2Z
388100000626     C   30              MOVEL     *BLANKS       DS2Z
388200000626     C     §2ZTPP        IFNE      *BLANKS
388300000626     C* LA BOLLA VIENE PAGATA
388400000626     C                   SETON                                        30
388500000626     C                   ENDIF
388600000626     C                   MOVEL     §2ZTPP        FTDFGC
388700000626    3C                   ENDIF
388800950118     C*
388900950118    6C     *IN30         IFEQ      *ON
389000950119     C                   MOVEL     'S'           W01PAG            1
389100950118     C                   ELSE
389200950119     C                   ADD       1             W01SNP                          no pagare
389300950118     C                   MOVEL     ' '           W01PAG
389400950118    6C                   ENDIF
389500950119     C*
389600950119     C     *IN19         IFEQ      *OFF
389700950119     C     W01PAG        ANDEQ     *BLANKS
389800950119     C     FTDDCM        ANDEQ     *ZEROS
389900950119     C                   ADD       1             W01SNE                          no cons.no pag
390000950119     C                   END
390100950118     C*
390200950118     C     W01PAG        IFEQ      'S'
390300950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
390400950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
390500931029     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
390600950123     C                   MOVE      'S'           §TAPAG
390700950118     C                   ELSE
390800950118     C                   MOVE      *ZEROS        §TAKFA                          PESO
390900950118     C                   MOVE      *ZEROS        §TATVL                          VOLUME
391000950118     C                   MOVE      *ZEROS        §TANCL                          COLLI
391100950123     C                   MOVE      ' '           §TAPAG
391200950118     C                   END
391300950118     C*
391400950118     C     *IN19         IFEQ      *ON
391500950118     C                   Z-ADD     FTDNCE        §TACPE                          COLLI ETICH
391600950118     C                   ELSE
391700950118     C                   Z-ADD     FTDNCL        §TACPE                          COLLI PIK.
391800950118     C                   END
391900950118     C*
392000950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
392100021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
392200950118     C*
392300950130     C                   Z-ADD     FTDSET        §TASET
392400931029     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
392500931029     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
392600020325     C*
392700020325     C* memorizza se c'è anteporto per Isola
392800020325     C*  ma attenzione,se almeno uno all'interno dello stesso STOP
392900020325     C* è diverso da isola, resetta il flag da passare al pgm di calcolo x stop
393000020325     C*
393100020325     C* Se almeno una delle spedizioni all'interno dello stop non è
393200020325     C*  un'isola ...lo memorizza.
393300020325     c                   if        almuno_iso = 1 and ftdFIN <> 'I'             Anteporto
393400020325     C                   move      *blank        w01isola                       se Isola
393500020325     c                   end
393600020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
393700020325     C*   all'interno dello stesso stop non sia gestita ad Isola che deve
393800020325     C*    segnalarlo.
393900020325     c                   if        almuno_iso = 0
394000020325     c                   z-add     1             almuno_iso        1 0
394100020325     C                   move      *blank        w01isola                       Anteporto
394200020325     c                   if        ftdFIN = 'I'                                 per indicare
394300020325     C                   move      'S'           w01isola                       se Isola
394400020325     c                   end
394500020325     c                   end
394600030109     C*
394700931111     C* SOMMA PESI A GIORNATA O A STOP:
394800931111     C* ELABORO COMUNQUE GLI ACCESSORI BOLLA PER BOLLA
394900931111     C*
395000020325      *  *-----------------------*
395100931111     C***   S T O P
395200020325      *  *-----------------------*
395300931111     C* SE NON C'E' IMPORTO MANUALE E NON E' VALORIZZATA BASE DISTINTA
395400931116     C* E NON E' SOLO UN RITORNO ALL'INCASSO
395500950126     C* E BASE SPEDIZIONE NON VALORIZZATA
395600931111     C*  ENTRO NEL CALCOLO DELLO "STOP"
395700931111    2C     FTDFIT        IFNE      'M'
395800931111     C     *IN15         ANDEQ     *OFF
395900931116     C     FTDSIC        ANDNE     'S'
396000960905    3C     FTDITT        IFEQ      *ZEROS
396100931111     C                   EXSR      CALSTP
396200960905   X3C                   ELSE
396300960906     C     FTDSTP        IFGT      *ZEROS
396400960906    4C     FTDSTP        ANDEQ     W01STP
396500960906     C                   CLEAR                   W01STP
396600960905     C                   CLEAR                   W01KFA
396700960905     C                   CLEAR                   W01NCL
396800960905     C                   CLEAR                   W01TVL
396900960905     C                   CLEAR                   W01CPE
397000960905    4C                   END
397100960905     C* SE ANCHE ACCESSORI PIENI NON TASSO
397200960905    4C     FTDITA        IFGT      0
397300960905     C     *IN16         OREQ      *ON
397400960905     C                   SETON                                        17
397500960905    4C                   ENDIF
397600960905    3C                   END
397700931111   X2C                   ELSE
397800931111     C* SE ANCHE ACCESSORI PIENI NON TASSO
397900931111    3C     FTDITA        IFGT      0
398000931111     C     *IN16         OREQ      *ON
398100931111     C                   SETON                                        17
398200931111    3C                   ENDIF
398300931111    2C                   ENDIF
398400931111     C*
398500931029     C* SOLO PER TARIFFA A KILO O QUINTALE
398600931123     C                   MOVEL     FTDCTP        WALF1
398700931111    2C     WALF1         IFEQ      '0'
398800931029     C     WALF1         OREQ      '3'
398900931029     C*
399000020325      *  *-----------------------*
399100931111     C***   G I O R N A T A
399200020325      *  *-----------------------*
399300931029     C* SOLO SE NON HO GIA' VALORIZZATO LA BASE
399400931111    4C     *IN15         IFEQ      *OFF
399500931029     C     FTDITT        ANDEQ     0
399600950118     C     W01PAG        IFEQ      'S'
399700931125     C                   ADD       FTDNCL        W01CLA                          COLL GIORN
399800020304     C                   ADD       FTDPKL        W01KFA                          PESO GIORN
399900020325     C                   ADD       FTDVLU        W01TVL                          VOL  GIORN
400000020325      *
400100020325      * non deve essere l'anteporto per Isole
400200020325      *  in quanto per le spedizioni Isole non si deve calcolare nè la
400300020325      *   base nè lo stop. Quindi escludo dalla somma della giornata i valori
400400020325      * per il calcolo .
400500151124     c**** ftdfin        ifne      'I'
400600020325     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
400700020304     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
400800020304     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
400900151124     c***********        end
401000020325      *
401100950118     C                   END
401200020320     C     *IN19         IFEQ      *on
401300020304     C                   ADD       FTDNCE        W01CPE                          COLLI ETICH
401400020304     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
401500950118     C                   ELSE
401600020304     C                   ADD       FTDNCL        W01CPE                          COLLI PIK.
401700020304     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
401800950118     C                   END
401900931029     C                   MOVEL     *HIVAL        §TAITT
402000931029     C*
402100950214     C     W01PAG        IFEQ      'S'
402200931111    5C     W02STP        IFNE      FTDSTP                                        STOP GIORN
402300931111    6C     W02STP        IFGT      0
402400931029     C                   ADD       1             W01SNA
402500020212     C                   ADD       1             g01SNA
402600931111    6C                   ENDIF
402700931102     C                   MOVEL     FTDSTP        W02STP
402800931111    5C                   ENDIF
402900931029     C*
403000931111    5C     W02STP        IFEQ      FTDSTP
403100931102     C     W02STP        ANDEQ     0
403200931029     C                   ADD       1             W01SNA
403300020212     C                   ADD       1             g01SNA
403400931111    5C                   ENDIF
403500931111    4C                   ENDIF
403600950214     C                   ENDIF
403700931029     C*
403800931111    4C     FTDITA        IFGT      0
403900931029     C     *IN16         OREQ      *ON
404000931029     C                   SETON                                        17
404100931111    4C                   ENDIF
404200931029     C*
404300020304    2C                   ENDIF
404400931111     C*
404500931111     C* T A S S A Z I O N E   B O L L A
404600931111     C*
404700950118     C* SE CI SONO ENTRAMBI GLI IMPORTI NON TASSO
404800931112    1C     FTDITT        IFGT      0
404900931112     C     *IN15         OREQ      *ON
405000950118     C     FTDSIC        OREQ      'S'                                          RITORNO INCASSO
405100931112     C     FTDITA        IFGT      0
405200931112     C     *IN16         OREQ      *ON
405300950118     C     W01PAG        OREQ      *BLANKS                                      NON PAGARE
405400931112     C                   SETON                                        17
405500931111    1C                   ENDIF
405600931112    1C                   ENDIF
405700931112     C*
405800020325     C                   movel     *blanks       §taISO
405900020213     C                   MOVEL     'S'           §TAtsm
406000931112     C                   EXSR      TASSAZ
406100931102     C*
406200020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
406300020326     C* i primi tre caratteri
406400020326     c                   if        ftdfgs = 0
406500020326     c                   movel     ftdpdr        ftdfgs
406600020326     c                   end
406700020326     C*
406800011105     C                   UPDATE    fiftd001
406900931029     C                   ENDSR
407000020212     C*---------------------------------------------------------------*
407100931022     C* CALCOLO LE SOMME PESI A STOP ---------------------------------*
407200020212     C*---------------------------------------------------------------*
407300931022     C     CALSTP        BEGSR
407400931102     C*
407500931102     C* SE TROVATO STOP ED E' CAMBIATO --> TASSO LO STOP
407600931102    1C     W01STP        IFNE      0
407700931102     C     FTDSTP        ANDNE     W01STP
407800931102     C     W01STP        ORNE      0
407900931123     C     FTDCTP        ANDNE     W01CTP
408000020212     C*
408100000626     C* SOLO PER DEBUG
408200000626     C     FTDSTP        IFEQ      3
408300000626     C     W01STP        ANDEQ     2
408400000626     C                   MOVE      2             W01STP
408500000626     C                   ENDIF
408600931102     C*
408700020313      *  nuovo modo di salvare la chiave della spedizione in quanto i ritiri
408800020313      *   nella nuova forma hanno la riga con la spedizione.
408900020315     c                   clear                   w01spc
409000020315     c                   clear                   w01spr
409100020315     c                   IF        ftdnsp > 0
409200020313     C                   MOVEL     FTDSPC        W01SPC
409300020313     c                   else
409400020313     C                   MOVEL     FTDSPR        W01SPR
409500020313     c                   end
409600020325     C*
409700931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
409800011105     C     W05NRR        CHAIN     fiftd000                           32
409900931102     C*
410000931102     C                   Z-ADD     W01KFA        §TAKFA
410100931102     C                   Z-ADD     W01TVL        §TATVL
410200931102     C                   Z-ADD     W01NCL        §TANCL
410300950118     C                   Z-ADD     W01CPE        §TACPE
410400950123     C                   MOVEL     'S'           §TAPAG
410500950118     C     §TAKFA        IFEQ      0
410600950118     C                   MOVE      *BLANKS       §TAPAG
410700950118     C                   END
410800950118     C*
410900931102     C                   MOVEL     *HIVAL        §TAITA
411000020325     C                   movel     w01isola      §taISO
411100931102     C*
411200020213     C                   MOVEL     'T'           §TAtsm
411300931102     C                   EXSR      TASSAZ
411400020328      *
411500020325     C*  resetta i contatori per le isole e per l'espresso.
411600020325     c                   z-add     0             almuno_iso        1 0
411700020325     c                   z-add     0             almuno_xpr        1 0
411800030109     C*  resetta a livello di stop il destinatario disagiato
411900030109     c                   z-add     0             almuno_disag      1 0
412000931102     C*
412100020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
412200020326     C* i primi tre caratteri
412300020326     c  n32              if        ftdfgs = 0
412400020326     c                   movel     ftdpdr        ftdfgs
412500020326     c                   end
412600020326     C*
412700011105     C  N32              UPDATE    fiftd000
412800020325     C*
412900931102     C* RILEGGO IL RECORD
413000020313      * prima testava l'indicatore adesso non è più possibile distinguere i ritiri
413100020313      * allo stesso modo
413200020315     c                   IF        w01spc <> *blank
413300931202     C                   MOVEL     W01SPC        FTDSPC
413400931202     C                   MOVEL     FTDAAS        VH6AAS
413500931202     C                   MOVEL     FTDNSP        VI6NSP
413600020315     c                   if        vh4tsr='R'
413700020315      * vista per i nuovi ritiri x spedizione
413800020315     C     KFTD5         CHAIN(N)  fiftd005                           32
413900020315     c                   else
414000020315      * seleziona solo i consegnati
414100011105     C     KFTD2         CHAIN(N)  fiftd002                           32
414200020315     c                   end
414300940415   X2C                   ELSE
414400931202     C                   MOVEL     W01SPR        FTDSPR
414500931202     C                   MOVEL     FTDKSC        VI6NSP
414600011105     C     KFTD4         CHAIN(N)  fiftd004                           32
414700940415    2C                   ENDIF
414800931102     C*
414900020122     C                   CLEAR                   FICNB2
415000931102     C                   CLEAR                   W01KFA
415100931102     C                   CLEAR                   W01NCL
415200931102     C                   CLEAR                   W01TVL
415300950118     C                   CLEAR                   W01CPE
415400931102     C* REIMPOSTO I CAMPI
415500950118     C     W01PAG        IFEQ      'S'
415600950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
415700950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
415800931102     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
415900950125     C                   MOVEL     'S'           §TAPAG
416000950118     C                   ELSE
416100950118     C                   MOVE      *ZEROS        §TAKFA
416200950118     C                   MOVE      *ZEROS        §TATVL
416300950118     C                   MOVE      *ZEROS        §TANCL
416400950125     C                   MOVEL     ' '           §TAPAG
416500950118     C                   END
416600020313      *
416700020320     C     *in19         ifeq      *on
416800950118     C                   Z-ADD     FTDNCE        §TACPE
416900950118     C                   ELSE
417000950118     C                   Z-ADD     FTDNCL        §TACPE
417100950118     C                   END
417200020313      *
417300950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
417400021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
417500931102     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
417600931102     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
417700931110     C                   MOVEL     FTDSTP        W01STP
417800940415     C* MEMORIZZO CODICE TARIFFA
417900940415    2C     FTDCTP        IFEQ      *BLANKS
418000940415     C                   MOVEL     WALF3         FTDCTP
418100000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
418200000623    3C     WCTP          IFEQ      *BLANKS
418300000623     C     WCTB          ANDEQ     *BLANKS
418400000623     C                   MOVEL     WALF3         FTDCTP
418500000623     C                   ELSE
418600000623     C* VERIFICO SE BOLLA POSTA OPPURE NO
418700000623     C* CONTROLLO IN AZORG SE PO POSTE
418800000623     C                   EXSR      CTRORG
418900000623     C*  P.O. POSTE
419000020729     C     §OgNTW        IFEQ      'PPT'
419100000623     C                   MOVEL     WCTP          FTDCTP
419200000623     C                   ELSE
419300000623     C                   MOVEL     WCTB          FTDCTP
419400000623    4C                   ENDIF
419500000623     C*
419600000623    3C                   ENDIF
419700000623     C*
419800940415    2C                   ENDIF
419900931102    1C                   ENDIF
420000931102     C*
420100931022     C* C'E' LO STOP E LA BASE NON E' TASSATA(NE' DI BOLLA NE' DI DIST)
420200931102    1C     FTDSTP        IFGT      0
420300931022     C                   MOVEL     FTDSTP        W01STP                          SALVO STOP
420400931123     C                   MOVEL     FTDCTP        W01CTP                          SALVO CTR
420500931102     C                   Z-ADD     FTDNRR        W05NRR            9 0
420600950118     C     W01PAG        IFEQ      'S'
420700931022     C                   ADD       FTDNCL        W01NCL
420800950119     C                   END
420900950119     C*
421000931022     C                   MOVEL     *HIVAL        §TAITT
421100931102    2C     FTDITA        IFGT      0
421200931022     C     *IN16         OREQ      *ON
421300931022     C                   SETON                                        17
421400931102    2C                   ENDIF
421500931102    1C                   ENDIF
421600931022     C*
421700931022     C                   ENDSR
421800020212     C*---------------------------------------------------------------*
421900931102     C* TASSAZIONE BOLLA ---------------------------------------------*
422000020212     C*---------------------------------------------------------------*
422100931102     C     TASSAZ        BEGSR
422200091012      *
422300931111     C* 17 OFF - D A   T A S S A R E
422400931111    0C     *IN17         IFEQ      *OFF
422500931123     C                   MOVEL     FTDCAP        §TACAP                          CAP MIT/DES
422600020326     C                   MOVEL     ftdfgs        §TAfgs
422700091012      *
422800020326     c                   if        §tafgs=0
422900020326     C                   MOVEL     FTDPDR        §TAfgs                          P.O. di gestione
423000020326     c                   end
423100091012      *
423200931102     C                   MOVEL     FTDPDR        §TAPDR                          PADRONCINO
423300931102     C                   MOVEL     PARSML        §TASML                          SIMULAZ
423400020326     C                   MOVEL     vh4DDC        §TADDC                          DATA DISTIN
423500020326     C                   z-add     vi4ndc        §tandc
423600020129     C                   z-add     ftdaas        §taaas
423700020129     C                   z-add     ftdlnp        §talnp
423800020129     C                   z-add     ftdnrs        §tanrs
423900020129     C                   z-add     ftdnsp        §tansp
424000020328     C                   z-add     ftdstp        §tastp
424100091012      *
424200020225     c                   if        §tansp = 0 and ftdtsr='R'
424300020225     C                   z-add     ftdksc        §tansp
424400020326     C                   z-add     ftdksc        §taksc
424500020225     c                   end
424600050909      *
424700050909     C* Comunque aggancia sempre la bolla in partenza x vedere
424800050909      *  il codice trattamento merce
424900091012     c                   clear                   trovato_arb       1
425000091012     c                   clear                   trovato_blp       1
425100050909     C                   clear                   DS1B
425200091012     c     bolla_ftd     chain     fnblp01l
425300050909     c                   if        %found(fnblp01l)
425400091012      * ------------
425500091012     c                   move      'S'           trovato_blp       1
425600091012     C                   MOVE      blpCbo        §tacbo
425700091012     C                   z-add     blpKSC        CodCliente        7 0
425800130305     C                   MOVEL     blpTSP        §TATSP                          TIPO SPEDIZ
425900091012      *
426000091012     C                   MOVEL     '3A'          COD
426100091012     C                   MOVEL     *BLANKS       KEY
426200091012     C                   MOVEL     blpCBO        KEY
426300091012     C     KTAB          CHAIN     TABEL                              30
426400091012     C  N30              MOVEL     TBLUNI        DS3A
426500091012     c                   If        %subst(§3ATB1:1:1) ='A'
426600091012     C     Bolla_FTD     CHAIN     FiAR601L
426700091012     c                   if        %Found(Fiar601L)
426800091012     C                   Z-ADD     ar6KSC        CodCliente                      Cliente
426900091012     c                   end
427000091012      **   SOLO X RITIRI
427100091012     c                   iF        ftdtsr = 'R'
427200091012     C                   MOVEL     '1B'          COD
427300091012     C                   MOVEL     *BLANKS       KEY
427400091012     C                   MOVEL     blpCTM        KEY
427500091012     C     KTAB          CHAIN     TABEL                              30
427600091012     c                   if        %found(TABEL00F)
427700091012     C                   MOVEL     TBLUNI        DS1B
427800091012      * deve controllare se abilitata a tariffa Disk C
427900091012      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
428000091012      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
428100091012      *   la tariffa di Etichettatura in generale.
428200091012      *
428300091012      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
428400091012      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
428500091012     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
428600091012     c                   move      'N'           §taPPE
428700091012     c                   end
428800091012      * Se è un DISKC Bartolini la regola dice che deve essere comunque
428900091012      * pagata la spedizione al Padroncino
429000091012     c                   iF        §1Bdkc ='B' and ftdnrs = 0
429100091012     c                   eval      §taPPE = 'S'
429200091012     c                   eval      §tadkc = 'S'
429300091012     c                   end
429400091012     c                   end
429500091012      **
429600091012     c                   endiF
429700091012      *
429800091012     c                   endiF
429900091012      * ------------
430000091012     c                   end
430100091012      * Se non ha trovato il BLP
430200091012      *  oppure
430300091012      * Comunque se si tratta di consegne
430400091012     c                   if        NOT %found(fnblp01l) or FTDTSR = 'C'
430500091012     c     bolla_ftd     chain     fnarb01l
430600091012     c                   if        %found(fnarb01l)
430700091012     c                   move      'S'           trovato_arb       1
430800091012     C                   MOVE      arbCbo        §tacbo
430900091012     C                   z-add     arbKSC        CodCliente        7 0
431000130305     C                   MOVEL     ARBTSP        §TATSP                          TIPO SPEDIZ
431100130305     C* SOLO SE CONSEGNATA
431200130305    2C     FTDDCM        IFGT      0
431300130305     C                   MOVEL     ARBTC1        §TATC1                          CONS PARTIC
431400130305     C                   MOVEL     ARBTC2        §TATC2                          CONS PARTIC
431500130305    2C                   ENDIF
431600130305     C* GESTIONE PARTICOLARITA' VARIE
431700130305     C                   MOVEL     ARBGVA        §TAGVA
431800130305     C                   MOVE      ' '           §TAGVA
431900130305     C*
432000091012     c                   end
432100091012     c                   end
432200091012      *
432300091012      *  Carica i campi di passaggio
432400931202     C                   Z-ADD     1             §TASPP                          NUM STOP
432500931123     C                   MOVEL     FTDTSR        §TATSR                          TIPO PREST
432600931108     C                   MOVEL     FTDFIN        §TAFIN                          INOLTRO/ANT
432700931124     C                   MOVEL     FTDCTP        §TACTR                          COD TARIFFA
432800931102     C*
432900931102     C* SE GIA' VALORIZZATA IMPOSTO MAX VALORE PER NON CALCOLARLA
433000931102     C   15              MOVEL     *HIVAL        §TAITT                          BASE
433100931102     C   16              MOVEL     *HIVAL        §TAITA                          ACCESSORI
433200950118     C*
433300950118     C* IMPORTO  FATTURA ASSEGNATO INCASSATO
433400950118     C                   Z-ADD     FTDIFP        §TAIFP
433500011109      *  Divisa
433600011109     c                   if        vh4tpr = 2
433700011109     c                   movel     vi4ctd        §tadiv
433800011109     c                   end
433900950118     C* FLAG SOLO INCASSO
434000950118     C                   MOVEL     FTDSIC        §TASIC
434100130305     C* *******
434200020325     C                   MOVEL     *blank        §taxpr
434300020325     C* Se a spedizione
434400020325     c     §tatsm        ifeq      'S'
434500130305      **
434600130305     c     trovato_arb   IFEQ      'S'
434700030109     C*----------
434800020325     C* memorizza se all'interno dello stop sono tutte spedizioni espresso
434900020325     C* Se almeno una delle spedizioni all'interno dello stop non è
435000020325     C*  espresso ...lo segnala.
435100130305     c                   if        almuno_xpr = 1 and arbtsp <> 'E'
435200130305     c                             and arbtsp <> 'H'
435300020325     C                   move      *blank        w01xpres
435400020325     c                   end
435500020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
435600020325     C*   all'interno dello stesso stop non sia un espresso che deve segnalarlo
435700020325     c                   if        almuno_xpr = 0
435800020325     c                   z-add     1             almuno_xpr        1 0
435900020325     C                   move      *blank        w01xpres                       Espresso
436000130305     c                   if        arbtsp = 'E' or arbtsp = 'H'
436100020325     C                   move      'S'           w01xpres
436200020325     c                   end
436300020325     c                   end
436400030109     C*----------
436500030109     C* Se almeno una delle spedizioni all'interno dello stop non ha
436600030109     C*  il destinatario disagiato lo memorizza per non pagare il disagio.
436700030109     c                   if        almuno_disag = 1
436800030109     c                   if        arbtc1 = ' ' and arbtc2 = ' ' or
436900030109     c                             arbdcm = 0
437000030109     C                   move      *blank        w01disagiato                   non disagiato
437100030109     c                   end
437200030109     c                   end
437300030109     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
437400030109     C*   all'interno dello stesso stop non sia segnalata come destinatario
437500030109     C*    disagiato che deve tenerlo presente per tutte
437600030109     c                   if        almuno_disag = 0
437700030109     c                   z-add     1             almuno_disag      1 0
437800030109     C                   move      *blank        w01disagiato
437900030109     c                   if        arbtc1 = 'X' or arbtc2 = 'X'                 per indicare
438000030109     c                   if        arbdcm > 0                                   se nella spediz.
438100030109     C                   move      'S'           w01disagiato                   il destinatario
438200030109     c                   end                                                    è disagiato
438300030109     c                   end                                                    è disagiato
438400030109     c                   end
438500020325     C*
438600130305     c                   END
438700130305      **
438800020325     c                   ElsE
438900130305      **   S T O P
439000020325     C* solo a STOP passa il flag valorizzato per il calcolo
439100020325     C                   move      w01xpres      §taxpr
439200030109     C                   move      w01disagiato  §taDED
439300020325     c                   endIF
439400931124     C*
439500931124    2C     §TAITA        IFEQ      0
439600950131     C* CONTRASSEGNO
439700950131     C                   Z-ADD     FTDCAS        §TACAS
439800950131     C                   MOVEL     FTDVCA        §TAVCA
439900011106      *   con l'EURO non è più vero che la divisa lira=*blank
440000011106     c                   if        §tavca=*blank
440100011106     c                   eval      §tavca='ITL'
440200011106     c                   end
440300011106      *
440400011106      *   con l'EURO è tolto ogni tipo di cambio dai files
440500011106      *    quindi adesso occorre passare a (0) sempre il cambio
440600011106      *     sarà il programma FNLRB9R che dovrà eseguire il
440700011106      *      ragionamwento di conversione DIVISA/EURO.
440800020129     C*
440900950131     C* VEDO SE C/A PRONTAMENTE RECUPERATO
441000931124     C                   EXSR      CTRASS
441100931124    2C                   ENDIF
441200000705     C*
441300130305     C* SE IL CODICE MANCATA CONSEGNA È VALORIZZATO
441400000705     C                   MOVEL     *BLANK        §TAFGC
441500000623    2C     FTDCMC        IFNE      *BLANK
441600000623     C* VERIFICO SE BOLLA POSTA
441700000623     C                   EXSR      CTRORG
441800000623     C*  P.O. POSTE
441900050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
442000020729     C     §OgNTW        IFEQ      'PPT'
442100050426     C     vh4tpo        andeq     'S'
442200130305     C*
442300000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
442400000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
442500000623     C                   MOVEL     '2Z'          COD
442600000623     C                   MOVEL     *BLANKS       KEY
442700000623     C                   MOVEL     FTDCMC        KEY
442800000623     C     KTAB          CHAIN     TABEL                              30
442900000623     C  N30              MOVEL     TBLUNI        DS2Z
443000000623     C   30              MOVEL     *BLANKS       DS2Z
443100000623     C                   MOVEL     §2ZTPP        §TAFGC
443200000623     C                   MOVEL     §2ZTPP        FTDFGC
443300000623    3C                   ENDIF
443400000623    2C                   ENDIF
443500091012     C*----------
443600950125     C     §TAPAG        IFEQ      ' '
443700950125     C                   MOVE      *HIVAL        §TAITA
443800950125     C                   END
443900950125     C     §TASIC        IFEQ      'S'
444000950125     C                   MOVE      *HIVAL        §TAITT
444100950125     C                   END
444200030821     C*----------
444300030821     C* contatore delle spedizioni presenti nello stesso STOP
444400030821     C* di quante "FW" e di quante "RCC".
444500030821     c     §tatsm        ifeq      'S'
444600130305      **
444700030821     c                   z-add     W01nss        §taNSS
444800030821     c                   z-add     W01fw         §taFW
444900030821     c                   z-add     W01rcc        §taRCC
445000030821     c                   z-add     W01nss        SAVnss
445100030821     c                   z-add     W01fw         SAVfw
445200030821     c                   z-add     W01rcc        SAVrcc
445300090911      *
445400090911      *  sullo stop tariffe particolari x cliente
445500090914     c                   eval       SAV_TarPartCLI = W01_TarPartCLI
445600090914     c                   eval       SAV_UnoOpiuCLI = W01_UnoOpiuCLI
445700090918     C*
445800090918      * se valorizza a Spedizione
445900090918     C*   Deve passare il codice del cliente solo se è un cliente particolare
446000090918     c     CodCliente    lookup    KcliPAR                                30
446100090918     c                   if        *in30
446200130307     C                   Z-ADD     CodCliente    §TAcliPAR
446300090918     C                   end
446400090911      *
446500030821     c                   else
446600090918      * se valorizza a Stop
446700090918     C*
446800030821      * quando è a stop imposta i campi precedentemente salvati altrimenti
446900030821      * sarebbero impostati per il giro successivo del nuovo STOP
447000030821     c                   z-add     SAVnss        §taNSS
447100030821     c                   z-add     SAVfw         §taFW
447200030821     c                   z-add     SAVrcc        §taRCC
447300090911      *
447400090911      *  sullo stop tariffe particolari x cliente
447500090911     c                   eval       §TACLIPAR      = SAV_UnoOpiuCLI
447600090911     c                   eval       §TATUTTE       = SAV_TarPartCLI
447700090911      *
447800030821     c                   end
447900030821     C*----------
448000020426     C                   MOVEL     'CALCVAL'     §TApgm
448100020205     C*
448200020122     C                   CALL      'FICNB2R'
448300020122     C                   PARM                    FICNB2
448400020328      *
448500020328      * cancella i records utilizzati nello STOP dal file di work
448600020328     C     §TAtsm        IFEQ      'T'
448700020422    3C     parsml        andeq     *blank
448800020328     c                   exsr      del_FTDW
448900020328     C                   END
449000020328      *
449100931102    0C                   ENDIF
449200931102     C*
449300931102     C* E R R ="E"  M A N C A   T A R I F F A
449400931103    1C     §TAERR        IFEQ      'E'
449500931102     C                   SETON                                        4290
449600931102     C* ERRORE: SEGNALO LA DISTINTA
449700931112     C     VI4SCE        IFNE      'E'
449800931102     C                   MOVEL     'E'           VI4SCE
449900011109     c     vh4tpr        comp      2                                      26
450000931102     C                   UPDATE    FRB8SF4
450100931112     C                   ENDIF
450200931102     C* STAMPA MANCA TARIFFA
450300931102     C                   EXSR      STAERR
450400931102     C*
450500931103   X1C                   ELSE
450600931102     C*
450700931102     C* N O N   E '   M A N C A   T A R I F F A
450800931102     C* SE BASE      DISTINTA VALORIZZATA AZZERO IL CAMPO IN BOLLA
450900931103    2C     *IN15         IFEQ      *ON
451000931102     C                   CLEAR                   FTDITT
451100931102     C                   CLEAR                   FTDFIT
451200931103   X2C                   ELSE
451300931102     C*
451400931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
451500020226    3C     §TAITT        IFNE      *ALL'9'
451600020214     C                   add       §TAITT        FTDITT                          BASE
451700931109     C                   ADD       §TATPE        FTDITT                          PIK/ETICH
451800931109     C                   ADD       §TATST        FTDITT                          STOP
451900020320     C                   add       §taITT        w01ITT                          BASE
452000020320     C                   add       §taTPE        w01ITT                          PIK/ETICH
452100020320     C                   add       §taTST        w01ITT                          STOP
452200020226    3C                   ENDIF
452300931103    2C                   ENDIF
452400931102     C* SE ACCESSORI DISTINTA VALORIZZATI AZZERO IL CAMPO IN BOLLA
452500931103    2C     *IN16         IFEQ      *ON
452600931102     C                   CLEAR                   FTDITA
452700931102     C                   CLEAR                   FTDFIA
452800931103   X2C                   ELSE
452900931102     C*
453000931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
453100020226    3C     §TAITA        IFNE      *ALL'9'
453200931102     C                   ADD       §TAITA        W01ITA
453300020214     C                   add       §TAITA        FTDITA
453400020226    3C                   ENDIF
453500931103    2C                   ENDIF
453600931102     C*
453700931103    1C                   ENDIF
453800931102     C*
453900931102     C* SE VALORIZZATA SIA BASE CHE ACCESSORI
454000931102     C*  CLEAR SE IMPOSTATA A LIVELLO DI TESTATA
454100931102     C   15              CLEAR                   FTDITT
454200931102     C   15              CLEAR                   FTDFIT
454300931102     C   16              CLEAR                   FTDITA
454400931102     C   16              CLEAR                   FTDFIA
454500130305      *
454600931102     C                   ENDSR
454700020212     C*---------------------------------------------------------------*
454800931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA TASSARE/SOMMARE ---------*
454900020212     C*---------------------------------------------------------------*
455000931102     C     CTRSTP        BEGSR
455100940606     C                   SETOFF                                       17
455200130305     C*
455300931102     C* S O M M A   P E S I   A   S T O P
455400931102     C     W01STP        IFNE      0
455500931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
455600011105     C     W05NRR        CHAIN     fiftd000                           32
455700931102     C*
455800020122     C                   CLEAR                   FICNB2
455900931102     C                   Z-ADD     W01KFA        §TAKFA
456000931102     C                   Z-ADD     W01TVL        §TATVL
456100931102     C                   Z-ADD     W01NCL        §TANCL
456200950118     C                   Z-ADD     W01CPE        §TACPE
456300950123     C                   MOVEL     'S'           §TAPAG
456400950118     C     §TAKFA        IFEQ      0
456500950118     C                   MOVE      *BLANKS       §TAPAG
456600950118     C                   END
456700931102     C                   MOVEL     *HIVAL        §TAITA
456800931123     C     FTDCTP        IFEQ      *BLANKS
456900931123     C                   MOVEL     WALF3         FTDCTP
457000931123     C                   ENDIF
457100950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
457200021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
457300020325     C                   movel     w01isola      §taISO
457400931102     C*
457500020213     C                   MOVEL     'T'           §TAtsm
457600931102     C                   EXSR      TASSAZ
457700931102     C*
457800020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
457900020326     C* i primi tre caratteri
458000020326     c  n32              if        ftdfgs = 0
458100020326     c                   movel     ftdpdr        ftdfgs
458200020326     c                   end
458300020326     C*
458400011105     C  N32              UPDATE    fiftd000
458500931102     C                   ENDIF
458600931102     C*
458700931102     C* S O M M A   P E S I   A   G I O R N A T A
458800950214     C     W02STP        IFGT      0
458900931102     C                   ADD       1             W01SNA
459000020212     C                   ADD       1             g01SNA
459100931102     C                   ENDIF
459200130305     C*
459300931102     C                   ENDSR
459400020212     C*---------------------------------------------------------------*
459500931103     C* SOMMO LE VALORIZZAZIONI CHE CI SONO NEL DETTAGLIO -----------*
459600020212     C*---------------------------------------------------------------*
459700931103     C     AGGFTD        BEGSR
459800130305     C*
459900011105     C     KFTD          SETLL     fiftd001
460000011105     C     KFTD          READE     fiftd001                               32
460100931103     C*
460200931103     C     *IN32         DOWEQ     *OFF
460300020405     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
460400020405     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
460500020405     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
460600020405     C     *IN19         IFEQ      *on
460700020405     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
460800020405     C                   ELSE
460900020405     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
461000020405     C                   END
461100931103     C* TESTATA
461200931103     C     *IN15         IFEQ      *ON
461300931103     C                   CLEAR                   FTDITT
461400931103     C                   CLEAR                   FTTFIT
461500931103     C                   ELSE
461600931103     C                   ADD       FTDITT        W01ITT
461700931103     C                   ENDIF
461800931103     C* DETTAGLIO
461900931103     C     *IN16         IFEQ      *ON
462000931103     C                   CLEAR                   FTDITA
462100931103     C                   CLEAR                   FTTFIA
462200931103     C                   ELSE
462300931103     C                   ADD       FTDITA        W01ITA
462400931103     C                   ENDIF
462500020326     C*
462600020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
462700020326     C* i primi tre caratteri
462800020326     c                   if        ftdfgs = 0
462900020326     c                   movel     ftdpdr        ftdfgs
463000020326     c                   end
463100020326     C*
463200011105     C                   UPDATE    fiftd001
463300931103     C*
463400011105     C     KFTD          READE     fiftd001                               32
463500931103     C                   ENDDO
463600931103     C*
463700931103     C                   ENDSR
463800020212     C*---------------------------------------------------------------*
463900931103     C* STAMPA MANCA TARIFFA -----------------------------------------*
464000020212     C*---------------------------------------------------------------*
464100950116     C     STAERR        BEGSR
464200950116     C                   MOVEL     FTDRSC        W01RSB           15
464300931103     C  N18VH4TSR        IFEQ      'R'
464400931104     C                   SETON                                        19
464500931103     C                   ELSE
464600931104     C                   SETOFF                                       19
464700931103     C                   ENDIF
464800931103     C* TESTATA
464900931103     C     *INOF         IFEQ      *ON
465000950127     C     VI4DDC        ORNE      WM1DDC
465100931103     C                   EXCEPT    TES
465200931103     C                   SETOFF                                       OF
465300950127     C                   Z-ADD     VI4DDC        WM1DDC
465400931103     C                   ENDIF
465500950127     C  N18              MOVE      FTDAAS        AASPRT            2 0
465600950127     C  N18              MOVEL     FTDCAP        CAPPRT            5
465700931103     C* DETTAGLIO
465800931103     C   18              EXCEPT    DET1
465900950116     C  N18              EXCEPT    DET2
466000931103     C*
466100931103     C                   ENDSR
466200020212     C*---------------------------------------------------------------*
466300931103     C*  VEDO SE DARE MANCA TARIFFA ---------------------------------*
466400020212     C*---------------------------------------------------------------*
466500931103     C     MANTAR        BEGSR
466600020122     C                   CLEAR                   FICNB2
466700020426     C                   MOVEL     'CALCVAL'     §TApgm
466800931209     C                   MOVEL     PARSML        §TASML
466900931103     C                   MOVEL     VI4PDR        §TAPDR
467000020326     C                   MOVEL     vi4pdr        §TAfgs
467100011205     c                   movel     vi4ctd        §tadiv
467200020326     C                   MOVEL     vh4DDC        §TADDC
467300020326     C                   z-add     vi4ndc        §tandc
467400931103     C                   MOVEL     'M'           §TAFLG
467500020122     C                   CALL      'FICNB2R'
467600020122     C                   PARM                    FICNB2
467700931103     C*
467800931103     C* ERR=E DEVO DARE MANCA TARIFFA
467900931103    1C     §TAERR        IFEQ      'E'
468000931202     C                   SETON                                        184290
468100931103     C                   EXSR      STAERR
468200931202     C                   SETOFF                                       18
468300931103     C* SEGNALO LA DISTINTA
468400931103     C                   MOVEL     'E'           VI4SCE
468500011109     c     vh4tpr        comp      2                                      26
468600931103     C                   UPDATE    FRB8SF4
468700931103     C*
468800931103   X1C                   ELSE
468900931103     C* SOMMO LE SINGOLE VALORIZZAZIONI EVENTUALMENTE FATTE IN TESTATA
469000931103     C*  SENZA DARE ERRORE
469100931103     C                   EXSR      AGGFTD
469200931103    1C                   ENDIF
469300130305      *
469400931103     C                   ENDSR
469500020212     C*---------------------------------------------------------------*
469600931103     OPRTF198   E            TES              02
469700931102     O                       STA(1)              66
469800931102     O                       STA(2)             132
469900931103     O                       STA(3)             198
470000931102     O                       RSUT                20
470100931102     O                       SIMFEL              24
470200950127     O                       VI4DDC              99 '  /  /  '
470300931103     O                       UDATE              160 '  .  .  '
470400931103     O                       PAGE          Z    198
470500931103     O          E            TES              03
470600931103     O                       UTIME              160 '  :  :  '
470700931103     O          E            TES              04
470800931103     O                       STA(4)              66
470900931103     O                       STA(7)             132
471000931103     O                       STA(10)            198
471100931103     O          E            TES            1 05
471200931103     O                       STA(5)              66
471300931103     O                       STA(8)             132
471400931103     O                       STA(11)            198
471500931103     O          E            DET1           1
471600931103     O                       STA(13)             66
471700931103     O                       STA(14)            132
471800931103     O                       STA(15)            198
471900931103     O                       VI4PDR               8
472000931103     O                                              '-'
472100931103     O                       VH4DPD
472200950120     O                       VH4TSH            +  1
472300931110     O                       VI4NDC        Z   +  2
472400931110     O                       W01ERR             170
472500931103     O          E            DET2           1
472600931103     O                       STA(13)             66
472700931103     O                       STA(14)            132
472800931103     O                       STA(15)            198
472900931103     O                       VI4PDR               8
473000931103     O                                              '-'
473100931103     O                       VH4DPD
473200950120     O                       VH4TSH            +  1
473300931110     O                       VI4NDC        Z   +  2
473400950127     O                       AASPRT        Z   +  1
473500931103     O                       FTDLNP        Z   +  1
473600931103     O                       FTDNRS        Z   +  1
473700931103     O                       FTDNSP        Z   +  1
473800931123     O              N19      W01RSB            +  1
473900931123     O              N19      FTDFIN            +  2
474000931123     O               19      W01RSB            +  2
474100931123     O               19      FTDFIN            +  2
474200950120     O                       FTDPKL        4   +  1
474300931103     O                       FTDNCL        Z   +  1
474400950116     O                       FTDVLU        4   +  1
474500950120     O              N19      CAPPRT            +  2
474600950120     O               19      CAPPRT            +  3
474700931110     O                       §TACTR            +  3
474800931110     O                       SER(1)            +  3
474900931103**
475000931028NON ESISTONO DISTINTE/RITIRI da visualizzare per la selezione effettuata
475100030519Inserire Tariffe Valide per Distinte di Consegna -> Mancano le Tariffe
475200030519Inserire Tariffe Valide per Distinte di Ritiro   -> Mancano le Tariffe
475300931103**
475400931103XXXXXXXXXXXXXXXXXXXX/XXX                ***  CONTROLLO  TASSAZIONE   1
475500020322  AUTOTRASP.  DEL GIORNO XX/XX/XX  ***                     FICNB1R   2
475600931103                    XX.XX.XX                             PAG. XXXX   3
475700931110'      C O D I C E      '   TIPO   ' NUMERO '  SPEDIZIONE     '      4
475800020322'   AUTOTRASPORTATORE   ' PRESTAZ. 'DISTINTA'                 'RAG   5
475900931103'XXXXXXX-XXXXXXXXXXXXXXX'XXXXXXXXXX' XXXXXXX'XX XXX XX XXXXXXX'XXX   6
476000931110DESTINATARIO   '     MITTENTE      ' PESO   'COLLI'VOLUME'  CAP  '
476100931110.SOCIALE     IN' RAG.SOCIALE     AN'        '     '      ' DEST. '
476200950120XXXXXXXXXXXX  X' XXXXXXXXXXXXXXX  X'XXXXXX,X'XXXXX'XX,XXX' XXXXX '
476300931110  CAP  ' COD '     A N O M A L I A      '
476400931110 MITT. ' TAR '                          '
476500931110 XXXXX ' XXX ' XXXXXXXXXXXXXXXXXXXXXXXXX'
476600931110'                       '          '        '                 '     13
476700931110               '                   '        '     '      '       '  14
476800931110       '     '                          '                           15
476900931103**
477000931103MANCA TARIFFA
477100020322L'AUTOTRASP.NON HA TARIFFE
477200020328**
477300020328RGZPFM FILE(FIFTDW0F)
