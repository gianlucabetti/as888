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
003600020328      *
003700931202     F* DETTAGLIO CONSEGNE E RITIRI
003800020408     Ffiftd01L  UF   E           K DISK    commit
003900011105     F                                     RENAME(fiftd000:fiftd001)
004000931102     F                                     INFDS(TARDS)
004100931202     F* DETTAGLIO DELLE CONSEGNE
004200060126     Ffiftd22L  UF   E           K DISK    commit
004300011105     F                                     RENAME(fiftd000:fiftd002)
004400020315     F* DETTAGLIO DEI nuovi RITIRI
004500020408     Ffiftd05L  UF   E           K DISK    commit
004600020315     F                                     RENAME(fiftd000:fiftd005)
004700020315     F* DETTAGLIO DEI RITIRI
004800020408     Ffiftd04L  UF   E           K DISK    commit
004900011105     F                                     RENAME(fiftd000:fiftd004)
005000020408     Ffiftd00F  UF   E             DISK    commit
005100011112     FFIFGT01L  IF   E           K DISK
005200091112     FFITGT01L  IF   E           K DISK
005300021202     Ffiapd01L  IF   E           K DISK
005400950113     FFNARB01L  IF   E           K DISK
005500090918     Ffiar601L  IF   E           K DISK
005600000622     FAZORG01L  IF   E           K DISK
005700931103     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005800930707     F*
005900020408      * record del sfl per gestire correttamente il posizionamento del sfl
006000020408     D info            DS
006100020408     D  nrrult               378    379B 0
006200020408     F*
006300911017     D L1              S              3  0 DIM(30)                              FIL GESTITE
006400950112     D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
006500931108     D TSR             S              1    DIM(30)                              TIPI PRESTAZ
006600931202     D DTS             S             15    DIM(30)                              DESCR.TIP PRESTAZ
006700931202     D CTS             S              3  0 DIM(30)                              CDTAR TIP PRESTAZ
006800040308     D CMC             S              6    DIM(9999)                            MANC.CONS DA TASS
006900040308     D CBL             S              6    DIM(9999)                            CD.BOL DA NO TASS
007000931018     D K17             S              1    DIM(17)                              SKI 17 ALFABETICA
007100931104     D*
007200090908     DKCliPar          S              7s 0 DIM(9999)                            Tar.Part.sul cliente
007300090909     DKtutteBolle      S              1A
007400090908     D*
007500030519     D ERR             S             72    DIM(3) CTDATA PERRCD(1)              MSG DI ERR VID
007600931103     D STA             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE STAMPA
007700931104     D SER             S             25    DIM(2) CTDATA PERRCD(1)              MSG DI ERR STA
007800020328     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              COMANDI da eseguire
007900950125     D* PASSAGGIO DATI DA CONFERMA CONTEGGI              - FNLRC0R -
008000931028     D PARAM           DS
008100931028     D  PARPDR                 1      7  0
008200931028     D  PARTSR                 8      8
008300950119     D  PARDDC                 9     16  0
008400931028     D  PARSML               256    256
008500950113     D* PASSAGGIO DATI A INTERROGAZIONE BOLLE ARRIVI     - FNLR36R -
008600931021     D PARAM2          DS
008700931021     D  GIA                    1     12  0 INZ
008800931021     D  RICH                  13     13    INZ
008900950113     D  PA2AAS                14     17  0
009000950113     D  PA2LNP                18     20  0
009100950113     D  PA2NRS                21     22  0
009200950113     D  PA2NSP                23     29  0
009300950113     D  PA2FLG                31     31
009400950116     D* PASSAGGIO DATI A RICERCA CODICE TARIFFA PADRONCINO-TNTA86R -
009500931028     D PARAM3          DS
009600931108     D  PA3PDR                 1      7  0
009700931108     D  PA3SML                 8      8
009800931108     D  PA3TSR                 9      9
009900931108     D  PA3CTR                11     13  0
010000931108     D  PA3PRG                14     16  0
010100931108     D  PA3SEL                17     17
010200020326      *
010300020326     D DSLS05          DS
010400020326     D  PA0AAS                 1      4  0
010500020326     D  PA0LNP                 5      7  0
010600020326     D  PA0NRS                 8      9  0
010700020326     D  PA0NSP                10     16  0
010800020326     D  PA0F03                17     17
010900020326     D  PA0FLG                18     18
011000020326     D  PA0A9                 19     19
011100020326     D  PA0B5                 20     20
011200020326     D  PA075                 21     21
011300020326     D  PA098                 22     22
011400020326     D  PA0RSU                23     42
011500020326     D  PA0CML                43     43
011600020326     D  PA0SIM                44     46  0
011700020326      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
011800020326     D  PA0CLI                47     47
011900020326      * PA0GIA indica se il pgm e' richiamato dalle giacenze
012000020326     D  PA0GIA                48     48
012100020326      *
012200950112     D* DS PER TRUL06R - CARICAMENTO £X
012300950112     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012400950112     D  LIN                    1     90  0
012500950112     D                                     DIM(30)                              FIL. COMODO
012600920810     D KPJBA         E DS
012700100513     d   KPJBUS        s                   like(KPJBU)
012800910423     D WLBDAT          DS
012900950112     D  G02DAT                 1      8  0
013000950112     D  G02INV                 9     16  0
013100950112     D  G02ERR                17     17
013200950112     D  G02TGI                18     22  0
013300910423     D WGIDAT          DS
013400910423     D  GIODAT                 1      6  0
013500910423     D  GIOINV                 7     12  0
013600910423     D  GIOTGI                13     17  0
013700931104     D TCUDS           DS
013800931104     D  F1                     1      1
013900931104     D  F3                     3      3
014000931104     D  F2                     2      2
014100931104     D  F4                     4      4
014200931104     D  F56                    5      6
014300931102     D TARDS           DS
014400931102     D  FTDNRR               397    400B 0
014500931018     D                 DS
014600931019     D  AA                     1      2
014700931019     D  MM                     3      4
014800931019     D  GG                     5      6
014900931018     D  DATA                   1      6  0
015000931028     D                 DS
015100931028     D  VH4TSR                 1      1
015200950116     D  ARBCBO                 2      3
015300950224     D  W018C                  1      3
015400931015     D                 DS
015500931015     D  VI1PF1                 1      3  0
015600931015     D  VI1PD1                 4      7  0
015700931103     D  VI1P1                  1      7  0
015800931015     D                 DS
015900931015     D  VI1PF2                 1      3  0
016000931015     D  VI1PD2                 4      7  0
016100931103     D  VI1P2                  1      7  0
016200931018     D                 DS
016300931018     D  VI1ND0                 1      7  0
016400931018     D  VI1ND1                 8     14  0
016500931018     D  VI1ND2                15     21  0
016600931018     D  VI1ND3                22     28  0
016700931018     D  VI1ND4                29     35  0
016800931018     D  VI1ND5                36     42  0
016900931018     D  VI1ND6                43     49  0
017000931018     D  VI1ND7                50     56  0
017100950112     D  VI1ND8                57     63  0
017200931018     D  VI1ND9                64     70  0
017300931018     D  NDC                    1     70  0
017400931018     D                                     DIM(10)                              NUM DISTINT SELEZ
017500931102     D                 DS
017600950116     D  FTDAAS                 1      4  0
017700950116     D  FTDNRS                 5      6  0
017800950116     D  FTDNSP                 7     13  0
017900950116     D  FTDLNP                14     16  0
018000950116     D  FTDSPC                 1     16
018100931202     D                 DS
018200931202     D  FTDKSC                 1      7  0
018300950116     D  FTDRSC                 8     42
018400950116     D  FTDSPR                 1     42
018500950116     D                 DS
018600950116     D  VI4PDR                 1      7  0
018700950116     D  VI4FIL                 1      3  0
018800931021     D DS1Z          E DS
018900000622     D DS3A          E DS
019000000623     D DS2Z          E DS
019100090909     D   DCTP        E DS
019200000622     D OG143         E DS
019300021212     D DS1B          E DS
019400030821     D FICNB1        E DS                  EXTNAME(FICNB1DS)
019500020122     D FICNB2        E DS                  EXTNAME(FICNB2DS)
019600020603     D FIcnB3        E DS                  EXTNAME(FICNB3DS)
019700931021     D CNCR80        E DS
019800900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
019900100513      *
020000100513     D ddatiute      e ds                  prefix(UT_)
020100100513     D azuteds       e ds                  extname(AZUTE00F)
020200100513     D tibs34ds      E DS                  inz
020300100513      *---------------------------------------------------------------------*
020400100513     D FICNB1ds0     E DS
020500040622      *---------------------------------------------------------------------*
020600040825     d   tar_valida    S              1    INZ('N')
020700040825     d rifPDR          s                   like(fgtPDR)
020800040825     d rifCTR          s                   like(fgtCTR)
020900040825     d ptarv_rap       s                   like(fgtrap)
021000040825     d ptarv_rct       s                   like(fgtrct)
021100040622     d dataoggi        s               d   datfmt(*iso)
021200040622     d finoadata       s               d   datfmt(*iso) inz(d'2004-08-10')
021300090911     d SAV_TarPartCLI  s                   like(W01_TarPartCLI)
021400090911     d SAV_UnoOpiuCLI  s                   like(W01_UnoOpiuCLI)
021500020328      *---------------------------------------------------------------------*
021600020328     D  MSGDS          S            100
021700020328     D  LENGH          S             15  5
021800020328      *---------------------------------------------------------------------*
021900931018     D***
022000950118     D* DEFINIZIONE COSTANTI
022100931018     D***
022200931018     D CDAVAL          C                   CONST('DA VALORIZZARE')
022300931018     D CVALOR          C                   CONST('VALORIZZATE   ')
022400931123     D CMITT           C                   CONST('--M I T T E N T E-- ')
022500931202     D CSPED           C                   CONST('---- SPEDIZIONE ----')
022600950119     D COS1            C                   CONST('Uscita: MAT')
022700950119     D COS2            C                   CONST('Uscita: POM')
022800950119     D COS3            C                   CONST('  Piking  ')
022900950119     D COS4            C                   CONST('  Etich.  ')
023000021211     D COS5            C                   CONST('Caric.')
023100021211     D COS6            C                   CONST(' Scar.')
023200011105     IFIFTT002
023300000714     I              FTTATB                      FT2ATB
023400011105     I              FTTpdu                      FT2pdu
023500000714     I              FTTPDR                      FT2PDR
023600000714     I              FTTTSR                      FT2TSR
023700021211     I              FTTlvl                      FT2FCS
023800011105     I              FTTdiv                      FT2div
023900011105     I              FTTfgs                      FT2fgs
024000000714     I              FTTNDC                      FT2NDC
024100000714     I              FTTDDC                      FT2DDC
024200000714     I              FTTFVL                      FT2FVL
024300000714     I              FTTFLA                      FT2FLA
024400000714     I              FTTSET                      FT2SET
024500000714     I              FTTSNT                      FT2SNT
024600000714     I              FTTSNP                      FT2SNP
024700000714     I              FTTSNE                      FT2SNE
024800000714     I              FTTSNA                      FT2SNA
024900000714     I              FTTCLA                      FT2CLA
025000000714     I              FTTCPE                      FT2CPE
025100000714     I              FTTKFA                      FT2KFA
025200000714     I              FTTTVL                      FT2TVL
025300000714     I              FTTITT                      FT2ITT
025400000714     I              FTTFIT                      FT2FIT
025500000714     I              FTTITA                      FT2ITA
025600000714     I              FTTFIA                      FT2FIA
025700000714     I              FTTTPE                      FT2TPE
025800000714     I              FTTTBN                      FT2TBN
025900000714     I              FTTTIM                      FT2TIM
026000000714     I              FTTMNT                      FT2MNT
026100000714     I              FTTDFT                      FT2DFT
026200011105     I              FTTNFF                      FT2NFT
026300000714     I              FTTTBS                      FT2TBS
026400000714     I              FTTCUC                      FT2CUC
026500000714     I              FTTFPP                      FT2FPP
026600000714     I              FTTPEP                      FT2PEP
026700000714     I              FTTDCV                      FT2DCV
026800011105     I              FTThlv                      FT2hlv
026900011105     I              FTTctg                      FT2ctg
027000011105     I              FTTdcn                      FT2dcn
027100011105     I              FTTsoc                      FT2soc
027200011105     I              FTTflr                      FT2flr
027300011105     I              FTTftr                      FT2ftr
027400011105     I              FTTdtr                      FT2dtr
027500000000     I/SPACE 3
027600900521     C****************************************************************
027700900521     C*  RIEPILOGO INDICATORI
027800900521     C***************************************************************
027900931018     C* 01    - IMMESSA SOLO DATA DAL
028000931018     C* 02    - IMMESSA DATA AL
028100950112     C* 03    -
028200931108     C* 04    - IMMESSO TIPO PRESTAZIONE
028300950116     C* 05    - IMMESSO UN SOLO NUMERO DISTINTA (SOLO PER CONSEGNE)
028400931201     C* 06    - FASE DI SIMULAZIONE
028500931124     C* 09    - LEGGO FLARBF1C CON I CAMPI DEL LEGAME BOLLA
028600931124     C* 11    - PROTEGGO COD PADRONCINO IN SFL4 PER NON RIPETERLO
028700931103     C* 12    - PROTEGGO COD PADRONCINO IN SFL2 PER NON RIPETERLO
028800950117     C* 13    - PROTEGGO IMPORTO ACCESSORI IN SFL6 PER NON INSERIRLI
028900931022     C* 14    - ANNULLO TASSAZIONI SINGOLE DI BOLLE SE DISTINTA TASSATA
029000931022     C*         IN MODO COMPLETO DA VIDEO
029100931022     C* 15    - IMMESSO IMPORTO BASE      A LIVELLO DI DISTINTA
029200931022     C* 16    - IMMESSO IMPORTO ACCESSORI A LIVELLO DI DISTINTA
029300931022     C* 17    - ON SPEDIZIONE DA NON TASSARE
029400931102     C* 18    - ON MANCA TARIFFA PER MANCANZA SOMMA PESI
029500931104     C* 19    - ON E' RITIRO   OFF E' CONSEGNA
029600931028     C* 20    - ON PROGRAMMA RICHIAMATO
029700931105     C* 21    - ON SVALORIZZATE DISTINTE: RIEMETTO SFL2
029800931110     C* 22    - ON OBBLIGO CMD6 SE AGGIORNO DISTINTA CON MANCA TARIFFA
029900950117     C* 23    - RITORNO ALL'INCASSO E PROTEZIONE IMPORTO BASE
030000950131     C* 24    - DISTINTA VALORIZZATA
030100000626     C* 25    - NON SI PAGA LA SPEDIZIONE PER BOLLE POSTE NON CONSEGNATE
030200011109     C* 26    - PROTECT DIVISA
030300931022     C* 30/32 - DI COMODO
030400000622     C* 34    - DI COMODO
030500931018     C* 35    - PULIZIA ES EMISSIONE SFL
030600931018     C* 36    - SFLNXTCHG
030700931110     C* 37    - EMISSIONE SFL6
030800000622     C* 40/48 - ERRORI
030900000626     C* 49    - PROTEGGO IL CAMPO NUMERO STOÈP PER BOLLE POSTE
031000000622     C* 50    - EMISSIONE ERRORE TARIFFA ERRATA BARTOLINI INESISTENTI
031100000622     C* 51    - EMISSIONE ERRORE TARIFFA ERRATA POSTE INESISTENTI
031200000622     C* 52/53 - TARIFFA ERRATA
031300011108     C* 60    - ON protegge le righe del windowsfl
031400011108     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
031500900521     C*****************************************************************
031600100513     C                   clear                   ficnb1ds0
031700100513     c                   eval      FN0PGM  = 'FICNB1R'
031800100513     c                   eval      FN0TIPO = 'FILIALE'
031900100513     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
032000100513     C                   MOVEL     KPJBU         kpjbus
032100100513     C                   clear                   kpjbu
032200100513     C                   MOVEL     ficnb1ds0     kpjbu
032300100513     c                   call      'FICNB1R0'
032400100513     c                   parm                    kpjba
032500100513     C                   MOVEL     kpjbu         ficnb1ds0
032600100513     C*   se già utilizzata da altro utente della stessa filiale fa uscire
032700100513     C*   forzatamente.
032800100513     c                   if        Fn0FLG <> *blank
032900100513     c                   goto      FINE
033000100513     c                   end
033100100513     C*
033200100513     C                   MOVEL     KPJBUs        kpjbu
033300100513     C*
033400931015     C     INIZIO        TAG
033500931015     C* PULIZIA FORMATO 1
033600931103     C  N20              EXSR      PUL01
033700930426     C*
033800931015     C     FOR01         TAG
033900931028     C*
034000931028     C* PGM NON RICHIAMO - EMETTO 1 VIDEATA ALTRIMENTI VADO ALLA 2A
034100931105     C* SE SVALORIZZATE SPEDIZIONI EMETTO SUBITO LA 2A (21 ON)
034200931028     C     *IN20         IFEQ      *OFF
034300931105     C     *IN21         ANDEQ     *OFF
034400931028     C*
034500931015     C                   EXFMT     FRB8D01
034600020409     c                   z-add     0             savnr2
034700020409     c                   z-add     0             savnr4
034800020409     c                   z-add     0             savnr6
034900020604     c                   move      'N'           seF6              1
035000020604      * se richieste le distinte già Valorizzate non deve permettere di rivalorizzare
035100020604      * quindi imposto la memoria del flag SeF6 come se fosse stato dato l'F6 di
035200020604      * valorizzazione e impedire un nuovo F6 che raddoppia i conteggi.
035300020604     c                   if        vi1fvl ='V'
035400020604     c                   move      'S'           seF6
035500020604     c                   end
035600931103     C                   SETOFF                                       4746
035700920323     C** CMD3 - FINE LAVORO
035800931028     C   KC              GOTO      FINE
035900931015     C* CONTROLLI FORMATO1
036000900528     C                   EXSR      CONTR1
036100900528     C   90              GOTO      FOR01
036200931028     C                   ENDIF
036300931018     C*
036400931018     C* CARICO DISTINTE SELEZIONATE
036500931018     C                   EXSR      CARNDC
036600950616     C   90
036700950616     CAN 20              GOTO      FINE
036800950616     C   90              GOTO      FOR01
036900900611     C*
037000931119     C                   Z-ADD     1             REC2
037100900518     C*
037200931018     C     FORCT2        TAG
037300931103     C  N90              WRITE     FRB8D03
037400020603     c                   if        savnr2 > 0 and savnr2 <= rec2
037500020409     c                   z-add     savnr2        rec2
037600020409     c                   end
037700931018     C                   EXFMT     FRB8CT2
037800020409     c                   z-add     nrrult        savnr2            5 0
037900020411     c                   z-add     0             savnr4
038000931028     C   KC              GOTO      FINE
038100931028     C   KL
038200931028     CANN20              GOTO      FOR01
038300931028     C   KL
038400931028     CAN 20              GOTO      FINE
038500931018     C*
038600931018     C** CMD1 O ENTER - SELEZIONE DISTINTE
038700931018     C                   EXSR      SELNDC
038800931018     C   90              GOTO      FORCT2
038900931018     C*
039000931103     C                   Z-ADD     1             REC4
039100931018     C     FORCT4        TAG
039200020604      *
039300020604      * se ha già eseguito la valorizzazione non deve permettere un secondo
039400020604      * F6 a causa di un manca tariffa
039500020604     c     seF6          comp      'N'                                    88
039600020604      *
039700931103     C  N90              WRITE     FRB8D05
039800020409     c                   if        savnr4 > 0
039900020409     c                   z-add     savnr4        rec4
040000020409     c                   end
040100931018     C                   EXFMT     FRB8CT4
040200020604      *
040300020409     c                   z-add     nrrult        savnr4            5 0
040400020411     c                   z-add     0             savnr6
040500960906     C                   SETOFF                                       90
040600931110     C* SE AGGIORNATO DISTINTA CON DEI MANCA TARIFFA OBBLIGO IN CMD6
040700931110     C     *INKL         IFEQ      *ON
040800931110     C     *INKC         OREQ      *ON
040900931110     C   22              SETON                                        4490
041000931110     C   22              GOTO      FORCT4
041100931110     C                   ENDIF
041200130305     C*
041300931105     C   KC              GOTO      FINE
041400931110     C   KL
041500931110     CANN20              GOTO      FORCT2
041600931110     C   KL
041700931110     CAN 20              GOTO      FINE
041800931018     C* CONTROLLO SFL4
041900931018     C                   EXSR      CONTR4
042000931103     C* CMD3 - DALLA VIDEATA DI MANUTENZIONE SPEDIZIONI
042100931019     C   LR              GOTO      FINE
042200931019     C* ERRORI
042300931105     C   90              GOTO      FORCT4
042400931105     C* RIEMETTO IL SFL2 PERCHE' HO ANNULLATO DELLE VALORIZZAZIONI
042500931105     C   21              GOTO      FOR01
042600931105     C* CMD6 - ESEGUO VALORIZZAZIONE
042700020604     c   kf              move      'S'           seF6
042800931105     C   KF              EXSR      VALZZ
042900020408     C   kf              commit
043000931105     C  NKF
043100931105     COR KF
043200931105     CAN 90              GOTO      FORCT4
043300931028     C  N20              GOTO      INIZIO
043400900611     C*
043500000000     C     FINE          TAG
043600100513     C*
043700100513     C*  deve cancellare il record di allocazione x impedire le trasmissioni
043800100513     C*   in sede durante l'utilizzo di questo programma.
043900100513     c     parsml        ifeq      *blank
044000100513     C     Kwalc         setll     fiwalc1l
044100100513     C     Kwalc         reade     fiwalc1l
044200100513     c                   dow       not %eof(fiwalc1l)
044300100513     c                   delete    fiwalc00
044400100513     C     Kwalc         reade     fiwalc1l
044500100513     c                   end
044600100513     c                   end
044700100513      *
044800020408     c                   commit
044900020408     C*
045000020328     C*  deve eseguire un rgzpfm del file di work per le voci di conto
045100020328     C*   economico.
045200020328     C                   clear                   FICNB2
045300020328     C                   move      'C'           §taflg
045400020328     C                   CALL      'FICNB2R'
045500020328     C                   PARM                    FICNB2
045600020328     C*
045700020328     c                   close     fiftdw1l
045800020328     C*
045900020422     c                   if        parsml=' '
046000051109     c                   goto      norgzpfm
046100020328     C                   clear                   MSGDS
046200020328     C                   MOVEL     CMD(1)        MSGDS
046300020328     C                   Z-ADD     80            LENGH
046400020328     C                   CALL      'QCMDEXC'                            99
046500020328     C                   PARM                    MSGDS
046600020328     C                   PARM                    LENGH
046700051109     c     norgzpfm      tag
046800051109     c                   end
046900020328     C*
047000931028     C                   SETON                                        LR
047100950112     C****************************************************************
047200950112     C     *INZSR        BEGSR
047300950112     C*
047400950112     C     *ENTRY        PLIST
047500950112     C                   PARM                    KPJBA
047600950112     C                   MOVEL     KPJBU         PARAM
047700100513      *
047800100513     c     *dtaara       define    §azute        azuteds
047900100513     c     *dtaara       define    §datiute      ddatiute
048000100513     C                   in(E)     *dtaara
048100100513     C                   IF        %Error  or  RSUT = *blanks
048200100513     C                   call      'TIBS34R'
048300100513     C                   parm                    Tibs34Ds
048400100513     C                   in        *dtaara
048500100513     c                   ENDIF
048600950112     C*
048700950112     C                   Z-ADD     1             CODUT
048800950112     C                   CALL      'X§PARUT'
048900950112     C                   PARM                    UT§DSE
049000950112     C                   MOVEL     RAGUT         RSUT             20
049100950112     C                   MOVEL     REC80         CNCR80
049200020328     C*
049300020328     C* apertura workf per ripartizione voci di conto economico
049400020328     c                   open      fiftdw1l
049500950112     C*
049600950112     C* DEFINIZIONE CHIAVI DI ACCESSO
049700100513     C     Kwalc         KLIST
049800100513     C                   KFLD                    WalPGM
049900100513     C                   KFLD                    WalNOJ
050000100513     C                   KFLD                    WalNUS
050100100513     C                   KFLD                    WalNRJ
050200090908      *
050300090908     C     KTbe          KLIST
050400090908     C                   KFLD                    tbeCOD
050500090908     C                   KFLD                    tbeKE1
050600090908     C                   KFLD                    tbeKE2
050700090908      *
050800950112     C     KTAB2         KLIST
050900950112     C                   KFLD                    CODUT
051000950112     C                   KFLD                    COD               2
051100950112     C     KTAB          KLIST
051200950112     C                   KFLD                    CODUT
051300950112     C                   KFLD                    COD               2
051400950112     C                   KFLD                    KEY               8
051500950113     C     KFTT          KLIST
051600950113     C                   KFLD                    VI4PDR
051700950113     C                   KFLD                    VH4TSR
051800950113     C                   KFLD                    VI4NDC
051900950116     C                   KFLD                    VH4DDC
052000950112     C     KFTT1         KLIST
052100950112     C                   KFLD                    W01PDR
052200950112     C                   KFLD                    VI1TSR
052300950112     C                   KFLD                    W01NDC
052400950112     C     KFTT3         KLIST
052500950112     C                   KFLD                    W01PDR
052600950112     C                   KFLD                    VI1TSR
052700060118     C     K21pdr        KLIST
052800060118     C                   KFLD                    VI1fvl
052900060118     C                   KFLD                    W01PDR
053000060118     C     K21pdr1       KLIST
053100060118     C                   KFLD                    VI1fvl
053200060118     C                   KFLD                    W01PDR
053300060118     C                   KFLD                    VI1TSR
053400060118     C                   KFLD                    W01NDC
053500060118     C     K21pdr3       KLIST
053600060118     C                   KFLD                    VI1fvl
053700060118     C                   KFLD                    W01PDR
053800060118     C                   KFLD                    VI1TSR
053900091112     C     Ktgt          KLIST
054000091112     C                   KFLD                    fgtPDR
054100091112     C                   KFLD                    fgtSML
054200091112     C                   KFLD                    fgtPRG
054300950112     C     KFGT          KLIST
054400950112     C                   KFLD                    VI4PDR
054500950112     C                   KFLD                    PARSML
054600950112     C                   KFLD                    VH4TSR
054700950112     C     KFGT2         KLIST
054800950112     C                   KFLD                    VI4PDR
054900950112     C                   KFLD                    PARSML
055000950112     C                   KFLD                    VH4TSR
055100950112     C                   KFLD                    W01CTR
055200040825      *
055300040825     C     KFGT_rif      KLIST
055400040825     C                   KFLD                    rifPDR
055500040825     C                   KFLD                    PARSML
055600040825     C                   KFLD                    VH4TSR
055700040825     C                   KFLD                    rifCTR
055800950112     C     KFTD          KLIST
055900950112     C                   KFLD                    VI4PDR
056000950112     C                   KFLD                    VH4TSR
056100950112     C                   KFLD                    VI4NDC
056200950116     C                   KFLD                    VH4DDC
056300950112     C     KFTD2         KLIST
056400950116     C                   KFLD                    VH6AAS
056500950116     C                   KFLD                    FTDLNP
056600950116     C                   KFLD                    FTDNRS
056700950116     C                   KFLD                    VI6NSP
056800950116     C                   KFLD                    VH4TSR
056900950112     C                   KFLD                    VI4PDR
057000950112     C                   KFLD                    VI4NDC
057100950116     C                   KFLD                    VH4DDC
057200020315     C     KFTD5         KLIST
057300020315     C                   KFLD                    VI4PDR
057400020315     C                   KFLD                    VH4TSR
057500020315     C                   KFLD                    VI4NDC
057600020315     C                   KFLD                    VH4DDC
057700020315     C                   KFLD                    VH6AAS
057800020315     C                   KFLD                    FTDLNP
057900020315     C                   KFLD                    FTDNRS
058000020315     C                   KFLD                    VI6NSP
058100091012     C     Bolla_FTD     KLIST
058200950112     C                   KFLD                    FTDAAS
058300950112     C                   KFLD                    FTDLNP
058400950112     C                   KFLD                    FTDNRS
058500950112     C                   KFLD                    FTDNSP
058600091012      *
058700950112     C     KFTD4         KLIST
058800950112     C                   KFLD                    VI4PDR
058900950112     C                   KFLD                    VH4TSR
059000950112     C                   KFLD                    VI4NDC
059100950116     C                   KFLD                    VH4DDC
059200950112     C                   KFLD                    VI6NSP
059300950116     C                   KFLD                    FTDRSC
059400000622     C     KFTDT         KLIST
059500000622     C                   KFLD                    VI4PDR
059600000622     C                   KFLD                    VH4TSR
059700000714     C                   KFLD                    FT2NDC
059800000622     C                   KFLD                    VH2DDC
059900000714     C     KFTTT         KLIST
060000000714     C                   KFLD                    VI4PDR
060100000714     C                   KFLD                    VI1FVL
060200000714     C                   KFLD                    VH2DDC
060300000714     C                   KFLD                    VH4TSR
060400950112     C     KLBL          KLIST
060500950112     C                   KFLD                    LBLAAN
060600950112     C                   KFLD                    LBLLPN
060700950112     C                   KFLD                    LBLNRN
060800950112     C                   KFLD                    LBLNSN
060900041124      *
061000041124     C     Del_FCE       KLIST
061100041124     C                   KFLD                    fgs_vi4pdr        3 0
061200041124     C                   KFLD                    VI4PDR
061300041124     C                   KFLD                    VH4TSR
061400041124     C                   KFLD                    VI4NDC
061500041124     C                   KFLD                    VH4DDC
061600020326      *
061700020326     c     Kfce          Klist
061800020326     C                   kfld                    ftdfgs
061900020326     C                   kfld                    ftdpdr
062000020326     C                   kfld                    ftdtsr
062100020326     C                   kfld                    ftdndc
062200020326     C                   kfld                    ftdddc
062300020326     C                   kfld                    ftdaas
062400020326     C                   kfld                    ftdlnp
062500020326     C                   kfld                    ftdnrs
062600020326     C                   kfld                    ftdnsp
062700020326      *
062800020326     c     KfceD         Klist
062900020326     C                   kfld                    vh6fgs
063000020326     C                   kfld                    vi4pdr
063100020326     C                   kfld                    vh6tsr
063200020326     C                   kfld                    vi4ndc
063300020326     C                   kfld                    vh4ddc
063400020326     C                   kfld                    vh6aas
063500020326     C                   kfld                    ftdlnp
063600020326     C                   kfld                    ftdnrs
063700020326     C                   kfld                    vh6nsp
063800020328     C*
063900020422     c     KftdW0        Klist
064000020328     C                   kfld                    kftwfgs
064100020328     C                   kfld                    kftwpdr
064200020328     C                   kfld                    kftwtsr
064300020328     C                   kfld                    kftwndc
064400020328     C                   kfld                    kftwddc
064500020422     C*
064600020422     c     KftdW         Klist
064700020422     C                   kfld                    kftwfgs
064800020422     C                   kfld                    kftwpdr
064900020422     C                   kfld                    kftwtsr
065000020422     C                   kfld                    kftwndc
065100020422     C                   kfld                    kftwddc
065200020328     C                   kfld                    kftwstp
065300021202     c     Kapdv         Klist
065400021202     C                   kfld                    apdtip
065500021202     C                   kfld                    vi1p1
065600021202     c     Kapdf         Klist
065700021202     C                   kfld                    apdtip
065800021202     C                   kfld                    fttpdr
065900021202     c                   eval      apdtip = 'A'
066000950112     C***
066100950112     C* 20 ON - PROGRAMMA RICHIAMATO
066200950112     C     PARTSR        IFNE      ' '
066300950112     C                   EXSR      PUL01
066400950112     C                   SETON                                        20
066500950112     C                   MOVEL     'V'           VI1FVL
066600950112     C                   MOVEL     PARPDR        VI1P1
066700950112     C                   MOVEL     PARTSR        VI1TSR
066800950112     C                   ENDIF
066900950112     C***
067000950112     C* 06 ON - FASE DI SIMULAZIONE
067100950112     C     PARSML        IFEQ      'S'
067200950112     C                   SETON                                        06
067300950112     C                   ENDIF
067400950112     C***
067500950112     C* CARICO TABELLA FILIALI GESTITE £1
067600950112     C                   CLEAR                   DSUL06
067700950112     C                   MOVE      '£1'          D06COD
067800020717     C                   MOVEL     SIMPOU        D06KEY
067900950112     C                   MOVEL     ' '           D06TLA
068000100513     C                   MOVEL     KPJBU         kpjbus
068100100513     C                   clear                   kpjbu
068200950112     C                   MOVEL     DSUL06        KPJBU
068300950112     C                   CALL      'TRUL06R'
068400950112     C                   PARM                    KPJBA
068500950112     C                   MOVEL     KPJBU         DSUL06
068600950112     C                   MOVEA     LIN           L1
068700100513     C                   MOVEL     KPJBUs        kpjbu
068800950112     C***
068900950112     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
069000950112     C                   CLEAR                   DSUL06
069100950112     C                   MOVE      '£6'          D06COD
069200020717     C                   MOVEL     SIMPOU        D06KEY
069300950112     C                   MOVEL     'L'           D06TLA
069400100513     C                   MOVEL     KPJBU         kpjbus
069500100513     C                   clear                   kpjbu
069600950112     C                   MOVEL     DSUL06        KPJBU
069700950112     C                   CALL      'TRUL06R'
069800950112     C                   PARM                    KPJBA
069900950112     C                   MOVEL     KPJBU         DSUL06
070000950112     C                   MOVEA     LIN           L6
070100100513     C                   MOVEL     KPJBUs        kpjbu
070200950112     C*
070300950112     C***
070400950112     C* CARICO TABELLA DEI TIPI PRESTAZIONE
070500950112     C                   Z-ADD     1             C
070600950112     C                   MOVEL     '1Z'          COD
070700950112     C     KTAB2         SETLL     TABEL
070800950112     C     KTAB2         READE     TABEL                                  30
070900950112     C*
071000950112     C     *IN30         DOWEQ     *OFF
071100950112     C     TBLFLG        IFEQ      ' '
071200950112     C                   MOVEL     TBLUNI        DS1Z
071300950112     C     §1ZUPD        IFEQ      'S'
071400950112     C                   MOVEL     TBLKEY        TSR(C)
071500950112     C                   MOVEL     §1ZDES        DTS(C)
071600950112     C                   MOVEL     §1ZCTR        CTS(C)
071700950112     C                   ADD       1             C
071800950112     C                   ENDIF
071900950112     C                   ENDIF
072000950112     C     KTAB2         READE     TABEL                                  30
072100950112     C                   ENDDO
072200950112     C***
072300950112     C* CARICO MANCATE CONSEGNE DA TASSARE
072400950112     C                   Z-ADD     1             C
072500950112     C                   MOVEL     '8B'          COD
072600950112     C     KTAB2         SETLL     TABEL
072700950112     C     KTAB2         READE     TABEL                                  30
072800950112     C*
072900950112     C     *IN30         DOWEQ     *OFF
073000950112     C     TBLFLG        IFEQ      ' '
073100950112     C                   MOVEL     TBLKEY        CMC(C)
073200950112     C                   MOVE      TBLKEY        WALF3             3
073300950112     C                   MOVE      WALF3         CMC(C)
073400950112     C                   ADD       1             C
073500950112     C                   ENDIF
073600950112     C     KTAB2         READE     TABEL                                  30
073700950112     C                   ENDDO
073800950112     C***
073900950112     C* CARICO I TIPI BOLLA DA NON TASSARE
074000950112     C                   Z-ADD     1             C
074100950112     C                   MOVEL     '8C'          COD
074200950112     C     KTAB2         SETLL     TABEL
074300950112     C     KTAB2         READE     TABEL                                  30
074400950112     C*
074500950112     C     *IN30         DOWEQ     *OFF
074600950112     C     TBLFLG        IFEQ      ' '
074700950112     C                   MOVEL     TBLKEY        CBL(C)
074800950112     C                   MOVE      TBLKEY        WALF3             3
074900950112     C                   MOVE      WALF3         CBL(C)
075000950112     C                   ADD       1             C
075100950112     C                   ENDIF
075200950112     C     KTAB2         READE     TABEL                                  30
075300950112     C                   ENDDO
075400090908      *----
075500090909     C                   Z-ADD     0             C
075600090908     C                   movel(P)  'CTP'         tbeCOD
075700090908     c                   clear                   KCliPar
075800090908     c                   clear                   KtutteBolle
075900090908     C     tbeCOD        setll     tntbe01l
076000090908     C     tbeCOD        reade     tntbe01l
076100090908      * consegne/ritiri
076200090908     c                   dow       not %eof(tntbe01l)
076300090909     C                   ADD       1             C
076400090908     C                   movel     tbeuni        dCTP
076500090908     C                   MOVEL     TBeKE1        KCliPar(C)
076600090908     C     tbeCOD        reade     tntbe01l
076700090908     c                   enddo
076800950112     C***
076900950112     C* DEFINIZIONE CAMPI
077000950112     C     *LIKE         DEFINE    TBLKEY        §KEY
077100950112     C     *LIKE         DEFINE    TBLCOD        §COD
077200950112     C     *LIKE         DEFINE    TBLKUT        §KUT
077300950112     C* AAMMGG DATA IMMESSA A VIDEO
077400950112     C     *LIKE         DEFINE    FTTDDC        W01DDC
077500950127     C     *LIKE         DEFINE    VI4DDC        WM1DDC
077600950112     C* DATA DI ELABORAZIONE (PUO' ESSERECENE SOLO UNA)
077700950112     C     *LIKE         DEFINE    FTTDDC        W02DDC
077800020129     C     *like         define    fttndc        w02ndc
077900011112     C     *like         define    fttdiv        w02div
078000950112     C     *LIKE         DEFINE    FTTNDC        W01NDC
078100950112     C     *LIKE         DEFINE    FTTPDR        W01PDR
078200011105      *
078300950112     C     *LIKE         DEFINE    FTTPDR        W02PDR
078400950112     C     *LIKE         DEFINE    FTTTSR        W01TSR
078500950112     C     *LIKE         DEFINE    FTTSNA        W01SNA
078600020212     C     *LIKE         DEFINE    FTTSNA        g01SNA
078700950112     C     *LIKE         DEFINE    FTTKFA        W01KFA
078800020212     C     *LIKE         DEFINE    FTTKFA        g01KFA
078900950112     C     *LIKE         DEFINE    FTTCLA        W01CLA
079000020212     C     *LIKE         DEFINE    FTTCLA        g01CLA
079100950118     C     *LIKE         DEFINE    FTTCPE        W01CPE
079200020212     C     *LIKE         DEFINE    FTTCPE        g01CPE
079300950112     C     *LIKE         DEFINE    FTTTVL        W01TVL
079400020212     C     *LIKE         DEFINE    FTTTVL        g01TVL
079500950112     C     *LIKE         DEFINE    FTTITA        W01ITA
079600950112     C     *LIKE         DEFINE    FTTITT        W01ITT
079700950112     C     *LIKE         DEFINE    FTTFIT        W01FIT
079800950112     C     *LIKE         DEFINE    FTTFIA        W01FIA
079900950119     C     *LIKE         DEFINE    FTTSNE        W01SNE
080000950112     C     *LIKE         DEFINE    FTTSNP        W01SNP
080100950112     C     *LIKE         DEFINE    FTDNCL        W01NCL
080200950112     C     *LIKE         DEFINE    FTDSTP        W01STP
080300950112     C     *LIKE         DEFINE    FTDSTP        W02STP
080400950112     C     *LIKE         DEFINE    FTDSPC        W01SPC
080500950112     C     *LIKE         DEFINE    FTDSPR        W01SPR
080600950112     C     *LIKE         DEFINE    NRR           W01NRR
080700950112     C     *LIKE         DEFINE    NRR           W04NRR
080800950112     C     *LIKE         DEFINE    NRR           W06NRR
080900950112     C     *LIKE         DEFINE    NRR           NRR4
081000950112     C     *LIKE         DEFINE    NRR           NRR6
081100011105     C     *LIKE         DEFINE    NRR           NRW6
081200950112     C     *LIKE         DEFINE    FGTCTR        W01CTR
081300950112     C     *LIKE         DEFINE    FTDCTP        W01CTP
081400020325     C     *like         define    ftdFIN        w01isola
081500020325     C     *like         define    arbtsp        w01xpres
081600020328     C     *like         define    ftwfgs        kftwfgs
081700020328     C     *like         define    ftwpdr        kftwpdr
081800020328     C     *like         define    ftwtsr        kftwtsr
081900020328     C     *like         define    ftwndc        kftwndc
082000020328     C     *like         define    ftwddc        kftwddc
082100020328     C     *like         define    ftwstp        kftwstp
082200020422     C*
082300020422     C     *like         define    ftdfgs        sv1fgs
082400020422     C     *like         define    ftdpdr        sv1pdr
082500020422     C     *like         define    ftdtsr        sv1tsr
082600020422     C     *like         define    ftdndc        sv1ndc
082700020422     C     *like         define    ftdddc        sv1ddc
082800020328     C*
082900030821     C     *like         define    §t1NSS        W01nss
083000030821     C     *like         define    §t1FW         W01fw
083100030821     C     *like         define    §t1RCC        W01rcc
083200030821     C     *like         define    ftdSTP        savSTP
083300030821     c     *like         define    W01nss        SAVnss
083400030821     c     *like         define    W01fw         SAVfw
083500030821     c     *like         define    W01rcc        SAVrcc
083600030821     C*
083700950112     C                   Z-ADD     0             B                 2 0
083800040112     C                   Z-ADD     0             C                 5 0
083900950112     C                   TIME                    UTIME             6 0
084000040622     C***
084100040622     C                   TIME                    W0140            14 0
084200040622     C                   MOVEl     W0140         Wora              6 0
084300040622     C                   MOVE      W0140         WDAT              8 0
084400040622     C*
084500040622     C                   Z-ADD     Wdat          G02DAT
084600040622     C                   MOVEL     *BLANK        G02ERR
084700040622     C                   CALL      'XSRDA8'
084800040622     C                   PARM                    WLBDAT
084900040622     C* UDATE A 8 IN AAAA/MM/GG
085000040622     C                   move      G02INV        dataoggi
085100090511      **
085200100513     C* scrive il record di allocazione x trasmissione
085300100513     c     parsml        ifeq      *blank
085400100513     C                   clear                   Fiwalc00
085500100513     C                   movel     w0140         walOrc
085600100513     C                   movel     g02inv        waldac
085700100513     C                   eval      WalSIF = KNSIF
085800100513     C                   eval      WalPGM = 'FICNB1R   '
085900100513     C                   eval      WalNOJ = KNMEB
086000100513     C                   eval      WalNUS = KNMUS
086100100513     C                   eval      WalNRJ = KNRJO
086200100513     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
086300100513     C                   write     Fiwalc00
086400100513     c                   feod      Fiwalc1L
086500100513     c                   end
086600090511     C***
086700950112     C                   ENDSR
086800020212     C*---------------------------------------------------------------*
086900931103     C* PULISCO 1 FORMATO --------------------------------------------*
087000020212     C*---------------------------------------------------------------*
087100931103     C     PUL01         BEGSR
087200931103     C                   CLEAR                   NDC
087300931103     C                   CLEAR                   VI1P1
087400931103     C                   CLEAR                   VI1P2
087500931103     C                   CLEAR                   VI1DDC
087600931103     C                   CLEAR                   VI1FVL
087700931103     C                   CLEAR                   VI1TSR
087800020717     C                   Z-ADD     SIMPOU        VI1PF1
087900950112     C                   ENDSR
088000020212     C*---------------------------------------------------------------*
088100900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
088200020212     C*---------------------------------------------------------------*
088300900528     C     CONTR1        BEGSR
088400931103     C                   SETOFF                                       90
088500931103     C                   CLEAR                   W01DDC
088600931103     C                   CLEAR                   W02DDC
088700020129     C                   clear                   w02ndc
088800011112     C                   clear                   w02div
088900931119     C* T I P O   P R E S T A Z I O N E
089000931021    1C     VI1TSR        IFNE      ' '
089100931021    2C     VI1TSR        IFEQ      '?'
089200931015     C                   MOVEL     CODUT         §KUT
089300931015     C                   MOVEL     '1Z'          §COD
089400931015     C                   MOVE      ' '           VI1TSR
089500931015     C                   MOVEL     *BLANKS       §KEY
089600931015     C                   CALL      'X§TABER'
089700931015     C                   PARM                    §KUT
089800931015     C                   PARM                    §COD
089900931015     C                   PARM                    §KEY
090000931015     C                   PARM                    §DES             30
090100931015     C                   MOVEL     §KEY          VI1TSR
090200931015     C                   SETON                                        90
090300931015     C                   GOTO      ENDCT1
090400931021    2C                   ENDIF
090500931018     C*
090600931015     C* 30 ON - INESISTENTE O ANNULLATO
090700931018     C     VI1TSR        LOOKUP    TSR                                    30
090800931018     C  N30              SETON                                        4090
090900931018     C  N30              GOTO      ENDCT1
091000931021    1C                   ENDIF
091100931015     C*
091200950112     C* F I L I A L E   P A D R O N C I N O
091300950112     C     VI1PF1        IFEQ      0
091400950112     C                   SETON                                        4190
091500950119     C                   GOTO      ENDCT1
091600950112     C                   ELSE
091700020717     C* Verifico esistenza filiale padroncino: se 2° LIV. su £6, se 1° LIV.
091800950112     C* su £1
091900020717     C                   IF        SIMTPP = '2'
092000950112     C     VI1PF1        LOOKUP    L6                                     30
092100950112     C  N30              SETON                                        4190
092200950119     C  N30              GOTO      ENDCT1
092300950112     C                   ELSE
092400950112     C     VI1PF1        LOOKUP    L1                                     30
092500950112     C  N30              SETON                                        4190
092600950119     C  N30              GOTO      ENDCT1
092700950112     C                   END
092800950112     C                   END
092900931018     C*
093000931018     C* P A D R O N C I N O   DAL AL
093100931021    1C     VI1PD2        IFGT      0
093200931119     C     VI1PD2        ANDNE     VI1PD1
093300931119     C*
093400931021    2C     VI1PD2        IFLT      VI1PD1
093500931015     C                   SETON                                        4290
093600090508     C                   GOTO      ENDCT1
093700931021    2C                   ENDIF
093800931015     C*
093900931021   X1C                   ELSE
094000931018     C* SE SOLO DAL CONTROLLO L'ESISTENZA IN TABELLA PADRONCINI
094100931021    2C     VI1PD1        IFGT      0
094200931110     C*
094300021202     C     kapdv         CHAIN     fiapd01L                           30
094400950123     C     *IN30         IFEQ      *ON
094500931015     C                   SETON                                        30
094600931021    3C                   ENDIF
094700931015     C*
094800931015     C   30              SETON                                        4390
094900931015     C   30              GOTO      ENDCT1
095000931021    2C                   ENDIF
095100931021    1C                   ENDIF
095200931018     C*
095300931018     C* D A T A   D I S T I N T A
095400931021    1C     VI1DDC        IFGT      0
095500950112     C                   Z-ADD     VI1DDC        G02DAT
095600931018     C                   MOVEL     *BLANK        G02ERR
095700950112     C                   CALL      'XSRDA8'
095800931018     C                   PARM                    WLBDAT
095900931021    2C     G02ERR        IFEQ      '1'
096000931018     C                   SETON                                        44  90
096100931018     C                   GOTO      ENDCT1
096200931021    2C                   END
096300931018     C                   Z-ADD     G02INV        W01DDC
096400931026    1C                   ENDIF
096500950113     C* SE IMMESSO UN SOLO NUMERO DISTINTA IMMETTERE ANCHE IL TIPO SERV
096600931018     C                   SORTA     NDC
096700931026    1C     NDC(10)       IFGT      0
096800931018     C     NDC(9)        ANDEQ     0
096900931018     C     VI1TSR        ANDEQ     ' '
097000931018     C                   SETON                                        45  90
097100931018     C                   GOTO      ENDCT1
097200931026    1C                   ENDIF
097300910320     C*
097400950112     C  N90              MOVEL     VI1PF1        VI1PF2
097500950112     C*
097600900614     C     ENDCT1        ENDSR
097700020212     C*---------------------------------------------------------------*
097800931018     C*--- CARICO DISTINTE SELEZIONATE -------------------------------*
097900020212     C*---------------------------------------------------------------*
098000931018     C     CARNDC        BEGSR
098100950113     C                   SETOFF                                       0102
098200931018     C                   SETOFF                                       040531
098300931105     C                   SETOFF                                       21
098400931018     C                   Z-ADD     0             NRR               5 0
098500931018     C                   Z-ADD     0             W01NRR
098600931018     C                   CLEAR                   VI1VAL
098700931119     C                   CLEAR                   WDATA
098800931018     C* PULIZIA SUBFILE
098900931018     C                   SETON                                        35
099000931018     C                   WRITE     FRB8CT2
099100931018     C                   SETOFF                                       35
099200931018     C*
099300931018     C* 01 ON - IMMESSO SOLO PADRONCINO DAL
099400931018     C* 02 ON - IMMESSI PADRONCINI DAL AL
099500931108     C* 04 ON - IMMESSO TIPO PRESTAZIONE
099600950113     C* 05 ON - IMMESSO SOLO UN NUMERO DISTINTA (CONSEGNE)
099700931018     C*
099800931108     C* TIPO PRESTAZ SELEZIONATO
099900931018     C     VI1TSR        IFNE      ' '
100000931018     C                   SETON                                        04
100100931018     C                   ENDIF
100200931018     C* PADRONCINO
100300931018     C     VI1PD2        IFGT      0
100400931018     C     VI1P2         ANDNE     VI1P1
100500931018     C                   SETON                                        02
100600931018     C                   ELSE
100700931018     C*
100800931018     C     VI1PD1        IFGT      0
100900931018     C                   SETON                                        01
101000950113     C* IMMESSO SOLO 1 NUMERO DISTINTA E TIPO SERIVZIO = 'C'
101100931018     C     NDC(10)       IFGT      0
101200931018     C     NDC(9)        ANDEQ     0
101300950113     C     VI1TSR        ANDEQ     'C'
101400931018     C                   SETON                                        05
101500931018     C                   MOVEL     NDC(10)       W01NDC
101600931018     C                   ENDIF
101700931018     C*
101800931018     C                   ENDIF
101900931018     C                   ENDIF
102000931018     C                   Z-ADD     VI1P1         W01PDR
102100931018     C* NUMERI DISTINTA
102200931018     C                   XFOOT     NDC           WNUM9             9 0
102300931018     C* FLAG VALORIZZAZIONE
102400931018     C     VI1FVL        IFEQ      ' '
102500931018     C                   MOVEL     CDAVAL        VI1VAL
102600931018     C                   ELSE
102700931018     C                   MOVEL     CVALOR        VI1VAL
102800931018     C                   ENDIF
102900921229     C**
103000931018     C* POSIZIONAMENTO PER LETTURA
103100931018     C**
103200931018     C                   SELECT
103300060118      **
103400931108     C** NON IMMESSO TIPO PRESTAZ: LEGGO SOLO PER PADRONCINO
103500931018     C     *IN04         WHENEQ    *OFF
103600931018     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
103700931018     C     VI1PD1        IFGT      0
103800060118     C     k21pdr        SETLL     fiftt21l
103900931018     C                   ELSE
104000931018     C                   EXSR      IMPO1
104100931018     C                   ENDIF
104200931018     C*
104300931018     C                   OTHER
104400950113     C** 01 ON  - LEGGO PER PADRONCINO/TIPO SERVIZIO/NUMERO/DATA DISTIN
104500950113     C**              O PER PADRONCINO/TIPO SERVIZIO
104600950113     C** 01 OFF - LEGGO PER PADRONCINO (TUTTI O NEL DAL-AL)/TIPO SERVIZ
104700060118      **
104800931018     C     *IN01         IFEQ      *ON
104900060118     C   05K21pdr1       CHAIN(N)  fiftt21l                           31
105000060118     C  N05K21pdr3       SETLL     fiftt21l
105100060118      **
105200931018     C                   ELSE
105300950113     C* SE NON IMMESSO DEVO CERCARE IL PRIMO
105400950113     C     VI1PD1        IFGT      0
105500060118     C     k21PDR        SETLL     fiftt21l
105600950113     C                   ELSE
105700931018     C                   EXSR      IMPO1
105800931018     C                   ENDIF
105900060118      **
106000950113     C                   ENDIF
106100060118      **
106200931018     C                   ENDSL
106300931018     C*
106400931018     C* LETTURA DISTINTE/RITIRI
106500931018     C     *IN31         IFEQ      *OFF
106600931018     C                   EXSR      LEGNDC
106700931018     C                   ELSE
106800931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
106900931018     C                   SETON                                        4690
107000931119     C                   MOVEL     ERR(1)        VI1ERR
107100030519      * solo in simulazione: (eccezione)
107200030519      *  se non c'è la tariffa non carica le distinte
107300030519      *  quindi non è valido il messaggio precedente
107400030519     c     parsml        ifeq      'S'
107500030519     c     §t3ctr        andeq     '???'
107600030519     C                   clear                   VI1ERR
107700030519     c     §t3tsr        ifeq      'C'
107800030519     C                   MOVEL     ERR(2)        VI1ERR
107900030519     c                   end
108000030519     c     §t3tsr        ifeq      'R'
108100030519     C                   MOVEL     ERR(3)        VI1ERR
108200030519     c                   end
108300030519     c                   end
108400931018     C                   ENDIF
108500931018     C*
108600900608     C                   ENDSR
108700020212     C*---------------------------------------------------------------*
108800931018     C* IMPOSTAZIONE LETTURA PER 04 OFF ------------------------------*
108900020212     C*---------------------------------------------------------------*
109000931018     C     IMPO1         BEGSR
109100060118      **
109200060118     C     k21PDR        setgt     fiftt21l
109300060118     C                   read(N)   fiftt21l                               31
109400950202     C*
109500950202     C     *IN31         IFEQ      *OFF
109600950202     C                   MOVEL     FTTPDR        W03               3 0
109700950202     C     VI1PF1        IFNE      W03
109800950202     C                   SETON                                        31
109900950202     C                   END
110000950202     C                   END
110100931018     C*
110200931018     C     *IN31         IFEQ      *OFF
110300931018     C                   Z-ADD     FTTPDR        W01PDR
110400060118     C  N04k21pdr        setll     fiftt21l
110500060118     C   04K21pdr3       setll     fiftt21l
110600931018     C                   ENDIF
110700060118      **
110800931018     C                   ENDSR
110900020212     C*---------------------------------------------------------------*
111000931018     C* LETTURA DITINTE DA CARICARE ----------------------------------*
111100020212     C*---------------------------------------------------------------*
111200931018     C     LEGNDC        BEGSR
111300931018     C                   CLEAR                   W01TSR
111400931018     C                   CLEAR                   W02PDR
111500931018     C* L E T T U R A
111600940325    1C     *IN31         DOWEQ     *OFF
111700931018     C                   SETOFF                                       9032
111800931018     C                   SELECT
111900931018     C** LEGGO PER PADRONCINO (TUTTO O NEL DAL-AL)
112000940325    2C     *IN04         WHENEQ    *OFF
112100931018     C*
112200940325    3C     *IN32         DOWEQ     *OFF
112300060118     C     k21pdr        READE(N)  fiftt21l                               31
112400931018     C*
112500940325    4C     *IN31         IFEQ      *ON
112600931018     C     *IN01         ANDEQ     *OFF
112700931018     C                   EXSR      IMPO1
112800931018     C   31              SETON                                        32
112900931018     C                   ELSE
113000931018     C                   SETON                                        32
113100940325    4C                   ENDIF
113200940325    3C                   ENDDO
113300931108     C** LEGGO TUTTI I PADRONCINI(O NEL DAL-AL) E TIPO PRESTAZIONE
113400940325    2C     *IN01         WHENEQ    *OFF
113500931018     C*
113600940325    3C     *IN32         DOWEQ     *OFF
113700060118     C     K21pdr3       READE(N)  fiftt21l                               31
113800931018     C*
113900940325    4C     *IN31         IFEQ      *ON
114000931018     C                   EXSR      IMPO1
114100931018     C   31              SETON                                        32
114200931018     C                   ELSE
114300931018     C                   SETON                                        32
114400940325    4C                   ENDIF
114500940325    3C                   ENDDO
114600060118      *
114700931108     C** LEGGO PER PADRONCINO TIPO PRESTAZIONE
114800940325    2C     *IN01         WHENEQ    *ON
114900931018     C     *IN05         ANDEQ     *OFF
115000060118     C     K21pdr3       READE(N)  fiftt21l                               31
115100931018     C*
115200931018     C                   ENDSL
115300931018     C*
115400931018     C* C O N F R O N T O   L  E   S E L E Z I O N I
115500020606     C     *IN31         IFEQ      *OFF
115600020606     C                   EXSR      CONFR
115700931018     C*
115800931018     C* C A R I C A M E N T O   S F L
115900900608     C* 90 ON - RECORD DA NON SELEZIONARE
116000020606     C     *IN90         ifeq      *OFF
116100931018     C*
116200931018     C                   CLEAR                   VI2SCE
116300011105     C                   clear                   vh2div
116400931028     C                   CLEAR                   VH2ITT
116500931028     C                   CLEAR                   VH2ITA
116600931018     C*
116700931018     C     FTTPDR        IFNE      W02PDR
116800020524     C                   CLEAR                   saltasf2          1
116900931103     C                   SETOFF                                       12
117000931103     C                   MOVEL     '0'           VH2I12
117100021202     C     kapdf         CHAIN     fiapd01L                           30
117200950117     C     *IN30         IFEQ      *OFF
117300100223     C                   MOVEL     APDRSf        VI2DPD
117400950117     C                   MOVEL     APDETM        VH2ETM
117500950117     C                   MOVEL     APDETP        VH2ETP
117600950117     C                   MOVEL     APDPKM        VH2PKM
117700950117     C                   MOVEL     APDPKP        VH2PKP
117800021211     C                   eval      vh2cam = apdCAM
117900021211     C                   eval      vh2cap = apdCAP
118000021211     C                   eval      vh2scm = apdSCM
118100021211     C                   eval      vh2scp = apdSCP
118200021211     C                   z-add     apdHAR        vh2HAR
118300021211     C                   z-add     apdHPA        vh2HPA
118400020524     C                   movel     APDpdd        saltasf2
118500950117     C                   ELSE
118600950117     C                   CLEAR                   VI2DPD
118700950117     C                   CLEAR                   VH2ETM
118800950117     C                   CLEAR                   VH2ETP
118900950117     C                   CLEAR                   VH2PKM
119000950117     C                   CLEAR                   VH2PKP
119100021211     C                   clear                   vh2CAM
119200021211     C                   clear                   vh2CAP
119300021211     C                   clear                   vh2SCM
119400021211     C                   clear                   vh2HAR
119500021211     C                   clear                   vh2HPA
119600021211     C                   clear                   vh2SCP
119700950117     C                   END
119800931018     C                   MOVEL     FTTPDR        VI2PDR
119900931018     C                   MOVEL     FTTPDR        W02PDR
120000931018     C                   ELSE
120100931103     C                   SETON                                        12
120200931103     C                   MOVEL     '1'           VH2I12
120300931018     C                   ENDIF
120400931108     C* TIPO PRESTAZIONE
120500931018     C     FTTTSR        IFNE      W01TSR
120600931018     C                   MOVEL     FTTTSR        K17(1)
120700931018     C                   MOVEL     '-'           K17(2)
120800931018     C                   Z-ADD     1             C
120900931018     C     FTTTSR        LOOKUP    TSR(C)                                 30
121000931018     C   30              MOVEA     DTS(C)        K17(3)
121100931018     C  N30              MOVEA     *BLANKS       K17(3)
121200931018     C*
121300931018     C                   MOVEA     K17           VI2TSR
121400931018     C                   MOVEL     FTTTSR        W01TSR
121500931018     C                   ENDIF
121600931018     C*
121700931119     C                   MOVEL     FTTNDC        VI2NDC                          NUMERO
121800950113     C                   MOVE      FTTDDC        DATA
121900950113     C                   MOVE      FTTDDC        VI2DDC                         DATA
122000931119     C                   MOVE      AA            VI2DDC
122100950113     C                   MOVEL     GG            VI2DDC
122200931119     C                   MOVEL     FTTDDC        VH2DDC
122300931027     C     FTTFIT        IFNE      ' '
122400011105     C                   Z-ADD     FTTITT        VH2ITT
122500931027     C                   ENDIF
122600931027     C     FTTFIA        IFNE      ' '
122700931027     C                   Z-ADD     FTTITA        VH2ITA
122800931027     C                   ENDIF
122900011105     C                   movel     fttdiv        vh2div
123000950117     C                   MOVEL     FTTFPP        VH2FPP
123100950117     C                   MOVEL     FTTPEP        VH2PEP
123200021211     C                   eval      vh2FCS = fttLVL
123300021217     C                   z-add     fttTBS        vh2TBS
123400900608     C*
123500020603     C*  Prova a cercare se in simulazione la tariffa legata al documento
123600020603     c     parsml        ifeq      'S'
123700030516     C*
123800020603     C* Routine esterna per restituire il codice tariffa del documento
123900020603     C                   clear                   ficnb3
124000030516     C*
124100030516     C* Attenzione se ci sono dei Ritiri Annullati, deve impostare il parametro
124200030516     C*  RAN con (S)
124300030516     c                   if        ftttsr ='R' and ftttbs > 0
124400030516     C                   movel     'S'           §t3RAN
124500030516     c                   end
124600020603     C                   movel     fttpdr        §t3pdr
124700020603     C                   movel     parsml        §t3sml
124800020603     C                   movel     ftttsr        §t3tsr
124900020603     C                   z-add     fttndc        §t3ndc
125000020603     C                   z-add     fttddc        §t3ddc
125100020603     C                   movel     fttdiv        §t3div
125200020603     C                   movel     *blank        §t3ctr
125300020605     C                   movel     vi1fvl        §t3fvl
125400020603     C                   CALL      'FICNB3R1'
125500020603     C                   PARM                    ficnb3
125600020603      *
125700020603     c                   endif
125800020603     C*
125900020524     C* se il padroncino è stato escluso dalla valorizzazione
126000020524     C* comunque in simulazione deve sempre permettere la valorizzazione
126100020603     c                   move      'S'           nowrite           1
126200020603      *
126300020603     c     saltasf2      ifeq      'S'
126400020603     c     parsml        andeq     *blank
126500020603      *
126600020524     c     parsml        oreq      'S'
126700020603     c     §t3ctr        andeq     '???'
126800020603     c     saltasf2      andeq     'S'
126900020603     c                   move      'N'           nowrite
127000020603     c                   endif
127100020524      *
127200020524     C* questo da quando si è voluto attivare le simulazioni per quei
127300020524     C* padroncini che non devono avere l'autofatturazione ma che occorre
127400020524     C* simulare comunque.
127500020603     c                   if        nowrite ='S'
127600020606     C*
127700020606     C* CONTROLLO QUANTE DATE CARICO
127800020606    1C     W02DDC        IFEQ      0
127900020606     C                   Z-ADD     FTTDDC        W02DDC
128000020606     C                   z-add     fttndc        w02ndc
128100020606     C                   movel     fttdiv        w02div
128200020606   X1C                   ELSE
128300020606     C* PRESENTI ALMENO 2 DATE
128400020606    2C     FTTDDC        IFNE      W02DDC
128500020606     C                   MOVEL     '1'           WDATA             1
128600020606    2C                   ENDIF
128700020606    1C                   ENDIF
128800020606     C*
128900900608     C                   ADD       1             NRR
129000931018     C                   WRITE     FRB8SF2
129100020603     c                   endif
129200020524      *
129300931018     C                   ENDIF
129400020606      *
129500020606     C                   ENDIF
129600940325     C*
129700940325     C* ESSENDO CHAIN SECCA, DOPO AVER LETTO UNA VOLTA ESCO
129800940325     C   01
129900940325     CAN 05              SETON                                        31
130000931018     C                   ENDDO
130100900521     C*
130200931018     C* NON  ESISTONO RECORD DA ELABORARE PER LA SELEZIONE FATTA
130300931119     C     NRR           IFEQ      0
130400931018     C                   SETON                                        4690
130500931119     C                   MOVEL     ERR(1)        VI1ERR
130600030519      * solo in simulazione: (eccezione)
130700030519      *  se non c'è la tariffa non carica le distinte
130800030519      *  quindi non è valido il messaggio precedente
130900030519     c     parsml        ifeq      'S'
131000030519     c     §t3ctr        andeq     '???'
131100030519     C                   clear                   VI1ERR
131200030519     c     §t3tsr        ifeq      'C'
131300030519     C                   MOVEL     ERR(2)        VI1ERR
131400030519     c                   end
131500030519     c     §t3tsr        ifeq      'R'
131600030519     C                   MOVEL     ERR(3)        VI1ERR
131700030519     c                   end
131800030519     c                   end
131900931018     C                   ELSE
132000931018     C                   Z-ADD     NRR           W01NRR
132100931119     C                   SETOFF                                       90
132200931018     C                   ENDIF
132300900509     C*
132400900518     C                   ENDSR
132500020212     C*---------------------------------------------------------------*
132600931018     C* CONFRONTO RECORD LETTI CON SELEZIONI EFFETTUATE --------------*
132700020212     C*---------------------------------------------------------------*
132800931018     C     CONFR         BEGSR
132900931108     C* TIPO PRESTAZIONE
133000931105     C   04FTTTSR        IFNE      VI1TSR
133100931105     C                   SETON                                        90
133200931105     C                   GOTO      ENDCON
133300931105     C                   ENDIF
133400931105     C* FLAG VALORIZZAZIONE
133500931105     C     FTTFVL        IFNE      VI1FVL
133600931105     C                   SETON                                        90
133700931105     C                   GOTO      ENDCON
133800931105     C                   ENDIF
133900931018     C* SE PADRONCINO SUPERA L'AL --> FINE LETTURA
134000931018     C   02FTTPDR        IFGT      VI1P2
134100931018     C                   SETON                                        3190
134200931018     C                   GOTO      ENDCON
134300931018     C                   ENDIF
134400931018     C* DATA DISTINTA
134500931018     C     VI1DDC        IFGT      0
134600931018     C     FTTDDC        ANDGT     W01DDC
134700931018     C                   SETON                                        90
134800931018     C                   GOTO      ENDCON
134900931018     C                   ENDIF
135000931028     C*
135100931028     C* PGM RICHIAMATO: CONTROLLO DATA DISTINTA
135200931028     C   20FTTDDC        IFNE      PARDDC
135300931028     C                   SETON                                        90
135400931028     C                   GOTO      ENDCON
135500931028     C                   ENDIF
135600950113     C* NUMERI DISTINTA O RITIRO E SCELTO UN SOLO NUMERO DISTINTA
135700931018     C     WNUM9         IFGT      0
135800950113     C     NDC(10)       ORGT      0
135900950113     C     NDC(9)        ANDEQ     0
136000950113     C     VI1TSR        ANDEQ     'R'
136100931018     C     FTTNDC        LOOKUP    NDC                                    30
136200931018     C  N30              SETON                                        90
136300931018     C  N30              GOTO      ENDCON
136400931018     C                   ENDIF
136500931018     C*
136600931018     C     ENDCON        ENDSR
136700020212     C*---------------------------------------------------------------*
136800931018     C* SELEZIONE DISTINTE DA VALORIZZARE ----------------------------*
136900020212     C*---------------------------------------------------------------*
137000931018     C     SELNDC        BEGSR
137100931105     C                   SETOFF                                       90
137200931018     C                   Z-ADD     1             NRR
137300931018     C                   Z-ADD     0             NRR4
137400931018     C                   Z-ADD     0             W04NRR
137500931018     C                   CLEAR                   W02PDR
137600931018     C                   CLEAR                   VI4SCE
137700931119     C* PRESENTI PIU' DATE
137800931119    1C     WDATA         IFNE      ' '
137900931119     C* SE CMD1 ERRORE
138000931119    2C     *INKA         IFEQ      *ON
138100931119     C                   Z-ADD     1             REC2
138200931119     C                   SETON                                        4090
138300931119     C                   GOTO      ENDSEL
138400931119     C*
138500931119   X2C                   ELSE
138600931119     C*
138700931119     C* SE ENTER CONTROLLO LE SELEZIONI FATTE
138800931123     C                   CLEAR                   W02DDC
138900020129     C                   clear                   w02ndc
139000011112     C                   clear                   w02div
139100931119     C                   SETON                                        36
139200931119     C                   READC     FRB8SF2                                30
139300931119     C*
139400931119    3C     *IN30         DOWEQ     *OFF
139500931122     C*
139600931122    4C     VI2SCE        IFEQ      '1'
139700931122    5C     W02DDC        IFEQ      0
139800931122     C                   MOVEL     VH2DDC        W02DDC
139900020129     C                   z-add     vi2ndc        w02ndc
140000011112     C                   movel     vh2div        w02div
140100931122   X5C                   ELSE
140200931122    6C     VH2DDC        IFNE      W02DDC
140300931119     C                   Z-ADD     NRR           REC2
140400931119     C                   SETON                                        409030
140500931122    6C                   ENDIF
140600931122    5C                   ENDIF
140700931122    4C                   ENDIF
140800931119     C*
140900931119    4C     VH2I12        IFEQ      '1'
141000931119     C                   SETON                                        12
141100931119   X4C                   ELSE
141200931119     C                   SETOFF                                       12
141300931119    4C                   ENDIF
141400931119     C*
141500931119     C                   UPDATE    FRB8SF2
141600931119     C*
141700931119     C  N30              READC     FRB8SF2                                30
141800931119    3C                   ENDDO
141900931119     C*
142000931119     C                   SETOFF                                       36
142100931119     C   90              GOTO      ENDSEL
142200931119    2C                   ENDIF
142300931119    1C                   ENDIF
142400931018     C* PULIZIA SUBFILE
142500931018     C                   SETON                                        35
142600931018     C                   WRITE     FRB8CT4
142700931018     C                   SETOFF                                       35
142800931119     C*
142900931119     C                   Z-ADD     1             NRR
143000950131     C* Indicatore protezione codice tariffa e Pik/Etich. S/N per
143100950131     C* distinte gia' valorizzate
143200950131     C     VI1FVL        COMP      'V'                                    24
143300931018     C*
143400931028    1C     NRR           DOWLE     W01NRR
143500931103     C*
143600931111     C   KANRR           CHAIN     FRB8SF2                            31
143700931111     C  NKA              READC     FRB8SF2                                31
143800931018     C*
143900931018     C* SE ENTER --> SOLO QUELLI CON L'"1"
144000931111    2C  N31*INKA         IFEQ      *OFF
144100931103     C     VI2SCE        ANDEQ     '1'
144200931018     C* SE CMD1 --> TUTTI
144300931018     C     *INKA         OREQ      *ON
144400931018     C*
144500931018     C* C O D I C E   P A D R O N C I N O
144600931028    3C     VI2PDR        IFNE      W02PDR
144700931018     C*
144800931018     C                   MOVEL     VI2PDR        W02PDR                          SALVATAGGIO
144900931019     C                   MOVEL     VI2PDR        VI4PDR                          VIDEO COD
145000931019     C                   MOVEL     VI2DPD        VI4DPD                          VIDEO DES
145100931018     C                   MOVEL     VI2DPD        VH4DPD                          HIDDEN X SLF6
145200931018     C*
145300931108     C* 11 OFF - EMETTO PADRONCINO E TIPO PRESTAZ A VIDEO
145400931019     C                   SETOFF                                       11
145500931028    3C                   ENDIF
145600931018     C*
145700931108     C* T I P O   P R E S T A Z I O N E
145800931028    3C     *IN11         IFEQ      *OFF
145900931019     C     VI2TSR        ORNE      W17TSR
146000931019     C                   SETOFF                                       11
146100021211     C                   MOVEL     *blank        vi4TCS
146200931018     C                   MOVEL     VI2TSR        W17TSR           17             CONFRONTO
146300931018     C                   MOVEL     VI2TSR        VI4TSR                          VIDEO
146400950120     C                   MOVEL     VI2TSR        VH4TSH                         T.SERV.X PRINT
146500931019     C                   MOVEL     VI2TSR        VH4TSR                          HIDDEN
146600950119     C                   MOVEL     VI2TSR        VH4TSD                          HIDDEN
146700950119     C                   MOVEL     VI2DPD        VI4DPD
146800090508     C                   MOVEL     VI2DPD        VH4DPD
146900090508      *
147000931123     C* RICERCA CODICE TARIFFA PER PADRONCINO/TIPO PRESTAZ
147100011112     C                   clear                   vi4ctd
147200931021     C                   EXSR      RICTAR
147300040728     c                   move      tar_poste     vh4TPO
147400090507     C*
147500931018     C                   CLEAR                   VI4NDC
147600931018     C                   CLEAR                   VI4DDC
147700950116     C                   CLEAR                   VH4DDC
147800931019     C                   CLEAR                   VH4NRR
147900931027     C                   CLEAR                   VI4ITT
148000931027     C                   CLEAR                   VI4ITA
148100011106     C                   z-add     1             vh4tpr
148200931018     C                   ADD       1             NRR4
148300011109     c     vh4tpr        comp      2                                      26
148400931018     C                   WRITE     FRB8SF4
148500931019     C                   SETON                                        11
148600000703     C                   SETOFF                                       36
148700931019     C                   Z-ADD     NRR4          VH4NRR
148800931028    3C                   ENDIF
148900931018     C*
149000950117     C* PRESTAZIONE PADRONCINO M o P
149100950119     C     VH2FPP        IFEQ      'M'
149200950117     C                   MOVEL     COS1          VI4DPD
149300950119     C                   ELSE
149400950119     C     VH2FPP        IFEQ      'P'
149500950119     C                   MOVEL     COS2          VI4DPD
149600950119     C                   END
149700950119     C                   END
149800021211      *
149900950117     C                   MOVE      VH2FPP        VH4FPP
150000021211      *
150100950117     C* PAGAMENTO TARIFFA ETICH/PIKING
150200950117     C                   SELECT
150300021211      *
150400950117     C* Se Consegna memorizza flag piking da anagrafico padroncino
150500021211      *  e memorizza anche carico/scarico
150600950117     C     VH4TSR        WHENEQ    'C'
150700021211      * Carico
150800950119     C                   MOVEL     COS3          VI4TSR
150900021211     C                   MOVEL     cos5          vi4TCS
151000950117     C     VH4FPP        IFEQ      'M'
151100950127     C                   MOVEL     VH2PKM        VH4PEA
151200021211     c                   eval      vh4ACS = vh2CAM
151300950117     C                   ELSE
151400950127     C                   MOVEL     VH2PKP        VH4PEA
151500021211     c                   eval      vh4ACS = vh2CAP
151600950117     C                   END
151700021211      *
151800950117     C* Se Ritiro memorizza flag etichettatura da anagrafico
151900950117     C     VH4TSR        WHENEQ    'R'
152000021211      * Scarico
152100950119     C                   MOVEL     COS4          VI4TSR
152200021211     C                   MOVEL     cos6          vi4TCS
152300950117     C     VH4FPP        IFEQ      'M'
152400950127     C                   MOVEL     VH2ETM        VH4PEA
152500021211     c                   eval      vh4ACS = vh2SCM
152600950117     C                   ELSE
152700950127     C                   MOVEL     VH2ETP        VH4PEA
152800021211     c                   eval      vh4ACS = vh2SCP
152900950117     C                   END
153000950117     C                   ENDSL
153100950127     C*
153200950127     C                   MOVEL     VH2PEP        VI4PEP
153300021211     c                   eval      vi4FCS = vh2FCS
153400021211      *
153500950127     C     VI4PEP        IFEQ      *BLANKS
153600950127     C                   MOVEL     VH4PEA        VI4PEP
153700950127     C                   END
153800021211      *
153900021211     C     vi4FCS        IFEQ      *BLANKS
154000021211     C                   eval      vi4FCS = vh4ACS
154100021211     C                   END
154200021211      *
154300931018     C* DISTINTA: NUMERO E DATA
154400931018     C                   MOVEL     VI2NDC        VI4NDC
154500931018     C                   MOVEL     VI2DDC        VI4DDC
154600950116     C                   MOVEL     VH2DDC        VH4DDC
154700011105     C                   MOVEL     VH2ITT        VI4ITT
154800931027     C                   MOVEL     VH2ITA        VI4ITA
154900021217     C                   z-add     vh2TBS        vh4TBS
155000030127      * espone se ci sono dei ritiri annullati
155100030127     C                   z-add     vh2TBS        vi4ran
155200011105     C                   CLEAR                   vi4ctd
155300011105     C                   MOVEL     vh2div        vi4ctd
155400011106     C                   z-add     2             vh4tpr
155500931018     C                   ADD       1             NRR4
155600011109     c     vh4tpr        comp      2                                      26
155700931018     C                   WRITE     FRB8SF4
155800931103     C*
155900931103     C* PULISCO IL CAMPO DI SCELTA
156000931103     C                   MOVEL     ' '           VI2SCE
156100931103     C     VH2I12        IFEQ      '0'
156200931103     C                   SETOFF                                       12
156300931103     C                   ELSE
156400931103     C                   SETON                                        12
156500931103     C                   ENDIF
156600931103     C*
156700931103     C                   UPDATE    FRB8SF2
156800931028    2C                   ENDIF
156900931103     C*
157000931103     C* FINE LETTURA SE NO CMD1
157100931111     C  NKA*IN31         IFEQ      *ON
157200931103     C                   Z-ADD     W01NRR        NRR
157300931103     C                   ENDIF
157400931018     C*
157500931018     C                   ADD       1             NRR
157600931028    1C                   ENDDO
157700931018     C*
157800931018     C* NON EFFETTUATA NESSUNA SELEZIONE--> RIEMETTO SFL 2
157900931028    1C     NRR4          IFEQ      0
158000931018     C                   SETON                                        90
158100931018     C                   ELSE
158200931018     C                   Z-ADD     NRR4          W04NRR
158300931028    1C                   ENDIF
158400931018     C*
158500931119     C     ENDSEL        ENDSR
158600020212     C*---------------------------------------------------------------*
158700931108     C* RICERCO TARIFFA PREFERENZIALE PER TIPO PRESTAZ/PADRONCINO ----*
158800020212     C*---------------------------------------------------------------*
158900931018     C     RICTAR        BEGSR
159000000629     C*
159100000629     C                   SETOFF                                       5051
159200000703     C                   SETON                                        36
159300000622     C* RICHIAMO LA ROUTINE CHE VERIFICA SE LE BOLLE CHE SONO IN DISTINTA SONO TUTTE BARTOLINI
159400000622     C* OPPURE TUTTE POSTE OPPURE MISTE PER FARE LA PROPOSTA DELLA TARIFFA GIUSTA
159500000622     C                   EXSR      RICBOL
159600931104     C* SE LA SOMMA PESI NON C'E' VIENE ASSUNTO SPEDIZIONE
159700931104     C                   MOVEL     'S'           VH4TSM
159800931028     C                   CLEAR                   WTARV
159900000622     C* CAMPI COMODO TARIFFE  POSTE
160000000622     C                   CLEAR                   PTARV
160100040825     C                   clear                   Ptarv_rap
160200040825     C                   clear                   Ptarv_rct
160300000622     C*
160400011112     C     KFGT          CHAIN     FIFGT000                           30
160500931018     C*
160600931021    1C     *IN30         DOWEQ     *OFF
160700931018     C*
160800011112     C* ESCLUDO LE TARIFFE ANNULLATE E QUELLE ANCORA DA DECORRERE
160900011112     C*  e quelle diverse dalla divisa del documento
161000020128      ****   e quelle senza data di stampa (quindi non convalidate)
161100020315      ****   ma solo se non in simulazione.
161200020315      ****   se in simulazione non deve controllare la data di stampa.
161300091112    2C     FGTATB        IFEQ      ' '
161400091112     C     w02div        andeq     fgtdiv
161500091112     C*
161600091112     C* Se tariffa del periodo
161700091112     C     Ktgt          CHAIN     FItgt01l
161800091112 2/3 c                   if        %Found(FItgt01l) and
161900091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
162000091112     **
162100091112    3C     parsml        IFeq      *blank
162200091112     C     tgtDTS        aNDgt     0
162300091112     C     parsml        OReq      'S'
162400011112      *
162500931108     C* ESCLUDO TARIFFA A GIORNATA E A PRESTAZ (COD.TAR = 999)
162600931105     C                   Z-ADD     1             C
162700931105     C     FGTTSR        LOOKUP    TSR(C)                                 32
162800091112    4C     FGTCTR        IFNE      CTS(C)
162900931018     C* TARIFFA VALIDA
163000931021     C*
163100931021     C* EMETTO LA PREFERENZIALE SE C'E'
163200931021    5C     FGTFTP        IFEQ      'P'
163300931021     C                   MOVEL     FGTCTR        WTARV             3
163400931021     C                   SETON                                        30
163500931021     C*
163600931021   X5C                   ELSE
163700931021     C* SE NON E' PREFERENZIALE TENGO MEMORIZZATA LA PRIMA
163800931022    6C     WTARV         IFEQ      *BLANKS
163900000622     C     FGTFTP        ANDNE     'O'
164000091112     C     WTARV         orne      *blank
164100040628     C     fgtFTP        ANDNE     'O'
164200091112     C     fgtCTR        andeq     sav_FTDCTB
164300091112     C     sav_FlgCTB    andeq     'S'
164400931022     C                   MOVEL     FGTCTR        WTARV
164500931022    6C                   ENDIF
164600091112      *
164700000622    6C     PTARV         IFEQ      *BLANKS
164800000622     C     FGTFTP        ANDEQ     'O'
164900091112     C     ptarv         Orne      *BLANKS
165000091112     C     fgtCTR        andeq     sav_FTDCTP
165100091112     C     sav_FlgCTP    andeq     'S'
165200040628     C     FGTFTP        ANDEQ     'O'
165300000622     C                   MOVEL     FGTCTR        PTARV             3
165400040825     C                   z-add     fgtrap        Ptarv_rap
165500040825     C                   z-add     fgtrct        Ptarv_rct
165600000622    6C                   ENDIF
165700091112      *
165800931022    5C                   ENDIF
165900091112     C*
166000091112    4C                   ENDIF
166100091112     C*
166200091112    3C                   ENDIF
166300091112 2/3 C                   ENDIF
166400091112    2C                   ENDIF
166500931018     C*
166600011112     C  N30KFGT          READE     FIFGT000                               30
166700931021    1C                   ENDDO
166800931021     C*
166900000714     C* SE CI SONO BOLLE BARTOLINI MUOVO LA TARIFFA BARTOLINI
167000000622    0C     VH4BAR        IFEQ      'S'
167100931028    1C     WTARV         IFNE      *BLANKS
167200011030     C                   MOVEL     WTARV         VI4CTD                          VALIDA
167300000703     C* SALVO LA TARIFFA BARTOLINI PER IL DETTAGLIO SPEDIZIONE
167400011030     C                   MOVEL     VI4CTD        VH4CTB
167500931028    1C                   ENDIF
167600000622    0C                   ENDIF
167700000622     C*
167800000714     C* SE CI SONO BOLLE POSTE MUOVO LA TARIFFA POSTA
167900000622    0C     VH4POS        IFEQ      'S'
168000040825      *
168100040825      *  Deve controllare se però c'è una tariffa di riferimento poste che non esiste +
168200091112      **** NON DEVE ESSERE PIU' utilizzata poichè comunque manca il progressivo in chiave
168300040825     C                   if        Ptarv_rap <> 0 and Ptarv_rct <> 0
168400040825     c                   exsr      Ric_Tar_RIF
168500040825     c                   end
168600040825      *
168700000622    1C     PTARV         IFNE      *BLANKS
168800011030     C                   MOVEL     PTARV         VI4CTD                          VALIDA
168900000703     C* SALVO LA TARIFFA POSTE     PER IL DETTAGLIO SPEDIZIONE
169000011030     C                   MOVEL     VI4CTD        VH4CTP
169100040726     c                   else
169200040726     **
169300040726     ** se non ci sono tariffe poste, dalla data specificata in FINOADATA
169400040726     ** deve prendere come tariffa poste la tariffa bartolini
169500040726     c                   if        dataoggi >= finoadata
169600040726     C                   MOVEL     WTARV         VI4CTD                          VALIDA
169700040726     C                   MOVEL     VI4CTD        VH4CTB
169800040726     c                   end
169900040726     **
170000000622    1C                   ENDIF
170100000622    0C                   ENDIF
170200931021     C*
170300931021     C* SE NON NE HO TRAVATA NESSUNA --> EMETTO ???
170400011030     C     VI4CTD        IFEQ      *BLANKS
170500011030     C                   MOVEL     '???'         VI4CTD
170600000629     C                   SETOFF                                       5150
170700000626     C                   ELSE
170800000626     C* SE HO UNA TARIFFA BARTOLINI  PROPONGO SEMPRE QUELLA
170900000626     C     VH4CTB        IFNE      *BLANKS
171000011030     C                   MOVEL     VH4CTB        VI4CTD
171100000626     C                   ENDIF
171200000626     C*
171300931021     C                   ENDIF
171400931021     C*
171500040727     ** Si memorizza se ha trovato una tariffa poste per poter
171600040727     **  gestire variazioni tariffe impostate a video su sfl su tariffe Bartolini
171700040728     c                   if        vh4TSR='C'
171800040727     c                   clear                   tar_poste         1
171900040727     c                   if        vh4CTP <> *blank
172000040727     c                   move      'S'           tar_poste         1
172100040727     c                   end
172200040728     c                   endIF
172300040727     **
172400931119     C                   ENDSR
172500040825     C*================================================================
172600040825     C* cerca eventuale tariffa di riferimento x Poste            ----*
172700040825     C*================================================================
172800040825     C     Ric_Tar_RIF   BEGSR
172900040825     **
173000040825     C                   Z-ADD     Ptarv_rap     rifPDR
173100040825     C                   Z-ADD     Ptarv_rct     rifCTR
173200040825     C*
173300040825     C                   move      'N'           tar_valida
173400040825     C*
173500040825     C     KFGT_rif      setll     fifgt01l
173600040825     C     KFGT_rif      reade     fifgt01l
173700040825      *
173800040825    1C                   DOW       not %eof(fifgt01l)
173900040825     C*
174000040825     C* TARIFFA VALIDA di riferimento POSTE
174100040825     C*  Non deve essere annullata e deve avere la divisa richiesta
174200040825      **   Data Documento deve essere compresa fra le date di attivazione e
174300040825      ***   scadenza e deve essere stata stampata (quindi convalidata)
174400040825      ****   >altrimenti è un Manca Tariffa<
174500040825    2C     FGTATB        ifEQ      ' '
174600091112     c     w02div        andeq     fgtdiv
174700091112     C*
174800091112     C* Se tariffa del periodo
174900091112     C     Ktgt          CHAIN     FItgt01l
175000091112 2/3 c                   if        %Found(FItgt01l) and
175100091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
175200091112      *
175300091112    3C     parsml        IFeq      *blank
175400091112     C     tgtDTS        andgt     0
175500091112     C     fgtatb        oreq      ' '
175600091112     c     parsml        andeq     'S'
175700040825     C*  se trovata ok deve uscire dal giro
175800040825     C                   move      'S'           tar_valida
175900040825     C                   leave
176000091112    3C                   end
176100091112      *
176200091112 2/3 C                   end
176300040825    2C                   end
176400040825     C*
176500040825     C     KFGT_rif      reade     fifgt01l
176600040825    1C                   ENDDO
176700040825     C*
176800040825    2C     Tar_valida    ifeq      'N'
176900040825     C                   clear                   PTARV
177000040825    2c                   end
177100040825     **
177200040825     C                   ENDSR
177300020212     C*---------------------------------------------------------------*
177400000622     C* RICERCA TIPOLOGIA BOLLE PER RICERCA TARIFFA ------------------*
177500020212     C*---------------------------------------------------------------*
177600000622     C     RICBOL        BEGSR
177700000622     C*
177800000622     C                   MOVEL     ' '           VH4POS
177900000622     C                   MOVEL     ' '           VH4BAR
178000000703     C                   MOVEL     *BLANKS       VH4CTP
178100000703     C                   MOVEL     *BLANKS       VH4CTB
178200021217     c                   z-add     0             conta             5 0
178300040628     c                   z-add     0             sav_FTDctb        3 0
178400040628     c                   z-add     0             sav_FTDctp        3 0
178500040727     C* Oltre  a salvare la tariffa vera e propria deve anche impostare un flag
178600040727     C* parallelamente poichè potrebbero esistere tariffe '000' e quindi potrebbe
178700040727     C* non essere comprensibile solo il dato sav_FTDct*
178800040727     c                   move      *blank        sav_FLGctb        1
178900040727     c                   move      *blank        sav_FLGctp        1
179000000622     C*
179100000622     C* LETTURA SEQUENZIALE DEL DETTAGLIO DISTINTA PADRONCINI PER RECUPERARE LA LNP
179200000622     C* UTILE PER RICONOSCERE SE BOLLA POSTA
179300011105     C     KFTTT         SETLL     FIFTT02L
179400011105     C     KFTTT         READE     FIFTT02L                               39
179500000714     C**
179600000714     C     *IN39         DOWEQ     *OFF
179700000714     C**
179800011105     C     KFTDT         SETLL     fiftd001
179900000622     C                   DO        *HIVAL
180000011105     C     KFTDT         READE     fiftd001                               32
180100000622     C*
180200000622     C   32              LEAVE
180300021217     c                   add       1             conta
180400000714     C**
180500000622     C* CONTROLLO IN AZORG SE PO POSTE
180600000622     C                   EXSR      CTRORG
180700000622     C*  P.O. POSTE
180800020729     C     §OgNTW        IFEQ      'PPT'
180900000622     C                   MOVEL     'S'           VH4POS
181000040729     c                   if        ftdctp <> *blank and sav_FLGctp = ' '
181100040628     c                   move      ftdctp        sav_FTDctp
181200040727     c                   move      'S'           sav_FLGctp
181300040628     c                   end
181400000622     C                   ELSE
181500000622     C                   MOVEL     'S'           VH4BAR
181600040729     c                   if        ftdctp <> *blank and sav_FLGctb = ' '
181700040628     c                   move      ftdctp        sav_FTDctb
181800040727     c                   move      'S'           sav_FLGctb
181900040628     c                   end
182000000622     C                   ENDIF
182100000622     C*
182200000622     C                   ENDDO
182300000714     C**
182400011105     C     KFTTT         READE     FIFTT02L                               39
182500000714     C                   ENDDO
182600040727     **
182700040727     ** Memorizza se sono solo spedizioni poste la tariffa sul sav Bartolini
182800040727     C                   if        VH4BAR = ' ' and VH4POS = 'S'
182900040727     c                   move      sav_FTDctp    sav_FTDctb
183000040727     c                   end
183100040727     **
183200021217     C* Attenzione: Occorre gestire una eccezione.
183300021217     C*  Se ci troviamo in presenza di una testata Ritiri senza nessuna riga
183400021217     C*  di dettaglio e con il campo Nr.RITIRI ANNULLATI >0 quindi stiamo
183500021217     C*  prendendo in considerazione solo dei Ritiri Annullati devo impostare
183600021217     C*  il VH4BAR come se avessi trovato solo delle bolle Bartolini.
183700021217     C* Questo accade quando sono stati preventivati enne ritiri e tutti gli
183800021217     C* enne ritiri sono stati annullati. In tal modo non si ha più la capacità
183900021217     C* di sapere se erano Bartolini o Poste quindi si imposta la tariffa
184000021217     C* su Bartolini. (Per Default)
184100030127     c                   if        vh4TSR='R' and conta=0 and vh2TBS > 0
184200021217     C                   MOVEL     'S'           VH4BAR
184300021217     C                   EndIF
184400021217     C*
184500000622     C                   ENDSR
184600020521     C*---------------------------------------------------------------*
184700020521     C* CONTROLLO IN AZORG SE PO POSTE
184800020212     C*---------------------------------------------------------------*
184900000622     C     CTRORG        BEGSR
185000020212     C*---------------------------------------------------------------*
185100000622     C* AGGANCIO AZORG
185200000622     C     FTDLNP        CHAIN     AZORG01L                           34
185300000622     C   34              MOVEL     *BLANKS       OG143
185400000622     C  N34              MOVEL     ORGDE3        OG143
185500000622     C*
185600000622     C                   ENDSR
185700020212     C*---------------------------------------------------------------*
185800931018     C* CONTROLLO DATI SUBFILE 4 -------------------------------------*
185900020212     C*---------------------------------------------------------------*
186000931018     C     CONTR4        BEGSR
186100931018     C                   Z-ADD     1             NRR4
186200931018     C                   SETOFF                                       90
186300911014     C*
186400931019     C                   READC     FRB8SF4                                32
186500930707     C*
186600931019    1C     *IN32         DOWEQ     *OFF
186700931019     C*
186800950117     C* RECORD DEL TIPO SERVIZIO
186900931019    2C     VH4NRR        IFEQ      0
187000931019     C*
187100931018     C* C O D I C E   T A R I F F A
187200011030    3C     VI4CTD        IFNE      '???'
187300931019     C* RICERCA
187400011112     C     '?'           SCAN      VI4CTD                                 30
187500011106     C*
187600011106     C* Se si tratta della 1° riga (significa Codice Tariffa)
187700011106     c     vh4tpr        ifeq      1
187800931018     C*
187900011112I   4C     *IN30         IFEQ      *ON
188000931028     C                   CLEAR                   PARAM3
188100931028     C                   MOVEL     VI4PDR        PA3PDR
188200931028     C                   MOVEL     PARSML        PA3SML
188300931028     C                   MOVEL     VH4TSR        PA3TSR
188400100513     C                   MOVEL     KPJBU         kpjbus
188500100513     C                   clear                   kpjbu
188600931028     C                   MOVEL     PARAM3        KPJBU
188700011112     C                   CALL      'FICN86R'
188800931028     C                   PARM                    KPJBA
188900931108     C                   MOVEL     KPJBU         PARAM3
189000100513     C                   MOVEL     KPJBUs        kpjbu
189100931028     C* SELEZIONE EFFETTUATA
189200931028     C     PA3SEL        IFEQ      ' '
189300011030     C                   MOVEL     PA3CTR        VI4CTD
189400931028     C                   ELSE
189500011030     C                   MOVEL     '???'         VI4CTD
189600931028     C                   ENDIF
189700931028     C*
189800931105     C                   SETON                                        36
189900931018     C                   WRITE     FRB8D05
190000000622    4C                   ENDIF
190100931018     C* CONTROLLO
190200011030     C                   TESTN                   VI4CTD               30
190300931105    5C     *IN30         IFEQ      *OFF
190400931105     C                   SETON                                        90
190500931105   X5C                   ELSE
190600931105     C*
190700011030     C                   MOVEL     VI4CTD        W01CTR
190800931105     C                   EXSR      CTRTAR
190900931105    5C                   ENDIF
191000931019     C   90              SETON                                        4036
191100000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
191200000622     C* POSTE O BARTOLINI
191300000622     C     *IN90         IFEQ      *OFF
191400000630     C* IN CASO DI DISTINTA GIA' VALORIZZATA NON SI FA
191500000630     C     *IN24         ANDEQ     *OFF
191600000622     C*
191700000622     C     VH4POS        IFEQ      'S'
191800000622     C     VH4BAR        ANDNE     'S'
191900000622     C* E LA TARIFFA È BARTOLINI ERRORE
192000000622     C     FGTFTP        ANDNE     'O'
192100040726     c                   if        dataoggi < finoadata
192200000622     C                   SETON                                        905236
192300040726     c                   end
192400000622     C                   ENDIF
192500000622     C*
192600000622     C     VH4POS        IFNE      'S'
192700000622     C     VH4BAR        ANDEQ     'S'
192800040622     C*
192900000622     C* E LA TARIFFA È BARTOLINI ERRORE
193000000622     C     FGTFTP        ANDEQ     'O'
193100000622     C                   SETON                                        905336
193200000622     C                   ENDIF
193300040622     C*
193400000703     C* SE CI SONO SPEDIZIONI MISTE MA MANCA ALMENO UNA DELLE DUE TARIFFE DO ERRORE
193500000703     C* LA PRIMA VOLTA CHE CONTROLLO
193600000703     C     VH4POS        IFEQ      'S'
193700000703     C     VH4BAR        ANDEQ     'S'
193800040622     C*
193900000703     C* MANCA TARIFFA POSTA
194000000703     C     VH4CTP        IFEQ      *BLANK
194100040727     ** oppure se in un secondo tempo non essendoci tariffe poste è stata impostata
194200040727     ** un'altra tariffa bartolini rispetto a quella standard proposta
194300040728     c     vh4TPO        oreq      *blank
194400040727     C     vh4CTP        andne     vi4CTD
194500040622     C*
194600040622     C*  fino al giorno 2004/08/10 il controllo è attivato
194700040726     C*   comunque imposta in Hidden il valore espresso a video
194800040726     C*    ma non da + l'errore
194900040622     c                   if        dataoggi < finoadata
195000000703     C                   SETON                                        905136
195100040726     c                   end
195200011030     C                   MOVEL     VI4CTD        VH4CTP
195300040622     C*
195400000703     C                   ENDIF
195500000703     C*
195600000703     C     VH4CTB        IFEQ      *BLANK
195700000703     C                   SETON                                        905036
195800011030     C                   MOVEL     VI4CTD        VH4CTB
195900000703     C                   ENDIF
196000000703     C*
196100000703     C                   ENDIF
196200000622     C*
196300000622     C* SE NON E' ACCESO IL 90 VALORIZZO I CAMPI TARIFFA HIDDEN SE CAMBIATI
196400000622     C     *IN90         IFEQ      *OFF
196500000622     C*+
196600000622     C     FGTFTP        IFEQ      'O'
196700011030     C                   MOVEL     VI4CTD        VH4CTP
196800000622     C                   ELSE
196900011030     C                   MOVEL     VI4CTD        VH4CTB
197000000622     C                   ENDIF
197100000622     C*
197200000622     C                   ENDIF
197300000622     C                   ENDIF
197400011106     C*
197500011106    4C                   end
197600011106     C*
197700931021    3C                   ENDIF
197800931019     C*
197900931108     C* NON E' POSSIBILE ANDARE IN MANUTENZIONE DI UN TIPO PRESTAZIONE
198000931019     C  N90VI4SCE        IFEQ      'M'
198100931019     C                   SETON                                        419036
198200931019     C                   ENDIF
198300931019     C*
198400931108     C* ANNULLAMENTO VALORIZZAZIONE DI TUTTO IL TIPO PRESTAZIONE
198500931019     C*  LO ESEGUO DOPO
198600931019     C  N90VI4SCE        IFEQ      'A'
198700931112     C   20              SETON                                        459036
198800931112     C  N20              SETON                                        36
198900931019     C                   ENDIF
199000931019     C*
199100931019   X2C                   ELSE
199200011107     C*
199300931019     C* RECORD DI DISTINTA/RITIRO
199400011107     C*
199500931019     C* MANUTENZIONE SPEDIZIONI DELLA DISTINTA
199600931022    3C     VI4SCE        IFEQ      'M'
199700931022    4C     VI4ITT        IFGT      0
199800931022     C     VI4ITA        ANDGT     0
199900931022     C                   SETON                                        439036
200000931022   X4C                   ELSE
200100931019     C                   EXSR      MANSPE
200200931019     C                   SETON                                        90
200300931028     C                   WRITE     FRB8D05
200400931105     C*
200500931105     C* RIAGGANCIO IL RECORD DI SUBFILE
200600931105     C     NRR4          CHAIN     FRB8SF4                            32
200700931022    4C                   ENDIF
200800931022    3C                   ENDIF
200900931019     C*
201000931019     C* ANNULLO VALORIZZAZIONE PRECEDENTE DELLA DISTINTA
201100931019     C     VI4SCE        IFEQ      'A'
201200931105     C     VI1FVL        ANDEQ     'V'
201300931112     C   20              SETON                                        459036
201400931112     C  N20              EXSR      ANNSPE
201500931112     C  N20              SETON                                        21
201600931105     C                   ENDIF
201700931019     C*
201800950117     C* CONTROLLO PIKING/ETICHETTATURA S/N
201900950117     C* Se = Blank imposto quello specificato su anagrafico padroncini
202000950117     C     VI4PEP        IFEQ      *BLANKS
202100950127     C                   MOVEL     VH4PEA        VI4PEP
202200950117     C                   END
202300021211      *
202400021211     C* Se = Blank imposto quello specificato su anagrafico padroncini
202500021211      *   per carico/scarico
202600021211     C     vi4FCS        IFEQ      *BLANKS
202700021211     C                   eval      vi4FCS = vh4ACS
202800021211     C                   END
202900950117     C*
203000931019    2C                   ENDIF
203100921228     C*
203200931019    2C     VH4NRR        IFEQ      0
203300931018     C                   SETOFF                                       11
203400931018     C                   ELSE
203500931018     C                   SETON                                        11
203600931019    2C                   ENDIF
203700911017     C*
203800931019     C  N36              MOVEL     ' '           VI4SCE
203900931018     C                   Z-ADD     NRR4          REC4
204000011109     c     vh4tpr        comp      2                                      26
204100931019     C                   UPDATE    FRB8SF4
204200931018     C                   SETOFF                                       36
204300930707     C*
204400931019     C   90              SETON                                        32
204500931019     C  N90              READC     FRB8SF4                                32
204600931018    1C                   ENDDO
204700931019     C**
204800931019     C* SE TUTTO OK CONTROLLO SE HO ANNULLAMENTI DI VALORIZZAZIONE
204900931108     C*  DI TIPO PRESTAZ DA FARE
205000931108    1C     *IN90         IFEQ      *OFF
205100931019     C                   READC     FRB8SF4                                32
205200931019     C*
205300931108    2C     *IN32         DOWEQ     *OFF
205400931105     C*
205500931108    3C     VI4SCE        IFEQ      'A'
205600931108     C* SE NON SONO GIA' VALORIZZATE PULISCO SOLO IL CAMPO DI SCELTA SF
205700931108    4C     VI1FVL        IFEQ      'V'
205800931019     C                   EXSR      ANNTSR
205900931105     C*
206000931105     C* AL TERMINE RIEMETTO IL SFL2 (21 ON)
206100931105     C                   SETON                                        21
206200931108   X4C                   ELSE
206300931108     C                   MOVEL     ' '           VI4SCE
206400011109     c     vh4tpr        comp      2                                      26
206500931108     C                   UPDATE    FRB8SF4
206600931108    4C                   ENDIF
206700931105     C*
206800931108    3C                   ENDIF
206900931019     C*
207000931019     C                   READC     FRB8SF4                                32
207100931108    2C                   ENDDO
207200931108    1C                   ENDIF
207300911017     C*
207400900614     C                   ENDSR
207500020212     C*---------------------------------------------------------------*
207600931019     C* MANUTENZIONE SPEDIZIONI DISTINTA SELEZIONATA -----------------*
207700020212     C*---------------------------------------------------------------*
207800931019     C     MANSPE        BEGSR
207900931110     C                   SETOFF                                       151637
208000931108     C                   CLEAR                   FRB8SF6
208100931028     C     VI4ITT        IFGT      0
208200931028     C                   SETON                                        15
208300931028     C                   ENDIF
208400931028     C     VI4ITA        IFGT      0
208500931028     C                   SETON                                        16
208600931028     C                   ENDIF
208700931028     C*
208800931019     C* CARICAMENTO SPEDIZIONI
208900931019     C                   EXSR      CARSPE
209000130305     C*
209100931110     C* SE NON CE NE SONO NON EMETTO IL SUBFILE
209200030127     c                   move      'N'           Dett6_a0          1
209300931110     C     NRR6          IFEQ      0
209400931110     C                   SETON                                        37
209500030127     c                   move      'S'           Dett6_a0
209600030127     c                   z-add     vi4ran        d8ran
209700931110     C                   ENDIF
209800931019     C*
209900931103     C                   Z-ADD     1             REC6
210000931019     C                   WRITE     FRB8D07
210100030127     C   37              WRITE     FRB8D08
210200931019     C     FORCT6        TAG
210300020409     c                   if        savnr6 > 0
210400020409     c                   z-add     savnr6        rec6
210500020409     c                   end
210600931019     C                   EXFMT     FRB8CT6
210700020409     c                   z-add     nrrult        savnr6            5 0
210800931019     C* CMD3 - FINE LAVORO
210900931019     C     *INKC         IFEQ      *ON
211000931111     C   22              SETON                                        4490
211100931111     C   22              GOTO      FORCT6
211200931019     C                   SETON                                        LR
211300931019     C                   GOTO      ENDMAN
211400931019     C                   ENDIF
211500931019     C* CMD12 - RITORNO
211600931019     C   KL              GOTO      ENDMAN
211700030127      *
211800030127      * CONTROLLI solo se ci sono delle righe e non sono solo
211900030127      *   dei ritiri annullati
212000030127     c                   if        Dett6_a0 = 'N'
212100931019     C                   EXSR      CONTR6
212200030127     c                   end
212300931202     C   90              GOTO      FORCT6
212400020418     C*
212500020418     c                   if        VI1FVL<>'V'
212600931019     C* CMD6 - AGGIORNAMENTO
212700931202     C   KE
212800030127     cOR KF              if        Dett6_a0 = 'N'
212900030127     C                   EXSR      AGGIO6
213000020408     C   ke
213100020408     Cor kf              commit
213200020418     c                   end
213300030127     c                   end
213400931202     C* CMD5 RIVISUALIZZA
213500931202     C   KE              EXSR      CARSPE
213600931202     C   KE              Z-ADD     1             REC6
213700931202     C* RIEMETTO VIDEATA PER ENTER O CMD5
213800931202     C  NKF              GOTO      FORCT6
213900931019     C*
214000931021     C     ENDMAN        ENDSR
214100020212     C*---------------------------------------------------------------*
214200950116     C* CARICO LE SPEDIZIONI DELLA DISTINTA A VIDEO ------------------*
214300020212     C*---------------------------------------------------------------*
214400931019     C     CARSPE        BEGSR
214500931021     C                   CLEAR                   NRR6
214600931021     C                   CLEAR                   W06NRR
214700931019     C*
214800931019     C                   SETON                                        35
214900931019     C                   WRITE     FRB8CT6
215000931019     C                   SETOFF                                       35
215100931019     C*
215200931019     C* DECODIFICA C A M P I   T E S T A T A
215300931019     C*
215400931019     C                   MOVEL     '1N'          COD
215500931019     C                   MOVEL     *BLANKS       KEY
215600931103     C                   MOVEL     VH4TSM        KEY
215700931019     C     KTAB          CHAIN     TABEL                              30
215800931019     C  N30              MOVEA     TBLUNI        K17(3)
215900931019     C   30              MOVEA     *BLANKS       K17(3)
216000931103     C                   MOVEL     VH4TSM        K17(1)
216100931019     C                   MOVEL     '-'           K17(2)
216200931019     C                   MOVEA     K17           VI6TSM
216300931019     C* MITTENTE/DESTINATARIO
216400931022    1C     VH4TSR        IFEQ      'R'
216500931104     C                   SETON                                        19
216600020311     C                   MOVEL     'In'          VI6IA
216700020311     C                   MOVEL     CSPED         VI6MD
216800931108     C*
216900931108   X1C                   ELSE
217000931108     C*
217100931104     C                   SETOFF                                       19
217200931029     C                   MOVEL     'In'          VI6IA
217300931202     C                   MOVEL     CSPED         VI6MD
217400931019    1C                   ENDIF
217500931021     C*
217600931021     C* REPERISCO CODICE TARIFFA DALLA RIGA IN CUI SI TROVA
217700121221     c                   move      'N'           h6targ            1
217800931103     C                   Z-ADD     NRR4          W05NRR
217900931021     C     VH4NRR        CHAIN     FRB8SF4                            30
218000931022     C   30              MOVEL     '000'         W01CTR
218100011106     c                   if        vh4tpr = 1
218200011030     C  N30VI4CTD        IFEQ      '???'
218300121221     c                   move      'S'           h6targ
218400931022     C                   MOVEL     000           W01CTR
218500931022     C                   ELSE
218600011030     C                   MOVEL     VI4CTD        W01CTR
218700931021     C                   ENDIF
218800011105     c                   endif
218900011105      *
219000000703     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
219100000703     C     VH4CTP        IFNE      *BLANKS
219200000703     C     VH4CTB        ANDNE     *BLANKS
219300000703     C                   MOVEL     VH4CTP        WCTP              3
219400000703     C                   MOVEL     VH4CTB        WCTB              3
219500000703     C                   ELSE
219600000703     C                   MOVEL     *BLANKS       WCTP
219700000703     C                   MOVEL     *BLANKS       WCTB
219800000703     C                   ENDIF
219900931103     C                   Z-ADD     W05NRR        NRR4
220000931028     C     NRR4          CHAIN     FRB8SF4                            30
220100931019     C*
220200931019     C* DECODIFICA  C A M P I   D E T T A G L I O
220300931019     C                   CLEAR                   K17
220400931203     C                   CLEAR                   WFTD
220500931108     C*
220600011105     C     KFTD          SETLL     fiftd001
220700011105     C     KFTD          READE     fiftd001                               32
220800931019     C*
220900931019    1C     *IN32         DOWEQ     *OFF
221000931203     C                   ADD       1             WFTD              5 0
221100000626     C                   SETOFF                                       132325
221200000626     C                   SETOFF                                       49
221300950118     C*
221400950119     C                   MOVE      *BLANKS       VI6FTC
221500950119     C                   MOVE      *BLANKS       VI6FT2
221600950119     C                   MOVE      *BLANKS       VI6TSP
221700030320     C                   MOVE      *BLANKS       Vh6cbo
221800950119     C                   MOVE      *BLANKS       VH6SIC
221900950119     C                   MOVE      *BLANKS       VH6GVA
222000950119     C                   MOVE      *ZEROS        VH6NCE
222100000705     C                   MOVE      *ZEROS        VH6FGC
222200000623     C*
222300000623     C* CONTROLLO SE BOLLA POSTA O BARTOLINI
222400000623     C                   EXSR      CTRORG
222500020729     C     §OgNTW        IFEQ      'PPT'
222600020729     c                   movel     'S'           VH6POS
222700020729     c                   else
222800020729     C                   MOVEL     *blank        VH6POS
222900020729     c                   end
223000000626     C* SE BOLLA POSTA PROTEGGO IL CAMPO NUMERO STOP
223100020729     C     §OgNTW        IFEQ      'PPT'
223200000626     C                   SETON                                        49
223300000626     C                   ELSE
223400000626     C                   SETOFF                                       49
223500000626     C                   ENDIF
223600950119     C*
223700011123     C                   z-add     FTDSTP        VI6STP                          STOP
223800020313     C* ----------
223900020313     C* servono questi 2 campi hidden per poter gestire la vista logica FIFTD04L
224000020313     C* in alternativa alla FIFTD02L
224100020313     C                   z-add     FTDksc        vh6ksc                          cliente
224200020313     C                   z-add     FTDnsp        vh6nsp                          spedizione
224300020326     C                   move      FTDtsr        vh6tsr
224400020326     C                   z-add     FTDfgs        vh6fgs
224500020326     c                   if        vh6fgs=0
224600020326     C                   movel     FTDpdr        vh6fgs
224700020326     c                   end
224800020313     C* ----------
224900950116     C                   MOVEL     FTDRSC        VI6RSB                          RAG SOCIALE
225000950113     C                   MOVEL     FTDCAP        VI6CAP
225100950118     C                   Z-ADD     FTDCAS        VH6CAS                          CONTRASSEGNO
225200950131     C                   MOVEL     FTDVCA        VH6VCA                          DIVISA
225300011106     c                   if        vh6vca=*blank
225400011106     c                   eval      vh6vca='ITL'
225500011106     c                   end
225600950118     C                   Z-ADD     FTDIFP        VH6IFP                          IMP.INCASS.
225700011109      ***>>>>>>>>>>>>>>
225800011109     c                   if        vh4tpr = 2
225900011109     c                   movel     vi4ctd        vi6div
226000011109     c                   end
226100950118     C                   Z-ADD     FTDSET        VH6SET
226200931202     C* RITIRI
226300931202    2C     VH4TSR        IFEQ      'R'
226400020311     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
226500020311    2C     ftdnsp        ifeq      0
226600020311    2C     ftdksc        andgt     0
226700020311     C                   z-add     ftdksc        vi6nsp                          COD CLIENTE
226800020311     C                   end
226900950118     C                   Z-ADD     FTDNCE        VH6NCE                          COLLI ETICH
227000931202     C                   ELSE
227100931202     C* CONSEGNE
227200091012     C     Bolla_FTD     CHAIN     FNARB01L                           30
227300090918     C                   Z-ADD     arbKSC        Vh6ksc                          Cliente
227400090921      *
227500960208     C                   MOVEL     ARBGVA        VH6GVA                          PARTICOLARITA'
227600960208     C                   MOVE      ' '           VH6GVA
227700950118     C                   MOVEL     ARBTSP        VI6TSP                          C/E/D
227800030320     C                   MOVEL     ARBcbo        Vh6cbo                          C/E/D
227900931202     C                   Z-ADD     FTDNSP        VI6NSP                          NUM  SPEDIZ
228000950113     C                   MOVE      ARBMGS        DATA                            DATA SPEDIZ
228100931019     C                   MOVEL     GG            VI6MGS
228200931019     C                   MOVE      MM            VI6MGS
228300931028     C* SPEDIZIONE CONSEGNATA
228400931105    3C     FTDDCM        IFGT      0
228500950113     C                   MOVE      FTDDCM        DATA
228600931019     C                   MOVEA     GG            K17(1)
228700931019     C                   MOVEL     '/'           K17(3)
228800931019     C                   MOVEA     MM            K17(4)
228900931019     C                   MOVEL     '/'           K17(6)
229000931019     C                   MOVEA     AA            K17(7)
229100931019     C                   MOVEA     K17           VI6CON
229200950117     C* FLAG RITORNO ALL'INCASSO
229300950117     C     FTDSIC        COMP      'S'                                    23
229400950117     C                   MOVEL     FTDSIC        VH6SIC
229500931029     C*
229600950113     C* CONSEGNA PARTICOLARE 1 (SOLO SE E' CONSEGNATA)
229700950113    3C     ARBTC1        IFNE      ' '
229800931029     C                   MOVEL     '1P'          COD
229900931029     C                   MOVEL     *BLANKS       KEY
230000950113     C                   MOVEL     ARBTC1        KEY
230100931029     C     KTAB          CHAIN     TABEL                              30
230200931029     C*
230300931029     C  N30              MOVEA     TBLUNI        K17(3)
230400931029     C   30              MOVEA     *BLANKS       K17(3)
230500950113     C                   MOVEL     ARBTC1        K17(1)
230600931029     C                   MOVEL     '-'           K17(2)
230700931029     C                   MOVEA     K17           VI6FTC
230800931029     C                   ENDIF
230900950113     C* CONSEGNA PARTICOLARE 2 (SOLO SE E' CONSEGNATA)
231000950113    3C     ARBTC2        IFNE      ' '
231100950113     C                   MOVEL     '1P'          COD
231200950113     C                   MOVEL     *BLANKS       KEY
231300950113     C                   MOVEL     ARBTC2        KEY
231400950113     C     KTAB          CHAIN     TABEL                              30
231500950113     C*
231600950113     C  N30              MOVEA     TBLUNI        K17(3)
231700950113     C   30              MOVEA     *BLANKS       K17(3)
231800950113     C                   MOVEL     ARBTC2        K17(1)
231900950113     C                   MOVEL     '-'           K17(2)
232000950113     C                   MOVEA     K17           VI6FT2
232100950113     C                   ENDIF
232200931019     C*
232300931019   X3C                   ELSE
232400931019     C*
232500931019     C* C'E' CODICE GIACENZA/MANCATA CONSEGNA
232600931019    4C     FTDCMC        IFNE      ' '
232700931019     C                   MOVEA     FTDCMC        K17(1)
232800931019     C                   MOVEL     '-'           K17(4)
232900931019     C                   MOVEL     '2A'          COD
233000931019     C                   MOVEL     *BLANKS       KEY
233100931019     C                   MOVEL     FTDCMC        KEY
233200931019     C     KTAB          CHAIN     TABEL                              30
233300931019     C  N30              MOVEA     TBLUNI        K17(5)
233400931021     C   30              MOVEA     *BLANKS       K17(5)
233500931019     C                   MOVEA     K17           VI6CON
233600000705     C*
233700000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
233800000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
233900000623     C     VH6POS        IFEQ      'S'
234000000623     C                   MOVEL     '2Z'          COD
234100000623     C                   MOVEL     *BLANKS       KEY
234200000623     C                   MOVEL     FTDCMC        KEY
234300000623     C     KTAB          CHAIN     TABEL                              30
234400000623     C  N30              MOVEL     TBLUNI        DS2Z
234500000623     C   30              MOVEL     *BLANKS       DS2Z
234600000626     C     §2ZTPP        IFEQ      ' '
234700000626     C* NON PAGO LA SPEDIZIONE
234800000630     C                   SETON                                        2513
234900000626     C*
235000000626     C                   ENDIF
235100000623     C                   MOVEL     §2ZTPP        VH6FGC
235200000623     C                   MOVEL     §2ZTPP        FTDFGC
235300000623     C                   ENDIF
235400931019     C*
235500931019   X4C                   ELSE
235600931019     C* C'E' MANCATA CONSEGNA
235700931019     C                   MOVEA     FTDTMC        K17(1)
235800931019     C                   MOVEL     '-'           K17(2)
235900931019     C                   MOVEL     '2A'          COD
236000931019     C                   MOVEL     *BLANKS       KEY
236100931019     C                   MOVEL     FTDTMC        KEY
236200931019     C     KTAB          CHAIN     TABEL                              30
236300931019     C  N30              MOVEA     TBLUNI        K17(3)
236400931021     C   30              MOVEA     *BLANKS       K17(3)
236500931019     C                   MOVEA     K17           VI6CON
236600931019     C*
236700931028     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
236800931019     C* PRIMA CERCO CON FILIALE DI APPARTENENZA
236900931021     C                   MOVEL     KEY           WALF6             6
237000931111     C                   EXSR      CTRCMC
237100000630    4C                   ENDIF
237200931019     C*
237300950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
237400931028     C*                                    (SE NON L'HO TROVATA)
237500931028     C     *IN30         IFEQ      *OFF
237600931019     C                   SETON                                        13
237700931019    4C                   ENDIF
237800931019    3C                   ENDIF
237900931028     C*
238000950119     C* CODICE BOLLA
238100030320     C                   MOVEL     ARBCBO        VI6CBO
238200950119     C                   MOVEL     '3A'          COD
238300950119     C                   MOVEL     *BLANKS       KEY
238400950119     C                   MOVEL     ARBCBO        KEY
238500950119     C     KTAB          CHAIN     TABEL                              30
238600950119     C  N30              MOVEL     TBLUNI        DS3A
238700950119     C  N30              MOVEL     §3ADES        VI6CBD
238800950119     C   30              MOVEL     *BLANKS       VI6CBD
238900950119     C*
239000931028     C* C O D I C E   B O L L A   D A   T A S S A R E
239100931028    3C     *IN13         IFEQ      *OFF
239200931111     C                   EXSR      CTRCBL
239300931028     C*
239400950117     C* 13 ON SPEDIZIONE NON TASSABILE --> PROTEGGO IMPORTO ACCESSORI
239500931028     C*                                    (SE L'HO TROVATO)
239600931111    4C     *IN31         IFEQ      *ON
239700931028     C                   SETON                                        13
239800931028    4C                   ENDIF
239900931028    3C                   ENDIF
240000931028    2C                   ENDIF
240100931202     C*
240200931202     C* AGGIORNO PERCHE' POTREI AVER IMPOSTATO IL FLAG DI RICONS.GIAC
240300011105     C                   UPDATE    fiftd001
240400931019     C*
240500931022     C     FTDCTP        IFNE      *BLANKS
240600931022     C                   MOVEL     FTDCTP        VI6CTR
240700931022     C                   ELSE
240800000622     C* VERIFICO SE ESISTONO SIA BOLLE POSTE CHE BOLLE BARTOLINI
240900000703     C     WCTB          IFNE      '   '
241000000703     C     WCTP          ANDNE     '   '
241100000622     C* SE E' UN PO POSTE
241200000622     C     VH6POS        IFEQ      'S'
241300000622     C* MUOVO TARIFFA POSTA
241400000703     C                   MOVEL     WCTP          VI6CTR
241500000622     C                   ELSE
241600000622     C* MUOVO TARIFFA BARTOLINI
241700000703     C                   MOVEL     WCTB          VI6CTR
241800000622     C                   ENDIF
241900000622     C                   ELSE
242000121221     C                   MOVEL     h6targ        Vh6targ
242100121221     C                   MOVEL     W01CTR        VI6CTR
242200000622     C                   ENDIF
242300000622     C*
242400931022     C                   ENDIF
242500011105      *
242600011105      *  pulito il campo di totale
242700011108     C                   clear                   vi6tot
242800011115     c                   move      '0'           vp6ita
242900011115     c                   move      '0'           vp6itt
243000011115      *
243100011115      * imposta il flag di protezione in memoria sulla riga
243200011115     c                   if        *in13 = *on or *in16 = *on
243300011115     c                   eval      vp6ita = '1'
243400011115     c                   end
243500011115      *
243600011115      * imposta il flag di protezione in memoria sulla riga
243700011115     c                   if        *in15 = *on or *in23 = *on or *in25 = *on
243800011115     c                   eval      vp6itt = '1'
243900011115     c                   end
244000011115      * pulisce i campi
244100011115     C                   z-add     0             vh6ita
244200011115     C                   z-add     0             vi6ita
244300011115     C                   z-add     0             vh6itt
244400011115     C                   z-add     0             vi6itt
244500011105      *
244600950117     C* ACCESSORI NON VISUALIZZABILI: NON CARICO NEMMENO L'IMPORTO
244700931105     C     *IN13         IFEQ      *ON
244800950119     C                   MOVEL     'B'           VH6FIA
244900931105     C                   ELSE
245000011109     C                   MOVEL     FTDITA        VI6ITA
245100011115     C  N16              add       ftdita        vi6tot
245200931022     C                   MOVEL     FTDITA        VH6ITA
245300931021     C                   MOVEL     FTDFIA        VH6FIA
245400931105     C                   ENDIF
245500950117     C* NON CARICO IMPORTO BASE SE SI TRATTA DI UN RITORNO ALL'INCASSO
245600950118     C     FTDSIC        IFNE      'S'
245700011109     C                   MOVEL     FTDITT        VI6ITT
245800011115     C  N15              add       ftditt        vi6tot
245900950117     C                   MOVEL     FTDITT        VH6ITT
246000950117     C                   MOVEL     FTDFIT        VH6FIT
246100950119     C                   ELSE
246200950119     C                   MOVEL     'B'           VH6FIT
246300950117     C                   ENDIF
246400931105     C                   MOVEL     FTDAAS        VH6AAS
246500931019     C                   ADD       1             NRR6
246600020313     C*
246700020418     c     vi1fvl        comp      'V'                                    57
246800931019     C                   WRITE     FRB8SF6
246900931019     C*
247000011105     C     KFTD          READE     fiftd001                               32
247100931019    1C                   ENDDO
247200931203     C*
247300931203     C                   MOVEL     W01CTR        W01CTP
247400931019     C*
247500931019     C                   ENDSR
247600020212     C*---------------------------------------------------------------*
247700931019     C* ANNULLAMENTO VALORIZZAZIONE DI UNA DISTINTA ------------------*
247800020212     C*---------------------------------------------------------------*
247900931019     C     ANNSPE        BEGSR
248000931108     C*
248100011105     C     KFTD          SETLL     fiftd001
248200011105     C     KFTD          READE     fiftd001                               32
248300931019     C*
248400931022    1C     *IN32         DOWEQ     *OFF
248500931022     C* 14 ON  - ANNULLO TUTTO
248600931022    2C     *IN14         IFEQ      *ON
248700931022     C                   CLEAR                   FTDFIT                          FLG TAR BAS
248800931022     C                   CLEAR                   FTDITT                          TAR BASE
248900931022     C                   CLEAR                   FTDFIA                          FLG TAR ACC
249000931022     C                   CLEAR                   FTDITA                          TAR ACCESS
249100931022     C*
249200931022   X2C                   ELSE
249300931022     C* 14 OFF - ANNULLO SOLO LE VALORIZZAZIONI AUTOMATICHE
249400931022     C*
249500931022    3C     FTDFIT        IFEQ      ' '
249600931022     C                   CLEAR                   FTDITT                          TAR BASE
249700931022    3C                   ENDIF
249800931022    3C     FTDFIA        IFEQ      ' '
249900931022     C                   CLEAR                   FTDITA                          TAR ACCESS
250000931022    3C                   ENDIF
250100931022    2C                   ENDIF
250200931022     C*
250300020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
250400020326     C* i primi tre caratteri
250500020326     c                   if        ftdfgs = 0
250600020326     c                   movel     ftdpdr        ftdfgs
250700020326     c                   end
250800020326     C*
250900931022     C                   CLEAR                   FTDCTP                          COD TAR
251000011105     C                   UPDATE    fiftd001
251100931019     C*
251200020326     C*          *-------------------------------------*
251300020408     c     parsml        ifeq      ' '
251400020326     C*  deve annullare/cancellare (se c'è) anche la voce di C/E
251500020326     c     Kfce          setll     FiFCE01L
251600020326     c     Kfce          reade     FiFCE01L
251700020326     c                   dow       not %Eof(FiFCE01L)
251800020326     c                   delete    FIFCE000
251900020326     c     Kfce          reade     FiFCE01L
252000020326     C                   enddo
252100020408     C                   endif
252200020326     C*          *-------------------------------------*
252300011105     C     KFTD          READE     fiftd001                               32
252400931022    1C                   ENDDO
252500931019     C*
252600931022     C* 14 OFF - SVALORIZZO ANCHE LA TESTATA
252700931022    1C     *IN14         IFEQ      *OFF
252800931022     C*
252900011105     C     KFTT          CHAIN     FIFTT000                           32
253000931022    2C     *IN32         IFEQ      *OFF
253100931022     C*
253200931022    3C     FTTFIT        IFEQ      ' '
253300931022     C                   CLEAR                   FTTITT
253400931022    3C                   ENDIF
253500940121     C*
253600931022    3C     FTTFIA        IFEQ      ' '
253700931022     C                   CLEAR                   FTTITA
253800931022    3C                   ENDIF
253900931022     C*
254000940121     C                   Z-ADD     VI4ITT        FTTITT
254100940121     C     VI4ITT        IFGT      0
254200940121     C                   MOVEL     'M'           FTTFIT
254300940121     C                   ENDIF
254400940121     C                   Z-ADD     VI4ITA        FTTITA
254500940121     C     VI4ITA        IFGT      0
254600940121     C                   MOVEL     'M'           FTTFIA
254700940121     C                   ENDIF
254800020326     C*
254900020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
255000020326     C* i primi tre caratteri
255100020326     c                   if        fttfgs = 0
255200020326     c                   movel     fttpdr        fttfgs
255300020326     c                   end
255400940121     C*
255500940121     C                   MOVEL     ' '           FTTFVL                          FLG VALORIZ
255600940121     C                   MOVEL     ' '           FTTFLA                          FLG MAN TAR
255700931123     C                   CLEAR                   FTTSNA
255800931125     C                   CLEAR                   FTTCLA
255900950119     C                   CLEAR                   FTTCPE
256000931123     C                   CLEAR                   FTTKFA
256100931123     C                   CLEAR                   FTTTVL
256200950119     C                   CLEAR                   FTTSNE
256300931202     C                   CLEAR                   FTTSNP
256400011105     C                   UPDATE    FIFTT000
256500931022    2C                   ENDIF
256600931022    1C                   ENDIF
256700931019     C*
256800931019     C                   ENDSR
256900020212     C*---------------------------------------------------------------*
257000931108     C* ANNULLAMENTO VALORIZZAZIONE DI UN TIPO PRESTAZ ---------------*
257100020212     C*---------------------------------------------------------------*
257200931019     C     ANNTSR        BEGSR
257300931021     C     VI4SCE        DOWEQ     'A'
257400931021     C* ELIMINO IL FLAG
257500931021     C                   MOVEL     ' '           VI4SCE
257600011109     c     vh4tpr        comp      2                                      26
257700931021     C                   UPDATE    FRB8SF4
257800931021     C*
257900931019     C                   Z-ADD     VI4PDR        W01PDR
258000931019     C                   MOVEL     VH4TSR        W01TSR
258100931108     C*
258200931019     C                   ADD       1             NRR4
258300931019     C     NRR4          CHAIN     FRB8SF4                            32
258400931019     C*
258500931108     C* LEGGO TUTTE LE DISTINTE O I RITIRI DI QUEL PADR/TIPO PRESTAZ
258600931019     C     *IN32         DOWEQ     *OFF
258700931019     C     VI4PDR        ANDEQ     W01PDR
258800931019     C     VH4TSR        ANDEQ     W01TSR
258900931019     C*
259000931019     C                   EXSR      ANNSPE
259100931019     C*
259200931103     C                   ADD       1             NRR4
259300931019     C     NRR4          CHAIN     FRB8SF4                            32
259400931019     C                   ENDDO
259500931021     C                   ENDDO
259600931019     C*
259700931019     C                   ENDSR
259800020212     C*---------------------------------------------------------------*
259900931019     C* CONTROLLO SFL6 -----------------------------------------------*
260000020212     C*---------------------------------------------------------------*
260100931019     C     CONTR6        BEGSR
260200011105     C*
260300931019     C                   Z-ADD     1             NRR6
260400931019     C                   SETOFF                                       90
260500931103     C                   CLEAR                   WALF3
260600931203     C                   CLEAR                   WREAD
260700931021     C                   SETON                                        36
260800931019     C                   READC     FRB8SF6                                32
260900931019     C*
261000931019    1C     *IN32         DOWEQ     *OFF
261100931203     C                   ADD       1             WREAD             5 0
261200931021     C*
261300931123     C* CONTROLLO SE HO INSERITO DEI VALORI MANUALMENTE
261400931123     C* T A R I F F A   B A S E
261500011109      *>>>>>>>>>>>>>>>
261600011109    2C     VI6ITT        IFNE      VH6ITT
261700931022     C*
261800011109    3C     VI6ITT        IFGT      0
261900931021     C                   MOVEL     'M'           VH6FIT
262000931022   X3C                   ELSE
262100931021     C                   MOVEL     ' '           VH6FIT
262200931022    3C                   ENDIF
262300011105     C*
262400011109     C                   MOVEL     VI6ITT        VH6ITT
262500011108     C*>>>>>>>>
262600931022    2C                   ENDIF
262700931021     C*
262800931022     C* T A R I F F A   A C C E S S O R I
262900011108      *>>>>>>>>>>>>>>>
263000011109    2C     VI6ITA        IFNE      VH6ITA
263100931022     C*
263200011109    3C     VI6ITA        IFGT      0
263300931206     C                   MOVEL     'M'           VH6FIA
263400931022   X3C                   ELSE
263500931206     C                   MOVEL     ' '           VH6FIA
263600931022    3C                   ENDIF
263700011105     C*
263800011109     C                   MOVEL     VI6ITA        VH6ITA
263900011108     C*>>>>>>>>
264000931022    2C                   ENDIF
264100931019     C*
264200931019     C* C O D I C E   T A R I F F A
264300931019     C* RICERCA
264400121221     c                   if        vh6targ <> 'S'
264500931019     C     '?'           SCAN      VI6CTR                                 90
264600931019     C*
264700931021    2C     *IN90         IFEQ      *ON
264800931028     C                   CLEAR                   PARAM3
264900931028     C                   MOVEL     VI4PDR        PA3PDR
265000931028     C                   MOVEL     PARSML        PA3SML
265100931028     C                   MOVEL     VH4TSR        PA3TSR
265200100513     C                   MOVEL     KPJBU         kpjbus
265300100513     C                   clear                   kpjbu
265400931028     C                   MOVEL     PARAM3        KPJBU
265500011112     C                   CALL      'FICN86R'
265600931028     C                   PARM                    KPJBA
265700931110     C                   MOVEL     KPJBU         PARAM3
265800100513     C                   MOVEL     KPJBUs        kpjbu
265900931028     C* SELEZIONE EFFETTUATA
266000931028     C     PA3SEL        IFEQ      ' '
266100931028     C                   MOVEL     PA3CTR        VI6CTR
266200931028     C                   ELSE
266300931028     C                   MOVEL     '000'         VI6CTR
266400931028     C                   ENDIF
266500121221      *
266600931028     C* RIEMETTO I PIEDI DEL VIDEO
266700931019     C                   WRITE     FRB8D07
266800931028     C*
266900000622   X2C                   ELSE
267000931019     C* CONTROLLO
267100931105     C                   TESTN                   VI6CTR               30
267200931105     C     *IN30         IFEQ      *OFF
267300931105     C                   SETON                                        4290
267400931105     C                   GOTO      ENDCT6
267500931105     C                   ENDIF
267600931105     C*
267700931019     C                   MOVEL     VI6CTR        W01CTR
267800121221      *
267900121221      * Controlla la Tariffa
268000931019     C                   EXSR      CTRTAR
268100121221      *
268200000622     C* PER NO 90 CONTROLLO SE LA TARIFFA TROVATA E' POSTA O BARTOLINI E SE CI SONO BOLLE
268300000622     C* POSTE O BARTOLINI
268400000622     C     *IN90         IFEQ      *OFF
268500000622     C*
268600000622     C     VH6POS        IFEQ      'S'
268700000622     C* E LA TARIFFA È BARTOLINI ERRORE
268800000622     C     FGTFTP        ANDNE     'O'
268900000623     C* ED ESISTE UNA TARIFFA BARTOLINI
269000000623     C     VH4CTB        ANDNE     '   '
269100040622     C*
269200040622     C*  fino al giorno 2004/08/10 il controllo è attivato
269300040622     c                   if        dataoggi < finoadata
269400000622     C                   SETON                                        9054
269500000622     C                   GOTO      ENDCT6
269600040622     c                   end
269700040622     C*
269800000622     C                   ENDIF
269900000622     C*
270000000622     C     VH6POS        IFNE      'S'
270100000622     C* E LA TARIFFA È BARTOLINI ERRORE
270200000622     C     FGTFTP        ANDEQ     'O'
270300000623     C* ED ESISTE UNA TARIFFA POSTE
270400000623     C     VH4CTP        ANDNE     '   '
270500000622     C                   SETON                                        9055
270600000622     C                   GOTO      ENDCT6
270700000622     C                   ENDIF
270800000622     C*
270900000622     C                   ENDIF
271000931021     C*
271100931021    3C     *IN90         IFEQ      *ON
271200931021    4C     *IN30         IFEQ      *OFF
271300000622     C                   SETON                                        40
271400931021   X4C                   ELSE
271500931021     C*
271600931021     C                   MOVEL     VI6CTR        WALF1             1
271700931021     C                   MOVEL     'TR'          COD
271800931021     C                   MOVEL     *BLANKS       KEY
271900931021     C                   MOVEL     WALF1         KEY
272000931021     C     KTAB          CHAIN     TABEL                              30
272100931021     C*
272200931021    5C     *IN30         IFEQ      *ON
272300931021     C     *IN30         OREQ      *OFF
272400931021     C     TBLFLG        ANDNE     ' '
272500931021     C                   SETON                                        42
272600931021   X5C                   ELSE
272700931021     C                   SETOFF                                       90
272800931021    5C                   ENDIF
272900931021    4C                   ENDIF
273000931021    3C                   ENDIF
273100121221     C*
273200000622    2C                   ENDIF
273300121221      *-----
273400121221     c                   endIF
273500121221      *-----
273600931022     C   90              GOTO      ENDCT6
273700020315     C* CONTROLLO NUMERO STOP: NON DEVE ESSERE > O = 9000
273800020315      * attenzione 99999 ha un significato ben preciso come STOP in quanto significa
273900020315      *  consegnato a magazzino.
274000020315     C     VI6STP        IFGE      9000
274100020419     C     VI6STP        oreq      0
274200950711     C                   SETON                                        4890
274300950711     C                   GOTO      ENDCT6
274400950711     C                   END
274500011109     C* >>>>>>>>>
274600011112      *
274700011109     c     tass_boldet   tag
274800011112      *
274900011112      *  flag per determinare da dove è richiamata la tassazione bolle
275000011112      *   se con la "T" oppure con l'"F2" dalla windowsfl in tal caso
275100011112      *    deve preparare i campi della window a ricevere i valori dalla
275200011112      *     tassazione automatica.
275300011112     c                   movel     ' '           bol_tassa         1
275400011109     C* >>>>>>>>>
275500011107     c* D e t t a g l i o    V a l o r i    s u    W i n d o w S f l
275600011107    2c     vi6sce        ifeq      'D'
275700011107     c                   movel     ' '           vi6sce
275800011108     c                   exsr      WndwSfl
275900011107     c                   endif
276000011107     C* >>>>>>>>>
276100931019     C*
276200931123     C* T A S S A Z I O N E   S I N G O L A   B O L L A
276300931022    2C     VI6SCE        IFEQ      'T'
276400931123     C                   MOVEL     ' '           VI6SCE
276500011108      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
276600931029     C* SOLO SE UNO DEI 2 IMPORTI E' A 0(E NON INSERITO IN PREC VIDEO)
276700011107      * >>>>>>>>>>>>>>>>>>>>>>>>>>>>
276800011105     C*
276900011109    3C     VI6ITT        IFEQ      0
277000011109     C     *IN15         ANDEQ     *OFF
277100011109     C     VI6ITA        OREQ      0
277200011109     C     *IN16         ANDEQ     *OFF
277300931029     C*
277400020122     C                   CLEAR                   FICNB2
277500020426     C                   MOVEL     'CALCVAL'     §TApgm
277600020213     C                   MOVEL     'S'           §TAtsm
277700020326     C                   MOVEL     vi4pdr        §TAfgs
277800020326     C                   MOVEL     VI4PDR        §TAPDR
277900950117     C                   MOVEL     VH4TSR        §TATSR                         CONSEGNA/RITIRO
278000931029     C                   MOVEL     PARSML        §TASML
278100020326     C                   MOVEL     vh4DDC        §TADDC
278200020326     C                   z-add     vi4ndc        §tandc
278300931202     C                   Z-ADD     1             §TASPP
278400950118     C                   Z-ADD     VH6SET        §TASET
278500950118     C                   MOVE      'S'           §TAPAG
278600020129     C                   z-add     vh6aas        §taaas
278700020129     C                   z-add     ftdlnp        §talnp
278800020129     C                   z-add     ftdnrs        §tanrs
278900020129     C                   z-add     vi6nsp        §tansp
279000950118     C* Tot.colli per piking
279100950118     C     VH4TSR        IFEQ      'C'
279200950118     C* Spedizione da non pagare
279300950119     C     VH6FIA        IFEQ      'B'
279400950118     C                   MOVE      *ZEROS        §TANCL
279500950118     C                   MOVE      *BLANKS       §TAPAG
279600950118     C                   ELSE
279700950118     C                   Z-ADD     FTDNCL        §TANCL
279800950118     C                   END
279900950118     C                   Z-ADD     FTDNCL        §TACPE
280000950118     C                   ELSE
280100950118     C* Tot.colli per etichettatura
280200950118     C                   Z-ADD     FTDNCL        §TANCL
280300950118     C                   Z-ADD     VH6NCE        §TACPE
280400950118     C                   END
280500950127     C                   MOVEL     VI4PEP        §TAPPE
280600021211     C                   movel     vi4FCS        §taFCS
280700950116     C                   Z-ADD     FTDPKL        §TAKFA
280800950116     C                   Z-ADD     FTDVLU        §TATVL
280900931029     C*
281000950118     C                   Z-ADD     VH6IFP        §TAIFP
281100011107     c                   movel     vi6div        §tadiv
281200950118     C                   MOVEL     VI6TSP        §TATSP
281300030320     C                   MOVEL     Vh6cbo        §TAcbo
281400950116     C                   MOVEL     VI6FTC        §TATC1
281500950116     C                   MOVEL     VI6FT2        §TATC2
281600950131     C                   MOVEL     VI6CAP        §TACAP
281700950118     C                   MOVEL     FTDFIN        §TAFIN
281800950118     C                   MOVEL     VH6FGC        §TAFGC
281900950118     C                   MOVEL     VH6GVA        §TAGVA
282000950118     C                   MOVEL     VH6SIC        §TASIC
282100931029     C                   MOVEL     VI6CTR        §TACTR
282200931029    5C     *IN15         IFEQ      *ON
282300950118     C     VH6SIC        OREQ      'S'
282400931029     C                   MOVEL     *HIVAL        §TAITT
282500931029     C                   ELSE
282600011109     C                   Z-ADD     VI6ITT        §TAITT
282700931029    5C                   ENDIF
282800931029     C*
282900931029    5C     *IN16         IFEQ      *ON
283000950119     C     VH6FIA        OREQ      'B'
283100931029     C                   MOVEL     *HIVAL        §TAITA
283200931029     C                   ELSE
283300011109     C                   Z-ADD     VI6ITA        §TAITA
283400931029    5C                   ENDIF
283500021212      *
283600050909     C* Comunque aggancia sempre la bolla in partenza x vedere
283700050909      *  il codice trattamento merce
283800050909     C                   clear                   DS1B
283900091012     c     bolla_ftd     chain     fnblp01l
284000050909     c                   if        %found(fnblp01l)
284100050909     C                   MOVE      blpCbo        §tacbo
284200050909     C                   MOVEL     '1B'          COD
284300050909     C                   MOVEL     *BLANKS       KEY
284400050909     C                   MOVEL     blpCTM        KEY
284500050909     C     KTAB          CHAIN     TABEL00F                           30
284600050909     c                   if        %found(TABEL00F)
284700050909     C                   MOVEL     TBLUNI        DS1B
284800021212      * ------------
284900050909      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
285000050909      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
285100050909      *   la tariffa di Etichettatura in generale.
285200050909      *
285300050909      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
285400050909      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
285500050909     c                   iF        ftdTSR = 'R'
285600050909     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
285700050909     c                   move      'N'           §taPPE
285800050909     c                   end
285900050909      * Se è un DISKC Bartolini la regola dice che deve essere comunque
286000050909      * pagata la spedizione al Padroncino
286100050909     c                   iF        §1Bdkc ='B' and ftdnrs = 0
286200050909     c                   eval      §taPPE = 'S'
286300050909     c                   eval      §tadkc = 'S'
286400050909     c                   end
286500050909     c                   end
286600050909      **
286700050909     c                   endiF
286800050909     c                   endiF
286900021212      * ------------
287000950131     C*
287100931124     C     §TAITA        IFEQ      0
287200931124     C     VH4TSR        ANDEQ     'C'
287300950131     C                   Z-ADD     VH6CAS        §TACAS
287400950131     C                   MOVEL     VH6VCA        §TAVCA
287500950131     C                   Z-ADD     VH6AAS        FTDAAS
287600950131     C                   Z-ADD     VI6NSP        FTDNSP
287700020129     C*
287800950131     C* CONTROLLO SE E' C/A PRONTAMENTE RECUPERATO
287900931124     C                   EXSR      CTRASS
288000931124     C                   ENDIF
288100090909     C*
288200090909     C*   Deve passare il codice del cliente solo se è un cliente particolare
288300090909     c     vh6KSC        lookup    KcliPAR                                30
288400090909     c                   if        *in30
288500090909     C                   Z-ADD     vh6KSC        §TAcliPAR
288600090909     C                   end
288700931029     C*
288800020122     C                   CALL      'FICNB2R'
288900020122     C                   PARM                    FICNB2
289000931029     C*
289100011107     C* di ritorno dall'elaborazione:
289200011107     C*
289300931029    5C     §TAERR        IFEQ      ' '
289400011107     c                   z-add     0             vi6tot
289500931108     C     *IN15         IFEQ      *OFF
289600950118     C     VH6SIC        ANDNE     'S'
289700011112      *
289800931108     C                   MOVEL     §TAITT        VH6ITT
289900931108     C                   ADD       §TATPE        VH6ITT
290000931108     C                   ADD       §TATST        VH6ITT
290100011107      * >>>>>>> su campo SFLWDW 1° riga
290200011109     C                   Z-ADD     VH6ITT        VI6ITT
290300011107     c                   add       vh6itt        vi6tot
290400011112      *
290500931108     C                   ENDIF
290600011112      *
290700950118     C     *IN16         IFEQ      *OFF
290800950119     C     VH6FIA        ANDNE     'B'
290900011112      *
291000011112     C                   MOVEL     §TAITA        VH6ITA
291100011112     C                   MOVEL     §TAITA        VI6ITA
291200011112      * >>>>>>> su campo SFLWDW 2° riga
291300011112     c                   add       vh6ita        vi6tot
291400011112      *
291500950118     C                   END
291600011112      *
291700931029   X5C                   ELSE
291800931029     C* ERRORE IN TASSAZIONE
291900931019     C                   SETON                                        4190
292000931022     C                   GOTO      ENDCT6
292100931029    5C                   ENDIF
292200931029    3C                   ENDIF
292300931029    2C                   ENDIF
292400011109     c*>>>>>>>>>>>>>
292500011109     c* se richiesta a tassazione dal dettaglio nella windsfl
292600011109     c*  deve tornare sul dettaglio della windowsfl.
292700011109     c                   if        bol_tassa = 'D'
292800011109     c                   eval      vi6sce = 'D'
292900011109     c                   goto      tass_boldet
293000011109     c                   endif
293100011109     c*>>>>>>>>>>>>>
293200011109     C*
293300931123     C* SE  S O M M A   P E S I   A   G I O R N A T A
293400931123    2C     VH4TSM        IFEQ      'G'
293500011109     C     VI6ITT        ANDEQ     0
293600950119     C     VH6FIA        ANDNE     'B'
293700950118     C     VH6SIC        ANDNE     'S'
293800931123     C                   MOVEL     VI6CTR        WALF1
293900931123     C* NON ACCETTO PIU' DI 1 COD TAR A KILO O A QUINTALE
294000931123     C*  OPPURE DEVO VALORIZZARE SUBITO LA BOLLA
294100931123    3C     WALF1         IFEQ      '0'
294200931123     C     WALF1         OREQ      '3'
294300931123     C*
294400931123    4C     WALF3         IFEQ      *BLANKS
294500931123     C                   MOVEL     VI6CTR        WALF3
294600931123   X4C                   ELSE
294700931123    5C     VI6CTR        IFNE      WALF3
294800931123     C                   SETON                                        4390
294900931123     C                   GOTO      ENDCT6
295000931123    5C                   ENDIF
295100931123    4C                   ENDIF
295200931123     C*
295300931123    3C                   ENDIF
295400931123    2C                   ENDIF
295500931019     C*
295600931123     C* V I S U A L I Z Z A Z I O N E   S I N G O L A   B O L L A
295700931022    2C     VI6SCE        IFEQ      'V'
295800931123     C                   MOVEL     ' '           VI6SCE
295900931123    3C     VH4TSR        IFEQ      'C'                                           CONSEGNE
296000931021     C                   MOVEL     VH6AAS        PA2AAS
296100931021     C                   MOVEL     FTDLNP        PA2LNP
296200931021     C                   MOVEL     FTDNRS        PA2NRS
296300931207     C                   MOVEL     VI6NSP        PA2NSP
296400931105     C                   MOVEL     '1'           PA2FLG
296500100513     C                   MOVEL     KPJBU         kpjbus
296600100513     C                   clear                   kpjbu
296700931021     C                   MOVEL     PARAM2        KPJBU
296800950116     C                   CALL      'FNLR36R'
296900931021     C                   PARM                    KPJBA
297000100513     C                   MOVEL     KPJBUs        kpjbu
297100931021     C*
297200931021     C                   WRITE     FRB8D07
297300931022     C                   SETON                                        90
297400931123     C                   GOTO      ENDCT6
297500931123    3C                   ENDIF
297600020326     C*
297700020326    3C     VH4TSR        ifeq      'R'                                           ritiri nuovi
297800020326    3C     vh6nsp        andgt     0
297900020326     C                   CLEAR                   DSLS05
298000020326     C                   MOVE      VH6AAS        PA0AAS
298100020326     C                   MOVE      FTDLNP        PA0LNP
298200020326     C                   MOVE      FTDNRS        PA0NRS
298300020326     C                   MOVE      VI6NSP        PA0NSP
298400020326     C                   MOVEL     RAGUT         PA0RSU
298500020326     C                   MOVE      SIMFEL        PA0SIM
298600100513     C                   MOVEL     KPJBU         kpjbus
298700100513     C                   clear                   kpjbu
298800020326     C                   MOVEL     DSLS05        KPJBU
298900020326     C                   CALL      'FNLS05R'
299000020326     C                   PARM                    KPJBA
299100100513     C                   MOVEL     KPJBUs        kpjbu
299200020326     C*
299300020326     C                   WRITE     FRB8D07
299400020326     C                   SETON                                        90
299500020326     C                   GOTO      ENDCT6
299600020326    3C                   end
299700020326     C*
299800931123    2C                   ENDIF
299900931019     C*
300000931022     C     ENDCT6        TAG
300100931105     C* BLOCCATO
300200950119     C     VH6FIA        IFEQ      'B'
300300931105     C                   SETON                                        13
300400931105     C                   ELSE
300500931105     C                   SETOFF                                       13
300600931105     C                   ENDIF
300700950117     C* RITORNO ALL'INCASSO
300800950117     C     VH6SIC        IFEQ      'S'
300900950117     C                   SETON                                        23
301000950117     C                   ELSE
301100950117     C                   SETOFF                                       23
301200950117     C                   ENDIF
301300931105     C*
301400931019     C                   Z-ADD     NRR6          REC6
301500931019     C                   UPDATE    FRB8SF6
301600931019     C*
301700931019     C   90              SETON                                        32
301800931019     C  N90              READC     FRB8SF6                                32
301900931019    1C                   ENDDO
302000931203     C*
302100931203     C*  SE: 90 OFF - NO ERRORE
302200931203     C*             - SE TROVATO ALMENO UN CODICE TARIFFA 0XX O 3XX
302300931203     C*             - TIPO SOMMA PESI A GIORNATA
302400931203     C*             - NO VARIATO TUTTE LE RIGHE DELLA DISTINTA
302500931203    1C     *IN90         IFEQ      *OFF
302600931203     C     WALF3         ANDNE     *BLANKS
302700931203     C     VH4TSM        ANDEQ     'G'
302800931203     C     WREAD         ANDNE     WFTD
302900931203     C*
303000931203     C                   MOVEL     W01CTP        WALF1
303100931203    2C     WALF1         IFEQ      '0'
303200931203     C     WALF1         OREQ      '3'
303300931203     C*
303400931203    3C     W01CTP        IFNE      WALF3
303500931203     C                   SETON                                        4390
303600931203     C                   CLEAR                   WALF3
303700931203     C     1             CHAIN     FRB8SF6                            30
303800931203     C                   GOTO      ENDCT6
303900931203    3C                   ENDIF
304000931203    2C                   ENDIF
304100931203    1C                   ENDIF
304200931203     C*
304300931021     C                   SETOFF                                       36
304400931019     C*
304500931021     C                   ENDSR
304600020212     C*---------------------------------------------------------------*
304700011108     C* Emette la finestra con il dettaglio degli importi tariffe ----*
304800020212     C*---------------------------------------------------------------*
304900011108     c     WndwSfl       begsr
305000011108     c*
305100011108     c     *like         define    w6ctot        wwctot
305200011108     c*
305300011108     c                   seton                                        35
305400011108     c                   write     frb8wc6
305500011123     c                   setoff                                       3561
305600011108     c*
305700011108     c                   clear                   nrw6
305800011108     c                   z-add     1             wrc6
305900011108     c                   z-add     ftdlnp        w6clnp
306000011108     c                   z-add     ftdnrs        w6cnrs
306100011218     c                   z-add     vi6nsp        w6cnsp
306200011108     c                   move      vi6ftc        w6cftc
306300011108     c                   move      vi6ft2        w6cft2
306400011108     c                   move      vi6con        w6ccon
306500011108     c                   move      vi6ia         w6cin
306600011108     c                   move      vi6div        w6cdiv
306700011108     c                   move      vi6cap        w6ccap
306800011108     c                   move      ftdfin        w6cfin
306900011108     c                   z-add     ftdncl        w6cncl
307000011108     c                   z-add     ftdpkl        w6cpkl
307100011108     c                   z-add     ftdvlu        w6cvlu
307200011108     c                   move      vi6tot        w6ctot
307300020128     c                   z-add     vi6itt        w6cbas
307400020128     c                   z-add     vi6ita        w6cacc
307500011109     c*------->
307600020327     c* Carica le 2 righe x importo Base e Accessori
307700020327     c                   do        *hival
307800020327     c* protect/no display
307900020327     c                   setoff                                       60
308000020327     c* riga del sfl
308100020327     c                   add       1             nrw6
308200020327     c*  deve caricare solo 2 righe
308300020327     c                   if        nrw6 > 2
308400020327     c                   leave
308500020327     c                   endif
308600020327     c*
308700020327     c                   z-add     nrw6          w1htrc
308800020327     c*
308900020327     c                   select
309000020327     c     nrw6          wheneq    1
309100020327     c     vp6itt        comp      '1'                                    60
309200020327     c                   movel     *blank        w1tipo
309300020327     c                   movel     'Base'        w1tipo
309400020327     c                   z-add     vi6itt        w1valr
309500020327     c                   z-add     vi6itt        w1hval
309600020327     c*
309700020327     c     nrw6          wheneq    2
309800020327     c     vp6ita        comp      '1'                                    60
309900020327     c                   movel     *blank        w1tipo
310000020327     c                   movel     'Accessori'   w1tipo
310100020327     c                   z-add     vi6ita        w1valr
310200020327     c                   z-add     vi6ita        w1hval
310300020327     c                   endsl
310400020327     c*
310500020327     c                   write     frb8ws6
310600020327     c                   enddo
310700011109     c*-------> Emissione Window
310800011108     c     resta_wdw     tag
310900011109      *                *----------------------*
311000011108     c                   write     frb8wd6
311100011108     c                   exfmt     frb8wc6
311200011109      *                *----------------------*
311300011123     C                   setoff                                       61
311400011108     c*  Controlli
311500011109      * ---------         ----------
311600020327     c* Cntrl le righe x importi modificati
311700020327     c                   z-add     0             nrw6
311800020327     c                   z-add     0             wwctot
311900020327      *------------        ---------------
312000020327     c* cicla sulle righe
312100020327     c                   do        *hival
312200020327     c                   add       1             nrw6
312300020327     c*
312400020327     c*  deve caricare solo 2 righe
312500020327     c                   if        nrw6 > 2
312600020327     c                   leave
312700020327     c                   endif
312800020327     c*
312900020327     c     nrw6          chain     frb8ws6
313000020327      *  totalizza le righe
313100020327     c                   add       w1valr        wwctot
313200020327     c* protect/no display
313300020327     c                   setoff                                       60
313400020327      *  campi protetti
313500020327     c     nrw6          ifeq      1
313600020327     c     vp6itt        comp      '1'                                    60
313700020327     c                   end
313800020327     c     nrw6          ifeq      2
313900020327     c     vp6ita        comp      '1'                                    60
314000020327     c                   end
314100020327      *
314200020327      *  x F8 -> Conferma
314300020327      *  valori modificati manualmente
314400020327     c   KB
314500020327     cor KH              IF        w1hval <>  w1valr
314600020327      *
314700020327     c                   MOVEL     ' '           flgfi
314800020327    3c                   if        w1valr > 0
314900020327     c                   MOVEL     'M'           flgfi             1
315000020327    3c                   end
315100020327      *
315200020327     c                   Select
315300020327      * Base
315400020327     c     nrw6          wheneq    1
315500020327     C                   movel     flgfi         VH6FIT
315600020327     C                   MOVEL     w1valr        vi6itt
315700020327     C                   MOVEL     vi6itt        VH6ITT
315800020327      * Accessori
315900020327     c     nrw6          wheneq    2
316000020327     C                   movel     flgfi         VH6FIA
316100020327     C                   MOVEL     w1valr        vi6ita
316200020327     C                   MOVEL     vi6ita        VH6ITA
316300020327      *
316400020327     c                   endsl
316500020327      *
316600020327     C                   z-add     w1valr        w1hval
316700020327     c                   endif
316800020327      *
316900020327     C                   move      w1valr        trebytes          3 0
317000020327     C                   if        w6cdiv ='ITL' and
317100020327     c                             trebytes > 0
317200020327     c                   seton                                        61
317300020327     c                   end
317400020327      *
317500020327     c                   update    frb8ws6
317600020327     c   61              leave
317700020327     c                   enddo
317800020327      * ---------         ----------
317900020327     c* resta sulla window se non ha premuto dei cmd
318000020327     c     *inkh         ifeq      *off
318100020327     c     *inkl         andeq     *off
318200020327     c     *inkb         andeq     *off
318300020327     c     *in61         oreq      *on
318400020327     c     *inkl         andeq     *off
318500020327     c                   goto      resta_wdw
318600020327     c                   end
318700020327      *
318800020327      * reimposta il totale delle singole righe
318900020327     c     *inkh         ifeq      *on
319000020327     c                   if        wwctot <>  vi6tot
319100020327     c                   z-add     wwctot        vi6tot
319200020327     c                   endif
319300020327     c                   end
319400020327     c*
319500020327     c*  Se richiesta la tassazione bolla in automatico
319600020327     c*   come se avesse chiesto "T" sulla scelta del SFL principale
319700020327     c*    e successivamente "D" di dettaglio.
319800020327     c     *inkb         ifeq      *on
319900020327     c                   move      'T'           vi6sce
320000020327     c                   move      'D'           bol_tassa
320100020327     c                   end
320200020327     c*
320300011109     c*
320400011108     c                   endsr
320500020212     c*---------------------------------------------------------------*
320600011108     C* VEDO SE E' C/A PRONTAMENTE RECUPERATO ------------------------*
320700020212     C*---------------------------------------------------------------*
320800011108     C     CTRASS        BEGSR
320900011108     C*
321000950131     C                   MOVE      *ZEROS        §TACA1
321100950131     C                   MOVE      *BLANKS       WTACPC            1
321200931124     C* SOLO SE INCASSATO IL C/A
321300950131    1C     §TACAS        IFGT      0
321400931124     C*
321500931124     C                   CLEAR                   WNUM3
321600931124     C                   SETOFF                                       09
321700931124     C*
321800931124     C* PER ESEGUIRLO EVENTUALEMTE 2 VOLTE
321900931124    2C     WNUM3         DOWLE     2
322000931124     C                   ADD       1             WNUM3
322100931124     C*
322200931124    3C     *IN09         IFEQ      *OFF                                          CON FTD
322300091012     C     Bolla_FTD     SETLL     FNARBK00
322400091012     C     Bolla_FTD     READE     FNARBK00                               30
322500931124   X3C                   ELSE
322600950116     C     KLBL          SETLL     FNARBK00                                      CON MAMMA
322700950116     C     KLBL          READE     FNARBK00                               30
322800931124    3C                   ENDIF
322900931124     C*
323000931124    3C     *IN30         DOWEQ     *OFF
323100931124     C*
323200931124     C* SE INSERITO COMPLETAMENTE OK
323300030418    4C     kARBCVB       IFEQ      'K5'
323400950131     C                   Z-ADD     §TACAS        §TACA1
323500950131     C                   MOVEL     'S'           WTACPC
323600931124     C                   SETON                                        30
323700931124   X4C                   ELSE
323800931124     C*
323900931124     C* SE MODIFICATO IMPORTO L'ASSEGNO SOLTANTO SE > 0 E PER LA DIFFER
324000030418    5C     kARBCVB       IFEQ      'KP'
324100931124     C                   SETON                                        30
324200030418     C     §TACAS        SUB       kARBCAS       §TACA1
324300950131    6C     §TACA1        IFLE      0
324400950131     C                   Z-ADD     0             §TACA1
324500931124   X6C                   ELSE
324600950131     C                   MOVEL     'S'           WTACPC
324700931124    6C                   ENDIF
324800931124    5C                   ENDIF
324900931124    4C                   ENDIF
325000931124     C*
325100950116     C  N30
325200091012     CANN09Bolla_FTD     READE     FNARBK00                               30
325300950116     C  N30
325400950116     CAN 09KLBL          READE     FNARBK00                               30
325500931124    3C                   ENDDO
325600931124     C*
325700931124     C* SE NON TROVATO NULLA GUARDO SE HA UNA MAMMA E CERCO SULLA MAM
325800950131    3C     WTACPC        IFEQ      ' '
325900931124     C     WNUM3         ANDEQ     1
326000931124     C*
326100091012     C     Bolla_FTD     SETLL     FNLBL000                               30
326200931124    4C     *IN30         IFEQ      *OFF
326300030418     C* per uscire dal ciclo
326400950119     C                   Z-ADD     3             WNUM3             3 0
326500931124   X4C                   ELSE
326600931124     C                   ADD       1             WNUM3
326700091012     C     Bolla_FTD     CHAIN     FNLBL000                           30
326800030418     C* deve eseguire solo se le linee di arrivo sono uguali fra madre e figlia
326900030418     C*  altrimenti forza l'uscita dal ciclo
327000030418    4c  n30              if        lbllan <> lbllap
327100030418     c                   leave
327200030418     c                   end
327300931124     C                   SETON                                        09
327400931124    4C                   ENDIF
327500931124     C*
327600931124     C                   ELSE
327700030418     C* per uscire dal ciclo
327800931124     C                   Z-ADD     3             WNUM3
327900931124    3C                   ENDIF
328000931124    2C                   ENDDO
328100931124    1C                   ENDIF
328200931029     C                   ENDSR
328300020212     C*---------------------------------------------------------------*
328400950116     C* AGGIORNAMENTO BOLLE DISTINTA/RITIRO --------------------------*
328500020212     C*---------------------------------------------------------------*
328600931021     C     AGGIO6        BEGSR
328700931021     C                   Z-ADD     1             NRR6
328800931021     C                   READC     FRB8SF6                                32
328900931109     C*
329000931109     C* SE GIA' VALORIZZATE AGGANCIO LA TESTATA
329100931112     C     VI1FVL        IFEQ      'V'
329200931123     C* SE SOMMA PESI A GIORNATA DEVO RICONFERMARE LA VALUTAZIONE
329300931123     C  N32VH4TSM        IFEQ      'G'
329400931123     C                   SETON                                        22
329500931123     C                   ENDIF
329600011105     C     KFTD          CHAIN     FIFTT000                           30
329700931109     C                   ENDIF
329800931021     C*
329900931021    1C     *IN32         DOWEQ     *OFF
330000020313      *
330100020313      * per distinguere il vecchio modo di lettura dei ritiri non è più possibile
330200020313      * utilizzare l'ind.19 ma il campo numero spedizione.
330300020313     c                   if        vh6nsp>0
330400020315     c                   if        vh4tsr='R'
330500020315      * vista per i nuovi ritiri x spedizione
330600020404     C     KFTD5         CHAIN     fiftd005                           32
330700020315     c                   else
330800020315      * seleziona solo i consegnati
330900020313     C     KFTD2         CHAIN     fiftd002                           30
331000020315     c                   end
331100020313     c                   else
331200020313     C     KFTD4         CHAIN     fiftd004                           30
331300020313     c                   end
331400931112     C*
331500020313    2C     *IN30         IFEQ      *OFF
331600931109     C*
331700931109     C* SE GIA' VALORIZZATE DEVO AGGIORNARE I CAMPI DI TOTALE
331800931109     C     VI1FVL        IFEQ      'V'
331900950118     C  N15VH6SIC        IFNE      'S'
332000931109     C                   SUB       FTDITT        FTTITT
332100011109     C                   ADD       VI6ITT        FTTITT
332200931110     C                   SETON                                        22
332300931109     C                   ENDIF
332400931110     C*
332500950119     C  N16VH6FIA        IFNE      'B'
332600931109     C                   SUB       FTDITA        FTTITA
332700011109     C                   ADD       VI6ITA        FTTITA
332800950118     C                   SETON                                        22
332900931109     C                   ENDIF
333000931109     C                   ENDIF
333100931109     C*
333200931029     C* SE IMPOSTATO VALORE DISTINTA BASE AZZERO SULLA BOLLA
333300931029     C     *IN15         IFEQ      *OFF
333400011109     C                   MOVEL     VI6ITT        FTDITT
333500931021     C                   MOVEL     VH6FIT        FTDFIT
333600931029     C                   ENDIF
333700931029     C* SE IMPOSTATO VALORE DISTINTA ACCESSORI AZZERO SULLA BOLLA
333800931029     C     *IN16         IFEQ      *OFF
333900011109     C                   MOVEL     VI6ITA        FTDITA
334000931021     C                   MOVEL     VH6FIA        FTDFIA
334100931029     C                   ENDIF
334200011030     C     VI6CTR        IFNE      VI4CTD
334300011106     c     vh4tpr        andeq     1
334400931022     C                   MOVEL     VI6CTR        FTDCTP
334500931109     C                   ENDIF
334600011123     C                   z-add     VI6STP        FTDSTP                          STOP
334700020326     C*
334800020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
334900020326     C* i primi tre caratteri
335000020326     c                   if        ftdfgs = 0
335100020326     c                   movel     ftdpdr        ftdfgs
335200020326     c                   end
335300020326     C*
335400020313     c                   if        vh6nsp > 0
335500020315     c                   if        vh4tsr='R'
335600020315     C                   UPDATE    fiftd005
335700020315     c                   else
335800020313     C                   UPDATE    fiftd002
335900020315     c                   end
336000020313     c                   else
336100020313     C                   UPDATE    fiftd004
336200020313     c                   end
336300020313     C*
336400931021    2C                   ENDIF
336500931021     C                   READC     FRB8SF6                                32
336600931021    1C                   ENDDO
336700931021     C*
336800931123     C* RIAGGIORNO LA TESTATA (PER EVITARE CHE NON SIA AGGIORNATA SE
336900931109     C*  ESCONO CON CMD12)
337000931109     C     VI1FVL        IFEQ      'V'
337100020326     C*
337200020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
337300020326     C* i primi tre caratteri
337400020326     c                   if        fttfgs = 0
337500020326     c                   movel     fttpdr        fttfgs
337600020326     c                   end
337700020326     C*
337800011105     C                   UPDATE    FIFTT000
337900931109     C                   ENDIF
338000931109     C*
338100931021     C                   ENDSR
338200020212     C*---------------------------------------------------------------*
338300931019     C* CONTROLLO CODICE TARIFFA -------------------------------------*
338400020212     C*---------------------------------------------------------------*
338500931019     C     CTRTAR        BEGSR
338600091112     C*
338700931123     C                   Z-ADD     1             C
338800931123     C     VH4TSR        LOOKUP    TSR(C)                                 30
338900931123     C  N30              Z-ADD     1             C
339000931123     C*
339100931123     C* SE E' IL CODICE TARIFFA GENERICO --> ERRORE
339200931123    1C     W01CTR        IFEQ      CTS(C)
339300931123     C                   SETON                                        90
339400931123   X1C                   ELSE
339500931123     C*
339600011112     C     KFGT2         CHAIN     FIFGT000                           30
339700011112     C*
339800011112     C* Modificato con le nuove regole di data scadenza e valuta
339900011112     C*  il modo di reperire il record valido.
340000011112    2C     *in30         doweq     *off
340100091112      *
340200011112     C     fgtatb        ifne      'A'
340300011112     C     w02div        andeq     fgtdiv
340400091112      *
340500091112     C* Se tariffa del periodo
340600091112     C     Ktgt          CHAIN     FItgt01l
340700091112 2/3 c                   if        %Found(FItgt01l) and
340800091112     c                             W02ddc >= tgtDDT and W02ddc <= tgtDST
340900091112      *
341000091112    3c     parsml        IFeq      *blank
341100091112     C     tgtDTS        aNDgt     0
341200091112     c     parsml        OReq      'S'
341300091112     C                   leave
341400091112    3C                   end
341500091112      *
341600091112 2/3 C                   end
341700091112      *
341800091112    2C                   end
341900091112      *
342000011112     C     kfgt2         reade     fifgt000                               30
342100011112    2C                   enddo
342200931019     C*
342300091112     C*  ????????????  strano test se trovato ritesta la data se minore della decorrenza ?????
342400931123    2C     *IN30         IFEQ      *ON
342500931019     C     *IN30         OREQ      *OFF
342600091112     C     W02DDC        ANDLT     tgtDDT
342700931019     C                   SETON                                        90
342800931123    2C                   ENDIF
342900091112     C*
343000931123    1C                   ENDIF
343100931021     C*
343200931021     C* 90 ON - TARIFFA INESISTENTE
343300931028     C*         VEDO SE HA ALMENO UN CODICE TARIFFA VALIDO
343400931021    1C     *IN90         IFEQ      *ON
343500931108     C* CERCO IL CODICE TARIFFA GENERICO DEL PRESTAZ DA ESCLUDERE
343600931028     C                   SETOFF                                       31
343700931028     C*
343800011112     C     KFGT          CHAIN     FIFGT000                           30
343900091112      *
344000091112     C*   in testata di periodo
344100091112     C  n30Ktgt          CHAIN     FItgt01l
344200091112 1/2 c  n30              if        %Found(FItgt01l)
344300931028     C*
344400931028     C* SE ANNULLATO O E' IL CODICE TARIFFA GENERICO RILEGGO
344500011112      * o se non è della stessa divisa o se scaduto
344600931022    2C  N30FGTATB        IFEQ      'A'
344700931028     C     FGTCTR        OREQ      CTS(C)
344800011112     C     fgtdiv        ORne      w02div
344900091112     C     tgtdst        ORgt      w02ddc
345000091112     C     tgtDTS        oreq      0
345100091112     c     parsml        andeq     *blank
345200931021     C                   SETON                                        31
345300931028    2C                   ENDIF
345400091112 1/2 c                   end
345500931021     C*
345600931021    2C     *IN30         DOWEQ     *OFF
345700931021     C     *IN31         ANDEQ     *ON
345800931021     C*
345900011112     C     KFGT          READE     FIFGT000                               30
346000091112      *
346100091112     C*   in testata di periodo
346200091112     C  n30Ktgt          CHAIN     FItgt01l
346300091112 2/3 c  n30              if        %Found(FItgt01l)
346400091112     C*
346500931022    3C  N30FGTATB        IFEQ      'A'
346600931028     C     FGTCTR        OREQ      CTS(C)
346700011112     C     fgtdiv        ORne      w02div
346800091112     C     tgtdst        ORgt      w02ddc
346900091112    3C     tgtDTS        oreq      0
347000020315    3c     parsml        andeq     *blank
347100931021     C                   SETON                                        31
347200931028     C                   ELSE
347300931028     C                   SETOFF                                       31
347400931021    3C                   ENDIF
347500931021     C*
347600091112 2/3 c                   end
347700091112     C*
347800931021    2C                   ENDDO
347900931021    1C                   ENDIF
348000931028     C*
348100931028     C* 90 ON 30 ON - NON ESISTONO TARIFFE PER IL CLIENTE
348200931021     C*
348300931019     C                   ENDSR
348400020212     C*---------------------------------------------------------------*
348500931111     C* CONTROLLO SE MANCATA CONSEGNA DA TASSARE ---------------------*
348600020212     C*---------------------------------------------------------------*
348700931111     C     CTRCMC        BEGSR
348800950116     C                   MOVE      VI4FIL        WALF6
348900931111     C     WALF6         LOOKUP    CMC                                    30
349000931111     C* SE INESISTENTE CERCO CON FILIALE A 0
349100931111     C     *IN30         IFEQ      *OFF
349200931111     C                   MOVE      000           WALF6
349300931111     C     WALF6         LOOKUP    CMC                                    30
349400931111     C                   ENDIF
349500931111     C                   ENDSR
349600020212     C*---------------------------------------------------------------*
349700931111     C* CONTROLLO CODICE BOLLA DA NON TASSARE ------------------------*
349800020212     C*---------------------------------------------------------------*
349900931111     C     CTRCBL        BEGSR
350000950116     C                   MOVEL     W018C         WALF6             6
350100950116     C                   MOVE      VI4FIL        WALF6
350200950116     C     WALF6         LOOKUP    CBL                                    31
350300931111     C* SE INESISTENTE CERCO CON FILIALE A 0
350400931111     C     *IN31         IFEQ      *OFF
350500950116     C                   MOVE      000           WALF6
350600950116     C     WALF6         LOOKUP    CBL                                    31
350700931111     C                   ENDIF
350800931111     C                   ENDSR
350900020212     C*---------------------------------------------------------------*
351000931021     C* ESEGUO VALORIZZAZIONE ----------------------------------------*
351100020212     C*---------------------------------------------------------------*
351200931021     C     VALZZ         BEGSR
351300931110     C* SPENGO IL 22 CHE OBBLIGA IL CMD6
351400931110     C                   SETOFF                                       22
351500931021     C                   Z-ADD     1             NRR4
351600931021    0C     NRR4          DOWLE     W04NRR
351700931021     C     NRR4          CHAIN     FRB8SF4                            32
351800931021     C*
351900931021    1C     *IN32         IFEQ      *OFF
352000931021     C*
352100931123     C* R E C O R D   D E L   T I P O   P R E S T A Z I O N E:
352200931029     C* ESCLUDO (MEMORIZZO IL COD.TARIFFA)
352300931021    2C     VH4NRR        IFEQ      0
352400011106     c                   if        vh4tpr = 1
352500011030     C                   MOVEL     VI4CTD        WALF3
352600011105     c                   end
352700000623     C* SE CI SONO TARIFFE BARTOLINI E POSTE LE MEMORIZZO
352800000623     C     VH4CTP        IFNE      *BLANKS
352900000623     C     VH4CTB        ANDNE     *BLANKS
353000000623     C                   MOVEL     VH4CTP        WCTP              3
353100000623     C                   MOVEL     VH4CTB        WCTB              3
353200000623     C                   ELSE
353300000623     C                   MOVEL     *BLANKS       WCTP
353400000623     C                   MOVEL     *BLANKS       WCTB
353500000623     C                   ENDIF
353600931029     C*
353700931021   X2C                   ELSE
353800931022     C*
353900931029     C* V A L O R I   B A S E   E   A C C E S S O R I   D I S T I N TA:
354000931029     C*  SE CI SONO ENTRAMBI NON FACCIO NULLA
354100931022    3C     VI4ITT        IFEQ      0
354200931022     C     VI4ITA        OREQ      0
354300931103     C*
354400931103     C* 15 ON - IMMESSA TARIFFA BASE
354500931103     C* 16 ON - IMMESSA TARIFFA ACCESSORI
354600931103     C     VI4ITT        COMP      0                                  15
354700931103     C     VI4ITA        COMP      0                                  16
354800931105     C* PULIZIA CAMPI DI TOTALE
354900931125     C                   CLEAR                   W01CLA
355000020212     C                   CLEAR                   g01CLA
355100950118     C                   CLEAR                   W01CPE
355200020212     C                   CLEAR                   g01CPE
355300931125     C                   CLEAR                   W01KFA
355400020212     C                   CLEAR                   g01KFA
355500931105     C                   CLEAR                   W01NCL
355600931105     C                   CLEAR                   W01TVL
355700020212     C                   CLEAR                   g01TVL
355800931105     C                   CLEAR                   W01SNA
355900020212     C                   CLEAR                   g01SNA
356000931105     C                   CLEAR                   W01STP
356100931105     C                   CLEAR                   W02STP
356200931123     C                   CLEAR                   W01CTP
356300931105     C                   CLEAR                   W01FIA
356400931105     C                   CLEAR                   W01FIT
356500931105     C                   CLEAR                   W01ITT
356600931105     C                   CLEAR                   W01ITA
356700950119     C                   CLEAR                   W01SNE
356800931202     C                   CLEAR                   W01SNP
356900931103     C*
357000931103     C* C O D I C E   T A R I F F A   I N E S I S T E N T E :
357100931103    4C     WALF3         IFEQ      '???'
357200931105     C*  VEDO SE DARE MANCA TARIFFA SOLO SE NON E' IMPOSTATA LA BASE
357300931105     C     *IN15         IFEQ      *OFF
357400931104     C                   MOVEL     SER(2)        W01ERR           25
357500931103     C                   EXSR      MANTAR
357600931105     C                   ENDIF
357700931103     C*
357800931103   X4C                   ELSE
357900931029     C*
358000931108     C     VH4TSR        IFEQ      'R'                                           RITIRI
358100931108     C                   SETON                                        19
358200931108     C                   ELSE
358300931108     C                   SETOFF                                       19         CONSEGNE
358400931108     C                   ENDIF
358500020325     C*
358600020325     C*  prima del ciclo di lettura di dettaglio resetta
358700020325     C*   i flags o i contatori
358800020325     c                   move      *blank        w01isola
358900020325     c                   move      *blank        w01xpres
359000030109     c                   move      *blank        w01disagiato      1
359100020325     c                   z-add     0             almuno_xpr
359200020325     c                   z-add     0             almuno_iso
359300030109     c                   z-add     0             almuno_disag
359400030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
359500030821     C*  lo stop.
359600030821     c                   z-add     0             W01nss
359700030821     c                   z-add     0             W01fw
359800030821     c                   z-add     0             W01rcc
359900030821     c                   move      *loval        savSTP
360000041124     C*
360100041124     C*   Comunque prima di valorizzare per sicurezza pulisce eventuali
360200041124     C*   records del FIFCE      *-------------*
360300041124     c                   exsr      Annulla_FCE
360400041124     C*                          *-------------*
360500931108     C*
360600011105     C     KFTD          SETLL     fiftd001
360700011105     C     KFTD          READE     fiftd001                               32
360800931021     C*
360900931104    5C     *IN32         DOWEQ     *OFF
361000020328     C*
361100030821     C*  a rottura di STOP conta quante spedizioni ci sono
361200030821     C*   all'interno dello stesso STOP
361300030821     c                   if        ftdSTP <> savSTP
361400030821     C*                          *-------------*
361500030821     c                   exsr      Sped_x_Stop
361600030821     C*                          *-------------*
361700030821     c                   eval         savSTP = ftdSTP
361800030821     c                   end
361900030821     C*
362000020422     C*               *--------------------*
362100950118     C                   EXSR      CTRTSM
362200020422     C*               *--------------------*
362300020328     C*
362400020328     C* scrive record x record sul workF
362500020423     C*  soo se hanno lo stop e se hanno un peso.
362600020620     c                   if        ftdstp <>0  and
362700020620     c                             ftdpkl <>0  and
362800020422    3C                             parsml = ' '
362900020328     C                   EXSR      wrt_FTDW
363000020419     c                   end
363100931021     C*
363200011105     C     KFTD          READE     fiftd001                               32
363300931104    5C                   ENDDO
363400931102     C*
363500931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA SOMMARE PER "G"
363600931102     C*                                 O DA TASSARE PER "T"
363700931102     C                   EXSR      CTRSTP
363800931103    4C                   ENDIF
363900931022     C*
364000950116     C* BASE  GIA' VALORIZZATA
364100931103    4C     *IN15         IFEQ      *ON
364200931105     C                   MOVEL     'M'           W01FIT
364300931105     C                   Z-ADD     VI4ITT        W01ITT
364400931103    4C                   ENDIF
364500950116     C* ACCESSORI  GIA' VALORIZZATI
364600931103    4C     *IN16         IFEQ      *ON
364700931105     C                   MOVEL     'M'           W01FIA
364800931105     C                   Z-ADD     VI4ITA        W01ITA
364900931103    4C                   ENDIF
365000931021     C*
365100931103   X3C                   ELSE
365200931022     C*
365300931022     C* BASE E ACCESSORI GIA' VALORIZZATI
365400931022     C                   Z-ADD     VI4ITT        W01ITT
365500931022     C                   Z-ADD     VI4ITA        W01ITA
365600931021     C                   MOVEL     'M'           W01FIT
365700931021     C                   MOVEL     'M'           W01FIA
365800931022     C*
365900931022     C* ANNULLO TUTTE LE EVENTUALI MEMORIZZAZIONI SULLE BOLLE
366000931022     C                   SETON                                        14
366100931022     C                   EXSR      ANNSPE
366200931022     C                   SETOFF                                       14
366300931022     C*
366400931021    3C                   ENDIF
366500931021     C* AGGIORNO TESTATA
366600011105     C     KFTD          CHAIN     FIFTT000                           30
366700020212     C                   z-add     g01CLA        FTTCLA
366800020212     C                   z-add     g01CPE        FTTCPE
366900020212     C                   z-add     g01KFA        FTTKFA
367000020212     C                   z-add     g01TVL        FTTTVL
367100020212     C                   z-add     g01SNA        FTTSNA
367200931022     C*
367300931022     C* SE TROVATI ERRORI IMPOSTO IL FLAG DI PRESENZA MANCA TARIFFA
367400931103    3C     VI4SCE        IFEQ      'E'
367500931022     C                   MOVEL     'S'           FTTFLA
367600931103   X3C                   ELSE
367700931022     C                   MOVEL     ' '           FTTFLA
367800931103    3C                   ENDIF
367900931022     C                   MOVEL     W01ITT        FTTITT
368000931022     C                   MOVEL     W01ITA        FTTITA
368100931021     C                   MOVEL     W01FIT        FTTFIT
368200931021     C                   MOVEL     W01FIA        FTTFIA
368300931202     C                   MOVEL     W01SNP        FTTSNP
368400950119     C                   MOVE      W01SNE        FTTSNE
368500950127     C                   MOVEL     VI4PEP        FTTPEP
368600021211     C                   movel     vi4FCS        fttLVL
368700931021     C                   MOVEL     'V'           FTTFVL
368800020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
368900020326     C* i primi tre caratteri
369000020326     c                   if        fttfgs = 0
369100020326     c                   movel     fttpdr        fttfgs
369200020326     c                   end
369300020326     C*
369400011105     C  N30              UPDATE    FIFTT000
369500020423     c*
369600020423     c* pulisce eventuali records del file di work rimasti in sospeso
369700020423     c*               *-----------------------*
369800020423     c  N30              exsr      del_FTDW0
369900020423     c*               *-----------------------*
370000931021    2C                   ENDIF
370100931021    1C                   ENDIF
370200931021     C*
370300931021     C                   ADD       1             NRR4
370400931103    0C                   ENDDO
370500931110     C* IMPOSTO IL FLAG DI VALORIZZATE
370600931110     C     *IN90         IFEQ      *ON
370700931110     C                   MOVEL     'V'           VI1FVL
370800931110     C                   MOVEL     CVALOR        VI1VAL
370900931110     C                   ENDIF
371000931021     C                   ENDSR
371100030821     C*---------------------------------------------------------------*
371200041124     C*  Ciclo preventivo di annullamento FIFCE se per problemi era
371300041124     C*  rimasto sporco. --> Non deve esserci nulla prima della Valorizzazione
371400030821     C*---------------------------------------------------------------*
371500041124     C     Annulla_FCE   BEGSR
371600041124      *
371700041124     c                   movel     vi4pdr        FGS_vi4pdr        3 0
371800041124      *          *-------------------------------------*
371900041124     c     parsml        ifeq      ' '
372000041124     c     Del_FCE       setll     fifce01l
372100041124     c     Del_FCE       reade     fifce01l
372200041124     c                   dow       not %Eof(fifce01l)
372300041124     c                   delete    fifce000
372400041124     c     Del_FCE       reade     fifce01l
372500041124     c                   enddo
372600041124     c                   end
372700041124      *          *-------------------------------------*
372800041124      *
372900041124     C                   ENDSR
373000041124     C*---------------------------------------------------------------*
373100041124     C* Conta quante Spedizioni ci sono all'interno dello Stesso Stop
373200041124     C*---------------------------------------------------------------*
373300041124     C     Sped_x_Stop   BEGSR
373400030821      * Azzera il contatore nr.di spedizioni x stop dopo aver calcolato
373500030821     C*  lo stop.
373600030821     c                   z-add     0             W01nss
373700030821     c                   z-add     0             W01fw
373800030821     c                   z-add     0             W01rcc
373900090911     c                   clear                   W01_TarPartCLI    1
374000090911     c                   clear                   W01_UnoOpiuCLI    7 0
374100030821      *
374200030821     C*  pgm x contare le spedizioni in STOP
374300030821     c                   clear                   FICNB1
374400030821     C                   eval      §t1PDR  =  ftdPDR
374500030821     C                   eval      §t1TSR  =  ftdTSR
374600030821     C                   eval      §t1NDC  =  ftdNDC
374700030821     C                   eval      §t1DDC  =  ftdDDC
374800030821     C                   eval      §t1STP  =  ftdSTP
374900030821     C*
375000030821     C                   CALL      'FICNB1R1'
375100030821     C                   PARM                    ficnb1
375200030821     C* Nr.Sped.totali in STOP
375300030821     c                   z-add     §T1nss        W01nss
375400030821     c                   z-add     §T1fw         W01fw
375500030821     c                   z-add     §T1rcc        W01rcc
375600090911     c                   eval       W01_TarPartCLI = §T1PARTIC
375700090911     c                   eval       W01_UnoOpiuCLI = §T1CLIPAR
375800030821      *
375900030821     C                   ENDSR
376000020212     C*---------------------------------------------------------------*
376100020328     C* Scrittura del file di work record x record
376200020212     C*---------------------------------------------------------------*
376300020328     C     wrt_FTDW      BEGSR
376400020328      *
376500020328     C                   z-add     ftdfgs        ftwfgs
376600020328     C                   z-add     ftdpdr        ftwpdr
376700020328     C                   move      ftdtsr        ftwtsr
376800020328     C                   z-add     ftdndc        ftwndc
376900020328     C                   z-add     ftdddc        ftwddc
377000020328     C                   z-add     ftdstp        ftwstp
377100020328     C                   z-add     ftdaas        ftwaas
377200020328     C                   z-add     ftdlnp        ftwlnp
377300020328     C                   z-add     ftdnrs        ftwnrs
377400020328     C                   z-add     ftdnsp        ftwnsp
377500020328     C                   z-add     ftdpkl        ftwpkl
377600020328     C                   z-add     ftdvlu        ftwvlu
377700020328     c                   write     fiftdw00
377800020328      *
377900020328     C                   ENDSR
378000020328     C*---------------------------------------------------------------*
378100020328     C* Cancella i record del work dopo aver eseguito lo stop
378200020328     C*---------------------------------------------------------------*
378300020328     C     del_FTDW      BEGSR
378400020328      *
378500020328     C                   z-add     ftdfgs        kftwfgs
378600020328     C                   z-add     ftdpdr        kftwpdr
378700020328     C                   move      ftdtsr        kftwtsr
378800020328     C                   z-add     ftdndc        kftwndc
378900020328     C                   z-add     ftdddc        kftwddc
379000020328     C                   z-add     ftdstp        kftwstp
379100020328      *
379200020328     c     kftdw         setll     fiftdw1l
379300020328     c     kftdw         reade     fiftdw1l
379400020328     c                   dow       not %eof(fiftdw1l)
379500020328     c                   delete    fiftdw00
379600020328     c     kftdw         reade     fiftdw1l
379700020328     C                   ENDdo
379800020328      *
379900020328     C                   ENDSR
380000020422     C*---------------------------------------------------------------*
380100020422     C* Cancella i record del work dopo aver eseguito lo stop
380200020422     C*---------------------------------------------------------------*
380300020422     C     del_FTDW0     BEGSR
380400020422      *
380500020423     C                   z-add     fttfgs        kftwfgs
380600020423     C                   z-add     fttpdr        kftwpdr
380700020423     C                   move      ftttsr        kftwtsr
380800020423     C                   z-add     fttndc        kftwndc
380900020423     C                   z-add     fttddc        kftwddc
381000020423      *
381100020422     c     kftdw0        setll     fiftdw1l
381200020422     c     kftdw0        reade     fiftdw1l
381300020422     c                   dow       not %eof(fiftdw1l)
381400020422     c                   delete    fiftdw00
381500020422     c     kftdw0        reade     fiftdw1l
381600020422     C                   ENDdo
381700020422      *
381800020422     C                   ENDSR
381900020328     C*---------------------------------------------------------------*
382000020328     C* CONTROLLO LA SOMMA PESI -------------------------------------*
382100020328     C*---------------------------------------------------------------*
382200020328     C     CTRTSM        BEGSR
382300931029     C                   SETOFF                                       17
382400020122     C                   CLEAR                   FICNB2
382500000626     C* VERIFICO SE BOLLA POSTA OPPURE NO
382600000626     C* CONTROLLO IN AZORG SE PO POSTE
382700000626     C                   EXSR      CTRORG
382800931123     C* MEMORIZZO CODICE TARIFFA
382900931123    1C     FTDCTP        IFEQ      *BLANKS
383000000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
383100000623    2C     WCTP          IFEQ      *BLANKS
383200000623     C     WCTB          ANDEQ     *BLANKS
383300931123     C                   MOVEL     WALF3         FTDCTP
383400000623     C                   ELSE
383500000623     C*  P.O. POSTE
383600020729     C     §OgNTW        IFEQ      'PPT'
383700000623     C                   MOVEL     WCTP          FTDCTP
383800000623     C                   ELSE
383900000623     C                   MOVEL     WCTB          FTDCTP
384000000623    3C                   ENDIF
384100000623     C*
384200000623    2C                   ENDIF
384300000623     C*
384400931123    1C                   ENDIF
384500931029     C*
384600950118     C* SOLO PER LE CONSEGNE: Verifica se spedizione da pagare
384700950118     C                   SETON                                        30
384800950118    6C     *IN19         IFEQ      *OFF
384900950118     C* T I P O   B O L L A   D A   T A S S A R E
385000950224     C                   CLEAR                   ARBCBO
385100091012     C     Bolla_FTD     CHAIN     FNARB01L                           32
385200950118     C                   EXSR      CTRCBL
385300030109      *
385400950118     C   31              SETOFF                                       30
385500950118     C* M A N C A T A   C O N S E G N A   D A   T A S S A R E
385600950118     C*  (SE NON CONSEGNATA)
385700950118    7C   30FTDDCM        IFEQ      0
385800950118     C*
385900950118    8C     FTDCMC        IFNE      *BLANKS
386000950118     C                   MOVEL     FTDCMC        WALF6
386100950118   X8C                   ELSE
386200950118     C                   MOVEL     FTDTMC        WALF6
386300950118    8C                   ENDIF
386400950118     C                   EXSR      CTRCMC
386500950118    7C                   ENDIF
386600950118    6C                   ENDIF
386700000626     C*  P.O. POSTE
386800050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
386900020729     C     §OgNTW        IFEQ      'PPT'
387000050426     C     vh4tpo        andeq     'S'
387100000626     C                   SETOFF                                       30
387200130305     C*
387300000626     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE SIGNIFICA CHE LA BOLLA
387400000626     C* SI DEVE PAGARE COME CONSEGNATA
387500000626     C                   MOVEL     '2Z'          COD
387600000626     C                   MOVEL     *BLANKS       KEY
387700000626     C                   MOVEL     FTDCMC        KEY
387800000626     C     KTAB          CHAIN     TABEL                              30
387900000626     C  N30              MOVEL     TBLUNI        DS2Z
388000000626     C   30              MOVEL     *BLANKS       DS2Z
388100000626     C     §2ZTPP        IFNE      *BLANKS
388200000626     C* LA BOLLA VIENE PAGATA
388300000626     C                   SETON                                        30
388400000626     C                   ENDIF
388500000626     C                   MOVEL     §2ZTPP        FTDFGC
388600000626    3C                   ENDIF
388700950118     C*
388800950118    6C     *IN30         IFEQ      *ON
388900950119     C                   MOVEL     'S'           W01PAG            1
389000950118     C                   ELSE
389100950119     C                   ADD       1             W01SNP                          no pagare
389200950118     C                   MOVEL     ' '           W01PAG
389300950118    6C                   ENDIF
389400950119     C*
389500950119     C     *IN19         IFEQ      *OFF
389600950119     C     W01PAG        ANDEQ     *BLANKS
389700950119     C     FTDDCM        ANDEQ     *ZEROS
389800950119     C                   ADD       1             W01SNE                          no cons.no pag
389900950119     C                   END
390000950118     C*
390100950118     C     W01PAG        IFEQ      'S'
390200950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
390300950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
390400931029     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
390500950123     C                   MOVE      'S'           §TAPAG
390600950118     C                   ELSE
390700950118     C                   MOVE      *ZEROS        §TAKFA                          PESO
390800950118     C                   MOVE      *ZEROS        §TATVL                          VOLUME
390900950118     C                   MOVE      *ZEROS        §TANCL                          COLLI
391000950123     C                   MOVE      ' '           §TAPAG
391100950118     C                   END
391200950118     C*
391300950118     C     *IN19         IFEQ      *ON
391400950118     C                   Z-ADD     FTDNCE        §TACPE                          COLLI ETICH
391500950118     C                   ELSE
391600950118     C                   Z-ADD     FTDNCL        §TACPE                          COLLI PIK.
391700950118     C                   END
391800950118     C*
391900950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
392000021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
392100950118     C*
392200950130     C                   Z-ADD     FTDSET        §TASET
392300931029     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
392400931029     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
392500020325     C*
392600020325     C* memorizza se c'è anteporto per Isola
392700020325     C*  ma attenzione,se almeno uno all'interno dello stesso STOP
392800020325     C* è diverso da isola, resetta il flag da passare al pgm di calcolo x stop
392900020325     C*
393000020325     C* Se almeno una delle spedizioni all'interno dello stop non è
393100020325     C*  un'isola ...lo memorizza.
393200020325     c                   if        almuno_iso = 1 and ftdFIN <> 'I'             Anteporto
393300020325     C                   move      *blank        w01isola                       se Isola
393400020325     c                   end
393500020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
393600020325     C*   all'interno dello stesso stop non sia gestita ad Isola che deve
393700020325     C*    segnalarlo.
393800020325     c                   if        almuno_iso = 0
393900020325     c                   z-add     1             almuno_iso        1 0
394000020325     C                   move      *blank        w01isola                       Anteporto
394100020325     c                   if        ftdFIN = 'I'                                 per indicare
394200020325     C                   move      'S'           w01isola                       se Isola
394300020325     c                   end
394400020325     c                   end
394500030109     C*
394600931111     C* SOMMA PESI A GIORNATA O A STOP:
394700931111     C* ELABORO COMUNQUE GLI ACCESSORI BOLLA PER BOLLA
394800931111     C*
394900020325      *  *-----------------------*
395000931111     C***   S T O P
395100020325      *  *-----------------------*
395200931111     C* SE NON C'E' IMPORTO MANUALE E NON E' VALORIZZATA BASE DISTINTA
395300931116     C* E NON E' SOLO UN RITORNO ALL'INCASSO
395400950126     C* E BASE SPEDIZIONE NON VALORIZZATA
395500931111     C*  ENTRO NEL CALCOLO DELLO "STOP"
395600931111    2C     FTDFIT        IFNE      'M'
395700931111     C     *IN15         ANDEQ     *OFF
395800931116     C     FTDSIC        ANDNE     'S'
395900960905    3C     FTDITT        IFEQ      *ZEROS
396000931111     C                   EXSR      CALSTP
396100960905   X3C                   ELSE
396200960906     C     FTDSTP        IFGT      *ZEROS
396300960906    4C     FTDSTP        ANDEQ     W01STP
396400960906     C                   CLEAR                   W01STP
396500960905     C                   CLEAR                   W01KFA
396600960905     C                   CLEAR                   W01NCL
396700960905     C                   CLEAR                   W01TVL
396800960905     C                   CLEAR                   W01CPE
396900960905    4C                   END
397000960905     C* SE ANCHE ACCESSORI PIENI NON TASSO
397100960905    4C     FTDITA        IFGT      0
397200960905     C     *IN16         OREQ      *ON
397300960905     C                   SETON                                        17
397400960905    4C                   ENDIF
397500960905    3C                   END
397600931111   X2C                   ELSE
397700931111     C* SE ANCHE ACCESSORI PIENI NON TASSO
397800931111    3C     FTDITA        IFGT      0
397900931111     C     *IN16         OREQ      *ON
398000931111     C                   SETON                                        17
398100931111    3C                   ENDIF
398200931111    2C                   ENDIF
398300931111     C*
398400931029     C* SOLO PER TARIFFA A KILO O QUINTALE
398500931123     C                   MOVEL     FTDCTP        WALF1
398600931111    2C     WALF1         IFEQ      '0'
398700931029     C     WALF1         OREQ      '3'
398800931029     C*
398900020325      *  *-----------------------*
399000931111     C***   G I O R N A T A
399100020325      *  *-----------------------*
399200931029     C* SOLO SE NON HO GIA' VALORIZZATO LA BASE
399300931111    4C     *IN15         IFEQ      *OFF
399400931029     C     FTDITT        ANDEQ     0
399500950118     C     W01PAG        IFEQ      'S'
399600931125     C                   ADD       FTDNCL        W01CLA                          COLL GIORN
399700020304     C                   ADD       FTDPKL        W01KFA                          PESO GIORN
399800020325     C                   ADD       FTDVLU        W01TVL                          VOL  GIORN
399900020325      *
400000020325      * non deve essere l'anteporto per Isole
400100020325      *  in quanto per le spedizioni Isole non si deve calcolare nè la
400200020325      *   base nè lo stop. Quindi escludo dalla somma della giornata i valori
400300020325      * per il calcolo .
400400020325     c     ftdfin        ifne      'I'
400500020325     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
400600020304     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
400700020304     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
400800020325     c                   end
400900020325      *
401000950118     C                   END
401100020320     C     *IN19         IFEQ      *on
401200020304     C                   ADD       FTDNCE        W01CPE                          COLLI ETICH
401300020304     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
401400950118     C                   ELSE
401500020304     C                   ADD       FTDNCL        W01CPE                          COLLI PIK.
401600020304     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
401700950118     C                   END
401800931029     C                   MOVEL     *HIVAL        §TAITT
401900931029     C*
402000950214     C     W01PAG        IFEQ      'S'
402100931111    5C     W02STP        IFNE      FTDSTP                                        STOP GIORN
402200931111    6C     W02STP        IFGT      0
402300931029     C                   ADD       1             W01SNA
402400020212     C                   ADD       1             g01SNA
402500931111    6C                   ENDIF
402600931102     C                   MOVEL     FTDSTP        W02STP
402700931111    5C                   ENDIF
402800931029     C*
402900931111    5C     W02STP        IFEQ      FTDSTP
403000931102     C     W02STP        ANDEQ     0
403100931029     C                   ADD       1             W01SNA
403200020212     C                   ADD       1             g01SNA
403300931111    5C                   ENDIF
403400931111    4C                   ENDIF
403500950214     C                   ENDIF
403600931029     C*
403700931111    4C     FTDITA        IFGT      0
403800931029     C     *IN16         OREQ      *ON
403900931029     C                   SETON                                        17
404000931111    4C                   ENDIF
404100931029     C*
404200020304    2C                   ENDIF
404300931111     C*
404400931111     C* T A S S A Z I O N E   B O L L A
404500931111     C*
404600950118     C* SE CI SONO ENTRAMBI GLI IMPORTI NON TASSO
404700931112    1C     FTDITT        IFGT      0
404800931112     C     *IN15         OREQ      *ON
404900950118     C     FTDSIC        OREQ      'S'                                          RITORNO INCASSO
405000931112     C     FTDITA        IFGT      0
405100931112     C     *IN16         OREQ      *ON
405200950118     C     W01PAG        OREQ      *BLANKS                                      NON PAGARE
405300931112     C                   SETON                                        17
405400931111    1C                   ENDIF
405500931112    1C                   ENDIF
405600931112     C*
405700020325     C                   movel     *blanks       §taISO
405800020213     C                   MOVEL     'S'           §TAtsm
405900931112     C                   EXSR      TASSAZ
406000931102     C*
406100020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
406200020326     C* i primi tre caratteri
406300020326     c                   if        ftdfgs = 0
406400020326     c                   movel     ftdpdr        ftdfgs
406500020326     c                   end
406600020326     C*
406700011105     C                   UPDATE    fiftd001
406800931029     C                   ENDSR
406900020212     C*---------------------------------------------------------------*
407000931022     C* CALCOLO LE SOMME PESI A STOP ---------------------------------*
407100020212     C*---------------------------------------------------------------*
407200931022     C     CALSTP        BEGSR
407300931102     C*
407400931102     C* SE TROVATO STOP ED E' CAMBIATO --> TASSO LO STOP
407500931102    1C     W01STP        IFNE      0
407600931102     C     FTDSTP        ANDNE     W01STP
407700931102     C     W01STP        ORNE      0
407800931123     C     FTDCTP        ANDNE     W01CTP
407900020212     C*
408000000626     C* SOLO PER DEBUG
408100000626     C     FTDSTP        IFEQ      3
408200000626     C     W01STP        ANDEQ     2
408300000626     C                   MOVE      2             W01STP
408400000626     C                   ENDIF
408500931102     C*
408600020313      *  nuovo modo di salvare la chiave della spedizione in quanto i ritiri
408700020313      *   nella nuova forma hanno la riga con la spedizione.
408800020315     c                   clear                   w01spc
408900020315     c                   clear                   w01spr
409000020315     c                   IF        ftdnsp > 0
409100020313     C                   MOVEL     FTDSPC        W01SPC
409200020313     c                   else
409300020313     C                   MOVEL     FTDSPR        W01SPR
409400020313     c                   end
409500020325     C*
409600931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
409700011105     C     W05NRR        CHAIN     fiftd000                           32
409800931102     C*
409900931102     C                   Z-ADD     W01KFA        §TAKFA
410000931102     C                   Z-ADD     W01TVL        §TATVL
410100931102     C                   Z-ADD     W01NCL        §TANCL
410200950118     C                   Z-ADD     W01CPE        §TACPE
410300950123     C                   MOVEL     'S'           §TAPAG
410400950118     C     §TAKFA        IFEQ      0
410500950118     C                   MOVE      *BLANKS       §TAPAG
410600950118     C                   END
410700950118     C*
410800931102     C                   MOVEL     *HIVAL        §TAITA
410900020325     C                   movel     w01isola      §taISO
411000931102     C*
411100020213     C                   MOVEL     'T'           §TAtsm
411200931102     C                   EXSR      TASSAZ
411300020328      *
411400020325     C*  resetta i contatori per le isole e per l'espresso.
411500020325     c                   z-add     0             almuno_iso        1 0
411600020325     c                   z-add     0             almuno_xpr        1 0
411700030109     C*  resetta a livello di stop il destinatario disagiato
411800030109     c                   z-add     0             almuno_disag      1 0
411900931102     C*
412000020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
412100020326     C* i primi tre caratteri
412200020326     c  n32              if        ftdfgs = 0
412300020326     c                   movel     ftdpdr        ftdfgs
412400020326     c                   end
412500020326     C*
412600011105     C  N32              UPDATE    fiftd000
412700020325     C*
412800931102     C* RILEGGO IL RECORD
412900020313      * prima testava l'indicatore adesso non è più possibile distinguere i ritiri
413000020313      * allo stesso modo
413100020315     c                   IF        w01spc <> *blank
413200931202     C                   MOVEL     W01SPC        FTDSPC
413300931202     C                   MOVEL     FTDAAS        VH6AAS
413400931202     C                   MOVEL     FTDNSP        VI6NSP
413500020315     c                   if        vh4tsr='R'
413600020315      * vista per i nuovi ritiri x spedizione
413700020315     C     KFTD5         CHAIN(N)  fiftd005                           32
413800020315     c                   else
413900020315      * seleziona solo i consegnati
414000011105     C     KFTD2         CHAIN(N)  fiftd002                           32
414100020315     c                   end
414200940415   X2C                   ELSE
414300931202     C                   MOVEL     W01SPR        FTDSPR
414400931202     C                   MOVEL     FTDKSC        VI6NSP
414500011105     C     KFTD4         CHAIN(N)  fiftd004                           32
414600940415    2C                   ENDIF
414700931102     C*
414800020122     C                   CLEAR                   FICNB2
414900931102     C                   CLEAR                   W01KFA
415000931102     C                   CLEAR                   W01NCL
415100931102     C                   CLEAR                   W01TVL
415200950118     C                   CLEAR                   W01CPE
415300931102     C* REIMPOSTO I CAMPI
415400950118     C     W01PAG        IFEQ      'S'
415500950116     C                   Z-ADD     FTDPKL        §TAKFA                          PESO
415600950116     C                   Z-ADD     FTDVLU        §TATVL                          VOLUME
415700931102     C                   Z-ADD     FTDNCL        §TANCL                          COLLI
415800950125     C                   MOVEL     'S'           §TAPAG
415900950118     C                   ELSE
416000950118     C                   MOVE      *ZEROS        §TAKFA
416100950118     C                   MOVE      *ZEROS        §TATVL
416200950118     C                   MOVE      *ZEROS        §TANCL
416300950125     C                   MOVEL     ' '           §TAPAG
416400950118     C                   END
416500020313      *
416600020320     C     *in19         ifeq      *on
416700950118     C                   Z-ADD     FTDNCE        §TACPE
416800950118     C                   ELSE
416900950118     C                   Z-ADD     FTDNCL        §TACPE
417000950118     C                   END
417100020313      *
417200950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
417300021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
417400931102     C                   Z-ADD     FTDITT        §TAITT                          TAR BASE
417500931102     C                   MOVEL     FTDITA        §TAITA                          TAR ACCES
417600931110     C                   MOVEL     FTDSTP        W01STP
417700940415     C* MEMORIZZO CODICE TARIFFA
417800940415    2C     FTDCTP        IFEQ      *BLANKS
417900940415     C                   MOVEL     WALF3         FTDCTP
418000000623     C* E NON ESISTONO ALTRE TARIFFE POSTE E BARTOLINI
418100000623    3C     WCTP          IFEQ      *BLANKS
418200000623     C     WCTB          ANDEQ     *BLANKS
418300000623     C                   MOVEL     WALF3         FTDCTP
418400000623     C                   ELSE
418500000623     C* VERIFICO SE BOLLA POSTA OPPURE NO
418600000623     C* CONTROLLO IN AZORG SE PO POSTE
418700000623     C                   EXSR      CTRORG
418800000623     C*  P.O. POSTE
418900020729     C     §OgNTW        IFEQ      'PPT'
419000000623     C                   MOVEL     WCTP          FTDCTP
419100000623     C                   ELSE
419200000623     C                   MOVEL     WCTB          FTDCTP
419300000623    4C                   ENDIF
419400000623     C*
419500000623    3C                   ENDIF
419600000623     C*
419700940415    2C                   ENDIF
419800931102    1C                   ENDIF
419900931102     C*
420000931022     C* C'E' LO STOP E LA BASE NON E' TASSATA(NE' DI BOLLA NE' DI DIST)
420100931102    1C     FTDSTP        IFGT      0
420200931022     C                   MOVEL     FTDSTP        W01STP                          SALVO STOP
420300931123     C                   MOVEL     FTDCTP        W01CTP                          SALVO CTR
420400931102     C                   Z-ADD     FTDNRR        W05NRR            9 0
420500950118     C     W01PAG        IFEQ      'S'
420600931022     C                   ADD       FTDNCL        W01NCL
420700950119     C                   END
420800950119     C*
420900931022     C                   MOVEL     *HIVAL        §TAITT
421000931102    2C     FTDITA        IFGT      0
421100931022     C     *IN16         OREQ      *ON
421200931022     C                   SETON                                        17
421300931102    2C                   ENDIF
421400931102    1C                   ENDIF
421500931022     C*
421600931022     C                   ENDSR
421700020212     C*---------------------------------------------------------------*
421800931102     C* TASSAZIONE BOLLA ---------------------------------------------*
421900020212     C*---------------------------------------------------------------*
422000931102     C     TASSAZ        BEGSR
422100091012      *
422200931111     C* 17 OFF - D A   T A S S A R E
422300931111    0C     *IN17         IFEQ      *OFF
422400931123     C                   MOVEL     FTDCAP        §TACAP                          CAP MIT/DES
422500020326     C                   MOVEL     ftdfgs        §TAfgs
422600091012      *
422700020326     c                   if        §tafgs=0
422800020326     C                   MOVEL     FTDPDR        §TAfgs                          P.O. di gestione
422900020326     c                   end
423000091012      *
423100931102     C                   MOVEL     FTDPDR        §TAPDR                          PADRONCINO
423200931102     C                   MOVEL     PARSML        §TASML                          SIMULAZ
423300020326     C                   MOVEL     vh4DDC        §TADDC                          DATA DISTIN
423400020326     C                   z-add     vi4ndc        §tandc
423500020129     C                   z-add     ftdaas        §taaas
423600020129     C                   z-add     ftdlnp        §talnp
423700020129     C                   z-add     ftdnrs        §tanrs
423800020129     C                   z-add     ftdnsp        §tansp
423900020328     C                   z-add     ftdstp        §tastp
424000091012      *
424100020225     c                   if        §tansp = 0 and ftdtsr='R'
424200020225     C                   z-add     ftdksc        §tansp
424300020326     C                   z-add     ftdksc        §taksc
424400020225     c                   end
424500050909      *
424600050909     C* Comunque aggancia sempre la bolla in partenza x vedere
424700050909      *  il codice trattamento merce
424800091012     c                   clear                   trovato_arb       1
424900091012     c                   clear                   trovato_blp       1
425000050909     C                   clear                   DS1B
425100091012     c     bolla_ftd     chain     fnblp01l
425200050909     c                   if        %found(fnblp01l)
425300091012      * ------------
425400091012     c                   move      'S'           trovato_blp       1
425500091012     C                   MOVE      blpCbo        §tacbo
425600091012     C                   z-add     blpKSC        CodCliente        7 0
425700130305     C                   MOVEL     blpTSP        §TATSP                          TIPO SPEDIZ
425800091012      *
425900091012     C                   MOVEL     '3A'          COD
426000091012     C                   MOVEL     *BLANKS       KEY
426100091012     C                   MOVEL     blpCBO        KEY
426200091012     C     KTAB          CHAIN     TABEL                              30
426300091012     C  N30              MOVEL     TBLUNI        DS3A
426400091012     c                   If        %subst(§3ATB1:1:1) ='A'
426500091012     C     Bolla_FTD     CHAIN     FiAR601L
426600091012     c                   if        %Found(Fiar601L)
426700091012     C                   Z-ADD     ar6KSC        CodCliente                      Cliente
426800091012     c                   end
426900091012      **   SOLO X RITIRI
427000091012     c                   iF        ftdtsr = 'R'
427100091012     C                   MOVEL     '1B'          COD
427200091012     C                   MOVEL     *BLANKS       KEY
427300091012     C                   MOVEL     blpCTM        KEY
427400091012     C     KTAB          CHAIN     TABEL                              30
427500091012     c                   if        %found(TABEL00F)
427600091012     C                   MOVEL     TBLUNI        DS1B
427700091012      * deve controllare se abilitata a tariffa Disk C
427800091012      *  Attenzione : se il padroncino non etichetta poichè è un Disk B oppure
427900091012      *   perchè il diskC lo ha già etichettato il cliente non deve prendere
428000091012      *   la tariffa di Etichettatura in generale.
428100091012      *
428200091012      *   -  Si riconosce un Disk B se sulla bolla c'è il nr.serie
428300091012      *   -  oppure dal tratt.merce se come diskC ha etichettato il cliente
428400091012     c                   iF        ftdnrs <>0 or §1Bdkc ='C'
428500091012     c                   move      'N'           §taPPE
428600091012     c                   end
428700091012      * Se è un DISKC Bartolini la regola dice che deve essere comunque
428800091012      * pagata la spedizione al Padroncino
428900091012     c                   iF        §1Bdkc ='B' and ftdnrs = 0
429000091012     c                   eval      §taPPE = 'S'
429100091012     c                   eval      §tadkc = 'S'
429200091012     c                   end
429300091012     c                   end
429400091012      **
429500091012     c                   endiF
429600091012      *
429700091012     c                   endiF
429800091012      * ------------
429900091012     c                   end
430000091012      * Se non ha trovato il BLP
430100091012      *  oppure
430200091012      * Comunque se si tratta di consegne
430300091012     c                   if        NOT %found(fnblp01l) or FTDTSR = 'C'
430400091012     c     bolla_ftd     chain     fnarb01l
430500091012     c                   if        %found(fnarb01l)
430600091012     c                   move      'S'           trovato_arb       1
430700091012     C                   MOVE      arbCbo        §tacbo
430800091012     C                   z-add     arbKSC        CodCliente        7 0
430900130305     C                   MOVEL     ARBTSP        §TATSP                          TIPO SPEDIZ
431000130305     C* SOLO SE CONSEGNATA
431100130305    2C     FTDDCM        IFGT      0
431200130305     C                   MOVEL     ARBTC1        §TATC1                          CONS PARTIC
431300130305     C                   MOVEL     ARBTC2        §TATC2                          CONS PARTIC
431400130305    2C                   ENDIF
431500130305     C* GESTIONE PARTICOLARITA' VARIE
431600130305     C                   MOVEL     ARBGVA        §TAGVA
431700130305     C                   MOVE      ' '           §TAGVA
431800130305     C*
431900091012     c                   end
432000091012     c                   end
432100091012      *
432200091012      *  Carica i campi di passaggio
432300931202     C                   Z-ADD     1             §TASPP                          NUM STOP
432400931123     C                   MOVEL     FTDTSR        §TATSR                          TIPO PREST
432500931108     C                   MOVEL     FTDFIN        §TAFIN                          INOLTRO/ANT
432600931124     C                   MOVEL     FTDCTP        §TACTR                          COD TARIFFA
432700931102     C*
432800931102     C* SE GIA' VALORIZZATA IMPOSTO MAX VALORE PER NON CALCOLARLA
432900931102     C   15              MOVEL     *HIVAL        §TAITT                          BASE
433000931102     C   16              MOVEL     *HIVAL        §TAITA                          ACCESSORI
433100950118     C*
433200950118     C* IMPORTO  FATTURA ASSEGNATO INCASSATO
433300950118     C                   Z-ADD     FTDIFP        §TAIFP
433400011109      *  Divisa
433500011109     c                   if        vh4tpr = 2
433600011109     c                   movel     vi4ctd        §tadiv
433700011109     c                   end
433800950118     C* FLAG SOLO INCASSO
433900950118     C                   MOVEL     FTDSIC        §TASIC
434000130305     C* *******
434100020325     C                   MOVEL     *blank        §taxpr
434200020325     C* Se a spedizione
434300020325     c     §tatsm        ifeq      'S'
434400130305      **
434500130305     c     trovato_arb   IFEQ      'S'
434600030109     C*----------
434700020325     C* memorizza se all'interno dello stop sono tutte spedizioni espresso
434800020325     C* Se almeno una delle spedizioni all'interno dello stop non è
434900020325     C*  espresso ...lo segnala.
435000130305     c                   if        almuno_xpr = 1 and arbtsp <> 'E'
435100130305     c                             and arbtsp <> 'H'
435200020325     C                   move      *blank        w01xpres
435300020325     c                   end
435400020325     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
435500020325     C*   all'interno dello stesso stop non sia un espresso che deve segnalarlo
435600020325     c                   if        almuno_xpr = 0
435700020325     c                   z-add     1             almuno_xpr        1 0
435800020325     C                   move      *blank        w01xpres                       Espresso
435900130305     c                   if        arbtsp = 'E' or arbtsp = 'H'
436000020325     C                   move      'S'           w01xpres
436100020325     c                   end
436200020325     c                   end
436300030109     C*----------
436400030109     C* Se almeno una delle spedizioni all'interno dello stop non ha
436500030109     C*  il destinatario disagiato lo memorizza per non pagare il disagio.
436600030109     c                   if        almuno_disag = 1
436700030109     c                   if        arbtc1 = ' ' and arbtc2 = ' ' or
436800030109     c                             arbdcm = 0
436900030109     C                   move      *blank        w01disagiato                   non disagiato
437000030109     c                   end
437100030109     c                   end
437200030109     C*  deve andare per eccezione ossia basta che una sola delle spedizioni
437300030109     C*   all'interno dello stesso stop non sia segnalata come destinatario
437400030109     C*    disagiato che deve tenerlo presente per tutte
437500030109     c                   if        almuno_disag = 0
437600030109     c                   z-add     1             almuno_disag      1 0
437700030109     C                   move      *blank        w01disagiato
437800030109     c                   if        arbtc1 = 'X' or arbtc2 = 'X'                 per indicare
437900030109     c                   if        arbdcm > 0                                   se nella spediz.
438000030109     C                   move      'S'           w01disagiato                   il destinatario
438100030109     c                   end                                                    è disagiato
438200030109     c                   end                                                    è disagiato
438300030109     c                   end
438400020325     C*
438500130305     c                   END
438600130305      **
438700020325     c                   ElsE
438800130305      **   S T O P
438900020325     C* solo a STOP passa il flag valorizzato per il calcolo
439000020325     C                   move      w01xpres      §taxpr
439100030109     C                   move      w01disagiato  §taDED
439200020325     c                   endIF
439300931124     C*
439400931124    2C     §TAITA        IFEQ      0
439500950131     C* CONTRASSEGNO
439600950131     C                   Z-ADD     FTDCAS        §TACAS
439700950131     C                   MOVEL     FTDVCA        §TAVCA
439800011106      *   con l'EURO non è più vero che la divisa lira=*blank
439900011106     c                   if        §tavca=*blank
440000011106     c                   eval      §tavca='ITL'
440100011106     c                   end
440200011106      *
440300011106      *   con l'EURO è tolto ogni tipo di cambio dai files
440400011106      *    quindi adesso occorre passare a (0) sempre il cambio
440500011106      *     sarà il programma FNLRB9R che dovrà eseguire il
440600011106      *      ragionamwento di conversione DIVISA/EURO.
440700020129     C*
440800950131     C* VEDO SE C/A PRONTAMENTE RECUPERATO
440900931124     C                   EXSR      CTRASS
441000931124    2C                   ENDIF
441100000705     C*
441200130305     C* SE IL CODICE MANCATA CONSEGNA È VALORIZZATO
441300000705     C                   MOVEL     *BLANK        §TAFGC
441400000623    2C     FTDCMC        IFNE      *BLANK
441500000623     C* VERIFICO SE BOLLA POSTA
441600000623     C                   EXSR      CTRORG
441700000623     C*  P.O. POSTE
441800050426     C*    e se in presenza di una tariffa poste (altrimenti ha la tar.Bartolini)
441900020729     C     §OgNTW        IFEQ      'PPT'
442000050426     C     vh4tpo        andeq     'S'
442100130305     C*
442200000623     C* VERIFICO SE LEGATO ALL'EVENTO C'È UN CODICE DI CONSEGNA PARTICOLARE E LO METTO NEL
442300000623     C* FLAG FGC SOLO SE E' UNA CONSEGNA DI UNA BOLLA POSTA
442400000623     C                   MOVEL     '2Z'          COD
442500000623     C                   MOVEL     *BLANKS       KEY
442600000623     C                   MOVEL     FTDCMC        KEY
442700000623     C     KTAB          CHAIN     TABEL                              30
442800000623     C  N30              MOVEL     TBLUNI        DS2Z
442900000623     C   30              MOVEL     *BLANKS       DS2Z
443000000623     C                   MOVEL     §2ZTPP        §TAFGC
443100000623     C                   MOVEL     §2ZTPP        FTDFGC
443200000623    3C                   ENDIF
443300000623    2C                   ENDIF
443400091012     C*----------
443500950125     C     §TAPAG        IFEQ      ' '
443600950125     C                   MOVE      *HIVAL        §TAITA
443700950125     C                   END
443800950125     C     §TASIC        IFEQ      'S'
443900950125     C                   MOVE      *HIVAL        §TAITT
444000950125     C                   END
444100030821     C*----------
444200030821     C* contatore delle spedizioni presenti nello stesso STOP
444300030821     C* di quante "FW" e di quante "RCC".
444400030821     c     §tatsm        ifeq      'S'
444500130305      **
444600030821     c                   z-add     W01nss        §taNSS
444700030821     c                   z-add     W01fw         §taFW
444800030821     c                   z-add     W01rcc        §taRCC
444900030821     c                   z-add     W01nss        SAVnss
445000030821     c                   z-add     W01fw         SAVfw
445100030821     c                   z-add     W01rcc        SAVrcc
445200090911      *
445300090911      *  sullo stop tariffe particolari x cliente
445400090914     c                   eval       SAV_TarPartCLI = W01_TarPartCLI
445500090914     c                   eval       SAV_UnoOpiuCLI = W01_UnoOpiuCLI
445600090918     C*
445700090918      * se valorizza a Spedizione
445800090918     C*   Deve passare il codice del cliente solo se è un cliente particolare
445900090918     c     CodCliente    lookup    KcliPAR                                30
446000090918     c                   if        *in30
446100130307     C                   Z-ADD     CodCliente    §TAcliPAR
446200090918     C                   end
446300090911      *
446400030821     c                   else
446500090918      * se valorizza a Stop
446600090918     C*
446700030821      * quando è a stop imposta i campi precedentemente salvati altrimenti
446800030821      * sarebbero impostati per il giro successivo del nuovo STOP
446900030821     c                   z-add     SAVnss        §taNSS
447000030821     c                   z-add     SAVfw         §taFW
447100030821     c                   z-add     SAVrcc        §taRCC
447200090911      *
447300090911      *  sullo stop tariffe particolari x cliente
447400090911     c                   eval       §TACLIPAR      = SAV_UnoOpiuCLI
447500090911     c                   eval       §TATUTTE       = SAV_TarPartCLI
447600090911      *
447700030821     c                   end
447800030821     C*----------
447900020426     C                   MOVEL     'CALCVAL'     §TApgm
448000020205     C*
448100020122     C                   CALL      'FICNB2R'
448200020122     C                   PARM                    FICNB2
448300020328      *
448400020328      * cancella i records utilizzati nello STOP dal file di work
448500020328     C     §TAtsm        IFEQ      'T'
448600020422    3C     parsml        andeq     *blank
448700020328     c                   exsr      del_FTDW
448800020328     C                   END
448900020328      *
449000931102    0C                   ENDIF
449100931102     C*
449200931102     C* E R R ="E"  M A N C A   T A R I F F A
449300931103    1C     §TAERR        IFEQ      'E'
449400931102     C                   SETON                                        4290
449500931102     C* ERRORE: SEGNALO LA DISTINTA
449600931112     C     VI4SCE        IFNE      'E'
449700931102     C                   MOVEL     'E'           VI4SCE
449800011109     c     vh4tpr        comp      2                                      26
449900931102     C                   UPDATE    FRB8SF4
450000931112     C                   ENDIF
450100931102     C* STAMPA MANCA TARIFFA
450200931102     C                   EXSR      STAERR
450300931102     C*
450400931103   X1C                   ELSE
450500931102     C*
450600931102     C* N O N   E '   M A N C A   T A R I F F A
450700931102     C* SE BASE      DISTINTA VALORIZZATA AZZERO IL CAMPO IN BOLLA
450800931103    2C     *IN15         IFEQ      *ON
450900931102     C                   CLEAR                   FTDITT
451000931102     C                   CLEAR                   FTDFIT
451100931103   X2C                   ELSE
451200931102     C*
451300931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
451400020226    3C     §TAITT        IFNE      *ALL'9'
451500020214     C                   add       §TAITT        FTDITT                          BASE
451600931109     C                   ADD       §TATPE        FTDITT                          PIK/ETICH
451700931109     C                   ADD       §TATST        FTDITT                          STOP
451800020320     C                   add       §taITT        w01ITT                          BASE
451900020320     C                   add       §taTPE        w01ITT                          PIK/ETICH
452000020320     C                   add       §taTST        w01ITT                          STOP
452100020226    3C                   ENDIF
452200931103    2C                   ENDIF
452300931102     C* SE ACCESSORI DISTINTA VALORIZZATI AZZERO IL CAMPO IN BOLLA
452400931103    2C     *IN16         IFEQ      *ON
452500931102     C                   CLEAR                   FTDITA
452600931102     C                   CLEAR                   FTDFIA
452700931103   X2C                   ELSE
452800931102     C*
452900931102     C* MEMORIZZO SOLO SE NON E' *HIVAL
453000020226    3C     §TAITA        IFNE      *ALL'9'
453100931102     C                   ADD       §TAITA        W01ITA
453200020214     C                   add       §TAITA        FTDITA
453300020226    3C                   ENDIF
453400931103    2C                   ENDIF
453500931102     C*
453600931103    1C                   ENDIF
453700931102     C*
453800931102     C* SE VALORIZZATA SIA BASE CHE ACCESSORI
453900931102     C*  CLEAR SE IMPOSTATA A LIVELLO DI TESTATA
454000931102     C   15              CLEAR                   FTDITT
454100931102     C   15              CLEAR                   FTDFIT
454200931102     C   16              CLEAR                   FTDITA
454300931102     C   16              CLEAR                   FTDFIA
454400130305      *
454500931102     C                   ENDSR
454600020212     C*---------------------------------------------------------------*
454700931102     C* CONTROLLO L'ULTIMO EVENTUALE STOP DA TASSARE/SOMMARE ---------*
454800020212     C*---------------------------------------------------------------*
454900931102     C     CTRSTP        BEGSR
455000940606     C                   SETOFF                                       17
455100130305     C*
455200931102     C* S O M M A   P E S I   A   S T O P
455300931102     C     W01STP        IFNE      0
455400931102     C* RILEGGO LA SPEDIZIONE PRECEDENTE DOVE AGGIORNO I DATI
455500011105     C     W05NRR        CHAIN     fiftd000                           32
455600931102     C*
455700020122     C                   CLEAR                   FICNB2
455800931102     C                   Z-ADD     W01KFA        §TAKFA
455900931102     C                   Z-ADD     W01TVL        §TATVL
456000931102     C                   Z-ADD     W01NCL        §TANCL
456100950118     C                   Z-ADD     W01CPE        §TACPE
456200950123     C                   MOVEL     'S'           §TAPAG
456300950118     C     §TAKFA        IFEQ      0
456400950118     C                   MOVE      *BLANKS       §TAPAG
456500950118     C                   END
456600931102     C                   MOVEL     *HIVAL        §TAITA
456700931123     C     FTDCTP        IFEQ      *BLANKS
456800931123     C                   MOVEL     WALF3         FTDCTP
456900931123     C                   ENDIF
457000950127     C                   MOVEL     VI4PEP        §TAPPE                          PIK/ETICH S/N
457100021211     C                   movel     vi4FCS        §taFCS                          Car/Scarico S/N
457200020325     C                   movel     w01isola      §taISO
457300931102     C*
457400020213     C                   MOVEL     'T'           §TAtsm
457500931102     C                   EXSR      TASSAZ
457600931102     C*
457700020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
457800020326     C* i primi tre caratteri
457900020326     c  n32              if        ftdfgs = 0
458000020326     c                   movel     ftdpdr        ftdfgs
458100020326     c                   end
458200020326     C*
458300011105     C  N32              UPDATE    fiftd000
458400931102     C                   ENDIF
458500931102     C*
458600931102     C* S O M M A   P E S I   A   G I O R N A T A
458700950214     C     W02STP        IFGT      0
458800931102     C                   ADD       1             W01SNA
458900020212     C                   ADD       1             g01SNA
459000931102     C                   ENDIF
459100130305     C*
459200931102     C                   ENDSR
459300020212     C*---------------------------------------------------------------*
459400931103     C* SOMMO LE VALORIZZAZIONI CHE CI SONO NEL DETTAGLIO -----------*
459500020212     C*---------------------------------------------------------------*
459600931103     C     AGGFTD        BEGSR
459700130305     C*
459800011105     C     KFTD          SETLL     fiftd001
459900011105     C     KFTD          READE     fiftd001                               32
460000931103     C*
460100931103     C     *IN32         DOWEQ     *OFF
460200020405     C                   ADD       FTDNCL        g01CLA                          COLL GIORN
460300020405     C                   ADD       FTDPKL        g01KFA                          PESO GIORN
460400020405     C                   ADD       FTDVLU        g01TVL                          VOL  GIORN
460500020405     C     *IN19         IFEQ      *on
460600020405     C                   ADD       FTDNCE        g01CPE                          COLLI ETICH
460700020405     C                   ELSE
460800020405     C                   ADD       FTDNCL        g01CPE                          COLLI PIK.
460900020405     C                   END
461000931103     C* TESTATA
461100931103     C     *IN15         IFEQ      *ON
461200931103     C                   CLEAR                   FTDITT
461300931103     C                   CLEAR                   FTTFIT
461400931103     C                   ELSE
461500931103     C                   ADD       FTDITT        W01ITT
461600931103     C                   ENDIF
461700931103     C* DETTAGLIO
461800931103     C     *IN16         IFEQ      *ON
461900931103     C                   CLEAR                   FTDITA
462000931103     C                   CLEAR                   FTTFIA
462100931103     C                   ELSE
462200931103     C                   ADD       FTDITA        W01ITA
462300931103     C                   ENDIF
462400020326     C*
462500020326     c* se non c'è il P.O. di gestione lo prende dal padroncino
462600020326     C* i primi tre caratteri
462700020326     c                   if        ftdfgs = 0
462800020326     c                   movel     ftdpdr        ftdfgs
462900020326     c                   end
463000020326     C*
463100011105     C                   UPDATE    fiftd001
463200931103     C*
463300011105     C     KFTD          READE     fiftd001                               32
463400931103     C                   ENDDO
463500931103     C*
463600931103     C                   ENDSR
463700020212     C*---------------------------------------------------------------*
463800931103     C* STAMPA MANCA TARIFFA -----------------------------------------*
463900020212     C*---------------------------------------------------------------*
464000950116     C     STAERR        BEGSR
464100950116     C                   MOVEL     FTDRSC        W01RSB           15
464200931103     C  N18VH4TSR        IFEQ      'R'
464300931104     C                   SETON                                        19
464400931103     C                   ELSE
464500931104     C                   SETOFF                                       19
464600931103     C                   ENDIF
464700931103     C* TESTATA
464800931103     C     *INOF         IFEQ      *ON
464900950127     C     VI4DDC        ORNE      WM1DDC
465000931103     C                   EXCEPT    TES
465100931103     C                   SETOFF                                       OF
465200950127     C                   Z-ADD     VI4DDC        WM1DDC
465300931103     C                   ENDIF
465400950127     C  N18              MOVE      FTDAAS        AASPRT            2 0
465500950127     C  N18              MOVEL     FTDCAP        CAPPRT            5
465600931103     C* DETTAGLIO
465700931103     C   18              EXCEPT    DET1
465800950116     C  N18              EXCEPT    DET2
465900931103     C*
466000931103     C                   ENDSR
466100020212     C*---------------------------------------------------------------*
466200931103     C*  VEDO SE DARE MANCA TARIFFA ---------------------------------*
466300020212     C*---------------------------------------------------------------*
466400931103     C     MANTAR        BEGSR
466500020122     C                   CLEAR                   FICNB2
466600020426     C                   MOVEL     'CALCVAL'     §TApgm
466700931209     C                   MOVEL     PARSML        §TASML
466800931103     C                   MOVEL     VI4PDR        §TAPDR
466900020326     C                   MOVEL     vi4pdr        §TAfgs
467000011205     c                   movel     vi4ctd        §tadiv
467100020326     C                   MOVEL     vh4DDC        §TADDC
467200020326     C                   z-add     vi4ndc        §tandc
467300931103     C                   MOVEL     'M'           §TAFLG
467400020122     C                   CALL      'FICNB2R'
467500020122     C                   PARM                    FICNB2
467600931103     C*
467700931103     C* ERR=E DEVO DARE MANCA TARIFFA
467800931103    1C     §TAERR        IFEQ      'E'
467900931202     C                   SETON                                        184290
468000931103     C                   EXSR      STAERR
468100931202     C                   SETOFF                                       18
468200931103     C* SEGNALO LA DISTINTA
468300931103     C                   MOVEL     'E'           VI4SCE
468400011109     c     vh4tpr        comp      2                                      26
468500931103     C                   UPDATE    FRB8SF4
468600931103     C*
468700931103   X1C                   ELSE
468800931103     C* SOMMO LE SINGOLE VALORIZZAZIONI EVENTUALMENTE FATTE IN TESTATA
468900931103     C*  SENZA DARE ERRORE
469000931103     C                   EXSR      AGGFTD
469100931103    1C                   ENDIF
469200130305      *
469300931103     C                   ENDSR
469400020212     C*---------------------------------------------------------------*
469500931103     OPRTF198   E            TES              02
469600931102     O                       STA(1)              66
469700931102     O                       STA(2)             132
469800931103     O                       STA(3)             198
469900931102     O                       RSUT                20
470000931102     O                       SIMFEL              24
470100950127     O                       VI4DDC              99 '  /  /  '
470200931103     O                       UDATE              160 '  .  .  '
470300931103     O                       PAGE          Z    198
470400931103     O          E            TES              03
470500931103     O                       UTIME              160 '  :  :  '
470600931103     O          E            TES              04
470700931103     O                       STA(4)              66
470800931103     O                       STA(7)             132
470900931103     O                       STA(10)            198
471000931103     O          E            TES            1 05
471100931103     O                       STA(5)              66
471200931103     O                       STA(8)             132
471300931103     O                       STA(11)            198
471400931103     O          E            DET1           1
471500931103     O                       STA(13)             66
471600931103     O                       STA(14)            132
471700931103     O                       STA(15)            198
471800931103     O                       VI4PDR               8
471900931103     O                                              '-'
472000931103     O                       VH4DPD
472100950120     O                       VH4TSH            +  1
472200931110     O                       VI4NDC        Z   +  2
472300931110     O                       W01ERR             170
472400931103     O          E            DET2           1
472500931103     O                       STA(13)             66
472600931103     O                       STA(14)            132
472700931103     O                       STA(15)            198
472800931103     O                       VI4PDR               8
472900931103     O                                              '-'
473000931103     O                       VH4DPD
473100950120     O                       VH4TSH            +  1
473200931110     O                       VI4NDC        Z   +  2
473300950127     O                       AASPRT        Z   +  1
473400931103     O                       FTDLNP        Z   +  1
473500931103     O                       FTDNRS        Z   +  1
473600931103     O                       FTDNSP        Z   +  1
473700931123     O              N19      W01RSB            +  1
473800931123     O              N19      FTDFIN            +  2
473900931123     O               19      W01RSB            +  2
474000931123     O               19      FTDFIN            +  2
474100950120     O                       FTDPKL        4   +  1
474200931103     O                       FTDNCL        Z   +  1
474300950116     O                       FTDVLU        4   +  1
474400950120     O              N19      CAPPRT            +  2
474500950120     O               19      CAPPRT            +  3
474600931110     O                       §TACTR            +  3
474700931110     O                       SER(1)            +  3
474800931103**
474900931028NON ESISTONO DISTINTE/RITIRI da visualizzare per la selezione effettuata
475000030519Inserire Tariffe Valide per Distinte di Consegna -> Mancano le Tariffe
475100030519Inserire Tariffe Valide per Distinte di Ritiro   -> Mancano le Tariffe
475200931103**
475300931103XXXXXXXXXXXXXXXXXXXX/XXX                ***  CONTROLLO  TASSAZIONE   1
475400020322  AUTOTRASP.  DEL GIORNO XX/XX/XX  ***                     FICNB1R   2
475500931103                    XX.XX.XX                             PAG. XXXX   3
475600931110'      C O D I C E      '   TIPO   ' NUMERO '  SPEDIZIONE     '      4
475700020322'   AUTOTRASPORTATORE   ' PRESTAZ. 'DISTINTA'                 'RAG   5
475800931103'XXXXXXX-XXXXXXXXXXXXXXX'XXXXXXXXXX' XXXXXXX'XX XXX XX XXXXXXX'XXX   6
475900931110DESTINATARIO   '     MITTENTE      ' PESO   'COLLI'VOLUME'  CAP  '
476000931110.SOCIALE     IN' RAG.SOCIALE     AN'        '     '      ' DEST. '
476100950120XXXXXXXXXXXX  X' XXXXXXXXXXXXXXX  X'XXXXXX,X'XXXXX'XX,XXX' XXXXX '
476200931110  CAP  ' COD '     A N O M A L I A      '
476300931110 MITT. ' TAR '                          '
476400931110 XXXXX ' XXX ' XXXXXXXXXXXXXXXXXXXXXXXXX'
476500931110'                       '          '        '                 '     13
476600931110               '                   '        '     '      '       '  14
476700931110       '     '                          '                           15
476800931103**
476900931103MANCA TARIFFA
477000020322L'AUTOTRASP.NON HA TARIFFE
477100020328**
477200020328RGZPFM FILE(FIFTDW0F)
