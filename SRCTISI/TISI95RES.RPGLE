000100970627     H*------------------------------------------------------------------------*
000200970515     H* CONTROLLO CAP/LOCALITA                                                 *
000300061207     H* NB: Compilare con la versione più vecchia V5R1M0                       *
000400020211     H* NB: Ricordarsi di non modificare mai il TISI95R ma di creare sempre un *
000500020211     H*     TISI95RNEW perchè i clienti scaricano e utilizzano il sorgente     *
000600970424     H*------------------------------------------------------------------------*
000700970424     H DECEDIT('0,') DATEDIT(*DMY.)
000800970424     F*------------------------------------------------------------------------*
000900970424     F* DATA BASE
001000970424     F*------------------------------------------------------------------------*
001100970515     FAZCPC01L  IF   E           K DISK
001200970515     FAZCPL01L  IF   E           K DISK
001300970605     FAZCPL02L  IF   E           K DISK
001400970605     F                                     RENAME(AZCPL000:AZCPL002)
001500970605     FAZCPS04L  IF   E           K DISK
001600970605     FAZCPS02L  IF   E           K DISK
001700970605     F                                     RENAME(AZCPS000:AZCPS002)
001800970627     FAZCPS01L  IF   E           K DISK
001900970627     F                                     RENAME(AZCPS000:AZCPS001)
002000970519     FAZCPT01L  IF   E           K DISK
002100970519     FAZCPE01L  IF   E           K DISK
002200970523     FAZCPR02L  IF   E           K DISK
002300930323     FTABEL00F  IF   E           K DISK
002400970424     D*------------------------------------------------------------------------*
002500970424     D* SCHIERE
002600970424     D*------------------------------------------------------------------------*
002700970424     D*-------------------
002800970424     D* MESSAGGI
002900970424     D*-------------------
003000970408     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970424     D*-------------------
003200970515     D* SCHIERE PER CONTROLLO FORMALE
003300970424     D*-------------------
003400970515     D CCP             S              1    DIM(9)                               CARATT.CONTROLLO
003500970515     D CAP             S              1    DIM(9)                               CAP DA CONTROLLAR
003600970515     D CAF             S              1    DIM(9)                               CAP FITTIZIO
003700150203     D CAPN            S              1    DIM(9)                               CAP NORMALIZZATO
003800040528     D LET             S              1    DIM(32) CTDATA PERRCD(32)            LETTERE ALFABETO
003900040528     D LEM             S              1    DIM(32) CTDATA PERRCD(32)            LETTERE ALFAB. MINUS
004000970515     D NUM             S              1    DIM(10) CTDATA PERRCD(10)            NUMERI
004100971104     D*-------------------
004200971104     D* SCHIERE PER OTTIMIZZAZIONE LOCALITA'
004300971104     D*-------------------
004400971104     D LOI             S              1    DIM(35)                              LOCALITA' INPUT DA O
004500971104     D LOO             S              1    DIM(35)                              LOCALITA' OUTPUT OTT
004600970424     D*------------------------------------------------------------------------*
004700970424     D* INPUT
004800970424     D*------------------------------------------------------------------------*
004900970424     D*-------------------
005000970424     D* DS PASSAGGIO PARAMETRI
005100970424     D*-------------------
005200970516     D TISI95        E DS                  EXTNAME(TISI95DS)
005300970424     D*-------------------
005400970516     D* DS NAZIONI
005500970424     D*-------------------
005600970516     D DS15          E DS
005700091002     D  §15POE       E                     EXTFLD(§15DPDPOE)
005800150204     D  §15PRE       E                     EXTFLD(§15PREF)
005900970519     D*-------------------
006000970519     D* DS PROVINCIA
006100970519     D*-------------------
006200970519     D DSPR          E DS
006300970424     D*-------------------
006400970424     D* DS PER MESSAGGI
006500970424     D*-------------------
006600970409     D                 DS
006700970409     D  DSMSG                  1     78
006800970409     D  DSERR                 75     78
006900970516     D*-------------------
007000970516     D* DS SCOMPOSIZIONE DATA IN ANNO/MESE/GIONRO
007100970516     D*-------------------
007200970516     D DS§DAT          DS
007300970516     D  DSSEC                  1      2  0
007400970516     D  DSANN                  3      4  0
007500970516     D  DSAAS                  1      4  0
007600970516     D  DSMES                  5      6  0
007700970516     D  DSGIO                  7      8  0
007800970516     D  DSMGS                  5      8  0
007900970516     D  DSDAT6                 3      8  0
008000970516     D  DSDAT8                 1      8  0
008100970516     D DS§TIM          DS
008200970516     D  DTORA                  1      2  0
008300970516     D  DTMIN                  3      4  0
008400970516     D  DTSCN                  5      6  0
008500970516     D  DTGIO                  7      8  0
008600970516     D  DTMES                  9     10  0
008700970516     D  DTSEC                 11     12  0
008800970516     D  DTANN                 13     14  0
008900970516     D  DTORA6                 1      6  0
009000970516     D  DTTIME                 1     14  0
009100000419     D*-------------------
009200000419     D* DS REPERIMENTO INSTRADAMENTO POSTE
009300000419     D*-------------------
009400000419     D TISI98        E DS                  EXTNAME(TISI98DS)
009500000419     D*-------------------
009600000419     D* DS LETTURA O95RDS -DATI INSTRADAMENTO POSTE-
009700000419     D*-------------------
009800000419     D DRDS          E DS
009900970424     D*-------------------
010000970424     D* ARCHITETTURA
010100970424     D*-------------------
010200970424     D KPJBA         E DS
010300970424     C*------------------------------------------------------------------------*
010400970424     C* MAIN LINE
010500970424     C*------------------------------------------------------------------------*
010600970516     C*
010700970424     C* OPERAZIONI INIZIALI
010800930304     C                   EXSR      RUTINZ
010900930528     C*
011000970519IF  1C     $ERR          IFEQ      'N'
011100930602     C*
011200970424     C* SE LANCIATO CON TIPO LANCIO='C' -CHIUDE FILE-, VA A FINE PROGRAMMA
011300970516IF  2C     I95TLA        IFNE      'C'
011400970604     C*
011500970604     C* CONTROLLI DI TIPO GENERALE
011600970604     C                   EXSR      CHKGEN
011700970604     C*
011800970604     C* SELEZIONI PER OGNI TIPO CONTROLLO
011900970604IF  3C     $ERR          IFEQ      'N'
012000970604SL  4C                   SELECT
012100970604     C     I95TCN        WHENEQ    '1'
012200970604     C                   EXSR      TCN1                                         *CONTR.FORMALE CAP
012300970604     C     I95TCN        WHENEQ    '3'
012400970604     C                   EXSR      TCN3                                         *CONTR.CAP/LOC
012500970604     C     I95TCN        WHENEQ    '4'
012600970604     C                   EXSR      TCN4                                         *CONTR.LOC/CAP
012700970627     C     I95TCN        WHENEQ    '7'
012800970630     C                   EXSR      TCN7                                         *CONTR.X INS.BOLLE
012900970604E   4C                   ENDSL
013000970604E   3C                   ENDIF
013100970516     C*
013200970516     C* SE LANCIATO CON TIPO CONTROLLO = 1, VA A FINE PGM
013300970516IF  3C     I95TCN        IFNE      '1'
013400970605     C*
013500970605     C* PORTA OUT CAP O LOC NELL'OUT GENERALE
013600970605     C     $ERR          IFEQ      'N'
013700970605     C                   EXSR      OUTGEN
013800970630     C                   EXSR      GESLID
013900970605     C                   ENDIF
014000970519     C*
014100980602     C* SE RICHIESTO RECUPERO TEMPI CONSEGNA
014200970519     C     $ERR          IFEQ      'N'
014300980602     C     I95FRT        IFEQ      'S'                                          *RICH.TEMPI CONS.
014400970519     C     O95TTC        ANDNE     *ZEROS
014500970519     C                   EXSR      CHKCPT
014600970519     C                   ENDIF
014700970519     C                   ENDIF
014800970519     C*
014900980602     C* SE RICHIESTO E IL CAP HA DELLE ECCEZIONI
015000970606     C* E SOLO SE TROVATO LIVELLO DATI CAP O CAP FITT.
015100970520     C* RECUPERO ECCEZIONI LINEE/ZONE
015200970519     C     $ERR          IFEQ      'N'
015300980602     C     I95FRE        IFEQ      'S'                                          *RICH.CONTR.ECCEZ.
015400970530     C     O95ESE        ANDEQ     'S'                                          *ESIST.DIFF. X LNP
015500970606     C     O95LID        IFEQ      '1'                                          *LIV DATI CAP FITT.
015600970606     C     O95LID        OREQ      '2'                                          *LIV DATI CAP
015700970616     C     O95LID        OREQ      '3'                                          *LIV DATI LOC
015800970616     C     O95LIV        ANDEQ     'C'                                          *E LIV GEST. CAP
015900970722     C     O95LID        OREQ      '4'                                          *LIV DATI SIN
016000970722     C     O95LIV        ANDEQ     'C'                                          *E LIV GEST. CAP
016100970519     C                   EXSR      CHKCPE
016200970606     C                   ENDIF
016300970519     C                   ENDIF
016400970519     C                   ENDIF
016500970616     C*
016600970616     C* ERRORE GENERICO
016700970616     C     O95ERR        IFEQ      *BLANKS
016800970616     C     O95CAP        ANDEQ     *BLANKS
016900970616     C                   MOVEL     MSG(20)       $MSG                           *ERR. GENERICO
017000970616     C                   EXSR      ERRORE
017100970616     C                   ENDIF
017200000419     C*
017300000419     C* SE IL TIPO SERVIZIO = "P", REPERISCE L'INSTRADAMENTO PER LE POSTE
017400000419IF  4C     I95TSP        IFEQ      'P'
017500000419     C                   EXSR      REPRDS
017600000419E   4C                   ENDIF
017700970516E   3C                   ENDIF
017800021028     C*
017900021028     C* SE LANCIO PER DPD, REPERISCE I DATI DI INTSTRADAMENTO DALLA TABELLA NAZIONI
018000130906     C     $ERR          IFEQ      'N'
018100021028IF  3C     I95FI1        IFEQ      'D'
018200021028     C                   EXSR      OUTDPD
018300021028E   3C                   ENDIF
018400130906     C                   ENDIF
018500020211     C*
018600020211     C* SE LANCIO PER FEDEX, REPERISCE I DATI DI INTSTRADAMENTO DALLA TABELLA NAZIONI
018700130906     C     $ERR          IFEQ      'N'
018800020211IF  3C     I95FI1        IFEQ      'S'
018900020211     C                   EXSR      OUTFED
019000020211E   3C                   ENDIF
019100130906     C                   ENDIF
019200020211     C*
019300970424E   2C                   ENDIF
019400970424E   1C                   ENDIF
019500930323     C*
019600970424     C* OPERAZIONI DI CHIUSURA
019700970630     C                   MOVEL     J95TCN        I95TCN
019800970630     C                   MOVEL     J95PRV        I95PRV
019900970516     C     I95TLA        IFEQ      'C'
020000000419     C                   EXSR      FINSR                                        *OPERAZIONI FINALI
020100930323     C                   SETON                                        LR
020200970516     C                   ELSE
020300930324     C                   SETON                                        RT
020400970516     C                   ENDIF
020500970516     C*------------------------------------------------------------------------*
020600970516     C* CHKGEN - CONTROLLI DI TIPO GENERALE
020700970516     C*------------------------------------------------------------------------*
020800970516     C     CHKGEN        BEGSR
020900970630     C*
021000970630     C* SE LANCIATO CON CONTROLLO '7' MA SENZA IL CAP TRSFORMO IL LANCIO IN '4'
021100970630     C     I95TCN        IFEQ      '7'
021200970630     C     I95CAP        ANDEQ     *BLANKS
021300970630     C                   MOVEL     '4'           I95TCN
021400970630     C                   ENDIF
021500980602     C*
021600980602     C* SE RICHIESTI TEMPI DI CONSEGNA E NON INSERITO IN INPUT IL TIPO SERVIZIO,ERRORE
021700980602     C     I95FRT        IFEQ      'S'                                          *RICH.TEMPI CONS.
021800980602     C     I95TSP        ANDEQ     *BLANKS                                      *TIPO SERVIZIO
021900980602     C                   MOVEL     MSG(3)        $MSG                           *RICH.TEMPI SENZA TS
022000980602     C                   EXSR      ERRORE
022100980602     C                   ENDIF
022200980602     C*
022300980602     C* SE RICHIESTO CONTROLLO ECCEZIONI LINEE/ZONE E NON INSERITO AS PARTENZA,ERRORE
022400980602     C     $ERR          IFEQ      'N'
022500980602     C     I95FRE        IFEQ      'S'                                          *RICH.CONTR.ECC.
022600980602     C     I95TFP        ANDEQ     *ZEROS                                       *AS PARTENZA
022700980602     C                   MOVEL     MSG(4)        $MSG                           *RICH.ECCEZ.SENZA TF
022800980602     C                   EXSR      ERRORE
022900980602     C                   ENDIF
023000980602     C                   ENDIF
023100970516     C*
023200970516     C* SE DATA RIFERIMENTO = 0, USO DATA DI OGGI
023300970516     C     I95DAT        IFEQ      *ZEROS
023400970516     C                   TIME                    DTTIME
023500970516     C                   Z-ADD     DTANN         DSANN
023600970516     C                   Z-ADD     DTMES         DSMES
023700970516     C                   Z-ADD     DTGIO         DSGIO
023800970516     C                   Z-ADD     DTSEC         DSSEC
023900970516     C                   Z-ADD     DSDAT8        I95DAT
024000970516     C                   ENDIF
024100970604     C*
024200970604     C* CONTROLLA NAZIONE
024300970604     C     $ERR          IFEQ      'N'
024400970604     C                   EXSR      CHKNAR
024500970604     C                   ENDIF
024600020212     C*
024700020212     C* SE NAZIONE SENZA CAPPARIO TRASFORMA IL TIPO CONTROLLO IN '1'
024800020212     C     §15CPP        IFEQ      'N'
024900020212     C                   MOVEL     '1'           I95TCN
025000020212     C                   ENDIF
025100970605     C*
025200970605     C* CONTROLLA PROVINCIA
025300970630IF  1C     $ERR          IFEQ      'N'
025400971024IF  2C     I95TCN        IFEQ      '7'
025500970630IF  3C     §15CPR        IFEQ      'S'                                          *NAZ.CON PRV DA CONT
025600970605     C                   EXSR      CHKPRV
025700970630X   3C                   ELSE
025800970630     C                   MOVEL     *BLANKS       I95PRV
025900970630E   3C                   ENDIF
026000971024X   2C                   ELSE
026100971024IF  3C     I95PRV        IFNE      *BLANKS
026200971024     C     §15CPR        ANDEQ     'S'                                          *NAZ.CON PRV DA CONT
026300971024     C                   EXSR      CHKPRV
026400971024E   3C                   ENDIF
026500971024E   2C                   ENDIF
026600970630E   1C                   ENDIF
026700970523     C*
026800970627     C* RECUPERO VERSIONE DALLA DATA DI RIFERIMENTO
026900970523IF  1C     $ERR          IFEQ      'N'
027000970523     C                   EXSR      CHKVER
027100970523E   1C                   ENDIF
027200971105     C*
027300971105     C* OTTIMIZZA LA LOCALITA' (TOGLIE SPAZI,CARATTERI STRANI..)
027400000727IF  1C     $ERR          IFEQ      'N'                                          *LANCIO X SCRIVERE J
027500000727     C                   MOVEL     'J'           TLAN              1
027600971105     C                   EXSR      OTTLOC
027700971105E   1C                   ENDIF
027800000727IF  1C     $ERR          IFEQ      'N'                                          *LANCIO X SCRIVERE Y
027900000727     C                   MOVEL     'Y'           TLAN              1
028000000727     C                   EXSR      OTTLOC
028100000727E   1C                   ENDIF
028200970516     C*
028300970516     C                   ENDSR
028400970604     C*------------------------------------------------------------------------*
028500970604     C* TCN1   - GESTIONE TIPO CONTROLLO 1 (CONTR.FORMALE CAP)
028600970604     C*------------------------------------------------------------------------*
028700970604     C     TCN1          BEGSR
028800970604     C*
028900970604     C* CONTROLLO FORMALE DEL CAP (SE IN INPUT)
029000970604     C     $ERR          IFEQ      'N'
029100970604     C                   EXSR      CHKFOR                                       *CONTROLLO FORMALE
029200150203     C                   EXSR      CHKCFC                                       *CONTROLLO FORMATO
029300970604     C                   ENDIF
029400970604     C*
029500970604     C                   ENDSR
029600970604     C*------------------------------------------------------------------------*
029700970604     C* TCN3   - GESTIONE TIPO CONTROLLO 3 (CONTR.CAP/LOC)
029800970604     C*------------------------------------------------------------------------*
029900970604     C     TCN3          BEGSR
030000970605     C*
030100970703     C* RECUPERA IL CAP FITTIZIO DEL CAP IN INPUT
030200970710     C   99
030300970710     CANN99              EXSR      CERFIT
030400970710     C                   EXSR      CHKFOR
030500150203     C                   EXSR      CHKCFC
030600970604     C*
030700970627IF  0C     $ERR          IFEQ      'N'
030800970605     C*
030900970604     C* PRIMO TENTATIVO CON CAP COMPLETO
031000970703     C                   Z-ADD     *ZEROS        AFFLS
031100970605     C                   MOVEL     I95CAP        WCAP
031200970604     C                   EXSR      LETCAP
031300970604     C*
031400970604IF  1C     TROCAP        IFEQ      'S'                                          *CAP TROVATO
031500970604     C*
031600970604     C                   EXSR      CAPOUT                                       *DATI CAPPARIO IN OU
031700010713     C* Effettuo 1° tentativo con località di input non "normalizzata"
031800010713     C     I95LOC        IFNE      *BLANKS
031900061030     C* Cerco localita/sinonimi solo se nazione Italia
032000061030IFNARC     I95NAR        IFEQ      *BLANKS
032100010713     C                   MOVEL     CPCCAP        WCAP
032200010713     C                   MOVEL     I95LOC        WLOC
032300010713     C                   EXSR      LETLOC                                       *CERCA LOC X CAP/LOC
032400010713     C     TROLOC        IFEQ      'S'
032500010713     C                   EXSR      LOCOUT                                       *DATI LOC -> OUT
032600010713     C                   Z-ADD     1             AFFLS
032700010713     C                   ELSE                                                   *LOC. NON TROV.
032800010713     C                   MOVEL     WLOC          WSIN
032900010713     C                   EXSR      CERSIN                                       *PROVO CON SINONIMI
033000010713IF  4C     NUMSIN        IFEQ      1                                            *NUM.SIN.TROVATI
033100010713     C                   EXSR      SINOUT                                       *DATI SIN. -> OUT
033200010713     C                   MOVEL     CPSLOC        WLOC                           *LEGGE LOC. DEL SIN.
033300010713     C                   EXSR      LETLOC                                       *CHAIN LOC.
033400010713     C                   EXSR      LOCOUT                                       *DATI LOC -> OUT
033500010713     C                   Z-ADD     1             AFFLS
033600010713X   4C                   ELSE                                                   *SIN NON TROV
033700010713IF  5C     NUMSIN        IFEQ      0                                            *NUM.SIN.TROVATI
033800010713     C                   MOVEL     MSG(13)       $MSG                           *LOC NON TROV. NEL C
033900010713     C                   EXSR      MESSAG
034000010713X   5C                   ELSE                                                   *NUM.SIN. > 1
034100010713     C                   MOVEL     MSG(17)       $MSG                           *LOC ESIST. MA NON U
034200010713     C                   EXSR      MESSAG
034300010713E   5C                   ENDIF
034400010713E   4C                   ENDIF
034500010713E   3C                   ENDIF
034600061030     C* SE ESTERO ASSUMO SEMPRE X BUONA LA LOCALITA
034700061030  NARC                   ELSE
034800061030  NARC                   Z-ADD     1             AFFLS
034900061030E NARC                   ENDIF
035000010713E   2C                   ENDIF
035100010713     C*
035200010713     C* Se a questo punto non ho ottenuto un risultato di OK =>
035300010713     C     TROLOC        IFNE      'S'
035400010713     C     NUMSIN        ANDNE     1
035500010713     C* => Effettuo 2° tentativo con località di input "normalizzata"
035600971105IF  2C     J95LOC        IFNE      *BLANKS                                      *LOC. NON IN INPUT
035700061030     C* Cerco localita/sinonimi solo se nazione Italia
035800061030IFNARC     I95NAR        IFEQ      *BLANKS
035900010713     C                   MOVEL     CPCCAP        WCAP
036000971105     C                   MOVEL     J95LOC        WLOC
036100970627     C                   EXSR      LETLOC                                       *CERCA LOC X CAP/LOC
036200970627IF  3C     TROLOC        IFEQ      'S'                                          *LOC. TROV.
036300970627     C                   EXSR      LOCOUT                                       *DATI LOC -> OUT
036400970703     C                   Z-ADD     1             AFFLS
036500970627X   3C                   ELSE                                                   *LOC. NON TROV.
036600010713     C                   MOVEL     WLOC          WSIN
036700970627     C                   EXSR      CERSIN                                       *PROVO CON SINONIMI
036800970627IF  4C     NUMSIN        IFEQ      1                                            *NUM.SIN.TROVATI
036900970627     C                   EXSR      SINOUT                                       *DATI SIN. -> OUT
037000970627     C                   MOVEL     CPSLOC        WLOC                           *LEGGE LOC. DEL SIN.
037100970627     C                   EXSR      LETLOC                                       *CHAIN LOC.
037200970627     C                   EXSR      LOCOUT                                       *DATI LOC -> OUT
037300970703     C                   Z-ADD     1             AFFLS
037400970703X   4C                   ELSE                                                   *SIN NON TROV
037500971125IF  5C     NUMSIN        IFEQ      0                                            *NUM.SIN.TROVATI
037600970605     C                   MOVEL     MSG(13)       $MSG                           *LOC NON TROV. NEL C
037700970605     C                   EXSR      MESSAG
037800971125X   5C                   ELSE                                                   *NUM.SIN. > 1
037900971125     C                   MOVEL     MSG(17)       $MSG                           *LOC ESIST. MA NON U
038000971125     C                   EXSR      MESSAG
038100971125E   5C                   ENDIF
038200970627E   4C                   ENDIF
038300970627E   3C                   ENDIF
038400061030     C* SE ESTERO ASSUMO SEMPRE X BUONA LA LOCALITA
038500061030  NARC                   ELSE
038600061030  NARC                   Z-ADD     1             AFFLS
038700061030E NARC                   ENDIF
038800970627E   2C                   ENDIF
038900010713     C                   ENDIF
039000970604     C*
039100970604X   1C                   ELSE                                                   *CAP NON TROVATO
039200970604     C*
039300970604     C* FACCIO SECONDO TENTATIVO CON CAP FITTIZIO
039400970604     C                   MOVEL     WCAPFI        WCAP
039500970604     C                   EXSR      LETCAP
039600970604IF  2C     TROCAP        IFEQ      'S'                                          *TROVATO CAP FITTIZI
039700970604     C                   EXSR      CAPOUT                                       *DATI CAPPARIO IN OU
039800970604X   2C                   ELSE                                                   *NON TROV.CAP FITTIZ
039900970604     C                   MOVEL     MSG(8)        $MSG                           *CAP INESISTENTE
040000970604     C                   EXSR      ERRORE
040100970604E   2C                   ENDIF
040200970604     C*
040300970604E   1C                   ENDIF
040400970604     C*
040500970604     C* SE IN INPUT ANCHE LA PROVINCIA CONTROLLA CONGRUENZA CON PRV DEL CAP
040600970627IF  1C     $ERR          IFEQ      'N'
040700970627     C     O95CAP        ANDNE     *BLANKS                                      *CAP TROVATO
040800970630     C     §15CPR        ANDEQ     'S'                                          *NAZ.CON PRV DA CONT
040900970627     C     I95PRV        ANDNE     *BLANKS
041000970627IF  2C     I95PRV        IFEQ      O95PRV
041100970618     C     I95PRV        OREQ      O95PRA
041200970627     C     O95PRA        ANDNE     *BLANKS
041300970703     C                   ADD       1             AFFLS
041400970627X   2C                   ELSE
041500970604     C                   MOVEL     MSG(14)       $MSG                           *PRV IN <> PRV CAP
041600051130     C***                  EXSR MESSAG
041700051130     C                   EXSR      ERRORE
041800970627E   2C                   ENDIF
041900970627E   1C                   ENDIF
042000970703     C*
042100970703     C* CONTROLLA LIVELLO AFFIDABILITA' RISCONTRATO
042200970703     C     $ERR          IFEQ      'N'
042300970703     C                   EXSR      GESLIA
042400970703     C                   ENDIF
042500970604     C*
042600970627E   0C                   ENDIF
042700970605     C*
042800970604     C                   ENDSR
042900970604     C*------------------------------------------------------------------------*
043000970604     C* TCN4   - GESTIONE TIPO CONTROLLO 4 (CONTR.LOC/CAP)
043100970604     C*------------------------------------------------------------------------*
043200970604     C     TCN4          BEGSR
043300061030     C*
043400061030     C* QUESTO CONTROLLO HA SENSO SOLO X NAZIONE DESTINO ITALIA
043500061030     C     I95NAR        IFEQ      *BLANKS                                      * SOLO NAZ ITALIA
043600970605     C*
043700970627     C* DEVE ESISTERE UNA LOCALITA' IN INPUT
043800971105     C     J95LOC        IFEQ      *BLANKS
043900970605     C                   MOVEL     MSG(18)       $MSG                           *LOC. OBBLIGATORIA
044000970605     C                   EXSR      ERRORE
044100970605     C                   ENDIF
044200970604     C*
044300010713     C* ESEGUO RICERCA LOCALITA/SINONIMO
044400970605     C     $ERR          IFEQ      'N'
044500010713     C* Effettuo 1° tentativo con località di input non "normalizzata"
044600010713     C                   MOVEL     I95LOC        WLOC
044700010713     C                   EXSR      CERLS
044800010713     C* SE HO TROVATO UNA SOLA LOC/SIN RECUPERO CAP E LOC
044900010713     C     NUMLS         IFEQ      1
045000010713 xxx C                   MOVEL     WOCAP         WCAP
045100010713     C                   EXSR      LETCAP
045200010713     C                   EXSR      CAPOUT
045300010713     C                   MOVEL     WOLOC         WLOC
045400010713     C                   EXSR      LETLOC
045500010713     C                   EXSR      LOCOUT
045600010713     C     SINLOC        IFEQ      'S'                                          *TROV. SINONIMO
045700010713     C                   MOVEL     WOSIN         WSIN
045800010713     C                   EXSR      LETSIN
045900010713     C                   EXSR      SINOUT
046000010713     C                   ENDIF
046100010713     C                   ENDIF
046200010713     C*
046300010713     C* Se a questo punto non ho ottenuto un risultato di OK =>
046400010713     C     NUMLS         IFNE      1
046500010713     C     SINLOC        ANDNE     'S'
046600010713     C* => Effettuo 2° tentativo con località di input "normalizzata"
046700971105     C                   MOVEL     J95LOC        WLOC
046800970627     C                   EXSR      CERLS
046900970627     C* SE HO TROVATO UNA SOLA LOC/SIN RECUPERO CAP E LOC
047000970627     C     NUMLS         IFEQ      1
047100970627     C                   MOVEL     WOCAP         WCAP
047200970627     C                   EXSR      LETCAP
047300970627     C                   EXSR      CAPOUT
047400970627     C                   MOVEL     WOLOC         WLOC
047500970627     C                   EXSR      LETLOC
047600970627     C                   EXSR      LOCOUT
047700970627     C     SINLOC        IFEQ      'S'                                          *TROV. SINONIMO
047800970627     C                   MOVEL     WOSIN         WSIN
047900970627     C                   EXSR      LETSIN
048000970627     C                   EXSR      SINOUT
048100970627     C                   ENDIF
048200970627     C                   ENDIF
048300970605     C                   ENDIF
048400010713     C                   ENDIF
048500970605     C*
048600970605     C* DEVE ESISTERE LA LOCALITA'
048700970605     C     $ERR          IFEQ      'N'
048800970630     C     I95TCN        ANDEQ     '4'
048900970627     C     NUMLS         IFGT      1
049000970606     C                   MOVEL     MSG(17)       $MSG                           *LOC.ESIT. MA NON UN
049100970606     C                   EXSR      ERRORE
049200970627     C                   ENDIF
049300970627     C     NUMLS         IFEQ      *ZEROS
049400970605     C                   MOVEL     MSG(11)       $MSG                           *LOC. INESISTENTE
049500970605     C                   EXSR      ERRORE
049600970605     C                   ENDIF
049700970605     C                   ENDIF
049800061030     C*
049900061030     C                   ENDIF                                                  * SOLO NAZ ITALIA
050000970616     C*
050100970618     C* CONTROLLO SE PROVINCIA CONGRUENTE CON LOCALITA
050200970616     C     $ERR          IFEQ      'N'
050300970627     C     O95CAP        IFNE      *BLANKS
050400970630     C     §15CPR        ANDEQ     'S'                                          *NAZ.CON PRV DA CONT
050500970627     C     I95PRV        ANDNE     *BLANKS
050600970627IF  2C     I95PRV        IFEQ      O95PRV
050700970627     C     I95PRV        OREQ      O95PRA
050800970627     C     O95PRA        ANDNE     *BLANKS
050900970616     C                   ELSE
051000970618     C                   MOVEL     MSG(9)        $MSG                           *PRV INPUT <> PRV LO
051100970616     C                   EXSR      MESSAG
051200970616     C                   ENDIF
051300970616     C                   ENDIF
051400970616     C                   ENDIF
051500970703     C*
051600970703     C* CONTROLLA LIVELLO AFFIDABILITA' RISCONTRATO
051700970703     C     $ERR          IFEQ      'N'
051800970703     C                   EXSR      GESLIA
051900970703     C                   ENDIF
052000970604     C*
052100970604     C                   ENDSR
052200970627     C*------------------------------------------------------------------------*
052300970627     C* TCN7   - GESTIONE TIPO CONTROLLO 7 (CONTR.X INS.BOLLE)
052400970627     C*------------------------------------------------------------------------*
052500970627     C     TCN7          BEGSR
052600970627     C*
052700970627     C* PRIMO TENTATIVO CON CAP/LOC
052800970627     C                   EXSR      TCN3
052900970627     C*
053000970627IF  1C     $ERR          IFEQ      'N'
053100970627IF  2C     O95LIA        IFNE      '3'
053200970630     C                   MOVEL     *BLANKS       O95LID
053300970630     C                   MOVEL     *BLANKS       O95MSG
053400970627     C                   MOVEL     I95CAP        DEPCAP            9
053500970627     C                   MOVEL     *BLANKS       I95CAP
053600970627     C                   EXSR      AZZCAP
053700970627     C                   EXSR      AZZLOC
053800970627     C                   EXSR      AZZSIN
053900970627     C                   EXSR      TCN4
054000970630     C                   EXSR      GESLID
054100970630     C                   MOVEL     DEPCAP        I95CAP
054200970627IF  3C     $ERR          IFEQ      'N'
054300970627IF  4C     O95LID        IFEQ      '3'
054400970627     C     O95LIA        ANDEQ     '2'
054500970630IF  4C     O95LID        OREQ      '4'
054600970630     C     O95LIA        ANDEQ     '2'
054700970627X   4C                   ELSE
054800970630     C                   MOVEL     *BLANKS       O95MSG
054900970627     C                   EXSR      AZZCAP
055000970627     C                   EXSR      AZZLOC
055100970627     C                   EXSR      AZZSIN
055200970627     C                   EXSR      TCN3
055300970627E   4C                   ENDIF
055400970627E   3C                   ENDIF
055500970627E   2C                   ENDIF
055600970627E   1C                   ENDIF
055700970627     C*
055800970627     C                   ENDSR
055900970523     C*------------------------------------------------------------------------*
056000970523     C* CHKVER - RECUPERA LA VERSIONE DALLA DATA RIFERIMENTO
056100970523     C*------------------------------------------------------------------------*
056200970523     C     CHKVER        BEGSR
056300970523     C*
056400970523     C                   MOVEL     'N'           $ERR
056500970523     C                   MOVEL     *OFF          *IN99
056600970523     C                   MOVEL     I95DAT        CPRDDE
056700970523     C     KCPR          SETGT     AZCPR02L
056800970530     C                   READP     AZCPR02L                               99
056900970523IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
057000970523     C     CPRATB        ORNE      *BLANKS                                      *ANNULLATO
057100970523     C     CPRDSC        ORLT      I95DAT                                       *GIA' SCADUTO
057200970523     C                   MOVEL     MSG(16)       $MSG                           *VERSIONE NON TROVAT
057300970523     C                   EXSR      ERRORE
057400970523X   1C                   ELSE                                                   *VERS.TROVATA
057500970523     C                   EXSR      VEROUT
057600970523E   1C                   ENDIF
057700970523     C*
057800970523     C                   ENDSR
057900970523     C*------------------------------------------------------------------------*
058000970523     C*   VEROUT - PORTA CAMPI DA FILE VERSIONI A DS OUTPUT VERSIONI
058100970523     C*------------------------------------------------------------------------*
058200970523     C     VEROUT        BEGSR
058300970523     C*
058400970530     C                   Z-ADD     CPRVER        O95VER
058500970530     C                   Z-ADD     CPRDDE        O95RDE
058600970530     C                   Z-ADD     CPRDSC        O95RSC
058700970530     C                   MOVEL     CPRDES        O95RDS
058800970523     C*
058900970523     C                   ENDSR
059000970424     C*------------------------------------------------------------------------*
059100970515     C* CHKNAR - CONTROLLA NAZIONE
059200970424     C*------------------------------------------------------------------------*
059300970515     C     CHKNAR        BEGSR
059400941227     C*
059500970515     C* CONTROLLO ESISTENZA NAZIONE SU TABELLA 15
059600970407     C                   Z-ADD     CODUT         KTBUTE
059700970515     C                   MOVEL     '15'          KTBCOD
059800970407     C                   MOVEL     *BLANKS       KTBKEY
059900970516     C                   MOVEL     I95NAR        KTBKEY
060000970409     C     KEYTAB        CHAIN     TABEL00F                           99
060100970515     C     *IN99         IFEQ      '1'
060200970515     C     TBLFLG        ORNE      *BLANKS
060300970515     C                   MOVEL     MSG(1)        $MSG                           *NAZIONE INESISTENTE
060400970409     C                   EXSR      ERRORE
060500970515     C                   ELSE
060600970515     C                   MOVEL     TBLUNI        DS15
060700970704     C                   EXSR      NAROUT
060800970515     C                   ENDIF
060900970407     C*
061000941227     C                   ENDSR
061100970519     C*------------------------------------------------------------------------*
061200970519     C* CHKPRV - CONTROLLA PROVINCIA
061300970519     C*------------------------------------------------------------------------*
061400970519     C     CHKPRV        BEGSR
061500970519     C*
061600970519     C* CONTROLLO ESISTENZA PROVINCIA SU TABELLA PR
061700970519     C                   Z-ADD     CODUT         KTBUTE
061800970519     C                   MOVEL     'PR'          KTBCOD
061900970519     C                   MOVEL     *BLANKS       KTBKEY
062000970519     C                   MOVEL     I95PRV        KTBKEY
062100970519     C     KEYTAB        CHAIN     TABEL00F                           99
062200970519     C     *IN99         IFEQ      '1'
062300970519     C     TBLFLG        ORNE      *BLANKS
062400970519     C                   MOVEL     MSG(12)       $MSG                           *PROVINCIA INESISTEN
062500970519     C                   EXSR      ERRORE
062600970519     C                   ELSE
062700970519     C                   MOVEL     TBLUNI        DSPR
062800970519     C                   MOVEL     I95PRV        O95PRV
062900970519     C                   ENDIF
063000970519     C*
063100970519     C                   ENDSR
063200970627     C*------------------------------------------------------------------------*
063300970627     C* CERLS  - CERCA LOCALITA/SINONIMI
063400970627     C*        * INPUT : WLOC  (+I95CAP,I95PRV)
063500970627     C*        * OUTPUT: NUMLS  (NUM. DI LOC/SIN TROVATI)
063600970627     C*                  AFFLS  (AFFIDAB. LOC/SIN TROVATI)
063700970627     C*                  WOCAP  (CAP TROVATO)
063800970627     C*                  WOLOC  (LOC TROVATA)
063900970627     C*                  WOSIN  (SIN TROVATO)
064000970627     C*                  SINLOC (L=TROV.LOC.,S=TROV.SIN.)
064100970627     C*------------------------------------------------------------------------*
064200970627     C     CERLS         BEGSR
064300970627     C*
064400970627     C                   MOVEL     'N'           TROLS             1            *TROVATA LOC/SIN UNI
064500970627     C                   MOVEL     *BLANKS       SINLOC            1            *TROVATO L=LOC,S=SIN
064600970627     C                   Z-ADD     *ZEROS        NUMLS             3 0          *NUM. LOC/SIN TROVAT
064700970627     C                   Z-ADD     *ZEROS        AFFLS             3 0          *AFFIDABILITA LOC/SI
064800970627     C                   MOVEL     *BLANKS       WOCAP
064900970627     C                   MOVEL     *BLANKS       WOLOC
065000970627     C                   MOVEL     *BLANKS       WOSIN
065100000727     C                   MOVEL     *BLANKS       LSIN
065200970627     C*
065300970627     C* PRIMA CERCA SULLE LOCALITA
065400970627     C                   MOVEL     *OFF          *IN99
065500970627     C                   Z-ADD     O95VER        CPLVER
065600970627     C                   MOVEL     O95NAR        CPLNAR
065700970627     C                   MOVEL     WLOC          CPLLOC
065800970627     C     KCPL2         SETLL     AZCPL02L                           99
065900970627     C     KCPL2         READE     AZCPL02L                               99
066000970627DO  1C     *IN99         DOWEQ     *OFF
066100970627IF  2C     CPLATB        IFEQ      *BLANKS                                      *NON ANNULL.
066200970917IF  2C     CPLFI1        ANDEQ     *BLANKS                                      *NON OBSOLETO
066300970627     C                   MOVEL     CPLCAP        WCAP
066400970627     C                   EXSR      LETCAP
066500970627     C                   EXSR      CHKCP                                        *CONTR.CAP E PRV
066600970627IF  3C     WAFFID        IFEQ      AFFLS
066700970627     C                   ADD       1             NUMLS
066800970627     C                   MOVEL     'L'           SINLOC
066900970627     C                   MOVEL     CPLCAP        WOCAP
067000970627     C                   MOVEL     CPLLOC        WOLOC
067100970627     C                   ENDIF
067200970627IF  3C     WAFFID        IFGT      AFFLS
067300970627     C                   Z-ADD     1             NUMLS
067400970627     C                   MOVEL     'L'           SINLOC
067500970627     C                   Z-ADD     WAFFID        AFFLS
067600970627     C                   MOVEL     CPLCAP        WOCAP
067700970627     C                   MOVEL     CPLLOC        WOLOC
067800970627E   3C                   ENDIF
067900970627E   2C                   ENDIF
068000970627     C     KCPL2         READE     AZCPL02L                               99
068100970627E   1C                   ENDDO
068200000727     C*
068300010713     C* POI CERCA SUI SINONIMI -CON LOCALITA' "PASSATA"-
068400000727     C                   MOVEL     WLOC          LSIN                           *LOCALITA' OTTIMIZZA
068500000727     C                   EXSR      LETCPS                                       *LEGGE SINONIMI
068600970627     C*
068700000727     C* POI CERCA SUI SINONIMI -CON LA LOCALITA' NON OTTIMIZZATA (INPUT)-
068800000727IF  1C     NUMLS         IFEQ      *ZEROS                                       *TROVATO NIENTE
068900000727     C                   MOVEL     Y95LOC        LSIN                           *LOCALITA' IN INPUT
069000000727     C                   EXSR      LETCPS                                       *LEGGE SINONIMI
069100000727E   1C                   ENDIF
069200000727     C*
069300000727     C                   ENDSR
069400000727     C*------------------------------------------------------------------------*
069500000727     C* LETCPS - LEGGE I SINONIMI DELLA LOCALITA'
069600000727     C*        * INPUT : LSIN  (LOCALITA' DA CERCARE COME SINONIMO)
069700000727     C*        * OUTPUT: NUMLS (NUM. DI LOC/SIN TROVATI)
069800000727     C*------------------------------------------------------------------------*
069900000727     C     LETCPS        BEGSR
070000000727     C*
070100970627     C                   MOVEL     *OFF          *IN99
070200970627     C                   Z-ADD     O95VER        CPSVER
070300970627     C                   MOVEL     O95NAR        CPSNAR
070400000727     C                   MOVEL     LSIN          CPSSIN
070500970627     C     KCPS2         SETLL     AZCPS02L                           99
070600970627     C     KCPS2         READE     AZCPS02L                               99
070700970627DO  1C     *IN99         DOWEQ     *OFF
070800970627IF  2C     CPSATB        IFEQ      *BLANKS                                      *NON ANNULL.
070900970627     C                   MOVEL     CPSCAP        WCAP
071000970917     C                   MOVEL     CPSLOC        WLOC
071100970917     C                   EXSR      LETLOC
071200970917IF 2AC     CPLFI1        IFEQ      *BLANKS                                      *LOC. NON OBSOLETA
071300970627     C                   EXSR      LETCAP
071400970627     C                   EXSR      CHKCP                                        *CONTR.CAP E PRV
071500970627IF  3C     WAFFID        IFEQ      AFFLS
071600970627     C                   ADD       1             NUMLS
071700970627     C                   MOVEL     'S'           SINLOC
071800970627     C                   MOVEL     CPSCAP        WOCAP
071900970627     C                   MOVEL     CPSLOC        WOLOC
072000970627     C                   MOVEL     CPSSIN        WOSIN
072100970627     C                   ENDIF
072200970627IF  3C     WAFFID        IFGT      AFFLS
072300970627     C                   Z-ADD     1             NUMLS
072400970627     C                   MOVEL     'S'           SINLOC
072500970627     C                   Z-ADD     WAFFID        AFFLS
072600970627     C                   MOVEL     CPSCAP        WOCAP
072700970627     C                   MOVEL     CPSLOC        WOLOC
072800970627     C                   MOVEL     CPSSIN        WOSIN
072900970627E   3C                   ENDIF
073000970917E  2AC                   ENDIF
073100970917E   2C                   ENDIF
073200970627     C     KCPS2         READE     AZCPS02L                               99
073300970627E   1C                   ENDDO
073400000727     C*
073500000727     C                   ENDSR
073600970627     C*------------------------------------------------------------------------*
073700970627     C* CERSIN - CERCA SINONIMI
073800970627     C*        * INPUT : WCAP,WSIN
073900970627     C*        * OUTPUT: NUMSIN (NUM. DI SIN TROVATI)
074000970627     C*------------------------------------------------------------------------*
074100970627     C     CERSIN        BEGSR
074200970627     C*
074300000727     C                   MOVEL     *BLANKS       LSIN
074400000727     C*
074500000727     C* CERCA SUI SINONIMI -CON LA LOCALITA' OTTIMIZZATA-
074600000727     C                   MOVEL     WSIN          LSIN                            *LOCALITA' OTTIMIZZ
074700000727     C                   EXSR      LE2CPS                                       *LEGGE SINONIMI
074800000727     C*
074900000727     C* POI CERCA SUI SINONIMI -CON LA LOCALITA' NON OTTIMIZZATA (INPUT)-
075000000727IF  1C     NUMSIN        IFEQ      *ZEROS                                       *TROVATO NIENTE
075100000727     C                   MOVEL     Y95LOC        LSIN                           *LOCALITA' IN INPUT
075200000727     C                   EXSR      LE2CPS                                       *LEGGE SINONIMI
075300000727E   1C                   ENDIF
075400000727     C*
075500000727     C                   ENDSR
075600000727     C*------------------------------------------------------------------------*
075700000727     C* LE2CPS - LEGGE I SINONIMI DELLA LOCALITA'
075800000727     C*        * INPUT : LSIN  (LOCALITA' DA CERCARE COME SINONIMO)
075900000727     C*        * OUTPUT: NUMSIN (NUM. DI SIN TROVATI)
076000000727     C*------------------------------------------------------------------------*
076100000727     C     LE2CPS        BEGSR
076200000727     C*
076300970627     C                   Z-ADD     *ZEROS        NUMSIN            3 0          *NUM. SIN TROVATI
076400970627     C                   MOVEL     *OFF          *IN99
076500970627     C                   Z-ADD     O95VER        CPSVER
076600970627     C                   MOVEL     O95NAR        CPSNAR
076700970627     C                   MOVEL     WCAP          CPSCAP
076800000727     C                   MOVEL     LSIN          CPSSIN
076900970627     C     KCPS4         SETLL     AZCPS04L                           99
077000970627     C     KCPS4         READE     AZCPS04L                               99
077100970627DO  1C     *IN99         DOWEQ     *OFF
077200970627IF  2C     CPSATB        IFEQ      *BLANKS                                      *NON ANNULL.
077300970627     C                   ADD       1             NUMSIN
077400970627E   2C                   ENDIF
077500970627     C     KCPS4         READE     AZCPS04L                               99
077600970627E   1C                   ENDDO
077700970627     C*
077800970627     C                   ENDSR
077900970627     C*------------------------------------------------------------------------*
078000970627     C* CHKCP  - CONTROLLA CAP E PROVINCIA
078100970627     C*------------------------------------------------------------------------*
078200970627     C     CHKCP         BEGSR
078300970627     C*
078400970627     C                   Z-ADD     *ZEROS        WAFFID            3 0
078500970627IF  1C     I95CAP        IFEQ      *BLANKS
078600970627     C     I95PRV        ANDEQ     *BLANKS
078700970627X   1C                   ELSE
078800970627IF  2C     I95CAP        IFNE      *BLANKS
078900970627     C     I95CAP        ANDEQ     CPCCAP
079000970627     C                   ADD       1             WAFFID
079100970627E   2C                   ENDIF
079200970627IF  2C     I95PRV        IFNE      *BLANKS
079300970627     C     §15CPR        ANDEQ     'S'                                          *NAZ.CON PRV DA CONT
079400970627IF  3C     CPCPRV        IFNE      *BLANKS
079500970627     C     I95PRV        ANDEQ     CPCPRV
079600970627     C                   ADD       1             WAFFID
079700970627E   3C                   ENDIF
079800970627IF  3C     CPCPRA        IFNE      *BLANKS
079900970627     C     I95PRV        ANDEQ     CPCPRA
080000970627     C                   ADD       1             WAFFID
080100970627E   3C                   ENDIF
080200970627E   2C                   ENDIF
080300970627E   1C                   ENDIF
080400970627     C*
080500970627     C                   ENDSR
080600970627     C*------------------------------------------------------------------------*
080700970627     C* LETCAP - LETTURA ARCHIVIO CAP
080800970627     C*        * INPUT : WCAP
080900970627     C*        * OUTPUT: TROCAP = S/N
081000970627     C*------------------------------------------------------------------------*
081100970627     C     LETCAP        BEGSR
081200970627     C*
081300970627     C                   MOVEL     'S'           TROCAP            1
081400970627     C                   MOVEL     *OFF          *IN99
081500970627     C                   Z-ADD     O95VER        CPCVER
081600970627     C                   MOVEL     O95NAR        CPCNAR
081700970627     C                   MOVEL     WCAP          CPCCAP
081800970627     C     KCPC          CHAIN     AZCPC01L                           99
081900970627     C     *IN99         IFEQ      *ON                                          *NON TROVATO
082000970627     C     CPCATB        ORNE      *BLANKS                                      *ANNULLATO
082100970627     C                   MOVEL     'N'           TROCAP                         *NON TROVATO
082200970627     C                   ENDIF
082300970627     C*
082400970627     C                   ENDSR
082500970604     C*------------------------------------------------------------------------*
082600970604     C* LETLOC - LETTURA ARCHIVIO LOCALITA CON CAP
082700970604     C*        * INPUT : WCAP,WLOC
082800970604     C*        * OUTPUT: TROLOC ('S'=TROVATO)
082900970604     C*------------------------------------------------------------------------*
083000970604     C     LETLOC        BEGSR
083100970604     C*
083200970604     C                   MOVEL     'S'           TROLOC            1
083300970604     C                   MOVEL     *OFF          *IN99
083400970604     C                   Z-ADD     O95VER        CPLVER
083500970604     C                   MOVEL     O95NAR        CPLNAR
083600970604     C                   MOVEL     WCAP          CPLCAP
083700970604     C                   MOVEL     WLOC          CPLLOC
083800970627     C     KCPL1         CHAIN     AZCPL01L                           99
083900970627     C     *IN99         IFEQ      *ON                                          *NON TROVATO
084000970627     C     CPLATB        ORNE      *BLANKS                                      *ANNULLATO
084100970604     C                   MOVEL     'N'           TROLOC                         *NON TROVATA
084200970604     C                   ENDIF
084300970604     C*
084400970604     C                   ENDSR
084500970627     C*------------------------------------------------------------------------*
084600970627     C* LETSIN - LETTURA ARCHIVIO SINONIMO CON CAP
084700970627     C*        * INPUT : WCAP,WLOC,WSIN
084800970627     C*        * OUTPUT: TROLOC ('S'=TROVATO)
084900970627     C*------------------------------------------------------------------------*
085000970627     C     LETSIN        BEGSR
085100970627     C*
085200970627     C                   MOVEL     'S'           TROSIN            1
085300970627     C                   MOVEL     *OFF          *IN99
085400970627     C                   Z-ADD     O95VER        CPSVER
085500970627     C                   MOVEL     O95NAR        CPSNAR
085600970627     C                   MOVEL     WCAP          CPSCAP
085700970627     C                   MOVEL     WLOC          CPSLOC
085800970627     C                   MOVEL     WSIN          CPSSIN
085900970627     C     KCPS1         CHAIN     AZCPS01L                           99
086000970627     C     *IN99         IFEQ      *ON                                          *NON TROVATO
086100970627     C     CPSATB        ORNE      *BLANKS                                      *ANNULLATO
086200970627     C                   MOVEL     'N'           TROSIN                         *NON TROVATA
086300970627     C                   ENDIF
086400970627     C*
086500970627     C                   ENDSR
086600970703     C*------------------------------------------------------------------------*
086700970703     C* CERFIT - CERCA CAP FITTIZIO DEL CAP IN INPUT
086800970703     C*------------------------------------------------------------------------*
086900970703     C     CERFIT        BEGSR
087000970703     C*
087100970703     C                   MOVEA     I95CAP        CAP
087200970703     C                   Z-ADD     §15PCF        P                 2 0
087300970703     C                   CLEAR                   CAF
087400970703     C                   MOVEA     CAP(P)        CAF(P)
087500970703     C                   ADD       §15ECF        P                              *CARATT.DA TENERE
087600970703     C     P             IFLT      10
087700970703     C                   MOVEA     *BLANKS       CAF(P)
087800970703     C                   ENDIF
087900970703     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
088000970703     C*
088100970703     C                   ENDSR
088200970627     C*------------------------------------------------------------------------*
088300970627     C* CHKFOR - CONTROLLO FORMALE DEL CAP IN INPUT
088400970627     C*------------------------------------------------------------------------*
088500970627     C     CHKFOR        BEGSR
088600970627     C*---
088700970627     C* RIASSUMO I CARATTERI DI CONTROLLO PREVISTI;
088800970627     C*  "N" - SOLO NUMERI
088900970627     C*  "L" - SOLO LETTERE
089000970627     C*  "A" - CAMPO ALFANUMERICO     (NUMERI LETTERE)
089100970627     C*  " " - CAMPO DA NON IMMETTERE
089200970627     C*  "O" - OPZIONALE ALFANUMERICO (NUMERI LETTERE E BLANK)
089300970627     C*  "Q" - OPZIONALE SOLO LETTERE (       LETTERE E BLANK)
089400970627     C*  "0" - OPZIONALE NUMERICO     (NUMERI         E BLANK)
089500970627     C*---
089600970627     C                   MOVEA     I95CAP        CAP
089700970627     C                   MOVEA     §15CAP        CCP
089800970627     C*
089900970627     C* RECUPERO CAP FITTIZIO PER EVENTUALE TENTATIVO SUCCESSIVO
090000970627     C                   Z-ADD     §15PCF        P                 2 0
090100970627     C                   CLEAR                   CAF
090200970627     C                   MOVEA     CAP(P)        CAF(P)
090300970627     C                   ADD       §15ECF        P                              *CARATT.DA TENERE
090400970627     C     P             IFLT      10
090500970627     C                   MOVEA     *BLANKS       CAF(P)
090600970627     C                   ENDIF
090700970627     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
090800970627     C*
090900970627     C*
091000970627     C                   Z-ADD     1             X                 2 0
091100970627     C                   SETOFF                                       90
091200970627     C*
091300970627    1C     X             DOWLE     9
091400970627     C*
091500970627     C* CARATTERE  D A   N O N   I M M E T T E R E
091600970627    2C     CCP(X)        IFEQ      ' '
091700970627     C     CAP(X)        ANDNE     ' '
091800970627     C                   SETON                                        90
091900970627    2C                   ENDIF
092000970627     C*
092100970627     C* CARATTERE  N U M E R I C O
092200970627    2C     CCP(X)        IFEQ      'N'
092300970627     C     CCP(X)        OREQ      'A'
092400970627     C     CCP(X)        OREQ      'O'
092500970627     C     CCP(X)        OREQ      '0'
092600970627     C     CAP(X)        LOOKUP    NUM                                    30
092700970627     C*
092800970627     C* 30 OFF- NON TROVATO: PER "N" ERRORE SUBITO
092900970627     C  N30              SETON                                        90
093000970627     C*
093100970627     C* 30 OFF- NON TROVATO: PER "0" ERRORE SE NON E' NEMMENO ' '
093200970627    3C     *IN90         IFEQ      *ON
093300970627     C     CCP(X)        ANDEQ     '0'
093400970627     C     CAP(X)        ANDEQ     ' '
093500970627     C                   SETOFF                                       90
093600970627    3C                   ENDIF
093700970627    2C                   ENDIF
093800970627     C*
093900970627     C* CARATTERE  A L F A N U M E R I C O
094000970627    2C     CCP(X)        IFEQ      'L'
094100970627     C     CCP(X)        OREQ      'A'
094200970627     C     *IN90         ANDEQ     *ON
094300970627     C     CCP(X)        OREQ      'O'
094400970627     C     *IN90         ANDEQ     *ON
094500970627     C     CCP(X)        OREQ      'Q'
094600970627     C     CAP(X)        LOOKUP    LET                                    30
094700970627     C*---
094800970627     C* 30 OFF- NON TROVATO: PER "L" ERRORE SUBITO
094900970627     C* 30 OFF- NON TROVATO: PER "A" ERRORE SUBITO
095000970627     C*                      XCHE' NON TROVATO NEMMENO NEI NUMERICI
095100970627     C*---
095200970627     C  N30              SETON                                        90
095300970627     C   30              SETOFF                                       90
095400970627     C*
095500970627     C* 30 OFF- NON TROVATO: PER "O" E "Q" ERRORE SE NON E' NEMMENO ' '
095600970627    3C     *IN90         IFEQ      *ON
095700970627     C     CAP(X)        ANDEQ     ' '
095800970627     C*
095900970627    4C     CCP(X)        IFEQ      'O'
096000970627     C     CCP(X)        OREQ      'Q'
096100970627     C                   SETOFF                                       90
096200970627    4C                   ENDIF
096300970627    3C                   ENDIF
096400970627    2C                   ENDIF
096500970627     C*
096600970627    2C     *IN90         IFEQ      *ON
096700970627     C                   Z-ADD     10            X
096800970627     C                   MOVEL     MSG(2)        $MSG                           *CAP FORMALMENTE ERR
096900970627     C                   EXSR      ERRORE
097000970627    2C                   ENDIF
097100970627     C                   ADD       1             X
097200970627    1C                   ENDDO
097300970627     C*
097400970627     C                   ENDSR
097500150203     C*------------------------------------------------------------------------*
097600150203     C* CHKCFC - CONTROLLO FORMATO DEL CAP IN INPUT
097700150203     C*------------------------------------------------------------------------*
097800150203     C     CHKCFC        BEGSR
097900150203     C*
098000150209     C* NORMALIZZO IL CAP DESTINATARIO (TOLGO SPAZI AL INTERNO)
098100150209     C                   EXSR      NORCAP
098200150209     C*
098300150203     C* CODICE CONTROLLO CAP 'GB ' (OVVERO FORMATO TIPO GRAN BRETAGNA)
098400150203     C     §15CFC        IFEQ      'GB'
098500150203     C                   EXSR      CFCGB
098600150203     C                   ENDIF
098700150203     C*
098800150203     C                   ENDSR
098900150209     C*------------------------------------------------------------------------*
099000150209     C* NORCAP - NORMALIZZA CAP
099100150209     C*------------------------------------------------------------------------*
099200150209     C     NORCAP        BEGSR
099300150209     C*
099400150209     C* NORMALIZZO IL CAP DI INPUT ELIMINANDO GLI SPAZI IN MEZZO
099500150209     C                   MOVEA     I95CAP        CAP
099600150209     C                   MOVEA     *BLANKS       CAPN
099700150209     C                   Z-ADD     1             X                 2 0
099800150209     C                   Z-ADD     1             Y                 2 0
099900150209     C     X             DOWLE     9
100000150209     C     CAP(X)        IFEQ      ' '
100100150209     C                   ELSE
100200150209     C                   MOVE      CAP(X)        CAPN(Y)
100300150209     C                   ADD       1             Y
100400150209     C                   ENDIF
100500150209     C                   ADD       1             X
100600150209     C                   ENDDO
100700150209     C*
100800150209     C                   ENDSR
100900150203     C*------------------------------------------------------------------------*
101000150203     C* CFCGB  - CODICE FORMATO 'GB '
101100150203     C*------------------------------------------------------------------------*
101200150203     C     CFCGB         BEGSR
101300150204     C*
101400150204     C* INIZIALIZZAZIONI
101500150204     C                   SETOFF                                       91
101600150204     C                   Z-ADD     *ZEROS        WLENG             1 0
101700150203     C*---
101800150203     C* CHECK PER MASCHERA 'AA9A9AA'
101900150203     C*
102000150204     C     *IN91         IFEQ      *OFF
102100150203     C                   MOVEL     *BLANKS       WMASK             9
102200150203     C                   MOVEL     'AA9A9AA'     WMASK
102300150204     C                   Z-ADD     4             WLENG
102400150209     C                   EXSR      CHKMSK
102500150203     C                   ENDIF
102600150203     C*
102700150203     C*---
102800150203     C* CHECK PER MASCHERA 'A9A9AA'
102900150203     C*
103000150204     C     *IN91         IFEQ      *OFF
103100150203     C                   MOVEL     *BLANKS       WMASK             9
103200150203     C                   MOVEL     'A9A9AA'      WMASK
103300150204     C                   Z-ADD     3             WLENG
103400150209     C                   EXSR      CHKMSK
103500150209     C                   ENDIF
103600150203     C*
103700150203     C*---
103800150203     C* CHECK PER MASCHERA 'A99AA'
103900150203     C*
104000150204     C     *IN91         IFEQ      *OFF
104100150203     C                   MOVEL     *BLANKS       WMASK             9
104200150203     C                   MOVEL     'A99AA'       WMASK
104300150204     C                   Z-ADD     2             WLENG
104400150209     C                   EXSR      CHKMSK
104500150209     C                   ENDIF
104600150203     C*
104700150203     C*---
104800150203     C* CHECK PER MASCHERA 'A999AA'
104900150203     C*
105000150204     C     *IN91         IFEQ      *OFF
105100150203     C                   MOVEL     *BLANKS       WMASK             9
105200150203     C                   MOVEL     'A999AA'      WMASK
105300150204     C                   Z-ADD     3             WLENG
105400150209     C                   EXSR      CHKMSK
105500150209     C                   ENDIF
105600150203     C*
105700150203     C*---
105800150203     C* CHECK PER MASCHERA 'AA99AA'
105900150203     C*
106000150204     C     *IN91         IFEQ      *OFF
106100150203     C                   MOVEL     *BLANKS       WMASK             9
106200150203     C                   MOVEL     'AA99AA'      WMASK
106300150204     C                   Z-ADD     3             WLENG
106400150209     C                   EXSR      CHKMSK
106500150209     C                   ENDIF
106600150203     C*
106700150203     C*---
106800150203     C* CHECK PER MASCHERA 'AA999AA'
106900150203     C*
107000150204     C     *IN91         IFEQ      *OFF
107100150203     C                   MOVEL     *BLANKS       WMASK             9
107200150203     C                   MOVEL     'AA999AA'     WMASK
107300150204     C                   Z-ADD     4             WLENG
107400150209     C                   EXSR      CHKMSK
107500150209     C                   ENDIF
107600150203     C*---
107700150204     C*
107800150204     C     *IN91         IFEQ      *OFF
107900150204     C                   MOVEL     MSG(2)        $MSG                           *CAP FORMALMENTE ERR
108000150204     C                   EXSR      ERRORE
108100150204     C                   ELSE
108200150204     C*
108300150204     C* AL TERMINE SE OK CONSIDERO I PRIMI <N> CARATTERI PER IL CONTROLLO DEL CAP FITTIZIO
108400150225     C                   MOVEA     CAPN          CAF                            *MEM CAP FITTIZIO
108500150204     C                   Z-ADD     1             Y                 2 0
108600150204     C     Y             DOWLE     9
108700150204     C     Y             IFLE      WLENG
108800150204     C                   ELSE
108900150204     C                   MOVE      *BLANK        CAF(Y)
109000150204     C                   ENDIF
109100150204     C                   ADD       1             Y
109200150204     C                   ENDDO
109300150225     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
109400150204     C                   ENDIF
109500150203     C*
109600150203     C                   ENDSR
109700150209     C*------------------------------------------------------------------------*
109800150209     C* CHKMSK - VERIFICA MASCHERA
109900150209     C*------------------------------------------------------------------------*
110000150209     C     CHKMSK        BEGSR
110100150209     C*
110200150209     C*---
110300150209     C* LEGENDA CODIFICA FOORMATI:
110400150209     C*  "A" - SOLO LETTERE
110500150209     C*  "9" - SOLO NUMERI
110600150209     C*---
110700150209     C                   SETOFF                                       90
110800150209     C                   MOVEA     CAPN          CAP
110900150209     C                   MOVEA     WMASK         CCP
111000150209     C*
111100150209     C* CICLO SUI CARATTERI DEL CAP IN INPUT E CONTROLLO CORRETTEZZA FORMATO
111200150209     C                   Z-ADD     1             X                 2 0
111300150209     C     X             DOWLE     9
111400150209     C*
111500150209     C* CARATTERE  N U M E R I C O
111600150209     C     *IN90         IFEQ      *OFF
111700150209     C     CCP(X)        IFEQ      '9'
111800150209     C     CAP(X)        LOOKUP    NUM                                    30
111900150209     C* SE NON TROVATO => NON E' NUMERO
112000150209     C  N30              SETON                                        90
112100150209     C                   ENDIF
112200150209     C                   ENDIF
112300150209     C*
112400150209     C* CARATTERE  A L F A B E T I C O
112500150209     C     *IN90         IFEQ      *OFF
112600150209     C     CCP(X)        IFEQ      'A'
112700150209     C     CAP(X)        LOOKUP    LET                                    30
112800150209     C* SE NON TROVATO => NON E' LETTTERA
112900150209     C  N30              SETON                                        90
113000150209     C                   ENDIF
113100150209     C                   ENDIF
113200150209     C*
113300150209     C                   ADD       1             X
113400150209     C                   ENDDO
113500150209     C*
113600150209     C     *IN90         IFEQ      *OFF
113700150209     C                   SETON                                        91
113800150209     C                   ENDIF
113900150209     C*
114000150209     C                   ENDSR
114100971103     C*------------------------------------------------------------------------*
114200971103     C*   OTTLOC - OTTIMIZZA LOCALITA' (TOGLIE SPAZI,MINUSCOLE,CARATT.STRANI,..)
114300971103     C*------------------------------------------------------------------------*
114400971103     C     OTTLOC        BEGSR
114500971103     C*
114600971104     C* CICLO PER TUTTI I CARATTERI DELLA LOCALITA'
114700971104     C                   MOVEA     I95LOC        LOI
114800971105     C                   CLEAR                   LOO
114900971105     C                   Z-ADD     *ZEROS        NSPAZI            3 0
115000971105     C                   MOVEL     'N'           FINLOC            1
115100971105     C                   Z-ADD     *ZEROS        O                 3 0
115200971104     C*
115300971105DO  1C                   DO        35            L                 3 0
115400971105IF  2C     FINLOC        IFEQ      'N'
115500971104     C                   MOVEL     LOI(L)        CARATT            1
115600971105     C                   MOVEL     'N'           SCARTO            1
115700971104     C*
115800971104     C* SE MINUSOLO TRASFORMA IN MAIUSOLO
115900971105     C                   Z-ADD     1             I                 3 0
116000971104     C     CARATT        LOOKUP    LEM(I)                                 99    *CERCA IL TIPO BOLLA
116100971104     C     *IN99         IFEQ      '1'                                          *E' MINUSOLO
116200971104     C                   MOVEL     LET(I)        CARATT
116300971104     C                   END
116400971104     C*
116500971104     C* SE VIRGOLA SOSTITUISCE CON SPAZIO
116600971104     C     CARATT        IFEQ      ','
116700971104     C                   MOVEL     *BLANKS       CARATT
116800971104     C                   ENDIF
116900971105     C*
117000971105     C* SE HO GIA' CONTATO ALMENO UNO SPAZIO E QUESTO E' IL SECONDO, SCARTO
117100971105     C* SE PERO' DEVO INCONTRARE ANCORA UN CARATT. DIVERSO DA SPAZIO, SCARTO
117200971105IF  3C     CARATT        IFEQ      *BLANKS
117300971105IF  4C     O             IFEQ      *ZEROS
117400971105     C                   MOVEL     'S'           SCARTO
117500971105X   4C                   ELSE
117600971105     C     NSPAZI        IFEQ      0
117700971105     C                   Z-ADD     1             NSPAZI
117800971105     C                   ELSE
117900971105     C                   MOVEL     'S'           SCARTO
118000971105     C                   ENDIF
118100971105E   4C                   ENDIF
118200971105     C* CARATTERE DIVERSO DA SPAZIO AZZERO CONTATORE SPAZI
118300971105X   3C                   ELSE
118400971105     C                   Z-ADD     *ZEROS        NSPAZI
118500971105E   3C                   ENDIF
118600971104     C*
118700971105     C* SE TROVO CARATTERE STRANO, SCARTO TUTTO CIO' CHE SI TROVA DOPO
118800000727     C     CARATT        IFEQ      '-'
118900000727     C*---       CARATT    OREQ '/'
119000971104     C     CARATT        OREQ      '('
119100971104     C     CARATT        OREQ      '*'
119200971105     C                   MOVEL     'S'           FINLOC
119300971104     C                   ENDIF
119400000727IF  3C     TLAN          IFEQ      'J'                                          *OTTIMIZZA X J95LOC
119500000727     C     CARATT        ANDEQ     '/'
119600000727     C                   MOVEL     'S'           FINLOC
119700000727E   3C                   ENDIF
119800971105     C*
119900971105     C* SE NON DEVO ESCLUDERE TUTTO IL RESTO DELLA LOCALITA'
120000971105     C* OPPURE NON DEVO SCARTARE, TRASFERISCO CARATTERE
120100971105     C     FINLOC        IFEQ      'N'
120200971105     C     SCARTO        ANDEQ     'N'
120300971105     C                   ADD       1             O
120400971105     C                   MOVEL     CARATT        LOO(O)
120500971105     C                   ENDIF
120600971104     C*
120700971105E   2C                   ENDIF
120800971105E   1C                   ENDDO
120900140212     C*
121000140212     C* SE COME ULTIMO CARATTERE TROVO UN "." --> LO ELIMINO
121100140212     C                   MOVEL     'N'           FINLOC            1
121200140212     C                   Z-ADD     35            L
121300140212DO  1C                   DO        35
121400140212IF  2C     FINLOC        IFEQ      'N'
121500140212     C                   MOVEL     LOO(L)        CARATT            1
121600140212     C     CARATT        IFNE      ' '
121700140212     C                   MOVEL     'S'           FINLOC
121800140212     C     CARATT        IFEQ      '.'
121900140212     C                   MOVEL     ' '           LOO(L)
122000140212     C                   ENDIF
122100140212     C*
122200140212     C                   ENDIF
122300140212     C                   ENDIF
122400140212     C                   SUB       1             L
122500140212     C                   ENDDO
122600971105     C*
122700000727IF  1C     TLAN          IFEQ      'J'                                          *OTTIMIZZA X J95LOC
122800971105     C                   MOVEA     LOO           J95LOC
122900000727X   1C                   ELSE                                                   *OTTIMIZZA X Y95LOC
123000000727     C                   MOVEA     LOO           Y95LOC
123100000727E   1C                   ENDIF
123200000727     C*
123300971103     C                   ENDSR
123400970627     C*------------------------------------------------------------------------*
123500970627     C*   CAPOUT - PORTA CAMPI DA FILE CAPPARIO A DS OUTPUT CAP
123600970627     C*------------------------------------------------------------------------*
123700970627     C     CAPOUT        BEGSR
123800970627     C*
123900970627     C**********           MOVELCPCNAR    O95NAR
124000970627     C                   MOVEL     CPCCAP        O95CAP
124100970627     C                   Z-ADD     CPCLNA        O95CLA
124200970627     C                   Z-ADD     CPCZNC        O95CZC
124300970627     C                   MOVEL     CPCPRV        O95PRV
124400970627     C                   MOVEL     CPCPRA        O95PRA
124500970627     C                   MOVEL     CPCFLG        O95FLG
124600970627     C                   MOVEL     CPCFIT        O95FIT
124700970627     C                   MOVEL     CPCISO        O95CIS
124800970627     C                   Z-ADD     CPCZFD        O95CZF
124900970627     C                   Z-ADD     CPCLKG        O95CLK
125000970627     C                   Z-ADD     CPCLMC        O95CLM
125100970627     C                   Z-ADD     CPCLOL        O95CLO
125200970627     C                   Z-ADD     CPCZOL        O95CZO
125300970627     C                   Z-ADD     CPCLKS        O95CKS
125400970627     C                   Z-ADD     CPCLMS        O95CMS
125500970627     C                   Z-ADD     CPCLOS        O95CLS
125600970627     C                   Z-ADD     CPCZOS        O95CZS
125700970627     C                   Z-ADD     CPCLK2        O95CK2
125800970627     C                   Z-ADD     CPCLM2        O95CM2
125900970627     C                   Z-ADD     CPCLO2        O95CL2
126000970627     C                   Z-ADD     CPCZO2        O95CZ2
126100970627     C                   MOVEL     CPCCTS        O95CCT
126200970627     C                   MOVEL     CPCCTE        O95CCE
126300970627     C                   Z-ADD     CPCTTC        O95CTC
126400970627     C                   MOVEL     CPCPII        O95CPI
126500970627     C                   MOVEL     CPCPID        O95CPD
126600970627     C                   MOVEL     CPCPIS        O95CPS
126700970627     C                   MOVEL     CPCGDD        O95CGD
126800970627     C                   MOVEL     CPCESE        O95ESE
126900970627     C                   MOVEL     CPCESL        O95ESL
127000970627     C                   MOVEL     CPCFI1        O95CF1
127100970627     C                   MOVEL     CPCFI2        O95CF2
127200970627     C*
127300970627     C                   ENDSR
127400970521     C*------------------------------------------------------------------------*
127500970604     C*   LOCOUT - PORTA CAMPI DA FILE LOCALITA A DS OUTPUT LOCALITA
127600970521     C*------------------------------------------------------------------------*
127700970604     C     LOCOUT        BEGSR
127800970521     C*
127900970516     C                   MOVEL     CPLLOC        O95LOC
128000970516     C                   MOVEL     CPLLIV        O95LIV
128100970520     C                   MOVEL     CPLDLV        O95DLV
128200970520     C                   Z-ADD     CPLLNA        O95LLA
128300970520     C                   Z-ADD     CPLZNC        O95LZC
128400970520     C                   MOVEL     CPLISO        O95LIS
128500970520     C                   Z-ADD     CPLZFD        O95LZF
128600970520     C                   Z-ADD     CPLLKG        O95LLK
128700970520     C                   Z-ADD     CPLLMC        O95LLM
128800970520     C                   Z-ADD     CPLLOL        O95LLO
128900970520     C                   Z-ADD     CPLZOL        O95LZO
129000970604     C                   Z-ADD     CPLLKS        O95LKS
129100970604     C                   Z-ADD     CPLLMS        O95LMS
129200970604     C                   Z-ADD     CPLLOS        O95LLS
129300970604     C                   Z-ADD     CPLZOS        O95LZS
129400970604     C                   Z-ADD     CPLLK2        O95LK2
129500970604     C                   Z-ADD     CPLLM2        O95LM2
129600970604     C                   Z-ADD     CPLLO2        O95LL2
129700970604     C                   Z-ADD     CPLZO2        O95LZ2
129800970520     C                   MOVEL     CPLCTS        O95LCT
129900970520     C                   MOVEL     CPLCTE        O95LCE
130000970520     C                   Z-ADD     CPLTTC        O95LTC
130100970520     C                   MOVEL     CPLPII        O95LPI
130200970520     C                   MOVEL     CPLPID        O95LPD
130300970520     C                   MOVEL     CPLPIS        O95LPS
130400970520     C                   MOVEL     CPLGDD        O95LGD
130500970520     C                   MOVEL     CPLESS        O95ESS
130600970530     C                   MOVEL     CPLESV        O95ESV
130700970520     C                   MOVEL     CPLFI1        O95LF1
130800970520     C                   MOVEL     CPLFI2        O95LF2
130900970516     C*
131000970516     C                   ENDSR
131100970520     C*------------------------------------------------------------------------*
131200970520     C*   SINOUT - PORTA CAMPI DA FILE SINONIMI A DS OUTPUT
131300970520     C*------------------------------------------------------------------------*
131400970520     C     SINOUT        BEGSR
131500970520     C*
131600970520     C                   MOVEL     CPSSIN        O95SIN
131700970520     C*
131800970520     C                   ENDSR
131900970704     C*------------------------------------------------------------------------*
132000970704     C*   NAROUT - PORTA CAMPI DA TAB 15 NAZIONI A DS OUTPUT
132100970704     C*------------------------------------------------------------------------*
132200970704     C     NAROUT        BEGSR
132300970704     C*
132400970704     C                   MOVEL     I95NAR        O95NAR
132500970704     C                   MOVEL     §15DES        O95NDE
132600970704     C                   MOVEL     §15UCF        O95NUF
132700970704     C                   MOVEL     §15ITA        O95NIT
132800970704     C                   MOVEL     §15CPR        O95NCP
132900970704     C*
133000970704     C                   ENDSR
133100970604     C*------------------------------------------------------------------------*
133200970604     C*   OUTGEN - PORTA CAMPI DA CAP OUT O LOC OUT AI CAMPI OUT GENERALI
133300970604     C*------------------------------------------------------------------------*
133400970604     C     OUTGEN        BEGSR
133500970604     C*
133600970604     C* SE TROVATA LOCALITA' E LIVELLO GESTIONE = LOCALITA',
133700970604     C* PORTO I DATI LOC. IN OUT GENERALE
133800970605IF  1C     O95LOC        IFNE      *BLANKS
133900970604     C     O95LIV        ANDEQ     'L'
134000970604     C*
134100970605     C                   Z-ADD     O95LLA        O95LNA                         *LINEA ARR.
134200970605     C                   Z-ADD     O95LZC        O95ZNC                         *ZONA CONS.
134300970606     C                   MOVEL     *BLANKS       O95LIL                         *LIV.LNA/ZNC
134400010903     C                   MOVEL     O95LF2        O95GF2                         *CODICE TEMPO RITIRO
134500971126     C* CONTROLLO FERMO DEPOSITO
134600971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
134700971126     C     I95FFD        ANDEQ     'S'                                          *SI FERMO DEP.
134800970606     C                   MOVEL     'F'           O95LIL                         *LIV.LNA/ZNC: F.DEP
134900970605     C                   Z-ADD     O95LZF        O95ZNC                         *ZONA CONS.
135000971126     C                   ENDIF
135100971126     C* CONTROLLO LIMITE OLTRE
135200160628     C***        I95TSP    IFNE 'E'                        *MAI OLTRE SE "E"
135300160628     C***        I95TSP    ANDNE'H'                        *MAI OLTRE SE "H"
135400160628     C     I95TSP        IFNE      'E'                                          *MAI OLTRE SE "E" E
135500160628     C     I95TSP        ANDNE     'H'                                          *MAI OLTRE SE "H" E
135600160628     C     I95DAT        ANDLT     20160901
135700160628     C     I95TSP        ORNE      'H'                                          *MAI OLTRE SE "H" E
135800160628     C     I95DAT        ANDGE     20160901
135900971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
136000971126     C     I95LKG        IFNE      *ZEROS                                       *LIMITE KG OLTRE
136100970605     C     O95LLK        ANDNE     *ZEROS
136200970605     C     I95LKG        ANDGT     O95LLK
136300970604     C     I95LMC        ORNE      *ZEROS                                       *LIMITE MC OLTRE
136400970605     C     O95LLM        ANDNE     *ZEROS
136500970605     C     I95LMC        ANDGT     O95LLM
136600970606     C                   MOVEL     'O'           O95LIL                         *LIV.LNA/ZNC: CON OL
136700970605     C                   Z-ADD     O95LLO        O95LNA
136800970605     C                   Z-ADD     O95LZO        O95ZNC
136900971126     C                   ENDIF
137000971126     C                   ENDIF
137100080219     C                   ENDIF
137200971126     C* CONTROLLO LIMITE SOTTO
137300971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
137400971126     C     I95LKG        IFNE      *ZEROS                                       *LIMITE KG SOTTO
137500970605     C     O95LKS        ANDNE     *ZEROS
137600970617     C     I95LKG        ANDLT     O95LKS
137700970604     C     I95LMC        ORNE      *ZEROS                                       *LIMITE MC SOTTO
137800970605     C     O95LMS        ANDNE     *ZEROS
137900970617     C     I95LMC        ANDLT     O95LMS
138000970606     C                   MOVEL     'S'           O95LIL                         *LIV.LNA/ZNC: CON SO
138100970606     C                   Z-ADD     O95LLS        O95LNA
138200970606     C                   Z-ADD     O95LZS        O95ZNC
138300971126     C                   ENDIF
138400971126     C                   ENDIF
138500971126     C* ALTRI DATI LOCALITA'
138600970616     C                   Z-ADD     O95LTC        O95TTC
138700970606     C                   MOVEL     O95LIS        O95ISO
138800970606     C                   MOVEL     O95LCT        O95CTS
138900970606     C                   MOVEL     O95LCE        O95CTE
139000970606     C                   MOVEL     O95LPI        O95PII
139100970606     C                   MOVEL     O95LPD        O95PID
139200970606     C                   MOVEL     O95LPS        O95PIS
139300970606     C                   MOVEL     O95LGD        O95GDD
139400970618     C     O95LNA        IFNE      O95LLA
139500970618     C                   MOVEL     'S'           O952LN
139600970618     C                   ENDIF
139700020603     C*
139800080219     C* SE CONSEGNA PARTICOLARE SUPERMERCATI, FORZA SEMPRE LINEA/ZONA OLTRE
139900020603IF  2C     I95FI2        IFNE      *BLANKS
140000020715     C     O95LLO        ANDGT     *ZEROS
140100020603     C                   MOVEL     I95FI2        A1S               1            *ALFA 1 SX
140200020603     C                   MOVE      I95FI2        A1D               1            *ALFA 1 DX
140300020603IF  3C     A1S           IFEQ      'S'                                          *SOLO SE SUPERMERCAT
140400020603     C     A1D           OREQ      'S'
140500020603     C                   MOVEL     'O'           O95LIL                         *LIV.LNA/ZNC: CON OL
140600020603     C                   Z-ADD     O95LLO        O95LNA
140700020612     C                   Z-ADD     O95LZO        O95ZNC
140800020603E   3C                   ENDIF
140900020603E   2C                   ENDIF
141000970604     C*
141100970604X   1C                   ELSE
141200970604     C*
141300970605     C* SE TROVATO CAP LO PORTO IN OUT GEN
141400971126IF  2C     O95CAP        IFNE      *BLANKS
141500970606     C                   Z-ADD     O95CLA        O95LNA                         *LINEA ARR.
141600970606     C                   Z-ADD     O95CZC        O95ZNC                         *ZONA CONS.
141700970606     C                   MOVEL     *BLANKS       O95LIL                         *LIV.LNA/ZNC
141800010903     C                   MOVEL     O95CF2        O95GF2                         *CODICE TEMPO RITIRO
141900971126     C* CONTROLLO FERMO DEPOSITO
142000971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
142100971126     C     I95FFD        ANDEQ     'S'                                          *SI FERMO DEP.
142200970606     C                   MOVEL     'F'           O95LIL                         *LIV.LNA/ZNC: F.DEP
142300970606     C                   Z-ADD     O95CZF        O95ZNC                         *ZONA CONS.
142400971126     C                   ENDIF
142500971126     C* CONTROLLO LIMITE OLTRE
142600160628     C***        I95TSP    IFNE 'E'                        *MAI OLTRE SE "E"
142700160628     C***        I95TSP    ANDNE'H'                        *MAI OLTRE SE "H"
142800160628     C     I95TSP        IFNE      'E'                                          *MAI OLTRE SE "E" E
142900160628     C     I95TSP        ANDNE     'H'                                          *MAI OLTRE SE "H" E
143000160628     C     I95DAT        ANDLT     20160901
143100160628     C     I95TSP        ORNE      'H'                                          *MAI OLTRE SE "H" E
143200160628     C     I95DAT        ANDGE     20160901
143300971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
143400971126     C     I95LKG        IFNE      *ZEROS                                       *LIMITE KG OLTRE
143500970606     C     O95CLK        ANDNE     *ZEROS
143600970606     C     I95LKG        ANDGT     O95CLK
143700970604     C     I95LMC        ORNE      *ZEROS                                       *LIMITE MC OLTRE
143800970606     C     O95CLM        ANDNE     *ZEROS
143900970606     C     I95LMC        ANDGT     O95CLM
144000970606     C                   MOVEL     'O'           O95LIL                         *LIV.LNA/ZNC: OLTRE
144100970606     C                   Z-ADD     O95CLO        O95LNA
144200970606     C                   Z-ADD     O95CZO        O95ZNC
144300971126     C                   ENDIF
144400971126     C                   ENDIF
144500080219     C                   ENDIF
144600971126     C* CONTROLLO LIMITE SOTTO
144700971126     C     O95LIL        IFEQ      *BLANKS                                      *NON ANCORA SCELTO L
144800971126     C     I95LKG        IFNE      *ZEROS                                       *LIMITE KG SOTTO
144900970606     C     O95CKS        ANDNE     *ZEROS
145000970617     C     I95LKG        ANDLT     O95CKS
145100970604     C     I95LMC        ORNE      *ZEROS                                       *LIMITE MC SOTTO
145200970606     C     O95CMS        ANDNE     *ZEROS
145300970617     C     I95LMC        ANDLT     O95CMS
145400970606     C                   MOVEL     'S'           O95LIL                         *LIV.LNA/ZNC: SOTTO
145500970606     C                   Z-ADD     O95CLS        O95LNA
145600970606     C                   Z-ADD     O95CZS        O95ZNC
145700971126     C                   ENDIF
145800971126     C                   ENDIF
145900971126     C* ALTRI DATI CAPPARIO
146000970616     C                   Z-ADD     O95CTC        O95TTC
146100970606     C                   MOVEL     O95CIS        O95ISO
146200970606     C                   MOVEL     O95CCT        O95CTS
146300970606     C                   MOVEL     O95CCE        O95CTE
146400970606     C                   MOVEL     O95CPI        O95PII
146500970606     C                   MOVEL     O95CPD        O95PID
146600970606     C                   MOVEL     O95CPS        O95PIS
146700970606     C                   MOVEL     O95CGD        O95GDD
146800970618     C     O95LNA        IFNE      O95CLA
146900970618     C                   MOVEL     'S'           O952LN
147000970618     C                   ENDIF
147100971126E   2C                   ENDIF
147200020603     C*
147300080219     C* SE CONSEGNA PARTICOLARE SUPERMERCATI, FORZA SEMPRE LINEA/ZONA OLTRE
147400020603IF  2C     I95FI2        IFNE      *BLANKS
147500020715     C     O95CLO        ANDGT     *ZEROS
147600020603     C                   MOVEL     I95FI2        A1S                            *ALFA 1 SX
147700020603     C                   MOVE      I95FI2        A1D                            *ALFA 1 DX
147800020603IF  3C     A1S           IFEQ      'S'                                          *SOLO SE SUPERMERCAT
147900020603     C     A1D           OREQ      'S'
148000020603     C                   MOVEL     'O'           O95LIL                         *LIV.LNA/ZNC: CON OL
148100020603     C                   Z-ADD     O95CLO        O95LNA
148200020612     C                   Z-ADD     O95CZO        O95ZNC
148300020603E   3C                   ENDIF
148400020603E   2C                   ENDIF
148500970604     C*
148600970604E   1C                   ENDIF
148700970630     C*
148800970630     C                   ENDSR
148900970630     C*------------------------------------------------------------------------*
149000990607     C* GESLID - DETERMINA VALORE CAMPO OUT O95LID (LIVELLO DATI)
149100970630     C*------------------------------------------------------------------------*
149200970630     C     GESLID        BEGSR
149300970604     C*
149400970630IF  1C     O95FIT        IFEQ      'S'
149500970630     C                   MOVEL     '1'           O95LID
149600970627X   1C                   ELSE
149700970627     C                   MOVEL     *BLANKS       O95LID
149800970627IF  2C     O95SIN        IFNE      *BLANKS
149900970627     C                   MOVEL     '4'           O95LID
150000970627X   2C                   ELSE
150100970627IF  3C     O95LOC        IFNE      *BLANKS
150200970627     C                   MOVEL     '3'           O95LID
150300970627X   3C                   ELSE
150400970627IF  4C     O95CAP        IFNE      *BLANKS
150500970627     C                   MOVEL     '2'           O95LID
150600970627E   4C                   ENDIF
150700970627E   3C                   ENDIF
150800970627E   2C                   ENDIF
150900970627E   1C                   ENDIF
151000970627     C*
151100970604     C                   ENDSR
151200970703     C*------------------------------------------------------------------------*
151300970703     C* GESLIA - DETERMINA VALORE CAMPO OUT O95LIA (AFFIDABILITA')
151400970703     C*------------------------------------------------------------------------*
151500970703     C     GESLIA        BEGSR
151600970703     C*
151700970710     C                   MOVEL     *BLANKS       O95LIA
151800970703     C* SE TROVATO CAP FITTIZIO
151900970703     C*    SE LA NAZIONE CONSENTE CAP FITTIZI: AFFIDABILITA' 2 (MEDIA)
152000970703     C*    SE LA NAZIONE NON CONSENTE CAP FITTIZI: AFFIDABILITA' 0 (BASSA)
152100970703IF  1C     O95FIT        IFEQ      'S'
152200970703IF  2C     §15UCF        IFEQ      'S'
152300970703     C                   MOVEL     '2'           O95LIA
152400970703X   2C                   ELSE
152500970703     C                   MOVEL     '0'           O95LIA
152600970703E   2C                   ENDIF
152700970703E   1C                   ENDIF
152800970703     C*
152900970703     C* SE IL CAP TROVATO RICHIEDE LA LOCALITA E QUESTA NON E' STATA TROVATA
153000970703     C* AFFIDABILITA' 1 (CAP RICHIEDE LOC)
153100970703IF  1C     O95LIA        IFEQ      *BLANKS
153200970703     C     O95ESL        ANDEQ     'S'
153300970703     C     O95LOC        ANDEQ     *BLANKS
153400970703     C                   MOVEL     '1'           O95LIA
153500970703E   1C                   ENDIF
153600970703     C*
153700970703     C* SE TROVATO ALMENO UN DATO COERENTE (ES. CAP E PRV) AFF.=1 , SE TROVATI 2 DATI
153800970703     C* COERENTI (CAP,LOC,PRV) AFFIDABILITA' 3 (ALTA)
153900970703IF  1C     O95LIA        IFEQ      *BLANKS
154000970710     C                   MOVEL     '0'           O95LIA
154100970703IF  2C     AFFLS         IFEQ      1
154200970703     C                   MOVEL     '2'           O95LIA
154300970703E   2C                   ENDIF
154400970703IF  2C     AFFLS         IFEQ      2
154500970703     C                   MOVEL     '3'           O95LIA
154600970703E   2C                   ENDIF
154700970703E   1C                   ENDIF
154800970703     C*
154900970703     C                   ENDSR
155000970519     C*------------------------------------------------------------------------*
155100970519     C* CHKCPT - CONTROLLO ESISTENZA TEMPI CONSEGNA
155200970519     C*------------------------------------------------------------------------*
155300970519     C     CHKCPT        BEGSR
155400970519     C*
155500970519     C                   MOVEL     'N'           $ERR
155600970519     C                   MOVEL     *OFF          *IN99
155700970523     C                   Z-ADD     O95VER        CPTVER
155800970519     C                   MOVEL     O95TTC        CPTTTC
155900970519     C                   MOVEL     I95TSP        CPTTSP
156000970523     C     KCPT          CHAIN     AZCPT01L                           99
156100970519IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
156200970519     C     CPTATB        ORNE      *BLANKS                                      *ANNULLATO
156300970519     C                   MOVEL     MSG(15)       $MSG                           *T.TEMPO/SERV. NON T
156400970519     C                   EXSR      ERRORE
156500970519X   1C                   ELSE                                                   *T.TEMPO TROVATO
156600970520     C                   EXSR      TEMOUT
156700970519E   1C                   ENDIF
156800970519     C*
156900970519     C                   ENDSR
157000970520     C*------------------------------------------------------------------------*
157100970520     C*   TEMOUT - PORTA CAMPI DA FILE TEMPI ALLA DS OUTPUT
157200970520     C*------------------------------------------------------------------------*
157300970520     C     TEMOUT        BEGSR
157400970520     C*
157500970520     C* ----TEMPI----
157600970520     C                   Z-ADD     CPTTTC        O95TTC
157700970520     C                   MOVEL     CPTDTC        O95DTC
157800970701     C                   MOVEL     CPTTSP        O95TSP
157900970520     C                   Z-ADD     CPT0IN        O950IN
158000970520     C                   Z-ADD     CPT0CN        O950CN
158100970520     C                   Z-ADD     CPT1IN        O951IN
158200970520     C                   Z-ADD     CPT1CN        O951CN
158300970520     C                   Z-ADD     CPTTIS        O95TIS
158400970520     C                   Z-ADD     CPTTCS        O95TCS
158500970520     C                   Z-ADD     CPTTLD        O95TLD
158600970520     C*
158700970520     C                   ENDSR
158800970519     C*------------------------------------------------------------------------*
158900970520     C* CHKCPE - CONTROLLO ECCEZIONI LINEE/ZONE
159000970519     C*------------------------------------------------------------------------*
159100970519     C     CHKCPE        BEGSR
159200970519     C*
159300970519     C                   MOVEL     'N'           $ERR
159400970519     C                   MOVEL     *OFF          *IN99
159500970523     C                   Z-ADD     O95VER        CPEVER
159600970519     C                   MOVEL     O95NAR        CPENAR
159700970519     C                   MOVEL     O95CAP        CPECAP
159800970519     C                   MOVEL     I95TFP        CPETFP
159900970523     C     KCPE          CHAIN     AZCPE01L                           99
160000970519IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
160100970519     C     CPEATB        ORNE      *BLANKS                                      *ANNULLATO
160200970519X   1C                   ELSE
160300970520     C                   EXSR      ECCOUT
160400970519E   1C                   ENDIF
160500970519     C*
160600970519     C                   ENDSR
160700970520     C*------------------------------------------------------------------------*
160800970520     C*   ECCOUT - PORTA CAMPI DA FILE ECCEZIONI ALLA DS OUTPUT
160900970520     C*------------------------------------------------------------------------*
161000970520     C     ECCOUT        BEGSR
161100970520     C*
161200970520     C* ----GENERALI----
161300020603     C* NON FA LE SOSTITUZIONE
161400020603     C* SE ESISTONO CONSEGNE PARTICOLARI, NON FA LE SOSTITUZIONE PER LE ECCEZIONI
161500020603     C                   MOVEL     I95FI2        A1S
161600020603     C                   MOVE      I95FI2        A1D
161700020603IF  0C     A1S           IFEQ      'S'                                          *SOLO SE SUPERMERCAT
161800020715     C     O95LIL        ANDEQ     'O'
161900020603     C     A1D           OREQ      'S'
162000020715     C     O95LIL        ANDEQ     'O'
162100020603X   0C                   ELSE
162200970616     C* SE LIV. SCELTA LNA/ZNC E' FERMO DEPOSITO NON CONSIDERO LNA/ZNC ECCEZIONE
162300970616IF  1C     O95LIL        IFNE      'F'                                          *SE LIV. LNA/ZNC <>
162400970520     C                   Z-ADD     CPELNA        O95LNA                         *LINEA ARR.
162500970520     C                   Z-ADD     CPEZNC        O95ZNC                         *ZONA CONS.
162600970606IF  2C     O95LIL        IFEQ      'O'                                          *SE LIV. LNA/ZNC OLT
162700970617     C     CPELOL        ANDNE     *ZEROS                                       *LNA <> 0
162800970520     C                   Z-ADD     CPELOL        O95LNA                         *LINEA ARRIVO
162900970520     C                   Z-ADD     CPEZOL        O95ZNC                         *ZONA CONSEGNA
163000970520E   2C                   ENDIF
163100970606IF  2C     O95LIL        IFEQ      'S'                                          *SE LIV. LNA/ZNC SOT
163200970617     C     CPELOS        ANDNE     *ZEROS                                       *LNA <> 0
163300970616     C                   Z-ADD     CPELOS        O95LNA                         *LINEA ARRIVO
163400970616     C                   Z-ADD     CPEZOS        O95ZNC                         *ZONA CONSEGNA
163500970606E   2C                   ENDIF
163600970606E   1C                   ENDIF
163700020603E   0C                   ENDIF
163800970520     C* ---ECCEZIONI---
163900970520     C                   Z-ADD     CPETFP        O95TFP
164000970520     C                   Z-ADD     CPELNA        O95ELA
164100970520     C                   Z-ADD     CPEZNC        O95EZC
164200970606     C                   Z-ADD     CPELOL        O95ELO
164300970606     C                   Z-ADD     CPEZOL        O95EZO
164400970606     C                   Z-ADD     CPELOS        O95ELS
164500970606     C                   Z-ADD     CPEZOS        O95EZS
164600970703     C                   Z-ADD     CPELO2        O95EL2
164700970703     C                   Z-ADD     CPEZO2        O95EZ2
164800970520     C*
164900970520     C                   ENDSR
165000000419     C*------------------------------------------------------------------------*
165100000419     C* REPERISCE INSTRADAMENTO POSTE
165200000419     C*------------------------------------------------------------------------*
165300000419     C     REPRDS        BEGSR
165400000419     C*
165500000629     C* NON REPERISCE L'INSTRADAMENTO POSTE SE CONTROLLO FORMALE CAP E
165600000629     C* SE IL CAP O LA LOCALITA' -SE DIVERSA DAL CAP- SONO OBSOLETI
165700000419IF  1C     I95TCN        IFNE      '1'
165800000629IF  2C     O95CF1        IFEQ      'O '                                         *CAP OBSOLETO o
165900000629     C     O95LIV        OREQ      'L'                                          *LOCALITA' <> CAP e
166000000629     C     O95LF1        ANDEQ     'O '                                         *LOCALITA' OBSOLETA
166100000629X   2C                   ELSE
166200000419     C                   MOVEL     'S'           $LPRDS            1            *PGM LANCIATO (' '/S
166300000419     C                   CLEAR                   TISI98                         *AZZERA DS PARAMETRI
166400000419     C                   MOVEL     I95TLA        I98TLA
166500000421     C                   MOVEL     I95TCN        I98TCN
166600000419     C                   MOVEL     I95FFD        I98FFD
166700000420     C                   Z-ADD     I95DAT        I98DAT
166800000419     C                   MOVEL     O95NAR        I98NAR
166900000419     C                   Z-ADD     O95VER        I98VER
167000000419     C                   MOVEL     O95CAP        I98CAP
167100000419     C                   MOVEL     O95LOC        I98LOC
167200000419     C                   MOVEL     O95LIV        I98LIV
167300000612     C                   MOVEL     I95FRE        I98FRE
167400000612     C                   Z-ADD     I95TFP        I98TFP
167500000612     C                   MOVEL     O95ESE        I98ESE
167600000612     C                   MOVEL     O95LID        I98LID
167700000612     C                   MOVEL     O95LIL        I98LIL
167800000419     C                   CALL      'TISI98R'
167900000419     C                   PARM                    TISI98
168000000629IF  3C     O98ERR        IFNE      *BLANKS                                      *ERRORE
168100000516     C                   MOVEL     O98MSG        $MSG
168200000419     C                   EXSR      ERRORE
168300000421     C                   CLEAR                   DRDS                           *IMPOSTA OUTPUT
168400000629E   3C                   ENDIF
168500000419     C                   EXSR      RDSOUT                                       *IMPOSTA OUTPUT
168600000629E   2C                   ENDIF
168700000629E   1C                   ENDIF
168800000419     C*
168900000419     C                   ENDSR
169000000419     C*------------------------------------------------------------------------*
169100000419     C* PORTA CAMPI OUTPUT DELLA TISI98DS NELLA DESCRIZIONE VERSIONE
169200000419     C*------------------------------------------------------------------------*
169300000419     C     RDSOUT        BEGSR
169400000419     C*
169500000419     C*
169600000419     C* ----GENERALI----
169700000419     C                   Z-ADD     O98LNA        O95LNA                         *LINEA ARRIVO
169800000419     C                   Z-ADD     O98ZNC        O95ZNC                         *ZONA CONSEGNA
169900000419     C*
170000000419     C* ----INSTRADAMENTO POSTE----
170100000419     C                   CLEAR                   DRDS                           *AZZERA DS
170200000419     C                   Z-ADD     O98LNA        RDSLNA                          -LINEA ARRIVO
170300000419     C                   Z-ADD     O98ZNC        RDSZNC                          -ZONA CONSEGNA
170400000419     C                   Z-ADD     O98ZFD        RDSZFD                          -FERMO DEPOSITO
170500000419     C                   Z-ADD     O98UPI        RDSUPI                          -UFFICIO POSTALE
170600000419     C                   Z-ADD     O98RS2        RDSRS2                          -RAGGRUPPAMENTO 2
170700000419     C                   MOVEL     O98RS1        RDSRS1                          -RAGGRUPPAMENTO 1
170800000419     C                   MOVEL(P)  DRDS          O95RDS                         *DESCRIZIONE VERSION
170900000419     C*
171000000419     C                   ENDSR
171100021028     C*------------------------------------------------------------------------*
171200021028     C*   OUTDPD - PORTA CAMPI INSTRADAMENTO DPD IN OUT GENERALI
171300021028     C*------------------------------------------------------------------------*
171400021028     C     OUTDPD        BEGSR
171500021028     C*
171600021028     C* SOSTITUISCE LA LINEA DI ARRIVO GENERAE CON QUELLA DELLA TABELLA NAZIONI
171700021028     C                   Z-ADD     §15LAD        O95LNA                         *LINEA ARRIVO
171800021028     C*
171900021028     C                   ENDSR
172000020211     C*------------------------------------------------------------------------*
172100020211     C*   OUTFED - PORTA CAMPI INSTRADAMENTO FEDEX IN OUT GENERALI
172200020211     C*------------------------------------------------------------------------*
172300020211     C     OUTFED        BEGSR
172400020211     C*
172500020211     C* AZZERA I CAMPI DI OUT DELLA DS SPECIFICI DEI SINGOLI ARCHVI
172600020211     C                   EXSR      AZZCAP                                       *AZZERA OUT CAP
172700020211     C                   EXSR      AZZLOC                                       *AZZERA OUT LOC
172800020211     C                   EXSR      AZZECC                                       *AZZERA OUT ECC
172900020211     C                   EXSR      AZZSIN                                       *AZZERA OUT SIN
173000020211     C*
173100020211     C* PRENDE I DATI DATTA TABELLA NAZIONI
173200020211     C                   MOVEL     §15LAF        O95LNA                         *LINEA ARRIVO
173300020211IF  1C     I95TPO        IFEQ      'F'                                          -FRANCO
173400020211     C                   MOVEL     01            O95ZNC                         *ZONA CONSEGNA
173500020211X   1C                   ELSE                                                   -ASSEGNATO
173600020211     C                   MOVEL     02            O95ZNC                         *ZONA CONSEGNA
173700020211E   1C                   ENDIF
173800020211     C                   MOVEL     §15TTC        O95TTC                         *TIPO TEMPO CONSEGNA
173900020211     C                   MOVEL     §15INL        O95ISO                         *FLAG INOLTRO
174000020211     C                   MOVEL     §15CTS        O95CTS                         *CODICE TASSAZIONE
174100020211     C                   MOVEL     'S'           O95PII
174200020211     C                   MOVEL     'S'           O95PID
174300020211     C                   MOVEL     'S'           O95PIS
174400020211     C                   MOVEL     'N'           O95GDD
174500020211     C*
174600020211     C* SE RICHIESTO RECUPERO TEMPI CONSEGNA
174700020211IF  1C     $ERR          IFEQ      'N'
174800020211IF  2C     I95FRT        IFEQ      'S'                                          *RICH.TEMPI CONS.
174900020211     C     O95TTC        ANDNE     *ZEROS
175000020211     C                   EXSR      CHKCPT
175100020211E   2C                   ENDIF
175200020211E   1C                   ENDIF
175300020211     C*
175400020211     C                   ENDSR
175500970519     C*------------------------------------------------------------------------*
175600970519     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
175700970519     C*------------------------------------------------------------------------*
175800970519     C     AZZERA        BEGSR
175900970604     C                   EXSR      AZZCAP                                       *AZZERA OUT CAP
176000970604     C                   EXSR      AZZLOC                                       *AZZERA OUT LOC
176100970606     C                   EXSR      AZZECC                                       *AZZERA OUT ECC
176200970605     C                   EXSR      AZZSIN                                       *AZZERA OUT SIN
176300970519     C                   CLEAR                   O95ERR
176400970519     C                   CLEAR                   O95MSG
176500970606     C                   CLEAR                   O95LID
176600970606     C                   CLEAR                   O95LIL
176700970627     C                   CLEAR                   O95LIA
176800970618     C                   CLEAR                   O952LN
176900970519     C                   CLEAR                   O95NAR
177000970519     C                   CLEAR                   O95CAP
177100970519     C                   CLEAR                   O95LNA
177200970519     C                   CLEAR                   O95ZNC
177300970519     C                   CLEAR                   O95ISO
177400970519     C                   CLEAR                   O95CTS
177500970519     C                   CLEAR                   O95CTE
177600970519     C                   CLEAR                   O95PII
177700970519     C                   CLEAR                   O95PID
177800970519     C                   CLEAR                   O95PIS
177900970519     C                   CLEAR                   O95GDD
178000980602     C                   CLEAR                   O95FL1
178100980602     C                   CLEAR                   O95FL2
178200980602     C                   CLEAR                   O95GF1
178300980602     C                   CLEAR                   O95GF2
178400980602     C                   CLEAR                   O95GF3
178500980602     C                   CLEAR                   O95GF4
178600970530     C                   CLEAR                   O95VER
178700970530     C                   CLEAR                   O95RDE
178800970530     C                   CLEAR                   O95RSC
178900970530     C                   CLEAR                   O95RDS
179000970519     C                   CLEAR                   O95TTC
179100970519     C                   CLEAR                   O95DTC
179200970521     C                   CLEAR                   O95TSP
179300970519     C                   CLEAR                   O950IN
179400970519     C                   CLEAR                   O950CN
179500970519     C                   CLEAR                   O951IN
179600970519     C                   CLEAR                   O951CN
179700970519     C                   CLEAR                   O95TIS
179800970519     C                   CLEAR                   O95TCS
179900970519     C                   CLEAR                   O95TLD
180000970521     C                   CLEAR                   O95TF1
180100970521     C                   CLEAR                   O95TF2
180200970519     C                   CLEAR                   O95IND
180300970519     C                   ENDSR
180400970604     C*------------------------------------------------------------------------*
180500970604     C*   AZZCAP - AZZERA TUTTI I CAMPI OUTPUT CAPPARIO
180600970604     C*------------------------------------------------------------------------*
180700970604     C     AZZCAP        BEGSR
180800970604     C                   CLEAR                   O95CLA
180900970604     C                   CLEAR                   O95CZC
181000970604     C                   CLEAR                   O95PRV
181100970604     C                   CLEAR                   O95PRA
181200970604     C                   CLEAR                   O95FLG
181300970604     C                   CLEAR                   O95FIT
181400970604     C                   CLEAR                   O95CIS
181500970604     C                   CLEAR                   O95CZF
181600970604     C                   CLEAR                   O95CLK
181700970604     C                   CLEAR                   O95CLM
181800970604     C                   CLEAR                   O95CLO
181900970604     C                   CLEAR                   O95CZO
182000970604     C                   CLEAR                   O95CCT
182100970604     C                   CLEAR                   O95CCE
182200970604     C                   CLEAR                   O95CTC
182300970604     C                   CLEAR                   O95CPI
182400970604     C                   CLEAR                   O95CPD
182500970604     C                   CLEAR                   O95CPS
182600970604     C                   CLEAR                   O95CGD
182700970604     C                   CLEAR                   O95ESE
182800970604     C                   CLEAR                   O95ESL
182900970604     C                   CLEAR                   O95CF1
183000970604     C                   CLEAR                   O95CF2
183100970604     C                   ENDSR
183200970604     C*------------------------------------------------------------------------*
183300970604     C*   AZZLOC - AZZERA TUTTI I CAMPI OUTPUT LOCALITA
183400970604     C*------------------------------------------------------------------------*
183500970604     C     AZZLOC        BEGSR
183600970604     C                   CLEAR                   O95LOC
183700970604     C                   CLEAR                   O95LIV
183800970604     C                   CLEAR                   O95DLV
183900970604     C                   CLEAR                   O95LLA
184000970604     C                   CLEAR                   O95LZC
184100970604     C                   CLEAR                   O95LIS
184200970604     C                   CLEAR                   O95LZF
184300970604     C                   CLEAR                   O95LLK
184400970604     C                   CLEAR                   O95LLM
184500970604     C                   CLEAR                   O95LLO
184600970604     C                   CLEAR                   O95LZO
184700970604     C                   CLEAR                   O95LKS
184800970604     C                   CLEAR                   O95LMS
184900970604     C                   CLEAR                   O95LLS
185000970604     C                   CLEAR                   O95LZS
185100970604     C                   CLEAR                   O95LK2
185200970604     C                   CLEAR                   O95LM2
185300970604     C                   CLEAR                   O95LL2
185400970604     C                   CLEAR                   O95LZ2
185500970604     C                   CLEAR                   O95LCT
185600970604     C                   CLEAR                   O95LCE
185700970604     C                   CLEAR                   O95LTC
185800970604     C                   CLEAR                   O95LPI
185900970604     C                   CLEAR                   O95LPD
186000970604     C                   CLEAR                   O95LPS
186100970604     C                   CLEAR                   O95LGD
186200970604     C                   CLEAR                   O95ESS
186300970604     C                   CLEAR                   O95ESV
186400970604     C                   CLEAR                   O95LF1
186500970604     C                   CLEAR                   O95LF2
186600970604     C                   ENDSR
186700970606     C*------------------------------------------------------------------------*
186800970606     C*   AZZECC - AZZERA CAMPI ECCEZIONI DS OUTPUT
186900970606     C*------------------------------------------------------------------------*
187000970606     C     AZZECC        BEGSR
187100970606     C                   CLEAR                   O95TFP
187200970606     C                   CLEAR                   O95ELA
187300970606     C                   CLEAR                   O95EZC
187400970606     C                   CLEAR                   O95ELO
187500970606     C                   CLEAR                   O95EZO
187600970606     C                   CLEAR                   O95ELS
187700970606     C                   CLEAR                   O95EZS
187800970703     C                   CLEAR                   O95EL2
187900970703     C                   CLEAR                   O95EZ2
188000970703     C                   ENDSR
188100970605     C*------------------------------------------------------------------------*
188200970605     C*   AZZSIN - AZZERA CAMPI SINONIMI DS OUTPUT
188300970605     C*------------------------------------------------------------------------*
188400970605     C     AZZSIN        BEGSR
188500970605     C                   CLEAR                   O95SIN
188600970605     C                   ENDSR
188700970605     C*------------------------------------------------------------------------*
188800970604     C*   MESSAG - ROUTINE GESTIONE MESSAGGI MA NON ERRORI
188900970604     C*------------------------------------------------------------------------*
189000970604     C     MESSAG        BEGSR
189100970604     C*
189200970605     C******************** MOVEL'S'       $ERR
189300970604     C     O95MSG        IFEQ      *BLANKS
189400970604     C                   MOVEL     $MSG          O95MSG
189500970604     C                   ENDIF
189600970604     C*
189700970604     C                   ENDSR
189800970516     C*------------------------------------------------------------------------*
189900970409     C*   ERRORE - ROUTINE GESTIONE ERRORI
190000970516     C*------------------------------------------------------------------------*
190100970409     C     ERRORE        BEGSR
190200970408     C*
190300970409     C                   MOVEL     'S'           $ERR
190400970604     C                   EXSR      AZZERA                                       *AZZ.TUTTI CAMPI OUT
190500970516     C                   MOVEL     $MSG          O95MSG
190600000419     C                   MOVE      $MSG          O95ERR
190700970408     C*
190800970408     C                   ENDSR
190900000419     C*------------------------------------------------------------------------*
191000000419     C* OPERAZIONI FINALI
191100000419     C*------------------------------------------------------------------------*
191200000419     C     FINSR         BEGSR
191300000419     C*
191400000419     C* LANCIA IN CHIUSURA I PGM (SOLO SE CHIAMATI)
191500000419IF  1C     $LPRDS        IFEQ      'S'                                          *PGM LANCIATO
191600000419     C                   CLEAR                   TISI98                         *AZZERA DS PARAMETRI
191700000419     C                   MOVEL     I95TLA        I98TLA
191800000419     C                   CALL      'TISI98R'
191900000419     C                   PARM                    TISI98
192000000419E   1C                   ENDIF
192100000419     C*
192200000419     C                   ENDSR
192300970516     C*------------------------------------------------------------------------*
192400930414     C*   RUTINZ - ROUTINE INIZIALE
192500970516     C*------------------------------------------------------------------------*
192600930414     C     RUTINZ        BEGSR
192700930414     C*
192800930414     C* PARAMETRI IN INPUT ALLA ROUTINE
192900930414     C     *ENTRY        PLIST
193000970516     C                   PARM                    TISI95
193100040528     C*
193200040528     C* CONVERTO SUBITO IN MAIUSCOLO LA LOCALITA IN INPUT
193300040528     C                   MOVEA     LEM           MINU             32
193400040528     C                   MOVEA     LET           MAIU             32
193500040528     C     MINU:MAIU     XLATE     I95LOC        I95LOC
193600970523     C*
193700970530     C* CHIAVE LETTURA AZCPR02L - COMPLETA
193800970523     C     KCPR          KLIST
193900970523     C                   KFLD                    CPRDDE
194000941227     C*
194100970515     C* CHIAVE LETTURA AZCPC01L - COMPLETA
194200970516     C     KCPC          KLIST
194300970523     C                   KFLD                    CPCVER
194400970515     C                   KFLD                    CPCNAR
194500970515     C                   KFLD                    CPCCAP
194600970516     C*
194700970516     C* CHIAVE LETTURA AZCPL01L - COMPLETA
194800970605     C     KCPL1         KLIST
194900970523     C                   KFLD                    CPLVER
195000970516     C                   KFLD                    CPLNAR
195100970516     C                   KFLD                    CPLCAP
195200970516     C                   KFLD                    CPLLOC
195300970605     C* CHIAVE LETTURA AZCPL02L - COMPLETA
195400970605     C     KCPL2         KLIST
195500970605     C                   KFLD                    CPLVER
195600970605     C                   KFLD                    CPLNAR
195700970605     C                   KFLD                    CPLLOC
195800970516     C*
195900970605     C* CHIAVE LETTURA AZCPS04L - COMPLETA
196000970605     C     KCPS4         KLIST
196100970523     C                   KFLD                    CPSVER
196200970516     C                   KFLD                    CPSNAR
196300970516     C                   KFLD                    CPSCAP
196400970516     C                   KFLD                    CPSSIN
196500970605     C* CHIAVE LETTURA AZCPS02L - COMPLETA
196600970605     C     KCPS2         KLIST
196700970605     C                   KFLD                    CPSVER
196800970605     C                   KFLD                    CPSNAR
196900970605     C                   KFLD                    CPSSIN
197000970627     C* CHIAVE LETTURA AZCPS01L - COMPLETA
197100970627     C     KCPS1         KLIST
197200970627     C                   KFLD                    CPSVER
197300970627     C                   KFLD                    CPSNAR
197400970627     C                   KFLD                    CPSCAP
197500970627     C                   KFLD                    CPSLOC
197600970627     C                   KFLD                    CPSSIN
197700970516     C*
197800970516     C* CHIAVE LETTURA AZCPE01L - COMPLETA
197900970519     C     KCPE          KLIST
198000970523     C                   KFLD                    CPEVER
198100970519     C                   KFLD                    CPENAR
198200970519     C                   KFLD                    CPECAP
198300970519     C                   KFLD                    CPETFP
198400970516     C*
198500970519     C* CHIAVE LETTURA AZCPT01L - COMPLETA
198600970519     C     KCPT          KLIST
198700970523     C                   KFLD                    CPTVER
198800970519     C                   KFLD                    CPTTTC
198900970519     C                   KFLD                    CPTTSP
199000930414     C*
199100930414     C* CHIAVE LETTURA TABEL00F - COMPLETA
199200930414     C     KEYTAB        KLIST
199300930414     C                   KFLD                    KTBUTE
199400930414     C                   KFLD                    KTBCOD
199500930414     C                   KFLD                    KTBKEY
199600930414     C*
199700930414     C* DEFINISCO CAMPI CHIAVE
199800930414     C     *LIKE         DEFINE    TBLKUT        KTBUTE
199900930414     C     *LIKE         DEFINE    TBLCOD        KTBCOD
200000930414     C     *LIKE         DEFINE    TBLKEY        KTBKEY
200100970605     C     *LIKE         DEFINE    CPCCAP        WCAP
200200970605     C     *LIKE         DEFINE    CPLLOC        WLOC
200300970605     C     *LIKE         DEFINE    CPSSIN        WSIN
200400970627     C     *LIKE         DEFINE    CPCCAP        WOCAP
200500970627     C     *LIKE         DEFINE    CPLLOC        WOLOC
200600970627     C     *LIKE         DEFINE    CPSSIN        WOSIN
200700970630     C     *LIKE         DEFINE    I95PRV        J95PRV
200800970630     C     *LIKE         DEFINE    I95TCN        J95TCN
200900971105     C     *LIKE         DEFINE    I95LOC        J95LOC
201000000727     C     *LIKE         DEFINE    I95LOC        Y95LOC
201100000727     C     *LIKE         DEFINE    CPSSIN        LSIN
201200930414     C*
201300930414     C* RECUPERO IL CODICE DELL'AZIENDA
201400930414     C                   Z-ADD     1             CODUT             1 0
201500970519     C                   MOVEL     'N'           $ERR              1
201600970409     C                   MOVEL     *BLANKS       $MSG             78
201700970605     C                   MOVEL     *BLANKS       WCAP
201800970605     C                   MOVEL     *BLANKS       WLOC
201900970605     C                   MOVEL     *BLANKS       WSIN
202000970703     C*
202100970703     C* IMPOSTO A ZERO I CAMPI NUMERICI IN INPUT NON OBBLIGATORI
202200970703     C                   MOVE      *ZEROS        W8                8
202300970703     C                   MOVE      I95LKG        W8
202400970703     C                   TESTN                   W8                   99
202500970703     C  N99              Z-ADD     *ZEROS        I95LKG
202600970703     C                   MOVE      *ZEROS        W8                8
202700970703     C                   MOVE      I95LMC        W8
202800970703     C                   TESTN                   W8                   99
202900970703     C  N99              Z-ADD     *ZEROS        I95LMC
203000970703     C                   MOVE      *ZEROS        W8                8
203100970703     C                   MOVE      I95TFP        W8
203200970703     C                   TESTN                   W8                   99
203300970703     C  N99              Z-ADD     *ZEROS        I95TFP
203400930414     C*
203500930414     C* AZZERO I CAMPI IN OUTPUT DEL PGM
203600970521     C                   EXSR      AZZERA
203700970630     C*
203800970630     C* SALVO CAMPI IN INPUT CHE POTREBBERO ESSERE MODIFICATI PER POTER REIMPOSTARLI
203900970630     C* ALLA FINE DEL PGM
204000970630     C                   MOVEL     I95PRV        J95PRV
204100970630     C                   MOVEL     I95TCN        J95TCN
204200930414     C*
204300930414     C                   ENDSR
204400970516     O*------------------------------------------------------------------------*
204500970516** MSG - MESSAGGI
204600970516TISI95R- Nazione inesistente.                                                1 1
204700970516TISI95R- CAP formalmente errato.                                             2 2
204800980602TISI95R- Richiesti tempi di consegna senza inserire il tipo servizio.        3 3
204900980602TISI95R- Richiesto controllo eccezioni senza AS di partenza.                 4 4
205000970630TISI95R- 5                                                                   5 5
205100970630TISI95R- 6                                                                   6 6
205200970630TISI95R- 7                                                                   7 7
205300970630TISI95R- CAP inesistente.                                                    8 8
205400970630TISI95R- Provincia diversa dalla provincia della località.                   9 9
205500970630TISI95R- Tipo controllo errato.                                              A10
205600970630TISI95R- Località inesistente.                                               B11
205700970630TISI95R- Provincia inesistente.                                              C12
205800970630TISI95R- Località inesistente in questo CAP.                                 D13
205900970630TISI95R- Provincia diversa da quella del CAP.                                E14
206000970630TISI95R- Tipo tempo/tipo servizio non trovati.                               F15
206100970630TISI95R- Versione cappario non trovata.                                      G16
206200970630TISI95R- Località esistente ma non univoca.                                  H17
206300970630TISI95R- Località obbligatoria.                                              I18
206400970630TISI95R- 19                                                                  L19
206500970630TISI95R- Errore generico.                                                    M20
206600150203** LET
206700040528ABCDEFGHIJKLMNOPQRSTUVWXYZOAUEEI
206800150203** LEM
206900040528abcdefghijklmnopqrstuvwxyzòàùèéì
207000150203** NUM
2071009705160123456789
