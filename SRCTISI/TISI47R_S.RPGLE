000100970526     H*------------------------------------------------------------------------*
000200070710     H* CAPPARIO - GESTIONE VERSIONI CAP                      - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     FTISI47D   CF   E             WORKSTN INFDS(DEVDS1)
000900970526     F                                     SFILE(SI47DMS:MSGULT)
001000970526     FAZCPR01L  UF A E           K DISK
001100970526     FAZCPL01L  UF   E           K DISK
001200970526     FAZCPS01L  UF   E           K DISK
001300970526     FAZCPE01L  UF   E           K DISK
001400970526     FAZCPT01L  UF   E           K DISK
001500970526     FAZCPC01L  UF   E           K DISK
001600140611     FAZCOS01L  UF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700970526     D*------------
005800970526     D* DS "XMSG" - GESTIONE MESSAGGI
005900970526     D*------------
006000970526     D DSMSG         E DS
006100970526     D*------------------
006200970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300970526     D*------------------
006400970526     D WLBDA8          DS                  INZ
006500970526     D  G08DAT                 1      8  0
006600970526     D  G08INV                 9     16  0
006700970526     D  G08ERR                17     17
006800970526     D  G08TGI                18     22  0
006900970526     D*------------
007000970526     D* DS DI PROCEDURA
007100970526     D*------------
007200970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
007300970526     D*------------
007400970526     D* ARCHITETTURA
007500970526     D*------------
007600970526     D KPJBA         E DS
007700970526     C*------------------------------------------------------------------------*
007800970526     C* MAIN LINES
007900970526     C*------------------------------------------------------------------------*
008000970526     C*
008100970526     C* NON CI SONO ERRORI IN ENTRATA
008200970526IF  1C     ERRL00        IFEQ      '0'
008300970526     C*
008400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008800970526     C*
008900970526     C* EMETTE FORMATO VIDEO
009000970526     C                   EXSR      BUTFMR
009100970526     C*
009200970526     C* GESTIONE TASTI FUNZIONALI
009300970526     C                   EXSR      FUNCON
009400970526E   2C                   ENDDO
009500970526     C*
009600970526     C* CI SONO ERRORI IN ENTRATA
009700970526X   1C                   ELSE
009800970526     C                   MOVEL     '1'           D00ERR
009900970526     C                   MOVEL     DSMSMS        D00MSG
010000970526E   1C                   ENDIF
010100970526     C* FINE PGM
010200970526     C                   SETON                                        LR
010300970526     C*------------------------------------------------------------------------*
010400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010500970526     C*------------------------------------------------------------------------*
010600970526     C     BUTFMR        BEGSR
010700970526     C*
010800970526     C* EMETTE LA TESTATA
010900970526     C                   WRITE     SI47D01
011000970526     C*
011100970526     C* SPEDISCE EVENTUALE MESSAGGIO
011200970526     C                   EXSR      SNDMSG
011300970526     C*
011400970526     C* EMETTE LE FUNZIONI
011500970526     C                   WRITE     SI47D02
011600970526     C*
011700970526     C* EMETTE IL VIDEO
011800970526     C                   EXFMT     SI47D03
011900970526     C*
012000970526     C* REIMPOSTA FLAG DI ERRORE
012100970526     C                   MOVEL     '0'           ERRL00
012200970526     C*
012300970526     C* REIMPOSTA INDICATORI DI ERRORE
012400970526     C                   SETOFF                                         5859
012500970526     C                   SETOFF                                       606162
012600970526     C                   SETOFF                                       636465
012700970526     C                   SETOFF                                       666768
012800970526     C                   SETOFF                                       697071
012900970526     C                   SETOFF                                       727374
013000970526     C                   SETOFF                                       757677
013100970526     C                   SETOFF                                       787980
013200970526     C                   SETOFF                                       818283
013300970526     C                   SETOFF                                       848586
013400970526     C                   SETOFF                                       878889
013500970526     C*
013600970526     C                   ENDSR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013900970526     C*------------------------------------------------------------------------*
014000970526     C     SNDMSG        BEGSR
014100970526     C*
014200970526     C* PULISCE IL SFILE MESSAGGI
014300970526     C                   MOVEL     '1'           *IN40
014400970526     C                   WRITE     SI47DMC
014500970526     C                   MOVEL     '0'           *IN40
014600970526     C*
014700970526     C* VISUALIZZA IL MESSAGGIO
014800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014900970526     C                   CALL      'XMSG'        XMSGPA
015000970526IF  1C     DSMSER        IFEQ      *ZEROS
015100970526     C                   MOVEL     DSMSKE        MSGKEY
015200970526E   1C                   ENDIF
015300970526     C                   Z-ADD     1             MSGULT
015400970526     C                   WRITE     SI47DMS
015500970526     C                   WRITE     SI47DMC
015600970526     C*
015700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015800970526     C                   MOVEL     *BLANKS       DSMSMS
015900970526     C*
016000970526     C                   ENDSR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* ENTGES - GESTIONE ENTER -> INVIO
016300970526     C*------------------------------------------------------------------------*
016400970526     C     ENTGES        BEGSR
016500970526     C*
016600970526     C* CONTROLLA I CAMPI IMMESSI
016700970526     C                   EXSR      DECODI
016800970526     C*
016900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017000970526IF  1C     DEPOP1        IFEQ      'O05'
017100970526     C                   MOVEL     '1'           FINL00
017200970526E   1C                   ENDIF
017300970526     C*
017400970526     C                   ENDSR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* F03GES - GESTIONE F03 -> FINE
017700970526     C*------------------------------------------------------------------------*
017800970526     C     F03GES        BEGSR
017900970526     C*
018000970526     C* IMPOSTA "PREMUTO F03"
018100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018200970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
018300970526     C*
018400970526     C                   ENDSR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     F06GES        BEGSR
018900970526     C*
019000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019100970526IF  1C     DEPOP1        IFEQ      'F06'
019200970526     C                   EXSR      INSERI
019300970526E   1C                   ENDIF
019400970526IF  1C     DEPOP1        IFEQ      'O02'
019500970526     C                   EXSR      MODIFI
019600970526E   1C                   ENDIF
019700970526IF  1C     DEPOP1        IFEQ      'O04'
019800970526     C                   EXSR      ANNULL
019900970526E   1C                   ENDIF
020000970526IF  1C     DEPOP1        IFEQ      'O07'
020100970526     C                   EXSR      RIPRIS
020200970526E   1C                   ENDIF
020300970526     C*
020400970526     C                   ENDSR
020500970528     C*------------------------------------------------------------------------*
020600970528     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
020700970528     C*------------------------------------------------------------------------*
020800970528     C     F08GES        BEGSR
020900970528     C*
021000970602     C                   Z-ADD     DDEVID        D00DRI                         *DATA RIFERIMENTO
021100970602     C                   Z-ADD     VIDVER        D00VER                         *VERSIONE
021200970528     C                   MOVEL     DEPOP0        WOP0
021300970528     C                   MOVEL     'L'           WOP0
021400970528     C                   MOVEL     'F08'         WOP1
021500970528     C                   MOVEL     'TISI30R'     WPGM              8
021600970528     C                   EXSR      LANPGM
021700970528     C*
021800970528     C                   ENDSR
021900970528     C*------------------------------------------------------------------------*
022000970528     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
022100970528     C*------------------------------------------------------------------------*
022200970528     C     F09GES        BEGSR
022300970528     C*
022400970602     C                   Z-ADD     DDEVID        D00DRI                         *DATA RIFERIMENTO
022500970602     C                   Z-ADD     VIDVER        D00VER                         *VERSIONE
022600970528     C                   MOVEL     DEPOP0        WOP0
022700970528     C                   MOVEL     'E'           WOP0
022800970528     C                   MOVEL     'F09'         WOP1
022900970528     C                   MOVEL     'TISI11R'     WPGM              8
023000970528     C                   EXSR      LANPGM
023100970528     C*
023200970528     C                   ENDSR
023300970528     C*------------------------------------------------------------------------*
023400970528     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
023500970528     C*------------------------------------------------------------------------*
023600970528     C     F10GES        BEGSR
023700970528     C*
023800970602     C                   Z-ADD     DDEVID        D00DRI                         *DATA RIFERIMENTO
023900970602     C                   Z-ADD     VIDVER        D00VER                         *VERSIONE
024000970528     C                   MOVEL     DEPOP0        WOP0
024100970528     C                   MOVEL     'T'           WOP0
024200970528     C                   MOVEL     'F10'         WOP1
024300970528     C                   MOVEL     'TISI41R'     WPGM              8
024400970528     C                   EXSR      LANPGM
024500970528     C*
024600970528     C                   ENDSR
024700970603     C*------------------------------------------------------------------------*
024800970528     C* F11GES - GESTIONE F11 -> CAP
024900970528     C*------------------------------------------------------------------------*
025000970528     C     F11GES        BEGSR
025100970528     C*
025200970602     C                   Z-ADD     DDEVID        D00DRI                         *DATA RIFERIMENTO
025300970602     C                   Z-ADD     VIDVER        D00VER                         *VERSIONE
025400970528     C                   MOVEL     DEPOP0        WOP0
025500970528     C                   MOVEL     'C'           WOP0
025600970528     C                   MOVEL     'F11'         WOP1
025700970528     C                   MOVEL     'TISI02R'     WPGM              8
025800970528     C                   EXSR      LANPGM
025900970528     C*
026000970528     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* F12GES - GESTIONE F12 -> RITORNO
026300970526     C*------------------------------------------------------------------------*
026400970526     C     F12GES        BEGSR
026500970526     C*
026600970526     C* IMPOSTA "PREMUTO F12"
026700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026800970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
026900970526     C*
027000970526     C                   ENDSR
027100970603     C*------------------------------------------------------------------------*
027200970603     C* F13GES - GESTIONE F13 -> SINONIMI
027300970603     C*------------------------------------------------------------------------*
027400970603     C     F13GES        BEGSR
027500970603     C*
027600970603     C                   Z-ADD     DDEVID        D00DRI                         *DATA RIFERIMENTO
027700970603     C                   Z-ADD     VIDVER        D00VER                         *VERSIONE
027800970603     C                   MOVEL     DEPOP0        WOP0
027900970603     C                   MOVEL     'S'           WOP0
028000970603     C                   MOVEL     'F13'         WOP1
028100970603     C                   MOVEL     'TISI35R'     WPGM              8
028200970603     C                   EXSR      LANPGM
028300970603     C*
028400970603     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028700970526     C*------------------------------------------------------------------------*
028800970526     C     F24GES        BEGSR
028900970526     C*
029000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029100970526     C*
029200970526     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
029500970526     C*------------------------------------------------------------------------*
029600970526     C     INSERI        BEGSR
029700970526     C*
029800970526     C* CONTROLLA I CAMPI IMMESSI
029900970526     C                   EXSR      DECODI
030000970526     C*
030100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030300970526     C                   Z-ADD     VIDVER        CPRVER                         *CHIAVE
030400970526     C                   EXSR      IMPCPR                                       *ALTRI CAMPI
030500970526     C                   Z-ADD     *ZEROS        CPRDTR                         *DATA TRASMISSIONE
030600970526     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
030700970526     C                   MOVEL     *BLANKS       CPRATB                         *ANNULLAMENTO
030800970526     C                   WRITE     AZCPR000
030900970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
031000970602     C*
031100970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
031200970602     C                   EXSR      AZZVID
031300970526E   1C                   ENDIF
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* MODIFI - OPERAZIONI DI MODIFICA
031800970526     C*------------------------------------------------------------------------*
031900970526     C     MODIFI        BEGSR
032000970526     C*
032100970526     C* CONTROLLA I CAMPI IMMESSI
032200970526     C                   EXSR      DECODI
032300970526     C*
032400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600970526     C                   EXSR      LETCPR
032700970526     C*
032800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033000970526     C                   EXSR      IMPCPR                                       *IMPOSTA I CAMPI
033100970526     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
033200970526     C                   MOVEL     *BLANKS       CPRATB                         *ANNULLAMENTO
033300970526     C                   UPDATE    AZCPR000
033400970526     C                   MOVEL     MSG(3)        D00MSG
033500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033600970526E   2C                   ENDIF
033700970526E   1C                   ENDIF
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
034200970526     C*------------------------------------------------------------------------*
034300970526     C     ANNULL        BEGSR
034400970526     C*
034500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
034600970526     C                   EXSR      LETCPR
034700970526     C*
034800970526     C* ANNULLA LE LOCALITA'
034900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000970526     C                   Z-ADD     CPRVER        KCLVER
035100970526     C     KEYCPL        SETLL     AZCPL01L                           99
035200970526     C     KEYCPL        READE     AZCPL01L                               99
035300970526DO  2C     *IN99         DOWEQ     *OFF
035400000602IF  3C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
035500000602IF  4C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
035600000602     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
035700000602     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
035800000602     C                   UPDATE    AZCPL000                                     *ANNULLA
035900000602X   4C                   ELSE                                                   *MAI TRASMESSO
036000000602     C                   DELETE    AZCPL000                                     *CANCELLA
036100000602E   4C                   ENDIF
036200000602E   3C                   ENDIF
036300970526     C     KEYCPL        READE     AZCPL01L                               99
036400970526E   2C                   ENDDO                                                  *FINE LOCALITA'
036500970526     C*
036600970526     C* ANNULLA I SINONIMI
036700970526     C                   Z-ADD     CPRVER        KCSVER
036800970526     C     KEYCPS        SETLL     AZCPS01L                           99
036900970526     C     KEYCPS        READE     AZCPS01L                               99
037000970526DO  2C     *IN99         DOWEQ     *OFF
037100970526IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
037200970526IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
037300970526     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
037400970526     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
037500970526     C                   UPDATE    AZCPS000                                     *ANNULLA
037600970526X   4C                   ELSE                                                   *MAI TRASMESSO
037700970526     C                   DELETE    AZCPS000                                     *CANCELLA
037800970526E   4C                   ENDIF
037900970526E   3C                   ENDIF
038000970526     C     KEYCPS        READE     AZCPS01L                               99
038100970526E   2C                   ENDDO                                                  *FINE SINONIMI
038200970526     C*
038300000602     C* ANNULLA LE ECCEZIONI
038400970526     C                   Z-ADD     CPRVER        KCEVER
038500970526     C     KEYCPE        SETLL     AZCPE01L                           99
038600970526     C     KEYCPE        READE     AZCPE01L                               99
038700970526DO  2C     *IN99         DOWEQ     *OFF
038800000602IF  3C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
038900000602IF  4C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
039000000602     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
039100000602     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
039200000602     C                   UPDATE    AZCPE000                                     *ANNULLA
039300000602X   4C                   ELSE                                                   *MAI TRASMESSO
039400000602     C                   DELETE    AZCPE000                                     *CANCELLA
039500000602E   4C                   ENDIF
039600000602E   3C                   ENDIF
039700970526     C     KEYCPE        READE     AZCPE01L                               99
039800970526E   2C                   ENDDO                                                  *FINE SINONIMI
039900970526     C*
040000970526     C* ANNULLA I TEMPI
040100970526     C                   Z-ADD     CPRVER        KCTVER
040200970526     C     KEYCPT        SETLL     AZCPT01L                           99
040300970526     C     KEYCPT        READE     AZCPT01L                               99
040400970526DO  2C     *IN99         DOWEQ     *OFF
040500970526IF  3C     CPTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
040600970526IF  4C     CPTDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
040700970526     C                   MOVEL     'A'           CPTATB                         *FLAG ANNULLAMENTO
040800970526     C                   MOVEL     *BLANKS       CPTFTR                         *FLAG TRASMISSIONE
040900970526     C                   UPDATE    AZCPT000                                     *ANNULLA
041000970526X   4C                   ELSE                                                   *MAI TRASMESSO
041100970526     C                   DELETE    AZCPT000                                     *CANCELLA
041200970526E   4C                   ENDIF
041300970526E   3C                   ENDIF
041400970526     C     KEYCPT        READE     AZCPT01L                               99
041500970526E   2C                   ENDDO                                                  *FINE SINONIMI
041600970526     C*
041700970526     C* ANNULLA I CAP
041800970526     C                   Z-ADD     CPRVER        KCCVER
041900970526     C     KEYCPC        SETLL     AZCPC01L                           99
042000970526     C     KEYCPC        READE     AZCPC01L                               99
042100970526DO  2C     *IN99         DOWEQ     *OFF
042200000602IF  3C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
042300000602IF  4C     CPCDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
042400000602     C                   MOVEL     'A'           CPCATB                         *FLAG ANNULLAMENTO
042500000602     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
042600000602     C                   UPDATE    AZCPC000                                     *ANNULLA
042700000602X   4C                   ELSE                                                   *MAI TRASMESSO
042800000602     C                   DELETE    AZCPC000                                     *CANCELLA
042900000602E   4C                   ENDIF
043000000602E   3C                   ENDIF
043100970526     C     KEYCPC        READE     AZCPC01L                               99
043200970526E   2C                   ENDDO                                                  *FINE SINONIMI
043300140611     C*
043400140611     C* ANNULLA GLI ORARI
043500140611     C                   Z-ADD     CPRVER        KCOVER
043600140611     C     KEYCOS        SETLL     AZCOS01L                           99
043700140611     C     KEYCOS        READE     AZCOS01L                               99
043800140611DO  2C     *IN99         DOWEQ     *OFF
043900140611IF  3C     COSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
044000140611     C                   DELETE    AZCOS000                                     *CANCELLA
044100140611E   3C                   ENDIF
044200140611     C     KEYCOS        READE     AZCOS01L                               99
044300140611E   2C                   ENDDO                                                  *FINE SINONIMI
044400970526     C*
044500970526     C* ANNULLA LA VERSIONE
044600970526IF  2C     CPRATB        IFEQ      *BLANKS                                      *NO ANNULLATI
044700970526IF  3C     CPRDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
044800970526     C                   MOVEL     'A'           CPRATB                         *FLAG ANNULLAMENTO
044900970526     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
045000970526     C                   UPDATE    AZCPR000                                     *ANNULLA
045100970526X   3C                   ELSE                                                   *MAI TRASMESSO
045200970526     C                   DELETE    AZCPR000                                     *CANCELLA
045300970526E   3C                   ENDIF
045400970526E   2C                   ENDIF
045500970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
045600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
045700970526E   1C                   ENDIF
045800970526     C*
045900970526     C                   ENDSR
046000970526     C*------------------------------------------------------------------------*
046100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
046200970526     C*------------------------------------------------------------------------*
046300970526     C     RIPRIS        BEGSR
046400970526     C*
046500970526     C* CONTROLLA I CAMPI IMMESSI
046600970526     C                   EXSR      DECODI
046700970526     C*
046800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
046900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047000970526     C                   EXSR      LETCPR
047100970526     C*
047200970526     C* IMPOSTA IL RECORD DA SCRIVERE
047300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047400970526     C                   MOVEL     *BLANKS       CPRATB                         *FLAG ANNULLAMENTO
047500970526     C                   MOVEL     *BLANKS       CPRFTR                         *FLAG TRASMISSIONE
047600970526     C                   UPDATE    AZCPR000                                     *ANNULLA
047700970526E   2C                   ENDIF
047800970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
047900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
048000970526E   1C                   ENDIF
048100970526     C*
048200970526     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400970526     C* LETCPR - LEGGE IL RECORD PERL'AGGIORNAMENTO
048500970526     C*------------------------------------------------------------------------*
048600970526     C     LETCPR        BEGSR
048700970526     C*
048800980127     C                   Z-ADD     D00VER        KCRVER
048900970526     C     KEYCPR        CHAIN     AZCPR01L                           9998
049000970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
049100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
049200970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
049300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
049400970526E   1C                   ENDIF
049500970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
049600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
049700970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
049800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
049900970526E   1C                   ENDIF
050000970526     C*
050100970526     C                   ENDSR
050200970528     C*------------------------------------------------------------------------*
050300970528     C* LANPGM - LANCIA UN PROGRAMMA
050400970528     C*------------------------------------------------------------------------*
050500970528     C     LANPGM        BEGSR
050600970528     C*
050700970528     C* IMPOSTA LA DS DI PROCEDURA
050800970528     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
050900970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
051000970528     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
051100970528     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
051200970528     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
051300970528     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
051400970602     C                   Z-ADD     DDRI          D00DRI                         *DATA RIFERIMENTO
051500970602     C                   Z-ADD     DVER          D00VER                         *VERSIONE
051600970530     C                   Z-ADD     *ZEROS        D00DDE                         *ALTRI CAMPI
051700970530     C                   MOVEL     *BLANKS       D00NAR
051800970530     C                   MOVEL     *BLANKS       D00CAP
051900970530     C                   Z-ADD     *ZEROS        D00TTC
052000970530     C                   MOVEL     *BLANKS       D00TSP
052100970530     C                   Z-ADD     *ZEROS        D00TFP
052200970530     C                   MOVEL     *BLANKS       D00LOC
052300970603     C                   MOVEL     *BLANKS       D00SIN
052400970530     C                   MOVEL     *BLANKS       D00EPA
052500970530     C                   Z-ADD     *ZEROS        D00ETP
052600970530     C                   Z-ADD     *ZEROS        D00ETA
052700970530     C                   Z-ADD     *ZEROS        D00EDD
052800970530     C                   MOVEL     *BLANKS       D00SPT
052900970530     C                   Z-ADD     *ZEROS        D00SLP
053000970530     C                   Z-ADD     *ZEROS        D00SLA
053100970530     C                   MOVEL     *BLANKS       D00STS
053200970530     C                   Z-ADD     *ZEROS        D00SDD
053300970528     C*
053400970528     C                   CALL      WPGM
053500970612     C                   PARM                    KPJBA
053600970612     C                   PARM                    DSSI00
053700970528     C*
053800970528     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
053900970528IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
054000970528     C                   MOVEL     '1'           ERRL00                         *ERRORE
054100970528     C                   MOVEL     '1'           F03L00                         *FINE PGM
054200970528X   1C                   ELSE                                                   *NON PREMUTO F03
054300970528     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
054400970528E   1C                   ENDIF
054500970528     C*
054600970528     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
054900970526     C*------------------------------------------------------------------------*
055000970526     C     DECODI        BEGSR
055100970526     C*---
055200970526     C* VERSIONE
055300970526     C*---
055400970526IF  1C     ERRL00        IFEQ      '0'
055500970526IF  2C     VIDVER        IFEQ      *ZEROS
055600970526     C                   MOVEL     '1'           ERRL00
055700970526     C                   MOVEL     MSG(14)       DSMSMS
055800970526     C                   SETON                                        60
055900970526E   2C                   ENDIF
056000970526E   1C                   ENDIF
056100970526     C* INSERIMENTO -> RECORD INESISTENTE
056200970526IF  1C     ERRL00        IFEQ      '0'
056300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
056400970526     C                   Z-ADD     VIDVER        KCRVER
056500970526     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
056600970526IF  3C     *IN99         IFEQ      *OFF
056700970526     C                   MOVEL     '1'           ERRL00
056800970528     C                   MOVEL     MSG(8)        DSMSMS
056900970526     C                   SETON                                        60
057000970526E   3C                   ENDIF
057100970526E   2C                   ENDIF
057200970526E   1C                   ENDIF
057300970526     C*---
057400970526     C* DECORRENZA
057500970526     C*---
057600970526     C     *LIKE         DEFINE    VIDDDE        DDEVID
057700970526IF  1C     ERRL00        IFEQ      '0'
057800970526     C                   Z-ADD     VIDDDE        WDA8
057900970526     C                   Z-ADD     *ZEROS        DA8W
058000970526     C                   EXSR      CHKDA8
058100970526     C                   Z-ADD     WDA8          VIDDDE
058200970526     C                   Z-ADD     DA8W          DDEVID
058300970526IF  2C     ERRL00        IFEQ      '1'
058400970526     C                   SETON                                        61
058500970526     C                   MOVEL     MSG(9)        DSMSMS
058600970526E   2C                   ENDIF
058700970526E   1C                   ENDIF
058800970526     C*---
058900970526     C* SCADENZA
059000970526     C*---
059100970526     C     *LIKE         DEFINE    VIDDSC        DSCVID
059200970526IF  1C     ERRL00        IFEQ      '0'
059300970526     C                   Z-ADD     VIDDSC        WDA8
059400970526     C                   Z-ADD     *ZEROS        DA8W
059500970526     C                   EXSR      CHKDA8
059600970526     C                   Z-ADD     WDA8          VIDDSC
059700970526     C                   Z-ADD     DA8W          DSCVID
059800970526IF  2C     ERRL00        IFEQ      '1'
059900970526     C                   SETON                                        62
060000970526     C                   MOVEL     MSG(9)        DSMSMS
060100970526E   2C                   ENDIF
060200970526E   1C                   ENDIF
060300970526     C* SCADENZA > DECORRENZA
060400970526IF  1C     ERRL00        IFEQ      '0'
060500970526IF  2C     DSCVID        IFLT      DDEVID
060600970526     C                   SETON                                        62
060700970526     C                   MOVEL     MSG(10)       DSMSMS
060800970526     C                   MOVEL     '1'           ERRL00
060900970526E   2C                   ENDIF
061000970526E   1C                   ENDIF
061100970526     C* SCADENZA >= DATA CORRENTE
061200970526IF  1C     ERRL00        IFEQ      '0'
061300970526IF  2C     DEPOP1        IFEQ      'O02'
061400970526     C     DEPOP1        OREQ      'O07'
061500970528     C     DEPOP1        OREQ      'F06'
061600970526IF  3C     DSCVID        IFLT      DATCOR
061700970526     C                   SETON                                        62
061800970526     C                   MOVEL     MSG(11)       DSMSMS
061900970526     C                   MOVEL     '1'           ERRL00
062000970526E   3C                   ENDIF
062100970526E   2C                   ENDIF
062200970526E   1C                   ENDIF
062300970526     C*---
062400970526     C* VERSIONE + DECORRENZA + SCADENZA
062500970526     C*---
062600970526     C* ACCAVALLAMENTO DATI
062700970526IF  1C     ERRL00        IFEQ      '0'
062800970526     C     *LOVAL        SETLL     AZCPR01L
062900970526     C                   READ(N)   AZCPR01L                               99
063000970526DO  2C     *IN99         DOWEQ     *OFF
063100970526IF  3C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
063200970526     C     DDEVID        ANDNE     CPRDDE                                       *RECORD IN LINEA
063300970526     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
063400970526IF  4C     CPRATB        IFEQ      *BLANKS
063500970526IF  5C     DDEVID        IFGE      CPRDDE                                       *DECORRENZA
063600970526     C     DDEVID        ANDLE     CPRDSC
063700970526     C     DSCVID        ORGE      CPRDDE                                       *SCADENZA
063800970526     C     DSCVID        ANDLE     CPRDSC
063900970526     C     CPRDDE        ORGE      DDEVID                                       *DECORRENZA
064000970526     C     CPRDSC        ANDLE     DSCVID
064100970526     C                   SETON                                        606162
064200970526     C                   MOVEL     MSG(15)       DSMSMS
064300970526     C                   MOVEL     '1'           ERRL00
064400970526     C                   LEAVE
064500970526E   5C                   ENDIF
064600970526E   4C                   ENDIF
064700970526E   3C                   ENDIF
064800970526     C                   READ(N)   AZCPR01L                               99
064900970526E   2C                   ENDDO
065000970526E   1C                   ENDIF
065100970526     C*---
065200970526     C* DESCRIZIONE
065300970526     C*---
065400970526IF  1C     ERRL00        IFEQ      '0'
065500970526IF  2C     VIDDES        IFEQ      *BLANKS
065600970526     C                   MOVEL     '1'           ERRL00
065700970526     C                   MOVEL     MSG(13)       DSMSMS
065800970526     C                   SETON                                        63
065900970526E   2C                   ENDIF
066000970526E   1C                   ENDIF
066100970526     C*
066200970526     C                   ENDSR
066300970526     C*------------------------------------------------------------------------*
066400970526     C* CHKDA8 - CONTROLLA LA DATA (8)
066500970526     C*------------------------------------------------------------------------*
066600970526     C     CHKDA8        BEGSR
066700970526     C*
066800970526     C     *LIKE         DEFINE    VIDDDE        WDA8
066900970526     C     *LIKE         DEFINE    VIDDDE        DA8W
067000970526     C*
067100970526     C                   Z-ADD     WDA8          G08DAT
067200970526     C                   Z-ADD     *ZEROS        G08INV
067300970526     C                   MOVEL     '0'           G08ERR
067400970526     C                   CALL      'XSRDA8'
067500970526     C                   PARM                    WLBDA8
067600970526     C                   Z-ADD     G08DAT        WDA8
067700970526     C                   Z-ADD     G08INV        DA8W
067800970526IF  1C     G08ERR        IFEQ      '1'
067900970526     C                   MOVEL     '1'           ERRL00
068000970526E   1C                   ENDIF
068100970526     C*
068200970526     C                   ENDSR
068300970526     C*------------------------------------------------------------------------*
068400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
068500970526     C*------------------------------------------------------------------------*
068600970526     C     IREVID        BEGSR
068700970526     C* CAMPI
068800970526     C                   Z-ADD     CPRVER        VIDVER
068900970526     C                   Z-ADD     CPRDDE        G08INV
069000970526     C                   Z-ADD     *ZEROS        G08DAT
069100970526     C                   MOVEL     '3'           G08ERR
069200970526     C                   CALL      'XSRDA8'
069300970526     C                   PARM                    WLBDA8
069400970526     C                   Z-ADD     G08DAT        VIDDDE
069500970526     C                   Z-ADD     CPRDSC        G08INV
069600970526     C                   Z-ADD     *ZEROS        G08DAT
069700970526     C                   MOVEL     '3'           G08ERR
069800970526     C                   CALL      'XSRDA8'
069900970526     C                   PARM                    WLBDA8
070000970526     C                   Z-ADD     G08DAT        VIDDSC
070100970526     C                   MOVEL     CPRDES        VIDDES
070200970526     C*
070300970526     C                   ENDSR
070400970526     C*------------------------------------------------------------------------*
070500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
070600970526     C*------------------------------------------------------------------------*
070700970526     C     AZZVID        BEGSR
070800970526     C*
070900970526     C                   Z-ADD     *ZEROS        VIDVER
071000970526     C                   Z-ADD     *ZEROS        VIDDDE
071100970526     C                   Z-ADD     *ZEROS        VIDDSC
071200970526     C                   MOVEL     *BLANKS       VIDDES
071300970526     C*
071400970526     C                   ENDSR
071500970526     C*------------------------------------------------------------------------*
071600970526     C* IMPCPR - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
071700970526     C*------------------------------------------------------------------------*
071800970526     C     IMPCPR        BEGSR
071900970526     C*
072000970526     C                   Z-ADD     DDEVID        CPRDDE
072100970526     C                   Z-ADD     DSCVID        CPRDSC
072200970526     C                   MOVEL     VIDDES        CPRDES
072300970526     C*
072400970526     C                   ENDSR
072500970526     C*------------------------------------------------------------------------*
072600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
072700970526     C*------------------------------------------------------------------------*
072800970526     C     IMPFUN        BEGSR
072900970526     C*
073000970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
073100970526IF  1C     DEPOP1        IFEQ      'O05'
073200970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
073300970526     C                   MOVEL     'N'           FUDATT
073400970526     C                   MOVEL     FUDRIG        FUD(6)
073500970526E   1C                   ENDIF
073600970526     C*
073700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
073800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
073900970526     C                   EXSR      FUNGES
074000970526     C*
074100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
074200970526     C*---
074300970526     C* 90 -> PROTEZIONE CHIAVE
074400970526     C* 91 -> PROTEZIONE TOTALE
074500970526     C*---
074600970526IF  1C     DEPOP0        IFEQ      'R02'                                        G E S T I O N E
074700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
074800970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
074900970526     C                   SETOFF                                       91
075000970526E   2C                   ENDIF
075100970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
075200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
075300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
075400970526E   2C                   ENDIF
075500970526     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
075600970526IF  2C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
075700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
075800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
075900970526     C                   SETON                                        91
076000970526E   2C                   ENDIF
076100970526X   1C                   ELSE                                                   *I N T E R R O G A
076200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
076300970526     C                   SETON                                        91
076400970526E   1C                   ENDIF
076500970526     C*
076600970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
076700970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
076800970526     C                   Z-ADD     D00VER        KCRVER
076900970526     C     KEYCPR        CHAIN(N)  AZCPR01L                           99
077000970526IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
077100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
077200970526     C                   MOVEL     MSG(6)        DSMSMS
077300970526X   2C                   ELSE
077400970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
077500980128IF  3C     DEPOP1        IFEQ      'O05'
077600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
077700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
077800980128E   3C                   ENDIF
077900970526E   2C                   ENDIF
078000970526X   1C                   ELSE                                                   *INSERIMENTO
078100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
078200970526E   1C                   ENDIF
078300970526     C*
078400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
078500970526IF  1C     DEPOP1        IFEQ      'O02'
078600970526     C                   MOVEL     DEC(1)        VIDFUN
078700970526E   1C                   ENDIF
078800970526IF  1C     DEPOP1        IFEQ      'O04'
078900970526     C                   MOVEL     DEC(2)        VIDFUN
079000970526E   1C                   ENDIF
079100970526IF  1C     DEPOP1        IFEQ      'O05'
079200970526     C                   MOVEL     DEC(3)        VIDFUN
079300970526E   1C                   ENDIF
079400970526IF  1C     DEPOP1        IFEQ      'O07'
079500970526     C                   MOVEL     DEC(4)        VIDFUN
079600970526E   1C                   ENDIF
079700970526IF  1C     DEPOP1        IFEQ      'F06'
079800970526     C                   MOVEL     DEC(5)        VIDFUN
079900970526E   1C                   ENDIF
080000970526     C*
080100970526     C                   ENDSR
080200970526     C*------------------------------------------------------------------------*
080300970526     C* *INZSR - OPERAZIONI INIZIALI
080400970526     C*------------------------------------------------------------------------*
080500970526     C     *INZSR        BEGSR
080600970526     C*---
080700970526     C* RICEVIMENTO PARAMETRI
080800970526     C*---
080900970526     C     *ENTRY        PLIST
081000970612     C                   PARM                    KPJBA
081100970612     C                   PARM                    DSSI00
081200970526     C*
081300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
081400970526     C     XMSGPA        PLIST
081500970526     C                   PARM                    DSMSMS
081600970526     C                   PARM                    DSMSPG
081700970526     C                   PARM                    DSMSKE
081800970526     C                   PARM                    DSMSER
081900970526     C*
082000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
082100970526     C                   CALL      'XKEY'
082200970526     C                   PARM                    DSKEY
082300970526     C*---
082400970526     C* VARIABILI NON RIFERITE AL DATA BASE
082500970526     C*---
082600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
082700970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
082800970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
082900970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
083000970528     C     *LIKE         DEFINE    D00OP1        WOP1
083100970602     C     *LIKE         DEFINE    D00DRI        DDRI
083200970602     C     *LIKE         DEFINE    D00VER        DVER
083300970526     C*---
083400970526     C* VARIABILI DI CONTROLLO
083500970526     C*---
083600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
083700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
083800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
083900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
084000970526     C*
084100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
084200970526     C                   MOVEL     'TISI47R'     DSMSPG
084300970526     C                   MOVEL     'TISI47R'     MSGQUE
084400970526     C                   MOVEL     *BLANKS       MSGKEY
084500970526     C                   MOVEL     *BLANKS       DSMSMS
084600970526     C*---
084700970526     C* VARIABILI RIFERITE AL DATA BASE
084800970526     C*---
084900970526     C     *LIKE         DEFINE    CPRVER        KCRVER                         *AZCPR00F
085000970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
085100970526     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
085200970526     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
085300970526     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
085400970526     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
085500140611     C     *LIKE         DEFINE    COSVER        KCOVER                         *AZCOS00F
085600970526     C*---
085700970526     C* CHIAVI DI LETTURA
085800970526     C*---
085900970526     C* LETTURA AZCPR01L - COMPLETA
086000970526     C     KEYCPR        KLIST
086100970526     C                   KFLD                    KCRVER                         *VERSIONE
086200970526     C* LETTURA AZCPC01L - PARZIALE
086300970526     C     KEYCPC        KLIST
086400970526     C                   KFLD                    KCCVER                         *VERSIONE
086500970526     C* LETTURA AZCPE01L - PARZIALE
086600970526     C     KEYCPE        KLIST
086700970526     C                   KFLD                    KCEVER                         *VERSIONE
086800970526     C* LETTURA AZCPT01L - PARZIALE
086900970526     C     KEYCPT        KLIST
087000970526     C                   KFLD                    KCTVER                         *VERSIONE
087100970526     C* LETTURA AZCPL01L - PARZIALE
087200970526     C     KEYCPL        KLIST
087300970526     C                   KFLD                    KCLVER                         *VERSIONE
087400970526     C* LETTURA AZCPS01L - PARZIALE
087500970526     C     KEYCPS        KLIST
087600970526     C                   KFLD                    KCSVER                         *VERSIONE
087700140611     C* LETTURA AZCOS01L - PARZIALE
087800140611     C     KEYCOS        KLIST
087900140611     C                   KFLD                    KCOVER                         *VERSIONE
088000970526     C*
088100970526     C* CALCOLA LA DATA CORRENTE
088200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
088300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
088400970526     C                   Z-ADD     WN8           G08DAT
088500970526     C                   Z-ADD     *ZEROS        G08INV
088600970526     C                   MOVEL     '0'           G08ERR
088700970526     C                   CALL      'XSRDA8'
088800970526     C                   PARM                    WLBDA8
088900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
089000970526     C*
089100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
089200970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
089300970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
089400970526     C*
089500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
089600970526     C                   EXSR      IMPFUN
089700970526     C*
089800970526     C                   ENDSR
089900970526     C*------------------------------------------------------------------------*
090000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
090100970526     C*------------------------------------------------------------------------*
090200970526     C     FUNGES        BEGSR
090300970526     C*
090400970526     C* PULISCO CAMPI DI LAVORO
090500970526     C                   Z-ADD     0             W                 5 0
090600970526     C*
090700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
090900970526     C     1             DO        24            I
091000970526     C                   MOVEL     FUD(I)        FUDRIG
091100970526     C*
091200970526     C* PULIZIA FUNZIONI PRECEDENTI
091300970526     C                   MOVEL     *BLANKS       FUV(I)
091400970526     C*
091500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
091600970526     C* (SALTA F24)
091700970526     C     FUDATT        IFEQ      'S'
091800970526     C     FUDFUN        ANDNE     '24'
091900970526     C                   ADD       1             W
092000970526     C                   MOVEL     FUDDES        FUV(W)
092100970526     C                   END
092200970526     C                   END                                                    DO WHILE
092300970526     C*
092400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
092500970526     C* ALTRIMENTI DISATTIVO F24
092600970526     C     1             DO        24            I
092700970526     C                   MOVEL     FUD(I)        FUDRIG
092800970526     C*
092900970526     C     FUDFUN        IFEQ      '24'
093000970526     C     W             IFGT      6
093100970526     C                   MOVEL     'S'           FUDATT
093200970526     C                   ELSE
093300970526     C                   MOVEL     *BLANKS       FUDATT
093400970526     C                   END                                                    W GT 8
093500970526     C                   MOVEL     FUDRIG        FUD(I)
093600970526     C                   END
093700970526     C                   END                                                    DO WHILE
093800970526     C*
093900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
094000970526     C     W             DIV       6             I
094100970526     C                   MVR                     Z
094200970526     C*
094300970526     C     Z             IFNE      0
094400970526     C     I             ADD       1             Z
094500970526     C                   ELSE
094600970526     C                   Z-ADD     I             Z                 5 0
094700970526     C                   END
094800970526     C*
094900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
095000970526     C                   ADD       1             FUNGRU
095100970526     C     FUNGRU        IFGT      Z
095200970526     C     FUNGRU        ORLT      1
095300970526     C                   Z-ADD     1             FUNGRU
095400970526     C                   END
095500970526     C*
095600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
095700970526     C     FUNGRU        IFEQ      1
095800970526     C                   MOVEL     FUV(1)        FUN001
095900970526     C                   MOVEL     FUV(2)        FUN002
096000970526     C                   MOVEL     FUV(3)        FUN003
096100970526     C                   MOVEL     FUV(4)        FUN004
096200970526     C                   MOVEL     FUV(5)        FUN005
096300970526     C                   MOVEL     FUV(6)        FUN006
096400970526     C                   END
096500970526     C*
096600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
096700970526     C     FUNGRU        IFEQ      2
096800970526     C                   MOVEL     FUV(7)        FUN001
096900970526     C                   MOVEL     FUV(8)        FUN002
097000970526     C                   MOVEL     FUV(9)        FUN003
097100970526     C                   MOVEL     FUV(10)       FUN004
097200970526     C                   MOVEL     FUV(11)       FUN005
097300970526     C                   MOVEL     FUV(12)       FUN006
097400970526     C                   END
097500970526     C*
097600970526     C* TERZO GRUPPO DI 6 FUNZIONI
097700970526     C     FUNGRU        IFEQ      3
097800970526     C                   MOVEL     FUV(13)       FUN001
097900970526     C                   MOVEL     FUV(14)       FUN002
098000970526     C                   MOVEL     FUV(15)       FUN003
098100970526     C                   MOVEL     FUV(16)       FUN004
098200970526     C                   MOVEL     FUV(17)       FUN005
098300970526     C                   MOVEL     FUV(18)       FUN006
098400970526     C                   END
098500970526     C*
098600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
098700970526     C     FUNGRU        IFEQ      4
098800970526     C                   MOVEL     FUV(19)       FUN001
098900970526     C                   MOVEL     FUV(20)       FUN002
099000970526     C                   MOVEL     FUV(21)       FUN003
099100970526     C                   MOVEL     FUV(22)       FUN004
099200970526     C                   MOVEL     *BLANKS       FUN005
099300970526     C                   MOVEL     *BLANKS       FUN006
099400970526     C                   END
099500970526     C*
099600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099700970526     C                   MOVEL     *BLANKS       FUN007
099800970526     C                   MOVEL     *BLANKS       FUN008
099900970526     C*
100000970526     C     1             DO        24            I
100100970526     C                   MOVEL     FUD(I)        FUDRIG
100200970526     C     FUDFUN        IFEQ      '24'
100300970526     C     FUDATT        ANDEQ     'S'
100400970526     C                   MOVEL     FUDDES        FUN008
100500970526     C                   END
100600970526     C                   END                                                    DO
100700970526     C*
100800970526     C                   ENDSR
100900970526     C*------------------------------------------------------------------------*
101000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
101100970526     C*------------------------------------------------------------------------*
101200970526     C     FUNCON        BEGSR
101300970526     C*
101400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
101500970526     C                   EXSR      FUNVAR
101600970526     C*
101700970526     C* PULISCO CAMPI DI LAVORO
101800970526     C                   Z-ADD     0             I                 5 0
101900970526     C                   Z-ADD     0             W                 5 0
102000970526     C*
102100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
102200970526     C     FUNKEY        IFNE      *BLANKS
102300970526     C     1             DO        24            I
102400970526     C                   MOVEL     FUD(I)        FUDRIG
102500970526     C*
102600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102700970526     C     FUNKEY        IFEQ      FUDFUN
102800970526     C     FUDATT        ANDEQ     'S'
102900970526     C                   Z-ADD     I             W
103000970526     C                   END
103100970526     C                   END                                                    DO WHILE
103200970526     C*
103300970526     C* FUNZIONE NON ATTIVA (ERRORE)
103400970526     C     W             IFEQ      0
103500970526     C*
103600970526     C* EMISSIONE MESSAGGIO DI ERRORE
103700970526     C                   MOVEL     MSG(1)        DSMSMS
103800970526     C*
103900970526     C                   END                                                    W=0
104000970526     C                   END                                                    FUNKEY NE *BLANKS
104100970526     C*
104200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
104300970526     C     W             IFNE      0
104400970526     C     FUNKEY        OREQ      *BLANKS
104500970526     C     FUNKEY        IFEQ      *BLANKS
104600970526     C                   EXSR      ENTGES
104700970526     C                   END
104800970526     C     FUNKEY        IFEQ      '03'
104900970526     C                   EXSR      F03GES
105000970526     C                   END
105100970526     C     FUNKEY        IFEQ      '06'
105200970526     C                   EXSR      F06GES
105300970526     C                   END
105400970528     C     FUNKEY        IFEQ      '08'
105500970528     C                   EXSR      F08GES
105600970528     C                   END
105700970528     C     FUNKEY        IFEQ      '09'
105800970528     C                   EXSR      F09GES
105900970528     C                   END
106000970528     C     FUNKEY        IFEQ      '10'
106100970528     C                   EXSR      F10GES
106200970528     C                   END
106300970528     C     FUNKEY        IFEQ      '11'
106400970528     C                   EXSR      F11GES
106500970528     C                   END
106600970526     C     FUNKEY        IFEQ      '12'
106700970526     C                   EXSR      F12GES
106800970526     C                   END
106900970603     C     FUNKEY        IFEQ      '13'
107000970603     C                   EXSR      F13GES
107100970603     C                   END
107200970526     C     FUNKEY        IFEQ      '24'
107300970526     C                   EXSR      F24GES
107400970526     C                   END
107500970526     C                   END
107600970526     C*
107700970526     C                   ENDSR
107800970526     C*------------------------------------------------------------------------*
107900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
108000970526     C*------------------------------------------------------------------------*
108100970526     C     FUNVAR        BEGSR
108200970526     C*
108300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
108400970526     C*
108500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
108600970526     C     KEY           IFEQ      ENTER
108700970526     C                   MOVEL     *BLANKS       FUNKEY
108800970526     C                   END
108900970526     C     KEY           IFEQ      F1
109000970526     C                   MOVEL     '01'          FUNKEY
109100970526     C                   END
109200970526     C     KEY           IFEQ      F2
109300970526     C                   MOVEL     '02'          FUNKEY
109400970526     C                   END
109500970526     C     KEY           IFEQ      F3
109600970526     C                   MOVEL     '03'          FUNKEY
109700970526     C                   END
109800970526     C     KEY           IFEQ      F4
109900970526     C                   MOVEL     '04'          FUNKEY
110000970526     C                   END
110100970526     C     KEY           IFEQ      F5
110200970526     C                   MOVEL     '05'          FUNKEY
110300970526     C                   END
110400970526     C     KEY           IFEQ      F6
110500970526     C                   MOVEL     '06'          FUNKEY
110600970526     C                   END
110700970526     C     KEY           IFEQ      F7
110800970526     C                   MOVEL     '07'          FUNKEY
110900970526     C                   END
111000970526     C     KEY           IFEQ      F8
111100970526     C                   MOVEL     '08'          FUNKEY
111200970526     C                   END
111300970526     C     KEY           IFEQ      F9
111400970526     C                   MOVEL     '09'          FUNKEY
111500970526     C                   END
111600970526     C     KEY           IFEQ      F10
111700970526     C                   MOVEL     '10'          FUNKEY
111800970526     C                   END
111900970526     C     KEY           IFEQ      F11
112000970526     C                   MOVEL     '11'          FUNKEY
112100970526     C                   END
112200970526     C     KEY           IFEQ      F12
112300970526     C                   MOVEL     '12'          FUNKEY
112400970526     C                   END
112500970526     C     KEY           IFEQ      F13
112600970526     C                   MOVEL     '13'          FUNKEY
112700970526     C                   END
112800970526     C     KEY           IFEQ      F14
112900970526     C                   MOVEL     '14'          FUNKEY
113000970526     C                   END
113100970526     C     KEY           IFEQ      F15
113200970526     C                   MOVEL     '15'          FUNKEY
113300970526     C                   END
113400970526     C     KEY           IFEQ      F16
113500970526     C                   MOVEL     '16'          FUNKEY
113600970526     C                   END
113700970526     C     KEY           IFEQ      F17
113800970526     C                   MOVEL     '17'          FUNKEY
113900970526     C                   END
114000970526     C     KEY           IFEQ      F18
114100970526     C                   MOVEL     '18'          FUNKEY
114200970526     C                   END
114300970526     C     KEY           IFEQ      F19
114400970526     C                   MOVEL     '19'          FUNKEY
114500970526     C                   END
114600970526     C     KEY           IFEQ      F20
114700970526     C                   MOVEL     '20'          FUNKEY
114800970526     C                   END
114900970526     C     KEY           IFEQ      F21
115000970526     C                   MOVEL     '21'          FUNKEY
115100970526     C                   END
115200970526     C     KEY           IFEQ      F23
115300970526     C                   MOVEL     '23'          FUNKEY
115400970526     C                   END
115500970526     C     KEY           IFEQ      F24
115600970526     C                   MOVEL     '24'          FUNKEY
115700970526     C                   END
115800970526     C     KEY           IFEQ      ROLLUP
115900970526     C                   MOVEL     'RU'          FUNKEY
116000970526     C                   END
116100970526     C     KEY           IFEQ      HELP
116200970526     C                   MOVEL     'HP'          FUNKEY
116300970526     C                   END
116400970526     C                   ENDSR
116500970526     C*------------------------------------------------------------------------*
116600970526     C* NOTE COSTRUZIONE SCHIERE
116700970526     C*------------------------------------------------------------------------*
116800970526     C*
116900970526     C* -MSG-MESSAGGI
117000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
117100970526     C* (NON TOCCARE MSG,1 E MSG,2)
117200970526     C*
117300970526     C* -CMD-COMANDI
117400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
117500970526     C*
117600970526     C* -OPD-OPZIONI
117700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
117800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
117900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
118000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
118100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
118200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
118300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
118400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
118500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
118600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
118700970526     C*
118800970526     C* -FUD-FUNZIONI
118900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
119000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
119100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
119200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
119300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
119400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
119500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
119600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
119700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
119800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
119900970526     O*------------------------------------------------------------------------*
120000970526** DEC - DECODIFICHE
120100970526    MODIFICA                                                      1
120200970526  ANNULLAMENTO                                                    2
120300970526VISUALIZZAZIONE                                                   3
120400970526   RIPRISTINO                                                     4
120500970526  INSERIMENTO                                                     5
120600970526** MSG - MESSAGGI
120700970526TISI47R- Tasto funzionale non gestito                                          1
120800970526TISI47R- Inserimento effettuato                                                2
120900970526TISI47R- Modifica effettuata                                                   3
121000970526TISI47R- Annullamento effettuato                                               4
121100970526TISI47R- Ripristino effettuato                                                 5
121200970526TISI47R- Richiesta funzione per record inesistente                             6
121300970526TISI47R- Record momentaneamente allocato: riprovare più tardi                  7
121400970527TISI47R- Richiesta funzione per record esistente                               8
121500970526TISI47R- Data errata                                                           9
121600970526TISI47R- La data scadenza deve essere maggiore della decorrenza               10
121700970526TISI47R- La versione non può scadere in un giorno già trascorso               11
121800970528TISI47R- 12
121900970526TISI47R- Descrizione obbligatoria                                             13
122000970526TISI47R- Inserire il numero della versione                                    14
122100970526TISI47R- La versione immessa si sovrappone ad una già in decorrenza           15
122200970526TISI47R- 16
122300970526TISI47R- 17
122400970526TISI47R- 18
122500970526TISI47R- 19
122600970526TISI47R- 20
122700970526** FUD - FUNZIONI
122800970526                   -        (S/N)=N                        funz.01¬
122900970526                   -        (S/N)=N                        funz.02¬
123000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
123100970526                   -        (S/N)=N                        funz.04¬
123200970526                   -        (S/N)=N                        funz.05¬
123300970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
123400970526                   -        (S/N)=N                        funz.07¬
123500970528F08=Località       -        (S/N)=S                        funz.08¬
123600970528F09=Eccezioni      -        (S/N)=S                        funz.09¬
123700970528F10=Tempi consegna -        (S/N)=S                        funz.10¬
123800970528F11=Cap            -        (S/N)=S                        funz.11¬
123900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
124000970603F13=Sinonimi       -        (S/N)=S                        funz.13¬
124100970526                   -        (S/N)=N                        funz.14¬
124200970526                   -        (S/N)=N                        funz.15¬
124300970526                   -        (S/N)=N                        funz.16¬
124400970526                   -        (S/N)=N                        funz.17¬
124500970526                   -        (S/N)=N                        funz.18¬
124600970526                   -        (S/N)=N                        funz.19¬
124700970526                   -        (S/N)=N                        funz.20¬
124800970526                   -        (S/N)=N                        funz.21¬
124900970526                   -        (S/N)=N                        funz.22¬
125000970526                   -        (S/N)=N                        funz.23¬
125100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
