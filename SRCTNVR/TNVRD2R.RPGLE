000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICND2R *----------------------------------------------------*
000300931123     H*         - PULIZIA FILE CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500931015     FTABEL00F  IF   E           K DISK
000600060127     Ffiftt01L  IF   E           K DISK
000700060127     F                                     RENAME(fiftt000:fiftt01)
000800060127     Ffiftt02L  IF   E           K DISK
000900060127     F                                     RENAME(fiftt000:fiftt02)
001000060124     Ffiftt05L  UF   E           K DISK
001100100504     Ffiftt04L  UF   E           K DISK    RENAME(fiftt000:fiftt04)
001200060124     Ffiftd01L  UF   E           K DISK
001300060321     F***fifce01L  UF   E           K DISK
001400060124     Ffifre02L  UF   E           K DISK
001500011114     Ffifst05L  UF   E           K DISK
001600011114     F                                     RENAME(fiftt000:fifst000)
001700100504     Ffifst04L  UF   E           K DISK    RENAME(fiftt000:fifst04)
001800020227     Ffifst01L  IF   E           K DISK
001900020227     F                                     RENAME(fiftt000:fifst01)
002000011114     Ffifsd01L  UF   E           K DISK
002100011114     F                                     RENAME(fiftd000:fifsd000)
002200060203     Ffiapd01L  UF   E           K DISK
002300930707     F*
002400931126     D WLBDAT          DS
002500950120     D  G02DAT                 1      8  0
002600950120     D  G02INV                 9     16  0
002700950120     D  G02ERR                17     17
002800950120     D  G02TGI                18     22  0
002900931126     D WGIDAT          DS
003000950120     D  GIODAT                 1      8  0
003100950120     D  GIOINV                 9     16  0
003200950120     D  GIOTGI                17     21  0
003300920810     D KPJBA         E DS
003400931123     D DS5A          E DS
003500000000     I/SPACE 3
003600900521     C****************************************************************
003700900521     C*  RIEPILOGO INDICATORI
003800900521     C***************************************************************
003900931123     C* 19    - ON RITIRI  OFF CONSEGNE
004000011114     C* 30    - LETTURA fiftt
004100931123     C* 31    - OFF CI SONO DEI RECROD DA PULIRE
004200011114     C* 32    - LETTURA fiftd
004300011114     C* 39    - ALLOCATO 1 RECORD IN fiftd
004400900521     C*****************************************************************
004500000000     C     *ENTRY        PLIST
004600000000     C                   PARM                    KPJBA
004700931123     C                   Z-ADD     1             CODUT             1 0
004800900511     C*---------------------------------------------------------------*
004900931015     C     KTAB          KLIST
005000950120     C                   KFLD                    CODUT
005100950120     C                   KFLD                    COD
005200950120     C                   KFLD                    KEY
005300931116     C     KFTT          KLIST
005400931123     C                   KFLD                    W01FVL
005500950120     C                   KFLD                    FTTDCV
005600931123     C     KFTD          KLIST
005700931123     C                   KFLD                    FTTPDR
005800931123     C                   KFLD                    FTTTSR
005900931123     C                   KFLD                    FTTNDC
006000950201     C                   KFLD                    FTTDDC
006100020227     C     KFTT1         KLIST
006200020227     C                   KFLD                    FTDPDR
006300020227     C                   KFLD                    FTDTSR
006400020227     C                   KFLD                    FTDNDC
006500020227     C                   KFLD                    FTDDDC
006600060124     C*
006700060124     C     KFCE          KLIST
006800060124     C                   KFLD                    FTTFGS
006900060124     C                   KFLD                    FTTPDR
007000060124     C                   KFLD                    FTTTSR
007100060124     C                   KFLD                    FTTNDC
007200060124     C                   KFLD                    FTTDDC
007300060127     C*
007400060321     C**** KFCE1         KLIST
007500060321     C****               KFLD                    FCEPDR
007600060321     C****               KFLD                    FCETSR
007700060321     C****               KFLD                    FCENDC
007800060321     C****               KFLD                    FCEDDC
007900060124     C*
008000060203     C     Kapd          KLIST
008100060203     C                   KFLD                    Ktip
008200060203     C                   KFLD                    FTTpdr
008300060320     C*
008400060321     C**** Kapdfce       KLIST
008500060321     C****               KFLD                    Ktip
008600060321     C****               KFLD                    FCEpdr
008700060203     C*
008800060124     C     KFRE          KLIST
008900060124     C                   KFLD                    Ktip              1
009000060124     C                   KFLD                    FTTPDR
009100060124     C                   KFLD                    FTTDDC
009200060124     C                   move      'A'           Ktip              1
009300060127     C*
009400060127     C     KFRE1         KLIST
009500060127     C                   KFLD                    FREPDR
009600060127     C                   KFLD                    FREfvl            1
009700060127     C                   KFLD                    FREDDC
009800950120     C*
009900950120     C* DEFINIZIONE CAMPI
010000950120     C     *LIKE         DEFINE    TBLCOD        COD
010100950120     C     *LIKE         DEFINE    TBLKEY        KEY
010200931123     C     *LIKE         DEFINE    FTTFVL        W01FVL
010300931123     C     *LIKE         DEFINE    FTTDDC        W01DDC
010400940114     C     *LIKE         DEFINE    FTTDDC        W01NTR
010500060127     C     *LIKE         DEFINE    FTTDDC        W01050
010600950120     C*
010700950120     C* LA DATA DEL GIORNO LA PRENDO DA TIME
010800950120     C                   TIME                    W0140            14 0
010900950120     C                   MOVE      W0140         UDATE8            8 0
011000950120     C*
011100931116     C*---------------------------------------------------------------*
011200060127     C* MEMORIZZIAMO RECORD RIEPILOGO CONTEGGI PADRONCINI X 50 GG
011300060127     C* MEMORIZZIAMO DETTAGLIO CONTEGGI PADRONCINI        X  3 GG
011400931207     C* MEMORIZZIAMO CONTEGGI PADRONCINI NON TASSATI      X 40 GG
011500931126     C*
011600931123     C* REPERISCO LA TABELLA CON I GIORNI DI PULIZIA
011700931123     C                   MOVEL     '5A'          COD
011800931123     C                   MOVEL     '1       '    KEY
011900931123     C     KTAB          CHAIN     TABEL                              30
012000931123    1C     *IN30         IFEQ      *ON
012100931126     C                   Z-ADD     3             §5AFTD
012200931126     C                   Z-ADD     50            §5AFTT
012300931207     C                   Z-ADD     40            §5AFT2
012400931207     C                   Z-ADD     3             §5AFSD
012500931207     C                   Z-ADD     50            §5AFST
012600931207     C                   Z-ADD     40            §5AFS2
012700931123     C                   ELSE
012800931123     C                   MOVEL     TBLUNI        DS5A
012900931123    1C                   ENDIF
013000950907     C*
013100950907     C* DATA PULIZIA PER CONTEGGI NON TASSATI (NO SIMULAZIONE)
013200950907     C* DATA PULIZIA = DATA GIORNO - GIORNI DI PULIZIA
013300950907     C                   MOVE      UDATE8        G02DAT
013400950907     C                   MOVEL     *BLANK        G02ERR
013500950907     C                   CALL      'XSRDA8'
013600950907     C                   PARM                    WLBDAT
013700950907     C*
013800950907     C     G02TGI        SUB       §5AFT2        GIOTGI
013900950907     C                   CALL      'XSRGI8'
014000950907     C                   PARM                    WGIDAT
014100950907     C                   Z-ADD     GIOINV        W01DDC
014200060127     C*
014300060127     C     G02TGI        SUB       §5AFT2        GIOTGI
014400060127     C                   SUB       50            GIOTGI
014500060127     C                   CALL      'XSRGI8'
014600060127     C                   PARM                    WGIDAT
014700060127     C                   Z-ADD     GIOINV        W01050
014800100513     c
014900100513     c                   goto      sotto1
015000060127     C**
015100060127     C* P U L I Z I A   D E T T A G L I O   C O N T E G G I
015200060127     C**
015300060127     C* VEDO SE DEVO PULIRE
015400060127     C                   Z-ADD     1             C                 3 0
015500060127     C                   MOVEL     'C'           W01FVL
015600060127     C                   MOVEL     *HIVAL        FTTDCV
015700060127     C*
015800060127    1C     C             DOWLE     §5AFTD
015900060127     C     *IN31         ANDEQ     *OFF
016000060127     C     KFTT          SETLL     fiftt000
016100060127     C     W01FVL        READPE(N) fiftt000                               31
016200060127     C                   ADD       1             C
016300060127    1C                   ENDDO
016400060127     C* SALVO IL GIORNO FINO AL QUALE NON DEVO PULIRE
016500060127     C                   Z-ADD     FTTDCV        W01NTR
016600060127    2C     W01NTR        IFEQ      0
016700060127     C                   Z-ADD     *HIVAL        W01NTR
016800060127    2C                   ENDIF
016900060127     C*
017000060127     C* SE LA DATA FINO ALLA QUALE NON DEVO PULIRE E' INFERIORE ALLA
017100060127     C*  DATA PULIZIA DEI CONTEGGI NON TASSATI, UTILIZZO QUEST'ULTIMA;
017200060127     C*  QUESTO PER PERMETTERE LA PULIZIA DI BOLLE ARRIVI MOLTO VECCHIE
017300060127    2C     W01NTR        IFLT      W01DDC
017400060127     C                   Z-ADD     W01DDC        W01NTR
017500060127    2C                   ENDIF
017600060127     C*
017700060127      * ?   -  Attenzione da quando i Padrnocini sono in UNI -
017800060127      * ? non bisogna + cancellare i records come si faceva prima
017900060127      * ? in FILIALE quindi,per permettere ad altri pgm gestionali
018000060127      * ? di funzionare senza essere gravati nelle performance x
018100060127      * ? la notevole mole di dati, si danno come cancellate le
018200060127      * ? bolle in dettaglio flaggando il campo FTDATB = 'A'
018300060127      * ? (annullo logico).
018400060127     C     W01FVL        SETLL     fiftt000
018500060127     C     W01FVL        READE(N)  fiftt000                               30
018600060127     C*
018700060127    2C     *IN30         DOWEQ     *OFF
018800060127     C     FTTDCV        ANDLT     W01NTR
018900060127     C*
019000060127     C* SOLO QUELLI CON TIPO PRESTAZIONE
019100060127     C*  trasmessi oppure con più di 50 giorni
019200060127    3C     FTTTSR        IFNE      ' '
019300060127    3C     FTTftr        Ifne      ' '
019400060127    3C     FTTDDC        orlt      W01050
019500060127     C                   EXSR      AGGFTD
019600060127    3C                   EndIF
019700060127    3C                   ENDIF
019800060127     C*
019900060127     C     W01FVL        READE(N)  fiftt000                               30
020000060127    2C                   ENDDO
020100100513     c     sotto1        tag
020200060127     C*
020300060127     C* P U L I Z I A   R I E P I L O G O   C O N T E G G I
020400060127     C*
020500060127     C* DATA PULIZIA = DATA GIORNO - GIORNI DI PULIZIA
020600060127     C                   MOVE      UDATE8        G02DAT
020700060127     C                   MOVEL     *BLANK        G02ERR
020800060127     C                   CALL      'XSRDA8'
020900060127     C                   PARM                    WLBDAT
021000060127     C*
021100060127     C     G02TGI        SUB       §5AFTT        GIOTGI
021200060127     C                   CALL      'XSRGI8'
021300060127     C                   PARM                    WGIDAT
021400060127     C                   Z-ADD     GIOINV        W01NTR
021500060127     C*
021600060127     C     G02TGI        SUB       §5AFTT        GIOTGI
021700060127     C                   SUB       50            GIOTGI
021800060127     C                   CALL      'XSRGI8'
021900060127     C                   PARM                    WGIDAT
022000060127     C                   Z-ADD     GIOINV        W01050
022100060127     C****
022200060127      * ? (Non deve essere + eseguita da quando i Padroncini sono in UNI)
022300060127     C****
022400060127     C**** W01FVL        SETLL     fiftt000
022500060127     C**** W01FVL        READE     fiftt000                               30
022600060127     C****
022700060127     C**** *IN30         DOWEQ     *OFF
022800060127     C**** FTTDDC        ANDLE     W01NTR
022900060127     C**** FTTTSR        IFEQ      ' '
023000060127    3C**** FTTftr        IFne      ' '
023100060127    3C**** FTTDDC        orlt      W01050
023200060127     C****               DELETE    fiftt000
023300060127     c****               endIF
023400060127     C****               ENDIF
023500060127     C****
023600060127     C**** W01FVL        READE     fiftt000                               30
023700060127     C****               ENDDO
023800060127     C**
023900931207     C* P U L I Z I A   C O N T E G G I   N O N   T A S S A T I
024000931207     C**
024100100513     c                   goto      sotto2
024200060124     C* PULISCO LE --> "DA VALORIZZARE"/"BLOCCATE"/"VALORIZZATE"
024300060124     C**      + vecchie di xxx giorni
024400060124     C                   CLEAR                   W01FVL
024500060124     C                   DO        3             X                 1 0
024600060124     C*
024700100504     C     W01FVL        SETLL     fiftt04
024800100504     C     W01FVL        READE     fiftt04                                30
024900060124     C*
025000060124     C     *IN30         DOWEQ     *OFF
025100060124     C     FTTDDC        ANDLE     W01DDC
025200060124    3C     FTTftr        andeq     ' '
025300060124     C                   EXSR      DELFTD
025400060203     C                   EXSR      DELFRE
025500060203      *
025600060203     c                   move      'N'           Fittizio          1
025700060203      *
025800060203      * ?Solo al primo giro delle testate non Valorizzate nè Bloccate
025900060203     c                   if        w01fvl = *Blank
026000060203     c     kapd          chain     fiapd01l
026100060203     c                   if        %Found(fiapd01l) and apdpdd ='S'
026200060203     c                   move      'S'           Fittizio
026300060203     c                   end
026400060203     c                   end
026500060203      *
026600060203      * ?se Fittizio non deve cancellare FCE poichè servono i records FCE
026700060203      * ?in sede e sarà la pulizia di sede ad occuparsene.
026800060203     c     fittizio      ifeq      'N'
026900060321     C******             EXSR      DELFCE
027000060203     c                   end
027100060203      *
027200060124     C* 39 ON - RECORD ALLOCATO NON POSSO CANCELLARE IL GIORNO
027300100504     C  N39              DELETE    fiftt04
027400060124     C*
027500100504     C     W01FVL        READE     fiftt04                                30
027600060124     C                   ENDDO
027700060124     C*
027800060124     C* PULISCO QUELLE ANCORA BLOCCATE
027900060124     C     X             IFEQ      1
028000060124     C                   MOVEL     'B'           W01FVL
028100060124     C                   ENDIF
028200060124     C* PULISCO LE VALORIZZATE NON CONFERMATE
028300060124     C     X             IFEQ      2
028400060124     C                   MOVEL     'V'           W01FVL
028500060124     C                   ENDIF
028600060124     C                   ENDDO
028700060124     C*
028800931207     C* ESEGUO LO STESSO PER I FILES DI SIMULAZIONE
028900931207     C                   EXSR      SIMUL
029000100513     c     sotto2        tag
029100060127     C*
029200060127     C* PULIZIA RECORDS FIFTD00F NON TROVATI SU FIFTT01L
029300060127     C                   EXSR      PULFTD
029400100513     c
029500100513     c                   goto      sotto3
029600060127     C*
029700060127     C* PULIZIA RECORDS FIFCE00F NON TROVATI SU FIFTT01L
029800060321     C*******            EXSR      PULFCE
029900060127     C*
030000060127     C* PULIZIA RECORDS FIFRE00F NON TROVATI SU FIFTT02L
030100060127     C                   EXSR      PULFRE
030200020715     C*
030300020227     C* PULIZIA RECORDS FIFSD00F NON TROVATI SU FIFST01L
030400020227     C                   EXSR      PULFSD
030500100513     c
030600100513     c     sotto3        tag
030700931123     C                   SETON                                        LR
030800931123     C*
030900060127     C* CANCELLO RECORD IL fiftd -------------------------------------*
031000060127     C     DELFTD        BEGSR
031100060127     C                   SETOFF                                       39
031200060127     C*
031300060127     C     KFTD          SETLL     fiftd000
031400060127     C     KFTD          READE     fiftd000                             3932
031500060127     C*
031600060127    1C     *IN32         DOWEQ     *OFF
031700060127     C     *IN39         ANDEQ     *OFF
031800060127     C*
031900060127     C                   DELETE    fiftd000                                      CONSEGNE
032000060127     C     KFTD          READE     fiftd000                             3932
032100060127    1C                   ENDDO
032200060127     C*
032300060127     C                   ENDSR
032400060127     C*
032500060127     C* ANNULLO  RECORD IL fiftd -------------------------------------*
032600060127     C     AGGFTD        BEGSR
032700060124     C                   SETOFF                                       39
032800060124     C*
032900060124     C     KFTD          SETLL     fiftd000
033000060124     C     KFTD          READE     fiftd000                             3932
033100060124     C*
033200060124    1C     *IN32         DOWEQ     *OFF
033300060124     C     *IN39         ANDEQ     *OFF
033400060127      * ?   -  Attenzione da quando i Padrnocini sono in UNI -
033500060127      * ? Per permettere al programma delle pulizie Bolle di filiale
033600060127      * ?  di pulire le bolle anche se ora i records dei Padroncini
033700060127      * ?   sono presenti per molto + tempo sugli archivi rispetto
033800060127      * ?   a prima che venivano fisicamente cancellati in filiale
033900060127     c                   move      'A'           FTDATB
034000060127     C                   update    fiftd000
034100060124     C     KFTD          READE     fiftd000                             3932
034200060124    1C                   ENDDO
034300060124     C*
034400060124     C                   ENDSR
034500060124     C*
034600060124     C* CANCELLO RECORD IL fifce -------------------------------------*
034700060321     C*****DELFCE        BEGSR
034800060321     C*******            SETOFF                                       39
034900060321     C*******
035000060321     C*****KFCE          SETLL     fifce000
035100060321     C*****KFCE          READE     fifce000                             3932
035200060321     C*******
035300060321    1C******IN32         DOWEQ     *OFF
035400060321     C******IN39         ANDEQ     *OFF
035500060321     C*******
035600060321     C*******            DELETE    fifce000
035700060321     C*****KFCE          READE     fifce000                             3932
035800060321    1C*******            ENDDO
035900060321     C*******
036000060321     C*******            ENDSR
036100060321     C*******
036200060124     C* CANCELLO RECORD IL fifre -------------------------------------*
036300060124     C     DELFRE        BEGSR
036400060124     C                   SETOFF                                       39
036500060124     C*
036600060124     C     KFRE          SETLL     fifre000
036700060124     C     KFRE          READE     fifre000                             3932
036800060124     C*
036900060124    1C     *IN32         DOWEQ     *OFF
037000060124     C     *IN39         ANDEQ     *OFF
037100060124     C*
037200060124     C                   DELETE    fifre000
037300060124     C     KFRE          READE     fifre000                             3932
037400060124    1C                   ENDDO
037500060124     C*
037600060124     C                   ENDSR
037700060124     C*
037800931207     C* S I M U L A Z I O N E ----------------------------------------*
037900931207     C     SIMUL         BEGSR
038000950907     C*
038100950907     C* DATA PULIZIA PER CONTEGGI NON TASSATI (SIMULAZIONE)
038200950907     C* DATA PULIZIA = DATA GIORNO - GIORNI DI PULIZIA
038300950907     C                   MOVE      UDATE8        G02DAT
038400950907     C                   MOVEL     *BLANK        G02ERR
038500950907     C                   CALL      'XSRDA8'
038600950907     C                   PARM                    WLBDAT
038700950907     C*
038800950907     C     G02TGI        SUB       §5AFS2        GIOTGI
038900950907     C                   CALL      'XSRGI8'
039000950907     C                   PARM                    WGIDAT
039100950907     C                   Z-ADD     GIOINV        W01DDC
039200931207     C**
039300931207     C* P U L I Z I A   D E T T A G L I O   C O N T E G G I
039400931207     C*               S I M U L A Z I O N E
039500931207     C**
039600931207     C* VEDO SE DEVO PULIRE
039700931207     C                   Z-ADD     1             C                 3 0
039800931207     C                   MOVEL     'C'           W01FVL
039900950120     C                   MOVEL     *HIVAL        FTTDCV
040000931207     C*
040100931207    1C     C             DOWLE     §5AFSD
040200931207     C     *IN31         ANDEQ     *OFF
040300011114     C     KFTT          SETLL     fifst000
040400011114     C     W01FVL        READPE(N) fifst000                               31
040500931207     C                   ADD       1             C
040600931207    1C                   ENDDO
040700931207     C*
040800931207     C* SALVO IL GIORNO FINO AL QUALE NON DEVO PULIRE
040900940114     C                   Z-ADD     FTTDDC        W01NTR
041000940114    2C     W01NTR        IFEQ      0
041100940114     C                   Z-ADD     *HIVAL        W01NTR
041200931207    2C                   ENDIF
041300950907     C*
041400950907     C* SE LA DATA FINO ALLA QUALE NON DEVO PULIRE E' INFERIORE ALLA
041500950907     C*  DATA PULIZIA DEI CONTEGGI NON TASSATI, UTILIZZO QUEST'ULTIMA;
041600950907     C*  QUESTO PER PERMETTERE LA PULIZIA DI BOLLE ARRIVI MOLTO VECCHIE
041700950907    2C     W01NTR        IFLT      W01DDC
041800950907     C                   Z-ADD     W01DDC        W01NTR
041900950907    2C                   ENDIF
042000931207     C*
042100011114     C     FTTFVL        SETLL     fifst000
042200011114     C     FTTFVL        READE     fifst000                               30
042300931207     C*
042400931207    2C     *IN30         DOWEQ     *OFF
042500950120     C     FTTDCV        ANDLT     W01NTR
042600931207     C*
042700931207     C* SOLO QUELLI CON TIPO PRESTAZIONE
042800931207    3C     FTTTSR        IFNE      ' '
042900931207     C                   EXSR      DELFSD
043000931207     C* 39 ON - RECORD ALLOCATO NON POSSO CANCELLARE IL GIORNO
043100011114     C  N39              DELETE    fifst000
043200931207    3C                   ENDIF
043300931207     C*
043400011114     C     FTTFVL        READE     fifst000                               30
043500931207    2C                   ENDDO
043600931207     C*
043700931207     C**
043800931207     C* P U L I Z I A   R I E P I L O G O   C O N T E G G I
043900931207     C*               S I M U L A Z I O N E
044000931207     C**
044100931207     C* DATA PULIZIA = DATA GIORNO - GIORNI DI PULIZIA
044200950120     C                   MOVE      UDATE8        G02DAT
044300931207     C                   MOVEL     *BLANK        G02ERR
044400950120     C                   CALL      'XSRDA8'
044500931207     C                   PARM                    WLBDAT
044600931207     C*
044700931207     C     G02TGI        SUB       §5AFST        GIOTGI
044800931207     C*
044900950120     C                   CALL      'XSRGI8'
045000931207     C                   PARM                    WGIDAT
045100940114     C                   Z-ADD     GIOINV        W01NTR
045200931207     C*
045300011114     C     W01FVL        SETLL     fifst000
045400011114     C     W01FVL        READE     fifst000                               30
045500931207     C*
045600931207     C     *IN30         DOWEQ     *OFF
045700960122     C     FTTDDC        ANDLE     W01NTR
045800931207     C     FTTTSR        IFEQ      ' '
045900011114     C                   DELETE    fifst000
046000931207     C                   ENDIF
046100931207     C*
046200011114     C     W01FVL        READE     fifst000                               30
046300931207     C                   ENDDO
046400931207     C**
046500931207     C* P U L I Z I A   C O N T E G G I   N O N   T A S S A T I
046600931207     C*               S I M U L A Z I O N E
046700931207     C**
046800931207     C* PULISCO LE "DA VALORIZZARE"
046900931207     C                   CLEAR                   W01FVL
047000030107     C                   DO        3             X                 1 0
047100931207     C*
047200100504     C     W01FVL        SETLL     fifst04
047300100504     C     W01FVL        READE     fifst04                                30
047400931207     C*
047500931207     C     *IN30         DOWEQ     *OFF
047600030103     C     FTTDDC        IFLE      W01DDC
047700931207     C                   EXSR      DELFSD
047800931207     C* 39 ON - RECORD ALLOCATO NON POSSO CANCELLARE IL GIORNO
047900100504     C  N39              DELETE    fifst04
048000030103     C                   END
048100931207     C*
048200100504     C     W01FVL        READE     fifst04                                30
048300931207     C                   ENDDO
048400931207     C*
048500931207     C* PULISCO LE VALORIZZATE NON CONFERMATE
048600030107     c                   if        X = 2
048700931207     C                   MOVEL     'V'           W01FVL
048800030107     c                   end
048900030107     c                   if        X = 3
049000030107     C                   MOVEL     'B'           W01FVL
049100030107     c                   end
049200931207     C                   ENDDO
049300931207     C*
049400060119     C                   ENDSR
049500011114     C* CANCELLO RECORD IN fifsd - SIMULAZIONE -----------------------*
049600931207     C     DELFSD        BEGSR
049700931207     C                   SETOFF                                       39
049800931207     C*
049900011114     C     KFTD          SETLL     fifsd000
050000011114     C     KFTD          READE     fifsd000                             3932
050100931207     C*
050200931207    1C     *IN32         DOWEQ     *OFF
050300931207     C     *IN39         ANDEQ     *OFF
050400931207     C*
050500011114     C                   DELETE    fifsd000                                      CONSEGNE
050600011114     C     KFTD          READE     fifsd000                             3932
050700931207    1C                   ENDDO
050800931207     C*
050900931207     C                   ENDSR
051000060127     C* PULIZIE RECORDS fiftd    ------------------------------------*
051100060127     C     PULFTD        BEGSR
051200060127     C*
051300060127     C     *LOVAL        SETLL     fiftd000
051400060127     C                   READ      fiftd000                               39
051500100513
051600060127    1C     *IN39         DOWEQ     *OFF
051700060127     C     KFTT1         CHAIN     fiftt01L                           32
051800060127     C*
051900060127     C     *IN32         IFEQ      *ON
052000060127     C                   DELETE    fiftd000
052100100513     c                   else
052200100513     C     KFTT1         setgt     fiftd01L                           32
052300060127     C                   ENDIF
052400100513
052500060127     C                   READ      fiftd000                               39
052600060127    1C                   ENDDO
052700060127     C*
052800060127     C                   ENDSR
052900060127     C*
053000060127     C* PULIZIE RECORDS fifce    ------------------------------------*
053100060321     C*****PULFCE        BEGSR
053200060321     C*******
053300060321     C******LOVAL        SETLL     fifce000
053400060321     C*******            READ      fifce000                               39
053500060321    1C******IN39         DOWEQ     *OFF
053600060321     C*****KFCE1         CHAIN     fiftt01L                           32
053700060321     C*******
053800060321     C******IN32         IFEQ      *ON
053900060321     C*******
054000060321     C**?Per*pulire, controlla che non sia un padroncino Fittizio
054100060321     c*******            move      'N'           Fittizio          1
054200060321     c*****kapdfce       chain     fiapd01l
054300060321     c*******            if        %Found(fiapd01l) and apdpdd ='S'
054400060321     c*******            move      'S'           Fittizio
054500060321     c*******            end
054600060321     C*******
054700060321     C**?se*Fittizio non deve cancellare FCE poichè servono i records FCE
054800060321     C**?in*sede e sarà la pulizia di sede ad occuparsene.
054900060321     c*****fittizio      ifeq      'N'
055000060321     C*******            DELETE    fifce000
055100060321     c*******            end
055200060321     C*******
055300060321     C*******            ENDIF
055400060321     C*******            READ      fifce000                               39
055500060321    1C*******            ENDDO
055600060321     C*******
055700060321     C*******            ENDSR
055800060321     C*******
055900060321     C* PULIZIE RECORDS fifre    ------------------------------------*
056000060321     C     PULFRE        BEGSR
056100060127     C*
056200060127     C     ktip          SETLL     fifre000
056300060127     C     ktip          READe     fifre000                               39
056400060127    1C     *IN39         DOWEQ     *OFF
056500060127     C*
056600060127     C                   move      'C'           FREfvl            1
056700060127     C     KFRE1         CHAIN     fiftt02L                           32
056800060127     C   32              move      'V'           FREfvl
056900060127     C   32KFRE1         CHAIN     fiftt02L                           32
057000060127     C   32              move      ' '           FREfvl
057100060127     C   32KFRE1         CHAIN     fiftt02L                           32
057200060127     C   32              move      'B'           FREfvl
057300060127     C   32KFRE1         CHAIN     fiftt02L                           32
057400060127     C*
057500060127     C     *IN32         IFEQ      *ON
057600060127     C                   DELETE    fifre000
057700060127     C                   ENDIF
057800060127     C*
057900060127     C     ktip          READe     fifre000                               39
058000060127    1C                   ENDDO
058100060127     C*
058200060127     C                   ENDSR
058300060127     C*
058400020227     C* PULIZIE RECORDS fifsd    ------------------------------------*
058500020227     C     PULFSD        BEGSR
058600020227     C*
058700020227     C     *LOVAL        SETLL     fifsd000
058800020227     C                   READ      fifsd000                               39
058900020227    1C     *IN39         DOWEQ     *OFF
059000020227     C     KFTT1         CHAIN     fifst01L                           32
059100020227     C*
059200020227     C     *IN32         IFEQ      *ON
059300020227     C                   DELETE    fifsd000
059400020227     C                   ENDIF
059500020227     C                   READ      fifsd000                               39
059600020227    1C                   ENDDO
059700020227     C*
059800020227     C                   ENDSR
