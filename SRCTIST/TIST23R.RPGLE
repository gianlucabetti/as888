000100000000     H*------------------------------------------------------------------------*
000200000710     H* STAMPA STATISTICA SERVIZI STRATEGI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800050706     FSISTL01L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100991123     FTIST23P   O    E             PRINTER OFLIND(*IN05)
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* INTESTAZIONI STAMPA
001700000000     D*-------------------
001800991123     D TES             S             30    DIM(4) CTDATA PERRCD(1)              RIGHE TESTATA
001900991123     D INT             S              9    DIM(5) CTDATA PERRCD(1)              DECODIFICA INTESTAZI
002000000000     D*-------------------
002100000000     D* SCHIERA FILIALI
002200000000     D*-------------------
002300000000     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002400000000     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002500000000     D*------------------------------------------------------------------------*
002600000000     D* INPUT
002700000000     D*------------------------------------------------------------------------*
002800000000     D*-------------------
002900000000     D* ARCHITETTURA
003000000000     D*-------------------
003100000000     D KPJBA         E DS
003200991025     D*-------------------
003300991025     D* DS PER SCOMPOSIZIONE CLIENTI
003400991025     D*-------------------
003500991025     D                 DS                  INZ
003600991025     D  KORG                   1      3  0
003700991025     D  CODCLI                 1      7  0
003800000000     D*-------------------
003900000000     D* DS PARAMETRI DI LANCIO PGM
004000000000     D*-------------------
004100000000     D PARAM           DS
004200991123     D  PARDAI                 1      8  0
004300991123     D  PARDAF                 9     16  0
004400991123     D  PARSNR                17     17
004500991123     D  PARDIV                18     18
004600991123     D  PARARE                19     21  0
004700991123     D  PARFIL                22     24  0
004800991123     D  PARKSC                25     31  0
004900991123     D  PARDET                32     32
005000011228     D  PARGIO                33     35  0
005100991123     D*-------------------
005200991123     D* DS CALCOLI SU DATE
005300991123     D*-------------------
005400991123     D DATAINZ                         D
005500991123     D DATAFIN                         D
005600991123     D*------------------
005700991123     D                 DS                  INZ
005800991123     D  GG                     1      2  0
005900991123     D  MM                     3      4  0
006000991123     D  AA                     5      8  0
006100991123     D  GIRDATA                1      8  0
006200991123     D*------------------
006300991123     D                 DS                  INZ
006400991123     D  AAINV                  1      4  0
006500991123     D  MMINV                  5      6  0
006600991123     D  GGINV                  7      8  0
006700991123     D  INVDATA                1      8  0
006800991123     D*------------------
006900991123     D                 DS                  INZ
007000991123     D  AAAAA                  1      4
007100991123     D  SEP1                   5      5
007200991123     D  MMA                    6      7
007300991123     D  SEP2                   8      8
007400991123     D  GGA                    9     10
007500991123     D  DATAA                  1     10
007600011227     D*-------------------
007700011227     D* SCHIERA DEI TIPI SERVIZI X CLIENTE
007800011227     D*-------------------
007900041217     D skKSC           S              7  0 DIM(32000)
008000041217     D jKSC            S              5  0 INZ(*zeros)
008100000000     C*------------------------------------------------------------------------*
008200000000     C* MAIN LINE
008300000000     C*------------------------------------------------------------------------*
008400000000     C*
008500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
008600000000     C                   EXSR      SETFIL
008700000000     C*
008800000000     C* CICLO FINO A FINE FILE DI LAVORO
008900000000DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
009000000000     C*
009100000000     C* OPERAZIONI PER NUOVA DIVISIONE
009200000000     C                   EXSR      NEWDIV
009300000000     C*
009400000000     C* CICLO FINO A ROTTURA DI DIVISIONE
009500000000DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
009600050706     C     STLDIV        ANDEQ     DEPDIV
009700000000     C*
009800000000     C* OPERAZIONI PER NUOVA AREA
009900000000     C                   EXSR      NEWARE
010000000000     C*
010100000000     C* CICLO FINO A ROTTURA DI AREA
010200000000DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
010300050706     C     STLDIV        ANDEQ     DEPDIV
010400050706     C     STLARE        ANDEQ     DEPARE
010500000000     C*
010600000000     C* OPERAZIONI PER NUOVA FILIALE
010700000000     C                   EXSR      NEWFIL
010800000000     C*
010900000000     C* CICLO FINO A ROTTURA DI FILIALE
011000000000DO  4C     FINL00        DOWEQ     '0'                                          --- 4 -->
011100050706     C     STLDIV        ANDEQ     DEPDIV
011200050706     C     STLARE        ANDEQ     DEPARE
011300050706     C     STLFIL        ANDEQ     DEPFIL
011400991124     C*
011500991124     C* OPERAZIONI PER SCELTA STAMPA IN DETTAGLIO
011600991124     C     SAVDET        IFEQ      'D'
011700000000     C*
011800991123     C* OPERAZIONI PER NUOVO CLIENTE/DATA
011900991123     C                   EXSR      NEWCLI
012000991026     C*
012100991123     C* CICLO FINO A ROTTURA DI CLIENTE/DATA PER DETTAGLIO
012200991026DO  5C     FINL00        DOWEQ     '0'                                          --- 5 -->
012300050706     C     STLDIV        ANDEQ     DEPDIV
012400050706     C     STLARE        ANDEQ     DEPARE
012500050706     C     STLFIL        ANDEQ     DEPFIL
012600050706     C     STLKSC        ANDEQ     DEPKSC
012700050706     C     STLDAT        ANDEQ     DEPDAT
012800000000     C*
012900000000     C* MEMORIZZA LE SPEDIZIONI
013000000000     C                   EXSR      MEMSPE
013100000000     C*
013200000000     C* LEGGE IL RECORD SUCCESSIVO
013300000000     C                   EXSR      LETFIL
013400000000     C*
013500991026E   5C                   ENDDO                                                  <-- 5 ---
013600991124     C*
013700991124     C                   ENDIF
013800991124     C*
013900991124     C* OPERAZIONI PER SCELTA STAMPA IN SINTETICO O X TOTALI
014000991124     C     SAVDET        IFEQ      'S'
014100991124     C     SAVDET        OREQ      'T'
014200991124     C*
014300991124     C* OPERAZIONI PER NUOVO CLIENTE
014400991124     C                   EXSR      NEWCLI
014500991124     C*
014600991124     C* CICLO FINO A ROTTURA DI CLIENTE PER SINTETICO
014700991124DO  5C     FINL00        DOWEQ     '0'                                          --- 5 -->
014800050706     C     STLDIV        ANDEQ     DEPDIV
014900050706     C     STLARE        ANDEQ     DEPARE
015000050706     C     STLFIL        ANDEQ     DEPFIL
015100050706     C     STLKSC        ANDEQ     DEPKSC
015200991124     C*
015300991124     C* MEMORIZZA LE SPEDIZIONI
015400991124     C                   EXSR      MEMSPE
015500991124     C*
015600991124     C* LEGGE IL RECORD SUCCESSIVO
015700991124     C                   EXSR      LETFIL
015800991124     C*
015900991124E   5C                   ENDDO                                                  <-- 5 ---
016000991124     C*
016100991124     C                   ENDIF
016200991026     C*
016300991124     C* STAMPA IL TOTALE CLIENTE/DATA O SOLO CLIENTE
016400991124     C     PARDET        IFEQ      'D'
016500991026     C                   EXSR      STACLI
016600991123     C                   ENDIF
016700991026     C*
016800991026E   4C                   ENDDO                                                  <-- 4 ---
016900000000     C*
017000000000     C* STAMPA IL TOTALE FILIALE
017100000000     C                   EXSR      STAFIL
017200000000     C*
017300000000E   3C                   ENDDO                                                  <-- 3 ---
017400000000     C*
017500000000     C* STAMPA IL TOTALE AREA
017600991124     C     PARDET        IFEQ      'S'
017700000000     C                   EXSR      STAARE
017800000000     C                   ENDIF
017900000000     C*
018000000000E   2C                   ENDDO                                                  <-- 2 ---
018100000000     C*
018200000000     C* STAMPA IL TOTALE DIVISIONE
018300991124     C     PARDET        IFEQ      'S'
018400000000     C                   EXSR      STADIV
018500000000     C                   ENDIF
018600000000     C*
018700000000E   1C                   ENDDO                                                  <-- 1 ---
018800000000     C*
018900000000     C* STAMPA IL TOTALE GENERALE
019000991124     C     PARDET        IFEQ      'S'
019100000000     C                   EXSR      STAGEN
019200000000     C                   ENDIF
019300000000     C*
019400000000     C* STAMPA RIGA FINE STAMPA
019500000103     C                   WRITE     ST23FI
019600000000     C*
019700991123     C                   SETON                                        LR
019800000000     C*------------------------------------------------------------------------*
019900000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
020000000000     C*------------------------------------------------------------------------*
020100000000     C     SETFIL        BEGSR
020200000000     C*
020300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020400030528     C***                MOVEL     *BLANKS       KWPDIV
020500030528     C***                Z-ADD     *ZEROS        KWPARE
020600030528     C***                Z-ADD     *ZEROS        KWPFIL
020700030528     C***                Z-ADD     *ZEROS        KWPKSC
020800030528     C***                Z-ADD     *ZEROS        KWPDAT
020900050706     C***  KEYWFP        SETLL     SISTL01L                           99        *FINE-ARCHIV
021000050706     C     *LOVAL        SETLL     SISTL01L                           99        *FINE-ARCHIV
021100000000     C*
021200000000     C     *IN99         IFEQ      '1'
021300000000     C                   MOVEL     '1'           FINL00
021400000000     C                   ELSE
021500000000     C                   EXSR      LETFIL
021600000000     C                   END
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* LETFIL - LETTURA PROSSIMO RECORD
022100000000     C*------------------------------------------------------------------------*
022200000000     C     LETFIL        BEGSR
022300000000     C*
022400000000     C* LEGGO FINO A:
022500000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
022600000000     C*     - TROVATO RECORD VALIDO
022700000000     C                   MOVEL     'N'           WRECOK
022800000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
022900000000     C     WRECOK        ANDEQ     'N'
023000000000     C*
023100000000     C* LETTURE SUCCESSIVE ALLA PRIMA
023200050706     C                   READ      SISTL01L                               99
023300000000     C*
023400000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
023500000000     C                   MOVEL     '1'           FINL00
023600000000     C                   ELSE
023700000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
023800000000     C                   END
023900000000     C*
024000000000     C                   END                                                    <-- 1 ---
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
024500000000     C*------------------------------------------------------------------------*
024600991025     C     CHKREC        BEGSR
024700000000     C*
024800000000     C                   MOVEL     'S'           WRECOK            1
024900000000     C*
025000991123     C* SE NON RIENTRA NEL PERIODO RICHIESTO ESCLUDO IL RECORD
025100050706     C     STLDAT        IFGE      PARDAI
025200050706     C     STLDAT        ANDLE     PARDAF
025300000000     C                   ELSE
025400000000     C                   MOVEL     'N'           WRECOK
025500030528     C                   ENDIF
025600000000     C*
025700000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
025800000000     C     PARDIV        IFNE      *BLANKS
025900050706     C     STLDIV        IFNE      PARDIV
026000000000     C                   MOVEL     'N'           WRECOK
026100030528     C                   ENDIF
026200030528     C                   ENDIF
026300000000     C*
026400000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
026500000000     C     PARARE        IFNE      *ZEROS
026600050706     C     STLARE        IFNE      PARARE
026700000000     C                   MOVEL     'N'           WRECOK
026800030528     C                   ENDIF
026900030528     C                   ENDIF
027000000000     C*
027100000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
027200000000     C     PARFIL        IFNE      *ZEROS
027300050706     C     STLFIL        IFNE      PARFIL
027400000000     C                   MOVEL     'N'           WRECOK
027500030528     C                   ENDIF
027600030528     C                   ENDIF
027700041209     C*
027800991123     C* SE CLIENTE DEL RECORD <> CLIENTE RICHIESTO, ESCLUDO
027900991123     C     PARKSC        IFNE      *ZEROS
028000050706     C     STLKSC        IFNE      PARKSC
028100991123     C                   MOVEL     'N'           WRECOK
028200030528     C                   ENDIF
028300030528     C                   ENDIF
028400000000     C*
028500000000     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700011220     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
028800000000     C*------------------------------------------------------------------------*
028900991124     C     MEMSPE        BEGSR
029000011220     C*
029100011220     C* VERIFICO SE TOTALIZZARE IL CLIENTE NEL GIORNO
029200041217     C                   SETOFF                                       22
029300050706     C                   IF        STLTTT > 0
029400050706     C     STLKSC        LOOKUP    skKSC                                  55
029500041217     C                   IF        not %found
029600041217     C                   ADD       1             jKSC
029700050706     C                   EVAL      skKSC(jKSC) = STLKSC
029800041217     C                   SETON                                        22
029900041217     C                   ENDIF
030000041217     C                   ENDIF
030100011227     C*
030200050706     C                   ADD       STLCDL        TDCDL
030300050706     C                   ADD       STLCUL        TDCUL
030400050706     C                   ADD       STLTTT        TDTTT
030500041217     C                   IF        *IN22 = *ON
030600041217     C                   ADD       1             TDTTTD
030700041217     C                   ENDIF
030800041209     C*
030900050706     C                   ADD       STLCDL        TACDL
031000050706     C                   ADD       STLCUL        TACUL
031100050706     C                   ADD       STLTTT        TATTT
031200041217     C                   IF        *IN22 = *ON
031300041217     C                   ADD       1             TATTTD
031400041217     C                   ENDIF
031500041209     C*
031600050706     C                   ADD       STLCDL        TFCDL
031700050706     C                   ADD       STLCUL        TFCUL
031800050706     C                   ADD       STLTTT        TFTTT
031900041217     C                   IF        *IN22 = *ON
032000041217     C                   ADD       1             TFTTTD
032100041217     C                   ENDIF
032200041209     C*
032300050706     C                   ADD       STLCDL        TCCDL
032400050706     C                   ADD       STLCUL        TCCUL
032500050706     C                   ADD       STLTTT        TCTTT
032600041217     C                   IF        *IN22 = *ON
032700041217     C                   ADD       1             TCTTTD
032800041217     C                   ENDIF
032900041209     C*
033000050706     C                   ADD       STLCDL        TGCDL             7 0
033100050706     C                   ADD       STLCUL        TGCUL             7 0
033200050706     C                   ADD       STLTTT        TGTTT             7 0
033300041217     C                   IF        *IN22 = *ON
033400041217     C                   ADD       1             TGTTTD            7 0
033500041217     C                   ENDIF
033600041217     C*
033700991123     C* TOTALE CLIENTE/DATA
033800050706     C                   ADD       STLDRC        TCDRC
033900050706     C                   ADD       STLDFS        TCDFS
034000050706     C                   ADD       STLDTN        TCDTN
034100050706     C                   ADD       STLURC        TCURC
034200050706     C                   ADD       STLUFS        TCUFS
034300050706     C                   ADD       STLUTN        TCUTN
034400050706     C                   ADD       STLTDB        TCTDB
034500050706     C                   ADD       STLORM        TCORM
034600050706     C                   ADD       STLVAG        TCVAG
034700050706     C                   ADD       STLTT2        TCTT2
034800050706     C                   ADD       STLTT3        TCTT3
034900050706     C                   ADD       STLGML        TCGML
035000050706     C                   ADD       STLPOD        TCPOD
035100991027     C* TOTALE FILIALE
035200050706     C                   ADD       STLDRC        TFDRC
035300050706     C                   ADD       STLDFS        TFDFS
035400050706     C                   ADD       STLDTN        TFDTN
035500050706     C                   ADD       STLURC        TFURC
035600050706     C                   ADD       STLUFS        TFUFS
035700050706     C                   ADD       STLUTN        TFUTN
035800050706     C                   ADD       STLTDB        TFTDB
035900050706     C                   ADD       STLORM        TFORM
036000050706     C                   ADD       STLVAG        TFVAG
036100050706     C                   ADD       STLTT2        TFTT2
036200050706     C                   ADD       STLTT3        TFTT3
036300050706     C                   ADD       STLGML        TFGML
036400050706     C                   ADD       STLPOD        TFPOD
036500991027     C* TOTALE AREA
036600050706     C                   ADD       STLDRC        TADRC
036700050706     C                   ADD       STLDFS        TADFS
036800050706     C                   ADD       STLDTN        TADTN
036900050706     C                   ADD       STLURC        TAURC
037000050706     C                   ADD       STLUFS        TAUFS
037100050706     C                   ADD       STLUTN        TAUTN
037200050706     C                   ADD       STLTDB        TATDB
037300050706     C                   ADD       STLORM        TAORM
037400050706     C                   ADD       STLVAG        TAVAG
037500050706     C                   ADD       STLTT2        TATT2
037600050706     C                   ADD       STLTT3        TATT3
037700050706     C                   ADD       STLGML        TAGML
037800050706     C                   ADD       STLPOD        TAPOD
037900991027     C* TOTALE DIVISIONE
038000050706     C                   ADD       STLDRC        TDDRC
038100050706     C                   ADD       STLDFS        TDDFS
038200050706     C                   ADD       STLDTN        TDDTN
038300050706     C                   ADD       STLURC        TDURC
038400050706     C                   ADD       STLUFS        TDUFS
038500050706     C                   ADD       STLUTN        TDUTN
038600050706     C                   ADD       STLTDB        TDTDB
038700050706     C                   ADD       STLORM        TDORM
038800050706     C                   ADD       STLVAG        TDVAG
038900050706     C                   ADD       STLTT2        TDTT2
039000050706     C                   ADD       STLTT3        TDTT3
039100050706     C                   ADD       STLGML        TDGML
039200050706     C                   ADD       STLPOD        TDPOD
039300991027     C* TOTALE GENERALE
039400050706     C                   ADD       STLDRC        TGDRC             7 0
039500050706     C                   ADD       STLDFS        TGDFS             7 0
039600050706     C                   ADD       STLDTN        TGDTN             7 0
039700050706     C                   ADD       STLURC        TGURC             7 0
039800050706     C                   ADD       STLUFS        TGUFS             7 0
039900050706     C                   ADD       STLUTN        TGUTN             7 0
040000050706     C                   ADD       STLTDB        TGTDB             7 0
040100050706     C                   ADD       STLORM        TGORM             7 0
040200050706     C                   ADD       STLVAG        TGVAG             7 0
040300050706     C                   ADD       STLTT2        TGTT2             7 0
040400050706     C                   ADD       STLTT3        TGTT3             7 0
040500050706     C                   ADD       STLGML        TGGML             7 0
040600050706     C                   ADD       STLPOD        TGPOD             7 0
040700991123     C*
040800991123     C                   ENDSR
040900000000     C*------------------------------------------------------------------------*
041000991124     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
041100000000     C*------------------------------------------------------------------------*
041200000000     C     NEWDIV        BEGSR
041300000000     C*
041400000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
041500050706     C                   MOVEL     STLDIV        DEPDIV
041600991124     C*
041700991124     C* DECODIFICA LA DIVISIONE
041800991124     C                   EXSR      DECDIV
041900991124     C*
042000991124     C* IMPOSTAZIONI CAMPI IN TESTATA
042100991124     C     PARDET        IFEQ      'S'                                          *SINTETICA
042200991124     C                   MOVEL     *BLANKS       STPFIL
042300991124     C                   MOVEL     *BLANKS       STPARE
042400991124     C                   MOVEL     *BLANKS       STPDIV
042500991124     C                   ELSE
042600991124     C                   MOVEL     *BLANKS       STPDIV
042700991124     C                   MOVEL     DESDIV        STPDIV
042800991124     C                   ENDIF
042900000000     C*
043000000000     C* AZZERA I TOTALI DELLA DIVISIONE
043100991123     C                   Z-ADD     *ZEROS        TDDRC             7 0
043200991123     C                   Z-ADD     *ZEROS        TDDFS             7 0
043300991123     C                   Z-ADD     *ZEROS        TDDTN             7 0
043400991123     C                   Z-ADD     *ZEROS        TDURC             7 0
043500991123     C                   Z-ADD     *ZEROS        TDUFS             7 0
043600991123     C                   Z-ADD     *ZEROS        TDUTN             7 0
043700991123     C                   Z-ADD     *ZEROS        TDTDB             7 0
043800010917     C                   Z-ADD     *ZEROS        TDORM             7 0
043900010917     C                   Z-ADD     *ZEROS        TDVAG             7 0
044000011220     C                   Z-ADD     *ZEROS        TDCDL             7 0
044100011220     C                   Z-ADD     *ZEROS        TDCUL             7 0
044200041217     C                   Z-ADD     *ZEROS        TDTTT             7 0
044300041217     C                   Z-ADD     *ZEROS        TDTTTD            7 0
044400041209     C                   Z-ADD     *ZEROS        TDTT2             7 0
044500041209     C                   Z-ADD     *ZEROS        TDTT3             7 0
044600041209     C                   Z-ADD     *ZEROS        TDGML             7 0
044700041209     C                   Z-ADD     *ZEROS        TDPOD             7 0
044800000000     C*
044900000000     C                   ENDSR
045000000000     C*------------------------------------------------------------------------*
045100000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
045200000000     C*------------------------------------------------------------------------*
045300000000     C     NEWARE        BEGSR
045400000000     C*
045500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
045600050706     C                   Z-ADD     STLARE        DEPARE
045700991124     C*
045800991124     C* DECODIFICA L'AREA
045900991124     C                   EXSR      DECARE
046000991124     C*
046100991124     C* IMPOSTAZIONI CAMPI IN TESTATA
046200991124     C     PARDET        IFEQ      'S'                                          *SINTETICA
046300991124     C*                  MOVEL     *BLANKS       STPFIL
046400991124     C                   MOVEL     *BLANKS       STPARE
046500991124     C                   ELSE
046600991124     C                   MOVEL     *BLANKS       STPARE
046700991124     C                   MOVEL     DESARE        STPARE
046800991124     C                   ENDIF
046900000000     C*
047000000000     C* AZZERA I TOTALI DELL'AREA
047100991123     C                   Z-ADD     *ZEROS        TADRC             7 0
047200991123     C                   Z-ADD     *ZEROS        TADFS             7 0
047300991123     C                   Z-ADD     *ZEROS        TADTN             7 0
047400991123     C                   Z-ADD     *ZEROS        TAURC             7 0
047500991123     C                   Z-ADD     *ZEROS        TAUFS             7 0
047600991123     C                   Z-ADD     *ZEROS        TAUTN             7 0
047700991123     C                   Z-ADD     *ZEROS        TATDB             7 0
047800010917     C                   Z-ADD     *ZEROS        TAORM             7 0
047900010917     C                   Z-ADD     *ZEROS        TAVAG             7 0
048000011220     C                   Z-ADD     *ZEROS        TACDL             7 0
048100011220     C                   Z-ADD     *ZEROS        TACUL             7 0
048200041217     C                   Z-ADD     *ZEROS        TATTT             7 0
048300041217     C                   Z-ADD     *ZEROS        TATTTD            7 0
048400041209     C                   Z-ADD     *ZEROS        TATT2             7 0
048500041209     C                   Z-ADD     *ZEROS        TATT3             7 0
048600041209     C                   Z-ADD     *ZEROS        TAGML             7 0
048700041209     C                   Z-ADD     *ZEROS        TAPOD             7 0
048800000000     C*
048900000000     C                   ENDSR
049000000000     C*------------------------------------------------------------------------*
049100991124     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
049200000000     C*------------------------------------------------------------------------*
049300000000     C     NEWFIL        BEGSR
049400000000     C*
049500000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
049600050706     C                   Z-ADD     STLFIL        DEPFIL
049700991124     C*
049800991124     C* DECODIFICA LA FILIALE
049900991124     C                   EXSR      DECFIL
050000991124     C*
050100991124     C* IMPOSTAZIONI CAMPI IN TESTATA
050200991124     C     PARDET        IFEQ      'S'                                          *SINTETICA
050300991124     C                   MOVEL     *BLANKS       STPFIL
050400991124     C                   ELSE
050500991124     C                   MOVEL     *BLANKS       STPFIL
050600991124     C                   MOVEL     DESFIL        STPFIL
050700991124     C                   ENDIF
050800000000     C*
050900000000     C* AZZERA I TOTALI DELLA FILIALE
051000991123     C                   Z-ADD     *ZEROS        TFDRC             7 0
051100991123     C                   Z-ADD     *ZEROS        TFDFS             7 0
051200991123     C                   Z-ADD     *ZEROS        TFDTN             7 0
051300991123     C                   Z-ADD     *ZEROS        TFURC             7 0
051400991123     C                   Z-ADD     *ZEROS        TFUFS             7 0
051500991123     C                   Z-ADD     *ZEROS        TFUTN             7 0
051600991123     C                   Z-ADD     *ZEROS        TFTDB             7 0
051700010917     C                   Z-ADD     *ZEROS        TFORM             7 0
051800010917     C                   Z-ADD     *ZEROS        TFVAG             7 0
051900011220     C                   Z-ADD     *ZEROS        TFCDL             7 0
052000011220     C                   Z-ADD     *ZEROS        TFCUL             7 0
052100041217     C                   Z-ADD     *ZEROS        TFTTT             7 0
052200041217     C                   Z-ADD     *ZEROS        TFTTTD            7 0
052300041209     C                   Z-ADD     *ZEROS        TFTT2             7 0
052400041209     C                   Z-ADD     *ZEROS        TFTT3             7 0
052500041209     C                   Z-ADD     *ZEROS        TFGML             7 0
052600041209     C                   Z-ADD     *ZEROS        TFPOD             7 0
052700000000     C*
052800000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
052900991123     C     *IN05         IFEQ      *ON                                          *FINE PAGINA
053000000000     C     PARDET        OREQ      'D'                                          *STAMPA IN DETTAGLIO
053100000103     C                   WRITE     ST23TE
053200991123     C                   SETOFF                                       05
053300000000     C                   ENDIF
053400000000     C*
053500000000     C                   ENDSR
053600000000     C*------------------------------------------------------------------------*
053700991123     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/DATA
053800000000     C*------------------------------------------------------------------------*
053900000000     C     NEWCLI        BEGSR
054000000000     C*
054100991124     C* MEMORIZZA NUOVO CLIENTE/DATA IN DEPOSITO
054200050706     C                   MOVEL     STLRAG        DEPRAG
054300050706     C                   Z-ADD     STLKSC        DEPKSC
054400050706     C                   Z-ADD     STLDAT        DEPDAT
054500000000     C*
054600991124     C* AZZERA I TOTALI DEL CLIENTE/DATA
054700991123     C                   Z-ADD     *ZEROS        TCDRC             7 0
054800991123     C                   Z-ADD     *ZEROS        TCDFS             7 0
054900991123     C                   Z-ADD     *ZEROS        TCDTN             7 0
055000991123     C                   Z-ADD     *ZEROS        TCURC             7 0
055100991123     C                   Z-ADD     *ZEROS        TCUFS             7 0
055200991123     C                   Z-ADD     *ZEROS        TCUTN             7 0
055300991123     C                   Z-ADD     *ZEROS        TCTDB             7 0
055400010917     C                   Z-ADD     *ZEROS        TCORM             7 0
055500010917     C                   Z-ADD     *ZEROS        TCVAG             7 0
055600011220     C                   Z-ADD     *ZEROS        TCCDL             7 0
055700011220     C                   Z-ADD     *ZEROS        TCCUL             7 0
055800041217     C                   Z-ADD     *ZEROS        TCTTT             7 0
055900041217     C                   Z-ADD     *ZEROS        TCTTTD            7 0
056000041209     C                   Z-ADD     *ZEROS        TCTT2             7 0
056100041209     C                   Z-ADD     *ZEROS        TCTT3             7 0
056200041209     C                   Z-ADD     *ZEROS        TCGML             7 0
056300041209     C                   Z-ADD     *ZEROS        TCPOD             7 0
056400991026     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700991123     C* STACLI - STAMPA TOTALI CLIENTE/DATA
056800000000     C*------------------------------------------------------------------------*
056900000000     C     STACLI        BEGSR
057000000000     C*
057100000000     C                   MOVEL     '0'           $STDAC
057200000000     C*
057300000103     C                   CLEAR                   ST23RA
057400000103     C                   CLEAR                   ST23RM
057500991123     C*
057600991124     C                   Z-ADD     DEPDAT        INVDATA
057700991124     C                   MOVE      AAINV         AAAAA
057800991124     C                   MOVE      MMINV         MMA
057900991124     C                   MOVE      GGINV         GGA
058000991124     C                   MOVEL     *BLANKS       STPDAT
058100991124     C     SAVDET        IFEQ      'D'
058200991124     C     GGA           CAT       '/':0         STPDAT
058300991124     C     STPDAT        CAT       MMA:0         STPDAT
058400991124     C     STPDAT        CAT       '/':0         STPDAT
058500991124     C     STPDAT        CAT       AAAAA:0       STPDAT
058600991124     C                   ENDIF
058700991123     C                   Z-ADD     DEPKSC        STPKSC
058800991123     C                   MOVEL     DEPRAG        STPRAG
058900991123     C*
059000991123     C                   Z-ADD     TCDFS         STPDFS
059100991123     C                   Z-ADD     TCUFS         STPUFS
059200991123     C                   Z-ADD     TCTDB         STPTDB
059300010917     C                   Z-ADD     TCORM         STPORM
059400010917     C                   Z-ADD     TCVAG         STPVAG
059500041209     C                   Z-ADD     TCGML         STPGML
059600041209     C                   Z-ADD     TCPOD         STPPOD
059700041214     C                   Z-ADD     TCTT3         STPTT3
059800011227     C                   Z-ADD     *zeros        STPCDL
059900011227     C                   Z-ADD     *zeros        STPCUL
060000041217     C                   Z-ADD     *zeros        STPTTT
060100041217     C                   Z-ADD     *zeros        STPTTTD
060200991123     C*
060300011227     C                   EVAL      STPDFSM=%inth(TCDFS/GGMEDI)
060400011227     C                   EVAL      STPUFSM=%inth(TCUFS/GGMEDI)
060500011227     C                   EVAL      STPTDBM=%inth(TCTDB/GGMEDI)
060600011227     C                   EVAL      STPORMM=%inth(TCORM/GGMEDI)
060700011227     C                   EVAL      STPVAGM=%inth(TCVAG/GGMEDI)
060800041209     C                   EVAL      STPGMLM=%inth(TCGML/GGMEDI)
060900041209     C                   EVAL      STPPODM=%inth(TCPOD/GGMEDI)
061000041214     C                   EVAL      STPTT3M=%inth(TCTT3/GGMEDI)
061100011227     C                   EVAL      STPCDLM=*zeros
061200011227     C                   EVAL      STPCULM=*zeros
061300041217     C                   EVAL      STPTTTM=*zeros
061400991123     C*
061500991123     C* STAMPO RIGA DETTAGLIO
061600991123     C                   EXSR      STRCLI
061700991123     C                   MOVEL     '1'           $STDAC
061800000000     C*
061900000000     C                   ENDSR
062000000000     C*------------------------------------------------------------------------*
062100000000     C* STAFIL - STAMPA TOTALI FILIALE
062200000000     C*------------------------------------------------------------------------*
062300000000     C     STAFIL        BEGSR
062400000000     C*
062500000000     C* IMPOSTO I VALORI DI STAMPA
062600000103     C                   CLEAR                   ST23RA
062700000103     C                   CLEAR                   ST23RM
062800991124     C*
062900991124     C* DESCRIZIONE TOTALE
063000991124     C     TES(1)        CAT       DESFIL:1      STPRAG
063100991123     C*
063200991123     C                   Z-ADD     TFDFS         STPDFS
063300991123     C                   Z-ADD     TFUFS         STPUFS
063400991123     C                   Z-ADD     TFTDB         STPTDB
063500010917     C                   Z-ADD     TFORM         STPORM
063600010917     C                   Z-ADD     TFVAG         STPVAG
063700011220     C                   Z-ADD     TFCDL         STPCDL
063800011220     C                   Z-ADD     TFCUL         STPCUL
063900041217     C                   Z-ADD     TFTTT         STPTTT
064000041217     C                   Z-ADD     TFTTTD        STPTTTD
064100041209     C                   Z-ADD     TFGML         STPGML
064200041209     C                   Z-ADD     TFPOD         STPPOD
064300041214     C                   Z-ADD     TFTT3         STPTT3
064400991123     C*
064500011227     C                   EVAL      STPDFSM=%inth(TFDFS/GGMEDI)
064600011227     C                   EVAL      STPUFSM=%inth(TFUFS/GGMEDI)
064700011227     C                   EVAL      STPTDBM=%inth(TFTDB/GGMEDI)
064800011227     C                   EVAL      STPORMM=%inth(TFORM/GGMEDI)
064900011227     C                   EVAL      STPVAGM=%inth(TFVAG/GGMEDI)
065000011227     C                   EVAL      STPCDLM=%inth(TFCDL/GGMEDI)
065100011227     C                   EVAL      STPCULM=%inth(TFCUL/GGMEDI)
065200041217     C                   EVAL      STPTTTM=%inth(TFTTT/GGMEDI)
065300041209     C                   EVAL      STPGMLM=%inth(TFGML/GGMEDI)
065400041209     C                   EVAL      STPPODM=%inth(TFPOD/GGMEDI)
065500041214     C                   EVAL      STPTT3M=%inth(TFTT3/GGMEDI)
065600991026     C*
065700000000     C* SE DETTAGLIO
065800000000     C     PARDET        IFEQ      'D'
065900000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
066000991027     C                   ENDIF
066100000000     C*
066200000000     C* STAMPO RIGA TOTALI
066300000000     C                   EXSR      STRTOT
066400000000     C*
066500000000     C* SE DETTAGLIO
066600000000     C     PARDET        IFEQ      'D'
066700000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
066800000000     C                   ENDIF
066900000000     C*
067000991123     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000000     C* STAARE - STAMPA TOTALI AREA
067300000000     C*------------------------------------------------------------------------*
067400000000     C     STAARE        BEGSR
067500991124     C*
067600991124     C     PARDET        IFEQ      'S'                                          *SINTETICA
067700000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
067800991124     C                   ELSE
067900000000     C*
068000000000     C* STAMPO I TOTALI AREA A PAGINA NUOVA SOLO PER STAMPA IN DETTAGLIO
068100000103     C                   WRITE     ST23TE
068200000000     C                   ENDIF
068300000000     C*
068400991123     C                   SETON                                        05        *SALTO PAG SUCCESSIV
068500991124     C*
068600000103     C                   CLEAR                   ST23RA
068700000103     C                   CLEAR                   ST23RM
068800991123     C* DESCRIZIONE TOTALE
068900991124     C     TES(2)        CAT       DESARE:1      STPRAG
069000000000     C*
069100991123     C* IMPOSTO I VALORI DI STAMPA
069200991123     C                   Z-ADD     TADFS         STPDFS
069300991123     C                   Z-ADD     TAUFS         STPUFS
069400991123     C                   Z-ADD     TATDB         STPTDB
069500010917     C                   Z-ADD     TAORM         STPORM
069600010917     C                   Z-ADD     TAVAG         STPVAG
069700011220     C                   Z-ADD     TACDL         STPCDL
069800011220     C                   Z-ADD     TACUL         STPCUL
069900041217     C                   Z-ADD     TATTT         STPTTT
070000041217     C                   Z-ADD     TATTTD        STPTTTD
070100041209     C                   Z-ADD     TAGML         STPGML
070200041209     C                   Z-ADD     TAPOD         STPPOD
070300041214     C                   Z-ADD     TATT3         STPTT3
070400991123     C*
070500011227     C                   EVAL      STPDFSM=%inth(TADFS/GGMEDI)
070600011227     C                   EVAL      STPUFSM=%inth(TAUFS/GGMEDI)
070700011227     C                   EVAL      STPTDBM=%inth(TATDB/GGMEDI)
070800011227     C                   EVAL      STPORMM=%inth(TAORM/GGMEDI)
070900011227     C                   EVAL      STPVAGM=%inth(TAVAG/GGMEDI)
071000011227     C                   EVAL      STPCDLM=%inth(TACDL/GGMEDI)
071100011227     C                   EVAL      STPCULM=%inth(TACUL/GGMEDI)
071200041217     C                   EVAL      STPTTTM=%inth(TATTT/GGMEDI)
071300041209     C                   EVAL      STPGMLM=%inth(TAGML/GGMEDI)
071400041209     C                   EVAL      STPPODM=%inth(TAPOD/GGMEDI)
071500041214     C                   EVAL      STPTT3M=%inth(TATT3/GGMEDI)
071600991026     C*
071700000000     C* STAMPO RIGA TOTALE
071800991124     C                   EXSR      STRTOT
071900000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
072000000000     C*
072100000000     C                   ENDSR
072200000000     C*------------------------------------------------------------------------*
072300000000     C* STADIV - STAMPA TOTALI DIVISIONE
072400000000     C*------------------------------------------------------------------------*
072500000000     C     STADIV        BEGSR
072600000000     C*
072700000000     C* STAMPA TOTALE DIVISIONE A PAGINA NUOVA
072800000103     C                   WRITE     ST23TE
072900991123     C                   SETON                                        05        *SALTO PAG SUCCESSIV
073000991123     C*
073100000103     C                   CLEAR                   ST23RA
073200000103     C                   CLEAR                   ST23RM
073300991123     C* DESCRIZIONE TOTALE
073400991124     C     TES(3)        CAT       DESDIV:1      STPRAG
073500000000     C*
073600991123     C* IMPOSTO I VALORI DI STAMPA
073700991123     C                   Z-ADD     TDDFS         STPDFS
073800991123     C                   Z-ADD     TDUFS         STPUFS
073900991123     C                   Z-ADD     TDTDB         STPTDB
074000010917     C                   Z-ADD     TDORM         STPORM
074100010917     C                   Z-ADD     TDVAG         STPVAG
074200011220     C                   Z-ADD     TDCDL         STPCDL
074300011220     C                   Z-ADD     TDCUL         STPCUL
074400041217     C                   Z-ADD     TDTTT         STPTTT
074500041217     C                   Z-ADD     TDTTTD        STPTTTD
074600041209     C                   Z-ADD     TDGML         STPGML
074700041209     C                   Z-ADD     TDPOD         STPPOD
074800041214     C                   Z-ADD     TDTT3         STPTT3
074900991123     C*
075000011227     C                   EVAL      STPDFSM=%inth(TDDFS/GGMEDI)
075100011227     C                   EVAL      STPUFSM=%inth(TDUFS/GGMEDI)
075200011227     C                   EVAL      STPTDBM=%inth(TDTDB/GGMEDI)
075300011227     C                   EVAL      STPORMM=%inth(TDORM/GGMEDI)
075400011227     C                   EVAL      STPVAGM=%inth(TDVAG/GGMEDI)
075500011227     C                   EVAL      STPCDLM=%inth(TDCDL/GGMEDI)
075600011227     C                   EVAL      STPCULM=%inth(TDCUL/GGMEDI)
075700041217     C                   EVAL      STPTTTM=%inth(TDTTT/GGMEDI)
075800041209     C                   EVAL      STPGMLM=%inth(TDGML/GGMEDI)
075900041209     C                   EVAL      STPPODM=%inth(TDPOD/GGMEDI)
076000041214     C                   EVAL      STPTT3M=%inth(TDTT3/GGMEDI)
076100991026     C*
076200000000     C* STAMPO RIGA TOTALE
076300991123     C                   EXSR      STRTOT
076400000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
076500000000     C*
076600991123     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C* STAGEN - STAMPA TOTALI GENERALI
076900000000     C*------------------------------------------------------------------------*
077000000000     C     STAGEN        BEGSR
077100000000     C*
077200000000     C* STAMPA TOTALE GENERALE A PAGINA NUOVA
077300991124     C                   MOVEL     *BLANKS       STPFIL
077400991124     C                   MOVEL     *BLANKS       STPARE
077500991124     C                   MOVEL     *BLANKS       STPDIV
077600000103     C                   WRITE     ST23TE
077700991124     C*
077800000103     C                   CLEAR                   ST23RA
077900000103     C                   CLEAR                   ST23RM
078000000000     C*
078100000000     C* DESCRIZIONE TOTALE
078200991123     C                   MOVEL     TES(4)        STPRAG
078300000000     C*
078400991123     C* IMPOSTO I VALORI DI STAMPA
078500991123     C                   Z-ADD     TGDFS         STPDFS
078600991123     C                   Z-ADD     TGUFS         STPUFS
078700991123     C                   Z-ADD     TGTDB         STPTDB
078800010917     C                   Z-ADD     TGORM         STPORM
078900010917     C                   Z-ADD     TGVAG         STPVAG
079000011220     C                   Z-ADD     TGCDL         STPCDL
079100011220     C                   Z-ADD     TGCUL         STPCUL
079200041217     C                   Z-ADD     TGTTT         STPTTT
079300041217     C                   Z-ADD     TGTTTD        STPTTTD
079400041209     C                   Z-ADD     TGGML         STPGML
079500041209     C                   Z-ADD     TGPOD         STPPOD
079600041214     C                   Z-ADD     TGTT3         STPTT3
079700991123     C*
079800011227     C                   EVAL      STPDFSM=%inth(TGDFS/GGMEDI)
079900011227     C                   EVAL      STPUFSM=%inth(TGUFS/GGMEDI)
080000011227     C                   EVAL      STPTDBM=%inth(TGTDB/GGMEDI)
080100011227     C                   EVAL      STPORMM=%inth(TGORM/GGMEDI)
080200011227     C                   EVAL      STPVAGM=%inth(TGVAG/GGMEDI)
080300011227     C                   EVAL      STPCDLM=%inth(TGCDL/GGMEDI)
080400011227     C                   EVAL      STPCULM=%inth(TGCUL/GGMEDI)
080500041217     C                   EVAL      STPTTTM=%inth(TGTTT/GGMEDI)
080600041209     C                   EVAL      STPGMLM=%inth(TGGML/GGMEDI)
080700041209     C                   EVAL      STPPODM=%inth(TGPOD/GGMEDI)
080800041214     C                   EVAL      STPTT3M=%inth(TGTT3/GGMEDI)
080900991026     C*
081000000000     C* STAMPO RIGA TOTALE
081100000000     C                   EXSR      STRTOT
081200000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
081300000000     C*
081400000000     C                   ENDSR
081500000000     C*------------------------------------------------------------------------*
081600991124     C* STRCLI - STAMPA RIGA CLIENTE/DATA
081700000000     C*------------------------------------------------------------------------*
081800000000     C     STRCLI        BEGSR
081900991123     C*
082000000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
082100991123     C     *IN05         IFEQ      '1'
082200000000     C                   SETON                                        01
082300000103     C                   WRITE     ST23RI                                       *RIGA DI SEPARAZIONE
082400000103     C                   WRITE     ST23TE
082500991123     C                   SETOFF                                       05
082600000000     C                   END
082700000000     C*
082800000000     C     $STDAC        IFEQ      '1'
082900991123     C                   MOVEL     *BLANKS       DEPRAG
083000991123     C                   Z-ADD     *ZEROS        DEPKSC
083100991123     C                   Z-ADD     *ZEROS        DEPDAT
083200991026     C                   ENDIF
083300000000     C*
083400991123     C* STAMPO RIGA CLIENTE/DATA
083500000103     C                   WRITE     ST23RA
083600991124     C     PARDAI        IFNE      PARDAF
083700000103     C                   WRITE     ST23RM
083800991124     C                   ENDIF
083900000000     C*
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* STRTOT - STAMPA RIGA TOTALE
084300000000     C*------------------------------------------------------------------------*
084400000000     C     STRTOT        BEGSR
084500991027     C*
084600991124     C* STAMPO RIGA CLIENTE/DATA
084700000103     C                   WRITE     ST23RA
084800991124     C     PARDAI        IFNE      PARDAF
084900000103     C                   WRITE     ST23RM
085000991124     C                   ENDIF
085100000000     C*
085200000000     C                   ENDSR
085300000000     C*------------------------------------------------------------------------*
085400000000     C* DECDIV - DECODIFICA DIVISIONE
085500000000     C*------------------------------------------------------------------------*
085600000000     C     DECDIV        BEGSR
085700000000     C*
085800030528     C***                IF        DEPDIV = '0'
085900030528     C***                MOVEL     'LED'         DESDIV
086000030528     C***                ELSE
086100020906     C                   MOVEL     1             KTBKUT
086200000000     C                   MOVE      '17'          KTBCOD
086300000000     C                   MOVE      *BLANKS       KTBKEY
086400000000     C                   MOVEL     DEPDIV        KTBKEY
086500000000     C     KEYTAB        CHAIN     TABEL                              97
086600000000     C     *IN97         IFEQ      '1'
086700000000     C     TBLFLG        OREQ      '*'
086800000000     C                   MOVEL     *ALL'*'       DESDIV           20
086900000000     C                   ELSE
087000000000     C                   MOVEL     TBLUNI        DESDIV
087100001115     C                   ENDIF
087200030528     C***                ENDIF
087300000000     C*
087400000000     C                   ENDSR
087500000000     C*------------------------------------------------------------------------*
087600000000     C* DECARE - DECODIFICA AREA
087700000000     C*------------------------------------------------------------------------*
087800000000     C     DECARE        BEGSR
087900000000     C*
088000030528     C***                IF        DEPARE = 199
088100030528     C***                MOVEL     'LED'         DESARE
088200030528     C***                ELSE
088300020906     C                   MOVEL     1             KTBKUT
088400000000     C                   MOVE      '05'          KTBCOD
088500000000     C                   MOVE      *BLANKS       KTBKEY
088600000000     C                   MOVEL     DEPARE        KTBKEY
088700000000     C     KEYTAB        CHAIN     TABEL                              97
088800000000     C     *IN97         IFEQ      '1'
088900000000     C     TBLFLG        OREQ      '*'
089000000000     C                   MOVEL     *ALL'*'       DESARE           20
089100000000     C                   ELSE
089200000000     C                   MOVEL     TBLUNI        DESARE
089300001115     C                   ENDIF
089400030528     C***                ENDIF
089500000000     C*
089600000000     C                   ENDSR
089700000000     C*------------------------------------------------------------------------*
089800000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
089900000000     C*------------------------------------------------------------------------*
090000000000     C     DECFIL        BEGSR
090100000000     C*
090200030528     C***                IF        DEPFIL = 199
090300030528     C***                MOVEL     'LED'         DESFIL
090400030528     C***                ELSE
090500000000     C                   Z-ADD     1             I
090600000000     C     DEPFIL        LOOKUP    FIL(I)                                 95
090700000000     C     *IN95         IFEQ      '0'
090800000000     C                   MOVEL     *ALL'*'       DESFIL           20
090900000000     C                   ELSE
091000000000     C                   MOVEL     DFIL(I)       DESFIL
091100001115     C                   ENDIF
091200030528     C***                ENDIF
091300991026     C*
091400991123     C                   ENDSR
091500000000     C*------------------------------------------------------------------------*
091600000000     C* *INZSR - ROUTINE INIZIALE
091700000000     C*------------------------------------------------------------------------*
091800000000     C     *INZSR        BEGSR
091900000000     C*--------------------
092000000000     C* RICEVIMENTO PARAMENTRI
092100000000     C*--------------------
092200000000     C     *ENTRY        PLIST
092300000000     C                   PARM                    KPJBA
092400000000     C                   MOVEL     KPJBU         PARAM
092500000000     C*--------------------
092600000000     C* CAMPI RIFERITI AL DATA BASE
092700000000     C*--------------------
092800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
092900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
093000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
093100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
093200050706     C     *LIKE         DEFINE    STLDIV        KWPDIV
093300050706     C     *LIKE         DEFINE    STLARE        KWPARE
093400050706     C     *LIKE         DEFINE    STLFIL        KWPFIL
093500050706     C     *LIKE         DEFINE    STLRAG        KWPRAG
093600050706     C     *LIKE         DEFINE    STLKSC        KWPKSC
093700050706     C     *LIKE         DEFINE    STLDAT        KWPDAT
093800050706     C     *LIKE         DEFINE    STLDIV        DEPDIV
093900050706     C     *LIKE         DEFINE    STLARE        DEPARE
094000050706     C     *LIKE         DEFINE    STLFIL        DEPFIL
094100050706     C     *LIKE         DEFINE    STLRAG        DEPRAG
094200050706     C     *LIKE         DEFINE    STLKSC        DEPKSC
094300050706     C     *LIKE         DEFINE    STLDAT        DEPDAT
094400000000     C*
094500000000     C*--------------------
094600000000     C* CHIAVI DI LETTURA
094700000000     C*--------------------
094800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
094900000000     C     KEYTAB        KLIST
095000000000     C                   KFLD                    KTBKUT
095100000000     C                   KFLD                    KTBCOD
095200000000     C                   KFLD                    KTBKEY
095300000000     C*
095400050706     C* CHIAVE LETTURA SISTL01L - COMPLETA
095500000000     C     KEYWFP        KLIST
095600000000     C                   KFLD                    KWPDIV
095700000000     C                   KFLD                    KWPARE
095800000000     C                   KFLD                    KWPFIL
095900991123     C                   KFLD                    KWPRAG
096000000000     C                   KFLD                    KWPKSC
096100011227     C                   KFLD                    KWPDAT
096200000000     C*--------------------
096300000000     C* CAMPI DI CONTROLLO
096400000000     C*--------------------
096500000000     C                   MOVEL     '0'           FINL00            1            *FINE FILE
096600991123     C                   SETON                                        05        *OVERFLOW
096700000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
096800000000     C                   TIME                    ORA               6 0          *ORA
096900000000     C                   MOVEL     '0'           $STDAC            1            *STAMPATI DATI CLI.
097000991124     C                   MOVEL     *BLANKS       SAVDET            1
097100011227     C*
097200011227     C* SPENGO GLI INDICATORI DI OPZIONE SU CAMPI/COSTANTI CHE NN VOGLIO VISUALIZZARE IN STAMPA
097300011227     C                   SETOFF                                       15
097400991124     C*
097500991124     C* CONSIDERAZIONI SUL TIPO DI LANCIO RICHIESTO
097600041217     C*    PARDET        IFEQ      'S'
097700041217     C*                  MOVEL     *BLANKS       STPDAT
097800041217     C*                  MOVEL     'SINTETICA'   STPTIP
097900041217     C*                  ENDIF
098000991124     C     PARDET        IFEQ      'D'
098100991124     C                   MOVEL     'CLIENTE'     STPCCC
098200041217     C***                MOVEL     'DATA'        STPDDD
098300991124     C                   MOVEL     'DETTAGLIO'   STPTIP
098400991124     C                   ENDIF
098500991124     C     PARDET        IFEQ      'T'
098600991124     C                   MOVEL     *BLANKS       STPDDD
098700991124     C                   MOVEL     ' TOTALI  '   STPTIP
098800991124     C                   ENDIF
098900991124     C*
099000041217     C     PARDET        IFEQ      'D'
099100991124     C                   MOVEL     'S'           SAVDET
099200991124     C                   MOVEL     'D'           PARDET
099300991124     C                   ENDIF
099400991124     C*
099500991124     C     PARDET        IFEQ      'T'
099600991124     C                   MOVEL     'T'           SAVDET
099700991124     C                   MOVEL     'S'           PARDET
099800991124     C                   ENDIF
099900991123     C*
100000991123     C* CALCOLO IL NUMERO DI GG TRA LE DUE DATE DI RANGE PASSATE
100100991123     C                   MOVEL     '-'           SEP1
100200991123     C                   MOVEL     '-'           SEP2
100300991123     C                   Z-ADD     PARDAI        INVDATA
100400991123     C                   MOVE      AAINV         AAAAA
100500991123     C                   MOVE      MMINV         MMA
100600991123     C                   MOVE      GGINV         GGA
100700991123     C                   MOVEL     DATAA         DATAINZ
100800991123     C*
100900991123     C                   Z-ADD     PARDAF        INVDATA
101000991123     C                   MOVE      AAINV         AAAAA
101100991123     C                   MOVE      MMINV         MMA
101200991123     C                   MOVE      GGINV         GGA
101300991123     C                   MOVEL     DATAA         DATAFIN
101400991123     C*
101500991123     C     DATAFIN       SUBDUR    DATAINZ       GGMEDI:*D         5 0
101600991123     C                   ADD       1             GGMEDI
101700011228     C*
101800011228     C* SOVRAPPONGO IL NUMERO DI GIORNI PRESI A VIDEO NEL LANCIO
101900011228     C                   Z-ADD     PARGIO        GGMEDI
102000991124     C*
102100991124     C* VALORIZZO CAMPI DI TESTATA DELLA STAMPA
102200991124     C                   Z-ADD     PARDAI        INVDATA
102300991124     C                   Z-ADD     AAINV         AA
102400991124     C                   Z-ADD     MMINV         MM
102500991124     C                   Z-ADD     GGINV         GG
102600991124     C                   Z-ADD     GIRDATA       STPDAI
102700000000     C*
102800991124     C                   Z-ADD     PARDAF        INVDATA
102900991124     C                   Z-ADD     AAINV         AA
103000991124     C                   Z-ADD     MMINV         MM
103100991124     C                   Z-ADD     GGINV         GG
103200991124     C                   Z-ADD     GIRDATA       STPDAF
103300991124     C*
103400991124     C                   Z-ADD     GGMEDI        STPGG
103500000000     C*
103600000000     C* CARICA ORGANIGRAMMA
103700000000     C                   EXSR      CARORG
103800000000     C*
103900000000     C                   ENDSR
104000000000     C*------------------------------------------------------------------------*
104100000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
104200000000     C*------------------------------------------------------------------------*
104300000000     C     CARORG        BEGSR
104400000000     C*
104500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
104600030528     C     *LOVAL        SETLL     AZORG01L                           98        *FINE-ARCHIV
104700000000     C  N98              READ      AZORG01L                               98
104800000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
104900000000     C     *IN98         DOWEQ     '0'                                          --- 1 -->
105000030528     C***  ORGFAG        IFNE      'V'
105100000000     C                   ADD       1             I
105200000000     C                   Z-ADD     ORGFIL        FIL(I)
105300000000     C                   MOVEL     ORGDES        DFIL(I)
105400030528     C***                ENDIF
105500000000     C                   READ      AZORG01L                               98
105600030528     C                   ENDDO                                                  <-- 1 ---
105700000000     C*
105800000000     C                   ENDSR
105900991123     C*
106000991123     C*------------------------------------------------------------------------*
106100991123     C*
106200000000** -TES-   (DESCRIZIONI  STAMPA)
106300991123TOT P.O.                                                            1
106400991123TOT AREA                                                            2
106500991123TOT DIS.                                                            3
106600991123TOT GENERALE                                                        4
106700000000** -INT-   (INTESTAZIONE)
106800000000DETTAGLIO                                                       1
106900000000SINTETICA                                                       2
1070000000003
1071000000004
1072000000005
