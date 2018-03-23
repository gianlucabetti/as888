000100000000     H*------------------------------------------------------------------------*
000200000000     H* STAMPA STATISTICA PESI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FWFPES01L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100080110     ftntam01l  if   e           k disk
001200000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300000000     F                                     FORMLEN(66)
001400000000     F                                     FORMOFL(60)
001500000000     L*------------------------------------------------------------------------*
001600080110     d ptap            s              1
001601090702     d stdes3          s             60
001602090702     d ii              s              2  0
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*-------------------
002100000000     D* INTESTAZIONI STAMPA
002200000000     D*-------------------
002300090630     D TES             S             66    DIM(21) CTDATA PERRCD(1)             RIGHE TESTATA
002400000000     D INT             S              9    DIM(5) CTDATA PERRCD(1)              DECODIFICA INTESTAZI
002500000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)             DECODIFICHE RIGHE
002600000000     D*-------------------
002700000000     D* SCHIERA FILIALI
002800000000     D*-------------------
002900000000     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
003000000000     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
003100020627     D PFIL            S              3    DIM(500)                             *NETWORK POSTE
003101090702      *
003102090702     d CTR             S              1    dim(10)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500991025     D*-------------------
003600991025     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
003700991025     D*-------------------
003800991025     D OG150         E DS
003900000706     D*-------------------
004000000706     D* LETTURA CAMPO ORGDE£ DI AZORG
004100000706     D*-------------------
004200000706     D OG143         E DS
004300000000     D*-------------------
004400000000     D* ARCHITETTURA
004500000000     D*-------------------
004600000000     D KPJBA         E DS
004700000000     D*------------------
004800000000     D* DS REPERIMENTO DATI UTENTE
004900000000     D*-------------------
005000000000     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005100000000     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005200000000     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005300000000     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005400000000     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005500080110
005600080110     d dsta01        e ds
005601090702     d dstr          e ds
005700080110
005800991025     D*-------------------
005900991025     D* DS PER SCOMPOSIZIONE CLIENTI
006000991025     D*-------------------
006100991025     D                 DS                  INZ
006200991025     D  KORG                   1      3  0
006300991025     D  CODCLI                 1      7  0
006400020910     D*------------------
006500020910     D* DS REPEIMENTO TERMINALs
006600020910     D*------------------
006700020910     D FNLV55DS      E DS
006800020910     D*------------------
006900020910     D* DS CONTROLLO DATA (8)
007000020910     D*------------------
007100020910     D WLBDA8          DS                  INZ
007200020910     D  G08DAT                 1      8  0
007300020910     D  G08INV                 9     16  0
007400020910     D  G08ERR                17     17
007500020910     D  G08TGI                18     22  0
007600000000     D*-------------------
007700000000     D* DS PARAMETRI DI LANCIO PGM
007800000000     D*-------------------
007900000000     D PARAM           DS
008000000000     D  PARTLA                 1      1
008100000000     D  PARANN                 2      5  0
008200000000     D  PARMMI                 6      7  0
008300000000     D  PARMMF                 8      9  0
008400000000     D  PARSNR                10     10
008500000000     D  PARDTI                11     18  0
008600000000     D  PARDTF                19     26  0
008700991025     D  PARPEI                27     31  2
008800000000     D  PARDIV                32     32
008900000000     D  PARARE                33     35  0
009000000000     D  PARFIL                36     38  0
009100000000     D  PARDET                39     39
009200000000     D  PARTOT                40     40
009300000000     D  PARRG1                41     44  1
009400000000     D  PARRG2                45     48  1
009500000000     D  PARTAR                49     53  3
009600991025     D  PARPES                54     58  2
009700991229     D  PARMIN                59     63  3
009800000706     D  PARPOS                64     64
009900050124     D  PARSAL                66     66
010000000000     C*------------------------------------------------------------------------*
010100000000     C* MAIN LINE
010200000000     C*------------------------------------------------------------------------*
010300000000     C*
010400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
010500000000     C                   EXSR      SETFIL
010600000000     C*
010700000000     C* CICLO FINO A FINE FILE DI LAVORO
010800000000DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
010900000000     C*
011000000000     C* OPERAZIONI PER NUOVA DIVISIONE
011100000000     C                   EXSR      NEWDIV
011200000000     C*
011300000000     C* CICLO FINO A ROTTURA DI DIVISIONE
011400000000DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
011500000000     C     WFPDIV        ANDEQ     DEPDIV
011600000000     C*
011700000000     C* OPERAZIONI PER NUOVA AREA
011800000000     C                   EXSR      NEWARE
011900000000     C*
012000000000     C* CICLO FINO A ROTTURA DI AREA
012100000000DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
012200000000     C     WFPDIV        ANDEQ     DEPDIV
012300000000     C     WFPARE        ANDEQ     DEPARE
012400000000     C*
012500000000     C* OPERAZIONI PER NUOVA FILIALE
012600000000     C                   EXSR      NEWFIL
012700000000     C*
012800000000     C* CICLO FINO A ROTTURA DI FILIALE
012900000000DO  4C     FINL00        DOWEQ     '0'                                          --- 4 -->
013000000000     C     WFPDIV        ANDEQ     DEPDIV
013100000000     C     WFPARE        ANDEQ     DEPARE
013200000000     C     WFPFIL        ANDEQ     DEPFIL
013300000000     C*
013400000000     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
013500991026     C                   EXSR      NEWPRO
013600991026     C*
013700991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
013800991026DO  5C     FINL00        DOWEQ     '0'                                          --- 5 -->
013900991026     C     WFPDIV        ANDEQ     DEPDIV
014000991026     C     WFPARE        ANDEQ     DEPARE
014100991026     C     WFPFIL        ANDEQ     DEPFIL
014200991026     C     WFPKSC        ANDEQ     DEPKSC
014300991026     C     WFPCTR        ANDEQ     DEPCTR
014400991026     C*
014500991026     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
014600991026     C                   EXSR      NEWCLI
014700000000     C*
014800991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
014900991026DO  6C     FINL00        DOWEQ     '0'                                          --- 6 -->
015000000000     C     WFPDIV        ANDEQ     DEPDIV
015100000000     C     WFPARE        ANDEQ     DEPARE
015200000000     C     WFPFIL        ANDEQ     DEPFIL
015300000000     C     WFPKSC        ANDEQ     DEPKSC
015400991026     C     WFPCTR        ANDEQ     DEPCTR
015500991026     C     WFPMES        ANDEQ     DEPMES
015600000000     C*
015700000000     C* MEMORIZZA LE SPEDIZIONI
015800000517     C     WRECOK        IFEQ      'S'
015900000000     C                   EXSR      MEMSPE
016000000517     C                   ENDIF
016100000000     C*
016200000000     C* LEGGE IL RECORD SUCCESSIVO
016300000000     C                   EXSR      LETFIL
016400000000     C*
016500991026E   6C                   ENDDO                                                  <-- 6 ---
016600050125     C*
016700050125     C* PROGRESSIVO: STAMPA IL TOTALE CLIENTE/TARIFFA AL MESE (Spostato qui)
016800050125     C                   EXSR      STACLI
016900000000     C*
017000991026E   5C                   ENDDO                                                  <-- 5 ---
017100991026     C*
017200991026     C* PROGRESSIVO: STAMPA IL TOTALE CLIENTE/TARIFFA AL MESE
017300050125     C********           EXSR      STACLI
017400991026E   4C                   ENDDO                                                  <-- 4 ---
017500000000     C*
017600000000     C* STAMPA IL TOTALE FILIALE
017700000000     C                   EXSR      STAFIL
017800000000     C*
017900000000E   3C                   ENDDO                                                  <-- 3 ---
018000000000     C*
018100000000     C* STAMPA IL TOTALE AREA
018200000000     C     PARTOT        IFEQ      'S'
018300000000     C                   EXSR      STAARE
018400000000     C                   ENDIF
018500000000     C*
018600000000E   2C                   ENDDO                                                  <-- 2 ---
018700000000     C*
018800000000     C* STAMPA IL TOTALE DIVISIONE
018900000000     C     PARTOT        IFEQ      'S'
019000000000     C                   EXSR      STADIV
019100000000     C                   ENDIF
019200000000     C*
019300000000E   1C                   ENDDO                                                  <-- 1 ---
019400000000     C*
019500000000     C* STAMPA IL TOTALE GENERALE
019600000000     C     PARTOT        IFEQ      'S'
019700000000     C                   EXSR      STAGEN
019800000000     C                   ENDIF
019900000000     C*
020000000000     C* STAMPA RIGA FINE STAMPA
020100000000     C                   EXCEPT    STAFIN
020200000000     C*
020300000000     C* OPERAZIONI FINALI
020400000000     C                   EXSR      FINSR
020500000000     C*
020600000000     C                   SETON                                        LR
020700000000     C*------------------------------------------------------------------------*
020800000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
020900000000     C*------------------------------------------------------------------------*
021000000000     C     SETFIL        BEGSR
021100000000     C*
021200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
021300000000     C                   Z-ADD     PARANN        KWPANN
021400000000     C                   MOVEL     *BLANKS       KWPDIV
021500000000     C                   Z-ADD     *ZEROS        KWPARE
021600000000     C                   Z-ADD     *ZEROS        KWPFIL
021700000000     C                   Z-ADD     *ZEROS        KWPKSC
021800000000     C                   Z-ADD     *ZEROS        KWPCTR
021900000000     C                   Z-ADD     *ZEROS        KWPMES
022000000000     C     KEYWFP        SETLL     WFPES01L                           99        *FINE-ARCHIV
022100000000     C*
022200000000     C     *IN99         IFEQ      '1'
022300000000     C                   MOVEL     '1'           FINL00
022400000000     C                   ELSE
022500000000     C                   EXSR      LETFIL
022600000000     C                   END
022700000000     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* LETFIL - LETTURA PROSSIMO RECORD
023100000000     C*------------------------------------------------------------------------*
023200000000     C     LETFIL        BEGSR
023300000000     C*
023400000000     C* LEGGO FINO A:
023500000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
023600000000     C*     - TROVATO RECORD VALIDO
023700000000     C                   MOVEL     'N'           WRECOK
023800040213     C                   MOVEL     'N'           WRECOKC
023900000000     C*
024000000000     C* LETTURE SUCCESSIVE ALLA PRIMA
024100000000     C                   READ      WFPES01L                               99
024200000000     C*
024300000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
024400000000     C     WFPANN        ORNE      PARANN                                       *SUPERATO ANNO
024500000000     C                   MOVEL     '1'           FINL00
024600000000     C                   ELSE
024700000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
024800000517     C                   ENDIF
024900000000     C*
025000000000     C                   ENDSR
025100000000     C*------------------------------------------------------------------------*
025200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
025300000000     C*------------------------------------------------------------------------*
025400991025     C     CHKREC        BEGSR
025500000000     C*
025600000000     C                   MOVEL     'S'           WRECOK            1
025700040213     C                   MOVEL     'S'           WRECOKC           1
025800000000     C*
025900000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
026000000000     C     WFPMES        IFGE      PARMMI
026100000000     C     WFPMES        ANDLE     PARMMF
026200000000     C                   ELSE
026300000000     C                   MOVEL     'N'           WRECOK
026400000000     C                   END
026500000706     C*
026600000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
026700000706IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
026800000706     C                   Z-ADD     1             I
026900000706     C     WFPFIL        LOOKUP    FIL(I)                                 95
027000000706IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
027100020627IF  3C     PFIL(I)       IFEQ      'PPT'                                        *FILIALE POSTE
027200000706     C                   MOVEL     'N'           WRECOK
027300000706E   3C                   ENDIF
027400000706X   2C                   ELSE                                                   *SI DATI POSTE
027500020627IF  3C     PFIL(I)       IFNE      'PPT'                                        *NO FILIALE POSTE
027600000706     C                   MOVEL     'N'           WRECOK
027700000706E   3C                   ENDIF
027800000706E   2C                   ENDIF
027900000706E   1C                   ENDIF
028000000000     C*
028100000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
028200000000     C     PARDIV        IFNE      *BLANKS
028300000000     C     WFPDIV        IFNE      PARDIV
028400000000     C                   MOVEL     'N'           WRECOK
028500000000     C                   END
028600000000     C                   END
028700000000     C*
028800000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
028900000000     C     PARARE        IFNE      *ZEROS
029000000000     C     WFPARE        IFNE      PARARE
029100000000     C                   MOVEL     'N'           WRECOK
029200000000     C                   END
029300000000     C                   END
029400000000     C*
029500000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
029600000000     C     PARFIL        IFNE      *ZEROS
029700000000     C     WFPFIL        IFNE      PARFIL
029800000000     C                   MOVEL     'N'           WRECOK
029900000000     C                   END
030000000000     C                   END
030100000000     C*
030200000000     C* CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLLETTATI
030300991025     C     WFPNCV        IFGT      *ZEROS
030400991027     C                   Z-ADD     WFPTCL        WCPNCL           12 1
030500991027     C                   Z-ADD     WFPNCV        WCPNCV           12 1
030600000000     C                   EXSR      CALPER
030700000000     C                   Z-ADD     WCPPVA        WPER              6 2
030800991025     C                   ELSE
030900991025     C                   MOVEL     'N'           WRECOK
031000991025     C                   END
031100000000     C*
031200000000     C* CALCOLA PERCENTUALE PESO VDL SU PESO BOLLETTATO
031300991025     C     WFPPSC        IFGT      *ZEROS
031400991027     C                   Z-ADD     WFPPSC        WCPPSC           13 1
031500991027     C                   Z-ADD     WFPPSB        WCPPSB           13 1
031600000000     C                   EXSR      CALRNG
031700000000     C                   Z-ADD     WCPRVA        WRNG              6 2
031800991025     C                   ELSE
031900040213     C                   MOVEL     'N'           WRECOK
032000991025     C                   END
032100000000     C*
032200991229     C* SE LA PERCENTUALE (B) E' INFERIORE ALLA MINIMA RICHIESTA,SCARTO
032300040309     C     WCPPVA        IFLT      PARXXX
032400040309     C     WCPPVA        OREQ      *ZEROS
032500040309     C                   MOVEL     'N'           WRECOKC
032600040309     C                   END
032700000000     C*
032800000517     C* SE LA PERCENTUALE (A) NON E' COMPRESA NEL RANGE INDICATO,SCARTO
032900991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
033000991229     C* DELLO SCARTO MINIMO (PARMIN)
033100040309     C                   Z-ADD     *zeros        PERC              6 2
033200040309     C                   EVAL      PERC=%abs(WCPRVA)
033300040309     C     WCPRVA        IFLT      PARRG1
033400040309     C     WCPRVA        ORGT      PARRG2
033500040309     C     WCPRVA        OREQ      *ZEROS
033600040309     C     PERC          ORLT      PARMIN
033700040309     C                   MOVEL     'N'           WRECOKC
033800040309     C                   END
033900000000     C*
034000000000     C                   ENDSR
034100000000     C*------------------------------------------------------------------------*
034200000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
034300000000     C*------------------------------------------------------------------------*
034400000000     C     MEMSPE        BEGSR
034500040303     c*
034600040303     C* TOTALE CLIENTE/TARIFFA
034700040303     C                   Z-ADD     0             TCNCV
034800040308     C                   Z-ADD     0             TCNCL
034900040308     C                   Z-ADD     0             TCNSP
035000040303     C                   Z-ADD     0             TCPSB
035100040303     C                   Z-ADD     0             TCPSC
035200040213     C*---
035300040213     c* MESE
035400040213     C*---
035500040213if  1C     DEPMES        IFEQ      PARMMF
035600991028     C* TOTALE CLIENTE/TARIFFA
035700040213IF  2C     WRECOKC       IFEQ      'S'
035701090702     c     ptap          oreq      'M'
035702090702     c     wfppsc        andgt     wfppsb
035800991027     C                   ADD       WFPNCV        TCNCV
035900991027     C                   ADD       WFPTCL        TCNCL
036000991027     C                   ADD       WFPNSP        TCNSP
036100991027     C                   ADD       WFPPSB        TCPSB
036200991027     C                   ADD       WFPPSC        TCPSC
036300040213E   2C                   ENDIF
036400040213     C                   ADD       WFPPSB        TCPSBT
036500040213     C                   ADD       WFPPSC        TCPSCT
036600991027     C* TOTALE FILIALE
036700991027     C                   ADD       WFPNCV        TFNCV
036800991027     C                   ADD       WFPTCL        TFNCL
036900991027     C                   ADD       WFPNSP        TFNSP
037000991027     C                   ADD       WFPPSB        TFPSB
037100991027     C                   ADD       WFPPSC        TFPSC
037200040213     C                   ADD       WFPPSB        TFPSBT
037300040213     C                   ADD       WFPPSC        TFPSCT
037400991027     C* TOTALE AREA
037500040308     C                   ADD       WFPNCV        TANCV
037600040308     C                   ADD       WFPTCL        TANCL
037700991027     C                   ADD       WFPNSP        TANSP
037800991027     C                   ADD       WFPPSB        TAPSB
037900991027     C                   ADD       WFPPSC        TAPSC
038000040213     C                   ADD       WFPPSB        TAPSBT
038100040213     C                   ADD       WFPPSC        TAPSCT
038200991027     C* TOTALE DIVISIONE
038300991027     C                   ADD       WFPNCV        TDNCV
038400991027     C                   ADD       WFPTCL        TDNCL
038500991027     C                   ADD       WFPNSP        TDNSP
038600991027     C                   ADD       WFPPSB        TDPSB
038700991027     C                   ADD       WFPPSC        TDPSC
038800040213     C                   ADD       WFPPSB        TDPSBT
038900040213     C                   ADD       WFPPSC        TDPSCT
039000991027     C* TOTALE GENERALE
039100040308     C                   ADD       WFPNCV        TGNCV             9 0
039200991027     C                   ADD       WFPTCL        TGNCL             9 0
039300991027     C                   ADD       WFPNSP        TGNSP             8 0
039400991027     C                   ADD       WFPPSB        TGPSB            10 1
039500991027     C                   ADD       WFPPSC        TGPSC            10 1
039600040213     C                   ADD       WFPPSB        TGPSBT           10 1
039700040213     C                   ADD       WFPPSC        TGPSCT           10 1
039800040213E   1C                   ENDIF
039900991027     C*-------------------------------
040000991027     C* INCREMENTO COMUNQUE X PROGRESSIVO
040100991027     C*-------------------------------
040200040213     C*---
040300040213     c* PROGRESSIVO
040400040213     C*---
040500040213     C* TOTALE CLIENTE/TARIFFA
040600040213IF  1C     WRECOKC       IFEQ      'S'
040601090702     c     ptap          oreq      'M'
040700991026     C                   ADD       WFPNCV        TPNCV
040800991027     C                   ADD       WFPTCL        TPNCL
040900040308     C                   ADD       WFPNSP        TPNSP
041000991026     C                   ADD       WFPPSB        TPPSB
041100991026     C                   ADD       WFPPSC        TPPSC
041200040213E   1C                   ENDIF
041300040213     C                   ADD       WFPPSB        TPPSBT
041400040213     C                   ADD       WFPPSC        TPPSCT
041500040213     C* TOTALE FILIALE
041600040213     C                   ADD       WFPNCV        TFNCVP
041700040213     C                   ADD       WFPTCL        TFNCLP
041800040213     C                   ADD       WFPNSP        TFNSPP
041900040213     C                   ADD       WFPPSB        TFPSBP
042000040308     C                   ADD       WFPPSC        TFPSCP
042100040213     C                   ADD       WFPPSB        TFPSBPT
042200040213     C                   ADD       WFPPSC        TFPSCPT
042300040213     C* TOTALE AREA
042400040213     C                   ADD       WFPNCV        TANCVP
042500040213     C                   ADD       WFPTCL        TANCLP
042600040301     C                   ADD       WFPNSP        TANSPP
042700040213     C                   ADD       WFPPSB        TAPSBP
042800040213     C                   ADD       WFPPSC        TAPSCP
042900040213     C                   ADD       WFPPSB        TAPSBPT
043000040213     C                   ADD       WFPPSC        TAPSCPT
043100040213     C* TOTALE DIVISIONE
043200040213     C                   ADD       WFPNCV        TDNCVP
043300040227     C                   ADD       WFPTCL        TDNCLP
043400040227     C                   ADD       WFPNSP        TDNSPP
043500040227     C                   ADD       WFPPSB        TDPSBP
043600040227     C                   ADD       WFPPSC        TDPSCP
043700040227     C                   ADD       WFPPSB        TDPSBPT
043800040213     C                   ADD       WFPPSC        TDPSCPT
043900040213     C* TOTALE GENERALE
044000040308     C                   ADD       WFPNCV        TGNCVP            9 0
044100040308     C                   ADD       WFPTCL        TGNCLP            9 0
044200040308     C                   ADD       WFPNSP        TGNSPP            8 0
044300040308     C                   ADD       WFPPSB        TGPSBP           10 1
044400040213     C                   ADD       WFPPSC        TGPSCP           10 1
044500040213     C                   ADD       WFPPSB        TGPSBPT          10 1
044600040213     C                   ADD       WFPPSC        TGPSCPT          10 1
044700000000     C*
044800000000     C                   ENDSR
044900000000     C*------------------------------------------------------------------------*
045000000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
045100000000     C*------------------------------------------------------------------------*
045200000000     C     NEWDIV        BEGSR
045300000000     C*
045400000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
045500000000     C                   MOVEL     WFPDIV        DEPDIV
045600000000     C*
045700000000     C* DECODIFICA LA DIVISIONE
045800000000     C                   EXSR      DECDIV
045900000000     C*
046000000000     C* AZZERA I TOTALI DELLA DIVISIONE
046100000000     C                   Z-ADD     *ZEROS        TDNCV             9 0
046200000000     C                   Z-ADD     *ZEROS        TDNCL             9 0
046300040227     C                   Z-ADD     *ZEROS        TDNSP             8 0
046400040227     C                   Z-ADD     *ZEROS        TDPSB            10 1
046500040227     C                   Z-ADD     *ZEROS        TDPSC            10 1
046600040227     C*
046700991026     C                   Z-ADD     *ZEROS        TDNCVP            9 0
046800991026     C                   Z-ADD     *ZEROS        TDNCLP            9 0
046900040308     C                   Z-ADD     *ZEROS        TDNSPP            8 0
047000991026     C                   Z-ADD     *ZEROS        TDPSBP           10 1
047100000517     C                   Z-ADD     *ZEROS        TDPSCP           10 1
047200040213     C*
047300000517     C                   Z-ADD     *ZEROS        TDPSBT           10 1
047400000517     C                   Z-ADD     *ZEROS        TDPSCT           10 1
047500040213     C*
047600000517     C                   Z-ADD     *ZEROS        TDPSBPT          10 1
047700000517     C                   Z-ADD     *ZEROS        TDPSCPT          10 1
047800000000     C*
047801090701     C                   Z-ADD     *ZEROS        TDNCV_m           9 0
047802090701     C                   Z-ADD     *ZEROS        TDNCL_m           9 0
047803090701     C                   Z-ADD     *ZEROS        TDNSP_m           8 0
047804090701     C                   Z-ADD     *ZEROS        TDPSB_m          10 1
047805090701     C                   Z-ADD     *ZEROS        TDPSC_m          10 1
047806090701     C*
047807090701     C                   Z-ADD     *ZEROS        TDNCVP_m          9 0
047808090701     C                   Z-ADD     *ZEROS        TDNCLP_m          9 0
047809090701     C                   Z-ADD     *ZEROS        TDNSPP_m          8 0
047810090701     C                   Z-ADD     *ZEROS        TDPSBP_m         10 1
047811090701     C                   Z-ADD     *ZEROS        TDPSCP_m         10 1
047812090701     C*
047813090701     C                   Z-ADD     *ZEROS        TDPSBT_m         10 1
047814090701     C                   Z-ADD     *ZEROS        TDPSCT_m         10 1
047815090701     C*
047816090701     C                   Z-ADD     *ZEROS        TDPSBPT_m        10 1
047817090701     C                   Z-ADD     *ZEROS        TDPSCPT_m        10 1
047900000000     C                   ENDSR
048000000000     C*------------------------------------------------------------------------*
048100000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
048200000000     C*------------------------------------------------------------------------*
048300000000     C     NEWARE        BEGSR
048400000000     C*
048500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
048600000000     C                   Z-ADD     WFPARE        DEPARE
048700000000     C*
048800000000     C* DECODIFICA L'AREA
048900000000     C                   EXSR      DECARE
049000040301     C*
049100000000     C* AZZERA I TOTALI DELL'AREA
049200000000     C                   MOVE      *ZEROS        TANCV             9 0
049300040301     C                   MOVE      *ZEROS        TANCL             9 0
049400040301     C                   MOVE      *ZEROS        TANSP             8 0
049500000000     C                   MOVE      *ZEROS        TAPSB            10 1
049600040213     C                   MOVE      *ZEROS        TAPSC            10 1
049700040213     C*
049800991026     C                   MOVE      *ZEROS        TANCVP            9 0
049900991026     C                   MOVE      *ZEROS        TANCLP            9 0
050000991026     C                   MOVE      *ZEROS        TANSPP            8 0
050100991026     C                   MOVE      *ZEROS        TAPSBP           10 1
050200000517     C                   MOVE      *ZEROS        TAPSCP           10 1
050300040213     C*
050400000517     C                   Z-ADD     *ZEROS        TAPSBT           10 1
050500000517     C                   Z-ADD     *ZEROS        TAPSCT           10 1
050600040213     C*
050700000517     C                   Z-ADD     *ZEROS        TAPSBPT          10 1
050800000517     C                   Z-ADD     *ZEROS        TAPSCPT          10 1
050900000000     C*
050901090701     C                   MOVE      *ZEROS        TANCV_m           9 0
050902090701     C                   MOVE      *ZEROS        TANCL_m           9 0
050903090701     C                   MOVE      *ZEROS        TANSP_m           8 0
050904090701     C                   MOVE      *ZEROS        TAPSB_m          10 1
050905090701     C                   MOVE      *ZEROS        TAPSC_m          10 1
050906090701     C*
050907090701     C                   MOVE      *ZEROS        TANCVP_m          9 0
050908090701     C                   MOVE      *ZEROS        TANCLP_m          9 0
050909090701     C                   MOVE      *ZEROS        TANSPP_m          8 0
050910090701     C                   MOVE      *ZEROS        TAPSBP_m         10 1
050911090701     C                   MOVE      *ZEROS        TAPSCP_m         10 1
050912090701     C*
050913090701     C                   Z-ADD     *ZEROS        TAPSBT_m         10 1
050914090701     C                   Z-ADD     *ZEROS        TAPSCT_m         10 1
050915090701     C*
050916090701     C                   Z-ADD     *ZEROS        TAPSBPT_m        10 1
050917090701     C                   Z-ADD     *ZEROS        TAPSCPT_m        10 1
051000000000     C                   ENDSR
051100000000     C*------------------------------------------------------------------------*
051200000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
051300000000     C*------------------------------------------------------------------------*
051400000000     C     NEWFIL        BEGSR
051500000000     C*
051600000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
051700000000     C                   Z-ADD     WFPFIL        DEPFIL
051800020910     C*
051900020910     C* REPERISCE IL TERMINAL PARTENZA
052000020910     C                   CLEAR                   FNLV55DS
052100020910     C                   MOVEL     'P'           D55TPT
052200020910     C                   MOVEL     DEPFIL        D55LIN
052300020910     C                   Z-ADD     DATCOR        D55DRF
052400020910     C                   CALL      'FNLV55R'
052500020910     C                   PARM                    FNLV55DS
052600020910IF  1C     D55ERR        IFNE      *BLANKS
052700020910     C                   MOVEL     DEPFIL        D55TFP
052800020910E   1C                   ENDIF
052900020910     C                   Z-ADD     D55TFP        DEPTFP
053000991027     C*
053100020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
053200020910     C     DEPTFP        CHAIN     AZORG01L                           88
053300040301     C     *IN88         IFEQ      *OFF
053400040301     C                   MOVEL     ORGDF0        OG150
053500040301     C     §OGPES        IFNE      *BLANKS
053600080311     C     §OGpes        andNE     *all'0'
053700040301     C                   MOVEL     DEC(3)        CHKVDL           20
053800991027     C                   Z-ADD     PARPEI        PARXXX            5 2
053900991027     C                   ELSE
054000991027     C                   MOVEL     DEC(4)        CHKVDL           20
054100991027     C                   Z-ADD     PARPES        PARXXX            5 2
054200991027     C                   ENDIF
054300991027     C                   ELSE
054400040227     C                   MOVEL     DEC(4)        CHKVDL           20
054500040227     C                   Z-ADD     PARPES        PARXXX            5 2
054600040227     C                   ENDIF
054700000000     C*
054800000000     C* AZZERA I TOTALI DELLA FILIALE
054900000000     C                   Z-ADD     *ZEROS        TFNCV             9 0
055000000000     C                   Z-ADD     *ZEROS        TFNCL             9 0
055100040301     C                   Z-ADD     *ZEROS        TFNSP             8 0
055200000000     C                   Z-ADD     *ZEROS        TFPSB            10 1
055300000000     C                   Z-ADD     *ZEROS        TFPSC            10 1
055400040213     C*
055500991026     C                   Z-ADD     *ZEROS        TFNCVP            9 0
055600991026     C                   Z-ADD     *ZEROS        TFNCLP            9 0
055700991026     C                   Z-ADD     *ZEROS        TFNSPP            8 0
055800991026     C                   Z-ADD     *ZEROS        TFPSBP           10 1
055900000517     C                   Z-ADD     *ZEROS        TFPSCP           10 1
056000040213     C*
056100000517     C                   Z-ADD     *ZEROS        TFPSBT           10 1
056200000517     C                   Z-ADD     *ZEROS        TFPSCT           10 1
056300040213     C*
056400000517     C                   Z-ADD     *ZEROS        TFPSBPT          10 1
056500000517     C                   Z-ADD     *ZEROS        TFPSCPT          10 1
056501090630     c* totali di cui ....
056502090630     C                   Z-ADD     *ZEROS        TFNCV_m           9 0
056503090630     C                   Z-ADD     *ZEROS        TFNCL_m           9 0
056504090630     C                   Z-ADD     *ZEROS        TFNSP_m           8 0
056505090630     C                   Z-ADD     *ZEROS        TFPSB_m          10 1
056506090630     C                   Z-ADD     *ZEROS        TFPSC_m          10 1
056507090630     C*
056508090630     C                   Z-ADD     *ZEROS        TFNCVP_m          9 0
056509090630     C                   Z-ADD     *ZEROS        TFNCLP_m          9 0
056510090630     C                   Z-ADD     *ZEROS        TFNSPP_m          8 0
056511090630     C                   Z-ADD     *ZEROS        TFPSBP_m         10 1
056512090630     C                   Z-ADD     *ZEROS        TFPSCP_m         10 1
056513090630     C*
056514090701     C                   Z-ADD     *ZEROS        TFPSBT_m         10 1
056515090630     C                   Z-ADD     *ZEROS        TFPSCT_m         10 1
056516090630     C*
056517090630     C                   Z-ADD     *ZEROS        TFPSBPT_m        10 1
056518090630     C                   Z-ADD     *ZEROS        TFPSCPT_m        10 1
056600000000     C*
056700000000     C* DECODIFICA LA FILIALE
056800000000     C                   EXSR      DECFIL
056900000000     C*
057000000000     C* IMPOSTAZIONI CAMPI IN TESTATA
057100000000     C     PARDET        IFEQ      'S'                                          *SINTETICA
057200000000     C                   MOVEL     *BLANKS       DESFIL
057300991026     C                   MOVEL     *BLANKS       CHKVDL
057400000000     C                   ELSE                                                   *DETTAGLIO
057500000000     C                   SETON                                        01
057600000000     C                   ENDIF
057700000000     C*
057800000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
057900000000     C     *INOF         IFEQ      *ON                                          *FINE PAGINA
058000000000     C     PARDET        OREQ      'D'                                          *STAMPA IN DETTAGLIO
058100000000     C                   EXCEPT    TESTA
058200000000     C                   SETOFF                                       OF
058300000000     C                   ENDIF
058400000000     C*
058500000000     C                   ENDSR
058600000000     C*------------------------------------------------------------------------*
058700991026     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
058800000000     C*------------------------------------------------------------------------*
058900000000     C     NEWCLI        BEGSR
059000000000     C*
059100000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
059200040220     C                   MOVEL     WFPRAG        DESCLI
059300040227     C                   Z-ADD     WFPMES        DEPMES
059400000000     C*
059500040227     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
059600991026     C                   Z-ADD     *ZEROS        TCNCV             9 0
059700991026     C                   Z-ADD     *ZEROS        TCNCL             9 0
059800991026     C                   Z-ADD     *ZEROS        TCNSP             8 0
059900040301     C                   Z-ADD     *ZEROS        TCPSB            10 1
060000000517     C                   Z-ADD     *ZEROS        TCPSC            10 1
060100040213     C*
060200000517     C                   Z-ADD     *ZEROS        TCPSBT           10 1
060300000517     C                   Z-ADD     *ZEROS        TCPSCT           10 1
060400991026     C*
060500000000     C                   ENDSR
060600991026     C*------------------------------------------------------------------------*
060700991026     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
060800991026     C*------------------------------------------------------------------------*
060900991026     C     NEWPRO        BEGSR
061000991026     C*
061100991026     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
061200991026     C                   Z-ADD     WFPKSC        DEPKSC
061300991026     C                   Z-ADD     WFPCTR        DEPCTR
061301090702     c                   clear                   ptap
061302090702     c                   movel     depctr        w_tr              1
061303090702     c                   if        %lookup(w_tr:CTR)>0
061304090702     c                   exsr      rectap
061305090702     c                   endif
061400991026     C*
061500991026     C* AZZERA I TOTALI PER IL PROGRESSIVO MESE DEL CLI/TAR
061600991026     C                   Z-ADD     *ZEROS        TPNCV             9 0
061700991026     C                   Z-ADD     *ZEROS        TPNCL             9 0
061800991026     C                   Z-ADD     *ZEROS        TPNSP             8 0
061900991026     C                   Z-ADD     *ZEROS        TPPSB            10 1
062000000517     C                   Z-ADD     *ZEROS        TPPSC            10 1
062100040213     C*
062200000517     C                   Z-ADD     *ZEROS        TPPSBT           10 1
062300000517     C                   Z-ADD     *ZEROS        TPPSCT           10 1
062400991026     C*
062500991026     C                   ENDSR
062600000000     C*------------------------------------------------------------------------*
062700991028     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
062800000000     C*------------------------------------------------------------------------*
062900000000     C     STACLI        BEGSR
063000000000     C*
063100000000     C                   MOVEL     '0'           $STDAC
063200000000     C*
063300000000     C* DECODIFICA IL CLIENTE
063400000000     C                   EXSR      DECCLI
063500991026     C*
063600991026     C*------------------------------
063700991026     C*OPERAZIONI PER DETTAGLIO VDL
063800991026     C*------------------------------
063900000000     C* DECODIFICA LA TARIFFA
064000000000     C                   MOVEL     *BLANKS       DESCTR
064100000000     C                   EXSR      DECCTR
064200080110      * recupero tipo addebito peso dalla tariffa
064300090630     c****               clear                   ptap
064400090630     c****               exsr      rectap
064500000000     C*
064600000000     C                   Z-ADD     TCNCV         STNCV             9 0
064700040227     C                   Z-ADD     TCNCL         STNCL             9 0
064800040227     C                   Z-ADD     TCNSP         STNSP             8 0
064900040227     C                   Z-ADD     TCPSB         STPSB            10 1
065000040227     C                   Z-ADD     TCPSC         STPSC            10 1
065100000000     C*
065200000000     C* CALCOLA PERCENTUALE COLLI VDL
065300040301     C                   Z-ADD     TCNCL         WCPNCL           12 1
065400040301     C                   Z-ADD     TCNCV         WCPNCV           12 1
065500040301     C                   EXSR      CALPER
065600040301     C                   Z-ADD     WCPPVA        WPER              6 2
065700040301     C                   Z-ADD     WPER          STPER             6 1
065800000000     C*
065900000000     C* CALCOLA PERCENTUALE PESO VDL
066000991027     C                   Z-ADD     TCPSC         WCPPSC           13 1
066100991027     C                   Z-ADD     TCPSB         WCPPSB           13 1
066200000000     C                   EXSR      CALRNG
066300000000     C                   Z-ADD     WCPRVA        WRNG              6 2
066400000516     C                   Z-ADD     WRNG          STRNG             6 1
066500000516     C*
066600000516     C* CALCOLA PERCENTUALE PESO VDL TOTALI
066700000516     C                   Z-ADD     TCPSCT        WCPPSC           13 1
066800000516     C                   Z-ADD     TCPSBT        WCPPSB           13 1
066900000516     C                   EXSR      CALRNG
067000000516     C                   Z-ADD     WCPRVA        WRNG              6 2
067100000516     C                   Z-ADD     WRNG          STTTP             6 1
067200991026     C*------------------------------
067300991026     C*OPERAZIONI PER PROGRESIVO
067400991026     C*------------------------------
067500040301     C                   Z-ADD     TPNCV         SPNCV             9 0
067600040301     C                   Z-ADD     TPNCL         SPNCL             9 0
067700991027     C                   Z-ADD     TPNSP         SPNSP             8 0
067800991027     C                   Z-ADD     TPPSB         SPPSB            10 1
067900991027     C                   Z-ADD     TPPSC         SPPSC            10 1
068000991026     C*
068100991026     C* CALCOLA PERCENTUALE COLLI VDL
068200991027     C                   Z-ADD     TPNCL         WCPNCL           12 1
068300991027     C                   Z-ADD     TPNCV         WCPNCV           12 1
068400991026     C                   EXSR      CALPER
068500991026     C                   Z-ADD     WCPPVA        WPER              6 2
068600000516     C                   Z-ADD     WPER          SPPER             6 1
068700991026     C*
068800991026     C* CALCOLA PERCENTUALE PESO VDL
068900991027     C                   Z-ADD     TPPSC         WCPPSC           13 1
069000991027     C                   Z-ADD     TPPSB         WCPPSB           13 1
069100991026     C                   EXSR      CALRNG
069200991026     C                   Z-ADD     WCPRVA        WRNG              6 2
069300000516     C                   Z-ADD     WRNG          SPRNG             6 1
069400000516     C*
069500000516     C* CALCOLA PERCENTUALE PESO VDL TOTALI
069600000516     C                   Z-ADD     TPPSCT        WCPPSC           13 1
069700000516     C                   Z-ADD     TPPSBT        WCPPSB           13 1
069800000516     C                   EXSR      CALRNG
069900000516     C                   Z-ADD     WCPRVA        WRNG              6 2
070000000516     C                   Z-ADD     WRNG          SPTTP             6 1
070100991026     C*
070200991028     C* STAMPA RIGA CLIENTE/TARIFFA SOLO SE PRESENTI DATI NEL MESE
070300991028     C*
070400040301     C     DEPMES        IFEQ      PARMMF
070500040308     C     STNSP         ANDGT     *ZEROS
070600991026     C                   EXSR      STRCLI
070700991026     C                   MOVEL     '1'           $STDAC
070701090703     c* Incremento totali "Di cui tariffe con tipo applicazione "M"=MAI VDL"
070702090703     c* Totali di FILIALE
070704090703     c                   if        ptap='M' and tcpsc>tcpsb
070705090703     C                   ADD       tcncv         TFNCV_m
070706090703     C                   ADD       tcncl         TFNCL_m
070707090703     C                   ADD       tcnsp         TFNSP_m
070708090703     C                   ADD       tcpsb         TFPSB_m
070709090703     C                   ADD       tcpsc         TFPSC_m
070710090703     C                   ADD       tcpsb         TFPSBT_m
070711090703     C                   ADD       tcpsc         TFPSCT_m
070713090703     c* Totali di AREA
070716090703     C                   ADD       tcncv         TANCV_m
070717090703     C                   ADD       tcncl         TANCL_m
070718090703     C                   ADD       tcnsp         TANSP_m
070719090703     C                   ADD       tcpsb         TAPSB_m
070720090703     C                   ADD       tcpsc         TAPSC_m
070721090703     C                   ADD       tcpsb         TAPSBT_m
070722090703     C                   ADD       tcpsc         TAPSCT_m
070724090703     c* Totali DIVISIONE
070727090703     C                   ADD       tcncv         TDNCV_m
070728090703     C                   ADD       tcncl         TDNCL_m
070729090703     C                   ADD       tcnsp         TDNSP_m
070730090703     C                   ADD       tcpsb         TDPSB_m
070731090703     C                   ADD       tcpsc         TDPSC_m
070732090703     C                   ADD       tcpsb         TDPSBT_m
070733090703     C                   ADD       tcpsc         TDPSCT_m
070735090703     C* Totale GENERALE
070738090703     C                   ADD       tcncv         TGNCV_m           9 0
070739090703     C                   ADD       tcncl         TGNCL_m           9 0
070740090703     C                   ADD       tcnsp         TGNSP_m           8 0
070741090703     C                   ADD       tcpsb         TGPSB_m          10 1
070742090703     C                   ADD       tcpsc         TGPSC_m          10 1
070743090703     C                   ADD       tcpsb         TGPSBT_m         10 1
070744090703     C                   ADD       tcpsc         TgPSCT_m         10 1
070746090703     C* INCREMENTO X PROGRESSIVO
070747090703     C* Totale FILIALE
070748090703     C                   ADD       tpncv         TFNCVp_m
070749090703     C                   ADD       tpncl         TFNCLp_m
070750090703     C                   ADD       tpnsp         TFNSPp_m
070751090703     C                   ADD       tppsb         TFPSBp_m
070752090703     C                   ADD       tppsc         TFPSCp_m
070753090703     C                   ADD       tppsb         TFPSBpT_m
070754090703     C                   ADD       tppsc         TFPSCpT_m
070755090703     C* Totale AREA
070756090703     C                   ADD       tpncv         TANCVp_m
070757090703     C                   ADD       tpncl         TANCLp_m
070758090703     C                   ADD       tpnsp         TANSPp_m
070759090703     C                   ADD       tppsb         TAPSBp_m
070760090703     C                   ADD       tppsc         TAPSCp_m
070761090703     C                   ADD       tppsb         TAPSBpT_m
070762090703     C                   ADD       tppsc         TAPSCpT_m
070763090703     C* Totale DIVISIONE
070764090703     C                   ADD       tpncv         TDNCVp_m
070765090703     C                   ADD       tpncl         TDNCLp_m
070766090703     C                   ADD       tpnsp         TDNSPp_m
070767090703     C                   ADD       tppsb         TDPSBp_m
070768090703     C                   ADD       tppsc         TDPSCp_m
070769090703     C                   ADD       tppsb         TDPSBpT_m
070770090703     C                   ADD       tppsc         TDPSCpT_m
070771090703     C* Totale GENERALE
070772090703     C                   ADD       tpncv         TGNCVp_m          9 0
070773090703     C                   ADD       tpncl         TGNCLp_m          9 0
070774090703     C                   ADD       tpnsp         TGNSPp_m          8 0
070775090703     C                   ADD       tppsb         TGPSBp_m         10 1
070776090703     C                   ADD       tppsc         TGPSCp_m         10 1
070777090703     C                   ADD       tppsb         TGPSBpT_m        10 1
070778090703     C                   ADD       tppsc         TGPSCpT_m        10 1
070779090703     c                   endif
070800991028     C                   ENDIF
070900000000     C*
071000000000     C                   ENDSR
071100000000     C*------------------------------------------------------------------------*
071200000000     C* STAFIL - STAMPA TOTALI FILIALE
071300000000     C*------------------------------------------------------------------------*
071400000000     C     STAFIL        BEGSR
071500040213     C*
071501090630     c                   setoff                                       21
071600000000     C                   MOVEL     TES(10)       STDES1           22
071700000000     C*
071800000000     C* DECODIFICA LA FILIALE
071900000000     C                   EXSR      DECFIL
072000000000     C                   MOVEL     DESFIL        STDES2           20
072100000000     C*
072200000000     C* IMPOSTO I VALORI DI STAMPA
072300000000     C                   Z-ADD     TFNCV         STNCV
072400991027     C                   Z-ADD     TFNCL         STNCL
072500000000     C                   Z-ADD     TFNSP         STNSP
072600000000     C                   Z-ADD     TFPSB         STPSB
072700000000     C                   Z-ADD     TFPSC         STPSC
072800000000     C*
072900991027     C                   Z-ADD     TFNCVP        SPNCV
073000991027     C                   Z-ADD     TFNCLP        SPNCL
073100991027     C                   Z-ADD     TFNSPP        SPNSP
073200991027     C                   Z-ADD     TFPSBP        SPPSB
073300991027     C                   Z-ADD     TFPSCP        SPPSC
073400000516     C*
073500000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
073600000516     C                   Z-ADD     TFPSCT        WCPPSC           13 1
073700040308     C                   Z-ADD     TFPSBT        WCPPSB           13 1
073800040308     C                   EXSR      CALRNG
073900000516     C                   Z-ADD     WCPRVA        STTTP
074000000516     C*
074100000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
074200000516     C                   Z-ADD     TFPSCPT       WCPPSC           13 1
074300000516     C                   Z-ADD     TFPSBPT       WCPPSB           13 1
074400000516     C                   EXSR      CALRNG
074500000516     C                   Z-ADD     WCPRVA        SPTTP
074600991026     C*
074700000000     C* SE DETTAGLIO
074800000000     C     PARDET        IFEQ      'D'
074900000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
075000991027     C                   ENDIF
075100000000     C*
075200000000     C* STAMPO RIGA TOTALI
075300000517     C     STNSP         IFEQ      *ZEROS
075400000517     C                   Z-ADD     *ZEROS        SPTTP
075500000517     C                   ENDIF
075600000000     C                   EXSR      STRTOT
075601090630     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
075602090630     c                   if        tfnsp_m>0
075603090630     C* IMPOSTO I VALORI DI STAMPA
075604090630     C                   Z-ADD     TFNCV_m       STNCV
075605090630     C                   Z-ADD     TFNCL_m       STNCL
075606090630     C                   Z-ADD     TFNSP_m       STNSP
075607090630     C                   Z-ADD     TFPSB_m       STPSB
075608090630     C                   Z-ADD     TFPSC_m       STPSC
075609090630     C*
075610090630     C                   Z-ADD     TFNCVP_m      SPNCV
075611090630     C                   Z-ADD     TFNCLP_m      SPNCL
075612090630     C                   Z-ADD     TFNSPP_m      SPNSP
075613090630     C                   Z-ADD     TFPSBP_m      SPPSB
075614090630     C                   Z-ADD     TFPSCP_m      SPPSC
075615090630     C*
075616090630     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
075617090630     C                   Z-ADD     TFPSCT_m      WCPPSC           13 1
075618090630     C                   Z-ADD     TFPSBT_m      WCPPSB           13 1
075619090630     C                   EXSR      CALRNG
075620090630     C                   Z-ADD     WCPRVA        STTTP
075621090630     C*
075622090630     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
075623090630     C                   Z-ADD     TFPSCPT_m     WCPPSC           13 1
075624090630     C                   Z-ADD     TFPSBPT_m     WCPPSB           13 1
075625090630     C                   EXSR      CALRNG
075626090630     C                   Z-ADD     WCPRVA        SPTTP
075627090630     c                   eval      stdes3=tes(21)
075629090630     c                   seton                                        21
075630090630     c                   exsr      strtot
075631090630     c                   endif
075700000000     C*
075800000000     C* SE DETTAGLIO
075900040301     C     PARDET        IFEQ      'D'
076000000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
076100000000     C                   ENDIF
076200000000     C*
076300000000     C                   ENDSR
076400000000     C*------------------------------------------------------------------------*
076500000000     C* STAARE - STAMPA TOTALI AREA
076600000000     C*------------------------------------------------------------------------*
076700000000     C     STAARE        BEGSR
076800000000     C*
076900000000     C* STAMPA SINTETICA
077000000000     C     PARDET        IFEQ      'S'
077100000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
077200000000     C                   ELSE
077300000000     C*
077400000000     C* STAMPO I TOTALI AREA A PAGINA NUOVA SOLO PER STAMPA IN DETTAGLIO
077500000000     C                   MOVEL     *BLANKS       DESFIL
077600000000     C                   SETOFF                                       01
077700000000     C                   EXCEPT    TESTA
077800000000     C                   ENDIF
077900000000     C*
078000000000     C                   SETON                                        OF        *SALTO PAG SUCCESSIV
078100000000     C*
078200000000     C* DESCRIZIONE TOTALE
078300000000     C                   MOVEL     TES(11)       STDES1
078400000000     C*
078500000000     C* DECODIFICA L'AREA
078600000000     C                   EXSR      DECARE
078700000000     C                   MOVEL     DESARE        STDES2
078800000000     C*
078900000000     C* IMPOSTO I VALORI DI STAMPA
079000000000     C                   Z-ADD     TANCV         STNCV
079100991027     C                   Z-ADD     TANCL         STNCL
079200000000     C                   Z-ADD     TANSP         STNSP
079300000000     C                   Z-ADD     TAPSB         STPSB
079400000000     C                   Z-ADD     TAPSC         STPSC
079500000000     C*
079600991026     C                   Z-ADD     TANCVP        SPNCV
079700991027     C                   Z-ADD     TANCLP        SPNCL
079800991026     C                   Z-ADD     TANSPP        SPNSP
079900991026     C                   Z-ADD     TAPSBP        SPPSB
080000991026     C                   Z-ADD     TAPSCP        SPPSC
080100000516     C*
080200000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
080300000516     C                   Z-ADD     TAPSCT        WCPPSC           13 1
080400000516     C                   Z-ADD     TAPSBT        WCPPSB           13 1
080500000516     C                   EXSR      CALRNG
080600000516     C                   Z-ADD     WCPRVA        STTTP
080700000516     C*
080800000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
080900000516     C                   Z-ADD     TAPSCPT       WCPPSC           13 1
081000000516     C                   Z-ADD     TAPSBPT       WCPPSB           13 1
081100000516     C                   EXSR      CALRNG
081200000516     C                   Z-ADD     WCPRVA        SPTTP
081300991026     C*
081400000000     C* STAMPO RIGA TOTALE
081401090701     c                   setoff                                       21
081500000000     C                   EXSR      STRTOT
081501090701     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
081502090701     c                   if        tansp_m>0
081503090701     C* IMPOSTO I VALORI DI STAMPA
081504090701     C                   Z-ADD     TANCV_m       STNCV
081505090701     C                   Z-ADD     TANCL_m       STNCL
081506090701     C                   Z-ADD     TANSP_m       STNSP
081507090701     C                   Z-ADD     TAPSB_m       STPSB
081508090701     C                   Z-ADD     TAPSC_m       STPSC
081509090701     C*
081510090701     C                   Z-ADD     TANCVP_m      SPNCV
081511090701     C                   Z-ADD     TANCLP_m      SPNCL
081512090701     C                   Z-ADD     TANSPP_m      SPNSP
081513090701     C                   Z-ADD     TAPSBP_m      SPPSB
081514090701     C                   Z-ADD     TAPSCP_m      SPPSC
081515090701     C*
081516090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
081517090701     C                   Z-ADD     TAPSCT_m      WCPPSC           13 1
081518090701     C                   Z-ADD     TAPSBT_m      WCPPSB           13 1
081519090701     C                   EXSR      CALRNG
081520090701     C                   Z-ADD     WCPRVA        STTTP
081521090701     C*
081522090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
081523090701     C                   Z-ADD     TAPSCPT_m     WCPPSC           13 1
081524090701     C                   Z-ADD     TAPSBPT_m     WCPPSB           13 1
081525090701     C                   EXSR      CALRNG
081526090701     C                   Z-ADD     WCPRVA        SPTTP
081527090701     c                   eval      stdes3=tes(21)
081528090701     c                   seton                                        21
081531090701     C                   EXSR      STRTOT
081532090701     c                   endif
081600000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
081700000000     C*
081800000000     C                   ENDSR
081900000000     C*------------------------------------------------------------------------*
082000000000     C* STADIV - STAMPA TOTALI DIVISIONE
082100000000     C*------------------------------------------------------------------------*
082200000000     C     STADIV        BEGSR
082300000000     C*
082400000000     C* STAMPA TOTALE DIVISIONE A PAGINA NUOVA
082500000000     C                   MOVEL     *BLANKS       DESFIL
082600000000     C                   MOVEL     *BLANKS       DESARE
082700000000     C                   SETOFF                                       01
082800000000     C                   EXCEPT    TESTA
082900000000     C                   SETON                                        OF        *SALTO PAG SUCCESSIV
083000000000     C*
083100000000     C* DESCRIZIONE TOTALE
083200000000     C                   MOVEL     TES(12)       STDES1
083300000000     C*
083400000000     C* DECODIFICA LA DIVISIONE
083500000000     C                   EXSR      DECDIV
083600000000     C                   MOVEL     DESDIV        STDES2
083700000000     C*
083800000000     C* IMPOSTO I VALORI DI STAMPA
083900000000     C                   Z-ADD     TDNCV         STNCV
084000991027     C                   Z-ADD     TDNCL         STNCL
084100000000     C                   Z-ADD     TDNSP         STNSP
084200000000     C                   Z-ADD     TDPSB         STPSB
084300000000     C                   Z-ADD     TDPSC         STPSC
084400000000     C*
084500991026     C                   Z-ADD     TDNCVP        SPNCV
084600991027     C                   Z-ADD     TDNCLP        SPNCL
084700991026     C                   Z-ADD     TDNSPP        SPNSP
084800991026     C                   Z-ADD     TDPSBP        SPPSB
084900991026     C                   Z-ADD     TDPSCP        SPPSC
085000000516     C*
085100000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
085200000516     C                   Z-ADD     TDPSCT        WCPPSC           13 1
085300000516     C                   Z-ADD     TDPSBT        WCPPSB           13 1
085400000516     C                   EXSR      CALRNG
085500000516     C                   Z-ADD     WCPRVA        STTTP
085600000516     C*
085700000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
085800000516     C                   Z-ADD     TDPSCPT       WCPPSC           13 1
085900000516     C                   Z-ADD     TDPSBPT       WCPPSB           13 1
086000000516     C                   EXSR      CALRNG
086100000516     C                   Z-ADD     WCPRVA        SPTTP
086200991026     C*
086300000000     C* STAMPO RIGA TOTALE
086301090701     c                   setoff                                       21
086400000000     C                   EXSR      STRTOT
086401090701     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
086402090701     c                   if        tdnsp_m>0
086403090701     C* IMPOSTO I VALORI DI STAMPA
086404090701     C                   Z-ADD     TDNCV_m       STNCV
086405090701     C                   Z-ADD     TDNCL_m       STNCL
086406090701     C                   Z-ADD     TDNSP_m       STNSP
086407090701     C                   Z-ADD     TDPSB_m       STPSB
086408090701     C                   Z-ADD     TDPSC_m       STPSC
086409090701     C*
086410090701     C                   Z-ADD     TDNCVP_m      SPNCV
086411090701     C                   Z-ADD     TDNCLP_m      SPNCL
086412090701     C                   Z-ADD     TDNSPP_m      SPNSP
086413090701     C                   Z-ADD     TDPSBP_m      SPPSB
086414090701     C                   Z-ADD     TDPSCP_m      SPPSC
086415090701     C*
086416090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
086417090701     C                   Z-ADD     TDPSCT_m      WCPPSC           13 1
086418090701     C                   Z-ADD     TDPSBT_m      WCPPSB           13 1
086419090701     C                   EXSR      CALRNG
086420090701     C                   Z-ADD     WCPRVA        STTTP
086421090701     C*
086422090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
086423090701     C                   Z-ADD     TDPSCPT_m     WCPPSC           13 1
086424090701     C                   Z-ADD     TDPSBPT_m     WCPPSB           13 1
086425090701     C                   EXSR      CALRNG
086426090701     C                   Z-ADD     WCPRVA        SPTTP
086429090701     c                   eval      stdes3=tes(21)
086430090701     c                   seton                                        21
086431090701     C                   EXSR      STRTOT
086432090701     c                   endif
086500000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
086600000000     C*
086700000000     C                   ENDSR
086800000000     C*------------------------------------------------------------------------*
086900000000     C* STAGEN - STAMPA TOTALI GENERALI
087000000000     C*------------------------------------------------------------------------*
087100000000     C     STAGEN        BEGSR
087200000000     C*
087300000000     C* STAMPA TOTALE GENERALE A PAGINA NUOVA
087400000000     C                   MOVEL     *BLANKS       DESFIL
087500000000     C                   MOVEL     *BLANKS       DESARE
087600000000     C                   MOVEL     *BLANKS       DESDIV
087700000000     C                   SETOFF                                       01
087800000000     C                   EXCEPT    TESTA
087900000000     C*
088000000000     C* DESCRIZIONE TOTALE
088100000000     C                   MOVEL     TES(13)       STDES1
088200000000     C                   MOVEL     *BLANKS       STDES2
088300000000     C*
088400000000     C* IMPOSTO I VALORI DI STAMPA
088500000000     C                   Z-ADD     TGNCV         STNCV
088600991027     C                   Z-ADD     TGNCL         STNCL
088700000000     C                   Z-ADD     TGNSP         STNSP
088800000000     C                   Z-ADD     TGPSB         STPSB
088900000000     C                   Z-ADD     TGPSC         STPSC
089000000000     C*
089100991026     C                   Z-ADD     TGNCVP        SPNCV
089200991027     C                   Z-ADD     TGNCLP        SPNCL
089300991026     C                   Z-ADD     TGNSPP        SPNSP
089400991026     C                   Z-ADD     TGPSBP        SPPSB
089500991026     C                   Z-ADD     TGPSCP        SPPSC
089600000516     C*
089700000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
089800000516     C                   Z-ADD     TGPSCT        WCPPSC           13 1
089900000516     C                   Z-ADD     TGPSBT        WCPPSB           13 1
090000000516     C                   EXSR      CALRNG
090100000516     C                   Z-ADD     WCPRVA        STTTP
090200000516     C*
090300000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
090400000516     C                   Z-ADD     TGPSCPT       WCPPSC           13 1
090500000516     C                   Z-ADD     TGPSBPT       WCPPSB           13 1
090600000516     C                   EXSR      CALRNG
090700000516     C                   Z-ADD     WCPRVA        SPTTP
090800991026     C*
090900000000     C* STAMPO RIGA TOTALE
090901090701     c                   setoff                                       21
091000000000     C                   EXSR      STRTOT
091001090701     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
091002090701     c                   if        tgnsp_m>0
091003090701     C* IMPOSTO I VALORI DI STAMPA
091004090701     C                   Z-ADD     TGNCV_m       STNCV
091005090701     C                   Z-ADD     TGNCL_m       STNCL
091006090701     C                   Z-ADD     TGNSP_m       STNSP
091007090701     C                   Z-ADD     TGPSB_m       STPSB
091008090701     C                   Z-ADD     TGPSC_m       STPSC
091009090701     C*
091010090701     C                   Z-ADD     TGNCVP_m      SPNCV
091011090701     C                   Z-ADD     TGNCLP_m      SPNCL
091012090701     C                   Z-ADD     TGNSPP_m      SPNSP
091013090701     C                   Z-ADD     TGPSBP_m      SPPSB
091014090701     C                   Z-ADD     TGPSCP_m      SPPSC
091015090701     C*
091016090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
091017090701     C                   Z-ADD     TGPSCT_m      WCPPSC           13 1
091018090701     C                   Z-ADD     TGPSBT_m      WCPPSB           13 1
091019090701     C                   EXSR      CALRNG
091020090701     C                   Z-ADD     WCPRVA        STTTP
091021090701     C*
091022090701     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI PROGRASSIVI)
091023090701     C                   Z-ADD     TGPSCPT_m     WCPPSC           13 1
091024090701     C                   Z-ADD     TGPSBPT_m     WCPPSB           13 1
091025090701     C                   EXSR      CALRNG
091026090701     C                   Z-ADD     WCPRVA        SPTTP
091027090701     c                   eval      stdes3=tes(21)
091028090701     c                   seton                                        21
091029090701     C                   EXSR      STRTOT
091030090701     c                   endif
091100000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
091200000000     C*
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
091600000000     C*------------------------------------------------------------------------*
091700000000     C     STRCLI        BEGSR
091800040213     C*
091900991105 MOD C     PARDET        IFEQ      'D'
092000000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
092100040308     C     *INOF         IFEQ      '1'
092200040308     C                   SETON                                        01
092300040308     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
092400040308     C                   EXCEPT    TESTA
092500040308     C                   SETOFF                                       OF
092600000000     C                   END
092700000000     C*
092800000000     C     $STDAC        IFEQ      '1'
092900000000     C                   Z-ADD     *ZEROS        DEPKSC
093000040220     C                   MOVEL     *BLANKS       DESCLI
093100000000     C                   Z-ADD     *ZEROS        DEPCTR
093200991026     C                   Z-ADD     *ZEROS        DEPMES
093300000000     C                   MOVEL     *BLANKS       DESCTR
093400991026     C*
093500991026     C                   ENDIF
093600000000     C*
093700000000     C* STAMPO RIGA CLIENTE/TARIFFA
093800040308     C                   EXCEPT    RIGCLI
093900040308 MOD C                   ENDIF
094000991028     C*
094100000000     C*
094200000000     C                   ENDSR
094300000000     C*------------------------------------------------------------------------*
094400000000     C* STRTOT - STAMPA RIGA TOTALE
094500000000     C*------------------------------------------------------------------------*
094600000000     C     STRTOT        BEGSR
094700991027     C*
094800991027     C*-------------------------
094900991027     C* OPERAZIONI X STAMPA DATI DEL MESE
095000991027     C*-------------------------
095100991027     C*
095200000000     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO
095300040309     c     stpsc         ifeq      stpsb
095400040309     c                   z-add     *zeros        strng
095500040309     c                   else
095600040309     C                   Z-ADD     STPSC         WCPPSC           13 1
095700040309     C                   Z-ADD     STPSB         WCPPSB           13 1
095800040309     C                   EXSR      CALRNG
095900040309     C                   Z-ADD     WCPRVA        STRNG
096000040309     c                   endif
096100991025     C*
096200991025     C* CALCOLA PERCENTUALE COLLI VDL E BOLLETTATO
096300040309     c     stncl         ifeq      stncv
096400040309     c                   z-add     *zeros        stper
096500040309     c                   else
096600040309     C                   Z-ADD     STNCL         WCPNCL           12 1
096700040309     C                   Z-ADD     STNCV         WCPNCV           12 1
096800040309     C                   EXSR      CALPER
096900040309     C                   Z-ADD     WCPPVA        STPER
097000040309     c                   endif
097100991027     C*
097200991027     C*-------------------------
097300991027     C* OPERAZIONI X STAMPA DATI PROGRESSIVI
097400991027     C*-------------------------
097500991027     C*
097600991027     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO
097700040308     c     sppsc         ifeq      sppsb
097800040308     c                   z-add     *zeros        sprng
097900040308     c                   else
098000991027     C                   Z-ADD     SPPSC         WCPPSC           13 1
098100991027     C                   Z-ADD     SPPSB         WCPPSB           13 1
098200991027     C                   EXSR      CALRNG
098300040309     C                   Z-ADD     WCPRVA        SPRNG
098400040309     c                   endif
098500991027     C*
098600991027     C* CALCOLA PERCENTUALE COLLI VDL E BOLLETTATO
098700040308     c     spncl         ifeq      spncv
098800040308     c                   z-add     *zeros        spper
098900040308     c                   else
099000991027     C                   Z-ADD     SPNCL         WCPNCL           12 1
099100991027     C                   Z-ADD     SPNCV         WCPNCV           12 1
099200991027     C                   EXSR      CALPER
099300991027     C                   Z-ADD     WCPPVA        SPPER
099400040308     c                   endif
099500000000     C*
099600000000     C* STAMPO RIGA CLIENTE/TARIFFA
099700000000     C                   EXCEPT    RIGTOT
099800000000     C*
099900000000     C                   ENDSR
100000000000     C*------------------------------------------------------------------------*
100100000000     C* DECDIV - DECODIFICA DIVISIONE
100200000000     C*------------------------------------------------------------------------*
100300000000     C     DECDIV        BEGSR
100400000000     C*
100500020906     C                   MOVEL     1             KTBKUT
100600000000     C                   MOVE      '17'          KTBCOD
100700000000     C                   MOVE      *BLANKS       KTBKEY
100800000000     C                   MOVEL     DEPDIV        KTBKEY
100900000000     C     KEYTAB        CHAIN     TABEL                              97
101000000000     C     *IN97         IFEQ      '1'
101100000000     C     TBLFLG        OREQ      '*'
101200000000     C                   MOVEL     *ALL'*'       DESDIV           20
101300000000     C                   ELSE
101400000000     C                   MOVEL     TBLUNI        DESDIV
101500000000     C                   END
101600000000     C*
101700000000     C                   ENDSR
101800000000     C*------------------------------------------------------------------------*
101900000000     C* DECARE - DECODIFICA AREA
102000000000     C*------------------------------------------------------------------------*
102100000000     C     DECARE        BEGSR
102200000000     C*
102300020906     C                   MOVEL     1             KTBKUT
102400000000     C                   MOVE      '05'          KTBCOD
102500000000     C                   MOVE      *BLANKS       KTBKEY
102600000000     C                   MOVEL     DEPARE        KTBKEY
102700000000     C     KEYTAB        CHAIN     TABEL                              97
102800000000     C     *IN97         IFEQ      '1'
102900000000     C     TBLFLG        OREQ      '*'
103000000000     C                   MOVEL     *ALL'*'       DESARE           20
103100000000     C                   ELSE
103200000000     C                   MOVEL     TBLUNI        DESARE
103300000000     C                   END
103400000000     C*
103500000000     C                   ENDSR
103600000000     C*------------------------------------------------------------------------*
103700000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
103800000000     C*------------------------------------------------------------------------*
103900000000     C     DECFIL        BEGSR
104000000000     C*
104100000000     C                   Z-ADD     1             I
104200000000     C     DEPFIL        LOOKUP    FIL(I)                                 95
104300000000     C     *IN95         IFEQ      '0'
104400000000     C                   MOVEL     *ALL'*'       DESFIL           20
104500000000     C                   ELSE
104600000000     C                   MOVEL     DFIL(I)       DESFIL
104700000000     C                   END
104800991026     C*
104900000000     C                   ENDSR
105000000000     C*------------------------------------------------------------------------*
105100000000     C* DECCLI - DECODIFICA CLIENTE
105200000000     C*------------------------------------------------------------------------*
105300000000     C     DECCLI        BEGSR
105400000000     C*
105500000000     C                   CLEAR                   BS69DS
105600000000     C                   CLEAR                   ACODS
105700000000     C                   MOVEL     KNSIF         I69SIF                         *S.I.
105800000000     C                   Z-ADD     DEPKSC        I69KAC                         *CLIENTE X CNACO
105900000000     C                   CALL      'TIBS69R'
106000000000     C                   PARM                    BS69DS
106100000000     C                   PARM                    ACODS
106200000000     C                   PARM                    INDDS
106300000000     C                   PARM                    CLPDS
106400000000     C                   PARM                    CLSDS
106500000000     C     O69ERR        IFNE      '1'                                          *NO ERRORE
106600000000     C                   MOVEL     ACORAG        DESCLI           30
106700000000     C                   ELSE
106800000000     C                   MOVEL     *ALL'*'       DESCLI           30
106900000000     C                   ENDIF
107000000000     C*
107100000000     C                   ENDSR
107200000000     C*------------------------------------------------------------------------*
107300000000     C* DECCTR - DECODIFICA TARIFFA
107400000000     C*------------------------------------------------------------------------*
107500000000     C     DECCTR        BEGSR
107600000000     C*
107700000000     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
107800000000     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
107900000000     C     DEPCTR        IFEQ      999
108000000000     C                   MOVEL     DEC(1)        DESCTR           15
108100000000     C                   ELSE
108200000000     C     DEPCTR        IFEQ      888
108300000000     C                   MOVEL     DEC(2)        DESCTR
108400000000     C                   ELSE
108500020906     C                   MOVEL     1             KTBKUT
108600000000     C                   MOVE      'TR'          KTBCOD
108700000000     C                   MOVE      *BLANKS       KTBKEY
108800000000     C                   MOVEL     DEPCTR        WTEM01            1
108900000000     C                   MOVEL     WTEM01        KTBKEY
109000000000     C     KEYTAB        CHAIN     TABEL                              97
109100000000     C     *IN97         IFEQ      '1'
109200000000     C     TBLFLG        OREQ      '*'
109300000000     C                   MOVEL     *ALL'*'       DESCTR
109400000000     C                   ELSE
109500000000     C                   MOVEL     TBLUNI        DESCTR
109600000000     C                   ENDIF
109700000000     C                   ENDIF
109800000000     C                   ENDIF
109900000000     C*
110000000000     C                   ENDSR
110100080110
110200080110      *------------------------------------------------------------------------*
110300080110      * Recupero tipo addebito peso da tariffa
110400080110      *------------------------------------------------------------------------*
110500080110     c     rectap        begsr
110600080110
110700080110     c                   clear                   dsta01
110800080110     c     ktntam        setgt     tntam01l
110900080110     c     ktntam        readpe    tntam01l
111000080110     c                   eval      dsta01 = tamflo
111100080110     c                   eval      ptap = §tatap
111200080110
111300080110     c                   endsr
111400080110
111500000000     C*------------------------------------------------------------------------*
111600000000     C* CALPER - CALCOLA PERCENTUALE COLLI
111700000000     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
111800000000     C*          - COLLI VDL               (WCPNCV)
111900000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
112000000000     C*------------------------------------------------------------------------*
112100040301     C     CALPER        BEGSR
112200000000     C*
112300000000     C* ===> CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLL.
112400000000     C                   Z-ADD     *ZEROS        WCPPVA            6 2
112500991025     C     WCPNCL        IFNE      WCPNCV                                       --- 1 -->
112600991025     C     WCPNCL        ANDGT     *ZEROS
112700040308     C     WCPNCV        ANDGT     *ZEROS
112800040308     C     WCPNCV        MULT      100           WMULTI           17 3
112900040301     C     WMULTI        DIV       WCPNCL        WRKPER           17 3
113000991025     C     WRKPER        IFGT      9999                                         --- 2 -->
113100000000     C                   Z-ADD     9999,99       WCPPVA
113200040308     C                   ELSE
113300040301     C                   Z-ADD     WRKPER        WCPPVA
113400040301     C                   END                                                    <-- 2 ---
113500040308     C                   END                                                    <-- 1 ---
113600991027     C*
113700991102     C     WCPNCL        IFEQ      WCPNCV                                       --- 1 -->
113800040301     C     WCPNCL        ANDGT     *ZEROS
113900991102     C     WCPNCV        ANDGT     *ZEROS
114000040308     C                   Z-ADD     100           WCPPVA
114100991102     C                   ENDIF
114200040301     C*
114300040308     C     WCPPVA        IFGT      100
114400040308     C                   Z-ADD     100           WCPPVA
114500040308     C                   ENDIF
114600991102     C*
114700040301     C                   ENDSR
114800000000     C*------------------------------------------------------------------------*
114900000000     C* CALRNG - CALCOLA PERCENTUALE RANGE
115000000000     C* INPUT:   - PESO BOLLETTAT0         (WCPPSB)
115100000000     C*          - PESO VDL                (WCPPSC)
115200000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPRVA)
115300000000     C*------------------------------------------------------------------------*
115400000000     C     CALRNG        BEGSR
115500000000     C*
115600000000     C* ===> CALCOLA PERCENTUALE PESO VDL SU PESO BOLL.
115700000000     C                   Z-ADD     *ZEROS        WCPRVA            6 2
115800000000     C     WCPPSC        IFNE      WCPPSB                                       --- 1 -->
115900040308     C     WCPPSC        ANDGT     *ZEROS
116000000000     C     WCPPSB        ANDGT     *ZEROS
116100991027     C     WCPPSB        SUB       WCPPSC        WDIFFE           13 1
116200040308     C     WDIFFE        MULT      100           WRKPER           17 3
116300991027     C     WRKPER        DIV       WCPPSB        WRKPER
116400000000     C     WRKPER        IFGT      9999                                         --- 2 -->
116500040308     C                   Z-ADD     9999,99       WCPRVA
116600040308     C                   ELSE
116700040308     C                   Z-ADD     WRKPER        WCPRVA
116800040308     C                   END                                                    <-- 2 ---
116900000000     C                   END                                                    <-- 1 ---
117000991102     C*
117100991102     C     WCPPSC        IFEQ      WCPPSB                                       --- 1 -->
117200991102     C     WCPPSC        ANDGT     *ZEROS
117300991102     C     WCPPSB        ANDGT     *ZEROS
117400040308     C                   Z-ADD     100           WCPPVA
117500991102     C                   ENDIF
117600040308     C*
117700040308     C     WCPPVA        IFGT      100
117800040308     C                   Z-ADD     100           WCPPVA
117900040308     C                   ENDIF
118000000000     C*
118100000000     C                   ENDSR
118200000000     C*------------------------------------------------------------------------*
118300000000     C* FINSR - OPERAZIONI FINALI
118400000000     C*------------------------------------------------------------------------*
118500000000     C     FINSR         BEGSR
118600000000     C*
118700000000     C* CHIUDE I FILE DEI PGM CHIAMATI
118800000000     C                   CLEAR                   BS69DS
118900000000     C                   CLEAR                   ACODS
119000000000     C                   CLEAR                   INDDS
119100000000     C                   CLEAR                   CLPDS
119200000000     C                   CLEAR                   CLSDS
119300000000     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
119400000000     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
119500000000     C                   CALL      'TIBS69R'
119600000000     C                   PARM                    BS69DS
119700000000     C                   PARM                    ACODS
119800000000     C                   PARM                    INDDS
119900000000     C                   PARM                    CLPDS
120000000000     C                   PARM                    CLSDS
120100000000     C*
120200000000     C                   ENDSR
120300000000     C*------------------------------------------------------------------------*
120400000000     C* *INZSR - ROUTINE INIZIALE
120500000000     C*------------------------------------------------------------------------*
120600000000     C     *INZSR        BEGSR
120700000000     C*--------------------
120800000000     C* RICEVIMENTO PARAMENTRI
120900000000     C*--------------------
121000000000     C     *ENTRY        PLIST
121100000000     C                   PARM                    KPJBA
121200000000     C                   MOVEL     KPJBU         PARAM
121300000000     C*--------------------
121400000000     C* CAMPI RIFERITI AL DATA BASE
121500000000     C*--------------------
121600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
121700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
121800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
121900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
122000000000     C     *LIKE         DEFINE    WFPANN        KWPANN
122100000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
122200000000     C     *LIKE         DEFINE    WFPARE        KWPARE
122300000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
122400040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
122500000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
122600000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
122700000000     C     *LIKE         DEFINE    WFPMES        KWPMES
122800000000     C     *LIKE         DEFINE    WFPDIV        DEPDIV
122900000000     C     *LIKE         DEFINE    WFPARE        DEPARE
123000000000     C     *LIKE         DEFINE    WFPFIL        DEPFIL
123100020910     C     *LIKE         DEFINE    WFPFIL        DEPTFP
123200000000     C     *LIKE         DEFINE    WFPKSC        DEPKSC
123300000000     C     *LIKE         DEFINE    WFPCTR        DEPCTR
123400991026     C     *LIKE         DEFINE    WFPMES        DEPMES
123500000000     C*
123600000000     C*--------------------
123700000000     C* CHIAVI DI LETTURA
123800000000     C*--------------------
123900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
124000000000     C     KEYTAB        KLIST
124100000000     C                   KFLD                    KTBKUT
124200000000     C                   KFLD                    KTBCOD
124300000000     C                   KFLD                    KTBKEY
124301090702     C     KEYTABp       KLIST
124302090702     C                   KFLD                    KTBKUT
124303090702     C                   KFLD                    KTBCOD
124400000000     C*
124500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
124600000000     C     KEYORG        KLIST
124700000000     C                   KFLD                    KORFIL
124800000000     C*
124900000000     C* CHIAVE LETTURA WFPES01L - COMPLETA
125000000000     C     KEYWFP        KLIST
125100000000     C                   KFLD                    KWPANN
125200000000     C                   KFLD                    KWPDIV
125300000000     C                   KFLD                    KWPARE
125400000000     C                   KFLD                    KWPFIL
125500040220     C                   KFLD                    KWPRAG
125600000000     C                   KFLD                    KWPKSC
125700000000     C                   KFLD                    KWPCTR
125800000000     C                   KFLD                    KWPMES
125900080110
126000080110     c     ktntam        klist
126100080110     c                   kfld                    depksc
126200080110     c                   kfld                    depctr
126300020910     C*--------------------
126400020910     C* REPERISCE DATA/ORA CORRENTE
126500020910     C*--------------------
126600020910     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
126700020910     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
126800020910     C                   Z-ADD     *ZEROS        G08INV
126900020910     C                   MOVEL     '0'           G08ERR
127000020910     C                   CALL      'XSRDA8'
127100020910     C                   PARM                    WLBDA8
127200020910     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
127300000000     C*--------------------
127400000000     C* CAMPI DI CONTROLLO
127500000000     C*--------------------
127600000000     C                   MOVEL     '0'           FINL00            1            *FINE FILE
127700000000     C                   SETON                                        OF        *OVERFLOW
127800000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
127900000000     C                   TIME                    ORA               6 0          *ORA
128000000000     C                   MOVEL     '0'           $STDAC            1            *STAMPATI DATI CLI.
128100991229     C*
128200991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
128300991229     C* PER FACILITARE I CONTROLLI
128400991229     C                   EVAL      PARMIN=%abs(PARMIN)
128500000000     C*
128600000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
128700000000IF  1C     PARDET        IFEQ      'D'
128800000000     C                   MOVEL     INT(1)        DECPAR            9
128900000000X   1C                   ELSE
129000000000     C                   MOVEL     INT(2)        DECPAR
129100000000E   1C                   ENDIF
129200000706     C*
129300000706     C* IMPOSTA LA DESCRIZIONE SE LANCIO POSTE OPPURE NO
129400000706IF  1C     PARPOS        IFEQ      'S'
129500000706     C                   MOVEL     DEC(5)        DECPOS           10
129600000706X   1C                   ELSE
129700000706     C                   MOVEL     DEC(6)        DECPOS
129800000706E   1C                   ENDIF
129900000000     C*
130000000000     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
130100000000IF  1C     PARDET        IFEQ      'S'
130200000000     C                   MOVEL     'S'           PARTOT                         *TOTALI AREA/DIV/GEN
130300000000E   1C                   ENDIF
130400000000     C*
130500000000     C* CARICA ORGANIGRAMMA
130600000000     C                   EXSR      CARORG
130700000000     C*
130800000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
130900000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
131000000000     C     PARDIV        IFEQ      *BLANKS
131100000000     C     PARARE        ANDEQ     *ZEROS
131200000000     C     PARFIL        ANDEQ     *ZEROS
131300000000     C                   SETOFF                                       02
131400000000     C                   ELSE
131500000000     C                   SETON                                        02
131600000000     C                   END
131700000000     C*
131800000000     C                   SETOFF                                       03
131801090702     c*
131802090702     c* Carico tabella tipi tariffa aventi flag "gestione rapporto peso/vol"='S'
131803090702     c                   clear                   ctr
131804090702     c                   clear                   ii
131805090702     C                   MOVEL     1             KTBKUT
131806090702     C                   MOVE      'TR'          KTBCOD
131807090702     c     keytabp       setll     tabel00f
131808090702     c     keytabp       reade     tabel00f
131809090702     c                   dow       not %eof(tabel00f)
131810090702     c                   movel     tbluni        dstr
131811090702     c                   if        §trrpv='S'
131812090702     c                   add       1             ii
131813090702     c                   movel     tblkey        ctr(ii)
131814090702     c                   endif
131815090702     c     keytabp       reade     tabel00f
131816090702     c                   enddo
131900000000     C*
132000000000     C                   ENDSR
132100000000     C*------------------------------------------------------------------------*
132200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
132300000000     C*------------------------------------------------------------------------*
132400000000     C     CARORG        BEGSR
132500000000     C*
132600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
132700000000     C                   Z-ADD     *ZEROS        KORFIL
132800000000     C     KEYORG        SETLL     AZORG01L                           98        *FINE-ARCHIV
132900000000     C  N98              READ      AZORG01L                               98
133000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
133100000000     C     *IN98         DOWEQ     '0'                                          --- 1 -->
133200991025     C*          ORGFVA    IFEQ *BLANKS                    --- 2 -->
133300000000     C     ORGFAG        IFNE      'V'
133400000000     C                   ADD       1             I
133500000000     C                   Z-ADD     ORGFIL        FIL(I)
133600000000     C                   MOVEL     ORGDES        DFIL(I)
133700040308     C                   MOVEL     ORGDE3        OG143
133800020627     C                   MOVEL     §OGNTW        PFIL(I)
133900000000     C                   END
134000991025     C*                    END                             <-- 2 ---
134100000000     C                   READ      AZORG01L                               98
134200000000     C                   END                                                    <-- 1 ---
134300000000     C*
134400000000     C                   ENDSR
134500000000     O*------------------------------------------------------------------------*
134600000000     OPRTF198   E            TESTA          1 02
134700000000     O                                            8 'TIST17R '
134800000000     O                                           90 '***      STATISTICA  '
134900000000     O                                          110 ' P E S I     ***  '
135000000000     O                       DECPAR             120
135100000000     O                                          150 'Data'
135200000000     O                       DATA               159 '  /  /  '
135300000000     O                                          169 'Ora'
135400000000     O                       ORA                178 '  :  :  '
135500000000     O                                          193 'Pag.'
135600000000     O                       PAGE          Z    198
135700000000     O*------------------------
135800000000     O          E   N03      TESTA          1
135900000000     O                                           83 'ANNO:'
136000000000     O                       PARANN              88
136100000000     O                                           97 'MESI:'
136200000000     O                       PARMMI             100
136300000000     O                                          102 '/'
136400000000     O                       PARMMF             105
136500000706     O                       DECPOS             198
136600000000     O*------------------------
136700000000     O          E    02      TESTA       1  1
136800000000     O                                           20 'PARZIALIZZAZIONI:   '
136900000000     O                                           40 '             DIVIS.:'
137000000000     O                       PARDIV              41
137100000000     O                                           61 '               AREA:'
137200000000     O                       PARARE        Z     64
137300090702     O                                           84 '            FILIALE:'
137400000000     O                       PARFIL        Z     87
137500000000     O*------------------------
137600000000     O          E            TESTA       1  1
137700000000     O                       TES(1)              66
137800000000     O                       TES(1)             132
137900000000     O                       TES(1)             198
138000000000     O*------------------------
138100000000     O          E            TESTA          1
138200090702     O                                           10 '! FILIALE:'
138300000000     O                       DESFIL            +  1
138400991025     O                                         +  1 '   AREA:'
138500991025     O                       DESARE            +  1
138600991025     O                                         +  1 '   DISTRETTO:'
138700000000     O                       DESDIV            +  1
138800991229     O                                         +  1 '   SCOST. (A) DA: '
138900991026     O                       PARRG1        N   +  1
139000000000     O                                         +  1 '%'
139100991025     O                                         +  1 '   A:'
139200991026     O                       PARRG2        N   +  1
139300000000     O                                         +  1 '%'
139400991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
139500991026     O                       PARPEI        N   +  1
139600000000     O                                         +  1 '%'
139700991229     O                                         +  1 ' SENZA IMP'
139800991026     O                       PARPES        N   +  1
139900991025     O                                         +  1 '%'
140000000000     O                                          198 '!'
140100991026     O*-------------------------                                       --------*
140200991102     O          E            TESTA          1
140300991102     O                                            1 '!'
140400991102     O                       CHKVDL            + 10
140500991229     O                                          115 'MIN.   (A)   :±'
140600991229     O                       PARMIN        N   +  0
140700991229     O                                         +  1 '%'
140800991102     O                                          198 '!'
140900991102     O*------------------------
141000991102     O          E            TESTA
141100991102     O                       TES(1)              66
141200991102     O                       TES(1)             132
141300991102     O                       TES(1)             198
141400991026     O*------------------------
141500991102     O          E            TESTA          1
141600991026     O                       TES(18)             66
141700991026     O                       TES(19)            132
141800991026     O                       TES(20)            198
141900991026     O*------------------------
142000991026     O          E            TESTA          1
142100040308     O                       TES(1)             132
142200991026     O                       TES(1)             198
142300000000     O*------------------------
142400000000     O          E            TESTA          1
142500000000     O                       TES(2)              66
142600000000     O                       TES(3)             132
142700000000     O                       TES(4)             198
142800000000     O*------------------------
142900000000     O          E            TESTA          1
143000000000     O                       TES(5)              66
143100000000     O               01                           9 'CLIENTE'
143200000000     O               01                          50 'TARIFFA'
143300080110     o               01                          63 'TAP'
143400000000     O                       TES(6)             132
143500000000     O                       TES(7)             198
143600000000     O*------------------------
143700000000     O          E            TESTA          1
143800000000     O                       TES(1)              66
143900000000     O                       TES(1)             132
144000000000     O                       TES(1)             198
144100000000     O*------------------------------------------------------------------------*
144200000000     O          E            RIGCLI         1
144300000000     O                                            1 '!'
144400000000     O                       DEPKSC        Z   +  1
144500000000     O                       DESCLI            +  1
144600040308     O                       DEPCTR            +  1
144700000000     O                       DESCTR            +  1
144800080110     o                       PTAP              +  1
144900080110     O                                         +  4 '!'
145000000516     O                       STNSP         Z     75
145100000516     O                       STPER         O     85
145200000516     O                       STPSB         4     98
145300000516     O                       STPSC         4    110
145400000516     O                       STRNG         O    119
145500040220     O*---                   STTTP         O    130
145600000516     O                                          133 '!!'
145700000516     O                       SPNSP         Z    141
145800000516     O                       SPPER         O    151
145900000516     O                       SPPSB         4    164
146000000516     O                       SPPSC         4    176
146100000516     O                       SPRNG         O    185
146200040220     O*---                   SPTTP         O    196
146300991026     O                                          198 '!'
146400991026     O*------------------------------------------------------------------------*
146500991026     O*       E  1             RIGNUL
146600991026     O*                                   1 '!'
146700991026     O*                                  67 '!'
146800991026     O*                                 132 '!'
146900991026     O*                                 133 '!'
147000991026     O*                                 198 '!'
147100000000     O*------------------------------------------------------------------------*
147200991026     O          E            RIGTOT         1
147300991026     O                                            1 '!'
147400090630     O              n21      STDES1            +  1
147500090630     O              n21      STDES2            +  1
147600090630     O              n21                        + 21 '!'
147601090702     O               21      STDES3              62
147602090702     O               21                          67 '!'
147700991026     O                       STNSP         Z     75
147800000516     O                       STPER         O     85
147900000516     O                       STPSB         4     98
148000000516     O                       STPSC         4    110
148100000516     O                       STRNG         O    119
148200040220     O*---                   STTTP         O    130
148300040308     O                                          133 '!!'
148400000516     O                       SPNSP         Z    141
148500000516     O                       SPPER         O    151
148600000516     O                       SPPSB         4    164
148700000516     O                       SPPSC         4    176
148800000516     O                       SPRNG         O    185
148900040220     O*---                   SPTTP         O    196
149000991026     O                                          198 '!'
149100991026     O*------------------------------------------------------------------------*
149200000000     O          E            RIGSEP         1
149300000000     O                       TES(1)              66
149400000000     O                       TES(1)             132
149500000000     O                       TES(1)             198
149600000000     O*------------------------
149700000000     O          E            STAFIN           63
149800000000     O                                          110 '*** FINE STAMPA ***'
149900000000     O*------------------------------------------------------------------------*
150000000000** -TES-   (DESCRIZIONI  STAMPA)
150100000000------------------------------------------------------------------  1
150200000000!                                                                   2
150300040308!   NUM.   (B) %RAPP.     PESO        PESO      (A) %            !  3
150400040308!   NUM.   (B) %RAPP.     PESO        PESO      (A) %            !  4
150500040308!                                                                   5
150600040308!   SPED.    COLLI        BOLL.       VDL       SCOST.           !  6
150700040308!   SPED.    COLLI        BOLL.       VDL       SCOST.           !  7
150800000000                                                                    8
150900000000                                                                    9
151000090702TOTALE FILIALE                                                     10
151100000000TOTALE AREA                                                        11
151200000000TOTALE DISTRETTO                                                   12
151300000000TOTALE GENERALE                                                    13
151400090702PROG.  FILIALE                                                     14
151500991026PROG.  AREA                                                        15
151600991026PROG.  DISTRETTO                                                   16
151700991026PROG.  GENERALE                                                    17
151800991026!                                                                  18
151900991026!                              MESE                              ! 19
152000991026!                           PROGRESSIVO                          ! 20
152001090702. . .  Di cui tariffe con tp.applicazione "MAI VDL" negativi       21
152100000000** -INT-   (INTESTAZIONE)
152200000000DETTAGLIO                                                       1
152300000000SINTETICA                                                       2
1524000000003
1525000000004
1526000000005
152700000000** -DEC-   (DECODIFICHE RIGHE)
152800000000ASSEGNATI                                                            1
152900000000FRANCHI                                                              2
153000991026CON IMPIANTO                                                         3
153100991026SENZA IMPIANTO                                                       4
153200000706POSTE                                                                5
153300000706NO POSTE                                                             6
1534000007067
1535000007068
1536000007069
15370000070610
