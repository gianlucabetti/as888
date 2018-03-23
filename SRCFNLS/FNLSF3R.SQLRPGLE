000100140611     /*PRM  dbgview(*source)
000200140611     /*END
000300000000     H*------------------------------------------------------------------------*
000400000000     H* STAMPA STATISTICA PESI
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700140611     h option(*nodebugio)
000800000000     F*------------------------------------------------------------------------*
000900000000     F* DATA BASE
001000000000     F*------------------------------------------------------------------------*
001100090708     FWFPESF1L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000000     FAZORG01L  IF   E           K DISK
001400140611     fAZCMM01L  if   e           k disk
001500080110     ftntam01l  if   e           k disk
001600000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001700000000     F                                     FORMLEN(66)
001800000000     F                                     FORMOFL(60)
001900000000     L*------------------------------------------------------------------------*
002000080110     d ptap            s              1
002100090708     d stdes3          s             60
002200090708     d ii              s              2  0
002300080318     d dataiso         s               d   datfmt(*iso)
002400080318     d dataeur         s               d   datfmt(*eur)
002500080318     d PARDAL_E        s                   LIKE(PARDAL)
002600080318     d PARal_e         s                   LIKE(PARAL)
002700090708     d orddes          s             18
002800090708     d decflp          s             20
002900090708     d w_primav        s               n
003000090710     d rotcom          s               n
003100000000     D*------------------------------------------------------------------------*
003200000000     D* SCHIERE
003300000000     D*------------------------------------------------------------------------*
003400000000     D*-------------------
003500000000     D* INTESTAZIONI STAMPA
003600000000     D*-------------------
003700090708     D TES             S             66    DIM(22) CTDATA PERRCD(1)
003800000000     D INT             S              9    DIM(5) CTDATA PERRCD(1)
003900000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)
004000000000     D*-------------------
004100000000     D* SCHIERA FILIALI
004200000000     D*-------------------
004300000000     D FIL             S              3  0 DIM(500)
004400000000     D DFIL            S             20    DIM(500)
004500020627     D PFIL            S              3    DIM(500)
004600090708      *
004700090708     d CTR             S              1    dim(10)
004800000000     D*------------------------------------------------------------------------*
004900000000     D* INPUT
005000000000     D*------------------------------------------------------------------------*
005100991025     D*-------------------
005200991025     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
005300991025     D*-------------------
005400991025     D OG150         E DS
005500000706     D*-------------------
005600000706     D* LETTURA CAMPO ORGDE£ DI AZORG
005700000706     D*-------------------
005800000706     D OG143         E DS
005900000000     D*-------------------
006000000000     D* ARCHITETTURA
006100000000     D*-------------------
006200000000     D KPJBA         E DS
006300000000     D*------------------
006400000000     D* DS REPERIMENTO DATI UTENTE
006500000000     D*-------------------
006600000000     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
006700000000     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
006800000000     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
006900000000     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
007000000000     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
007100090512
007200090708     d wfpesds       e ds                  extname(wfpesf0f) inz
007300080110
007400080110     d dsta01        e ds
007500090708     d dstr          e ds
007600080110
007700991025     D*-------------------
007800991025     D* DS PER SCOMPOSIZIONE CLIENTI
007900991025     D*-------------------
008000991025     D                 DS                  INZ
008100991025     D  KORG                   1      3  0
008200991025     D  CODCLI                 1      7  0
008300020910     D*------------------
008400020910     D* DS REPEIMENTO TERMINALs
008500020910     D*------------------
008600020910     D FNLV55DS      E DS
008700020910     D*------------------
008800020910     D* DS CONTROLLO DATA (8)
008900020910     D*------------------
009000020910     D WLBDA8          DS                  INZ
009100020910     D  G08DAT                 1      8  0
009200020910     D  G08INV                 9     16  0
009300020910     D  G08ERR                17     17
009400020910     D  G08TGI                18     22  0
009500000000     D*-------------------
009600000000     D* DS PARAMETRI DI LANCIO PGM
009700000000     D*-------------------
009800000000     D PARAM           DS
009900000000     D  PARTLA                 1      1
010000080318     D**PARANN                 2      5  0
010100080318     D**PARMMI                 6      7  0
010200080318     D**PARMMF                 8      9  0
010300080318     d  pardal                 2      9  0
010400080318     D**PARSNR                10     10
010500080318     D**PARDTI                11     18  0
010600080318     D**PARDTF                19     26  0
010700991025     D  PARPEI                27     31  2
010800080318     D**PARDIV                32     32
010900080318     D**PARARE                33     35  0
011000080318     D**PARFIL                36     38  0
011100080318     D  PARtfp                36     38  0
011200000000     D  PARDET                39     39
011300080318     D**PARTOT                40     40
011400000000     D  PARRG1                41     44  1
011500000000     D  PARRG2                45     48  1
011600000000     D  PARTAR                49     53  3
011700991025     D  PARPES                54     58  2
011800991229     D  PARMIN                59     63  3
011900080318     D**PARPOS                64     64
012000080318     D**PARSAL                66     66
012100080318     d  paral                 67     74  0
012200090512     d  parord                75     75
012300090708     d  parpag                76     76
012400090708     d  parflp                77     79  0
012500000000     C*------------------------------------------------------------------------*
012600000000     C* MAIN LINE
012700000000     C*------------------------------------------------------------------------*
012800000000     C*
012900090708     c* In base all'ordinamento scelto (1-comm.le unif.; 2-ragione sociale)
013000090512     c* eseguo sql o utilizzo v.l.
013100090512     c                   if        parord='1'
013200090708     c                   exsr      sr_ordcomset
013300090708     c                   exsr      sr_ordcomlet
013400090708     c                   exsr      sr_elabcom
013500090512     c                   else
013600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013700000000     C                   EXSR      SETFIL
013800000000     C*
013900000000     C* CICLO FINO A FINE FILE DI LAVORO
014000000000DO  1C     FINL00        DOWEQ     '0'
014100000000     C*
014200000000     C* OPERAZIONI PER NUOVA DIVISIONE
014300000000     C                   EXSR      NEWDIV
014400000000     C*
014500000000     C* CICLO FINO A ROTTURA DI DIVISIONE
014600000000DO  2C     FINL00        DOWEQ     '0'
014700000000     C     WFPDIV        ANDEQ     DEPDIV
014800000000     C*
014900000000     C* OPERAZIONI PER NUOVA AREA
015000000000     C                   EXSR      NEWARE
015100000000     C*
015200000000     C* CICLO FINO A ROTTURA DI AREA
015300000000DO  3C     FINL00        DOWEQ     '0'
015400000000     C     WFPDIV        ANDEQ     DEPDIV
015500000000     C     WFPARE        ANDEQ     DEPARE
015600000000     C*
015700000000     C* OPERAZIONI PER NUOVA FILIALE
015800000000     C                   EXSR      NEWFIL
015900000000     C*
016000000000     C* CICLO FINO A ROTTURA DI FILIALE
016100000000DO  4C     FINL00        DOWEQ     '0'
016200000000     C     WFPDIV        ANDEQ     DEPDIV
016300000000     C     WFPARE        ANDEQ     DEPARE
016400000000     C     WFPFIL        ANDEQ     DEPFIL
016500000000     C*
016600000000     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
016700991026     C                   EXSR      NEWPRO
016800991026     C*
016900991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
017000991026DO  5C     FINL00        DOWEQ     '0'
017100991026     C     WFPDIV        ANDEQ     DEPDIV
017200991026     C     WFPARE        ANDEQ     DEPARE
017300991026     C     WFPFIL        ANDEQ     DEPFIL
017400991026     C     WFPKSC        ANDEQ     DEPKSC
017500991026     C     WFPCTR        ANDEQ     DEPCTR
017600991026     C*
017700991026     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
017800991026     C                   EXSR      NEWCLI
017900000000     C*
018000991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
018100991026DO  6C     FINL00        DOWEQ     '0'
018200000000     C     WFPDIV        ANDEQ     DEPDIV
018300000000     C     WFPARE        ANDEQ     DEPARE
018400000000     C     WFPFIL        ANDEQ     DEPFIL
018500000000     C     WFPKSC        ANDEQ     DEPKSC
018600991026     C     WFPCTR        ANDEQ     DEPCTR
018700991026     C     WFPMES        ANDEQ     DEPMES
018800000000     C*
018900000000     C* MEMORIZZA LE SPEDIZIONI
019000000517     C     WRECOK        IFEQ      'S'
019100000000     C                   EXSR      MEMSPE
019200000517     C                   ENDIF
019300000000     C*
019400000000     C* LEGGE IL RECORD SUCCESSIVO
019500000000     C                   EXSR      LETFIL
019600000000     C*
019700991026E   6C                   ENDDO
019800050125     C*
019900050125     C* PROGRESSIVO: STAMPA IL TOTALE CLIENTE/TARIFFA AL MESE (Spostato qui)
020000050125     C                   EXSR      STACLI
020100000000     C*
020200991026E   5C                   ENDDO
020300991026     C*
020400991026E   4C                   ENDDO
020500000000     C*
020600000000     C* STAMPA IL TOTALE FILIALE
020700000000     C                   EXSR      STAFIL
020800000000     C*
020900000000E   3C                   ENDDO
021000000000     C*
021100000000E   2C                   ENDDO
021200000000     C*
021300000000E   1C                   ENDDO
021400000000     C*
021500000000     C*
021600000000     C* STAMPA RIGA FINE STAMPA
021700000000     C                   EXCEPT    STAFIN
021800000000     C*
021900000000     C* OPERAZIONI FINALI
022000000000     C                   EXSR      FINSR
022100090708     c                   endif
022200090512      /free
022300090512       if parord='1';
022400090513          exec sql close WPEScsr;
022500090512       endif;
022600090512      /end-free
022700000000     C*
022800000000     C                   SETON                                        LR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
023100000000     C*------------------------------------------------------------------------*
023200000000     C     SETFIL        BEGSR
023300000000     C*
023400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
023500080318     C                   clear                   KWPANN
023600000000     C                   MOVEL     *BLANKS       KWPDIV
023700000000     C                   Z-ADD     *ZEROS        KWPARE
023800000000     C                   Z-ADD     *ZEROS        KWPFIL
023900000000     C                   Z-ADD     *ZEROS        KWPKSC
024000000000     C                   Z-ADD     *ZEROS        KWPCTR
024100000000     C                   Z-ADD     *ZEROS        KWPMES
024200090708     C**   KEYWFP        SETLL     WFPESF1L                           99
024300090708     c     *loval        setll     wfpesf1l                           99
024400000000     C*
024500000000     C     *IN99         IFEQ      '1'
024600000000     C                   MOVEL     '1'           FINL00
024700000000     C                   ELSE
024800000000     C                   EXSR      LETFIL
024900000000     C                   END
025000000000     C*
025100000000     C                   ENDSR
025200000000     C*------------------------------------------------------------------------*
025300000000     C* LETFIL - LETTURA PROSSIMO RECORD
025400000000     C*------------------------------------------------------------------------*
025500000000     C     LETFIL        BEGSR
025600000000     C*
025700000000     C* LEGGO FINO A:
025800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
025900000000     C*     - TROVATO RECORD VALIDO
026000000000     C                   MOVEL     'N'           WRECOK
026100040213     C                   MOVEL     'N'           WRECOKC
026200000000     C*
026300000000     C* LETTURE SUCCESSIVE ALLA PRIMA
026400090708     C                   READ      WFPESf1L                               99
026500000000     C*
026600000000     C     *IN99         IFEQ      '1'
026700080318     C***  WFPANN        ORNE      PARANN
026800000000     C                   MOVEL     '1'           FINL00
026900000000     C                   ELSE
027000000000     C                   EXSR      CHKREC
027100000517     C                   ENDIF
027200000000     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
027600000000     C*------------------------------------------------------------------------*
027700991025     C     CHKREC        BEGSR
027800000000     C*
027900000000     C                   MOVEL     'S'           WRECOK            1
028000040213     C                   MOVEL     'S'           WRECOKC           1
028100000000     C*
028200000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
028300080318     C**   WFPMES        IFGE      PARMMI
028400080318     C**   WFPMES        ANDLE     PARMMF
028500080318     C**                 ELSE
028600080318     C**                 MOVEL     'N'           WRECOK
028700080318     C**                 END
028800000706     C*
028900000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
029000080318IF  1C*    WRECOK        IFEQ      'S'
029100080318     C*                  Z-ADD     1             I
029200080318     C*    WFPFIL        LOOKUP    FIL(I)                                 95
029300080318IF  2C*    PARPOS        IFEQ      'N'
029400080318IF  3C*    PFIL(I)       IFEQ      'PPT'
029500080318     C*                  MOVEL     'N'           WRECOK
029600080318E   3C*                  ENDIF
029700080318X   2C*                  ELSE
029800080318IF  3C*    PFIL(I)       IFNE      'PPT'
029900080318     C*                  MOVEL     'N'           WRECOK
030000080318E   3C*                  ENDIF
030100080318E   2C*                  ENDIF
030200080318E   1C*                  ENDIF
030300000000     C*
030400000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
030500080318     C**   PARDIV        IFNE      *BLANKS
030600080318     C**   WFPDIV        IFNE      PARDIV
030700080318     C**                 MOVEL     'N'           WRECOK
030800080318     C**                 END
030900080318     C**                 END
031000000000     C*
031100000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
031200080318     C**   PARARE        IFNE      *ZEROS
031300080318     C**   WFPARE        IFNE      PARARE
031400080318     C**                 MOVEL     'N'           WRECOK
031500080318     C**                 END
031600080318     C**                 END
031700000000     C*
031800000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
031900080318     C**   PARFIL        IFNE      *ZEROS
032000080318     C**   WFPFIL        IFNE      PARFIL
032100080318     C**                 MOVEL     'N'           WRECOK
032200080318     C**                 END
032300080318     C**                 END
032400000000     C*
032500000000     C* CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLLETTATI
032600991025     C     WFPNCV        IFGT      *ZEROS
032700991027     C                   Z-ADD     WFPTCL        WCPNCL           12 1
032800991027     C                   Z-ADD     WFPNCV        WCPNCV           12 1
032900000000     C                   EXSR      CALPER
033000000000     C                   Z-ADD     WCPPVA        WPER              6 2
033100991025     C                   ELSE
033200991025     C                   MOVEL     'N'           WRECOK
033300991025     C                   END
033400000000     C*
033500000000     C* CALCOLA PERCENTUALE PESO VDL SU PESO BOLLETTATO
033600991025     C     WFPPSC        IFGT      *ZEROS
033700991027     C                   Z-ADD     WFPPSC        WCPPSC           13 1
033800991027     C                   Z-ADD     WFPPSB        WCPPSB           13 1
033900000000     C                   EXSR      CALRNG
034000000000     C                   Z-ADD     WCPRVA        WRNG              6 2
034100991025     C                   ELSE
034200040213     C                   MOVEL     'N'           WRECOK
034300991025     C                   END
034400000000     C*
034500991229     C* SE LA PERCENTUALE (B) E' INFERIORE ALLA MINIMA RICHIESTA,SCARTO
034600040309     C     WCPPVA        IFLT      PARXXX
034700040309     C     WCPPVA        OREQ      *ZEROS
034800040309     C                   MOVEL     'N'           WRECOKC
034900040309     C                   END
035000000000     C*
035100000517     C* SE LA PERCENTUALE (A) NON E' COMPRESA NEL RANGE INDICATO,SCARTO
035200991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
035300991229     C* DELLO SCARTO MINIMO (PARMIN)
035400040309     C                   Z-ADD     *zeros        PERC              6 2
035500040309     C                   EVAL      PERC=%abs(WCPRVA)
035600040309     C     WCPRVA        IFLT      PARRG1
035700040309     C     WCPRVA        ORGT      PARRG2
035800040309     C     WCPRVA        OREQ      *ZEROS
035900040309     C     PERC          ORLT      PARMIN
036000040309     C                   MOVEL     'N'           WRECOKC
036100040309     C                   END
036200000000     C*
036300000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
036600000000     C*------------------------------------------------------------------------*
036700000000     C     MEMSPE        BEGSR
036800040303     c*
036900040303     C* TOTALE CLIENTE/TARIFFA
037000040303     C                   Z-ADD     0             TCNCV
037100040308     C                   Z-ADD     0             TCNCL
037200040308     C                   Z-ADD     0             TCNSP
037300040303     C                   Z-ADD     0             TCPSB
037400040303     C                   Z-ADD     0             TCPSC
037500090709     c*
037600991028     C* TOTALE CLIENTE/TARIFFA
037700040213IF  2C     WRECOKC       IFEQ      'S'
037800090708     c     ptap          oreq      'M'
037900090708     c     wfppsc        andgt     wfppsb
038000991027     C                   ADD       WFPNCV        TCNCV
038100991027     C                   ADD       WFPTCL        TCNCL
038200991027     C                   ADD       WFPNSP        TCNSP
038300991027     C                   ADD       WFPPSB        TCPSB
038400991027     C                   ADD       WFPPSC        TCPSC
038500040213E   2C                   ENDIF
038600040213     C                   ADD       WFPPSB        TCPSBT
038700040213     C                   ADD       WFPPSC        TCPSCT
038800991027     C* TOTALE FILIALE
038900991027     C                   ADD       WFPNCV        TFNCV
039000991027     C                   ADD       WFPTCL        TFNCL
039100991027     C                   ADD       WFPNSP        TFNSP
039200991027     C                   ADD       WFPPSB        TFPSB
039300991027     C                   ADD       WFPPSC        TFPSC
039400040213     C                   ADD       WFPPSB        TFPSBT
039500040213     C                   ADD       WFPPSC        TFPSCT
039600090708     C* TOTALE commerciale unificante
039700090708     c                   if        parord='1'
039800090708     C                   ADD       WFPNCV        TKNCV
039900090708     C                   ADD       WFPTCL        TKNCL
040000090708     C                   ADD       WFPNSP        TKNSP
040100090708     C                   ADD       WFPPSB        TkPSB
040200090708     C                   ADD       WFPPSC        TKPSC
040300090708     C                   ADD       WFPPSB        TkPSBT
040400090708     C                   ADD       WFPPSC        TkPSCT
040500090708     c                   endif
040600000000     C*
040700000000     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
041000000000     C*------------------------------------------------------------------------*
041100000000     C     NEWDIV        BEGSR
041200000000     C*
041300000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
041400000000     C                   MOVEL     WFPDIV        DEPDIV
041500000000     C*
041600000000     C* DECODIFICA LA DIVISIONE
041700000000     C                   EXSR      DECDIV
041800000000     C*
041900000000     C                   ENDSR
042000000000     C*------------------------------------------------------------------------*
042100000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
042200000000     C*------------------------------------------------------------------------*
042300000000     C     NEWARE        BEGSR
042400000000     C*
042500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
042600000000     C                   Z-ADD     WFPARE        DEPARE
042700000000     C*
042800000000     C* DECODIFICA L'AREA
042900000000     C                   EXSR      DECARE
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
043400000000     C*------------------------------------------------------------------------*
043500000000     C     NEWFIL        BEGSR
043600000000     C*
043700000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
043800000000     C                   Z-ADD     WFPFIL        DEPFIL
043900020910     C*
044000020910     C* REPERISCE IL TERMINAL PARTENZA
044100020910     C                   CLEAR                   FNLV55DS
044200020910     C                   MOVEL     'P'           D55TPT
044300020910     C                   MOVEL     DEPFIL        D55LIN
044400020910     C                   Z-ADD     DATCOR        D55DRF
044500020910     C                   CALL      'FNLV55R'
044600020910     C                   PARM                    FNLV55DS
044700020910IF  1C     D55ERR        IFNE      *BLANKS
044800020910     C                   MOVEL     DEPFIL        D55TFP
044900020910E   1C                   ENDIF
045000020910     C                   Z-ADD     D55TFP        DEPTFP
045100991027     C*
045200020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
045300020910     C     DEPTFP        CHAIN     AZORG01L                           88
045400040301     C     *IN88         IFEQ      *OFF
045500040301     C                   MOVEL     ORGDF0        OG150
045600120914     C     §OGPES        IFNE      *BLANKS
045700120914     C     §OGpes        andNE     *all'0'
045800120914     c***  §ogspp        ifeq      'S'
045900040301     C                   MOVEL     DEC(3)        CHKVDL           20
046000991027     C                   Z-ADD     PARPEI        PARXXX            5 2
046100991027     C                   ELSE
046200991027     C                   MOVEL     DEC(4)        CHKVDL           20
046300991027     C                   Z-ADD     PARPES        PARXXX            5 2
046400991027     C                   ENDIF
046500991027     C                   ELSE
046600040227     C                   MOVEL     DEC(4)        CHKVDL           20
046700040227     C                   Z-ADD     PARPES        PARXXX            5 2
046800040227     C                   ENDIF
046900000000     C*
047000000000     C* AZZERA I TOTALI DELLA FILIALE
047100000000     C                   Z-ADD     *ZEROS        TFNCV             9 0
047200000000     C                   Z-ADD     *ZEROS        TFNCL             9 0
047300040301     C                   Z-ADD     *ZEROS        TFNSP             8 0
047400000000     C                   Z-ADD     *ZEROS        TFPSB            10 1
047500000000     C                   Z-ADD     *ZEROS        TFPSC            10 1
047600040213     C*
047700000517     C                   Z-ADD     *ZEROS        TFPSBT           10 1
047800000517     C                   Z-ADD     *ZEROS        TFPSCT           10 1
047900090708     c* totali di cui ....
048000090708     C                   Z-ADD     *ZEROS        TFNCV_m           9 0
048100090708     C                   Z-ADD     *ZEROS        TFNCL_m           9 0
048200090708     C                   Z-ADD     *ZEROS        TFNSP_m           8 0
048300090708     C                   Z-ADD     *ZEROS        TFPSB_m          10 1
048400090708     C                   Z-ADD     *ZEROS        TFPSC_m          10 1
048500090708     C*
048600090708     C                   Z-ADD     *ZEROS        TFNCVP_m          9 0
048700090708     C                   Z-ADD     *ZEROS        TFNCLP_m          9 0
048800040213     C*
048900000000     C*
049000000000     C* DECODIFICA LA FILIALE
049100000000     C                   EXSR      DECFIL
049200000000     C*
049300000000     C* IMPOSTAZIONI CAMPI IN TESTATA
049400080318     C     PARDET        IFEQ      'S'
049500080318     C*                  MOVEL     *BLANKS       DESFIL
049600080318     C*                  MOVEL     *BLANKS       CHKVDL
049700000000     C                   ELSE
049800000000     C                   SETON                                        01
049900000000     C                   ENDIF
050000000000     C*
050100000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
050200000000     C     *INOF         IFEQ      *ON
050300000000     C     PARDET        OREQ      'D'
050400000000     C                   EXCEPT    TESTA
050500000000     C                   SETOFF                                       OF
050600000000     C                   ENDIF
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000991026     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
051100000000     C*------------------------------------------------------------------------*
051200000000     C     NEWCLI        BEGSR
051300000000     C*
051400000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
051500040220     C                   MOVEL     WFPRAG        DESCLI
051600040227     C                   Z-ADD     WFPMES        DEPMES
051700000000     C*
051800040227     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
051900991026     C                   Z-ADD     *ZEROS        TCNCV             9 0
052000991026     C                   Z-ADD     *ZEROS        TCNCL             9 0
052100991026     C                   Z-ADD     *ZEROS        TCNSP             8 0
052200040301     C                   Z-ADD     *ZEROS        TCPSB            10 1
052300000517     C                   Z-ADD     *ZEROS        TCPSC            10 1
052400040213     C*
052500000517     C                   Z-ADD     *ZEROS        TCPSBT           10 1
052600000517     C                   Z-ADD     *ZEROS        TCPSCT           10 1
052700991026     C*
052800000000     C                   ENDSR
052900991026     C*------------------------------------------------------------------------*
053000991026     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
053100991026     C*------------------------------------------------------------------------*
053200991026     C     NEWPRO        BEGSR
053300991026     C*
053400991026     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
053500991026     C                   Z-ADD     WFPKSC        DEPKSC
053600991026     C                   Z-ADD     WFPCTR        DEPCTR
053700090708     c*
053800090708     c                   clear                   ptap
053900090708     c                   movel     depctr        w_tr              1
054000090708     c                   if        %lookup(w_tr:CTR)>0
054100090708     c                   exsr      rectap
054200090708     c                   endif
054300991026     C*
054400991026     C                   ENDSR
054500090708     C*------------------------------------------------------------------------*
054600090708     C* NEWCOM - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
054700090708     C*------------------------------------------------------------------------*
054800090708     C     NEWCOM        BEGSR
054900090708     C*
055000090710     c                   eval      rotcom=*on
055100090709     C* MEMORIZZA LA NUOVA AREA
055200090708     C                   Z-ADD     WFPcun        DEPcun
055300090708     C*
055400090708     C* DECODIFICA il commerciale unificante
055500090708     C                   EXSR      DECcun
055600090708     c*
055700090708     C* AZZERA I TOTALI DEL commerciale unificante
055800090708     C                   Z-ADD     *ZEROS        TKNCV             9 0
055900090708     C                   Z-ADD     *ZEROS        TKNCL             9 0
056000090708     C                   Z-ADD     *ZEROS        TkNSP             8 0
056100090708     C                   Z-ADD     *ZEROS        TkPSB            10 1
056200090708     C                   Z-ADD     *ZEROS        TKPSC            10 1
056300090708     C*
056400090708     C                   Z-ADD     *ZEROS        TKPSBT           10 1
056500090708     C                   Z-ADD     *ZEROS        TKPSCT           10 1
056600090709     c*
056700090709     C                   Z-ADD     *ZEROS        TKNCV_m
056800090709     C                   Z-ADD     *ZEROS        TKNCL_m
056900090709     C                   Z-ADD     *ZEROS        TkNSP_m
057000090709     C                   Z-ADD     *ZEROS        TkPSB_m
057100090709     C                   Z-ADD     *ZEROS        TKPSC_m
057200090709     C*
057300090709     C                   Z-ADD     *ZEROS        TKPSBT_m
057400090709     C                   Z-ADD     *ZEROS        TKPSCT_m
057500090708      *
057600090708     C*
057700090708     C                   ENDSR
057800000000     C*------------------------------------------------------------------------*
057900991028     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
058000000000     C*------------------------------------------------------------------------*
058100000000     C     STACLI        BEGSR
058200000000     C*
058300000000     C                   MOVEL     '0'           $STDAC
058400000000     C*
058500000000     C* DECODIFICA IL CLIENTE
058600000000     C                   EXSR      DECCLI
058700991026     C*
058800991026     C*------------------------------
058900991026     C*OPERAZIONI PER DETTAGLIO VDL
059000991026     C*------------------------------
059100000000     C* DECODIFICA LA TARIFFA
059200000000     C                   MOVEL     *BLANKS       DESCTR
059300000000     C                   EXSR      DECCTR
059400080110      * recupero tipo addebito peso dalla tariffa
059500090708     c****               clear                   ptap
059600090708     c****               exsr      rectap
059700000000     C*
059800000000     C                   Z-ADD     TCNCV         STNCV             9 0
059900040227     C                   Z-ADD     TCNCL         STNCL             9 0
060000040227     C                   Z-ADD     TCNSP         STNSP             8 0
060100040227     C                   Z-ADD     TCPSB         STPSB            10 1
060200040227     C                   Z-ADD     TCPSC         STPSC            10 1
060300000000     C*
060400000000     C* CALCOLA PERCENTUALE COLLI VDL
060500040301     C                   Z-ADD     TCNCL         WCPNCL           12 1
060600040301     C                   Z-ADD     TCNCV         WCPNCV           12 1
060700040301     C                   EXSR      CALPER
060800040301     C                   Z-ADD     WCPPVA        WPER              6 2
060900040301     C                   Z-ADD     WPER          STPER             6 1
061000000000     C*
061100000000     C* CALCOLA PERCENTUALE PESO VDL
061200991027     C                   Z-ADD     TCPSC         WCPPSC           13 1
061300991027     C                   Z-ADD     TCPSB         WCPPSB           13 1
061400000000     C                   EXSR      CALRNG
061500000000     C                   Z-ADD     WCPRVA        WRNG              6 2
061600000516     C                   Z-ADD     WRNG          STRNG             6 1
061700000516     C*
061800000516     C* CALCOLA PERCENTUALE PESO VDL TOTALI
061900000516     C                   Z-ADD     TCPSCT        WCPPSC           13 1
062000000516     C                   Z-ADD     TCPSBT        WCPPSB           13 1
062100000516     C                   EXSR      CALRNG
062200000516     C                   Z-ADD     WCPRVA        WRNG              6 2
062300000516     C                   Z-ADD     WRNG          STTTP             6 1
062400991026     C*
062500991028     C*
062600080318     C     STNSP         ifgt      *ZEROS
062700090710     c*
062800090710     c* prima di stampare la riga del cliente/tariffa, se c'è stata
062900090710     c* rottura di commerciale devo prima stampare il nuovo commerciale
063000090710     c                   if        parord='1'  and rotcom=*on
063100090710     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
063200090710     C     *INOF         IFEQ      *ON
063300090710     C     PARpag        OREQ      'S'
063400090710     c     depcun_p      andgt     0
063500090710     c                   seton                                        01
063600090710     C                   EXCEPT    TESTA
063700090710     C                   SETOFF                                       OF
063800090710     C                   ENDIF
063900090710     c                   eval      depcun_p=depcun
064000090710     c* stampo la riga del commerciale unificante
064100090710     c                   if        pardet='D'
064200090710     c                   except    rigcom
064300090710     c                   endif
064400090710     c                   eval      rotcom=*off
064500090710     c                   endif
064600090710     c*
064700991026     C                   EXSR      STRCLI
064800991026     C                   MOVEL     '1'           $STDAC
064900090708     c* Incremento totali "Di cui tariffe con tipo applicazione "M"=MAI VDL"
065000090708     c* Totali di FILIALE
065100090708     c                   if        ptap='M' and tcpsc>tcpsb
065200090708     C                   ADD       tcncv         TFNCV_m
065300090708     C                   ADD       tcncl         TFNCL_m
065400090708     C                   ADD       tcnsp         TFNSP_m
065500090708     C                   ADD       tcpsb         TFPSB_m
065600090708     C                   ADD       tcpsc         TFPSC_m
065700090708     C* Totale commerciale unificante
065800090708     C                   ADD       tcncv         TkNCV_m           9 0
065900090708     C                   ADD       tcncl         TkNCL_m           9 0
066000090708     C                   ADD       tcnsp         TkNSP_m           8 0
066100090708     C                   ADD       tcpsb         TkPSB_m          10 1
066200090708     C                   ADD       tcpsc         TkPSC_m          10 1
066300090708     C                   ADD       tcpsb         TkPSBT_m         10 1
066400090708     C                   ADD       tcpsc         TkPSCT_m         10 1
066500090708     c                   endif
066600991028     C                   ENDIF
066700000000     C*
066800000000     C                   ENDSR
066900000000     C*------------------------------------------------------------------------*
067000000000     C* STAFIL - STAMPA TOTALI FILIALE
067100000000     C*------------------------------------------------------------------------*
067200000000     C     STAFIL        BEGSR
067300040213     C*
067400090708     c                   setoff                                       21
067500000000     C                   MOVEL     TES(10)       STDES1           22
067600000000     C*
067700000000     C* DECODIFICA LA FILIALE
067800080318     C*****              EXSR      DECFIL
067900080318     C                   Z-ADD     1             I
068000080318     C     partfp        LOOKUP    FIL(I)                                 95
068100080318     C     *IN95         IFEQ      '0'
068200080318     C                   MOVEL     *ALL'*'       DESFIL           20
068300080318     C                   ELSE
068400080318     C                   MOVEL     DFIL(I)       DESFIL
068500080318     C                   END
068600000000     C                   MOVEL     DESFIL        STDES2           20
068700000000     C*
068800000000     C* IMPOSTO I VALORI DI STAMPA
068900000000     C                   Z-ADD     TFNCV         STNCV
069000991027     C                   Z-ADD     TFNCL         STNCL
069100000000     C                   Z-ADD     TFNSP         STNSP
069200000000     C                   Z-ADD     TFPSB         STPSB
069300000000     C                   Z-ADD     TFPSC         STPSC
069400000516     C*
069500000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
069600000516     C                   Z-ADD     TFPSCT        WCPPSC           13 1
069700040308     C                   Z-ADD     TFPSBT        WCPPSB           13 1
069800040308     C                   EXSR      CALRNG
069900000516     C                   Z-ADD     WCPRVA        STTTP
070000000516     C*
070100090708     c* se stampa per commerciale questo totale lo devo stampare a pagina
070200090708     c* nuova
070300090708     c                   if        parord='1'
070400090708     c                   setoff                                       01
070500090708     c                   except    testa
070600090708     c                   endif
070700991026     C*
070800000000     C* SE DETTAGLIO
070900000000     C     PARDET        IFEQ      'D'
071000000000     C                   EXCEPT    RIGSEP
071100991027     C                   ENDIF
071200000000     C*
071300000000     C* STAMPO RIGA TOTALI
071400000000     C                   EXSR      STRTOT
071500090708     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
071600090708     c                   if        tfnsp_m>0
071700090708     C* IMPOSTO I VALORI DI STAMPA
071800090708     C                   Z-ADD     TFNCV_m       STNCV
071900090708     C                   Z-ADD     TFNCL_m       STNCL
072000090708     C                   Z-ADD     TFNSP_m       STNSP
072100090708     C                   Z-ADD     TFPSB_m       STPSB
072200090708     C                   Z-ADD     TFPSC_m       STPSC
072300090708     C*
072400090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
072500090708     C*
072600090708     c                   eval      stdes3=tes(22)
072700090708     c                   seton                                        21
072800090708     c                   exsr      strtot
072900090708     c                   endif
073000000000     C*
073100000000     C* SE DETTAGLIO
073200040301     C     PARDET        IFEQ      'D'
073300000000     C                   EXCEPT    RIGSEP
073400000000     C                   ENDIF
073500000000     C*
073600000000     C                   ENDSR
073700090708     C*------------------------------------------------------------------------*
073800090708     C* STAcom - STAMPA TOTALI commerciale unificante
073900090708     C*------------------------------------------------------------------------*
074000090708     C     STAcom        BEGSR
074100090708     C*
074200090710     C* SE NON CI SONO pesi, NON STAMPO
074300090710IF  1c     TkpsB         IFEQ      *ZEROS
074400090710     C     TkpsC         ANDEQ     *ZEROS
074500090710X   1C                   ELSE
074600090709     c                   setoff                                       21
074700090708     C                   MOVEL     TES(21)       STDES1           22
074800090708     c                   movel     descun        stdes2
074900090708     C*
075000090708     C* IMPOSTO I VALORI DI STAMPA
075100090708     C                   Z-ADD     TkNCV         STNCV
075200090708     C                   Z-ADD     TkNCL         STNCL
075300090708     C                   Z-ADD     TkNSP         STNSP
075400090708     C                   Z-ADD     TkPSB         STPSB
075500090708     C                   Z-ADD     TkPSC         STPSC
075600090708     C*
075700090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
075800090708     C                   Z-ADD     TkPSCT        WCPPSC           13 1
075900090708     C                   Z-ADD     TkPSBT        WCPPSB           13 1
076000090708     C                   EXSR      CALRNG
076100090708     C                   Z-ADD     WCPRVA        STTTP
076200090708     C*
076300090708     C*
076400090708     C* SE DETTAGLIO
076500090708     C     PARDET        IFEQ      'D'
076600090708     C                   EXCEPT    RIGSEP
076700090708     C                   ENDIF
076800090708     C*
076900090708     C* STAMPO RIGA TOTALI
077000090708     C                   EXSR      STRTOT
077100090708     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
077200090708     c                   if        tknsp_m>0
077300090708     C* IMPOSTO I VALORI DI STAMPA
077400090708     C                   Z-ADD     TkNCV_m       STNCV
077500090708     C                   Z-ADD     TkNCL_m       STNCL
077600090708     C                   Z-ADD     TkNSP_m       STNSP
077700090708     C                   Z-ADD     TkPSB_m       STPSB
077800090708     C                   Z-ADD     TkPSC_m       STPSC
077900090708     C*
078000090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
078100090708     C                   Z-ADD     TkPSCT_m      WCPPSC           13 1
078200090708     C                   Z-ADD     TkPSBT_m      WCPPSB           13 1
078300090708     C                   EXSR      CALRNG
078400090708     C                   Z-ADD     WCPRVA        STTTP
078500090708     C*
078600090709     c                   eval      stdes3=tes(22)
078700090708     c                   seton                                        21
078800090708     c                   exsr      strtot
078900090708     c                   endif
079000090708     C*
079100090708     C* SE DETTAGLIO
079200090708     C     PARDET        IFEQ      'D'
079300090708     C                   EXCEPT    RIGSEP
079400090708     C                   ENDIF
079500090710     c*
079600090710     c                   endif
079700090708     C*
079800090708     C                   ENDSR
079900000000     C*------------------------------------------------------------------------*
080000000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
080100000000     C*------------------------------------------------------------------------*
080200000000     C     STRCLI        BEGSR
080300040213     C*
080400991105 MOD C     PARDET        IFEQ      'D'
080500000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
080600040308     C     *INOF         IFEQ      '1'
080700040308     C                   SETON                                        01
080800040308     C                   EXCEPT    RIGSEP
080900040308     C                   EXCEPT    TESTA
081000040308     C                   SETOFF                                       OF
081100000000     C                   END
081200000000     C*
081300000000     C     $STDAC        IFEQ      '1'
081400000000     C                   Z-ADD     *ZEROS        DEPKSC
081500040220     C                   MOVEL     *BLANKS       DESCLI
081600000000     C                   Z-ADD     *ZEROS        DEPCTR
081700991026     C                   Z-ADD     *ZEROS        DEPMES
081800000000     C                   MOVEL     *BLANKS       DESCTR
081900991026     C*
082000991026     C                   ENDIF
082100000000     C*
082200000000     C* STAMPO RIGA CLIENTE/TARIFFA
082300040308     C                   EXCEPT    RIGCLI
082400040308 MOD C                   ENDIF
082500991028     C*
082600000000     C*
082700000000     C                   ENDSR
082800000000     C*------------------------------------------------------------------------*
082900000000     C* STRTOT - STAMPA RIGA TOTALE
083000000000     C*------------------------------------------------------------------------*
083100000000     C     STRTOT        BEGSR
083200991027     C*
083300991027     C*-------------------------
083400991027     C* OPERAZIONI X STAMPA DATI DEL MESE
083500991027     C*-------------------------
083600991027     C*
083700000000     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO
083800040309     c     stpsc         ifeq      stpsb
083900040309     c                   z-add     *zeros        strng
084000040309     c                   else
084100040309     C                   Z-ADD     STPSC         WCPPSC           13 1
084200040309     C                   Z-ADD     STPSB         WCPPSB           13 1
084300040309     C                   EXSR      CALRNG
084400040309     C                   Z-ADD     WCPRVA        STRNG
084500040309     c                   endif
084600991025     C*
084700991025     C* CALCOLA PERCENTUALE COLLI VDL E BOLLETTATO
084800040309     c     stncl         ifeq      stncv
084900040309     c                   z-add     *zeros        stper
085000040309     c                   else
085100040309     C                   Z-ADD     STNCL         WCPNCL           12 1
085200040309     C                   Z-ADD     STNCV         WCPNCV           12 1
085300040309     C                   EXSR      CALPER
085400040309     C                   Z-ADD     WCPPVA        STPER
085500040309     c                   endif
085600000000     C*
085700000000     C* STAMPO RIGA CLIENTE/TARIFFA
085800000000     C                   EXCEPT    RIGTOT
085900000000     C*
086000000000     C                   ENDSR
086100000000     C*------------------------------------------------------------------------*
086200000000     C* DECDIV - DECODIFICA DIVISIONE
086300000000     C*------------------------------------------------------------------------*
086400000000     C     DECDIV        BEGSR
086500000000     C*
086600020906     C                   MOVEL     1             KTBKUT
086700000000     C                   MOVE      '17'          KTBCOD
086800000000     C                   MOVE      *BLANKS       KTBKEY
086900000000     C                   MOVEL     DEPDIV        KTBKEY
087000000000     C     KEYTAB        CHAIN     TABEL                              97
087100000000     C     *IN97         IFEQ      '1'
087200000000     C     TBLFLG        OREQ      '*'
087300000000     C                   MOVEL     *ALL'*'       DESDIV           20
087400000000     C                   ELSE
087500000000     C                   MOVEL     TBLUNI        DESDIV
087600000000     C                   END
087700000000     C*
087800000000     C                   ENDSR
087900000000     C*------------------------------------------------------------------------*
088000000000     C* DECARE - DECODIFICA AREA
088100000000     C*------------------------------------------------------------------------*
088200000000     C     DECARE        BEGSR
088300000000     C*
088400020906     C                   MOVEL     1             KTBKUT
088500000000     C                   MOVE      '05'          KTBCOD
088600000000     C                   MOVE      *BLANKS       KTBKEY
088700000000     C                   MOVEL     DEPARE        KTBKEY
088800000000     C     KEYTAB        CHAIN     TABEL                              97
088900000000     C     *IN97         IFEQ      '1'
089000000000     C     TBLFLG        OREQ      '*'
089100000000     C                   MOVEL     *ALL'*'       DESARE           20
089200000000     C                   ELSE
089300000000     C                   MOVEL     TBLUNI        DESARE
089400000000     C                   END
089500000000     C*
089600000000     C                   ENDSR
089700090708     C*------------------------------------------------------------------------*
089800090708     C* DECcun - DECODIFICA commerciale unificante
089900090708     C*------------------------------------------------------------------------*
090000090708     C     DECcun        BEGSR
090100090708     C*
090200140611     C     DEPcun        chain     AZCMM000
090300140611     c                   IF        NOT %found(AZCMM01L)  or
090400140611     C                             CMMatb <> *blank
090500090708     C                   MOVEL     *ALL'*'       DEScun           25
090600090708     C                   ELSE
090700140611     C                   MOVEL     CMMdes        descun
090800090708     C                   END
090900090708     C*
091000090708     C                   ENDSR
091100000000     C*------------------------------------------------------------------------*
091200000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
091300000000     C*------------------------------------------------------------------------*
091400000000     C     DECFIL        BEGSR
091500000000     C*
091600000000     C                   Z-ADD     1             I
091700000000     C     DEPFIL        LOOKUP    FIL(I)                                 95
091800000000     C     *IN95         IFEQ      '0'
091900000000     C                   MOVEL     *ALL'*'       DESFIL           20
092000000000     C                   ELSE
092100000000     C                   MOVEL     DFIL(I)       DESFIL
092200000000     C                   END
092300991026     C*
092400000000     C                   ENDSR
092500000000     C*------------------------------------------------------------------------*
092600000000     C* DECCLI - DECODIFICA CLIENTE
092700000000     C*------------------------------------------------------------------------*
092800000000     C     DECCLI        BEGSR
092900000000     C*
093000000000     C                   CLEAR                   BS69DS
093100000000     C                   CLEAR                   ACODS
093200000000     C                   MOVEL     KNSIF         I69SIF
093300000000     C                   Z-ADD     DEPKSC        I69KAC
093400000000     C                   CALL      'TIBS69R'
093500000000     C                   PARM                    BS69DS
093600000000     C                   PARM                    ACODS
093700000000     C                   PARM                    INDDS
093800000000     C                   PARM                    CLPDS
093900000000     C                   PARM                    CLSDS
094000000000     C     O69ERR        IFNE      '1'
094100000000     C                   MOVEL     ACORAG        DESCLI           30
094200000000     C                   ELSE
094300000000     C                   MOVEL     *ALL'*'       DESCLI           30
094400000000     C                   ENDIF
094500000000     C*
094600000000     C                   ENDSR
094700000000     C*------------------------------------------------------------------------*
094800000000     C* DECCTR - DECODIFICA TARIFFA
094900000000     C*------------------------------------------------------------------------*
095000000000     C     DECCTR        BEGSR
095100000000     C*
095200000000     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
095300000000     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
095400000000     C     DEPCTR        IFEQ      999
095500000000     C                   MOVEL     DEC(1)        DESCTR           15
095600000000     C                   ELSE
095700000000     C     DEPCTR        IFEQ      888
095800000000     C                   MOVEL     DEC(2)        DESCTR
095900000000     C                   ELSE
096000020906     C                   MOVEL     1             KTBKUT
096100000000     C                   MOVE      'TR'          KTBCOD
096200000000     C                   MOVE      *BLANKS       KTBKEY
096300000000     C                   MOVEL     DEPCTR        WTEM01            1
096400000000     C                   MOVEL     WTEM01        KTBKEY
096500000000     C     KEYTAB        CHAIN     TABEL                              97
096600000000     C     *IN97         IFEQ      '1'
096700000000     C     TBLFLG        OREQ      '*'
096800000000     C                   MOVEL     *ALL'*'       DESCTR
096900000000     C                   ELSE
097000000000     C                   MOVEL     TBLUNI        DESCTR
097100000000     C                   ENDIF
097200000000     C                   ENDIF
097300000000     C                   ENDIF
097400000000     C*
097500000000     C                   ENDSR
097600080110
097700080110      *------------------------------------------------------------------------*
097800080110      * Recupero tipo addebito peso da tariffa
097900080110      *------------------------------------------------------------------------*
098000080110     c     rectap        begsr
098100080110
098200080110     c                   clear                   dsta01
098300080110     c     ktntam        setgt     tntam01l
098400080110     c     ktntam        readpe    tntam01l
098500080110     c                   eval      dsta01 = tamflo
098600080110     c                   eval      ptap = §tatap
098700080110
098800080110     c                   endsr
098900080110
099000000000     C*------------------------------------------------------------------------*
099100000000     C* CALPER - CALCOLA PERCENTUALE COLLI
099200000000     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
099300000000     C*          - COLLI VDL               (WCPNCV)
099400000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
099500000000     C*------------------------------------------------------------------------*
099600040301     C     CALPER        BEGSR
099700000000     C*
099800000000     C* ===> CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLL.
099900000000     C                   Z-ADD     *ZEROS        WCPPVA            6 2
100000991025     C     WCPNCL        IFNE      WCPNCV
100100991025     C     WCPNCL        ANDGT     *ZEROS
100200040308     C     WCPNCV        ANDGT     *ZEROS
100300040308     C     WCPNCV        MULT      100           WMULTI           17 3
100400040301     C     WMULTI        DIV       WCPNCL        WRKPER           17 3
100500991025     C     WRKPER        IFGT      9999
100600000000     C                   Z-ADD     9999,99       WCPPVA
100700040308     C                   ELSE
100800040301     C                   Z-ADD     WRKPER        WCPPVA
100900040301     C                   END
101000040308     C                   END
101100991027     C*
101200991102     C     WCPNCL        IFEQ      WCPNCV
101300040301     C     WCPNCL        ANDGT     *ZEROS
101400991102     C     WCPNCV        ANDGT     *ZEROS
101500040308     C                   Z-ADD     100           WCPPVA
101600991102     C                   ENDIF
101700040301     C*
101800040308     C     WCPPVA        IFGT      100
101900040308     C                   Z-ADD     100           WCPPVA
102000040308     C                   ENDIF
102100991102     C*
102200040301     C                   ENDSR
102300000000     C*------------------------------------------------------------------------*
102400000000     C* CALRNG - CALCOLA PERCENTUALE RANGE
102500000000     C* INPUT:   - PESO BOLLETTAT0         (WCPPSB)
102600000000     C*          - PESO VDL                (WCPPSC)
102700000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPRVA)
102800000000     C*------------------------------------------------------------------------*
102900000000     C     CALRNG        BEGSR
103000000000     C*
103100000000     C* ===> CALCOLA PERCENTUALE PESO VDL SU PESO BOLL.
103200000000     C                   Z-ADD     *ZEROS        WCPRVA            6 2
103300000000     C     WCPPSC        IFNE      WCPPSB
103400040308     C     WCPPSC        ANDGT     *ZEROS
103500000000     C     WCPPSB        ANDGT     *ZEROS
103600991027     C     WCPPSB        SUB       WCPPSC        WDIFFE           13 1
103700040308     C     WDIFFE        MULT      100           WRKPER           17 3
103800991027     C     WRKPER        DIV       WCPPSB        WRKPER
103900000000     C     WRKPER        IFGT      9999
104000040308     C                   Z-ADD     9999,99       WCPRVA
104100040308     C                   ELSE
104200040308     C                   Z-ADD     WRKPER        WCPRVA
104300040308     C                   END
104400000000     C                   END
104500991102     C*
104600991102     C     WCPPSC        IFEQ      WCPPSB
104700991102     C     WCPPSC        ANDGT     *ZEROS
104800991102     C     WCPPSB        ANDGT     *ZEROS
104900040308     C                   Z-ADD     100           WCPPVA
105000991102     C                   ENDIF
105100040308     C*
105200040308     C     WCPPVA        IFGT      100
105300040308     C                   Z-ADD     100           WCPPVA
105400040308     C                   ENDIF
105500000000     C*
105600000000     C                   ENDSR
105700000000     C*------------------------------------------------------------------------*
105800000000     C* FINSR - OPERAZIONI FINALI
105900000000     C*------------------------------------------------------------------------*
106000000000     C     FINSR         BEGSR
106100000000     C*
106200000000     C* CHIUDE I FILE DEI PGM CHIAMATI
106300000000     C                   CLEAR                   BS69DS
106400000000     C                   CLEAR                   ACODS
106500000000     C                   CLEAR                   INDDS
106600000000     C                   CLEAR                   CLPDS
106700000000     C                   CLEAR                   CLSDS
106800000000     C                   MOVEL     'C'           I69TLA
106900000000     C                   MOVEL     KNSIF         I69SIF
107000000000     C                   CALL      'TIBS69R'
107100000000     C                   PARM                    BS69DS
107200000000     C                   PARM                    ACODS
107300000000     C                   PARM                    INDDS
107400000000     C                   PARM                    CLPDS
107500000000     C                   PARM                    CLSDS
107600000000     C*
107700000000     C                   ENDSR
107800090512     C*------------------------------------------------------------------------*
107900090708     C* ordinamento per commerciale unificante
108000090512     C*------------------------------------------------------------------------*
108100090708     C     sr_ordcomset  BEGSR
108200090512      /free
108300090708       exec sql declare WPEScsr cursor for Select * from wfpesf0f
108400090708        order by wfpann, wfpdiv, wfpare, wfpfil, wfpcun, wfprag, wfpksc, wfpctr,
108500090708         wfpmes;
108600090512       exec sql open WPEScsr;
108700090512      /end-free
108800090512     c                   endsr
108900090512     C*------------------------------------------------------------------------*
109000090708     C* ordinamento per commerciale unificante
109100090512     C*------------------------------------------------------------------------*
109200090708     C     sr_ordcomlet  BEGSR
109300090513      /free
109400090512       wrecok='N';
109500090512       wrecokc='N';
109600090512       exec sql Fetch WPEScsr into :wfpesds;
109700090512         if sqlcod=100 or sqlcod<0;
109800090512            finl00='1';
109900090512         else;
110000090512            exsr chkrec;
110100090512         endif;
110200090512      /end-free
110300090512     c                   endsr
110400090708     C*------------------------------------------------------------------------*
110500090708     C* elaborazione e stampa per commerciale unificante
110600090708     C*------------------------------------------------------------------------*
110700090708     C     sr_elabcom    BEGSR
110800090708     c* solo la prima volta
110900090708     c                   if        w_primav=*off
111000090708     c                   exsr      newdiv
111100090708     c                   exsr      neware
111200090708     c                   exsr      newfil
111300090708     c                   eval      w_primav=*on
111400090708     c                   endif
111500090708     c     finl00        doweq     '0'
111600090708     c* operazioni per nuovo commerciale unificante
111700090708     c                   exsr      newcom
111800090708     c     finl00        doweq     '0'
111900090708     c     wfpcun        andeq     depcun
112000090708     c* operazioni per nuovo cliente/tariffa
112100090708     c                   exsr      newcli
112200090708     C                   z-add     WFPKSC        DEPKSC
112300090708     C                   z-add     WFPCTR        DEPCTR
112400090708     c                   clear                   ptap
112500090708     c                   movel     depctr        w_tr              1
112600090708     c                   if        %lookup(w_tr:CTR)>0
112700090708     c                   exsr      rectap
112800090708     c                   endif
112900090708     c     finl00        doweq     '0'
113000090708     c     wfpcun        andeq     depcun
113100090708     c     wfpksc        andeq     depksc
113200090708     c     wfpctr        andeq     depctr
113300090708     C* Memorizza le spedizioni
113400090708     C     WRECOK        IFEQ      'S'
113500090708     C                   exsr      memspe
113600090708     C                   ENDIF
113700090708     c* leggo il record successivo
113800090708     c                   exsr      sr_ordcomlet
113900090708     c                   enddo
114000090708     c* stampa totale cliente/tariffa
114100090708     c                   exsr      stacli
114200090708     c                   enddo
114300090708     c* stampa totale commerciale unificante
114400090708     c                   exsr      stacom
114500090708     c                   enddo
114600090708     c                   exsr      stafil
114700090708     C* STAMPA RIGA FINE STAMPA
114800090708     C                   EXCEPT    stafin
114900090708     C* OPERAZIONI FINALI
115000090708     C                   exsr      finsr
115100090708     c                   endsr
115200000000     C*------------------------------------------------------------------------*
115300000000     C* *INZSR - ROUTINE INIZIALE
115400000000     C*------------------------------------------------------------------------*
115500000000     C     *INZSR        BEGSR
115600000000     C*--------------------
115700000000     C* RICEVIMENTO PARAMENTRI
115800000000     C*--------------------
115900000000     C     *ENTRY        PLIST
116000000000     C                   PARM                    KPJBA
116100000000     C                   MOVEL     KPJBU         PARAM
116200000000     C*--------------------
116300000000     C* CAMPI RIFERITI AL DATA BASE
116400000000     C*--------------------
116500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
116600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
116700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
116800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
116900000000     C     *LIKE         DEFINE    WFPANN        KWPANN
117000000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
117100000000     C     *LIKE         DEFINE    WFPARE        KWPARE
117200000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
117300040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
117400000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
117500000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
117600000000     C     *LIKE         DEFINE    WFPMES        KWPMES
117700000000     C     *LIKE         DEFINE    WFPDIV        DEPDIV
117800000000     C     *LIKE         DEFINE    WFPARE        DEPARE
117900000000     C     *LIKE         DEFINE    WFPFIL        DEPFIL
118000020910     C     *LIKE         DEFINE    WFPFIL        DEPTFP
118100000000     C     *LIKE         DEFINE    WFPKSC        DEPKSC
118200000000     C     *LIKE         DEFINE    WFPCTR        DEPCTR
118300991026     C     *LIKE         DEFINE    WFPMES        DEPMES
118400090708     C     *LIKE         DEFINE    WFPcun        DEPcun
118500090710     C     *LIKE         DEFINE    WFPcun        DEPcun_p
118600000000     C*
118700000000     C*--------------------
118800000000     C* CHIAVI DI LETTURA
118900000000     C*--------------------
119000000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
119100000000     C     KEYTAB        KLIST
119200000000     C                   KFLD                    KTBKUT
119300000000     C                   KFLD                    KTBCOD
119400000000     C                   KFLD                    KTBKEY
119500090708     c*
119600090708     C     KEYTABp       KLIST
119700090708     C                   KFLD                    KTBKUT
119800090708     C                   KFLD                    KTBCOD
119900000000     C*
120000000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
120100000000     C     KEYORG        KLIST
120200000000     C                   KFLD                    KORFIL
120300000000     C*
120400090708     C* CHIAVE LETTURA WFPESf1L - COMPLETA
120500000000     C     KEYWFP        KLIST
120600000000     C                   KFLD                    KWPANN
120700000000     C                   KFLD                    KWPDIV
120800000000     C                   KFLD                    KWPARE
120900000000     C                   KFLD                    KWPFIL
121000040220     C                   KFLD                    KWPRAG
121100000000     C                   KFLD                    KWPKSC
121200000000     C                   KFLD                    KWPCTR
121300000000     C                   KFLD                    KWPMES
121400080110
121500080110     c     ktntam        klist
121600080110     c                   kfld                    depksc
121700080110     c                   kfld                    depctr
121800020910     C*--------------------
121900020910     C* REPERISCE DATA/ORA CORRENTE
122000020910     C*--------------------
122100020910     C                   TIME                    N14              14 0
122200020910     C                   MOVE      N14           G08DAT            8 0
122300020910     C                   Z-ADD     *ZEROS        G08INV
122400020910     C                   MOVEL     '0'           G08ERR
122500020910     C                   CALL      'XSRDA8'
122600020910     C                   PARM                    WLBDA8
122700020910     C                   Z-ADD     G08INV        DATCOR            8 0
122800000000     C*--------------------
122900000000     C* CAMPI DI CONTROLLO
123000000000     C*--------------------
123100000000     C                   MOVEL     '0'           FINL00            1
123200000000     C                   SETON                                        OF
123300000000     C                   Z-ADD     UDATE         DATA              6 0
123400000000     C                   TIME                    ORA               6 0
123500000000     C                   MOVEL     '0'           $STDAC            1
123600991229     C*
123700991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
123800991229     C* PER FACILITARE I CONTROLLI
123900991229     C                   EVAL      PARMIN=%abs(PARMIN)
124000000000     C*
124100000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
124200000000IF  1C     PARDET        IFEQ      'D'
124300000000     C                   MOVEL     INT(1)        DECPAR            9
124400000000X   1C                   ELSE
124500000000     C                   MOVEL     INT(2)        DECPAR
124600000000E   1C                   ENDIF
124700000706     C*
124800000000     C*
124900000000     C*
125000000000     C* CARICA ORGANIGRAMMA
125100000000     C                   EXSR      CARORG
125200000000     C*
125300090513     C**                 SETOFF                                       02
125400000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
125500000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
125600080318     C**   PARDIV        IFEQ      *BLANKS
125700080318     C**   PARARE        ANDEQ     *ZEROS
125800080318     C**   PARFIL        ANDEQ     *ZEROS
125900080318     C**                 SETOFF                                       02
126000080318     C**                 ELSE
126100080318     C**                 SETON                                        02
126200080318     C**                 END
126300090513     c                   if        parord='1'
126400090708     c                   eval      orddes='Comm.le unificante'
126500090513     c                   else
126600090708     c                   eval      orddes='Ragione Sociale   '
126700090513     c                   endif
126800090708     c                   clear                   decflp
126900090708     c                   setoff                                       04
127000090708     c                   if        parflp>0
127100090708     c                   seton                                        04
127200090708     c     parflp        chain     azorg01l
127300090708     c                   if        %found(azorg01l)
127400090708     C                   MOVEL     ORGDES        decflp
127500090708     c                   endif
127600090708     c                   endif
127700000000     C*
127800000000     C                   SETOFF                                       03
127900080318     c* inverto data spedizione dal/al in giorno/mese/anno per la stampa
128000080318     c                   move      pardal        dataiso
128100080318     c                   move      dataiso       dataeur
128200080318     c                   move      dataeur       pardal_E
128300080318     c                   move      paral         dataiso
128400080318     c                   move      dataiso       dataeur
128500080318     c                   move      dataeur       paral_E
128600090708     c*
128700090708     c* Carico tabella tipi tariffa aventi flag "gestione rapporto peso/vol"='S'
128800090708     c                   clear                   ctr
128900090708     c                   clear                   ii
129000090708     C                   MOVEL     1             KTBKUT
129100090708     C                   MOVE      'TR'          KTBCOD
129200090708     c     keytabp       setll     tabel00f
129300090708     c     keytabp       reade     tabel00f
129400090708     c                   dow       not %eof(tabel00f)
129500090708     c                   movel     tbluni        dstr
129600090708     c                   if        §trrpv='S'
129700090708     c                   add       1             ii
129800090708     c                   movel     tblkey        ctr(ii)
129900090708     c                   endif
130000090708     c     keytabp       reade     tabel00f
130100090708     c                   enddo
130200000000     C*
130300000000     C                   ENDSR
130400000000     C*------------------------------------------------------------------------*
130500000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
130600000000     C*------------------------------------------------------------------------*
130700000000     C     CARORG        BEGSR
130800000000     C*
130900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
131000000000     C                   Z-ADD     *ZEROS        KORFIL
131100000000     C     KEYORG        SETLL     AZORG01L                           98
131200000000     C  N98              READ      AZORG01L                               98
131300000000     C                   Z-ADD     0             I                 3 0
131400000000     C     *IN98         DOWEQ     '0'
131500991025     C*          ORGFVA    IFEQ *BLANKS                    --- 2 -->
131600000000     C     ORGFAG        IFNE      'V'
131700000000     C                   ADD       1             I
131800000000     C                   Z-ADD     ORGFIL        FIL(I)
131900000000     C                   MOVEL     ORGDES        DFIL(I)
132000040308     C                   MOVEL     ORGDE3        OG143
132100020627     C                   MOVEL     §OGNTW        PFIL(I)
132200000000     C                   END
132300991025     C*                    END                             <-- 2 ---
132400000000     C                   READ      AZORG01L                               98
132500000000     C                   END
132600000000     C*
132700000000     C                   ENDSR
132800000000     O*------------------------------------------------------------------------*
132900000000     OPRTF198   E            TESTA          1 02
133000080318     O                                            8 'FNLSF3R '
133100000000     O                                           90 '***      STATISTICA  '
133200000000     O                                          110 ' P E S I     ***  '
133300000000     O                       DECPAR             120
133400000000     O                                          150 'Data'
133500000000     O                       DATA               159 '  /  /  '
133600000000     O                                          169 'Ora'
133700000000     O                       ORA                178 '  :  :  '
133800000000     O                                          193 'Pag.'
133900000000     O                       PAGE          Z    198
134000000000     O*------------------------
134100000000     O          E   N03      TESTA          1
134200080318     O                                           85 'DATA Spedizione dal'
134300080318     O                       PARdal_E      8  +   1
134400080318     O                                        +   2 'al'
134500080318     O                       PARal_E       8  +   1
134600080318     O***                                        97 'MESI:'
134700080318     O***                    PARMMI             100
134800080318     O***                                       102 '/'
134900080318     O***                    PARMMF             105
135000080318     O***                    DECPOS             198
135100000000     O*------------------------
135200090513     O          E            TESTA       1  1
135300090513     O                                           20 'ORDINAMENTO: per'
135400090513     O                       orddes           +   1
135500090709     o               04                       +   4 'SOLO FILIALE DI PARTENZA'
135600090708     o               04      parflp           +   1
135700090708     o               04      decflp           +   1
135800080318     O***                                        61 '               AREA:'
135900080318     O***                    PARARE        Z     64
136000080318     O***                                        84 '            P.OPER.:'
136100080318     O***                    PARFIL        Z     87
136200000000     O*------------------------
136300000000     O          E            TESTA       1  1
136400000000     O                       TES(1)              66
136500000000     O                       TES(1)             132
136600000000     O                       TES(1)             198
136700000000     O*------------------------
136800000000     O          E            TESTA          1
136900080318     O                                           10 '! TER.PAR:'
137000000000     O                       DESFIL            +  1
137100991025     O                                         +  1 '   AREA:'
137200991025     O                       DESARE            +  1
137300991025     O                                         +  1 '   DISTRETTO:'
137400000000     O                       DESDIV            +  1
137500991229     O                                         +  1 '   SCOST. (A) DA: '
137600991026     O                       PARRG1        N   +  1
137700000000     O                                         +  1 '%'
137800991025     O                                         +  1 '   A:'
137900991026     O                       PARRG2        N   +  1
138000000000     O                                         +  1 '%'
138100991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
138200991026     O                       PARPEI        N   +  1
138300000000     O                                         +  1 '%'
138400991229     O                                         +  1 ' SENZA IMP'
138500991026     O                       PARPES        N   +  1
138600991025     O                                         +  1 '%'
138700000000     O                                          198 '!'
138800991026     O*-------------------------                                       --------*
138900991102     O          E            TESTA          1
139000991102     O                                            1 '!'
139100991102     O                       CHKVDL            + 10
139200991229     O                                          115 'MIN.   (A)   :±'
139300991229     O                       PARMIN        N   +  0
139400991229     O                                         +  1 '%'
139500991102     O                                          198 '!'
139600991102     O*------------------------
139700991102     O          E            TESTA
139800991102     O                       TES(1)              66
139900991102     O                       TES(1)             132
140000991102     O                       TES(1)             198
140100991026     O*------------------------
140200080318     O*         E            TESTA          1
140300080318     O*                      TES(18)             66
140400080318     O*                      TES(19)            132
140500080318     O*                      TES(20)            198
140600991026     O*------------------------
140700080318     O*         E            TESTA          1
140800080318     O*                      TES(1)             132
140900080318     O*                      TES(1)             198
141000000000     O*------------------------
141100000000     O          E            TESTA          1
141200000000     O                       TES(2)              66
141300000000     O                       TES(3)             132
141400000000     O                       TES(4)             198
141500000000     O*------------------------
141600000000     O          E            TESTA          1
141700000000     O                       TES(5)              66
141800000000     O               01                           9 'CLIENTE'
141900000000     O               01                          50 'TARIFFA'
142000080110     o               01                          63 'TAP'
142100000000     O                       TES(6)             132
142200000000     O                       TES(7)             198
142300000000     O*------------------------
142400000000     O          E            TESTA          1
142500000000     O                       TES(1)              66
142600000000     O                       TES(1)             132
142700000000     O                       TES(1)             198
142800090708     O          E            rigcom         0
142900090708     O                                        +   3 'Commerciale unificante'
143000090708     o                       depcun              34
143100090708     o                       descun              60
143200090708     O          E            rigcom         1
143300090708     o                       depcun              34
143400090708     o                       descun              60
143500090708     O          E            rigcom         1
143600090708     O                       TES(1)              66
143700090708     O                       TES(1)             132
143800090708     O                       TES(1)             198
143900000000     O*------------------------------------------------------------------------*
144000000000     O          E            RIGCLI         1
144100000000     O                                            1 '!'
144200000000     O                       DEPKSC        Z   +  1
144300000000     O                       DESCLI            +  1
144400040308     O                       DEPCTR            +  1
144500000000     O                       DESCTR            +  1
144600080110     o                       PTAP              +  1
144700080110     O                                         +  4 '!'
144800000516     O                       STNSP         Z     75
144900000516     O                       STPER         O     85
145000000516     O                       STPSB         4     98
145100000516     O                       STPSC         4    110
145200000516     O                       STRNG         O    119
145300040220     O*---                   STTTP         O    130
145400080318     O****                                      133 '!!'
145500080318     O***                    SPNSP         Z    141
145600080318     O***                    SPPER         O    151
145700080318     O***                    SPPSB         4    164
145800080318     O***                    SPPSC         4    176
145900080318     O***                    SPRNG         O    185
146000991026     O                                          198 '!'
146100991026     O*------------------------------------------------------------------------*
146200991026     O*       E  1             RIGNUL
146300991026     O*                                   1 '!'
146400991026     O*                                  67 '!'
146500991026     O*                                 132 '!'
146600991026     O*                                 133 '!'
146700991026     O*                                 198 '!'
146800000000     O*------------------------------------------------------------------------*
146900991026     O          E            RIGTOT         1
147000991026     O                                            1 '!'
147100090708     O              n21      STDES1            +  1
147200090708     O              n21      STDES2            +  1
147300090708     O              n21                        + 21 '!'
147400090708     O               21      STDES3              62
147500090708     O               21                          67 '!'
147600991026     O                       STNSP         Z     75
147700000516     O                       STPER         O     85
147800000516     O                       STPSB         4     98
147900000516     O                       STPSC         4    110
148000000516     O                       STRNG         O    119
148100040220     O*---                   STTTP         O    130
148200080318     O****                                      133 '!!'
148300080318     O****                   SPNSP         Z    141
148400080318     O****                   SPPER         O    151
148500080318     O****                   SPPSB         4    164
148600080318     O****                   SPPSC         4    176
148700080318     O****                   SPRNG         O    185
148800991026     O                                          198 '!'
148900991026     O*------------------------------------------------------------------------*
149000000000     O          E            RIGSEP         1
149100000000     O                       TES(1)              66
149200000000     O                       TES(1)             132
149300000000     O                       TES(1)             198
149400000000     O*------------------------
149500000000     O          E            STAFIN           63
149600000000     O                                          110 '*** FINE STAMPA ***'
149700000000     O*------------------------------------------------------------------------*
149800000000** -TES-   (DESCRIZIONI  STAMPA)
149900000000------------------------------------------------------------------  1
150000000000!                                                                   2
150100080318!   NUM.   (B) %RAPP.     PESO        PESO      (A) %               3
150200080318                                                                 !  4
150300040308!                                                                   5
150400080318!   SPED.    COLLI        BOLL.       VDL       SCOST.              6
150500080318                                                                 !  7
150600000000                                                                    8
150700000000                                                                    9
150800080318TOTALE TER.PART                                                    10
150900000000TOTALE AREA                                                        11
151000000000TOTALE DISTRETTO                                                   12
151100000000TOTALE GENERALE                                                    13
151200090708PROG.  FILIALE                                                     14
151300991026PROG.  AREA                                                        15
151400991026PROG.  DISTRETTO                                                   16
151500991026PROG.  GENERALE                                                    17
151600991026!                                                                  18
151700080318!                                                                  19
151800080318                                                                 ! 20
151900090708TOTALE COMM.LE                                                     21
152000090708. . .  Di cui tariffe con tp.applicazione "MAI VDL" negativi       22
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
