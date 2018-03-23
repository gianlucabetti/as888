000100000000     H*------------------------------------------------------------------------*
000200000000     H* STAMPA STATISTICA PESI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800090708     FWFPESF1L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100080110     ftntam01l  if   e           k disk
001200000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300000000     F                                     FORMLEN(66)
001400000000     F                                     FORMOFL(60)
001500000000     L*------------------------------------------------------------------------*
001600080110     d ptap            s              1
001601090708     d stdes3          s             60
001602090708     d ii              s              2  0
001700080318     d dataiso         s               d   datfmt(*iso)
001800080318     d dataeur         s               d   datfmt(*eur)
001900080318     d PARDAL_E        s                   LIKE(PARDAL)
002000080318     d PARal_e         s                   LIKE(PARAL)
002001090708     d orddes          s             18
002002090708     d decflp          s             20
002003090708     d w_primav        s               n
002004090710     d rotcom          s               n
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400000000     D*-------------------
002500000000     D* INTESTAZIONI STAMPA
002600000000     D*-------------------
002700090708     D TES             S             66    DIM(22) CTDATA PERRCD(1)
002800000000     D INT             S              9    DIM(5) CTDATA PERRCD(1)
002900000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)
003000000000     D*-------------------
003100000000     D* SCHIERA FILIALI
003200000000     D*-------------------
003300000000     D FIL             S              3  0 DIM(500)
003400000000     D DFIL            S             20    DIM(500)
003500020627     D PFIL            S              3    DIM(500)
003501090708      *
003502090708     d CTR             S              1    dim(10)
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900991025     D*-------------------
004000991025     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
004100991025     D*-------------------
004200991025     D OG150         E DS
004300000706     D*-------------------
004400000706     D* LETTURA CAMPO ORGDE£ DI AZORG
004500000706     D*-------------------
004600000706     D OG143         E DS
004601090708     D*-------------------
004602090708     d ds01          e ds
004700000000     D*-------------------
004800000000     D* ARCHITETTURA
004900000000     D*-------------------
005000000000     D KPJBA         E DS
005100000000     D*------------------
005200000000     D* DS REPERIMENTO DATI UTENTE
005300000000     D*-------------------
005400000000     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005500000000     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005600000000     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005700000000     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005800000000     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005801090512
005802090708     d wfpesds       e ds                  extname(wfpesf0f) inz
005900080110
006000080110     d dsta01        e ds
006001090708     d dstr          e ds
006100080110
006200991025     D*-------------------
006300991025     D* DS PER SCOMPOSIZIONE CLIENTI
006400991025     D*-------------------
006500991025     D                 DS                  INZ
006600991025     D  KORG                   1      3  0
006700991025     D  CODCLI                 1      7  0
006800020910     D*------------------
006900020910     D* DS REPEIMENTO TERMINALs
007000020910     D*------------------
007100020910     D FNLV55DS      E DS
007200020910     D*------------------
007300020910     D* DS CONTROLLO DATA (8)
007400020910     D*------------------
007500020910     D WLBDA8          DS                  INZ
007600020910     D  G08DAT                 1      8  0
007700020910     D  G08INV                 9     16  0
007800020910     D  G08ERR                17     17
007900020910     D  G08TGI                18     22  0
008000000000     D*-------------------
008100000000     D* DS PARAMETRI DI LANCIO PGM
008200000000     D*-------------------
008300000000     D PARAM           DS
008400000000     D  PARTLA                 1      1
008500080318     D**PARANN                 2      5  0
008600080318     D**PARMMI                 6      7  0
008700080318     D**PARMMF                 8      9  0
008800080318     d  pardal                 2      9  0
008900080318     D**PARSNR                10     10
009000080318     D**PARDTI                11     18  0
009100080318     D**PARDTF                19     26  0
009200991025     D  PARPEI                27     31  2
009300080318     D**PARDIV                32     32
009400080318     D**PARARE                33     35  0
009500080318     D**PARFIL                36     38  0
009600080318     D  PARtfp                36     38  0
009700000000     D  PARDET                39     39
009800080318     D**PARTOT                40     40
009900000000     D  PARRG1                41     44  1
010000000000     D  PARRG2                45     48  1
010100000000     D  PARTAR                49     53  3
010200991025     D  PARPES                54     58  2
010300991229     D  PARMIN                59     63  3
010400080318     D**PARPOS                64     64
010500080318     D**PARSAL                66     66
010600080318     d  paral                 67     74  0
010601090512     d  parord                75     75
010602090708     d  parpag                76     76
010603090708     d  parflp                77     79  0
010700000000     C*------------------------------------------------------------------------*
010800000000     C* MAIN LINE
010900000000     C*------------------------------------------------------------------------*
011000000000     C*
011001090708     c* In base all'ordinamento scelto (1-comm.le unif.; 2-ragione sociale)
011002090512     c* eseguo sql o utilizzo v.l.
011003090512     c                   if        parord='1'
011004090708     c                   exsr      sr_ordcomset
011005090708     c                   exsr      sr_ordcomlet
011006090708     c                   exsr      sr_elabcom
011007090512     c                   else
011100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011200000000     C                   EXSR      SETFIL
011300000000     C*
011400000000     C* CICLO FINO A FINE FILE DI LAVORO
011500000000DO  1C     FINL00        DOWEQ     '0'
011600000000     C*
011700000000     C* OPERAZIONI PER NUOVA DIVISIONE
011800000000     C                   EXSR      NEWDIV
011900000000     C*
012000000000     C* CICLO FINO A ROTTURA DI DIVISIONE
012100000000DO  2C     FINL00        DOWEQ     '0'
012200000000     C     WFPDIV        ANDEQ     DEPDIV
012300000000     C*
012400000000     C* OPERAZIONI PER NUOVA AREA
012500000000     C                   EXSR      NEWARE
012600000000     C*
012700000000     C* CICLO FINO A ROTTURA DI AREA
012800000000DO  3C     FINL00        DOWEQ     '0'
012900000000     C     WFPDIV        ANDEQ     DEPDIV
013000000000     C     WFPARE        ANDEQ     DEPARE
013100000000     C*
013200000000     C* OPERAZIONI PER NUOVA FILIALE
013300000000     C                   EXSR      NEWFIL
013400000000     C*
013500000000     C* CICLO FINO A ROTTURA DI FILIALE
013600000000DO  4C     FINL00        DOWEQ     '0'
013700000000     C     WFPDIV        ANDEQ     DEPDIV
013800000000     C     WFPARE        ANDEQ     DEPARE
013900000000     C     WFPFIL        ANDEQ     DEPFIL
014000000000     C*
014100000000     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
014200991026     C                   EXSR      NEWPRO
014300991026     C*
014400991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
014500991026DO  5C     FINL00        DOWEQ     '0'
014600991026     C     WFPDIV        ANDEQ     DEPDIV
014700991026     C     WFPARE        ANDEQ     DEPARE
014800991026     C     WFPFIL        ANDEQ     DEPFIL
014900991026     C     WFPKSC        ANDEQ     DEPKSC
015000991026     C     WFPCTR        ANDEQ     DEPCTR
015100991026     C*
015200991026     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
015300991026     C                   EXSR      NEWCLI
015400000000     C*
015500991026     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
015600991026DO  6C     FINL00        DOWEQ     '0'
015700000000     C     WFPDIV        ANDEQ     DEPDIV
015800000000     C     WFPARE        ANDEQ     DEPARE
015900000000     C     WFPFIL        ANDEQ     DEPFIL
016000000000     C     WFPKSC        ANDEQ     DEPKSC
016100991026     C     WFPCTR        ANDEQ     DEPCTR
016200991026     C     WFPMES        ANDEQ     DEPMES
016300000000     C*
016400000000     C* MEMORIZZA LE SPEDIZIONI
016500000517     C     WRECOK        IFEQ      'S'
016600000000     C                   EXSR      MEMSPE
016700000517     C                   ENDIF
016800000000     C*
016900000000     C* LEGGE IL RECORD SUCCESSIVO
017000000000     C                   EXSR      LETFIL
017100000000     C*
017200991026E   6C                   ENDDO
017300050125     C*
017400050125     C* PROGRESSIVO: STAMPA IL TOTALE CLIENTE/TARIFFA AL MESE (Spostato qui)
017500050125     C                   EXSR      STACLI
017600000000     C*
017700991026E   5C                   ENDDO
017800991026     C*
018100991026E   4C                   ENDDO
018200000000     C*
018300000000     C* STAMPA IL TOTALE FILIALE
018400000000     C                   EXSR      STAFIL
018500000000     C*
018600000000E   3C                   ENDDO
019200000000     C*
019300000000E   2C                   ENDDO
019900000000     C*
020000000000E   1C                   ENDDO
020100000000     C*
020600000000     C*
020700000000     C* STAMPA RIGA FINE STAMPA
020800000000     C                   EXCEPT    STAFIN
020900000000     C*
021000000000     C* OPERAZIONI FINALI
021100000000     C                   EXSR      FINSR
021101090708     c                   endif
021102090512      /free
021103090512       if parord='1';
021104090513          exec sql close WPEScsr;
021105090512       endif;
021106090512      /end-free
021200000000     C*
021300000000     C                   SETON                                        LR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
021600000000     C*------------------------------------------------------------------------*
021700000000     C     SETFIL        BEGSR
021800000000     C*
021900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
022000080318     C                   clear                   KWPANN
022100000000     C                   MOVEL     *BLANKS       KWPDIV
022200000000     C                   Z-ADD     *ZEROS        KWPARE
022300000000     C                   Z-ADD     *ZEROS        KWPFIL
022400000000     C                   Z-ADD     *ZEROS        KWPKSC
022500000000     C                   Z-ADD     *ZEROS        KWPCTR
022600000000     C                   Z-ADD     *ZEROS        KWPMES
022700090708     C**   KEYWFP        SETLL     WFPESF1L                           99
022800090708     c     *loval        setll     wfpesf1l                           99
022900000000     C*
023000000000     C     *IN99         IFEQ      '1'
023100000000     C                   MOVEL     '1'           FINL00
023200000000     C                   ELSE
023300000000     C                   EXSR      LETFIL
023400000000     C                   END
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C* LETFIL - LETTURA PROSSIMO RECORD
023900000000     C*------------------------------------------------------------------------*
024000000000     C     LETFIL        BEGSR
024100000000     C*
024200000000     C* LEGGO FINO A:
024300000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
024400000000     C*     - TROVATO RECORD VALIDO
024500000000     C                   MOVEL     'N'           WRECOK
024600040213     C                   MOVEL     'N'           WRECOKC
024700000000     C*
024800000000     C* LETTURE SUCCESSIVE ALLA PRIMA
024900090708     C                   READ      WFPESf1L                               99
025000000000     C*
025100000000     C     *IN99         IFEQ      '1'
025200080318     C***  WFPANN        ORNE      PARANN
025300000000     C                   MOVEL     '1'           FINL00
025400000000     C                   ELSE
025500000000     C                   EXSR      CHKREC
025600000517     C                   ENDIF
025700000000     C*
025800000000     C                   ENDSR
025900000000     C*------------------------------------------------------------------------*
026000000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
026100000000     C*------------------------------------------------------------------------*
026200991025     C     CHKREC        BEGSR
026300000000     C*
026400000000     C                   MOVEL     'S'           WRECOK            1
026500040213     C                   MOVEL     'S'           WRECOKC           1
026600000000     C*
026700000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
026800080318     C**   WFPMES        IFGE      PARMMI
026900080318     C**   WFPMES        ANDLE     PARMMF
027000080318     C**                 ELSE
027100080318     C**                 MOVEL     'N'           WRECOK
027200080318     C**                 END
027300000706     C*
027400000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
027500080318IF  1C*    WRECOK        IFEQ      'S'
027600080318     C*                  Z-ADD     1             I
027700080318     C*    WFPFIL        LOOKUP    FIL(I)                                 95
027800080318IF  2C*    PARPOS        IFEQ      'N'
027900080318IF  3C*    PFIL(I)       IFEQ      'PPT'
028000080318     C*                  MOVEL     'N'           WRECOK
028100080318E   3C*                  ENDIF
028200080318X   2C*                  ELSE
028300080318IF  3C*    PFIL(I)       IFNE      'PPT'
028400080318     C*                  MOVEL     'N'           WRECOK
028500080318E   3C*                  ENDIF
028600080318E   2C*                  ENDIF
028700080318E   1C*                  ENDIF
028800000000     C*
028900000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
029000080318     C**   PARDIV        IFNE      *BLANKS
029100080318     C**   WFPDIV        IFNE      PARDIV
029200080318     C**                 MOVEL     'N'           WRECOK
029300080318     C**                 END
029400080318     C**                 END
029500000000     C*
029600000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
029700080318     C**   PARARE        IFNE      *ZEROS
029800080318     C**   WFPARE        IFNE      PARARE
029900080318     C**                 MOVEL     'N'           WRECOK
030000080318     C**                 END
030100080318     C**                 END
030200000000     C*
030300000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
030400080318     C**   PARFIL        IFNE      *ZEROS
030500080318     C**   WFPFIL        IFNE      PARFIL
030600080318     C**                 MOVEL     'N'           WRECOK
030700080318     C**                 END
030800080318     C**                 END
030900000000     C*
031000000000     C* CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLLETTATI
031100991025     C     WFPNCV        IFGT      *ZEROS
031200991027     C                   Z-ADD     WFPTCL        WCPNCL           12 1
031300991027     C                   Z-ADD     WFPNCV        WCPNCV           12 1
031400000000     C                   EXSR      CALPER
031500000000     C                   Z-ADD     WCPPVA        WPER              6 2
031600991025     C                   ELSE
031700991025     C                   MOVEL     'N'           WRECOK
031800991025     C                   END
031900000000     C*
032000000000     C* CALCOLA PERCENTUALE PESO VDL SU PESO BOLLETTATO
032100991025     C     WFPPSC        IFGT      *ZEROS
032200991027     C                   Z-ADD     WFPPSC        WCPPSC           13 1
032300991027     C                   Z-ADD     WFPPSB        WCPPSB           13 1
032400000000     C                   EXSR      CALRNG
032500000000     C                   Z-ADD     WCPRVA        WRNG              6 2
032600991025     C                   ELSE
032700040213     C                   MOVEL     'N'           WRECOK
032800991025     C                   END
032900000000     C*
033000991229     C* SE LA PERCENTUALE (B) E' INFERIORE ALLA MINIMA RICHIESTA,SCARTO
033100040309     C     WCPPVA        IFLT      PARXXX
033200040309     C     WCPPVA        OREQ      *ZEROS
033300040309     C                   MOVEL     'N'           WRECOKC
033400040309     C                   END
033500000000     C*
033600000517     C* SE LA PERCENTUALE (A) NON E' COMPRESA NEL RANGE INDICATO,SCARTO
033700991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
033800991229     C* DELLO SCARTO MINIMO (PARMIN)
033900040309     C                   Z-ADD     *zeros        PERC              6 2
034000040309     C                   EVAL      PERC=%abs(WCPRVA)
034100040309     C     WCPRVA        IFLT      PARRG1
034200040309     C     WCPRVA        ORGT      PARRG2
034300040309     C     WCPRVA        OREQ      *ZEROS
034400040309     C     PERC          ORLT      PARMIN
034500040309     C                   MOVEL     'N'           WRECOKC
034600040309     C                   END
034700000000     C*
034800000000     C                   ENDSR
034900000000     C*------------------------------------------------------------------------*
035000000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
035100000000     C*------------------------------------------------------------------------*
035200000000     C     MEMSPE        BEGSR
035300040303     c*
035400040303     C* TOTALE CLIENTE/TARIFFA
035500040303     C                   Z-ADD     0             TCNCV
035600040308     C                   Z-ADD     0             TCNCL
035700040308     C                   Z-ADD     0             TCNSP
035800040303     C                   Z-ADD     0             TCPSB
035900040303     C                   Z-ADD     0             TCPSC
035901090709     c*
036400991028     C* TOTALE CLIENTE/TARIFFA
036500040213IF  2C     WRECOKC       IFEQ      'S'
036501090708     c     ptap          oreq      'M'
036502090708     c     wfppsc        andgt     wfppsb
036600991027     C                   ADD       WFPNCV        TCNCV
036700991027     C                   ADD       WFPTCL        TCNCL
036800991027     C                   ADD       WFPNSP        TCNSP
036900991027     C                   ADD       WFPPSB        TCPSB
037000991027     C                   ADD       WFPPSC        TCPSC
037100040213E   2C                   ENDIF
037200040213     C                   ADD       WFPPSB        TCPSBT
037300040213     C                   ADD       WFPPSC        TCPSCT
037400991027     C* TOTALE FILIALE
037500991027     C                   ADD       WFPNCV        TFNCV
037600991027     C                   ADD       WFPTCL        TFNCL
037700991027     C                   ADD       WFPNSP        TFNSP
037800991027     C                   ADD       WFPPSB        TFPSB
037900991027     C                   ADD       WFPPSC        TFPSC
038000040213     C                   ADD       WFPPSB        TFPSBT
038100040213     C                   ADD       WFPPSC        TFPSCT
038101090708     C* TOTALE commerciale unificante
038102090708     c                   if        parord='1'
038103090708     C                   ADD       WFPNCV        TKNCV
038104090708     C                   ADD       WFPTCL        TKNCL
038105090708     C                   ADD       WFPNSP        TKNSP
038106090708     C                   ADD       WFPPSB        TkPSB
038107090708     C                   ADD       WFPPSC        TKPSC
038108090708     C                   ADD       WFPPSB        TkPSBT
038109090708     C                   ADD       WFPPSC        TkPSCT
038110090708     c                   endif
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*------------------------------------------------------------------------*
045800000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
045900000000     C*------------------------------------------------------------------------*
046000000000     C     NEWDIV        BEGSR
046100000000     C*
046200000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
046300000000     C                   MOVEL     WFPDIV        DEPDIV
046400000000     C*
046500000000     C* DECODIFICA LA DIVISIONE
046600000000     C                   EXSR      DECDIV
048600000000     C*
048700000000     C                   ENDSR
048800000000     C*------------------------------------------------------------------------*
048900000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
049000000000     C*------------------------------------------------------------------------*
049100000000     C     NEWARE        BEGSR
049200000000     C*
049300000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
049400000000     C                   Z-ADD     WFPARE        DEPARE
049500000000     C*
049600000000     C* DECODIFICA L'AREA
049700000000     C                   EXSR      DECARE
051700000000     C*
051800000000     C                   ENDSR
051900000000     C*------------------------------------------------------------------------*
052000000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
052100000000     C*------------------------------------------------------------------------*
052200000000     C     NEWFIL        BEGSR
052300000000     C*
052400000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
052500000000     C                   Z-ADD     WFPFIL        DEPFIL
052600020910     C*
052700020910     C* REPERISCE IL TERMINAL PARTENZA
052800020910     C                   CLEAR                   FNLV55DS
052900020910     C                   MOVEL     'P'           D55TPT
053000020910     C                   MOVEL     DEPFIL        D55LIN
053100020910     C                   Z-ADD     DATCOR        D55DRF
053200020910     C                   CALL      'FNLV55R'
053300020910     C                   PARM                    FNLV55DS
053400020910IF  1C     D55ERR        IFNE      *BLANKS
053500020910     C                   MOVEL     DEPFIL        D55TFP
053600020910E   1C                   ENDIF
053700020910     C                   Z-ADD     D55TFP        DEPTFP
053800991027     C*
053900020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
054000020910     C     DEPTFP        CHAIN     AZORG01L                           88
054100040301     C     *IN88         IFEQ      *OFF
054200040301     C                   MOVEL     ORGDF0        OG150
054300120914     C     §OGPES        IFNE      *BLANKS
054400120914     C     §OGpes        andNE     *all'0'
054500120914     c***  §ogspp        ifeq      'S'
054600040301     C                   MOVEL     DEC(3)        CHKVDL           20
054700991027     C                   Z-ADD     PARPEI        PARXXX            5 2
054800991027     C                   ELSE
054900991027     C                   MOVEL     DEC(4)        CHKVDL           20
055000991027     C                   Z-ADD     PARPES        PARXXX            5 2
055100991027     C                   ENDIF
055200991027     C                   ELSE
055300040227     C                   MOVEL     DEC(4)        CHKVDL           20
055400040227     C                   Z-ADD     PARPES        PARXXX            5 2
055500040227     C                   ENDIF
055600000000     C*
055700000000     C* AZZERA I TOTALI DELLA FILIALE
055800000000     C                   Z-ADD     *ZEROS        TFNCV             9 0
055900000000     C                   Z-ADD     *ZEROS        TFNCL             9 0
056000040301     C                   Z-ADD     *ZEROS        TFNSP             8 0
056100000000     C                   Z-ADD     *ZEROS        TFPSB            10 1
056200000000     C                   Z-ADD     *ZEROS        TFPSC            10 1
056900040213     C*
057000000517     C                   Z-ADD     *ZEROS        TFPSBT           10 1
057100000517     C                   Z-ADD     *ZEROS        TFPSCT           10 1
057101090708     c* totali di cui ....
057102090708     C                   Z-ADD     *ZEROS        TFNCV_m           9 0
057103090708     C                   Z-ADD     *ZEROS        TFNCL_m           9 0
057104090708     C                   Z-ADD     *ZEROS        TFNSP_m           8 0
057105090708     C                   Z-ADD     *ZEROS        TFPSB_m          10 1
057106090708     C                   Z-ADD     *ZEROS        TFPSC_m          10 1
057107090708     C*
057108090708     C                   Z-ADD     *ZEROS        TFNCVP_m          9 0
057109090708     C                   Z-ADD     *ZEROS        TFNCLP_m          9 0
057200040213     C*
057500000000     C*
057600000000     C* DECODIFICA LA FILIALE
057700000000     C                   EXSR      DECFIL
057800000000     C*
057900000000     C* IMPOSTAZIONI CAMPI IN TESTATA
058000080318     C     PARDET        IFEQ      'S'
058100080318     C*                  MOVEL     *BLANKS       DESFIL
058200080318     C*                  MOVEL     *BLANKS       CHKVDL
058300000000     C                   ELSE
058400000000     C                   SETON                                        01
058500000000     C                   ENDIF
058600000000     C*
058700000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
058800000000     C     *INOF         IFEQ      *ON
058900000000     C     PARDET        OREQ      'D'
059000000000     C                   EXCEPT    TESTA
059100000000     C                   SETOFF                                       OF
059200000000     C                   ENDIF
059300000000     C*
059400000000     C                   ENDSR
059500000000     C*------------------------------------------------------------------------*
059600991026     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
059700000000     C*------------------------------------------------------------------------*
059800000000     C     NEWCLI        BEGSR
059900000000     C*
060000000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
060100040220     C                   MOVEL     WFPRAG        DESCLI
060200040227     C                   Z-ADD     WFPMES        DEPMES
060300000000     C*
060400040227     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
060500991026     C                   Z-ADD     *ZEROS        TCNCV             9 0
060600991026     C                   Z-ADD     *ZEROS        TCNCL             9 0
060700991026     C                   Z-ADD     *ZEROS        TCNSP             8 0
060800040301     C                   Z-ADD     *ZEROS        TCPSB            10 1
060900000517     C                   Z-ADD     *ZEROS        TCPSC            10 1
061000040213     C*
061100000517     C                   Z-ADD     *ZEROS        TCPSBT           10 1
061200000517     C                   Z-ADD     *ZEROS        TCPSCT           10 1
061300991026     C*
061400000000     C                   ENDSR
061500991026     C*------------------------------------------------------------------------*
061600991026     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
061700991026     C*------------------------------------------------------------------------*
061800991026     C     NEWPRO        BEGSR
061900991026     C*
062000991026     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
062100991026     C                   Z-ADD     WFPKSC        DEPKSC
062200991026     C                   Z-ADD     WFPCTR        DEPCTR
062201090708     c*
062202090708     c                   clear                   ptap
062203090708     c                   movel     depctr        w_tr              1
062204090708     c                   if        %lookup(w_tr:CTR)>0
062205090708     c                   exsr      rectap
062206090708     c                   endif
063300991026     C*
063400991026     C                   ENDSR
063401090708     C*------------------------------------------------------------------------*
063402090708     C* NEWCOM - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
063403090708     C*------------------------------------------------------------------------*
063404090708     C     NEWCOM        BEGSR
063405090708     C*
063414090710     c                   eval      rotcom=*on
063415090709     C* MEMORIZZA LA NUOVA AREA
063416090708     C                   Z-ADD     WFPcun        DEPcun
063417090708     C*
063418090708     C* DECODIFICA il commerciale unificante
063419090708     C                   EXSR      DECcun
063420090708     c*
063421090708     C* AZZERA I TOTALI DEL commerciale unificante
063422090708     C                   Z-ADD     *ZEROS        TKNCV             9 0
063423090708     C                   Z-ADD     *ZEROS        TKNCL             9 0
063424090708     C                   Z-ADD     *ZEROS        TkNSP             8 0
063425090708     C                   Z-ADD     *ZEROS        TkPSB            10 1
063426090708     C                   Z-ADD     *ZEROS        TKPSC            10 1
063427090708     C*
063428090708     C                   Z-ADD     *ZEROS        TKPSBT           10 1
063429090708     C                   Z-ADD     *ZEROS        TKPSCT           10 1
063430090709     c*
063431090709     C                   Z-ADD     *ZEROS        TKNCV_m
063432090709     C                   Z-ADD     *ZEROS        TKNCL_m
063433090709     C                   Z-ADD     *ZEROS        TkNSP_m
063434090709     C                   Z-ADD     *ZEROS        TkPSB_m
063435090709     C                   Z-ADD     *ZEROS        TKPSC_m
063436090709     C*
063437090709     C                   Z-ADD     *ZEROS        TKPSBT_m
063438090709     C                   Z-ADD     *ZEROS        TKPSCT_m
063439090708      *
063450090708     C*
063451090708     C                   ENDSR
063500000000     C*------------------------------------------------------------------------*
063600991028     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
063700000000     C*------------------------------------------------------------------------*
063800000000     C     STACLI        BEGSR
063900000000     C*
064000000000     C                   MOVEL     '0'           $STDAC
064100000000     C*
064200000000     C* DECODIFICA IL CLIENTE
064300000000     C                   EXSR      DECCLI
064400991026     C*
064500991026     C*------------------------------
064600991026     C*OPERAZIONI PER DETTAGLIO VDL
064700991026     C*------------------------------
064800000000     C* DECODIFICA LA TARIFFA
064900000000     C                   MOVEL     *BLANKS       DESCTR
065000000000     C                   EXSR      DECCTR
065100080110      * recupero tipo addebito peso dalla tariffa
065200090708     c****               clear                   ptap
065300090708     c****               exsr      rectap
065400000000     C*
065500000000     C                   Z-ADD     TCNCV         STNCV             9 0
065600040227     C                   Z-ADD     TCNCL         STNCL             9 0
065700040227     C                   Z-ADD     TCNSP         STNSP             8 0
065800040227     C                   Z-ADD     TCPSB         STPSB            10 1
065900040227     C                   Z-ADD     TCPSC         STPSC            10 1
066000000000     C*
066100000000     C* CALCOLA PERCENTUALE COLLI VDL
066200040301     C                   Z-ADD     TCNCL         WCPNCL           12 1
066300040301     C                   Z-ADD     TCNCV         WCPNCV           12 1
066400040301     C                   EXSR      CALPER
066500040301     C                   Z-ADD     WCPPVA        WPER              6 2
066600040301     C                   Z-ADD     WPER          STPER             6 1
066700000000     C*
066800000000     C* CALCOLA PERCENTUALE PESO VDL
066900991027     C                   Z-ADD     TCPSC         WCPPSC           13 1
067000991027     C                   Z-ADD     TCPSB         WCPPSB           13 1
067100000000     C                   EXSR      CALRNG
067200000000     C                   Z-ADD     WCPRVA        WRNG              6 2
067300000516     C                   Z-ADD     WRNG          STRNG             6 1
067400000516     C*
067500000516     C* CALCOLA PERCENTUALE PESO VDL TOTALI
067600000516     C                   Z-ADD     TCPSCT        WCPPSC           13 1
067700000516     C                   Z-ADD     TCPSBT        WCPPSB           13 1
067800000516     C                   EXSR      CALRNG
067900000516     C                   Z-ADD     WCPRVA        WRNG              6 2
068000000516     C                   Z-ADD     WRNG          STTTP             6 1
071000991026     C*
071200991028     C*
071500080318     C     STNSP         ifgt      *ZEROS
071501090710     c*
071502090710     c* prima di stampare la riga del cliente/tariffa, se c'è stata
071503090710     c* rottura di commerciale devo prima stampare il nuovo commerciale
071504090710     c                   if        parord='1'  and rotcom=*on
071505090710     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
071506090710     C     *INOF         IFEQ      *ON
071507090710     C     PARpag        OREQ      'S'
071508090710     c     depcun_p      andgt     0
071509090710     c                   seton                                        01
071510090710     C                   EXCEPT    TESTA
071511090710     C                   SETOFF                                       OF
071512090710     C                   ENDIF
071513090710     c                   eval      depcun_p=depcun
071514090710     c* stampo la riga del commerciale unificante
071515090710     c                   if        pardet='D'
071516090710     c                   except    rigcom
071517090710     c                   endif
071518090710     c                   eval      rotcom=*off
071519090710     c                   endif
071520090710     c*
071600991026     C                   EXSR      STRCLI
071700991026     C                   MOVEL     '1'           $STDAC
071701090708     c* Incremento totali "Di cui tariffe con tipo applicazione "M"=MAI VDL"
071702090708     c* Totali di FILIALE
071703090708     c                   if        ptap='M' and tcpsc>tcpsb
071704090708     C                   ADD       tcncv         TFNCV_m
071705090708     C                   ADD       tcncl         TFNCL_m
071706090708     C                   ADD       tcnsp         TFNSP_m
071707090708     C                   ADD       tcpsb         TFPSB_m
071708090708     C                   ADD       tcpsc         TFPSC_m
071727090708     C* Totale commerciale unificante
071728090708     C                   ADD       tcncv         TkNCV_m           9 0
071729090708     C                   ADD       tcncl         TkNCL_m           9 0
071730090708     C                   ADD       tcnsp         TkNSP_m           8 0
071731090708     C                   ADD       tcpsb         TkPSB_m          10 1
071732090708     C                   ADD       tcpsc         TkPSC_m          10 1
071733090708     C                   ADD       tcpsb         TkPSBT_m         10 1
071734090708     C                   ADD       tcpsc         TkPSCT_m         10 1
071768090708     c                   endif
071800991028     C                   ENDIF
071900000000     C*
072000000000     C                   ENDSR
072100000000     C*------------------------------------------------------------------------*
072200000000     C* STAFIL - STAMPA TOTALI FILIALE
072300000000     C*------------------------------------------------------------------------*
072400000000     C     STAFIL        BEGSR
072500040213     C*
072501090708     c                   setoff                                       21
072600000000     C                   MOVEL     TES(10)       STDES1           22
072700000000     C*
072800000000     C* DECODIFICA LA FILIALE
072900080318     C*****              EXSR      DECFIL
073000080318     C                   Z-ADD     1             I
073100080318     C     partfp        LOOKUP    FIL(I)                                 95
073200080318     C     *IN95         IFEQ      '0'
073300080318     C                   MOVEL     *ALL'*'       DESFIL           20
073400080318     C                   ELSE
073500080318     C                   MOVEL     DFIL(I)       DESFIL
073600080318     C                   END
073700000000     C                   MOVEL     DESFIL        STDES2           20
073800000000     C*
073900000000     C* IMPOSTO I VALORI DI STAMPA
074000000000     C                   Z-ADD     TFNCV         STNCV
074100991027     C                   Z-ADD     TFNCL         STNCL
074200000000     C                   Z-ADD     TFNSP         STNSP
074300000000     C                   Z-ADD     TFPSB         STPSB
074400000000     C                   Z-ADD     TFPSC         STPSC
075100000516     C*
075200000516     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
075300000516     C                   Z-ADD     TFPSCT        WCPPSC           13 1
075400040308     C                   Z-ADD     TFPSBT        WCPPSB           13 1
075500040308     C                   EXSR      CALRNG
075600000516     C                   Z-ADD     WCPRVA        STTTP
075700000516     C*
075701090708     c* se stampa per commerciale questo totale lo devo stampare a pagina
075702090708     c* nuova
075703090708     c                   if        parord='1'
075704090708     c                   setoff                                       01
075705090708     c                   except    testa
075706090708     c                   endif
076300991026     C*
076400000000     C* SE DETTAGLIO
076500000000     C     PARDET        IFEQ      'D'
076600000000     C                   EXCEPT    RIGSEP
076700991027     C                   ENDIF
076800000000     C*
076900000000     C* STAMPO RIGA TOTALI
077300000000     C                   EXSR      STRTOT
077301090708     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
077302090708     c                   if        tfnsp_m>0
077303090708     C* IMPOSTO I VALORI DI STAMPA
077304090708     C                   Z-ADD     TFNCV_m       STNCV
077305090708     C                   Z-ADD     TFNCL_m       STNCL
077306090708     C                   Z-ADD     TFNSP_m       STNSP
077307090708     C                   Z-ADD     TFPSB_m       STPSB
077308090708     C                   Z-ADD     TFPSC_m       STPSC
077315090708     C*
077316090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
077321090708     C*
077327090708     c                   eval      stdes3=tes(22)
077328090708     c                   seton                                        21
077329090708     c                   exsr      strtot
077330090708     c                   endif
077400000000     C*
077500000000     C* SE DETTAGLIO
077600040301     C     PARDET        IFEQ      'D'
077700000000     C                   EXCEPT    RIGSEP
077800000000     C                   ENDIF
077900000000     C*
078000000000     C                   ENDSR
078001090708     C*------------------------------------------------------------------------*
078002090708     C* STAcom - STAMPA TOTALI commerciale unificante
078003090708     C*------------------------------------------------------------------------*
078004090708     C     STAcom        BEGSR
078005090708     C*
078006090710     C* SE NON CI SONO pesi, NON STAMPO
078007090710IF  1c     TkpsB         IFEQ      *ZEROS
078008090710     C     TkpsC         ANDEQ     *ZEROS
078009090710X   1C                   ELSE
078010090709     c                   setoff                                       21
078011090708     C                   MOVEL     TES(21)       STDES1           22
078012090708     c                   movel     descun        stdes2
078018090708     C*
078019090708     C* IMPOSTO I VALORI DI STAMPA
078020090708     C                   Z-ADD     TkNCV         STNCV
078021090708     C                   Z-ADD     TkNCL         STNCL
078022090708     C                   Z-ADD     TkNSP         STNSP
078023090708     C                   Z-ADD     TkPSB         STPSB
078024090708     C                   Z-ADD     TkPSC         STPSC
078025090708     C*
078026090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
078027090708     C                   Z-ADD     TkPSCT        WCPPSC           13 1
078028090708     C                   Z-ADD     TkPSBT        WCPPSB           13 1
078029090708     C                   EXSR      CALRNG
078030090708     C                   Z-ADD     WCPRVA        STTTP
078031090708     C*
078037090708     C*
078038090708     C* SE DETTAGLIO
078039090708     C     PARDET        IFEQ      'D'
078040090708     C                   EXCEPT    RIGSEP
078041090708     C                   ENDIF
078042090708     C*
078043090708     C* STAMPO RIGA TOTALI
078047090708     C                   EXSR      STRTOT
078048090708     c* . . . .totale "di cui con tipo applicazione MAI VDL negativi"
078049090708     c                   if        tknsp_m>0
078050090708     C* IMPOSTO I VALORI DI STAMPA
078051090708     C                   Z-ADD     TkNCV_m       STNCV
078052090708     C                   Z-ADD     TkNCL_m       STNCL
078053090708     C                   Z-ADD     TkNSP_m       STNSP
078054090708     C                   Z-ADD     TkPSB_m       STPSB
078055090708     C                   Z-ADD     TkPSC_m       STPSC
078062090708     C*
078063090708     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO (TOTALI)
078064090708     C                   Z-ADD     TkPSCT_m      WCPPSC           13 1
078065090708     C                   Z-ADD     TkPSBT_m      WCPPSB           13 1
078066090708     C                   EXSR      CALRNG
078067090708     C                   Z-ADD     WCPRVA        STTTP
078068090708     C*
078074090709     c                   eval      stdes3=tes(22)
078075090708     c                   seton                                        21
078076090708     c                   exsr      strtot
078077090708     c                   endif
078078090708     C*
078079090708     C* SE DETTAGLIO
078080090708     C     PARDET        IFEQ      'D'
078081090708     C                   EXCEPT    RIGSEP
078082090708     C                   ENDIF
078083090710     c*
078084090710     c                   endif
078085090708     C*
078086090708     C                   ENDSR
093100000000     C*------------------------------------------------------------------------*
093200000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
093300000000     C*------------------------------------------------------------------------*
093400000000     C     STRCLI        BEGSR
093500040213     C*
093600991105 MOD C     PARDET        IFEQ      'D'
093700000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
093800040308     C     *INOF         IFEQ      '1'
093900040308     C                   SETON                                        01
094000040308     C                   EXCEPT    RIGSEP
094100040308     C                   EXCEPT    TESTA
094200040308     C                   SETOFF                                       OF
094300000000     C                   END
094400000000     C*
094500000000     C     $STDAC        IFEQ      '1'
094600000000     C                   Z-ADD     *ZEROS        DEPKSC
094700040220     C                   MOVEL     *BLANKS       DESCLI
094800000000     C                   Z-ADD     *ZEROS        DEPCTR
094900991026     C                   Z-ADD     *ZEROS        DEPMES
095000000000     C                   MOVEL     *BLANKS       DESCTR
095100991026     C*
095200991026     C                   ENDIF
095300000000     C*
095400000000     C* STAMPO RIGA CLIENTE/TARIFFA
095500040308     C                   EXCEPT    RIGCLI
095600040308 MOD C                   ENDIF
095700991028     C*
095800000000     C*
095900000000     C                   ENDSR
096000000000     C*------------------------------------------------------------------------*
096100000000     C* STRTOT - STAMPA RIGA TOTALE
096200000000     C*------------------------------------------------------------------------*
096300000000     C     STRTOT        BEGSR
096400991027     C*
096500991027     C*-------------------------
096600991027     C* OPERAZIONI X STAMPA DATI DEL MESE
096700991027     C*-------------------------
096800991027     C*
096900000000     C* CALCOLA PERCENTUALE SUL TOTALE PESO VDL E BOLLETTATO
097000040309     c     stpsc         ifeq      stpsb
097100040309     c                   z-add     *zeros        strng
097200040309     c                   else
097300040309     C                   Z-ADD     STPSC         WCPPSC           13 1
097400040309     C                   Z-ADD     STPSB         WCPPSB           13 1
097500040309     C                   EXSR      CALRNG
097600040309     C                   Z-ADD     WCPRVA        STRNG
097700040309     c                   endif
097800991025     C*
097900991025     C* CALCOLA PERCENTUALE COLLI VDL E BOLLETTATO
098000040309     c     stncl         ifeq      stncv
098100040309     c                   z-add     *zeros        stper
098200040309     c                   else
098300040309     C                   Z-ADD     STNCL         WCPNCL           12 1
098400040309     C                   Z-ADD     STNCV         WCPNCV           12 1
098500040309     C                   EXSR      CALPER
098600040309     C                   Z-ADD     WCPPVA        STPER
098700040309     c                   endif
101200000000     C*
101300000000     C* STAMPO RIGA CLIENTE/TARIFFA
101400000000     C                   EXCEPT    RIGTOT
101500000000     C*
101600000000     C                   ENDSR
101700000000     C*------------------------------------------------------------------------*
101800000000     C* DECDIV - DECODIFICA DIVISIONE
101900000000     C*------------------------------------------------------------------------*
102000000000     C     DECDIV        BEGSR
102100000000     C*
102200020906     C                   MOVEL     1             KTBKUT
102300000000     C                   MOVE      '17'          KTBCOD
102400000000     C                   MOVE      *BLANKS       KTBKEY
102500000000     C                   MOVEL     DEPDIV        KTBKEY
102600000000     C     KEYTAB        CHAIN     TABEL                              97
102700000000     C     *IN97         IFEQ      '1'
102800000000     C     TBLFLG        OREQ      '*'
102900000000     C                   MOVEL     *ALL'*'       DESDIV           20
103000000000     C                   ELSE
103100000000     C                   MOVEL     TBLUNI        DESDIV
103200000000     C                   END
103300000000     C*
103400000000     C                   ENDSR
103500000000     C*------------------------------------------------------------------------*
103600000000     C* DECARE - DECODIFICA AREA
103700000000     C*------------------------------------------------------------------------*
103800000000     C     DECARE        BEGSR
103900000000     C*
104000020906     C                   MOVEL     1             KTBKUT
104100000000     C                   MOVE      '05'          KTBCOD
104200000000     C                   MOVE      *BLANKS       KTBKEY
104300000000     C                   MOVEL     DEPARE        KTBKEY
104400000000     C     KEYTAB        CHAIN     TABEL                              97
104500000000     C     *IN97         IFEQ      '1'
104600000000     C     TBLFLG        OREQ      '*'
104700000000     C                   MOVEL     *ALL'*'       DESARE           20
104800000000     C                   ELSE
104900000000     C                   MOVEL     TBLUNI        DESARE
105000000000     C                   END
105100000000     C*
105200000000     C                   ENDSR
105201090708     C*------------------------------------------------------------------------*
105202090708     C* DECcun - DECODIFICA commerciale unificante
105203090708     C*------------------------------------------------------------------------*
105204090708     C     DECcun        BEGSR
105205090708     C*
105206090708     C                   MOVEL     1             KTBKUT
105207090708     C                   MOVE      '01'          KTBCOD
105208090708     C                   MOVE      *BLANKS       KTBKEY
105209090708     C                   MOVEL     DEPcun        KTBKEY
105210090708     C     KEYTAB        CHAIN     TABEL                              97
105211090708     C     *IN97         IFEQ      '1'
105212090708     C     TBLFLG        OREQ      '*'
105213090708     C                   MOVEL     *ALL'*'       DEScun           25
105214090708     C                   ELSE
105215090708     C                   MOVEL     TBLUNI        Ds01
105216090708     C                   MOVEL     §01age        descun
105217090708     C                   END
105218090708     C*
105219090708     C                   ENDSR
105300000000     C*------------------------------------------------------------------------*
105400000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
105500000000     C*------------------------------------------------------------------------*
105600000000     C     DECFIL        BEGSR
105700000000     C*
105800000000     C                   Z-ADD     1             I
105900000000     C     DEPFIL        LOOKUP    FIL(I)                                 95
106000000000     C     *IN95         IFEQ      '0'
106100000000     C                   MOVEL     *ALL'*'       DESFIL           20
106200000000     C                   ELSE
106300000000     C                   MOVEL     DFIL(I)       DESFIL
106400000000     C                   END
106500991026     C*
106600000000     C                   ENDSR
106700000000     C*------------------------------------------------------------------------*
106800000000     C* DECCLI - DECODIFICA CLIENTE
106900000000     C*------------------------------------------------------------------------*
107000000000     C     DECCLI        BEGSR
107100000000     C*
107200000000     C                   CLEAR                   BS69DS
107300000000     C                   CLEAR                   ACODS
107400000000     C                   MOVEL     KNSIF         I69SIF
107500000000     C                   Z-ADD     DEPKSC        I69KAC
107600000000     C                   CALL      'TIBS69R'
107700000000     C                   PARM                    BS69DS
107800000000     C                   PARM                    ACODS
107900000000     C                   PARM                    INDDS
108000000000     C                   PARM                    CLPDS
108100000000     C                   PARM                    CLSDS
108200000000     C     O69ERR        IFNE      '1'
108300000000     C                   MOVEL     ACORAG        DESCLI           30
108400000000     C                   ELSE
108500000000     C                   MOVEL     *ALL'*'       DESCLI           30
108600000000     C                   ENDIF
108700000000     C*
108800000000     C                   ENDSR
108900000000     C*------------------------------------------------------------------------*
109000000000     C* DECCTR - DECODIFICA TARIFFA
109100000000     C*------------------------------------------------------------------------*
109200000000     C     DECCTR        BEGSR
109300000000     C*
109400000000     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
109500000000     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
109600000000     C     DEPCTR        IFEQ      999
109700000000     C                   MOVEL     DEC(1)        DESCTR           15
109800000000     C                   ELSE
109900000000     C     DEPCTR        IFEQ      888
110000000000     C                   MOVEL     DEC(2)        DESCTR
110100000000     C                   ELSE
110200020906     C                   MOVEL     1             KTBKUT
110300000000     C                   MOVE      'TR'          KTBCOD
110400000000     C                   MOVE      *BLANKS       KTBKEY
110500000000     C                   MOVEL     DEPCTR        WTEM01            1
110600000000     C                   MOVEL     WTEM01        KTBKEY
110700000000     C     KEYTAB        CHAIN     TABEL                              97
110800000000     C     *IN97         IFEQ      '1'
110900000000     C     TBLFLG        OREQ      '*'
111000000000     C                   MOVEL     *ALL'*'       DESCTR
111100000000     C                   ELSE
111200000000     C                   MOVEL     TBLUNI        DESCTR
111300000000     C                   ENDIF
111400000000     C                   ENDIF
111500000000     C                   ENDIF
111600000000     C*
111700000000     C                   ENDSR
111800080110
111900080110      *------------------------------------------------------------------------*
112000080110      * Recupero tipo addebito peso da tariffa
112100080110      *------------------------------------------------------------------------*
112200080110     c     rectap        begsr
112300080110
112400080110     c                   clear                   dsta01
112500080110     c     ktntam        setgt     tntam01l
112600080110     c     ktntam        readpe    tntam01l
112700080110     c                   eval      dsta01 = tamflo
112800080110     c                   eval      ptap = §tatap
112900080110
113000080110     c                   endsr
113100080110
113200000000     C*------------------------------------------------------------------------*
113300000000     C* CALPER - CALCOLA PERCENTUALE COLLI
113400000000     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
113500000000     C*          - COLLI VDL               (WCPNCV)
113600000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
113700000000     C*------------------------------------------------------------------------*
113800040301     C     CALPER        BEGSR
113900000000     C*
114000000000     C* ===> CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLL.
114100000000     C                   Z-ADD     *ZEROS        WCPPVA            6 2
114200991025     C     WCPNCL        IFNE      WCPNCV
114300991025     C     WCPNCL        ANDGT     *ZEROS
114400040308     C     WCPNCV        ANDGT     *ZEROS
114500040308     C     WCPNCV        MULT      100           WMULTI           17 3
114600040301     C     WMULTI        DIV       WCPNCL        WRKPER           17 3
114700991025     C     WRKPER        IFGT      9999
114800000000     C                   Z-ADD     9999,99       WCPPVA
114900040308     C                   ELSE
115000040301     C                   Z-ADD     WRKPER        WCPPVA
115100040301     C                   END
115200040308     C                   END
115300991027     C*
115400991102     C     WCPNCL        IFEQ      WCPNCV
115500040301     C     WCPNCL        ANDGT     *ZEROS
115600991102     C     WCPNCV        ANDGT     *ZEROS
115700040308     C                   Z-ADD     100           WCPPVA
115800991102     C                   ENDIF
115900040301     C*
116000040308     C     WCPPVA        IFGT      100
116100040308     C                   Z-ADD     100           WCPPVA
116200040308     C                   ENDIF
116300991102     C*
116400040301     C                   ENDSR
116500000000     C*------------------------------------------------------------------------*
116600000000     C* CALRNG - CALCOLA PERCENTUALE RANGE
116700000000     C* INPUT:   - PESO BOLLETTAT0         (WCPPSB)
116800000000     C*          - PESO VDL                (WCPPSC)
116900000000     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPRVA)
117000000000     C*------------------------------------------------------------------------*
117100000000     C     CALRNG        BEGSR
117200000000     C*
117300000000     C* ===> CALCOLA PERCENTUALE PESO VDL SU PESO BOLL.
117400000000     C                   Z-ADD     *ZEROS        WCPRVA            6 2
117500000000     C     WCPPSC        IFNE      WCPPSB
117600040308     C     WCPPSC        ANDGT     *ZEROS
117700000000     C     WCPPSB        ANDGT     *ZEROS
117800991027     C     WCPPSB        SUB       WCPPSC        WDIFFE           13 1
117900040308     C     WDIFFE        MULT      100           WRKPER           17 3
118000991027     C     WRKPER        DIV       WCPPSB        WRKPER
118100000000     C     WRKPER        IFGT      9999
118200040308     C                   Z-ADD     9999,99       WCPRVA
118300040308     C                   ELSE
118400040308     C                   Z-ADD     WRKPER        WCPRVA
118500040308     C                   END
118600000000     C                   END
118700991102     C*
118800991102     C     WCPPSC        IFEQ      WCPPSB
118900991102     C     WCPPSC        ANDGT     *ZEROS
119000991102     C     WCPPSB        ANDGT     *ZEROS
119100040308     C                   Z-ADD     100           WCPPVA
119200991102     C                   ENDIF
119300040308     C*
119400040308     C     WCPPVA        IFGT      100
119500040308     C                   Z-ADD     100           WCPPVA
119600040308     C                   ENDIF
119700000000     C*
119800000000     C                   ENDSR
119900000000     C*------------------------------------------------------------------------*
120000000000     C* FINSR - OPERAZIONI FINALI
120100000000     C*------------------------------------------------------------------------*
120200000000     C     FINSR         BEGSR
120300000000     C*
120400000000     C* CHIUDE I FILE DEI PGM CHIAMATI
120500000000     C                   CLEAR                   BS69DS
120600000000     C                   CLEAR                   ACODS
120700000000     C                   CLEAR                   INDDS
120800000000     C                   CLEAR                   CLPDS
120900000000     C                   CLEAR                   CLSDS
121000000000     C                   MOVEL     'C'           I69TLA
121100000000     C                   MOVEL     KNSIF         I69SIF
121200000000     C                   CALL      'TIBS69R'
121300000000     C                   PARM                    BS69DS
121400000000     C                   PARM                    ACODS
121500000000     C                   PARM                    INDDS
121600000000     C                   PARM                    CLPDS
121700000000     C                   PARM                    CLSDS
121800000000     C*
121900000000     C                   ENDSR
121901090512     C*------------------------------------------------------------------------*
121902090708     C* ordinamento per commerciale unificante
121903090512     C*------------------------------------------------------------------------*
121904090708     C     sr_ordcomset  BEGSR
121905090512      /free
121909090708       exec sql declare WPEScsr cursor for Select * from wfpesf0f
121910090708        order by wfpann, wfpdiv, wfpare, wfpfil, wfpcun, wfprag, wfpksc, wfpctr,
121911090708         wfpmes;
121912090512       exec sql open WPEScsr;
121913090512      /end-free
121914090512     c                   endsr
121915090512     C*------------------------------------------------------------------------*
121916090708     C* ordinamento per commerciale unificante
121917090512     C*------------------------------------------------------------------------*
121918090708     C     sr_ordcomlet  BEGSR
121919090513      /free
121920090512       wrecok='N';
121921090512       wrecokc='N';
121922090512       exec sql Fetch WPEScsr into :wfpesds;
121923090512         if sqlcod=100 or sqlcod<0;
121924090512            finl00='1';
121925090512         else;
121926090512            exsr chkrec;
121927090512         endif;
121928090512      /end-free
121929090512     c                   endsr
121930090708     C*------------------------------------------------------------------------*
121931090708     C* elaborazione e stampa per commerciale unificante
121932090708     C*------------------------------------------------------------------------*
121933090708     C     sr_elabcom    BEGSR
121934090708     c* solo la prima volta
121935090708     c                   if        w_primav=*off
121936090708     c                   exsr      newdiv
121937090708     c                   exsr      neware
121938090708     c                   exsr      newfil
121939090708     c                   eval      w_primav=*on
121940090708     c                   endif
121941090708     c     finl00        doweq     '0'
121942090708     c* operazioni per nuovo commerciale unificante
121943090708     c                   exsr      newcom
121944090708     c     finl00        doweq     '0'
121945090708     c     wfpcun        andeq     depcun
121946090708     c* operazioni per nuovo cliente/tariffa
121947090708     c                   exsr      newcli
121948090708     C                   z-add     WFPKSC        DEPKSC
121949090708     C                   z-add     WFPCTR        DEPCTR
121950090708     c                   clear                   ptap
121951090708     c                   movel     depctr        w_tr              1
121952090708     c                   if        %lookup(w_tr:CTR)>0
121953090708     c                   exsr      rectap
121954090708     c                   endif
121955090708     c     finl00        doweq     '0'
121956090708     c     wfpcun        andeq     depcun
121957090708     c     wfpksc        andeq     depksc
121958090708     c     wfpctr        andeq     depctr
121959090708     C* Memorizza le spedizioni
121960090708     C     WRECOK        IFEQ      'S'
121961090708     C                   exsr      memspe
121962090708     C                   ENDIF
121963090708     c* leggo il record successivo
121964090708     c                   exsr      sr_ordcomlet
121965090708     c                   enddo
121966090708     c* stampa totale cliente/tariffa
121967090708     c                   exsr      stacli
121968090708     c                   enddo
121969090708     c* stampa totale commerciale unificante
121970090708     c                   exsr      stacom
121971090708     c                   enddo
121972090708     c                   exsr      stafil
121973090708     C* STAMPA RIGA FINE STAMPA
121974090708     C                   EXCEPT    stafin
121975090708     C* OPERAZIONI FINALI
121976090708     C                   exsr      finsr
121977090708     c                   endsr
122000000000     C*------------------------------------------------------------------------*
122100000000     C* *INZSR - ROUTINE INIZIALE
122200000000     C*------------------------------------------------------------------------*
122300000000     C     *INZSR        BEGSR
122400000000     C*--------------------
122500000000     C* RICEVIMENTO PARAMENTRI
122600000000     C*--------------------
122700000000     C     *ENTRY        PLIST
122800000000     C                   PARM                    KPJBA
122900000000     C                   MOVEL     KPJBU         PARAM
123000000000     C*--------------------
123100000000     C* CAMPI RIFERITI AL DATA BASE
123200000000     C*--------------------
123300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
123400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
123500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
123600000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
123700000000     C     *LIKE         DEFINE    WFPANN        KWPANN
123800000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
123900000000     C     *LIKE         DEFINE    WFPARE        KWPARE
124000000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
124100040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
124200000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
124300000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
124400000000     C     *LIKE         DEFINE    WFPMES        KWPMES
124500000000     C     *LIKE         DEFINE    WFPDIV        DEPDIV
124600000000     C     *LIKE         DEFINE    WFPARE        DEPARE
124700000000     C     *LIKE         DEFINE    WFPFIL        DEPFIL
124800020910     C     *LIKE         DEFINE    WFPFIL        DEPTFP
124900000000     C     *LIKE         DEFINE    WFPKSC        DEPKSC
125000000000     C     *LIKE         DEFINE    WFPCTR        DEPCTR
125100991026     C     *LIKE         DEFINE    WFPMES        DEPMES
125101090708     C     *LIKE         DEFINE    WFPcun        DEPcun
125102090710     C     *LIKE         DEFINE    WFPcun        DEPcun_p
125200000000     C*
125300000000     C*--------------------
125400000000     C* CHIAVI DI LETTURA
125500000000     C*--------------------
125600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
125700000000     C     KEYTAB        KLIST
125800000000     C                   KFLD                    KTBKUT
125900000000     C                   KFLD                    KTBCOD
126000000000     C                   KFLD                    KTBKEY
126001090708     c*
126002090708     C     KEYTABp       KLIST
126003090708     C                   KFLD                    KTBKUT
126004090708     C                   KFLD                    KTBCOD
126100000000     C*
126200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
126300000000     C     KEYORG        KLIST
126400000000     C                   KFLD                    KORFIL
126500000000     C*
126600090708     C* CHIAVE LETTURA WFPESf1L - COMPLETA
126700000000     C     KEYWFP        KLIST
126800000000     C                   KFLD                    KWPANN
126900000000     C                   KFLD                    KWPDIV
127000000000     C                   KFLD                    KWPARE
127100000000     C                   KFLD                    KWPFIL
127200040220     C                   KFLD                    KWPRAG
127300000000     C                   KFLD                    KWPKSC
127400000000     C                   KFLD                    KWPCTR
127500000000     C                   KFLD                    KWPMES
127600080110
127700080110     c     ktntam        klist
127800080110     c                   kfld                    depksc
127900080110     c                   kfld                    depctr
128000020910     C*--------------------
128100020910     C* REPERISCE DATA/ORA CORRENTE
128200020910     C*--------------------
128300020910     C                   TIME                    N14              14 0
128400020910     C                   MOVE      N14           G08DAT            8 0
128500020910     C                   Z-ADD     *ZEROS        G08INV
128600020910     C                   MOVEL     '0'           G08ERR
128700020910     C                   CALL      'XSRDA8'
128800020910     C                   PARM                    WLBDA8
128900020910     C                   Z-ADD     G08INV        DATCOR            8 0
129000000000     C*--------------------
129100000000     C* CAMPI DI CONTROLLO
129200000000     C*--------------------
129300000000     C                   MOVEL     '0'           FINL00            1
129400000000     C                   SETON                                        OF
129500000000     C                   Z-ADD     UDATE         DATA              6 0
129600000000     C                   TIME                    ORA               6 0
129700000000     C                   MOVEL     '0'           $STDAC            1
129800991229     C*
129900991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
130000991229     C* PER FACILITARE I CONTROLLI
130100991229     C                   EVAL      PARMIN=%abs(PARMIN)
130200000000     C*
130300000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
130400000000IF  1C     PARDET        IFEQ      'D'
130500000000     C                   MOVEL     INT(1)        DECPAR            9
130600000000X   1C                   ELSE
130700000000     C                   MOVEL     INT(2)        DECPAR
130800000000E   1C                   ENDIF
130900000706     C*
131600000000     C*
132100000000     C*
132200000000     C* CARICA ORGANIGRAMMA
132300000000     C                   EXSR      CARORG
132400000000     C*
132500090513     C**                 SETOFF                                       02
132600000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
132700000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
132800080318     C**   PARDIV        IFEQ      *BLANKS
132900080318     C**   PARARE        ANDEQ     *ZEROS
133000080318     C**   PARFIL        ANDEQ     *ZEROS
133100080318     C**                 SETOFF                                       02
133200080318     C**                 ELSE
133300080318     C**                 SETON                                        02
133400080318     C**                 END
133401090513     c                   if        parord='1'
133402090708     c                   eval      orddes='Comm.le unificante'
133403090513     c                   else
133404090708     c                   eval      orddes='Ragione Sociale   '
133405090513     c                   endif
133406090708     c                   clear                   decflp
133407090708     c                   setoff                                       04
133408090708     c                   if        parflp>0
133409090708     c                   seton                                        04
133410090708     c     parflp        chain     azorg01l
133411090708     c                   if        %found(azorg01l)
133412090708     C                   MOVEL     ORGDES        decflp
133413090708     c                   endif
133414090708     c                   endif
133500000000     C*
133600000000     C                   SETOFF                                       03
133700080318     c* inverto data spedizione dal/al in giorno/mese/anno per la stampa
133800080318     c                   move      pardal        dataiso
133900080318     c                   move      dataiso       dataeur
134000080318     c                   move      dataeur       pardal_E
134100080318     c                   move      paral         dataiso
134200080318     c                   move      dataiso       dataeur
134300080318     c                   move      dataeur       paral_E
134301090708     c*
134302090708     c* Carico tabella tipi tariffa aventi flag "gestione rapporto peso/vol"='S'
134303090708     c                   clear                   ctr
134304090708     c                   clear                   ii
134305090708     C                   MOVEL     1             KTBKUT
134306090708     C                   MOVE      'TR'          KTBCOD
134307090708     c     keytabp       setll     tabel00f
134308090708     c     keytabp       reade     tabel00f
134309090708     c                   dow       not %eof(tabel00f)
134310090708     c                   movel     tbluni        dstr
134311090708     c                   if        §trrpv='S'
134312090708     c                   add       1             ii
134313090708     c                   movel     tblkey        ctr(ii)
134314090708     c                   endif
134315090708     c     keytabp       reade     tabel00f
134316090708     c                   enddo
134400000000     C*
134500000000     C                   ENDSR
134600000000     C*------------------------------------------------------------------------*
134700000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
134800000000     C*------------------------------------------------------------------------*
134900000000     C     CARORG        BEGSR
135000000000     C*
135100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
135200000000     C                   Z-ADD     *ZEROS        KORFIL
135300000000     C     KEYORG        SETLL     AZORG01L                           98
135400000000     C  N98              READ      AZORG01L                               98
135500000000     C                   Z-ADD     0             I                 3 0
135600000000     C     *IN98         DOWEQ     '0'
135700991025     C*          ORGFVA    IFEQ *BLANKS                    --- 2 -->
135800000000     C     ORGFAG        IFNE      'V'
135900000000     C                   ADD       1             I
136000000000     C                   Z-ADD     ORGFIL        FIL(I)
136100000000     C                   MOVEL     ORGDES        DFIL(I)
136200040308     C                   MOVEL     ORGDE3        OG143
136300020627     C                   MOVEL     §OGNTW        PFIL(I)
136400000000     C                   END
136500991025     C*                    END                             <-- 2 ---
136600000000     C                   READ      AZORG01L                               98
136700000000     C                   END
136800000000     C*
136900000000     C                   ENDSR
137000000000     O*------------------------------------------------------------------------*
137100000000     OPRTF198   E            TESTA          1 02
137200080318     O                                            8 'FNLSF3R '
137300000000     O                                           90 '***      STATISTICA  '
137400000000     O                                          110 ' P E S I     ***  '
137500000000     O                       DECPAR             120
137600000000     O                                          150 'Data'
137700000000     O                       DATA               159 '  /  /  '
137800000000     O                                          169 'Ora'
137900000000     O                       ORA                178 '  :  :  '
138000000000     O                                          193 'Pag.'
138100000000     O                       PAGE          Z    198
138200000000     O*------------------------
138300000000     O          E   N03      TESTA          1
138400080318     O                                           85 'DATA Spedizione dal'
138500080318     O                       PARdal_E      8  +   1
138600080318     O                                        +   2 'al'
138700080318     O                       PARal_E       8  +   1
138800080318     O***                                        97 'MESI:'
138900080318     O***                    PARMMI             100
139000080318     O***                                       102 '/'
139100080318     O***                    PARMMF             105
139200080318     O***                    DECPOS             198
139300000000     O*------------------------
139400090513     O          E            TESTA       1  1
139500090513     O                                           20 'ORDINAMENTO: per'
139700090513     O                       orddes           +   1
139701090709     o               04                       +   4 'SOLO FILIALE DI PARTENZA'
139702090708     o               04      parflp           +   1
139703090708     o               04      decflp           +   1
139800080318     O***                                        61 '               AREA:'
139900080318     O***                    PARARE        Z     64
140000080318     O***                                        84 '            P.OPER.:'
140100080318     O***                    PARFIL        Z     87
140200000000     O*------------------------
140300000000     O          E            TESTA       1  1
140400000000     O                       TES(1)              66
140500000000     O                       TES(1)             132
140600000000     O                       TES(1)             198
140700000000     O*------------------------
140800000000     O          E            TESTA          1
140900080318     O                                           10 '! TER.PAR:'
141000000000     O                       DESFIL            +  1
141100991025     O                                         +  1 '   AREA:'
141200991025     O                       DESARE            +  1
141300991025     O                                         +  1 '   DISTRETTO:'
141400000000     O                       DESDIV            +  1
141500991229     O                                         +  1 '   SCOST. (A) DA: '
141600991026     O                       PARRG1        N   +  1
141700000000     O                                         +  1 '%'
141800991025     O                                         +  1 '   A:'
141900991026     O                       PARRG2        N   +  1
142000000000     O                                         +  1 '%'
142100991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
142200991026     O                       PARPEI        N   +  1
142300000000     O                                         +  1 '%'
142400991229     O                                         +  1 ' SENZA IMP'
142500991026     O                       PARPES        N   +  1
142600991025     O                                         +  1 '%'
142700000000     O                                          198 '!'
142800991026     O*-------------------------                                       --------*
142900991102     O          E            TESTA          1
143000991102     O                                            1 '!'
143100991102     O                       CHKVDL            + 10
143200991229     O                                          115 'MIN.   (A)   :±'
143300991229     O                       PARMIN        N   +  0
143400991229     O                                         +  1 '%'
143500991102     O                                          198 '!'
143600991102     O*------------------------
143700991102     O          E            TESTA
143800991102     O                       TES(1)              66
143900991102     O                       TES(1)             132
144000991102     O                       TES(1)             198
144100991026     O*------------------------
144200080318     O*         E            TESTA          1
144300080318     O*                      TES(18)             66
144400080318     O*                      TES(19)            132
144500080318     O*                      TES(20)            198
144600991026     O*------------------------
144700080318     O*         E            TESTA          1
144800080318     O*                      TES(1)             132
144900080318     O*                      TES(1)             198
145000000000     O*------------------------
145100000000     O          E            TESTA          1
145200000000     O                       TES(2)              66
145300000000     O                       TES(3)             132
145400000000     O                       TES(4)             198
145500000000     O*------------------------
145600000000     O          E            TESTA          1
145700000000     O                       TES(5)              66
145800000000     O               01                           9 'CLIENTE'
145900000000     O               01                          50 'TARIFFA'
146000080110     o               01                          63 'TAP'
146100000000     O                       TES(6)             132
146200000000     O                       TES(7)             198
146300000000     O*------------------------
146400000000     O          E            TESTA          1
146500000000     O                       TES(1)              66
146600000000     O                       TES(1)             132
146700000000     O                       TES(1)             198
146701090708     O          E            rigcom         0
146702090708     O                                        +   3 'Commerciale unificante'
146703090708     o                       depcun              34
146704090708     o                       descun              60
146705090708     O          E            rigcom         1
146707090708     o                       depcun              34
146708090708     o                       descun              60
146709090708     O          E            rigcom         1
146710090708     O                       TES(1)              66
146711090708     O                       TES(1)             132
146712090708     O                       TES(1)             198
146800000000     O*------------------------------------------------------------------------*
146900000000     O          E            RIGCLI         1
147000000000     O                                            1 '!'
147100000000     O                       DEPKSC        Z   +  1
147200000000     O                       DESCLI            +  1
147300040308     O                       DEPCTR            +  1
147400000000     O                       DESCTR            +  1
147500080110     o                       PTAP              +  1
147600080110     O                                         +  4 '!'
147700000516     O                       STNSP         Z     75
147800000516     O                       STPER         O     85
147900000516     O                       STPSB         4     98
148000000516     O                       STPSC         4    110
148100000516     O                       STRNG         O    119
148200040220     O*---                   STTTP         O    130
148300080318     O****                                      133 '!!'
148400080318     O***                    SPNSP         Z    141
148500080318     O***                    SPPER         O    151
148600080318     O***                    SPPSB         4    164
148700080318     O***                    SPPSC         4    176
148800080318     O***                    SPRNG         O    185
149000991026     O                                          198 '!'
149100991026     O*------------------------------------------------------------------------*
149200991026     O*       E  1             RIGNUL
149300991026     O*                                   1 '!'
149400991026     O*                                  67 '!'
149500991026     O*                                 132 '!'
149600991026     O*                                 133 '!'
149700991026     O*                                 198 '!'
149800000000     O*------------------------------------------------------------------------*
149900991026     O          E            RIGTOT         1
150000991026     O                                            1 '!'
150100090708     O              n21      STDES1            +  1
150200090708     O              n21      STDES2            +  1
150300090708     O              n21                        + 21 '!'
150301090708     O               21      STDES3              62
150302090708     O               21                          67 '!'
150400991026     O                       STNSP         Z     75
150500000516     O                       STPER         O     85
150600000516     O                       STPSB         4     98
150700000516     O                       STPSC         4    110
150800000516     O                       STRNG         O    119
150900040220     O*---                   STTTP         O    130
151000080318     O****                                      133 '!!'
151100080318     O****                   SPNSP         Z    141
151200080318     O****                   SPPER         O    151
151300080318     O****                   SPPSB         4    164
151400080318     O****                   SPPSC         4    176
151500080318     O****                   SPRNG         O    185
151700991026     O                                          198 '!'
151800991026     O*------------------------------------------------------------------------*
151900000000     O          E            RIGSEP         1
152000000000     O                       TES(1)              66
152100000000     O                       TES(1)             132
152200000000     O                       TES(1)             198
152300000000     O*------------------------
152400000000     O          E            STAFIN           63
152500000000     O                                          110 '*** FINE STAMPA ***'
152600000000     O*------------------------------------------------------------------------*
152700000000** -TES-   (DESCRIZIONI  STAMPA)
152800000000------------------------------------------------------------------  1
152900000000!                                                                   2
153000080318!   NUM.   (B) %RAPP.     PESO        PESO      (A) %               3
153100080318                                                                 !  4
153200040308!                                                                   5
153300080318!   SPED.    COLLI        BOLL.       VDL       SCOST.              6
153400080318                                                                 !  7
153500000000                                                                    8
153600000000                                                                    9
153700080318TOTALE TER.PART                                                    10
153800000000TOTALE AREA                                                        11
153900000000TOTALE DISTRETTO                                                   12
154000000000TOTALE GENERALE                                                    13
154100090708PROG.  FILIALE                                                     14
154200991026PROG.  AREA                                                        15
154300991026PROG.  DISTRETTO                                                   16
154400991026PROG.  GENERALE                                                    17
154500991026!                                                                  18
154600080318!                                                                  19
154700080318                                                                 ! 20
154701090708TOTALE COMM.LE                                                     21
154702090708. . .  Di cui tariffe con tp.applicazione "MAI VDL" negativi       22
154800000000** -INT-   (INTESTAZIONE)
154900000000DETTAGLIO                                                       1
155000000000SINTETICA                                                       2
1551000000003
1552000000004
1553000000005
155400000000** -DEC-   (DECODIFICHE RIGHE)
155500000000ASSEGNATI                                                            1
155600000000FRANCHI                                                              2
155700991026CON IMPIANTO                                                         3
155800991026SENZA IMPIANTO                                                       4
155900000706POSTE                                                                5
156000000706NO POSTE                                                             6
1561000007067
1562000007068
1563000007069
15640000070610
