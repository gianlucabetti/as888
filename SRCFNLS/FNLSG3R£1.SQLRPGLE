000100000000     H*------------------------------------------------------------------------*
000200000000     H* STAMPA STATISTICA VOLUMI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800090709     FWFVLMF1L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100010314     FTNTAM01L  IF   E           K DISK
001200000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300000000     F                                     FORMLEN(66)
001400000000     F                                     FORMOFL(60)
001500000000     L*------------------------------------------------------------------------*
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900080318     d dataiso         s               d   datfmt(*iso)
002000080318     d dataeur         s               d   datfmt(*eur)
002100080318     d PARDAL_E        s                   LIKE(PARDAL)
002200080318     d PARal_e         s                   LIKE(PARAL)
002201090709     d orddes          s             18
002202090709     d decflp          s             20
002203090709     d w_primav        s               n
002204090710     d rotcom          s               n
002300000000     D*-------------------
002400000000     D* INTESTAZIONI STAMPA
002500000000     D*-------------------
002600090709     D TES             S             66    DIM(14) CTDATA PERRCD(1)
002700950629     D INT             S              9    DIM(5) CTDATA PERRCD(1)
002800000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)
002900000000     D*-------------------
003000000000     D* SCHIERA FILIALI
003100000000     D*-------------------
003200971124     D FIL             S              3  0 DIM(500)
003300971124     D DFIL            S             20    DIM(500)
003400020627     D PFIL            S              3    DIM(500)
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800000000     D*-------------------
003900000000     D* RICERCA PARAMETRI UTENTI
004000000000     D*-------------------
004100020919     D TIBS34DS      E DS
004200020919     D DDATIUTE      E DS
004300020919     D AZUTEDS       E DS                  extname(AZUTE00F)
004301090513
004302090709     d wfvlmds       e ds                  extname(wfvlmf0f) inz
004400000000     D*-------------------
004500000000     D* ARCHITETTURA
004600000000     D*-------------------
004700000000     D KPJBA         E DS
004800991102     D*-------------------
004900991102     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
005000991102     D*-------------------
005100991102     D OG150         E DS
005200000706     D*-------------------
005300000706     D* LETTURA CAMPO ORGDE£ DI AZORG
005400000706     D*-------------------
005500000706     D OG143         E DS
005501090709     D*-------------------
005502090709     d ds01          e ds
005600020910     D*------------------
005700020910     D* DS REPEIMENTO TERMINALs
005800020910     D*------------------
005900020910     D FNLV55DS      E DS
006000030909     D*------------------
006100030909     D* DS RIDEFINIZIONE FLAG OPERATIVI TARIFFE
006200030909     D*------------------
006300030909     D DSTA01        E DS
006400020910     D*------------------
006500020910     D* DS CONTROLLO DATA (8)
006600020910     D*------------------
006700020910     D WLBDA8          DS                  INZ
006800020910     D  G08DAT                 1      8  0
006900020910     D  G08INV                 9     16  0
007000020910     D  G08ERR                17     17
007100020910     D  G08TGI                18     22  0
007200000000     D*-------------------
007300000000     D* DS PARAMETRI DI LANCIO PGM
007400000000     D*-------------------
007500950703     D PARAM           DS
007600991029     D  PARTLA                 1      1
007700080318     D**PARANN                 2      5  0
007800080318     D**PARMMI                 6      7  0
007900080318     D**PARMMF                 8      9  0
008000080318     d  pardal                 2      9  0
008100080318     D**PARSNR                10     10
008200991029     D  PARPID                27     31  2
008300080318     D**PARDIV                32     32
008400080318     D**PARARE                33     35  0
008500080318     D**PARFIL                36     38  0
008600080318     D  PARtfp                36     38  0
008700991029     D  PARDET                39     39
008800080318     D**PARTOT                40     40
008900991029     D  PARRG1                41     44  1
009000991029     D  PARRG2                45     48  1
009100080318     D**PARPIA                49     53  2
009200991029     D  PARPSD                54     58  2
009300080318     D**PARPSA                59     63  2
009400080318     D**PAROK                 65     66
009500991229     D  PARMIN                67     71  3
009600080318     D**PARPOS                72     72
009700080318     D**PARNT1                73     75
009800080318     D**PARNT2                76     78
009900080318     D**PARNT3                79     81
010000080318     D**PARNT4                82     84
010100080318     D**PARSAL                85     85
010200080318     d  paral                 86     93  0
010201090513     d  parord                75     75
010202090709     d  parpag                76     76
010203090709     d  parflp                77     79  0
010300000000     C*------------------------------------------------------------------------*
010400000000     C* MAIN LINE
010500000000     C*------------------------------------------------------------------------*
010600000000     C*
010601090513     c* In base all'ordinamento scelto (1-cliente; 2-ragione sociale)
010602090513     c* eseguo sql o utilizzo v.l.
010603090513     c                   if        parord='1'
010604090709     c                   exsr      sr_ordcomset
010605090709     c                   exsr      sr_ordcomlet
010606090709     c                   exsr      sr_elabcom
010607090513     c                   else
010700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
010800000000     C                   EXSR      SETFIL
010900000000     C*
011000000000     C* CICLO FINO A FINE FILE DI LAVORO
011100000000DO  1C     FINL00        DOWEQ     '0'
011200000000     C*
011300000000     C* OPERAZIONI PER NUOVA DIVISIONE
011400000000     C                   EXSR      NEWDIV
011500000000     C*
011600000000     C* CICLO FINO A ROTTURA DI DIVISIONE
011700000000DO  2C     FINL00        DOWEQ     '0'
011800000000     C     WFVDIV        ANDEQ     DEPDIV
011900000000     C*
012000000000     C* OPERAZIONI PER NUOVA AREA
012100000000     C                   EXSR      NEWARE
012200000000     C*
012300000000     C* CICLO FINO A ROTTURA DI AREA
012400000000DO  3C     FINL00        DOWEQ     '0'
012500000000     C     WFVDIV        ANDEQ     DEPDIV
012600000000     C     WFVARE        ANDEQ     DEPARE
012700000000     C*
012800000000     C* OPERAZIONI PER NUOVA FILIALE
012900000000     C                   EXSR      NEWFIL
013000000000     C*
013100000000     C* CICLO FINO A ROTTURA DI FILIALE
013200000000DO  4C     FINL00        DOWEQ     '0'
013300000000     C     WFVDIV        ANDEQ     DEPDIV
013400000000     C     WFVARE        ANDEQ     DEPARE
013500000000     C     WFVFIL        ANDEQ     DEPFIL
013600991029     C*
013700991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
013800991029     C                   EXSR      NEWPRO
013900991029     C*
014000991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
014100991029DO  5C     FINL00        DOWEQ     '0'
014200991029     C     WFVDIV        ANDEQ     DEPDIV
014300991029     C     WFVARE        ANDEQ     DEPARE
014400991029     C     WFVFIL        ANDEQ     DEPFIL
014500991029     C     WFVKSC        ANDEQ     DEPKSC
014600991029     C     WFVCTR        ANDEQ     DEPCTR
014700000000     C*
014800991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
014900000000     C                   EXSR      NEWCLI
015000000000     C*
015100991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
015200991029DO  6C     FINL00        DOWEQ     '0'
015300991029     C     WFVDIV        ANDEQ     DEPDIV
015400991029     C     WFVARE        ANDEQ     DEPARE
015500991029     C     WFVFIL        ANDEQ     DEPFIL
015600991029     C     WFVKSC        ANDEQ     DEPKSC
015700991029     C     WFVCTR        ANDEQ     DEPCTR
015800991029     C     WFVMES        ANDEQ     DEPMES
015900000000     C*
016000000000     C* MEMORIZZA LE SPEDIZIONI
016100000517     C     WRECOK        IFEQ      'S'
016200000517     C                   EXSR      MEMSPE
016300000517     C                   ENDIF
016400000000     C*
016500000000     C* LEGGE IL RECORD SUCCESSIVO
016600000000     C                   EXSR      LETFIL
016700000000     C*
016800040220E   6C                   ENDDO
016900000000     C*
017000991029     C* DETTAGLIO: STAMPA IL TOTALE CLIENTE/TARIFFA/MESE
017100000000     C                   EXSR      STACLI
017200000000     C*
017300991029E   5C                   ENDDO
017400991029     C*
017700991029     C*
017800000000E   4C                   ENDDO
017900000000     C*
018000000000     C* STAMPA IL TOTALE FILIALE
018100000000     C                   EXSR      STAFIL
018200000000     C*
018300000000E   3C                   ENDDO
018900000000     C*
019000000000E   2C                   ENDDO
019600000000     C*
019700000000E   1C                   ENDDO
020300000000     C*
020400000000     C* STAMPA RIGA FINE STAMPA
020500000000     C                   EXCEPT    STAFIN
020501090709     c                   endif
020502090513      /free
020503090513       if parord='1';
020504090513          exec sql close WVLMcsr;
020505090513       endif;
020506090513      /end-free
020600000000     C*
020700000000     C                   SETON                                        LR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
021000000000     C*------------------------------------------------------------------------*
021100000000     C     SETFIL        BEGSR
021200000000     C*
021300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
021400080318     C                   clear                   KWVANN
021500000000     C                   MOVEL     *BLANKS       KWVDIV
021600000000     C                   Z-ADD     *ZEROS        KWVARE
021700000000     C                   Z-ADD     *ZEROS        KWVFIL
021800000000     C                   Z-ADD     *ZEROS        KWVKSC
021900000000     C                   Z-ADD     *ZEROS        KWVCTR
022000000000     C                   Z-ADD     *ZEROS        KWVMES
022100950629     C                   MOVEL     *BLANKS       KWVFVB
022200090709     C**   KEYWFV        SETLL     WFVLMF1L                           99
022300090709     C     *loval        SETLL     WFVLMF1L                           99
022400000000     C*
022500000000     C     *IN99         IFEQ      '1'
022600000000     C                   MOVEL     '1'           FINL00
022700000000     C                   ELSE
022800000000     C                   EXSR      LETFIL
022900000000     C                   END
023000000000     C*
023100000000     C                   ENDSR
023200000000     C*------------------------------------------------------------------------*
023300941207     C* LETFIL - LETTURA PROSSIMO RECORD
023400000000     C*------------------------------------------------------------------------*
023500000000     C     LETFIL        BEGSR
023600000000     C*
023700000000     C* LEGGO FINO A:
023800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
023900000000     C*     - TROVATO RECORD VALIDO
024000000000     C                   MOVEL     'N'           WRECOK
024100040211     C                   MOVEL     'N'           WRECOKC
024200000000     C*
024300000000     C* LETTURE SUCCESSIVE ALLA PRIMA
024400090709     C                   READ      WFVLMF1L                               99
024500000000     C*
024600000000     C     *IN99         IFEQ      '1'
024700080318     C***  WFVANN        ORNE      PARANN
024800000000     C                   MOVEL     '1'           FINL00
024900000000     C                   ELSE
025000000000     C                   EXSR      CHKREC
025100000517     C                   ENDIF
025200000000     C*
025300000000     C                   ENDSR
025400000000     C*------------------------------------------------------------------------*
025500000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
025600000000     C*------------------------------------------------------------------------*
025700000000     C     CHKREC        BEGSR
025800000000     C*
025900000000     C                   MOVEL     'S'           WRECOK            1
026000040211     C                   MOVEL     'S'           WRECOKC           1
026100000000     C*
026200000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
026300080318     C**   WFVMES        IFGE      PARMMI
026400080318     C**   WFVMES        ANDLE     PARMMF
026500080318     C**                 ELSE
026600080318     C**                 MOVEL     'N'           WRECOK
026700080318     C**                 END
026800000706     C*
026900000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
027000080318IF  1C**   WRECOK        IFEQ      'S'
027100080318     C**                 Z-ADD     1             I
027200080318     C*    WFVFIL        LOOKUP    FIL(I)                                 95
027300080318IF  2C*    PARPOS        IFEQ      'N'
027400080318IF  3C*    PFIL(I)       IFEQ      'PPT'
027500080318     C*                  MOVEL     'N'           WRECOK
027600080318E   3C*                  ENDIF
027700080318X   2C*                  ELSE
027800080318IF  3C*    PFIL(I)       IFNE      'PPT'
027900080318     C*                  MOVEL     'N'           WRECOK
028000080318E   3C*                  ENDIF
028100080318E   2C*                  ENDIF
028200080318E   1C*                  ENDIF
028300000000     C*
028400000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
028500080318     C*    PARDIV        IFNE      *BLANKS
028600080318     C*    WFVDIV        IFNE      PARDIV
028700080318     C*                  MOVEL     'N'           WRECOK
028800080318     C*                  END
028900080318     C*                  END
029000000000     C*
029100000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
029200080318     C*    PARARE        IFNE      *ZEROS
029300080318     C*    WFVARE        IFNE      PARARE
029400080318     C*                  MOVEL     'N'           WRECOK
029500080318     C*                  END
029600080318     C*                  END
029700000000     C*
029800000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
029900080318     C*    PARFIL        IFNE      *ZEROS
030000080318     C*    WFVFIL        IFNE      PARFIL
030100080318     C*                  MOVEL     'N'           WRECOK
030200080318     C*                  END
030300080318     C*                  END
030400950705     C*
030500950705     C* CALCOLA PERCENTUALE VOLUME CML SU VOL.AUTOM.
030600001009     C                   Z-ADD     WFVVLB        WCPVLB           15 3
030700001009     C                   Z-ADD     WFVVLC        WCPVLC           15 3
030800991102     C                   EXSR      CALRNG
030900991102     C                   Z-ADD     WCPRVA        WRNG              6 2
031000991102     C*
031100991104     C* CALCOLA RAPPORTO COLLI CML SU COLLI BOLL.
031200001009     C                   Z-ADD     WFVTCL        WCPNCL           15 3
031300001009     C                   Z-ADD     WFVNCR        WCPNCR           15 3
031400991102     C                   EXSR      CALPER
031500991102     C                   Z-ADD     WCPPVA        WPER              6 2
031600000000     C*
031700991104     C* SE IL RAPPORTO COLLI VDL CON/SENZA IMPIANTO NO IN RANGE RICHIESTO, SCART
031800040211     c* CONSIDERAZIONI SOLO PER RIGA DETTAGLIO
031900991102     C     WCPPVA        IFLT      PARXDA
032000991229     C     WCPPVA        OREQ      *ZEROS
032100040211     C                   MOVEL     'N'           WRECOKC
032200000000     C                   END
032300991102     C*
032400991229     C* SE LA PERCENTUALE (A) SCOST. VOL NON COMPRESA IN RANGE RICHIESTO,SCARTO
032500991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
032600991229     C* DELLO SCARTO MINIMO (PARMIN)
032700991229     C                   Z-ADD     *zeros        PERC              6 2
032800991229     C                   EVAL      PERC=%abs(WCPRVA)
032900991229     C     WCPRVA        IFLT      PARRG1
033000991229     C     WCPRVA        ORGT      PARRG2
033100991229     C     WCPRVA        OREQ      *ZEROS
033200991229     C     PERC          ORLT      PARMIN
033300040211     C                   MOVEL     'N'           WRECOKC
033400991102     C                   END
033500030910     C*
033600030910     C* CONTROLLO SE RICHIESTE SELEZIONI X NETWORK TARIFFE
033700080318     C*                  IF        WRECOK <> 'N'
033800080318     C*                  IF        PARNT1 <> *blanks OR
033900080318     C*                            PARNT2 <> *blanks OR
034000080318     C*                            PARNT3 <> *blanks OR
034100080318     C*                            PARNT4 <> *blanks
034200030910     C* SE RICHIESTE SELEZIONI VERIFICO
034300080318     C*                  MOVEL     *blanks       wNTWTAR           3
034400030910     C* NEGO I FLAG X POI EVENTUALMENTE ATTIVARLO SE RISPONDENTE AI PARAMETRI RI
034500080318     C*                  MOVEL     'N'           WRECOK
034600030910     C* REPERISCO IL NETWORK DELLA TARIFFA DEL RECORD CORRENTE SE NN TRATTASI DI
034700080318     C*    WFVCTR        IFNE      888
034800080318     C*    WFVCTR        ANDNE     999
034900080318     C*                  Z-ADD     WFVKSC        KTFKSC
035000080318     C*                  Z-ADD     WFVCTR        KTFCTR
035100080318     C*                  Z-ADD     *ZEROS        KTFPRG
035200080318     C*    KEYTAM        SETLL     TNTAM01L                           99
035300080318     C* N99              READ      TNTAM01L                               99
035400080318     C*    *IN99         DOWEQ     '0'
035500080318     C*    TAMKSC        ANDEQ     KTFKSC
035600080318     C*    TAMCTR        ANDEQ     KTFCTR
035700080318     C*    TAMATB        IFEQ      ' '
035800080318     C*                  MOVEL(P)  TAMFLO        DSTA01
035900080318     C*                  IF        TAMFIE  = 'I' AND §TADPD <> 'S'
036000080318     C*                  EVAL      wNTWTAR = 'ITA'
036100080318     C*                  ENDIF
036200080318     C*                  IF        TAMFIE  = 'I' AND §TADPD  = 'S'
036300080318     C*                  EVAL      wNTWTAR = 'DPD'
036400080318     C*                  ENDIF
036500080318     C*                  IF        TAMFIE  = 'E' AND §TAFED <> 'S'
036600080318     C*                  EVAL      wNTWTAR = 'EEX'
036700080318     C*                  ENDIF
036800080318     C*                  IF        TAMFIE  = 'E' AND §TAFED  = 'S'
036900080318     C*                  EVAL      wNTWTAR = 'FED'
037000080318     C*                  ENDIF
037100080318     C*                  ENDIF
037200080318     C*                  READ      TNTAM01L                               99
037300080318     C*                  ENDDO
037400080318     C*                  ENDIF
037500030910     C* EFFETTUO CONSIDERAZIONI
037600080318     C*                  IF        PARNT1 = 'ITA' OR
037700080318     C*                            PARNT2 = 'ITA' OR
037800080318     C*                            PARNT3 = 'ITA' OR
037900080318     C*                            PARNT4 = 'ITA'
038000080318     C*                  IF        wNTWTAR = 'ITA' OR
038100080318     C*                            WFVCTR = 888    OR
038200080318     C*                            WFVCTR = 999
038300080318     C*                  MOVEL     'S'           WRECOK
038400080318     C*                  ENDIF
038500080318     C*                  ENDIF
038600080318     C*                  IF        PARNT1 = 'EEX' OR
038700080318     C*                            PARNT2 = 'EEX' OR
038800080318     C*                            PARNT3 = 'EEX' OR
038900080318     C*                            PARNT4 = 'EEX'
039000080318     C*                  IF        wNTWTAR = 'EEX'
039100080318     C*                  MOVEL     'S'           WRECOK
039200080318     C*                  ENDIF
039300080318     C*                  ENDIF
039400080318     C*                  IF        PARNT1 = 'DPD' OR
039500080318     C*                            PARNT2 = 'DPD' OR
039600080318     C*                            PARNT3 = 'DPD' OR
039700080318     C*                            PARNT4 = 'DPD'
039800080318     C*                  IF        wNTWTAR = 'DPD'
039900080318     C*                  MOVEL     'S'           WRECOK
040000080318     C*                  ENDIF
040100080318     C*                  ENDIF
040200080318     C*                  IF        PARNT1 = 'FED' OR
040300080318     C*                            PARNT2 = 'FED' OR
040400080318     C*                            PARNT3 = 'FED' OR
040500080318     C*                            PARNT4 = 'FED'
040600080318     C*                  IF        wNTWTAR = 'FED'
040700080318     C*                  MOVEL     'S'           WRECOK
040800080318     C*                  ENDIF
040900080318     C*                  ENDIF
041000030910     C*
041100080318     C*                  ENDIF
041200080318     C*                  ENDIF
041300000000     C*
041400000000     C                   ENDSR
041500000000     C*------------------------------------------------------------------------*
041600000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
041700000000     C*------------------------------------------------------------------------*
041800000000     C     MEMSPE        BEGSR
041801090709     c*
042300000000     C* TOTALE CLIENTE/TARIFFA
042400040212IF  2C     WRECOKC       IFEQ      'S'
042500040216     c                   ADD       WFVTCL        TCTCL
042600040216     C                   ADD       WFVNCL        TCNCL
042700040216     C                   ADD       WFVNCR        TCNCR
042800040216     C                   ADD       WFVPKB        TCPKB
042900040216     C                   ADD       WFVNSP        TCNSP
043000040216     C                   ADD       WFVVLB        TCVLB
043100040216     C                   ADD       WFVVLC        TCVLC
043200040216E   2C                   ENDIF
043300040216     C                   ADD       WFVVLB        TCVLBT
043400040216     C                   ADD       WFVVLC        TCVLCT
043500000000     C* TOTALE FILIALE
043600040212     C                   ADD       WFVTCL        TFTCL
043700040216     C                   ADD       WFVNCL        TFNCL
043800040216     C                   ADD       WFVNCR        TFNCR
043900040216     C                   ADD       WFVPKB        TFPKB
044000040216     C                   ADD       WFVNSP        TFNSP
044100991102     C                   ADD       WFVVLB        TFVLB
044200991102     C                   ADD       WFVVLC        TFVLC
044300040212     C                   ADD       WFVVLB        TFVLBT
044400040212     C                   ADD       WFVVLC        TFVLCT
044401090709     C* TOTALE commerciale unificante
044402090709     c                   if        parord='1'
044403090709     C                   ADD       WFVTCL        TKTCL
044404090709     C                   ADD       WFVNCL        TKNCL
044405090709     C                   ADD       WFVNCR        TKNCR
044406090709     C                   ADD       WFVPKB        TKPKB
044407090709     C                   ADD       WFVNSP        TKNSP
044408090709     C                   ADD       WFVVLB        TKVLB
044409090709     C                   ADD       WFVVLC        TKVLC
044410090709     C                   ADD       WFVVLB        TKVLBT
044411090709     C                   ADD       WFVVLC        TKVLCT
044412090709     c                   endif
053200040212     C*
053300040212     C                   ENDSR
053400000000     C*------------------------------------------------------------------------*
053500000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
053600000000     C*------------------------------------------------------------------------*
053700000000     C     NEWDIV        BEGSR
053800000000     C*
053900000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
054000000000     C                   MOVEL     WFVDIV        DEPDIV
054100000000     C*
054200000000     C* DECODIFICA LA DIVISIONE
054300000000     C                   EXSR      DECDIV
054400000000     C*
056700000000     C                   ENDSR
056800000000     C*------------------------------------------------------------------------*
056900000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
057000000000     C*------------------------------------------------------------------------*
057100000000     C     NEWARE        BEGSR
057200000000     C*
057300000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
057400000000     C                   Z-ADD     WFVARE        DEPARE
057500000000     C*
057600000000     C* DECODIFICA L'AREA
057700000000     C                   EXSR      DECARE
060000000000     C*
060100000000     C                   ENDSR
060200000000     C*------------------------------------------------------------------------*
060300000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
060400000000     C*------------------------------------------------------------------------*
060500000000     C     NEWFIL        BEGSR
060600000000     C*--------------------
060700000000     C* T E S T A T A
060800000000     C*--------------------
060900000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
061000000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
061100000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
061200000000     C*         RIGHE DI TOTALE
061300000000     C*
061400000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
061500000000     C                   Z-ADD     WFVFIL        DEPFIL
061600020910     C*
061700020910     C* REPERISCE IL TERMINAL PARTENZA
061800020910     C                   CLEAR                   FNLV55DS
061900020910     C                   MOVEL     'P'           D55TPT
062000020910     C                   MOVEL     DEPFIL        D55LIN
062100020910     C                   Z-ADD     DATCOR        D55DRF
062200020910     C                   CALL      'FNLV55R'
062300020910     C                   PARM                    FNLV55DS
062400020910IF  1C     D55ERR        IFNE      *BLANKS
062500020910     C                   MOVEL     DEPFIL        D55TFP
062600020910E   1C                   ENDIF
062700020910     C                   Z-ADD     D55TFP        DEPTFP
062800991102     C*
062900020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
063000020910     C     DEPTFP        CHAIN     AZORG01L                           88
063100991102     C     *IN88         IFEQ      *OFF
063200991102     C                   MOVEL     ORGDF0        OG150
063300120914     C     §OGVOL        IFNE      *BLANKS
063400120914     C     §OGVOL        andNE     *all'0'
063500120914     c**   §ogspv        ifeq      'S'
063600991102     C                   MOVEL     DEC(3)        CHKCML           20
063700991102     C                   Z-ADD     PARPID        PARXDA            5 2
063800991102     C                   ELSE
063900991102     C                   MOVEL     DEC(4)        CHKCML           20
064000991102     C                   Z-ADD     PARPSD        PARXDA            5 2
064100991102     C                   ENDIF
064200991102     C                   ELSE
064300991102     C                   MOVEL     DEC(4)        CHKCML           20
064400991102     C                   Z-ADD     PARPSD        PARXDA            5 2
064500991102     C                   ENDIF
064600000000     C*
064700000000     C* AZZERA I TOTALI DELLA FILIALE
064800001009     C                   Z-ADD     *ZEROS        TFVLB            15 3
064900001009     C                   Z-ADD     *ZEROS        TFVLC            15 3
065000991029     C                   Z-ADD     *ZEROS        TFNCR             9 0
065100991104     C                   Z-ADD     *ZEROS        TFTCL             9 0
065200991104     C                   Z-ADD     *ZEROS        TFNCL             9 0
065300991102     C                   Z-ADD     *ZEROS        TFPKB            10 1
065400941207     C                   Z-ADD     *ZEROS        TFNSP             8 0
065500040212     c*
066300040212     c*
066400001009     C                   Z-ADD     *ZEROS        TFVLBT           15 3
066500001009     C                   Z-ADD     *ZEROS        TFVLCT           15 3
066800000000     C*
066900000000     C* DECODIFICA LA FILIALE
067000000000     C                   EXSR      DECFIL
067100000000     C*
067200000000     C* IMPOSTAZIONI CAMPI IN TESTATA
067300000000IF  1C     PARDET        IFEQ      'S'
067400000000     C                   MOVEL     *BLANKS       DESFIL
067500991102     C                   MOVEL     *BLANKS       CHKCML
067600000000E   1C                   ELSE
067700000000     C                   SETON                                        01
067800000000E   1C                   ENDIF
067900000000     C*
068000000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
068100000000IF  1C     *INOF         IFEQ      *ON
068200000000     C     PARDET        OREQ      'D'
068300000000     C                   EXCEPT    TESTA
068400000000     C                   SETOFF                                       OF
068500000000E   1C                   ENDIF
068600000000     C*
068700000000     C                   ENDSR
068800000000     C*------------------------------------------------------------------------*
068900000000     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA
069000000000     C*------------------------------------------------------------------------*
069100000000     C     NEWCLI        BEGSR
069200000000     C*
069300000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
069400040216     C                   MOVEL     WFVRAG        DESCLI
069500040216     C                   Z-ADD     WFVKSC        DEPKSC
069600000000     C                   Z-ADD     WFVCTR        DEPCTR
069700040216     C                   Z-ADD     WFVMES        DEPMES
069800000000     C*
069900040219     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
070000001009     C                   Z-ADD     *ZEROS        TCVLB            15 3
070100001009     C                   Z-ADD     *ZEROS        TCVLC            15 3
070200991029     C                   Z-ADD     *ZEROS        TCNCR             9 0
070300991104     C                   Z-ADD     *ZEROS        TCTCL             9 0
070400991104     C                   Z-ADD     *ZEROS        TCNCL             9 0
070500991102     C                   Z-ADD     *ZEROS        TCPKB            10 1
070600950629     C                   Z-ADD     *ZEROS        TCNSP             8 0
070700040212     C*
070800001009     C                   Z-ADD     *ZEROS        TCVLBT           15 3
070900001009     C                   Z-ADD     *ZEROS        TCVLCT           15 3
071000040220     c*
071100080319     C**   depmes        ifle      parmmf
071200080319     C                   Z-ADD     0             FLGCLI            1 0
071300080319     c**                 endif
071400950629     C*
071500000000     C                   ENDSR
071600991029     C*------------------------------------------------------------------------*
071700991029     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
071800991029     C*------------------------------------------------------------------------*
071900991029     C     NEWPRO        BEGSR
072000991029     C*
072100991029     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
072200991029     C                   Z-ADD     WFVKSC        DEPKSC
072300991029     C                   Z-ADD     WFVCTR        DEPCTR
073600040212     c*
073700991029     C                   ENDSR
073701090709     C*------------------------------------------------------------------------*
073702090709     C* NEWCOM - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
073703090709     C*------------------------------------------------------------------------*
073704090709     C     NEWCOM        BEGSR
073705090709     C*
073706090710     c                   eval      rotcom=*on
073714090709     C* MEMORIZZA LA NUOVA AREA
073715090709     C                   Z-ADD     WFvcun        DEPcun
073716090709     C*
073717090709     C* DECODIFICA il commerciale unificante
073718090709     C                   EXSR      DECcun
073719090709     c*
073720090709     C* AZZERA I TOTALI DEL commerciale unificante
073721090709     C                   Z-ADD     *ZEROS        TkVLB            15 3
073722090709     C                   Z-ADD     *ZEROS        TkVLC            15 3
073723090709     C                   Z-ADD     *ZEROS        TkNCR             9 0
073724090709     C                   Z-ADD     *ZEROS        TkTCL             9 0
073725090709     C                   Z-ADD     *ZEROS        TkNCL             9 0
073726090709     C                   Z-ADD     *ZEROS        TkPKB            10 1
073727090709     C                   Z-ADD     *ZEROS        TkNSP             8 0
073738090709      *
073739090709     C                   Z-ADD     *ZEROS        TkVLBT           15 3
073740090709     C                   Z-ADD     *ZEROS        TkVLCT           15 3
073750090709     C*
073751090709     C                   ENDSR
073800000000     C*------------------------------------------------------------------------*
073900000000     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
074000000000     C*------------------------------------------------------------------------*
074100000000     C     STACLI        BEGSR
074200950629     C*
074300040219     C* PER NON STAMPARE DUE VOLTE I DATI DEL CLIENTE SE HA DATI
074400950629     C                   MOVEL     '0'           $STDAC
074500000000     C*
074600040212     C* IMPOSTA I VALORI DI STAMPA
074700040216IF  1C     TCVLB         IFNE      *ZEROS
074800950629     C     TCVLC         ORNE      *ZEROS
074900950629     C*
075000950629     C* DECODIFICA LA TARIFFA
075100950629     C                   MOVEL     *BLANKS       DESCTR
075200950629     C                   EXSR      DECCTR
075300001009     C                   Z-ADD     TCVLB         STVLB             9 2
075400001009     C                   Z-ADD     TCVLC         STVLC             9 2
075500991029     C                   Z-ADD     TCNCR         STNCR             9 0
075600991105     C                   Z-ADD     TCTCL         STTCL             9 0
075700991105     C                   Z-ADD     TCNCL         STNCL             9 0
075800991102     C                   Z-ADD     TCPKB         STPKB            10 1
075900941207     C                   Z-ADD     TCNSP         STNSP             8 0
076000950705     C*
076100040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME
076200001009     C                   Z-ADD     TCVLB         WCPVLB           15 3
076300001009     C                   Z-ADD     TCVLC         WCPVLC           15 3
076400991102     C                   Z-ADD     *ZEROS        WCPRVA
076500991102     C                   EXSR      CALRNG
076600991102     C                   Z-ADD     WCPRVA        WRNG              6 2
076700000519     C                   Z-ADD     WRNG          STRNG             4 1
076800000517     C*
076900040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME  TOTALI
077000001009     C                   Z-ADD     TCVLBT        WCPVLB           15 3
077100001009     C                   Z-ADD     TCVLCT        WCPVLC           15 3
077200000517     C                   Z-ADD     *ZEROS        WCPRVA
077300000517     C                   EXSR      CALRNG
077400000517     C                   Z-ADD     WCPRVA        WRNG              6 2
077500000718     C                   Z-ADD     WRNG          STTTP             4 1
077600991102     C*
077700991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLLI BOLL.
077800001009     C                   Z-ADD     TCTCL         WCPNCL           15 3
077900001009     C                   Z-ADD     TCNCR         WCPNCR           15 3
078000991102     C                   Z-ADD     *ZEROS        WCPPVA
078100991102     C                   EXSR      CALPER
078200991102     C                   Z-ADD     WCPPVA        WPER              6 2
078300000517     C                   Z-ADD     WPER          STPER             4 1
078301090710     c*
078302090710     c* prima di stampare la riga del cliente/tariffa, se c'è stata
078303090710     c* rottura di commerciale devo prima stampare il nuovo commerciale
078304090710     c                   if        parord='1'  and rotcom=*on
078305090710     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
078306090710     C     *INOF         IFEQ      *ON
078307090710     C     PARpag        OREQ      'S'
078308090710     c     depcun_p      andgt     0
078309090710     C                   EXCEPT    TESTA
078310090710     C                   SETOFF                                       OF
078312090710     C                   ENDIF
078313090710     c* stampo la riga del commerciale unificante
078314090710     c                   eval      depcun_p=depcun
078315090710     c                   if        pardet='D'
078316090710     c                   except    rigcom
078317090710     c                   endif
078318090710     c                   eval      rotcom=*off
078319090710     c                   endif
078400950629     C*
078500991029     C* STAMPA RIGA CLIENTE/TARIFFA SOLO SE MESE E' ULTIMO DEL RANGE
078600080319IF  2C**   DEPMES        IFEQ      PARMMF
078700950629     C                   EXSR      STRCLI
078800950629     C                   MOVEL     '1'           $STDAC
078900080319E   2C***                ENDIF
079000040212E   1C                   ENDIF
079100000000     C*
079200000000     C                   ENDSR
085400000000     C*------------------------------------------------------------------------*
085500000000     C* STAFIL - STAMPA TOTALI FILIALE
085600000000     C*------------------------------------------------------------------------*
085700000000     C     STAFIL        BEGSR
085800000000     C*
085900000000     C* SE NON CI SONO VOLUMI, NON STAMPO
086000040212IF  1C     TFVLB         IFEQ      *ZEROS
086100000000     C     TFVLC         ANDEQ     *ZEROS
086200040212X   1C                   ELSE
086300000000     C*
086400000000     C* DESCRIZIONE TOTALE
086500991102     C                   MOVEL     TES(10)       STDES1           22
086600000000     C*
086700000000     C* DECODIFICA LA FILIALE
086800080319     C****               EXSR      DECFIL
086900080319     C                   Z-ADD     1             I
087000080319     C     partfp        LOOKUP    FIL(I)                                 95
087100080319     C     *IN95         IFEQ      '0'
087200080319     C                   MOVEL     *ALL'*'       DESFIL           20
087300080319     C                   ELSE
087400080319     C                   MOVEL     DFIL(I)       DESFIL
087500080319     C                   END
087600000000     C                   MOVEL     DESFIL        STDES2           20
087700000000     C*
087800000000     C* IMPOSTO I VALORI DI STAMPA
087900941207     C                   Z-ADD     TFVLB         STVLB
088000000000     C                   Z-ADD     TFVLC         STVLC
088100991105     C                   Z-ADD     TFTCL         STTCL
088200991105     C                   Z-ADD     TFNCL         STNCL
088300000000     C                   Z-ADD     TFPKB         STPKB
088400000000     C                   Z-ADD     TFNSP         STNSP
089300991102     C*
089400991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
089500001009     C                   Z-ADD     TFVLB         WCPVLB           15 3
089600001009     C                   Z-ADD     TFVLC         WCPVLC           15 3
089700991102     C                   Z-ADD     *ZEROS        WCPRVA
089800991102     C                   EXSR      CALRNG
089900991102     C                   Z-ADD     WCPRVA        WRNG
090000000519     C                   Z-ADD     WRNG          STRNG             4 1
090100000517     C*
090200000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
090300001009     C                   Z-ADD     TFVLBT        WCPVLB           15 3
090400001009     C                   Z-ADD     TFVLCT        WCPVLC           15 3
090500000517     C                   Z-ADD     *ZEROS        WCPRVA
090600000517     C                   EXSR      CALRNG
090700000517     C                   Z-ADD     WCPRVA        WRNG
090800000519     C                   Z-ADD     WRNG          STTTP             4 1
090900991102     C*
091000991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
091100001009     C                   Z-ADD     TFTCL         WCPNCL           15 3
091200001009     C                   Z-ADD     TFNCR         WCPNCR           15 3
091300991102     C                   Z-ADD     *ZEROS        WCPPVA
091400991102     C                   EXSR      CALPER
091500991102     C                   Z-ADD     WCPPVA        WPER
091600000517     C                   Z-ADD     WPER          STPER             4 1
091700991102     C*
091701090709     c* se stampa per commerciale questo totale lo devo stampare a pagina
091702090709     c* nuova
091703090709     c                   if        parord='1'
091704090709     c                   setoff                                       01
091705090709     c                   except    testa
091706090709     c                   endif
094200000000     C* SE DETTAGLIO
094300000000IF  2C     PARDET        IFEQ      'D'
094400000000     C                   EXCEPT    RIGSEP
094500000000E   2C                   ENDIF
094600000000     C*
094700000000     C* STAMPO RIGA TOTALI
094800000000     C                   EXSR      STRTOT
094900000000     C*
095000000000     C* SE DETTAGLIO
095100000000IF  2C     PARDET        IFEQ      'D'
095200000000     C                   EXCEPT    RIGSEP
095300000000E   2C                   ENDIF
095400040212E   1C                   ENDIF
095500000000     C*
095600000000     C                   ENDSR
095601090709     C*------------------------------------------------------------------------*
095602090709     C* STAcom - STAMPA TOTALI commerciale unificante
095603090709     C*------------------------------------------------------------------------*
095604090709     C     STAcom        BEGSR
095605090709     C*
095606090709     C* SE NON CI SONO VOLUMI, NON STAMPO
095607090709IF  1C     TkVLB         IFEQ      *ZEROS
095608090709     C     TkVLC         ANDEQ     *ZEROS
095609090709X   1C                   ELSE
095610090709     c                   setoff                                       21
095611090709     C                   MOVEL     TES(14)       STDES1           22
095612090709     c                   movel     descun        stdes2
095613090709     C*
095614090709     C* IMPOSTO I VALORI DI STAMPA
095615090709     C                   Z-ADD     TkVLB         STVLB
095616090709     C                   Z-ADD     TkVLC         STVLC
095617090709     C                   Z-ADD     TkTCL         STTCL
095618090709     C                   Z-ADD     TkNCL         STNCL
095619090709     C                   Z-ADD     TkPKB         STPKB
095620090709     C                   Z-ADD     TkNSP         STNSP
095621090709     C*
095622090709     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
095623090709     C                   Z-ADD     TkVLB         WCPVLB           15 3
095624090709     C                   Z-ADD     TkVLC         WCPVLC           15 3
095625090709     C                   Z-ADD     *ZEROS        WCPRVA
095626090709     C                   EXSR      CALRNG
095627090709     C                   Z-ADD     WCPRVA        WRNG
095628090709     C                   Z-ADD     WRNG          STRNG             4 1
095629090709     C*
095630090709     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
095631090709     C                   Z-ADD     TkVLBT        WCPVLB           15 3
095632090709     C                   Z-ADD     TkVLCT        WCPVLC           15 3
095633090709     C                   Z-ADD     *ZEROS        WCPRVA
095634090709     C                   EXSR      CALRNG
095635090709     C                   Z-ADD     WCPRVA        WRNG
095636090709     C                   Z-ADD     WRNG          STTTP             4 1
095637090709     C*
095638090709     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
095639090709     C                   Z-ADD     TkTCL         WCPNCL           15 3
095640090709     C                   Z-ADD     TkNCR         WCPNCR           15 3
095641090709     C                   Z-ADD     *ZEROS        WCPPVA
095642090709     C                   EXSR      CALPER
095643090709     C                   Z-ADD     WCPPVA        WPER
095644090709     C                   Z-ADD     WPER          STPER             4 1
095645090709     C*
095648090709     C* SE DETTAGLIO
095649090709IF  2C     PARDET        IFEQ      'D'
095650090709     C                   EXCEPT    RIGSEP
095651090709E   2C                   ENDIF
095652090709     C*
095653090709     C* STAMPO RIGA TOTALI
095654090709     C                   EXSR      STRTOT
095655090709     C*
095656090709     C* SE DETTAGLIO
095657090709IF  2C     PARDET        IFEQ      'D'
095658090709     C                   EXCEPT    RIGSEP
095659090709E   2C                   ENDIF
095660090709     c                   endif
095661090709     C*
095662090709     C                   ENDSR
125900000000     C*------------------------------------------------------------------------*
126000000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
126100000000     C*------------------------------------------------------------------------*
126200000000     C     STRCLI        BEGSR
126300000000     C*--------------------
126400000000     C* T E S T A T A
126500000000     C*--------------------
126600000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
126700000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
126800000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
126900000000     C*         RIGHE DI TOTALE
127000000000     C*
127100950629     C* RECUPERA CALCOLO ATTUALE DEL VOLUME E CALCOLA IL PREVISTO
127200000000     C                   EXSR      RECCAL
127300991105 MOD C     PARDET        IFEQ      'D'
127400000000     C     *INOF         IFEQ      '1'
127500000000     C                   SETON                                        01
127600000000     C                   EXCEPT    RIGSEP
127700000000     C                   EXCEPT    TESTA
127800000000     C                   SETOFF                                       OF
127900000718     C                   ENDIF
128000950629     C*
128100040219     C* SE HA GIA' STAMPATO UNA RIGA HA GIA' STAMPATO
128200950629     C* ANCHE I DATI DEL CLIENTE, PER CUI PER LA SECONDA RIGA LI PULISCE
128300950629IF  1C     $STDAC        IFEQ      '1'
128400950629     C                   Z-ADD     *ZEROS        DEPKSC
128500030909     C                   MOVEL     *BLANKS       DESCLI           29
128600950629     C                   Z-ADD     *ZEROS        DEPCTR
128700950629     C                   MOVEL     *BLANKS       DESCTR
128800950629     C                   MOVEL     *BLANKS       DESDCV
128900950629     C                   Z-ADD     *ZEROS        STCAK
129000950629     C                   Z-ADD     *ZEROS        STCAC
129100950629     C                   Z-ADD     *ZEROS        STCIK
129200950629     C                   Z-ADD     *ZEROS        STCIC
129300950629E   1C                   ENDIF
129400000000     C*
129500000000     C* STAMPO RIGA CLIENTE/TARIFFA
129600000000     C                   EXCEPT    RIGCLI
129700991105 MOD C                   ENDIF
129800991102     C                   Z-ADD     1             FLGCLI
129900000000     C*
130000000000     C                   ENDSR
133100000000     C*------------------------------------------------------------------------*
133200000000     C* STRTOT - STAMPA RIGA TOTALE
133300000000     C*------------------------------------------------------------------------*
133400000000     C     STRTOT        BEGSR
133500000000     C*
133600000000     C* STAMPO RIGA CLIENTE/TARIFFA
133700000000     C                   EXCEPT    RIGTOT
133900000000     C*
134000000000     C                   ENDSR
134100000000     C*------------------------------------------------------------------------*
134200000000     C* DECDIV - DECODIFICA DIVISIONE
134300000000     C*------------------------------------------------------------------------*
134400000000     C     DECDIV        BEGSR
134500000000     C*
134600020919     C                   MOVEL     1             KTBKUT
134700000000     C                   MOVE      '17'          KTBCOD
134800000000     C                   MOVE      *BLANKS       KTBKEY
134900000000     C                   MOVEL     DEPDIV        KTBKEY
135000000000     C     KEYTAB        CHAIN     TABEL                              99
135100000000     C     *IN99         IFEQ      '1'
135200000000     C     TBLFLG        OREQ      '*'
135300000000     C                   MOVEL     *ALL'*'       DESDIV           20
135400000000     C                   ELSE
135500000000     C                   MOVEL     TBLUNI        DESDIV
135600000000     C                   END
135700000000     C*
135800000000     C                   ENDSR
135900000000     C*------------------------------------------------------------------------*
136000000000     C* DECARE - DECODIFICA AREA
136100000000     C*------------------------------------------------------------------------*
136200000000     C     DECARE        BEGSR
136300000000     C*
136400020919     C                   MOVEL     1             KTBKUT
136500000000     C                   MOVE      '05'          KTBCOD
136600000000     C                   MOVE      *BLANKS       KTBKEY
136700000000     C                   MOVEL     DEPARE        KTBKEY
136800000000     C     KEYTAB        CHAIN     TABEL                              99
136900000000     C     *IN99         IFEQ      '1'
137000000000     C     TBLFLG        OREQ      '*'
137100000000     C                   MOVEL     *ALL'*'       DESARE           20
137200000000     C                   ELSE
137300000000     C                   MOVEL     TBLUNI        DESARE
137400000000     C                   END
137500000000     C*
137600000000     C                   ENDSR
137601090709     C*------------------------------------------------------------------------*
137602090709     C* DECcun - DECODIFICA commerciale unificante
137603090709     C*------------------------------------------------------------------------*
137604090709     C     DECcun        BEGSR
137605090709     C*
137606090709     C                   MOVEL     1             KTBKUT
137607090709     C                   MOVE      '01'          KTBCOD
137608090709     C                   MOVE      *BLANKS       KTBKEY
137609090709     C                   MOVEL     DEPcun        KTBKEY
137610090709     C     KEYTAB        CHAIN     TABEL                              97
137611090709     C     *IN97         IFEQ      '1'
137612090709     C     TBLFLG        OREQ      '*'
137613090709     C                   MOVEL     *ALL'*'       DEScun           25
137614090709     C                   ELSE
137615090709     C                   MOVEL     TBLUNI        Ds01
137616090709     C                   MOVEL     §01age        descun
137617090709     C                   END
137618090709     C*
137619090709     C                   ENDSR
137700000000     C*------------------------------------------------------------------------*
137800000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
137900000000     C*------------------------------------------------------------------------*
138000000000     C     DECFIL        BEGSR
138100000000     C*
138200000000     C                   Z-ADD     1             I
138300000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
138400000000     C     *IN99         IFEQ      '0'
138500000000     C                   MOVEL     *ALL'*'       DESFIL           20
138600000000     C                   ELSE
138700000000     C                   MOVEL     DFIL(I)       DESFIL
138800000000     C                   END
138900000000     C*
139000000000     C                   ENDSR
139100000000     C*------------------------------------------------------------------------*
139200000000     C* DECCTR - DECODIFICA TARIFFA
139300000000     C*------------------------------------------------------------------------*
139400000000     C     DECCTR        BEGSR
139500980421     C*
139600980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
139700980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
139800980421IF  1C     DEPCTR        IFEQ      999
139900991104     C                   MOVEL     DEC(1)        DESCTR           10
140000980421X   1C                   ELSE
140100980421IF  2C     DEPCTR        IFEQ      888
140200980421     C                   MOVEL     DEC(2)        DESCTR
140300980421X   2C                   ELSE
140400020919     C                   MOVEL     1             KTBKUT
140500000000     C                   MOVE      'TR'          KTBCOD
140600000000     C                   MOVE      *BLANKS       KTBKEY
140700000000     C                   MOVEL     DEPCTR        WTEM01            1
140800000000     C                   MOVEL     WTEM01        KTBKEY
140900000000     C     KEYTAB        CHAIN     TABEL                              99
141000980421IF  3C     *IN99         IFEQ      '1'
141100000000     C     TBLFLG        OREQ      '*'
141200980421     C                   MOVEL     *ALL'*'       DESCTR
141300980421X   3C                   ELSE
141400000000     C                   MOVEL     TBLUNI        DESCTR
141500980421E   3C                   ENDIF
141600980421E   2C                   ENDIF
141700980421E   1C                   ENDIF
141800000000     C*
141900000000     C                   ENDSR
142000991102     C*------------------------------------------------------------------------*
142100991102     C* CALPER - CALCOLA PERCENTUALE RAPPORTO COLLI
142200991102     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
142300991102     C*          - COLLI CML               (WCPNCR)
142400991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
142500991102     C*------------------------------------------------------------------------*
142600991102     C     CALPER        BEGSR
142700991102     C*
142800991102     C* ===> CALCOLA PERCENTUALE COLLI CML SU COLLI BOLL.
142900991102     C                   Z-ADD     *ZEROS        WCPPVA            6 2
143000991102     C     WCPNCL        IFNE      WCPNCR
143100991102     C     WCPNCL        ANDGT     *ZEROS
143200991102     C     WCPNCR        ANDGT     *ZEROS
143300001009     C     WCPNCR        MULT      100           WMULTI           18 3
143400001009     C     WMULTI        DIV       WCPNCL        WRKPER           20 3
143500991102     C     WRKPER        IFGT      9999
143600991102     C                   Z-ADD     9999,99       WCPPVA
143700991102     C                   ELSE
143800991102     C                   Z-ADD     WRKPER        WCPPVA
143900991102     C                   END
144000991102     C                   END
144100991102     C*
144200991102     C     WCPNCL        IFEQ      WCPNCR
144300991102     C     WCPNCL        ANDGT     *ZEROS
144400991102     C     WCPNCR        ANDGT     *ZEROS
144500991102     C                   Z-ADD     100           WCPPVA
144600991102     C                   ENDIF
144700991102     C*
144800000000     C                   ENDSR
144900991029     C*------------------------------------------------------------------------*
145000991102     C* CALRNG - CALCOLA PERCENTUALE SCOSTAMENTO VOLUMI
145100991102     C* INPUT:   - VOLUME BOLLETTAT0         (WCPVLB)
145200991102     C*          - VOLUME VDL                (WCPVLC)
145300991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA    (WCPRVA)
145400991029     C*------------------------------------------------------------------------*
145500991102     C     CALRNG        BEGSR
145600991029     C*
145700991229     C* ===> CALCOLA PERCENTUALE VOL CML SU VOL BOLL.
145800991102     C                   Z-ADD     *ZEROS        WCPRVA            6 2
145900991102     C     WCPVLC        IFNE      WCPVLB
146000991102     C     WCPVLC        ANDGT     *ZEROS
146100991102     C     WCPVLB        ANDGT     *ZEROS
146200001009     C     WCPVLB        SUB       WCPVLC        WDIFFE           18 3
146300001009     C     WDIFFE        MULT      100           WRKPER           20 3
146400991102     C     WRKPER        DIV       WCPVLB        WRKPER
146500991102     C     WRKPER        IFGT      9999
146600991102     C                   Z-ADD     9999,99       WCPRVA
146700991102     C                   ELSE
146800991102     C                   Z-ADD     WRKPER        WCPRVA
146900991102     C                   END
147000991102     C                   END
147100991104 OLD C*
147200991104  "  C* SPECIFICA LOGICAMENTE ERRATA MA INSERITA X MANTENERE UNIFORMI
147300991104  "  C* I CRITERI DI VALUTAZIONE UTENTE RISPETTO ALLA VECCHIA
147400991104  "  C* STATISTICA VOLUMI
147500991104  "  C*                    MULT -1        WCPRVA
147600991104     C*
147700991102     C     WCPVLC        IFEQ      WCPVLB
147800991102     C     WCPVLC        ANDGT     *ZEROS
147900991102     C     WCPVLB        ANDGT     *ZEROS
148000991102     C                   Z-ADD     100           WCPRVA
148100991102     C                   ENDIF
148200991102     C*
148300991102     C                   ENDSR
148400000000     C*------------------------------------------------------------------------*
148500000000     C* RECCAL - RECUPERA CALCOLO ATTUALE VOLUME E CALCOLA IL CONSIGLIATO
148600000000     C*------------------------------------------------------------------------*
148700000000     C     RECCAL        BEGSR
148800000000     C*
148900980421     C* AZZERA I DATI IN STAMPA
149000000000     C                   Z-ADD     *ZEROS        STCAK             5 1
149100000000     C                   Z-ADD     *ZEROS        STCAC             6 3
149200000000     C                   Z-ADD     *ZEROS        STCIK             5 1
149300000000     C                   Z-ADD     *ZEROS        STCIC             6 3
149400991104     C*                    Z-ADD*ZERO     STKPM   51
149500030909     C                   MOVEL     *BLANKS       DESDCV           12
149600030909     C                   MOVEL     *BLANKS       DESTTR            3
149700980421     C*
149800980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
149900980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI,
150000980421     C* IN QUESTI CASI SALTA LA LETTURA DELLE TARIFFE PERCHE' INESISTENTI
150100980421     C     DEPCTR        IFNE      999
150200980421     C     DEPCTR        ANDNE     888
150300000000     C*
150400980402     C* ======> CERCA IL VALORE ATTUALE DI CALCOLO E ALTRI DATI
150500000000     C                   Z-ADD     DEPKSC        KTFKSC
150600000000     C                   Z-ADD     DEPCTR        KTFCTR
150700000000     C                   Z-ADD     *ZEROS        KTFPRG
150800010314     C     KEYTAM        SETLL     TNTAM01L                           99
150900010314     C  N99              READ      TNTAM01L                               99
151000000000     C     *IN99         DOWEQ     '0'
151100000000     C     TAMKSC        ANDEQ     KTFKSC
151200000000     C     TAMCTR        ANDEQ     KTFCTR
151300000000     C     TAMATB        IFEQ      ' '
151400000000     C     TAMTIN        IFEQ      'K'
151500000000     C                   Z-ADD     TAMKPM        STCAK
151600000000     C                   Z-ADD     *ZEROS        STCAC
151700000000     C                   ELSE
151800000000     C     TAMKPM        DIV       100           STCAC
151900000000     C                   Z-ADD     *ZEROS        STCAK
152000000000     C                   END
152100980421     C                   MOVEL     TAMDCV        DESDCV
152200030909     C* REPERISCO IL NETWORK DELLA TARIFFA
152300030909     C                   MOVEL(P)  TAMFLO        DSTA01
152400030909     C                   IF        TAMFIE = 'I' AND §TADPD <> 'S'
152500030909     C                   EVAL      DESTTR = 'ITA'
152600030909     C                   ENDIF
152700030909     C                   IF        TAMFIE = 'I' AND §TADPD  = 'S'
152800030909     C                   EVAL      DESTTR = 'DPD'
152900030909     C                   ENDIF
153000030909     C                   IF        TAMFIE = 'E' AND §TAFED <> 'S'
153100030909     C                   EVAL      DESTTR = 'EEX'
153200030909     C                   ENDIF
153300030909     C                   IF        TAMFIE = 'E' AND §TAFED  = 'S'
153400030909     C                   EVAL      DESTTR = 'FED'
153500030909     C                   ENDIF
153600030909     C*
153700000000     C                   Z-ADD     TAMPRG        WPRG              3 0
153800000000     C                   END
153900010314     C                   READ      TNTAM01L                               99
154000000000     C                   END
154100000000     C*
154200000000     C* ====> IN BASE ALLA PERCENTUALE TROVATA INDICA IL VALORE DI CALCOLO OK
154300000000     C* KG/MC
154400000000     C*                             VALORE ATTUALE * VOLUME AUTOMATICO
154500000000     C*          VALORE PROPOSTO = ------------------------------------
154600000000     C*                                       VOLUME CML
154700000000     C*
154800000000     C     STCAK         IFNE      *ZEROS
154900001009     C     STVLC         ANDNE     *ZEROS
155000000000     C     STPKB         DIV(H)    STVLC         STCIK
155100000000     C                   END
155200000000     C* MC/COLLI
155300000000     C*                             VALORE ATTUALE * VOLUME CML
155400000000     C*          VALORE PROPOSTO = -----------------------------
155500000000     C*                                  VOLUME AUTOMATICO
155600000000     C*
155700000000     C     STCAC         IFNE      *ZEROS
155800001009     C     STNCL         ANDNE     *ZEROS
155900000000     C     STVLC         DIV(H)    STNCL         STCIC
156000000000     C                   END
156100980421     C                   ENDIF
156200000000     C*
156300000000     C                   ENDSR
159301090513     C*------------------------------------------------------------------------*
159302090709     C* ordinamento per commerciale unificante
159303090513     C*------------------------------------------------------------------------*
159304090709     C     sr_ordcomset  BEGSR
159305090513      /free
159306090709       exec sql declare WVLMcsr cursor for Select * from wfvlmf0f
159307090709        order by wfvann, wfvdiv, wfvare, wfvfil, wfvcun, wfvrag, wfvksc, wfvctr,
159308090709         wfvmes;
159309090513       exec sql open WVLMcsr;
159310090513      /end-free
159311090513     c                   endsr
159312090513     C*------------------------------------------------------------------------*
159313090709     C* ordinamento per commerciale unificante
159314090513     C*------------------------------------------------------------------------*
159315090709     C     sr_ordcomlet  BEGSR
159316090513      /free
159317090513       wrecok='N';
159318090513       wrecokc='N';
159319090513       exec sql Fetch WVLMcsr into :wfvlmds;
159320090513         if sqlcod=100 or sqlcod<0;
159321090513            finl00='1';
159322090513         else;
159323090513            exsr chkrec;
159324090513         endif;
159325090513      /end-free
159326090513     c                   endsr
159327090709     C*------------------------------------------------------------------------*
159328090709     C* elaborazione e stampa per commerciale unificante
159329090709     C*------------------------------------------------------------------------*
159330090709     C     sr_elabcom    BEGSR
159331090709     c* solo la prima volta
159332090709     c                   if        w_primav=*off
159333090709     c                   exsr      newdiv
159334090709     c                   exsr      neware
159335090709     c                   exsr      newfil
159336090709     c                   eval      w_primav=*on
159337090709     c                   endif
159338090710    1c     finl00        doweq     '0'
159339090709     c* operazioni per nuovo commerciale unificante
159340090709     c                   exsr      newcom
159341090710    2c     finl00        doweq     '0'
159342090709     c     wfvcun        andeq     depcun
159343090709     c* operazioni per nuovo cliente/tariffa
159344090709     c                   exsr      newcli
159352090710    3c     finl00        doweq     '0'
159353090709     c     wfvcun        andeq     depcun
159354090709     c     wfvksc        andeq     depksc
159355090709     c     wfvctr        andeq     depctr
159356090709     C* Memorizza le spedizioni
159357090710    4C     WRECOK        IFEQ      'S'
159358090709     C                   exsr      memspe
159359090710    4C                   ENDIF
159360090709     c* leggo il record successivo
159361090709     c                   exsr      sr_ordcomlet
159362090710    3c                   enddo
159363090709     c* stampa totale cliente/tariffa
159364090709     c                   exsr      stacli
159365090710    2c                   enddo
159366090709     c* stampa totale commerciale unificante
159367090709     c                   exsr      stacom
159368090710    1c                   enddo
159369090709     c                   exsr      stafil
159370090709     C* STAMPA RIGA FINE STAMPA
159371090709     C                   EXCEPT    stafin
159374090709     c                   endsr
159400020919     C*-------------------------------------------------------------------------
159500020919     C* REPERISCE I DATI UTENTE
159600020919     C*-------------------------------------------------------------------------
159700020919     C     REPDATIUTE    BEGSR
159800020919     C*
159900020919     C* INIZIALIZZA VARIABILI DI WRK
160000020919     C                   CLEAR                   TIBS34DS
160100020919     C                   CLEAR                   AZUTEDS
160200020919     C                   CLEAR                   DDATIUTE
160300020919     C*
160400020919     C     *DTAARA       DEFINE    §azute        azuteds
160500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
160600020919     C                   IN(E)     *DTAARA
160700020919if  1C                   IF        %Error
160800020919     c                   EVAL      I34Tla = 'L'
160900020919     C                   CALL      'TIBS34R'
161000020919     C                   PARM                    Tibs34Ds
161100020919     C                   IN        *DTAARA
161200020919e   1C                   ENDIF
161300020919     C*
161400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
161500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
161600020919     C*
161700020919     C                   ENDSR
161800000000     C*------------------------------------------------------------------------*
161900000000     C* *INZSR - ROUTINE INIZIALE
162000000000     C*------------------------------------------------------------------------*
162100000000     C     *INZSR        BEGSR
162200000000     C*--------------------
162300000000     C* RICEVIMENTO PARAMENTRI
162400000000     C*--------------------
162500000000     C     *ENTRY        PLIST
162600000000     C                   PARM                    KPJBA
162700950703     C                   MOVEL     KPJBU         PARAM
162800000000     C*--------------------
162900000000     C* CAMPI RIFERITI AL DATA BASE
163000000000     C*--------------------
163100980402     C     *LIKE         DEFINE    TBLKUT        KTBKUT
163200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
163300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
163400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
163500000000     C     *LIKE         DEFINE    TAMKSC        KTFKSC
163600000000     C     *LIKE         DEFINE    TAMCTR        KTFCTR
163700000000     C     *LIKE         DEFINE    TAMPRG        KTFPRG
163800000000     C     *LIKE         DEFINE    WFVANN        KWVANN
163900000000     C     *LIKE         DEFINE    WFVDIV        KWVDIV
164000000000     C     *LIKE         DEFINE    WFVARE        KWVARE
164100000000     C     *LIKE         DEFINE    WFVFIL        KWVFIL
164200991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
164300000000     C     *LIKE         DEFINE    WFVKSC        KWVKSC
164400000000     C     *LIKE         DEFINE    WFVCTR        KWVCTR
164500000000     C     *LIKE         DEFINE    WFVMES        KWVMES
164600950629     C     *LIKE         DEFINE    WFVFVB        KWVFVB
164700000000     C     *LIKE         DEFINE    WFVDIV        DEPDIV
164800000000     C     *LIKE         DEFINE    WFVARE        DEPARE
164900000000     C     *LIKE         DEFINE    WFVFIL        DEPFIL
165000020910     C     *LIKE         DEFINE    WFVFIL        DEPTFP
165100000000     C     *LIKE         DEFINE    WFVKSC        DEPKSC
165200000000     C     *LIKE         DEFINE    WFVCTR        DEPCTR
165300991029     C     *LIKE         DEFINE    WFVMES        DEPMES
165400000000     C     *LIKE         DEFINE    TAMKSC        WKSC
165500000000     C     *LIKE         DEFINE    TAMCTR        WCTR
165501090709     C     *LIKE         DEFINE    WFVcun        DEPcun
165502090710     C     *LIKE         DEFINE    WFVcun        DEPcun_p
165600000000     C*--------------------
165700000000     C* CHIAVI DI LETTURA
165800000000     C*--------------------
165900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
166000000000     C     KEYTAB        KLIST
166100980402     C                   KFLD                    KTBKUT
166200000000     C                   KFLD                    KTBCOD
166300000000     C                   KFLD                    KTBKEY
166400000000     C*
166500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
166600000000     C     KEYORG        KLIST
166700000000     C                   KFLD                    KORFIL
166800000000     C*
166900991104     C* CHIAVE LETTURA WFVLM01L - COMPLETA
167000000000     C     KEYWFV        KLIST
167100000000     C                   KFLD                    KWVANN
167200000000     C                   KFLD                    KWVDIV
167300000000     C                   KFLD                    KWVARE
167400000000     C                   KFLD                    KWVFIL
167500991029     C                   KFLD                    KWVRAG
167600000000     C                   KFLD                    KWVKSC
167700000000     C                   KFLD                    KWVCTR
167800000000     C                   KFLD                    KWVMES
167900950629     C                   KFLD                    KWVFVB
168000000000     C*
168100980407     C* CHIAVE LETTURA TNTAM31C - COMPLETA
168200000000     C     KEYTAM        KLIST
168300000000     C                   KFLD                    KTFKSC
168400000000     C                   KFLD                    KTFCTR
168500000000     C                   KFLD                    KTFPRG
168600000000     C*
168700980407     C* CHIAVE LETTURA TNTAM31C - PARZIALE
168800000000     C     KE1TAM        KLIST
168900000000     C                   KFLD                    KTFKSC
169000000000     C                   KFLD                    KTFCTR
169100020910     C*--------------------
169200020910     C* REPERISCE DATA/ORA CORRENTE
169300020910     C*--------------------
169400020910     C                   TIME                    N14              14 0
169500020910     C                   MOVE      N14           G08DAT            8 0
169600020910     C                   Z-ADD     *ZEROS        G08INV
169700020910     C                   MOVEL     '0'           G08ERR
169800020910     C                   CALL      'XSRDA8'
169900020910     C                   PARM                    WLBDA8
170000020910     C                   Z-ADD     G08INV        DATCOR            8 0
170100000000     C*--------------------
170200000000     C* CAMPI DI CONTROLLO
170300000000     C*--------------------
170400000000     C                   MOVEL     '0'           FINL00            1
170500000000     C                   SETON                                        OF
170600000000     C                   Z-ADD     UDATE         DATA              6 0
170700000000     C                   TIME                    ORA               6 0
170800040211     C*---               MOVEL     *BLANKS       $REA              1
170900950629     C                   MOVEL     '0'           $STDAC            1
171000991229     C*
171100991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
171200991229     C* PER FACILITARE I CONTROLLI
171300991229     C                   EVAL      PARMIN=%abs(PARMIN)
171400950629     C*
171500000000     C* LANCIO ROUTINE PER VERIFICARE SE
171600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
171700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
171800020919     C                   EXSR      REPDATIUTE
171900000000     C*
172000000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
172100000000IF  1C     PARDET        IFEQ      'D'
172200000000     C                   MOVEL     INT(1)        DECPAR            9
172300000000X   1C                   ELSE
172400000000     C                   MOVEL     INT(2)        DECPAR
172500000000E   1C                   ENDIF
172600000706     C*
172700000706     C* IMPOSTA LA DESCRIZIONE SE LANCIO POSTE OPPURE NO
172800080319IF  1C***  PARPOS        IFEQ      'S'
172900080319     C***                MOVEL     DEC(5)        DECPOS           10
173000080319X   1C***                ELSE
173100080319     C***                MOVEL     DEC(6)        DECPOS
173200080319E   1C***                ENDIF
173300030911     C*
173400030911     C* IMPOSTA L'ELENCOI DEI NETWORK TARIFFE RICHIESTI NEL LANCIO
173500030911     C                   MOVEL     *blanks       DECNTW           15
173600080319IF  1C***  PARNT1        IFNE      *blanks
173700080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT1 + ' '
173800080319E   1C***                ENDIF
173900080319IF  1C***  PARNT2        IFNE      *blanks
174000080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT2 + ' '
174100080319E   1C***                ENDIF
174200080319IF  1C***  PARNT3        IFNE      *blanks
174300080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT3 + ' '
174400080319E   1C***                ENDIF
174500080319IF  1C***  PARNT4        IFNE      *blanks
174600080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT4 + ' '
174700080319E   1C***                ENDIF
174800080319     C***                IF        DECNTW = *blanks
174900080319     C***                EVAL      DECNTW = 'TUTTI'
175000080319E   1C***                ENDIF
175100000000     C*
175200000000     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
175300080319IF  1C***  PARDET        IFEQ      'S'
175400080319     C***                MOVEL     'S'           PARTOT
175500080319E   1C***                ENDIF
175600000000     C*
175700000000     C* CARICA ORGANIGRAMMA
175800000000     C                   EXSR      CARORG
175900000000     C*
176000000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
176100000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
176200080319     C**   PARDIV        IFEQ      *BLANKS
176300080319     C**   PARARE        ANDEQ     *ZEROS
176400080319     C**   PARFIL        ANDEQ     *ZEROS
176500000000     C                   SETOFF                                       02
176600080319     C**                 ELSE
176700080319     C**                 SETON                                        02
176800080319     C**                 END
176801090513     c                   if        parord='1'
176802090709     c                   eval      orddes='Comm.le unificante'
176803090513     c                   else
176804090709     c                   eval      orddes='Ragione Sociale   '
176805090513     c                   endif
176806090709     c                   clear                   decflp
176807090709     c                   setoff                                       04
176808090709     c                   if        parflp>0
176809090709     c                   seton                                        04
176810090709     c     parflp        chain     azorg01l
176811090709     c                   if        %found(azorg01l)
176812090709     C                   MOVEL     ORGDES        decflp
176813090709     c                   endif
176814090709     c                   endif
176900000000     C*
177000000000     C* SE LANCIO PER SALDI INTESTA STAMPE CON ANNO/MESE (IND.03 OFF)
177100000000     C* SE LANCIO ESTEMPORANEO INTESTA CON DATA INIZIO E FINE (IND.03 ON)
177200080319     C     PARTLA        IFEQ      'S'
177300080319     C                   SETOFF                                       03
177400080319     C                   END
177500080319     C     PARTLA        IFEQ      'E'
177600080319     C                   SETON                                        03
177700080319     C                   END
177800080319     c* inverto data spedizione dal/al in giorno/mese/anno per la stampa
177900080319     c                   move      pardal        dataiso
178000080319     c                   move      dataiso       dataeur
178100080319     c                   move      dataeur       pardal_E
178200080319     c                   move      paral         dataiso
178300080319     c                   move      dataiso       dataeur
178400080319     c                   move      dataeur       paral_E
178500000000     C*
178600000000     C                   ENDSR
178700000000     C*------------------------------------------------------------------------*
178800000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
178900000000     C*------------------------------------------------------------------------*
179000000000     C     CARORG        BEGSR
179100000000     C*
179200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
179300000000     C                   Z-ADD     *ZEROS        KORFIL
179400000000     C     KEYORG        SETLL     AZORG01L                           99
179500000000     C  N99              READ      AZORG01L                               99
179600000000     C                   Z-ADD     0             I                 3 0
179700000000     C     *IN99         DOWEQ     '0'
179800000000     C     ORGFVA        IFEQ      *BLANKS
179900000000     C     ORGFAG        IFNE      'V'
180000000000     C                   ADD       1             I
180100000000     C                   Z-ADD     ORGFIL        FIL(I)
180200000000     C                   MOVEL     ORGDES        DFIL(I)
180300000706     C                   MOVEL     ORGDE3        OG143
180400020627     C                   MOVEL     §OGNTW        PFIL(I)
180500000000     C                   END
180600000000     C                   END
180700000000     C                   READ      AZORG01L                               99
180800000000     C                   END
180900000000     C*
181000000000     C                   ENDSR
181100000000     O*------------------------------------------------------------------------*
181200000000     OPRTF198   E            TESTA          1 02
181300080319     O                                            8 'FNLSG3R '
181400950706     O                                           90 '***    STATISTICA   '
181500950706     O                                          110 'V O L U M I    ***  '
181600980403     O                       DECPAR             120
181700980421     O                                          150 'Data'
181800000000     O                       DATA               159 '  /  /  '
181900980421     O                                          169 'Ora'
182000980421     O                       ORA                178 '  :  :  '
182100980421     O                                          193 'Pag.'
182200000000     O                       PAGE          Z    198
182300000000     O*------------------------
182400000000     O          E   N03      TESTA          1
182500080319     O****                                       17 'Network tariffe: '
182600080319     O****                   DECNTW              32
182700080319     O***                                        83 'ANNO:'
182800080319     O***                    PARANN              88
182900080319     O***                                        97 'MESI:'
183000080319     O****                   PARMMI             100
183100080319     O***                                       102 '/'
183200080319     O****                   PARMMF             105
183300080319     O***+                   DECPOS             198
183400080319     O                                           85 'DATA Spedizione dal'
183500080319     O                       PARdal_E      8  +   1
183600080319     O                                        +   2 'al'
183700080319     O                       PARal_E       8  +   1
183800000000     O*------------------------
183900080319     O***       E    02      TESTA       1  1
184000080319     O***                                        20 'PARZIALIZZAZIONI:   '
184100080319     O***                                        40 '             DIVIS.:'
184200080319     O***                    PARDIV              41
184300080319     O***                                        61 '               AREA:'
184400080319     O***                    PARARE        Z     64
184500080319     O***                                        84 '            P.OPER.:'
184600080319     O***                    PARFIL        Z     87
184601090513     O          E            TESTA       1  1
184602090513     O                                           20 'ORDINAMENTO: per:'
184603090513     O                       orddes           +   1
184604090709     o               04                       +   4 'SOLO FILIALE DI PARTENZA'
184605090709     o               04      parflp           +   1
184606090709     o               04      decflp           +   1
184700000000     O*------------------------
184800991102     O          E            TESTA       1  1
184900000000     O                       TES(1)              66
185000000000     O                       TES(1)             132
185100991229     O                       TES(1)             198
185200991229     O*------------------------
185300991229     O          E            TESTA          1
185400080319     O                                           10 '! TER.PAR:'
185500991229     O                       DESFIL            +  1
185600991229     O                                         +  1 '   AREA:'
185700991229     O                       DESARE            +  1
185800991229     O                                         +  1 '   DISTRETTO:'
185900991229     O                       DESDIV            +  1
186000991229     O                                         +  1 '   SCOST. (A) DA: '
186100991229     O                       PARRG1        N   +  1
186200991229     O                                         +  1 '%'
186300991229     O                                         +  1 '   A:'
186400991229     O                       PARRG2        N   +  1
186500991229     O                                         +  1 '%'
186600991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
186700991229     O                       PARPID        N   +  1
186800991229     O                                         +  1 '%'
186900991229     O                                         +  1 ' SENZA IMP'
187000991229     O                       PARPSD        N   +  1
187100991229     O                                         +  1 '%'
187200991229     O                                          198 '!'
187300991229     O*-------------------------                                       --------*
187400991229     O          E            TESTA          1
187500991229     O                                            1 '!'
187600991229     O                       CHKCML            + 10
187700991229     O                                          115 'MIN.   (A)   :±'
187800991229     O                       PARMIN        N   +  0
187900991229     O                                         +  1 '%'
188000991229     O                                          198 '!'
188100991102     O*------------------------
188200991102     O          E            TESTA          1
188300991102     O                       TES(1)              66
188400991102     O                       TES(1)             132
188500991102     O                       TES(1)             198
188600000000     O*------------------------
188700000000     O          E            TESTA          1
188800000000     O                       TES(2)              66
188900000000     O                       TES(3)             132
189000000000     O                       TES(4)             198
189100000000     O               01                         175 'VALORE ATTUALE '
189200000000     O               01                         196 'VALORE PROPOSTO'
189300000000     O*------------------------
189400000000     O          E            TESTA          1
189500000000     O                       TES(5)              66
189600000000     O               01                           9 'CLIENTE'
189700000000     O               01                          50 'TARIFFA'
189800000000     O                       TES(6)             132
189900000000     O                       TES(7)             198
190000000000     O               01                         175 'KG/MC    MC/COL'
190100000000     O               01                         196 ' KG/MC   MC/COL'
190200000000     O*------------------------
190300000000     O          E            TESTA          1
190400000000     O                       TES(1)              66
190500000000     O                       TES(1)             132
190600000000     O                       TES(1)             198
190601090709     O          E            rigcom         0
190602090709     O                                        +   3 'Commerciale unificante'
190603090709     o                       depcun              34
190604090709     o                       descun              60
190605090709     O          E            rigcom         1
190606090709     o                       depcun              34
190607090709     o                       descun              60
190608090709     O          E            rigcom         1
190609090709     O                       TES(1)              66
190610090709     O                       TES(1)             132
190611090709     O                       TES(1)             198
190700000000     O*------------------------------------------------------------------------*
190800000000     O          E            RIGCLI         1
190900000000     O                                            1 '!'
191000950629     O                       DEPKSC        Z   +  1
191100000000     O                       DESCLI            +  1
191200030909     O                       DESTTR            +  1
191300980421     O                       DEPCTR            +  1
191400991104     O                       DESCTR            +  1
191500991104     O                       DESDCV            +  1
191600080319     O***                                      +  2 '!'
191700080319     O***                    PARMMF            +  1
191800080319     O***                                      +  1 '!'
191900000517     O                       STPER         K     87
192000000517     O                       STNCL         Z     96
192100000517     O                       STPKB         4    108
192200000517     O                       STNSP         Z    117
192300000517     O                                          119 '!'
192400000517     O                       STVLB         4    131
192500000517     O                       STVLC         4    143
192600000519     O                       STRNG         K    152
192700000718     O*                      STTTP         K    159
192800000517     O                                          160 '!'
192900000517     O                       STCAK         4    168
193000000517     O                       STCAC         4    177
193100000517     O                       STCIK         4    189
193200000517     O                       STCIC         4    196
193300941206     O                                          198 '!'
195400000000     O*------------------------------------------------------------------------*
195500000000     O          E            RIGTOT         1
195600000000     O                                            1 '!'
195700090709     O                       STDES1            +  1
195800090709     O                       STDES2            +  1
195900080319     O***                                      + 28 '!'
196000080319     O***                    PARMMF            +  1
196100080319     O***                                      +  1 '!'
196200000517     O                       STPER         K     87
196300000517     O                       STNCL         Z     96
196400000517     O                       STPKB         4    108
196500000517     O                       STNSP         Z    117
196600000517     O                                          119 '!'
196700000517     O                       STVLB         4    131
196800000517     O                       STVLC         4    143
196900000519     O                       STRNG         K    152
197000040219     O*---                   STTTP         K    159
197100000517     O                                          160 '!'
197200000000     O                                          198 '!'
198900000000     O*------------------------
199000000000     O          E            RIGSEP         1
199100000000     O                       TES(1)              66
199200000000     O                       TES(1)             132
199300000000     O                       TES(1)             198
199400000000     O*------------------------
199500000000     O          E            STAFIN           63
199600000000     O                                           76 '*** FINE STAMPA ***'
199700941206     O*------------------------------------------------------------------------*
199800980421** -TES-   (DESCRIZIONI  STAMPA)
199900000000------------------------------------------------------------------  1
200000000000!                                                                   2
200100080319            ! (B) %RAPP.  NUM.                 NUM. !    VOLUME     3
200200040219   VOLUME    (A) %         !                                     !  4
200300941207!                                                                   5
200400080319            !   COLLI    COLLI        PESO    SPED. ! BOLL./REALE   6
200500080319     VDL     SCOST.        !                                     !  7
200600000000                                                                    8
200700000000                                                                    9
200800080319TOTALE TER.PART                                                    10
200900000000TOTALE AREA                                                        11
201000980421TOTALE DISTRETTO                                                   12
201100000000TOTALE GENERALE                                                    13
201101090709TOTALE COMM.LE                                                     14
201200000000** -INT-   (INTESTAZIONE)
201300000000DETTAGLIO                                                       1
201400000000SINTETICA                                                       2
2015009506293
2016009804214
201700950629R E A L E 5
201800980421** -DEC-   (DECODIFICHE RIGHE)
201900980421ASSEGNATI                                                            1
202000980421FRANCHI                                                              2
202100991102CON IMPIANTO                                                         3
202200991102SENZA IMPIANTO                                                       4
202300000706POSTE                                                                5
202400000706NO POSTE                                                             6
2025000007067
2026000007068
2027000007069
20280000070610
