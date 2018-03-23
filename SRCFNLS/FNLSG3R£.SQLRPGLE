000100140611     /*PRM  dbgview(*source)
000200140611     /*END
000300000000     H*------------------------------------------------------------------------*
000400000000     H* STAMPA STATISTICA VOLUMI
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700140611     h option(*nodebugio)
000800000000     F*------------------------------------------------------------------------*
000900000000     F* DATA BASE
001000000000     F*------------------------------------------------------------------------*
001100090709     FWFVLMF1L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000000     FAZORG01L  IF   E           K DISK
001400140611     fAZCMM01L  if   e           k disk
001500010314     FTNTAM01L  IF   E           K DISK
001600000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001700000000     F                                     FORMLEN(66)
001800000000     F                                     FORMOFL(60)
001900000000     L*------------------------------------------------------------------------*
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300080318     d dataiso         s               d   datfmt(*iso)
002400080318     d dataeur         s               d   datfmt(*eur)
002500080318     d PARDAL_E        s                   LIKE(PARDAL)
002600080318     d PARal_e         s                   LIKE(PARAL)
002700090709     d orddes          s             18
002800090709     d decflp          s             20
002900090709     d w_primav        s               n
003000090710     d rotcom          s               n
003100000000     D*-------------------
003200000000     D* INTESTAZIONI STAMPA
003300000000     D*-------------------
003400090709     D TES             S             66    DIM(14) CTDATA PERRCD(1)
003500950629     D INT             S              9    DIM(5) CTDATA PERRCD(1)
003600000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)
003700000000     D*-------------------
003800000000     D* SCHIERA FILIALI
003900000000     D*-------------------
004000971124     D FIL             S              3  0 DIM(500)
004100971124     D DFIL            S             20    DIM(500)
004200020627     D PFIL            S              3    DIM(500)
004300000000     D*------------------------------------------------------------------------*
004400000000     D* INPUT
004500000000     D*------------------------------------------------------------------------*
004600000000     D*-------------------
004700000000     D* RICERCA PARAMETRI UTENTI
004800000000     D*-------------------
004900020919     D TIBS34DS      E DS
005000020919     D DDATIUTE      E DS
005100020919     D AZUTEDS       E DS                  extname(AZUTE00F)
005200090513
005300090709     d wfvlmds       e ds                  extname(wfvlmf0f) inz
005400000000     D*-------------------
005500000000     D* ARCHITETTURA
005600000000     D*-------------------
005700000000     D KPJBA         E DS
005800991102     D*-------------------
005900991102     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
006000991102     D*-------------------
006100991102     D OG150         E DS
006200000706     D*-------------------
006300000706     D* LETTURA CAMPO ORGDE£ DI AZORG
006400000706     D*-------------------
006500000706     D OG143         E DS
006600020910     D*------------------
006700020910     D* DS REPEIMENTO TERMINALs
006800020910     D*------------------
006900020910     D FNLV55DS      E DS
007000030909     D*------------------
007100030909     D* DS RIDEFINIZIONE FLAG OPERATIVI TARIFFE
007200030909     D*------------------
007300030909     D DSTA01        E DS
007400020910     D*------------------
007500020910     D* DS CONTROLLO DATA (8)
007600020910     D*------------------
007700020910     D WLBDA8          DS                  INZ
007800020910     D  G08DAT                 1      8  0
007900020910     D  G08INV                 9     16  0
008000020910     D  G08ERR                17     17
008100020910     D  G08TGI                18     22  0
008200000000     D*-------------------
008300000000     D* DS PARAMETRI DI LANCIO PGM
008400000000     D*-------------------
008500950703     D PARAM           DS
008600991029     D  PARTLA                 1      1
008700080318     D**PARANN                 2      5  0
008800080318     D**PARMMI                 6      7  0
008900080318     D**PARMMF                 8      9  0
009000080318     d  pardal                 2      9  0
009100080318     D**PARSNR                10     10
009200991029     D  PARPID                27     31  2
009300080318     D**PARDIV                32     32
009400080318     D**PARARE                33     35  0
009500080318     D**PARFIL                36     38  0
009600080318     D  PARtfp                36     38  0
009700991029     D  PARDET                39     39
009800080318     D**PARTOT                40     40
009900991029     D  PARRG1                41     44  1
010000991029     D  PARRG2                45     48  1
010100080318     D**PARPIA                49     53  2
010200991029     D  PARPSD                54     58  2
010300080318     D**PARPSA                59     63  2
010400080318     D**PAROK                 65     66
010500991229     D  PARMIN                67     71  3
010600080318     D**PARPOS                72     72
010700080318     D**PARNT1                73     75
010800080318     D**PARNT2                76     78
010900080318     D**PARNT3                79     81
011000080318     D**PARNT4                82     84
011100080318     D**PARSAL                85     85
011200080318     d  paral                 86     93  0
011300090513     d  parord                75     75
011400090709     d  parpag                76     76
011500090709     d  parflp                77     79  0
011600000000     C*------------------------------------------------------------------------*
011700000000     C* MAIN LINE
011800000000     C*------------------------------------------------------------------------*
011900000000     C*
012000090513     c* In base all'ordinamento scelto (1-cliente; 2-ragione sociale)
012100090513     c* eseguo sql o utilizzo v.l.
012200090513     c                   if        parord='1'
012300090709     c                   exsr      sr_ordcomset
012400090709     c                   exsr      sr_ordcomlet
012500090709     c                   exsr      sr_elabcom
012600090513     c                   else
012700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012800000000     C                   EXSR      SETFIL
012900000000     C*
013000000000     C* CICLO FINO A FINE FILE DI LAVORO
013100000000DO  1C     FINL00        DOWEQ     '0'
013200000000     C*
013300000000     C* OPERAZIONI PER NUOVA DIVISIONE
013400000000     C                   EXSR      NEWDIV
013500000000     C*
013600000000     C* CICLO FINO A ROTTURA DI DIVISIONE
013700000000DO  2C     FINL00        DOWEQ     '0'
013800000000     C     WFVDIV        ANDEQ     DEPDIV
013900000000     C*
014000000000     C* OPERAZIONI PER NUOVA AREA
014100000000     C                   EXSR      NEWARE
014200000000     C*
014300000000     C* CICLO FINO A ROTTURA DI AREA
014400000000DO  3C     FINL00        DOWEQ     '0'
014500000000     C     WFVDIV        ANDEQ     DEPDIV
014600000000     C     WFVARE        ANDEQ     DEPARE
014700000000     C*
014800000000     C* OPERAZIONI PER NUOVA FILIALE
014900000000     C                   EXSR      NEWFIL
015000000000     C*
015100000000     C* CICLO FINO A ROTTURA DI FILIALE
015200000000DO  4C     FINL00        DOWEQ     '0'
015300000000     C     WFVDIV        ANDEQ     DEPDIV
015400000000     C     WFVARE        ANDEQ     DEPARE
015500000000     C     WFVFIL        ANDEQ     DEPFIL
015600991029     C*
015700991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
015800991029     C                   EXSR      NEWPRO
015900991029     C*
016000991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
016100991029DO  5C     FINL00        DOWEQ     '0'
016200991029     C     WFVDIV        ANDEQ     DEPDIV
016300991029     C     WFVARE        ANDEQ     DEPARE
016400991029     C     WFVFIL        ANDEQ     DEPFIL
016500991029     C     WFVKSC        ANDEQ     DEPKSC
016600991029     C     WFVCTR        ANDEQ     DEPCTR
016700000000     C*
016800991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
016900000000     C                   EXSR      NEWCLI
017000000000     C*
017100991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
017200991029DO  6C     FINL00        DOWEQ     '0'
017300991029     C     WFVDIV        ANDEQ     DEPDIV
017400991029     C     WFVARE        ANDEQ     DEPARE
017500991029     C     WFVFIL        ANDEQ     DEPFIL
017600991029     C     WFVKSC        ANDEQ     DEPKSC
017700991029     C     WFVCTR        ANDEQ     DEPCTR
017800991029     C     WFVMES        ANDEQ     DEPMES
017900000000     C*
018000000000     C* MEMORIZZA LE SPEDIZIONI
018100000517     C     WRECOK        IFEQ      'S'
018200000517     C                   EXSR      MEMSPE
018300000517     C                   ENDIF
018400000000     C*
018500000000     C* LEGGE IL RECORD SUCCESSIVO
018600000000     C                   EXSR      LETFIL
018700000000     C*
018800040220E   6C                   ENDDO
018900000000     C*
019000991029     C* DETTAGLIO: STAMPA IL TOTALE CLIENTE/TARIFFA/MESE
019100000000     C                   EXSR      STACLI
019200000000     C*
019300991029E   5C                   ENDDO
019400991029     C*
019500991029     C*
019600000000E   4C                   ENDDO
019700000000     C*
019800000000     C* STAMPA IL TOTALE FILIALE
019900000000     C                   EXSR      STAFIL
020000000000     C*
020100000000E   3C                   ENDDO
020200000000     C*
020300000000E   2C                   ENDDO
020400000000     C*
020500000000E   1C                   ENDDO
020600000000     C*
020700000000     C* STAMPA RIGA FINE STAMPA
020800000000     C                   EXCEPT    STAFIN
020900090709     c                   endif
021000090513      /free
021100090513       if parord='1';
021200090513          exec sql close WVLMcsr;
021300090513       endif;
021400090513      /end-free
021500000000     C*
021600000000     C                   SETON                                        LR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
021900000000     C*------------------------------------------------------------------------*
022000000000     C     SETFIL        BEGSR
022100000000     C*
022200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
022300080318     C                   clear                   KWVANN
022400000000     C                   MOVEL     *BLANKS       KWVDIV
022500000000     C                   Z-ADD     *ZEROS        KWVARE
022600000000     C                   Z-ADD     *ZEROS        KWVFIL
022700000000     C                   Z-ADD     *ZEROS        KWVKSC
022800000000     C                   Z-ADD     *ZEROS        KWVCTR
022900000000     C                   Z-ADD     *ZEROS        KWVMES
023000950629     C                   MOVEL     *BLANKS       KWVFVB
023100090709     C**   KEYWFV        SETLL     WFVLMF1L                           99
023200090709     C     *loval        SETLL     WFVLMF1L                           99
023300000000     C*
023400000000     C     *IN99         IFEQ      '1'
023500000000     C                   MOVEL     '1'           FINL00
023600000000     C                   ELSE
023700000000     C                   EXSR      LETFIL
023800000000     C                   END
023900000000     C*
024000000000     C                   ENDSR
024100000000     C*------------------------------------------------------------------------*
024200941207     C* LETFIL - LETTURA PROSSIMO RECORD
024300000000     C*------------------------------------------------------------------------*
024400000000     C     LETFIL        BEGSR
024500000000     C*
024600000000     C* LEGGO FINO A:
024700000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
024800000000     C*     - TROVATO RECORD VALIDO
024900000000     C                   MOVEL     'N'           WRECOK
025000040211     C                   MOVEL     'N'           WRECOKC
025100000000     C*
025200000000     C* LETTURE SUCCESSIVE ALLA PRIMA
025300090709     C                   READ      WFVLMF1L                               99
025400000000     C*
025500000000     C     *IN99         IFEQ      '1'
025600080318     C***  WFVANN        ORNE      PARANN
025700000000     C                   MOVEL     '1'           FINL00
025800000000     C                   ELSE
025900000000     C                   EXSR      CHKREC
026000000517     C                   ENDIF
026100000000     C*
026200000000     C                   ENDSR
026300000000     C*------------------------------------------------------------------------*
026400000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
026500000000     C*------------------------------------------------------------------------*
026600000000     C     CHKREC        BEGSR
026700000000     C*
026800000000     C                   MOVEL     'S'           WRECOK            1
026900040211     C                   MOVEL     'S'           WRECOKC           1
027000000000     C*
027100000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
027200080318     C**   WFVMES        IFGE      PARMMI
027300080318     C**   WFVMES        ANDLE     PARMMF
027400080318     C**                 ELSE
027500080318     C**                 MOVEL     'N'           WRECOK
027600080318     C**                 END
027700000706     C*
027800000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
027900080318IF  1C**   WRECOK        IFEQ      'S'
028000080318     C**                 Z-ADD     1             I
028100080318     C*    WFVFIL        LOOKUP    FIL(I)                                 95
028200080318IF  2C*    PARPOS        IFEQ      'N'
028300080318IF  3C*    PFIL(I)       IFEQ      'PPT'
028400080318     C*                  MOVEL     'N'           WRECOK
028500080318E   3C*                  ENDIF
028600080318X   2C*                  ELSE
028700080318IF  3C*    PFIL(I)       IFNE      'PPT'
028800080318     C*                  MOVEL     'N'           WRECOK
028900080318E   3C*                  ENDIF
029000080318E   2C*                  ENDIF
029100080318E   1C*                  ENDIF
029200000000     C*
029300000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
029400080318     C*    PARDIV        IFNE      *BLANKS
029500080318     C*    WFVDIV        IFNE      PARDIV
029600080318     C*                  MOVEL     'N'           WRECOK
029700080318     C*                  END
029800080318     C*                  END
029900000000     C*
030000000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
030100080318     C*    PARARE        IFNE      *ZEROS
030200080318     C*    WFVARE        IFNE      PARARE
030300080318     C*                  MOVEL     'N'           WRECOK
030400080318     C*                  END
030500080318     C*                  END
030600000000     C*
030700000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
030800080318     C*    PARFIL        IFNE      *ZEROS
030900080318     C*    WFVFIL        IFNE      PARFIL
031000080318     C*                  MOVEL     'N'           WRECOK
031100080318     C*                  END
031200080318     C*                  END
031300950705     C*
031400950705     C* CALCOLA PERCENTUALE VOLUME CML SU VOL.AUTOM.
031500001009     C                   Z-ADD     WFVVLB        WCPVLB           15 3
031600001009     C                   Z-ADD     WFVVLC        WCPVLC           15 3
031700991102     C                   EXSR      CALRNG
031800991102     C                   Z-ADD     WCPRVA        WRNG              6 2
031900991102     C*
032000991104     C* CALCOLA RAPPORTO COLLI CML SU COLLI BOLL.
032100001009     C                   Z-ADD     WFVTCL        WCPNCL           15 3
032200001009     C                   Z-ADD     WFVNCR        WCPNCR           15 3
032300991102     C                   EXSR      CALPER
032400991102     C                   Z-ADD     WCPPVA        WPER              6 2
032500000000     C*
032600991104     C* SE IL RAPPORTO COLLI VDL CON/SENZA IMPIANTO NO IN RANGE RICHIESTO, SCART
032700040211     c* CONSIDERAZIONI SOLO PER RIGA DETTAGLIO
032800991102     C     WCPPVA        IFLT      PARXDA
032900991229     C     WCPPVA        OREQ      *ZEROS
033000040211     C                   MOVEL     'N'           WRECOKC
033100000000     C                   END
033200991102     C*
033300991229     C* SE LA PERCENTUALE (A) SCOST. VOL NON COMPRESA IN RANGE RICHIESTO,SCARTO
033400991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
033500991229     C* DELLO SCARTO MINIMO (PARMIN)
033600991229     C                   Z-ADD     *zeros        PERC              6 2
033700991229     C                   EVAL      PERC=%abs(WCPRVA)
033800991229     C     WCPRVA        IFLT      PARRG1
033900991229     C     WCPRVA        ORGT      PARRG2
034000991229     C     WCPRVA        OREQ      *ZEROS
034100991229     C     PERC          ORLT      PARMIN
034200040211     C                   MOVEL     'N'           WRECOKC
034300991102     C                   END
034400030910     C*
034500030910     C* CONTROLLO SE RICHIESTE SELEZIONI X NETWORK TARIFFE
034600080318     C*                  IF        WRECOK <> 'N'
034700080318     C*                  IF        PARNT1 <> *blanks OR
034800080318     C*                            PARNT2 <> *blanks OR
034900080318     C*                            PARNT3 <> *blanks OR
035000080318     C*                            PARNT4 <> *blanks
035100030910     C* SE RICHIESTE SELEZIONI VERIFICO
035200080318     C*                  MOVEL     *blanks       wNTWTAR           3
035300030910     C* NEGO I FLAG X POI EVENTUALMENTE ATTIVARLO SE RISPONDENTE AI PARAMETRI RI
035400080318     C*                  MOVEL     'N'           WRECOK
035500030910     C* REPERISCO IL NETWORK DELLA TARIFFA DEL RECORD CORRENTE SE NN TRATTASI DI
035600080318     C*    WFVCTR        IFNE      888
035700080318     C*    WFVCTR        ANDNE     999
035800080318     C*                  Z-ADD     WFVKSC        KTFKSC
035900080318     C*                  Z-ADD     WFVCTR        KTFCTR
036000080318     C*                  Z-ADD     *ZEROS        KTFPRG
036100080318     C*    KEYTAM        SETLL     TNTAM01L                           99
036200080318     C* N99              READ      TNTAM01L                               99
036300080318     C*    *IN99         DOWEQ     '0'
036400080318     C*    TAMKSC        ANDEQ     KTFKSC
036500080318     C*    TAMCTR        ANDEQ     KTFCTR
036600080318     C*    TAMATB        IFEQ      ' '
036700080318     C*                  MOVEL(P)  TAMFLO        DSTA01
036800080318     C*                  IF        TAMFIE  = 'I' AND §TADPD <> 'S'
036900080318     C*                  EVAL      wNTWTAR = 'ITA'
037000080318     C*                  ENDIF
037100080318     C*                  IF        TAMFIE  = 'I' AND §TADPD  = 'S'
037200080318     C*                  EVAL      wNTWTAR = 'DPD'
037300080318     C*                  ENDIF
037400080318     C*                  IF        TAMFIE  = 'E' AND §TAFED <> 'S'
037500080318     C*                  EVAL      wNTWTAR = 'EEX'
037600080318     C*                  ENDIF
037700080318     C*                  IF        TAMFIE  = 'E' AND §TAFED  = 'S'
037800080318     C*                  EVAL      wNTWTAR = 'FED'
037900080318     C*                  ENDIF
038000080318     C*                  ENDIF
038100080318     C*                  READ      TNTAM01L                               99
038200080318     C*                  ENDDO
038300080318     C*                  ENDIF
038400030910     C* EFFETTUO CONSIDERAZIONI
038500080318     C*                  IF        PARNT1 = 'ITA' OR
038600080318     C*                            PARNT2 = 'ITA' OR
038700080318     C*                            PARNT3 = 'ITA' OR
038800080318     C*                            PARNT4 = 'ITA'
038900080318     C*                  IF        wNTWTAR = 'ITA' OR
039000080318     C*                            WFVCTR = 888    OR
039100080318     C*                            WFVCTR = 999
039200080318     C*                  MOVEL     'S'           WRECOK
039300080318     C*                  ENDIF
039400080318     C*                  ENDIF
039500080318     C*                  IF        PARNT1 = 'EEX' OR
039600080318     C*                            PARNT2 = 'EEX' OR
039700080318     C*                            PARNT3 = 'EEX' OR
039800080318     C*                            PARNT4 = 'EEX'
039900080318     C*                  IF        wNTWTAR = 'EEX'
040000080318     C*                  MOVEL     'S'           WRECOK
040100080318     C*                  ENDIF
040200080318     C*                  ENDIF
040300080318     C*                  IF        PARNT1 = 'DPD' OR
040400080318     C*                            PARNT2 = 'DPD' OR
040500080318     C*                            PARNT3 = 'DPD' OR
040600080318     C*                            PARNT4 = 'DPD'
040700080318     C*                  IF        wNTWTAR = 'DPD'
040800080318     C*                  MOVEL     'S'           WRECOK
040900080318     C*                  ENDIF
041000080318     C*                  ENDIF
041100080318     C*                  IF        PARNT1 = 'FED' OR
041200080318     C*                            PARNT2 = 'FED' OR
041300080318     C*                            PARNT3 = 'FED' OR
041400080318     C*                            PARNT4 = 'FED'
041500080318     C*                  IF        wNTWTAR = 'FED'
041600080318     C*                  MOVEL     'S'           WRECOK
041700080318     C*                  ENDIF
041800080318     C*                  ENDIF
041900030910     C*
042000080318     C*                  ENDIF
042100080318     C*                  ENDIF
042200000000     C*
042300000000     C                   ENDSR
042400000000     C*------------------------------------------------------------------------*
042500000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
042600000000     C*------------------------------------------------------------------------*
042700000000     C     MEMSPE        BEGSR
042800090709     c*
042900000000     C* TOTALE CLIENTE/TARIFFA
043000040212IF  2C     WRECOKC       IFEQ      'S'
043100040216     c                   ADD       WFVTCL        TCTCL
043200040216     C                   ADD       WFVNCL        TCNCL
043300040216     C                   ADD       WFVNCR        TCNCR
043400040216     C                   ADD       WFVPKB        TCPKB
043500040216     C                   ADD       WFVNSP        TCNSP
043600040216     C                   ADD       WFVVLB        TCVLB
043700040216     C                   ADD       WFVVLC        TCVLC
043800040216E   2C                   ENDIF
043900040216     C                   ADD       WFVVLB        TCVLBT
044000040216     C                   ADD       WFVVLC        TCVLCT
044100000000     C* TOTALE FILIALE
044200040212     C                   ADD       WFVTCL        TFTCL
044300040216     C                   ADD       WFVNCL        TFNCL
044400040216     C                   ADD       WFVNCR        TFNCR
044500040216     C                   ADD       WFVPKB        TFPKB
044600040216     C                   ADD       WFVNSP        TFNSP
044700991102     C                   ADD       WFVVLB        TFVLB
044800991102     C                   ADD       WFVVLC        TFVLC
044900040212     C                   ADD       WFVVLB        TFVLBT
045000040212     C                   ADD       WFVVLC        TFVLCT
045100090709     C* TOTALE commerciale unificante
045200090709     c                   if        parord='1'
045300090709     C                   ADD       WFVTCL        TKTCL
045400090709     C                   ADD       WFVNCL        TKNCL
045500090709     C                   ADD       WFVNCR        TKNCR
045600090709     C                   ADD       WFVPKB        TKPKB
045700090709     C                   ADD       WFVNSP        TKNSP
045800090709     C                   ADD       WFVVLB        TKVLB
045900090709     C                   ADD       WFVVLC        TKVLC
046000090709     C                   ADD       WFVVLB        TKVLBT
046100090709     C                   ADD       WFVVLC        TKVLCT
046200090709     c                   endif
046300040212     C*
046400040212     C                   ENDSR
046500000000     C*------------------------------------------------------------------------*
046600000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
046700000000     C*------------------------------------------------------------------------*
046800000000     C     NEWDIV        BEGSR
046900000000     C*
047000000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
047100000000     C                   MOVEL     WFVDIV        DEPDIV
047200000000     C*
047300000000     C* DECODIFICA LA DIVISIONE
047400000000     C                   EXSR      DECDIV
047500000000     C*
047600000000     C                   ENDSR
047700000000     C*------------------------------------------------------------------------*
047800000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
047900000000     C*------------------------------------------------------------------------*
048000000000     C     NEWARE        BEGSR
048100000000     C*
048200000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
048300000000     C                   Z-ADD     WFVARE        DEPARE
048400000000     C*
048500000000     C* DECODIFICA L'AREA
048600000000     C                   EXSR      DECARE
048700000000     C*
048800000000     C                   ENDSR
048900000000     C*------------------------------------------------------------------------*
049000000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
049100000000     C*------------------------------------------------------------------------*
049200000000     C     NEWFIL        BEGSR
049300000000     C*--------------------
049400000000     C* T E S T A T A
049500000000     C*--------------------
049600000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
049700000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
049800000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
049900000000     C*         RIGHE DI TOTALE
050000000000     C*
050100000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
050200000000     C                   Z-ADD     WFVFIL        DEPFIL
050300020910     C*
050400020910     C* REPERISCE IL TERMINAL PARTENZA
050500020910     C                   CLEAR                   FNLV55DS
050600020910     C                   MOVEL     'P'           D55TPT
050700020910     C                   MOVEL     DEPFIL        D55LIN
050800020910     C                   Z-ADD     DATCOR        D55DRF
050900020910     C                   CALL      'FNLV55R'
051000020910     C                   PARM                    FNLV55DS
051100020910IF  1C     D55ERR        IFNE      *BLANKS
051200020910     C                   MOVEL     DEPFIL        D55TFP
051300020910E   1C                   ENDIF
051400020910     C                   Z-ADD     D55TFP        DEPTFP
051500991102     C*
051600020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
051700020910     C     DEPTFP        CHAIN     AZORG01L                           88
051800991102     C     *IN88         IFEQ      *OFF
051900991102     C                   MOVEL     ORGDF0        OG150
052000120914     C     §OGVOL        IFNE      *BLANKS
052100120914     C     §OGVOL        andNE     *all'0'
052200120914     c**   §ogspv        ifeq      'S'
052300991102     C                   MOVEL     DEC(3)        CHKCML           20
052400991102     C                   Z-ADD     PARPID        PARXDA            5 2
052500991102     C                   ELSE
052600991102     C                   MOVEL     DEC(4)        CHKCML           20
052700991102     C                   Z-ADD     PARPSD        PARXDA            5 2
052800991102     C                   ENDIF
052900991102     C                   ELSE
053000991102     C                   MOVEL     DEC(4)        CHKCML           20
053100991102     C                   Z-ADD     PARPSD        PARXDA            5 2
053200991102     C                   ENDIF
053300000000     C*
053400000000     C* AZZERA I TOTALI DELLA FILIALE
053500001009     C                   Z-ADD     *ZEROS        TFVLB            15 3
053600001009     C                   Z-ADD     *ZEROS        TFVLC            15 3
053700991029     C                   Z-ADD     *ZEROS        TFNCR             9 0
053800991104     C                   Z-ADD     *ZEROS        TFTCL             9 0
053900991104     C                   Z-ADD     *ZEROS        TFNCL             9 0
054000991102     C                   Z-ADD     *ZEROS        TFPKB            10 1
054100941207     C                   Z-ADD     *ZEROS        TFNSP             8 0
054200040212     c*
054300040212     c*
054400001009     C                   Z-ADD     *ZEROS        TFVLBT           15 3
054500001009     C                   Z-ADD     *ZEROS        TFVLCT           15 3
054600000000     C*
054700000000     C* DECODIFICA LA FILIALE
054800000000     C                   EXSR      DECFIL
054900000000     C*
055000000000     C* IMPOSTAZIONI CAMPI IN TESTATA
055100000000IF  1C     PARDET        IFEQ      'S'
055200000000     C                   MOVEL     *BLANKS       DESFIL
055300991102     C                   MOVEL     *BLANKS       CHKCML
055400000000E   1C                   ELSE
055500000000     C                   SETON                                        01
055600000000E   1C                   ENDIF
055700000000     C*
055800000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
055900000000IF  1C     *INOF         IFEQ      *ON
056000000000     C     PARDET        OREQ      'D'
056100000000     C                   EXCEPT    TESTA
056200000000     C                   SETOFF                                       OF
056300000000E   1C                   ENDIF
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA
056800000000     C*------------------------------------------------------------------------*
056900000000     C     NEWCLI        BEGSR
057000000000     C*
057100000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
057200040216     C                   MOVEL     WFVRAG        DESCLI
057300040216     C                   Z-ADD     WFVKSC        DEPKSC
057400000000     C                   Z-ADD     WFVCTR        DEPCTR
057500040216     C                   Z-ADD     WFVMES        DEPMES
057600000000     C*
057700040219     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
057800001009     C                   Z-ADD     *ZEROS        TCVLB            15 3
057900001009     C                   Z-ADD     *ZEROS        TCVLC            15 3
058000991029     C                   Z-ADD     *ZEROS        TCNCR             9 0
058100991104     C                   Z-ADD     *ZEROS        TCTCL             9 0
058200991104     C                   Z-ADD     *ZEROS        TCNCL             9 0
058300991102     C                   Z-ADD     *ZEROS        TCPKB            10 1
058400950629     C                   Z-ADD     *ZEROS        TCNSP             8 0
058500040212     C*
058600001009     C                   Z-ADD     *ZEROS        TCVLBT           15 3
058700001009     C                   Z-ADD     *ZEROS        TCVLCT           15 3
058800040220     c*
058900080319     C**   depmes        ifle      parmmf
059000080319     C                   Z-ADD     0             FLGCLI            1 0
059100080319     c**                 endif
059200950629     C*
059300000000     C                   ENDSR
059400991029     C*------------------------------------------------------------------------*
059500991029     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
059600991029     C*------------------------------------------------------------------------*
059700991029     C     NEWPRO        BEGSR
059800991029     C*
059900991029     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
060000991029     C                   Z-ADD     WFVKSC        DEPKSC
060100991029     C                   Z-ADD     WFVCTR        DEPCTR
060200040212     c*
060300991029     C                   ENDSR
060400090709     C*------------------------------------------------------------------------*
060500090709     C* NEWCOM - OPERAZIONI PER NUOVO COMMERCIALE UNIFICANTE
060600090709     C*------------------------------------------------------------------------*
060700090709     C     NEWCOM        BEGSR
060800090709     C*
060900090710     c                   eval      rotcom=*on
061000090709     C* MEMORIZZA LA NUOVA AREA
061100090709     C                   Z-ADD     WFvcun        DEPcun
061200090709     C*
061300090709     C* DECODIFICA il commerciale unificante
061400090709     C                   EXSR      DECcun
061500090709     c*
061600090709     C* AZZERA I TOTALI DEL commerciale unificante
061700090709     C                   Z-ADD     *ZEROS        TkVLB            15 3
061800090709     C                   Z-ADD     *ZEROS        TkVLC            15 3
061900090709     C                   Z-ADD     *ZEROS        TkNCR             9 0
062000090709     C                   Z-ADD     *ZEROS        TkTCL             9 0
062100090709     C                   Z-ADD     *ZEROS        TkNCL             9 0
062200090709     C                   Z-ADD     *ZEROS        TkPKB            10 1
062300090709     C                   Z-ADD     *ZEROS        TkNSP             8 0
062400090709      *
062500090709     C                   Z-ADD     *ZEROS        TkVLBT           15 3
062600090709     C                   Z-ADD     *ZEROS        TkVLCT           15 3
062700090709     C*
062800090709     C                   ENDSR
062900000000     C*------------------------------------------------------------------------*
063000000000     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
063100000000     C*------------------------------------------------------------------------*
063200000000     C     STACLI        BEGSR
063300950629     C*
063400040219     C* PER NON STAMPARE DUE VOLTE I DATI DEL CLIENTE SE HA DATI
063500950629     C                   MOVEL     '0'           $STDAC
063600000000     C*
063700040212     C* IMPOSTA I VALORI DI STAMPA
063800040216IF  1C     TCVLB         IFNE      *ZEROS
063900950629     C     TCVLC         ORNE      *ZEROS
064000950629     C*
064100950629     C* DECODIFICA LA TARIFFA
064200950629     C                   MOVEL     *BLANKS       DESCTR
064300950629     C                   EXSR      DECCTR
064400001009     C                   Z-ADD     TCVLB         STVLB             9 2
064500001009     C                   Z-ADD     TCVLC         STVLC             9 2
064600991029     C                   Z-ADD     TCNCR         STNCR             9 0
064700991105     C                   Z-ADD     TCTCL         STTCL             9 0
064800991105     C                   Z-ADD     TCNCL         STNCL             9 0
064900991102     C                   Z-ADD     TCPKB         STPKB            10 1
065000941207     C                   Z-ADD     TCNSP         STNSP             8 0
065100950705     C*
065200040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME
065300001009     C                   Z-ADD     TCVLB         WCPVLB           15 3
065400001009     C                   Z-ADD     TCVLC         WCPVLC           15 3
065500991102     C                   Z-ADD     *ZEROS        WCPRVA
065600991102     C                   EXSR      CALRNG
065700991102     C                   Z-ADD     WCPRVA        WRNG              6 2
065800000519     C                   Z-ADD     WRNG          STRNG             4 1
065900000517     C*
066000040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME  TOTALI
066100001009     C                   Z-ADD     TCVLBT        WCPVLB           15 3
066200001009     C                   Z-ADD     TCVLCT        WCPVLC           15 3
066300000517     C                   Z-ADD     *ZEROS        WCPRVA
066400000517     C                   EXSR      CALRNG
066500000517     C                   Z-ADD     WCPRVA        WRNG              6 2
066600000718     C                   Z-ADD     WRNG          STTTP             4 1
066700991102     C*
066800991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLLI BOLL.
066900001009     C                   Z-ADD     TCTCL         WCPNCL           15 3
067000001009     C                   Z-ADD     TCNCR         WCPNCR           15 3
067100991102     C                   Z-ADD     *ZEROS        WCPPVA
067200991102     C                   EXSR      CALPER
067300991102     C                   Z-ADD     WCPPVA        WPER              6 2
067400000517     C                   Z-ADD     WPER          STPER             4 1
067500090710     c*
067600090710     c* prima di stampare la riga del cliente/tariffa, se c'è stata
067700090710     c* rottura di commerciale devo prima stampare il nuovo commerciale
067800090710     c                   if        parord='1'  and rotcom=*on
067900090710     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
068000090710     C     *INOF         IFEQ      *ON
068100090710     C     PARpag        OREQ      'S'
068200090710     c     depcun_p      andgt     0
068300090710     C                   EXCEPT    TESTA
068400090710     C                   SETOFF                                       OF
068500090710     C                   ENDIF
068600090710     c* stampo la riga del commerciale unificante
068700090710     c                   eval      depcun_p=depcun
068800090710     c                   if        pardet='D'
068900090710     c                   except    rigcom
069000090710     c                   endif
069100090710     c                   eval      rotcom=*off
069200090710     c                   endif
069300950629     C*
069400991029     C* STAMPA RIGA CLIENTE/TARIFFA SOLO SE MESE E' ULTIMO DEL RANGE
069500080319IF  2C**   DEPMES        IFEQ      PARMMF
069600950629     C                   EXSR      STRCLI
069700950629     C                   MOVEL     '1'           $STDAC
069800080319E   2C***                ENDIF
069900040212E   1C                   ENDIF
070000000000     C*
070100000000     C                   ENDSR
070200000000     C*------------------------------------------------------------------------*
070300000000     C* STAFIL - STAMPA TOTALI FILIALE
070400000000     C*------------------------------------------------------------------------*
070500000000     C     STAFIL        BEGSR
070600000000     C*
070700000000     C* SE NON CI SONO VOLUMI, NON STAMPO
070800040212IF  1C     TFVLB         IFEQ      *ZEROS
070900000000     C     TFVLC         ANDEQ     *ZEROS
071000040212X   1C                   ELSE
071100000000     C*
071200000000     C* DESCRIZIONE TOTALE
071300991102     C                   MOVEL     TES(10)       STDES1           22
071400000000     C*
071500000000     C* DECODIFICA LA FILIALE
071600080319     C****               EXSR      DECFIL
071700080319     C                   Z-ADD     1             I
071800080319     C     partfp        LOOKUP    FIL(I)                                 95
071900080319     C     *IN95         IFEQ      '0'
072000080319     C                   MOVEL     *ALL'*'       DESFIL           20
072100080319     C                   ELSE
072200080319     C                   MOVEL     DFIL(I)       DESFIL
072300080319     C                   END
072400000000     C                   MOVEL     DESFIL        STDES2           20
072500000000     C*
072600000000     C* IMPOSTO I VALORI DI STAMPA
072700941207     C                   Z-ADD     TFVLB         STVLB
072800000000     C                   Z-ADD     TFVLC         STVLC
072900991105     C                   Z-ADD     TFTCL         STTCL
073000991105     C                   Z-ADD     TFNCL         STNCL
073100000000     C                   Z-ADD     TFPKB         STPKB
073200000000     C                   Z-ADD     TFNSP         STNSP
073300991102     C*
073400991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
073500001009     C                   Z-ADD     TFVLB         WCPVLB           15 3
073600001009     C                   Z-ADD     TFVLC         WCPVLC           15 3
073700991102     C                   Z-ADD     *ZEROS        WCPRVA
073800991102     C                   EXSR      CALRNG
073900991102     C                   Z-ADD     WCPRVA        WRNG
074000000519     C                   Z-ADD     WRNG          STRNG             4 1
074100000517     C*
074200000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
074300001009     C                   Z-ADD     TFVLBT        WCPVLB           15 3
074400001009     C                   Z-ADD     TFVLCT        WCPVLC           15 3
074500000517     C                   Z-ADD     *ZEROS        WCPRVA
074600000517     C                   EXSR      CALRNG
074700000517     C                   Z-ADD     WCPRVA        WRNG
074800000519     C                   Z-ADD     WRNG          STTTP             4 1
074900991102     C*
075000991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
075100001009     C                   Z-ADD     TFTCL         WCPNCL           15 3
075200001009     C                   Z-ADD     TFNCR         WCPNCR           15 3
075300991102     C                   Z-ADD     *ZEROS        WCPPVA
075400991102     C                   EXSR      CALPER
075500991102     C                   Z-ADD     WCPPVA        WPER
075600000517     C                   Z-ADD     WPER          STPER             4 1
075700991102     C*
075800090709     c* se stampa per commerciale questo totale lo devo stampare a pagina
075900090709     c* nuova
076000090709     c                   if        parord='1'
076100090709     c                   setoff                                       01
076200090709     c                   except    testa
076300090709     c                   endif
076400000000     C* SE DETTAGLIO
076500000000IF  2C     PARDET        IFEQ      'D'
076600000000     C                   EXCEPT    RIGSEP
076700000000E   2C                   ENDIF
076800000000     C*
076900000000     C* STAMPO RIGA TOTALI
077000000000     C                   EXSR      STRTOT
077100000000     C*
077200000000     C* SE DETTAGLIO
077300000000IF  2C     PARDET        IFEQ      'D'
077400000000     C                   EXCEPT    RIGSEP
077500000000E   2C                   ENDIF
077600040212E   1C                   ENDIF
077700000000     C*
077800000000     C                   ENDSR
077900090709     C*------------------------------------------------------------------------*
078000090709     C* STAcom - STAMPA TOTALI commerciale unificante
078100090709     C*------------------------------------------------------------------------*
078200090709     C     STAcom        BEGSR
078300090709     C*
078400090709     C* SE NON CI SONO VOLUMI, NON STAMPO
078500090709IF  1C     TkVLB         IFEQ      *ZEROS
078600090709     C     TkVLC         ANDEQ     *ZEROS
078700090709X   1C                   ELSE
078800090709     c                   setoff                                       21
078900090709     C                   MOVEL     TES(14)       STDES1           22
079000090709     c                   movel     descun        stdes2
079100090709     C*
079200090709     C* IMPOSTO I VALORI DI STAMPA
079300090709     C                   Z-ADD     TkVLB         STVLB
079400090709     C                   Z-ADD     TkVLC         STVLC
079500090709     C                   Z-ADD     TkTCL         STTCL
079600090709     C                   Z-ADD     TkNCL         STNCL
079700090709     C                   Z-ADD     TkPKB         STPKB
079800090709     C                   Z-ADD     TkNSP         STNSP
079900090709     C*
080000090709     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
080100090709     C                   Z-ADD     TkVLB         WCPVLB           15 3
080200090709     C                   Z-ADD     TkVLC         WCPVLC           15 3
080300090709     C                   Z-ADD     *ZEROS        WCPRVA
080400090709     C                   EXSR      CALRNG
080500090709     C                   Z-ADD     WCPRVA        WRNG
080600090709     C                   Z-ADD     WRNG          STRNG             4 1
080700090709     C*
080800090709     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
080900090709     C                   Z-ADD     TkVLBT        WCPVLB           15 3
081000090709     C                   Z-ADD     TkVLCT        WCPVLC           15 3
081100090709     C                   Z-ADD     *ZEROS        WCPRVA
081200090709     C                   EXSR      CALRNG
081300090709     C                   Z-ADD     WCPRVA        WRNG
081400090709     C                   Z-ADD     WRNG          STTTP             4 1
081500090709     C*
081600090709     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
081700090709     C                   Z-ADD     TkTCL         WCPNCL           15 3
081800090709     C                   Z-ADD     TkNCR         WCPNCR           15 3
081900090709     C                   Z-ADD     *ZEROS        WCPPVA
082000090709     C                   EXSR      CALPER
082100090709     C                   Z-ADD     WCPPVA        WPER
082200090709     C                   Z-ADD     WPER          STPER             4 1
082300090709     C*
082400090709     C* SE DETTAGLIO
082500090709IF  2C     PARDET        IFEQ      'D'
082600090709     C                   EXCEPT    RIGSEP
082700090709E   2C                   ENDIF
082800090709     C*
082900090709     C* STAMPO RIGA TOTALI
083000090709     C                   EXSR      STRTOT
083100090709     C*
083200090709     C* SE DETTAGLIO
083300090709IF  2C     PARDET        IFEQ      'D'
083400090709     C                   EXCEPT    RIGSEP
083500090709E   2C                   ENDIF
083600090709     c                   endif
083700090709     C*
083800090709     C                   ENDSR
083900000000     C*------------------------------------------------------------------------*
084000000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
084100000000     C*------------------------------------------------------------------------*
084200000000     C     STRCLI        BEGSR
084300000000     C*--------------------
084400000000     C* T E S T A T A
084500000000     C*--------------------
084600000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
084700000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
084800000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
084900000000     C*         RIGHE DI TOTALE
085000000000     C*
085100950629     C* RECUPERA CALCOLO ATTUALE DEL VOLUME E CALCOLA IL PREVISTO
085200000000     C                   EXSR      RECCAL
085300991105 MOD C     PARDET        IFEQ      'D'
085400000000     C     *INOF         IFEQ      '1'
085500000000     C                   SETON                                        01
085600000000     C                   EXCEPT    RIGSEP
085700000000     C                   EXCEPT    TESTA
085800000000     C                   SETOFF                                       OF
085900000718     C                   ENDIF
086000950629     C*
086100040219     C* SE HA GIA' STAMPATO UNA RIGA HA GIA' STAMPATO
086200950629     C* ANCHE I DATI DEL CLIENTE, PER CUI PER LA SECONDA RIGA LI PULISCE
086300950629IF  1C     $STDAC        IFEQ      '1'
086400950629     C                   Z-ADD     *ZEROS        DEPKSC
086500030909     C                   MOVEL     *BLANKS       DESCLI           29
086600950629     C                   Z-ADD     *ZEROS        DEPCTR
086700950629     C                   MOVEL     *BLANKS       DESCTR
086800950629     C                   MOVEL     *BLANKS       DESDCV
086900950629     C                   Z-ADD     *ZEROS        STCAK
087000950629     C                   Z-ADD     *ZEROS        STCAC
087100950629     C                   Z-ADD     *ZEROS        STCIK
087200950629     C                   Z-ADD     *ZEROS        STCIC
087300950629E   1C                   ENDIF
087400000000     C*
087500000000     C* STAMPO RIGA CLIENTE/TARIFFA
087600000000     C                   EXCEPT    RIGCLI
087700991105 MOD C                   ENDIF
087800991102     C                   Z-ADD     1             FLGCLI
087900000000     C*
088000000000     C                   ENDSR
088100000000     C*------------------------------------------------------------------------*
088200000000     C* STRTOT - STAMPA RIGA TOTALE
088300000000     C*------------------------------------------------------------------------*
088400000000     C     STRTOT        BEGSR
088500000000     C*
088600000000     C* STAMPO RIGA CLIENTE/TARIFFA
088700000000     C                   EXCEPT    RIGTOT
088800000000     C*
088900000000     C                   ENDSR
089000000000     C*------------------------------------------------------------------------*
089100000000     C* DECDIV - DECODIFICA DIVISIONE
089200000000     C*------------------------------------------------------------------------*
089300000000     C     DECDIV        BEGSR
089400000000     C*
089500020919     C                   MOVEL     1             KTBKUT
089600000000     C                   MOVE      '17'          KTBCOD
089700000000     C                   MOVE      *BLANKS       KTBKEY
089800000000     C                   MOVEL     DEPDIV        KTBKEY
089900000000     C     KEYTAB        CHAIN     TABEL                              99
090000000000     C     *IN99         IFEQ      '1'
090100000000     C     TBLFLG        OREQ      '*'
090200000000     C                   MOVEL     *ALL'*'       DESDIV           20
090300000000     C                   ELSE
090400000000     C                   MOVEL     TBLUNI        DESDIV
090500000000     C                   END
090600000000     C*
090700000000     C                   ENDSR
090800000000     C*------------------------------------------------------------------------*
090900000000     C* DECARE - DECODIFICA AREA
091000000000     C*------------------------------------------------------------------------*
091100000000     C     DECARE        BEGSR
091200000000     C*
091300020919     C                   MOVEL     1             KTBKUT
091400000000     C                   MOVE      '05'          KTBCOD
091500000000     C                   MOVE      *BLANKS       KTBKEY
091600000000     C                   MOVEL     DEPARE        KTBKEY
091700000000     C     KEYTAB        CHAIN     TABEL                              99
091800000000     C     *IN99         IFEQ      '1'
091900000000     C     TBLFLG        OREQ      '*'
092000000000     C                   MOVEL     *ALL'*'       DESARE           20
092100000000     C                   ELSE
092200000000     C                   MOVEL     TBLUNI        DESARE
092300000000     C                   END
092400000000     C*
092500000000     C                   ENDSR
092600090709     C*------------------------------------------------------------------------*
092700090709     C* DECcun - DECODIFICA commerciale unificante
092800090709     C*------------------------------------------------------------------------*
092900090709     C     DECcun        BEGSR
093000090709     C*
093100140611     C     DEPcun        chain     AZCMM000
093200140611     c                   IF        NOT %found(AZCMM01L)  or
093300140611     C                             CMMatb <> *blank
093400090709     C                   MOVEL     *ALL'*'       DEScun           25
093500090709     C                   ELSE
093600140611     C                   MOVEL     CMMdes        descun
093700090709     C                   END
093800090709     C*
093900090709     C                   ENDSR
094000000000     C*------------------------------------------------------------------------*
094100000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
094200000000     C*------------------------------------------------------------------------*
094300000000     C     DECFIL        BEGSR
094400000000     C*
094500000000     C                   Z-ADD     1             I
094600000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
094700000000     C     *IN99         IFEQ      '0'
094800000000     C                   MOVEL     *ALL'*'       DESFIL           20
094900000000     C                   ELSE
095000000000     C                   MOVEL     DFIL(I)       DESFIL
095100000000     C                   END
095200000000     C*
095300000000     C                   ENDSR
095400000000     C*------------------------------------------------------------------------*
095500000000     C* DECCTR - DECODIFICA TARIFFA
095600000000     C*------------------------------------------------------------------------*
095700000000     C     DECCTR        BEGSR
095800980421     C*
095900980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
096000980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
096100980421IF  1C     DEPCTR        IFEQ      999
096200991104     C                   MOVEL     DEC(1)        DESCTR           10
096300980421X   1C                   ELSE
096400980421IF  2C     DEPCTR        IFEQ      888
096500980421     C                   MOVEL     DEC(2)        DESCTR
096600980421X   2C                   ELSE
096700020919     C                   MOVEL     1             KTBKUT
096800000000     C                   MOVE      'TR'          KTBCOD
096900000000     C                   MOVE      *BLANKS       KTBKEY
097000000000     C                   MOVEL     DEPCTR        WTEM01            1
097100000000     C                   MOVEL     WTEM01        KTBKEY
097200000000     C     KEYTAB        CHAIN     TABEL                              99
097300980421IF  3C     *IN99         IFEQ      '1'
097400000000     C     TBLFLG        OREQ      '*'
097500980421     C                   MOVEL     *ALL'*'       DESCTR
097600980421X   3C                   ELSE
097700000000     C                   MOVEL     TBLUNI        DESCTR
097800980421E   3C                   ENDIF
097900980421E   2C                   ENDIF
098000980421E   1C                   ENDIF
098100000000     C*
098200000000     C                   ENDSR
098300991102     C*------------------------------------------------------------------------*
098400991102     C* CALPER - CALCOLA PERCENTUALE RAPPORTO COLLI
098500991102     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
098600991102     C*          - COLLI CML               (WCPNCR)
098700991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
098800991102     C*------------------------------------------------------------------------*
098900991102     C     CALPER        BEGSR
099000991102     C*
099100991102     C* ===> CALCOLA PERCENTUALE COLLI CML SU COLLI BOLL.
099200991102     C                   Z-ADD     *ZEROS        WCPPVA            6 2
099300991102     C     WCPNCL        IFNE      WCPNCR
099400991102     C     WCPNCL        ANDGT     *ZEROS
099500991102     C     WCPNCR        ANDGT     *ZEROS
099600001009     C     WCPNCR        MULT      100           WMULTI           18 3
099700001009     C     WMULTI        DIV       WCPNCL        WRKPER           20 3
099800991102     C     WRKPER        IFGT      9999
099900991102     C                   Z-ADD     9999,99       WCPPVA
100000991102     C                   ELSE
100100991102     C                   Z-ADD     WRKPER        WCPPVA
100200991102     C                   END
100300991102     C                   END
100400991102     C*
100500991102     C     WCPNCL        IFEQ      WCPNCR
100600991102     C     WCPNCL        ANDGT     *ZEROS
100700991102     C     WCPNCR        ANDGT     *ZEROS
100800991102     C                   Z-ADD     100           WCPPVA
100900991102     C                   ENDIF
101000991102     C*
101100000000     C                   ENDSR
101200991029     C*------------------------------------------------------------------------*
101300991102     C* CALRNG - CALCOLA PERCENTUALE SCOSTAMENTO VOLUMI
101400991102     C* INPUT:   - VOLUME BOLLETTAT0         (WCPVLB)
101500991102     C*          - VOLUME VDL                (WCPVLC)
101600991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA    (WCPRVA)
101700991029     C*------------------------------------------------------------------------*
101800991102     C     CALRNG        BEGSR
101900991029     C*
102000991229     C* ===> CALCOLA PERCENTUALE VOL CML SU VOL BOLL.
102100991102     C                   Z-ADD     *ZEROS        WCPRVA            6 2
102200991102     C     WCPVLC        IFNE      WCPVLB
102300991102     C     WCPVLC        ANDGT     *ZEROS
102400991102     C     WCPVLB        ANDGT     *ZEROS
102500001009     C     WCPVLB        SUB       WCPVLC        WDIFFE           18 3
102600001009     C     WDIFFE        MULT      100           WRKPER           20 3
102700991102     C     WRKPER        DIV       WCPVLB        WRKPER
102800991102     C     WRKPER        IFGT      9999
102900991102     C                   Z-ADD     9999,99       WCPRVA
103000991102     C                   ELSE
103100991102     C                   Z-ADD     WRKPER        WCPRVA
103200991102     C                   END
103300991102     C                   END
103400991104 OLD C*
103500991104  "  C* SPECIFICA LOGICAMENTE ERRATA MA INSERITA X MANTENERE UNIFORMI
103600991104  "  C* I CRITERI DI VALUTAZIONE UTENTE RISPETTO ALLA VECCHIA
103700991104  "  C* STATISTICA VOLUMI
103800991104  "  C*                    MULT -1        WCPRVA
103900991104     C*
104000991102     C     WCPVLC        IFEQ      WCPVLB
104100991102     C     WCPVLC        ANDGT     *ZEROS
104200991102     C     WCPVLB        ANDGT     *ZEROS
104300991102     C                   Z-ADD     100           WCPRVA
104400991102     C                   ENDIF
104500991102     C*
104600991102     C                   ENDSR
104700000000     C*------------------------------------------------------------------------*
104800000000     C* RECCAL - RECUPERA CALCOLO ATTUALE VOLUME E CALCOLA IL CONSIGLIATO
104900000000     C*------------------------------------------------------------------------*
105000000000     C     RECCAL        BEGSR
105100000000     C*
105200980421     C* AZZERA I DATI IN STAMPA
105300000000     C                   Z-ADD     *ZEROS        STCAK             5 1
105400000000     C                   Z-ADD     *ZEROS        STCAC             6 3
105500000000     C                   Z-ADD     *ZEROS        STCIK             5 1
105600000000     C                   Z-ADD     *ZEROS        STCIC             6 3
105700991104     C*                    Z-ADD*ZERO     STKPM   51
105800030909     C                   MOVEL     *BLANKS       DESDCV           12
105900030909     C                   MOVEL     *BLANKS       DESTTR            3
106000980421     C*
106100980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
106200980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI,
106300980421     C* IN QUESTI CASI SALTA LA LETTURA DELLE TARIFFE PERCHE' INESISTENTI
106400980421     C     DEPCTR        IFNE      999
106500980421     C     DEPCTR        ANDNE     888
106600000000     C*
106700980402     C* ======> CERCA IL VALORE ATTUALE DI CALCOLO E ALTRI DATI
106800000000     C                   Z-ADD     DEPKSC        KTFKSC
106900000000     C                   Z-ADD     DEPCTR        KTFCTR
107000000000     C                   Z-ADD     *ZEROS        KTFPRG
107100010314     C     KEYTAM        SETLL     TNTAM01L                           99
107200010314     C  N99              READ      TNTAM01L                               99
107300000000     C     *IN99         DOWEQ     '0'
107400000000     C     TAMKSC        ANDEQ     KTFKSC
107500000000     C     TAMCTR        ANDEQ     KTFCTR
107600000000     C     TAMATB        IFEQ      ' '
107700000000     C     TAMTIN        IFEQ      'K'
107800000000     C                   Z-ADD     TAMKPM        STCAK
107900000000     C                   Z-ADD     *ZEROS        STCAC
108000000000     C                   ELSE
108100000000     C     TAMKPM        DIV       100           STCAC
108200000000     C                   Z-ADD     *ZEROS        STCAK
108300000000     C                   END
108400980421     C                   MOVEL     TAMDCV        DESDCV
108500030909     C* REPERISCO IL NETWORK DELLA TARIFFA
108600030909     C                   MOVEL(P)  TAMFLO        DSTA01
108700030909     C                   IF        TAMFIE = 'I' AND §TADPD <> 'S'
108800030909     C                   EVAL      DESTTR = 'ITA'
108900030909     C                   ENDIF
109000030909     C                   IF        TAMFIE = 'I' AND §TADPD  = 'S'
109100030909     C                   EVAL      DESTTR = 'DPD'
109200030909     C                   ENDIF
109300030909     C                   IF        TAMFIE = 'E' AND §TAFED <> 'S'
109400030909     C                   EVAL      DESTTR = 'EEX'
109500030909     C                   ENDIF
109600030909     C                   IF        TAMFIE = 'E' AND §TAFED  = 'S'
109700030909     C                   EVAL      DESTTR = 'FED'
109800030909     C                   ENDIF
109900030909     C*
110000000000     C                   Z-ADD     TAMPRG        WPRG              3 0
110100000000     C                   END
110200010314     C                   READ      TNTAM01L                               99
110300000000     C                   END
110400000000     C*
110500000000     C* ====> IN BASE ALLA PERCENTUALE TROVATA INDICA IL VALORE DI CALCOLO OK
110600000000     C* KG/MC
110700000000     C*                             VALORE ATTUALE * VOLUME AUTOMATICO
110800000000     C*          VALORE PROPOSTO = ------------------------------------
110900000000     C*                                       VOLUME CML
111000000000     C*
111100000000     C     STCAK         IFNE      *ZEROS
111200001009     C     STVLC         ANDNE     *ZEROS
111300000000     C     STPKB         DIV(H)    STVLC         STCIK
111400000000     C                   END
111500000000     C* MC/COLLI
111600000000     C*                             VALORE ATTUALE * VOLUME CML
111700000000     C*          VALORE PROPOSTO = -----------------------------
111800000000     C*                                  VOLUME AUTOMATICO
111900000000     C*
112000000000     C     STCAC         IFNE      *ZEROS
112100001009     C     STNCL         ANDNE     *ZEROS
112200000000     C     STVLC         DIV(H)    STNCL         STCIC
112300000000     C                   END
112400980421     C                   ENDIF
112500000000     C*
112600000000     C                   ENDSR
112700090513     C*------------------------------------------------------------------------*
112800090709     C* ordinamento per commerciale unificante
112900090513     C*------------------------------------------------------------------------*
113000090709     C     sr_ordcomset  BEGSR
113100090513      /free
113200090709       exec sql declare WVLMcsr cursor for Select * from wfvlmf0f
113300090709        order by wfvann, wfvdiv, wfvare, wfvfil, wfvcun, wfvrag, wfvksc, wfvctr,
113400090709         wfvmes;
113500090513       exec sql open WVLMcsr;
113600090513      /end-free
113700090513     c                   endsr
113800090513     C*------------------------------------------------------------------------*
113900090709     C* ordinamento per commerciale unificante
114000090513     C*------------------------------------------------------------------------*
114100090709     C     sr_ordcomlet  BEGSR
114200090513      /free
114300090513       wrecok='N';
114400090513       wrecokc='N';
114500090513       exec sql Fetch WVLMcsr into :wfvlmds;
114600090513         if sqlcod=100 or sqlcod<0;
114700090513            finl00='1';
114800090513         else;
114900090513            exsr chkrec;
115000090513         endif;
115100090513      /end-free
115200090513     c                   endsr
115300090709     C*------------------------------------------------------------------------*
115400090709     C* elaborazione e stampa per commerciale unificante
115500090709     C*------------------------------------------------------------------------*
115600090709     C     sr_elabcom    BEGSR
115700090709     c* solo la prima volta
115800090709     c                   if        w_primav=*off
115900090709     c                   exsr      newdiv
116000090709     c                   exsr      neware
116100090709     c                   exsr      newfil
116200090709     c                   eval      w_primav=*on
116300090709     c                   endif
116400090710    1c     finl00        doweq     '0'
116500090709     c* operazioni per nuovo commerciale unificante
116600090709     c                   exsr      newcom
116700090710    2c     finl00        doweq     '0'
116800090709     c     wfvcun        andeq     depcun
116900090709     c* operazioni per nuovo cliente/tariffa
117000090709     c                   exsr      newcli
117100090710    3c     finl00        doweq     '0'
117200090709     c     wfvcun        andeq     depcun
117300090709     c     wfvksc        andeq     depksc
117400090709     c     wfvctr        andeq     depctr
117500090709     C* Memorizza le spedizioni
117600090710    4C     WRECOK        IFEQ      'S'
117700090709     C                   exsr      memspe
117800090710    4C                   ENDIF
117900090709     c* leggo il record successivo
118000090709     c                   exsr      sr_ordcomlet
118100090710    3c                   enddo
118200090709     c* stampa totale cliente/tariffa
118300090709     c                   exsr      stacli
118400090710    2c                   enddo
118500090709     c* stampa totale commerciale unificante
118600090709     c                   exsr      stacom
118700090710    1c                   enddo
118800090709     c                   exsr      stafil
118900090709     C* STAMPA RIGA FINE STAMPA
119000090709     C                   EXCEPT    stafin
119100090709     c                   endsr
119200020919     C*-------------------------------------------------------------------------
119300020919     C* REPERISCE I DATI UTENTE
119400020919     C*-------------------------------------------------------------------------
119500020919     C     REPDATIUTE    BEGSR
119600020919     C*
119700020919     C* INIZIALIZZA VARIABILI DI WRK
119800020919     C                   CLEAR                   TIBS34DS
119900020919     C                   CLEAR                   AZUTEDS
120000020919     C                   CLEAR                   DDATIUTE
120100020919     C*
120200020919     C     *DTAARA       DEFINE    §azute        azuteds
120300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
120400020919     C                   IN(E)     *DTAARA
120500020919if  1C                   IF        %Error
120600020919     c                   EVAL      I34Tla = 'L'
120700020919     C                   CALL      'TIBS34R'
120800020919     C                   PARM                    Tibs34Ds
120900020919     C                   IN        *DTAARA
121000020919e   1C                   ENDIF
121100020919     C*
121200020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
121300020919     C                   MOVEL(P)  RSUT          DSFIRS           20
121400020919     C*
121500020919     C                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C* *INZSR - ROUTINE INIZIALE
121800000000     C*------------------------------------------------------------------------*
121900000000     C     *INZSR        BEGSR
122000000000     C*--------------------
122100000000     C* RICEVIMENTO PARAMENTRI
122200000000     C*--------------------
122300000000     C     *ENTRY        PLIST
122400000000     C                   PARM                    KPJBA
122500950703     C                   MOVEL     KPJBU         PARAM
122600000000     C*--------------------
122700000000     C* CAMPI RIFERITI AL DATA BASE
122800000000     C*--------------------
122900980402     C     *LIKE         DEFINE    TBLKUT        KTBKUT
123000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
123100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
123200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
123300000000     C     *LIKE         DEFINE    TAMKSC        KTFKSC
123400000000     C     *LIKE         DEFINE    TAMCTR        KTFCTR
123500000000     C     *LIKE         DEFINE    TAMPRG        KTFPRG
123600000000     C     *LIKE         DEFINE    WFVANN        KWVANN
123700000000     C     *LIKE         DEFINE    WFVDIV        KWVDIV
123800000000     C     *LIKE         DEFINE    WFVARE        KWVARE
123900000000     C     *LIKE         DEFINE    WFVFIL        KWVFIL
124000991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
124100000000     C     *LIKE         DEFINE    WFVKSC        KWVKSC
124200000000     C     *LIKE         DEFINE    WFVCTR        KWVCTR
124300000000     C     *LIKE         DEFINE    WFVMES        KWVMES
124400950629     C     *LIKE         DEFINE    WFVFVB        KWVFVB
124500000000     C     *LIKE         DEFINE    WFVDIV        DEPDIV
124600000000     C     *LIKE         DEFINE    WFVARE        DEPARE
124700000000     C     *LIKE         DEFINE    WFVFIL        DEPFIL
124800020910     C     *LIKE         DEFINE    WFVFIL        DEPTFP
124900000000     C     *LIKE         DEFINE    WFVKSC        DEPKSC
125000000000     C     *LIKE         DEFINE    WFVCTR        DEPCTR
125100991029     C     *LIKE         DEFINE    WFVMES        DEPMES
125200000000     C     *LIKE         DEFINE    TAMKSC        WKSC
125300000000     C     *LIKE         DEFINE    TAMCTR        WCTR
125400090709     C     *LIKE         DEFINE    WFVcun        DEPcun
125500090710     C     *LIKE         DEFINE    WFVcun        DEPcun_p
125600000000     C*--------------------
125700000000     C* CHIAVI DI LETTURA
125800000000     C*--------------------
125900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
126000000000     C     KEYTAB        KLIST
126100980402     C                   KFLD                    KTBKUT
126200000000     C                   KFLD                    KTBCOD
126300000000     C                   KFLD                    KTBKEY
126400000000     C*
126500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
126600000000     C     KEYORG        KLIST
126700000000     C                   KFLD                    KORFIL
126800000000     C*
126900991104     C* CHIAVE LETTURA WFVLM01L - COMPLETA
127000000000     C     KEYWFV        KLIST
127100000000     C                   KFLD                    KWVANN
127200000000     C                   KFLD                    KWVDIV
127300000000     C                   KFLD                    KWVARE
127400000000     C                   KFLD                    KWVFIL
127500991029     C                   KFLD                    KWVRAG
127600000000     C                   KFLD                    KWVKSC
127700000000     C                   KFLD                    KWVCTR
127800000000     C                   KFLD                    KWVMES
127900950629     C                   KFLD                    KWVFVB
128000000000     C*
128100980407     C* CHIAVE LETTURA TNTAM31C - COMPLETA
128200000000     C     KEYTAM        KLIST
128300000000     C                   KFLD                    KTFKSC
128400000000     C                   KFLD                    KTFCTR
128500000000     C                   KFLD                    KTFPRG
128600000000     C*
128700980407     C* CHIAVE LETTURA TNTAM31C - PARZIALE
128800000000     C     KE1TAM        KLIST
128900000000     C                   KFLD                    KTFKSC
129000000000     C                   KFLD                    KTFCTR
129100020910     C*--------------------
129200020910     C* REPERISCE DATA/ORA CORRENTE
129300020910     C*--------------------
129400020910     C                   TIME                    N14              14 0
129500020910     C                   MOVE      N14           G08DAT            8 0
129600020910     C                   Z-ADD     *ZEROS        G08INV
129700020910     C                   MOVEL     '0'           G08ERR
129800020910     C                   CALL      'XSRDA8'
129900020910     C                   PARM                    WLBDA8
130000020910     C                   Z-ADD     G08INV        DATCOR            8 0
130100000000     C*--------------------
130200000000     C* CAMPI DI CONTROLLO
130300000000     C*--------------------
130400000000     C                   MOVEL     '0'           FINL00            1
130500000000     C                   SETON                                        OF
130600000000     C                   Z-ADD     UDATE         DATA              6 0
130700000000     C                   TIME                    ORA               6 0
130800040211     C*---               MOVEL     *BLANKS       $REA              1
130900950629     C                   MOVEL     '0'           $STDAC            1
131000991229     C*
131100991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
131200991229     C* PER FACILITARE I CONTROLLI
131300991229     C                   EVAL      PARMIN=%abs(PARMIN)
131400950629     C*
131500000000     C* LANCIO ROUTINE PER VERIFICARE SE
131600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
131700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
131800020919     C                   EXSR      REPDATIUTE
131900000000     C*
132000000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
132100000000IF  1C     PARDET        IFEQ      'D'
132200000000     C                   MOVEL     INT(1)        DECPAR            9
132300000000X   1C                   ELSE
132400000000     C                   MOVEL     INT(2)        DECPAR
132500000000E   1C                   ENDIF
132600000706     C*
132700000706     C* IMPOSTA LA DESCRIZIONE SE LANCIO POSTE OPPURE NO
132800080319IF  1C***  PARPOS        IFEQ      'S'
132900080319     C***                MOVEL     DEC(5)        DECPOS           10
133000080319X   1C***                ELSE
133100080319     C***                MOVEL     DEC(6)        DECPOS
133200080319E   1C***                ENDIF
133300030911     C*
133400030911     C* IMPOSTA L'ELENCOI DEI NETWORK TARIFFE RICHIESTI NEL LANCIO
133500030911     C                   MOVEL     *blanks       DECNTW           15
133600080319IF  1C***  PARNT1        IFNE      *blanks
133700080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT1 + ' '
133800080319E   1C***                ENDIF
133900080319IF  1C***  PARNT2        IFNE      *blanks
134000080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT2 + ' '
134100080319E   1C***                ENDIF
134200080319IF  1C***  PARNT3        IFNE      *blanks
134300080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT3 + ' '
134400080319E   1C***                ENDIF
134500080319IF  1C***  PARNT4        IFNE      *blanks
134600080319     C***                EVAL      DECNTW = %trim(DECNTW) + PARNT4 + ' '
134700080319E   1C***                ENDIF
134800080319     C***                IF        DECNTW = *blanks
134900080319     C***                EVAL      DECNTW = 'TUTTI'
135000080319E   1C***                ENDIF
135100000000     C*
135200000000     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
135300080319IF  1C***  PARDET        IFEQ      'S'
135400080319     C***                MOVEL     'S'           PARTOT
135500080319E   1C***                ENDIF
135600000000     C*
135700000000     C* CARICA ORGANIGRAMMA
135800000000     C                   EXSR      CARORG
135900000000     C*
136000000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
136100000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
136200080319     C**   PARDIV        IFEQ      *BLANKS
136300080319     C**   PARARE        ANDEQ     *ZEROS
136400080319     C**   PARFIL        ANDEQ     *ZEROS
136500000000     C                   SETOFF                                       02
136600080319     C**                 ELSE
136700080319     C**                 SETON                                        02
136800080319     C**                 END
136900090513     c                   if        parord='1'
137000090709     c                   eval      orddes='Comm.le unificante'
137100090513     c                   else
137200090709     c                   eval      orddes='Ragione Sociale   '
137300090513     c                   endif
137400090709     c                   clear                   decflp
137500090709     c                   setoff                                       04
137600090709     c                   if        parflp>0
137700090709     c                   seton                                        04
137800090709     c     parflp        chain     azorg01l
137900090709     c                   if        %found(azorg01l)
138000090709     C                   MOVEL     ORGDES        decflp
138100090709     c                   endif
138200090709     c                   endif
138300000000     C*
138400000000     C* SE LANCIO PER SALDI INTESTA STAMPE CON ANNO/MESE (IND.03 OFF)
138500000000     C* SE LANCIO ESTEMPORANEO INTESTA CON DATA INIZIO E FINE (IND.03 ON)
138600080319     C     PARTLA        IFEQ      'S'
138700080319     C                   SETOFF                                       03
138800080319     C                   END
138900080319     C     PARTLA        IFEQ      'E'
139000080319     C                   SETON                                        03
139100080319     C                   END
139200080319     c* inverto data spedizione dal/al in giorno/mese/anno per la stampa
139300080319     c                   move      pardal        dataiso
139400080319     c                   move      dataiso       dataeur
139500080319     c                   move      dataeur       pardal_E
139600080319     c                   move      paral         dataiso
139700080319     c                   move      dataiso       dataeur
139800080319     c                   move      dataeur       paral_E
139900000000     C*
140000000000     C                   ENDSR
140100000000     C*------------------------------------------------------------------------*
140200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
140300000000     C*------------------------------------------------------------------------*
140400000000     C     CARORG        BEGSR
140500000000     C*
140600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
140700000000     C                   Z-ADD     *ZEROS        KORFIL
140800000000     C     KEYORG        SETLL     AZORG01L                           99
140900000000     C  N99              READ      AZORG01L                               99
141000000000     C                   Z-ADD     0             I                 3 0
141100000000     C     *IN99         DOWEQ     '0'
141200000000     C     ORGFVA        IFEQ      *BLANKS
141300000000     C     ORGFAG        IFNE      'V'
141400000000     C                   ADD       1             I
141500000000     C                   Z-ADD     ORGFIL        FIL(I)
141600000000     C                   MOVEL     ORGDES        DFIL(I)
141700000706     C                   MOVEL     ORGDE3        OG143
141800020627     C                   MOVEL     §OGNTW        PFIL(I)
141900000000     C                   END
142000000000     C                   END
142100000000     C                   READ      AZORG01L                               99
142200000000     C                   END
142300000000     C*
142400000000     C                   ENDSR
142500000000     O*------------------------------------------------------------------------*
142600000000     OPRTF198   E            TESTA          1 02
142700080319     O                                            8 'FNLSG3R '
142800950706     O                                           90 '***    STATISTICA   '
142900950706     O                                          110 'V O L U M I    ***  '
143000980403     O                       DECPAR             120
143100980421     O                                          150 'Data'
143200000000     O                       DATA               159 '  /  /  '
143300980421     O                                          169 'Ora'
143400980421     O                       ORA                178 '  :  :  '
143500980421     O                                          193 'Pag.'
143600000000     O                       PAGE          Z    198
143700000000     O*------------------------
143800000000     O          E   N03      TESTA          1
143900080319     O****                                       17 'Network tariffe: '
144000080319     O****                   DECNTW              32
144100080319     O***                                        83 'ANNO:'
144200080319     O***                    PARANN              88
144300080319     O***                                        97 'MESI:'
144400080319     O****                   PARMMI             100
144500080319     O***                                       102 '/'
144600080319     O****                   PARMMF             105
144700080319     O***+                   DECPOS             198
144800080319     O                                           85 'DATA Spedizione dal'
144900080319     O                       PARdal_E      8  +   1
145000080319     O                                        +   2 'al'
145100080319     O                       PARal_E       8  +   1
145200000000     O*------------------------
145300080319     O***       E    02      TESTA       1  1
145400080319     O***                                        20 'PARZIALIZZAZIONI:   '
145500080319     O***                                        40 '             DIVIS.:'
145600080319     O***                    PARDIV              41
145700080319     O***                                        61 '               AREA:'
145800080319     O***                    PARARE        Z     64
145900080319     O***                                        84 '            P.OPER.:'
146000080319     O***                    PARFIL        Z     87
146100090513     O          E            TESTA       1  1
146200090513     O                                           20 'ORDINAMENTO: per:'
146300090513     O                       orddes           +   1
146400090709     o               04                       +   4 'SOLO FILIALE DI PARTENZA'
146500090709     o               04      parflp           +   1
146600090709     o               04      decflp           +   1
146700000000     O*------------------------
146800991102     O          E            TESTA       1  1
146900000000     O                       TES(1)              66
147000000000     O                       TES(1)             132
147100991229     O                       TES(1)             198
147200991229     O*------------------------
147300991229     O          E            TESTA          1
147400080319     O                                           10 '! TER.PAR:'
147500991229     O                       DESFIL            +  1
147600991229     O                                         +  1 '   AREA:'
147700991229     O                       DESARE            +  1
147800991229     O                                         +  1 '   DISTRETTO:'
147900991229     O                       DESDIV            +  1
148000991229     O                                         +  1 '   SCOST. (A) DA: '
148100991229     O                       PARRG1        N   +  1
148200991229     O                                         +  1 '%'
148300991229     O                                         +  1 '   A:'
148400991229     O                       PARRG2        N   +  1
148500991229     O                                         +  1 '%'
148600991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
148700991229     O                       PARPID        N   +  1
148800991229     O                                         +  1 '%'
148900991229     O                                         +  1 ' SENZA IMP'
149000991229     O                       PARPSD        N   +  1
149100991229     O                                         +  1 '%'
149200991229     O                                          198 '!'
149300991229     O*-------------------------                                       --------*
149400991229     O          E            TESTA          1
149500991229     O                                            1 '!'
149600991229     O                       CHKCML            + 10
149700991229     O                                          115 'MIN.   (A)   :±'
149800991229     O                       PARMIN        N   +  0
149900991229     O                                         +  1 '%'
150000991229     O                                          198 '!'
150100991102     O*------------------------
150200991102     O          E            TESTA          1
150300991102     O                       TES(1)              66
150400991102     O                       TES(1)             132
150500991102     O                       TES(1)             198
150600000000     O*------------------------
150700000000     O          E            TESTA          1
150800000000     O                       TES(2)              66
150900000000     O                       TES(3)             132
151000000000     O                       TES(4)             198
151100000000     O               01                         175 'VALORE ATTUALE '
151200000000     O               01                         196 'VALORE PROPOSTO'
151300000000     O*------------------------
151400000000     O          E            TESTA          1
151500000000     O                       TES(5)              66
151600000000     O               01                           9 'CLIENTE'
151700000000     O               01                          50 'TARIFFA'
151800000000     O                       TES(6)             132
151900000000     O                       TES(7)             198
152000000000     O               01                         175 'KG/MC    MC/COL'
152100000000     O               01                         196 ' KG/MC   MC/COL'
152200000000     O*------------------------
152300000000     O          E            TESTA          1
152400000000     O                       TES(1)              66
152500000000     O                       TES(1)             132
152600000000     O                       TES(1)             198
152700090709     O          E            rigcom         0
152800090709     O                                        +   3 'Commerciale unificante'
152900090709     o                       depcun              34
153000090709     o                       descun              60
153100090709     O          E            rigcom         1
153200090709     o                       depcun              34
153300090709     o                       descun              60
153400090709     O          E            rigcom         1
153500090709     O                       TES(1)              66
153600090709     O                       TES(1)             132
153700090709     O                       TES(1)             198
153800000000     O*------------------------------------------------------------------------*
153900000000     O          E            RIGCLI         1
154000000000     O                                            1 '!'
154100950629     O                       DEPKSC        Z   +  1
154200000000     O                       DESCLI            +  1
154300030909     O                       DESTTR            +  1
154400980421     O                       DEPCTR            +  1
154500991104     O                       DESCTR            +  1
154600991104     O                       DESDCV            +  1
154700080319     O***                                      +  2 '!'
154800080319     O***                    PARMMF            +  1
154900080319     O***                                      +  1 '!'
155000000517     O                       STPER         K     87
155100000517     O                       STNCL         Z     96
155200000517     O                       STPKB         4    108
155300000517     O                       STNSP         Z    117
155400000517     O                                          119 '!'
155500000517     O                       STVLB         4    131
155600000517     O                       STVLC         4    143
155700000519     O                       STRNG         K    152
155800000718     O*                      STTTP         K    159
155900000517     O                                          160 '!'
156000000517     O                       STCAK         4    168
156100000517     O                       STCAC         4    177
156200000517     O                       STCIK         4    189
156300000517     O                       STCIC         4    196
156400941206     O                                          198 '!'
156500000000     O*------------------------------------------------------------------------*
156600000000     O          E            RIGTOT         1
156700000000     O                                            1 '!'
156800090709     O                       STDES1            +  1
156900090709     O                       STDES2            +  1
157000080319     O***                                      + 28 '!'
157100080319     O***                    PARMMF            +  1
157200080319     O***                                      +  1 '!'
157300000517     O                       STPER         K     87
157400000517     O                       STNCL         Z     96
157500000517     O                       STPKB         4    108
157600000517     O                       STNSP         Z    117
157700000517     O                                          119 '!'
157800000517     O                       STVLB         4    131
157900000517     O                       STVLC         4    143
158000000519     O                       STRNG         K    152
158100040219     O*---                   STTTP         K    159
158200000517     O                                          160 '!'
158300000000     O                                          198 '!'
158400000000     O*------------------------
158500000000     O          E            RIGSEP         1
158600000000     O                       TES(1)              66
158700000000     O                       TES(1)             132
158800000000     O                       TES(1)             198
158900000000     O*------------------------
159000000000     O          E            STAFIN           63
159100000000     O                                           76 '*** FINE STAMPA ***'
159200941206     O*------------------------------------------------------------------------*
159300980421** -TES-   (DESCRIZIONI  STAMPA)
159400000000------------------------------------------------------------------  1
159500000000!                                                                   2
159600080319            ! (B) %RAPP.  NUM.                 NUM. !    VOLUME     3
159700040219   VOLUME    (A) %         !                                     !  4
159800941207!                                                                   5
159900080319            !   COLLI    COLLI        PESO    SPED. ! BOLL./REALE   6
160000080319     VDL     SCOST.        !                                     !  7
160100000000                                                                    8
160200000000                                                                    9
160300080319TOTALE TER.PART                                                    10
160400000000TOTALE AREA                                                        11
160500980421TOTALE DISTRETTO                                                   12
160600000000TOTALE GENERALE                                                    13
160700090709TOTALE COMM.LE                                                     14
160800000000** -INT-   (INTESTAZIONE)
160900000000DETTAGLIO                                                       1
161000000000SINTETICA                                                       2
1611009506293
1612009804214
161300950629R E A L E 5
161400980421** -DEC-   (DECODIFICHE RIGHE)
161500980421ASSEGNATI                                                            1
161600980421FRANCHI                                                              2
161700991102CON IMPIANTO                                                         3
161800991102SENZA IMPIANTO                                                       4
161900000706POSTE                                                                5
162000000706NO POSTE                                                             6
1621000007067
1622000007068
1623000007069
16240000070610
