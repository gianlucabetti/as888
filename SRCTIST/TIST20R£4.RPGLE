000100000000     H*------------------------------------------------------------------------*
000200000000     H* STAMPA STATISTICA VOLUMI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991104     FWFVLM01L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100010314     FTNTAM01L  IF   E           K DISK
001101130207     fwfvpr00f  o    e             disk    usropn
001102130218     fwfvprt0f  o    e             disk    usropn
001200000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300000000     F                                     FORMLEN(66)
001400000000     F                                     FORMOFL(60)
001500000000     L*------------------------------------------------------------------------*
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* INTESTAZIONI STAMPA
002100000000     D*-------------------
002200991104     D TES             S             66    DIM(13) CTDATA PERRCD(1)             RIGHE TESTATA
002300950629     D INT             S              9    DIM(5) CTDATA PERRCD(1)              DECODIFICA INTESTAZI
002400000706     D DEC             S             66    DIM(10) CTDATA PERRCD(1)             DECODIFICHE RIGHE
002500000000     D*-------------------
002600000000     D* SCHIERA FILIALI
002700000000     D*-------------------
002800971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002900971124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
003000020627     D PFIL            S              3    DIM(500)                             *NETWORK POSTE
003100000000     D*------------------------------------------------------------------------*
003200000000     D* INPUT
003300000000     D*------------------------------------------------------------------------*
003400000000     D*-------------------
003500000000     D* RICERCA PARAMETRI UTENTI
003600000000     D*-------------------
003700020919     D TIBS34DS      E DS                                                       *Profili utente
003800020919     D DDATIUTE      E DS                                                       *Dati utente
003900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004000000000     D*-------------------
004100000000     D* ARCHITETTURA
004200000000     D*-------------------
004300000000     D KPJBA         E DS
004400991102     D*-------------------
004500991102     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
004600991102     D*-------------------
004700991102     D OG150         E DS
004800000706     D*-------------------
004900000706     D* LETTURA CAMPO ORGDE£ DI AZORG
005000000706     D*-------------------
005100000706     D OG143         E DS
005200020910     D*------------------
005300020910     D* DS REPEIMENTO TERMINALs
005400020910     D*------------------
005500020910     D FNLV55DS      E DS
005600030909     D*------------------
005700030909     D* DS RIDEFINIZIONE FLAG OPERATIVI TARIFFE
005800030909     D*------------------
005900030909     D DSTA01        E DS
006000020910     D*------------------
006100020910     D* DS CONTROLLO DATA (8)
006200020910     D*------------------
006300020910     D WLBDA8          DS                  INZ
006400020910     D  G08DAT                 1      8  0
006500020910     D  G08INV                 9     16  0
006600020910     D  G08ERR                17     17
006700020910     D  G08TGI                18     22  0
006800000000     D*-------------------
006900000000     D* DS PARAMETRI DI LANCIO PGM
007000000000     D*-------------------
007100950703     D PARAM           DS
007200991029     D  PARTLA                 1      1
007300991029     D  PARANN                 2      5  0
007400991029     D  PARMMI                 6      7  0
007500991029     D  PARMMF                 8      9  0
007600991029     D  PARSNR                10     10
007700991029     D  PARPID                27     31  2
007800991029     D  PARDIV                32     32
007900991029     D  PARARE                33     35  0
008000991029     D  PARFIL                36     38  0
008100991029     D  PARDET                39     39
008200991029     D  PARTOT                40     40
008300991029     D  PARRG1                41     44  1
008400991029     D  PARRG2                45     48  1
008500991029     D  PARPIA                49     53  2
008600991029     D  PARPSD                54     58  2
008700991029     D  PARPSA                59     63  2
008800991229     D  PAROK                 65     66
008900991229     D  PARMIN                67     71  3
009000000706     D  PARPOS                72     72
009100030910     D  PARNT1                73     75
009200030910     D  PARNT2                76     78
009300030910     D  PARNT3                79     81
009400030910     D  PARNT4                82     84
009500050124     D  PARSAL                85     85
009501130207
009502130207     d wdesfil         s                   like(desfil)
009503130207     d Qcmd            s             80    inz
009504130207     d Qlen            s             15  5 inz(%len(Qcmd))
009505130218     d m_wcpncl        s                   like(tftcl)
009506130218     d m_wcpncr        s                   like(tfncr)
009507130218     d p_wcpncl        s                   like(tfnclp)
009508130218     d p_wcpncr        s                   like(tfncrp)
009509130218     d m_stvlb         s                   like(tfvlb)
009510130218     d m_stvlc         s                   like(tfvlc)
009511130218     d p_spvlb         s                   like(tfvlbp)
009512130218     d p_spvlc         s                   like(tfvlcp)
009513130207
009514130207     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
009515130207     d                                                   WFVPR00F)')
009516130218     d C_Cmd_ClrWFt    c                   const('CLRPFM FILE(*LIBL/+
009517130218     d                                                   WFVPRT0F)')
009600000000     C*------------------------------------------------------------------------*
009700000000     C* MAIN LINE
009800000000     C*------------------------------------------------------------------------*
009900000000     C*
010000000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
010100000000     C                   EXSR      SETFIL
010200000000     C*
010300000000     C* CICLO FINO A FINE FILE DI LAVORO
010400000000DO  1C     FINL00        DOWEQ     '0'                                          --- 1 -->
010500000000     C*
010600000000     C* OPERAZIONI PER NUOVA DIVISIONE
010700000000     C                   EXSR      NEWDIV
010800000000     C*
010900000000     C* CICLO FINO A ROTTURA DI DIVISIONE
011000000000DO  2C     FINL00        DOWEQ     '0'                                          --- 2 -->
011100000000     C     WFVDIV        ANDEQ     DEPDIV
011200000000     C*
011300000000     C* OPERAZIONI PER NUOVA AREA
011400000000     C                   EXSR      NEWARE
011500000000     C*
011600000000     C* CICLO FINO A ROTTURA DI AREA
011700000000DO  3C     FINL00        DOWEQ     '0'                                          --- 3 -->
011800000000     C     WFVDIV        ANDEQ     DEPDIV
011900000000     C     WFVARE        ANDEQ     DEPARE
012000000000     C*
012100000000     C* OPERAZIONI PER NUOVA FILIALE
012200000000     C                   EXSR      NEWFIL
012300000000     C*
012400000000     C* CICLO FINO A ROTTURA DI FILIALE
012500000000DO  4C     FINL00        DOWEQ     '0'                                          --- 4 -->
012600000000     C     WFVDIV        ANDEQ     DEPDIV
012700000000     C     WFVARE        ANDEQ     DEPARE
012800000000     C     WFVFIL        ANDEQ     DEPFIL
012900991029     C*
013000991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA
013100991029     C                   EXSR      NEWPRO
013200991029     C*
013300991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA PER PROGRESSIVO
013400991029DO  5C     FINL00        DOWEQ     '0'                                          --- 5 -->
013500991029     C     WFVDIV        ANDEQ     DEPDIV
013600991029     C     WFVARE        ANDEQ     DEPARE
013700991029     C     WFVFIL        ANDEQ     DEPFIL
013800991029     C     WFVKSC        ANDEQ     DEPKSC
013900991029     C     WFVCTR        ANDEQ     DEPCTR
014000000000     C*
014100991029     C* OPERAZIONI PER NUOVO CLIENTE/TARIFFA/MESE
014200000000     C                   EXSR      NEWCLI
014300000000     C*
014400991029     C* CICLO FINO A ROTTURA DI CLIENTE/TARIFFA/MESE PER DETTAGLIO
014500991029DO  6C     FINL00        DOWEQ     '0'                                          --- 6 -->
014600991029     C     WFVDIV        ANDEQ     DEPDIV
014700991029     C     WFVARE        ANDEQ     DEPARE
014800991029     C     WFVFIL        ANDEQ     DEPFIL
014900991029     C     WFVKSC        ANDEQ     DEPKSC
015000991029     C     WFVCTR        ANDEQ     DEPCTR
015100991029     C     WFVMES        ANDEQ     DEPMES
015200000000     C*
015300000000     C* MEMORIZZA LE SPEDIZIONI
015400000517     C     WRECOK        IFEQ      'S'
015500000517     C                   EXSR      MEMSPE
015600000517     C                   ENDIF
015700000000     C*
015800000000     C* LEGGE IL RECORD SUCCESSIVO
015900000000     C                   EXSR      LETFIL
016000000000     C*
016100040220E   6C                   ENDDO                                                  <-- 5 ---
016200000000     C*
016300991029     C* DETTAGLIO: STAMPA IL TOTALE CLIENTE/TARIFFA/MESE
016400000000     C                   EXSR      STACLI
016500000000     C*
016600991029E   5C                   ENDDO                                                  <-- 5 ---
016700991029     C*
016800991029     C* PROGRESSIVO: STAMPA IL TOTALE CLIENTE/TARIFFA AL MESE
016900991029     C                   EXSR      STAPRO
016901130208     c*
016902130208     c                   if        parsnr='S' and flgcli=1
016903130208     c                   write     wfvpr
016904130208     c                   endif
017000991029     C*
017100000000E   4C                   ENDDO                                                  <-- 4 ---
017200000000     C*
017300000000     C* STAMPA IL TOTALE FILIALE
017400000000     C                   EXSR      STAFIL
017500000000     C*
017600000000E   3C                   ENDDO                                                  <-- 3 ---
017700000000     C*
017800000000     C* STAMPA IL TOTALE AREA
017900000000     C     PARTOT        IFEQ      'S'
018000000000     C                   EXSR      STAARE
018100000000     C                   ENDIF
018200000000     C*
018300000000E   2C                   ENDDO                                                  <-- 2 ---
018400000000     C*
018500000000     C* STAMPA IL TOTALE DIVISIONE
018600000000     C     PARTOT        IFEQ      'S'
018700000000     C                   EXSR      STADIV
018800000000     C                   ENDIF
018900000000     C*
019000000000E   1C                   ENDDO                                                  <-- 1 ---
019100000000     C*
019200000000     C* STAMPA IL TOTALE GENERALE
019300000000     C     PARTOT        IFEQ      'S'
019400000000     C                   EXSR      STAGEN
019500000000     C                   ENDIF
019600000000     C*
019700000000     C* STAMPA RIGA FINE STAMPA
019800000000     C                   EXCEPT    STAFIN
019900000000     C*
020000000000     C                   SETON                                        LR
020100000000     C*------------------------------------------------------------------------*
020200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
020300000000     C*------------------------------------------------------------------------*
020400000000     C     SETFIL        BEGSR
020500000000     C*
020600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020700000000     C                   Z-ADD     PARANN        KWVANN
020800000000     C                   MOVEL     *BLANKS       KWVDIV
020900000000     C                   Z-ADD     *ZEROS        KWVARE
021000000000     C                   Z-ADD     *ZEROS        KWVFIL
021100000000     C                   Z-ADD     *ZEROS        KWVKSC
021200000000     C                   Z-ADD     *ZEROS        KWVCTR
021300000000     C                   Z-ADD     *ZEROS        KWVMES
021400950629     C                   MOVEL     *BLANKS       KWVFVB
021500991104     C     KEYWFV        SETLL     WFVLM01L                           99        *FINE-ARCHIV
021600000000     C*
021700000000     C     *IN99         IFEQ      '1'
021800000000     C                   MOVEL     '1'           FINL00
021900000000     C                   ELSE
022000000000     C                   EXSR      LETFIL
022100000000     C                   END
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500941207     C* LETFIL - LETTURA PROSSIMO RECORD
022600000000     C*------------------------------------------------------------------------*
022700000000     C     LETFIL        BEGSR
022800000000     C*
022900000000     C* LEGGO FINO A:
023000000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
023100000000     C*     - TROVATO RECORD VALIDO
023200000000     C                   MOVEL     'N'           WRECOK
023300040211     C                   MOVEL     'N'           WRECOKC
023400000000     C*
023500000000     C* LETTURE SUCCESSIVE ALLA PRIMA
023600991104     C                   READ      WFVLM01L                               99
023700000000     C*
023800000000     C     *IN99         IFEQ      '1'                                          *FINA FILE
023900000000     C     WFVANN        ORNE      PARANN                                       *SUPERATO ANNO
024000000000     C                   MOVEL     '1'           FINL00
024100000000     C                   ELSE
024200000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
024300000517     C                   ENDIF
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
024800000000     C*------------------------------------------------------------------------*
024900000000     C     CHKREC        BEGSR
025000000000     C*
025100000000     C                   MOVEL     'S'           WRECOK            1
025200040211     C                   MOVEL     'S'           WRECOKC           1
025300000000     C*
025400000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
025500000000     C     WFVMES        IFGE      PARMMI
025600000000     C     WFVMES        ANDLE     PARMMF
025700000000     C                   ELSE
025800000000     C                   MOVEL     'N'           WRECOK
025900000000     C                   END
026000000706     C*
026100000706     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
026200000706IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
026300000706     C                   Z-ADD     1             I
026400000706     C     WFVFIL        LOOKUP    FIL(I)                                 95
026500000706IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
026600020627IF  3C     PFIL(I)       IFEQ      'PPT'                                        *FILIALE POSTE
026700000706     C                   MOVEL     'N'           WRECOK
026800000706E   3C                   ENDIF
026900000706X   2C                   ELSE                                                   *SI DATI POSTE
027000020627IF  3C     PFIL(I)       IFNE      'PPT'                                        *NO FILIALE POSTE
027100000706     C                   MOVEL     'N'           WRECOK
027200000706E   3C                   ENDIF
027300000706E   2C                   ENDIF
027400000706E   1C                   ENDIF
027500000000     C*
027600000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
027700000000     C     PARDIV        IFNE      *BLANKS
027800000000     C     WFVDIV        IFNE      PARDIV
027900000000     C                   MOVEL     'N'           WRECOK
028000000000     C                   END
028100000000     C                   END
028200000000     C*
028300000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
028400000000     C     PARARE        IFNE      *ZEROS
028500000000     C     WFVARE        IFNE      PARARE
028600000000     C                   MOVEL     'N'           WRECOK
028700000000     C                   END
028800000000     C                   END
028900000000     C*
029000000000     C* SE FILIALE DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
029100000000     C     PARFIL        IFNE      *ZEROS
029200000000     C     WFVFIL        IFNE      PARFIL
029300000000     C                   MOVEL     'N'           WRECOK
029400000000     C                   END
029500000000     C                   END
029600950705     C*
029700950705     C* CALCOLA PERCENTUALE VOLUME CML SU VOL.AUTOM.
029800001009     C                   Z-ADD     WFVVLB        WCPVLB           15 3
029900001009     C                   Z-ADD     WFVVLC        WCPVLC           15 3
030000991102     C                   EXSR      CALRNG
030100991102     C                   Z-ADD     WCPRVA        WRNG              6 2
030200991102     C*
030300991104     C* CALCOLA RAPPORTO COLLI CML SU COLLI BOLL.
030400001009     C                   Z-ADD     WFVTCL        WCPNCL           15 3
030500001009     C                   Z-ADD     WFVNCR        WCPNCR           15 3
030600991102     C                   EXSR      CALPER
030700991102     C                   Z-ADD     WCPPVA        WPER              6 2
030800000000     C*
030900991104     C* SE IL RAPPORTO COLLI VDL CON/SENZA IMPIANTO NO IN RANGE RICHIESTO, SCARTO
031000040211     c* CONSIDERAZIONI SOLO PER RIGA DETTAGLIO
031100991102     C     WCPPVA        IFLT      PARXDA
031200991229     C     WCPPVA        OREQ      *ZEROS
031300040211     C                   MOVEL     'N'           WRECOKC
031400000000     C                   END
031500991102     C*
031600991229     C* SE LA PERCENTUALE (A) SCOST. VOL NON COMPRESA IN RANGE RICHIESTO,SCARTO
031700991229     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
031800991229     C* DELLO SCARTO MINIMO (PARMIN)
031900991229     C                   Z-ADD     *zeros        PERC              6 2
032000991229     C                   EVAL      PERC=%abs(WCPRVA)
032100991229     C     WCPRVA        IFLT      PARRG1
032200991229     C     WCPRVA        ORGT      PARRG2
032300991229     C     WCPRVA        OREQ      *ZEROS
032400991229     C     PERC          ORLT      PARMIN
032500040211     C                   MOVEL     'N'           WRECOKC
032600991102     C                   END
032700030910     C*
032800030910     C* CONTROLLO SE RICHIESTE SELEZIONI X NETWORK TARIFFE
032900030910     C                   IF        WRECOK <> 'N'
033000030910     C                   IF        PARNT1 <> *blanks OR
033100030910     C                             PARNT2 <> *blanks OR
033200030910     C                             PARNT3 <> *blanks OR
033300030910     C                             PARNT4 <> *blanks
033400030910     C* SE RICHIESTE SELEZIONI VERIFICO
033500030910     C                   MOVEL     *blanks       wNTWTAR           3
033600030910     C* NEGO I FLAG X POI EVENTUALMENTE ATTIVARLO SE RISPONDENTE AI PARAMETRI RICHIESTI
033700030910     C                   MOVEL     'N'           WRECOK
033800030910     C* REPERISCO IL NETWORK DELLA TARIFFA DEL RECORD CORRENTE SE NN TRATTASI DI TARIFFE 888 O 999
033900030910     C     WFVCTR        IFNE      888
034000030910     C     WFVCTR        ANDNE     999
034100030910     C                   Z-ADD     WFVKSC        KTFKSC
034200030910     C                   Z-ADD     WFVCTR        KTFCTR
034300030910     C                   Z-ADD     *ZEROS        KTFPRG
034400030910     C     KEYTAM        SETLL     TNTAM01L                           99
034500030910     C  N99              READ      TNTAM01L                               99
034600030910     C     *IN99         DOWEQ     '0'
034700030910     C     TAMKSC        ANDEQ     KTFKSC
034800030910     C     TAMCTR        ANDEQ     KTFCTR
034900030910     C     TAMATB        IFEQ      ' '
035000030910     C                   MOVEL(P)  TAMFLO        DSTA01
035100030910     C                   IF        TAMFIE  = 'I' AND §TADPD <> 'S'
035200030910     C                   EVAL      wNTWTAR = 'ITA'
035300030910     C                   ENDIF
035400030910     C                   IF        TAMFIE  = 'I' AND §TADPD  = 'S'
035500030910     C                   EVAL      wNTWTAR = 'DPD'
035600030910     C                   ENDIF
035700030910     C                   IF        TAMFIE  = 'E' AND §TAFED <> 'S'
035800030910     C                   EVAL      wNTWTAR = 'EEX'
035900030910     C                   ENDIF
036000030910     C                   IF        TAMFIE  = 'E' AND §TAFED  = 'S'
036100030910     C                   EVAL      wNTWTAR = 'FED'
036200030910     C                   ENDIF
036300030910     C                   ENDIF
036400030910     C                   READ      TNTAM01L                               99
036500030910     C                   ENDDO
036600030910     C                   ENDIF
036700030910     C* EFFETTUO CONSIDERAZIONI
036800030910     C                   IF        PARNT1 = 'ITA' OR
036900030910     C                             PARNT2 = 'ITA' OR
037000030910     C                             PARNT3 = 'ITA' OR
037100030910     C                             PARNT4 = 'ITA'
037200030910     C                   IF        wNTWTAR = 'ITA' OR
037300030910     C                             WFVCTR = 888    OR
037400030910     C                             WFVCTR = 999
037500030910     C                   MOVEL     'S'           WRECOK
037600030910     C                   ENDIF
037700030910     C                   ENDIF
037800030910     C                   IF        PARNT1 = 'EEX' OR
037900030910     C                             PARNT2 = 'EEX' OR
038000030910     C                             PARNT3 = 'EEX' OR
038100030910     C                             PARNT4 = 'EEX'
038200030910     C                   IF        wNTWTAR = 'EEX'
038300030910     C                   MOVEL     'S'           WRECOK
038400030910     C                   ENDIF
038500030910     C                   ENDIF
038600030910     C                   IF        PARNT1 = 'DPD' OR
038700030910     C                             PARNT2 = 'DPD' OR
038800030910     C                             PARNT3 = 'DPD' OR
038900030910     C                             PARNT4 = 'DPD'
039000030910     C                   IF        wNTWTAR = 'DPD'
039100030910     C                   MOVEL     'S'           WRECOK
039200030910     C                   ENDIF
039300030910     C                   ENDIF
039400030910     C                   IF        PARNT1 = 'FED' OR
039500030910     C                             PARNT2 = 'FED' OR
039600030910     C                             PARNT3 = 'FED' OR
039700030910     C                             PARNT4 = 'FED'
039800030910     C                   IF        wNTWTAR = 'FED'
039900030910     C                   MOVEL     'S'           WRECOK
040000030910     C                   ENDIF
040100030910     C                   ENDIF
040200030910     C*
040300030910     C                   ENDIF
040400030910     C                   ENDIF
040500000000     C*
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
040900000000     C*------------------------------------------------------------------------*
041000000000     C     MEMSPE        BEGSR
041100040212     C*---
041200040212     c* MESE
041300040212     C*---
041400040212IF  1C     DEPMES        IFEQ      PARMMF
041500000000     C* TOTALE CLIENTE/TARIFFA
041600040212IF  2C     WRECOKC       IFEQ      'S'
041700040216     c                   ADD       WFVTCL        TCTCL
041800040216     C                   ADD       WFVNCL        TCNCL
041900040216     C                   ADD       WFVNCR        TCNCR
042000040216     C                   ADD       WFVPKB        TCPKB
042100040216     C                   ADD       WFVNSP        TCNSP
042200040216     C                   ADD       WFVVLB        TCVLB
042300040216     C                   ADD       WFVVLC        TCVLC
042400040216E   2C                   ENDIF
042500040216     C                   ADD       WFVVLB        TCVLBT
042600040216     C                   ADD       WFVVLC        TCVLCT
042700000000     C* TOTALE FILIALE
042800040212     C                   ADD       WFVTCL        TFTCL
042900040216     C                   ADD       WFVNCL        TFNCL
043000040216     C                   ADD       WFVNCR        TFNCR
043100040216     C                   ADD       WFVPKB        TFPKB
043200040216     C                   ADD       WFVNSP        TFNSP
043300991102     C                   ADD       WFVVLB        TFVLB
043400991102     C                   ADD       WFVVLC        TFVLC
043500040212     C                   ADD       WFVVLB        TFVLBT
043600040212     C                   ADD       WFVVLC        TFVLCT
043700000000     C* TOTALE AREA
043800040212     C                   ADD       WFVTCL        TATCL
043900040212     C                   ADD       WFVNCL        TANCL
044000991102     C                   ADD       WFVNCR        TANCR
044100991102     C                   ADD       WFVPKB        TAPKB
044200040212     C                   ADD       WFVNSP        TANSP
044300991102     C                   ADD       WFVVLB        TAVLB
044400991102     C                   ADD       WFVVLC        TAVLC
044500040212     C                   ADD       WFVVLB        TAVLBT
044600040212     C                   ADD       WFVVLC        TAVLCT
044700000000     C* TOTALE DIVISIONE
044800040212     C                   ADD       WFVTCL        TDTCL
044900040216     C                   ADD       WFVNCL        TDNCL
045000040216     C                   ADD       WFVNCR        TDNCR
045100040216     C                   ADD       WFVPKB        TDPKB
045200040216     C                   ADD       WFVNSP        TDNSP
045300040216     C                   ADD       WFVVLB        TDVLB
045400991102     C                   ADD       WFVVLC        TDVLC
045500040212     C                   ADD       WFVVLB        TDVLBT
045600040212     C                   ADD       WFVVLC        TDVLCT
045700000000     C* TOTALE GENERALE
045800040212     C                   ADD       WFVTCL        TGTCL             9 0
045900040212     C                   ADD       WFVNCL        TGNCL             9 0
046000991102     C                   ADD       WFVNCR        TGNCR             9 0
046100991102     C                   ADD       WFVPKB        TGPKB            10 1
046200991102     C                   ADD       WFVNSP        TGNSP             8 0
046300001009     C                   ADD       WFVVLB        TGVLB            15 3
046400001009     C                   ADD       WFVVLC        TGVLC            15 3
046500040212     C                   ADD       WFVVLB        TGVLBT           15 3
046600040212     C                   ADD       WFVVLC        TGVLCT           15 3
046700991102     C*
046800040212E   1C                   ENDIF
046900040212     C*---
047000040212     c* PROGRESSIVO
047100040212     C*---
047200991102     C* TOTALE CLIENTE/TARIFFA
047300040211IF  1C     WRECOKC       IFEQ      'S'
047400991104     C                   ADD       WFVTCL        TPTCL
047500991104     C                   ADD       WFVNCL        TPNCL
047600991102     C                   ADD       WFVNCR        TPNCR
047700991102     C                   ADD       WFVPKB        TPPKB
047800991102     C                   ADD       WFVNSP        TPNSP
047900991102     C                   ADD       WFVVLB        TPVLB
048000991102     C                   ADD       WFVVLC        TPVLC
048100040211E   1C                   ENDIF
048200040212     C                   ADD       WFVVLB        TPVLBT
048300040212     C                   ADD       WFVVLC        TPVLCT
048400040212     C* TOTALE FILIALE
048500040212     C                   ADD       WFVVLB        TFVLBP
048600040212     C                   ADD       WFVVLC        TFVLCP
048700040212     C                   ADD       WFVNCR        TFNCRP
048800040212     C                   ADD       WFVTCL        TFTCLP
048900040212     C                   ADD       WFVNCL        TFNCLP
049000040212     C                   ADD       WFVPKB        TFPKBP
049100040212     C                   ADD       WFVNSP        TFNSPP
049200040212     C                   ADD       WFVVLB        TFVLBPT
049300040212     C                   ADD       WFVVLC        TFVLCPT
049400040212     C* TOTALE AREA
049500040212     C                   ADD       WFVVLB        TAVLBP
049600040212     C                   ADD       WFVVLC        TAVLCP
049700040212     C                   ADD       WFVNCR        TANCRP
049800040212     C                   ADD       WFVTCL        TATCLP
049900040212     C                   ADD       WFVNCL        TANCLP
050000040212     C                   ADD       WFVPKB        TAPKBP
050100040212     C                   ADD       WFVNSP        TANSPP
050200040212     C                   ADD       WFVVLB        TAVLBPT
050300040212     C                   ADD       WFVVLC        TAVLCPT
050400040212     C* TOTALE DIVISIONE
050500040212     C                   ADD       WFVVLB        TDVLBP
050600040216     C                   ADD       WFVVLC        TDVLCP
050700040216     C                   ADD       WFVNCR        TDNCRP
050800040216     C                   ADD       WFVTCL        TDTCLP
050900040216     C                   ADD       WFVNCL        TDNCLP
051000040216     C                   ADD       WFVPKB        TDPKBP
051100040216     C                   ADD       WFVNSP        TDNSPP
051200040212     C                   ADD       WFVVLB        TDVLBPT
051300040212     C                   ADD       WFVVLC        TDVLCPT
051400040212     C* TOTALE GENERALE
051500040212     C                   ADD       WFVVLB        TGVLBP           15 3
051600040212     C                   ADD       WFVVLC        TGVLCP           15 3
051700040212     C                   ADD       WFVNCR        TGNCRP            9 0
051800040212     C                   ADD       WFVTCL        TGTCLP            9 0
051900040212     C                   ADD       WFVNCL        TGNCLP            9 0
052000040212     C                   ADD       WFVPKB        TGPKBP           10 1
052100040212     C                   ADD       WFVNSP        TGNSPP            8 0
052200040212     C                   ADD       WFVVLB        TGVLBPT          15 3
052300040212     C                   ADD       WFVVLC        TGVLCPT          15 3
052400040212     C*
052500040212     C                   ENDSR
052600000000     C*------------------------------------------------------------------------*
052700000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
052800000000     C*------------------------------------------------------------------------*
052900000000     C     NEWDIV        BEGSR
053000000000     C*
053100000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
053200000000     C                   MOVEL     WFVDIV        DEPDIV
053300000000     C*
053400000000     C* DECODIFICA LA DIVISIONE
053500000000     C                   EXSR      DECDIV
053600000000     C*
053700000000     C* AZZERA I TOTALI DELLA DIVISIONE
053800001009     C                   Z-ADD     *ZEROS        TDVLB            15 3
053900001009     C                   Z-ADD     *ZEROS        TDVLC            15 3
054000991029     C                   Z-ADD     *ZEROS        TDNCR             9 0
054100991104     C                   Z-ADD     *ZEROS        TDTCL             9 0
054200991104     C                   Z-ADD     *ZEROS        TDNCL             9 0
054300991102     C                   Z-ADD     *ZEROS        TDPKB            10 1
054400941207     C                   Z-ADD     *ZEROS        TDNSP             8 0
054500040212     C*
054600001009     C                   Z-ADD     *ZEROS        TDVLBP           15 3
054700001009     C                   Z-ADD     *ZEROS        TDVLCP           15 3
054800991102     C                   Z-ADD     *ZEROS        TDNCRP            9 0
054900991104     C                   Z-ADD     *ZEROS        TDTCLP            9 0
055000991104     C                   Z-ADD     *ZEROS        TDNCLP            9 0
055100991102     C                   Z-ADD     *ZEROS        TDPKBP           10 1
055200991102     C                   Z-ADD     *ZEROS        TDNSPP            8 0
055300040212     C*
055400001009     C                   Z-ADD     *ZEROS        TDVLBT           15 3
055500001009     C                   Z-ADD     *ZEROS        TDVLCT           15 3
055600001009     C                   Z-ADD     *ZEROS        TDVLBPT          15 3
055700001009     C                   Z-ADD     *ZEROS        TDVLCPT          15 3
055800000000     C*
055900000000     C                   ENDSR
056000000000     C*------------------------------------------------------------------------*
056100000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
056200000000     C*------------------------------------------------------------------------*
056300000000     C     NEWARE        BEGSR
056400000000     C*
056500000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
056600000000     C                   Z-ADD     WFVARE        DEPARE
056700000000     C*
056800000000     C* DECODIFICA L'AREA
056900000000     C                   EXSR      DECARE
057000000000     C*
057100000000     C* AZZERA I TOTALI DELL'AREA
057200040216     C                   MOVE      *ZEROS        TAVLB            15 3
057300040216     C                   MOVE      *ZEROS        TAVLC            15 3
057400040216     C                   MOVE      *ZEROS        TANCR             9 0
057500040216     C                   MOVE      *ZEROS        TATCL             9 0
057600040216     C                   MOVE      *ZEROS        TANCL             9 0
057700040216     C                   MOVE      *ZEROS        TAPKB            10 1
057800941207     C                   MOVE      *ZEROS        TANSP             8 0
057900040212     C*
058000001009     C                   MOVE      *ZEROS        TAVLBP           15 3
058100001009     C                   MOVE      *ZEROS        TAVLCP           15 3
058200991102     C                   MOVE      *ZEROS        TANCRP            9 0
058300991104     C                   MOVE      *ZEROS        TATCLP            9 0
058400991104     C                   MOVE      *ZEROS        TANCLP            9 0
058500991104     C                   MOVE      *ZEROS        TAPKBP           10 1
058600991102     C                   MOVE      *ZEROS        TANSPP            8 0
058700040212     C*
058800001009     C                   MOVE      *ZEROS        TAVLBT           15 3
058900001009     C                   MOVE      *ZEROS        TAVLCT           15 3
059000001009     C                   MOVE      *ZEROS        TAVLBPT          15 3
059100001009     C                   MOVE      *ZEROS        TAVLCPT          15 3
059200000000     C*
059300000000     C                   ENDSR
059400000000     C*------------------------------------------------------------------------*
059500000000     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
059600000000     C*------------------------------------------------------------------------*
059700000000     C     NEWFIL        BEGSR
059800000000     C*--------------------
059900000000     C* T E S T A T A
060000000000     C*--------------------
060100000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
060200000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
060300000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
060400000000     C*         RIGHE DI TOTALE
060500000000     C*
060600000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
060700000000     C                   Z-ADD     WFVFIL        DEPFIL
060800020910     C*
060900020910     C* REPERISCE IL TERMINAL PARTENZA
061000020910     C                   CLEAR                   FNLV55DS
061100020910     C                   MOVEL     'P'           D55TPT
061200020910     C                   MOVEL     DEPFIL        D55LIN
061300020910     C                   Z-ADD     DATCOR        D55DRF
061400020910     C                   CALL      'FNLV55R'
061500020910     C                   PARM                    FNLV55DS
061600020910IF  1C     D55ERR        IFNE      *BLANKS
061700020910     C                   MOVEL     DEPFIL        D55TFP
061800020910E   1C                   ENDIF
061900020910     C                   Z-ADD     D55TFP        DEPTFP
062000991102     C*
062100020910     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
062200020910     C     DEPTFP        CHAIN     AZORG01L                           88
062300991102     C     *IN88         IFEQ      *OFF
062400991102     C                   MOVEL     ORGDF0        OG150
062500991104     C     §OGVOL        IFNE      *BLANKS
062501080310     C     §OGVOL        andNE     *all'0'
062600991102     C                   MOVEL     DEC(3)        CHKCML           20
062700991102     C                   Z-ADD     PARPID        PARXDA            5 2
062800991104     C*                    Z-ADDPARPIA    PARXXA  52
062900991102     C                   ELSE
063000991102     C                   MOVEL     DEC(4)        CHKCML           20
063100991102     C                   Z-ADD     PARPSD        PARXDA            5 2
063200991104     C*                    Z-ADDPARPSA    PARXXA  52
063300991102     C                   ENDIF
063400991102     C                   ELSE
063500991102     C                   MOVEL     DEC(4)        CHKCML           20
063600991102     C                   Z-ADD     PARPSD        PARXDA            5 2
063700991104     C*                    Z-ADDPARPSA    PARXXA  52
063800991102     C                   ENDIF
063900000000     C*
064000000000     C* AZZERA I TOTALI DELLA FILIALE
064100001009     C                   Z-ADD     *ZEROS        TFVLB            15 3
064200001009     C                   Z-ADD     *ZEROS        TFVLC            15 3
064300991029     C                   Z-ADD     *ZEROS        TFNCR             9 0
064400991104     C                   Z-ADD     *ZEROS        TFTCL             9 0
064500991104     C                   Z-ADD     *ZEROS        TFNCL             9 0
064600991102     C                   Z-ADD     *ZEROS        TFPKB            10 1
064700941207     C                   Z-ADD     *ZEROS        TFNSP             8 0
064800040212     c*
064900001009     C                   Z-ADD     *ZEROS        TFVLBP           15 3
065000001009     C                   Z-ADD     *ZEROS        TFVLCP           15 3
065100991102     C                   Z-ADD     *ZEROS        TFNCRP            9 0
065200991104     C                   Z-ADD     *ZEROS        TFTCLP            9 0
065300991104     C                   Z-ADD     *ZEROS        TFNCLP            9 0
065400991102     C                   Z-ADD     *ZEROS        TFPKBP           10 1
065500991102     C                   Z-ADD     *ZEROS        TFNSPP            8 0
065600040212     c*
065700001009     C                   Z-ADD     *ZEROS        TFVLBT           15 3
065800001009     C                   Z-ADD     *ZEROS        TFVLCT           15 3
065900001009     C                   Z-ADD     *ZEROS        TFVLBPT          15 3
066000001009     C                   Z-ADD     *ZEROS        TFVLCPT          15 3
066100000000     C*
066200000000     C* DECODIFICA LA FILIALE
066300000000     C                   EXSR      DECFIL
066400000000     C*
066500000000     C* IMPOSTAZIONI CAMPI IN TESTATA
066600000000IF  1C     PARDET        IFEQ      'S'                                          *SINTETICA
066700000000     C                   MOVEL     *BLANKS       DESFIL
066800991102     C                   MOVEL     *BLANKS       CHKCML
066900000000E   1C                   ELSE                                                   *DETTAGLIO
067000000000     C                   SETON                                        01
067100000000E   1C                   ENDIF
067200000000     C*
067300000000     C* STAMPA TESTATA SU FOGLIO SUCCESSIVO
067400000000IF  1C     *INOF         IFEQ      *ON                                          *FINE PAGINA
067500000000     C     PARDET        OREQ      'D'                                          *STAMPA IN DETTAGLIO
067600000000     C                   EXCEPT    TESTA
067700000000     C                   SETOFF                                       OF
067800000000E   1C                   ENDIF
067900000000     C*
068000000000     C                   ENDSR
068100000000     C*------------------------------------------------------------------------*
068200000000     C* NEWCLI - OPERAZIONI PER NUOVO CLIENTE/TARIFFA
068300000000     C*------------------------------------------------------------------------*
068400000000     C     NEWCLI        BEGSR
068500000000     C*
068600000000     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
068700040216     C                   MOVEL     WFVRAG        DESCLI
068800040216     C                   Z-ADD     WFVKSC        DEPKSC
068900000000     C                   Z-ADD     WFVCTR        DEPCTR
069000040216     C                   Z-ADD     WFVMES        DEPMES
069100000000     C*
069200040219     C* AZZERA I TOTALI DEL CLIENTE/TARIFFA
069300001009     C                   Z-ADD     *ZEROS        TCVLB            15 3
069400001009     C                   Z-ADD     *ZEROS        TCVLC            15 3
069500991029     C                   Z-ADD     *ZEROS        TCNCR             9 0
069600991104     C                   Z-ADD     *ZEROS        TCTCL             9 0
069700991104     C                   Z-ADD     *ZEROS        TCNCL             9 0
069800991102     C                   Z-ADD     *ZEROS        TCPKB            10 1
069900950629     C                   Z-ADD     *ZEROS        TCNSP             8 0
070000040212     C*
070100001009     C                   Z-ADD     *ZEROS        TCVLBT           15 3
070200001009     C                   Z-ADD     *ZEROS        TCVLCT           15 3
070300040220     c*
070400040220     C     depmes        ifle      parmmf
070500040220     C                   Z-ADD     0             FLGCLI            1 0
070600040220     c                   endif
070700950629     C*
070800000000     C                   ENDSR
070900991029     C*------------------------------------------------------------------------*
071000991029     C* NEWPRO - OPERAZIONI PER NUOVO CLIENTE/TARIFFA (X PROGRESSIVO)
071100991029     C*------------------------------------------------------------------------*
071200991029     C     NEWPRO        BEGSR
071300991029     C*
071400991029     C* MEMORIZZA NUOVO CLIENTE/TARIFFA IN DEPOSITO
071500991029     C                   Z-ADD     WFVKSC        DEPKSC
071600991029     C                   Z-ADD     WFVCTR        DEPCTR
071700991029     C*
071800040219     C* AZZERA I TOTALI PER IL PROGRESSIVO MESE DEL CLI/TAR VOL
071900991104     C                   Z-ADD     *ZEROS        TPTCL             9 0
072000991104     C                   Z-ADD     *ZEROS        TPNCL             9 0
072100991029     C                   Z-ADD     *ZEROS        TPNCR             9 0
072200991029     C                   Z-ADD     *ZEROS        TPNSP             8 0
072300991102     C                   Z-ADD     *ZEROS        TPPKB            10 1
072400001009     C                   Z-ADD     *ZEROS        TPVLC            15 3
072500001009     C                   Z-ADD     *ZEROS        TPVLB            15 3
072600040212     C*
072700001009     C                   Z-ADD     *ZEROS        TPVLCT           15 3
072800001009     C                   Z-ADD     *ZEROS        TPVLBT           15 3
072900040212     c*
073000991029     C                   ENDSR
073100000000     C*------------------------------------------------------------------------*
073200000000     C* STACLI - STAMPA TOTALI CLIENTE/TARIFFA
073300000000     C*------------------------------------------------------------------------*
073400000000     C     STACLI        BEGSR
073500950629     C*
073600040219     C* PER NON STAMPARE DUE VOLTE I DATI DEL CLIENTE SE HA DATI
073700950629     C                   MOVEL     '0'           $STDAC
073800000000     C*
073900040212     C* IMPOSTA I VALORI DI STAMPA
074000040216IF  1C     TCVLB         IFNE      *ZEROS
074100950629     C     TCVLC         ORNE      *ZEROS
074200950629     C*
074300950629     C* DECODIFICA LA TARIFFA
074400950629     C                   MOVEL     *BLANKS       DESCTR
074500950629     C                   EXSR      DECCTR
074600001009     C                   Z-ADD     TCVLB         STVLB             9 2
074700001009     C                   Z-ADD     TCVLC         STVLC             9 2
074800991029     C                   Z-ADD     TCNCR         STNCR             9 0
074900991105     C                   Z-ADD     TCTCL         STTCL             9 0
075000991105     C                   Z-ADD     TCNCL         STNCL             9 0
075100991102     C                   Z-ADD     TCPKB         STPKB            10 1
075200941207     C                   Z-ADD     TCNSP         STNSP             8 0
075300950705     C*
075400040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME
075500001009     C                   Z-ADD     TCVLB         WCPVLB           15 3
075600001009     C                   Z-ADD     TCVLC         WCPVLC           15 3
075700991102     C                   Z-ADD     *ZEROS        WCPRVA
075800991102     C                   EXSR      CALRNG
075900991102     C                   Z-ADD     WCPRVA        WRNG              6 2
076000000519     C                   Z-ADD     WRNG          STRNG             4 1
076100000517     C*
076200040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME  TOTALI
076300001009     C                   Z-ADD     TCVLBT        WCPVLB           15 3
076400001009     C                   Z-ADD     TCVLCT        WCPVLC           15 3
076500000517     C                   Z-ADD     *ZEROS        WCPRVA
076600000517     C                   EXSR      CALRNG
076700000517     C                   Z-ADD     WCPRVA        WRNG              6 2
076800000718     C                   Z-ADD     WRNG          STTTP             4 1
076900991102     C*
077000991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLLI BOLL.
077100001009     C                   Z-ADD     TCTCL         WCPNCL           15 3
077200001009     C                   Z-ADD     TCNCR         WCPNCR           15 3
077300991102     C                   Z-ADD     *ZEROS        WCPPVA
077400991102     C                   EXSR      CALPER
077500991102     C                   Z-ADD     WCPPVA        WPER              6 2
077600000517     C                   Z-ADD     WPER          STPER             4 1
077700950629     C*
077800991029     C* STAMPA RIGA CLIENTE/TARIFFA SOLO SE MESE E' ULTIMO DEL RANGE
077900040212IF  2C     DEPMES        IFEQ      PARMMF
078000950629     C                   EXSR      STRCLI
078100950629     C                   MOVEL     '1'           $STDAC
078200950629E   2C                   ENDIF
078300040212E   1C                   ENDIF
078400000000     C*
078500000000     C                   ENDSR
078600991029     C*------------------------------------------------------------------------*
078700991029     C* STAPRO - STAMPA TOTALI CLIENTE/TARIFFA X PROGRESSIVO
078800991029     C*------------------------------------------------------------------------*
078900991029     C     STAPRO        BEGSR
079000991029     C*
079100040219     C* PER NON STAMPARE DUE VOLTE I DATI DEL CLIENTE SE HA DATI
079200991102     C                   MOVEL     '0'           $STDAC
079300991102     C*
079400040212     C* IMPOSTA I VALORI DI STAMPA
079500040212IF  1C     TPVLB         IFNE      *ZEROS
079600991102     C     TPVLC         ORNE      *ZEROS
079700991102     C*
079800991102     C* ASSEGNAZIONE VALORI IN STAMPA
079900001009     C                   Z-ADD     TPVLB         STVLB             9 2
080000001009     C                   Z-ADD     TPVLC         STVLC             9 2
080100991102     C                   Z-ADD     TPNCR         STNCR             9 0
080200991105     C                   Z-ADD     TPTCL         STTCL             9 0
080300991105     C                   Z-ADD     TPNCL         STNCL             9 0
080400991102     C                   Z-ADD     TPPKB         STPKB            10 1
080500991102     C                   Z-ADD     TPNSP         STNSP             8 0
080600001009     C                   Z-ADD     TPVLB         SPVLB             9 2
080700040219     C                   Z-ADD     TPVLC         SPVLC             9 2
080800010222     C                   Z-ADD     TPVLB         SPVLBW           15 3
080900010222     C                   Z-ADD     TPVLC         SPVLCW           15 3
081000991102     C                   Z-ADD     TPNCR         SPNCR             9 0
081100991105     C                   Z-ADD     TPTCL         SPTCL             9 0
081200991105     C                   Z-ADD     TPNCL         SPNCL             9 0
081300991102     C                   Z-ADD     TPPKB         SPPKB            10 1
081400991102     C                   Z-ADD     TPNSP         SPNSP             8 0
081500991102     C*
081600040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME
081700001009     C                   Z-ADD     TPVLB         WCPVLB           15 3
081800001009     C                   Z-ADD     TPVLC         WCPVLC           15 3
081900991102     C                   Z-ADD     *ZEROS        WCPRVA
082000991102     C                   EXSR      CALRNG
082100991102     C                   Z-ADD     WCPRVA        WRNG
082200000519     C                   Z-ADD     WRNG          SPRNG             4 1
082300000517     C*
082400040219     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME  TOTALE
082500001009     C                   Z-ADD     TPVLBT        WCPVLB           15 3
082600001009     C                   Z-ADD     TPVLCT        WCPVLC           15 3
082700000517     C                   Z-ADD     *ZEROS        WCPRVA
082800000517     C                   EXSR      CALRNG
082900000517     C                   Z-ADD     WCPRVA        WRNG
083000000519     C                   Z-ADD     WRNG          SPTTP             4 1
083100991102     C*
083200991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
083300001009     C                   Z-ADD     TPTCL         WCPNCL           15 3
083400001009     C                   Z-ADD     TPNCR         WCPNCR           15 3
083500991102     C                   Z-ADD     *ZEROS        WCPPVA
083600991102     C                   EXSR      CALPER
083700991102     C                   Z-ADD     WCPPVA        WPER
083800000517     C                   Z-ADD     WPER          SPPER             4 1
083900991102     C*
084000040220IF  2C     FLGCLI        IFEQ      1
084100991102     C                   EXSR      STRPRO
084200991102     C                   MOVEL     '1'           $STDAC
084201130208
084202130208     c                   if        parsnr='S'
084203130208     c                   exsr      riewfvpr_p
084204130208     c                   endif
084205130208
084300040219E   2C                   ENDIF
084400040212E   1C                   ENDIF
084500991029     C*
084600991029     C                   ENDSR
084700000000     C*------------------------------------------------------------------------*
084800000000     C* STAFIL - STAMPA TOTALI FILIALE
084900000000     C*------------------------------------------------------------------------*
085000000000     C     STAFIL        BEGSR
085100000000     C*
085200000000     C* SE NON CI SONO VOLUMI, NON STAMPO
085300040212IF  1C     TFVLB         IFEQ      *ZEROS                                       --- 1 -->
085400000000     C     TFVLC         ANDEQ     *ZEROS
085500040212X   1C                   ELSE                                                   <-- 1 -->
085600000000     C*
085700000000     C* DESCRIZIONE TOTALE
085800991102     C                   MOVEL     TES(10)       STDES1           22
085900000000     C*
086000000000     C* DECODIFICA LA FILIALE
086100000000     C                   EXSR      DECFIL
086200000000     C                   MOVEL     DESFIL        STDES2           20
086300000000     C*
086400000000     C* IMPOSTO I VALORI DI STAMPA
086500941207     C                   Z-ADD     TFVLB         STVLB
086600000000     C                   Z-ADD     TFVLC         STVLC
086601130218     C                   Z-ADD     TFVLB         m_STVLB
086602130218     C                   Z-ADD     TFVLC         m_STVLC
086700991105     C                   Z-ADD     TFTCL         STTCL
086800991105     C                   Z-ADD     TFNCL         STNCL
086900000000     C                   Z-ADD     TFPKB         STPKB
087000000000     C                   Z-ADD     TFNSP         STNSP
087100991102     C                   Z-ADD     TFVLBP        SPVLB
087200991102     C                   Z-ADD     TFVLCP        SPVLC
087201130218     C                   Z-ADD     TFVLBP        p_SPVLB
087202130218     C                   Z-ADD     TFVLCP        p_SPVLC
087300010222     C                   Z-ADD     TFVLBP        SPVLBW
087400010222     C                   Z-ADD     TFVLCP        SPVLCW
087500991105     C                   Z-ADD     TFTCLP        SPTCL
087600991105     C                   Z-ADD     TFNCLP        SPNCL
087700991102     C                   Z-ADD     TFPKBP        SPPKB
087800991102     C                   Z-ADD     TFNSPP        SPNSP
087900991102     C*
088000991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
088100001009     C                   Z-ADD     TFVLB         WCPVLB           15 3
088200001009     C                   Z-ADD     TFVLC         WCPVLC           15 3
088300991102     C                   Z-ADD     *ZEROS        WCPRVA
088400991102     C                   EXSR      CALRNG
088500991102     C                   Z-ADD     WCPRVA        WRNG
088600000519     C                   Z-ADD     WRNG          STRNG             4 1
088700000517     C*
088800000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
088900001009     C                   Z-ADD     TFVLBT        WCPVLB           15 3
089000001009     C                   Z-ADD     TFVLCT        WCPVLC           15 3
089100000517     C                   Z-ADD     *ZEROS        WCPRVA
089200000517     C                   EXSR      CALRNG
089300000517     C                   Z-ADD     WCPRVA        WRNG
089400000519     C                   Z-ADD     WRNG          STTTP             4 1
089500991102     C*
089600991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
089700001009     C                   Z-ADD     TFTCL         WCPNCL           15 3
089800001009     C                   Z-ADD     TFNCR         WCPNCR           15 3
089900991102     C                   Z-ADD     *ZEROS        WCPPVA
090000991102     C                   EXSR      CALPER
090100991102     C                   Z-ADD     WCPPVA        WPER
090200000517     C                   Z-ADD     WPER          STPER             4 1
090201130218     C                   Z-ADD     TFTCL         M_WCPNCL
090202130218     C                   Z-ADD     TFNCR         m_WCPNCR
090300991102     C*
090400991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG.
090500001009     C                   Z-ADD     TFVLBP        WCPVLB           15 3
090600001009     C                   Z-ADD     TFVLCP        WCPVLC           15 3
090700991102     C                   Z-ADD     *ZEROS        WCPRVA
090800991102     C                   EXSR      CALRNG
090900991102     C                   Z-ADD     WCPRVA        WRNG
091000000519     C                   Z-ADD     WRNG          SPRNG             4 1
091100000517     C*
091200000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG. TOTALI
091300001009     C                   Z-ADD     TFVLBPT       WCPVLB           15 3
091400001009     C                   Z-ADD     TFVLCPT       WCPVLC           15 3
091500000517     C                   Z-ADD     *ZEROS        WCPRVA
091600000517     C                   EXSR      CALRNG
091700000517     C                   Z-ADD     WCPRVA        WRNG
091800000519     C                   Z-ADD     WRNG          SPTTP             4 1
091900991102     C*
092000991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL. X PROG.
092100001009     C                   Z-ADD     TFTCLP        WCPNCL           15 3
092200001009     C                   Z-ADD     TFNCRP        WCPNCR           15 3
092300991102     C                   Z-ADD     *ZEROS        WCPPVA
092400991102     C                   EXSR      CALPER
092500991102     C                   Z-ADD     WCPPVA        WPER
092600000517     C                   Z-ADD     WPER          SPPER             4 1
092601130218     C                   Z-ADD     TFTCLP        p_WCPNCL
092602130218     C                   Z-ADD     TFNCRP        p_WCPNCR
092700000000     C*
092800000000     C* SE DETTAGLIO
092900000000IF  2C     PARDET        IFEQ      'D'
093000000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
093100000000E   2C                   ENDIF
093200000000     C*
093300000000     C* STAMPO RIGA TOTALI
093400000000     C                   EXSR      STRTOT
093500000000     C*
093600000000     C* SE DETTAGLIO
093700000000IF  2C     PARDET        IFEQ      'D'
093800000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
093900000000E   2C                   ENDIF
094000040212E   1C                   ENDIF                                                  <-- 1 ---
094100000000     C*
094200000000     C                   ENDSR
094300000000     C*------------------------------------------------------------------------*
094400000000     C* STAARE - STAMPA TOTALI AREA
094500000000     C*------------------------------------------------------------------------*
094600000000     C     STAARE        BEGSR
094700000000     C*--------------------
094800000000     C* T E S T A T A
094900000000     C*--------------------
095000000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
095100000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
095200000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
095300000000     C*         RIGHE DI TOTALE
095400000000     C*
095500000000     C* SE NON CI SONO SPEDIZIONI NON STAMPO
095600941207IF  1C     TAVLB         IFEQ      *ZEROS
095700000000     C     TAVLC         ANDEQ     *ZEROS
095800000000X   1C                   ELSE
095900000000     C*
096000000000     C* STAMPA SINTETICA
096100000000IF  2C     PARDET        IFEQ      'S'
096200000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
096300000000X   2C                   ELSE
096400000000     C*
096500000000     C* STAMPO I TOTALI AREA A PAGINA NUOVA SOLO PER STAMPA IN DETTAGLIO
096600000000     C                   MOVEL     *BLANKS       DESFIL
096700000000     C                   SETOFF                                       01
096800000000     C                   EXCEPT    TESTA
096900000000E   2C                   ENDIF
097000000000     C*
097001130218     C                   MOVEL     *BLANKS       wDESFIL
097100000000     C                   SETON                                        OF        *SALTO PAG SUCCESSIV
097200000000     C*
097300000000     C* DESCRIZIONE TOTALE
097400000000     C                   MOVEL     TES(11)       STDES1
097500000000     C*
097600000000     C* DECODIFICA L'AREA
097700000000     C                   EXSR      DECARE
097800000000     C                   MOVEL     DESARE        STDES2
097900000000     C*
098000000000     C* IMPOSTO I VALORI DI STAMPA
098100941207     C                   Z-ADD     TAVLB         STVLB
098200000000     C                   Z-ADD     TAVLC         STVLC
098201130219     C                   Z-ADD     TAVLB         m_STVLB
098202130219     C                   Z-ADD     TAVLC         m_STVLC
098300991105     C                   Z-ADD     TATCL         STTCL
098400991105     C                   Z-ADD     TANCL         STNCL
098500000000     C                   Z-ADD     TAPKB         STPKB
098600000000     C                   Z-ADD     TANSP         STNSP
098700991102     C                   Z-ADD     TAVLBP        SPVLB
098800991102     C                   Z-ADD     TAVLCP        SPVLC
098801130218     C                   Z-ADD     TAVLBP        p_SPVLB
098802130218     C                   Z-ADD     TAVLCP        p_SPVLC
098900010222     C                   Z-ADD     TAVLBP        SPVLBW
099000010222     C                   Z-ADD     TAVLCP        SPVLCW
099100991105     C                   Z-ADD     TATCLP        SPTCL
099200991105     C                   Z-ADD     TANCLP        SPNCL
099300991102     C                   Z-ADD     TAPKBP        SPPKB
099400991102     C                   Z-ADD     TANSPP        SPNSP
099500991102     C*
099600991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
099700001009     C                   Z-ADD     TAVLB         WCPVLB           15 3
099800001009     C                   Z-ADD     TAVLC         WCPVLC           15 3
099900991102     C                   Z-ADD     *ZEROS        WCPRVA
100000991102     C                   EXSR      CALRNG
100100991102     C                   Z-ADD     WCPRVA        WRNG
100200000519     C                   Z-ADD     WRNG          STRNG             4 1
100300000517     C*
100400000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
100500001009     C                   Z-ADD     TAVLBT        WCPVLB           15 3
100600001009     C                   Z-ADD     TAVLCT        WCPVLC           15 3
100700000517     C                   Z-ADD     *ZEROS        WCPRVA
100800000517     C                   EXSR      CALRNG
100900000517     C                   Z-ADD     WCPRVA        WRNG
101000000519     C                   Z-ADD     WRNG          STTTP             4 1
101100991102     C*
101200991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
101300001009     C                   Z-ADD     TATCL         WCPNCL           15 3
101400001009     C                   Z-ADD     TANCR         WCPNCR           15 3
101500991102     C                   Z-ADD     *ZEROS        WCPPVA
101600991102     C                   EXSR      CALPER
101700991102     C                   Z-ADD     WCPPVA        WPER
101800000517     C                   Z-ADD     WPER          STPER             4 1
101801130218     C                   Z-ADD     TATCL         M_WCPNCL
101802130218     C                   Z-ADD     TANCR         m_WCPNCR
101900991102     C*
102000991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG.
102100001009     C                   Z-ADD     TAVLBP        WCPVLB           15 3
102200001009     C                   Z-ADD     TAVLCP        WCPVLC           15 3
102300991102     C                   Z-ADD     *ZEROS        WCPRVA
102400991102     C                   EXSR      CALRNG
102500991102     C                   Z-ADD     WCPRVA        WRNG
102600000519     C                   Z-ADD     WRNG          SPRNG             4 1
102700000517     C*
102800000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG. TOTALI
102900001009     C                   Z-ADD     TAVLBPT       WCPVLB           15 3
103000001009     C                   Z-ADD     TAVLCPT       WCPVLC           15 3
103100000517     C                   Z-ADD     *ZEROS        WCPRVA
103200000517     C                   EXSR      CALRNG
103300000517     C                   Z-ADD     WCPRVA        WRNG
103400000519     C                   Z-ADD     WRNG          SPTTP             4 1
103500991102     C*
103600991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL. X PROG.
103700001009     C                   Z-ADD     TATCLP        WCPNCL           15 3
103800001009     C                   Z-ADD     TANCRP        WCPNCR           15 3
103900991102     C                   Z-ADD     *ZEROS        WCPPVA
104000991102     C                   EXSR      CALPER
104100991102     C                   Z-ADD     WCPPVA        WPER
104200000517     C                   Z-ADD     WPER          SPPER             4 1
104201130218     C                   Z-ADD     TATCLP        p_WCPNCL
104202130218     C                   Z-ADD     TANCRP        p_WCPNCR
104300000000     C*
104400000000     C* STAMPO RIGA TOTALE
104500000000     C                   EXSR      STRTOT
104600000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
104700000000     C*
104800000000E   1C                   ENDIF
104900000000     C*
105000000000     C                   ENDSR
105100000000     C*------------------------------------------------------------------------*
105200000000     C* STADIV - STAMPA TOTALI DIVISIONE
105300000000     C*------------------------------------------------------------------------*
105400000000     C     STADIV        BEGSR
105500000000     C*--------------------
105600000000     C* T E S T A T A
105700000000     C*--------------------
105800000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
105900000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
106000000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
106100000000     C*         RIGHE DI TOTALE
106200000000     C*
106300000000     C* SE NON CI SONO SPEDIZIONI NON STAMPO
106400941207     C     TDVLB         IFEQ      *ZEROS                                       --- 1 -->
106500000000     C     TDVLC         ANDEQ     *ZEROS
106600000000     C                   ELSE                                                   <-- 1 -->
106700000000     C*
106800000000     C* STAMPA TOTALE DIVISIONE A PAGINA NUOVA
106900000000     C                   MOVEL     *BLANKS       DESFIL
106901130218     C                   MOVEL     *BLANKS       wDESFIL
107000000000     C                   MOVEL     *BLANKS       DESARE
107100000000     C                   SETOFF                                       01
107200000000     C                   EXCEPT    TESTA
107300000000     C                   SETON                                        OF        *SALTO PAG SUCCESSIV
107400000000     C*
107500000000     C* DESCRIZIONE TOTALE
107600000000     C                   MOVEL     TES(12)       STDES1
107700000000     C*
107800000000     C* DECODIFICA LA DIVISIONE
107900000000     C                   EXSR      DECDIV
108000000000     C                   MOVEL     DESDIV        STDES2
108100000000     C*
108200000000     C* IMPOSTO I VALORI DI STAMPA
108300941207     C                   Z-ADD     TDVLB         STVLB
108400000000     C                   Z-ADD     TDVLC         STVLC
108401130218     C                   Z-ADD     TDVLB         m_STVLB
108402130218     C                   Z-ADD     TDVLC         m_STVLC
108500991105     C                   Z-ADD     TDTCL         STTCL
108600991105     C                   Z-ADD     TDNCL         STNCL
108700000000     C                   Z-ADD     TDPKB         STPKB
108800000000     C                   Z-ADD     TDNSP         STNSP
108900991102     C                   Z-ADD     TDVLBP        SPVLB
109000991102     C                   Z-ADD     TDVLCP        SPVLC
109001130218     C                   Z-ADD     TDVLBP        p_SPVLB
109002130218     C                   Z-ADD     TDVLCP        p_SPVLC
109100010222     C                   Z-ADD     TDVLBP        SPVLBW
109200010222     C                   Z-ADD     TDVLCP        SPVLCW
109300991105     C                   Z-ADD     TDTCLP        SPTCL
109400991105     C                   Z-ADD     TDNCLP        SPNCL
109500991102     C                   Z-ADD     TDPKBP        SPPKB
109600991102     C                   Z-ADD     TDNSPP        SPNSP
109700991102     C*
109800991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
109900001009     C                   Z-ADD     TDVLB         WCPVLB           15 3
110000001009     C                   Z-ADD     TDVLC         WCPVLC           15 3
110100991102     C                   Z-ADD     *ZEROS        WCPRVA
110200991102     C                   EXSR      CALRNG
110300991102     C                   Z-ADD     WCPRVA        WRNG
110400000519     C                   Z-ADD     WRNG          STRNG             4 1
110500000517     C*
110600000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
110700001009     C                   Z-ADD     TDVLBT        WCPVLB           15 3
110800001009     C                   Z-ADD     TDVLCT        WCPVLC           15 3
110900000517     C                   Z-ADD     *ZEROS        WCPRVA
111000000517     C                   EXSR      CALRNG
111100000517     C                   Z-ADD     WCPRVA        WRNG
111200000519     C                   Z-ADD     WRNG          STTTP             4 1
111300991102     C*
111400991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
111500001009     C                   Z-ADD     TDTCL         WCPNCL           15 3
111600001009     C                   Z-ADD     TDNCR         WCPNCR           15 3
111700991102     C                   Z-ADD     *ZEROS        WCPPVA
111800991102     C                   EXSR      CALPER
111900991102     C                   Z-ADD     WCPPVA        WPER
112000000517     C                   Z-ADD     WPER          STPER             4 1
112001130218     C                   Z-ADD     TDTCL         M_WCPNCL
112002130218     C                   Z-ADD     TDNCR         m_WCPNCR
112100991102     C*
112200991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG.
112300001009     C                   Z-ADD     TDVLBP        WCPVLB           15 3
112400001009     C                   Z-ADD     TDVLCP        WCPVLC           15 3
112500991102     C                   Z-ADD     *ZEROS        WCPRVA
112600991102     C                   EXSR      CALRNG
112700991102     C                   Z-ADD     WCPRVA        WRNG
112800000519     C                   Z-ADD     WRNG          SPRNG             4 1
112900000517     C*
113000000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG. TOTALI
113100001009     C                   Z-ADD     TDVLBPT       WCPVLB           15 3
113200001009     C                   Z-ADD     TDVLCPT       WCPVLC           15 3
113300000517     C                   Z-ADD     *ZEROS        WCPRVA
113400000517     C                   EXSR      CALRNG
113500000517     C                   Z-ADD     WCPRVA        WRNG
113600000519     C                   Z-ADD     WRNG          SPTTP             4 1
113700991102     C*
113800991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL. X PROG.
113900001009     C                   Z-ADD     TDTCLP        WCPNCL           15 3
114000001009     C                   Z-ADD     TDNCRP        WCPNCR           15 3
114100991102     C                   Z-ADD     *ZEROS        WCPPVA
114200991102     C                   EXSR      CALPER
114300991102     C                   Z-ADD     WCPPVA        WPER
114400000517     C                   Z-ADD     WPER          SPPER             4 1
114401130218     C                   Z-ADD     TDTCLP        p_WCPNCL
114402130218     C                   Z-ADD     TDNCRP        p_WCPNCR
114500000000     C*
114600000000     C* STAMPO RIGA TOTALE
114700000000     C                   EXSR      STRTOT
114800000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
114900000000     C*
115000000000     C                   END                                                    <-- 1 ---
115100000000     C*
115200000000     C                   ENDSR
115300000000     C*------------------------------------------------------------------------*
115400000000     C* STAGEN - STAMPA TOTALI GENERALI
115500000000     C*------------------------------------------------------------------------*
115600000000     C     STAGEN        BEGSR
115700000000     C*--------------------
115800000000     C* T E S T A T A
115900000000     C*--------------------
116000000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
116100000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
116200000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
116300000000     C*         RIGHE DI TOTALE
116400000000     C*
116500000000     C* STAMPA TOTALE GENERALE A PAGINA NUOVA
116600000000     C                   MOVEL     *BLANKS       DESFIL
116601130218     C                   MOVEL     *BLANKS       wDESFIL
116700000000     C                   MOVEL     *BLANKS       DESARE
116800000000     C                   MOVEL     *BLANKS       DESDIV
116900000000     C                   SETOFF                                       01
117000000000     C                   EXCEPT    TESTA
117100000000     C*
117200000000     C* DESCRIZIONE TOTALE
117300000000     C                   MOVEL     TES(13)       STDES1
117400000000     C                   MOVEL     *BLANKS       STDES2
117500000000     C*
117600000000     C* IMPOSTO I VALORI DI STAMPA
117700941207     C                   Z-ADD     TGVLB         STVLB
117800000000     C                   Z-ADD     TGVLC         STVLC
117801130218     C                   Z-ADD     TGVLB         m_STVLB
117802130218     C                   Z-ADD     TGVLC         m_STVLC
117900991105     C                   Z-ADD     TGTCL         STTCL
118000991105     C                   Z-ADD     TGNCL         STNCL
118100000000     C                   Z-ADD     TGPKB         STPKB
118200000000     C                   Z-ADD     TGNSP         STNSP
118300991102     C                   Z-ADD     TGVLBP        SPVLB
118400991102     C                   Z-ADD     TGVLCP        SPVLC
118401130218     C                   Z-ADD     TGVLBP        p_SPVLB
118402130218     C                   Z-ADD     TGVLCP        p_SPVLC
118500010222     C                   Z-ADD     TGVLBP        SPVLBW
118600010222     C                   Z-ADD     TGVLCP        SPVLCW
118700991105     C                   Z-ADD     TGTCLP        SPTCL
118800991105     C                   Z-ADD     TGNCLP        SPNCL
118900991102     C                   Z-ADD     TGPKBP        SPPKB
119000991102     C                   Z-ADD     TGNSPP        SPNSP
119100991102     C*
119200991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL.
119300001009     C                   Z-ADD     TGVLB         WCPVLB           15 3
119400001009     C                   Z-ADD     TGVLC         WCPVLC           15 3
119500991102     C                   Z-ADD     *ZEROS        WCPRVA
119600991102     C                   EXSR      CALRNG
119700991102     C                   Z-ADD     WCPRVA        WRNG
119800000519     C                   Z-ADD     WRNG          STRNG             4 1
119900000517     C*
120000000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. TOTALI
120100001009     C                   Z-ADD     TGVLBT        WCPVLB           15 3
120200001009     C                   Z-ADD     TGVLCT        WCPVLC           15 3
120300000517     C                   Z-ADD     *ZEROS        WCPRVA
120400000517     C                   EXSR      CALRNG
120500000517     C                   Z-ADD     WCPRVA        WRNG
120600000519     C                   Z-ADD     WRNG          STTTP             4 1
120700991102     C*
120800991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL.
120900001009     C                   Z-ADD     TGTCL         WCPNCL           15 3
121000001009     C                   Z-ADD     TGNCR         WCPNCR           15 3
121100991102     C                   Z-ADD     *ZEROS        WCPPVA
121200991102     C                   EXSR      CALPER
121300991102     C                   Z-ADD     WCPPVA        WPER
121400000517     C                   Z-ADD     WPER          STPER             4 1
121401130218     C                   Z-ADD     TGTCL         M_WCPNCL
121402130218     C                   Z-ADD     TGNCR         m_WCPNCR
121500991102     C*
121600991102     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG.
121700001009     C                   Z-ADD     TGVLBP        WCPVLB           15 3
121800001009     C                   Z-ADD     TGVLCP        WCPVLC           15 3
121900991102     C                   Z-ADD     *ZEROS        WCPRVA
122000991102     C                   EXSR      CALRNG
122100991102     C                   Z-ADD     WCPRVA        WRNG
122200000519     C                   Z-ADD     WRNG          SPRNG             4 1
122300000517     C*
122400000517     C* CALCOLA PERCENTUALE VOLUME CML SU VOLUME BOLL. X PROG. TOTALI
122500001009     C                   Z-ADD     TGVLBPT       WCPVLB           15 3
122600001009     C                   Z-ADD     TGVLCPT       WCPVLC           15 3
122700000517     C                   Z-ADD     *ZEROS        WCPRVA
122800000517     C                   EXSR      CALRNG
122900000517     C                   Z-ADD     WCPRVA        WRNG
123000000519     C                   Z-ADD     WRNG          SPTTP             4 1
123100991102     C*
123200991102     C* CALCOLA PERCENTUALE RAPPORTO COLLI CML SU COLI BOLL. X PROG.
123300001009     C                   Z-ADD     TGTCLP        WCPNCL           15 3
123400001009     C                   Z-ADD     TGNCRP        WCPNCR           15 3
123500991102     C                   Z-ADD     *ZEROS        WCPPVA
123600991102     C                   EXSR      CALPER
123700991102     C                   Z-ADD     WCPPVA        WPER
123800000517     C                   Z-ADD     WPER          SPPER             4 1
123801130218     C                   Z-ADD     TGTCLP        p_WCPNCL
123802130218     C                   Z-ADD     TGNCRP        p_WCPNCR
123900000000     C*
124000000000     C* STAMPO RIGA TOTALE
124100000000     C                   EXSR      STRTOT
124200000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
124300000000     C*
124400000000     C                   ENDSR
124500000000     C*------------------------------------------------------------------------*
124600000000     C* STRCLI - STAMPA RIGA CLIENTE/TARIFFA
124700000000     C*------------------------------------------------------------------------*
124800000000     C     STRCLI        BEGSR
124900000000     C*--------------------
125000000000     C* T E S T A T A
125100000000     C*--------------------
125200000000     C* 01 ON - STAMPA IN INTESTAZIONE LA DICITURA "CLIENTE", "TARIFFA", "RAPP."
125300000000     C*         "PES/VOL" PER LA STAMPA IN DETTAGLIO
125400000000     C* 01 OFF- STAMPA L' INTESTAZIONE A BLANKS PER LA STAMPA SINTETICA O PER LE
125500000000     C*         RIGHE DI TOTALE
125600000000     C*
125700950629     C* RECUPERA CALCOLO ATTUALE DEL VOLUME E CALCOLA IL PREVISTO
125800000000     C                   EXSR      RECCAL
125900991105 MOD C     PARDET        IFEQ      'D'
126000000000     C     *INOF         IFEQ      '1'
126100000000     C                   SETON                                        01
126200000000     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
126300000000     C                   EXCEPT    TESTA
126400000000     C                   SETOFF                                       OF
126500000718     C                   ENDIF
126600950629     C*
126700040219     C* SE HA GIA' STAMPATO UNA RIGA HA GIA' STAMPATO
126800950629     C* ANCHE I DATI DEL CLIENTE, PER CUI PER LA SECONDA RIGA LI PULISCE
126900950629IF  1C     $STDAC        IFEQ      '1'
127000950629     C                   Z-ADD     *ZEROS        DEPKSC
127100030909     C                   MOVEL     *BLANKS       DESCLI           29
127200950629     C                   Z-ADD     *ZEROS        DEPCTR
127300950629     C                   MOVEL     *BLANKS       DESCTR
127400950629     C                   MOVEL     *BLANKS       DESDCV
127500950629     C                   Z-ADD     *ZEROS        STCAK
127600950629     C                   Z-ADD     *ZEROS        STCAC
127700950629     C                   Z-ADD     *ZEROS        STCIK
127800950629     C                   Z-ADD     *ZEROS        STCIC
127900950629E   1C                   ENDIF
128000000000     C*
128100000000     C* STAMPO RIGA CLIENTE/TARIFFA
128200000000     C                   EXCEPT    RIGCLI
128300991105 MOD C                   ENDIF
128400991102     C                   Z-ADD     1             FLGCLI
128401130207     c* La scrittura del file WFVPR00F è pilotata dallo stesso flag
128402130207     c* per la scrittura di wfvlm
128403130207     c                   if        parsnr='S'
128404130208     c                   exsr      riewfvpr
128405130207     c                   endif
128500000000     C*
128600000000     C                   ENDSR
128700991029     C*------------------------------------------------------------------------*
128800991029     C* STRPRO - STAMPA RIGA PROGRESSIVO CLIENTE/TARIFFA AL MESE
128900991029     C*------------------------------------------------------------------------*
129000991029     C     STRPRO        BEGSR
129100991102     C*
129200991102     C                   EXSR      RECPRO
129300991105     C*
129400991105     C* * * STAMPO EFFETTIVAMENTE SOLO SE RICHIESTO DETTAGLIO * * *
129500991105     C*
129600991105     C     PARDET        IFEQ      'D'
129700991029     C     *INOF         IFEQ      '1'
129800991029     C                   SETON                                        01
129900991029     C                   EXCEPT    RIGSEP                                       *RIGA DI SEPARAZIONE
130000991029     C                   EXCEPT    TESTA
130100991029     C                   SETOFF                                       OF
130200991029     C                   END
130300991029     C*
130400991029     C     $STDAC        IFEQ      '1'
130500991029     C                   Z-ADD     *ZEROS        DEPKSC
130600991029     C                   MOVEL     *BLANKS       DESCLI
130700991029     C                   Z-ADD     *ZEROS        DEPCTR
130800991029     C                   Z-ADD     *ZEROS        DEPMES
130900991029     C                   MOVEL     *BLANKS       DESCTR
131000991029     C                   ENDIF
131100991029     C*
131200991029     C* STAMPO RIGA CLIENTE/TARIFFA
131300991029     C                   EXCEPT    RIGPRO
131400991105     C                   ENDIF
131500991029     C*
131600991029     C                   ENDSR
131700000000     C*------------------------------------------------------------------------*
131800000000     C* STRTOT - STAMPA RIGA TOTALE
131900000000     C*------------------------------------------------------------------------*
132000000000     C     STRTOT        BEGSR
132100000000     C*
132200000000     C* STAMPO RIGA CLIENTE/TARIFFA
132300000000     C                   EXCEPT    RIGTOT
132400991102     C                   EXCEPT    RIGTOP
132401130218     c                   if        parsnr='S'
132402130218     c                   exsr      riewfvprt
132403130218     c                   write     wfvprt
132404130218     c                   endif
132500000000     C*
132600000000     C                   ENDSR
132700000000     C*------------------------------------------------------------------------*
132800000000     C* DECDIV - DECODIFICA DIVISIONE
132900000000     C*------------------------------------------------------------------------*
133000000000     C     DECDIV        BEGSR
133100000000     C*
133200020919     C                   MOVEL     1             KTBKUT
133300000000     C                   MOVE      '17'          KTBCOD
133400000000     C                   MOVE      *BLANKS       KTBKEY
133500000000     C                   MOVEL     DEPDIV        KTBKEY
133600000000     C     KEYTAB        CHAIN     TABEL                              99
133700000000     C     *IN99         IFEQ      '1'
133800000000     C     TBLFLG        OREQ      '*'
133900000000     C                   MOVEL     *ALL'*'       DESDIV           20
134000000000     C                   ELSE
134100000000     C                   MOVEL     TBLUNI        DESDIV
134200000000     C                   END
134300000000     C*
134400000000     C                   ENDSR
134500000000     C*------------------------------------------------------------------------*
134600000000     C* DECARE - DECODIFICA AREA
134700000000     C*------------------------------------------------------------------------*
134800000000     C     DECARE        BEGSR
134900000000     C*
135000020919     C                   MOVEL     1             KTBKUT
135100000000     C                   MOVE      '05'          KTBCOD
135200000000     C                   MOVE      *BLANKS       KTBKEY
135300000000     C                   MOVEL     DEPARE        KTBKEY
135400000000     C     KEYTAB        CHAIN     TABEL                              99
135500000000     C     *IN99         IFEQ      '1'
135600000000     C     TBLFLG        OREQ      '*'
135700000000     C                   MOVEL     *ALL'*'       DESARE           20
135800000000     C                   ELSE
135900000000     C                   MOVEL     TBLUNI        DESARE
136000000000     C                   END
136100000000     C*
136200000000     C                   ENDSR
136300000000     C*------------------------------------------------------------------------*
136400000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
136500000000     C*------------------------------------------------------------------------*
136600000000     C     DECFIL        BEGSR
136700000000     C*
136800000000     C                   Z-ADD     1             I
136900000000     C     DEPFIL        LOOKUP    FIL(I)                                 99
137000000000     C     *IN99         IFEQ      '0'
137100000000     C                   MOVEL     *ALL'*'       DESFIL           20
137200000000     C                   ELSE
137300000000     C                   MOVEL     DFIL(I)       DESFIL
137400000000     C                   END
137401130207     c                   eval      wdesfil=desfil
137500000000     C*
137600000000     C                   ENDSR
137700000000     C*------------------------------------------------------------------------*
137800000000     C* DECCTR - DECODIFICA TARIFFA
137900000000     C*------------------------------------------------------------------------*
138000000000     C     DECCTR        BEGSR
138100980421     C*
138200980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
138300980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI
138400980421IF  1C     DEPCTR        IFEQ      999
138500991104     C                   MOVEL     DEC(1)        DESCTR           10
138600980421X   1C                   ELSE
138700980421IF  2C     DEPCTR        IFEQ      888
138800980421     C                   MOVEL     DEC(2)        DESCTR
138900980421X   2C                   ELSE
139000020919     C                   MOVEL     1             KTBKUT
139100000000     C                   MOVE      'TR'          KTBCOD
139200000000     C                   MOVE      *BLANKS       KTBKEY
139300000000     C                   MOVEL     DEPCTR        WTEM01            1
139400000000     C                   MOVEL     WTEM01        KTBKEY
139500000000     C     KEYTAB        CHAIN     TABEL                              99
139600980421IF  3C     *IN99         IFEQ      '1'
139700000000     C     TBLFLG        OREQ      '*'
139800980421     C                   MOVEL     *ALL'*'       DESCTR
139900980421X   3C                   ELSE
140000000000     C                   MOVEL     TBLUNI        DESCTR
140100980421E   3C                   ENDIF
140200980421E   2C                   ENDIF
140300980421E   1C                   ENDIF
140400000000     C*
140500000000     C                   ENDSR
140600991102     C*------------------------------------------------------------------------*
140700991102     C* CALPER - CALCOLA PERCENTUALE RAPPORTO COLLI
140800991102     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
140900991102     C*          - COLLI CML               (WCPNCR)
141000991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
141100991102     C*------------------------------------------------------------------------*
141200991102     C     CALPER        BEGSR
141300991102     C*
141400991102     C* ===> CALCOLA PERCENTUALE COLLI CML SU COLLI BOLL.
141500991102     C                   Z-ADD     *ZEROS        WCPPVA            6 2
141600991102     C     WCPNCL        IFNE      WCPNCR                                       --- 1 -->
141700991102     C     WCPNCL        ANDGT     *ZEROS
141800991102     C     WCPNCR        ANDGT     *ZEROS
141900001009     C     WCPNCR        MULT      100           WMULTI           18 3
142000001009     C     WMULTI        DIV       WCPNCL        WRKPER           20 3
142100991102     C     WRKPER        IFGT      9999                                         --- 2 -->
142200991102     C                   Z-ADD     9999,99       WCPPVA
142300991102     C                   ELSE
142400991102     C                   Z-ADD     WRKPER        WCPPVA
142500991102     C                   END                                                    <-- 2 ---
142600991102     C                   END                                                    <-- 1 ---
142700991102     C*
142800991102     C     WCPNCL        IFEQ      WCPNCR                                       --- 1 -->
142900991102     C     WCPNCL        ANDGT     *ZEROS
143000991102     C     WCPNCR        ANDGT     *ZEROS
143100991102     C                   Z-ADD     100           WCPPVA
143200991102     C                   ENDIF
143300991102     C*
143400000000     C                   ENDSR
143500991029     C*------------------------------------------------------------------------*
143600991102     C* CALRNG - CALCOLA PERCENTUALE SCOSTAMENTO VOLUMI
143700991102     C* INPUT:   - VOLUME BOLLETTAT0         (WCPVLB)
143800991102     C*          - VOLUME VDL                (WCPVLC)
143900991102     C* OUTPUT:  - PERCENTUALE DIFFERENZA    (WCPRVA)
144000991029     C*------------------------------------------------------------------------*
144100991102     C     CALRNG        BEGSR
144200991029     C*
144300991229     C* ===> CALCOLA PERCENTUALE VOL CML SU VOL BOLL.
144400991102     C                   Z-ADD     *ZEROS        WCPRVA            6 2
144500991102     C     WCPVLC        IFNE      WCPVLB                                       --- 1 -->
144600991102     C     WCPVLC        ANDGT     *ZEROS
144700991102     C     WCPVLB        ANDGT     *ZEROS
144800001009     C     WCPVLB        SUB       WCPVLC        WDIFFE           18 3
144900001009     C     WDIFFE        MULT      100           WRKPER           20 3
145000991102     C     WRKPER        DIV       WCPVLB        WRKPER
145100991102     C     WRKPER        IFGT      9999                                         --- 2 -->
145200991102     C                   Z-ADD     9999,99       WCPRVA
145300991102     C                   ELSE
145400991102     C                   Z-ADD     WRKPER        WCPRVA
145500991102     C                   END                                                    <-- 2 ---
145600991102     C                   END                                                    <-- 1 ---
145700991104 OLD C*
145800991104  "  C* SPECIFICA LOGICAMENTE ERRATA MA INSERITA X MANTENERE UNIFORMI
145900991104  "  C* I CRITERI DI VALUTAZIONE UTENTE RISPETTO ALLA VECCHIA
146000991104  "  C* STATISTICA VOLUMI
146100991104  "  C*                    MULT -1        WCPRVA
146200991104     C*
146300991102     C     WCPVLC        IFEQ      WCPVLB                                       --- 1 -->
146400991102     C     WCPVLC        ANDGT     *ZEROS
146500991102     C     WCPVLB        ANDGT     *ZEROS
146600991102     C                   Z-ADD     100           WCPRVA
146700991102     C                   ENDIF
146800991102     C*
146900991102     C                   ENDSR
147000000000     C*------------------------------------------------------------------------*
147100000000     C* RECCAL - RECUPERA CALCOLO ATTUALE VOLUME E CALCOLA IL CONSIGLIATO
147200000000     C*------------------------------------------------------------------------*
147300000000     C     RECCAL        BEGSR
147400000000     C*
147500980421     C* AZZERA I DATI IN STAMPA
147600000000     C                   Z-ADD     *ZEROS        STCAK             5 1
147700000000     C                   Z-ADD     *ZEROS        STCAC             6 3
147800000000     C                   Z-ADD     *ZEROS        STCIK             5 1
147900000000     C                   Z-ADD     *ZEROS        STCIC             6 3
148000991104     C*                    Z-ADD*ZERO     STKPM   51
148100030909     C                   MOVEL     *BLANKS       DESDCV           12
148200030909     C                   MOVEL     *BLANKS       DESTTR            3
148300980421     C*
148400980421     C* SE LA TARIFFA E'=999 SIGNIFICA CHE E' LA TARIFFA IMPOSTATA DAL PGM PER
148500980421     C* GLI ASSEGNATI, SE =888 E' QUELLA IMPOSTATA PER I FRANCHI NON CODIFICATI,
148600980421     C* IN QUESTI CASI SALTA LA LETTURA DELLE TARIFFE PERCHE' INESISTENTI
148700980421     C     DEPCTR        IFNE      999                                          --- 0 -->
148800980421     C     DEPCTR        ANDNE     888
148900000000     C*
149000980402     C* ======> CERCA IL VALORE ATTUALE DI CALCOLO E ALTRI DATI
149100000000     C                   Z-ADD     DEPKSC        KTFKSC
149200000000     C                   Z-ADD     DEPCTR        KTFCTR
149300000000     C                   Z-ADD     *ZEROS        KTFPRG
149400010314     C     KEYTAM        SETLL     TNTAM01L                           99        *FINE-ARCHIV
149500010314     C  N99              READ      TNTAM01L                               99
149600000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
149700000000     C     TAMKSC        ANDEQ     KTFKSC
149800000000     C     TAMCTR        ANDEQ     KTFCTR
149900000000     C     TAMATB        IFEQ      ' '                                          --- 2 -->
150000000000     C     TAMTIN        IFEQ      'K'                                          *TAMTIN='K'
150100000000     C                   Z-ADD     TAMKPM        STCAK
150200000000     C                   Z-ADD     *ZEROS        STCAC
150300000000     C                   ELSE                                                   *TAMTIN='C'
150400000000     C     TAMKPM        DIV       100           STCAC
150500000000     C                   Z-ADD     *ZEROS        STCAK
150600000000     C                   END
150700980421     C                   MOVEL     TAMDCV        DESDCV
150800030909     C* REPERISCO IL NETWORK DELLA TARIFFA
150900030909     C                   MOVEL(P)  TAMFLO        DSTA01
151000030909     C                   IF        TAMFIE = 'I' AND §TADPD <> 'S'
151100030909     C                   EVAL      DESTTR = 'ITA'
151200030909     C                   ENDIF
151300030909     C                   IF        TAMFIE = 'I' AND §TADPD  = 'S'
151400030909     C                   EVAL      DESTTR = 'DPD'
151500030909     C                   ENDIF
151600030909     C                   IF        TAMFIE = 'E' AND §TAFED <> 'S'
151700030909     C                   EVAL      DESTTR = 'EEX'
151800030909     C                   ENDIF
151900030909     C                   IF        TAMFIE = 'E' AND §TAFED  = 'S'
152000030909     C                   EVAL      DESTTR = 'FED'
152100030909     C                   ENDIF
152200030909     C*
152300000000     C                   Z-ADD     TAMPRG        WPRG              3 0
152400000000     C                   END                                                    <-- 2 ---
152500010314     C                   READ      TNTAM01L                               99
152600000000     C                   END                                                    <-- 1 ---
152700000000     C*
152800000000     C* ====> IN BASE ALLA PERCENTUALE TROVATA INDICA IL VALORE DI CALCOLO OK
152900000000     C* KG/MC
153000000000     C*                             VALORE ATTUALE * VOLUME AUTOMATICO
153100000000     C*          VALORE PROPOSTO = ------------------------------------
153200000000     C*                                       VOLUME CML
153300000000     C*
153400000000     C     STCAK         IFNE      *ZEROS
153500001009     C     STVLC         ANDNE     *ZEROS
153600000000     C     STPKB         DIV(H)    STVLC         STCIK
153700000000     C                   END
153800000000     C* MC/COLLI
153900000000     C*                             VALORE ATTUALE * VOLUME CML
154000000000     C*          VALORE PROPOSTO = -----------------------------
154100000000     C*                                  VOLUME AUTOMATICO
154200000000     C*
154300000000     C     STCAC         IFNE      *ZEROS
154400001009     C     STNCL         ANDNE     *ZEROS
154500000000     C     STVLC         DIV(H)    STNCL         STCIC
154600000000     C                   END
154700980421     C                   ENDIF                                                  <-- 0 ---
154800000000     C*
154900000000     C                   ENDSR
155000991102     C*------------------------------------------------------------------------*
155100991102     C* RECPRO - RECUPERA CALCOLO ATTUALE VOLUME E CALCOLA IL CONSIGLIATO X PROG.
155200991102     C*------------------------------------------------------------------------*
155300991102     C     RECPRO        BEGSR
155400991102     C*
155500991102     C* AZZERA I DATI IN STAMPA
155600991102     C                   Z-ADD     *ZEROS        SPCIK             5 1
155700991102     C                   Z-ADD     *ZEROS        SPCIC             6 3
155800991102     C*
155900991102     C* ====> IN BASE ALLA PERCENTUALE TROVATA INDICA IL VALORE DI CALCOLO OK
156000991102     C* KG/MC
156100991102     C*                             VALORE ATTUALE * VOLUME AUTOMATICO
156200991102     C*          VALORE PROPOSTO = ------------------------------------
156300991102     C*                                       VOLUME CML
156400991102     C*
156500991102     C     STCAK         IFNE      *ZEROS
156600001009     C     STVLC         ANDNE     *ZEROS
156700991102     C     SPPKB         DIV(H)    SPVLC         SPCIK
156800991102     C                   END
156900991102     C* MC/COLLI
157000991102     C*                             VALORE ATTUALE * VOLUME CML
157100991102     C*          VALORE PROPOSTO = -----------------------------
157200991102     C*                                  VOLUME AUTOMATICO
157300991102     C*
157400991102     C     STCAC         IFNE      *ZEROS
157500001009     C     STNCL         ANDNE     *ZEROS
157600991102     C     SPVLC         DIV(H)    SPNCL         SPCIC
157700991102     C                   END
157800991102     C*
157900991102     C                   ENDSR
158000020919     C*--------------------------------------------------------------------------------------------*
158100020919     C* REPERISCE I DATI UTENTE
158200020919     C*--------------------------------------------------------------------------------------------*
158300020919     C     REPDATIUTE    BEGSR
158400020919     C*
158500020919     C* INIZIALIZZA VARIABILI DI WRK
158600020919     C                   CLEAR                   TIBS34DS
158700020919     C                   CLEAR                   AZUTEDS
158800020919     C                   CLEAR                   DDATIUTE
158900020919     C*
159000020919     C     *DTAARA       DEFINE    §azute        azuteds
159100020919     C     *DTAARA       DEFINE    §datiute      ddatiute
159200020919     C                   IN(E)     *DTAARA
159300020919if  1C                   IF        %Error
159400020919     c                   EVAL      I34Tla = 'L'
159500020919     C                   CALL      'TIBS34R'
159600020919     C                   PARM                    Tibs34Ds
159700020919     C                   IN        *DTAARA
159800020919e   1C                   ENDIF
159900020919     C*
160000020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
160100020919     C                   MOVEL(P)  RSUT          DSFIRS           20
160200020919     C*
160300020919     C                   ENDSR
160400000000     C*------------------------------------------------------------------------*
160500000000     C* *INZSR - ROUTINE INIZIALE
160600000000     C*------------------------------------------------------------------------*
160700000000     C     *INZSR        BEGSR
160800000000     C*--------------------
160900000000     C* RICEVIMENTO PARAMENTRI
161000000000     C*--------------------
161100000000     C     *ENTRY        PLIST
161200000000     C                   PARM                    KPJBA
161300950703     C                   MOVEL     KPJBU         PARAM
161400000000     C*--------------------
161500000000     C* CAMPI RIFERITI AL DATA BASE
161600000000     C*--------------------
161700980402     C     *LIKE         DEFINE    TBLKUT        KTBKUT
161800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
161900000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
162000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
162100000000     C     *LIKE         DEFINE    TAMKSC        KTFKSC
162200000000     C     *LIKE         DEFINE    TAMCTR        KTFCTR
162300000000     C     *LIKE         DEFINE    TAMPRG        KTFPRG
162400000000     C     *LIKE         DEFINE    WFVANN        KWVANN
162500000000     C     *LIKE         DEFINE    WFVDIV        KWVDIV
162600000000     C     *LIKE         DEFINE    WFVARE        KWVARE
162700000000     C     *LIKE         DEFINE    WFVFIL        KWVFIL
162800991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
162900000000     C     *LIKE         DEFINE    WFVKSC        KWVKSC
163000000000     C     *LIKE         DEFINE    WFVCTR        KWVCTR
163100000000     C     *LIKE         DEFINE    WFVMES        KWVMES
163200950629     C     *LIKE         DEFINE    WFVFVB        KWVFVB
163300000000     C     *LIKE         DEFINE    WFVDIV        DEPDIV
163400000000     C     *LIKE         DEFINE    WFVARE        DEPARE
163500000000     C     *LIKE         DEFINE    WFVFIL        DEPFIL
163600020910     C     *LIKE         DEFINE    WFVFIL        DEPTFP
163700000000     C     *LIKE         DEFINE    WFVKSC        DEPKSC
163800000000     C     *LIKE         DEFINE    WFVCTR        DEPCTR
163900991029     C     *LIKE         DEFINE    WFVMES        DEPMES
164000000000     C     *LIKE         DEFINE    TAMKSC        WKSC
164100000000     C     *LIKE         DEFINE    TAMCTR        WCTR
164200000000     C*--------------------
164300000000     C* CHIAVI DI LETTURA
164400000000     C*--------------------
164500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
164600000000     C     KEYTAB        KLIST
164700980402     C                   KFLD                    KTBKUT
164800000000     C                   KFLD                    KTBCOD
164900000000     C                   KFLD                    KTBKEY
165000000000     C*
165100000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
165200000000     C     KEYORG        KLIST
165300000000     C                   KFLD                    KORFIL
165400000000     C*
165500991104     C* CHIAVE LETTURA WFVLM01L - COMPLETA
165600000000     C     KEYWFV        KLIST
165700000000     C                   KFLD                    KWVANN
165800000000     C                   KFLD                    KWVDIV
165900000000     C                   KFLD                    KWVARE
166000000000     C                   KFLD                    KWVFIL
166100991029     C                   KFLD                    KWVRAG
166200000000     C                   KFLD                    KWVKSC
166300000000     C                   KFLD                    KWVCTR
166400000000     C                   KFLD                    KWVMES
166500950629     C                   KFLD                    KWVFVB
166600000000     C*
166700980407     C* CHIAVE LETTURA TNTAM31C - COMPLETA
166800000000     C     KEYTAM        KLIST
166900000000     C                   KFLD                    KTFKSC
167000000000     C                   KFLD                    KTFCTR
167100000000     C                   KFLD                    KTFPRG
167200000000     C*
167300980407     C* CHIAVE LETTURA TNTAM31C - PARZIALE
167400000000     C     KE1TAM        KLIST
167500000000     C                   KFLD                    KTFKSC
167600000000     C                   KFLD                    KTFCTR
167700020910     C*--------------------
167800020910     C* REPERISCE DATA/ORA CORRENTE
167900020910     C*--------------------
168000020910     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
168100020910     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
168200020910     C                   Z-ADD     *ZEROS        G08INV
168300020910     C                   MOVEL     '0'           G08ERR
168400020910     C                   CALL      'XSRDA8'
168500020910     C                   PARM                    WLBDA8
168600020910     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
168700000000     C*--------------------
168800000000     C* CAMPI DI CONTROLLO
168900000000     C*--------------------
169000000000     C                   MOVEL     '0'           FINL00            1            *FINE FILE
169100000000     C                   SETON                                        OF        *OVERFLOW
169200000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
169300000000     C                   TIME                    ORA               6 0          *ORA
169400040211     C*---               MOVEL     *BLANKS       $REA              1            *VOLUMI REALI O NON
169500950629     C                   MOVEL     '0'           $STDAC            1            *STAMPATI DATI CLI.
169600991229     C*
169700991229     C* IMPOSTO SEMPRE E COMUNQUE A POSITIVO IL "PARMIN"
169800991229     C* PER FACILITARE I CONTROLLI
169900991229     C                   EVAL      PARMIN=%abs(PARMIN)
170000950629     C*
170100000000     C* LANCIO ROUTINE PER VERIFICARE SE
170200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
170300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
170400020919     C                   EXSR      REPDATIUTE
170500000000     C*
170600000000     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
170700000000IF  1C     PARDET        IFEQ      'D'
170800000000     C                   MOVEL     INT(1)        DECPAR            9
170900000000X   1C                   ELSE
171000000000     C                   MOVEL     INT(2)        DECPAR
171100000000E   1C                   ENDIF
171200000706     C*
171300000706     C* IMPOSTA LA DESCRIZIONE SE LANCIO POSTE OPPURE NO
171400000706IF  1C     PARPOS        IFEQ      'S'
171500000706     C                   MOVEL     DEC(5)        DECPOS           10
171600000706X   1C                   ELSE
171700000706     C                   MOVEL     DEC(6)        DECPOS
171800000706E   1C                   ENDIF
171900030911     C*
172000030911     C* IMPOSTA L'ELENCOI DEI NETWORK TARIFFE RICHIESTI NEL LANCIO
172100030911     C                   MOVEL     *blanks       DECNTW           15
172200030911IF  1C     PARNT1        IFNE      *blanks
172300030911     C                   EVAL      DECNTW = %trim(DECNTW) + PARNT1 + ' '
172400030911E   1C                   ENDIF
172500030911IF  1C     PARNT2        IFNE      *blanks
172600030911     C                   EVAL      DECNTW = %trim(DECNTW) + PARNT2 + ' '
172700030911E   1C                   ENDIF
172800030911IF  1C     PARNT3        IFNE      *blanks
172900030911     C                   EVAL      DECNTW = %trim(DECNTW) + PARNT3 + ' '
173000030911E   1C                   ENDIF
173100030911IF  1C     PARNT4        IFNE      *blanks
173200030911     C                   EVAL      DECNTW = %trim(DECNTW) + PARNT4 + ' '
173300030911E   1C                   ENDIF
173400030911     C                   IF        DECNTW = *blanks
173500030911     C                   EVAL      DECNTW = 'TUTTI'
173600030911E   1C                   ENDIF
173700000000     C*
173800000000     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
173900000000IF  1C     PARDET        IFEQ      'S'
174000000000     C                   MOVEL     'S'           PARTOT                         *TOTALI AREA/DIV/GEN
174100000000E   1C                   ENDIF
174200000000     C*
174300000000     C* CARICA ORGANIGRAMMA
174400000000     C                   EXSR      CARORG
174500000000     C*
174600000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
174700000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
174800000000     C     PARDIV        IFEQ      *BLANKS
174900000000     C     PARARE        ANDEQ     *ZEROS
175000000000     C     PARFIL        ANDEQ     *ZEROS
175100000000     C                   SETOFF                                       02
175200000000     C                   ELSE
175300000000     C                   SETON                                        02
175400000000     C                   END
175500000000     C*
175600000000     C* SE LANCIO PER SALDI INTESTA STAMPE CON ANNO/MESE (IND.03 OFF)
175700000000     C* SE LANCIO ESTEMPORANEO INTESTA CON DATA INIZIO E FINE (IND.03 ON)
175800000000     C     PARTLA        IFEQ      'S'
175900000000     C                   SETOFF                                       03
176000000000     C                   END
176100000000     C     PARTLA        IFEQ      'E'
176200000000     C                   SETON                                        03
176300000000     C                   END
176301130207     C*
176302130207     c* pulizia file wfvpr
176303130207     c                   if        parsnr='S'
176304130207     c                   call      'QCMDEXC'
176305130207     c                   parm      C_Cmd_ClrWF   Qcmd
176306130207     c                   parm                    Qlen
176307130207     c
176308130218     c                   open      WFVPR00F
176309130218     c* pulizia file wfvprt
176310130218     c                   call      'QCMDEXC'
176311130218     c                   parm      C_Cmd_ClrWFt  Qcmd
176312130218     c                   parm                    Qlen
176313130218     c
176314130218     c                   open      WFVPRT0F
176315130207     c                   endif
176400000000     C*
176500000000     C                   ENDSR
176600000000     C*------------------------------------------------------------------------*
176700000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
176800000000     C*------------------------------------------------------------------------*
176900000000     C     CARORG        BEGSR
177000000000     C*
177100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
177200000000     C                   Z-ADD     *ZEROS        KORFIL
177300000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
177400000000     C  N99              READ      AZORG01L                               99
177500000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
177600000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
177700000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
177800000000     C     ORGFAG        IFNE      'V'
177900000000     C                   ADD       1             I
178000000000     C                   Z-ADD     ORGFIL        FIL(I)
178100000000     C                   MOVEL     ORGDES        DFIL(I)
178200000706     C                   MOVEL     ORGDE3        OG143
178300020627     C                   MOVEL     §OGNTW        PFIL(I)
178400000000     C                   END
178500000000     C                   END                                                    <-- 2 ---
178600000000     C                   READ      AZORG01L                               99
178700000000     C                   END                                                    <-- 1 ---
178800000000     C*
178900000000     C                   ENDSR
178901130207     C*------------------------------------------------------------------------*
178902130207     C* Scrittura dati stampati su file
178903130207     C*------------------------------------------------------------------------*
178904130208     C     riewfvpr      BEGSR
178905130208     c                   clear                   wfvpr
178906130207     c                   eval      WFrANN=parANN
178907130207     c                   eval      WFrMmi=parMMI
178908130207     c                   eval      WFrMmf=parMMF
178909130207     C                   EVAL      WFRDIV=DEPDIV
178910130207     C                   EVAL      WFRDID=DESDIV
178911130207     C                   EVAL      WFRARE=DEPARE
178912130207     C                   EVAL      WFRARD=DESARE
178913130207     C                   EVAL      WFRFIL=DEPFIL
178914130207     C                   EVAL      WFRFID=WDESFIL
178915130207     c                   eval      WFrKSC=DEPKSC
178916130207     c                   eval      WFrRAG=DESCLI
178917130207     c                   eval      WFrCTR=DEPCTR
178918130207     c                   eval      WFrDCT=DESCTR
178919130208     c                   eval      WFrDCv=DESdcv
178920130219     c                   eval      WFRMTCL=tctcl
178921130219     c                   eval      WFRMNCR=tcncr
178922130207     c                   eval      WFRMPP=stper
178923130207     c                   eval      WFRMCL=stncl
178924130207     c                   eval      WFRMKB=stpkb
178925130207     c                   eval      WFRMSP=stnsp
178926130219     c                   eval      WFRMLB=tcvlb
178927130219     c                   eval      WFRMLC=tcvlc
178928130207     c                   eval      WFRMNG=strng
178929130207     c                   eval      WFRMAK=stcak
178930130207     c                   eval      WFRMAC=stcac
178931130207     c                   eval      WFRMIK=stcik
178932130207     c                   eval      WFRMIC=stcic
178933130208     c                   endsr
178934130208     C*------------------------------------------------------------------------*
178935130208     C* Scrittura dati stampati su file - progressivi + write del record
178936130208     C*------------------------------------------------------------------------*
178937130208     C     riewfvpr_p    BEGSR
178938130219     c                   eval      WFRPTCL=tptcl
178939130219     c                   eval      WFRPNCR=tpncr
178940130208     c                   eval      WFRPPP=spper
178941130208     c                   eval      WFRPCL=stncl
178942130208     c                   eval      WFRPKB=stpkb
178943130208     c                   eval      WFRPSP=stnsp
178944130219     c                   eval      WFRPLB=tpvlb
178945130219     c                   eval      WFRPLC=tpvlc
178946130208     c                   eval      WFRPNG=sprng
178947130208     c                   eval      WFRPAK=stcak
178948130208     c                   eval      WFRPAC=stcac
178949130208     c                   eval      WFRPIK=spcik
178950130208     c                   eval      WFRPIC=spcic
178951130207     c                   endsr
178952130218     C*------------------------------------------------------------------------*
178953130218     C* Scrittura TOTALI stampati su file
178954130218     C*------------------------------------------------------------------------*
178955130218     C     riewfvprt     BEGSR
178956130218     c                   clear                   wfvprt
178957130218     c                   eval      WFtANN=parANN
178958130218     c                   eval      WFtMmi=parMMI
178959130218     c                   eval      WFtMmf=parMMF
178960130218     c                   if        desdiv <> *blanks
178961130218     C                   EVAL      WFtDID=DESDIV
178962130218     C                   EVAL      WFtDIV=DEPDIV
178963130218     c                   endif
178964130218     c                   if        desare <> *blanks
178965130218     C                   EVAL      WFtARE=DEPARE
178966130218     C                   EVAL      WFtARD=DESARE
178967130218     c                   endif
178968130218     c                   if        wdesfil <> *blanks
178969130218     C                   EVAL      WFtFIL=DEPFIL
178970130218     C                   EVAL      WFtFID=WDESFIL
178971130218     c                   endif
178974130218     c                   eval      WFTMTCL=m_wcpncl
178975130218     c                   eval      WFTMNCR=m_wcpncr
178976130218     c                   eval      WFtMPP=stper
178977130218     c                   eval      WFtMCL=stncl
178978130218     c                   eval      WFtMKB=stpkb
178979130218     c                   eval      WFtMSP=stnsp
178980130218     c                   eval      WFtMLB=m_stvlb
178981130218     c                   eval      WFtMLC=m_stvlc
178982130218     c                   eval      WFtMNG=strng
178983130218     c                   eval      WFTpTCL=p_wcpncl
178984130218     c                   eval      WFTpNCR=p_wcpncr
178985130218     c                   eval      WFTPPP=spper
178986130218     c                   eval      WFTPCL=spncl
178987130218     c                   eval      WFTPKB=sppkb
178988130218     c                   eval      WFTPSP=spnsp
178989130218     c                   eval      WFTPLB=p_spvlb
178990130218     c                   eval      WFTPLC=p_spvlc
178991130218     c                   eval      WFTPNG=sprng
178992130218     c                   endsr
179000000000     O*------------------------------------------------------------------------*
179100000000     OPRTF198   E            TESTA          1 02
179200991104     O                                            8 'TIST20R '
179300950706     O                                           90 '***    STATISTICA   '
179400950706     O                                          110 'V O L U M I    ***  '
179500980403     O                       DECPAR             120
179600980421     O                                          150 'Data'
179700000000     O                       DATA               159 '  /  /  '
179800980421     O                                          169 'Ora'
179900980421     O                       ORA                178 '  :  :  '
180000980421     O                                          193 'Pag.'
180100000000     O                       PAGE          Z    198
180200000000     O*------------------------
180300000000     O          E   N03      TESTA          1
180400030911     O                                           17 'Network tariffe: '
180500030911     O                       DECNTW              32
180600000000     O                                           83 'ANNO:'
180700000000     O                       PARANN              88
180800000000     O                                           97 'MESI:'
180900000000     O                       PARMMI             100
181000000000     O                                          102 '/'
181100000000     O                       PARMMF             105
181200000706     O                       DECPOS             198
181300000000     O*------------------------
181400000000     O          E    02      TESTA       1  1
181500000000     O                                           20 'PARZIALIZZAZIONI:   '
181600000000     O                                           40 '             DIVIS.:'
181700000000     O                       PARDIV              41
181800000000     O                                           61 '               AREA:'
181900000000     O                       PARARE        Z     64
182000090710     O                                           84 '            FILIALE:'
182100000000     O                       PARFIL        Z     87
182200000000     O*------------------------
182300991102     O          E            TESTA       1  1
182400000000     O                       TES(1)              66
182500000000     O                       TES(1)             132
182600991229     O                       TES(1)             198
182700991229     O*------------------------
182800991229     O          E            TESTA          1
182900090710     O                                           10 '! FILIALE:'
183000991229     O                       DESFIL            +  1
183100991229     O                                         +  1 '   AREA:'
183200991229     O                       DESARE            +  1
183300991229     O                                         +  1 '   DISTRETTO:'
183400991229     O                       DESDIV            +  1
183500991229     O                                         +  1 '   SCOST. (A) DA: '
183600991229     O                       PARRG1        N   +  1
183700991229     O                                         +  1 '%'
183800991229     O                                         +  1 '   A:'
183900991229     O                       PARRG2        N   +  1
184000991229     O                                         +  1 '%'
184100991229     O                                         +  3 ' RAPP.MIN. (B): CON IMP'
184200991229     O                       PARPID        N   +  1
184300991229     O                                         +  1 '%'
184400991229     O                                         +  1 ' SENZA IMP'
184500991229     O                       PARPSD        N   +  1
184600991229     O                                         +  1 '%'
184700991229     O                                          198 '!'
184800991229     O*-------------------------                                       --------*
184900991229     O          E            TESTA          1
185000991229     O                                            1 '!'
185100991229     O                       CHKCML            + 10
185200991229     O                                          115 'MIN.   (A)   :±'
185300991229     O                       PARMIN        N   +  0
185400991229     O                                         +  1 '%'
185500991229     O                                          198 '!'
185600991102     O*------------------------
185700991102     O          E            TESTA          1
185800991102     O                       TES(1)              66
185900991102     O                       TES(1)             132
186000991102     O                       TES(1)             198
186100000000     O*------------------------
186200000000     O          E            TESTA          1
186300000000     O                       TES(2)              66
186400000000     O                       TES(3)             132
186500000000     O                       TES(4)             198
186600000000     O               01                         175 'VALORE ATTUALE '
186700000000     O               01                         196 'VALORE PROPOSTO'
186800000000     O*------------------------
186900000000     O          E            TESTA          1
187000000000     O                       TES(5)              66
187100000000     O               01                           9 'CLIENTE'
187200000000     O               01                          50 'TARIFFA'
187300000000     O                       TES(6)             132
187400000000     O                       TES(7)             198
187500000000     O               01                         175 'KG/MC    MC/COL'
187600000000     O               01                         196 ' KG/MC   MC/COL'
187700000000     O*------------------------
187800000000     O          E            TESTA          1
187900000000     O                       TES(1)              66
188000000000     O                       TES(1)             132
188100000000     O                       TES(1)             198
188200000000     O*------------------------------------------------------------------------*
188300000000     O          E            RIGCLI         1
188400000000     O                                            1 '!'
188500950629     O                       DEPKSC        Z   +  1
188600000000     O                       DESCLI            +  1
188700030909     O                       DESTTR            +  1
188800980421     O                       DEPCTR            +  1
188900991104     O                       DESCTR            +  1
189000991104     O                       DESDCV            +  1
189100991104     O                                         +  2 '!'
189200991104     O                       PARMMF            +  1
189300991104     O                                         +  1 '!'
189400000517     O                       STPER         K     87
189500000517     O                       STNCL         Z     96
189600000517     O                       STPKB         4    108
189700000517     O                       STNSP         Z    117
189800000517     O                                          119 '!'
189900000517     O                       STVLB         4    131
190000000517     O                       STVLC         4    143
190100000519     O                       STRNG         K    152
190200000718     O*                      STTTP         K    159
190300000517     O                                          160 '!'
190400000517     O                       STCAK         4    168
190500000517     O                       STCAC         4    177
190600000517     O                       STCIK         4    189
190700000517     O                       STCIC         4    196
190800941206     O                                          198 '!'
190900991029     O*------------------------------------------------------------------------*
191000991029     O          E            RIGPRO         1
191100991029     O                                            1 '!'
191200991104     O                                         + 72 '! PR'
191300991104     O                                         +  1 '!'
191400000517     O                       SPPER         K     87
191500000517     O                       STNCL         Z     96
191600000517     O                       STPKB         4    108
191700000517     O                       STNSP         Z    117
191800000517     O                                          119 '!'
191900000517     O                       STVLB         4    131
192000000517     O                       STVLC         4    143
192100000519     O                       SPRNG         K    152
192200040219     O*---                   SPTTP         K    159
192300000517     O                                          160 '!'
192400000517     O                       STCAK         4    168
192500000517     O                       STCAC         4    177
192600000517     O                       SPCIK         4    189
192700000517     O                       SPCIC         4    196
192800991029     O                                          198 '!'
192900000000     O*------------------------------------------------------------------------*
193000000000     O          E            RIGTOT         1
193100000000     O                                            1 '!'
193200000000     O                       STDES1            +  1
193300000000     O                       STDES2            +  1
193400991104     O                                         + 28 '!'
193500991104     O                       PARMMF            +  1
193600991104     O                                         +  1 '!'
193700000517     O                       STPER         K     87
193800000517     O                       STNCL         Z     96
193900000517     O                       STPKB         4    108
194000000517     O                       STNSP         Z    117
194100000517     O                                          119 '!'
194200000517     O                       STVLB         4    131
194300000517     O                       STVLC         4    143
194400000519     O                       STRNG         K    152
194500040219     O*---                   STTTP         K    159
194600000517     O                                          160 '!'
194700000000     O                                          198 '!'
194800991102     O*------------------------------------------------------------------------*
194900991102     O          E            RIGTOP         1
195000991102     O                                            1 '!'
195100991104     O                                         + 72 '! PR'
195200991104     O                                         +  1 '!'
195300000517     O                       SPPER         K     87
195400000517     O                       SPNCL         Z     96
195500000517     O                       SPPKB         4    108
195600000517     O                       SPNSP         Z    117
195700000517     O                                          119 '!'
195800000517     O                       SPVLB         4    131
195900000517     O                       SPVLC         4    143
196000000519     O                       SPRNG         K    152
196100040219     O*---                   SPTTP         K    159
196200000517     O                                          160 '!'
196300991102     O                                          198 '!'
196400000000     O*------------------------
196500000000     O          E            RIGSEP         1
196600000000     O                       TES(1)              66
196700000000     O                       TES(1)             132
196800000000     O                       TES(1)             198
196900000000     O*------------------------
197000000000     O          E            STAFIN           63
197100000000     O                                           76 '*** FINE STAMPA ***'
197200941206     O*------------------------------------------------------------------------*
197300980421** -TES-   (DESCRIZIONI  STAMPA)
197400000000------------------------------------------------------------------  1
197500000000!                                                                   2
197600000517       ! MM ! (B) %RAPP.  NUM.                 NUM. !    VOLUME     3
197700040219   VOLUME    (A) %         !                                     !  4
197800941207!                                                                   5
197900000517       ! PR !   COLLI    COLLI        PESO    SPED. ! BOLL./REALE   6
198000040219     CML     SCOST.        !                                     !  7
198100000000                                                                    8
198200000000                                                                    9
198300090710TOTALE FILIALE                                                     10
198400000000TOTALE AREA                                                        11
198500980421TOTALE DISTRETTO                                                   12
198600000000TOTALE GENERALE                                                    13
198700000000** -INT-   (INTESTAZIONE)
198800000000DETTAGLIO                                                       1
198900000000SINTETICA                                                       2
1990009506293
1991009804214
199200950629R E A L E 5
199300980421** -DEC-   (DECODIFICHE RIGHE)
199400980421ASSEGNATI                                                            1
199500980421FRANCHI                                                              2
199600991102CON IMPIANTO                                                         3
199700991102SENZA IMPIANTO                                                       4
199800000706POSTE                                                                5
199900000706NO POSTE                                                             6
2000000007067
2001000007068
2002000007069
20030000070610
