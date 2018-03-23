000100970613     H*------------------------------------------------------------------------*
000200970613     H* CAPPARIO - STAMPA CAPPARIO - ESEGUE
000300970613     H*------------------------------------------------------------------------*
000400970613     H DECEDIT('0,') DATEDIT(*DMY.)
000500970613     F*------------------------------------------------------------------------*
000600970613     F* DATA BASE
000700970613     F*------------------------------------------------------------------------*
000800000428     FAZPPC01L  IF   E           K DISK
000900000428     FAZPPL01L  IF   E           K DISK
001000000529     FAZPPE01L  IF   E           K DISK
001100970613     FAZCPC01L  IF   E           K DISK
001200970613     FAZCPC02L  IF   E           K DISK
001300970613     F                                     RENAME(AZCPC000:AZCPC2)
001400970613     FAZCPC03L  IF   E           K DISK
001500970613     F                                     RENAME(AZCPC000:AZCPC3)
001600970613     FAZCPT01L  IF   E           K DISK
001700970613     FAZCPL01L  IF   E           K DISK
001800970613     FAZCPE01L  IF   E           K DISK
001900970613     FAZORG01L  IF   E           K DISK
002000970613     FTABEL00F  IF   E           K DISK
002100970613     FTISI05P   O    E             PRINTER OFLIND(*IN01)
002200970613     D*------------------------------------------------------------------------*
002300970613     D* SCHIERE
002400970613     D*------------------------------------------------------------------------*
002500970613     D*------------------
002600970613     D* DECODIFICHE
002700970613     D*------------------
002800970613     D DEC             S             60    DIM(10) CTDATA PERRCD(1)
002900970613     D*-------------------
003000970613     D* FILIALI
003100970613     D*-------------------
003200971118     D ORG             S              3  0 DIM(999)                             *CODICE
003300971118     D DORG            S             20    DIM(999)                             *DESCRIZIONE
003400970613     D*-------------------
003500970613     D* NAZIONI
003600970613     D*-------------------
003700011218     D NAR             S              3    DIM(500)                             *CODICE
003800011218     D DNAR            S             20    DIM(500)                             *DESCRIZIONE
003900970613     D*-------------------
004000970613     D* PROVINCE
004100970613     D*-------------------
004200970613     D PRV             S              2    DIM(300)                             *CODICE
004300970613     D DPRV            S             35    DIM(300)                             *DESCRIZIONE
004400970613     D*-------------------
004500970613     D* TIPI INOLTRO
004600970613     D*-------------------
004700970613     D INL             S              1    DIM(10)                              *CODICE
004800970613     D DINL            S             20    DIM(10)                              *DESCRIZIONE
004900970613     D*------------------------------------------------------------------------*
005000970613     D* INPUT
005100970613     D*------------------------------------------------------------------------*
005200970613     D*------------------
005300970613     D* DS NAZIONI
005400970613     D*------------------
005500970613     D DS15          E DS
005600970613     D*------------------
005700970613     D* DS PROVINCIE
005800970613     D*------------------
005900970613     D DSPR          E DS
006000970613     D*------------------
006100970613     D* DS TIPI INOLTRO
006200970613     D*------------------
006300970613     D DS9A          E DS
006400970613     D*------------------
006500970613     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600970613     D*------------------
006700970613     D WLBDA8          DS                  INZ
006800970613     D  G08DAT                 1      8  0
006900970613     D  G08INV                 9     16  0
007000970613     D  G08ERR                17     17
007100970613     D  G08TGI                18     22  0
007200971118     D*------------------
007300970613     D* DS CAMPO DEDMIX -STAMPA-
007400970613     D*------------------
007500970613     D                 DS
007600970613     D  DECPRV                 1      2
007700971118     D  DEDPRX                 4      4
007800971118     D  DECPRA                 6      7
007900970613     D  DECTFP                 1      3
008000971118     D  DEDTFP                 5     19
008100971118     D  DECLOC                 1     19
008200971118     D  DEDMIX                 1     19
008300970613     D*-------------------
008400970613     D* DS PARAMETRI LANCIO
008500970613     D*-------------------
008600970613     D PARAM           DS
008700970613     D  PARTOR                 1      1
008800970613     D  PARCA1                 2     10
008900970613     D  PARCA2                11     19
009000970613     D  PARNA1                20     22
009100970613     D  PARNA2                23     25
009200970613     D  PARPR1                26     27
009300970613     D  PARPR2                28     29
009400970613     D  PARLN1                30     32  0
009500970613     D  PARLN2                33     35  0
009600970613     D  PARISO                36     36
009700970613     D  PARTTC                37     41  0
009800970613     D  PARTEM                42     42
009900970613     D  PARTIC                43     43
010000970613     D  PARLOC                44     44
010100970613     D  PARLIV                45     45
010200970613     D  PARVER                46     50  0
010300970613     D  PARDRI                51     58  0
010400971003     D  PAROBS                59     59
010500970613     D*-------------------
010600970613     D* ARCHITETTURA
010700970613     D*-------------------
010800970613     D KPJBA         E DS
010900970613     D  PROFIL                 1     10
011000970613     C*------------------------------------------------------------------------*
011100970613     C* MAIN LINE
011200970613     C*------------------------------------------------------------------------*
011300970613     C*
011400970613     C* STAMPA PAGINA PARAMETRI DI LANCIO
011500970613     C                   EXSR      STAPAR
011600970613     C*
011700970613     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011800970613     C                   EXSR      SETFIL
011900970613     C*
012000970613     C* CICLO FINO A FINE FILE
012100970613DO  1C     FINL00        DOWEQ     '0'
012200970613     C*
012300970613     C* CONTROLLA LE ROTTURE PER SALTO PAGINA
012400970613     C                   EXSR      CHKROT
012500970613     C*
012600970613     C* STAMPA RIGA DETTAGLIO -CAP-
012700970613     C                   EXSR      STACAP
012800970613     C*
012900970613     C* CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
013000970613     C                   EXSR      LOPCPL
013100970613     C*
013200970613     C* LEGGE IL RECORD SUCCESSIVO
013300970613     C                   EXSR      LETFIL
013400970613E   1C                   ENDDO                                                  *FINE CAP
013500970613     C*
013600970613     C* STAMPA RIGA DI FINE PROSPETTO
013700970613     C                   WRITE     SI05RI                                       *RIGA SEPARAZIONE
013800970613     C                   WRITE     SI05FI                                       *RIGA FINE STAMPA
013900970613     C*
014000970613     C                   SETON                                        LR
014100970613     C*------------------------------------------------------------------------*
014200970613     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
014300970613     C*------------------------------------------------------------------------*
014400970613     C     SETFIL        BEGSR
014500970613     C*
014600970613     C* IMPOSTA LA NAZIONE
014700970613     C                   MOVEL     PARNA1        DEPNA1
014800970613IF  1C     PARNA1        IFEQ      'I  '
014900970613     C                   MOVEL     *BLANKS       DEPNA1
015000970613E   1C                   ENDIF
015100970613     C*
015200970613     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
015300970613IF  1C     PARTOR        IFEQ      'C'                                          *PER CAP
015400970613     C                   Z-ADD     PARVER        KCCVER
015500970613     C                   MOVEL     DEPNA1        KCCNAR
015600970613     C                   MOVEL     PARCA1        KCCCAP
015700970613     C     KEYCPC        SETLL     AZCPC01L                           99
015800970613E   1C                   ENDIF
015900970613     C*
016000970613IF  1C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
016100970613     C                   Z-ADD     PARVER        KCCVER
016200970613     C                   MOVEL     DEPNA1        KCCNAR
016300970613     C                   MOVEL     PARPR1        KCCPRV
016400970613     C                   MOVEL     PARCA1        KCCCAP
016500970613     C     KE2CPC        SETLL     AZCPC02L                           99
016600970613E   1C                   ENDIF
016700970613     C*
016800970613IF  1C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
016900970613     C                   Z-ADD     PARVER        KCCVER
017000970613     C                   MOVEL     DEPNA1        KCCNAR
017100010730     C*---               Z-ADD     PARLN1        KCCLNA
017200010730     C*---               MOVEL     PARCA1        KCCCAP
017300010730     C                   Z-ADD     *ZEROS        KCCLNA
017400010730     C                   MOVEL     *BLANKS       KCCCAP
017500970613     C     KE3CPC        SETLL     AZCPC03L                           99
017600970613E   1C                   ENDIF
017700970613     C*
017800970613     C* LETTURA RECORD IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
017900970613IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
018000970613     C                   EXSR      LETFIL                                       *LETTURA RECORD
018100970613X   1C                   ELSE                                                   *FINE FILE
018200970613     C                   MOVEL     '1'           FINL00                         *FINE PGM
018300970613E   1C                   ENDIF
018400970613     C*
018500970613     C                   ENDSR
018600970613     C*------------------------------------------------------------------------*
018700970613     C* LETFIL - LETTURA SUCESSIVA RECORD
018800970613     C*------------------------------------------------------------------------*
018900970613     C     LETFIL        BEGSR
019000970613     C*
019100970613     C* LEGGE FINO A:
019200970613     C*      -FINE FILE
019300970613     C*      -TROVATO RECORD VALIDO
019400970613     C                   MOVEL     'N'           WRECOK
019500970613DO  1C     *IN99         DOWEQ     *OFF
019600970613     C     WRECOK        ANDEQ     'N'
019700970613IF  2C     PARTOR        IFEQ      'C'                                          *PER CAP
019800980603     C     KCCVER        READE     AZCPC01L                               99
019900970613E   2C                   ENDIF
020000970613     C*
020100970613IF  2C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
020200980603     C     KCCVER        READE     AZCPC02L                               99
020300970613E   2C                   ENDIF
020400970613     C*
020500970613IF  2C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
020600980603     C     KCCVER        READE     AZCPC03L                               99
020700970613E   2C                   ENDIF
020800970613     C*
020900970613IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
021000970613     C                   MOVEL     '1'           FINL00                         *FINE PROGRAMMA
021100970613X   2C                   ELSE                                                   *RECORD TROVATO
021200970613     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
021300970613E   2C                   ENDIF
021400970613E   1C                   ENDDO
021500970613     C*
021600970613     C                   ENDSR
021700970613     C*------------------------------------------------------------------------*
021800970613     C* CHKREC - CONTROLLA VALIDITA' RECORD
021900970613     C*------------------------------------------------------------------------*
022000970613     C     CHKREC        BEGSR
022100970613     C*
022200970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
022300010730     C                   MOVEL     'N'           $LNLOC            1            *SI LOC.LINEE SELEZ.
022400970613     C*
022500971003     C* CAP ANNULLATO, ESCLUDE
022600970613IF  1C     CPCATB        IFNE      *BLANKS
022700970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022800970613E   1C                   ENDIF
022900970613     C*
023000970613     C* NAZIONE <> DA QUELLE SELEZIONATE, ESCLUDE
023100970613IF  1C     PARNA1        IFNE      *BLANKS
023200970613IF  2C     PARNA1        IFEQ      'I  '
023300970613     C                   MOVEL     *BLANKS       DEPNA1
023400970613     C                   MOVEL     *BLANKS       DEPNA2
023500970613X   2C                   ELSE
023600970613     C                   MOVEL     PARNA1        DEPNA1
023700970613     C                   MOVEL     PARNA2        DEPNA2
023800970613E   2C                   ENDIF
023900970613IF  2C     CPCNAR        IFGE      DEPNA1
024000970613     C     CPCNAR        ANDLE     DEPNA2
024100970613X   2C                   ELSE
024200970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024300970613E   2C                   ENDIF
024400970613E   1C                   ENDIF
024500970613     C*
024600970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
024700970613IF  1C     PARCA1        IFNE      *BLANKS
024800970613IF  2C     CPCCAP        IFGE      PARCA1
024900970613     C     CPCCAP        ANDLE     PARCA2
025000970613X   2C                   ELSE
025100970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025200970613E   2C                   ENDIF
025300970613E   1C                   ENDIF
025400970613     C*
025500970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
025600970613IF  1C     PARPR1        IFNE      *BLANKS
025700970613IF  2C     CPCPRV        IFGE      PARPR1
025800970613     C     CPCPRV        ANDLE     PARPR2
025900970613X   2C                   ELSE
026000970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026100970613E   2C                   ENDIF
026200970613E   1C                   ENDIF
026300971003     C*
026400971003     C* CAP OBSOLETO E RICHIESTI I NON OBSOLETI, ESCLUDE
026500971003IF  1C     PAROBS        IFEQ      'N'
026600971003     C     CPCFI1        ANDEQ     'O'
026700971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026800971003E   1C                   ENDIF
026900010730     C*
027000010730     c* se scelta una linea controlla che sia nei CAP che nelle LOCALITA' ci siano record
027100010730     c* che abbiano la linea STANDARD o la OLTRE o la OLTRE 2 uguale a quella selezionata
027200010730     C                   MOVEL     'N'           $LNLOC                         *LOC.LINEE SELEZ.
027300010730IF  1C     WRECOK        IFEQ      'S'                                          *CAP ANCORA VALIDO
027400010730IF  2C     PARLN1        IFGT      *ZEROS
027500010730     C*
027600010730     C* solo se esistono eccezioni a livello di località
027700010730IF  3C     CPCESL        IFEQ      'S'                                          *LOC.PARTICOLARI
027800010730     C                   EXSR      CLNLOC
027900010730E   3C                   ENDIF
028000010730IF  3C                   IF        (CPCLNA >= PARLN1 AND CPCLNA <= PARLN2) OR
028100010730     C                             (CPCLOL >= PARLN1 AND CPCLOL <= PARLN2) OR
028200010730     C                             (CPCLO2 >= PARLN1 AND CPCLO2 <= PARLN2) OR
028300010730     C                             $LNLOC = 'S'                                 *SI CHK X LINEE LOC
028400010730X   3C                   ELSE
028500010730     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028600010730E   3c                   ENDIF
028700010730E   2C                   ENDIF
028800010730E   1C                   ENDIF
028900970613     C*
029000970613     C                   ENDSR
029100010730     C*--------------------------------------------------------------------------------------------*
029200010730     C* CLNLOC - CONTROLLA CHE LE LOCALITA' DEL CAP ABBIANO LINEE FRA QUELLE SELEZIONATE
029300010730     C*--------------------------------------------------------------------------------------------*
029400010730     C     CLNLOC        BEGSR
029500010730     C*
029600010730     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
029700010730     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
029800010730     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
029900010730     C     KEYCPL        SETLL     AZCPL01L
030000010730     C     KEYCPL        READE     AZCPL01L                               98
030100010730DO  1C     *IN98         DOWEQ     *OFF
030200010730IF  2C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
030300010730     C     CPLLIV        ANDEQ     'L'                                          *LOC. PARTICOLARE
030400010730     C*
030500010730     C* CONTROLLA LE LINEE FRA QUELLE SELEZIONATE
030600010730IF  3C                   IF        (CPLLNA >= PARLN1 AND CPLLNA <= PARLN2) OR
030700010730     C                             (CPLLOL >= PARLN1 AND CPLLOL <= PARLN2) OR
030800010730     C                             (CPLLO2 >= PARLN1 AND CPLLO2 <= PARLN2)
030900010730     C                   MOVEL     'S'           $LNLOC                         *SI LOC.LINEE SELEZ.
031000010730     C                   LEAVE
031100010730E   3C                   ENDIF
031200010730E   2C                   ENDIF
031300010730     C*
031400010730     C     KEYCPL        READE     AZCPL01L                               98
031500010730E   1C                   ENDDO                                                  *FINE LOCALITA' CAP
031600010730     C*
031700010730     C                   ENDSR
031800970613     C*------------------------------------------------------------------------*
031900971003     C* CHKROT - CONTROLLA LE ROTTURE PER SALTO PAGINA
032000970613     C*------------------------------------------------------------------------*
032100970613     C     CHKROT        BEGSR
032200970613     C*
032300970613     C* ROTTURA NAZIONE -SEMPRE-
032400970613IF  1C     CPCNAR        IFNE      ROTNAR
032500970613     C                   MOVEL     CPCNAR        ROTNAR                         *DEPOSITO ROTTURA
032600970613     C                   MOVEL     CPCNAR        INCNAR                         *PER INTESTAZIONE
032700970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
032800970613E   1C                   ENDIF
032900970613     C*
033000970613     C* ROTTURA PROVINCIA -SE ORDINAMENTO PER PROVINCIA-
033100970613IF  1C     PARTOR        IFEQ      'P'
033200970613IF  2C     CPCPRV        IFNE      ROTPRV
033300970613     C                   MOVEL     CPCPRV        ROTPRV                         *DEPOSITO ROTTURA
033400970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
033500970613E   1C                   ENDIF
033600970613E   1C                   ENDIF
033700970613     C*
033800970613     C* ROTTURA LINEA DI ARRIVO -SE ORDINAMENTO PER LINEA DI ARRIVO-
033900970613IF  1C     PARTOR        IFEQ      'L'
034000970613IF  2C     CPCLNA        IFNE      ROTLNA
034100970613     C                   MOVEL     CPCLNA        ROTLNA                         *DEPOSITO ROTTURA
034200970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034300970613E   1C                   ENDIF
034400970613E   1C                   ENDIF
034500970613     C*
034600970613     C                   ENDSR
034700970613     C*------------------------------------------------------------------------*
034800970613     C* STACAP - STAMPA RIGA DI DETTAGLIO -CAP-
034900970613     C*------------------------------------------------------------------------*
035000970613     C     STACAP        BEGSR
035100970613     C*
035200970613     C* REIMPOSTA LE VARIABILI DI LAVORO
035300000529     C                   MOVEL     'CPC'         STEP1             3
035400970613     C                   MOVEL     'N'           WSTCAP                         *CAP NON STAMPATO
035500970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
035600970613     C*
035700970613     C* CONTROLLA SE IL CAP E' DA STAMPARE
035800970613     C                   EXSR      CHKCAP
035900970613IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
036000970613     C* CAMPI
036100970613     C                   MOVEL     CPCCAP        DECCAP
036200970613     C                   MOVEL     CPCFLG        DECFLG
036300970917     C                   MOVEL     CPCFI1        DECFI1
036400970613     C                   MOVEL     *BLANKS       DEDMIX
036500970613     C                   MOVEL     CPCPRV        DECPRV
036600970613     C                   MOVEL     CPCPRA        DECPRA
036700970613IF  2C     CPCPRA        IFNE      *BLANKS
036800970613     C                   MOVEL     '/'           DEDPRX
036900970613E   2C                   ENDIF
037000970613     C                   Z-ADD     CPCLNA        DECLNA
037100970613     C                   MOVEL     CPCZNC        DECZNC
037200970613     C                   MOVEL     CPCZFD        DECZFD
037300970613IF  2C     CPCLKS        IFGT      *ZEROS
037400000518     C     CPCLMS        ORGT      *ZEROS
037500970613     C                   MOVEL     CPCLOS        DECLOS
037600970613     C                   MOVEL     CPCZOS        DECZOS
037700970613     C                   Z-ADD     CPCLKS        DECLKS
037800970613     C                   Z-ADD     CPCLMS        DECLMS
037900970613E   2C                   ENDIF
038000970613IF  2C     CPCLKG        IFGT      *ZEROS
038100970613     C     CPCLMC        ORGT      *ZEROS
038200970613     C                   MOVEL     CPCLOL        DECLOL
038300970613     C                   MOVEL     CPCZOL        DECZOL
038400970613     C                   Z-ADD     CPCLKG        DECLKG
038500970613     C                   Z-ADD     CPCLMC        DECLMC
038600970613E   2C                   ENDIF
038700000518IF  2C     CPCLK2        IFGT      *ZEROS
038800000518     C     CPCLM2        ORGT      *ZEROS
038900971218     C                   MOVEL     CPCLO2        DECLO2
039000971218     C                   MOVEL     CPCZO2        DECZO2
039100971218     C                   Z-ADD     CPCLK2        DECLK2
039200971218     C                   Z-ADD     CPCLM2        DECLM2
039300000518E   2C                   ENDIF
039400971218     C*
039500970613     C                   MOVEL     CPCCTS        DECCTS
039600970613     C                   MOVEL     CPCISO        DECINL
039700010727     C                   Z-ADD     CPCTTC        DECTTC
039800010727     C                   MOVEL     *ALL'9'       DECTTR
039900010727     C                   MOVE      CPCFI2        DECTTR
040000970613     C*
040100970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN DEL CAP
040200970613IF  2C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
040300970613     C                   Z-ADD     CPCTTC        DEPTTC                         *TIPO TEMPO
040400970613     C                   EXSR      LOPCPT
040500970613E   2C                   ENDIF
040600970613     C*
040700970613     C* STAMPA RIGA DI DETTALGIO
040800970613     C                   MOVEL     'S'           WSTCAP                         *CAP STAMPATO
040900970613     C                   EXSR      STMDET
041000970613     C*
041100970613     C* CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
041200970613     C                   EXSR      LOPCPE
041300970613E   1C                   ENDIF
041400970613     C*
041500970613     C                   ENDSR
041600970613     C*------------------------------------------------------------------------*
041700970613     C* CHKCAP - CONTROLLA SE IL CAP E' DA STAMPARE
041800970613     C*------------------------------------------------------------------------*
041900970613     C     CHKCAP        BEGSR
042000970613     C*
042100970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
042200970613     C*
042300970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
042400970613IF  1C     PARISO        IFNE      *BLANKS
042500970613IF  2C     CPCISO        IFNE      PARISO
042600970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042700970613E   2C                   ENDIF
042800970613E   1C                   ENDIF
042900970613     C*
043000970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
043100970613IF  1C     PARTTC        IFGT      *ZEROS
043200970613IF  2C     CPCTTC        IFNE      PARTTC
043300970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043400970613E   2C                   ENDIF
043500970613E   1C                   ENDIF
043600970613     C*
043700970613     C                   ENDSR
043800970613     C*------------------------------------------------------------------------*
043900970613     C* LOPCPE - CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
044000970613     C*------------------------------------------------------------------------*
044100970613     C     LOPCPE        BEGSR
044200970613     C*
044300970613     C* REIMPOSTA LE VARIABILI DI LAVOR
044400970613     C                   MOVEL     'N'           WSTECC                         *ECC NON STAMPATO
044500970613     C*
044600970613IF  1C     CPCESE        IFEQ      'S'                                          *ESISTONO ECCEZIONI
044700970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
044800970613     C                   Z-ADD     CPCVER        KCEVER                         *VERSIONE
044900970613     C                   MOVEL     CPCNAR        KCENAR                         *NAZIONE
045000970613     C                   MOVEL     CPCCAP        KCECAP                         *CAP
045100970613     C     KEYCPE        SETLL     AZCPE01L
045200970613     C     KEYCPE        READE     AZCPE01L                               98
045300970613DO  2C     *IN98         DOWEQ     *OFF
045400000529     C                   MOVEL     'CPE'         STEP1
045500970613IF  3C     CPEATB        IFEQ      *BLANKS                                      *VALIDI
045600970613     C* CAMPI
045700970613     C                   MOVEL     *BLANKS       DEDMIX
045800970613     C                   MOVEL     CPETFP        DECTFP
045900970613     C                   Z-ADD     CPELNA        DECLNA
046000970613     C                   MOVEL     CPEZNC        DECZNC
046100970613IF  4C     CPCLKS        IFGT      *ZEROS
046200970613     C     CPCLMS        ORGT      *ZEROS
046300970613     C                   MOVEL     CPELOS        DECLOS
046400970613     C                   MOVEL     CPEZOS        DECZOS
046500970613E   4C                   ENDIF
046600970613IF  4C     CPCLKG        IFGT      *ZEROS
046700970613     C     CPCLMC        ORGT      *ZEROS
046800970613     C                   MOVEL     CPELOL        DECLOL
046900970613     C                   MOVEL     CPEZOL        DECZOL
047000970613E   4C                   ENDIF
047100970613     C* DECODIFICHE
047200970613     C                   Z-ADD     1             I
047300970613     C                   SETOFF                                           98
047400970613     C     CPETFP        LOOKUP    ORG(I)                                 98
047500970613IF  4C     *IN98         IFEQ      *ON
047600970613     C                   MOVEL     DORG(I)       DEDTFP                         *TERMINAL PARTENZA
047700970613E   4C                   ENDIF
047800970613     C*
047900970613     C* STAMPA RIGA DI DETTALGIO
048000970613     C                   MOVEL     'S'           WSTECC                         *ECCEZIONI STAMPATE
048100970613     C                   EXSR      STMDET
048200970613E   3C                   ENDIF
048300970613     C     KEYCPE        READE     AZCPE01L                               98
048400970613E   2C                   ENDDO                                                  *FINE ECCEZIONI
048500970613E   1C                   ENDIF
048600970613     C*
048700970613     C                   ENDSR
048800970613     C*------------------------------------------------------------------------*
048900970613     C* LOPCPL - CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
049000970613     C*------------------------------------------------------------------------*
049100970613     C     LOPCPL        BEGSR
049200970613     C*
049300970613     C* DEPOSITA LA DESCRIZIONE DELLA PROVINCIA DEL CAP
049400970613     C                   Z-ADD     1             I
049500970613     C                   SETOFF                                           98
049600970613     C     CPCPRV        LOOKUP    PRV(I)                                 98
049700970613IF  1C     *IN98         IFEQ      *ON
049800970613     C                   MOVEL     DPRV(I)       DEPDPR
049900970613X   1C                   ELSE
050000970613     C                   MOVEL     *BLANKS       DEPDPR
050100970613E   1C                   ENDIF
050200970613     C*
050300970613     C* SE RICHIESTI IN STAMPA, LEGGE LE LOCALITA'
050400010730IF  1c     PARLOC        IFNE      'N'
050500970613     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
050600970613     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
050700970613     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
050800970613     C     KEYCPL        SETLL     AZCPL01L
050900970613     C     KEYCPL        READE     AZCPL01L                               98
051000970613DO  2C     *IN98         DOWEQ     *OFF
051100000529     C                   MOVEL     'CPL'         STEP1
051200970613IF  3C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
051300970613IF  4C     PARLOC        IFEQ      'D'                                          *SELEZ.LOC.DELIVERY
051400970613     C     CPLDLV        ANDEQ     'S'
051500970613     C     PARLOC        ORNE      'D'                                          *SELEZ.TUTTE
051600970613IF  5C     PARLIV        IFEQ      'S'                                          *SELEZ.LOC <> CAP
051700970613     C     CPLLIV        ANDEQ     'L'
051800970613     C     PARLIV        OREQ      'N'                                          *SELEZ.TUTTE
051900970613     C*
052000990525     C* NON STAMPA LA LOCALITA' SE E' UGUALE ALLA DESCRIZIONE DELLA PROVINCIA E
052100990525     C* SE E' GESTITA A LIVELLO DI LOCALITA'
052200970613IF  6C     DEPDPR        IFNE      CPLLOC                                       *-PEZZA-
052300990525     C     DEPDPR        OREQ      CPLLOC
052400990525     C     CPLLIV        ANDEQ     'L'
052500970613     C*
052600970613     C* CONTROLLA SE LA LOCALITA' E' DA STAMPARE
052700970613     C                   EXSR      CHKLOC
052800970613IF  7C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
052900970613     C* CAMPI
053000970613IF  8C     WSTECC        IFEQ      'S'                                          *ECCEZIONI STAMPATE
053100970613     C                   WRITE     SI05R2                                       *STAMPA RIGA SEPARAZ
053200970613     C                   MOVEL     'N'           WSTECC                         *X NON STAMPARE +RIG
053300970613E   8C                   ENDIF
053400970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
053500970613IF  8C     WSTCAP        IFEQ      'N'                                          *CAP NON STMPATO
053600970613     C                   MOVEL     CPCCAP        DECCAP                         *RIEMPE CAMPI CAP
053700970613E   8C                   ENDIF
053800970917     C                   MOVEL     CPLFI1        DECFI1
053900970613     C                   MOVEL     *BLANKS       DEDMIX
054000970613     C                   MOVEL     CPLLOC        DECLOC
054100000529     C                   MOVEL     CPLLOC        SAVLOC           35
054200970613IF  8C     CPLLIV        IFEQ      'L'                                          *LOC.GEST.LIV.LOC.
054300970613     C                   Z-ADD     CPLLNA        DECLNA
054400970613     C                   MOVEL     CPLZNC        DECZNC
054500970613     C                   MOVEL     CPLZFD        DECZFD
054600970621IF  9C     CPLLKS        IFGT      *ZEROS
054700970621     C     CPLLMS        ORGT      *ZEROS
054800970613     C                   MOVEL     CPLLOS        DECLOS
054900970613     C                   MOVEL     CPLZOS        DECZOS
055000970613     C                   Z-ADD     CPLLKS        DECLKS
055100970613     C                   Z-ADD     CPLLMS        DECLMS
055200970613E   9C                   ENDIF
055300970621IF  9C     CPLLKG        IFGT      *ZEROS
055400970621     C     CPLLMC        ORGT      *ZEROS
055500970613     C                   MOVEL     CPLLOL        DECLOL
055600970613     C                   MOVEL     CPLZOL        DECZOL
055700970613     C                   Z-ADD     CPLLKG        DECLKG
055800970613     C                   Z-ADD     CPLLMC        DECLMC
055900970613E   9C                   ENDIF
056000000518IF  9C     CPLLK2        IFGT      *ZEROS
056100000518     C     CPLLM2        ORGT      *ZEROS
056200971218     C                   MOVEL     CPLLO2        DECLO2
056300971218     C                   MOVEL     CPLZO2        DECZO2
056400971218     C                   Z-ADD     CPLLK2        DECLK2
056500971218     C                   Z-ADD     CPLLM2        DECLM2
056600000518E   9C                   ENDIF
056700000518     C*
056800970613     C                   MOVEL     CPLCTS        DECCTS
056900970613     C                   MOVEL     CPLISO        DECINL
057000010727     C                   Z-ADD     CPLTTC        DECTTC
057100010727     C                   MOVEL     *ALL'9'       DECTTR
057200010727     C                   MOVE      CPLFI2        DECTTR
057300970613     C*
057400970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGNA DELLA LOCALITA'
057500970613IF  9C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
057600970613     C                   Z-ADD     CPLTTC        DEPTTC                         *TIPO TEMPO
057700970613     C                   EXSR      LOPCPT
057800970613E   9C                   ENDIF
057900970613E   8C                   ENDIF
058000970613     C*
058100970613     C* STAMPA RIGA DI DETTALGIO
058200970613     C                   EXSR      STMDET
058300970613E   7C                   ENDIF
058400970613E   6C                   ENDIF
058500970613E   5C                   ENDIF
058600970613E   4C                   ENDIF
058700970613E   3C                   ENDIF
058800970613     C*
058900970613     C     KEYCPL        READE     AZCPL01L                               98
059000970613E   2C                   ENDDO                                                  *FINE LOCALITA'
059100970613E   1C                   ENDIF
059200970613     C*
059300970613     C                   ENDSR
059400970613     C*------------------------------------------------------------------------*
059500970613     C* CHKLOC - CONTROLLA SE LA LOCCALITA' E' DA STAMPARE
059600970613     C*------------------------------------------------------------------------*
059700970613     C     CHKLOC        BEGSR
059800970613     C*
059900970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
060000971003     C*
060100971003     C* LOCALITA' OBSOLETA E RICHIESTI LE NON OBSOLETE, ESCLUDE
060200971003IF  1C     PAROBS        IFEQ      'N'
060300971003     C     CPLFI1        ANDEQ     'O'
060400971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
060500971003E   1C                   ENDIF
060600970613     C*
060700970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
060800971107IF  1C*---       PARISO    IFNE *BLANKS
060900971107IF  2C*---       CPLISO    IFEQ PARISO
061000971107     C*---       CPCISO    OREQ PARISO
061100971107X   2C*---                 ELSE
061200971107     C*---                 MOVEL'N'       WRECOK           *RECORD NON VALIDO
061300971107E   2C*---                 ENDIF
061400971107E   1C*---                 ENDIF
061500971107     C*
061600971107IF  1C     PARISO        IFNE      *BLANKS
061700971107IF  2C     CPLISO        IFEQ      PARISO
061800971107     C     CPLLIV        OREQ      'C'
061900971107     C     CPCISO        ANDEQ     PARISO
062000971107X   2C                   ELSE
062100971107     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
062200971107E   2C                   ENDIF
062300971107E   1C                   ENDIF
062400970613     C*
062500970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
062600970613IF  1C     PARTTC        IFGT      *ZEROS
062700970613IF  2C     CPLTTC        IFNE      PARTTC
062800970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
062900970613E   2C                   ENDIF
063000970613E   1C                   ENDIF
063100970613     C*
063200970613     C                   ENDSR
063300970613     C*------------------------------------------------------------------------*
063400970613     C* LOPCPT- CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN
063500970613     C*------------------------------------------------------------------------*
063600970613     C     LOPCPT        BEGSR
063700970613     C*
063800970613     C                   Z-ADD     CPCVER        KCTVER                         *VERSIONE
063900970613     C                   Z-ADD     DEPTTC        KCTTTC                         *TIPO TEMPO
064000970613     C     KEYCPT        SETLL     AZCPT01L
064100970613     C     KEYCPT        READE     AZCPT01L                               98
064200970613DO  1C     *IN98         DOWEQ     *OFF
064300970613IF  2C     CPTATB        IFEQ      *BLANKS                                      *VALIDI
064400970613IF  3C     CPTTSP        IFEQ      'D'                                          *DISTRIBUZIONE
064500970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
064600970613     C     PARTIC        OREQ      'T'
064700970613     C                   Z-ADD     CPT0IN        DECD0I                         *DIS INTERNO 00
064800970613     C                   Z-ADD     CPT1IN        DECD1I                         *DIS INTERNO 12
064900970613E   4C                   ENDIF
065000970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
065100970613     C     PARTIC        OREQ      'T'
065200970613     C                   Z-ADD     CPT0CN        DECD0C                         *DIS CLIENTE 00
065300970613     C                   Z-ADD     CPT1CN        DECD1C                         *DIS CLIENTE 12
065400970613E   4C                   ENDIF
065500970613     C                   Z-ADD     CPTTIS        DECDIS                         *DIS ISOLA
065600970613     C                   Z-ADD     CPTTLD        DECDLD                         *DIS L.DISAGIATA
065700970613     C                   Z-ADD     CPTTCS        DECDCS                         *DIS C.STORICO
065800970613E   3C                   ENDIF
065900970613     C*
066000970613IF  3C     CPTTSP        IFEQ      'C'                                          *CORRIERE
066100970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
066200970613     C     PARTIC        OREQ      'T'
066300970613     C                   Z-ADD     CPT0IN        DECC0I                         *COR INTERNO 00
066400970613     C                   Z-ADD     CPT1IN        DECC1I                         *COR INTERNO 12
066500970613E   4C                   ENDIF
066600970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
066700970613     C     PARTIC        OREQ      'T'
066800970613     C                   Z-ADD     CPT0CN        DECC0C                         *COR CLIENTE 00
066900970613     C                   Z-ADD     CPT1CN        DECC1C                         *COR CLIENTE 12
067000970613E   4C                   ENDIF
067100970613     C                   Z-ADD     CPTTIS        DECCIS                         *COR ISOLA
067200970613     C                   Z-ADD     CPTTLD        DECCLD                         *COR L.DISAGIATA
067300970613     C                   Z-ADD     CPTTCS        DECCCS                         *COR C.STORICO
067400970613E   3C                   ENDIF
067500970613     C*
067600970613IF  3C     CPTTSP        IFEQ      'E'                                          *CORRIERE
067700970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
067800970613     C     PARTIC        OREQ      'T'
067900970613     C                   Z-ADD     CPT0IN        DECE0I                         *ESP INTERNO 00
068000970613     C                   Z-ADD     CPT1IN        DECE1I                         *ESP INTERNO 12
068100970613E   4C                   ENDIF
068200970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
068300970613     C     PARTIC        OREQ      'T'
068400970613     C                   Z-ADD     CPT0CN        DECE0C                         *ESP CLIENTE 00
068500970613     C                   Z-ADD     CPT1CN        DECE1C                         *ESP CLIENTE 12
068600970613E   4C                   ENDIF
068700970613     C                   Z-ADD     CPTTIS        DECEIS                         *ESP ISOLA
068800970613     C                   Z-ADD     CPTTLD        DECELD                         *ESP L.DISAGIATA
068900970613     C                   Z-ADD     CPTTCS        DECECS                         *ESP C.STORICO
069000970613E   3C                   ENDIF
069100970613E   2C                   ENDIF
069200970613     C     KEYCPT        READE     AZCPT01L                               98
069300970613E   1C                   ENDDO                                                  *FINE TEMPI
069400970613     C*
069500970613     C                   ENDSR
069600970613     C*------------------------------------------------------------------------*
069700970613     C* STMDET - STAMPA RIGA DI DETTAGLIO
069800970613     C*------------------------------------------------------------------------*
069900970613     C     STMDET        BEGSR
070000970613     C*
070100970613     C* SE FINE PAGINA, STAMPA L'INTESTAZIONE
070200970613IF  1C     *IN01         IFEQ      *ON
070300970613     C                   WRITE     SI05TE                                       *TESTATA
070400970613     C                   WRITE     SI05RI                                       *RIGA
070500970613     C                   WRITE     SI05IN                                       *INTESTAZIONE
070600970613     C                   WRITE     SI05RI                                       *RIGA
070700970613     C                   SETOFF                                       01
070800970613E   1C                   ENDIF
070900000428     C*
071000000428     C* ESEGUO ROUTINE DI REPERIMENTO DATI DA FILE ESTENSIONI CAPPARIO X POSTE
071100000428     C                   EXSR      RTVPPT
071200000428     C* STAMPA DETTAGLIO
071300970613     C                   WRITE     SI05DE                                       *DETTAGLIO
071400000428     C     PARTEM        IFEQ      'S'
071500000428     C                   WRITE     SI05TM                                       *DETTAGLIO X TEMPIAR
071600000428     C                   ENDIF
071700970613     C*
071800970613     C                   ENDSR
071900000428     C*------------------------------------------------------------------------*
072000000428     C* RTVPPT - REPERIMENTO DATI RELATIVI ALLE POSTE
072100000428     C*------------------------------------------------------------------------*
072200000428     C     RTVPPT        BEGSR
072300000428     C*
072400000518     C* OPERO SOLO SE TRATTASI DI "ITALIA"
072500000428     C     INCNAR        IFEQ      *BLANKS
072600000428     C     DECCAP        IFNE      *BLANKS
072700000428     C                   MOVEL     DECCAP        POGCAP
072800000428     C                   ENDIF
072900000428     C                   Z-ADD     PARVER        KPCVER
073000000428     C                   MOVEL     INCNAR        KPCNAR
073100000428     C                   MOVEL     POGCAP        KPCCAP
073200000529     C     STEP1         IFEQ      'CPC'
073300000428     C* PROCEDO CON CAPPARIO
073400000428     C     KEYPPC        CHAIN     AZPPC01L                           55
073500000428     C     *IN55         IFEQ      *OFF
073600000428     C     PPCATB        ANDNE     'A'
073700000518     C                   MOVE      PPCLNA        TSTLNA            3 0
073800000518     C     TSTLNA        IFGT      *ZEROS
073900000428     C                   MOVE      PPCLNA        STLNAP
074000000518     C                   ENDIF
074100000518     C                   MOVE      PPCZNC        TSTZNC            2 0
074200000518     C     TSTZNC        IFGT      *ZEROS
074300000518     C                   MOVE      PPCZNC        STZNCP
074400000518     C                   ENDIF
074500000518     C                   MOVE      PPCZFD        TSTZFD            2 0
074600000518     C     TSTZFD        IFGT      *ZEROS
074700000518     C                   MOVE      PPCZFD        STZFDP
074800000518     C                   ENDIF
074900000518     C                   MOVE      PPCUPI        TSTUPI            5 0
075000000518     C     TSTUPI        IFGT      *ZEROS
075100000428     C                   MOVE      PPCUPI        STUPIP
075200000518     C                   ENDIF
075300000428     C                   ENDIF
075400000529     C                   ENDIF
075500000529     C     STEP1         IFEQ      'CPL'
075600000428     C* PROCEDO CON LOCATARIO
075700000529     C                   MOVEL     SAVLOC        KPLLOC
075800000428     C     KEYPPL        CHAIN     AZPPL01L                           55
075900000428     C     *IN55         IFEQ      *OFF
076000000428     C     PPLATB        ANDNE     'A'
076100000518     C                   MOVE      PPLLNA        TSTLNA            3 0
076200000518     C     TSTLNA        IFGT      *ZEROS
076300000518     C                   MOVE      PPLLNA        STLNAP
076400000518     C                   ENDIF
076500000518     C                   MOVE      PPLZNC        TSTZNC            2 0
076600000518     C     TSTZNC        IFGT      *ZEROS
076700000518     C                   MOVE      PPLZNC        STZNCP
076800000518     C                   ENDIF
076900000518     C                   MOVE      PPLZFD        TSTZFD            2 0
077000000518     C     TSTZFD        IFGT      *ZEROS
077100000518     C                   MOVE      PPLZFD        STZFDP
077200000518     C                   ENDIF
077300000518     C                   MOVE      PPLUPI        TSTUPI            5 0
077400000518     C     TSTUPI        IFGT      *ZEROS
077500000518     C                   MOVE      PPLUPI        STUPIP
077600000518     C                   ENDIF
077700000529     C                   ENDIF
077800000428     C                   ENDIF
077900000529     C     STEP1         IFEQ      'CPE'
078000000529     C* PROCEDO CON ECCEZIONI
078100000529     C                   MOVEL     DECTFP        KPETFP
078200000529     C     KEYPPE        CHAIN     AZPPE01L                           55
078300000529     C     *IN55         IFEQ      *OFF
078400000529     C     PPEATB        ANDNE     'A'
078500000529     C                   MOVE      PPELNA        TSTLNA            3 0
078600000529     C     TSTLNA        IFGT      *ZEROS
078700000529     C                   MOVE      PPELNA        STLNAP
078800000529     C                   ENDIF
078900000529     C                   MOVE      PPEZNC        TSTZNC            2 0
079000000529     C     TSTZNC        IFGT      *ZEROS
079100000529     C                   MOVE      PPEZNC        STZNCP
079200000529     C                   ENDIF
079300000529     C                   ENDIF
079400000529     C                   ENDIF
079500000529     C*
079600000428     C                   ENDIF
079700000428     C*
079800000529     C                   MOVEL     *BLANKS       STEP1
079900000529     C*
080000000428     C                   ENDSR
080100970613     C*------------------------------------------------------------------------*
080200970613     C* STAPAR - STAMPA PAGINA PARAMETRI
080300970613     C*------------------------------------------------------------------------*
080400970613     C     STAPAR        BEGSR
080500970613     C* TESTATA
080600970613     C                   Z-ADD     PARDRI        G08INV                         *DATA RIFERIMENTO
080700970613     C                   Z-ADD     *ZEROS        G08DAT
080800970613     C                   MOVEL     '3'           G08ERR
080900970613     C                   CALL      'XSRDA8'
081000970613     C                   PARM                    WLBDA8
081100970613     C                   Z-ADD     G08DAT        TECDRI
081200970613     C* PARAMETRI
081300970613     C                   MOVEL     PARTOR        PACTOR                         *ORDINAMENTO
081400970613IF  1C     PARTOR        IFEQ      'C'
081500970613     C                   MOVEL     DEC(1)        PADTOR
081600970613E   1C                   ENDIF
081700970613IF  1C     PARTOR        IFEQ      'P'
081800970613     C                   MOVEL     DEC(2)        PADTOR
081900970613E   1C                   ENDIF
082000970613IF  1C     PARTOR        IFEQ      'L'
082100970613     C                   MOVEL     DEC(3)        PADTOR
082200970613E   1C                   ENDIF
082300970613     C*
082400970613IF  1C     PARNA1        IFNE      *BLANKS
082500970613     C                   MOVEL     PARNA1        PACNA1                         *NAZIONE DAL
082600970613IF  2C     PARNA1        IFEQ      'I'
082700970613     C                   MOVEL     DEC(8)        PADNA1                         *NAZIONE ITALIA
082800970613X   2C                   ELSE
082900970613     C                   Z-ADD     1             I
083000970613     C                   SETOFF                                           98
083100970613     C     PARNA1        LOOKUP    NAR(I)                                 98
083200970613IF  3C     *IN98         IFEQ      *ON
083300970613     C                   MOVEL     DNAR(I)       PADNA1
083400970613E   3C                   ENDIF
083500970613E   2C                   ENDIF
083600970613E   1C                   ENDIF
083700970613     C*
083800970613IF  1C     PARNA2        IFNE      *BLANKS
083900970613     C                   MOVEL     PARNA2        PACNA2                         *NAZIONE AL
084000970613IF  2C     PARNA2        IFEQ      'I'
084100970613     C                   MOVEL     DEC(8)        PADNA2                         *NAZIONE ITALIA
084200970613X   2C                   ELSE
084300970613     C                   Z-ADD     1             I
084400970613     C                   SETOFF                                           98
084500970613     C     PARNA2        LOOKUP    NAR(I)                                 98
084600970613IF  3C     *IN98         IFEQ      *ON
084700970613     C                   MOVEL     DNAR(I)       PADNA2
084800970613E   3C                   ENDIF
084900970613E   2C                   ENDIF
085000970613E   1C                   ENDIF
085100970613     C*
085200970613     C                   MOVEL     PARCA1        PACCA1                         *CAP DAL
085300970613     C                   MOVEL     PARCA2        PACCA2                         *CAP AL
085400970613     C*
085500970613IF  1C     PARPR1        IFNE      *BLANKS
085600970613     C                   MOVEL     PARPR1        PACPR1                         *PROVINCIA DAL
085700970613     C                   Z-ADD     1             I
085800970613     C                   SETOFF                                           98
085900970613     C     PARPR1        LOOKUP    PRV(I)                                 98
086000970613IF  2C     *IN98         IFEQ      *ON
086100970613     C                   MOVEL     DPRV(I)       PADPR1
086200970613E   2C                   ENDIF
086300970613E   1C                   ENDIF
086400970613     C*
086500970613IF  1C     PARPR2        IFNE      *BLANKS
086600970613     C                   MOVEL     PARPR1        PACPR2                         *PROVINCIA AL
086700970613     C                   Z-ADD     1             I
086800970613     C                   SETOFF                                           98
086900970613     C     PARPR2        LOOKUP    PRV(I)                                 98
087000970613IF  2C     *IN98         IFEQ      *ON
087100970613     C                   MOVEL     DPRV(I)       PADPR2
087200970613E   2C                   ENDIF
087300970613E   1C                   ENDIF
087400970613     C*
087500970613IF  1C     PARLN1        IFGT      *ZEROS
087600970613     C                   Z-ADD     PARLN1        PACLN1                         *LINEA ARRIVO DAL
087700970613     C                   Z-ADD     1             I
087800970613     C                   SETOFF                                           98
087900970613     C     PARLN1        LOOKUP    ORG(I)                                 98
088000970613IF  2C     *IN98         IFEQ      *ON
088100970613     C                   MOVEL     DORG(I)       PADLN1
088200970613E   2C                   ENDIF
088300970613E   1C                   ENDIF
088400970613     C*
088500970613IF  1C     PARLN2        IFGT      *ZEROS
088600970613     C                   Z-ADD     PARLN2        PACLN2                         *LINEA ARRIVO AL
088700970613     C                   Z-ADD     1             I
088800970613     C                   SETOFF                                           98
088900970613     C     PARLN2        LOOKUP    ORG(I)                                 98
089000970613IF  2C     *IN98         IFEQ      *ON
089100970613     C                   MOVEL     DORG(I)       PADLN2
089200970613E   2C                   ENDIF
089300970613E   1C                   ENDIF
089400970613     C*
089500970613IF  1C     PARISO        IFNE      *BLANKS
089600970613     C                   MOVEL     PARISO        PACISO                         *TIPO INOLTRO
089700970613     C                   Z-ADD     1             I
089800970613     C                   SETOFF                                           98
089900970613     C     PARISO        LOOKUP    INL(I)                                 98
090000970613IF  2C     *IN98         IFEQ      *ON
090100970613     C                   MOVEL     DINL(I)       PADISO
090200970613E   2C                   ENDIF
090300970613E   1C                   ENDIF
090400970613     C*
090500970613     C                   Z-ADD     PARTTC        PACTTC
090600970613     C*
090700970613     C                   MOVEL     PARTEM        PACTEM
090800970613     C*
090900970613     C                   MOVEL     PARTIC        PACTIC                         *TEMPI CONSEGNA
091000970613IF  1C     PARTIC        IFEQ      'I'
091100970613     C                   MOVEL     DEC(4)        PADTIC
091200970613E   1C                   ENDIF
091300970613IF  1C     PARTIC        IFEQ      'C'
091400970613     C                   MOVEL     DEC(5)        PADTIC
091500970613E   1C                   ENDIF
091600970613IF  1C     PARTIC        IFEQ      'T'
091700970613     C                   MOVEL     DEC(6)        PADTIC
091800970613E   1C                   ENDIF
091900970613     C*
092000970613     C                   MOVEL     PARLOC        PACLOC
092100970613IF  1C     PARLOC        IFEQ      'D'
092200970613     C                   MOVEL     DEC(7)        PADLOC
092300970613E   1C                   ENDIF
092400970613     C*
092500970613     C                   MOVEL     PARLIV        PACLIV
092600971003     C*
092700971003     C                   MOVEL     PAROBS        PACOBS
092800970613     C*
092900970613     C                   WRITE     SI05TE                                       *TESTATA
093000970613     C                   WRITE     SI05PA                                       *PARAMETRI
093100970613     C*
093200970613     C                   ENDSR
093300970613     C*------------------------------------------------------------------------*
093400970613     C* CARTAB - CARICA LE TABELLE
093500970613     C*------------------------------------------------------------------------*
093600970613     C     CARTAB        BEGSR
093700970613     C*--------------------
093800970613     C* FILIALI
093900970613     C*--------------------
094000970613     C                   Z-ADD     *ZEROS        I                 5 0
094100970613     C     *LOVAL        SETLL     AZORG01L
094200970613     C                   READ      AZORG01L                               99
094300970613DO  1C     *IN99         DOWEQ     *OFF
094400970613IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
094500970613     C                   ADD       1             I
094600970613     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
094700970613IF  3C     ORGDE5        IFNE      *BLANKS
094800970613     C                   MOVEL     ORGDE5        DORG(I)                        *DESCRIZIONE + CORTA
094900970613X   3C                   ELSE
095000970613     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE + LUNGA
095100970613E   3C                   ENDIF
095200970613E   2C                   ENDIF
095300970613     C                   READ      AZORG01L                               99
095400970613E   1C                   ENDDO
095500970613     C*--------------------
095600970613     C* NAZIONI
095700970613     C*--------------------
095800970613     C                   Z-ADD     *ZEROS        I
095900020906     C                   Z-ADD     1             KTBKUT
096000970613     C                   MOVEL     '15'          KTBCOD
096100970613     C     KE2TAB        SETLL     TABEL00F
096200970613     C     KE2TAB        READE     TABEL00F                               99
096300970613DO  1C     *IN99         DOWEQ     *OFF
096400970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
096500970613     C                   ADD       1             I
096600970613     C                   MOVEL     TBLKEY        NAR(I)                         *CODICE
096700970613     C                   MOVEL     TBLUNI        DS15
096800970613     C                   MOVEL     §15DES        DNAR(I)                        *DESCRIZIONE
096900970613E   2C                   ENDIF
097000970613     C     KE2TAB        READE     TABEL00F                               99
097100970613E   1C                   ENDDO
097200970613     C*--------------------
097300970613     C* PROVINCE
097400970613     C*--------------------
097500970613     C                   Z-ADD     *ZEROS        I
097600020906     C                   Z-ADD     1             KTBKUT
097700970613     C                   MOVEL     'PR'          KTBCOD
097800970613     C     KE2TAB        SETLL     TABEL00F
097900970613     C     KE2TAB        READE     TABEL00F                               99
098000970613DO  1C     *IN99         DOWEQ     *OFF
098100970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
098200970613     C                   ADD       1             I
098300970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
098400970613     C                   MOVEL     TBLUNI        DSPR
098500970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
098600970613E   2C                   ENDIF
098700970613     C     KE2TAB        READE     TABEL00F                               99
098800970613E   1C                   ENDDO
098900970613     C*--------------------
099000970613     C* TIPI INOLTRO
099100970613     C*--------------------
099200970613     C                   Z-ADD     *ZEROS        I
099300020906     C                   Z-ADD     1             KTBKUT
099400970613     C                   MOVEL     '9A'          KTBCOD
099500970613     C     KE2TAB        SETLL     TABEL00F
099600970613     C     KE2TAB        READE     TABEL00F                               99
099700970613DO  1C     *IN99         DOWEQ     *OFF
099800970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
099900970613     C                   ADD       1             I
100000970613     C                   MOVEL     TBLKEY        INL(I)                         *CODICE
100100970613     C                   MOVEL     TBLUNI        DS9A
100200970613     C                   MOVEL     §9ADES        DINL(I)                        *DESCRIZIONE
100300970613E   2C                   ENDIF
100400970613     C     KE2TAB        READE     TABEL00F                               99
100500970613E   1C                   ENDDO
100600970613     C*
100700970613     C                   ENDSR
100800970613     C*------------------------------------------------------------------------*
100900970613     C* *INZSR - OPERAZIONI INIZIALI
101000970613     C*------------------------------------------------------------------------*
101100970613     C     *INZSR        BEGSR
101200970613     C*
101300970613     C* RICEVIMENTO PARAMETRI
101400970613     C     *ENTRY        PLIST
101500970613     C                   PARM                    KPJBA
101600970613     C                   MOVEL     KPJBU         PARAM
101700000509     C* IMPOSTAZIONI INIZIALI
101800000509     C     PARTEM        IFEQ      'S'
101900000509     C                   SETON                                        40
102000000509     C                   ENDIF
102100970613     C*
102200970613     C* VARIABILI RIFERITE AL DATA BASE
102300970613     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
102400970613     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
102500970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
102600970613     C     *LIKE         DEFINE    TBLKEY        KTBKEY
102700970613     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
102800970613     C     *LIKE         DEFINE    CPCNAR        KCCNAR
102900970613     C     *LIKE         DEFINE    CPCCAP        KCCCAP
103000970613     C     *LIKE         DEFINE    CPCPRV        KCCPRV
103100970613     C     *LIKE         DEFINE    CPCLNA        KCCLNA
103200970613     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
103300970613     C     *LIKE         DEFINE    CPENAR        KCENAR
103400970613     C     *LIKE         DEFINE    CPECAP        KCECAP
103500970613     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
103600970613     C     *LIKE         DEFINE    CPTTTC        KCTTTC
103700970613     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
103800970613     C     *LIKE         DEFINE    CPLNAR        KCLNAR
103900970613     C     *LIKE         DEFINE    CPLCAP        KCLCAP
104000970613     C     *LIKE         DEFINE    CPCNAR        DEPNA1                         *DEPOSITI
104100970613     C     *LIKE         DEFINE    CPCNAR        DEPNA2
104200970613     C     *LIKE         DEFINE    CPTTTC        DEPTTC
104300970613     C     *LIKE         DEFINE    CPLLOC        DEPDPR
104400970613     C     *LIKE         DEFINE    CPCNAR        ROTNAR                         *ROTTURE
104500970613     C     *LIKE         DEFINE    CPCPRV        ROTPRV
104600970613     C     *LIKE         DEFINE    CPCLNA        ROTLNA
104700000428     C     *LIKE         DEFINE    PPCVER        KPCVER                         *AZPPC01L
104800000428     C     *LIKE         DEFINE    PPCNAR        KPCNAR
104900000428     C     *LIKE         DEFINE    PPCCAP        KPCCAP
105000000428     C     *LIKE         DEFINE    PPCCAP        POGCAP                         *VARIE
105100000428     C     *LIKE         DEFINE    PPLLOC        KPLLOC                         *AZPPL01L
105200000529     C     *LIKE         DEFINE    PPETFP        KPETFP                         *AZPPE01L
105300970613     C*
105400970613     C* CHIAVI DI LETTURA
105500970613     C* AZORG01L - COMPLETA
105600970613     C     KEYORG        KLIST
105700970613     C                   KFLD                    KORFIL                         *FILIALE
105800970613     C* TABEL00F - PARZIALE
105900970613     C     KE2TAB        KLIST
106000970613     C                   KFLD                    KTBKUT                         *UTENTE
106100970613     C                   KFLD                    KTBCOD                         *TABELLA
106200970613     C* AZCPC01L - COMPLETA
106300970613     C     KEYCPC        KLIST
106400970613     C                   KFLD                    KCCVER                         *VERSIONE
106500970613     C                   KFLD                    KCCNAR                         *NAZIONE
106600970613     C                   KFLD                    KCCCAP                         *CAP
106700000428     C* AZPPC01L - COMPLETA
106800000428     C     KEYPPC        KLIST
106900000428     C                   KFLD                    KPCVER                         *VERSIONE
107000000428     C                   KFLD                    KPCNAR                         *NAZIONE
107100000428     C                   KFLD                    KPCCAP                         *CAP
107200970613     C* AZCPC02L - COMPLETA
107300970613     C     KE2CPC        KLIST
107400970613     C                   KFLD                    KCCVER                         *VERSIONE
107500970613     C                   KFLD                    KCCNAR                         *NAZIONE
107600970613     C                   KFLD                    KCCPRV                         *PROVINCIA
107700970613     C                   KFLD                    KCCCAP                         *CAP
107800970613     C* AZCPC03L - COMPLETA
107900970613     C     KE3CPC        KLIST
108000970613     C                   KFLD                    KCCVER                         *VERSIONE
108100970613     C                   KFLD                    KCCNAR                         *NAZIONE
108200970613     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
108300970613     C                   KFLD                    KCCCAP                         *CAP
108400970613     C* AZCPE01L - PARZIALE
108500970613     C     KEYCPE        KLIST
108600970613     C                   KFLD                    KCEVER                         *VERSIONE
108700970613     C                   KFLD                    KCENAR                         *NAZIONE
108800970613     C                   KFLD                    KCECAP                         *CAP
108900000529     C* AZPPE01L - COMPLETA
109000000529     C     KEYPPE        KLIST
109100000529     C                   KFLD                    KPCVER                         *VERSIONE
109200000529     C                   KFLD                    KPCNAR                         *NAZIONE
109300000529     C                   KFLD                    KPCCAP                         *CAP
109400000529     C                   KFLD                    KPETFP                         *AS400 DI PARTENZA
109500970613     C* AZCPT01L - PARZIALE
109600970613     C     KEYCPT        KLIST
109700970613     C                   KFLD                    KCTVER                         *VERSIONE
109800970613     C                   KFLD                    KCTTTC                         *TIPO TEMPO
109900970613     C* AZCPL01L - PARZIALE
110000970613     C     KEYCPL        KLIST
110100970613     C                   KFLD                    KCLVER                         *VERSIONE
110200970613     C                   KFLD                    KCLNAR                         *NAZIONE
110300970613     C                   KFLD                    KCLCAP                         *CAP
110400000428     C* AZPPL01L - COMPLETA
110500000428     C     KEYPPL        KLIST
110600000428     C                   KFLD                    KPCVER                         *VERSIONE
110700000428     C                   KFLD                    KPCNAR                         *NAZIONE
110800000428     C                   KFLD                    KPCCAP                         *CAP
110900000428     C                   KFLD                    KPLLOC                         *LOCALITA
111000970613     C*
111100970613     C* VARIABILI DI CONTROLLO
111200970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
111300970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
111400970613     C                   MOVEL     'N'           WSTCAP            1            *CAP DA STAMPARE
111500970613     C                   MOVEL     'N'           WSTECC            1            *ECC DA STAMPATE
111600970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
111700970613     C*
111800970613     C* REPERIMENTO DATA E ORA CORRENTE
111900970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
112000970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
112100970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
112200970613     C*
112300970613     C* CARICA LE TABELLE OCCORRENTI
112400970613     C                   EXSR      CARTAB
112500970613     C*
112600970613     C                   ENDSR
112700970613     C*------------------------------------------------------------------------*
112800970613**   DEC - DECODIFICHE
112900970613CAP                                                         1
113000970613PROVINCIA                                                   2
113100970613LINEA DI ARRIVO                                             3
113200970613INTERNI                                                     4
113300970613CLIENTE                                                     5
113400970613TUTTI                                                       6
113500970613LOCALITA' DELIVERY                                          7
113600970613ITALIA                                                      8
1137009706139
11380097061310
