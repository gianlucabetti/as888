000100970613     H*------------------------------------------------------------------------*
000200970613     H* CAPPARIO - STAMPA CAPPARIO - ESEGUE
000300970613     H*------------------------------------------------------------------------*
000400970613     H DECEDIT('0,') DATEDIT(*DMY.)
000500970613     F*------------------------------------------------------------------------*
000600970613     F* DATA BASE
000700970613     F*------------------------------------------------------------------------*
000800970613     FAZCPC01L  IF   E           K DISK
000900970613     FAZCPC02L  IF   E           K DISK
001000970613     F                                     RENAME(AZCPC000:AZCPC2)
001100970613     FAZCPC03L  IF   E           K DISK
001200970613     F                                     RENAME(AZCPC000:AZCPC3)
001300970613     FAZCPT01L  IF   E           K DISK
001400970613     FAZCPL01L  IF   E           K DISK
001500970613     FAZCPE01L  IF   E           K DISK
001600970613     FAZORG01L  IF   E           K DISK
001700970613     FTABEL00F  IF   E           K DISK
001800970613     FTISI05P   O    E             PRINTER OFLIND(*IN01)
001900970613     D*------------------------------------------------------------------------*
002000970613     D* SCHIERE
002100970613     D*------------------------------------------------------------------------*
002200970613     D*------------------
002300970613     D* DECODIFICHE
002400970613     D*------------------
002500970613     D DEC             S             60    DIM(10) CTDATA PERRCD(1)
002600970613     D*-------------------
002700970613     D* FILIALI
002800970613     D*-------------------
002900971118     D ORG             S              3  0 DIM(999)                             *CODICE
003000971118     D DORG            S             20    DIM(999)                             *DESCRIZIONE
003100970613     D*-------------------
003200970613     D* NAZIONI
003300970613     D*-------------------
003400011218     D NAR             S              3    DIM(500)                             *CODICE
003500011218     D DNAR            S             20    DIM(500)                             *DESCRIZIONE
003600970613     D*-------------------
003700970613     D* PROVINCE
003800970613     D*-------------------
003900970613     D PRV             S              2    DIM(300)                             *CODICE
004000970613     D DPRV            S             35    DIM(300)                             *DESCRIZIONE
004100970613     D*-------------------
004200970613     D* TIPI INOLTRO
004300970613     D*-------------------
004400970613     D INL             S              1    DIM(10)                              *CODICE
004500970613     D DINL            S             20    DIM(10)                              *DESCRIZIONE
004600970613     D*------------------------------------------------------------------------*
004700970613     D* INPUT
004800970613     D*------------------------------------------------------------------------*
004900970613     D*------------------
005000970613     D* DS NAZIONI
005100970613     D*------------------
005200970613     D DS15          E DS
005300970613     D*------------------
005400970613     D* DS PROVINCIE
005500970613     D*------------------
005600970613     D DSPR          E DS
005700970613     D*------------------
005800970613     D* DS TIPI INOLTRO
005900970613     D*------------------
006000970613     D DS9A          E DS
006100970613     D*------------------
006200970613     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300970613     D*------------------
006400970613     D WLBDA8          DS                  INZ
006500970613     D  G08DAT                 1      8  0
006600970613     D  G08INV                 9     16  0
006700970613     D  G08ERR                17     17
006800970613     D  G08TGI                18     22  0
006900971118     D*------------------
007000970613     D* DS CAMPO DEDMIX -STAMPA-
007100970613     D*------------------
007200970613     D                 DS
007300970613     D  DECPRV                 1      2
007400971118     D  DEDPRX                 4      4
007500971118     D  DECPRA                 6      7
007600970613     D  DECTFP                 1      3
007700971118     D  DEDTFP                 5     19
007800061016     ***D  DECLOC                 1     19
007900061016     ***D  DEDMIX                 1     19
008000061017     d  DECloc                 1     35
008100061017     d  DEDmix                 1     35
008200970613     D*-------------------
008300970613     D* DS PARAMETRI LANCIO
008400970613     D*-------------------
008500970613     D PARAM           DS
008600970613     D  PARTOR                 1      1
008700970613     D  PARCA1                 2     10
008800970613     D  PARCA2                11     19
008900970613     D  PARNA1                20     22
009000970613     D  PARNA2                23     25
009100970613     D  PARPR1                26     27
009200970613     D  PARPR2                28     29
009300970613     D  PARLN1                30     32  0
009400970613     D  PARLN2                33     35  0
009500970613     D  PARISO                36     36
009600970613     D  PARTTC                37     41  0
009700970613     D  PARTEM                42     42
009800970613     D  PARTIC                43     43
009900970613     D  PARLOC                44     44
010000970613     D  PARLIV                45     45
010100970613     D  PARVER                46     50  0
010200970613     D  PARDRI                51     58  0
010300971003     D  PAROBS                59     59
010400970613     D*-------------------
010500970613     D* ARCHITETTURA
010600970613     D*-------------------
010700970613     D KPJBA         E DS
010800061016      * - Parametri x Controllo profilo utenti
010900061016     d TIBS34ds      e ds
011000061016      * - Ds di riferimento al file esterno AZUTE00F
011100061016     d AZUTEds       e ds                  extname(AZUTE00F)
011200061016      * - Ds per dati organigramma
011300061016     d dDATIUTE      e ds
011400970613     C*------------------------------------------------------------------------*
011500970613     C* MAIN LINE
011600970613     C*------------------------------------------------------------------------*
011700970613     C*
011800970613     C* STAMPA PAGINA PARAMETRI DI LANCIO
011900970613     C                   EXSR      STAPAR
012000970613     C*
012100970613     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012200970613     C                   EXSR      SETFIL
012300970613     C*
012400970613     C* CICLO FINO A FINE FILE
012500970613DO  1C     FINL00        DOWEQ     '0'
012600970613     C*
012700970613     C* CONTROLLA LE ROTTURE PER SALTO PAGINA
012800970613     C                   EXSR      CHKROT
012900970613     C*
013000970613     C* STAMPA RIGA DETTAGLIO -CAP-
013100970613     C                   EXSR      STACAP
013200970613     C*
013300970613     C* CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
013400970613     C                   EXSR      LOPCPL
013500970613     C*
013600970613     C* LEGGE IL RECORD SUCCESSIVO
013700970613     C                   EXSR      LETFIL
013800970613E   1C                   ENDDO                                                  *FINE CAP
013900970613     C*
014000970613     C* STAMPA RIGA DI FINE PROSPETTO
014100970613     C                   WRITE     SI05RI                                       *RIGA SEPARAZIONE
014200970613     C                   WRITE     SI05FI                                       *RIGA FINE STAMPA
014300970613     C*
014400970613     C                   SETON                                        LR
014500970613     C*------------------------------------------------------------------------*
014600970613     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
014700970613     C*------------------------------------------------------------------------*
014800970613     C     SETFIL        BEGSR
014900970613     C*
015000970613     C* IMPOSTA LA NAZIONE
015100970613     C                   MOVEL     PARNA1        DEPNA1
015200970613IF  1C     PARNA1        IFEQ      'I  '
015300970613     C                   MOVEL     *BLANKS       DEPNA1
015400970613E   1C                   ENDIF
015500970613     C*
015600970613     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
015700970613IF  1C     PARTOR        IFEQ      'C'                                          *PER CAP
015800970613     C                   Z-ADD     PARVER        KCCVER
015900970613     C                   MOVEL     DEPNA1        KCCNAR
016000970613     C                   MOVEL     PARCA1        KCCCAP
016100970613     C     KEYCPC        SETLL     AZCPC01L                           99
016200970613E   1C                   ENDIF
016300970613     C*
016400970613IF  1C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
016500970613     C                   Z-ADD     PARVER        KCCVER
016600970613     C                   MOVEL     DEPNA1        KCCNAR
016700970613     C                   MOVEL     PARPR1        KCCPRV
016800970613     C                   MOVEL     PARCA1        KCCCAP
016900970613     C     KE2CPC        SETLL     AZCPC02L                           99
017000970613E   1C                   ENDIF
017100970613     C*
017200970613IF  1C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
017300970613     C                   Z-ADD     PARVER        KCCVER
017400970613     C                   MOVEL     DEPNA1        KCCNAR
017500010730     C*---               Z-ADD     PARLN1        KCCLNA
017600010730     C*---               MOVEL     PARCA1        KCCCAP
017700010730     C                   Z-ADD     *ZEROS        KCCLNA
017800010730     C                   MOVEL     *BLANKS       KCCCAP
017900970613     C     KE3CPC        SETLL     AZCPC03L                           99
018000970613E   1C                   ENDIF
018100970613     C*
018200970613     C* LETTURA RECORD IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
018300970613IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
018400970613     C                   EXSR      LETFIL                                       *LETTURA RECORD
018500970613X   1C                   ELSE                                                   *FINE FILE
018600970613     C                   MOVEL     '1'           FINL00                         *FINE PGM
018700970613E   1C                   ENDIF
018800970613     C*
018900970613     C                   ENDSR
019000970613     C*------------------------------------------------------------------------*
019100970613     C* LETFIL - LETTURA SUCESSIVA RECORD
019200970613     C*------------------------------------------------------------------------*
019300970613     C     LETFIL        BEGSR
019400970613     C*
019500970613     C* LEGGE FINO A:
019600970613     C*      -FINE FILE
019700970613     C*      -TROVATO RECORD VALIDO
019800970613     C                   MOVEL     'N'           WRECOK
019900970613DO  1C     *IN99         DOWEQ     *OFF
020000970613     C     WRECOK        ANDEQ     'N'
020100970613IF  2C     PARTOR        IFEQ      'C'                                          *PER CAP
020200980603     C     KCCVER        READE     AZCPC01L                               99
020300970613E   2C                   ENDIF
020400970613     C*
020500970613IF  2C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
020600980603     C     KCCVER        READE     AZCPC02L                               99
020700970613E   2C                   ENDIF
020800970613     C*
020900970613IF  2C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
021000980603     C     KCCVER        READE     AZCPC03L                               99
021100970613E   2C                   ENDIF
021200970613     C*
021300970613IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
021400970613     C                   MOVEL     '1'           FINL00                         *FINE PROGRAMMA
021500970613X   2C                   ELSE                                                   *RECORD TROVATO
021600970613     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
021700970613E   2C                   ENDIF
021800970613E   1C                   ENDDO
021900970613     C*
022000970613     C                   ENDSR
022100970613     C*------------------------------------------------------------------------*
022200970613     C* CHKREC - CONTROLLA VALIDITA' RECORD
022300970613     C*------------------------------------------------------------------------*
022400970613     C     CHKREC        BEGSR
022500970613     C*
022600970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
022700010730     C                   MOVEL     'N'           $LNLOC            1            *SI LOC.LINEE SELEZ.
022800970613     C*
022900971003     C* CAP ANNULLATO, ESCLUDE
023000970613IF  1C     CPCATB        IFNE      *BLANKS
023100970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023200970613E   1C                   ENDIF
023300970613     C*
023400970613     C* NAZIONE <> DA QUELLE SELEZIONATE, ESCLUDE
023500970613IF  1C     PARNA1        IFNE      *BLANKS
023600970613IF  2C     PARNA1        IFEQ      'I  '
023700970613     C                   MOVEL     *BLANKS       DEPNA1
023800970613     C                   MOVEL     *BLANKS       DEPNA2
023900970613X   2C                   ELSE
024000970613     C                   MOVEL     PARNA1        DEPNA1
024100970613     C                   MOVEL     PARNA2        DEPNA2
024200970613E   2C                   ENDIF
024300970613IF  2C     CPCNAR        IFGE      DEPNA1
024400970613     C     CPCNAR        ANDLE     DEPNA2
024500970613X   2C                   ELSE
024600970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024700970613E   2C                   ENDIF
024800970613E   1C                   ENDIF
024900970613     C*
025000970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
025100970613IF  1C     PARCA1        IFNE      *BLANKS
025200970613IF  2C     CPCCAP        IFGE      PARCA1
025300970613     C     CPCCAP        ANDLE     PARCA2
025400970613X   2C                   ELSE
025500970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025600970613E   2C                   ENDIF
025700970613E   1C                   ENDIF
025800970613     C*
025900970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
026000970613IF  1C     PARPR1        IFNE      *BLANKS
026100970613IF  2C     CPCPRV        IFGE      PARPR1
026200970613     C     CPCPRV        ANDLE     PARPR2
026300970613X   2C                   ELSE
026400970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026500970613E   2C                   ENDIF
026600970613E   1C                   ENDIF
026700971003     C*
026800971003     C* CAP OBSOLETO E RICHIESTI I NON OBSOLETI, ESCLUDE
026900971003IF  1C     PAROBS        IFEQ      'N'
027000971003     C     CPCFI1        ANDEQ     'O'
027100971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027200971003E   1C                   ENDIF
027300010730     C*
027400010730     c* se scelta una linea controlla che sia nei CAP che nelle LOCALITA' ci siano record
027500010730     c* che abbiano la linea STANDARD o la OLTRE o la OLTRE 2 uguale a quella selezionata
027600010730     C                   MOVEL     'N'           $LNLOC                         *LOC.LINEE SELEZ.
027700010730IF  1C     WRECOK        IFEQ      'S'                                          *CAP ANCORA VALIDO
027800010730IF  2C     PARLN1        IFGT      *ZEROS
027900010730     C*
028000010730     C* solo se esistono eccezioni a livello di località
028100010730IF  3C     CPCESL        IFEQ      'S'                                          *LOC.PARTICOLARI
028200010730     C                   EXSR      CLNLOC
028300010730E   3C                   ENDIF
028400010730IF  3C                   IF        (CPCLNA >= PARLN1 AND CPCLNA <= PARLN2) OR
028500010730     C                             (CPCLOL >= PARLN1 AND CPCLOL <= PARLN2) OR
028600010730     C                             (CPCLO2 >= PARLN1 AND CPCLO2 <= PARLN2) OR
028700010730     C                             $LNLOC = 'S'                                 *SI CHK X LINEE LOC
028800010730X   3C                   ELSE
028900010730     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029000010730E   3c                   ENDIF
029100010730E   2C                   ENDIF
029200010730E   1C                   ENDIF
029300970613     C*
029400970613     C                   ENDSR
029500010730     C*--------------------------------------------------------------------------------------------*
029600010730     C* CLNLOC - CONTROLLA CHE LE LOCALITA' DEL CAP ABBIANO LINEE FRA QUELLE SELEZIONATE
029700010730     C*--------------------------------------------------------------------------------------------*
029800010730     C     CLNLOC        BEGSR
029900010730     C*
030000010730     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
030100010730     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
030200010730     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
030300010730     C     KEYCPL        SETLL     AZCPL01L
030400010730     C     KEYCPL        READE     AZCPL01L                               98
030500010730DO  1C     *IN98         DOWEQ     *OFF
030600010730IF  2C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
030700010730     C     CPLLIV        ANDEQ     'L'                                          *LOC. PARTICOLARE
030800010730     C*
030900010730     C* CONTROLLA LE LINEE FRA QUELLE SELEZIONATE
031000010730IF  3C                   IF        (CPLLNA >= PARLN1 AND CPLLNA <= PARLN2) OR
031100010730     C                             (CPLLOL >= PARLN1 AND CPLLOL <= PARLN2) OR
031200010730     C                             (CPLLO2 >= PARLN1 AND CPLLO2 <= PARLN2)
031300010730     C                   MOVEL     'S'           $LNLOC                         *SI LOC.LINEE SELEZ.
031400010730     C                   LEAVE
031500010730E   3C                   ENDIF
031600010730E   2C                   ENDIF
031700010730     C*
031800010730     C     KEYCPL        READE     AZCPL01L                               98
031900010730E   1C                   ENDDO                                                  *FINE LOCALITA' CAP
032000010730     C*
032100010730     C                   ENDSR
032200970613     C*------------------------------------------------------------------------*
032300971003     C* CHKROT - CONTROLLA LE ROTTURE PER SALTO PAGINA
032400970613     C*------------------------------------------------------------------------*
032500970613     C     CHKROT        BEGSR
032600970613     C*
032700970613     C* ROTTURA NAZIONE -SEMPRE-
032800970613IF  1C     CPCNAR        IFNE      ROTNAR
032900970613     C                   MOVEL     CPCNAR        ROTNAR                         *DEPOSITO ROTTURA
033000970613     C                   MOVEL     CPCNAR        INCNAR                         *PER INTESTAZIONE
033100970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
033200970613E   1C                   ENDIF
033300970613     C*
033400970613     C* ROTTURA PROVINCIA -SE ORDINAMENTO PER PROVINCIA-
033500970613IF  1C     PARTOR        IFEQ      'P'
033600970613IF  2C     CPCPRV        IFNE      ROTPRV
033700970613     C                   MOVEL     CPCPRV        ROTPRV                         *DEPOSITO ROTTURA
033800970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
033900970613E   1C                   ENDIF
034000970613E   1C                   ENDIF
034100970613     C*
034200970613     C* ROTTURA LINEA DI ARRIVO -SE ORDINAMENTO PER LINEA DI ARRIVO-
034300970613IF  1C     PARTOR        IFEQ      'L'
034400970613IF  2C     CPCLNA        IFNE      ROTLNA
034500970613     C                   MOVEL     CPCLNA        ROTLNA                         *DEPOSITO ROTTURA
034600970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034700970613E   1C                   ENDIF
034800970613E   1C                   ENDIF
034900970613     C*
035000970613     C                   ENDSR
035100970613     C*------------------------------------------------------------------------*
035200970613     C* STACAP - STAMPA RIGA DI DETTAGLIO -CAP-
035300970613     C*------------------------------------------------------------------------*
035400970613     C     STACAP        BEGSR
035500970613     C*
035600970613     C* REIMPOSTA LE VARIABILI DI LAVORO
035700970613     C                   MOVEL     'N'           WSTCAP                         *CAP NON STAMPATO
035800970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
035900970613     C*
036000970613     C* CONTROLLA SE IL CAP E' DA STAMPARE
036100970613     C                   EXSR      CHKCAP
036200970613IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
036300970613     C* CAMPI
036400970613     C                   MOVEL     CPCCAP        DECCAP
036500970613     C                   MOVEL     CPCFLG        DECFLG
036600970917     C                   MOVEL     CPCFI1        DECFI1
036700970613     C                   MOVEL     *BLANKS       DEDMIX
036800970613     C                   MOVEL     CPCPRV        DECPRV
036900970613     C                   MOVEL     CPCPRA        DECPRA
037000970613IF  2C     CPCPRA        IFNE      *BLANKS
037100970613     C                   MOVEL     '/'           DEDPRX
037200970613E   2C                   ENDIF
037300970613     C                   Z-ADD     CPCLNA        DECLNA
037400970613     C                   MOVEL     CPCZNC        DECZNC
037500970613     C                   MOVEL     CPCZFD        DECZFD
037600970613IF  2C     CPCLKS        IFGT      *ZEROS
037700000518     C     CPCLMS        ORGT      *ZEROS
037800970613     C                   MOVEL     CPCLOS        DECLOS
037900970613     C                   MOVEL     CPCZOS        DECZOS
038000970613     C                   Z-ADD     CPCLKS        DECLKS
038100970613     C                   Z-ADD     CPCLMS        DECLMS
038200970613E   2C                   ENDIF
038300970613IF  2C     CPCLKG        IFGT      *ZEROS
038400970613     C     CPCLMC        ORGT      *ZEROS
038500970613     C                   MOVEL     CPCLOL        DECLOL
038600970613     C                   MOVEL     CPCZOL        DECZOL
038700970613     C                   Z-ADD     CPCLKG        DECLKG
038800970613     C                   Z-ADD     CPCLMC        DECLMC
038900970613E   2C                   ENDIF
039000971218     C*
039100970613     C                   MOVEL     CPCCTS        DECCTS
039200970613     C                   MOVEL     CPCISO        DECINL
039300010727     C                   Z-ADD     CPCTTC        DECTTC
039400010727     C                   MOVEL     *ALL'9'       DECTTR
039500010727     C                   MOVE      CPCFI2        DECTTR
039600970613     C*
039700970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN DEL CAP
039800970613IF  2C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
039900970613     C                   Z-ADD     CPCTTC        DEPTTC                         *TIPO TEMPO
040000970613     C                   EXSR      LOPCPT
040100970613E   2C                   ENDIF
040200970613     C*
040300970613     C* STAMPA RIGA DI DETTALGIO
040400970613     C                   MOVEL     'S'           WSTCAP                         *CAP STAMPATO
040500970613     C                   EXSR      STMDET
040600970613     C*
040700970613     C* CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
040800970613     C                   EXSR      LOPCPE
040900970613E   1C                   ENDIF
041000970613     C*
041100970613     C                   ENDSR
041200970613     C*------------------------------------------------------------------------*
041300970613     C* CHKCAP - CONTROLLA SE IL CAP E' DA STAMPARE
041400970613     C*------------------------------------------------------------------------*
041500970613     C     CHKCAP        BEGSR
041600970613     C*
041700970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
041800970613     C*
041900970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
042000970613IF  1C     PARISO        IFNE      *BLANKS
042100970613IF  2C     CPCISO        IFNE      PARISO
042200970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042300970613E   2C                   ENDIF
042400970613E   1C                   ENDIF
042500970613     C*
042600970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
042700970613IF  1C     PARTTC        IFGT      *ZEROS
042800970613IF  2C     CPCTTC        IFNE      PARTTC
042900970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043000970613E   2C                   ENDIF
043100970613E   1C                   ENDIF
043200970613     C*
043300970613     C                   ENDSR
043400970613     C*------------------------------------------------------------------------*
043500970613     C* LOPCPE - CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
043600970613     C*------------------------------------------------------------------------*
043700970613     C     LOPCPE        BEGSR
043800970613     C*
043900970613     C* REIMPOSTA LE VARIABILI DI LAVOR
044000970613     C                   MOVEL     'N'           WSTECC                         *ECC NON STAMPATO
044100970613     C*
044200970613IF  1C     CPCESE        IFEQ      'S'                                          *ESISTONO ECCEZIONI
044300970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
044400970613     C                   Z-ADD     CPCVER        KCEVER                         *VERSIONE
044500970613     C                   MOVEL     CPCNAR        KCENAR                         *NAZIONE
044600970613     C                   MOVEL     CPCCAP        KCECAP                         *CAP
044700970613     C     KEYCPE        SETLL     AZCPE01L
044800970613     C     KEYCPE        READE     AZCPE01L                               98
044900970613DO  2C     *IN98         DOWEQ     *OFF
045000970613IF  3C     CPEATB        IFEQ      *BLANKS                                      *VALIDI
045100970613     C* CAMPI
045200970613     C                   MOVEL     *BLANKS       DEDMIX
045300970613     C                   MOVEL     CPETFP        DECTFP
045400970613     C                   Z-ADD     CPELNA        DECLNA
045500970613     C                   MOVEL     CPEZNC        DECZNC
045600970613IF  4C     CPCLKS        IFGT      *ZEROS
045700970613     C     CPCLMS        ORGT      *ZEROS
045800970613     C                   MOVEL     CPELOS        DECLOS
045900970613     C                   MOVEL     CPEZOS        DECZOS
046000970613E   4C                   ENDIF
046100970613IF  4C     CPCLKG        IFGT      *ZEROS
046200970613     C     CPCLMC        ORGT      *ZEROS
046300970613     C                   MOVEL     CPELOL        DECLOL
046400970613     C                   MOVEL     CPEZOL        DECZOL
046500970613E   4C                   ENDIF
046600970613     C* DECODIFICHE
046700970613     C                   Z-ADD     1             I
046800970613     C                   SETOFF                                           98
046900970613     C     CPETFP        LOOKUP    ORG(I)                                 98
047000970613IF  4C     *IN98         IFEQ      *ON
047100970613     C                   MOVEL     DORG(I)       DEDTFP                         *TERMINAL PARTENZA
047200970613E   4C                   ENDIF
047300970613     C*
047400970613     C* STAMPA RIGA DI DETTALGIO
047500970613     C                   MOVEL     'S'           WSTECC                         *ECCEZIONI STAMPATE
047600970613     C                   EXSR      STMDET
047700970613E   3C                   ENDIF
047800970613     C     KEYCPE        READE     AZCPE01L                               98
047900970613E   2C                   ENDDO                                                  *FINE ECCEZIONI
048000970613E   1C                   ENDIF
048100970613     C*
048200970613     C                   ENDSR
048300970613     C*------------------------------------------------------------------------*
048400970613     C* LOPCPL - CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
048500970613     C*------------------------------------------------------------------------*
048600970613     C     LOPCPL        BEGSR
048700970613     C*
048800970613     C* DEPOSITA LA DESCRIZIONE DELLA PROVINCIA DEL CAP
048900970613     C                   Z-ADD     1             I
049000970613     C                   SETOFF                                           98
049100970613     C     CPCPRV        LOOKUP    PRV(I)                                 98
049200970613IF  1C     *IN98         IFEQ      *ON
049300970613     C                   MOVEL     DPRV(I)       DEPDPR
049400970613X   1C                   ELSE
049500970613     C                   MOVEL     *BLANKS       DEPDPR
049600970613E   1C                   ENDIF
049700970613     C*
049800970613     C* SE RICHIESTI IN STAMPA, LEGGE LE LOCALITA'
049900010730IF  1c     PARLOC        IFNE      'N'
050000970613     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
050100970613     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
050200970613     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
050300970613     C     KEYCPL        SETLL     AZCPL01L
050400970613     C     KEYCPL        READE     AZCPL01L                               98
050500970613DO  2C     *IN98         DOWEQ     *OFF
050600970613IF  3C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
050700970613IF  4C     PARLOC        IFEQ      'D'                                          *SELEZ.LOC.DELIVERY
050800970613     C     CPLDLV        ANDEQ     'S'
050900970613     C     PARLOC        ORNE      'D'                                          *SELEZ.TUTTE
051000970613IF  5C     PARLIV        IFEQ      'S'                                          *SELEZ.LOC <> CAP
051100970613     C     CPLLIV        ANDEQ     'L'
051200970613     C     PARLIV        OREQ      'N'                                          *SELEZ.TUTTE
051300970613     C*
051400990525     C* NON STAMPA LA LOCALITA' SE E' UGUALE ALLA DESCRIZIONE DELLA PROVINCIA E
051500990525     C* SE E' GESTITA A LIVELLO DI LOCALITA'
051600970613IF  6C     DEPDPR        IFNE      CPLLOC                                       *-PEZZA-
051700990525     C     DEPDPR        OREQ      CPLLOC
051800990525     C     CPLLIV        ANDEQ     'L'
051900970613     C*
052000970613     C* CONTROLLA SE LA LOCALITA' E' DA STAMPARE
052100970613     C                   EXSR      CHKLOC
052200970613IF  7C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
052300970613     C* CAMPI
052400970613IF  8C     WSTECC        IFEQ      'S'                                          *ECCEZIONI STAMPATE
052500970613     C                   WRITE     SI05R2                                       *STAMPA RIGA SEPARAZ
052600970613     C                   MOVEL     'N'           WSTECC                         *X NON STAMPARE +RIG
052700970613E   8C                   ENDIF
052800970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
052900970613IF  8C     WSTCAP        IFEQ      'N'                                          *CAP NON STMPATO
053000970613     C                   MOVEL     CPCCAP        DECCAP                         *RIEMPE CAMPI CAP
053100970613E   8C                   ENDIF
053200970917     C                   MOVEL     CPLFI1        DECFI1
053300970613     C                   MOVEL     *BLANKS       DEDMIX
053400970613     C                   MOVEL     CPLLOC        DECLOC
053500000529     C                   MOVEL     CPLLOC        SAVLOC           35
053600970613IF  8C     CPLLIV        IFEQ      'L'                                          *LOC.GEST.LIV.LOC.
053700970613     C                   Z-ADD     CPLLNA        DECLNA
053800970613     C                   MOVEL     CPLZNC        DECZNC
053900970613     C                   MOVEL     CPLZFD        DECZFD
054000970621IF  9C     CPLLKS        IFGT      *ZEROS
054100970621     C     CPLLMS        ORGT      *ZEROS
054200970613     C                   MOVEL     CPLLOS        DECLOS
054300970613     C                   MOVEL     CPLZOS        DECZOS
054400970613     C                   Z-ADD     CPLLKS        DECLKS
054500970613     C                   Z-ADD     CPLLMS        DECLMS
054600970613E   9C                   ENDIF
054700970621IF  9C     CPLLKG        IFGT      *ZEROS
054800970621     C     CPLLMC        ORGT      *ZEROS
054900970613     C                   MOVEL     CPLLOL        DECLOL
055000970613     C                   MOVEL     CPLZOL        DECZOL
055100970613     C                   Z-ADD     CPLLKG        DECLKG
055200970613     C                   Z-ADD     CPLLMC        DECLMC
055300970613E   9C                   ENDIF
055400000518     C*
055500970613     C                   MOVEL     CPLCTS        DECCTS
055600970613     C                   MOVEL     CPLISO        DECINL
055700010727     C                   Z-ADD     CPLTTC        DECTTC
055800010727     C                   MOVEL     *ALL'9'       DECTTR
055900010727     C                   MOVE      CPLFI2        DECTTR
056000970613     C*
056100970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGNA DELLA LOCALITA'
056200970613IF  9C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
056300970613     C                   Z-ADD     CPLTTC        DEPTTC                         *TIPO TEMPO
056400970613     C                   EXSR      LOPCPT
056500970613E   9C                   ENDIF
056600970613E   8C                   ENDIF
056700970613     C*
056800970613     C* STAMPA RIGA DI DETTALGIO
056900970613     C                   EXSR      STMDET
057000970613E   7C                   ENDIF
057100970613E   6C                   ENDIF
057200970613E   5C                   ENDIF
057300970613E   4C                   ENDIF
057400970613E   3C                   ENDIF
057500970613     C*
057600970613     C     KEYCPL        READE     AZCPL01L                               98
057700970613E   2C                   ENDDO                                                  *FINE LOCALITA'
057800970613E   1C                   ENDIF
057900970613     C*
058000970613     C                   ENDSR
058100970613     C*------------------------------------------------------------------------*
058200970613     C* CHKLOC - CONTROLLA SE LA LOCCALITA' E' DA STAMPARE
058300970613     C*------------------------------------------------------------------------*
058400970613     C     CHKLOC        BEGSR
058500970613     C*
058600970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
058700971003     C*
058800971003     C* LOCALITA' OBSOLETA E RICHIESTI LE NON OBSOLETE, ESCLUDE
058900971003IF  1C     PAROBS        IFEQ      'N'
059000971003     C     CPLFI1        ANDEQ     'O'
059100971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
059200971003E   1C                   ENDIF
059300970613     C*
059400970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
059500971107IF  1C*---       PARISO    IFNE *BLANKS
059600971107IF  2C*---       CPLISO    IFEQ PARISO
059700971107     C*---       CPCISO    OREQ PARISO
059800971107X   2C*---                 ELSE
059900971107     C*---                 MOVEL'N'       WRECOK           *RECORD NON VALIDO
060000971107E   2C*---                 ENDIF
060100971107E   1C*---                 ENDIF
060200971107     C*
060300971107IF  1C     PARISO        IFNE      *BLANKS
060400971107IF  2C     CPLISO        IFEQ      PARISO
060500971107     C     CPLLIV        OREQ      'C'
060600971107     C     CPCISO        ANDEQ     PARISO
060700971107X   2C                   ELSE
060800971107     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
060900971107E   2C                   ENDIF
061000971107E   1C                   ENDIF
061100970613     C*
061200970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
061300970613IF  1C     PARTTC        IFGT      *ZEROS
061400970613IF  2C     CPLTTC        IFNE      PARTTC
061500970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061600970613E   2C                   ENDIF
061700970613E   1C                   ENDIF
061800970613     C*
061900970613     C                   ENDSR
062000970613     C*------------------------------------------------------------------------*
062100970613     C* LOPCPT- CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN
062200970613     C*------------------------------------------------------------------------*
062300970613     C     LOPCPT        BEGSR
062400970613     C*
062500970613     C                   Z-ADD     CPCVER        KCTVER                         *VERSIONE
062600970613     C                   Z-ADD     DEPTTC        KCTTTC                         *TIPO TEMPO
062700970613     C     KEYCPT        SETLL     AZCPT01L
062800970613     C     KEYCPT        READE     AZCPT01L                               98
062900970613DO  1C     *IN98         DOWEQ     *OFF
063000970613IF  2C     CPTATB        IFEQ      *BLANKS                                      *VALIDI
063100970613IF  3C     CPTTSP        IFEQ      'D'                                          *DISTRIBUZIONE
063200970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
063300970613     C     PARTIC        OREQ      'T'
063400970613     C                   Z-ADD     CPT0IN        DECD0I                         *DIS INTERNO 00
063500970613     C                   Z-ADD     CPT1IN        DECD1I                         *DIS INTERNO 12
063600970613E   4C                   ENDIF
063700970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
063800970613     C     PARTIC        OREQ      'T'
063900970613     C                   Z-ADD     CPT0CN        DECD0C                         *DIS CLIENTE 00
064000970613     C                   Z-ADD     CPT1CN        DECD1C                         *DIS CLIENTE 12
064100970613E   4C                   ENDIF
064200970613     C                   Z-ADD     CPTTIS        DECDIS                         *DIS ISOLA
064300970613     C                   Z-ADD     CPTTLD        DECDLD                         *DIS L.DISAGIATA
064400970613     C                   Z-ADD     CPTTCS        DECDCS                         *DIS C.STORICO
064500970613E   3C                   ENDIF
064600970613     C*
064700970613IF  3C     CPTTSP        IFEQ      'C'                                          *CORRIERE
064800970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
064900970613     C     PARTIC        OREQ      'T'
065000970613     C                   Z-ADD     CPT0IN        DECC0I                         *COR INTERNO 00
065100970613     C                   Z-ADD     CPT1IN        DECC1I                         *COR INTERNO 12
065200970613E   4C                   ENDIF
065300970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
065400970613     C     PARTIC        OREQ      'T'
065500970613     C                   Z-ADD     CPT0CN        DECC0C                         *COR CLIENTE 00
065600970613     C                   Z-ADD     CPT1CN        DECC1C                         *COR CLIENTE 12
065700970613E   4C                   ENDIF
065800970613     C                   Z-ADD     CPTTIS        DECCIS                         *COR ISOLA
065900970613     C                   Z-ADD     CPTTLD        DECCLD                         *COR L.DISAGIATA
066000970613     C                   Z-ADD     CPTTCS        DECCCS                         *COR C.STORICO
066100970613E   3C                   ENDIF
066200970613     C*
066300970613IF  3C     CPTTSP        IFEQ      'E'                                          *CORRIERE
066400970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
066500970613     C     PARTIC        OREQ      'T'
066600970613     C                   Z-ADD     CPT0IN        DECE0I                         *ESP INTERNO 00
066700970613     C                   Z-ADD     CPT1IN        DECE1I                         *ESP INTERNO 12
066800970613E   4C                   ENDIF
066900970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
067000970613     C     PARTIC        OREQ      'T'
067100970613     C                   Z-ADD     CPT0CN        DECE0C                         *ESP CLIENTE 00
067200970613     C                   Z-ADD     CPT1CN        DECE1C                         *ESP CLIENTE 12
067300970613E   4C                   ENDIF
067400970613     C                   Z-ADD     CPTTIS        DECEIS                         *ESP ISOLA
067500970613     C                   Z-ADD     CPTTLD        DECELD                         *ESP L.DISAGIATA
067600970613     C                   Z-ADD     CPTTCS        DECECS                         *ESP C.STORICO
067700970613E   3C                   ENDIF
067800970613E   2C                   ENDIF
067900970613     C     KEYCPT        READE     AZCPT01L                               98
068000970613E   1C                   ENDDO                                                  *FINE TEMPI
068100970613     C*
068200970613     C                   ENDSR
068300970613     C*------------------------------------------------------------------------*
068400970613     C* STMDET - STAMPA RIGA DI DETTAGLIO
068500970613     C*------------------------------------------------------------------------*
068600970613     C     STMDET        BEGSR
068700970613     C*
068800970613     C* SE FINE PAGINA, STAMPA L'INTESTAZIONE
068900970613IF  1C     *IN01         IFEQ      *ON
069000970613     C                   WRITE     SI05TE                                       *TESTATA
069100970613     C                   WRITE     SI05RI                                       *RIGA
069200970613     C                   WRITE     SI05IN                                       *INTESTAZIONE
069300970613     C                   WRITE     SI05RI                                       *RIGA
069400970613     C                   SETOFF                                       01
069500970613E   1C                   ENDIF
069600000428     C*
069700000428     C* STAMPA DETTAGLIO
069800970613     C                   WRITE     SI05DE                                       *DETTAGLIO
069900000428     C     PARTEM        IFEQ      'S'
070000000428     C                   WRITE     SI05TM                                       *DETTAGLIO X TEMPIAR
070100000428     C                   ENDIF
070200970613     C*
070300970613     C                   ENDSR
070400970613     C*------------------------------------------------------------------------*
070500970613     C* STAPAR - STAMPA PAGINA PARAMETRI
070600970613     C*------------------------------------------------------------------------*
070700970613     C     STAPAR        BEGSR
070800970613     C* TESTATA
070900970613     C                   Z-ADD     PARDRI        G08INV                         *DATA RIFERIMENTO
071000970613     C                   Z-ADD     *ZEROS        G08DAT
071100970613     C                   MOVEL     '3'           G08ERR
071200970613     C                   CALL      'XSRDA8'
071300970613     C                   PARM                    WLBDA8
071400970613     C                   Z-ADD     G08DAT        TECDRI
071500970613     C* PARAMETRI
071600970613     C                   MOVEL     PARTOR        PACTOR                         *ORDINAMENTO
071700970613IF  1C     PARTOR        IFEQ      'C'
071800970613     C                   MOVEL     DEC(1)        PADTOR
071900970613E   1C                   ENDIF
072000970613IF  1C     PARTOR        IFEQ      'P'
072100970613     C                   MOVEL     DEC(2)        PADTOR
072200970613E   1C                   ENDIF
072300970613IF  1C     PARTOR        IFEQ      'L'
072400970613     C                   MOVEL     DEC(3)        PADTOR
072500970613E   1C                   ENDIF
072600970613     C*
072700970613IF  1C     PARNA1        IFNE      *BLANKS
072800970613     C                   MOVEL     PARNA1        PACNA1                         *NAZIONE DAL
072900970613IF  2C     PARNA1        IFEQ      'I'
073000970613     C                   MOVEL     DEC(8)        PADNA1                         *NAZIONE ITALIA
073100970613X   2C                   ELSE
073200970613     C                   Z-ADD     1             I
073300970613     C                   SETOFF                                           98
073400970613     C     PARNA1        LOOKUP    NAR(I)                                 98
073500970613IF  3C     *IN98         IFEQ      *ON
073600970613     C                   MOVEL     DNAR(I)       PADNA1
073700970613E   3C                   ENDIF
073800970613E   2C                   ENDIF
073900970613E   1C                   ENDIF
074000970613     C*
074100970613IF  1C     PARNA2        IFNE      *BLANKS
074200970613     C                   MOVEL     PARNA2        PACNA2                         *NAZIONE AL
074300970613IF  2C     PARNA2        IFEQ      'I'
074400970613     C                   MOVEL     DEC(8)        PADNA2                         *NAZIONE ITALIA
074500970613X   2C                   ELSE
074600970613     C                   Z-ADD     1             I
074700970613     C                   SETOFF                                           98
074800970613     C     PARNA2        LOOKUP    NAR(I)                                 98
074900970613IF  3C     *IN98         IFEQ      *ON
075000970613     C                   MOVEL     DNAR(I)       PADNA2
075100970613E   3C                   ENDIF
075200970613E   2C                   ENDIF
075300970613E   1C                   ENDIF
075400970613     C*
075500970613     C                   MOVEL     PARCA1        PACCA1                         *CAP DAL
075600970613     C                   MOVEL     PARCA2        PACCA2                         *CAP AL
075700970613     C*
075800970613IF  1C     PARPR1        IFNE      *BLANKS
075900970613     C                   MOVEL     PARPR1        PACPR1                         *PROVINCIA DAL
076000970613     C                   Z-ADD     1             I
076100970613     C                   SETOFF                                           98
076200970613     C     PARPR1        LOOKUP    PRV(I)                                 98
076300970613IF  2C     *IN98         IFEQ      *ON
076400970613     C                   MOVEL     DPRV(I)       PADPR1
076500970613E   2C                   ENDIF
076600970613E   1C                   ENDIF
076700970613     C*
076800970613IF  1C     PARPR2        IFNE      *BLANKS
076900970613     C                   MOVEL     PARPR1        PACPR2                         *PROVINCIA AL
077000970613     C                   Z-ADD     1             I
077100970613     C                   SETOFF                                           98
077200970613     C     PARPR2        LOOKUP    PRV(I)                                 98
077300970613IF  2C     *IN98         IFEQ      *ON
077400970613     C                   MOVEL     DPRV(I)       PADPR2
077500970613E   2C                   ENDIF
077600970613E   1C                   ENDIF
077700970613     C*
077800970613IF  1C     PARLN1        IFGT      *ZEROS
077900970613     C                   Z-ADD     PARLN1        PACLN1                         *LINEA ARRIVO DAL
078000970613     C                   Z-ADD     1             I
078100970613     C                   SETOFF                                           98
078200970613     C     PARLN1        LOOKUP    ORG(I)                                 98
078300970613IF  2C     *IN98         IFEQ      *ON
078400970613     C                   MOVEL     DORG(I)       PADLN1
078500970613E   2C                   ENDIF
078600970613E   1C                   ENDIF
078700970613     C*
078800970613IF  1C     PARLN2        IFGT      *ZEROS
078900970613     C                   Z-ADD     PARLN2        PACLN2                         *LINEA ARRIVO AL
079000970613     C                   Z-ADD     1             I
079100970613     C                   SETOFF                                           98
079200970613     C     PARLN2        LOOKUP    ORG(I)                                 98
079300970613IF  2C     *IN98         IFEQ      *ON
079400970613     C                   MOVEL     DORG(I)       PADLN2
079500970613E   2C                   ENDIF
079600970613E   1C                   ENDIF
079700970613     C*
079800970613IF  1C     PARISO        IFNE      *BLANKS
079900970613     C                   MOVEL     PARISO        PACISO                         *TIPO INOLTRO
080000970613     C                   Z-ADD     1             I
080100970613     C                   SETOFF                                           98
080200970613     C     PARISO        LOOKUP    INL(I)                                 98
080300970613IF  2C     *IN98         IFEQ      *ON
080400970613     C                   MOVEL     DINL(I)       PADISO
080500970613E   2C                   ENDIF
080600970613E   1C                   ENDIF
080700970613     C*
080800970613     C                   Z-ADD     PARTTC        PACTTC
080900970613     C*
081000970613     C                   MOVEL     PARTEM        PACTEM
081100970613     C*
081200970613     C                   MOVEL     PARTIC        PACTIC                         *TEMPI CONSEGNA
081300970613IF  1C     PARTIC        IFEQ      'I'
081400970613     C                   MOVEL     DEC(4)        PADTIC
081500970613E   1C                   ENDIF
081600970613IF  1C     PARTIC        IFEQ      'C'
081700970613     C                   MOVEL     DEC(5)        PADTIC
081800970613E   1C                   ENDIF
081900970613IF  1C     PARTIC        IFEQ      'T'
082000970613     C                   MOVEL     DEC(6)        PADTIC
082100970613E   1C                   ENDIF
082200970613     C*
082300970613     C                   MOVEL     PARLOC        PACLOC
082400970613IF  1C     PARLOC        IFEQ      'D'
082500970613     C                   MOVEL     DEC(7)        PADLOC
082600970613E   1C                   ENDIF
082700970613     C*
082800970613     C                   MOVEL     PARLIV        PACLIV
082900971003     C*
083000971003     C                   MOVEL     PAROBS        PACOBS
083100970613     C*
083200970613     C                   WRITE     SI05TE                                       *TESTATA
083300970613     C                   WRITE     SI05PA                                       *PARAMETRI
083400970613     C*
083500970613     C                   ENDSR
083600970613     C*------------------------------------------------------------------------*
083700970613     C* CARTAB - CARICA LE TABELLE
083800970613     C*------------------------------------------------------------------------*
083900970613     C     CARTAB        BEGSR
084000970613     C*--------------------
084100970613     C* FILIALI
084200970613     C*--------------------
084300970613     C                   Z-ADD     *ZEROS        I                 5 0
084400970613     C     *LOVAL        SETLL     AZORG01L
084500970613     C                   READ      AZORG01L                               99
084600970613DO  1C     *IN99         DOWEQ     *OFF
084700970613IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
084800970613     C                   ADD       1             I
084900970613     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
085000970613IF  3C     ORGDE5        IFNE      *BLANKS
085100970613     C                   MOVEL     ORGDE5        DORG(I)                        *DESCRIZIONE + CORTA
085200970613X   3C                   ELSE
085300970613     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE + LUNGA
085400970613E   3C                   ENDIF
085500970613E   2C                   ENDIF
085600970613     C                   READ      AZORG01L                               99
085700970613E   1C                   ENDDO
085800970613     C*--------------------
085900970613     C* NAZIONI
086000970613     C*--------------------
086100970613     C                   Z-ADD     *ZEROS        I
086200020906     C                   Z-ADD     1             KTBKUT
086300970613     C                   MOVEL     '15'          KTBCOD
086400970613     C     KE2TAB        SETLL     TABEL00F
086500970613     C     KE2TAB        READE     TABEL00F                               99
086600970613DO  1C     *IN99         DOWEQ     *OFF
086700970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
086800970613     C                   ADD       1             I
086900970613     C                   MOVEL     TBLKEY        NAR(I)                         *CODICE
087000970613     C                   MOVEL     TBLUNI        DS15
087100970613     C                   MOVEL     §15DES        DNAR(I)                        *DESCRIZIONE
087200970613E   2C                   ENDIF
087300970613     C     KE2TAB        READE     TABEL00F                               99
087400970613E   1C                   ENDDO
087500970613     C*--------------------
087600970613     C* PROVINCE
087700970613     C*--------------------
087800970613     C                   Z-ADD     *ZEROS        I
087900020906     C                   Z-ADD     1             KTBKUT
088000970613     C                   MOVEL     'PR'          KTBCOD
088100970613     C     KE2TAB        SETLL     TABEL00F
088200970613     C     KE2TAB        READE     TABEL00F                               99
088300970613DO  1C     *IN99         DOWEQ     *OFF
088400970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
088500970613     C                   ADD       1             I
088600970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
088700970613     C                   MOVEL     TBLUNI        DSPR
088800970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
088900970613E   2C                   ENDIF
089000970613     C     KE2TAB        READE     TABEL00F                               99
089100970613E   1C                   ENDDO
089200970613     C*--------------------
089300970613     C* TIPI INOLTRO
089400970613     C*--------------------
089500970613     C                   Z-ADD     *ZEROS        I
089600020906     C                   Z-ADD     1             KTBKUT
089700970613     C                   MOVEL     '9A'          KTBCOD
089800970613     C     KE2TAB        SETLL     TABEL00F
089900970613     C     KE2TAB        READE     TABEL00F                               99
090000970613DO  1C     *IN99         DOWEQ     *OFF
090100970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
090200970613     C                   ADD       1             I
090300970613     C                   MOVEL     TBLKEY        INL(I)                         *CODICE
090400970613     C                   MOVEL     TBLUNI        DS9A
090500970613     C                   MOVEL     §9ADES        DINL(I)                        *DESCRIZIONE
090600970613E   2C                   ENDIF
090700970613     C     KE2TAB        READE     TABEL00F                               99
090800970613E   1C                   ENDDO
090900970613     C*
091000970613     C                   ENDSR
091100970613     C*------------------------------------------------------------------------*
091200970613     C* *INZSR - OPERAZIONI INIZIALI
091300970613     C*------------------------------------------------------------------------*
091400970613     C     *INZSR        BEGSR
091500970613     C*
091600970613     C* RICEVIMENTO PARAMETRI
091700970613     C     *ENTRY        PLIST
091800970613     C                   PARM                    KPJBA
091900970613     C                   MOVEL     KPJBU         PARAM
092000061016      *
092100061016      * Reperimento dati aziendali
092200061016     c     *dtaara       define    §azute        AZUTEds
092300061016     c     *dtaara       define    §datiute      dDATIUTE
092400061016     c                   in(E)     *dtaara
092500061016     c                   if        %ERROR or RSUT = *blanks
092600061016     c                   clear                   Tibs34Ds
092700061016     c                   call      'TIBS34R'
092800061016     c                   parm                    Tibs34Ds
092900061016     c                   in        *dtaara
093000061016     c                   endif
093100061016      *
093200000509     C* IMPOSTAZIONI INIZIALI
093300000509     C     PARTEM        IFEQ      'S'
093400000509     C                   SETON                                        40
093500000509     C                   ENDIF
093600970613     C*
093700970613     C* VARIABILI RIFERITE AL DATA BASE
093800970613     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
093900970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
094000970613     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
094100970613     C     *LIKE         DEFINE    CPCNAR        KCCNAR
094200970613     C     *LIKE         DEFINE    CPCCAP        KCCCAP
094300970613     C     *LIKE         DEFINE    CPCPRV        KCCPRV
094400970613     C     *LIKE         DEFINE    CPCLNA        KCCLNA
094500970613     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
094600970613     C     *LIKE         DEFINE    CPENAR        KCENAR
094700970613     C     *LIKE         DEFINE    CPECAP        KCECAP
094800970613     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
094900970613     C     *LIKE         DEFINE    CPTTTC        KCTTTC
095000970613     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
095100970613     C     *LIKE         DEFINE    CPLNAR        KCLNAR
095200970613     C     *LIKE         DEFINE    CPLCAP        KCLCAP
095300970613     C     *LIKE         DEFINE    CPCNAR        DEPNA1                         *DEPOSITI
095400970613     C     *LIKE         DEFINE    CPCNAR        DEPNA2
095500970613     C     *LIKE         DEFINE    CPTTTC        DEPTTC
095600970613     C     *LIKE         DEFINE    CPLLOC        DEPDPR
095700970613     C     *LIKE         DEFINE    CPCNAR        ROTNAR                         *ROTTURE
095800970613     C     *LIKE         DEFINE    CPCPRV        ROTPRV
095900970613     C     *LIKE         DEFINE    CPCLNA        ROTLNA
096000970613     C*
096100970613     C* CHIAVI DI LETTURA
096200970613     C* TABEL00F - PARZIALE
096300970613     C     KE2TAB        KLIST
096400970613     C                   KFLD                    KTBKUT                         *UTENTE
096500970613     C                   KFLD                    KTBCOD                         *TABELLA
096600970613     C* AZCPC01L - COMPLETA
096700970613     C     KEYCPC        KLIST
096800970613     C                   KFLD                    KCCVER                         *VERSIONE
096900970613     C                   KFLD                    KCCNAR                         *NAZIONE
097000970613     C                   KFLD                    KCCCAP                         *CAP
097100970613     C* AZCPC02L - COMPLETA
097200970613     C     KE2CPC        KLIST
097300970613     C                   KFLD                    KCCVER                         *VERSIONE
097400970613     C                   KFLD                    KCCNAR                         *NAZIONE
097500970613     C                   KFLD                    KCCPRV                         *PROVINCIA
097600970613     C                   KFLD                    KCCCAP                         *CAP
097700970613     C* AZCPC03L - COMPLETA
097800970613     C     KE3CPC        KLIST
097900970613     C                   KFLD                    KCCVER                         *VERSIONE
098000970613     C                   KFLD                    KCCNAR                         *NAZIONE
098100970613     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
098200970613     C                   KFLD                    KCCCAP                         *CAP
098300970613     C* AZCPE01L - PARZIALE
098400970613     C     KEYCPE        KLIST
098500970613     C                   KFLD                    KCEVER                         *VERSIONE
098600970613     C                   KFLD                    KCENAR                         *NAZIONE
098700970613     C                   KFLD                    KCECAP                         *CAP
098800970613     C* AZCPT01L - PARZIALE
098900970613     C     KEYCPT        KLIST
099000970613     C                   KFLD                    KCTVER                         *VERSIONE
099100970613     C                   KFLD                    KCTTTC                         *TIPO TEMPO
099200970613     C* AZCPL01L - PARZIALE
099300970613     C     KEYCPL        KLIST
099400970613     C                   KFLD                    KCLVER                         *VERSIONE
099500970613     C                   KFLD                    KCLNAR                         *NAZIONE
099600970613     C                   KFLD                    KCLCAP                         *CAP
099700970613     C*
099800970613     C* VARIABILI DI CONTROLLO
099900970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
100000970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
100100970613     C                   MOVEL     'N'           WSTCAP            1            *CAP DA STAMPARE
100200970613     C                   MOVEL     'N'           WSTECC            1            *ECC DA STAMPATE
100300970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
100400970613     C*
100500970613     C* REPERIMENTO DATA E ORA CORRENTE
100600970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
100700970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
100800970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
100900970613     C*
101000970613     C* CARICA LE TABELLE OCCORRENTI
101100970613     C                   EXSR      CARTAB
101200970613     C*
101300970613     C                   ENDSR
101400970613     C*------------------------------------------------------------------------*
101500970613**   DEC - DECODIFICHE
101600970613CAP                                                         1
101700970613PROVINCIA                                                   2
101800970613LINEA DI ARRIVO                                             3
101900970613INTERNI                                                     4
102000970613CLIENTE                                                     5
102100970613TUTTI                                                       6
102200970613LOCALITA' DELIVERY                                          7
102300970613ITALIA                                                      8
1024009706139
10250097061310
