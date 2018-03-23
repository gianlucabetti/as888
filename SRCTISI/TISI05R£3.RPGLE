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
011400090605
011500090605     d ds5e          e ds
011600970613     C*------------------------------------------------------------------------*
011700970613     C* MAIN LINE
011800970613     C*------------------------------------------------------------------------*
011900970613     C*
012000970613     C* STAMPA PAGINA PARAMETRI DI LANCIO
012100970613     C                   EXSR      STAPAR
012200970613     C*
012300970613     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012400970613     C                   EXSR      SETFIL
012500970613     C*
012600970613     C* CICLO FINO A FINE FILE
012700970613DO  1C     FINL00        DOWEQ     '0'
012800970613     C*
012900970613     C* CONTROLLA LE ROTTURE PER SALTO PAGINA
013000970613     C                   EXSR      CHKROT
013100970613     C*
013200970613     C* STAMPA RIGA DETTAGLIO -CAP-
013300970613     C                   EXSR      STACAP
013400970613     C*
013500970613     C* CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
013600970613     C                   EXSR      LOPCPL
013700970613     C*
013800970613     C* LEGGE IL RECORD SUCCESSIVO
013900970613     C                   EXSR      LETFIL
014000970613E   1C                   ENDDO                                                  *FINE CAP
014100970613     C*
014200970613     C* STAMPA RIGA DI FINE PROSPETTO
014300970613     C                   WRITE     SI05RI                                       *RIGA SEPARAZIONE
014400970613     C                   WRITE     SI05FI                                       *RIGA FINE STAMPA
014500970613     C*
014600970613     C                   SETON                                        LR
014700970613     C*------------------------------------------------------------------------*
014800970613     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
014900970613     C*------------------------------------------------------------------------*
015000970613     C     SETFIL        BEGSR
015100970613     C*
015200970613     C* IMPOSTA LA NAZIONE
015300970613     C                   MOVEL     PARNA1        DEPNA1
015400970613IF  1C     PARNA1        IFEQ      'I  '
015500970613     C                   MOVEL     *BLANKS       DEPNA1
015600970613E   1C                   ENDIF
015700970613     C*
015800970613     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
015900970613IF  1C     PARTOR        IFEQ      'C'                                          *PER CAP
016000970613     C                   Z-ADD     PARVER        KCCVER
016100970613     C                   MOVEL     DEPNA1        KCCNAR
016200970613     C                   MOVEL     PARCA1        KCCCAP
016300970613     C     KEYCPC        SETLL     AZCPC01L                           99
016400970613E   1C                   ENDIF
016500970613     C*
016600970613IF  1C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
016700970613     C                   Z-ADD     PARVER        KCCVER
016800970613     C                   MOVEL     DEPNA1        KCCNAR
016900970613     C                   MOVEL     PARPR1        KCCPRV
017000970613     C                   MOVEL     PARCA1        KCCCAP
017100970613     C     KE2CPC        SETLL     AZCPC02L                           99
017200970613E   1C                   ENDIF
017300970613     C*
017400970613IF  1C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
017500970613     C                   Z-ADD     PARVER        KCCVER
017600970613     C                   MOVEL     DEPNA1        KCCNAR
017700010730     C*---               Z-ADD     PARLN1        KCCLNA
017800010730     C*---               MOVEL     PARCA1        KCCCAP
017900010730     C                   Z-ADD     *ZEROS        KCCLNA
018000010730     C                   MOVEL     *BLANKS       KCCCAP
018100970613     C     KE3CPC        SETLL     AZCPC03L                           99
018200970613E   1C                   ENDIF
018300970613     C*
018400970613     C* LETTURA RECORD IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
018500970613IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
018600970613     C                   EXSR      LETFIL                                       *LETTURA RECORD
018700970613X   1C                   ELSE                                                   *FINE FILE
018800970613     C                   MOVEL     '1'           FINL00                         *FINE PGM
018900970613E   1C                   ENDIF
019000970613     C*
019100970613     C                   ENDSR
019200970613     C*------------------------------------------------------------------------*
019300970613     C* LETFIL - LETTURA SUCESSIVA RECORD
019400970613     C*------------------------------------------------------------------------*
019500970613     C     LETFIL        BEGSR
019600970613     C*
019700970613     C* LEGGE FINO A:
019800970613     C*      -FINE FILE
019900970613     C*      -TROVATO RECORD VALIDO
020000970613     C                   MOVEL     'N'           WRECOK
020100970613DO  1C     *IN99         DOWEQ     *OFF
020200970613     C     WRECOK        ANDEQ     'N'
020300970613IF  2C     PARTOR        IFEQ      'C'                                          *PER CAP
020400980603     C     KCCVER        READE     AZCPC01L                               99
020500970613E   2C                   ENDIF
020600970613     C*
020700970613IF  2C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
020800980603     C     KCCVER        READE     AZCPC02L                               99
020900970613E   2C                   ENDIF
021000970613     C*
021100970613IF  2C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
021200980603     C     KCCVER        READE     AZCPC03L                               99
021300970613E   2C                   ENDIF
021400970613     C*
021500970613IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
021600970613     C                   MOVEL     '1'           FINL00                         *FINE PROGRAMMA
021700970613X   2C                   ELSE                                                   *RECORD TROVATO
021800970613     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
021900970613E   2C                   ENDIF
022000970613E   1C                   ENDDO
022100970613     C*
022200970613     C                   ENDSR
022300970613     C*------------------------------------------------------------------------*
022400970613     C* CHKREC - CONTROLLA VALIDITA' RECORD
022500970613     C*------------------------------------------------------------------------*
022600970613     C     CHKREC        BEGSR
022700970613     C*
022800970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
022900010730     C                   MOVEL     'N'           $LNLOC            1            *SI LOC.LINEE SELEZ.
023000970613     C*
023100971003     C* CAP ANNULLATO, ESCLUDE
023200970613IF  1C     CPCATB        IFNE      *BLANKS
023300970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
023400970613E   1C                   ENDIF
023500970613     C*
023600970613     C* NAZIONE <> DA QUELLE SELEZIONATE, ESCLUDE
023700970613IF  1C     PARNA1        IFNE      *BLANKS
023800970613IF  2C     PARNA1        IFEQ      'I  '
023900970613     C                   MOVEL     *BLANKS       DEPNA1
024000970613     C                   MOVEL     *BLANKS       DEPNA2
024100970613X   2C                   ELSE
024200970613     C                   MOVEL     PARNA1        DEPNA1
024300970613     C                   MOVEL     PARNA2        DEPNA2
024400970613E   2C                   ENDIF
024500970613IF  2C     CPCNAR        IFGE      DEPNA1
024600970613     C     CPCNAR        ANDLE     DEPNA2
024700970613X   2C                   ELSE
024800970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024900970613E   2C                   ENDIF
025000970613E   1C                   ENDIF
025100970613     C*
025200970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
025300970613IF  1C     PARCA1        IFNE      *BLANKS
025400970613IF  2C     CPCCAP        IFGE      PARCA1
025500970613     C     CPCCAP        ANDLE     PARCA2
025600970613X   2C                   ELSE
025700970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025800970613E   2C                   ENDIF
025900970613E   1C                   ENDIF
026000970613     C*
026100970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
026200970613IF  1C     PARPR1        IFNE      *BLANKS
026300970613IF  2C     CPCPRV        IFGE      PARPR1
026400970613     C     CPCPRV        ANDLE     PARPR2
026500970613X   2C                   ELSE
026600970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026700970613E   2C                   ENDIF
026800970613E   1C                   ENDIF
026900971003     C*
027000971003     C* CAP OBSOLETO E RICHIESTI I NON OBSOLETI, ESCLUDE
027100971003IF  1C     PAROBS        IFEQ      'N'
027200971003     C     CPCFI1        ANDEQ     'O'
027300971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027400971003E   1C                   ENDIF
027500010730     C*
027600010730     c* se scelta una linea controlla che sia nei CAP che nelle LOCALITA' ci siano record
027700010730     c* che abbiano la linea STANDARD o la OLTRE o la OLTRE 2 uguale a quella selezionata
027800010730     C                   MOVEL     'N'           $LNLOC                         *LOC.LINEE SELEZ.
027900010730IF  1C     WRECOK        IFEQ      'S'                                          *CAP ANCORA VALIDO
028000010730IF  2C     PARLN1        IFGT      *ZEROS
028100010730     C*
028200010730     C* solo se esistono eccezioni a livello di località
028300010730IF  3C     CPCESL        IFEQ      'S'                                          *LOC.PARTICOLARI
028400010730     C                   EXSR      CLNLOC
028500010730E   3C                   ENDIF
028600010730IF  3C                   IF        (CPCLNA >= PARLN1 AND CPCLNA <= PARLN2) OR
028700010730     C                             (CPCLOL >= PARLN1 AND CPCLOL <= PARLN2) OR
028800010730     C                             (CPCLO2 >= PARLN1 AND CPCLO2 <= PARLN2) OR
028900010730     C                             $LNLOC = 'S'                                 *SI CHK X LINEE LOC
029000010730X   3C                   ELSE
029100010730     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029200010730E   3c                   ENDIF
029300010730E   2C                   ENDIF
029400010730E   1C                   ENDIF
029500970613     C*
029600970613     C                   ENDSR
029700010730     C*--------------------------------------------------------------------------------------------*
029800010730     C* CLNLOC - CONTROLLA CHE LE LOCALITA' DEL CAP ABBIANO LINEE FRA QUELLE SELEZIONATE
029900010730     C*--------------------------------------------------------------------------------------------*
030000010730     C     CLNLOC        BEGSR
030100010730     C*
030200010730     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
030300010730     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
030400010730     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
030500010730     C     KEYCPL        SETLL     AZCPL01L
030600010730     C     KEYCPL        READE     AZCPL01L                               98
030700010730DO  1C     *IN98         DOWEQ     *OFF
030800010730IF  2C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
030900010730     C     CPLLIV        ANDEQ     'L'                                          *LOC. PARTICOLARE
031000010730     C*
031100010730     C* CONTROLLA LE LINEE FRA QUELLE SELEZIONATE
031200010730IF  3C                   IF        (CPLLNA >= PARLN1 AND CPLLNA <= PARLN2) OR
031300010730     C                             (CPLLOL >= PARLN1 AND CPLLOL <= PARLN2) OR
031400010730     C                             (CPLLO2 >= PARLN1 AND CPLLO2 <= PARLN2)
031500010730     C                   MOVEL     'S'           $LNLOC                         *SI LOC.LINEE SELEZ.
031600010730     C                   LEAVE
031700010730E   3C                   ENDIF
031800010730E   2C                   ENDIF
031900010730     C*
032000010730     C     KEYCPL        READE     AZCPL01L                               98
032100010730E   1C                   ENDDO                                                  *FINE LOCALITA' CAP
032200010730     C*
032300010730     C                   ENDSR
032400970613     C*------------------------------------------------------------------------*
032500971003     C* CHKROT - CONTROLLA LE ROTTURE PER SALTO PAGINA
032600970613     C*------------------------------------------------------------------------*
032700970613     C     CHKROT        BEGSR
032800970613     C*
032900970613     C* ROTTURA NAZIONE -SEMPRE-
033000970613IF  1C     CPCNAR        IFNE      ROTNAR
033100970613     C                   MOVEL     CPCNAR        ROTNAR                         *DEPOSITO ROTTURA
033200970613     C                   MOVEL     CPCNAR        INCNAR                         *PER INTESTAZIONE
033300970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
033400970613E   1C                   ENDIF
033500970613     C*
033600970613     C* ROTTURA PROVINCIA -SE ORDINAMENTO PER PROVINCIA-
033700970613IF  1C     PARTOR        IFEQ      'P'
033800970613IF  2C     CPCPRV        IFNE      ROTPRV
033900970613     C                   MOVEL     CPCPRV        ROTPRV                         *DEPOSITO ROTTURA
034000970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034100970613E   1C                   ENDIF
034200970613E   1C                   ENDIF
034300970613     C*
034400970613     C* ROTTURA LINEA DI ARRIVO -SE ORDINAMENTO PER LINEA DI ARRIVO-
034500970613IF  1C     PARTOR        IFEQ      'L'
034600970613IF  2C     CPCLNA        IFNE      ROTLNA
034700970613     C                   MOVEL     CPCLNA        ROTLNA                         *DEPOSITO ROTTURA
034800970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034900970613E   1C                   ENDIF
035000970613E   1C                   ENDIF
035100970613     C*
035200970613     C                   ENDSR
035300970613     C*------------------------------------------------------------------------*
035400970613     C* STACAP - STAMPA RIGA DI DETTAGLIO -CAP-
035500970613     C*------------------------------------------------------------------------*
035600970613     C     STACAP        BEGSR
035700970613     C*
035800970613     C* REIMPOSTA LE VARIABILI DI LAVORO
035900970613     C                   MOVEL     'N'           WSTCAP                         *CAP NON STAMPATO
036000970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
036100970613     C*
036200970613     C* CONTROLLA SE IL CAP E' DA STAMPARE
036300970613     C                   EXSR      CHKCAP
036400970613IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
036500970613     C* CAMPI
036600970613     C                   MOVEL     CPCCAP        DECCAP
036700970613     C                   MOVEL     CPCFLG        DECFLG
036800970917     C                   MOVEL     CPCFI1        DECFI1
036900970613     C                   MOVEL     *BLANKS       DEDMIX
037000970613     C                   MOVEL     CPCPRV        DECPRV
037100970613     C                   MOVEL     CPCPRA        DECPRA
037200970613IF  2C     CPCPRA        IFNE      *BLANKS
037300970613     C                   MOVEL     '/'           DEDPRX
037400970613E   2C                   ENDIF
037500970613     C                   Z-ADD     CPCLNA        DECLNA
037600970613     C                   MOVEL     CPCZNC        DECZNC
037700970613     C                   MOVEL     CPCZFD        DECZFD
037800970613IF  2C     CPCLKS        IFGT      *ZEROS
037900000518     C     CPCLMS        ORGT      *ZEROS
038000970613     C                   MOVEL     CPCLOS        DECLOS
038100970613     C                   MOVEL     CPCZOS        DECZOS
038200970613     C                   Z-ADD     CPCLKS        DECLKS
038300970613     C                   Z-ADD     CPCLMS        DECLMS
038400970613E   2C                   ENDIF
038500970613IF  2C     CPCLKG        IFGT      *ZEROS
038600970613     C     CPCLMC        ORGT      *ZEROS
038700970613     C                   MOVEL     CPCLOL        DECLOL
038800970613     C                   MOVEL     CPCZOL        DECZOL
038900970613     C                   Z-ADD     CPCLKG        DECLKG
039000970613     C                   Z-ADD     CPCLMC        DECLMC
039100970613E   2C                   ENDIF
039200971218     C*
039300970613     C                   MOVEL     CPCCTS        DECCTS
039400970613     C                   MOVEL     CPCISO        DECINL
039500010727     C                   Z-ADD     CPCTTC        DECTTC
039600010727     C                   MOVEL     *ALL'9'       DECTTR
039700010727     C                   MOVE      CPCFI2        DECTTR
039800970613     C*
039900970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN DEL CAP
040000970613IF  2C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
040100970613     C                   Z-ADD     CPCTTC        DEPTTC                         *TIPO TEMPO
040200970613     C                   EXSR      LOPCPT
040300970613E   2C                   ENDIF
040400970613     C*
040500970613     C* STAMPA RIGA DI DETTALGIO
040600970613     C                   MOVEL     'S'           WSTCAP                         *CAP STAMPATO
040700970613     C                   EXSR      STMDET
040800970613     C*
040900970613     C* CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
041000970613     C                   EXSR      LOPCPE
041100970613E   1C                   ENDIF
041200970613     C*
041300970613     C                   ENDSR
041400970613     C*------------------------------------------------------------------------*
041500970613     C* CHKCAP - CONTROLLA SE IL CAP E' DA STAMPARE
041600970613     C*------------------------------------------------------------------------*
041700970613     C     CHKCAP        BEGSR
041800970613     C*
041900970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
042000970613     C*
042100970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
042200970613IF  1C     PARISO        IFNE      *BLANKS
042300970613IF  2C     CPCISO        IFNE      PARISO
042400970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042500970613E   2C                   ENDIF
042600970613E   1C                   ENDIF
042700970613     C*
042800970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
042900970613IF  1C     PARTTC        IFGT      *ZEROS
043000970613IF  2C     CPCTTC        IFNE      PARTTC
043100970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043200970613E   2C                   ENDIF
043300970613E   1C                   ENDIF
043400970613     C*
043500970613     C                   ENDSR
043600970613     C*------------------------------------------------------------------------*
043700970613     C* LOPCPE - CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
043800970613     C*------------------------------------------------------------------------*
043900970613     C     LOPCPE        BEGSR
044000970613     C*
044100970613     C* REIMPOSTA LE VARIABILI DI LAVOR
044200970613     C                   MOVEL     'N'           WSTECC                         *ECC NON STAMPATO
044300970613     C*
044400970613IF  1C     CPCESE        IFEQ      'S'                                          *ESISTONO ECCEZIONI
044500970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
044600970613     C                   Z-ADD     CPCVER        KCEVER                         *VERSIONE
044700970613     C                   MOVEL     CPCNAR        KCENAR                         *NAZIONE
044800970613     C                   MOVEL     CPCCAP        KCECAP                         *CAP
044900970613     C     KEYCPE        SETLL     AZCPE01L
045000970613     C     KEYCPE        READE     AZCPE01L                               98
045100970613DO  2C     *IN98         DOWEQ     *OFF
045200970613IF  3C     CPEATB        IFEQ      *BLANKS                                      *VALIDI
045300970613     C* CAMPI
045400970613     C                   MOVEL     *BLANKS       DEDMIX
045500970613     C                   MOVEL     CPETFP        DECTFP
045600970613     C                   Z-ADD     CPELNA        DECLNA
045700970613     C                   MOVEL     CPEZNC        DECZNC
045800970613IF  4C     CPCLKS        IFGT      *ZEROS
045900970613     C     CPCLMS        ORGT      *ZEROS
046000970613     C                   MOVEL     CPELOS        DECLOS
046100970613     C                   MOVEL     CPEZOS        DECZOS
046200970613E   4C                   ENDIF
046300970613IF  4C     CPCLKG        IFGT      *ZEROS
046400970613     C     CPCLMC        ORGT      *ZEROS
046500970613     C                   MOVEL     CPELOL        DECLOL
046600970613     C                   MOVEL     CPEZOL        DECZOL
046700970613E   4C                   ENDIF
046800970613     C* DECODIFICHE
046900970613     C                   Z-ADD     1             I
047000970613     C                   SETOFF                                           98
047100970613     C     CPETFP        LOOKUP    ORG(I)                                 98
047200970613IF  4C     *IN98         IFEQ      *ON
047300970613     C                   MOVEL     DORG(I)       DEDTFP                         *TERMINAL PARTENZA
047400970613E   4C                   ENDIF
047500970613     C*
047600970613     C* STAMPA RIGA DI DETTALGIO
047700970613     C                   MOVEL     'S'           WSTECC                         *ECCEZIONI STAMPATE
047800970613     C                   EXSR      STMDET
047900970613E   3C                   ENDIF
048000970613     C     KEYCPE        READE     AZCPE01L                               98
048100970613E   2C                   ENDDO                                                  *FINE ECCEZIONI
048200970613E   1C                   ENDIF
048300970613     C*
048400970613     C                   ENDSR
048500970613     C*------------------------------------------------------------------------*
048600970613     C* LOPCPL - CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
048700970613     C*------------------------------------------------------------------------*
048800970613     C     LOPCPL        BEGSR
048900970613     C*
049000970613     C* DEPOSITA LA DESCRIZIONE DELLA PROVINCIA DEL CAP
049100970613     C                   Z-ADD     1             I
049200970613     C                   SETOFF                                           98
049300970613     C     CPCPRV        LOOKUP    PRV(I)                                 98
049400970613IF  1C     *IN98         IFEQ      *ON
049500970613     C                   MOVEL     DPRV(I)       DEPDPR
049600970613X   1C                   ELSE
049700970613     C                   MOVEL     *BLANKS       DEPDPR
049800970613E   1C                   ENDIF
049900970613     C*
050000970613     C* SE RICHIESTI IN STAMPA, LEGGE LE LOCALITA'
050100010730IF  1c     PARLOC        IFNE      'N'
050200970613     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
050300970613     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
050400970613     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
050500970613     C     KEYCPL        SETLL     AZCPL01L
050600970613     C     KEYCPL        READE     AZCPL01L                               98
050700970613DO  2C     *IN98         DOWEQ     *OFF
050800970613IF  3C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
050900970613IF  4C     PARLOC        IFEQ      'D'                                          *SELEZ.LOC.DELIVERY
051000970613     C     CPLDLV        ANDEQ     'S'
051100970613     C     PARLOC        ORNE      'D'                                          *SELEZ.TUTTE
051200970613IF  5C     PARLIV        IFEQ      'S'                                          *SELEZ.LOC <> CAP
051300970613     C     CPLLIV        ANDEQ     'L'
051400970613     C     PARLIV        OREQ      'N'                                          *SELEZ.TUTTE
051500970613     C*
051600990525     C* NON STAMPA LA LOCALITA' SE E' UGUALE ALLA DESCRIZIONE DELLA PROVINCIA E
051700990525     C* SE E' GESTITA A LIVELLO DI LOCALITA'
051800970613IF  6C     DEPDPR        IFNE      CPLLOC                                       *-PEZZA-
051900990525     C     DEPDPR        OREQ      CPLLOC
052000990525     C     CPLLIV        ANDEQ     'L'
052100970613     C*
052200970613     C* CONTROLLA SE LA LOCALITA' E' DA STAMPARE
052300970613     C                   EXSR      CHKLOC
052400970613IF  7C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
052500970613     C* CAMPI
052600970613IF  8C     WSTECC        IFEQ      'S'                                          *ECCEZIONI STAMPATE
052700970613     C                   WRITE     SI05R2                                       *STAMPA RIGA SEPARAZ
052800970613     C                   MOVEL     'N'           WSTECC                         *X NON STAMPARE +RIG
052900970613E   8C                   ENDIF
053000970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
053100970613IF  8C     WSTCAP        IFEQ      'N'                                          *CAP NON STMPATO
053200970613     C                   MOVEL     CPCCAP        DECCAP                         *RIEMPE CAMPI CAP
053300970613E   8C                   ENDIF
053400970917     C                   MOVEL     CPLFI1        DECFI1
053500970613     C                   MOVEL     *BLANKS       DEDMIX
053600970613     C                   MOVEL     CPLLOC        DECLOC
053700000529     C                   MOVEL     CPLLOC        SAVLOC           35
053800970613IF  8C     CPLLIV        IFEQ      'L'                                          *LOC.GEST.LIV.LOC.
053900970613     C                   Z-ADD     CPLLNA        DECLNA
054000970613     C                   MOVEL     CPLZNC        DECZNC
054100970613     C                   MOVEL     CPLZFD        DECZFD
054200970621IF  9C     CPLLKS        IFGT      *ZEROS
054300970621     C     CPLLMS        ORGT      *ZEROS
054400970613     C                   MOVEL     CPLLOS        DECLOS
054500970613     C                   MOVEL     CPLZOS        DECZOS
054600970613     C                   Z-ADD     CPLLKS        DECLKS
054700970613     C                   Z-ADD     CPLLMS        DECLMS
054800970613E   9C                   ENDIF
054900970621IF  9C     CPLLKG        IFGT      *ZEROS
055000970621     C     CPLLMC        ORGT      *ZEROS
055100970613     C                   MOVEL     CPLLOL        DECLOL
055200970613     C                   MOVEL     CPLZOL        DECZOL
055300970613     C                   Z-ADD     CPLLKG        DECLKG
055400970613     C                   Z-ADD     CPLLMC        DECLMC
055500970613E   9C                   ENDIF
055600000518     C*
055700970613     C                   MOVEL     CPLCTS        DECCTS
055800970613     C                   MOVEL     CPLISO        DECINL
055900010727     C                   Z-ADD     CPLTTC        DECTTC
056000010727     C                   MOVEL     *ALL'9'       DECTTR
056100010727     C                   MOVE      CPLFI2        DECTTR
056200970613     C*
056300970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGNA DELLA LOCALITA'
056400970613IF  9C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
056500970613     C                   Z-ADD     CPLTTC        DEPTTC                         *TIPO TEMPO
056600970613     C                   EXSR      LOPCPT
056700970613E   9C                   ENDIF
056800970613E   8C                   ENDIF
056900970613     C*
057000970613     C* STAMPA RIGA DI DETTALGIO
057100970613     C                   EXSR      STMDET
057200970613E   7C                   ENDIF
057300970613E   6C                   ENDIF
057400970613E   5C                   ENDIF
057500970613E   4C                   ENDIF
057600970613E   3C                   ENDIF
057700970613     C*
057800970613     C     KEYCPL        READE     AZCPL01L                               98
057900970613E   2C                   ENDDO                                                  *FINE LOCALITA'
058000970613E   1C                   ENDIF
058100970613     C*
058200970613     C                   ENDSR
058300970613     C*------------------------------------------------------------------------*
058400970613     C* CHKLOC - CONTROLLA SE LA LOCCALITA' E' DA STAMPARE
058500970613     C*------------------------------------------------------------------------*
058600970613     C     CHKLOC        BEGSR
058700970613     C*
058800970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
058900971003     C*
059000971003     C* LOCALITA' OBSOLETA E RICHIESTI LE NON OBSOLETE, ESCLUDE
059100971003IF  1C     PAROBS        IFEQ      'N'
059200971003     C     CPLFI1        ANDEQ     'O'
059300971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
059400971003E   1C                   ENDIF
059500970613     C*
059600970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
059700971107IF  1C*---       PARISO    IFNE *BLANKS
059800971107IF  2C*---       CPLISO    IFEQ PARISO
059900971107     C*---       CPCISO    OREQ PARISO
060000971107X   2C*---                 ELSE
060100971107     C*---                 MOVEL'N'       WRECOK           *RECORD NON VALIDO
060200971107E   2C*---                 ENDIF
060300971107E   1C*---                 ENDIF
060400971107     C*
060500971107IF  1C     PARISO        IFNE      *BLANKS
060600971107IF  2C     CPLISO        IFEQ      PARISO
060700971107     C     CPLLIV        OREQ      'C'
060800971107     C     CPCISO        ANDEQ     PARISO
060900971107X   2C                   ELSE
061000971107     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061100971107E   2C                   ENDIF
061200971107E   1C                   ENDIF
061300970613     C*
061400970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
061500970613IF  1C     PARTTC        IFGT      *ZEROS
061600970613IF  2C     CPLTTC        IFNE      PARTTC
061700970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061800970613E   2C                   ENDIF
061900970613E   1C                   ENDIF
062000970613     C*
062100970613     C                   ENDSR
062200970613     C*------------------------------------------------------------------------*
062300970613     C* LOPCPT- CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN
062400970613     C*------------------------------------------------------------------------*
062500970613     C     LOPCPT        BEGSR
062600970613     C*
062700970613     C                   Z-ADD     CPCVER        KCTVER                         *VERSIONE
062800970613     C                   Z-ADD     DEPTTC        KCTTTC                         *TIPO TEMPO
062900970613     C     KEYCPT        SETLL     AZCPT01L
063000970613     C     KEYCPT        READE     AZCPT01L                               98
063100970613DO  1C     *IN98         DOWEQ     *OFF
063200970613IF  2C     CPTATB        IFEQ      *BLANKS                                      *VALIDI
063300970613IF  3C     CPTTSP        IFEQ      'D'                                          *DISTRIBUZIONE
063400970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
063500970613     C     PARTIC        OREQ      'T'
063600970613     C                   Z-ADD     CPT0IN        DECD0I                         *DIS INTERNO 00
063700970613     C                   Z-ADD     CPT1IN        DECD1I                         *DIS INTERNO 12
063800970613E   4C                   ENDIF
063900970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
064000970613     C     PARTIC        OREQ      'T'
064100970613     C                   Z-ADD     CPT0CN        DECD0C                         *DIS CLIENTE 00
064200970613     C                   Z-ADD     CPT1CN        DECD1C                         *DIS CLIENTE 12
064300970613E   4C                   ENDIF
064400970613     C                   Z-ADD     CPTTIS        DECDIS                         *DIS ISOLA
064500970613     C                   Z-ADD     CPTTLD        DECDLD                         *DIS L.DISAGIATA
064600970613     C                   Z-ADD     CPTTCS        DECDCS                         *DIS C.STORICO
064700970613E   3C                   ENDIF
064800970613     C*
064900970613IF  3C     CPTTSP        IFEQ      'C'                                          *CORRIERE
065000970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
065100970613     C     PARTIC        OREQ      'T'
065200970613     C                   Z-ADD     CPT0IN        DECC0I                         *COR INTERNO 00
065300970613     C                   Z-ADD     CPT1IN        DECC1I                         *COR INTERNO 12
065400970613E   4C                   ENDIF
065500970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
065600970613     C     PARTIC        OREQ      'T'
065700970613     C                   Z-ADD     CPT0CN        DECC0C                         *COR CLIENTE 00
065800970613     C                   Z-ADD     CPT1CN        DECC1C                         *COR CLIENTE 12
065900970613E   4C                   ENDIF
066000970613     C                   Z-ADD     CPTTIS        DECCIS                         *COR ISOLA
066100970613     C                   Z-ADD     CPTTLD        DECCLD                         *COR L.DISAGIATA
066200970613     C                   Z-ADD     CPTTCS        DECCCS                         *COR C.STORICO
066300970613E   3C                   ENDIF
066400970613     C*
066500970613IF  3C     CPTTSP        IFEQ      'E'                                          *CORRIERE
066600970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
066700970613     C     PARTIC        OREQ      'T'
066800970613     C                   Z-ADD     CPT0IN        DECE0I                         *ESP INTERNO 00
066900970613     C                   Z-ADD     CPT1IN        DECE1I                         *ESP INTERNO 12
067000970613E   4C                   ENDIF
067100970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
067200970613     C     PARTIC        OREQ      'T'
067300970613     C                   Z-ADD     CPT0CN        DECE0C                         *ESP CLIENTE 00
067400970613     C                   Z-ADD     CPT1CN        DECE1C                         *ESP CLIENTE 12
067500970613E   4C                   ENDIF
067600970613     C                   Z-ADD     CPTTIS        DECEIS                         *ESP ISOLA
067700970613     C                   Z-ADD     CPTTLD        DECELD                         *ESP L.DISAGIATA
067800970613     C                   Z-ADD     CPTTCS        DECECS                         *ESP C.STORICO
067900970613E   3C                   ENDIF
068000970613E   2C                   ENDIF
068100970613     C     KEYCPT        READE     AZCPT01L                               98
068200970613E   1C                   ENDDO                                                  *FINE TEMPI
068300970613     C*
068400970613     C                   ENDSR
068500970613     C*------------------------------------------------------------------------*
068600970613     C* STMDET - STAMPA RIGA DI DETTAGLIO
068700970613     C*------------------------------------------------------------------------*
068800970613     C     STMDET        BEGSR
068900970613     C*
069000970613     C* SE FINE PAGINA, STAMPA L'INTESTAZIONE
069100970613IF  1C     *IN01         IFEQ      *ON
069200970613     C                   WRITE     SI05TE                                       *TESTATA
069300970613     C                   WRITE     SI05RI                                       *RIGA
069400970613     C                   WRITE     SI05IN                                       *INTESTAZIONE
069500970613     C                   WRITE     SI05RI                                       *RIGA
069600970613     C                   SETOFF                                       01
069700970613E   1C                   ENDIF
069800000428     C*
069900000428     C* STAMPA DETTAGLIO
070000970613     C                   WRITE     SI05DE                                       *DETTAGLIO
070100000428     C     PARTEM        IFEQ      'S'
070200000428     C                   WRITE     SI05TM                                       *DETTAGLIO X TEMPIAR
070300000428     C                   ENDIF
070400970613     C*
070500970613     C                   ENDSR
070600970613     C*------------------------------------------------------------------------*
070700970613     C* STAPAR - STAMPA PAGINA PARAMETRI
070800970613     C*------------------------------------------------------------------------*
070900970613     C     STAPAR        BEGSR
071000970613     C* TESTATA
071100970613     C                   Z-ADD     PARDRI        G08INV                         *DATA RIFERIMENTO
071200970613     C                   Z-ADD     *ZEROS        G08DAT
071300970613     C                   MOVEL     '3'           G08ERR
071400970613     C                   CALL      'XSRDA8'
071500970613     C                   PARM                    WLBDA8
071600970613     C                   Z-ADD     G08DAT        TECDRI
071700970613     C* PARAMETRI
071800970613     C                   MOVEL     PARTOR        PACTOR                         *ORDINAMENTO
071900970613IF  1C     PARTOR        IFEQ      'C'
072000970613     C                   MOVEL     DEC(1)        PADTOR
072100970613E   1C                   ENDIF
072200970613IF  1C     PARTOR        IFEQ      'P'
072300970613     C                   MOVEL     DEC(2)        PADTOR
072400970613E   1C                   ENDIF
072500970613IF  1C     PARTOR        IFEQ      'L'
072600970613     C                   MOVEL     DEC(3)        PADTOR
072700970613E   1C                   ENDIF
072800970613     C*
072900970613IF  1C     PARNA1        IFNE      *BLANKS
073000970613     C                   MOVEL     PARNA1        PACNA1                         *NAZIONE DAL
073100970613IF  2C     PARNA1        IFEQ      'I'
073200970613     C                   MOVEL     DEC(8)        PADNA1                         *NAZIONE ITALIA
073300970613X   2C                   ELSE
073400970613     C                   Z-ADD     1             I
073500970613     C                   SETOFF                                           98
073600970613     C     PARNA1        LOOKUP    NAR(I)                                 98
073700970613IF  3C     *IN98         IFEQ      *ON
073800970613     C                   MOVEL     DNAR(I)       PADNA1
073900970613E   3C                   ENDIF
074000970613E   2C                   ENDIF
074100970613E   1C                   ENDIF
074200970613     C*
074300970613IF  1C     PARNA2        IFNE      *BLANKS
074400970613     C                   MOVEL     PARNA2        PACNA2                         *NAZIONE AL
074500970613IF  2C     PARNA2        IFEQ      'I'
074600970613     C                   MOVEL     DEC(8)        PADNA2                         *NAZIONE ITALIA
074700970613X   2C                   ELSE
074800970613     C                   Z-ADD     1             I
074900970613     C                   SETOFF                                           98
075000970613     C     PARNA2        LOOKUP    NAR(I)                                 98
075100970613IF  3C     *IN98         IFEQ      *ON
075200970613     C                   MOVEL     DNAR(I)       PADNA2
075300970613E   3C                   ENDIF
075400970613E   2C                   ENDIF
075500970613E   1C                   ENDIF
075600970613     C*
075700970613     C                   MOVEL     PARCA1        PACCA1                         *CAP DAL
075800970613     C                   MOVEL     PARCA2        PACCA2                         *CAP AL
075900970613     C*
076000970613IF  1C     PARPR1        IFNE      *BLANKS
076100970613     C                   MOVEL     PARPR1        PACPR1                         *PROVINCIA DAL
076200970613     C                   Z-ADD     1             I
076300970613     C                   SETOFF                                           98
076400970613     C     PARPR1        LOOKUP    PRV(I)                                 98
076500970613IF  2C     *IN98         IFEQ      *ON
076600970613     C                   MOVEL     DPRV(I)       PADPR1
076700970613E   2C                   ENDIF
076800970613E   1C                   ENDIF
076900970613     C*
077000970613IF  1C     PARPR2        IFNE      *BLANKS
077100970613     C                   MOVEL     PARPR1        PACPR2                         *PROVINCIA AL
077200970613     C                   Z-ADD     1             I
077300970613     C                   SETOFF                                           98
077400970613     C     PARPR2        LOOKUP    PRV(I)                                 98
077500970613IF  2C     *IN98         IFEQ      *ON
077600970613     C                   MOVEL     DPRV(I)       PADPR2
077700970613E   2C                   ENDIF
077800970613E   1C                   ENDIF
077900970613     C*
078000970613IF  1C     PARLN1        IFGT      *ZEROS
078100970613     C                   Z-ADD     PARLN1        PACLN1                         *LINEA ARRIVO DAL
078200970613     C                   Z-ADD     1             I
078300970613     C                   SETOFF                                           98
078400970613     C     PARLN1        LOOKUP    ORG(I)                                 98
078500970613IF  2C     *IN98         IFEQ      *ON
078600970613     C                   MOVEL     DORG(I)       PADLN1
078700970613E   2C                   ENDIF
078800970613E   1C                   ENDIF
078900970613     C*
079000970613IF  1C     PARLN2        IFGT      *ZEROS
079100970613     C                   Z-ADD     PARLN2        PACLN2                         *LINEA ARRIVO AL
079200970613     C                   Z-ADD     1             I
079300970613     C                   SETOFF                                           98
079400970613     C     PARLN2        LOOKUP    ORG(I)                                 98
079500970613IF  2C     *IN98         IFEQ      *ON
079600970613     C                   MOVEL     DORG(I)       PADLN2
079700970613E   2C                   ENDIF
079800970613E   1C                   ENDIF
079900970613     C*
080000970613IF  1C     PARISO        IFNE      *BLANKS
080100970613     C                   MOVEL     PARISO        PACISO                         *TIPO INOLTRO
080200970613     C                   Z-ADD     1             I
080300970613     C                   SETOFF                                           98
080400970613     C     PARISO        LOOKUP    INL(I)                                 98
080500970613IF  2C     *IN98         IFEQ      *ON
080600970613     C                   MOVEL     DINL(I)       PADISO
080700970613E   2C                   ENDIF
080800970613E   1C                   ENDIF
080900970613     C*
081000970613     C                   Z-ADD     PARTTC        PACTTC
081100970613     C*
081200970613     C                   MOVEL     PARTEM        PACTEM
081300970613     C*
081400970613     C                   MOVEL     PARTIC        PACTIC                         *TEMPI CONSEGNA
081500970613IF  1C     PARTIC        IFEQ      'I'
081600970613     C                   MOVEL     DEC(4)        PADTIC
081700970613E   1C                   ENDIF
081800970613IF  1C     PARTIC        IFEQ      'C'
081900970613     C                   MOVEL     DEC(5)        PADTIC
082000970613E   1C                   ENDIF
082100970613IF  1C     PARTIC        IFEQ      'T'
082200970613     C                   MOVEL     DEC(6)        PADTIC
082300970613E   1C                   ENDIF
082400970613     C*
082500970613     C                   MOVEL     PARLOC        PACLOC
082600970613IF  1C     PARLOC        IFEQ      'D'
082700970613     C                   MOVEL     DEC(7)        PADLOC
082800970613E   1C                   ENDIF
082900970613     C*
083000970613     C                   MOVEL     PARLIV        PACLIV
083100971003     C*
083200971003     C                   MOVEL     PAROBS        PACOBS
083300090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
083400090605      * 'C' - 'D' - 'E'
083500090605     c                   eval      ktbcod = '5E'
083600090605     c     ke2tab        setll     tabel00f
083700090605     c                   do        *hival
083800090605     c     ke2tab        reade     tabel00f
083900090605     c                   if        %eof(tabel00f)
084000090605     c                   leave
084100090605     c                   endif
084200090605     c                   eval      ds5e = tbluni
084300090605     c                   if        tblkey = 'C'
084400090605     c                   eval      dtspci = §5ed03
084500090605     c                   eval      dtspcl = §5ed03
084600090605     c                   eval      dtspcc = §5ed03
084700090605     c                   endif
084800090605     c                   if        tblkey = 'D'
084900090605     c                   eval      dtspdi = §5ed03
085000090605     c                   eval      dtspdl = §5ed03
085100090605     c                   eval      dtspdc = §5ed03
085200090605     c                   endif
085300090605     c                   if        tblkey = 'E'
085400090605     c                   eval      dtspei = §5ed03
085500090605     c                   eval      dtspel = §5ed03
085600090605     c                   eval      dtspec = §5ed03
085700090605     c                   endif
085800090605     c                   enddo
085900970613     C*
086000970613     C                   WRITE     SI05TE                                       *TESTATA
086100970613     C                   WRITE     SI05PA                                       *PARAMETRI
086200970613     C*
086300970613     C                   ENDSR
086400970613     C*------------------------------------------------------------------------*
086500970613     C* CARTAB - CARICA LE TABELLE
086600970613     C*------------------------------------------------------------------------*
086700970613     C     CARTAB        BEGSR
086800970613     C*--------------------
086900970613     C* FILIALI
087000970613     C*--------------------
087100970613     C                   Z-ADD     *ZEROS        I                 5 0
087200970613     C     *LOVAL        SETLL     AZORG01L
087300970613     C                   READ      AZORG01L                               99
087400970613DO  1C     *IN99         DOWEQ     *OFF
087500970613IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
087600970613     C                   ADD       1             I
087700970613     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
087800970613IF  3C     ORGDE5        IFNE      *BLANKS
087900970613     C                   MOVEL     ORGDE5        DORG(I)                        *DESCRIZIONE + CORTA
088000970613X   3C                   ELSE
088100970613     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE + LUNGA
088200970613E   3C                   ENDIF
088300970613E   2C                   ENDIF
088400970613     C                   READ      AZORG01L                               99
088500970613E   1C                   ENDDO
088600970613     C*--------------------
088700970613     C* NAZIONI
088800970613     C*--------------------
088900970613     C                   Z-ADD     *ZEROS        I
089000020906     C                   Z-ADD     1             KTBKUT
089100970613     C                   MOVEL     '15'          KTBCOD
089200970613     C     KE2TAB        SETLL     TABEL00F
089300970613     C     KE2TAB        READE     TABEL00F                               99
089400970613DO  1C     *IN99         DOWEQ     *OFF
089500970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
089600970613     C                   ADD       1             I
089700970613     C                   MOVEL     TBLKEY        NAR(I)                         *CODICE
089800970613     C                   MOVEL     TBLUNI        DS15
089900970613     C                   MOVEL     §15DES        DNAR(I)                        *DESCRIZIONE
090000970613E   2C                   ENDIF
090100970613     C     KE2TAB        READE     TABEL00F                               99
090200970613E   1C                   ENDDO
090300970613     C*--------------------
090400970613     C* PROVINCE
090500970613     C*--------------------
090600970613     C                   Z-ADD     *ZEROS        I
090700020906     C                   Z-ADD     1             KTBKUT
090800970613     C                   MOVEL     'PR'          KTBCOD
090900970613     C     KE2TAB        SETLL     TABEL00F
091000970613     C     KE2TAB        READE     TABEL00F                               99
091100970613DO  1C     *IN99         DOWEQ     *OFF
091200970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
091300970613     C                   ADD       1             I
091400970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
091500970613     C                   MOVEL     TBLUNI        DSPR
091600970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
091700970613E   2C                   ENDIF
091800970613     C     KE2TAB        READE     TABEL00F                               99
091900970613E   1C                   ENDDO
092000970613     C*--------------------
092100970613     C* TIPI INOLTRO
092200970613     C*--------------------
092300970613     C                   Z-ADD     *ZEROS        I
092400020906     C                   Z-ADD     1             KTBKUT
092500970613     C                   MOVEL     '9A'          KTBCOD
092600970613     C     KE2TAB        SETLL     TABEL00F
092700970613     C     KE2TAB        READE     TABEL00F                               99
092800970613DO  1C     *IN99         DOWEQ     *OFF
092900970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
093000970613     C                   ADD       1             I
093100970613     C                   MOVEL     TBLKEY        INL(I)                         *CODICE
093200970613     C                   MOVEL     TBLUNI        DS9A
093300970613     C                   MOVEL     §9ADES        DINL(I)                        *DESCRIZIONE
093400970613E   2C                   ENDIF
093500970613     C     KE2TAB        READE     TABEL00F                               99
093600970613E   1C                   ENDDO
093700970613     C*
093800970613     C                   ENDSR
093900970613     C*------------------------------------------------------------------------*
094000970613     C* *INZSR - OPERAZIONI INIZIALI
094100970613     C*------------------------------------------------------------------------*
094200970613     C     *INZSR        BEGSR
094300970613     C*
094400970613     C* RICEVIMENTO PARAMETRI
094500970613     C     *ENTRY        PLIST
094600970613     C                   PARM                    KPJBA
094700970613     C                   MOVEL     KPJBU         PARAM
094800061016      *
094900061016      * Reperimento dati aziendali
095000061016     c     *dtaara       define    §azute        AZUTEds
095100061016     c     *dtaara       define    §datiute      dDATIUTE
095200061016     c                   in(E)     *dtaara
095300061016     c                   if        %ERROR or RSUT = *blanks
095400061016     c                   clear                   Tibs34Ds
095500061016     c                   call      'TIBS34R'
095600061016     c                   parm                    Tibs34Ds
095700061016     c                   in        *dtaara
095800061016     c                   endif
095900061016      *
096000000509     C* IMPOSTAZIONI INIZIALI
096100000509     C     PARTEM        IFEQ      'S'
096200000509     C                   SETON                                        40
096300000509     C                   ENDIF
096400970613     C*
096500970613     C* VARIABILI RIFERITE AL DATA BASE
096600970613     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
096700970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
096800970613     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
096900970613     C     *LIKE         DEFINE    CPCNAR        KCCNAR
097000970613     C     *LIKE         DEFINE    CPCCAP        KCCCAP
097100970613     C     *LIKE         DEFINE    CPCPRV        KCCPRV
097200970613     C     *LIKE         DEFINE    CPCLNA        KCCLNA
097300970613     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
097400970613     C     *LIKE         DEFINE    CPENAR        KCENAR
097500970613     C     *LIKE         DEFINE    CPECAP        KCECAP
097600970613     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
097700970613     C     *LIKE         DEFINE    CPTTTC        KCTTTC
097800970613     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
097900970613     C     *LIKE         DEFINE    CPLNAR        KCLNAR
098000970613     C     *LIKE         DEFINE    CPLCAP        KCLCAP
098100970613     C     *LIKE         DEFINE    CPCNAR        DEPNA1                         *DEPOSITI
098200970613     C     *LIKE         DEFINE    CPCNAR        DEPNA2
098300970613     C     *LIKE         DEFINE    CPTTTC        DEPTTC
098400970613     C     *LIKE         DEFINE    CPLLOC        DEPDPR
098500970613     C     *LIKE         DEFINE    CPCNAR        ROTNAR                         *ROTTURE
098600970613     C     *LIKE         DEFINE    CPCPRV        ROTPRV
098700970613     C     *LIKE         DEFINE    CPCLNA        ROTLNA
098800970613     C*
098900970613     C* CHIAVI DI LETTURA
099000970613     C* TABEL00F - PARZIALE
099100970613     C     KE2TAB        KLIST
099200970613     C                   KFLD                    KTBKUT                         *UTENTE
099300970613     C                   KFLD                    KTBCOD                         *TABELLA
099400970613     C* AZCPC01L - COMPLETA
099500970613     C     KEYCPC        KLIST
099600970613     C                   KFLD                    KCCVER                         *VERSIONE
099700970613     C                   KFLD                    KCCNAR                         *NAZIONE
099800970613     C                   KFLD                    KCCCAP                         *CAP
099900970613     C* AZCPC02L - COMPLETA
100000970613     C     KE2CPC        KLIST
100100970613     C                   KFLD                    KCCVER                         *VERSIONE
100200970613     C                   KFLD                    KCCNAR                         *NAZIONE
100300970613     C                   KFLD                    KCCPRV                         *PROVINCIA
100400970613     C                   KFLD                    KCCCAP                         *CAP
100500970613     C* AZCPC03L - COMPLETA
100600970613     C     KE3CPC        KLIST
100700970613     C                   KFLD                    KCCVER                         *VERSIONE
100800970613     C                   KFLD                    KCCNAR                         *NAZIONE
100900970613     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
101000970613     C                   KFLD                    KCCCAP                         *CAP
101100970613     C* AZCPE01L - PARZIALE
101200970613     C     KEYCPE        KLIST
101300970613     C                   KFLD                    KCEVER                         *VERSIONE
101400970613     C                   KFLD                    KCENAR                         *NAZIONE
101500970613     C                   KFLD                    KCECAP                         *CAP
101600970613     C* AZCPT01L - PARZIALE
101700970613     C     KEYCPT        KLIST
101800970613     C                   KFLD                    KCTVER                         *VERSIONE
101900970613     C                   KFLD                    KCTTTC                         *TIPO TEMPO
102000970613     C* AZCPL01L - PARZIALE
102100970613     C     KEYCPL        KLIST
102200970613     C                   KFLD                    KCLVER                         *VERSIONE
102300970613     C                   KFLD                    KCLNAR                         *NAZIONE
102400970613     C                   KFLD                    KCLCAP                         *CAP
102500970613     C*
102600970613     C* VARIABILI DI CONTROLLO
102700970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
102800970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
102900970613     C                   MOVEL     'N'           WSTCAP            1            *CAP DA STAMPARE
103000970613     C                   MOVEL     'N'           WSTECC            1            *ECC DA STAMPATE
103100970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
103200970613     C*
103300970613     C* REPERIMENTO DATA E ORA CORRENTE
103400970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
103500970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
103600970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
103700970613     C*
103800970613     C* CARICA LE TABELLE OCCORRENTI
103900970613     C                   EXSR      CARTAB
104000970613     C*
104100970613     C                   ENDSR
104200970613     C*------------------------------------------------------------------------*
104300970613**   DEC - DECODIFICHE
104400970613CAP                                                         1
104500970613PROVINCIA                                                   2
104600970613LINEA DI ARRIVO                                             3
104700970613INTERNI                                                     4
104800970613CLIENTE                                                     5
104900970613TUTTI                                                       6
105000970613LOCALITA' DELIVERY                                          7
105100970613ITALIA                                                      8
1052009706139
10530097061310
