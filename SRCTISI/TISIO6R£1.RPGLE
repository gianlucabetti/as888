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
006100091008     d
006200091008     d dvpodeco      e ds
006300091008     d Tibs02Ds      e ds
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
008100061016     ***D  DECLOC                 1     19
008200061016     ***D  DEDMIX                 1     19
008300061017     d  DECloc                 1     35
008400061017     d  DEDmix                 1     35
008500970613     D*-------------------
008600970613     D* DS PARAMETRI LANCIO
008700970613     D*-------------------
008800970613     D PARAM           DS
008900970613     D  PARTOR                 1      1
009000970613     D  PARCA1                 2     10
009100970613     D  PARCA2                11     19
009200970613     D  PARNA1                20     22
009300970613     D  PARNA2                23     25
009400970613     D  PARPR1                26     27
009500970613     D  PARPR2                28     29
009600970613     D  PARLN1                30     32  0
009700970613     D  PARLN2                33     35  0
009800970613     D  PARISO                36     36
009900970613     D  PARTTC                37     41  0
010000970613     D  PARTEM                42     42
010100970613     D  PARTIC                43     43
010200970613     D  PARLOC                44     44
010300970613     D  PARLIV                45     45
010400970613     D  PARVER                46     50  0
010500970613     D  PARDRI                51     58  0
010600971003     D  PAROBS                59     59
010700970613     D*-------------------
010800970613     D* ARCHITETTURA
010900970613     D*-------------------
011000970613     D KPJBA         E DS
011100061016      * - Parametri x Controllo profilo utenti
011200061016     d TIBS34ds      e ds
011300061016      * - Ds di riferimento al file esterno AZUTE00F
011400061016     d AZUTEds       e ds                  extname(AZUTE00F)
011500061016      * - Ds per dati organigramma
011600061016     d dDATIUTE      e ds
011700090605
011800090605     d ds5e          e ds
011900091008     d
012000091008     d dataiso         s               d   datfmt(*iso)
012100091008     d datasys         s               d   inz(*sys)
012200970613     C*------------------------------------------------------------------------*
012300970613     C* MAIN LINE
012400970613     C*------------------------------------------------------------------------*
012500970613     C*
012600970613     C* STAMPA PAGINA PARAMETRI DI LANCIO
012700970613     C                   EXSR      STAPAR
012800970613     C*
012900970613     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013000970613     C                   EXSR      SETFIL
013100970613     C*
013200970613     C* CICLO FINO A FINE FILE
013300970613DO  1C     FINL00        DOWEQ     '0'
013400970613     C*
013500970613     C* CONTROLLA LE ROTTURE PER SALTO PAGINA
013600970613     C                   EXSR      CHKROT
013700970613     C*
013800970613     C* STAMPA RIGA DETTAGLIO -CAP-
013900970613     C                   EXSR      STACAP
014000970613     C*
014100970613     C* CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
014200970613     C                   EXSR      LOPCPL
014300970613     C*
014400970613     C* LEGGE IL RECORD SUCCESSIVO
014500970613     C                   EXSR      LETFIL
014600970613E   1C                   ENDDO                                                  *FINE CAP
014700970613     C*
014800970613     C* STAMPA RIGA DI FINE PROSPETTO
014900970613     C                   WRITE     SI05RI                                       *RIGA SEPARAZIONE
015000970613     C                   WRITE     SI05FI                                       *RIGA FINE STAMPA
015100970613     C*
015200970613     C                   SETON                                        LR
015300970613     C*------------------------------------------------------------------------*
015400970613     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
015500970613     C*------------------------------------------------------------------------*
015600970613     C     SETFIL        BEGSR
015700970613     C*
015800970613     C* IMPOSTA LA NAZIONE
015900970613     C                   MOVEL     PARNA1        DEPNA1
016000970613IF  1C     PARNA1        IFEQ      'I  '
016100970613     C                   MOVEL     *BLANKS       DEPNA1
016200970613E   1C                   ENDIF
016300970613     C*
016400970613     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
016500970613IF  1C     PARTOR        IFEQ      'C'                                          *PER CAP
016600970613     C                   Z-ADD     PARVER        KCCVER
016700970613     C                   MOVEL     DEPNA1        KCCNAR
016800970613     C                   MOVEL     PARCA1        KCCCAP
016900970613     C     KEYCPC        SETLL     AZCPC01L                           99
017000970613E   1C                   ENDIF
017100970613     C*
017200970613IF  1C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
017300970613     C                   Z-ADD     PARVER        KCCVER
017400970613     C                   MOVEL     DEPNA1        KCCNAR
017500970613     C                   MOVEL     PARPR1        KCCPRV
017600970613     C                   MOVEL     PARCA1        KCCCAP
017700970613     C     KE2CPC        SETLL     AZCPC02L                           99
017800970613E   1C                   ENDIF
017900970613     C*
018000970613IF  1C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
018100970613     C                   Z-ADD     PARVER        KCCVER
018200970613     C                   MOVEL     DEPNA1        KCCNAR
018300010730     C*---               Z-ADD     PARLN1        KCCLNA
018400010730     C*---               MOVEL     PARCA1        KCCCAP
018500010730     C                   Z-ADD     *ZEROS        KCCLNA
018600010730     C                   MOVEL     *BLANKS       KCCCAP
018700970613     C     KE3CPC        SETLL     AZCPC03L                           99
018800970613E   1C                   ENDIF
018900970613     C*
019000970613     C* LETTURA RECORD IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
019100970613IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
019200970613     C                   EXSR      LETFIL                                       *LETTURA RECORD
019300970613X   1C                   ELSE                                                   *FINE FILE
019400970613     C                   MOVEL     '1'           FINL00                         *FINE PGM
019500970613E   1C                   ENDIF
019600970613     C*
019700970613     C                   ENDSR
019800970613     C*------------------------------------------------------------------------*
019900970613     C* LETFIL - LETTURA SUCESSIVA RECORD
020000970613     C*------------------------------------------------------------------------*
020100970613     C     LETFIL        BEGSR
020200970613     C*
020300970613     C* LEGGE FINO A:
020400970613     C*      -FINE FILE
020500970613     C*      -TROVATO RECORD VALIDO
020600970613     C                   MOVEL     'N'           WRECOK
020700970613DO  1C     *IN99         DOWEQ     *OFF
020800970613     C     WRECOK        ANDEQ     'N'
020900970613IF  2C     PARTOR        IFEQ      'C'                                          *PER CAP
021000980603     C     KCCVER        READE     AZCPC01L                               99
021100970613E   2C                   ENDIF
021200970613     C*
021300970613IF  2C     PARTOR        IFEQ      'P'                                          *PER PROVINCIA
021400980603     C     KCCVER        READE     AZCPC02L                               99
021500970613E   2C                   ENDIF
021600970613     C*
021700970613IF  2C     PARTOR        IFEQ      'L'                                          *PER LINEA ARRIVO
021800980603     C     KCCVER        READE     AZCPC03L                               99
021900970613E   2C                   ENDIF
022000970613     C*
022100970613IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
022200970613     C                   MOVEL     '1'           FINL00                         *FINE PROGRAMMA
022300970613X   2C                   ELSE                                                   *RECORD TROVATO
022400970613     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
022500970613E   2C                   ENDIF
022600970613E   1C                   ENDDO
022700970613     C*
022800970613     C                   ENDSR
022900970613     C*------------------------------------------------------------------------*
023000970613     C* CHKREC - CONTROLLA VALIDITA' RECORD
023100970613     C*------------------------------------------------------------------------*
023200970613     C     CHKREC        BEGSR
023300970613     C*
023400970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
023500010730     C                   MOVEL     'N'           $LNLOC            1            *SI LOC.LINEE SELEZ.
023600970613     C*
023700971003     C* CAP ANNULLATO, ESCLUDE
023800970613IF  1C     CPCATB        IFNE      *BLANKS
023900970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024000970613E   1C                   ENDIF
024100970613     C*
024200970613     C* NAZIONE <> DA QUELLE SELEZIONATE, ESCLUDE
024300970613IF  1C     PARNA1        IFNE      *BLANKS
024400970613IF  2C     PARNA1        IFEQ      'I  '
024500970613     C                   MOVEL     *BLANKS       DEPNA1
024600970613     C                   MOVEL     *BLANKS       DEPNA2
024700970613X   2C                   ELSE
024800970613     C                   MOVEL     PARNA1        DEPNA1
024900970613     C                   MOVEL     PARNA2        DEPNA2
025000970613E   2C                   ENDIF
025100970613IF  2C     CPCNAR        IFGE      DEPNA1
025200970613     C     CPCNAR        ANDLE     DEPNA2
025300970613X   2C                   ELSE
025400970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025500970613E   2C                   ENDIF
025600970613E   1C                   ENDIF
025700970613     C*
025800970613     C* CAP <> DA QUELLI SELEZIONATI, ESCLUDE
025900970613IF  1C     PARCA1        IFNE      *BLANKS
026000970613IF  2C     CPCCAP        IFGE      PARCA1
026100970613     C     CPCCAP        ANDLE     PARCA2
026200970613X   2C                   ELSE
026300970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026400970613E   2C                   ENDIF
026500970613E   1C                   ENDIF
026600970613     C*
026700970613     C* PROVINCIA <> DA QUELLE SELEZIONATE, ESCLUDE
026800970613IF  1C     PARPR1        IFNE      *BLANKS
026900970613IF  2C     CPCPRV        IFGE      PARPR1
027000970613     C     CPCPRV        ANDLE     PARPR2
027100970613X   2C                   ELSE
027200970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027300970613E   2C                   ENDIF
027400970613E   1C                   ENDIF
027500971003     C*
027600971003     C* CAP OBSOLETO E RICHIESTI I NON OBSOLETI, ESCLUDE
027700971003IF  1C     PAROBS        IFEQ      'N'
027800971003     C     CPCFI1        ANDEQ     'O'
027900971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028000971003E   1C                   ENDIF
028100010730     C*
028200010730     c* se scelta una linea controlla che sia nei CAP che nelle LOCALITA' ci siano record
028300010730     c* che abbiano la linea STANDARD o la OLTRE o la OLTRE 2 uguale a quella selezionata
028400010730     C                   MOVEL     'N'           $LNLOC                         *LOC.LINEE SELEZ.
028500010730IF  1C     WRECOK        IFEQ      'S'                                          *CAP ANCORA VALIDO
028600010730IF  2C     PARLN1        IFGT      *ZEROS
028700010730     C*
028800010730     C* solo se esistono eccezioni a livello di località
028900010730IF  3C     CPCESL        IFEQ      'S'                                          *LOC.PARTICOLARI
029000010730     C                   EXSR      CLNLOC
029100010730E   3C                   ENDIF
029200010730IF  3C                   IF        (CPCLNA >= PARLN1 AND CPCLNA <= PARLN2) OR
029300010730     C                             (CPCLOL >= PARLN1 AND CPCLOL <= PARLN2) OR
029400010730     C                             (CPCLO2 >= PARLN1 AND CPCLO2 <= PARLN2) OR
029500010730     C                             $LNLOC = 'S'                                 *SI CHK X LINEE LOC
029600010730X   3C                   ELSE
029700010730     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029800010730E   3c                   ENDIF
029900010730E   2C                   ENDIF
030000010730E   1C                   ENDIF
030100970613     C*
030200970613     C                   ENDSR
030300010730     C*--------------------------------------------------------------------------------------------*
030400010730     C* CLNLOC - CONTROLLA CHE LE LOCALITA' DEL CAP ABBIANO LINEE FRA QUELLE SELEZIONATE
030500010730     C*--------------------------------------------------------------------------------------------*
030600010730     C     CLNLOC        BEGSR
030700010730     C*
030800010730     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
030900010730     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
031000010730     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
031100010730     C     KEYCPL        SETLL     AZCPL01L
031200010730     C     KEYCPL        READE     AZCPL01L                               98
031300010730DO  1C     *IN98         DOWEQ     *OFF
031400010730IF  2C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
031500010730     C     CPLLIV        ANDEQ     'L'                                          *LOC. PARTICOLARE
031600010730     C*
031700010730     C* CONTROLLA LE LINEE FRA QUELLE SELEZIONATE
031800010730IF  3C                   IF        (CPLLNA >= PARLN1 AND CPLLNA <= PARLN2) OR
031900010730     C                             (CPLLOL >= PARLN1 AND CPLLOL <= PARLN2) OR
032000010730     C                             (CPLLO2 >= PARLN1 AND CPLLO2 <= PARLN2)
032100010730     C                   MOVEL     'S'           $LNLOC                         *SI LOC.LINEE SELEZ.
032200010730     C                   LEAVE
032300010730E   3C                   ENDIF
032400010730E   2C                   ENDIF
032500010730     C*
032600010730     C     KEYCPL        READE     AZCPL01L                               98
032700010730E   1C                   ENDDO                                                  *FINE LOCALITA' CAP
032800010730     C*
032900010730     C                   ENDSR
033000970613     C*------------------------------------------------------------------------*
033100971003     C* CHKROT - CONTROLLA LE ROTTURE PER SALTO PAGINA
033200970613     C*------------------------------------------------------------------------*
033300970613     C     CHKROT        BEGSR
033400970613     C*
033500970613     C* ROTTURA NAZIONE -SEMPRE-
033600970613IF  1C     CPCNAR        IFNE      ROTNAR
033700970613     C                   MOVEL     CPCNAR        ROTNAR                         *DEPOSITO ROTTURA
033800970613     C                   MOVEL     CPCNAR        INCNAR                         *PER INTESTAZIONE
033900970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034000970613E   1C                   ENDIF
034100970613     C*
034200970613     C* ROTTURA PROVINCIA -SE ORDINAMENTO PER PROVINCIA-
034300970613IF  1C     PARTOR        IFEQ      'P'
034400970613IF  2C     CPCPRV        IFNE      ROTPRV
034500970613     C                   MOVEL     CPCPRV        ROTPRV                         *DEPOSITO ROTTURA
034600970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
034700970613E   1C                   ENDIF
034800970613E   1C                   ENDIF
034900970613     C*
035000970613     C* ROTTURA LINEA DI ARRIVO -SE ORDINAMENTO PER LINEA DI ARRIVO-
035100970613IF  1C     PARTOR        IFEQ      'L'
035200970613IF  2C     CPCLNA        IFNE      ROTLNA
035300970613     C                   MOVEL     CPCLNA        ROTLNA                         *DEPOSITO ROTTURA
035400970613     C                   MOVEL     '1'           *IN01                          *SALTO PAGINA
035500970613E   1C                   ENDIF
035600970613E   1C                   ENDIF
035700970613     C*
035800970613     C                   ENDSR
035900970613     C*------------------------------------------------------------------------*
036000970613     C* STACAP - STAMPA RIGA DI DETTAGLIO -CAP-
036100970613     C*------------------------------------------------------------------------*
036200970613     C     STACAP        BEGSR
036300970613     C*
036400970613     C* REIMPOSTA LE VARIABILI DI LAVORO
036500970613     C                   MOVEL     'N'           WSTCAP                         *CAP NON STAMPATO
036600970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
036700970613     C*
036800970613     C* CONTROLLA SE IL CAP E' DA STAMPARE
036900970613     C                   EXSR      CHKCAP
037000970613IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
037100970613     C* CAMPI
037200970613     C                   MOVEL     CPCCAP        DECCAP
037300970613     C                   MOVEL     CPCFLG        DECFLG
037400970917     C                   MOVEL     CPCFI1        DECFI1
037500970613     C                   MOVEL     *BLANKS       DEDMIX
037600970613     C                   MOVEL     CPCPRV        DECPRV
037700970613     C                   MOVEL     CPCPRA        DECPRA
037800970613IF  2C     CPCPRA        IFNE      *BLANKS
037900970613     C                   MOVEL     '/'           DEDPRX
038000970613E   2C                   ENDIF
038100970613     C                   Z-ADD     CPCLNA        DECLNA
038200970613     C                   MOVEL     CPCZNC        DECZNC
038300970613     C                   MOVEL     CPCZFD        DECZFD
038400970613IF  2C     CPCLKS        IFGT      *ZEROS
038500000518     C     CPCLMS        ORGT      *ZEROS
038600970613     C                   MOVEL     CPCLOS        DECLOS
038700970613     C                   MOVEL     CPCZOS        DECZOS
038800970613     C                   Z-ADD     CPCLKS        DECLKS
038900970613     C                   Z-ADD     CPCLMS        DECLMS
039000970613E   2C                   ENDIF
039100970613IF  2C     CPCLKG        IFGT      *ZEROS
039200970613     C     CPCLMC        ORGT      *ZEROS
039300970613     C                   MOVEL     CPCLOL        DECLOL
039400970613     C                   MOVEL     CPCZOL        DECZOL
039500970613     C                   Z-ADD     CPCLKG        DECLKG
039600970613     C                   Z-ADD     CPCLMC        DECLMC
039700970613E   2C                   ENDIF
039800971218     C*
039900970613     C                   MOVEL     CPCCTS        DECCTS
040000970613     C                   MOVEL     CPCISO        DECINL
040100010727     C                   Z-ADD     CPCTTC        DECTTC
040200010727     C                   MOVEL     *ALL'9'       DECTTR
040300010727     C                   MOVE      CPCFI2        DECTTR
040400970613     C*
040500970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN DEL CAP
040600970613IF  2C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
040700970613     C                   Z-ADD     CPCTTC        DEPTTC                         *TIPO TEMPO
040800970613     C                   EXSR      LOPCPT
040900970613E   2C                   ENDIF
041000970613     C*
041100970613     C* STAMPA RIGA DI DETTALGIO
041200970613     C                   MOVEL     'S'           WSTCAP                         *CAP STAMPATO
041300970613     C                   EXSR      STMDET
041400970613     C*
041500970613     C* CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
041600970613     C                   EXSR      LOPCPE
041700970613E   1C                   ENDIF
041800970613     C*
041900970613     C                   ENDSR
042000970613     C*------------------------------------------------------------------------*
042100970613     C* CHKCAP - CONTROLLA SE IL CAP E' DA STAMPARE
042200970613     C*------------------------------------------------------------------------*
042300970613     C     CHKCAP        BEGSR
042400970613     C*
042500970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
042600970613     C*
042700970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
042800970613IF  1C     PARISO        IFNE      *BLANKS
042900970613IF  2C     CPCISO        IFNE      PARISO
043000970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043100970613E   2C                   ENDIF
043200970613E   1C                   ENDIF
043300970613     C*
043400970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
043500970613IF  1C     PARTTC        IFGT      *ZEROS
043600970613IF  2C     CPCTTC        IFNE      PARTTC
043700970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043800970613E   2C                   ENDIF
043900970613E   1C                   ENDIF
044000970613     C*
044100970613     C                   ENDSR
044200970613     C*------------------------------------------------------------------------*
044300970613     C* LOPCPE - CICLO DI LETTURA E STAMPA DELLE ECCEZIONI LINEA/ZONA DEL CAP
044400970613     C*------------------------------------------------------------------------*
044500970613     C     LOPCPE        BEGSR
044600970613     C*
044700970613     C* REIMPOSTA LE VARIABILI DI LAVOR
044800970613     C                   MOVEL     'N'           WSTECC                         *ECC NON STAMPATO
044900970613     C*
045000970613IF  1C     CPCESE        IFEQ      'S'                                          *ESISTONO ECCEZIONI
045100970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
045200970613     C                   Z-ADD     CPCVER        KCEVER                         *VERSIONE
045300970613     C                   MOVEL     CPCNAR        KCENAR                         *NAZIONE
045400970613     C                   MOVEL     CPCCAP        KCECAP                         *CAP
045500970613     C     KEYCPE        SETLL     AZCPE01L
045600970613     C     KEYCPE        READE     AZCPE01L                               98
045700970613DO  2C     *IN98         DOWEQ     *OFF
045800970613IF  3C     CPEATB        IFEQ      *BLANKS                                      *VALIDI
045900970613     C* CAMPI
046000970613     C                   MOVEL     *BLANKS       DEDMIX
046100970613     C                   MOVEL     CPETFP        DECTFP
046200970613     C                   Z-ADD     CPELNA        DECLNA
046300970613     C                   MOVEL     CPEZNC        DECZNC
046400970613IF  4C     CPCLKS        IFGT      *ZEROS
046500970613     C     CPCLMS        ORGT      *ZEROS
046600970613     C                   MOVEL     CPELOS        DECLOS
046700970613     C                   MOVEL     CPEZOS        DECZOS
046800970613E   4C                   ENDIF
046900970613IF  4C     CPCLKG        IFGT      *ZEROS
047000970613     C     CPCLMC        ORGT      *ZEROS
047100970613     C                   MOVEL     CPELOL        DECLOL
047200970613     C                   MOVEL     CPEZOL        DECZOL
047300970613E   4C                   ENDIF
047400970613     C* DECODIFICHE
047500970613     C                   Z-ADD     1             I
047600970613     C                   SETOFF                                           98
047700970613     C     CPETFP        LOOKUP    ORG(I)                                 98
047800970613IF  4C     *IN98         IFEQ      *ON
047900970613     C                   MOVEL     DORG(I)       DEDTFP                         *TERMINAL PARTENZA
048000970613E   4C                   ENDIF
048100970613     C*
048200970613     C* STAMPA RIGA DI DETTALGIO
048300970613     C                   MOVEL     'S'           WSTECC                         *ECCEZIONI STAMPATE
048400970613     C                   EXSR      STMDET
048500970613E   3C                   ENDIF
048600970613     C     KEYCPE        READE     AZCPE01L                               98
048700970613E   2C                   ENDDO                                                  *FINE ECCEZIONI
048800970613E   1C                   ENDIF
048900970613     C*
049000970613     C                   ENDSR
049100970613     C*------------------------------------------------------------------------*
049200970613     C* LOPCPL - CICLO DI LETTURA E STAMPA DELLE LOCALITA' DEL CAP
049300970613     C*------------------------------------------------------------------------*
049400970613     C     LOPCPL        BEGSR
049500970613     C*
049600970613     C* DEPOSITA LA DESCRIZIONE DELLA PROVINCIA DEL CAP
049700970613     C                   Z-ADD     1             I
049800970613     C                   SETOFF                                           98
049900970613     C     CPCPRV        LOOKUP    PRV(I)                                 98
050000970613IF  1C     *IN98         IFEQ      *ON
050100970613     C                   MOVEL     DPRV(I)       DEPDPR
050200970613X   1C                   ELSE
050300970613     C                   MOVEL     *BLANKS       DEPDPR
050400970613E   1C                   ENDIF
050500970613     C*
050600970613     C* SE RICHIESTI IN STAMPA, LEGGE LE LOCALITA'
050700010730IF  1c     PARLOC        IFNE      'N'
050800970613     C                   Z-ADD     CPCVER        KCLVER                         *VERSIONE
050900970613     C                   MOVEL     CPCNAR        KCLNAR                         *NAZIONE
051000970613     C                   MOVEL     CPCCAP        KCLCAP                         *CAP
051100970613     C     KEYCPL        SETLL     AZCPL01L
051200970613     C     KEYCPL        READE     AZCPL01L                               98
051300970613DO  2C     *IN98         DOWEQ     *OFF
051400970613IF  3C     CPLATB        IFEQ      *BLANKS                                      *VALIDI
051500970613IF  4C     PARLOC        IFEQ      'D'                                          *SELEZ.LOC.DELIVERY
051600970613     C     CPLDLV        ANDEQ     'S'
051700970613     C     PARLOC        ORNE      'D'                                          *SELEZ.TUTTE
051800970613IF  5C     PARLIV        IFEQ      'S'                                          *SELEZ.LOC <> CAP
051900970613     C     CPLLIV        ANDEQ     'L'
052000970613     C     PARLIV        OREQ      'N'                                          *SELEZ.TUTTE
052100970613     C*
052200990525     C* NON STAMPA LA LOCALITA' SE E' UGUALE ALLA DESCRIZIONE DELLA PROVINCIA E
052300990525     C* SE E' GESTITA A LIVELLO DI LOCALITA'
052400970613IF  6C     DEPDPR        IFNE      CPLLOC                                       *-PEZZA-
052500990525     C     DEPDPR        OREQ      CPLLOC
052600990525     C     CPLLIV        ANDEQ     'L'
052700970613     C*
052800970613     C* CONTROLLA SE LA LOCALITA' E' DA STAMPARE
052900970613     C                   EXSR      CHKLOC
053000970613IF  7C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
053100970613     C* CAMPI
053200970613IF  8C     WSTECC        IFEQ      'S'                                          *ECCEZIONI STAMPATE
053300970613     C                   WRITE     SI05R2                                       *STAMPA RIGA SEPARAZ
053400970613     C                   MOVEL     'N'           WSTECC                         *X NON STAMPARE +RIG
053500970613E   8C                   ENDIF
053600970613     C                   CLEAR                   SI05DE                         *PULISCE FORMATO
053700970613IF  8C     WSTCAP        IFEQ      'N'                                          *CAP NON STMPATO
053800970613     C                   MOVEL     CPCCAP        DECCAP                         *RIEMPE CAMPI CAP
053900970613E   8C                   ENDIF
054000970917     C                   MOVEL     CPLFI1        DECFI1
054100970613     C                   MOVEL     *BLANKS       DEDMIX
054200970613     C                   MOVEL     CPLLOC        DECLOC
054300000529     C                   MOVEL     CPLLOC        SAVLOC           35
054400970613IF  8C     CPLLIV        IFEQ      'L'                                          *LOC.GEST.LIV.LOC.
054500970613     C                   Z-ADD     CPLLNA        DECLNA
054600970613     C                   MOVEL     CPLZNC        DECZNC
054700970613     C                   MOVEL     CPLZFD        DECZFD
054800970621IF  9C     CPLLKS        IFGT      *ZEROS
054900970621     C     CPLLMS        ORGT      *ZEROS
055000970613     C                   MOVEL     CPLLOS        DECLOS
055100970613     C                   MOVEL     CPLZOS        DECZOS
055200970613     C                   Z-ADD     CPLLKS        DECLKS
055300970613     C                   Z-ADD     CPLLMS        DECLMS
055400970613E   9C                   ENDIF
055500970621IF  9C     CPLLKG        IFGT      *ZEROS
055600970621     C     CPLLMC        ORGT      *ZEROS
055700970613     C                   MOVEL     CPLLOL        DECLOL
055800970613     C                   MOVEL     CPLZOL        DECZOL
055900970613     C                   Z-ADD     CPLLKG        DECLKG
056000970613     C                   Z-ADD     CPLLMC        DECLMC
056100970613E   9C                   ENDIF
056200000518     C*
056300970613     C                   MOVEL     CPLCTS        DECCTS
056400970613     C                   MOVEL     CPLISO        DECINL
056500010727     C                   Z-ADD     CPLTTC        DECTTC
056600010727     C                   MOVEL     *ALL'9'       DECTTR
056700010727     C                   MOVE      CPLFI2        DECTTR
056800970613     C*
056900970613     C* CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGNA DELLA LOCALITA'
057000970613IF  9C     PARTEM        IFEQ      'S'                                          *SE RICHIESTI
057100970613     C                   Z-ADD     CPLTTC        DEPTTC                         *TIPO TEMPO
057200970613     C                   EXSR      LOPCPT
057300970613E   9C                   ENDIF
057400970613E   8C                   ENDIF
057500970613     C*
057600970613     C* STAMPA RIGA DI DETTALGIO
057700970613     C                   EXSR      STMDET
057800970613E   7C                   ENDIF
057900970613E   6C                   ENDIF
058000970613E   5C                   ENDIF
058100970613E   4C                   ENDIF
058200970613E   3C                   ENDIF
058300970613     C*
058400970613     C     KEYCPL        READE     AZCPL01L                               98
058500970613E   2C                   ENDDO                                                  *FINE LOCALITA'
058600970613E   1C                   ENDIF
058700970613     C*
058800970613     C                   ENDSR
058900970613     C*------------------------------------------------------------------------*
059000970613     C* CHKLOC - CONTROLLA SE LA LOCCALITA' E' DA STAMPARE
059100970613     C*------------------------------------------------------------------------*
059200970613     C     CHKLOC        BEGSR
059300970613     C*
059400970613     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
059500971003     C*
059600971003     C* LOCALITA' OBSOLETA E RICHIESTI LE NON OBSOLETE, ESCLUDE
059700971003IF  1C     PAROBS        IFEQ      'N'
059800971003     C     CPLFI1        ANDEQ     'O'
059900971003     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
060000971003E   1C                   ENDIF
060100970613     C*
060200970613     C* TIPO INOLTRO <> DA QUELLO SELEZIONATO, ESCLUDE
060300971107IF  1C*---       PARISO    IFNE *BLANKS
060400971107IF  2C*---       CPLISO    IFEQ PARISO
060500971107     C*---       CPCISO    OREQ PARISO
060600971107X   2C*---                 ELSE
060700971107     C*---                 MOVEL'N'       WRECOK           *RECORD NON VALIDO
060800971107E   2C*---                 ENDIF
060900971107E   1C*---                 ENDIF
061000971107     C*
061100971107IF  1C     PARISO        IFNE      *BLANKS
061200971107IF  2C     CPLISO        IFEQ      PARISO
061300971107     C     CPLLIV        OREQ      'C'
061400971107     C     CPCISO        ANDEQ     PARISO
061500971107X   2C                   ELSE
061600971107     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061700971107E   2C                   ENDIF
061800971107E   1C                   ENDIF
061900970613     C*
062000970613     C* TIPO TEMPO CONSEGNA <> DA QUELLO SELEZIONATO, ESCLUDE
062100970613IF  1C     PARTTC        IFGT      *ZEROS
062200970613IF  2C     CPLTTC        IFNE      PARTTC
062300970613     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
062400970613E   2C                   ENDIF
062500970613E   1C                   ENDIF
062600970613     C*
062700970613     C                   ENDSR
062800970613     C*------------------------------------------------------------------------*
062900970613     C* LOPCPT- CICLO DI LETTURA E STAMPA DEI TEMPI DI CONSEGN
063000970613     C*------------------------------------------------------------------------*
063100970613     C     LOPCPT        BEGSR
063200970613     C*
063300970613     C                   Z-ADD     CPCVER        KCTVER                         *VERSIONE
063400970613     C                   Z-ADD     DEPTTC        KCTTTC                         *TIPO TEMPO
063500970613     C     KEYCPT        SETLL     AZCPT01L
063600970613     C     KEYCPT        READE     AZCPT01L                               98
063700970613DO  1C     *IN98         DOWEQ     *OFF
063800970613IF  2C     CPTATB        IFEQ      *BLANKS                                      *VALIDI
063900970613IF  3C     CPTTSP        IFEQ      'D'                                          *DISTRIBUZIONE
064000970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
064100970613     C     PARTIC        OREQ      'T'
064200970613     C                   Z-ADD     CPT0IN        DECD0I                         *DIS INTERNO 00
064300970613     C                   Z-ADD     CPT1IN        DECD1I                         *DIS INTERNO 12
064400970613E   4C                   ENDIF
064500970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
064600970613     C     PARTIC        OREQ      'T'
064700970613     C                   Z-ADD     CPT0CN        DECD0C                         *DIS CLIENTE 00
064800970613     C                   Z-ADD     CPT1CN        DECD1C                         *DIS CLIENTE 12
064900970613E   4C                   ENDIF
065000970613     C                   Z-ADD     CPTTIS        DECDIS                         *DIS ISOLA
065100970613     C                   Z-ADD     CPTTLD        DECDLD                         *DIS L.DISAGIATA
065200970613     C                   Z-ADD     CPTTCS        DECDCS                         *DIS C.STORICO
065300970613E   3C                   ENDIF
065400970613     C*
065500970613IF  3C     CPTTSP        IFEQ      'C'                                          *CORRIERE
065600970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
065700970613     C     PARTIC        OREQ      'T'
065800970613     C                   Z-ADD     CPT0IN        DECC0I                         *COR INTERNO 00
065900970613     C                   Z-ADD     CPT1IN        DECC1I                         *COR INTERNO 12
066000970613E   4C                   ENDIF
066100970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
066200970613     C     PARTIC        OREQ      'T'
066300970613     C                   Z-ADD     CPT0CN        DECC0C                         *COR CLIENTE 00
066400970613     C                   Z-ADD     CPT1CN        DECC1C                         *COR CLIENTE 12
066500970613E   4C                   ENDIF
066600970613     C                   Z-ADD     CPTTIS        DECCIS                         *COR ISOLA
066700970613     C                   Z-ADD     CPTTLD        DECCLD                         *COR L.DISAGIATA
066800970613     C                   Z-ADD     CPTTCS        DECCCS                         *COR C.STORICO
066900970613E   3C                   ENDIF
067000970613     C*
067100091008IF  3C     CPTTSP        IFEQ      'E'                                          *priority
067200970613IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
067300970613     C     PARTIC        OREQ      'T'
067400970613     C                   Z-ADD     CPT0IN        DECE0I                         *ESP INTERNO 00
067500970613     C                   Z-ADD     CPT1IN        DECE1I                         *ESP INTERNO 12
067600970613E   4C                   ENDIF
067700970613IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
067800970613     C     PARTIC        OREQ      'T'
067900970613     C                   Z-ADD     CPT0CN        DECE0C                         *ESP CLIENTE 00
068000970613     C                   Z-ADD     CPT1CN        DECE1C                         *ESP CLIENTE 12
068100970613E   4C                   ENDIF
068200970613     C                   Z-ADD     CPTTIS        DECEIS                         *ESP ISOLA
068300970613     C                   Z-ADD     CPTTLD        DECELD                         *ESP L.DISAGIATA
068400970613     C                   Z-ADD     CPTTCS        DECECS                         *ESP C.STORICO
068500970613E   3C                   ENDIF
068600091008
068700091008IF  3C     CPTTSP        IFEQ      'H'                                          *H 10 30
068800091008IF  4C     PARTIC        IFEQ      'I'                                          *SE RICHIESTI
068900091008     C     PARTIC        OREQ      'T'
069000091008     C                   Z-ADD     CPT0IN        DECh0I                         *"H" INTERNO 00
069100091008E   4C                   ENDIF
069200091008IF  4C     PARTIC        IFEQ      'C'                                          *SE RICHIESTI
069300091008     C     PARTIC        OREQ      'T'
069400091008     C                   Z-ADD     CPT0CN        DECh0C                         *"H" CLIENTE 00
069500091008E   4C                   ENDIF
069600091008E   3C                   ENDIF
069700091008
069800970613E   2C                   ENDIF
069900970613     C     KEYCPT        READE     AZCPT01L                               98
070000970613E   1C                   ENDDO                                                  *FINE TEMPI
070100970613     C*
070200970613     C                   ENDSR
070300970613     C*------------------------------------------------------------------------*
070400970613     C* STMDET - STAMPA RIGA DI DETTAGLIO
070500970613     C*------------------------------------------------------------------------*
070600970613     C     STMDET        BEGSR
070700970613     C*
070800970613     C* SE FINE PAGINA, STAMPA L'INTESTAZIONE
070900970613IF  1C     *IN01         IFEQ      *ON
071000970613     C                   WRITE     SI05TE                                       *TESTATA
071100970613     C                   WRITE     SI05RI                                       *RIGA
071200970613     C                   WRITE     SI05IN                                       *INTESTAZIONE
071300970613     C                   WRITE     SI05RI                                       *RIGA
071400970613     C                   SETOFF                                       01
071500970613E   1C                   ENDIF
071600000428     C*
071700000428     C* STAMPA DETTAGLIO
071800970613     C                   WRITE     SI05DE                                       *DETTAGLIO
071900000428     C     PARTEM        IFEQ      'S'
072000000428     C                   WRITE     SI05TM                                       *DETTAGLIO X TEMPIAR
072100000428     C                   ENDIF
072200970613     C*
072300970613     C                   ENDSR
072400970613     C*------------------------------------------------------------------------*
072500970613     C* STAPAR - STAMPA PAGINA PARAMETRI
072600970613     C*------------------------------------------------------------------------*
072700970613     C     STAPAR        BEGSR
072800970613     C* TESTATA
072900970613     C                   Z-ADD     PARDRI        G08INV                         *DATA RIFERIMENTO
073000970613     C                   Z-ADD     *ZEROS        G08DAT
073100970613     C                   MOVEL     '3'           G08ERR
073200970613     C                   CALL      'XSRDA8'
073300970613     C                   PARM                    WLBDA8
073400970613     C                   Z-ADD     G08DAT        TECDRI
073500970613     C* PARAMETRI
073600970613     C                   MOVEL     PARTOR        PACTOR                         *ORDINAMENTO
073700970613IF  1C     PARTOR        IFEQ      'C'
073800970613     C                   MOVEL     DEC(1)        PADTOR
073900970613E   1C                   ENDIF
074000970613IF  1C     PARTOR        IFEQ      'P'
074100970613     C                   MOVEL     DEC(2)        PADTOR
074200970613E   1C                   ENDIF
074300970613IF  1C     PARTOR        IFEQ      'L'
074400970613     C                   MOVEL     DEC(3)        PADTOR
074500970613E   1C                   ENDIF
074600970613     C*
074700970613IF  1C     PARNA1        IFNE      *BLANKS
074800970613     C                   MOVEL     PARNA1        PACNA1                         *NAZIONE DAL
074900970613IF  2C     PARNA1        IFEQ      'I'
075000970613     C                   MOVEL     DEC(8)        PADNA1                         *NAZIONE ITALIA
075100970613X   2C                   ELSE
075200970613     C                   Z-ADD     1             I
075300970613     C                   SETOFF                                           98
075400970613     C     PARNA1        LOOKUP    NAR(I)                                 98
075500970613IF  3C     *IN98         IFEQ      *ON
075600970613     C                   MOVEL     DNAR(I)       PADNA1
075700970613E   3C                   ENDIF
075800970613E   2C                   ENDIF
075900970613E   1C                   ENDIF
076000970613     C*
076100970613IF  1C     PARNA2        IFNE      *BLANKS
076200970613     C                   MOVEL     PARNA2        PACNA2                         *NAZIONE AL
076300970613IF  2C     PARNA2        IFEQ      'I'
076400970613     C                   MOVEL     DEC(8)        PADNA2                         *NAZIONE ITALIA
076500970613X   2C                   ELSE
076600970613     C                   Z-ADD     1             I
076700970613     C                   SETOFF                                           98
076800970613     C     PARNA2        LOOKUP    NAR(I)                                 98
076900970613IF  3C     *IN98         IFEQ      *ON
077000970613     C                   MOVEL     DNAR(I)       PADNA2
077100970613E   3C                   ENDIF
077200970613E   2C                   ENDIF
077300970613E   1C                   ENDIF
077400970613     C*
077500970613     C                   MOVEL     PARCA1        PACCA1                         *CAP DAL
077600970613     C                   MOVEL     PARCA2        PACCA2                         *CAP AL
077700970613     C*
077800970613IF  1C     PARPR1        IFNE      *BLANKS
077900970613     C                   MOVEL     PARPR1        PACPR1                         *PROVINCIA DAL
078000970613     C                   Z-ADD     1             I
078100970613     C                   SETOFF                                           98
078200970613     C     PARPR1        LOOKUP    PRV(I)                                 98
078300970613IF  2C     *IN98         IFEQ      *ON
078400970613     C                   MOVEL     DPRV(I)       PADPR1
078500970613E   2C                   ENDIF
078600970613E   1C                   ENDIF
078700970613     C*
078800970613IF  1C     PARPR2        IFNE      *BLANKS
078900970613     C                   MOVEL     PARPR1        PACPR2                         *PROVINCIA AL
079000970613     C                   Z-ADD     1             I
079100970613     C                   SETOFF                                           98
079200970613     C     PARPR2        LOOKUP    PRV(I)                                 98
079300970613IF  2C     *IN98         IFEQ      *ON
079400970613     C                   MOVEL     DPRV(I)       PADPR2
079500970613E   2C                   ENDIF
079600970613E   1C                   ENDIF
079700970613     C*
079800970613IF  1C     PARLN1        IFGT      *ZEROS
079900970613     C                   Z-ADD     PARLN1        PACLN1                         *LINEA ARRIVO DAL
080000970613     C                   Z-ADD     1             I
080100970613     C                   SETOFF                                           98
080200970613     C     PARLN1        LOOKUP    ORG(I)                                 98
080300970613IF  2C     *IN98         IFEQ      *ON
080400970613     C                   MOVEL     DORG(I)       PADLN1
080500970613E   2C                   ENDIF
080600970613E   1C                   ENDIF
080700970613     C*
080800970613IF  1C     PARLN2        IFGT      *ZEROS
080900970613     C                   Z-ADD     PARLN2        PACLN2                         *LINEA ARRIVO AL
081000970613     C                   Z-ADD     1             I
081100970613     C                   SETOFF                                           98
081200970613     C     PARLN2        LOOKUP    ORG(I)                                 98
081300970613IF  2C     *IN98         IFEQ      *ON
081400970613     C                   MOVEL     DORG(I)       PADLN2
081500970613E   2C                   ENDIF
081600970613E   1C                   ENDIF
081700970613     C*
081800970613IF  1C     PARISO        IFNE      *BLANKS
081900970613     C                   MOVEL     PARISO        PACISO                         *TIPO INOLTRO
082000970613     C                   Z-ADD     1             I
082100970613     C                   SETOFF                                           98
082200970613     C     PARISO        LOOKUP    INL(I)                                 98
082300970613IF  2C     *IN98         IFEQ      *ON
082400970613     C                   MOVEL     DINL(I)       PADISO
082500970613E   2C                   ENDIF
082600970613E   1C                   ENDIF
082700970613     C*
082800970613     C                   Z-ADD     PARTTC        PACTTC
082900970613     C*
083000970613     C                   MOVEL     PARTEM        PACTEM
083100970613     C*
083200970613     C                   MOVEL     PARTIC        PACTIC                         *TEMPI CONSEGNA
083300970613IF  1C     PARTIC        IFEQ      'I'
083400970613     C                   MOVEL     DEC(4)        PADTIC
083500970613E   1C                   ENDIF
083600970613IF  1C     PARTIC        IFEQ      'C'
083700970613     C                   MOVEL     DEC(5)        PADTIC
083800970613E   1C                   ENDIF
083900970613IF  1C     PARTIC        IFEQ      'T'
084000970613     C                   MOVEL     DEC(6)        PADTIC
084100970613E   1C                   ENDIF
084200970613     C*
084300970613     C                   MOVEL     PARLOC        PACLOC
084400970613IF  1C     PARLOC        IFEQ      'D'
084500970613     C                   MOVEL     DEC(7)        PADLOC
084600970613E   1C                   ENDIF
084700970613     C*
084800970613     C                   MOVEL     PARLIV        PACLIV
084900971003     C*
085000971003     C                   MOVEL     PAROBS        PACOBS
085100090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
085200090605      * 'C' - 'D' - 'E'
085300090605     c                   eval      ktbcod = '5E'
085400090605     c     ke2tab        setll     tabel00f
085500090605     c                   do        *hival
085600090605     c     ke2tab        reade     tabel00f
085700090605     c                   if        %eof(tabel00f)
085800090605     c                   leave
085900090605     c                   endif
086000090605     c                   eval      ds5e = tbluni
086100090605     c                   if        tblkey = 'C'
086200090605     c                   eval      dtspci = §5ed03
086300091008     c***                eval      dtspcl = §5ed03
086400090605     c                   eval      dtspcc = §5ed03
086500090605     c                   endif
086600090605     c                   if        tblkey = 'D'
086700090605     c                   eval      dtspdi = §5ed03
086800091008     c***                eval      dtspdl = §5ed03
086900090605     c                   eval      dtspdc = §5ed03
087000090605     c                   endif
087100090605     c                   if        tblkey = 'E'
087200090605     c                   eval      dtspei = §5ed03
087300091008     c***                eval      dtspel = §5ed03
087400090605     c                   eval      dtspec = §5ed03
087500090605     c                   endif
087600090605     c                   enddo
087700970613     C*
087800970613     C                   WRITE     SI05TE                                       *TESTATA
087900970613     C                   WRITE     SI05PA                                       *PARAMETRI
088000970613     C*
088100970613     C                   ENDSR
088200970613     C*------------------------------------------------------------------------*
088300970613     C* CARTAB - CARICA LE TABELLE
088400970613     C*------------------------------------------------------------------------*
088500970613     C     CARTAB        BEGSR
088600970613     C*--------------------
088700970613     C* FILIALI
088800970613     C*--------------------
088900970613     C                   Z-ADD     *ZEROS        I                 5 0
089000970613     C     *LOVAL        SETLL     AZORG01L
089100970613     C                   READ      AZORG01L                               99
089200970613DO  1C     *IN99         DOWEQ     *OFF
089300970613IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
089400970613     C                   ADD       1             I
089500970613     C                   Z-ADD     ORGFIL        ORG(I)                         *CODICE
089600970613IF  3C     ORGDE5        IFNE      *BLANKS
089700970613     C                   MOVEL     ORGDE5        DORG(I)                        *DESCRIZIONE + CORTA
089800970613X   3C                   ELSE
089900970613     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE + LUNGA
090000970613E   3C                   ENDIF
090100970613E   2C                   ENDIF
090200970613     C                   READ      AZORG01L                               99
090300970613E   1C                   ENDDO
090400970613     C*--------------------
090500970613     C* NAZIONI
090600970613     C*--------------------
090700970613     C                   Z-ADD     *ZEROS        I
090800020906     C                   Z-ADD     1             KTBKUT
090900970613     C                   MOVEL     '15'          KTBCOD
091000970613     C     KE2TAB        SETLL     TABEL00F
091100970613     C     KE2TAB        READE     TABEL00F                               99
091200970613DO  1C     *IN99         DOWEQ     *OFF
091300970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
091400970613     C                   ADD       1             I
091500970613     C                   MOVEL     TBLKEY        NAR(I)                         *CODICE
091600970613     C                   MOVEL     TBLUNI        DS15
091700970613     C                   MOVEL     §15DES        DNAR(I)                        *DESCRIZIONE
091800970613E   2C                   ENDIF
091900970613     C     KE2TAB        READE     TABEL00F                               99
092000970613E   1C                   ENDDO
092100970613     C*--------------------
092200970613     C* PROVINCE
092300970613     C*--------------------
092400970613     C                   Z-ADD     *ZEROS        I
092500020906     C                   Z-ADD     1             KTBKUT
092600970613     C                   MOVEL     'PR'          KTBCOD
092700970613     C     KE2TAB        SETLL     TABEL00F
092800970613     C     KE2TAB        READE     TABEL00F                               99
092900970613DO  1C     *IN99         DOWEQ     *OFF
093000970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
093100970613     C                   ADD       1             I
093200970613     C                   MOVEL     TBLKEY        PRV(I)                         *CODICE
093300970613     C                   MOVEL     TBLUNI        DSPR
093400970613     C                   MOVEL     §PRDES        DPRV(I)                        *DESCRIZIONE
093500970613E   2C                   ENDIF
093600970613     C     KE2TAB        READE     TABEL00F                               99
093700970613E   1C                   ENDDO
093800970613     C*--------------------
093900970613     C* TIPI INOLTRO
094000970613     C*--------------------
094100970613     C                   Z-ADD     *ZEROS        I
094200020906     C                   Z-ADD     1             KTBKUT
094300970613     C                   MOVEL     '9A'          KTBCOD
094400970613     C     KE2TAB        SETLL     TABEL00F
094500970613     C     KE2TAB        READE     TABEL00F                               99
094600970613DO  1C     *IN99         DOWEQ     *OFF
094700970613IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
094800970613     C                   ADD       1             I
094900970613     C                   MOVEL     TBLKEY        INL(I)                         *CODICE
095000970613     C                   MOVEL     TBLUNI        DS9A
095100970613     C                   MOVEL     §9ADES        DINL(I)                        *DESCRIZIONE
095200970613E   2C                   ENDIF
095300970613     C     KE2TAB        READE     TABEL00F                               99
095400970613E   1C                   ENDDO
095500970613     C*
095600970613     C                   ENDSR
095700970613     C*------------------------------------------------------------------------*
095800970613     C* *INZSR - OPERAZIONI INIZIALI
095900970613     C*------------------------------------------------------------------------*
096000970613     C     *INZSR        BEGSR
096100970613     C*
096200970613     C* RICEVIMENTO PARAMETRI
096300970613     C     *ENTRY        PLIST
096400970613     C                   PARM                    KPJBA
096500970613     C                   MOVEL     KPJBU         PARAM
096600061016      *
096700061016      * Reperimento dati aziendali
096800061016     c     *dtaara       define    §azute        AZUTEds
096900061016     c     *dtaara       define    §datiute      dDATIUTE
097000061016     c                   in(E)     *dtaara
097100061016     c                   if        %ERROR or RSUT = *blanks
097200061016     c                   clear                   Tibs34Ds
097300061016     c                   call      'TIBS34R'
097400061016     c                   parm                    Tibs34Ds
097500061016     c                   in        *dtaara
097600061016     c                   endif
097700091008     c*
097800091008     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
097900091008     c                   clear                   tibs02ds
098000091008     c                   clear                   dvpodeco
098100091008     c                   eval      t02mod = 'C'
098200091008     c                   eval      t02sif = knsif
098300091008     c                   eval      t02cod = 'VPO'
098400091008     c                   eval      t02ke1 = 'DECO'
098500091008     c                   call      'TIBS02R'
098600091008     c                   parm                    kpjba
098700091008     c                   parm                    tibs02ds
098800091008     c                   if        t02err = *blanks
098900091008     c                   eval      dvpodeco = t02uni
099000091008     c                   else
099100091008     c                   eval      §vpotsph=20091001
099200091008     c                   endif
099300091008
099400091008     c*
099500091221     c                   if        parDri>=§vpotsph  or %subst(knmus:1:3)='EDP'
099600091008     c                   seton                                        10
099700091008     c                   endif
099800061016      *
099900000509     C* IMPOSTAZIONI INIZIALI
100000000509     C     PARTEM        IFEQ      'S'
100100000509     C                   SETON                                        40
100200000509     C                   ENDIF
100300970613     C*
100400970613     C* VARIABILI RIFERITE AL DATA BASE
100500970613     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
100600970613     C     *LIKE         DEFINE    TBLCOD        KTBCOD
100700970613     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
100800970613     C     *LIKE         DEFINE    CPCNAR        KCCNAR
100900970613     C     *LIKE         DEFINE    CPCCAP        KCCCAP
101000970613     C     *LIKE         DEFINE    CPCPRV        KCCPRV
101100970613     C     *LIKE         DEFINE    CPCLNA        KCCLNA
101200970613     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
101300970613     C     *LIKE         DEFINE    CPENAR        KCENAR
101400970613     C     *LIKE         DEFINE    CPECAP        KCECAP
101500970613     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
101600970613     C     *LIKE         DEFINE    CPTTTC        KCTTTC
101700970613     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
101800970613     C     *LIKE         DEFINE    CPLNAR        KCLNAR
101900970613     C     *LIKE         DEFINE    CPLCAP        KCLCAP
102000970613     C     *LIKE         DEFINE    CPCNAR        DEPNA1                         *DEPOSITI
102100970613     C     *LIKE         DEFINE    CPCNAR        DEPNA2
102200970613     C     *LIKE         DEFINE    CPTTTC        DEPTTC
102300970613     C     *LIKE         DEFINE    CPLLOC        DEPDPR
102400970613     C     *LIKE         DEFINE    CPCNAR        ROTNAR                         *ROTTURE
102500970613     C     *LIKE         DEFINE    CPCPRV        ROTPRV
102600970613     C     *LIKE         DEFINE    CPCLNA        ROTLNA
102700970613     C*
102800970613     C* CHIAVI DI LETTURA
102900970613     C* TABEL00F - PARZIALE
103000970613     C     KE2TAB        KLIST
103100970613     C                   KFLD                    KTBKUT                         *UTENTE
103200970613     C                   KFLD                    KTBCOD                         *TABELLA
103300970613     C* AZCPC01L - COMPLETA
103400970613     C     KEYCPC        KLIST
103500970613     C                   KFLD                    KCCVER                         *VERSIONE
103600970613     C                   KFLD                    KCCNAR                         *NAZIONE
103700970613     C                   KFLD                    KCCCAP                         *CAP
103800970613     C* AZCPC02L - COMPLETA
103900970613     C     KE2CPC        KLIST
104000970613     C                   KFLD                    KCCVER                         *VERSIONE
104100970613     C                   KFLD                    KCCNAR                         *NAZIONE
104200970613     C                   KFLD                    KCCPRV                         *PROVINCIA
104300970613     C                   KFLD                    KCCCAP                         *CAP
104400970613     C* AZCPC03L - COMPLETA
104500970613     C     KE3CPC        KLIST
104600970613     C                   KFLD                    KCCVER                         *VERSIONE
104700970613     C                   KFLD                    KCCNAR                         *NAZIONE
104800970613     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
104900970613     C                   KFLD                    KCCCAP                         *CAP
105000970613     C* AZCPE01L - PARZIALE
105100970613     C     KEYCPE        KLIST
105200970613     C                   KFLD                    KCEVER                         *VERSIONE
105300970613     C                   KFLD                    KCENAR                         *NAZIONE
105400970613     C                   KFLD                    KCECAP                         *CAP
105500970613     C* AZCPT01L - PARZIALE
105600970613     C     KEYCPT        KLIST
105700970613     C                   KFLD                    KCTVER                         *VERSIONE
105800970613     C                   KFLD                    KCTTTC                         *TIPO TEMPO
105900970613     C* AZCPL01L - PARZIALE
106000970613     C     KEYCPL        KLIST
106100970613     C                   KFLD                    KCLVER                         *VERSIONE
106200970613     C                   KFLD                    KCLNAR                         *NAZIONE
106300970613     C                   KFLD                    KCLCAP                         *CAP
106400970613     C*
106500970613     C* VARIABILI DI CONTROLLO
106600970613     C                   MOVEL     '0'           FINL00            1            *FINE PGM
106700970613     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
106800970613     C                   MOVEL     'N'           WSTCAP            1            *CAP DA STAMPARE
106900970613     C                   MOVEL     'N'           WSTECC            1            *ECC DA STAMPATE
107000970613     C                   MOVEL     '1'           *IN01                          *OVERFLOW
107100970613     C*
107200970613     C* REPERIMENTO DATA E ORA CORRENTE
107300970613     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
107400970613     C                   MOVEL     WN14          ORA               6 0          *ORA  (6)
107500970613     C                   MOVE      WN14          DATA              8 0          *DATA (8) IN G/M/AA
107600970613     C*
107700970613     C* CARICA LE TABELLE OCCORRENTI
107800970613     C                   EXSR      CARTAB
107900970613     C*
108000970613     C                   ENDSR
108100970613     C*------------------------------------------------------------------------*
108200970613**   DEC - DECODIFICHE
108300970613CAP                                                         1
108400970613PROVINCIA                                                   2
108500970613LINEA DI ARRIVO                                             3
108600970613INTERNI                                                     4
108700970613CLIENTE                                                     5
108800970613TUTTI                                                       6
108900970613LOCALITA' DELIVERY                                          7
109000970613ITALIA                                                      8
1091009706139
10920097061310
