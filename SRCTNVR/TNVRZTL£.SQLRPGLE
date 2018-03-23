000100940912     H*------------------------------------------------------------------------*
000200040304     H* TRASFERIMENTO FIAR500F A FIAR5P0F
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
007104040304     FFIAR500F  Uf   E             DISK
007106040303     F                                     INFDS(£DSAR5)
007107040304     FFIAR5P0F  O  A E             DISK
007600931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
007700931105     F                                     FORMLEN(66)
007800931105     F                                     FORMOFL(60)
007900940912     L*------------------------------------------------------------------------*
007901040304     D RRNAR5          S              9  0
007923040303     D £DSAR5          DS
007924040303     D  NRRAR5               397    400B 0
008200940912     D*------------------------------------------------------------------------*
008300931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
013900940912     C*------------------------------------------------------------------------*
013901040304     C/EXEC SQL
013902040304     C+ DECLARE A1 CURSOR FOR
013903040304     C+ SELECT RRN(FIAR500F) FROM FIAR500F JOIN TITASP0F ON
013904040304     C+ AR5AAS = TASAAS AND AR5LNP = TASLNP AND AR5NRS = TASNRS AND
013905040305     C+ AR5NSP = TASNSP where ar5aas < 2004
013906040304     C/END-EXEC
013907040304     c*
013908040304     C* SE TROVA LA BOLLA IN TITASP CANCELLO FIAR500F E LO METTO IN FIAR5P
013909040304     c*
013910040304     C/EXEC SQL
013911040304     C+ OPEN A1
013912040304     C/END-EXEC
030616040304
030617040304     C                   do        *hival
030618040304
030619040304     C/EXEC SQL
030620040304     C+ FETCH NEXT FROM A1 INTO :rrnar5
030621040304     C/END-EXEC
030622040304
030623040304     C                   select
030624040304     C* FINE FILE
030625040304     C                   when      sqlcod = 100
030626040304     C* stampa riga di fine
030627040304     C                   EXCEPT    STAFIN
030628040304     C                   SETON                                        LR
030629040304     c                   leave
030630040304     C* DETTAGLIO
030631040304     C                   when      sqlcod = 0
030633040304     C     RRNAR5        chain     FIAR5000                                     *CANCELLO
030634040308     c                   if        %found(fiar500f)
030635040304     C     rrnar5        DELETE    FIAR5000                                     *CANCELLO
030636040304     C                   WRITE     FIAR5P00                             99      *SCRIVO ECC2VP
030637040304     C                   IF        *IN99
030638040304     C                   EXCEPT    ERRAR5
030639040304     C                   SETON                                        LR
030640040305     C                   leave
030641040304     C                   ELSE
030642040304     C                   ADD       1             NUMAR5                         *REC TRASF ECC2V
030643040304     C                   END
030644040308     C                   END
030645040304     C                   endsl
030646040304     C*
030647040304     C                   enddo
030648040304
030649040304     C/EXEC SQL
030650040304     C+ CLOSE A1
030651040304     C/END-EXEC
031801040304     D*------------------------------------------------------------------------*
060501040304     C     *INZSR        BEGSR
060502040304     D*------------------------------------------------------------------------*
071602040303     C     KEYAR5        KLIST
071603040303     C                   KFLD                    AR5AAS                         ANNO SPEDIZIONE
071604040303     C                   KFLD                    AR5LNP                         LINEA DI PARTENZA
071605040303     C                   KFLD                    AR5NRS                         NUMERO SERIE
071606040303     C                   KFLD                    AR5NSP                         NUMERO SPEDIZIONE
073101040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
073102040304     C* STAMPA RIGA DI TESTATA
073103040304     C                   EXCEPT    TESTA
073104040304     C                   ENDSR
073700940905     OPRTF198   E            TESTA            02
073800040304     O                                           +0 'TNVRAR5R1'
074000040304     O                                           82 'TRASF. FIAR5->FIAR5P    '
074900931105     O                                          190 'Pagina:'
075000931105     O                       PAGE          Z    198
075100931105     O          E            TESTA       1  1
075200931105     O                       TES(2)              66
075300931105     O                       TES(3)             132
075400931105     O                       TES(4)             198
075500931105     O          E            TESTA          1
075600931105     O                       TES(1)              66
075700931105     O                       TES(1)             132
075800931105     O                       TES(1)             198
084904040303     O          E            ERRAR5         1
084905040303     O                       AR5AAS            +  5
084906040303     O                       AR5LNP            +  1
084907040303     O                       AR5NRS            +  1
084908040303     O                       AR5NSP            +  1
084910040303     O                                         +  1 'ERR. WRITE FIAR5   '
084911040303     O                                         +  1 'NR. RELATIO REC.'
084912040303     O                       NRRAR5            +  1
088301040303     O          E            STAFIN         1
088302040303     O                                           30 'N.REC.TRASF. FIAR5'
088303040303     O                       NUMAR5        2     44
088400950308     O          E            STAFIN           60
088500931115     O                                           76 '*** FINE STAMPA ***'
089000940912     O*------------------------------------------------------------------------*
089100931105**
089200931105------------------------------------------------------------------  1
089300931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
089400950308E                                                                   3
089500931105                                                                    4
089600931105                                                                    5
089700931105                                                                    6
089800931105                                                                    7
089900931105                                                                    8
090000931105                                                                    9
090100931105                                                                   10
090200931105                                                                   11
090300931105                                                                   12
090400931105                                                                   13
090500931105                                                                   14
090600931105                                                                   15
