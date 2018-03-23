000100001108     H*--------------------------------------------------------------------------------------------*
000200001108     H* Allineamento archivi per clienti - Richiesta parametri
000300001108     H*--------------------------------------------------------------------------------------------*
000400001108     H DECEDIT('0,') DATEDIT(*DMY.)
000500001108     F*--------------------------------------------------------------------------------------------*
000600001108     F* Data base
000700001108     F*--------------------------------------------------------------------------------------------*
000800001108     Ftisi78d   CF   E             WORKSTN
000900001108     Fazcpr01l  IF   E           K DISK
001000151130     Fazcpc01l  IF   E           K DISK
001100001108     Fazcpl01l  UF   E           K DISK
001200001108     Fazcps01l  UF A E           K DISK
001300001108     Fazcps04l  IF   E           K DISK    RENAME(azcps000:azcps004)
001400001108     F                                     PREFIX(W_)
001500001108     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001600001108     F                                     USROPN
001700001108     D*--------------------------------------------------------------------------------------------*
001800001108     D* Data structure
001900001108     D*--------------------------------------------------------------------------------------------*
002000001108     D*----------------
002100001108     D* schiere a tempo di compilazione
002200001108     D*----------------
002300001108     D msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi video
002400001108     D sta             S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002500001108     D msp             S             66    DIM(10) CTDATA PERRCD(1)             *messaggi stampa
002600001108     D*----------------
002700001108     D* Variabili riferite al data base
002800001108     D*----------------
002900001108     D kprver          S                   LIKE(cprver)                         *per azcpr00f
003000001108     D kplver          S                   LIKE(cplver)                         *per azcplo0f
003100001108     D kplnar          S                   LIKE(cplnar)
003200001108     D kplcap          S                   LIKE(cplcap)
003300001108     D kplloc          S                   LIKE(cplloc)
003400001108     D kpsver          S                   LIKE(cpsver)                         *per azcps00f
003500001108     D kpsnar          S                   LIKE(cpsnar)
003600001108     D kpscap          S                   LIKE(cpscap)
003700001108     D kpsloc          S                   LIKE(cpsloc)
003800001108     D kpssin          S                   LIKE(cpssin)                         *deposito
003900151130     D kpcver          S                   LIKE(cpcver)                         *per azcps00f
004000151130     D kpcnar          S                   LIKE(cpcnar)
004100151130     D kpccap          S                   LIKE(cpccap)
004200001108     D depsin          S                   LIKE(cpssin)
004300001108     D tip             S              3    INZ(*blanks)
004400001108     D*----------------
004500001108     D* Variabili di controllo
004600001108     D*----------------
004700001108     D $fine           S              1    INZ('N')                             *Gestione
004800001108     D $err            S              1    INZ('0')                             *errore
004900001108     D $tesvd          S              1    INZ('1')                             *riemettere testata
005000001108     D $tipvid         S              1    INZ('1')                             *gestione video
005100001108     D*----------------
005200001108     D* Variabili a programma
005300001108     D*----------------
005400001108     D stmver          S                   LIKE(cpsver)                         *di stampa
005500001108     D stmnar          S                   LIKE(cpsnar)
005600001108     D stmcap          S                   LIKE(cpscap)
005700001108     D stmloc          S                   LIKE(cpsloc)
005800001108     D stmsin          S                   LIKE(cpssin)
005900001108     D stmmsg          S             40
006000001108     D p96dri          S                   LIKE(cprdde)                         *lancio TISI96R
006100001108     D p96ver          S                   LIKE(cprver)
006200001108     D posapice        S              3  0 inz(*zeros)
006300001108     D chkins          S              1    inz(*zeros)
006400001108     D lungh           S              3  0 inz(*zeros)
006500001108     D n14             S             14  0                                      *numerico 14
006600001108     D n8              S              8  0                                      *numerico 8
006700001108     D datcor          S              8  0                                      *data corrente a/m/g
006800001109     D lancio          S              6    inz('(SIM.)')
006900001108     D*----------------
007000001108     D* Ds
007100001108     D*----------------
007200001108     D dssi00        E DS                  EXTNAME(tisi00ds)                    reperimento versioni
007300001108     D kpjba         E DS                                                       architettura
007400001108     C*--------------------------------------------------------------------------------------------*
007500001108     C* MAIN LINES
007600001108     C*--------------------------------------------------------------------------------------------*
007700000000     C*
007800001108     C* Impostazioni testata
007900000000     C                   EXSR      impvd0
008000000000     C*
008100001108     C* Impostazioni prima videata
008200000000     C                   EXSR      impvd1
008300000000     C*
008400001108     C* Ciclo fino a richiesta fine elaborazione
008500001108     C                   DOU       $fine='S'
008600000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
008700000000     C                   ENDCS
008800001108     C                   ENDDO
008900000000     C*
009000001108     C                   SETON                                        LR
009100001108     C*--------------------------------------------------------------------------------------------*
009200001108     C* IMPVD1 - Impostazioni prima videata
009300001108     C*--------------------------------------------------------------------------------------------*
009400001108     C     IMPVD1        BEGSR
009500001108     C*
009600001108     C* Reperisce l'ultima versione
009700001108     C                   Z-ADD     *zeros        p96dri                         *ultima versione
009800001108     C                   Z-ADD     *zeros        p96ver
009900001108     C                   CALL      'TISI96R'
010000001108     C                   PARM                    p96dri
010100001108     C                   PARM                    p96ver
010200001108     C                   Z-ADD     p96ver        v1cver                         *versione
010300000000     C                   MOVEL     'S'           v1csim                         *simulazione
010400000000     C*
010500001108     C* Imposta formato video e indicatori
010600000000     C                   EXSR      setvd1
010700000000     C                   MOVEL     '1'           $tipvid                        *gestione video
010800000000     C*
010900000000     C                   ENDSR
011000001108     C*--------------------------------------------------------------------------------------------*
011100001108     C* SETVD1 - Impostazioni indicatori prima videata
011200001108     C*--------------------------------------------------------------------------------------------*
011300001108     C     SETVD1        BEGSR
011400000000     C*
011500001108     C                   SETOFF                                       2021       *errori
011600000000     C                   SETOFF                                       90         *generico di errore
011700000000     C                   MOVEL     *blanks       $msg                            *messaggio
011800000000     C*
011900000000     C                   ENDSR
012000001108     C*--------------------------------------------------------------------------------------------*
012100001108     C* GESVD1 - Gestione prima videata
012200001108     C*--------------------------------------------------------------------------------------------*
012300001108     C     GESVD1        BEGSR
012400000000     C*
012500001108     C* Emissione videata
012600001108     C                   IF        $tesvd='1'
012700001108     C                   WRITE     si78d00                                      *testata
012800000000     C                   MOVEL     '0'           $tesvd                         *no testata
012900001108     C                   ENDIF
013000001108     C                   EXFMT     si78d01                                      *video
013100000000     C                   EXSR      setvd1
013200000000     C* F03-Fine
013300001108     C                   IF        *inkc
013400000000     C                   EXSR      f03ges
013500001108     C                   ENDIF
013600001108     C* F10-Ricerca Versione
013700001108     C                   IF        *inkj
013800000000     C                   EXSR      f10ges
013900001108     C                   ENDIF
014000000000     C* F12-Ritorno
014100001108     C                   IF        *inkl
014200000000     C                   EXSR      f12ges
014300001108     C                   ENDIF
014400000000     C* F06-Conferma
014500001108     C                   IF        *inkf
014600001108     C                   EXSR      ctrvd1                                       *controllo parametri
014700001108     C  N90              EXSR      f06ges                                       *se no errori
014800001108     C                   ENDIF
014900000000     C*
015000001108     C                   ENDSR
015100001108     C*--------------------------------------------------------------------------------------------*
015200001108     C* CTRVD1 - Controlla prima videata
015300001108     C*--------------------------------------------------------------------------------------------*
015400001108     C     CTRVD1        BEGSR
015500000000     C*
015600001108     C* Reimposta le variabili di lavoro
015700000000     C                   MOVEL     '0'           $err
015800000000     C*---
015900001108     C* Simulazione
016000000000     C*---
016100001108     C                   IF        $err = '0'
016200001108     C                   IF        v1csim = 'S' OR
016300001108     C                             v1csim = 'N'
016400001108     C                   ELSE
016500000000     C                   SETON                                        2090
016600000000     C                   MOVEL     msg(1)        $msg
016700000000     C                   MOVEL     '1'           $err
016800001108     C                   ENDIF
016900001108     C                   ENDIF
017000000000     C*---
017100001108     C* Versione
017200000000     C*---
017300001108     C                   IF        $err = '0'
017400000000     C                   MOVEL     *blanks       v1dver
017500000000     C                   Z-ADD     v1cver        kprver
017600001108     C     kprver        CHAIN     azcpr01l
017700001108     C                   IF        %FOUND(azcpr01l)
017800001108     C                   MOVEL     cprdes        v1dver
017900001108     C                   ELSE
018000000000     C                   SETON                                        2190
018100000000     C                   MOVEL     '1'           $err
018200000000     C                   MOVEL     msg(2)        $msg
018300001108     C                   ENDIF
018400001108     C                   ENDIF
018500000000     C*
018600001108     C                   ENDSR
018700001108     C*--------------------------------------------------------------------------------------------*
018800001108     C* F03GES - Gestione tasto funzionale F03 ==> Fine
018900001108     C*--------------------------------------------------------------------------------------------*
019000001108     C     F03GES        BEGSR
019100000000     C*
019200000000     C                   MOVEL     'S'           $fine                          *fine programma
019300000000     C*
019400000000     C                   ENDSR
019500001108     C*--------------------------------------------------------------------------------------------*
019600001108     C* F06GES - Gestione tasto funzionale F06 ==> Conferma
019700001108     C*--------------------------------------------------------------------------------------------*
019800001108     C     F06GES        BEGSR
019900001108     C*
020000001108     C* Apre il file di stampa
020100001108     C                   OPEN      qsysprt
020200001109     C*
020300001109     C* Imposto subito il "Tipo lancio" in testata
020400001109     C                   IF        v1csim = 'N'                                 *NO simulazione
020500001109     C                   EVAL      lancio = '(AGG.)'
020600001109     C                   ENDIF
020700001108     C*
020800001108     C* Esegue routine di elaborazione
020900001108     C                   EXSR      inscpl
021000151209     C***                EXSR      inscps
021100001108     C*
021200001108     C* Chiude il file di stampa
021300001108     C                   CLOSE     qsysprt
021400001108     C*
021500001108     C* Invia messaggio
021600001108     C                   SETON                                        90
021700001108     C                   MOVEL     msg(10)       $msg
021800001108     C*
021900001108     C                   ENDSR
022000001108     C*--------------------------------------------------------------------------------------------*
022100001108     C* F10GES - Gestione tasto funzionale F10 ==> Ricerca versioni
022200001108     C*--------------------------------------------------------------------------------------------*
022300001108     C     F10GES        BEGSR
022400001108     C*
022500001108     C* Lancia il programma di gestione versioni in ricerca
022600001108     C                   CLEAR                   dssi00                         *pulisce ds
022700001108     C                   MOVEL     'R01'         d00op0                         *livello procedura
022800001108     C                   MOVEL     *blanks       d00op1                         *livello programma
022900001108     C                   MOVEL     '0'           d00f03                         *premuto F03
023000001108     C                   MOVEL     '0'           d00f12                         *premuto F12
023100001108     C                   MOVEL     '0'           d00err                         *errore
023200001108     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
023300001108     C                   CALL      'TISI46R'                                    *gestione versioni
023400001108     C                   PARM                    kpjba
023500001108     C                   PARM                    dssi00
023600001108     C                   IF        d00err='1'                                   *errore
023700000000     C                   SETON                                        2190
023800000000     C                   MOVEL     msg(2)        $msg
023900001108     C                   ELSE                                                   *no errore
024000001108     C                   Z-ADD     d00ver        v1cver
024100001108     C                   ENDIF
024200001108     C                   MOVEL     '1'           $tesvd
024300000000     C*
024400000000     C                   ENDSR
024500001108     C*--------------------------------------------------------------------------------------------*
024600001108     C* F12GES - Gestione tasto funzionale F12 ==> Ritorno
024700001108     C*--------------------------------------------------------------------------------------------*
024800001108     C     F12GES        BEGSR
024900000000     C*
025000000000     C                   MOVEL     'S'           $fine                          *fine programma
025100000000     C*
025200000000     C                   ENDSR
025300000000     C*--------------------------------------------------------------------------------------------*
025400001108     C* INSCPL - Creazione nuovi sinonimi da AZCPL
025500000000     C*--------------------------------------------------------------------------------------------*
025600001108     C     INSCPL        BEGSR
025700001108     C*
025800001108     C* Imposta variabili di lavoro
025900001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
026000001108     C                   MOVEL     'CPL'         tip
026100151130     C                   SETOFF                                       55
026200001108     C*
026300001108     C* Legge le località AZCPL
026400001108     C     V1CVER        SETLL     azcpl01l
026500001108     C     V1CVER        READE     azcpl01l
026600001108     C                   DOW       not %eof(azcpl01l)
026700001108     C                   IF        cplatb = *blanks AND                         *no annullati
026800000000     C                             cplfi1 = *blanks                             *no obsoleti
026900000000     C*
027000001108     C* Procedo con le considerazioni da fare:
027100001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
027200001108     C                   MOVEL     '0'           CHKINS
027300001108     C*                  EVAL      posapice = %scan('''':cplloc)
027400001108     C*                  IF        posapice > 0 and
027500001108     C*                            %subst(cplloc:(posapice+1):1) <> ' '
027600001108     C     '''':' '      XLATE     cplloc        depsin
027700001108     C                   IF        cplloc <> depsin
027800001108     C*
027900001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
028000001108     C                   Z-ADD     cplver        kpsver
028100001108     C                   MOVEL     cplnar        kpsnar
028200001108     C                   MOVEL     cplcap        kpscap
028300001108     C                   MOVEL     cplloc        kpsloc
028400001108     C                   MOVEL     depsin        kpssin
028500001108     C     keycps        CHAIN(N)  azcps01l
028600001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
028700001108     C                   Z-ADD     kpsver        cpsver                         *chiave
028800001108     C                   MOVEL     kpsnar        cpsnar
028900001108     C                   MOVEL     kpscap        cpscap
029000001108     C                   MOVEL     kpsloc        cpsloc
029100001108     C                   MOVEL     kpssin        cpssin
029200001108     C                   MOVEL     *blanks       cpsatb                         *campi
029300001108     C                   MOVEL     *blanks       cpsftr
029400001108     C                   Z-ADD     *zeros        cpsdtr
029500001108     C                   IF        v1csim = 'N'                                 *NO simulazione
029600001108     C                   WRITE     azcps000                                     *INSERIMENTO
029700001108     C                   MOVEL     '1'           CHKINS
029800151130     C                   SETON                                        55
029900001108     C                   ENDIF
030000001108     C* Stampa riga di verifica
030100001108     C                   MOVEL     msp(1)        stmmsg
030200001108     C                   EXSR      stadet
030300001108     C                   ENDIF
030400001108     C                   ENDIF
030500001108     C*                  ENDIF
030600001108     C*
030700001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
030800001108     C                   MOVEL     '0'           CHKINS
030900001108     C                   Z-ADD     *HIVAL        posapice
031000001108     C                   EVAL      depsin = %trim(cplloc)
031100001108     C                   EVAL      lungh = %len(%trim(depsin))
031200001108     C                   DOU       posapice = *zeros
031300001108     C                   EVAL      posapice = %scan('''':depsin)
031400001108     C                   IF        posapice > 0 and posapice <= lungh and
031500001108     C                             %subst(depsin:(posapice+1):1) <> ' '
031600001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
031700001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
031800001108     C                   EVAL      lungh = %len(%trim(depsin))
031900001108     C                   ELSE
032000001108     C                   LEAVE
032100001108     C                   ENDIF
032200001108     C                   ENDDO
032300001108     C*
032400001108     C                   IF        depsin <> %trim(cplloc)
032500001108     C*
032600001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
032700001108     C                   Z-ADD     cplver        kpsver
032800001108     C                   MOVEL     cplnar        kpsnar
032900001108     C                   MOVEL     cplcap        kpscap
033000001108     C                   MOVEL     cplloc        kpsloc
033100001108     C                   MOVEL     depsin        kpssin
033200001108     C     keycps        CHAIN(N)  azcps01l
033300001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
033400001108     C                   Z-ADD     kpsver        cpsver                         *chiave
033500001108     C                   MOVEL     kpsnar        cpsnar
033600001108     C                   MOVEL     kpscap        cpscap
033700001108     C                   MOVEL     kpsloc        cpsloc
033800001108     C                   MOVEL     kpssin        cpssin
033900001108     C                   MOVEL     *blanks       cpsatb                         *campi
034000001108     C                   MOVEL     *blanks       cpsftr
034100001108     C                   Z-ADD     *zeros        cpsdtr
034200001108     C                   IF        v1csim = 'N'                                 *NO simulazione
034300001108     C                   WRITE     azcps000                                     *INSERIMENTO
034400001108     C                   MOVEL     '1'           CHKINS
034500151130     C                   SETON                                        55
034600001108     C                   ENDIF
034700001108     C* Stampa riga di verifica
034800001108     C                   MOVEL     msp(1)        stmmsg
034900001108     C                   EXSR      stadet
035000001108     C                   ENDIF
035100001108     C                   ENDIF
035200151130     C*
035300151130     C** 3) ==> aggiungo un sinonimo uguale alla località + siglia PROV in coda
035400151130     C                   MOVEL     '0'           CHKINS
035500151130     C                   IF        %len(%trim(cplloc)) < 32
035600151209     C                   Z-ADD     cplver        kpcver
035700151209     C                   MOVEL     cplnar        kpcnar
035800151209     C                   MOVEL     cplcap        kpccap
035900151130     C     keycpc        CHAIN(N)  azcpc01l
036000151130     C                   IF        %FOUND(azcpc01l)
036100151130     C                   EVAL      depsin = %trim(cplloc)+' '+cpcprv
036200151130     C                   ENDIF
036300151130     C*
036400151130     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
036500151130     C                   Z-ADD     cplver        kpsver
036600151130     C                   MOVEL     cplnar        kpsnar
036700151130     C                   MOVEL     cplcap        kpscap
036800151130     C                   MOVEL     cplloc        kpsloc
036900151130     C                   MOVEL     depsin        kpssin
037000151130     C     keycps        CHAIN(N)  azcps01l
037100151130     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
037200151130     C                   Z-ADD     kpsver        cpsver                         *chiave
037300151130     C                   MOVEL     kpsnar        cpsnar
037400151130     C                   MOVEL     kpscap        cpscap
037500151130     C                   MOVEL     kpsloc        cpsloc
037600151130     C                   MOVEL     kpssin        cpssin
037700151130     C                   MOVEL     *blanks       cpsatb                         *campi
037800151130     C                   MOVEL     *blanks       cpsftr
037900151130     C                   Z-ADD     *zeros        cpsdtr
038000151130     C                   IF        v1csim = 'N'                                 *NO simulazione
038100151130     C                   WRITE     azcps000                                     *INSERIMENTO
038200151130     C                   MOVEL     '1'           CHKINS
038300151130     C                   SETON                                        55
038400151130     C                   ENDIF
038500151130     C* Stampa riga di verifica
038600151130     C                   MOVEL     msp(1)        stmmsg
038700151130     C                   EXSR      stadet
038800151130     C                   ENDIF
038900151130     C                   ENDIF
039000001108     C*
039100001108     C* Verifica di correttezza impostazione flag su AZCPL
039200151130     C                   IF        *IN55 = '1' AND
039300001108     C                             cpless <> 'S'
039400001108     C                   MOVEL     'S'           cpless
039500001108     C                   MOVEL     *blanks       cplftr
039600001108     C                   Z-ADD     *zeros        cpldtr
039700001108     C                   UPDATE    azcpl000
039800001108     C                   ENDIF
039900001108     C                   ENDIF
040000001108     C     V1CVER        READE     azcpl01l                               99
040100001108     C                   ENDDO
040200000000     C*
040300000000     C                   ENDSR
040400001108     C*--------------------------------------------------------------------------------------------*
040500001108     C* INSCPS - Creazione nuovi sinonimi da AZCPS
040600001108     C*--------------------------------------------------------------------------------------------*
040700001108     C     INSCPS        BEGSR
040800001108     C*
040900001108     C* Imposta variabili di lavoro
041000001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
041100001108     C                   MOVEL     'CPS'         tip
041200001108     C*
041300001108     C* Legge le località AZCPL
041400001108     C     V1CVER        SETLL     azcps04l
041500001108     C     V1CVER        READE     azcps04l
041600001108     C                   DOW       not %eof(azcps04l)
041700001108     C                   IF        w_cpsatb = *blanks                             *no annullati
041800001108     C*
041900001108     C* Procedo con le considerazioni da fare:
042000001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
042100001108     C*                  EVAL      posapice = %scan('''':w_cpssin)
042200001108     C*                  IF        posapice > 0 and
042300001108     C*                            %subst(w_cpssin:(posapice+1):1) <> ' '
042400001108     C     '''':' '      XLATE     w_cpssin      depsin
042500001108     C                   IF        w_cpssin <> depsin and
042600001108     C                             w_cpsloc <> depsin
042700001108     C*
042800001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
042900001108     C                   Z-ADD     w_cpsver      kpsver
043000001108     C                   MOVEL     w_cpsnar      kpsnar
043100001108     C                   MOVEL     w_cpscap      kpscap
043200001108     C                   MOVEL     w_cpsloc      kpsloc
043300001108     C                   MOVEL     depsin        kpssin
043400001108     C     keycps        CHAIN(N)  azcps01l
043500001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
043600001108     C                   Z-ADD     kpsver        cpsver                         *chiave
043700001108     C                   MOVEL     kpsnar        cpsnar
043800001108     C                   MOVEL     kpscap        cpscap
043900001108     C                   MOVEL     kpsloc        cpsloc
044000001108     C                   MOVEL     kpssin        cpssin
044100001108     C                   MOVEL     *blanks       cpsatb                         *campi
044200001108     C                   MOVEL     *blanks       cpsftr
044300001108     C                   Z-ADD     *zeros        cpsdtr
044400001108     C                   IF        v1csim = 'N'                                 *NO simulazione
044500001108     C                   WRITE     azcps000                                     *INSERIMENTO
044600001108     C                   ENDIF
044700001108     C* Stampa riga di verifica
044800001108     C                   MOVEL     msp(2)        stmmsg
044900001108     C                   EXSR      stadet
045000001108     C                   ENDIF
045100001108     C                   ENDIF
045200001108     C*                  ENDIF
045300001108     C*
045400001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
045500001108     C                   MOVEL     '0'           CHKINS
045600001108     C                   Z-ADD     *HIVAL        posapice
045700001108     C                   EVAL      depsin = %trim(w_cpssin)
045800001108     C                   EVAL      lungh = %len(%trim(depsin))
045900001108     C                   DOU       posapice = *zeros
046000001108     C                   EVAL      posapice = %scan('''':depsin)
046100001108     C                   IF        posapice > 0 and posapice <= lungh and
046200001108     C                             %subst(depsin:(posapice+1):1) <> ' '
046300001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
046400001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
046500001108     C                   EVAL      lungh = %len(%trim(depsin))
046600001108     C                   ELSE
046700001108     C                   LEAVE
046800001108     C                   ENDIF
046900001108     C                   ENDDO
047000001108     C*
047100001108     C                   IF        depsin <> %trim(w_cpssin) and
047200001108     C                             depsin <> %trim(w_cpsloc)
047300001108     C*
047400001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
047500001108     C                   Z-ADD     w_cpsver      kpsver
047600001108     C                   MOVEL     w_cpsnar      kpsnar
047700001108     C                   MOVEL     w_cpscap      kpscap
047800001108     C                   MOVEL     w_cpsloc      kpsloc
047900001108     C                   MOVEL     depsin        kpssin
048000001108     C     keycps        CHAIN(N)  azcps01l
048100001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
048200001108     C                   Z-ADD     kpsver        cpsver                         *chiave
048300001108     C                   MOVEL     kpsnar        cpsnar
048400001108     C                   MOVEL     kpscap        cpscap
048500001108     C                   MOVEL     kpsloc        cpsloc
048600001108     C                   MOVEL     kpssin        cpssin
048700001108     C                   MOVEL     *blanks       cpsatb                         *campi
048800001108     C                   MOVEL     *blanks       cpsftr
048900001108     C                   Z-ADD     *zeros        cpsdtr
049000001108     C                   IF        v1csim = 'N'                                 *NO simulazione
049100001108     C                   WRITE     azcps000                                     *INSERIMENTO
049200001108     C                   ENDIF
049300001108     C* Stampa riga di verifica
049400001108     C                   MOVEL     msp(2)        stmmsg
049500001108     C                   EXSR      stadet
049600001108     C                   ENDIF
049700001108     C                   ENDIF
049800001108     C                   ENDIF
049900001108     C     V1CVER        READE     azcps04l
050000001108     C                   ENDDO
050100001108     C*
050200001108     C                   ENDSR
050300001108     C*--------------------------------------------------------------------------------------------*
050400001108     C* STADET - Stampa esito elaborazione
050500001108     C*--------------------------------------------------------------------------------------------*
050600001108     C     STADET        BEGSR
050700001108     C*
050800001108     C* Stampa testata
050900001108     C                   IF        *inof                                        *fine pagina
051000001108     C                   EXCEPT    tes
051100001108     C                   MOVEL     '0'           *inof
051200001108     C                   ENDIF
051300001108     C*
051400001108     C* Stampa dettaglio
051500001108     C                   Z-ADD     kpsver        stmver
051600001108     C                   MOVEL     kpsnar        stmnar
051700001108     C                   MOVEL     kpscap        stmcap
051800001108     C                   IF        tip = 'CPL'
051900001108     C                   MOVEL     kpsloc        stmloc
052000001108     C                   ENDIF
052100001108     C                   IF        tip = 'CPS'
052200001108     C                   MOVEL     w_cpssin      stmloc
052300001108     C                   ENDIF
052400001108     C                   MOVEL     kpssin        stmsin
052500001108     C                   EXCEPT    det
052600001108     C*
052700001108     C                   ENDSR
052800001108     C*--------------------------------------------------------------------------------------------*
052900001108     C* IMPVD0 - Impostazioni testata
053000001108     C*--------------------------------------------------------------------------------------------*
053100001108     C     IMPVD0        BEGSR
053200000000     C*
053300000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
053400000000     C*
053500000000     C                   ENDSR
053600001108     C*--------------------------------------------------------------------------------------------*
053700001108     C* *INZSR - Operazioni iniziali
053800001108     C*--------------------------------------------------------------------------------------------*
053900001108     C     *INZSR        BEGSR
054000001108     C*
054100001108     C* Ricevimento parametri
054200001108     C     *ENTRY        PLIST
054300001108     C                   PARM                    kpjba
054400000000     C*
054500001108     C* Chiavi di lettura
054600000000     C     keycps        klist                                                  *azcps00f
054700000000     C                   KFLD                    kpsver                          -versione
054800000000     C                   KFLD                    kpsnar                          -nazione
054900000000     C                   KFLD                    kpscap                          -cap
055000000000     C                   KFLD                    kpsloc                          -località
055100000000     C                   KFLD                    kpssin                          -sinonimo
055200151130     C     keycpc        klist                                                  *azcps00f
055300151130     C                   KFLD                    kpcver                          -versione
055400151130     C                   KFLD                    kpcnar                          -nazione
055500151130     C                   KFLD                    kpccap                          -cap
055600000000     C*
055700001108     C* Reperisce la data corrente
055800000000     C                   TIME                    n14                            *ora (6)+ data (8)
055900000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
056000000000     C                   Z-ADD     n8            datcor                         *Data corrente
056100001108     C*
056200001108     C                   ENDSR
056300001108     O*--------------------------------------------------------------------------------------------*
056400001108     O* Stampa
056500001108     O*--------------------------------------------------------------------------------------------*
056600000000     Oqsysprt   E            tes              02
056700000000     O                       sta(1)              66
056800000000     O                       sta(2)             132
056900001109     O                       lancio             105
057000000000     O                       datcor        8    117
057100000000     O                       PAGE          Z    132
057200000000     O*------------------------
057300000000     O          E            tes              03
057400000000     O                       sta(3)              66
057500000000     O                       sta(3)             132
057600000000     O*------------------------
057700000000     O          E            tes              04
057800000000     O                       sta(4)              66
057900000000     O                       sta(6)             132
058000000000     O*------------------------
058100000000     O          E            tes              05
058200000000     O                       sta(3)              66
058300000000     O                       sta(3)             132
058400000000     O*------------------------
058500000000     O          E            det         1
058600000000     O                       stmver        Z      5
058700000000     O                       stmnar               9
058800000000     O                       stmcap              19
058900000000     O                       stmloc              55
059000000000     O                       stmsin              91
059100000000     O                       stmmsg             132
059200000000     O*--------------------------------------------------------------------------------------------*
059300000000** MSG -Messaggi video-
059400001108TISI78R- Immettere uno dei valori possibili                                   1
059500001108TISI78R- Versione inesistente                                                 2
059600001108TISI78R- 3
059700001108TISI78R- 4
059800001108TISI78R- 5
059900001108TISI78R- 6
060000001108TISI78R- 7
060100001108TISI78R- 8
060200001108TISI78R- 9
060300001108TISI78R- Elaborazione eseguita ...                                            10
060400000000** STA -Righe di stampa -
060500001108TISI78R                            ***  Aggiunta sinonimi da AZCPL1
060600001108 e AZCPS  ***                            99/99/99         PAG.99992
060700000000------------------------------------------------------------------3
060800001108VER   NAZ CAP       LOCALITA'/SIN. ESISTENTE            SINONIMO  4
06090000000099999 XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX5
061000000000                          ESITO                                   6
061100000000XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7
0612000000008
0613000000009
06140000000010
061500000000** MSP -Messaggi stampa-
061600001108CREATO SINONIMO DA LOC.                                           1
061700001108AGGIUNTO SINONIMO DA SIN.                                         2
0618000011083
0619000011084
0620000000005
