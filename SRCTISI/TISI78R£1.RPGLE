000100001108     H*--------------------------------------------------------------------------------------------*
000200001108     H* Allineamento archivi per clienti - Richiesta parametri
000300001108     H*--------------------------------------------------------------------------------------------*
000400001108     H DECEDIT('0,') DATEDIT(*DMY.)
000500001108     F*--------------------------------------------------------------------------------------------*
000600001108     F* Data base
000700001108     F*--------------------------------------------------------------------------------------------*
000800001108     Ftisi78d   CF   E             WORKSTN
000900001108     Fazcpr01l  IF   E           K DISK
001000001108     Fazcpl01l  UF   E           K DISK
001100001108     Fazcps01l  UF A E           K DISK
001200001108     Fazcps04l  IF   E           K DISK    RENAME(azcps000:azcps004)
001300001108     F                                     PREFIX(W_)
001400001108     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001500001108     F                                     USROPN
001600001108     D*--------------------------------------------------------------------------------------------*
001700001108     D* Data structure
001800001108     D*--------------------------------------------------------------------------------------------*
001900001108     D*----------------
002000001108     D* schiere a tempo di compilazione
002100001108     D*----------------
002200001108     D msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi video
002300001108     D sta             S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002400001108     D msp             S             66    DIM(10) CTDATA PERRCD(1)             *messaggi stampa
002500001108     D*----------------
002600001108     D* Variabili riferite al data base
002700001108     D*----------------
002800001108     D kprver          S                   LIKE(cprver)                         *per azcpr00f
002900001108     D kplver          S                   LIKE(cplver)                         *per azcplo0f
003000001108     D kplnar          S                   LIKE(cplnar)
003100001108     D kplcap          S                   LIKE(cplcap)
003200001108     D kplloc          S                   LIKE(cplloc)
003300001108     D kpsver          S                   LIKE(cpsver)                         *per azcps00f
003400001108     D kpsnar          S                   LIKE(cpsnar)
003500001108     D kpscap          S                   LIKE(cpscap)
003600001108     D kpsloc          S                   LIKE(cpsloc)
003700001108     D kpssin          S                   LIKE(cpssin)                         *deposito
003800001108     D depsin          S                   LIKE(cpssin)
003900001108     D tip             S              3    INZ(*blanks)
004000001108     D*----------------
004100001108     D* Variabili di controllo
004200001108     D*----------------
004300001108     D $fine           S              1    INZ('N')                             *Gestione
004400001108     D $err            S              1    INZ('0')                             *errore
004500001108     D $tesvd          S              1    INZ('1')                             *riemettere testata
004600001108     D $tipvid         S              1    INZ('1')                             *gestione video
004700001108     D*----------------
004800001108     D* Variabili a programma
004900001108     D*----------------
005000001108     D stmver          S                   LIKE(cpsver)                         *di stampa
005100001108     D stmnar          S                   LIKE(cpsnar)
005200001108     D stmcap          S                   LIKE(cpscap)
005300001108     D stmloc          S                   LIKE(cpsloc)
005400001108     D stmsin          S                   LIKE(cpssin)
005500001108     D stmmsg          S             40
005600001108     D p96dri          S                   LIKE(cprdde)                         *lancio TISI96R
005700001108     D p96ver          S                   LIKE(cprver)
005800001108     D posapice        S              3  0 inz(*zeros)
005900001108     D chkins          S              1    inz(*zeros)
006000001108     D lungh           S              3  0 inz(*zeros)
006100001108     D n14             S             14  0                                      *numerico 14
006200001108     D n8              S              8  0                                      *numerico 8
006300001108     D datcor          S              8  0                                      *data corrente a/m/g
006400001109     D lancio          S              6    inz('(SIM.)')
006500001108     D*----------------
006600001108     D* Ds
006700001108     D*----------------
006800001108     D dsfil         E DS                                                       reperimento dati
006900001108     D dssi00        E DS                  EXTNAME(tisi00ds)                    reperimento versioni
007000001108     D kpjba         E DS                                                       architettura
007100001108     C*--------------------------------------------------------------------------------------------*
007200001108     C* MAIN LINES
007300001108     C*--------------------------------------------------------------------------------------------*
007400000000     C*
007500001108     C* Impostazioni testata
007600000000     C                   EXSR      impvd0
007700000000     C*
007800001108     C* Impostazioni prima videata
007900000000     C                   EXSR      impvd1
008000000000     C*
008100001108     C* Ciclo fino a richiesta fine elaborazione
008200001108     C                   DOU       $fine='S'
008300000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
008400000000     C                   ENDCS
008500001108     C                   ENDDO
008600000000     C*
008700001108     C                   SETON                                        LR
008800001108     C*--------------------------------------------------------------------------------------------*
008900001108     C* IMPVD1 - Impostazioni prima videata
009000001108     C*--------------------------------------------------------------------------------------------*
009100001108     C     IMPVD1        BEGSR
009200001108     C*
009300001108     C* Reperisce l'ultima versione
009400001108     C                   Z-ADD     *zeros        p96dri                         *ultima versione
009500001108     C                   Z-ADD     *zeros        p96ver
009600001108     C                   CALL      'TISI96R'
009700001108     C                   PARM                    p96dri
009800001108     C                   PARM                    p96ver
009900001108     C                   Z-ADD     p96ver        v1cver                         *versione
010000000000     C                   MOVEL     'S'           v1csim                         *simulazione
010100000000     C*
010200001108     C* Imposta formato video e indicatori
010300000000     C                   EXSR      setvd1
010400000000     C                   MOVEL     '1'           $tipvid                        *gestione video
010500000000     C*
010600000000     C                   ENDSR
010700001108     C*--------------------------------------------------------------------------------------------*
010800001108     C* SETVD1 - Impostazioni indicatori prima videata
010900001108     C*--------------------------------------------------------------------------------------------*
011000001108     C     SETVD1        BEGSR
011100000000     C*
011200001108     C                   SETOFF                                       2021       *errori
011300000000     C                   SETOFF                                       90         *generico di errore
011400000000     C                   MOVEL     *blanks       $msg                            *messaggio
011500000000     C*
011600000000     C                   ENDSR
011700001108     C*--------------------------------------------------------------------------------------------*
011800001108     C* GESVD1 - Gestione prima videata
011900001108     C*--------------------------------------------------------------------------------------------*
012000001108     C     GESVD1        BEGSR
012100000000     C*
012200001108     C* Emissione videata
012300001108     C                   IF        $tesvd='1'
012400001108     C                   WRITE     si78d00                                      *testata
012500000000     C                   MOVEL     '0'           $tesvd                         *no testata
012600001108     C                   ENDIF
012700001108     C                   EXFMT     si78d01                                      *video
012800000000     C                   EXSR      setvd1
012900000000     C* F03-Fine
013000001108     C                   IF        *inkc
013100000000     C                   EXSR      f03ges
013200001108     C                   ENDIF
013300001108     C* F10-Ricerca Versione
013400001108     C                   IF        *inkj
013500000000     C                   EXSR      f10ges
013600001108     C                   ENDIF
013700000000     C* F12-Ritorno
013800001108     C                   IF        *inkl
013900000000     C                   EXSR      f12ges
014000001108     C                   ENDIF
014100000000     C* F06-Conferma
014200001108     C                   IF        *inkf
014300001108     C                   EXSR      ctrvd1                                       *controllo parametri
014400001108     C  N90              EXSR      f06ges                                       *se no errori
014500001108     C                   ENDIF
014600000000     C*
014700001108     C                   ENDSR
014800001108     C*--------------------------------------------------------------------------------------------*
014900001108     C* CTRVD1 - Controlla prima videata
015000001108     C*--------------------------------------------------------------------------------------------*
015100001108     C     CTRVD1        BEGSR
015200000000     C*
015300001108     C* Reimposta le variabili di lavoro
015400000000     C                   MOVEL     '0'           $err
015500000000     C*---
015600001108     C* Simulazione
015700000000     C*---
015800001108     C                   IF        $err = '0'
015900001108     C                   IF        v1csim = 'S' OR
016000001108     C                             v1csim = 'N'
016100001108     C                   ELSE
016200000000     C                   SETON                                        2090
016300000000     C                   MOVEL     msg(1)        $msg
016400000000     C                   MOVEL     '1'           $err
016500001108     C                   ENDIF
016600001108     C                   ENDIF
016700000000     C*---
016800001108     C* Versione
016900000000     C*---
017000001108     C                   IF        $err = '0'
017100000000     C                   MOVEL     *blanks       v1dver
017200000000     C                   Z-ADD     v1cver        kprver
017300001108     C     kprver        CHAIN     azcpr01l
017400001108     C                   IF        %FOUND(azcpr01l)
017500001108     C                   MOVEL     cprdes        v1dver
017600001108     C                   ELSE
017700000000     C                   SETON                                        2190
017800000000     C                   MOVEL     '1'           $err
017900000000     C                   MOVEL     msg(2)        $msg
018000001108     C                   ENDIF
018100001108     C                   ENDIF
018200000000     C*
018300001108     C                   ENDSR
018400001108     C*--------------------------------------------------------------------------------------------*
018500001108     C* F03GES - Gestione tasto funzionale F03 ==> Fine
018600001108     C*--------------------------------------------------------------------------------------------*
018700001108     C     F03GES        BEGSR
018800000000     C*
018900000000     C                   MOVEL     'S'           $fine                          *fine programma
019000000000     C*
019100000000     C                   ENDSR
019200001108     C*--------------------------------------------------------------------------------------------*
019300001108     C* F06GES - Gestione tasto funzionale F06 ==> Conferma
019400001108     C*--------------------------------------------------------------------------------------------*
019500001108     C     F06GES        BEGSR
019600001108     C*
019700001108     C* Apre il file di stampa
019800001108     C                   OPEN      qsysprt
019900001109     C*
020000001109     C* Imposto subito il "Tipo lancio" in testata
020100001109     C                   IF        v1csim = 'N'                                 *NO simulazione
020200001109     C                   EVAL      lancio = '(AGG.)'
020300001109     C                   ENDIF
020400001108     C*
020500001108     C* Esegue routine di elaborazione
020600001108     C                   EXSR      inscpl
020700001108     C                   EXSR      inscps
020800001108     C*
020900001108     C* Chiude il file di stampa
021000001108     C                   CLOSE     qsysprt
021100001108     C*
021200001108     C* Invia messaggio
021300001108     C                   SETON                                        90
021400001108     C                   MOVEL     msg(10)       $msg
021500001108     C*
021600001108     C                   ENDSR
021700001108     C*--------------------------------------------------------------------------------------------*
021800001108     C* F10GES - Gestione tasto funzionale F10 ==> Ricerca versioni
021900001108     C*--------------------------------------------------------------------------------------------*
022000001108     C     F10GES        BEGSR
022100001108     C*
022200001108     C* Lancia il programma di gestione versioni in ricerca
022300001108     C                   CLEAR                   dssi00                         *pulisce ds
022400001108     C                   MOVEL     'R01'         d00op0                         *livello procedura
022500001108     C                   MOVEL     *blanks       d00op1                         *livello programma
022600001108     C                   MOVEL     '0'           d00f03                         *premuto F03
022700001108     C                   MOVEL     '0'           d00f12                         *premuto F12
022800001108     C                   MOVEL     '0'           d00err                         *errore
022900001108     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
023000001108     C                   CALL      'TISI46R'                                    *gestione versioni
023100001108     C                   PARM                    kpjba
023200001108     C                   PARM                    dssi00
023300001108     C                   IF        d00err='1'                                   *errore
023400000000     C                   SETON                                        2190
023500000000     C                   MOVEL     msg(2)        $msg
023600001108     C                   ELSE                                                   *no errore
023700001108     C                   Z-ADD     d00ver        v1cver
023800001108     C                   ENDIF
023900001108     C                   MOVEL     '1'           $tesvd
024000000000     C*
024100000000     C                   ENDSR
024200001108     C*--------------------------------------------------------------------------------------------*
024300001108     C* F12GES - Gestione tasto funzionale F12 ==> Ritorno
024400001108     C*--------------------------------------------------------------------------------------------*
024500001108     C     F12GES        BEGSR
024600000000     C*
024700000000     C                   MOVEL     'S'           $fine                          *fine programma
024800000000     C*
024900000000     C                   ENDSR
025000000000     C*--------------------------------------------------------------------------------------------*
025100001108     C* INSCPL - Creazione nuovi sinonimi da AZCPL
025200000000     C*--------------------------------------------------------------------------------------------*
025300001108     C     INSCPL        BEGSR
025400001108     C*
025500001108     C* Imposta variabili di lavoro
025600001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
025700001108     C                   MOVEL     'CPL'         tip
025800001108     C*
025900001108     C* Legge le località AZCPL
026000001108     C     V1CVER        SETLL     azcpl01l
026100001108     C     V1CVER        READE     azcpl01l
026200001108     C                   DOW       not %eof(azcpl01l)
026300001108     C                   IF        cplatb = *blanks AND                         *no annullati
026400000000     C                             cplfi1 = *blanks                             *no obsoleti
026500000000     C*
026600001108     C* Procedo con le considerazioni da fare:
026700001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
026800001108     C                   MOVEL     '0'           CHKINS
026900001108     C*                  EVAL      posapice = %scan('''':cplloc)
027000001108     C*                  IF        posapice > 0 and
027100001108     C*                            %subst(cplloc:(posapice+1):1) <> ' '
027200001108     C     '''':' '      XLATE     cplloc        depsin
027300001108     C                   IF        cplloc <> depsin
027400001108     C*
027500001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
027600001108     C                   Z-ADD     cplver        kpsver
027700001108     C                   MOVEL     cplnar        kpsnar
027800001108     C                   MOVEL     cplcap        kpscap
027900001108     C                   MOVEL     cplloc        kpsloc
028000001108     C                   MOVEL     depsin        kpssin
028100001108     C     keycps        CHAIN(N)  azcps01l
028200001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
028300001108     C                   Z-ADD     kpsver        cpsver                         *chiave
028400001108     C                   MOVEL     kpsnar        cpsnar
028500001108     C                   MOVEL     kpscap        cpscap
028600001108     C                   MOVEL     kpsloc        cpsloc
028700001108     C                   MOVEL     kpssin        cpssin
028800001108     C                   MOVEL     *blanks       cpsatb                         *campi
028900001108     C                   MOVEL     *blanks       cpsftr
029000001108     C                   Z-ADD     *zeros        cpsdtr
029100001108     C                   IF        v1csim = 'N'                                 *NO simulazione
029200001108     C                   WRITE     azcps000                                     *INSERIMENTO
029300001108     C                   MOVEL     '1'           CHKINS
029400001108     C                   ENDIF
029500001108     C* Stampa riga di verifica
029600001108     C                   MOVEL     msp(1)        stmmsg
029700001108     C                   EXSR      stadet
029800001108     C                   ENDIF
029900001108     C                   ENDIF
030000001108     C*                  ENDIF
030100001108     C*
030200001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
030300001108     C                   MOVEL     '0'           CHKINS
030400001108     C                   Z-ADD     *HIVAL        posapice
030500001108     C                   EVAL      depsin = %trim(cplloc)
030600001108     C                   EVAL      lungh = %len(%trim(depsin))
030700001108     C                   DOU       posapice = *zeros
030800001108     C                   EVAL      posapice = %scan('''':depsin)
030900001108     C                   IF        posapice > 0 and posapice <= lungh and
031000001108     C                             %subst(depsin:(posapice+1):1) <> ' '
031100001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
031200001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
031300001108     C                   EVAL      lungh = %len(%trim(depsin))
031400001108     C                   ELSE
031500001108     C                   LEAVE
031600001108     C                   ENDIF
031700001108     C                   ENDDO
031800001108     C*
031900001108     C                   IF        depsin <> %trim(cplloc)
032000001108     C*
032100001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
032200001108     C                   Z-ADD     cplver        kpsver
032300001108     C                   MOVEL     cplnar        kpsnar
032400001108     C                   MOVEL     cplcap        kpscap
032500001108     C                   MOVEL     cplloc        kpsloc
032600001108     C                   MOVEL     depsin        kpssin
032700001108     C     keycps        CHAIN(N)  azcps01l
032800001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
032900001108     C                   Z-ADD     kpsver        cpsver                         *chiave
033000001108     C                   MOVEL     kpsnar        cpsnar
033100001108     C                   MOVEL     kpscap        cpscap
033200001108     C                   MOVEL     kpsloc        cpsloc
033300001108     C                   MOVEL     kpssin        cpssin
033400001108     C                   MOVEL     *blanks       cpsatb                         *campi
033500001108     C                   MOVEL     *blanks       cpsftr
033600001108     C                   Z-ADD     *zeros        cpsdtr
033700001108     C                   IF        v1csim = 'N'                                 *NO simulazione
033800001108     C                   WRITE     azcps000                                     *INSERIMENTO
033900001108     C                   MOVEL     '1'           CHKINS
034000001108     C                   ENDIF
034100001108     C* Stampa riga di verifica
034200001108     C                   MOVEL     msp(1)        stmmsg
034300001108     C                   EXSR      stadet
034400001108     C                   ENDIF
034500001108     C                   ENDIF
034600001108     C*
034700001108     C* Verifica di correttezza impostazione flag su AZCPL
034800001108     C                   IF        CHKINS = '1' AND
034900001108     C                             cpless <> 'S'
035000001108     C                   MOVEL     'S'           cpless
035100001108     C                   MOVEL     *blanks       cplftr
035200001108     C                   Z-ADD     *zeros        cpldtr
035300001108     C                   UPDATE    azcpl000
035400001108     C                   ENDIF
035500001108     C                   ENDIF
035600001108     C     V1CVER        READE     azcpl01l                               99
035700001108     C                   ENDDO
035800000000     C*
035900000000     C                   ENDSR
036000001108     C*--------------------------------------------------------------------------------------------*
036100001108     C* INSCPS - Creazione nuovi sinonimi da AZCPS
036200001108     C*--------------------------------------------------------------------------------------------*
036300001108     C     INSCPS        BEGSR
036400001108     C*
036500001108     C* Imposta variabili di lavoro
036600001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
036700001108     C                   MOVEL     'CPS'         tip
036800001108     C*
036900001108     C* Legge le località AZCPL
037000001108     C     V1CVER        SETLL     azcps04l
037100001108     C     V1CVER        READE     azcps04l
037200001108     C                   DOW       not %eof(azcps04l)
037300001108     C                   IF        w_cpsatb = *blanks                             *no annullati
037400001108     C*
037500001108     C* Procedo con le considerazioni da fare:
037600001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
037700001108     C*                  EVAL      posapice = %scan('''':w_cpssin)
037800001108     C*                  IF        posapice > 0 and
037900001108     C*                            %subst(w_cpssin:(posapice+1):1) <> ' '
038000001108     C     '''':' '      XLATE     w_cpssin      depsin
038100001108     C                   IF        w_cpssin <> depsin and
038200001108     C                             w_cpsloc <> depsin
038300001108     C*
038400001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
038500001108     C                   Z-ADD     w_cpsver      kpsver
038600001108     C                   MOVEL     w_cpsnar      kpsnar
038700001108     C                   MOVEL     w_cpscap      kpscap
038800001108     C                   MOVEL     w_cpsloc      kpsloc
038900001108     C                   MOVEL     depsin        kpssin
039000001108     C     keycps        CHAIN(N)  azcps01l
039100001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
039200001108     C                   Z-ADD     kpsver        cpsver                         *chiave
039300001108     C                   MOVEL     kpsnar        cpsnar
039400001108     C                   MOVEL     kpscap        cpscap
039500001108     C                   MOVEL     kpsloc        cpsloc
039600001108     C                   MOVEL     kpssin        cpssin
039700001108     C                   MOVEL     *blanks       cpsatb                         *campi
039800001108     C                   MOVEL     *blanks       cpsftr
039900001108     C                   Z-ADD     *zeros        cpsdtr
040000001108     C                   IF        v1csim = 'N'                                 *NO simulazione
040100001108     C                   WRITE     azcps000                                     *INSERIMENTO
040200001108     C                   ENDIF
040300001108     C* Stampa riga di verifica
040400001108     C                   MOVEL     msp(2)        stmmsg
040500001108     C                   EXSR      stadet
040600001108     C                   ENDIF
040700001108     C                   ENDIF
040800001108     C*                  ENDIF
040900001108     C*
041000001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
041100001108     C                   MOVEL     '0'           CHKINS
041200001108     C                   Z-ADD     *HIVAL        posapice
041300001108     C                   EVAL      depsin = %trim(w_cpssin)
041400001108     C                   EVAL      lungh = %len(%trim(depsin))
041500001108     C                   DOU       posapice = *zeros
041600001108     C                   EVAL      posapice = %scan('''':depsin)
041700001108     C                   IF        posapice > 0 and posapice <= lungh and
041800001108     C                             %subst(depsin:(posapice+1):1) <> ' '
041900001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
042000001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
042100001108     C                   EVAL      lungh = %len(%trim(depsin))
042200001108     C                   ELSE
042300001108     C                   LEAVE
042400001108     C                   ENDIF
042500001108     C                   ENDDO
042600001108     C*
042700001108     C                   IF        depsin <> %trim(w_cpssin) and
042800001108     C                             depsin <> %trim(w_cpsloc)
042900001108     C*
043000001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
043100001108     C                   Z-ADD     w_cpsver      kpsver
043200001108     C                   MOVEL     w_cpsnar      kpsnar
043300001108     C                   MOVEL     w_cpscap      kpscap
043400001108     C                   MOVEL     w_cpsloc      kpsloc
043500001108     C                   MOVEL     depsin        kpssin
043600001108     C     keycps        CHAIN(N)  azcps01l
043700001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
043800001108     C                   Z-ADD     kpsver        cpsver                         *chiave
043900001108     C                   MOVEL     kpsnar        cpsnar
044000001108     C                   MOVEL     kpscap        cpscap
044100001108     C                   MOVEL     kpsloc        cpsloc
044200001108     C                   MOVEL     kpssin        cpssin
044300001108     C                   MOVEL     *blanks       cpsatb                         *campi
044400001108     C                   MOVEL     *blanks       cpsftr
044500001108     C                   Z-ADD     *zeros        cpsdtr
044600001108     C                   IF        v1csim = 'N'                                 *NO simulazione
044700001108     C                   WRITE     azcps000                                     *INSERIMENTO
044800001108     C                   ENDIF
044900001108     C* Stampa riga di verifica
045000001108     C                   MOVEL     msp(2)        stmmsg
045100001108     C                   EXSR      stadet
045200001108     C                   ENDIF
045300001108     C                   ENDIF
045400001108     C                   ENDIF
045500001108     C     V1CVER        READE     azcps04l
045600001108     C                   ENDDO
045700001108     C*
045800001108     C                   ENDSR
045900001108     C*--------------------------------------------------------------------------------------------*
046000001108     C* STADET - Stampa esito elaborazione
046100001108     C*--------------------------------------------------------------------------------------------*
046200001108     C     STADET        BEGSR
046300001108     C*
046400001108     C* Stampa testata
046500001108     C                   IF        *inof                                        *fine pagina
046600001108     C                   EXCEPT    tes
046700001108     C                   MOVEL     '0'           *inof
046800001108     C                   ENDIF
046900001108     C*
047000001108     C* Stampa dettaglio
047100001108     C                   Z-ADD     kpsver        stmver
047200001108     C                   MOVEL     kpsnar        stmnar
047300001108     C                   MOVEL     kpscap        stmcap
047400001108     C                   IF        tip = 'CPL'
047500001108     C                   MOVEL     kpsloc        stmloc
047600001108     C                   ENDIF
047700001108     C                   IF        tip = 'CPS'
047800001108     C                   MOVEL     w_cpssin      stmloc
047900001108     C                   ENDIF
048000001108     C                   MOVEL     kpssin        stmsin
048100001108     C                   EXCEPT    det
048200001108     C*
048300001108     C                   ENDSR
048400001108     C*--------------------------------------------------------------------------------------------*
048500001108     C* IMPVD0 - Impostazioni testata
048600001108     C*--------------------------------------------------------------------------------------------*
048700001108     C     IMPVD0        BEGSR
048800000000     C*
048900000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
049000000000     C*
049100000000     C                   ENDSR
049200001108     C*--------------------------------------------------------------------------------------------*
049300001108     C* *INZSR - Operazioni iniziali
049400001108     C*--------------------------------------------------------------------------------------------*
049500001108     C     *INZSR        BEGSR
049600001108     C*
049700001108     C* Ricevimento parametri
049800001108     C     *ENTRY        PLIST
049900001108     C                   PARM                    kpjba
050000000000     C*
050100001108     C* Chiavi di lettura
050200000000     C     keycps        klist                                                  *azcps00f
050300000000     C                   KFLD                    kpsver                          -versione
050400000000     C                   KFLD                    kpsnar                          -nazione
050500000000     C                   KFLD                    kpscap                          -cap
050600000000     C                   KFLD                    kpsloc                          -località
050700000000     C                   KFLD                    kpssin                          -sinonimo
050800000000     C*
050900001108     C* Reperisce la data corrente
051000000000     C                   TIME                    n14                            *ora (6)+ data (8)
051100000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
051200000000     C                   Z-ADD     n8            datcor                         *Data corrente
051300000000     C*
051400001108     C* reperimento dati utente
051500001108     C                   CALL      'XFIL'
051600001108     C                   PARM                    dsfil
051700001108     C*
051800001108     C                   ENDSR
051900001108     O*--------------------------------------------------------------------------------------------*
052000001108     O* Stampa
052100001108     O*--------------------------------------------------------------------------------------------*
052200000000     Oqsysprt   E            tes              02
052300000000     O                       sta(1)              66
052400000000     O                       sta(2)             132
052500001109     O                       lancio             105
052600000000     O                       datcor        8    117
052700000000     O                       PAGE          Z    132
052800000000     O*------------------------
052900000000     O          E            tes              03
053000000000     O                       sta(3)              66
053100000000     O                       sta(3)             132
053200000000     O*------------------------
053300000000     O          E            tes              04
053400000000     O                       sta(4)              66
053500000000     O                       sta(6)             132
053600000000     O*------------------------
053700000000     O          E            tes              05
053800000000     O                       sta(3)              66
053900000000     O                       sta(3)             132
054000000000     O*------------------------
054100000000     O          E            det         1
054200000000     O                       stmver        Z      5
054300000000     O                       stmnar               9
054400000000     O                       stmcap              19
054500000000     O                       stmloc              55
054600000000     O                       stmsin              91
054700000000     O                       stmmsg             132
054800000000     O*--------------------------------------------------------------------------------------------*
054900000000** MSG -Messaggi video-
055000001108TISI78R- Immettere uno dei valori possibili                                   1
055100001108TISI78R- Versione inesistente                                                 2
055200001108TISI78R- 3
055300001108TISI78R- 4
055400001108TISI78R- 5
055500001108TISI78R- 6
055600001108TISI78R- 7
055700001108TISI78R- 8
055800001108TISI78R- 9
055900001108TISI78R- Elaborazione eseguita ...                                            10
056000000000** STA -Righe di stampa -
056100001108TISI78R                            ***  Aggiunta sinonimi da AZCPL1
056200001108 e AZCPS  ***                            99/99/99         PAG.99992
056300000000------------------------------------------------------------------3
056400001108VER   NAZ CAP       LOCALITA'/SIN. ESISTENTE            SINONIMO  4
05650000000099999 XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX5
056600000000                          ESITO                                   6
056700000000XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7
0568000000008
0569000000009
05700000000010
057100000000** MSP -Messaggi stampa-
057200001108CREATO SINONIMO DA LOC.                                           1
057300001108AGGIUNTO SINONIMO DA SIN.                                         2
0574000011083
0575000011084
0576000000005
