000100001108     H*--------------------------------------------------------------------------------------------*
000200001108     H* Allineamento archivi per clienti - Richiesta parametri
000300001108     H*--------------------------------------------------------------------------------------------*
000400001108     H DECEDIT('0,') DATEDIT(*DMY.)
000500001108     F*--------------------------------------------------------------------------------------------*
000600001108     F* Data base
000700001108     F*--------------------------------------------------------------------------------------------*
000800001108     Ftisi78d   CF   E             WORKSTN
000900001108     Fazcpr01l  IF   E           K DISK
000901151130     Fazcpc01l  IF   E           K DISK
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
003701151130     D kpcver          S                   LIKE(cpcver)                         *per azcps00f
003702151130     D kpcnar          S                   LIKE(cpcnar)
003703151130     D kpccap          S                   LIKE(cpccap)
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
006800001108     D dssi00        E DS                  EXTNAME(tisi00ds)                    reperimento versioni
006900001108     D kpjba         E DS                                                       architettura
007000001108     C*--------------------------------------------------------------------------------------------*
007100001108     C* MAIN LINES
007200001108     C*--------------------------------------------------------------------------------------------*
007300000000     C*
007400001108     C* Impostazioni testata
007500000000     C                   EXSR      impvd0
007600000000     C*
007700001108     C* Impostazioni prima videata
007800000000     C                   EXSR      impvd1
007900000000     C*
008000001108     C* Ciclo fino a richiesta fine elaborazione
008100001108     C                   DOU       $fine='S'
008200000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
008300000000     C                   ENDCS
008400001108     C                   ENDDO
008500000000     C*
008600001108     C                   SETON                                        LR
008700001108     C*--------------------------------------------------------------------------------------------*
008800001108     C* IMPVD1 - Impostazioni prima videata
008900001108     C*--------------------------------------------------------------------------------------------*
009000001108     C     IMPVD1        BEGSR
009100001108     C*
009200001108     C* Reperisce l'ultima versione
009300001108     C                   Z-ADD     *zeros        p96dri                         *ultima versione
009400001108     C                   Z-ADD     *zeros        p96ver
009500001108     C                   CALL      'TISI96R'
009600001108     C                   PARM                    p96dri
009700001108     C                   PARM                    p96ver
009800001108     C                   Z-ADD     p96ver        v1cver                         *versione
009900000000     C                   MOVEL     'S'           v1csim                         *simulazione
010000000000     C*
010100001108     C* Imposta formato video e indicatori
010200000000     C                   EXSR      setvd1
010300000000     C                   MOVEL     '1'           $tipvid                        *gestione video
010400000000     C*
010500000000     C                   ENDSR
010600001108     C*--------------------------------------------------------------------------------------------*
010700001108     C* SETVD1 - Impostazioni indicatori prima videata
010800001108     C*--------------------------------------------------------------------------------------------*
010900001108     C     SETVD1        BEGSR
011000000000     C*
011100001108     C                   SETOFF                                       2021       *errori
011200000000     C                   SETOFF                                       90         *generico di errore
011300000000     C                   MOVEL     *blanks       $msg                            *messaggio
011400000000     C*
011500000000     C                   ENDSR
011600001108     C*--------------------------------------------------------------------------------------------*
011700001108     C* GESVD1 - Gestione prima videata
011800001108     C*--------------------------------------------------------------------------------------------*
011900001108     C     GESVD1        BEGSR
012000000000     C*
012100001108     C* Emissione videata
012200001108     C                   IF        $tesvd='1'
012300001108     C                   WRITE     si78d00                                      *testata
012400000000     C                   MOVEL     '0'           $tesvd                         *no testata
012500001108     C                   ENDIF
012600001108     C                   EXFMT     si78d01                                      *video
012700000000     C                   EXSR      setvd1
012800000000     C* F03-Fine
012900001108     C                   IF        *inkc
013000000000     C                   EXSR      f03ges
013100001108     C                   ENDIF
013200001108     C* F10-Ricerca Versione
013300001108     C                   IF        *inkj
013400000000     C                   EXSR      f10ges
013500001108     C                   ENDIF
013600000000     C* F12-Ritorno
013700001108     C                   IF        *inkl
013800000000     C                   EXSR      f12ges
013900001108     C                   ENDIF
014000000000     C* F06-Conferma
014100001108     C                   IF        *inkf
014200001108     C                   EXSR      ctrvd1                                       *controllo parametri
014300001108     C  N90              EXSR      f06ges                                       *se no errori
014400001108     C                   ENDIF
014500000000     C*
014600001108     C                   ENDSR
014700001108     C*--------------------------------------------------------------------------------------------*
014800001108     C* CTRVD1 - Controlla prima videata
014900001108     C*--------------------------------------------------------------------------------------------*
015000001108     C     CTRVD1        BEGSR
015100000000     C*
015200001108     C* Reimposta le variabili di lavoro
015300000000     C                   MOVEL     '0'           $err
015400000000     C*---
015500001108     C* Simulazione
015600000000     C*---
015700001108     C                   IF        $err = '0'
015800001108     C                   IF        v1csim = 'S' OR
015900001108     C                             v1csim = 'N'
016000001108     C                   ELSE
016100000000     C                   SETON                                        2090
016200000000     C                   MOVEL     msg(1)        $msg
016300000000     C                   MOVEL     '1'           $err
016400001108     C                   ENDIF
016500001108     C                   ENDIF
016600000000     C*---
016700001108     C* Versione
016800000000     C*---
016900001108     C                   IF        $err = '0'
017000000000     C                   MOVEL     *blanks       v1dver
017100000000     C                   Z-ADD     v1cver        kprver
017200001108     C     kprver        CHAIN     azcpr01l
017300001108     C                   IF        %FOUND(azcpr01l)
017400001108     C                   MOVEL     cprdes        v1dver
017500001108     C                   ELSE
017600000000     C                   SETON                                        2190
017700000000     C                   MOVEL     '1'           $err
017800000000     C                   MOVEL     msg(2)        $msg
017900001108     C                   ENDIF
018000001108     C                   ENDIF
018100000000     C*
018200001108     C                   ENDSR
018300001108     C*--------------------------------------------------------------------------------------------*
018400001108     C* F03GES - Gestione tasto funzionale F03 ==> Fine
018500001108     C*--------------------------------------------------------------------------------------------*
018600001108     C     F03GES        BEGSR
018700000000     C*
018800000000     C                   MOVEL     'S'           $fine                          *fine programma
018900000000     C*
019000000000     C                   ENDSR
019100001108     C*--------------------------------------------------------------------------------------------*
019200001108     C* F06GES - Gestione tasto funzionale F06 ==> Conferma
019300001108     C*--------------------------------------------------------------------------------------------*
019400001108     C     F06GES        BEGSR
019500001108     C*
019600001108     C* Apre il file di stampa
019700001108     C                   OPEN      qsysprt
019800001109     C*
019900001109     C* Imposto subito il "Tipo lancio" in testata
020000001109     C                   IF        v1csim = 'N'                                 *NO simulazione
020100001109     C                   EVAL      lancio = '(AGG.)'
020200001109     C                   ENDIF
020300001108     C*
020400001108     C* Esegue routine di elaborazione
020500001108     C                   EXSR      inscpl
020600001108     C                   EXSR      inscps
020700001108     C*
020800001108     C* Chiude il file di stampa
020900001108     C                   CLOSE     qsysprt
021000001108     C*
021100001108     C* Invia messaggio
021200001108     C                   SETON                                        90
021300001108     C                   MOVEL     msg(10)       $msg
021400001108     C*
021500001108     C                   ENDSR
021600001108     C*--------------------------------------------------------------------------------------------*
021700001108     C* F10GES - Gestione tasto funzionale F10 ==> Ricerca versioni
021800001108     C*--------------------------------------------------------------------------------------------*
021900001108     C     F10GES        BEGSR
022000001108     C*
022100001108     C* Lancia il programma di gestione versioni in ricerca
022200001108     C                   CLEAR                   dssi00                         *pulisce ds
022300001108     C                   MOVEL     'R01'         d00op0                         *livello procedura
022400001108     C                   MOVEL     *blanks       d00op1                         *livello programma
022500001108     C                   MOVEL     '0'           d00f03                         *premuto F03
022600001108     C                   MOVEL     '0'           d00f12                         *premuto F12
022700001108     C                   MOVEL     '0'           d00err                         *errore
022800001108     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
022900001108     C                   CALL      'TISI46R'                                    *gestione versioni
023000001108     C                   PARM                    kpjba
023100001108     C                   PARM                    dssi00
023200001108     C                   IF        d00err='1'                                   *errore
023300000000     C                   SETON                                        2190
023400000000     C                   MOVEL     msg(2)        $msg
023500001108     C                   ELSE                                                   *no errore
023600001108     C                   Z-ADD     d00ver        v1cver
023700001108     C                   ENDIF
023800001108     C                   MOVEL     '1'           $tesvd
023900000000     C*
024000000000     C                   ENDSR
024100001108     C*--------------------------------------------------------------------------------------------*
024200001108     C* F12GES - Gestione tasto funzionale F12 ==> Ritorno
024300001108     C*--------------------------------------------------------------------------------------------*
024400001108     C     F12GES        BEGSR
024500000000     C*
024600000000     C                   MOVEL     'S'           $fine                          *fine programma
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*--------------------------------------------------------------------------------------------*
025000001108     C* INSCPL - Creazione nuovi sinonimi da AZCPL
025100000000     C*--------------------------------------------------------------------------------------------*
025200001108     C     INSCPL        BEGSR
025300001108     C*
025400001108     C* Imposta variabili di lavoro
025500001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
025600001108     C                   MOVEL     'CPL'         tip
025700001108     C*
025800001108     C* Legge le località AZCPL
025900001108     C     V1CVER        SETLL     azcpl01l
026000001108     C     V1CVER        READE     azcpl01l
026100001108     C                   DOW       not %eof(azcpl01l)
026200001108     C                   IF        cplatb = *blanks AND                         *no annullati
026300000000     C                             cplfi1 = *blanks                             *no obsoleti
026400000000     C*
026500001108     C* Procedo con le considerazioni da fare:
026600001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
026700001108     C                   MOVEL     '0'           CHKINS
026800001108     C*                  EVAL      posapice = %scan('''':cplloc)
026900001108     C*                  IF        posapice > 0 and
027000001108     C*                            %subst(cplloc:(posapice+1):1) <> ' '
027100001108     C     '''':' '      XLATE     cplloc        depsin
027200001108     C                   IF        cplloc <> depsin
027300001108     C*
027400001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
027500001108     C                   Z-ADD     cplver        kpsver
027600001108     C                   MOVEL     cplnar        kpsnar
027700001108     C                   MOVEL     cplcap        kpscap
027800001108     C                   MOVEL     cplloc        kpsloc
027900001108     C                   MOVEL     depsin        kpssin
028000001108     C     keycps        CHAIN(N)  azcps01l
028100001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
028200001108     C                   Z-ADD     kpsver        cpsver                         *chiave
028300001108     C                   MOVEL     kpsnar        cpsnar
028400001108     C                   MOVEL     kpscap        cpscap
028500001108     C                   MOVEL     kpsloc        cpsloc
028600001108     C                   MOVEL     kpssin        cpssin
028700001108     C                   MOVEL     *blanks       cpsatb                         *campi
028800001108     C                   MOVEL     *blanks       cpsftr
028900001108     C                   Z-ADD     *zeros        cpsdtr
029000001108     C                   IF        v1csim = 'N'                                 *NO simulazione
029100001108     C                   WRITE     azcps000                                     *INSERIMENTO
029200001108     C                   MOVEL     '1'           CHKINS
029300001108     C                   ENDIF
029400001108     C* Stampa riga di verifica
029500001108     C                   MOVEL     msp(1)        stmmsg
029600001108     C                   EXSR      stadet
029700001108     C                   ENDIF
029800001108     C                   ENDIF
029900001108     C*                  ENDIF
030000001108     C*
030100001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
030200001108     C                   MOVEL     '0'           CHKINS
030300001108     C                   Z-ADD     *HIVAL        posapice
030400001108     C                   EVAL      depsin = %trim(cplloc)
030500001108     C                   EVAL      lungh = %len(%trim(depsin))
030600001108     C                   DOU       posapice = *zeros
030700001108     C                   EVAL      posapice = %scan('''':depsin)
030800001108     C                   IF        posapice > 0 and posapice <= lungh and
030900001108     C                             %subst(depsin:(posapice+1):1) <> ' '
031000001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
031100001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
031200001108     C                   EVAL      lungh = %len(%trim(depsin))
031300001108     C                   ELSE
031400001108     C                   LEAVE
031500001108     C                   ENDIF
031600001108     C                   ENDDO
031700001108     C*
031800001108     C                   IF        depsin <> %trim(cplloc)
031900001108     C*
032000001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
032100001108     C                   Z-ADD     cplver        kpsver
032200001108     C                   MOVEL     cplnar        kpsnar
032300001108     C                   MOVEL     cplcap        kpscap
032400001108     C                   MOVEL     cplloc        kpsloc
032500001108     C                   MOVEL     depsin        kpssin
032600001108     C     keycps        CHAIN(N)  azcps01l
032700001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
032800001108     C                   Z-ADD     kpsver        cpsver                         *chiave
032900001108     C                   MOVEL     kpsnar        cpsnar
033000001108     C                   MOVEL     kpscap        cpscap
033100001108     C                   MOVEL     kpsloc        cpsloc
033200001108     C                   MOVEL     kpssin        cpssin
033300001108     C                   MOVEL     *blanks       cpsatb                         *campi
033400001108     C                   MOVEL     *blanks       cpsftr
033500001108     C                   Z-ADD     *zeros        cpsdtr
033600001108     C                   IF        v1csim = 'N'                                 *NO simulazione
033700001108     C                   WRITE     azcps000                                     *INSERIMENTO
033800001108     C                   MOVEL     '1'           CHKINS
033900001108     C                   ENDIF
034000001108     C* Stampa riga di verifica
034100001108     C                   MOVEL     msp(1)        stmmsg
034200001108     C                   EXSR      stadet
034300001108     C                   ENDIF
034400001108     C                   ENDIF
034500001108     C*
034600001108     C* Verifica di correttezza impostazione flag su AZCPL
034700001108     C                   IF        CHKINS = '1' AND
034800001108     C                             cpless <> 'S'
034900001108     C                   MOVEL     'S'           cpless
035000001108     C                   MOVEL     *blanks       cplftr
035100001108     C                   Z-ADD     *zeros        cpldtr
035200001108     C                   UPDATE    azcpl000
035300001108     C                   ENDIF
035400001108     C                   ENDIF
035500001108     C     V1CVER        READE     azcpl01l                               99
035600001108     C                   ENDDO
035700000000     C*
035800000000     C                   ENDSR
035801151130     C*--------------------------------------------------------------------------------------------*
035802151130     C* INSCPLb - Creazione nuovi sinonimi da AZCPL
035803151130     C*--------------------------------------------------------------------------------------------*
035804151130     C     INSCPLb       BEGSR
035805151130     C*
035806151130     C* Imposta variabili di lavoro
035807151130     C                   MOVEL     '1'           *INOF                          *SI fine pagina
035808151130     C                   MOVEL     'CPL'         tip
035809151130     C*
035810151130     C* Legge le località AZCPL
035811151130     C     V1CVER        SETLL     azcpl01l
035812151130     C     V1CVER        READE     azcpl01l
035813151130     C                   DOW       not %eof(azcpl01l)
035814151130     C                   IF        cplatb = *blanks AND                         *no annullati
035815151130     C                             cplfi1 = *blanks                             *no obsoleti
035816151130     C*
035817151130     C                   Z-ADD     cplver        kpcver
035818151130     C                   MOVEL     cplnar        kpcnar
035819151130     C                   MOVEL     cplcap        kpccap
035820151130
035821151130     C* Procedo con le considerazioni da fare:
035822151130     C** 1) ==> Sostituisco <'> con < > (apice con blank)
035823151130     C                   MOVEL     '0'           CHKINS
035824151130     C*                  EVAL      posapice = %scan('''':cplloc)
035825151130     C*                  IF        posapice > 0 and
035826151130     C*                            %subst(cplloc:(posapice+1):1) <> ' '
035827151130     C     '''':' '      XLATE     cplloc        depsin
035828151130     C                   IF        cplloc <> depsin
035829151130     C*
035830151130     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
035831151130     C                   Z-ADD     cplver        kpsver
035832151130     C                   MOVEL     cplnar        kpsnar
035833151130     C                   MOVEL     cplcap        kpscap
035834151130     C                   MOVEL     cplloc        kpsloc
035835151130     C                   MOVEL     depsin        kpssin
035836151130     C     keycps        CHAIN(N)  azcps01l
035837151130     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
035838151130     C                   Z-ADD     kpsver        cpsver                         *chiave
035839151130     C                   MOVEL     kpsnar        cpsnar
035840151130     C                   MOVEL     kpscap        cpscap
035841151130     C                   MOVEL     kpsloc        cpsloc
035842151130     C                   MOVEL     kpssin        cpssin
035843151130     C                   MOVEL     *blanks       cpsatb                         *campi
035844151130     C                   MOVEL     *blanks       cpsftr
035845151130     C                   Z-ADD     *zeros        cpsdtr
035846151130     C                   IF        v1csim = 'N'                                 *NO simulazione
035847151130     C                   WRITE     azcps000                                     *INSERIMENTO
035848151130     C                   MOVEL     '1'           CHKINS
035849151130     C                   ENDIF
035850151130     C* Stampa riga di verifica
035851151130     C                   MOVEL     msp(1)        stmmsg
035852151130     C                   EXSR      stadet
035853151130     C                   ENDIF
035854151130     C                   ENDIF
035855151130     C*                  ENDIF
035856151130     C*
035857151130     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
035858151130     C                   MOVEL     '0'           CHKINS
035859151130     C                   Z-ADD     *HIVAL        posapice
035860151130     C                   EVAL      depsin = %trim(cplloc)
035861151130     C                   EVAL      lungh = %len(%trim(depsin))
035862151130     C                   DOU       posapice = *zeros
035863151130     C                   EVAL      posapice = %scan('''':depsin)
035864151130     C                   IF        posapice > 0 and posapice <= lungh and
035865151130     C                             %subst(depsin:(posapice+1):1) <> ' '
035866151130     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
035867151130     C                             %subst(depsin:(posapice+1):(lungh-posapice))
035868151130     C                   EVAL      lungh = %len(%trim(depsin))
035869151130     C                   ELSE
035870151130     C                   LEAVE
035871151130     C                   ENDIF
035872151130     C                   ENDDO
035873151130     C*
035874151130     C                   IF        depsin <> %trim(cplloc)
035875151130     C*
035876151130     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
035877151130     C                   Z-ADD     cplver        kpsver
035878151130     C                   MOVEL     cplnar        kpsnar
035879151130     C                   MOVEL     cplcap        kpscap
035880151130     C                   MOVEL     cplloc        kpsloc
035881151130     C                   MOVEL     depsin        kpssin
035882151130     C     keycps        CHAIN(N)  azcps01l
035883151130     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
035884151130     C                   Z-ADD     kpsver        cpsver                         *chiave
035885151130     C                   MOVEL     kpsnar        cpsnar
035886151130     C                   MOVEL     kpscap        cpscap
035887151130     C                   MOVEL     kpsloc        cpsloc
035888151130     C                   MOVEL     kpssin        cpssin
035889151130     C                   MOVEL     *blanks       cpsatb                         *campi
035890151130     C                   MOVEL     *blanks       cpsftr
035891151130     C                   Z-ADD     *zeros        cpsdtr
035892151130     C                   IF        v1csim = 'N'                                 *NO simulazione
035893151130     C                   WRITE     azcps000                                     *INSERIMENTO
035894151130     C                   MOVEL     '1'           CHKINS
035895151130     C                   ENDIF
035896151130     C* Stampa riga di verifica
035897151130     C                   MOVEL     msp(1)        stmmsg
035898151130     C                   EXSR      stadet
035899151130     C                   ENDIF
035900151130     C                   ENDIF
035901151130     C*
035902151130     C*
035903151130     C** 3) ==> aggiungo siglia PROV in coda alla località
035904151130     C                   MOVEL     '0'           CHKINS
035907151130     C                   EVAL      lungh = %len(%trim(cplloc))
035910151130     C                   IF        lingh <32
035911151130     C     keycpc        CHAIN(N)  azcpc01l
035912151130     C                   IF        NOT %FOUND(azcpc01l)                         *INESISTENTE
035913151130     C                   EVAL      depsin = cplloc + ' ' + cpcprv           +
035917151130     C                   ENDIF
035918151130     C                   ENDIF
035919151130     C                   ENDDO
035920151130     C*
035923151130     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
035924151130     C                   Z-ADD     cplver        kpsver
035925151130     C                   MOVEL     cplnar        kpsnar
035926151130     C                   MOVEL     cplcap        kpscap
035927151130     C                   MOVEL     cplloc        kpsloc
035928151130     C                   MOVEL     depsin        kpssin
035929151130     C     keycps        CHAIN(N)  azcps01l
035930151130     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
035931151130     C                   Z-ADD     kpsver        cpsver                         *chiave
035932151130     C                   MOVEL     kpsnar        cpsnar
035933151130     C                   MOVEL     kpscap        cpscap
035934151130     C                   MOVEL     kpsloc        cpsloc
035935151130     C                   MOVEL     kpssin        cpssin
035936151130     C                   MOVEL     *blanks       cpsatb                         *campi
035937151130     C                   MOVEL     *blanks       cpsftr
035938151130     C                   Z-ADD     *zeros        cpsdtr
035939151130     C                   IF        v1csim = 'N'                                 *NO simulazione
035940151130     C                   WRITE     azcps000                                     *INSERIMENTO
035941151130     C                   MOVEL     '1'           CHKINS
035942151130     C                   ENDIF
035943151130     C* Stampa riga di verifica
035944151130     C                   MOVEL     msp(1)        stmmsg
035945151130     C                   EXSR      stadet
035946151130     C                   ENDIF
035947151130     C                   ENDIF
035948151130     C*
035949151130     C* Verifica di correttezza impostazione flag su AZCPL
035950151130     C                   IF        CHKINS = '1' AND
035951151130     C                             cpless <> 'S'
035952151130     C                   MOVEL     'S'           cpless
035953151130     C                   MOVEL     *blanks       cplftr
035954151130     C                   Z-ADD     *zeros        cpldtr
035955151130     C                   UPDATE    azcpl000
035956151130     C                   ENDIF
035957151130     C                   ENDIF
035958151130     C     V1CVER        READE     azcpl01l                               99
035959151130     C                   ENDDO
035960151130     C*
035961151130     C                   ENDSR
035962001108     C*--------------------------------------------------------------------------------------------*
036000001108     C* INSCPS - Creazione nuovi sinonimi da AZCPS
036100001108     C*--------------------------------------------------------------------------------------------*
036200001108     C     INSCPS        BEGSR
036300001108     C*
036400001108     C* Imposta variabili di lavoro
036500001108     C                   MOVEL     '1'           *INOF                          *SI fine pagina
036600001108     C                   MOVEL     'CPS'         tip
036700001108     C*
036800001108     C* Legge le località AZCPL
036900001108     C     V1CVER        SETLL     azcps04l
037000001108     C     V1CVER        READE     azcps04l
037100001108     C                   DOW       not %eof(azcps04l)
037200001108     C                   IF        w_cpsatb = *blanks                             *no annullati
037300001108     C*
037400001108     C* Procedo con le considerazioni da fare:
037500001108     C** 1) ==> Sostituisco <'> con < > (apice con blank)
037600001108     C*                  EVAL      posapice = %scan('''':w_cpssin)
037700001108     C*                  IF        posapice > 0 and
037800001108     C*                            %subst(w_cpssin:(posapice+1):1) <> ' '
037900001108     C     '''':' '      XLATE     w_cpssin      depsin
038000001108     C                   IF        w_cpssin <> depsin and
038100001108     C                             w_cpsloc <> depsin
038200001108     C*
038300001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
038400001108     C                   Z-ADD     w_cpsver      kpsver
038500001108     C                   MOVEL     w_cpsnar      kpsnar
038600001108     C                   MOVEL     w_cpscap      kpscap
038700001108     C                   MOVEL     w_cpsloc      kpsloc
038800001108     C                   MOVEL     depsin        kpssin
038900001108     C     keycps        CHAIN(N)  azcps01l
039000001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
039100001108     C                   Z-ADD     kpsver        cpsver                         *chiave
039200001108     C                   MOVEL     kpsnar        cpsnar
039300001108     C                   MOVEL     kpscap        cpscap
039400001108     C                   MOVEL     kpsloc        cpsloc
039500001108     C                   MOVEL     kpssin        cpssin
039600001108     C                   MOVEL     *blanks       cpsatb                         *campi
039700001108     C                   MOVEL     *blanks       cpsftr
039800001108     C                   Z-ADD     *zeros        cpsdtr
039900001108     C                   IF        v1csim = 'N'                                 *NO simulazione
040000001108     C                   WRITE     azcps000                                     *INSERIMENTO
040100001108     C                   ENDIF
040200001108     C* Stampa riga di verifica
040300001108     C                   MOVEL     msp(2)        stmmsg
040400001108     C                   EXSR      stadet
040500001108     C                   ENDIF
040600001108     C                   ENDIF
040700001108     C*                  ENDIF
040800001108     C*
040900001108     C** 2) ==> Sostituisco <'> con <' > (apice con apice+blank)
041000001108     C                   MOVEL     '0'           CHKINS
041100001108     C                   Z-ADD     *HIVAL        posapice
041200001108     C                   EVAL      depsin = %trim(w_cpssin)
041300001108     C                   EVAL      lungh = %len(%trim(depsin))
041400001108     C                   DOU       posapice = *zeros
041500001108     C                   EVAL      posapice = %scan('''':depsin)
041600001108     C                   IF        posapice > 0 and posapice <= lungh and
041700001108     C                             %subst(depsin:(posapice+1):1) <> ' '
041800001108     C                   EVAL      depsin = %subst(depsin:1:posapice) + ' ' +
041900001108     C                             %subst(depsin:(posapice+1):(lungh-posapice))
042000001108     C                   EVAL      lungh = %len(%trim(depsin))
042100001108     C                   ELSE
042200001108     C                   LEAVE
042300001108     C                   ENDIF
042400001108     C                   ENDDO
042500001108     C*
042600001108     C                   IF        depsin <> %trim(w_cpssin) and
042700001108     C                             depsin <> %trim(w_cpsloc)
042800001108     C*
042900001108     C* Controlla che non esista già nei Sinonimi, se non esiste lo inserisco
043000001108     C                   Z-ADD     w_cpsver      kpsver
043100001108     C                   MOVEL     w_cpsnar      kpsnar
043200001108     C                   MOVEL     w_cpscap      kpscap
043300001108     C                   MOVEL     w_cpsloc      kpsloc
043400001108     C                   MOVEL     depsin        kpssin
043500001108     C     keycps        CHAIN(N)  azcps01l
043600001108     C                   IF        NOT %FOUND(azcps01l)                         *INESISTENTE
043700001108     C                   Z-ADD     kpsver        cpsver                         *chiave
043800001108     C                   MOVEL     kpsnar        cpsnar
043900001108     C                   MOVEL     kpscap        cpscap
044000001108     C                   MOVEL     kpsloc        cpsloc
044100001108     C                   MOVEL     kpssin        cpssin
044200001108     C                   MOVEL     *blanks       cpsatb                         *campi
044300001108     C                   MOVEL     *blanks       cpsftr
044400001108     C                   Z-ADD     *zeros        cpsdtr
044500001108     C                   IF        v1csim = 'N'                                 *NO simulazione
044600001108     C                   WRITE     azcps000                                     *INSERIMENTO
044700001108     C                   ENDIF
044800001108     C* Stampa riga di verifica
044900001108     C                   MOVEL     msp(2)        stmmsg
045000001108     C                   EXSR      stadet
045100001108     C                   ENDIF
045200001108     C                   ENDIF
045300001108     C                   ENDIF
045400001108     C     V1CVER        READE     azcps04l
045500001108     C                   ENDDO
045600001108     C*
045700001108     C                   ENDSR
045800001108     C*--------------------------------------------------------------------------------------------*
045900001108     C* STADET - Stampa esito elaborazione
046000001108     C*--------------------------------------------------------------------------------------------*
046100001108     C     STADET        BEGSR
046200001108     C*
046300001108     C* Stampa testata
046400001108     C                   IF        *inof                                        *fine pagina
046500001108     C                   EXCEPT    tes
046600001108     C                   MOVEL     '0'           *inof
046700001108     C                   ENDIF
046800001108     C*
046900001108     C* Stampa dettaglio
047000001108     C                   Z-ADD     kpsver        stmver
047100001108     C                   MOVEL     kpsnar        stmnar
047200001108     C                   MOVEL     kpscap        stmcap
047300001108     C                   IF        tip = 'CPL'
047400001108     C                   MOVEL     kpsloc        stmloc
047500001108     C                   ENDIF
047600001108     C                   IF        tip = 'CPS'
047700001108     C                   MOVEL     w_cpssin      stmloc
047800001108     C                   ENDIF
047900001108     C                   MOVEL     kpssin        stmsin
048000001108     C                   EXCEPT    det
048100001108     C*
048200001108     C                   ENDSR
048300001108     C*--------------------------------------------------------------------------------------------*
048400001108     C* IMPVD0 - Impostazioni testata
048500001108     C*--------------------------------------------------------------------------------------------*
048600001108     C     IMPVD0        BEGSR
048700000000     C*
048800000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
048900000000     C*
049000000000     C                   ENDSR
049100001108     C*--------------------------------------------------------------------------------------------*
049200001108     C* *INZSR - Operazioni iniziali
049300001108     C*--------------------------------------------------------------------------------------------*
049400001108     C     *INZSR        BEGSR
049500001108     C*
049600001108     C* Ricevimento parametri
049700001108     C     *ENTRY        PLIST
049800001108     C                   PARM                    kpjba
049900000000     C*
050000001108     C* Chiavi di lettura
050100000000     C     keycps        klist                                                  *azcps00f
050200000000     C                   KFLD                    kpsver                          -versione
050300000000     C                   KFLD                    kpsnar                          -nazione
050400000000     C                   KFLD                    kpscap                          -cap
050500000000     C                   KFLD                    kpsloc                          -località
050600000000     C                   KFLD                    kpssin                          -sinonimo
050601151130     C     keycpc        klist                                                  *azcps00f
050602151130     C                   KFLD                    kpcver                          -versione
050603151130     C                   KFLD                    kpcnar                          -nazione
050604151130     C                   KFLD                    kpccap                          -cap
050700000000     C*
050800001108     C* Reperisce la data corrente
050900000000     C                   TIME                    n14                            *ora (6)+ data (8)
051000000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
051100000000     C                   Z-ADD     n8            datcor                         *Data corrente
051200001108     C*
051300001108     C                   ENDSR
051400001108     O*--------------------------------------------------------------------------------------------*
051500001108     O* Stampa
051600001108     O*--------------------------------------------------------------------------------------------*
051700000000     Oqsysprt   E            tes              02
051800000000     O                       sta(1)              66
051900000000     O                       sta(2)             132
052000001109     O                       lancio             105
052100000000     O                       datcor        8    117
052200000000     O                       PAGE          Z    132
052300000000     O*------------------------
052400000000     O          E            tes              03
052500000000     O                       sta(3)              66
052600000000     O                       sta(3)             132
052700000000     O*------------------------
052800000000     O          E            tes              04
052900000000     O                       sta(4)              66
053000000000     O                       sta(6)             132
053100000000     O*------------------------
053200000000     O          E            tes              05
053300000000     O                       sta(3)              66
053400000000     O                       sta(3)             132
053500000000     O*------------------------
053600000000     O          E            det         1
053700000000     O                       stmver        Z      5
053800000000     O                       stmnar               9
053900000000     O                       stmcap              19
054000000000     O                       stmloc              55
054100000000     O                       stmsin              91
054200000000     O                       stmmsg             132
054300000000     O*--------------------------------------------------------------------------------------------*
054400000000** MSG -Messaggi video-
054500001108TISI78R- Immettere uno dei valori possibili                                   1
054600001108TISI78R- Versione inesistente                                                 2
054700001108TISI78R- 3
054800001108TISI78R- 4
054900001108TISI78R- 5
055000001108TISI78R- 6
055100001108TISI78R- 7
055200001108TISI78R- 8
055300001108TISI78R- 9
055400001108TISI78R- Elaborazione eseguita ...                                            10
055500000000** STA -Righe di stampa -
055600001108TISI78R                            ***  Aggiunta sinonimi da AZCPL1
055700001108 e AZCPS  ***                            99/99/99         PAG.99992
055800000000------------------------------------------------------------------3
055900001108VER   NAZ CAP       LOCALITA'/SIN. ESISTENTE            SINONIMO  4
05600000000099999 XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX5
056100000000                          ESITO                                   6
056200000000XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7
0563000000008
0564000000009
05650000000010
056600000000** MSP -Messaggi stampa-
056700001108CREATO SINONIMO DA LOC.                                           1
056800001108AGGIUNTO SINONIMO DA SIN.                                         2
0569000011083
0570000011084
0571000000005
