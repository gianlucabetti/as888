000100000000     h*--------------------------------------------------------------------------------------------*
000200000000     h* Allineamento archivi per clienti - Richiesta parametri
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600000000     f* Data base
000700000000     f*--------------------------------------------------------------------------------------------*
000800000000     ftisi87d   CF   E             WORKSTN
000900000000     fazcpr01l  IF   E           K DISK
001000000000     fazcpl01l  IF   E           K DISK
001100000000     fazloc00f  IF   E           K DISK
001200000000     fazcps01l  UF A E           K DISK
001300000726     fazcps04l  UF   E           K DISK
001400000726     f                                     RENAME(azcps000:azcps4)
001500000000     fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001600000000     f                                     USROPN
001700000000     d*--------------------------------------------------------------------------------------------*
001800000000     d* Data structure
001900000000     d*--------------------------------------------------------------------------------------------*
002000000000     d*----------------
002100000726     d* schiere a tempo di compilazione
002200000000     d*----------------
002300000000     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi video
002400000000     d sta             S             66    DIM(10) CTDATA PERRCD(1)             *righe di stampa
002500000000     d msp             S             66    DIM(10) CTDATA PERRCD(1)             *messaggi stampa
002600000726     d*----------------
002700000726     d* schiere
002800000726     d*----------------
002900000731     d sanar           S              3    DIM(500)                             *nazione sin da ANN
003000000731     d sacap           S              9    DIM(500)                             *cap     sin da ANN
003100000731     d sasin           S             35    DIM(500)                             *sinonimo da ANN
003200000000     d*----------------
003300000000     d* Variabili riferite al data base
003400000000     d*----------------
003500000000     d kprver          S                   LIKE(cprver)                         *per azcpr00f
003600000000     d kplver          S                   LIKE(cplver)                         *per azcplo0f
003700000000     d kplnar          S                   LIKE(cplnar)
003800000000     d kplcap          S                   LIKE(cplcap)
003900000000     d kplloc          S                   LIKE(cplloc)
004000000000     d kpsver          S                   LIKE(cpsver)                         *per azcps00f
004100000000     d kpsnar          S                   LIKE(cpsnar)
004200000000     d kpscap          S                   LIKE(cpscap)
004300000000     d kpsloc          S                   LIKE(cpsloc)
004400000726     d kpssin          S                   LIKE(cpssin)                         *deposito
004500000726     d depnar          S                   LIKE(cpsnar)
004600000726     d depcap          S                   LIKE(cpscap)
004700000726     d depsin          S                   LIKE(cpssin)
004800000000     d*----------------
004900000000     d* Variabili di controllo
005000000000     d*----------------
005100000000     d $fine           S              1    INZ('N')                             *Gestione
005200000000     d $err            S              1    INZ('0')                             *errore
005300000000     d $tesvd          S              1    INZ('1')                             *riemettere testata
005400000000     d $tipvid         S              1    INZ('1')                             *gestione video
005500000000     d*----------------
005600000000     d* Variabili a programma
005700000000     d*----------------
005800000726     d s               S              5  0                                      *indice
005900000726     d i               S              5  0                                      *indice
006000000726     d n14             S             14  0                                      *numerico 14
006100000000     d n8              S              8  0                                      *numerico 8
006200000000     d datcor          S              8  0                                      *data corrente a/m/g
006300000000     d asterisco       S              2  0                                      *posizione "*"
006400000726     d ContSinU        S              5  0
006500000000     d wver            S                   LIKE(cprver)                         *di lavoro
006600000000     d wnar            S                   LIKE(cplnar)
006700000000     d wcap            S                   LIKE(cplcap)
006800000000     d wloc            S                   LIKE(cplloc)
006900000000     d wsin            S                   LIKE(cpssin)
007000000000     d wlun            S              2  0
007100000000     d wsinlav         S             90
007200000000     d stmver          S                   LIKE(cpsver)                         *di stampa
007300000000     d stmnar          S                   LIKE(cpsnar)
007400000000     d stmcap          S                   LIKE(cpscap)
007500000000     d stmloc          S                   LIKE(cpsloc)
007600000000     d stmsin          S                   LIKE(cpssin)
007700000000     d stmmsg          S             40
007800000000     d p96dri          S                   LIKE(cprdde)                         *lancio TISI96R
007900000000     d p96ver          S                   LIKE(cprver)
008000000000     d*----------------
008100000000     d* Ds
008200000000     d*----------------
008300000000     d dssi00        E DS                  EXTNAME(tisi00ds)                    reperimento versioni
008400000000     d kpjba         E DS                                                       architettura
008500000000     c*--------------------------------------------------------------------------------------------*
008600000000     c* Main lines
008700000000     c*--------------------------------------------------------------------------------------------*
008800000000     C*
008900000000     C* impostazioni testata
009000000000     C                   EXSR      impvd0
009100000000     C*
009200000000     C* impostazioni prima videata
009300000000     C                   EXSR      impvd1
009400000000     C*
009500000000     C* ciclo fino a richiesta di fine elaborazione
009600000000do  1C                   DOU       $fine='S'
009700000000     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
009800000000     C                   ENDCS
009900000000e   1C                   ENDDO
010000000000     c*
010100000000     c* operazioni finali
010200000000     c                   EXSR      finsr
010300000000     C*
010400000000     C                   SETON                                        lr
010500000000     c*--------------------------------------------------------------------------------------------*
010600000000     c* impvd1 - impostazioni prima videata
010700000000     c*--------------------------------------------------------------------------------------------*
010800000000     C     impvd1        BEGSR
010900000000     c*
011000000000     c* reperisce l'ultima versione
011100000000     c                   Z-ADD     *zeros        p96dri                         *ultima versione
011200000000     c                   Z-ADD     *zeros        p96ver
011300000000     c                   CALL      'TISI96R'
011400000000     c                   PARM                    p96dri
011500000000     c                   PARM                    p96ver
011600000000     c                   Z-ADD     p96ver        v1cver                         *versione
011700000000     C                   MOVEL     'S'           v1csim                         *simulazione
011800000000     C                   MOVEL     'S'           v1ccpl                         *crea da AZCPL
011900000000     C                   Z-ADD     20            v1clun                         *NEW lunghezza
012000000000     C                   MOVEL     'S'           v1cloc                         *crea da AZLOC
012100000726     C                   MOVEL     'S'           v1csse                         *sistema AZCPS
012200000000     C*
012300000000     C* imposta formato video e indicatori
012400000000     C                   EXSR      setvd1
012500000000     C                   MOVEL     '1'           $tipvid                        *gestione video
012600000000     C*
012700000000     C                   ENDSR
012800000000     c*--------------------------------------------------------------------------------------------*
012900000000     c* setvd1 - impostazioni indicatori prima videata
013000000000     c*--------------------------------------------------------------------------------------------*
013100000000     C     setvd1        BEGSR
013200000000     C*
013300000000     C                   SETOFF                                       202122     *errori
013400000726     C                   SETOFF                                       232425
013500000000     C                   SETOFF                                       90         *generico di errore
013600000000     C                   MOVEL     *blanks       $msg                            *messaggio
013700000000     C*
013800000000     C                   ENDSR
013900000000     c*--------------------------------------------------------------------------------------------*
014000000000     C* gesvd1 - gestione prima videata
014100000000     c*--------------------------------------------------------------------------------------------*
014200000000     C     gesvd1        BEGSR
014300000000     C*
014400000000     C* emissione videata
014500000000if  1C                   IF        $tesvd='1'
014600000000     C                   WRITE     si87d00                                      *testata
014700000000     C                   MOVEL     '0'           $tesvd                         *no testata
014800000000e   1C                   ENDIF
014900000000     C                   EXFMT     si87d01                                      *video
015000000000     C                   EXSR      setvd1
015100000000     C* F03-Fine
015200000000if  1C                   IF        *inkc
015300000000     C                   EXSR      f03ges
015400000000     C                   GOTO      finvd1
015500000000e   1C                   ENDIF
015600000000     C* F10-Ricerca
015700000000if  1C                   IF        *inkj
015800000000     C                   EXSR      f10ges
015900000000     C                   GOTO      finvd1
016000000000e   1C                   ENDIF
016100000000     C* F12-Ritorno
016200000000if  1C                   IF        *inkl
016300000000     C                   EXSR      f12ges
016400000000     C                   GOTO      finvd1
016500000000e   1C                   ENDIF
016600000000     C* Controlla video
016700000000     C                   EXSR      ctrvd1
016800000000if  1C                   IF        *in90                                        *errori
016900000000     C                   GOTO      finvd1
017000000000e   1C                   ENDIF
017100000000     C* F06-Conferma
017200000000if  1C                   IF        *inkf
017300000000     C                   EXSR      f06ges
017400000000     C                   GOTO      finvd1
017500000000e   1C                   ENDIF
017600000000     C*
017700000000     C     finvd1        ENDSR
017800000000     c*--------------------------------------------------------------------------------------------*
017900000000     C* ctrvd1 - controlla prima videata
018000000000     c*--------------------------------------------------------------------------------------------*
018100000000     C     ctrvd1        BEGSR
018200000000     C*
018300000000     C* reimposta le variabili di lavoro
018400000000     C                   MOVEL     '0'           $err
018500000000     C*---
018600000000     C* simulazione
018700000000     C*---
018800000000if  1c                   IF        v1csim = 'S' OR
018900000000     c                             v1csim = 'N'
019000000000x   1c                   ELSE
019100000000     C                   SETON                                        2090
019200000000     C                   MOVEL     msg(1)        $msg
019300000000     C                   MOVEL     '1'           $err
019400000000     C                   GOTO      finct1
019500000000e   1C                   ENDIF
019600000000     C*---
019700000000     C* versione
019800000000     C*---
019900000000     C                   MOVEL     *blanks       v1dver
020000000000     C                   Z-ADD     v1cver        kprver
020100000000     c     kprver        CHAIN     azcpr01l
020200000000     c                   IF        %FOUND
020300000000     c                   MOVEL     cprdes        v1dver
020400000000     c                   ELSE
020500000000     C                   SETON                                        2190
020600000000     C                   MOVEL     '1'           $err
020700000000     C                   MOVEL     msg(2)        $msg
020800000000     C                   GOTO      finct1
020900000000     c                   ENDIF
021000000000     C*---
021100000000     C* creazione da AZCPL
021200000000     C*---
021300000000if  1c                   IF        v1ccpl = 'S' OR
021400000000     c                             v1ccpl = 'N'
021500000000x   1c                   ELSE
021600000000     C                   SETON                                        2290
021700000000     C                   MOVEL     msg(1)        $msg
021800000000     C                   MOVEL     '1'           $err
021900000000     C                   GOTO      finct1
022000000000e   1C                   ENDIF
022100000000     C*---
022200000000     C* lunghezza nuovo sinonimo
022300000000     C*---
022400000000if  1c                   IF        v1clun > 0 AND
022500000000     c                             v1clun < 35
022600000000x   1c                   ELSE
022700000000     C                   SETON                                        2390
022800000000     C                   MOVEL     msg(3)        $msg
022900000000     C                   MOVEL     '1'           $err
023000000000     C                   GOTO      finct1
023100000000e   1C                   ENDIF
023200000000     C*---
023300000000     C* creazione da AZLOC
023400000000     C*---
023500000000if  1c                   IF        v1cloc = 'S' OR
023600000000     c                             v1cloc = 'N'
023700000000x   1c                   ELSE
023800000000     C                   SETON                                        2490
023900000000     C                   MOVEL     msg(1)        $msg
024000000000     C                   MOVEL     '1'           $err
024100000000     C                   GOTO      finct1
024200000000e   1C                   ENDIF
024300000726     C*---
024400000726     C* sistema sinonimi esistenti
024500000726     C*---
024600000726if  1c                   IF        v1csse = 'S' OR
024700000726     c                             v1csse = 'N'
024800000726x   1c                   ELSE
024900000726     C                   SETON                                        2590
025000000726     C                   MOVEL     msg(1)        $msg
025100000726     C                   MOVEL     '1'           $err
025200000726     C                   GOTO      finct1
025300000726e   1C                   ENDIF
025400000000     C*
025500000000     C     finct1        ENDSR
025600000000     c*--------------------------------------------------------------------------------------------*
025700000000     C* f03ges - gestione tasto funzionale f03 ==> Fine
025800000000     c*--------------------------------------------------------------------------------------------*
025900000000     C     f03ges        BEGSR
026000000000     C*
026100000000     C                   MOVEL     'S'           $fine                          *fine programma
026200000000     C*
026300000000     C                   ENDSR
026400000000     c*--------------------------------------------------------------------------------------------*
026500000000     C* f06ges - gestione tasto funzionale f06 ==> Conferma
026600000000     c*--------------------------------------------------------------------------------------------*
026700000000     c     f06ges        BEGSR
026800000000     c*
026900000000     c* crea sinonimi da AZCPL
027000000000if  1c                   IF        v1ccpl = 'S'                                 *se richiesto
027100000000     c                   Z-ADD     v1cver        wver                           *versione
027200000000     c                   Z-ADD     v1clun        wlun                           *lunghezza nuova
027300000000     c                   EXSR      crtsincpl                                    *CREAZIONE SINONIMI
027400000000e   1c                   ENDIF
027500000000     c*
027600000000     c* crea sinonimi da AZLOC
027700000000if  1c                   IF        v1cloc = 'S'                                 *se richiesto
027800000000     c                   Z-ADD     v1cver        wver                           *versione
027900000000     c                   EXSR      crtsinloc                                    *CREAZIONE SINONIMI
028000000000e   1c                   ENDIF
028100000726     c*
028200000726     c* sistema sinonimi esistenti
028300000726if  1c                   IF        v1csse = 'S'                                 *se richiesto
028400000726     c                   Z-ADD     v1cver        wver                           *versione
028500000726     c                   EXSR      aggsincps                                    *SISTEMA SINONIMI
028600000726e   1c                   ENDIF
028700000000     c*
028800000000     c* invia messaggio
028900000000     c                   SETON                                        90
029000000000     c                   MOVEL     msg(10)       $msg
029100000000     c*
029200000000     c                   ENDSR
029300000000     c*--------------------------------------------------------------------------------------------*
029400000000     C* f10ges - gestione tasto funzionale f10 ==> Ricerca versioni
029500000000     c*--------------------------------------------------------------------------------------------*
029600000000     C     f10ges        BEGSR
029700000000     c*
029800000000     c* lancia il programma di gestione versioni in ricerca
029900000000     c                   CLEAR                   dssi00                         *pulisce ds
030000000000     c                   MOVEL     'R01'         d00op0                         *livello procedura
030100000000     c                   MOVEL     *blanks       d00op1                         *livello programma
030200000000     c                   MOVEL     '0'           d00f03                         *premuto F03
030300000000     c                   MOVEL     '0'           d00f12                         *premuto F12
030400000000     c                   MOVEL     '0'           d00err                         *errore
030500000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
030600000000     c                   CALL      'TISI46R'                                    *gestione versioni
030700000000     c                   PARM                    kpjba
030800000000     c                   PARM                    dssi00
030900000000if  1c                   IF        d00err='1'                                   *errore
031000000000     C                   SETON                                        2190
031100000000     C                   MOVEL     msg(2)        $msg
031200000000x   1c                   ELSE                                                   *no errore
031300000000     c                   Z-ADD     d00ver        v1cver
031400000000e   1c                   ENDIF
031500000000     c                   MOVEL     '1'           $tesvd
031600000000     C*
031700000000     C                   ENDSR
031800000000     c*--------------------------------------------------------------------------------------------*
031900000000     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
032000000000     c*--------------------------------------------------------------------------------------------*
032100000000     C     f12ges        BEGSR
032200000000     C*
032300000000     C                   MOVEL     'S'           $fine                          *fine programma
032400000000     C*
032500000000     C                   ENDSR
032600000000     C*--------------------------------------------------------------------------------------------*
032700000000     C* crtsincpl - creazione nuovi sinonimi da AZCPL
032800000000     C*--------------------------------------------------------------------------------------------*
032900000000     C     crtsincpl     BEGSR
033000000000     c*
033100000000     c* apre il file di stampa
033200000000     c                   OPEN      qsysprt
033300000000     c*
033400000000     c* imposta variabili di lavoro
033500000000     c                   MOVEL     '1'           *INOF                          *SI fine pagina
033600000000     C*
033700000000     C* legge le località AZCPL
033800000000     C     wver          SETLL     azcpl01l
033900000000     C     wver          READE     azcpl01l                               99
034000000000do  1C                   DOW       NOT *in99
034100000000if  2C                   IF        cplatb = *blanks AND                         *no annullati
034200000000     C                             cplfi1 = *blanks                             *no obsoleti
034300000000     C*
034400000000     C* elabora solo quelle di lunghezza maggiore di quella richiesta
034500000000if  3C                   IF        %LEN(%TRIM(cplloc)) > wlun
034600000000     C                   EVAL      wsin = %SUBST(%TRIM(cplloc):1:wlun)          *nuovo sinonimo
034700000000     C*
034800000000     C* scrive il nuovo sinonimo
034900000000     C                   MOVEL     cplnar        wnar
035000000000     C                   MOVEL     cplcap        wcap
035100000000     C                   MOVEL     cplloc        wloc
035200000000     c                   EXSR      scrsin
035300000000e   3C                   ENDIF
035400000000e   2C                   ENDIF
035500000000     C     wver          READE     azcpl01l                               99
035600000000e   1C                   ENDDO
035700000000     c*
035800000000     c* chiude il file di stampa
035900000000     c                   CLOSE     qsysprt
036000000000     C*
036100000000     C                   ENDSR
036200000000     C*--------------------------------------------------------------------------------------------*
036300000000     C* crtsinloc - creazione nuovi sinonimi da AZLOC
036400000000     C*--------------------------------------------------------------------------------------------*
036500000000     C     crtsinloc     BEGSR
036600000000     c*
036700000000     c* apre il file di stampa
036800000000     c                   OPEN      qsysprt
036900000000     c*
037000000000     c* imposta variabili di lavoro
037100000000     c                   MOVEL     '1'           *INOF                          *SI fine pagina
037200000000     C*
037300000000     C* legge le località AZLOC
037400000000     C                   READ      azloc00f                               99
037500000000do  1C                   DOW       NOT *in99
037600000000     C*
037700000000     C* controlla l'esistenza della località POSTE nel cappario
037800000000     c                   Z-ADD     wver          kplver
037900000000     c                   MOVEL     *blanks       kplnar
038000000000     c                   MOVEL     loccap        kplcap
038100000000     c                   MOVEL     locloc        kplloc
038200000000     c     keycpl        CHAIN     azcpl01l
038300000000     C*
038400000000     C* elabora solo quelle che hanno un trattino nella località alternativa
038500000000if  2c                   IF        %FOUND                                       *trovata
038600000000if  3C                   IF        locfi4 = ' - '
038700000000     c*
038800000000     c* controlla SE e DOVE c'è un "*" nella località alternativa
038900000000     c                   Z-ADD     1             asterisco
039000000000     c     '*'           SCAN      locage        asterisco                28
039100000000if  4c                   IF        NOT *in28                                    *non trovato
039200000000     c                   Z-ADD     36            asterisco
039300000000e   4c                   ENDIF
039400000000     C                   EVAL      wsinlav = %TRIM(locloc) +
039500000000     C                             locfi4 +
039600000000     C                             %TRIM(%SUBST(locage:1:(asterisco-1)))
039700000000     c                   MOVEL     wsinlav       wsin                           *nuovo sinonimo
039800000000     C*
039900000000     C* scrive il nuovo sinonimo
040000000000     C                   MOVEL     cplnar        wnar
040100000000     C                   MOVEL     cplcap        wcap
040200000000     C                   MOVEL     cplloc        wloc
040300000000     c                   EXSR      scrsin
040400000000e   3C                   ENDIF
040500000000e   2C                   ENDIF
040600000000     C                   READ      azloc00f                               99
040700000000e   1C                   ENDDO
040800000000     c*
040900000000     c* chiude il file di stampa
041000000000     c                   CLOSE     qsysprt
041100000000     C*
041200000000     C                   ENDSR
041300000000     C*--------------------------------------------------------------------------------------------*
041400000000     C* scrive il nuovo sinonimo
041500000000     C*--------------------------------------------------------------------------------------------*
041600000000     C     scrsin        BEGSR
041700000000     c*
041800000000     c* controlla esistenza nuovo sinonomo
041900000000     C                   Z-ADD     wver          kpsver
042000000000     C                   MOVEL     wnar          kpsnar
042100000000     C                   MOVEL     wcap          kpscap
042200000000     C                   MOVEL     wloc          kpsloc
042300000000     C                   MOVEL     wsin          kpssin
042400000731     C     keycps        CHAIN(N)  azcps01l
042500000000if  1C                   IF        NOT %FOUND                                   *INESISTENTE
042600000000     C                   Z-ADD     kpsver        cpsver                         *chiave
042700000000     C                   MOVEL     kpsnar        cpsnar
042800000000     C                   MOVEL     kpscap        cpscap
042900000000     C                   MOVEL     kpsloc        cpsloc
043000000000     C                   MOVEL     kpssin        cpssin
043100000000     C                   MOVEL     *blanks       cpsatb                         *campi
043200000000     C                   MOVEL     *blanks       cpsftr
043300000000     C                   Z-ADD     *zeros        cpsdtr
043400000000if  2c                   IF        v1csim = 'N'                                 *NO simulazione
043500000000     C                   WRITE     azcps000                                     *INSERIMENTO
043600000000e   2C                   ENDIF
043700000000     C                   MOVEL     msp(1)        stmmsg
043800000000x   2C                   ELSE                                                   *ESISTENTE
043900000000     C                   MOVEL     msp(2)        stmmsg
044000000000e   1C                   ENDIF
044100000000     c*
044200000000     c* stampa esito elaborazione
044300000000     C                   EXSR      stadet
044400000000     c*
044500000000     C                   ENDSR
044600000726     C*--------------------------------------------------------------------------------------------*
044700000726     C* aggsincps - sistema sinonimi esistenti
044800000726     C*--------------------------------------------------------------------------------------------*
044900000726     C     aggsincps     BEGSR
045000000726     c*
045100000726     c* apre il file di stampa
045200000726     c                   OPEN      qsysprt
045300000726     c*
045400000726     c* imposta variabili di lavoro
045500000726     c                   MOVEL     '1'           *INOF                          *SI fine pagina
045600000731     c*
045700000731     c* azzera variabili di lavoro
045800000731     c                   CLEAR                   sanar                          *nazione sin da ANN
045900000731     c                   CLEAR                   sacap                          *cap     sin da ANN
046000000731     c                   CLEAR                   sasin                          *sinonimo da ANN
046100000731     c                   CLEAR                   s                              *indice sinonimi
046200000726     C*
046300000726     C* legge i sinonimi esistenti
046400000726     C     wver          SETLL     azcps04l
046500000726     C     wver          READE     azcps04l                               99
046600000726do  1C                   DOW       NOT *in99
046700000726     c*
046800000726     c* operazioni per nuovo nazione/cap/sinonimo
046900000726     c                   EXSR      newsin
047000000726     C*
047100000726     C* ciclo fino a rottura nazione/cap/sinonimo
047200000726do  2C                   DOW       NOT *in99 AND
047300000726     c                             depnar = cpsnar AND
047400000726     c                             depcap = cpscap  AND
047500000726     c                             depsin = cpssin
047600000726     c*
047700000726     c* intanto annulla alcuni sinonimi sui "santi"
047800000726if  3C                   IF        cpssin = 'S'  OR
047900000726     c                             cpssin = 'S.' OR
048000000726     c                             cpssin = 'SS'   OR
048100000726     c                             cpssin = 'S.S.'
048200000726     C                   MOVEL     msp(3)        stmmsg
048300000726     c                   EXSR      updsin                                       *ANNULLAMENTO
048400000726e   3C                   ENDIF
048500000726     c*
048600000726     c* incrementa contatore sinonimi uguali per stesso cap
048700000726     c                   ADD       1             ContSinU
048800000726     C     wver          READE     azcps04l                               99
048900000726e   2c                   ENDDO                                                  *rottura sinonimo
049000000726     c*
049100000726     c* se esistono più sinonimi per lo stesso cap, lo memorizza in una schiera
049200000726if  2c                   IF        ContSinU > 1
049300000726     c                   ADD       1             s
049400000726     c                   MOVEL     depnar        sanar(s)
049500000726     c                   MOVEL     depcap        sacap(s)
049600000726     c                   MOVEL     depsin        sasin(s)
049700000726e   2c                   ENDIF
049800000726e   1C                   ENDDO                                                  *fine file
049900000726     c*
050000000726     c* annulla in AZCPS i sinonimi memorizzati in schiera
050100000726do  1c                   DO        s             i
050200000726     c                   Z-ADD     wver          kpsver
050300000726     c                   MOVEL     sanar(i)      kpsnar
050400000726     c                   MOVEL     sacap(i)      kpscap
050500000726     c                   MOVEL     sasin(i)      kpssin
050600000726     c     ke2cps        SETLL     azcps04l
050700000726     C     ke2cps        READE     azcps04l                               99
050800000726do  2C                   DOW       NOT *in99
050900000809if  3c                   IF        sasin(i) <> 'LANA'                           *CASO PIETOSO
051000000726     C                   MOVEL     msp(4)        stmmsg
051100000726     c                   EXSR      updsin                                       *ANNULLAMENTO
051200000809e   3c                   ENDIF
051300000726     C     ke2cps        READE     azcps04l                               99
051400000726e   2c                   ENDDO                                                  *fine sinonimi =
051500000726e   1c                   ENDDO                                                  *fine schiera
051600000726     c*
051700000726     c* chiude il file di stampa
051800000726     c                   CLOSE     qsysprt
051900000726     C*
052000000726     C                   ENDSR
052100000726     C*--------------------------------------------------------------------------------------------*
052200000726     C* newsin - operazioni per nuovo sinonimo
052300000726     C*--------------------------------------------------------------------------------------------*
052400000726     c     newsin        BEGSR
052500000726     c*
052600000726     c* memorizza il nuovo sinonimo in un deposito
052700000726     c                   MOVEL     cpsnar        depnar
052800000726     c                   MOVEL     cpscap        depcap
052900000726     c                   MOVEL     cpssin        depsin
053000000726     c*
053100000726     c* azzera contatore sinonimi uguali per stesso cap
053200000726     c                   Z-ADD     *zeros        ContSinU
053300000726     c*
053400000726     c                   ENDSR
053500000726     C*--------------------------------------------------------------------------------------------*
053600000726     C* updsin - sistema sinonimi esistenti
053700000726     C*--------------------------------------------------------------------------------------------*
053800000726     c     updsin        BEGSR
053900000726     c*
054000000726     c* annulla il sinonimo
054100000726     c                   MOVEL     'A'           cpsatb
054200000726     c                   MOVEL     *blanks       cpsftr
054300000726if  1c                   IF        v1csim = 'N'                                 *NO simulazione
054400000726     c                   UPDATE    azcps4                                       *MODIFICA
054500000726e   1c                   ENDIF
054600000726     c*
054700000726     c* stampa esito elaborazione
054800000726     c                   Z-ADD     cpsver        kpsver
054900000726     c                   MOVEL     cpsnar        kpsnar
055000000726     c                   MOVEL     cpscap        kpscap
055100000726     c                   MOVEL     cpsloc        kpsloc
055200000726     c                   MOVEL     cpssin        kpssin
055300000726     C                   EXSR      stadet
055400000726     c*
055500000726     C                   ENDSR
055600000000     c*--------------------------------------------------------------------------------------------*
055700000000     c* stadet - stampa esito elaborazione
055800000000     c*--------------------------------------------------------------------------------------------*
055900000000     c     stadet        BEGSR
056000000000     c*
056100000000     c* stampa testata
056200000000if  1c                   IF        *inof                                        *fine pagina
056300000000     c                   EXCEPT    tes
056400000000     c                   MOVEL     '0'           *inof
056500000000e   1c                   ENDIF
056600000000     c*
056700000000     c* stampa dettaglio
056800000000     c                   Z-ADD     kpsver        stmver
056900000000     c                   MOVEL     kpsnar        stmnar
057000000000     c                   MOVEL     kpscap        stmcap
057100000000     c                   MOVEL     kpsloc        stmloc
057200000000     c                   MOVEL     kpssin        stmsin
057300000000     c                   EXCEPT    det
057400000000     c*
057500000000     c                   ENDSR
057600000000     c*--------------------------------------------------------------------------------------------*
057700000000     C* impvd0 - Impostazioni testata
057800000000     c*--------------------------------------------------------------------------------------------*
057900000000     C     impvd0        BEGSR
058000000000     C*
058100000000     C                   MOVEL     '1'           $tesvd                         *riemettere testata
058200000000     C*
058300000000     C                   ENDSR
058400000000     c*--------------------------------------------------------------------------------------------*
058500000000     C* finsr - operazioni finali
058600000000     c*--------------------------------------------------------------------------------------------*
058700000000     C     finsr         BEGSR
058800000000     C*
058900000000     C                   ENDSR
059000000000     c*--------------------------------------------------------------------------------------------*
059100000000     C* *inzsr - operazioni iniziali
059200000000     c*--------------------------------------------------------------------------------------------*
059300000000     C     *inzsr        BEGSR
059400000000     c*
059500000000     c* ricevimento parametri
059600000000     c     *ENTRY        PLIST
059700000000     c                   PARM                    kpjba
059800000000     C*
059900000000     C* chiavi di lettura
060000000000     C     keycps        klist                                                  *azcps00f
060100000000     C                   KFLD                    kpsver                          -versione
060200000000     C                   KFLD                    kpsnar                          -nazione
060300000000     C                   KFLD                    kpscap                          -cap
060400000000     C                   KFLD                    kpsloc                          -località
060500000000     C                   KFLD                    kpssin                          -sinonimo
060600000726     C     ke2cps        klist                                                  *azcps00f
060700000726     C                   KFLD                    kpsver                          -versione
060800000726     C                   KFLD                    kpsnar                          -nazione
060900000726     C                   KFLD                    kpscap                          -cap
061000000726     C                   KFLD                    kpssin                          -sinonimo
061100000000     C     keycpl        klist                                                  *azcpl00f
061200000000     C                   KFLD                    kplver                          -versione
061300000000     C                   KFLD                    kplnar                          -nazione
061400000000     C                   KFLD                    kplcap                          -cap
061500000000     C                   KFLD                    kplloc                          -località
061600000000     C*
061700000000     C* reperisce la data corrente
061800000000     C                   TIME                    n14                            *ora (6)+ data (8)
061900000000     C                   MOVE      n14           n8                             *data (8) in g/m/aa
062000000000     C                   Z-ADD     n8            datcor                         *Data corrente
062100000000     c*
062200000000     c                   ENDSR
062300000000     o*--------------------------------------------------------------------------------------------*
062400000000     o* Stampa
062500000000     o*--------------------------------------------------------------------------------------------*
062600000000     Oqsysprt   E            tes              02
062700000000     O                       sta(1)              66
062800000000     O                       sta(2)             132
062900000000     O                       datcor        8    117
063000000000     O                       PAGE          Z    132
063100000000     O*------------------------
063200000000     O          E            tes              03
063300000000     O                       sta(3)              66
063400000000     O                       sta(3)             132
063500000000     O*------------------------
063600000000     O          E            tes              04
063700000000     O                       sta(4)              66
063800000000     O                       sta(6)             132
063900000000     O*------------------------
064000000000     O          E            tes              05
064100000000     O                       sta(3)              66
064200000000     O                       sta(3)             132
064300000000     O*------------------------
064400000000     O          E            det         1
064500000000     O                       stmver        Z      5
064600000000     O                       stmnar               9
064700000000     O                       stmcap              19
064800000000     O                       stmloc              55
064900000000     O                       stmsin              91
065000000000     O                       stmmsg             132
065100000000     O*--------------------------------------------------------------------------------------------*
065200000000** MSG -Messaggi video-
065300000000TISI87R- Immettere uno dei valori possibili                                   1
065400000000TISI87R- Versione inesistente                                                 2
065500000000TISI87R- Lunghezza errata: deve essere > 0 e minore di 35                     3
065600000000TISI87R- 4
065700000000TISI87R- 5
065800000000TISI87R- 6
065900000000TISI87R- 7
066000000000TISI87R- 8
066100000000TISI87R- 9
066200000000TISI87R- Elaborazione eseguita ...                                            10
066300000000** STA -Righe di stampa -
066400000731TISI87R                          ***  Crea sinonimi da AZCPL/AZLOC1
066500000731 e li sistema  ***                       99/99/99         PAG.99992
066600000000------------------------------------------------------------------3
066700000000VER   NAZ CAP       LOCALITA'                           SINONIMO  4
06680000000099999 XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX5
066900000000                          ESITO                                   6
067000000000XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7
0671000000008
0672000000009
06730000000010
067400000000** MSP -Messaggi stampa-
067500000000SINONIMO INSERITO                                                 1
067600000000SINONIMO NON INSERITO: ESISTEVA GIA'                              2
067700000726SINONIMO ANNULLATO: ERA UN SANTO VAGO                             3
067800000726SINONIMO ANNULLATO: SINONIMI = PER CAP =                          4
0679000000005
