000100970627     h*--------------------------------------------------------------------------------------------*
000200970627     h* Controlla località bolle arrivo - Richiesta parametri
000300970627     h*--------------------------------------------------------------------------------------------*
000400000622     h DECEDIT('0,') DATEDIT(*DMY.)
000500970627     f*--------------------------------------------------------------------------------------------*
000600970627     f* DATA BASE
000700970627     f*--------------------------------------------------------------------------------------------*
000800970718     ftabel00f  IF   E           K DISK
000900970627     ftisi90d   CF   E             WORKSTN
001000970627     d*--------------------------------------------------------------------------------------------*
001100970627     d* Data structure
001200970627     d*--------------------------------------------------------------------------------------------*
001300970627     d*----------------
001400970627     d* schiere
001500970627     d*----------------
001600970627     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001700970627     D*----------------
001800970627     D* Variabili riferite al data base
001900970627     D*----------------
002000970718     D ktbkut          S                   LIKE(tblkut)                         *per tabel00f
002100970718     D ktbcod          S                   LIKE(tblcod)
002200000621     D ktbkey          S                   LIKE(tblkey)
002300970627     d*----------------
002400970627     d* Variabili di controllo
002500970627     d*----------------
002600970627     d $fine           S              1    INZ('N')                             *Gestione
002700970627     d $tesvd          S              1    INZ('1')                             *riemettere testata
002800970627     d $tipvid         S              1    INZ('1')                             *gestione video
002900970627     d*----------------
003000970627     d* Variabili a programma
003100970627     d*----------------
003200970718     d wn14            S             14  0                                      *Numerico di 14
003300970718     d wn8             S              8  0                                      *Numerico di 8
003400970718     d werr            S              1    INZ('0')                             *esistenza versioni
003500970627     d wda8            S              8  0                                      *data di 8 in g/m/a
003600970627     d da8w            S              8  0                                      *data di 8 in a/m/g
003700970718     d daiv1c          S              8  0                                      *data iniziale
003800970718     d dafv1c          S              8  0                                      *data finale
003900000830     d driv1c          S              8  0                                      *data riferimento
004000970718     d p94pgm          S             10                                         *parametri TRUL94C
004100000103     d p94dat          S              8
004200970718     d p94err          S              1
004300970718     d datcor          S              8  0                                      *data corrente a/m/g
004400970718     d cordat          S              8  0                                      *data corrente g/m/a
004500970718     d §programma      S             10                                         *programma
004600970718     d §giorni         S              3  0                                      *giorni
004700970718     d §datini         S              8  0                                      *data inizio aa/m/g
004800970718     d pdatini         S              8  0                                      *partenza
004900970718     d adatini         S              8  0                                      *arrivo
005000970718     d §datfin         S              8  0                                      *data fine   aa/m/g
005100970718     d pdatfin         S              8  0                                      *partenza
005200970718     d adatfin         S              8  0                                      *arrivo
005300970718     d §inidat         S              8  0                                      *data inizio g/m/aa
005400970718     d pinidat         S              8  0                                      *partenza
005500970718     d ainidat         S              8  0                                      *arrivo
005600970718     d §findat         S              8  0                                      *data fine   g/m/aa
005700970718     d pfindat         S              8  0                                      *partenza
005800970718     d afindat         S              8  0                                      *arrivo
005900970627     d*----------------
006000970627     d* Ds
006100970627     d*----------------
006200021001     d TIBS34DS      E DS                                                       *Profili utente
006300021001     d DDATIUTE      E DS                                                       *Dati utente
006400021001     d AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006500970718     d ds5a          E DS                                                       *giorni di pulizia
006600970627     d wlbda8          DS                  INZ                                  *controlla data
006700970718     d  g08dat                        8  0                                       -data
006800970718     d  g08inv                        8  0                                       -data invertita
006900970718     d  g08err                        1                                          -errore
007000970718     d  g08tgi                        5  0                                       -n°giorni
007100970718     d wgidat          DS                                                       *crea data da giorni
007200970718     d  giodat                        8  0                                       -data
007300970718     d  gioinv                        8  0                                       -data invertita
007400970718     d  giotgi                        5  0                                       -n°giorni
007500970627     d dspar90         DS                                                       *ds parametri lancio
007600970711     d  p90bpa                        1                                          -tipo bolla
007700970711     d  p90dai                        8  0                                       -data iniziale
007800970627     d  p90daf                        8  0                                       -data finale
007900970711     d  p90ld1                        1                                          -livello dati 1
008000970711     d  p90ld2                        1                                          -livello dati 2
008100970711     d  p90ld3                        1                                          -livello dati 3
008200970711     d  p90ld4                        1                                          -livello dati 4
008300970711     d  p90ld5                        1                                          -livello dati 5
008400970711     d  p90la1                        1                                          -livello aff. 1
008500970711     d  p90la2                        1                                          -livello aff. 2
008600970711     d  p90la3                        1                                          -livello aff. 3
008700970711     d  p90la4                        1                                          -livello aff. 4
008800970711     d  p90la5                        1                                          -livello aff. 5
008900000608     d  p90sed                        1                                          -lancio da SEDE
009000000830     d  p90dri                        8  0                                       -data riferiemento
009100970627     d kpjba         E DS                                                       *architettura
009200970627     c*--------------------------------------------------------------------------------------------*
009300970627     c* Main lines
009400970627     c*--------------------------------------------------------------------------------------------*
009500970627     C*
009600970627     C* impostazioni testata
009700970627     C                   EXSR      impvd0
009800970627     C*
009900970627     C* impostazioni prima videata
010000970627     C                   EXSR      impvd1
010100970627     C*
010200970627     C* ciclo fino a richiesta di fine elaborazione
010300970627do  1C                   DOU       $fine='S'
010400970627     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
010500970627     C                   ENDCS
010600970627e   1C                   ENDDO
010700970627     C*
010800970627     C                   SETON                                        lr
010900970627     c*--------------------------------------------------------------------------------------------*
011000970627     c* impvd1 - impostazioni prima videata
011100970627     c*--------------------------------------------------------------------------------------------*
011200970627     C     impvd1        BEGSR
011300970627     C*
011400970627     C* reimposta formato video e indicatori
011500970627     C                   EXSR      setvd1
011600970627     C                   MOVEL     '1'           $tipvid                        *gestione video
011700000608     c  N62              MOVEL     'P'           v1cbpa                         *campi videata
011800970714     c                   MOVEL     '1'           v1cld1
011900970714     c                   MOVEL     '2'           v1cld2
012000970714     c                   MOVEL     '3'           v1cld3
012100970714     c                   MOVEL     '4'           v1cld4
012200970714     c                   MOVEL     '0'           v1cla1
012300970714     c                   MOVEL     '1'           v1cla2
012400970714     c                   MOVEL     '2'           v1cla3
012500970714     c                   MOVEL     '3'           v1cla4
012600000830     c                   Z-ADD     *zeros        v1cdri
012700970627     C*
012800970627     C                   ENDSR
012900970627     c*--------------------------------------------------------------------------------------------*
013000970627     c* setvd1 - impostazioni indicatori prima videata
013100970627     c*--------------------------------------------------------------------------------------------*
013200970627     C     setvd1        BEGSR
013300970627     C*
013400970711     C                   SETOFF                                       596061    *errori
013500000830     C                   SETOFF                                       63
013600000830     C                   SETOFF                                       90
013700970711     C                   MOVEL     *blanks       $msg                           *messaggio
013800970627     C*
013900970627     C                   ENDSR
014000970627     c*--------------------------------------------------------------------------------------------*
014100970627     C* gesvd1 - gestione prima videata
014200970627     c*--------------------------------------------------------------------------------------------*
014300970627     C     gesvd1        BEGSR
014400970627     C*
014500970627     C* emissione videata
014600970627if  1C                   IF        $tesvd='1'
014700970627     C                   WRITE     si90d00                                      *testata
014800970627     C                   MOVEL     '0'           $tesvd
014900970627e   1C                   ENDIF
015000970627     C                   EXFMT     si90d01                                      *video
015100970627     C                   EXSR      setvd1
015200970627     C* F03-Fine
015300970627if  1C                   IF        *inkc
015400970627     C                   EXSR      f03ges
015500970627     C                   GOTO      finvd1
015600970627e   1C                   ENDIF
015700970627     C* F12-Ritorno
015800970627if  1C                   IF        *inkl
015900970627     C                   EXSR      f12ges
016000970627     C                   GOTO      finvd1
016100970627e   1C                   ENDIF
016200970627     C* Enter-Controlla
016300970627     C                   EXSR      ctrvd1
016400970627if  1C                   IF        *in90                                        *errori
016500970627     C                   GOTO      finvd1
016600970627e   1C                   ENDIF
016700970627     C* F06-Conferma
016800970627if  1C                   IF        *inkf
016900000608     C                   EXSR      ctrvd1
017000970627     C                   EXSR      f06ges
017100970627     C                   GOTO      finvd1
017200970627e   1C                   ENDIF
017300970627     C*
017400970627     C     finvd1        ENDSR
017500970627     c*--------------------------------------------------------------------------------------------*
017600970627     C* ctrvd1 - controlla prima videata
017700970627     c*--------------------------------------------------------------------------------------------*
017800970627     C     ctrvd1        BEGSR
017900970627     C*
018000970627     C* reimposta le variabili di lavoro
018100970627     C                   MOVEL     '0'           werr
018200970711     C*---
018300970711     C* tipo bolla
018400970711     C*---
018500000608     C                   IF        *in62=*off
018600970711if  1C                   IF        v1cbpa <> 'P' AND
018700970711     C                             v1cbpa <> 'A'
018800970711     c                   MOVEL     '1'           werr
018900970711     C                   MOVEL     msg(1)        $msg
019000970711     C                   SETON                                        59
019100970711     C                   GOTO      finct1
019200970711e   1C                   ENDIF
019300000608     C                   ENDIF
019400970718     C*---
019500970718     C* Imposta le date se vuote
019600970718     C*---
019700970718if  1c                   IF        v1cdai=*zeros AND v1cdaf=*zeros
019800970718if  2C                   IF        v1cbpa='P'
019900000621     c                   IF        pinidat>*zeros AND pfindat>*zeros
020000970718     c                   Z-ADD     pinidat       v1cdai
020100970718     c                   Z-ADD     pfindat       v1cdaf
020200000621     C                   ENDIF
020300970718x   2c                   ELSE
020400000621     c                   IF        ainidat>*zeros AND afindat>*zeros
020500970718     c                   Z-ADD     ainidat       v1cdai
020600970718     c                   Z-ADD     afindat       v1cdaf
020700000621     C                   ENDIF
020800970718e   2C                   ENDIF
020900970718e   1C                   ENDIF
021000970711     C*---
021100970627     C* data iniziale
021200970627     C*---
021300970627     C                   Z-ADD     *zeros        daiv1c
021400970627     C                   Z-ADD     v1cdai        wda8
021500970627     C                   Z-ADD     *zeros        da8w
021600970627     C                   MOVEL     '0'           werr
021700970627     C                   EXSR      chkda8
021800970627     C                   Z-ADD     wda8          v1cdai
021900970627     C                   Z-ADD     da8w          daiv1c
022000970627if  1C                   IF        werr='1'
022100970627     C                   SETON                                        6090
022200970711     C                   MOVEL     msg(2)        $msg
022300970627     C                   GOTO      finct1
022400970627e   1C                   ENDIF
022500970718     c*
022600970718if  1C                   IF        v1cbpa='P' AND
022700970718     c                             daiv1c < pdatini OR
022800970718     C                             v1cbpa='A' AND
022900970718     c                             daiv1c < adatini
023000970718     C                   SETON                                        6090
023100970718     c                   MOVEL     '1'           werr
023200970718     C                   MOVEL     msg(4)        $msg
023300970718     C                   GOTO      finct1
023400970718e   1C                   ENDIF
023500970627     C*---
023600970627     C* data finale
023700970627     C*---
023800970627     C                   Z-ADD     *zeros        dafv1c
023900970627     C                   Z-ADD     v1cdaf        wda8
024000970627     C                   Z-ADD     *zeros        da8w
024100970627     C                   MOVEL     '0'           werr
024200970627     C                   EXSR      chkda8
024300970627     C                   Z-ADD     wda8          v1cdaf
024400970627     C                   Z-ADD     da8w          dafv1c
024500970627if  1C                   IF        werr='1'
024600970627     C                   SETON                                        6190
024700970711     C                   MOVEL     msg(2)        $msg
024800970627     C                   GOTO      finct1
024900970627e   1C                   ENDIF
025000970627     c*
025100970718if  1C                   IF        v1cbpa='P' AND
025200970718     c                             dafv1c > pdatfin OR
025300970718     C                             v1cbpa='A' AND
025400970718     c                             dafv1c > adatfin
025500970718     C                   SETON                                        6190
025600970718     c                   MOVEL     '1'           werr
025700970718     C                   MOVEL     msg(5)        $msg
025800970718     C                   GOTO      finct1
025900970718e   1C                   ENDIF
026000970627     C*---
026100970627     C* data iniziale + data finale
026200970627     C*---
026300970627if  1c                   IF        dafv1c < daiv1c
026400970627     C                   SETON                                        606190
026500970718     C                   MOVEL     msg(3)        $msg
026600970627     c                   MOVEL     '1'           werr
026700970627     C                   GOTO      finct1
026800970627e   1C                   ENDIF
026900000608     C*---
027000000608     C* in ogni caso le date vanno indicate
027100000608     C*---
027200000608     C                   IF        v1cdai = *zeros OR
027300000608     C                             v1cdaf = *zeros
027400000608     C                   SETON                                        606190
027500000608     C                   MOVEL     msg(6)        $msg
027600000608     c                   MOVEL     '1'           werr
027700000608     C                   ENDIF
027800000830     C*---
027900000830     C* data riferimento
028000000830     C*---
028100000830     C                   Z-ADD     *zeros        driv1c
028200000830if  1c                   IF        v1cdri > *zeros
028300000830     C                   Z-ADD     v1cdri        wda8
028400000830     C                   Z-ADD     *zeros        da8w
028500000830     C                   MOVEL     '0'           werr
028600000830     C                   EXSR      chkda8
028700000830     C                   Z-ADD     wda8          v1cdri
028800000830     C                   Z-ADD     da8w          driv1c
028900000830if  2C                   IF        werr='1'
029000000830     C                   SETON                                        6390
029100000830     C                   MOVEL     msg(2)        $msg
029200000830     C                   GOTO      finct1
029300000830e   2C                   ENDIF
029400000830e   1C                   ENDIF
029500000608     C
029600970627     C*
029700970627     C     finct1        ENDSR
029800970627     c*--------------------------------------------------------------------------------------------*
029900970627     c* chkda8 - controlla la data (8)
030000970627     c*--------------------------------------------------------------------------------------------*
030100970627     C     chkda8        BEGSR
030200970627     C*
030300970627     C                   Z-ADD     wda8          g08dat
030400970627     C                   Z-ADD     *zeros        g08inv
030500970627     C                   MOVEL     '0'           g08err
030600970627     C                   CALL      'XSRDA8'
030700970627     C                   PARM                    wlbda8
030800970627     C                   Z-ADD     g08dat        wda8
030900970627IF  1C                   IF        g08err='1'
031000970627     C                   MOVEL     '1'           werr
031100970627X   1C                   ELSE
031200970627     C                   Z-ADD     g08inv        da8w
031300970627E   1C                   ENDIF
031400970627     C*
031500970627     C                   ENDSR
031600970627     c*--------------------------------------------------------------------------------------------*
031700970627     C* f03ges - gestione tasto funzionale f03 ==> Fine
031800970627     c*--------------------------------------------------------------------------------------------*
031900970627     C     f03ges        BEGSR
032000970627     C*
032100970627     C                   MOVEL     'S'           $fine                          *fine programma
032200970627     C*
032300970627     C                   ENDSR
032400970627     c*--------------------------------------------------------------------------------------------*
032500970627     C* f06ges - gestione tasto funzionale f06 ==> Conferma
032600970627     c*--------------------------------------------------------------------------------------------*
032700970627     C     f06ges        BEGSR
032800970627     c*
032900970627     c* imposta la ds di procedura
033000970711     c                   MOVEL     v1cbpa        p90bpa                         *tipo bolla
033100970711     c                   Z-ADD     daiv1c        p90dai                         *data iniziale
033200970627     c                   Z-ADD     dafv1c        p90daf                         *data finale
033300970711     c                   MOVEL     v1cld1        p90ld1                         *livello dati 1
033400970711     c                   MOVEL     v1cld2        p90ld2                         *livello dati 2
033500970711     c                   MOVEL     v1cld3        p90ld3                         *livello dati 3
033600970711     c                   MOVEL     v1cld4        p90ld4                         *livello dati 4
033700970711     c                   MOVEL     v1cld5        p90ld5                         *livello dati 5
033800970711     c                   MOVEL     v1cla1        p90la1                         *livello aff. 1
033900970711     c                   MOVEL     v1cla2        p90la2                         *livello aff. 2
034000970711     c                   MOVEL     v1cla3        p90la3                         *livello aff. 3
034100970711     c                   MOVEL     v1cla4        p90la4                         *livello aff. 4
034200970711     c                   MOVEL     v1cla5        p90la5                         *livello aff. 5
034300000608     c                   MOVEL     v1csed        p90sed                         *lancio da SEDE
034400000830     c                   Z-ADD     driv1c        p90dri                         *data riferimento
034500970627     c                   MOVEL     dspar90       kpjbu
034600970702     c                   MOVEL     'SI91'        kcoaz
034700020227     c                   CALL      'BCH10'
034800970702     c                   PARM                    kpjba
034900970627     C                   MOVEL     '1'           $tesvd                          *riemettere testata
035000970627     c*
035100970627     C                   ENDSR
035200970627     c*--------------------------------------------------------------------------------------------*
035300970627     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
035400970627     c*--------------------------------------------------------------------------------------------*
035500970627     C     f12ges        BEGSR
035600970627     C*
035700970627     C                   MOVEL     'S'           $fine                          *fine programma
035800970627     C*
035900970627     C                   ENDSR
036000970627     c*--------------------------------------------------------------------------------------------*
036100970627     C* impvd0 - Impostazioni testata
036200970627     c*--------------------------------------------------------------------------------------------*
036300970627     C     impvd0        BEGSR
036400970627     C*
036500970627     C                   MOVEL     '1'           $tesvd                         *riemettere testata
036600970627     C*
036700970627     C                   ENDSR
036800970718     c*--------------------------------------------------------------------------------------------*
036900970718     C* cartab - carica le tabelle occorrenti
037000970718     c*--------------------------------------------------------------------------------------------*
037100970718     C     cartab        BEGSR
037200970718     c*
037300970718     C                   clear                   ds5a
037400021001     C                   Z-ADD     1             ktbkut
037500970718     C                   MOVEL     '5A'          ktbcod
037600000621     C                   MOVEL(p)  '1'           ktbkey
037700970718     C     ke2tab        CHAIN     tabel00f                           99
037800970718if  1C                   IF        NOT *in99 AND
037900970718     C                             tblflg=*blanks
038000970718     C                   MOVEL     tbluni        ds5a
038100970718e   1C                   ENDIF
038200970718     C*
038300970718     C                   ENDSR
038400970718     c*--------------------------------------------------------------------------------------------*
038500970718     C* impdat - imposta le date di lavoro
038600970718     c*--------------------------------------------------------------------------------------------*
038700970718     C     impdat        BEGSR
038800970718     C*
038900970718     C* data corrente
039000970718     C                   TIME                    wn14                           *ora(6) + data(8)
039100970718     C                   MOVE      wn14          wn8                            *data(8)
039200970718     C                   Z-ADD     wn8           cordat                         *data corrente g/m/a
039300970718     C                   Z-ADD     wn8           g08dat
039400970718     C                   Z-ADD     *zeros        g08inv
039500970718     C                   MOVEL     *blanks       g08err
039600970718     C                   CALL      'XSRDA8'
039700970718     C                   PARM                    wlbda8
039800970718     C                   Z-ADD     g08inv        datcor                         *data corrente a/m/g
039900970718     c*
040000970718     c* reperisce la data iniziale e finale minima/massima per elaborare le bolle partenza
040100970718     C                   MOVEL     'FNLS61R'     §programma
040200970718     C                   Z-ADD     §5ablp        §giorni
040300970718     c                   EXSR      repdat
040400970718     c                   Z-ADD     §datini       pdatini                        *date in aa/m/g
040500970718     c                   Z-ADD     §datfin       pdatfin
040600970718     c                   Z-ADD     §inidat       pinidat                        *date in g/m/aa
040700970718     c                   Z-ADD     §findat       pfindat
040800970718     c*
040900970718     c* reperisce la data iniziale e finale minima/massima per elaborare le bolle partenza
041000970718     C                   MOVEL     'FNLR84R'     §programma
041100970718     C                   Z-ADD     §5aarb        §giorni
041200970718     c                   EXSR      repdat
041300970718     c                   Z-ADD     §datini       adatini                        *date in aa/m/g
041400970718     c                   Z-ADD     §datfin       adatfin
041500970718     c                   Z-ADD     §inidat       ainidat                        *date in g/m/aa
041600970718     c                   Z-ADD     §findat       afindat
041700970718     c*
041800970718     C                   ENDSR
041900970718     c*--------------------------------------------------------------------------------------------*
042000970718     C* repdat - reperisce le date
042100970718     c*--------------------------------------------------------------------------------------------*
042200970718     C     repdat        BEGSR
042300970718     C*
042400970718     C* azzera le variabili di lavoro
042500970718     C                   Z-ADD     *zeros        §datini                        *date in aa/m/g
042600970718     C                   Z-ADD     *zeros        §datfin
042700970718     C                   Z-ADD     *zeros        §inidat                        *date in g/m/aa
042800970718     C                   Z-ADD     *zeros        §findat
042900970718     C*
043000970718     C* data inizio = data ultimo utilizzo pgm pulizia - gg mantenimento bolle
043100970718     C                   MOVEL     §programma    p94pgm
043200970718     C                   MOVEL     *blanks       p94dat
043300970718     C                   MOVEL     *blanks       p94err
043400970718     C                   CALL      'TRUL49C'                            99
043500970718     C                   PARM                    p94pgm
043600970718     C                   PARM                    p94dat
043700970718     C                   PARM                    p94err
043800970718if  1C                   IF        p94err='1' OR *in99                          *errore
043900970718     C                   Z-ADD     datcor        g08inv                           -data corrente
044000970718x   1C                   else                                                    *no errore
044100000608if  2C                   IF        %len(%trim(p94dat))<8                              -pgm mai usato
044200970718     C                   Z-ADD     *zeros        g08inv                            -data a zero
044300970718x   2C                   ELSE                                                     -pgm usato
044400000103     C                   MOVEL     p94dat        g08inv                            -ultimo utilizzo
044500970718e   2C                   ENDIF
044600970718e   1C                   ENDIF
044700970718if  1C                   IF        g08inv <> *zeros
044800970718     C                   Z-ADD     *zeros        g08dat
044900970718     C                   Z-ADD     *zeros        g08tgi
045000970718     C                   MOVEL     '3'           g08err
045100970718     C                   CALL      'XSRDA8'
045200970718     C                   PARM                    wlbda8                         *CALCOLA GG DATA
045300970718     C                   SUB       §giorni       g08tgi                         *MENO GG MANTENIMENT
045400970718     C                   Z-ADD     *zeros        giodat
045500970718     C                   Z-ADD     *zeros        gioinv
045600970718     C                   Z-ADD     g08tgi        giotgi
045700970718     C                   CALL      'XSRGI8'
045800970718     C                   PARM                    wgidat
045900970718     C                   Z-ADD     gioinv        §datini                        *data iniziale
046000970718e   1C                   ENDIF
046100970718     C*
046200970718     C* data finale = data corrente
046300970718     C                   Z-ADD     datcor        §datfin                        *data finale
046400970718     C*
046500970718     C* Inverte le date in gg/mm/anno
046600970718     C                   Z-ADD     §datini       g08inv
046700970718     C                   Z-ADD     *zeros        g08dat
046800970718     C                   MOVEL     '3'           g08err
046900970718     C                   CALL      'XSRDA8'
047000970718     C                   PARM                    wlbda8
047100970718     C                   Z-ADD     g08dat        §inidat                        *data iniziale G/M/A
047200970718     C                   Z-ADD     §datfin       g08inv
047300970718     C                   Z-ADD     *zeros        g08dat
047400970718     C                   MOVEL     '3'           g08err
047500970718     C                   CALL      'XSRDA8'
047600970718     C                   PARM                    wlbda8
047700970718     C                   Z-ADD     g08dat        §findat                        *data finale G/M/AA
047800970718     C*
047900970718     C                   ENDSR
048000021001     C*--------------------------------------------------------------------------------------------*
048100021001     C* REPERISCE I DATI UTENTE
048200021001     C*--------------------------------------------------------------------------------------------*
048300021001     C     REPDATIUTE    BEGSR
048400021001     C*
048500021001     C* INIZIALIZZA VARIABILI DI WRK
048600021001     C                   CLEAR                   TIBS34DS
048700021001     C                   CLEAR                   AZUTEDS
048800021001     C                   CLEAR                   DDATIUTE
048900021001     C*
049000021001     C     *DTAARA       DEFINE    §azute        azuteds
049100021001     C     *DTAARA       DEFINE    §datiute      ddatiute
049200021001     C                   IN(E)     *DTAARA
049300021001if  1C                   IF        %Error
049400021001     c                   EVAL      I34Tla = 'L'
049500021001     C                   CALL      'TIBS34R'
049600021001     C                   PARM                    Tibs34Ds
049700021001     C                   IN        *DTAARA
049800021001e   1C                   ENDIF
049900021001     C*
050000021001     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
050100021001     C                   MOVEL(P)  RSUT          DSFIRS           20
050200021001     C*
050300021001     C                   ENDSR
050400970627     c*--------------------------------------------------------------------------------------------*
050500970627     C* *inzsr - operazioni iniziali
050600970627     c*--------------------------------------------------------------------------------------------*
050700970627     C     *inzsr        BEGSR
050800970627     c*
050900970627     c* ricevimento parametri
051000970627     c     *ENTRY        PLIST
051100970627     c                   PARM                    kpjba
051200021001     C*
051300021001     C* REPERISCE I DATI UTENTE
051400021001     C                   EXSR      REPDATIUTE
051500970718     C*
051600970718     C* chiavi di lettura
051700970718     c     ke2tab        KLIST                                                  *di tabel00f
051800970718     c                   KFLD                    ktbkut                          -utente
051900970718     c                   KFLD                    ktbcod                          -tabella
052000000621     c                   KFLD                    ktbkey                          -codice
052100000608     C*
052200000608     C* Prime considerazioni
052300021007     C                   IF        simfel = *zeros
052400000608     C                   MOVEL     'S'           V1CSED
052500000608     C                   SETON                                        62
052600000608     C                   ENDIF
052700970718     C*
052800970718     C* carica le tabelle occorrenti
052900970718     C                   EXSR      cartab
053000970718     C*
053100970718     C* imposta le date di lavoro
053200970718     c                   EXSR      impdat
053300970627     C*
053400970627     C                   ENDSR
053500970627     O*--------------------------------------------------------------------------------------------*
053600970627** -msg-
053700970714TISI90R- Immettere uno dei valori possibili                                   1
053800970711TISI90R- Data errata                                                          2
053900970718TISI90R- La data finale deve essere maggiore o uguale alla data iniziale      3
054000970718TISI90R- La data DAL è inferiore alla data ultima di esistenza bolle          4
054100970718TISI90R- La data AL non può essere maggiore della data corrente               5
054200000608TISI90R- Le date sono obbligatorie                                            6
054300970718TISI90R- 7
054400970627TISI90R- 8
054500970627TISI90R- 9
054600970627TISI90R- 10
