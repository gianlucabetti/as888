000100970711     H*--------------------------------------------------------------------------------------------*
000200000622     H* Controlla località bolle in cappario - Esegue
000300970711     H*--------------------------------------------------------------------------------------------*
000400970711     H DECEDIT('0,') DATEDIT(*DMY.)
000500970711     F*--------------------------------------------------------------------------------------------*
000600970711     F* Data base
000700970711     F*--------------------------------------------------------------------------------------------*
000800971126     Ftabel00f  IF   E           K DISK                                         *Tabelle
000900971204     Fazorg01l  IF   E           K DISK                                         *Filiali
001000970711     Ffnarb01l  IF   E           K DISK    USROPN                               *Bolle arrivo
001100970711     Ffnblp01l  IF   E           K DISK    USROPN                               *Bolle partenza
001200000608     Ftitas38c  IF   E           K DISK    USROPN                               *Bolle di Sede
001300020702     Ftntbe01l  IF   E           K DISK
001400970711     Fwfloc01l  UF A E           K DISK                                         *Workfile
001500970711     d*--------------------------------------------------------------------------------------------*
001600970711     d* Data structure
001700970711     d*--------------------------------------------------------------------------------------------*
001800970711     d*----------------
001900970711     d* Schiere
002000970711     d*----------------
002100971126     d cbo             S              2    DIM(50)                              *codice bolla
002200971126     d ctb1            S              1    DIM(50)                              *tipo bolla 1
002300971126     d ctb2            S              1    DIM(50)                              *tipo bolla 2
002400971126     d cfca            S              1  0 DIM(50)                              *c/assegno
002500971204     d filit           S              3  0 DIM(999)                             *filiali italia
002600020227     d filfed          S              3  0 DIM(999)                             *filiali FED-EX
002700970711     d*----------------
002800970711     d* Variabili riferite al data base
002900970711     d*----------------
003000970714     d kwldri          S                   LIKE(wfldri)                         *lettura wfloc01l
003100970714     d kwlnar          S                   LIKE(wflnar)
003200970714     d kwlcap          S                   LIKE(wflcap)
003300970714     d kwlloc          S                   LIKE(wflloc)
003400970714     d kwlprv          S                   LIKE(wflprv)
003500971126     d ktbkut          S                   LIKE(tblkut)                         *lettura tabel00f
003600971126     d ktbcod          S                   LIKE(tblcod)
003700000608     d ktaaas          S                   LIKE(tasaas)                         *lettura titas38c
003800000608     d ktamgs          S                   LIKE(tasmgs)
003900970714     d wlna            S                   LIKE(blplna)                         *di lavoro
004000970714     d wnar            S                   LIKE(wflnar)
004100970714     d wcap            S                   LIKE(wflcap)
004200970714     d wloc            S                   LIKE(wflloc)
004300970714     d wprv            S                   LIKE(wflprv)
004400970714     d wdri            S                   LIKE(wfldri)
004500970714     d wncl            S                   LIKE(wflncl)
004600971126     d wcbo            S                   LIKE(arbcbo)
004700971126     d wtpo            S                   LIKE(i95tpo)
004800971126     d wfca            S                   LIKE(i95fca)
004900970711     d*----------------
005000970711     d* Variabili di controllo
005100970711     d*----------------
005200970711     d $fine           S              1    INZ('N')                             *fine programma
005300970711     d $recok          S              1    INZ('N')                             *validità record
005400970711     d $err            S              1    INZ('0')                             *errore generico
005500970711     d*----------------
005600970711     d* Variabili a programma
005700970711     d*----------------
005800970714     d wsloc           S              1    INZ('N')                             località da scrivere
005900971126     d i               S              5  0                                      indice
006000020227     d k               S              3  0                                      indice
006100970711     d*----------------
006200970711     d* Ds
006300970711     d*----------------
006400970711     d wlbda8          DS                  INZ                                  *controlla data
006500970711     d  g08dat                        8  0                                       -data
006600970711     d  g08inv                        8  0                                       -data invertita
006700970711     d  g08err                        1                                          -errore
006800970711     d  g08tgi                        5  0                                       -giorni data
006900970715     d                 DS                  INZ                                  *data sped bolle arr
007000970715     d  blpaas                 1      4  0                                       -anno
007100970715     d  blpmgs                 5      8  0                                       -mese/giorno
007200970715     d  blpdsp                 1      8  0                                      *data sped bolle par
007300970714     d                 DS                  INZ                                  *data sped bolle arr
007400970714     d  arbaas                 1      4  0                                       -anno
007500970714     d  arbmgs                 5      8  0                                       -mese/giorno
007600970714     d  arbdsp                 1      8  0                                      *data sped bolle arr
007700000608     d                 DS                  INZ                                  *data sped bolle arr
007800000608     d  tasaas                 1      4  0                                       -anno
007900000608     d  tasmgs                 5      8  0                                       -mese/giorno
008000000608     d  tasdsp                 1      8  0                                      *data sped bolle sed
008100000608     d                 DS                  INZ                                  *data sped bolle arr
008200000608     d  wrkaas                 1      4  0                                       -anno
008300000608     d  wrkmgs                 5      8  0                                       -mese/giorno
008400000608     d  wrkdai                 1      8  0                                      *data iniziale a vid
008500000608
008600970711     d dspar90         DS                                                       *ds parametri lancio
008700970711     d  p90bpa                        1                                          -tipo bolla
008800970711     d  p90dai                        8  0                                       -data iniziale
008900970711     d  p90daf                        8  0                                       -data finale
009000970711     d  p90ld1                        1                                          -livello dati 1
009100970711     d  p90ld2                        1                                          -livello dati 2
009200970711     d  p90ld3                        1                                          -livello dati 3
009300970711     d  p90ld4                        1                                          -livello dati 4
009400970711     d  p90ld5                        1                                          -livello dati 5
009500970711     d  p90la1                        1                                          -livello aff. 1
009600970711     d  p90la2                        1                                          -livello aff. 2
009700970711     d  p90la3                        1                                          -livello aff. 3
009800970711     d  p90la4                        1                                          -livello aff. 4
009900970711     d  p90la5                        1                                          -livello aff. 5
010000000608     d  p90sed                        1                                          -lancio da SEDE
010100020227     d  p90dri                        8  0                                       -data riferiemento
010200970711     d dssi95        E DS                  EXTNAME(tisi95ds)                    *ds "TISI95R"
010300020227     d ds3a          E DS                                                       *codici bolla
010400020227     d og143         E DS                                                       *ds campo ORGDE3
010500970711     d kpjba         E DS                                                       *Architettura
010600020702     d dntw          E DS                                                       *tabella NTW
010700970711     C*--------------------------------------------------------------------------------------------*
010800970711     C* Main lines
010900970711     C*--------------------------------------------------------------------------------------------*
011000970711     C*
011100970711     C* apertura files
011200970711     c                   EXSR      apre
011300970711     C*
011400970711     C* legge le bolle arrivo
011500970711     c                   EXSR      setfil
011600970711     C*
011700970711     C* ciclo fino a fine file
011800970711do  1c                   DOW       $fine='N'
011900970828     C*
012000970828     C* controlla validità record
012100970828     c                   MOVEL     'N'           $recok
012200970828     c                   EXSR      chkrec
012300970711     C*
012400970714     C* controlla la località
012500970828if  2c                   IF        $recok = 'S'
012600970714     c                   MOVEL     'N'           wsloc
012700970711     c                   EXSR      chkloc
012800970711     C*
012900970711     C* scrive il file di lavoro
013000970828if  3c                   IF        wsloc = 'S'
013100970711     c                   EXSR      scrwfl
013200970828e   3c                   ENDIF
013300970828e   2c                   ENDIF
013400970711     C*
013500970711     C* lettura sucessiva file
013600970711     c                   EXSR      letfil
013700970828e   1c                   ENDDO
013800970711     C*
013900970711     C* operazioni di chiusura
014000970711     c                   EXSR      chiude
014100970711     C*
014200970711     C                   SETON                                        lr
014300970711     c*--------------------------------------------------------------------------------------------*
014400970711     c* setfil - posizionamento sul file e prima lettura
014500970711     c*--------------------------------------------------------------------------------------------*
014600970711     C     setfil        BEGSR
014700970711     C*
014800970711     C* posizionamento sul file
014900000608     c                   IF        p90sed = 'S'
015000000608     c                   Z-ADD     p90dai        wrkdai
015100000608     c                   Z-ADD     wrkaas        ktaaas
015200000608     c                   Z-ADD     wrkmgs        ktamgs
015300000608     c     keytas        SETLL     titas38c                           99        *bolle di sede
015400000608     c                   ELSE
015500970711if  1c                   IF        p90bpa = 'P'
015600970711     c     *loval        SETLL     fnblp01l                           99        *bolle partenza
015700970711x   1c                   ELSE
015800970711     c     *loval        SETLL     fnarb01l                           99        *bolle arrivo
015900970711e   1C                   ENDIF
016000000608     C                   ENDIF
016100970711     C*
016200970711if  1c                   IF        NOT *in99
016300970711     c                   EXSR      letfil                                       *lettura record
016400970711x   1c                   ELSE
016500970711     C                   MOVEL     'S'           $fine                          *fine programma
016600970711e   1C                   ENDIF
016700970711     C*
016800970711     C                   ENDSR
016900970711     c*--------------------------------------------------------------------------------------------*
017000970711     c* letfil - lettura record
017100970711     c*--------------------------------------------------------------------------------------------*
017200970711     C     letfil        BEGSR
017300970828     C*
017400000608     c                   IF        p90sed = 'S'
017500000608     c                   READ      titas38c                               99    *bolle in sede
017600000608     c                   ELSE
017700970828if  1c                   IF        p90bpa = 'P'
017800970711     C                   READ      fnblp01l                               99    *bolle partenza
017900970828x   1c                   ELSE
018000970711     C                   READ      fnarb01l                               99    *bolle arrivo
018100970828e   1c                   ENDIF
018200000608     c                   ENDIF
018300970711     C*
018400970828if  1c                   IF        *in99                                        *fine file
018500970711     c                   MOVEL     'S'           $fine                          *fine programma
018600970828e   1c                   ENDIF
018700970711     C*
018800970711     C                   ENDSR
018900970711     c*--------------------------------------------------------------------------------------------*
019000970711     c* chkrec - controlla validità record
019100970711     c*--------------------------------------------------------------------------------------------*
019200970711     C     chkrec        BEGSR
019300970711     C*
019400970711     c                   MOVEL     'S'           $recok                         *record valido
019500970711     C*
019600970711     C* imposta i campi da controllare
019700000608     c                   IF        p90sed = 'S'
019800000608     c                   Z-ADD     taslna        wlna
019900000608     c                   Z-ADD     tasdsp        wdri
020000000608     c                   ELSE
020100970711if  1c                   IF        p90bpa = 'P'                                 *dati partenza
020200970711     c                   Z-ADD     blplna        wlna
020300970714     c                   Z-ADD     blpdsp        wdri
020400970711x   1c                   ELSE                                                   *dati arrivo
020500970714     c                   Z-ADD     arblna        wlna
020600970714     c                   Z-ADD     arbdsp        wdri
020700970714e   1c                   ENDIF
020800000608     c                   ENDIF
020900970711     C*
021000970711     C* linea arrivo bolle estero, esclude
021100971204if  1c                   IF        $recok = 'S'                                 *ancora valido
021200971204     c     wlna          LOOKUP    filit                                  98
021300971204if  2c                   IF        NOT *in98                                    *non trovata
021400970711     c                   MOVEL     'N'           $recok                         *record non valido
021500971204e   2c                   ENDIF
021600971204e   1c                   ENDIF
021700970711     C*
021800970711     C* data spedizione bolla <> dal periodo richiesto, esclude
021900970715if  1c                   IF        wdri >= p90dai AND
022000970715     c                             wdri <= p90daf
022100970714x   1c                   ELSE
022200970711     c                   MOVEL     'N'           $recok                         *record non valido
022300000608     c                   IF        p90sed = 'S'
022400000608     c                   SETON                                        99
022500000608     c                   ENDIF
022600970711e   1c                   ENDIF
022700970711     C*
022800970711     C                   ENDSR
022900970714     C*--------------------------------------------------------------------------------------------*
023000970714     C* chkloc - controlla la località
023100970714     C*--------------------------------------------------------------------------------------------*
023200970714     c     chkloc        BEGSR
023300970714     C*
023400970714     C* imposta le variabili di lavoro
023500000608     c                   IF        p90sed = 'S'                                 -SEDE-
023600000608     c                   MOVEL     tasnzd        wnar                           *nazione
023700000608     c                   MOVEL     tascad        wcap                           *cap
023800000608     c                   MOVEL     taslod        wloc                           *località
023900000608     c                   MOVEL     tasprd        wprv                           *provincia
024000000608     c                   Z-ADD     tasdsp        wdri                           *data di riferimento
024100000608     c                   Z-ADD     tasncl        wncl                           *numero colli
024200000608     c                   MOVEL     tascbo        wcbo                           *codice bolla
024300000608     c                   ELSE
024400970714if  1c                   IF        p90bpa = 'P'                                 -PARTENZA-
024500970714     c                   MOVEL     blpnzd        wnar                           *nazione
024600970714     c                   MOVEL     blpcad        wcap                           *cap
024700970714     c                   MOVEL     blplod        wloc                           *località
024800970714     c                   MOVEL     blpprd        wprv                           *provincia
024900970714     c                   Z-ADD     blpdsp        wdri                           *data di riferimento
025000970714     c                   Z-ADD     blpncl        wncl                           *numero colli
025100971126     c                   MOVEL     blpcbo        wcbo                           *codice bolla
025200970714x   1c                   ELSE                                                   -ARRIVO-
025300970714     c                   MOVEL     arbnzd        wnar                           *nazione
025400970714     c                   MOVEL     arbcad        wcap                           *cap
025500970714     c                   MOVEL     arblod        wloc                           *località
025600970714     c                   MOVEL     arbprd        wprv                           *provincia
025700970714     c                   Z-ADD     arbdsp        wdri                           *data di riferimento
025800970714     c                   Z-ADD     arbncl        wncl                           *numero colli
025900971126     c                   MOVEL     arbcbo        wcbo                           *codice bolla
026000970714e   1c                   ENDIF
026100000608     c                   ENDIF
026200971126     C*
026300971126     C* reperisce il tipo porto e l'esistenza del contrassegno
026400971126     c                   EXSR      chkcbo
026500970714     C*
026600970714     C* lancia il programma di controllo cap
026700970714     c                   CLEAR                   dssi95                         *azzera i parametri
026800970714     c                   MOVEL     *blanks       i95tla                         *tipo lancio
026900970714     c                   MOVEL     '7'           i95tcn                         *tipo controllo
027000970714     c                   MOVEL     wnar          i95nar                         *nazione
027100970714     c                   MOVEL     wcap          i95cap                         *cap
027200970714     c                   MOVEL     wloc          i95loc                         *località
027300970714     c                   MOVEL     wprv          i95prv                         *provincia
027400970714     c                   Z-ADD     wdri          i95dat                         *data di riferimento
027500000830if  1c                   IF        p90dri > *zeros                              *data richiesta <>
027600000830     c                   Z-ADD     p90dri        i95dat                         *data di riferimento
027700000830e   1c                   ENDIF
027800971126     c                   MOVEL     wtpo          i95tpo                         *tipo porto
027900971126     c                   MOVEL     wfca          i95fca                         *contrassegno
028000020227     c*
028100020227     c* VERIFICO SE LNA E' FILIALE FEDEX
028200020227     c                   IF        wlna > *zeros
028300020227     c     wlna          LOOKUP    filfed                                 55
028400020227     c                   IF        *in55 = *on
028500020227     c                   EVAL      i95fi1 = 'S'
028600020227     c                   ENDIF
028700020227     c                   ENDIF
028800020227     c*
028900020227     c                   CALL      'TISI95R'
029000970714     c                   PARM                    dssi95
029100970714     C*
029200970714     C* controlla se la località è uscita dal controllo con un livello dati richiesto
029300970714if  1c                   IF        o95lid = p90ld1 OR                           *livello richiesto
029400970714     c                             o95lid = p90ld2 OR
029500970714     c                             o95lid = p90ld3 OR
029600970714     c                             o95lid = p90ld4 OR
029700970714     c                             o95lid = p90ld5 OR
029800970714     c                             p90ld1 = *blanks AND                         *tutti i livelli
029900970714     c                             p90ld2 = *blanks AND
030000970714     c                             p90ld3 = *blanks AND
030100970714     c                             p90ld4 = *blanks AND
030200970714     c                             p90ld5 = *blanks OR
030300970714     c                             o95err <> *blanks                            *errore
030400970714     C*
030500970714     C* controlla se la località è uscita dal controllo con un livello affidabilità richiesto
030600970714if  2c                   IF        o95lia = p90la1 OR                           *livello richiesto
030700970714     c                             o95lia = p90la2 OR
030800970714     c                             o95lia = p90la3 OR
030900970714     c                             o95lia = p90la4 OR
031000970714     c                             o95lia = p90la5 OR
031100970714     c                             p90la1 = *blanks AND                         *tutti i livelli
031200970714     c                             p90la2 = *blanks AND
031300970714     c                             p90la3 = *blanks AND
031400970714     c                             p90la4 = *blanks AND
031500970714     c                             p90la5 = *blanks OR
031600970714     c                             o95err <> *blanks                            *errore
031700970714     c                   MOVEL     'S'           wsloc                          *scrivere località
031800970714e   2c                   ENDIF
031900970714e   1c                   ENDIF
032000970714     C*
032100970714     c                   ENDSR
032200970714     C*--------------------------------------------------------------------------------------------*
032300970714     C* scrwfl - scrive il file di lavoro
032400970714     C*--------------------------------------------------------------------------------------------*
032500970714     C     scrwfl        BEGSR
032600970714     C*
032700970714     C* legge il record del file di lavoro
032800970714     c                   Z-ADD     wdri          kwldri                         *data di riferimento
032900970714     c                   MOVEL     wnar          kwlnar                         *nazione
033000970714     c                   MOVEL     wcap          kwlcap                         *cap
033100970714     c                   MOVEL     wloc          kwlloc                         *località
033200970714     c                   MOVEL     wprv          kwlprv                         *provincia
033300970714     c     keywfl        CHAIN     wfloc01l                           99
033400970714     c*
033500970714     c* imposta i campi del record da scrivere
033600970714     c                   MOVEL     o95lid        wfllid                         livello dati
033700970714     c                   MOVEL     o95lia        wfllia                         livello affidabilità
033800970714     c                   MOVEL     o95err        wflerr                         errore
033900000725     c                   MOVEL     o95CAP        wflcao                         cap trovato
034000970714     c*
034100970714     c* inserimento
034200970714if  1c                   IF        *in99
034300970714     c                   Z-ADD     kwldri        wfldri                         *data di riferimento
034400970714     c                   MOVEL     kwlnar        wflnar                         *nazione
034500970714     c                   MOVEL     kwlcap        wflcap                         *cap
034600970714     c                   MOVEL     kwlloc        wflloc                         *località
034700970714     c                   MOVEL     kwlprv        wflprv                         *provincia
034800971029     c                   Z-ADD     1             wflspe                         *numero spedizioni
034900970714     c                   Z-ADD     wncl          wflncl                         *numero colli
035000000608     c                   IF        p90sed = 'S'                                 -SEDE-
035100000608     c                   Z-ADD     taslnp        wfllnp                         *linea di partenza
035200000608     c                   Z-ADD     tasnrs        wflnrs                         *serie
035300000608     c                   Z-ADD     tasnsp        wflnsp                         *spedizione
035400000608     c                   Z-ADD     tasksc        wflksc                         *cliente
035500000608     c                   ELSE
035600971029if  2c                   IF        p90bpa = 'P'                                 -PARTENZA-
035700971030     c                   Z-ADD     blplnp        wfllnp                         *linea di partenza
035800971030     c                   Z-ADD     blpnrs        wflnrs                         *serie
035900971029     c                   Z-ADD     blpnsp        wflnsp                         *spedizione
036000971029     c                   Z-ADD     blpksc        wflksc                         *cliente
036100971029x   2c                   ELSE                                                   -ARRIVO-
036200971030     c                   Z-ADD     arblnp        wfllnp                         *linea di partenza
036300971029     c                   Z-ADD     arbnrs        wflnrs                         *serie
036400971029     c                   Z-ADD     arbnsp        wflnsp                         *spedizione
036500971029     c                   Z-ADD     arbksc        wflksc                         *cliente
036600971029e   2c                   ENDIF
036700000608     c                   ENDIF
036800970714     C                   WRITE     wfloc000
036900970714     c* modifica
037000970714x   1c                   ELSE
037100971029     c                   ADD       1             wflspe                         *numero spedizioni
037200970828     c                   ADD       wncl          wflncl                         *numero colli
037300970714     C                   UPDATE    wfloc000
037400970714e   1c                   ENDIF
037500970714     C*
037600970714     c                   ENDSR
037700971126     C*--------------------------------------------------------------------------------------------*
037800971126     C* chkcbo - reperisce il tipo porto e l'esistenza del contrassegno
037900971126     C*--------------------------------------------------------------------------------------------*
038000971126     C     chkcbo        BEGSR
038100971126     C*
038200971126     C* reimposta le variabili di lavoro
038300971126     c                   MOVEL     'F'           wtpo
038400971126     c                   MOVEL     'N'           wfca
038500971126     C*
038600971126     c                   Z-ADD     1             i
038700971204     c     wcbo          LOOKUP    cbo(i)                                 98
038800971126if  1c                   IF        *in98                                        *trovato
038900971126if  2c                   IF        ctb1(i) = 'A' OR
039000971126     c                             ctb2(i) = 'A'
039100971126     c                   MOVEL     'A'           wtpo                           *tipo porto
039200971126e   2c                   ENDIF
039300971126if  2c                   IF        cfca(i) <> 0
039400971126     c                   MOVEL     'S'           wfca                           *c/assegno
039500971126e   2c                   ENDIF
039600971126e   1c                   ENDIF
039700971126     C*
039800971126     c                   ENDSR
039900970711     C*--------------------------------------------------------------------------------------------*
040000970711     C* apre - operazioni di apertura
040100970711     C*--------------------------------------------------------------------------------------------*
040200970711     C     apre          BEGSR
040300970711     c*
040400970711     c* files
040500000608     c                   IF        p90sed = 'S'
040600000608     c                   OPEN      titas38c                                     *bolle di sede
040700000608     c                   ELSE
040800970711if  1c                   IF        p90bpa = 'P'
040900970711     c                   OPEN      fnblp01l                                     *bolle partenza
041000970711x   1c                   ELSE
041100970711     c                   OPEN      fnarb01l                                     *bolle arrivo
041200970711e   1c                   ENDIF
041300000608     c                   ENDIF
041400970711     c*
041500970711     C                   ENDSR
041600970711     C*--------------------------------------------------------------------------------------------*
041700970711     C* chiude - operazioni di chiusura
041800970711     C*--------------------------------------------------------------------------------------------*
041900970711     C     chiude        BEGSR
042000970711     c*
042100970711     c* files
042200000608     c                   IF        p90sed = 'S'
042300000608     c                   CLOSE     titas38c                                     *bolle di sede
042400000608     c                   ELSE
042500970711if  1c                   IF        p90bpa = 'P'
042600970711     c                   CLOSE     fnblp01l                                     *bolle partenza
042700970711x   1c                   ELSE
042800970711     c                   CLOSE     fnarb01l                                     *bolle arrivo
042900970711e   1c                   ENDIF
043000000608     c                   ENDIF
043100970711     c*
043200970711     c* programmi
043300970711     c                   CLEAR                   dssi95
043400970711     c                   MOVEL     'C'           i95tla
043500970711     c                   CALL      'TISI95R'
043600970711     c                   PARM                    dssi95
043700970711     c*
043800970711     C                   ENDSR
043900971126     C*--------------------------------------------------------------------------------------------*
044000971126     C* cartab - carica le tabelle occorrenti
044100971126     C*--------------------------------------------------------------------------------------------*
044200971126     C     cartab        BEGSR
044300971204     C*---
044400971204     C* codici bolla
044500971204     C*---
044600971126     c                   Z-ADD     *zeros        i
044700020906     C                   Z-ADD     1             ktbkut
044800971126     C                   MOVEL     '3A'          ktbcod
044900971126     C     keytab        SETLL     tabel00f
045000971126     C     keytab        READE     tabel00f                               99
045100971126do  1c                   DOW       NOT *in99
045200971126if  2c                   IF        tblflg = *blanks                             *valido
045300971126     c                   MOVEL     tbluni        ds3a
045400971126     c                   ADD       1             i
045500971126     c                   MOVEL     tblkey        cbo(i)
045600971126     c                   MOVEL     §3atb1        ctb1(i)
045700971126     c                   MOVEL     §3atb2        ctb2(i)
045800971126     c                   Z-ADD     §3afca        cfca(i)
045900971126e   2c                   ENDIF
046000971126     C     keytab        READE     tabel00f                               99
046100971126e   1c                   ENDDO
046200020227     C*---
046300020227     C* filiali italia/FEDEX
046400020227     C*---
046500020227     c                   Z-ADD     *zeros        k
046600971204     c                   Z-ADD     *zeros        i
046700020227     c     *loval        SETLL     azorg01l
046800020227     c                   READ      azorg01l                               99
046900971204do  1c                   DOW       NOT *in99
047000971204if  2c                   IF        orgfva = *blanks AND                         *valido
047100971204     c                             orgfag <> 'V'                                *filiali o agenzie
047200020627     c                   MOVEL     orgde3        og143
047300020702     C*
047400020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
047500020702     C                   CLEAR                   DNTW
047600020702     C*
047700020702     C                   MOVEL(P)  'NTW'         KTECOD
047800020702     C                   MOVEL(P)  §OGNTW        KTEKE1
047900020702     C     KEYTBE        CHAIN     TNTBE01L
048000020702     C                   IF        %found(TNTBE01L)
048100020702     C                   MOVEL     TBEUNI        DNTW
048200020702     C                   ENDIF
048300020702     C*
048600020702     C                   IF        §NTWFIE = *blanks
048700020702     C                   ADD       1             i
048800020702     C                   Z-ADD     orgfil        filit(i)
048900020702     C                   ENDIF
049100020227     c* VERIFICO SE FILIALE FEDEX
049200020227     c                   IF        §ogntw = 'FED'
049300020227     c                   ADD       1             K
049400020227     c                   Z-ADD     orgfil        filfed(k)
049500020227     c                   ENDIF
049600020227e   2c                   ENDIF
049700020227     c                   READ      azorg01l                               99
049800971204e   1c                   ENDDO
049900971126     c*
050000020227     c                   ENDSR
050100970711     C*--------------------------------------------------------------------------------------------*
050200970711     C* *inzsr - Operazioni iniziali
050300970711     C*--------------------------------------------------------------------------------------------*
050400970711     C     *inzsr        BEGSR
050500970711     C*---
050600970711     C* ricevimento parametri
050700970711     C*---
050800970711     C     *ENTRY        PLIST
050900970711     C                   PARM                    kpjba
051000970711     C                   MOVEL     kpjbu         dspar90
051100020702     C*---
051200020702     C* variabili riferite al database
051300020702     C*---
051400020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
051500020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
051600970711     C*---
051700970711     C* chiavi di lettura
051800970711     C*---
051900970711     C* di wfloc01l - completa
052000970711     C     keywfl        KLIST
052100970711     C                   KFLD                    kwldri                         *data riferimento
052200970711     C                   KFLD                    kwlnar                         *nazione
052300970711     C                   KFLD                    kwlcap                         *cap
052400970711     C                   KFLD                    kwlloc                         *località
052500970711     C                   KFLD                    kwlprv                         *provincia
052600971126     C* di tabel00f - parziale
052700971126     C     keytab        KLIST
052800971126     C                   KFLD                    ktbkut                         *utente
052900971126     C                   KFLD                    ktbcod                         *tabella
053000000608     C* di titas38c - parziale
053100000608     C     keytas        KLIST
053200000608     C                   KFLD                    ktaaas                         *anno
053300000608     C                   KFLD                    ktamgs                         *mese/giorno
053400020702     C*di tntbe01l  - parziale
053500020702     C     keytbe        KLIST
053600020702     C                   KFLD                    ktecod                         *codice
053700020702     C                   KFLD                    kteke1                         *chiave
053800971126     C*
053900971126     C* carica le tabelle occorrenti
054000971126     c                   EXSR      cartab
054100970711     C*
054200970711     C                   ENDSR
