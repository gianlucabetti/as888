000100990101     h*--------------------------------------------------------------------------------------------*
000200990101     h* Controlla instradamenti in filiale - Richiesta parametri
000300990101     h*--------------------------------------------------------------------------------------------*
000400990101     h DECEDIT('0,') DATEDIT(*DMY.)
000500990101     f*--------------------------------------------------------------------------------------------*
000600990101     f* DATA BASE
000700990101     f*--------------------------------------------------------------------------------------------*
000800990101     ftisi75d   CF   E             WORKSTN
000900990101     fcnaco00f  IF   E           K DISK
001000990101     fazorg01l  IF   E           K DISK
001100990101     ftabel00f  IF   E           K DISK
001200990101     d*--------------------------------------------------------------------------------------------*
001300990101     d* Data structure
001400990101     d*--------------------------------------------------------------------------------------------*
001500990101     d*----------------
001600990101     d* schiere
001700990101     d*----------------
001800990101     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001900990101     d ri              S              4  0 DIM(4)                               *tabelle capoconto
002000990101     d*----------------
002100990101     d* Variabili riferite al data base
002200990101     d*----------------
002300990101     d ktbkut          S                   LIKE(tblkut)                         *per tabel00f
002400990101     d ktbcod          S                   LIKE(tblcod)
002500000622     d ktbkey          S                   LIKE(tblkey)
002600990101     d kcokut          S                   LIKE(acokut)                         *per cnaco00f
002700990101     d kcokcc          S                   LIKE(acokcc)
002800990101     d kcoksc          S                   LIKE(acoksc)
002900990101     d krgfil          S                   LIKE(orgfil)                         *per azorg00f
003000990101     d*----------------
003100990101     d* Variabili di controllo
003200990101     d*----------------
003300990101     d $fine           S              1    INZ('N')                             *Gestione
003400990101     d $tesvd          S              1    INZ('1')                             *riemettere testata
003500990101     d $tipvid         S              1    INZ('1')                             *gestione video
003600990101     d*----------------
003700990101     d* Variabili a programma
003800990101     d*----------------
003900990101     d wn14            S             14  0                                      *Numerico di 14
004000990101     d wn8             S              8  0                                      *Numerico di 8
004100990101     d werr            S              1    INZ('0')                             *esistenza versioni
004200990101     d wda8            S              8  0                                      *data di 8 in g/m/a
004300990101     d da8w            S              8  0                                      *data di 8 in a/m/g
004400990101     d wksc            S              7  0                                      *cliente
004500990101     d wdksc           S             30                                         *descrizione cliente
004600990101     d wlnp            S              3  0                                      *filiale
004700990101     d wdlnp           S             30                                         *descrizione filiale
004800990101     d ds1v1c          S              8  0                                      *data iniziale
004900990101     d ds2v1c          S              8  0                                      *data finale
005000990101     d p94pgm          S             10                                         *parametri TRUL94C
005100000103     d p94dat          S              8
005200990101     d p94err          S              1
005300990101     d datcor          S              8  0                                      *data corrente a/m/g
005400990101     d cordat          S              8  0                                      *data corrente g/m/a
005500990101     d §programma      S             10                                         *programma
005600990101     d §giorni         S              3  0                                      *giorni
005700990101     d §datini         S              8  0                                      *data inizio aa/m/g
005800990101     d pdatini         S              8  0                                      *partenza
005900990101     d adatini         S              8  0                                      *arrivo
006000990101     d §datfin         S              8  0                                      *data fine   aa/m/g
006100990101     d pdatfin         S              8  0                                      *partenza
006200990101     d adatfin         S              8  0                                      *arrivo
006300990101     d §inidat         S              8  0                                      *data inizio g/m/aa
006400990101     d pinidat         S              8  0                                      *partenza
006500990101     d ainidat         S              8  0                                      *arrivo
006600990101     d §findat         S              8  0                                      *data fine   g/m/aa
006700990101     d pfindat         S              8  0                                      *partenza
006800990101     d afindat         S              8  0                                      *arrivo
006900990101     d*----------------
007000990101     d* Ds
007100990101     d*----------------
007200020919     D TIBS34DS      E DS                                                       *Profili utente
007300020919     D DDATIUTE      E DS                                                       *Dati utente
007400020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007500990101     d ds5a          E DS                                                       *giorni di pulizia
007600990101     d*------------------
007700990101     d* DS caricamento capoconti di contabilità
007800990101     d*------------------
007900990101     d carcnt          DS
008000990101     d  fl12cn                 1      2
008100990101     d  fl34cn                 3      4
008200990101     d  tipcnt                 5      6
008300990101     d tcuds           DS
008400990101     d  e1                     1      1
008500990101     d  e3                     3      3
008600990101     d  e2                     2      2
008700990101     d  e4                     4      4
008800990101     d  e56                    5      6
008900990101     d wlbda8          DS                  INZ                                  *controlla data
009000990101     d  g08dat                        8  0                                       -data
009100990101     d  g08inv                        8  0                                       -data invertita
009200990101     d  g08err                        1                                          -errore
009300990101     d  g08tgi                        5  0                                       -n°giorni
009400990101     d wgidat          DS                                                       *crea data da giorni
009500990101     d  giodat                        8  0                                       -data
009600990101     d  gioinv                        8  0                                       -data invertita
009700990101     d  giotgi                        5  0                                       -n°giorni
009800990101     d dspar75         DS                                                       *ds parametri lancio
009900990101     d  p75pav                        1                                          -tipo bolla
010000990101     d  p75ds1                        8  0                                       -data iniziale
010100990101     d  p75ds2                        8  0                                       -data finale
010200990101     d  p75ksc                        7  0                                       -cliente
010300990101     d  p75lnp                        3  0                                       -linea partenza
010400990730     d  p75fns                        1                                          -bolle con serie
010500000627     d  p75fel                        1                                          -errori di linea
010600000731     d  p75nrs                        2  0                                       -serie
010700011126     d  p75tot                        1                                          -solo totali
010800020621     d  p75bpt                        1                                          -anche bolle PT
010900990101     d kpjba         E DS                                                       *architettura
011000990101     c*--------------------------------------------------------------------------------------------*
011100990101     c* Main lines
011200990101     c*--------------------------------------------------------------------------------------------*
011300990101     C*
011400990101     C* impostazioni testata
011500990101     C                   EXSR      impvd0
011600990101     C*
011700990101     C* impostazioni prima videata
011800990101     C                   EXSR      impvd1
011900990101     C*
012000990101     C* ciclo fino a richiesta di fine elaborazione
012100990101do  1C                   DOU       $fine='S'
012200990101     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
012300990101     C                   ENDCS
012400990101e   1C                   ENDDO
012500990101     C*
012600990101     C                   SETON                                        lr
012700990101     c*--------------------------------------------------------------------------------------------*
012800990101     c* impvd1 - impostazioni prima videata
012900990101     c*--------------------------------------------------------------------------------------------*
013000990101     C     impvd1        BEGSR
013100990101     C*
013200990101     C* reimposta formato video e indicatori
013300990101     C                   EXSR      setvd1
013400990101     C                   MOVEL     '1'           $tipvid                        *gestione video
013500990730     C* campi videata
013600011126     C                   MOVEL     'N'           v1cfns                         *selezioni
013700000627     C                   MOVEL     'N'           v1cfel                         *selezioni
013800011126     C                   MOVEL     'N'           v1ctot                         *selezioni
013900020927     C                   IF        DUTLPO = 'S'
014000020621     C                   MOVEL     'S'           v1cbpt                         *selezioni
014100020927     C                   ELSE
014200020927     C                   MOVEL     'N'           v1cbpt                         *selezioni
014300020927     C                   ENDIF
014400990101     C*
014500990101     C                   ENDSR
014600990101     c*--------------------------------------------------------------------------------------------*
014700990101     c* setvd1 - impostazioni indicatori prima videata
014800990101     c*--------------------------------------------------------------------------------------------*
014900990101     C     setvd1        BEGSR
015000990730     c*
015100990101     C                   SETOFF                                       606162    *errori
015200000627     C                   SETOFF                                       636465
015300000731     C                   SETOFF                                       6667
015400990101     C                   SETOFF                                       90        *generico di errore
015500990101     C                   MOVEL     *blanks       $msg                           *messaggio
015600990101     C*
015700990101     C                   ENDSR
015800990101     c*--------------------------------------------------------------------------------------------*
015900990101     C* gesvd1 - gestione prima videata
016000990101     c*--------------------------------------------------------------------------------------------*
016100990101     C     gesvd1        BEGSR
016200990101     C*
016300990101     C* emissione videata
016400990101if  1C                   IF        $tesvd='1'
016500990101     C                   WRITE     si75d00                                      *testata
016600990101     C                   MOVEL     '0'           $tesvd
016700990101e   1C                   ENDIF
016800990101     C                   EXFMT     si75d01                                      *video
016900990101     C                   EXSR      setvd1
017000990101     C* F03-Fine
017100990101if  1C                   IF        *inkc
017200990101     C                   EXSR      f03ges
017300990101     C                   GOTO      finvd1
017400990101e   1C                   ENDIF
017500990101     C* F12-Ritorno
017600990101if  1C                   IF        *inkl
017700990101     C                   EXSR      f12ges
017800990101     C                   GOTO      finvd1
017900990101e   1C                   ENDIF
018000990101     C* Enter-Controlla
018100990101     C                   EXSR      ctrvd1
018200990101if  1C                   IF        *in90                                        *errori
018300990101     C                   GOTO      finvd1
018400990101e   1C                   ENDIF
018500990101     C* F06-Conferma
018600990101if  1C                   IF        *inkf
018700990101     C                   EXSR      f06ges
018800990101     C                   GOTO      finvd1
018900990101e   1C                   ENDIF
019000990101     C*
019100990101     C     finvd1        ENDSR
019200990101     c*--------------------------------------------------------------------------------------------*
019300990101     C* ctrvd1 - controlla prima videata
019400990101     c*--------------------------------------------------------------------------------------------*
019500990101     C     ctrvd1        BEGSR
019600990101     C*
019700990101     C* reimposta le variabili di lavoro
019800990101     C                   MOVEL     '0'           werr
019900990101     C*---
020000990101     C* tipo bolla
020100990101     C*---
020200990101if  1C                   IF        v1cpav <> 'P' AND
020300990101     C                             v1cpav <> 'A' AND
020400000626     C                             v1cpav <> 'D' AND
020500011120     C                             v1cpav <> 'T' AND
020600011120     C                             v1cpav <> 'S'
020700990101     c                   MOVEL     '1'           werr
020800990101     C                   MOVEL     msg(1)        $msg
020900990101     C                   SETON                                        6090
021000990101     C                   GOTO      finct1
021100990101e   1C                   ENDIF
021200011120     C*---
021300011120     C* controllo Sede/Filiale
021400011120     C*---
021500011205if  1C                   IF        v1cpav =  'S' AND
021600020919     C                             DUTLPO <> 'S'
021700011120     c                   MOVEL     '1'           werr
021800011120     C                   MOVEL     msg(8)        $msg
021900011120     C                   SETON                                        6090
022000011120     C                   GOTO      finct1
022100011120e   1C                   ENDIF
022200011120     C*---
022300990101     C* Imposta le date se vuote
022400990101     C*---
022500990101if  1c                   IF        v1cds1=*zeros AND v1cds2=*zeros
022600990101if  2C                   IF        v1cpav='P'
022700990101     c                   Z-ADD     pinidat       v1cds1
022800990101     c                   Z-ADD     pfindat       v1cds2
022900990101e   2C                   ENDIF
023000990101if  2C                   IF        v1cpav='A'
023100990101     c                   Z-ADD     ainidat       v1cds1
023200990101     c                   Z-ADD     afindat       v1cds2
023300990101e   2C                   ENDIF
023400990101e   1C                   ENDIF
023500990101     C*---
023600990101     C* data iniziale
023700990101     C*---
023800990101     C                   Z-ADD     *zeros        ds1v1c
023900990101     C                   Z-ADD     v1cds1        wda8
024000990101     C                   Z-ADD     *zeros        da8w
024100990101     C                   MOVEL     '0'           werr
024200990101     C                   EXSR      chkda8
024300990101     C                   Z-ADD     wda8          v1cds1
024400990101     C                   Z-ADD     da8w          ds1v1c
024500990101if  1C                   IF        werr='1'
024600990101     C                   SETON                                        6190
024700990101     C                   MOVEL     msg(2)        $msg
024800990101     C                   GOTO      finct1
024900990101e   1C                   ENDIF
025000000907if  1C*---               IF        v1cpav='P' AND
025100000907     c*---                         ds1v1c < pdatini OR
025200000907     C*---                         v1cpav='A' AND
025300000907     c*---                         ds1v1c < adatini
025400000907     C*---               SETON                                        61
025500000907     C*---               SETON                                        90
025600000907     c*---               MOVEL     '1'           werr
025700000907     C*---               MOVEL     msg(4)        $msg
025800000907     C*---               GOTO      finct1
025900000907e   1C*---               ENDIF
026000990101     C*---
026100990101     C* data finale
026200990101     C*---
026300990101     C                   Z-ADD     *zeros        ds2v1c
026400990101     C                   Z-ADD     v1cds2        wda8
026500990101     C                   Z-ADD     *zeros        da8w
026600990101     C                   MOVEL     '0'           werr
026700990101     C                   EXSR      chkda8
026800990101     C                   Z-ADD     wda8          v1cds2
026900990101     C                   Z-ADD     da8w          ds2v1c
027000990101if  1C                   IF        werr='1'
027100990101     C                   SETON                                        6290
027200990101     C                   MOVEL     msg(2)        $msg
027300990101     C                   GOTO      finct1
027400990101e   1C                   ENDIF
027500000907if  1C*---               IF        v1cpav='P' AND
027600000907     c*---                         ds2v1c > pdatfin OR
027700000907     C*---                         v1cpav='A' AND
027800000907     c*---                         ds2v1c > adatfin
027900000907     C*---               SETON                                        62
028000000907     C*---               SETON                                        90
028100000907     c*---               MOVEL     '1'           werr
028200000907     C*---               MOVEL     msg(5)        $msg
028300000907     C*---               GOTO      finct1
028400000907e   1C*---               ENDIF
028500990101     C*---
028600990101     C* data iniziale + data finale
028700990101     C*---
028800990101if  1c                   IF        ds2v1c < ds1v1c
028900990101     C                   SETON                                        616290
029000990101     C                   MOVEL     msg(3)        $msg
029100990101     c                   MOVEL     '1'           werr
029200990101     C                   GOTO      finct1
029300990101e   1C                   ENDIF
029400990101     C*---
029500990101     C* cliente
029600990101     C*---
029700990101     C                   MOVEL     *blanks       v1dksc
029800990101if  1c                   IF        v1cksc <> *zeros
029900990101     C                   Z-ADD     v1cksc        wksc
030000990101     C                   MOVEL     *blanks       wdksc
030100990101     C                   MOVEL     '0'           werr
030200990101     C                   EXSR      chkksc
030300990101     C                   Z-ADD     wksc          v1cksc
030400990101     C                   MOVEL     wdksc         v1dksc
030500990101if  2C                   IF        werr='1'
030600990101     C                   SETON                                        6390
030700990101     C                   MOVEL     msg(6)        $msg
030800990101     C                   GOTO      finct1
030900990101e   2c                   ENDIF
031000990101e   1C                   ENDIF
031100990101     C*---
031200990101     C* linea di partenza
031300990101     C*---
031400990101     C                   MOVEL     *blanks       v1dlnp
031500990101if  1c                   IF        v1clnp <> *zeros
031600990101     C                   Z-ADD     v1clnp        wlnp
031700990101     C                   MOVEL     *blanks       wdlnp
031800990101     C                   MOVEL     '0'           werr
031900990101     C                   EXSR      chklnp
032000990101     C                   Z-ADD     wlnp          v1clnp
032100990101     C                   MOVEL     wdlnp         v1dlnp
032200990101if  2C                   IF        werr='1'
032300990101     C                   SETON                                        6490
032400990101     C                   MOVEL     msg(7)        $msg
032500990101     C                   GOTO      finct1
032600990101e   2c                   ENDIF
032700990101e   1C                   ENDIF
032800990730     C*---
032900990730     C* solo bolle con serie
033000990730     C*---
033100011126if  1C                   IF        v1cfns <> 'S' AND
033200011126     C                             v1cfns <> 'N'
033300990730     c                   MOVEL     '1'           werr
033400990730     C                   MOVEL     msg(1)        $msg
033500990730     C                   SETON                                        6590
033600990730     C                   GOTO      finct1
033700990730e   1C                   ENDIF
033800020621     C*---
033900020621     C* includi bolle POSTE
034000020621     C*---
034100020621if  1C                   IF        v1cbpt <> 'S' AND
034200020621     C                             v1cbpt <> 'N'
034300020621     c                   MOVEL     '1'           werr
034400020621     C                   MOVEL     msg(1)        $msg
034500020621     C                   SETON                                        6890
034600020621     C                   GOTO      finct1
034700020621e   1C                   ENDIF
034800000627     C*---
034900000627     C* solo errori sulla linea arrivo
035000000627     C*---
035100000627if  1C                   IF        v1cfel <> 'S' AND
035200000627     C                             v1cfel <> 'N'
035300000627     c                   MOVEL     '1'           werr
035400000627     C                   MOVEL     msg(1)        $msg
035500000627     C                   SETON                                        6690
035600000627     C                   GOTO      finct1
035700000627e   1C                   ENDIF
035800020927     C*---
035900020927     C* controlli relativi all'utente
036000020927     C*---
036100020927if  1C                   IF        DUTLPO <> 'S'   AND
036200020927     C                             DUTNTW <> 'PPT' AND
036300021002     C                             v1cbpt =  'S'
036400020927     C                   MOVEL     '1'           werr
036500020927     C                   MOVEL     msg(9)        $msg
036600021002     C                   SETON                                        6890
036700020927     C                   GOTO      finct1
036800020927e   1C                   ENDIF
036900021002if  1C                   IF        DUTLPO <> 'S'   AND
037000021002     C                             DUTNTW <> 'PPT' AND
037100021002     C                             v1cpav =  'T'
037200021002     C                   MOVEL     '1'           werr
037300021002     C                   MOVEL     msg(9)        $msg
037400021002     C                   SETON                                        6090
037500021002     C                   GOTO      finct1
037600021002e   1C                   ENDIF
037700990101     C*
037800990101     C     finct1        ENDSR
037900990101     c*--------------------------------------------------------------------------------------------*
038000990101     c* chkda8 - controlla la data (8)
038100990101     c*--------------------------------------------------------------------------------------------*
038200990101     C     chkda8        BEGSR
038300990101     C*
038400990101     C                   Z-ADD     wda8          g08dat
038500990101     C                   Z-ADD     *zeros        g08inv
038600990101     C                   MOVEL     '0'           g08err
038700990101     C                   CALL      'XSRDA8'
038800990101     C                   PARM                    wlbda8
038900990101     C                   Z-ADD     g08dat        wda8
039000990101IF  1C                   IF        g08err='1'
039100990101     C                   MOVEL     '1'           werr
039200990101X   1C                   ELSE
039300990101     C                   Z-ADD     g08inv        da8w
039400990101E   1C                   ENDIF
039500990101     C*
039600990101     C                   ENDSR
039700990101     c*--------------------------------------------------------------------------------------------*
039800990101     c* chkksc - controlla il cliente
039900990101     c*--------------------------------------------------------------------------------------------*
040000990101     c     chkksc        BEGSR
040100990101     c*
040200020919     c                   Z-ADD     1             kcokut
040300020919     c                   Z-ADD     DUTKCI        kcokcc
040400990101     c                   Z-ADD     wksc          kcoksc
040500990101     c     keyaco        CHAIN     cnaco00f                           99
040600990101if  1c                   IF        *in99                                        *inesistente
040700990101     c                   MOVEL     '1'           werr
040800990101X   1c                   ELSE                                                   *esistente
040900990101     c                   MOVEL     acorag        wdksc                          *descrizione
041000990101e   1c                   ENDIF
041100990101     c*
041200990101     c                   ENDSR
041300990101     c*--------------------------------------------------------------------------------------------*
041400990101     c* chklnp - controlla la filiale
041500990101     c*--------------------------------------------------------------------------------------------*
041600990101     c     chklnp        BEGSR
041700990101     c*
041800990101     c                   Z-ADD     wlnp          krgfil
041900990101     c     keyorg        CHAIN     azorg01l                           99
042000990101if  1c                   IF        *in99                                        *inesistente
042100990101     c                   MOVEL     '1'           werr
042200990101X   1c                   ELSE                                                   *esistente
042300990101     c                   MOVEL     orgdes        wdlnp                          *descrizione
042400990101e   1c                   ENDIF
042500990101     c*
042600990101     c                   ENDSR
042700990101     c*--------------------------------------------------------------------------------------------*
042800990101     C* f03ges - gestione tasto funzionale f03 ==> Fine
042900990101     c*--------------------------------------------------------------------------------------------*
043000990101     C     f03ges        BEGSR
043100990101     C*
043200990101     C                   MOVEL     'S'           $fine                          *fine programma
043300990101     C*
043400990101     C                   ENDSR
043500990101     c*--------------------------------------------------------------------------------------------*
043600990101     C* f06ges - gestione tasto funzionale f06 ==> Conferma
043700990101     c*--------------------------------------------------------------------------------------------*
043800990101     C     f06ges        BEGSR
043900990101     c*
044000990101     c* imposta la ds parametri lancio
044100990101     c                   MOVEL     v1cpav        p75pav                         *tipo bolla
044200990101     c                   Z-ADD     ds1v1c        p75ds1                         *data iniziale
044300990101     c                   Z-ADD     ds2v1c        p75ds2                         *data finale
044400990101     c                   Z-ADD     v1cksc        p75ksc                         *cliente
044500990101     c                   Z-ADD     v1clnp        p75lnp                         *cliente
044600990730     c                   MOVEL     v1cfns        p75fns                         *bolle con serie
044700000627     c                   MOVEL     v1cfel        p75fel                         *errori di linea
044800000731     c                   Z-ADD     v1cnrs        p75nrs                         *serie
044900011126     c                   MOVEL     v1ctot        p75tot                         *solo totali
045000020621     c                   MOVEL     v1cbpt        p75bpt                         *anche bolle PT
045100990101     c                   MOVEL     dspar75       kpjbu
045200001024     c                   MOVEL     'SI76'        kcoaz
045300040702     c                   CALL      'BCH10'
045400040702 MOM c*                  CALL      'TISI76C'
045500990101     c                   PARM                    kpjba
045600990101     C                   MOVEL     '1'           $tesvd                          *riemettere testata
045700000626     C                   MOVEL     'S'           $fine                          *fine programma
045800990101     c*
045900990101     C                   ENDSR
046000990101     c*--------------------------------------------------------------------------------------------*
046100990101     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
046200990101     c*--------------------------------------------------------------------------------------------*
046300990101     C     f12ges        BEGSR
046400990101     C*
046500990101     C                   MOVEL     'S'           $fine                          *fine programma
046600990101     C*
046700990101     C                   ENDSR
046800990101     c*--------------------------------------------------------------------------------------------*
046900990101     C* impvd0 - Impostazioni testata
047000990101     c*--------------------------------------------------------------------------------------------*
047100990101     C     impvd0        BEGSR
047200990101     C*
047300990101     C                   MOVEL     '1'           $tesvd                         *riemettere testata
047400990101     C*
047500990101     C                   ENDSR
047600990101     c*--------------------------------------------------------------------------------------------*
047700990101     C* cartab - carica le tabelle occorrenti
047800990101     c*--------------------------------------------------------------------------------------------*
047900990101     C     cartab        BEGSR
048000990101     c*
048100990101     C                   clear                   ds5a
048200020919     C                   Z-ADD     1             ktbkut
048300990101     C                   MOVEL     '5A'          ktbcod
048400000622     C                   MOVEL(P)  '1'           ktbkey
048500000622     C     keytab        CHAIN     tabel00f                           99
048600990101if  1C                   IF        NOT *in99 AND
048700990101     C                             tblflg=*blanks
048800990101     C                   MOVEL     tbluni        ds5a
048900990101e   1C                   ENDIF
049000990101     C*
049100990101     C                   ENDSR
049200990101     c*--------------------------------------------------------------------------------------------*
049300990101     C* impdat - imposta le date di lavoro
049400990101     c*--------------------------------------------------------------------------------------------*
049500990101     C     impdat        BEGSR
049600990101     C*
049700990101     C* data corrente
049800990101     C                   TIME                    wn14                           *ora(6) + data(8)
049900990101     C                   MOVE      wn14          wn8                            *data(8)
050000990101     C                   Z-ADD     wn8           cordat                         *data corrente g/m/a
050100990101     C                   Z-ADD     wn8           g08dat
050200990101     C                   Z-ADD     *zeros        g08inv
050300990101     C                   MOVEL     *blanks       g08err
050400990101     C                   CALL      'XSRDA8'
050500990101     C                   PARM                    wlbda8
050600990101     C                   Z-ADD     g08inv        datcor                         *data corrente a/m/g
050700990101     c*
050800990101     c* reperisce la data iniziale e finale minima/massima per elaborare le bolle partenza
050900990101     C                   MOVEL     'FNLS61R'     §programma
051000990101     C                   Z-ADD     §5ablp        §giorni
051100990101     c                   EXSR      repdat
051200990101     c                   Z-ADD     §datini       pdatini                        *date in aa/m/g
051300990101     c                   Z-ADD     §datfin       pdatfin
051400990101     c                   Z-ADD     §inidat       pinidat                        *date in g/m/aa
051500990101     c                   Z-ADD     §findat       pfindat
051600990101     c*
051700990101     c* reperisce la data iniziale e finale minima/massima per elaborare le bolle partenza
051800990101     C                   MOVEL     'FNLR84R'     §programma
051900990101     C                   Z-ADD     §5aarb        §giorni
052000990101     c                   EXSR      repdat
052100990101     c                   Z-ADD     §datini       adatini                        *date in aa/m/g
052200990101     c                   Z-ADD     §datfin       adatfin
052300990101     c                   Z-ADD     §inidat       ainidat                        *date in g/m/aa
052400990101     c                   Z-ADD     §findat       afindat
052500990101     c*
052600990101     C                   ENDSR
052700990101     c*--------------------------------------------------------------------------------------------*
052800990101     C* repdat - reperisce le date
052900990101     c*--------------------------------------------------------------------------------------------*
053000990101     C     repdat        BEGSR
053100990101     C*
053200990101     C* azzera le variabili di lavoro
053300990101     C                   Z-ADD     *zeros        §datini                        *date in aa/m/g
053400990101     C                   Z-ADD     *zeros        §datfin
053500990101     C                   Z-ADD     *zeros        §inidat                        *date in g/m/aa
053600990101     C                   Z-ADD     *zeros        §findat
053700990101     C*
053800990101     C* data inizio = data ultimo utilizzo pgm pulizia - gg mantenimento bolle
053900990101     C                   MOVEL     §programma    p94pgm
054000990101     C                   MOVEL     *blanks       p94dat
054100990101     C                   MOVEL     *blanks       p94err
054200990101     C                   CALL      'TRUL49C'                            99
054300990101     C                   PARM                    p94pgm
054400990101     C                   PARM                    p94dat
054500990101     C                   PARM                    p94err
054600990101if  1C                   IF        p94err='1' OR *in99                          *errore
054700990101     C                   Z-ADD     datcor        g08inv                           -data corrente
054800990101x   1C                   else                                                    *no errore
054900990101if  2C                   IF        p94dat=*blanks                                 -pgm mai usato
055000990101     C                   Z-ADD     *zeros        g08inv                            -data a zero
055100990101x   2C                   ELSE                                                     -pgm usato
055200000103     C                   MOVEL     p94dat        g08inv                            -ultimo utilizzo
055300990101e   2C                   ENDIF
055400990101e   1C                   ENDIF
055500990101if  1C                   IF        g08inv <> *zeros
055600990101     C                   Z-ADD     *zeros        g08dat
055700990101     C                   Z-ADD     *zeros        g08tgi
055800990101     C                   MOVEL     '3'           g08err
055900990101     C                   CALL      'XSRDA8'
056000990101     C                   PARM                    wlbda8                         *CALCOLA GG DATA
056100990101     C                   SUB       §giorni       g08tgi                         *MENO GG MANTENIMENT
056200990101     C                   Z-ADD     *zeros        giodat
056300990101     C                   Z-ADD     *zeros        gioinv
056400990101     C                   Z-ADD     g08tgi        giotgi
056500990101     C                   CALL      'XSRGI8'
056600990101     C                   PARM                    wgidat
056700990101     C                   Z-ADD     gioinv        §datini                        *data iniziale
056800990101e   1C                   ENDIF
056900990101     C*
057000990101     C* data finale = data corrente
057100990101     C                   Z-ADD     datcor        §datfin                        *data finale
057200990101     C*
057300990101     C* Inverte le date in gg/mm/anno
057400990101     C                   Z-ADD     §datini       g08inv
057500990101     C                   Z-ADD     *zeros        g08dat
057600990101     C                   MOVEL     '3'           g08err
057700990101     C                   CALL      'XSRDA8'
057800990101     C                   PARM                    wlbda8
057900990101     C                   Z-ADD     g08dat        §inidat                        *data iniziale G/M/A
058000990101     C                   Z-ADD     §datfin       g08inv
058100990101     C                   Z-ADD     *zeros        g08dat
058200990101     C                   MOVEL     '3'           g08err
058300990101     C                   CALL      'XSRDA8'
058400990101     C                   PARM                    wlbda8
058500990101     C                   Z-ADD     g08dat        §findat                        *data finale G/M/AA
058600990101     C*
058700990101     C                   ENDSR
058800020919     C*--------------------------------------------------------------------------------------------*
058900020919     C* REPERISCE I DATI UTENTE
059000020919     C*--------------------------------------------------------------------------------------------*
059100020919     C     REPDATIUTE    BEGSR
059200020919     C*
059300020919     C* INIZIALIZZA VARIABILI DI WRK
059400020919     C                   CLEAR                   TIBS34DS
059500020919     C                   CLEAR                   AZUTEDS
059600020919     C                   CLEAR                   DDATIUTE
059700020919     C*
059800020919     C     *DTAARA       DEFINE    §azute        azuteds
059900020919     C     *DTAARA       DEFINE    §datiute      ddatiute
060000020919     C                   IN(E)     *DTAARA
060100020919if  1C                   IF        %Error
060200020919     c                   EVAL      I34Tla = 'L'
060300020919     C                   CALL      'TIBS34R'
060400020919     C                   PARM                    Tibs34Ds
060500020919     C                   IN        *DTAARA
060600020919e   1C                   ENDIF
060700020919     C*
060800020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
060900020919     C                   MOVEL(P)  RSUT          DSFIRS           20
061000020919     C*
061100020919     C                   ENDSR
061200990101     c*--------------------------------------------------------------------------------------------*
061300990101     C* *inzsr - operazioni iniziali
061400990101     c*--------------------------------------------------------------------------------------------*
061500990101     C     *inzsr        BEGSR
061600990101     c*
061700990101     c* ricevimento parametri
061800990101     c     *ENTRY        PLIST
061900990101     c                   PARM                    kpjba
062000990101     C*
062100990101     C* chiavi di lettura
062200000622     c     keytab        KLIST                                                  *tabel00f
062300000622     c                   KFLD                    ktbkut                          -utente
062400000622     c                   KFLD                    ktbcod                          -tabella
062500000622     c                   KFLD                    ktbkey                          -chiave
062600990101     c     ke2tab        KLIST                                                  *tabel00f
062700990101     c                   KFLD                    ktbkut                          -utente
062800990101     c                   KFLD                    ktbcod                          -tabella
062900990101     c     keyaco        KLIST                                                  *cnaco00f
063000990101     c                   KFLD                    kcokut                          -utente
063100990101     c                   KFLD                    kcokcc                          -capoconto
063200990101     c                   KFLD                    kcoksc                          -sottoconto
063300990101     c     keyorg        KLIST                                                  *azorg00f
063400990101     c                   KFLD                    krgfil                          -filiale
063500990101     C*
063600990101     C* reperimento dati utente
063700020919     C                   EXSR      REPDATIUTE
063800990101     C*
063900990101     C* carica le tabelle occorrenti
064000990101     C                   EXSR      cartab
064100990101     C*
064200990101     C* imposta le date di lavoro
064300990101     c                   EXSR      impdat
064400990101     C*
064500990101     C                   ENDSR
064600990101     O*--------------------------------------------------------------------------------------------*
064700990101** -msg-
064800990101TISI75R- Immettere uno dei valori possibili                                   1
064900990101TISI75R- Data errata                                                          2
065000990101TISI75R- La data finale deve essere maggiore o uguale alla data iniziale      3
065100000731TISI75R- INFO: La data DAL è inferiore alla data ultima di esistenza bolle    4
065200990101TISI75R- La data AL non può essere maggiore della data corrente               5
065300990101TISI75R- Cliente errato o non in gestione                                     6
065400990101TISI75R- Filiale errata                                                       7
065500011120TISI75R- In filiale è impossibile verificare le bolle di sede.                8
065600020927TISI75R- Non consentito gestire bolle POSTE.                                  9
065700990101TISI75R- 10
