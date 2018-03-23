000100970526     h*--------------------------------------------------------------------------------------------*
000200970609     h* Gestione cappario - Richiesta data di riferimento
000300970526     h*--------------------------------------------------------------------------------------------*
000400970526     h DECEDIT('0,') DATEDIT(*DMY.)
000500970526     f*--------------------------------------------------------------------------------------------*
000600970526     f* DATA BASE
000700970526     f*--------------------------------------------------------------------------------------------*
000800970526     ftisi00d   CF   E             WORKSTN
000900970526     fazcpr02l  IF   E           K DISK
001000970526     d*--------------------------------------------------------------------------------------------*
001100970526     d* Data structure
001200970526     d*--------------------------------------------------------------------------------------------*
001300970526     d*----------------
001400970526     d* schiere
001500970526     d*----------------
001600970526     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
001700970526     D*----------------
001800970526     D* Variabili riferite al data base
001900970526     D*----------------
002000970526     D driv1c          S                   LIKE(v1cdri)                         *data video a/m/g
002100970526     D depver          S                   LIKE(cprver)                         *depositi
002200970526     d*----------------
002300970609     d* Variabili di controllo
002400970526     d*----------------
002500970526     d $fine           S              1    INZ('N')                             *Gestione
002600970526     d $tesvd          S              1    INZ('1')                             *riemettere testata
002700970609     d $tipvid         S              1    INZ('1')                             *gestione video
002800970609     d*----------------
002900970609     d* Variabili a programma
003000970609     d*----------------
003100970609     d wecpr           S              1    INZ('N')                             *esistenza versioni
003200970603     d dulver          S              8  0                                      *data ult.vers.a/m/g
003300970526     d datcor          S              8  0                                      *data corrente a/m/g
003400000711     d cordat          S              8  0                                      *data corrente a/m/g
003500970526     d wn8             S              8  0                                      *numerico 8
003600970526     d wn14            S             14  0                                      *numerico 14
003700970526     d werr            S              1    INZ('0')                             *esistenza versioni
003800970526     d wda8            S              8  0                                      *data di 8 in g/m/a
003900970526     d da8w            S              8  0                                      *data di 8 in a/m/g
004000000711     d wgesope         S              3                                         *gestione+operazione
004100000711     d wges            S              1                                         *gestione
004200970526     d wope            S              2                                         *operazione
004300970526     d wpgm            S              8                                         *programma
004400970609     d*----------------
004500970609     d* Ds
004600970609     d*----------------
004700970609     d wlbda8          DS                  INZ                                  *controlla data
004800970609     d  g08dat                        8  0
004900970609     d  g08inv                        8  0
005000970609     d  g08err                        1
005100970609     d  g08tgi                        5  0
005200970609     d dssi00        E DS                  EXTNAME(tisi00ds)                    *ds di procedura
005300970609     d kpjba         E DS                                                       *architettura
005400970526     c*--------------------------------------------------------------------------------------------*
005500970526     c* Main lines
005600970526     c*--------------------------------------------------------------------------------------------*
005700970526     C*
005800970526     C* impostazioni testata
005900970526     C                   EXSR      impvd0
006000970526     C*
006100970526     C* impostazioni prima videata
006200970526     C                   EXSR      impvd1
006300970526     C*
006400970526     C* ciclo fino a richiesta di fine elaborazione
006500970526do  1C                   DOU       $fine='S'
006600970609     C     $tipvid       CASEQ     '1'           gesvd1                         *1° video
006700970526     C                   ENDCS
006800970526e   1C                   ENDDO
006900970526     C*
007000970526     C                   SETON                                        lr
007100970526     c*--------------------------------------------------------------------------------------------*
007200970526     c* impvd1 - impostazioni prima videata
007300970526     c*--------------------------------------------------------------------------------------------*
007400970526     C     impvd1        BEGSR
007500970526     C*
007600970526     C* reimposta formato video e indicatori
007700970526     C                   EXSR      setvd1
007800970609     C                   MOVEL     '1'           $tipvid                        *gestione video
007900000711IF  1C                   IF        wope = '02'                                  *MODIFICA
008000000711     C                   Z-ADD     dulver        v1cdri                         data ultima versione
008100000711X   1C                   ELSE                                                   *NO MODIFICA
008200090420     C                   IF        KCDAZ <> 'SI04'
008300090420     C                   Z-ADD     cordat        v1cdri                         data corrente
008400090420     C                   ENDIF
008500000711E   1C                   ENDIF
008600970526     C*
008700970526     C                   ENDSR
008800970526     c*--------------------------------------------------------------------------------------------*
008900970526     c* setvd1 - impostazioni indicatori prima videata
009000970526     c*--------------------------------------------------------------------------------------------*
009100970526     C     setvd1        BEGSR
009200970526     C*
009300970526     C                   SETOFF                                       20         *errori
009400970526     C                   SETOFF                                       90         *generico di errore
009500970526     C                   MOVEL     *blanks       $msg                            *messaggio
009600970526     C*
009700970526     C                   ENDSR
009800970526     c*--------------------------------------------------------------------------------------------*
009900970526     C* gesvd1 - gestione prima videata
010000970526     c*--------------------------------------------------------------------------------------------*
010100970526     C     gesvd1        BEGSR
010200970526     C*
010300970526     C* emissione videata
010400970526if  1C                   IF        $tesvd='1'
010500970526     C                   WRITE     si00d00                                      *testata
010600970526     C                   MOVEL     '0'           $tesvd
010700970526e   1C                   ENDIF
010800970526     C                   EXFMT     si00d01                                      *video
010900970526     C                   EXSR      setvd1
011000970526     C* F03-Fine
011100970526if  1C                   IF        *inkc
011200970526     C                   EXSR      f03ges
011300970526     C                   GOTO      finvd1
011400970526e   1C                   ENDIF
011500970526     C* F12-Ritorno
011600970526if  1C                   IF        *inkl
011700970526     C                   EXSR      f12ges
011800970526     C                   GOTO      finvd1
011900970526e   1C                   ENDIF
012000970526     C* F10-Ricerca
012100970526if  1C                   IF        *inkj
012200970526     C                   EXSR      f10ges
012300970526     C                   GOTO      finvd1
012400970526e   1C                   ENDIF
012500970526     C* Enter-Controlla
012600970526     C                   EXSR      ctrvd1
012700970526if  1C                   IF        *in90                                        *errori
012800970526     C                   GOTO      finvd1
012900970526x   1c                   ELSE
013000970526     c                   EXSR      entges
013100970526e   1C                   ENDIF
013200970526     C*
013300970526     C     finvd1        ENDSR
013400970526     c*--------------------------------------------------------------------------------------------*
013500970526     C* ctrvd1 - controlla prima videata
013600970526     c*--------------------------------------------------------------------------------------------*
013700970526     C     ctrvd1        BEGSR
013800970526     C*
013900970526     C* reimposta le variabili di lavoro
014000970526     C                   MOVEL     '0'           werr
014100970526     C*---
014200970526     C* data di riferimento
014300970526     C*---
014400970526     C                   Z-ADD     *zeros        driv1c
014500970526     C                   Z-ADD     v1cdri        wda8
014600970526     C                   Z-ADD     *zeros        da8w
014700970526     C                   MOVEL     '0'           werr
014800970526     C                   EXSR      chkda8
014900970526     C                   Z-ADD     wda8          v1cdri
015000970526     C                   Z-ADD     da8w          driv1c
015100970526if  1C                   IF        werr='1'
015200970526     C                   SETON                                        2090
015300970526     C                   MOVEL     msg(1)        $msg
015400970526     C                   GOTO      finct1
015500970526e   1C                   ENDIF
015600970526     c*
015700970526     C* reperisce la versione
015800970526if  1C                   IF        werr='0'                                     *no errori
015900970609     c                   MOVEL     'N'           wecpr                          *esistenza versioni
016000970526     c                   Z-ADD     *zeros        depver
016100970526     c     *loval        SETLL     azcpr02l
016200970526     c                   READ      azcpr02l                               98
016300970526do  2c                   DOW       NOT *in98
016400970526if  3c                   IF        cpratb=*blanks
016500970526if  4c                   IF        driv1c>=cprdde and driv1c<=cprdsc            *cap in decorrenza
016600970526     c                   Z-ADD     cprver        depver                         *versione
016700970609     c                   MOVEL     'S'           wecpr                          *esiste versione
016800970526e   4c                   ENDIF
016900970526e   3c                   ENDIF
017000970526     c                   READ      azcpr02l                               98
017100970526e   2c                   ENDDO                                                  *fine versioni
017200970609if  2c                   IF        wecpr='N'                                    *versione no trovata
017300970526     C                   SETON                                        2090
017400970526     C                   MOVEL     msg(2)        $msg
017500970526     C                   GOTO      finct1
017600970526e   2c                   ENDIF
017700970526e   1C                   ENDIF
017800970526     C*
017900970526     C     finct1        ENDSR
018000970526     c*--------------------------------------------------------------------------------------------*
018100970526     c* chkda8 - controlla la data (8)
018200970526     c*--------------------------------------------------------------------------------------------*
018300970526     C     chkda8        BEGSR
018400970526     C*
018500970526     C                   Z-ADD     wda8          g08dat
018600970526     C                   Z-ADD     *zeros        g08inv
018700970526     C                   MOVEL     '0'           g08err
018800970526     C                   CALL      'XSRDA8'
018900970526     C                   PARM                    wlbda8
019000970526     C                   Z-ADD     g08dat        wda8
019100970526IF  1C                   IF        g08err='1'
019200970526     C                   MOVEL     '1'           werr
019300970526X   1C                   ELSE
019400970526     C                   Z-ADD     g08inv        da8w
019500970526E   1C                   ENDIF
019600970526     C*
019700970526     C                   ENDSR
019800970526     c*--------------------------------------------------------------------------------------------*
019900970526     C* f03ges - gestione tasto funzionale f03 ==> Fine
020000970526     c*--------------------------------------------------------------------------------------------*
020100970526     C     f03ges        BEGSR
020200970526     C*
020300970526     C                   MOVEL     'S'           $fine                          *fine programma
020400970526     C*
020500970526     C                   ENDSR
020600970526     c*--------------------------------------------------------------------------------------------*
020700970526     C* f10ges - gestione tasto funzionale f10 ==> Ricerca versioni
020800970526     c*--------------------------------------------------------------------------------------------*
020900970526     C     f10ges        BEGSR
021000970526     C*
021100970526     C* lancia il programma di gestione versioni in ricerca
021200970526     c                   CLEAR                   dssi00                         *pulisce ds
021300970526     c                   MOVEL     'R01'         d00op0                         *livello procedura
021400970526     c                   MOVEL     *blanks       d00op1                         *livello programma
021500970526     c                   MOVEL     '0'           d00f03                         *premuto F03
021600970526     c                   MOVEL     '0'           d00f12                         *premuto F12
021700970526     c                   MOVEL     '0'           d00err                         *errore
021800970526     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
021900970526     c                   CALL      'TISI46R'                                    *gestione versioni
022000970612     c                   PARM                    kpjba
022100970612     c                   PARM                    dssi00
022200970526     C* ritorno
022300970526if  1c                   IF        d00err='1'
022400970526     C                   SETON                                        2090
022500970526     C                   MOVEL     msg(3)        $msg
022600970526x   1c                   ELSE
022700970526     c                   Z-ADD     d00dde        g08inv
022800970526     c                   Z-ADD     *zeros        g08dat
022900970526     c                   MOVEL     '3'           g08err
023000970526     c                   CALL      'XSRDA8'
023100970526     c                   PARM                    wlbda8
023200970526     c                   Z-ADD     g08dat        v1cdri
023300970526e   1c                   ENDIF
023400970528     c                   MOVEL     '1'           $tesvd
023500970526     C*
023600970526     C                   ENDSR
023700970526     c*--------------------------------------------------------------------------------------------*
023800970526     C* f12ges - gestione tasto funzionale f12 ==> Ritorno
023900970526     c*--------------------------------------------------------------------------------------------*
024000970526     C     f12ges        BEGSR
024100970526     C*
024200970526     C                   MOVEL     'S'           $fine                          *fine programma
024300970526     C*
024400970526     C                   ENDSR
024500970526     c*--------------------------------------------------------------------------------------------*
024600970526     C* entges - gestione tasto funzionale enter ==> Invio
024700970526     c*--------------------------------------------------------------------------------------------*
024800970526     C     entges        BEGSR
024900970526     c*
025000970526     c* imposta la ds di procedura
025100970526     c                   CLEAR                   dssi00
025200970526     c                   MOVEL     kpjbu         d00op0                         *livello procedura
025300970526     c                   MOVEL     *blanks       d00op1                         *livello programma
025400970526     c                   MOVEL     '0'           d00f03                         *premuto F03
025500970526     c                   MOVEL     '0'           d00f12                         *premuto F12
025600970526     c                   MOVEL     '0'           d00err                         *errore
025700970526     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
025800970526     c                   Z-ADD     driv1c        d00dri                         *data riferimento
025900970526     c                   MOVEL     depver        d00ver                         *versione
026000970526     c*
026100970526     c* gestione cappario
026200970526if  1c                   IF        wges='C'
026300970526if  2c                   IF        wope='01' OR wope='02' OR wope='05'          *gestione
026400970526     c                   MOVEL     'TISI02R'     wpgm
026500970526e   2c                   ENDIF
026600970526if  2c                   IF        wope='03'                                    *stampa
026700970526     c                   MOVEL     'TISI04R'     wpgm
026800970526e   2c                   ENDIF
026900000223if  2c                   IF        wope='07'                                    *stampa località
027000000223     c                   MOVEL     'TISI08R'     wpgm
027100000221e   2c                   ENDIF
027200970526e   1c                   ENDIF
027300970526     c*
027400970526     c* gestione eccezioni linee/zone
027500970526if  1c                   IF        wges='E'
027600970526     c                   MOVEL     'TISI11R'     wpgm
027700970526e   1c                   ENDIF
027800970526     c*
027900970526     c* gestione località
028000970526if  1c                   IF        wges='L'
028100970526     c                   MOVEL     'TISI30R'     wpgm
028200970526e   1c                   ENDIF
028300970526     c*
028400970526     c* gestione sinonimi
028500970526if  1c                   IF        wges='S'
028600970526     c                   MOVEL     'TISI35R'     wpgm
028700970526e   1c                   ENDIF
028800970526     c*
028900970526     c* gestione tempi di consegna
029000970526if  1c                   IF        wges='T'
029100970526     c                   MOVEL     'TISI41R'     wpgm
029200970526e   1c                   ENDIF
029300970526     c*
029400970526     c* lancia il programma
029500970526     c                   CALL      wpgm
029600970612     c                   PARM                    kpjba
029700970526     c                   PARM                    dssi00
029800970526     C                   MOVEL     '1'           $tesvd                          *riemettere testata
029900970526     C*
030000980225     C* se ritorna premuto f3, chiude il programma
030100970526IF  1c                   IF        d00f03='1'                                    *premuto f03
030200970526     c                   MOVEL     'S'           $fine                           *fine pgm
030300970526X   1c                   ELSE                                                    *non premuto f03
030400970526     C                   MOVEL     d00msg        $msg                            *messaggio
030500970526e   1c                   ENDIF
030600970526     c*
030700970526     C                   ENDSR
030800970526     c*--------------------------------------------------------------------------------------------*
030900970526     C* impvd0 - Impostazioni testata
031000970526     c*--------------------------------------------------------------------------------------------*
031100970526     C     impvd0        BEGSR
031200970526     C*
031300970526     C                   MOVEL     '1'           $tesvd                         *riemettere testata
031400970526     C*
031500970526     C                   ENDSR
031600970526     c*--------------------------------------------------------------------------------------------*
031700970526     C* *inzsr - operazioni iniziali
031800970526     c*--------------------------------------------------------------------------------------------*
031900970526     C     *inzsr        BEGSR
032000970526     c*
032100970526     c* ricevimento parametri
032200970526     c     *ENTRY        PLIST
032300970526     c                   PARM                    kpjba
032400970526     C*
032500970526     C* reperisce la data corrente
032600970526     C                   TIME                    wn14                           *ora (6)+ data (8)
032700970526     C                   MOVE      wn14          wn8                            *data (8) in g/m/aa
032800000711     C                   Z-ADD     wn8           cordat                         *data in g/m/aa
032900970526     C                   Z-ADD     wn8           g08dat
033000970526     C                   Z-ADD     *zeros        g08inv
033100970526     C                   MOVEL     '0'           g08err
033200970526     C                   CALL      'XSRDA8'
033300970526     C                   PARM                    wlbda8
033400970526     C                   Z-ADD     g08inv        datcor                         *data in aa/m/g
033500970603     C*
033600970603     C* reperisce la data decorrenza dell'ultima versione
033700970603     c     *hival        SETLL     azcpr02l
033800970603     c                   READP     azcpr02l                               99
033900970603do  1c                   DOW       NOT *in99
034000970603if  2c                   IF        cpratb=*blanks
034100970603     c                   Z-ADD     cprdde        dulver
034200970603     c                   LEAVE
034300970603e   2c                   ENDIF
034400970603e   1c                   ENDDO
034500970603     C                   Z-ADD     dulver        g08inv
034600970603     C                   Z-ADD     *zeros        g08dat
034700970603     C                   MOVEL     '3'           g08err
034800970603     C                   CALL      'XSRDA8'
034900970603     C                   PARM                    wlbda8
035000970603     C                   Z-ADD     g08dat        dulver                         *data in g/m/aa
035100970526     C*
035200970526     C* reperisce la procedura di entrata
035300000711     c*---               MOVEL     kpjbu         wges                           *gestione
035400000711     c                   MOVEL     kpjbu         wgesope                        *gestione+operazione
035500000711     c                   MOVEL     wgesope       wges                           *gestione
035600000711     c                   MOVE      wgesope       wope                           *operazione
035700970526     C*
035800970526     C                   ENDSR
035900970526     O*--------------------------------------------------------------------------------------------*
036000970526** -msg-
036100970526TISI00R- Data errata                                                          1
036200970526TISI00R- Non esistono dati nel periodo richiesto                              2
036300970526TISI00R- Errori nella funzione di richiesta versioni. Avvertire il CED        3
036400970526TISI00R- 4
036500970526TISI00R- 5
036600970526TISI00R- 6
036700970526TISI00R- 7
036800970526TISI00R- 8
036900970526TISI00R- 9
037000970526TISI00R- 10
