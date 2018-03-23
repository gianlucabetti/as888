000100991102     h*--------------------------------------------------------------------------------------------*
000200991102     h* Creazione file estratto bolle -TIVTA-                                                      *
000300991102     h*--------------------------------------------------------------------------------------------*
000400991026     h DECEDIT('0,') DATEDIT(*DMY.)
000500991102     f*--------------------------------------------------------------------------------------------*
000600991102     f* Data base
000700991102     f*--------------------------------------------------------------------------------------------*
000800991105     f*---
000900991105     f* Abilitazioni clienti
001000991105     f*---
001100991109     ftivss02l  IF   E           K DISK                                         *Bolle
001200000212     fkplog01l  IF   E           K DISK                                         *Bolle
001300991102     d*--------------------------------------------------------------------------------------------*
001400991102     d* Data structure
001500991102     d*--------------------------------------------------------------------------------------------*
001600991209     d*---
001700991209     d* Variabili riferite al data base
001800991209     d*---
001900991209     d depksu          S                   LIKE(vssksu)                         *lettura tivss00f
002000991209     d depsun          S                   LIKE(vsssun)
002100991102     d*---
002200991102     d* Variabili non riferite al data base
002300991102     d*---
002400991102     d n14             S             14  0                                      *Numerico 14
002500991102     d n8              S              8  0                                      *Numerico 8
002600991102     d datcor          S              8  0                                      *data corrente a/m/g
002700011112     d comman          S            300                                         *comando CLP
002800011112     d lenght          S             15  5 INZ(%len(comman))                    *lunghezza coman CLP
002900991129     d koda            S             10                                         *jobq da forzare
003000991109     d*---
003100991109     d* Variabili di controllo
003200991109     d*---
003300991109     d $fine           S              1    INZ('N')                             *fine programma
003400991109     d $fvss           S              1    INZ('N')                             *fine lettura TIVSS
003500991109     d $recok          S              1    INZ('N')                             *validità record
003600991109     d $err            S              1    INZ('N')                             *errore generico
003700021129     d $giro           S              1    INZ('1')                             *contatore x cicli
003800991109     d*---
003900991109     d* Ds
004000991109     d*---
004100991109      * DS controllo data
004200991109     d wlbda8          DS                  INZ                                  *controlla data
004300991109     d  g08dat                        8  0                                       -data dritta
004400991109     d  g08inv                        8  0                                       -data invertita
004500991109     d  g08err                        1                                          -errore
004600991109     d  g08tgi                        5  0                                       -giorni fra date
004700991109      * DS Input programma da eseguire
004800991109     d tis799dsI     E DS
004900991109      * architettura
005000991109     d kpjba         E DS                                                       *Architettura
005100020226      * parametri in ingresso
005200020226     d param           DS                  INZ                                  *Parametri di elab.
005300020226     d  parksu                        8                                          -cliente abilitato
005400020226     d  parpod                        1                                          -pari o dispari
005500991102     c*--------------------------------------------------------------------------------------------*
005600991102     c* Main lines
005700991102     c*--------------------------------------------------------------------------------------------*
005800021129     c*
005900021129     c* primo giro x le abilitazioni scadute (solo x lancio T=Tutti, ovvero Pari+Dispari)
006000040903     c***                IF        parpod = 'T'
006100021129     c                   EXSR      vssscad
006200040903     c***                ENDIF
006300021129     c*
006400021129     c* secondo giro x le abilitazioni attive
006500021129     c                   EXSR      vssok
006600021129     c*
006700021129     c* lancia la riorganizzazione file -estratto bolle-
006800021129     c*                  EVAL      comman =
006900021129     c*                            'SBMJOB CMD(CALL PGM(TIS799C))'
007000021129     c*                            +' JOB(RGZ_TIVTA) JOBQ('
007100021129     c*                            + KODA
007200021129     c*                            + ')'
007300021129     c*                  CALL      'QCMDEXC'                            97
007400021129     c*                  PARM                    comman
007500021129     c*                  PARM                    lenght
007600021129     c*
007700021129     c                   SETON                                            lr
007800021129     c*--------------------------------------------------------------------------------------------*
007900021129     c* prima lettura del TIVSS x le abilitazioni scadute
008000021129     c*--------------------------------------------------------------------------------------------*
008100021129     c     vssscad       BEGSR
008200021129     c*
008300021129     c                   MOVEL     'N'           $fvss
008400021129     c                   MOVEL     '1'           $giro
008500991109     c*
008600991109     c* posizionamento e prima lettura -clienti abilitati-
008700991109     c                   EXSR      setvss
008800991109     c*
008900991109     c* ciclo fino a fine file
009000991109do  1c                   DOW       $fvss='N'
009100991209     c*
009200991209     c* operazioni per nuovo cliente unificante
009300991209     c                   EXSR      newksu
009400991209     c*
009500991209     c* ciclo fino a rottura -cliente unificante-
009600991209do  2c                   DOW       $fvss='N' AND
009700991209     c                             depksu = vssksu
009800991209     c*
009900991209     c* lettura successiva -clienti abilitati-
010000991209     c                   EXSR      letvss
010100991209e   2c                   ENDDO                                                  *rottura unificante
010200991109     c*
010300991109     c* lancia il programma di aggiornamento bolle del cliente unificante abilitato
010400991209     c                   EXSR      elaksu
010500991209e   1c                   ENDDO                                                  *fine file
010600021129     c*
010700021129     c                   ENDSR
010800021129     c*--------------------------------------------------------------------------------------------*
010900021129     c* seconda lettura del TIVSS x le abilitazioni attive
011000021129     c*--------------------------------------------------------------------------------------------*
011100021129     c     vssok         BEGSR
011200021129     c*
011300021129     c                   MOVEL     'N'           $fvss
011400021129     c                   MOVEL     '2'           $giro
011500021129     c*
011600021129     c* posizionamento e prima lettura -clienti abilitati-
011700021129     c                   EXSR      setvss
011800021129     c*
011900021129     c* ciclo fino a fine file
012000021129do  1c                   DOW       $fvss='N'
012100021129     c*
012200021129     c* operazioni per nuovo cliente unificante
012300021129     c                   EXSR      newksu
012400021129     c*
012500021129     c* ciclo fino a rottura -cliente unificante-
012600021129do  2c                   DOW       $fvss='N' AND
012700021129     c                             depksu = vssksu
012800021129     c*
012900021129     c* lettura successiva -clienti abilitati-
013000021129     c                   EXSR      letvss
013100021129e   2c                   ENDDO                                                  *rottura unificante
013200021129     c*
013300021129     c* lancia il programma di aggiornamento bolle del cliente unificante abilitato
013400021129     c                   EXSR      elaksu
013500021129e   1c                   ENDDO                                                  *fine file
013600021129     c*
013700021129     c                   ENDSR
013800991109     c*--------------------------------------------------------------------------------------------*
013900991109     c* posizionamento e prima lettura -clienti abilitati-
014000991109     c*--------------------------------------------------------------------------------------------*
014100991109     c     setvss        BEGSR
014200991109     c*
014300991109     c     *loval        SETLL     tivss02l                           99
014400991109if  1c                   IF        NOT *in99
014500991109     c                   EXSR      letvss                                       *lettura record
014600991109x   1c                   ELSE                                                   *fine file
014700991109     C                   MOVEL     'S'           $fvss                          *fine lettura
014800991109e   1C                   ENDIF
014900991109     C*
015000991109     C                   ENDSR
015100991109     c*--------------------------------------------------------------------------------------------*
015200991109     c* lettura successiva -clienti abilitati-
015300991109     c*--------------------------------------------------------------------------------------------*
015400991109     c     letvss        BEGSR
015500991109     c*
015600991109     c                   MOVEL     'N'           $recok
015700991109do  1c     $fvss         DOWEQ     'N'
015800991109     c     $recok        ANDEQ     'N'
015900991110     c                   READ      tivss02l                               99
016000991109if  2c                   IF        *in99                                        *fine file
016100991109     c                   MOVEL     'S'           $fvss                          *fine lettura
016200991109x   2c                   ELSE
016300991109     c                   EXSR      chkvss                                       *controlla record
016400991109e   2c                   ENDIF
016500991109e   1c                   ENDDO
016600991109     c*
016700991109     c                   ENDSR
016800991109     c*--------------------------------------------------------------------------------------------*
016900991109     c* controlla record -clienti abilitati-
017000991109     c*--------------------------------------------------------------------------------------------*
017100991109     c     chkvss        BEGSR
017200991109     c*
017300991109     c                   MOVEL     'S'           $recok                         *record valido
017400991109     c*
017500991109     c* tipo servizio diverso dal TRACK & TRACING, esclude
017600020131     c                   IF        $recok = 'S'
017700991109if  1c                   IF        vssisv <> 'TT'
017800991109     c                   MOVEL     'N'           $recok                         *record non valido
017900991109e   1c                   ENDIF
018000020131     c                   ENDIF
018100021129     c*
018200021129     c* introduco il test sul cliente eventualmente richiesto
018300021129     c                   IF        $recok = 'S'
018400021129     c                   IF        parksu <> *blanks                            *cliente singolo
018500021129     c                   IF        vssksu <> parksu
018600021129     c                   MOVEL     'N'           $recok                         *record non valido
018700021129     c                   ENDIF
018800021129     c                   ENDIF
018900021129     c                   ENDIF
019000021129     c*
019100021129     c* nel primo giro elaboro solo se l'abilitazione è scaduta
019200021129     c                   IF        $recok = 'S'
019300021129     c                   IF        $giro = '1'
019400021129     c                   IF        vssdde <= datcor AND
019500021129     c                             vssdsc >= datcor
019600021129     c                   MOVEL     'N'           $recok                         *record non valido
019700021129     c                   ENDIF
019800021129     c                   ENDIF
019900021129     c                   ENDIF
020000021129     c*
020100021129     c* nel secondo giro elaboro solo se l'abilitazione è attiva
020200021129     c                   IF        $recok = 'S'
020300021129     c                   IF        $giro = '2'
020400021129     c                   IF        vssdde <= datcor AND
020500021129     c                             vssdsc >= datcor
020600021129     c                   ELSE
020700021129     c                   MOVEL     'N'           $recok                         *record non valido
020800021129     c                   ENDIF
020900021129     c                   ENDIF
021000021129     c                   ENDIF
021100020131     c*
021200021129     c* test su clienti pari in data pari e cliente dispari in data dispari (solo x 2° giro)
021300020131     c                   IF        $recok = 'S'
021400021129     c                   IF        $giro = '2'
021500020504     c                   MOVE(p)   vssksu        wrkCodCli         8 0
021600020226     c                   IF        parpod = *blanks
021700020131     c                   IF        ((datcor/2)-%int(datcor/2)       = 0  AND
021800020131     c                              (wrkCodCli/2)-%int(wrkCodCli/2) = 0) OR
021900020131     c                             ((datcor/2)-%int(datcor/2)       > 0 AND
022000020131     c                              (wrkCodCli/2)-%int(wrkCodCli/2) > 0)
022100020131     c                   ELSE
022200020131     c                   MOVEL     'N'           $recok                         *record non valido
022300020131     c                   ENDIF
022400020226     c                   ELSE
022500020226     c                   IF        parpod = 'P' AND                             *richiesti pari
022600020226     c                             (wrkCodCli/2)-%int(wrkCodCli/2) > 0          *cliente dispari
022700020226     c                   MOVEL     'N'           $recok                         *record non valido
022800020226     c                   ENDIF
022900020226     c                   IF        parpod = 'D' AND                             *richiesti dispari
023000020226     c                             (wrkCodCli/2)-%int(wrkCodCli/2) = 0          *cliente pari
023100020226     c                   MOVEL     'N'           $recok                         *record non valido
023200020226     c                   ENDIF
023300020226     c                   ENDIF
023400020131     c                   ENDIF
023500021129     c                   ENDIF
023600991109     c*
023700991109     c                   ENDSR
023800991209     c*--------------------------------------------------------------------------------------------*
023900991209     c* operazioni per nuovo cliente unificante
024000991209     c*--------------------------------------------------------------------------------------------*
024100991209     c     newksu        BEGSR
024200991209     c*
024300991209     c* memorizza il cliente unificante in un deposito
024400991209     c                   EVAL      depksu = vssksu
024500991209     c*
024600991209     c* memorizza gli alri campi per il lancio
024700991209     c                   EVAL      depsun = vsssun                              *utente strategi
024800991209     c*
024900991209     c                   ENDSR
025000991109     c*--------------------------------------------------------------------------------------------*
025100991209     c* lancia il programma di aggiornamento bolle -cliente unificante-
025200991109     c*--------------------------------------------------------------------------------------------*
025300991209     c     elaksu        BEGSR
025400991109     c*
025500991111     c* Imposta DS di lancio
025600991109     c                   CLEAR                   tis799dsI
025700991213     c                   MOVEL     depsun        suni99                         *cliente strategi
025800991213     c                   MOVEL     depksu        ksui99                         *cliente unificante
025900991109     c                   MOVEL     *blanks       clii99
026000991111     c*
026100991111     c* lancia il programma
026200091211     c***                EVAL      comman =
026300091211     c***                          'SBMJOB CMD(CALL PGM(TIS799R) PARM('''
026400091211     c***                          + TIS799DSI                                  *DS input
026500091211     c***                          + ''')) JOB(CREA_TIVTA) JOBQ('
026600091211     c***                          + KODA
026700091211     c***                          + ')'
026800091211     c***                CALL      'QCMDEXC'                            97
026900091211     c***                PARM                    comman
027000091211     c***                PARM                    lenght
027100091211     c*
027200091211     c* Effettuo chiamata sincrona
027300091211     c                   CALL(e)   'TIS799R'
027400091211     c                   PARM                    TIS799DSI
027500091211     c*
027600991109     c                   ENDSR
027700991102     c*--------------------------------------------------------------------------------------------*
027800991102     c* operazioni iniziali
027900991102     c*--------------------------------------------------------------------------------------------*
028000991102     c     *inzsr        BEGSR
028100991102     c*
028200991102     c* ricevimento parametri
028300991104     c     *ENTRY        PLIST
028400991109     c                   PARM                    kpjba
028500020226     c                   MOVEL     kpjbu         param
028600991109     c*
028700991109     c* reperimento data corrente
028800991109     C                   TIME                    n14                            *ora (6)+ data (8)
028900991109     C                   MOVE      n14           n8                             *data (8) in g/m/a
029000991109     C                   Z-ADD     n8            g08dat
029100991109     C                   Z-ADD     *zeros        g08inv
029200991109     C                   MOVEL     '0'           g08err
029300991109     c                   CALL      'XSRDA8'
029400991109     c                   PARM                    wlbda8
029500991109     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
029600000212     c*
029700000212     c* reperimento coda di lancio per sottomettere nella stessa coda
029800000212     c     knraz         chain     kplog01l                           54
029900000212     c  n54              movel     logcbi        koda
030000000212if  1c                   IF        koda  = *blank
030100000212     c                   MOVEL     '*JOBD'       koda
030200000212e   1c                   ENDIF
030300000212     c*
030400991111     c                   ENDSR
