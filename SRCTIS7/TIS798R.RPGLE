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
001200991102     d*--------------------------------------------------------------------------------------------*
001300991102     d* Data structure
001400991102     d*--------------------------------------------------------------------------------------------*
001500991209     d*---
001600991209     d* Variabili riferite al data base
001700991209     d*---
001800991209     d depksu          S                   LIKE(vssksu)                         *lettura tivss00f
001900991209     d depsun          S                   LIKE(vsssun)
002000991102     d*---
002100991102     d* Variabili non riferite al data base
002200991102     d*---
002300991102     d datcor          S              8  0                                      *data corrente a/m/g
002400991109     d*---
002500991109     d* Variabili di controllo
002600991109     d*---
002700991109     d $fvss           S              1    INZ('N')                             *fine lettura TIVSS
002800991109     d $recok          S              1    INZ('N')                             *validità record
002900021129     d $giro           S              1    INZ('1')                             *contatore x cicli
003000991109     d*---
003100991109     d* Ds
003200991109     d*---
003300991109      * DS Input programma da eseguire
003400991109     d tis799dsI     E DS
003500991109      * architettura
003600991109     d kpjba         E DS                                                       *Architettura
003700020226      * parametri in ingresso
003800020226     d param           DS                  INZ                                  *Parametri di elab.
003900020226     d  parksu                        8                                          -cliente abilitato
004000020226     d  parpod                        1                                          -pari o dispari
004100991102     c*--------------------------------------------------------------------------------------------*
004200991102     c* Main lines
004300991102     c*--------------------------------------------------------------------------------------------*
004400021129     c*
004500091214     c* primo giro x le abilitazioni scadute
004600021129     c                   EXSR      vssscad
004700021129     c*
004800021129     c* secondo giro x le abilitazioni attive
004900021129     c                   EXSR      vssok
005000021129     c*
005100021129     c                   SETON                                            lr
005200021129     c*--------------------------------------------------------------------------------------------*
005300021129     c* prima lettura del TIVSS x le abilitazioni scadute
005400021129     c*--------------------------------------------------------------------------------------------*
005500021129     c     vssscad       BEGSR
005600021129     c*
005700021129     c                   MOVEL     'N'           $fvss
005800021129     c                   MOVEL     '1'           $giro
005900991109     c*
006000991109     c* posizionamento e prima lettura -clienti abilitati-
006100991109     c                   EXSR      setvss
006200991109     c*
006300991109     c* ciclo fino a fine file
006400991109do  1c                   DOW       $fvss='N'
006500991209     c*
006600991209     c* operazioni per nuovo cliente unificante
006700991209     c                   EXSR      newksu
006800991209     c*
006900991209     c* ciclo fino a rottura -cliente unificante-
007000991209do  2c                   DOW       $fvss='N' AND
007100991209     c                             depksu = vssksu
007200991209     c*
007300991209     c* lettura successiva -clienti abilitati-
007400991209     c                   EXSR      letvss
007500991209e   2c                   ENDDO                                                  *rottura unificante
007600991109     c*
007700991109     c* lancia il programma di aggiornamento bolle del cliente unificante abilitato
007800991209     c                   EXSR      elaksu
007900991209e   1c                   ENDDO                                                  *fine file
008000021129     c*
008100021129     c                   ENDSR
008200021129     c*--------------------------------------------------------------------------------------------*
008300021129     c* seconda lettura del TIVSS x le abilitazioni attive
008400021129     c*--------------------------------------------------------------------------------------------*
008500021129     c     vssok         BEGSR
008600021129     c*
008700021129     c                   MOVEL     'N'           $fvss
008800021129     c                   MOVEL     '2'           $giro
008900021129     c*
009000021129     c* posizionamento e prima lettura -clienti abilitati-
009100021129     c                   EXSR      setvss
009200021129     c*
009300021129     c* ciclo fino a fine file
009400021129do  1c                   DOW       $fvss='N'
009500021129     c*
009600021129     c* operazioni per nuovo cliente unificante
009700021129     c                   EXSR      newksu
009800021129     c*
009900021129     c* ciclo fino a rottura -cliente unificante-
010000021129do  2c                   DOW       $fvss='N' AND
010100021129     c                             depksu = vssksu
010200021129     c*
010300021129     c* lettura successiva -clienti abilitati-
010400021129     c                   EXSR      letvss
010500021129e   2c                   ENDDO                                                  *rottura unificante
010600021129     c*
010700021129     c* lancia il programma di aggiornamento bolle del cliente unificante abilitato
010800021129     c                   EXSR      elaksu
010900021129e   1c                   ENDDO                                                  *fine file
011000021129     c*
011100021129     c                   ENDSR
011200991109     c*--------------------------------------------------------------------------------------------*
011300991109     c* posizionamento e prima lettura -clienti abilitati-
011400991109     c*--------------------------------------------------------------------------------------------*
011500991109     c     setvss        BEGSR
011600991109     c*
011700991109     c     *loval        SETLL     tivss02l                           99
011800991109if  1c                   IF        NOT *in99
011900991109     c                   EXSR      letvss                                       *lettura record
012000991109x   1c                   ELSE                                                   *fine file
012100991109     C                   MOVEL     'S'           $fvss                          *fine lettura
012200991109e   1C                   ENDIF
012300991109     C*
012400991109     C                   ENDSR
012500991109     c*--------------------------------------------------------------------------------------------*
012600991109     c* lettura successiva -clienti abilitati-
012700991109     c*--------------------------------------------------------------------------------------------*
012800991109     c     letvss        BEGSR
012900991109     c*
013000991109     c                   MOVEL     'N'           $recok
013100991109do  1c     $fvss         DOWEQ     'N'
013200991109     c     $recok        ANDEQ     'N'
013300991110     c                   READ      tivss02l                               99
013400991109if  2c                   IF        *in99                                        *fine file
013500991109     c                   MOVEL     'S'           $fvss                          *fine lettura
013600991109x   2c                   ELSE
013700991109     c                   EXSR      chkvss                                       *controlla record
013800991109e   2c                   ENDIF
013900991109e   1c                   ENDDO
014000991109     c*
014100991109     c                   ENDSR
014200991109     c*--------------------------------------------------------------------------------------------*
014300991109     c* controlla record -clienti abilitati-
014400991109     c*--------------------------------------------------------------------------------------------*
014500991109     c     chkvss        BEGSR
014600991109     c*
014700991109     c                   MOVEL     'S'           $recok                         *record valido
014800991109     c*
014900991109     c* tipo servizio diverso dal TRACK & TRACING, esclude
015000020131     c                   IF        $recok = 'S'
015100991109if  1c                   IF        vssisv <> 'TT'
015200991109     c                   MOVEL     'N'           $recok                         *record non valido
015300991109e   1c                   ENDIF
015400020131     c                   ENDIF
015500021129     c*
015600021129     c* introduco il test sul cliente eventualmente richiesto
015700021129     c                   IF        $recok = 'S'
015800021129     c                   IF        parksu <> *blanks                            *cliente singolo
015900021129     c                   IF        vssksu <> parksu
016000021129     c                   MOVEL     'N'           $recok                         *record non valido
016100021129     c                   ENDIF
016200021129     c                   ENDIF
016300021129     c                   ENDIF
016400021129     c*
016500021129     c* nel primo giro elaboro solo se l'abilitazione è scaduta
016600021129     c                   IF        $recok = 'S'
016700021129     c                   IF        $giro = '1'
016800021129     c                   IF        vssdde <= datcor AND
016900021129     c                             vssdsc >= datcor
017000021129     c                   MOVEL     'N'           $recok                         *record non valido
017100021129     c                   ENDIF
017200021129     c                   ENDIF
017300021129     c                   ENDIF
017400021129     c*
017500021129     c* nel secondo giro elaboro solo se l'abilitazione è attiva
017600021129     c                   IF        $recok = 'S'
017700021129     c                   IF        $giro = '2'
017800021129     c                   IF        vssdde <= datcor AND
017900021129     c                             vssdsc >= datcor
018000021129     c                   ELSE
018100021129     c                   MOVEL     'N'           $recok                         *record non valido
018200021129     c                   ENDIF
018300021129     c                   ENDIF
018400021129     c                   ENDIF
018500020131     c*
018600021129     c* test su clienti pari in data pari e cliente dispari in data dispari (solo x 2° giro)
018700020131     c                   IF        $recok = 'S'
018800021129     c                   IF        $giro = '2'
018900020504     c                   MOVE(p)   vssksu        wrkCodCli         8 0
019000020226     c                   IF        parpod = *blanks
019100020131     c                   IF        ((datcor/2)-%int(datcor/2)       = 0  AND
019200020131     c                              (wrkCodCli/2)-%int(wrkCodCli/2) = 0) OR
019300020131     c                             ((datcor/2)-%int(datcor/2)       > 0 AND
019400020131     c                              (wrkCodCli/2)-%int(wrkCodCli/2) > 0)
019500020131     c                   ELSE
019600020131     c                   MOVEL     'N'           $recok                         *record non valido
019700020131     c                   ENDIF
019800020226     c                   ELSE
019900020226     c                   IF        parpod = 'P' AND                             *richiesti pari
020000020226     c                             (wrkCodCli/2)-%int(wrkCodCli/2) > 0          *cliente dispari
020100020226     c                   MOVEL     'N'           $recok                         *record non valido
020200020226     c                   ENDIF
020300020226     c                   IF        parpod = 'D' AND                             *richiesti dispari
020400020226     c                             (wrkCodCli/2)-%int(wrkCodCli/2) = 0          *cliente pari
020500020226     c                   MOVEL     'N'           $recok                         *record non valido
020600020226     c                   ENDIF
020700020226     c                   ENDIF
020800020131     c                   ENDIF
020900021129     c                   ENDIF
021000991109     c*
021100991109     c                   ENDSR
021200991209     c*--------------------------------------------------------------------------------------------*
021300991209     c* operazioni per nuovo cliente unificante
021400991209     c*--------------------------------------------------------------------------------------------*
021500991209     c     newksu        BEGSR
021600991209     c*
021700991209     c* memorizza il cliente unificante in un deposito
021800991209     c                   EVAL      depksu = vssksu
021900991209     c*
022000991209     c* memorizza gli alri campi per il lancio
022100991209     c                   EVAL      depsun = vsssun                              *utente strategi
022200991209     c*
022300991209     c                   ENDSR
022400991109     c*--------------------------------------------------------------------------------------------*
022500991209     c* lancia il programma di aggiornamento bolle -cliente unificante-
022600991109     c*--------------------------------------------------------------------------------------------*
022700991209     c     elaksu        BEGSR
022800991109     c*
022900991111     c* Imposta DS di lancio
023000991109     c                   CLEAR                   tis799dsI
023100991213     c                   MOVEL     depsun        suni99                         *cliente strategi
023200991213     c                   MOVEL     depksu        ksui99                         *cliente unificante
023300991109     c                   MOVEL     *blanks       clii99
023400091211     c*
023500091211     c* Effettuo chiamata sincrona
023600091211     c                   CALL(e)   'TIS799R'
023700091211     c                   PARM                    TIS799DSI
023800091211     c*
023900991109     c                   ENDSR
024000991102     c*--------------------------------------------------------------------------------------------*
024100991102     c* operazioni iniziali
024200991102     c*--------------------------------------------------------------------------------------------*
024300991102     c     *inzsr        BEGSR
024400991102     c*
024500991102     c* ricevimento parametri
024600991104     c     *ENTRY        PLIST
024700991109     c                   PARM                    kpjba
024800020226     c                   MOVEL     kpjbu         param
024900991109     c*
025000991109     c* reperimento data corrente
025100091214     c                   eval      datcor = %dec(%date() : *ISO)
025200000212     c*
025300991111     c                   ENDSR
