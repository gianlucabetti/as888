000100971216     h*--------------------------------------------------------------------------------------------*
000200971216     h* Lettura anagrafiche cliente - Esegue
000300971216     h*--------------------------------------------------------------------------------------------*
000400020522     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500971216     h DECEDIT('0,') DATEDIT(*DMY.)
000600971216     f*--------------------------------------------------------------------------------------------*
000700971216     f* Data base
000800971216     f*--------------------------------------------------------------------------------------------*
000900971216     f*---
001000971216     f* del sistema informativo corrente
001100971216     f*---
001200020522     fcnaco00f  IF   E           K DISK    USROPN                               *Base
001300020522     fcnind00f  IF   E           K DISK    USROPN                               *Indirizzo
001400020522     fcnclp00f  IF   E           K DISK    USROPN                               *Particolarità uno
001500020522     ffncls01l  IF   E           K DISK    USROPN                               *Particolarità due
001600971216     d*--------------------------------------------------------------------------------------------*
001700971216     d* Data structure
001800971216     d*--------------------------------------------------------------------------------------------*
001900971216     d*----------------
002000971216     d* Schiere
002100971216     d*----------------
002200971218     d msg             S             78    DIM(10) CTDATA PERRCD(1)             *messaggi
002300980113     d ri              S              4  0 DIM(4)                               *ricerca capiconto
002400971216     d*----------------
002500971216     d* Variabili riferite al data base
002600971216     d*----------------
002700971216     d kclkut          S                   LIKE(acokut)                         *per cnaco/ind/clp
002800971216     d kclkcc          S                   LIKE(acokcc)
002900971216     d kclksc          S                   LIKE(acoksc)
003000971216     d kcsksc          S                   LIKE(clsksc)                         *per fncls
003100971216     d*----------------
003200971216     d* Variabili di controllo
003300971216     d*----------------
003400971218     d $err            S              1                                         *errore
003500980317     d $calaltaco      S              1                                         *calcola lib.altern.
003600980317     d $calaltind      S              1                                         *calcola lib.altern.
003700980317     d $calaltclp      S              1                                         *calcola lib.altern.
003800980317     d $calaltcls      S              1                                         *calcola lib.altern.
003900971218     d $opnaco         S              1                                         apertura
004000971218     d $opnind         S              1                                         apertura
004100971218     d $opnclp         S              1                                         apertura
004200971218     d $opncls         S              1                                         apertura
004300971216     d*----------------
004400971216     d* Variabili di programma
004500971216     d*----------------
004600980113     d kcc             S              4  0                                      *capoconto da usare
004700980113     d x2              S              2  0                                      *ricerca capiconto
004800980113     d z               S              3  0                                      *ricerca capiconto
004900980113     d kccci           S              4  0                                      *ricerca capiconto
005000980113     d kcccl           S              4  0                                      *ricerca capiconto
005100980113     d kccfo           S              4  0                                      *ricerca capiconto
005200971216     d*----------------
005300971216     d* Ds
005400980113     D*------------------
005500980113     D* DS "XPARUT" - RICERCA PARAMETRI UTENTE
005600980113     D*------------------
005700980113     d carcnt          DS
005800980113     d  fl12cn                 1      2
005900980113     d  fl34cn                 3      4
006000980113     d  tipcnt                 5      6
006100980113     d parut         E DS                  EXTNAME(utedse0f)                    *ricerca capiconto
006200980113     d  flu                   66     97
006300980113     d                                     DIM(32)
006400980113     d  tcu                  398    697
006500980113     d                                     DIM(50)
006600980113     d  kcu                  698    847P 0
006700980113     d                                     DIM(50)
006800980113     d                                     PACKEVEN
006900980113     d tcuds           DS                                                       *ricerca capiconto
007000980113     d  e1                     1      1
007100980113     d  e3                     3      3
007200980113     d  e2                     2      2
007300980113     d  e4                     4      4
007400980113     d  e56                    5      6
007500971217     d bs69ds        E DS                  EXTNAME(tibs69ds)                    *ds di procedura
007600971217     d cnacods       E DS                  EXTNAME(cnaco00f)                    *record cnaco
007700971217     d cnindds       E DS                  EXTNAME(cnind00f)                    *record cnaco
007800971217     d cnclpds       E DS                  EXTNAME(cnclp00f)                    *record cnaco
007900971217     d fnclsds       E DS                  EXTNAME(fncls00f)                    *record cnaco
008000971216     c*--------------------------------------------------------------------------------------------*
008100971216     c* Main lines
008200971216     c*--------------------------------------------------------------------------------------------*
008300971216     C*
008400971216     C* operazioni iniziali
008500971216if  1c                   IF        i69tla <> 'C'                                *per elaborare
008600971216     C                   EXSR      rutinz
008700971216     C*
008800971216     C* lettura anagrafica di base
008900971217if  2c                   IF        $err = 'N'                                   *no errore
009000971217if  3c                   IF        i69kac > *zeros                              *se richiesto
009100971216     C                   EXSR      letaco
009200971217e   3c                   ENDIF
009300971217e   2c                   ENDIF
009400971216     C*
009500971216     C* lettura anagrafica indirizzi
009600971217if  2c                   IF        $err = 'N'                                   *no errore
009700971217if  3c                   IF        i69kin > *zeros                              *se richiesto
009800971216     C                   EXSR      letind
009900971217e   3c                   ENDIF
010000971217e   2c                   ENDIF
010100971216     C*
010200971216     C* lettura anagrafica particolarità uno
010300971217if  2c                   IF        $err = 'N'                                   *no errore
010400971217if  3c                   IF        i69kcp > *zeros                              *se richiesto
010500971216     C                   EXSR      letclp
010600971217e   3c                   ENDIF
010700971217e   2c                   ENDIF
010800971216     C*
010900971216     C* lettura anagrafica particolarità due
011000971217if  2c                   IF        $err = 'N'                                   *no errore
011100971217if  3c                   IF        i69kcs > *zeros                              *se richiesto
011200971216     C                   EXSR      letcls
011300971217e   3c                   ENDIF
011400971217e   2c                   ENDIF
011500971216e   1c                   ENDIF
011600971216     c*
011700971216     C* fine pgm
011800971218     c                   EXSR      finsr                                        *operazioni finali
011900971218if  1C                   IF        i69tla = *blanks                             R lascia file aperti
012000971216     C                   SETON                                        rt
012100971218x   1C                   ELSE                                                   C/L chiude i file
012200971216     C                   SETON                                        lr
012300971217e   1C                   ENDIF
012400971216     c*--------------------------------------------------------------------------------------------*
012500971216     C* letaco - lettura anagrafica di base
012600971216     c*--------------------------------------------------------------------------------------------*
012700971216     C     letaco        BEGSR
012800971218     C*
012900971218     C* la prima volta, apre il file
013000020521if  1c                   IF        $opnaco = *blanks                            *mai aperto
013100020521     c                   MOVEL     '1'           $opnaco                        *già aperto
013200020521     c                   OPEN      cnaco00f
013300020521e   1c                   ENDIF
013400971216     C*
013500971217     C* cerca il file nella libreria corrente
013600971216     c                   Z-ADD     1             kclkut                         *utente
013700980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
013800971216     c                   Z-ADD     i69kac        kclksc                         *codice
013900971217     c     keycli        CHAIN     cnaco00f                           99
014000971217if  1c                   IF        NOT *in99                                    * trovato
014100971217     C*
014200000105     C* non trovato, cerca nella libreria alternativa se impostata
014300971217x   1c                   ELSE                                                   *non trovato
014400971217     C*
014500971217     C* apre il file nella libreria alternativa
014600000105     c                   MOVEL     'S'           $err
014700000105     c                   MOVEL     '1'           o69err                         *errore
014800000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
014900000105e   1c                   ENDIF
015000971216     c*
015100971216     c                   ENDSR
015200971217     c*--------------------------------------------------------------------------------------------*
015300971217     C* letind - lettura anagrafica indirizzi
015400971217     c*--------------------------------------------------------------------------------------------*
015500971217     C     letind        BEGSR
015600971218     C*
015700971218     C* se non è già stato fatto, apre il file nella libreria corrente
015800020521if  1c                   IF        $opnind = *blanks                            *mai aperto
015900020521     c                   MOVEL     '1'           $opnind                        *già aperto
016000020521     c                   OPEN      cnind00f
016100020521e   1c                   ENDIF
016200971217     C*
016300971217     C* cerca il file nella libreria corrente
016400971217     c                   Z-ADD     1             kclkut                         *utente
016500980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
016600971217     c                   Z-ADD     i69kin        kclksc                         *codice
016700971217     c     keycli        CHAIN     cnind00f                           99
016800971217if  1c                   IF        NOT *in99                                    * trovato
016900000105     C*
017000000105     C* non trovato, cerca nella libreria alternativa se impostata
017100971217x   1c                   ELSE                                                   *non trovato
017200971217     C*
017300971217     C* apre il file nella libreria alternativa
017400000105     c                   MOVEL     'S'           $err
017500000105     c                   MOVEL     '1'           o69err                         *errore
017600000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
017700971217e   1c                   ENDIF
017800971217     c*
017900971217     c                   ENDSR
018000971217     c*--------------------------------------------------------------------------------------------*
018100971217     C* letclp - lettura anagrafica particolarità uno
018200971217     c*--------------------------------------------------------------------------------------------*
018300971217     C     letclp        BEGSR
018400971218     C*
018500971218     C* se non è già stato fatto, apre il file nella libreria corrente
018600020521if  1c                   IF        $opnclp = *blanks                            *mai aperto
018700020521     c                   MOVEL     '1'           $opnclp                        *già aperto
018800020521     c                   OPEN      cnclp00f
018900020521e   1c                   ENDIF
019000971217     C*
019100971217     C* cerca il file nella libreria corrente
019200971217     c                   Z-ADD     1             kclkut                         *utente
019300980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
019400971217     c                   Z-ADD     i69kcp        kclksc                         *codice
019500971217     c     keycli        CHAIN     cnclp00f                           99
019600971217if  1c                   IF        NOT *in99                                    * trovato
019700000105     C*
019800000105     C* non trovato, cerca nella libreria alternativa se impostata
019900971217x   1c                   ELSE                                                   *non trovato
020000971217     C*
020100971217     C* apre il file nella libreria alternativa
020200000105     c                   MOVEL     'S'           $err
020300000105     c                   MOVEL     '1'           o69err                         *errore
020400000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
020500971217e   1c                   ENDIF
020600971217     c*
020700971217     c                   ENDSR
020800971217     c*--------------------------------------------------------------------------------------------*
020900971217     C* letcls - lettura anagrafica particolarità due
021000971217     c*--------------------------------------------------------------------------------------------*
021100971217     C     letcls        BEGSR
021200971218     C*
021300971218     C* se non è già stato fatto, apre il file nella libreria corrente
021400020521if  1c                   IF        $opncls = *blanks                            *mai aperto
021500020521     c                   MOVEL     '1'           $opncls                        *già aperto
021600020521     c                   OPEN      fncls01l
021700020521e   1c                   ENDIF
021800971217     C*
021900971217     C* cerca il file nella libreria corrente
022000971217     c                   Z-ADD     i69kcs        kcsksc                         *codice
022100971217     c     keycls        CHAIN     fncls01l                           99
022200971217if  1c                   IF        NOT *in99                                    * trovato
022300000105     C*
022400000105     C* non trovato, cerca nella libreria alternativa se impostata
022500971217x   1c                   ELSE                                                   *non trovato
022600000105     C*
022700000105     C* apre il file nella libreria alternativa
022800000105     c                   MOVEL     'S'           $err
022900000105     c                   MOVEL     '1'           o69err                         *errore
023000000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
023100971217e   1c                   ENDIF
023200971217     c*
023300971217     c                   ENDSR
023400971218     c*--------------------------------------------------------------------------------------------*
023500971218     C* finsr - operazioni finali
023600971218     c*--------------------------------------------------------------------------------------------*
023700971218     C     finsr         BEGSR
023800971218     c*
023900971218     c* chiude le sostituzioni e i file aperti
024000971218if  1C                   IF        i69tla <> *blanks                            *se da chiudere
024100971218if  2c                   IF        $opnaco <> *blanks                           *aperto
024200971218     c                   CLOSE     cnaco00f
024300971218e   2c                   ENDIF
024400971218     c*
024500971218if  2c                   IF        $opnind <> *blanks                           *aperto
024600971218     c                   CLOSE     cnind00f
024700971218e   2c                   ENDIF
024800971218     c*
024900971218if  2c                   IF        $opnclp <> *blanks                           *aperto
025000971218     c                   CLOSE     cnclp00f
025100971218e   2c                   ENDIF
025200971218     c*
025300971218if  2c                   IF        $opncls <> *blanks                           *aperto
025400971218     c                   CLOSE     fncls01l
025500971218e   2c                   ENDIF
025600971218e   1c                   ENDIF
025700971218     c*
025800971218     c                   ENDSR
025900980113     c*--------------------------------------------------------------------------------------------*
026000980113     C* cartab - carica le tabelle occorrenti
026100980113     c*--------------------------------------------------------------------------------------------*
026200980113     C     cartab        BEGSR
026300980113     c*----------
026400980113     c* Capiconto
026500980113     c*----------
026600980113     c                   Z-ADD     1             codut
026700980113     c                   CALL      'XPARUT'
026800980113     c                   PARM                    parut
026900980113     c                   Z-ADD     1             z
027000980113do  1c                   DO        50            x2
027100980113     c                   MOVE      tcu(x2)       carcnt
027200980113if  2c                   IF        tipcnt='CI'
027300980113     c                   MOVE      kcu(x2)       kccci                          *Contab.industriale
027400980113e   2c                   ENDIF
027500980113if  2c                   IF        fl34cn='01' AND
027600980113     c                             tipcnt='CG'
027700980113     c                   Z-ADD     kcu(x2)       kcccl
027800980113e   2c                   ENDIF                                                  *Clienti italia
027900980113if  2c                   IF        fl34cn='02' AND
028000980113     c                             tipcnt='CG'
028100980113     c                   Z-ADD     kcu(x2)       kccfo
028200980113e   2c                   ENDIF                                                  *Fornitori italia
028300980113if  2c                   IF        tipcnt='RI'
028400980113     c                   Z-ADD     kcu(X2)       ri(z)
028500980113     c                   ADD       1             z                               * ?!?
028600980113e   2c                   ENDIF
028700980113e   1c                   ENDDO
028800980113     c*
028900980113     c                   ENDSR
029000000104     c*--------------------------------------------------------------------------------------------*
029100000104     C* *inzsr - operazioni iniziali -solo la prima volta-
029200000104     c*--------------------------------------------------------------------------------------------*
029300000104     C     *inzsr        BEGSR
029400000104     c*
029500000104     c* ricevimento parametri
029600000104     c     *ENTRY        PLIST
029700000104     c                   PARM                    bs69ds                         *ds di procedura
029800000104     c                   PARM                    cnacods                        *record di cnaco
029900000104     c                   PARM                    cnindds                        *record di cnind
030000000104     c                   PARM                    cnclpds                        *record di cnclp
030100000104     c                   PARM                    fnclsds                        *record di fncls
030200000104     C*
030300000104     C* chiave di lettura
030400000104     C     keycli        KLIST                                                  *di cnaco/ind/clp
030500000104     C                   KFLD                    kclkut                         -utente
030600000104     C                   KFLD                    kclkcc                         -capoconto
030700000104     C                   KFLD                    kclksc                         -codice
030800000104     C     keycls        KLIST                                                  *di fncls
030900000104     C                   KFLD                    kcsksc                         -codice
031000000105     c*
031100000105     c* carica le tabelle occorrenti
031200000105     c                   EXSR      cartab
031300000104     c*
031400000104     c                   ENDSR
031500971216     c*--------------------------------------------------------------------------------------------*
031600971216     C* rutinz - operazioni iniziali
031700971216     c*--------------------------------------------------------------------------------------------*
031800971216     C     rutinz        BEGSR
031900971218     C*
032000971218     C* imposta variabili di controllo
032100971218     c                   MOVEL     'N'           $err                           *errore generico
032200980317     c                   MOVEL     'N'           $calaltaco                     *calcola lib altern.
032300980317     c                   MOVEL     'N'           $calaltind                     *calcola lib altern.
032400980317     c                   MOVEL     'N'           $calaltclp                     *calcola lib altern.
032500980317     c                   MOVEL     'N'           $calaltcls                     *calcola lib altern.
032600971216     C*
032700971217     C* azzera i dati di output
032800971217     c                   CLEAR                   o69err                         *Dati ds
032900971217     c                   CLEAR                   o69msg
033000971218     c                   CLEAR                   o69lib
033100980407if  1c                   IF        i69kac > *zeros                              *richiesto cnaco
033200980407     c                   CLEAR                   cnacods                        *pulisce formato
033300980407e   1c                   ENDIF
033400980407if  1c                   IF        i69kin > *zeros                              *richiesto cnind
033500980407     c                   CLEAR                   cnindds                        *pulisce formato
033600980407e   1c                   ENDIF
033700980407if  1c                   IF        i69kcp > *zeros                              *richiesto cnclp
033800980407     c                   CLEAR                   cnclpds                        *pulisce formato
033900980407e   1c                   ENDIF
034000980407if  1c                   IF        i69kcs > *zeros                              *richiesto fncls
034100980407     c                   CLEAR                   fnclsds                        *pulisce formato
034200980407e   1c                   ENDIF
034300980113     C*
034400980113     C* calcola il capoconto da usare
034500980113if  1c                   IF        i69kcc = *zeros                              *non richiesto
034600980113     c                   Z-ADD     kcccl         kcc                            *capoconto cliente
034700980113x   1c                   ELSE                                                   *richiesto
034800980113     c                   Z-ADD     i69kcc        kcc                            *capoconto x
034900980113e   1c                   ENDIF
035000971218     C*
035100971216     c                   ENDSR
035200971216     o*--------------------------------------------------------------------------------------------*
035300971217** msg - messaggi
035400971219TIBS69R- Libreria alternativa non trovata per CNACO                            1
035500980317TIBS69R- Libreria alternativa non trovata per CNIND                            2
035600980317TIBS69R- Libreria alternativa non trovata per CNCLP                            3
035700980317TIBS69R- Libreria alternativa non trovata per FNCLS                            4
035800980114TIBS69R- Anagrafica non trovata in CNACO                                       5
035900980114TIBS69R- Anagrafica non trovata in CNIND                                       6
036000980114TIBS69R- Anagrafica non trovata in CNCLP                                       7
036100980114TIBS69R- Anagrafica non trovata in FNCLS                                       8
036200971218TIBS69R- 9                                                                     9
036300971218TIBS69R- 10                                                                   10
