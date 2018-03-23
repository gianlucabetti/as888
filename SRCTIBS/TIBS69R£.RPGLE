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
002300000104     d cmd             S             80    DIM(10) CTDATA PERRCD(1)             *comandi clp OVRDBF
002400980113     d ri              S              4  0 DIM(4)                               *ricerca capiconto
002500971216     d*----------------
002600971216     d* Variabili riferite al data base
002700971216     d*----------------
002800971216     d kclkut          S                   LIKE(acokut)                         *per cnaco/ind/clp
002900971216     d kclkcc          S                   LIKE(acokcc)
003000971216     d kclksc          S                   LIKE(acoksc)
003100971216     d kcsksc          S                   LIKE(clsksc)                         *per fncls
003200971216     d*----------------
003300971216     d* Variabili di controllo
003400971216     d*----------------
003500971218     d $err            S              1                                         *errore
003600980317     d $calaltaco      S              1                                         *calcola lib.altern.
003700980317     d $calaltind      S              1                                         *calcola lib.altern.
003800980317     d $calaltclp      S              1                                         *calcola lib.altern.
003900980317     d $calaltcls      S              1                                         *calcola lib.altern.
004000971218     d $opnaco         S              1                                         apertura
004100971218     d $ovraco         S              1                                         apertura alternativa
004200971218     d $opnind         S              1                                         apertura
004300971218     d $ovrind         S              1                                         apertura alternativa
004400971218     d $opnclp         S              1                                         apertura
004500971218     d $ovrclp         S              1                                         apertura alternativa
004600971218     d $opncls         S              1                                         apertura
004700971218     d $ovrcls         S              1                                         apertura alternativa
004800971216     d*----------------
004900971216     d* Variabili di programma
005000971216     d*----------------
005100971217     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
005200971217     D comman          S             80                                         *valore cmd CLP
005300980113     d kcc             S              4  0                                      *capoconto da usare
005400980113     d x2              S              2  0                                      *ricerca capiconto
005500980113     d z               S              3  0                                      *ricerca capiconto
005600980113     d kccci           S              4  0                                      *ricerca capiconto
005700980113     d kcccl           S              4  0                                      *ricerca capiconto
005800980113     d kccfo           S              4  0                                      *ricerca capiconto
005900971216     d*----------------
006000971216     d* Ds
006100971216     d*----------------
006200971217     d                 DS                                                       *comando generico
006300971217     d  dslib                 30     38                                          -libr.alternativa
006400971217     d  dscmd                  1     80                                          -comando
006500980113     D*------------------
006600980113     D* DS "XPARUT" - RICERCA PARAMETRI UTENTE
006700980113     D*------------------
006800980113     d carcnt          DS
006900980113     d  fl12cn                 1      2
007000980113     d  fl34cn                 3      4
007100980113     d  tipcnt                 5      6
007200980113     d parut         E DS                  EXTNAME(utedse0f)                    *ricerca capiconto
007300980113     d  flu                   66     97
007400980113     d                                     DIM(32)
007500980113     d  tcu                  398    697
007600980113     d                                     DIM(50)
007700980113     d  kcu                  698    847P 0
007800980113     d                                     DIM(50)
007900980113     d                                     PACKEVEN
008000980113     d tcuds           DS                                                       *ricerca capiconto
008100980113     d  e1                     1      1
008200980113     d  e3                     3      3
008300980113     d  e2                     2      2
008400980113     d  e4                     4      4
008500980113     d  e56                    5      6
008600971217     d bs69ds        E DS                  EXTNAME(tibs69ds)                    *ds di procedura
008700971217     d cnacods       E DS                  EXTNAME(cnaco00f)                    *record cnaco
008800971217     d cnindds       E DS                  EXTNAME(cnind00f)                    *record cnaco
008900971217     d cnclpds       E DS                  EXTNAME(cnclp00f)                    *record cnaco
009000971217     d fnclsds       E DS                  EXTNAME(fncls00f)                    *record cnaco
009100971216     c*--------------------------------------------------------------------------------------------*
009200971216     c* Main lines
009300971216     c*--------------------------------------------------------------------------------------------*
009400971216     C*
009500971216     C* operazioni iniziali
009600971216if  1c                   IF        i69tla <> 'C'                                *per elaborare
009700971216     C                   EXSR      rutinz
009800971216     C*
009900971216     C* lettura anagrafica di base
010000971217if  2c                   IF        $err = 'N'                                   *no errore
010100971217if  3c                   IF        i69kac > *zeros                              *se richiesto
010200971216     C                   EXSR      letaco
010300971217e   3c                   ENDIF
010400971217e   2c                   ENDIF
010500971216     C*
010600971216     C* lettura anagrafica indirizzi
010700971217if  2c                   IF        $err = 'N'                                   *no errore
010800971217if  3c                   IF        i69kin > *zeros                              *se richiesto
010900971216     C                   EXSR      letind
011000971217e   3c                   ENDIF
011100971217e   2c                   ENDIF
011200971216     C*
011300971216     C* lettura anagrafica particolarità uno
011400971217if  2c                   IF        $err = 'N'                                   *no errore
011500971217if  3c                   IF        i69kcp > *zeros                              *se richiesto
011600971216     C                   EXSR      letclp
011700971217e   3c                   ENDIF
011800971217e   2c                   ENDIF
011900971216     C*
012000971216     C* lettura anagrafica particolarità due
012100971217if  2c                   IF        $err = 'N'                                   *no errore
012200971217if  3c                   IF        i69kcs > *zeros                              *se richiesto
012300971216     C                   EXSR      letcls
012400971217e   3c                   ENDIF
012500971217e   2c                   ENDIF
012600971216e   1c                   ENDIF
012700971216     c*
012800971216     C* fine pgm
012900971218     c                   EXSR      finsr                                        *operazioni finali
013000971218if  1C                   IF        i69tla = *blanks                             R lascia file aperti
013100971216     C                   SETON                                        rt
013200971218x   1C                   ELSE                                                   C/L chiude i file
013300971216     C                   SETON                                        lr
013400971217e   1C                   ENDIF
013500971216     c*--------------------------------------------------------------------------------------------*
013600971216     C* letaco - lettura anagrafica di base
013700971216     c*--------------------------------------------------------------------------------------------*
013800971216     C     letaco        BEGSR
013900971218     C*
014000971218     C* la prima volta, apre il file
014100020521if  1c                   IF        $opnaco = *blanks                            *mai aperto
014200020521     c                   MOVEL     '1'           $opnaco                        *già aperto
014300020521     c                   OPEN      cnaco00f
014400020521e   1c                   ENDIF
014500971216     C*
014600971217     C* cerca il file nella libreria corrente
014700971216     c                   Z-ADD     1             kclkut                         *utente
014800980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
014900971216     c                   Z-ADD     i69kac        kclksc                         *codice
015000971217     c     keycli        CHAIN     cnaco00f                           99
015100971217if  1c                   IF        NOT *in99                                    * trovato
015200971217     C*
015300000105     C* non trovato, cerca nella libreria alternativa se impostata
015400971217x   1c                   ELSE                                                   *non trovato
015500971217     C*
015600971217     C* apre il file nella libreria alternativa
015700000105     c                   MOVEL     'S'           $err
015800000105     c                   MOVEL     '1'           o69err                         *errore
015900000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
016000000105e   1c                   ENDIF
016100971216     c*
016200971216     c                   ENDSR
016300971217     c*--------------------------------------------------------------------------------------------*
016400971217     C* letind - lettura anagrafica indirizzi
016500971217     c*--------------------------------------------------------------------------------------------*
016600971217     C     letind        BEGSR
016700971218     C*
016800971218     C* se non è già stato fatto, apre il file nella libreria corrente
016900020521if  1c                   IF        $opnind = *blanks                            *mai aperto
017000020521     c                   MOVEL     '1'           $opnind                        *già aperto
017100020521     c                   OPEN      cnind00f
017200020521e   1c                   ENDIF
017300971217     C*
017400971217     C* cerca il file nella libreria corrente
017500971217     c                   Z-ADD     1             kclkut                         *utente
017600980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
017700971217     c                   Z-ADD     i69kin        kclksc                         *codice
017800971217     c     keycli        CHAIN     cnind00f                           99
017900971217if  1c                   IF        NOT *in99                                    * trovato
018000000105     C*
018100000105     C* non trovato, cerca nella libreria alternativa se impostata
018200971217x   1c                   ELSE                                                   *non trovato
018300971217     C*
018400971217     C* apre il file nella libreria alternativa
018500000105     c                   MOVEL     'S'           $err
018600000105     c                   MOVEL     '1'           o69err                         *errore
018700000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
018800971217e   1c                   ENDIF
018900971217     c*
019000971217     c                   ENDSR
019100971217     c*--------------------------------------------------------------------------------------------*
019200971217     C* letclp - lettura anagrafica particolarità uno
019300971217     c*--------------------------------------------------------------------------------------------*
019400971217     C     letclp        BEGSR
019500971218     C*
019600971218     C* se non è già stato fatto, apre il file nella libreria corrente
019700020521if  1c                   IF        $opnclp = *blanks                            *mai aperto
019800020521     c                   MOVEL     '1'           $opnclp                        *già aperto
019900020521     c                   OPEN      cnclp00f
020000020521e   1c                   ENDIF
020100971217     C*
020200971217     C* cerca il file nella libreria corrente
020300971217     c                   Z-ADD     1             kclkut                         *utente
020400980113     c                   Z-ADD     kcc           kclkcc                         *capoconto
020500971217     c                   Z-ADD     i69kcp        kclksc                         *codice
020600971217     c     keycli        CHAIN     cnclp00f                           99
020700971217if  1c                   IF        NOT *in99                                    * trovato
020800000105     C*
020900000105     C* non trovato, cerca nella libreria alternativa se impostata
021000971217x   1c                   ELSE                                                   *non trovato
021100971217     C*
021200971217     C* apre il file nella libreria alternativa
021300000105     c                   MOVEL     'S'           $err
021400000105     c                   MOVEL     '1'           o69err                         *errore
021500000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
021600971217e   1c                   ENDIF
021700971217     c*
021800971217     c                   ENDSR
021900971217     c*--------------------------------------------------------------------------------------------*
022000971217     C* letcls - lettura anagrafica particolarità due
022100971217     c*--------------------------------------------------------------------------------------------*
022200971217     C     letcls        BEGSR
022300971218     C*
022400971218     C* se non è già stato fatto, apre il file nella libreria corrente
022500020521if  1c                   IF        $opncls = *blanks                            *mai aperto
022600020521     c                   MOVEL     '1'           $opncls                        *già aperto
022700020521     c                   OPEN      fncls01l
022800020521e   1c                   ENDIF
022900971217     C*
023000971217     C* cerca il file nella libreria corrente
023100971217     c                   Z-ADD     i69kcs        kcsksc                         *codice
023200971217     c     keycls        CHAIN     fncls01l                           99
023300971217if  1c                   IF        NOT *in99                                    * trovato
023400000105     C*
023500000105     C* non trovato, cerca nella libreria alternativa se impostata
023600971217x   1c                   ELSE                                                   *non trovato
023700000105     C*
023800000105     C* apre il file nella libreria alternativa
023900000105     c                   MOVEL     'S'           $err
024000000105     c                   MOVEL     '1'           o69err                         *errore
024100000105     c                   MOVEL     msg(5)        o69msg                         *messaggio
024200971217e   1c                   ENDIF
024300971217     c*
024400971217     c                   ENDSR
024500971218     c*--------------------------------------------------------------------------------------------*
024600971218     C* finsr - operazioni finali
024700971218     c*--------------------------------------------------------------------------------------------*
024800971218     C     finsr         BEGSR
024900971218     c*
025000971218     c* chiude le sostituzioni e i file aperti
025100971218if  1C                   IF        i69tla <> *blanks                            *se da chiudere
025200971218if  2c                   IF        $opnaco <> *blanks                           *aperto
025300971218     c                   CLOSE     cnaco00f
025400971218e   2c                   ENDIF
025500971218     c*
025600971218if  2c                   IF        $opnind <> *blanks                           *aperto
025700971218     c                   CLOSE     cnind00f
025800971218e   2c                   ENDIF
025900971218     c*
026000971218if  2c                   IF        $opnclp <> *blanks                           *aperto
026100971218     c                   CLOSE     cnclp00f
026200971218e   2c                   ENDIF
026300971218     c*
026400971218if  2c                   IF        $opncls <> *blanks                           *aperto
026500971218     c                   CLOSE     fncls01l
026600971218e   2c                   ENDIF
026700971218e   1c                   ENDIF
026800971218     c*
026900971218     c                   ENDSR
027000980113     c*--------------------------------------------------------------------------------------------*
027100980113     C* cartab - carica le tabelle occorrenti
027200980113     c*--------------------------------------------------------------------------------------------*
027300980113     C     cartab        BEGSR
027400980113     c*----------
027500980113     c* Capiconto
027600980113     c*----------
027700980113     c                   Z-ADD     1             codut
027800980113     c                   CALL      'XPARUT'
027900980113     c                   PARM                    parut
028000980113     c                   Z-ADD     1             z
028100980113do  1c                   DO        50            x2
028200980113     c                   MOVE      tcu(x2)       carcnt
028300980113if  2c                   IF        tipcnt='CI'
028400980113     c                   MOVE      kcu(x2)       kccci                          *Contab.industriale
028500980113e   2c                   ENDIF
028600980113if  2c                   IF        fl34cn='01' AND
028700980113     c                             tipcnt='CG'
028800980113     c                   Z-ADD     kcu(x2)       kcccl
028900980113e   2c                   ENDIF                                                  *Clienti italia
029000980113if  2c                   IF        fl34cn='02' AND
029100980113     c                             tipcnt='CG'
029200980113     c                   Z-ADD     kcu(x2)       kccfo
029300980113e   2c                   ENDIF                                                  *Fornitori italia
029400980113if  2c                   IF        tipcnt='RI'
029500980113     c                   Z-ADD     kcu(X2)       ri(z)
029600980113     c                   ADD       1             z                               * ?!?
029700980113e   2c                   ENDIF
029800980113e   1c                   ENDDO
029900980113     c*
030000980113     c                   ENDSR
030100000104     c*--------------------------------------------------------------------------------------------*
030200000104     C* *inzsr - operazioni iniziali -solo la prima volta-
030300000104     c*--------------------------------------------------------------------------------------------*
030400000104     C     *inzsr        BEGSR
030500000104     c*
030600000104     c* ricevimento parametri
030700000104     c     *ENTRY        PLIST
030800000104     c                   PARM                    bs69ds                         *ds di procedura
030900000104     c                   PARM                    cnacods                        *record di cnaco
031000000104     c                   PARM                    cnindds                        *record di cnind
031100000104     c                   PARM                    cnclpds                        *record di cnclp
031200000104     c                   PARM                    fnclsds                        *record di fncls
031300000104     C*
031400000104     C* chiave di lettura
031500000104     C     keycli        KLIST                                                  *di cnaco/ind/clp
031600000104     C                   KFLD                    kclkut                         -utente
031700000104     C                   KFLD                    kclkcc                         -capoconto
031800000104     C                   KFLD                    kclksc                         -codice
031900000104     C     keycls        KLIST                                                  *di fncls
032000000104     C                   KFLD                    kcsksc                         -codice
032100000105     c*
032200000105     c* carica le tabelle occorrenti
032300000105     c                   EXSR      cartab
032400000104     c*
032500000104     c                   ENDSR
032600971216     c*--------------------------------------------------------------------------------------------*
032700971216     C* rutinz - operazioni iniziali
032800971216     c*--------------------------------------------------------------------------------------------*
032900971216     C     rutinz        BEGSR
033000971218     C*
033100971218     C* imposta variabili di controllo
033200971218     c                   MOVEL     'N'           $err                           *errore generico
033300980317     c                   MOVEL     'N'           $calaltaco                     *calcola lib altern.
033400980317     c                   MOVEL     'N'           $calaltind                     *calcola lib altern.
033500980317     c                   MOVEL     'N'           $calaltclp                     *calcola lib altern.
033600980317     c                   MOVEL     'N'           $calaltcls                     *calcola lib altern.
033700971216     C*
033800971217     C* azzera i dati di output
033900971217     c                   CLEAR                   o69err                         *Dati ds
034000971217     c                   CLEAR                   o69msg
034100971218     c                   CLEAR                   o69lib
034200980407if  1c                   IF        i69kac > *zeros                              *richiesto cnaco
034300980407     c                   CLEAR                   cnacods                        *pulisce formato
034400980407e   1c                   ENDIF
034500980407if  1c                   IF        i69kin > *zeros                              *richiesto cnind
034600980407     c                   CLEAR                   cnindds                        *pulisce formato
034700980407e   1c                   ENDIF
034800980407if  1c                   IF        i69kcp > *zeros                              *richiesto cnclp
034900980407     c                   CLEAR                   cnclpds                        *pulisce formato
035000980407e   1c                   ENDIF
035100980407if  1c                   IF        i69kcs > *zeros                              *richiesto fncls
035200980407     c                   CLEAR                   fnclsds                        *pulisce formato
035300980407e   1c                   ENDIF
035400980113     C*
035500980113     C* calcola il capoconto da usare
035600980113if  1c                   IF        i69kcc = *zeros                              *non richiesto
035700980113     c                   Z-ADD     kcccl         kcc                            *capoconto cliente
035800980113x   1c                   ELSE                                                   *richiesto
035900980113     c                   Z-ADD     i69kcc        kcc                            *capoconto x
036000980113e   1c                   ENDIF
036100971218     C*
036200971216     c                   ENDSR
036300971216     o*--------------------------------------------------------------------------------------------*
036400971217** msg - messaggi
036500971219TIBS69R- Libreria alternativa non trovata per CNACO                            1
036600980317TIBS69R- Libreria alternativa non trovata per CNIND                            2
036700980317TIBS69R- Libreria alternativa non trovata per CNCLP                            3
036800980317TIBS69R- Libreria alternativa non trovata per FNCLS                            4
036900980114TIBS69R- Anagrafica non trovata in CNACO                                       5
037000980114TIBS69R- Anagrafica non trovata in CNIND                                       6
037100980114TIBS69R- Anagrafica non trovata in CNCLP                                       7
037200980114TIBS69R- Anagrafica non trovata in FNCLS                                       8
037300971218TIBS69R- 9                                                                     9
037400971218TIBS69R- 10                                                                   10
037500971218** cmd - comandi clp
037600971218OVRDBF FILE(CNACO00F) TOFILE(xxxxxxxxx/CNACO00F)                                         1
037700971218OVRDBF FILE(CNIND00F) TOFILE(xxxxxxxxx/CNIND00F)                                         2
037800971218OVRDBF FILE(CNCLP00F) TOFILE(xxxxxxxxx/CNCLP00F)                                         3
037900971218OVRDBF FILE(FNCLS01L) TOFILE(xxxxxxxxx/FNCLS01L)                                         4
0380009712185
038100000105CHKOBJ OBJ(SDITRA101/CNACO00F) OBJTYPE(*FILE)                                            6
038200000105CHKOBJ OBJ(GAITRA201/CNACO00F) OBJTYPE(*FILE)                                            7
0383000001048
0384000001049
03850000010410
