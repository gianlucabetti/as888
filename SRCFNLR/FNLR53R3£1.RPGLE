000100961119     H DECEDIT('0,') DATEDIT(*DMY.)
000200961119      *---------*----------------------------------------------------*
000300970117      * FNLR53R2*                                                    *
000400961119      *---------*                                                    *
000500970117      *         - GESTIONE CHIUSURA DISTINTA ITALIA -                *
000600970117      *         - IMMISSIONE MOTIVI MANCATA CONSEGNA-                *
000700961119      *--------------------------------------------------------------*
000800970117     FFNLR53D3  CF   E             WORKSTN
000900970117     F                                     SFILE(LR53S01:NRR)
001000970117     F*---------
001100970117     FTABEL00F  IF   E           K DISK
001200961120     F*---------
001300970121     FFNAR401L  IF   E           K DISK    USROPN
001400970121     F                                     COMMIT
001500970117      *----------------------------------------------------------------
001600970117      *  S C H I E R E
001700970117      *----------------------------------------------------------------
001800970529     D T7K             S              2    DIM(50)                              TRC
001900970211     D TRC             S              1    DIM(100)                             TRC X NO CONS
002000970123     D C2A             S              3    DIM(100)                             SKI CHIAVI 2A
002100970123     D D2A             S             89    DIM(100)                             SKI TAB. 2A
002200970123     D ERR             S             70    DIM(7) CTDATA PERRCD(1)
002300961119     D*--------------------------------------------------------------*
002400970117     D*  D S
002500961119     D*--------------------------------------------------------------*
002600961119     D KPJBA         E DS
002700970117     D FNLR53        E DS                  EXTNAME(FNLR53DS3)
002800970117     D DS7K          E DS
002900970123     D DS2A          E DS
003000970117     D DS7R          E DS
003100970120     D  TMC                   47     51
003200970117     D                                     DIM(5)                               TIP.NO CONSE.
003300970117     D  VOL                   52     56
003400970117     D                                     DIM(5)                               VOLTE
003500961119     C*--------------------------------------------------------------*
003600961119     C*  C I C L O        P R I N C I P A L E
003700961119     C*--------------------------------------------------------------*
003800970117     C* Reperisco codice particolarità consegna
003900970117     C                   EXSR      REPGMA
004000970117     C* Eseguo controlli
004100970117     C                   EXSR      CTRGMA
004200970117     C* Se non ci sono errori gestisco videata
004300970117     C     D53ERR        IFNE      '1'
004400970121     C* Se devo visualizzare dati eseguo caricamento ed emissione
004500970121     C* subfile
004600970121     C     D53VIS        IFEQ      'S'
004700970117     C                   EXSR      CARS01                                       Carico dati
004800970121     C*     Gestione note
004900970117     C     WFINE         DOWNE     'S'
005000970117     C                   EXSR      GESS01
005100970117     C                   END
005200970121     C*     Se no F12 aggiorno
005300970117     C     *INKL         IFEQ      '0'
005400970117     C                   EXSR      AGGIO
005500970121     C                   ELSE
005600970121     C                   MOVEL     '1'           D53F12
005700970117     C                   END
005800970121     C* ... altrimenti imposto decodifica 1a riga
005900970121     C                   ELSE
006000970121     C                   EXSR      DECCMC
006100970121     C                   END
006200970121     C                   END
006300970121     C*
006400970121     C*
006500961217     C     FINE          TAG
006600970123     C                   MOVEL     FNLR53        KPJBU
006700961121     C                   SETON                                        LR
006800970117     C*-----------------------------------------------------*
006900970117     C*  REPGMA: Reperisco particolarità consegna
007000970117     C*-----------------------------------------------------*
007100970117     C     REPGMA        BEGSR
007200970117     C*
007300970117     C* Carico dati tabella 7R
007400970123     C     D53GMA        IFEQ      *BLANKS
007500970123     C                   MOVEL     '1'           D53ERR
007600970123     C                   MOVEL     ERR(7)        D53MSG
007700970123     C                   GOTO      FINE
007800970123     C                   ELSE
007900970117     C                   Z-ADD     1             KKUT
008000970117     C                   MOVEL     '7R'          KCOD
008100970117     C                   MOVEL     *BLANKS       KKEY
008200970117     C                   MOVEL     D53GMA        KKEY
008300970117     C     KTAB1         CHAIN     TABEL00F                           31
008400970117     C     *IN31         IFEQ      '1'
008500970117     C                   MOVEL     '1'           D53ERR
008600970117     C                   MOVEL     ERR(1)        D53MSG
008700970117     C                   GOTO      FINE
008800970117     C                   ELSE
008900970117     C                   MOVEL     TBLUNI        DS7R
009000970123     C*  Se particolarià non prevede invio note mancata consegna
009100970123     C*  al cliente do errore
009200970123     C                   MOVEA     TMC           WTMC              5
009300970123     C     WTMC          IFEQ      *BLANKS
009400970123     C                   MOVEL     '1'           D53ERR
009500970123     C                   MOVEL     ERR(7)        D53MSG
009600970123     C                   GOTO      FINE
009700970123     C                   END
009800970123     C*  Se la particolarità consegna prevede un tipo l.avviso
009900970123     C*  diverso da standard e ho ricevuto un evento di questo
010000970123     C*  tipo modifico codice e descrizione l.avviso con quella
010100970123     C*  proposto dalla particolarità
010200970611     C     §7R4EV        IFNE      *BLANKS
010300970123     C     D53TMC        ANDEQ     'A'
010400970123     C                   Z-ADD     1             X
010500970611     C     §7R4EV        LOOKUP    C2A(X)                                 32
010600970123     C     *IN32         IFEQ      '1'
010700970123     C                   MOVEL     D2A(X)        D53DMC
010800970123     C                   MOVEL     C2A(X)        D53CMC
010900970123     C                   END
011000970123     C                   END
011100970123     C*
011200970117     C                   END
011300970123     C                   END
011400970117     C*
011500970117     C                   ENDSR
011600970117     C*-----------------------------------------------------*
011700970117     C*  CRTGMA: Controllo la particolarità consegna
011800970117     C*-----------------------------------------------------*
011900970117     C     CTRGMA        BEGSR
012000970121     C*------------------------------------------------*
012100970121     C*  Controllo se il tipo del codice mancata consegna
012200970121     C     D53TMC        IFEQ      *BLANKS
012300970121     C     D53VIS        ANDEQ     'S'
012400970121     C                   MOVEL     ERR(6)        D53MSG
012500970121     C                   MOVEL     '1'           D53ERR
012600970121     C                   GOTO      FINCTR
012700970121     C                   END
012800970117     C*------------------------------------------------*
012900970117     C*  Controllo se il tipo del codice mancata consegna
013000970117     C*  immesso è congruente con la particolarità:
013100970117     C*     - se la particolarità prevede tutti i casi
013200970117     C*       di mancata consegna ---> Tutto Ok
013300970117     C                   Z-ADD     1             X                 3 0
013400970117     C     'T'           LOOKUP    TMC(X)                                 32
013500970117     C*    - se la particolarità gestisce il tipo mancata
013600970117     C*      consegna del codice che sto esaminando
013700970117     C*    --> Tutto Ok
013800970117     C  N32D53TMC        LOOKUP    TMC(X)                                 32
013900970117     C*
014000970117     C*  ed in caso contrario do messaggio di errore
014100970117     C     *IN32         IFEQ      '0'
014200970123     C     D53VIS        IFEQ      'S'
014300970117     C                   MOVEL     ERR(2)        D53MSG
014400970117     C                   MOVEL     '1'           D53ERR
014500970117     C                   GOTO      FINCTR
014600970123     C                   ELSE
014700970123     C                   MOVEL     *BLANKS       D53NT1
014800970123     C                   MOVEL     *BLANKS       D53NT2
014900970123     C                   GOTO      FINE
015000970117     C                   END
015100970123     C                   END
015200970117     C*------------------------------------------------*
015300970117     C*  Leggo i tipi di estensione bolla
015400970117     C                   Z-ADD     0             Y                 2 0
015500970211     C                   Z-ADD     0             YY                3 0
015600970117     C                   MOVEL     '7K'          KCOD
015700970117     C     KTAB2         CHAIN     TABEL00F                           31
015800970117     C     *IN31         DOWEQ     '0'
015900970117     C     TBLFLG        IFEQ      *BLANKS
016000970117     C                   MOVEL     TBLUNI        DS7K
016100970117     C*  Se esistono tp.record che prevedono la memorizz.del
016200970117     C*  tipo mancata consegna in esame li carico in schiera
016300970217     C     §7KTMC        IFNE      *BLANKS
016400970217     C     §7KTMC        IFEQ      D53TMC
016500970117     C                   ADD       1             Y
016600970529     C                   MOVEL     TBLKEY        T7K(Y)
016700970211     C                   ADD       1             YY
016800970211     C                   MOVEL     TBLKEY        TRC(YY)
016900970529     C                   END
017000970217     C                   END
017100970117     C                   END
017200970117     C     KTAB2         READE     TABEL00F                               31
017300970117     C                   END
017400970117     C*  Se non ho caricato nulla do errore
017500970120     C     Y             IFEQ      0
017600970117     C                   MOVEL     ERR(3)        D53MSG
017700970117     C                   MOVEL     '1'           D53ERR
017800970117     C                   GOTO      FINCTR
017900970117     C                   END
018000970117     C*------------------------------------------------*
018100970117     C*  Imposto dati spedizione
018200970117     C                   Z-ADD     D53AAS        KAAS
018300970117     C                   Z-ADD     D53LNP        KLNP
018400970117     C                   Z-ADD     D53NRS        KNRS
018500970117     C                   Z-ADD     D53NSP        KNSP
018600970117     C*  Se la particolarità prevede un solo invio e ho
018700970529     C*  già scritto delle estensioni bolla per quel
018800970529     C*  motivo di consegna do errore
018900970117     C     VOL(X)        IFNE      'S'
019000970211     C                   DO        YY            S                 3 0
019100970211     C                   MOVEL     TRC(S)        KTRC
019200970117     C     KAR4          CHAIN     FNAR401L                           31
019300970117     C     *IN31         IFEQ      '0'
019400970211     C                   MOVEL     'S'           D53TRV
019500970123     C     D53VIS        IFEQ      'S'
019600970117     C                   MOVEL     ERR(4)        D53MSG
019700970220     C     D53RIC        IFEQ      'W'
019800970117     C                   MOVEL     '1'           D53ERR
019900970117     C                   GOTO      FINCTR
020000970220     C                   ELSE
020100970220     C                   MOVEL     *BLANKS       D53NT1
020200970220     C                   MOVEL     *BLANKS       D53NT2
020300970220     C                   GOTO      FINE
020400970220     C                   END
020500970123     C                   ELSE
020600970123     C                   MOVEL     *BLANKS       D53NT1
020700970211     C                   MOVEL     *BLANKS       D53NT2
020800970123     C                   GOTO      FINE
020900970123     C                   END
021000970117     C                   END
021100970211     C*
021200970211     C                   END
021300970117     C                   END
021400970117     C*------------------------------------------------*
021500970117     C     FINCTR        ENDSR
021600970117     C*-----------------------------------------------------*
021700970117     C*  CARS01: Caricamento subfile note
021800970117     C*-----------------------------------------------------*
021900970117     C     CARS01        BEGSR
022000970117     C*
022100970117     C                   SETOFF                                       2021
022200970117     C                   WRITE     LR53C01
022300970117     C                   SETON                                        2021
022400970121     C* Aggiornamento nr.spedizione
022500970121     C                   Z-ADD     D53AAS        V1CAAS
022600970121     C                   Z-ADD     D53LNP        V1CLNP
022700970121     C                   Z-ADD     D53NRS        V1CNRS
022800970121     C                   Z-ADD     D53NSP        V1CNSP
022900970121     C* Se devo aggiornare un motivo generico eseguo caric. da
023000970121     C* prima riga
023100970121     C                   MOVEL     *BLANKS       WNOT1            35
023200970121     C                   MOVEL     *BLANKS       WNOT2            35
023300970121     C                   MOVE      D53NT1        WCOM31           31
023400970121     C     D53NT1        IFNE      *BLANKS
023500970121     C     D53EXC        IFEQ      'G  '
023600970121     C     D53EXC        OREQ      'GEN'
023700970124     C                   SETOFF                                       02
023800970121     C                   MOVEL     WCOM31        WNOT1
023900970121     C                   MOVEL     D53NT2        WNOT2
024000970121     C                   ELSE
024100970124     C                   SETON                                        02
024200970121     C                   MOVEL     D53NT2        WNOT1
024300970121     C                   END
024400970121     C                   END
024500970117     C* Carico prima riga
024600970117     C                   MOVEL     D53CMC        V1CCMC
024700970120     C     D53CMC        IFNE      'G  '
024800970318     C     D53CMC        ANDNE     'GEN'
024900970117     C                   MOVEL     D53DMC        V1DCMC
025000970121     C                   ELSE
025100970121     C     WNOT1         IFNE      *BLANKS
025200970121     C                   MOVEL     WNOT1         V1DCMC
025300970121     C                   MOVEL     *BLANKS       WNOT1
025400970121     C                   ELSE
025500970121     C     WNOT2         IFNE      *BLANKS
025600970121     C                   MOVEL     WNOT2         V1DCMC
025700970121     C                   MOVEL     *BLANKS       WNOT2
025800970121     C                   ELSE
025900970121     C                   MOVEL     *BLANKS       V1DCMC
026000970121     C                   END
026100970121     C                   END
026200970120     C                   END
026300970117     C* Se decodifica motivo non è a blanks proteggo campo
026400970117     C     V1DCMC        COMP      *BLANKS                            0101
026500970121     C                   Z-ADD     1             Z                 4 0
026600970121     C*--------------------------------------------------------*
026700970121     C* Eseguo caricamento righe vuote
026800970121     C     W             DO        Y
026900970117     C                   ADD       1             NRR               4 0
027000970121     C     WNOT1         IFNE      *BLANKS
027100970121     C                   MOVEL     WNOT1         V1CNOT
027200970121     C                   MOVEL     *BLANKS       WNOT1
027300970121     C                   ELSE
027400970121     C     WNOT2         IFNE      *BLANKS
027500970121     C                   MOVEL     WNOT2         V1CNOT
027600970121     C                   MOVEL     *BLANKS       WNOT2
027700970121     C                   ELSE
027800970117     C                   MOVEL     *BLANKS       V1CNOT
027900970121     C                   END
028000970121     C                   END
028100970117     C                   WRITE     LR53S01
028200970117     C                   END
028300970120     C*
028400970120     C                   Z-ADD     1             NR1KEY
028500970117     C*
028600970117     C                   ENDSR
028700970121     C*-----------------------------------------------------*
028800970121     C*  DECCMC: Decodifica prima riga
028900970121     C*-----------------------------------------------------*
029000970121     C     DECCMC        BEGSR
029100970121     C*
029200970121     C* Carico prima riga
029300970121     C                   MOVEL     D53CMC        D53NT1
029400970121     C                   MOVEL     D53DMC        WCOM31
029500970121     C                   MOVE      WCOM31        D53NT1
029600970121     C                   MOVEL     T7K(1)        D53TR1
029700970121     C*
029800970121     C                   ENDSR
029900970117     C*-----------------------------------------------------*
030000970117     C*  GESS01: Gestione subfile note
030100970117     C*-----------------------------------------------------*
030200970117     C     GESS01        BEGSR
030300970117     C*
030400970117     C*  Eseguo exfmt
030500970117     C                   WRITE     LR53D02
030600970117     C                   WRITE     LR53Z01
030700970117     C                   EXFMT     LR53C01
030800970117     C                   SETOFF                                       284041
030900970117     C*  Ritorno
031000970117     C     *INKL         IFEQ      '1'
031100970117     C                   MOVEL     'S'           WFINE             1
031200970117     C                   GOTO      FINVD1
031300970117     C                   END
031400970117     C*  Controlli
031500970117     C                   EXSR      CTR01
031600970117     C*  Se non ci sono errori vado a fine pgm
031700970117     C  N28              MOVEL     'S'           WFINE
031800970117     C*
031900970117     C     FINVD1        ENDSR
032000970117     C*-----------------------------------------------------*
032100970117     C*  CTR01: Controlli
032200970117     C*-----------------------------------------------------*
032300970117     C     CTR01         BEGSR
032400970117     C*
032500970117     C*  Controllo se dovevo immettere 1° decodifica
032600970120     C     D53CMC        IFEQ      'G  '
032700970318     C     D53CMC        OREQ      'GEN'
032800970117     C     V1DCMC        IFEQ      *BLANKS
032900970117     C                   SETON                                        2840
033000970117     C                   MOVEL     ERR(5)        $MSG
033100970117     C                   END
033200970117     C                   ELSE
033300970117     C*  Controllo se richiesta immissione righe aggiuntive
033400970117     C     1             CHAIN     LR53S01                            29
033500970117     C     V1CNOT        IFEQ      *BLANKS
033600970117     C                   SETON                                        2841
033700970117     C                   MOVEL     ERR(5)        $MSG
033800970117     C                   ELSE
033900970117     C                   SETOFF                                       41
034000970117     C                   END
034100970117     C                   UPDATE    LR53S01
034200970117     C                   END
034300970117     C*
034400970117     C                   ENDSR
034500970117     C*-----------------------------------------------------*
034600970121     C*  AGGIO: Aggiorno subfile note consegna
034700970117     C*-----------------------------------------------------*
034800970117     C     AGGIO         BEGSR
034900970121     C*------------------------------------------------------*
035000970121     C                   MOVEL     *BLANKS       D53TR1
035100970121     C                   MOVEL     *BLANKS       D53NT1
035200970121     C                   MOVEL     *BLANKS       D53TR2
035300970121     C                   MOVEL     *BLANKS       D53NT2
035400970117     C*  Scrivo prima riga
035500970121     C                   MOVEL     T7K(1)        D53TR1
035600970121     C                   MOVEL     V1CCMC        D53NT1
035700970121     C                   MOVE      V1DCMC        D53NT1
035800970117     C*  Carico altre righe
035900970117     C                   Z-ADD     1             NRR
036000970121     C                   Z-ADD     1             W                 4 0
036100970117     C     NRR           CHAIN     LR53S01                            29
036200970117     C     *IN29         DOWEQ     '0'
036300970117     C     V1CNOT        IFNE      *BLANKS
036400970121     C                   ADD       1             W
036500970121     C                   MOVEL     T7K(W)        D53TR2
036600970121     C                   MOVEL     V1CNOT        D53NT2
036700970121     C                   END
036800970121     C                   ADD       1             NRR
036900970121     C     NRR           CHAIN     LR53S01                            29
037000970121     C                   END
037100970121     C*------------------------------------------------------*
037200970117     C                   ENDSR
037300970117     C*-----------------------------------------------------*
037400970117     C*  *INZSR: Operazioni iniziali
037500970117     C*-----------------------------------------------------*
037600970117     C     *INZSR        BEGSR
037700970117     C*
037800970117     C     *ENTRY        PLIST
037900970117     C                   PARM                    KPJBA
038000970117     C                   MOVEL     KPJBU         FNLR53
038100970121     C* Open controllate
038200970121     C                   OPEN      FNAR401L                             16
038300970121     C   16              GOTO      FINE                                         OPEN vado a fine
038400970117     C* Definisco chiave di accesso
038500970117     C     KTAB1         KLIST
038600970117     C                   KFLD                    KKUT
038700970117     C                   KFLD                    KCOD
038800970117     C                   KFLD                    KKEY
038900970117     C     KTAB2         KLIST
039000970117     C                   KFLD                    KKUT
039100970117     C                   KFLD                    KCOD
039200970117     C     KAR4          KLIST
039300970117     C                   KFLD                    KAAS
039400970117     C                   KFLD                    KLNP
039500970117     C                   KFLD                    KNRS
039600970117     C                   KFLD                    KNSP
039700970117     C                   KFLD                    KTRC
039800970117     C* Definisco variabili
039900970117     C     *LIKE         DEFINE    AR4AAS        KAAS
040000970117     C     *LIKE         DEFINE    AR4LNP        KLNP
040100970117     C     *LIKE         DEFINE    AR4NRS        KNRS
040200970117     C     *LIKE         DEFINE    AR4NSP        KNSP
040300970117     C     *LIKE         DEFINE    AR4TRC        KTRC
040400970117     C     *LIKE         DEFINE    TBLKUT        KKUT
040500970117     C     *LIKE         DEFINE    TBLCOD        KCOD
040600970117     C     *LIKE         DEFINE    TBLKEY        KKEY
040700970123     C*
040800970123     C                   Z-ADD     0             X
040900970123     C                   Z-ADD     1             KKUT
041000970123     C                   MOVEL     '2A'          KCOD
041100970123     C     KTAB2         CHAIN     TABEL00F                           96
041200970123     C     *IN96         DOWEQ     '0'
041300970123     C     X             ANDLT     100
041400970123     C     TBLFLG        IFEQ      ' '
041500970123     C                   ADD       1             X
041600970123     C                   MOVEL     TBLKEY        C2A(X)
041700970123     C                   MOVEL     TBLUNI        D2A(X)
041800970123     C                   END
041900970123     C     KTAB2         READE     TABEL00F                               96
042000970123     C                   END
042100970123     C     *IN96         IFEQ      '0'
042200970123     C                   SETON                                        H1
042300970123     C                   GOTO      FINE
042400970123     C                   END
042500970117     C*
042600970117     C                   ENDSR
042700970123**
042800970117Codice particolarità inesistente
042900970117Non prevista l'immissione di note per questo tipo di mancata consegna
043000970117Non previsti cod.estenzioni bolle per questo tipo di mancata consegna
043100970117Primo invio già effettuato: la particolarità non ne consente altri
043200970117Descrizione obbligatoria
043300970121Non prevista immissione note se merce consegnata
043400970123Cliente non abilitato alla trasmissione dei motivi mancata consegna
