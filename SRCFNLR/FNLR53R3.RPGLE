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
001300061030     F*iAR401L  IF   E           K DISK    USROPN
001400061030     F*                                    commit
001500061030     FFiAR401L  IF   E           K DISK
001600970117      *----------------------------------------------------------------
001700970117      *  S C H I E R E
001800970117      *----------------------------------------------------------------
001900970529     D T7K             S              2    DIM(50)                              TRC
002000090119     D TRC             S              1    DIM(200)                             TRC X NO CONS
002100090119     D C2A             S              3    DIM(200)                             SKI CHIAVI 2A
002200090119     D D2A             S             89    DIM(200)                             SKI TAB. 2A
002300970123     D ERR             S             70    DIM(7) CTDATA PERRCD(1)
002400961119     D*--------------------------------------------------------------*
002500970117     D*  D S
002600961119     D*--------------------------------------------------------------*
002700961119     D KPJBA         E DS
002800970117     D FNLR53        E DS                  EXTNAME(FNLR53DS3)
002900970117     D DS7K          E DS
003000970123     D DS2A          E DS
003100970117     D DS7R          E DS
003200970120     D  TMC                   47     51
003300970117     D                                     DIM(5)                               TIP.NO CONSE.
003400970117     D  VOL                   52     56
003500970117     D                                     DIM(5)                               VOLTE
003600961119     C*--------------------------------------------------------------*
003700961119     C*  C I C L O        P R I N C I P A L E
003800961119     C*--------------------------------------------------------------*
003900970117     C* Reperisco codice particolarità consegna
004000970117     C                   EXSR      REPGMA
004100970117     C* Eseguo controlli
004200970117     C                   EXSR      CTRGMA
004300970117     C* Se non ci sono errori gestisco videata
004400970117     C     D53ERR        IFNE      '1'
004500970121     C* Se devo visualizzare dati eseguo caricamento ed emissione
004600970121     C* subfile
004700970121     C     D53VIS        IFEQ      'S'
004800970117     C                   EXSR      CARS01                                       Carico dati
004900970121     C*     Gestione note
005000970117     C     WFINE         DOWNE     'S'
005100970117     C                   EXSR      GESS01
005200970117     C                   END
005300970121     C*     Se no F12 aggiorno
005400970117     C     *INKL         IFEQ      '0'
005500970117     C                   EXSR      AGGIO
005600970121     C                   ELSE
005700970121     C                   MOVEL     '1'           D53F12
005800970117     C                   END
005900970121     C* ... altrimenti imposto decodifica 1a riga
006000970121     C                   ELSE
006100970121     C                   EXSR      DECCMC
006200970121     C                   END
006300970121     C                   END
006400970121     C*
006500970121     C*
006600961217     C     FINE          TAG
006700970123     C                   MOVEL     FNLR53        KPJBU
006800961121     C                   SETON                                        LR
006900970117     C*-----------------------------------------------------*
007000970117     C*  REPGMA: Reperisco particolarità consegna
007100970117     C*-----------------------------------------------------*
007200970117     C     REPGMA        BEGSR
007300970117     C*
007400970117     C* Carico dati tabella 7R
007500970123     C     D53GMA        IFEQ      *BLANKS
007600970123     C                   MOVEL     '1'           D53ERR
007700970123     C                   MOVEL     ERR(7)        D53MSG
007800970123     C                   GOTO      FINE
007900970123     C                   ELSE
008000970117     C                   Z-ADD     1             KKUT
008100970117     C                   MOVEL     '7R'          KCOD
008200970117     C                   MOVEL     *BLANKS       KKEY
008300970117     C                   MOVEL     D53GMA        KKEY
008400970117     C     KTAB1         CHAIN     TABEL00F                           31
008500970117     C     *IN31         IFEQ      '1'
008600970117     C                   MOVEL     '1'           D53ERR
008700970117     C                   MOVEL     ERR(1)        D53MSG
008800970117     C                   GOTO      FINE
008900970117     C                   ELSE
009000970117     C                   MOVEL     TBLUNI        DS7R
009100970123     C*  Se particolarià non prevede invio note mancata consegna
009200970123     C*  al cliente do errore
009300970123     C                   MOVEA     TMC           WTMC              5
009400970123     C     WTMC          IFEQ      *BLANKS
009500970123     C                   MOVEL     '1'           D53ERR
009600970123     C                   MOVEL     ERR(7)        D53MSG
009700970123     C                   GOTO      FINE
009800970123     C                   END
009900970123     C*  Se la particolarità consegna prevede un tipo l.avviso
010000970123     C*  diverso da standard e ho ricevuto un evento di questo
010100970123     C*  tipo modifico codice e descrizione l.avviso con quella
010200970123     C*  proposto dalla particolarità
010300061030     C*mm  §7R4EV        IFNE      *BLANKS
010400061030     C*mm  D53TMC        ANDEQ     'A'
010500970123     C                   Z-ADD     1             X
010600061030     C*mm  §7R4EV        LOOKUP    C2A(X)                                 32
010700061030     C     d53cmc        LOOKUP    C2A(X)                                 32
010800970123     C     *IN32         IFEQ      '1'
010900970123     C                   MOVEL     D2A(X)        D53DMC
011000061030     C*mm                MOVEL     C2A(X)        D53CMC
011100970123     C                   END
011200061030     C*mm                END
011300970123     C*
011400970117     C                   END
011500970123     C                   END
011600970117     C*
011700970117     C                   ENDSR
011800970117     C*-----------------------------------------------------*
011900970117     C*  CRTGMA: Controllo la particolarità consegna
012000970117     C*-----------------------------------------------------*
012100970117     C     CTRGMA        BEGSR
012200970121     C*------------------------------------------------*
012300970121     C*  Controllo se il tipo del codice mancata consegna
012400970121     C     D53TMC        IFEQ      *BLANKS
012500970121     C     D53VIS        ANDEQ     'S'
012600970121     C                   MOVEL     ERR(6)        D53MSG
012700970121     C                   MOVEL     '1'           D53ERR
012800970121     C                   GOTO      FINCTR
012900970121     C                   END
013000970117     C*------------------------------------------------*
013100970117     C*  Controllo se il tipo del codice mancata consegna
013200970117     C*  immesso è congruente con la particolarità:
013300970117     C*     - se la particolarità prevede tutti i casi
013400970117     C*       di mancata consegna ---> Tutto Ok
013500970117     C                   Z-ADD     1             X                 3 0
013600970117     C     'T'           LOOKUP    TMC(X)                                 32
013700970117     C*    - se la particolarità gestisce il tipo mancata
013800970117     C*      consegna del codice che sto esaminando
013900970117     C*    --> Tutto Ok
014000970117     C  N32D53TMC        LOOKUP    TMC(X)                                 32
014100970117     C*
014200970117     C*  ed in caso contrario do messaggio di errore
014300970117     C     *IN32         IFEQ      '0'
014400970123     C     D53VIS        IFEQ      'S'
014500970117     C                   MOVEL     ERR(2)        D53MSG
014600970117     C                   MOVEL     '1'           D53ERR
014700970117     C                   GOTO      FINCTR
014800970123     C                   ELSE
014900970123     C                   MOVEL     *BLANKS       D53NT1
015000970123     C                   MOVEL     *BLANKS       D53NT2
015100970123     C                   GOTO      FINE
015200970117     C                   END
015300970123     C                   END
015400970117     C*------------------------------------------------*
015500970117     C*  Leggo i tipi di estensione bolla
015600970117     C                   Z-ADD     0             Y                 2 0
015700970211     C                   Z-ADD     0             YY                3 0
015800970117     C                   MOVEL     '7K'          KCOD
015900970117     C     KTAB2         CHAIN     TABEL00F                           31
016000970117     C     *IN31         DOWEQ     '0'
016100970117     C     TBLFLG        IFEQ      *BLANKS
016200970117     C                   MOVEL     TBLUNI        DS7K
016300970117     C*  Se esistono tp.record che prevedono la memorizz.del
016400970117     C*  tipo mancata consegna in esame li carico in schiera
016500970217     C     §7KTMC        IFNE      *BLANKS
016600970217     C     §7KTMC        IFEQ      D53TMC
016700970117     C                   ADD       1             Y
016800970529     C                   MOVEL     TBLKEY        T7K(Y)
016900970211     C                   ADD       1             YY
017000970211     C                   MOVEL     TBLKEY        TRC(YY)
017100970529     C                   END
017200970217     C                   END
017300970117     C                   END
017400970117     C     KTAB2         READE     TABEL00F                               31
017500970117     C                   END
017600970117     C*  Se non ho caricato nulla do errore
017700970120     C     Y             IFEQ      0
017800970117     C                   MOVEL     ERR(3)        D53MSG
017900970117     C                   MOVEL     '1'           D53ERR
018000970117     C                   GOTO      FINCTR
018100970117     C                   END
018200970117     C*------------------------------------------------*
018300970117     C*  Imposto dati spedizione
018400970117     C                   Z-ADD     D53AAS        KAAS
018500970117     C                   Z-ADD     D53LNP        KLNP
018600970117     C                   Z-ADD     D53NRS        KNRS
018700970117     C                   Z-ADD     D53NSP        KNSP
018800970117     C*  Se la particolarità prevede un solo invio e ho
018900970529     C*  già scritto delle estensioni bolla per quel
019000970529     C*  motivo di consegna do errore
019100970117     C     VOL(X)        IFNE      'S'
019200970211     C                   DO        YY            S                 3 0
019300970211     C                   MOVEL     TRC(S)        KTRC
019400060214     C     KAR4          CHAIN     FiAR401L                           31
019500970117     C     *IN31         IFEQ      '0'
019600970211     C                   MOVEL     'S'           D53TRV
019700970123     C     D53VIS        IFEQ      'S'
019800970117     C                   MOVEL     ERR(4)        D53MSG
019900970220     C     D53RIC        IFEQ      'W'
020000970117     C                   MOVEL     '1'           D53ERR
020100970117     C                   GOTO      FINCTR
020200970220     C                   ELSE
020300970220     C                   MOVEL     *BLANKS       D53NT1
020400970220     C                   MOVEL     *BLANKS       D53NT2
020500970220     C                   GOTO      FINE
020600970220     C                   END
020700970123     C                   ELSE
020800970123     C                   MOVEL     *BLANKS       D53NT1
020900970211     C                   MOVEL     *BLANKS       D53NT2
021000970123     C                   GOTO      FINE
021100970123     C                   END
021200970117     C                   END
021300970211     C*
021400970211     C                   END
021500970117     C                   END
021600970117     C*------------------------------------------------*
021700970117     C     FINCTR        ENDSR
021800970117     C*-----------------------------------------------------*
021900970117     C*  CARS01: Caricamento subfile note
022000970117     C*-----------------------------------------------------*
022100970117     C     CARS01        BEGSR
022200970117     C*
022300970117     C                   SETOFF                                       2021
022400970117     C                   WRITE     LR53C01
022500970117     C                   SETON                                        2021
022600970121     C* Aggiornamento nr.spedizione
022700970121     C                   Z-ADD     D53AAS        V1CAAS
022800970121     C                   Z-ADD     D53LNP        V1CLNP
022900970121     C                   Z-ADD     D53NRS        V1CNRS
023000970121     C                   Z-ADD     D53NSP        V1CNSP
023100970121     C* Se devo aggiornare un motivo generico eseguo caric. da
023200970121     C* prima riga
023300970121     C                   MOVEL     *BLANKS       WNOT1            35
023400970121     C                   MOVEL     *BLANKS       WNOT2            35
023500970121     C                   MOVE      D53NT1        WCOM31           31
023600970121     C     D53NT1        IFNE      *BLANKS
023700970121     C     D53EXC        IFEQ      'G  '
023800970121     C     D53EXC        OREQ      'GEN'
023900970124     C                   SETOFF                                       02
024000970121     C                   MOVEL     WCOM31        WNOT1
024100970121     C                   MOVEL     D53NT2        WNOT2
024200970121     C                   ELSE
024300970124     C                   SETON                                        02
024400970121     C                   MOVEL     D53NT2        WNOT1
024500970121     C                   END
024600970121     C                   END
024700970117     C* Carico prima riga
024800970117     C                   MOVEL     D53CMC        V1CCMC
024900970120     C     D53CMC        IFNE      'G  '
025000970318     C     D53CMC        ANDNE     'GEN'
025100970117     C                   MOVEL     D53DMC        V1DCMC
025200970121     C                   ELSE
025300970121     C     WNOT1         IFNE      *BLANKS
025400970121     C                   MOVEL     WNOT1         V1DCMC
025500970121     C                   MOVEL     *BLANKS       WNOT1
025600970121     C                   ELSE
025700970121     C     WNOT2         IFNE      *BLANKS
025800970121     C                   MOVEL     WNOT2         V1DCMC
025900970121     C                   MOVEL     *BLANKS       WNOT2
026000970121     C                   ELSE
026100970121     C                   MOVEL     *BLANKS       V1DCMC
026200970121     C                   END
026300970121     C                   END
026400970120     C                   END
026500970117     C* Se decodifica motivo non è a blanks proteggo campo
026600970117     C     V1DCMC        COMP      *BLANKS                            0101
026700970121     C                   Z-ADD     1             Z                 4 0
026800970121     C*--------------------------------------------------------*
026900970121     C* Eseguo caricamento righe vuote
027000970121     C     W             DO        Y
027100970117     C                   ADD       1             NRR               4 0
027200970121     C     WNOT1         IFNE      *BLANKS
027300970121     C                   MOVEL     WNOT1         V1CNOT
027400970121     C                   MOVEL     *BLANKS       WNOT1
027500970121     C                   ELSE
027600970121     C     WNOT2         IFNE      *BLANKS
027700970121     C                   MOVEL     WNOT2         V1CNOT
027800970121     C                   MOVEL     *BLANKS       WNOT2
027900970121     C                   ELSE
028000970117     C                   MOVEL     *BLANKS       V1CNOT
028100970121     C                   END
028200970121     C                   END
028300970117     C                   WRITE     LR53S01
028400970117     C                   END
028500970120     C*
028600970120     C                   Z-ADD     1             NR1KEY
028700970117     C*
028800970117     C                   ENDSR
028900970121     C*-----------------------------------------------------*
029000970121     C*  DECCMC: Decodifica prima riga
029100970121     C*-----------------------------------------------------*
029200970121     C     DECCMC        BEGSR
029300970121     C*
029400970121     C* Carico prima riga
029500970121     C                   MOVEL     D53CMC        D53NT1
029600970121     C                   MOVEL     D53DMC        WCOM31
029700970121     C                   MOVE      WCOM31        D53NT1
029800970121     C                   MOVEL     T7K(1)        D53TR1
029900970121     C*
030000970121     C                   ENDSR
030100970117     C*-----------------------------------------------------*
030200970117     C*  GESS01: Gestione subfile note
030300970117     C*-----------------------------------------------------*
030400970117     C     GESS01        BEGSR
030500970117     C*
030600970117     C*  Eseguo exfmt
030700970117     C                   WRITE     LR53D02
030800970117     C                   WRITE     LR53Z01
030900970117     C                   EXFMT     LR53C01
031000970117     C                   SETOFF                                       284041
031100970117     C*  Ritorno
031200970117     C     *INKL         IFEQ      '1'
031300970117     C                   MOVEL     'S'           WFINE             1
031400970117     C                   GOTO      FINVD1
031500970117     C                   END
031600970117     C*  Controlli
031700970117     C                   EXSR      CTR01
031800970117     C*  Se non ci sono errori vado a fine pgm
031900970117     C  N28              MOVEL     'S'           WFINE
032000970117     C*
032100970117     C     FINVD1        ENDSR
032200970117     C*-----------------------------------------------------*
032300970117     C*  CTR01: Controlli
032400970117     C*-----------------------------------------------------*
032500970117     C     CTR01         BEGSR
032600970117     C*
032700970117     C*  Controllo se dovevo immettere 1° decodifica
032800970120     C     D53CMC        IFEQ      'G  '
032900970318     C     D53CMC        OREQ      'GEN'
033000970117     C     V1DCMC        IFEQ      *BLANKS
033100970117     C                   SETON                                        2840
033200970117     C                   MOVEL     ERR(5)        $MSG
033300970117     C                   END
033400970117     C                   ELSE
033500970117     C*  Controllo se richiesta immissione righe aggiuntive
033600970117     C     1             CHAIN     LR53S01                            29
033700970117     C     V1CNOT        IFEQ      *BLANKS
033800970117     C                   SETON                                        2841
033900970117     C                   MOVEL     ERR(5)        $MSG
034000970117     C                   ELSE
034100970117     C                   SETOFF                                       41
034200970117     C                   END
034300970117     C                   UPDATE    LR53S01
034400970117     C                   END
034500970117     C*
034600970117     C                   ENDSR
034700970117     C*-----------------------------------------------------*
034800970121     C*  AGGIO: Aggiorno subfile note consegna
034900970117     C*-----------------------------------------------------*
035000970117     C     AGGIO         BEGSR
035100970121     C*------------------------------------------------------*
035200970121     C                   MOVEL     *BLANKS       D53TR1
035300970121     C                   MOVEL     *BLANKS       D53NT1
035400970121     C                   MOVEL     *BLANKS       D53TR2
035500970121     C                   MOVEL     *BLANKS       D53NT2
035600970117     C*  Scrivo prima riga
035700970121     C                   MOVEL     T7K(1)        D53TR1
035800970121     C                   MOVEL     V1CCMC        D53NT1
035900970121     C                   MOVE      V1DCMC        D53NT1
036000970117     C*  Carico altre righe
036100970117     C                   Z-ADD     1             NRR
036200970121     C                   Z-ADD     1             W                 4 0
036300970117     C     NRR           CHAIN     LR53S01                            29
036400970117     C     *IN29         DOWEQ     '0'
036500970117     C     V1CNOT        IFNE      *BLANKS
036600970121     C                   ADD       1             W
036700970121     C                   MOVEL     T7K(W)        D53TR2
036800970121     C                   MOVEL     V1CNOT        D53NT2
036900970121     C                   END
037000970121     C                   ADD       1             NRR
037100970121     C     NRR           CHAIN     LR53S01                            29
037200970121     C                   END
037300970121     C*------------------------------------------------------*
037400970117     C                   ENDSR
037500970117     C*-----------------------------------------------------*
037600970117     C*  *INZSR: Operazioni iniziali
037700970117     C*-----------------------------------------------------*
037800970117     C     *INZSR        BEGSR
037900970117     C*
038000970117     C     *ENTRY        PLIST
038100970117     C                   PARM                    KPJBA
038200970117     C                   MOVEL     KPJBU         FNLR53
038300970121     C* Open controllate
038400061030     C*                  OPEN      FiAR401L                             16
038500061030     C*  16              GOTO      FINE                                         OPEN vado a fine
038600970117     C* Definisco chiave di accesso
038700970117     C     KTAB1         KLIST
038800970117     C                   KFLD                    KKUT
038900970117     C                   KFLD                    KCOD
039000970117     C                   KFLD                    KKEY
039100970117     C     KTAB2         KLIST
039200970117     C                   KFLD                    KKUT
039300970117     C                   KFLD                    KCOD
039400970117     C     KAR4          KLIST
039500970117     C                   KFLD                    KAAS
039600970117     C                   KFLD                    KLNP
039700970117     C                   KFLD                    KNRS
039800970117     C                   KFLD                    KNSP
039900970117     C                   KFLD                    KTRC
040000970117     C* Definisco variabili
040100970117     C     *LIKE         DEFINE    AR4AAS        KAAS
040200970117     C     *LIKE         DEFINE    AR4LNP        KLNP
040300970117     C     *LIKE         DEFINE    AR4NRS        KNRS
040400970117     C     *LIKE         DEFINE    AR4NSP        KNSP
040500970117     C     *LIKE         DEFINE    AR4TRC        KTRC
040600970117     C     *LIKE         DEFINE    TBLKUT        KKUT
040700970117     C     *LIKE         DEFINE    TBLCOD        KCOD
040800970117     C     *LIKE         DEFINE    TBLKEY        KKEY
040900970123     C*
041000970123     C                   Z-ADD     0             X
041100970123     C                   Z-ADD     1             KKUT
041200970123     C                   MOVEL     '2A'          KCOD
041300970123     C     KTAB2         CHAIN     TABEL00F                           96
041400970123     C     *IN96         DOWEQ     '0'
041500090119     C     X             ANDLT     200
041600970123     C     TBLFLG        IFEQ      ' '
041700970123     C                   ADD       1             X
041800970123     C                   MOVEL     TBLKEY        C2A(X)
041900970123     C                   MOVEL     TBLUNI        D2A(X)
042000970123     C                   END
042100970123     C     KTAB2         READE     TABEL00F                               96
042200970123     C                   END
042300970123     C     *IN96         IFEQ      '0'
042400970123     C                   SETON                                        H1
042500970123     C                   GOTO      FINE
042600970123     C                   END
042700970117     C*
042800970117     C                   ENDSR
042900970123**
043000970117Codice particolarità inesistente
043100970117Non prevista l'immissione di note per questo tipo di mancata consegna
043200970117Non previsti cod.estenzioni bolle per questo tipo di mancata consegna
043300970117Primo invio già effettuato: la particolarità non ne consente altri
043400970117Descrizione obbligatoria
043500970121Non prevista immissione note se merce consegnata
043600970123Cliente non abilitato alla trasmissione dei motivi mancata consegna
