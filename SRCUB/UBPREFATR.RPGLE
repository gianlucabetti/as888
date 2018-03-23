000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400910830     FTABEL00F  IF   E           K DISK
000500040531     FFIAR531C  IF   E           K DISK
000600150917     FTNCSB03L  IF   E           K DISK
000700150917     FFNEVB04L  IF   E           K DISK
000800050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
000900990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
001000150917
001100150917     D* Codici bolla che prevedono la tassazione di una singola varia
001200150917     D CBO             S              2    DIM(50)                              LEGENDA VARIE SIGLE
001300150917     D CBV             S              1    DIM(50)                              LEGENDA VARIE SIGLE
001400150917
001500150922     D* Schiera particolarità consegna con abilitazione PIN Code
001600150917     D skGMA           S              2A   DIM(10)
001700150917
001800150922     D* DS varie di procedura
001900890921     D KPJBA         E DS
002000150917     D DS3A          E DS
002100150917     D DS7R          E DS
002200090424
002300150922     D* DS FIAR5
002400150922     D DAR5BNB       E DS
002500150922     D DAR5BAN       E DS
002600151116     D DAR5GEN       E DS
002700150921
002800150922     D* DS legate a TITAS00F
002900150922     D DTASFLO       E DS
003000150922     D DTAS          E DS
003100150922     D DSTASFLO      E                     EXTFLD(TASFLO)
003200150922     D DTAS01        E DS
003300150922     D DTASV         E DS
003400151020     D  TSV                    1     20                                         (T = da TNSF22R)
003500150922     D                                     DIM(20)                              Sigle varie
003600151020     D  TVA                   21    140P 3                                      (T = da TNSF22R)
003700150922     D                                     DIM(20)                              Importi varie
003800151020     D DTASPES       E DS
003900060127
004000150922     D* PARAMETRi di procedura
004100150922     D IN_PF_OPZ       S              1                                         C=chiusura
004200150922     D IN_PF_DSTAS   E DS                  EXTNAME(titas00f) PREFIX(IN_)
004300150922
004400150922     D* Variabili riferite al DB
004500150922     D KAR5TRD         S                   LIKE(AR5TRD)
004600150922     D KEVBCEV         S                   LIKE(EVBCEV) INZ('RIC')
004700150922     D KTBKUT          S                   LIKE(TBLKUT) INZ(1)
004800150922     D KTBCOD          S                   LIKE(TBLCOD)
004900150922
005000010402
005100060127     C*
005200150922     C* Se lancio solo per chiusura => esco in LR
005300150922     C                   IF        IN_PF_OPZ = 'C'
005400150922     C                   SETON                                        LR
005500150922     C                   ELSE
005600150922     C                   EXSR      EXEPREFAT
005700150922     C                   SETON                                        RT
005800150922     C                   ENDIF
005900150922     C*
006000910830     C*----------------------------------------------------*
006100150922     C     CARTAB        BEGSR
006200150922     C*
006300150922     C** Aggancio tabella 3A - caricamento codici bolla che prevedono di tassare solo 1 varia
006400150917     C                   MOVEL     '3A'          KTBCOD
006500150917     C                   Z-ADD     0             KK                4 0
006600150917     C     KEYTAB        SETLL     TABEL00F
006700150917     C     KEYTAB        READE     TABEL00F
006800150917     C                   DOW       not %eof(TABEL00F)
006900150917     C*
007000150917     C                   IF        TBLFLG  = *blanks AND
007100150917     C                             TBLKEY <> *blanks
007200150917     C                   MOVEL     TBLUNI        DS3A
007300150917     C* Bolla con singola varia
007400150917     C                   IF        §3ASVA  <> *blanks
007500150917     C                   ADD       1             KK
007600150917     C                   MOVEL     TBLKEY        CBO(KK)
007700150917     C                   MOVEL     §3ASVA        CBV(KK)
007800150917     C                   ENDIF
007900150917     C                   ENDIF
008000150917     C*
008100150917     C     KEYTAB        READE     TABEL00F
008200150917     C                   ENDDO
008300150922     C*
008400150922     C** Aggancio tabella 7R - caricamento particolarità consegna con PIN code
008500150917     C                   MOVEL     '7R'          KTBCOD
008600150917     C                   Z-ADD     0             KK                4 0
008700150917     C     KEYTAB        SETLL     TABEL00F
008800150917     C     KEYTAB        READE     TABEL00F
008900150917     C                   DOW       not %eof(TABEL00F)
009000150917     C                   IF        TBLFLG  = *blanks AND
009100150917     C                             TBLKEY <> *blanks
009200150917     C                   MOVEL     TBLUNI        DS7R
009300150917     C* Particolarità consegna con PIN code
009400150917     C                   IF        §7Rpincode = 'S'
009500150917     C                   ADD       1             KK
009600150917     C                   MOVEL     TBLKEY        skGMA(KK)
009700150917     C                   ENDIF
009800150917     C                   ENDIF
009900150917     C*
010000150917     C     KEYTAB        READE     TABEL00F
010100150917     C                   ENDDO
010200990817     C*
010300910830     C                   ENDSR
010400050906     C*--------------------------------------------------------
010500150922     C* EXEPREFAT - ESEGUE CALCOLO PER PREVISIOEN DATI DI FATTURAZIONE
010600000124     C*--------------------------------------------------------
010700150922     C     EXEPREFAT     BEGSR
010800150922     C*
010900150922     C* Carico le tabelle
011000150922     C                   exsr      CARTAB
011100150922     C*
011200150922     C* Inizializzazioni
011300150922     C                   exsr      INZVAR
011400150922     C*
011500150922     C* Valorizzazioini "standard" (ovvero campi corrispondenti TITAS <=> DTAS)
011600150922     C                   exsr      VALDTAS
011700150922     C*
011800150922     C* ********
011900150917     C* Reperisco tutti i dati necessari non presenti in forma diretta su TITAS
012000150917     C*
012100150917     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
012200150922     C                   if        %subst(IN_TASGVA:1:1) = 'O'
012300150922     C                   eval      KAR5TRD  = 'BNB'
012400150922     C                   clear                   DAR5BNB
012500150922     C     KAR531_P      chain     fiar531c
012600150922     C                   if        %found(fiar531c)
012700150922     C                   movel     AR5UNI        DAR5BNB
012800150922     C                   endif
012900150922     C                   eval      TASNCL = §AR5BCOR                            Colli originali
013000150917     C                   endif
013100150922     C*
013200150922     C* Se particolità varia = 'B' recupero da FIAR5 il numero dei bancali
013300150922     C                   if        %subst(IN_TASGVA:1:1) = 'B'
013400150922     C                   eval      KAR5TRD  = 'BAN'
013500150922     C                   clear                   DAR5BAN
013600150922     C     KAR531_P      chain     fiar531c
013700150922     C                   if        %found(fiar531c)
013800150922     C                   movel     AR5UNI        DAR5BAN
013900150922     C                   endif
014000150922     C                   eval      TASBAN = §AR5NBA + §AR5NB2
014100150922     C                   endif
014200151116     C*
014300151116     C* Se, presente, reperisco dal FIAR5 il flag della "Stampa Packing List"
014400151116     C                   eval      KAR5TRD  = 'GEN'
014500151116     C                   clear                   DAR5GEN
014600151116     C     KAR531_P      chain     fiar531c
014700151116     C                   if        %found(fiar531c)
014800151116     C                   movel     AR5UNI        DAR5GEN
014900151116     C                   if        §AR5ALS = 'S' or §AR5ALX = 'S'
015000151116     C                   eval      TASSPL = 'S'
015100151116     C                   endif
015200151116     C                   endif
015300150922     C*
015400150922     C* Verifico se c'è evento 'RIC'
015500150922     C     KEVB04_P      chain     fnevb04l
015600150922     C                   if        %found(fnevb04l)
015700150922     C                   eval      TASRIC = 'S'
015800150922     C                   endif
015900150922     C*
016000150922     C* Reperimento contrassegno
016100150922     C                   if        IN_TASFCA <> *blanks
016200150922     C                   exsr      RTVCSB
016300150922     C                   endif
016400150917     C*
016500150917     C* Imposto alcuni campi "composti"
016600150922     C                   movel     IN_TASFTC     TASTC1
016700150917     C                   eval      TASDCT = %dec(%date() : *ISO)
016800150922     C                   eval      TASDSP = IN_TASAAS*10000+IN_TASMGS
016900150917     C*
017000150917     C* Pulisco i campi dell'assicurazione se calcolata in fattura
017100150922     C                   if        IN_TASFCM = 'F'
017200150917     C                   clear                   TASIAS
017300150917     C                   clear                   TASVAS
017400150921     C                   endif
017500150917     C*
017600150917     C* Se si tratta di codice bolla che prevede solo una varia, passo solo quella
017700150917     C                   z-add     1             KK
017800150922     C     IN_TASCBO     lookup    CBO(KK)                                05
017900150917     C                   if        *in05
018000150917     C                   eval      TASSVA = CBV(KK)
018100150917     C                   endif
018200150917     C*
018300150917     C* Flag DDT SI/NO
018400150922     C                   if        IN_TASLL1 = 'C' or IN_TASLL1 = 'S'
018500150917     C                   movel     'S'           TASDDT
018600150921     C                   else
018700150917     C                   clear                   TASDDT
018800150921     C                   endif
018900150917     C*
019000150917     C* Flag operativi - TASFLO
019100150917     C                   clear                   DTAS01
019200150922     C                   movel     IN_TASFBR     §asFBR
019300150922     C                   movel     IN_TASCCA     §asCCA
019400150917     C*
019500150917     C* Valorizzo flag email al destinatario
019600150922     C                   movel     IN_TASFLO     DTASFLO
019700150917     C                   move      §floemd       §asemd
019800151116     C                   movel(p)  §floado       TASPRT
019900150917      /free
020000150917       //?Imposto se part.consegna con PinCode
020100150922         IF IN_TASGMA <> *blanks and %lookup(IN_TASGMA:skGMA) > 0;
020200150917           §ASpin = 'S';
020300150917         ENDIF;
020400150917      /end-free
020500150917     C
020600150922     C                   eval      DSTASFLO = DTAS01
020700150922     C* ********
020800151020     C*
020900151020     C* Imposto campi della nuova DS per il calcolo sul Peso Desunto
021000151020     C                   eval      taspPKB = IN_tasPKB
021100151020     C                   if        IN_tasDFT > *zeros
021200151020     C                   eval      taspDTT = IN_tasDFT
021300151020     C                   else
021400151020     C                   eval      taspDTT = datcor
021500151020     C                   endif
021600160503     C*
021700160503     C* Imposto campi della nuova DS per il calcolo sul Volume Desunto
021800160503     C                   eval      taspFVF = IN_tasFVF
021900160503     C                   eval      taspNCR = IN_tasNCR
022000150922     C*
022100150917     C* ***********
022200150921     C* Forzo la tassazione "provvisoria" della bolla corrente
022300150921     C* ***********
022400150917     C                   if        TASSVA = *blanks OR not *in05
022500151020     C                   call      'TNSF22R'
022600150921     C                   parm                    KPJBA
022700150921     C                   parm                    DTAS
022800150921     C                   parm                    DTASV
022900151020     C                   parm                    DTASPES
023000150917     C                   endif
023100090423     C*
023200000124     C                   ENDSR
023300150917     C*---------------------------------------------------------------*
023400150917     C*  RICERCA CONTRASSEGNO
023500150917     C*---------------------------------------------------------------*
023600150917     C     RTVCSB        BEGSR
023700150917     C
023800150922     C     KCSB03        chain     TNCSB03L
023900150922     C                   if        %found(tncsb03l) and
024000150917     C* Solo per stesso tipo bolla
024100150917     C                             tastbl = csbtbl
024200150917     C                   movel     csbsta        TASSTA
024300150917     C                   z-add     csbcas        TASCAS
024400150917     C                   if        csbfus <> *blanks
024500150917     C                   move      csbfus        TASTIC
024600150917     C                   else
024700150917     C                   move      csbtpi        TASTIC
024800150917     C                   endif
024900150917     C* Mittente
025000150917     C                   movel     csbvca        TASVCA
025100150917     C                   z-add     csbcmb        TASCMB
025200150917     C                   endif
025300150917     C*
025400150922     C                   ENDSR
025500150922     C*---------------------------------------------------------------*
025600150922     C*  INIZIALIZZAZIONE VARIABILI
025700150922     C*---------------------------------------------------------------*
025800150922     C     INZVAR        BEGSR
025900150922     C*
026000150922     C                   clear                   DTASFLO
026100150922     C                   clear                   DSTASFLO
026200150922     C                   clear                   DTAS
026300151020     C                   clear                   DTASPES
026400150922     C                   clear                   DTAS01
026500150922     C                   clear                   DTASV
026600150922     C*
026700150922     C                   ENDSR
026800150922     C*---------------------------------------------------------------*
026900150922     C*  VALORIZZAZIOEN CAMPI DS DTAS DA CAMPI TITAS IN INPUT
027000150922     C*---------------------------------------------------------------*
027100150922     C     VALDTAS       BEGSR
027200150922     C*
027300150922     C                   eval      TASTLA  = *blanks
027400150922     C                   eval      TASKSC  = IN_TASKSC
027500150922     C                   eval      TASPKF  = IN_TASPKF
027600150922     C                   eval      TASPKC  = IN_TASPKC
027700150922     C                   eval      TASNCP  = IN_TASNCP
027800150922     C                   eval      TASVLF  = IN_TASVLF
027900150922     C                   eval      TASNCL  = IN_TASNCL
028000160530     C                   eval      TASNCLB = IN_TASNCL
028100150922     C                   eval      TASCTR  = IN_TASCTR
028200150922     C                   eval      TASCTS  = IN_TASCTS
028300150922     C                   eval      TASNZD  = IN_TASNZD
028400150922     C                   eval      TASCAD  = IN_TASCAD
028500150922     C                   eval      TASFIN  = IN_TASFIN
028600150922     C                   eval      TASFDN  = IN_TASFDN
028700150922     C                   eval      TASMCT  = IN_TASMCT
028800150922     C                   eval      TASNZM  = IN_TASNZM
028900150922     C                   eval      TASCAM  = IN_TASCAM
029000150922     C                   eval      TASFAP  = IN_TASFAP
029100150922     C                   eval      TASTSP  = IN_TASTSP
029200150922     C                   eval      TASTC2  = IN_TASTC2
029300150922     C                   eval      TASLNP  = IN_TASLNP
029400150922     C                   eval      TASLNA  = IN_TASLNA
029500150922     C                   eval      TASTBL  = IN_TASTBL
029600150922     C                   eval      TASVAS  = IN_TASVAS
029700150922     C                   eval      TASIAS  = IN_TASIAS
029800150922     C                   eval      TASQFT  = IN_TASQFT
029900150922     C                   eval      TASDIV  = IN_TASDIV
030000150922     C                   eval      TASPVL  = IN_TASPVL
030100160530     C                   eval      TASVLC  = IN_TASVLC
030200160530     C                   eval      TASNCR  = IN_TASNCR
030300160530     C                   eval      TASFVF  = IN_TASFVF
030400160530     C                   eval      TASPKB  = IN_TASPKB
030500150922     C*
030600150922     C                   ENDSR
030700090424     C*----------------------------------------------------*
030800090424     C     *INZSR        BEGSR
030900090424     C*
031000090424     C     *ENTRY        PLIST
031100150922     C                   PARM                    IN_PF_OPZ
031200150922     C                   PARM                    IN_PF_DSTAS
031300150922     C                   PARM                    DTAS
031400150922     C                   PARM                    DTASV
031500151020     C                   PARM                    DTASPES
031600150922     C*
031700150922     C* Calcola la data corrente
031800150917     C                   z-add     *zeros        datcor            8 0
031900150917     C                   eval      datcor = %dec(%date() : *ISO)
032000150917     C* TNCSB03L
032100150922     C     KCSB03        KLIST
032200150922     C                   KFLD                    IN_TASAAS
032300150922     C                   KFLD                    IN_TASLNP
032400150922     C                   KFLD                    IN_TASNRS
032500150922     C                   KFLD                    IN_TASNSP
032600150922     C* FIAR531C Parziale
032700150922     C     KAR531_P      KLIST
032800150922     C                   KFLD                    IN_TASAAS
032900150922     C                   KFLD                    IN_TASLNP
033000150922     C                   KFLD                    IN_TASNRS
033100150922     C                   KFLD                    IN_TASNSP
033200150922     C                   KFLD                    KAR5TRD
033300150917     C* FNEVB04L Parziale
033400150922     C     KEVB04_P      KLIST
033500150922     C                   KFLD                    IN_TASAAS
033600150922     C                   KFLD                    IN_TASLNP
033700150922     C                   KFLD                    IN_TASNRS
033800150922     C                   KFLD                    IN_TASNSP
033900150922     C                   KFLD                    KEVBCEV
034000150922     C* TABEL00F
034100150922     C     KEYTAB        KLIST
034200090424     C                   KFLD                    KTBKUT
034300090424     C                   KFLD                    KTBCOD
034400090424     C*
034500090424     C                   ENDSR
