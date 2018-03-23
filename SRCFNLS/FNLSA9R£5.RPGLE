000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLSA9R *----------------------------------------------------*
000300080729     H*         - RITORNO DATI AL CLIENTE
000400080417      *? Rimane come tipo invio solo il "WW" e "EW". Tutti i riferimenti
000500080417      *? degil altri tipi invio della 3T usati nella 3K vengono ignorati.
000600000000     H*--------------------------------------------------------------*
000700921026     FTABEL00F  UF   E           K DISK
000800050309     Ftigcp01L  IF   E           K DISK
000900941228     FFNBLP55L  UF   E           K DISK
001000051115     FFiar901L  IF   E           K DISK
001100060214     FFiar401L  UF   E           K DISK
001200090603     FFiar501L  UF a E           K DISK
001300990810     FFNEVB01L  IF   E           K DISK
001400131014     FFIARBF2C  IF   E           K DISK
001500000526     F                                     IGNORE(FNARBD00)
001600000526     F                                     IGNORE(FIARBT00)
001700000526     F                                     IGNORE(FNARBP00)
001800131014     F                                     IGNORE(FNARBN00)
001900040715     F***FNDCT02L  IF   E           K DISK
002000990602     FFNLBL01L  IF   E           K DISK
002100060921     Ffnlbl02L  IF   E           K DISK    rename(fnlbl000:fnlbl2)
002200060921     f                                     prefix(ex)
002300971126     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002400001102     FFNBLT01L  IF   E           K DISK
002500060214     FFNBLP67J  IF   E           K DISK
002600010927     F                                     RENAME(FNBLP000:FNBLPJ)
002700010927     FFNBLP01L  UF   E           K DISK
002800010927     F                                     RENAME(FNBLP000:FNBLP1)
002900050606     ftivgd00f  o    e             disk
003000971126     D*--------------------------------------------------------------*
003100981210     D CMD             S             49    DIM(1) CTDATA PERRCD(1)
003200921026     D MSG             S             67    DIM(3) CTDATA PERRCD(1)
003300001031     D COS             S             50    DIM(2) CTDATA PERRCD(1)
003400001102     D COM             S              1    DIM(31)
003500921026     D SK              S              1    DIM(201)
003600970122     D* Schiera per caricamento tipi record estensione bolla
003700970122     D T7K             S              2    DIM(50)
003800970122     D* Schiera per caricamento codici e tipi mancate consegne
003900970122     D C2A             S              3    DIM(200)
004000970122     D T2A             S              1    DIM(200)
004100970305     D D2A             S             75    DIM(200)
004200000525     D FIP             S              3  0 DIM(50)
004300971126      *----------------------------------------------------------------
004400080417      * Controllo e semaforo x scrittura del VAC: TRUL47R
004500080417      *----------------------------------------------------------------
004600080417     D trul47ds      e ds
004700131113      * Note degli Eventi
004800131113     D Devb01        e ds
004900080417     D DVGDFLO       e ds
005000080417     D FNVACDS       e ds
005100090529     D FNVAChgDS     e ds
005200090603     D  DSVAChg        ds
005300090603     d    VacGC1chg                        like(VacGC1)
005400090603     d    VacGC2chg                        like(VacGC2)
005500090603     d    VacFFDchg                        like(VacFFD)
005600090603     d    VacTCRchg                        like(VacTCR)
005700090603     d    VacDCRchg                        like(VacDCR)
005800090603     d    VacHCRchg                        like(VacHCR)
005900090603     d    VacDCMchg                        like(VacDCM)
006000090603     d    VacHMCchg                        like(VacHMC)
006100090603     d    VacTC1chg                        like(VacTC1)
006200090603     d    VacTC2chg                        like(VacTC2)
006300090603     d    VacCCAchg                        like(VacCCA)
006400090603     d    VacDLAchg                        like(VacDLA)
006500090603     d    VacDAGchg                        like(VacDAG)
006600080417      *
006700941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006800941228     D WLBDAT          DS                  INZ
006900941228     D  G02DAT                 1      8  0
007000941228     D  G02INV                 9     16  0
007100941228     D  G02ERR                17     17
007200941228     D  G02TGI                18     22  0
007300900517     D KPJBA         E DS
007400921023     D DS3K          E DS
007500970122     D DS7K          E DS
007600941228     D DS2A          E DS
007700000525     D OG143         E DS
007800000526     D DSBL4J        E DS
007900050606     D fnvagds       E DS
008000040715
008100040715     d Fidn12ds      e ds
008200040715     d  skKey                 26    305    dim(20)
008300040715     d  skAnn                306    325    dim(20)
008400040715     d  skDca                326    485    dim(20)
008500040715     d  skFca                486    545  0 dim(20)
008600040715     d  skDch                546    705  0 dim(20)
008700040715     d  skCch                706    745    dim(20)
008800971126      *----------------------------------------------------------------
008900971126     IAZORG01L  AA
009000971126     I                             P    4    5 0ORGFIL
009100971126     I                                 11   11  ORGFL1
009200000525     I                               3776 3800  ORGDE3
009300090408     IFNARBG00      51
009400090408     IFNARBK00      52
009500920325     C*---------------------------------------------------------------*
009600920812     C* INDICATORI
009700920812     C*---------------------------------------------------------------*
009800921026     C* 01    - NON TUTTI I MEMBRI SONO STATI ALLOCATI
009900921026     C* 30    - LETTURA FLBLP00F
010000921026     C* 31    - LETTURA TABEL00F
010100921026     C* 32    - VEDO SE ESISTE GIA' BOLLA SU FLBLR
010200921026     C* 33    - GIACENZA
010300921026     C* 34    - COSEGNA PARTICOLARE NUMERICA (COD ANOMALIA)
010400970122     C* 35    - TROVATE ESTENSIONI BOLLE X MANCATE CONSEGNE
010500970123     C* 99    - ESEGUO CHIUSURA E DISALLOCAZIONE MEMBRO FNVAC
010600920812     C*---------------------------------------------------------------*
010700000000     C     *ENTRY        PLIST
010800000000     C                   PARM                    KPJBA
010900980407     C                   MOVEL     KPJBU         WTPINV            2
011000921026     C                   Z-ADD     1             CODUT
011100900511     C*---------------------------------------------------------------*
011200080417      *?Prima di iniziare, controlla se è possibile Procedere mediante  ?
011300080417      *? un controllo semaforico fatto con il TRUL47R.                  ?
011400080417      *  Se la strada è libera si può procedere altrimenti si va a fine.
011500080417     C*---------------------------------------------------------------*
011600080417      *? Come prima cosa avvio il blocco elaborazione TIVGD x tipo file ?
011700080417     C*---------------------------------------------------------------*
011800080417     C                   clear                   trul47ds
011900080417     C                   eval      d47opz  = 'I'
012000080417     C                   eval      d47tip  = 'VC'
012100080417     C                   eval      d47lck  = 'N'
012200121023     C*****  Gurrieri indica di modificare questo flag
012300121023     C*****              eval      d47chkj = 'N'
012400121023     C                   eval      d47chkj = 'S'
012500080930     C     WTPINV        IFEQ      'E'
012600121106     C                   eval      d47pgm  = 'FNLSA9§'
012700080930     c                   else
012800080417     C                   eval      d47pgm  = 'FNLSA9R'
012900080930     c                   end
013000080417     C                   call      'TRUL47R'
013100080417     C                   parm                    trul47ds
013200080417      *
013300080417      *  se strada bloccata si va a fine pgm
013400080417     C                   if        d47sts = 'A'
013500080417     c                   goto      Vai_a_fine
013600080417     c                   end
013700080417     C*---------------------------------------------------------------*
013800080417      *?  Definizione Campi e Caricamento Tabelle   ?
013900080417     C*---------------------------------------------------------------*
014000080417      *
014100060921     C     Klbl2         KLIST
014200060921     C                   KFLD                    klb2_AAS
014300060921     C                   KFLD                    klb2_LNP
014400060921     C                   KFLD                    klb2_NRS
014500060921     C                   KFLD                    klb2_NSP
014600060921     C     Klbl2p        KLIST
014700060921     C                   KFLD                    exLBLAAp
014800060921     C                   KFLD                    exLBLLPp
014900060921     C                   KFLD                    exLBLNRp
015000060921     C                   KFLD                    exLBLNSp
015100060921     C     KBLP          KLIST
015200060921     C                   KFLD                    BLPAAS
015300060921     C                   KFLD                    BLPLNP
015400060921     C                   KFLD                    BLPNRS
015500060921     C                   KFLD                    BLPNSP
015600060214     C     Kar4          KLIST
015700970122     C                   KFLD                    BLPAAS
015800970122     C                   KFLD                    BLPLNP
015900970122     C                   KFLD                    BLPNRS
016000970122     C                   KFLD                    BLPNSP
016100970122     C                   KFLD                    KTRC
016200921026     C     KBLP2         KLIST
016300921026     C                   KFLD                    COD99             7 0
016400921026     C                   KFLD                    KSC
016500920316     C     KTAB          KLIST
016600921026     C                   KFLD                    CODUT             1 0
016700921026     C                   KFLD                    COD               2
016800941228     C     KTAB2         KLIST
016900941228     C                   KFLD                    CODUT             1 0
017000941228     C                   KFLD                    COD               2
017100941228     C                   KFLD                    KEY               8
017200090529     C     Kar5          KLIST
017300090529     C                   KFLD                    BLPAAS
017400090529     C                   KFLD                    BLPLNP
017500090529     C                   KFLD                    BLPNRS
017600090529     C                   KFLD                    BLPNSP
017700090529     C                   KFLD                    Kar5vac           3
017800941228     C**
017900941228     C* DEFINIZIONE CAMPI
018000941228     C**
018100941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
018200060214     C     *LIKE         DEFINE    ar4TRC        KTRC
018300051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
018400051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
018500051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
018600000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
018700000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
018800000526     C     *LIKE         DEFINE    VACDAG        PTDTG
018900060921      *
019000060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
019100060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
019200060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
019300060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
019400941228     C*---------------------------------------------------------------*
019500921026     C* GIRO UDATE
019600941228     C                   TIME                    W0140            14 0
019700941228     C* UDATE IN GGMMAAAA
019800941228     C                   MOVE      W0140         WDTGIO            8 0
019900090529     C                   MOVEl     W0140         Orario            6 0
020000941228     C*
020100941228     C* UDATE IN AAAAMMGG
020200941228     C                   Z-ADD     WDTGIO        G02DAT
020300941228     C                   MOVEL     *BLANK        G02ERR
020400941228     C                   CALL      'XSRDA8'
020500941228     C                   PARM                    WLBDAT
020600941228     C                   MOVEL     G02INV        DATEU             8 0
020700941228     C*---------------------------------------------------------------*
020800000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
020900000525     C                   CLEAR                   FIP
021000000525     C                   Z-ADD     0             Y
021100000525     C     *LOVAL        SETLL     AZORG01L
021200000525     C                   DO        *HIVAL
021300000525     C                   READ      AZORG01L                               95
021400000525     C   95              LEAVE
021500000525     C                   MOVEL     ORGDE3        OG143
021600020729     C     §OgNTW        IFEQ      'PPT'
021700000525     C                   Z-ADD     1             Y
021800000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
021900000525     C   23              Z-ADD     ORGFIL        FIP(Y)
022000000525     C                   END
022100000525     C                   END
022200000525     C*---------------------------------------------------------------*
022300970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
022400970122     C                   Z-ADD     0             Y                 4 0
022500970122     C                   MOVEA     *HIVAL        T7K
022600970122     C                   MOVEL     '7K'          COD
022700970122     C     KTAB          CHAIN     TABEL                              31
022800970122     C     *IN31         DOWEQ     '0'
022900970122     C     TBLFLG        IFEQ      ' '
023000970122     C* Controllo se impostato tipo mancata consegna
023100970122     C                   MOVEL     TBLUNI        DS7K
023200970122     C     §7KTMC        IFNE      *BLANKS
023300970122     C                   ADD       1             Y
023400970122     C                   MOVEL     §7KTMC        T7K(Y)
023500970123     C                   MOVEL     TBLKEY        WTRC              1
023600970123     C                   MOVE      WTRC          T7K(Y)
023700970122     C                   END
023800970122     C                   END
023900970122     C     KTAB          READE     TABEL                                  31
024000970122     C                   END
024100970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
024200970122     C     Y             IFGT      0
024300970122     C                   SORTA     T7K
024400970122     C                   END
024500970122     C*---------------------------------------------------------------*
024600970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
024700970122     C                   Z-ADD     0             W                 4 0
024800970122     C                   MOVEA     *HIVAL        C2A
024900970305     C                   MOVEA     *HIVAL        D2A
025000970122     C                   MOVEA     *HIVAL        T2A
025100970122     C                   MOVEL     '2A'          COD
025200970122     C     KTAB          CHAIN     TABEL                              31
025300970122     C     *IN31         DOWEQ     '0'
025400970122     C     TBLFLG        IFEQ      ' '
025500970122     C* Caricamento evento + tipo mancata consegna
025600970122     C                   MOVEL     TBLUNI        DS2A
025700970122     C     §2AFTC        IFNE      *BLANKS
025800970122     C                   ADD       1             W
025900970122     C                   MOVEL     §2AFTC        T2A(W)
026000970305     C                   MOVEL     §2ADES        D2A(W)
026100970123     C                   MOVEL     TBLKEY        C2A(W)
026200970122     C                   END
026300970122     C                   END
026400970122     C     KTAB          READE     TABEL                                  31
026500970122     C                   END
026600080417     C*---------------------------------------------------------------*
026700080417      * ?  CICLO Principale del Programma              ?
026800970122     C*---------------------------------------------------------------*
026900080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
027000080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
027100921023     C                   MOVEL     '3K'          COD
027200921023     C     KTAB          SETLL     TABEL
027300921023     C     KTAB          READE     TABEL                                  31
027400921023     C*
027500921023     C     *IN31         DOWEQ     '0'
027600131113      *
027700921023     C     TBLFLG        IFEQ      ' '
027800980407     C                   MOVEL     TBLUNI        DS3K
027900080417      * ?  Se Estero:  ?
028000980409     C     WTPINV        IFEQ      'E'
028100080417     C     §3KCSD        ANDEQ     'EW'
028200980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
028300080417      * ?  oppure se NON Estero e non EDI:  ?
028400980409     C     WTPINV        ORNE      'E'
028500080417     C     §3KCSD        ANDNE     'EW'
028600091116     C     §3KCSD        ANDNE     *blank
028700091116      *
028800921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
028900921026     C                   MOVEL     TBLKEY        KSC               7 0
029000930811     C                   MOVEL     KSC           COD99
029100930811     C                   MOVE      9999          COD99
029200011002     C                   MOVEL     KSC           KSCJ              7
029300930811     C*
029400941228     C     KSC           SETLL     FNBLP000                               32
029500941228     C  N32KBLP2         SETLL     FNBLP000                               32
029600060214     C  N32KSCJ          SETLL     FNBLP67J                               32
029700921023     C*
029800930811     C     *IN32         IFEQ      *ON
029900131113     C*                                                SALVO LA KEY
030000941228     C                   MOVEL     TBLKEY        SAVKEY
030100930811     C                   EXSR      ELAB
030200941228     C*
030300131113     C* RICHAINO IL RECORD DI TABEL
030400941228     C                   MOVEL     '3K'          COD
030500941228     C                   MOVEL     SAVKEY        KEY
030600941228     C     KTAB2         CHAIN     TABEL                              31
030700930811     C                   Z-ADD     DATEU         §3KCDU
030800930811     C                   MOVEL     DS3K          TBLUNI
030900930811     C                   UPDATE    TABEL
031000131113      *
031100921026     C                   END
031200131113      *
031300921023     C                   END
031400980409     C                   END
031500921023     C*
031600921023     C     KTAB          READE     TABEL                                  31
031700921023     C                   END
031800981210     C*
031900981210     C* CALCELLO LE OVR ESEGUITE
032000981210     C                   MOVEL     *BLANKS       COMMAN           80
032100981210     C                   Z-ADD     17            LUNG
032200981210     C                   MOVEA     CMD(1)        COMMAN
032300981210     C                   CALL      'QCMDEXC'                            21
032400981210     C                   PARM                    COMMAN
032500080417     C                   PARM                    LUNG             15 5
032600080417      **
032700080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
032800980409     C   06WTPINV        IFEQ      'E'
032900110419     C                   CALL      'TRTCT80R'
033000030924     C                   parm                    Kpjba
033100980409     C                   END
033200080417     C*
033300080417      *? Controllato se non si è potuto procedere, si è arrivati qui.  ?
033400080417     c     Vai_a_fine    tag
033500080417     C*
033600080417      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
033700080417     C                   clear                   trul47ds
033800080417     C                   eval      d47opz  = 'F'
033900080417     C                   eval      d47tip  = 'VC'
034000080417     C                   call      'TRUL47R'
034100080417     C                   parm                    trul47ds
034200080417     C*
034300921023     C                   SETON                                        LR
034400080417      *================================================================
034500921023     C* ELABORO SPEDIZIONI DEL CLIENTE -------------------------------*
034600080417      *================================================================
034700921023     C     ELAB          BEGSR
034800040319     C*
034900941228     C     KSC           SETLL     FNBLP000
035000941228     C     KSC           READE     FNBLP000                               30
035100921026     C*
035200921026     C     *IN30         DOWEQ     '0'
035300921026     C                   EXSR      CONTR
035400941228     C     KSC           READE     FNBLP000                               30
035500921026     C                   END
035600921026     C*
035700921026     C* LEGGO I 9999 CODIFICATI
035800921026     C*
035900941228     C     KBLP2         SETLL     FNBLP000
036000941228     C     KBLP2         READE     FNBLP000                               30
036100941228     C*
036200921026     C     *IN30         DOWEQ     '0'
036300921026     C                   EXSR      CONTR
036400941228     C     KBLP2         READE     FNBLP000                               30
036500921026     C                   END
036600040319     C*
036700010927     C* INSERISCO LOOP SU FILE DI JOIN PER VEDERE SE ESISTONO RECORD
036800060214     C* SU ar4 CON CHIAVE KSC IN ar4NOT
036900010927      *
037000060214     C     KSCJ          SETLL     FNBLP67J
037100010927     C                   DO        *HIVAL
037200060214     C     KSCJ          READE     FNBLP67J                               66
037300010927     C   66              LEAVE
037400010927     C     KBLP          CHAIN     FNBLP01L                           67
037500010927     C     *IN67         IFEQ      '0'
037600010927     C                   SETON                                        68
037700010927     C                   EXSR      CONTR
037800010927     C                   SETOFF                                       68
037900010927     C                   ENDIF
038000010927     C                   ENDDO
038100040319     C*
038200010927     C                   SETOFF                                       68
038300040319     C*
038400921026     C                   ENDSR
038500921026     C*
038600921026     C* VEDO SE ELABORARE BOLLA --------------------------------------*
038700921026     C     CONTR         BEGSR
038800990602     C*
038900990602     C* Escludo bolle figlie xchè mando già via i dati tramite la mamma
039000131113     C     KBLP          CHAIN     FNLBL01L                           23
039100921026     C*
039200040319     C*  Se non è una bolla dirottata in quanto non è figlia di
039300040319     C*   un'altra bolla......
039400990602     C     *IN23         IFEQ      '1'
039500040319     C*   Oppure.....
039600040319     C*   .... se trovato il legame deve essere un dirottamento legato
039700040319     C*       non all'Estero e deve essere un discorso di reso/dirottam
039800040319     C*        Italia
039900040319     C     *in23         oreq      *off
040000040319     C     wtpINV        andne     'E'
040100040319     C     §3kcsd        andne     'EW'
040200040319     C     lblLAN        andne     lblLAP
040300040319     C     blpCCA        andne     *blank
040400040319     C*
040500050309     C     KBLP          CHAIN     tigcp000                           33
040600990602     C* 33 OFF - GIACENZA
040700961204     C*  Se giacenza bolla estero mando la data giacenza solo quando
040800961204     C*  l'ufficio internazionale mi ha scritto l'ulteriore motivaz.
040900961204     C*  in inglese
041000971126     C     *IN33         IFEQ      '0'
041100971126     C                   MOVEL     *BLANKS       ORGFL1
041200971126     C     GCPLNP        CHAIN     AZORG01L                           36
041300020717     C  n36              MOVEL     ORGDE3        OG143
041400020717     C  n36§OgNTW        ifeq      'FED'
041500020717     C     §OgNTW        oreq      'EEX'
041600020717     C     §OgNTW        oreq      'EUP'
041700020717     C     GCPFAS        Iflt      15
041800961204     C                   SETON                                        33
041900020717     C                   End
042000961204     C                   END
042100971126     C                   END
042200941228     C*
042300990602     C* Controllo se esiste una pratica di danno aperta
042400990602     C                   EXSR      CRTCA
042500941228     C*
042600060921      **?Attenzione:  a fronte di Bolla con Bolle figlie legate su cui
042700060921      *   è presente il Contrassegno, si vuole inviare al cliente
042800060921      *   l'info del contrassegno sul VAC della bolla madre.
042900060921      **?  Regola: il contrassegno si trova sull'ultima figlia della catena
043000060921      * ? o sulla penultima se l'ultima è una Locale.
043100060921      *    Occorre agganciare il file Contrassegni trovando prima la
043200060921      *    chiave della bolla su cui si dovrebbe trovare il contrassegno.
043300060921     c                   exsr      ChecK_figlie
043400060921      *   All'uscita della Routine è impostata la chiave della bolla valida
043500060921      *   x verificare la presenza del contrassegno sulla spedizione.
043600060921      *
043700060921     C     Klbl2         CHAIN     Fiar9000                           34
043800060921      *
043900941228     C     *IN34         IFEQ      *ON
044000051115     C                   CLEAR                   ar9CAS
044100051115     C                   CLEAR                   ar9VCA
044200051115     C                   CLEAR                   ar9TIC
044300941228     C                   ENDIF
044400941228     C*
044500000525      * Se la LNP della spedizione è una filiale POSTE richiamo
044600000525      * routine x reperire eventuale C/assegno e la data di giacenza
044700000525      * dalla data dell' evento
044800000526     C                   EXSR      POSTE
044900090408      *-------
045000090408      * ?  O x qualsiasi Variazione Bolla inerente alla Data Cons.Richiesta  ?
045100090408     c                   move      'N'           VariataOggi       1
045200090408     C     §3KCQ8        ifEQ      'S'
045300131014     c                   setoff                                       51
045400131014     C     KBLP          setll     FIARBF2C
045500131014     C     KBLP          reade     FIARBF2C
045600131014     C                   dow       not %Eof(FIARBF2C)
045700090408     c                   if        *in51
045800090408     c                   if        arbDTV = dateu
045900090408     c                   if        arbCVB = 'CR' or
046000090408     c                             arbCVB = 'CS'
046100090408     c                   move      'S'           VariataOggi
046200090408     c                   leave
046300090408     c                   end
046400090408     c                   end
046500090408     c                   end
046600131014     c                   setoff                                       51
046700131014     C     KBLP          reade     FIARBF2C
046800090408     C                   end
046900090408     C                   ENDIF
047000090408      *-------
047100941228     C* CERCO EVENTO LASCIATO AVVISO
047200941228     C                   CLEAR                   WDLA
047300941228     C                   MOVEL     '2A'          COD
047400941228     C*
047500131113     C     kBLP          SETGT     FNEVB000
047600131113     C     kBLP          READPE    FNEVB000                               34
047700941228     C*
047800941228     C     *IN34         DOWEQ     *OFF
047900131113     C*
048000131113      **  imposta sulla DS le note x testare il flag Progressivo
048100131113     c                   movel     evbNOT        Devb01
048200131113      *
048300131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
048400131113     c                   if        wTPinv <> 'E'
048500131113     c                               or
048600131113     c                             wTPinv = 'E' and
048700131113     c                             (§notPRG = ' ' or §notPRG = 'U')
048800131113      **
048900941228     C                   MOVEL(P)  EVBCEV        KEY
049000941228     C*
049100941228     C     KTAB2         CHAIN     TABEL                              30
049200941228     C     *IN30         IFEQ      *OFF
049300040319     C                   MOVEL     TBLUNI        DS2A
049400941228     C     §2AFTC        IFEQ      'A'
049500941228     C                   Z-ADD     EVBDEV        WDLA              8 0
049600941228     C                   SETON                                        34
049700941228     C                   ENDIF
049800941228     C                   ENDIF
049900941228     C*
050000131113     c                   endif
050100131113     C*
050200131113     C  N34kBLP          READPE    FNEVB000                               34
050300941228     C                   ENDDO
050400921026     C*
050500970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
050600970721     C                   MOVEL     'N'           WRTVAG            1
050700970121     C                   MOVEL     'S'           WFLDT3            1
050800970121     C                   MOVEL     'S'           WINVAC            1
050900040319      *
051000970121     C     §3KNMC        IFEQ      'S'
051100970121     C     BLPDCM        ANDEQ     0
051200011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
051300060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
051400011002     C     *IN68         ANDEQ     '0'
051500040319      *
051600040319      * ....ma anche
051700040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
051800040319      *
051900040319     C     §3kNMC        orEQ      'S'
052000040319     C     blpDCM        andGT     0
052100040319     C     blpCCA        andNE     *blank
052200040319     C     *in68         andEQ     *off
052300040319      *
052400970121     C                   EXSR      TSTNMC
052500970121     C                   END
052600001031     C*
052700001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
052800001031      *  PARZIALE E VERIFICA DELLE DATE
052900001106     C                   MOVEL     'N'           WTRPAR            1
053000131113      **
053100001031     C     §3KCQ6        IFEQ      'S'
053200001031     C     BLPDCM        ANDEQ     0
053300001031     C     BLPDCP        ANDGT     0
053400011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
053500060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
053600011002     C     *IN68         ANDEQ     '0'
053700001031     C                   EXSR      PARZIA
053800001031     C                   END
053900131113      **
054000980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
054100980318     C                   MOVEL     'N'           WTRMIC
054200060912     C                   MOVEL     'N'           WTRRIP
054300990602     C                   Z-ADD     0             WDTMIC            8 0
054400131113      **
054500131113      **  Non Consegnata e si deve inviare il MIC o la Data Consegna
054600131113      **      se si tratta dell'ULTIMO EVENTO registrato
054700980406     C     §3KCQ3        IFEQ      'S'
054800980318     C     BLPDCM        ANDEQ     0
054900060912     C     §3KCQ1        orEQ      'S'
055000060912     C     BLPDCM        ANDEQ     0
055100131113      **
055200131113     C     kBLP          SETGT     FNEVB000
055300131113     C     kBLP          READPE    FNEVB000                               34
055400131113      *  Cercando sempre l'ultimo evento NON deve fare distinzioni
055500131113      *   fra Italia o Estero
055600131113      *
055700980318     C     *IN34         IFEQ      *OFF
055800060912     C     EVBCEV        ifEQ      'MIC'
055900060912     C     §3KCQ3        andeq     'S'
056000980318     C                   MOVEL     'S'           WTRMIC            1
056100990602     C                   Z-ADD     EVBDEV        WDTMIC
056200060912     c                   else
056300060912      *ripristino
056400060912     C     EVBCEV        ifeq      'RIP'
056500060912     C     EVBCEV        oreQ      'RDC'
056600060912     C                   MOVEL     'S'           WTRRip            1
056700060912     C                   endif
056800060912      *
056900060912     C                   endif
057000980318     C                   END
057100131113      *
057200980318     C                   END
057300921026     C* DATA CONSEGNA
057400930809     C     §3KCQ1        IFEQ      'S'
057500921026     C     BLPDCM        ANDNE     0
057600080410      *
057700010202     C     §3KCQ1        OREQ      'S'
057800010202     C     §3KCSD        ANDEQ     'EW'
057900010202     C     BLPDCM        ANDEQ     0
058000010202     C     BLPDCP        ANDNE     0
058100080410      *
058200930809     C* DATA LASCIATO AVVISO
058300930809     C     §3KCQ2        OREQ      'S'
058400941228     C     WDLA          ANDGT     0
058500080410      *
058600930809     C* ANOMALIE
058700930809     C     §3KCQ4        OREQ      'S'
058800930809     C     BLPCCA        ANDNE     *BLANKS
058900080410      *
059000990602     C* ANOMALIE DANNI
059100990602     C     §3KCQ4        OREQ      'S'
059200990602     C     DATMAX        ANDNE     0
059300080410      *
059400930809     C* GIACENZA
059500930809     C     §3KCQ5        OREQ      'S'
059600930809     C     *IN33         ANDEQ     *OFF
059700080410      *
059800970123     C* MANCATE CONSEGNE
059900970123     C     §3KNMC        OREQ      'S'
060000970123     C     BLPDCM        ANDEQ     0
060100080410      *
060200980318     C* Messa in consegna
060300980318     C     §3KCQ3        OREQ      'S'
060400980318     C     WTRMIC        ANDEQ     'S'
060500080410      *
060600060912     C* Messa in consegna
060700060912     C     §3KCQ1        OREQ      'S'
060800060912     C     WTRrip        ANDEQ     'S'
060900060912     C     BLPDCM        ANDEQ     0
061000080410      *
061100090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
061200080410     C     §3KCQ8        OREQ      'S'
061300090408     c     VariataOggi   andEQ     'S'
061400090408     C     BLPDCR        ANDGT     0
061500080721     C*
061600080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
061700080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
061800080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
061900921026     C*
062000970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
062100970123     C* consegna o se li devo trasmettere e li ho trovati
062200970123     C     §3KNMC        IFNE      'S'
062300970123     C     WINVAC        OREQ      'S'
062400990624     C* ANOMALIE DANNI
062500990624     C     §3KCQ4        OREQ      'S'
062600990624     C     DATMAX        ANDNE     0
062700990624     C* Messa in consegna
062800990624     C     §3KCQ3        OREQ      'S'
062900990624     C     WTRMIC        ANDEQ     'S'
063000060912     C* Evento di ripristino
063100060912     C     §3KCQ1        OREQ      'S'
063200060912     C     WTRrip        ANDEQ     'S'
063300080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
063400080721     C     §3KCQ8        OREQ      'S'
063500080721     C     BLPDCR        ANDGT     0
063600090408     c     VariataOggi   andEQ     'S'
063700930809     C                   EXSR      CARBLR
063800970123     C                   END
063900080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
064000970123     C*
064100930809     C                   END
064200990602     C                   END
064300921026     C*
064400970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
064500970123     C* consegna o se li devo trasmettere e li ho trovati
064600970123     C     §3KNMC        IFNE      'S'
064700090527      *
064800970123     C     WFLDT3        OREQ      'S'
064900090527      *
065000090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
065100090527      *  fino ad un successivo Evento sulle bolle figlie)
065200090527     C     §3KCQ4        OREQ      'S'
065300090527     C     BLPCCA        ANDNE     *BLANKS
065400090527      *
065500990624     C* ANOMALIE DANNI
065600990624     C     §3KCQ4        OREQ      'S'
065700990624     C     DATMAX        ANDNE     0
065800990624     C* Messa in consegna
065900990624     C     §3KCQ3        OREQ      'S'
066000990624     C     WTRMIC        ANDEQ     'S'
066100001106     C* CONSEGNA PARZIALE
066200001106     C     §3KCQ6        OREQ      'S'
066300001106     C     WTRPAR        ANDEQ     'S'
066400080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
066500080729     C     §3KCQ8        OREQ      'S'
066600080729     C     BLPDCR        ANDGT     0
066700090408     c     VariataOggi   andEQ     'S'
066800080417      *
066900921026     C                   MOVEL     'T'           BLPFT3
067000941228     C                   MOVEL     DATEU         BLPDT3
067100080417      *
067200010927     C  N68              UPDATE    FNBLP000
067300010927     C   68              UPDATE    FNBLP1
067400970123     C                   END
067500921026     C*
067600921026     C                   ENDSR
067700000526     C*
067800060921      **?-------------------------------------------------------------*
067900060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
068000060921      **?-------------------------------------------------------------*
068100060921     C     ChecK_Figlie  BEGSR
068200060921      *
068300060921      *  inizializza flag di work della Routine
068400060921     c                   move      'S'           Primo_legame      1
068500060921      *
068600060921      **?chiave salvata della Bolla x partire nella ricerca
068700060921     c                   z-add     blpAAS        klb2_AAS
068800060921     c                   z-add     blpLNP        klb2_LNP
068900060921     c                   z-add     blpNRS        klb2_NRS
069000060921     c                   z-add     blpNSP        klb2_NSP
069100060921      *
069200060921      *  Chiave Bolla Precedente
069300060921     c     new_ciclo     tag
069400060921      **?                                  Flag di work del ciclo
069500060921     c                   move      'N'           ha_figlia         1
069600060921     C     Klbl2         setLL     FnLBL02L
069700060921     C     Klbl2         reade     FnLBL02L
069800060921      *
069900060921     c                   Dow       not %EoF(FnLBL02L)
070000060921      **?primo controllo:
070100060921      *    tutta la catena deve partire dalla bolla mamma che è
070200060921      *     uguale alla bolla originale del primo record della catena
070300060921     c                   if          Primo_legame = 'S'
070400060921     c                   eval      Primo_legame = 'N'
070500060921     c                   if          blpAAS <> exLBLAAo   or
070600060921     c                               blpLNP <> exLBLLPo   or
070700060921     c                               blpNRS <> exLBLNRo   or
070800060921     c                               blpNSP <> exLBLNSo
070900060921      *    se così non è non deve saltare il ciclo e non impostare i
071000060921      *     campi chiave x il contrassegno.
071100060921     c                   leave
071200060921     c                   end
071300060921     c                   end
071400060921      **?secondo controllo:
071500060921      *     non deve essere una bolla locale
071600060921      *   (sulle locali non c'è l'informazione di contrassegno)
071700060921     c                   if        exLBLLPN <> exLBLLPP
071800060921      *  imposta la chiave dell'ultima figlia
071900060921      *       per il prossimo giro di controllo
072000060921     c                   z-add     exLBLAAN      klb2_AAS
072100060921     c                   z-add     exLBLLPN      klb2_LNP
072200060921     c                   z-add     exLBLNRN      klb2_NRS
072300060921     c                   z-add     exLBLNSN      klb2_NSP
072400060921     c                   move      'S'           ha_figlia
072500060921     c                   end
072600060921      *
072700060921      **?   (attenzione a non sposizionarsi)
072800060921      * Chiave impostata come Bolla Precedente appena letta
072900060921     C     KLBL2P        reade     FnLBL02L
073000060921     c                   end
073100060921      *
073200060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
073300060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
073400060921      *   se invece non ha trovato delle figlie allora va a fine con la
073500060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
073600060921     c                   if        ha_figlia = 'S'
073700060921     c                   if        exLBLAAp <> klb2_AAS or
073800060921     c                             exLBLLPp <> klb2_LNP or
073900060921     c                             exLBLNRp <> klb2_NRS or
074000060921     c                             exLBLNSp <> klb2_NSP
074100060921     c                   goto      new_ciclo
074200060921     c                   end
074300060921     c                   end
074400060921      *
074500060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
074600060921      **?   x verificare la presenza del contrassegno su AR9
074700060921      *
074800060921     C                   ENDSR
074900060921      **?-------------------------------------------------------------*
075000000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
075100921026     C*
075200131119     C*   per quel che riguarda le POSTE
075300131119     C*    NON è stato adeguato alla logica degli eventi passati dal PDA
075400131119     C*     quindi con i flag del progressivo 'P' e 'U'
075500131119     C*
075600000526     C     POSTE         BEGSR
075700000526      *
075800000526     C                   CLEAR                   PTTIC
075900000526     C                   CLEAR                   PTCAS
076000000526     C                   CLEAR                   PTVCA
076100000526     C                   CLEAR                   PTIAS
076200000526     C                   CLEAR                   PTVAS
076300000526     C                   CLEAR                   PTDTG
076400000526     C                   CLEAR                   §POST
076500000526      *
076600000526     C                   SETOFF                                       22
076700000526     C     BLPLNP        LOOKUP    FIP                                    22
076800000526     C   22              MOVEL     'S'           §POST             1
076900131113      *
077000000526     C     §POST         IFEQ      'S'
077100131113      *
077200000526      * Se la bolla è stata chiusa con consegna anomala la merce
077300000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
077400000526      * questo è stato annullato in quanto non più incassabile da
077500000526      * Bartolini.
077600000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
077700000526      * informazione da ritornare alla SDA
077800000526     C     BLPCCA        IFNE      *BLANK
077900051115     C     ar9CAS        ANDEQ     *ZEROS
078000000526     C     KBLP          CHAIN     FNARBK00                           22
078100000526     C     *IN22         IFEQ      '0'
078200000526     C                   MOVEL     ARBTIC        PTTIC
078300000526     C                   Z-ADD     ARBCAS        PTCAS
078400000526     C                   MOVEL     ARBVCA        PTVCA
078500000526     C                   ENDIF
078600000526     C                   ENDIF
078700000526      *
078800060214      * Prendo importo da assicurare da Fiar4 t.r.=J
078900000526     C                   MOVEL     'J'           KTRC
079000060214     C     Kar4          CHAIN     Fiar401L                           35
079100000526     C     *IN35         IFEQ      '0'
079200060214     C                   MOVEL     ar4NOT        DSBL4J
079300000526     C                   Z-ADD     §B4IAS        PTIAS
079400000526     C                   MOVEL     §B4VAS        PTVAS
079500000526     C                   ENDIF
079600000526      *
079700000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
079800000526      * gestite le giacenze per le bolle poste
079900000526      *
080000131113     C     kBLP          SETGT     FNEVB000
080100131113     C     kBLP          READPE    FNEVB000                               34
080200000526      * Devo cercare l'ultimo evento di tipo giacenza
080300000526      *
080400000526     C     *IN34         DOWEQ     *OFF
080500000526     C                   Z-ADD     1             WW
080600000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
080700131119     c                   if         *in42 and T2A(WW) ='G'
080800000526     C                   Z-ADD     EVBDEV        PTDTG
080900000619     C                   LEAVE
081000131119     C                   ENDIF
081100131119      *
081200131113     C     kBLP          READPE    FNEVB000                               34
081300000526     C                   ENDDO
081400000526     C                   ENDIF
081500000526      *
081600000526     C                   ENDSR
081700000526     C*
081800941228     C* CREO RECORD FNVAC01L -----------------------------------------*
081900921026     C     CARBLR        BEGSR
082000080417      *
082100080417     c                   clear                   fnvacds
082200080417      *
082300941228     C                   MOVEL     BLPAAS        VACAAS
082400941228     C                   MOVEL     BLPLNP        VACLNP
082500941228     C                   MOVEL     BLPMGS        VACMGS
082600941228     C                   MOVEL     BLPNRS        VACNRS
082700941228     C                   MOVEL     BLPNSP        VACNSP
082800941228     C                   MOVEL     BLPCBO        VACCBO
082900941228     C                   MOVEL     BLPLNA        VACLNA
083000941228     C                   MOVEL     BLPRSD        VACRSD
083100941228     C                   MOVEL     BLPPRD        VACPRD
083200941228     C                   MOVEL     BLPGC1        VACGC1
083300941228     C                   MOVEL     BLPGC2        VACGC2
083400941228     C                   MOVEL     BLPCTR        VACCTR
083500941228     C                   MOVEL     BLPCTS        VACCTS
083600941228     C                   MOVEL     BLPFTM        VACFTM
083700941228     C                   MOVEL     BLPFIN        VACFIN
083800941228     C                   MOVEL     BLPFAP        VACFAP
083900941228     C                   MOVEL     BLPTSP        VACTSP
084000000526      *
084100000526     C     §POST         IFEQ      'S'
084200000526     C                   MOVEL     PTIAS         VACIAS
084300000526     C                   MOVEL     PTVAS         VACVAS
084400000526     C                   ELSE
084500000526     C                   MOVEL     BLPIAS        VACIAS
084600000526     C                   MOVEL     BLPVAS        VACVAS
084700000526     C                   ENDIF
084800000526      *
084900941228     C                   MOVEL     BLPNAS        VACNAS
085000941228     C                   MOVEL     BLPNCL        VACNCL
085100941228     C                   MOVEL     BLPPKF        VACPKB
085200941228     C                   MOVEL     BLPVLF        VACVLB
085300941228     C                   MOVEL     BLPQFT        VACQFT
085400000526      *
085500000526     C     §POST         IFEQ      'S'
085600051115     C     ar9CAS        ANDEQ     *ZEROS
085700000526     C                   MOVEL     PTCAS         VACCAS
085800000526     C                   MOVEL     PTVCA         VACVCA
085900000526     C                   MOVEL     PTTIC         VACTIC
086000000526     C                   ELSE
086100051115     C                   MOVEL     ar9CAS        VACCAS
086200051115     C                   MOVEL     ar9VCA        VACVCA
086300051115     C                   MOVEL     ar9TIC        VACTIC
086400000526     C                   ENDIF
086500000526      *
086600060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
086700011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
086800011002      *
086900011002     C     *IN68         IFEQ      '1'
087000060214     C                   MOVEL     ar4KSC        VACCCM
087100011002     C                   ELSE
087200921026     C     BLPCCM        IFNE      0
087300941228     C                   MOVEL     BLPCCM        VACCCM
087400921026     C                   ELSE
087500941228     C                   MOVEL     BLPKSC        VACCCM
087600921026     C                   END
087700011002     C                   END
087800131113      *
087900941228     C                   MOVEL     BLPRMN        VACRMN
088000941228     C                   MOVEL     BLPRMA        VACRMA
088100941228     C                   MOVEL     BLPRMO        VACRMO
088200941228     C                   MOVEL     BLPFFD        VACFFD
088300131113      *
088400000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
088500000921     C     §POST         IFEQ      'S'
088600000921     C                   Z-ADD     BLPNCD        VACDCR
088700000921     C                   ELSE
088800000921     C                   MOVEL     BLPDCR        VACDCR
088900000921     C                   ENDIF
089000941228     C                   MOVEL     BLPTCR        VACTCR
089100941228     C                   MOVEL     BLPHCR        VACHCR
089200941228     C                   MOVEL     BLPTC1        VACTC1
089300941228     C                   MOVEL     BLPTC2        VACTC2
089400941228     C                   MOVEL     BLPDCM        VACDCM
089500941228     C                   MOVEL     BLPHMC        VACHMC
089600941228     C                   MOVEL     BLPCCA        VACCCA
089700941228     C                   MOVEL     WDLA          VACDLA
089800000526      *
089900000526     C     §POST         IFEQ      'S'
090000000526     C                   MOVEL     PTDTG         VACDAG
090100000526     C                   ELSE
090200941228     C  N33              Z-ADD     GCPMGC        VACDAG
090300941228     C  N33              MOVEL     GCPAGC        VACDAG
090400941228     C   33              MOVEL     *ZEROS        VACDAG
090500000526     C                   ENDIF
090600000526      *
090700990602     C*---------------------------------------------------------------*
090800990602     C* Se ho trovato un evanto di messa in consegna....
090900060912 1   C     WTRMIC        IFEQ      'S'
091000990602     C* ... e non ho la data di consegna totale sulla bolla ..
091100990602     C     BLPDCM        ANDEQ     0
091200040319      *
091300990602     C* Verifico se ho trovato una pratica C.A. aperta ...
091400060912 2   C     DATMAX        IFNE      0
091500990602     C     §3KCQ4        ANDEQ     'S'
091600990602     C* .. e se l'apertura di quest'ultima è successiva alla
091700990602     C*   messa in consegna ...
091800990602     C     DATMAX        ANDGE     WDTMIC
091900990602     C* .. allora imposto la data apertura C.A. nella data consegna
092000990602     C*    e imposto il tipo consegna anomala = 'A'
092100990602     C                   Z-ADD     1200          VACHMC
092200990602     C                   Z-ADD     DATMAX        VACDCM
092300990602     C                   MOVEL     'A'           VACCCA
092400060912 x2  C                   ELSE
092500990602     C* ... altrimenti imposto data messa in consegna
092600990602     C                   Z-ADD     EVBHEV        VACHMC
092700980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
092800990602     C                   MOVEL     'C'           VACCCA
092900060912 e2  C                   END
093000990602     C*
093100990602     C* ... se è stata impostata la data di consegna sulla bolla ..
093200060912 x1  C                   ELSE
093300040319     C*
093400060912 2   C     BLPDCM        IFNE      0
093500040319     C*
093600040319     C* Un tempo era stato asteriscato questo pezzo
093700040319     C* >>>>>>>>>>>>>
093800040409      * non deve essere fatto per l'estero
093900060912 3   C     wtpINV        Ifne      'E'
094000040409      *
094100990602     C* ... verifico se ho trovato una C.A. aperta ...
094200060912 4   C     DATMAX        IFNE      0
094300040319     C     §3KCQ4        ANDEQ     'S'
094400990602     C* .. e se l'apertura di quest'ultima è successiva alla
094500990602     C* .. consegna ...
094600040319     C     DATMAX        ANDGT     BLPDCM
094700040319     C*
094800990602     C* .. allora imposto la data apertura C.A. nella data consegna
094900990602     C*    e imposto il tipo consegna anomala = 'A'
095000040319     C                   Z-ADD     1200          VACHMC
095100040319     C                   Z-ADD     DATMAX        VACDCM
095200040319     C                   MOVEL     'A'           VACCCA
095300040319     C*
095400060912 e4  C                   ENDif
095500060912 e3  c                   end
095600040319     C* >>>>>>>>>>>>>
095700040319     C*
095800990602     C* ... se non è stata effettuata la messa in consegna, la
095900990602     C*     bolla risulta non consegnata, ....
096000060912 x2  C                   ELSE
096100990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
096200990602     C*     le consegne anomale, imposto la data consegna + flag
096300990602     C*     consegna anomala='A'
096400060912 3   C     DATMAX        IFNE      0
096500990602     C     §3KCQ4        ANDEQ     'S'
096600990602     C                   Z-ADD     1200          VACHMC
096700990602     C                   Z-ADD     DATMAX        VACDCM
096800990602     C                   MOVEL     'A'           VACCCA
096900060912 e3  C                   END
097000060912 e2  C                   END
097100060912 e1  C                   END
097200060912     C* Se ho trovato un evento di ripristino
097300060912 1   C     WTRrip        IFEQ      'S'
097400060912     C     BLPDCM        ANDEQ     0
097500060912     C     §3KCQ1        ANDEQ     'S'
097600060912     C                   Z-ADD     0             VACHMC
097700060912     C                   Z-ADD     0             VACDCM
097800060912     C                   MOVEL     'R'           VACCCA
097900060912 e1  c                   endif
098000060912
098100080417     c                   exsr      Write_VAC
098200930603     C                   SETON                                        06
098300990604     C*
098400990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
098500990618     C* e questa è stata anche.....
098600060912 1   C     §3KCQ1        IFEQ      'S'
098700080417     C     §3KCSD        ANDEQ     'EW'
098800080417      *
098900060912 2   C     VACCCA        IFEQ      'A'
099000990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
099100990618     C* invio al partner dell'informazione della consegna parziale
0992000609123    C     BLPDCM        IFEQ      0
099300990618     C     BLPDCP        ANDNE     0
099400990618     C                   Z-ADD     0             VACHMC
099500990618     C                   Z-ADD     0             VACDCM
099600990618     C                   MOVEL     ' '           VACCCA
099700080417     c                   exsr      Write_VAC
099800060912x3   C                   ELSE
099900990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
100000990618     C* invio al partner dell'informazione
100100060912 4   C     BLPDCM        IFEQ      0
100200990624     C     WDTMIC        ANDGT     0
100300990618     C                   Z-ADD     EVBHEV        VACHMC
100400990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
100500990618     C                   MOVEL     'C'           VACCCA
100600080417     c                   exsr      Write_VAC
100700060912e4   C                   END
100800060912e3   C                   END
100900990618     C*
101000990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
101100990618     C* scrivo anche record x apertura C.A.
101200060912x2   C                   ELSE
1013000609123    C     DATMAX        IFNE      0
101400990618     C     §3KCQ4        ANDEQ     'S'
101500990618     C                   Z-ADD     1200          VACHMC
101600990618     C                   Z-ADD     DATMAX        VACDCM
101700990618     C                   MOVEL     'A'           VACCCA
101800080417     c                   exsr      Write_VAC
101900060912 e3  C                   END
102000060912 e2  C                   END
102100060912 e1  C                   END
102200990618     C*
102300921026     C                   ENDSR
102400990602     C*---------------------------------------------------------------*
102500990602     C* Controllo se esiste una pratica C.A. aperta                  -*
102600990602     C*---------------------------------------------------------------*
102700990602     C     CRTCA         BEGSR
102800990602     C*
102900990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
103000990602     C* per reperire la pratica di danno con data più alta
103100990602     C                   Z-ADD     0             DATMAX            8 0
103200990602     C     §3KCQ4        IFEQ      'S'
103300040715      **
103400040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
103500040715      * SIA COME MAMMA CHE COME FIGLIA
103600040715      **
103700040715     c                   clear                   fidn12ds
103800040715     c                   eval      i12aas = blpAAS
103900040715     c                   eval      i12lnp = blpLNP
104000040715     c                   eval      i12nrs = blpNRS
104100040715     c                   eval      i12nsp = blpNSP
104200040715     c                   eval      i12fel = 'E'
104300040715     c                   eval      i12fan = 'E'
104400040715     c                   eval      i12fch = 'E'
104500040715     c                   call      'FIDN12R'
104600040715     c                   parm                    fidn12ds
104700040715     c                   z-add     *zeros        ii                2 0
104800040715      **
104900040715      * prende fra le C.A. aperte quella con la data ultima
105000040715     c                   if        o12nca > 0
105100040715     c                   dow       ii <  o12nca
105200040715      **
105300040715     c                   add       1             ii
105400040715     C                   movel     skDca(ii)     datape            8 0
105500040715     C     DATMAX        IFLT      DATAPE
105600040715     C                   Z-ADD     DATAPE        DATMAX
105700040715     C                   END
105800040715      *
105900040715     c                   enddo
106000040715     c                   endif
106100040715      *
106200040715     C                   END
106300040715     C*
106400990602     C                   ENDSR
106500970122     C*---------------------------------------------------------------*
106600970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
106700970122     C*---------------------------------------------------------------*
106800970121     C     TSTNMC        BEGSR
106900040422     **
107000040422     ** avendo fatto entrare anche x data consegna GT di 0
107100040422     ** il flag deve essere resettato solo per la vecchia condizione
107200040422     ** altrimenti non scrive + il VAC come una volta.
107300040422     c                   if        blpdcm = 0
107400040422     **
107500970122     C                   MOVEL     'N'           WFLDT3
107600090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
107700090223     C**   sempre il VAC.
107800090223     C********           MOVEL     'N'           WINVAC
107900040422     **
108000040422     c                   end
108100040422     **
108200970122     C                   MOVEL     'N'           WTREVE            1
108300970122     C                   MOVEL     *BLANKS       WSVTMC            1
108400970122     C                   MOVEL     *BLANKS       WMOT1            35
108500970122     C                   MOVEL     *BLANKS       WMOT2            35
108600970122     C*  Leggo le estensioni bolla della spedizione e per quelle
108700970122     C*  relative ad un motivo di mancata consegna scrivo VAG
108800970122     C                   DO        Y             W
108900970122     C                   MOVEL     T7K(W)        WTMC              1
109000970122     C                   MOVE      T7K(W)        KTRC
109100060214     C     Kar4          CHAIN     Fiar401L                           35
109200970123     C     *IN35         IFEQ      '0'
109300060214     C     ar4FTR        ANDNE     'T'
109400060214     C     ar4FTR        ANDNE     'C'
109500970122     C*  Se ho trovato estensione bolla e non è trasmesso
109600970122     C*  controllo se ho un nuovo tp.mancata consegna
109700970122     C*  e se si reperisco evento corrispondente
109800970122     C     WTMC          IFNE      WSVTMC
109900970122     C                   EXSR      REPEVE
110000970122     C                   END
110100970122     C*  se ho trovato l'evento controllo se devo scrivere
110200970122     C*  nuovo record VAG:
110300970122     C     WTREVE        IFEQ      'S'
110400970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
110500970122     C*  - se ho già scritto 2 motivi --> new record
110600970122     C     WTMC          IFNE      WSVTMC
110700970122     C     WMOT2         ORNE      *BLANKS
110800970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
110900970124     C     WSVTMC        IFNE      *BLANKS
111000970124     C     WMOT2         ANDEQ     *BLANKS
111100970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
111200970318     C*  primi 15 chr nella seconda motivazione
111300970318     C     WCHR15        IFNE      *BLANKS
111400970318     C                   MOVEL     WCHR15        VAGNOT
111500970318     C                   END
111600080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
111700050606     c                   move      'VG'          vgdtip
111800091116     c                   move      §3kgks        vgdksu
111900050627     c                   movel     0             vgdksu
112000091116     c                   move      §3kgsd        vgdtsc
112100050608     c                   eval      vgdpgm = 'FNLSA9R'
112200050627     c                   z-add     dateu         vgddat
112300091116     c                   if        §3kgsd  <> '  '
112400050606     C                   WRITE     tivgd000
112500091116     c                   end
112600970124     C                   SETON                                        06
112700970124     C                   END
112800970124     C*  New record
112900970122     C                   EXSR      NEWVAG
113000970122     C                   ELSE
113100970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
113200970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
113300970318     C     WCHR15        IFNE      *BLANKS
113400970318     C                   MOVEL     WCHR15        VAGNOT
113500060214     C                   MOVE      ar4NOT        VAGNOT
113600970318     C                   ELSE
113700970318     C*  Se codice generico occupo chr. da 32 posizione
113800970318     C     VAGCMC        IFEQ      'G  '
113900970318     C     VAGCMC        OREQ      'GEN'
114000060214     C                   MOVEL     ar4NOT        WCHR19           19
114100060214     C                   MOVE      ar4NOT        WCHR16           16
114200970318     C                   MOVE      WCHR19        VAGDMC
114300970318     C                   MOVEL     WCHR16        VAGNOT
114400970318     C                   ELSE
114500970318     C*  Altrimenti imposto solo ulteriore descrizione
114600060214     C                   MOVEL     ar4NOT        VAGNOT
114700970318     C                   END
114800970318     C                   END
114900060214     C                   MOVEL     ar4NOT        WMOT2
115000970122     C                   MOVEL     *BLANKS       WMOT1
115100050606     C*  Scrivo tivgd
115200080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
115300050606     c                   move      'VG'          vgdtip
115400091116     c                   move      §3kgks        vgdksu
115500050627     c                   movel     0             vgdksu
115600091116     c                   move      §3kgsd        vgdtsc
115700050608     c                   eval      vgdpgm = 'FNLSA9R'
115800050627     c                   z-add     dateu         vgddat
115900091116     c                   if        §3kgsd  <> '  '
116000050606     C                   WRITE     tivgd000
116100091116     c                   end
116200970122     C                   SETON                                        06
116300970122     C                   END
116400970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
116500970122     C                   MOVEL     'S'           WFLDT3
116600970122     C                   MOVEL     'S'           WINVAC
116700060214     C*  Fleggo Fiar4
116800060214     C                   MOVEL     'C'           ar4FTR
116900060214     C                   Z-ADD     DATEU         ar4DTR
117000060214     C                   EXCEPT    AGGar4
117100970122     C                   END
117200970122     C                   END
117300970122     C*
117400970122     C                   END
117500970122     C*  Controllo se mi è rimasto un VAG da scrivere
117600970122     C     WMOT1         IFNE      *BLANKS
117700970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
117800970318     C*  primi 15 chr nella seconda motivazione
117900970318     C     WCHR15        IFNE      *BLANKS
118000970318     C                   MOVEL     WCHR15        VAGNOT
118100970318     C                   END
118200050606     C*  Scrivo tivgd
118300080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
118400050606     c                   move      'VG'          vgdtip
118500091116     c                   move      §3kgks        vgdksu
118600050627     c                   movel     0             vgdksu
118700091116     c                   move      §3kgsd        vgdtsc
118800050608     c                   eval      vgdpgm = 'FNLSA9R'
118900050627     c                   z-add     dateu         vgddat
119000091116     c                   if        §3kgsd  <> '  '
119100050606     C                   WRITE     tivgd000
119200091116     c                   end
119300970122     C                   SETON                                        06
119400970122     C                   END
119500970122     C*
119600970122     C                   ENDSR
119700970122     C*---------------------------------------------------------------*
119800970122     C* Controllo se esiste evento                                   -*
119900970122     C*---------------------------------------------------------------*
120000970122     C     REPEVE        BEGSR
120100970122     C*
120200970122     C                   MOVEL     'N'           WTREVE
120300970122     C*  Mi posiziono su file eventi
120400131113     C     kBLP          SETGT     FNEVB01L
120500131113     C     kBLP          READPE    FNEVB01L                               34
120600131113      *
120700970122     C*  Loop letture fino a fine file o travato evento
120800970122     C     *IN34         DOWEQ     '0'
120900970122     C     WTREVE        ANDEQ     'N'
121000040504      **
121100040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
121200040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
121300040504      **  le A16/A23 come varianti in programmi che precedono facendo
121400040504      **  sballare il ragionamento in questo pgm.
121500040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
121600040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
121700040504     c                   movel     '0'           evbCEV
121800040504     c                   end
121900131113      *
122000131113      **  imposta sulla DS le note x testare il flag Progressivo
122100131113     c                   movel     evbNOT        Devb01
122200131119      *
122300131119      ** Sia per l'Italia che per L'estero deve sempre prendere l'ultimo EVENTO
122400131119      **  in quanto MANCATA CONSEGNA perchè la bolla sarà aggiornata solo con
122500131119      **  l'evento "U" o BLK
122600131119      *
122700131119     c                   if         §notPRG = ' ' or §notPRG = 'U'
122800131119      *
122900970122     C*  Reperisco tipo mancata consegna evento letto
123000970122     C                   Z-ADD     1             WW                4 0
123100970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
123200131113      *
123300970122     C*  Controllo se tipo mancata consegna evento letto
123400970122     C*  è uguale a quella dell'estensione bolla
123500131113      *
123600131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
123700131119     c                   if         *in42 and T2A(WW) = WTMC
123800970122     C                   MOVEL     'S'           WTREVE
123900131119     c                   leave
124000131119     C                   END
124100131113      *
124200131119     C                   EndIF
124300131119      *
124400131113     C     kBLP          READPE    FNEVB01L                               34
124500970122     C                   END
124600970122     C*
124700970122     C                   ENDSR
124800970122     C*---------------------------------------------------------------*
124900970122     C* Nuovo record FNVAG                                           -*
125000970122     C*---------------------------------------------------------------*
125100970122     C     NEWVAG        BEGSR
125200970122     C*
125300050606     C                   CLEAR                   FNVAGds
125400970721     C                   MOVEL     'S'           WRTVAG            1
125500970318     C                   MOVEL     *BLANKS       WCHR25           25
125600970318     C                   MOVEL     *BLANKS       WCHR15           15
125700970122     C                   Z-ADD     BLPAAS        VAGAAS
125800970122     C                   Z-ADD     BLPLNP        VAGLNP
125900970122     C                   Z-ADD     BLPNRS        VAGNRS
126000970122     C                   Z-ADD     BLPNSP        VAGNSP
126100970122     C                   Z-ADD     BLPLNA        VAGLNA
126200990810     C                   MOVEL     EVBDEV        WAA2              4 0
126300970123     C                   MOVE      WAA2          VAGAGC
126400970122     C                   MOVE      EVBDEV        VAGMGC
126500060214     C                   MOVEL     ar4NOT        VAGCMC
126600060214     C                   MOVEL     ar4NOT        WMOT1
126700970122     C                   MOVEL     *BLANKS       WMOT2
126800970307     C                   Z-ADD     1             YY                4 0
126900970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
127000970307     C     VAGCMC        IFNE      'G  '
127100970307     C     VAGCMC        ANDNE     'GEN'
127200970307     C                   MOVEL     D2A(YY)       VAGDMC
127300970318     C                   MOVE      D2A(YY)       WCHR25
127400970318     C                   MOVEL     WCHR25        WCHR15
127500970305     C                   ELSE
127600060214     C                   MOVE      ar4NOT        WCOM31           31
127700970122     C                   MOVEL     WCOM31        VAGDMC
127800970305     C                   END
127900970122     C                   MOVEL     BLPRMA        VAGRMA
128000970122     C                   Z-ADD     BLPRMN        VAGRMN
128100970122     C     BLPCCM        IFNE      0
128200970122     C                   MOVEL     BLPCCM        VAGSCM
128300970122     C                   ELSE
128400970122     C                   MOVEL     BLPKSC        VAGSCM
128500970122     C                   END
128600970123     C*
128700970123     C                   MOVEL     WTMC          WSVTMC
128800970122     C*
128900970122     C                   ENDSR
129000001031     C*---------------------------------------------------------------*
129100001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
129200001031     C*---------------------------------------------------------------*
129300001031     C     PARZIA        BEGSR
129400001031     C*
129500001031      * LOOP SU DETTAGLIO COLLI
129600001103     C                   Z-ADD     0             TOTCO
129700001031     C                   Z-ADD     0             CONS
129800001102     C                   Z-ADD     0             ULTIMA            8 0
129900001031     C     KBLP          SETLL     FNBLT01L
130000001031     C                   DO        *HIVAL
130100001102     C     KBLP          READE     FNBLT01L                               97
130200001031     C   97              LEAVE
130300001031      * CONTA CONSEGNATI E NON CONSEGNATI
130400001103     C     BLTDCM        IFGT      0
130500001031     C                   ADD       1             CONS              3 0
130600001031     C                   END
130700001103     C                   ADD       1             TOTCO             3 0
130800001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
130900001031     C     BLTDCM        IFGT      ULTIMA
131000001031     C                   Z-ADD     BLTDCM        ULTIMA
131100001031     C                   ENDIF
131200001031     C                   ENDDO
131300001102     C                   MOVE      CONS          CONSA             3
131400001103     C                   MOVE      TOTCO         TOTCOA            3
131500001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
131600001031      *
131700050606     C                   CLEAR                   FNVAGds
131800001031     C                   MOVE      BLPLNP        VAGLNP
131900001031     C                   MOVE      BLPAAS        VAGAAS
132000001031     C                   MOVE      BLPNRS        VAGNRS
132100001031     C                   MOVE      BLPNSP        VAGNSP
132200001107     C                   MOVEL     'P  '         VAGCMC
132300001031     C                   MOVE      BLPRMN        VAGRMN
132400001031     C                   MOVE      BLPRMA        VAGRMA
132500001031     C                   MOVE      BLPLNA        VAGLNA
132600001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
132700001031     C                   MOVEL     COS(1)        VAGDMC
132800001103     C                   MOVEA     COS(2)        COM
132900001102     C                   MOVEA     CONSA         COM(22)
133000001103     C                   MOVEA     TOTCOA        COM(29)
133100001102     C                   MOVEA     COM           VAGNOT
133200001031      *
133300001031     C     BLPCCM        IFGT      0
133400001031     C                   Z-ADD     BLPCCM        VAGSCM
133500001031     C                   ELSE
133600001031     C                   Z-ADD     BLPKSC        VAGSCM
133700001031     C                   END
133800001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
133900001031     C                   MOVEL     ULTIMA        VAGAGC
134000001031     C                   MOVE      ULTIMA        VAGMGC
134100001031      *
134200080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
134300050606     c                   move      'VG'          vgdtip
134400091116     c                   move      §3kgks        vgdksu
134500050627     c                   movel     0             vgdksu
134600091116     c                   move      §3kgsd        vgdtsc
134700050608     c                   eval      vgdpgm = 'FNLSA9R'
134800050627     c                   z-add     dateu         vgddat
134900091116     c                   if        §3kgsd  <> '  '
135000050606     C                   WRITE     tivgd000
135100091116     c                   end
135200001106     C                   MOVEL     'S'           WTRPAR
135300001031      *
135400001031     C                   ENDSR
135500080417      *------------------------------------------------------------------------*
135600080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
135700080417      *------------------------------------------------------------------------*
135800080417     C     Write_VAC     BEGSR
135900080417      *
136000090529      *  >>>>>>>>>
136100090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
136200090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
136300090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
136400090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
136500090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
136600090529      *   il record.
136700090529      *  >>>>>>>>>
136800090603     c                   if        §3kCTB ='S'
136900090603     c                   clear                   uguale_VAC        1
137000090529      *
137100090529     C                   movel     'VAC'         Kar5vac
137200090529     c     kar5          chain     fiar501l
137300090529      *
137400090529     c                   if        %Found(fiar501l)
137500090529      * Imposta i campi precedentemente salvati nella DS da comparare
137600090529     c                   eval      fnvacHGds = ar5UNI
137700090603      *
137800090603     c                   eval      VacGC1chg = VacGC1
137900090603     c                   eval      VacGC2chg = VacGC2
138000090603     c                   eval      VacFFDchg = VacFFD
138100090603     c                   eval      VacTCRchg = VacTCR
138200090603     c                   eval      VacDCRchg = VacDCR
138300090603     c                   eval      VacHCRchg = VacHCR
138400090603     c                   eval      VacDCMchg = VacDCM
138500090603     c                   eval      VacHMCchg = VacHMC
138600090603     c                   eval      VacTC1chg = VacTC1
138700090603     c                   eval      VacTC2chg = VacTC2
138800090603     c                   eval      VacCCAchg = VacCCA
138900090603     c                   eval      VacDLAchg = VacDLA
139000090603     c                   eval      VacDAGchg = VacDAG
139100090529      * quindi
139200090529      *    controllo di uguaglianza
139300090529     c                   if        fnvacHGds = DSvacHG
139400090529     c                   eval      uguale_VAC = 'S'
139500090603     c                   move      '='           ar5UNI
139600090603     c                   else
139700090603     c                   move      ' '           ar5UNI
139800090603     c                   eval      ar5UNI    = DSvacHG
139900090603     c                   end
140000090529      *
140100090529     c                   eval      AR5DAC = dateu
140200090529     c                   eval      AR5ORC = Orario
140300090529     c                   eval      AR5PRG = AR5PRG + 1
140400090529     c*
140500090529     c                   update    fiar5000
140600090529     c*
140700090529     c                   if        uguale_VAC = 'S'
140800090529     c                   leavesr
140900090529     c                   end
141000090529      *
141100090529     c                   else
141200090529      *
141300090529      *  lo memorizza x la prima volta
141400090529     c                   clear                   fiar5000
141500090529     c                   eval      ar5AAS = blpAAS
141600090529     c                   eval      ar5LNP = blpLNP
141700090529     c                   eval      ar5NRS = blpNRS
141800090529     c                   eval      ar5NSP = blpNSP
141900090529     c                   eval      ar5TRD = 'VAC'
142000090529     c                   eval      ar5DAC = dateu
142100090529     c                   eval      ar5ORC = Orario
142200090529     c                   eval      ar5PRG = AR5PRG + 1
142300090603      *
142400090603     c                   eval      VacGC1chg = VacGC1
142500090603     c                   eval      VacGC2chg = VacGC2
142600090603     c                   eval      VacFFDchg = VacFFD
142700090603     c                   eval      VacTCRchg = VacTCR
142800090603     c                   eval      VacDCRchg = VacDCR
142900090603     c                   eval      VacHCRchg = VacHCR
143000090603     c                   eval      VacDCMchg = VacDCM
143100090603     c                   eval      VacHMCchg = VacHMC
143200090603     c                   eval      VacTC1chg = VacTC1
143300090603     c                   eval      VacTC2chg = VacTC2
143400090603     c                   eval      VacCCAchg = VacCCA
143500090603     c                   eval      VacDLAchg = VacDLA
143600090603     c                   eval      VacDAGchg = VacDAG
143700090529     c                   eval      ar5UNI = DSvacHG
143800090529     c                   write     fiar5000
143900090529      *
144000090529     c                   end
144100090529     c                   end
144200090529      *  >>>>>>>>>
144300080417     C                   clear                   tivgd000
144400080417     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
144500080417     C                   eval      vgdTIP = 'VC'
144600080417     C                   movel     *all'0'       vgdKSU
144700080417     c                   move      §3kcks        vgdksu
144800080417      * tipo invio
144900080417     c                   move      §3kcsd        vgdtsc
145000080417     c                   eval      vgdpgm = 'FNLSA9R'
145100080417     c                   z-add     dateu         vgddat
145200080417      *
145300080417      *  x generare l'EDI:
145400080417      *    occorre farlo in modo controllato (personalizzato)
145500080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
145600080417     C                   clear                   DVGDFLO
145700080417     C                   movel     'P'           §VGDFELA
145800080417     C                   movel     DVGDFLO       vgdflo
145900080417     c                   end
146000080417      *
146100091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
146200091116      *  Non deve scrivere nulla.
146300091116     c                   if        §3kcsd <> *blank
146400080417     C                   write     tivgd000
146500110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
146600110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
146700110721     C                   MOVEL     'S'           WFLDT3
146800091116     c                   end
146900080417      *
147000080417     C                   ENDSR
147100080930      *------------------------------------------------------------------------*
147200080930      * Routine di controllo x eventuali Errori
147300080930      *------------------------------------------------------------------------*
147400080930     C     *PSSR         BEGSR
147500080930      *
147600080930      * Comunque eseguo x rilasciare il TIVGD.
147700080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
147800080930     C                   clear                   trul47ds
147900080930     C                   eval      d47opz  = 'F'
148000080930     C                   eval      d47tip  = 'VC'
148100080930     C                   call      'TRUL47R'
148200080930     C                   parm                    trul47ds
148300080930      *
148400080930     C                   ENDSR     '*CANCL'
148500080417     C*------------------------------------------------------------------------*
148600060214     OFiar4000  E            AGGar4
148700060214     O                       ar4FTR
148800060214     O                       ar4DTR
148900921026**
149000981210DLTOVR FILE(*ALL)
149100000726**
149200921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
149300921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
149400921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
149500001031**
149600001031Consegna parziale
149700001031Nr. colli consegnati xxx su xxx
