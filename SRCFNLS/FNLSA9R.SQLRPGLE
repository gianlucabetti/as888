000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200170322     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300941228     H* FNLSA9R *----------------------------------------------------*
000400080729     H*         - RITORNO DATI AL CLIENTE
000500080417      *? Rimane come tipo invio solo il "WW" e "EW". Tutti i riferimenti
000600080417      *? degil altri tipi invio della 3T usati nella 3K vengono ignorati.
000700000000     H*--------------------------------------------------------------*
000800921026     FTABEL00F  UF   E           K DISK
000900050309     Ftigcp01L  IF   E           K DISK
001000941228     FFNBLP55L  UF   E           K DISK
001100051115     FFiar901L  IF   E           K DISK
001200060214     FFiar401L  UF   E           K DISK
001300090603     FFiar501L  UF a E           K DISK
001400990810     FFNEVB01L  IF   E           K DISK
001500131014     FFIARBF2C  IF   E           K DISK
001600000526     F                                     IGNORE(FNARBD00)
001700000526     F                                     IGNORE(FIARBT00)
001800000526     F                                     IGNORE(FNARBP00)
001900131014     F                                     IGNORE(FNARBN00)
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
004300170322
004400170322     D psds           sds
004500170322     D  procname         *PROC
004600170322     D TIS7VASDS     E DS                  inz
004700170322
004800131113      * Note degli Eventi
004900131113     D Devb01        e ds
005000080417     D DVGDFLO       e ds
005100080417     D FNVACDS       e ds
005200090529     D FNVAChgDS     e ds
005300090603     D  DSVAChg        ds
005400090603     d    VacGC1chg                        like(VacGC1)
005500090603     d    VacGC2chg                        like(VacGC2)
005600090603     d    VacFFDchg                        like(VacFFD)
005700090603     d    VacTCRchg                        like(VacTCR)
005800090603     d    VacDCRchg                        like(VacDCR)
005900090603     d    VacHCRchg                        like(VacHCR)
006000090603     d    VacDCMchg                        like(VacDCM)
006100090603     d    VacHMCchg                        like(VacHMC)
006200090603     d    VacTC1chg                        like(VacTC1)
006300090603     d    VacTC2chg                        like(VacTC2)
006400090603     d    VacCCAchg                        like(VacCCA)
006500090603     d    VacDLAchg                        like(VacDLA)
006600090603     d    VacDAGchg                        like(VacDAG)
006700080417      *
006800941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006900941228     D WLBDAT          DS                  INZ
007000941228     D  G02DAT                 1      8  0
007100941228     D  G02INV                 9     16  0
007200941228     D  G02ERR                17     17
007300941228     D  G02TGI                18     22  0
007400900517     D KPJBA         E DS
007500921023     D DS3K          E DS
007600170309     D tabelds       e ds                  extname('TABEL00F')
007700970122     D DS7K          E DS
007800941228     D DS2A          E DS
007900000525     D OG143         E DS
008000000526     D DSBL4J        E DS
008100170511     D fnvagds       E DS                  extname(fnvag00t)
008200040715
008300040715     d Fidn12ds      e ds
008400040715     d  skKey                 26    305    dim(20)
008500040715     d  skAnn                306    325    dim(20)
008600040715     d  skDca                326    485    dim(20)
008700040715     d  skFca                486    545  0 dim(20)
008800040715     d  skDch                546    705  0 dim(20)
008900040715     d  skCch                706    745    dim(20)
009000170309
009100170309     d wSQl            s           1024    inz  varying
009200170322     d s3KCKS          s                   like(§3KCKS) inz
009300170322     d s3KCSD          s                   like(§3KCSD) inz
009400170322     d s§7VASPRG       s                   like(o§7VASPRG) inz
009500170322     d g§7VASPRG       s                   like(o§7VASPRG) inz
009600170322
009700971126      *----------------------------------------------------------------
009800971126     IAZORG01L  AA
009900971126     I                             P    4    5 0ORGFIL
010000971126     I                                 11   11  ORGFL1
010100000525     I                               3776 3800  ORGDE3
010200090408     IFNARBG00      51
010300090408     IFNARBK00      52
010400920325     C*---------------------------------------------------------------*
010500920812     C* INDICATORI
010600920812     C*---------------------------------------------------------------*
010700921026     C* 01    - NON TUTTI I MEMBRI SONO STATI ALLOCATI
010800921026     C* 30    - LETTURA FLBLP00F
010900921026     C* 31    - LETTURA TABEL00F
011000921026     C* 32    - VEDO SE ESISTE GIA' BOLLA SU FLBLR
011100921026     C* 33    - GIACENZA
011200921026     C* 34    - COSEGNA PARTICOLARE NUMERICA (COD ANOMALIA)
011300970122     C* 35    - TROVATE ESTENSIONI BOLLE X MANCATE CONSEGNE
011400970123     C* 99    - ESEGUO CHIUSURA E DISALLOCAZIONE MEMBRO FNVAC
011500920812     C*---------------------------------------------------------------*
011600000000     C     *ENTRY        PLIST
011700000000     C                   PARM                    KPJBA
011800980407     C                   MOVEL     KPJBU         WTPINV            2
011900921026     C                   Z-ADD     1             CODUT
012000170322     C*
012100170322     C* Avvio il monitoring del processo
012200170322     C                   monitor
012300170322     C*
012400080417     C*---------------------------------------------------------------*
012500080417      *?  Definizione Campi e Caricamento Tabelle   ?
012600080417     C*---------------------------------------------------------------*
012700080417      *
012800060921     C     Klbl2         KLIST
012900060921     C                   KFLD                    klb2_AAS
013000060921     C                   KFLD                    klb2_LNP
013100060921     C                   KFLD                    klb2_NRS
013200060921     C                   KFLD                    klb2_NSP
013300060921     C     Klbl2p        KLIST
013400060921     C                   KFLD                    exLBLAAp
013500060921     C                   KFLD                    exLBLLPp
013600060921     C                   KFLD                    exLBLNRp
013700060921     C                   KFLD                    exLBLNSp
013800060921     C     KBLP          KLIST
013900060921     C                   KFLD                    BLPAAS
014000060921     C                   KFLD                    BLPLNP
014100060921     C                   KFLD                    BLPNRS
014200060921     C                   KFLD                    BLPNSP
014300060214     C     Kar4          KLIST
014400970122     C                   KFLD                    BLPAAS
014500970122     C                   KFLD                    BLPLNP
014600970122     C                   KFLD                    BLPNRS
014700970122     C                   KFLD                    BLPNSP
014800970122     C                   KFLD                    KTRC
014900921026     C     KBLP2         KLIST
015000921026     C                   KFLD                    COD99             7 0
015100921026     C                   KFLD                    KSC
015200920316     C     KTAB          KLIST
015300921026     C                   KFLD                    CODUT             1 0
015400921026     C                   KFLD                    COD               2
015500941228     C     KTAB2         KLIST
015600941228     C                   KFLD                    CODUT             1 0
015700941228     C                   KFLD                    COD               2
015800941228     C                   KFLD                    KEY               8
015900090529     C     Kar5          KLIST
016000090529     C                   KFLD                    BLPAAS
016100090529     C                   KFLD                    BLPLNP
016200090529     C                   KFLD                    BLPNRS
016300090529     C                   KFLD                    BLPNSP
016400090529     C                   KFLD                    Kar5vac           3
016500941228     C**
016600941228     C* DEFINIZIONE CAMPI
016700941228     C**
016800941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
016900060214     C     *LIKE         DEFINE    ar4TRC        KTRC
017000051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
017100051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
017200051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
017300000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
017400000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
017500000526     C     *LIKE         DEFINE    VACDAG        PTDTG
017600060921      *
017700060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
017800060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
017900060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
018000060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
018100941228     C*---------------------------------------------------------------*
018200921026     C* GIRO UDATE
018300941228     C                   TIME                    W0140            14 0
018400941228     C* UDATE IN GGMMAAAA
018500941228     C                   MOVE      W0140         WDTGIO            8 0
018600090529     C                   MOVEl     W0140         Orario            6 0
018700941228     C*
018800941228     C* UDATE IN AAAAMMGG
018900941228     C                   Z-ADD     WDTGIO        G02DAT
019000941228     C                   MOVEL     *BLANK        G02ERR
019100941228     C                   CALL      'XSRDA8'
019200941228     C                   PARM                    WLBDAT
019300941228     C                   MOVEL     G02INV        DATEU             8 0
019400941228     C*---------------------------------------------------------------*
019500000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
019600000525     C                   CLEAR                   FIP
019700000525     C                   Z-ADD     0             Y
019800000525     C     *LOVAL        SETLL     AZORG01L
019900000525     C                   DO        *HIVAL
020000000525     C                   READ      AZORG01L                               95
020100000525     C   95              LEAVE
020200000525     C                   MOVEL     ORGDE3        OG143
020300020729     C     §OgNTW        IFEQ      'PPT'
020400000525     C                   Z-ADD     1             Y
020500000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
020600000525     C   23              Z-ADD     ORGFIL        FIP(Y)
020700000525     C                   END
020800000525     C                   END
020900000525     C*---------------------------------------------------------------*
021000970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
021100970122     C                   Z-ADD     0             Y                 4 0
021200970122     C                   MOVEA     *HIVAL        T7K
021300970122     C                   MOVEL     '7K'          COD
021400970122     C     KTAB          CHAIN     TABEL                              31
021500970122     C     *IN31         DOWEQ     '0'
021600970122     C     TBLFLG        IFEQ      ' '
021700970122     C* Controllo se impostato tipo mancata consegna
021800970122     C                   MOVEL     TBLUNI        DS7K
021900970122     C     §7KTMC        IFNE      *BLANKS
022000970122     C                   ADD       1             Y
022100970122     C                   MOVEL     §7KTMC        T7K(Y)
022200970123     C                   MOVEL     TBLKEY        WTRC              1
022300970123     C                   MOVE      WTRC          T7K(Y)
022400970122     C                   END
022500970122     C                   END
022600970122     C     KTAB          READE     TABEL                                  31
022700970122     C                   END
022800970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
022900970122     C     Y             IFGT      0
023000970122     C                   SORTA     T7K
023100970122     C                   END
023200970122     C*---------------------------------------------------------------*
023300970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
023400970122     C                   Z-ADD     0             W                 4 0
023500970122     C                   MOVEA     *HIVAL        C2A
023600970305     C                   MOVEA     *HIVAL        D2A
023700970122     C                   MOVEA     *HIVAL        T2A
023800970122     C                   MOVEL     '2A'          COD
023900970122     C     KTAB          CHAIN     TABEL                              31
024000970122     C     *IN31         DOWEQ     '0'
024100970122     C     TBLFLG        IFEQ      ' '
024200970122     C* Caricamento evento + tipo mancata consegna
024300970122     C                   MOVEL     TBLUNI        DS2A
024400970122     C     §2AFTC        IFNE      *BLANKS
024500970122     C                   ADD       1             W
024600970122     C                   MOVEL     §2AFTC        T2A(W)
024700970305     C                   MOVEL     §2ADES        D2A(W)
024800970123     C                   MOVEL     TBLKEY        C2A(W)
024900970122     C                   END
025000970122     C                   END
025100970122     C     KTAB          READE     TABEL                                  31
025200970122     C                   END
025300080417     C*---------------------------------------------------------------*
025400080417      * ?  CICLO Principale del Programma              ?
025500970122     C*---------------------------------------------------------------*
025600170322     C*
025700170323     C* Forzo ZERO come progressivo univoco download specifico per il VG
025800170324     C                   exsr      RTVPRG
025900170324     C                   eval      g§7VASPRG = o§7VASPRG
026000170324     C                   eval      %subst(g§7VASPRG:1:2) = 'XX'
026100170322     C*
026200170322     C*---------------------------------------------------------------*
026300080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
026400080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
026500921023     C                   MOVEL     '3K'          COD
026600170309     c                   exsr      setll_sql
026700170309     c                   exsr      fetch_sql
026800921023     C*
026900921023     C     *IN31         DOWEQ     '0'
027000131113      *
027100921023     C     TBLFLG        IFEQ      ' '
027200980407     C                   MOVEL     TBLUNI        DS3K
027300080417      * ?  Se Estero:  ?
027400980409     C     WTPINV        IFEQ      'E'
027500080417     C     §3KCSD        ANDEQ     'EW'
027600980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
027700080417      * ?  oppure se NON Estero e non EDI:  ?
027800980409     C     WTPINV        ORNE      'E'
027900080417     C     §3KCSD        ANDNE     'EW'
028000091116     C     §3KCSD        ANDNE     *blank
028100091116      *
028200921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
028300921026     C                   MOVEL     TBLKEY        KSC               7 0
028400930811     C                   MOVEL     KSC           COD99
028500930811     C                   MOVE      9999          COD99
028600011002     C                   MOVEL     KSC           KSCJ              7
028700930811     C*
028800941228     C     KSC           SETLL     FNBLP000                               32
028900941228     C  N32KBLP2         SETLL     FNBLP000                               32
029000060214     C  N32KSCJ          SETLL     FNBLP67J                               32
029100921023     C*
029200930811     C     *IN32         IFEQ      *ON
029300131113     C*                                                SALVO LA KEY
029400941228     C                   MOVEL     TBLKEY        SAVKEY
029500930811     C                   EXSR      ELAB
029600941228     C*
029700131113     C* RICHAINO IL RECORD DI TABEL
029800941228     C                   MOVEL     '3K'          COD
029900941228     C                   MOVEL     SAVKEY        KEY
030000941228     C     KTAB2         CHAIN     TABEL                              31
030100930811     C                   Z-ADD     DATEU         §3KCDU
030200930811     C                   MOVEL     DS3K          TBLUNI
030300930811     C                   UPDATE    TABEL
030400131113      *
030500921026     C                   END
030600131113      *
030700921023     C                   END
030800980409     C                   END
030900921023     C*
031000170309     c                   exsr      fetch_sql
031100921023     C                   END
031200170309     c*
031300170309     c* chiudo il cursore
031400170309     c                   exsr      close_sql
031500170322     C*
031600170322     C* Finalizzo la transazione in sospeso VAC
031700170322     C                   exsr      RLSPRG
031800170322     C*
031900170322     C* Finalizzo la transazione VAG
032000170322     C                   exsr      RLTPRG
032100981210     C*
032200981210     C* CALCELLO LE OVR ESEGUITE
032300981210     C                   MOVEL     *BLANKS       COMMAN           80
032400981210     C                   Z-ADD     17            LUNG
032500981210     C                   MOVEA     CMD(1)        COMMAN
032600981210     C                   CALL      'QCMDEXC'                            21
032700981210     C                   PARM                    COMMAN
032800080417     C                   PARM                    LUNG             15 5
032900080417      **
033000080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
033100980409     C   06WTPINV        IFEQ      'E'
033200110419     C                   CALL      'TRTCT80R'
033300030924     C                   parm                    Kpjba
033400980409     C                   END
033500170322     C*
033600170322     C* Su errore
033700170322     C                   on-error
033800170322     C                   exsr      exeerr
033900170322     C*
034000170322     C* Fine monitoring
034100170322     C                   endmon
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
046000170511     c                             arbCVB = 'CS' or
046100170511     c                             arbCVB = 'CI' or
046200170511     c                             arbCVB = 'CA' or
046300170511     c                             arbCVB = 'CD'
046400090408     c                   move      'S'           VariataOggi
046500090408     c                   leave
046600090408     c                   end
046700090408     c                   end
046800090408     c                   end
046900131014     c                   setoff                                       51
047000131014     C     KBLP          reade     FIARBF2C
047100090408     C                   end
047200090408     C                   ENDIF
047300090408      *-------
047400941228     C* CERCO EVENTO LASCIATO AVVISO
047500941228     C                   CLEAR                   WDLA
047600941228     C                   MOVEL     '2A'          COD
047700941228     C*
047800131113     C     kBLP          SETGT     FNEVB000
047900131113     C     kBLP          READPE    FNEVB000                               34
048000941228     C*
048100941228     C     *IN34         DOWEQ     *OFF
048200131113     C*
048300131113      **  imposta sulla DS le note x testare il flag Progressivo
048400131113     c                   movel     evbNOT        Devb01
048500131113      *
048600131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
048700131113     c                   if        wTPinv <> 'E'
048800131113     c                               or
048900131113     c                             wTPinv = 'E' and
049000131113     c                             (§notPRG = ' ' or §notPRG = 'U')
049100131113      **
049200941228     C                   MOVEL(P)  EVBCEV        KEY
049300941228     C*
049400941228     C     KTAB2         CHAIN     TABEL                              30
049500941228     C     *IN30         IFEQ      *OFF
049600040319     C                   MOVEL     TBLUNI        DS2A
049700941228     C     §2AFTC        IFEQ      'A'
049800941228     C                   Z-ADD     EVBDEV        WDLA              8 0
049900941228     C                   SETON                                        34
050000941228     C                   ENDIF
050100941228     C                   ENDIF
050200941228     C*
050300131113     c                   endif
050400131113     C*
050500131113     C  N34kBLP          READPE    FNEVB000                               34
050600941228     C                   ENDDO
050700921026     C*
050800970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
050900970721     C                   MOVEL     'N'           WRTVAG            1
051000970121     C                   MOVEL     'S'           WFLDT3            1
051100970121     C                   MOVEL     'S'           WINVAC            1
051200040319      *
051300970121     C     §3KNMC        IFEQ      'S'
051400970121     C     BLPDCM        ANDEQ     0
051500011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
051600060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
051700011002     C     *IN68         ANDEQ     '0'
051800040319      *
051900040319      * ....ma anche
052000040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
052100040319      *
052200040319     C     §3kNMC        orEQ      'S'
052300040319     C     blpDCM        andGT     0
052400040319     C     blpCCA        andNE     *blank
052500040319     C     *in68         andEQ     *off
052600040319      *
052700970121     C                   EXSR      TSTNMC
052800970121     C                   END
052900001031     C*
053000001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
053100001031      *  PARZIALE E VERIFICA DELLE DATE
053200001106     C                   MOVEL     'N'           WTRPAR            1
053300131113      **
053400001031     C     §3KCQ6        IFEQ      'S'
053500001031     C     BLPDCM        ANDEQ     0
053600001031     C     BLPDCP        ANDGT     0
053700011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
053800060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
053900011002     C     *IN68         ANDEQ     '0'
054000001031     C                   EXSR      PARZIA
054100001031     C                   END
054200131113      **
054300980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
054400980318     C                   MOVEL     'N'           WTRMIC
054500060912     C                   MOVEL     'N'           WTRRIP
054600990602     C                   Z-ADD     0             WDTMIC            8 0
054700131113      **
054800131113      **  Non Consegnata e si deve inviare il MIC o la Data Consegna
054900131113      **      se si tratta dell'ULTIMO EVENTO registrato
055000980406     C     §3KCQ3        IFEQ      'S'
055100980318     C     BLPDCM        ANDEQ     0
055200060912     C     §3KCQ1        orEQ      'S'
055300060912     C     BLPDCM        ANDEQ     0
055400131113      **
055500131113     C     kBLP          SETGT     FNEVB000
055600131113     C     kBLP          READPE    FNEVB000                               34
055700131113      *  Cercando sempre l'ultimo evento NON deve fare distinzioni
055800131113      *   fra Italia o Estero
055900131113      *
056000980318     C     *IN34         IFEQ      *OFF
056100060912     C     EVBCEV        ifEQ      'MIC'
056200060912     C     §3KCQ3        andeq     'S'
056300980318     C                   MOVEL     'S'           WTRMIC            1
056400990602     C                   Z-ADD     EVBDEV        WDTMIC
056500060912     c                   else
056600060912      *ripristino
056700060912     C     EVBCEV        ifeq      'RIP'
056800060912     C     EVBCEV        oreQ      'RDC'
056900060912     C                   MOVEL     'S'           WTRRip            1
057000060912     C                   endif
057100060912      *
057200060912     C                   endif
057300980318     C                   END
057400131113      *
057500980318     C                   END
057600921026     C* DATA CONSEGNA
057700930809     C     §3KCQ1        IFEQ      'S'
057800921026     C     BLPDCM        ANDNE     0
057900080410      *
058000010202     C     §3KCQ1        OREQ      'S'
058100010202     C     §3KCSD        ANDEQ     'EW'
058200010202     C     BLPDCM        ANDEQ     0
058300010202     C     BLPDCP        ANDNE     0
058400080410      *
058500930809     C* DATA LASCIATO AVVISO
058600930809     C     §3KCQ2        OREQ      'S'
058700941228     C     WDLA          ANDGT     0
058800080410      *
058900930809     C* ANOMALIE
059000930809     C     §3KCQ4        OREQ      'S'
059100930809     C     BLPCCA        ANDNE     *BLANKS
059200080410      *
059300990602     C* ANOMALIE DANNI
059400990602     C     §3KCQ4        OREQ      'S'
059500990602     C     DATMAX        ANDNE     0
059600080410      *
059700930809     C* GIACENZA
059800930809     C     §3KCQ5        OREQ      'S'
059900930809     C     *IN33         ANDEQ     *OFF
060000080410      *
060100970123     C* MANCATE CONSEGNE
060200970123     C     §3KNMC        OREQ      'S'
060300970123     C     BLPDCM        ANDEQ     0
060400080410      *
060500980318     C* Messa in consegna
060600980318     C     §3KCQ3        OREQ      'S'
060700980318     C     WTRMIC        ANDEQ     'S'
060800080410      *
060900060912     C* Messa in consegna
061000060912     C     §3KCQ1        OREQ      'S'
061100060912     C     WTRrip        ANDEQ     'S'
061200060912     C     BLPDCM        ANDEQ     0
061300080410      *
061400090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
061500080410     C     §3KCQ8        OREQ      'S'
061600090408     c     VariataOggi   andEQ     'S'
061700090408     C     BLPDCR        ANDGT     0
061800080721     C*
061900080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
062000080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
062100080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
062200921026     C*
062300970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
062400970123     C* consegna o se li devo trasmettere e li ho trovati
062500970123     C     §3KNMC        IFNE      'S'
062600970123     C     WINVAC        OREQ      'S'
062700990624     C* ANOMALIE DANNI
062800990624     C     §3KCQ4        OREQ      'S'
062900990624     C     DATMAX        ANDNE     0
063000990624     C* Messa in consegna
063100990624     C     §3KCQ3        OREQ      'S'
063200990624     C     WTRMIC        ANDEQ     'S'
063300060912     C* Evento di ripristino
063400060912     C     §3KCQ1        OREQ      'S'
063500060912     C     WTRrip        ANDEQ     'S'
063600080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
063700080721     C     §3KCQ8        OREQ      'S'
063800080721     C     BLPDCR        ANDGT     0
063900090408     c     VariataOggi   andEQ     'S'
064000930809     C                   EXSR      CARBLR
064100970123     C                   END
064200080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
064300970123     C*
064400930809     C                   END
064500990602     C                   END
064600921026     C*
064700970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
064800970123     C* consegna o se li devo trasmettere e li ho trovati
064900970123     C     §3KNMC        IFNE      'S'
065000090527      *
065100970123     C     WFLDT3        OREQ      'S'
065200090527      *
065300090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
065400090527      *  fino ad un successivo Evento sulle bolle figlie)
065500090527     C     §3KCQ4        OREQ      'S'
065600090527     C     BLPCCA        ANDNE     *BLANKS
065700090527      *
065800990624     C* ANOMALIE DANNI
065900990624     C     §3KCQ4        OREQ      'S'
066000990624     C     DATMAX        ANDNE     0
066100990624     C* Messa in consegna
066200990624     C     §3KCQ3        OREQ      'S'
066300990624     C     WTRMIC        ANDEQ     'S'
066400001106     C* CONSEGNA PARZIALE
066500001106     C     §3KCQ6        OREQ      'S'
066600001106     C     WTRPAR        ANDEQ     'S'
066700080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
066800080729     C     §3KCQ8        OREQ      'S'
066900080729     C     BLPDCR        ANDGT     0
067000090408     c     VariataOggi   andEQ     'S'
067100080417      *
067200921026     C                   MOVEL     'T'           BLPFT3
067300941228     C                   MOVEL     DATEU         BLPDT3
067400080417      *
067500010927     C  N68              UPDATE    FNBLP000
067600010927     C   68              UPDATE    FNBLP1
067700970123     C                   END
067800921026     C*
067900921026     C                   ENDSR
068000000526     C*
068100170309       //--------------------------------------------------------------
068200170309       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3K
068300170309       //--------------------------------------------------------------
068400170309       begsr  setll_sql;
068500170309
068600170309         wSQL = 'select * +
068700170309                 from tabel00f +
068800170309                 where +
068900170309                 tblkut = 1 and +
069000170309                 tblcod = ''3K'' +
069100170309                 order by +
069200170309                 substr(TBLUNI , 27 , 7)';
069300170309
069400170309         // Dichiarazione cursore
069500170309         exec sql   prepare S1   from :wSQL;
069600170309         exec sql   declare C1  asensitive   cursor for S1;
069700170309
069800170309         // Apertura del cursore
069900170309         exec sql   open C1;
070000170309
070100170309         *in99 = *off;
070200170309         if SQLCode < 0;
070300170309           *in99 = *on;
070400170309         endif;
070500170309
070600170309       endsr;
070700170309       //--------------------------------------------------------------
070800170309       // fetch_sql  - legge il rcdset di TABEL00F
070900170309       //--------------------------------------------------------------
071000170309       begsr  fetch_sql;
071100170309
071200170309         exec sql  fetch next  from C1  into :TABELDS;
071300170309
071400170309         *in31 = *off;
071500170309         if SQLCode = 100 or SQLCode < 0;
071600170309           *in31 = *on;
071700170309         endif;
071800170309
071900170309       endsr;
072000170309       //--------------------------------------------------------------
072100170309       // close_sql  - chiusura cursore
072200170309       //--------------------------------------------------------------
072300170309       begsr  close_sql;
072400170309
072500170309         // chiusura del cursore
072600170309         exec sql  close C1;
072700170309
072800170309       endsr;
072900170309
073000060921      **?-------------------------------------------------------------*
073100060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
073200060921      **?-------------------------------------------------------------*
073300060921     C     ChecK_Figlie  BEGSR
073400060921      *
073500060921      *  inizializza flag di work della Routine
073600060921     c                   move      'S'           Primo_legame      1
073700060921      *
073800060921      **?chiave salvata della Bolla x partire nella ricerca
073900060921     c                   z-add     blpAAS        klb2_AAS
074000060921     c                   z-add     blpLNP        klb2_LNP
074100060921     c                   z-add     blpNRS        klb2_NRS
074200060921     c                   z-add     blpNSP        klb2_NSP
074300060921      *
074400060921      *  Chiave Bolla Precedente
074500060921     c     new_ciclo     tag
074600060921      **?                                  Flag di work del ciclo
074700060921     c                   move      'N'           ha_figlia         1
074800060921     C     Klbl2         setLL     FnLBL02L
074900060921     C     Klbl2         reade     FnLBL02L
075000060921      *
075100060921     c                   Dow       not %EoF(FnLBL02L)
075200060921      **?primo controllo:
075300060921      *    tutta la catena deve partire dalla bolla mamma che è
075400060921      *     uguale alla bolla originale del primo record della catena
075500060921     c                   if          Primo_legame = 'S'
075600060921     c                   eval      Primo_legame = 'N'
075700060921     c                   if          blpAAS <> exLBLAAo   or
075800060921     c                               blpLNP <> exLBLLPo   or
075900060921     c                               blpNRS <> exLBLNRo   or
076000060921     c                               blpNSP <> exLBLNSo
076100060921      *    se così non è non deve saltare il ciclo e non impostare i
076200060921      *     campi chiave x il contrassegno.
076300060921     c                   leave
076400060921     c                   end
076500060921     c                   end
076600060921      **?secondo controllo:
076700060921      *     non deve essere una bolla locale
076800060921      *   (sulle locali non c'è l'informazione di contrassegno)
076900060921     c                   if        exLBLLPN <> exLBLLPP
077000060921      *  imposta la chiave dell'ultima figlia
077100060921      *       per il prossimo giro di controllo
077200060921     c                   z-add     exLBLAAN      klb2_AAS
077300060921     c                   z-add     exLBLLPN      klb2_LNP
077400060921     c                   z-add     exLBLNRN      klb2_NRS
077500060921     c                   z-add     exLBLNSN      klb2_NSP
077600060921     c                   move      'S'           ha_figlia
077700060921     c                   end
077800060921      *
077900060921      **?   (attenzione a non sposizionarsi)
078000060921      * Chiave impostata come Bolla Precedente appena letta
078100060921     C     KLBL2P        reade     FnLBL02L
078200060921     c                   end
078300060921      *
078400060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
078500060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
078600060921      *   se invece non ha trovato delle figlie allora va a fine con la
078700060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
078800060921     c                   if        ha_figlia = 'S'
078900060921     c                   if        exLBLAAp <> klb2_AAS or
079000060921     c                             exLBLLPp <> klb2_LNP or
079100060921     c                             exLBLNRp <> klb2_NRS or
079200060921     c                             exLBLNSp <> klb2_NSP
079300060921     c                   goto      new_ciclo
079400060921     c                   end
079500060921     c                   end
079600060921      *
079700060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
079800060921      **?   x verificare la presenza del contrassegno su AR9
079900060921      *
080000060921     C                   ENDSR
080100060921      **?-------------------------------------------------------------*
080200000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
080300921026     C*
080400131119     C*   per quel che riguarda le POSTE
080500131119     C*    NON è stato adeguato alla logica degli eventi passati dal PDA
080600131119     C*     quindi con i flag del progressivo 'P' e 'U'
080700131119     C*
080800000526     C     POSTE         BEGSR
080900000526      *
081000000526     C                   CLEAR                   PTTIC
081100000526     C                   CLEAR                   PTCAS
081200000526     C                   CLEAR                   PTVCA
081300000526     C                   CLEAR                   PTIAS
081400000526     C                   CLEAR                   PTVAS
081500000526     C                   CLEAR                   PTDTG
081600000526     C                   CLEAR                   §POST
081700000526      *
081800000526     C                   SETOFF                                       22
081900000526     C     BLPLNP        LOOKUP    FIP                                    22
082000000526     C   22              MOVEL     'S'           §POST             1
082100131113      *
082200000526     C     §POST         IFEQ      'S'
082300131113      *
082400000526      * Se la bolla è stata chiusa con consegna anomala la merce
082500000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
082600000526      * questo è stato annullato in quanto non più incassabile da
082700000526      * Bartolini.
082800000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
082900000526      * informazione da ritornare alla SDA
083000000526     C     BLPCCA        IFNE      *BLANK
083100051115     C     ar9CAS        ANDEQ     *ZEROS
083200000526     C     KBLP          CHAIN     FNARBK00                           22
083300000526     C     *IN22         IFEQ      '0'
083400000526     C                   MOVEL     ARBTIC        PTTIC
083500000526     C                   Z-ADD     ARBCAS        PTCAS
083600000526     C                   MOVEL     ARBVCA        PTVCA
083700000526     C                   ENDIF
083800000526     C                   ENDIF
083900000526      *
084000060214      * Prendo importo da assicurare da Fiar4 t.r.=J
084100000526     C                   MOVEL     'J'           KTRC
084200060214     C     Kar4          CHAIN     Fiar401L                           35
084300000526     C     *IN35         IFEQ      '0'
084400060214     C                   MOVEL     ar4NOT        DSBL4J
084500000526     C                   Z-ADD     §B4IAS        PTIAS
084600000526     C                   MOVEL     §B4VAS        PTVAS
084700000526     C                   ENDIF
084800000526      *
084900000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
085000000526      * gestite le giacenze per le bolle poste
085100000526      *
085200131113     C     kBLP          SETGT     FNEVB000
085300131113     C     kBLP          READPE    FNEVB000                               34
085400000526      * Devo cercare l'ultimo evento di tipo giacenza
085500000526      *
085600000526     C     *IN34         DOWEQ     *OFF
085700000526     C                   Z-ADD     1             WW
085800000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
085900131119     c                   if         *in42 and T2A(WW) ='G'
086000000526     C                   Z-ADD     EVBDEV        PTDTG
086100000619     C                   LEAVE
086200131119     C                   ENDIF
086300131119      *
086400131113     C     kBLP          READPE    FNEVB000                               34
086500000526     C                   ENDDO
086600000526     C                   ENDIF
086700000526      *
086800000526     C                   ENDSR
086900000526     C*
087000941228     C* CREO RECORD FNVAC01L -----------------------------------------*
087100921026     C     CARBLR        BEGSR
087200080417      *
087300080417     c                   clear                   fnvacds
087400080417      *
087500941228     C                   MOVEL     BLPAAS        VACAAS
087600941228     C                   MOVEL     BLPLNP        VACLNP
087700941228     C                   MOVEL     BLPMGS        VACMGS
087800941228     C                   MOVEL     BLPNRS        VACNRS
087900941228     C                   MOVEL     BLPNSP        VACNSP
088000941228     C                   MOVEL     BLPCBO        VACCBO
088100941228     C                   MOVEL     BLPLNA        VACLNA
088200941228     C                   MOVEL     BLPRSD        VACRSD
088300941228     C                   MOVEL     BLPPRD        VACPRD
088400941228     C                   MOVEL     BLPGC1        VACGC1
088500941228     C                   MOVEL     BLPGC2        VACGC2
088600941228     C                   MOVEL     BLPCTR        VACCTR
088700941228     C                   MOVEL     BLPCTS        VACCTS
088800941228     C                   MOVEL     BLPFTM        VACFTM
088900941228     C                   MOVEL     BLPFIN        VACFIN
089000941228     C                   MOVEL     BLPFAP        VACFAP
089100941228     C                   MOVEL     BLPTSP        VACTSP
089200000526      *
089300000526     C     §POST         IFEQ      'S'
089400000526     C                   MOVEL     PTIAS         VACIAS
089500000526     C                   MOVEL     PTVAS         VACVAS
089600000526     C                   ELSE
089700000526     C                   MOVEL     BLPIAS        VACIAS
089800000526     C                   MOVEL     BLPVAS        VACVAS
089900000526     C                   ENDIF
090000000526      *
090100941228     C                   MOVEL     BLPNAS        VACNAS
090200941228     C                   MOVEL     BLPNCL        VACNCL
090300941228     C                   MOVEL     BLPPKF        VACPKB
090400941228     C                   MOVEL     BLPVLF        VACVLB
090500941228     C                   MOVEL     BLPQFT        VACQFT
090600000526      *
090700000526     C     §POST         IFEQ      'S'
090800051115     C     ar9CAS        ANDEQ     *ZEROS
090900000526     C                   MOVEL     PTCAS         VACCAS
091000000526     C                   MOVEL     PTVCA         VACVCA
091100000526     C                   MOVEL     PTTIC         VACTIC
091200000526     C                   ELSE
091300051115     C                   MOVEL     ar9CAS        VACCAS
091400051115     C                   MOVEL     ar9VCA        VACVCA
091500051115     C                   MOVEL     ar9TIC        VACTIC
091600000526     C                   ENDIF
091700000526      *
091800060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
091900011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
092000011002      *
092100011002     C     *IN68         IFEQ      '1'
092200060214     C                   MOVEL     ar4KSC        VACCCM
092300011002     C                   ELSE
092400921026     C     BLPCCM        IFNE      0
092500941228     C                   MOVEL     BLPCCM        VACCCM
092600921026     C                   ELSE
092700941228     C                   MOVEL     BLPKSC        VACCCM
092800921026     C                   END
092900011002     C                   END
093000131113      *
093100941228     C                   MOVEL     BLPRMN        VACRMN
093200941228     C                   MOVEL     BLPRMA        VACRMA
093300941228     C                   MOVEL     BLPRMO        VACRMO
093400941228     C                   MOVEL     BLPFFD        VACFFD
093500131113      *
093600000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
093700000921     C     §POST         IFEQ      'S'
093800000921     C                   Z-ADD     BLPNCD        VACDCR
093900000921     C                   ELSE
094000000921     C                   MOVEL     BLPDCR        VACDCR
094100000921     C                   ENDIF
094200941228     C                   MOVEL     BLPTCR        VACTCR
094300941228     C                   MOVEL     BLPHCR        VACHCR
094400941228     C                   MOVEL     BLPTC1        VACTC1
094500941228     C                   MOVEL     BLPTC2        VACTC2
094600941228     C                   MOVEL     BLPDCM        VACDCM
094700941228     C                   MOVEL     BLPHMC        VACHMC
094800941228     C                   MOVEL     BLPCCA        VACCCA
094900941228     C                   MOVEL     WDLA          VACDLA
095000000526      *
095100000526     C     §POST         IFEQ      'S'
095200000526     C                   MOVEL     PTDTG         VACDAG
095300000526     C                   ELSE
095400941228     C  N33              Z-ADD     GCPMGC        VACDAG
095500941228     C  N33              MOVEL     GCPAGC        VACDAG
095600941228     C   33              MOVEL     *ZEROS        VACDAG
095700000526     C                   ENDIF
095800000526      *
095900990602     C*---------------------------------------------------------------*
096000990602     C* Se ho trovato un evanto di messa in consegna....
096100060912 1   C     WTRMIC        IFEQ      'S'
096200990602     C* ... e non ho la data di consegna totale sulla bolla ..
096300990602     C     BLPDCM        ANDEQ     0
096400040319      *
096500990602     C* Verifico se ho trovato una pratica C.A. aperta ...
096600060912 2   C     DATMAX        IFNE      0
096700990602     C     §3KCQ4        ANDEQ     'S'
096800990602     C* .. e se l'apertura di quest'ultima è successiva alla
096900990602     C*   messa in consegna ...
097000990602     C     DATMAX        ANDGE     WDTMIC
097100990602     C* .. allora imposto la data apertura C.A. nella data consegna
097200990602     C*    e imposto il tipo consegna anomala = 'A'
097300990602     C                   Z-ADD     1200          VACHMC
097400990602     C                   Z-ADD     DATMAX        VACDCM
097500990602     C                   MOVEL     'A'           VACCCA
097600060912 x2  C                   ELSE
097700990602     C* ... altrimenti imposto data messa in consegna
097800990602     C                   Z-ADD     EVBHEV        VACHMC
097900980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
098000990602     C                   MOVEL     'C'           VACCCA
098100060912 e2  C                   END
098200990602     C*
098300990602     C* ... se è stata impostata la data di consegna sulla bolla ..
098400060912 x1  C                   ELSE
098500040319     C*
098600060912 2   C     BLPDCM        IFNE      0
098700040319     C*
098800040319     C* Un tempo era stato asteriscato questo pezzo
098900040319     C* >>>>>>>>>>>>>
099000040409      * non deve essere fatto per l'estero
099100060912 3   C     wtpINV        Ifne      'E'
099200040409      *
099300990602     C* ... verifico se ho trovato una C.A. aperta ...
099400060912 4   C     DATMAX        IFNE      0
099500040319     C     §3KCQ4        ANDEQ     'S'
099600990602     C* .. e se l'apertura di quest'ultima è successiva alla
099700990602     C* .. consegna ...
099800040319     C     DATMAX        ANDGT     BLPDCM
099900040319     C*
100000990602     C* .. allora imposto la data apertura C.A. nella data consegna
100100990602     C*    e imposto il tipo consegna anomala = 'A'
100200040319     C                   Z-ADD     1200          VACHMC
100300040319     C                   Z-ADD     DATMAX        VACDCM
100400040319     C                   MOVEL     'A'           VACCCA
100500040319     C*
100600060912 e4  C                   ENDif
100700060912 e3  c                   end
100800040319     C* >>>>>>>>>>>>>
100900040319     C*
101000990602     C* ... se non è stata effettuata la messa in consegna, la
101100990602     C*     bolla risulta non consegnata, ....
101200060912 x2  C                   ELSE
101300990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
101400990602     C*     le consegne anomale, imposto la data consegna + flag
101500990602     C*     consegna anomala='A'
101600060912 3   C     DATMAX        IFNE      0
101700990602     C     §3KCQ4        ANDEQ     'S'
101800990602     C                   Z-ADD     1200          VACHMC
101900990602     C                   Z-ADD     DATMAX        VACDCM
102000990602     C                   MOVEL     'A'           VACCCA
102100060912 e3  C                   END
102200060912 e2  C                   END
102300060912 e1  C                   END
102400060912     C* Se ho trovato un evento di ripristino
102500060912 1   C     WTRrip        IFEQ      'S'
102600060912     C     BLPDCM        ANDEQ     0
102700060912     C     §3KCQ1        ANDEQ     'S'
102800060912     C                   Z-ADD     0             VACHMC
102900060912     C                   Z-ADD     0             VACDCM
103000060912     C                   MOVEL     'R'           VACCCA
103100060912 e1  c                   endif
103200060912
103300080417     c                   exsr      Write_VAC
103400930603     C                   SETON                                        06
103500990604     C*
103600990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
103700990618     C* e questa è stata anche.....
103800060912 1   C     §3KCQ1        IFEQ      'S'
103900080417     C     §3KCSD        ANDEQ     'EW'
104000080417      *
104100060912 2   C     VACCCA        IFEQ      'A'
104200990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
104300990618     C* invio al partner dell'informazione della consegna parziale
1044000609123    C     BLPDCM        IFEQ      0
104500990618     C     BLPDCP        ANDNE     0
104600990618     C                   Z-ADD     0             VACHMC
104700990618     C                   Z-ADD     0             VACDCM
104800990618     C                   MOVEL     ' '           VACCCA
104900080417     c                   exsr      Write_VAC
105000060912x3   C                   ELSE
105100990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
105200990618     C* invio al partner dell'informazione
105300060912 4   C     BLPDCM        IFEQ      0
105400990624     C     WDTMIC        ANDGT     0
105500990618     C                   Z-ADD     EVBHEV        VACHMC
105600990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
105700990618     C                   MOVEL     'C'           VACCCA
105800080417     c                   exsr      Write_VAC
105900060912e4   C                   END
106000060912e3   C                   END
106100990618     C*
106200990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
106300990618     C* scrivo anche record x apertura C.A.
106400060912x2   C                   ELSE
1065000609123    C     DATMAX        IFNE      0
106600990618     C     §3KCQ4        ANDEQ     'S'
106700990618     C                   Z-ADD     1200          VACHMC
106800990618     C                   Z-ADD     DATMAX        VACDCM
106900990618     C                   MOVEL     'A'           VACCCA
107000080417     c                   exsr      Write_VAC
107100060912 e3  C                   END
107200060912 e2  C                   END
107300060912 e1  C                   END
107400990618     C*
107500921026     C                   ENDSR
107600990602     C*---------------------------------------------------------------*
107700990602     C* Controllo se esiste una pratica C.A. aperta                  -*
107800990602     C*---------------------------------------------------------------*
107900990602     C     CRTCA         BEGSR
108000990602     C*
108100990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
108200990602     C* per reperire la pratica di danno con data più alta
108300990602     C                   Z-ADD     0             DATMAX            8 0
108400990602     C     §3KCQ4        IFEQ      'S'
108500040715      **
108600040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
108700040715      * SIA COME MAMMA CHE COME FIGLIA
108800040715      **
108900040715     c                   clear                   fidn12ds
109000040715     c                   eval      i12aas = blpAAS
109100040715     c                   eval      i12lnp = blpLNP
109200040715     c                   eval      i12nrs = blpNRS
109300040715     c                   eval      i12nsp = blpNSP
109400040715     c                   eval      i12fel = 'E'
109500040715     c                   eval      i12fan = 'E'
109600040715     c                   eval      i12fch = 'E'
109700040715     c                   call      'FIDN12R'
109800040715     c                   parm                    fidn12ds
109900040715     c                   z-add     *zeros        ii                2 0
110000040715      **
110100040715      * prende fra le C.A. aperte quella con la data ultima
110200040715     c                   if        o12nca > 0
110300040715     c                   dow       ii <  o12nca
110400040715      **
110500040715     c                   add       1             ii
110600040715     C                   movel     skDca(ii)     datape            8 0
110700040715     C     DATMAX        IFLT      DATAPE
110800040715     C                   Z-ADD     DATAPE        DATMAX
110900040715     C                   END
111000040715      *
111100040715     c                   enddo
111200040715     c                   endif
111300040715      *
111400040715     C                   END
111500040715     C*
111600990602     C                   ENDSR
111700970122     C*---------------------------------------------------------------*
111800970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
111900970122     C*---------------------------------------------------------------*
112000970121     C     TSTNMC        BEGSR
112100040422     **
112200040422     ** avendo fatto entrare anche x data consegna GT di 0
112300040422     ** il flag deve essere resettato solo per la vecchia condizione
112400040422     ** altrimenti non scrive + il VAC come una volta.
112500040422     c                   if        blpdcm = 0
112600040422     **
112700970122     C                   MOVEL     'N'           WFLDT3
112800090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
112900090223     C**   sempre il VAC.
113000090223     C********           MOVEL     'N'           WINVAC
113100040422     **
113200040422     c                   end
113300040422     **
113400970122     C                   MOVEL     'N'           WTREVE            1
113500970122     C                   MOVEL     *BLANKS       WSVTMC            1
113600970122     C                   MOVEL     *BLANKS       WMOT1            35
113700970122     C                   MOVEL     *BLANKS       WMOT2            35
113800970122     C*  Leggo le estensioni bolla della spedizione e per quelle
113900970122     C*  relative ad un motivo di mancata consegna scrivo VAG
114000970122     C                   DO        Y             W
114100970122     C                   MOVEL     T7K(W)        WTMC              1
114200970122     C                   MOVE      T7K(W)        KTRC
114300060214     C     Kar4          CHAIN     Fiar401L                           35
114400970123     C     *IN35         IFEQ      '0'
114500060214     C     ar4FTR        ANDNE     'T'
114600060214     C     ar4FTR        ANDNE     'C'
114700970122     C*  Se ho trovato estensione bolla e non è trasmesso
114800970122     C*  controllo se ho un nuovo tp.mancata consegna
114900970122     C*  e se si reperisco evento corrispondente
115000970122     C     WTMC          IFNE      WSVTMC
115100970122     C                   EXSR      REPEVE
115200970122     C                   END
115300970122     C*  se ho trovato l'evento controllo se devo scrivere
115400970122     C*  nuovo record VAG:
115500970122     C     WTREVE        IFEQ      'S'
115600970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
115700970122     C*  - se ho già scritto 2 motivi --> new record
115800970122     C     WTMC          IFNE      WSVTMC
115900970122     C     WMOT2         ORNE      *BLANKS
116000970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
116100970124     C     WSVTMC        IFNE      *BLANKS
116200970124     C     WMOT2         ANDEQ     *BLANKS
116300970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
116400970318     C*  primi 15 chr nella seconda motivazione
116500970318     C     WCHR15        IFNE      *BLANKS
116600970318     C                   MOVEL     WCHR15        VAGNOT
116700970318     C                   END
116800170322     C*
116900170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
117000170322     C                   eval      vgdTIP = 'VG'
117100170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
117200170322     C                   eval      vgdTSC = §3KGSD
117300170322     C                   eval      vgdDAT = dateu
117400170322     C                   eval      vgdPRG = g§7VASPRG
117500170322     C                   eval      vgdPGM = procname
117600170322     C                   eval      vgdSTO = '?'
117700170322     c                   if        §3KGSD  <> *blanks
117800170322     C                   WRITE     tivgd000
117900170322     c                   endif
118000970124     C                   SETON                                        06
118100970124     C                   END
118200970124     C*  New record
118300970122     C                   EXSR      NEWVAG
118400970122     C                   ELSE
118500970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
118600970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
118700970318     C     WCHR15        IFNE      *BLANKS
118800970318     C                   MOVEL     WCHR15        VAGNOT
118900060214     C                   MOVE      ar4NOT        VAGNOT
119000970318     C                   ELSE
119100970318     C*  Se codice generico occupo chr. da 32 posizione
119200970318     C     VAGCMC        IFEQ      'G  '
119300970318     C     VAGCMC        OREQ      'GEN'
119400060214     C                   MOVEL     ar4NOT        WCHR19           19
119500060214     C                   MOVE      ar4NOT        WCHR16           16
119600970318     C                   MOVE      WCHR19        VAGDMC
119700970318     C                   MOVEL     WCHR16        VAGNOT
119800970318     C                   ELSE
119900970318     C*  Altrimenti imposto solo ulteriore descrizione
120000060214     C                   MOVEL     ar4NOT        VAGNOT
120100970318     C                   END
120200970318     C                   END
120300060214     C                   MOVEL     ar4NOT        WMOT2
120400970122     C                   MOVEL     *BLANKS       WMOT1
120500170322     C*
120600170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
120700170322     C                   eval      vgdTIP = 'VG'
120800170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
120900170322     C                   eval      vgdTSC = §3KGSD
121000170322     C                   eval      vgdDAT = dateu
121100170322     C                   eval      vgdPRG = g§7VASPRG
121200170322     C                   eval      vgdPGM = procname
121300170322     C                   eval      vgdSTO = '?'
121400170322     c                   if        §3KGSD  <> *blanks
121500170322     C                   WRITE     tivgd000
121600170322     c                   endif
121700970122     C                   SETON                                        06
121800970122     C                   END
121900970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
122000970122     C                   MOVEL     'S'           WFLDT3
122100970122     C                   MOVEL     'S'           WINVAC
122200060214     C*  Fleggo Fiar4
122300060214     C                   MOVEL     'C'           ar4FTR
122400060214     C                   Z-ADD     DATEU         ar4DTR
122500060214     C                   EXCEPT    AGGar4
122600970122     C                   END
122700970122     C                   END
122800970122     C*
122900970122     C                   END
123000970122     C*  Controllo se mi è rimasto un VAG da scrivere
123100970122     C     WMOT1         IFNE      *BLANKS
123200970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
123300970318     C*  primi 15 chr nella seconda motivazione
123400970318     C     WCHR15        IFNE      *BLANKS
123500970318     C                   MOVEL     WCHR15        VAGNOT
123600970318     C                   END
123700170322     C*
123800170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
123900170322     C                   eval      vgdTIP = 'VG'
124000170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
124100170322     C                   eval      vgdTSC = §3KGSD
124200170322     C                   eval      vgdDAT = dateu
124300170322     C                   eval      vgdPRG = g§7VASPRG
124400170322     C                   eval      vgdPGM = procname
124500170322     C                   eval      vgdSTO = '?'
124600170322     c                   if        §3KGSD  <> *blanks
124700170322     C                   WRITE     tivgd000
124800170322     c                   endif
124900970122     C                   SETON                                        06
125000970122     C                   END
125100970122     C*
125200970122     C                   ENDSR
125300970122     C*---------------------------------------------------------------*
125400970122     C* Controllo se esiste evento                                   -*
125500970122     C*---------------------------------------------------------------*
125600970122     C     REPEVE        BEGSR
125700970122     C*
125800970122     C                   MOVEL     'N'           WTREVE
125900970122     C*  Mi posiziono su file eventi
126000131113     C     kBLP          SETGT     FNEVB01L
126100131113     C     kBLP          READPE    FNEVB01L                               34
126200131113      *
126300970122     C*  Loop letture fino a fine file o travato evento
126400970122     C     *IN34         DOWEQ     '0'
126500970122     C     WTREVE        ANDEQ     'N'
126600040504      **
126700040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
126800040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
126900040504      **  le A16/A23 come varianti in programmi che precedono facendo
127000040504      **  sballare il ragionamento in questo pgm.
127100040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
127200040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
127300040504     c                   movel     '0'           evbCEV
127400040504     c                   end
127500131113      *
127600131113      **  imposta sulla DS le note x testare il flag Progressivo
127700131113     c                   movel     evbNOT        Devb01
127800131119      *
127900131119      ** Sia per l'Italia che per L'estero deve sempre prendere l'ultimo EVENTO
128000131119      **  in quanto MANCATA CONSEGNA perchè la bolla sarà aggiornata solo con
128100131119      **  l'evento "U" o BLK
128200131119      *
128300131119     c                   if         §notPRG = ' ' or §notPRG = 'U'
128400131119      *
128500970122     C*  Reperisco tipo mancata consegna evento letto
128600970122     C                   Z-ADD     1             WW                4 0
128700970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
128800131113      *
128900970122     C*  Controllo se tipo mancata consegna evento letto
129000970122     C*  è uguale a quella dell'estensione bolla
129100131113      *
129200131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
129300131119     c                   if         *in42 and T2A(WW) = WTMC
129400970122     C                   MOVEL     'S'           WTREVE
129500131119     c                   leave
129600131119     C                   END
129700131113      *
129800131119     C                   EndIF
129900131119      *
130000131113     C     kBLP          READPE    FNEVB01L                               34
130100970122     C                   END
130200970122     C*
130300970122     C                   ENDSR
130400970122     C*---------------------------------------------------------------*
130500970122     C* Nuovo record FNVAG                                           -*
130600970122     C*---------------------------------------------------------------*
130700970122     C     NEWVAG        BEGSR
130800970122     C*
130900050606     C                   CLEAR                   FNVAGds
131000970721     C                   MOVEL     'S'           WRTVAG            1
131100970318     C                   MOVEL     *BLANKS       WCHR25           25
131200970318     C                   MOVEL     *BLANKS       WCHR15           15
131300970122     C                   Z-ADD     BLPAAS        VAGAAS
131400970122     C                   Z-ADD     BLPLNP        VAGLNP
131500970122     C                   Z-ADD     BLPNRS        VAGNRS
131600970122     C                   Z-ADD     BLPNSP        VAGNSP
131700970122     C                   Z-ADD     BLPLNA        VAGLNA
131800990810     C                   MOVEL     EVBDEV        WAA2              4 0
131900970123     C                   MOVE      WAA2          VAGAGC
132000970122     C                   MOVE      EVBDEV        VAGMGC
132100060214     C                   MOVEL     ar4NOT        VAGCMC
132200060214     C                   MOVEL     ar4NOT        WMOT1
132300970122     C                   MOVEL     *BLANKS       WMOT2
132400970307     C                   Z-ADD     1             YY                4 0
132500970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
132600970307     C     VAGCMC        IFNE      'G  '
132700970307     C     VAGCMC        ANDNE     'GEN'
132800970307     C                   MOVEL     D2A(YY)       VAGDMC
132900970318     C                   MOVE      D2A(YY)       WCHR25
133000970318     C                   MOVEL     WCHR25        WCHR15
133100970305     C                   ELSE
133200060214     C                   MOVE      ar4NOT        WCOM31           31
133300970122     C                   MOVEL     WCOM31        VAGDMC
133400970305     C                   END
133500970122     C                   MOVEL     BLPRMA        VAGRMA
133600970122     C                   Z-ADD     BLPRMN        VAGRMN
133700970122     C     BLPCCM        IFNE      0
133800970122     C                   MOVEL     BLPCCM        VAGSCM
133900970122     C                   ELSE
134000970122     C                   MOVEL     BLPKSC        VAGSCM
134100970122     C                   END
134200970123     C*
134300970123     C                   MOVEL     WTMC          WSVTMC
134400970122     C*
134500970122     C                   ENDSR
134600001031     C*---------------------------------------------------------------*
134700001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
134800001031     C*---------------------------------------------------------------*
134900001031     C     PARZIA        BEGSR
135000001031     C*
135100001031      * LOOP SU DETTAGLIO COLLI
135200001103     C                   Z-ADD     0             TOTCO
135300001031     C                   Z-ADD     0             CONS
135400001102     C                   Z-ADD     0             ULTIMA            8 0
135500001031     C     KBLP          SETLL     FNBLT01L
135600001031     C                   DO        *HIVAL
135700001102     C     KBLP          READE     FNBLT01L                               97
135800001031     C   97              LEAVE
135900001031      * CONTA CONSEGNATI E NON CONSEGNATI
136000001103     C     BLTDCM        IFGT      0
136100001031     C                   ADD       1             CONS              3 0
136200001031     C                   END
136300001103     C                   ADD       1             TOTCO             3 0
136400001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
136500001031     C     BLTDCM        IFGT      ULTIMA
136600001031     C                   Z-ADD     BLTDCM        ULTIMA
136700001031     C                   ENDIF
136800001031     C                   ENDDO
136900001102     C                   MOVE      CONS          CONSA             3
137000001103     C                   MOVE      TOTCO         TOTCOA            3
137100001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
137200001031      *
137300050606     C                   CLEAR                   FNVAGds
137400001031     C                   MOVE      BLPLNP        VAGLNP
137500001031     C                   MOVE      BLPAAS        VAGAAS
137600001031     C                   MOVE      BLPNRS        VAGNRS
137700001031     C                   MOVE      BLPNSP        VAGNSP
137800001107     C                   MOVEL     'P  '         VAGCMC
137900001031     C                   MOVE      BLPRMN        VAGRMN
138000001031     C                   MOVE      BLPRMA        VAGRMA
138100001031     C                   MOVE      BLPLNA        VAGLNA
138200001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
138300001031     C                   MOVEL     COS(1)        VAGDMC
138400001103     C                   MOVEA     COS(2)        COM
138500001102     C                   MOVEA     CONSA         COM(22)
138600001103     C                   MOVEA     TOTCOA        COM(29)
138700001102     C                   MOVEA     COM           VAGNOT
138800001031      *
138900001031     C     BLPCCM        IFGT      0
139000001031     C                   Z-ADD     BLPCCM        VAGSCM
139100001031     C                   ELSE
139200001031     C                   Z-ADD     BLPKSC        VAGSCM
139300001031     C                   END
139400001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
139500001031     C                   MOVEL     ULTIMA        VAGAGC
139600001031     C                   MOVE      ULTIMA        VAGMGC
139700170322     C*
139800170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
139900170322     C                   eval      vgdTIP = 'VG'
140000170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
140100170322     C                   eval      vgdTSC = §3KGSD
140200170322     C                   eval      vgdDAT = dateu
140300170322     C                   eval      vgdPRG = g§7VASPRG
140400170322     C                   eval      vgdPGM = procname
140500170322     C                   eval      vgdSTO = '?'
140600170322     c                   if        §3KGSD  <> *blanks
140700170322     C                   WRITE     tivgd000
140800170322     c                   endif
140900170322     C*
141000001106     C                   MOVEL     'S'           WTRPAR
141100001031      *
141200001031     C                   ENDSR
141300080417      *------------------------------------------------------------------------*
141400080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
141500080417      *------------------------------------------------------------------------*
141600080417     C     Write_VAC     BEGSR
141700080417      *
141800090529      *  >>>>>>>>>
141900090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
142000090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
142100090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
142200090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
142300090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
142400090529      *   il record.
142500090529      *  >>>>>>>>>
142600090603     c                   if        §3kCTB ='S'
142700090603     c                   clear                   uguale_VAC        1
142800090529      *
142900090529     C                   movel     'VAC'         Kar5vac
143000090529     c     kar5          chain     fiar501l
143100090529      *
143200090529     c                   if        %Found(fiar501l)
143300090529      * Imposta i campi precedentemente salvati nella DS da comparare
143400090529     c                   eval      fnvacHGds = ar5UNI
143500090603      *
143600090603     c                   eval      VacGC1chg = VacGC1
143700090603     c                   eval      VacGC2chg = VacGC2
143800090603     c                   eval      VacFFDchg = VacFFD
143900090603     c                   eval      VacTCRchg = VacTCR
144000090603     c                   eval      VacDCRchg = VacDCR
144100090603     c                   eval      VacHCRchg = VacHCR
144200090603     c                   eval      VacDCMchg = VacDCM
144300090603     c                   eval      VacHMCchg = VacHMC
144400090603     c                   eval      VacTC1chg = VacTC1
144500090603     c                   eval      VacTC2chg = VacTC2
144600090603     c                   eval      VacCCAchg = VacCCA
144700090603     c                   eval      VacDLAchg = VacDLA
144800090603     c                   eval      VacDAGchg = VacDAG
144900090529      * quindi
145000090529      *    controllo di uguaglianza
145100090529     c                   if        fnvacHGds = DSvacHG
145200090529     c                   eval      uguale_VAC = 'S'
145300090603     c                   move      '='           ar5UNI
145400090603     c                   else
145500090603     c                   move      ' '           ar5UNI
145600090603     c                   eval      ar5UNI    = DSvacHG
145700090603     c                   end
145800090529      *
145900090529     c                   eval      AR5DAC = dateu
146000090529     c                   eval      AR5ORC = Orario
146100090529     c                   eval      AR5PRG = AR5PRG + 1
146200090529     c*
146300090529     c                   update    fiar5000
146400090529     c*
146500090529     c                   if        uguale_VAC = 'S'
146600090529     c                   leavesr
146700090529     c                   end
146800090529      *
146900090529     c                   else
147000090529      *
147100090529      *  lo memorizza x la prima volta
147200090529     c                   clear                   fiar5000
147300090529     c                   eval      ar5AAS = blpAAS
147400090529     c                   eval      ar5LNP = blpLNP
147500090529     c                   eval      ar5NRS = blpNRS
147600090529     c                   eval      ar5NSP = blpNSP
147700090529     c                   eval      ar5TRD = 'VAC'
147800090529     c                   eval      ar5DAC = dateu
147900090529     c                   eval      ar5ORC = Orario
148000090529     c                   eval      ar5PRG = AR5PRG + 1
148100090603      *
148200090603     c                   eval      VacGC1chg = VacGC1
148300090603     c                   eval      VacGC2chg = VacGC2
148400090603     c                   eval      VacFFDchg = VacFFD
148500090603     c                   eval      VacTCRchg = VacTCR
148600090603     c                   eval      VacDCRchg = VacDCR
148700090603     c                   eval      VacHCRchg = VacHCR
148800090603     c                   eval      VacDCMchg = VacDCM
148900090603     c                   eval      VacHMCchg = VacHMC
149000090603     c                   eval      VacTC1chg = VacTC1
149100090603     c                   eval      VacTC2chg = VacTC2
149200090603     c                   eval      VacCCAchg = VacCCA
149300090603     c                   eval      VacDLAchg = VacDLA
149400090603     c                   eval      VacDAGchg = VacDAG
149500090529     c                   eval      ar5UNI = DSvacHG
149600090529     c                   write     fiar5000
149700090529      *
149800090529     c                   end
149900090529     c                   end
150000090529      *  >>>>>>>>>
150100170322     C*
150200170322     C* Verifico rottura di codice per cliente unificante
150300170322     C                   exsr      ROTVAC
150400170322     C*
150500080417     C                   clear                   tivgd000
150600170322     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
150700170322     C                   eval      vgdTIP = 'VC'
150800170322     C                   eval      vgdKSU = '0'+%editc(§3KCKS:'X')
150900170322     C                   eval      vgdTSC = §3KCSD
151000170322     C                   eval      vgdDAT = dateu
151100170322     C                   eval      vgdPRG = o§7VASPRG
151200170322     C                   eval      vgdPGM = procname
151300170322     C                   eval      vgdSTO = '?'
151400080417      *
151500080417      *  x generare l'EDI:
151600080417      *    occorre farlo in modo controllato (personalizzato)
151700080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
151800080417     C                   clear                   DVGDFLO
151900080417     C                   movel     'P'           §VGDFELA
152000080417     C                   movel     DVGDFLO       vgdflo
152100170322     C                   eval      vgdPRG = *blanks
152200170322     C                   eval      vgdSTO = *blanks
152300080417     c                   end
152400080417      *
152500091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
152600091116      *  Non deve scrivere nulla.
152700170322     c                   if        §3KCSD <> *blank
152800080417     C                   write     tivgd000
152900110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
153000110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
153100110721     C                   MOVEL     'S'           WFLDT3
153200091116     c                   end
153300080417      *
153400080417     C                   ENDSR
153500170322
153600170322
153700170322
153800170322     C*------------------------------------------------------------------------*
153900170322     C* EXEERR - Routine di esecuzione azioni su Errore
154000170322     C*------------------------------------------------------------------------*
154100170322     C     EXEERR        BEGSR
154200170322     C*
154300170322     C                   dump(A)
154400170322     C                   rolbk(e)
154500170322     C                   seton                                        lr
154600170322     C                   return
154700170322     C*
154800170322     C                   ENDSR
154900170322     C*------------------------------------------------------------------------*
155000170322
155100170322
155200170322
155300170322     C*------------------------------------------------------------------------*
155400170322     C* ROTVAC - Routine di gestione rottura di codice VAC
155500170322     C*------------------------------------------------------------------------*
155600170322     C     ROTVAC        BEGSR
155700170322     C*
155800170322     C* Verifico rottura di codice per cliente unificante
155900170322     C                   if        §3KCKS <> s3KCKS
156000170322     C*
156100170322     C* Stacco progressivo univoco download
156200170322     C                   exsr      RTVPRG
156300170322     C*
156400170322     C* Se non primo giro => Finalizzo transazione precedente
156500170322     C                   if        s§7VASPRG <> *blanks
156600170322     C                   exsr      RLSPRG
156700170322     C                   endif
156800170322     C*
156900170322     C* Salvo l'unificante corrente
157000170322     C                   eval      s3KCKS = §3KCKS
157100170322     C                   eval      s3KCSD = §3KCSD
157200170322     C*
157300170322     C* Salvo il progressivo univoco download corrente
157400170322     C                   eval      s§7VASPRG = o§7VASPRG
157500170322     C                   endif
157600170322     C*
157700170322     C                   ENDSR
157800170322     C*------------------------------------------------------------------------*
157900170322
158000170322
158100170322
158200170322     C*------------------------------------------------------------------------*
158300170322     C* RTVPRG - Routine di reperimento progressivo univoco transazione
158400170322     C*------------------------------------------------------------------------*
158500170322     C     RTVPRG        BEGSR
158600170322     C*
158700170322     c                   CLEAR                   TIS7VASDS
158800170322     c                   EVAL      i§7VASOPZ = 'PRG'
158900170322     c                   CALL(e)   'TIS7VASR1'
159000170322     c                   PARM                    TIS7VASDS
159100170322     c*
159200170322     c                   if        %error OR
159300170322     c                             o§7VASOK = *off OR
159400170322     c                             o§7VASPRG = *blanks
159500170322     c                   exsr      EXEERR
159600170322     c                   endif
159700170322     C*
159800170322     C                   ENDSR
159900170322     C*------------------------------------------------------------------------*
160000170322
160100170322
160200170322
160300170322     C*------------------------------------------------------------------------*
160400170322     C* RLSPRG - Routine di finalizzazione transazione
160500170322     C*------------------------------------------------------------------------*
160600170322     C     RLSPRG        BEGSR
160700170322     C*
160800170322     C                   EVAL      i§7VASOPZ  = 'RLS'
160900170322     C                   EVAL      i§7VASTIP  = 'VC'
161000170322     C                   EVAL      i§7VASKSU  = '0'+%editc(s3KCKS:'X')
161100170322     C                   EVAL      i§7VASTSC  = s3KCSD
161200170322     C                   EVAL      i§7VASSTO  = '?'
161300170322     C                   EVAL      i§7VASSTTO = 'G'
161400170322     C                   EVAL      i§7VASPRG  = s§7VASPRG
161500170322     C                   CALL(e)   'TIS7VASR1'
161600170322     C                   PARM                    TIS7VASDS
161700170322     C*
161800170322     C* Verifico esito chiamata
161900170322     C                   if        %error OR o§7VASOK = *off
162000170322     C                   exsr      EXEERR
162100170322     C                   endif
162200170322     C*
162300170322     C                   ENDSR
162400170322     C*------------------------------------------------------------------------*
162500170322
162600170322
162700170322
162800170322     C*------------------------------------------------------------------------*
162900170322     C* RLTPRG - Routine di finalizzazione transazione (solo TIP)
163000170322     C*------------------------------------------------------------------------*
163100170322     C     RLTPRG        BEGSR
163200170322     C*
163300170322     C                   EVAL      i§7VASOPZ  = 'RLT'
163400170322     C                   EVAL      i§7VASTIP  = 'VG'
163500170322     C                   EVAL      i§7VASSTO  = '?'
163600170322     C                   EVAL      i§7VASSTTO = 'G'
163700170322     C                   EVAL      i§7VASPRG  = g§7VASPRG
163800170322     C                   CALL(e)   'TIS7VASR1'
163900170322     C                   PARM                    TIS7VASDS
164000170322     C*
164100170322     C* Verifico esito chiamata
164200170322     C                   if        %error OR o§7VASOK = *off
164300170322     C                   exsr      EXEERR
164400170322     C                   endif
164500170322     C*
164600170322     C                   ENDSR
164700170322     C*------------------------------------------------------------------------*
164800170322
164900060214     OFiar4000  E            AGGar4
165000060214     O                       ar4FTR
165100060214     O                       ar4DTR
165200921026**
165300981210DLTOVR FILE(*ALL)
165400000726**
165500921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
165600921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
165700921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
165800001031**
165900001031Consegna parziale
166000001031Nr. colli consegnati xxx su xxx
