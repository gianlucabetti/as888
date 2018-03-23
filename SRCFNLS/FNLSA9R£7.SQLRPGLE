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
002000040715     F***FNDCT02L  IF   E           K DISK
002100990602     FFNLBL01L  IF   E           K DISK
002200060921     Ffnlbl02L  IF   E           K DISK    rename(fnlbl000:fnlbl2)
002300060921     f                                     prefix(ex)
002400971126     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002500001102     FFNBLT01L  IF   E           K DISK
002600060214     FFNBLP67J  IF   E           K DISK
002700010927     F                                     RENAME(FNBLP000:FNBLPJ)
002800010927     FFNBLP01L  UF   E           K DISK
002900010927     F                                     RENAME(FNBLP000:FNBLP1)
003000050606     ftivgd00f  o    e             disk
003100971126     D*--------------------------------------------------------------*
003200981210     D CMD             S             49    DIM(1) CTDATA PERRCD(1)
003300921026     D MSG             S             67    DIM(3) CTDATA PERRCD(1)
003400001031     D COS             S             50    DIM(2) CTDATA PERRCD(1)
003500001102     D COM             S              1    DIM(31)
003600921026     D SK              S              1    DIM(201)
003700970122     D* Schiera per caricamento tipi record estensione bolla
003800970122     D T7K             S              2    DIM(50)
003900970122     D* Schiera per caricamento codici e tipi mancate consegne
004000970122     D C2A             S              3    DIM(200)
004100970122     D T2A             S              1    DIM(200)
004200970305     D D2A             S             75    DIM(200)
004300000525     D FIP             S              3  0 DIM(50)
004400170322
004500170322     D psds           sds
004600170322     D  procname         *PROC
004700170322     D TIS7VASDS     E DS                  inz
004800170322
004900131113      * Note degli Eventi
005000131113     D Devb01        e ds
005100080417     D DVGDFLO       e ds
005200080417     D FNVACDS       e ds
005300090529     D FNVAChgDS     e ds
005400090603     D  DSVAChg        ds
005500090603     d    VacGC1chg                        like(VacGC1)
005600090603     d    VacGC2chg                        like(VacGC2)
005700090603     d    VacFFDchg                        like(VacFFD)
005800090603     d    VacTCRchg                        like(VacTCR)
005900090603     d    VacDCRchg                        like(VacDCR)
006000090603     d    VacHCRchg                        like(VacHCR)
006100090603     d    VacDCMchg                        like(VacDCM)
006200090603     d    VacHMCchg                        like(VacHMC)
006300090603     d    VacTC1chg                        like(VacTC1)
006400090603     d    VacTC2chg                        like(VacTC2)
006500090603     d    VacCCAchg                        like(VacCCA)
006600090603     d    VacDLAchg                        like(VacDLA)
006700090603     d    VacDAGchg                        like(VacDAG)
006800080417      *
006900941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007000941228     D WLBDAT          DS                  INZ
007100941228     D  G02DAT                 1      8  0
007200941228     D  G02INV                 9     16  0
007300941228     D  G02ERR                17     17
007400941228     D  G02TGI                18     22  0
007500900517     D KPJBA         E DS
007600921023     D DS3K          E DS
007700170309     D tabelds       e ds                  extname('TABEL00F')
007800970122     D DS7K          E DS
007900941228     D DS2A          E DS
008000000525     D OG143         E DS
008100000526     D DSBL4J        E DS
008200050606     D fnvagds       E DS
008300040715
008400040715     d Fidn12ds      e ds
008500040715     d  skKey                 26    305    dim(20)
008600040715     d  skAnn                306    325    dim(20)
008700040715     d  skDca                326    485    dim(20)
008800040715     d  skFca                486    545  0 dim(20)
008900040715     d  skDch                546    705  0 dim(20)
009000040715     d  skCch                706    745    dim(20)
009100170309
009200170309     d wSQl            s           1024    inz  varying
009300170322     d s3KCKS          s                   like(§3KCKS) inz
009400170322     d s3KCSD          s                   like(§3KCSD) inz
009500170322     d s§7VASPRG       s                   like(o§7VASPRG) inz
009600170322     d g§7VASPRG       s                   like(o§7VASPRG) inz
009700170322
009800971126      *----------------------------------------------------------------
009900971126     IAZORG01L  AA
010000971126     I                             P    4    5 0ORGFIL
010100971126     I                                 11   11  ORGFL1
010200000525     I                               3776 3800  ORGDE3
010300090408     IFNARBG00      51
010400090408     IFNARBK00      52
010500920325     C*---------------------------------------------------------------*
010600920812     C* INDICATORI
010700920812     C*---------------------------------------------------------------*
010800921026     C* 01    - NON TUTTI I MEMBRI SONO STATI ALLOCATI
010900921026     C* 30    - LETTURA FLBLP00F
011000921026     C* 31    - LETTURA TABEL00F
011100921026     C* 32    - VEDO SE ESISTE GIA' BOLLA SU FLBLR
011200921026     C* 33    - GIACENZA
011300921026     C* 34    - COSEGNA PARTICOLARE NUMERICA (COD ANOMALIA)
011400970122     C* 35    - TROVATE ESTENSIONI BOLLE X MANCATE CONSEGNE
011500970123     C* 99    - ESEGUO CHIUSURA E DISALLOCAZIONE MEMBRO FNVAC
011600920812     C*---------------------------------------------------------------*
011700000000     C     *ENTRY        PLIST
011800000000     C                   PARM                    KPJBA
011900980407     C                   MOVEL     KPJBU         WTPINV            2
012000921026     C                   Z-ADD     1             CODUT
012100170322     C*
012200170322     C* Avvio il monitoring del processo
012300170322     C                   monitor
012400170322     C*
012500080417     C*---------------------------------------------------------------*
012600080417      *?  Definizione Campi e Caricamento Tabelle   ?
012700080417     C*---------------------------------------------------------------*
012800080417      *
012900060921     C     Klbl2         KLIST
013000060921     C                   KFLD                    klb2_AAS
013100060921     C                   KFLD                    klb2_LNP
013200060921     C                   KFLD                    klb2_NRS
013300060921     C                   KFLD                    klb2_NSP
013400060921     C     Klbl2p        KLIST
013500060921     C                   KFLD                    exLBLAAp
013600060921     C                   KFLD                    exLBLLPp
013700060921     C                   KFLD                    exLBLNRp
013800060921     C                   KFLD                    exLBLNSp
013900060921     C     KBLP          KLIST
014000060921     C                   KFLD                    BLPAAS
014100060921     C                   KFLD                    BLPLNP
014200060921     C                   KFLD                    BLPNRS
014300060921     C                   KFLD                    BLPNSP
014400060214     C     Kar4          KLIST
014500970122     C                   KFLD                    BLPAAS
014600970122     C                   KFLD                    BLPLNP
014700970122     C                   KFLD                    BLPNRS
014800970122     C                   KFLD                    BLPNSP
014900970122     C                   KFLD                    KTRC
015000921026     C     KBLP2         KLIST
015100921026     C                   KFLD                    COD99             7 0
015200921026     C                   KFLD                    KSC
015300920316     C     KTAB          KLIST
015400921026     C                   KFLD                    CODUT             1 0
015500921026     C                   KFLD                    COD               2
015600941228     C     KTAB2         KLIST
015700941228     C                   KFLD                    CODUT             1 0
015800941228     C                   KFLD                    COD               2
015900941228     C                   KFLD                    KEY               8
016000090529     C     Kar5          KLIST
016100090529     C                   KFLD                    BLPAAS
016200090529     C                   KFLD                    BLPLNP
016300090529     C                   KFLD                    BLPNRS
016400090529     C                   KFLD                    BLPNSP
016500090529     C                   KFLD                    Kar5vac           3
016600941228     C**
016700941228     C* DEFINIZIONE CAMPI
016800941228     C**
016900941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
017000060214     C     *LIKE         DEFINE    ar4TRC        KTRC
017100051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
017200051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
017300051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
017400000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
017500000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
017600000526     C     *LIKE         DEFINE    VACDAG        PTDTG
017700060921      *
017800060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
017900060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
018000060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
018100060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
018200941228     C*---------------------------------------------------------------*
018300921026     C* GIRO UDATE
018400941228     C                   TIME                    W0140            14 0
018500941228     C* UDATE IN GGMMAAAA
018600941228     C                   MOVE      W0140         WDTGIO            8 0
018700090529     C                   MOVEl     W0140         Orario            6 0
018800941228     C*
018900941228     C* UDATE IN AAAAMMGG
019000941228     C                   Z-ADD     WDTGIO        G02DAT
019100941228     C                   MOVEL     *BLANK        G02ERR
019200941228     C                   CALL      'XSRDA8'
019300941228     C                   PARM                    WLBDAT
019400941228     C                   MOVEL     G02INV        DATEU             8 0
019500941228     C*---------------------------------------------------------------*
019600000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
019700000525     C                   CLEAR                   FIP
019800000525     C                   Z-ADD     0             Y
019900000525     C     *LOVAL        SETLL     AZORG01L
020000000525     C                   DO        *HIVAL
020100000525     C                   READ      AZORG01L                               95
020200000525     C   95              LEAVE
020300000525     C                   MOVEL     ORGDE3        OG143
020400020729     C     §OgNTW        IFEQ      'PPT'
020500000525     C                   Z-ADD     1             Y
020600000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
020700000525     C   23              Z-ADD     ORGFIL        FIP(Y)
020800000525     C                   END
020900000525     C                   END
021000000525     C*---------------------------------------------------------------*
021100970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
021200970122     C                   Z-ADD     0             Y                 4 0
021300970122     C                   MOVEA     *HIVAL        T7K
021400970122     C                   MOVEL     '7K'          COD
021500970122     C     KTAB          CHAIN     TABEL                              31
021600970122     C     *IN31         DOWEQ     '0'
021700970122     C     TBLFLG        IFEQ      ' '
021800970122     C* Controllo se impostato tipo mancata consegna
021900970122     C                   MOVEL     TBLUNI        DS7K
022000970122     C     §7KTMC        IFNE      *BLANKS
022100970122     C                   ADD       1             Y
022200970122     C                   MOVEL     §7KTMC        T7K(Y)
022300970123     C                   MOVEL     TBLKEY        WTRC              1
022400970123     C                   MOVE      WTRC          T7K(Y)
022500970122     C                   END
022600970122     C                   END
022700970122     C     KTAB          READE     TABEL                                  31
022800970122     C                   END
022900970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
023000970122     C     Y             IFGT      0
023100970122     C                   SORTA     T7K
023200970122     C                   END
023300970122     C*---------------------------------------------------------------*
023400970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
023500970122     C                   Z-ADD     0             W                 4 0
023600970122     C                   MOVEA     *HIVAL        C2A
023700970305     C                   MOVEA     *HIVAL        D2A
023800970122     C                   MOVEA     *HIVAL        T2A
023900970122     C                   MOVEL     '2A'          COD
024000970122     C     KTAB          CHAIN     TABEL                              31
024100970122     C     *IN31         DOWEQ     '0'
024200970122     C     TBLFLG        IFEQ      ' '
024300970122     C* Caricamento evento + tipo mancata consegna
024400970122     C                   MOVEL     TBLUNI        DS2A
024500970122     C     §2AFTC        IFNE      *BLANKS
024600970122     C                   ADD       1             W
024700970122     C                   MOVEL     §2AFTC        T2A(W)
024800970305     C                   MOVEL     §2ADES        D2A(W)
024900970123     C                   MOVEL     TBLKEY        C2A(W)
025000970122     C                   END
025100970122     C                   END
025200970122     C     KTAB          READE     TABEL                                  31
025300970122     C                   END
025400080417     C*---------------------------------------------------------------*
025500080417      * ?  CICLO Principale del Programma              ?
025600970122     C*---------------------------------------------------------------*
025700170322     C*
025800170323     C* Forzo ZERO come progressivo univoco download specifico per il VG
025900170323     C                   eval      g§7VASPRG = *all'0'
026000170322     C*
026100170322     C*---------------------------------------------------------------*
026200080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
026300080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
026400921023     C                   MOVEL     '3K'          COD
026500170309     c                   exsr      setll_sql
026600170309     c                   exsr      fetch_sql
026700921023     C*
026800921023     C     *IN31         DOWEQ     '0'
026900131113      *
027000921023     C     TBLFLG        IFEQ      ' '
027100980407     C                   MOVEL     TBLUNI        DS3K
027200080417      * ?  Se Estero:  ?
027300980409     C     WTPINV        IFEQ      'E'
027400080417     C     §3KCSD        ANDEQ     'EW'
027500980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
027600080417      * ?  oppure se NON Estero e non EDI:  ?
027700980409     C     WTPINV        ORNE      'E'
027800080417     C     §3KCSD        ANDNE     'EW'
027900091116     C     §3KCSD        ANDNE     *blank
028000091116      *
028100921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
028200921026     C                   MOVEL     TBLKEY        KSC               7 0
028300930811     C                   MOVEL     KSC           COD99
028400930811     C                   MOVE      9999          COD99
028500011002     C                   MOVEL     KSC           KSCJ              7
028600930811     C*
028700941228     C     KSC           SETLL     FNBLP000                               32
028800941228     C  N32KBLP2         SETLL     FNBLP000                               32
028900060214     C  N32KSCJ          SETLL     FNBLP67J                               32
029000921023     C*
029100930811     C     *IN32         IFEQ      *ON
029200131113     C*                                                SALVO LA KEY
029300941228     C                   MOVEL     TBLKEY        SAVKEY
029400930811     C                   EXSR      ELAB
029500941228     C*
029600131113     C* RICHAINO IL RECORD DI TABEL
029700941228     C                   MOVEL     '3K'          COD
029800941228     C                   MOVEL     SAVKEY        KEY
029900941228     C     KTAB2         CHAIN     TABEL                              31
030000930811     C                   Z-ADD     DATEU         §3KCDU
030100930811     C                   MOVEL     DS3K          TBLUNI
030200930811     C                   UPDATE    TABEL
030300131113      *
030400921026     C                   END
030500131113      *
030600921023     C                   END
030700980409     C                   END
030800921023     C*
030900170309     c                   exsr      fetch_sql
031000921023     C                   END
031100170309     c*
031200170309     c* chiudo il cursore
031300170309     c                   exsr      close_sql
031400170322     C*
031500170322     C* Finalizzo la transazione in sospeso VAC
031600170322     C                   exsr      RLSPRG
031700170322     C*
031800170322     C* Finalizzo la transazione VAG
031900170322     C                   exsr      RLTPRG
032000981210     C*
032100981210     C* CALCELLO LE OVR ESEGUITE
032200981210     C                   MOVEL     *BLANKS       COMMAN           80
032300981210     C                   Z-ADD     17            LUNG
032400981210     C                   MOVEA     CMD(1)        COMMAN
032500981210     C                   CALL      'QCMDEXC'                            21
032600981210     C                   PARM                    COMMAN
032700080417     C                   PARM                    LUNG             15 5
032800080417      **
032900080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
033000980409     C   06WTPINV        IFEQ      'E'
033100110419     C                   CALL      'TRTCT80R'
033200030924     C                   parm                    Kpjba
033300980409     C                   END
033400170322     C*
033500170322     C* Su errore
033600170322     C                   on-error
033700170322     C                   exsr      exeerr
033800170322     C*
033900170322     C* Fine monitoring
034000170322     C                   endmon
034100080417     C*
034200921023     C                   SETON                                        LR
034300080417      *================================================================
034400921023     C* ELABORO SPEDIZIONI DEL CLIENTE -------------------------------*
034500080417      *================================================================
034600921023     C     ELAB          BEGSR
034700040319     C*
034800941228     C     KSC           SETLL     FNBLP000
034900941228     C     KSC           READE     FNBLP000                               30
035000921026     C*
035100921026     C     *IN30         DOWEQ     '0'
035200921026     C                   EXSR      CONTR
035300941228     C     KSC           READE     FNBLP000                               30
035400921026     C                   END
035500921026     C*
035600921026     C* LEGGO I 9999 CODIFICATI
035700921026     C*
035800941228     C     KBLP2         SETLL     FNBLP000
035900941228     C     KBLP2         READE     FNBLP000                               30
036000941228     C*
036100921026     C     *IN30         DOWEQ     '0'
036200921026     C                   EXSR      CONTR
036300941228     C     KBLP2         READE     FNBLP000                               30
036400921026     C                   END
036500040319     C*
036600010927     C* INSERISCO LOOP SU FILE DI JOIN PER VEDERE SE ESISTONO RECORD
036700060214     C* SU ar4 CON CHIAVE KSC IN ar4NOT
036800010927      *
036900060214     C     KSCJ          SETLL     FNBLP67J
037000010927     C                   DO        *HIVAL
037100060214     C     KSCJ          READE     FNBLP67J                               66
037200010927     C   66              LEAVE
037300010927     C     KBLP          CHAIN     FNBLP01L                           67
037400010927     C     *IN67         IFEQ      '0'
037500010927     C                   SETON                                        68
037600010927     C                   EXSR      CONTR
037700010927     C                   SETOFF                                       68
037800010927     C                   ENDIF
037900010927     C                   ENDDO
038000040319     C*
038100010927     C                   SETOFF                                       68
038200040319     C*
038300921026     C                   ENDSR
038400921026     C*
038500921026     C* VEDO SE ELABORARE BOLLA --------------------------------------*
038600921026     C     CONTR         BEGSR
038700990602     C*
038800990602     C* Escludo bolle figlie xchè mando già via i dati tramite la mamma
038900131113     C     KBLP          CHAIN     FNLBL01L                           23
039000921026     C*
039100040319     C*  Se non è una bolla dirottata in quanto non è figlia di
039200040319     C*   un'altra bolla......
039300990602     C     *IN23         IFEQ      '1'
039400040319     C*   Oppure.....
039500040319     C*   .... se trovato il legame deve essere un dirottamento legato
039600040319     C*       non all'Estero e deve essere un discorso di reso/dirottam
039700040319     C*        Italia
039800040319     C     *in23         oreq      *off
039900040319     C     wtpINV        andne     'E'
040000040319     C     §3kcsd        andne     'EW'
040100040319     C     lblLAN        andne     lblLAP
040200040319     C     blpCCA        andne     *blank
040300040319     C*
040400050309     C     KBLP          CHAIN     tigcp000                           33
040500990602     C* 33 OFF - GIACENZA
040600961204     C*  Se giacenza bolla estero mando la data giacenza solo quando
040700961204     C*  l'ufficio internazionale mi ha scritto l'ulteriore motivaz.
040800961204     C*  in inglese
040900971126     C     *IN33         IFEQ      '0'
041000971126     C                   MOVEL     *BLANKS       ORGFL1
041100971126     C     GCPLNP        CHAIN     AZORG01L                           36
041200020717     C  n36              MOVEL     ORGDE3        OG143
041300020717     C  n36§OgNTW        ifeq      'FED'
041400020717     C     §OgNTW        oreq      'EEX'
041500020717     C     §OgNTW        oreq      'EUP'
041600020717     C     GCPFAS        Iflt      15
041700961204     C                   SETON                                        33
041800020717     C                   End
041900961204     C                   END
042000971126     C                   END
042100941228     C*
042200990602     C* Controllo se esiste una pratica di danno aperta
042300990602     C                   EXSR      CRTCA
042400941228     C*
042500060921      **?Attenzione:  a fronte di Bolla con Bolle figlie legate su cui
042600060921      *   è presente il Contrassegno, si vuole inviare al cliente
042700060921      *   l'info del contrassegno sul VAC della bolla madre.
042800060921      **?  Regola: il contrassegno si trova sull'ultima figlia della catena
042900060921      * ? o sulla penultima se l'ultima è una Locale.
043000060921      *    Occorre agganciare il file Contrassegni trovando prima la
043100060921      *    chiave della bolla su cui si dovrebbe trovare il contrassegno.
043200060921     c                   exsr      ChecK_figlie
043300060921      *   All'uscita della Routine è impostata la chiave della bolla valida
043400060921      *   x verificare la presenza del contrassegno sulla spedizione.
043500060921      *
043600060921     C     Klbl2         CHAIN     Fiar9000                           34
043700060921      *
043800941228     C     *IN34         IFEQ      *ON
043900051115     C                   CLEAR                   ar9CAS
044000051115     C                   CLEAR                   ar9VCA
044100051115     C                   CLEAR                   ar9TIC
044200941228     C                   ENDIF
044300941228     C*
044400000525      * Se la LNP della spedizione è una filiale POSTE richiamo
044500000525      * routine x reperire eventuale C/assegno e la data di giacenza
044600000525      * dalla data dell' evento
044700000526     C                   EXSR      POSTE
044800090408      *-------
044900090408      * ?  O x qualsiasi Variazione Bolla inerente alla Data Cons.Richiesta  ?
045000090408     c                   move      'N'           VariataOggi       1
045100090408     C     §3KCQ8        ifEQ      'S'
045200131014     c                   setoff                                       51
045300131014     C     KBLP          setll     FIARBF2C
045400131014     C     KBLP          reade     FIARBF2C
045500131014     C                   dow       not %Eof(FIARBF2C)
045600090408     c                   if        *in51
045700090408     c                   if        arbDTV = dateu
045800090408     c                   if        arbCVB = 'CR' or
045900090408     c                             arbCVB = 'CS'
046000090408     c                   move      'S'           VariataOggi
046100090408     c                   leave
046200090408     c                   end
046300090408     c                   end
046400090408     c                   end
046500131014     c                   setoff                                       51
046600131014     C     KBLP          reade     FIARBF2C
046700090408     C                   end
046800090408     C                   ENDIF
046900090408      *-------
047000941228     C* CERCO EVENTO LASCIATO AVVISO
047100941228     C                   CLEAR                   WDLA
047200941228     C                   MOVEL     '2A'          COD
047300941228     C*
047400131113     C     kBLP          SETGT     FNEVB000
047500131113     C     kBLP          READPE    FNEVB000                               34
047600941228     C*
047700941228     C     *IN34         DOWEQ     *OFF
047800131113     C*
047900131113      **  imposta sulla DS le note x testare il flag Progressivo
048000131113     c                   movel     evbNOT        Devb01
048100131113      *
048200131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
048300131113     c                   if        wTPinv <> 'E'
048400131113     c                               or
048500131113     c                             wTPinv = 'E' and
048600131113     c                             (§notPRG = ' ' or §notPRG = 'U')
048700131113      **
048800941228     C                   MOVEL(P)  EVBCEV        KEY
048900941228     C*
049000941228     C     KTAB2         CHAIN     TABEL                              30
049100941228     C     *IN30         IFEQ      *OFF
049200040319     C                   MOVEL     TBLUNI        DS2A
049300941228     C     §2AFTC        IFEQ      'A'
049400941228     C                   Z-ADD     EVBDEV        WDLA              8 0
049500941228     C                   SETON                                        34
049600941228     C                   ENDIF
049700941228     C                   ENDIF
049800941228     C*
049900131113     c                   endif
050000131113     C*
050100131113     C  N34kBLP          READPE    FNEVB000                               34
050200941228     C                   ENDDO
050300921026     C*
050400970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
050500970721     C                   MOVEL     'N'           WRTVAG            1
050600970121     C                   MOVEL     'S'           WFLDT3            1
050700970121     C                   MOVEL     'S'           WINVAC            1
050800040319      *
050900970121     C     §3KNMC        IFEQ      'S'
051000970121     C     BLPDCM        ANDEQ     0
051100011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
051200060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
051300011002     C     *IN68         ANDEQ     '0'
051400040319      *
051500040319      * ....ma anche
051600040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
051700040319      *
051800040319     C     §3kNMC        orEQ      'S'
051900040319     C     blpDCM        andGT     0
052000040319     C     blpCCA        andNE     *blank
052100040319     C     *in68         andEQ     *off
052200040319      *
052300970121     C                   EXSR      TSTNMC
052400970121     C                   END
052500001031     C*
052600001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
052700001031      *  PARZIALE E VERIFICA DELLE DATE
052800001106     C                   MOVEL     'N'           WTRPAR            1
052900131113      **
053000001031     C     §3KCQ6        IFEQ      'S'
053100001031     C     BLPDCM        ANDEQ     0
053200001031     C     BLPDCP        ANDGT     0
053300011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
053400060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
053500011002     C     *IN68         ANDEQ     '0'
053600001031     C                   EXSR      PARZIA
053700001031     C                   END
053800131113      **
053900980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
054000980318     C                   MOVEL     'N'           WTRMIC
054100060912     C                   MOVEL     'N'           WTRRIP
054200990602     C                   Z-ADD     0             WDTMIC            8 0
054300131113      **
054400131113      **  Non Consegnata e si deve inviare il MIC o la Data Consegna
054500131113      **      se si tratta dell'ULTIMO EVENTO registrato
054600980406     C     §3KCQ3        IFEQ      'S'
054700980318     C     BLPDCM        ANDEQ     0
054800060912     C     §3KCQ1        orEQ      'S'
054900060912     C     BLPDCM        ANDEQ     0
055000131113      **
055100131113     C     kBLP          SETGT     FNEVB000
055200131113     C     kBLP          READPE    FNEVB000                               34
055300131113      *  Cercando sempre l'ultimo evento NON deve fare distinzioni
055400131113      *   fra Italia o Estero
055500131113      *
055600980318     C     *IN34         IFEQ      *OFF
055700060912     C     EVBCEV        ifEQ      'MIC'
055800060912     C     §3KCQ3        andeq     'S'
055900980318     C                   MOVEL     'S'           WTRMIC            1
056000990602     C                   Z-ADD     EVBDEV        WDTMIC
056100060912     c                   else
056200060912      *ripristino
056300060912     C     EVBCEV        ifeq      'RIP'
056400060912     C     EVBCEV        oreQ      'RDC'
056500060912     C                   MOVEL     'S'           WTRRip            1
056600060912     C                   endif
056700060912      *
056800060912     C                   endif
056900980318     C                   END
057000131113      *
057100980318     C                   END
057200921026     C* DATA CONSEGNA
057300930809     C     §3KCQ1        IFEQ      'S'
057400921026     C     BLPDCM        ANDNE     0
057500080410      *
057600010202     C     §3KCQ1        OREQ      'S'
057700010202     C     §3KCSD        ANDEQ     'EW'
057800010202     C     BLPDCM        ANDEQ     0
057900010202     C     BLPDCP        ANDNE     0
058000080410      *
058100930809     C* DATA LASCIATO AVVISO
058200930809     C     §3KCQ2        OREQ      'S'
058300941228     C     WDLA          ANDGT     0
058400080410      *
058500930809     C* ANOMALIE
058600930809     C     §3KCQ4        OREQ      'S'
058700930809     C     BLPCCA        ANDNE     *BLANKS
058800080410      *
058900990602     C* ANOMALIE DANNI
059000990602     C     §3KCQ4        OREQ      'S'
059100990602     C     DATMAX        ANDNE     0
059200080410      *
059300930809     C* GIACENZA
059400930809     C     §3KCQ5        OREQ      'S'
059500930809     C     *IN33         ANDEQ     *OFF
059600080410      *
059700970123     C* MANCATE CONSEGNE
059800970123     C     §3KNMC        OREQ      'S'
059900970123     C     BLPDCM        ANDEQ     0
060000080410      *
060100980318     C* Messa in consegna
060200980318     C     §3KCQ3        OREQ      'S'
060300980318     C     WTRMIC        ANDEQ     'S'
060400080410      *
060500060912     C* Messa in consegna
060600060912     C     §3KCQ1        OREQ      'S'
060700060912     C     WTRrip        ANDEQ     'S'
060800060912     C     BLPDCM        ANDEQ     0
060900080410      *
061000090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
061100080410     C     §3KCQ8        OREQ      'S'
061200090408     c     VariataOggi   andEQ     'S'
061300090408     C     BLPDCR        ANDGT     0
061400080721     C*
061500080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
061600080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
061700080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
061800921026     C*
061900970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
062000970123     C* consegna o se li devo trasmettere e li ho trovati
062100970123     C     §3KNMC        IFNE      'S'
062200970123     C     WINVAC        OREQ      'S'
062300990624     C* ANOMALIE DANNI
062400990624     C     §3KCQ4        OREQ      'S'
062500990624     C     DATMAX        ANDNE     0
062600990624     C* Messa in consegna
062700990624     C     §3KCQ3        OREQ      'S'
062800990624     C     WTRMIC        ANDEQ     'S'
062900060912     C* Evento di ripristino
063000060912     C     §3KCQ1        OREQ      'S'
063100060912     C     WTRrip        ANDEQ     'S'
063200080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
063300080721     C     §3KCQ8        OREQ      'S'
063400080721     C     BLPDCR        ANDGT     0
063500090408     c     VariataOggi   andEQ     'S'
063600930809     C                   EXSR      CARBLR
063700970123     C                   END
063800080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
063900970123     C*
064000930809     C                   END
064100990602     C                   END
064200921026     C*
064300970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
064400970123     C* consegna o se li devo trasmettere e li ho trovati
064500970123     C     §3KNMC        IFNE      'S'
064600090527      *
064700970123     C     WFLDT3        OREQ      'S'
064800090527      *
064900090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
065000090527      *  fino ad un successivo Evento sulle bolle figlie)
065100090527     C     §3KCQ4        OREQ      'S'
065200090527     C     BLPCCA        ANDNE     *BLANKS
065300090527      *
065400990624     C* ANOMALIE DANNI
065500990624     C     §3KCQ4        OREQ      'S'
065600990624     C     DATMAX        ANDNE     0
065700990624     C* Messa in consegna
065800990624     C     §3KCQ3        OREQ      'S'
065900990624     C     WTRMIC        ANDEQ     'S'
066000001106     C* CONSEGNA PARZIALE
066100001106     C     §3KCQ6        OREQ      'S'
066200001106     C     WTRPAR        ANDEQ     'S'
066300080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
066400080729     C     §3KCQ8        OREQ      'S'
066500080729     C     BLPDCR        ANDGT     0
066600090408     c     VariataOggi   andEQ     'S'
066700080417      *
066800921026     C                   MOVEL     'T'           BLPFT3
066900941228     C                   MOVEL     DATEU         BLPDT3
067000080417      *
067100010927     C  N68              UPDATE    FNBLP000
067200010927     C   68              UPDATE    FNBLP1
067300970123     C                   END
067400921026     C*
067500921026     C                   ENDSR
067600000526     C*
067700170309       //--------------------------------------------------------------
067800170309       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3K
067900170309       //--------------------------------------------------------------
068000170309       begsr  setll_sql;
068100170309
068200170309         wSQL = 'select * +
068300170309                 from tabel00f +
068400170309                 where +
068500170309                 tblkut = 1 and +
068600170309                 tblcod = ''3K'' +
068700170309                 order by +
068800170309                 substr(TBLUNI , 27 , 7)';
068900170309
069000170309         // Dichiarazione cursore
069100170309         exec sql   prepare S1   from :wSQL;
069200170309         exec sql   declare C1  asensitive   cursor for S1;
069300170309
069400170309         // Apertura del cursore
069500170309         exec sql   open C1;
069600170309
069700170309         *in99 = *off;
069800170309         if SQLCode < 0;
069900170309           *in99 = *on;
070000170309         endif;
070100170309
070200170309       endsr;
070300170309       //--------------------------------------------------------------
070400170309       // fetch_sql  - legge il rcdset di TABEL00F
070500170309       //--------------------------------------------------------------
070600170309       begsr  fetch_sql;
070700170309
070800170309         exec sql  fetch next  from C1  into :TABELDS;
070900170309
071000170309         *in31 = *off;
071100170309         if SQLCode = 100 or SQLCode < 0;
071200170309           *in31 = *on;
071300170309         endif;
071400170309
071500170309       endsr;
071600170309       //--------------------------------------------------------------
071700170309       // close_sql  - chiusura cursore
071800170309       //--------------------------------------------------------------
071900170309       begsr  close_sql;
072000170309
072100170309         // chiusura del cursore
072200170309         exec sql  close C1;
072300170309
072400170309       endsr;
072500170309
072600060921      **?-------------------------------------------------------------*
072700060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
072800060921      **?-------------------------------------------------------------*
072900060921     C     ChecK_Figlie  BEGSR
073000060921      *
073100060921      *  inizializza flag di work della Routine
073200060921     c                   move      'S'           Primo_legame      1
073300060921      *
073400060921      **?chiave salvata della Bolla x partire nella ricerca
073500060921     c                   z-add     blpAAS        klb2_AAS
073600060921     c                   z-add     blpLNP        klb2_LNP
073700060921     c                   z-add     blpNRS        klb2_NRS
073800060921     c                   z-add     blpNSP        klb2_NSP
073900060921      *
074000060921      *  Chiave Bolla Precedente
074100060921     c     new_ciclo     tag
074200060921      **?                                  Flag di work del ciclo
074300060921     c                   move      'N'           ha_figlia         1
074400060921     C     Klbl2         setLL     FnLBL02L
074500060921     C     Klbl2         reade     FnLBL02L
074600060921      *
074700060921     c                   Dow       not %EoF(FnLBL02L)
074800060921      **?primo controllo:
074900060921      *    tutta la catena deve partire dalla bolla mamma che è
075000060921      *     uguale alla bolla originale del primo record della catena
075100060921     c                   if          Primo_legame = 'S'
075200060921     c                   eval      Primo_legame = 'N'
075300060921     c                   if          blpAAS <> exLBLAAo   or
075400060921     c                               blpLNP <> exLBLLPo   or
075500060921     c                               blpNRS <> exLBLNRo   or
075600060921     c                               blpNSP <> exLBLNSo
075700060921      *    se così non è non deve saltare il ciclo e non impostare i
075800060921      *     campi chiave x il contrassegno.
075900060921     c                   leave
076000060921     c                   end
076100060921     c                   end
076200060921      **?secondo controllo:
076300060921      *     non deve essere una bolla locale
076400060921      *   (sulle locali non c'è l'informazione di contrassegno)
076500060921     c                   if        exLBLLPN <> exLBLLPP
076600060921      *  imposta la chiave dell'ultima figlia
076700060921      *       per il prossimo giro di controllo
076800060921     c                   z-add     exLBLAAN      klb2_AAS
076900060921     c                   z-add     exLBLLPN      klb2_LNP
077000060921     c                   z-add     exLBLNRN      klb2_NRS
077100060921     c                   z-add     exLBLNSN      klb2_NSP
077200060921     c                   move      'S'           ha_figlia
077300060921     c                   end
077400060921      *
077500060921      **?   (attenzione a non sposizionarsi)
077600060921      * Chiave impostata come Bolla Precedente appena letta
077700060921     C     KLBL2P        reade     FnLBL02L
077800060921     c                   end
077900060921      *
078000060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
078100060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
078200060921      *   se invece non ha trovato delle figlie allora va a fine con la
078300060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
078400060921     c                   if        ha_figlia = 'S'
078500060921     c                   if        exLBLAAp <> klb2_AAS or
078600060921     c                             exLBLLPp <> klb2_LNP or
078700060921     c                             exLBLNRp <> klb2_NRS or
078800060921     c                             exLBLNSp <> klb2_NSP
078900060921     c                   goto      new_ciclo
079000060921     c                   end
079100060921     c                   end
079200060921      *
079300060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
079400060921      **?   x verificare la presenza del contrassegno su AR9
079500060921      *
079600060921     C                   ENDSR
079700060921      **?-------------------------------------------------------------*
079800000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
079900921026     C*
080000131119     C*   per quel che riguarda le POSTE
080100131119     C*    NON è stato adeguato alla logica degli eventi passati dal PDA
080200131119     C*     quindi con i flag del progressivo 'P' e 'U'
080300131119     C*
080400000526     C     POSTE         BEGSR
080500000526      *
080600000526     C                   CLEAR                   PTTIC
080700000526     C                   CLEAR                   PTCAS
080800000526     C                   CLEAR                   PTVCA
080900000526     C                   CLEAR                   PTIAS
081000000526     C                   CLEAR                   PTVAS
081100000526     C                   CLEAR                   PTDTG
081200000526     C                   CLEAR                   §POST
081300000526      *
081400000526     C                   SETOFF                                       22
081500000526     C     BLPLNP        LOOKUP    FIP                                    22
081600000526     C   22              MOVEL     'S'           §POST             1
081700131113      *
081800000526     C     §POST         IFEQ      'S'
081900131113      *
082000000526      * Se la bolla è stata chiusa con consegna anomala la merce
082100000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
082200000526      * questo è stato annullato in quanto non più incassabile da
082300000526      * Bartolini.
082400000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
082500000526      * informazione da ritornare alla SDA
082600000526     C     BLPCCA        IFNE      *BLANK
082700051115     C     ar9CAS        ANDEQ     *ZEROS
082800000526     C     KBLP          CHAIN     FNARBK00                           22
082900000526     C     *IN22         IFEQ      '0'
083000000526     C                   MOVEL     ARBTIC        PTTIC
083100000526     C                   Z-ADD     ARBCAS        PTCAS
083200000526     C                   MOVEL     ARBVCA        PTVCA
083300000526     C                   ENDIF
083400000526     C                   ENDIF
083500000526      *
083600060214      * Prendo importo da assicurare da Fiar4 t.r.=J
083700000526     C                   MOVEL     'J'           KTRC
083800060214     C     Kar4          CHAIN     Fiar401L                           35
083900000526     C     *IN35         IFEQ      '0'
084000060214     C                   MOVEL     ar4NOT        DSBL4J
084100000526     C                   Z-ADD     §B4IAS        PTIAS
084200000526     C                   MOVEL     §B4VAS        PTVAS
084300000526     C                   ENDIF
084400000526      *
084500000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
084600000526      * gestite le giacenze per le bolle poste
084700000526      *
084800131113     C     kBLP          SETGT     FNEVB000
084900131113     C     kBLP          READPE    FNEVB000                               34
085000000526      * Devo cercare l'ultimo evento di tipo giacenza
085100000526      *
085200000526     C     *IN34         DOWEQ     *OFF
085300000526     C                   Z-ADD     1             WW
085400000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
085500131119     c                   if         *in42 and T2A(WW) ='G'
085600000526     C                   Z-ADD     EVBDEV        PTDTG
085700000619     C                   LEAVE
085800131119     C                   ENDIF
085900131119      *
086000131113     C     kBLP          READPE    FNEVB000                               34
086100000526     C                   ENDDO
086200000526     C                   ENDIF
086300000526      *
086400000526     C                   ENDSR
086500000526     C*
086600941228     C* CREO RECORD FNVAC01L -----------------------------------------*
086700921026     C     CARBLR        BEGSR
086800080417      *
086900080417     c                   clear                   fnvacds
087000080417      *
087100941228     C                   MOVEL     BLPAAS        VACAAS
087200941228     C                   MOVEL     BLPLNP        VACLNP
087300941228     C                   MOVEL     BLPMGS        VACMGS
087400941228     C                   MOVEL     BLPNRS        VACNRS
087500941228     C                   MOVEL     BLPNSP        VACNSP
087600941228     C                   MOVEL     BLPCBO        VACCBO
087700941228     C                   MOVEL     BLPLNA        VACLNA
087800941228     C                   MOVEL     BLPRSD        VACRSD
087900941228     C                   MOVEL     BLPPRD        VACPRD
088000941228     C                   MOVEL     BLPGC1        VACGC1
088100941228     C                   MOVEL     BLPGC2        VACGC2
088200941228     C                   MOVEL     BLPCTR        VACCTR
088300941228     C                   MOVEL     BLPCTS        VACCTS
088400941228     C                   MOVEL     BLPFTM        VACFTM
088500941228     C                   MOVEL     BLPFIN        VACFIN
088600941228     C                   MOVEL     BLPFAP        VACFAP
088700941228     C                   MOVEL     BLPTSP        VACTSP
088800000526      *
088900000526     C     §POST         IFEQ      'S'
089000000526     C                   MOVEL     PTIAS         VACIAS
089100000526     C                   MOVEL     PTVAS         VACVAS
089200000526     C                   ELSE
089300000526     C                   MOVEL     BLPIAS        VACIAS
089400000526     C                   MOVEL     BLPVAS        VACVAS
089500000526     C                   ENDIF
089600000526      *
089700941228     C                   MOVEL     BLPNAS        VACNAS
089800941228     C                   MOVEL     BLPNCL        VACNCL
089900941228     C                   MOVEL     BLPPKF        VACPKB
090000941228     C                   MOVEL     BLPVLF        VACVLB
090100941228     C                   MOVEL     BLPQFT        VACQFT
090200000526      *
090300000526     C     §POST         IFEQ      'S'
090400051115     C     ar9CAS        ANDEQ     *ZEROS
090500000526     C                   MOVEL     PTCAS         VACCAS
090600000526     C                   MOVEL     PTVCA         VACVCA
090700000526     C                   MOVEL     PTTIC         VACTIC
090800000526     C                   ELSE
090900051115     C                   MOVEL     ar9CAS        VACCAS
091000051115     C                   MOVEL     ar9VCA        VACVCA
091100051115     C                   MOVEL     ar9TIC        VACTIC
091200000526     C                   ENDIF
091300000526      *
091400060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
091500011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
091600011002      *
091700011002     C     *IN68         IFEQ      '1'
091800060214     C                   MOVEL     ar4KSC        VACCCM
091900011002     C                   ELSE
092000921026     C     BLPCCM        IFNE      0
092100941228     C                   MOVEL     BLPCCM        VACCCM
092200921026     C                   ELSE
092300941228     C                   MOVEL     BLPKSC        VACCCM
092400921026     C                   END
092500011002     C                   END
092600131113      *
092700941228     C                   MOVEL     BLPRMN        VACRMN
092800941228     C                   MOVEL     BLPRMA        VACRMA
092900941228     C                   MOVEL     BLPRMO        VACRMO
093000941228     C                   MOVEL     BLPFFD        VACFFD
093100131113      *
093200000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
093300000921     C     §POST         IFEQ      'S'
093400000921     C                   Z-ADD     BLPNCD        VACDCR
093500000921     C                   ELSE
093600000921     C                   MOVEL     BLPDCR        VACDCR
093700000921     C                   ENDIF
093800941228     C                   MOVEL     BLPTCR        VACTCR
093900941228     C                   MOVEL     BLPHCR        VACHCR
094000941228     C                   MOVEL     BLPTC1        VACTC1
094100941228     C                   MOVEL     BLPTC2        VACTC2
094200941228     C                   MOVEL     BLPDCM        VACDCM
094300941228     C                   MOVEL     BLPHMC        VACHMC
094400941228     C                   MOVEL     BLPCCA        VACCCA
094500941228     C                   MOVEL     WDLA          VACDLA
094600000526      *
094700000526     C     §POST         IFEQ      'S'
094800000526     C                   MOVEL     PTDTG         VACDAG
094900000526     C                   ELSE
095000941228     C  N33              Z-ADD     GCPMGC        VACDAG
095100941228     C  N33              MOVEL     GCPAGC        VACDAG
095200941228     C   33              MOVEL     *ZEROS        VACDAG
095300000526     C                   ENDIF
095400000526      *
095500990602     C*---------------------------------------------------------------*
095600990602     C* Se ho trovato un evanto di messa in consegna....
095700060912 1   C     WTRMIC        IFEQ      'S'
095800990602     C* ... e non ho la data di consegna totale sulla bolla ..
095900990602     C     BLPDCM        ANDEQ     0
096000040319      *
096100990602     C* Verifico se ho trovato una pratica C.A. aperta ...
096200060912 2   C     DATMAX        IFNE      0
096300990602     C     §3KCQ4        ANDEQ     'S'
096400990602     C* .. e se l'apertura di quest'ultima è successiva alla
096500990602     C*   messa in consegna ...
096600990602     C     DATMAX        ANDGE     WDTMIC
096700990602     C* .. allora imposto la data apertura C.A. nella data consegna
096800990602     C*    e imposto il tipo consegna anomala = 'A'
096900990602     C                   Z-ADD     1200          VACHMC
097000990602     C                   Z-ADD     DATMAX        VACDCM
097100990602     C                   MOVEL     'A'           VACCCA
097200060912 x2  C                   ELSE
097300990602     C* ... altrimenti imposto data messa in consegna
097400990602     C                   Z-ADD     EVBHEV        VACHMC
097500980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
097600990602     C                   MOVEL     'C'           VACCCA
097700060912 e2  C                   END
097800990602     C*
097900990602     C* ... se è stata impostata la data di consegna sulla bolla ..
098000060912 x1  C                   ELSE
098100040319     C*
098200060912 2   C     BLPDCM        IFNE      0
098300040319     C*
098400040319     C* Un tempo era stato asteriscato questo pezzo
098500040319     C* >>>>>>>>>>>>>
098600040409      * non deve essere fatto per l'estero
098700060912 3   C     wtpINV        Ifne      'E'
098800040409      *
098900990602     C* ... verifico se ho trovato una C.A. aperta ...
099000060912 4   C     DATMAX        IFNE      0
099100040319     C     §3KCQ4        ANDEQ     'S'
099200990602     C* .. e se l'apertura di quest'ultima è successiva alla
099300990602     C* .. consegna ...
099400040319     C     DATMAX        ANDGT     BLPDCM
099500040319     C*
099600990602     C* .. allora imposto la data apertura C.A. nella data consegna
099700990602     C*    e imposto il tipo consegna anomala = 'A'
099800040319     C                   Z-ADD     1200          VACHMC
099900040319     C                   Z-ADD     DATMAX        VACDCM
100000040319     C                   MOVEL     'A'           VACCCA
100100040319     C*
100200060912 e4  C                   ENDif
100300060912 e3  c                   end
100400040319     C* >>>>>>>>>>>>>
100500040319     C*
100600990602     C* ... se non è stata effettuata la messa in consegna, la
100700990602     C*     bolla risulta non consegnata, ....
100800060912 x2  C                   ELSE
100900990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
101000990602     C*     le consegne anomale, imposto la data consegna + flag
101100990602     C*     consegna anomala='A'
101200060912 3   C     DATMAX        IFNE      0
101300990602     C     §3KCQ4        ANDEQ     'S'
101400990602     C                   Z-ADD     1200          VACHMC
101500990602     C                   Z-ADD     DATMAX        VACDCM
101600990602     C                   MOVEL     'A'           VACCCA
101700060912 e3  C                   END
101800060912 e2  C                   END
101900060912 e1  C                   END
102000060912     C* Se ho trovato un evento di ripristino
102100060912 1   C     WTRrip        IFEQ      'S'
102200060912     C     BLPDCM        ANDEQ     0
102300060912     C     §3KCQ1        ANDEQ     'S'
102400060912     C                   Z-ADD     0             VACHMC
102500060912     C                   Z-ADD     0             VACDCM
102600060912     C                   MOVEL     'R'           VACCCA
102700060912 e1  c                   endif
102800060912
102900080417     c                   exsr      Write_VAC
103000930603     C                   SETON                                        06
103100990604     C*
103200990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
103300990618     C* e questa è stata anche.....
103400060912 1   C     §3KCQ1        IFEQ      'S'
103500080417     C     §3KCSD        ANDEQ     'EW'
103600080417      *
103700060912 2   C     VACCCA        IFEQ      'A'
103800990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
103900990618     C* invio al partner dell'informazione della consegna parziale
1040000609123    C     BLPDCM        IFEQ      0
104100990618     C     BLPDCP        ANDNE     0
104200990618     C                   Z-ADD     0             VACHMC
104300990618     C                   Z-ADD     0             VACDCM
104400990618     C                   MOVEL     ' '           VACCCA
104500080417     c                   exsr      Write_VAC
104600060912x3   C                   ELSE
104700990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
104800990618     C* invio al partner dell'informazione
104900060912 4   C     BLPDCM        IFEQ      0
105000990624     C     WDTMIC        ANDGT     0
105100990618     C                   Z-ADD     EVBHEV        VACHMC
105200990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
105300990618     C                   MOVEL     'C'           VACCCA
105400080417     c                   exsr      Write_VAC
105500060912e4   C                   END
105600060912e3   C                   END
105700990618     C*
105800990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
105900990618     C* scrivo anche record x apertura C.A.
106000060912x2   C                   ELSE
1061000609123    C     DATMAX        IFNE      0
106200990618     C     §3KCQ4        ANDEQ     'S'
106300990618     C                   Z-ADD     1200          VACHMC
106400990618     C                   Z-ADD     DATMAX        VACDCM
106500990618     C                   MOVEL     'A'           VACCCA
106600080417     c                   exsr      Write_VAC
106700060912 e3  C                   END
106800060912 e2  C                   END
106900060912 e1  C                   END
107000990618     C*
107100921026     C                   ENDSR
107200990602     C*---------------------------------------------------------------*
107300990602     C* Controllo se esiste una pratica C.A. aperta                  -*
107400990602     C*---------------------------------------------------------------*
107500990602     C     CRTCA         BEGSR
107600990602     C*
107700990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
107800990602     C* per reperire la pratica di danno con data più alta
107900990602     C                   Z-ADD     0             DATMAX            8 0
108000990602     C     §3KCQ4        IFEQ      'S'
108100040715      **
108200040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
108300040715      * SIA COME MAMMA CHE COME FIGLIA
108400040715      **
108500040715     c                   clear                   fidn12ds
108600040715     c                   eval      i12aas = blpAAS
108700040715     c                   eval      i12lnp = blpLNP
108800040715     c                   eval      i12nrs = blpNRS
108900040715     c                   eval      i12nsp = blpNSP
109000040715     c                   eval      i12fel = 'E'
109100040715     c                   eval      i12fan = 'E'
109200040715     c                   eval      i12fch = 'E'
109300040715     c                   call      'FIDN12R'
109400040715     c                   parm                    fidn12ds
109500040715     c                   z-add     *zeros        ii                2 0
109600040715      **
109700040715      * prende fra le C.A. aperte quella con la data ultima
109800040715     c                   if        o12nca > 0
109900040715     c                   dow       ii <  o12nca
110000040715      **
110100040715     c                   add       1             ii
110200040715     C                   movel     skDca(ii)     datape            8 0
110300040715     C     DATMAX        IFLT      DATAPE
110400040715     C                   Z-ADD     DATAPE        DATMAX
110500040715     C                   END
110600040715      *
110700040715     c                   enddo
110800040715     c                   endif
110900040715      *
111000040715     C                   END
111100040715     C*
111200990602     C                   ENDSR
111300970122     C*---------------------------------------------------------------*
111400970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
111500970122     C*---------------------------------------------------------------*
111600970121     C     TSTNMC        BEGSR
111700040422     **
111800040422     ** avendo fatto entrare anche x data consegna GT di 0
111900040422     ** il flag deve essere resettato solo per la vecchia condizione
112000040422     ** altrimenti non scrive + il VAC come una volta.
112100040422     c                   if        blpdcm = 0
112200040422     **
112300970122     C                   MOVEL     'N'           WFLDT3
112400090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
112500090223     C**   sempre il VAC.
112600090223     C********           MOVEL     'N'           WINVAC
112700040422     **
112800040422     c                   end
112900040422     **
113000970122     C                   MOVEL     'N'           WTREVE            1
113100970122     C                   MOVEL     *BLANKS       WSVTMC            1
113200970122     C                   MOVEL     *BLANKS       WMOT1            35
113300970122     C                   MOVEL     *BLANKS       WMOT2            35
113400970122     C*  Leggo le estensioni bolla della spedizione e per quelle
113500970122     C*  relative ad un motivo di mancata consegna scrivo VAG
113600970122     C                   DO        Y             W
113700970122     C                   MOVEL     T7K(W)        WTMC              1
113800970122     C                   MOVE      T7K(W)        KTRC
113900060214     C     Kar4          CHAIN     Fiar401L                           35
114000970123     C     *IN35         IFEQ      '0'
114100060214     C     ar4FTR        ANDNE     'T'
114200060214     C     ar4FTR        ANDNE     'C'
114300970122     C*  Se ho trovato estensione bolla e non è trasmesso
114400970122     C*  controllo se ho un nuovo tp.mancata consegna
114500970122     C*  e se si reperisco evento corrispondente
114600970122     C     WTMC          IFNE      WSVTMC
114700970122     C                   EXSR      REPEVE
114800970122     C                   END
114900970122     C*  se ho trovato l'evento controllo se devo scrivere
115000970122     C*  nuovo record VAG:
115100970122     C     WTREVE        IFEQ      'S'
115200970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
115300970122     C*  - se ho già scritto 2 motivi --> new record
115400970122     C     WTMC          IFNE      WSVTMC
115500970122     C     WMOT2         ORNE      *BLANKS
115600970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
115700970124     C     WSVTMC        IFNE      *BLANKS
115800970124     C     WMOT2         ANDEQ     *BLANKS
115900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
116000970318     C*  primi 15 chr nella seconda motivazione
116100970318     C     WCHR15        IFNE      *BLANKS
116200970318     C                   MOVEL     WCHR15        VAGNOT
116300970318     C                   END
116400170322     C*
116500170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
116600170322     C                   eval      vgdTIP = 'VG'
116700170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
116800170322     C                   eval      vgdTSC = §3KGSD
116900170322     C                   eval      vgdDAT = dateu
117000170322     C                   eval      vgdPRG = g§7VASPRG
117100170322     C                   eval      vgdPGM = procname
117200170322     C                   eval      vgdSTO = '?'
117300170322     c                   if        §3KGSD  <> *blanks
117400170322     C                   WRITE     tivgd000
117500170322     c                   endif
117600970124     C                   SETON                                        06
117700970124     C                   END
117800970124     C*  New record
117900970122     C                   EXSR      NEWVAG
118000970122     C                   ELSE
118100970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
118200970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
118300970318     C     WCHR15        IFNE      *BLANKS
118400970318     C                   MOVEL     WCHR15        VAGNOT
118500060214     C                   MOVE      ar4NOT        VAGNOT
118600970318     C                   ELSE
118700970318     C*  Se codice generico occupo chr. da 32 posizione
118800970318     C     VAGCMC        IFEQ      'G  '
118900970318     C     VAGCMC        OREQ      'GEN'
119000060214     C                   MOVEL     ar4NOT        WCHR19           19
119100060214     C                   MOVE      ar4NOT        WCHR16           16
119200970318     C                   MOVE      WCHR19        VAGDMC
119300970318     C                   MOVEL     WCHR16        VAGNOT
119400970318     C                   ELSE
119500970318     C*  Altrimenti imposto solo ulteriore descrizione
119600060214     C                   MOVEL     ar4NOT        VAGNOT
119700970318     C                   END
119800970318     C                   END
119900060214     C                   MOVEL     ar4NOT        WMOT2
120000970122     C                   MOVEL     *BLANKS       WMOT1
120100170322     C*
120200170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
120300170322     C                   eval      vgdTIP = 'VG'
120400170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
120500170322     C                   eval      vgdTSC = §3KGSD
120600170322     C                   eval      vgdDAT = dateu
120700170322     C                   eval      vgdPRG = g§7VASPRG
120800170322     C                   eval      vgdPGM = procname
120900170322     C                   eval      vgdSTO = '?'
121000170322     c                   if        §3KGSD  <> *blanks
121100170322     C                   WRITE     tivgd000
121200170322     c                   endif
121300970122     C                   SETON                                        06
121400970122     C                   END
121500970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
121600970122     C                   MOVEL     'S'           WFLDT3
121700970122     C                   MOVEL     'S'           WINVAC
121800060214     C*  Fleggo Fiar4
121900060214     C                   MOVEL     'C'           ar4FTR
122000060214     C                   Z-ADD     DATEU         ar4DTR
122100060214     C                   EXCEPT    AGGar4
122200970122     C                   END
122300970122     C                   END
122400970122     C*
122500970122     C                   END
122600970122     C*  Controllo se mi è rimasto un VAG da scrivere
122700970122     C     WMOT1         IFNE      *BLANKS
122800970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
122900970318     C*  primi 15 chr nella seconda motivazione
123000970318     C     WCHR15        IFNE      *BLANKS
123100970318     C                   MOVEL     WCHR15        VAGNOT
123200970318     C                   END
123300170322     C*
123400170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
123500170322     C                   eval      vgdTIP = 'VG'
123600170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
123700170322     C                   eval      vgdTSC = §3KGSD
123800170322     C                   eval      vgdDAT = dateu
123900170322     C                   eval      vgdPRG = g§7VASPRG
124000170322     C                   eval      vgdPGM = procname
124100170322     C                   eval      vgdSTO = '?'
124200170322     c                   if        §3KGSD  <> *blanks
124300170322     C                   WRITE     tivgd000
124400170322     c                   endif
124500970122     C                   SETON                                        06
124600970122     C                   END
124700970122     C*
124800970122     C                   ENDSR
124900970122     C*---------------------------------------------------------------*
125000970122     C* Controllo se esiste evento                                   -*
125100970122     C*---------------------------------------------------------------*
125200970122     C     REPEVE        BEGSR
125300970122     C*
125400970122     C                   MOVEL     'N'           WTREVE
125500970122     C*  Mi posiziono su file eventi
125600131113     C     kBLP          SETGT     FNEVB01L
125700131113     C     kBLP          READPE    FNEVB01L                               34
125800131113      *
125900970122     C*  Loop letture fino a fine file o travato evento
126000970122     C     *IN34         DOWEQ     '0'
126100970122     C     WTREVE        ANDEQ     'N'
126200040504      **
126300040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
126400040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
126500040504      **  le A16/A23 come varianti in programmi che precedono facendo
126600040504      **  sballare il ragionamento in questo pgm.
126700040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
126800040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
126900040504     c                   movel     '0'           evbCEV
127000040504     c                   end
127100131113      *
127200131113      **  imposta sulla DS le note x testare il flag Progressivo
127300131113     c                   movel     evbNOT        Devb01
127400131119      *
127500131119      ** Sia per l'Italia che per L'estero deve sempre prendere l'ultimo EVENTO
127600131119      **  in quanto MANCATA CONSEGNA perchè la bolla sarà aggiornata solo con
127700131119      **  l'evento "U" o BLK
127800131119      *
127900131119     c                   if         §notPRG = ' ' or §notPRG = 'U'
128000131119      *
128100970122     C*  Reperisco tipo mancata consegna evento letto
128200970122     C                   Z-ADD     1             WW                4 0
128300970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
128400131113      *
128500970122     C*  Controllo se tipo mancata consegna evento letto
128600970122     C*  è uguale a quella dell'estensione bolla
128700131113      *
128800131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
128900131119     c                   if         *in42 and T2A(WW) = WTMC
129000970122     C                   MOVEL     'S'           WTREVE
129100131119     c                   leave
129200131119     C                   END
129300131113      *
129400131119     C                   EndIF
129500131119      *
129600131113     C     kBLP          READPE    FNEVB01L                               34
129700970122     C                   END
129800970122     C*
129900970122     C                   ENDSR
130000970122     C*---------------------------------------------------------------*
130100970122     C* Nuovo record FNVAG                                           -*
130200970122     C*---------------------------------------------------------------*
130300970122     C     NEWVAG        BEGSR
130400970122     C*
130500050606     C                   CLEAR                   FNVAGds
130600970721     C                   MOVEL     'S'           WRTVAG            1
130700970318     C                   MOVEL     *BLANKS       WCHR25           25
130800970318     C                   MOVEL     *BLANKS       WCHR15           15
130900970122     C                   Z-ADD     BLPAAS        VAGAAS
131000970122     C                   Z-ADD     BLPLNP        VAGLNP
131100970122     C                   Z-ADD     BLPNRS        VAGNRS
131200970122     C                   Z-ADD     BLPNSP        VAGNSP
131300970122     C                   Z-ADD     BLPLNA        VAGLNA
131400990810     C                   MOVEL     EVBDEV        WAA2              4 0
131500970123     C                   MOVE      WAA2          VAGAGC
131600970122     C                   MOVE      EVBDEV        VAGMGC
131700060214     C                   MOVEL     ar4NOT        VAGCMC
131800060214     C                   MOVEL     ar4NOT        WMOT1
131900970122     C                   MOVEL     *BLANKS       WMOT2
132000970307     C                   Z-ADD     1             YY                4 0
132100970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
132200970307     C     VAGCMC        IFNE      'G  '
132300970307     C     VAGCMC        ANDNE     'GEN'
132400970307     C                   MOVEL     D2A(YY)       VAGDMC
132500970318     C                   MOVE      D2A(YY)       WCHR25
132600970318     C                   MOVEL     WCHR25        WCHR15
132700970305     C                   ELSE
132800060214     C                   MOVE      ar4NOT        WCOM31           31
132900970122     C                   MOVEL     WCOM31        VAGDMC
133000970305     C                   END
133100970122     C                   MOVEL     BLPRMA        VAGRMA
133200970122     C                   Z-ADD     BLPRMN        VAGRMN
133300970122     C     BLPCCM        IFNE      0
133400970122     C                   MOVEL     BLPCCM        VAGSCM
133500970122     C                   ELSE
133600970122     C                   MOVEL     BLPKSC        VAGSCM
133700970122     C                   END
133800970123     C*
133900970123     C                   MOVEL     WTMC          WSVTMC
134000970122     C*
134100970122     C                   ENDSR
134200001031     C*---------------------------------------------------------------*
134300001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
134400001031     C*---------------------------------------------------------------*
134500001031     C     PARZIA        BEGSR
134600001031     C*
134700001031      * LOOP SU DETTAGLIO COLLI
134800001103     C                   Z-ADD     0             TOTCO
134900001031     C                   Z-ADD     0             CONS
135000001102     C                   Z-ADD     0             ULTIMA            8 0
135100001031     C     KBLP          SETLL     FNBLT01L
135200001031     C                   DO        *HIVAL
135300001102     C     KBLP          READE     FNBLT01L                               97
135400001031     C   97              LEAVE
135500001031      * CONTA CONSEGNATI E NON CONSEGNATI
135600001103     C     BLTDCM        IFGT      0
135700001031     C                   ADD       1             CONS              3 0
135800001031     C                   END
135900001103     C                   ADD       1             TOTCO             3 0
136000001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
136100001031     C     BLTDCM        IFGT      ULTIMA
136200001031     C                   Z-ADD     BLTDCM        ULTIMA
136300001031     C                   ENDIF
136400001031     C                   ENDDO
136500001102     C                   MOVE      CONS          CONSA             3
136600001103     C                   MOVE      TOTCO         TOTCOA            3
136700001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
136800001031      *
136900050606     C                   CLEAR                   FNVAGds
137000001031     C                   MOVE      BLPLNP        VAGLNP
137100001031     C                   MOVE      BLPAAS        VAGAAS
137200001031     C                   MOVE      BLPNRS        VAGNRS
137300001031     C                   MOVE      BLPNSP        VAGNSP
137400001107     C                   MOVEL     'P  '         VAGCMC
137500001031     C                   MOVE      BLPRMN        VAGRMN
137600001031     C                   MOVE      BLPRMA        VAGRMA
137700001031     C                   MOVE      BLPLNA        VAGLNA
137800001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
137900001031     C                   MOVEL     COS(1)        VAGDMC
138000001103     C                   MOVEA     COS(2)        COM
138100001102     C                   MOVEA     CONSA         COM(22)
138200001103     C                   MOVEA     TOTCOA        COM(29)
138300001102     C                   MOVEA     COM           VAGNOT
138400001031      *
138500001031     C     BLPCCM        IFGT      0
138600001031     C                   Z-ADD     BLPCCM        VAGSCM
138700001031     C                   ELSE
138800001031     C                   Z-ADD     BLPKSC        VAGSCM
138900001031     C                   END
139000001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
139100001031     C                   MOVEL     ULTIMA        VAGAGC
139200001031     C                   MOVE      ULTIMA        VAGMGC
139300170322     C*
139400170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
139500170322     C                   eval      vgdTIP = 'VG'
139600170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
139700170322     C                   eval      vgdTSC = §3KGSD
139800170322     C                   eval      vgdDAT = dateu
139900170322     C                   eval      vgdPRG = g§7VASPRG
140000170322     C                   eval      vgdPGM = procname
140100170322     C                   eval      vgdSTO = '?'
140200170322     c                   if        §3KGSD  <> *blanks
140300170322     C                   WRITE     tivgd000
140400170322     c                   endif
140500170322     C*
140600001106     C                   MOVEL     'S'           WTRPAR
140700001031      *
140800001031     C                   ENDSR
140900080417      *------------------------------------------------------------------------*
141000080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
141100080417      *------------------------------------------------------------------------*
141200080417     C     Write_VAC     BEGSR
141300080417      *
141400090529      *  >>>>>>>>>
141500090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
141600090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
141700090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
141800090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
141900090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
142000090529      *   il record.
142100090529      *  >>>>>>>>>
142200090603     c                   if        §3kCTB ='S'
142300090603     c                   clear                   uguale_VAC        1
142400090529      *
142500090529     C                   movel     'VAC'         Kar5vac
142600090529     c     kar5          chain     fiar501l
142700090529      *
142800090529     c                   if        %Found(fiar501l)
142900090529      * Imposta i campi precedentemente salvati nella DS da comparare
143000090529     c                   eval      fnvacHGds = ar5UNI
143100090603      *
143200090603     c                   eval      VacGC1chg = VacGC1
143300090603     c                   eval      VacGC2chg = VacGC2
143400090603     c                   eval      VacFFDchg = VacFFD
143500090603     c                   eval      VacTCRchg = VacTCR
143600090603     c                   eval      VacDCRchg = VacDCR
143700090603     c                   eval      VacHCRchg = VacHCR
143800090603     c                   eval      VacDCMchg = VacDCM
143900090603     c                   eval      VacHMCchg = VacHMC
144000090603     c                   eval      VacTC1chg = VacTC1
144100090603     c                   eval      VacTC2chg = VacTC2
144200090603     c                   eval      VacCCAchg = VacCCA
144300090603     c                   eval      VacDLAchg = VacDLA
144400090603     c                   eval      VacDAGchg = VacDAG
144500090529      * quindi
144600090529      *    controllo di uguaglianza
144700090529     c                   if        fnvacHGds = DSvacHG
144800090529     c                   eval      uguale_VAC = 'S'
144900090603     c                   move      '='           ar5UNI
145000090603     c                   else
145100090603     c                   move      ' '           ar5UNI
145200090603     c                   eval      ar5UNI    = DSvacHG
145300090603     c                   end
145400090529      *
145500090529     c                   eval      AR5DAC = dateu
145600090529     c                   eval      AR5ORC = Orario
145700090529     c                   eval      AR5PRG = AR5PRG + 1
145800090529     c*
145900090529     c                   update    fiar5000
146000090529     c*
146100090529     c                   if        uguale_VAC = 'S'
146200090529     c                   leavesr
146300090529     c                   end
146400090529      *
146500090529     c                   else
146600090529      *
146700090529      *  lo memorizza x la prima volta
146800090529     c                   clear                   fiar5000
146900090529     c                   eval      ar5AAS = blpAAS
147000090529     c                   eval      ar5LNP = blpLNP
147100090529     c                   eval      ar5NRS = blpNRS
147200090529     c                   eval      ar5NSP = blpNSP
147300090529     c                   eval      ar5TRD = 'VAC'
147400090529     c                   eval      ar5DAC = dateu
147500090529     c                   eval      ar5ORC = Orario
147600090529     c                   eval      ar5PRG = AR5PRG + 1
147700090603      *
147800090603     c                   eval      VacGC1chg = VacGC1
147900090603     c                   eval      VacGC2chg = VacGC2
148000090603     c                   eval      VacFFDchg = VacFFD
148100090603     c                   eval      VacTCRchg = VacTCR
148200090603     c                   eval      VacDCRchg = VacDCR
148300090603     c                   eval      VacHCRchg = VacHCR
148400090603     c                   eval      VacDCMchg = VacDCM
148500090603     c                   eval      VacHMCchg = VacHMC
148600090603     c                   eval      VacTC1chg = VacTC1
148700090603     c                   eval      VacTC2chg = VacTC2
148800090603     c                   eval      VacCCAchg = VacCCA
148900090603     c                   eval      VacDLAchg = VacDLA
149000090603     c                   eval      VacDAGchg = VacDAG
149100090529     c                   eval      ar5UNI = DSvacHG
149200090529     c                   write     fiar5000
149300090529      *
149400090529     c                   end
149500090529     c                   end
149600090529      *  >>>>>>>>>
149700170322     C*
149800170322     C* Verifico rottura di codice per cliente unificante
149900170322     C                   exsr      ROTVAC
150000170322     C*
150100080417     C                   clear                   tivgd000
150200170322     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
150300170322     C                   eval      vgdTIP = 'VC'
150400170322     C                   eval      vgdKSU = '0'+%editc(§3KCKS:'X')
150500170322     C                   eval      vgdTSC = §3KCSD
150600170322     C                   eval      vgdDAT = dateu
150700170322     C                   eval      vgdPRG = o§7VASPRG
150800170322     C                   eval      vgdPGM = procname
150900170322     C                   eval      vgdSTO = '?'
151000080417      *
151100080417      *  x generare l'EDI:
151200080417      *    occorre farlo in modo controllato (personalizzato)
151300080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
151400080417     C                   clear                   DVGDFLO
151500080417     C                   movel     'P'           §VGDFELA
151600080417     C                   movel     DVGDFLO       vgdflo
151700170322     C                   eval      vgdPRG = *blanks
151800170322     C                   eval      vgdSTO = *blanks
151900080417     c                   end
152000080417      *
152100091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
152200091116      *  Non deve scrivere nulla.
152300170322     c                   if        §3KCSD <> *blank
152400080417     C                   write     tivgd000
152500110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
152600110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
152700110721     C                   MOVEL     'S'           WFLDT3
152800091116     c                   end
152900080417      *
153000080417     C                   ENDSR
153100170322
153200170322
153300170322
153400170322     C*------------------------------------------------------------------------*
153500170322     C* EXEERR - Routine di esecuzione azioni su Errore
153600170322     C*------------------------------------------------------------------------*
153700170322     C     EXEERR        BEGSR
153800170322     C*
153900170322     C                   dump(A)
154000170322     C                   rolbk(e)
154100170322     C                   seton                                        lr
154200170322     C                   return
154300170322     C*
154400170322     C                   ENDSR
154500170322     C*------------------------------------------------------------------------*
154600170322
154700170322
154800170322
154900170322     C*------------------------------------------------------------------------*
155000170322     C* ROTVAC - Routine di gestione rottura di codice VAC
155100170322     C*------------------------------------------------------------------------*
155200170322     C     ROTVAC        BEGSR
155300170322     C*
155400170322     C* Verifico rottura di codice per cliente unificante
155500170322     C                   if        §3KCKS <> s3KCKS
155600170322     C*
155700170322     C* Stacco progressivo univoco download
155800170322     C                   exsr      RTVPRG
155900170322     C*
156000170322     C* Se non primo giro => Finalizzo transazione precedente
156100170322     C                   if        s§7VASPRG <> *blanks
156200170322     C                   exsr      RLSPRG
156300170322     C                   endif
156400170322     C*
156500170322     C* Salvo l'unificante corrente
156600170322     C                   eval      s3KCKS = §3KCKS
156700170322     C                   eval      s3KCSD = §3KCSD
156800170322     C*
156900170322     C* Salvo il progressivo univoco download corrente
157000170322     C                   eval      s§7VASPRG = o§7VASPRG
157100170322     C                   endif
157200170322     C*
157300170322     C                   ENDSR
157400170322     C*------------------------------------------------------------------------*
157500170322
157600170322
157700170322
157800170322     C*------------------------------------------------------------------------*
157900170322     C* RTVPRG - Routine di reperimento progressivo univoco transazione
158000170322     C*------------------------------------------------------------------------*
158100170322     C     RTVPRG        BEGSR
158200170322     C*
158300170322     c                   CLEAR                   TIS7VASDS
158400170322     c                   EVAL      i§7VASOPZ = 'PRG'
158500170322     c                   CALL(e)   'TIS7VASR1'
158600170322     c                   PARM                    TIS7VASDS
158700170322     c*
158800170322     c                   if        %error OR
158900170322     c                             o§7VASOK = *off OR
159000170322     c                             o§7VASPRG = *blanks
159100170322     c                   exsr      EXEERR
159200170322     c                   endif
159300170322     C*
159400170322     C                   ENDSR
159500170322     C*------------------------------------------------------------------------*
159600170322
159700170322
159800170322
159900170322     C*------------------------------------------------------------------------*
160000170322     C* RLSPRG - Routine di finalizzazione transazione
160100170322     C*------------------------------------------------------------------------*
160200170322     C     RLSPRG        BEGSR
160300170322     C*
160400170322     C                   EVAL      i§7VASOPZ  = 'RLS'
160500170322     C                   EVAL      i§7VASTIP  = 'VC'
160600170322     C                   EVAL      i§7VASKSU  = '0'+%editc(s3KCKS:'X')
160700170322     C                   EVAL      i§7VASTSC  = s3KCSD
160800170322     C                   EVAL      i§7VASSTO  = '?'
160900170322     C                   EVAL      i§7VASSTTO = 'G'
161000170322     C                   EVAL      i§7VASPRG  = s§7VASPRG
161100170322     C                   CALL(e)   'TIS7VASR1'
161200170322     C                   PARM                    TIS7VASDS
161300170322     C*
161400170322     C* Verifico esito chiamata
161500170322     C                   if        %error OR o§7VASOK = *off
161600170322     C                   exsr      EXEERR
161700170322     C                   endif
161800170322     C*
161900170322     C                   ENDSR
162000170322     C*------------------------------------------------------------------------*
162100170322
162200170322
162300170322
162400170322     C*------------------------------------------------------------------------*
162500170322     C* RLTPRG - Routine di finalizzazione transazione (solo TIP)
162600170322     C*------------------------------------------------------------------------*
162700170322     C     RLTPRG        BEGSR
162800170322     C*
162900170322     C                   EVAL      i§7VASOPZ  = 'RLT'
163000170322     C                   EVAL      i§7VASTIP  = 'VG'
163100170322     C                   EVAL      i§7VASSTO  = '?'
163200170322     C                   EVAL      i§7VASSTTO = 'G'
163300170322     C                   EVAL      i§7VASPRG  = g§7VASPRG
163400170322     C                   CALL(e)   'TIS7VASR1'
163500170322     C                   PARM                    TIS7VASDS
163600170322     C*
163700170322     C* Verifico esito chiamata
163800170322     C                   if        %error OR o§7VASOK = *off
163900170322     C                   exsr      EXEERR
164000170322     C                   endif
164100170322     C*
164200170322     C                   ENDSR
164300170322     C*------------------------------------------------------------------------*
164400170322
164500060214     OFiar4000  E            AGGar4
164600060214     O                       ar4FTR
164700060214     O                       ar4DTR
164800921026**
164900981210DLTOVR FILE(*ALL)
165000000726**
165100921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
165200921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
165300921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
165400001031**
165500001031Consegna parziale
165600001031Nr. colli consegnati xxx su xxx
