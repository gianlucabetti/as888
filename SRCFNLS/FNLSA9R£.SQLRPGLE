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
008100050606     D fnvagds       E DS
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
067800170309       //--------------------------------------------------------------
067900170309       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3K
068000170309       //--------------------------------------------------------------
068100170309       begsr  setll_sql;
068200170309
068300170309         wSQL = 'select * +
068400170309                 from tabel00f +
068500170309                 where +
068600170309                 tblkut = 1 and +
068700170309                 tblcod = ''3K'' +
068800170309                 order by +
068900170309                 substr(TBLUNI , 27 , 7)';
069000170309
069100170309         // Dichiarazione cursore
069200170309         exec sql   prepare S1   from :wSQL;
069300170309         exec sql   declare C1  asensitive   cursor for S1;
069400170309
069500170309         // Apertura del cursore
069600170309         exec sql   open C1;
069700170309
069800170309         *in99 = *off;
069900170309         if SQLCode < 0;
070000170309           *in99 = *on;
070100170309         endif;
070200170309
070300170309       endsr;
070400170309       //--------------------------------------------------------------
070500170309       // fetch_sql  - legge il rcdset di TABEL00F
070600170309       //--------------------------------------------------------------
070700170309       begsr  fetch_sql;
070800170309
070900170309         exec sql  fetch next  from C1  into :TABELDS;
071000170309
071100170309         *in31 = *off;
071200170309         if SQLCode = 100 or SQLCode < 0;
071300170309           *in31 = *on;
071400170309         endif;
071500170309
071600170309       endsr;
071700170309       //--------------------------------------------------------------
071800170309       // close_sql  - chiusura cursore
071900170309       //--------------------------------------------------------------
072000170309       begsr  close_sql;
072100170309
072200170309         // chiusura del cursore
072300170309         exec sql  close C1;
072400170309
072500170309       endsr;
072600170309
072700060921      **?-------------------------------------------------------------*
072800060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
072900060921      **?-------------------------------------------------------------*
073000060921     C     ChecK_Figlie  BEGSR
073100060921      *
073200060921      *  inizializza flag di work della Routine
073300060921     c                   move      'S'           Primo_legame      1
073400060921      *
073500060921      **?chiave salvata della Bolla x partire nella ricerca
073600060921     c                   z-add     blpAAS        klb2_AAS
073700060921     c                   z-add     blpLNP        klb2_LNP
073800060921     c                   z-add     blpNRS        klb2_NRS
073900060921     c                   z-add     blpNSP        klb2_NSP
074000060921      *
074100060921      *  Chiave Bolla Precedente
074200060921     c     new_ciclo     tag
074300060921      **?                                  Flag di work del ciclo
074400060921     c                   move      'N'           ha_figlia         1
074500060921     C     Klbl2         setLL     FnLBL02L
074600060921     C     Klbl2         reade     FnLBL02L
074700060921      *
074800060921     c                   Dow       not %EoF(FnLBL02L)
074900060921      **?primo controllo:
075000060921      *    tutta la catena deve partire dalla bolla mamma che è
075100060921      *     uguale alla bolla originale del primo record della catena
075200060921     c                   if          Primo_legame = 'S'
075300060921     c                   eval      Primo_legame = 'N'
075400060921     c                   if          blpAAS <> exLBLAAo   or
075500060921     c                               blpLNP <> exLBLLPo   or
075600060921     c                               blpNRS <> exLBLNRo   or
075700060921     c                               blpNSP <> exLBLNSo
075800060921      *    se così non è non deve saltare il ciclo e non impostare i
075900060921      *     campi chiave x il contrassegno.
076000060921     c                   leave
076100060921     c                   end
076200060921     c                   end
076300060921      **?secondo controllo:
076400060921      *     non deve essere una bolla locale
076500060921      *   (sulle locali non c'è l'informazione di contrassegno)
076600060921     c                   if        exLBLLPN <> exLBLLPP
076700060921      *  imposta la chiave dell'ultima figlia
076800060921      *       per il prossimo giro di controllo
076900060921     c                   z-add     exLBLAAN      klb2_AAS
077000060921     c                   z-add     exLBLLPN      klb2_LNP
077100060921     c                   z-add     exLBLNRN      klb2_NRS
077200060921     c                   z-add     exLBLNSN      klb2_NSP
077300060921     c                   move      'S'           ha_figlia
077400060921     c                   end
077500060921      *
077600060921      **?   (attenzione a non sposizionarsi)
077700060921      * Chiave impostata come Bolla Precedente appena letta
077800060921     C     KLBL2P        reade     FnLBL02L
077900060921     c                   end
078000060921      *
078100060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
078200060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
078300060921      *   se invece non ha trovato delle figlie allora va a fine con la
078400060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
078500060921     c                   if        ha_figlia = 'S'
078600060921     c                   if        exLBLAAp <> klb2_AAS or
078700060921     c                             exLBLLPp <> klb2_LNP or
078800060921     c                             exLBLNRp <> klb2_NRS or
078900060921     c                             exLBLNSp <> klb2_NSP
079000060921     c                   goto      new_ciclo
079100060921     c                   end
079200060921     c                   end
079300060921      *
079400060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
079500060921      **?   x verificare la presenza del contrassegno su AR9
079600060921      *
079700060921     C                   ENDSR
079800060921      **?-------------------------------------------------------------*
079900000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
080000921026     C*
080100131119     C*   per quel che riguarda le POSTE
080200131119     C*    NON è stato adeguato alla logica degli eventi passati dal PDA
080300131119     C*     quindi con i flag del progressivo 'P' e 'U'
080400131119     C*
080500000526     C     POSTE         BEGSR
080600000526      *
080700000526     C                   CLEAR                   PTTIC
080800000526     C                   CLEAR                   PTCAS
080900000526     C                   CLEAR                   PTVCA
081000000526     C                   CLEAR                   PTIAS
081100000526     C                   CLEAR                   PTVAS
081200000526     C                   CLEAR                   PTDTG
081300000526     C                   CLEAR                   §POST
081400000526      *
081500000526     C                   SETOFF                                       22
081600000526     C     BLPLNP        LOOKUP    FIP                                    22
081700000526     C   22              MOVEL     'S'           §POST             1
081800131113      *
081900000526     C     §POST         IFEQ      'S'
082000131113      *
082100000526      * Se la bolla è stata chiusa con consegna anomala la merce
082200000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
082300000526      * questo è stato annullato in quanto non più incassabile da
082400000526      * Bartolini.
082500000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
082600000526      * informazione da ritornare alla SDA
082700000526     C     BLPCCA        IFNE      *BLANK
082800051115     C     ar9CAS        ANDEQ     *ZEROS
082900000526     C     KBLP          CHAIN     FNARBK00                           22
083000000526     C     *IN22         IFEQ      '0'
083100000526     C                   MOVEL     ARBTIC        PTTIC
083200000526     C                   Z-ADD     ARBCAS        PTCAS
083300000526     C                   MOVEL     ARBVCA        PTVCA
083400000526     C                   ENDIF
083500000526     C                   ENDIF
083600000526      *
083700060214      * Prendo importo da assicurare da Fiar4 t.r.=J
083800000526     C                   MOVEL     'J'           KTRC
083900060214     C     Kar4          CHAIN     Fiar401L                           35
084000000526     C     *IN35         IFEQ      '0'
084100060214     C                   MOVEL     ar4NOT        DSBL4J
084200000526     C                   Z-ADD     §B4IAS        PTIAS
084300000526     C                   MOVEL     §B4VAS        PTVAS
084400000526     C                   ENDIF
084500000526      *
084600000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
084700000526      * gestite le giacenze per le bolle poste
084800000526      *
084900131113     C     kBLP          SETGT     FNEVB000
085000131113     C     kBLP          READPE    FNEVB000                               34
085100000526      * Devo cercare l'ultimo evento di tipo giacenza
085200000526      *
085300000526     C     *IN34         DOWEQ     *OFF
085400000526     C                   Z-ADD     1             WW
085500000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
085600131119     c                   if         *in42 and T2A(WW) ='G'
085700000526     C                   Z-ADD     EVBDEV        PTDTG
085800000619     C                   LEAVE
085900131119     C                   ENDIF
086000131119      *
086100131113     C     kBLP          READPE    FNEVB000                               34
086200000526     C                   ENDDO
086300000526     C                   ENDIF
086400000526      *
086500000526     C                   ENDSR
086600000526     C*
086700941228     C* CREO RECORD FNVAC01L -----------------------------------------*
086800921026     C     CARBLR        BEGSR
086900080417      *
087000080417     c                   clear                   fnvacds
087100080417      *
087200941228     C                   MOVEL     BLPAAS        VACAAS
087300941228     C                   MOVEL     BLPLNP        VACLNP
087400941228     C                   MOVEL     BLPMGS        VACMGS
087500941228     C                   MOVEL     BLPNRS        VACNRS
087600941228     C                   MOVEL     BLPNSP        VACNSP
087700941228     C                   MOVEL     BLPCBO        VACCBO
087800941228     C                   MOVEL     BLPLNA        VACLNA
087900941228     C                   MOVEL     BLPRSD        VACRSD
088000941228     C                   MOVEL     BLPPRD        VACPRD
088100941228     C                   MOVEL     BLPGC1        VACGC1
088200941228     C                   MOVEL     BLPGC2        VACGC2
088300941228     C                   MOVEL     BLPCTR        VACCTR
088400941228     C                   MOVEL     BLPCTS        VACCTS
088500941228     C                   MOVEL     BLPFTM        VACFTM
088600941228     C                   MOVEL     BLPFIN        VACFIN
088700941228     C                   MOVEL     BLPFAP        VACFAP
088800941228     C                   MOVEL     BLPTSP        VACTSP
088900000526      *
089000000526     C     §POST         IFEQ      'S'
089100000526     C                   MOVEL     PTIAS         VACIAS
089200000526     C                   MOVEL     PTVAS         VACVAS
089300000526     C                   ELSE
089400000526     C                   MOVEL     BLPIAS        VACIAS
089500000526     C                   MOVEL     BLPVAS        VACVAS
089600000526     C                   ENDIF
089700000526      *
089800941228     C                   MOVEL     BLPNAS        VACNAS
089900941228     C                   MOVEL     BLPNCL        VACNCL
090000941228     C                   MOVEL     BLPPKF        VACPKB
090100941228     C                   MOVEL     BLPVLF        VACVLB
090200941228     C                   MOVEL     BLPQFT        VACQFT
090300000526      *
090400000526     C     §POST         IFEQ      'S'
090500051115     C     ar9CAS        ANDEQ     *ZEROS
090600000526     C                   MOVEL     PTCAS         VACCAS
090700000526     C                   MOVEL     PTVCA         VACVCA
090800000526     C                   MOVEL     PTTIC         VACTIC
090900000526     C                   ELSE
091000051115     C                   MOVEL     ar9CAS        VACCAS
091100051115     C                   MOVEL     ar9VCA        VACVCA
091200051115     C                   MOVEL     ar9TIC        VACTIC
091300000526     C                   ENDIF
091400000526      *
091500060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
091600011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
091700011002      *
091800011002     C     *IN68         IFEQ      '1'
091900060214     C                   MOVEL     ar4KSC        VACCCM
092000011002     C                   ELSE
092100921026     C     BLPCCM        IFNE      0
092200941228     C                   MOVEL     BLPCCM        VACCCM
092300921026     C                   ELSE
092400941228     C                   MOVEL     BLPKSC        VACCCM
092500921026     C                   END
092600011002     C                   END
092700131113      *
092800941228     C                   MOVEL     BLPRMN        VACRMN
092900941228     C                   MOVEL     BLPRMA        VACRMA
093000941228     C                   MOVEL     BLPRMO        VACRMO
093100941228     C                   MOVEL     BLPFFD        VACFFD
093200131113      *
093300000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
093400000921     C     §POST         IFEQ      'S'
093500000921     C                   Z-ADD     BLPNCD        VACDCR
093600000921     C                   ELSE
093700000921     C                   MOVEL     BLPDCR        VACDCR
093800000921     C                   ENDIF
093900941228     C                   MOVEL     BLPTCR        VACTCR
094000941228     C                   MOVEL     BLPHCR        VACHCR
094100941228     C                   MOVEL     BLPTC1        VACTC1
094200941228     C                   MOVEL     BLPTC2        VACTC2
094300941228     C                   MOVEL     BLPDCM        VACDCM
094400941228     C                   MOVEL     BLPHMC        VACHMC
094500941228     C                   MOVEL     BLPCCA        VACCCA
094600941228     C                   MOVEL     WDLA          VACDLA
094700000526      *
094800000526     C     §POST         IFEQ      'S'
094900000526     C                   MOVEL     PTDTG         VACDAG
095000000526     C                   ELSE
095100941228     C  N33              Z-ADD     GCPMGC        VACDAG
095200941228     C  N33              MOVEL     GCPAGC        VACDAG
095300941228     C   33              MOVEL     *ZEROS        VACDAG
095400000526     C                   ENDIF
095500000526      *
095600990602     C*---------------------------------------------------------------*
095700990602     C* Se ho trovato un evanto di messa in consegna....
095800060912 1   C     WTRMIC        IFEQ      'S'
095900990602     C* ... e non ho la data di consegna totale sulla bolla ..
096000990602     C     BLPDCM        ANDEQ     0
096100040319      *
096200990602     C* Verifico se ho trovato una pratica C.A. aperta ...
096300060912 2   C     DATMAX        IFNE      0
096400990602     C     §3KCQ4        ANDEQ     'S'
096500990602     C* .. e se l'apertura di quest'ultima è successiva alla
096600990602     C*   messa in consegna ...
096700990602     C     DATMAX        ANDGE     WDTMIC
096800990602     C* .. allora imposto la data apertura C.A. nella data consegna
096900990602     C*    e imposto il tipo consegna anomala = 'A'
097000990602     C                   Z-ADD     1200          VACHMC
097100990602     C                   Z-ADD     DATMAX        VACDCM
097200990602     C                   MOVEL     'A'           VACCCA
097300060912 x2  C                   ELSE
097400990602     C* ... altrimenti imposto data messa in consegna
097500990602     C                   Z-ADD     EVBHEV        VACHMC
097600980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
097700990602     C                   MOVEL     'C'           VACCCA
097800060912 e2  C                   END
097900990602     C*
098000990602     C* ... se è stata impostata la data di consegna sulla bolla ..
098100060912 x1  C                   ELSE
098200040319     C*
098300060912 2   C     BLPDCM        IFNE      0
098400040319     C*
098500040319     C* Un tempo era stato asteriscato questo pezzo
098600040319     C* >>>>>>>>>>>>>
098700040409      * non deve essere fatto per l'estero
098800060912 3   C     wtpINV        Ifne      'E'
098900040409      *
099000990602     C* ... verifico se ho trovato una C.A. aperta ...
099100060912 4   C     DATMAX        IFNE      0
099200040319     C     §3KCQ4        ANDEQ     'S'
099300990602     C* .. e se l'apertura di quest'ultima è successiva alla
099400990602     C* .. consegna ...
099500040319     C     DATMAX        ANDGT     BLPDCM
099600040319     C*
099700990602     C* .. allora imposto la data apertura C.A. nella data consegna
099800990602     C*    e imposto il tipo consegna anomala = 'A'
099900040319     C                   Z-ADD     1200          VACHMC
100000040319     C                   Z-ADD     DATMAX        VACDCM
100100040319     C                   MOVEL     'A'           VACCCA
100200040319     C*
100300060912 e4  C                   ENDif
100400060912 e3  c                   end
100500040319     C* >>>>>>>>>>>>>
100600040319     C*
100700990602     C* ... se non è stata effettuata la messa in consegna, la
100800990602     C*     bolla risulta non consegnata, ....
100900060912 x2  C                   ELSE
101000990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
101100990602     C*     le consegne anomale, imposto la data consegna + flag
101200990602     C*     consegna anomala='A'
101300060912 3   C     DATMAX        IFNE      0
101400990602     C     §3KCQ4        ANDEQ     'S'
101500990602     C                   Z-ADD     1200          VACHMC
101600990602     C                   Z-ADD     DATMAX        VACDCM
101700990602     C                   MOVEL     'A'           VACCCA
101800060912 e3  C                   END
101900060912 e2  C                   END
102000060912 e1  C                   END
102100060912     C* Se ho trovato un evento di ripristino
102200060912 1   C     WTRrip        IFEQ      'S'
102300060912     C     BLPDCM        ANDEQ     0
102400060912     C     §3KCQ1        ANDEQ     'S'
102500060912     C                   Z-ADD     0             VACHMC
102600060912     C                   Z-ADD     0             VACDCM
102700060912     C                   MOVEL     'R'           VACCCA
102800060912 e1  c                   endif
102900060912
103000080417     c                   exsr      Write_VAC
103100930603     C                   SETON                                        06
103200990604     C*
103300990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
103400990618     C* e questa è stata anche.....
103500060912 1   C     §3KCQ1        IFEQ      'S'
103600080417     C     §3KCSD        ANDEQ     'EW'
103700080417      *
103800060912 2   C     VACCCA        IFEQ      'A'
103900990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
104000990618     C* invio al partner dell'informazione della consegna parziale
1041000609123    C     BLPDCM        IFEQ      0
104200990618     C     BLPDCP        ANDNE     0
104300990618     C                   Z-ADD     0             VACHMC
104400990618     C                   Z-ADD     0             VACDCM
104500990618     C                   MOVEL     ' '           VACCCA
104600080417     c                   exsr      Write_VAC
104700060912x3   C                   ELSE
104800990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
104900990618     C* invio al partner dell'informazione
105000060912 4   C     BLPDCM        IFEQ      0
105100990624     C     WDTMIC        ANDGT     0
105200990618     C                   Z-ADD     EVBHEV        VACHMC
105300990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
105400990618     C                   MOVEL     'C'           VACCCA
105500080417     c                   exsr      Write_VAC
105600060912e4   C                   END
105700060912e3   C                   END
105800990618     C*
105900990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
106000990618     C* scrivo anche record x apertura C.A.
106100060912x2   C                   ELSE
1062000609123    C     DATMAX        IFNE      0
106300990618     C     §3KCQ4        ANDEQ     'S'
106400990618     C                   Z-ADD     1200          VACHMC
106500990618     C                   Z-ADD     DATMAX        VACDCM
106600990618     C                   MOVEL     'A'           VACCCA
106700080417     c                   exsr      Write_VAC
106800060912 e3  C                   END
106900060912 e2  C                   END
107000060912 e1  C                   END
107100990618     C*
107200921026     C                   ENDSR
107300990602     C*---------------------------------------------------------------*
107400990602     C* Controllo se esiste una pratica C.A. aperta                  -*
107500990602     C*---------------------------------------------------------------*
107600990602     C     CRTCA         BEGSR
107700990602     C*
107800990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
107900990602     C* per reperire la pratica di danno con data più alta
108000990602     C                   Z-ADD     0             DATMAX            8 0
108100990602     C     §3KCQ4        IFEQ      'S'
108200040715      **
108300040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
108400040715      * SIA COME MAMMA CHE COME FIGLIA
108500040715      **
108600040715     c                   clear                   fidn12ds
108700040715     c                   eval      i12aas = blpAAS
108800040715     c                   eval      i12lnp = blpLNP
108900040715     c                   eval      i12nrs = blpNRS
109000040715     c                   eval      i12nsp = blpNSP
109100040715     c                   eval      i12fel = 'E'
109200040715     c                   eval      i12fan = 'E'
109300040715     c                   eval      i12fch = 'E'
109400040715     c                   call      'FIDN12R'
109500040715     c                   parm                    fidn12ds
109600040715     c                   z-add     *zeros        ii                2 0
109700040715      **
109800040715      * prende fra le C.A. aperte quella con la data ultima
109900040715     c                   if        o12nca > 0
110000040715     c                   dow       ii <  o12nca
110100040715      **
110200040715     c                   add       1             ii
110300040715     C                   movel     skDca(ii)     datape            8 0
110400040715     C     DATMAX        IFLT      DATAPE
110500040715     C                   Z-ADD     DATAPE        DATMAX
110600040715     C                   END
110700040715      *
110800040715     c                   enddo
110900040715     c                   endif
111000040715      *
111100040715     C                   END
111200040715     C*
111300990602     C                   ENDSR
111400970122     C*---------------------------------------------------------------*
111500970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
111600970122     C*---------------------------------------------------------------*
111700970121     C     TSTNMC        BEGSR
111800040422     **
111900040422     ** avendo fatto entrare anche x data consegna GT di 0
112000040422     ** il flag deve essere resettato solo per la vecchia condizione
112100040422     ** altrimenti non scrive + il VAC come una volta.
112200040422     c                   if        blpdcm = 0
112300040422     **
112400970122     C                   MOVEL     'N'           WFLDT3
112500090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
112600090223     C**   sempre il VAC.
112700090223     C********           MOVEL     'N'           WINVAC
112800040422     **
112900040422     c                   end
113000040422     **
113100970122     C                   MOVEL     'N'           WTREVE            1
113200970122     C                   MOVEL     *BLANKS       WSVTMC            1
113300970122     C                   MOVEL     *BLANKS       WMOT1            35
113400970122     C                   MOVEL     *BLANKS       WMOT2            35
113500970122     C*  Leggo le estensioni bolla della spedizione e per quelle
113600970122     C*  relative ad un motivo di mancata consegna scrivo VAG
113700970122     C                   DO        Y             W
113800970122     C                   MOVEL     T7K(W)        WTMC              1
113900970122     C                   MOVE      T7K(W)        KTRC
114000060214     C     Kar4          CHAIN     Fiar401L                           35
114100970123     C     *IN35         IFEQ      '0'
114200060214     C     ar4FTR        ANDNE     'T'
114300060214     C     ar4FTR        ANDNE     'C'
114400970122     C*  Se ho trovato estensione bolla e non è trasmesso
114500970122     C*  controllo se ho un nuovo tp.mancata consegna
114600970122     C*  e se si reperisco evento corrispondente
114700970122     C     WTMC          IFNE      WSVTMC
114800970122     C                   EXSR      REPEVE
114900970122     C                   END
115000970122     C*  se ho trovato l'evento controllo se devo scrivere
115100970122     C*  nuovo record VAG:
115200970122     C     WTREVE        IFEQ      'S'
115300970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
115400970122     C*  - se ho già scritto 2 motivi --> new record
115500970122     C     WTMC          IFNE      WSVTMC
115600970122     C     WMOT2         ORNE      *BLANKS
115700970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
115800970124     C     WSVTMC        IFNE      *BLANKS
115900970124     C     WMOT2         ANDEQ     *BLANKS
116000970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
116100970318     C*  primi 15 chr nella seconda motivazione
116200970318     C     WCHR15        IFNE      *BLANKS
116300970318     C                   MOVEL     WCHR15        VAGNOT
116400970318     C                   END
116500170322     C*
116600170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
116700170322     C                   eval      vgdTIP = 'VG'
116800170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
116900170322     C                   eval      vgdTSC = §3KGSD
117000170322     C                   eval      vgdDAT = dateu
117100170322     C                   eval      vgdPRG = g§7VASPRG
117200170322     C                   eval      vgdPGM = procname
117300170322     C                   eval      vgdSTO = '?'
117400170322     c                   if        §3KGSD  <> *blanks
117500170322     C                   WRITE     tivgd000
117600170322     c                   endif
117700970124     C                   SETON                                        06
117800970124     C                   END
117900970124     C*  New record
118000970122     C                   EXSR      NEWVAG
118100970122     C                   ELSE
118200970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
118300970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
118400970318     C     WCHR15        IFNE      *BLANKS
118500970318     C                   MOVEL     WCHR15        VAGNOT
118600060214     C                   MOVE      ar4NOT        VAGNOT
118700970318     C                   ELSE
118800970318     C*  Se codice generico occupo chr. da 32 posizione
118900970318     C     VAGCMC        IFEQ      'G  '
119000970318     C     VAGCMC        OREQ      'GEN'
119100060214     C                   MOVEL     ar4NOT        WCHR19           19
119200060214     C                   MOVE      ar4NOT        WCHR16           16
119300970318     C                   MOVE      WCHR19        VAGDMC
119400970318     C                   MOVEL     WCHR16        VAGNOT
119500970318     C                   ELSE
119600970318     C*  Altrimenti imposto solo ulteriore descrizione
119700060214     C                   MOVEL     ar4NOT        VAGNOT
119800970318     C                   END
119900970318     C                   END
120000060214     C                   MOVEL     ar4NOT        WMOT2
120100970122     C                   MOVEL     *BLANKS       WMOT1
120200170322     C*
120300170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
120400170322     C                   eval      vgdTIP = 'VG'
120500170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
120600170322     C                   eval      vgdTSC = §3KGSD
120700170322     C                   eval      vgdDAT = dateu
120800170322     C                   eval      vgdPRG = g§7VASPRG
120900170322     C                   eval      vgdPGM = procname
121000170322     C                   eval      vgdSTO = '?'
121100170322     c                   if        §3KGSD  <> *blanks
121200170322     C                   WRITE     tivgd000
121300170322     c                   endif
121400970122     C                   SETON                                        06
121500970122     C                   END
121600970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
121700970122     C                   MOVEL     'S'           WFLDT3
121800970122     C                   MOVEL     'S'           WINVAC
121900060214     C*  Fleggo Fiar4
122000060214     C                   MOVEL     'C'           ar4FTR
122100060214     C                   Z-ADD     DATEU         ar4DTR
122200060214     C                   EXCEPT    AGGar4
122300970122     C                   END
122400970122     C                   END
122500970122     C*
122600970122     C                   END
122700970122     C*  Controllo se mi è rimasto un VAG da scrivere
122800970122     C     WMOT1         IFNE      *BLANKS
122900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
123000970318     C*  primi 15 chr nella seconda motivazione
123100970318     C     WCHR15        IFNE      *BLANKS
123200970318     C                   MOVEL     WCHR15        VAGNOT
123300970318     C                   END
123400170322     C*
123500170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
123600170322     C                   eval      vgdTIP = 'VG'
123700170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
123800170322     C                   eval      vgdTSC = §3KGSD
123900170322     C                   eval      vgdDAT = dateu
124000170322     C                   eval      vgdPRG = g§7VASPRG
124100170322     C                   eval      vgdPGM = procname
124200170322     C                   eval      vgdSTO = '?'
124300170322     c                   if        §3KGSD  <> *blanks
124400170322     C                   WRITE     tivgd000
124500170322     c                   endif
124600970122     C                   SETON                                        06
124700970122     C                   END
124800970122     C*
124900970122     C                   ENDSR
125000970122     C*---------------------------------------------------------------*
125100970122     C* Controllo se esiste evento                                   -*
125200970122     C*---------------------------------------------------------------*
125300970122     C     REPEVE        BEGSR
125400970122     C*
125500970122     C                   MOVEL     'N'           WTREVE
125600970122     C*  Mi posiziono su file eventi
125700131113     C     kBLP          SETGT     FNEVB01L
125800131113     C     kBLP          READPE    FNEVB01L                               34
125900131113      *
126000970122     C*  Loop letture fino a fine file o travato evento
126100970122     C     *IN34         DOWEQ     '0'
126200970122     C     WTREVE        ANDEQ     'N'
126300040504      **
126400040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
126500040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
126600040504      **  le A16/A23 come varianti in programmi che precedono facendo
126700040504      **  sballare il ragionamento in questo pgm.
126800040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
126900040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
127000040504     c                   movel     '0'           evbCEV
127100040504     c                   end
127200131113      *
127300131113      **  imposta sulla DS le note x testare il flag Progressivo
127400131113     c                   movel     evbNOT        Devb01
127500131119      *
127600131119      ** Sia per l'Italia che per L'estero deve sempre prendere l'ultimo EVENTO
127700131119      **  in quanto MANCATA CONSEGNA perchè la bolla sarà aggiornata solo con
127800131119      **  l'evento "U" o BLK
127900131119      *
128000131119     c                   if         §notPRG = ' ' or §notPRG = 'U'
128100131119      *
128200970122     C*  Reperisco tipo mancata consegna evento letto
128300970122     C                   Z-ADD     1             WW                4 0
128400970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
128500131113      *
128600970122     C*  Controllo se tipo mancata consegna evento letto
128700970122     C*  è uguale a quella dell'estensione bolla
128800131113      *
128900131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
129000131119     c                   if         *in42 and T2A(WW) = WTMC
129100970122     C                   MOVEL     'S'           WTREVE
129200131119     c                   leave
129300131119     C                   END
129400131113      *
129500131119     C                   EndIF
129600131119      *
129700131113     C     kBLP          READPE    FNEVB01L                               34
129800970122     C                   END
129900970122     C*
130000970122     C                   ENDSR
130100970122     C*---------------------------------------------------------------*
130200970122     C* Nuovo record FNVAG                                           -*
130300970122     C*---------------------------------------------------------------*
130400970122     C     NEWVAG        BEGSR
130500970122     C*
130600050606     C                   CLEAR                   FNVAGds
130700970721     C                   MOVEL     'S'           WRTVAG            1
130800970318     C                   MOVEL     *BLANKS       WCHR25           25
130900970318     C                   MOVEL     *BLANKS       WCHR15           15
131000970122     C                   Z-ADD     BLPAAS        VAGAAS
131100970122     C                   Z-ADD     BLPLNP        VAGLNP
131200970122     C                   Z-ADD     BLPNRS        VAGNRS
131300970122     C                   Z-ADD     BLPNSP        VAGNSP
131400970122     C                   Z-ADD     BLPLNA        VAGLNA
131500990810     C                   MOVEL     EVBDEV        WAA2              4 0
131600970123     C                   MOVE      WAA2          VAGAGC
131700970122     C                   MOVE      EVBDEV        VAGMGC
131800060214     C                   MOVEL     ar4NOT        VAGCMC
131900060214     C                   MOVEL     ar4NOT        WMOT1
132000970122     C                   MOVEL     *BLANKS       WMOT2
132100970307     C                   Z-ADD     1             YY                4 0
132200970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
132300970307     C     VAGCMC        IFNE      'G  '
132400970307     C     VAGCMC        ANDNE     'GEN'
132500970307     C                   MOVEL     D2A(YY)       VAGDMC
132600970318     C                   MOVE      D2A(YY)       WCHR25
132700970318     C                   MOVEL     WCHR25        WCHR15
132800970305     C                   ELSE
132900060214     C                   MOVE      ar4NOT        WCOM31           31
133000970122     C                   MOVEL     WCOM31        VAGDMC
133100970305     C                   END
133200970122     C                   MOVEL     BLPRMA        VAGRMA
133300970122     C                   Z-ADD     BLPRMN        VAGRMN
133400970122     C     BLPCCM        IFNE      0
133500970122     C                   MOVEL     BLPCCM        VAGSCM
133600970122     C                   ELSE
133700970122     C                   MOVEL     BLPKSC        VAGSCM
133800970122     C                   END
133900970123     C*
134000970123     C                   MOVEL     WTMC          WSVTMC
134100970122     C*
134200970122     C                   ENDSR
134300001031     C*---------------------------------------------------------------*
134400001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
134500001031     C*---------------------------------------------------------------*
134600001031     C     PARZIA        BEGSR
134700001031     C*
134800001031      * LOOP SU DETTAGLIO COLLI
134900001103     C                   Z-ADD     0             TOTCO
135000001031     C                   Z-ADD     0             CONS
135100001102     C                   Z-ADD     0             ULTIMA            8 0
135200001031     C     KBLP          SETLL     FNBLT01L
135300001031     C                   DO        *HIVAL
135400001102     C     KBLP          READE     FNBLT01L                               97
135500001031     C   97              LEAVE
135600001031      * CONTA CONSEGNATI E NON CONSEGNATI
135700001103     C     BLTDCM        IFGT      0
135800001031     C                   ADD       1             CONS              3 0
135900001031     C                   END
136000001103     C                   ADD       1             TOTCO             3 0
136100001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
136200001031     C     BLTDCM        IFGT      ULTIMA
136300001031     C                   Z-ADD     BLTDCM        ULTIMA
136400001031     C                   ENDIF
136500001031     C                   ENDDO
136600001102     C                   MOVE      CONS          CONSA             3
136700001103     C                   MOVE      TOTCO         TOTCOA            3
136800001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
136900001031      *
137000050606     C                   CLEAR                   FNVAGds
137100001031     C                   MOVE      BLPLNP        VAGLNP
137200001031     C                   MOVE      BLPAAS        VAGAAS
137300001031     C                   MOVE      BLPNRS        VAGNRS
137400001031     C                   MOVE      BLPNSP        VAGNSP
137500001107     C                   MOVEL     'P  '         VAGCMC
137600001031     C                   MOVE      BLPRMN        VAGRMN
137700001031     C                   MOVE      BLPRMA        VAGRMA
137800001031     C                   MOVE      BLPLNA        VAGLNA
137900001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
138000001031     C                   MOVEL     COS(1)        VAGDMC
138100001103     C                   MOVEA     COS(2)        COM
138200001102     C                   MOVEA     CONSA         COM(22)
138300001103     C                   MOVEA     TOTCOA        COM(29)
138400001102     C                   MOVEA     COM           VAGNOT
138500001031      *
138600001031     C     BLPCCM        IFGT      0
138700001031     C                   Z-ADD     BLPCCM        VAGSCM
138800001031     C                   ELSE
138900001031     C                   Z-ADD     BLPKSC        VAGSCM
139000001031     C                   END
139100001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
139200001031     C                   MOVEL     ULTIMA        VAGAGC
139300001031     C                   MOVE      ULTIMA        VAGMGC
139400170322     C*
139500170322     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
139600170322     C                   eval      vgdTIP = 'VG'
139700170322     C                   eval      vgdKSU = '0'+%editc(§3KGKS:'X')
139800170322     C                   eval      vgdTSC = §3KGSD
139900170322     C                   eval      vgdDAT = dateu
140000170322     C                   eval      vgdPRG = g§7VASPRG
140100170322     C                   eval      vgdPGM = procname
140200170322     C                   eval      vgdSTO = '?'
140300170322     c                   if        §3KGSD  <> *blanks
140400170322     C                   WRITE     tivgd000
140500170322     c                   endif
140600170322     C*
140700001106     C                   MOVEL     'S'           WTRPAR
140800001031      *
140900001031     C                   ENDSR
141000080417      *------------------------------------------------------------------------*
141100080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
141200080417      *------------------------------------------------------------------------*
141300080417     C     Write_VAC     BEGSR
141400080417      *
141500090529      *  >>>>>>>>>
141600090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
141700090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
141800090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
141900090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
142000090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
142100090529      *   il record.
142200090529      *  >>>>>>>>>
142300090603     c                   if        §3kCTB ='S'
142400090603     c                   clear                   uguale_VAC        1
142500090529      *
142600090529     C                   movel     'VAC'         Kar5vac
142700090529     c     kar5          chain     fiar501l
142800090529      *
142900090529     c                   if        %Found(fiar501l)
143000090529      * Imposta i campi precedentemente salvati nella DS da comparare
143100090529     c                   eval      fnvacHGds = ar5UNI
143200090603      *
143300090603     c                   eval      VacGC1chg = VacGC1
143400090603     c                   eval      VacGC2chg = VacGC2
143500090603     c                   eval      VacFFDchg = VacFFD
143600090603     c                   eval      VacTCRchg = VacTCR
143700090603     c                   eval      VacDCRchg = VacDCR
143800090603     c                   eval      VacHCRchg = VacHCR
143900090603     c                   eval      VacDCMchg = VacDCM
144000090603     c                   eval      VacHMCchg = VacHMC
144100090603     c                   eval      VacTC1chg = VacTC1
144200090603     c                   eval      VacTC2chg = VacTC2
144300090603     c                   eval      VacCCAchg = VacCCA
144400090603     c                   eval      VacDLAchg = VacDLA
144500090603     c                   eval      VacDAGchg = VacDAG
144600090529      * quindi
144700090529      *    controllo di uguaglianza
144800090529     c                   if        fnvacHGds = DSvacHG
144900090529     c                   eval      uguale_VAC = 'S'
145000090603     c                   move      '='           ar5UNI
145100090603     c                   else
145200090603     c                   move      ' '           ar5UNI
145300090603     c                   eval      ar5UNI    = DSvacHG
145400090603     c                   end
145500090529      *
145600090529     c                   eval      AR5DAC = dateu
145700090529     c                   eval      AR5ORC = Orario
145800090529     c                   eval      AR5PRG = AR5PRG + 1
145900090529     c*
146000090529     c                   update    fiar5000
146100090529     c*
146200090529     c                   if        uguale_VAC = 'S'
146300090529     c                   leavesr
146400090529     c                   end
146500090529      *
146600090529     c                   else
146700090529      *
146800090529      *  lo memorizza x la prima volta
146900090529     c                   clear                   fiar5000
147000090529     c                   eval      ar5AAS = blpAAS
147100090529     c                   eval      ar5LNP = blpLNP
147200090529     c                   eval      ar5NRS = blpNRS
147300090529     c                   eval      ar5NSP = blpNSP
147400090529     c                   eval      ar5TRD = 'VAC'
147500090529     c                   eval      ar5DAC = dateu
147600090529     c                   eval      ar5ORC = Orario
147700090529     c                   eval      ar5PRG = AR5PRG + 1
147800090603      *
147900090603     c                   eval      VacGC1chg = VacGC1
148000090603     c                   eval      VacGC2chg = VacGC2
148100090603     c                   eval      VacFFDchg = VacFFD
148200090603     c                   eval      VacTCRchg = VacTCR
148300090603     c                   eval      VacDCRchg = VacDCR
148400090603     c                   eval      VacHCRchg = VacHCR
148500090603     c                   eval      VacDCMchg = VacDCM
148600090603     c                   eval      VacHMCchg = VacHMC
148700090603     c                   eval      VacTC1chg = VacTC1
148800090603     c                   eval      VacTC2chg = VacTC2
148900090603     c                   eval      VacCCAchg = VacCCA
149000090603     c                   eval      VacDLAchg = VacDLA
149100090603     c                   eval      VacDAGchg = VacDAG
149200090529     c                   eval      ar5UNI = DSvacHG
149300090529     c                   write     fiar5000
149400090529      *
149500090529     c                   end
149600090529     c                   end
149700090529      *  >>>>>>>>>
149800170322     C*
149900170322     C* Verifico rottura di codice per cliente unificante
150000170322     C                   exsr      ROTVAC
150100170322     C*
150200080417     C                   clear                   tivgd000
150300170322     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
150400170322     C                   eval      vgdTIP = 'VC'
150500170322     C                   eval      vgdKSU = '0'+%editc(§3KCKS:'X')
150600170322     C                   eval      vgdTSC = §3KCSD
150700170322     C                   eval      vgdDAT = dateu
150800170322     C                   eval      vgdPRG = o§7VASPRG
150900170322     C                   eval      vgdPGM = procname
151000170322     C                   eval      vgdSTO = '?'
151100080417      *
151200080417      *  x generare l'EDI:
151300080417      *    occorre farlo in modo controllato (personalizzato)
151400080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
151500080417     C                   clear                   DVGDFLO
151600080417     C                   movel     'P'           §VGDFELA
151700080417     C                   movel     DVGDFLO       vgdflo
151800170322     C                   eval      vgdPRG = *blanks
151900170322     C                   eval      vgdSTO = *blanks
152000080417     c                   end
152100080417      *
152200091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
152300091116      *  Non deve scrivere nulla.
152400170322     c                   if        §3KCSD <> *blank
152500080417     C                   write     tivgd000
152600110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
152700110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
152800110721     C                   MOVEL     'S'           WFLDT3
152900091116     c                   end
153000080417      *
153100080417     C                   ENDSR
153200170322
153300170322
153400170322
153500170322     C*------------------------------------------------------------------------*
153600170322     C* EXEERR - Routine di esecuzione azioni su Errore
153700170322     C*------------------------------------------------------------------------*
153800170322     C     EXEERR        BEGSR
153900170322     C*
154000170322     C                   dump(A)
154100170322     C                   rolbk(e)
154200170322     C                   seton                                        lr
154300170322     C                   return
154400170322     C*
154500170322     C                   ENDSR
154600170322     C*------------------------------------------------------------------------*
154700170322
154800170322
154900170322
155000170322     C*------------------------------------------------------------------------*
155100170322     C* ROTVAC - Routine di gestione rottura di codice VAC
155200170322     C*------------------------------------------------------------------------*
155300170322     C     ROTVAC        BEGSR
155400170322     C*
155500170322     C* Verifico rottura di codice per cliente unificante
155600170322     C                   if        §3KCKS <> s3KCKS
155700170322     C*
155800170322     C* Stacco progressivo univoco download
155900170322     C                   exsr      RTVPRG
156000170322     C*
156100170322     C* Se non primo giro => Finalizzo transazione precedente
156200170322     C                   if        s§7VASPRG <> *blanks
156300170322     C                   exsr      RLSPRG
156400170322     C                   endif
156500170322     C*
156600170322     C* Salvo l'unificante corrente
156700170322     C                   eval      s3KCKS = §3KCKS
156800170322     C                   eval      s3KCSD = §3KCSD
156900170322     C*
157000170322     C* Salvo il progressivo univoco download corrente
157100170322     C                   eval      s§7VASPRG = o§7VASPRG
157200170322     C                   endif
157300170322     C*
157400170322     C                   ENDSR
157500170322     C*------------------------------------------------------------------------*
157600170322
157700170322
157800170322
157900170322     C*------------------------------------------------------------------------*
158000170322     C* RTVPRG - Routine di reperimento progressivo univoco transazione
158100170322     C*------------------------------------------------------------------------*
158200170322     C     RTVPRG        BEGSR
158300170322     C*
158400170322     c                   CLEAR                   TIS7VASDS
158500170322     c                   EVAL      i§7VASOPZ = 'PRG'
158600170322     c                   CALL(e)   'TIS7VASR1'
158700170322     c                   PARM                    TIS7VASDS
158800170322     c*
158900170322     c                   if        %error OR
159000170322     c                             o§7VASOK = *off OR
159100170322     c                             o§7VASPRG = *blanks
159200170322     c                   exsr      EXEERR
159300170322     c                   endif
159400170322     C*
159500170322     C                   ENDSR
159600170322     C*------------------------------------------------------------------------*
159700170322
159800170322
159900170322
160000170322     C*------------------------------------------------------------------------*
160100170322     C* RLSPRG - Routine di finalizzazione transazione
160200170322     C*------------------------------------------------------------------------*
160300170322     C     RLSPRG        BEGSR
160400170322     C*
160500170322     C                   EVAL      i§7VASOPZ  = 'RLS'
160600170322     C                   EVAL      i§7VASTIP  = 'VC'
160700170322     C                   EVAL      i§7VASKSU  = '0'+%editc(s3KCKS:'X')
160800170322     C                   EVAL      i§7VASTSC  = s3KCSD
160900170322     C                   EVAL      i§7VASSTO  = '?'
161000170322     C                   EVAL      i§7VASSTTO = 'G'
161100170322     C                   EVAL      i§7VASPRG  = s§7VASPRG
161200170322     C                   CALL(e)   'TIS7VASR1'
161300170322     C                   PARM                    TIS7VASDS
161400170322     C*
161500170322     C* Verifico esito chiamata
161600170322     C                   if        %error OR o§7VASOK = *off
161700170322     C                   exsr      EXEERR
161800170322     C                   endif
161900170322     C*
162000170322     C                   ENDSR
162100170322     C*------------------------------------------------------------------------*
162200170322
162300170322
162400170322
162500170322     C*------------------------------------------------------------------------*
162600170322     C* RLTPRG - Routine di finalizzazione transazione (solo TIP)
162700170322     C*------------------------------------------------------------------------*
162800170322     C     RLTPRG        BEGSR
162900170322     C*
163000170322     C                   EVAL      i§7VASOPZ  = 'RLT'
163100170322     C                   EVAL      i§7VASTIP  = 'VG'
163200170322     C                   EVAL      i§7VASSTO  = '?'
163300170322     C                   EVAL      i§7VASSTTO = 'G'
163400170322     C                   EVAL      i§7VASPRG  = g§7VASPRG
163500170322     C                   CALL(e)   'TIS7VASR1'
163600170322     C                   PARM                    TIS7VASDS
163700170322     C*
163800170322     C* Verifico esito chiamata
163900170322     C                   if        %error OR o§7VASOK = *off
164000170322     C                   exsr      EXEERR
164100170322     C                   endif
164200170322     C*
164300170322     C                   ENDSR
164400170322     C*------------------------------------------------------------------------*
164500170322
164600060214     OFiar4000  E            AGGar4
164700060214     O                       ar4FTR
164800060214     O                       ar4DTR
164900921026**
165000981210DLTOVR FILE(*ALL)
165100000726**
165200921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
165300921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
165400921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
165500001031**
165600001031Consegna parziale
165700001031Nr. colli consegnati xxx su xxx
