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
007500170309     D tabelds       e ds                  extname('TABEL00F')
007600970122     D DS7K          E DS
007700941228     D DS2A          E DS
007800000525     D OG143         E DS
007900000526     D DSBL4J        E DS
008000050606     D fnvagds       E DS
008100040715
008200040715     d Fidn12ds      e ds
008300040715     d  skKey                 26    305    dim(20)
008400040715     d  skAnn                306    325    dim(20)
008500040715     d  skDca                326    485    dim(20)
008600040715     d  skFca                486    545  0 dim(20)
008700040715     d  skDch                546    705  0 dim(20)
008800040715     d  skCch                706    745    dim(20)
008900170309
009000170309     d wSQl            s           1024    inz  varying
009100971126      *----------------------------------------------------------------
009200971126     IAZORG01L  AA
009300971126     I                             P    4    5 0ORGFIL
009400971126     I                                 11   11  ORGFL1
009500000525     I                               3776 3800  ORGDE3
009600090408     IFNARBG00      51
009700090408     IFNARBK00      52
009800920325     C*---------------------------------------------------------------*
009900920812     C* INDICATORI
010000920812     C*---------------------------------------------------------------*
010100921026     C* 01    - NON TUTTI I MEMBRI SONO STATI ALLOCATI
010200921026     C* 30    - LETTURA FLBLP00F
010300921026     C* 31    - LETTURA TABEL00F
010400921026     C* 32    - VEDO SE ESISTE GIA' BOLLA SU FLBLR
010500921026     C* 33    - GIACENZA
010600921026     C* 34    - COSEGNA PARTICOLARE NUMERICA (COD ANOMALIA)
010700970122     C* 35    - TROVATE ESTENSIONI BOLLE X MANCATE CONSEGNE
010800970123     C* 99    - ESEGUO CHIUSURA E DISALLOCAZIONE MEMBRO FNVAC
010900920812     C*---------------------------------------------------------------*
011000000000     C     *ENTRY        PLIST
011100000000     C                   PARM                    KPJBA
011200980407     C                   MOVEL     KPJBU         WTPINV            2
011300921026     C                   Z-ADD     1             CODUT
011400900511     C*---------------------------------------------------------------*
011500080417      *?Prima di iniziare, controlla se è possibile Procedere mediante  ?
011600080417      *? un controllo semaforico fatto con il TRUL47R.                  ?
011700080417      *  Se la strada è libera si può procedere altrimenti si va a fine.
011800080417     C*---------------------------------------------------------------*
011900080417      *? Come prima cosa avvio il blocco elaborazione TIVGD x tipo file ?
012000080417     C*---------------------------------------------------------------*
012100080417     C                   clear                   trul47ds
012200080417     C                   eval      d47opz  = 'I'
012300080417     C                   eval      d47tip  = 'VC'
012400080417     C                   eval      d47lck  = 'N'
012500121023     C*****  Gurrieri indica di modificare questo flag
012600121023     C*****              eval      d47chkj = 'N'
012700121023     C                   eval      d47chkj = 'S'
012800080930     C     WTPINV        IFEQ      'E'
012900121106     C                   eval      d47pgm  = 'FNLSA9§'
013000080930     c                   else
013100080417     C                   eval      d47pgm  = 'FNLSA9R'
013200080930     c                   end
013300080417     C                   call      'TRUL47R'
013400080417     C                   parm                    trul47ds
013500080417      *
013600080417      *  se strada bloccata si va a fine pgm
013700080417     C                   if        d47sts = 'A'
013800080417     c                   goto      Vai_a_fine
013900080417     c                   end
014000080417     C*---------------------------------------------------------------*
014100080417      *?  Definizione Campi e Caricamento Tabelle   ?
014200080417     C*---------------------------------------------------------------*
014300080417      *
014400060921     C     Klbl2         KLIST
014500060921     C                   KFLD                    klb2_AAS
014600060921     C                   KFLD                    klb2_LNP
014700060921     C                   KFLD                    klb2_NRS
014800060921     C                   KFLD                    klb2_NSP
014900060921     C     Klbl2p        KLIST
015000060921     C                   KFLD                    exLBLAAp
015100060921     C                   KFLD                    exLBLLPp
015200060921     C                   KFLD                    exLBLNRp
015300060921     C                   KFLD                    exLBLNSp
015400060921     C     KBLP          KLIST
015500060921     C                   KFLD                    BLPAAS
015600060921     C                   KFLD                    BLPLNP
015700060921     C                   KFLD                    BLPNRS
015800060921     C                   KFLD                    BLPNSP
015900060214     C     Kar4          KLIST
016000970122     C                   KFLD                    BLPAAS
016100970122     C                   KFLD                    BLPLNP
016200970122     C                   KFLD                    BLPNRS
016300970122     C                   KFLD                    BLPNSP
016400970122     C                   KFLD                    KTRC
016500921026     C     KBLP2         KLIST
016600921026     C                   KFLD                    COD99             7 0
016700921026     C                   KFLD                    KSC
016800920316     C     KTAB          KLIST
016900921026     C                   KFLD                    CODUT             1 0
017000921026     C                   KFLD                    COD               2
017100941228     C     KTAB2         KLIST
017200941228     C                   KFLD                    CODUT             1 0
017300941228     C                   KFLD                    COD               2
017400941228     C                   KFLD                    KEY               8
017500090529     C     Kar5          KLIST
017600090529     C                   KFLD                    BLPAAS
017700090529     C                   KFLD                    BLPLNP
017800090529     C                   KFLD                    BLPNRS
017900090529     C                   KFLD                    BLPNSP
018000090529     C                   KFLD                    Kar5vac           3
018100941228     C**
018200941228     C* DEFINIZIONE CAMPI
018300941228     C**
018400941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
018500060214     C     *LIKE         DEFINE    ar4TRC        KTRC
018600051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
018700051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
018800051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
018900000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
019000000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
019100000526     C     *LIKE         DEFINE    VACDAG        PTDTG
019200060921      *
019300060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
019400060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
019500060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
019600060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
019700941228     C*---------------------------------------------------------------*
019800921026     C* GIRO UDATE
019900941228     C                   TIME                    W0140            14 0
020000941228     C* UDATE IN GGMMAAAA
020100941228     C                   MOVE      W0140         WDTGIO            8 0
020200090529     C                   MOVEl     W0140         Orario            6 0
020300941228     C*
020400941228     C* UDATE IN AAAAMMGG
020500941228     C                   Z-ADD     WDTGIO        G02DAT
020600941228     C                   MOVEL     *BLANK        G02ERR
020700941228     C                   CALL      'XSRDA8'
020800941228     C                   PARM                    WLBDAT
020900941228     C                   MOVEL     G02INV        DATEU             8 0
021000941228     C*---------------------------------------------------------------*
021100000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
021200000525     C                   CLEAR                   FIP
021300000525     C                   Z-ADD     0             Y
021400000525     C     *LOVAL        SETLL     AZORG01L
021500000525     C                   DO        *HIVAL
021600000525     C                   READ      AZORG01L                               95
021700000525     C   95              LEAVE
021800000525     C                   MOVEL     ORGDE3        OG143
021900020729     C     §OgNTW        IFEQ      'PPT'
022000000525     C                   Z-ADD     1             Y
022100000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
022200000525     C   23              Z-ADD     ORGFIL        FIP(Y)
022300000525     C                   END
022400000525     C                   END
022500000525     C*---------------------------------------------------------------*
022600970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
022700970122     C                   Z-ADD     0             Y                 4 0
022800970122     C                   MOVEA     *HIVAL        T7K
022900970122     C                   MOVEL     '7K'          COD
023000970122     C     KTAB          CHAIN     TABEL                              31
023100970122     C     *IN31         DOWEQ     '0'
023200970122     C     TBLFLG        IFEQ      ' '
023300970122     C* Controllo se impostato tipo mancata consegna
023400970122     C                   MOVEL     TBLUNI        DS7K
023500970122     C     §7KTMC        IFNE      *BLANKS
023600970122     C                   ADD       1             Y
023700970122     C                   MOVEL     §7KTMC        T7K(Y)
023800970123     C                   MOVEL     TBLKEY        WTRC              1
023900970123     C                   MOVE      WTRC          T7K(Y)
024000970122     C                   END
024100970122     C                   END
024200970122     C     KTAB          READE     TABEL                                  31
024300970122     C                   END
024400970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
024500970122     C     Y             IFGT      0
024600970122     C                   SORTA     T7K
024700970122     C                   END
024800970122     C*---------------------------------------------------------------*
024900970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
025000970122     C                   Z-ADD     0             W                 4 0
025100970122     C                   MOVEA     *HIVAL        C2A
025200970305     C                   MOVEA     *HIVAL        D2A
025300970122     C                   MOVEA     *HIVAL        T2A
025400970122     C                   MOVEL     '2A'          COD
025500970122     C     KTAB          CHAIN     TABEL                              31
025600970122     C     *IN31         DOWEQ     '0'
025700970122     C     TBLFLG        IFEQ      ' '
025800970122     C* Caricamento evento + tipo mancata consegna
025900970122     C                   MOVEL     TBLUNI        DS2A
026000970122     C     §2AFTC        IFNE      *BLANKS
026100970122     C                   ADD       1             W
026200970122     C                   MOVEL     §2AFTC        T2A(W)
026300970305     C                   MOVEL     §2ADES        D2A(W)
026400970123     C                   MOVEL     TBLKEY        C2A(W)
026500970122     C                   END
026600970122     C                   END
026700970122     C     KTAB          READE     TABEL                                  31
026800970122     C                   END
026900080417     C*---------------------------------------------------------------*
027000080417      * ?  CICLO Principale del Programma              ?
027100970122     C*---------------------------------------------------------------*
027200080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
027300080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
027400921023     C                   MOVEL     '3K'          COD
027500170309     C***  KTAB          SETLL     TABEL
027600170309     c                   exsr      setll_sql
027700170309     C***  KTAB          READE     TABEL                                  31
027800170309     c                   exsr      fetch_sql
027900921023     C*
028000921023     C     *IN31         DOWEQ     '0'
028100131113      *
028200921023     C     TBLFLG        IFEQ      ' '
028300980407     C                   MOVEL     TBLUNI        DS3K
028400080417      * ?  Se Estero:  ?
028500980409     C     WTPINV        IFEQ      'E'
028600080417     C     §3KCSD        ANDEQ     'EW'
028700980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
028800080417      * ?  oppure se NON Estero e non EDI:  ?
028900980409     C     WTPINV        ORNE      'E'
029000080417     C     §3KCSD        ANDNE     'EW'
029100091116     C     §3KCSD        ANDNE     *blank
029200091116      *
029300921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
029400921026     C                   MOVEL     TBLKEY        KSC               7 0
029500930811     C                   MOVEL     KSC           COD99
029600930811     C                   MOVE      9999          COD99
029700011002     C                   MOVEL     KSC           KSCJ              7
029800930811     C*
029900941228     C     KSC           SETLL     FNBLP000                               32
030000941228     C  N32KBLP2         SETLL     FNBLP000                               32
030100060214     C  N32KSCJ          SETLL     FNBLP67J                               32
030200921023     C*
030300930811     C     *IN32         IFEQ      *ON
030400131113     C*                                                SALVO LA KEY
030500941228     C                   MOVEL     TBLKEY        SAVKEY
030600930811     C                   EXSR      ELAB
030700941228     C*
030800131113     C* RICHAINO IL RECORD DI TABEL
030900941228     C                   MOVEL     '3K'          COD
031000941228     C                   MOVEL     SAVKEY        KEY
031100941228     C     KTAB2         CHAIN     TABEL                              31
031200930811     C                   Z-ADD     DATEU         §3KCDU
031300930811     C                   MOVEL     DS3K          TBLUNI
031400930811     C                   UPDATE    TABEL
031500131113      *
031600921026     C                   END
031700131113      *
031800921023     C                   END
031900980409     C                   END
032000921023     C*
032100170309     C***  KTAB          READE     TABEL                                  31
032200170309     c                   exsr      fetch_sql
032300921023     C                   END
032400170309     c*
032500170309     c* chiudo il cursore
032600170309     c                   exsr      close_sql
032700981210     C*
032800981210     C* CALCELLO LE OVR ESEGUITE
032900981210     C                   MOVEL     *BLANKS       COMMAN           80
033000981210     C                   Z-ADD     17            LUNG
033100981210     C                   MOVEA     CMD(1)        COMMAN
033200981210     C                   CALL      'QCMDEXC'                            21
033300981210     C                   PARM                    COMMAN
033400080417     C                   PARM                    LUNG             15 5
033500080417      **
033600080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
033700980409     C   06WTPINV        IFEQ      'E'
033800110419     C                   CALL      'TRTCT80R'
033900030924     C                   parm                    Kpjba
034000980409     C                   END
034100080417     C*
034200080417      *? Controllato se non si è potuto procedere, si è arrivati qui.  ?
034300080417     c     Vai_a_fine    tag
034400080417     C*
034500080417      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
034600080417     C                   clear                   trul47ds
034700080417     C                   eval      d47opz  = 'F'
034800080417     C                   eval      d47tip  = 'VC'
034900080417     C                   call      'TRUL47R'
035000080417     C                   parm                    trul47ds
035100080417     C*
035200921023     C                   SETON                                        LR
035300080417      *================================================================
035400921023     C* ELABORO SPEDIZIONI DEL CLIENTE -------------------------------*
035500080417      *================================================================
035600921023     C     ELAB          BEGSR
035700040319     C*
035800941228     C     KSC           SETLL     FNBLP000
035900941228     C     KSC           READE     FNBLP000                               30
036000921026     C*
036100921026     C     *IN30         DOWEQ     '0'
036200921026     C                   EXSR      CONTR
036300941228     C     KSC           READE     FNBLP000                               30
036400921026     C                   END
036500921026     C*
036600921026     C* LEGGO I 9999 CODIFICATI
036700921026     C*
036800941228     C     KBLP2         SETLL     FNBLP000
036900941228     C     KBLP2         READE     FNBLP000                               30
037000941228     C*
037100921026     C     *IN30         DOWEQ     '0'
037200921026     C                   EXSR      CONTR
037300941228     C     KBLP2         READE     FNBLP000                               30
037400921026     C                   END
037500040319     C*
037600010927     C* INSERISCO LOOP SU FILE DI JOIN PER VEDERE SE ESISTONO RECORD
037700060214     C* SU ar4 CON CHIAVE KSC IN ar4NOT
037800010927      *
037900060214     C     KSCJ          SETLL     FNBLP67J
038000010927     C                   DO        *HIVAL
038100060214     C     KSCJ          READE     FNBLP67J                               66
038200010927     C   66              LEAVE
038300010927     C     KBLP          CHAIN     FNBLP01L                           67
038400010927     C     *IN67         IFEQ      '0'
038500010927     C                   SETON                                        68
038600010927     C                   EXSR      CONTR
038700010927     C                   SETOFF                                       68
038800010927     C                   ENDIF
038900010927     C                   ENDDO
039000040319     C*
039100010927     C                   SETOFF                                       68
039200040319     C*
039300921026     C                   ENDSR
039400921026     C*
039500921026     C* VEDO SE ELABORARE BOLLA --------------------------------------*
039600921026     C     CONTR         BEGSR
039700990602     C*
039800990602     C* Escludo bolle figlie xchè mando già via i dati tramite la mamma
039900131113     C     KBLP          CHAIN     FNLBL01L                           23
040000921026     C*
040100040319     C*  Se non è una bolla dirottata in quanto non è figlia di
040200040319     C*   un'altra bolla......
040300990602     C     *IN23         IFEQ      '1'
040400040319     C*   Oppure.....
040500040319     C*   .... se trovato il legame deve essere un dirottamento legato
040600040319     C*       non all'Estero e deve essere un discorso di reso/dirottam
040700040319     C*        Italia
040800040319     C     *in23         oreq      *off
040900040319     C     wtpINV        andne     'E'
041000040319     C     §3kcsd        andne     'EW'
041100040319     C     lblLAN        andne     lblLAP
041200040319     C     blpCCA        andne     *blank
041300040319     C*
041400050309     C     KBLP          CHAIN     tigcp000                           33
041500990602     C* 33 OFF - GIACENZA
041600961204     C*  Se giacenza bolla estero mando la data giacenza solo quando
041700961204     C*  l'ufficio internazionale mi ha scritto l'ulteriore motivaz.
041800961204     C*  in inglese
041900971126     C     *IN33         IFEQ      '0'
042000971126     C                   MOVEL     *BLANKS       ORGFL1
042100971126     C     GCPLNP        CHAIN     AZORG01L                           36
042200020717     C  n36              MOVEL     ORGDE3        OG143
042300020717     C  n36§OgNTW        ifeq      'FED'
042400020717     C     §OgNTW        oreq      'EEX'
042500020717     C     §OgNTW        oreq      'EUP'
042600020717     C     GCPFAS        Iflt      15
042700961204     C                   SETON                                        33
042800020717     C                   End
042900961204     C                   END
043000971126     C                   END
043100941228     C*
043200990602     C* Controllo se esiste una pratica di danno aperta
043300990602     C                   EXSR      CRTCA
043400941228     C*
043500060921      **?Attenzione:  a fronte di Bolla con Bolle figlie legate su cui
043600060921      *   è presente il Contrassegno, si vuole inviare al cliente
043700060921      *   l'info del contrassegno sul VAC della bolla madre.
043800060921      **?  Regola: il contrassegno si trova sull'ultima figlia della catena
043900060921      * ? o sulla penultima se l'ultima è una Locale.
044000060921      *    Occorre agganciare il file Contrassegni trovando prima la
044100060921      *    chiave della bolla su cui si dovrebbe trovare il contrassegno.
044200060921     c                   exsr      ChecK_figlie
044300060921      *   All'uscita della Routine è impostata la chiave della bolla valida
044400060921      *   x verificare la presenza del contrassegno sulla spedizione.
044500060921      *
044600060921     C     Klbl2         CHAIN     Fiar9000                           34
044700060921      *
044800941228     C     *IN34         IFEQ      *ON
044900051115     C                   CLEAR                   ar9CAS
045000051115     C                   CLEAR                   ar9VCA
045100051115     C                   CLEAR                   ar9TIC
045200941228     C                   ENDIF
045300941228     C*
045400000525      * Se la LNP della spedizione è una filiale POSTE richiamo
045500000525      * routine x reperire eventuale C/assegno e la data di giacenza
045600000525      * dalla data dell' evento
045700000526     C                   EXSR      POSTE
045800090408      *-------
045900090408      * ?  O x qualsiasi Variazione Bolla inerente alla Data Cons.Richiesta  ?
046000090408     c                   move      'N'           VariataOggi       1
046100090408     C     §3KCQ8        ifEQ      'S'
046200131014     c                   setoff                                       51
046300131014     C     KBLP          setll     FIARBF2C
046400131014     C     KBLP          reade     FIARBF2C
046500131014     C                   dow       not %Eof(FIARBF2C)
046600090408     c                   if        *in51
046700090408     c                   if        arbDTV = dateu
046800090408     c                   if        arbCVB = 'CR' or
046900090408     c                             arbCVB = 'CS'
047000090408     c                   move      'S'           VariataOggi
047100090408     c                   leave
047200090408     c                   end
047300090408     c                   end
047400090408     c                   end
047500131014     c                   setoff                                       51
047600131014     C     KBLP          reade     FIARBF2C
047700090408     C                   end
047800090408     C                   ENDIF
047900090408      *-------
048000941228     C* CERCO EVENTO LASCIATO AVVISO
048100941228     C                   CLEAR                   WDLA
048200941228     C                   MOVEL     '2A'          COD
048300941228     C*
048400131113     C     kBLP          SETGT     FNEVB000
048500131113     C     kBLP          READPE    FNEVB000                               34
048600941228     C*
048700941228     C     *IN34         DOWEQ     *OFF
048800131113     C*
048900131113      **  imposta sulla DS le note x testare il flag Progressivo
049000131113     c                   movel     evbNOT        Devb01
049100131113      *
049200131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
049300131113     c                   if        wTPinv <> 'E'
049400131113     c                               or
049500131113     c                             wTPinv = 'E' and
049600131113     c                             (§notPRG = ' ' or §notPRG = 'U')
049700131113      **
049800941228     C                   MOVEL(P)  EVBCEV        KEY
049900941228     C*
050000941228     C     KTAB2         CHAIN     TABEL                              30
050100941228     C     *IN30         IFEQ      *OFF
050200040319     C                   MOVEL     TBLUNI        DS2A
050300941228     C     §2AFTC        IFEQ      'A'
050400941228     C                   Z-ADD     EVBDEV        WDLA              8 0
050500941228     C                   SETON                                        34
050600941228     C                   ENDIF
050700941228     C                   ENDIF
050800941228     C*
050900131113     c                   endif
051000131113     C*
051100131113     C  N34kBLP          READPE    FNEVB000                               34
051200941228     C                   ENDDO
051300921026     C*
051400970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
051500970721     C                   MOVEL     'N'           WRTVAG            1
051600970121     C                   MOVEL     'S'           WFLDT3            1
051700970121     C                   MOVEL     'S'           WINVAC            1
051800040319      *
051900970121     C     §3KNMC        IFEQ      'S'
052000970121     C     BLPDCM        ANDEQ     0
052100011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
052200060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
052300011002     C     *IN68         ANDEQ     '0'
052400040319      *
052500040319      * ....ma anche
052600040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
052700040319      *
052800040319     C     §3kNMC        orEQ      'S'
052900040319     C     blpDCM        andGT     0
053000040319     C     blpCCA        andNE     *blank
053100040319     C     *in68         andEQ     *off
053200040319      *
053300970121     C                   EXSR      TSTNMC
053400970121     C                   END
053500001031     C*
053600001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
053700001031      *  PARZIALE E VERIFICA DELLE DATE
053800001106     C                   MOVEL     'N'           WTRPAR            1
053900131113      **
054000001031     C     §3KCQ6        IFEQ      'S'
054100001031     C     BLPDCM        ANDEQ     0
054200001031     C     BLPDCP        ANDGT     0
054300011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
054400060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
054500011002     C     *IN68         ANDEQ     '0'
054600001031     C                   EXSR      PARZIA
054700001031     C                   END
054800131113      **
054900980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
055000980318     C                   MOVEL     'N'           WTRMIC
055100060912     C                   MOVEL     'N'           WTRRIP
055200990602     C                   Z-ADD     0             WDTMIC            8 0
055300131113      **
055400131113      **  Non Consegnata e si deve inviare il MIC o la Data Consegna
055500131113      **      se si tratta dell'ULTIMO EVENTO registrato
055600980406     C     §3KCQ3        IFEQ      'S'
055700980318     C     BLPDCM        ANDEQ     0
055800060912     C     §3KCQ1        orEQ      'S'
055900060912     C     BLPDCM        ANDEQ     0
056000131113      **
056100131113     C     kBLP          SETGT     FNEVB000
056200131113     C     kBLP          READPE    FNEVB000                               34
056300131113      *  Cercando sempre l'ultimo evento NON deve fare distinzioni
056400131113      *   fra Italia o Estero
056500131113      *
056600980318     C     *IN34         IFEQ      *OFF
056700060912     C     EVBCEV        ifEQ      'MIC'
056800060912     C     §3KCQ3        andeq     'S'
056900980318     C                   MOVEL     'S'           WTRMIC            1
057000990602     C                   Z-ADD     EVBDEV        WDTMIC
057100060912     c                   else
057200060912      *ripristino
057300060912     C     EVBCEV        ifeq      'RIP'
057400060912     C     EVBCEV        oreQ      'RDC'
057500060912     C                   MOVEL     'S'           WTRRip            1
057600060912     C                   endif
057700060912      *
057800060912     C                   endif
057900980318     C                   END
058000131113      *
058100980318     C                   END
058200921026     C* DATA CONSEGNA
058300930809     C     §3KCQ1        IFEQ      'S'
058400921026     C     BLPDCM        ANDNE     0
058500080410      *
058600010202     C     §3KCQ1        OREQ      'S'
058700010202     C     §3KCSD        ANDEQ     'EW'
058800010202     C     BLPDCM        ANDEQ     0
058900010202     C     BLPDCP        ANDNE     0
059000080410      *
059100930809     C* DATA LASCIATO AVVISO
059200930809     C     §3KCQ2        OREQ      'S'
059300941228     C     WDLA          ANDGT     0
059400080410      *
059500930809     C* ANOMALIE
059600930809     C     §3KCQ4        OREQ      'S'
059700930809     C     BLPCCA        ANDNE     *BLANKS
059800080410      *
059900990602     C* ANOMALIE DANNI
060000990602     C     §3KCQ4        OREQ      'S'
060100990602     C     DATMAX        ANDNE     0
060200080410      *
060300930809     C* GIACENZA
060400930809     C     §3KCQ5        OREQ      'S'
060500930809     C     *IN33         ANDEQ     *OFF
060600080410      *
060700970123     C* MANCATE CONSEGNE
060800970123     C     §3KNMC        OREQ      'S'
060900970123     C     BLPDCM        ANDEQ     0
061000080410      *
061100980318     C* Messa in consegna
061200980318     C     §3KCQ3        OREQ      'S'
061300980318     C     WTRMIC        ANDEQ     'S'
061400080410      *
061500060912     C* Messa in consegna
061600060912     C     §3KCQ1        OREQ      'S'
061700060912     C     WTRrip        ANDEQ     'S'
061800060912     C     BLPDCM        ANDEQ     0
061900080410      *
062000090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
062100080410     C     §3KCQ8        OREQ      'S'
062200090408     c     VariataOggi   andEQ     'S'
062300090408     C     BLPDCR        ANDGT     0
062400080721     C*
062500080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
062600080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
062700080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
062800921026     C*
062900970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
063000970123     C* consegna o se li devo trasmettere e li ho trovati
063100970123     C     §3KNMC        IFNE      'S'
063200970123     C     WINVAC        OREQ      'S'
063300990624     C* ANOMALIE DANNI
063400990624     C     §3KCQ4        OREQ      'S'
063500990624     C     DATMAX        ANDNE     0
063600990624     C* Messa in consegna
063700990624     C     §3KCQ3        OREQ      'S'
063800990624     C     WTRMIC        ANDEQ     'S'
063900060912     C* Evento di ripristino
064000060912     C     §3KCQ1        OREQ      'S'
064100060912     C     WTRrip        ANDEQ     'S'
064200080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
064300080721     C     §3KCQ8        OREQ      'S'
064400080721     C     BLPDCR        ANDGT     0
064500090408     c     VariataOggi   andEQ     'S'
064600930809     C                   EXSR      CARBLR
064700970123     C                   END
064800080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
064900970123     C*
065000930809     C                   END
065100990602     C                   END
065200921026     C*
065300970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
065400970123     C* consegna o se li devo trasmettere e li ho trovati
065500970123     C     §3KNMC        IFNE      'S'
065600090527      *
065700970123     C     WFLDT3        OREQ      'S'
065800090527      *
065900090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
066000090527      *  fino ad un successivo Evento sulle bolle figlie)
066100090527     C     §3KCQ4        OREQ      'S'
066200090527     C     BLPCCA        ANDNE     *BLANKS
066300090527      *
066400990624     C* ANOMALIE DANNI
066500990624     C     §3KCQ4        OREQ      'S'
066600990624     C     DATMAX        ANDNE     0
066700990624     C* Messa in consegna
066800990624     C     §3KCQ3        OREQ      'S'
066900990624     C     WTRMIC        ANDEQ     'S'
067000001106     C* CONSEGNA PARZIALE
067100001106     C     §3KCQ6        OREQ      'S'
067200001106     C     WTRPAR        ANDEQ     'S'
067300080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
067400080729     C     §3KCQ8        OREQ      'S'
067500080729     C     BLPDCR        ANDGT     0
067600090408     c     VariataOggi   andEQ     'S'
067700080417      *
067800921026     C                   MOVEL     'T'           BLPFT3
067900941228     C                   MOVEL     DATEU         BLPDT3
068000080417      *
068100010927     C  N68              UPDATE    FNBLP000
068200010927     C   68              UPDATE    FNBLP1
068300970123     C                   END
068400921026     C*
068500921026     C                   ENDSR
068600000526     C*
068700170309       //--------------------------------------------------------------
068800170309       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3K
068900170309       //--------------------------------------------------------------
069000170309       begsr  setll_sql;
069100170309
069200170309         wSQL = 'select * +
069300170309                 from tabel00f +
069400170309                 where +
069500170309                 tblkut = 1 and +
069600170309                 tblcod = ''3K'' +
069700170309                 order by +
069800170309                 substr(TBLUNI , 27 , 7)';
069900170309
070000170309         // Dichiarazione cursore
070100170309         exec sql   prepare S1   from :wSQL;
070200170309         exec sql   declare C1  asensitive   cursor for S1;
070300170309
070400170309         // Apertura del cursore
070500170309         exec sql   open C1;
070600170309
070700170309         *in99 = *off;
070800170309         if SQLCode < 0;
070900170309           *in99 = *on;
071000170309         endif;
071100170309
071200170309       endsr;
071300170309       //--------------------------------------------------------------
071400170309       // fetch_sql  - legge il rcdset di TABEL00F
071500170309       //--------------------------------------------------------------
071600170309       begsr  fetch_sql;
071700170309
071800170309         exec sql  fetch next  from C1  into :TABELDS;
071900170309
072000170309         *in31 = *off;
072100170309         if SQLCode = 100 or SQLCode < 0;
072200170309           *in31 = *on;
072300170309         endif;
072400170309
072500170309       endsr;
072600170309       //--------------------------------------------------------------
072700170309       // close_sql  - chiusura cursore
072800170309       //--------------------------------------------------------------
072900170309       begsr  close_sql;
073000170309
073100170309         // chiusura del cursore
073200170309         exec sql  close C1;
073300170309
073400170309       endsr;
073500170309
073600060921      **?-------------------------------------------------------------*
073700060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
073800060921      **?-------------------------------------------------------------*
073900060921     C     ChecK_Figlie  BEGSR
074000060921      *
074100060921      *  inizializza flag di work della Routine
074200060921     c                   move      'S'           Primo_legame      1
074300060921      *
074400060921      **?chiave salvata della Bolla x partire nella ricerca
074500060921     c                   z-add     blpAAS        klb2_AAS
074600060921     c                   z-add     blpLNP        klb2_LNP
074700060921     c                   z-add     blpNRS        klb2_NRS
074800060921     c                   z-add     blpNSP        klb2_NSP
074900060921      *
075000060921      *  Chiave Bolla Precedente
075100060921     c     new_ciclo     tag
075200060921      **?                                  Flag di work del ciclo
075300060921     c                   move      'N'           ha_figlia         1
075400060921     C     Klbl2         setLL     FnLBL02L
075500060921     C     Klbl2         reade     FnLBL02L
075600060921      *
075700060921     c                   Dow       not %EoF(FnLBL02L)
075800060921      **?primo controllo:
075900060921      *    tutta la catena deve partire dalla bolla mamma che è
076000060921      *     uguale alla bolla originale del primo record della catena
076100060921     c                   if          Primo_legame = 'S'
076200060921     c                   eval      Primo_legame = 'N'
076300060921     c                   if          blpAAS <> exLBLAAo   or
076400060921     c                               blpLNP <> exLBLLPo   or
076500060921     c                               blpNRS <> exLBLNRo   or
076600060921     c                               blpNSP <> exLBLNSo
076700060921      *    se così non è non deve saltare il ciclo e non impostare i
076800060921      *     campi chiave x il contrassegno.
076900060921     c                   leave
077000060921     c                   end
077100060921     c                   end
077200060921      **?secondo controllo:
077300060921      *     non deve essere una bolla locale
077400060921      *   (sulle locali non c'è l'informazione di contrassegno)
077500060921     c                   if        exLBLLPN <> exLBLLPP
077600060921      *  imposta la chiave dell'ultima figlia
077700060921      *       per il prossimo giro di controllo
077800060921     c                   z-add     exLBLAAN      klb2_AAS
077900060921     c                   z-add     exLBLLPN      klb2_LNP
078000060921     c                   z-add     exLBLNRN      klb2_NRS
078100060921     c                   z-add     exLBLNSN      klb2_NSP
078200060921     c                   move      'S'           ha_figlia
078300060921     c                   end
078400060921      *
078500060921      **?   (attenzione a non sposizionarsi)
078600060921      * Chiave impostata come Bolla Precedente appena letta
078700060921     C     KLBL2P        reade     FnLBL02L
078800060921     c                   end
078900060921      *
079000060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
079100060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
079200060921      *   se invece non ha trovato delle figlie allora va a fine con la
079300060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
079400060921     c                   if        ha_figlia = 'S'
079500060921     c                   if        exLBLAAp <> klb2_AAS or
079600060921     c                             exLBLLPp <> klb2_LNP or
079700060921     c                             exLBLNRp <> klb2_NRS or
079800060921     c                             exLBLNSp <> klb2_NSP
079900060921     c                   goto      new_ciclo
080000060921     c                   end
080100060921     c                   end
080200060921      *
080300060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
080400060921      **?   x verificare la presenza del contrassegno su AR9
080500060921      *
080600060921     C                   ENDSR
080700060921      **?-------------------------------------------------------------*
080800000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
080900921026     C*
081000131119     C*   per quel che riguarda le POSTE
081100131119     C*    NON è stato adeguato alla logica degli eventi passati dal PDA
081200131119     C*     quindi con i flag del progressivo 'P' e 'U'
081300131119     C*
081400000526     C     POSTE         BEGSR
081500000526      *
081600000526     C                   CLEAR                   PTTIC
081700000526     C                   CLEAR                   PTCAS
081800000526     C                   CLEAR                   PTVCA
081900000526     C                   CLEAR                   PTIAS
082000000526     C                   CLEAR                   PTVAS
082100000526     C                   CLEAR                   PTDTG
082200000526     C                   CLEAR                   §POST
082300000526      *
082400000526     C                   SETOFF                                       22
082500000526     C     BLPLNP        LOOKUP    FIP                                    22
082600000526     C   22              MOVEL     'S'           §POST             1
082700131113      *
082800000526     C     §POST         IFEQ      'S'
082900131113      *
083000000526      * Se la bolla è stata chiusa con consegna anomala la merce
083100000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
083200000526      * questo è stato annullato in quanto non più incassabile da
083300000526      * Bartolini.
083400000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
083500000526      * informazione da ritornare alla SDA
083600000526     C     BLPCCA        IFNE      *BLANK
083700051115     C     ar9CAS        ANDEQ     *ZEROS
083800000526     C     KBLP          CHAIN     FNARBK00                           22
083900000526     C     *IN22         IFEQ      '0'
084000000526     C                   MOVEL     ARBTIC        PTTIC
084100000526     C                   Z-ADD     ARBCAS        PTCAS
084200000526     C                   MOVEL     ARBVCA        PTVCA
084300000526     C                   ENDIF
084400000526     C                   ENDIF
084500000526      *
084600060214      * Prendo importo da assicurare da Fiar4 t.r.=J
084700000526     C                   MOVEL     'J'           KTRC
084800060214     C     Kar4          CHAIN     Fiar401L                           35
084900000526     C     *IN35         IFEQ      '0'
085000060214     C                   MOVEL     ar4NOT        DSBL4J
085100000526     C                   Z-ADD     §B4IAS        PTIAS
085200000526     C                   MOVEL     §B4VAS        PTVAS
085300000526     C                   ENDIF
085400000526      *
085500000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
085600000526      * gestite le giacenze per le bolle poste
085700000526      *
085800131113     C     kBLP          SETGT     FNEVB000
085900131113     C     kBLP          READPE    FNEVB000                               34
086000000526      * Devo cercare l'ultimo evento di tipo giacenza
086100000526      *
086200000526     C     *IN34         DOWEQ     *OFF
086300000526     C                   Z-ADD     1             WW
086400000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
086500131119     c                   if         *in42 and T2A(WW) ='G'
086600000526     C                   Z-ADD     EVBDEV        PTDTG
086700000619     C                   LEAVE
086800131119     C                   ENDIF
086900131119      *
087000131113     C     kBLP          READPE    FNEVB000                               34
087100000526     C                   ENDDO
087200000526     C                   ENDIF
087300000526      *
087400000526     C                   ENDSR
087500000526     C*
087600941228     C* CREO RECORD FNVAC01L -----------------------------------------*
087700921026     C     CARBLR        BEGSR
087800080417      *
087900080417     c                   clear                   fnvacds
088000080417      *
088100941228     C                   MOVEL     BLPAAS        VACAAS
088200941228     C                   MOVEL     BLPLNP        VACLNP
088300941228     C                   MOVEL     BLPMGS        VACMGS
088400941228     C                   MOVEL     BLPNRS        VACNRS
088500941228     C                   MOVEL     BLPNSP        VACNSP
088600941228     C                   MOVEL     BLPCBO        VACCBO
088700941228     C                   MOVEL     BLPLNA        VACLNA
088800941228     C                   MOVEL     BLPRSD        VACRSD
088900941228     C                   MOVEL     BLPPRD        VACPRD
089000941228     C                   MOVEL     BLPGC1        VACGC1
089100941228     C                   MOVEL     BLPGC2        VACGC2
089200941228     C                   MOVEL     BLPCTR        VACCTR
089300941228     C                   MOVEL     BLPCTS        VACCTS
089400941228     C                   MOVEL     BLPFTM        VACFTM
089500941228     C                   MOVEL     BLPFIN        VACFIN
089600941228     C                   MOVEL     BLPFAP        VACFAP
089700941228     C                   MOVEL     BLPTSP        VACTSP
089800000526      *
089900000526     C     §POST         IFEQ      'S'
090000000526     C                   MOVEL     PTIAS         VACIAS
090100000526     C                   MOVEL     PTVAS         VACVAS
090200000526     C                   ELSE
090300000526     C                   MOVEL     BLPIAS        VACIAS
090400000526     C                   MOVEL     BLPVAS        VACVAS
090500000526     C                   ENDIF
090600000526      *
090700941228     C                   MOVEL     BLPNAS        VACNAS
090800941228     C                   MOVEL     BLPNCL        VACNCL
090900941228     C                   MOVEL     BLPPKF        VACPKB
091000941228     C                   MOVEL     BLPVLF        VACVLB
091100941228     C                   MOVEL     BLPQFT        VACQFT
091200000526      *
091300000526     C     §POST         IFEQ      'S'
091400051115     C     ar9CAS        ANDEQ     *ZEROS
091500000526     C                   MOVEL     PTCAS         VACCAS
091600000526     C                   MOVEL     PTVCA         VACVCA
091700000526     C                   MOVEL     PTTIC         VACTIC
091800000526     C                   ELSE
091900051115     C                   MOVEL     ar9CAS        VACCAS
092000051115     C                   MOVEL     ar9VCA        VACVCA
092100051115     C                   MOVEL     ar9TIC        VACTIC
092200000526     C                   ENDIF
092300000526      *
092400060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
092500011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
092600011002      *
092700011002     C     *IN68         IFEQ      '1'
092800060214     C                   MOVEL     ar4KSC        VACCCM
092900011002     C                   ELSE
093000921026     C     BLPCCM        IFNE      0
093100941228     C                   MOVEL     BLPCCM        VACCCM
093200921026     C                   ELSE
093300941228     C                   MOVEL     BLPKSC        VACCCM
093400921026     C                   END
093500011002     C                   END
093600131113      *
093700941228     C                   MOVEL     BLPRMN        VACRMN
093800941228     C                   MOVEL     BLPRMA        VACRMA
093900941228     C                   MOVEL     BLPRMO        VACRMO
094000941228     C                   MOVEL     BLPFFD        VACFFD
094100131113      *
094200000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
094300000921     C     §POST         IFEQ      'S'
094400000921     C                   Z-ADD     BLPNCD        VACDCR
094500000921     C                   ELSE
094600000921     C                   MOVEL     BLPDCR        VACDCR
094700000921     C                   ENDIF
094800941228     C                   MOVEL     BLPTCR        VACTCR
094900941228     C                   MOVEL     BLPHCR        VACHCR
095000941228     C                   MOVEL     BLPTC1        VACTC1
095100941228     C                   MOVEL     BLPTC2        VACTC2
095200941228     C                   MOVEL     BLPDCM        VACDCM
095300941228     C                   MOVEL     BLPHMC        VACHMC
095400941228     C                   MOVEL     BLPCCA        VACCCA
095500941228     C                   MOVEL     WDLA          VACDLA
095600000526      *
095700000526     C     §POST         IFEQ      'S'
095800000526     C                   MOVEL     PTDTG         VACDAG
095900000526     C                   ELSE
096000941228     C  N33              Z-ADD     GCPMGC        VACDAG
096100941228     C  N33              MOVEL     GCPAGC        VACDAG
096200941228     C   33              MOVEL     *ZEROS        VACDAG
096300000526     C                   ENDIF
096400000526      *
096500990602     C*---------------------------------------------------------------*
096600990602     C* Se ho trovato un evanto di messa in consegna....
096700060912 1   C     WTRMIC        IFEQ      'S'
096800990602     C* ... e non ho la data di consegna totale sulla bolla ..
096900990602     C     BLPDCM        ANDEQ     0
097000040319      *
097100990602     C* Verifico se ho trovato una pratica C.A. aperta ...
097200060912 2   C     DATMAX        IFNE      0
097300990602     C     §3KCQ4        ANDEQ     'S'
097400990602     C* .. e se l'apertura di quest'ultima è successiva alla
097500990602     C*   messa in consegna ...
097600990602     C     DATMAX        ANDGE     WDTMIC
097700990602     C* .. allora imposto la data apertura C.A. nella data consegna
097800990602     C*    e imposto il tipo consegna anomala = 'A'
097900990602     C                   Z-ADD     1200          VACHMC
098000990602     C                   Z-ADD     DATMAX        VACDCM
098100990602     C                   MOVEL     'A'           VACCCA
098200060912 x2  C                   ELSE
098300990602     C* ... altrimenti imposto data messa in consegna
098400990602     C                   Z-ADD     EVBHEV        VACHMC
098500980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
098600990602     C                   MOVEL     'C'           VACCCA
098700060912 e2  C                   END
098800990602     C*
098900990602     C* ... se è stata impostata la data di consegna sulla bolla ..
099000060912 x1  C                   ELSE
099100040319     C*
099200060912 2   C     BLPDCM        IFNE      0
099300040319     C*
099400040319     C* Un tempo era stato asteriscato questo pezzo
099500040319     C* >>>>>>>>>>>>>
099600040409      * non deve essere fatto per l'estero
099700060912 3   C     wtpINV        Ifne      'E'
099800040409      *
099900990602     C* ... verifico se ho trovato una C.A. aperta ...
100000060912 4   C     DATMAX        IFNE      0
100100040319     C     §3KCQ4        ANDEQ     'S'
100200990602     C* .. e se l'apertura di quest'ultima è successiva alla
100300990602     C* .. consegna ...
100400040319     C     DATMAX        ANDGT     BLPDCM
100500040319     C*
100600990602     C* .. allora imposto la data apertura C.A. nella data consegna
100700990602     C*    e imposto il tipo consegna anomala = 'A'
100800040319     C                   Z-ADD     1200          VACHMC
100900040319     C                   Z-ADD     DATMAX        VACDCM
101000040319     C                   MOVEL     'A'           VACCCA
101100040319     C*
101200060912 e4  C                   ENDif
101300060912 e3  c                   end
101400040319     C* >>>>>>>>>>>>>
101500040319     C*
101600990602     C* ... se non è stata effettuata la messa in consegna, la
101700990602     C*     bolla risulta non consegnata, ....
101800060912 x2  C                   ELSE
101900990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
102000990602     C*     le consegne anomale, imposto la data consegna + flag
102100990602     C*     consegna anomala='A'
102200060912 3   C     DATMAX        IFNE      0
102300990602     C     §3KCQ4        ANDEQ     'S'
102400990602     C                   Z-ADD     1200          VACHMC
102500990602     C                   Z-ADD     DATMAX        VACDCM
102600990602     C                   MOVEL     'A'           VACCCA
102700060912 e3  C                   END
102800060912 e2  C                   END
102900060912 e1  C                   END
103000060912     C* Se ho trovato un evento di ripristino
103100060912 1   C     WTRrip        IFEQ      'S'
103200060912     C     BLPDCM        ANDEQ     0
103300060912     C     §3KCQ1        ANDEQ     'S'
103400060912     C                   Z-ADD     0             VACHMC
103500060912     C                   Z-ADD     0             VACDCM
103600060912     C                   MOVEL     'R'           VACCCA
103700060912 e1  c                   endif
103800060912
103900080417     c                   exsr      Write_VAC
104000930603     C                   SETON                                        06
104100990604     C*
104200990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
104300990618     C* e questa è stata anche.....
104400060912 1   C     §3KCQ1        IFEQ      'S'
104500080417     C     §3KCSD        ANDEQ     'EW'
104600080417      *
104700060912 2   C     VACCCA        IFEQ      'A'
104800990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
104900990618     C* invio al partner dell'informazione della consegna parziale
1050000609123    C     BLPDCM        IFEQ      0
105100990618     C     BLPDCP        ANDNE     0
105200990618     C                   Z-ADD     0             VACHMC
105300990618     C                   Z-ADD     0             VACDCM
105400990618     C                   MOVEL     ' '           VACCCA
105500080417     c                   exsr      Write_VAC
105600060912x3   C                   ELSE
105700990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
105800990618     C* invio al partner dell'informazione
105900060912 4   C     BLPDCM        IFEQ      0
106000990624     C     WDTMIC        ANDGT     0
106100990618     C                   Z-ADD     EVBHEV        VACHMC
106200990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
106300990618     C                   MOVEL     'C'           VACCCA
106400080417     c                   exsr      Write_VAC
106500060912e4   C                   END
106600060912e3   C                   END
106700990618     C*
106800990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
106900990618     C* scrivo anche record x apertura C.A.
107000060912x2   C                   ELSE
1071000609123    C     DATMAX        IFNE      0
107200990618     C     §3KCQ4        ANDEQ     'S'
107300990618     C                   Z-ADD     1200          VACHMC
107400990618     C                   Z-ADD     DATMAX        VACDCM
107500990618     C                   MOVEL     'A'           VACCCA
107600080417     c                   exsr      Write_VAC
107700060912 e3  C                   END
107800060912 e2  C                   END
107900060912 e1  C                   END
108000990618     C*
108100921026     C                   ENDSR
108200990602     C*---------------------------------------------------------------*
108300990602     C* Controllo se esiste una pratica C.A. aperta                  -*
108400990602     C*---------------------------------------------------------------*
108500990602     C     CRTCA         BEGSR
108600990602     C*
108700990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
108800990602     C* per reperire la pratica di danno con data più alta
108900990602     C                   Z-ADD     0             DATMAX            8 0
109000990602     C     §3KCQ4        IFEQ      'S'
109100040715      **
109200040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
109300040715      * SIA COME MAMMA CHE COME FIGLIA
109400040715      **
109500040715     c                   clear                   fidn12ds
109600040715     c                   eval      i12aas = blpAAS
109700040715     c                   eval      i12lnp = blpLNP
109800040715     c                   eval      i12nrs = blpNRS
109900040715     c                   eval      i12nsp = blpNSP
110000040715     c                   eval      i12fel = 'E'
110100040715     c                   eval      i12fan = 'E'
110200040715     c                   eval      i12fch = 'E'
110300040715     c                   call      'FIDN12R'
110400040715     c                   parm                    fidn12ds
110500040715     c                   z-add     *zeros        ii                2 0
110600040715      **
110700040715      * prende fra le C.A. aperte quella con la data ultima
110800040715     c                   if        o12nca > 0
110900040715     c                   dow       ii <  o12nca
111000040715      **
111100040715     c                   add       1             ii
111200040715     C                   movel     skDca(ii)     datape            8 0
111300040715     C     DATMAX        IFLT      DATAPE
111400040715     C                   Z-ADD     DATAPE        DATMAX
111500040715     C                   END
111600040715      *
111700040715     c                   enddo
111800040715     c                   endif
111900040715      *
112000040715     C                   END
112100040715     C*
112200990602     C                   ENDSR
112300970122     C*---------------------------------------------------------------*
112400970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
112500970122     C*---------------------------------------------------------------*
112600970121     C     TSTNMC        BEGSR
112700040422     **
112800040422     ** avendo fatto entrare anche x data consegna GT di 0
112900040422     ** il flag deve essere resettato solo per la vecchia condizione
113000040422     ** altrimenti non scrive + il VAC come una volta.
113100040422     c                   if        blpdcm = 0
113200040422     **
113300970122     C                   MOVEL     'N'           WFLDT3
113400090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
113500090223     C**   sempre il VAC.
113600090223     C********           MOVEL     'N'           WINVAC
113700040422     **
113800040422     c                   end
113900040422     **
114000970122     C                   MOVEL     'N'           WTREVE            1
114100970122     C                   MOVEL     *BLANKS       WSVTMC            1
114200970122     C                   MOVEL     *BLANKS       WMOT1            35
114300970122     C                   MOVEL     *BLANKS       WMOT2            35
114400970122     C*  Leggo le estensioni bolla della spedizione e per quelle
114500970122     C*  relative ad un motivo di mancata consegna scrivo VAG
114600970122     C                   DO        Y             W
114700970122     C                   MOVEL     T7K(W)        WTMC              1
114800970122     C                   MOVE      T7K(W)        KTRC
114900060214     C     Kar4          CHAIN     Fiar401L                           35
115000970123     C     *IN35         IFEQ      '0'
115100060214     C     ar4FTR        ANDNE     'T'
115200060214     C     ar4FTR        ANDNE     'C'
115300970122     C*  Se ho trovato estensione bolla e non è trasmesso
115400970122     C*  controllo se ho un nuovo tp.mancata consegna
115500970122     C*  e se si reperisco evento corrispondente
115600970122     C     WTMC          IFNE      WSVTMC
115700970122     C                   EXSR      REPEVE
115800970122     C                   END
115900970122     C*  se ho trovato l'evento controllo se devo scrivere
116000970122     C*  nuovo record VAG:
116100970122     C     WTREVE        IFEQ      'S'
116200970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
116300970122     C*  - se ho già scritto 2 motivi --> new record
116400970122     C     WTMC          IFNE      WSVTMC
116500970122     C     WMOT2         ORNE      *BLANKS
116600970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
116700970124     C     WSVTMC        IFNE      *BLANKS
116800970124     C     WMOT2         ANDEQ     *BLANKS
116900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
117000970318     C*  primi 15 chr nella seconda motivazione
117100970318     C     WCHR15        IFNE      *BLANKS
117200970318     C                   MOVEL     WCHR15        VAGNOT
117300970318     C                   END
117400080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
117500050606     c                   move      'VG'          vgdtip
117600091116     c                   move      §3kgks        vgdksu
117700050627     c                   movel     0             vgdksu
117800091116     c                   move      §3kgsd        vgdtsc
117900050608     c                   eval      vgdpgm = 'FNLSA9R'
118000050627     c                   z-add     dateu         vgddat
118100091116     c                   if        §3kgsd  <> '  '
118200050606     C                   WRITE     tivgd000
118300091116     c                   end
118400970124     C                   SETON                                        06
118500970124     C                   END
118600970124     C*  New record
118700970122     C                   EXSR      NEWVAG
118800970122     C                   ELSE
118900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
119000970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
119100970318     C     WCHR15        IFNE      *BLANKS
119200970318     C                   MOVEL     WCHR15        VAGNOT
119300060214     C                   MOVE      ar4NOT        VAGNOT
119400970318     C                   ELSE
119500970318     C*  Se codice generico occupo chr. da 32 posizione
119600970318     C     VAGCMC        IFEQ      'G  '
119700970318     C     VAGCMC        OREQ      'GEN'
119800060214     C                   MOVEL     ar4NOT        WCHR19           19
119900060214     C                   MOVE      ar4NOT        WCHR16           16
120000970318     C                   MOVE      WCHR19        VAGDMC
120100970318     C                   MOVEL     WCHR16        VAGNOT
120200970318     C                   ELSE
120300970318     C*  Altrimenti imposto solo ulteriore descrizione
120400060214     C                   MOVEL     ar4NOT        VAGNOT
120500970318     C                   END
120600970318     C                   END
120700060214     C                   MOVEL     ar4NOT        WMOT2
120800970122     C                   MOVEL     *BLANKS       WMOT1
120900050606     C*  Scrivo tivgd
121000080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
121100050606     c                   move      'VG'          vgdtip
121200091116     c                   move      §3kgks        vgdksu
121300050627     c                   movel     0             vgdksu
121400091116     c                   move      §3kgsd        vgdtsc
121500050608     c                   eval      vgdpgm = 'FNLSA9R'
121600050627     c                   z-add     dateu         vgddat
121700091116     c                   if        §3kgsd  <> '  '
121800050606     C                   WRITE     tivgd000
121900091116     c                   end
122000970122     C                   SETON                                        06
122100970122     C                   END
122200970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
122300970122     C                   MOVEL     'S'           WFLDT3
122400970122     C                   MOVEL     'S'           WINVAC
122500060214     C*  Fleggo Fiar4
122600060214     C                   MOVEL     'C'           ar4FTR
122700060214     C                   Z-ADD     DATEU         ar4DTR
122800060214     C                   EXCEPT    AGGar4
122900970122     C                   END
123000970122     C                   END
123100970122     C*
123200970122     C                   END
123300970122     C*  Controllo se mi è rimasto un VAG da scrivere
123400970122     C     WMOT1         IFNE      *BLANKS
123500970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
123600970318     C*  primi 15 chr nella seconda motivazione
123700970318     C     WCHR15        IFNE      *BLANKS
123800970318     C                   MOVEL     WCHR15        VAGNOT
123900970318     C                   END
124000050606     C*  Scrivo tivgd
124100080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
124200050606     c                   move      'VG'          vgdtip
124300091116     c                   move      §3kgks        vgdksu
124400050627     c                   movel     0             vgdksu
124500091116     c                   move      §3kgsd        vgdtsc
124600050608     c                   eval      vgdpgm = 'FNLSA9R'
124700050627     c                   z-add     dateu         vgddat
124800091116     c                   if        §3kgsd  <> '  '
124900050606     C                   WRITE     tivgd000
125000091116     c                   end
125100970122     C                   SETON                                        06
125200970122     C                   END
125300970122     C*
125400970122     C                   ENDSR
125500970122     C*---------------------------------------------------------------*
125600970122     C* Controllo se esiste evento                                   -*
125700970122     C*---------------------------------------------------------------*
125800970122     C     REPEVE        BEGSR
125900970122     C*
126000970122     C                   MOVEL     'N'           WTREVE
126100970122     C*  Mi posiziono su file eventi
126200131113     C     kBLP          SETGT     FNEVB01L
126300131113     C     kBLP          READPE    FNEVB01L                               34
126400131113      *
126500970122     C*  Loop letture fino a fine file o travato evento
126600970122     C     *IN34         DOWEQ     '0'
126700970122     C     WTREVE        ANDEQ     'N'
126800040504      **
126900040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
127000040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
127100040504      **  le A16/A23 come varianti in programmi che precedono facendo
127200040504      **  sballare il ragionamento in questo pgm.
127300040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
127400040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
127500040504     c                   movel     '0'           evbCEV
127600040504     c                   end
127700131113      *
127800131113      **  imposta sulla DS le note x testare il flag Progressivo
127900131113     c                   movel     evbNOT        Devb01
128000131119      *
128100131119      ** Sia per l'Italia che per L'estero deve sempre prendere l'ultimo EVENTO
128200131119      **  in quanto MANCATA CONSEGNA perchè la bolla sarà aggiornata solo con
128300131119      **  l'evento "U" o BLK
128400131119      *
128500131119     c                   if         §notPRG = ' ' or §notPRG = 'U'
128600131119      *
128700970122     C*  Reperisco tipo mancata consegna evento letto
128800970122     C                   Z-ADD     1             WW                4 0
128900970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
129000131113      *
129100970122     C*  Controllo se tipo mancata consegna evento letto
129200970122     C*  è uguale a quella dell'estensione bolla
129300131113      *
129400131113      *  x l'Italia prende tutto invece x l'Estero solo l'ultimo o il blk
129500131119     c                   if         *in42 and T2A(WW) = WTMC
129600970122     C                   MOVEL     'S'           WTREVE
129700131119     c                   leave
129800131119     C                   END
129900131113      *
130000131119     C                   EndIF
130100131119      *
130200131113     C     kBLP          READPE    FNEVB01L                               34
130300970122     C                   END
130400970122     C*
130500970122     C                   ENDSR
130600970122     C*---------------------------------------------------------------*
130700970122     C* Nuovo record FNVAG                                           -*
130800970122     C*---------------------------------------------------------------*
130900970122     C     NEWVAG        BEGSR
131000970122     C*
131100050606     C                   CLEAR                   FNVAGds
131200970721     C                   MOVEL     'S'           WRTVAG            1
131300970318     C                   MOVEL     *BLANKS       WCHR25           25
131400970318     C                   MOVEL     *BLANKS       WCHR15           15
131500970122     C                   Z-ADD     BLPAAS        VAGAAS
131600970122     C                   Z-ADD     BLPLNP        VAGLNP
131700970122     C                   Z-ADD     BLPNRS        VAGNRS
131800970122     C                   Z-ADD     BLPNSP        VAGNSP
131900970122     C                   Z-ADD     BLPLNA        VAGLNA
132000990810     C                   MOVEL     EVBDEV        WAA2              4 0
132100970123     C                   MOVE      WAA2          VAGAGC
132200970122     C                   MOVE      EVBDEV        VAGMGC
132300060214     C                   MOVEL     ar4NOT        VAGCMC
132400060214     C                   MOVEL     ar4NOT        WMOT1
132500970122     C                   MOVEL     *BLANKS       WMOT2
132600970307     C                   Z-ADD     1             YY                4 0
132700970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
132800970307     C     VAGCMC        IFNE      'G  '
132900970307     C     VAGCMC        ANDNE     'GEN'
133000970307     C                   MOVEL     D2A(YY)       VAGDMC
133100970318     C                   MOVE      D2A(YY)       WCHR25
133200970318     C                   MOVEL     WCHR25        WCHR15
133300970305     C                   ELSE
133400060214     C                   MOVE      ar4NOT        WCOM31           31
133500970122     C                   MOVEL     WCOM31        VAGDMC
133600970305     C                   END
133700970122     C                   MOVEL     BLPRMA        VAGRMA
133800970122     C                   Z-ADD     BLPRMN        VAGRMN
133900970122     C     BLPCCM        IFNE      0
134000970122     C                   MOVEL     BLPCCM        VAGSCM
134100970122     C                   ELSE
134200970122     C                   MOVEL     BLPKSC        VAGSCM
134300970122     C                   END
134400970123     C*
134500970123     C                   MOVEL     WTMC          WSVTMC
134600970122     C*
134700970122     C                   ENDSR
134800001031     C*---------------------------------------------------------------*
134900001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
135000001031     C*---------------------------------------------------------------*
135100001031     C     PARZIA        BEGSR
135200001031     C*
135300001031      * LOOP SU DETTAGLIO COLLI
135400001103     C                   Z-ADD     0             TOTCO
135500001031     C                   Z-ADD     0             CONS
135600001102     C                   Z-ADD     0             ULTIMA            8 0
135700001031     C     KBLP          SETLL     FNBLT01L
135800001031     C                   DO        *HIVAL
135900001102     C     KBLP          READE     FNBLT01L                               97
136000001031     C   97              LEAVE
136100001031      * CONTA CONSEGNATI E NON CONSEGNATI
136200001103     C     BLTDCM        IFGT      0
136300001031     C                   ADD       1             CONS              3 0
136400001031     C                   END
136500001103     C                   ADD       1             TOTCO             3 0
136600001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
136700001031     C     BLTDCM        IFGT      ULTIMA
136800001031     C                   Z-ADD     BLTDCM        ULTIMA
136900001031     C                   ENDIF
137000001031     C                   ENDDO
137100001102     C                   MOVE      CONS          CONSA             3
137200001103     C                   MOVE      TOTCO         TOTCOA            3
137300001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
137400001031      *
137500050606     C                   CLEAR                   FNVAGds
137600001031     C                   MOVE      BLPLNP        VAGLNP
137700001031     C                   MOVE      BLPAAS        VAGAAS
137800001031     C                   MOVE      BLPNRS        VAGNRS
137900001031     C                   MOVE      BLPNSP        VAGNSP
138000001107     C                   MOVEL     'P  '         VAGCMC
138100001031     C                   MOVE      BLPRMN        VAGRMN
138200001031     C                   MOVE      BLPRMA        VAGRMA
138300001031     C                   MOVE      BLPLNA        VAGLNA
138400001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
138500001031     C                   MOVEL     COS(1)        VAGDMC
138600001103     C                   MOVEA     COS(2)        COM
138700001102     C                   MOVEA     CONSA         COM(22)
138800001103     C                   MOVEA     TOTCOA        COM(29)
138900001102     C                   MOVEA     COM           VAGNOT
139000001031      *
139100001031     C     BLPCCM        IFGT      0
139200001031     C                   Z-ADD     BLPCCM        VAGSCM
139300001031     C                   ELSE
139400001031     C                   Z-ADD     BLPKSC        VAGSCM
139500001031     C                   END
139600001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
139700001031     C                   MOVEL     ULTIMA        VAGAGC
139800001031     C                   MOVE      ULTIMA        VAGMGC
139900001031      *
140000080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
140100050606     c                   move      'VG'          vgdtip
140200091116     c                   move      §3kgks        vgdksu
140300050627     c                   movel     0             vgdksu
140400091116     c                   move      §3kgsd        vgdtsc
140500050608     c                   eval      vgdpgm = 'FNLSA9R'
140600050627     c                   z-add     dateu         vgddat
140700091116     c                   if        §3kgsd  <> '  '
140800050606     C                   WRITE     tivgd000
140900091116     c                   end
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
150100080417     C                   clear                   tivgd000
150200080417     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
150300080417     C                   eval      vgdTIP = 'VC'
150400080417     C                   movel     *all'0'       vgdKSU
150500080417     c                   move      §3kcks        vgdksu
150600080417      * tipo invio
150700080417     c                   move      §3kcsd        vgdtsc
150800080417     c                   eval      vgdpgm = 'FNLSA9R'
150900080417     c                   z-add     dateu         vgddat
151000080417      *
151100080417      *  x generare l'EDI:
151200080417      *    occorre farlo in modo controllato (personalizzato)
151300080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
151400080417     C                   clear                   DVGDFLO
151500080417     C                   movel     'P'           §VGDFELA
151600080417     C                   movel     DVGDFLO       vgdflo
151700080417     c                   end
151800080417      *
151900091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
152000091116      *  Non deve scrivere nulla.
152100091116     c                   if        §3kcsd <> *blank
152200080417     C                   write     tivgd000
152300110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
152400110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
152500110721     C                   MOVEL     'S'           WFLDT3
152600091116     c                   end
152700080417      *
152800080417     C                   ENDSR
152900080930      *------------------------------------------------------------------------*
153000080930      * Routine di controllo x eventuali Errori
153100080930      *------------------------------------------------------------------------*
153200080930     C     *PSSR         BEGSR
153300080930      *
153400080930      * Comunque eseguo x rilasciare il TIVGD.
153500080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
153600080930     C                   clear                   trul47ds
153700080930     C                   eval      d47opz  = 'F'
153800080930     C                   eval      d47tip  = 'VC'
153900080930     C                   call      'TRUL47R'
154000080930     C                   parm                    trul47ds
154100080930      *
154200080930     C                   ENDSR     '*CANCL'
154300080417     C*------------------------------------------------------------------------*
154400060214     OFiar4000  E            AGGar4
154500060214     O                       ar4FTR
154600060214     O                       ar4DTR
154700921026**
154800981210DLTOVR FILE(*ALL)
154900000726**
155000921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
155100921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
155200921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
155300001031**
155400001031Consegna parziale
155500001031Nr. colli consegnati xxx su xxx
