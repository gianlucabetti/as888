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
001400000526     FFIARBF1C  IF   E           K DISK
001500000526     F                                     IGNORE(FNARBD00)
001600000526     F                                     IGNORE(FIARBT00)
001700090408     F*******                              IGNORE(FNARBG00)
001800000526     F                                     IGNORE(FNARBP00)
001900040715     F***FNDCT02L  IF   E           K DISK
002000990602     FFNLBL01L  IF   E           K DISK
002100060921     Ffnlbl02L  IF   E           K DISK    rename(fnlbl000:fnlbl2)
002200060921     f                                     prefix(ex)
002300080417     F***FNVAC01T  UF A E           K DISK    USROPN
002400971126     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002500001102     FFNBLT01L  IF   E           K DISK
002600060214     FFNBLP67J  IF   E           K DISK
002700010927     F                                     RENAME(FNBLP000:FNBLPJ)
002800010927     FFNBLP01L  UF   E           K DISK
002900010927     F                                     RENAME(FNBLP000:FNBLP1)
003000050606     ftivgd00f  o    e             disk
003100971126     D*--------------------------------------------------------------*
003200970122     D CM1             S              1    DIM(47) CTDATA PERRCD(47)
003300970122     D CM2             S              1    DIM(48) CTDATA PERRCD(48)
003400970122     D CM3             S              1    DIM(48) CTDATA PERRCD(48)
003500981210     D CMD             S             49    DIM(1) CTDATA PERRCD(1)
003600921026     D MSG             S             67    DIM(3) CTDATA PERRCD(1)
003700001031     D COS             S             50    DIM(2) CTDATA PERRCD(1)
003800001102     D COM             S              1    DIM(31)
003900921026     D SK              S              1    DIM(201)
004000970122     D* Schiera per caricamento tipi record estensione bolla
004100970122     D T7K             S              2    DIM(50)
004200970122     D* Schiera per caricamento codici e tipi mancate consegne
004300970122     D C2A             S              3    DIM(200)
004400970122     D T2A             S              1    DIM(200)
004500970305     D D2A             S             75    DIM(200)
004600000525     D FIP             S              3  0 DIM(50)
004700971126      *----------------------------------------------------------------
004800080417      * Controllo e semaforo x scrittura del VAC: TRUL47R
004900080417      *----------------------------------------------------------------
005000080417     D trul47ds      e ds
005100080417      *
005200080417     D DVGDFLO       e ds
005300080417     D FNVACDS       e ds
005400090529     D FNVAChgDS     e ds
005500090603     D  DSVAChg        ds
005600090603     d    VacGC1chg                        like(VacGC1)
005700090603     d    VacGC2chg                        like(VacGC2)
005800090603     d    VacFFDchg                        like(VacFFD)
005900090603     d    VacTCRchg                        like(VacTCR)
006000090603     d    VacDCRchg                        like(VacDCR)
006100090603     d    VacHCRchg                        like(VacHCR)
006200090603     d    VacDCMchg                        like(VacDCM)
006300090603     d    VacHMCchg                        like(VacHMC)
006400090603     d    VacTC1chg                        like(VacTC1)
006500090603     d    VacTC2chg                        like(VacTC2)
006600090603     d    VacCCAchg                        like(VacCCA)
006700090603     d    VacDLAchg                        like(VacDLA)
006800090603     d    VacDAGchg                        like(VacDAG)
006900080417      *
007000941228     D***
007100941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
007200941228     D***
007300941228     D WLBDAT          DS                  INZ
007400941228     D  G02DAT                 1      8  0
007500941228     D  G02INV                 9     16  0
007600941228     D  G02ERR                17     17
007700941228     D  G02TGI                18     22  0
007800900517     D KPJBA         E DS
007900921023     D DS3K          E DS
008000970122     D DS7K          E DS
008100941228     D DS2A          E DS
008200000525     D OG143         E DS
008300000526     D DSBL4J        E DS
008400050606     D fnvagds       E DS
008500040715
008600040715     d Fidn12ds      e ds
008700040715     d  skKey                 26    305    dim(20)
008800040715     d  skAnn                306    325    dim(20)
008900040715     d  skDca                326    485    dim(20)
009000040715     d  skFca                486    545  0 dim(20)
009100040715     d  skDch                546    705  0 dim(20)
009200040715     d  skCch                706    745    dim(20)
009300040715
009400971126      *----------------------------------------------------------------
009500971126     IAZORG01L  AA
009600971126     I                             P    4    5 0ORGFIL
009700971126     I                                 11   11  ORGFL1
009800000525     I                               3776 3800  ORGDE3
009900090408     IFNARBG00      51
010000090408     IFNARBK00      52
010100920325     C*---------------------------------------------------------------*
010200920812     C* INDICATORI
010300920812     C*---------------------------------------------------------------*
010400921026     C* 01    - NON TUTTI I MEMBRI SONO STATI ALLOCATI
010500921026     C* 30    - LETTURA FLBLP00F
010600921026     C* 31    - LETTURA TABEL00F
010700921026     C* 32    - VEDO SE ESISTE GIA' BOLLA SU FLBLR
010800921026     C* 33    - GIACENZA
010900921026     C* 34    - COSEGNA PARTICOLARE NUMERICA (COD ANOMALIA)
011000970122     C* 35    - TROVATE ESTENSIONI BOLLE X MANCATE CONSEGNE
011100970123     C* 40    - ERRORE IN QCMDEXC SU FNVAC
011200970123     C* 41    - ERRORE IN QCMDEXC SU FNVAG
011300970123     C* 99    - ESEGUO CHIUSURA E DISALLOCAZIONE MEMBRO FNVAC
011400920812     C*---------------------------------------------------------------*
011500000000     C     *ENTRY        PLIST
011600000000     C                   PARM                    KPJBA
011700980407     C                   MOVEL     KPJBU         WTPINV            2
011800921026     C                   Z-ADD     1             CODUT
011900900511     C*---------------------------------------------------------------*
012000080417      *?Prima di iniziare, controlla se è possibile Procedere mediante  ?
012100080417      *? un controllo semaforico fatto con il TRUL47R.                  ?
012200080417      *  Se la strada è libera si può procedere altrimenti si va a fine.
012300080417     C*---------------------------------------------------------------*
012400080417      *? Come prima cosa avvio il blocco elaborazione TIVGD x tipo file ?
012500080417     C*---------------------------------------------------------------*
012600080417     C                   clear                   trul47ds
012700080417     C                   eval      d47opz  = 'I'
012800080417     C                   eval      d47tip  = 'VC'
012900080417     C                   eval      d47lck  = 'N'
013000121023     C*****  Gurrieri indica di modificare questo flag
013100121023     C*****              eval      d47chkj = 'N'
013200121023     C                   eval      d47chkj = 'S'
013300080930     C     WTPINV        IFEQ      'E'
013400121106     C                   eval      d47pgm  = 'FNLSA9§'
013500080930     c                   else
013600080417     C                   eval      d47pgm  = 'FNLSA9R'
013700080930     c                   end
013800080417     C                   call      'TRUL47R'
013900080417     C                   parm                    trul47ds
014000080417      *
014100080417      *  se strada bloccata si va a fine pgm
014200080417     C                   if        d47sts = 'A'
014300080417     c                   goto      Vai_a_fine
014400080417     c                   end
014500080417     C*---------------------------------------------------------------*
014600080417      *?  Definizione Campi e Caricamento Tabelle   ?
014700080417     C*---------------------------------------------------------------*
014800080417      *
014900060921     C     Klbl2         KLIST
015000060921     C                   KFLD                    klb2_AAS
015100060921     C                   KFLD                    klb2_LNP
015200060921     C                   KFLD                    klb2_NRS
015300060921     C                   KFLD                    klb2_NSP
015400060921     C     Klbl2p        KLIST
015500060921     C                   KFLD                    exLBLAAp
015600060921     C                   KFLD                    exLBLLPp
015700060921     C                   KFLD                    exLBLNRp
015800060921     C                   KFLD                    exLBLNSp
015900060921     C     KBLP          KLIST
016000060921     C                   KFLD                    BLPAAS
016100060921     C                   KFLD                    BLPLNP
016200060921     C                   KFLD                    BLPNRS
016300060921     C                   KFLD                    BLPNSP
016400040715     C***  KDCT          KLIST
016500040715     C***                KFLD                    BLPAAS
016600040715     C***                KFLD                    BLPLNP
016700040715     C***                KFLD                    BLPNRS
016800040715     C***                KFLD                    BLPNSP
016900990602     C     KLBL          KLIST
017000990602     C                   KFLD                    BLPAAS
017100990602     C                   KFLD                    BLPLNP
017200990602     C                   KFLD                    BLPNRS
017300990602     C                   KFLD                    BLPNSP
017400060214     C     Kar4          KLIST
017500970122     C                   KFLD                    BLPAAS
017600970122     C                   KFLD                    BLPLNP
017700970122     C                   KFLD                    BLPNRS
017800970122     C                   KFLD                    BLPNSP
017900970122     C                   KFLD                    KTRC
018000941228     C     KEVB          KLIST
018100990810     C                   KFLD                    WAAS              4 0
018200941228     C                   KFLD                    BLPLNP
018300941228     C                   KFLD                    BLPNRS
018400941228     C                   KFLD                    BLPNSP
018500921026     C     KBLP2         KLIST
018600921026     C                   KFLD                    COD99             7 0
018700921026     C                   KFLD                    KSC
018800920316     C     KTAB          KLIST
018900921026     C                   KFLD                    CODUT             1 0
019000921026     C                   KFLD                    COD               2
019100941228     C     KTAB2         KLIST
019200941228     C                   KFLD                    CODUT             1 0
019300941228     C                   KFLD                    COD               2
019400941228     C                   KFLD                    KEY               8
019500090529     C     Kar5          KLIST
019600090529     C                   KFLD                    BLPAAS
019700090529     C                   KFLD                    BLPLNP
019800090529     C                   KFLD                    BLPNRS
019900090529     C                   KFLD                    BLPNSP
020000090529     C                   KFLD                    Kar5vac           3
020100941228     C**
020200941228     C* DEFINIZIONE CAMPI
020300941228     C**
020400941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
020500060214     C     *LIKE         DEFINE    ar4TRC        KTRC
020600051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
020700051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
020800051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
020900000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
021000000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
021100000526     C     *LIKE         DEFINE    VACDAG        PTDTG
021200060921      *
021300060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
021400060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
021500060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
021600060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
021700941228     C*---------------------------------------------------------------*
021800921026     C* GIRO UDATE
021900941228     C                   TIME                    W0140            14 0
022000941228     C* UDATE IN GGMMAAAA
022100941228     C                   MOVE      W0140         WDTGIO            8 0
022200090529     C                   MOVEl     W0140         Orario            6 0
022300941228     C*
022400941228     C* UDATE IN AAAAMMGG
022500941228     C                   Z-ADD     WDTGIO        G02DAT
022600941228     C                   MOVEL     *BLANK        G02ERR
022700941228     C                   CALL      'XSRDA8'
022800941228     C                   PARM                    WLBDAT
022900941228     C                   MOVEL     G02INV        DATEU             8 0
023000941228     C*---------------------------------------------------------------*
023100000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
023200000525     C                   CLEAR                   FIP
023300000525     C                   Z-ADD     0             Y
023400000525     C     *LOVAL        SETLL     AZORG01L
023500000525     C                   DO        *HIVAL
023600000525     C                   READ      AZORG01L                               95
023700000525     C   95              LEAVE
023800000525     C                   MOVEL     ORGDE3        OG143
023900020729     C     §OgNTW        IFEQ      'PPT'
024000000525     C                   Z-ADD     1             Y
024100000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
024200000525     C   23              Z-ADD     ORGFIL        FIP(Y)
024300000525     C                   END
024400000525     C                   END
024500000525     C*---------------------------------------------------------------*
024600970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
024700970122     C                   Z-ADD     0             Y                 4 0
024800970122     C                   MOVEA     *HIVAL        T7K
024900970122     C                   MOVEL     '7K'          COD
025000970122     C     KTAB          CHAIN     TABEL                              31
025100970122     C     *IN31         DOWEQ     '0'
025200970122     C     TBLFLG        IFEQ      ' '
025300970122     C* Controllo se impostato tipo mancata consegna
025400970122     C                   MOVEL     TBLUNI        DS7K
025500970122     C     §7KTMC        IFNE      *BLANKS
025600970122     C                   ADD       1             Y
025700970122     C                   MOVEL     §7KTMC        T7K(Y)
025800970123     C                   MOVEL     TBLKEY        WTRC              1
025900970123     C                   MOVE      WTRC          T7K(Y)
026000970122     C                   END
026100970122     C                   END
026200970122     C     KTAB          READE     TABEL                                  31
026300970122     C                   END
026400970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
026500970122     C     Y             IFGT      0
026600970122     C                   SORTA     T7K
026700970122     C                   END
026800970122     C*---------------------------------------------------------------*
026900970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
027000970122     C                   Z-ADD     0             W                 4 0
027100970122     C                   MOVEA     *HIVAL        C2A
027200970305     C                   MOVEA     *HIVAL        D2A
027300970122     C                   MOVEA     *HIVAL        T2A
027400970122     C                   MOVEL     '2A'          COD
027500970122     C     KTAB          CHAIN     TABEL                              31
027600970122     C     *IN31         DOWEQ     '0'
027700970122     C     TBLFLG        IFEQ      ' '
027800970122     C* Caricamento evento + tipo mancata consegna
027900970122     C                   MOVEL     TBLUNI        DS2A
028000970122     C     §2AFTC        IFNE      *BLANKS
028100970122     C                   ADD       1             W
028200970122     C                   MOVEL     §2AFTC        T2A(W)
028300970305     C                   MOVEL     §2ADES        D2A(W)
028400970123     C                   MOVEL     TBLKEY        C2A(W)
028500970122     C                   END
028600970122     C                   END
028700970122     C     KTAB          READE     TABEL                                  31
028800970122     C                   END
028900080417     C*---------------------------------------------------------------*
029000080417      * ?  CICLO Principale del Programma              ?
029100970122     C*---------------------------------------------------------------*
029200080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
029300080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
029400921023     C                   MOVEL     '3K'          COD
029500921023     C     KTAB          SETLL     TABEL
029600921023     C     KTAB          READE     TABEL                                  31
029700921023     C*
029800921023     C     *IN31         DOWEQ     '0'
029900921023     C     TBLFLG        IFEQ      ' '
030000980407     C                   MOVEL     TBLUNI        DS3K
030100080417      *
030200080417      * ?  Se Estero:  ?
030300980409     C     WTPINV        IFEQ      'E'
030400080417     C     §3KCSD        ANDEQ     'EW'
030500080417      *
030600080417      ********
030700080417     C* SE RICEVO TIPO INVIO IMPOSTATO A 'ES' DEVO SPEDIRE SOLO
030800080417     C* LE DATE DI CONSEGNA PER L'EDI ITALIA/ESTERO + EUROPOLITAN
030900080417     C**** WTPINV        OREQ      'E'
031000080417     C**** §3KCSD        ANDEQ     'ED'
031100080417     C**** WTPINV        OREQ      'E'
031200080417     C**** §3KCSD        ANDEQ     'ES'
031300080417     C**** WTPINV        OREQ      'E'
031400080417     C**** §3KCSD        ANDEQ     'EU'
031500080417     C**** WTPINV        OREQ      'E'
031600080417     C**** §3KCSD        ANDEQ     'EB'
031700080417      *
031800980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
031900080417      * ?  oppure se NON Estero e non EDI:  ?
032000980409     C     WTPINV        ORNE      'E'
032100080417     C     §3KCSD        ANDNE     'EW'
032200091116     C     §3KCSD        ANDNE     *blank
032300091116      *
032400080417     C**** §3KCSD        ANDNE     'ES'
032500080417     C**** §3KCSD        ANDNE     'ED'
032600080417     C**** §3KCSD        ANDNE     'EU'
032700080417     C**** §3KCSD        ANDNE     'EB'
032800921026     C*
032900921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
033000921026     C                   MOVEL     TBLKEY        KSC               7 0
033100930811     C                   MOVEL     KSC           COD99
033200930811     C                   MOVE      9999          COD99
033300011002     C                   MOVEL     KSC           KSCJ              7
033400930811     C*
033500941228     C     KSC           SETLL     FNBLP000                               32
033600941228     C  N32KBLP2         SETLL     FNBLP000                               32
033700060214     C  N32KSCJ          SETLL     FNBLP67J                               32
033800921023     C*
033900930811     C     *IN32         IFEQ      *ON
034000080417     C*
034100080417     c                   setoff                                       40
034200080417      *?Con il TIVGD Il VAC multimembro non esiste +  ?
034300080417     C* CONTROLLO ESISTENZA MEMBRO DEL CLIENTE
034400080417     C*********          EXSR      CTRMBR
034500921023     C*
034600080417      *?Con *in40 = Off in OR --> questa IF è come se non ci fosse.
034700080417      *?  quindi entra sempre ed elabora il cliente.
034800921023     C* ELABORO SPEDIZIONI DEL CLIENTE IN SCARICO
034900930811     C     *IN40         IFEQ      *OFF
035000970123     C     §3KNMC        ANDNE     'S'
035100970122     C     *IN40         OREQ      *OFF
035200001103     C     *IN40         OREQ      *OFF
035300001103     C     §3KCQ6        ANDNE     'S'
035400080417      *
035500941228     C* SALVO LA KEY
035600941228     C                   MOVEL     TBLKEY        SAVKEY
035700930811     C                   EXSR      ELAB
035800941228     C*
035900941228     C* RICHAINO IL RECORD DI TABELAA
036000941228     C                   MOVEL     '3K'          COD
036100941228     C                   MOVEL     SAVKEY        KEY
036200941228     C     KTAB2         CHAIN     TABEL                              31
036300930811     C                   Z-ADD     DATEU         §3KCDU
036400930811     C                   MOVEL     DS3K          TBLUNI
036500930811     C                   UPDATE    TABEL
036600921026     C                   END
036700930811     C                   END
036800921023     C                   END
036900980409     C                   END
037000921023     C*
037100921023     C     KTAB          READE     TABEL                                  31
037200921023     C                   END
037300921026     C*
037400080417      *?Accendeva lo (01) durante il controllo/allocazione del
037500080417      *? vecchio VAC multimembro se qualcosa era andata storta
037600080417      *? su un cliente.
037700080417      * ?  quindi questo messaggio non serve +  ?
037800921026     C* 01 ON - NON TUTTI I CLIENTI SONO STATI ELABORATI: RILANCIARE
037900080417     C** 01 *****        DO
038000080417     C***********        MOVEA     MSG(1)        SK(1)
038100080417     C***********        MOVEA     MSG(2)        SK(68)
038200080417     C***********        MOVEA     MSG(3)        SK(135)
038300080417     C***********        MOVEA     SK            COMM2           201
038400080417     C***********        Z-ADD     201           LUNG             15 5
038500080417     C***********
038600080417     C***********        CALL      'QCMDEXC'
038700080417     C***********        PARM                    COMM2
038800080417     C***********        PARM                    LUNG
038900080417     C***********        END
039000930601     C* PREPARAZIONE DATI
039100080417     C*********          CLOSE     FNVAC01T
039200981210     C*
039300981210     C* CALCELLO LE OVR ESEGUITE
039400981210     C                   MOVEL     *BLANKS       COMMAN           80
039500981210     C                   Z-ADD     17            LUNG
039600981210     C                   MOVEA     CMD(1)        COMMAN
039700981210     C                   CALL      'QCMDEXC'                            21
039800981210     C                   PARM                    COMMAN
039900080417     C                   PARM                    LUNG             15 5
040000080417      **
040100080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
040200980409     C   06WTPINV        IFEQ      'E'
040300080417      *
040400080730     C**********         CALL      'TRTC51R'
040500110419     C**********         CALL      'TRTC50R'
040600110419     C                   CALL      'TRTCT80R'
040700030924     C                   parm                    Kpjba
040800080417      *
040900980409     C                   ELSE
041000080417      *
041100080417      *?Scompare il vecchio discorso x Halley (non esiste +)  ?
041200000616      * SE IL SIF È DI PROVA  RICHIAMO PGM AFFINCHÈ VENGA
041300000616      * CREATA DTAARA IN QTEMP CHE PERMETTE DI SIMULARE DI
041400000616      * ESSERE IN FILIALE
041500080417     C**********         MOVEL     KNSIF         SIFPRO            7
041600080417     C**** SIFPRO        IFEQ      'GAITRAP'
041700080417     C**** SIFPRO        OREQ      'FILTRAP'
041800080417     C**********         CALL      'TRTC00CP'
041900080417     C**********         ENDIF
042000080417     C**********
042100080417     C**********         CALL      'TRTC00C3'
042200080417      *
042300980409     C                   END
042400080417     C*
042500080417      *? Controllato se non si è potuto procedere, si è arrivati qui.  ?
042600080417     c     Vai_a_fine    tag
042700080417     C*
042800080417      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
042900080417     C                   clear                   trul47ds
043000080417     C                   eval      d47opz  = 'F'
043100080417     C                   eval      d47tip  = 'VC'
043200080417     C                   call      'TRUL47R'
043300080417     C                   parm                    trul47ds
043400080417     C*
043500921023     C                   SETON                                        LR
043600080417      *================================================================
043700921026     C* CONTROLLO ESISTENZA MEMBRO E APERTURA ------------------------*
043800080417      *? Questa Routine è da eliminare poichè ora l'FNVAC00T in Filiale?
043900080417      *?  è stato sostituito con il TIVGD come Standard di Download.
044000080417      *================================================================
044100921023     C     CTRMBR        BEGSR
044200080417      *
044300921026     C   40              SETOFF                                       9940
044400921026     C*
044500000619      * SE IL SIF È DI PROVA  FORZO  GAITRAPRD
044600080417     C                   MOVEL     KNSIF         SIFPRO            7
044700000619     C                   MOVE      KNSIF         PROVA             4
044800000619     C     PROVA         IFEQ      'PFP '
044900000619     C     PROVA         OREQ      'PFM '
045000000619     C                   MOVEL     SIFPRO        LIB
045100000619     C                   MOVE      'RD '         LIB
045200000619     C                   ENDIF
045300921026     C   99              DO
045400080417      *
045500080417     C********           CLOSE     FNVAC01T
045600921026     C* DISALLOCO
045700921026     C                   MOVEA     LOGMBR        CM3(37)
045800921026     C                   MOVEL     *BLANKS       COMMAN
045900921026     C                   MOVEA     CM3           COMMAN           80
046000921028     C                   Z-ADD     48            LUNG             15 5
046100921026     C*
046200921026     C                   CALL      'QCMDEXC'                            40
046300921026     C                   PARM                    COMMAN
046400921026     C                   PARM                    LUNG
046500921026     C                   END
046600921026     C* CHKOBJ E ADDLFM
046700941228     C                   MOVEL     'M'           ALF8              8
046800941228     C                   MOVE      §3KCKS        ALF8
046900941228     C*
047000941228     C                   MOVEL     'FNVAC00T'    FISICO           10
047100921026     C                   MOVEL     ALF8          FISMBR           10
047200941228     C                   MOVEL     'FNVAC01T'    LOGICO           10
047300921026     C                   MOVEL     ALF8          LOGMBR           10
047400000619     C     PROVA         IFNE      'PFP '
047500000619     C     PROVA         ANDNE     'PFM '
047600921026     C                   MOVEL     KNSIF         LIB              10
047700000619     C                   END
047800921026     C                   MOVEL     ' '           FLG               1
047900921026     C*
048000921026     C                   CALL      'TRUL50C'
048100921026     C                   PARM                    FISICO
048200921026     C                   PARM                    FISMBR
048300921026     C                   PARM                    LIB
048400921026     C                   PARM                    LOGICO
048500921026     C                   PARM                    LOGMBR
048600921026     C                   PARM                    FLG
048700921026     C* ALLOCO
048800921026     C                   MOVEA     LOGMBR        CM2(37)
048900921026     C                   MOVEL     *BLANKS       COMMAN
049000921026     C                   MOVEA     CM2           COMMAN           80
049100921028     C                   Z-ADD     48            LUNG             15 5
049200921026     C*
049300921026     C                   CALL      'QCMDEXC'                            40
049400921026     C                   PARM                    COMMAN
049500921026     C                   PARM                    LUNG
049600921026     C*
049700921026     C* 40 OFF - ALLOCATO
049800921026     C  N40              DO
049900921026     C* OVRDBF
050000921026     C                   MOVEA     LOGMBR        CM1(37)
050100921026     C                   MOVEL     *BLANKS       COMMAN
050200921026     C                   MOVEA     CM1           COMMAN           80
050300921028     C                   Z-ADD     47            LUNG             15 5
050400921026     C                   CALL      'QCMDEXC'                            40
050500921026     C                   PARM                    COMMAN
050600921026     C                   PARM                    LUNG
050700921026     C*
050800080417     C**N40*****         OPEN      FNVAC01T
050900921026     C  N40              SETON                                        99
051000921026     C                   END
051100941228     C*
051200921026     C   40              SETON                                        01
051300080417     C*
051400921023     C                   ENDSR
051500080417      *================================================================
051600921023     C* ELABORO SPEDIZIONI DEL CLIENTE -------------------------------*
051700080417      *================================================================
051800921023     C     ELAB          BEGSR
051900040319     C*
052000941228     C     KSC           SETLL     FNBLP000
052100941228     C     KSC           READE     FNBLP000                               30
052200921026     C*
052300921026     C     *IN30         DOWEQ     '0'
052400921026     C                   EXSR      CONTR
052500941228     C     KSC           READE     FNBLP000                               30
052600921026     C                   END
052700921026     C*
052800921026     C* LEGGO I 9999 CODIFICATI
052900921026     C*
053000941228     C     KBLP2         SETLL     FNBLP000
053100941228     C     KBLP2         READE     FNBLP000                               30
053200941228     C*
053300921026     C     *IN30         DOWEQ     '0'
053400921026     C                   EXSR      CONTR
053500941228     C     KBLP2         READE     FNBLP000                               30
053600921026     C                   END
053700040319     C*
053800010927     C* INSERISCO LOOP SU FILE DI JOIN PER VEDERE SE ESISTONO RECORD
053900060214     C* SU ar4 CON CHIAVE KSC IN ar4NOT
054000010927      *
054100060214     C     KSCJ          SETLL     FNBLP67J
054200010927     C                   DO        *HIVAL
054300060214     C     KSCJ          READE     FNBLP67J                               66
054400010927     C   66              LEAVE
054500010927     C     KBLP          CHAIN     FNBLP01L                           67
054600010927     C     *IN67         IFEQ      '0'
054700010927     C                   SETON                                        68
054800010927     C                   EXSR      CONTR
054900010927     C                   SETOFF                                       68
055000010927     C                   ENDIF
055100010927     C                   ENDDO
055200040319     C*
055300010927     C                   SETOFF                                       68
055400040319     C*
055500921026     C                   ENDSR
055600921026     C*
055700921026     C* VEDO SE ELABORARE BOLLA --------------------------------------*
055800921026     C     CONTR         BEGSR
055900990602     C*
056000990602     C* Escludo bolle figlie xchè mando già via i dati tramite la mamma
056100990602     C     KLBL          CHAIN     FNLBL01L                           23
056200921026     C*
056300040319     C*  Se non è una bolla dirottata in quanto non è figlia di
056400040319     C*   un'altra bolla......
056500990602     C     *IN23         IFEQ      '1'
056600040319     C*   Oppure.....
056700040319     C*   .... se trovato il legame deve essere un dirottamento legato
056800040319     C*       non all'Estero e deve essere un discorso di reso/dirottam
056900040319     C*        Italia
057000040319     C     *in23         oreq      *off
057100040319     C     wtpINV        andne     'E'
057200040319     C     §3kcsd        andne     'EW'
057300040319     C     lblLAN        andne     lblLAP
057400040319     C     blpCCA        andne     *blank
057500040319     C*
057600050309     C     KBLP          CHAIN     tigcp000                           33
057700990602     C* 33 OFF - GIACENZA
057800961204     C*  Se giacenza bolla estero mando la data giacenza solo quando
057900961204     C*  l'ufficio internazionale mi ha scritto l'ulteriore motivaz.
058000961204     C*  in inglese
058100971126     C     *IN33         IFEQ      '0'
058200971126     C                   MOVEL     *BLANKS       ORGFL1
058300971126     C     GCPLNP        CHAIN     AZORG01L                           36
058400020717     C  n36              MOVEL     ORGDE3        OG143
058500020717     C  n36§OgNTW        ifeq      'FED'
058600020717     C     §OgNTW        oreq      'EEX'
058700020717     C     §OgNTW        oreq      'EUP'
058800020717     C     GCPFAS        Iflt      15
058900961204     C                   SETON                                        33
059000020717     C                   End
059100961204     C                   END
059200971126     C                   END
059300941228     C*
059400990602     C* Controllo se esiste una pratica di danno aperta
059500990602     C                   EXSR      CRTCA
059600941228     C*
059700060921      **?Attenzione:  a fronte di Bolla con Bolle figlie legate su cui
059800060921      *   è presente il Contrassegno, si vuole inviare al cliente
059900060921      *   l'info del contrassegno sul VAC della bolla madre.
060000060921      **?  Regola: il contrassegno si trova sull'ultima figlia della catena
060100060921      * ? o sulla penultima se l'ultima è una Locale.
060200060921      *    Occorre agganciare il file Contrassegni trovando prima la
060300060921      *    chiave della bolla su cui si dovrebbe trovare il contrassegno.
060400060921     c                   exsr      ChecK_figlie
060500060921      *   All'uscita della Routine è impostata la chiave della bolla valida
060600060921      *   x verificare la presenza del contrassegno sulla spedizione.
060700060921      *
060800060921     C**** KBLP          CHAIN     Fiar9000                           34
060900060921     C     Klbl2         CHAIN     Fiar9000                           34
061000060921      *
061100941228     C     *IN34         IFEQ      *ON
061200051115     C                   CLEAR                   ar9CAS
061300051115     C                   CLEAR                   ar9VCA
061400051115     C                   CLEAR                   ar9TIC
061500941228     C                   ENDIF
061600941228     C*
061700000525      * Se la LNP della spedizione è una filiale POSTE richiamo
061800000525      * routine x reperire eventuale C/assegno e la data di giacenza
061900000525      * dalla data dell' evento
062000000526     C                   EXSR      POSTE
062100090408      *-------
062200090408      * ?  O x qualsiasi Variazione Bolla inerente alla Data Cons.Richiesta  ?
062300090408     c                   move      'N'           VariataOggi       1
062400090408     C     §3KCQ8        ifEQ      'S'
062500090408     C     KBLP          setll     FIARBF1C
062600090408     C     KBLP          reade     FIARBF1C
062700090408     C                   dow       not %Eof(FIARBF1C)
062800090408     c                   if        *in51
062900090408     c                   if        arbDTV = dateu
063000090408     c                   if        arbCVB = 'CR' or
063100090408     c                             arbCVB = 'CS'
063200090408     c                   move      'S'           VariataOggi
063300090408     c                   leave
063400090408     c                   end
063500090408     c                   end
063600090408     c                   end
063700090408     C     KBLP          reade     FIARBF1C
063800090408     C                   end
063900090408     C                   ENDIF
064000090408      *-------
064100000525      *
064200941228     C* CERCO EVENTO LASCIATO AVVISO
064300941228     C                   CLEAR                   WDLA
064400941228     C                   MOVEL     '2A'          COD
064500941228     C*
064600941228     C                   MOVE      BLPAAS        WAAS
064700990810     C     KEVB          SETGT     FNEVB000
064800990810     C     KEVB          READPE    FNEVB000                               34
064900941228     C*
065000941228     C     *IN34         DOWEQ     *OFF
065100941228     C                   MOVEL(P)  EVBCEV        KEY
065200941228     C*
065300941228     C     KTAB2         CHAIN     TABEL                              30
065400941228     C     *IN30         IFEQ      *OFF
065500040319     C                   MOVEL     TBLUNI        DS2A
065600941228     C     §2AFTC        IFEQ      'A'
065700941228     C                   Z-ADD     EVBDEV        WDLA              8 0
065800941228     C                   SETON                                        34
065900941228     C                   ENDIF
066000941228     C                   ENDIF
066100941228     C*
066200990810     C  N34KEVB          READPE    FNEVB000                               34
066300941228     C                   ENDDO
066400921026     C*
066500970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
066600970721     C                   MOVEL     'N'           WRTVAG            1
066700970121     C                   MOVEL     'S'           WFLDT3            1
066800970121     C                   MOVEL     'S'           WINVAC            1
066900040319      *
067000970121     C     §3KNMC        IFEQ      'S'
067100970121     C     BLPDCM        ANDEQ     0
067200011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
067300060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
067400011002     C     *IN68         ANDEQ     '0'
067500040319      *
067600040319      * ....ma anche
067700040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
067800040319      *
067900040319     C     §3kNMC        orEQ      'S'
068000040319     C     blpDCM        andGT     0
068100040319     C     blpCCA        andNE     *blank
068200040319     C     *in68         andEQ     *off
068300040319      *
068400970121     C                   EXSR      TSTNMC
068500970121     C                   END
068600001031     C*
068700001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
068800001031      *  PARZIALE E VERIFICA DELLE DATE
068900001106     C                   MOVEL     'N'           WTRPAR            1
069000001031     C     §3KCQ6        IFEQ      'S'
069100001031     C     BLPDCM        ANDEQ     0
069200001031     C     BLPDCP        ANDGT     0
069300011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
069400060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
069500011002     C     *IN68         ANDEQ     '0'
069600001031     C                   EXSR      PARZIA
069700001031     C                   END
069800980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
069900980318     C                   MOVEL     'N'           WTRMIC
070000060912     C                   MOVEL     'N'           WTRRIP
070100990602     C                   Z-ADD     0             WDTMIC            8 0
070200980406     C     §3KCQ3        IFEQ      'S'
070300980318     C     BLPDCM        ANDEQ     0
070400060912     C     §3KCQ1        orEQ      'S'
070500060912     C     BLPDCM        ANDEQ     0
070600980318     C                   MOVE      BLPAAS        WAAS
070700990810     C     KEVB          SETGT     FNEVB000
070800990810     C     KEVB          READPE    FNEVB000                               34
070900980318     C*
071000980318     C     *IN34         IFEQ      *OFF
071100060912     C     EVBCEV        ifEQ      'MIC'
071200060912     C     §3KCQ3        andeq     'S'
071300980318     C                   MOVEL     'S'           WTRMIC            1
071400990602     C                   Z-ADD     EVBDEV        WDTMIC
071500060912     c                   else
071600060912      *ripristino
071700060912     C     EVBCEV        ifeq      'RIP'
071800060912     C     EVBCEV        oreQ      'RDC'
071900060912     C                   MOVEL     'S'           WTRRip            1
072000060912     C                   endif
072100060912      *
072200060912     C                   endif
072300980318     C                   END
072400980318     C                   END
072500921026     C* DATA CONSEGNA
072600930809     C     §3KCQ1        IFEQ      'S'
072700921026     C     BLPDCM        ANDNE     0
072800080410      *
072900960729     C* CONSEGNA PARZIALE X EDI ESTERO
073000080417     C**** §3KCQ1        OREQ      'S'
073100080417     C**** §3KCSD        ANDEQ     'ES'
073200080417     C**** BLPDCM        ANDEQ     0
073300080417     C**** BLPDCP        ANDNE     0
073400080410      *
073500010202     C     §3KCQ1        OREQ      'S'
073600010202     C     §3KCSD        ANDEQ     'EW'
073700010202     C     BLPDCM        ANDEQ     0
073800010202     C     BLPDCP        ANDNE     0
073900080410      *
074000990604     C* CONSEGNA PARZIALE X EUROPOLITAN
074100080417     C**** §3KCQ1        OREQ      'S'
074200080417     C**** §3KCSD        ANDEQ     'EU'
074300080417     C**** BLPDCM        ANDEQ     0
074400080417     C**** BLPDCP        ANDNE     0
074500080410      *
074600930809     C* DATA LASCIATO AVVISO
074700930809     C     §3KCQ2        OREQ      'S'
074800941228     C     WDLA          ANDGT     0
074900080410      *
075000930809     C* ANOMALIE
075100930809     C     §3KCQ4        OREQ      'S'
075200930809     C     BLPCCA        ANDNE     *BLANKS
075300080410      *
075400990602     C* ANOMALIE DANNI
075500990602     C     §3KCQ4        OREQ      'S'
075600990602     C     DATMAX        ANDNE     0
075700080410      *
075800930809     C* GIACENZA
075900930809     C     §3KCQ5        OREQ      'S'
076000930809     C     *IN33         ANDEQ     *OFF
076100080410      *
076200970123     C* MANCATE CONSEGNE
076300970123     C     §3KNMC        OREQ      'S'
076400970123     C     BLPDCM        ANDEQ     0
076500080410      *
076600980318     C* Messa in consegna
076700980318     C     §3KCQ3        OREQ      'S'
076800980318     C     WTRMIC        ANDEQ     'S'
076900080410      *
077000060912     C* Messa in consegna
077100060912     C     §3KCQ1        OREQ      'S'
077200060912     C     WTRrip        ANDEQ     'S'
077300060912     C     BLPDCM        ANDEQ     0
077400080410      *
077500090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
077600080410     C     §3KCQ8        OREQ      'S'
077700090408     c     VariataOggi   andEQ     'S'
077800090408     C     BLPDCR        ANDGT     0
077900080721     C*
078000080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
078100080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
078200080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
078300921026     C*
078400970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
078500970123     C* consegna o se li devo trasmettere e li ho trovati
078600970123     C     §3KNMC        IFNE      'S'
078700970123     C     WINVAC        OREQ      'S'
078800990624     C* ANOMALIE DANNI
078900990624     C     §3KCQ4        OREQ      'S'
079000990624     C     DATMAX        ANDNE     0
079100990624     C* Messa in consegna
079200990624     C     §3KCQ3        OREQ      'S'
079300990624     C     WTRMIC        ANDEQ     'S'
079400060912     C* Evento di ripristino
079500060912     C     §3KCQ1        OREQ      'S'
079600060912     C     WTRrip        ANDEQ     'S'
079700080721      *
079800080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
079900080721     C     §3KCQ8        OREQ      'S'
080000080721     C     BLPDCR        ANDGT     0
080100090408     c     VariataOggi   andEQ     'S'
080200080721      *
080300930809     C                   EXSR      CARBLR
080400970123     C                   END
080500080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
080600970123     C*
080700930809     C                   END
080800990602     C*
080900990602     C                   END
081000921026     C*
081100970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
081200970123     C* consegna o se li devo trasmettere e li ho trovati
081300970123     C     §3KNMC        IFNE      'S'
081400090527      *
081500970123     C     WFLDT3        OREQ      'S'
081600090527      *
081700090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
081800090527      *  fino ad un successivo Evento sulle bolle figlie)
081900090527     C     §3KCQ4        OREQ      'S'
082000090527     C     BLPCCA        ANDNE     *BLANKS
082100090527      *
082200990624     C* ANOMALIE DANNI
082300990624     C     §3KCQ4        OREQ      'S'
082400990624     C     DATMAX        ANDNE     0
082500990624     C* Messa in consegna
082600990624     C     §3KCQ3        OREQ      'S'
082700990624     C     WTRMIC        ANDEQ     'S'
082800001106     C* CONSEGNA PARZIALE
082900001106     C     §3KCQ6        OREQ      'S'
083000001106     C     WTRPAR        ANDEQ     'S'
083100080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
083200080729     C     §3KCQ8        OREQ      'S'
083300080729     C     BLPDCR        ANDGT     0
083400090408     c     VariataOggi   andEQ     'S'
083500080417      *
083600921026     C                   MOVEL     'T'           BLPFT3
083700941228     C                   MOVEL     DATEU         BLPDT3
083800080417      *
083900010927     C  N68              UPDATE    FNBLP000
084000010927     C   68              UPDATE    FNBLP1
084100970123     C                   END
084200921026     C*
084300921026     C                   ENDSR
084400000526     C*
084500060921      **?-------------------------------------------------------------*
084600060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
084700060921      **?-------------------------------------------------------------*
084800060921     C     ChecK_Figlie  BEGSR
084900060921      *
085000060921      *  inizializza flag di work della Routine
085100060921     c                   move      'S'           Primo_legame      1
085200060921      *
085300060921      **?chiave salvata della Bolla x partire nella ricerca
085400060921     c                   z-add     blpAAS        klb2_AAS
085500060921     c                   z-add     blpLNP        klb2_LNP
085600060921     c                   z-add     blpNRS        klb2_NRS
085700060921     c                   z-add     blpNSP        klb2_NSP
085800060921      *
085900060921      *  Chiave Bolla Precedente
086000060921     c     new_ciclo     tag
086100060921      **?                                  Flag di work del ciclo
086200060921     c                   move      'N'           ha_figlia         1
086300060921     C     Klbl2         setLL     FnLBL02L
086400060921     C     Klbl2         reade     FnLBL02L
086500060921      *
086600060921     c                   Dow       not %EoF(FnLBL02L)
086700060921      **?primo controllo:
086800060921      *    tutta la catena deve partire dalla bolla mamma che è
086900060921      *     uguale alla bolla originale del primo record della catena
087000060921     c                   if          Primo_legame = 'S'
087100060921     c                   eval      Primo_legame = 'N'
087200060921     c                   if          blpAAS <> exLBLAAo   or
087300060921     c                               blpLNP <> exLBLLPo   or
087400060921     c                               blpNRS <> exLBLNRo   or
087500060921     c                               blpNSP <> exLBLNSo
087600060921      *    se così non è non deve saltare il ciclo e non impostare i
087700060921      *     campi chiave x il contrassegno.
087800060921     c                   leave
087900060921     c                   end
088000060921     c                   end
088100060921      **?secondo controllo:
088200060921      *     non deve essere una bolla locale
088300060921      *   (sulle locali non c'è l'informazione di contrassegno)
088400060921     c                   if        exLBLLPN <> exLBLLPP
088500060921      *  imposta la chiave dell'ultima figlia
088600060921      *       per il prossimo giro di controllo
088700060921     c                   z-add     exLBLAAN      klb2_AAS
088800060921     c                   z-add     exLBLLPN      klb2_LNP
088900060921     c                   z-add     exLBLNRN      klb2_NRS
089000060921     c                   z-add     exLBLNSN      klb2_NSP
089100060921     c                   move      'S'           ha_figlia
089200060921     c                   end
089300060921      *
089400060921      **?   (attenzione a non sposizionarsi)
089500060921      * Chiave impostata come Bolla Precedente appena letta
089600060921     C     KLBL2P        reade     FnLBL02L
089700060921     c                   end
089800060921      *
089900060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
090000060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
090100060921      *   se invece non ha trovato delle figlie allora va a fine con la
090200060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
090300060921     c                   if        ha_figlia = 'S'
090400060921     c                   if        exLBLAAp <> klb2_AAS or
090500060921     c                             exLBLLPp <> klb2_LNP or
090600060921     c                             exLBLNRp <> klb2_NRS or
090700060921     c                             exLBLNSp <> klb2_NSP
090800060921     c                   goto      new_ciclo
090900060921     c                   end
091000060921     c                   end
091100060921      *
091200060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
091300060921      **?   x verificare la presenza del contrassegno su AR9
091400060921      *
091500060921     C                   ENDSR
091600060921      **?-------------------------------------------------------------*
091700000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
091800921026     C*
091900000526     C     POSTE         BEGSR
092000000526      *
092100000526     C                   CLEAR                   PTTIC
092200000526     C                   CLEAR                   PTCAS
092300000526     C                   CLEAR                   PTVCA
092400000526     C                   CLEAR                   PTIAS
092500000526     C                   CLEAR                   PTVAS
092600000526     C                   CLEAR                   PTDTG
092700000526     C                   CLEAR                   §POST
092800000526      *
092900000526     C                   SETOFF                                       22
093000000526     C     BLPLNP        LOOKUP    FIP                                    22
093100000526     C   22              MOVEL     'S'           §POST             1
093200000526     C     §POST         IFEQ      'S'
093300000526      * Se la bolla è stata chiusa con consegna anomala la merce
093400000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
093500000526      * questo è stato annullato in quanto non più incassabile da
093600000526      * Bartolini.
093700000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
093800000526      * informazione da ritornare alla SDA
093900000526     C     BLPCCA        IFNE      *BLANK
094000051115     C     ar9CAS        ANDEQ     *ZEROS
094100000526     C     KBLP          CHAIN     FNARBK00                           22
094200000526     C     *IN22         IFEQ      '0'
094300000526     C                   MOVEL     ARBTIC        PTTIC
094400000526     C                   Z-ADD     ARBCAS        PTCAS
094500000526     C                   MOVEL     ARBVCA        PTVCA
094600000526     C                   ENDIF
094700000526     C                   ENDIF
094800000526      *
094900060214      * Prendo importo da assicurare da Fiar4 t.r.=J
095000000526      *
095100000526     C                   MOVEL     'J'           KTRC
095200060214     C     Kar4          CHAIN     Fiar401L                           35
095300000526     C     *IN35         IFEQ      '0'
095400060214     C                   MOVEL     ar4NOT        DSBL4J
095500000526     C                   Z-ADD     §B4IAS        PTIAS
095600000526     C                   MOVEL     §B4VAS        PTVAS
095700000526     C                   ENDIF
095800000526      *
095900000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
096000000526      * gestite le giacenze per le bolle poste
096100000526      *
096200000526     C     KEVB          SETGT     FNEVB000
096300000526     C     KEVB          READPE    FNEVB000                               34
096400000526      * Devo cercare l'ultimo evento di tipo giacenza
096500000526      *
096600000526     C     *IN34         DOWEQ     *OFF
096700000526     C                   Z-ADD     1             WW
096800000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
096900000526     C     *IN42         IFEQ      '1'
097000000526     C     T2A(WW)       ANDEQ     'G'
097100000526     C                   Z-ADD     EVBDEV        PTDTG
097200000619     C                   LEAVE
097300000526     C                   ELSE
097400000526     C     KEVB          READPE    FNEVB000                               34
097500000526     C                   ENDIF
097600000526      *
097700000526     C                   ENDDO
097800000526      *
097900000526     C                   ENDIF
098000000526      *
098100000526     C                   ENDSR
098200000526     C*
098300941228     C* CREO RECORD FNVAC01L -----------------------------------------*
098400921026     C     CARBLR        BEGSR
098500080417      *
098600080417     C***  KBLP          CHAIN     FNVAC01T                           32
098700080417      *
098800080417     c                   clear                   fnvacds
098900080417      *
099000941228     C                   MOVEL     BLPAAS        VACAAS
099100941228     C                   MOVEL     BLPLNP        VACLNP
099200941228     C                   MOVEL     BLPMGS        VACMGS
099300941228     C                   MOVEL     BLPNRS        VACNRS
099400941228     C                   MOVEL     BLPNSP        VACNSP
099500941228     C                   MOVEL     BLPCBO        VACCBO
099600941228     C                   MOVEL     BLPLNA        VACLNA
099700941228     C                   MOVEL     BLPRSD        VACRSD
099800941228     C                   MOVEL     BLPPRD        VACPRD
099900941228     C                   MOVEL     BLPGC1        VACGC1
100000941228     C                   MOVEL     BLPGC2        VACGC2
100100941228     C                   MOVEL     BLPCTR        VACCTR
100200941228     C                   MOVEL     BLPCTS        VACCTS
100300941228     C                   MOVEL     BLPFTM        VACFTM
100400941228     C                   MOVEL     BLPFIN        VACFIN
100500941228     C                   MOVEL     BLPFAP        VACFAP
100600941228     C                   MOVEL     BLPTSP        VACTSP
100700000526      *
100800000526     C     §POST         IFEQ      'S'
100900000526     C                   MOVEL     PTIAS         VACIAS
101000000526     C                   MOVEL     PTVAS         VACVAS
101100000526     C                   ELSE
101200000526     C                   MOVEL     BLPIAS        VACIAS
101300000526     C                   MOVEL     BLPVAS        VACVAS
101400000526     C                   ENDIF
101500000526      *
101600941228     C                   MOVEL     BLPNAS        VACNAS
101700941228     C                   MOVEL     BLPNCL        VACNCL
101800941228     C                   MOVEL     BLPPKF        VACPKB
101900941228     C                   MOVEL     BLPVLF        VACVLB
102000941228     C                   MOVEL     BLPQFT        VACQFT
102100000526      *
102200000526     C     §POST         IFEQ      'S'
102300051115     C     ar9CAS        ANDEQ     *ZEROS
102400000526     C                   MOVEL     PTCAS         VACCAS
102500000526     C                   MOVEL     PTVCA         VACVCA
102600000526     C                   MOVEL     PTTIC         VACTIC
102700000526     C                   ELSE
102800051115     C                   MOVEL     ar9CAS        VACCAS
102900051115     C                   MOVEL     ar9VCA        VACVCA
103000051115     C                   MOVEL     ar9TIC        VACTIC
103100000526     C                   ENDIF
103200000526      *
103300060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
103400011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
103500011002      *
103600011002     C     *IN68         IFEQ      '1'
103700060214     C                   MOVEL     ar4KSC        VACCCM
103800011002     C                   ELSE
103900921026     C     BLPCCM        IFNE      0
104000941228     C                   MOVEL     BLPCCM        VACCCM
104100921026     C                   ELSE
104200941228     C                   MOVEL     BLPKSC        VACCCM
104300921026     C                   END
104400011002     C                   END
104500941228     C                   MOVEL     BLPRMN        VACRMN
104600941228     C                   MOVEL     BLPRMA        VACRMA
104700941228     C                   MOVEL     BLPRMO        VACRMO
104800941228     C                   MOVEL     BLPFFD        VACFFD
104900000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
105000000921     C     §POST         IFEQ      'S'
105100000921     C                   Z-ADD     BLPNCD        VACDCR
105200000921     C                   ELSE
105300000921     C                   MOVEL     BLPDCR        VACDCR
105400000921     C                   ENDIF
105500941228     C                   MOVEL     BLPTCR        VACTCR
105600941228     C                   MOVEL     BLPHCR        VACHCR
105700941228     C                   MOVEL     BLPTC1        VACTC1
105800941228     C                   MOVEL     BLPTC2        VACTC2
105900941228     C                   MOVEL     BLPDCM        VACDCM
106000941228     C                   MOVEL     BLPHMC        VACHMC
106100941228     C                   MOVEL     BLPCCA        VACCCA
106200941228     C                   MOVEL     WDLA          VACDLA
106300000526      *
106400000526     C     §POST         IFEQ      'S'
106500000526     C                   MOVEL     PTDTG         VACDAG
106600000526     C                   ELSE
106700941228     C  N33              Z-ADD     GCPMGC        VACDAG
106800941228     C  N33              MOVEL     GCPAGC        VACDAG
106900941228     C   33              MOVEL     *ZEROS        VACDAG
107000000526     C                   ENDIF
107100000526      *
107200990602     C*---------------------------------------------------------------*
107300990602     C* Se ho trovato un evanto di messa in consegna....
107400060912 1   C     WTRMIC        IFEQ      'S'
107500990602     C* ... e non ho la data di consegna totale sulla bolla ..
107600990602     C     BLPDCM        ANDEQ     0
107700040319      *
107800990602     C* Verifico se ho trovato una pratica C.A. aperta ...
107900060912 2   C     DATMAX        IFNE      0
108000990602     C     §3KCQ4        ANDEQ     'S'
108100990602     C* .. e se l'apertura di quest'ultima è successiva alla
108200990602     C*   messa in consegna ...
108300990602     C     DATMAX        ANDGE     WDTMIC
108400990602     C* .. allora imposto la data apertura C.A. nella data consegna
108500990602     C*    e imposto il tipo consegna anomala = 'A'
108600990602     C                   Z-ADD     1200          VACHMC
108700990602     C                   Z-ADD     DATMAX        VACDCM
108800990602     C                   MOVEL     'A'           VACCCA
108900060912 x2  C                   ELSE
109000990602     C* ... altrimenti imposto data messa in consegna
109100990602     C                   Z-ADD     EVBHEV        VACHMC
109200980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
109300990602     C                   MOVEL     'C'           VACCCA
109400060912 e2  C                   END
109500990602     C*
109600990602     C* ... se è stata impostata la data di consegna sulla bolla ..
109700060912 x1  C                   ELSE
109800040319     C*
109900060912 2   C     BLPDCM        IFNE      0
110000040319     C*
110100040319     C* Un tempo era stato asteriscato questo pezzo
110200040319     C* >>>>>>>>>>>>>
110300040409      * non deve essere fatto per l'estero
110400060912 3   C     wtpINV        Ifne      'E'
110500040409      *
110600990602     C* ... verifico se ho trovato una C.A. aperta ...
110700060912 4   C     DATMAX        IFNE      0
110800040319     C     §3KCQ4        ANDEQ     'S'
110900990602     C* .. e se l'apertura di quest'ultima è successiva alla
111000990602     C* .. consegna ...
111100040319     C     DATMAX        ANDGT     BLPDCM
111200040319     C*
111300990602     C* .. allora imposto la data apertura C.A. nella data consegna
111400990602     C*    e imposto il tipo consegna anomala = 'A'
111500040319     C                   Z-ADD     1200          VACHMC
111600040319     C                   Z-ADD     DATMAX        VACDCM
111700040319     C                   MOVEL     'A'           VACCCA
111800040319     C*
111900060912 e4  C                   ENDif
112000060912 e3  c                   end
112100040319     C* >>>>>>>>>>>>>
112200040319     C*
112300990602     C* ... se non è stata effettuata la messa in consegna, la
112400990602     C*     bolla risulta non consegnata, ....
112500060912 x2  C                   ELSE
112600990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
112700990602     C*     le consegne anomale, imposto la data consegna + flag
112800990602     C*     consegna anomala='A'
112900060912 3   C     DATMAX        IFNE      0
113000990602     C     §3KCQ4        ANDEQ     'S'
113100990602     C                   Z-ADD     1200          VACHMC
113200990602     C                   Z-ADD     DATMAX        VACDCM
113300990602     C                   MOVEL     'A'           VACCCA
113400060912 e3  C                   END
113500060912 e2  C                   END
113600060912 e1  C                   END
113700060912     C* Se ho trovato un evento di ripristino
113800060912 1   C     WTRrip        IFEQ      'S'
113900060912     C     BLPDCM        ANDEQ     0
114000060912     C     §3KCQ1        ANDEQ     'S'
114100060912     C                   Z-ADD     0             VACHMC
114200060912     C                   Z-ADD     0             VACDCM
114300060912     C                   MOVEL     'R'           VACCCA
114400060912 e1  c                   endif
114500060912
114600080417     C***  N32              UPDATE    FNVAC000
114700080417     C***   32              WRITE     FNVAC000
114800080417     c                   exsr      Write_VAC
114900930603     C                   SETON                                        06
115000990604     C*
115100990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
115200990618     C* e questa è stata anche.....
115300060912 1   C     §3KCQ1        IFEQ      'S'
115400080417     C     §3KCSD        ANDEQ     'EW'
115500080417      *
115600080417     C**** §3KCQ1        OREQ      'S'
115700080417     C**** §3KCSD        ANDEQ     'ES'
115800080417     C**** §3KCQ1        OREQ      'S'
115900080417     C**** §3KCSD        ANDEQ     'EU'
116000080417      *
116100060912 2   C     VACCCA        IFEQ      'A'
116200990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
116300990618     C* invio al partner dell'informazione della consegna parziale
1164000609123    C     BLPDCM        IFEQ      0
116500990618     C     BLPDCP        ANDNE     0
116600990618     C                   Z-ADD     0             VACHMC
116700990618     C                   Z-ADD     0             VACDCM
116800990618     C                   MOVEL     ' '           VACCCA
116900080417     C***                   WRITE     FNVAC000
117000080417     c                   exsr      Write_VAC
117100060912x3   C                   ELSE
117200990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
117300990618     C* invio al partner dell'informazione
117400060912 4   C     BLPDCM        IFEQ      0
117500990624     C     WDTMIC        ANDGT     0
117600990618     C                   Z-ADD     EVBHEV        VACHMC
117700990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
117800990618     C                   MOVEL     'C'           VACCCA
117900080417     C***                   WRITE     FNVAC000
118000080417     c                   exsr      Write_VAC
118100060912e4   C                   END
118200060912e3   C                   END
118300990618     C*
118400990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
118500990618     C* scrivo anche record x apertura C.A.
118600060912x2   C                   ELSE
1187000609123    C     DATMAX        IFNE      0
118800990618     C     §3KCQ4        ANDEQ     'S'
118900990618     C                   Z-ADD     1200          VACHMC
119000990618     C                   Z-ADD     DATMAX        VACDCM
119100990618     C                   MOVEL     'A'           VACCCA
119200080417     C***                   WRITE     FNVAC000
119300080417     c                   exsr      Write_VAC
119400060912 e3  C                   END
119500060912 e2  C                   END
119600060912 e1  C                   END
119700990618     C*
119800921026     C                   ENDSR
119900990602     C*---------------------------------------------------------------*
120000990602     C* Controllo se esiste una pratica C.A. aperta                  -*
120100990602     C*---------------------------------------------------------------*
120200990602     C     CRTCA         BEGSR
120300990602     C*
120400990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
120500990602     C* per reperire la pratica di danno con data più alta
120600990602     C                   Z-ADD     0             DATMAX            8 0
120700990602     C     §3KCQ4        IFEQ      'S'
120800040715      **
120900040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
121000040715      * SIA COME MAMMA CHE COME FIGLIA
121100040715      **
121200040715     c                   clear                   fidn12ds
121300040715     c                   eval      i12aas = blpAAS
121400040715     c                   eval      i12lnp = blpLNP
121500040715     c                   eval      i12nrs = blpNRS
121600040715     c                   eval      i12nsp = blpNSP
121700040715     c                   eval      i12fel = 'E'
121800040715     c                   eval      i12fan = 'E'
121900040715     c                   eval      i12fch = 'E'
122000040715     c                   call      'FIDN12R'
122100040715     c                   parm                    fidn12ds
122200040715     c                   z-add     *zeros        ii                2 0
122300040715      **
122400040715      * prende fra le C.A. aperte quella con la data ultima
122500040715     c                   if        o12nca > 0
122600040715     c                   dow       ii <  o12nca
122700040715      **
122800040715     c                   add       1             ii
122900040715     C                   movel     skDca(ii)     datape            8 0
123000040715     C     DATMAX        IFLT      DATAPE
123100040715     C                   Z-ADD     DATAPE        DATMAX
123200040715     C                   END
123300040715      *
123400040715     c                   enddo
123500040715     c                   endif
123600040715      ****
123700040715     C**** KDCT          CHAIN     FNDCT02L                           21
123800040715     C**** *IN21         DOWEQ     '0'
123900040715     C**** DCTATB        IFEQ      ' '
124000040715     C**** DCTDCH        ANDEQ     0
124100040715     C****               Z-ADD     DCTMGC        DATAPE            8 0
124200040715     C****               MOVEL     DCTAAC        DATAPE
124300040715     C**** DATMAX        IFLT      DATAPE
124400040715     C****               Z-ADD     DATAPE        DATMAX
124500040715     C****               END
124600040715     C****               END
124700040715     C**** KDCT          READE     FNDCT02L                               21
124800040715     C****               END
124900040715      *
125000040715     C                   END
125100040715     C*
125200990602     C                   ENDSR
125300970122     C*---------------------------------------------------------------*
125400970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
125500970122     C*---------------------------------------------------------------*
125600970121     C     TSTNMC        BEGSR
125700040422     **
125800040422     ** avendo fatto entrare anche x data consegna GT di 0
125900040422     ** il flag deve essere resettato solo per la vecchia condizione
126000040422     ** altrimenti non scrive + il VAC come una volta.
126100040422     c                   if        blpdcm = 0
126200040422     **
126300970122     C                   MOVEL     'N'           WFLDT3
126400090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
126500090223     C**   sempre il VAC.
126600090223     C********           MOVEL     'N'           WINVAC
126700040422     **
126800040422     c                   end
126900040422     **
127000970122     C                   MOVEL     'N'           WTREVE            1
127100970122     C                   MOVEL     *BLANKS       WSVTMC            1
127200970122     C                   MOVEL     *BLANKS       WMOT1            35
127300970122     C                   MOVEL     *BLANKS       WMOT2            35
127400970122     C*  Leggo le estensioni bolla della spedizione e per quelle
127500970122     C*  relative ad un motivo di mancata consegna scrivo VAG
127600970122     C                   DO        Y             W
127700970122     C                   MOVEL     T7K(W)        WTMC              1
127800970122     C                   MOVE      T7K(W)        KTRC
127900060214     C     Kar4          CHAIN     Fiar401L                           35
128000970123     C     *IN35         IFEQ      '0'
128100060214     C     ar4FTR        ANDNE     'T'
128200060214     C     ar4FTR        ANDNE     'C'
128300970122     C*  Se ho trovato estensione bolla e non è trasmesso
128400970122     C*  controllo se ho un nuovo tp.mancata consegna
128500970122     C*  e se si reperisco evento corrispondente
128600970122     C     WTMC          IFNE      WSVTMC
128700970122     C                   EXSR      REPEVE
128800970122     C                   END
128900970122     C*  se ho trovato l'evento controllo se devo scrivere
129000970122     C*  nuovo record VAG:
129100970122     C     WTREVE        IFEQ      'S'
129200970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
129300970122     C*  - se ho già scritto 2 motivi --> new record
129400970122     C     WTMC          IFNE      WSVTMC
129500970122     C     WMOT2         ORNE      *BLANKS
129600970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
129700970124     C     WSVTMC        IFNE      *BLANKS
129800970124     C     WMOT2         ANDEQ     *BLANKS
129900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
130000970318     C*  primi 15 chr nella seconda motivazione
130100970318     C     WCHR15        IFNE      *BLANKS
130200970318     C                   MOVEL     WCHR15        VAGNOT
130300970318     C                   END
130400080403     c*****              movel     fnvagds       vgddta
130500080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
130600050606     c                   move      'VG'          vgdtip
130700091116     c******             move      §3kcks        vgdksu
130800091116     c                   move      §3kgks        vgdksu
130900050627     c                   movel     0             vgdksu
131000091116     c******             move      §3kcsd        vgdtsc
131100091116     c                   move      §3kgsd        vgdtsc
131200050608     c                   eval      vgdpgm = 'FNLSA9R'
131300050627     c                   z-add     dateu         vgddat
131400091116     c                   if        §3kgsd  <> '  '
131500050606     C                   WRITE     tivgd000
131600091116     c                   end
131700970124     C                   SETON                                        06
131800970124     C                   END
131900970124     C*  New record
132000970122     C                   EXSR      NEWVAG
132100970122     C                   ELSE
132200970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
132300970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
132400970318     C     WCHR15        IFNE      *BLANKS
132500970318     C                   MOVEL     WCHR15        VAGNOT
132600060214     C                   MOVE      ar4NOT        VAGNOT
132700970318     C                   ELSE
132800970318     C*  Se codice generico occupo chr. da 32 posizione
132900970318     C     VAGCMC        IFEQ      'G  '
133000970318     C     VAGCMC        OREQ      'GEN'
133100060214     C                   MOVEL     ar4NOT        WCHR19           19
133200060214     C                   MOVE      ar4NOT        WCHR16           16
133300970318     C                   MOVE      WCHR19        VAGDMC
133400970318     C                   MOVEL     WCHR16        VAGNOT
133500970318     C                   ELSE
133600970318     C*  Altrimenti imposto solo ulteriore descrizione
133700060214     C                   MOVEL     ar4NOT        VAGNOT
133800970318     C                   END
133900970318     C                   END
134000060214     C                   MOVEL     ar4NOT        WMOT2
134100970122     C                   MOVEL     *BLANKS       WMOT1
134200050606     C*  Scrivo tivgd
134300080403     c*****              movel     fnvagds       vgddta
134400080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
134500050606     c                   move      'VG'          vgdtip
134600091116     c*****              move      §3kcks        vgdksu
134700091116     c                   move      §3kgks        vgdksu
134800050627     c                   movel     0             vgdksu
134900091116     c*****              move      §3kcsd        vgdtsc
135000091116     c                   move      §3kgsd        vgdtsc
135100050608     c                   eval      vgdpgm = 'FNLSA9R'
135200050627     c                   z-add     dateu         vgddat
135300091116     c                   if        §3kgsd  <> '  '
135400050606     C                   WRITE     tivgd000
135500091116     c                   end
135600970122     C                   SETON                                        06
135700970122     C                   END
135800970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
135900970122     C                   MOVEL     'S'           WFLDT3
136000970122     C                   MOVEL     'S'           WINVAC
136100060214     C*  Fleggo Fiar4
136200060214     C                   MOVEL     'C'           ar4FTR
136300060214     C                   Z-ADD     DATEU         ar4DTR
136400060214     C                   EXCEPT    AGGar4
136500970122     C                   END
136600970122     C                   END
136700970122     C*
136800970122     C                   END
136900970122     C*  Controllo se mi è rimasto un VAG da scrivere
137000970122     C     WMOT1         IFNE      *BLANKS
137100970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
137200970318     C*  primi 15 chr nella seconda motivazione
137300970318     C     WCHR15        IFNE      *BLANKS
137400970318     C                   MOVEL     WCHR15        VAGNOT
137500970318     C                   END
137600050606     C*  Scrivo tivgd
137700080403     c*****              movel     fnvagds       vgddta
137800080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
137900050606     c                   move      'VG'          vgdtip
138000091116     c*****              move      §3kcks        vgdksu
138100091116     c                   move      §3kgks        vgdksu
138200050627     c                   movel     0             vgdksu
138300091116     c*****              move      §3kcsd        vgdtsc
138400091116     c                   move      §3kgsd        vgdtsc
138500050608     c                   eval      vgdpgm = 'FNLSA9R'
138600050627     c                   z-add     dateu         vgddat
138700091116     c                   if        §3kgsd  <> '  '
138800050606     C                   WRITE     tivgd000
138900091116     c                   end
139000970122     C                   SETON                                        06
139100970122     C                   END
139200970122     C*
139300970122     C                   ENDSR
139400970122     C*---------------------------------------------------------------*
139500970122     C* Controllo se esiste evento                                   -*
139600970122     C*---------------------------------------------------------------*
139700970122     C     REPEVE        BEGSR
139800970122     C*
139900970122     C                   MOVEL     'N'           WTREVE
140000970122     C*  Mi posiziono su file eventi
140100970122     C                   Z-ADD     BLPAAS        WAAS
140200990810     C     KEVB          SETGT     FNEVB01L
140300990810     C     KEVB          READPE    FNEVB01L                               34
140400970122     C*  Loop letture fino a fine file o travato evento
140500970122     C     *IN34         DOWEQ     '0'
140600970122     C     WTREVE        ANDEQ     'N'
140700040504      **
140800040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
140900040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
141000040504      **  le A16/A23 come varianti in programmi che precedono facendo
141100040504      **  sballare il ragionamento in questo pgm.
141200040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
141300040504      **
141400040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
141500040504     c                   movel     '0'           evbCEV
141600040504     c                   end
141700040504      **
141800040504      **
141900970122     C*  Reperisco tipo mancata consegna evento letto
142000970122     C                   Z-ADD     1             WW                4 0
142100970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
142200970122     C*  Controllo se tipo mancata consegna evento letto
142300970122     C*  è uguale a quella dell'estensione bolla
142400970122     C     *IN42         IFEQ      '1'
142500970122     C     T2A(WW)       ANDEQ     WTMC
142600970122     C                   MOVEL     'S'           WTREVE
142700970122     C                   ELSE
142800990810     C     KEVB          READPE    FNEVB01L                               34
142900970122     C                   END
143000970122     C                   END
143100970122     C*
143200970122     C                   ENDSR
143300970122     C*---------------------------------------------------------------*
143400970122     C* Nuovo record FNVAG                                           -*
143500970122     C*---------------------------------------------------------------*
143600970122     C     NEWVAG        BEGSR
143700970122     C*
143800050606     C                   CLEAR                   FNVAGds
143900970721     C                   MOVEL     'S'           WRTVAG            1
144000970318     C                   MOVEL     *BLANKS       WCHR25           25
144100970318     C                   MOVEL     *BLANKS       WCHR15           15
144200970122     C                   Z-ADD     BLPAAS        VAGAAS
144300970122     C                   Z-ADD     BLPLNP        VAGLNP
144400970122     C                   Z-ADD     BLPNRS        VAGNRS
144500970122     C                   Z-ADD     BLPNSP        VAGNSP
144600970122     C                   Z-ADD     BLPLNA        VAGLNA
144700990810     C                   MOVEL     EVBDEV        WAA2              4 0
144800970123     C                   MOVE      WAA2          VAGAGC
144900970122     C                   MOVE      EVBDEV        VAGMGC
145000060214     C                   MOVEL     ar4NOT        VAGCMC
145100060214     C                   MOVEL     ar4NOT        WMOT1
145200970122     C                   MOVEL     *BLANKS       WMOT2
145300970307     C                   Z-ADD     1             YY                4 0
145400970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
145500970307     C     VAGCMC        IFNE      'G  '
145600970307     C     VAGCMC        ANDNE     'GEN'
145700970307     C                   MOVEL     D2A(YY)       VAGDMC
145800970318     C                   MOVE      D2A(YY)       WCHR25
145900970318     C                   MOVEL     WCHR25        WCHR15
146000970305     C                   ELSE
146100060214     C                   MOVE      ar4NOT        WCOM31           31
146200970122     C                   MOVEL     WCOM31        VAGDMC
146300970305     C                   END
146400970122     C                   MOVEL     BLPRMA        VAGRMA
146500970122     C                   Z-ADD     BLPRMN        VAGRMN
146600970122     C     BLPCCM        IFNE      0
146700970122     C                   MOVEL     BLPCCM        VAGSCM
146800970122     C                   ELSE
146900970122     C                   MOVEL     BLPKSC        VAGSCM
147000970122     C                   END
147100970123     C*
147200970123     C                   MOVEL     WTMC          WSVTMC
147300970122     C*
147400970122     C                   ENDSR
147500001031     C*---------------------------------------------------------------*
147600001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
147700001031     C*---------------------------------------------------------------*
147800001031     C     PARZIA        BEGSR
147900001031     C*
148000001031      * LOOP SU DETTAGLIO COLLI
148100001103     C                   Z-ADD     0             TOTCO
148200001031     C                   Z-ADD     0             CONS
148300001102     C                   Z-ADD     0             ULTIMA            8 0
148400001031     C     KBLP          SETLL     FNBLT01L
148500001031     C                   DO        *HIVAL
148600001102     C     KBLP          READE     FNBLT01L                               97
148700001031     C   97              LEAVE
148800001031      * CONTA CONSEGNATI E NON CONSEGNATI
148900001103     C     BLTDCM        IFGT      0
149000001031     C                   ADD       1             CONS              3 0
149100001031     C                   END
149200001103     C                   ADD       1             TOTCO             3 0
149300001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
149400001031     C     BLTDCM        IFGT      ULTIMA
149500001031     C                   Z-ADD     BLTDCM        ULTIMA
149600001031     C                   ENDIF
149700001031     C                   ENDDO
149800001102     C                   MOVE      CONS          CONSA             3
149900001103     C                   MOVE      TOTCO         TOTCOA            3
150000001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
150100001031      *
150200050606     C                   CLEAR                   FNVAGds
150300001031     C                   MOVE      BLPLNP        VAGLNP
150400001031     C                   MOVE      BLPAAS        VAGAAS
150500001031     C                   MOVE      BLPNRS        VAGNRS
150600001031     C                   MOVE      BLPNSP        VAGNSP
150700001107     C                   MOVEL     'P  '         VAGCMC
150800001031     C                   MOVE      BLPRMN        VAGRMN
150900001031     C                   MOVE      BLPRMA        VAGRMA
151000001031     C                   MOVE      BLPLNA        VAGLNA
151100001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
151200001031     C                   MOVEL     COS(1)        VAGDMC
151300001103     C                   MOVEA     COS(2)        COM
151400001102     C                   MOVEA     CONSA         COM(22)
151500001103     C                   MOVEA     TOTCOA        COM(29)
151600001102     C                   MOVEA     COM           VAGNOT
151700001031      *
151800001031     C     BLPCCM        IFGT      0
151900001031     C                   Z-ADD     BLPCCM        VAGSCM
152000001031     C                   ELSE
152100001031     C                   Z-ADD     BLPKSC        VAGSCM
152200001031     C                   END
152300001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
152400001031     C                   MOVEL     ULTIMA        VAGAGC
152500001031     C                   MOVE      ULTIMA        VAGMGC
152600001031      *
152700080403     c*****              movel     fnvagds       vgddta
152800080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
152900050606     c                   move      'VG'          vgdtip
153000091116     c*****              move      §3kcks        vgdksu
153100091116     c                   move      §3kgks        vgdksu
153200050627     c                   movel     0             vgdksu
153300091116     c*****              move      §3kcsd        vgdtsc
153400091116     c                   move      §3kgsd        vgdtsc
153500050608     c                   eval      vgdpgm = 'FNLSA9R'
153600050627     c                   z-add     dateu         vgddat
153700091116     c                   if        §3kgsd  <> '  '
153800050606     C                   WRITE     tivgd000
153900091116     c                   end
154000001106     C                   MOVEL     'S'           WTRPAR
154100001031      *
154200001031     C                   ENDSR
154300080417      *------------------------------------------------------------------------*
154400080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
154500080417      *------------------------------------------------------------------------*
154600080417     C     Write_VAC     BEGSR
154700080417      *
154800090529      *  >>>>>>>>>
154900090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
155000090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
155100090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
155200090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
155300090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
155400090529      *   il record.
155500090529      *  >>>>>>>>>
155600090603     c                   if        §3kCTB ='S'
155700090603     c                   clear                   uguale_VAC        1
155800090529      *
155900090529     C                   movel     'VAC'         Kar5vac
156000090529     c     kar5          chain     fiar501l
156100090529      *
156200090529     c                   if        %Found(fiar501l)
156300090529      * Imposta i campi precedentemente salvati nella DS da comparare
156400090529     c                   eval      fnvacHGds = ar5UNI
156500090603      *
156600090603     c                   eval      VacGC1chg = VacGC1
156700090603     c                   eval      VacGC2chg = VacGC2
156800090603     c                   eval      VacFFDchg = VacFFD
156900090603     c                   eval      VacTCRchg = VacTCR
157000090603     c                   eval      VacDCRchg = VacDCR
157100090603     c                   eval      VacHCRchg = VacHCR
157200090603     c                   eval      VacDCMchg = VacDCM
157300090603     c                   eval      VacHMCchg = VacHMC
157400090603     c                   eval      VacTC1chg = VacTC1
157500090603     c                   eval      VacTC2chg = VacTC2
157600090603     c                   eval      VacCCAchg = VacCCA
157700090603     c                   eval      VacDLAchg = VacDLA
157800090603     c                   eval      VacDAGchg = VacDAG
157900090529      * quindi
158000090529      *    controllo di uguaglianza
158100090529     c                   if        fnvacHGds = DSvacHG
158200090529     c                   eval      uguale_VAC = 'S'
158300090603     c                   move      '='           ar5UNI
158400090603     c                   else
158500090603     c                   move      ' '           ar5UNI
158600090603     c                   eval      ar5UNI    = DSvacHG
158700090603     c                   end
158800090529      *
158900090529     c                   eval      AR5DAC = dateu
159000090529     c                   eval      AR5ORC = Orario
159100090529     c                   eval      AR5PRG = AR5PRG + 1
159200090529     c*
159300090529     c                   update    fiar5000
159400090529     c*
159500090529     c                   if        uguale_VAC = 'S'
159600090529     c                   leavesr
159700090529     c                   end
159800090529      *
159900090529     c                   else
160000090529      *
160100090529      *  lo memorizza x la prima volta
160200090529     c                   clear                   fiar5000
160300090529     c                   eval      ar5AAS = blpAAS
160400090529     c                   eval      ar5LNP = blpLNP
160500090529     c                   eval      ar5NRS = blpNRS
160600090529     c                   eval      ar5NSP = blpNSP
160700090529     c                   eval      ar5TRD = 'VAC'
160800090529     c                   eval      ar5DAC = dateu
160900090529     c                   eval      ar5ORC = Orario
161000090529     c                   eval      ar5PRG = AR5PRG + 1
161100090603      *
161200090603     c                   eval      VacGC1chg = VacGC1
161300090603     c                   eval      VacGC2chg = VacGC2
161400090603     c                   eval      VacFFDchg = VacFFD
161500090603     c                   eval      VacTCRchg = VacTCR
161600090603     c                   eval      VacDCRchg = VacDCR
161700090603     c                   eval      VacHCRchg = VacHCR
161800090603     c                   eval      VacDCMchg = VacDCM
161900090603     c                   eval      VacHMCchg = VacHMC
162000090603     c                   eval      VacTC1chg = VacTC1
162100090603     c                   eval      VacTC2chg = VacTC2
162200090603     c                   eval      VacCCAchg = VacCCA
162300090603     c                   eval      VacDLAchg = VacDLA
162400090603     c                   eval      VacDAGchg = VacDAG
162500090529     c                   eval      ar5UNI = DSvacHG
162600090529     c                   write     fiar5000
162700090529      *
162800090529     c                   end
162900090529     c                   end
163000090529      *  >>>>>>>>>
163100090529      *
163200080417     C                   clear                   tivgd000
163300080417     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
163400080417     C                   eval      vgdTIP = 'VC'
163500080417     C                   movel     *all'0'       vgdKSU
163600080417     c                   move      §3kcks        vgdksu
163700080417      * tipo invio
163800080417     c                   move      §3kcsd        vgdtsc
163900080417     c                   eval      vgdpgm = 'FNLSA9R'
164000080417     c                   z-add     dateu         vgddat
164100080417      *
164200080417      *  x generare l'EDI:
164300080417      *    occorre farlo in modo controllato (personalizzato)
164400080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
164500080417     C                   clear                   DVGDFLO
164600080417     C                   movel     'P'           §VGDFELA
164700080417     C                   movel     DVGDFLO       vgdflo
164800080417     c                   end
164900080417      *
165000091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
165100091116      *  Non deve scrivere nulla.
165200091116     c                   if        §3kcsd <> *blank
165300080417     C                   write     tivgd000
165400110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
165500110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
165600110721     C                   MOVEL     'S'           WFLDT3
165700091116     c                   end
165800080417      *
165900080417     C                   ENDSR
166000080930      *------------------------------------------------------------------------*
166100080930      * Routine di controllo x eventuali Errori
166200080930      *------------------------------------------------------------------------*
166300080930     C     *PSSR         BEGSR
166400080930      *
166500080930      * Comunque eseguo x rilasciare il TIVGD.
166600080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
166700080930     C                   clear                   trul47ds
166800080930     C                   eval      d47opz  = 'F'
166900080930     C                   eval      d47tip  = 'VC'
167000080930     C                   call      'TRUL47R'
167100080930     C                   parm                    trul47ds
167200080930      *
167300080930     C                   ENDSR     '*CANCL'
167400080417     C*------------------------------------------------------------------------*
167500060214     OFiar4000  E            AGGar4
167600060214     O                       ar4FTR
167700060214     O                       ar4DTR
167800001031**
167900941228OVRDBF FILE(FNVAC01T)           MBR(MXXXXXXXXX)
168000921026**
168100941228ALCOBJ OBJ((FNVAC01T *FILE *EXCL    MXXXXXXXXX))
168200921026**
168300941228DLCOBJ OBJ((FNVAC01T *FILE *EXCL    MXXXXXXXXX))
168400921026**
168500981210DLTOVR FILE(*ALL)
168600000726**
168700921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
168800921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
168900921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
169000001031**
169100001031Consegna parziale
169200001031Nr. colli consegnati xxx su xxx
