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
013000080417     C                   eval      d47chkj = 'N'
013100080930     C     WTPINV        IFEQ      'E'
013200080930     C                   eval      d47pgm  = 'FNLSA9R§'
013300080930     c                   else
013400080417     C                   eval      d47pgm  = 'FNLSA9R'
013500080930     c                   end
013600080417     C                   call      'TRUL47R'
013700080417     C                   parm                    trul47ds
013800080417      *
013900080417      *  se strada bloccata si va a fine pgm
014000080417     C                   if        d47sts = 'A'
014100080417     c                   goto      Vai_a_fine
014200080417     c                   end
014300080417     C*---------------------------------------------------------------*
014400080417      *?  Definizione Campi e Caricamento Tabelle   ?
014500080417     C*---------------------------------------------------------------*
014600080417      *
014700060921     C     Klbl2         KLIST
014800060921     C                   KFLD                    klb2_AAS
014900060921     C                   KFLD                    klb2_LNP
015000060921     C                   KFLD                    klb2_NRS
015100060921     C                   KFLD                    klb2_NSP
015200060921     C     Klbl2p        KLIST
015300060921     C                   KFLD                    exLBLAAp
015400060921     C                   KFLD                    exLBLLPp
015500060921     C                   KFLD                    exLBLNRp
015600060921     C                   KFLD                    exLBLNSp
015700060921     C     KBLP          KLIST
015800060921     C                   KFLD                    BLPAAS
015900060921     C                   KFLD                    BLPLNP
016000060921     C                   KFLD                    BLPNRS
016100060921     C                   KFLD                    BLPNSP
016200040715     C***  KDCT          KLIST
016300040715     C***                KFLD                    BLPAAS
016400040715     C***                KFLD                    BLPLNP
016500040715     C***                KFLD                    BLPNRS
016600040715     C***                KFLD                    BLPNSP
016700990602     C     KLBL          KLIST
016800990602     C                   KFLD                    BLPAAS
016900990602     C                   KFLD                    BLPLNP
017000990602     C                   KFLD                    BLPNRS
017100990602     C                   KFLD                    BLPNSP
017200060214     C     Kar4          KLIST
017300970122     C                   KFLD                    BLPAAS
017400970122     C                   KFLD                    BLPLNP
017500970122     C                   KFLD                    BLPNRS
017600970122     C                   KFLD                    BLPNSP
017700970122     C                   KFLD                    KTRC
017800941228     C     KEVB          KLIST
017900990810     C                   KFLD                    WAAS              4 0
018000941228     C                   KFLD                    BLPLNP
018100941228     C                   KFLD                    BLPNRS
018200941228     C                   KFLD                    BLPNSP
018300921026     C     KBLP2         KLIST
018400921026     C                   KFLD                    COD99             7 0
018500921026     C                   KFLD                    KSC
018600920316     C     KTAB          KLIST
018700921026     C                   KFLD                    CODUT             1 0
018800921026     C                   KFLD                    COD               2
018900941228     C     KTAB2         KLIST
019000941228     C                   KFLD                    CODUT             1 0
019100941228     C                   KFLD                    COD               2
019200941228     C                   KFLD                    KEY               8
019300090529     C     Kar5          KLIST
019400090529     C                   KFLD                    BLPAAS
019500090529     C                   KFLD                    BLPLNP
019600090529     C                   KFLD                    BLPNRS
019700090529     C                   KFLD                    BLPNSP
019800090529     C                   KFLD                    Kar5vac           3
019900941228     C**
020000941228     C* DEFINIZIONE CAMPI
020100941228     C**
020200941228     C     *LIKE         DEFINE    TBLKEY        SAVKEY
020300060214     C     *LIKE         DEFINE    ar4TRC        KTRC
020400051115     C     *LIKE         DEFINE    ar9TIC        PTTIC
020500051115     C     *LIKE         DEFINE    ar9CAS        PTCAS
020600051115     C     *LIKE         DEFINE    ar9VCA        PTVCA
020700000526     C     *LIKE         DEFINE    BLPIAS        PTIAS
020800000526     C     *LIKE         DEFINE    BLPVAS        PTVAS
020900000526     C     *LIKE         DEFINE    VACDAG        PTDTG
021000060921      *
021100060921     c     *LIKE         DEFINE    blpAAS        klb2_AAS
021200060921     c     *LIKE         DEFINE    blpLNP        klb2_LNP
021300060921     c     *LIKE         DEFINE    blpNRS        klb2_NRS
021400060921     c     *LIKE         DEFINE    blpNSP        klb2_NSP
021500941228     C*---------------------------------------------------------------*
021600921026     C* GIRO UDATE
021700941228     C                   TIME                    W0140            14 0
021800941228     C* UDATE IN GGMMAAAA
021900941228     C                   MOVE      W0140         WDTGIO            8 0
022000090529     C                   MOVEl     W0140         Orario            6 0
022100941228     C*
022200941228     C* UDATE IN AAAAMMGG
022300941228     C                   Z-ADD     WDTGIO        G02DAT
022400941228     C                   MOVEL     *BLANK        G02ERR
022500941228     C                   CALL      'XSRDA8'
022600941228     C                   PARM                    WLBDAT
022700941228     C                   MOVEL     G02INV        DATEU             8 0
022800941228     C*---------------------------------------------------------------*
022900000525     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
023000000525     C                   CLEAR                   FIP
023100000525     C                   Z-ADD     0             Y
023200000525     C     *LOVAL        SETLL     AZORG01L
023300000525     C                   DO        *HIVAL
023400000525     C                   READ      AZORG01L                               95
023500000525     C   95              LEAVE
023600000525     C                   MOVEL     ORGDE3        OG143
023700020729     C     §OgNTW        IFEQ      'PPT'
023800000525     C                   Z-ADD     1             Y
023900000525     C     *ZEROS        LOOKUP    FIP(Y)                                 23
024000000525     C   23              Z-ADD     ORGFIL        FIP(Y)
024100000525     C                   END
024200000525     C                   END
024300000525     C*---------------------------------------------------------------*
024400970122     C* Eseguo caricamento estensioni bolle x mot.no consegne da 7K
024500970122     C                   Z-ADD     0             Y                 4 0
024600970122     C                   MOVEA     *HIVAL        T7K
024700970122     C                   MOVEL     '7K'          COD
024800970122     C     KTAB          CHAIN     TABEL                              31
024900970122     C     *IN31         DOWEQ     '0'
025000970122     C     TBLFLG        IFEQ      ' '
025100970122     C* Controllo se impostato tipo mancata consegna
025200970122     C                   MOVEL     TBLUNI        DS7K
025300970122     C     §7KTMC        IFNE      *BLANKS
025400970122     C                   ADD       1             Y
025500970122     C                   MOVEL     §7KTMC        T7K(Y)
025600970123     C                   MOVEL     TBLKEY        WTRC              1
025700970123     C                   MOVE      WTRC          T7K(Y)
025800970122     C                   END
025900970122     C                   END
026000970122     C     KTAB          READE     TABEL                                  31
026100970122     C                   END
026200970122     C* Se ho trovato qualcosa riordino x tp.mancata consegna+tipo rec.
026300970122     C     Y             IFGT      0
026400970122     C                   SORTA     T7K
026500970122     C                   END
026600970122     C*---------------------------------------------------------------*
026700970122     C* Eseguo caricamento eventi con relativo motivo mancata consegna
026800970122     C                   Z-ADD     0             W                 4 0
026900970122     C                   MOVEA     *HIVAL        C2A
027000970305     C                   MOVEA     *HIVAL        D2A
027100970122     C                   MOVEA     *HIVAL        T2A
027200970122     C                   MOVEL     '2A'          COD
027300970122     C     KTAB          CHAIN     TABEL                              31
027400970122     C     *IN31         DOWEQ     '0'
027500970122     C     TBLFLG        IFEQ      ' '
027600970122     C* Caricamento evento + tipo mancata consegna
027700970122     C                   MOVEL     TBLUNI        DS2A
027800970122     C     §2AFTC        IFNE      *BLANKS
027900970122     C                   ADD       1             W
028000970122     C                   MOVEL     §2AFTC        T2A(W)
028100970305     C                   MOVEL     §2ADES        D2A(W)
028200970123     C                   MOVEL     TBLKEY        C2A(W)
028300970122     C                   END
028400970122     C                   END
028500970122     C     KTAB          READE     TABEL                                  31
028600970122     C                   END
028700080417     C*---------------------------------------------------------------*
028800080417      * ?  CICLO Principale del Programma              ?
028900970122     C*---------------------------------------------------------------*
029000080417      * ? LETTURA TABELLA "3K" DI GUIDA SCARICO DATI   ?
029100080417      * ?  x l'estero è rimasto solo "EW" -> EDI.      ?
029200921023     C                   MOVEL     '3K'          COD
029300921023     C     KTAB          SETLL     TABEL
029400921023     C     KTAB          READE     TABEL                                  31
029500921023     C*
029600921023     C     *IN31         DOWEQ     '0'
029700921023     C     TBLFLG        IFEQ      ' '
029800980407     C                   MOVEL     TBLUNI        DS3K
029900080417      *
030000080417      * ?  Se Estero:  ?
030100980409     C     WTPINV        IFEQ      'E'
030200080417     C     §3KCSD        ANDEQ     'EW'
030300080417      *
030400080417      ********
030500080417     C* SE RICEVO TIPO INVIO IMPOSTATO A 'ES' DEVO SPEDIRE SOLO
030600080417     C* LE DATE DI CONSEGNA PER L'EDI ITALIA/ESTERO + EUROPOLITAN
030700080417     C**** WTPINV        OREQ      'E'
030800080417     C**** §3KCSD        ANDEQ     'ED'
030900080417     C**** WTPINV        OREQ      'E'
031000080417     C**** §3KCSD        ANDEQ     'ES'
031100080417     C**** WTPINV        OREQ      'E'
031200080417     C**** §3KCSD        ANDEQ     'EU'
031300080417     C**** WTPINV        OREQ      'E'
031400080417     C**** §3KCSD        ANDEQ     'EB'
031500080417      *
031600980408     C* ..DATE DI CONSEGNA X ALTRI CLIENTI
031700080417      * ?  oppure se NON Estero e non EDI:  ?
031800980409     C     WTPINV        ORNE      'E'
031900080417     C     §3KCSD        ANDNE     'EW'
032000091116     C     §3KCSD        ANDNE     *blank
032100091116      *
032200080417     C**** §3KCSD        ANDNE     'ES'
032300080417     C**** §3KCSD        ANDNE     'ED'
032400080417     C**** §3KCSD        ANDNE     'EU'
032500080417     C**** §3KCSD        ANDNE     'EB'
032600921026     C*
032700921026     C* ELABORO SOLO SE ESISTE ALMENO UNA BOLLA
032800921026     C                   MOVEL     TBLKEY        KSC               7 0
032900930811     C                   MOVEL     KSC           COD99
033000930811     C                   MOVE      9999          COD99
033100011002     C                   MOVEL     KSC           KSCJ              7
033200930811     C*
033300941228     C     KSC           SETLL     FNBLP000                               32
033400941228     C  N32KBLP2         SETLL     FNBLP000                               32
033500060214     C  N32KSCJ          SETLL     FNBLP67J                               32
033600921023     C*
033700930811     C     *IN32         IFEQ      *ON
033800080417     C*
033900080417     c                   setoff                                       40
034000080417      *?Con il TIVGD Il VAC multimembro non esiste +  ?
034100080417     C* CONTROLLO ESISTENZA MEMBRO DEL CLIENTE
034200080417     C*********          EXSR      CTRMBR
034300921023     C*
034400080417      *?Con *in40 = Off in OR --> questa IF è come se non ci fosse.
034500080417      *?  quindi entra sempre ed elabora il cliente.
034600921023     C* ELABORO SPEDIZIONI DEL CLIENTE IN SCARICO
034700930811     C     *IN40         IFEQ      *OFF
034800970123     C     §3KNMC        ANDNE     'S'
034900970122     C     *IN40         OREQ      *OFF
035000001103     C     *IN40         OREQ      *OFF
035100001103     C     §3KCQ6        ANDNE     'S'
035200080417      *
035300941228     C* SALVO LA KEY
035400941228     C                   MOVEL     TBLKEY        SAVKEY
035500930811     C                   EXSR      ELAB
035600941228     C*
035700941228     C* RICHAINO IL RECORD DI TABELAA
035800941228     C                   MOVEL     '3K'          COD
035900941228     C                   MOVEL     SAVKEY        KEY
036000941228     C     KTAB2         CHAIN     TABEL                              31
036100930811     C                   Z-ADD     DATEU         §3KCDU
036200930811     C                   MOVEL     DS3K          TBLUNI
036300930811     C                   UPDATE    TABEL
036400921026     C                   END
036500930811     C                   END
036600921023     C                   END
036700980409     C                   END
036800921023     C*
036900921023     C     KTAB          READE     TABEL                                  31
037000921023     C                   END
037100921026     C*
037200080417      *?Accendeva lo (01) durante il controllo/allocazione del
037300080417      *? vecchio VAC multimembro se qualcosa era andata storta
037400080417      *? su un cliente.
037500080417      * ?  quindi questo messaggio non serve +  ?
037600921026     C* 01 ON - NON TUTTI I CLIENTI SONO STATI ELABORATI: RILANCIARE
037700080417     C** 01 *****        DO
037800080417     C***********        MOVEA     MSG(1)        SK(1)
037900080417     C***********        MOVEA     MSG(2)        SK(68)
038000080417     C***********        MOVEA     MSG(3)        SK(135)
038100080417     C***********        MOVEA     SK            COMM2           201
038200080417     C***********        Z-ADD     201           LUNG             15 5
038300080417     C***********
038400080417     C***********        CALL      'QCMDEXC'
038500080417     C***********        PARM                    COMM2
038600080417     C***********        PARM                    LUNG
038700080417     C***********        END
038800930601     C* PREPARAZIONE DATI
038900080417     C*********          CLOSE     FNVAC01T
039000981210     C*
039100981210     C* CALCELLO LE OVR ESEGUITE
039200981210     C                   MOVEL     *BLANKS       COMMAN           80
039300981210     C                   Z-ADD     17            LUNG
039400981210     C                   MOVEA     CMD(1)        COMMAN
039500981210     C                   CALL      'QCMDEXC'                            21
039600981210     C                   PARM                    COMMAN
039700080417     C                   PARM                    LUNG             15 5
039800080417      **
039900080417      *?Rimane solo il Lancio x la generazione dell'EDI dell'estero.  ?
040000980409     C   06WTPINV        IFEQ      'E'
040100080417      *
040200080730     C**********         CALL      'TRTC51R'
040300110419     C**********         CALL      'TRTC50R'
040400110419     C                   CALL      'TRTCT80R'
040500030924     C                   parm                    Kpjba
040600080417      *
040700980409     C                   ELSE
040800080417      *
040900080417      *?Scompare il vecchio discorso x Halley (non esiste +)  ?
041000000616      * SE IL SIF È DI PROVA  RICHIAMO PGM AFFINCHÈ VENGA
041100000616      * CREATA DTAARA IN QTEMP CHE PERMETTE DI SIMULARE DI
041200000616      * ESSERE IN FILIALE
041300080417     C**********         MOVEL     KNSIF         SIFPRO            7
041400080417     C**** SIFPRO        IFEQ      'GAITRAP'
041500080417     C**** SIFPRO        OREQ      'FILTRAP'
041600080417     C**********         CALL      'TRTC00CP'
041700080417     C**********         ENDIF
041800080417     C**********
041900080417     C**********         CALL      'TRTC00C3'
042000080417      *
042100980409     C                   END
042200080417     C*
042300080417      *? Controllato se non si è potuto procedere, si è arrivati qui.  ?
042400080417     c     Vai_a_fine    tag
042500080417     C*
042600080417      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
042700080417     C                   clear                   trul47ds
042800080417     C                   eval      d47opz  = 'F'
042900080417     C                   eval      d47tip  = 'VC'
043000080417     C                   call      'TRUL47R'
043100080417     C                   parm                    trul47ds
043200080417     C*
043300921023     C                   SETON                                        LR
043400080417      *================================================================
043500921026     C* CONTROLLO ESISTENZA MEMBRO E APERTURA ------------------------*
043600080417      *? Questa Routine è da eliminare poichè ora l'FNVAC00T in Filiale?
043700080417      *?  è stato sostituito con il TIVGD come Standard di Download.
043800080417      *================================================================
043900921023     C     CTRMBR        BEGSR
044000080417      *
044100921026     C   40              SETOFF                                       9940
044200921026     C*
044300000619      * SE IL SIF È DI PROVA  FORZO  GAITRAPRD
044400080417     C                   MOVEL     KNSIF         SIFPRO            7
044500000619     C                   MOVE      KNSIF         PROVA             4
044600000619     C     PROVA         IFEQ      'PFP '
044700000619     C     PROVA         OREQ      'PFM '
044800000619     C                   MOVEL     SIFPRO        LIB
044900000619     C                   MOVE      'RD '         LIB
045000000619     C                   ENDIF
045100921026     C   99              DO
045200080417      *
045300080417     C********           CLOSE     FNVAC01T
045400921026     C* DISALLOCO
045500921026     C                   MOVEA     LOGMBR        CM3(37)
045600921026     C                   MOVEL     *BLANKS       COMMAN
045700921026     C                   MOVEA     CM3           COMMAN           80
045800921028     C                   Z-ADD     48            LUNG             15 5
045900921026     C*
046000921026     C                   CALL      'QCMDEXC'                            40
046100921026     C                   PARM                    COMMAN
046200921026     C                   PARM                    LUNG
046300921026     C                   END
046400921026     C* CHKOBJ E ADDLFM
046500941228     C                   MOVEL     'M'           ALF8              8
046600941228     C                   MOVE      §3KCKS        ALF8
046700941228     C*
046800941228     C                   MOVEL     'FNVAC00T'    FISICO           10
046900921026     C                   MOVEL     ALF8          FISMBR           10
047000941228     C                   MOVEL     'FNVAC01T'    LOGICO           10
047100921026     C                   MOVEL     ALF8          LOGMBR           10
047200000619     C     PROVA         IFNE      'PFP '
047300000619     C     PROVA         ANDNE     'PFM '
047400921026     C                   MOVEL     KNSIF         LIB              10
047500000619     C                   END
047600921026     C                   MOVEL     ' '           FLG               1
047700921026     C*
047800921026     C                   CALL      'TRUL50C'
047900921026     C                   PARM                    FISICO
048000921026     C                   PARM                    FISMBR
048100921026     C                   PARM                    LIB
048200921026     C                   PARM                    LOGICO
048300921026     C                   PARM                    LOGMBR
048400921026     C                   PARM                    FLG
048500921026     C* ALLOCO
048600921026     C                   MOVEA     LOGMBR        CM2(37)
048700921026     C                   MOVEL     *BLANKS       COMMAN
048800921026     C                   MOVEA     CM2           COMMAN           80
048900921028     C                   Z-ADD     48            LUNG             15 5
049000921026     C*
049100921026     C                   CALL      'QCMDEXC'                            40
049200921026     C                   PARM                    COMMAN
049300921026     C                   PARM                    LUNG
049400921026     C*
049500921026     C* 40 OFF - ALLOCATO
049600921026     C  N40              DO
049700921026     C* OVRDBF
049800921026     C                   MOVEA     LOGMBR        CM1(37)
049900921026     C                   MOVEL     *BLANKS       COMMAN
050000921026     C                   MOVEA     CM1           COMMAN           80
050100921028     C                   Z-ADD     47            LUNG             15 5
050200921026     C                   CALL      'QCMDEXC'                            40
050300921026     C                   PARM                    COMMAN
050400921026     C                   PARM                    LUNG
050500921026     C*
050600080417     C**N40*****         OPEN      FNVAC01T
050700921026     C  N40              SETON                                        99
050800921026     C                   END
050900941228     C*
051000921026     C   40              SETON                                        01
051100080417     C*
051200921023     C                   ENDSR
051300080417      *================================================================
051400921023     C* ELABORO SPEDIZIONI DEL CLIENTE -------------------------------*
051500080417      *================================================================
051600921023     C     ELAB          BEGSR
051700040319     C*
051800941228     C     KSC           SETLL     FNBLP000
051900941228     C     KSC           READE     FNBLP000                               30
052000921026     C*
052100921026     C     *IN30         DOWEQ     '0'
052200921026     C                   EXSR      CONTR
052300941228     C     KSC           READE     FNBLP000                               30
052400921026     C                   END
052500921026     C*
052600921026     C* LEGGO I 9999 CODIFICATI
052700921026     C*
052800941228     C     KBLP2         SETLL     FNBLP000
052900941228     C     KBLP2         READE     FNBLP000                               30
053000941228     C*
053100921026     C     *IN30         DOWEQ     '0'
053200921026     C                   EXSR      CONTR
053300941228     C     KBLP2         READE     FNBLP000                               30
053400921026     C                   END
053500040319     C*
053600010927     C* INSERISCO LOOP SU FILE DI JOIN PER VEDERE SE ESISTONO RECORD
053700060214     C* SU ar4 CON CHIAVE KSC IN ar4NOT
053800010927      *
053900060214     C     KSCJ          SETLL     FNBLP67J
054000010927     C                   DO        *HIVAL
054100060214     C     KSCJ          READE     FNBLP67J                               66
054200010927     C   66              LEAVE
054300010927     C     KBLP          CHAIN     FNBLP01L                           67
054400010927     C     *IN67         IFEQ      '0'
054500010927     C                   SETON                                        68
054600010927     C                   EXSR      CONTR
054700010927     C                   SETOFF                                       68
054800010927     C                   ENDIF
054900010927     C                   ENDDO
055000040319     C*
055100010927     C                   SETOFF                                       68
055200040319     C*
055300921026     C                   ENDSR
055400921026     C*
055500921026     C* VEDO SE ELABORARE BOLLA --------------------------------------*
055600921026     C     CONTR         BEGSR
055700990602     C*
055800990602     C* Escludo bolle figlie xchè mando già via i dati tramite la mamma
055900990602     C     KLBL          CHAIN     FNLBL01L                           23
056000921026     C*
056100040319     C*  Se non è una bolla dirottata in quanto non è figlia di
056200040319     C*   un'altra bolla......
056300990602     C     *IN23         IFEQ      '1'
056400040319     C*   Oppure.....
056500040319     C*   .... se trovato il legame deve essere un dirottamento legato
056600040319     C*       non all'Estero e deve essere un discorso di reso/dirottam
056700040319     C*        Italia
056800040319     C     *in23         oreq      *off
056900040319     C     wtpINV        andne     'E'
057000040319     C     §3kcsd        andne     'EW'
057100040319     C     lblLAN        andne     lblLAP
057200040319     C     blpCCA        andne     *blank
057300040319     C*
057400050309     C     KBLP          CHAIN     tigcp000                           33
057500990602     C* 33 OFF - GIACENZA
057600961204     C*  Se giacenza bolla estero mando la data giacenza solo quando
057700961204     C*  l'ufficio internazionale mi ha scritto l'ulteriore motivaz.
057800961204     C*  in inglese
057900971126     C     *IN33         IFEQ      '0'
058000971126     C                   MOVEL     *BLANKS       ORGFL1
058100971126     C     GCPLNP        CHAIN     AZORG01L                           36
058200020717     C  n36              MOVEL     ORGDE3        OG143
058300020717     C  n36§OgNTW        ifeq      'FED'
058400020717     C     §OgNTW        oreq      'EEX'
058500020717     C     §OgNTW        oreq      'EUP'
058600020717     C     GCPFAS        Iflt      15
058700961204     C                   SETON                                        33
058800020717     C                   End
058900961204     C                   END
059000971126     C                   END
059100941228     C*
059200990602     C* Controllo se esiste una pratica di danno aperta
059300990602     C                   EXSR      CRTCA
059400941228     C*
059500060921      **?Attenzione:  a fronte di Bolla con Bolle figlie legate su cui
059600060921      *   è presente il Contrassegno, si vuole inviare al cliente
059700060921      *   l'info del contrassegno sul VAC della bolla madre.
059800060921      **?  Regola: il contrassegno si trova sull'ultima figlia della catena
059900060921      * ? o sulla penultima se l'ultima è una Locale.
060000060921      *    Occorre agganciare il file Contrassegni trovando prima la
060100060921      *    chiave della bolla su cui si dovrebbe trovare il contrassegno.
060200060921     c                   exsr      ChecK_figlie
060300060921      *   All'uscita della Routine è impostata la chiave della bolla valida
060400060921      *   x verificare la presenza del contrassegno sulla spedizione.
060500060921      *
060600060921     C**** KBLP          CHAIN     Fiar9000                           34
060700060921     C     Klbl2         CHAIN     Fiar9000                           34
060800060921      *
060900941228     C     *IN34         IFEQ      *ON
061000051115     C                   CLEAR                   ar9CAS
061100051115     C                   CLEAR                   ar9VCA
061200051115     C                   CLEAR                   ar9TIC
061300941228     C                   ENDIF
061400941228     C*
061500000525      * Se la LNP della spedizione è una filiale POSTE richiamo
061600000525      * routine x reperire eventuale C/assegno e la data di giacenza
061700000525      * dalla data dell' evento
061800000526     C                   EXSR      POSTE
061900090408      *-------
062000090408      * ?  O x qualsiasi Variazione Bolla inerente alla Data Cons.Richiesta  ?
062100090408     c                   move      'N'           VariataOggi       1
062200090408     C     §3KCQ8        ifEQ      'S'
062300090408     C     KBLP          setll     FIARBF1C
062400090408     C     KBLP          reade     FIARBF1C
062500090408     C                   dow       not %Eof(FIARBF1C)
062600090408     c                   if        *in51
062700090408     c                   if        arbDTV = dateu
062800090408     c                   if        arbCVB = 'CR' or
062900090408     c                             arbCVB = 'CS'
063000090408     c                   move      'S'           VariataOggi
063100090408     c                   leave
063200090408     c                   end
063300090408     c                   end
063400090408     c                   end
063500090408     C     KBLP          reade     FIARBF1C
063600090408     C                   end
063700090408     C                   ENDIF
063800090408      *-------
063900000525      *
064000941228     C* CERCO EVENTO LASCIATO AVVISO
064100941228     C                   CLEAR                   WDLA
064200941228     C                   MOVEL     '2A'          COD
064300941228     C*
064400941228     C                   MOVE      BLPAAS        WAAS
064500990810     C     KEVB          SETGT     FNEVB000
064600990810     C     KEVB          READPE    FNEVB000                               34
064700941228     C*
064800941228     C     *IN34         DOWEQ     *OFF
064900941228     C                   MOVEL(P)  EVBCEV        KEY
065000941228     C*
065100941228     C     KTAB2         CHAIN     TABEL                              30
065200941228     C     *IN30         IFEQ      *OFF
065300040319     C                   MOVEL     TBLUNI        DS2A
065400941228     C     §2AFTC        IFEQ      'A'
065500941228     C                   Z-ADD     EVBDEV        WDLA              8 0
065600941228     C                   SETON                                        34
065700941228     C                   ENDIF
065800941228     C                   ENDIF
065900941228     C*
066000990810     C  N34KEVB          READPE    FNEVB000                               34
066100941228     C                   ENDDO
066200921026     C*
066300970121     C*  Preimposto a 'S' flag di invio VAC e x fleggare DT3
066400970721     C                   MOVEL     'N'           WRTVAG            1
066500970121     C                   MOVEL     'S'           WFLDT3            1
066600970121     C                   MOVEL     'S'           WINVAC            1
066700040319      *
066800970121     C     §3KNMC        IFEQ      'S'
066900970121     C     BLPDCM        ANDEQ     0
067000011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
067100060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
067200011002     C     *IN68         ANDEQ     '0'
067300040319      *
067400040319      * ....ma anche
067500040319      *   Si vuole creare il VAG x un reso dovuto a mancata consegna
067600040319      *
067700040319     C     §3kNMC        orEQ      'S'
067800040319     C     blpDCM        andGT     0
067900040319     C     blpCCA        andNE     *blank
068000040319     C     *in68         andEQ     *off
068100040319      *
068200970121     C                   EXSR      TSTNMC
068300970121     C                   END
068400001031     C*
068500001031     C*  CONTROLLA SE IMPOSTATO FLAG PER TRASMISSIONI DATI CONSEGNA
068600001031      *  PARZIALE E VERIFICA DELLE DATE
068700001106     C                   MOVEL     'N'           WTRPAR            1
068800001031     C     §3KCQ6        IFEQ      'S'
068900001031     C     BLPDCM        ANDEQ     0
069000001031     C     BLPDCP        ANDGT     0
069100011002      * INDICATORE 68 ACCESO RIGUARDA LE BOLLE RICAVATE DALL'ESTENSIONE
069200060214      * DI ar4 PER CUI NON SI DEVE CREARE IL VAG
069300011002     C     *IN68         ANDEQ     '0'
069400001031     C                   EXSR      PARZIA
069500001031     C                   END
069600980318     C*  Controllo se l'ultimo evento immesso è di mancata consegna
069700980318     C                   MOVEL     'N'           WTRMIC
069800060912     C                   MOVEL     'N'           WTRRIP
069900990602     C                   Z-ADD     0             WDTMIC            8 0
070000980406     C     §3KCQ3        IFEQ      'S'
070100980318     C     BLPDCM        ANDEQ     0
070200060912     C     §3KCQ1        orEQ      'S'
070300060912     C     BLPDCM        ANDEQ     0
070400980318     C                   MOVE      BLPAAS        WAAS
070500990810     C     KEVB          SETGT     FNEVB000
070600990810     C     KEVB          READPE    FNEVB000                               34
070700980318     C*
070800980318     C     *IN34         IFEQ      *OFF
070900060912     C     EVBCEV        ifEQ      'MIC'
071000060912     C     §3KCQ3        andeq     'S'
071100980318     C                   MOVEL     'S'           WTRMIC            1
071200990602     C                   Z-ADD     EVBDEV        WDTMIC
071300060912     c                   else
071400060912      *ripristino
071500060912     C     EVBCEV        ifeq      'RIP'
071600060912     C     EVBCEV        oreQ      'RDC'
071700060912     C                   MOVEL     'S'           WTRRip            1
071800060912     C                   endif
071900060912      *
072000060912     C                   endif
072100980318     C                   END
072200980318     C                   END
072300921026     C* DATA CONSEGNA
072400930809     C     §3KCQ1        IFEQ      'S'
072500921026     C     BLPDCM        ANDNE     0
072600080410      *
072700960729     C* CONSEGNA PARZIALE X EDI ESTERO
072800080417     C**** §3KCQ1        OREQ      'S'
072900080417     C**** §3KCSD        ANDEQ     'ES'
073000080417     C**** BLPDCM        ANDEQ     0
073100080417     C**** BLPDCP        ANDNE     0
073200080410      *
073300010202     C     §3KCQ1        OREQ      'S'
073400010202     C     §3KCSD        ANDEQ     'EW'
073500010202     C     BLPDCM        ANDEQ     0
073600010202     C     BLPDCP        ANDNE     0
073700080410      *
073800990604     C* CONSEGNA PARZIALE X EUROPOLITAN
073900080417     C**** §3KCQ1        OREQ      'S'
074000080417     C**** §3KCSD        ANDEQ     'EU'
074100080417     C**** BLPDCM        ANDEQ     0
074200080417     C**** BLPDCP        ANDNE     0
074300080410      *
074400930809     C* DATA LASCIATO AVVISO
074500930809     C     §3KCQ2        OREQ      'S'
074600941228     C     WDLA          ANDGT     0
074700080410      *
074800930809     C* ANOMALIE
074900930809     C     §3KCQ4        OREQ      'S'
075000930809     C     BLPCCA        ANDNE     *BLANKS
075100080410      *
075200990602     C* ANOMALIE DANNI
075300990602     C     §3KCQ4        OREQ      'S'
075400990602     C     DATMAX        ANDNE     0
075500080410      *
075600930809     C* GIACENZA
075700930809     C     §3KCQ5        OREQ      'S'
075800930809     C     *IN33         ANDEQ     *OFF
075900080410      *
076000970123     C* MANCATE CONSEGNE
076100970123     C     §3KNMC        OREQ      'S'
076200970123     C     BLPDCM        ANDEQ     0
076300080410      *
076400980318     C* Messa in consegna
076500980318     C     §3KCQ3        OREQ      'S'
076600980318     C     WTRMIC        ANDEQ     'S'
076700080410      *
076800060912     C* Messa in consegna
076900060912     C     §3KCQ1        OREQ      'S'
077000060912     C     WTRrip        ANDEQ     'S'
077100060912     C     BLPDCM        ANDEQ     0
077200080410      *
077300090408      * ?  O x qualsiasi Variazione Bolla dovuta alla Data cons.Richiesta  ?
077400080410     C     §3KCQ8        OREQ      'S'
077500090408     c     VariataOggi   andEQ     'S'
077600090408     C     BLPDCR        ANDGT     0
077700080721     C*
077800080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
077900080721      * >>>>>>>>  ?ATTENZIONE Test finale x Scrivere il VAC? <<<<<<<<
078000080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
078100921026     C*
078200970123     C* Scrivo Vac se non devo trasmettere i motivi di mancata
078300970123     C* consegna o se li devo trasmettere e li ho trovati
078400970123     C     §3KNMC        IFNE      'S'
078500970123     C     WINVAC        OREQ      'S'
078600990624     C* ANOMALIE DANNI
078700990624     C     §3KCQ4        OREQ      'S'
078800990624     C     DATMAX        ANDNE     0
078900990624     C* Messa in consegna
079000990624     C     §3KCQ3        OREQ      'S'
079100990624     C     WTRMIC        ANDEQ     'S'
079200060912     C* Evento di ripristino
079300060912     C     §3KCQ1        OREQ      'S'
079400060912     C     WTRrip        ANDEQ     'S'
079500080721      *
079600080721     C* O x qualsiasi Variazione Bolla e alla data richiesta
079700080721     C     §3KCQ8        OREQ      'S'
079800080721     C     BLPDCR        ANDGT     0
079900090408     c     VariataOggi   andEQ     'S'
080000080721      *
080100930809     C                   EXSR      CARBLR
080200970123     C                   END
080300080721     C* >>>>>>>> ------------------------------------------- <<<<<<<<
080400970123     C*
080500930809     C                   END
080600990602     C*
080700990602     C                   END
080800921026     C*
080900970123     C* Fleggo blp se non devo trasmettere i motivi di mancata
081000970123     C* consegna o se li devo trasmettere e li ho trovati
081100970123     C     §3KNMC        IFNE      'S'
081200090527      *
081300970123     C     WFLDT3        OREQ      'S'
081400090527      *
081500090527     C* ANOMALIE  (altrimenti i Dirottamenti non venivano flaggati
081600090527      *  fino ad un successivo Evento sulle bolle figlie)
081700090527     C     §3KCQ4        OREQ      'S'
081800090527     C     BLPCCA        ANDNE     *BLANKS
081900090527      *
082000990624     C* ANOMALIE DANNI
082100990624     C     §3KCQ4        OREQ      'S'
082200990624     C     DATMAX        ANDNE     0
082300990624     C* Messa in consegna
082400990624     C     §3KCQ3        OREQ      'S'
082500990624     C     WTRMIC        ANDEQ     'S'
082600001106     C* CONSEGNA PARZIALE
082700001106     C     §3KCQ6        OREQ      'S'
082800001106     C     WTRPAR        ANDEQ     'S'
082900080729     C* O x qualsiasi Variazione Bolla e alla data richiesta
083000080729     C     §3KCQ8        OREQ      'S'
083100080729     C     BLPDCR        ANDGT     0
083200090408     c     VariataOggi   andEQ     'S'
083300080417      *
083400921026     C                   MOVEL     'T'           BLPFT3
083500941228     C                   MOVEL     DATEU         BLPDT3
083600080417      *
083700010927     C  N68              UPDATE    FNBLP000
083800010927     C   68              UPDATE    FNBLP1
083900970123     C                   END
084000921026     C*
084100921026     C                   ENDSR
084200000526     C*
084300060921      **?-------------------------------------------------------------*
084400060921      * Controlla la catena delle Figlie x reperire info su Contrassegno
084500060921      **?-------------------------------------------------------------*
084600060921     C     ChecK_Figlie  BEGSR
084700060921      *
084800060921      *  inizializza flag di work della Routine
084900060921     c                   move      'S'           Primo_legame      1
085000060921      *
085100060921      **?chiave salvata della Bolla x partire nella ricerca
085200060921     c                   z-add     blpAAS        klb2_AAS
085300060921     c                   z-add     blpLNP        klb2_LNP
085400060921     c                   z-add     blpNRS        klb2_NRS
085500060921     c                   z-add     blpNSP        klb2_NSP
085600060921      *
085700060921      *  Chiave Bolla Precedente
085800060921     c     new_ciclo     tag
085900060921      **?                                  Flag di work del ciclo
086000060921     c                   move      'N'           ha_figlia         1
086100060921     C     Klbl2         setLL     FnLBL02L
086200060921     C     Klbl2         reade     FnLBL02L
086300060921      *
086400060921     c                   Dow       not %EoF(FnLBL02L)
086500060921      **?primo controllo:
086600060921      *    tutta la catena deve partire dalla bolla mamma che è
086700060921      *     uguale alla bolla originale del primo record della catena
086800060921     c                   if          Primo_legame = 'S'
086900060921     c                   eval      Primo_legame = 'N'
087000060921     c                   if          blpAAS <> exLBLAAo   or
087100060921     c                               blpLNP <> exLBLLPo   or
087200060921     c                               blpNRS <> exLBLNRo   or
087300060921     c                               blpNSP <> exLBLNSo
087400060921      *    se così non è non deve saltare il ciclo e non impostare i
087500060921      *     campi chiave x il contrassegno.
087600060921     c                   leave
087700060921     c                   end
087800060921     c                   end
087900060921      **?secondo controllo:
088000060921      *     non deve essere una bolla locale
088100060921      *   (sulle locali non c'è l'informazione di contrassegno)
088200060921     c                   if        exLBLLPN <> exLBLLPP
088300060921      *  imposta la chiave dell'ultima figlia
088400060921      *       per il prossimo giro di controllo
088500060921     c                   z-add     exLBLAAN      klb2_AAS
088600060921     c                   z-add     exLBLLPN      klb2_LNP
088700060921     c                   z-add     exLBLNRN      klb2_NRS
088800060921     c                   z-add     exLBLNSN      klb2_NSP
088900060921     c                   move      'S'           ha_figlia
089000060921     c                   end
089100060921      *
089200060921      **?   (attenzione a non sposizionarsi)
089300060921      * Chiave impostata come Bolla Precedente appena letta
089400060921     C     KLBL2P        reade     FnLBL02L
089500060921     c                   end
089600060921      *
089700060921      *  se all'uscita del ciclo ha trovato almeno una figlia,
089800060921      *   deve reiniziare il ciclo per cercare l'ultima figlia valida
089900060921      *   se invece non ha trovato delle figlie allora va a fine con la
090000060921      *   chiave salvata sull'ultima bolla valida x il contrassegno.
090100060921     c                   if        ha_figlia = 'S'
090200060921     c                   if        exLBLAAp <> klb2_AAS or
090300060921     c                             exLBLLPp <> klb2_LNP or
090400060921     c                             exLBLNRp <> klb2_NRS or
090500060921     c                             exLBLNSp <> klb2_NSP
090600060921     c                   goto      new_ciclo
090700060921     c                   end
090800060921     c                   end
090900060921      *
091000060921      **?  Alla fine della Routine la chiave contiene la bolla da testare
091100060921      **?   x verificare la presenza del contrassegno su AR9
091200060921      *
091300060921     C                   ENDSR
091400060921      **?-------------------------------------------------------------*
091500000526     C* ROUTINE PER SPEDIZIONI DELLE POSTE ---------------------------*
091600921026     C*
091700000526     C     POSTE         BEGSR
091800000526      *
091900000526     C                   CLEAR                   PTTIC
092000000526     C                   CLEAR                   PTCAS
092100000526     C                   CLEAR                   PTVCA
092200000526     C                   CLEAR                   PTIAS
092300000526     C                   CLEAR                   PTVAS
092400000526     C                   CLEAR                   PTDTG
092500000526     C                   CLEAR                   §POST
092600000526      *
092700000526     C                   SETOFF                                       22
092800000526     C     BLPLNP        LOOKUP    FIP                                    22
092900000526     C   22              MOVEL     'S'           §POST             1
093000000526     C     §POST         IFEQ      'S'
093100000526      * Se la bolla è stata chiusa con consegna anomala la merce
093200000526      * dovrebbe essere stata data alla SDA . Se c'era il C/assegno
093300000526      * questo è stato annullato in quanto non più incassabile da
093400000526      * Bartolini.
093500000526      * Nel FNVAC però il C/Ass lo devo mettere ugualmente in quanto
093600000526      * informazione da ritornare alla SDA
093700000526     C     BLPCCA        IFNE      *BLANK
093800051115     C     ar9CAS        ANDEQ     *ZEROS
093900000526     C     KBLP          CHAIN     FNARBK00                           22
094000000526     C     *IN22         IFEQ      '0'
094100000526     C                   MOVEL     ARBTIC        PTTIC
094200000526     C                   Z-ADD     ARBCAS        PTCAS
094300000526     C                   MOVEL     ARBVCA        PTVCA
094400000526     C                   ENDIF
094500000526     C                   ENDIF
094600000526      *
094700060214      * Prendo importo da assicurare da Fiar4 t.r.=J
094800000526      *
094900000526     C                   MOVEL     'J'           KTRC
095000060214     C     Kar4          CHAIN     Fiar401L                           35
095100000526     C     *IN35         IFEQ      '0'
095200060214     C                   MOVEL     ar4NOT        DSBL4J
095300000526     C                   Z-ADD     §B4IAS        PTIAS
095400000526     C                   MOVEL     §B4VAS        PTVAS
095500000526     C                   ENDIF
095600000526      *
095700000526      * Calcolo la data di giacenza dagli eventi poichè non vengono
095800000526      * gestite le giacenze per le bolle poste
095900000526      *
096000000526     C     KEVB          SETGT     FNEVB000
096100000526     C     KEVB          READPE    FNEVB000                               34
096200000526      * Devo cercare l'ultimo evento di tipo giacenza
096300000526      *
096400000526     C     *IN34         DOWEQ     *OFF
096500000526     C                   Z-ADD     1             WW
096600000526     C     EVBCEV        LOOKUP    C2A(WW)                                42
096700000526     C     *IN42         IFEQ      '1'
096800000526     C     T2A(WW)       ANDEQ     'G'
096900000526     C                   Z-ADD     EVBDEV        PTDTG
097000000619     C                   LEAVE
097100000526     C                   ELSE
097200000526     C     KEVB          READPE    FNEVB000                               34
097300000526     C                   ENDIF
097400000526      *
097500000526     C                   ENDDO
097600000526      *
097700000526     C                   ENDIF
097800000526      *
097900000526     C                   ENDSR
098000000526     C*
098100941228     C* CREO RECORD FNVAC01L -----------------------------------------*
098200921026     C     CARBLR        BEGSR
098300080417      *
098400080417     C***  KBLP          CHAIN     FNVAC01T                           32
098500080417      *
098600080417     c                   clear                   fnvacds
098700080417      *
098800941228     C                   MOVEL     BLPAAS        VACAAS
098900941228     C                   MOVEL     BLPLNP        VACLNP
099000941228     C                   MOVEL     BLPMGS        VACMGS
099100941228     C                   MOVEL     BLPNRS        VACNRS
099200941228     C                   MOVEL     BLPNSP        VACNSP
099300941228     C                   MOVEL     BLPCBO        VACCBO
099400941228     C                   MOVEL     BLPLNA        VACLNA
099500941228     C                   MOVEL     BLPRSD        VACRSD
099600941228     C                   MOVEL     BLPPRD        VACPRD
099700941228     C                   MOVEL     BLPGC1        VACGC1
099800941228     C                   MOVEL     BLPGC2        VACGC2
099900941228     C                   MOVEL     BLPCTR        VACCTR
100000941228     C                   MOVEL     BLPCTS        VACCTS
100100941228     C                   MOVEL     BLPFTM        VACFTM
100200941228     C                   MOVEL     BLPFIN        VACFIN
100300941228     C                   MOVEL     BLPFAP        VACFAP
100400941228     C                   MOVEL     BLPTSP        VACTSP
100500000526      *
100600000526     C     §POST         IFEQ      'S'
100700000526     C                   MOVEL     PTIAS         VACIAS
100800000526     C                   MOVEL     PTVAS         VACVAS
100900000526     C                   ELSE
101000000526     C                   MOVEL     BLPIAS        VACIAS
101100000526     C                   MOVEL     BLPVAS        VACVAS
101200000526     C                   ENDIF
101300000526      *
101400941228     C                   MOVEL     BLPNAS        VACNAS
101500941228     C                   MOVEL     BLPNCL        VACNCL
101600941228     C                   MOVEL     BLPPKF        VACPKB
101700941228     C                   MOVEL     BLPVLF        VACVLB
101800941228     C                   MOVEL     BLPQFT        VACQFT
101900000526      *
102000000526     C     §POST         IFEQ      'S'
102100051115     C     ar9CAS        ANDEQ     *ZEROS
102200000526     C                   MOVEL     PTCAS         VACCAS
102300000526     C                   MOVEL     PTVCA         VACVCA
102400000526     C                   MOVEL     PTTIC         VACTIC
102500000526     C                   ELSE
102600051115     C                   MOVEL     ar9CAS        VACCAS
102700051115     C                   MOVEL     ar9VCA        VACVCA
102800051115     C                   MOVEL     ar9TIC        VACTIC
102900000526     C                   ENDIF
103000000526      *
103100060214      * FORZO SCRITTURA DEL CODICE CLIENTE DELL'ESTENSIONE SU ar4
103200011002      * SE STO LEGGENDO IL FILE DI JOIN BOLLE ED ESTENSIONE
103300011002      *
103400011002     C     *IN68         IFEQ      '1'
103500060214     C                   MOVEL     ar4KSC        VACCCM
103600011002     C                   ELSE
103700921026     C     BLPCCM        IFNE      0
103800941228     C                   MOVEL     BLPCCM        VACCCM
103900921026     C                   ELSE
104000941228     C                   MOVEL     BLPKSC        VACCCM
104100921026     C                   END
104200011002     C                   END
104300941228     C                   MOVEL     BLPRMN        VACRMN
104400941228     C                   MOVEL     BLPRMA        VACRMA
104500941228     C                   MOVEL     BLPRMO        VACRMO
104600941228     C                   MOVEL     BLPFFD        VACFFD
104700000921      * SE POSTE "NR. SEGNACOLLO DA" -->    VACDRC
104800000921     C     §POST         IFEQ      'S'
104900000921     C                   Z-ADD     BLPNCD        VACDCR
105000000921     C                   ELSE
105100000921     C                   MOVEL     BLPDCR        VACDCR
105200000921     C                   ENDIF
105300941228     C                   MOVEL     BLPTCR        VACTCR
105400941228     C                   MOVEL     BLPHCR        VACHCR
105500941228     C                   MOVEL     BLPTC1        VACTC1
105600941228     C                   MOVEL     BLPTC2        VACTC2
105700941228     C                   MOVEL     BLPDCM        VACDCM
105800941228     C                   MOVEL     BLPHMC        VACHMC
105900941228     C                   MOVEL     BLPCCA        VACCCA
106000941228     C                   MOVEL     WDLA          VACDLA
106100000526      *
106200000526     C     §POST         IFEQ      'S'
106300000526     C                   MOVEL     PTDTG         VACDAG
106400000526     C                   ELSE
106500941228     C  N33              Z-ADD     GCPMGC        VACDAG
106600941228     C  N33              MOVEL     GCPAGC        VACDAG
106700941228     C   33              MOVEL     *ZEROS        VACDAG
106800000526     C                   ENDIF
106900000526      *
107000990602     C*---------------------------------------------------------------*
107100990602     C* Se ho trovato un evanto di messa in consegna....
107200060912 1   C     WTRMIC        IFEQ      'S'
107300990602     C* ... e non ho la data di consegna totale sulla bolla ..
107400990602     C     BLPDCM        ANDEQ     0
107500040319      *
107600990602     C* Verifico se ho trovato una pratica C.A. aperta ...
107700060912 2   C     DATMAX        IFNE      0
107800990602     C     §3KCQ4        ANDEQ     'S'
107900990602     C* .. e se l'apertura di quest'ultima è successiva alla
108000990602     C*   messa in consegna ...
108100990602     C     DATMAX        ANDGE     WDTMIC
108200990602     C* .. allora imposto la data apertura C.A. nella data consegna
108300990602     C*    e imposto il tipo consegna anomala = 'A'
108400990602     C                   Z-ADD     1200          VACHMC
108500990602     C                   Z-ADD     DATMAX        VACDCM
108600990602     C                   MOVEL     'A'           VACCCA
108700060912 x2  C                   ELSE
108800990602     C* ... altrimenti imposto data messa in consegna
108900990602     C                   Z-ADD     EVBHEV        VACHMC
109000980319     C                   Z-ADD     EVBDEV        VACDCM            8 0
109100990602     C                   MOVEL     'C'           VACCCA
109200060912 e2  C                   END
109300990602     C*
109400990602     C* ... se è stata impostata la data di consegna sulla bolla ..
109500060912 x1  C                   ELSE
109600040319     C*
109700060912 2   C     BLPDCM        IFNE      0
109800040319     C*
109900040319     C* Un tempo era stato asteriscato questo pezzo
110000040319     C* >>>>>>>>>>>>>
110100040409      * non deve essere fatto per l'estero
110200060912 3   C     wtpINV        Ifne      'E'
110300040409      *
110400990602     C* ... verifico se ho trovato una C.A. aperta ...
110500060912 4   C     DATMAX        IFNE      0
110600040319     C     §3KCQ4        ANDEQ     'S'
110700990602     C* .. e se l'apertura di quest'ultima è successiva alla
110800990602     C* .. consegna ...
110900040319     C     DATMAX        ANDGT     BLPDCM
111000040319     C*
111100990602     C* .. allora imposto la data apertura C.A. nella data consegna
111200990602     C*    e imposto il tipo consegna anomala = 'A'
111300040319     C                   Z-ADD     1200          VACHMC
111400040319     C                   Z-ADD     DATMAX        VACDCM
111500040319     C                   MOVEL     'A'           VACCCA
111600040319     C*
111700060912 e4  C                   ENDif
111800060912 e3  c                   end
111900040319     C* >>>>>>>>>>>>>
112000040319     C*
112100990602     C* ... se non è stata effettuata la messa in consegna, la
112200990602     C*     bolla risulta non consegnata, ....
112300060912 x2  C                   ELSE
112400990602     C*     c'è una pratica di danno aperta e devo inviare al cliente
112500990602     C*     le consegne anomale, imposto la data consegna + flag
112600990602     C*     consegna anomala='A'
112700060912 3   C     DATMAX        IFNE      0
112800990602     C     §3KCQ4        ANDEQ     'S'
112900990602     C                   Z-ADD     1200          VACHMC
113000990602     C                   Z-ADD     DATMAX        VACDCM
113100990602     C                   MOVEL     'A'           VACCCA
113200060912 e3  C                   END
113300060912 e2  C                   END
113400060912 e1  C                   END
113500060912     C* Se ho trovato un evento di ripristino
113600060912 1   C     WTRrip        IFEQ      'S'
113700060912     C     BLPDCM        ANDEQ     0
113800060912     C     §3KCQ1        ANDEQ     'S'
113900060912     C                   Z-ADD     0             VACHMC
114000060912     C                   Z-ADD     0             VACDCM
114100060912     C                   MOVEL     'R'           VACCCA
114200060912 e1  c                   endif
114300060912
114400080417     C***  N32              UPDATE    FNVAC000
114500080417     C***   32              WRITE     FNVAC000
114600080417     c                   exsr      Write_VAC
114700930603     C                   SETON                                        06
114800990604     C*
114900990604     C* Se ho appena scritto un record per l'apertura C.A. di una bolla
115000990618     C* e questa è stata anche.....
115100060912 1   C     §3KCQ1        IFEQ      'S'
115200080417     C     §3KCSD        ANDEQ     'EW'
115300080417      *
115400080417     C**** §3KCQ1        OREQ      'S'
115500080417     C**** §3KCSD        ANDEQ     'ES'
115600080417     C**** §3KCQ1        OREQ      'S'
115700080417     C**** §3KCSD        ANDEQ     'EU'
115800080417      *
115900060912 2   C     VACCCA        IFEQ      'A'
116000990618     C* consegnata parzialmente scrivo un'altro record x l'eventuale
116100990618     C* invio al partner dell'informazione della consegna parziale
1162000609123    C     BLPDCM        IFEQ      0
116300990618     C     BLPDCP        ANDNE     0
116400990618     C                   Z-ADD     0             VACHMC
116500990618     C                   Z-ADD     0             VACDCM
116600990618     C                   MOVEL     ' '           VACCCA
116700080417     C***                   WRITE     FNVAC000
116800080417     c                   exsr      Write_VAC
116900060912x3   C                   ELSE
117000990618     C* è stata messa in consegna scivo un'altro record x l'eventuale
117100990618     C* invio al partner dell'informazione
117200060912 4   C     BLPDCM        IFEQ      0
117300990624     C     WDTMIC        ANDGT     0
117400990618     C                   Z-ADD     EVBHEV        VACHMC
117500990618     C                   Z-ADD     EVBDEV        VACDCM            8 0
117600990618     C                   MOVEL     'C'           VACCCA
117700080417     C***                   WRITE     FNVAC000
117800080417     c                   exsr      Write_VAC
117900060912e4   C                   END
118000060912e3   C                   END
118100990618     C*
118200990618     C* x estero: se ho sia la data consegna che la data apertura C.A.
118300990618     C* scrivo anche record x apertura C.A.
118400060912x2   C                   ELSE
1185000609123    C     DATMAX        IFNE      0
118600990618     C     §3KCQ4        ANDEQ     'S'
118700990618     C                   Z-ADD     1200          VACHMC
118800990618     C                   Z-ADD     DATMAX        VACDCM
118900990618     C                   MOVEL     'A'           VACCCA
119000080417     C***                   WRITE     FNVAC000
119100080417     c                   exsr      Write_VAC
119200060912 e3  C                   END
119300060912 e2  C                   END
119400060912 e1  C                   END
119500990618     C*
119600921026     C                   ENDSR
119700990602     C*---------------------------------------------------------------*
119800990602     C* Controllo se esiste una pratica C.A. aperta                  -*
119900990602     C*---------------------------------------------------------------*
120000990602     C     CRTCA         BEGSR
120100990602     C*
120200990602     C* Aggancio archivio pratiche di danno e eseguo loop x data spediz.
120300990602     C* per reperire la pratica di danno con data più alta
120400990602     C                   Z-ADD     0             DATMAX            8 0
120500990602     C     §3KCQ4        IFEQ      'S'
120600040715      **
120700040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
120800040715      * SIA COME MAMMA CHE COME FIGLIA
120900040715      **
121000040715     c                   clear                   fidn12ds
121100040715     c                   eval      i12aas = blpAAS
121200040715     c                   eval      i12lnp = blpLNP
121300040715     c                   eval      i12nrs = blpNRS
121400040715     c                   eval      i12nsp = blpNSP
121500040715     c                   eval      i12fel = 'E'
121600040715     c                   eval      i12fan = 'E'
121700040715     c                   eval      i12fch = 'E'
121800040715     c                   call      'FIDN12R'
121900040715     c                   parm                    fidn12ds
122000040715     c                   z-add     *zeros        ii                2 0
122100040715      **
122200040715      * prende fra le C.A. aperte quella con la data ultima
122300040715     c                   if        o12nca > 0
122400040715     c                   dow       ii <  o12nca
122500040715      **
122600040715     c                   add       1             ii
122700040715     C                   movel     skDca(ii)     datape            8 0
122800040715     C     DATMAX        IFLT      DATAPE
122900040715     C                   Z-ADD     DATAPE        DATMAX
123000040715     C                   END
123100040715      *
123200040715     c                   enddo
123300040715     c                   endif
123400040715      ****
123500040715     C**** KDCT          CHAIN     FNDCT02L                           21
123600040715     C**** *IN21         DOWEQ     '0'
123700040715     C**** DCTATB        IFEQ      ' '
123800040715     C**** DCTDCH        ANDEQ     0
123900040715     C****               Z-ADD     DCTMGC        DATAPE            8 0
124000040715     C****               MOVEL     DCTAAC        DATAPE
124100040715     C**** DATMAX        IFLT      DATAPE
124200040715     C****               Z-ADD     DATAPE        DATMAX
124300040715     C****               END
124400040715     C****               END
124500040715     C**** KDCT          READE     FNDCT02L                               21
124600040715     C****               END
124700040715      *
124800040715     C                   END
124900040715     C*
125000990602     C                   ENDSR
125100970122     C*---------------------------------------------------------------*
125200970122     C* Testo operazioni fare se devo inviare mancate consegne       -*
125300970122     C*---------------------------------------------------------------*
125400970121     C     TSTNMC        BEGSR
125500040422     **
125600040422     ** avendo fatto entrare anche x data consegna GT di 0
125700040422     ** il flag deve essere resettato solo per la vecchia condizione
125800040422     ** altrimenti non scrive + il VAC come una volta.
125900040422     c                   if        blpdcm = 0
126000040422     **
126100970122     C                   MOVEL     'N'           WFLDT3
126200090223     C** Dopo i controlli incrociati anche con il VAG si vuole da adesso in poi
126300090223     C**   sempre il VAC.
126400090223     C********           MOVEL     'N'           WINVAC
126500040422     **
126600040422     c                   end
126700040422     **
126800970122     C                   MOVEL     'N'           WTREVE            1
126900970122     C                   MOVEL     *BLANKS       WSVTMC            1
127000970122     C                   MOVEL     *BLANKS       WMOT1            35
127100970122     C                   MOVEL     *BLANKS       WMOT2            35
127200970122     C*  Leggo le estensioni bolla della spedizione e per quelle
127300970122     C*  relative ad un motivo di mancata consegna scrivo VAG
127400970122     C                   DO        Y             W
127500970122     C                   MOVEL     T7K(W)        WTMC              1
127600970122     C                   MOVE      T7K(W)        KTRC
127700060214     C     Kar4          CHAIN     Fiar401L                           35
127800970123     C     *IN35         IFEQ      '0'
127900060214     C     ar4FTR        ANDNE     'T'
128000060214     C     ar4FTR        ANDNE     'C'
128100970122     C*  Se ho trovato estensione bolla e non è trasmesso
128200970122     C*  controllo se ho un nuovo tp.mancata consegna
128300970122     C*  e se si reperisco evento corrispondente
128400970122     C     WTMC          IFNE      WSVTMC
128500970122     C                   EXSR      REPEVE
128600970122     C                   END
128700970122     C*  se ho trovato l'evento controllo se devo scrivere
128800970122     C*  nuovo record VAG:
128900970122     C     WTREVE        IFEQ      'S'
129000970122     C*  - se è cambiato tipo mancata consegna --> new rcd.
129100970122     C*  - se ho già scritto 2 motivi --> new record
129200970122     C     WTMC          IFNE      WSVTMC
129300970122     C     WMOT2         ORNE      *BLANKS
129400970124     C*  Se non è prima volta scrivo record VAG x mot.precedente
129500970124     C     WSVTMC        IFNE      *BLANKS
129600970124     C     WMOT2         ANDEQ     *BLANKS
129700970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
129800970318     C*  primi 15 chr nella seconda motivazione
129900970318     C     WCHR15        IFNE      *BLANKS
130000970318     C                   MOVEL     WCHR15        VAGNOT
130100970318     C                   END
130200080403     c*****              movel     fnvagds       vgddta
130300080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
130400050606     c                   move      'VG'          vgdtip
130500091116     c******             move      §3kcks        vgdksu
130600091116     c                   move      §3kgks        vgdksu
130700050627     c                   movel     0             vgdksu
130800091116     c******             move      §3kcsd        vgdtsc
130900091116     c                   move      §3kgsd        vgdtsc
131000050608     c                   eval      vgdpgm = 'FNLSA9R'
131100050627     c                   z-add     dateu         vgddat
131200091116     c                   if        §3kgsd  <> '  '
131300050606     C                   WRITE     tivgd000
131400091116     c                   end
131500970124     C                   SETON                                        06
131600970124     C                   END
131700970124     C*  New record
131800970122     C                   EXSR      NEWVAG
131900970122     C                   ELSE
132000970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
132100970318     C*  primi 15 chr nella seconda motivazione + mot.aggiuntiva
132200970318     C     WCHR15        IFNE      *BLANKS
132300970318     C                   MOVEL     WCHR15        VAGNOT
132400060214     C                   MOVE      ar4NOT        VAGNOT
132500970318     C                   ELSE
132600970318     C*  Se codice generico occupo chr. da 32 posizione
132700970318     C     VAGCMC        IFEQ      'G  '
132800970318     C     VAGCMC        OREQ      'GEN'
132900060214     C                   MOVEL     ar4NOT        WCHR19           19
133000060214     C                   MOVE      ar4NOT        WCHR16           16
133100970318     C                   MOVE      WCHR19        VAGDMC
133200970318     C                   MOVEL     WCHR16        VAGNOT
133300970318     C                   ELSE
133400970318     C*  Altrimenti imposto solo ulteriore descrizione
133500060214     C                   MOVEL     ar4NOT        VAGNOT
133600970318     C                   END
133700970318     C                   END
133800060214     C                   MOVEL     ar4NOT        WMOT2
133900970122     C                   MOVEL     *BLANKS       WMOT1
134000050606     C*  Scrivo tivgd
134100080403     c*****              movel     fnvagds       vgddta
134200080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
134300050606     c                   move      'VG'          vgdtip
134400091116     c*****              move      §3kcks        vgdksu
134500091116     c                   move      §3kgks        vgdksu
134600050627     c                   movel     0             vgdksu
134700091116     c*****              move      §3kcsd        vgdtsc
134800091116     c                   move      §3kgsd        vgdtsc
134900050608     c                   eval      vgdpgm = 'FNLSA9R'
135000050627     c                   z-add     dateu         vgddat
135100091116     c                   if        §3kgsd  <> '  '
135200050606     C                   WRITE     tivgd000
135300091116     c                   end
135400970122     C                   SETON                                        06
135500970122     C                   END
135600970122     C*  Imposto flag x scrittura VAC e fleg. bolla a 'S'
135700970122     C                   MOVEL     'S'           WFLDT3
135800970122     C                   MOVEL     'S'           WINVAC
135900060214     C*  Fleggo Fiar4
136000060214     C                   MOVEL     'C'           ar4FTR
136100060214     C                   Z-ADD     DATEU         ar4DTR
136200060214     C                   EXCEPT    AGGar4
136300970122     C                   END
136400970122     C                   END
136500970122     C*
136600970122     C                   END
136700970122     C*  Controllo se mi è rimasto un VAG da scrivere
136800970122     C     WMOT1         IFNE      *BLANKS
136900970318     C*  Se il motivo mancata consegna supera i 50 chr riporto i
137000970318     C*  primi 15 chr nella seconda motivazione
137100970318     C     WCHR15        IFNE      *BLANKS
137200970318     C                   MOVEL     WCHR15        VAGNOT
137300970318     C                   END
137400050606     C*  Scrivo tivgd
137500080403     c*****              movel     fnvagds       vgddta
137600080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
137700050606     c                   move      'VG'          vgdtip
137800091116     c*****              move      §3kcks        vgdksu
137900091116     c                   move      §3kgks        vgdksu
138000050627     c                   movel     0             vgdksu
138100091116     c*****              move      §3kcsd        vgdtsc
138200091116     c                   move      §3kgsd        vgdtsc
138300050608     c                   eval      vgdpgm = 'FNLSA9R'
138400050627     c                   z-add     dateu         vgddat
138500091116     c                   if        §3kgsd  <> '  '
138600050606     C                   WRITE     tivgd000
138700091116     c                   end
138800970122     C                   SETON                                        06
138900970122     C                   END
139000970122     C*
139100970122     C                   ENDSR
139200970122     C*---------------------------------------------------------------*
139300970122     C* Controllo se esiste evento                                   -*
139400970122     C*---------------------------------------------------------------*
139500970122     C     REPEVE        BEGSR
139600970122     C*
139700970122     C                   MOVEL     'N'           WTREVE
139800970122     C*  Mi posiziono su file eventi
139900970122     C                   Z-ADD     BLPAAS        WAAS
140000990810     C     KEVB          SETGT     FNEVB01L
140100990810     C     KEVB          READPE    FNEVB01L                               34
140200970122     C*  Loop letture fino a fine file o travato evento
140300970122     C     *IN34         DOWEQ     '0'
140400970122     C     WTREVE        ANDEQ     'N'
140500040504      **
140600040504      **  Attenzione tipologie A16 e A23 devono essere ricondotte
140700040504      **  rispettivamente alle 016 e 023 poichè sono state introdotte
140800040504      **  le A16/A23 come varianti in programmi che precedono facendo
140900040504      **  sballare il ragionamento in questo pgm.
141000040504      **  Quindi CHIODO per cercare i codici sostituendoli x la LOOKUP.
141100040504      **
141200040504     c                   if        evbCEV = 'A16' or evbCEV = 'A23'
141300040504     c                   movel     '0'           evbCEV
141400040504     c                   end
141500040504      **
141600040504      **
141700970122     C*  Reperisco tipo mancata consegna evento letto
141800970122     C                   Z-ADD     1             WW                4 0
141900970122     C     EVBCEV        LOOKUP    C2A(WW)                                42
142000970122     C*  Controllo se tipo mancata consegna evento letto
142100970122     C*  è uguale a quella dell'estensione bolla
142200970122     C     *IN42         IFEQ      '1'
142300970122     C     T2A(WW)       ANDEQ     WTMC
142400970122     C                   MOVEL     'S'           WTREVE
142500970122     C                   ELSE
142600990810     C     KEVB          READPE    FNEVB01L                               34
142700970122     C                   END
142800970122     C                   END
142900970122     C*
143000970122     C                   ENDSR
143100970122     C*---------------------------------------------------------------*
143200970122     C* Nuovo record FNVAG                                           -*
143300970122     C*---------------------------------------------------------------*
143400970122     C     NEWVAG        BEGSR
143500970122     C*
143600050606     C                   CLEAR                   FNVAGds
143700970721     C                   MOVEL     'S'           WRTVAG            1
143800970318     C                   MOVEL     *BLANKS       WCHR25           25
143900970318     C                   MOVEL     *BLANKS       WCHR15           15
144000970122     C                   Z-ADD     BLPAAS        VAGAAS
144100970122     C                   Z-ADD     BLPLNP        VAGLNP
144200970122     C                   Z-ADD     BLPNRS        VAGNRS
144300970122     C                   Z-ADD     BLPNSP        VAGNSP
144400970122     C                   Z-ADD     BLPLNA        VAGLNA
144500990810     C                   MOVEL     EVBDEV        WAA2              4 0
144600970123     C                   MOVE      WAA2          VAGAGC
144700970122     C                   MOVE      EVBDEV        VAGMGC
144800060214     C                   MOVEL     ar4NOT        VAGCMC
144900060214     C                   MOVEL     ar4NOT        WMOT1
145000970122     C                   MOVEL     *BLANKS       WMOT2
145100970307     C                   Z-ADD     1             YY                4 0
145200970307     C     VAGCMC        LOOKUP    C2A(YY)                                42
145300970307     C     VAGCMC        IFNE      'G  '
145400970307     C     VAGCMC        ANDNE     'GEN'
145500970307     C                   MOVEL     D2A(YY)       VAGDMC
145600970318     C                   MOVE      D2A(YY)       WCHR25
145700970318     C                   MOVEL     WCHR25        WCHR15
145800970305     C                   ELSE
145900060214     C                   MOVE      ar4NOT        WCOM31           31
146000970122     C                   MOVEL     WCOM31        VAGDMC
146100970305     C                   END
146200970122     C                   MOVEL     BLPRMA        VAGRMA
146300970122     C                   Z-ADD     BLPRMN        VAGRMN
146400970122     C     BLPCCM        IFNE      0
146500970122     C                   MOVEL     BLPCCM        VAGSCM
146600970122     C                   ELSE
146700970122     C                   MOVEL     BLPKSC        VAGSCM
146800970122     C                   END
146900970123     C*
147000970123     C                   MOVEL     WTMC          WSVTMC
147100970122     C*
147200970122     C                   ENDSR
147300001031     C*---------------------------------------------------------------*
147400001031     C* DETTAGLIO CONSEGNE PARZIALI E SCRITTURA FNVAG                -*
147500001031     C*---------------------------------------------------------------*
147600001031     C     PARZIA        BEGSR
147700001031     C*
147800001031      * LOOP SU DETTAGLIO COLLI
147900001103     C                   Z-ADD     0             TOTCO
148000001031     C                   Z-ADD     0             CONS
148100001102     C                   Z-ADD     0             ULTIMA            8 0
148200001031     C     KBLP          SETLL     FNBLT01L
148300001031     C                   DO        *HIVAL
148400001102     C     KBLP          READE     FNBLT01L                               97
148500001031     C   97              LEAVE
148600001031      * CONTA CONSEGNATI E NON CONSEGNATI
148700001103     C     BLTDCM        IFGT      0
148800001031     C                   ADD       1             CONS              3 0
148900001031     C                   END
149000001103     C                   ADD       1             TOTCO             3 0
149100001031      * MEMORIZZA ULTIMA DATA DI CONSEGNA
149200001031     C     BLTDCM        IFGT      ULTIMA
149300001031     C                   Z-ADD     BLTDCM        ULTIMA
149400001031     C                   ENDIF
149500001031     C                   ENDDO
149600001102     C                   MOVE      CONS          CONSA             3
149700001103     C                   MOVE      TOTCO         TOTCOA            3
149800001031      *FINE LOOP SU DETTAGLIO COLLI SCRIVE FNVAG
149900001031      *
150000050606     C                   CLEAR                   FNVAGds
150100001031     C                   MOVE      BLPLNP        VAGLNP
150200001031     C                   MOVE      BLPAAS        VAGAAS
150300001031     C                   MOVE      BLPNRS        VAGNRS
150400001031     C                   MOVE      BLPNSP        VAGNSP
150500001107     C                   MOVEL     'P  '         VAGCMC
150600001031     C                   MOVE      BLPRMN        VAGRMN
150700001031     C                   MOVE      BLPRMA        VAGRMA
150800001031     C                   MOVE      BLPLNA        VAGLNA
150900001031      * IMPOSTA LE COSTANTI PER CAMPO NOTE E MOTIVO MANCATA CONSEGNA
151000001031     C                   MOVEL     COS(1)        VAGDMC
151100001103     C                   MOVEA     COS(2)        COM
151200001102     C                   MOVEA     CONSA         COM(22)
151300001103     C                   MOVEA     TOTCOA        COM(29)
151400001102     C                   MOVEA     COM           VAGNOT
151500001031      *
151600001031     C     BLPCCM        IFGT      0
151700001031     C                   Z-ADD     BLPCCM        VAGSCM
151800001031     C                   ELSE
151900001031     C                   Z-ADD     BLPKSC        VAGSCM
152000001031     C                   END
152100001031      *IMPOSTA DATI ULTIMA DATA CONSEGNA MEMORIZZATA
152200001031     C                   MOVEL     ULTIMA        VAGAGC
152300001031     C                   MOVE      ULTIMA        VAGMGC
152400001031      *
152500080403     c*****              movel     fnvagds       vgddta
152600080411     C                   eval      vgdDTA = %subst(FNVAGDS:1:%size(FNVAGDS))
152700050606     c                   move      'VG'          vgdtip
152800091116     c*****              move      §3kcks        vgdksu
152900091116     c                   move      §3kgks        vgdksu
153000050627     c                   movel     0             vgdksu
153100091116     c*****              move      §3kcsd        vgdtsc
153200091116     c                   move      §3kgsd        vgdtsc
153300050608     c                   eval      vgdpgm = 'FNLSA9R'
153400050627     c                   z-add     dateu         vgddat
153500091116     c                   if        §3kgsd  <> '  '
153600050606     C                   WRITE     tivgd000
153700091116     c                   end
153800001106     C                   MOVEL     'S'           WTRPAR
153900001031      *
154000001031     C                   ENDSR
154100080417      *------------------------------------------------------------------------*
154200080417      * VAC - Routine di scrittura file TIVGD (file VAS generico download)
154300080417      *------------------------------------------------------------------------*
154400080417     C     Write_VAC     BEGSR
154500080417      *
154600090529      *  >>>>>>>>>
154700090529      *  Se il cliente è abilitato al controllo sulla "3K" per non ricevere
154800090529      *   uguali informazioni di VAC consecutive, controlla l'ultima immagine
154900090529      *   inviata memorizzata sul FIAR500F tipo record "VAC" .
155000090529      *   se i campi della DS FNVAChgDS sono identici a quelli nel campo del
155100090529      *   FIAR500F (Ar5UNI) deve andare a fine Subroutine senza scrivere
155200090529      *   il record.
155300090529      *  >>>>>>>>>
155400090603     c                   if        §3kCTB ='S'
155500090603     c                   clear                   uguale_VAC        1
155600090529      *
155700090529     C                   movel     'VAC'         Kar5vac
155800090529     c     kar5          chain     fiar501l
155900090529      *
156000090529     c                   if        %Found(fiar501l)
156100090529      * Imposta i campi precedentemente salvati nella DS da comparare
156200090529     c                   eval      fnvacHGds = ar5UNI
156300090603      *
156400090603     c                   eval      VacGC1chg = VacGC1
156500090603     c                   eval      VacGC2chg = VacGC2
156600090603     c                   eval      VacFFDchg = VacFFD
156700090603     c                   eval      VacTCRchg = VacTCR
156800090603     c                   eval      VacDCRchg = VacDCR
156900090603     c                   eval      VacHCRchg = VacHCR
157000090603     c                   eval      VacDCMchg = VacDCM
157100090603     c                   eval      VacHMCchg = VacHMC
157200090603     c                   eval      VacTC1chg = VacTC1
157300090603     c                   eval      VacTC2chg = VacTC2
157400090603     c                   eval      VacCCAchg = VacCCA
157500090603     c                   eval      VacDLAchg = VacDLA
157600090603     c                   eval      VacDAGchg = VacDAG
157700090529      * quindi
157800090529      *    controllo di uguaglianza
157900090529     c                   if        fnvacHGds = DSvacHG
158000090529     c                   eval      uguale_VAC = 'S'
158100090603     c                   move      '='           ar5UNI
158200090603     c                   else
158300090603     c                   move      ' '           ar5UNI
158400090603     c                   eval      ar5UNI    = DSvacHG
158500090603     c                   end
158600090529      *
158700090529     c                   eval      AR5DAC = dateu
158800090529     c                   eval      AR5ORC = Orario
158900090529     c                   eval      AR5PRG = AR5PRG + 1
159000090529     c*
159100090529     c                   update    fiar5000
159200090529     c*
159300090529     c                   if        uguale_VAC = 'S'
159400090529     c                   leavesr
159500090529     c                   end
159600090529      *
159700090529     c                   else
159800090529      *
159900090529      *  lo memorizza x la prima volta
160000090529     c                   clear                   fiar5000
160100090529     c                   eval      ar5AAS = blpAAS
160200090529     c                   eval      ar5LNP = blpLNP
160300090529     c                   eval      ar5NRS = blpNRS
160400090529     c                   eval      ar5NSP = blpNSP
160500090529     c                   eval      ar5TRD = 'VAC'
160600090529     c                   eval      ar5DAC = dateu
160700090529     c                   eval      ar5ORC = Orario
160800090529     c                   eval      ar5PRG = AR5PRG + 1
160900090603      *
161000090603     c                   eval      VacGC1chg = VacGC1
161100090603     c                   eval      VacGC2chg = VacGC2
161200090603     c                   eval      VacFFDchg = VacFFD
161300090603     c                   eval      VacTCRchg = VacTCR
161400090603     c                   eval      VacDCRchg = VacDCR
161500090603     c                   eval      VacHCRchg = VacHCR
161600090603     c                   eval      VacDCMchg = VacDCM
161700090603     c                   eval      VacHMCchg = VacHMC
161800090603     c                   eval      VacTC1chg = VacTC1
161900090603     c                   eval      VacTC2chg = VacTC2
162000090603     c                   eval      VacCCAchg = VacCCA
162100090603     c                   eval      VacDLAchg = VacDLA
162200090603     c                   eval      VacDAGchg = VacDAG
162300090529     c                   eval      ar5UNI = DSvacHG
162400090529     c                   write     fiar5000
162500090529      *
162600090529     c                   end
162700090529     c                   end
162800090529      *  >>>>>>>>>
162900090529      *
163000080417     C                   clear                   tivgd000
163100080417     C                   eval      vgdDTA = %subst(FNVACDS:1:%size(FNVACDS))
163200080417     C                   eval      vgdTIP = 'VC'
163300080417     C                   movel     *all'0'       vgdKSU
163400080417     c                   move      §3kcks        vgdksu
163500080417      * tipo invio
163600080417     c                   move      §3kcsd        vgdtsc
163700080417     c                   eval      vgdpgm = 'FNLSA9R'
163800080417     c                   z-add     dateu         vgddat
163900080417      *
164000080417      *  x generare l'EDI:
164100080417      *    occorre farlo in modo controllato (personalizzato)
164200080417     c                   if        §3kcsd ='EW' and WTPINV = 'E'
164300080417     C                   clear                   DVGDFLO
164400080417     C                   movel     'P'           §VGDFELA
164500080417     C                   movel     DVGDFLO       vgdflo
164600080417     c                   end
164700080417      *
164800091116      *      Se codificato l'invio dei dati di consegna scrive altrimenti
164900091116      *  Non deve scrivere nulla.
165000091116     c                   if        §3kcsd <> *blank
165100080417     C                   write     tivgd000
165200110721      *  ed in ogni caso viene flaggato il BLP come inviato al cliente
165300110721      *     poichè non flaggandolo si trasmettevano più volte lo stesso VAC.
165400110721     C                   MOVEL     'S'           WFLDT3
165500091116     c                   end
165600080417      *
165700080417     C                   ENDSR
165800080930      *------------------------------------------------------------------------*
165900080930      * Routine di controllo x eventuali Errori
166000080930      *------------------------------------------------------------------------*
166100080930     C     *PSSR         BEGSR
166200080930      *
166300080930      * Comunque eseguo x rilasciare il TIVGD.
166400080930      *? Infine elimino il blocco elaborazione TIVGD x tipo file qui.  ?
166500080930     C                   clear                   trul47ds
166600080930     C                   eval      d47opz  = 'F'
166700080930     C                   eval      d47tip  = 'VC'
166800080930     C                   call      'TRUL47R'
166900080930     C                   parm                    trul47ds
167000080930      *
167100080930     C                   ENDSR     '*CANCL'
167200080417     C*------------------------------------------------------------------------*
167300060214     OFiar4000  E            AGGar4
167400060214     O                       ar4FTR
167500060214     O                       ar4DTR
167600001031**
167700941228OVRDBF FILE(FNVAC01T)           MBR(MXXXXXXXXX)
167800921026**
167900941228ALCOBJ OBJ((FNVAC01T *FILE *EXCL    MXXXXXXXXX))
168000921026**
168100941228DLCOBJ OBJ((FNVAC01T *FILE *EXCL    MXXXXXXXXX))
168200921026**
168300981210DLTOVR FILE(*ALL)
168400000726**
168500921026SNDMSG MSG('ATTENZIONE!!  LE BOLLE DI ALCUNI CLIENTI NON SONO STATE
168600921026 ELABORATE: RILANCIARE LA PROCEDURA DI  ''PREPARAZIONE  DATI  DI  R
168700921026ITORNO  AL  CLIENTE''  DA MENU''')                   TOUSR(*SYSOPR)
168800001031**
168900001031Consegna parziale
169000001031Nr. colli consegnati xxx su xxx
