000100970424     H*------------------------------------------------------------------------*
000200130226     H* TARIFFE DA CONTO ECONOMICO - CALCOLO DELTA BOLLE tariffe/offerte       *
000300970424     H*------------------------------------------------------------------------*
000400970424     H DECEDIT('0,') DATEDIT(*DMY.)
000500970424     F*------------------------------------------------------------------------*
000600970424     F* DATA BASE
000700970424     F*------------------------------------------------------------------------*
000800140521     Ftitas30c  IF   E           K DISK
000900060809     Ftncsb03L  IF   E           K DISK
001000080908     Ffnevb04l  if   e           k disk
001100140521     FFIAR531c  IF   E           K DISK
001200970424     FTABEL00F  IF   E           K DISK
001300011012     FTNTBE01L  IF   E           K DISK
001400130507     fWFDBF10F  o    e             disk    usropn
001500970424     D*-------------------
001600970424     D* MESSAGGI
001700970424     D*-------------------
001800970424     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900970424     D*-------------------
002000970424     D* VOCI VARIE DI TASSAZIONE
002100970424     D*-------------------
002200970424     D TSV             S              1    DIM(20)
002300020109     D TVA             S             11  3 DIM(20)
002400970424     D TEV             S              1    DIM(20)
002500080908     D Wcev            s                   like(evbcev) inz('RIC')
002600970424     D*-------------------
002700970424     D* CODICI E IMPORTI - ALFANUMERICI
002800970424     D*-------------------
002900030515     D CAL             S              2    DIM(75)
003000030515     D IAL             S              7    DIM(75)
003100141211
003200141211       //?Sk part.consegna con abilitazione PIN Code
003300141211     d skGMA           s              2a   dim(10)
003400141211
003500970424     D*-------------------
003600970424     D* DS PASSAGGIO PARAMETRI
003700970424     D*-------------------
003800970424     D DS50          E DS                  EXTNAME(TNTE50DS)
003900970424     D*-------------------
004000970424     D* DS CODICI BOLLA
004100970424     D*-------------------
004200970424     D DS3A          E DS
004300060904     D DStb          E DS
004400141211
004500141211       //?Ds tabella 7R - part.consegna
004600141211     d ds7R          e ds
004700141211
004800970424     D*-------------------
004900970424     D* DS TASSAZIONE BOLLA
005000970424     D*-------------------
005100061109     D DTAS01        E DS
005200060809     D DTAS          E DS                  inz
005300060811     D  dksc         E                     extfld(tasksc)
005400060811     D  dctr         E                     extfld(tasctr)
005500060811     D  dncl         E                     extfld(tasncl)
005600060811     D  dstasflo     E                     extfld(tasflo)
005700060809     D DTASv         E DS                  inz
005800060809     d  sv                     1     20
005900060809     d                                     Dim(20)                              Sigle varie
006000060809     d  va                    21    140P 3
006100060809     d                                     Dim(20)                              Importi varie
006200060825     d  vn             s            120
006300060811     d                                     Dim(20)                              Importi varie
006400151015     d Dtaspes       e ds                  inz
006500130301
006600130301      * - Ds per varie file di lavoro
006700130301     d                 ds
006800130301     d  WF1sv1                 1      1
006900130301     d  WF1sv2                 2      2
007000130301     d  WF1sv3                 3      3
007100130301     d  WF1sv4                 4      4
007200130301     d  WF1sv5                 5      5
007300130301     d  WF1sv6                 6      6
007400130301     d  WF1sv7                 7      7
007500130301     d  WF1sv8                 8      8
007600130301     d  WF1sv9                 9      9
007700130301     d  WF1s10                10     10
007800130301     d  WF1s11                11     11
007900130301     d  WF1s12                12     12
008000130301     d  WF1s13                13     13
008100130301     d  WF1s14                14     14
008200130301     d  WF1s15                15     15
008300130301     d  WF1s16                16     16
008400130301     d  WF1s17                17     17
008500130301     d  WF1s18                18     18
008600130301     d  WF1s19                19     19
008700130301     d  WF1s20                20     20
008800130301     d  WF1_sv                 1     20    Dim(20)                              Sigle varie
008900130301      * - Ds per varie file di lavoro
009000130301     d                 ds
009100130301     d  WF1va1                 1     11  3
009200130301     d  WF1va2                12     22  3
009300130301     d  WF1va3                23     33  3
009400130301     d  WF1va4                34     44  3
009500130301     d  WF1va5                45     55  3
009600130301     d  WF1va6                56     66  3
009700130301     d  WF1va7                67     77  3
009800130301     d  WF1va8                78     88  3
009900130301     d  WF1va9                89     99  3
010000130301     d  WF1v10               100    110  3
010100130301     d  WF1v11               111    121  3
010200130301     d  WF1v12               122    132  3
010300130301     d  WF1v13               133    143  3
010400130301     d  WF1v14               144    154  3
010500130301     d  WF1v15               155    165  3
010600130301     d  WF1v16               166    176  3
010700130301     d  WF1v17               177    187  3
010800130301     d  WF1v18               188    198  3
010900130301     d  WF1v19               199    209  3
011000130301     d  WF1v20               210    220  3
011100130301     d  WF1_va                 1    220  3 Dim(20)                              Importo varie
011200040806     D*-------------------
011300040806     D* DS AR5UNI TIP. REC. GEN
011400040806     D*-------------------
011500040806     D DAR5GEN       E DS
011600060809     D DAR5TRS       E DS
011700040806     D*-------------------
011800040806     D* DS TASFLO
011900040806     D*-------------------
012000040806     D DTASFLO       E DS
012100970424     D*------------------
012200970424     D* DS "TISI95R" - CONTROLLO CAP
012300970424     D*------------------
012400970424     D DSSI95        E DS                  EXTNAME(TISI95DS)
012500970424     D*-------------------
012600970424     D* DS CALCOLO COMPETENZE
012700970424     D*-------------------
012800970424     D DS$ECO        E DS
012900970424     D*-------------------
013000970424     D* DS RIDEFINIZIONE IMMAGINE PRIMA E DOPO DI TNTAS10F
013100970424     D*-------------------
013200970424     D TRECDS        E DS
013300970424     D  D$PDS1                 1    250
013400970424     D  D$PDS2               251    500
013500970424     D  D$DDS1               501    750
013600970424     D  D$DDS2               751   1000
013700970424     D  D$ODS1              1001   1250
013800970424     D  D$ODS2              1251   1500
013900030122     D*-------------------
014000030122     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
014100030122     D*-------------------
014200030122     D DAR5BAN       E DS
014300030729     D*-------------------
014400030729     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI BARTOLINI
014500030729     D*-------------------
014600030729     D DAR5BNB       E DS
014700970424     D*-------------------
014800970424     D* DS PASSAGGIO CAMPI PACKED -CODICE-
014900970424     D*-------------------
015000970424     D                 DS
015100970424     D  DSWCN                  1      3P 0
015200970424     D  DSWCA                  1      3
015300090907     D  DSWIN                  4     13P 5
015400020109     D  DSWIA                  4     13
015500970424     D*-------------------
015600970424     D* DS PER MESSAGGI
015700970424     D*-------------------
015800970424     D                 DS
015900970424     D  DSMSG                  1     78
016000970424     D  DSERR                 75     78
016100980826     D*-------------------
016200980826     D* DS PER COMPORRE DATA SPED.
016300980826     D*-------------------
016400980826     D                 DS
016500060809     D  tasAAS                 1      4  0
016600060809     D  tasMGS                 5      8  0
016700060809     D  tasDSPt                1      8  0
016800970424     D*-------------------
016900970424     D* ARCHITETTURA
017000970424     D*-------------------
017100970424     D KPJBA         E DS
017200011012     D*------------------
017300011012     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
017400011012     D*------------------
017500011012     D DSGEDCE       E DS
017600011012     D*-------------------
017700011012     D* CONVERSIONI IMPORTI
017800011012     D*-------------------
017900011012     D YEUR          E DS                  EXTNAME(YEURCODS)
018000011015     D*-------------------
018100011015     D* DS "XSRDA8" - CONTROLLO DATA (8)
018200011015     D*-------------------
018300011015     D WLBDA8          DS                  INZ
018400011015     D  G08DAT                 1      8  0
018500011015     D  G08INV                 9     16  0
018600011015     D  G08ERR                17     17
018700011015     D  G08TGI                18     22  0
018800130228
018900130228       // -?Data decorrenza tariffa/offerta
019000130228     d parddt          s              8
019100130228     d dataddt         s              8  0
019200130507
019300130507       // -?Flag booleani
019400130507     d wNoWf           s               n   inz(*off)
019500130507
019600130507       // -?Campi
019700130507     d partest         s              1
019800130507     d k               s              3  0
019900130228
020000060809     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS39C
020100060809     ITITAS000      01
020200060809     ITITAS010      02
020300140521     ITITASP00      03
020400970424     C*------------------------------------------------------------------------*
020500970424     C* MAIN LINE
020600970424     C*------------------------------------------------------------------------*
020700970424     C*
020800970424     C* OPERAZIONI INIZIALI
020900970424     C                   EXSR      RUTINZ
021000970424     C*
021100970424     C* SE LANCIATO CON TIPO LANCIO='C' -CHIUDE FILE-, VA A FINE PROGRAMMA
021200970424IF  1C     $ERR          IFEQ      '0'
021300970424IF  2C     D50TLA        IFNE      'C'
021400970424     C*
021500970424     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
021600970424IF  3C     $ERR          IFEQ      '0'
021700970424IF  4C     D50NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
021800060809     C                   EXSR      LETtas                                       *LEGGE BOLLA PART.
021900970424E   4C                   ENDIF
022000970424E   3C                   ENDIF
022100970424     C*
022200970424     C* CALCOLA I RICAVI
022300970424IF  3C     $ERR          IFEQ      '0'
022400970424     C                   EXSR      TASSAZ
022500970424E   3C                   ENDIF
022600970424     C*
022700970424     C* CALCOLA LE COMPETENZE
022800970424IF  3C     $ERR          IFEQ      '0'
022900060904     C*    §3ATB1        ANDEQ     'F1'
023000970424     C                   EXSR      CALCOM
023100971027E   3C                   ENDIF
023200970424     C*
023300970424     C* CALCOLA IL DELTA
023400970424     C                   Z-ADD     D50RIC        WRRW             16 3          *RICAVO REALE
023500970424     C                   Z-ADD     D50COM        WRPW             16 3          *RICAVO PRESUNTO
023600970424     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
023700970424     C                   EXSR      CALDEL
023800970424     C                   Z-ADD     WDPW          D50DEL                         *DELTA
023900970424     C*
024000970424E   2C                   ENDIF
024100970424E   1C                   ENDIF
024200970424     C*
024300970424     C* OPERAZIONI DI CHIUSURA
024400970424IF  1C     D50TLA        IFEQ      'C'
024500970424     C*
024600970424     C                   MOVEL     'C'           D$GSTS
024700970424     C                   CALL      'TRECD8R'                            98
024800970424     C                   PARM                    DS$ECO
024900970424     C*
025000970424     C                   MOVEL     'C'           TASTLA
025100151015     C                   CALL      'TNSF22R'                            99
025200970424     C                   PARM                    KPJBA
025300991126     C                   PARM                    DTAS
025400991126     C                   PARM                    DTASV
025500151015     c                   parm                    dtaspes
025600970424     C*
025700970424     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
025800970424     C                   MOVEL     'C'           I95TLA
025900970424     C                   CALL      'TISI95R'
026000970424     C                   PARM                    DSSI95
026100970424     C* FINE PROGRAMMA
026200970424     C                   SETON                                        LR
026300970424X   1C                   ELSE
026400970424     C                   SETON                                        RT
026500970424E   1C                   ENDIF
026600970424     C*------------------------------------------------------------------------*
026700060809     C* LETtas - LEGGE LE BOLLE PARTENZA
026800970424     C*------------------------------------------------------------------------*
026900060809     C     LETtas        BEGSR
027000970424     C*
027100060809     C                   Z-ADD     D50AAS        tasAAS
027200060809     C                   Z-ADD     D50LNP        tasLNP
027300060809     C                   Z-ADD     D50NRS        tasNRS
027400060809     C                   Z-ADD     D50NSP        tasNSP
027500060809     c                   setoff                                       0102
027600060809     C     KEYtas        CHAIN     titas30c                           99
027700970424IF  1C     *IN99         IFEQ      '1'                                          *NON TROV.
027800970424     C                   MOVEL     MSG(1)        $MSG                           *BOLLA NON TROV. O A
027900011102     C                   EXSR      ERRORE
028000970424E   1C                   ENDIF
028100970424     C*
028200970424     C* RECUPERA IL CODICE BOLLA
028300970424IF  1C     $ERR          IFEQ      '0'
028400970424     C                   Z-ADD     CODUT         KTBUTE
028500970424     C                   MOVEL     '3A'          KTBCOD
028600970424     C                   MOVEL     *BLANKS       KTBKEY
028700060809     C                   MOVEL     tasCBO        KTBKEY
028800970424     C     KEYTAB        CHAIN     TABEL00F                           99
028900970424IF  2C     *IN99         IFEQ      '1'
029000970424     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
029100970424     C                   EXSR      ERRORE
029200060904     c                   else
029300060904     C                   MOVEL     TBLUNI        DS3A
029400060904     C                   MOVEL     §3ATB1        FLGTB1            1
029500970424E   2C                   ENDIF
029600970424E   1C                   ENDIF
029700060904     c* solo tipi bolla Normali (F1 e A2)
029800060904IF  1C     $ERR          IFEQ      '0'
029900060904     C                   Z-ADD     CODUT         KTBUTE
030000060904     C                   MOVEL     'TB'          KTBCOD
030100060904     C                   MOVEL     *BLANKS       KTBKEY
030200060904     C                   MOVEL     §3ATB1        KTBKEY
030300060904     C     KEYTAB        CHAIN     TABEL00F                           99
030400060904IF  2C     *IN99         IFEQ      '1'
030500060904     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
030600060904     C                   EXSR      ERRORE
030700060904     c                   else
030800060904     C                   MOVEL     TBLUNI        DSTB
030900060904E   2C                   ENDIF
031000060904E   1C                   ENDIF
031100970424     C*
031200970424     C* CONTROLLA SE IL TIPO BOLLA E' CALCOLABILE
031300970424IF  1C     $ERR          IFEQ      '0'
031400060904IF  2C     §tbrbl        IFNE      'N'
031500970424     C                   MOVEL     MSG(4)        $MSG                           *TIPO BOLLA NON CALC
031600970424     C                   EXSR      ERRORE
031700970424E   2C                   ENDIF
031800970424E   1C                   ENDIF
031900970424     C*
032000970424     C* LEGGE I CONTRASSEGNI
032100061221     C                   CLEAR                   csbsta
032200061221     C                   CLEAR                   csbVCA
032300061221     C                   CLEAR                   csbcmb
032400061221     C                   CLEAR                   csbCAS
032500061221     C                   CLEAR                   csbtpi
032600061221     C                   CLEAR                   csbtpa
032700061221     C                   CLEAR                   csbfus
032800060809      * mittente
032900970424IF  1C     $ERR          IFEQ      '0'
033000970424     C     §3AFCA        ANDNE     *ZEROS                                       *HA CONTRASSEGNO
033100060809     C                   Z-ADD     D50AAS        csbAAS
033200060809     C                   Z-ADD     D50LNP        csbLNP
033300060809     C                   Z-ADD     D50NRS        csbNRS
033400060809     C                   Z-ADD     D50NSP        csbNSP
033500060809     C     KEYcsb        CHAIN     tncsb03l                           99
033600970424IF  2C     *IN99         IFEQ      '1'                                          *NON TROVATO
033700061221     C                   CLEAR                   csbsta
033800061221     C                   CLEAR                   csbVCA
033900061221     C                   CLEAR                   csbcmb
034000060809     C                   CLEAR                   csbCAS
034100061221     C                   CLEAR                   csbtpi
034200061221     C                   CLEAR                   csbtpa
034300060809     C                   CLEAR                   csbfus
034400970424E   2C                   ENDIF
034500970424E   1C                   ENDIF
034600030122     C*
034700030122     C* LEGGE IL FILE ESTENSIONI PER I BANCALI
034800030122     C                   CLEAR                   DAR5BAN
034900060809If  1c                   If        %Subst(tasGva:1:1) ='B'
035000030122IF  1C     $ERR          IFEQ      '0'
035100030122     C                   Z-ADD     D50AAS        AR5AAS
035200030122     C                   Z-ADD     D50LNP        AR5LNP
035300030122     C                   Z-ADD     D50NRS        AR5NRS
035400030122     C                   Z-ADD     D50NSP        AR5NSP
035500030122     C                   MOVEL     'BAN'         AR5TRD
035600060809     C                   CLEAR                   AR5PRG
035700060809     C     KEYAR5        CHAIN     FIAR531c                           99
035800030122IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
035900030122     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
036000030122     C                   MOVEL     AR5UNI        DAR5BAN
036100030122E   2C                   ENDIF
036200030122E   1C                   ENDIF
036300030729     c                   EndIf
036400030729     C* LEGGE IL FILE ESTENSIONI PER I BANCALI BARTOLINI
036500030729     c                   Clear                   dAr5Bnb
036600060809If  1c                   If        %Subst(tasGva:1:1) ='O'
036700030729IF  1C     $ERR          IFEQ      '0'
036800030729     C                   Z-ADD     D50AAS        AR5AAS
036900030729     C                   Z-ADD     D50LNP        AR5LNP
037000030729     C                   Z-ADD     D50NRS        AR5NRS
037100030729     C                   Z-ADD     D50NSP        AR5NSP
037200030729     c                   Eval      Ar5Trd = 'BNB'
037300060809     C                   CLEAR                   AR5PRG
037400060809     C     KEYAR5        CHAIN     FIAR531c                           99
037500030729IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
037600030729     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
037700030729     c                   Movel     AR5UNI        dAr5Bnb
037800030729E   2C                   ENDIF
037900030729E   1C                   ENDIF
038000030729     c                   EndIf
038100060809     C* LEGGO fiar500F x transiti
038200060809     C                   CLEAR                   DAR5TRS
038300060809    6C                   if        TASFLP > *zeros OR TASFL2 > *zeros
038400060809IF  1C     $ERR          IFEQ      '0'
038500060809     C                   MOVEL     'TRS'         AR5TRD
038600060809     C                   EVAL      AR5PRG = TASFLP
038700060809     C     KEYAR5        CHAIN     FIAR531C                           99
038800060809if  7C     *IN99         IFEQ      '0'
038900060809     C     AR5ATB        andeq     *BLANKS
039000060809     C                   MOVEL     AR5UNI        DAR5TRS
039100060809     C                   END
039200060809     C                   END
039300060809     C                   END
039400040806     C*
039500040806     C* LEGGE IL FILE ESTENSIONI PER LA MERCE DI VALORE
039600040806     C                   CLEAR                   DAR5GEN
039700040806IF  1C     $ERR          IFEQ      '0'
039800040806     C                   Z-ADD     D50AAS        AR5AAS
039900040806     C                   Z-ADD     D50LNP        AR5LNP
040000040806     C                   Z-ADD     D50NRS        AR5NRS
040100040806     C                   Z-ADD     D50NSP        AR5NSP
040200040806     C                   MOVEL     'GEN'         AR5TRD
040300151130     C                   CLEAR                   AR5PRG
040400060809     C     KEYAR5        CHAIN     fiar531c                           99
040500040806IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
040600040806     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
040700040806     C                   MOVEL     AR5UNI        DAR5GEN
040800040806E   2C                   ENDIF
040900040806E   1C                   ENDIF
041000970424     C*
041100970424     C                   ENDSR
041200970424     C*------------------------------------------------------------------------*
041300970424     C* TASSAZ - TASSAZIONE BOLLA
041400970424     C*------------------------------------------------------------------------*
041500970424     C     TASSAZ        BEGSR
041600060810     c*
041700061221     c                   clear                   tassta
041800061221     c                   clear                   tascas
041900061221     c                   clear                   tastic
042000061221     c                   clear                   tasvca
042100061221     c                   clear                   tascmb
042200061221     c                   clear                   tassva
042300060809     c                   movel     *Blanks       tastla
042400060809     c                   movel     *Blanks       taserr
042500060809     c                   movel     *Blanks       tasmsg
042600060809     c                   clear                   tasial
042700060809     c                   clear                   task88
042800060809     c                   clear                   tasfcaa
042900060809     c                   clear                   taspkcn
043000060809     c                   clear                   tasric
043100060809     c                   clear                   tasspc
043200060809     c                   clear                   tasBan
043300080908     c                   clear                   tasric
043400060810     c                   movel     Tasftc        tastc1
043500080908      * verifico se devo calcolare l'addebito di lasciato avviso
043600080908      * verifico se c'è evento
043700080908     c     Kevb4         chain     Fnevb04l
043800080908
043900080908     c                   If        %found(Fnevb04l)
044000080908     c                   eval      tasric = 'S'
044100080908     c                   endif
044200060825     c                   clear                   tasimv
044300060825     c                   clear                   taspor
044400060825     c*
044500060825     c                   if        $err='0'
044600060907     C                   CLEAR                   DTASV
044700130301     C                   CLEAR                   WF1_SV
044800130301     C                   CLEAR                   WF1_VA
044900970424     C*
045000970424IF  1C     D50KSC        IFNE      *ZEROS
045100060811     C                   MOVEL     D50KSC        dKSC
045200060811     C                   MOVEL     D50CTR        dCTR
045300060811     c                   else
045400060811     C                   MOVEL     tasKSC        dKSC
045500060811     C                   MOVEL     tasCTR        dCTR
045600970424E   1C                   ENDIF
045700970424     C     D50DTR        IFGT      *ZEROS
045800970424     C                   Z-ADD     D50DTR        TASDSP
045900060810     c                   else
046000060810     C                   MOVEL     TASDSPt       TASDsp                         AAAA/MM/GG
046100970424     C                   ENDIF
046200130228     C                   MOVEL     TASDSP        TASDCT                         AAAA/MM/GG
046300130228     C**                 MOVEL     TASDSPt       TASDCT                         AAAA/MM/GG
046400970424    1C     §3AFCA        IFEQ      1
046500061221     c                   movel     csbsta        Tassta
046600060809     c                   z-add     csbcas        Tascas
046700061221     c                   z-add     csbcmb        tascmb
046800061221     c                   movel     csbvca        tasvca
046900060809     c                   if        csbfus <> *blanks
047000060809     c                   move      csbfus        tastic
047100060809     c                   else
047200060809     c                   move      csbtpi        tastic
047300060809     c                   end
047400970424E   1C                   ENDIF
047500060809      * Flag SI NO DDT
047600060809    1C                   If        tasll1 = 'C' or tasll1 = 'S'
047700060809     C                   movel     'S'           Tasddt
047800060809   x1C                   Else
047900060809     C                   Clear                   Tasddt
048000060809    1C                   Endif
048100061109     c                   clear                   dtas01
048200061109     c                   movel     tasfbr        §asfbr
048300061109     c                   movel     tascca        §ascca
048400110804     c* servizio email
048500110804     c                   eval      dtasflo = tasflo
048600110804     c                   eval      §asemd=§floemd
048700141211
048800141211      /free
048900151130       //?Imposto se Prenotazione Ritiro Telefonico
049000151130         TASprt = §FLOado;
049100151130
049200141211       //?Imposto se part.consegna con PinCode
049300141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
049400141211           §ASpin = 'S';
049500141211         ENDIF;
049600151130
049700151130       //?Imposto se Stampa Packing List
049800151130         clear TASspl;
049900151130         IF  §AR5als = 'S' or §AR5alx = 'S';
050000151130           TASspl = 'S';
050100151130         ENDIF;
050200141211      /end-free
050300141211
050400130228      * data decorrenza tariffa/offerta
050500130228     c                   IF        dataddt > 0
050600130228     c                   eval      §asdrt = %editc(dataddt:'X')
050700130228     c                   ENDIF
050800061109     c                   eval      dstasflo = dtas01
050900030204      * Bancali
051000060809If  1c                   If        %Subst(tasGva:1:1) ='B'
051100030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
051200030205    1c                   EndIf
051300030729      * Colli originali
051400060809If  1c                   If        %Subst(tasGva:1:1) ='O'
051500060811     c                   Eval      dNcl = §Ar5bCor
051600061030     c                   else
051700061030     c                   Eval      dNcl = tasncl
051800030729    1c                   EndIf
051900130226      * Assicurazione, se presente è da togliere
052000130226     c                   IF        TASfcm = 'F'
052100130226     c                   clear                   TASfcm
052200130226     c                   clear                   TASias
052300130226     c                   clear                   TASvas
052400130226     c                   ENDIF
052500970424     C*--------------------------
052600970424     C*  D S T A S V
052700970424     C*--------------------------
052800970424     C                   CLEAR                   KPJBU
052900151015
053000151015      * imposto dati dtaspes
053100151015     c                   eval      tasppkb = taspkb
053200151015     c                   eval      taspdtt = tasdsp
053201160504     c                   eval      taspfvf = tasfvf
053202160504     c                   eval      taspncr = tasncr
053300970424     C*--------------------------
053400011102     C*  CALCOLO TASSAZIONE
053500970424     C*--------------------------
053600151015     C                   CALL      'TNSF22R'                            99
053700970424     C                   PARM                    KPJBA
053800991126     C                   PARM                    DTAS
053900991126     C                   PARM                    DTASV
054000151015     c                   parm                    dtaspes
054100011102     C*
054200130228     C* In uscita dal TNSF20R verifico la divisa della DS di passaggio rispetto a quella
054300011102     C* di calcolo del CAT reperita dalla tabella GED tipo record CE.
054400011102     C                   EXSR      CNVDTAS
054500060809     c                   end
054600011102     C*
054700970424     C                   Z-ADD     TASPVL        D50PVL
054800060904IF  1C     tasERR        IFNE      ' '                                          *ERRORE
054900060904     C                   MOVEL     tasMSG        DSMSG
055000130228     C                   MOVEL     'SF20'        DSERR
055100970424     C                   MOVEL     DSMSG         $MSG
055200970424     C                   EXSR      ERRORE
055300970424X   1C                   ELSE
055400991126     C                   Z-ADD     TASIMV        D50RIC
055500970424E   1C                   ENDIF
055600130301      * scrivo file delle varie
055700130507     c                   IF        not wNoWf
055800130301     c                   exsr      wrtVarie
055900130507     c                   ENDIF
056000970424     C*
056100970424     C                   ENDSR
056200970424     C**************************************************************************
056300970424     C*    CALCOLO COMPETENZE
056400970424     C**************************************************************************
056500970424     C     CALCOM        BEGSR
056600970424     C*
056700970424     C                   CLEAR                   DS$ECO
056800970424     C                   CLEAR                   TRECDS
056900060810     C                   EXSR      AZZW
057000970424     C                   MOVEL     D50TLA        D$GSTS                         *STATUS
057100970424     C                   MOVEL     'TNTE50R'     D$GPGF                         *PGM CHE HA MODIF.FI
057200060810     C                   EXSR      cddTAQ
057300060904     C                   EXSR      AZZnCDDTAQ
057400970424     C* CONTRASSEGNO
057500060809     c                   movel     csbtpa        D$DTPI
057600060810     c                   movel     csbsta        D$Dsta
057700060809     c                   if        csbfus <> *blanks
057800060809     c                   move      csbfus        D$DTPI
057900060809     c                   else
058000060809     c                   move      csbtpi        D$DTPI
058100060809     c                   end
058200020125     C                   Z-ADD     TASCAS        D$DCAS                         *LIRE CONTRASS
058300970424     C* ASSICURAZIONE
058400020125     C                   Z-ADD     TASIAS        D$DVAE
058500060809     C* TRANSITI
058600060809     C                   Z-ADD     §AR5flp       D$DAR5flp
058700060809     C                   Z-ADD     §AR5fl2       D$DAR5fl2
058800060809     C                   Z-ADD     §AR5fl3       D$DAR5fl3
058900060809     C                   Z-ADD     §AR5DE3       D$DAR5DE3
059000060809     C                   Z-ADD     §AR5DU3       D$DAR5DU3
059100060809     C                   Z-ADD     §AR5fl4       D$DAR5fl4
059200060809     C                   Z-ADD     §AR5DE4       D$DAR5DE4
059300060809     C                   Z-ADD     §AR5DU4       D$DAR5DU4
059400060809     C                   Z-ADD     §AR5fl5       D$DAR5fl5
059500060809     C                   Z-ADD     §AR5DE5       D$DAR5DE5
059600060809     C                   Z-ADD     §AR5DU5       D$DAR5DU5
059700060809     C                   Z-ADD     §AR5DET       D$DAR5DET
059800060809     C                   Z-ADD     §AR5DUT       D$DAR5DUT
059900060809     C                   Z-ADD     §AR5DE2       D$DAR5DE2
060000060809     C                   Z-ADD     §AR5DU2       D$DAR5DU2
060100030204     C*
060200030122     C                   Z-ADD     §AR5TBA       D$DAR5TBA                      *TIPO BANCALE 1
060300030122     C                   Z-ADD     §AR5NBA       D$DAR5NBA                      *NUM. BANCALE 1
060400030122     C                   Z-ADD     §AR5TB2       D$DAR5TB2                      *TIPO BANCALE 2
060500030122     C                   Z-ADD     §AR5NB2       D$DAR5NB2                      *NUM. BANCALE 2
060600030730     C*
060700030730     c                   IF        §AR5BNBA > *zeros OR
060800030730     c                             §AR5BNB2 > *zeros
060900030730     C                   Z-ADD     §AR5BTBA      D$DAR5TBA                      *TIPO BANCALE 1 BAR
061000030730     C                   Z-ADD     §AR5BNBA      D$DAR5NBA                      *NUM. BANCALE 1 BAR
061100030730     C                   Z-ADD     §AR5BTB2      D$DAR5TB2                      *TIPO BANCALE 2 BAR
061200030730     C                   Z-ADD     §AR5BNB2      D$DAR5NB2                      *NUM. BANCALE 2 BAR
061300030730     c                   ENDIF
061400040315     C                   Z-ADD     §AR5BCSF      D$DAR5CSF                      *NUM. BANCALE 2 BAR
061500040315     C                   Z-ADD     §AR5BCOR      D$DAR5COR                      *NUM. BANCALE 2 BAR
061600030730     C*
061700970424     C* SCHIERA RICAVI
061800030515     C     1             DO        75            I                              *AZZERA SCHIERE
061900970424     C                   Z-ADD     *ZEROS        DSWCN                          SCH NUM --> DS NUM
062000970424     C                   MOVE      DSWCA         CAL(I)                         DS ALF --> SCH ALF
062100970424     C                   Z-ADD     *ZEROS        DSWIN
062200970424     C                   MOVE      DSWIA         IAL(I)
062300970424     C                   ENDDO
062400970424     C                   Z-ADD     556           DSWCN                          SCH NUM --> DS NUM
062500970424     C                   MOVE      DSWCA         CAL(1)                         DS ALF --> SCH ALF
062600970424     C                   Z-ADD     D50RIC        DSWIN
062700970424     C                   MOVE      DSWIA         IAL(1)
062800970424     C                   MOVEA     CAL           D$DCCD                         *DS COD. RICAVI
062900970424     C                   MOVEA     IAL           D$DICD                         *DS IMP.RICAVI
063000060809     C                   MOVEL     tasmct        D$DMCT
063100060811     C                   MOVEA     SV            D$DSVN
063200060825     c                   do        20            k
063300060825     C                   MOVE      Va(k)         vn(k)
063400060825     c                   end
063500060825     C                   MOVEA     Vn            D$DVAN
063600060904     C* non considero la GIACENZA
063700970424     C                   MOVEL     'N'           D$DFGC                         *FORZATURA GIAC=NO
063800051212     C                   Z-ADD     0             D$DGGS
063900970424     C* NUMERO STOP DI RITIRO
064000020109     C                   Z-ADD     1             D$DNSR                         *N.STOP RITIRO
064100060810IF  1C     tasstr        IFGT      1
064200060810     C     1             DIV       tasstr        D$DNSR                         *1 / N. DI STOP
064300020109IF  2C     D$DNSR        IFLT      0,00001
064400020109     C                   Z-ADD     0,00001       D$DNSR
064500971024E   2C                   ENDIF
064600970424E   1C                   ENDIF
064700971027     C* NUMERO STOP DI CONSEGNA
064800971027     C                   Z-ADD     1             D$DNSC                         *N.STOP CONSEGNA
064900060810IF  1C     tasstp        IFGT      1
065000060810     C     1             DIV       tasstp        D$DNSC                         *1 / N. DI STOP
065100020109IF  2C     D$DNSC        IFLT      0,00001
065200020109     C                   Z-ADD     0,00001       D$DNSC
065300971027E   2C                   ENDIF
065400971027E   1C                   ENDIF
065500971024     C*
065600971024     C* IMPOSTA ALTRI CAMPI "DOPO"
065700980826     C                   MOVEL     §3ATB1        D$DTPO                         *TIPO PORTO F/A
065800060810     C     D$DFBR        IFNE      'S'
065900060810     C                   MOVEL     'S'           D$DFIG
066000060810     C                   ENDIF
066100970424     C*
066200970424     C                   MOVEL     D$DDS1        D$DRE1                         *DS DOPO
066300970424     C                   MOVEL     D$DDS2        D$DRE2
066400970424     C                   MOVEL     D$ODS1        D$ORE1                         *DS ORIG
066500970424     C                   MOVEL     D$ODS2        D$ORE2
066600970424     C*
066700970424     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
066800970424     C                   CALL      'TRECD8R'                            98
066900970424     C                   PARM                    DS$ECO                         *DS IMMAGINI BOLLA
067000970424     C*
067100970424     C* SE RITORNA ERRORE ESCO PGM
067200970424     C     D$GERR        IFNE      *BLANKS
067300970424     C                   MOVEL     D$GMSG        $MSG
067400970424     C                   EXSR      ERRORE
067500970424     C                   ENDIF
067600970424     C*
067700970424     C     $ERR          IFEQ      '0'                                          --- 1 -->
067800970424     C*
067900970424     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
068000970424     C                   MOVEA     D$DCCD        CAL                            SCHIERE ALFANUMERICH
068100970424     C                   MOVEA     D$DICD        IAL
068200030515     C     1             DO        75            I                 6 0
068300970424     C                   MOVE      CAL(I)        DSWCA                          SCH ALF --> DS ALF
068400970424     C                   Z-ADD     DSWCN         CD                6 0          DS NUM --> SCH NUM
068500970424     C     CD            IFNE      *ZEROS
068600970424     C                   MOVE      IAL(I)        DSWIA
068700090922     C                   Z-ADD     DSWIN         IMP              13 5
068800970424     C     CD            IFEQ      900                                          *RICAVO PRES. DELTA
068900090922     C                   Z-ADD(h)  IMP           D50COM
069000970424     C                   ENDIF
069100970424     C                   ENDIF
069200970424     C                   ENDDO
069300970424     C                   ENDIF                                                  <-- 1 ---
069400970424     C*
069500970424     C                   ENDSR
069600130301
069700130301      ********************************************************************
069800130301      *   wrtVarie - Scrivo file delle varie
069900130301      ********************************************************************
070000130301     c     wrtvarie      BEGSR
070100130301
070200130301     c                   clear                   wfdbf100
070300130301
070400130301     c                   eval      WF1aas = TASaas
070500130301     c                   eval      WF1mgs = TASmgs
070600130301     c                   eval      WF1lnp = TASlnp
070700130301     c                   eval      WF1nrs = TASnrs
070800130301     c                   eval      WF1nsp = TASnsp
070900130301     c                   eval      WF1ksc = TASksc
071000130301     c                   eval      WF1div = TASdiv
071100130301     c                   eval      WF1por = TASpor
071200130301
071300130301     c                   movea     sv            wf1_sv
071400130301     c                   movea     va            wf1_va
071500130301
071600130301     c                   eval      WF1imv = TASimv
071700130301
071800130301     c                   write     wfdbf100
071900130301
072000130301     c                   ENDSR
072100130301
072200060810     C********************************************************************
072300060810     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
072400060810     C********************************************************************
072500060810     C     AZZW          BEGSR
072600060810     C*
072700060810     C* AZZERO CAMPI DS ESTERNA
072800060810     c* è possibile usare all'interno del pgm solo i campi prima e dopo
072900060810     c* del TITAS senza testare i relativi flag prima o dopo.
073000060810     c* Se si vogliono usare i campi degli altri files senza testare i
073100060810     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
073200060810     c* contenute nel /COPY
073300060810     C                   EXSR      APDTAQ
073400060810     C                   EXSR      ADDTAQ
073500060810     C                   EXSR      AODTAQ
073600060810     C                   MOVEL     D$ODS1        D$ORE1
073700060810     C                   MOVEL     D$ODS2        D$ORE2
073800060810     C                   MOVEL     *BLANKS       D$PCO1
073900060810     C                   MOVEL     *BLANKS       D$PCO2
074000060810     C                   MOVEL     *BLANKS       D$DCO1
074100060810     C                   MOVEL     *BLANKS       D$DCO2
074200060810     C                   MOVEL     *BLANKS       D$PSVN
074300060810     C                   MOVEL     *BLANKS       D$PVAN
074400060810     C                   MOVEL     *BLANKS       D$DSVN
074500060810     C                   MOVEL     *BLANKS       D$DVAN
074600060810     C                   MOVEL     *BLANKS       D$OSVN
074700060810     C                   MOVEL     *BLANKS       D$OVAN
074800060810     C                   Z-ADD     *ZEROS        D$PCAS
074900060810     C                   MOVEL     *BLANKS       D$PTPI
075000060810     C                   MOVEL     *BLANKS       D$Psta
075100060810     C                   Z-ADD     *ZEROS        D$DCAS
075200060810     C                   MOVEL     *BLANKS       D$DTPI
075300060810     C                   MOVEL     *BLANKS       D$Dsta
075400060810     C                   Z-ADD     *ZEROS        D$PVAE
075500060810     C                   Z-ADD     *ZEROS        D$DVAE
075600060810     C                   Z-ADD     *ZEROS        D$PNSR
075700060810     C                   Z-ADD     *ZEROS        D$DNSR
075800060810     C                   Z-ADD     *ZEROS        D$PAGC
075900060810     C                   Z-ADD     *ZEROS        D$PMGC
076000060810     C                   Z-ADD     *ZEROS        D$DAGC
076100060810     C                   Z-ADD     *ZEROS        D$DMGC
076200060810     C                   MOVEL     *BLANKS       D$PTPO
076300060810     C                   MOVEL     *BLANKS       D$PRBL
076400060810     C                   MOVEL     'N'           D$PCSF
076500060810     C                   MOVEL     'N'           D$PCCS
076600060810     C                   MOVEL     'N'           D$PDBO
076700060810     C                   MOVEL     'N'           D$PFRI
076800060810     C                   MOVEL     'N'           D$PFSP
076900060810     C                   MOVEL     *BLANKS       D$DTPO
077000060810     C                   MOVEL     *BLANKS       D$DRBL
077100060810     C                   MOVEL     'N'           D$DCSF
077200060810     C                   MOVEL     'N'           D$DCCS
077300060810     C                   MOVEL     'N'           D$DDBO
077400060810     C                   MOVEL     'N'           D$DFRI
077500060810     C                   MOVEL     'N'           D$DFSP
077600060810     C                   MOVEL     *BLANKS       D$PAR5T
077700060810     C                   MOVEL     *BLANKS       D$DAR5T
077800060810     C                   MOVEL     *BLANKS       D$PAR5B
077900060810     C                   MOVEL     *BLANKS       D$DAR5B
078000060810     C                   Z-ADD     *ZEROS        D$PAR5flp
078100060810     C                   Z-ADD     *ZEROS        D$PAR5DET
078200060810     C                   Z-ADD     *ZEROS        D$PAR5DUT
078300060810     C                   Z-ADD     *ZEROS        D$PAR5fl2
078400060810     C                   Z-ADD     *ZEROS        D$PAR5DE2
078500060810     C                   Z-ADD     *ZEROS        D$PAR5DU2
078600060810     C                   Z-ADD     *ZEROS        D$PAR5fl3
078700060810     C                   Z-ADD     *ZEROS        D$PAR5DE3
078800060810     C                   Z-ADD     *ZEROS        D$PAR5DU3
078900060810     C                   Z-ADD     *ZEROS        D$PAR5fl4
079000060810     C                   Z-ADD     *ZEROS        D$PAR5DE4
079100060810     C                   Z-ADD     *ZEROS        D$PAR5DU4
079200060810     C                   Z-ADD     *ZEROS        D$PAR5fl5
079300060810     C                   Z-ADD     *ZEROS        D$PAR5DE5
079400060810     C                   Z-ADD     *ZEROS        D$PAR5DU5
079500060810     C                   Z-ADD     *ZEROS        D$DAR5flp
079600060810     C                   Z-ADD     *ZEROS        D$DAR5DET
079700060810     C                   Z-ADD     *ZEROS        D$DAR5DUT
079800060810     C                   Z-ADD     *ZEROS        D$DAR5fl2
079900060810     C                   Z-ADD     *ZEROS        D$DAR5DE2
080000060810     C                   Z-ADD     *ZEROS        D$DAR5DU2
080100060810     C                   Z-ADD     *ZEROS        D$DAR5fl3
080200060810     C                   Z-ADD     *ZEROS        D$DAR5DE3
080300060810     C                   Z-ADD     *ZEROS        D$DAR5DU3
080400060810     C                   Z-ADD     *ZEROS        D$DAR5fl4
080500060810     C                   Z-ADD     *ZEROS        D$DAR5DE4
080600060810     C                   Z-ADD     *ZEROS        D$DAR5DU4
080700060810     C                   Z-ADD     *ZEROS        D$DAR5fl5
080800060810     C                   Z-ADD     *ZEROS        D$DAR5DE5
080900060810     C                   Z-ADD     *ZEROS        D$DAR5DU5
081000060810     C                   Z-ADD     *ZEROS        D$PAR5TBA
081100060810     C                   Z-ADD     *ZEROS        D$PAR5NBA
081200060810     C                   Z-ADD     *ZEROS        D$PAR5TB2
081300060810     C                   Z-ADD     *ZEROS        D$PAR5NB2
081400060810     C                   Z-ADD     *ZEROS        D$DAR5TBA
081500060810     C                   Z-ADD     *ZEROS        D$DAR5NBA
081600060810     C                   Z-ADD     *ZEROS        D$DAR5TB2
081700060810     C                   Z-ADD     *ZEROS        D$DAR5NB2
081800060810     C                   Z-ADD     *ZEROS        D$PAR5CSF
081900060810     C                   Z-ADD     *ZEROS        D$PAR5COR
082000060810     C                   Z-ADD     *ZEROS        D$DAR5CSF
082100060810     C                   Z-ADD     *ZEROS        D$DAR5COR
082200060810     C                   MOVEL     *BLANKS       D$XXXX
082300060810     C*
082400060810     C                   ENDSR
082500060904     C*-------------------------------------------------------------------
082600060904     C*?Azzera varia "N" immagine -dopo- TITAS
082700060904     C*-------------------------------------------------------------------
082800060904     C     AZZnCDDTAQ    BEGSR
082900060904     c*
083000060904if  1C                   IF        (D$DVA1 = 88888888,000)
083100060904     C                   Z-ADD     *zeros        D$DVA1
083200060904e   1c                   ENDIF
083300060904if  1C                   IF        (D$DVA2 = 88888888,000)
083400060904     C                   Z-ADD     *zeros        D$DVA2
083500060904e   1c                   ENDIF
083600060904if  1C                   IF        (D$DVA3 = 88888888,000)
083700060904     C                   Z-ADD     *zeros        D$DVA3
083800060904e   1c                   ENDIF
083900060904     c*
084000060904     C                   ENDSR
084100970424     C********************************************************************
084200970424     C*   CALDEL - CALCOLA IL DELTA
084300970424     C*          INPUT  --> WRRW: RICAVO REALE
084400970424     C*                     WRPW: RICAVO PRESUNTO
084500970424     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
084600970424     C********************************************************************
084700970424     C     CALDEL        BEGSR
084800970424     C*
084900970424     C*--------------------
085000970424     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
085100970424     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
085200970424     C*--------------------
085300970424     C*
085400970424     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
085500970424     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
085600970424     C     GAP           MULT      100           GAP                            (GAP x 100)
085700970424     C*
085800970424     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
085900970424IF  1C     WRRW          IFEQ      *ZEROS
086000970424     C                   Z-ADD     1             WRRW
086100970424E   1C                   ENDIF
086200970424     C*
086300970424     C     GAP           DIV       WRRW          GAP
086400970424     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
086500970424     C*
086600970424     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
086700970424     C     WDPW          COMP      0                                    28
086800970424     C* POSITIVO
086900970424IF  1C     *IN28         IFEQ      *OFF
087000970424IF  2C     GAP           IFGE      1000
087100970424     C                   Z-ADD     +999,9        WDPW
087200970424E   2C                   ENDIF
087300970424     C* NEGATIVO
087400970424X   1C                   ELSE
087500970424IF  2C     GAP           IFLE      -1000
087600970424     C                   Z-ADD     -999,9        WDPW
087700970424E   2C                   ENDIF
087800970424E   1C                   ENDIF
087900970424     C*
088000970424     C                   ENDSR
088100970424     C********************************************************************
088200970424     C*   ERRORE - ROUTINE GESTIONE ERRORI
088300970424     C********************************************************************
088400970424     C     ERRORE        BEGSR
088500970424     C*
088600970424     C                   MOVEL     'S'           $ERR
088700970424     C                   MOVEL     $MSG          D50MSG
088800970424     C                   MOVE      $MSG          D50ERR
088900970424     C*
089000970424     C                   ENDSR
089100011012     C*------------------------------------------------------------------------*
089200011012     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
089300011012     C*------------------------------------------------------------------------*
089400011012     C     CARGEDCE      BEGSR
089500011012     C*
089600011012     C                   CLEAR                   DSGEDCE
089700011026     C                   MOVEL     *blanks       WDIVCE            3
089800011012     C*
089900011012     C                   MOVEL(P)  'GED'         KTECOD
090000011012     C                   MOVEL(P)  'CE'          KTEKE1
090100011012     C     KEYTBE        CHAIN     TNTBE01L
090200011012     C                   IF        %found(TNTBE01L)
090300011012     C                   MOVEL     TBEUNI        DSGEDCE
090400011012     C                   ENDIF
090500011012     C*
090600011102     C                   IF        §GEDCEDAT > DATCOR
090700011026     C                   EVAL      WDIVCE = §GEDCEDIP
090800011026     C                   ELSE
090900011026     C                   EVAL      WDIVCE = §GEDCEDIV
091000011012     C                   ENDIF
091100011012     C*
091200011012     C                   ENDSR
091300011102     C*------------------------------------------------------------------------*
091400011102     C* CNVDTAS - CONVERTE I CAMPI DELLE DS DTAS/DTASV NELLA DIVISA DI CALCOLO DEL CE
091500011102     C*------------------------------------------------------------------------*
091600011102     C     CNVDTAS       BEGSR
091700011102     C*
091800011102     C* CONTRASSEGNO
091900011102     C                   Z-ADD     TASCAS        WIMI             30 9
092000011107     C                   MOVEL     TASVCA        WDVI              3
092100011102     C                   EXSR      CNVEUR
092200011102     C                   Z-ADD     WIMO          TASCAS
092300011102     C*
092400011102     C* IMPORTO DA ASSICURARE
092500011102     C                   Z-ADD     TASIAS        WIMI             30 9
092600011107     C                   MOVEL     TASVAS        WDVI              3
092700011102     C                   EXSR      CNVEUR
092800011102     C                   Z-ADD     WIMO          TASIAS
092900011102     C*
093000011102     C* QUANTITA' DA FATTURARE (non viene convertita x ambiguità su suo utilizzo)
093100011102     C*
093200011102     C* TRASPORTO
093300011102     C                   Z-ADD     TASPOR        WIMI             30 9
093400011102     C                   MOVEL     TASDIV        WDVI              3
093500011102     C                   EXSR      CNVEUR
093600011102     C                   Z-ADD     WIMO          TASPOR
093700011102     C*
093800011102     C* TOTALE IMPONIBILE
093900011102     C                   Z-ADD     TASIMV        WIMI             30 9
094000011102     C                   MOVEL     TASDIV        WDVI              3
094100011102     C                   EXSR      CNVEUR
094200011102     C                   Z-ADD     WIMO          TASIMV
094300011102     C*
094400011102     C* IMPORTO ASS. IN MONETA O MAX VALORE ...
094500011102     C                   Z-ADD     TASIAL        WIMI             30 9
094600011123     C                   MOVEL     TASDIV        WDVI              3
094700011102     C                   EXSR      CNVEUR
094800011102     C                   Z-ADD     WIMO          TASIAL
094900011102     C*
095000011102     C* VARIE VARIE
095100011102     C                   Z-ADD     TASVA1        WIMI             30 9
095200011102     C                   MOVEL     TASDIV        WDVI              3
095300011102     C                   EXSR      CNVEUR
095400011102     C                   Z-ADD     WIMO          TASVA1
095500011102     C                   Z-ADD     TASVA2        WIMI             30 9
095600011102     C                   MOVEL     TASDIV        WDVI              3
095700011102     C                   EXSR      CNVEUR
095800011102     C                   Z-ADD     WIMO          TASVA2
095900011102     C                   Z-ADD     TASVA3        WIMI             30 9
096000011102     C                   MOVEL     TASDIV        WDVI              3
096100011102     C                   EXSR      CNVEUR
096200011102     C                   Z-ADD     WIMO          TASVA3
096300011102     C                   Z-ADD     TASVA4        WIMI             30 9
096400011102     C                   MOVEL     TASDIV        WDVI              3
096500011102     C                   EXSR      CNVEUR
096600011102     C                   Z-ADD     WIMO          TASVA4
096700011102     C                   Z-ADD     TASVA5        WIMI             30 9
096800011102     C                   MOVEL     TASDIV        WDVI              3
096900011102     C                   EXSR      CNVEUR
097000011102     C                   Z-ADD     WIMO          TASVA5
097100011102     C                   Z-ADD     TASVA6        WIMI             30 9
097200011102     C                   MOVEL     TASDIV        WDVI              3
097300011102     C                   EXSR      CNVEUR
097400011102     C                   Z-ADD     WIMO          TASVA6
097500011102     C                   Z-ADD     TASVA7        WIMI             30 9
097600011102     C                   MOVEL     TASDIV        WDVI              3
097700011102     C                   EXSR      CNVEUR
097800011102     C                   Z-ADD     WIMO          TASVA7
097900011102     C                   Z-ADD     TASVA8        WIMI             30 9
098000011102     C                   MOVEL     TASDIV        WDVI              3
098100011102     C                   EXSR      CNVEUR
098200011102     C                   Z-ADD     WIMO          TASVA8
098300011102     C                   Z-ADD     TASVA9        WIMI             30 9
098400011102     C                   MOVEL     TASDIV        WDVI              3
098500011102     C                   EXSR      CNVEUR
098600011102     C                   Z-ADD     WIMO          TASVA9
098700011102     C                   Z-ADD     TASVA0        WIMI             30 9
098800011102     C                   MOVEL     TASDIV        WDVI              3
098900011102     C                   EXSR      CNVEUR
099000011102     C                   Z-ADD     WIMO          TASVA0
099100011102     C                   Z-ADD     TASV11        WIMI             30 9
099200011102     C                   MOVEL     TASDIV        WDVI              3
099300011102     C                   EXSR      CNVEUR
099400011102     C                   Z-ADD     WIMO          TASV11
099500011102     C                   Z-ADD     TASV12        WIMI             30 9
099600011102     C                   MOVEL     TASDIV        WDVI              3
099700011102     C                   EXSR      CNVEUR
099800011102     C                   Z-ADD     WIMO          TASV12
099900011102     C                   Z-ADD     TASV13        WIMI             30 9
100000011102     C                   MOVEL     TASDIV        WDVI              3
100100011102     C                   EXSR      CNVEUR
100200011102     C                   Z-ADD     WIMO          TASV13
100300011102     C                   Z-ADD     TASV14        WIMI             30 9
100400011102     C                   MOVEL     TASDIV        WDVI              3
100500011102     C                   EXSR      CNVEUR
100600011102     C                   Z-ADD     WIMO          TASV14
100700011102     C                   Z-ADD     TASV15        WIMI             30 9
100800011102     C                   MOVEL     TASDIV        WDVI              3
100900011102     C                   EXSR      CNVEUR
101000011102     C                   Z-ADD     WIMO          TASV15
101100011102     C                   Z-ADD     TASV16        WIMI             30 9
101200011102     C                   MOVEL     TASDIV        WDVI              3
101300011102     C                   EXSR      CNVEUR
101400011102     C                   Z-ADD     WIMO          TASV16
101500011102     C                   Z-ADD     TASV17        WIMI             30 9
101600011102     C                   MOVEL     TASDIV        WDVI              3
101700011102     C                   EXSR      CNVEUR
101800011102     C                   Z-ADD     WIMO          TASV17
101900011102     C                   Z-ADD     TASV18        WIMI             30 9
102000011102     C                   MOVEL     TASDIV        WDVI              3
102100011102     C                   EXSR      CNVEUR
102200011102     C                   Z-ADD     WIMO          TASV18
102300011102     C                   Z-ADD     TASV19        WIMI             30 9
102400011102     C                   MOVEL     TASDIV        WDVI              3
102500011102     C                   EXSR      CNVEUR
102600011102     C                   Z-ADD     WIMO          TASV19
102700011102     C                   Z-ADD     TASV20        WIMI             30 9
102800011102     C                   MOVEL     TASDIV        WDVI              3
102900011102     C                   EXSR      CNVEUR
103000011102     C                   Z-ADD     WIMO          TASV20
103100011102     C*
103200011102     C                   ENDSR
103300011012     C*------------------------------------------------------------------------*
103400011012     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
103500011012     C*------------------------------------------------------------------------*
103600011012     C     CNVEUR        BEGSR
103700011112     C*
103800011112     C* AZZERA VARIABILI DI LAVORO
103900011112     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
104000011112     C                   MOVEL     '9'           NDEC              1            *N. DEC IN OUTPUT
104100011112     C*
104200011112     C* DI DEFUALT ATTRIBUISCO LO STESSO VALORE AGLI IMPORTI "DA CONVERTIRE" E "CONVERTITI"
104300011112     C                   Z-ADD     WIMI          WIMO
104400011107     C*
104500011107     C* COME PRIMA COSA AI FINI DELLA CONVERSIONE TESTO SE LE DIVISE SONO DIVERSE
104600011107     C     WDVI          IFNE      WDIVCE
104700011012     C*
104800011012IF  1C     WIMI          IFNE      *ZEROS
104900011012     C                   CLEAR                   YEUR
105000011012     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
105100011012     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
105200011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
105300011012     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
105400011012     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
105500011012     C                   CALL      'YEURCO'
105600011012     C                   PARM                    YEUR
105700011012IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
105800011012     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
105900011012X   2C                   ELSE                                                   *SI ERRORI
106000011012     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
106100011012E   2C                   ENDIF
106200011012E   1C                   ENDIF
106300011012     C*
106400011107     C                   ENDIF
106500011107     C*
106600011012     C                   ENDSR
106700970424     C********************************************************************
106800970424     C*   RUTINZ - ROUTINE INIZIALE
106900970424     C********************************************************************
107000970424     C     RUTINZ        BEGSR
107100970424     C*
107200970424     C* PARAMETRI IN INPUT ALLA ROUTINE
107300970424     C     *ENTRY        PLIST
107400970424     C                   PARM                    DS50
107500130228     C                   PARM                    parddt
107600130507     C                   PARM                    partest
107700130228
107800130228      /free
107900130507         wNoWf = *off;
108000130228       //?Se non passato il paramtro della data decorrenza tariffa/offerta
108100130228       //?pulisco il campo
108200130228         IF  %parms = 1;
108300130228           clear dataddt;
108400130228         ELSE;
108500130228           dataddt = %int(parddt);
108600130228         ENDIF;
108700130507       //?Se passato il terzo parametro non devo aprire il file di work
108800130507         IF  %parms > 2;
108900130507           wNoWf = *on;
109000130507         ENDIF;
109100130507       //?apro il file di work
109200130509         IF  not wNoWf and not %open(WFDBF10F);
109300130507           open WFDBF10F;
109400130507         ENDIF;
109500130228      /end-free
109600970424     C*
109700060809     C* CHIAVE LETTURA titas30c - COMPLETA
109800060809     C     KEYtas        KLIST
109900060809     C                   KFLD                    tasAAS
110000060809     C                   KFLD                    tasLNP
110100060809     C                   KFLD                    tasNRS
110200060809     C                   KFLD                    tasNSP
110300060809      *  KLIST PER ACCESSO FILES
110400060809     c     KTA7          KLIST
110500060809     c                   KFLD                    TASAAS
110600060809     c                   KFLD                    TASLNP
110700060809     c                   KFLD                    TASNRS
110800060809     c                   KFLD                    TASNSP
110900060809     c                   KFLD                    TASTBL
111000030122     C*
111100060809     C* CHIAVE LETTURA FIAR531c - COMPLETA
111200030122     C     KEYAR5        KLIST
111300030122     C                   KFLD                    AR5AAS
111400030122     C                   KFLD                    AR5LNP
111500030122     C                   KFLD                    AR5NRS
111600030122     C                   KFLD                    AR5NSP
111700030122     C                   KFLD                    AR5TRD
111800060809     C                   KFLD                    AR5prg
111900970424     C*
112000060809     C* CHIAVE LETTURA tncsb03l - COMPLETA
112100060809     C     KEYcsb        KLIST
112200060809     C                   KFLD                    csbAAS
112300060809     C                   KFLD                    csbLNP
112400060809     C                   KFLD                    csbNRS
112500060809     C                   KFLD                    csbNSP
112600970424     C*
112700970424     C* CHIAVE LETTURA TABEL00F - COMPLETA
112800970424     C     KEYTAB        KLIST
112900970424     C                   KFLD                    KTBUTE
113000970424     C                   KFLD                    KTBCOD
113100970424     C                   KFLD                    KTBKEY
113200011012     C*
113300011012     C* LETTURA TNTBE01L - PARZIALE
113400011012     C     KEYTBE        KLIST
113500011012     C                   KFLD                    KTECOD                         *UTENTE
113600011012     C                   KFLD                    KTEKE1                         *CODICE
113700080908      * Chiave file eventi FNEVB
113800080908     c     Kevb4         Klist
113900080908     c                   Kfld                    Tasaas
114000080908     c                   Kfld                    Taslnp
114100080908     c                   Kfld                    Tasnrs
114200080908     c                   Kfld                    Tasnsp
114300080908     c                   Kfld                    Wcev
114400970424     C* DEFINISCO CAMPI CHIAVE
114500011012     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
114600970424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
114700970424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
114800011012     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
114900011012     C     *LIKE         DEFINE    TBEKE1        KTEKE1
115000970424     C*
115100970424     C* RECUPERO IL CODICE DELL'AZIENDA
115200970424     C                   Z-ADD     1             CODUT             1 0
115300970424     C                   MOVEL     '0'           $ERR              1
115400970424     C                   MOVEL     *BLANKS       $MSG             78
115500011015     C*
115600011015     C* REPERISCE LA DATA CORRENTE
115700011015     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
115800011015     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
115900011015     C                   Z-ADD     WN8           G08DAT
116000011015     C                   Z-ADD     *ZEROS        G08INV
116100011015     C                   MOVEL     '0'           G08ERR
116200011015     C                   CALL      'XSRDA8'
116300011015     C                   PARM                    WLBDA8
116400011015     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
116500011012     C*
116600011012     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
116700011012     C                   EXSR      CARGEDCE
116800141211
116900141211      /free
117000141211       //?Carico part.consegna con abilitazione PinCode
117100141211         KTBcod = '7R';
117200141211         clear k;
117300141211         setll (KTBute:KTBcod) TABEL00F;
117400141211         reade (KTBute:KTBcod) TABEL00F;
117500141211         DOW  not %eof(TABEL00F);
117600141211           IF  TBLflg = *blanks;
117700141211             ds7R = TBLuni;
117800141211             IF  §7Rpincode = 'S';
117900141211               k += 1;
118000141211               skGMA(k) = TBLkey;
118100141211             ENDIF;
118200141211           ENDIF;
118300141211           reade (KTBute:KTBcod) TABEL00F;
118400141211         ENDDO;
118500141211      /end-free
118600141211
118700970424     C*
118800970424     C* AZZERO I CAMPI IN OUTPUT DEL PGM
118900970424     C                   EXSR      AZZERA
119000970424     C*
119100970424     C                   ENDSR
119200970424     C***********************************************************************
119300970424     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
119400970424     C***********************************************************************
119500970424     C     AZZERA        BEGSR
119600970424     C*
119700970424     C                   MOVEL     *BLANKS       D50ERR
119800970424     C                   MOVEL     *BLANKS       D50MSG
119900970424     C                   Z-ADD     *ZEROS        D50RIC
120000970424     C                   Z-ADD     *ZEROS        D50COM
120100970424     C                   Z-ADD     *ZEROS        D50DEL
120200970424     C*
120300970424     C                   ENDSR
120400060810      /COPY GAITRASRC/SRCTREC,TRECDT
120500970424     C********************************************************************
120600970424** ======== SCHIERA: MSG  (MESSAGGI)                       ================
120700970424TNTE50R- Bolla partenza non trovata o annullata.                          BPNT 1
120800970424TNTE50R- Valuta non trovata nella tab. CV (cambi valuta).                 VNTT 2
120900970424TNTE50R- CAP mittente non trovato.                                        CAPN 3
121000970424TNTE50R- Bolla non calcolabile.                                           NONC 4
121100970424TNTE50R- Tipo bolla non trovato in tabella 3A. Avvertire CED.             3ANT 5
121200970424TNTE50R- 6                                                                ERRO 6
121300970424TNTE50R- 7                                                                ERRO 7
121400970424TNTE50R- 8                                                                ERRO 8
121500970424TNTE50R- 9                                                                ERRO 9
121600970424TNTE50R- 10                                                               ERRO10
121700970424TNTE50R- 11                                                               ERRO11
121800970424TNTE50R- 12                                                               ERRO12
121900970424TNTE50R- 13                                                               ERRO13
122000970424TNTE50R- 14                                                               ERRO14
122100970424TNTE50R- 15                                                               ERRO15
122200970424TNTE50R- 16                                                               ERRO16
122300970424TNTE50R- 17                                                               ERRO17
122400970424TNTE50R- 18                                                               ERRO18
122500970424TNTE50R- 19                                                               ERRO19
122600970424TNTE50R- 20                                                               ERRO20
