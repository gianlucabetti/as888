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
040300060809     C     KEYAR5        CHAIN     fiar531c                           99
040400040806IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
040500040806     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
040600040806     C                   MOVEL     AR5UNI        DAR5GEN
040700040806E   2C                   ENDIF
040800040806E   1C                   ENDIF
040900970424     C*
041000970424     C                   ENDSR
041100970424     C*------------------------------------------------------------------------*
041200970424     C* TASSAZ - TASSAZIONE BOLLA
041300970424     C*------------------------------------------------------------------------*
041400970424     C     TASSAZ        BEGSR
041500060810     c*
041600061221     c                   clear                   tassta
041700061221     c                   clear                   tascas
041800061221     c                   clear                   tastic
041900061221     c                   clear                   tasvca
042000061221     c                   clear                   tascmb
042100061221     c                   clear                   tassva
042200060809     c                   movel     *Blanks       tastla
042300060809     c                   movel     *Blanks       taserr
042400060809     c                   movel     *Blanks       tasmsg
042500060809     c                   clear                   tasial
042600060809     c                   clear                   task88
042700060809     c                   clear                   tasfcaa
042800060809     c                   clear                   taspkcn
042900060809     c                   clear                   tasric
043000060809     c                   clear                   tasspc
043100060809     c                   clear                   tasBan
043200080908     c                   clear                   tasric
043300060810     c                   movel     Tasftc        tastc1
043400080908      * verifico se devo calcolare l'addebito di lasciato avviso
043500080908      * verifico se c'è evento
043600080908     c     Kevb4         chain     Fnevb04l
043700080908
043800080908     c                   If        %found(Fnevb04l)
043900080908     c                   eval      tasric = 'S'
044000080908     c                   endif
044100060825     c                   clear                   tasimv
044200060825     c                   clear                   taspor
044300060825     c*
044400060825     c                   if        $err='0'
044500060907     C                   CLEAR                   DTASV
044600130301     C                   CLEAR                   WF1_SV
044700130301     C                   CLEAR                   WF1_VA
044800970424     C*
044900970424IF  1C     D50KSC        IFNE      *ZEROS
045000060811     C                   MOVEL     D50KSC        dKSC
045100060811     C                   MOVEL     D50CTR        dCTR
045200060811     c                   else
045300060811     C                   MOVEL     tasKSC        dKSC
045400060811     C                   MOVEL     tasCTR        dCTR
045500970424E   1C                   ENDIF
045600970424     C     D50DTR        IFGT      *ZEROS
045700970424     C                   Z-ADD     D50DTR        TASDSP
045800060810     c                   else
045900060810     C                   MOVEL     TASDSPt       TASDsp                         AAAA/MM/GG
046000970424     C                   ENDIF
046100130228     C                   MOVEL     TASDSP        TASDCT                         AAAA/MM/GG
046200130228     C**                 MOVEL     TASDSPt       TASDCT                         AAAA/MM/GG
046300970424    1C     §3AFCA        IFEQ      1
046400061221     c                   movel     csbsta        Tassta
046500060809     c                   z-add     csbcas        Tascas
046600061221     c                   z-add     csbcmb        tascmb
046700061221     c                   movel     csbvca        tasvca
046800060809     c                   if        csbfus <> *blanks
046900060809     c                   move      csbfus        tastic
047000060809     c                   else
047100060809     c                   move      csbtpi        tastic
047200060809     c                   end
047300970424E   1C                   ENDIF
047400060809      * Flag SI NO DDT
047500060809    1C                   If        tasll1 = 'C' or tasll1 = 'S'
047600060809     C                   movel     'S'           Tasddt
047700060809   x1C                   Else
047800060809     C                   Clear                   Tasddt
047900060809    1C                   Endif
048000061109     c                   clear                   dtas01
048100061109     c                   movel     tasfbr        §asfbr
048200061109     c                   movel     tascca        §ascca
048300110804     c* servizio email
048400110804     c                   eval      dtasflo = tasflo
048500110804     c                   eval      §asemd=§floemd
048600141211
048700141211      /free
048800141211       //?Imposto se part.consegna con PinCode
048900141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
049000141211           §ASpin = 'S';
049100141211         ENDIF;
049200141211      /end-free
049300141211
049400130228      * data decorrenza tariffa/offerta
049500130228     c                   IF        dataddt > 0
049600130228     c                   eval      §asdrt = %editc(dataddt:'X')
049700130228     c                   ENDIF
049800061109     c                   eval      dstasflo = dtas01
049900030204      * Bancali
050000060809If  1c                   If        %Subst(tasGva:1:1) ='B'
050100030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
050200030205    1c                   EndIf
050300030729      * Colli originali
050400060809If  1c                   If        %Subst(tasGva:1:1) ='O'
050500060811     c                   Eval      dNcl = §Ar5bCor
050600061030     c                   else
050700061030     c                   Eval      dNcl = tasncl
050800030729    1c                   EndIf
050900130226      * Assicurazione, se presente è da togliere
051000130226     c                   IF        TASfcm = 'F'
051100130226     c                   clear                   TASfcm
051200130226     c                   clear                   TASias
051300130226     c                   clear                   TASvas
051400130226     c                   ENDIF
051500970424     C*--------------------------
051600970424     C*  D S T A S V
051700970424     C*--------------------------
051800970424     C                   CLEAR                   KPJBU
051900151015
052000151015      * imposto dati dtaspes
052100151015     c                   eval      tasppkb = taspkb
052200151015     c                   eval      taspdtt = tasdsp
052300970424     C*--------------------------
052400011102     C*  CALCOLO TASSAZIONE
052500970424     C*--------------------------
052600151015     C                   CALL      'TNSF22R'                            99
052700970424     C                   PARM                    KPJBA
052800991126     C                   PARM                    DTAS
052900991126     C                   PARM                    DTASV
053000151015     c                   parm                    dtaspes
053100011102     C*
053200130228     C* In uscita dal TNSF20R verifico la divisa della DS di passaggio rispetto a quella
053300011102     C* di calcolo del CAT reperita dalla tabella GED tipo record CE.
053400011102     C                   EXSR      CNVDTAS
053500060809     c                   end
053600011102     C*
053700970424     C                   Z-ADD     TASPVL        D50PVL
053800060904IF  1C     tasERR        IFNE      ' '                                          *ERRORE
053900060904     C                   MOVEL     tasMSG        DSMSG
054000130228     C                   MOVEL     'SF20'        DSERR
054100970424     C                   MOVEL     DSMSG         $MSG
054200970424     C                   EXSR      ERRORE
054300970424X   1C                   ELSE
054400991126     C                   Z-ADD     TASIMV        D50RIC
054500970424E   1C                   ENDIF
054600130301      * scrivo file delle varie
054700130507     c                   IF        not wNoWf
054800130301     c                   exsr      wrtVarie
054900130507     c                   ENDIF
055000970424     C*
055100970424     C                   ENDSR
055200970424     C**************************************************************************
055300970424     C*    CALCOLO COMPETENZE
055400970424     C**************************************************************************
055500970424     C     CALCOM        BEGSR
055600970424     C*
055700970424     C                   CLEAR                   DS$ECO
055800970424     C                   CLEAR                   TRECDS
055900060810     C                   EXSR      AZZW
056000970424     C                   MOVEL     D50TLA        D$GSTS                         *STATUS
056100970424     C                   MOVEL     'TNTE50R'     D$GPGF                         *PGM CHE HA MODIF.FI
056200060810     C                   EXSR      cddTAQ
056300060904     C                   EXSR      AZZnCDDTAQ
056400970424     C* CONTRASSEGNO
056500060809     c                   movel     csbtpa        D$DTPI
056600060810     c                   movel     csbsta        D$Dsta
056700060809     c                   if        csbfus <> *blanks
056800060809     c                   move      csbfus        D$DTPI
056900060809     c                   else
057000060809     c                   move      csbtpi        D$DTPI
057100060809     c                   end
057200020125     C                   Z-ADD     TASCAS        D$DCAS                         *LIRE CONTRASS
057300970424     C* ASSICURAZIONE
057400020125     C                   Z-ADD     TASIAS        D$DVAE
057500060809     C* TRANSITI
057600060809     C                   Z-ADD     §AR5flp       D$DAR5flp
057700060809     C                   Z-ADD     §AR5fl2       D$DAR5fl2
057800060809     C                   Z-ADD     §AR5fl3       D$DAR5fl3
057900060809     C                   Z-ADD     §AR5DE3       D$DAR5DE3
058000060809     C                   Z-ADD     §AR5DU3       D$DAR5DU3
058100060809     C                   Z-ADD     §AR5fl4       D$DAR5fl4
058200060809     C                   Z-ADD     §AR5DE4       D$DAR5DE4
058300060809     C                   Z-ADD     §AR5DU4       D$DAR5DU4
058400060809     C                   Z-ADD     §AR5fl5       D$DAR5fl5
058500060809     C                   Z-ADD     §AR5DE5       D$DAR5DE5
058600060809     C                   Z-ADD     §AR5DU5       D$DAR5DU5
058700060809     C                   Z-ADD     §AR5DET       D$DAR5DET
058800060809     C                   Z-ADD     §AR5DUT       D$DAR5DUT
058900060809     C                   Z-ADD     §AR5DE2       D$DAR5DE2
059000060809     C                   Z-ADD     §AR5DU2       D$DAR5DU2
059100030204     C*
059200030122     C                   Z-ADD     §AR5TBA       D$DAR5TBA                      *TIPO BANCALE 1
059300030122     C                   Z-ADD     §AR5NBA       D$DAR5NBA                      *NUM. BANCALE 1
059400030122     C                   Z-ADD     §AR5TB2       D$DAR5TB2                      *TIPO BANCALE 2
059500030122     C                   Z-ADD     §AR5NB2       D$DAR5NB2                      *NUM. BANCALE 2
059600030730     C*
059700030730     c                   IF        §AR5BNBA > *zeros OR
059800030730     c                             §AR5BNB2 > *zeros
059900030730     C                   Z-ADD     §AR5BTBA      D$DAR5TBA                      *TIPO BANCALE 1 BAR
060000030730     C                   Z-ADD     §AR5BNBA      D$DAR5NBA                      *NUM. BANCALE 1 BAR
060100030730     C                   Z-ADD     §AR5BTB2      D$DAR5TB2                      *TIPO BANCALE 2 BAR
060200030730     C                   Z-ADD     §AR5BNB2      D$DAR5NB2                      *NUM. BANCALE 2 BAR
060300030730     c                   ENDIF
060400040315     C                   Z-ADD     §AR5BCSF      D$DAR5CSF                      *NUM. BANCALE 2 BAR
060500040315     C                   Z-ADD     §AR5BCOR      D$DAR5COR                      *NUM. BANCALE 2 BAR
060600030730     C*
060700970424     C* SCHIERA RICAVI
060800030515     C     1             DO        75            I                              *AZZERA SCHIERE
060900970424     C                   Z-ADD     *ZEROS        DSWCN                          SCH NUM --> DS NUM
061000970424     C                   MOVE      DSWCA         CAL(I)                         DS ALF --> SCH ALF
061100970424     C                   Z-ADD     *ZEROS        DSWIN
061200970424     C                   MOVE      DSWIA         IAL(I)
061300970424     C                   ENDDO
061400970424     C                   Z-ADD     556           DSWCN                          SCH NUM --> DS NUM
061500970424     C                   MOVE      DSWCA         CAL(1)                         DS ALF --> SCH ALF
061600970424     C                   Z-ADD     D50RIC        DSWIN
061700970424     C                   MOVE      DSWIA         IAL(1)
061800970424     C                   MOVEA     CAL           D$DCCD                         *DS COD. RICAVI
061900970424     C                   MOVEA     IAL           D$DICD                         *DS IMP.RICAVI
062000060809     C                   MOVEL     tasmct        D$DMCT
062100060811     C                   MOVEA     SV            D$DSVN
062200060825     c                   do        20            k
062300060825     C                   MOVE      Va(k)         vn(k)
062400060825     c                   end
062500060825     C                   MOVEA     Vn            D$DVAN
062600060904     C* non considero la GIACENZA
062700970424     C                   MOVEL     'N'           D$DFGC                         *FORZATURA GIAC=NO
062800051212     C                   Z-ADD     0             D$DGGS
062900970424     C* NUMERO STOP DI RITIRO
063000020109     C                   Z-ADD     1             D$DNSR                         *N.STOP RITIRO
063100060810IF  1C     tasstr        IFGT      1
063200060810     C     1             DIV       tasstr        D$DNSR                         *1 / N. DI STOP
063300020109IF  2C     D$DNSR        IFLT      0,00001
063400020109     C                   Z-ADD     0,00001       D$DNSR
063500971024E   2C                   ENDIF
063600970424E   1C                   ENDIF
063700971027     C* NUMERO STOP DI CONSEGNA
063800971027     C                   Z-ADD     1             D$DNSC                         *N.STOP CONSEGNA
063900060810IF  1C     tasstp        IFGT      1
064000060810     C     1             DIV       tasstp        D$DNSC                         *1 / N. DI STOP
064100020109IF  2C     D$DNSC        IFLT      0,00001
064200020109     C                   Z-ADD     0,00001       D$DNSC
064300971027E   2C                   ENDIF
064400971027E   1C                   ENDIF
064500971024     C*
064600971024     C* IMPOSTA ALTRI CAMPI "DOPO"
064700980826     C                   MOVEL     §3ATB1        D$DTPO                         *TIPO PORTO F/A
064800060810     C     D$DFBR        IFNE      'S'
064900060810     C                   MOVEL     'S'           D$DFIG
065000060810     C                   ENDIF
065100970424     C*
065200970424     C                   MOVEL     D$DDS1        D$DRE1                         *DS DOPO
065300970424     C                   MOVEL     D$DDS2        D$DRE2
065400970424     C                   MOVEL     D$ODS1        D$ORE1                         *DS ORIG
065500970424     C                   MOVEL     D$ODS2        D$ORE2
065600970424     C*
065700970424     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
065800970424     C                   CALL      'TRECD8R'                            98
065900970424     C                   PARM                    DS$ECO                         *DS IMMAGINI BOLLA
066000970424     C*
066100970424     C* SE RITORNA ERRORE ESCO PGM
066200970424     C     D$GERR        IFNE      *BLANKS
066300970424     C                   MOVEL     D$GMSG        $MSG
066400970424     C                   EXSR      ERRORE
066500970424     C                   ENDIF
066600970424     C*
066700970424     C     $ERR          IFEQ      '0'                                          --- 1 -->
066800970424     C*
066900970424     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
067000970424     C                   MOVEA     D$DCCD        CAL                            SCHIERE ALFANUMERICH
067100970424     C                   MOVEA     D$DICD        IAL
067200030515     C     1             DO        75            I                 6 0
067300970424     C                   MOVE      CAL(I)        DSWCA                          SCH ALF --> DS ALF
067400970424     C                   Z-ADD     DSWCN         CD                6 0          DS NUM --> SCH NUM
067500970424     C     CD            IFNE      *ZEROS
067600970424     C                   MOVE      IAL(I)        DSWIA
067700090922     C                   Z-ADD     DSWIN         IMP              13 5
067800970424     C     CD            IFEQ      900                                          *RICAVO PRES. DELTA
067900090922     C                   Z-ADD(h)  IMP           D50COM
068000970424     C                   ENDIF
068100970424     C                   ENDIF
068200970424     C                   ENDDO
068300970424     C                   ENDIF                                                  <-- 1 ---
068400970424     C*
068500970424     C                   ENDSR
068600130301
068700130301      ********************************************************************
068800130301      *   wrtVarie - Scrivo file delle varie
068900130301      ********************************************************************
069000130301     c     wrtvarie      BEGSR
069100130301
069200130301     c                   clear                   wfdbf100
069300130301
069400130301     c                   eval      WF1aas = TASaas
069500130301     c                   eval      WF1mgs = TASmgs
069600130301     c                   eval      WF1lnp = TASlnp
069700130301     c                   eval      WF1nrs = TASnrs
069800130301     c                   eval      WF1nsp = TASnsp
069900130301     c                   eval      WF1ksc = TASksc
070000130301     c                   eval      WF1div = TASdiv
070100130301     c                   eval      WF1por = TASpor
070200130301
070300130301     c                   movea     sv            wf1_sv
070400130301     c                   movea     va            wf1_va
070500130301
070600130301     c                   eval      WF1imv = TASimv
070700130301
070800130301     c                   write     wfdbf100
070900130301
071000130301     c                   ENDSR
071100130301
071200060810     C********************************************************************
071300060810     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
071400060810     C********************************************************************
071500060810     C     AZZW          BEGSR
071600060810     C*
071700060810     C* AZZERO CAMPI DS ESTERNA
071800060810     c* è possibile usare all'interno del pgm solo i campi prima e dopo
071900060810     c* del TITAS senza testare i relativi flag prima o dopo.
072000060810     c* Se si vogliono usare i campi degli altri files senza testare i
072100060810     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
072200060810     c* contenute nel /COPY
072300060810     C                   EXSR      APDTAQ
072400060810     C                   EXSR      ADDTAQ
072500060810     C                   EXSR      AODTAQ
072600060810     C                   MOVEL     D$ODS1        D$ORE1
072700060810     C                   MOVEL     D$ODS2        D$ORE2
072800060810     C                   MOVEL     *BLANKS       D$PCO1
072900060810     C                   MOVEL     *BLANKS       D$PCO2
073000060810     C                   MOVEL     *BLANKS       D$DCO1
073100060810     C                   MOVEL     *BLANKS       D$DCO2
073200060810     C                   MOVEL     *BLANKS       D$PSVN
073300060810     C                   MOVEL     *BLANKS       D$PVAN
073400060810     C                   MOVEL     *BLANKS       D$DSVN
073500060810     C                   MOVEL     *BLANKS       D$DVAN
073600060810     C                   MOVEL     *BLANKS       D$OSVN
073700060810     C                   MOVEL     *BLANKS       D$OVAN
073800060810     C                   Z-ADD     *ZEROS        D$PCAS
073900060810     C                   MOVEL     *BLANKS       D$PTPI
074000060810     C                   MOVEL     *BLANKS       D$Psta
074100060810     C                   Z-ADD     *ZEROS        D$DCAS
074200060810     C                   MOVEL     *BLANKS       D$DTPI
074300060810     C                   MOVEL     *BLANKS       D$Dsta
074400060810     C                   Z-ADD     *ZEROS        D$PVAE
074500060810     C                   Z-ADD     *ZEROS        D$DVAE
074600060810     C                   Z-ADD     *ZEROS        D$PNSR
074700060810     C                   Z-ADD     *ZEROS        D$DNSR
074800060810     C                   Z-ADD     *ZEROS        D$PAGC
074900060810     C                   Z-ADD     *ZEROS        D$PMGC
075000060810     C                   Z-ADD     *ZEROS        D$DAGC
075100060810     C                   Z-ADD     *ZEROS        D$DMGC
075200060810     C                   MOVEL     *BLANKS       D$PTPO
075300060810     C                   MOVEL     *BLANKS       D$PRBL
075400060810     C                   MOVEL     'N'           D$PCSF
075500060810     C                   MOVEL     'N'           D$PCCS
075600060810     C                   MOVEL     'N'           D$PDBO
075700060810     C                   MOVEL     'N'           D$PFRI
075800060810     C                   MOVEL     'N'           D$PFSP
075900060810     C                   MOVEL     *BLANKS       D$DTPO
076000060810     C                   MOVEL     *BLANKS       D$DRBL
076100060810     C                   MOVEL     'N'           D$DCSF
076200060810     C                   MOVEL     'N'           D$DCCS
076300060810     C                   MOVEL     'N'           D$DDBO
076400060810     C                   MOVEL     'N'           D$DFRI
076500060810     C                   MOVEL     'N'           D$DFSP
076600060810     C                   MOVEL     *BLANKS       D$PAR5T
076700060810     C                   MOVEL     *BLANKS       D$DAR5T
076800060810     C                   MOVEL     *BLANKS       D$PAR5B
076900060810     C                   MOVEL     *BLANKS       D$DAR5B
077000060810     C                   Z-ADD     *ZEROS        D$PAR5flp
077100060810     C                   Z-ADD     *ZEROS        D$PAR5DET
077200060810     C                   Z-ADD     *ZEROS        D$PAR5DUT
077300060810     C                   Z-ADD     *ZEROS        D$PAR5fl2
077400060810     C                   Z-ADD     *ZEROS        D$PAR5DE2
077500060810     C                   Z-ADD     *ZEROS        D$PAR5DU2
077600060810     C                   Z-ADD     *ZEROS        D$PAR5fl3
077700060810     C                   Z-ADD     *ZEROS        D$PAR5DE3
077800060810     C                   Z-ADD     *ZEROS        D$PAR5DU3
077900060810     C                   Z-ADD     *ZEROS        D$PAR5fl4
078000060810     C                   Z-ADD     *ZEROS        D$PAR5DE4
078100060810     C                   Z-ADD     *ZEROS        D$PAR5DU4
078200060810     C                   Z-ADD     *ZEROS        D$PAR5fl5
078300060810     C                   Z-ADD     *ZEROS        D$PAR5DE5
078400060810     C                   Z-ADD     *ZEROS        D$PAR5DU5
078500060810     C                   Z-ADD     *ZEROS        D$DAR5flp
078600060810     C                   Z-ADD     *ZEROS        D$DAR5DET
078700060810     C                   Z-ADD     *ZEROS        D$DAR5DUT
078800060810     C                   Z-ADD     *ZEROS        D$DAR5fl2
078900060810     C                   Z-ADD     *ZEROS        D$DAR5DE2
079000060810     C                   Z-ADD     *ZEROS        D$DAR5DU2
079100060810     C                   Z-ADD     *ZEROS        D$DAR5fl3
079200060810     C                   Z-ADD     *ZEROS        D$DAR5DE3
079300060810     C                   Z-ADD     *ZEROS        D$DAR5DU3
079400060810     C                   Z-ADD     *ZEROS        D$DAR5fl4
079500060810     C                   Z-ADD     *ZEROS        D$DAR5DE4
079600060810     C                   Z-ADD     *ZEROS        D$DAR5DU4
079700060810     C                   Z-ADD     *ZEROS        D$DAR5fl5
079800060810     C                   Z-ADD     *ZEROS        D$DAR5DE5
079900060810     C                   Z-ADD     *ZEROS        D$DAR5DU5
080000060810     C                   Z-ADD     *ZEROS        D$PAR5TBA
080100060810     C                   Z-ADD     *ZEROS        D$PAR5NBA
080200060810     C                   Z-ADD     *ZEROS        D$PAR5TB2
080300060810     C                   Z-ADD     *ZEROS        D$PAR5NB2
080400060810     C                   Z-ADD     *ZEROS        D$DAR5TBA
080500060810     C                   Z-ADD     *ZEROS        D$DAR5NBA
080600060810     C                   Z-ADD     *ZEROS        D$DAR5TB2
080700060810     C                   Z-ADD     *ZEROS        D$DAR5NB2
080800060810     C                   Z-ADD     *ZEROS        D$PAR5CSF
080900060810     C                   Z-ADD     *ZEROS        D$PAR5COR
081000060810     C                   Z-ADD     *ZEROS        D$DAR5CSF
081100060810     C                   Z-ADD     *ZEROS        D$DAR5COR
081200060810     C                   MOVEL     *BLANKS       D$XXXX
081300060810     C*
081400060810     C                   ENDSR
081500060904     C*-------------------------------------------------------------------
081600060904     C*?Azzera varia "N" immagine -dopo- TITAS
081700060904     C*-------------------------------------------------------------------
081800060904     C     AZZnCDDTAQ    BEGSR
081900060904     c*
082000060904if  1C                   IF        (D$DVA1 = 88888888,000)
082100060904     C                   Z-ADD     *zeros        D$DVA1
082200060904e   1c                   ENDIF
082300060904if  1C                   IF        (D$DVA2 = 88888888,000)
082400060904     C                   Z-ADD     *zeros        D$DVA2
082500060904e   1c                   ENDIF
082600060904if  1C                   IF        (D$DVA3 = 88888888,000)
082700060904     C                   Z-ADD     *zeros        D$DVA3
082800060904e   1c                   ENDIF
082900060904     c*
083000060904     C                   ENDSR
083100970424     C********************************************************************
083200970424     C*   CALDEL - CALCOLA IL DELTA
083300970424     C*          INPUT  --> WRRW: RICAVO REALE
083400970424     C*                     WRPW: RICAVO PRESUNTO
083500970424     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
083600970424     C********************************************************************
083700970424     C     CALDEL        BEGSR
083800970424     C*
083900970424     C*--------------------
084000970424     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
084100970424     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
084200970424     C*--------------------
084300970424     C*
084400970424     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
084500970424     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
084600970424     C     GAP           MULT      100           GAP                            (GAP x 100)
084700970424     C*
084800970424     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
084900970424IF  1C     WRRW          IFEQ      *ZEROS
085000970424     C                   Z-ADD     1             WRRW
085100970424E   1C                   ENDIF
085200970424     C*
085300970424     C     GAP           DIV       WRRW          GAP
085400970424     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
085500970424     C*
085600970424     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
085700970424     C     WDPW          COMP      0                                    28
085800970424     C* POSITIVO
085900970424IF  1C     *IN28         IFEQ      *OFF
086000970424IF  2C     GAP           IFGE      1000
086100970424     C                   Z-ADD     +999,9        WDPW
086200970424E   2C                   ENDIF
086300970424     C* NEGATIVO
086400970424X   1C                   ELSE
086500970424IF  2C     GAP           IFLE      -1000
086600970424     C                   Z-ADD     -999,9        WDPW
086700970424E   2C                   ENDIF
086800970424E   1C                   ENDIF
086900970424     C*
087000970424     C                   ENDSR
087100970424     C********************************************************************
087200970424     C*   ERRORE - ROUTINE GESTIONE ERRORI
087300970424     C********************************************************************
087400970424     C     ERRORE        BEGSR
087500970424     C*
087600970424     C                   MOVEL     'S'           $ERR
087700970424     C                   MOVEL     $MSG          D50MSG
087800970424     C                   MOVE      $MSG          D50ERR
087900970424     C*
088000970424     C                   ENDSR
088100011012     C*------------------------------------------------------------------------*
088200011012     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
088300011012     C*------------------------------------------------------------------------*
088400011012     C     CARGEDCE      BEGSR
088500011012     C*
088600011012     C                   CLEAR                   DSGEDCE
088700011026     C                   MOVEL     *blanks       WDIVCE            3
088800011012     C*
088900011012     C                   MOVEL(P)  'GED'         KTECOD
089000011012     C                   MOVEL(P)  'CE'          KTEKE1
089100011012     C     KEYTBE        CHAIN     TNTBE01L
089200011012     C                   IF        %found(TNTBE01L)
089300011012     C                   MOVEL     TBEUNI        DSGEDCE
089400011012     C                   ENDIF
089500011012     C*
089600011102     C                   IF        §GEDCEDAT > DATCOR
089700011026     C                   EVAL      WDIVCE = §GEDCEDIP
089800011026     C                   ELSE
089900011026     C                   EVAL      WDIVCE = §GEDCEDIV
090000011012     C                   ENDIF
090100011012     C*
090200011012     C                   ENDSR
090300011102     C*------------------------------------------------------------------------*
090400011102     C* CNVDTAS - CONVERTE I CAMPI DELLE DS DTAS/DTASV NELLA DIVISA DI CALCOLO DEL CE
090500011102     C*------------------------------------------------------------------------*
090600011102     C     CNVDTAS       BEGSR
090700011102     C*
090800011102     C* CONTRASSEGNO
090900011102     C                   Z-ADD     TASCAS        WIMI             30 9
091000011107     C                   MOVEL     TASVCA        WDVI              3
091100011102     C                   EXSR      CNVEUR
091200011102     C                   Z-ADD     WIMO          TASCAS
091300011102     C*
091400011102     C* IMPORTO DA ASSICURARE
091500011102     C                   Z-ADD     TASIAS        WIMI             30 9
091600011107     C                   MOVEL     TASVAS        WDVI              3
091700011102     C                   EXSR      CNVEUR
091800011102     C                   Z-ADD     WIMO          TASIAS
091900011102     C*
092000011102     C* QUANTITA' DA FATTURARE (non viene convertita x ambiguità su suo utilizzo)
092100011102     C*
092200011102     C* TRASPORTO
092300011102     C                   Z-ADD     TASPOR        WIMI             30 9
092400011102     C                   MOVEL     TASDIV        WDVI              3
092500011102     C                   EXSR      CNVEUR
092600011102     C                   Z-ADD     WIMO          TASPOR
092700011102     C*
092800011102     C* TOTALE IMPONIBILE
092900011102     C                   Z-ADD     TASIMV        WIMI             30 9
093000011102     C                   MOVEL     TASDIV        WDVI              3
093100011102     C                   EXSR      CNVEUR
093200011102     C                   Z-ADD     WIMO          TASIMV
093300011102     C*
093400011102     C* IMPORTO ASS. IN MONETA O MAX VALORE ...
093500011102     C                   Z-ADD     TASIAL        WIMI             30 9
093600011123     C                   MOVEL     TASDIV        WDVI              3
093700011102     C                   EXSR      CNVEUR
093800011102     C                   Z-ADD     WIMO          TASIAL
093900011102     C*
094000011102     C* VARIE VARIE
094100011102     C                   Z-ADD     TASVA1        WIMI             30 9
094200011102     C                   MOVEL     TASDIV        WDVI              3
094300011102     C                   EXSR      CNVEUR
094400011102     C                   Z-ADD     WIMO          TASVA1
094500011102     C                   Z-ADD     TASVA2        WIMI             30 9
094600011102     C                   MOVEL     TASDIV        WDVI              3
094700011102     C                   EXSR      CNVEUR
094800011102     C                   Z-ADD     WIMO          TASVA2
094900011102     C                   Z-ADD     TASVA3        WIMI             30 9
095000011102     C                   MOVEL     TASDIV        WDVI              3
095100011102     C                   EXSR      CNVEUR
095200011102     C                   Z-ADD     WIMO          TASVA3
095300011102     C                   Z-ADD     TASVA4        WIMI             30 9
095400011102     C                   MOVEL     TASDIV        WDVI              3
095500011102     C                   EXSR      CNVEUR
095600011102     C                   Z-ADD     WIMO          TASVA4
095700011102     C                   Z-ADD     TASVA5        WIMI             30 9
095800011102     C                   MOVEL     TASDIV        WDVI              3
095900011102     C                   EXSR      CNVEUR
096000011102     C                   Z-ADD     WIMO          TASVA5
096100011102     C                   Z-ADD     TASVA6        WIMI             30 9
096200011102     C                   MOVEL     TASDIV        WDVI              3
096300011102     C                   EXSR      CNVEUR
096400011102     C                   Z-ADD     WIMO          TASVA6
096500011102     C                   Z-ADD     TASVA7        WIMI             30 9
096600011102     C                   MOVEL     TASDIV        WDVI              3
096700011102     C                   EXSR      CNVEUR
096800011102     C                   Z-ADD     WIMO          TASVA7
096900011102     C                   Z-ADD     TASVA8        WIMI             30 9
097000011102     C                   MOVEL     TASDIV        WDVI              3
097100011102     C                   EXSR      CNVEUR
097200011102     C                   Z-ADD     WIMO          TASVA8
097300011102     C                   Z-ADD     TASVA9        WIMI             30 9
097400011102     C                   MOVEL     TASDIV        WDVI              3
097500011102     C                   EXSR      CNVEUR
097600011102     C                   Z-ADD     WIMO          TASVA9
097700011102     C                   Z-ADD     TASVA0        WIMI             30 9
097800011102     C                   MOVEL     TASDIV        WDVI              3
097900011102     C                   EXSR      CNVEUR
098000011102     C                   Z-ADD     WIMO          TASVA0
098100011102     C                   Z-ADD     TASV11        WIMI             30 9
098200011102     C                   MOVEL     TASDIV        WDVI              3
098300011102     C                   EXSR      CNVEUR
098400011102     C                   Z-ADD     WIMO          TASV11
098500011102     C                   Z-ADD     TASV12        WIMI             30 9
098600011102     C                   MOVEL     TASDIV        WDVI              3
098700011102     C                   EXSR      CNVEUR
098800011102     C                   Z-ADD     WIMO          TASV12
098900011102     C                   Z-ADD     TASV13        WIMI             30 9
099000011102     C                   MOVEL     TASDIV        WDVI              3
099100011102     C                   EXSR      CNVEUR
099200011102     C                   Z-ADD     WIMO          TASV13
099300011102     C                   Z-ADD     TASV14        WIMI             30 9
099400011102     C                   MOVEL     TASDIV        WDVI              3
099500011102     C                   EXSR      CNVEUR
099600011102     C                   Z-ADD     WIMO          TASV14
099700011102     C                   Z-ADD     TASV15        WIMI             30 9
099800011102     C                   MOVEL     TASDIV        WDVI              3
099900011102     C                   EXSR      CNVEUR
100000011102     C                   Z-ADD     WIMO          TASV15
100100011102     C                   Z-ADD     TASV16        WIMI             30 9
100200011102     C                   MOVEL     TASDIV        WDVI              3
100300011102     C                   EXSR      CNVEUR
100400011102     C                   Z-ADD     WIMO          TASV16
100500011102     C                   Z-ADD     TASV17        WIMI             30 9
100600011102     C                   MOVEL     TASDIV        WDVI              3
100700011102     C                   EXSR      CNVEUR
100800011102     C                   Z-ADD     WIMO          TASV17
100900011102     C                   Z-ADD     TASV18        WIMI             30 9
101000011102     C                   MOVEL     TASDIV        WDVI              3
101100011102     C                   EXSR      CNVEUR
101200011102     C                   Z-ADD     WIMO          TASV18
101300011102     C                   Z-ADD     TASV19        WIMI             30 9
101400011102     C                   MOVEL     TASDIV        WDVI              3
101500011102     C                   EXSR      CNVEUR
101600011102     C                   Z-ADD     WIMO          TASV19
101700011102     C                   Z-ADD     TASV20        WIMI             30 9
101800011102     C                   MOVEL     TASDIV        WDVI              3
101900011102     C                   EXSR      CNVEUR
102000011102     C                   Z-ADD     WIMO          TASV20
102100011102     C*
102200011102     C                   ENDSR
102300011012     C*------------------------------------------------------------------------*
102400011012     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
102500011012     C*------------------------------------------------------------------------*
102600011012     C     CNVEUR        BEGSR
102700011112     C*
102800011112     C* AZZERA VARIABILI DI LAVORO
102900011112     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
103000011112     C                   MOVEL     '9'           NDEC              1            *N. DEC IN OUTPUT
103100011112     C*
103200011112     C* DI DEFUALT ATTRIBUISCO LO STESSO VALORE AGLI IMPORTI "DA CONVERTIRE" E "CONVERTITI"
103300011112     C                   Z-ADD     WIMI          WIMO
103400011107     C*
103500011107     C* COME PRIMA COSA AI FINI DELLA CONVERSIONE TESTO SE LE DIVISE SONO DIVERSE
103600011107     C     WDVI          IFNE      WDIVCE
103700011012     C*
103800011012IF  1C     WIMI          IFNE      *ZEROS
103900011012     C                   CLEAR                   YEUR
104000011012     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
104100011012     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
104200011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
104300011012     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
104400011012     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
104500011012     C                   CALL      'YEURCO'
104600011012     C                   PARM                    YEUR
104700011012IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
104800011012     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
104900011012X   2C                   ELSE                                                   *SI ERRORI
105000011012     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
105100011012E   2C                   ENDIF
105200011012E   1C                   ENDIF
105300011012     C*
105400011107     C                   ENDIF
105500011107     C*
105600011012     C                   ENDSR
105700970424     C********************************************************************
105800970424     C*   RUTINZ - ROUTINE INIZIALE
105900970424     C********************************************************************
106000970424     C     RUTINZ        BEGSR
106100970424     C*
106200970424     C* PARAMETRI IN INPUT ALLA ROUTINE
106300970424     C     *ENTRY        PLIST
106400970424     C                   PARM                    DS50
106500130228     C                   PARM                    parddt
106600130507     C                   PARM                    partest
106700130228
106800130228      /free
106900130507         wNoWf = *off;
107000130228       //?Se non passato il paramtro della data decorrenza tariffa/offerta
107100130228       //?pulisco il campo
107200130228         IF  %parms = 1;
107300130228           clear dataddt;
107400130228         ELSE;
107500130228           dataddt = %int(parddt);
107600130228         ENDIF;
107700130507       //?Se passato il terzo parametro non devo aprire il file di work
107800130507         IF  %parms > 2;
107900130507           wNoWf = *on;
108000130507         ENDIF;
108100130507       //?apro il file di work
108200130509         IF  not wNoWf and not %open(WFDBF10F);
108300130507           open WFDBF10F;
108400130507         ENDIF;
108500130228      /end-free
108600970424     C*
108700060809     C* CHIAVE LETTURA titas30c - COMPLETA
108800060809     C     KEYtas        KLIST
108900060809     C                   KFLD                    tasAAS
109000060809     C                   KFLD                    tasLNP
109100060809     C                   KFLD                    tasNRS
109200060809     C                   KFLD                    tasNSP
109300060809      *  KLIST PER ACCESSO FILES
109400060809     c     KTA7          KLIST
109500060809     c                   KFLD                    TASAAS
109600060809     c                   KFLD                    TASLNP
109700060809     c                   KFLD                    TASNRS
109800060809     c                   KFLD                    TASNSP
109900060809     c                   KFLD                    TASTBL
110000030122     C*
110100060809     C* CHIAVE LETTURA FIAR531c - COMPLETA
110200030122     C     KEYAR5        KLIST
110300030122     C                   KFLD                    AR5AAS
110400030122     C                   KFLD                    AR5LNP
110500030122     C                   KFLD                    AR5NRS
110600030122     C                   KFLD                    AR5NSP
110700030122     C                   KFLD                    AR5TRD
110800060809     C                   KFLD                    AR5prg
110900970424     C*
111000060809     C* CHIAVE LETTURA tncsb03l - COMPLETA
111100060809     C     KEYcsb        KLIST
111200060809     C                   KFLD                    csbAAS
111300060809     C                   KFLD                    csbLNP
111400060809     C                   KFLD                    csbNRS
111500060809     C                   KFLD                    csbNSP
111600970424     C*
111700970424     C* CHIAVE LETTURA TABEL00F - COMPLETA
111800970424     C     KEYTAB        KLIST
111900970424     C                   KFLD                    KTBUTE
112000970424     C                   KFLD                    KTBCOD
112100970424     C                   KFLD                    KTBKEY
112200011012     C*
112300011012     C* LETTURA TNTBE01L - PARZIALE
112400011012     C     KEYTBE        KLIST
112500011012     C                   KFLD                    KTECOD                         *UTENTE
112600011012     C                   KFLD                    KTEKE1                         *CODICE
112700080908      * Chiave file eventi FNEVB
112800080908     c     Kevb4         Klist
112900080908     c                   Kfld                    Tasaas
113000080908     c                   Kfld                    Taslnp
113100080908     c                   Kfld                    Tasnrs
113200080908     c                   Kfld                    Tasnsp
113300080908     c                   Kfld                    Wcev
113400970424     C* DEFINISCO CAMPI CHIAVE
113500011012     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
113600970424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
113700970424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
113800011012     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
113900011012     C     *LIKE         DEFINE    TBEKE1        KTEKE1
114000970424     C*
114100970424     C* RECUPERO IL CODICE DELL'AZIENDA
114200970424     C                   Z-ADD     1             CODUT             1 0
114300970424     C                   MOVEL     '0'           $ERR              1
114400970424     C                   MOVEL     *BLANKS       $MSG             78
114500011015     C*
114600011015     C* REPERISCE LA DATA CORRENTE
114700011015     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
114800011015     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
114900011015     C                   Z-ADD     WN8           G08DAT
115000011015     C                   Z-ADD     *ZEROS        G08INV
115100011015     C                   MOVEL     '0'           G08ERR
115200011015     C                   CALL      'XSRDA8'
115300011015     C                   PARM                    WLBDA8
115400011015     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
115500011012     C*
115600011012     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
115700011012     C                   EXSR      CARGEDCE
115800141211
115900141211      /free
116000141211       //?Carico part.consegna con abilitazione PinCode
116100141211         KTBcod = '7R';
116200141211         clear k;
116300141211         setll (KTBute:KTBcod) TABEL00F;
116400141211         reade (KTBute:KTBcod) TABEL00F;
116500141211         DOW  not %eof(TABEL00F);
116600141211           IF  TBLflg = *blanks;
116700141211             ds7R = TBLuni;
116800141211             IF  §7Rpincode = 'S';
116900141211               k += 1;
117000141211               skGMA(k) = TBLkey;
117100141211             ENDIF;
117200141211           ENDIF;
117300141211           reade (KTBute:KTBcod) TABEL00F;
117400141211         ENDDO;
117500141211      /end-free
117600141211
117700970424     C*
117800970424     C* AZZERO I CAMPI IN OUTPUT DEL PGM
117900970424     C                   EXSR      AZZERA
118000970424     C*
118100970424     C                   ENDSR
118200970424     C***********************************************************************
118300970424     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
118400970424     C***********************************************************************
118500970424     C     AZZERA        BEGSR
118600970424     C*
118700970424     C                   MOVEL     *BLANKS       D50ERR
118800970424     C                   MOVEL     *BLANKS       D50MSG
118900970424     C                   Z-ADD     *ZEROS        D50RIC
119000970424     C                   Z-ADD     *ZEROS        D50COM
119100970424     C                   Z-ADD     *ZEROS        D50DEL
119200970424     C*
119300970424     C                   ENDSR
119400060810      /COPY GAITRASRC/SRCTREC,TRECDT
119500970424     C********************************************************************
119600970424** ======== SCHIERA: MSG  (MESSAGGI)                       ================
119700970424TNTE50R- Bolla partenza non trovata o annullata.                          BPNT 1
119800970424TNTE50R- Valuta non trovata nella tab. CV (cambi valuta).                 VNTT 2
119900970424TNTE50R- CAP mittente non trovato.                                        CAPN 3
120000970424TNTE50R- Bolla non calcolabile.                                           NONC 4
120100970424TNTE50R- Tipo bolla non trovato in tabella 3A. Avvertire CED.             3ANT 5
120200970424TNTE50R- 6                                                                ERRO 6
120300970424TNTE50R- 7                                                                ERRO 7
120400970424TNTE50R- 8                                                                ERRO 8
120500970424TNTE50R- 9                                                                ERRO 9
120600970424TNTE50R- 10                                                               ERRO10
120700970424TNTE50R- 11                                                               ERRO11
120800970424TNTE50R- 12                                                               ERRO12
120900970424TNTE50R- 13                                                               ERRO13
121000970424TNTE50R- 14                                                               ERRO14
121100970424TNTE50R- 15                                                               ERRO15
121200970424TNTE50R- 16                                                               ERRO16
121300970424TNTE50R- 17                                                               ERRO17
121400970424TNTE50R- 18                                                               ERRO18
121500970424TNTE50R- 19                                                               ERRO19
121600970424TNTE50R- 20                                                               ERRO20
