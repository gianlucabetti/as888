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
006400130301
006500130301      * - Ds per varie file di lavoro
006600130301     d                 ds
006700130301     d  WF1sv1                 1      1
006800130301     d  WF1sv2                 2      2
006900130301     d  WF1sv3                 3      3
007000130301     d  WF1sv4                 4      4
007100130301     d  WF1sv5                 5      5
007200130301     d  WF1sv6                 6      6
007300130301     d  WF1sv7                 7      7
007400130301     d  WF1sv8                 8      8
007500130301     d  WF1sv9                 9      9
007600130301     d  WF1s10                10     10
007700130301     d  WF1s11                11     11
007800130301     d  WF1s12                12     12
007900130301     d  WF1s13                13     13
008000130301     d  WF1s14                14     14
008100130301     d  WF1s15                15     15
008200130301     d  WF1s16                16     16
008300130301     d  WF1s17                17     17
008400130301     d  WF1s18                18     18
008500130301     d  WF1s19                19     19
008600130301     d  WF1s20                20     20
008700130301     d  WF1_sv                 1     20    Dim(20)                              Sigle varie
008800130301      * - Ds per varie file di lavoro
008900130301     d                 ds
009000130301     d  WF1va1                 1     11  3
009100130301     d  WF1va2                12     22  3
009200130301     d  WF1va3                23     33  3
009300130301     d  WF1va4                34     44  3
009400130301     d  WF1va5                45     55  3
009500130301     d  WF1va6                56     66  3
009600130301     d  WF1va7                67     77  3
009700130301     d  WF1va8                78     88  3
009800130301     d  WF1va9                89     99  3
009900130301     d  WF1v10               100    110  3
010000130301     d  WF1v11               111    121  3
010100130301     d  WF1v12               122    132  3
010200130301     d  WF1v13               133    143  3
010300130301     d  WF1v14               144    154  3
010400130301     d  WF1v15               155    165  3
010500130301     d  WF1v16               166    176  3
010600130301     d  WF1v17               177    187  3
010700130301     d  WF1v18               188    198  3
010800130301     d  WF1v19               199    209  3
010900130301     d  WF1v20               210    220  3
011000130301     d  WF1_va                 1    220  3 Dim(20)                              Importo varie
011100040806     D*-------------------
011200040806     D* DS AR5UNI TIP. REC. GEN
011300040806     D*-------------------
011400040806     D DAR5GEN       E DS
011500060809     D DAR5TRS       E DS
011600040806     D*-------------------
011700040806     D* DS TASFLO
011800040806     D*-------------------
011900040806     D DTASFLO       E DS
012000970424     D*------------------
012100970424     D* DS "TISI95R" - CONTROLLO CAP
012200970424     D*------------------
012300970424     D DSSI95        E DS                  EXTNAME(TISI95DS)
012400970424     D*-------------------
012500970424     D* DS CALCOLO COMPETENZE
012600970424     D*-------------------
012700970424     D DS$ECO        E DS
012800970424     D*-------------------
012900970424     D* DS RIDEFINIZIONE IMMAGINE PRIMA E DOPO DI TNTAS10F
013000970424     D*-------------------
013100970424     D TRECDS        E DS
013200970424     D  D$PDS1                 1    250
013300970424     D  D$PDS2               251    500
013400970424     D  D$DDS1               501    750
013500970424     D  D$DDS2               751   1000
013600970424     D  D$ODS1              1001   1250
013700970424     D  D$ODS2              1251   1500
013800030122     D*-------------------
013900030122     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
014000030122     D*-------------------
014100030122     D DAR5BAN       E DS
014200030729     D*-------------------
014300030729     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI BARTOLINI
014400030729     D*-------------------
014500030729     D DAR5BNB       E DS
014600970424     D*-------------------
014700970424     D* DS PASSAGGIO CAMPI PACKED -CODICE-
014800970424     D*-------------------
014900970424     D                 DS
015000970424     D  DSWCN                  1      3P 0
015100970424     D  DSWCA                  1      3
015200090907     D  DSWIN                  4     13P 5
015300020109     D  DSWIA                  4     13
015400970424     D*-------------------
015500970424     D* DS PER MESSAGGI
015600970424     D*-------------------
015700970424     D                 DS
015800970424     D  DSMSG                  1     78
015900970424     D  DSERR                 75     78
016000980826     D*-------------------
016100980826     D* DS PER COMPORRE DATA SPED.
016200980826     D*-------------------
016300980826     D                 DS
016400060809     D  tasAAS                 1      4  0
016500060809     D  tasMGS                 5      8  0
016600060809     D  tasDSPt                1      8  0
016700970424     D*-------------------
016800970424     D* ARCHITETTURA
016900970424     D*-------------------
017000970424     D KPJBA         E DS
017100011012     D*------------------
017200011012     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
017300011012     D*------------------
017400011012     D DSGEDCE       E DS
017500011012     D*-------------------
017600011012     D* CONVERSIONI IMPORTI
017700011012     D*-------------------
017800011012     D YEUR          E DS                  EXTNAME(YEURCODS)
017900011015     D*-------------------
018000011015     D* DS "XSRDA8" - CONTROLLO DATA (8)
018100011015     D*-------------------
018200011015     D WLBDA8          DS                  INZ
018300011015     D  G08DAT                 1      8  0
018400011015     D  G08INV                 9     16  0
018500011015     D  G08ERR                17     17
018600011015     D  G08TGI                18     22  0
018700130228
018800130228       // -?Data decorrenza tariffa/offerta
018900130228     d parddt          s              8
019000130228     d dataddt         s              8  0
019100130507
019200130507       // -?Flag booleani
019300130507     d wNoWf           s               n   inz(*off)
019400130507
019500130507       // -?Campi
019600130507     d partest         s              1
019700130507     d k               s              3  0
019800130228
019900060809     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS39C
020000060809     ITITAS000      01
020100060809     ITITAS010      02
020200140521     ITITASP00      03
020300970424     C*------------------------------------------------------------------------*
020400970424     C* MAIN LINE
020500970424     C*------------------------------------------------------------------------*
020600970424     C*
020700970424     C* OPERAZIONI INIZIALI
020800970424     C                   EXSR      RUTINZ
020900970424     C*
021000970424     C* SE LANCIATO CON TIPO LANCIO='C' -CHIUDE FILE-, VA A FINE PROGRAMMA
021100970424IF  1C     $ERR          IFEQ      '0'
021200970424IF  2C     D50TLA        IFNE      'C'
021300970424     C*
021400970424     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
021500970424IF  3C     $ERR          IFEQ      '0'
021600970424IF  4C     D50NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
021700060809     C                   EXSR      LETtas                                       *LEGGE BOLLA PART.
021800970424E   4C                   ENDIF
021900970424E   3C                   ENDIF
022000970424     C*
022100970424     C* CALCOLA I RICAVI
022200970424IF  3C     $ERR          IFEQ      '0'
022300970424     C                   EXSR      TASSAZ
022400970424E   3C                   ENDIF
022500970424     C*
022600970424     C* CALCOLA LE COMPETENZE
022700970424IF  3C     $ERR          IFEQ      '0'
022800060904     C*    §3ATB1        ANDEQ     'F1'
022900970424     C                   EXSR      CALCOM
023000971027E   3C                   ENDIF
023100970424     C*
023200970424     C* CALCOLA IL DELTA
023300970424     C                   Z-ADD     D50RIC        WRRW             16 3          *RICAVO REALE
023400970424     C                   Z-ADD     D50COM        WRPW             16 3          *RICAVO PRESUNTO
023500970424     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
023600970424     C                   EXSR      CALDEL
023700970424     C                   Z-ADD     WDPW          D50DEL                         *DELTA
023800970424     C*
023900970424E   2C                   ENDIF
024000970424E   1C                   ENDIF
024100970424     C*
024200970424     C* OPERAZIONI DI CHIUSURA
024300970424IF  1C     D50TLA        IFEQ      'C'
024400970424     C*
024500970424     C                   MOVEL     'C'           D$GSTS
024600970424     C                   CALL      'TRECD8R'                            98
024700970424     C                   PARM                    DS$ECO
024800970424     C*
024900970424     C                   MOVEL     'C'           TASTLA
025000130228     C                   CALL      'TNSF20R'                            99
025100970424     C                   PARM                    KPJBA
025200991126     C                   PARM                    DTAS
025300991126     C                   PARM                    DTASV
025400970424     C*
025500970424     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
025600970424     C                   MOVEL     'C'           I95TLA
025700970424     C                   CALL      'TISI95R'
025800970424     C                   PARM                    DSSI95
025900970424     C* FINE PROGRAMMA
026000970424     C                   SETON                                        LR
026100970424X   1C                   ELSE
026200970424     C                   SETON                                        RT
026300970424E   1C                   ENDIF
026400970424     C*------------------------------------------------------------------------*
026500060809     C* LETtas - LEGGE LE BOLLE PARTENZA
026600970424     C*------------------------------------------------------------------------*
026700060809     C     LETtas        BEGSR
026800970424     C*
026900060809     C                   Z-ADD     D50AAS        tasAAS
027000060809     C                   Z-ADD     D50LNP        tasLNP
027100060809     C                   Z-ADD     D50NRS        tasNRS
027200060809     C                   Z-ADD     D50NSP        tasNSP
027300060809     c                   setoff                                       0102
027400060809     C     KEYtas        CHAIN     titas30c                           99
027500970424IF  1C     *IN99         IFEQ      '1'                                          *NON TROV.
027600970424     C                   MOVEL     MSG(1)        $MSG                           *BOLLA NON TROV. O A
027700011102     C                   EXSR      ERRORE
027800970424E   1C                   ENDIF
027900970424     C*
028000970424     C* RECUPERA IL CODICE BOLLA
028100970424IF  1C     $ERR          IFEQ      '0'
028200970424     C                   Z-ADD     CODUT         KTBUTE
028300970424     C                   MOVEL     '3A'          KTBCOD
028400970424     C                   MOVEL     *BLANKS       KTBKEY
028500060809     C                   MOVEL     tasCBO        KTBKEY
028600970424     C     KEYTAB        CHAIN     TABEL00F                           99
028700970424IF  2C     *IN99         IFEQ      '1'
028800970424     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
028900970424     C                   EXSR      ERRORE
029000060904     c                   else
029100060904     C                   MOVEL     TBLUNI        DS3A
029200060904     C                   MOVEL     §3ATB1        FLGTB1            1
029300970424E   2C                   ENDIF
029400970424E   1C                   ENDIF
029500060904     c* solo tipi bolla Normali (F1 e A2)
029600060904IF  1C     $ERR          IFEQ      '0'
029700060904     C                   Z-ADD     CODUT         KTBUTE
029800060904     C                   MOVEL     'TB'          KTBCOD
029900060904     C                   MOVEL     *BLANKS       KTBKEY
030000060904     C                   MOVEL     §3ATB1        KTBKEY
030100060904     C     KEYTAB        CHAIN     TABEL00F                           99
030200060904IF  2C     *IN99         IFEQ      '1'
030300060904     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
030400060904     C                   EXSR      ERRORE
030500060904     c                   else
030600060904     C                   MOVEL     TBLUNI        DSTB
030700060904E   2C                   ENDIF
030800060904E   1C                   ENDIF
030900970424     C*
031000970424     C* CONTROLLA SE IL TIPO BOLLA E' CALCOLABILE
031100970424IF  1C     $ERR          IFEQ      '0'
031200060904IF  2C     §tbrbl        IFNE      'N'
031300970424     C                   MOVEL     MSG(4)        $MSG                           *TIPO BOLLA NON CALC
031400970424     C                   EXSR      ERRORE
031500970424E   2C                   ENDIF
031600970424E   1C                   ENDIF
031700970424     C*
031800970424     C* LEGGE I CONTRASSEGNI
031900061221     C                   CLEAR                   csbsta
032000061221     C                   CLEAR                   csbVCA
032100061221     C                   CLEAR                   csbcmb
032200061221     C                   CLEAR                   csbCAS
032300061221     C                   CLEAR                   csbtpi
032400061221     C                   CLEAR                   csbtpa
032500061221     C                   CLEAR                   csbfus
032600060809      * mittente
032700970424IF  1C     $ERR          IFEQ      '0'
032800970424     C     §3AFCA        ANDNE     *ZEROS                                       *HA CONTRASSEGNO
032900060809     C                   Z-ADD     D50AAS        csbAAS
033000060809     C                   Z-ADD     D50LNP        csbLNP
033100060809     C                   Z-ADD     D50NRS        csbNRS
033200060809     C                   Z-ADD     D50NSP        csbNSP
033300060809     C     KEYcsb        CHAIN     tncsb03l                           99
033400970424IF  2C     *IN99         IFEQ      '1'                                          *NON TROVATO
033500061221     C                   CLEAR                   csbsta
033600061221     C                   CLEAR                   csbVCA
033700061221     C                   CLEAR                   csbcmb
033800060809     C                   CLEAR                   csbCAS
033900061221     C                   CLEAR                   csbtpi
034000061221     C                   CLEAR                   csbtpa
034100060809     C                   CLEAR                   csbfus
034200970424E   2C                   ENDIF
034300970424E   1C                   ENDIF
034400030122     C*
034500030122     C* LEGGE IL FILE ESTENSIONI PER I BANCALI
034600030122     C                   CLEAR                   DAR5BAN
034700060809If  1c                   If        %Subst(tasGva:1:1) ='B'
034800030122IF  1C     $ERR          IFEQ      '0'
034900030122     C                   Z-ADD     D50AAS        AR5AAS
035000030122     C                   Z-ADD     D50LNP        AR5LNP
035100030122     C                   Z-ADD     D50NRS        AR5NRS
035200030122     C                   Z-ADD     D50NSP        AR5NSP
035300030122     C                   MOVEL     'BAN'         AR5TRD
035400060809     C                   CLEAR                   AR5PRG
035500060809     C     KEYAR5        CHAIN     FIAR531c                           99
035600030122IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
035700030122     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
035800030122     C                   MOVEL     AR5UNI        DAR5BAN
035900030122E   2C                   ENDIF
036000030122E   1C                   ENDIF
036100030729     c                   EndIf
036200030729     C* LEGGE IL FILE ESTENSIONI PER I BANCALI BARTOLINI
036300030729     c                   Clear                   dAr5Bnb
036400060809If  1c                   If        %Subst(tasGva:1:1) ='O'
036500030729IF  1C     $ERR          IFEQ      '0'
036600030729     C                   Z-ADD     D50AAS        AR5AAS
036700030729     C                   Z-ADD     D50LNP        AR5LNP
036800030729     C                   Z-ADD     D50NRS        AR5NRS
036900030729     C                   Z-ADD     D50NSP        AR5NSP
037000030729     c                   Eval      Ar5Trd = 'BNB'
037100060809     C                   CLEAR                   AR5PRG
037200060809     C     KEYAR5        CHAIN     FIAR531c                           99
037300030729IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
037400030729     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
037500030729     c                   Movel     AR5UNI        dAr5Bnb
037600030729E   2C                   ENDIF
037700030729E   1C                   ENDIF
037800030729     c                   EndIf
037900060809     C* LEGGO fiar500F x transiti
038000060809     C                   CLEAR                   DAR5TRS
038100060809    6C                   if        TASFLP > *zeros OR TASFL2 > *zeros
038200060809IF  1C     $ERR          IFEQ      '0'
038300060809     C                   MOVEL     'TRS'         AR5TRD
038400060809     C                   EVAL      AR5PRG = TASFLP
038500060809     C     KEYAR5        CHAIN     FIAR531C                           99
038600060809if  7C     *IN99         IFEQ      '0'
038700060809     C     AR5ATB        andeq     *BLANKS
038800060809     C                   MOVEL     AR5UNI        DAR5TRS
038900060809     C                   END
039000060809     C                   END
039100060809     C                   END
039200040806     C*
039300040806     C* LEGGE IL FILE ESTENSIONI PER LA MERCE DI VALORE
039400040806     C                   CLEAR                   DAR5GEN
039500040806IF  1C     $ERR          IFEQ      '0'
039600040806     C                   Z-ADD     D50AAS        AR5AAS
039700040806     C                   Z-ADD     D50LNP        AR5LNP
039800040806     C                   Z-ADD     D50NRS        AR5NRS
039900040806     C                   Z-ADD     D50NSP        AR5NSP
040000040806     C                   MOVEL     'GEN'         AR5TRD
040100060809     C     KEYAR5        CHAIN     fiar531c                           99
040200040806IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
040300040806     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
040400040806     C                   MOVEL     AR5UNI        DAR5GEN
040500040806E   2C                   ENDIF
040600040806E   1C                   ENDIF
040700970424     C*
040800970424     C                   ENDSR
040900970424     C*------------------------------------------------------------------------*
041000970424     C* TASSAZ - TASSAZIONE BOLLA
041100970424     C*------------------------------------------------------------------------*
041200970424     C     TASSAZ        BEGSR
041300060810     c*
041400061221     c                   clear                   tassta
041500061221     c                   clear                   tascas
041600061221     c                   clear                   tastic
041700061221     c                   clear                   tasvca
041800061221     c                   clear                   tascmb
041900061221     c                   clear                   tassva
042000060809     c                   movel     *Blanks       tastla
042100060809     c                   movel     *Blanks       taserr
042200060809     c                   movel     *Blanks       tasmsg
042300060809     c                   clear                   tasial
042400060809     c                   clear                   task88
042500060809     c                   clear                   tasfcaa
042600060809     c                   clear                   taspkcn
042700060809     c                   clear                   tasric
042800060809     c                   clear                   tasspc
042900060809     c                   clear                   tasBan
043000080908     c                   clear                   tasric
043100060810     c                   movel     Tasftc        tastc1
043200080908      * verifico se devo calcolare l'addebito di lasciato avviso
043300080908      * verifico se c'è evento
043400080908     c     Kevb4         chain     Fnevb04l
043500080908
043600080908     c                   If        %found(Fnevb04l)
043700080908     c                   eval      tasric = 'S'
043800080908     c                   endif
043900060825     c                   clear                   tasimv
044000060825     c                   clear                   taspor
044100060825     c*
044200060825     c                   if        $err='0'
044300060907     C                   CLEAR                   DTASV
044400130301     C                   CLEAR                   WF1_SV
044500130301     C                   CLEAR                   WF1_VA
044600970424     C*
044700970424IF  1C     D50KSC        IFNE      *ZEROS
044800060811     C                   MOVEL     D50KSC        dKSC
044900060811     C                   MOVEL     D50CTR        dCTR
045000060811     c                   else
045100060811     C                   MOVEL     tasKSC        dKSC
045200060811     C                   MOVEL     tasCTR        dCTR
045300970424E   1C                   ENDIF
045400970424     C     D50DTR        IFGT      *ZEROS
045500970424     C                   Z-ADD     D50DTR        TASDSP
045600060810     c                   else
045700060810     C                   MOVEL     TASDSPt       TASDsp                         AAAA/MM/GG
045800970424     C                   ENDIF
045900130228     C                   MOVEL     TASDSP        TASDCT                         AAAA/MM/GG
046000130228     C**                 MOVEL     TASDSPt       TASDCT                         AAAA/MM/GG
046100970424    1C     §3AFCA        IFEQ      1
046200061221     c                   movel     csbsta        Tassta
046300060809     c                   z-add     csbcas        Tascas
046400061221     c                   z-add     csbcmb        tascmb
046500061221     c                   movel     csbvca        tasvca
046600060809     c                   if        csbfus <> *blanks
046700060809     c                   move      csbfus        tastic
046800060809     c                   else
046900060809     c                   move      csbtpi        tastic
047000060809     c                   end
047100970424E   1C                   ENDIF
047200060809      * Flag SI NO DDT
047300060809    1C                   If        tasll1 = 'C' or tasll1 = 'S'
047400060809     C                   movel     'S'           Tasddt
047500060809   x1C                   Else
047600060809     C                   Clear                   Tasddt
047700060809    1C                   Endif
047800061109     c                   clear                   dtas01
047900061109     c                   movel     tasfbr        §asfbr
048000061109     c                   movel     tascca        §ascca
048100110804     c* servizio email
048200110804     c                   eval      dtasflo = tasflo
048300110804     c                   eval      §asemd=§floemd
048400141211
048500141211      /free
048600141211       //?Imposto se part.consegna con PinCode
048700141211         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
048800141211           §ASpin = 'S';
048900141211         ENDIF;
049000141211      /end-free
049100141211
049200130228      * data decorrenza tariffa/offerta
049300130228     c                   IF        dataddt > 0
049400130228     c                   eval      §asdrt = %editc(dataddt:'X')
049500130228     c                   ENDIF
049600061109     c                   eval      dstasflo = dtas01
049700030204      * Bancali
049800060809If  1c                   If        %Subst(tasGva:1:1) ='B'
049900030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
050000030205    1c                   EndIf
050100030729      * Colli originali
050200060809If  1c                   If        %Subst(tasGva:1:1) ='O'
050300060811     c                   Eval      dNcl = §Ar5bCor
050400061030     c                   else
050500061030     c                   Eval      dNcl = tasncl
050600030729    1c                   EndIf
050700130226      * Assicurazione, se presente è da togliere
050800130226     c                   IF        TASfcm = 'F'
050900130226     c                   clear                   TASfcm
051000130226     c                   clear                   TASias
051100130226     c                   clear                   TASvas
051200130226     c                   ENDIF
051300970424     C*--------------------------
051400970424     C*  D S T A S V
051500970424     C*--------------------------
051600970424     C                   CLEAR                   KPJBU
051700970424     C*--------------------------
051800011102     C*  CALCOLO TASSAZIONE
051900970424     C*--------------------------
052000130228     C                   CALL      'TNSF20R'                            99
052100970424     C                   PARM                    KPJBA
052200991126     C                   PARM                    DTAS
052300991126     C                   PARM                    DTASV
052400011102     C*
052500130228     C* In uscita dal TNSF20R verifico la divisa della DS di passaggio rispetto a quella
052600011102     C* di calcolo del CAT reperita dalla tabella GED tipo record CE.
052700011102     C                   EXSR      CNVDTAS
052800060809     c                   end
052900011102     C*
053000970424     C                   Z-ADD     TASPVL        D50PVL
053100060904IF  1C     tasERR        IFNE      ' '                                          *ERRORE
053200060904     C                   MOVEL     tasMSG        DSMSG
053300130228     C                   MOVEL     'SF20'        DSERR
053400970424     C                   MOVEL     DSMSG         $MSG
053500970424     C                   EXSR      ERRORE
053600970424X   1C                   ELSE
053700991126     C                   Z-ADD     TASIMV        D50RIC
053800970424E   1C                   ENDIF
053900130301      * scrivo file delle varie
054000130507     c                   IF        not wNoWf
054100130301     c                   exsr      wrtVarie
054200130507     c                   ENDIF
054300970424     C*
054400970424     C                   ENDSR
054500970424     C**************************************************************************
054600970424     C*    CALCOLO COMPETENZE
054700970424     C**************************************************************************
054800970424     C     CALCOM        BEGSR
054900970424     C*
055000970424     C                   CLEAR                   DS$ECO
055100970424     C                   CLEAR                   TRECDS
055200060810     C                   EXSR      AZZW
055300970424     C                   MOVEL     D50TLA        D$GSTS                         *STATUS
055400970424     C                   MOVEL     'TNTE50R'     D$GPGF                         *PGM CHE HA MODIF.FI
055500060810     C                   EXSR      cddTAQ
055600060904     C                   EXSR      AZZnCDDTAQ
055700970424     C* CONTRASSEGNO
055800060809     c                   movel     csbtpa        D$DTPI
055900060810     c                   movel     csbsta        D$Dsta
056000060809     c                   if        csbfus <> *blanks
056100060809     c                   move      csbfus        D$DTPI
056200060809     c                   else
056300060809     c                   move      csbtpi        D$DTPI
056400060809     c                   end
056500020125     C                   Z-ADD     TASCAS        D$DCAS                         *LIRE CONTRASS
056600970424     C* ASSICURAZIONE
056700020125     C                   Z-ADD     TASIAS        D$DVAE
056800060809     C* TRANSITI
056900060809     C                   Z-ADD     §AR5flp       D$DAR5flp
057000060809     C                   Z-ADD     §AR5fl2       D$DAR5fl2
057100060809     C                   Z-ADD     §AR5fl3       D$DAR5fl3
057200060809     C                   Z-ADD     §AR5DE3       D$DAR5DE3
057300060809     C                   Z-ADD     §AR5DU3       D$DAR5DU3
057400060809     C                   Z-ADD     §AR5fl4       D$DAR5fl4
057500060809     C                   Z-ADD     §AR5DE4       D$DAR5DE4
057600060809     C                   Z-ADD     §AR5DU4       D$DAR5DU4
057700060809     C                   Z-ADD     §AR5fl5       D$DAR5fl5
057800060809     C                   Z-ADD     §AR5DE5       D$DAR5DE5
057900060809     C                   Z-ADD     §AR5DU5       D$DAR5DU5
058000060809     C                   Z-ADD     §AR5DET       D$DAR5DET
058100060809     C                   Z-ADD     §AR5DUT       D$DAR5DUT
058200060809     C                   Z-ADD     §AR5DE2       D$DAR5DE2
058300060809     C                   Z-ADD     §AR5DU2       D$DAR5DU2
058400030204     C*
058500030122     C                   Z-ADD     §AR5TBA       D$DAR5TBA                      *TIPO BANCALE 1
058600030122     C                   Z-ADD     §AR5NBA       D$DAR5NBA                      *NUM. BANCALE 1
058700030122     C                   Z-ADD     §AR5TB2       D$DAR5TB2                      *TIPO BANCALE 2
058800030122     C                   Z-ADD     §AR5NB2       D$DAR5NB2                      *NUM. BANCALE 2
058900030730     C*
059000030730     c                   IF        §AR5BNBA > *zeros OR
059100030730     c                             §AR5BNB2 > *zeros
059200030730     C                   Z-ADD     §AR5BTBA      D$DAR5TBA                      *TIPO BANCALE 1 BAR
059300030730     C                   Z-ADD     §AR5BNBA      D$DAR5NBA                      *NUM. BANCALE 1 BAR
059400030730     C                   Z-ADD     §AR5BTB2      D$DAR5TB2                      *TIPO BANCALE 2 BAR
059500030730     C                   Z-ADD     §AR5BNB2      D$DAR5NB2                      *NUM. BANCALE 2 BAR
059600030730     c                   ENDIF
059700040315     C                   Z-ADD     §AR5BCSF      D$DAR5CSF                      *NUM. BANCALE 2 BAR
059800040315     C                   Z-ADD     §AR5BCOR      D$DAR5COR                      *NUM. BANCALE 2 BAR
059900030730     C*
060000970424     C* SCHIERA RICAVI
060100030515     C     1             DO        75            I                              *AZZERA SCHIERE
060200970424     C                   Z-ADD     *ZEROS        DSWCN                          SCH NUM --> DS NUM
060300970424     C                   MOVE      DSWCA         CAL(I)                         DS ALF --> SCH ALF
060400970424     C                   Z-ADD     *ZEROS        DSWIN
060500970424     C                   MOVE      DSWIA         IAL(I)
060600970424     C                   ENDDO
060700970424     C                   Z-ADD     556           DSWCN                          SCH NUM --> DS NUM
060800970424     C                   MOVE      DSWCA         CAL(1)                         DS ALF --> SCH ALF
060900970424     C                   Z-ADD     D50RIC        DSWIN
061000970424     C                   MOVE      DSWIA         IAL(1)
061100970424     C                   MOVEA     CAL           D$DCCD                         *DS COD. RICAVI
061200970424     C                   MOVEA     IAL           D$DICD                         *DS IMP.RICAVI
061300060809     C                   MOVEL     tasmct        D$DMCT
061400060811     C                   MOVEA     SV            D$DSVN
061500060825     c                   do        20            k
061600060825     C                   MOVE      Va(k)         vn(k)
061700060825     c                   end
061800060825     C                   MOVEA     Vn            D$DVAN
061900060904     C* non considero la GIACENZA
062000970424     C                   MOVEL     'N'           D$DFGC                         *FORZATURA GIAC=NO
062100051212     C                   Z-ADD     0             D$DGGS
062200970424     C* NUMERO STOP DI RITIRO
062300020109     C                   Z-ADD     1             D$DNSR                         *N.STOP RITIRO
062400060810IF  1C     tasstr        IFGT      1
062500060810     C     1             DIV       tasstr        D$DNSR                         *1 / N. DI STOP
062600020109IF  2C     D$DNSR        IFLT      0,00001
062700020109     C                   Z-ADD     0,00001       D$DNSR
062800971024E   2C                   ENDIF
062900970424E   1C                   ENDIF
063000971027     C* NUMERO STOP DI CONSEGNA
063100971027     C                   Z-ADD     1             D$DNSC                         *N.STOP CONSEGNA
063200060810IF  1C     tasstp        IFGT      1
063300060810     C     1             DIV       tasstp        D$DNSC                         *1 / N. DI STOP
063400020109IF  2C     D$DNSC        IFLT      0,00001
063500020109     C                   Z-ADD     0,00001       D$DNSC
063600971027E   2C                   ENDIF
063700971027E   1C                   ENDIF
063800971024     C*
063900971024     C* IMPOSTA ALTRI CAMPI "DOPO"
064000980826     C                   MOVEL     §3ATB1        D$DTPO                         *TIPO PORTO F/A
064100060810     C     D$DFBR        IFNE      'S'
064200060810     C                   MOVEL     'S'           D$DFIG
064300060810     C                   ENDIF
064400970424     C*
064500970424     C                   MOVEL     D$DDS1        D$DRE1                         *DS DOPO
064600970424     C                   MOVEL     D$DDS2        D$DRE2
064700970424     C                   MOVEL     D$ODS1        D$ORE1                         *DS ORIG
064800970424     C                   MOVEL     D$ODS2        D$ORE2
064900970424     C*
065000970424     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
065100970424     C                   CALL      'TRECD8R'                            98
065200970424     C                   PARM                    DS$ECO                         *DS IMMAGINI BOLLA
065300970424     C*
065400970424     C* SE RITORNA ERRORE ESCO PGM
065500970424     C     D$GERR        IFNE      *BLANKS
065600970424     C                   MOVEL     D$GMSG        $MSG
065700970424     C                   EXSR      ERRORE
065800970424     C                   ENDIF
065900970424     C*
066000970424     C     $ERR          IFEQ      '0'                                          --- 1 -->
066100970424     C*
066200970424     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
066300970424     C                   MOVEA     D$DCCD        CAL                            SCHIERE ALFANUMERICH
066400970424     C                   MOVEA     D$DICD        IAL
066500030515     C     1             DO        75            I                 6 0
066600970424     C                   MOVE      CAL(I)        DSWCA                          SCH ALF --> DS ALF
066700970424     C                   Z-ADD     DSWCN         CD                6 0          DS NUM --> SCH NUM
066800970424     C     CD            IFNE      *ZEROS
066900970424     C                   MOVE      IAL(I)        DSWIA
067000090922     C                   Z-ADD     DSWIN         IMP              13 5
067100970424     C     CD            IFEQ      900                                          *RICAVO PRES. DELTA
067200090922     C                   Z-ADD(h)  IMP           D50COM
067300970424     C                   ENDIF
067400970424     C                   ENDIF
067500970424     C                   ENDDO
067600970424     C                   ENDIF                                                  <-- 1 ---
067700970424     C*
067800970424     C                   ENDSR
067900130301
068000130301      ********************************************************************
068100130301      *   wrtVarie - Scrivo file delle varie
068200130301      ********************************************************************
068300130301     c     wrtvarie      BEGSR
068400130301
068500130301     c                   clear                   wfdbf100
068600130301
068700130301     c                   eval      WF1aas = TASaas
068800130301     c                   eval      WF1mgs = TASmgs
068900130301     c                   eval      WF1lnp = TASlnp
069000130301     c                   eval      WF1nrs = TASnrs
069100130301     c                   eval      WF1nsp = TASnsp
069200130301     c                   eval      WF1ksc = TASksc
069300130301     c                   eval      WF1div = TASdiv
069400130301     c                   eval      WF1por = TASpor
069500130301
069600130301     c                   movea     sv            wf1_sv
069700130301     c                   movea     va            wf1_va
069800130301
069900130301     c                   eval      WF1imv = TASimv
070000130301
070100130301     c                   write     wfdbf100
070200130301
070300130301     c                   ENDSR
070400130301
070500060810     C********************************************************************
070600060810     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
070700060810     C********************************************************************
070800060810     C     AZZW          BEGSR
070900060810     C*
071000060810     C* AZZERO CAMPI DS ESTERNA
071100060810     c* è possibile usare all'interno del pgm solo i campi prima e dopo
071200060810     c* del TITAS senza testare i relativi flag prima o dopo.
071300060810     c* Se si vogliono usare i campi degli altri files senza testare i
071400060810     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
071500060810     c* contenute nel /COPY
071600060810     C                   EXSR      APDTAQ
071700060810     C                   EXSR      ADDTAQ
071800060810     C                   EXSR      AODTAQ
071900060810     C                   MOVEL     D$ODS1        D$ORE1
072000060810     C                   MOVEL     D$ODS2        D$ORE2
072100060810     C                   MOVEL     *BLANKS       D$PCO1
072200060810     C                   MOVEL     *BLANKS       D$PCO2
072300060810     C                   MOVEL     *BLANKS       D$DCO1
072400060810     C                   MOVEL     *BLANKS       D$DCO2
072500060810     C                   MOVEL     *BLANKS       D$PSVN
072600060810     C                   MOVEL     *BLANKS       D$PVAN
072700060810     C                   MOVEL     *BLANKS       D$DSVN
072800060810     C                   MOVEL     *BLANKS       D$DVAN
072900060810     C                   MOVEL     *BLANKS       D$OSVN
073000060810     C                   MOVEL     *BLANKS       D$OVAN
073100060810     C                   Z-ADD     *ZEROS        D$PCAS
073200060810     C                   MOVEL     *BLANKS       D$PTPI
073300060810     C                   MOVEL     *BLANKS       D$Psta
073400060810     C                   Z-ADD     *ZEROS        D$DCAS
073500060810     C                   MOVEL     *BLANKS       D$DTPI
073600060810     C                   MOVEL     *BLANKS       D$Dsta
073700060810     C                   Z-ADD     *ZEROS        D$PVAE
073800060810     C                   Z-ADD     *ZEROS        D$DVAE
073900060810     C                   Z-ADD     *ZEROS        D$PNSR
074000060810     C                   Z-ADD     *ZEROS        D$DNSR
074100060810     C                   Z-ADD     *ZEROS        D$PAGC
074200060810     C                   Z-ADD     *ZEROS        D$PMGC
074300060810     C                   Z-ADD     *ZEROS        D$DAGC
074400060810     C                   Z-ADD     *ZEROS        D$DMGC
074500060810     C                   MOVEL     *BLANKS       D$PTPO
074600060810     C                   MOVEL     *BLANKS       D$PRBL
074700060810     C                   MOVEL     'N'           D$PCSF
074800060810     C                   MOVEL     'N'           D$PCCS
074900060810     C                   MOVEL     'N'           D$PDBO
075000060810     C                   MOVEL     'N'           D$PFRI
075100060810     C                   MOVEL     'N'           D$PFSP
075200060810     C                   MOVEL     *BLANKS       D$DTPO
075300060810     C                   MOVEL     *BLANKS       D$DRBL
075400060810     C                   MOVEL     'N'           D$DCSF
075500060810     C                   MOVEL     'N'           D$DCCS
075600060810     C                   MOVEL     'N'           D$DDBO
075700060810     C                   MOVEL     'N'           D$DFRI
075800060810     C                   MOVEL     'N'           D$DFSP
075900060810     C                   MOVEL     *BLANKS       D$PAR5T
076000060810     C                   MOVEL     *BLANKS       D$DAR5T
076100060810     C                   MOVEL     *BLANKS       D$PAR5B
076200060810     C                   MOVEL     *BLANKS       D$DAR5B
076300060810     C                   Z-ADD     *ZEROS        D$PAR5flp
076400060810     C                   Z-ADD     *ZEROS        D$PAR5DET
076500060810     C                   Z-ADD     *ZEROS        D$PAR5DUT
076600060810     C                   Z-ADD     *ZEROS        D$PAR5fl2
076700060810     C                   Z-ADD     *ZEROS        D$PAR5DE2
076800060810     C                   Z-ADD     *ZEROS        D$PAR5DU2
076900060810     C                   Z-ADD     *ZEROS        D$PAR5fl3
077000060810     C                   Z-ADD     *ZEROS        D$PAR5DE3
077100060810     C                   Z-ADD     *ZEROS        D$PAR5DU3
077200060810     C                   Z-ADD     *ZEROS        D$PAR5fl4
077300060810     C                   Z-ADD     *ZEROS        D$PAR5DE4
077400060810     C                   Z-ADD     *ZEROS        D$PAR5DU4
077500060810     C                   Z-ADD     *ZEROS        D$PAR5fl5
077600060810     C                   Z-ADD     *ZEROS        D$PAR5DE5
077700060810     C                   Z-ADD     *ZEROS        D$PAR5DU5
077800060810     C                   Z-ADD     *ZEROS        D$DAR5flp
077900060810     C                   Z-ADD     *ZEROS        D$DAR5DET
078000060810     C                   Z-ADD     *ZEROS        D$DAR5DUT
078100060810     C                   Z-ADD     *ZEROS        D$DAR5fl2
078200060810     C                   Z-ADD     *ZEROS        D$DAR5DE2
078300060810     C                   Z-ADD     *ZEROS        D$DAR5DU2
078400060810     C                   Z-ADD     *ZEROS        D$DAR5fl3
078500060810     C                   Z-ADD     *ZEROS        D$DAR5DE3
078600060810     C                   Z-ADD     *ZEROS        D$DAR5DU3
078700060810     C                   Z-ADD     *ZEROS        D$DAR5fl4
078800060810     C                   Z-ADD     *ZEROS        D$DAR5DE4
078900060810     C                   Z-ADD     *ZEROS        D$DAR5DU4
079000060810     C                   Z-ADD     *ZEROS        D$DAR5fl5
079100060810     C                   Z-ADD     *ZEROS        D$DAR5DE5
079200060810     C                   Z-ADD     *ZEROS        D$DAR5DU5
079300060810     C                   Z-ADD     *ZEROS        D$PAR5TBA
079400060810     C                   Z-ADD     *ZEROS        D$PAR5NBA
079500060810     C                   Z-ADD     *ZEROS        D$PAR5TB2
079600060810     C                   Z-ADD     *ZEROS        D$PAR5NB2
079700060810     C                   Z-ADD     *ZEROS        D$DAR5TBA
079800060810     C                   Z-ADD     *ZEROS        D$DAR5NBA
079900060810     C                   Z-ADD     *ZEROS        D$DAR5TB2
080000060810     C                   Z-ADD     *ZEROS        D$DAR5NB2
080100060810     C                   Z-ADD     *ZEROS        D$PAR5CSF
080200060810     C                   Z-ADD     *ZEROS        D$PAR5COR
080300060810     C                   Z-ADD     *ZEROS        D$DAR5CSF
080400060810     C                   Z-ADD     *ZEROS        D$DAR5COR
080500060810     C                   MOVEL     *BLANKS       D$XXXX
080600060810     C*
080700060810     C                   ENDSR
080800060904     C*-------------------------------------------------------------------
080900060904     C*?Azzera varia "N" immagine -dopo- TITAS
081000060904     C*-------------------------------------------------------------------
081100060904     C     AZZnCDDTAQ    BEGSR
081200060904     c*
081300060904if  1C                   IF        (D$DVA1 = 88888888,000)
081400060904     C                   Z-ADD     *zeros        D$DVA1
081500060904e   1c                   ENDIF
081600060904if  1C                   IF        (D$DVA2 = 88888888,000)
081700060904     C                   Z-ADD     *zeros        D$DVA2
081800060904e   1c                   ENDIF
081900060904if  1C                   IF        (D$DVA3 = 88888888,000)
082000060904     C                   Z-ADD     *zeros        D$DVA3
082100060904e   1c                   ENDIF
082200060904     c*
082300060904     C                   ENDSR
082400970424     C********************************************************************
082500970424     C*   CALDEL - CALCOLA IL DELTA
082600970424     C*          INPUT  --> WRRW: RICAVO REALE
082700970424     C*                     WRPW: RICAVO PRESUNTO
082800970424     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
082900970424     C********************************************************************
083000970424     C     CALDEL        BEGSR
083100970424     C*
083200970424     C*--------------------
083300970424     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
083400970424     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
083500970424     C*--------------------
083600970424     C*
083700970424     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
083800970424     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
083900970424     C     GAP           MULT      100           GAP                            (GAP x 100)
084000970424     C*
084100970424     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
084200970424IF  1C     WRRW          IFEQ      *ZEROS
084300970424     C                   Z-ADD     1             WRRW
084400970424E   1C                   ENDIF
084500970424     C*
084600970424     C     GAP           DIV       WRRW          GAP
084700970424     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
084800970424     C*
084900970424     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
085000970424     C     WDPW          COMP      0                                    28
085100970424     C* POSITIVO
085200970424IF  1C     *IN28         IFEQ      *OFF
085300970424IF  2C     GAP           IFGE      1000
085400970424     C                   Z-ADD     +999,9        WDPW
085500970424E   2C                   ENDIF
085600970424     C* NEGATIVO
085700970424X   1C                   ELSE
085800970424IF  2C     GAP           IFLE      -1000
085900970424     C                   Z-ADD     -999,9        WDPW
086000970424E   2C                   ENDIF
086100970424E   1C                   ENDIF
086200970424     C*
086300970424     C                   ENDSR
086400970424     C********************************************************************
086500970424     C*   ERRORE - ROUTINE GESTIONE ERRORI
086600970424     C********************************************************************
086700970424     C     ERRORE        BEGSR
086800970424     C*
086900970424     C                   MOVEL     'S'           $ERR
087000970424     C                   MOVEL     $MSG          D50MSG
087100970424     C                   MOVE      $MSG          D50ERR
087200970424     C*
087300970424     C                   ENDSR
087400011012     C*------------------------------------------------------------------------*
087500011012     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
087600011012     C*------------------------------------------------------------------------*
087700011012     C     CARGEDCE      BEGSR
087800011012     C*
087900011012     C                   CLEAR                   DSGEDCE
088000011026     C                   MOVEL     *blanks       WDIVCE            3
088100011012     C*
088200011012     C                   MOVEL(P)  'GED'         KTECOD
088300011012     C                   MOVEL(P)  'CE'          KTEKE1
088400011012     C     KEYTBE        CHAIN     TNTBE01L
088500011012     C                   IF        %found(TNTBE01L)
088600011012     C                   MOVEL     TBEUNI        DSGEDCE
088700011012     C                   ENDIF
088800011012     C*
088900011102     C                   IF        §GEDCEDAT > DATCOR
089000011026     C                   EVAL      WDIVCE = §GEDCEDIP
089100011026     C                   ELSE
089200011026     C                   EVAL      WDIVCE = §GEDCEDIV
089300011012     C                   ENDIF
089400011012     C*
089500011012     C                   ENDSR
089600011102     C*------------------------------------------------------------------------*
089700011102     C* CNVDTAS - CONVERTE I CAMPI DELLE DS DTAS/DTASV NELLA DIVISA DI CALCOLO DEL CE
089800011102     C*------------------------------------------------------------------------*
089900011102     C     CNVDTAS       BEGSR
090000011102     C*
090100011102     C* CONTRASSEGNO
090200011102     C                   Z-ADD     TASCAS        WIMI             30 9
090300011107     C                   MOVEL     TASVCA        WDVI              3
090400011102     C                   EXSR      CNVEUR
090500011102     C                   Z-ADD     WIMO          TASCAS
090600011102     C*
090700011102     C* IMPORTO DA ASSICURARE
090800011102     C                   Z-ADD     TASIAS        WIMI             30 9
090900011107     C                   MOVEL     TASVAS        WDVI              3
091000011102     C                   EXSR      CNVEUR
091100011102     C                   Z-ADD     WIMO          TASIAS
091200011102     C*
091300011102     C* QUANTITA' DA FATTURARE (non viene convertita x ambiguità su suo utilizzo)
091400011102     C*
091500011102     C* TRASPORTO
091600011102     C                   Z-ADD     TASPOR        WIMI             30 9
091700011102     C                   MOVEL     TASDIV        WDVI              3
091800011102     C                   EXSR      CNVEUR
091900011102     C                   Z-ADD     WIMO          TASPOR
092000011102     C*
092100011102     C* TOTALE IMPONIBILE
092200011102     C                   Z-ADD     TASIMV        WIMI             30 9
092300011102     C                   MOVEL     TASDIV        WDVI              3
092400011102     C                   EXSR      CNVEUR
092500011102     C                   Z-ADD     WIMO          TASIMV
092600011102     C*
092700011102     C* IMPORTO ASS. IN MONETA O MAX VALORE ...
092800011102     C                   Z-ADD     TASIAL        WIMI             30 9
092900011123     C                   MOVEL     TASDIV        WDVI              3
093000011102     C                   EXSR      CNVEUR
093100011102     C                   Z-ADD     WIMO          TASIAL
093200011102     C*
093300011102     C* VARIE VARIE
093400011102     C                   Z-ADD     TASVA1        WIMI             30 9
093500011102     C                   MOVEL     TASDIV        WDVI              3
093600011102     C                   EXSR      CNVEUR
093700011102     C                   Z-ADD     WIMO          TASVA1
093800011102     C                   Z-ADD     TASVA2        WIMI             30 9
093900011102     C                   MOVEL     TASDIV        WDVI              3
094000011102     C                   EXSR      CNVEUR
094100011102     C                   Z-ADD     WIMO          TASVA2
094200011102     C                   Z-ADD     TASVA3        WIMI             30 9
094300011102     C                   MOVEL     TASDIV        WDVI              3
094400011102     C                   EXSR      CNVEUR
094500011102     C                   Z-ADD     WIMO          TASVA3
094600011102     C                   Z-ADD     TASVA4        WIMI             30 9
094700011102     C                   MOVEL     TASDIV        WDVI              3
094800011102     C                   EXSR      CNVEUR
094900011102     C                   Z-ADD     WIMO          TASVA4
095000011102     C                   Z-ADD     TASVA5        WIMI             30 9
095100011102     C                   MOVEL     TASDIV        WDVI              3
095200011102     C                   EXSR      CNVEUR
095300011102     C                   Z-ADD     WIMO          TASVA5
095400011102     C                   Z-ADD     TASVA6        WIMI             30 9
095500011102     C                   MOVEL     TASDIV        WDVI              3
095600011102     C                   EXSR      CNVEUR
095700011102     C                   Z-ADD     WIMO          TASVA6
095800011102     C                   Z-ADD     TASVA7        WIMI             30 9
095900011102     C                   MOVEL     TASDIV        WDVI              3
096000011102     C                   EXSR      CNVEUR
096100011102     C                   Z-ADD     WIMO          TASVA7
096200011102     C                   Z-ADD     TASVA8        WIMI             30 9
096300011102     C                   MOVEL     TASDIV        WDVI              3
096400011102     C                   EXSR      CNVEUR
096500011102     C                   Z-ADD     WIMO          TASVA8
096600011102     C                   Z-ADD     TASVA9        WIMI             30 9
096700011102     C                   MOVEL     TASDIV        WDVI              3
096800011102     C                   EXSR      CNVEUR
096900011102     C                   Z-ADD     WIMO          TASVA9
097000011102     C                   Z-ADD     TASVA0        WIMI             30 9
097100011102     C                   MOVEL     TASDIV        WDVI              3
097200011102     C                   EXSR      CNVEUR
097300011102     C                   Z-ADD     WIMO          TASVA0
097400011102     C                   Z-ADD     TASV11        WIMI             30 9
097500011102     C                   MOVEL     TASDIV        WDVI              3
097600011102     C                   EXSR      CNVEUR
097700011102     C                   Z-ADD     WIMO          TASV11
097800011102     C                   Z-ADD     TASV12        WIMI             30 9
097900011102     C                   MOVEL     TASDIV        WDVI              3
098000011102     C                   EXSR      CNVEUR
098100011102     C                   Z-ADD     WIMO          TASV12
098200011102     C                   Z-ADD     TASV13        WIMI             30 9
098300011102     C                   MOVEL     TASDIV        WDVI              3
098400011102     C                   EXSR      CNVEUR
098500011102     C                   Z-ADD     WIMO          TASV13
098600011102     C                   Z-ADD     TASV14        WIMI             30 9
098700011102     C                   MOVEL     TASDIV        WDVI              3
098800011102     C                   EXSR      CNVEUR
098900011102     C                   Z-ADD     WIMO          TASV14
099000011102     C                   Z-ADD     TASV15        WIMI             30 9
099100011102     C                   MOVEL     TASDIV        WDVI              3
099200011102     C                   EXSR      CNVEUR
099300011102     C                   Z-ADD     WIMO          TASV15
099400011102     C                   Z-ADD     TASV16        WIMI             30 9
099500011102     C                   MOVEL     TASDIV        WDVI              3
099600011102     C                   EXSR      CNVEUR
099700011102     C                   Z-ADD     WIMO          TASV16
099800011102     C                   Z-ADD     TASV17        WIMI             30 9
099900011102     C                   MOVEL     TASDIV        WDVI              3
100000011102     C                   EXSR      CNVEUR
100100011102     C                   Z-ADD     WIMO          TASV17
100200011102     C                   Z-ADD     TASV18        WIMI             30 9
100300011102     C                   MOVEL     TASDIV        WDVI              3
100400011102     C                   EXSR      CNVEUR
100500011102     C                   Z-ADD     WIMO          TASV18
100600011102     C                   Z-ADD     TASV19        WIMI             30 9
100700011102     C                   MOVEL     TASDIV        WDVI              3
100800011102     C                   EXSR      CNVEUR
100900011102     C                   Z-ADD     WIMO          TASV19
101000011102     C                   Z-ADD     TASV20        WIMI             30 9
101100011102     C                   MOVEL     TASDIV        WDVI              3
101200011102     C                   EXSR      CNVEUR
101300011102     C                   Z-ADD     WIMO          TASV20
101400011102     C*
101500011102     C                   ENDSR
101600011012     C*------------------------------------------------------------------------*
101700011012     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
101800011012     C*------------------------------------------------------------------------*
101900011012     C     CNVEUR        BEGSR
102000011112     C*
102100011112     C* AZZERA VARIABILI DI LAVORO
102200011112     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
102300011112     C                   MOVEL     '9'           NDEC              1            *N. DEC IN OUTPUT
102400011112     C*
102500011112     C* DI DEFUALT ATTRIBUISCO LO STESSO VALORE AGLI IMPORTI "DA CONVERTIRE" E "CONVERTITI"
102600011112     C                   Z-ADD     WIMI          WIMO
102700011107     C*
102800011107     C* COME PRIMA COSA AI FINI DELLA CONVERSIONE TESTO SE LE DIVISE SONO DIVERSE
102900011107     C     WDVI          IFNE      WDIVCE
103000011012     C*
103100011012IF  1C     WIMI          IFNE      *ZEROS
103200011012     C                   CLEAR                   YEUR
103300011012     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
103400011012     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
103500011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
103600011012     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
103700011012     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
103800011012     C                   CALL      'YEURCO'
103900011012     C                   PARM                    YEUR
104000011012IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
104100011012     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
104200011012X   2C                   ELSE                                                   *SI ERRORI
104300011012     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
104400011012E   2C                   ENDIF
104500011012E   1C                   ENDIF
104600011012     C*
104700011107     C                   ENDIF
104800011107     C*
104900011012     C                   ENDSR
105000970424     C********************************************************************
105100970424     C*   RUTINZ - ROUTINE INIZIALE
105200970424     C********************************************************************
105300970424     C     RUTINZ        BEGSR
105400970424     C*
105500970424     C* PARAMETRI IN INPUT ALLA ROUTINE
105600970424     C     *ENTRY        PLIST
105700970424     C                   PARM                    DS50
105800130228     C                   PARM                    parddt
105900130507     C                   PARM                    partest
106000130228
106100130228      /free
106200130507         wNoWf = *off;
106300130228       //?Se non passato il paramtro della data decorrenza tariffa/offerta
106400130228       //?pulisco il campo
106500130228         IF  %parms = 1;
106600130228           clear dataddt;
106700130228         ELSE;
106800130228           dataddt = %int(parddt);
106900130228         ENDIF;
107000130507       //?Se passato il terzo parametro non devo aprire il file di work
107100130507         IF  %parms > 2;
107200130507           wNoWf = *on;
107300130507         ENDIF;
107400130507       //?apro il file di work
107500130509         IF  not wNoWf and not %open(WFDBF10F);
107600130507           open WFDBF10F;
107700130507         ENDIF;
107800130228      /end-free
107900970424     C*
108000060809     C* CHIAVE LETTURA titas30c - COMPLETA
108100060809     C     KEYtas        KLIST
108200060809     C                   KFLD                    tasAAS
108300060809     C                   KFLD                    tasLNP
108400060809     C                   KFLD                    tasNRS
108500060809     C                   KFLD                    tasNSP
108600060809      *  KLIST PER ACCESSO FILES
108700060809     c     KTA7          KLIST
108800060809     c                   KFLD                    TASAAS
108900060809     c                   KFLD                    TASLNP
109000060809     c                   KFLD                    TASNRS
109100060809     c                   KFLD                    TASNSP
109200060809     c                   KFLD                    TASTBL
109300030122     C*
109400060809     C* CHIAVE LETTURA FIAR531c - COMPLETA
109500030122     C     KEYAR5        KLIST
109600030122     C                   KFLD                    AR5AAS
109700030122     C                   KFLD                    AR5LNP
109800030122     C                   KFLD                    AR5NRS
109900030122     C                   KFLD                    AR5NSP
110000030122     C                   KFLD                    AR5TRD
110100060809     C                   KFLD                    AR5prg
110200970424     C*
110300060809     C* CHIAVE LETTURA tncsb03l - COMPLETA
110400060809     C     KEYcsb        KLIST
110500060809     C                   KFLD                    csbAAS
110600060809     C                   KFLD                    csbLNP
110700060809     C                   KFLD                    csbNRS
110800060809     C                   KFLD                    csbNSP
110900970424     C*
111000970424     C* CHIAVE LETTURA TABEL00F - COMPLETA
111100970424     C     KEYTAB        KLIST
111200970424     C                   KFLD                    KTBUTE
111300970424     C                   KFLD                    KTBCOD
111400970424     C                   KFLD                    KTBKEY
111500011012     C*
111600011012     C* LETTURA TNTBE01L - PARZIALE
111700011012     C     KEYTBE        KLIST
111800011012     C                   KFLD                    KTECOD                         *UTENTE
111900011012     C                   KFLD                    KTEKE1                         *CODICE
112000080908      * Chiave file eventi FNEVB
112100080908     c     Kevb4         Klist
112200080908     c                   Kfld                    Tasaas
112300080908     c                   Kfld                    Taslnp
112400080908     c                   Kfld                    Tasnrs
112500080908     c                   Kfld                    Tasnsp
112600080908     c                   Kfld                    Wcev
112700970424     C* DEFINISCO CAMPI CHIAVE
112800011012     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
112900970424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
113000970424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
113100011012     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
113200011012     C     *LIKE         DEFINE    TBEKE1        KTEKE1
113300970424     C*
113400970424     C* RECUPERO IL CODICE DELL'AZIENDA
113500970424     C                   Z-ADD     1             CODUT             1 0
113600970424     C                   MOVEL     '0'           $ERR              1
113700970424     C                   MOVEL     *BLANKS       $MSG             78
113800011015     C*
113900011015     C* REPERISCE LA DATA CORRENTE
114000011015     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
114100011015     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
114200011015     C                   Z-ADD     WN8           G08DAT
114300011015     C                   Z-ADD     *ZEROS        G08INV
114400011015     C                   MOVEL     '0'           G08ERR
114500011015     C                   CALL      'XSRDA8'
114600011015     C                   PARM                    WLBDA8
114700011015     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
114800011012     C*
114900011012     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
115000011012     C                   EXSR      CARGEDCE
115100141211
115200141211      /free
115300141211       //?Carico part.consegna con abilitazione PinCode
115400141211         KTBcod = '7R';
115500141211         clear k;
115600141211         setll (KTBute:KTBcod) TABEL00F;
115700141211         reade (KTBute:KTBcod) TABEL00F;
115800141211         DOW  not %eof(TABEL00F);
115900141211           IF  TBLflg = *blanks;
116000141211             ds7R = TBLuni;
116100141211             IF  §7Rpincode = 'S';
116200141211               k += 1;
116300141211               skGMA(k) = TBLkey;
116400141211             ENDIF;
116500141211           ENDIF;
116600141211           reade (KTBute:KTBcod) TABEL00F;
116700141211         ENDDO;
116800141211      /end-free
116900141211
117000970424     C*
117100970424     C* AZZERO I CAMPI IN OUTPUT DEL PGM
117200970424     C                   EXSR      AZZERA
117300970424     C*
117400970424     C                   ENDSR
117500970424     C***********************************************************************
117600970424     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
117700970424     C***********************************************************************
117800970424     C     AZZERA        BEGSR
117900970424     C*
118000970424     C                   MOVEL     *BLANKS       D50ERR
118100970424     C                   MOVEL     *BLANKS       D50MSG
118200970424     C                   Z-ADD     *ZEROS        D50RIC
118300970424     C                   Z-ADD     *ZEROS        D50COM
118400970424     C                   Z-ADD     *ZEROS        D50DEL
118500970424     C*
118600970424     C                   ENDSR
118700060810      /COPY GAITRASRC/SRCTREC,TRECDT
118800970424     C********************************************************************
118900970424** ======== SCHIERA: MSG  (MESSAGGI)                       ================
119000970424TNTE50R- Bolla partenza non trovata o annullata.                          BPNT 1
119100970424TNTE50R- Valuta non trovata nella tab. CV (cambi valuta).                 VNTT 2
119200970424TNTE50R- CAP mittente non trovato.                                        CAPN 3
119300970424TNTE50R- Bolla non calcolabile.                                           NONC 4
119400970424TNTE50R- Tipo bolla non trovato in tabella 3A. Avvertire CED.             3ANT 5
119500970424TNTE50R- 6                                                                ERRO 6
119600970424TNTE50R- 7                                                                ERRO 7
119700970424TNTE50R- 8                                                                ERRO 8
119800970424TNTE50R- 9                                                                ERRO 9
119900970424TNTE50R- 10                                                               ERRO10
120000970424TNTE50R- 11                                                               ERRO11
120100970424TNTE50R- 12                                                               ERRO12
120200970424TNTE50R- 13                                                               ERRO13
120300970424TNTE50R- 14                                                               ERRO14
120400970424TNTE50R- 15                                                               ERRO15
120500970424TNTE50R- 16                                                               ERRO16
120600970424TNTE50R- 17                                                               ERRO17
120700970424TNTE50R- 18                                                               ERRO18
120800970424TNTE50R- 19                                                               ERRO19
120900970424TNTE50R- 20                                                               ERRO20
