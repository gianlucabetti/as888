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
003100970424     D*-------------------
003200970424     D* DS PASSAGGIO PARAMETRI
003300970424     D*-------------------
003400970424     D DS50          E DS                  EXTNAME(TNTE50DS)
003500970424     D*-------------------
003600970424     D* DS CODICI BOLLA
003700970424     D*-------------------
003800970424     D DS3A          E DS
003900060904     D DStb          E DS
004000970424     D*-------------------
004100970424     D* DS TASSAZIONE BOLLA
004200970424     D*-------------------
004300061109     D DTAS01        E DS
004400060809     D DTAS          E DS                  inz
004500060811     D  dksc         E                     extfld(tasksc)
004600060811     D  dctr         E                     extfld(tasctr)
004700060811     D  dncl         E                     extfld(tasncl)
004800060811     D  dstasflo     E                     extfld(tasflo)
004900060809     D DTASv         E DS                  inz
005000060809     d  sv                     1     20
005100060809     d                                     Dim(20)                              Sigle varie
005200060809     d  va                    21    140P 3
005300060809     d                                     Dim(20)                              Importi varie
005400060825     d  vn             s            120
005500060811     d                                     Dim(20)                              Importi varie
005600130301
005700130301      * - Ds per varie file di lavoro
005800130301     d                 ds
005900130301     d  WF1sv1                 1      1
006000130301     d  WF1sv2                 2      2
006100130301     d  WF1sv3                 3      3
006200130301     d  WF1sv4                 4      4
006300130301     d  WF1sv5                 5      5
006400130301     d  WF1sv6                 6      6
006500130301     d  WF1sv7                 7      7
006600130301     d  WF1sv8                 8      8
006700130301     d  WF1sv9                 9      9
006800130301     d  WF1s10                10     10
006900130301     d  WF1s11                11     11
007000130301     d  WF1s12                12     12
007100130301     d  WF1s13                13     13
007200130301     d  WF1s14                14     14
007300130301     d  WF1s15                15     15
007400130301     d  WF1s16                16     16
007500130301     d  WF1s17                17     17
007600130301     d  WF1s18                18     18
007700130301     d  WF1s19                19     19
007800130301     d  WF1s20                20     20
007900130301     d  WF1_sv                 1     20    Dim(20)                              Sigle varie
008000130301      * - Ds per varie file di lavoro
008100130301     d                 ds
008200130301     d  WF1va1                 1     11  3
008300130301     d  WF1va2                12     22  3
008400130301     d  WF1va3                23     33  3
008500130301     d  WF1va4                34     44  3
008600130301     d  WF1va5                45     55  3
008700130301     d  WF1va6                56     66  3
008800130301     d  WF1va7                67     77  3
008900130301     d  WF1va8                78     88  3
009000130301     d  WF1va9                89     99  3
009100130301     d  WF1v10               100    110  3
009200130301     d  WF1v11               111    121  3
009300130301     d  WF1v12               122    132  3
009400130301     d  WF1v13               133    143  3
009500130301     d  WF1v14               144    154  3
009600130301     d  WF1v15               155    165  3
009700130301     d  WF1v16               166    176  3
009800130301     d  WF1v17               177    187  3
009900130301     d  WF1v18               188    198  3
010000130301     d  WF1v19               199    209  3
010100130301     d  WF1v20               210    220  3
010200130301     d  WF1_va                 1    220  3 Dim(20)                              Importo varie
010300040806     D*-------------------
010400040806     D* DS AR5UNI TIP. REC. GEN
010500040806     D*-------------------
010600040806     D DAR5GEN       E DS
010700060809     D DAR5TRS       E DS
010800040806     D*-------------------
010900040806     D* DS TASFLO
011000040806     D*-------------------
011100040806     D DTASFLO       E DS
011200970424     D*------------------
011300970424     D* DS "TISI95R" - CONTROLLO CAP
011400970424     D*------------------
011500970424     D DSSI95        E DS                  EXTNAME(TISI95DS)
011600970424     D*-------------------
011700970424     D* DS CALCOLO COMPETENZE
011800970424     D*-------------------
011900970424     D DS$ECO        E DS
012000970424     D*-------------------
012100970424     D* DS RIDEFINIZIONE IMMAGINE PRIMA E DOPO DI TNTAS10F
012200970424     D*-------------------
012300970424     D TRECDS        E DS
012400970424     D  D$PDS1                 1    250
012500970424     D  D$PDS2               251    500
012600970424     D  D$DDS1               501    750
012700970424     D  D$DDS2               751   1000
012800970424     D  D$ODS1              1001   1250
012900970424     D  D$ODS2              1251   1500
013000030122     D*-------------------
013100030122     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI
013200030122     D*-------------------
013300030122     D DAR5BAN       E DS
013400030729     D*-------------------
013500030729     D* DS LETTURA CAMPO UNICO DI FIAR500F RECORD BANCALI BARTOLINI
013600030729     D*-------------------
013700030729     D DAR5BNB       E DS
013800970424     D*-------------------
013900970424     D* DS PASSAGGIO CAMPI PACKED -CODICE-
014000970424     D*-------------------
014100970424     D                 DS
014200970424     D  DSWCN                  1      3P 0
014300970424     D  DSWCA                  1      3
014400090907     D  DSWIN                  4     13P 5
014500020109     D  DSWIA                  4     13
014600970424     D*-------------------
014700970424     D* DS PER MESSAGGI
014800970424     D*-------------------
014900970424     D                 DS
015000970424     D  DSMSG                  1     78
015100970424     D  DSERR                 75     78
015200980826     D*-------------------
015300980826     D* DS PER COMPORRE DATA SPED.
015400980826     D*-------------------
015500980826     D                 DS
015600060809     D  tasAAS                 1      4  0
015700060809     D  tasMGS                 5      8  0
015800060809     D  tasDSPt                1      8  0
015900970424     D*-------------------
016000970424     D* ARCHITETTURA
016100970424     D*-------------------
016200970424     D KPJBA         E DS
016300011012     D*------------------
016400011012     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
016500011012     D*------------------
016600011012     D DSGEDCE       E DS
016700011012     D*-------------------
016800011012     D* CONVERSIONI IMPORTI
016900011012     D*-------------------
017000011012     D YEUR          E DS                  EXTNAME(YEURCODS)
017100011015     D*-------------------
017200011015     D* DS "XSRDA8" - CONTROLLO DATA (8)
017300011015     D*-------------------
017400011015     D WLBDA8          DS                  INZ
017500011015     D  G08DAT                 1      8  0
017600011015     D  G08INV                 9     16  0
017700011015     D  G08ERR                17     17
017800011015     D  G08TGI                18     22  0
017900130228
018000130228       // -?Data decorrenza tariffa/offerta
018100130228     d parddt          s              8
018200130228     d dataddt         s              8  0
018300130507
018400130507       // -?Flag booleani
018500130507     d wNoWf           s               n   inz(*off)
018600130507
018700130507       // -?Campi
018800130507     d partest         s              1
018900130507     d k               s              3  0
019000130228
019100060809     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS39C
019200060809     ITITAS000      01
019300060809     ITITAS010      02
019301140521     ITITASP00      03
019400970424     C*------------------------------------------------------------------------*
019500970424     C* MAIN LINE
019600970424     C*------------------------------------------------------------------------*
019700970424     C*
019800970424     C* OPERAZIONI INIZIALI
019900970424     C                   EXSR      RUTINZ
020000970424     C*
020100970424     C* SE LANCIATO CON TIPO LANCIO='C' -CHIUDE FILE-, VA A FINE PROGRAMMA
020200970424IF  1C     $ERR          IFEQ      '0'
020300970424IF  2C     D50TLA        IFNE      'C'
020400970424     C*
020500970424     C* RECUPERA I DATI DALLE BOLLE PARTENZA O ARRIVO
020600970424IF  3C     $ERR          IFEQ      '0'
020700970424IF  4C     D50NSP        IFNE      *ZEROS                                       *DS NON IMPOSTATA
020800060809     C                   EXSR      LETtas                                       *LEGGE BOLLA PART.
020900970424E   4C                   ENDIF
021000970424E   3C                   ENDIF
021100970424     C*
021200970424     C* CALCOLA I RICAVI
021300970424IF  3C     $ERR          IFEQ      '0'
021400970424     C                   EXSR      TASSAZ
021500970424E   3C                   ENDIF
021600970424     C*
021700970424     C* CALCOLA LE COMPETENZE
021800970424IF  3C     $ERR          IFEQ      '0'
021900060904     C*    §3ATB1        ANDEQ     'F1'
022000970424     C                   EXSR      CALCOM
022100971027E   3C                   ENDIF
022200970424     C*
022300970424     C* CALCOLA IL DELTA
022400970424     C                   Z-ADD     D50RIC        WRRW             16 3          *RICAVO REALE
022500970424     C                   Z-ADD     D50COM        WRPW             16 3          *RICAVO PRESUNTO
022600970424     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
022700970424     C                   EXSR      CALDEL
022800970424     C                   Z-ADD     WDPW          D50DEL                         *DELTA
022900970424     C*
023000970424E   2C                   ENDIF
023100970424E   1C                   ENDIF
023200970424     C*
023300970424     C* OPERAZIONI DI CHIUSURA
023400970424IF  1C     D50TLA        IFEQ      'C'
023500970424     C*
023600970424     C                   MOVEL     'C'           D$GSTS
023700970424     C                   CALL      'TRECD8R'                            98
023800970424     C                   PARM                    DS$ECO
023900970424     C*
024000970424     C                   MOVEL     'C'           TASTLA
024100130228     C                   CALL      'TNSF20R'                            99
024200970424     C                   PARM                    KPJBA
024300991126     C                   PARM                    DTAS
024400991126     C                   PARM                    DTASV
024500970424     C*
024600970424     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
024700970424     C                   MOVEL     'C'           I95TLA
024800970424     C                   CALL      'TISI95R'
024900970424     C                   PARM                    DSSI95
025000970424     C* FINE PROGRAMMA
025100970424     C                   SETON                                        LR
025200970424X   1C                   ELSE
025300970424     C                   SETON                                        RT
025400970424E   1C                   ENDIF
025500970424     C*------------------------------------------------------------------------*
025600060809     C* LETtas - LEGGE LE BOLLE PARTENZA
025700970424     C*------------------------------------------------------------------------*
025800060809     C     LETtas        BEGSR
025900970424     C*
026000060809     C                   Z-ADD     D50AAS        tasAAS
026100060809     C                   Z-ADD     D50LNP        tasLNP
026200060809     C                   Z-ADD     D50NRS        tasNRS
026300060809     C                   Z-ADD     D50NSP        tasNSP
026400060809     c                   setoff                                       0102
026500060809     C     KEYtas        CHAIN     titas30c                           99
026600970424IF  1C     *IN99         IFEQ      '1'                                          *NON TROV.
026700970424     C                   MOVEL     MSG(1)        $MSG                           *BOLLA NON TROV. O A
026800011102     C                   EXSR      ERRORE
026900970424E   1C                   ENDIF
027000970424     C*
027100970424     C* RECUPERA IL CODICE BOLLA
027200970424IF  1C     $ERR          IFEQ      '0'
027300970424     C                   Z-ADD     CODUT         KTBUTE
027400970424     C                   MOVEL     '3A'          KTBCOD
027500970424     C                   MOVEL     *BLANKS       KTBKEY
027600060809     C                   MOVEL     tasCBO        KTBKEY
027700970424     C     KEYTAB        CHAIN     TABEL00F                           99
027800970424IF  2C     *IN99         IFEQ      '1'
027900970424     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
028000970424     C                   EXSR      ERRORE
028100060904     c                   else
028200060904     C                   MOVEL     TBLUNI        DS3A
028300060904     C                   MOVEL     §3ATB1        FLGTB1            1
028400970424E   2C                   ENDIF
028500970424E   1C                   ENDIF
028600060904     c* solo tipi bolla Normali (F1 e A2)
028700060904IF  1C     $ERR          IFEQ      '0'
028800060904     C                   Z-ADD     CODUT         KTBUTE
028900060904     C                   MOVEL     'TB'          KTBCOD
029000060904     C                   MOVEL     *BLANKS       KTBKEY
029100060904     C                   MOVEL     §3ATB1        KTBKEY
029200060904     C     KEYTAB        CHAIN     TABEL00F                           99
029300060904IF  2C     *IN99         IFEQ      '1'
029400060904     C                   MOVEL     MSG(5)        $MSG                           *TAB. 3A NON TROV.
029500060904     C                   EXSR      ERRORE
029600060904     c                   else
029700060904     C                   MOVEL     TBLUNI        DSTB
029800060904E   2C                   ENDIF
029900060904E   1C                   ENDIF
030000970424     C*
030100970424     C* CONTROLLA SE IL TIPO BOLLA E' CALCOLABILE
030200970424IF  1C     $ERR          IFEQ      '0'
030300060904IF  2C     §tbrbl        IFNE      'N'
030400970424     C                   MOVEL     MSG(4)        $MSG                           *TIPO BOLLA NON CALC
030500970424     C                   EXSR      ERRORE
030600970424E   2C                   ENDIF
030700970424E   1C                   ENDIF
030800970424     C*
030900970424     C* LEGGE I CONTRASSEGNI
031000061221     C                   CLEAR                   csbsta
031100061221     C                   CLEAR                   csbVCA
031200061221     C                   CLEAR                   csbcmb
031300061221     C                   CLEAR                   csbCAS
031400061221     C                   CLEAR                   csbtpi
031500061221     C                   CLEAR                   csbtpa
031600061221     C                   CLEAR                   csbfus
031700060809      * mittente
031800970424IF  1C     $ERR          IFEQ      '0'
031900970424     C     §3AFCA        ANDNE     *ZEROS                                       *HA CONTRASSEGNO
032000060809     C                   Z-ADD     D50AAS        csbAAS
032100060809     C                   Z-ADD     D50LNP        csbLNP
032200060809     C                   Z-ADD     D50NRS        csbNRS
032300060809     C                   Z-ADD     D50NSP        csbNSP
032400060809     C     KEYcsb        CHAIN     tncsb03l                           99
032500970424IF  2C     *IN99         IFEQ      '1'                                          *NON TROVATO
032600061221     C                   CLEAR                   csbsta
032700061221     C                   CLEAR                   csbVCA
032800061221     C                   CLEAR                   csbcmb
032900060809     C                   CLEAR                   csbCAS
033000061221     C                   CLEAR                   csbtpi
033100061221     C                   CLEAR                   csbtpa
033200060809     C                   CLEAR                   csbfus
033300970424E   2C                   ENDIF
033400970424E   1C                   ENDIF
033500030122     C*
033600030122     C* LEGGE IL FILE ESTENSIONI PER I BANCALI
033700030122     C                   CLEAR                   DAR5BAN
033800060809If  1c                   If        %Subst(tasGva:1:1) ='B'
033900030122IF  1C     $ERR          IFEQ      '0'
034000030122     C                   Z-ADD     D50AAS        AR5AAS
034100030122     C                   Z-ADD     D50LNP        AR5LNP
034200030122     C                   Z-ADD     D50NRS        AR5NRS
034300030122     C                   Z-ADD     D50NSP        AR5NSP
034400030122     C                   MOVEL     'BAN'         AR5TRD
034500060809     C                   CLEAR                   AR5PRG
034600060809     C     KEYAR5        CHAIN     FIAR531c                           99
034700030122IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
034800030122     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
034900030122     C                   MOVEL     AR5UNI        DAR5BAN
035000030122E   2C                   ENDIF
035100030122E   1C                   ENDIF
035200030729     c                   EndIf
035300030729     C* LEGGE IL FILE ESTENSIONI PER I BANCALI BARTOLINI
035400030729     c                   Clear                   dAr5Bnb
035500060809If  1c                   If        %Subst(tasGva:1:1) ='O'
035600030729IF  1C     $ERR          IFEQ      '0'
035700030729     C                   Z-ADD     D50AAS        AR5AAS
035800030729     C                   Z-ADD     D50LNP        AR5LNP
035900030729     C                   Z-ADD     D50NRS        AR5NRS
036000030729     C                   Z-ADD     D50NSP        AR5NSP
036100030729     c                   Eval      Ar5Trd = 'BNB'
036200060809     C                   CLEAR                   AR5PRG
036300060809     C     KEYAR5        CHAIN     FIAR531c                           99
036400030729IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
036500030729     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
036600030729     c                   Movel     AR5UNI        dAr5Bnb
036700030729E   2C                   ENDIF
036800030729E   1C                   ENDIF
036900030729     c                   EndIf
037000060809     C* LEGGO fiar500F x transiti
037100060809     C                   CLEAR                   DAR5TRS
037200060809    6C                   if        TASFLP > *zeros OR TASFL2 > *zeros
037300060809IF  1C     $ERR          IFEQ      '0'
037400060809     C                   MOVEL     'TRS'         AR5TRD
037500060809     C                   EVAL      AR5PRG = TASFLP
037600060809     C     KEYAR5        CHAIN     FIAR531C                           99
037700060809if  7C     *IN99         IFEQ      '0'
037800060809     C     AR5ATB        andeq     *BLANKS
037900060809     C                   MOVEL     AR5UNI        DAR5TRS
038000060809     C                   END
038100060809     C                   END
038200060809     C                   END
038300040806     C*
038400040806     C* LEGGE IL FILE ESTENSIONI PER LA MERCE DI VALORE
038500040806     C                   CLEAR                   DAR5GEN
038600040806IF  1C     $ERR          IFEQ      '0'
038700040806     C                   Z-ADD     D50AAS        AR5AAS
038800040806     C                   Z-ADD     D50LNP        AR5LNP
038900040806     C                   Z-ADD     D50NRS        AR5NRS
039000040806     C                   Z-ADD     D50NSP        AR5NSP
039100040806     C                   MOVEL     'GEN'         AR5TRD
039200060809     C     KEYAR5        CHAIN     fiar531c                           99
039300040806IF  2C     *IN99         IFEQ      *OFF                                         *TROVATO
039400040806     C     AR5ATB        ANDEQ     *BLANKS                                      *NON ANNULLATO
039500040806     C                   MOVEL     AR5UNI        DAR5GEN
039600040806E   2C                   ENDIF
039700040806E   1C                   ENDIF
039800970424     C*
039900970424     C                   ENDSR
040000970424     C*------------------------------------------------------------------------*
040100970424     C* TASSAZ - TASSAZIONE BOLLA
040200970424     C*------------------------------------------------------------------------*
040300970424     C     TASSAZ        BEGSR
040400060810     c*
040500061221     c                   clear                   tassta
040600061221     c                   clear                   tascas
040700061221     c                   clear                   tastic
040800061221     c                   clear                   tasvca
040900061221     c                   clear                   tascmb
041000061221     c                   clear                   tassva
041100060809     c                   movel     *Blanks       tastla
041200060809     c                   movel     *Blanks       taserr
041300060809     c                   movel     *Blanks       tasmsg
041400060809     c                   clear                   tasial
041500060809     c                   clear                   task88
041600060809     c                   clear                   tasfcaa
041700060809     c                   clear                   taspkcn
041800060809     c                   clear                   tasric
041900060809     c                   clear                   tasspc
042000060809     c                   clear                   tasBan
042100080908     c                   clear                   tasric
042200060810     c                   movel     Tasftc        tastc1
042300080908      * verifico se devo calcolare l'addebito di lasciato avviso
042400080908      * verifico se c'è evento
042500080908     c     Kevb4         chain     Fnevb04l
042600080908
042700080908     c                   If        %found(Fnevb04l)
042800080908     c                   eval      tasric = 'S'
042900080908     c                   endif
043000060825     c                   clear                   tasimv
043100060825     c                   clear                   taspor
043200060825     c*
043300060825     c                   if        $err='0'
043400060907     C                   CLEAR                   DTASV
043500130301     C                   CLEAR                   WF1_SV
043600130301     C                   CLEAR                   WF1_VA
043700970424     C*
043800970424IF  1C     D50KSC        IFNE      *ZEROS
043900060811     C                   MOVEL     D50KSC        dKSC
044000060811     C                   MOVEL     D50CTR        dCTR
044100060811     c                   else
044200060811     C                   MOVEL     tasKSC        dKSC
044300060811     C                   MOVEL     tasCTR        dCTR
044400970424E   1C                   ENDIF
044500970424     C     D50DTR        IFGT      *ZEROS
044600970424     C                   Z-ADD     D50DTR        TASDSP
044700060810     c                   else
044800060810     C                   MOVEL     TASDSPt       TASDsp                         AAAA/MM/GG
044900970424     C                   ENDIF
045000130228     C                   MOVEL     TASDSP        TASDCT                         AAAA/MM/GG
045100130228     C**                 MOVEL     TASDSPt       TASDCT                         AAAA/MM/GG
045200970424    1C     §3AFCA        IFEQ      1
045300061221     c                   movel     csbsta        Tassta
045400060809     c                   z-add     csbcas        Tascas
045500061221     c                   z-add     csbcmb        tascmb
045600061221     c                   movel     csbvca        tasvca
045700060809     c                   if        csbfus <> *blanks
045800060809     c                   move      csbfus        tastic
045900060809     c                   else
046000060809     c                   move      csbtpi        tastic
046100060809     c                   end
046200970424E   1C                   ENDIF
046300060809      * Flag SI NO DDT
046400060809    1C                   If        tasll1 = 'C' or tasll1 = 'S'
046500060809     C                   movel     'S'           Tasddt
046600060809   x1C                   Else
046700060809     C                   Clear                   Tasddt
046800060809    1C                   Endif
046900061109     c                   clear                   dtas01
047000061109     c                   movel     tasfbr        §asfbr
047100061109     c                   movel     tascca        §ascca
047200110804     c* servizio email
047300110804     c                   eval      dtasflo = tasflo
047400110804     c                   eval      §asemd=§floemd
047500130228      * data decorrenza tariffa/offerta
047600130228     c                   IF        dataddt > 0
047700130228     c                   eval      §asdrt = %editc(dataddt:'X')
047800130228     c                   ENDIF
047900061109     c                   eval      dstasflo = dtas01
048000030204      * Bancali
048100060809If  1c                   If        %Subst(tasGva:1:1) ='B'
048200030204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
048300030205    1c                   EndIf
048400030729      * Colli originali
048500060809If  1c                   If        %Subst(tasGva:1:1) ='O'
048600060811     c                   Eval      dNcl = §Ar5bCor
048700061030     c                   else
048800061030     c                   Eval      dNcl = tasncl
048900030729    1c                   EndIf
049000130226      * Assicurazione, se presente è da togliere
049100130226     c                   IF        TASfcm = 'F'
049200130226     c                   clear                   TASfcm
049300130226     c                   clear                   TASias
049400130226     c                   clear                   TASvas
049500130226     c                   ENDIF
049600970424     C*--------------------------
049700970424     C*  D S T A S V
049800970424     C*--------------------------
049900970424     C                   CLEAR                   KPJBU
050000970424     C*--------------------------
050100011102     C*  CALCOLO TASSAZIONE
050200970424     C*--------------------------
050300130228     C                   CALL      'TNSF20R'                            99
050400970424     C                   PARM                    KPJBA
050500991126     C                   PARM                    DTAS
050600991126     C                   PARM                    DTASV
050700011102     C*
050800130228     C* In uscita dal TNSF20R verifico la divisa della DS di passaggio rispetto a quella
050900011102     C* di calcolo del CAT reperita dalla tabella GED tipo record CE.
051000011102     C                   EXSR      CNVDTAS
051100060809     c                   end
051200011102     C*
051300970424     C                   Z-ADD     TASPVL        D50PVL
051400060904IF  1C     tasERR        IFNE      ' '                                          *ERRORE
051500060904     C                   MOVEL     tasMSG        DSMSG
051600130228     C                   MOVEL     'SF20'        DSERR
051700970424     C                   MOVEL     DSMSG         $MSG
051800970424     C                   EXSR      ERRORE
051900970424X   1C                   ELSE
052000991126     C                   Z-ADD     TASIMV        D50RIC
052100970424E   1C                   ENDIF
052200130301      * scrivo file delle varie
052300130507     c                   IF        not wNoWf
052400130301     c                   exsr      wrtVarie
052500130507     c                   ENDIF
052600970424     C*
052700970424     C                   ENDSR
052800970424     C**************************************************************************
052900970424     C*    CALCOLO COMPETENZE
053000970424     C**************************************************************************
053100970424     C     CALCOM        BEGSR
053200970424     C*
053300970424     C                   CLEAR                   DS$ECO
053400970424     C                   CLEAR                   TRECDS
053500060810     C                   EXSR      AZZW
053600970424     C                   MOVEL     D50TLA        D$GSTS                         *STATUS
053700970424     C                   MOVEL     'TNTE50R'     D$GPGF                         *PGM CHE HA MODIF.FI
053800060810     C                   EXSR      cddTAQ
053900060904     C                   EXSR      AZZnCDDTAQ
054000970424     C* CONTRASSEGNO
054100060809     c                   movel     csbtpa        D$DTPI
054200060810     c                   movel     csbsta        D$Dsta
054300060809     c                   if        csbfus <> *blanks
054400060809     c                   move      csbfus        D$DTPI
054500060809     c                   else
054600060809     c                   move      csbtpi        D$DTPI
054700060809     c                   end
054800020125     C                   Z-ADD     TASCAS        D$DCAS                         *LIRE CONTRASS
054900970424     C* ASSICURAZIONE
055000020125     C                   Z-ADD     TASIAS        D$DVAE
055100060809     C* TRANSITI
055200060809     C                   Z-ADD     §AR5flp       D$DAR5flp
055300060809     C                   Z-ADD     §AR5fl2       D$DAR5fl2
055400060809     C                   Z-ADD     §AR5fl3       D$DAR5fl3
055500060809     C                   Z-ADD     §AR5DE3       D$DAR5DE3
055600060809     C                   Z-ADD     §AR5DU3       D$DAR5DU3
055700060809     C                   Z-ADD     §AR5fl4       D$DAR5fl4
055800060809     C                   Z-ADD     §AR5DE4       D$DAR5DE4
055900060809     C                   Z-ADD     §AR5DU4       D$DAR5DU4
056000060809     C                   Z-ADD     §AR5fl5       D$DAR5fl5
056100060809     C                   Z-ADD     §AR5DE5       D$DAR5DE5
056200060809     C                   Z-ADD     §AR5DU5       D$DAR5DU5
056300060809     C                   Z-ADD     §AR5DET       D$DAR5DET
056400060809     C                   Z-ADD     §AR5DUT       D$DAR5DUT
056500060809     C                   Z-ADD     §AR5DE2       D$DAR5DE2
056600060809     C                   Z-ADD     §AR5DU2       D$DAR5DU2
056700030204     C*
056800030122     C                   Z-ADD     §AR5TBA       D$DAR5TBA                      *TIPO BANCALE 1
056900030122     C                   Z-ADD     §AR5NBA       D$DAR5NBA                      *NUM. BANCALE 1
057000030122     C                   Z-ADD     §AR5TB2       D$DAR5TB2                      *TIPO BANCALE 2
057100030122     C                   Z-ADD     §AR5NB2       D$DAR5NB2                      *NUM. BANCALE 2
057200030730     C*
057300030730     c                   IF        §AR5BNBA > *zeros OR
057400030730     c                             §AR5BNB2 > *zeros
057500030730     C                   Z-ADD     §AR5BTBA      D$DAR5TBA                      *TIPO BANCALE 1 BAR
057600030730     C                   Z-ADD     §AR5BNBA      D$DAR5NBA                      *NUM. BANCALE 1 BAR
057700030730     C                   Z-ADD     §AR5BTB2      D$DAR5TB2                      *TIPO BANCALE 2 BAR
057800030730     C                   Z-ADD     §AR5BNB2      D$DAR5NB2                      *NUM. BANCALE 2 BAR
057900030730     c                   ENDIF
058000040315     C                   Z-ADD     §AR5BCSF      D$DAR5CSF                      *NUM. BANCALE 2 BAR
058100040315     C                   Z-ADD     §AR5BCOR      D$DAR5COR                      *NUM. BANCALE 2 BAR
058200030730     C*
058300970424     C* SCHIERA RICAVI
058400030515     C     1             DO        75            I                              *AZZERA SCHIERE
058500970424     C                   Z-ADD     *ZEROS        DSWCN                          SCH NUM --> DS NUM
058600970424     C                   MOVE      DSWCA         CAL(I)                         DS ALF --> SCH ALF
058700970424     C                   Z-ADD     *ZEROS        DSWIN
058800970424     C                   MOVE      DSWIA         IAL(I)
058900970424     C                   ENDDO
059000970424     C                   Z-ADD     556           DSWCN                          SCH NUM --> DS NUM
059100970424     C                   MOVE      DSWCA         CAL(1)                         DS ALF --> SCH ALF
059200970424     C                   Z-ADD     D50RIC        DSWIN
059300970424     C                   MOVE      DSWIA         IAL(1)
059400970424     C                   MOVEA     CAL           D$DCCD                         *DS COD. RICAVI
059500970424     C                   MOVEA     IAL           D$DICD                         *DS IMP.RICAVI
059600060809     C                   MOVEL     tasmct        D$DMCT
059700060811     C                   MOVEA     SV            D$DSVN
059800060825     c                   do        20            k
059900060825     C                   MOVE      Va(k)         vn(k)
060000060825     c                   end
060100060825     C                   MOVEA     Vn            D$DVAN
060200060904     C* non considero la GIACENZA
060300970424     C                   MOVEL     'N'           D$DFGC                         *FORZATURA GIAC=NO
060400051212     C                   Z-ADD     0             D$DGGS
060500970424     C* NUMERO STOP DI RITIRO
060600020109     C                   Z-ADD     1             D$DNSR                         *N.STOP RITIRO
060700060810IF  1C     tasstr        IFGT      1
060800060810     C     1             DIV       tasstr        D$DNSR                         *1 / N. DI STOP
060900020109IF  2C     D$DNSR        IFLT      0,00001
061000020109     C                   Z-ADD     0,00001       D$DNSR
061100971024E   2C                   ENDIF
061200970424E   1C                   ENDIF
061300971027     C* NUMERO STOP DI CONSEGNA
061400971027     C                   Z-ADD     1             D$DNSC                         *N.STOP CONSEGNA
061500060810IF  1C     tasstp        IFGT      1
061600060810     C     1             DIV       tasstp        D$DNSC                         *1 / N. DI STOP
061700020109IF  2C     D$DNSC        IFLT      0,00001
061800020109     C                   Z-ADD     0,00001       D$DNSC
061900971027E   2C                   ENDIF
062000971027E   1C                   ENDIF
062100971024     C*
062200971024     C* IMPOSTA ALTRI CAMPI "DOPO"
062300980826     C                   MOVEL     §3ATB1        D$DTPO                         *TIPO PORTO F/A
062400060810     C     D$DFBR        IFNE      'S'
062500060810     C                   MOVEL     'S'           D$DFIG
062600060810     C                   ENDIF
062700970424     C*
062800970424     C                   MOVEL     D$DDS1        D$DRE1                         *DS DOPO
062900970424     C                   MOVEL     D$DDS2        D$DRE2
063000970424     C                   MOVEL     D$ODS1        D$ORE1                         *DS ORIG
063100970424     C                   MOVEL     D$ODS2        D$ORE2
063200970424     C*
063300970424     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
063400970424     C                   CALL      'TRECD8R'                            98
063500970424     C                   PARM                    DS$ECO                         *DS IMMAGINI BOLLA
063600970424     C*
063700970424     C* SE RITORNA ERRORE ESCO PGM
063800970424     C     D$GERR        IFNE      *BLANKS
063900970424     C                   MOVEL     D$GMSG        $MSG
064000970424     C                   EXSR      ERRORE
064100970424     C                   ENDIF
064200970424     C*
064300970424     C     $ERR          IFEQ      '0'                                          --- 1 -->
064400970424     C*
064500970424     C* TRATTO CODICI E IMPORTI COMPETENZE -DOPO-
064600970424     C                   MOVEA     D$DCCD        CAL                            SCHIERE ALFANUMERICH
064700970424     C                   MOVEA     D$DICD        IAL
064800030515     C     1             DO        75            I                 6 0
064900970424     C                   MOVE      CAL(I)        DSWCA                          SCH ALF --> DS ALF
065000970424     C                   Z-ADD     DSWCN         CD                6 0          DS NUM --> SCH NUM
065100970424     C     CD            IFNE      *ZEROS
065200970424     C                   MOVE      IAL(I)        DSWIA
065300090922     C                   Z-ADD     DSWIN         IMP              13 5
065400970424     C     CD            IFEQ      900                                          *RICAVO PRES. DELTA
065500090922     C                   Z-ADD(h)  IMP           D50COM
065600970424     C                   ENDIF
065700970424     C                   ENDIF
065800970424     C                   ENDDO
065900970424     C                   ENDIF                                                  <-- 1 ---
066000970424     C*
066100970424     C                   ENDSR
066200130301
066300130301      ********************************************************************
066400130301      *   wrtVarie - Scrivo file delle varie
066500130301      ********************************************************************
066600130301     c     wrtvarie      BEGSR
066700130301
066800130301     c                   clear                   wfdbf100
066900130301
067000130301     c                   eval      WF1aas = TASaas
067100130301     c                   eval      WF1mgs = TASmgs
067200130301     c                   eval      WF1lnp = TASlnp
067300130301     c                   eval      WF1nrs = TASnrs
067400130301     c                   eval      WF1nsp = TASnsp
067500130301     c                   eval      WF1ksc = TASksc
067600130301     c                   eval      WF1div = TASdiv
067700130301     c                   eval      WF1por = TASpor
067800130301
067900130301     c                   movea     sv            wf1_sv
068000130301     c                   movea     va            wf1_va
068100130301
068200130301     c                   eval      WF1imv = TASimv
068300130301
068400130301     c                   write     wfdbf100
068500130301
068600130301     c                   ENDSR
068700130301
068800060810     C********************************************************************
068900060810     C*   AZZW   - AZZERA TUTTI I CAMPI DI LAVORO
069000060810     C********************************************************************
069100060810     C     AZZW          BEGSR
069200060810     C*
069300060810     C* AZZERO CAMPI DS ESTERNA
069400060810     c* è possibile usare all'interno del pgm solo i campi prima e dopo
069500060810     c* del TITAS senza testare i relativi flag prima o dopo.
069600060810     c* Se si vogliono usare i campi degli altri files senza testare i
069700060810     c* flag prima o dopo, bisogna inserire le routine di inizializzazione
069800060810     c* contenute nel /COPY
069900060810     C                   EXSR      APDTAQ
070000060810     C                   EXSR      ADDTAQ
070100060810     C                   EXSR      AODTAQ
070200060810     C                   MOVEL     D$ODS1        D$ORE1
070300060810     C                   MOVEL     D$ODS2        D$ORE2
070400060810     C                   MOVEL     *BLANKS       D$PCO1
070500060810     C                   MOVEL     *BLANKS       D$PCO2
070600060810     C                   MOVEL     *BLANKS       D$DCO1
070700060810     C                   MOVEL     *BLANKS       D$DCO2
070800060810     C                   MOVEL     *BLANKS       D$PSVN
070900060810     C                   MOVEL     *BLANKS       D$PVAN
071000060810     C                   MOVEL     *BLANKS       D$DSVN
071100060810     C                   MOVEL     *BLANKS       D$DVAN
071200060810     C                   MOVEL     *BLANKS       D$OSVN
071300060810     C                   MOVEL     *BLANKS       D$OVAN
071400060810     C                   Z-ADD     *ZEROS        D$PCAS
071500060810     C                   MOVEL     *BLANKS       D$PTPI
071600060810     C                   MOVEL     *BLANKS       D$Psta
071700060810     C                   Z-ADD     *ZEROS        D$DCAS
071800060810     C                   MOVEL     *BLANKS       D$DTPI
071900060810     C                   MOVEL     *BLANKS       D$Dsta
072000060810     C                   Z-ADD     *ZEROS        D$PVAE
072100060810     C                   Z-ADD     *ZEROS        D$DVAE
072200060810     C                   Z-ADD     *ZEROS        D$PNSR
072300060810     C                   Z-ADD     *ZEROS        D$DNSR
072400060810     C                   Z-ADD     *ZEROS        D$PAGC
072500060810     C                   Z-ADD     *ZEROS        D$PMGC
072600060810     C                   Z-ADD     *ZEROS        D$DAGC
072700060810     C                   Z-ADD     *ZEROS        D$DMGC
072800060810     C                   MOVEL     *BLANKS       D$PTPO
072900060810     C                   MOVEL     *BLANKS       D$PRBL
073000060810     C                   MOVEL     'N'           D$PCSF
073100060810     C                   MOVEL     'N'           D$PCCS
073200060810     C                   MOVEL     'N'           D$PDBO
073300060810     C                   MOVEL     'N'           D$PFRI
073400060810     C                   MOVEL     'N'           D$PFSP
073500060810     C                   MOVEL     *BLANKS       D$DTPO
073600060810     C                   MOVEL     *BLANKS       D$DRBL
073700060810     C                   MOVEL     'N'           D$DCSF
073800060810     C                   MOVEL     'N'           D$DCCS
073900060810     C                   MOVEL     'N'           D$DDBO
074000060810     C                   MOVEL     'N'           D$DFRI
074100060810     C                   MOVEL     'N'           D$DFSP
074200060810     C                   MOVEL     *BLANKS       D$PAR5T
074300060810     C                   MOVEL     *BLANKS       D$DAR5T
074400060810     C                   MOVEL     *BLANKS       D$PAR5B
074500060810     C                   MOVEL     *BLANKS       D$DAR5B
074600060810     C                   Z-ADD     *ZEROS        D$PAR5flp
074700060810     C                   Z-ADD     *ZEROS        D$PAR5DET
074800060810     C                   Z-ADD     *ZEROS        D$PAR5DUT
074900060810     C                   Z-ADD     *ZEROS        D$PAR5fl2
075000060810     C                   Z-ADD     *ZEROS        D$PAR5DE2
075100060810     C                   Z-ADD     *ZEROS        D$PAR5DU2
075200060810     C                   Z-ADD     *ZEROS        D$PAR5fl3
075300060810     C                   Z-ADD     *ZEROS        D$PAR5DE3
075400060810     C                   Z-ADD     *ZEROS        D$PAR5DU3
075500060810     C                   Z-ADD     *ZEROS        D$PAR5fl4
075600060810     C                   Z-ADD     *ZEROS        D$PAR5DE4
075700060810     C                   Z-ADD     *ZEROS        D$PAR5DU4
075800060810     C                   Z-ADD     *ZEROS        D$PAR5fl5
075900060810     C                   Z-ADD     *ZEROS        D$PAR5DE5
076000060810     C                   Z-ADD     *ZEROS        D$PAR5DU5
076100060810     C                   Z-ADD     *ZEROS        D$DAR5flp
076200060810     C                   Z-ADD     *ZEROS        D$DAR5DET
076300060810     C                   Z-ADD     *ZEROS        D$DAR5DUT
076400060810     C                   Z-ADD     *ZEROS        D$DAR5fl2
076500060810     C                   Z-ADD     *ZEROS        D$DAR5DE2
076600060810     C                   Z-ADD     *ZEROS        D$DAR5DU2
076700060810     C                   Z-ADD     *ZEROS        D$DAR5fl3
076800060810     C                   Z-ADD     *ZEROS        D$DAR5DE3
076900060810     C                   Z-ADD     *ZEROS        D$DAR5DU3
077000060810     C                   Z-ADD     *ZEROS        D$DAR5fl4
077100060810     C                   Z-ADD     *ZEROS        D$DAR5DE4
077200060810     C                   Z-ADD     *ZEROS        D$DAR5DU4
077300060810     C                   Z-ADD     *ZEROS        D$DAR5fl5
077400060810     C                   Z-ADD     *ZEROS        D$DAR5DE5
077500060810     C                   Z-ADD     *ZEROS        D$DAR5DU5
077600060810     C                   Z-ADD     *ZEROS        D$PAR5TBA
077700060810     C                   Z-ADD     *ZEROS        D$PAR5NBA
077800060810     C                   Z-ADD     *ZEROS        D$PAR5TB2
077900060810     C                   Z-ADD     *ZEROS        D$PAR5NB2
078000060810     C                   Z-ADD     *ZEROS        D$DAR5TBA
078100060810     C                   Z-ADD     *ZEROS        D$DAR5NBA
078200060810     C                   Z-ADD     *ZEROS        D$DAR5TB2
078300060810     C                   Z-ADD     *ZEROS        D$DAR5NB2
078400060810     C                   Z-ADD     *ZEROS        D$PAR5CSF
078500060810     C                   Z-ADD     *ZEROS        D$PAR5COR
078600060810     C                   Z-ADD     *ZEROS        D$DAR5CSF
078700060810     C                   Z-ADD     *ZEROS        D$DAR5COR
078800060810     C                   MOVEL     *BLANKS       D$XXXX
078900060810     C*
079000060810     C                   ENDSR
079100060904     C*-------------------------------------------------------------------
079200060904     C*?Azzera varia "N" immagine -dopo- TITAS
079300060904     C*-------------------------------------------------------------------
079400060904     C     AZZnCDDTAQ    BEGSR
079500060904     c*
079600060904if  1C                   IF        (D$DVA1 = 88888888,000)
079700060904     C                   Z-ADD     *zeros        D$DVA1
079800060904e   1c                   ENDIF
079900060904if  1C                   IF        (D$DVA2 = 88888888,000)
080000060904     C                   Z-ADD     *zeros        D$DVA2
080100060904e   1c                   ENDIF
080200060904if  1C                   IF        (D$DVA3 = 88888888,000)
080300060904     C                   Z-ADD     *zeros        D$DVA3
080400060904e   1c                   ENDIF
080500060904     c*
080600060904     C                   ENDSR
080700970424     C********************************************************************
080800970424     C*   CALDEL - CALCOLA IL DELTA
080900970424     C*          INPUT  --> WRRW: RICAVO REALE
081000970424     C*                     WRPW: RICAVO PRESUNTO
081100970424     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
081200970424     C********************************************************************
081300970424     C     CALDEL        BEGSR
081400970424     C*
081500970424     C*--------------------
081600970424     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
081700970424     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
081800970424     C*--------------------
081900970424     C*
082000970424     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
082100970424     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
082200970424     C     GAP           MULT      100           GAP                            (GAP x 100)
082300970424     C*
082400970424     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
082500970424IF  1C     WRRW          IFEQ      *ZEROS
082600970424     C                   Z-ADD     1             WRRW
082700970424E   1C                   ENDIF
082800970424     C*
082900970424     C     GAP           DIV       WRRW          GAP
083000970424     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
083100970424     C*
083200970424     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
083300970424     C     WDPW          COMP      0                                    28
083400970424     C* POSITIVO
083500970424IF  1C     *IN28         IFEQ      *OFF
083600970424IF  2C     GAP           IFGE      1000
083700970424     C                   Z-ADD     +999,9        WDPW
083800970424E   2C                   ENDIF
083900970424     C* NEGATIVO
084000970424X   1C                   ELSE
084100970424IF  2C     GAP           IFLE      -1000
084200970424     C                   Z-ADD     -999,9        WDPW
084300970424E   2C                   ENDIF
084400970424E   1C                   ENDIF
084500970424     C*
084600970424     C                   ENDSR
084700970424     C********************************************************************
084800970424     C*   ERRORE - ROUTINE GESTIONE ERRORI
084900970424     C********************************************************************
085000970424     C     ERRORE        BEGSR
085100970424     C*
085200970424     C                   MOVEL     'S'           $ERR
085300970424     C                   MOVEL     $MSG          D50MSG
085400970424     C                   MOVE      $MSG          D50ERR
085500970424     C*
085600970424     C                   ENDSR
085700011012     C*------------------------------------------------------------------------*
085800011012     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
085900011012     C*------------------------------------------------------------------------*
086000011012     C     CARGEDCE      BEGSR
086100011012     C*
086200011012     C                   CLEAR                   DSGEDCE
086300011026     C                   MOVEL     *blanks       WDIVCE            3
086400011012     C*
086500011012     C                   MOVEL(P)  'GED'         KTECOD
086600011012     C                   MOVEL(P)  'CE'          KTEKE1
086700011012     C     KEYTBE        CHAIN     TNTBE01L
086800011012     C                   IF        %found(TNTBE01L)
086900011012     C                   MOVEL     TBEUNI        DSGEDCE
087000011012     C                   ENDIF
087100011012     C*
087200011102     C                   IF        §GEDCEDAT > DATCOR
087300011026     C                   EVAL      WDIVCE = §GEDCEDIP
087400011026     C                   ELSE
087500011026     C                   EVAL      WDIVCE = §GEDCEDIV
087600011012     C                   ENDIF
087700011012     C*
087800011012     C                   ENDSR
087900011102     C*------------------------------------------------------------------------*
088000011102     C* CNVDTAS - CONVERTE I CAMPI DELLE DS DTAS/DTASV NELLA DIVISA DI CALCOLO DEL CE
088100011102     C*------------------------------------------------------------------------*
088200011102     C     CNVDTAS       BEGSR
088300011102     C*
088400011102     C* CONTRASSEGNO
088500011102     C                   Z-ADD     TASCAS        WIMI             30 9
088600011107     C                   MOVEL     TASVCA        WDVI              3
088700011102     C                   EXSR      CNVEUR
088800011102     C                   Z-ADD     WIMO          TASCAS
088900011102     C*
089000011102     C* IMPORTO DA ASSICURARE
089100011102     C                   Z-ADD     TASIAS        WIMI             30 9
089200011107     C                   MOVEL     TASVAS        WDVI              3
089300011102     C                   EXSR      CNVEUR
089400011102     C                   Z-ADD     WIMO          TASIAS
089500011102     C*
089600011102     C* QUANTITA' DA FATTURARE (non viene convertita x ambiguità su suo utilizzo)
089700011102     C*
089800011102     C* TRASPORTO
089900011102     C                   Z-ADD     TASPOR        WIMI             30 9
090000011102     C                   MOVEL     TASDIV        WDVI              3
090100011102     C                   EXSR      CNVEUR
090200011102     C                   Z-ADD     WIMO          TASPOR
090300011102     C*
090400011102     C* TOTALE IMPONIBILE
090500011102     C                   Z-ADD     TASIMV        WIMI             30 9
090600011102     C                   MOVEL     TASDIV        WDVI              3
090700011102     C                   EXSR      CNVEUR
090800011102     C                   Z-ADD     WIMO          TASIMV
090900011102     C*
091000011102     C* IMPORTO ASS. IN MONETA O MAX VALORE ...
091100011102     C                   Z-ADD     TASIAL        WIMI             30 9
091200011123     C                   MOVEL     TASDIV        WDVI              3
091300011102     C                   EXSR      CNVEUR
091400011102     C                   Z-ADD     WIMO          TASIAL
091500011102     C*
091600011102     C* VARIE VARIE
091700011102     C                   Z-ADD     TASVA1        WIMI             30 9
091800011102     C                   MOVEL     TASDIV        WDVI              3
091900011102     C                   EXSR      CNVEUR
092000011102     C                   Z-ADD     WIMO          TASVA1
092100011102     C                   Z-ADD     TASVA2        WIMI             30 9
092200011102     C                   MOVEL     TASDIV        WDVI              3
092300011102     C                   EXSR      CNVEUR
092400011102     C                   Z-ADD     WIMO          TASVA2
092500011102     C                   Z-ADD     TASVA3        WIMI             30 9
092600011102     C                   MOVEL     TASDIV        WDVI              3
092700011102     C                   EXSR      CNVEUR
092800011102     C                   Z-ADD     WIMO          TASVA3
092900011102     C                   Z-ADD     TASVA4        WIMI             30 9
093000011102     C                   MOVEL     TASDIV        WDVI              3
093100011102     C                   EXSR      CNVEUR
093200011102     C                   Z-ADD     WIMO          TASVA4
093300011102     C                   Z-ADD     TASVA5        WIMI             30 9
093400011102     C                   MOVEL     TASDIV        WDVI              3
093500011102     C                   EXSR      CNVEUR
093600011102     C                   Z-ADD     WIMO          TASVA5
093700011102     C                   Z-ADD     TASVA6        WIMI             30 9
093800011102     C                   MOVEL     TASDIV        WDVI              3
093900011102     C                   EXSR      CNVEUR
094000011102     C                   Z-ADD     WIMO          TASVA6
094100011102     C                   Z-ADD     TASVA7        WIMI             30 9
094200011102     C                   MOVEL     TASDIV        WDVI              3
094300011102     C                   EXSR      CNVEUR
094400011102     C                   Z-ADD     WIMO          TASVA7
094500011102     C                   Z-ADD     TASVA8        WIMI             30 9
094600011102     C                   MOVEL     TASDIV        WDVI              3
094700011102     C                   EXSR      CNVEUR
094800011102     C                   Z-ADD     WIMO          TASVA8
094900011102     C                   Z-ADD     TASVA9        WIMI             30 9
095000011102     C                   MOVEL     TASDIV        WDVI              3
095100011102     C                   EXSR      CNVEUR
095200011102     C                   Z-ADD     WIMO          TASVA9
095300011102     C                   Z-ADD     TASVA0        WIMI             30 9
095400011102     C                   MOVEL     TASDIV        WDVI              3
095500011102     C                   EXSR      CNVEUR
095600011102     C                   Z-ADD     WIMO          TASVA0
095700011102     C                   Z-ADD     TASV11        WIMI             30 9
095800011102     C                   MOVEL     TASDIV        WDVI              3
095900011102     C                   EXSR      CNVEUR
096000011102     C                   Z-ADD     WIMO          TASV11
096100011102     C                   Z-ADD     TASV12        WIMI             30 9
096200011102     C                   MOVEL     TASDIV        WDVI              3
096300011102     C                   EXSR      CNVEUR
096400011102     C                   Z-ADD     WIMO          TASV12
096500011102     C                   Z-ADD     TASV13        WIMI             30 9
096600011102     C                   MOVEL     TASDIV        WDVI              3
096700011102     C                   EXSR      CNVEUR
096800011102     C                   Z-ADD     WIMO          TASV13
096900011102     C                   Z-ADD     TASV14        WIMI             30 9
097000011102     C                   MOVEL     TASDIV        WDVI              3
097100011102     C                   EXSR      CNVEUR
097200011102     C                   Z-ADD     WIMO          TASV14
097300011102     C                   Z-ADD     TASV15        WIMI             30 9
097400011102     C                   MOVEL     TASDIV        WDVI              3
097500011102     C                   EXSR      CNVEUR
097600011102     C                   Z-ADD     WIMO          TASV15
097700011102     C                   Z-ADD     TASV16        WIMI             30 9
097800011102     C                   MOVEL     TASDIV        WDVI              3
097900011102     C                   EXSR      CNVEUR
098000011102     C                   Z-ADD     WIMO          TASV16
098100011102     C                   Z-ADD     TASV17        WIMI             30 9
098200011102     C                   MOVEL     TASDIV        WDVI              3
098300011102     C                   EXSR      CNVEUR
098400011102     C                   Z-ADD     WIMO          TASV17
098500011102     C                   Z-ADD     TASV18        WIMI             30 9
098600011102     C                   MOVEL     TASDIV        WDVI              3
098700011102     C                   EXSR      CNVEUR
098800011102     C                   Z-ADD     WIMO          TASV18
098900011102     C                   Z-ADD     TASV19        WIMI             30 9
099000011102     C                   MOVEL     TASDIV        WDVI              3
099100011102     C                   EXSR      CNVEUR
099200011102     C                   Z-ADD     WIMO          TASV19
099300011102     C                   Z-ADD     TASV20        WIMI             30 9
099400011102     C                   MOVEL     TASDIV        WDVI              3
099500011102     C                   EXSR      CNVEUR
099600011102     C                   Z-ADD     WIMO          TASV20
099700011102     C*
099800011102     C                   ENDSR
099900011012     C*------------------------------------------------------------------------*
100000011012     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
100100011012     C*------------------------------------------------------------------------*
100200011012     C     CNVEUR        BEGSR
100300011112     C*
100400011112     C* AZZERA VARIABILI DI LAVORO
100500011112     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
100600011112     C                   MOVEL     '9'           NDEC              1            *N. DEC IN OUTPUT
100700011112     C*
100800011112     C* DI DEFUALT ATTRIBUISCO LO STESSO VALORE AGLI IMPORTI "DA CONVERTIRE" E "CONVERTITI"
100900011112     C                   Z-ADD     WIMI          WIMO
101000011107     C*
101100011107     C* COME PRIMA COSA AI FINI DELLA CONVERSIONE TESTO SE LE DIVISE SONO DIVERSE
101200011107     C     WDVI          IFNE      WDIVCE
101300011012     C*
101400011012IF  1C     WIMI          IFNE      *ZEROS
101500011012     C                   CLEAR                   YEUR
101600011012     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
101700011012     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
101800011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
101900011012     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
102000011012     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
102100011012     C                   CALL      'YEURCO'
102200011012     C                   PARM                    YEUR
102300011012IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
102400011012     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
102500011012X   2C                   ELSE                                                   *SI ERRORI
102600011012     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
102700011012E   2C                   ENDIF
102800011012E   1C                   ENDIF
102900011012     C*
103000011107     C                   ENDIF
103100011107     C*
103200011012     C                   ENDSR
103300970424     C********************************************************************
103400970424     C*   RUTINZ - ROUTINE INIZIALE
103500970424     C********************************************************************
103600970424     C     RUTINZ        BEGSR
103700970424     C*
103800970424     C* PARAMETRI IN INPUT ALLA ROUTINE
103900970424     C     *ENTRY        PLIST
104000970424     C                   PARM                    DS50
104100130228     C                   PARM                    parddt
104200130507     C                   PARM                    partest
104300130228
104400130228      /free
104500130507         wNoWf = *off;
104600130228       //?Se non passato il paramtro della data decorrenza tariffa/offerta
104700130228       //?pulisco il campo
104800130228         IF  %parms = 1;
104900130228           clear dataddt;
105000130228         ELSE;
105100130228           dataddt = %int(parddt);
105200130228         ENDIF;
105300130507       //?Se passato il terzo parametro non devo aprire il file di work
105400130507         IF  %parms > 2;
105500130507           wNoWf = *on;
105600130507         ENDIF;
105700130507       //?apro il file di work
105800130509         IF  not wNoWf and not %open(WFDBF10F);
105900130507           open WFDBF10F;
106000130507         ENDIF;
106100130228      /end-free
106200970424     C*
106300060809     C* CHIAVE LETTURA titas30c - COMPLETA
106400060809     C     KEYtas        KLIST
106500060809     C                   KFLD                    tasAAS
106600060809     C                   KFLD                    tasLNP
106700060809     C                   KFLD                    tasNRS
106800060809     C                   KFLD                    tasNSP
106900060809      *  KLIST PER ACCESSO FILES
107000060809     c     KTA7          KLIST
107100060809     c                   KFLD                    TASAAS
107200060809     c                   KFLD                    TASLNP
107300060809     c                   KFLD                    TASNRS
107400060809     c                   KFLD                    TASNSP
107500060809     c                   KFLD                    TASTBL
107600030122     C*
107700060809     C* CHIAVE LETTURA FIAR531c - COMPLETA
107800030122     C     KEYAR5        KLIST
107900030122     C                   KFLD                    AR5AAS
108000030122     C                   KFLD                    AR5LNP
108100030122     C                   KFLD                    AR5NRS
108200030122     C                   KFLD                    AR5NSP
108300030122     C                   KFLD                    AR5TRD
108400060809     C                   KFLD                    AR5prg
108500970424     C*
108600060809     C* CHIAVE LETTURA tncsb03l - COMPLETA
108700060809     C     KEYcsb        KLIST
108800060809     C                   KFLD                    csbAAS
108900060809     C                   KFLD                    csbLNP
109000060809     C                   KFLD                    csbNRS
109100060809     C                   KFLD                    csbNSP
109200970424     C*
109300970424     C* CHIAVE LETTURA TABEL00F - COMPLETA
109400970424     C     KEYTAB        KLIST
109500970424     C                   KFLD                    KTBUTE
109600970424     C                   KFLD                    KTBCOD
109700970424     C                   KFLD                    KTBKEY
109800011012     C*
109900011012     C* LETTURA TNTBE01L - PARZIALE
110000011012     C     KEYTBE        KLIST
110100011012     C                   KFLD                    KTECOD                         *UTENTE
110200011012     C                   KFLD                    KTEKE1                         *CODICE
110300080908      * Chiave file eventi FNEVB
110400080908     c     Kevb4         Klist
110500080908     c                   Kfld                    Tasaas
110600080908     c                   Kfld                    Taslnp
110700080908     c                   Kfld                    Tasnrs
110800080908     c                   Kfld                    Tasnsp
110900080908     c                   Kfld                    Wcev
111000970424     C* DEFINISCO CAMPI CHIAVE
111100011012     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
111200970424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
111300970424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
111400011012     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
111500011012     C     *LIKE         DEFINE    TBEKE1        KTEKE1
111600970424     C*
111700970424     C* RECUPERO IL CODICE DELL'AZIENDA
111800970424     C                   Z-ADD     1             CODUT             1 0
111900970424     C                   MOVEL     '0'           $ERR              1
112000970424     C                   MOVEL     *BLANKS       $MSG             78
112100011015     C*
112200011015     C* REPERISCE LA DATA CORRENTE
112300011015     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
112400011015     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
112500011015     C                   Z-ADD     WN8           G08DAT
112600011015     C                   Z-ADD     *ZEROS        G08INV
112700011015     C                   MOVEL     '0'           G08ERR
112800011015     C                   CALL      'XSRDA8'
112900011015     C                   PARM                    WLBDA8
113000011015     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
113100011012     C*
113200011012     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
113300011012     C                   EXSR      CARGEDCE
113400970424     C*
113500970424     C* AZZERO I CAMPI IN OUTPUT DEL PGM
113600970424     C                   EXSR      AZZERA
113700970424     C*
113800970424     C                   ENDSR
113900970424     C***********************************************************************
114000970424     C*   AZZERA - AZZERA O ABBLENCA(?) TUTTI I CAMPI IN OUTPUT DELLA ROUTINE
114100970424     C***********************************************************************
114200970424     C     AZZERA        BEGSR
114300970424     C*
114400970424     C                   MOVEL     *BLANKS       D50ERR
114500970424     C                   MOVEL     *BLANKS       D50MSG
114600970424     C                   Z-ADD     *ZEROS        D50RIC
114700970424     C                   Z-ADD     *ZEROS        D50COM
114800970424     C                   Z-ADD     *ZEROS        D50DEL
114900970424     C*
115000970424     C                   ENDSR
115100060810      /COPY GAITRASRC/SRCTREC,TRECDT
115200970424     C********************************************************************
115300970424** ======== SCHIERA: MSG  (MESSAGGI)                       ================
115400970424TNTE50R- Bolla partenza non trovata o annullata.                          BPNT 1
115500970424TNTE50R- Valuta non trovata nella tab. CV (cambi valuta).                 VNTT 2
115600970424TNTE50R- CAP mittente non trovato.                                        CAPN 3
115700970424TNTE50R- Bolla non calcolabile.                                           NONC 4
115800970424TNTE50R- Tipo bolla non trovato in tabella 3A. Avvertire CED.             3ANT 5
115900970424TNTE50R- 6                                                                ERRO 6
116000970424TNTE50R- 7                                                                ERRO 7
116100970424TNTE50R- 8                                                                ERRO 8
116200970424TNTE50R- 9                                                                ERRO 9
116300970424TNTE50R- 10                                                               ERRO10
116400970424TNTE50R- 11                                                               ERRO11
116500970424TNTE50R- 12                                                               ERRO12
116600970424TNTE50R- 13                                                               ERRO13
116700970424TNTE50R- 14                                                               ERRO14
116800970424TNTE50R- 15                                                               ERRO15
116900970424TNTE50R- 16                                                               ERRO16
117000970424TNTE50R- 17                                                               ERRO17
117100970424TNTE50R- 18                                                               ERRO18
117200970424TNTE50R- 19                                                               ERRO19
117300970424TNTE50R- 20                                                               ERRO20
