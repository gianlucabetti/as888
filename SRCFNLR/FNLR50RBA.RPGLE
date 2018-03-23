000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200941107      * FNLR50R *----------------------------------------------------*
000300930428      *---------*                                                    *
000400911016      *     - GESTIONE PREPARAZIONE DISTINTA USCITA CONSEGNA -       *
000500960702     F*--------------------------------------------------------------*
000600941107     FFNLR50D   CF   E             WORKSTN
000700941107     F                                     SFILE(LR50S03:NRR)
000800911016     FTABEL00F  IF   E           K DISK
000900990920     FTNTBE01L  IF   E           K DISK
001000021204     Ffiapd01L  IF   E           K DISK
001100080326     Ffidsf02L  IF   E           K DISK
001200110224     FFipct02L  IF   E           K DISK
001300020905     FFNARB70L  IF   E           K DISK
001400020905     F                                     RENAME(FNARB000:FNARB70)
001500941107     FFNARB01L  UF   E           K DISK
001600070410     FFiarg01L  UF a E           K DISK
001700990920     FFIAR601L  IF   E           K DISK
001800051115     FFIAR901L  IF   E           K DISK
001900990920     FFIARI01L  IF   E           K DISK
002000941107     FFNARBW1L  UF A E           K DISK
002100941107     FFNARBW2L  UF   E           K DISK
002200941107     F                                     RENAME(FNARBW00:FNARBW2)
002300020905     FFNFVV01L  UF A E           K DISK
002400941107     FFNLBL01L  IF   E           K DISK
002500941107     FFNLBL02L  IF   E           K DISK
002600941107     F                                     RENAME(FNLBL000:FNLBL02)
002700990519     FFNANM01L  IF   E           K DISK
002800990519     F                                     INFDS(FNANM1)
002900941107     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003000011121     Ffiftt02L  IF   E           K DISK
003100011121     Ffifst02L  IF   E           K DISK
003200011121     F                                     RENAME(fiftt000:fifst000)
003300950607     FAZCLN01L  IF   E           K DISK
003400990803     FFNEVB04L  UF A E           K DISK
003500060214     FFiAR401L  IF   E           K DISK
003600070118     FFNBRV01L  IF   E           K DISK
003700010403     FFNORM06L  IF   E           K DISK
003800031020     Ffnart01l  IF   E           K DISK
003900040217     FFiar501L  IF   E           K DISK
004000041007     FFiqdt01L  uf a E           K DISK
004100070410     FFidst09L  uf a E           K DISK    prefix(£)
004200070312     FFidst31L  uf a E           K DISK
004300080121     FFidst51L  uf a E           K DISK
004400070312     Ffiapd22L  IF   E           K DISK
004500070312     Ffiapd31L  IF   E           K DISK
004600080121     Ffiapd51L  IF   E           K DISK
004700940321      *---------------------------------------------------------------
004800950613     D L£6             S              3  0 DIM(50)                              LINEE £6
004900950613     D LIN             S              3  0 DIM(29)                              LINEE ARRIVI
005000911022     D UNI             S              3    DIM(29)                              LINEE DA TABEL
005100911021     D LMN             S              3  0 DIM(29)                              FILIALI
005200160502     D ERR             S             70    DIM(28) CTDATA PERRCD(1)             ERRORI A VIDEO
005300100203     D s7r             S              2    DIM(100)                             CODICE TABELLA 3A
005400100514     d s7rc            S              2    DIM(100)                             CODICE TABELLA 3A
005500100203     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
005600941109     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
005700941108     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
005800941108     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
005900950606     D MES             S              1    DIM(31)
006000940321      *---------------------------------------------------------------
006100940321      *  Schiere per gestione caricamento Stop
006200940321      *---------------------------------------------------------------
006300070312     d* schiere file fiapd20f
006400070312     D Ptc1            S               *
006500070312     D                                     INZ(%ADDR(apd2tc1))
006600070312     D tc1             S                   LIKE(apd2tc1)
006700070312     D                                     DIM(5)
006800070312     D                                     BASED(Ptc1)
006900070312     D Pts1            S               *
007000070312     D                                     INZ(%ADDR(apd2ts1))
007100070312     D ts1             S                   LIKE(apd2ts1)
007200070312     D                                     DIM(2)
007300070312     D                                     BASED(Pts1)
007400070312     d* schiere file fidst00f
007500070312     D Ptc1d           S               *
007600070312     D                                     INZ(%ADDR(£dsttc1))
007700070312     D tc1d            S                   LIKE(£dsttc1)
007800070312     D                                     DIM(5)
007900070312     D                                     BASED(Ptc1d)
008000070312     D Pts1d           S               *
008100070312     D                                     INZ(%ADDR(£dstts1))
008200070312     D ts1d            S                   LIKE(£dstts1)
008300070312     D                                     DIM(2)
008400070312     D                                     BASED(Pts1d)
008500070312     d*
008600961217     D STP             S              3  0 DIM(999)                             1° schiera Stop
008700961217     D STP2            S              3  0 DIM(999)                             2° schiera Stop
008800961217     D RSD2            S             70    DIM(999)                             2° schiera Rag.soc.
008900080410     c* ds file
009000160316     D dnsd          E DS
009001160316     D FVVDS         E DS                  EXTNAME(FNFVV00F)
009100070312     D fidstds       E DS                  INZ extname(fidst00f) prefix(£)
009200070312     D fidstds2      E DS                  INZ extname(fidst00f) prefix($)
009300070312     D fiapd2ds      E DS                  extname(fiapd20f)
009400070410     D fiargds       E DS                  extname(fiarg00f)
009500080410     D fnarbds       E DS                  extname(fnarb00f)
009600100203     D dARGFLO       E DS
009700080410      * DS per parcel/messaggerie su FNFVV
009800050523     D dfvvspn       E DS
009900050523      * DS per parcel/messaggerie su FIAPD
010000050523     D dapdflr       E DS
010100080208      * DS filler  FIdst
010200080208     D ddstflr       E DS
010300080226      * DS per richiamo pgm fnlrd1r x cambio aut. se pda orm partito
010400080226     D fnlrd1ds      E DS
010500080226      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
010600080226     D fnlr99ds      E DS
010700031125      * DS per richiamo pgm fnlr98r x richiamo pgm per togliere bolla da distinta
010800031125     D fnlr98ds      E DS
010900141112     D fnlr98ds2     E DS
011000030630      * DS per richiamo pgm FNLR50R1
011100030630     D FNLR50        E DS                  EXTNAME(FNLR50DS)
011200080410      * DS per richiamo pgm FNLR6R controllo numeratore distinta
011300020906     D FNLR6ds       E DS
011400130424      * DS per richiamo pgm TNSD99R calcolo affidabilità
011500130424     D tnsd99ds      E DS
011600080410      * DS per richiamo pgm FNLR36R interrogazione bolle
011700941108     D DSLR36          DS
011800941108     D  P36AAS                14     17  0
011900941108     D  P36LNP                18     20  0
012000941108     D  P36NRS                21     22  0
012100941108     D  P36NSP                23     29  0
012200941108     D  P36F03                30     30
012300941108     D  P36FLG                31     31
012400941109      * DS per richiamo pgm FNLR55R
012500941109     D FNLR55        E DS                  EXTNAME(FNLR55DS)
012600941110      * DS per richiamo pgm FNLR02R
012700941109     D DSLR01        E DS
012800021204      * DS per richiamo pgm FNLV24R
012900021204     D fnlv24ds      E DS
013000941110      * DS per richiamo pgm FNLR39R
013100941110     D DSLR02        E DS
013200041008      * DS  pgm fnlrf8r x gestione bancali affidati rientrati e colli ritirati
013300041008     D fnlrf8ds      E DS
013400080410      * DS  pgm fidg37r x aggiornare i valori della distinta                ti
013500070517     D fidg37ds      E DS
013600080410      * DS  pgm fidg09r x controllare giro tellus                           ti
013700070725     D fidg09ds      E DS
013800080410      * DS  pgm fidg44r x forzatura distinta con fase CUS                   ti
013900080410     D fidg44ds      E DS
014000080410      * DS  pgm fidg42r x aggiornare i dati a PDA x spedizione              ti
014100080410     D fidg42ds      E DS
014200141029     D fnlry09ds2    E DS
014300080410     D* ds tabelle
014400990921     D YEURDS        E DS                  EXTNAME(YEURCODS)
014500990920     D DGED          E DS
014600000929     D DDGD          E DS
014601160502     D DS03          E DS
014700990920     D DSCV          E DS
014800911106     D DS3A          E DS
014900941108     D DS1P          E DS
015000100203     D DS7r          E DS
015100050520     D og150         E DS
015200070309     D OG146         E DS                  INZ
015300080208     D OG148         E DS                  INZ
015400080121     D OG147         E DS                  INZ
015500911016     D KPJBA         E DS
015600080410      * ds interne pgm
015700000620     D OG143         E DS
015800020905     c* numeratori
015900020905     d trul33ds      e ds
016000020905     c*
016100131031     d dEVB01        e ds
016200131031     c*
016300020509     d trul06ds      e ds
016400020509     d  lint                   1     90  0 dim(30)
016500911016      *
016600941107     D WLBDA8          DS
016700941107     D  G02DAT                 1      8  0
016800941107     D  G02INV                 9     16  0
016900941107     D  G02ERR                17     17
017000941107     D  G02TGI                18     22  0
017100911017      *
017200020419     d Wdata8          DS
017300020419     d  dadata                 1      8  0
017400020419     d  adata                  9     16  0
017500020419     d  GioLav                17     21  0
017600020419      *
017700920306     D DATPAR          DS
017800941107     D  GIODAT                 1      8  0
017900941107     D  GIOINV                 9     16  0
018000941107     D  GIOTGI                17     21  0
018100950123     D                 DS
018200950123     D  V1CFPD                 1      3  0
018300950123     D  V1CCPC                 4      7
018400950123     D  WCDCPC                 1      7
018500960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
018600990519     D FNANM1          DS
018700960415     D  ANMNR1               397    400B 0
018800020905     D ddatiute      e ds
018900020905     D azuteds       e ds                  extname(AZUTE00F)
019000020905     D tibs34ds      E DS                  inz
019100000515     D DSBL4A        E DS
019200090211     D*DFAR          E DS
019300090211     D* §FARDS       E                     EXTFLD(D§FARDES)
019400090211     D* §FARFB       E                     EXTFLD(D§FARFFB)
019500090211     D* §FARNF       E                     EXTFLD(D§FARNONF)
019600090211     D* §FARMN       E                     EXTFLD(D§FARMAN)
019700090211     D* §FARST       E                     EXTFLD(D§FARSTP)
019800090211     D* §FARAF       E                     EXTFLD(D§FARAFVV)
019900090211     D* §FARCF       E                     EXTFLD(D§FARCFVV)
020000030618     c* ds per sottocampare il barcode della spedizione
020100030618     D ds_pis          DS
020200030618     D  ds_pisaas                     2  0
020300030618     D  ds_pislnp                     3  0
020400030618     D  ds_pisnrs                     2  0
020500030618     D  ds_pisnsp                     7  0
020600030623     D  ds_chkdgt                     1  0
020700030623     D trul28ds      E DS
020800040217     D Dar5gen       E DS
020900150929     D Dar5emd       E DS
021000030623     d savchk          s                   like(o28ckd)
021100030623     d barcode         s             14
021200070312     d wnfv            s                   like(£dstnfv)
021300070612     d wpes            s                   like(arbpkf)
021400070612     d wvol            s                   like(arbvlf)
021500100203     d DTAISO          s               D
021600940321      *---------------------------------------------------------------
021700941109     IAZORG01L  AA
021800941109     I                             P    4    5 0ORGFIL
021900971119     I                                 11   11  ORGFL1
022000000620     I                               3776 3800  ORGDE3
022100070309     I                               3851 3875  ORGDe6
022200080121     I                               3876 3900  ORGDe7
022300080208     I                               3901 3925  ORGDe8
022400070309     I                               3951 3975  ORGDF0
022500911016      *---------------------------------------------------------------*
022600911016      * FLUSSO PRINCIPALE                                             *
022700911016      *---------------------------------------------------------------*
022800941107      *  Loop Principale
022900941107     C     *INKC         DOWEQ     '0'
023000941107     C                   EXSR      GESD01
023100941107     C                   END                                                    *do hival
023200911016     C*
023300911016     C*                  FINE LAVORO
023400911018     C     FINE          TAG
023500960415     C* RICHIAMO PGM PER CHIUSURA FILES
023600960415     C     W33R          IFEQ      '1'
023700960415     C                   MOVEL     'C'           D33TLA
023800960415     C                   CALL      'FNLR33R'
023900960415     C                   PARM                    DSLR33
024000960415     C                   END
024100911128      *
024200911018     C                   SETON                                        LR
024300941107     C*----------------------------------------------------*
024400941107     C* Gestione Primo formato                             *
024500941107     C*----------------------------------------------------*
024600941107     C     GESD01        BEGSR
024700941107     C*
024800941107     C                   Z-ADD     0             NUM
024900040217     C                   SETOFF                                       464267
025000100203     C                   SETOFF                                       48
025100030701     c* v1cisp mi servono per sapere se voglio inserire  le
025200080605     c* spedizioni a mano o con la pistola. ora forzo P perchè oramai
025300080605     c* tutti i po stampano le bolle su laser.
025400030701     c                   movel     'P'           v1cisp
025500030618     C*
025600950215     C                   EXFMT     LR50D01
025700941107     C                   MOVEL     *BLANKS       $MSG
025800941107     C                   SETOFF                                       901113
025900950123     C                   SETOFF                                       171219
026000040217     C                   SETOFF                                       464767
026100080208     C                   SETOFF                                       751069
026200941107     C                   Z-ADD     0             ULTSTP
026300941107     C                   Z-ADD     0             W                 3 0
026400941107     C                   Z-ADD     0             STP2
026500951121     C                   MOVEA     *BLANKS       RSD2
026600941107?    C* F3 - Fine lavoro
026700941107     C     *INKC         IFEQ      '1'
026800941107     C                   GOTO      FINVD1
026900941107     C                   END
027000950907?    C* Controllo se pgm non e richiamato da gestione fogli consegna
027100950907     C     D50TPR        IFEQ      'C'
027200950907     C                   EXSR      PGMRIC
027300950907     C                   ELSE
027400950907     C                   EXSR      PGMMNU
027500950907     C                   END
027600941107     C*
027700941107     C     FINVD1        ENDSR
027800950907     C*----------------------------------------------------*
027900950907     C* Pgm. richiamato da menù                            *
028000950907     C*----------------------------------------------------*
028100950907     C     PGMMNU        BEGSR
028200950907     C*
028300030804?    C* F2 - toglie spedizioni da distinta
028400030804     C     *INKB         IFEQ      '1'
028500110125     c                   clear                   fnlr98ds
028600110125     c                   movel(p)  fnlr98ds      kpjbu
028700030804     c                   call      'FNLR98C'
028800030804     c                   parm                    kpjba
028900020905     C                   MOVE      *ZEROS        SAVNDC
029000950907     C                   GOTO      FINMNU
029100950907     C                   END
029200140924?    C* F19- Richiama pgm Disposizioni via WEB
029300140924     C     *inkt         IFEQ      '1'
029400141029     c                   clear                   fnlry09ds2
029500140924     c                   eval      ilry09fgs = v1clna
029600140924     c                   eval      ilry09tch = 'E'
029700141013     c                   exsr      verdispo
029800141029     c                   if        OLRY09ESAL = '1'
029900140926     c                   seton                                        97
030000141001     c                   else
030100141001     c                   setoff                                       97
030200140926     c                   endif
030300140930     C                   GOTO      FINMNU
030400140924     c                   endif
030500030804?    C* F18 - Cambio Filiale
030600030804     C     *INKS         IFEQ      '1'
030700030804     C*??
030800030804     C                   MOVE      *ZEROS        SAVNDC
030900030804     C                   SETOFF                                       20
031000030804     C                   GOTO      FINMNU
031100030804     C                   END
031200950907     C*
031300950907     C     *INKA         IFEQ      '1'
031400080411     C                   MOVEL     £dstpdr       SAVCPCorm         7
031500080226     C                   MOVEL     WCDCPC        SAVCPC            7
031600950907     C                   SETON                                        03
031700950907     C                   END
031800020906     C* controlli po in gestione
031900950907     C                   EXSR      CONTR1
032000020905     c   90              goto      finmnu
032100020906     C* Controllo il numeratore 12 che non sia bloccato
032200020906     C                   CLEAR                   TRUL33DS
032300020906     c                   eval      i33tla = 'L'
032400020906     C                   Z-ADD     0             I33AAA
032500020906     C                   Z-ADD     1             I33ope
032600020906     C                   Z-ADD     12            I33CNU
032700020906     C                   Z-ADD     v1clna        I33PO1
032800020906     C                   Z-ADD     1             I33NUM
032900020906     c                   movel(p)  trul33ds      kpjbu
033000020906     c                   call      'TRUL33R'
033100020906     c                   parm                    kpjba
033200020906     c                   movel     kpjbu         trul33ds
033300020906     c* record numeratore allocato
033400020906     C                   IF        O33ERR = 02
033500020906     C                   SETON                                        9013
033600020906     C                   MOVEL     ERR(12)       $MSG
033700020906     C                   GOTO      FINMNU
033800020906     C                   END
033900020905?    C* F11 - Sblocco Distinte in preparazione
034000020905     C     *INKK         IFEQ      '1'
034100020905     C                   CLEAR                   FNLR50
034200020905     C                   Z-ADD     V1CLNA        D50FIL
034300020906     C                   TESTN                   V1CNDC               3131
034400020905     C   31              MOVE      V1CNDC        D50NDT
034500020905     C                   MOVEL     FNLR50        KPJBU
034600020905     C                   CALL      'FNLR50R1'
034700020905     C                   PARM                    KPJBA
034800020905     C                   END
034900020906     C* controlli successivi
035000020906     C                   EXSR      CONTR2
035100020906     c   90              goto      finmnu
035200950907     C*
035300950907     C* Controllo se digitato il numero distinta vado in modifica
035400020905     C     V01NDC        IFNE      *ZEROS
035500070312     C* alloco il file delle distinte FIDST00F
035600070312     c                   exsr      srlckdst
035700070312     c   90              goto      finmnu
035800950907     C* carico il file di lavoro con le sped.riguardanti la distinta
035900950907     C     V01NDC        IFEQ      SAVNDC
036000950907     C*   Alloco la distinta
036100020905     C     KFVV          CHAIN     FNFVV01L                           12
036200030926     c                   if        not *in12 and fvvatb<>'*'
036300030926     c                   if        fvvfcf <> ' '
036400030926     C                   SETON                                        9012
036500030926     C                   MOVEL     ERR(14)       $MSG
036600030926     c                   unlock    fnfvv01l
036700030926     c   90              goto      finmnu
036800030926     c                   end
036900030926     C                   MOVEL     'A'           FVVFCF
037000030926     C                   UPDATE    FNFVV000
037100950907     C                   EXSR      CARDIS
037200950907     C                   EXSR      GESSBF
037300950907     C                   MOVE      *ZEROS        SAVNDC
037400950907     C                   Z-ADD     V01NDC        FVVNFV
037500030926     c                   end
037600950907     C                   ELSE
037700950907     C                   Z-ADD     V01NDC        SAVNDC            7 0
037800950907     C                   END
037900950907     C*
038000950907     C                   ELSE
038100950907     C     TAG01         TAG
038200950907     C                   SETOFF                                       93
038300950907     C*
038400950907     C* Compilazione distinta con inserimento singola spedizione
038500950907     C                   MOVE      *ZEROS        SAVAAS
038600950907     C                   MOVE      *ZEROS        SAVLNP
038700950907     C                   MOVE      *ZEROS        SAVNRS
038800950907     C                   MOVE      *ZEROS        SAVNSP
038900950907     C  N90              EXSR      GESFM3
039000950907     C                   MOVE      *ZEROS        SAVNDC
039100950907     C                   END                                                    *if modifica
039200950907     C* Se nelle videate precedenti hanno richiesto una videata
039300950907     C* particolare ritorno al controllo del tipo compilazione
039400950907     C   93              GOTO      TAG01
039500950907     C*
039600950907     C     FINMNU        ENDSR
039700080410     C*----------------------------------------------------*
039800080410     C* se pda e cambio aut. richiamo FNLRD1               *
039900080410     C*----------------------------------------------------*
040000080410     C     srchgcpc      BEGSR
040100080410     C* richiamo pgm per cambiare aut sui record FIPDO00T
040200080801     c                   if        savcpcorm <> wcdcpc
040300140212     c                             and savcpcorm <> ' '
040400080226     c                   clear                   fnlrd1ds
040500080226     c                   eval      LRD1FGS = v1clna
040600080226     c                   eval      LRD1NDC = v01ndc
040700080226     c                   movel     savcpcorm     LRD1PDRO
040800080226     c                   movel     wcdcpc        LRD1PDRN
040900080226     c                   call      'FNLRD1R'
041000080226     c                   parm                    fnlrd1ds
041100140212     c                   clear                   savcpcorm
041200080226     c                   end
041300080410     C                   ENDSR
041400080226     C*----------------------------------------------------*
041500080226     C* Pgm. richiamato da gestione fogli consegna         *
041600080226     C*----------------------------------------------------*
041700080226     C     PGMRIC        BEGSR
041800070315     C                   Z-ADD     4             FVVNPG
041900070315     C                   Z-ADD     D50NDT        FVVNFV
042000070315     C                   Z-ADD     D50FIL        FVVFGS
042100070312     C* alloco il file delle distinte FIDST00F
042200070312     c                   exsr      srlckdst
042300070312     c   90              goto      finric
042400950907     C*   Alloco la distinta
042500020905     C     KFVV          CHAIN     FNFVV01L                           12
042600030926     c                   if        not *in12 and fvvatb<>'*'
042700030926     c                   if        fvvfcf <> ' '
042800030926     C                   SETON                                        9012
042900030926     C                   MOVEL     ERR(14)       $MSG
043000030926     c                   unlock    fnfvv01l
043100030926     c   90              goto      finric
043200030926     c                   end
043300950907     C                   MOVE      FVVDFV        ROVDDC
043400950907     C                   MOVEL     'A'           FVVFCF
043500950907     C                   UPDATE    FNFVV000
043600950907     C*   Reperisco decodifica padroncino
043700950907     C                   MOVE      WCDCPC        APDPDR
043800021204     C     kapd          CHAIN     fiapd01L                           13
043900950907     C                   MOVE      APDRSC        V05PAD
044000950907     C                   MOVE      APDRSC        V06PAD
044100950908     C                   MOVE      WCDCPC        CODPAD            7 0
044200950907     C*------------------------------------------------------*
044300950907     C*  Controllo sul calendario il tipo di giorno che è la
044400950907     C*  data di consegna
044500950907     C     ROVDDC        IFNE      0
044600950907     C                   Z-ADD     ROVDDC        WMMGG             4 0
044700950907     C                   Z-ADD     ROVDDC        WG                2 0
044800950907     C                   MOVEL     ROVDDC        KANN
044900950907     C                   Z-ADD     0             KTFP
045000950907     C                   Z-ADD     0             KTFA
045100950907     C                   MOVEL     WMMGG         KMES
045200950907     C     KCLN          CHAIN     AZCLN01L                           31
045300950907     C                   MOVEA     CLNMAT        MES
045400950907     C                   MOVE      MES(WG)       WGG               1
045500950907     C                   END
045600950907     C*------------------------------------------------------*
045700950907     C                   EXSR      CARDIS
045800950907     C* Compilazione distinta con inserimento singola spedizione
045900950907     C     TAG02         TAG
046000950907     C                   SETOFF                                       93
046100950907     C                   MOVE      *ZEROS        SAVAAS
046200950907     C                   MOVE      *ZEROS        SAVLNP
046300950907     C                   MOVE      *ZEROS        SAVNRS
046400950907     C                   MOVE      *ZEROS        SAVNSP
046500950907     C  N90              EXSR      GESFM3
046600950907     C                   MOVE      *ZEROS        SAVNDC
046700950907     C* Se nelle videate precedenti hanno richiesto una videata
046800950907     C* particolare ritorno al controllo del tipo compilazione
046900950907     C   93              GOTO      TAG02
047000950907     C                   END
047100950907     C*
047200030926     C     finric        ENDSR
047300070312     C*----------------------------------------------------*
047400070312     C* alloco il record della distinta sul file FIDST     *
047500070312     C*----------------------------------------------------*
047600070312     C     srlckdst      BEGSR
047700070312     c                   eval      wnfv = fvvnfv
047800070410     C     KDST2         CHAIN(e)  fidst09l
047900070312     c                   if        %error
048000070312     C                   SETON                                        9012
048100070312     C                   MOVEL     ERR(14)       $MSG
048200070312     c                   end
048300070612     c                   if        %found(fidst09l)
048400070830     c                   z-add     £DSTPKL       V02PTO
048500070830     c                   z-add     £DSTVLU       V02CTO
048600070612     c                   else
048700070612     c                   clear                   v02pto
048800070612     c                   clear                   v02cto
048900070612     c                   end
049000070312     C                   ENDSR
049100020906     C*----------------------------------------------------*
049200020906     C* CONTROLLO FORMATO 1: po di gestione                *
049300020906     C*----------------------------------------------------*
049400020906     C     CONTR1        BEGSR
049500040901     C* controllo e inverto la data distinta (G02INV)
049600040901     c                   exsr      srddc
049700040901     c* no errore data proseguo
049800040901     c                   if        not *in90
049900040901     C* Controllo la filiale che cambia tutta la mia linea di arrivo
050000941108     C     *IN21         IFEQ      '1'                                          *
050100020509     c                   clear                   trul06ds
050200020509     c                   eval      d06cod = '£1'
050300020916     c                   movel     simfel        d06key
050400040901     c                   move      'S'           d06key
050500040901     C                   Z-ADD     G02INV        d06drf
050600020509     c                   movel     trul06ds      kpjbu
050700020509     c                   call      'TRUL06R'
050800020509     c                   parm                    kpjba
050900020509     c                   movel     kpjbu         trul06ds
051000020701     c                   movea     lint          Lmn
051100941107     C* Controllo se esiste la filiale da me digitata
051200941108     C     V1CLNA        LOOKUP    LMN                                    70
051300940503     C     *IN70         IFEQ      '0'
051400940503     C                   SETON                                        9017
051500940503     C                   MOVEL     ERR(6)        $MSG
051600941107     C                   ELSE
051700950613     C* Controllo se filiale gestita da altra
051800950613     C     V1CLNA        LOOKUP    L£6                                    70
051900950613     C     *IN70         IFEQ      '1'
052000950613     C                   SETON                                        9017
052100950613     C                   MOVEL     ERR(18)       $MSG
052200950613     C                   ELSE
052300941107     C* Recupero le linee di arrivo dalla tabella £6
052400941108     C                   MOVE      V1CLNA        FILIA
052500941107     C* Aggancio la £6 e recupero le linee a loro associate
052600020509     c                   clear                   trul06ds
052700020509     c                   eval      d06cod = '£6'
052800020509     c                   movel     filia         d06key
052900040831     c                   move      'S'           d06key
053000040901     C                   Z-ADD     G02INV        d06drf
053100020509     c                   movel     trul06ds      kpjbu
053200020509     c                   call      'TRUL06R'
053300020509     c                   parm                    kpjba
053400020509     c                   movel     kpjbu         trul06ds
053500020509     c                   movea     lint          Lin
053600941108     C                   END
053700911021     C                   END
053800950613     C                   END
053900040901     C                   END
054000140926     c                   setoff                                       97
054100141029     c                   clear                   fnlry09ds2
054200140926     c                   eval      ilry09fgs = v1clna
054300140926     c                   eval      ilry09tch = 'C'
054400141013     c                   exsr      verdispo
054500141029     c                   if        OLRY09ESAL = '1' and
054600140926     c                             OLRY09ERR  = *blank
054700140926     c                   seton                                        97
054800140926     c                   endif
054900050520     c* parcel/messaggerie
055000050520     c                   if        not *in17
055100050520     c                   exsr      srtpm
055200050520     c                   end
055300080208     C                   ENDSR
055400020906     C*----------------------------------------------------*
055500050520     C* CONTROLLO parcel / messaggerie                     *
055600020906     C*----------------------------------------------------*
055700050520     C     srtpm         BEGSR
055800070412     c                   setoff                                       98
055900080208     c* leggo organigramma x gestire ind 04 parcel+messaggerie
056000050520     c     v1clna        chain     azorg01l
056100050520     c                   if        %found(azorg01l)
056200070309     c                   movel     orgde6        og146
056300080208     c                   movel     orgde8        og148
056400080121     C                   MOVEL     ORGde7        og147
056500080212     c* se procedura PDA non partita proteggo i flag
056600080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
056700080212     c                   setoff                                       71
056800080212     c                   else
056900080212     c                   seton                                        71
057000080212     c                   end
057100070313     c                   if        §ogdda <> ' '
057200070313     c                   movel     §ogdda        comdda            8 0
057300070412     c                   if        comdda <= dateu
057400070419     c*                  seton                                        98
057500070412     c                   end
057600070313     c                   else
057700070313     c                   clear                   comdda
057800070313     c                   end
057900070309     c                   movel     orgdf0        og150
058000050520     c     §OGPKM        comp      'S'                                    04
058001160429     c     §OGturno      comp      'S'                                    01
058100050520     c                   end
058200050520     C*
058300050520     C                   ENDSR
058400050520     C*----------------------------------------------------*
058500050520     C* CONTROLLO FORMATO 1                                *
058600050520     C*----------------------------------------------------*
058700050520     C     CONTR2        BEGSR
058800941109     C*
058900941109     C* NUMERO FOGLIO VIAGGIO
059000941109     C     V1CNDC        IFNE      *BLANKS
059100941109     C     V1CNDC        ANDNE     *ZEROS
059200941109     C*
059300020906     C* ricerca
059400941109     C     '?'           SCAN      V1CNDC                                 31
059500941109     C     *IN31         IFEQ      '1'
059600941110     C                   CLEAR                   DSLR01
059700941109     C                   Z-ADD     DATEU         DLRADA
059800941109     C                   Z-ADD     V1CLNA        DLRFGS
059900941109     C                   MOVEL     'FNLR50R'     DLRPGM
060000941109     C                   MOVEL     'S'           DLRRIC
060100941109     C                   MOVEL     '2'           DLRTFV
060200941109     C                   Z-ADD     4             DLRNPG
060300941110     C                   MOVEL     DSLR01        KPJBU
060400941109     C                   CALL      'FNLR02R'
060500941109     C                   PARM                    KPJBA
060600941109     C                   MOVEL     KPJBU         DSLR01
060700950215     C                   MOVE      *ZEROS        V1CNDC
060800950215     C                   MOVE      DLRNFV        V1CNDC
060900941109     C                   END
061000020906     C* controlli
061100941109     C                   TESTN                   V1CNDC               31
061200020918     c                   if        not *in31
061300020918     C                   SETON                                        9012
061400020918     C                   MOVEL     ERR(13)       $MSG
061500020918     C                   Z-ADD     *ZEROS        V01NDC
061600020918     C                   ELSE
061700020918     C                   MOVE      V1CNDC        V01NDC
061800020918     C                   END
061900020918     C                   ELSE
062000941109     C                   Z-ADD     *ZEROS        V01NDC
062100950601     C                   MOVEL     *BLANKS       V1CPEP
062200941109     C                   END
062300941109     C*  Controllo il numero di distinta se diverso da zero
062400941109     C*  deve esistere in archivio flarb00 o flarb50
062500941109     C*
062600960604     C  N90V01NDC        IFNE      *ZEROS
062700941109     C* Controllo se esiste in archivio FLFVV
062800941109     C                   Z-ADD     V01NDC        FVVNFV
062900020905     C                   Z-ADD     V1cLNA        FVVFGS
063000941109     C                   SETOFF                                       49
063100020905     C     KFVV          CHAIN     FNFVV01L                           1249
063200020906     C* rilascio
063300950207     C  N12
063400950207     CANN49              UPDATE    FNFVV000
063500950207     C  N12
063600950207     CANN49V01NDC        IFNE      SAVNDC
063700941228     C                   MOVEL     FVVFPP        V1CFPP
063800950601     C                   MOVEL     FVVPEP        V1CPEP
063900050523     C                   MOVEL     FVVspn        dfvvspn
064000150804     c                   if        *in04
064100160429     C                   MOVEL     §fvvtpm       v1ctpm
064200150804     c                   else
064300150804     c                   clear                   v1ctpm
064400150804     c                   end
064500150804     c                   if        v1ctpm = ' '
064600050523     C                   MOVEL     'P'           v1ctpm
064700050523     c                   end
064701160429     c* turno
064702160429     c                   if        *in01
064703160429     C                   MOVEL     §fvvturno     v1ctur
064704160429     c                   else
064705160429     c                   clear                   v1ctur
064706160429     c                   end
064707160429     c                   if        v1ctur = ' ' and §fvvtpm = ' '
064708160429     C                   MOVEL     '1'           v1ctur
064709160429     c                   end
064800941228     C                   END
064900941109     C     *IN49         IFEQ      '1'
065000941109     C                   SETON                                        9013
065100941109     C                   MOVEL     ERR(12)       $MSG
065200941109     C                   END
065300960604     C* Se numero distinta > 5 chr do errore
065400960604     C                   MOVEL     V01NDC        WCOMO2            2 0
065500960604     C  N12WCOMO2        IFGT      0
065600960604     C                   SETON                                        9012
065700960604     C                   MOVEL     ERR(13)       $MSG
065800960604     C                   END
065900960604     C*
066000941109     C     *IN12         IFEQ      '1'
066100941109     C     FVVATB        OREQ      '*'
066200941109     C     FVVFCF        OREQ      'S'
066300941109     C                   SETON                                        9012
066400941109     C                   MOVEL     ERR(13)       $MSG
066500941109     C                   END
066600941109     C  N90              DO
066700950613     C     FVVFCF        IFNE      ' '
066800941109     C                   SETON                                        9012
066900941109     C                   MOVEL     ERR(14)       $MSG
067000941109     C                   END
067100020906     C* in gestione ad altra filiale
067200020906     C*    FVVFGS        LOOKUP    LIN                                    31
067300020906     C*    *IN31         IFEQ      '0'
067400020906     C*                  SETON                                        9012
067500020906     C*                  MOVEL     ERR(15)       $MSG
067600020906     C*                  END
067700941109     C*
067800951010     C     V01NDC        IFNE      SAVNDC
067900941109     C                   MOVE      FVVDFV        ROVDDC
068000941109     C                   MOVE      FVVDFV        G02INV
068100941109     C                   MOVEL     '3'           G02ERR
068200941109     C                   CALL      'XSRDA8'
068300941109     C                   PARM                    WLBDA8
068400941109     C                   Z-ADD     G02INV        ROVDDC
068500941109     C                   Z-ADD     G02DAT        V1CDDC
068600941109     C                   MOVEL     ROVDDC        WAA4              4 0
068700941109     C                   MOVE      WAA4          AAA
068800941109     C                   END
068900951010     C                   END
069000941109     C*
069100941109     C  N90              DO
069200020905     C                   Z-ADD     V1CLNA        KFGS
069300020905     C                   Z-ADD     V01NDC        KNDC
069400960702     C                   Z-ADD     ROVDDC        KDDC
069500080121     c                   eval      wnfv = kndc
069600020905     C  N12KARB          CHAIN     FNARB70L                           92
069700080121     c* imposto flag pda
069800080208     C     V01NDC        IFNE      SAVNDC
069900080121     C     KDST3         CHAIN(N)  FiDST09L
070000080121     c                   if        %found(fidst09l)
070100080208     c                   eval      ddstflr = £dstflr
070300080208     c                   eval      v1ctstpda = §dsttstpda
070302160429     c                   eval      v1cpda = £dstpda
070400080121     c                   else
070500080121     c                   clear                   v1cpda
070600080208     c                   clear                   v1ctstpda
070700080121     c                   end
070800080319     c* verifico fasi distinta e se presenti ORM
070900080320     c                   exsr      srfasi
071000080410     c                   end
071100120718     C  N12              MOVEL     £DSTPDR       ARBPDC
071200941109     C*
071300950601     C     *IN92         IFEQ      '0'
071400950601     C     V1CCPC        IFEQ      *ZEROS
071500941109     C     V1CCPC        OREQ      *BLANKS
071600950123     C                   MOVE      ARBPDC        WCDCPC
071700941109     C                   END
071800120806     c                   else
071900120806     C     V1CCPC        IFEQ      *ZEROS
072000120806     C     V1CCPC        OREQ      *BLANKS
072100120806     C                   MOVE      £dstpdr       WCDCPC
072200120806     C                   END
072300950601     C                   END
072400950601     C*
072500941109     C                   SETOFF                                       70
072600941109     C  N92ARBLNA        LOOKUP    LIN                                    70
072700941109     C  N92
072800020905     CANN70dutlpo        IFEQ      '2'
072900941109     C                   SETON                                        9012
073000941109     C                   MOVEL     ERR(13)       $MSG
073100941109     C                   END
073200941109     C                   END
073300941109     C*
073400941109     C                   END
073500020906     C* DATA DISTINTA
073600941107     C  N90V1CDDC        IFEQ      *ZEROS
073700940503     C                   SETON                                        9011
073800940503     C                   MOVEL     ERR(7)        $MSG
073900911016     C                   ELSE
074000911017     C* routine di controllo data distinta
074100040901     c                   exsr      srddc
074200040901     C     G02ERR        IFne      '1'
074300951010     C* IN MODIFICA NON POSSO VARIARE LA DATE DISTINTA
074400951010     C  N90V01NDC        IFNE      *ZEROS
074500951010     C     G02INV        ANDNE     FVVDFV
074600951010     C                   SETON                                        9011
074700951010     C                   MOVEL     ERR(19)       $MSG
074800951010     C                   END
074900941107     C                   Z-ADD     G02DAT        V1CDDC
075000940503     C                   END
075100950418     C*
075200971119     C* CONTROLLO SE LNA ITALIA O  ESTERO
075300971119     C                   MOVEL     *BLANKS       ORGFL1
075400971119     C                   MOVE      V1CLNA        ORGFIL
075500160429     C     ORGFIL        CHAIN     AZORG01L
075600050711     c                   movel     orgde3        og143
075700020729     C                   MOVEL     *blank        flgest            1
075800020729      * se estero imposto il flag
075801160429     c                   if        %found(azorg01l)
075900160429     C     §OgNTW        ifeq      'FED'
076000020729     C     §OgNTW        oreq      'EEX'
076100020729     C     §OgNTW        oreq      'EUP'
076200020729     C                   MOVEL     'E'           flgest
076300020729     c                   end
076301160429     c                   end
076400050711      *?  inserimento  DISTINTA ESTERO TESTO LA DATA
076500050711     C     V1CNDC        IFeq      *BLANKS
076600050711     C     flgest        ANDeq     'E'
076700050711     C     V1CNDC        OReq      *ZEROS
076800050711     C     flgest        andeq     'E'
076900050711      *controllo range fra le date di oggi e quella immessa
077000050711     c                   eval      dadata = g02inv
077100050711     c                   eval      adata  = dateu
077200050711     c                   CALL      'XSRLAV8'
077300050711     c                   PARM                    Wdata8
077400050711     c                   if        giolav > §dgdgg
077500050711     c                   SETON                                        9011
077600050711     c                   MOVEL     ERR(23)       $MSG
077700050711     c                   end
077800050711     c                   end
077900020419     C* inserimento  PER NO DISTINTA ESTERO TESTO LA DATA
078000020419     C     V1CNDC        IFeq      *BLANKS
078100020729     C     flgest        ANDNE     'E'
078200020419     C     V1CNDC        OReq      *ZEROS
078300020729     C     flgest        andne     'E'
078400020419      *non ammessa data minore di oggi
078500020419     c                   if        g02inv < dateu
078600020419     c                   SETON                                        9011
078700020419     c                   MOVEL     ERR(24)       $MSG
078800020419     c                   endif
078900020419      *controllo range fra le date di oggi e quella immessa
079000020419     c                   eval      dadata = dateu
079100020419     c                   eval      adata  = g02inv
079200020419     c                   CALL      'XSRLAV8'
079300020419     c                   PARM                    Wdata8
079400020419     c                   if        giolav > §dgddc
079500020419     c                   SETON                                        9011
079600020419     c                   MOVEL     ERR(23)       $MSG
079700020419     c                   end
079800020419     c                   end
079900020419     C* manutenzione  PER NO DISTINTA ESTERO TESTO LA DATA
080000020419     C     V1CNDC        IFne      *BLANKS
080100020729     C     flgest        andne     'E'
080200020419     C     V1CNDC        ORne      *ZEROS
080300020729     C     flgest        andne     'E'
080400920306     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
080500950418     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
080600990823     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
080700990823     C                   MOVE      DATEU         WMGUDT            4 0
080800990823     C                   MOVEL     WMGUDT        WMUDT             2 0
080900000929     C                   Z-ADD     §DGDGG        NRGIO             3 0
081000000929     C  N90WMGUDT        IFGE      §DGDEI
081100000929     C     WMGUDT        ANDLE     §DGDEF
081200000929     C                   Z-ADD     §DGDGE        NRGIO
081300000929     C                   END
081400000929     C  N90DATEU         IFGE      §DGDII
081500000929     C     DATEU         ANDLE     §DGDIF
081600000929     C                   Z-ADD     §DGDGI        NRGIO
081700000929     C                   END
081800000929     C     G02TGI        SUB       NRGIO         GIOTGI
081900941107     C                   CALL      'XSRGI8'
082000920306     C                   PARM                    DATPAR
082100950418     C                   Z-ADD     GIOINV        DATM15            8 0
082200950418     C     DATM15        IFGT      DATEU
082300950418     C                   SETON                                        9011
082400950418     C                   MOVEL     ERR(16)       $MSG
082500950418     C                   END
082600000929     C     G02TGI        ADD       NRGIO         GIOTGI
082700941107     C                   CALL      'XSRGI8'
082800920306     C                   PARM                    DATPAR
082900950418     C                   Z-ADD     GIOINV        DATP15            8 0
083000950418     C     DATP15        IFLT      DATEU
083100950418     C                   SETON                                        9011
083200950418     C                   MOVEL     ERR(16)       $MSG
083300950418     C                   END
083400950418     C*
083500920306     C                   END
083600920306     C*
083700941107     C  N90              Z-ADD     G02INV        ROVDDC            8 0
083800950418     C  N90              MOVEL     ROVDDC        WAA4              4 0
083900950418     C  N90              MOVE      WAA4          AAA
084000911016     C                   END
084100950607     C*------------------------------------------------------*
084200950607     C*  Controllo sul calendario il tipo di giorno che è la
084300950607     C*  data di consegna
084400950609     C     ROVDDC        IFNE      0
084500950607     C                   Z-ADD     ROVDDC        WMMGG             4 0
084600950607     C                   Z-ADD     ROVDDC        WG                2 0
084700950607     C                   MOVEL     ROVDDC        KANN
084800950607     C                   Z-ADD     0             KTFP
084900950607     C                   Z-ADD     0             KTFA
085000950607     C                   MOVEL     WMMGG         KMES
085100950607     C     KCLN          CHAIN     AZCLN01L                           31
085200950607     C                   MOVEA     CLNMAT        MES
085300950607     C                   MOVE      MES(WG)       WGG               1
085400950609     C                   END
085500020906     C*PADRONCINO:
085600020906     c* il po di gestione e il po padroncino devono essere uguali
085700020906     C     V1CFPD        IFNE      V1CLNA
085800020906     C                   SETON                                        9019
085900020906     C                   MOVEL     ERR(17)       $MSG
086000020906     C                   END
086100020906     C* Ricerca padroncino
086200950123     C  N90V1CCPC        IFNE      *BLANKS
086300950123     C     V1CCPC        ANDNE     *ZEROS
086400911113     C                   SETOFF                                           31
086500941107     C     '?'           SCAN      V1CCPC                                 31
086600911104     C   31              DO
086700021204     c                   clear                   fnlv24ds
086800021204     C                   MOVEL     'A'           d24tip
086900021204     C                   MOVEL     'R'           d24FLG
087000021204     C                   MOVEL     V1CFPD        d24FIL
087100941107     C                   MOVE      *BLANKS       V1CCPC
087200021204     C                   MOVEL(p)  fnlv24ds      KPJBU
087300941108     C                   CALL      'FNLV24R'
087400921028     C                   PARM                    KPJBA
087500021204     C                   MOVEL     KPJBU         fnlv24ds
087600021204     C     d24PDR        IFNE      *ZEROS
087700021204     C                   MOVEL     d24PDR        WCDCPC
087800911104     C                   END
087900950123     C*
088000950123     C                   END
088100911104     C                   END
088200911017     C*
088300911017     C* Controllo il codice padroncino se corretto
088400911017     C*
088500941107     C  N90V1CCPC        IFNE      *ZEROS
088600941107     C     V1CCPC        ANDNE     *BLANKS
088700950217     C* Controllo se esiste nell'anagrafica padroncini
088800950217     C                   SETOFF                                       3132
088900950217     C                   TESTN                   WCDCPC               31
089000950217     C  N31              SETON                                        32
089100950217     C  N32              MOVE      WCDCPC        APDPDR
089200021204     C  N32kapd          CHAIN     fiapd01L                           13
089300000728     C  N32
089400000728     CANN13APDATB        IFNE      ' '
089500000728     C                   SETON                                        13
089600000728     C                   ENDIF
089700940503     C     *IN13         IFEQ      '1'
089800950217     C     *IN32         OREQ      '1'
089900940503     C                   SETON                                        9013
090000940503     C                   MOVEL     ERR(9)        $MSG
090100940503     C                   ELSE
090200941109     C                   MOVE      APDRSC        V05PAD
090300941109     C                   MOVE      APDRSC        V06PAD
090400950123     C                   MOVE      WCDCPC        CODPAD            7 0
090500950123     C                   MOVEL     V1CFPD        WFIL              3 0
090600950613     C     WFIL          LOOKUP    LIN                                    37
090700950613     C     *IN37         IFEQ      '0'
090800940503     C                   SETON                                        9013
090900940503     C                   MOVEL     ERR(10)       $MSG
091000940503     C                   END
091100950601     C*  Se picking non presente su foglio viaggio lo imposto da qui
091200950601     C     V1CPEP        IFEQ      *BLANKS
091300950601     C     APDPKM        IFEQ      'S'
091400950601     C     V1CFPP        ANDEQ     'M'
091500950601     C     APDPKP        OREQ      'S'
091600950601     C     V1CFPP        ANDEQ     'P'
091700950601     C                   MOVEL     'S'           V1CPEP
091800950601     C                   ELSE
091900950601     C                   MOVEL     'N'           V1CPEP
092000950601     C                   END
092100950601     C                   END
092200080121     c                   movel     apdflr        dapdflr
092300080410     c******************** Dati PDA  ****************************
092400080121     c                   if        v1cpda = *blanks
092500080121     c                   movel     §apdpda       v1cpda
092600101021     c                   movel     §apdtstpda    v1ctstpda
092700080709     c                   end
092800080121     c                   if        v1cpda = *blanks
092900080121     c                   movel     'N'           v1cpda
093000101021     c                   clear                   v1ctstpda
093100080121     c                   end
093200080212     c* controllo flag dati per PDA se attivo
093300080212     c                   if        *in71
093400080208     C                   if        (v1cpda = 'O' and
093500080208     c                             §OGPDAORM = ' ') or
093600080208     C                             (v1cpda = 'C' and
093700080208     c                             §OGPDACON = ' ') or
093800080208     C                             (v1cpda = 'E' and
093900080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
094000080208     C                   MOVEL     err(25)       $msg
094100080208     c                   select
094200080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
094300080208     c                   eval      $msg = %trim($msg)+' CONSEGNE'
094400080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
094500080208     c                   eval      $msg = %trim($msg) + ' ORM'
094600080208     c                   endsl
094700080208     C                   SETON                                        1090
094800080208     C                   ENDIF
094900080318     c* se distinta già preparata per pda non posso cambiare il flag x con.
095000080318     c* se ho la fase PDC
095100081111     c                   if        (£dstpda = 'E' or £dstpda ='C') and
095200081111     c                             (v1cpda ='N' or v1cpda='O') and
095300080320     c                             fasecon ='PDC'
095400080318     c                   seton                                        1090
095500080318     C                   MOVEL     err(27)       $msg
095600080318     c                   end
095700080318     c* se distinta già preparata per pda non posso cambiare il flag x orm
095800080318     c* se ho il FIPCO00T
095900081111     c                   if        (£dstpda = 'E' or £dstpda ='O') and
096000081111     c                             (v1cpda = 'N' or v1cpda ='C') and
096100080320     c                              faseorm ='PDR'
096200080318     c                   seton                                        1090
096300080318     C                   MOVEL     err(27)       $msg
096400080318     c                   end
096500080208     c* controllo flag test per PDA
096600080208     C                   if        (v1ctstpda = 'O' and
096700080208     c                             §OGPDAORM = ' ') or
096800080208     C                             (v1ctstpda = 'C' and
096900080208     c                             §OGPDACON = ' ') or
097000080208     C                             (v1ctstpda = 'E' and
097100080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
097200080208     C                   MOVEL     err(25)       $msg
097300080208     c                   select
097400080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
097500080208     c                   eval      $msg = %trim($msg) + ' CONSEGNE'
097600080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
097700080208     c                   eval      $msg = %trim($msg) + ' ORM'
097800080208     c                   endsl
097900080208     C                   SETON                                        6990
098000080208     C                   ENDIF
098100080326     c* controllo la congruenza dei valori impostati nel test x pda e
098200080326     c* e nei dati x pda
098300080326     C                   if        v1ctstpda <>' '
098400080326     C                   if        (v1cpda = 'O' and
098500080326     C                             v1ctstpda <>'O') or
098600080326     C                             (v1cpda = 'C' and
098700080326     C                             v1ctstpda <>'C') or
098800080326     C                             v1cpda = 'N'
098900080326     C                   MOVEL     err(26)       $MSG
099000080326     C                   SETON                                        6990
099100080326     C                   ENDIF
099200110224     c* si può inserire il flag di test C/E solo se non si sono ricevuti
099300110224     c* esiti consegne (se già inseriti ma non ancora ricevuti, paziena !!!)
099400110224     c                   if        §dsttstpda = ' ' and
099500110224     c                             v1ctstpda <> 'O'
099600110225     c     kpct          setll     fipct02l                               69
099700110225     c                   if        *in69
099800110225     C                   MOVEL     err(27)       $MSG
099900110225     C                   SETON                                        90
100000110224     c                   end
100100110224     c                   end
100200110224     c* se si era in test con Entrambi e si vuole passare in buona con uno
100300110224     c* dei due, non si può fare
100400110530     c*                  if        §dsttstpda = 'E' and
100500110530     c*                            v1ctstpda <> 'E'
100600110530     C*                  MOVEL     err(26)       $MSG
100700110530     C*                  SETON                                        9069
100800110530     c*                  end
100900110224     c* non è possibile eliminare il flag di test
101000110224     c                   else
101100110224     c                   if        §dsttstpda <> ' ' and
101200110225     c                             (faseorm ='PDR' or fasecon = 'PDC')
101300110225     C                   MOVEL     err(27)       $msg
101400110225     C                   SETON                                        9069
101500110224     c                   end
101600110224     C                   ENDIF
101700080208     c* se procedura attiva e aut. in test avviso
101800080208     C                   if        (v1ctstpda ='E' and
101900080211     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
102000080208     C                             or (v1ctstpda ='C' and
102100080208     C                             §OGPDACON='S')
102200080208     C                             or (v1ctstpda ='O' and
102300080208     C                             §OGPDAORM='S')
102400080208     C                   eval      msgv= 'ATT. gestione PDA attiva'
102500080208     c                   else
102600080208     c                   clear                   msgv
102700080208     C                   ENDIF
102800080318     c* se aut. ha dati per pda differenti da quelli della distinta avviso
102900080319     c                   clear                   msgv1
103000080415     c                   setoff                                       45
103100101025     C                   if        (v1cpda = 'N' and
103200080319     C                             v1cpda <> §apdpda)
103300080415     C                   eval      msgv1= 'ATTENZIONE Aut. con PDA'
103400080415     c                   seton                                        45
103500080319     c                   end
103600101025     C                   if        (v1cpda <> 'N' and
103700080319     C                             §apdpda = 'N')
103800080415     C                   eval      msgv1= 'ATTENZIONE Aut.senza PDA'
103900080415     c                   seton                                        45
104000080319     c                   end
104100080212     C                   ENDIF
104200080121     c* parcel/messaggerie
104300050524     c                   if        v1ctpm = *blanks
104400150804     c                   if        *in04
104500150804     c                   movel     §apdtpm       v1ctpm
104600150804     c                   end
104700150804     c                   if        v1ctpm = ' '
104800050523     c                   movel     'P'           v1ctpm
104900050523     c                   end
105000050523     c                   end
105001160429     c* turno
105002160505     c                   if        v1ctur = *blanks and v1ctpm = 'P'
105003160502     c                   if        *in01
105004160429     c                   movel     §apdturno     v1ctur
105005160505     c                   end
105006160429     c                   if        v1ctur = ' '
105007160429     c                   movel     '1'           v1ctur
105009160429     c                   end
105020160429     c                   end
105021160505     c                   if        v1ctpm = 'M' and v1ctur <> ' '
105022160505     c                             and *in01
105023160505     C                   MOVEL     ERR(28)       $MSG
105024160505     c                   seton                                        9190
105025160505     c                   end
105100950601     C*
105200941109     C  N90APDPDD        IFNE      'S'
105300931109     C*  Se padroncino non è dipedente controllo se in filiale
105400931109     C*  di appartenza è attivata la gestione conteggi
105500941109     C                   MOVE      WFIL          ORGFIL
105600160429     C     ORGFIL        CHAIN     AZORG01L
105700160429     C                   IF        %found(azorg01l)
105800950123     C                   MOVE      WCDCPC        FTWPDR
105900931109     C                   MOVE      'C'           FTWFVL
106000931109     C                   MOVE      ' '           FTWTSR
106100931109     C                   MOVE      ROVDDC        FTWDDC
106200160429     C     KEYFT2        CHAIN     fiftt02L
106300160429     C     KEYFT2        CHAIN     fifst02L
106400160429     C                   if        %found(fiftt02l) or
106401160429     C                             %found(fifst02l)
106500940503     C                   SETON                                        9011
106600940503     C                   MOVEL     ERR(8)        $MSG
106700931109     C                   END
106800940503     C                   END
106900931109     C                   END
107000911024     C                   END
107100080318     c* se cambio l'aut ed ho spedizioni o orm sulla distinta chiedo di
107200080318     c* confermare la modifica
107300080319     C     V01NDC        IFNE      *ZEROS
107400941108     C     ARBPDC        ANDNE     CODPAD
107500950123     C     SAVCPC        ANDNE     WCDCPC
107600080319     C     *in92         ANDeq     '0'
107700080318     C     V01NDC        orNE      *ZEROS
107800080326     C     ARBPDC        ANDNE     CODPAD
107900080319     C     SAVCPC        ANDNE     WCDCPC
108000080326     C     worm          ANDeq     'S'
108100940503     C     *IN03         IFEQ      '0'
108200940503     C                   SETON                                        9013
108300940503     C                   MOVEL     ERR(11)       $MSG
108400940503     C                   ELSE
108500940503     C                   SETON                                        90
108600940503     C                   MOVEL     ERR(11)       $MSG
108700940503     C                   SETOFF                                       03
108800911104     C                   END
108900940503     C                   END
109000911017     C*
109100911105     C                   END
109200950123     C   92
109300950123     CANN90V1CCPC        IFEQ      *ZEROS
109400950123     C     V1CCPC        OREQ      *BLANKS
109500941107     C     V01NDC        IFNE      *ZEROS
109600940503     C                   SETON                                        9013
109700940503     C                   MOVEL     ERR(9)        $MSG
109800911108     C                   END
109900911108     C                   END
110000950123     C  N90V1CCPC        IFEQ      *ZEROS
110100950123     C     V1CCPC        OREQ      *BLANKS
110200941107     C     V01NDC        IFEQ      *ZEROS
110300940503     C                   SETON                                        9013
110400940503     C                   MOVEL     ERR(9)        $MSG
110500911104     C                   END
110600911105     C                   END
110700030618     c* accendo o spengo l'indicatore 36 per gestire in seguito il campo
110800030618     c* dove andrà il barcode della spedizione
110900030618     c     v1cisp        comp      'P'                                    36
111000030703     c                   if        v1cisp = 'P'
111100030703     c                   eval      v03f2 = 'F2=Manuale'
111200030703     c                   else
111300030703     c                   eval      v03f2 = 'F2=Pistola'
111400030703     c                   end
111500911017     C*
111600911017     C                   ENDSR
111700911017     C*----------------------------------------------------*
111800040901     C* Controllo data distinta                            *
111900911017     C*----------------------------------------------------*
112000040901     C     srddc         BEGSR
112100911017     C*
112200040901     C                   MOVE      V1CDDC        G02DAT
112300040901     C                   MOVEL     *BLANK        G02ERR
112400040901     C                   CALL      'XSRDA8'
112500040901     C                   PARM                    WLBDA8
112600040901     C     G02ERR        IFEQ      '1'
112700040901     C                   SETON                                        9011
112800040901     C                   MOVEL     ERR(7)        $MSG
112900040901     c                   end
113000040901     C*
113100040901     C                   ENDSR
113200040901     C*----------------------------------------------------*
113300040901     C* CARICO IL FILE DI LAVORO CON LE SPEDIZIONE DISTINTA*
113400040901     C*----------------------------------------------------*
113500040901     C     CARDIS        BEGSR
113600040901     C*
113700020905     C                   Z-ADD     V1cLNA        KFGS
113800960702     C                   Z-ADD     V01NDC        KNDC
113900960702     C                   Z-ADD     ROVDDC        KDDC
114000020905     C     KARB          SETLL     FNARB70L
114100911017     C*
114200941108     C* Azzero il contatore delle bolle in dist.
114300920220     C                   Z-ADD     0             NUM               3 0
114400911017     C                   DO        *HIVAL
114500911017     C*
114600020905     C     KARB          READE     FNARB70L                               50
114700941107     C     *IN50         IFEQ      '0'
114800160429     C     KARB1         CHAIN(N)  FNARBW2L
114900911017     C*
115000911112     C                   MOVE      '1'           ARBFLG
115100911017     C*
115101160429     c                   if        not %found(fnarbw2l) and
115200160429     C                             arbngd > NUM
115300920528     C                   Z-ADD     ARBNGD        NUM
115400920528     C                   END
115500990920     C*  Carico in schiera gli eventuali stop
115600940322     C     ARBSTP        IFNE      0
115700990920     C*  Azzero campo codice cliente
115800990920     C                   Z-ADD     0             CODCLI            7 0
115900990920     C* Verifico il tipo prima e seconda bolla
116000951207     C                   Z-ADD     1             X                 2 0
116100951207     C                   MOVEL     *BLANKS       WTBOL1
116200951207     C                   CLEAR                   DS3A
116300951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
116400951207     C     *IN13         IFEQ      '1'
116500951207     C                   MOVEL     D3A(X)        DS3A
116600951207     C                   MOVEL     §3ATB1        WTBOL1            1
116700990920     C                   MOVEL     §3ATB2        WTBOL2            1
116800951207     C                   END
116900990920     C* Se la prima bolla è un assegnato eseguo posizionamento
117000990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
117100990920     C     WTBOL1        IFEQ      'A'
117200990920     C                   MOVEL     '1'           KTRC
117300990920     C                   Z-ADD     ARBAAS        KAAS
117400990920     C                   Z-ADD     ARBAAS        KLNP
117500990920     C                   Z-ADD     ARBAAS        KNRS
117600990920     C                   Z-ADD     ARBAAS        KNSP
117700990920     C     KAR6          CHAIN     FIAR601L                           74
117800990920     C* ... se non trovo bolla o il codice cliente non è
117900990920     C* impostato reperisco il codice da FNARB
118000990920     C     *IN74         IFEQ      '1'
118100990920     C     AR6KSC        OREQ      0
118200990920     C                   Z-ADD     ARBKSC        CODCLI
118300990920     C                   ELSE
118400990920     C* ... altrimenti recupero codice cliente da FIAR6
118500990920     C                   Z-ADD     AR6KSC        CODCLI
118600990920     C                   END
118700990920     C* .... se prima bolla è un porto franco ....
118800990920     C                   ELSE
118900990920     C* .... verifico se la seconda è un assegnato
119000990920     C     WTBOL2        IFEQ      'A'
119100990920     C                   MOVEL     '2'           KTRC
119200990920     C                   Z-ADD     ARBAAS        KAAS
119300990920     C                   Z-ADD     ARBAAS        KLNP
119400990920     C                   Z-ADD     ARBAAS        KNRS
119500990920     C                   Z-ADD     ARBAAS        KNSP
119600990920     C     KAR6          CHAIN     FIAR601L                           74
119700990920     C* ... e reperisco il codice bolla da lì
119800990920     C     *IN74         IFEQ      '0'
119900990920     C     AR6KSC        ANDNE     0
120000990920     C                   Z-ADD     AR6KSC        CODCLI
120100951207     C                   END
120200990920     C                   END
120300990920     C*
120400990920     C                   END
120500990920     C* Controllo se codice bolla è codificato
120600951207     C                   Z-ADD     CODCLI        APPOG             4 0
120700951207     C     APPOG         IFNE      8888
120800951207     C     APPOG         ANDNE     9999
120900990920     C* ed è un assegnato
121000990920     C     CODCLI        ANDNE     0
121100951211     C                   MOVEL     *BLANKS       RAGCLI
121200951211     C                   MOVEL     CODCLI        RAGCLI
121300951207     C                   ELSE
121400951121     C                   MOVEL     ARBRSD        RAGCLI           70
121500951121     C                   MOVE      ARBIND        RAGCLI           70
121600951207     C                   END
121700961128     C                   Z-ADD     1             X                 2 0
121800951123     C     ARBSTP        LOOKUP    STP2(X)                                13
121900961217     C     *IN13         IFEQ      '0'
122000961217     C     W             ANDGE     999
122100961217     C                   SETON                                        79
122200961217     C                   MOVEL     ERR(20)       $ERR
122300961217     C                   END
122400940331     C     *IN13         IFEQ      '0'
122500961217     C     W             ANDLT     999
122600940331     C                   ADD       1             W
122700940322     C                   Z-ADD     ARBSTP        STP2(W)
122800951121     C                   MOVEL     RAGCLI        RSD2(W)
122900940331     C                   END
123000940322     C     ARBSTP        IFGT      ULTSTP
123100940322     C                   Z-ADD     ARBSTP        ULTSTP
123200940322     C                   END
123300940322     C                   END
123301160429     c                   if        not %found(fnarbw2l) and *in50 = '0'
123500160429     C                   WRITE     FNARBW00
123600911108     C                   END
123601160429     C                   END
123700911017     C*
123800941107     C  N50              END
123900911017     C*
124000911104     C                   SETON                                        05
124100941107     C                   FEOD      FNARBW2L
124200911017     C                   ENDSR
124300911017     C*----------------------------------------------------*
124400911017     C* GESTIONE DEL FORMATO 3                             *
124500911017     C*----------------------------------------------------*
124600940202     C     GESFM3        BEGSR
124700911017     C*
124800911017     C* Pulisco i campi che devo vidualizzare nel fm2
124900030618     C                   clear                   V03pis
125000030618     C                   MOVEL     ROVDDC        VIDANS
125100911017     C                   Z-ADD     *ZEROS        V03LNP
125200911017     C                   Z-ADD     *ZEROS        V03NRS
125300911017     C                   Z-ADD     *ZEROS        V03NSP
125400911105     C                   SETOFF                                       23
125500911108     C                   Z-ADD     *ZEROS        CTR
125600911017     C*
125700911017     C                   DO        *HIVAL
125800030701     c* NO ERRORE
125900030620     C* se non c'è errore ed è stata trovata la bolla pulisco i riferimenti
126000030620     c* della spedizione
126100030620     c                   if        not *in90
126200030620     C                   clear                   V03pis
126300030620     C                   MOVEL     ROVDDC        VIDANS
126400030620     C                   Z-ADD     *ZEROS        V03LNP
126500030620     C                   Z-ADD     *ZEROS        V03NRS
126600030620     C                   Z-ADD     *ZEROS        V03NSP
126700080415     C                   SETOFF                                       4341
126800030620     c                   else
126900030701     c* SI ERRORE
127000030701     c* se c'è ind. 90 di errore generico acceso, abblenco i campi della
127100030701     c* spedizione se ho barcode impostato e se sono entrato in modalità
127200030701     c* pistola, questo per evitare di dover abblencare i campi in caso
127300030701     c* di errore
127400030701     c                   if        v1cisp = 'P' and v03nsp <> 0 and
127500030701     c                             v03pis <> 0
127600030701     C                   MOVEL     ROVDDC        VIDANS
127700030701     C                   Z-ADD     *ZEROS        V03LNP
127800030701     C                   Z-ADD     *ZEROS        V03NRS
127900030701     C                   Z-ADD     *ZEROS        V03NSP
128000030620     c                   end
128100030701     c* se c'è ind. 90 di errore generico acceso, abblenco il campo del
128200030701     c* barcode se ho spedizione impostata e se sono entrato in modalità
128300030701     c* manuale, questo per evitare di dover abblencare i campi in caso
128400030701     c* di errore
128500030701     c                   if        v1cisp = 'M' and v03nsp <> 0 and
128600030701     c                             v03pis <> 0
128700030701     C                   Z-ADD     *ZEROS        V03pis
128800030701     c                   end
128900030701     c                   end
129000941108     C                   EXFMT     LR50D02
129100030910     C                   SETOFF                                       90
129200030701     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
129300030701     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
129400030701     c* della spedizione
129500030701     c                   if        (v03pis <> 0 and v03nsp <> 0) or
129600030701     c                             (v03pis = 0 and v03nsp = 0)
129700030701     c     v1cisp        comp      'P'                                    73
129800030702     c                   end
129900030702     c                   if        (v03pis <> 0 and v03nsp = 0) or
130000030702     c                             (v03pis = 0 and v03nsp <> 0)
130100030701     c     v03pis        comp      0                                  7373
130200030701     c                   end
130300030618     c* se il campo v03pis è impostato valorizzo i campi della spedizione
130400030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
130500030620     c* ha dato l'errore
130600030618     c                   clear                   ds_pis
130700030623     c                   if        v03pis <> 0
130800030702     c                   move      v03pis        ds_pis
130900030623     c                   if        v03nsp = 0 and
131000030620     c                             v03lnp = 0 and v03nrs = 0
131100030701     C                   MOVEL     ROVDDC        VIDANS
131200030618     c                   move      ds_pisaas     vidans
131300030618     c                   move      ds_pislnp     v03lnp
131400030618     c                   move      ds_pisnrs     v03nrs
131500030618     c                   move      ds_pisnsp     v03nsp
131600030623     c* controllo il check digits
131700030623     c                   exsr      srchkdgt
131800030623     c                   if        *in40
131900030623     c                   iter
132000030618     c                   end
132100030623     c                   else
132200030623     c                   move      vidans        com02             2 0
132300030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
132400030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
132500030623     c                   seton                                        5190
132600030623     c                   iter
132700030623     c                   end
132800030623     c                   end
132900030623     c                   end
133000950117     C*  Imposto variabili x test forzature
133100950117     C     VIDANS        IFNE      SAVAAS
133200950117     C     V03LNP        ORNE      SAVLNP
133300950117     C     V03NSP        ORNE      SAVNSP
133400950117     C     V03NRS        ORNE      SAVNRS
133501160502     C                   MOVE      'N'           WFZFFD            1
133600950117     C                   MOVE      'N'           WFZAPP            1
133700950607     C                   MOVE      'N'           WFZTUR            1
133800100203     C                   MOVE      'N'           WFZatt            1
133900150929     C                   MOVE      'N'           WFZCAlert         1
133901160502     C                   MOVE      'N'           WFZCtur           1
134000031119     c                   clear                   k17
134100110803     c                   clear                   k08
134200950117     C                   END
134300950117     C*
134400911108     C   KL              DO
134500911108     C* 1° VOLTA
134600911108     C     CTR           IFEQ      *ZEROS
134700911108     C                   ADD       1             CTR               1 0
134800911108     C                   SETON                                        9094
134900911108     C                   ELSE
135000070410     c                   unlock    fidst09l
135100911108     C* 2° VOLTA
135200940202     C                   Z-ADD     4             FVVNPG
135300941107     C                   MOVE      V01NDC        FVVNFV
135400020905     C                   MOVE      V1cLNA        FVVFGS
135500020905     C     KFVV          CHAIN     FNFVV01L                           50
135600940209     C  N50FVVFCF        IFEQ      'A'
135700940202     C                   MOVEL     *BLANKS       FVVFCF
135800940202     C                   END
135900941109     C  N50              UPDATE    FNFVV000
136000000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
136100000515     C                   MOVE      FVVDS         FVVDS2
136200000515     C                   Z-ADD     8             FVVNPG
136300000515     C     KFVV          CHAIN     FNFVV000                           50
136400000515     C     *IN50         IFEQ      *OFF
136500000515     C                   MOVE      FVVDS2        FVVDS
136600000515     C                   Z-ADD     8             FVVNPG
136700000515     C                   UPDATE    FNFVV000
136800000515     C                   END
136900000626     C                   Z-ADD     4             FVVNPG
137000940202     C                   Z-ADD     *ZEROS        CTR
137100940202     C                   EXSR      PULRIC
137200911108     C                   END
137300911108     C                   END
137400911128     C**
137500030703     C** F2  - INserimento spedizione da manuale a pistola o viceversa
137600030703     c**       serve per il posizionamento sulla spedizione o sul barcode
137700030703     C   KB              DO
137800030703     c                   if        v1cisp = 'P'
137900030703     c                   movel     'M'           v1cisp
138000030703     c                   eval      v03f2 = 'F2=Pistola'
138100030703     c                   seton                                        90
138200030703     c                   setoff                                       7336
138300030703     c                   else
138400030703     c                   movel     'P'           v1cisp
138500030703     c                   eval      v03f2 = 'F2=Manuale'
138600030703     c                   seton                                        907336
138700030703     c                   end
138800030703     C                   ENDdo
138900030703     C**
139000030703     C** F7  - INTERROGAZIONE BOLLE ARRIVI
139100911128     C   KG              DO
139200941109     C                   MOVEL     '2'           P36FLG
139300941108     C                   Z-ADD     VIDANS        P36AAS
139400941108     C                   Z-ADD     V03LNP        P36LNP
139500941108     C                   Z-ADD     V03NSP        P36NSP
139600941108     C                   Z-ADD     V03NRS        P36NRS
139700941109     C                   MOVEL     ' '           P36F03
139800941109     C     P36NSP        IFGT      0
139900941109     C                   MOVEL     '1'           P36FLG
140000941109     C                   END
140100941108     C                   MOVEL     DSLR36        KPJBU
140200941108     C                   CALL      'FNLR36R'
140300911128     C                   PARM                    KPJBA
140400941109     C                   MOVEL     KPJBU         DSLR36
140500941109     C     P36F03        IFNE      '1'
140600941215     C     V03NSP        ANDEQ     0
140700941109     C                   Z-ADD     P36AAS        VIDANS
140800941109     C                   Z-ADD     P36LNP        V03LNP
140900941109     C                   Z-ADD     P36NRS        V03NRS
141000941109     C                   Z-ADD     P36NSP        V03NSP
141100941109     C                   END
141200911128     C                   SETON                                        90
141300911128     C                   END
141400911017     C*
141500941109     C** F13 - BLOCCO SBLOCCO SPEDIZIONI
141600920831     C   KM              DO
141700031017     c                   clear                   dslr02
141800030630     c                   exsr      srfnlr39
141900031017     C                   SETON                                        90
142000920831     C                   END
142100920206     C*
142200031125     C** F17 - ABILITAZIONE MERCE (forzatura non abilita più)
142300031119     c   kr              z-add     1             k17               1 0
142400110803     C** F08 - forzatura immissione in distinta x sped. perfect order
142500110803     c   kh              z-add     1             k08               1 0
142600920206     C*
142700941108     C* F12 Videata precedente
142800911128     C  NKL
142900911128     CANN90              DO
143000940202     C                   Z-ADD     *ZEROS        CTR
143100911017     C*
143200911017     C* Controllo se la spedizione è esistente o valida su video
143300911017     C*
143400911017     C     VIDANS        IFEQ      *ZEROS
143500911017     C                   SETON                                        1790
143600911017     C                   END
143700911017     C     V03LNP        IFEQ      *ZEROS
143800911017     C                   SETON                                        1490
143900911017     C                   END
144000911017     C     V03NSP        IFEQ      *ZEROS
144100911127     C                   SETON                                        2990
144200911017     C                   END
144300911017     C* N90 CERCO LA SPEDIZIONE NELL'ARCHIVIO ARB00 E ARB50
144400911017     C*
144500911023     C  N90              DO
144600911017     C                   Z-ADD     VIDANS        ARBAAS
144700911017     C                   Z-ADD     V03LNP        ARBLNP
144800911017     C                   Z-ADD     V03NRS        ARBNRS
144900911017     C                   Z-ADD     V03NSP        ARBNSP
145000030909     C* AGGANCIO FNARB
145100940207     C                   SETOFF                                       86
145200941108     C     KARB1         CHAIN(N)  FNARB01L                           15
145300030623     C  N15              DO
145400030909     c* se non ci sono errori controllo che la spedizione non sia già
145500030909     c* stata inserita in QUESTA distinta
145600030909     C                   if        not *in90
145700030909     C     KARB1         CHAIN(N)  FNARBW2L
145800030909     c                   if        %found(fnarbw2l)
145900030909     c                   seton                                        5790
146000030909     c                   end
146100030909     c                   end
146200941108     C*  Reperisco Tipo Bolla
146300941108     C                   Z-ADD     1             X                 2 0
146400941108     C                   CLEAR                   DS3A
146500941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
146600941108     C     *IN13         IFEQ      '1'
146700941109     C                   MOVEL     D3A(X)        DS3A
146800941108     C                   END
146900940322     C     *IN90         IFEQ      '0'
147000940322     C     ARBNDC        ANDNE     *ZEROS
147100941107     C     ARBNDC        ANDNE     V01NDC
147200930621     C     ARBNMI        ANDEQ     0
147300141112     c*
147400031125     c                   clear                   fnlr98ds
147500031125     c                   z-add     arbaas        i98aas
147600031125     c                   z-add     arblnp        i98lnp
147700031125     c                   z-add     arbnsp        i98nsp
147800031125     c                   z-add     arbnrs        i98nrs
147900110125     c                   z-add     arbifp        i98fgs
148000110125     c                   z-add     4             i98npg
148100110125     c                   z-add     arbndc        i98ndc
148200031125     c                   movel     fnlr98ds      kpjbu
148300031125     c                   call      'FNLR98C'
148400031125     c                   parm                    kpjba
148500031125     c                   movel     kpjbu         fnlr98ds
148600031125     c                   if        o98esito <> '0'
148700930520     C                   SETON                                        2490
148800031125     c                   endif
148900930520     C                   END
149000940322     C*
149100941108     C* Controllo se linea di arrivo abilitante
149200911129     C  N90ARBLNA        LOOKUP    LIN                                    70
149300911127     C  N70              SETON                                        2590
149400941108     C* Controllo che la spedizione non sia una mamma
149500110804     C     KARB1         CHAIN     FNLBL02L                           74
149600110804     C  N74              SETON                                        9089
149700110804     C   74              SETOFF                                       89
149800941108     C*----------------------------------------------------*
149900050613     C*  errore per spedizioni V.PACCO
150000050613     c                   if        arbrsd = 'VEDI PACCO' and not *in90
150100050613     c                   seton                                        5890
150200050613     c                   end
150300941108     C* Controlli x bolle da consegnare
150400941108     C     ARBDCM        IFEQ      0
150500941108     C* Controllo il livello di abilitazione merce
150600941108     C  N90ARBAMA        IFEQ      *ZEROS
150700040312     c     arbdam        andeq     0
150800911127     C                   SETON                                        2690
150900911105     C                   END
151000040312     C  N90ARBAMA        IFEQ      *ZEROS
151100040312     c     arbdam        andne     0
151200040312     c     k17           andeq     0
151300040312     C                   SETON                                        6890
151400040312     C                   END
151500110803     c* perfect order: se si inserisce una spedizione avviso + f8 x forzare
151600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
151700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
151800130221     C**N90ARBxco*****   IFne      ' '
151900130221     C  N90ARBxco        IFgt      '0'
152000110803     c     arbntc        andeq     0
152100110803     c     k08           andeq     0
152200110803     C                   SETON                                        6590
152300110803     C                   END
152400941108     C* Controllo che la data di consegna prevista <=  alla data distinta
152500030701     C  n90ARBDCR        IFGT      ROVDDC
152600021015     C     ARBTCR        andne     'P'
152700030701     C                   SETON                                        4490
152800920218     C                   END
152900941108     C* Se data consegna richiesta ha come flag dopo controllo
153000030701     C  n90ARBTCR        IFEQ      'D'
153100920306     C     ARBDCR        ANDEQ     ROVDDC
153200030701     C                   SETON                                        4490
153300920218     C                   END
153301160316     c* se spedizione con giro "VP" vedi pacco DPD, deve esssere
153302160316     c* in una distinta di un autista fittizio
153304160316     c     karb1         chain(n)  fiarg01l
153305160316     c                   if        %found(fiarg01l) and argcgi = §nsdcgi
153306160316     c                             and apdpdd <> 'S'
153307160316     C                   SETON                                        9990
153309160316     c                   end
153400150929      ****
153500030909     C* controllo gli errori che richiedono una forzatura
153600030909     c  n90              do
153700950117     C*----------------------------------------------------*
153800040217     C                   SETOFF                                       4667
153900030909     C     *INKK         IFEQ      '1'
154000950607     C                   MOVEL     'S'           WFZAPP
154100950117     C                   ELSE
154200950607     C     WFZAPP        IFEQ      'N'
154300950607     C     ARBTC1        IFEQ      'A'
154400950607     C     ARBTC2        OREQ      'A'
154500950607     C* Controllo che ci sia la consegna richiesta x consegna
154600950607     C* con appuntamento
154700030909     C     ARBDCR        IFEQ      0
154800950607     C     ARBTCR        ANDEQ     ' '
154900950607     C     ARBHCR        ANDEQ     0
155000040217     c                   exsr      numtel
155100040217     c                   if        §ar5teld = *blank
155200030910     C                   SETON                                        4690
155300030909     c                   leave
155400040217     c                   else
155500040217     c                   seton                                        6790
155600040217     c                   endif
155700950607     C                   END
155800950607     C                   END
155900950607     C*
156000950607     C                   END
156100950607     C                   END
156200030909     C*----------------------------------------------------*
156300030910     C                   SETOFF                                       47
156400030909     C     *INKF         IFEQ      '1'
156500030909     C                   MOVEL     'S'           WFZFFD
156600030909     C                   ELSE
156700030909     C     WFZFFD        IFEQ      'N'
156800030909     C* Controllo se la spedizione è in fermo deposito e lo segnalo
156900030909     C     ARBFFD        IFEQ      'S'
157000051107     C     ARBFFD        oreq      'M'
157100030910     C                   SETON                                        4790
157200030909     c                   leave
157300030909     C                   END
157400030909     C                   END
157500030909     C                   END
157600950607     C*----------------------------------------------------*
157700030910     C                   SETOFF                                       42
157800030909     c     *INKT         IFEQ      '1'
157900950607     C                   MOVEL     'S'           WFZTUR
158000950607     C                   ELSE
158100950607     C     WFZTUR        IFEQ      'N'
158200030909     C     ARBGC1        IFNE      *BLANKS
158300950607     C                   MOVEL     ARBGC1        WGIOR             1
158400950607     C     WGIOR         IFEQ      WGG
158500030910     C                   SETON                                        4290
158600030909     c                   leave
158700950607     C                   END
158800950607     C                   END
158900030909     C     ARBGC2        IFNE      *BLANKS
159000950607     C                   MOVEL     ARBGC2        WGIOR             1
159100950607     C     WGIOR         IFEQ      WGG
159200030910     C                   SETON                                        4290
159300030909     c                   leave
159400950607     C                   END
159500950607     C                   END
159600950607     C                   END
159700950607     C                   END
159800150929     C*----------------------------------------------------*
159900100203     C* controllo se sepdizione ha particolarità cons. di attesa
160000100203     C                   SETOFF                                       48
160100100203     c     *INKn         IFEQ      '1'
160200100203     C                   MOVEL     'S'           WFZatt
160300100203     C                   ELSE
160400100203     C     WFZatt        IFEQ      'N'
160500100203     C     ARBGma        andne     *BLANKS
160600100203     C     ARBTCR        andeq     'P'
160700100203     C     ARBdcr        IFGT      rovddc
160800130429     c                   clear                   tnsd99ds
160900130424     C                   Z-ADD     ROVDDC        D98DFC
161000130424     C                   Z-ADD     2359          D98OFC
161100130424     C                   MOVEL     ' '           D98TLA
161200130424     C                   MOVEL     'A'           D98TBO
161300130424     C                   Z-ADD     ARBAAS        D98AAS
161400130424     C                   Z-ADD     ARBLNP        D98LNP
161500130424     C                   Z-ADD     ARBNRS        D98NRS
161600130424     C                   Z-ADD     ARBNSP        D98NSP
161700130424     C                   CALL      'TNSD99R'
161800130424     C                   PARM                    TNSD99DS
161900130424     c                   if        d98nci <=-24
162000100203     C     arbgma        lookup    s7r                                    48
162100100203     C                   IF        *in48
162200100203     C                   SETON                                        90
162300100203     c                   leave
162400100203     C                   END
162500100203     C                   END
162600100203     C                   END
162700100203     C                   END
162800100203     C                   END
162900150929     C*----------------------------------------------------*
163000150929     C* controllo se mandato un contro ALERT nello stesso giorno
163100150929     C                   SETOFF                                       53
163200150929     c     *INKP         IFEQ      '1'
163300150929     C                   MOVEL     'S'           WFZCAlert
163400150929     C                   ELSE
163500150929     C     WFZCAlert     IFEQ      'N'
163600150929     c                   exsr      contro_alert
163700150929     c   53              leave
163800150929     C                   END
163900150929     C                   END
163901160502     C* controllo se la spedizione è in una zona del secondo turno
163902160502     C                   SETOFF                                       59
163903160502     c     *INKi         IFEQ      '1'
163904160502     C                   MOVEL     'S'           WFZCtur
163905160502     C                   ELSE
163906160502     C     WFZCtur       IFEQ      'N'
163907160502     c                   exsr      contro_turno
163908160502     c   59              leave
163909160502     C                   END
163910160502     C                   END
164000150929     C*----------------------------------------------------*
164100150929      ***
164200030909     c                   enddo
164300150929      **
164400030909     c* imposto i campi salvati per forzatura
164500030909     c                   if         *in42 or *in47 or *in46
164600100203     c                              or *in48
164700030909     C                   z-add     VIDANS        SAVAAS
164800030909     C                   z-add     V03LNP        SAVLNP
164900030909     C                   z-add     V03NSP        SAVNSP
165000030909     C                   z-add     V03NRS        SAVNRS
165100030909     c                   end
165200950117     C*----------------------------------------------------*
165300941108     C* Controllo abilitazione incasso contrassegno e assegnato
165400941108     C  N90ARBACC        IFNE      'S'
165500911127     C                   SETON                                        2790
165600911105     C                   END
165700911105     C*
165800941108     C  N90ARBACA        IFNE      'S'
165900911127     C                   SETON                                        2890
166000911105     C                   END
166100911114     C*
166200941108     C  N90ARBFDC        IFNE      ' '
166300930513     C     ARBDCP        ANDEQ     0
166400911127     C                   SETON                                        2490
166500911114     C                   END
166600030630     c* se c'è un lasciato avviso sblocco in automatico la spedizione
166700030630     c                   IF        arbfbc = 'A'
166800031017     c                   clear                   dslr02
166900031017     c                   eval      dr2vid = 'N'
167000030630     c                   exsr      srfnlr39
167100031017     c                   if        dr2err = *blanks
167200030630     c                   clear                   arbcmc
167300030630     c                   clear                   arbfbc
167400030630     c                   end
167500030630     c                   end
167600941108     C* Controllo se bolla abilitata oppure no alla consegna
167700911127     C  N90ARBFBC        IFNE      *BLANK
167800911127     C                   SETON                                        2290
167900911105     C                   END
168000920124     C*
168100150929     C* Controlla se presente un contro Alert per il giorno stesso FORZABILE
168200150929     C*
168300150929     C*
168400941108     C                   ELSE
168500941108     C*----------------------------------------------------*
168600941108     C* Bolla già consegnata
168700941108     C*----------------------------------------------------*
168800990913     C* Controllo abilitazione incasso contrassegno
168900990913     C  N90ARBACC        IFNE      'S'
169000990913     C                   SETON                                        2790
169100990913     C                   END
169200990913     C* CONTROLLO SE C'E' ASSEGNATO DA INCASSARE
169300990913     C  N90ARBACA        IFNE      'S'
169400990913     C                   SETON                                        2890
169500990913     C                   END
169600920220     C* CONTROLLO SE I FLAG DI INCASSO ICA / ICC SONO DA PRENDERE IN
169700930427     C* CONSIDERAZIONE (NEL CASO DI CONSEGNA PARZIALE O CONSEGNA CON
169800930427     C* RITORNO ALL'INCASSO)
169900940207     C  N24ARBFIP        IFNE      'I'
170000911129     C     ARBICC        IFEQ      ' '
170100941108     C     ARBICA        ANDNE     'R'
170200941108     C*  Controllo se c'è contrassegno
170300941108     C                   Z-ADD     0             AR9CAS
170400941108     C     §3AFCA        IFNE      0
170500051115     C     KARB1         CHAIN     FIAR901L                           08
170600941108     C                   END
170700941108     C     AR9CAS        IFEQ      *ZEROS
170800940207     C                   SETON                                        249086
170900911129     C                   END
171000941108     C                   END
171100911129     C*
171200941108     C  N24ARBICC        IFNE      'R'
171300941108     C     ARBICA        ANDNE     'R'
171400920120     C                   SETON                                        2490
171500920120     C                   END
171600920124     C                   END
171700941108     C*----------------------------------------------------*
171800930428     C                   END
171900941108     C*----------------------------------------------------*
172000941109     C     KARB1         CHAIN     FNLBL01L                           87
172100940207     C     *IN87         IFEQ      '0'
172200950126     C     ARBLNA        ANDEQ     LBLLAP
172300941108     C     KLBL          CHAIN(N)  FNARB01L                           87
172400980429     C*
172500980429     C     *IN87         IFEQ      '0'
172600980429     C     ARBCCA        ANDEQ     '2'
172700980429     C     *IN87         OREQ      '0'
172800980429     C     ARBCCA        ANDEQ     '6'
172900980429     C                   SETON                                        87
173000980430     C     KARB2         CHAIN     FNARB01L                           30
173100980429     C                   END
173200940208     C*
173300941108     C     *IN87         IFEQ      '0'
173400941108     C*  Reperisco Tipo Bolla
173500941108     C                   Z-ADD     1             X                 2 0
173600941108     C                   CLEAR                   DS3A
173700941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
173800941108     C     *IN13         IFEQ      '1'
173900941109     C                   MOVEL     D3A(X)        DS3A
174000941108     C                   END
174100941108     C*  Controllo se c'è contrassegno
174200941108     C                   Z-ADD     0             AR9CAS
174300941108     C     §3AFCA        IFNE      0
174400051115     C     KARB1         CHAIN     FIAR901L                           08
174500941108     C                   END
174600940208     C*  Controllo se la mamma è già stata consegnata ma non ancora
174700940208     C*  contabilizzata
174800941108     C     ARBICC        IFEQ      'S'
174900941108     C     ARBNMI        ANDEQ     0
175000941108     C     AR9CAS        ANDNE     *ZEROS
175100941108     C     ARBICC        OREQ      'I'
175200940208     C     ARBNMI        ANDEQ     0
175300941108     C     AR9CAS        ANDNE     *ZEROS
175400940208     C                   SETON                                        9038
175500940208     C                   END
175600941108     C                   END
175700940208     C*
175800950223     C  N87
175900950223     CANN38ARBICC        IFEQ      *BLANKS
176000941108     C     AR9CAS        ANDNE     0
176100941108     C     ARBICC        OREQ      'R'
176200941108     C     AR9CAS        ANDNE     0
176300940208     C   86              SETOFF                                       9024
176400940208     C*  Controllo se la mamma della spedizione inserita ha un'altra
176500940208     C*  figlia inserita in un'altra distinta
176600941108     C     KARB1         CHAIN     FNLBL02L                           85
176700940208     C     *IN85         DOWEQ     '0'
176800940208     C     *IN39         ANDEQ     '0'
176900940208     C*   Se ho trovato una figlia diversa dall' originale mi posiziono
177000940208     C*   su di essa e controllo se ha impostato il flag di ditinta chiusa
177100940208     C     VIDANS        IFNE      LBLAAN
177200940208     C     V03LNP        ORNE      LBLLPN
177300940208     C     V03NRS        ORNE      LBLNRN
177400940208     C     V03NSP        ORNE      LBLNSN
177500100514     C     KARB2         CHAIN(N)  FNARB01L                           84
177600940208     C     *IN84         IFEQ      '0'
177700940208     C     ARBNDC        ANDNE     0
177800940208     C     ARBFDC        ANDEQ     ' '
177900940208     C                   SETON                                        3990
178000940208     C                   END
178100940208     C                   END
178200940208     C*   Leggo la figlia successiva
178300940208     C     *IN39         IFEQ      '0'
178400940208     C                   Z-ADD     LBLAAP        ARBAAS
178500940208     C                   Z-ADD     LBLLPP        ARBLNP
178600940208     C                   Z-ADD     LBLNRP        ARBNRS
178700940208     C                   Z-ADD     LBLNSP        ARBNSP
178800941108     C     KARB1         READE     FNLBL02L                               85
178900940208     C                   END
179000940208     C                   END
179100940208     C*   reimposto la chiave per il posizionamento sulla figlia originale
179200940208     C                   Z-ADD     VIDANS        LBLAAN
179300940208     C                   Z-ADD     V03LNP        LBLLPN
179400940208     C                   Z-ADD     V03NRS        LBLNRN
179500940208     C                   Z-ADD     V03NSP        LBLNSN
179600940208     C                   END
179700940208     C*
179800941108     C* Riaggancio la figlia
179900941108     C     KLBL1         CHAIN(N)  FNARB01L                           87
180000920324     C                   END
180100941108     C                   END
180200930429     C*
180300941108     C* Per record esistente controllo se è valido alla consegna
180400941108     C* controllo se ho già caricato al file di lavoro
180500911127     C  N90              DO
180600940322     C*
180700940322     C*  Richiamo routine per gestione attribuzione Stop
180800940322     C                   EXSR      GESSTP
180900941108     C     KARB1         CHAIN(N)  FNARBW2L                           52
181000911113     C                   MOVE      ' '           ARBFLG
181100930615     C   52
181200930615     CANN15              ADD       1             NUM
181300930615     C   52
181400930615     CANN15              Z-ADD     NUM           ARBNGD
181500941108     C   52
181600941108     CANN15              WRITE     FNARBW00
181700911105     C                   SETOFF                                       23
181800911105     C                   END
181900911017     C*
182000911023     C                   END
182100911022     C*
182200941108     C* Per F10 vado in visualizzazione subfile
182300941108     C   KJ              DO
182400911106     C                   SETOFF                                       90
182500941108     C                   FEOD      FNARBW2L
182600911024     C                   EXSR      GESSBF
182700911106     C                   SETOFF                                       93
182800950208     C   81              GOTO      EFM3
182900911024     C                   END
183000911017     C                   END
183100911108     C   94
183200911108     CAN KL
183300911108     CORNKL              END
183400911017     C*
183500911024     C     EFM3          TAG
183600911017     C                   ENDSR
183700040217     C*-----------------------------------------------------*
183800040217     C* controllo numero di telefono se appuntamento
183900040217     C*-----------------------------------------------------*
184000040217     C     numtel        BEGSR
184100040217      * rekord GEN riferimenti destinatario
184200040217     C                   clear                   dar5gen
184300040217     C                   move      'GEN'         trcar5
184400040217     C     Kfiar5        CHAIN     FiAR501L                           43
184500040217     C     *IN43         IFEQ      '0'
184600040217     C                   MOVEL     ar5uni        dar5gen
184700040217     C                   endif
184800040217     C                   ENDSR
184900150929     C*-----------------------------------------------------*
185000150929     C* controllo se mandato un CONTRO Alert al CLIENTE
185100150929     C*-----------------------------------------------------*
185200150929     C     contro_alert  BEGSR
185300150929      * rec.EMD   invio Alert e contro Alert
185400150929     C                   setoff                                       53
185500150929     C                   clear                   dar5emd
185600150929     C                   move      'EMD'         trcar5
185700150929     C     Kfiar5        CHAIN     FiAR501L
185800150929     C                   IF        %Found(FiAR501L)
185900150929     C                   MOVEL     ar5uni        dar5emd
186000150929     c                   if        §AR5ICA ='S'
186100150929     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
186200150929     c                   move      §AR5DPC       data8DPC          8 0
186300150929     c                   if        data8DPC = Dateu
186400150929     C                   seton                                        5390
186500150929     C                   end
186600150929     C                   end
186700150929     C                   end
186800150929     C                   endif
186900150929     C                   ENDSR
186901160502     C*-----------------------------------------------------*
186902160502     C* controllo se spedizone è del secondo turno
186903160502     C*-----------------------------------------------------*
186904160502     C     contro_turno  BEGSR
186905160502     c*
186906160502     C                   setoff                                       59
186908160502     C                   if        v1ctpm = 'P' and
186910160502     c                             v1ctur = '1' and *in01
186915160502     c                   clear                   ds03
186916160502     C                   eval      tblcod = '03'
186917160502     C                   movel     v1cfpd        com05             5
186918160502     C                   move      arbznc        com05             5
186919160502     C                   movel(p)  com05         tblkey
186920160502     C     ktab          chain     Tabel00f
186921160502     c                   if        %found(tabel00f)
186922160502     C                   MOVEL     TBLUNI        DS03
186923160502     c                   if        §03TPM = ' ' and
186925160502     c                             §03Turno = '2'
186931160502     C                   seton                                        5990
186933160502     c                   end
186934160502     c                   end
186935160502     c                   end
186946160502     C                   ENDSR
187000911018     C*-----------------------------------------------------*
187100030623     C* controllo check digits
187200911018     C*-----------------------------------------------------*
187300030623     C     srchkdgt      BEGSR
187400030623     c                   movel     ds_pis        barcode
187500030623     C                   clear                   trul28ds
187600030623     c                   eval      i28mod = 'BAR'
187700030623     c                   eval      i28cod = barcode
187800030623     c                   call      'TRUL28R1'
187900030623     c                   parm                    trul28ds
188000030623     c* se reperito barcode imposto i campi in stampa
188100030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
188200030623     c                   seton                                        4090
188300030623     c                   end
188400030623     C                   ENDSR
188500030630     C*-----------------------------------------------------*
188600030630     C* chiamata a fnlr39r
188700030630     C*-----------------------------------------------------*
188800030630     C     srfnlr39      BEGSR
188900030630     C                   Z-ADD     VIDANS        DR2AAS
189000030630     C                   Z-ADD     V03LNP        DR2LNP
189100030630     C                   Z-ADD     V03NRS        DR2NRS
189200030630     C                   Z-ADD     V03NSP        DR2NSP
189300030630     C                   Z-ADD     V1CLNA        DR2FGS
189400030630     C                   MOVEL     '1'           DR2RIC
189500030630     C                   MOVEL     DSLR02        KPJBU
189600030630     C                   CALL      'FNLR39R'
189700030630     C                   PARM                    KPJBA
189800031017     c                   movel     kpjbu         dslr02
189900030630     C                   ENDSR
190000030623     C*-----------------------------------------------------*
190100030623     C* GESTIONE DEL SUBFILE E DELLE VIDEATE A LUI COLLEGATE*
190200030623     C*-----------------------------------------------------*
190300030623     C     GESSBF        BEGSR
190400911018     C*
190500911104     C                   SETON                                        64
190600911106     C                   Z-ADD     *ZEROS        CTR
190700930608     C                   DO        *HIVAL
190800911018     C*
190900941108     C* Pulizia del subfile per il caricamento
191000930608     C  N75
191100930608     CANN18              DO
191200940323     C                   Z-ADD     1             NRRKEY
191300911018     C                   SETON                                        62
191400941108     C                   WRITE     LR50C03
191500911018     C                   SETOFF                                       62
191600911018     C                   Z-ADD     *ZEROS        NRR               4 0
191700911018     C*
191800911018     C                   EXSR      CARSFL
191900911018     C*
192000911104     C     NRR           COMP      *ZEROS                             60
192100911104     C                   SETON                                        61
192200930608     C                   END
192300911023     C*
192400911104     C     PRIMA         TAG
192500930608     C                   DO
192600941108     C                   WRITE     LR50Z03
192700941108     C                   EXFMT     LR50C03
192800911023     C*
192900940322     C                   MOVEL     *BLANKS       $MSG
193000911023     C                   SETOFF                                       767775
193100940322     C                   SETOFF                                       189016
193200911023     C*
193300911023     C*
193400941108     C* F12 Controllo se digitati la prima volta mando un messaggio di
193500941108     C* avvertimento
193600911106     C  N81              Z-ADD     *ZEROS        CTR
193700911106     C   81              DO
193800911106     C* 1° VOLTA
193900911106     C     CTR           IFEQ      *ZEROS
194000911106     C                   ADD       1             CTR               1 0
194100940322     C                   SETON                                        909475
194200940322     C                   MOVEL     ERR(1)        $MSG
194300911106     C                   SETOFF                                       81
194400911106     C                   ELSE
194500070410     c                   unlock    fidst09l
194600911106     C* 2° VOLTA
194700950224     C                   Z-ADD     4             FVVNPG
194800950224     C                   MOVE      V01NDC        FVVNFV
194900020905     C                   Z-ADD     V1cLNA        FVVFGS
195000020905     C     KFVV          CHAIN     FNFVV01L                           50
195100950224     C  N50FVVFCF        IFEQ      'A'
195200950224     C                   MOVEL     *BLANKS       FVVFCF
195300950224     C                   UPDATE    FNFVV000
195400950224     C                   END
195500000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
195600000515     C                   MOVE      FVVDS         FVVDS2
195700000515     C                   Z-ADD     8             FVVNPG
195800000515     C     KFVV          CHAIN     FNFVV000                           50
195900000515     C     *IN50         IFEQ      *OFF
196000000515     C                   MOVE      FVVDS2        FVVDS
196100000515     C                   Z-ADD     8             FVVNPG
196200000515     C                   UPDATE    FNFVV000
196300000515     C                   END
196400000626     C                   Z-ADD     4             FVVNPG
196500000515      *
196600911106     C                   Z-ADD     *ZEROS        CTR
196700911106     C                   EXSR      PULRIC
196800911106     C                   END
196900911106     C                   END
197000911023     C*
197100941108     C* F12 Torno prima videata richieata bolla singola
197200941108     C* F10 Vado videata selezione soedizione
197300941108     C   KJ              SETON                                        93
197400911106     C   81
197500941108     COR KJ              GOTO      ENDGES
197600911104     C* PER COMANDO 22 SPROTEGGO I CAMPI DEL GIRO E DELLO STOP
197700911104     C   KW              SETOFF                                       64
197800911104     C* PER COMANDO 16 ANNULLAMENTO DISTINTA DEVE ESSERE ABILITATO
197900911104     C* SOLO IN CASO DI VARIAZIONE
198000911106     C* PER ANNULLAMNETO CHIEDO CONFERMA
198100911106     C   KQ              DO
198200941108     C                   EXFMT     LR50D03
198300911106     C     V08SNO        IFEQ      'SI'
198400911106     C                   EXSR      ANNUL
198500920423     C  N90              MOVEL     *BLANK        SAVCPC
198600941107     C  N90              MOVEL     *BLANK        V1CCPC
198700941107     C  N90              MOVE      V01NDC        V1CNDP
198800941107     C  N90              MOVEL     *BLANK        V1CNDC
198900940322     C  N90              SETON                                        8175
199000000717     C   90
199100000717     CANN78              MOVEL     ERR(2)        $MSG
199200911106     C                   ELSE
199300911106     C                   SETON                                        90
199400911106     C                   END
199500911106     C                   END
199600911023     C* SE DATO ENTER LEGGO I CAMPI DEL SUBFILE
199700911023     C*
199800930608     C  N81
199900930608     CAN 60
200000930608     CANN90              DO
200100911023     C*
200200911023     C                   SETOFF                                       54
200300911023     C*
200400930608     C                   DO        *HIVAL
200500911023     C*
200600941108     C                   READC     LR50S03                                54
200700930608     C  N54              DO
200800911022     C* CONTROLLO LE OPZIONI INSERITE
200900911018     C*
201000941108     C     V3CSEL        IFEQ      'E'
201100911023     C                   SETON                                        77
201200930608     C                   END
201300911022     C*
201400941108     C     V3CSEL        IFEQ      'I'
201500911023     C                   SETON                                        76
201600930608     C                   END
201700911022     C*
201800941108     C     V3CSEL        IFEQ      'V'
201900941108     C                   MOVEL     '1'           P36FLG
202000941108     C                   Z-ADD     V3CAAS        P36AAS
202100941108     C                   Z-ADD     V3CLNP        P36LNP
202200941108     C                   Z-ADD     V3CNSP        P36NSP
202300941108     C                   Z-ADD     V3CNRS        P36NRS
202400941108     C                   MOVEL     DSLR36        KPJBU
202500941108     C                   CALL      'FNLR36R'
202600911105     C                   PARM                    KPJBA
202700911022     C*
202800911022     C* MUOVO I PARAMETRI CHE SERVONO
202900911022     C* FACCIO LA CALL AL PROGRAMMA DI VISUALIZZAZIONE
203000930608     C                   END
203100911022     C*
203200940322     C     *IN76         IFEQ      '1'
203300940322     C     *IN77         ANDEQ     '1'
203400940323     C                   SETON                                        7518
203500940322     C                   MOVEL     ERR(3)        $MSG
203600940322     C                   END
203700940322     C  N76
203800940322     CANN77              SETOFF                                       18
203900940322     C*
204000940322     C*  Controllo gli Stop !!!
204100940322     C                   EXSR      CTRSTP
204200110804     c* perfect order
204300130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
204400130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
204500130221     c*****v3cxco        comp      ' '                                8888
204600130221     c     v3cxco        comp      '0'                                88
204700911023     C*
204800941108     C                   UPDATE    LR50S03
204900911022     C*
205000930608     C                   END
205100930608     C  N54              END
205200911023     C*
205300911023     C* SE NON CI SONO ERRORI
205400911021     C*
205500940322     C  N75
205600940322     CANN18              Z-ADD     0             NRR
205700940322     C  N75
205800940322     CANN18              DO        *HIVAL
205900911023     C                   ADD       1             NRR
206000911018     C*
206100941108     C     NRR           CHAIN     LR50S03                            54
206200911018     C*
206300930608     C  N54              DO
206400911018     C* CONTROLLO LE OPZIONI INSERITE
206500941109     C                   MOVE      V3CAAS        ARBAAS
206600941109     C                   MOVE      V3CLNP        ARBLNP
206700941109     C                   MOVE      V3CNRS        ARBNRS
206800941109     C                   MOVE      V3CNSP        ARBNSP
206900941109     C*
207000941108     C     V3CSEL        IFEQ      ' '
207100930608     C     *IN76         IFEQ      '1'
207200911023     C* CANCELLO IL RECORD DI LAVORO
207300941108     C     KARB1         DELETE    FNARBW2L                           55
207400911023     C                   END
207500941109     C*
207600930608     C     *IN77         IFEQ      '1'
207700940322     C     *IN77         OREQ      '0'
207800940322     C     *IN76         ANDEQ     '0'
207900911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
208000941108     C     KARB1         CHAIN     FNARBW2L                           55
208100941109     C     *IN55         IFEQ      '0'
208200941109     C                   Z-ADD     V3CNGD        ARBNGD
208300941109     C                   Z-ADD     V3CSTP        ARBSTP
208400941109     C                   MOVE      '1'           ARBFLG
208500941109     C                   UPDATE    FNARBW2
208600930608     C                   END
208700941109     C                   END
208800940322     C                   END
208900911023     C*
209000941108     C     V3CSEL        IFEQ      'E'
209100141112     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
209200141112     c                   clear                   fnlr98ds2
209300141112     c                   z-add     arbaas        i982aas
209400141112     c                   z-add     arblnp        i982lnp
209500141112     c                   z-add     arbnsp        i982nsp
209600141112     c                   z-add     arbnrs        i982nrs
209700141112     c                   call      'FNLR98R2'
209800141112     c                   parm                    fnlr98ds2
209900141112     c                   if        o982esito = '0'
210000911023     C* CANCELLO IL RECORD DI LAVORO
210100941108     C     KARB1         DELETE    FNARBW2L                           55
210200930623     C* AGGIORNO LE EVENTUALI ANOMALIE SULLA BOLLA
210300931227     C  N55              EXSR      ANOM
210400141112     c                   endif
210500911023     C*
210600930608     C                   END
210700911023     C*
210800941108     C     V3CSEL        IFEQ      'I'
210900911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
211000941108     C     KARB1         CHAIN     FNARBW2L                           55
211100941109     C  N55              Z-ADD     V3CNGD        ARBNGD
211200941109     C  N55              Z-ADD     V3CSTP        ARBSTP
211300941109     C  N55              MOVE      '1'           ARBFLG
211400941108     C  N55              UPDATE    FNARBW2
211500930608     C                   END
211600911018     C*
211700930608     C                   END
211800930608     C  N54              END
211900930608     C                   END
212000911108     C*
212100911108     C* PULIZIA DEL SUBFILE PER IL CARICAMENTO
212200911108     C*
212300941110     C   KF
212400941110     CANN75              DO
212500911108     C                   SETON                                        62
212600941108     C                   WRITE     LR50C03
212700911108     C                   SETOFF                                       62
212800911108     C                   Z-ADD     *ZEROS        NRR               4 0
212900911108     C*
213000940323     C                   Z-ADD     1             NRRKEY
213100911108     C                   EXSR      CARSFL
213200911108     C* PER COMANDO 1 RICARICO IL SUBFILE ED EMETTO FORMATO DI TOTALE
213300911108     C                   SETOFF                                       06
213400911108     C                   EXSR      GESFM6
213500941108     C                   WRITE     LR50Z03
213600911108     C                   END
213700930608     C                   END
213800930608     C  N81              END
213900911018     C*
214000911104     C     ENDGES        ENDSR
214100911018     C*-----------------------------------------------------*
214200070309     C* aggiorno file distinte FIDST00F                     *
214300911018     C*-----------------------------------------------------*
214400070309     C     srfidst       BEGSR
214500070309     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
214600070309     c* se data distinte automatiche < oggi p.o. partito
214700090310     c*                  if        comdda <> 0
214800070309     C* F I D S T 0 0 F
214900070309     c                   clear                   fidstds
215000070312     c                   eval      wnfv = fvvnfv
215100070410     C     KDST2         CHAIN     fidst09l
215200120718     C                   eval      £dstpdr = apdpdr
215300070309     C                   MOVEL     'A'           £DSTtipa
215400120718     c                   eval      £dstnot = d55not
215500070309     C                   eval      £dstnpg = fvvnpg
215600070309     C                   eval      £dstfgs = fvvFGS
215700070309     C                   eval      £DSTNfv = fvvnfv
215800070309     C                   eval      £dstDFV = fvvdfv
215900070309     C                   MOVEL     FVVFSC        £dstFca
216000070309     C                   MOVEL     FVVFPP        £dstFPP
216100070309     C                   MOVEL     FVVPEP        £dstpEP
216200080121     C                   MOVEL     v1cpda        £dstpda
216300080208     C                   MOVEL     v1ctstpda     §dsttstpda
216500070309     C                   MOVEL     FVVspn        dfvvspn
216600070309     C                   MOVEL     §fvvtpm       £DSTTPM
216601160429     C                   MOVEL     §fvvturno     §DSTturno
216602160429     C                   MOVEL     ddstflr       £dstflr
216700070410     c                   if        %found(fidst09l)
216800080121     c* allineo i giri consegne
216900070312     C     KDST1         setll     FiDST31l
217000070312     c                   do        *hival
217100070312     C     KDST1         reade     FiDST31l
217200070312     c                   if        %eof(fidst31l)
217300070312     c                   leave
217400070312     c                   end
217500070316     c* annullamento
217600120718     c                   if        £dstatb <> ' '
217700070312     C                   eval      dst3ATB = 'A'
217800070312     c                   else
217900070316     c* ripristino
218000070312     C                   CLEAR                   DST3ATB
218100070312     c                   end
218200070312     C                   UPDATE    FiDST300
218300070312     c                   enddo
218400080121     c* allineo i giri ORM
218500080121     c                   if        §OGDDAO <> ' '
218600080121     C     KDST1         setll     FiDST51l
218700080121     c                   do        *hival
218800080121     C     KDST1         reade     FiDST51l
218900080121     c                   if        %eof(fidst51l)
219000080121     c                   leave
219100080121     c                   end
219200080121     c* annullamento
219300120718     c                   if        £dstatb <> ' '
219400080121     C                   eval      dst5ATB = 'A'
219500080121     c                   else
219600080121     c* ripristino
219700080121     C                   CLEAR                   DST5ATB
219800080121     c                   end
219900080121     C                   UPDATE    FiDST500
220000080121     c                   enddo
220100080121     c                   end
220200070517     C                   update    Fidst000
220300070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f
220400070517     c                   exsr      srfidg37
220500070309     c                   else
220600070316     c* INSERIMENTO
220700070316     c*
220800070316     c* aggancio estensione anagrafico aut. per giri
220900070316     c     kapd21        setgt     fiapd22l
221000070316     c     kapd2         readpe    fiapd22l
221100070316     c                   if        not %eof(fiapd22l) and apd2atb = ' ' and
221200070316     c                             fvvdfv>=apd2dde and fvvdfv<=apd2dscad
221300070316     c                   else
221400070316     c* se aut. non gestisce giri pulisco gli  archivi
221500070316     c                   clear                   fiapd2ds
221600070316    1C                   end
221700070309     c                   movea     tc1           tc1d
221800070309     c                   movea     ts1           ts1d
221900070309     c                   if        apd2ftc = ' '
222000070312     c                   eval      £dstftc = 'E'
222100070309     c                   else
222200070312     c                   eval      £dstftc = apd2ftc
222300070309     c                   end
222400070309     c                   if        apd2fts = ' '
222500070312     c                   eval      £dstfts = 'E'
222600070309     c                   else
222700070312     c                   eval      £dstfts = apd2fts
222800070309     c                   end
222900070312     c                   if        apd2vlu = 0
223000070312     c                   eval      £dstvlu = apdvlu
223100070312     c                   else
223200070312     c                   eval      £dstvlu = apd2vlu
223300070312     c                   end
223400070312     c                   if        apd2pkl = 0
223500070312     c                   eval      £dstpkl = apdpkl
223600070312     c                   else
223700070312     c                   eval      £dstpkl = apd2pkl
223800070312     c                   end
223900070312     c                   eval      £dstnbl = apd2nbl
224000070312     c                   eval      £dstnftl= apd2nftl
224100070312     c                   eval      £dstpsf = apd2psf
224200070312     c                   eval      £dsttol = apd2tol
224300070312     c                   if        apd2ftc = ' '
224400070312     c                   eval      £dstinc = 'S'
224500070309     c                   else
224600070312     c                   eval      £dstinc = apd2inc
224700070309     c                   end
224800080121     c                   if        §OGDDAO <> ' '
224900080121     c                   eval      £dstrvlu = apd2rvlu
225000080121     c                   eval      £dstrpkl = apd2rpkl
225100080121     c                   eval      £dstrnbl = apd2rnbl
225200080121     c                   eval      £dstrnftl= apd2rnftl
225300080121     c                   eval      £dstrtol = apd2rtol
225400080121     c                   if        apd2rinc = ' '
225500080121     c                   eval      £dstrinc = 'S'
225600080121     c                   else
225700080121     c                   eval      £dstrinc = apd2rinc
225800080121     c                   end
225900080121     c                   if        apd2rspi = ' '
226000080121     C*???
226100080121     c                   eval      £dstrspi = 'N'
226200080121     c                   else
226300080121     c                   eval      £dstrspi = apd2rspi
226400080121     c                   end
226500080121     c                   else
226600080121     c                   clear                   £dstrvlu
226700080121     c                   clear                   £dstrpkl
226800080121     c                   clear                   £dstrnbl
226900080121     c                   clear                   £dstrnftl
227000080121     c                   clear                   £dstrtol
227100080121     c                   clear                   £dstrinc
227200080121     c                   clear                   £dstrspi
227300080121     c                   end
227400080121     c                   eval      £DSTrPKLA=0
227500080121     c                   eval      £DSTrVLUA=0
227600080121     c                   eval      £DSTrSTPA=0
227700080121     c                   eval      £DSTrnblA=0
227800080121     c                   eval      £DSTrnbaA=0
227900070309     c* scrivo i giri
228000070309     c                   clear                   fidst300
228100070312     c                   eval      dst3fgs = fvvfgs
228200070312     c                   eval      dst3npg = fvvnpg
228300070312     c                   eval      dst3dfv = fvvdfv
228400070312     c                   eval      dst3nfv = fvvnfv
228500070309     c     apd2nras      setll     fiapd31l
228600070309     c                   do        *hival
228700070309     c     apd2nras      reade     fiapd31l
228800070309     c                   if        %eof(fiapd31l)
228900070309     c                   leave
229000070309     c                   end
229100070309     c                   eval      dst3fgsg= apd3fgsg
229200070309     c                   eval      dst3cgi = apd3cgi
229300070309     c                   eval      dst3prgg= apd3prgg
229400070309     c                   eval      dst3sql = apd3sql
229500070725     c                   clear                   fidg09ds
229600070725     c                   eval      d09ifgs = apd3fgsg
229700070725     c                   eval      d09icgi = apd3cgi
229800070725     c                   eval      D09ITUG = 'C'
229900070725     c                   exsr      srcgi
230000070725     c                   if        d09oerr <>'1'
230100070725     c                   write     fidst300
230200070725     c                   end
230300070309     c                   enddo
230400080121     c* scrivo i giri ORM
230500080121     c                   if        §OGDDAO <> ' '
230600080121     c                   clear                   fidst500
230700080121     c                   eval      dst5fgs = fvvfgs
230800080121     c                   eval      dst5npg = fvvnpg
230900080121     c                   eval      dst5dfv = fvvdfv
231000080121     c                   eval      dst5nfv = fvvnfv
231100080121     c     apd2nras      setll     fiapd51l
231200080121     c                   do        *hival
231300080121     c     apd2nras      reade     fiapd51l
231400080121     c                   if        %eof(fiapd51l)
231500080121     c                   leave
231600080121     c                   end
231700080121     c                   if        apd5atb <> ' '
231800080121     c                   iter
231900080121     c                   end
232000080121     c                   eval      dst5fgsg= apd5fgsg
232100080121     c                   eval      dst5cgi = apd5cgi
232200080121     c                   eval      dst5prgg= apd5prgg
232300080121     c                   eval      dst5sql = apd5sql
232400080121     c                   clear                   fidg09ds
232500080121     c                   eval      d09ifgs = apd5fgsg
232600080121     c                   eval      d09icgi = apd5cgi
232700080121     c                   eval      D09ITUG = 'R'
232800080121     c                   exsr      srcgi
232900080121     c                   if        d09oerr <>'1'
233000080121     c                   write     fidst500
233100080121     c                   end
233200080121     c                   enddo
233300080121     c                   end
233400070517     C                   WRITE     Fidst000
233500070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f (deve prima essere
233600070517     c* scritto il record su fidst)
233700070517     c                   exsr      srfidg37
233800070309     c                   end
233900090310     c*                  end
234000080506     c* chiamo il pgm per aggiornamento dati pda per chiudere in lr
234100080506     c                   if        §OGPDAcon <> ' ' and
234200080506     c                             (v1cpda =  'C' or v1cpda = 'E' or
234300080506     c                             v1ctstpda = 'C' or
234400080506     c                             v1ctstpda = 'E')
234500080506     c                   eval      co42tla = 'C'
234600080506     c                   exsr      srpda
234700080506     c                   end
234800070309     c*
234900070309     C                   ENDSR
235000070725     C**************************************************************************
235100070725     C* controllo e decodifico giro
235200070725     C**************************************************************************
235300070725     C     Srcgi         BEGSR
235400070725     C*
235500070725     c                   eval      D09IOP0 = '001'
235600070725     c                   move      fvvdfv        d09idat
235700070725     c                   eval      kpjbu = fidg09ds
235800070725     c                   call      'FIDG09R'
235900070725     c                   parm                    kpjba
236000070725     c                   eval      fidg09ds = kpjbu
236100070725     C*
236200070725     C                   ENDSR
236300070309     C*-----------------------------------------------------*
236400070517     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
236500070309     C*-----------------------------------------------------*
236600070517     C     srfidg37      BEGSR
236700070328     c*
236800070517     c                   clear                   fidg37ds
236900070517     c                   eval      D37FGSI = fvvfgs
237000070517     c                   eval      D37DFVI = fvvdfv
237100070517     c                   eval      D37NFVI = fvvnfv
237200070517     c                   eval      D37cmtI = 'S'
237300070517     c                   eval      kpjbu = fidg37ds
237400070517     c                   call      'FIDG37C'
237500070517     c                   parm                    kpjba
237600070517     c                   eval      fidg37ds = kpjbu
237700070328     c*
237800070328     C                   ENDSR
237900070328     C*-----------------------------------------------------*
238000070328     C* Controllo che le spedizioni non siano già chiuse    *
238100070328     C*-----------------------------------------------------*
238200070328     C     Ctrarb        BEGSR
238300911018     C*
238400070309     C                   Z-ADD     ROVDDC        KDDC
238500070309     C                   Z-ADD     V1cLNA        KFGS
238600070309     C                   Z-ADD     V01NDC        KNDC
238700070309     C     KARB          SETLL     FNARB70L
238800070309     C                   DO        *HIVAL
238900070309     C     KARB          READE(n)  FNARB70L
239000070309     c                   if        %eof(fnarb70l)
239100070309     c                   leave
239200070309     c                   end
239300120518     c                   if        arbfdc = 'S'
239400070309     c                   seton                                        lr
239500070309     c                   return
239600070309     c                   end
239700070309     c                   enddo
239800070309     C*
239900070309     C                   ENDSR
240000070309     C*-----------------------------------------------------*
240100070309     C* CARICAMENTO  SUBFILE  DAL FILE DI LAVORO            *
240200070309     C*-----------------------------------------------------*
240300070309     C     CARSFL        BEGSR
240400070309     C*
240500940323     C                   Z-ADD     0             XX
240600940323     C                   Z-ADD     0             STP
240700911018     C                   Z-ADD     0             V06STP
240800911018     C                   Z-ADD     *ZEROS        V06PKG
240900911018     C                   Z-ADD     *ZEROS        V06VOL
241000911018     C                   Z-ADD     0             V06TSP
241100911018     C                   Z-ADD     *ZEROS        V06TIN
241200990920     C                   Z-ADD     *ZEROS        V06TEN
241300941108     C                   Z-ADD     *ZEROS        V4CTIV
241400941108     C                   Z-ADD     *ZEROS        V3CSTP
241500990920     C* Abblenco divise
241600011009     C                   MOVEL     §GEDCR        V06DVI
241700990920     C                   MOVEL     DIVEUR        V06DVE
241800941108     C     *LOVAL        SETLL     FNARBW1L
241900911018     C*
242000911018     C                   DO        *HIVAL
242100941108     C                   SETOFF                                       55
242200941108     C                   Z-ADD     *ZEROS        COMASS
242300941108     C                   MOVEL     *BLANKS       COMVCA
242400941109     C                   MOVEL     *BLANKS       AR9TIC
242500941108     C                   READ      FNARBW1L                               55
242600911023     C  N55              DO
242700941108     C                   MOVE      ARBNGD        V3CNGD
242800911022     C     ARBSTP        IFNE      *ZEROS
242900940323     C     ARBSTP        LOOKUP    STP                                    13
243000940323     C     *IN13         IFEQ      '0'
243100940323     C                   ADD       1             V06STP
243200961217     C     XX            IFGE      999
243300961217     C                   SETON                                        79
243400961217     C                   MOVEL     ERR(20)       $ERR
243500961217     C                   END
243600961217     C     XX            IFLT      999
243700940323     C                   ADD       1             XX                3 0
243800940323     C                   Z-ADD     ARBSTP        STP(XX)
243900961128     C                   END
244000911018     C                   END
244100940323     C                   END
244200911022     C     ARBSTP        IFEQ      *ZEROS
244300911022     C                   ADD       1             V06STP
244400911022     C                   END
244500941108     C                   MOVE      ARBSTP        V3CSTP
244600941108     C                   MOVE      ARBSTP        V3CPST
244700911105     C*
244800911105     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
244900941108     C     KARB1         CHAIN     FNLBL01L                           54
245000911113     C     *IN54         IFEQ      '0'
245100950126     C     ARBLNA        ANDEQ     LBLLAP
245200941108     C     KLBL          CHAIN(N)  FNARB01L                           56
245300980429     C*
245400980429     C     *IN56         IFEQ      '0'
245500980429     C     ARBCCA        ANDEQ     '2'
245600980429     C     *IN56         OREQ      '0'
245700980429     C     ARBCCA        ANDEQ     '6'
245800980429     C                   SETON                                        5456
245900980430     C     KARB2         CHAIN     FNARB01L                           30
246000980429     C                   END
246100980429     C*
246200941108     C  N56ARBICC        IFEQ      ' '
246300941108     C     ARBICC        OREQ      'R'
246400941108     C*  Controllo il tipo bolla mamma
246500941108     C                   Z-ADD     1             X                 2 0
246600941108     C                   MOVEL     *BLANKS       V3CCBO
246700941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
246800941108     C     *IN13         IFEQ      '1'
246900941109     C                   MOVEL     D3A(X)        DS3A
247000941108     C*  Se bolla mamma con C/Assegno vado a reperirlo
247100941108     C     §3AFCA        IFNE      0
247200051115     C     KLBL          CHAIN     FIAR901L                           56
247300941108     C     *IN56         IFEQ      '0'
247400941108     C                   Z-ADD     AR9CAS        COMASS           13 3
247500941108     C                   MOVEL     AR9VCA        COMVCA            3
247600941109     C                   MOVEL     AR9TIC        COMTIC            2
247700941108     C                   END
247800941108     C                   END
247900941108     C                   END
248000941108     C                   END
248100941108     C*
248200941108     C     KLBL1         CHAIN(N)  FNARB01L                           56
248300941108     C                   ELSE
248400941108     C     KARB1         CHAIN(N)  FNARB01L                           56
248500941108     C                   END
248600941108     C*
248700941108     C*  Controllo il tipo bolla
248800941108     C                   Z-ADD     1             X                 2 0
248900941108     C                   MOVEL     *BLANKS       V3CCBO
249000951121     C                   MOVEL     *BLANKS       WTBOL1            1
249100941108     C                   CLEAR                   DS3A
249200941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
249300941108     C     *IN13         IFEQ      '1'
249400941109     C                   MOVEL     D3A(X)        DS3A
249500941108     C                   MOVEL     §3ATB1        WTBOL1
249600990924     C                   MOVEL     §3ATB2        WTBOL2
249700941108     C                   END
249800951207     C                   MOVE      ARBKSC        V3CCLI
249900951121     C                   MOVEL     ARBRSD        V3CRAG
250000951121     C                   MOVE      ARBIND        V3CRAG
250100911018     C*
250200911105     C  N56              DO
250300911106     C     ARBFAN        IFEQ      'S'
250400941108     C                   MOVEL     'ANOM.'       V3DANO
250500911106     C                   ELSE
250600941108     C                   MOVEL     *BLANKS       V3DANO
250700911106     C                   END
250800941108     C                   MOVEL     ARBCBO        V3CCBO
250900941108     C                   MOVEL     §3ATB1        V3CTB1
251000941108     C                   MOVEL     *BLANK        V3CSEL
251100941108     C                   MOVEL     ARBRSD        V3CRSD
251200941108     C                   MOVEL     ARBIND        V3CIND
251300941108     C                   MOVEL     ARBLOD        V3CLOD
251400941108     C                   MOVEL     ARBPRD        V3CPRD
251500941108     C                   MOVEL     ARBCAD        V3CCAD
251600110804     C                   MOVEL     ARBxco        V3Cxco
251700941108     C                   Z-ADD     ARBAAS        V3CAAS
251800941108     C                   Z-ADD     ARBLNP        V3CLNP
251900941108     C                   Z-ADD     ARBNRS        V3CNRS
252000941108     C                   Z-ADD     ARBNSP        V3CNSP
252100941108     C                   Z-ADD     ARBLNA        V3CLNA
252200941108     C                   Z-ADD     ARBZNC        V3CZNC
252300941108     C                   Z-ADD     ARBNCL        V3CNCL
252400070612     c* peso in base alle regole VDL
252500070612     c                   exsr      calpes
252600070612     C                   Z-ADD     wpes          V3CPKF
252700070612     c* vol. in base alle regole VDL
252800070612     c                   exsr      calvol
252900070612     C                   Z-ADD     wvol          V3CVLF
253000911018     C                   MOVE      ARBDCR        MMGG              4 0
253100911018     C                   MOVEL     MMGG          MM                2 0
253200911018     C                   MOVE      MMGG          GG                2 0
253300941108     C                   MOVE      MM            V3CGMC
253400941108     C                   MOVEL     GG            V3CGMC
253500941108     C                   MOVE      ARBHCR        V3CHCR
253600941108     C                   MOVE      ARBGC1        V3CGC1
253700941108     C                   MOVE      ARBGC2        V3CGC2
253800941108     C                   MOVE      ARBCBO        V3CCBO
253900941108     C                   MOVE      ARBTSP        V3CTSP
254000941108     C                   Z-ADD     *ZEROS        V3CIMA
254100941109     C                   MOVEL     *BLANKS       V3CVCA
254200990920     C                   MOVEL     *BLANKS       V3CVAS
254300941109     C                   MOVEL     *BLANKS       V3CTIC
254400941109     C                   Z-ADD     *ZEROS        V3CCAS
254500941108     C*
254600941108     C* Decodifico i codici consegna particolare
254700941108     C                   MOVEL     *BLANKS       V3DTC1
254800941108     C                   MOVEL     *BLANKS       V3DTC2
254900941108     C                   Z-ADD     1             XY                3 0
255000941108     C     ARBTC1        LOOKUP    C1P(XY)                                23
255100941108     C     *IN23         IFEQ      '1'
255200941108     C                   MOVEL     D1P(XY)       V3DTC1
255300941108     C                   END
255400941108     C                   Z-ADD     1             XY                3 0
255500941108     C     ARBTC2        LOOKUP    C1P(XY)                                23
255600941108     C     *IN23         IFEQ      '1'
255700941108     C                   MOVEL     D1P(XY)       V3DTC2
255800941108     C                   END
255900990920     C*
256000990920     C* Verifico se devo gestire un assegnato
256100990920     C     WTBOL1        IFNE      *BLANKS
256200990920     C     WTBOL2        ORNE      *BLANKS
256300990920     C                   Z-ADD     ARBAAS        KAAS
256400990920     C                   Z-ADD     ARBLNP        KLNP
256500990920     C                   Z-ADD     ARBNRS        KNRS
256600990920     C                   Z-ADD     ARBNSP        KNSP
256700990920     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
256800990920     C     WTBOL2        IFNE      *BLANKS
256900990920     C                   MOVE      '2'           KTRC
257000990920     C* .... se devo gestire 1° bolla imposto tipo record '1'
257100990920     C                   ELSE
257200990920     C     WTBOL1        IFNE      'F'
257300990920     C                   MOVE      '1'           KTRC
257400990920     C                   END
257500990920     C                   END
257600990920     C* Aggancio FIAR6 x importo assegnato
257700990920     C     KAR6          CHAIN     FIAR601L                           74
257800990920     C  N74AR6DFT        IFNE      *ZEROS
257900990920     C* Se ho effettuato un ritorno all'incasso controllo se
258000990920     C     ARBICA        IFEQ      'R'
258100990920     C* ... si riferisce ad un incasso parziale
258200990920     C                   CLEAR                   ARIIPP
258300990920     C                   MOVEL     'A'           KTIP
258400990920     C     KARI          CHAIN     FIARI01L                           74
258500990921     C                   MOVEL     AR6DIV        V3CVAS
258600990921     C                   Z-ADD     AR6IFT        V3CIMA
258700990920     C* ... se si calcolo importo da incassare x differenza
258800990921     C     *IN74         DOWEQ     '0'
258900990921     C     AR6DIV        IFEQ      ARIDIV
259000990921     C                   SUB       ARIIPP        V3CIMA
259100990921     C                   ELSE
259200990921     C* Se la divisa di incasso è diversa converto importo già
259300990921     C* incassato nella divisa della bolla
259400990921     C                   CLEAR                   YEURDS
259500990921     C                   MOVEL     ARIDIV        YECDVI
259600990921     C                   Z-ADD     ARIIPP        YECIMI
259700990927     C                   MOVEL     AR6DIV        YECDVO
259800990921     C                   MOVEL     'H'           YECTAR
259900990921     C                   CALL      'YEURCO'
260000990921     C                   PARM                    YEURDS
260100990921     C                   SUB       YECIMO        V3CIMA
260200990921     C                   END
260300990921     C     KARI          READE     FIARI01L                               74
260400990921     C                   END
260500990920     C                   ELSE
260600990920     C* ... altrimenti imposto totale fattura
260700991202     C     ARBICA        IFEQ      ' '
260800991202     C     ARBICA        OREQ      'R'
260900990920     C                   Z-ADD     AR6IFT        V3CIMA
261000990920     C                   MOVEL     AR6DIV        V3CVAS
261100940323     C                   END
261200991202     C                   END
261300941108     C  N74WTBOL1        IFEQ      'F'
261400990920     C                   MOVE      AR6KSC        V3CCLI
261500941108     C                   MOVEL     'A'           V3CTB1
261600911206     C                   END
261700940323     C                   END
261800941108     C*
261900940323     C                   END
262000941108     C                   MOVEL     ARBRSM        V3CRSM
262100911118     C     ARBICC        IFEQ      ' '
262200941108     C     ARBICC        OREQ      'R'
262300941109     C     §3AFCA        IFNE      0
262400941108     C                   Z-ADD     0             AR9CAS
262500941108     C                   MOVEL     *BLANKS       AR9VCA
262600051115     C     KARB1         CHAIN     FIAR901L                           23
262700941109     C     *IN23         IFEQ      '0'
262800941108     C                   Z-ADD     AR9CAS        V3CCAS
262900941108     C                   MOVEL     AR9VCA        V3CVCA
263000941109     C                   MOVE      AR9TIC        V3CTIC
263100941108     C                   END
263200941109     C                   END
263300941108     C     COMASS        IFNE      *ZEROS
263400941108     C                   Z-ADD     COMASS        V3CCAS
263500941109     C                   MOVEL     COMVCA        V3CVCA
263600941109     C                   MOVE      COMTIC        V3CTIC
263700911105     C                   END
263800911113     C                   END
263900911112     C* SE SPED.DI SELEZIONE PRECEDENTE ILLUMINO IL NUMERO SPEDIZIONE
264000911112     C     ARBFLG        IFEQ      '1'
264100911112     C                   SETON                                        35
264200911112     C                   ELSE
264300911112     C                   SETOFF                                       35
264400911112     C                   END
264500911018     C*
264600000515     C*  Controllo se LE SPEDIZIONI SONO AUTOGENERATE
264700000515     C                   SETOFF                                       63
264800000515     C                   MOVEL     'A'           KTRC
264900060214     C     KAR6          CHAIN     FiAR401L                           95
265000000515     C  N95              MOVEL     AR4NOT        DSBL4A
265100000515     C  N95§B4ASO        COMP      '&'                                    63
265200000515     C*
265300911018     C                   ADD       1             NRR
265400911018     C*
265500991006     C* Accendo indicatori x C/Assegno e assegnato
265600160502     C*    V3CCAS        COMP      0                                      59
265700991006     C     V3CIMA        COMP      0                                      41
265800130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
265900130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
266000130221     C*****V3Cxco        COMP      ' '                                8888
266100130221     C     V3Cxco        COMP      '0'                                88
266200941109     C                   WRITE     LR50S03
266300941108     C                   MOVEL     *BLANKS       V3DANO
266400911018     C*
266500911018     C* CALCOLO I TOTALE DA VIDEATA 6
266600911018     C*
266700070612     C                   ADD       wpes          V06PKG
266800070612     C                   ADD       wvol          V06VOL
266900911018     C                   ADD       1             V06TSP
267000990920     C* Sommo totale da incassare in moneta di conto o altra divisa
267100990920     C* permessa
267200990920     C                   SELECT
267300990920     C     V3CVAS        WHENEQ    *BLANKS
267400011009     C     V3CVAS        OREQ      §GEDCR
267500990920     C                   ADD       V3CIMA        V06TIN
267600990920     C* ... altra divisa valida
267700990920     C     V3CVAS        WHENEQ    DIVEUR
267800990920     C                   ADD       V3CIMA        V06TEN
267900990920     C                   ENDSL
268000990920     C* Somma nel totale corretto: moneta di conto
268100990920     C                   SELECT
268200990920     C     V3CVCA        WHENEQ    *BLANKS
268300011009     C     V3CVCA        OREQ      §GEDCR
268400941108     C                   ADD       V3CCAS        V06TIN
268500990920     C* ... altra divisa valida
268600990920     C     V3CVCA        WHENEQ    DIVEUR
268700990920     C                   ADD       V3CCAS        V06TEN
268800990920     C* ... divise non Unione Europea
268900990920     C                   OTHER
269000941108     C                   ADD       V3CCAS        V4CTIV
269100990920     C                   ENDSL
269200911018     C                   END
269300911105     C*
269400911105     C                   END
269500911018     C*
269600911021     C  N55              END
269700911018     C*
269800911018     C                   ENDSR
269900911018     C*-----------------------------------------------------*
270000911018     C* GESTIONE FORMATO 6 TOTALI E CONFERMA                *
270100911018     C*-----------------------------------------------------*
270200911018     C     GESFM6        BEGSR
270300911018     C*
270400930428     C                   SETOFF                                       0706
270500050204     C                   MOVEL     'N'           V06ban
270600050207     C                   MOVEL     'N'           V06SIN
270700911021     C                   DO        *HIVAL                                       1
270800911018     C*
270900991117     C     V06TIN        COMP      0                                  0808
271000991117     C     V06TEN        COMP      0                                  0909
271100991117     C     V4CTIV        COMP      0                                  5353
271200941108     C                   EXFMT     LR50D04
271300070315     c* prima di aggiornare una distinta controllo che non
271400070315     c* sia stata sbloccata da altri e quindi già chiusa per evitare di
271500070315     c* perdere i riferimenti della chiusura distinta. Nel caso forzo
271600070315     c* l'uscita
271700070315     c                   if        v01ndc <> 0
271800070315     c                   exsr      ctrarb
271900070315     c                   end
272000911018     C*
272100911022     C  N80              DO                                                     2
272200911018     C*
272300941108     C   KF              SETON                                        0706
272400911018     C*
272500941108     C  NKF*IN07         IFEQ      '1'                                          3
272600950907     C   02              MOVEL     '1'           *INKC
272700911018     C* CONTROLLO SE ERO IN MODIFICA OPPURE IN CREAZIONE NUOVA DISTINTA
272800941107     C     V01NDC        IFEQ      *ZEROS                                       4
272900020905     c* prende il numero della distinta del po in gestione
273000020905     c                   do        *hival
273100020905     C                   CLEAR                   TRUL33DS
273200020905     c                   eval      i33tla = 'L'
273300020905     C                   Z-ADD     0             I33AAA
273400020905     C                   Z-ADD     12            I33CNU
273500020905     C                   Z-ADD     V1CLNA        I33PO1
273600020905     C                   Z-ADD     1             I33NUM
273700020905     c                   movel(p)  trul33ds      kpjbu
273800020905     c                   call      'TRUL33R'
273900020905     c                   parm                    kpjba
274000020905     c                   movel     kpjbu         trul33ds
274100020905     C                   IF        O33ERR = 0
274200020905     c* controllo che il numero non è già stato usato
274300020905     c                   clear                   fnlr6ds
274400020905     c                   eval      d6tla = 'L'
274500020905     C                   Z-ADD     8             D6npg
274600020905     C                   Z-ADD     V1CLNA        D6fgs
274700020905     C                   Z-ADD     O33Nrf        D6NDT
274800020905     c                   movel(p)  fnlr6ds       kpjbu
274900020905     c                   call      'FNLR6R'
275000020905     c                   parm                    kpjba
275100020905     c                   movel     kpjbu         fnlr6ds
275200020905     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
275300020905     c* come foglio o in una bolla
275400020905     c                   if        d6flg <> 'N'
275500020905     c                   iter
275600020905     c                   else
275700020905     C                   Z-ADD     O33Nrf        V01NDC
275800020905     c                   leave
275900020905     C                   END
276000020905     C                   END
276100020905     C                   ENDdo
276200020905     C                   MOVEL     V01NDC        V1CNDC
276300941107     C                   Z-ADD     V01NDC        FVVNFV            5 0
276400911018     C*
276500911018     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
276600911021     C*
276700950601     C                   MOVEL     *BLANKS       FVVATB
276800911021     C*
276900941109     C                   MOVEL     V1CLNA        FVVFGS
277000020916     C                   MOVEL     simfel        FVVFLE
277100911021     C*
277200941109     C                   Z-ADD     ROVDDC        FVVDFV
277300941109     C                   MOVEL     V06PAD        FVVDSF
277400941109     C                   MOVEL     *BLANK        FVVFCF
277500941109     C                   Z-ADD     4             FVVNPG
277600941109     C                   MOVE      *BLANK        FVVSPG
277700941109     C                   MOVE      *BLANK        FVVSPN
277800911021     C                   MOVEL     'S'           FVVFSC
277900941109     C                   MOVEL     V1CFPP        FVVFPP
278000950601     C                   MOVE      V1CPEP        FVVPEP
278100050523     c                   clear                   dfvvspn
278200050523     C                   MOVEl     V1Ctpm        §fvvtpm
278300050523     c                   if        v1ctpm = 'P'
278400050523     C                   MOVEl     *blanks       §fvvtpm
278500050523     c                   end
278501160429     C                   MOVEl     V1Ctur        §fvvturno
278502160429     c                   if        v1ctur = '1'
278503160429     C                   MOVEl     *blanks       §fvvturno
278504160429     c                   end
278600050523     c                   movel     dfvvspn       fvvspn
278700000515     C                   WRITE     FNFVV000
278800000515      *SCRIVO FOGLIO TIPO 8
278900000515     C                   Z-ADD     8             FVVNPG
279000000515     C                   WRITE     FNFVV000
279100000515     C                   Z-ADD     4             FVVNPG
279200041007     C* gestione file dei rientri
279300041007     c                   move      v1clna        wlna
279400041007     c                   z-add     v01ndc        wdis
279500041007     C     Kqdt          chain     fiqdt01l                           83
279600041007     c   83              clear                   fiqdt000
279700041007     C                   movel     v1clna        qdtfgs
279800041007     C                   z-add     v01ndc        qdtnfv
279900041007     C                   z-add     codpad        qdtpdr
280000041007     C                   z-add     fvvdfv        qdtddc
280100110311     C   83              z-add     fvvdfv        qdtdaiaf
280200110311     C   83              movel     whhdat        qdtoraaf
280300110311     C   83              move      *zeros        qdtnbnaf
280400110311     C   83              move      apdptp        qdtnbnaf
280500110311     C   83              move      'S'           qdtfnbaf
280600041007     c   83              write     fiqdt000
280700041007     c  n83              update    fiqdt000
280800950601     C*
280900911021     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
281000911021     C* GIRO,STOP E PROGRESSIVO DI STAMPA
281100911021     C*
281200941108     C     *LOVAL        SETLL     FNARBW1L
281300911021     C*
281400030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
281500911021     C                   DO        *HIVAL                                       4
281600911108     C                   SETOFF                                           55
281700941108     C                   READ      FNARBW1L                               55
281800911021     C  N55              DO                                                     5
281900911021     C                   Z-ADD     ARBSTP        WSTP              3 0
282000911021     C                   Z-ADD     ARBNGD        WNGD              3 0
282100080505     c                   clear                   fidg42ds
282200911021     C*
282300941108     C     KARB1         CHAIN     FNARB01L                           56
282400930428     C  N56              DO
282500110127     c* controllo che la spedizione non sia già stata inserita in altra
282600110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
282700110202     c* la vecchia distinta
282800110127     c                   if        arbndc <> 0 and arbndc <> v01ndc
282900110202     c                             and arbicc <> 'R' and arbica <> 'R'
283000110128     c                   seton                                        90
283100110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
283200110202     c                             iornate correttamente. VERIFICARE!!!'
283300110127     c                   iter
283400110127     c                   end
283500980306     C* Controllo se devo scrivere MIC (evento messa in consegna)
283600980310     C     ARBDCM        IFEQ      0
283700980306     C                   EXSR      WRTMIC
283800980310     C                   END
283900980306     C*
284000020905     C                   Z-ADD     V1CLNA        ARBIFP
284100020905     C                   Z-ADD     V01NDC        ARBNDC
284200911021     C                   Z-ADD     ROVDDC        ARBDDC
284300930428     C                   Z-ADD     0             ARBNMI
284400930428     C                   MOVEL     *BLANKS       ARBFDC
284500030722     C                   Z-ADD     0             ARBNSS
284600911021     C                   Z-ADD     WSTP          ARBSTP
284700911021     C                   Z-ADD     WNGD          ARBNGD
284800080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
284900080505     c                   eval      co42tla = 'A'
285000080505     c                   else
285100080505     c                   clear                   co42tla
285200080505     c                   end
285300080505     C                   Z-ADD     CODPAD        ARBPDC
285400911021     C*
285500941109     C                   UPDATE    FNARB000
285600941109     C                   DELETE    FNARBW00
285700030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
285800030910     c* deve andare a legare l'orm alla distinta
285900070227     c*                  if        spunta <> *blank
286000030910     c                   exsr      srorm
286100070227     c*                  endif
286200070319     c* mettro i riferimenti della distinta anche su FIARG x evitare di
286300070319     c* elaborare la spedizione nelle distinte automatiche
286400070319     c                   exsr      srfiarg
286500080410     c* richiamo il pgm per aggiornare i dati per PDA
286600080410     c                   if        §OGPDAcon <> ' ' and
286700080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
286800080411     c                             v1ctstpda = 'C' or
286900080411     c                             v1ctstpda = 'E')
287000080505     c                   if        co42tla = 'A'
287100080505     c                   exsr      srpda
287200080505     c                   end
287300080410     c                   eval      co42tla = 'I'
287400080410     c                   exsr      srpda
287500080410     c                   end
287600911113     C                   END
287700911021     C*
287800930428     C                   END
287900930428     C  N55              END
288000911105     C                   ELSE
288100950601     C*
288200041007     C* gestione file dei rientri
288300041007     c                   move      v1clna        wlna
288400041007     c                   z-add     v01ndc        wdis
288500041007     C     Kqdt          chain     fiqdt01l                           83
288600041007     c   83              clear                   fiqdt000
288700041007     C                   movel     v1clna        qdtfgs
288800041007     C                   z-add     v01ndc        qdtnfv
288900041007     C                   z-add     codpad        qdtpdr
289000041007     C                   z-add     fvvdfv        qdtddc
289100041007     c   83              write     fiqdt000
289200041007     c  n83              update    fiqdt000
289300120718     C*  Aggiorno FiDST
289400120718     c                   exsr      srfidst
289500911128     C*
289600960702     C                   Z-ADD     ROVDDC        KDDC
289700020905     C                   Z-ADD     V1cLNA        KFGS
289800020905     C                   Z-ADD     V01NDC        KNDC
289900020905     C     KARB          SETLL     FNARB70L
290000911105     C*
290100930608     C                   DO        *HIVAL
290200941108     C                   SETOFF                                           50
290300911105     C*
290400020905     C     KARB          READE     FNARB70L                               50
290500911105     C*
290600930428     C  N50              DO
290700911105     C*
290800080318     C     KARB1         CHAIN(N)  FNARBW2L
290900080326     C                   if        not %found(fnarbw2l)
291000980309     C* Se era in consegna e adesso non c'è più annullo la messa
291100980309     C* consegna
291200980309     C     KARB1         CHAIN     FNARB01L                           31
291300110127     c*
291400980310     C     ARBDCM        IFEQ      0
291500980309     C                   EXSR      ANNMIC
291600980310     C                   END
291700950602     C                   Z-ADD     *ZEROS        ARBNSS
291800020905     C                   Z-ADD     *ZEROS        ARBIFP
291900020905     C                   Z-ADD     *ZEROS        ARBNDC
292000911111     C                   Z-ADD     *ZEROS        ARBDDC
292100911111     C                   Z-ADD     *ZEROS        ARBSTP
292200911111     C                   Z-ADD     *ZEROS        ARBNGD
292300941108     C                   Z-ADD     *ZEROS        ARBPDC
292400941109     C  N31              UPDATE    FNARB000
292500030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
292600030910     c* deve andare a togliere la distinta dall'orm
292700030910     c                   exsr      srorm
292800070319     c* tolgo i riferimenti della distinta anche su FIARG x
292900070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
293000070319     c                   exsr      srfiarg
293100080410     c* richiamo il pgm per aggiornare i dati per PDA
293200080410     c                   if        §OGPDAcon <> ' ' and
293300080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
293400080411     c                             v1ctstpda = 'C' or
293500080411     c                             v1ctstpda = 'E')
293600080410     c                   clear                   fidg42ds
293700080410     c                   eval      co42tla = 'A'
293800080410     c                   exsr      srpda
293900080410     c                   end
294000930428     C                   END
294100930428     C                   END
294200911111     C  N50              END
294300911111     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
294400911111     C* GIRO,STOP E PROGRESSIVO DI STAMPA
294500080505     c                   clear                   fidg42ds
294600911111     C*
294700941109     C     *LOVAL        SETLL     FNARBW1L
294800911111     C*
294900030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
295000911111     C                   DO        *HIVAL                                       4
295100911111     C                   SETOFF                                           55
295200941109     C                   READ      FNARBW1L                               55
295300911111     C  N55              DO                                                     5
295400911111     C                   Z-ADD     ARBSTP        WSTP              3 0
295500911111     C                   Z-ADD     ARBNGD        WNGD              3 0
295600911111     C*
295700941109     C     KARB1         CHAIN     FNARB01L                           56
295800930428     C  N56              DO
295900110127     c* controllo che la spedizione non sia già stata inserita in altra
296000110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
296100110202     c* la vecchia distinta
296200110202     c                   if        arbndc <> 0 and arbndc <> v01ndc
296300110202     c                             and arbicc <> 'R' and arbica <> 'R'
296400110128     c                   seton                                        90
296500110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
296600110202     c                             iornate correttamente. VERIFICARE!!!'
296700110127     c                   iter
296800110127     c                   end
296900980309     C* Controllo se devo scrivere MIC (evento messa in consegna)
297000980310     C     ARBDCM        IFEQ      0
297100980309     C                   EXSR      WRTMIC
297200980310     C                   END
297300980309     C*
297400020905     C                   Z-ADD     V1CLNA        ARBIFP
297500020905     C                   Z-ADD     V01NDC        ARBNDC
297600911111     C                   Z-ADD     ROVDDC        ARBDDC
297700930428     C                   Z-ADD     0             ARBNMI
297800930428     C                   MOVEL     *BLANKS       ARBFDC
297900030722     C                   Z-ADD     0             ARBNSS
298000911111     C                   Z-ADD     WSTP          ARBSTP
298100911111     C                   Z-ADD     WNGD          ARBNGD
298200080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
298300080505     c                   eval      co42tla = 'A'
298400080505     c                   else
298500080505     c                   clear                   co42tla
298600080505     c                   end
298700080505     C                   Z-ADD     CODPAD        ARBPDC
298800911111     C*
298900941109     C                   UPDATE    FNARB000
299000941109     C                   DELETE    FNARBW00
299100030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
299200030910     c* deve andare a legare l'orm alla distinta
299300030910     c                   exsr      srorm
299400070319     c* metto i riferimenti della distinta anche su FIARG x evitare di
299500070319     c* elaborare la spedizione nelle distinte automatiche
299600070319     c                   exsr      srfiarg
299700080410     c* richiamo il pgm per aggiornare i dati per PDA
299800080410     c                   if        §OGPDAcon <> ' ' and
299900080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
300000080411     c                             v1ctstpda = 'C' or
300100080411     c                             v1ctstpda = 'E')
300200080505     c                   if        co42tla = 'A'
300300080505     c                   exsr      srpda
300400080505     c                   end
300500080410     c                   eval      co42tla = 'I'
300600080410     c                   exsr      srpda
300700080410     c                   end
300800911114     C                   END
300900911111     C*
301000930428     C                   END
301100930428     C  N55              END
301200140930     C* IN CASO DI VARIZIONE DISTINTA GIA' ESISTENTE
301300140930     C* E SE DA TOGLIERE DALLA DISTINTA
301400140930     C                   Z-ADD     V01NDC        FVVNFV            5 0
301500140930     C                   Z-ADD     V1CLNA        FVVFGS
301600140930     C*
301700140930     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
301800140930     C*
301900140930     C     KFVV          CHAIN     FNFVV01L                           83
302000140930     C  N83              DO
302100140930     C                   MOVEL     *BLANKS       FVVATB
302200140930     C*
302300140930     C                   MOVEL     V1CLNA        FVVFGS
302400140930     C                   MOVEL     simfel        FVVFLE
302500140930     C*
302600140930     C                   MOVEL     V06PAD        FVVDSF
302700140930     C                   MOVEL     *BLANK        FVVFCF
302800140930     C                   Z-ADD     4             FVVNPG
302900140930     C                   MOVEL     'S'           FVVFSC
303000140930     C                   MOVEL     V1CFPP        FVVFPP
303100140930     C                   MOVEL     V1CPEP        FVVPEP
303200140930     C                   MOVEL     fvvspn        dfvvspn
303300140930     C                   MOVEL     v1ctpm        §fvvtpm
303400140930     c                   if        v1ctpm = 'P'
303500140930     C                   MOVEL     *blanks       §fvvtpm
303600140930     c                   end
303601160429     C                   MOVEL     v1ctur        §fvvturno
303602160429     c                   if        v1ctur = '1'
303603160429     C                   MOVEL     *blanks       §fvvturno
303604160429     c                   end
303700140930     C                   MOVEL     dfvvspn       fvvspn
303800140930     C*
303900140930     C                   UPDATE    FNFVV000
304000140930      *GESTIONE FOGLIO TIPO 8
304100140930     C                   MOVE      FVVDS         FVVDS2
304200140930     C                   Z-ADD     8             FVVNPG
304300140930     C     KFVV          CHAIN     FNFVV000                           83
304400140930     C     *IN83         IFEQ      *OFF
304500140930     C                   MOVE      FVVDS2        FVVDS
304600140930     C                   Z-ADD     8             FVVNPG
304700140930     C                   UPDATE    FNFVV000
304800140930     C                   MOVE      FVVDS2        FVVDS
304900140930     C                   END
305000140930     C                   Z-ADD     4             FVVNPG
305100140930     C                   END
305200930428     C                   END
305300070312     c* aggiorno il file FIDST00F
305400070312     c                   exsr      srfidst
305500080320     c* se variato il cod. aut. richiamo pgm orm se pda partito
305600080320     c                   exsr      srchgcpc
305700070312     C*
305800911023     C   55              SETON                                        8081
305900920304     C   50              EXSR      PULRIC
306000911021     C*
306100041008      * richiesta gestione inserimento bancali
306200041008     c                   if        v06ban  = 'S'
306300041008     c                   clear                   fnlrf8ds
306400041008     c                   z-add     fvvnfv        rf8dis
306500041008     c                   z-add     fvvfgs        rf8fgs
306600041008     c                   movel(p)  fnlrf8ds      kpjbu
306700041008     c                   call      'FNLRF8R'
306800041008     c                   parm                    kpjba
306900041008     c                   endif
307000911105     C* SE CHIEDO STAMPA
307100010216     C     V06SIN        IFNE      'N'
307200941109     C                   CLEAR                   FNLR55
307300950102     C                   Z-ADD     ROVDDC        D55DAT
307400020905     C                   Z-ADD     ARBNDC        D55NRD
307500950102     C                   Z-ADD     ARBNDC        D55NRA
307600950102     C                   MOVEL     'A'           D55TIP
307700950102     C                   MOVEL     V1CLNA        D55FIL
307800041007     C*                  Z-ADD     DSTNBN        D55NBN
307900041007     C                   Z-ADD     qdtnbnaf      D55NBN
308000050207     C                   MOVEL     QDTFNBAF      D55fba
308100120718     C                   MOVEL     £DSTNOT       D55NOT
308200010216     C                   MOVEL     V06SIN        D55STP
308300941109     C                   MOVEL     FNLR55        KPJBU
308400050427     C                   CALL      'FNLR55C'
308500911128     C                   PARM                    KPJBA
308600911105     C                   END
308700941107     C                   MOVE      *BLANKS       V1CCPC
308800941109     C                   MOVE      WHHDAT        V1CDDC
308900930428     C                   END
309000911021     C                   END
309100911022     C  N80              END
309200950907     C*
309300950907     C     D50TPR        IFEQ      ' '
309400941107     C                   MOVEL     V01NDC        V1CNDP
309500941107     C                   MOVE      *BLANK        V1CNDC
309600080208     c                   clear                   msgv
309700080318     c                   clear                   msgv1
309800080415     c                   setoff                                       45
309900941110     C                   MOVE      *ZEROS        SAVNDC
310000920423     C                   MOVE      *BLANKS       SAVCPC
310100950608     C                   MOVE      WHHDAT        V1CDDC
310200081028     c* non deve pulire i campi della prima videata se premuto f12
310300081028     c                   if        not *inkl
310400080306     C                   MOVE      *BLANK        V1CPEP
310500080306     C                   MOVE      *blanks       V1Ctpm
310501160429     C                   MOVEl     *blanks       V1Ctur
310600080306     c                   clear                   v1cpda
310700080306     c                   clear                   v1ctstpda
310800080306     c                   end
310900950907     C                   END
311000911021     C*
311100911021     C                   ENDSR
311200980306     C*-----------------------------------------------------*
311300980306     C* WRTMIC: Ctrl se devo inviare "Messa in consegna"    *
311400980306     C*-----------------------------------------------------*
311500980306     C     WRTMIC        BEGSR
311600980306     C*
311700980311     C* Scrivo evento di messa in consegna solo se il numero
311800980311     C* distinta sulla bolla è diverso da quella attuale
311900980311     C* (Sono in fase di immissione speidizione in distinta)
312000131031      **
312100131104     c                   clear                   Devb01
312200131104     C                   move      *zeros        §NOTNDC
312300131104     C                   move      *zeros        §NOTPDC
312400131031     C                   move      V01NDC        §NOTNDC
312500131031     C                   move      CODPAD        §NOTPDC
312600131031      **
312700980311     C     V01NDC        IFNE      ARBNDC
312800980306     C* Controllo se ho già scritto evento MIC
312900980318     C     KARB1         CHAIN     FNLBL01L                           87
313000980318     C     *IN87         IFEQ      '0'
313100980318     C     ARBLNA        ANDEQ     LBLLAP
313200980429     C*
313300980429     C     KLBL          CHAIN     FNARB01L                           30
313400980429     C     *IN56         IFEQ      '0'
313500980429     C     ARBCCA        ANDEQ     '2'
313600980429     C     *IN56         OREQ      '0'
313700980429     C     ARBCCA        ANDEQ     '6'
313800980429     C                   SETON                                        87
313900980429     C                   END
314000980429     C     KARB2         CHAIN     FNARB01L                           30
314100980429     C                   END
314200980429     C*
314300980429     C     *IN87         IFEQ      '0'
314400980429     C     ARBLNA        ANDEQ     LBLLAP
314500980318     C                   Z-ADD     LBLAAP        KAAS
314600980318     C                   Z-ADD     LBLLPP        KLNP
314700980318     C                   Z-ADD     LBLNRP        KNRS
314800980318     C                   Z-ADD     LBLNSP        KNSP
314900980318     C                   ELSE
315000980306     C                   Z-ADD     ARBAAS        KAAS
315100980306     C                   Z-ADD     ARBLNP        KLNP
315200980306     C                   Z-ADD     ARBNRS        KNRS
315300980306     C                   Z-ADD     ARBNSP        KNSP
315400980318     C                   END
315500031017      * verifica se deve scrivere il mic se esiste almeno una spunta lo scrive
315600031017     c                   exsr      eventimic
315700031017     c     spunta        cabeq     *blank        nomic
315800031017
315900980306     C                   MOVEL     'MIC'         KCEV
316000990803     C     KEVB          SETGT     FNEVB04L
316100990803     C     KEVB          READPE    FNEVB04L                               33
316200980311     C* Se l'evento di messa in consegna non esiste o esista ma
316300980311     C* ha data diversa dalla distinta di consegna scrivo un
316400980311     C* nuovo evento di messa in consegna
316500980311     C     *IN33         IFEQ      '1'                                          No Trv Mic
316600990803     C     FVVDFV        ORNE      EVBDEV
316700990803     C                   CLEAR                   FNEVB000
316800980309     C                   MOVEL     'MIC'         EVBCEV
316900980309     C                   Z-ADD     DATEU         EVBDTV
317000980309     C                   TIME                    EVBORV
317100980309     C                   Z-ADD     V1CLNA        EVBFLE
317200020905     C                   Z-ADD     DUTCOU        EVBCDU
317300980318     C                   Z-ADD     KAAS          EVBAAS
317400980318     C                   Z-ADD     KLNP          EVBLNP
317500980318     C                   Z-ADD     KNRS          EVBNRS
317600980318     C                   Z-ADD     KNSP          EVBNSP
317700980309     C                   Z-ADD     FVVDFV        EVBDEV
317800980309     C     FVVFPP        IFEQ      'M'
317900980310     C                   MOVEL     0800          EVBHEV
318000980309     C                   ELSE
318100980310     C                   MOVEL     1200          EVBHEV
318200980309     C                   END
318300131031     C                   MOVEL     Devb01        EVBNOT
318400990803     C                   MOVEL     *BLANKS       EVBDTC
318500990803     C                   WRITE     FNEVB000
318600980311     C                   ELSE                                                   No Trv Mic
318700980311     C* Se l'evento c'è e ha data uguale alla distinta controllo
318800980311     C* se è già stato trasmesso
318900980311     C     EVBDTR        IFEQ      0                                            MIC trasm
319000980311     C* Se non è stato trasmesso aggiorno la data e l'ora di
319100980311     C* di immissione evento con quella del giorno + altri dati
319200980311     C                   Z-ADD     DATEU         EVBDTV
319300980311     C                   TIME                    EVBORV
319400980311     C                   Z-ADD     V1CLNA        EVBFLE
319500020905     C                   Z-ADD     DUTCOU        EVBCDU
319600131031     C                   MOVEL     Devb01        EVBNOT
319700980311     C     FVVFPP        IFEQ      'M'
319800980311     C                   MOVEL     0800          EVBHEV
319900980311     C                   ELSE
320000980311     C                   MOVEL     1200          EVBHEV
320100980311     C                   END
320200990803     C                   MOVEL     *BLANKS       EVBDTC
320300990803     C                   UPDATE    FNEVB000
320400980311     C* Altrimenti scrivo un nuovo evento di messa in consegna
320500980311     C                   ELSE                                                   No Trasm
320600990803     C                   CLEAR                   FNEVB000
320700980311     C                   MOVEL     'MIC'         EVBCEV
320800980311     C                   Z-ADD     DATEU         EVBDTV
320900980311     C                   TIME                    EVBORV
321000980311     C                   Z-ADD     V1CLNA        EVBFLE
321100020905     C                   Z-ADD     DUTCOU        EVBCDU
321200980318     C                   Z-ADD     KAAS          EVBAAS
321300980318     C                   Z-ADD     KLNP          EVBLNP
321400980318     C                   Z-ADD     KNRS          EVBNRS
321500980318     C                   Z-ADD     KNSP          EVBNSP
321600980311     C                   Z-ADD     FVVDFV        EVBDEV
321700980311     C     FVVFPP        IFEQ      'M'
321800980311     C                   MOVEL     0800          EVBHEV
321900980311     C                   ELSE
322000980311     C                   MOVEL     1200          EVBHEV
322100980311     C                   END
322200131031     C                   MOVEL     Devb01        EVBNOT
322300990803     C                   MOVEL     *BLANKS       EVBDTC
322400990803     C                   WRITE     FNEVB000
322500980311     C                   END                                                    Mic Trasm
322600980311     C*
322700980311     C                   END                                                    Trv MIC
322800980309     C*
322900980311     C                   END                                                    ARBNDC<>
323000980309     C*
323100031017     C     nomic         ENDSR
323200031017     C*-----------------------------------------------------*
323300031017     C* verifica esistenza spunte per scrittura MIC         *
323400031017     C*-----------------------------------------------------*
323500031017     C     eventimic     BEGSR
323600031017     c                   clear                   spunta            1
323700031017     c                   z-add     v01ndc        knf6
323800031020     C                   Z-ADD     V1CLNA        KFGS3
323900031017     c     kart          setll     fnart01l
324000031017     c                   do        *hival
324100031017     c     kart          reade     fnart01l                               96
324200031017     c   96              leave
324300070118     c     kbrv          chain     fnbrv01l
324400070118     c                   if        %found(fnbrv01l)
324500031017     c                   move      'X'           spunta
324600031017     c                   leave
324700031017     c                   endif
324800031017     c                   enddo
324900031017     c                   endsr
325000980309     C*-----------------------------------------------------*
325100980309     C* ANNMIC: Annullo evento di  "Messa in consegna"      *
325200980309     C*-----------------------------------------------------*
325300980309     C     ANNMIC        BEGSR
325400980309     C*
325500980311     C* Se non sono in immissione (nr.distinta bolla=video)
325600980311     C     V01NDC        IFEQ      ARBNDC
325700980311     C* Controllo se ho scritto evento MIC
325800980318     C     KARB1         CHAIN     FNLBL01L                           87
325900980429     C*
326000980318     C     *IN87         IFEQ      '0'
326100980318     C     ARBLNA        ANDEQ     LBLLAP
326200980429     C     KLBL          CHAIN     FNARB01L                           30
326300980429     C     *IN56         IFEQ      '0'
326400980429     C     ARBCCA        ANDEQ     '2'
326500980429     C     *IN56         OREQ      '0'
326600980429     C     ARBCCA        ANDEQ     '6'
326700980429     C                   SETON                                        87
326800980429     C                   END
326900980429     C     KARB2         CHAIN     FNARB01L                           30
327000980527     C                   END
327100980429     C*
327200980429     C     *IN87         IFEQ      '0'
327300980429     C     ARBLNA        ANDEQ     LBLLAP
327400980318     C                   Z-ADD     LBLAAP        KAAS
327500980318     C                   Z-ADD     LBLLPP        KLNP
327600980318     C                   Z-ADD     LBLNRP        KNRS
327700980318     C                   Z-ADD     LBLNSP        KNSP
327800980318     C                   ELSE
327900980309     C                   Z-ADD     ARBAAS        KAAS
328000980309     C                   Z-ADD     ARBLNP        KLNP
328100980309     C                   Z-ADD     ARBNRS        KNRS
328200980309     C                   Z-ADD     ARBNSP        KNSP
328300980318     C                   END
328400980309     C                   MOVEL     'MIC'         KCEV
328500990803     C     KEVB          SETGT     FNEVB04L
328600990803     C     KEVB          READPE    FNEVB04L                               33
328700980309     C     *IN33         IFEQ      '0'
328800990803     C     FVVDFV        ANDEQ     EVBDEV
328900980311     C* Se MIC esiste e la data della distinta è la stessa
329000980311     C* dell'evento ... controllo se è già trasmesso
329100980311     C     EVBDTR        IFNE      0
329200990803     C                   MOVEL     EVBDEV        WDTMIC           12 0
329300980311     C                   MOVE      EVBHEV        WDTMIC
329400980311     C* se l'evento di messa in consegna è stato trasmesso
329500980311     C* controllo se esiste già un evento NIC tolto dalla
329600980311     C* consegna
329700980311     C                   MOVEL     'NIC'         KCEV
329800990803     C     KEVB          SETGT     FNEVB04L
329900990803     C     KEVB          READPE    FNEVB04L                               33
330000031021     C* SE HO TROVATO EVENTO NIC
330100980311     C     *IN33         IFEQ      '0'                                          Wtr Nic
330200990803     C     FVVDFV        ANDEQ     EVBDEV
330300031021     C* Se non è stato trasmesso aggiorno la data e l'ora di
330400031021     C* di immissione evento con quella del giorno + altri dati
330500031021     C     EVBDTR        IFEQ      0
330600031021     C                   move      dateu         EVBDTV
330700031021     C                   TIME                    EVBORV
330800031021     C                   Z-ADD     v1clna        EVBFLE
330900031021     C                   Z-ADD     DUTCOU        EVBCDU
331000031021     C     FVVFPP        IFEQ      'M'
331100031021     C                   MOVEL     0800          EVBHEV
331200031021     C                   ELSE
331300031021     C                   MOVEL     1200          EVBHEV
331400031021     C                   END
331500031021     C                   MOVEL     *BLANKS       EVBDTC
331600031021     C                   UPDATE    FNEVB000
331700031021     C* Se è stato trasmesso controllo se non ha data più
331800031021     C* alta del MIC
331900031021     C                   ELSE
332000031021     C                   MOVEL     EVBDEV        WDTNIC           12 0
332100031021     C                   MOVE      EVBHEV        WDTNIC
332200031021     C     WDTNIC        IFLE      WDTMIC
332300031021     C* Se la data non è più alta scrivo un nuovo NIC
332400031021     C                   CLEAR                   FNEVB000
332500031021     C                   MOVEL     'NIC'         EVBCEV
332600031021     C                   move      dateu         EVBDTV
332700031021     C                   TIME                    EVBORV
332800031021     C                   Z-ADD     v1clna        EVBFLE
332900031021     C                   Z-ADD     DUTCOU        EVBCDU
333000031021     C                   Z-ADD     KAAS          EVBAAS
333100031021     C                   Z-ADD     KLNP          EVBLNP
333200031021     C                   Z-ADD     KNRS          EVBNRS
333300031021     C                   Z-ADD     KNSP          EVBNSP
333400031021     C                   Z-ADD     FVVDFV        EVBDEV
333500031021     C     FVVFPP        IFEQ      'M'
333600031021     C                   MOVEL     0800          EVBHEV
333700031021     C                   ELSE
333800031021     C                   MOVEL     1200          EVBHEV
333900031021     C                   END
334000131104     c                   clear                   Devb01
334100131104     C                   move      *zeros        §NOTNDC
334200131104     C                   move      *zeros        §NOTPDC
334300131031     C                   move      ARBNDC        §NOTNDC
334400131031     C                   move      ARBPDC        §NOTPDC
334500131031     C                   MOVEL     Devb01        EVBNOT
334600031021     C                   MOVEL     *BLANKS       EVBDTC
334700031021     C                   WRITE     FNEVB000
334800031021     C                   END
334900031021     C*
335000031021     C                   END
335100031021     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
335200031021     C* distinta lo scrivo
335300031021     C                   ELSE
335400031021     C                   CLEAR                   FNEVB000
335500031021     C                   MOVEL     'NIC'         EVBCEV
335600031021     C                   move      dateu         EVBDTV
335700031021     C                   TIME                    EVBORV
335800031021     C                   Z-ADD     v1clna        EVBFLE
335900031021     C                   Z-ADD     DUTCOU        EVBCDU
336000031021     C                   Z-ADD     KAAS          EVBAAS
336100031021     C                   Z-ADD     KLNP          EVBLNP
336200031021     C                   Z-ADD     KNRS          EVBNRS
336300031021     C                   Z-ADD     KNSP          EVBNSP
336400031021     C                   Z-ADD     FVVDFV        EVBDEV
336500031021     C     FVVFPP        IFEQ      'M'
336600031021     C                   MOVEL     0800          EVBHEV
336700031021     C                   ELSE
336800031021     C                   MOVEL     1200          EVBHEV
336900031021     C                   END
337000131104     c                   clear                   Devb01
337100131104     C                   move      *zeros        §NOTNDC
337200131104     C                   move      *zeros        §NOTPDC
337300131031     C                   move      ARBNDC        §NOTNDC
337400131031     C                   move      ARBPDC        §NOTPDC
337500131031     C                   MOVEL     Devb01        EVBNOT
337600031021     C                   MOVEL     *BLANKS       EVBDTC
337700031021     C                   WRITE     FNEVB000
337800031021     C*
337900031021     C                   END
338000031021     C* Se MIC non è stato trasmesso annullo fisicamente record
338100031021     C                   ELSE
338200031021     C* annullo fisicamente l'evento se da trasmettere
338300031021     C  N33              DELETE    FNEVB04L
338400031021     C                   END
338500031021     C                   END
338600980311     C                   END                                                    Data dist.=
338700980309     C*
338800980309     C                   ENDSR
338900911105     C*-----------------------------------------------------*
339000080319     C* controllo presenza orm e se dati scaricati a PDA         *
339100911105     C*-----------------------------------------------------*
339200080320     C     srfasi        BEGSR
339300080318     C                   CLEAR                   WORM
339400080318     C                   Z-ADD     wnfv          FVVNF6
339500090211     C     KORM          SETLL     FNORM06L                               34
339600090211     C   34              MOVEL     'S'           WORM              1
339700080320     c* se ho la fase PDR (preparato x pda ORM)
339800080320     c* non faccio manutenzionare il flag matt/pom
339900090211     C                   CLEAR                   faseorm           3
340000080320     c                   eval      dsftrd = 'PDR'
340100080326     C     KDST4         chain     fidsf02l
340200080326     c                   if        %found(fidsf02l)
340300080320     c                   eval      faseorm = dsftrd
340400080320     c                   seton                                        82
340500080326     c                   end
340600080319     c* controllo se i flag per PDA e mat/pom sono modificabili
340700080320     c                   clear                   fasecon           3
340800080319     c                   setoff                                       7282
340900080319     c* se ho la fase PDC (preparato x pda con) o PDO x orm
341000080319     c* non faccio manutenzionare il flag matt/pom
341100080319     c                   eval      dsftrd = 'PDC'
341200080326     C     KDST4         chain     fidsf02l
341300080326     c                   if        %found(fidsf02l)
341400080320     c                   eval      fasecon = dsftrd
341500080319     c                   seton                                        82
341600080326     c                   end
341700080410     c* se ho la fase CUS (ok da spc) non faccio manutenzionare i flag
341800080410     c* dati a pda e matt/pom sia x con che x orm
341900080410     c                   eval      dsftrd = 'CUS'
342000080410     C     KDST4         chain     fidsf02l
342100080410     c                   if        %found(fidsf02l)
342200080410     c* chiamo il pgm per richiesta forzatura
342300080410     c                   exsr      srforza
342400130308     c*                  eval      fasecon = dsftrd
342500110224     c*                  seton                                        7282
342600080410     c                   end
342700080318     C                   ENDSR
342800080318     C*-----------------------------------------------------*
342900080410     C* pgm per forzare modifica distinta fase CUS          *
343000080318     C*-----------------------------------------------------*
343100080410     C     srforza       BEGSR
343200911105     C*
343300080410     c                   clear                   fidg44ds
343400080410     c                   eval      co44fgs=fvvfgs
343500080410     c                   eval      co44ndc=wnfv
343600080410     c                   eval      co44wnd = 'S'
343700080410     c                   eval      co44td = 'E'
343800080410     c* se ho la fase CUS e non la fase PDC vuol dire che è una distinta di
343900080410     c* di soli orm. Imposto O nel tipo distinta per evitare controlli
344000080410     c* inerenti alle consegne
344100080410     c                   if        fasecon = ' ' and worm = 'S'
344200080410     c                   eval      co44td = 'O'
344300080410     c                   end
344400080410     c                   movel(p)  fidg44ds      kpjbu
344500080410     c                   call      'FIDG44R'
344600080410     c                   parm                    kpjba
344700080410     c                   movel     kpjbu         fidg44ds
344800080410     c* se mi ritorna che non ha forzato accendo indicatore d'errore
344900080410     c                   if        co44fcus = 'N'
345000080410     c                   seton                                        90
345100080410     c                   end
345200080410     C*
345300080410     C                   ENDSR
345400080410     C*-----------------------------------------------------*
345500080410     C* ANNULLAMENTO DISTINTA                               *
345600080410     C*-----------------------------------------------------*
345700080410     C     ANNUL         BEGSR
345800080410     C*
345900911105     C* CONTROLLO SE SONO IN IMMMISSIONE O VARIAZIONE DISTINTA
346000911105     C*
346100000717     C                   SETOFF                                       78
346200000717     C                   Z-ADD     V01NDC        FVVNF6
346300070118     C     KFIBRV        SETLL     FNBRV01L                               78
346400000717     C     *IN78         IFEQ      '1'
346500000717     C                   SETON                                        839075
346600000717     C                   MOVEL     ERR(21)       $MSG
346700000717     C                   ELSE
346800941107     C     V01NDC        IFEQ      *ZEROS
346900940322     C                   SETON                                        7590
347000940322     C                   MOVEL     ERR(4)        $MSG
347100911105     C                   ELSE
347200010403     C* WORM = "S"  --->  ESISTONO O.R.M. IN FASE < 900
347300010403    3C     WORM          IFEQ      'S'
347400010403     C                   SETON                                        789075
347500010403     C                   MOVEL     ERR(22)       $MSG
347600010403   X3C                   ELSE
347700020905     C                   Z-ADD     V1CLNA        KFGS
347800020905     C                   Z-ADD     V01NDC        KNDC
347900960702     C                   Z-ADD     ROVDDC        KDDC
348000020905     C     KARB          SETLL     FNARB70L
348100911105     C*
348200911105     C                   DO        *HIVAL
348300911105     C*
348400080318     C     KARB          READE     FNARB70L
348500080318     C                   if        %eof(fnarb70l)
348600080318     c                   leave
348700080318     c                   end
348800941109     C     KARB1         CHAIN     FNARB01L                           31
348900980309     C* Se era in consegna e adesso non c'è più annullo la messa
349000980309     C* consegna
349100980309     C                   EXSR      ANNMIC
349200980309     C*
349300950602     C                   Z-ADD     *ZEROS        ARBNSS
349400020905     C                   Z-ADD     *ZEROS        ARBIFP
349500020905     C                   Z-ADD     *ZEROS        ARBNDC
349600911108     C                   Z-ADD     *ZEROS        ARBDDC
349700911108     C                   Z-ADD     *ZEROS        ARBSTP
349800911108     C                   Z-ADD     *ZEROS        ARBNGD
349900941108     C                   Z-ADD     *ZEROS        ARBPDC
350000941109     C                   UPDATE    FNARB000
350100030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
350200030910     c* deve andare a togliere la distinta dall'orm
350300030910     c                   exsr      srorm
350400070319     c* tolgo i riferimenti della distinta anche su FIARG x
350500070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
350600070319     c                   exsr      srfiarg
350700080410     c* richiamo il pgm per aggiornare i dati per PDA
350800080410     c                   if        §OGPDAcon <> ' ' and
350900080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
351000080411     c                             v1ctstpda = 'C' or
351100080411     c                             v1ctstpda = 'E')
351200080410     c                   clear                   fidg42ds
351300080410     c                   eval      co42tla = 'A'
351400080410     c                   exsr      srpda
351500080410     c                   end
351600930624     C* ELIMINO EVENTUALI ANOMALIE RELATIVE ALLA BOLLA
351700931214     C  N31              EXSR      ANOM
351800911105     C*
351900160429     C     KARB1         CHAIN     FNARBW2L
351901160429     c                   if        %found(fnarbw2l)
352000160429     C                   DELETE    FNARBW2L
352001160429     c                   end
352100080318     C                   ENDdo
352200911105     C*
352300020905     C                   Z-ADD     V1CLNA        FVVFGS
352400020905     C     KFVV          CHAIN     FNFVV01L                           83
352500920914     C  N83              MOVEL     '*'           FVVATB
352600970401     C  N83              MOVEL     *BLANKS       FVVFCF
352700941109     C  N83              UPDATE    FNFVV000
352800000515     C* FOGLIO 8
352900000717     C  N83              Z-ADD     8             FVVNPG
353000020905     C  N83KFVV          CHAIN     FNFVV01L                           83
353100000515     C  N83              MOVEL     '*'           FVVATB
353200000515     C  N83              MOVEL     *BLANKS       FVVFCF
353300000515     C  N83              UPDATE    FNFVV000
353400000515     C                   Z-ADD     4             FVVNPG
353500950601     C*
353600120718     c* aggiorno il file FIDST00F
353700120718     C  N83KDST2         CHAIN     FiDST09L                           83
353800120807     C  N83              MOVEL     'A'           £DSTATB
353900120718     C  N83              UPDATE    FiDST000
354000120718     c                   exsr      srfidst
354100010403    3C                   END
354200911105     C                   END
354300000218     C*
354400000717     C                   END
354500000218     C* REIMPOSTO LA DATA DISTINTA CON LA DATA DEL GIORNO
354600000218     C                   MOVE      WHHDAT        V1CDDC
354700911105     C*
354800911105     C                   ENDSR
354900930623     C*-----------------------------------------------------*
355000930623     C* GESTIONE ANOMALIE DELLA BOLLA                       *
355100930623     C*-----------------------------------------------------*
355200930623     C     ANOM          BEGSR
355300930623     C*
355400960415     C                   CLEAR                   DSLR33
355500960415     C                   MOVE      4             D33FSC
355600960415     C                   MOVE      ROVDDC        D33DCH
355700960415     C*
355800990519     C                   MOVE      ARBAAS        WAA4              4 0
355900950915     C                   Z-ADD     135           §§§CAA
356000990519     C     KANM          SETLL     FNANM01L                           34
356100930624     C     *IN34         DOWEQ     '0'
356200990519     C     KANM          READE     FNANM01L                               34
356300960415     C     *IN34         IFEQ      *OFF
356400960415     C                   Z-ADD     ANMNR1        D33NRR
356500960415     C                   CALL      'FNLR33R'
356600960415     C                   PARM                    DSLR33
356700960415     C                   MOVE      '1'           W33R              1
356800960415     C                   END
356900960415     C                   ENDDO
357000950918     C*
357100950915     C                   Z-ADD     150           §§§CAA
357200990519     C     KANM          SETLL     FNANM01L                           34
357300950915     C     *IN34         DOWEQ     '0'
357400990519     C     KANM          READE     FNANM01L                               34
357500960415     C     *IN34         IFEQ      *OFF
357600960415     C                   Z-ADD     ANMNR1        D33NRR
357700960415     C                   CALL      'FNLR33R'
357800960415     C                   PARM                    DSLR33
357900960415     C                   MOVE      '1'           W33R              1
358000960415     C                   END
358100960415     C                   ENDDO
358200930623     C*
358300930623     C                   ENDSR
358400940322     C*-----------------------------------------------------*
358500940322     C* PULIZIA RICHIESTA PRECEDENTE                        *
358600940322     C*-----------------------------------------------------*
358700940322     C     PULRIC        BEGSR
358800911106     C*
358900911106     C* PULISCO  IL FILE DI LAVORO
359000911106     C*
359100941109     C     *LOVAL        SETLL     FNARBW1L
359200911106     C*
359300911106     C                   DO        *HIVAL                                       4
359400941109     C                   READ      FNARBW1L                               55
359500911106     C*
359600941109     C  N55              DELETE    FNARBW1L
359700911106     C  N55              END
359800911106     C*
359900950907     C     D50TPR        IFEQ      ' '
360000920423     C                   MOVEL     *BLANK        SAVCPC
360100941107     C                   MOVEL     *BLANK        V1CCPC
360200941107     C                   MOVE      V01NDC        V1CNDP
360300941107     C                   MOVEL     *BLANK        V1CNDC
360400950601     C                   MOVEL     *BLANK        V1CPEP
360500050524     C                   MOVEL     *blanks       V1Ctpm
360501160429     C                   MOVEL     *blanks       V1Ctur
360600080121     c                   clear                   v1cpda
360700080208     c                   clear                   v1ctstpda
360800080208     c                   clear                   msgv
360900080318     c                   clear                   msgv1
361000080415     c                   setoff                                       45
361100950608     C                   MOVE      WHHDAT        V1CDDC
361200950907     C                   END
361300911106     C*
361400911106     C                   ENDSR
361500940322     C*-----------------------------------------------------*
361600940322     C* Gestione eventuale attribuzione Stop                *
361700940322     C*-----------------------------------------------------*
361800940322     C     GESSTP        BEGSR
361900951121     C*
362000951207     C                   Z-ADD     0             CODCLI
362100951121     C                   MOVEL     *BLANKS       RAGCLI
362200951207     C*  Controllo se devo attribuire lo Stop
362300951207     C                   Z-ADD     1             X                 2 0
362400951207     C                   MOVEL     *BLANKS       V3CCBO
362500951207     C                   MOVEL     *BLANKS       WTBOL1
362600951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
362700951207     C     *IN13         IFEQ      '1'
362800951207     C                   MOVEL     D3A(X)        DS3A
362900951207     C                   MOVEL     §3ATB1        WTBOL1
363000990920     C                   MOVEL     §3ATB2        WTBOL2
363100990920     C                   END
363200990920     C* Se la prima bolla è un assegnato eseguo posizionamento
363300990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
363400990920     C     WTBOL1        IFEQ      'A'
363500990920     C                   MOVEL     '1'           KTRC
363600990920     C                   Z-ADD     ARBAAS        KAAS
363700990920     C                   Z-ADD     ARBAAS        KLNP
363800990920     C                   Z-ADD     ARBAAS        KNRS
363900990920     C                   Z-ADD     ARBAAS        KNSP
364000990920     C     KAR6          CHAIN     FIAR601L                           74
364100990920     C* ... se non trovo bolla o il codice cliente non è
364200990920     C* impostato reperisco il codice da FNARB
364300990920     C     *IN74         IFEQ      '1'
364400990920     C     AR6KSC        OREQ      0
364500990920     C                   Z-ADD     ARBKSC        CODCLI
364600990920     C                   ELSE
364700990920     C* ... altrimenti recupero codice cliente da FIAR6
364800990920     C                   Z-ADD     AR6KSC        CODCLI
364900990920     C                   END
365000990920     C* .... se prima bolla è un porto franco ....
365100990920     C                   ELSE
365200990920     C* .... verifico se la seconda è un assegnato
365300990920     C     WTBOL2        IFEQ      'A'
365400990920     C                   MOVEL     '2'           KTRC
365500990920     C                   Z-ADD     ARBAAS        KAAS
365600990920     C                   Z-ADD     ARBAAS        KLNP
365700990920     C                   Z-ADD     ARBAAS        KNRS
365800990920     C                   Z-ADD     ARBAAS        KNSP
365900990920     C     KAR6          CHAIN     FIAR601L                           74
366000990920     C* ... e reperisco il codice bolla da lì
366100990920     C     *IN74         IFEQ      '0'
366200990920     C     AR6KSC        ANDNE     0
366300990920     C                   Z-ADD     AR6KSC        CODCLI
366400990920     C                   END
366500990920     C                   END
366600990920     C*
366700990920     C                   END
366800951207     C*
366900951207     C*  Controllo se cliente no generico e se si attribuisco stop
367000951207     C                   Z-ADD     CODCLI        APPOG             4 0
367100951207     C     CODCLI        IFNE      0
367200951207     C     APPOG         ANDNE     9999
367300951207     C     APPOG         ANDNE     8888
367400951211     C                   MOVEL     *BLANKS       RAGCLI
367500951211     C                   MOVEL     CODCLI        RAGCLI
367600951207     C                   ELSE
367700940322     C*  Controllo se devo attribuire lo Stop
367800951121     C                   MOVEL     ARBRSD        RAGCLI
367900951121     C                   MOVE      ARBIND        RAGCLI
368000951207     C                   END
368100940322     C*  Controllo se cliente esiste già
368200940322     C                   Z-ADD     1             Y                 3 0
368300951121     C     RAGCLI        LOOKUP    RSD2(Y)                                13
368400940322     C     *IN13         IFEQ      '1'
368500940322     C                   Z-ADD     STP2(Y)       ARBSTP
368600940322     C                   ELSE
368700961217     C     W             IFLT      999
368800940323     C                   ADD       1             ULTSTP            3 0
368900940322     C                   ADD       1             W
369000940323     C                   Z-ADD     ULTSTP        ARBSTP
369100940322     C                   Z-ADD     ULTSTP        STP2(W)
369200951121     C                   MOVEL     RAGCLI        RSD2(W)
369300940322     C                   END
369400951123     C                   END
369500940322     C*
369600940322     C*
369700940322     C                   ENDSR
369800940322     C*-----------------------------------------------------*
369900940322     C* Controllo gli stop                                  *
370000940322     C*-----------------------------------------------------*
370100940322     C     CTRSTP        BEGSR
370200940322     C*
370300940322     C                   SETOFF                                       16
370400941108     C     V3CPST        IFNE      V3CSTP
370500941108     C     V3CSTP        ANDNE     0
370600941108     C     V3CSTP        IFGT      ULTSTP
370700941108     C                   Z-ADD     V3CSTP        ULTSTP
370800940323     C                   END
370900951207     C                   MOVE      V3CCLI        APPOG
371000951207     C*
371100951207     C     V3CTB1        IFEQ      'A'
371200951207     C     APPOG         ANDNE     8888
371300951207     C     APPOG         ANDNE     9999
371400951211     C                   MOVEL     *BLANKS       RAGCLI
371500951211     C                   MOVEL     V3CCLI        RAGCLI
371600951207     C                   Z-ADD     1             X
371700951207     C     V3CSTP        LOOKUP    STP2(X)                                13
371800951207     C     *IN13         IFEQ      '1'
371900951207     C     RSD2(X)       ANDNE     RAGCLI
372000951207     C     RSD2(X)       ANDNE     *BLANKS
372100951207     C                   Z-ADD     NRR           NRRKEY
372200951207     C                   SETON                                        7516
372300951207     C                   MOVEL     ERR(5)        $MSG
372400951207     C                   ELSE
372500951207     C                   MOVEL     V3CSTP        V3CPST
372600951207     C     *IN13         IFEQ      '0'
372700951207     C                   ADD       1             W
372800951207     C                   Z-ADD     V3CSTP        STP2(W)
372900951207     C                   MOVEL     RAGCLI        RSD2(W)
373000951207     C                   END                                                    NEW STOP
373100951207     C                   END                                                    STOP GIA' ESIST.
373200951207     C                   ELSE                                                   ASSEGNATO COD.
373300940330     C*
373400940322     C                   Z-ADD     1             X
373500941108     C     V3CSTP        LOOKUP    STP2(X)                                13
373600940322     C*
373700940322     C     *IN13         IFEQ      '1'
373800951121     C     RSD2(X)       ANDNE     V3CRAG
373900951121     C     RSD2(X)       ANDNE     *BLANKS
374000940323     C                   Z-ADD     NRR           NRRKEY
374100940322     C                   SETON                                        7516
374200940322     C                   MOVEL     ERR(5)        $MSG
374300940322     C                   ELSE
374400941108     C                   MOVEL     V3CSTP        V3CPST
374500940322     C     *IN13         IFEQ      '0'
374600961217     C     W             ANDLT     999
374700940322     C                   ADD       1             W
374800951121     C                   MOVEL     V3CRAG        RAGCLI
374900941108     C                   Z-ADD     V3CSTP        STP2(W)
375000951121     C                   MOVEL     RAGCLI        RSD2(W)
375100951207     C                   END                                                    NEW STOP
375200951207     C                   END                                                    ESISTE GIA'
375300951207     C*
375400951207     C                   END                                                    ASSEGN. COD.
375500940322     C*
375600951123     C                   END
375700951123     C*
375800940322     C                   ENDSR
375900030910     C*-----------------------------------------------------*
376000070319     C*  toglie o mette distinta sul FIARG
376100030910     C*-----------------------------------------------------*
376200070319     C     srfiarg       BEGSR
376300070322     c                   if        comdda <= dateu and comdda <> 0
376400070319     c     karb1         chain(e)  fiarg01l
376500070531     c                   if        not %error and %found(fiarg01l)
376600070531     c                             and arbndc = 0
376700070531     C                   eval      argfgs = ARBIFP
376800070531     C                   eval      argNDC = ARBNDC
376900070531     C                   eval      argddc = ARBDDC
377000070531     C                   eval      argstp = ARBSTP
377100070531     C                   eval      argpdc = ARBPDC
377200070531     c                   clear                   argnftl
377300070531     c                   clear                   ARGDTVDIS
377400070531     c                   clear                   ARGHVDIS
377500070531     c                   eval      argslb = 0
377600100203     c                   movel     argflo        dargflo
377700100203     c                   clear                   §argattesa
377800100203     c                   movel     dargflo       argflo
377900070531     c                   update    fiarg000
378000070531     c                   end
378100070410     c                   if        not %found(fiarg01l) and arbndc <> 0
378200070509     c                             and not %Error
378300070410     c                   clear                   fiargds
378400070410     c                   z-add     arbaas        argaas
378500070410     c                   z-add     arblnp        arglnp
378600070410     c                   z-add     arbnrs        argnrs
378700070410     c                   z-add     arbnsp        argnsp
378800070410     c                   z-add     arblna        arglna
378900070410     c                   z-add     arbifp        argpoc
379000070508     c******* Questo bloccava Tellus nell'attribuzione dei GIRI
379100070508     c******* quindi è stato tolto.
379200070508     c*******            eval      ARGTGI = 'M'
379300070410     c                   write     fiarg000
379400070319     c                   end
379500080520     c                   unlock    fiarg01l
379600070410     c                   end
379700080410     c                   endsr
379800080410     C*----------------------------------------------------*
379900080410     C* se pda e tolgo o metto spedizione e fase PDC richiamo pgm per
380000080410     c* aggiornare i dati al PDA
380100080410     C*----------------------------------------------------*
380200080410     C     srpda         BEGSR
380300080410     c                   eval      co42FGS = v1clna
380400080410     C                   eval      co42NDC = v01ndc
380500080410     c                   eval      co42ddc = rovddc
380600080911     c                   if        co42tla = 'C'
380700080911     c                   clear                   co42aas
380800080911     c                   clear                   co42lnp
380900080911     c                   clear                   co42nrs
381000080911     c                   clear                   co42nsp
381100080911     c                   else
381200080410     c                   eval      co42aas = arbaas
381300080410     c                   eval      co42lnp  = arblnp
381400080410     c                   eval      co42nrs  = arbnrs
381500080410     c                   eval      co42nsp  = arbnsp
381600080911     c                   end
381700080410     c                   movel(p)  fidg42ds      kpjbu
381800080410     c                   call      'FIDG42R'
381900080410     c                   parm                    kpjba
382000080410     c                   parm                    fnarbds
382100080410     c* ritorno errore ???
382200080410     C                   ENDSR
382300070319     C*-----------------------------------------------------*
382400070319     C*  toglie o mette distinta sull'orm x particolarità RC
382500070319     C*-----------------------------------------------------*
382600070319     C     srorm         BEGSR
382700100514     C     arbgma        lookup    s7rc                                   84
382800100514     c                   if        *in84
382900030910     c                   clear                   fnlr99ds
383000030910     c                   eval      i99tla = 'L'
383100030910     c                   eval      i99aas = arbaas
383200030910     c                   eval      i99lnp = arblnp
383300030910     c                   eval      i99nrs = arbnrs
383400030910     c                   eval      i99nsp = arbnsp
383500030910     c                   eval      i99ndc = arbndc
383600030910     c                   eval      i99ddc = arbddc
383700071001     c                   eval      i99fgs = v1clna
383800030910     c* toglie
383900030910     c                   if        arbndc = 0
384000030910     c                   eval      i99fao = 390
384100030910     c                   else
384200030910     c* mette
384300030910     c                   eval      i99fao = 400
384400030910     c                   eval      i99npg = 4
384500071002     c                   eval      i99nftl= 99999
384600030910     c                   end
384700030910     c                   call      'FNLR99R'
384800030910     c                   parm                    kpjba
384900030910     c                   parm                    fnlr99ds
385000030910     c                   end
385100030910     C                   ENDSR
385200070612      *---------------------------------------------------------*
385300070612      *  Determinazione del volume da utilizzare
385400070612      *---------------------------------------------------------*
385500070612     C     CALVOL        BEGSR
385600070612     c* t=volume da fatturare
385700070612     C     ARBFVF        IFEQ      'T'
385800070612     C                   Z-ADD     ARBVLF        WVOL
385900070612     C                   ELSE
386000070612     C     ARBNCR        ifeq      ARBNCL
386100070612     C                   Z-ADD     ARBVLC        WVOL
386200070612     C                   ELSE
386300070612     C     ARBVLC        IFGE      ARBVLF
386400070612     C                   Z-ADD     ARBVLC        WVOL
386500070612     C                   ELSE
386600070612     C                   Z-ADD     ARBVLF        WVOL
386700070612     C                   ENDIF
386800070612     C                   ENDIF
386900070612     C                   ENDIF
387000070612     C*
387100070612     C                   ENDSR
387200070612      *---------------------------------------------------------*
387300070612      *  Determinazione del peso da utilizzare
387400070612      *---------------------------------------------------------*
387500070612     C     calPES        BEGSR
387600070612     C     arbncp        ifeq      arbncl
387700070612     C                   Z-ADD     arbPKC        wpes
387800070612     C                   ELSE
387900070612     C     arbPKC        ifge      arbPKF
388000070612     C                   Z-ADD     arbPKC        wpes
388100070612     C                   else
388200070612     C                   Z-ADD     arbPKF        wpes
388300070612     C                   end
388400070612     C                   ENDIF
388500070612     C                   ENDSR
388600141013      *_______________________________________________________________*
388700141013     c     verdispo      begsr
388800141013      *_______________________________________________________________*
388900141013     c                   eval      ilry09tla = 'L'
389000141029     c                   eval      ILRY09TDIS = 'E'
389100141029     c                   movel     fnlry09ds2    kpjbu
389200141013     c                   call      'FNLRY09R'
389300141013     c                   parm                    kpjba
389400141029     c                   movel     kpjbu         fnlry09ds2
389500141013     c                   endsr
389600940322     C*-----------------------------------------------------*
389700941107     C*    Operazioni iniziali
389800941107     C*-----------------------------------------------------*
389900940322     C     *INZSR        BEGSR
390000950907     C*
390100940322     C     *ENTRY        PLIST
390200940322     C                   PARM                    KPJBA
390300950907     C                   MOVEL     KPJBU         FNLR50
390400030701     c                   z-add     1             tblkut
390500020905     c*
390600020905     c     *dtaara       define    §azute        azuteds
390700020905     c     *dtaara       define    §datiute      ddatiute
390800020905     C                   in(E)     *dtaara
390900020905     C                   IF        %Error  or  RSUT = *blanks
391000020905     C                   call      'TIBS34R'
391100020905     C                   parm                    Tibs34Ds
391200020905     C                   in        *dtaara
391300020905     c                   ENDIF
391400941107     C*
391500941107     C* Inverto la data del giorno
391600941107     C                   TIME                    WHHDAT           14 0
391700941107     C                   MOVE      WHHDAT        G02DAT
391800940322     C                   MOVE      *BLANKS       G02ERR
391900941107     C                   CALL      'XSRDA8'
392000941107     C                   PARM                    WLBDA8
392100941107     C                   Z-ADD     G02INV        DATEU             8 0
392200020905     C*
392300990920     C*  Aggancio tabella 'GED' x reperire la divisa corrente
392400990920     C                   MOVEL     'GED'         KCOD1
392500990920     C                   MOVEL     *BLANKS       KKEY1
392600990920     C                   MOVEL     '1'           KKEY1
392700990920     C     KTBE          CHAIN     TNTBE01L                           13
392800990920     C     *IN13         IFEQ      '0'
392900990920     C                   MOVEL     TBEUNI        DGED
393000990920     C                   END
393001160316     C*  Aggancio tabella 'NSD' informazioni vedi pacco dpd
393002160316     C                   MOVEL     'NSD'         KCOD1
393003160316     C                   MOVEL     *BLANKS       KKEY1
393004160316     C                   MOVEL     '1'           KKEY1
393005160316     C     KTBE          CHAIN     TNTBE01L                           13
393006160316     C     *IN13         IFEQ      '0'
393007160316     C                   MOVEL     TBEUNI        DNSD
393008160316     C                   END
393100000929     C*
393200000929     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
393300000929     C                   MOVEL     'DGD'         KCOD1
393400000929     C                   MOVEL     *BLANKS       KKEY1
393500000929     C                   MOVEL     '1'           KKEY1
393600000929     C     KTBE          CHAIN     TNTBE01L                           13
393700000929     C     *IN13         IFEQ      '0'
393800000929     C                   MOVEL     TBEUNI        DDGD
393900000929     C                   END
394000940322     C*  CARICO DATI TABELLA 3A
394100940323     C                   MOVEL     '3A'          TBLCOD
394200940322     C                   Z-ADD     0             X
394300940322     C     KTAB1         CHAIN     TABEL00F                           13
394400940322     C     *IN13         DOWEQ     '0'
394500941108     C     X             ANDLT     100
394600940322     C                   ADD       1             X
394700940322     C                   MOVEL     TBLKEY        C3A(X)
394800941109     C                   MOVEL     TBLUNI        D3A(X)
394900940323     C     KTAB1         READE     TABEL00F                               13
395000940322     C                   END
395100941110     C*
395200100203     C*  CARICO DATI TABELLA 7r part. consegne attesa
395300100203     C                   MOVEL     '7R'          TBLCOD
395400100203     C                   Z-ADD     0             X
395500100203     C     KTAB1         CHAIN     TABEL00F                           13
395600100203     C     *IN13         DOWEQ     '0'
395700100514     c                   movel     tbluni        ds7r
395800100514     C                   if        x < 100 and
395900100514     c                             §7ratt = 'S'
396000100203     C                   ADD       1             X
396100100203     C                   MOVEL     TBLKEY        s7r(X)
396200100514     c                   end
396300100517     C                   if        xx < 100 and
396400100514     c                             §7RRC  = 'S'
396500100514     C                   ADD       1             Xx
396600100514     C                   MOVEL     TBLKEY        s7rc(Xx)
396700100514     c                   end
396800100203     C     KTAB1         READE     TABEL00F                               13
396900100203     C                   END
397000941110     C*  CARICO DATI TABELLA CV
397100990920     C                   MOVEL     *BLANKS       DIVEUR
397200941110     C                   MOVEL     'CV'          TBLCOD
397300941110     C                   Z-ADD     0             X
397400941110     C     KTAB1         CHAIN     TABEL00F                           13
397500941110     C     *IN13         DOWEQ     '0'
397600941110     C     X             ANDLT     100
397700941110     C                   ADD       1             X
397800990920     C                   MOVEL     TBLKEY        DIVCV             3
397900990920     C                   MOVEL     TBLUNI        DSCV
398000990920     C* Se divisa diversa dalla moneta di conto reperisco data
398100990920     C* validità x trovare altra divisa da utilizzare x incassi
398200011009     C     §GEDCR        IFNE      DIVCV
398300990920     C     DIVEUR        ANDEQ     *BLANKS
398400990920     C     §CVDVA        ANDGE     'S'
398500990920     C                   MOVEL     DIVCV         DIVEUR            3
398600990920     C                   END
398700941110     C     KTAB1         READE     TABEL00F                               13
398800941110     C                   END
398900941108     C*
399000941108     C*  CARICO DATI TABELLA 1P
399100941108     C                   MOVEL     '1P'          TBLCOD
399200941108     C                   Z-ADD     0             X
399300941108     C     KTAB1         CHAIN     TABEL00F                           13
399400941108     C     *IN13         DOWEQ     '0'
399500941108     C     X             ANDLT     50
399600941108     C                   ADD       1             X
399700941108     C                   MOVEL     TBLUNI        DS1P
399800941108     C                   MOVEL     TBLKEY        C1P(X)
399900941108     C                   MOVEL     §1PDES        D1P(X)
400000941108     C     KTAB1         READE     TABEL00F                               13
400100941108     C                   END
400200940322     C*
400300941107     C                   MOVEL     'FNLR50R'     VIDPGM
400400940322     C                   SETON                                        2003
400500950907      *  Controllo se pgm richiamato da gestione fogli distinta
400600950907      *  consegna
400700950907     C                   SETOFF                                       02
400800950907     C     D50TPR        IFEQ      'C'
400900950907     C                   SETON                                        02
401000950908     C                   Z-ADD     D50FIL        V1CLNA
401100950907     C                   MOVE      D50NDT        V1CNDC
401200950907     C                   Z-ADD     D50NDT        V01NDC
401300020905     C                   Z-ADD     D50FPD        V1CFPD
401400950907     C                   Z-ADD     D50DDT        V1CDDC
401500950907     C                   MOVE      D50CPD        V1CCPC
401600950907     C                   MOVEL     D50FPP        V1CFPP
401700950907     C                   MOVEL     D50PEP        V1CPEP
401800050523     C                   MOVEL     D50tpm        V1Ctpm
401900950908     C                   MOVE      D50FIL        FILIA             3 0
402000950907     C                   ELSE
402100020730      * x As Unico imposta la filiale del P.O.
402200020905     C                   z-add     dutpou        FILIA             3 0
402300020730     C                   SETOFF                                       21
402400020730      * se è il primo livello
402500020905     c                   if        dutlpo = '1'
402600020730     C                   SETON                                        21
402700020730     c                   end
402800020730      *
402900950907     C                   MOVE      WHHDAT        V1CDDC
403000950907     C                   MOVEL     'M'           V1CFPP
403100080121     C                   clear                   V1Cpda
403200080208     C                   clear                   V1Ctstpda
403300080208     C                   clear                   msgv
403400080318     C                   clear                   msgv1
403500080415     c                   setoff                                       45
403600050524     C                   MOVEL     *blanks       V1CTPM
403601160429     C                   MOVEL     *blanks       V1CTur
403700950907     C* VALORIZZO IL CAMPO A VIDEO DI LINEA
403800950907     C                   MOVE      FILIA         V1CLNA
403900950907     C                   MOVE      FILIA         V1CFPD
404000950907     C                   END
404100050520     C* Parcel / messaggerie
404200050520     c                   exsr      srtpm
404300050520     C* inverto la data distinta
404400050520     c                   exsr      srddc
404500020701     C* Aggancio la £6 e recupero le linee a loro associate
404600020701     c                   clear                   trul06ds
404700020701     c                   eval      d06cod = '£6'
404800020701     c                   movel     filia         d06key
404900040831     c                   move      'S'           d06key
405000040831     C                   Z-ADD     G02INV        d06drf
405100020701     c                   movel     trul06ds      kpjbu
405200020701     c                   call      'TRUL06R'
405300020701     c                   parm                    kpjba
405400020701     c                   movel     kpjbu         trul06ds
405500020701     c                   movea     lint          Lin
405600020701     c
405700020701     C* Aggancio la £6 e recupero le linee a loro associate
405800020701     c                   clear                   trul06ds
405900020701     c                   eval      d06cod = '£6'
406000020701     c                   movel     filia         d06key
406100020701     c                   move      'G'           d06esc
406200040831     c                   move      'S'           d06key
406300040831     C                   Z-ADD     G02INV        d06drf
406400020701     c                   movel     trul06ds      kpjbu
406500020701     c                   call      'TRUL06R'
406600020701     c                   parm                    kpjba
406700020701     c                   movel     kpjbu         trul06ds
406800020701     c                   movea     lint          L£6
406900140926      * verifica presenza disposizioni web
407000141029     c                   clear                   fnlry09ds2
407100140926     c                   eval      ilry09fgs = filia
407200140926     c                   eval      ilry09tch = 'C'
407300141013     c                   exsr      verdispo
407400141029     c                   if        OLRY09ESAL = '1' and
407500140926     c                             OLRY09ERR  = *blank
407600140926     c                   seton                                        97
407700140926     c                   endif
407800940322      *---------------------------------------------------------------*
407900940322      * CHIAVI                                                        *
408000940322      *---------------------------------------------------------------*
408100070312     C     Kdst2         KLIST
408200070410     C                   KFLD                    fvvFGS
408300070312     C                   KFLD                    fvvnpg
408400070312     C                   KFLD                    wnfv
408500070312     C     Kdst1         KLIST
408600070312     C                   KFLD                    fvvFGS
408700070312     C                   KFLD                    wnfv
408800080121     C     Kdst3         KLIST
408900080121     C                   KFLD                    fvvfgs
409000080121     C                   KFLD                    knpg
409100080121     C                   KFLD                    wnfv
409200080318     C     Kdst4         KLIST
409300080318     C                   KFLD                    knpg
409400080318     C                   KFLD                    wnfv
409500080318     C                   KFLD                    fvvfgs
409600080319     C                   KFLD                    dsftrd
409700021204     C     Kapd          KLIST
409800021204     C                   KFLD                    apdtip
409900021204     C                   KFLD                    apdpdr
410000021204     c                   movel     'A'           apdtip
410100070312     c     Kapd21        Klist
410200070312     c                   Kfld                    apdtip
410300120718     c                   Kfld                    £dstpdr
410400070312     c                   Kfld                    fvvfpp
410500070312     c                   Kfld                    fvvdfv
410600070312     c     Kapd2         Klist
410700070312     c                   Kfld                    apdtip
410800120718     c                   Kfld                    £dstpdr
410900070312     c                   Kfld                    fvvfpp
411000940322     C     KTAB          KLIST
411100020906     C                   KFLD                    tblkut
411200940322     C                   KFLD                    TBLCOD
411300940322     C                   KFLD                    TBLKEY
411400940322     C     KTAB1         KLIST
411500020906     C                   KFLD                    tblkut
411600940322     C                   KFLD                    TBLCOD
411700990920     C     KTBE          KLIST
411800990920     C                   KFLD                    KCOD1
411900990920     C                   KFLD                    KKEY1
412000951211     C     KNUF          KLIST
412100941107     C                   KFLD                    AAA               2 0
412200940322     C                   KFLD                    CNU               3 0
412300951211     C     KNUF0         KLIST
412400951211     C                   KFLD                    AAA0              2 0
412500951211     C                   KFLD                    CNU               3 0
412600951211     C                   Z-ADD     12            CNU
412700941107     C                   MOVE      WHHDAT        AAA
412800951211     C                   Z-ADD     0             AAA0
412900940322     C     KARB1         KLIST
413000940322     C                   KFLD                    ARBAAS
413100940322     C                   KFLD                    ARBLNP
413200940322     C                   KFLD                    ARBNRS
413300940322     C                   KFLD                    ARBNSP
413400960702     C     KARB          KLIST
413500020906     C                   KFLD                    Kfgs
413600020905     C                   KFLD                    KNDC
413700960702     C                   KFLD                    KDDC
413800940322     C     KARB2         KLIST
413900940322     C                   KFLD                    LBLAAN
414000940322     C                   KFLD                    LBLLPN
414100940322     C                   KFLD                    LBLNRN
414200940322     C                   KFLD                    LBLNSN
414300940322     C     KFVV          KLIST
414400940322     C                   KFLD                    FVVNPG
414500940322     C                   KFLD                    FVVNFV
414600020905     C                   KFLD                    FVVFGS
414700950601     C     KDST          KLIST
414800950601     C                   KFLD                    FVVNFV
414900950601     C                   KFLD                    FVVFGS
415000000717     C     KFIBRV        KLIST
415100000717     C                   KFLD                    FVVNPG
415200000717     C                   KFLD                    FVVNF6            6 0
415300030226     C                   KFLD                    FVVfgs
415400980309     C     KEVB          KLIST
415500980309     C                   KFLD                    KAAS
415600980309     C                   KFLD                    KLNP
415700980309     C                   KFLD                    KNRS
415800980309     C                   KFLD                    KNSP
415900980309     C                   KFLD                    KCEV
416000990920     C     KAR6          KLIST
416100990920     C                   KFLD                    KAAS
416200990920     C                   KFLD                    KLNP
416300990920     C                   KFLD                    KNRS
416400990920     C                   KFLD                    KNSP
416500990920     C                   KFLD                    KTRC
416600990920     C     KARI          KLIST
416700990920     C                   KFLD                    KAAS
416800990920     C                   KFLD                    KLNP
416900990920     C                   KFLD                    KNRS
417000990920     C                   KFLD                    KNSP
417100990920     C                   KFLD                    KTIP
417200941229     C*
417300940322     C                   Z-ADD     4             FVVNPG
417400940322     C*
417500940322     C     KLBL          KLIST
417600940322     C                   KFLD                    LBLAAP
417700940322     C                   KFLD                    LBLLPP
417800940322     C                   KFLD                    LBLNRP
417900940322     C                   KFLD                    LBLNSP
418000940322     C*
418100940322     C     KLBL1         KLIST
418200940322     C                   KFLD                    LBLAAN
418300940322     C                   KFLD                    LBLLPN
418400940322     C                   KFLD                    LBLNRN
418500940322     C                   KFLD                    LBLNSN
418600940322     C*
418700940322     C     KANM          KLIST
418800990519     C                   KFLD                    WAA4
418900940322     C                   KFLD                    ARBLNP
419000940322     C                   KFLD                    ARBNRS
419100940322     C                   KFLD                    ARBNSP
419200940322     C                   KFLD                    §§§CAA            3 0
419300940322     C*
419400940322     C     KEYFT2        KLIST
419500940322     C                   KFLD                    FTWPDR            7 0
419600940322     C                   KFLD                    FTWFVL            1
419700941109     C                   KFLD                    FTWDDC            8 0
419800940322     C                   KFLD                    FTWTSR            1
419900950606     C*
420000950606     C     KCLN          KLIST
420100950606     C                   KFLD                    KTFP
420200950606     C                   KFLD                    KTFA
420300950606     C                   KFLD                    KANN
420400950606     C                   KFLD                    KMES
420500010403     C* ACCESSO FNORM06L
420600010403     C     KORM          KLIST
420700010403     C                   KFLD                    FVVFGS
420800010403     C                   KFLD                    FVVNF6
420900031017     C     Kbrv          KLIST
421000031020     C                   KFLD                    kNPG              1 0
421100031017     C                   KFLD                    knf6              6 0
421200031020     C                   KFLD                    kfgs3             3 0
421300031017     c                   KFLD                    artLNP
421400031017     c                   KFLD                    artLNA
421500031017     c                   KFLD                    artNRS
421600031017     c                   kfld                    artNSC
421700031017     C                   Z-ADD     4             kNPG
421800031017     C     Kart          KLIST
421900031017     C                   KFLD                    KAAS
422000031017     C                   KFLD                    KLNP
422100031017     C                   KFLD                    KNRS
422200031017     c                   KFLD                    KNSP
422300040217      *
422400040217     C     Kfiar5        KLIST
422500040217     C                   KFLD                    arbAAS
422600040217     C                   KFLD                    arbLNP
422700040217     C                   KFLD                    arbNRS
422800040217     C                   KFLD                    arbNSP
422900040217     C                   KFLD                    trcar5            3
423000041007     C     kqdt          klist
423100041007     C                   kfld                    wlna              3 0
423200041007     C                   kfld                    wdis              6 0
423300110224     c     Kpct          Klist
423400110225     c                   Kfld                    £dstFGS
423500110225     c                   Kfld                    £dstNFV
423600110225     c                   Kfld                    £dstpdr
423700110324     c                   Kfld                    wtrd              3
423800110324     c                   eval       wtrd  = 'CET'
423900940322     C*
424000000515     C     *LIKE         DEFINE    FVVDS         FVVDS2
424100940322     C     *LIKE         DEFINE    TBLKUT        §KUT
424200940322     C     *LIKE         DEFINE    TBLCOD        §COD
424300940322     C     *LIKE         DEFINE    TBLKEY        §KEY
424400950117     C     *LIKE         DEFINE    VIDANS        SAVAAS
424500950117     C     *LIKE         DEFINE    V03LNP        SAVLNP
424600950117     C     *LIKE         DEFINE    V03NRS        SAVNRS
424700950117     C     *LIKE         DEFINE    V03NSP        SAVNSP
424800950606     C     *LIKE         DEFINE    CLNTFP        KTFP
424900950606     C     *LIKE         DEFINE    CLNTFA        KTFA
425000950606     C     *LIKE         DEFINE    CLNANN        KANN
425100950606     C     *LIKE         DEFINE    CLNMES        KMES
425200020905     C     *LIKE         DEFINE    ARBIFP        KFGS
425300020905     C     *LIKE         DEFINE    ARBNDC        KNDC
425400960702     C     *LIKE         DEFINE    ARBDDC        KDDC
425500980309     C     *LIKE         DEFINE    EVBAAS        KAAS
425600980309     C     *LIKE         DEFINE    EVBLNP        KLNP
425700980309     C     *LIKE         DEFINE    EVBNRS        KNRS
425800980309     C     *LIKE         DEFINE    EVBNSP        KNSP
425900980309     C     *LIKE         DEFINE    EVBCEV        KCEV
426000990920     C     *LIKE         DEFINE    TBECOD        KCOD1
426100990920     C     *LIKE         DEFINE    TBEKE1        KKEY1
426200990920     C     *LIKE         DEFINE    ARITIP        KTIP
426300990920     C     *LIKE         DEFINE    AR6TRC        KTRC
426400950117     C*
426500950123     C                   SETOFF                                       1319
426600940322     C*
426700940322     C                   ENDSR
426800010403** ERR
426900941109Eventuali variazioni andranno perse premendo F12                      01
427000940503Non è possibile annullare distinta in immissione                      02
427100940503Abilitata inclusione o esclusione non entrambi                        03
427200940503Non è posssibile annullare la distinta in immissione                  04
427300940503Stop già utilizzato per un altro cliente                              05
427400940503Linea errata                                                          06
427500940503Data distinta errata                                                  07
427600940503Data distinta errata: già confermati i conteggi                       08
427700940503Codice padroncino inesistente                                         09
427800961115Codice padroncino appartenente a punto operativo non in gestione      10
427900080318Autotrasp. diverso da distinta F1 confermi variazione                 11
428000940503Impossibile gestire distinta attendi 5'' e ritenta                    12
428100940503Distinta errata                                                       13
428200940503Distinta al momento non disponibile: ATTENZIONE !!!                   14
428300961115Numero distinta in gestione ad altra punto operativo                  15
428400000929Data distinta anteriore o superiore alla data odierna dei gg.ammessi  16
428500020906Punto operat. padroncino deve essere = al punto operat. in gestione   17
428600961115Puto operativo di consegna gestita da altro punto operativo           18
428700961217Data distinta non modificabile                                        19
428800961217Numero stop non attribuito correttamente: ci sono più di 999 spediz.  20
428900000717DISTINTA NON ANNULLABILE ESISTONO DELLE SPUNTE SU QUESTO FOGLIO       21
429000010403DISTINTA NON ANNULLABILE: PRESENTI O.R.M.                             22
429100020419Data distinta superiore alla data odierna dei gg.ammessi              23
429200020419Data distinta inferiore alla data odierna inserimento non possibile   24
429300080208Filiale non ancora attiva per gestione PDA                            25
429400110225Scelta incongruente o non ammessa                                     26
429500080318La distinta è in una fase dove non è + possibile modificare il valore 27
429600160502Il turno è ammesso solo per distinte Parcel                           28
