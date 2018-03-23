000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200941107      * FNLR50R *----------------------------------------------------*
000300930428      *---------*                                                    *
000400911016      *     - GESTIONE PREPARAZIONE DISTINTA USCITA CONSEGNA -       *
000500960702     F*--------------------------------------------------------------*
000600160510     Ffiprm01l  IF   E           K DISK
000700941107     FFNLR50D   CF   E             WORKSTN
000800941107     F                                     SFILE(LR50S03:NRR)
000900911016     FTABEL00F  IF   E           K DISK
001000990920     FTNTBE01L  IF   E           K DISK
001100021204     Ffiapd01L  IF   E           K DISK
001200080326     Ffidsf02L  IF   E           K DISK
001300110224     FFipct02L  IF   E           K DISK
001400020905     FFNARB70L  IF   E           K DISK
001500020905     F                                     RENAME(FNARB000:FNARB70)
001600941107     FFNARB01L  UF   E           K DISK
001700070410     FFiarg01L  UF a E           K DISK
001800990920     FFIAR601L  IF   E           K DISK
001900051115     FFIAR901L  IF   E           K DISK
002000990920     FFIARI01L  IF   E           K DISK
002100941107     FFNARBW1L  UF A E           K DISK
002200941107     FFNARBW2L  UF   E           K DISK
002300941107     F                                     RENAME(FNARBW00:FNARBW2)
002400020905     FFNFVV01L  UF A E           K DISK
002500941107     FFNLBL01L  IF   E           K DISK
002600941107     FFNLBL02L  IF   E           K DISK
002700941107     F                                     RENAME(FNLBL000:FNLBL02)
002800990519     FFNANM01L  IF   E           K DISK
002900990519     F                                     INFDS(FNANM1)
003000941107     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003100011121     Ffiftt02L  IF   E           K DISK
003200011121     Ffifst02L  IF   E           K DISK
003300011121     F                                     RENAME(fiftt000:fifst000)
003400950607     FAZCLN01L  IF   E           K DISK
003500990803     FFNEVB04L  UF A E           K DISK
003600060214     FFiAR401L  IF   E           K DISK
003700070118     FFNBRV01L  IF   E           K DISK
003800010403     FFNORM06L  IF   E           K DISK
003900031020     Ffnart01l  IF   E           K DISK
004000040217     FFiar501L  IF   E           K DISK
004100041007     FFiqdt01L  uf a E           K DISK
004200070410     FFidst09L  uf a E           K DISK    prefix(£)
004300070312     FFidst31L  uf a E           K DISK
004400080121     FFidst51L  uf a E           K DISK
004500070312     Ffiapd22L  IF   E           K DISK
004600070312     Ffiapd31L  IF   E           K DISK
004700080121     Ffiapd51L  IF   E           K DISK
004800940321      *---------------------------------------------------------------
004900950613     D L£6             S              3  0 DIM(50)                              LINEE £6
005000950613     D LIN             S              3  0 DIM(29)                              LINEE ARRIVI
005100911022     D UNI             S              3    DIM(29)                              LINEE DA TABEL
005200911021     D LMN             S              3  0 DIM(29)                              FILIALI
005300170203     D ERR             S             70    DIM(29) CTDATA PERRCD(1)             ERRORI A VIDEO
005400100203     D s7r             S              2    DIM(100)                             CODICE TABELLA 3A
005500100514     d s7rc            S              2    DIM(100)                             CODICE TABELLA 3A
005600100203     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
005700941109     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
005800941108     D C1P             S              1    DIM(50)                              CODICI TABELLA 1P
005900941108     D D1P             S             25    DIM(50)                              DESCR. TABELLA 1P
006000950606     D MES             S              1    DIM(31)
006100160719     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
006200940321      *---------------------------------------------------------------
006300940321      *  Schiere per gestione caricamento Stop
006400940321      *---------------------------------------------------------------
006500070312     d* schiere file fiapd20f
006600070312     D Ptc1            S               *
006700070312     D                                     INZ(%ADDR(apd2tc1))
006800070312     D tc1             S                   LIKE(apd2tc1)
006900070312     D                                     DIM(5)
007000070312     D                                     BASED(Ptc1)
007100070312     D Pts1            S               *
007200070312     D                                     INZ(%ADDR(apd2ts1))
007300070312     D ts1             S                   LIKE(apd2ts1)
007400070312     D                                     DIM(2)
007500070312     D                                     BASED(Pts1)
007600070312     d* schiere file fidst00f
007700070312     D Ptc1d           S               *
007800070312     D                                     INZ(%ADDR(£dsttc1))
007900070312     D tc1d            S                   LIKE(£dsttc1)
008000070312     D                                     DIM(5)
008100070312     D                                     BASED(Ptc1d)
008200070312     D Pts1d           S               *
008300070312     D                                     INZ(%ADDR(£dstts1))
008400070312     D ts1d            S                   LIKE(£dstts1)
008500070312     D                                     DIM(2)
008600070312     D                                     BASED(Pts1d)
008700070312     d*
008800961217     D STP             S              3  0 DIM(999)                             1° schiera Stop
008900961217     D STP2            S              3  0 DIM(999)                             2° schiera Stop
009000961217     D RSD2            S             70    DIM(999)                             2° schiera Rag.soc.
009100080410     c* ds file
009200160316     D dnsd          E DS
009300160316     D FVVDS         E DS                  EXTNAME(FNFVV00F)
009400070312     D fidstds       E DS                  INZ extname(fidst00f) prefix(£)
009500070312     D fidstds2      E DS                  INZ extname(fidst00f) prefix($)
009600070312     D fiapd2ds      E DS                  extname(fiapd20f)
009700070410     D fiargds       E DS                  extname(fiarg00f)
009800080410     D fnarbds       E DS                  extname(fnarb00f)
009900100203     D dARGFLO       E DS
010000160510     D fidg17ds      E DS
010100080410      * DS per parcel/messaggerie su FNFVV
010200050523     D dfvvspn       E DS
010300050523      * DS per parcel/messaggerie su FIAPD
010400050523     D dapdflr       E DS
010500080208      * DS filler  FIdst
010600080208     D ddstflr       E DS
010700080226      * DS per richiamo pgm fnlrd1r x cambio aut. se pda orm partito
010800080226     D fnlrd1ds      E DS
010900080226      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
011000080226     D fnlr99ds      E DS
011100031125      * DS per richiamo pgm fnlr98r x richiamo pgm per togliere bolla da distinta
011200031125     D fnlr98ds      E DS
011300141112     D fnlr98ds2     E DS
011400030630      * DS per richiamo pgm FNLR50R1
011500030630     D FNLR50        E DS                  EXTNAME(FNLR50DS)
011600080410      * DS per richiamo pgm FNLR6R controllo numeratore distinta
011700020906     D FNLR6ds       E DS
011800130424      * DS per richiamo pgm TNSD99R calcolo affidabilità
011900130424     D tnsd99ds      E DS
012000080410      * DS per richiamo pgm FNLR36R interrogazione bolle
012100941108     D DSLR36          DS
012200941108     D  P36AAS                14     17  0
012300941108     D  P36LNP                18     20  0
012400941108     D  P36NRS                21     22  0
012500941108     D  P36NSP                23     29  0
012600941108     D  P36F03                30     30
012700941108     D  P36FLG                31     31
012800941109      * DS per richiamo pgm FNLR55R
012900941109     D FNLR55        E DS                  EXTNAME(FNLR55DS)
013000941110      * DS per richiamo pgm FNLR02R
013100160719     D*DSLR01        E DS
013200160719      * DS per richiamo pgm Fidg31R
013300160719     D fidg31ds      E DS
013400021204      * DS per richiamo pgm FNLV24R
013500021204     D fnlv24ds      E DS
013600941110      * DS per richiamo pgm FNLR39R
013700941110     D DSLR02        E DS
013800041008      * DS  pgm fnlrf8r x gestione bancali affidati rientrati e colli ritirati
013900041008     D fnlrf8ds      E DS
014000080410      * DS  pgm fidg37r x aggiornare i valori della distinta                ti
014100070517     D fidg37ds      E DS
014200080410      * DS  pgm fidg09r x controllare giro tellus                           ti
014300070725     D fidg09ds      E DS
014400080410      * DS  pgm fidg44r x forzatura distinta con fase CUS                   ti
014500080410     D fidg44ds      E DS
014600080410      * DS  pgm fidg42r x aggiornare i dati a PDA x spedizione              ti
014700080410     D fidg42ds      E DS
014800141029     D fnlry09ds2    E DS
014900080410     D* ds tabelle
015000990921     D YEURDS        E DS                  EXTNAME(YEURCODS)
015100990920     D DGED          E DS
015200000929     D DDGD          E DS
015300160502     D DS03          E DS
015400990920     D DSCV          E DS
015500911106     D DS3A          E DS
015600941108     D DS1P          E DS
015700100203     D DS7r          E DS
015800050520     D og150         E DS
015900070309     D OG146         E DS                  INZ
016000080208     D OG148         E DS                  INZ
016100080121     D OG147         E DS                  INZ
016200911016     D KPJBA         E DS
016300080410      * ds interne pgm
016400000620     D OG143         E DS
016500020905     c* numeratori
016600020905     d trul33ds      e ds
016700020905     c*
016800131031     d dEVB01        e ds
016900131031     c*
017000020509     d trul06ds      e ds
017100020509     d  lint                   1     90  0 dim(30)
017200911016      *
017300941107     D WLBDA8          DS
017400941107     D  G02DAT                 1      8  0
017500941107     D  G02INV                 9     16  0
017600941107     D  G02ERR                17     17
017700941107     D  G02TGI                18     22  0
017800911017      *
017900020419     d Wdata8          DS
018000020419     d  dadata                 1      8  0
018100020419     d  adata                  9     16  0
018200020419     d  GioLav                17     21  0
018300020419      *
018400920306     D DATPAR          DS
018500941107     D  GIODAT                 1      8  0
018600941107     D  GIOINV                 9     16  0
018700941107     D  GIOTGI                17     21  0
018800950123     D                 DS
018900950123     D  V1CFPD                 1      3  0
019000950123     D  V1CCPC                 4      7
019100950123     D  WCDCPC                 1      7
019200960415     D DSLR33        E DS                  EXTNAME(FNLR33DS)
019300990519     D FNANM1          DS
019400960415     D  ANMNR1               397    400B 0
019500020905     D ddatiute      e ds
019600020905     D azuteds       e ds                  extname(AZUTE00F)
019700020905     D tibs34ds      E DS                  inz
019800000515     D DSBL4A        E DS
019900170203     D fnlr50wDS     E DS
020000170203     d   savpjbu       s                   like(kpjbu)
020100090211     D*DFAR          E DS
020200090211     D* §FARDS       E                     EXTFLD(D§FARDES)
020300090211     D* §FARFB       E                     EXTFLD(D§FARFFB)
020400090211     D* §FARNF       E                     EXTFLD(D§FARNONF)
020500090211     D* §FARMN       E                     EXTFLD(D§FARMAN)
020600090211     D* §FARST       E                     EXTFLD(D§FARSTP)
020700090211     D* §FARAF       E                     EXTFLD(D§FARAFVV)
020800090211     D* §FARCF       E                     EXTFLD(D§FARCFVV)
020900030618     c* ds per sottocampare il barcode della spedizione
021000030618     D ds_pis          DS
021100030618     D  ds_pisaas                     2  0
021200030618     D  ds_pislnp                     3  0
021300030618     D  ds_pisnrs                     2  0
021400030618     D  ds_pisnsp                     7  0
021500030623     D  ds_chkdgt                     1  0
021600030623     D trul28ds      E DS
021700040217     D Dar5gen       E DS
021800150929     D Dar5emd       E DS
021900030623     d savchk          s                   like(o28ckd)
022000030623     d barcode         s             14
022100070312     d wnfv            s                   like(£dstnfv)
022200070612     d wpes            s                   like(arbpkf)
022300070612     d wvol            s                   like(arbvlf)
022400160510     d oggi            s               D
022500940321      *---------------------------------------------------------------
022600941109     IAZORG01L  AA
022700941109     I                             P    4    5 0ORGFIL
022800971119     I                                 11   11  ORGFL1
022900000620     I                               3776 3800  ORGDE3
023000070309     I                               3851 3875  ORGDe6
023100080121     I                               3876 3900  ORGDe7
023200080208     I                               3901 3925  ORGDe8
023300070309     I                               3951 3975  ORGDF0
023400911016      *---------------------------------------------------------------*
023500911016      * FLUSSO PRINCIPALE                                             *
023600911016      *---------------------------------------------------------------*
023700941107      *  Loop Principale
023800941107     C     *INKC         DOWEQ     '0'
023900941107     C                   EXSR      GESD01
024000941107     C                   END                                                    *do hival
024100911016     C*
024200911016     C*                  FINE LAVORO
024300911018     C     FINE          TAG
024400960415     C* RICHIAMO PGM PER CHIUSURA FILES
024500960415     C     W33R          IFEQ      '1'
024600960415     C                   MOVEL     'C'           D33TLA
024700960415     C                   CALL      'FNLR33R'
024800960415     C                   PARM                    DSLR33
024900960415     C                   END
025000911128      *
025100911018     C                   SETON                                        LR
025200941107     C*----------------------------------------------------*
025300941107     C* Gestione Primo formato                             *
025400941107     C*----------------------------------------------------*
025500941107     C     GESD01        BEGSR
025600941107     C*
025700941107     C                   Z-ADD     0             NUM
025800040217     C                   SETOFF                                       464267
025900100203     C                   SETOFF                                       48
026000030701     c* v1cisp mi servono per sapere se voglio inserire  le
026100080605     c* spedizioni a mano o con la pistola. ora forzo P perchè oramai
026200080605     c* tutti i po stampano le bolle su laser.
026300030701     c                   movel     'P'           v1cisp
026400030618     C*
026500950215     C                   EXFMT     LR50D01
026600941107     C                   MOVEL     *BLANKS       $MSG
026700941107     C                   SETOFF                                       901113
026800950123     C                   SETOFF                                       171219
026900040217     C                   SETOFF                                       464767
027000080208     C                   SETOFF                                       751069
027100941107     C                   Z-ADD     0             ULTSTP
027200941107     C                   Z-ADD     0             W                 3 0
027300941107     C                   Z-ADD     0             STP2
027400951121     C                   MOVEA     *BLANKS       RSD2
027500941107?    C* F3 - Fine lavoro
027600941107     C     *INKC         IFEQ      '1'
027700941107     C                   GOTO      FINVD1
027800941107     C                   END
027900950907?    C* Controllo se pgm non e richiamato da gestione fogli consegna
028000950907     C     D50TPR        IFEQ      'C'
028100950907     C                   EXSR      PGMRIC
028200950907     C                   ELSE
028300950907     C                   EXSR      PGMMNU
028400950907     C                   END
028500941107     C*
028600941107     C     FINVD1        ENDSR
028700950907     C*----------------------------------------------------*
028800950907     C* Pgm. richiamato da menù                            *
028900950907     C*----------------------------------------------------*
029000950907     C     PGMMNU        BEGSR
029100950907     C*
029200030804?    C* F2 - toglie spedizioni da distinta
029300030804     C     *INKB         IFEQ      '1'
029400110125     c                   clear                   fnlr98ds
029500110125     c                   movel(p)  fnlr98ds      kpjbu
029600030804     c                   call      'FNLR98C'
029700030804     c                   parm                    kpjba
029800020905     C                   MOVE      *ZEROS        SAVNDC
029900950907     C                   GOTO      FINMNU
030000950907     C                   END
030100140924?    C* F19- Richiama pgm Disposizioni via WEB
030200140924     C     *inkt         IFEQ      '1'
030300141029     c                   clear                   fnlry09ds2
030400140924     c                   eval      ilry09fgs = v1clna
030500140924     c                   eval      ilry09tch = 'E'
030600141013     c                   exsr      verdispo
030700141029     c                   if        OLRY09ESAL = '1'
030800140926     c                   seton                                        97
030900141001     c                   else
031000141001     c                   setoff                                       97
031100140926     c                   endif
031200140930     C                   GOTO      FINMNU
031300140924     c                   endif
031400030804?    C* F18 - Cambio Filiale
031500030804     C     *INKS         IFEQ      '1'
031600030804     C*??
031700030804     C                   MOVE      *ZEROS        SAVNDC
031800030804     C                   SETOFF                                       20
031900030804     C                   GOTO      FINMNU
032000030804     C                   END
032100950907     C*
032200950907     C     *INKA         IFEQ      '1'
032300080411     C                   MOVEL     £dstpdr       SAVCPCorm         7
032400080226     C                   MOVEL     WCDCPC        SAVCPC            7
032500950907     C                   SETON                                        03
032600950907     C                   END
032700020906     C* controlli po in gestione
032800950907     C                   EXSR      CONTR1
032900020905     c   90              goto      finmnu
033000020906     C* Controllo il numeratore 12 che non sia bloccato
033100020906     C                   CLEAR                   TRUL33DS
033200020906     c                   eval      i33tla = 'L'
033300020906     C                   Z-ADD     0             I33AAA
033400020906     C                   Z-ADD     1             I33ope
033500020906     C                   Z-ADD     12            I33CNU
033600020906     C                   Z-ADD     v1clna        I33PO1
033700020906     C                   Z-ADD     1             I33NUM
033800020906     c                   movel(p)  trul33ds      kpjbu
033900020906     c                   call      'TRUL33R'
034000020906     c                   parm                    kpjba
034100020906     c                   movel     kpjbu         trul33ds
034200020906     c* record numeratore allocato
034300020906     C                   IF        O33ERR = 02
034400020906     C                   SETON                                        9013
034500020906     C                   MOVEL     ERR(12)       $MSG
034600020906     C                   GOTO      FINMNU
034700020906     C                   END
034800020905?    C* F11 - Sblocco Distinte in preparazione
034900020905     C     *INKK         IFEQ      '1'
035000020905     C                   CLEAR                   FNLR50
035100020905     C                   Z-ADD     V1CLNA        D50FIL
035200020906     C                   TESTN                   V1CNDC               3131
035300020905     C   31              MOVE      V1CNDC        D50NDT
035400020905     C                   MOVEL     FNLR50        KPJBU
035500020905     C                   CALL      'FNLR50R1'
035600020905     C                   PARM                    KPJBA
035700020905     C                   END
035800020906     C* controlli successivi
035900020906     C                   EXSR      CONTR2
036000020906     c   90              goto      finmnu
036100950907     C*
036200950907     C* Controllo se digitato il numero distinta vado in modifica
036300020905     C     V01NDC        IFNE      *ZEROS
036400070312     C* alloco il file delle distinte FIDST00F
036500070312     c                   exsr      srlckdst
036600070312     c   90              goto      finmnu
036700950907     C* carico il file di lavoro con le sped.riguardanti la distinta
036800950907     C     V01NDC        IFEQ      SAVNDC
036900950907     C*   Alloco la distinta
037000020905     C     KFVV          CHAIN     FNFVV01L                           12
037100030926     c                   if        not *in12 and fvvatb<>'*'
037200030926     c                   if        fvvfcf <> ' '
037300030926     C                   SETON                                        9012
037400030926     C                   MOVEL     ERR(14)       $MSG
037500030926     c                   unlock    fnfvv01l
037600030926     c   90              goto      finmnu
037700030926     c                   end
037800030926     C                   MOVEL     'A'           FVVFCF
037900030926     C                   UPDATE    FNFVV000
038000950907     C                   EXSR      CARDIS
038100950907     C                   EXSR      GESSBF
038200950907     C                   MOVE      *ZEROS        SAVNDC
038300950907     C                   Z-ADD     V01NDC        FVVNFV
038400030926     c                   end
038500950907     C                   ELSE
038600950907     C                   Z-ADD     V01NDC        SAVNDC            7 0
038700950907     C                   END
038800950907     C*
038900950907     C                   ELSE
039000950907     C     TAG01         TAG
039100950907     C                   SETOFF                                       93
039200950907     C*
039300950907     C* Compilazione distinta con inserimento singola spedizione
039400950907     C                   MOVE      *ZEROS        SAVAAS
039500950907     C                   MOVE      *ZEROS        SAVLNP
039600950907     C                   MOVE      *ZEROS        SAVNRS
039700950907     C                   MOVE      *ZEROS        SAVNSP
039800170307     C                   MOVE      'N'           WForza            1
039900950907     C  N90              EXSR      GESFM3
040000950907     C                   MOVE      *ZEROS        SAVNDC
040100950907     C                   END                                                    *if modifica
040200950907     C* Se nelle videate precedenti hanno richiesto una videata
040300950907     C* particolare ritorno al controllo del tipo compilazione
040400950907     C   93              GOTO      TAG01
040500950907     C*
040600950907     C     FINMNU        ENDSR
040700080410     C*----------------------------------------------------*
040800080410     C* se pda e cambio aut. richiamo FNLRD1               *
040900080410     C*----------------------------------------------------*
041000080410     C     srchgcpc      BEGSR
041100080410     C* richiamo pgm per cambiare aut sui record FIPDO00T
041200080801     c                   if        savcpcorm <> wcdcpc
041300140212     c                             and savcpcorm <> ' '
041400080226     c                   clear                   fnlrd1ds
041500080226     c                   eval      LRD1FGS = v1clna
041600080226     c                   eval      LRD1NDC = v01ndc
041700080226     c                   movel     savcpcorm     LRD1PDRO
041800080226     c                   movel     wcdcpc        LRD1PDRN
041900080226     c                   call      'FNLRD1R'
042000080226     c                   parm                    fnlrd1ds
042100140212     c                   clear                   savcpcorm
042200080226     c                   end
042300080410     C                   ENDSR
042400080226     C*----------------------------------------------------*
042500080226     C* Pgm. richiamato da gestione fogli consegna         *
042600080226     C*----------------------------------------------------*
042700080226     C     PGMRIC        BEGSR
042800070315     C                   Z-ADD     4             FVVNPG
042900070315     C                   Z-ADD     D50NDT        FVVNFV
043000070315     C                   Z-ADD     D50FIL        FVVFGS
043100070312     C* alloco il file delle distinte FIDST00F
043200070312     c                   exsr      srlckdst
043300070312     c   90              goto      finric
043400950907     C*   Alloco la distinta
043500020905     C     KFVV          CHAIN     FNFVV01L                           12
043600030926     c                   if        not *in12 and fvvatb<>'*'
043700030926     c                   if        fvvfcf <> ' '
043800030926     C                   SETON                                        9012
043900030926     C                   MOVEL     ERR(14)       $MSG
044000030926     c                   unlock    fnfvv01l
044100030926     c   90              goto      finric
044200030926     c                   end
044300950907     C                   MOVE      FVVDFV        ROVDDC
044400950907     C                   MOVEL     'A'           FVVFCF
044500950907     C                   UPDATE    FNFVV000
044600950907     C*   Reperisco decodifica padroncino
044700950907     C                   MOVE      WCDCPC        APDPDR
044800021204     C     kapd          CHAIN     fiapd01L                           13
044900950907     C                   MOVE      APDRSC        V05PAD
045000950907     C                   MOVE      APDRSC        V06PAD
045100950908     C                   MOVE      WCDCPC        CODPAD            7 0
045200950907     C*------------------------------------------------------*
045300950907     C*  Controllo sul calendario il tipo di giorno che è la
045400950907     C*  data di consegna
045500950907     C     ROVDDC        IFNE      0
045600950907     C                   Z-ADD     ROVDDC        WMMGG             4 0
045700950907     C                   Z-ADD     ROVDDC        WG                2 0
045800950907     C                   MOVEL     ROVDDC        KANN
045900950907     C                   Z-ADD     0             KTFP
046000950907     C                   Z-ADD     0             KTFA
046100950907     C                   MOVEL     WMMGG         KMES
046200950907     C     KCLN          CHAIN     AZCLN01L                           31
046300950907     C                   MOVEA     CLNMAT        MES
046400950907     C                   MOVE      MES(WG)       WGG               1
046500950907     C                   END
046600950907     C*------------------------------------------------------*
046700950907     C                   EXSR      CARDIS
046800950907     C* Compilazione distinta con inserimento singola spedizione
046900950907     C     TAG02         TAG
047000950907     C                   SETOFF                                       93
047100950907     C                   MOVE      *ZEROS        SAVAAS
047200950907     C                   MOVE      *ZEROS        SAVLNP
047300950907     C                   MOVE      *ZEROS        SAVNRS
047400950907     C                   MOVE      *ZEROS        SAVNSP
047500170307     C                   MOVE      'N'           WForza            1
047600950907     C  N90              EXSR      GESFM3
047700950907     C                   MOVE      *ZEROS        SAVNDC
047800950907     C* Se nelle videate precedenti hanno richiesto una videata
047900950907     C* particolare ritorno al controllo del tipo compilazione
048000950907     C   93              GOTO      TAG02
048100950907     C                   END
048200950907     C*
048300030926     C     finric        ENDSR
048400070312     C*----------------------------------------------------*
048500070312     C* alloco il record della distinta sul file FIDST     *
048600070312     C*----------------------------------------------------*
048700070312     C     srlckdst      BEGSR
048800070312     c                   eval      wnfv = fvvnfv
048900070410     C     KDST2         CHAIN(e)  fidst09l
049000070312     c                   if        %error
049100070312     C                   SETON                                        9012
049200070312     C                   MOVEL     ERR(14)       $MSG
049300070312     c                   end
049400070612     c                   if        %found(fidst09l)
049500070830     c                   z-add     £DSTPKL       V02PTO
049600070830     c                   z-add     £DSTVLU       V02CTO
049700070612     c                   else
049800070612     c                   clear                   v02pto
049900070612     c                   clear                   v02cto
050000070612     c                   end
050100070312     C                   ENDSR
050200020906     C*----------------------------------------------------*
050300020906     C* CONTROLLO FORMATO 1: po di gestione                *
050400020906     C*----------------------------------------------------*
050500020906     C     CONTR1        BEGSR
050600040901     C* controllo e inverto la data distinta (G02INV)
050700040901     c                   exsr      srddc
050800040901     c* no errore data proseguo
050900040901     c                   if        not *in90
051000040901     C* Controllo la filiale che cambia tutta la mia linea di arrivo
051100941108     C     *IN21         IFEQ      '1'                                          *
051200020509     c                   clear                   trul06ds
051300020509     c                   eval      d06cod = '£1'
051400020916     c                   movel     simfel        d06key
051500040901     c                   move      'S'           d06key
051600040901     C                   Z-ADD     G02INV        d06drf
051700020509     c                   movel     trul06ds      kpjbu
051800020509     c                   call      'TRUL06R'
051900020509     c                   parm                    kpjba
052000020509     c                   movel     kpjbu         trul06ds
052100020701     c                   movea     lint          Lmn
052200941107     C* Controllo se esiste la filiale da me digitata
052300941108     C     V1CLNA        LOOKUP    LMN                                    70
052400940503     C     *IN70         IFEQ      '0'
052500940503     C                   SETON                                        9017
052600940503     C                   MOVEL     ERR(6)        $MSG
052700941107     C                   ELSE
052800950613     C* Controllo se filiale gestita da altra
052900950613     C     V1CLNA        LOOKUP    L£6                                    70
053000950613     C     *IN70         IFEQ      '1'
053100950613     C                   SETON                                        9017
053200950613     C                   MOVEL     ERR(18)       $MSG
053300950613     C                   ELSE
053400941107     C* Recupero le linee di arrivo dalla tabella £6
053500941108     C                   MOVE      V1CLNA        FILIA
053600941107     C* Aggancio la £6 e recupero le linee a loro associate
053700020509     c                   clear                   trul06ds
053800020509     c                   eval      d06cod = '£6'
053900020509     c                   movel     filia         d06key
054000040831     c                   move      'S'           d06key
054100040901     C                   Z-ADD     G02INV        d06drf
054200020509     c                   movel     trul06ds      kpjbu
054300020509     c                   call      'TRUL06R'
054400020509     c                   parm                    kpjba
054500020509     c                   movel     kpjbu         trul06ds
054600020509     c                   movea     lint          Lin
054700941108     C                   END
054800911021     C                   END
054900950613     C                   END
055000040901     C                   END
055100140926     c                   setoff                                       97
055200141029     c                   clear                   fnlry09ds2
055300140926     c                   eval      ilry09fgs = v1clna
055400140926     c                   eval      ilry09tch = 'C'
055500141013     c                   exsr      verdispo
055600141029     c                   if        OLRY09ESAL = '1' and
055700140926     c                             OLRY09ERR  = *blank
055800140926     c                   seton                                        97
055900140926     c                   endif
056000050520     c* parcel/messaggerie
056100050520     c                   if        not *in17
056200050520     c                   exsr      srtpm
056300050520     c                   end
056400080208     C                   ENDSR
056500020906     C*----------------------------------------------------*
056600050520     C* CONTROLLO parcel / messaggerie                     *
056700020906     C*----------------------------------------------------*
056800050520     C     srtpm         BEGSR
056900070412     c                   setoff                                       98
057000080208     c* leggo organigramma x gestire ind 04 parcel+messaggerie
057100050520     c     v1clna        chain     azorg01l
057200050520     c                   if        %found(azorg01l)
057300070309     c                   movel     orgde6        og146
057400080208     c                   movel     orgde8        og148
057500080121     C                   MOVEL     ORGde7        og147
057600080212     c* se procedura PDA non partita proteggo i flag
057700080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
057800080212     c                   setoff                                       71
057900080212     c                   else
058000080212     c                   seton                                        71
058100080212     c                   end
058200070313     c                   if        §ogdda <> ' '
058300070313     c                   movel     §ogdda        comdda            8 0
058400070412     c                   if        comdda <= dateu
058500070419     c*                  seton                                        98
058600070412     c                   end
058700070313     c                   else
058800070313     c                   clear                   comdda
058900070313     c                   end
059000070309     c                   movel     orgdf0        og150
059100050520     c     §OGPKM        comp      'S'                                    04
059200160429     c     §OGturno      comp      'S'                                    01
059300050520     c                   end
059400160510     c*
059500160510     c                   exsr      srprm
059600050520     C*
059700050520     C                   ENDSR
059800050520     C*----------------------------------------------------*
059900050520     C* CONTROLLO FORMATO 1                                *
060000050520     C*----------------------------------------------------*
060100050520     C     CONTR2        BEGSR
060200941109     C*
060300941109     C* NUMERO FOGLIO VIAGGIO
060400941109     C     V1CNDC        IFNE      *BLANKS
060500941109     C     V1CNDC        ANDNE     *ZEROS
060600941109     C*
060700020906     C* ricerca
060800941109     C     '?'           SCAN      V1CNDC                                 31
060900941109     C     *IN31         IFEQ      '1'
061000160719     C                   CLEAR                   fidg31ds
061100160802     C                   Z-ADD     0             FDGGDA
061200160727     C                   Z-ADD     0             FDGGAL
061300160719     C                   Z-ADD     V1CLNA        fdgFGS
061400160719     C                   MOVEL     'FNLR50R'     fdgPGM
061500160802     C                   MOVEL     'I'           fdgRIC
061600160719     C                   MOVEL     '2'           fdgTFV
061700160719     C                   Z-ADD     4             fdgNPG
061800160802     C                   move      'T'           fdgtpv
061900160719     C                   MOVEL     fidg31ds      KPJBU
062000160719     c                   clear                   $FV6
062100160719      * impostato a tutti 99999
062200160719     c                   do        600           yy                3 0
062300160719     c                   z-add     999999        $FV6(yy)
062400160719     c                   enddo
062500160719     C                   CALL      'FIDG31R'
062600941109     C                   PARM                    KPJBA
062700160719     c                   parm                    $FV6
062800160719     C                   MOVEL     KPJBU         fidg31ds
062900950215     C                   MOVE      *ZEROS        V1CNDC
063000160719     C                   MOVE      fdgNFV        V1CNDC
063100941109     C                   END
063200020906     C* controlli
063300941109     C                   TESTN                   V1CNDC               31
063400020918     c                   if        not *in31
063500020918     C                   SETON                                        9012
063600020918     C                   MOVEL     ERR(13)       $MSG
063700020918     C                   Z-ADD     *ZEROS        V01NDC
063800020918     C                   ELSE
063900020918     C                   MOVE      V1CNDC        V01NDC
064000020918     C                   END
064100020918     C                   ELSE
064200941109     C                   Z-ADD     *ZEROS        V01NDC
064300950601     C                   MOVEL     *BLANKS       V1CPEP
064400941109     C                   END
064500941109     C*  Controllo il numero di distinta se diverso da zero
064600941109     C*  deve esistere in archivio flarb00 o flarb50
064700941109     C*
064800960604     C  N90V01NDC        IFNE      *ZEROS
064900941109     C* Controllo se esiste in archivio FLFVV
065000941109     C                   Z-ADD     V01NDC        FVVNFV
065100020905     C                   Z-ADD     V1cLNA        FVVFGS
065200941109     C                   SETOFF                                       49
065300020905     C     KFVV          CHAIN     FNFVV01L                           1249
065400020906     C* rilascio
065500950207     C  N12
065600950207     CANN49              UPDATE    FNFVV000
065700950207     C  N12
065800950207     CANN49V01NDC        IFNE      SAVNDC
065900941228     C                   MOVEL     FVVFPP        V1CFPP
066000950601     C                   MOVEL     FVVPEP        V1CPEP
066100050523     C                   MOVEL     FVVspn        dfvvspn
066200150804     c                   if        *in04
066300160429     C                   MOVEL     §fvvtpm       v1ctpm
066400150804     c                   else
066500150804     c                   clear                   v1ctpm
066600150804     c                   end
066700150804     c                   if        v1ctpm = ' '
066800050523     C                   MOVEL     'P'           v1ctpm
066900050523     c                   end
067000160429     c* turno
067100160429     c                   if        *in01
067200160429     C                   MOVEL     §fvvturno     v1ctur
067300160429     c                   else
067400160429     c                   clear                   v1ctur
067500160429     c                   end
067600160429     c                   if        v1ctur = ' ' and §fvvtpm = ' '
067700160429     C                   MOVEL     '1'           v1ctur
067800160429     c                   end
067900941228     C                   END
068000941109     C     *IN49         IFEQ      '1'
068100941109     C                   SETON                                        9013
068200941109     C                   MOVEL     ERR(12)       $MSG
068300941109     C                   END
068400960604     C* Se numero distinta > 5 chr do errore
068500960604     C                   MOVEL     V01NDC        WCOMO2            2 0
068600960604     C  N12WCOMO2        IFGT      0
068700960604     C                   SETON                                        9012
068800960604     C                   MOVEL     ERR(13)       $MSG
068900960604     C                   END
069000960604     C*
069100941109     C     *IN12         IFEQ      '1'
069200941109     C     FVVATB        OREQ      '*'
069300941109     C     FVVFCF        OREQ      'S'
069400941109     C                   SETON                                        9012
069500941109     C                   MOVEL     ERR(13)       $MSG
069600941109     C                   END
069700941109     C  N90              DO
069800950613     C     FVVFCF        IFNE      ' '
069900941109     C                   SETON                                        9012
070000941109     C                   MOVEL     ERR(14)       $MSG
070100941109     C                   END
070200020906     C* in gestione ad altra filiale
070300020906     C*    FVVFGS        LOOKUP    LIN                                    31
070400020906     C*    *IN31         IFEQ      '0'
070500020906     C*                  SETON                                        9012
070600020906     C*                  MOVEL     ERR(15)       $MSG
070700020906     C*                  END
070800941109     C*
070900951010     C     V01NDC        IFNE      SAVNDC
071000941109     C                   MOVE      FVVDFV        ROVDDC
071100941109     C                   MOVE      FVVDFV        G02INV
071200941109     C                   MOVEL     '3'           G02ERR
071300941109     C                   CALL      'XSRDA8'
071400941109     C                   PARM                    WLBDA8
071500941109     C                   Z-ADD     G02INV        ROVDDC
071600941109     C                   Z-ADD     G02DAT        V1CDDC
071700941109     C                   MOVEL     ROVDDC        WAA4              4 0
071800941109     C                   MOVE      WAA4          AAA
071900941109     C                   END
072000951010     C                   END
072100941109     C*
072200941109     C  N90              DO
072300020905     C                   Z-ADD     V1CLNA        KFGS
072400020905     C                   Z-ADD     V01NDC        KNDC
072500960702     C                   Z-ADD     ROVDDC        KDDC
072600080121     c                   eval      wnfv = kndc
072700020905     C  N12KARB          CHAIN     FNARB70L                           92
072800080121     c* imposto flag pda
072900080208     C     V01NDC        IFNE      SAVNDC
073000080121     C     KDST3         CHAIN(N)  FiDST09L
073100080121     c                   if        %found(fidst09l)
073200080208     c                   eval      ddstflr = £dstflr
073300080208     c                   eval      v1ctstpda = §dsttstpda
073400160429     c                   eval      v1cpda = £dstpda
073500080121     c                   else
073600080121     c                   clear                   v1cpda
073700080208     c                   clear                   v1ctstpda
073800080121     c                   end
073900080319     c* verifico fasi distinta e se presenti ORM
074000080320     c                   exsr      srfasi
074100080410     c                   end
074200120718     C  N12              MOVEL     £DSTPDR       ARBPDC
074300941109     C*
074400950601     C     *IN92         IFEQ      '0'
074500950601     C     V1CCPC        IFEQ      *ZEROS
074600941109     C     V1CCPC        OREQ      *BLANKS
074700950123     C                   MOVE      ARBPDC        WCDCPC
074800941109     C                   END
074900120806     c                   else
075000120806     C     V1CCPC        IFEQ      *ZEROS
075100120806     C     V1CCPC        OREQ      *BLANKS
075200120806     C                   MOVE      £dstpdr       WCDCPC
075300120806     C                   END
075400950601     C                   END
075500950601     C*
075600941109     C                   SETOFF                                       70
075700941109     C  N92ARBLNA        LOOKUP    LIN                                    70
075800941109     C  N92
075900020905     CANN70dutlpo        IFEQ      '2'
076000941109     C                   SETON                                        9012
076100941109     C                   MOVEL     ERR(13)       $MSG
076200941109     C                   END
076300941109     C                   END
076400941109     C*
076500941109     C                   END
076600020906     C* DATA DISTINTA
076700941107     C  N90V1CDDC        IFEQ      *ZEROS
076800940503     C                   SETON                                        9011
076900940503     C                   MOVEL     ERR(7)        $MSG
077000911016     C                   ELSE
077100911017     C* routine di controllo data distinta
077200040901     c                   exsr      srddc
077300040901     C     G02ERR        IFne      '1'
077400951010     C* IN MODIFICA NON POSSO VARIARE LA DATE DISTINTA
077500951010     C  N90V01NDC        IFNE      *ZEROS
077600951010     C     G02INV        ANDNE     FVVDFV
077700951010     C                   SETON                                        9011
077800951010     C                   MOVEL     ERR(19)       $MSG
077900951010     C                   END
078000941107     C                   Z-ADD     G02DAT        V1CDDC
078100940503     C                   END
078200950418     C*
078300971119     C* CONTROLLO SE LNA ITALIA O  ESTERO
078400971119     C                   MOVEL     *BLANKS       ORGFL1
078500971119     C                   MOVE      V1CLNA        ORGFIL
078600160429     C     ORGFIL        CHAIN     AZORG01L
078700050711     c                   movel     orgde3        og143
078800020729     C                   MOVEL     *blank        flgest            1
078900020729      * se estero imposto il flag
079000160429     c                   if        %found(azorg01l)
079100160429     C     §OgNTW        ifeq      'FED'
079200020729     C     §OgNTW        oreq      'EEX'
079300020729     C     §OgNTW        oreq      'EUP'
079400020729     C                   MOVEL     'E'           flgest
079500020729     c                   end
079600160429     c                   end
079700050711      *?  inserimento  DISTINTA ESTERO TESTO LA DATA
079800050711     C     V1CNDC        IFeq      *BLANKS
079900050711     C     flgest        ANDeq     'E'
080000050711     C     V1CNDC        OReq      *ZEROS
080100050711     C     flgest        andeq     'E'
080200050711      *controllo range fra le date di oggi e quella immessa
080300050711     c                   eval      dadata = g02inv
080400050711     c                   eval      adata  = dateu
080500050711     c                   CALL      'XSRLAV8'
080600050711     c                   PARM                    Wdata8
080700050711     c                   if        giolav > §dgdgg
080800050711     c                   SETON                                        9011
080900050711     c                   MOVEL     ERR(23)       $MSG
081000050711     c                   end
081100050711     c                   end
081200020419     C* inserimento  PER NO DISTINTA ESTERO TESTO LA DATA
081300020419     C     V1CNDC        IFeq      *BLANKS
081400020729     C     flgest        ANDNE     'E'
081500020419     C     V1CNDC        OReq      *ZEROS
081600020729     C     flgest        andne     'E'
081700020419      *non ammessa data minore di oggi
081800020419     c                   if        g02inv < dateu
081900020419     c                   SETON                                        9011
082000020419     c                   MOVEL     ERR(24)       $MSG
082100020419     c                   endif
082200020419      *controllo range fra le date di oggi e quella immessa
082300020419     c                   eval      dadata = dateu
082400020419     c                   eval      adata  = g02inv
082500020419     c                   CALL      'XSRLAV8'
082600020419     c                   PARM                    Wdata8
082700020419     c                   if        giolav > §dgddc
082800020419     c                   SETON                                        9011
082900020419     c                   MOVEL     ERR(23)       $MSG
083000020419     c                   end
083100020419     c                   end
083200020419     C* manutenzione  PER NO DISTINTA ESTERO TESTO LA DATA
083300020419     C     V1CNDC        IFne      *BLANKS
083400020729     C     flgest        andne     'E'
083500020419     C     V1CNDC        ORne      *ZEROS
083600020729     C     flgest        andne     'E'
083700920306     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
083800950418     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
083900990823     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO 1 15 GG
084000990823     C                   MOVE      DATEU         WMGUDT            4 0
084100990823     C                   MOVEL     WMGUDT        WMUDT             2 0
084200000929     C                   Z-ADD     §DGDGG        NRGIO             3 0
084300000929     C  N90WMGUDT        IFGE      §DGDEI
084400000929     C     WMGUDT        ANDLE     §DGDEF
084500000929     C                   Z-ADD     §DGDGE        NRGIO
084600000929     C                   END
084700000929     C  N90DATEU         IFGE      §DGDII
084800000929     C     DATEU         ANDLE     §DGDIF
084900000929     C                   Z-ADD     §DGDGI        NRGIO
085000000929     C                   END
085100000929     C     G02TGI        SUB       NRGIO         GIOTGI
085200941107     C                   CALL      'XSRGI8'
085300920306     C                   PARM                    DATPAR
085400950418     C                   Z-ADD     GIOINV        DATM15            8 0
085500950418     C     DATM15        IFGT      DATEU
085600950418     C                   SETON                                        9011
085700950418     C                   MOVEL     ERR(16)       $MSG
085800950418     C                   END
085900000929     C     G02TGI        ADD       NRGIO         GIOTGI
086000941107     C                   CALL      'XSRGI8'
086100920306     C                   PARM                    DATPAR
086200950418     C                   Z-ADD     GIOINV        DATP15            8 0
086300950418     C     DATP15        IFLT      DATEU
086400950418     C                   SETON                                        9011
086500950418     C                   MOVEL     ERR(16)       $MSG
086600950418     C                   END
086700950418     C*
086800920306     C                   END
086900920306     C*
087000941107     C  N90              Z-ADD     G02INV        ROVDDC            8 0
087100950418     C  N90              MOVEL     ROVDDC        WAA4              4 0
087200950418     C  N90              MOVE      WAA4          AAA
087300911016     C                   END
087400950607     C*------------------------------------------------------*
087500950607     C*  Controllo sul calendario il tipo di giorno che è la
087600950607     C*  data di consegna
087700950609     C     ROVDDC        IFNE      0
087800950607     C                   Z-ADD     ROVDDC        WMMGG             4 0
087900950607     C                   Z-ADD     ROVDDC        WG                2 0
088000950607     C                   MOVEL     ROVDDC        KANN
088100950607     C                   Z-ADD     0             KTFP
088200950607     C                   Z-ADD     0             KTFA
088300950607     C                   MOVEL     WMMGG         KMES
088400950607     C     KCLN          CHAIN     AZCLN01L                           31
088500950607     C                   MOVEA     CLNMAT        MES
088600950607     C                   MOVE      MES(WG)       WGG               1
088700950609     C                   END
088800020906     C*PADRONCINO:
088900020906     c* il po di gestione e il po padroncino devono essere uguali
089000020906     C     V1CFPD        IFNE      V1CLNA
089100020906     C                   SETON                                        9019
089200020906     C                   MOVEL     ERR(17)       $MSG
089300020906     C                   END
089400020906     C* Ricerca padroncino
089500950123     C  N90V1CCPC        IFNE      *BLANKS
089600950123     C     V1CCPC        ANDNE     *ZEROS
089700911113     C                   SETOFF                                           31
089800941107     C     '?'           SCAN      V1CCPC                                 31
089900911104     C   31              DO
090000021204     c                   clear                   fnlv24ds
090100021204     C                   MOVEL     'A'           d24tip
090200021204     C                   MOVEL     'R'           d24FLG
090300021204     C                   MOVEL     V1CFPD        d24FIL
090400941107     C                   MOVE      *BLANKS       V1CCPC
090500021204     C                   MOVEL(p)  fnlv24ds      KPJBU
090600941108     C                   CALL      'FNLV24R'
090700921028     C                   PARM                    KPJBA
090800021204     C                   MOVEL     KPJBU         fnlv24ds
090900021204     C     d24PDR        IFNE      *ZEROS
091000021204     C                   MOVEL     d24PDR        WCDCPC
091100911104     C                   END
091200950123     C*
091300950123     C                   END
091400911104     C                   END
091500911017     C*
091600911017     C* Controllo il codice padroncino se corretto
091700911017     C*
091800941107     C  N90V1CCPC        IFNE      *ZEROS
091900941107     C     V1CCPC        ANDNE     *BLANKS
092000950217     C* Controllo se esiste nell'anagrafica padroncini
092100950217     C                   SETOFF                                       3132
092200950217     C                   TESTN                   WCDCPC               31
092300950217     C  N31              SETON                                        32
092400950217     C  N32              MOVE      WCDCPC        APDPDR
092500021204     C  N32kapd          CHAIN     fiapd01L                           13
092600000728     C  N32
092700000728     CANN13APDATB        IFNE      ' '
092800000728     C                   SETON                                        13
092900000728     C                   ENDIF
093000170203      ***
093100170203      *   Controlla se Disaccreditato
093200170203      ***
093300170203     c                   if        not *in13
093400170208     c                             and  hPdrAutista <> apdPdr
093500170203     c                   clear                   fnlr50wds
093600170208     c                   clear                   hPdrAutista       7 0
093700170203     C                   eval      savpjbu = kpjbu
093800170203     c                   z-add     apdPDR        §50autista
093900170203     c                   z-add     rovDDC        §50datadis
094000170203     c                   eval      §50WINDOW  = 'F'
094100170203     C                   eval         kpjbu = fnlr50wds
094200170203      ***
094300170206     C                   write     LR50D01
094400170203     C                   call      'FNLR50WR'
094500170203     C                   parm                    kpjba
094600170203     C                   eval      fnlr50wds = kpjbu
094700170203     C                   eval      kpjbu = savpjbu
094800170203      ***
094900170208     c                   IF        §50esito ='1'
095000170208     c                   IF        §50funz  ='F12'
095100170203     C                   SETON                                        9013
095200170203     C                   MOVEL     ERR(29)       $MSG
095300170203     c                   leavesr
095400170203     c                   endif
095500170208     c                   IF        §50funz  ='F07'
095600170208     c                   eval        hPdrAutista = §50autista
095700170208     c                   endif
095800170208     c                   endif
095900170203      ***
096000170203     c                   end
096100170203      ***
096200940503     C     *IN13         IFEQ      '1'
096300950217     C     *IN32         OREQ      '1'
096400940503     C                   SETON                                        9013
096500940503     C                   MOVEL     ERR(9)        $MSG
096600940503     C                   ELSE
096700941109     C                   MOVE      APDRSC        V05PAD
096800941109     C                   MOVE      APDRSC        V06PAD
096900950123     C                   MOVE      WCDCPC        CODPAD            7 0
097000950123     C                   MOVEL     V1CFPD        WFIL              3 0
097100950613     C     WFIL          LOOKUP    LIN                                    37
097200950613     C     *IN37         IFEQ      '0'
097300940503     C                   SETON                                        9013
097400940503     C                   MOVEL     ERR(10)       $MSG
097500940503     C                   END
097600950601     C*  Se picking non presente su foglio viaggio lo imposto da qui
097700950601     C     V1CPEP        IFEQ      *BLANKS
097800950601     C     APDPKM        IFEQ      'S'
097900950601     C     V1CFPP        ANDEQ     'M'
098000950601     C     APDPKP        OREQ      'S'
098100950601     C     V1CFPP        ANDEQ     'P'
098200950601     C                   MOVEL     'S'           V1CPEP
098300950601     C                   ELSE
098400950601     C                   MOVEL     'N'           V1CPEP
098500950601     C                   END
098600950601     C                   END
098700080121     c                   movel     apdflr        dapdflr
098800080410     c******************** Dati PDA  ****************************
098900080121     c                   if        v1cpda = *blanks
099000080121     c                   movel     §apdpda       v1cpda
099100101021     c                   movel     §apdtstpda    v1ctstpda
099200080709     c                   end
099300080121     c                   if        v1cpda = *blanks
099400080121     c                   movel     'N'           v1cpda
099500101021     c                   clear                   v1ctstpda
099600080121     c                   end
099700080212     c* controllo flag dati per PDA se attivo
099800080212     c                   if        *in71
099900080208     C                   if        (v1cpda = 'O' and
100000080208     c                             §OGPDAORM = ' ') or
100100080208     C                             (v1cpda = 'C' and
100200080208     c                             §OGPDACON = ' ') or
100300080208     C                             (v1cpda = 'E' and
100400080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
100500080208     C                   MOVEL     err(25)       $msg
100600080208     c                   select
100700080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
100800080208     c                   eval      $msg = %trim($msg)+' CONSEGNE'
100900080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
101000080208     c                   eval      $msg = %trim($msg) + ' ORM'
101100080208     c                   endsl
101200080208     C                   SETON                                        1090
101300080208     C                   ENDIF
101400080318     c* se distinta già preparata per pda non posso cambiare il flag x con.
101500080318     c* se ho la fase PDC
101600081111     c                   if        (£dstpda = 'E' or £dstpda ='C') and
101700081111     c                             (v1cpda ='N' or v1cpda='O') and
101800080320     c                             fasecon ='PDC'
101900080318     c                   seton                                        1090
102000080318     C                   MOVEL     err(27)       $msg
102100080318     c                   end
102200080318     c* se distinta già preparata per pda non posso cambiare il flag x orm
102300080318     c* se ho il FIPCO00T
102400081111     c                   if        (£dstpda = 'E' or £dstpda ='O') and
102500081111     c                             (v1cpda = 'N' or v1cpda ='C') and
102600080320     c                              faseorm ='PDR'
102700080318     c                   seton                                        1090
102800080318     C                   MOVEL     err(27)       $msg
102900080318     c                   end
103000080208     c* controllo flag test per PDA
103100080208     C                   if        (v1ctstpda = 'O' and
103200080208     c                             §OGPDAORM = ' ') or
103300080208     C                             (v1ctstpda = 'C' and
103400080208     c                             §OGPDACON = ' ') or
103500080208     C                             (v1ctstpda = 'E' and
103600080208     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
103700080208     C                   MOVEL     err(25)       $msg
103800080208     c                   select
103900080208     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
104000080208     c                   eval      $msg = %trim($msg) + ' CONSEGNE'
104100080208     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
104200080208     c                   eval      $msg = %trim($msg) + ' ORM'
104300080208     c                   endsl
104400080208     C                   SETON                                        6990
104500080208     C                   ENDIF
104600080326     c* controllo la congruenza dei valori impostati nel test x pda e
104700080326     c* e nei dati x pda
104800080326     C                   if        v1ctstpda <>' '
104900080326     C                   if        (v1cpda = 'O' and
105000080326     C                             v1ctstpda <>'O') or
105100080326     C                             (v1cpda = 'C' and
105200080326     C                             v1ctstpda <>'C') or
105300080326     C                             v1cpda = 'N'
105400080326     C                   MOVEL     err(26)       $MSG
105500080326     C                   SETON                                        6990
105600080326     C                   ENDIF
105700110224     c* si può inserire il flag di test C/E solo se non si sono ricevuti
105800110224     c* esiti consegne (se già inseriti ma non ancora ricevuti, paziena !!!)
105900110224     c                   if        §dsttstpda = ' ' and
106000110224     c                             v1ctstpda <> 'O'
106100110225     c     kpct          setll     fipct02l                               69
106200110225     c                   if        *in69
106300110225     C                   MOVEL     err(27)       $MSG
106400110225     C                   SETON                                        90
106500110224     c                   end
106600110224     c                   end
106700110224     c* se si era in test con Entrambi e si vuole passare in buona con uno
106800110224     c* dei due, non si può fare
106900110530     c*                  if        §dsttstpda = 'E' and
107000110530     c*                            v1ctstpda <> 'E'
107100110530     C*                  MOVEL     err(26)       $MSG
107200110530     C*                  SETON                                        9069
107300110530     c*                  end
107400110224     c* non è possibile eliminare il flag di test
107500110224     c                   else
107600110224     c                   if        §dsttstpda <> ' ' and
107700110225     c                             (faseorm ='PDR' or fasecon = 'PDC')
107800110225     C                   MOVEL     err(27)       $msg
107900110225     C                   SETON                                        9069
108000110224     c                   end
108100110224     C                   ENDIF
108200080208     c* se procedura attiva e aut. in test avviso
108300080208     C                   if        (v1ctstpda ='E' and
108400080211     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
108500080208     C                             or (v1ctstpda ='C' and
108600080208     C                             §OGPDACON='S')
108700080208     C                             or (v1ctstpda ='O' and
108800080208     C                             §OGPDAORM='S')
108900080208     C                   eval      msgv= 'ATT. gestione PDA attiva'
109000080208     c                   else
109100080208     c                   clear                   msgv
109200080208     C                   ENDIF
109300080318     c* se aut. ha dati per pda differenti da quelli della distinta avviso
109400080319     c                   clear                   msgv1
109500080415     c                   setoff                                       45
109600101025     C                   if        (v1cpda = 'N' and
109700080319     C                             v1cpda <> §apdpda)
109800080415     C                   eval      msgv1= 'ATTENZIONE Aut. con PDA'
109900080415     c                   seton                                        45
110000080319     c                   end
110100101025     C                   if        (v1cpda <> 'N' and
110200080319     C                             §apdpda = 'N')
110300080415     C                   eval      msgv1= 'ATTENZIONE Aut.senza PDA'
110400080415     c                   seton                                        45
110500080319     c                   end
110600080212     C                   ENDIF
110700080121     c* parcel/messaggerie
110800050524     c                   if        v1ctpm = *blanks
110900150804     c                   if        *in04
111000150804     c                   movel     §apdtpm       v1ctpm
111100150804     c                   end
111200150804     c                   if        v1ctpm = ' '
111300050523     c                   movel     'P'           v1ctpm
111400050523     c                   end
111500050523     c                   end
111600160429     c* turno
111700160505     c                   if        v1ctur = *blanks and v1ctpm = 'P'
111800160502     c                   if        *in01
111900160429     c                   movel     §apdturno     v1ctur
112000160505     c                   end
112100160429     c                   if        v1ctur = ' '
112200160429     c                   movel     '1'           v1ctur
112300160429     c                   end
112400160429     c                   end
112500160505     c                   if        v1ctpm = 'M' and v1ctur <> ' '
112600160505     c                             and *in01
112700160505     C                   MOVEL     ERR(28)       $MSG
112800160505     c                   seton                                        9190
112900160505     c                   end
113000950601     C*
113100941109     C  N90APDPDD        IFNE      'S'
113200931109     C*  Se padroncino non è dipedente controllo se in filiale
113300931109     C*  di appartenza è attivata la gestione conteggi
113400941109     C                   MOVE      WFIL          ORGFIL
113500160429     C     ORGFIL        CHAIN     AZORG01L
113600160429     C                   IF        %found(azorg01l)
113700950123     C                   MOVE      WCDCPC        FTWPDR
113800931109     C                   MOVE      'C'           FTWFVL
113900931109     C                   MOVE      ' '           FTWTSR
114000931109     C                   MOVE      ROVDDC        FTWDDC
114100160429     C     KEYFT2        CHAIN     fiftt02L
114200160429     C     KEYFT2        CHAIN     fifst02L
114300160429     C                   if        %found(fiftt02l) or
114400160429     C                             %found(fifst02l)
114500940503     C                   SETON                                        9011
114600940503     C                   MOVEL     ERR(8)        $MSG
114700931109     C                   END
114800940503     C                   END
114900931109     C                   END
115000911024     C                   END
115100080318     c* se cambio l'aut ed ho spedizioni o orm sulla distinta chiedo di
115200080318     c* confermare la modifica
115300080319     C     V01NDC        IFNE      *ZEROS
115400941108     C     ARBPDC        ANDNE     CODPAD
115500950123     C     SAVCPC        ANDNE     WCDCPC
115600080319     C     *in92         ANDeq     '0'
115700080318     C     V01NDC        orNE      *ZEROS
115800080326     C     ARBPDC        ANDNE     CODPAD
115900080319     C     SAVCPC        ANDNE     WCDCPC
116000080326     C     worm          ANDeq     'S'
116100940503     C     *IN03         IFEQ      '0'
116200940503     C                   SETON                                        9013
116300940503     C                   MOVEL     ERR(11)       $MSG
116400940503     C                   ELSE
116500940503     C                   SETON                                        90
116600940503     C                   MOVEL     ERR(11)       $MSG
116700940503     C                   SETOFF                                       03
116800911104     C                   END
116900940503     C                   END
117000911017     C*
117100911105     C                   END
117200950123     C   92
117300950123     CANN90V1CCPC        IFEQ      *ZEROS
117400950123     C     V1CCPC        OREQ      *BLANKS
117500941107     C     V01NDC        IFNE      *ZEROS
117600940503     C                   SETON                                        9013
117700940503     C                   MOVEL     ERR(9)        $MSG
117800911108     C                   END
117900911108     C                   END
118000950123     C  N90V1CCPC        IFEQ      *ZEROS
118100950123     C     V1CCPC        OREQ      *BLANKS
118200941107     C     V01NDC        IFEQ      *ZEROS
118300940503     C                   SETON                                        9013
118400940503     C                   MOVEL     ERR(9)        $MSG
118500911104     C                   END
118600911105     C                   END
118700030618     c* accendo o spengo l'indicatore 36 per gestire in seguito il campo
118800030618     c* dove andrà il barcode della spedizione
118900030618     c     v1cisp        comp      'P'                                    36
119000030703     c                   if        v1cisp = 'P'
119100030703     c                   eval      v03f2 = 'F2=Manuale'
119200030703     c                   else
119300030703     c                   eval      v03f2 = 'F2=Pistola'
119400030703     c                   end
119500911017     C*
119600911017     C                   ENDSR
119700911017     C*----------------------------------------------------*
119800040901     C* Controllo data distinta                            *
119900911017     C*----------------------------------------------------*
120000040901     C     srddc         BEGSR
120100911017     C*
120200040901     C                   MOVE      V1CDDC        G02DAT
120300040901     C                   MOVEL     *BLANK        G02ERR
120400040901     C                   CALL      'XSRDA8'
120500040901     C                   PARM                    WLBDA8
120600040901     C     G02ERR        IFEQ      '1'
120700040901     C                   SETON                                        9011
120800040901     C                   MOVEL     ERR(7)        $MSG
120900040901     c                   end
121000040901     C*
121100040901     C                   ENDSR
121200040901     C*----------------------------------------------------*
121300040901     C* CARICO IL FILE DI LAVORO CON LE SPEDIZIONE DISTINTA*
121400040901     C*----------------------------------------------------*
121500040901     C     CARDIS        BEGSR
121600040901     C*
121700020905     C                   Z-ADD     V1cLNA        KFGS
121800960702     C                   Z-ADD     V01NDC        KNDC
121900960702     C                   Z-ADD     ROVDDC        KDDC
122000020905     C     KARB          SETLL     FNARB70L
122100911017     C*
122200941108     C* Azzero il contatore delle bolle in dist.
122300920220     C                   Z-ADD     0             NUM               3 0
122400911017     C                   DO        *HIVAL
122500911017     C*
122600020905     C     KARB          READE     FNARB70L                               50
122700941107     C     *IN50         IFEQ      '0'
122800160429     C     KARB1         CHAIN(N)  FNARBW2L
122900911017     C*
123000911112     C                   MOVE      '1'           ARBFLG
123100911017     C*
123200160429     c                   if        not %found(fnarbw2l) and
123300160429     C                             arbngd > NUM
123400920528     C                   Z-ADD     ARBNGD        NUM
123500920528     C                   END
123600990920     C*  Carico in schiera gli eventuali stop
123700940322     C     ARBSTP        IFNE      0
123800990920     C*  Azzero campo codice cliente
123900990920     C                   Z-ADD     0             CODCLI            7 0
124000990920     C* Verifico il tipo prima e seconda bolla
124100951207     C                   Z-ADD     1             X                 2 0
124200951207     C                   MOVEL     *BLANKS       WTBOL1
124300951207     C                   CLEAR                   DS3A
124400951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
124500951207     C     *IN13         IFEQ      '1'
124600951207     C                   MOVEL     D3A(X)        DS3A
124700951207     C                   MOVEL     §3ATB1        WTBOL1            1
124800990920     C                   MOVEL     §3ATB2        WTBOL2            1
124900951207     C                   END
125000990920     C* Se la prima bolla è un assegnato eseguo posizionamento
125100990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
125200990920     C     WTBOL1        IFEQ      'A'
125300990920     C                   MOVEL     '1'           KTRC
125400990920     C                   Z-ADD     ARBAAS        KAAS
125500990920     C                   Z-ADD     ARBAAS        KLNP
125600990920     C                   Z-ADD     ARBAAS        KNRS
125700990920     C                   Z-ADD     ARBAAS        KNSP
125800990920     C     KAR6          CHAIN     FIAR601L                           74
125900990920     C* ... se non trovo bolla o il codice cliente non è
126000990920     C* impostato reperisco il codice da FNARB
126100990920     C     *IN74         IFEQ      '1'
126200990920     C     AR6KSC        OREQ      0
126300990920     C                   Z-ADD     ARBKSC        CODCLI
126400990920     C                   ELSE
126500990920     C* ... altrimenti recupero codice cliente da FIAR6
126600990920     C                   Z-ADD     AR6KSC        CODCLI
126700990920     C                   END
126800990920     C* .... se prima bolla è un porto franco ....
126900990920     C                   ELSE
127000990920     C* .... verifico se la seconda è un assegnato
127100990920     C     WTBOL2        IFEQ      'A'
127200990920     C                   MOVEL     '2'           KTRC
127300990920     C                   Z-ADD     ARBAAS        KAAS
127400990920     C                   Z-ADD     ARBAAS        KLNP
127500990920     C                   Z-ADD     ARBAAS        KNRS
127600990920     C                   Z-ADD     ARBAAS        KNSP
127700990920     C     KAR6          CHAIN     FIAR601L                           74
127800990920     C* ... e reperisco il codice bolla da lì
127900990920     C     *IN74         IFEQ      '0'
128000990920     C     AR6KSC        ANDNE     0
128100990920     C                   Z-ADD     AR6KSC        CODCLI
128200951207     C                   END
128300990920     C                   END
128400990920     C*
128500990920     C                   END
128600990920     C* Controllo se codice bolla è codificato
128700951207     C                   Z-ADD     CODCLI        APPOG             4 0
128800951207     C     APPOG         IFNE      8888
128900951207     C     APPOG         ANDNE     9999
129000990920     C* ed è un assegnato
129100990920     C     CODCLI        ANDNE     0
129200951211     C                   MOVEL     *BLANKS       RAGCLI
129300951211     C                   MOVEL     CODCLI        RAGCLI
129400951207     C                   ELSE
129500951121     C                   MOVEL     ARBRSD        RAGCLI           70
129600951121     C                   MOVE      ARBIND        RAGCLI           70
129700951207     C                   END
129800961128     C                   Z-ADD     1             X                 2 0
129900951123     C     ARBSTP        LOOKUP    STP2(X)                                13
130000961217     C     *IN13         IFEQ      '0'
130100961217     C     W             ANDGE     999
130200961217     C                   SETON                                        79
130300961217     C                   MOVEL     ERR(20)       $ERR
130400961217     C                   END
130500940331     C     *IN13         IFEQ      '0'
130600961217     C     W             ANDLT     999
130700940331     C                   ADD       1             W
130800940322     C                   Z-ADD     ARBSTP        STP2(W)
130900951121     C                   MOVEL     RAGCLI        RSD2(W)
131000940331     C                   END
131100940322     C     ARBSTP        IFGT      ULTSTP
131200940322     C                   Z-ADD     ARBSTP        ULTSTP
131300940322     C                   END
131400940322     C                   END
131500160429     c                   if        not %found(fnarbw2l) and *in50 = '0'
131600160429     C                   WRITE     FNARBW00
131700911108     C                   END
131800160429     C                   END
131900911017     C*
132000941107     C  N50              END
132100911017     C*
132200911104     C                   SETON                                        05
132300941107     C                   FEOD      FNARBW2L
132400911017     C                   ENDSR
132500911017     C*----------------------------------------------------*
132600911017     C* GESTIONE DEL FORMATO 3                             *
132700911017     C*----------------------------------------------------*
132800940202     C     GESFM3        BEGSR
132900911017     C*
133000911017     C* Pulisco i campi che devo vidualizzare nel fm2
133100030618     C                   clear                   V03pis
133200030618     C                   MOVEL     ROVDDC        VIDANS
133300911017     C                   Z-ADD     *ZEROS        V03LNP
133400911017     C                   Z-ADD     *ZEROS        V03NRS
133500911017     C                   Z-ADD     *ZEROS        V03NSP
133600911105     C                   SETOFF                                       23
133700911108     C                   Z-ADD     *ZEROS        CTR
133800911017     C*
133900911017     C                   DO        *HIVAL
134000030701     c* NO ERRORE
134100030620     C* se non c'è errore ed è stata trovata la bolla pulisco i riferimenti
134200030620     c* della spedizione
134300030620     c                   if        not *in90
134400030620     C                   clear                   V03pis
134500030620     C                   MOVEL     ROVDDC        VIDANS
134600030620     C                   Z-ADD     *ZEROS        V03LNP
134700030620     C                   Z-ADD     *ZEROS        V03NRS
134800030620     C                   Z-ADD     *ZEROS        V03NSP
134900080415     C                   SETOFF                                       4341
135000030620     c                   else
135100030701     c* SI ERRORE
135200030701     c* se c'è ind. 90 di errore generico acceso, abblenco i campi della
135300030701     c* spedizione se ho barcode impostato e se sono entrato in modalità
135400030701     c* pistola, questo per evitare di dover abblencare i campi in caso
135500030701     c* di errore
135600030701     c                   if        v1cisp = 'P' and v03nsp <> 0 and
135700030701     c                             v03pis <> 0
135800030701     C                   MOVEL     ROVDDC        VIDANS
135900030701     C                   Z-ADD     *ZEROS        V03LNP
136000030701     C                   Z-ADD     *ZEROS        V03NRS
136100030701     C                   Z-ADD     *ZEROS        V03NSP
136200030620     c                   end
136300030701     c* se c'è ind. 90 di errore generico acceso, abblenco il campo del
136400030701     c* barcode se ho spedizione impostata e se sono entrato in modalità
136500030701     c* manuale, questo per evitare di dover abblencare i campi in caso
136600030701     c* di errore
136700030701     c                   if        v1cisp = 'M' and v03nsp <> 0 and
136800030701     c                             v03pis <> 0
136900030701     C                   Z-ADD     *ZEROS        V03pis
137000030701     c                   end
137100030701     c                   end
137200941108     C                   EXFMT     LR50D02
137300030910     C                   SETOFF                                       90
137400030701     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
137500030701     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
137600030701     c* della spedizione
137700030701     c                   if        (v03pis <> 0 and v03nsp <> 0) or
137800030701     c                             (v03pis = 0 and v03nsp = 0)
137900030701     c     v1cisp        comp      'P'                                    73
138000030702     c                   end
138100030702     c                   if        (v03pis <> 0 and v03nsp = 0) or
138200030702     c                             (v03pis = 0 and v03nsp <> 0)
138300030701     c     v03pis        comp      0                                  7373
138400030701     c                   end
138500030618     c* se il campo v03pis è impostato valorizzo i campi della spedizione
138600030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
138700030620     c* ha dato l'errore
138800030618     c                   clear                   ds_pis
138900030623     c                   if        v03pis <> 0
139000030702     c                   move      v03pis        ds_pis
139100030623     c                   if        v03nsp = 0 and
139200030620     c                             v03lnp = 0 and v03nrs = 0
139300030701     C                   MOVEL     ROVDDC        VIDANS
139400030618     c                   move      ds_pisaas     vidans
139500030618     c                   move      ds_pislnp     v03lnp
139600030618     c                   move      ds_pisnrs     v03nrs
139700030618     c                   move      ds_pisnsp     v03nsp
139800030623     c* controllo il check digits
139900030623     c                   exsr      srchkdgt
140000030623     c                   if        *in40
140100030623     c                   iter
140200030618     c                   end
140300030623     c                   else
140400030623     c                   move      vidans        com02             2 0
140500030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
140600030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
140700030623     c                   seton                                        5190
140800030623     c                   iter
140900030623     c                   end
141000030623     c                   end
141100030623     c                   end
141200950117     C*  Imposto variabili x test forzature
141300950117     C     VIDANS        IFNE      SAVAAS
141400950117     C     V03LNP        ORNE      SAVLNP
141500950117     C     V03NSP        ORNE      SAVNSP
141600950117     C     V03NRS        ORNE      SAVNRS
141700170307     c                   if        WForza = 'N'
141800160502     C                   MOVE      'N'           WFZFFD            1
141900950117     C                   MOVE      'N'           WFZAPP            1
142000950607     C                   MOVE      'N'           WFZTUR            1
142100100203     C                   MOVE      'N'           WFZatt            1
142200150929     C                   MOVE      'N'           WFZCAlert         1
142300160502     C                   MOVE      'N'           WFZCtur           1
142400031119     c                   clear                   k17
142500110803     c                   clear                   k08
142600170307     c                   end
142700950117     C                   END
142800950117     C*
142900911108     C   KL              DO
143000911108     C* 1° VOLTA
143100911108     C     CTR           IFEQ      *ZEROS
143200911108     C                   ADD       1             CTR               1 0
143300911108     C                   SETON                                        9094
143400911108     C                   ELSE
143500070410     c                   unlock    fidst09l
143600911108     C* 2° VOLTA
143700940202     C                   Z-ADD     4             FVVNPG
143800941107     C                   MOVE      V01NDC        FVVNFV
143900020905     C                   MOVE      V1cLNA        FVVFGS
144000020905     C     KFVV          CHAIN     FNFVV01L                           50
144100940209     C  N50FVVFCF        IFEQ      'A'
144200940202     C                   MOVEL     *BLANKS       FVVFCF
144300940202     C                   END
144400941109     C  N50              UPDATE    FNFVV000
144500000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
144600000515     C                   MOVE      FVVDS         FVVDS2
144700000515     C                   Z-ADD     8             FVVNPG
144800000515     C     KFVV          CHAIN     FNFVV000                           50
144900000515     C     *IN50         IFEQ      *OFF
145000000515     C                   MOVE      FVVDS2        FVVDS
145100000515     C                   Z-ADD     8             FVVNPG
145200000515     C                   UPDATE    FNFVV000
145300000515     C                   END
145400000626     C                   Z-ADD     4             FVVNPG
145500940202     C                   Z-ADD     *ZEROS        CTR
145600940202     C                   EXSR      PULRIC
145700911108     C                   END
145800911108     C                   END
145900911128     C**
146000030703     C** F2  - INserimento spedizione da manuale a pistola o viceversa
146100030703     c**       serve per il posizionamento sulla spedizione o sul barcode
146200030703     C   KB              DO
146300030703     c                   if        v1cisp = 'P'
146400030703     c                   movel     'M'           v1cisp
146500030703     c                   eval      v03f2 = 'F2=Pistola'
146600030703     c                   seton                                        90
146700030703     c                   setoff                                       7336
146800030703     c                   else
146900030703     c                   movel     'P'           v1cisp
147000030703     c                   eval      v03f2 = 'F2=Manuale'
147100030703     c                   seton                                        907336
147200030703     c                   end
147300030703     C                   ENDdo
147400030703     C**
147500030703     C** F7  - INTERROGAZIONE BOLLE ARRIVI
147600911128     C   KG              DO
147700941109     C                   MOVEL     '2'           P36FLG
147800941108     C                   Z-ADD     VIDANS        P36AAS
147900941108     C                   Z-ADD     V03LNP        P36LNP
148000941108     C                   Z-ADD     V03NSP        P36NSP
148100941108     C                   Z-ADD     V03NRS        P36NRS
148200941109     C                   MOVEL     ' '           P36F03
148300941109     C     P36NSP        IFGT      0
148400941109     C                   MOVEL     '1'           P36FLG
148500941109     C                   END
148600941108     C                   MOVEL     DSLR36        KPJBU
148700941108     C                   CALL      'FNLR36R'
148800911128     C                   PARM                    KPJBA
148900941109     C                   MOVEL     KPJBU         DSLR36
149000941109     C     P36F03        IFNE      '1'
149100941215     C     V03NSP        ANDEQ     0
149200941109     C                   Z-ADD     P36AAS        VIDANS
149300941109     C                   Z-ADD     P36LNP        V03LNP
149400941109     C                   Z-ADD     P36NRS        V03NRS
149500941109     C                   Z-ADD     P36NSP        V03NSP
149600941109     C                   END
149700911128     C                   SETON                                        90
149800911128     C                   END
149900911017     C*
150000941109     C** F13 - BLOCCO SBLOCCO SPEDIZIONI
150100920831     C   KM              DO
150200031017     c                   clear                   dslr02
150300030630     c                   exsr      srfnlr39
150400031017     C                   SETON                                        90
150500920831     C                   END
150600920206     C*
150700031125     C** F17 - ABILITAZIONE MERCE (forzatura non abilita più)
150800031119     c   kr              z-add     1             k17               1 0
150900110803     C** F08 - forzatura immissione in distinta x sped. perfect order
151000110803     c   kh              z-add     1             k08               1 0
151100920206     C*
151200941108     C* F12 Videata precedente
151300911128     C  NKL
151400911128     CANN90              DO
151500940202     C                   Z-ADD     *ZEROS        CTR
151600911017     C*
151700911017     C* Controllo se la spedizione è esistente o valida su video
151800911017     C*
151900911017     C     VIDANS        IFEQ      *ZEROS
152000911017     C                   SETON                                        1790
152100911017     C                   END
152200911017     C     V03LNP        IFEQ      *ZEROS
152300911017     C                   SETON                                        1490
152400911017     C                   END
152500911017     C     V03NSP        IFEQ      *ZEROS
152600911127     C                   SETON                                        2990
152700911017     C                   END
152800911017     C* N90 CERCO LA SPEDIZIONE NELL'ARCHIVIO ARB00 E ARB50
152900911017     C*
153000911023     C  N90              DO
153100911017     C                   Z-ADD     VIDANS        ARBAAS
153200911017     C                   Z-ADD     V03LNP        ARBLNP
153300911017     C                   Z-ADD     V03NRS        ARBNRS
153400911017     C                   Z-ADD     V03NSP        ARBNSP
153500030909     C* AGGANCIO FNARB
153600940207     C                   SETOFF                                       86
153700941108     C     KARB1         CHAIN(N)  FNARB01L                           15
153800030623     C  N15              DO
153900030909     c* se non ci sono errori controllo che la spedizione non sia già
154000030909     c* stata inserita in QUESTA distinta
154100030909     C                   if        not *in90
154200030909     C     KARB1         CHAIN(N)  FNARBW2L
154300030909     c                   if        %found(fnarbw2l)
154400030909     c                   seton                                        5790
154500030909     c                   end
154600030909     c                   end
154700941108     C*  Reperisco Tipo Bolla
154800941108     C                   Z-ADD     1             X                 2 0
154900941108     C                   CLEAR                   DS3A
155000941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
155100941108     C     *IN13         IFEQ      '1'
155200941109     C                   MOVEL     D3A(X)        DS3A
155300941108     C                   END
155400940322     C     *IN90         IFEQ      '0'
155500940322     C     ARBNDC        ANDNE     *ZEROS
155600941107     C     ARBNDC        ANDNE     V01NDC
155700930621     C     ARBNMI        ANDEQ     0
155800141112     c*
155900031125     c                   clear                   fnlr98ds
156000031125     c                   z-add     arbaas        i98aas
156100031125     c                   z-add     arblnp        i98lnp
156200031125     c                   z-add     arbnsp        i98nsp
156300031125     c                   z-add     arbnrs        i98nrs
156400110125     c                   z-add     arbifp        i98fgs
156500110125     c                   z-add     4             i98npg
156600110125     c                   z-add     arbndc        i98ndc
156700031125     c                   movel     fnlr98ds      kpjbu
156800031125     c                   call      'FNLR98C'
156900031125     c                   parm                    kpjba
157000031125     c                   movel     kpjbu         fnlr98ds
157100031125     c                   if        o98esito <> '0'
157200930520     C                   SETON                                        2490
157300031125     c                   endif
157400930520     C                   END
157500940322     C*
157600941108     C* Controllo se linea di arrivo abilitante
157700911129     C  N90ARBLNA        LOOKUP    LIN                                    70
157800911127     C  N70              SETON                                        2590
157900941108     C* Controllo che la spedizione non sia una mamma
158000110804     C     KARB1         CHAIN     FNLBL02L                           74
158100110804     C  N74              SETON                                        9089
158200110804     C   74              SETOFF                                       89
158300941108     C*----------------------------------------------------*
158400050613     C*  errore per spedizioni V.PACCO
158500050613     c                   if        arbrsd = 'VEDI PACCO' and not *in90
158600050613     c                   seton                                        5890
158700050613     c                   end
158800941108     C* Controlli x bolle da consegnare
158900941108     C     ARBDCM        IFEQ      0
159000941108     C* Controllo il livello di abilitazione merce
159100941108     C  N90ARBAMA        IFEQ      *ZEROS
159200040312     c     arbdam        andeq     0
159300911127     C                   SETON                                        2690
159400911105     C                   END
159500040312     C  N90ARBAMA        IFEQ      *ZEROS
159600040312     c     arbdam        andne     0
159700040312     c     k17           andeq     0
159800040312     C                   SETON                                        6890
159900040312     C                   END
160000110803     c* perfect order: se si inserisce una spedizione avviso + f8 x forzare
160100130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
160200130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
160300130221     C**N90ARBxco*****   IFne      ' '
160400130221     C  N90ARBxco        IFgt      '0'
160500110803     c     arbntc        andeq     0
160600110803     c     k08           andeq     0
160700110803     C                   SETON                                        6590
160800110803     C                   END
160900941108     C* Controllo che la data di consegna prevista <=  alla data distinta
161000030701     C  n90ARBDCR        IFGT      ROVDDC
161100021015     C     ARBTCR        andne     'P'
161200030701     C                   SETON                                        4490
161300920218     C                   END
161400941108     C* Se data consegna richiesta ha come flag dopo controllo
161500030701     C  n90ARBTCR        IFEQ      'D'
161600920306     C     ARBDCR        ANDEQ     ROVDDC
161700030701     C                   SETON                                        4490
161800920218     C                   END
161900160316     c* se spedizione con giro "VP" vedi pacco DPD, deve esssere
162000160316     c* in una distinta di un autista fittizio
162100160316     c     karb1         chain(n)  fiarg01l
162200160316     c                   if        %found(fiarg01l) and argcgi = §nsdcgi
162300160316     c                             and apdpdd <> 'S'
162400160316     C                   SETON                                        9990
162500160316     c                   end
162600150929      ****
162700030909     C* controllo gli errori che richiedono una forzatura
162800030909     c  n90              do
162900950117     C*----------------------------------------------------*
163000040217     C                   SETOFF                                       4667
163100030909     C     *INKK         IFEQ      '1'
163200950607     C                   MOVEL     'S'           WFZAPP
163300950117     C                   ELSE
163400950607     C     WFZAPP        IFEQ      'N'
163500950607     C     ARBTC1        IFEQ      'A'
163600950607     C     ARBTC2        OREQ      'A'
163700950607     C* Controllo che ci sia la consegna richiesta x consegna
163800950607     C* con appuntamento
163900030909     C     ARBDCR        IFEQ      0
164000950607     C     ARBTCR        ANDEQ     ' '
164100950607     C     ARBHCR        ANDEQ     0
164200040217     c                   exsr      numtel
164300040217     c                   if        §ar5teld = *blank
164400030910     C                   SETON                                        4690
164500030909     c                   leave
164600040217     c                   else
164700040217     c                   seton                                        6790
164800040217     c                   endif
164900950607     C                   END
165000950607     C                   END
165100950607     C*
165200950607     C                   END
165300950607     C                   END
165400030909     C*----------------------------------------------------*
165500030910     C                   SETOFF                                       47
165600030909     C     *INKF         IFEQ      '1'
165700030909     C                   MOVEL     'S'           WFZFFD
165800170307     C                   MOVE      'S'           WForza            1
165900030909     C                   ELSE
166000030909     C     WFZFFD        IFEQ      'N'
166100030909     C* Controllo se la spedizione è in fermo deposito e lo segnalo
166200030909     C     ARBFFD        IFEQ      'S'
166300051107     C     ARBFFD        oreq      'M'
166400030910     C                   SETON                                        4790
166500030909     c                   leave
166600030909     C                   END
166700030909     C                   END
166800030909     C                   END
166900950607     C*----------------------------------------------------*
167000030910     C                   SETOFF                                       42
167100030909     c     *INKT         IFEQ      '1'
167200950607     C                   MOVEL     'S'           WFZTUR
167300170307     C                   MOVE      'S'           WForza            1
167400950607     C                   ELSE
167500950607     C     WFZTUR        IFEQ      'N'
167600030909     C     ARBGC1        IFNE      *BLANKS
167700950607     C                   MOVEL     ARBGC1        WGIOR             1
167800950607     C     WGIOR         IFEQ      WGG
167900030910     C                   SETON                                        4290
168000030909     c                   leave
168100950607     C                   END
168200950607     C                   END
168300030909     C     ARBGC2        IFNE      *BLANKS
168400950607     C                   MOVEL     ARBGC2        WGIOR             1
168500950607     C     WGIOR         IFEQ      WGG
168600030910     C                   SETON                                        4290
168700030909     c                   leave
168800950607     C                   END
168900950607     C                   END
169000950607     C                   END
169100950607     C                   END
169200150929     C*----------------------------------------------------*
169300100203     C* controllo se sepdizione ha particolarità cons. di attesa
169400100203     C                   SETOFF                                       48
169500100203     c     *INKn         IFEQ      '1'
169600100203     C                   MOVEL     'S'           WFZatt
169700170307     C                   MOVE      'S'           WForza            1
169800100203     C                   ELSE
169900100203     C     WFZatt        IFEQ      'N'
170000100203     C     ARBGma        andne     *BLANKS
170100100203     C     ARBTCR        andeq     'P'
170200100203     C     ARBdcr        IFGT      rovddc
170300130429     c                   clear                   tnsd99ds
170400130424     C                   Z-ADD     ROVDDC        D98DFC
170500130424     C                   Z-ADD     2359          D98OFC
170600130424     C                   MOVEL     ' '           D98TLA
170700130424     C                   MOVEL     'A'           D98TBO
170800130424     C                   Z-ADD     ARBAAS        D98AAS
170900130424     C                   Z-ADD     ARBLNP        D98LNP
171000130424     C                   Z-ADD     ARBNRS        D98NRS
171100130424     C                   Z-ADD     ARBNSP        D98NSP
171200130424     C                   CALL      'TNSD99R'
171300130424     C                   PARM                    TNSD99DS
171400130424     c                   if        d98nci <=-24
171500100203     C     arbgma        lookup    s7r                                    48
171600100203     C                   IF        *in48
171700100203     C                   SETON                                        90
171800100203     c                   leave
171900100203     C                   END
172000100203     C                   END
172100100203     C                   END
172200100203     C                   END
172300100203     C                   END
172400150929     C*----------------------------------------------------*
172500150929     C* controllo se mandato un contro ALERT nello stesso giorno
172600150929     C                   SETOFF                                       53
172700150929     c     *INKP         IFEQ      '1'
172800150929     C                   MOVEL     'S'           WFZCAlert
172900170307     C                   MOVE      'S'           WForza            1
173000150929     C                   ELSE
173100150929     C     WFZCAlert     IFEQ      'N'
173200150929     c                   exsr      contro_alert
173300150929     c   53              leave
173400150929     C                   END
173500150929     C                   END
173600160502     C* controllo se la spedizione è in una zona del secondo turno
173700160502     C                   SETOFF                                       59
173800160502     c     *INKi         IFEQ      '1'
173900160502     C                   MOVEL     'S'           WFZCtur
174000170307     C                   MOVE      'S'           WForza            1
174100160502     C                   ELSE
174200160502     C     WFZCtur       IFEQ      'N'
174300160502     c                   exsr      contro_turno
174400160502     c   59              leave
174500160502     C                   END
174600160502     C                   END
174700150929     C*----------------------------------------------------*
174800150929      ***
174900030909     c                   enddo
175000150929      **
175100030909     c* imposto i campi salvati per forzatura
175200030909     c                   if         *in42 or *in47 or *in46
175300100203     c                              or *in48
175400170307     c                              or *in53 or *in59
175500030909     C                   z-add     VIDANS        SAVAAS
175600030909     C                   z-add     V03LNP        SAVLNP
175700030909     C                   z-add     V03NSP        SAVNSP
175800030909     C                   z-add     V03NRS        SAVNRS
175900170307     C                   MOVE      'N'           WForza            1
176000030909     c                   end
176100950117     C*----------------------------------------------------*
176200941108     C* Controllo abilitazione incasso contrassegno e assegnato
176300941108     C  N90ARBACC        IFNE      'S'
176400911127     C                   SETON                                        2790
176500911105     C                   END
176600911105     C*
176700941108     C  N90ARBACA        IFNE      'S'
176800911127     C                   SETON                                        2890
176900911105     C                   END
177000911114     C*
177100941108     C  N90ARBFDC        IFNE      ' '
177200930513     C     ARBDCP        ANDEQ     0
177300911127     C                   SETON                                        2490
177400911114     C                   END
177500030630     c* se c'è un lasciato avviso sblocco in automatico la spedizione
177600030630     c                   IF        arbfbc = 'A'
177700031017     c                   clear                   dslr02
177800031017     c                   eval      dr2vid = 'N'
177900030630     c                   exsr      srfnlr39
178000031017     c                   if        dr2err = *blanks
178100030630     c                   clear                   arbcmc
178200030630     c                   clear                   arbfbc
178300030630     c                   end
178400030630     c                   end
178500941108     C* Controllo se bolla abilitata oppure no alla consegna
178600911127     C  N90ARBFBC        IFNE      *BLANK
178700911127     C                   SETON                                        2290
178800911105     C                   END
178900920124     C*
179000150929     C* Controlla se presente un contro Alert per il giorno stesso FORZABILE
179100150929     C*
179200150929     C*
179300941108     C                   ELSE
179400941108     C*----------------------------------------------------*
179500941108     C* Bolla già consegnata
179600941108     C*----------------------------------------------------*
179700990913     C* Controllo abilitazione incasso contrassegno
179800990913     C  N90ARBACC        IFNE      'S'
179900990913     C                   SETON                                        2790
180000990913     C                   END
180100990913     C* CONTROLLO SE C'E' ASSEGNATO DA INCASSARE
180200990913     C  N90ARBACA        IFNE      'S'
180300990913     C                   SETON                                        2890
180400990913     C                   END
180500920220     C* CONTROLLO SE I FLAG DI INCASSO ICA / ICC SONO DA PRENDERE IN
180600930427     C* CONSIDERAZIONE (NEL CASO DI CONSEGNA PARZIALE O CONSEGNA CON
180700930427     C* RITORNO ALL'INCASSO)
180800940207     C  N24ARBFIP        IFNE      'I'
180900911129     C     ARBICC        IFEQ      ' '
181000941108     C     ARBICA        ANDNE     'R'
181100941108     C*  Controllo se c'è contrassegno
181200941108     C                   Z-ADD     0             AR9CAS
181300941108     C     §3AFCA        IFNE      0
181400051115     C     KARB1         CHAIN     FIAR901L                           08
181500941108     C                   END
181600941108     C     AR9CAS        IFEQ      *ZEROS
181700940207     C                   SETON                                        249086
181800911129     C                   END
181900941108     C                   END
182000911129     C*
182100941108     C  N24ARBICC        IFNE      'R'
182200941108     C     ARBICA        ANDNE     'R'
182300920120     C                   SETON                                        2490
182400920120     C                   END
182500920124     C                   END
182600941108     C*----------------------------------------------------*
182700930428     C                   END
182800941108     C*----------------------------------------------------*
182900941109     C     KARB1         CHAIN     FNLBL01L                           87
183000940207     C     *IN87         IFEQ      '0'
183100950126     C     ARBLNA        ANDEQ     LBLLAP
183200941108     C     KLBL          CHAIN(N)  FNARB01L                           87
183300980429     C*
183400980429     C     *IN87         IFEQ      '0'
183500980429     C     ARBCCA        ANDEQ     '2'
183600980429     C     *IN87         OREQ      '0'
183700980429     C     ARBCCA        ANDEQ     '6'
183800980429     C                   SETON                                        87
183900980430     C     KARB2         CHAIN     FNARB01L                           30
184000980429     C                   END
184100940208     C*
184200941108     C     *IN87         IFEQ      '0'
184300941108     C*  Reperisco Tipo Bolla
184400941108     C                   Z-ADD     1             X                 2 0
184500941108     C                   CLEAR                   DS3A
184600941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
184700941108     C     *IN13         IFEQ      '1'
184800941109     C                   MOVEL     D3A(X)        DS3A
184900941108     C                   END
185000941108     C*  Controllo se c'è contrassegno
185100941108     C                   Z-ADD     0             AR9CAS
185200941108     C     §3AFCA        IFNE      0
185300051115     C     KARB1         CHAIN     FIAR901L                           08
185400941108     C                   END
185500940208     C*  Controllo se la mamma è già stata consegnata ma non ancora
185600940208     C*  contabilizzata
185700941108     C     ARBICC        IFEQ      'S'
185800941108     C     ARBNMI        ANDEQ     0
185900941108     C     AR9CAS        ANDNE     *ZEROS
186000941108     C     ARBICC        OREQ      'I'
186100940208     C     ARBNMI        ANDEQ     0
186200941108     C     AR9CAS        ANDNE     *ZEROS
186300940208     C                   SETON                                        9038
186400940208     C                   END
186500941108     C                   END
186600940208     C*
186700950223     C  N87
186800950223     CANN38ARBICC        IFEQ      *BLANKS
186900941108     C     AR9CAS        ANDNE     0
187000941108     C     ARBICC        OREQ      'R'
187100941108     C     AR9CAS        ANDNE     0
187200940208     C   86              SETOFF                                       9024
187300940208     C*  Controllo se la mamma della spedizione inserita ha un'altra
187400940208     C*  figlia inserita in un'altra distinta
187500941108     C     KARB1         CHAIN     FNLBL02L                           85
187600940208     C     *IN85         DOWEQ     '0'
187700940208     C     *IN39         ANDEQ     '0'
187800940208     C*   Se ho trovato una figlia diversa dall' originale mi posiziono
187900940208     C*   su di essa e controllo se ha impostato il flag di ditinta chiusa
188000940208     C     VIDANS        IFNE      LBLAAN
188100940208     C     V03LNP        ORNE      LBLLPN
188200940208     C     V03NRS        ORNE      LBLNRN
188300940208     C     V03NSP        ORNE      LBLNSN
188400100514     C     KARB2         CHAIN(N)  FNARB01L                           84
188500940208     C     *IN84         IFEQ      '0'
188600940208     C     ARBNDC        ANDNE     0
188700940208     C     ARBFDC        ANDEQ     ' '
188800940208     C                   SETON                                        3990
188900940208     C                   END
189000940208     C                   END
189100940208     C*   Leggo la figlia successiva
189200940208     C     *IN39         IFEQ      '0'
189300940208     C                   Z-ADD     LBLAAP        ARBAAS
189400940208     C                   Z-ADD     LBLLPP        ARBLNP
189500940208     C                   Z-ADD     LBLNRP        ARBNRS
189600940208     C                   Z-ADD     LBLNSP        ARBNSP
189700941108     C     KARB1         READE     FNLBL02L                               85
189800940208     C                   END
189900940208     C                   END
190000940208     C*   reimposto la chiave per il posizionamento sulla figlia originale
190100940208     C                   Z-ADD     VIDANS        LBLAAN
190200940208     C                   Z-ADD     V03LNP        LBLLPN
190300940208     C                   Z-ADD     V03NRS        LBLNRN
190400940208     C                   Z-ADD     V03NSP        LBLNSN
190500940208     C                   END
190600940208     C*
190700941108     C* Riaggancio la figlia
190800941108     C     KLBL1         CHAIN(N)  FNARB01L                           87
190900920324     C                   END
191000941108     C                   END
191100930429     C*
191200941108     C* Per record esistente controllo se è valido alla consegna
191300941108     C* controllo se ho già caricato al file di lavoro
191400911127     C  N90              DO
191500940322     C*
191600940322     C*  Richiamo routine per gestione attribuzione Stop
191700940322     C                   EXSR      GESSTP
191800941108     C     KARB1         CHAIN(N)  FNARBW2L                           52
191900911113     C                   MOVE      ' '           ARBFLG
192000930615     C   52
192100930615     CANN15              ADD       1             NUM
192200930615     C   52
192300930615     CANN15              Z-ADD     NUM           ARBNGD
192400941108     C   52
192500941108     CANN15              WRITE     FNARBW00
192600911105     C                   SETOFF                                       23
192700911105     C                   END
192800911017     C*
192900911023     C                   END
193000911022     C*
193100941108     C* Per F10 vado in visualizzazione subfile
193200941108     C   KJ              DO
193300911106     C                   SETOFF                                       90
193400941108     C                   FEOD      FNARBW2L
193500911024     C                   EXSR      GESSBF
193600911106     C                   SETOFF                                       93
193700950208     C   81              GOTO      EFM3
193800911024     C                   END
193900911017     C                   END
194000911108     C   94
194100911108     CAN KL
194200911108     CORNKL              END
194300911017     C*
194400911024     C     EFM3          TAG
194500911017     C                   ENDSR
194600040217     C*-----------------------------------------------------*
194700040217     C* controllo numero di telefono se appuntamento
194800040217     C*-----------------------------------------------------*
194900040217     C     numtel        BEGSR
195000040217      * rekord GEN riferimenti destinatario
195100040217     C                   clear                   dar5gen
195200040217     C                   move      'GEN'         trcar5
195300040217     C     Kfiar5        CHAIN     FiAR501L                           43
195400040217     C     *IN43         IFEQ      '0'
195500040217     C                   MOVEL     ar5uni        dar5gen
195600040217     C                   endif
195700040217     C                   ENDSR
195800150929     C*-----------------------------------------------------*
195900150929     C* controllo se mandato un CONTRO Alert al CLIENTE
196000150929     C*-----------------------------------------------------*
196100150929     C     contro_alert  BEGSR
196200150929      * rec.EMD   invio Alert e contro Alert
196300150929     C                   setoff                                       53
196400150929     C                   clear                   dar5emd
196500150929     C                   move      'EMD'         trcar5
196600150929     C     Kfiar5        CHAIN     FiAR501L
196700150929     C                   IF        %Found(FiAR501L)
196800150929     C                   MOVEL     ar5uni        dar5emd
196900150929     c                   if        §AR5ICA ='S'
197000150929     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
197100150929     c                   move      §AR5DPC       data8DPC          8 0
197200150929     c                   if        data8DPC = Dateu
197300150929     C                   seton                                        5390
197400150929     C                   end
197500150929     C                   end
197600150929     C                   end
197700150929     C                   endif
197800150929     C                   ENDSR
197900160502     C*-----------------------------------------------------*
198000160502     C* controllo se spedizone è del secondo turno
198100160502     C*-----------------------------------------------------*
198200160502     C     contro_turno  BEGSR
198300160502     c*
198400160502     C                   setoff                                       59
198500160502     C                   if        v1ctpm = 'P' and
198600160502     c                             v1ctur = '1' and *in01
198700160510     c                             and d17tur1 <> 'U' and d17tur2 <> 'U'
198800160502     c                   clear                   ds03
198900160502     C                   eval      tblcod = '03'
199000160502     C                   movel     v1cfpd        com05             5
199100160502     C                   move      arbznc        com05             5
199200160502     C                   movel(p)  com05         tblkey
199300160502     C     ktab          chain     Tabel00f
199400160502     c                   if        %found(tabel00f)
199500160502     C                   MOVEL     TBLUNI        DS03
199600160502     c                   if        §03TPM = ' ' and
199700160502     c                             §03Turno = '2'
199800160502     C                   seton                                        5990
199900160502     c                   end
200000160502     c                   end
200100160502     c                   end
200200160502     C                   ENDSR
200300911018     C*-----------------------------------------------------*
200400030623     C* controllo check digits
200500911018     C*-----------------------------------------------------*
200600030623     C     srchkdgt      BEGSR
200700030623     c                   movel     ds_pis        barcode
200800030623     C                   clear                   trul28ds
200900030623     c                   eval      i28mod = 'BAR'
201000030623     c                   eval      i28cod = barcode
201100030623     c                   call      'TRUL28R1'
201200030623     c                   parm                    trul28ds
201300030623     c* se reperito barcode imposto i campi in stampa
201400030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
201500030623     c                   seton                                        4090
201600030623     c                   end
201700030623     C                   ENDSR
201800030630     C*-----------------------------------------------------*
201900030630     C* chiamata a fnlr39r
202000030630     C*-----------------------------------------------------*
202100030630     C     srfnlr39      BEGSR
202200030630     C                   Z-ADD     VIDANS        DR2AAS
202300030630     C                   Z-ADD     V03LNP        DR2LNP
202400030630     C                   Z-ADD     V03NRS        DR2NRS
202500030630     C                   Z-ADD     V03NSP        DR2NSP
202600030630     C                   Z-ADD     V1CLNA        DR2FGS
202700030630     C                   MOVEL     '1'           DR2RIC
202800030630     C                   MOVEL     DSLR02        KPJBU
202900030630     C                   CALL      'FNLR39R'
203000030630     C                   PARM                    KPJBA
203100031017     c                   movel     kpjbu         dslr02
203200030630     C                   ENDSR
203300030623     C*-----------------------------------------------------*
203400030623     C* GESTIONE DEL SUBFILE E DELLE VIDEATE A LUI COLLEGATE*
203500030623     C*-----------------------------------------------------*
203600030623     C     GESSBF        BEGSR
203700911018     C*
203800911104     C                   SETON                                        64
203900911106     C                   Z-ADD     *ZEROS        CTR
204000930608     C                   DO        *HIVAL
204100911018     C*
204200941108     C* Pulizia del subfile per il caricamento
204300930608     C  N75
204400930608     CANN18              DO
204500940323     C                   Z-ADD     1             NRRKEY
204600911018     C                   SETON                                        62
204700941108     C                   WRITE     LR50C03
204800911018     C                   SETOFF                                       62
204900911018     C                   Z-ADD     *ZEROS        NRR               4 0
205000911018     C*
205100911018     C                   EXSR      CARSFL
205200911018     C*
205300911104     C     NRR           COMP      *ZEROS                             60
205400911104     C                   SETON                                        61
205500930608     C                   END
205600911023     C*
205700911104     C     PRIMA         TAG
205800930608     C                   DO
205900941108     C                   WRITE     LR50Z03
206000941108     C                   EXFMT     LR50C03
206100911023     C*
206200940322     C                   MOVEL     *BLANKS       $MSG
206300911023     C                   SETOFF                                       767775
206400940322     C                   SETOFF                                       189016
206500911023     C*
206600911023     C*
206700941108     C* F12 Controllo se digitati la prima volta mando un messaggio di
206800941108     C* avvertimento
206900911106     C  N81              Z-ADD     *ZEROS        CTR
207000911106     C   81              DO
207100911106     C* 1° VOLTA
207200911106     C     CTR           IFEQ      *ZEROS
207300911106     C                   ADD       1             CTR               1 0
207400940322     C                   SETON                                        909475
207500940322     C                   MOVEL     ERR(1)        $MSG
207600911106     C                   SETOFF                                       81
207700911106     C                   ELSE
207800070410     c                   unlock    fidst09l
207900911106     C* 2° VOLTA
208000950224     C                   Z-ADD     4             FVVNPG
208100950224     C                   MOVE      V01NDC        FVVNFV
208200020905     C                   Z-ADD     V1cLNA        FVVFGS
208300020905     C     KFVV          CHAIN     FNFVV01L                           50
208400950224     C  N50FVVFCF        IFEQ      'A'
208500950224     C                   MOVEL     *BLANKS       FVVFCF
208600950224     C                   UPDATE    FNFVV000
208700950224     C                   END
208800000515      *GESTIONE FOGLIO 8 (RIENTRI) PARALLELA A FOGLIO 4
208900000515     C                   MOVE      FVVDS         FVVDS2
209000000515     C                   Z-ADD     8             FVVNPG
209100000515     C     KFVV          CHAIN     FNFVV000                           50
209200000515     C     *IN50         IFEQ      *OFF
209300000515     C                   MOVE      FVVDS2        FVVDS
209400000515     C                   Z-ADD     8             FVVNPG
209500000515     C                   UPDATE    FNFVV000
209600000515     C                   END
209700000626     C                   Z-ADD     4             FVVNPG
209800000515      *
209900911106     C                   Z-ADD     *ZEROS        CTR
210000911106     C                   EXSR      PULRIC
210100911106     C                   END
210200911106     C                   END
210300911023     C*
210400941108     C* F12 Torno prima videata richieata bolla singola
210500941108     C* F10 Vado videata selezione soedizione
210600941108     C   KJ              SETON                                        93
210700911106     C   81
210800941108     COR KJ              GOTO      ENDGES
210900911104     C* PER COMANDO 22 SPROTEGGO I CAMPI DEL GIRO E DELLO STOP
211000911104     C   KW              SETOFF                                       64
211100911104     C* PER COMANDO 16 ANNULLAMENTO DISTINTA DEVE ESSERE ABILITATO
211200911104     C* SOLO IN CASO DI VARIAZIONE
211300911106     C* PER ANNULLAMNETO CHIEDO CONFERMA
211400911106     C   KQ              DO
211500941108     C                   EXFMT     LR50D03
211600911106     C     V08SNO        IFEQ      'SI'
211700911106     C                   EXSR      ANNUL
211800920423     C  N90              MOVEL     *BLANK        SAVCPC
211900941107     C  N90              MOVEL     *BLANK        V1CCPC
212000941107     C  N90              MOVE      V01NDC        V1CNDP
212100941107     C  N90              MOVEL     *BLANK        V1CNDC
212200940322     C  N90              SETON                                        8175
212300000717     C   90
212400000717     CANN78              MOVEL     ERR(2)        $MSG
212500911106     C                   ELSE
212600911106     C                   SETON                                        90
212700911106     C                   END
212800911106     C                   END
212900911023     C* SE DATO ENTER LEGGO I CAMPI DEL SUBFILE
213000911023     C*
213100930608     C  N81
213200930608     CAN 60
213300930608     CANN90              DO
213400911023     C*
213500911023     C                   SETOFF                                       54
213600911023     C*
213700930608     C                   DO        *HIVAL
213800911023     C*
213900941108     C                   READC     LR50S03                                54
214000930608     C  N54              DO
214100911022     C* CONTROLLO LE OPZIONI INSERITE
214200911018     C*
214300941108     C     V3CSEL        IFEQ      'E'
214400911023     C                   SETON                                        77
214500930608     C                   END
214600911022     C*
214700941108     C     V3CSEL        IFEQ      'I'
214800911023     C                   SETON                                        76
214900930608     C                   END
215000911022     C*
215100941108     C     V3CSEL        IFEQ      'V'
215200941108     C                   MOVEL     '1'           P36FLG
215300941108     C                   Z-ADD     V3CAAS        P36AAS
215400941108     C                   Z-ADD     V3CLNP        P36LNP
215500941108     C                   Z-ADD     V3CNSP        P36NSP
215600941108     C                   Z-ADD     V3CNRS        P36NRS
215700941108     C                   MOVEL     DSLR36        KPJBU
215800941108     C                   CALL      'FNLR36R'
215900911105     C                   PARM                    KPJBA
216000911022     C*
216100911022     C* MUOVO I PARAMETRI CHE SERVONO
216200911022     C* FACCIO LA CALL AL PROGRAMMA DI VISUALIZZAZIONE
216300930608     C                   END
216400911022     C*
216500940322     C     *IN76         IFEQ      '1'
216600940322     C     *IN77         ANDEQ     '1'
216700940323     C                   SETON                                        7518
216800940322     C                   MOVEL     ERR(3)        $MSG
216900940322     C                   END
217000940322     C  N76
217100940322     CANN77              SETOFF                                       18
217200940322     C*
217300940322     C*  Controllo gli Stop !!!
217400940322     C                   EXSR      CTRSTP
217500110804     c* perfect order
217600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
217700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
217800130221     c*****v3cxco        comp      ' '                                8888
217900130221     c     v3cxco        comp      '0'                                88
218000911023     C*
218100941108     C                   UPDATE    LR50S03
218200911022     C*
218300930608     C                   END
218400930608     C  N54              END
218500911023     C*
218600911023     C* SE NON CI SONO ERRORI
218700911021     C*
218800940322     C  N75
218900940322     CANN18              Z-ADD     0             NRR
219000940322     C  N75
219100940322     CANN18              DO        *HIVAL
219200911023     C                   ADD       1             NRR
219300911018     C*
219400941108     C     NRR           CHAIN     LR50S03                            54
219500911018     C*
219600930608     C  N54              DO
219700911018     C* CONTROLLO LE OPZIONI INSERITE
219800941109     C                   MOVE      V3CAAS        ARBAAS
219900941109     C                   MOVE      V3CLNP        ARBLNP
220000941109     C                   MOVE      V3CNRS        ARBNRS
220100941109     C                   MOVE      V3CNSP        ARBNSP
220200941109     C*
220300941108     C     V3CSEL        IFEQ      ' '
220400930608     C     *IN76         IFEQ      '1'
220500911023     C* CANCELLO IL RECORD DI LAVORO
220600941108     C     KARB1         DELETE    FNARBW2L                           55
220700911023     C                   END
220800941109     C*
220900930608     C     *IN77         IFEQ      '1'
221000940322     C     *IN77         OREQ      '0'
221100940322     C     *IN76         ANDEQ     '0'
221200911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
221300941108     C     KARB1         CHAIN     FNARBW2L                           55
221400941109     C     *IN55         IFEQ      '0'
221500941109     C                   Z-ADD     V3CNGD        ARBNGD
221600941109     C                   Z-ADD     V3CSTP        ARBSTP
221700941109     C                   MOVE      '1'           ARBFLG
221800941109     C                   UPDATE    FNARBW2
221900930608     C                   END
222000941109     C                   END
222100940322     C                   END
222200911023     C*
222300941108     C     V3CSEL        IFEQ      'E'
222400141112     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
222500141112     c                   clear                   fnlr98ds2
222600141112     c                   z-add     arbaas        i982aas
222700141112     c                   z-add     arblnp        i982lnp
222800141112     c                   z-add     arbnsp        i982nsp
222900141112     c                   z-add     arbnrs        i982nrs
223000141112     c                   call      'FNLR98R2'
223100141112     c                   parm                    fnlr98ds2
223200141112     c                   if        o982esito = '0'
223300911023     C* CANCELLO IL RECORD DI LAVORO
223400941108     C     KARB1         DELETE    FNARBW2L                           55
223500930623     C* AGGIORNO LE EVENTUALI ANOMALIE SULLA BOLLA
223600931227     C  N55              EXSR      ANOM
223700141112     c                   endif
223800911023     C*
223900930608     C                   END
224000911023     C*
224100941108     C     V3CSEL        IFEQ      'I'
224200911023     C* AGGIORNO IL FILE DI LAVORO CON GIRO E STOP
224300941108     C     KARB1         CHAIN     FNARBW2L                           55
224400941109     C  N55              Z-ADD     V3CNGD        ARBNGD
224500941109     C  N55              Z-ADD     V3CSTP        ARBSTP
224600941109     C  N55              MOVE      '1'           ARBFLG
224700941108     C  N55              UPDATE    FNARBW2
224800930608     C                   END
224900911018     C*
225000930608     C                   END
225100930608     C  N54              END
225200930608     C                   END
225300911108     C*
225400911108     C* PULIZIA DEL SUBFILE PER IL CARICAMENTO
225500911108     C*
225600941110     C   KF
225700941110     CANN75              DO
225800911108     C                   SETON                                        62
225900941108     C                   WRITE     LR50C03
226000911108     C                   SETOFF                                       62
226100911108     C                   Z-ADD     *ZEROS        NRR               4 0
226200911108     C*
226300940323     C                   Z-ADD     1             NRRKEY
226400911108     C                   EXSR      CARSFL
226500911108     C* PER COMANDO 1 RICARICO IL SUBFILE ED EMETTO FORMATO DI TOTALE
226600911108     C                   SETOFF                                       06
226700911108     C                   EXSR      GESFM6
226800941108     C                   WRITE     LR50Z03
226900911108     C                   END
227000930608     C                   END
227100930608     C  N81              END
227200911018     C*
227300911104     C     ENDGES        ENDSR
227400911018     C*-----------------------------------------------------*
227500070309     C* aggiorno file distinte FIDST00F                     *
227600911018     C*-----------------------------------------------------*
227700070309     C     srfidst       BEGSR
227800070309     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
227900070309     c* se data distinte automatiche < oggi p.o. partito
228000090310     c*                  if        comdda <> 0
228100070309     C* F I D S T 0 0 F
228200070309     c                   clear                   fidstds
228300070312     c                   eval      wnfv = fvvnfv
228400070410     C     KDST2         CHAIN     fidst09l
228500120718     C                   eval      £dstpdr = apdpdr
228600070309     C                   MOVEL     'A'           £DSTtipa
228700120718     c                   eval      £dstnot = d55not
228800070309     C                   eval      £dstnpg = fvvnpg
228900070309     C                   eval      £dstfgs = fvvFGS
229000070309     C                   eval      £DSTNfv = fvvnfv
229100070309     C                   eval      £dstDFV = fvvdfv
229200070309     C                   MOVEL     FVVFSC        £dstFca
229300070309     C                   MOVEL     FVVFPP        £dstFPP
229400070309     C                   MOVEL     FVVPEP        £dstpEP
229500080121     C                   MOVEL     v1cpda        £dstpda
229600080208     C                   MOVEL     v1ctstpda     §dsttstpda
229700070309     C                   MOVEL     FVVspn        dfvvspn
229800070309     C                   MOVEL     §fvvtpm       £DSTTPM
229900160429     C                   MOVEL     §fvvturno     §DSTturno
230000160429     C                   MOVEL     ddstflr       £dstflr
230100070410     c                   if        %found(fidst09l)
230200080121     c* allineo i giri consegne
230300070312     C     KDST1         setll     FiDST31l
230400070312     c                   do        *hival
230500070312     C     KDST1         reade     FiDST31l
230600070312     c                   if        %eof(fidst31l)
230700070312     c                   leave
230800070312     c                   end
230900070316     c* annullamento
231000120718     c                   if        £dstatb <> ' '
231100070312     C                   eval      dst3ATB = 'A'
231200070312     c                   else
231300070316     c* ripristino
231400070312     C                   CLEAR                   DST3ATB
231500070312     c                   end
231600070312     C                   UPDATE    FiDST300
231700070312     c                   enddo
231800080121     c* allineo i giri ORM
231900080121     c                   if        §OGDDAO <> ' '
232000080121     C     KDST1         setll     FiDST51l
232100080121     c                   do        *hival
232200080121     C     KDST1         reade     FiDST51l
232300080121     c                   if        %eof(fidst51l)
232400080121     c                   leave
232500080121     c                   end
232600080121     c* annullamento
232700120718     c                   if        £dstatb <> ' '
232800080121     C                   eval      dst5ATB = 'A'
232900080121     c                   else
233000080121     c* ripristino
233100080121     C                   CLEAR                   DST5ATB
233200080121     c                   end
233300080121     C                   UPDATE    FiDST500
233400080121     c                   enddo
233500080121     c                   end
233600070517     C                   update    Fidst000
233700070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f
233800070517     c                   exsr      srfidg37
233900070309     c                   else
234000070316     c* INSERIMENTO
234100070316     c*
234200070316     c* aggancio estensione anagrafico aut. per giri
234300070316     c     kapd21        setgt     fiapd22l
234400070316     c     kapd2         readpe    fiapd22l
234500070316     c                   if        not %eof(fiapd22l) and apd2atb = ' ' and
234600070316     c                             fvvdfv>=apd2dde and fvvdfv<=apd2dscad
234700070316     c                   else
234800070316     c* se aut. non gestisce giri pulisco gli  archivi
234900070316     c                   clear                   fiapd2ds
235000070316    1C                   end
235100070309     c                   movea     tc1           tc1d
235200070309     c                   movea     ts1           ts1d
235300070309     c                   if        apd2ftc = ' '
235400070312     c                   eval      £dstftc = 'E'
235500070309     c                   else
235600070312     c                   eval      £dstftc = apd2ftc
235700070309     c                   end
235800070309     c                   if        apd2fts = ' '
235900070312     c                   eval      £dstfts = 'E'
236000070309     c                   else
236100070312     c                   eval      £dstfts = apd2fts
236200070309     c                   end
236300070312     c                   if        apd2vlu = 0
236400070312     c                   eval      £dstvlu = apdvlu
236500070312     c                   else
236600070312     c                   eval      £dstvlu = apd2vlu
236700070312     c                   end
236800070312     c                   if        apd2pkl = 0
236900070312     c                   eval      £dstpkl = apdpkl
237000070312     c                   else
237100070312     c                   eval      £dstpkl = apd2pkl
237200070312     c                   end
237300070312     c                   eval      £dstnbl = apd2nbl
237400070312     c                   eval      £dstnftl= apd2nftl
237500070312     c                   eval      £dstpsf = apd2psf
237600070312     c                   eval      £dsttol = apd2tol
237700070312     c                   if        apd2ftc = ' '
237800070312     c                   eval      £dstinc = 'S'
237900070309     c                   else
238000070312     c                   eval      £dstinc = apd2inc
238100070309     c                   end
238200080121     c                   if        §OGDDAO <> ' '
238300080121     c                   eval      £dstrvlu = apd2rvlu
238400080121     c                   eval      £dstrpkl = apd2rpkl
238500080121     c                   eval      £dstrnbl = apd2rnbl
238600080121     c                   eval      £dstrnftl= apd2rnftl
238700080121     c                   eval      £dstrtol = apd2rtol
238800080121     c                   if        apd2rinc = ' '
238900080121     c                   eval      £dstrinc = 'S'
239000080121     c                   else
239100080121     c                   eval      £dstrinc = apd2rinc
239200080121     c                   end
239300080121     c                   if        apd2rspi = ' '
239400080121     C*???
239500080121     c                   eval      £dstrspi = 'N'
239600080121     c                   else
239700080121     c                   eval      £dstrspi = apd2rspi
239800080121     c                   end
239900080121     c                   else
240000080121     c                   clear                   £dstrvlu
240100080121     c                   clear                   £dstrpkl
240200080121     c                   clear                   £dstrnbl
240300080121     c                   clear                   £dstrnftl
240400080121     c                   clear                   £dstrtol
240500080121     c                   clear                   £dstrinc
240600080121     c                   clear                   £dstrspi
240700080121     c                   end
240800080121     c                   eval      £DSTrPKLA=0
240900080121     c                   eval      £DSTrVLUA=0
241000080121     c                   eval      £DSTrSTPA=0
241100080121     c                   eval      £DSTrnblA=0
241200080121     c                   eval      £DSTrnbaA=0
241300070309     c* scrivo i giri
241400070309     c                   clear                   fidst300
241500070312     c                   eval      dst3fgs = fvvfgs
241600070312     c                   eval      dst3npg = fvvnpg
241700070312     c                   eval      dst3dfv = fvvdfv
241800070312     c                   eval      dst3nfv = fvvnfv
241900070309     c     apd2nras      setll     fiapd31l
242000070309     c                   do        *hival
242100070309     c     apd2nras      reade     fiapd31l
242200070309     c                   if        %eof(fiapd31l)
242300070309     c                   leave
242400070309     c                   end
242500070309     c                   eval      dst3fgsg= apd3fgsg
242600070309     c                   eval      dst3cgi = apd3cgi
242700070309     c                   eval      dst3prgg= apd3prgg
242800070309     c                   eval      dst3sql = apd3sql
242900070725     c                   clear                   fidg09ds
243000070725     c                   eval      d09ifgs = apd3fgsg
243100070725     c                   eval      d09icgi = apd3cgi
243200070725     c                   eval      D09ITUG = 'C'
243300070725     c                   exsr      srcgi
243400070725     c                   if        d09oerr <>'1'
243500070725     c                   write     fidst300
243600070725     c                   end
243700070309     c                   enddo
243800080121     c* scrivo i giri ORM
243900080121     c                   if        §OGDDAO <> ' '
244000080121     c                   clear                   fidst500
244100080121     c                   eval      dst5fgs = fvvfgs
244200080121     c                   eval      dst5npg = fvvnpg
244300080121     c                   eval      dst5dfv = fvvdfv
244400080121     c                   eval      dst5nfv = fvvnfv
244500080121     c     apd2nras      setll     fiapd51l
244600080121     c                   do        *hival
244700080121     c     apd2nras      reade     fiapd51l
244800080121     c                   if        %eof(fiapd51l)
244900080121     c                   leave
245000080121     c                   end
245100080121     c                   if        apd5atb <> ' '
245200080121     c                   iter
245300080121     c                   end
245400080121     c                   eval      dst5fgsg= apd5fgsg
245500080121     c                   eval      dst5cgi = apd5cgi
245600080121     c                   eval      dst5prgg= apd5prgg
245700080121     c                   eval      dst5sql = apd5sql
245800080121     c                   clear                   fidg09ds
245900080121     c                   eval      d09ifgs = apd5fgsg
246000080121     c                   eval      d09icgi = apd5cgi
246100080121     c                   eval      D09ITUG = 'R'
246200080121     c                   exsr      srcgi
246300080121     c                   if        d09oerr <>'1'
246400080121     c                   write     fidst500
246500080121     c                   end
246600080121     c                   enddo
246700080121     c                   end
246800070517     C                   WRITE     Fidst000
246900070517     c* aggiorna pesi/volumi/fermate assegnate x fidst00f (deve prima essere
247000070517     c* scritto il record su fidst)
247100070517     c                   exsr      srfidg37
247200070309     c                   end
247300090310     c*                  end
247400080506     c* chiamo il pgm per aggiornamento dati pda per chiudere in lr
247500080506     c                   if        §OGPDAcon <> ' ' and
247600080506     c                             (v1cpda =  'C' or v1cpda = 'E' or
247700080506     c                             v1ctstpda = 'C' or
247800080506     c                             v1ctstpda = 'E')
247900080506     c                   eval      co42tla = 'C'
248000080506     c                   exsr      srpda
248100080506     c                   end
248200070309     c*
248300070309     C                   ENDSR
248400070725     C**************************************************************************
248500070725     C* controllo e decodifico giro
248600070725     C**************************************************************************
248700070725     C     Srcgi         BEGSR
248800070725     C*
248900070725     c                   eval      D09IOP0 = '001'
249000070725     c                   move      fvvdfv        d09idat
249100070725     c                   eval      kpjbu = fidg09ds
249200070725     c                   call      'FIDG09R'
249300070725     c                   parm                    kpjba
249400070725     c                   eval      fidg09ds = kpjbu
249500070725     C*
249600070725     C                   ENDSR
249700070309     C*-----------------------------------------------------*
249800070517     C* aggiorna pesi/volumi/fermate assegnate x fidst00f   *
249900070309     C*-----------------------------------------------------*
250000070517     C     srfidg37      BEGSR
250100070328     c*
250200070517     c                   clear                   fidg37ds
250300070517     c                   eval      D37FGSI = fvvfgs
250400070517     c                   eval      D37DFVI = fvvdfv
250500070517     c                   eval      D37NFVI = fvvnfv
250600070517     c                   eval      D37cmtI = 'S'
250700070517     c                   eval      kpjbu = fidg37ds
250800070517     c                   call      'FIDG37C'
250900070517     c                   parm                    kpjba
251000070517     c                   eval      fidg37ds = kpjbu
251100070328     c*
251200070328     C                   ENDSR
251300070328     C*-----------------------------------------------------*
251400070328     C* Controllo che le spedizioni non siano già chiuse    *
251500070328     C*-----------------------------------------------------*
251600070328     C     Ctrarb        BEGSR
251700911018     C*
251800070309     C                   Z-ADD     ROVDDC        KDDC
251900070309     C                   Z-ADD     V1cLNA        KFGS
252000070309     C                   Z-ADD     V01NDC        KNDC
252100070309     C     KARB          SETLL     FNARB70L
252200070309     C                   DO        *HIVAL
252300160720     C     KARB          READE     FNARB70L
252400070309     c                   if        %eof(fnarb70l)
252500070309     c                   leave
252600070309     c                   end
252700120518     c                   if        arbfdc = 'S'
252800070309     c                   seton                                        lr
252900070309     c                   return
253000070309     c                   end
253100070309     c                   enddo
253200070309     C*
253300070309     C                   ENDSR
253400070309     C*-----------------------------------------------------*
253500070309     C* CARICAMENTO  SUBFILE  DAL FILE DI LAVORO            *
253600070309     C*-----------------------------------------------------*
253700070309     C     CARSFL        BEGSR
253800070309     C*
253900940323     C                   Z-ADD     0             XX
254000940323     C                   Z-ADD     0             STP
254100911018     C                   Z-ADD     0             V06STP
254200911018     C                   Z-ADD     *ZEROS        V06PKG
254300911018     C                   Z-ADD     *ZEROS        V06VOL
254400911018     C                   Z-ADD     0             V06TSP
254500911018     C                   Z-ADD     *ZEROS        V06TIN
254600990920     C                   Z-ADD     *ZEROS        V06TEN
254700941108     C                   Z-ADD     *ZEROS        V4CTIV
254800941108     C                   Z-ADD     *ZEROS        V3CSTP
254900990920     C* Abblenco divise
255000011009     C                   MOVEL     §GEDCR        V06DVI
255100990920     C                   MOVEL     DIVEUR        V06DVE
255200941108     C     *LOVAL        SETLL     FNARBW1L
255300911018     C*
255400911018     C                   DO        *HIVAL
255500941108     C                   SETOFF                                       55
255600941108     C                   Z-ADD     *ZEROS        COMASS
255700941108     C                   MOVEL     *BLANKS       COMVCA
255800941109     C                   MOVEL     *BLANKS       AR9TIC
255900941108     C                   READ      FNARBW1L                               55
256000911023     C  N55              DO
256100941108     C                   MOVE      ARBNGD        V3CNGD
256200911022     C     ARBSTP        IFNE      *ZEROS
256300940323     C     ARBSTP        LOOKUP    STP                                    13
256400940323     C     *IN13         IFEQ      '0'
256500940323     C                   ADD       1             V06STP
256600961217     C     XX            IFGE      999
256700961217     C                   SETON                                        79
256800961217     C                   MOVEL     ERR(20)       $ERR
256900961217     C                   END
257000961217     C     XX            IFLT      999
257100940323     C                   ADD       1             XX                3 0
257200940323     C                   Z-ADD     ARBSTP        STP(XX)
257300961128     C                   END
257400911018     C                   END
257500940323     C                   END
257600911022     C     ARBSTP        IFEQ      *ZEROS
257700911022     C                   ADD       1             V06STP
257800911022     C                   END
257900941108     C                   MOVE      ARBSTP        V3CSTP
258000941108     C                   MOVE      ARBSTP        V3CPST
258100911105     C*
258200911105     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
258300941108     C     KARB1         CHAIN     FNLBL01L                           54
258400911113     C     *IN54         IFEQ      '0'
258500950126     C     ARBLNA        ANDEQ     LBLLAP
258600941108     C     KLBL          CHAIN(N)  FNARB01L                           56
258700980429     C*
258800980429     C     *IN56         IFEQ      '0'
258900980429     C     ARBCCA        ANDEQ     '2'
259000980429     C     *IN56         OREQ      '0'
259100980429     C     ARBCCA        ANDEQ     '6'
259200980429     C                   SETON                                        5456
259300980430     C     KARB2         CHAIN     FNARB01L                           30
259400980429     C                   END
259500980429     C*
259600941108     C  N56ARBICC        IFEQ      ' '
259700941108     C     ARBICC        OREQ      'R'
259800941108     C*  Controllo il tipo bolla mamma
259900941108     C                   Z-ADD     1             X                 2 0
260000941108     C                   MOVEL     *BLANKS       V3CCBO
260100941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
260200941108     C     *IN13         IFEQ      '1'
260300941109     C                   MOVEL     D3A(X)        DS3A
260400941108     C*  Se bolla mamma con C/Assegno vado a reperirlo
260500941108     C     §3AFCA        IFNE      0
260600051115     C     KLBL          CHAIN     FIAR901L                           56
260700941108     C     *IN56         IFEQ      '0'
260800941108     C                   Z-ADD     AR9CAS        COMASS           13 3
260900941108     C                   MOVEL     AR9VCA        COMVCA            3
261000941109     C                   MOVEL     AR9TIC        COMTIC            2
261100941108     C                   END
261200941108     C                   END
261300941108     C                   END
261400941108     C                   END
261500941108     C*
261600941108     C     KLBL1         CHAIN(N)  FNARB01L                           56
261700941108     C                   ELSE
261800941108     C     KARB1         CHAIN(N)  FNARB01L                           56
261900941108     C                   END
262000941108     C*
262100941108     C*  Controllo il tipo bolla
262200941108     C                   Z-ADD     1             X                 2 0
262300941108     C                   MOVEL     *BLANKS       V3CCBO
262400951121     C                   MOVEL     *BLANKS       WTBOL1            1
262500941108     C                   CLEAR                   DS3A
262600941108     C     ARBCBO        LOOKUP    C3A(X)                                 13
262700941108     C     *IN13         IFEQ      '1'
262800941109     C                   MOVEL     D3A(X)        DS3A
262900941108     C                   MOVEL     §3ATB1        WTBOL1
263000990924     C                   MOVEL     §3ATB2        WTBOL2
263100941108     C                   END
263200951207     C                   MOVE      ARBKSC        V3CCLI
263300951121     C                   MOVEL     ARBRSD        V3CRAG
263400951121     C                   MOVE      ARBIND        V3CRAG
263500911018     C*
263600911105     C  N56              DO
263700911106     C     ARBFAN        IFEQ      'S'
263800941108     C                   MOVEL     'ANOM.'       V3DANO
263900911106     C                   ELSE
264000941108     C                   MOVEL     *BLANKS       V3DANO
264100911106     C                   END
264200941108     C                   MOVEL     ARBCBO        V3CCBO
264300941108     C                   MOVEL     §3ATB1        V3CTB1
264400941108     C                   MOVEL     *BLANK        V3CSEL
264500941108     C                   MOVEL     ARBRSD        V3CRSD
264600941108     C                   MOVEL     ARBIND        V3CIND
264700941108     C                   MOVEL     ARBLOD        V3CLOD
264800941108     C                   MOVEL     ARBPRD        V3CPRD
264900941108     C                   MOVEL     ARBCAD        V3CCAD
265000110804     C                   MOVEL     ARBxco        V3Cxco
265100941108     C                   Z-ADD     ARBAAS        V3CAAS
265200941108     C                   Z-ADD     ARBLNP        V3CLNP
265300941108     C                   Z-ADD     ARBNRS        V3CNRS
265400941108     C                   Z-ADD     ARBNSP        V3CNSP
265500941108     C                   Z-ADD     ARBLNA        V3CLNA
265600941108     C                   Z-ADD     ARBZNC        V3CZNC
265700941108     C                   Z-ADD     ARBNCL        V3CNCL
265800070612     c* peso in base alle regole VDL
265900070612     c                   exsr      calpes
266000070612     C                   Z-ADD     wpes          V3CPKF
266100070612     c* vol. in base alle regole VDL
266200070612     c                   exsr      calvol
266300070612     C                   Z-ADD     wvol          V3CVLF
266400911018     C                   MOVE      ARBDCR        MMGG              4 0
266500911018     C                   MOVEL     MMGG          MM                2 0
266600911018     C                   MOVE      MMGG          GG                2 0
266700941108     C                   MOVE      MM            V3CGMC
266800941108     C                   MOVEL     GG            V3CGMC
266900941108     C                   MOVE      ARBHCR        V3CHCR
267000941108     C                   MOVE      ARBGC1        V3CGC1
267100941108     C                   MOVE      ARBGC2        V3CGC2
267200941108     C                   MOVE      ARBCBO        V3CCBO
267300941108     C                   MOVE      ARBTSP        V3CTSP
267400941108     C                   Z-ADD     *ZEROS        V3CIMA
267500941109     C                   MOVEL     *BLANKS       V3CVCA
267600990920     C                   MOVEL     *BLANKS       V3CVAS
267700941109     C                   MOVEL     *BLANKS       V3CTIC
267800941109     C                   Z-ADD     *ZEROS        V3CCAS
267900941108     C*
268000941108     C* Decodifico i codici consegna particolare
268100941108     C                   MOVEL     *BLANKS       V3DTC1
268200941108     C                   MOVEL     *BLANKS       V3DTC2
268300941108     C                   Z-ADD     1             XY                3 0
268400941108     C     ARBTC1        LOOKUP    C1P(XY)                                23
268500941108     C     *IN23         IFEQ      '1'
268600941108     C                   MOVEL     D1P(XY)       V3DTC1
268700941108     C                   END
268800941108     C                   Z-ADD     1             XY                3 0
268900941108     C     ARBTC2        LOOKUP    C1P(XY)                                23
269000941108     C     *IN23         IFEQ      '1'
269100941108     C                   MOVEL     D1P(XY)       V3DTC2
269200941108     C                   END
269300990920     C*
269400990920     C* Verifico se devo gestire un assegnato
269500990920     C     WTBOL1        IFNE      *BLANKS
269600990920     C     WTBOL2        ORNE      *BLANKS
269700990920     C                   Z-ADD     ARBAAS        KAAS
269800990920     C                   Z-ADD     ARBLNP        KLNP
269900990920     C                   Z-ADD     ARBNRS        KNRS
270000990920     C                   Z-ADD     ARBNSP        KNSP
270100990920     C* Se devo gestire 2° bolla imposto tipo record '2' x posiz.FIAR6
270200990920     C     WTBOL2        IFNE      *BLANKS
270300990920     C                   MOVE      '2'           KTRC
270400990920     C* .... se devo gestire 1° bolla imposto tipo record '1'
270500990920     C                   ELSE
270600990920     C     WTBOL1        IFNE      'F'
270700990920     C                   MOVE      '1'           KTRC
270800990920     C                   END
270900990920     C                   END
271000990920     C* Aggancio FIAR6 x importo assegnato
271100990920     C     KAR6          CHAIN     FIAR601L                           74
271200990920     C  N74AR6DFT        IFNE      *ZEROS
271300990920     C* Se ho effettuato un ritorno all'incasso controllo se
271400990920     C     ARBICA        IFEQ      'R'
271500990920     C* ... si riferisce ad un incasso parziale
271600990920     C                   CLEAR                   ARIIPP
271700990920     C                   MOVEL     'A'           KTIP
271800990920     C     KARI          CHAIN     FIARI01L                           74
271900990921     C                   MOVEL     AR6DIV        V3CVAS
272000990921     C                   Z-ADD     AR6IFT        V3CIMA
272100990920     C* ... se si calcolo importo da incassare x differenza
272200990921     C     *IN74         DOWEQ     '0'
272300990921     C     AR6DIV        IFEQ      ARIDIV
272400990921     C                   SUB       ARIIPP        V3CIMA
272500990921     C                   ELSE
272600990921     C* Se la divisa di incasso è diversa converto importo già
272700990921     C* incassato nella divisa della bolla
272800990921     C                   CLEAR                   YEURDS
272900990921     C                   MOVEL     ARIDIV        YECDVI
273000990921     C                   Z-ADD     ARIIPP        YECIMI
273100990927     C                   MOVEL     AR6DIV        YECDVO
273200990921     C                   MOVEL     'H'           YECTAR
273300990921     C                   CALL      'YEURCO'
273400990921     C                   PARM                    YEURDS
273500990921     C                   SUB       YECIMO        V3CIMA
273600990921     C                   END
273700990921     C     KARI          READE     FIARI01L                               74
273800990921     C                   END
273900990920     C                   ELSE
274000990920     C* ... altrimenti imposto totale fattura
274100991202     C     ARBICA        IFEQ      ' '
274200991202     C     ARBICA        OREQ      'R'
274300990920     C                   Z-ADD     AR6IFT        V3CIMA
274400990920     C                   MOVEL     AR6DIV        V3CVAS
274500940323     C                   END
274600991202     C                   END
274700941108     C  N74WTBOL1        IFEQ      'F'
274800990920     C                   MOVE      AR6KSC        V3CCLI
274900941108     C                   MOVEL     'A'           V3CTB1
275000911206     C                   END
275100940323     C                   END
275200941108     C*
275300940323     C                   END
275400941108     C                   MOVEL     ARBRSM        V3CRSM
275500911118     C     ARBICC        IFEQ      ' '
275600941108     C     ARBICC        OREQ      'R'
275700941109     C     §3AFCA        IFNE      0
275800941108     C                   Z-ADD     0             AR9CAS
275900941108     C                   MOVEL     *BLANKS       AR9VCA
276000051115     C     KARB1         CHAIN     FIAR901L                           23
276100941109     C     *IN23         IFEQ      '0'
276200941108     C                   Z-ADD     AR9CAS        V3CCAS
276300941108     C                   MOVEL     AR9VCA        V3CVCA
276400941109     C                   MOVE      AR9TIC        V3CTIC
276500941108     C                   END
276600941109     C                   END
276700941108     C     COMASS        IFNE      *ZEROS
276800941108     C                   Z-ADD     COMASS        V3CCAS
276900941109     C                   MOVEL     COMVCA        V3CVCA
277000941109     C                   MOVE      COMTIC        V3CTIC
277100911105     C                   END
277200911113     C                   END
277300911112     C* SE SPED.DI SELEZIONE PRECEDENTE ILLUMINO IL NUMERO SPEDIZIONE
277400911112     C     ARBFLG        IFEQ      '1'
277500911112     C                   SETON                                        35
277600911112     C                   ELSE
277700911112     C                   SETOFF                                       35
277800911112     C                   END
277900911018     C*
278000000515     C*  Controllo se LE SPEDIZIONI SONO AUTOGENERATE
278100000515     C                   SETOFF                                       63
278200000515     C                   MOVEL     'A'           KTRC
278300060214     C     KAR6          CHAIN     FiAR401L                           95
278400000515     C  N95              MOVEL     AR4NOT        DSBL4A
278500000515     C  N95§B4ASO        COMP      '&'                                    63
278600000515     C*
278700911018     C                   ADD       1             NRR
278800911018     C*
278900991006     C* Accendo indicatori x C/Assegno e assegnato
279000160502     C*    V3CCAS        COMP      0                                      59
279100991006     C     V3CIMA        COMP      0                                      41
279200130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
279300130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
279400130221     C*****V3Cxco        COMP      ' '                                8888
279500130221     C     V3Cxco        COMP      '0'                                88
279600941109     C                   WRITE     LR50S03
279700941108     C                   MOVEL     *BLANKS       V3DANO
279800911018     C*
279900911018     C* CALCOLO I TOTALE DA VIDEATA 6
280000911018     C*
280100070612     C                   ADD       wpes          V06PKG
280200070612     C                   ADD       wvol          V06VOL
280300911018     C                   ADD       1             V06TSP
280400990920     C* Sommo totale da incassare in moneta di conto o altra divisa
280500990920     C* permessa
280600990920     C                   SELECT
280700990920     C     V3CVAS        WHENEQ    *BLANKS
280800011009     C     V3CVAS        OREQ      §GEDCR
280900990920     C                   ADD       V3CIMA        V06TIN
281000990920     C* ... altra divisa valida
281100990920     C     V3CVAS        WHENEQ    DIVEUR
281200990920     C                   ADD       V3CIMA        V06TEN
281300990920     C                   ENDSL
281400990920     C* Somma nel totale corretto: moneta di conto
281500990920     C                   SELECT
281600990920     C     V3CVCA        WHENEQ    *BLANKS
281700011009     C     V3CVCA        OREQ      §GEDCR
281800941108     C                   ADD       V3CCAS        V06TIN
281900990920     C* ... altra divisa valida
282000990920     C     V3CVCA        WHENEQ    DIVEUR
282100990920     C                   ADD       V3CCAS        V06TEN
282200990920     C* ... divise non Unione Europea
282300990920     C                   OTHER
282400941108     C                   ADD       V3CCAS        V4CTIV
282500990920     C                   ENDSL
282600911018     C                   END
282700911105     C*
282800911105     C                   END
282900911018     C*
283000911021     C  N55              END
283100911018     C*
283200911018     C                   ENDSR
283300911018     C*-----------------------------------------------------*
283400911018     C* GESTIONE FORMATO 6 TOTALI E CONFERMA                *
283500911018     C*-----------------------------------------------------*
283600911018     C     GESFM6        BEGSR
283700911018     C*
283800930428     C                   SETOFF                                       0706
283900050204     C                   MOVEL     'N'           V06ban
284000050207     C                   MOVEL     'N'           V06SIN
284100911021     C                   DO        *HIVAL                                       1
284200911018     C*
284300991117     C     V06TIN        COMP      0                                  0808
284400991117     C     V06TEN        COMP      0                                  0909
284500991117     C     V4CTIV        COMP      0                                  5353
284600941108     C                   EXFMT     LR50D04
284700070315     c* prima di aggiornare una distinta controllo che non
284800070315     c* sia stata sbloccata da altri e quindi già chiusa per evitare di
284900070315     c* perdere i riferimenti della chiusura distinta. Nel caso forzo
285000070315     c* l'uscita
285100070315     c                   if        v01ndc <> 0
285200070315     c                   exsr      ctrarb
285300070315     c                   end
285400911018     C*
285500911022     C  N80              DO                                                     2
285600911018     C*
285700941108     C   KF              SETON                                        0706
285800911018     C*
285900941108     C  NKF*IN07         IFEQ      '1'                                          3
286000950907     C   02              MOVEL     '1'           *INKC
286100911018     C* CONTROLLO SE ERO IN MODIFICA OPPURE IN CREAZIONE NUOVA DISTINTA
286200941107     C     V01NDC        IFEQ      *ZEROS                                       4
286300020905     c* prende il numero della distinta del po in gestione
286400020905     c                   do        *hival
286500020905     C                   CLEAR                   TRUL33DS
286600020905     c                   eval      i33tla = 'L'
286700020905     C                   Z-ADD     0             I33AAA
286800020905     C                   Z-ADD     12            I33CNU
286900020905     C                   Z-ADD     V1CLNA        I33PO1
287000020905     C                   Z-ADD     1             I33NUM
287100020905     c                   movel(p)  trul33ds      kpjbu
287200020905     c                   call      'TRUL33R'
287300020905     c                   parm                    kpjba
287400020905     c                   movel     kpjbu         trul33ds
287500020905     C                   IF        O33ERR = 0
287600020905     c* controllo che il numero non è già stato usato
287700020905     c                   clear                   fnlr6ds
287800020905     c                   eval      d6tla = 'L'
287900020905     C                   Z-ADD     8             D6npg
288000020905     C                   Z-ADD     V1CLNA        D6fgs
288100020905     C                   Z-ADD     O33Nrf        D6NDT
288200020905     c                   movel(p)  fnlr6ds       kpjbu
288300020905     c                   call      'FNLR6R'
288400020905     c                   parm                    kpjba
288500020905     c                   movel     kpjbu         fnlr6ds
288600020905     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
288700020905     c* come foglio o in una bolla
288800020905     c                   if        d6flg <> 'N'
288900020905     c                   iter
289000020905     c                   else
289100020905     C                   Z-ADD     O33Nrf        V01NDC
289200020905     c                   leave
289300020905     C                   END
289400020905     C                   END
289500020905     C                   ENDdo
289600020905     C                   MOVEL     V01NDC        V1CNDC
289700941107     C                   Z-ADD     V01NDC        FVVNFV            5 0
289800911018     C*
289900911018     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
290000911021     C*
290100950601     C                   MOVEL     *BLANKS       FVVATB
290200911021     C*
290300941109     C                   MOVEL     V1CLNA        FVVFGS
290400020916     C                   MOVEL     simfel        FVVFLE
290500911021     C*
290600941109     C                   Z-ADD     ROVDDC        FVVDFV
290700941109     C                   MOVEL     V06PAD        FVVDSF
290800941109     C                   MOVEL     *BLANK        FVVFCF
290900941109     C                   Z-ADD     4             FVVNPG
291000941109     C                   MOVE      *BLANK        FVVSPG
291100941109     C                   MOVE      *BLANK        FVVSPN
291200911021     C                   MOVEL     'S'           FVVFSC
291300941109     C                   MOVEL     V1CFPP        FVVFPP
291400950601     C                   MOVE      V1CPEP        FVVPEP
291500050523     c                   clear                   dfvvspn
291600050523     C                   MOVEl     V1Ctpm        §fvvtpm
291700050523     c                   if        v1ctpm = 'P'
291800050523     C                   MOVEl     *blanks       §fvvtpm
291900050523     c                   end
292000160429     C                   MOVEl     V1Ctur        §fvvturno
292100160429     c                   if        v1ctur = '1'
292200160429     C                   MOVEl     *blanks       §fvvturno
292300160429     c                   end
292400050523     c                   movel     dfvvspn       fvvspn
292500000515     C                   WRITE     FNFVV000
292600000515      *SCRIVO FOGLIO TIPO 8
292700000515     C                   Z-ADD     8             FVVNPG
292800000515     C                   WRITE     FNFVV000
292900000515     C                   Z-ADD     4             FVVNPG
293000041007     C* gestione file dei rientri
293100041007     c                   move      v1clna        wlna
293200041007     c                   z-add     v01ndc        wdis
293300041007     C     Kqdt          chain     fiqdt01l                           83
293400041007     c   83              clear                   fiqdt000
293500041007     C                   movel     v1clna        qdtfgs
293600041007     C                   z-add     v01ndc        qdtnfv
293700041007     C                   z-add     codpad        qdtpdr
293800041007     C                   z-add     fvvdfv        qdtddc
293900110311     C   83              z-add     fvvdfv        qdtdaiaf
294000110311     C   83              movel     whhdat        qdtoraaf
294100110311     C   83              move      *zeros        qdtnbnaf
294200110311     C   83              move      apdptp        qdtnbnaf
294300110311     C   83              move      'S'           qdtfnbaf
294400041007     c   83              write     fiqdt000
294500041007     c  n83              update    fiqdt000
294600950601     C*
294700911021     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
294800911021     C* GIRO,STOP E PROGRESSIVO DI STAMPA
294900911021     C*
295000941108     C     *LOVAL        SETLL     FNARBW1L
295100911021     C*
295200030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
295300911021     C                   DO        *HIVAL                                       4
295400911108     C                   SETOFF                                           55
295500941108     C                   READ      FNARBW1L                               55
295600911021     C  N55              DO                                                     5
295700911021     C                   Z-ADD     ARBSTP        WSTP              3 0
295800911021     C                   Z-ADD     ARBNGD        WNGD              3 0
295900080505     c                   clear                   fidg42ds
296000911021     C*
296100941108     C     KARB1         CHAIN     FNARB01L                           56
296200930428     C  N56              DO
296300110127     c* controllo che la spedizione non sia già stata inserita in altra
296400110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
296500110202     c* la vecchia distinta
296600110127     c                   if        arbndc <> 0 and arbndc <> v01ndc
296700110202     c                             and arbicc <> 'R' and arbica <> 'R'
296800110128     c                   seton                                        90
296900110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
297000110202     c                             iornate correttamente. VERIFICARE!!!'
297100110127     c                   iter
297200110127     c                   end
297300980306     C* Controllo se devo scrivere MIC (evento messa in consegna)
297400980310     C     ARBDCM        IFEQ      0
297500980306     C                   EXSR      WRTMIC
297600980310     C                   END
297700980306     C*
297800020905     C                   Z-ADD     V1CLNA        ARBIFP
297900020905     C                   Z-ADD     V01NDC        ARBNDC
298000911021     C                   Z-ADD     ROVDDC        ARBDDC
298100930428     C                   Z-ADD     0             ARBNMI
298200930428     C                   MOVEL     *BLANKS       ARBFDC
298300030722     C                   Z-ADD     0             ARBNSS
298400911021     C                   Z-ADD     WSTP          ARBSTP
298500911021     C                   Z-ADD     WNGD          ARBNGD
298600080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
298700080505     c                   eval      co42tla = 'A'
298800080505     c                   else
298900080505     c                   clear                   co42tla
299000080505     c                   end
299100080505     C                   Z-ADD     CODPAD        ARBPDC
299200911021     C*
299300941109     C                   UPDATE    FNARB000
299400941109     C                   DELETE    FNARBW00
299500030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
299600030910     c* deve andare a legare l'orm alla distinta
299700070227     c*                  if        spunta <> *blank
299800030910     c                   exsr      srorm
299900070227     c*                  endif
300000070319     c* mettro i riferimenti della distinta anche su FIARG x evitare di
300100070319     c* elaborare la spedizione nelle distinte automatiche
300200070319     c                   exsr      srfiarg
300300080410     c* richiamo il pgm per aggiornare i dati per PDA
300400080410     c                   if        §OGPDAcon <> ' ' and
300500080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
300600080411     c                             v1ctstpda = 'C' or
300700080411     c                             v1ctstpda = 'E')
300800080505     c                   if        co42tla = 'A'
300900080505     c                   exsr      srpda
301000080505     c                   end
301100080410     c                   eval      co42tla = 'I'
301200080410     c                   exsr      srpda
301300080410     c                   end
301400911113     C                   END
301500911021     C*
301600930428     C                   END
301700930428     C  N55              END
301800911105     C                   ELSE
301900950601     C*
302000041007     C* gestione file dei rientri
302100041007     c                   move      v1clna        wlna
302200041007     c                   z-add     v01ndc        wdis
302300041007     C     Kqdt          chain     fiqdt01l                           83
302400041007     c   83              clear                   fiqdt000
302500041007     C                   movel     v1clna        qdtfgs
302600041007     C                   z-add     v01ndc        qdtnfv
302700041007     C                   z-add     codpad        qdtpdr
302800041007     C                   z-add     fvvdfv        qdtddc
302900041007     c   83              write     fiqdt000
303000041007     c  n83              update    fiqdt000
303100120718     C*  Aggiorno FiDST
303200120718     c                   exsr      srfidst
303300911128     C*
303400960702     C                   Z-ADD     ROVDDC        KDDC
303500020905     C                   Z-ADD     V1cLNA        KFGS
303600020905     C                   Z-ADD     V01NDC        KNDC
303700020905     C     KARB          SETLL     FNARB70L
303800911105     C*
303900930608     C                   DO        *HIVAL
304000941108     C                   SETOFF                                           50
304100911105     C*
304200020905     C     KARB          READE     FNARB70L                               50
304300911105     C*
304400930428     C  N50              DO
304500911105     C*
304600080318     C     KARB1         CHAIN(N)  FNARBW2L
304700080326     C                   if        not %found(fnarbw2l)
304800980309     C* Se era in consegna e adesso non c'è più annullo la messa
304900980309     C* consegna
305000980309     C     KARB1         CHAIN     FNARB01L                           31
305100110127     c*
305200980310     C     ARBDCM        IFEQ      0
305300980309     C                   EXSR      ANNMIC
305400980310     C                   END
305500950602     C                   Z-ADD     *ZEROS        ARBNSS
305600020905     C                   Z-ADD     *ZEROS        ARBIFP
305700020905     C                   Z-ADD     *ZEROS        ARBNDC
305800911111     C                   Z-ADD     *ZEROS        ARBDDC
305900911111     C                   Z-ADD     *ZEROS        ARBSTP
306000911111     C                   Z-ADD     *ZEROS        ARBNGD
306100941108     C                   Z-ADD     *ZEROS        ARBPDC
306200941109     C  N31              UPDATE    FNARB000
306300030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
306400030910     c* deve andare a togliere la distinta dall'orm
306500030910     c                   exsr      srorm
306600070319     c* tolgo i riferimenti della distinta anche su FIARG x
306700070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
306800070319     c                   exsr      srfiarg
306900080410     c* richiamo il pgm per aggiornare i dati per PDA
307000080410     c                   if        §OGPDAcon <> ' ' and
307100080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
307200080411     c                             v1ctstpda = 'C' or
307300080411     c                             v1ctstpda = 'E')
307400080410     c                   clear                   fidg42ds
307500080410     c                   eval      co42tla = 'A'
307600080410     c                   exsr      srpda
307700080410     c                   end
307800930428     C                   END
307900930428     C                   END
308000911111     C  N50              END
308100911111     C* PER CONFERMA VADO A VARIARE/IMPUTARE IL NUMERO E DATA DISTINTA,
308200911111     C* GIRO,STOP E PROGRESSIVO DI STAMPA
308300080505     c                   clear                   fidg42ds
308400911111     C*
308500941109     C     *LOVAL        SETLL     FNARBW1L
308600911111     C*
308700030722     C*                  Z-ADD     *ZEROS        PRGSTP            3 0
308800911111     C                   DO        *HIVAL                                       4
308900911111     C                   SETOFF                                           55
309000941109     C                   READ      FNARBW1L                               55
309100911111     C  N55              DO                                                     5
309200911111     C                   Z-ADD     ARBSTP        WSTP              3 0
309300911111     C                   Z-ADD     ARBNGD        WNGD              3 0
309400911111     C*
309500941109     C     KARB1         CHAIN     FNARB01L                           56
309600930428     C  N56              DO
309700110127     c* controllo che la spedizione non sia già stata inserita in altra
309800110202     c* distinta eccetto che per i ritorni all'incasso perchè mantengono
309900110202     c* la vecchia distinta
310000110202     c                   if        arbndc <> 0 and arbndc <> v01ndc
310100110202     c                             and arbicc <> 'R' and arbica <> 'R'
310200110128     c                   seton                                        90
310300110202     c                   eval      $msg = 'Alcune spedizioni non sono state agg-
310400110202     c                             iornate correttamente. VERIFICARE!!!'
310500110127     c                   iter
310600110127     c                   end
310700980309     C* Controllo se devo scrivere MIC (evento messa in consegna)
310800980310     C     ARBDCM        IFEQ      0
310900980309     C                   EXSR      WRTMIC
311000980310     C                   END
311100980309     C*
311200020905     C                   Z-ADD     V1CLNA        ARBIFP
311300020905     C                   Z-ADD     V01NDC        ARBNDC
311400911111     C                   Z-ADD     ROVDDC        ARBDDC
311500930428     C                   Z-ADD     0             ARBNMI
311600930428     C                   MOVEL     *BLANKS       ARBFDC
311700030722     C                   Z-ADD     0             ARBNSS
311800911111     C                   Z-ADD     WSTP          ARBSTP
311900911111     C                   Z-ADD     WNGD          ARBNGD
312000080506     c                   if        codpad <> arbpdc AND ARBPDC <> 0
312100080505     c                   eval      co42tla = 'A'
312200080505     c                   else
312300080505     c                   clear                   co42tla
312400080505     c                   end
312500080505     C                   Z-ADD     CODPAD        ARBPDC
312600911111     C*
312700941109     C                   UPDATE    FNARB000
312800941109     C                   DELETE    FNARBW00
312900030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
313000030910     c* deve andare a legare l'orm alla distinta
313100030910     c                   exsr      srorm
313200070319     c* metto i riferimenti della distinta anche su FIARG x evitare di
313300070319     c* elaborare la spedizione nelle distinte automatiche
313400070319     c                   exsr      srfiarg
313500080410     c* richiamo il pgm per aggiornare i dati per PDA
313600080410     c                   if        §OGPDAcon <> ' ' and
313700080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
313800080411     c                             v1ctstpda = 'C' or
313900080411     c                             v1ctstpda = 'E')
314000080505     c                   if        co42tla = 'A'
314100080505     c                   exsr      srpda
314200080505     c                   end
314300080410     c                   eval      co42tla = 'I'
314400080410     c                   exsr      srpda
314500080410     c                   end
314600911114     C                   END
314700911111     C*
314800930428     C                   END
314900930428     C  N55              END
315000140930     C* IN CASO DI VARIZIONE DISTINTA GIA' ESISTENTE
315100140930     C* E SE DA TOGLIERE DALLA DISTINTA
315200140930     C                   Z-ADD     V01NDC        FVVNFV            5 0
315300140930     C                   Z-ADD     V1CLNA        FVVFGS
315400140930     C*
315500140930     C* VALORIZZO I CAMPI PER LA SCRITTURA SU FLVV00
315600140930     C*
315700140930     C     KFVV          CHAIN     FNFVV01L                           83
315800140930     C  N83              DO
315900140930     C                   MOVEL     *BLANKS       FVVATB
316000140930     C*
316100140930     C                   MOVEL     V1CLNA        FVVFGS
316200140930     C                   MOVEL     simfel        FVVFLE
316300140930     C*
316400140930     C                   MOVEL     V06PAD        FVVDSF
316500140930     C                   MOVEL     *BLANK        FVVFCF
316600140930     C                   Z-ADD     4             FVVNPG
316700140930     C                   MOVEL     'S'           FVVFSC
316800140930     C                   MOVEL     V1CFPP        FVVFPP
316900140930     C                   MOVEL     V1CPEP        FVVPEP
317000140930     C                   MOVEL     fvvspn        dfvvspn
317100140930     C                   MOVEL     v1ctpm        §fvvtpm
317200140930     c                   if        v1ctpm = 'P'
317300140930     C                   MOVEL     *blanks       §fvvtpm
317400140930     c                   end
317500160429     C                   MOVEL     v1ctur        §fvvturno
317600160429     c                   if        v1ctur = '1'
317700160429     C                   MOVEL     *blanks       §fvvturno
317800160429     c                   end
317900140930     C                   MOVEL     dfvvspn       fvvspn
318000140930     C*
318100140930     C                   UPDATE    FNFVV000
318200140930      *GESTIONE FOGLIO TIPO 8
318300140930     C                   MOVE      FVVDS         FVVDS2
318400140930     C                   Z-ADD     8             FVVNPG
318500140930     C     KFVV          CHAIN     FNFVV000                           83
318600140930     C     *IN83         IFEQ      *OFF
318700140930     C                   MOVE      FVVDS2        FVVDS
318800140930     C                   Z-ADD     8             FVVNPG
318900140930     C                   UPDATE    FNFVV000
319000140930     C                   MOVE      FVVDS2        FVVDS
319100140930     C                   END
319200140930     C                   Z-ADD     4             FVVNPG
319300140930     C                   END
319400930428     C                   END
319500070312     c* aggiorno il file FIDST00F
319600070312     c                   exsr      srfidst
319700080320     c* se variato il cod. aut. richiamo pgm orm se pda partito
319800080320     c                   exsr      srchgcpc
319900070312     C*
320000911023     C   55              SETON                                        8081
320100920304     C   50              EXSR      PULRIC
320200911021     C*
320300041008      * richiesta gestione inserimento bancali
320400041008     c                   if        v06ban  = 'S'
320500041008     c                   clear                   fnlrf8ds
320600041008     c                   z-add     fvvnfv        rf8dis
320700041008     c                   z-add     fvvfgs        rf8fgs
320800041008     c                   movel(p)  fnlrf8ds      kpjbu
320900041008     c                   call      'FNLRF8R'
321000041008     c                   parm                    kpjba
321100041008     c                   endif
321200911105     C* SE CHIEDO STAMPA
321300010216     C     V06SIN        IFNE      'N'
321400941109     C                   CLEAR                   FNLR55
321500950102     C                   Z-ADD     ROVDDC        D55DAT
321600020905     C                   Z-ADD     ARBNDC        D55NRD
321700950102     C                   Z-ADD     ARBNDC        D55NRA
321800950102     C                   MOVEL     'A'           D55TIP
321900950102     C                   MOVEL     V1CLNA        D55FIL
322000041007     C*                  Z-ADD     DSTNBN        D55NBN
322100041007     C                   Z-ADD     qdtnbnaf      D55NBN
322200050207     C                   MOVEL     QDTFNBAF      D55fba
322300120718     C                   MOVEL     £DSTNOT       D55NOT
322400010216     C                   MOVEL     V06SIN        D55STP
322500941109     C                   MOVEL     FNLR55        KPJBU
322600050427     C                   CALL      'FNLR55C'
322700911128     C                   PARM                    KPJBA
322800911105     C                   END
322900941107     C                   MOVE      *BLANKS       V1CCPC
323000941109     C                   MOVE      WHHDAT        V1CDDC
323100930428     C                   END
323200911021     C                   END
323300911022     C  N80              END
323400950907     C*
323500950907     C     D50TPR        IFEQ      ' '
323600941107     C                   MOVEL     V01NDC        V1CNDP
323700941107     C                   MOVE      *BLANK        V1CNDC
323800080208     c                   clear                   msgv
323900080318     c                   clear                   msgv1
324000080415     c                   setoff                                       45
324100941110     C                   MOVE      *ZEROS        SAVNDC
324200920423     C                   MOVE      *BLANKS       SAVCPC
324300950608     C                   MOVE      WHHDAT        V1CDDC
324400081028     c* non deve pulire i campi della prima videata se premuto f12
324500081028     c                   if        not *inkl
324600080306     C                   MOVE      *BLANK        V1CPEP
324700080306     C                   MOVE      *blanks       V1Ctpm
324800160429     C                   MOVEl     *blanks       V1Ctur
324900080306     c                   clear                   v1cpda
325000080306     c                   clear                   v1ctstpda
325100080306     c                   end
325200950907     C                   END
325300911021     C*
325400911021     C                   ENDSR
325500980306     C*-----------------------------------------------------*
325600980306     C* WRTMIC: Ctrl se devo inviare "Messa in consegna"    *
325700980306     C*-----------------------------------------------------*
325800980306     C     WRTMIC        BEGSR
325900980306     C*
326000980311     C* Scrivo evento di messa in consegna solo se il numero
326100980311     C* distinta sulla bolla è diverso da quella attuale
326200980311     C* (Sono in fase di immissione speidizione in distinta)
326300131031      **
326400131104     c                   clear                   Devb01
326500131104     C                   move      *zeros        §NOTNDC
326600131104     C                   move      *zeros        §NOTPDC
326700131031     C                   move      V01NDC        §NOTNDC
326800131031     C                   move      CODPAD        §NOTPDC
326900131031      **
327000980311     C     V01NDC        IFNE      ARBNDC
327100980306     C* Controllo se ho già scritto evento MIC
327200980318     C     KARB1         CHAIN     FNLBL01L                           87
327300980318     C     *IN87         IFEQ      '0'
327400980318     C     ARBLNA        ANDEQ     LBLLAP
327500980429     C*
327600980429     C     KLBL          CHAIN     FNARB01L                           30
327700980429     C     *IN56         IFEQ      '0'
327800980429     C     ARBCCA        ANDEQ     '2'
327900980429     C     *IN56         OREQ      '0'
328000980429     C     ARBCCA        ANDEQ     '6'
328100980429     C                   SETON                                        87
328200980429     C                   END
328300980429     C     KARB2         CHAIN     FNARB01L                           30
328400980429     C                   END
328500980429     C*
328600980429     C     *IN87         IFEQ      '0'
328700980429     C     ARBLNA        ANDEQ     LBLLAP
328800980318     C                   Z-ADD     LBLAAP        KAAS
328900980318     C                   Z-ADD     LBLLPP        KLNP
329000980318     C                   Z-ADD     LBLNRP        KNRS
329100980318     C                   Z-ADD     LBLNSP        KNSP
329200980318     C                   ELSE
329300980306     C                   Z-ADD     ARBAAS        KAAS
329400980306     C                   Z-ADD     ARBLNP        KLNP
329500980306     C                   Z-ADD     ARBNRS        KNRS
329600980306     C                   Z-ADD     ARBNSP        KNSP
329700980318     C                   END
329800031017      * verifica se deve scrivere il mic se esiste almeno una spunta lo scrive
329900031017     c                   exsr      eventimic
330000031017     c     spunta        cabeq     *blank        nomic
330100031017
330200980306     C                   MOVEL     'MIC'         KCEV
330300990803     C     KEVB          SETGT     FNEVB04L
330400990803     C     KEVB          READPE    FNEVB04L                               33
330500980311     C* Se l'evento di messa in consegna non esiste o esista ma
330600980311     C* ha data diversa dalla distinta di consegna scrivo un
330700980311     C* nuovo evento di messa in consegna
330800980311     C     *IN33         IFEQ      '1'                                          No Trv Mic
330900990803     C     FVVDFV        ORNE      EVBDEV
331000990803     C                   CLEAR                   FNEVB000
331100980309     C                   MOVEL     'MIC'         EVBCEV
331200980309     C                   Z-ADD     DATEU         EVBDTV
331300980309     C                   TIME                    EVBORV
331400980309     C                   Z-ADD     V1CLNA        EVBFLE
331500020905     C                   Z-ADD     DUTCOU        EVBCDU
331600980318     C                   Z-ADD     KAAS          EVBAAS
331700980318     C                   Z-ADD     KLNP          EVBLNP
331800980318     C                   Z-ADD     KNRS          EVBNRS
331900980318     C                   Z-ADD     KNSP          EVBNSP
332000980309     C                   Z-ADD     FVVDFV        EVBDEV
332100980309     C     FVVFPP        IFEQ      'M'
332200980310     C                   MOVEL     0800          EVBHEV
332300980309     C                   ELSE
332400980310     C                   MOVEL     1200          EVBHEV
332500980309     C                   END
332600131031     C                   MOVEL     Devb01        EVBNOT
332700990803     C                   MOVEL     *BLANKS       EVBDTC
332800990803     C                   WRITE     FNEVB000
332900980311     C                   ELSE                                                   No Trv Mic
333000980311     C* Se l'evento c'è e ha data uguale alla distinta controllo
333100980311     C* se è già stato trasmesso
333200980311     C     EVBDTR        IFEQ      0                                            MIC trasm
333300980311     C* Se non è stato trasmesso aggiorno la data e l'ora di
333400980311     C* di immissione evento con quella del giorno + altri dati
333500980311     C                   Z-ADD     DATEU         EVBDTV
333600980311     C                   TIME                    EVBORV
333700980311     C                   Z-ADD     V1CLNA        EVBFLE
333800020905     C                   Z-ADD     DUTCOU        EVBCDU
333900131031     C                   MOVEL     Devb01        EVBNOT
334000980311     C     FVVFPP        IFEQ      'M'
334100980311     C                   MOVEL     0800          EVBHEV
334200980311     C                   ELSE
334300980311     C                   MOVEL     1200          EVBHEV
334400980311     C                   END
334500990803     C                   MOVEL     *BLANKS       EVBDTC
334600990803     C                   UPDATE    FNEVB000
334700980311     C* Altrimenti scrivo un nuovo evento di messa in consegna
334800980311     C                   ELSE                                                   No Trasm
334900990803     C                   CLEAR                   FNEVB000
335000980311     C                   MOVEL     'MIC'         EVBCEV
335100980311     C                   Z-ADD     DATEU         EVBDTV
335200980311     C                   TIME                    EVBORV
335300980311     C                   Z-ADD     V1CLNA        EVBFLE
335400020905     C                   Z-ADD     DUTCOU        EVBCDU
335500980318     C                   Z-ADD     KAAS          EVBAAS
335600980318     C                   Z-ADD     KLNP          EVBLNP
335700980318     C                   Z-ADD     KNRS          EVBNRS
335800980318     C                   Z-ADD     KNSP          EVBNSP
335900980311     C                   Z-ADD     FVVDFV        EVBDEV
336000980311     C     FVVFPP        IFEQ      'M'
336100980311     C                   MOVEL     0800          EVBHEV
336200980311     C                   ELSE
336300980311     C                   MOVEL     1200          EVBHEV
336400980311     C                   END
336500131031     C                   MOVEL     Devb01        EVBNOT
336600990803     C                   MOVEL     *BLANKS       EVBDTC
336700990803     C                   WRITE     FNEVB000
336800980311     C                   END                                                    Mic Trasm
336900980311     C*
337000980311     C                   END                                                    Trv MIC
337100980309     C*
337200980311     C                   END                                                    ARBNDC<>
337300980309     C*
337400031017     C     nomic         ENDSR
337500031017     C*-----------------------------------------------------*
337600031017     C* verifica esistenza spunte per scrittura MIC         *
337700031017     C*-----------------------------------------------------*
337800031017     C     eventimic     BEGSR
337900031017     c                   clear                   spunta            1
338000031017     c                   z-add     v01ndc        knf6
338100031020     C                   Z-ADD     V1CLNA        KFGS3
338200031017     c     kart          setll     fnart01l
338300031017     c                   do        *hival
338400031017     c     kart          reade     fnart01l                               96
338500031017     c   96              leave
338600070118     c     kbrv          chain     fnbrv01l
338700070118     c                   if        %found(fnbrv01l)
338800031017     c                   move      'X'           spunta
338900031017     c                   leave
339000031017     c                   endif
339100031017     c                   enddo
339200031017     c                   endsr
339300980309     C*-----------------------------------------------------*
339400980309     C* ANNMIC: Annullo evento di  "Messa in consegna"      *
339500980309     C*-----------------------------------------------------*
339600980309     C     ANNMIC        BEGSR
339700980309     C*
339800980311     C* Se non sono in immissione (nr.distinta bolla=video)
339900980311     C     V01NDC        IFEQ      ARBNDC
340000980311     C* Controllo se ho scritto evento MIC
340100980318     C     KARB1         CHAIN     FNLBL01L                           87
340200980429     C*
340300980318     C     *IN87         IFEQ      '0'
340400980318     C     ARBLNA        ANDEQ     LBLLAP
340500980429     C     KLBL          CHAIN     FNARB01L                           30
340600980429     C     *IN56         IFEQ      '0'
340700980429     C     ARBCCA        ANDEQ     '2'
340800980429     C     *IN56         OREQ      '0'
340900980429     C     ARBCCA        ANDEQ     '6'
341000980429     C                   SETON                                        87
341100980429     C                   END
341200980429     C     KARB2         CHAIN     FNARB01L                           30
341300980527     C                   END
341400980429     C*
341500980429     C     *IN87         IFEQ      '0'
341600980429     C     ARBLNA        ANDEQ     LBLLAP
341700980318     C                   Z-ADD     LBLAAP        KAAS
341800980318     C                   Z-ADD     LBLLPP        KLNP
341900980318     C                   Z-ADD     LBLNRP        KNRS
342000980318     C                   Z-ADD     LBLNSP        KNSP
342100980318     C                   ELSE
342200980309     C                   Z-ADD     ARBAAS        KAAS
342300980309     C                   Z-ADD     ARBLNP        KLNP
342400980309     C                   Z-ADD     ARBNRS        KNRS
342500980309     C                   Z-ADD     ARBNSP        KNSP
342600980318     C                   END
342700980309     C                   MOVEL     'MIC'         KCEV
342800990803     C     KEVB          SETGT     FNEVB04L
342900990803     C     KEVB          READPE    FNEVB04L                               33
343000980309     C     *IN33         IFEQ      '0'
343100990803     C     FVVDFV        ANDEQ     EVBDEV
343200980311     C* Se MIC esiste e la data della distinta è la stessa
343300980311     C* dell'evento ... controllo se è già trasmesso
343400980311     C     EVBDTR        IFNE      0
343500990803     C                   MOVEL     EVBDEV        WDTMIC           12 0
343600980311     C                   MOVE      EVBHEV        WDTMIC
343700980311     C* se l'evento di messa in consegna è stato trasmesso
343800980311     C* controllo se esiste già un evento NIC tolto dalla
343900980311     C* consegna
344000980311     C                   MOVEL     'NIC'         KCEV
344100990803     C     KEVB          SETGT     FNEVB04L
344200990803     C     KEVB          READPE    FNEVB04L                               33
344300031021     C* SE HO TROVATO EVENTO NIC
344400980311     C     *IN33         IFEQ      '0'                                          Wtr Nic
344500990803     C     FVVDFV        ANDEQ     EVBDEV
344600031021     C* Se non è stato trasmesso aggiorno la data e l'ora di
344700031021     C* di immissione evento con quella del giorno + altri dati
344800031021     C     EVBDTR        IFEQ      0
344900031021     C                   move      dateu         EVBDTV
345000031021     C                   TIME                    EVBORV
345100031021     C                   Z-ADD     v1clna        EVBFLE
345200031021     C                   Z-ADD     DUTCOU        EVBCDU
345300031021     C     FVVFPP        IFEQ      'M'
345400031021     C                   MOVEL     0800          EVBHEV
345500031021     C                   ELSE
345600031021     C                   MOVEL     1200          EVBHEV
345700031021     C                   END
345800031021     C                   MOVEL     *BLANKS       EVBDTC
345900031021     C                   UPDATE    FNEVB000
346000031021     C* Se è stato trasmesso controllo se non ha data più
346100031021     C* alta del MIC
346200031021     C                   ELSE
346300031021     C                   MOVEL     EVBDEV        WDTNIC           12 0
346400031021     C                   MOVE      EVBHEV        WDTNIC
346500031021     C     WDTNIC        IFLE      WDTMIC
346600031021     C* Se la data non è più alta scrivo un nuovo NIC
346700031021     C                   CLEAR                   FNEVB000
346800031021     C                   MOVEL     'NIC'         EVBCEV
346900031021     C                   move      dateu         EVBDTV
347000031021     C                   TIME                    EVBORV
347100031021     C                   Z-ADD     v1clna        EVBFLE
347200031021     C                   Z-ADD     DUTCOU        EVBCDU
347300031021     C                   Z-ADD     KAAS          EVBAAS
347400031021     C                   Z-ADD     KLNP          EVBLNP
347500031021     C                   Z-ADD     KNRS          EVBNRS
347600031021     C                   Z-ADD     KNSP          EVBNSP
347700031021     C                   Z-ADD     FVVDFV        EVBDEV
347800031021     C     FVVFPP        IFEQ      'M'
347900031021     C                   MOVEL     0800          EVBHEV
348000031021     C                   ELSE
348100031021     C                   MOVEL     1200          EVBHEV
348200031021     C                   END
348300131104     c                   clear                   Devb01
348400131104     C                   move      *zeros        §NOTNDC
348500131104     C                   move      *zeros        §NOTPDC
348600131031     C                   move      ARBNDC        §NOTNDC
348700131031     C                   move      ARBPDC        §NOTPDC
348800131031     C                   MOVEL     Devb01        EVBNOT
348900031021     C                   MOVEL     *BLANKS       EVBDTC
349000031021     C                   WRITE     FNEVB000
349100031021     C                   END
349200031021     C*
349300031021     C                   END
349400031021     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
349500031021     C* distinta lo scrivo
349600031021     C                   ELSE
349700031021     C                   CLEAR                   FNEVB000
349800031021     C                   MOVEL     'NIC'         EVBCEV
349900031021     C                   move      dateu         EVBDTV
350000031021     C                   TIME                    EVBORV
350100031021     C                   Z-ADD     v1clna        EVBFLE
350200031021     C                   Z-ADD     DUTCOU        EVBCDU
350300031021     C                   Z-ADD     KAAS          EVBAAS
350400031021     C                   Z-ADD     KLNP          EVBLNP
350500031021     C                   Z-ADD     KNRS          EVBNRS
350600031021     C                   Z-ADD     KNSP          EVBNSP
350700031021     C                   Z-ADD     FVVDFV        EVBDEV
350800031021     C     FVVFPP        IFEQ      'M'
350900031021     C                   MOVEL     0800          EVBHEV
351000031021     C                   ELSE
351100031021     C                   MOVEL     1200          EVBHEV
351200031021     C                   END
351300131104     c                   clear                   Devb01
351400131104     C                   move      *zeros        §NOTNDC
351500131104     C                   move      *zeros        §NOTPDC
351600131031     C                   move      ARBNDC        §NOTNDC
351700131031     C                   move      ARBPDC        §NOTPDC
351800131031     C                   MOVEL     Devb01        EVBNOT
351900031021     C                   MOVEL     *BLANKS       EVBDTC
352000031021     C                   WRITE     FNEVB000
352100031021     C*
352200031021     C                   END
352300031021     C* Se MIC non è stato trasmesso annullo fisicamente record
352400031021     C                   ELSE
352500031021     C* annullo fisicamente l'evento se da trasmettere
352600031021     C  N33              DELETE    FNEVB04L
352700031021     C                   END
352800031021     C                   END
352900980311     C                   END                                                    Data dist.=
353000980309     C*
353100980309     C                   ENDSR
353200911105     C*-----------------------------------------------------*
353300080319     C* controllo presenza orm e se dati scaricati a PDA         *
353400911105     C*-----------------------------------------------------*
353500080320     C     srfasi        BEGSR
353600080318     C                   CLEAR                   WORM
353700080318     C                   Z-ADD     wnfv          FVVNF6
353800090211     C     KORM          SETLL     FNORM06L                               34
353900090211     C   34              MOVEL     'S'           WORM              1
354000080320     c* se ho la fase PDR (preparato x pda ORM)
354100080320     c* non faccio manutenzionare il flag matt/pom
354200090211     C                   CLEAR                   faseorm           3
354300080320     c                   eval      dsftrd = 'PDR'
354400080326     C     KDST4         chain     fidsf02l
354500080326     c                   if        %found(fidsf02l)
354600080320     c                   eval      faseorm = dsftrd
354700080320     c                   seton                                        82
354800080326     c                   end
354900080319     c* controllo se i flag per PDA e mat/pom sono modificabili
355000080320     c                   clear                   fasecon           3
355100080319     c                   setoff                                       7282
355200080319     c* se ho la fase PDC (preparato x pda con) o PDO x orm
355300080319     c* non faccio manutenzionare il flag matt/pom
355400080319     c                   eval      dsftrd = 'PDC'
355500080326     C     KDST4         chain     fidsf02l
355600080326     c                   if        %found(fidsf02l)
355700080320     c                   eval      fasecon = dsftrd
355800080319     c                   seton                                        82
355900080326     c                   end
356000080410     c* se ho la fase CUS (ok da spc) non faccio manutenzionare i flag
356100080410     c* dati a pda e matt/pom sia x con che x orm
356200080410     c                   eval      dsftrd = 'CUS'
356300080410     C     KDST4         chain     fidsf02l
356400080410     c                   if        %found(fidsf02l)
356500080410     c* chiamo il pgm per richiesta forzatura
356600080410     c                   exsr      srforza
356700130308     c*                  eval      fasecon = dsftrd
356800110224     c*                  seton                                        7282
356900080410     c                   end
357000080318     C                   ENDSR
357100080318     C*-----------------------------------------------------*
357200080410     C* pgm per forzare modifica distinta fase CUS          *
357300080318     C*-----------------------------------------------------*
357400080410     C     srforza       BEGSR
357500911105     C*
357600080410     c                   clear                   fidg44ds
357700080410     c                   eval      co44fgs=fvvfgs
357800080410     c                   eval      co44ndc=wnfv
357900080410     c                   eval      co44wnd = 'S'
358000080410     c                   eval      co44td = 'E'
358100080410     c* se ho la fase CUS e non la fase PDC vuol dire che è una distinta di
358200080410     c* di soli orm. Imposto O nel tipo distinta per evitare controlli
358300080410     c* inerenti alle consegne
358400080410     c                   if        fasecon = ' ' and worm = 'S'
358500080410     c                   eval      co44td = 'O'
358600080410     c                   end
358700080410     c                   movel(p)  fidg44ds      kpjbu
358800080410     c                   call      'FIDG44R'
358900080410     c                   parm                    kpjba
359000080410     c                   movel     kpjbu         fidg44ds
359100080410     c* se mi ritorna che non ha forzato accendo indicatore d'errore
359200080410     c                   if        co44fcus = 'N'
359300080410     c                   seton                                        90
359400080410     c                   end
359500080410     C*
359600080410     C                   ENDSR
359700080410     C*-----------------------------------------------------*
359800080410     C* ANNULLAMENTO DISTINTA                               *
359900080410     C*-----------------------------------------------------*
360000080410     C     ANNUL         BEGSR
360100080410     C*
360200911105     C* CONTROLLO SE SONO IN IMMMISSIONE O VARIAZIONE DISTINTA
360300911105     C*
360400000717     C                   SETOFF                                       78
360500000717     C                   Z-ADD     V01NDC        FVVNF6
360600070118     C     KFIBRV        SETLL     FNBRV01L                               78
360700000717     C     *IN78         IFEQ      '1'
360800000717     C                   SETON                                        839075
360900000717     C                   MOVEL     ERR(21)       $MSG
361000000717     C                   ELSE
361100941107     C     V01NDC        IFEQ      *ZEROS
361200940322     C                   SETON                                        7590
361300940322     C                   MOVEL     ERR(4)        $MSG
361400911105     C                   ELSE
361500010403     C* WORM = "S"  --->  ESISTONO O.R.M. IN FASE < 900
361600010403    3C     WORM          IFEQ      'S'
361700010403     C                   SETON                                        789075
361800010403     C                   MOVEL     ERR(22)       $MSG
361900010403   X3C                   ELSE
362000020905     C                   Z-ADD     V1CLNA        KFGS
362100020905     C                   Z-ADD     V01NDC        KNDC
362200960702     C                   Z-ADD     ROVDDC        KDDC
362300020905     C     KARB          SETLL     FNARB70L
362400911105     C*
362500911105     C                   DO        *HIVAL
362600911105     C*
362700080318     C     KARB          READE     FNARB70L
362800080318     C                   if        %eof(fnarb70l)
362900080318     c                   leave
363000080318     c                   end
363100941109     C     KARB1         CHAIN     FNARB01L                           31
363200980309     C* Se era in consegna e adesso non c'è più annullo la messa
363300980309     C* consegna
363400980309     C                   EXSR      ANNMIC
363500980309     C*
363600950602     C                   Z-ADD     *ZEROS        ARBNSS
363700020905     C                   Z-ADD     *ZEROS        ARBIFP
363800020905     C                   Z-ADD     *ZEROS        ARBNDC
363900911108     C                   Z-ADD     *ZEROS        ARBDDC
364000911108     C                   Z-ADD     *ZEROS        ARBSTP
364100911108     C                   Z-ADD     *ZEROS        ARBNGD
364200941108     C                   Z-ADD     *ZEROS        ARBPDC
364300941109     C                   UPDATE    FNARB000
364400030910     c* se la bolla ha particolarità RC(ORM contestuale alla consegna)
364500030910     c* deve andare a togliere la distinta dall'orm
364600030910     c                   exsr      srorm
364700070319     c* tolgo i riferimenti della distinta anche su FIARG x
364800070319     c* elaborare eventualmente la spedizione nelle distinte automatiche
364900070319     c                   exsr      srfiarg
365000080410     c* richiamo il pgm per aggiornare i dati per PDA
365100080410     c                   if        §OGPDAcon <> ' ' and
365200080411     c                             (v1cpda =  'C' or v1cpda = 'E' or
365300080411     c                             v1ctstpda = 'C' or
365400080411     c                             v1ctstpda = 'E')
365500080410     c                   clear                   fidg42ds
365600080410     c                   eval      co42tla = 'A'
365700080410     c                   exsr      srpda
365800080410     c                   end
365900930624     C* ELIMINO EVENTUALI ANOMALIE RELATIVE ALLA BOLLA
366000931214     C  N31              EXSR      ANOM
366100911105     C*
366200160429     C     KARB1         CHAIN     FNARBW2L
366300160429     c                   if        %found(fnarbw2l)
366400160429     C                   DELETE    FNARBW2L
366500160429     c                   end
366600080318     C                   ENDdo
366700911105     C*
366800020905     C                   Z-ADD     V1CLNA        FVVFGS
366900020905     C     KFVV          CHAIN     FNFVV01L                           83
367000920914     C  N83              MOVEL     '*'           FVVATB
367100970401     C  N83              MOVEL     *BLANKS       FVVFCF
367200941109     C  N83              UPDATE    FNFVV000
367300000515     C* FOGLIO 8
367400000717     C  N83              Z-ADD     8             FVVNPG
367500020905     C  N83KFVV          CHAIN     FNFVV01L                           83
367600000515     C  N83              MOVEL     '*'           FVVATB
367700000515     C  N83              MOVEL     *BLANKS       FVVFCF
367800000515     C  N83              UPDATE    FNFVV000
367900000515     C                   Z-ADD     4             FVVNPG
368000950601     C*
368100120718     c* aggiorno il file FIDST00F
368200120718     C  N83KDST2         CHAIN     FiDST09L                           83
368300120807     C  N83              MOVEL     'A'           £DSTATB
368400120718     C  N83              UPDATE    FiDST000
368500120718     c                   exsr      srfidst
368600010403    3C                   END
368700911105     C                   END
368800000218     C*
368900000717     C                   END
369000000218     C* REIMPOSTO LA DATA DISTINTA CON LA DATA DEL GIORNO
369100000218     C                   MOVE      WHHDAT        V1CDDC
369200911105     C*
369300911105     C                   ENDSR
369400930623     C*-----------------------------------------------------*
369500930623     C* GESTIONE ANOMALIE DELLA BOLLA                       *
369600930623     C*-----------------------------------------------------*
369700930623     C     ANOM          BEGSR
369800930623     C*
369900960415     C                   CLEAR                   DSLR33
370000960415     C                   MOVE      4             D33FSC
370100960415     C                   MOVE      ROVDDC        D33DCH
370200960415     C*
370300990519     C                   MOVE      ARBAAS        WAA4              4 0
370400950915     C                   Z-ADD     135           §§§CAA
370500990519     C     KANM          SETLL     FNANM01L                           34
370600930624     C     *IN34         DOWEQ     '0'
370700990519     C     KANM          READE     FNANM01L                               34
370800960415     C     *IN34         IFEQ      *OFF
370900960415     C                   Z-ADD     ANMNR1        D33NRR
371000960415     C                   CALL      'FNLR33R'
371100960415     C                   PARM                    DSLR33
371200960415     C                   MOVE      '1'           W33R              1
371300960415     C                   END
371400960415     C                   ENDDO
371500950918     C*
371600950915     C                   Z-ADD     150           §§§CAA
371700990519     C     KANM          SETLL     FNANM01L                           34
371800950915     C     *IN34         DOWEQ     '0'
371900990519     C     KANM          READE     FNANM01L                               34
372000960415     C     *IN34         IFEQ      *OFF
372100960415     C                   Z-ADD     ANMNR1        D33NRR
372200960415     C                   CALL      'FNLR33R'
372300960415     C                   PARM                    DSLR33
372400960415     C                   MOVE      '1'           W33R              1
372500960415     C                   END
372600960415     C                   ENDDO
372700930623     C*
372800930623     C                   ENDSR
372900940322     C*-----------------------------------------------------*
373000940322     C* PULIZIA RICHIESTA PRECEDENTE                        *
373100940322     C*-----------------------------------------------------*
373200940322     C     PULRIC        BEGSR
373300911106     C*
373400911106     C* PULISCO  IL FILE DI LAVORO
373500911106     C*
373600941109     C     *LOVAL        SETLL     FNARBW1L
373700911106     C*
373800911106     C                   DO        *HIVAL                                       4
373900941109     C                   READ      FNARBW1L                               55
374000911106     C*
374100941109     C  N55              DELETE    FNARBW1L
374200911106     C  N55              END
374300911106     C*
374400950907     C     D50TPR        IFEQ      ' '
374500920423     C                   MOVEL     *BLANK        SAVCPC
374600941107     C                   MOVEL     *BLANK        V1CCPC
374700941107     C                   MOVE      V01NDC        V1CNDP
374800941107     C                   MOVEL     *BLANK        V1CNDC
374900950601     C                   MOVEL     *BLANK        V1CPEP
375000050524     C                   MOVEL     *blanks       V1Ctpm
375100160429     C                   MOVEL     *blanks       V1Ctur
375200080121     c                   clear                   v1cpda
375300080208     c                   clear                   v1ctstpda
375400080208     c                   clear                   msgv
375500080318     c                   clear                   msgv1
375600080415     c                   setoff                                       45
375700950608     C                   MOVE      WHHDAT        V1CDDC
375800170208     c                   clear                   hPDRAutista
375900950907     C                   END
376000911106     C*
376100911106     C                   ENDSR
376200940322     C*-----------------------------------------------------*
376300940322     C* Gestione eventuale attribuzione Stop                *
376400940322     C*-----------------------------------------------------*
376500940322     C     GESSTP        BEGSR
376600951121     C*
376700951207     C                   Z-ADD     0             CODCLI
376800951121     C                   MOVEL     *BLANKS       RAGCLI
376900951207     C*  Controllo se devo attribuire lo Stop
377000951207     C                   Z-ADD     1             X                 2 0
377100951207     C                   MOVEL     *BLANKS       V3CCBO
377200951207     C                   MOVEL     *BLANKS       WTBOL1
377300951207     C     ARBCBO        LOOKUP    C3A(X)                                 13
377400951207     C     *IN13         IFEQ      '1'
377500951207     C                   MOVEL     D3A(X)        DS3A
377600951207     C                   MOVEL     §3ATB1        WTBOL1
377700990920     C                   MOVEL     §3ATB2        WTBOL2
377800990920     C                   END
377900990920     C* Se la prima bolla è un assegnato eseguo posizionamento
378000990920     C* su FIAR6 con flag tipo bolla impostato a '1' .....
378100990920     C     WTBOL1        IFEQ      'A'
378200990920     C                   MOVEL     '1'           KTRC
378300990920     C                   Z-ADD     ARBAAS        KAAS
378400990920     C                   Z-ADD     ARBAAS        KLNP
378500990920     C                   Z-ADD     ARBAAS        KNRS
378600990920     C                   Z-ADD     ARBAAS        KNSP
378700990920     C     KAR6          CHAIN     FIAR601L                           74
378800990920     C* ... se non trovo bolla o il codice cliente non è
378900990920     C* impostato reperisco il codice da FNARB
379000990920     C     *IN74         IFEQ      '1'
379100990920     C     AR6KSC        OREQ      0
379200990920     C                   Z-ADD     ARBKSC        CODCLI
379300990920     C                   ELSE
379400990920     C* ... altrimenti recupero codice cliente da FIAR6
379500990920     C                   Z-ADD     AR6KSC        CODCLI
379600990920     C                   END
379700990920     C* .... se prima bolla è un porto franco ....
379800990920     C                   ELSE
379900990920     C* .... verifico se la seconda è un assegnato
380000990920     C     WTBOL2        IFEQ      'A'
380100990920     C                   MOVEL     '2'           KTRC
380200990920     C                   Z-ADD     ARBAAS        KAAS
380300990920     C                   Z-ADD     ARBAAS        KLNP
380400990920     C                   Z-ADD     ARBAAS        KNRS
380500990920     C                   Z-ADD     ARBAAS        KNSP
380600990920     C     KAR6          CHAIN     FIAR601L                           74
380700990920     C* ... e reperisco il codice bolla da lì
380800990920     C     *IN74         IFEQ      '0'
380900990920     C     AR6KSC        ANDNE     0
381000990920     C                   Z-ADD     AR6KSC        CODCLI
381100990920     C                   END
381200990920     C                   END
381300990920     C*
381400990920     C                   END
381500951207     C*
381600951207     C*  Controllo se cliente no generico e se si attribuisco stop
381700951207     C                   Z-ADD     CODCLI        APPOG             4 0
381800951207     C     CODCLI        IFNE      0
381900951207     C     APPOG         ANDNE     9999
382000951207     C     APPOG         ANDNE     8888
382100951211     C                   MOVEL     *BLANKS       RAGCLI
382200951211     C                   MOVEL     CODCLI        RAGCLI
382300951207     C                   ELSE
382400940322     C*  Controllo se devo attribuire lo Stop
382500951121     C                   MOVEL     ARBRSD        RAGCLI
382600951121     C                   MOVE      ARBIND        RAGCLI
382700951207     C                   END
382800940322     C*  Controllo se cliente esiste già
382900940322     C                   Z-ADD     1             Y                 3 0
383000951121     C     RAGCLI        LOOKUP    RSD2(Y)                                13
383100940322     C     *IN13         IFEQ      '1'
383200940322     C                   Z-ADD     STP2(Y)       ARBSTP
383300940322     C                   ELSE
383400961217     C     W             IFLT      999
383500940323     C                   ADD       1             ULTSTP            3 0
383600940322     C                   ADD       1             W
383700940323     C                   Z-ADD     ULTSTP        ARBSTP
383800940322     C                   Z-ADD     ULTSTP        STP2(W)
383900951121     C                   MOVEL     RAGCLI        RSD2(W)
384000940322     C                   END
384100951123     C                   END
384200940322     C*
384300940322     C*
384400940322     C                   ENDSR
384500940322     C*-----------------------------------------------------*
384600940322     C* Controllo gli stop                                  *
384700940322     C*-----------------------------------------------------*
384800940322     C     CTRSTP        BEGSR
384900940322     C*
385000940322     C                   SETOFF                                       16
385100941108     C     V3CPST        IFNE      V3CSTP
385200941108     C     V3CSTP        ANDNE     0
385300941108     C     V3CSTP        IFGT      ULTSTP
385400941108     C                   Z-ADD     V3CSTP        ULTSTP
385500940323     C                   END
385600951207     C                   MOVE      V3CCLI        APPOG
385700951207     C*
385800951207     C     V3CTB1        IFEQ      'A'
385900951207     C     APPOG         ANDNE     8888
386000951207     C     APPOG         ANDNE     9999
386100951211     C                   MOVEL     *BLANKS       RAGCLI
386200951211     C                   MOVEL     V3CCLI        RAGCLI
386300951207     C                   Z-ADD     1             X
386400951207     C     V3CSTP        LOOKUP    STP2(X)                                13
386500951207     C     *IN13         IFEQ      '1'
386600951207     C     RSD2(X)       ANDNE     RAGCLI
386700951207     C     RSD2(X)       ANDNE     *BLANKS
386800951207     C                   Z-ADD     NRR           NRRKEY
386900951207     C                   SETON                                        7516
387000951207     C                   MOVEL     ERR(5)        $MSG
387100951207     C                   ELSE
387200951207     C                   MOVEL     V3CSTP        V3CPST
387300951207     C     *IN13         IFEQ      '0'
387400951207     C                   ADD       1             W
387500951207     C                   Z-ADD     V3CSTP        STP2(W)
387600951207     C                   MOVEL     RAGCLI        RSD2(W)
387700951207     C                   END                                                    NEW STOP
387800951207     C                   END                                                    STOP GIA' ESIST.
387900951207     C                   ELSE                                                   ASSEGNATO COD.
388000940330     C*
388100940322     C                   Z-ADD     1             X
388200941108     C     V3CSTP        LOOKUP    STP2(X)                                13
388300940322     C*
388400940322     C     *IN13         IFEQ      '1'
388500951121     C     RSD2(X)       ANDNE     V3CRAG
388600951121     C     RSD2(X)       ANDNE     *BLANKS
388700940323     C                   Z-ADD     NRR           NRRKEY
388800940322     C                   SETON                                        7516
388900940322     C                   MOVEL     ERR(5)        $MSG
389000940322     C                   ELSE
389100941108     C                   MOVEL     V3CSTP        V3CPST
389200940322     C     *IN13         IFEQ      '0'
389300961217     C     W             ANDLT     999
389400940322     C                   ADD       1             W
389500951121     C                   MOVEL     V3CRAG        RAGCLI
389600941108     C                   Z-ADD     V3CSTP        STP2(W)
389700951121     C                   MOVEL     RAGCLI        RSD2(W)
389800951207     C                   END                                                    NEW STOP
389900951207     C                   END                                                    ESISTE GIA'
390000951207     C*
390100951207     C                   END                                                    ASSEGN. COD.
390200940322     C*
390300951123     C                   END
390400951123     C*
390500940322     C                   ENDSR
390600030910     C*-----------------------------------------------------*
390700070319     C*  toglie o mette distinta sul FIARG
390800030910     C*-----------------------------------------------------*
390900070319     C     srfiarg       BEGSR
391000070322     c                   if        comdda <= dateu and comdda <> 0
391100070319     c     karb1         chain(e)  fiarg01l
391200070531     c                   if        not %error and %found(fiarg01l)
391300070531     c                             and arbndc = 0
391400070531     C                   eval      argfgs = ARBIFP
391500070531     C                   eval      argNDC = ARBNDC
391600070531     C                   eval      argddc = ARBDDC
391700070531     C                   eval      argstp = ARBSTP
391800070531     C                   eval      argpdc = ARBPDC
391900070531     c                   clear                   argnftl
392000070531     c                   clear                   ARGDTVDIS
392100070531     c                   clear                   ARGHVDIS
392200070531     c                   eval      argslb = 0
392300100203     c                   movel     argflo        dargflo
392400100203     c                   clear                   §argattesa
392500100203     c                   movel     dargflo       argflo
392600070531     c                   update    fiarg000
392700070531     c                   end
392800070410     c                   if        not %found(fiarg01l) and arbndc <> 0
392900070509     c                             and not %Error
393000070410     c                   clear                   fiargds
393100070410     c                   z-add     arbaas        argaas
393200070410     c                   z-add     arblnp        arglnp
393300070410     c                   z-add     arbnrs        argnrs
393400070410     c                   z-add     arbnsp        argnsp
393500070410     c                   z-add     arblna        arglna
393600070410     c                   z-add     arbifp        argpoc
393700070508     c******* Questo bloccava Tellus nell'attribuzione dei GIRI
393800070508     c******* quindi è stato tolto.
393900070508     c*******            eval      ARGTGI = 'M'
394000070410     c                   write     fiarg000
394100070319     c                   end
394200080520     c                   unlock    fiarg01l
394300070410     c                   end
394400080410     c                   endsr
394500080410     C*----------------------------------------------------*
394600080410     C* se pda e tolgo o metto spedizione e fase PDC richiamo pgm per
394700080410     c* aggiornare i dati al PDA
394800080410     C*----------------------------------------------------*
394900080410     C     srpda         BEGSR
395000080410     c                   eval      co42FGS = v1clna
395100080410     C                   eval      co42NDC = v01ndc
395200080410     c                   eval      co42ddc = rovddc
395300080911     c                   if        co42tla = 'C'
395400080911     c                   clear                   co42aas
395500080911     c                   clear                   co42lnp
395600080911     c                   clear                   co42nrs
395700080911     c                   clear                   co42nsp
395800080911     c                   else
395900080410     c                   eval      co42aas = arbaas
396000080410     c                   eval      co42lnp  = arblnp
396100080410     c                   eval      co42nrs  = arbnrs
396200080410     c                   eval      co42nsp  = arbnsp
396300080911     c                   end
396400080410     c                   movel(p)  fidg42ds      kpjbu
396500080410     c                   call      'FIDG42R'
396600080410     c                   parm                    kpjba
396700080410     c                   parm                    fnarbds
396800080410     c* ritorno errore ???
396900080410     C                   ENDSR
397000070319     C*-----------------------------------------------------*
397100070319     C*  toglie o mette distinta sull'orm x particolarità RC
397200070319     C*-----------------------------------------------------*
397300070319     C     srorm         BEGSR
397400100514     C     arbgma        lookup    s7rc                                   84
397500100514     c                   if        *in84
397600030910     c                   clear                   fnlr99ds
397700030910     c                   eval      i99tla = 'L'
397800030910     c                   eval      i99aas = arbaas
397900030910     c                   eval      i99lnp = arblnp
398000030910     c                   eval      i99nrs = arbnrs
398100030910     c                   eval      i99nsp = arbnsp
398200030910     c                   eval      i99ndc = arbndc
398300030910     c                   eval      i99ddc = arbddc
398400071001     c                   eval      i99fgs = v1clna
398500030910     c* toglie
398600030910     c                   if        arbndc = 0
398700030910     c                   eval      i99fao = 390
398800030910     c                   else
398900030910     c* mette
399000030910     c                   eval      i99fao = 400
399100030910     c                   eval      i99npg = 4
399200071002     c                   eval      i99nftl= 99999
399300030910     c                   end
399400030910     c                   call      'FNLR99R'
399500030910     c                   parm                    kpjba
399600030910     c                   parm                    fnlr99ds
399700030910     c                   end
399800030910     C                   ENDSR
399900070612      *---------------------------------------------------------*
400000070612      *  Determinazione del volume da utilizzare
400100070612      *---------------------------------------------------------*
400200070612     C     CALVOL        BEGSR
400300070612     c* t=volume da fatturare
400400070612     C     ARBFVF        IFEQ      'T'
400500070612     C                   Z-ADD     ARBVLF        WVOL
400600070612     C                   ELSE
400700070612     C     ARBNCR        ifeq      ARBNCL
400800070612     C                   Z-ADD     ARBVLC        WVOL
400900070612     C                   ELSE
401000070612     C     ARBVLC        IFGE      ARBVLF
401100070612     C                   Z-ADD     ARBVLC        WVOL
401200070612     C                   ELSE
401300070612     C                   Z-ADD     ARBVLF        WVOL
401400070612     C                   ENDIF
401500070612     C                   ENDIF
401600070612     C                   ENDIF
401700070612     C*
401800070612     C                   ENDSR
401900070612      *---------------------------------------------------------*
402000070612      *  Determinazione del peso da utilizzare
402100070612      *---------------------------------------------------------*
402200070612     C     calPES        BEGSR
402300070612     C     arbncp        ifeq      arbncl
402400070612     C                   Z-ADD     arbPKC        wpes
402500070612     C                   ELSE
402600070612     C     arbPKC        ifge      arbPKF
402700070612     C                   Z-ADD     arbPKC        wpes
402800070612     C                   else
402900070612     C                   Z-ADD     arbPKF        wpes
403000070612     C                   end
403100070612     C                   ENDIF
403200070612     C                   ENDSR
403300141013      *_______________________________________________________________*
403400141013     c     verdispo      begsr
403500141013      *_______________________________________________________________*
403600141013     c                   eval      ilry09tla = 'L'
403700141029     c                   eval      ILRY09TDIS = 'E'
403800141029     c                   movel     fnlry09ds2    kpjbu
403900141013     c                   call      'FNLRY09R'
404000141013     c                   parm                    kpjba
404100141029     c                   movel     kpjbu         fnlry09ds2
404200141013     c                   endsr
404300160510     C*--------------------------------------------------------------*
404400160510     C*  parametri ultimo lancio assegnazione automatica
404500160510     C*--------------------------------------------------------------*
404600160510     C     srprm         BEGSR
404700160510     c                   movel     'FIDG17R'     pgm
404800160510     c     kprm          chain     fiprm01l
404900160510     c                   if        %found(fiprm01l) and prmdta = oggi
405000160510     c                   movel     prmpar        fidg17ds
405100160510     c                   else
405200160510     c                   clear                   fidg17ds
405300160510     c                   eval      d17tur1 = 'U'
405400160510     c                   end
405500160510     C                   ENDSR
405600940322     C*-----------------------------------------------------*
405700941107     C*    Operazioni iniziali
405800941107     C*-----------------------------------------------------*
405900940322     C     *INZSR        BEGSR
406000950907     C*
406100940322     C     *ENTRY        PLIST
406200940322     C                   PARM                    KPJBA
406300950907     C                   MOVEL     KPJBU         FNLR50
406400030701     c                   z-add     1             tblkut
406500020905     c*
406600020905     c     *dtaara       define    §azute        azuteds
406700020905     c     *dtaara       define    §datiute      ddatiute
406800020905     C                   in(E)     *dtaara
406900020905     C                   IF        %Error  or  RSUT = *blanks
407000020905     C                   call      'TIBS34R'
407100020905     C                   parm                    Tibs34Ds
407200020905     C                   in        *dtaara
407300020905     c                   ENDIF
407400941107     C*
407500941107     C* Inverto la data del giorno
407600941107     C                   TIME                    WHHDAT           14 0
407700941107     C                   MOVE      WHHDAT        G02DAT
407800940322     C                   MOVE      *BLANKS       G02ERR
407900941107     C                   CALL      'XSRDA8'
408000941107     C                   PARM                    WLBDA8
408100941107     C                   Z-ADD     G02INV        DATEU             8 0
408200160510     C                   movel     DATEU         oggi
408300020905     C*
408400990920     C*  Aggancio tabella 'GED' x reperire la divisa corrente
408500990920     C                   MOVEL     'GED'         KCOD1
408600990920     C                   MOVEL     *BLANKS       KKEY1
408700990920     C                   MOVEL     '1'           KKEY1
408800990920     C     KTBE          CHAIN     TNTBE01L                           13
408900990920     C     *IN13         IFEQ      '0'
409000990920     C                   MOVEL     TBEUNI        DGED
409100990920     C                   END
409200160316     C*  Aggancio tabella 'NSD' informazioni vedi pacco dpd
409300160316     C                   MOVEL     'NSD'         KCOD1
409400160316     C                   MOVEL     *BLANKS       KKEY1
409500160316     C                   MOVEL     '1'           KKEY1
409600160316     C     KTBE          CHAIN     TNTBE01L                           13
409700160316     C     *IN13         IFEQ      '0'
409800160316     C                   MOVEL     TBEUNI        DNSD
409900160316     C                   END
410000000929     C*
410100000929     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
410200000929     C                   MOVEL     'DGD'         KCOD1
410300000929     C                   MOVEL     *BLANKS       KKEY1
410400000929     C                   MOVEL     '1'           KKEY1
410500000929     C     KTBE          CHAIN     TNTBE01L                           13
410600000929     C     *IN13         IFEQ      '0'
410700000929     C                   MOVEL     TBEUNI        DDGD
410800000929     C                   END
410900940322     C*  CARICO DATI TABELLA 3A
411000940323     C                   MOVEL     '3A'          TBLCOD
411100940322     C                   Z-ADD     0             X
411200940322     C     KTAB1         CHAIN     TABEL00F                           13
411300940322     C     *IN13         DOWEQ     '0'
411400941108     C     X             ANDLT     100
411500940322     C                   ADD       1             X
411600940322     C                   MOVEL     TBLKEY        C3A(X)
411700941109     C                   MOVEL     TBLUNI        D3A(X)
411800940323     C     KTAB1         READE     TABEL00F                               13
411900940322     C                   END
412000941110     C*
412100100203     C*  CARICO DATI TABELLA 7r part. consegne attesa
412200100203     C                   MOVEL     '7R'          TBLCOD
412300100203     C                   Z-ADD     0             X
412400100203     C     KTAB1         CHAIN     TABEL00F                           13
412500100203     C     *IN13         DOWEQ     '0'
412600100514     c                   movel     tbluni        ds7r
412700100514     C                   if        x < 100 and
412800100514     c                             §7ratt = 'S'
412900100203     C                   ADD       1             X
413000100203     C                   MOVEL     TBLKEY        s7r(X)
413100100514     c                   end
413200100517     C                   if        xx < 100 and
413300100514     c                             §7RRC  = 'S'
413400100514     C                   ADD       1             Xx
413500100514     C                   MOVEL     TBLKEY        s7rc(Xx)
413600100514     c                   end
413700100203     C     KTAB1         READE     TABEL00F                               13
413800100203     C                   END
413900941110     C*  CARICO DATI TABELLA CV
414000990920     C                   MOVEL     *BLANKS       DIVEUR
414100941110     C                   MOVEL     'CV'          TBLCOD
414200941110     C                   Z-ADD     0             X
414300941110     C     KTAB1         CHAIN     TABEL00F                           13
414400941110     C     *IN13         DOWEQ     '0'
414500941110     C     X             ANDLT     100
414600941110     C                   ADD       1             X
414700990920     C                   MOVEL     TBLKEY        DIVCV             3
414800990920     C                   MOVEL     TBLUNI        DSCV
414900990920     C* Se divisa diversa dalla moneta di conto reperisco data
415000990920     C* validità x trovare altra divisa da utilizzare x incassi
415100011009     C     §GEDCR        IFNE      DIVCV
415200990920     C     DIVEUR        ANDEQ     *BLANKS
415300990920     C     §CVDVA        ANDGE     'S'
415400990920     C                   MOVEL     DIVCV         DIVEUR            3
415500990920     C                   END
415600941110     C     KTAB1         READE     TABEL00F                               13
415700941110     C                   END
415800941108     C*
415900941108     C*  CARICO DATI TABELLA 1P
416000941108     C                   MOVEL     '1P'          TBLCOD
416100941108     C                   Z-ADD     0             X
416200941108     C     KTAB1         CHAIN     TABEL00F                           13
416300941108     C     *IN13         DOWEQ     '0'
416400941108     C     X             ANDLT     50
416500941108     C                   ADD       1             X
416600941108     C                   MOVEL     TBLUNI        DS1P
416700941108     C                   MOVEL     TBLKEY        C1P(X)
416800941108     C                   MOVEL     §1PDES        D1P(X)
416900941108     C     KTAB1         READE     TABEL00F                               13
417000941108     C                   END
417100940322     C*
417200941107     C                   MOVEL     'FNLR50R'     VIDPGM
417300940322     C                   SETON                                        2003
417400950907      *  Controllo se pgm richiamato da gestione fogli distinta
417500950907      *  consegna
417600950907     C                   SETOFF                                       02
417700950907     C     D50TPR        IFEQ      'C'
417800950907     C                   SETON                                        02
417900950908     C                   Z-ADD     D50FIL        V1CLNA
418000950907     C                   MOVE      D50NDT        V1CNDC
418100950907     C                   Z-ADD     D50NDT        V01NDC
418200020905     C                   Z-ADD     D50FPD        V1CFPD
418300950907     C                   Z-ADD     D50DDT        V1CDDC
418400950907     C                   MOVE      D50CPD        V1CCPC
418500950907     C                   MOVEL     D50FPP        V1CFPP
418600950907     C                   MOVEL     D50PEP        V1CPEP
418700050523     C                   MOVEL     D50tpm        V1Ctpm
418800950908     C                   MOVE      D50FIL        FILIA             3 0
418900950907     C                   ELSE
419000020730      * x As Unico imposta la filiale del P.O.
419100020905     C                   z-add     dutpou        FILIA             3 0
419200020730     C                   SETOFF                                       21
419300020730      * se è il primo livello
419400020905     c                   if        dutlpo = '1'
419500020730     C                   SETON                                        21
419600020730     c                   end
419700020730      *
419800950907     C                   MOVE      WHHDAT        V1CDDC
419900950907     C                   MOVEL     'M'           V1CFPP
420000080121     C                   clear                   V1Cpda
420100080208     C                   clear                   V1Ctstpda
420200080208     C                   clear                   msgv
420300080318     C                   clear                   msgv1
420400080415     c                   setoff                                       45
420500050524     C                   MOVEL     *blanks       V1CTPM
420600160429     C                   MOVEL     *blanks       V1CTur
420700950907     C* VALORIZZO IL CAMPO A VIDEO DI LINEA
420800950907     C                   MOVE      FILIA         V1CLNA
420900950907     C                   MOVE      FILIA         V1CFPD
421000950907     C                   END
421100050520     C* Parcel / messaggerie
421200050520     c                   exsr      srtpm
421300050520     C* inverto la data distinta
421400050520     c                   exsr      srddc
421500020701     C* Aggancio la £6 e recupero le linee a loro associate
421600020701     c                   clear                   trul06ds
421700020701     c                   eval      d06cod = '£6'
421800020701     c                   movel     filia         d06key
421900040831     c                   move      'S'           d06key
422000040831     C                   Z-ADD     G02INV        d06drf
422100020701     c                   movel     trul06ds      kpjbu
422200020701     c                   call      'TRUL06R'
422300020701     c                   parm                    kpjba
422400020701     c                   movel     kpjbu         trul06ds
422500020701     c                   movea     lint          Lin
422600020701     c
422700020701     C* Aggancio la £6 e recupero le linee a loro associate
422800020701     c                   clear                   trul06ds
422900020701     c                   eval      d06cod = '£6'
423000020701     c                   movel     filia         d06key
423100020701     c                   move      'G'           d06esc
423200040831     c                   move      'S'           d06key
423300040831     C                   Z-ADD     G02INV        d06drf
423400020701     c                   movel     trul06ds      kpjbu
423500020701     c                   call      'TRUL06R'
423600020701     c                   parm                    kpjba
423700020701     c                   movel     kpjbu         trul06ds
423800020701     c                   movea     lint          L£6
423900140926      * verifica presenza disposizioni web
424000141029     c                   clear                   fnlry09ds2
424100140926     c                   eval      ilry09fgs = filia
424200140926     c                   eval      ilry09tch = 'C'
424300141013     c                   exsr      verdispo
424400141029     c                   if        OLRY09ESAL = '1' and
424500140926     c                             OLRY09ERR  = *blank
424600140926     c                   seton                                        97
424700170208      **
424800170208     c                   clear                   hPdrAutista       7 0
424900170208      **
425000140926     c                   endif
425100940322      *---------------------------------------------------------------*
425200940322      * CHIAVI                                                        *
425300940322      *---------------------------------------------------------------*
425400160510     c     kprm          klist
425500160510     c                   kfld                    v1clna
425600160510     c                   kfld                    pgm              10
425700070312     C     Kdst2         KLIST
425800070410     C                   KFLD                    fvvFGS
425900070312     C                   KFLD                    fvvnpg
426000070312     C                   KFLD                    wnfv
426100070312     C     Kdst1         KLIST
426200070312     C                   KFLD                    fvvFGS
426300070312     C                   KFLD                    wnfv
426400080121     C     Kdst3         KLIST
426500080121     C                   KFLD                    fvvfgs
426600080121     C                   KFLD                    knpg
426700080121     C                   KFLD                    wnfv
426800080318     C     Kdst4         KLIST
426900080318     C                   KFLD                    knpg
427000080318     C                   KFLD                    wnfv
427100080318     C                   KFLD                    fvvfgs
427200080319     C                   KFLD                    dsftrd
427300021204     C     Kapd          KLIST
427400021204     C                   KFLD                    apdtip
427500021204     C                   KFLD                    apdpdr
427600021204     c                   movel     'A'           apdtip
427700070312     c     Kapd21        Klist
427800070312     c                   Kfld                    apdtip
427900120718     c                   Kfld                    £dstpdr
428000070312     c                   Kfld                    fvvfpp
428100070312     c                   Kfld                    fvvdfv
428200070312     c     Kapd2         Klist
428300070312     c                   Kfld                    apdtip
428400120718     c                   Kfld                    £dstpdr
428500070312     c                   Kfld                    fvvfpp
428600940322     C     KTAB          KLIST
428700020906     C                   KFLD                    tblkut
428800940322     C                   KFLD                    TBLCOD
428900940322     C                   KFLD                    TBLKEY
429000940322     C     KTAB1         KLIST
429100020906     C                   KFLD                    tblkut
429200940322     C                   KFLD                    TBLCOD
429300990920     C     KTBE          KLIST
429400990920     C                   KFLD                    KCOD1
429500990920     C                   KFLD                    KKEY1
429600951211     C     KNUF          KLIST
429700941107     C                   KFLD                    AAA               2 0
429800940322     C                   KFLD                    CNU               3 0
429900951211     C     KNUF0         KLIST
430000951211     C                   KFLD                    AAA0              2 0
430100951211     C                   KFLD                    CNU               3 0
430200951211     C                   Z-ADD     12            CNU
430300941107     C                   MOVE      WHHDAT        AAA
430400951211     C                   Z-ADD     0             AAA0
430500940322     C     KARB1         KLIST
430600940322     C                   KFLD                    ARBAAS
430700940322     C                   KFLD                    ARBLNP
430800940322     C                   KFLD                    ARBNRS
430900940322     C                   KFLD                    ARBNSP
431000960702     C     KARB          KLIST
431100020906     C                   KFLD                    Kfgs
431200020905     C                   KFLD                    KNDC
431300960702     C                   KFLD                    KDDC
431400940322     C     KARB2         KLIST
431500940322     C                   KFLD                    LBLAAN
431600940322     C                   KFLD                    LBLLPN
431700940322     C                   KFLD                    LBLNRN
431800940322     C                   KFLD                    LBLNSN
431900940322     C     KFVV          KLIST
432000940322     C                   KFLD                    FVVNPG
432100940322     C                   KFLD                    FVVNFV
432200020905     C                   KFLD                    FVVFGS
432300950601     C     KDST          KLIST
432400950601     C                   KFLD                    FVVNFV
432500950601     C                   KFLD                    FVVFGS
432600000717     C     KFIBRV        KLIST
432700000717     C                   KFLD                    FVVNPG
432800000717     C                   KFLD                    FVVNF6            6 0
432900030226     C                   KFLD                    FVVfgs
433000980309     C     KEVB          KLIST
433100980309     C                   KFLD                    KAAS
433200980309     C                   KFLD                    KLNP
433300980309     C                   KFLD                    KNRS
433400980309     C                   KFLD                    KNSP
433500980309     C                   KFLD                    KCEV
433600990920     C     KAR6          KLIST
433700990920     C                   KFLD                    KAAS
433800990920     C                   KFLD                    KLNP
433900990920     C                   KFLD                    KNRS
434000990920     C                   KFLD                    KNSP
434100990920     C                   KFLD                    KTRC
434200990920     C     KARI          KLIST
434300990920     C                   KFLD                    KAAS
434400990920     C                   KFLD                    KLNP
434500990920     C                   KFLD                    KNRS
434600990920     C                   KFLD                    KNSP
434700990920     C                   KFLD                    KTIP
434800941229     C*
434900940322     C                   Z-ADD     4             FVVNPG
435000940322     C*
435100940322     C     KLBL          KLIST
435200940322     C                   KFLD                    LBLAAP
435300940322     C                   KFLD                    LBLLPP
435400940322     C                   KFLD                    LBLNRP
435500940322     C                   KFLD                    LBLNSP
435600940322     C*
435700940322     C     KLBL1         KLIST
435800940322     C                   KFLD                    LBLAAN
435900940322     C                   KFLD                    LBLLPN
436000940322     C                   KFLD                    LBLNRN
436100940322     C                   KFLD                    LBLNSN
436200940322     C*
436300940322     C     KANM          KLIST
436400990519     C                   KFLD                    WAA4
436500940322     C                   KFLD                    ARBLNP
436600940322     C                   KFLD                    ARBNRS
436700940322     C                   KFLD                    ARBNSP
436800940322     C                   KFLD                    §§§CAA            3 0
436900940322     C*
437000940322     C     KEYFT2        KLIST
437100940322     C                   KFLD                    FTWPDR            7 0
437200940322     C                   KFLD                    FTWFVL            1
437300941109     C                   KFLD                    FTWDDC            8 0
437400940322     C                   KFLD                    FTWTSR            1
437500950606     C*
437600950606     C     KCLN          KLIST
437700950606     C                   KFLD                    KTFP
437800950606     C                   KFLD                    KTFA
437900950606     C                   KFLD                    KANN
438000950606     C                   KFLD                    KMES
438100010403     C* ACCESSO FNORM06L
438200010403     C     KORM          KLIST
438300010403     C                   KFLD                    FVVFGS
438400010403     C                   KFLD                    FVVNF6
438500031017     C     Kbrv          KLIST
438600031020     C                   KFLD                    kNPG              1 0
438700031017     C                   KFLD                    knf6              6 0
438800031020     C                   KFLD                    kfgs3             3 0
438900031017     c                   KFLD                    artLNP
439000031017     c                   KFLD                    artLNA
439100031017     c                   KFLD                    artNRS
439200031017     c                   kfld                    artNSC
439300031017     C                   Z-ADD     4             kNPG
439400031017     C     Kart          KLIST
439500031017     C                   KFLD                    KAAS
439600031017     C                   KFLD                    KLNP
439700031017     C                   KFLD                    KNRS
439800031017     c                   KFLD                    KNSP
439900040217      *
440000040217     C     Kfiar5        KLIST
440100040217     C                   KFLD                    arbAAS
440200040217     C                   KFLD                    arbLNP
440300040217     C                   KFLD                    arbNRS
440400040217     C                   KFLD                    arbNSP
440500040217     C                   KFLD                    trcar5            3
440600041007     C     kqdt          klist
440700041007     C                   kfld                    wlna              3 0
440800041007     C                   kfld                    wdis              6 0
440900110224     c     Kpct          Klist
441000110225     c                   Kfld                    £dstFGS
441100110225     c                   Kfld                    £dstNFV
441200110225     c                   Kfld                    £dstpdr
441300110324     c                   Kfld                    wtrd              3
441400110324     c                   eval       wtrd  = 'CET'
441500940322     C*
441600000515     C     *LIKE         DEFINE    FVVDS         FVVDS2
441700940322     C     *LIKE         DEFINE    TBLKUT        §KUT
441800940322     C     *LIKE         DEFINE    TBLCOD        §COD
441900940322     C     *LIKE         DEFINE    TBLKEY        §KEY
442000950117     C     *LIKE         DEFINE    VIDANS        SAVAAS
442100950117     C     *LIKE         DEFINE    V03LNP        SAVLNP
442200950117     C     *LIKE         DEFINE    V03NRS        SAVNRS
442300950117     C     *LIKE         DEFINE    V03NSP        SAVNSP
442400950606     C     *LIKE         DEFINE    CLNTFP        KTFP
442500950606     C     *LIKE         DEFINE    CLNTFA        KTFA
442600950606     C     *LIKE         DEFINE    CLNANN        KANN
442700950606     C     *LIKE         DEFINE    CLNMES        KMES
442800020905     C     *LIKE         DEFINE    ARBIFP        KFGS
442900020905     C     *LIKE         DEFINE    ARBNDC        KNDC
443000960702     C     *LIKE         DEFINE    ARBDDC        KDDC
443100980309     C     *LIKE         DEFINE    EVBAAS        KAAS
443200980309     C     *LIKE         DEFINE    EVBLNP        KLNP
443300980309     C     *LIKE         DEFINE    EVBNRS        KNRS
443400980309     C     *LIKE         DEFINE    EVBNSP        KNSP
443500980309     C     *LIKE         DEFINE    EVBCEV        KCEV
443600990920     C     *LIKE         DEFINE    TBECOD        KCOD1
443700990920     C     *LIKE         DEFINE    TBEKE1        KKEY1
443800990920     C     *LIKE         DEFINE    ARITIP        KTIP
443900990920     C     *LIKE         DEFINE    AR6TRC        KTRC
444000950117     C*
444100950123     C                   SETOFF                                       1319
444200940322     C*
444300940322     C                   ENDSR
444400010403** ERR
444500941109Eventuali variazioni andranno perse premendo F12                      01
444600940503Non è possibile annullare distinta in immissione                      02
444700940503Abilitata inclusione o esclusione non entrambi                        03
444800940503Non è posssibile annullare la distinta in immissione                  04
444900940503Stop già utilizzato per un altro cliente                              05
445000940503Linea errata                                                          06
445100940503Data distinta errata                                                  07
445200940503Data distinta errata: già confermati i conteggi                       08
445300940503Codice padroncino inesistente                                         09
445400961115Codice padroncino appartenente a punto operativo non in gestione      10
445500080318Autotrasp. diverso da distinta F1 confermi variazione                 11
445600940503Impossibile gestire distinta attendi 5'' e ritenta                    12
445700940503Distinta errata                                                       13
445800940503Distinta al momento non disponibile: ATTENZIONE !!!                   14
445900961115Numero distinta in gestione ad altra punto operativo                  15
446000000929Data distinta anteriore o superiore alla data odierna dei gg.ammessi  16
446100020906Punto operat. padroncino deve essere = al punto operat. in gestione   17
446200961115Puto operativo di consegna gestita da altro punto operativo           18
446300961217Data distinta non modificabile                                        19
446400961217Numero stop non attribuito correttamente: ci sono più di 999 spediz.  20
446500000717DISTINTA NON ANNULLABILE ESISTONO DELLE SPUNTE SU QUESTO FOGLIO       21
446600010403DISTINTA NON ANNULLABILE: PRESENTI O.R.M.                             22
446700020419Data distinta superiore alla data odierna dei gg.ammessi              23
446800020419Data distinta inferiore alla data odierna inserimento non possibile   24
446900080208Filiale non ancora attiva per gestione PDA                            25
447000110225Scelta incongruente o non ammessa                                     26
447100080318La distinta è in una fase dove non è + possibile modificare il valore 27
447200160502Il turno è ammesso solo per distinte Parcel                           28
447300170224Autista BLOCCATO !!                                                   29
