000100960329      ***********************************************************************
000200960924      *   STAMPA TOTALI BOLLE IMPORTATE PER TERMINAL DI ARRIVO              *
000300960329      ***********************************************************************
000400960329     H DECEDIT('0,') DATEDIT(*DMY.)
000500960329     F*---------------------------------------------------------------------*
000600960329     F*  DATA BASE                                                          *
000700960329     F*---------------------------------------------------------------------*
000800091106     FTABEL00F  IF   E           K DISK
000900020919     FEDivab1L  IF   E           K DISK
001000960924     F*---------
001100960925     FCNACO00F  IF   E           K DISK
001200960924     F*---------
001300091106     FAZORG01L  IF   E           K DISK
001400960924     F*---------
001500091127     FTRTC86P   O    E             PRINTER OFLIND(*IN01) USROPN
001600960329     D*---------------------------------------------------------------------*
001700960329     D* Schiere
001800960329     D*---------------------------------------------------------------------*
001900960925     D*  lna - filiali elaboratore
002000960925     D LNA             S              3  0 DIM(999)
002100980113     D TFE             S              3  0 DIM(999)
002200960925     D*  Filiali elaboratore
002300960925     D TFA             S              3  0 DIM(300)                             Cod.Terminal arrivo
002400960925     D DFA             S             20    DIM(300)                             Descrizione
002500960925     D*  Schiere capiconto
002600960329     D*---------------------------------------------------------------------*
002700960329     D* DS
002800960329     D*---------------------------------------------------------------------*
002900960925     D*  Ds per caricamento totali terminal arrivo - CMR in schiera
003000960925     D MULT            DS                  OCCURS(300)
003100960925     D  SPETFA                 1     15  0
003200960925     D  NCLTFA                16     31  0
003300960925     D  PKGTFA                32     48  1
003400960925     D  VMCTFA                49     66  3
003500960925     D*  Ds per caricamento totali generali terminal arrivo
003600960925     D TOTT            DS                  OCCURS(300)
003700960925     D  SPEGEN                 1     15  0
003800960925     D  NCLGEN                16     31  0
003900960925     D  PKGGEN                32     48  1
004000960925     D  VMCGEN                49     66  3
004100020919     D kpjbus          S                   LIKE(kpjbu)
004200091106     D ds3a          E DS
004300091106     D KPJBA         E DS
004400091106     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004500091106     D DSLV13        E DS                  EXTNAME(FNLV13DS)
004600091106     D DSSI95        E DS                  EXTNAME(TISI95DS)
004700091106     d
004800020919     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004900020919     D  TCU                  398    697
005000020919     D                                     DIM(50)                              Flg 8 tp.conto
005100020919     D  KCU                  698    847P 0
005200020919     D                                     DIM(50)                              Capiconto
005300020919     D                                     PACKEVEN
005400020919     D CNCR80        E DS
005500960531     D* Ds scomposzione tipo capoconti
005600960531     D TCUDS           DS
005700960531     D  F34                    3      4
005800960531     D  F56                    5      6
005900960528     D*  DS x salvataggio dati impostati in FNVAB
006000960528     D WLBDA8          DS
006100960528     D  G02DAT                 1      8  0
006200960528     D  G02INV                 9     16  0
006300960528     D  G02ERR                17     17
006400960528     D  G02TGI                18     22  0
006500960925     D TRTC86        E DS                  EXTNAME(TRTC86DS)
006600020919     D TRUL06DS      E DS
006700020919     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
006800960925     D                SDS
006900960925     D  NOMPGM                 1     10
007000960925     D*  Costante per lna a zero
007100960925     D COST1           C                   CONST('DA ATTRIBUIRE       ')
007200960329     C*---------------------------------------------------------------------*
007300960329     C* Ciclo principale
007400960925     C*---------------------------------------------------------------------*
007500091124     C*  Reperisco parametri in entrata
007600091124     C     *ENTRY        PLIST
007700091124     C                   PARM                    KPJBA
007800091124     C                   MOVEL     KPJBU         TRTC86
007900091127     c*
008000020919      *
008100960925     C*  Se ho immesso il numero CMR eseguo calcolo
008200960926     C     D86STA        IFEQ      'S'
008300960925     C*  Azzero schiera CMR
008400960925     C                   EXSR      AZZMUL
008500960925     C*  Posizionamento iniziale
008600960925     C                   EXSR      POSINI
008700960925     C     *IN30         DOWEQ     '0'
008800960925     C                   EXSR      AGGMUL
008900020920      *
009000020920     c                   exsr      readVAB
009100020920      *
009200960925     C                   END
009300960925     C*  Eseguo stampa totali per CMR e aggioprno totali generali
009400960925     C                   EXSR      STPMUL
009500960925     C                   END
009600960926     C*
009700960926     C*  Controllo se devo stampare totali
009800960926     C     D86RIE        IFEQ      'S'
009900091127     c                   if        %open(TRTC86P)
010000091127     c                   close     TRTC86P
010100091127     c                   open      TRTC86P
010200091127     c                   seton                                        01
010300091127     c                   endif
010400091127     c
010500960926     C                   EXSR      STPTOT
010600960926     C                   END
010700091127
010800960925     C*  Controllo come devo chiudere il pgm
010900960925     C     D86CHI        IFEQ      'RT'
011000960925     C*  Se devo chiudere con RT accendo indicatore
011100960925     C                   SETON                                        RT
011200960925     C                   ELSE
011300980113     C* Imposto i dati in LV55
011400091106     C                   CLEAR                   dslv55
011500091106     C                   MOVEL     'C'           D55TLA
011600980113     C                   CALL      'FNLV55R'
011700091106     C                   PARM                    dslv55
011800091106     c
011900091106     C                   MOVEL     'C'           I13TLA
012000091106     C                   CALL      'FNLV13R'
012100091106     C                   PARM                    KPJBA
012200091106     C                   PARM                    DSLV13
012300091106     C                   PARM                    DSSI95
012400091106     c
012500960926     C*  Se devo chiudere con LR accendo indicatore
012600960925     C                   SETON                                        LR
012700960925     C                   END
012800960925     C*---------------------------------------------------------------------*
012900980113     C* CARTFE: Eseguo caricamento terminal di arrivo
013000960925     C*---------------------------------------------------------------------*
013100980113     C     CARTFE        BEGSR
013200960925     C*
013300960926     C                   Z-ADD     1             X
013400960926     C                   Z-ADD     1             Y                 3 0
013500960926     C                   Z-ADD     0             LNA
013600980113     C                   Z-ADD     0             TFE
013700960925     C*  Carico filiale 0
013800960925     C                   Z-ADD     0             TFA(1)
013900960925     C                   MOVEL     COST1         DFA(1)
014000960925     C*  Carico altri terminal
014100960925     C                   READ      AZORG01L                               30
014200960925     C     *IN30         DOWEQ     '0'
014300960925     C     ORGFVA        IFEQ      *BLANKS
014400960925     C                   ADD       1             X
014500960925     C                   Z-ADD     ORGFIL        LNA(X)
014600980113      *  Reperisco terminal di partenza collegato
014700091106     C                   CLEAR                   dsLV55
014800980113     C* Imposto i dati in LV55
014900980113     C                   MOVEL     'A'           D55TPT
015000980113     C                   MOVEL     WOGGI         D55DRF
015100980113     C                   MOVEL     ORGFIL        D55LIN
015200980113     C                   CALL      'FNLV55R'
015300091106     C                   PARM                    DSLV55
015400980113     C                   MOVE      D55TFA        TFE(X)
015500960926     C*  Se primo livello carico anche TFA DFA
015600980113     C     ORGFIL        IFEQ      D55TFA
015700960926     C                   ADD       1             Y
015800980113     C                   Z-ADD     D55TFA        TFA(Y)
015900960926     C                   MOVEL     ORGDES        DFA(Y)
016000960925     C                   END
016100960926     C                   END
016200960926     C                   READ      AZORG01L                               30
016300960926     C                   END
016400960925     C*
016500960925     C                   ENDSR
016600960925     C*---------------------------------------------------------------------*
016700960925     C* AZZMUL: Azzeramento totali x terminal di arrivo CMR
016800960925     C*---------------------------------------------------------------------*
016900960925     C     AZZMUL        BEGSR
017000960925     C*
017100960925     C                   DO        300           X                 3 0
017200960925     C     X             OCCUR     MULT
017300960925     C                   CLEAR                   MULT
017400960925     C                   END
017500960925     C*
017600960926     C                   Z-ADD     1             W                 3 0
017700960926     C                   Z-ADD     1             X                 3 0
017800960926     C                   SETON                                        32
017900091127     c*
018000091127     c* salto pagina ad ogni nuovo   CMR stampato
018100091127     C                   SETON                                        01
018200960925     C*
018300960925     C                   ENDSR
018400960925     C*---------------------------------------------------------------------*
018500960925     C* AZZTOT: Azzeramento totali generali x terminal di arrivo
018600960925     C*---------------------------------------------------------------------*
018700960925     C     AZZTOT        BEGSR
018800960925     C*
018900960925     C                   DO        300           X                 3 0
019000960925     C     X             OCCUR     TOTT
019100960925     C                   CLEAR                   TOTT
019200960925     C                   END
019300960925     C*
019400960925     C                   Z-ADD     0             WTOCMR            6 0
019500960925     C*
019600960925     C                   ENDSR
019700960925     C*---------------------------------------------------------------------*
019800960925     C* POSINI: Posizionamento iniziale
019900960925     C*---------------------------------------------------------------------*
020000960925     C     POSINI        BEGSR
020100960925     C*
020200960925     C* Aggiorno totale CMR esaminati
020300960925     C                   ADD       1             WTOCMR
020400960925     C* Posizionamento iniziale
020500960925     C                   Z-ADD     D86CCM        KCCM
020600960925     C                   Z-ADD     D86CNT        KCNT
020700960925     C                   MOVEL     D86CMR        KCMR
020800020920     C     KVAB          setll     EDivab1L
020900020920      *
021000020920     c                   exsr      readVAB
021100020920      *
021200960925     C* Decodifico cliente
021300960925     C     *IN30         IFEQ      '0'
021400960925     C                   Z-ADD     1             KKUT
021500960925     C                   Z-ADD     KCI           KKCC
021600960925     C                   Z-ADD     VABCCM        KKSC
021700960925     C                   MOVEL     *BLANKS       ACORAG
021800960925     C     KACO          CHAIN     CNACO00F                           31
021900960925     C                   MOVEL     ACORAG        DECCLI
022000960925     C                   Z-ADD     VABCCM        CODCLI
022100960925     C*  Metto in stampa dati CMR
022200960925     C                   MOVEL     VABCMR        NUMCMR
022300960925     C                   Z-ADD     VABDCM        G02INV
022400960925     C                   MOVEL     '3'           G02ERR
022500960925     C                   Z-ADD     0             G02DAT
022600960925     C                   CALL      'XSRDA8'
022700960925     C                   PARM                    WLBDA8
022800960925     C                   Z-ADD     G02DAT        DATCMR
022900960925     C                   Z-ADD     VABDTS        G02INV
023000960925     C                   MOVEL     '3'           G02ERR
023100960925     C                   Z-ADD     0             G02DAT
023200960925     C                   CALL      'XSRDA8'
023300960925     C                   PARM                    WLBDA8
023400960925     C                   Z-ADD     G02DAT        DATMSG
023500960925     C                   Z-ADD     VABHMS        ORAMSG
023600960925     C                   END
023700960925     C*
023800960925     C                   ENDSR
023900020920     C*---------------------------------------------------------------*
024000020920     C*  READVAB: Legge il record di EDIVAB
024100020920     C*---------------------------------------------------------------*
024200020920     C     ReadVAB       BEGSR
024300020920     C*
024400020920     c     rileggi       tag
024500020920     C     KVAB          reade     EDivab1L                               30
024600020920     C*
024700020920     C*  controlla se non è compresa fra le linee gestite dal terminal
024800020920     C*   quindi legge nuovo record.
024900020920     c                   if        not %eof(Edivab1L)
025000020920     c     vabfgs        cabeq     0             rileggi
025100020920     c     vabfgs        lookup    lin                                    32
025200020920     c     *in32         cabeq     *off          rileggi
025300020920     c                   end
025400020920     C*
025500020920     C                   ENDSR
025600960925     C*---------------------------------------------------------------------*
025700960925     C* AGGMUL: Aggiorno totali
025800960925     C*---------------------------------------------------------------------*
025900960925     C     AGGMUL        BEGSR
026000091106     c* sse la linea di arrivo è vuota la determino
026100091106     c                   if        vablna=0
026200091106     c                   exsr      determLNA
026300091106     c                   endif
026400960925     C*
026500960925     C* Reperisco la filiale elaboratore della lna
026600091106     c
026700960926     C     VABLNA        IFNE      LNA(X)
026800960926     C     VABLNA        ANDNE     TFA(W)
026900960926     C                   Z-ADD     1             X
027000960926     C     VABLNA        LOOKUP    LNA(X)                                 32
027100960926     C     *IN32         IFEQ      '1'
027200960926     C                   Z-ADD     1             W                 3 0
027300980113     C     TFE(X)        LOOKUP    TFA(W)                                 32
027400960926     C                   END
027500091106     C                   END
027600091106     c
027700960925     C* Aggiorno totali
027800960926     C     *IN32         IFEQ      '1'
027900960926     C     W             OCCUR     MULT
028000960925     C                   ADD       1             SPETFA                         tot.sped.
028100960925     C                   ADD       VABNCL        NCLTFA                         tot.colli
028200960925     C                   ADD       VABPKB        PKGTFA                         tot.peso
028300960925     C                   ADD       VABVLB        VMCTFA                         tot.volu.
028400960926     C                   END
028500960925     C*
028600960925     C                   ENDSR
028700091106     C*---------------------------------------------------------------------*
028800091106     C* Determino la linea di arrivo richiamando il pgm fnlv13r
028900091106     C*---------------------------------------------------------------------*
029000091106     c     DetermLNA     BEGSR
029100091106     C                   CLEAR                   DSSI95
029200091106     C                   CLEAR                   DSLV13
029300091106     C                   MOVEL     vabNZD        I95NAR
029400091106     C                   MOVEL     vabCAD        I95CAP
029500091106     C                   MOVEL     vabLOD        I95LOC
029600091106     C                   MOVEL     vabPRD        I95PRV
029700091106     C                   IF        VABFFD='S'
029800091106     C                   ENDIF
029900091106     C                   Z-ADD     VABPKB        I95LKG
030000091106     C                   Z-ADD     VABVLB        I95LMC
030100091106     C* Determino il terminal di partenza
030200091106     C                   CLEAR                   DSLV55
030300091106     C                   MOVEL     'P'           D55TPT
030400091106     C                   MOVEL     vablnp        D55LIN
030500091106     C                   MOVEL     woggi         D55DRF
030600091106     C                   CALL      'FNLV55R'
030700091106     C                   PARM                    DSLV55
030800091106     C                   Z-ADD     d55tfp        I95TFP
030900091106     C*
031000091106     C     vabTSP        IFNE      *BLANKS
031100091106    1C                   MOVEL     vabTSP        I95TSP
031200091106     C                   ENDIF
031300091106     c* Se c'e' il contrassegno
031400091106     C                   IF        vabcas>0
031500091106     C                   MOVEL     'S'           I95FCA
031600091106     C                   ENDIF
031700091106     c
031800091106     c* Tipo bolla
031900091106     C                   MOVEL     '3A'          COD
032000091106     C                   MOVEL(P)  vabcbo        KEY
032100091106     C     KTAB2         CHAIN     TABEL                              30
032200091106     c                   if        %found(tabel00f)
032300091106     c                   movel     tbluni        ds3a
032400091106     C                   MOVEL     §3atb1        I95TPO
032500091106     c                   else
032600091106     c                   movel     'F'           I95tpo
032700091106     c                   endif
032800091106     c
032900091106     C                   MOVEL     '7'           I95TCN
033000091106     C                   MOVEL     woggi         I95DAT
033100091106     C                   MOVEL     'S'           I95FRE
033200091106     c*
033300091106     c                   movel     vabtc1        i95fi2
033400091106     c                   move      vabtc2        i95fi2
033500091106     C*
033600091106     C                   CALL      'FNLV13R'
033700091106     C                   PARM                    KPJBA
033800091106     C                   PARM                    DSLV13
033900091106     C                   PARM                    DSSI95
034000091106     c*
034100091106     c                   movel     o95lna        vablna
034200091106     c*
034300091106     C                   ENDSR
034400960925     C*---------------------------------------------------------------------*
034500960925     C* STPMUL: Stampa totali x CMR-terminal di arrivo
034600960925     C*---------------------------------------------------------------------*
034700960925     C     STPMUL        BEGSR
034800960925     C*
034900960925     C*  Azzero totali stampa relativi all'intero CMR
035000960925     C                   Z-ADD     0             CMRSPE
035100960925     C                   Z-ADD     0             CMRNCL
035200960925     C                   Z-ADD     0             CMRPKG
035300960925     C                   Z-ADD     0             CMRVMC
035400960925     C*  A inizio CMR stampo relativa intestazione
035500960925     C     *IN01         IFEQ      '1'
035600960925     C                   WRITE     TESTA
035700960925     C                   WRITE     TESCMR
035800960925     C                   WRITE     TESTAB
035900960925     C                   SETOFF                                       01
036000960925     C                   ELSE
036100960925     C                   WRITE     TESCMR
036200960925     C                   END
036300960925     C*  Scorro schiera per vedere totali terminal filiale di arrivo
036400960925     C*  da stampare
036500960925     C                   DO        Y             X
036600960925     C*  Sposto totali da schiera-Ds multipla a stampa
036700960925     C     X             OCCUR     MULT
036800960926     C     SPETFA        IFGT      0
036900960925     C                   Z-ADD     TFA(X)        CODLNA
037000960925     C                   MOVEL     DFA(X)        DECLNA
037100960925     C                   Z-ADD     SPETFA        TOTSPE
037200960925     C                   Z-ADD     NCLTFA        TOTNCL
037300960925     C                   Z-ADD     PKGTFA        TOTPKG
037400960925     C                   Z-ADD     VMCTFA        TOTVMC
037500960926     C*  Se acceso indicatore di overflow stampo testata
037600960926     C     *IN01         IFEQ      '1'
037700960926     C                   WRITE     TESTA
037800960926     C                   WRITE     TESCMR
037900960926     C                   WRITE     TESTAB
038000960926     C                   SETOFF                                       01
038100960926     C                   END
038200960925     C                   WRITE     DETFLA
038300960925     C*  Incremento totali per CMR
038400960925     C                   ADD       SPETFA        CMRSPE
038500960925     C                   ADD       NCLTFA        CMRNCL
038600960925     C                   ADD       PKGTFA        CMRPKG
038700960925     C                   ADD       VMCTFA        CMRVMC
038800960925     C*  Aggiorno totali generali schiera relativa
038900960925     C     X             OCCUR     TOTT
039000960925     C                   ADD       SPETFA        SPEGEN
039100960925     C                   ADD       NCLTFA        NCLGEN
039200960925     C                   ADD       PKGTFA        PKGGEN
039300960925     C                   ADD       VMCTFA        VMCGEN
039400960926     C                   END
039500960926     C                   END
039600960925     C*  Stampo totale per CMR
039700960925     C     *IN01         IFEQ      '1'
039800960925     C                   WRITE     TESTA
039900960925     C                   SETOFF                                       01
040000960925     C                   END
040100960925     C                   WRITE     TOTCMR
040200960925     C*
040300960925     C                   ENDSR
040400960925     C*---------------------------------------------------------------------*
040500960925     C* STPTOT: Stampa totali x generali per terminal di arrivo
040600960925     C*---------------------------------------------------------------------*
040700960925     C     STPTOT        BEGSR
040800960925     C*
040900960925     C*  Azzero totali stampa relativi all'intero CMR
041000960925     C                   Z-ADD     0             GENSPE
041100960925     C                   Z-ADD     0             GENNCL
041200960925     C                   Z-ADD     0             GENPKG
041300960925     C                   Z-ADD     0             GENVMC
041400960925     C*  A inizio CMR stampo relativa intestazione
041500960925     C                   WRITE     TESTA
041600960925     C                   SETOFF                                       01
041700960925     C                   WRITE     TESRIE
041800960925     C                   WRITE     TESTAB
041900960925     C*  Scorro schiera per vedere totali terminal filiale di arrivo
042000960925     C*  da stampare
042100960926     C                   DO        Y             X
042200960925     C*  Scarico totali generali da schiera-Ds multipla a stampa
042300960926     C     X             OCCUR     TOTT
042400960926     C     SPEGEN        IFGT      0
042500960925     C                   Z-ADD     TFA(X)        CODLNA
042600960925     C                   MOVEL     DFA(X)        DECLNA
042700960925     C                   Z-ADD     SPEGEN        TOTSPE
042800960925     C                   Z-ADD     NCLGEN        TOTNCL
042900960925     C                   Z-ADD     PKGGEN        TOTPKG
043000960925     C                   Z-ADD     VMCGEN        TOTVMC
043100960926     C*  Se acceso indicatore di overflow stampo testata
043200960926     C     *IN01         IFEQ      '1'
043300960926     C                   WRITE     TESTA
043400960926     C                   WRITE     TESTAB
043500960926     C                   SETOFF                                       01
043600960926     C                   END
043700960925     C                   WRITE     DETFLA
043800960925     C*  Incremento totali generali
043900960925     C                   ADD       SPEGEN        GENSPE
044000960925     C                   ADD       NCLGEN        GENNCL
044100960925     C                   ADD       PKGGEN        GENPKG
044200960925     C                   ADD       VMCGEN        GENVMC
044300960926     C                   END
044400960925     C                   END
044500960925     C*  Stampo totale generale
044600960925     C     *IN01         IFEQ      '1'
044700960925     C                   WRITE     TESTA
044800960925     C                   SETOFF                                       01
044900960925     C                   END
045000960925     C                   WRITE     TOTGEN
045100960925     C*
045200960925     C                   ENDSR
045300960925     C*---------------------------------------------------------------------*
045400960925     C* *INZSR: Operazioni iniziali
045500960925     C*---------------------------------------------------------------------*
045600960925     C     *INZSR        BEGSR
045700960925     C*
045800020919     C*  Richiamo XPARUT
045900020919     C                   Z-ADD     1             CODUT
046000020919     C                   CALL      'X§PARUT'
046100020919     C                   PARM                    UT§DSE
046200020919     C                   MOVEL     REC80         CNCR80
046300020919      *
046400020919     C                   MOVEL     kpjbu         KPJBUs
046500020919      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
046600020919     C                   CLEAR                   TRUL06DS
046700020919     C                   MOVE      '£1'          D06COD
046800020919     C                   MOVEL     SIMFEL        D06KEY
046900020919     C                   MOVEL     'L'           D06TLA
047000020919     C                   MOVEL     TRUL06DS      KPJBU
047100020919      *
047200020919     C                   CALL      'TRUL06R'
047300020919     C                   PARM                    KPJBA
047400020919     C                   MOVEL     KPJBU         TRUL06DS
047500020919     C                   MOVEL     kpjbus        KPJBU
047600020919      *
047700960925     C                   MOVEL     RAGUT         RSUT             20
047800960925     C* Ricerca capoconti
047900960925     C                   DO        50            X
048000960925     C                   MOVE      TCU(X)        TCUDS
048100960925     C     F56           IFEQ      'CG'
048200960925     C     F34           ANDEQ     '01'
048300960925     C                   Z-ADD     KCU(X)        KCI               4 0
048400960925     C                   END
048500960925     C                   END
048600960925     C* Chiave di accesso
048700960925     C     KVAB          KLIST
048800960925     C                   KFLD                    KCCM
048900960925     C                   KFLD                    KCMR
049000960925     C                   KFLD                    KCNT
049100960925     C     KACO          KLIST
049200960925     C                   KFLD                    KKUT
049300960925     C                   KFLD                    KKCC
049400960925     C                   KFLD                    KKSC
049500091106     C     KTAB2         KLIST
049600091106     C                   KFLD                    CODUT
049700091106     C                   KFLD                    COD               2
049800091106     C                   KFLD                    KEY               8
049900960925     C* Definizione variabili
050000960925     C     *LIKE         DEFINE    VABCCM        KCCM
050100960925     C     *LIKE         DEFINE    VABCMR        KCMR
050200960925     C     *LIKE         DEFINE    VABCNT        KCNT
050300960925     C     *LIKE         DEFINE    ACOKUT        KKUT
050400960925     C     *LIKE         DEFINE    ACOKCC        KKCC
050500960925     C     *LIKE         DEFINE    ACOKSC        KKSC
050600960925     C* Eseguo azzeramento schiera totali
050700960925     C                   EXSR      AZZTOT
050800980113      *  Data del giorno
050900980113     C                   TIME                    WAMGDT           14 0
051000980113     C                   MOVE      WAMGDT        G02DAT
051100980113     C                   MOVE      *ZEROS        G02INV
051200980113     C                   MOVEL     *BLANK        G02ERR
051300980113     C                   CALL      'XSRDA8'
051400980113     C                   PARM                    WLBDA8
051500980113     C                   Z-ADD     G02INV        WOGGI             8 0
051600960925     C* Eseguo caricamento terminal di arrivo
051700980113     C                   EXSR      CARTFE
051800960925     C* Inizializzo variabili
051900960925     C                   MOVEL     *ALL'-'       CMP132
052000960925     C                   MOVEL     *ALL'-'       CM1132
052100980113     C* Accendo indicatore overflow per prima stampa
052200980113     C                   SETON                                        01
052300091127     c                   open      TRTC86P
052400960925     C*
052500960925     C                   ENDSR
