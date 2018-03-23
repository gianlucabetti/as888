000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200020605      * FNLS40R *----------------------------------------------------*
000300020605      *        - LANCIO PULIZIE VARIE PARTENZE (CALCOLO DATE + CALL) *
000400020605      *--------------------------------------------------------------*
000500001027     FTABEL00F  IF   E           K DISK
000600001027     FAZORG01L  IF   E           K DISK
000700020909     fAzcln01l  if   e           k Disk
000800020605
000900050705     D KPJBA         E DS
001000050705     D OG143         E DS                  inz
001100050705     D DS5A          E DS                  inz
001200050705     D DS5A2         E DS                  inz
001300050705     d Ds1y          e ds                  inz
001400050705      *
001500050705      * - Parametri x Controllo profilo utenti
001600050705     d TIBS34ds      e ds                  inz
001700050705      * - Ds di riferimento al file esterno AZUTE00F
001800050705     d AZUTEds       e ds                  inz extname(AZUTE00F)
001900050705      * - Ds per dati organigramma
002000050705     d dDATIUTE      e ds                  inz
002100050705      *
002200020620      * DATE PER PULIZIE
002300050705     D PARAM1          DS                  inz
002400050705     D  DATPUL                 5     12  0 inz
002500050705     D  DATBRV                13     20  0 inz
002600050705     D  DATBRC                21     28  0 inz
002700050705     D  DATxxx                29     36  0 inz
002800050705     D  DatMSS                37     44  0 inz
002900051007     D  DatSCA                45     52  0 inz
003000061020     D  Datcan                53     60  0 inz
003100020620      *
003200050705     D WLBDAT          DS                  inz
003300050705     D  G02DAT                 1      8  0 inz
003400050705     D  G02INV                 9     16  0 inz
003500050705     D  G02ERR                17     17    inz
003600050705     D  G02TGI                18     22  0 inz
003700050705     D WGIDAT          DS                  inz
003800050705     D  GIODAT                 1      8  0 inz
003900050705     D  GIOINV                 9     16  0 inz
004000050705     D  GIOTGI                17     21  0 inz
004100020909
004200020909     d ClnMat          ds
004300020909     d  Mat                    1     31    Dim(31)
004400020909     d ClnPom          ds
004500020909     d  Pom                    1     31    Dim(31)
004600020909
004700050705     d WPJBU           s                   like(KPJBU ) inz
004800050705     d CODUT           s                   like(TBLkut) inz(1)
004900050705     d COD             s                   like(TBLCOD) inz
005000050705     d KEY             s                   like(TBLKEY) inz
005100050705     d COMBRV          s                   like(§5ABRV) inz
005200050705     d COMBRC          s                   like(§5ABRC) inz
005300050705     d ktfp            s                   like(clntfp) inz
005400050705     d ktfa            s                   like(clntfa) inz
005500050705     d kann            s                   like(clnann) inz
005600050705     d kmes            s                   like(clnmes) inz
005700050705     d Wdata           s               d   Datfmt(*iso) inz
005800050705     d Wgiorno         s              2  0 inz
005900050705     d Wbrv            s              8  0 inz
006000050705     d Wbrc            s              8  0 inz
006100050705     d COMSPP          s              8  0 inz
006200050705     d COMBLD          s              8  0 inz
006300050705     d COMTR1          s              8  0 inz
006400050705     d COMMSC          s              8  0 inz
006500050705     d COMMSS          s              8  0 inz
006600050705     d COMNDP          s              8  0 inz
006700051007     d COMSca          s              8  0 inz
006800061020     d COMcan          s              8  0 inz
006900080121     d COMpsr          s              8  0 inz
007000081114     d comsga          s              8  0 inz
007100050705     C*---------------------------------------------------------------*
007200050705     C* ACCESSO TABEL00F
007300050705     C     KTAB          KLIST
007400050705     C                   KFLD                    CODUT
007500050705     C                   KFLD                    COD
007600050705     C                   KFLD                    KEY
007700050705
007800050705     c     Kazcln        Klist
007900050705     c                   Kfld                    Ktfp
008000050705     c                   Kfld                    Ktfa
008100050705     c                   Kfld                    Kann
008200050705     c                   Kfld                    Kmes
008300050705     C*
008400001027     C*---------------------------------------------------------------*
008500001027     C     *ENTRY        PLIST
008600001027     C                   PARM                    KPJBA
008700050705      *
008800050705     c     *dtaara       define    §azute        AZUTEds
008900050705     c     *dtaara       define    §datiute      dDATIUTE
009000050705     c                   in(E)     *dtaara
009100050705     c                   if        %ERROR or RSUT = *blanks
009200050705     c                   clear                   Tibs34Ds
009300050705     c                   call      'TIBS34R'
009400050705     c                   parm                    Tibs34Ds
009500050705     c                   in        *dtaara
009600050705     c                   endif
009700001027     C*
009800001027     C*---------------------------------------------------------------*
009900050705     ***C* CARICAMENTO TABELLE
010000050705     *** * Non carico piu' la £1 per segnalare esistenza p.o DPD
010100001027     C*
010200050829     C* reperimento data di riferimento per calcolo date di pulizia
010300001027     C*
010400050829     C                   CALL      'FNLV61R'
010500050829     C                   PARM                    O61DRF            8 0
010600050829     C                   z-add     o61drf        G02inv
010700050829     C                   MOVEL     '3'           G02ERR
010800001027     C                   CALL      'XSRDA8'
010900001027     C                   PARM                    WLBDAT
011000001027     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
011100001027     C                   MOVEL     '5A'          COD
011200001027     C                   MOVEL     '1       '    KEY
011300001027     C     KTAB          CHAIN     TABEL                              30
011400001027     C  N30              MOVEL     TBLUNI        DS5A
011500001027     C   30              MOVEL     *ZEROS        DS5A
011600001027     C*
011700001027     C*
011800001027     C* SEGNACOLLI     :  SOTTRAGGO §5ABLD AI GIORNI
011900001027     C     G02TGI        SUB       §5ABLD        GIOTGI
012000001027     C*
012100001027     C                   CALL      'XSRGI8'
012200001027     C                   PARM                    WGIDAT
012300050705     C                   Z-ADD     GIOINV        COMBLD
012400020620      *
012500020620      * STORICO SPUNTE CML/VDL/DISK C:SOTTRAGGO §5ATR1 AI GIORNI
012600001027     C     G02TGI        SUB       §5ATR1        GIOTGI
012700020620      *
012800001027     C                   CALL      'XSRGI8'
012900001027     C                   PARM                    WGIDAT
013000050705     C                   Z-ADD     GIOINV        COMTR1
013100020620      *
013200050705      * LETTURE PACCHI TEST CML/VDL:SOTTRAGGO §5AMSC AI GIORNI
013300050705     C     G02TGI        SUB       §5AMSC        GIOTGI
013400020620      *
013500020620     C                   CALL      'XSRGI8'
013600020620     C                   PARM                    WGIDAT
013700050705     C                   Z-ADD     GIOINV        COMMSC
013800050705      *
013900050705      * Saldi Attività Smistamento: sottraggo §5AMSS ai giorni
014000050705     c     G02tgi        sub       §5AMSS        GIOtgi
014100050705     c                   call      'XSRGI8'
014200050705     c                   parm                    WGIdat
014300050705     c                   z-add     GIOinv        ComMSS
014400001027     C*
014500001027     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
014600021104     C                   CLEAR                   COMNDP
014700021104      * Li reperisco sempre
014800021104     C                   MOVEL     '5A'          COD
014900021104     C                   MOVEL     '2       '    KEY
015000021104     C     KTAB          CHAIN     TABEL                              30
015100021104     C  N30              MOVEL     TBLUNI        DS5A2
015200021104     C   30              MOVEL     *ZEROS        DS5A2
015300001027     C* NUMERATORI DPD : :SOTTRAGGO §5ANDP AI GIORNI
015400021104     C     G02TGI        SUB       §5ANDP        GIOTGI
015500021104     C*
015600021104     C                   CALL      'XSRGI8'
015700021104     C                   PARM                    WGIDAT
015800050705     C                   Z-ADD     GIOINV        COMNDP
015900080118     C* Log spunte da pistole radio sottraggo §5APSR
016000080118     C     G02TGI        SUB       §5APSR        GIOTGI
016100080118     C*
016200080118     C                   CALL      'XSRGI8'
016300080118     C                   PARM                    WGIDAT
016400080118     C                   Z-ADD     GIOINV        COMPSR
016500081114
016600081114      * Giorni pulizia file WFSGA00F stat. giornaliera anomalie
016700081114     C     G02TGI        SUB       §5ASGA        GIOTGI
016800081114     C*
016900081114     C                   CALL      'XSRGI8'
017000081114     C                   PARM                    WGIDAT
017100081114     C                   Z-ADD     GIOINV        COMSGA
017200001027     C**
017300001027     C** --------------
017400001027     C** LANCIO PULIZIE
017500001027     C** --------------
017600001027     C* SPUNTE SEGNACOLLI
017700001027     C     COMBLD        IFNE      0
017800001027     C                   MOVEL     COMBLD        DATPUL
017900001027     C                   MOVEL     PARAM1        KPJBU
018000001027     C                   CALL      'FNLS79R'
018100001027     C                   PARM                    KPJBA
018200001027     C                   END
018300020620      * FLTR700F            - STORICO SPUNTE VDL/DISK C
018400020620     C                   IF        COMTR1 <> *zeros
018500001027     C                   MOVEL     COMTR1        DATPUL
018600001027     C                   MOVEL     PARAM1        KPJBU
018700001027     C                   CALL      'FNLRB2R'
018800001027     C                   PARM                    KPJBA
018900020620     C                   ENDIF
019000020620      * FNMSA00F            - ATTIVITA' SMISTAMENTO
019100020620      * FNMSC00F            - LETTURE PACCHI CIVETTA
019200050705      * FNMSS00F            - SALDI ATTIVITA' SMISTAMENTO
019300020620     C                   IF        COMTR1 <> *zeros  or
019400050705     C                             COMMSC <> *zeros  or
019500050705     C                             ComMSS <> *zeros
019600020620     C                   MOVEL     COMTR1        DATPUL
019700020620     C                   MOVEL     COMMSC        DATxxx
019800050705     c                   movel     ComMSS        DatMSS
019900020620     C                   MOVEL     PARAM1        KPJBU
020000001027     C                   CALL      'FIMS30R'
020100001027     C                   PARM                    KPJBA
020200020620     C                   ENDIF
020300001027     C* BOLLE E GIACENZE    - DATI INVIATI A CLIENTI
020400001027     C                   CLEAR                   KPJBU
020500001027     C                   CALL      'TRTC22R'
020600001027     C                   PARM                    KPJBA
020700001027     C*
020800001027     C* PULIZIA ARCHIVI CONTEGGI PADRONCINI
020900100504     c* 05/2010 ES --> tolta la pulizia e sottoomessa a parte
021000100504     C***                CALL      'FICND2R'
021100100504     C***                PARM                    KPJBA
021200001027     C* PULIZIA NUMERATORI DPD
021300021104     C     COMNDP        IFNE      0
021400001027     C                   MOVEL     COMNDP        DATPUL
021500001027     C                   MOVEL     PARAM1        KPJBU
021600001027     C                   CALL      'FNLS42R'
021700001027     C                   PARM                    KPJBA
021800021104     C                   END
021900010328     C* PULIZIA FILE FNBLD00F
022000010328     C                   CALL      'FNLS47R'
022100001122     C                   PARM                    KPJBA
022200080118     c* Pulizia log spunte pistole radio
022300080118     C     COMPSR        IFNE      0
022400080118     C                   MOVEL     COMPSR        DATPUL
022500080118     C                   MOVEL     PARAM1        KPJBU
022600080118     C                   CALL      'FNLRB1R'
022700080118     C                   PARM                    KPJBA
022800080118     C                   END
022900081114
023000081114      * Pulizia WFSGA00F Statistica giornaliera anomalie
023100081114     C     COMSGA        IFNE      0
023200081114     C                   MOVEL     COMSGA        DATPUL
023300081114     C                   MOVEL     PARAM1        KPJBU
023400081117     C                   CALL      'FNLRE2R'
023500081114     C                   PARM                    KPJBA
023600081114     C                   END
023700081114
023800001027     C*
023900001027     C     FINE          TAG
024000010402     C* L'USO DI WPJBU ERA STATO MESSO PER POTER VISUALIZZARE
024100010402     C* IN INTERROGAZIONE AZIONI ESEGUITI I PARAMETRI DI RICHIAMO
024200010402     C* A PGM FNLR10R MA NON FUNZIONA
024300001123     C                   MOVE      WPJBU         KPJBU
024400001027     C                   SETON                                        LR
