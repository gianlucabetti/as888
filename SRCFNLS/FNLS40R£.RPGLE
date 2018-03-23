000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200020605      * FNLS40R *----------------------------------------------------*
000300020605      *        - LANCIO PULIZIE VARIE PARTENZE (CALCOLO DATE + CALL) *
000400020605      *--------------------------------------------------------------*
000500001027     FTABEL00F  IF   E           K DISK
000600001027     FAZORG01L  IF   E           K DISK
000700020605
000800100504     D CMDDSP          S             70    DIM(1) CTDATA PERRCD(1)
000900100505     D COMsnd          S             70    DIM(1) CTDATA PERRCD(1)
001000050705     D KPJBA         E DS
001100050705     D DS5A          E DS                  inz
001200050705     D DS5A2         E DS                  inz
001300050705      *
001400050705      * - Parametri x Controllo profilo utenti
001500050705     d TIBS34ds      e ds                  inz
001600050705      * - Ds di riferimento al file esterno AZUTE00F
001700050705     d AZUTEds       e ds                  inz extname(AZUTE00F)
001800050705      * - Ds per dati organigramma
001900050705     d dDATIUTE      e ds                  inz
002000050705      *
002100020620      * DATE PER PULIZIE
002200050705     D PARAM1          DS                  inz
002300050705     D  DATPUL                 5     12  0 inz
002400050705     D  DATxxx                29     36  0 inz
002500050705     D  DatMSS                37     44  0 inz
002600020620      *
002700050705     D WLBDAT          DS                  inz
002800050705     D  G02DAT                 1      8  0 inz
002900050705     D  G02INV                 9     16  0 inz
003000050705     D  G02ERR                17     17    inz
003100050705     D  G02TGI                18     22  0 inz
003200050705     D WGIDAT          DS                  inz
003300050705     D  GIODAT                 1      8  0 inz
003400050705     D  GIOINV                 9     16  0 inz
003500050705     D  GIOTGI                17     21  0 inz
003600020909
003700050705     d WPJBU           s                   like(KPJBU ) inz
003800050705     d CODUT           s                   like(TBLkut) inz(1)
003900050705     d COD             s                   like(TBLCOD) inz
004000050705     d KEY             s                   like(TBLKEY) inz
004100050705     d COMBLD          s              8  0 inz
004200050705     d COMTR1          s              8  0 inz
004300050705     d COMMSC          s              8  0 inz
004400050705     d COMMSS          s              8  0 inz
004500050705     d COMNDP          s              8  0 inz
004600080121     d COMpsr          s              8  0 inz
004700081114     d comsga          s              8  0 inz
004800050705     C*---------------------------------------------------------------*
004900050705     C* ACCESSO TABEL00F
005000050705     C     KTAB          KLIST
005100050705     C                   KFLD                    CODUT
005200050705     C                   KFLD                    COD
005300050705     C                   KFLD                    KEY
005400001027     C*---------------------------------------------------------------*
005500001027     C     *ENTRY        PLIST
005600001027     C                   PARM                    KPJBA
005700050705      *
005800050705     c     *dtaara       define    §azute        AZUTEds
005900050705     c     *dtaara       define    §datiute      dDATIUTE
006000050705     c                   in(E)     *dtaara
006100050705     c                   if        %ERROR or RSUT = *blanks
006200050705     c                   clear                   Tibs34Ds
006300050705     c                   call      'TIBS34R'
006400050705     c                   parm                    Tibs34Ds
006500050705     c                   in        *dtaara
006600050705     c                   endif
006700001027     C*
006800001027     C*---------------------------------------------------------------*
006900050705     ***C* CARICAMENTO TABELLE
007000050705     *** * Non carico piu' la £1 per segnalare esistenza p.o DPD
007100001027     C*
007200050829     C* reperimento data di riferimento per calcolo date di pulizia
007300001027     C*
007400050829     C                   CALL      'FNLV61R'
007500050829     C                   PARM                    O61DRF            8 0
007600050829     C                   z-add     o61drf        G02inv
007700050829     C                   MOVEL     '3'           G02ERR
007800001027     C                   CALL      'XSRDA8'
007900001027     C                   PARM                    WLBDAT
008000001027     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
008100001027     C                   MOVEL     '5A'          COD
008200001027     C                   MOVEL     '1       '    KEY
008300001027     C     KTAB          CHAIN     TABEL                              30
008400001027     C  N30              MOVEL     TBLUNI        DS5A
008500001027     C   30              MOVEL     *ZEROS        DS5A
008600001027     C*
008700001027     C* SEGNACOLLI     :  SOTTRAGGO §5ABLD AI GIORNI
008800001027     C     G02TGI        SUB       §5ABLD        GIOTGI
008900001027     C*
009000001027     C                   CALL      'XSRGI8'
009100001027     C                   PARM                    WGIDAT
009200050705     C                   Z-ADD     GIOINV        COMBLD
009300020620      *
009400020620      * STORICO SPUNTE CML/VDL/DISK C:SOTTRAGGO §5ATR1 AI GIORNI
009500001027     C     G02TGI        SUB       §5ATR1        GIOTGI
009600020620      *
009700001027     C                   CALL      'XSRGI8'
009800001027     C                   PARM                    WGIDAT
009900050705     C                   Z-ADD     GIOINV        COMTR1
010000020620      *
010100050705      * LETTURE PACCHI TEST CML/VDL:SOTTRAGGO §5AMSC AI GIORNI
010200050705     C     G02TGI        SUB       §5AMSC        GIOTGI
010300020620      *
010400020620     C                   CALL      'XSRGI8'
010500020620     C                   PARM                    WGIDAT
010600050705     C                   Z-ADD     GIOINV        COMMSC
010700050705      *
010800050705      * Saldi Attività Smistamento: sottraggo §5AMSS ai giorni
010900050705     c     G02tgi        sub       §5AMSS        GIOtgi
011000050705     c                   call      'XSRGI8'
011100050705     c                   parm                    WGIdat
011200050705     c                   z-add     GIOinv        ComMSS
011300051007      *
011400001027     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
011500021104     C                   CLEAR                   COMNDP
011600021104      * Li reperisco sempre
011700021104     C                   MOVEL     '5A'          COD
011800021104     C                   MOVEL     '2       '    KEY
011900021104     C     KTAB          CHAIN     TABEL                              30
012000021104     C  N30              MOVEL     TBLUNI        DS5A2
012100021104     C   30              MOVEL     *ZEROS        DS5A2
012200001027     C* NUMERATORI DPD : :SOTTRAGGO §5ANDP AI GIORNI
012300021104     C     G02TGI        SUB       §5ANDP        GIOTGI
012400021104     C*
012500021104     C                   CALL      'XSRGI8'
012600021104     C                   PARM                    WGIDAT
012700050705     C                   Z-ADD     GIOINV        COMNDP
012800080118     C* Log spunte da pistole radio sottraggo §5APSR
012900080118     C     G02TGI        SUB       §5APSR        GIOTGI
013000080118     C*
013100080118     C                   CALL      'XSRGI8'
013200080118     C                   PARM                    WGIDAT
013300080118     C                   Z-ADD     GIOINV        COMPSR
013400081114
013500081114      * Giorni pulizia file WFSGA00F stat. giornaliera anomalie
013600081114     C     G02TGI        SUB       §5ASGA        GIOTGI
013700081114     C*
013800081114     C                   CALL      'XSRGI8'
013900081114     C                   PARM                    WGIDAT
014000081114     C                   Z-ADD     GIOINV        COMSGA
014100001027     C**
014200001027     C* SPUNTE SEGNACOLLI
014300001027     C     COMBLD        IFNE      0
014400100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS79R   '
014500100505     C                   EXSR      INVIA
014600100505     c
014700001027     C                   MOVEL     COMBLD        DATPUL
014800001027     C                   MOVEL     PARAM1        KPJBU
014900001027     C                   CALL      'FNLS79R'
015000001027     C                   PARM                    KPJBA
015100100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS79R   '
015200100505     C                   EXSR      INVIA
015300100505     c
015400001027     C                   END
015500020620      * FLTR700F            - STORICO SPUNTE VDL/DISK C
015600020620     C                   IF        COMTR1 <> *zeros
015700100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRB2R   '
015800100505     C                   EXSR      INVIA
015900100505     c
016000001027     C                   MOVEL     COMTR1        DATPUL
016100001027     C                   MOVEL     PARAM1        KPJBU
016200001027     C                   CALL      'FNLRB2R'
016300001027     C                   PARM                    KPJBA
016400100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRB2R   '
016500100505     C                   EXSR      INVIA
016600100505     c
016700020620     C                   ENDIF
016800020620      * FNMSA00F            - ATTIVITA' SMISTAMENTO
016900020620      * FNMSC00F            - LETTURE PACCHI CIVETTA
017000050705      * FNMSS00F            - SALDI ATTIVITA' SMISTAMENTO
017100020620     C                   IF        COMTR1 <> *zeros  or
017200050705     C                             COMMSC <> *zeros  or
017300050705     C                             ComMSS <> *zeros
017400100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FIMS30R   '
017500100505     C                   EXSR      INVIA
017600100505     c
017700020620     C                   MOVEL     COMTR1        DATPUL
017800020620     C                   MOVEL     COMMSC        DATxxx
017900050705     c                   movel     ComMSS        DatMSS
018000020620     C                   MOVEL     PARAM1        KPJBU
018100001027     C                   CALL      'FIMS30R'
018200001027     C                   PARM                    KPJBA
018300100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FIMS30R   '
018400100505     C                   EXSR      INVIA
018500100505     c
018600020620     C                   ENDIF
018700100505
018800001027     C* BOLLE E GIACENZE    - DATI INVIATI A CLIENTI
018900100505     c                   eval      %subst(COMSND(1):13:17)='Inizio TRTC22R   '
019000100505     C                   EXSR      INVIA
019100100505     c
019200001027     C                   CLEAR                   KPJBU
019300001027     C                   CALL      'TRTC22R'
019400001027     C                   PARM                    KPJBA
019500100505     c                   eval      %subst(COMSND(1):13:17)='Fine   TRTC22R   '
019600100505     C                   EXSR      INVIA
019700001027     C*
019800001027     C* PULIZIA NUMERATORI DPD
019900021104     C     COMNDP        IFNE      0
020000100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS42R   '
020100100505     C                   EXSR      INVIA
020200100505     c
020300001027     C                   MOVEL     COMNDP        DATPUL
020400001027     C                   MOVEL     PARAM1        KPJBU
020500001027     C                   CALL      'FNLS42R'
020600001027     C                   PARM                    KPJBA
020700100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS42R   '
020800100505     C                   EXSR      INVIA
020900100505     c
021000021104     C                   END
021100010328     C* PULIZIA FILE FNBLD00F
021200100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS47R   '
021300100505     C                   EXSR      INVIA
021400100505     c
021500010328     C                   CALL      'FNLS47R'
021600001122     C                   PARM                    KPJBA
021700100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS47R   '
021800100505     C                   EXSR      INVIA
021900100505     c
022000080118     c* Pulizia log spunte pistole radio
022100080118     C     COMPSR        IFNE      0
022200100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRB1R   '
022300100505     C                   EXSR      INVIA
022400100505     c
022500080118     C                   MOVEL     COMPSR        DATPUL
022600080118     C                   MOVEL     PARAM1        KPJBU
022700080118     C                   CALL      'FNLRB1R'
022800080118     C                   PARM                    KPJBA
022900100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRB1R   '
023000100505     C                   EXSR      INVIA
023100100505     c
023200080118     C                   END
023300081114
023400081114      * Pulizia WFSGA00F Statistica giornaliera anomalie
023500081114     C     COMSGA        IFNE      0
023600100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRE2R   '
023700100505     C                   EXSR      INVIA
023800100505     c
023900081114     C                   MOVEL     COMSGA        DATPUL
024000081114     C                   MOVEL     PARAM1        KPJBU
024100081117     C                   CALL      'FNLRE2R'
024200081114     C                   PARM                    KPJBA
024300100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRE2R   '
024400100505     C                   EXSR      INVIA
024500100505     c
024600081114     C                   END
024700081114
024800001027     C*
024900010402     C* L'USO DI WPJBU ERA STATO MESSO PER POTER VISUALIZZARE
025000010402     C* IN INTERROGAZIONE AZIONI ESEGUITI I PARAMETRI DI RICHIAMO
025100010402     C* A PGM FNLR10R MA NON FUNZIONA
025200001123     C                   MOVE      WPJBU         KPJBU
025300100504     C* FACCIO DSPJOBLOG
025400100504     C                   Z-ADD     70            LENGHT
025500100504     C                   MOVEL     CMDDSP(1)     COMMAN           80
025600100504     c                   Call      'QCMDEXC'                            99
025700100504     c                   Parm                    comman
025800100504     c                   Parm                    lenght           15 5
025900001027     C                   SETON                                        LR
026000100505     c*-------------------------------------------------------------------
026100100505     C     INVIA         BEGSR
026200100505     C                   Z-ADD     70            LENGHT
026300100505     C                   MOVEL     COMsnd(1)     COMMAN           80
026400100505     c                   Call      'QCMDEXC'                            99
026500100505     c                   Parm                    comman
026600100505     c                   Parm                    lenght           15 5
026700100505     C                   ENDSR
026800100504**
026900100504DSPJOBLOG OUTPUT(*PRINT)
027000100505**
027100100505SNDMSG MSG('inizio xxxxxxxxxx') TOUSR(*REQUESTER)
