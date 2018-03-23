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
001300170210     d ds5A4         e ds                  inz
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
002500050705     D  DATxxx                29     36  0 inz
002600050705     D  DatMSS                37     44  0 inz
002700170210     d  DatMSP0               45     52  0 inz
002800170210     d  DatMSP1               53     60  0 inz
002900020620      *
003000050705     D WLBDAT          DS                  inz
003100050705     D  G02DAT                 1      8  0 inz
003200050705     D  G02INV                 9     16  0 inz
003300050705     D  G02ERR                17     17    inz
003400050705     D  G02TGI                18     22  0 inz
003500050705     D WGIDAT          DS                  inz
003600050705     D  GIODAT                 1      8  0 inz
003700050705     D  GIOINV                 9     16  0 inz
003800050705     D  GIOTGI                17     21  0 inz
003900020909
004000050705     d WPJBU           s                   like(KPJBU ) inz
004100050705     d CODUT           s                   like(TBLkut) inz(1)
004200050705     d COD             s                   like(TBLCOD) inz
004300050705     d KEY             s                   like(TBLKEY) inz
004400050705     d COMBLD          s              8  0 inz
004500050705     d COMTR1          s              8  0 inz
004600050705     d COMMSC          s              8  0 inz
004700050705     d COMMSS          s              8  0 inz
004800050705     d COMNDP          s              8  0 inz
004900080121     d COMpsr          s              8  0 inz
005000081114     d comsga          s              8  0 inz
005100170210     d ComMSP0         s              8  0 inz
005200170210     d ComMSP1         s              8  0 inz
005300050705     C*---------------------------------------------------------------*
005400050705     C* ACCESSO TABEL00F
005500050705     C     KTAB          KLIST
005600050705     C                   KFLD                    CODUT
005700050705     C                   KFLD                    COD
005800050705     C                   KFLD                    KEY
005900001027     C*---------------------------------------------------------------*
006000001027     C     *ENTRY        PLIST
006100001027     C                   PARM                    KPJBA
006200050705      *
006300050705     c     *dtaara       define    §azute        AZUTEds
006400050705     c     *dtaara       define    §datiute      dDATIUTE
006500050705     c                   in(E)     *dtaara
006600050705     c                   if        %ERROR or RSUT = *blanks
006700050705     c                   clear                   Tibs34Ds
006800050705     c                   call      'TIBS34R'
006900050705     c                   parm                    Tibs34Ds
007000050705     c                   in        *dtaara
007100050705     c                   endif
007200001027     C*
007300001027     C*---------------------------------------------------------------*
007400050705     ***C* CARICAMENTO TABELLE
007500050705     *** * Non carico piu' la £1 per segnalare esistenza p.o DPD
007600001027     C*
007700050829     C* reperimento data di riferimento per calcolo date di pulizia
007800001027     C*
007900050829     C                   CALL      'FNLV61R'
008000050829     C                   PARM                    O61DRF            8 0
008100050829     C                   z-add     o61drf        G02inv
008200050829     C                   MOVEL     '3'           G02ERR
008300001027     C                   CALL      'XSRDA8'
008400001027     C                   PARM                    WLBDAT
008500001027     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A
008600001027     C                   MOVEL     '5A'          COD
008700001027     C                   MOVEL     '1       '    KEY
008800001027     C     KTAB          CHAIN     TABEL                              30
008900001027     C  N30              MOVEL     TBLUNI        DS5A
009000001027     C   30              MOVEL     *ZEROS        DS5A
009100001027     C*
009200001027     C* SEGNACOLLI     :  SOTTRAGGO §5ABLD AI GIORNI
009300001027     C     G02TGI        SUB       §5ABLD        GIOTGI
009400001027     C*
009500001027     C                   CALL      'XSRGI8'
009600001027     C                   PARM                    WGIDAT
009700050705     C                   Z-ADD     GIOINV        COMBLD
009800020620      *
009900020620      * STORICO SPUNTE CML/VDL/DISK C:SOTTRAGGO §5ATR1 AI GIORNI
010000001027     C     G02TGI        SUB       §5ATR1        GIOTGI
010100020620      *
010200001027     C                   CALL      'XSRGI8'
010300001027     C                   PARM                    WGIDAT
010400050705     C                   Z-ADD     GIOINV        COMTR1
010500020620      *
010600050705      * LETTURE PACCHI TEST CML/VDL:SOTTRAGGO §5AMSC AI GIORNI
010700050705     C     G02TGI        SUB       §5AMSC        GIOTGI
010800020620      *
010900020620     C                   CALL      'XSRGI8'
011000020620     C                   PARM                    WGIDAT
011100050705     C                   Z-ADD     GIOINV        COMMSC
011200050705      *
011300050705      * Saldi Attività Smistamento: sottraggo §5AMSS ai giorni
011400050705     c     G02tgi        sub       §5AMSS        GIOtgi
011500050705     c                   call      'XSRGI8'
011600050705     c                   parm                    WGIdat
011700050705     c                   z-add     GIOinv        ComMSS
011800051007      *
011900170210     C* DATE PULIZIE: REPERISCO GIORNI DA DS5A2
012000021104     C                   CLEAR                   COMNDP
012100021104      * Li reperisco sempre
012200021104     C                   MOVEL     '5A'          COD
012300021104     C                   MOVEL     '2       '    KEY
012400021104     C     KTAB          CHAIN     TABEL                              30
012500021104     C  N30              MOVEL     TBLUNI        DS5A2
012600021104     C   30              MOVEL     *ZEROS        DS5A2
012700001027     C* NUMERATORI DPD : :SOTTRAGGO §5ANDP AI GIORNI
012800021104     C     G02TGI        SUB       §5ANDP        GIOTGI
012900021104     C*
013000021104     C                   CALL      'XSRGI8'
013100021104     C                   PARM                    WGIDAT
013200050705     C                   Z-ADD     GIOINV        COMNDP
013300080118     C* Log spunte da pistole radio sottraggo §5APSR
013400080118     C     G02TGI        SUB       §5APSR        GIOTGI
013500080118     C*
013600080118     C                   CALL      'XSRGI8'
013700080118     C                   PARM                    WGIDAT
013800080118     C                   Z-ADD     GIOINV        COMPSR
013900081114
014000081114      * Giorni pulizia file WFSGA00F stat. giornaliera anomalie
014100081114     C     G02TGI        SUB       §5ASGA        GIOTGI
014200081114     C*
014300081114     C                   CALL      'XSRGI8'
014400081114     C                   PARM                    WGIDAT
014500081114     C                   Z-ADD     GIOINV        COMSGA
014600170210      *
014700170210      * -?DATE PULIZIE: reperisco giorni da DS5A4?
014800170210     c                   movel     '5A'          COD
014900170210     c                   movel     '4       '    KEY
015000170210     c     KTAB          chain     TABEL                              30
015100170210     c  N30              movel     TBLuni        ds5A4
015200170210     c   30              movel     *zeros        ds5A4
015300170210      *
015400170210      * -?File FIMSP00F: sottaraggo §5A4MSP0 ai giorni?
015500170210     c     G02tgi        sub       §5A4MSP0      GioTGI
015600170210     c                   call      'XSRGI8'
015700170210     c                   parm                    WGIDAT
015800170210     c                   z-add     GioINV        ComMSP0
015900170210      * -?File FIMSP10F: sottaraggo §5A4MSP1 ai giorni?
016000170210     c     G02tgi        sub       §5A4MSP1      GioTGI
016100170210     c                   call      'XSRGI8'
016200170210     c                   parm                    WGIDAT
016300170210     c                   z-add     GioINV        ComMSP1
016400170210      *
016500001027     C**
016600001027     C* SPUNTE SEGNACOLLI
016700001027     C     COMBLD        IFNE      0
016800100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS79R   '
016900100505     C                   EXSR      INVIA
017000100505     c
017100001027     C                   MOVEL     COMBLD        DATPUL
017200001027     C                   MOVEL     PARAM1        KPJBU
017300001027     C                   CALL      'FNLS79R'
017400001027     C                   PARM                    KPJBA
017500100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS79R   '
017600100505     C                   EXSR      INVIA
017700100505     c
017800001027     C                   END
017900020620      * FLTR700F            - STORICO SPUNTE VDL/DISK C
018000020620     C                   IF        COMTR1 <> *zeros
018100100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRB2R   '
018200100505     C                   EXSR      INVIA
018300100505     c
018400001027     C                   MOVEL     COMTR1        DATPUL
018500001027     C                   MOVEL     PARAM1        KPJBU
018600001027     C                   CALL      'FNLRB2R'
018700001027     C                   PARM                    KPJBA
018800100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRB2R   '
018900100505     C                   EXSR      INVIA
019000100505     c
019100020620     C                   ENDIF
019200020620      * FNMSA00F            - ATTIVITA' SMISTAMENTO
019300020620      * FNMSC00F            - LETTURE PACCHI CIVETTA
019400050705      * FNMSS00F            - SALDI ATTIVITA' SMISTAMENTO
019500170210      * FIMSP00F            - Letture modalità Picking - Saldi per minuto
019600170210      * FIMSP10F            - Orari di Inizio/Fine picking
019700170210     C                   IF        COMTR1  <> *zeros  or
019800170210     C                             COMMSC  <> *zeros  or
019900170210     C                             ComMSS  <> *zeros  or
020000170210     C                             ComMSP0 <> *zeros  or
020100170210     C                             ComMSP1 <> *zeros
020200100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FIMS30R   '
020300100505     C                   EXSR      INVIA
020400170210     c*
020500020620     C                   MOVEL     COMTR1        DATPUL
020600020620     C                   MOVEL     COMMSC        DATxxx
020700050705     c                   movel     ComMSS        DatMSS
020800170210     c                   movel     ComMSP0       DatMSP0
020900170210     c                   movel     ComMSP1       DatMSP1
021000020620     C                   MOVEL     PARAM1        KPJBU
021100001027     C                   CALL      'FIMS30R'
021200001027     C                   PARM                    KPJBA
021300100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FIMS30R   '
021400100505     C                   EXSR      INVIA
021500100505     c
021600020620     C                   ENDIF
021700100505
021800001027     C* BOLLE E GIACENZE    - DATI INVIATI A CLIENTI
021900100505     c                   eval      %subst(COMSND(1):13:17)='Inizio TRTC22R   '
022000100505     C                   EXSR      INVIA
022100100505     c
022200001027     C                   CLEAR                   KPJBU
022300001027     C                   CALL      'TRTC22R'
022400001027     C                   PARM                    KPJBA
022500100505     c                   eval      %subst(COMSND(1):13:17)='Fine   TRTC22R   '
022600100505     C                   EXSR      INVIA
022700001027     C*
022800001027     C* PULIZIA NUMERATORI DPD
022900021104     C     COMNDP        IFNE      0
023000100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS42R   '
023100100505     C                   EXSR      INVIA
023200100505     c
023300001027     C                   MOVEL     COMNDP        DATPUL
023400001027     C                   MOVEL     PARAM1        KPJBU
023500001027     C                   CALL      'FNLS42R'
023600001027     C                   PARM                    KPJBA
023700100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS42R   '
023800100505     C                   EXSR      INVIA
023900100505     c
024000021104     C                   END
024100010328     C* PULIZIA FILE FNBLD00F
024200100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLS47R   '
024300100505     C                   EXSR      INVIA
024400100505     c
024500010328     C                   CALL      'FNLS47R'
024600001122     C                   PARM                    KPJBA
024700100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLS47R   '
024800100505     C                   EXSR      INVIA
024900100505     c
025000080118     c* Pulizia log spunte pistole radio
025100080118     C     COMPSR        IFNE      0
025200100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRB1R   '
025300100505     C                   EXSR      INVIA
025400100505     c
025500080118     C                   MOVEL     COMPSR        DATPUL
025600080118     C                   MOVEL     PARAM1        KPJBU
025700080118     C                   CALL      'FNLRB1R'
025800080118     C                   PARM                    KPJBA
025900100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRB1R   '
026000100505     C                   EXSR      INVIA
026100100505     c
026200080118     C                   END
026300081114
026400081114      * Pulizia WFSGA00F Statistica giornaliera anomalie
026500081114     C     COMSGA        IFNE      0
026600100505     c                   eval      %subst(COMSND(1):13:17)='Inizio FNLRE2R   '
026700100505     C                   EXSR      INVIA
026800100505     c
026900081114     C                   MOVEL     COMSGA        DATPUL
027000081114     C                   MOVEL     PARAM1        KPJBU
027100081117     C                   CALL      'FNLRE2R'
027200081114     C                   PARM                    KPJBA
027300100505     c                   eval      %subst(COMSND(1):13:17)='Fine   FNLRE2R   '
027400100505     C                   EXSR      INVIA
027500100505     c
027600081114     C                   END
027700081114
027800001027     C*
027900010402     C* L'USO DI WPJBU ERA STATO MESSO PER POTER VISUALIZZARE
028000010402     C* IN INTERROGAZIONE AZIONI ESEGUITI I PARAMETRI DI RICHIAMO
028100010402     C* A PGM FNLR10R MA NON FUNZIONA
028200001123     C                   MOVE      WPJBU         KPJBU
028300100504     C* FACCIO DSPJOBLOG
028400100504     C                   Z-ADD     70            LENGHT
028500100504     C                   MOVEL     CMDDSP(1)     COMMAN           80
028600100504     c                   Call      'QCMDEXC'                            99
028700100504     c                   Parm                    comman
028800100504     c                   Parm                    lenght           15 5
028900001027     C                   SETON                                        LR
029000100505     c*-------------------------------------------------------------------
029100100505     C     INVIA         BEGSR
029200100505     C                   Z-ADD     70            LENGHT
029300100505     C                   MOVEL     COMsnd(1)     COMMAN           80
029400100505     c                   Call      'QCMDEXC'                            99
029500100505     c                   Parm                    comman
029600100505     c                   Parm                    lenght           15 5
029700100505     C                   ENDSR
029800100504**
029900100504DSPJOBLOG OUTPUT(*PRINT)
030000100505**
030100100505SNDMSG MSG('inizio xxxxxxxxxx') TOUSR(*REQUESTER)
