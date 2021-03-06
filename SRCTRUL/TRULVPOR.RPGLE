000100020513      *-----------*
000200140313      *  TRULVPOR *  CARICAMENTO P.O. DALLA TABELLA VPO
000300020513      *-----------*
000400020513      *
000500020513      * INPUT
000600020513      * -----
000601140313      *  IVPOKE1  CHIAVE TABELLA 1
001100020513      *
001200020513      * OUTPUT
001300020513      * ------
001400140313      *  OVPOPOG   Schiera P.O. gestibili (250 elem.)
001500050608      *
002200020513      *--------------------------------------------------------------*
002300020513
002400041214     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
002500020513
002600140313     FTNTBE01L  IF   E           K DISK
002800020513
002900020513      *
003000020513      * === COSTANTI =============================================== *
003100020513      *
003200020513     D MaxPOG          c                   const(250)
003900020513      * === SCHIERE A TEMPO DI ESECUZIONE ========================== *
004000020513      *
004100050609      * P.O. gestibili di work e di output
004200020513     D $POG            s              3    dim(MaxPOG) inz(*zeros)
004300050609     D OPOG            s              3    dim(MaxPOG) inz(*zeros)
005300020513      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
005400020513      *
005500020513      * Passaggio Parametri
005700140313     D TRULVPODS     e ds
006700020513      *
006800020513      * === STRUTTURE DATI DEFINITE INTERNAMENTE =================== *
006801140313      * - Filiali abilitate alle telefonate
006802140313     d FilOk           ds           256
006803140313     d  skFilOk                1    255    dim(85)
006804140313      *
006900020513      *
007000020513      * === VARIABILI ============================================== *
007100020513      *
007200020513      * Indice schiere
007300020513     D XX              S              3  0 inz
007400050609     D yy              S              3  0 inz
007601140313
007602140313      * campi di comodo
007603140313     d K_cod           s                   like(Tbecod)
007604140313     d k_ke1           s                   like(Tbeke1)
008300020513
008400020513      *--------------------------------------------------------------*
008500020513      * === MAIN LINE PROGRAM ====================================== *
008600020513      *--------------------------------------------------------------*
008700020513      *
008701140313      *
008702140313      * TNTBE01L
008703140313     C     KTBE01        klist
008704140313     C                   kfld                    K_cod
008705140313     C                   kfld                    K_ke1
008706140313      *
008800020513     C     *entry        plist
009000140313     C                   parm                    TRULVPODS
009200020513      *
009300020513      * inizializzazione variabili
009400020513     C                   exsr      INZVAR
009800020513      *
009900140313      * Intabellamento P.O. DELLA TABELLA VPO
010000020513     C                   exsr      CARPOG
010100020513      *
010200020513      * Fine
010300020513     C                   EXSR      ENDPGM
010400020513
010500020513      *--------------------------------------------------------------*
010600020513      * === INIZIALIZZAZIONE VARIABILI ============================= *
010700020513      *--------------------------------------------------------------*
010800020513     C     INZVAR        BEGSR
010900020513      *
011000020513      * Impostazione iniziale dati di output
011100140313     C                   clear                   OVPOpog
011700050609     c
011800020513     C                   reset                   $POG
011801140313     C                   reset                   OPOG
012400020513     C                   reset                   XX
012401140313     C                   reset                   YY
012500020513      *
012700020513      *
012800020513     C     EndInzVar     ENDSR
012900020513
015300020513      *--------------------------------------------------------------*
015400020513      * === CARICAMENTO P.O. GESTIBILI DAL P.O. UTENTE ============= *
015500020513      *--------------------------------------------------------------*
015600020513     C     CARPOG        BEGSR
015700020513      *
015701140313     c                   eval      K_cod = 'VPO'
015702140313     c                   eval      K_ke1 = ivpoke1
015703140313     c     kTBE01        setll     tntbe01l
015704140313     c                   DO        *hival
015705140313     c     ktbe01        reade     tntbe01l
015706140313     c                   if        %eof(tntbe01l)
015707140313     c                   leave
015708140313     c                   Endif
015709140313     C                   Eval      FilOK = Tbeuni
015710140318     c                   eval      yy = 1
015711140317    3c                   do        85            yy
015712140313    4c                   if        skFilOk(yy) <='000'
015713140313     c                   leave
015714140313    4c                   endif
015715140313     c     skFilOk(yy)   lookup    opog(1)                                30
015716140313      *
015717140313    6c                   if        not %found
015718140313     c                   z-add     1             xx
015719140313     c     '000'         lookup    opog(xx)                               30
015720140318     c                   eval      opog(xx) = skFilok(yy)
015721140313    6c                   endif
015722140313      *
015723140313     c                   enddo
015724140313     c                   ENDDO
015725140313     c
019500020513     C                   ENDSR
019600020513
070300020513
070400020513      *--------------------------------------------------------------*
070500020513      * === FINE PROGRAMMA ========================================= *
070600020513      *--------------------------------------------------------------*
070700020513     C     ENDPGM        BEGSR
070800020513      *
070900020513      * Restituzione dei dati reperiti
071000140313     C                   movea     OPOG          OVPOPOG
071900020513      *
072000020513     C                   eval      *inLR  = *on
072100020513     C                   return
072200020513      *
072300020513     C                   ENDSR
072400020513
