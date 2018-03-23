000100001130      *------------------------------------------------------------------------*
000200030109      * CREAZIONE SALDI MOVIMENTI GABBIE/BANCALI
000300001130      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500030109     F*------------------------------------------------------------------------*
000600030109     F* DATA BASE
000700030109     F*------------------------------------------------------------------------*
000800000000     D*------------------------------------------------------------------------*
000900000000     D* INPUT
001000000000     D*------------------------------------------------------------------------*
001100000000     D*-------------------
001200000000     D* ARCHITETTURA
001300000000     D*-------------------
001400000000     D KPJBA         E DS
001500030122     D*-------------------
001600030122     D* DS DI PROCEDURA
001700030122     D*-------------------
001800030122     D TISIG0DS      E DS
001900000000     D*-------------------
002000000000     D* DS PARAMETRI DI LANCIO PGM
002100000000     D*-------------------
002200000000     D PARAM           DS
002300071217     D  PARDAF                        8  0
002400071217     D  PARDLT                        1
002500000914     D*-------------------
002600000914     D* DS DI LETTURA RECORDSET VIA SQL
002700000914     D*-------------------
002800030109     D DSSALDI         DS
002900030109     D  W_TIP                         1
003000030109     D  W_SOG                         8
003100030109     D  W_COD                        15
003200030109     D  W_QTA                        30  3
003300030109     D*-------------------
003400030109     D* DS REPERIMENTO DATI UTENTE
003500030109     D*-------------------
003600030109     D TIBS34DS      E DS                                                       *Profili utente
003700030109     D DDATIUTE      E DS                                                       *Dati utente
003800030109     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003900000914     D*------------------------------------------------------------------------*
004000000918     D* VARIABILI DI WRK
004100000914     D*------------------------------------------------------------------------*
004200030122     D wrkNum          S              1  0 INZ(1)
004300030122     D wrkDiv          S              1  0 INZ(0)
004400030122     D W_CAU           S              4
004500071217     D wMGBSOG         S              8    INZ
004600071217     D wMAXDATSALDO    S              8  0 INZ
004700071217     D wMAXDATMOV      S              8  0 INZ
004800000921     D*------------------
004900000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000000921     D*------------------
005100000921     D WLBDA8          DS                  INZ
005200000921     D  G08DAT                 1      8  0
005300000921     D  G08INV                 9     16  0
005400000921     D  G08ERR                17     17
005500000921     D  G08TGI                18     22  0
005600000000     C*------------------------------------------------------------------------*
005700000000     C* MAIN LINE
005800000000     C*------------------------------------------------------------------------*
005900071217     C*
006000071217     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006100071217     C
006200071217     C/EXEC SQL
006300071217     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006400071217     C/END-EXEC
006500000000     C*
006600071217     C*
006700071217     C* Routine d saldificazione
006800000920     C                   exsr      procedi
006900071217     C*
007000071217     C* Routine d eliminazione record clienti senza movimenti + recenti dell'ultimo saldo
007100071217     C                   if        PARDLT = 'S'
007200071217     C                   exsr      procedi02
007300071217     C                   endif
007400000914     C*
007500000914     C                   SETON                                        LR
007600000914     C*------------------------------------------------------------------------*
007700071217     C* PROCEDI - Routine di elaborazione saldificazione
007800000914     C*------------------------------------------------------------------------*
007900000914     C     PROCEDI       BEGSR
008000000920     C*
008100030109     C* Raggruppo i movimenti del periodo richiesto per tipo sogg/sogg/articolo
008200030109     C*
008300000914     C/EXEC SQL
008400010104     C+ declare C1 cursor for
008500030109     C+ select mgbtip, mgbsog, mgbcod, sum(mgbqta) from azmgb00f
008600030109     C+ where mgbdat <= :PARDAF and mgbtim <> 'S'
008700030109     C+ group by mgbtip, mgbsog, mgbcod
008800010104     C+ for fetch only
008900000914     C/END-EXEC
009000000914     C
009100000914     C/EXEC SQL
009200000914     C+ open C1
009300000914     C/END-EXEC
009400000914     C
009500000914     C/EXEC SQL
009600030109     C+ Fetch C1 into :dssaldi
009700000914     C/END-EXEC
009800000914     C*
009900001130     C                   dow       sqlcod = *zeros
010000020704     C*
010100000914     C                   exsr      creare
010200000914     C*
010300000914     C/EXEC SQL
010400030109     C+ Fetch C1 into :dssaldi
010500000914     C/END-EXEC
010600000914     C*
010700001130     C                   enddo
010800000914     C*
010900000914     C/EXEC SQL
011000000914     C+ close C1
011100000914     C/END-EXEC
011200030109     C*
011300071217     C*
011400071217     C* Prima d eliminare salvo i record saldificati
011500071217     C*
011600071217     C/EXEC SQL
011700071217     C+ insert into azmgb00s
011800071217     C+ (select * from azmgb00f
011900071217     C+ where mgbdat <= :PARDAF
012000071217     C+ and mgbtim <> 'S')
012100071217     C/END-EXEC
012200030109     C*
012300071217     C* Elimino i record saldificati
012400071217     C*
012500071217     C/EXEC SQL
012600071217     C+ delete from azmgb00f
012700071217     C+ where mgbdat <= :PARDAF
012800071217     C+ and mgbtim <> 'S'
012900071217     C/END-EXEC
013000071217     C*
013100000920     C*
013200000914     C                   ENDSR
013300071217     C*------------------------------------------------------------------------*
013400071217     C* PROCEDI02 - Routine di elaborazione cancellazione record clienti senza movimenti + recenti
013500071217     C*             rispetto all'ultimo saldo
013600071217     C*------------------------------------------------------------------------*
013700071217     C     PROCEDI02     BEGSR
013800071217     C*
013900071217     C* Raggruppo i movimenti del periodo richiesto per tipo sogg/sogg/articolo
014000071217     C*
014100071217     C/EXEC SQL
014200071217     C+ declare C2 cursor for
014300071217     C+ select distinct mgbsog from azmgb00f
014400071217     C+ where mgbtip = 'C'
014500071217     C+ for fetch only
014600071217     C/END-EXEC
014700071217     C
014800071217     C/EXEC SQL
014900071217     C+ open C2
015000071217     C/END-EXEC
015100071217     C
015200071217     C/EXEC SQL
015300071217     C+ Fetch C2 into :wMGBSOG
015400071217     C/END-EXEC
015500071217     C*
015600071217     C                   dow       sqlcod = *zeros
015700071217     C*
015800071217     C* Inizializzo sempre le date di confronto
015900071217     C                   z-add     *zeros        wMAXDATSALDO
016000071217     C                   z-add     *zeros        wMAXDATMOV
016100071217     C*
016200071217     C/EXEC SQL
016300071217     C+ select max(mgbdat)
016400071217     C+ into :wMAXDATSALDO
016500071217     C+ from azmgb00f
016600071217     C+ where mgbtip = 'C' and mgbtim = 'S' and
016700071217     C+ mgbsog = :wMGBSOG
016800071217     C/END-EXEC
016900071217     C*
017000071217     C*
017100071217     C/EXEC SQL
017200071217     C+ select max(mgbdat)
017300071217     C+ into :wMAXDATMOV
017400071217     C+ from azmgb00f
017500071217     C+ where mgbtip = 'C' and mgbtim <> 'S' and
017600071217     C+ mgbsog = :wMGBSOG
017700071217     C/END-EXEC
017800071217     C*
017900071217     C* Confronto le date: se data MOV >= data SALDO => tengo i record cliente, altrimenti elimino
018000071217     C                   if        wMAXDATMOV >= wMAXDATSALDO
018100071217     C                   else
018200071217     C*
018300071217     C* Prima d eliminare salvo TUTTI i record clienti
018400071217     C*
018500071217     C/EXEC SQL
018600071217     C+ insert into azmgb00s
018700071217     C+ (select * from azmgb00f
018800071217     C+ where mgbtip = 'C' and mgbsog = :wMGBSOG)
018900071217     C/END-EXEC
019000071217     C*
019100071217     C* Elimino TUTTI i record (e movimento e saldi) del cliente corrente
019200071217     C*
019300071217     C/EXEC SQL
019400071217     C+ delete from azmgb00f
019500071217     C+ where mgbtip = 'C' and mgbsog = :wMGBSOG
019600071217     C/END-EXEC
019700071217     C*
019800071217     C                   endif
019900071217     C*
020000071217     C*
020100071217     C/EXEC SQL
020200071217     C+ Fetch C2 into :wMGBSOG
020300071217     C/END-EXEC
020400071217     C*
020500071217     C                   enddo
020600071217     C*
020700071217     C/EXEC SQL
020800071217     C+ close C2
020900071217     C/END-EXEC
021000071217     C*
021100071217     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
021400000000     C*------------------------------------------------------------------------*
021500000000     C     CREARE        BEGSR
021600030122     C*
021700030122     C* Prima effettuo alcune considerazioni sul tipo soggetto da cui dipende la causale di saldo.
021800030122     C                   if        W_TIP = 'C'
021900030122     C                   eval      W_CAU = 'SALC'
022000030122     C                   endif
022100030122     C                   if        W_TIP = 'D'
022200030122     C                   eval      W_CAU = 'SALD'
022300030122     C                   endif
022400030122     C                   if        W_TIP = 'F'
022500030122     C                   eval      W_CAU = 'SALF'
022600030122     C                   endif
022700030122     C*
022800030122     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento saldo
022900030122     C                   CLEAR                   TISIG0DS
023000060130     C                   Z-ADD     PARDAF        IG0DAT
023100060130     C                   Z-ADD     235900        IG0ORA
023200030122     C                   MOVEL     DUTUTE        IG0UTE
023300030122     C                   Z-ADD     DUTPOU        IG0POU
023400030122     C                   MOVEL     W_CAU         IG0CAU
023500030122     C                   MOVEL     W_SOG         IG0SOG
023600030122     C                   MOVEL     W_COD         IG0COD
023700030122     C                   Z-ADD     W_QTA         IG0QTA
023800030127     C                   EVAL      IG0RDO = 'Saldo al:'
023900060130     C                   Z-ADD     PARDAF        IG0DDO
024000030122     C*
024100030122     C* Lancio del "driver"
024200030122     C                   CALL      'TISIG5R'
024201150227     C                   PARM                    KPJBA
024300030122     C                   PARM                    TISIG0DS
024400030122     C*
024500030122     C* BRUTTO ma se errore in creazione movimento di saldo preferisco far spaccare il pgm
024600030122     C                   IF        OG0ERR <> ' '
024700030122     C                   EVAL      wrkNum = wrkNum / wrkDiv
024800030122     C                   ENDIF
024900000914     C*
025000000000     C                   ENDSR
025100030109     C*--------------------------------------------------------------------------------------------*
025200030109     C* REPERISCE I DATI UTENTE
025300030109     C*--------------------------------------------------------------------------------------------*
025400030109     C     REPDATIUTE    BEGSR
025500030109     C*
025600030109     C* INIZIALIZZA VARIABILI DI WRK
025700030109     C                   CLEAR                   TIBS34DS
025800030109     C                   CLEAR                   AZUTEDS
025900030109     C                   CLEAR                   DDATIUTE
026000030109     C*
026100030109     C     *DTAARA       DEFINE    §azute        azuteds
026200030109     C     *DTAARA       DEFINE    §datiute      ddatiute
026300030109     C                   IN(E)     *DTAARA
026400030109     C                   IF        %Error
026500030109     C                   EVAL      I34Tla = 'L'
026600030109     C                   CALL      'TIBS34R'
026700030109     C                   PARM                    Tibs34Ds
026800030109     C                   IN        *DTAARA
026900030109     C                   ENDIF
027000030109     C*
027100030109     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
027200030109     C                   MOVEL(P)  RSUT          DSFIRS           20
027300030109     C*
027400030109     C                   ENDSR
027500000000     C*------------------------------------------------------------------------*
027600000000     C* *INZSR - ROUTINE INIZIALE
027700000000     C*------------------------------------------------------------------------*
027800000000     C     *INZSR        BEGSR
027900030109     C*
028000000000     C* RICEVIMENTO PARAMETRI
028100000000     C     *ENTRY        PLIST
028200000000     C                   PARM                    KPJBA
028300000000     C                   MOVEL     KPJBU         PARAM
028400030109     C*
028500030109     C* REPERISCE I DATI DELL'UTENTE
028600030109     C                   EXSR      REPDATIUTE
028700030109     C*
028800000921     C* CALCOLA LA DATA CORRENTE
028900000921     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
029000000921     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
029100030109     C                   MOVEL     WN14          ORACOR            6 0          *ORA  (8) IN H/M/SS
029200000921     C                   Z-ADD     WN8           G08DAT
029300000921     C                   Z-ADD     *ZEROS        G08INV
029400000921     C                   MOVEL     '0'           G08ERR
029500000921     C                   CALL      'XSRDA8'
029600000921     C                   PARM                    WLBDA8
029700000921     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
029800000000     C*
029900000000     C                   ENDSR
