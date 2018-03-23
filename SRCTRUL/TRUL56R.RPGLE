000100000614     H DECEDIT('0,') DATEDIT(*DMY.)
000200100909     H* trul56r *----------------------------------------------------*
000300100909     H*       - Scrittura bolla legata in arrivo causa ogm interrotto
000400000000     H*--------------------------------------------------------------*
000500100909     FTRUL56D   CF   E             WORKSTN
000600941228     FFNBLP01L  IF   E           K DISK
000700100909     FFiar401L  IF   E           K DISK
000800100909     FFNlbl01L  IF   E           K DISK
000900100909     FFNarb01L  IF   E           K DISK
001000030613
001100100909     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001200020628
001300100909     D kpjba         E DS
001400100909     D cncr80        E DS
001500100909     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600100909     D trul41ds      E DS
001700100909     d
001800100909     d                 ds
001900100909     dlblaap                   1      4  0
002000100909     dlblmgp                   5      8  0
002100100909     dlblamgp                  1      8  0
002200100909     d Dataiso         s               d   datfmt(*iso)
002300100909     d Dataeur         s               d   datfmt(*eur)
002400910412     C*****************************************************************
002500000613      *
002600950411     C                   CLEAR                   V1CLNP
002700100909     C                   MOVEL     *ZEROS        V1Cnrs
002800100909     C                   MOVEL     *ZEROS        V1Cnsp
002900100909     c                   clear                   fnblp000
003000100909     c                   clear                   fnlbl000
003100100909     c                   clear                   vidnot1
003200100909     c                   clear                   vidnot2
003300100909     c                   clear                   lbldat
003400950411     C                   TIME                    W0140            14 0
003500950411     C* UDATE IN GGMMAAAA
003600950411     C                   MOVE      W0140         V1CAAS
003700950411     C**
003800900509     C     FOR01         TAG
003900100909     C                   EXFMT     ul56D01
004000941227     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
004100100909     C                   SETOFF                                       902844
004200100909     C** F3 - FINE LAVORO
004300900509     C   KC              GOTO      FINE
004400910205     C* CONTROLLI FORMATO
004500910205     C                   EXSR      CONTR
004600100909     C  NKF
004700941228     COR 90              GOTO      FOR01
004800901214     C*
004900100909     c* Creo bolla richiamando pgm in arrivo
005000100909     c                   clear                   trul41ds
005100100909     C                   z-add     v1caas        D41AAS
005200100909     C                   z-add     v1clnp        d41LNP
005300100909     C                   z-add     v1cnrs        d41NRS
005400100909     C                   z-add     v1cnsp        d41NSP
005500100909     C                   CALL      'TRUL41R'
005600100909     C                   PARM                    trul41ds
005700950116     C*
005800000000     C     FINE          TAG
005900000000     C                   SETON                                        LR
006000941116     C*
006100910205     C* CONTROLLO FORMATO -------------------------------------------
006200910205     C     CONTR         BEGSR
006300910205     C                   SETOFF                                       90
006400100909     c                   clear                   fnlbl000
006500100909     c                   clear                   fnblp000
006600100909     c                   clear                   lbldat
006700050329
006800100909     c                   eval      *In44 = *Off
006900100909     c* Verifico se esiste la bolla in partenza
007000100909     c     kblp          chain     fnblp01l
007100100909     c                   if        not %found(fnblp01l)
007200100909     c                   seton                                        442890
007300100909     c                   eval      v1cmsg=msg(1)
007400100909     c                   goto      endctr
007500100909     c                   endif
007600100909     c
007700100909     c* Verifico se esiste la bolla in arrivo
007800100909     c     kblp          chain     fnarb01l
007900100909     c                   if        %found(fnarb01l)
008000100909     c                   seton                                        442890
008100100909     c                   eval      v1cmsg=msg(2)
008200100909     c                   goto      endctr
008300100909     c                   endif
008400100909     c* Verifico se spedizione legata
008500100909     c     kblp          chain     fnlbl01l
008600100909     c                   if        not %found(fnlbl01l)
008700100909     c                   seton                                        442890
008800100909     c                   eval      v1cmsg=msg(3)
008900100909     c                   goto      endctr
009000100909     c                   endif
009100100909
009200100909     c     *iso          movel     lblamgp       dataiso
009300100909     c                   eval      dataeur=dataiso
009400100909     c                   eval      lbldat=%dec(dataeur)
009500100909     c* La spedizione deve essere non consegnata e borderizzata
009600100909     c                   if        blpdcm>0
009700100909     c                   seton                                        442890
009800100909     c                   eval      v1cmsg=msg(4)
009900100909     c                   goto      endctr
010000100909     c                   endif
010100100909     c                   if        blpdbr=0
010200100909     c                   seton                                        442890
010300100909     c                   eval      v1cmsg=msg(5)
010400100909     c                   goto      endctr
010500100909     c                   endif
010600100909     c* Prendo le note
010700100909     c                   eval      ktrc='8'
010800100909     c     kbl4          chain     fiar401l
010900100909     c                   if        %found(fiar401l)
011000100909     c                   movel     ar4not        vidnot1
011100100909     c                   else
011200100909     c                   clear                   vidnot1
011300100909     c                   endif
011400100909     c                   eval      ktrc='9'
011500100909     c     kbl4          chain     fiar401l
011600100909     c                   if        %found(fiar401l)
011700100909     c                   movel     ar4not        vidnot2
011800100909     c                   else
011900100909     c                   clear                   vidnot2
012000100909     c                   endif
012100100909     c
012200910207     C*
012300910205     C     ENDCTR        ENDSR
012400910207     C*
012500941116     C* OPERAZIONI INIZIALI-------------------------------------------*
012600941116     C     *INZSR        BEGSR
012700941116     C*
012800941116     C     *ENTRY        PLIST
012900941116     C                   PARM                    KPJBA
013000050329
013100100909     C                   Z-ADD     1             CODUT             1 0
013200941227     C                   CALL      'X§PARUT'
013300941227     C                   PARM                    UT§DSE
013400941116     C                   MOVEL     RAGUT         RSUT             20
013500941227     C                   MOVEL     REC80         CNCR80
013600941116     C**
013700941116     C* KLIST
013800941116     C**
013900950419     C* ACCESSO   FNBLP01L
014000950419     C     KBLP          KLIST
014100941227     C                   KFLD                    V1CAAS
014200100909     C                   KFLD                    V1Clnp
014300100909     C                   KFLD                    v1cnrs
014400100909     C                   KFLD                    v1cnsp
014500100909     C     KBL4          KLIST
014600100909     C                   KFLD                    V1CAAS
014700100909     C                   KFLD                    V1Clnp
014800100909     C                   KFLD                    v1cnrs
014900100909     C                   KFLD                    v1cnsp
015000100909     C                   KFLD                    ktrc              1
015100941116     C*
015200000613     C                   ENDSR
015300000613**
015400100909Spedizione inesistente in Partenza
015500100909Spedizione già presente in arrivo
015600100909Spedizione non legata
015700100909La spedizione è consegnata: scrittura non ammessa
015800100909La spedizione non è ancora stata borderizzata !!
