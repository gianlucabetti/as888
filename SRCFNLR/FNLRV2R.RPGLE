000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200040607     H* FNLRV2R *-----------------------------------------------------*
000300040607     H*   Controllo colli di valore al 2 livello in arrivo
000400930126     H*---------------------------------------------------------------*
000500160920     Fazorg01l  IF   E           K DISK
000600040607     FFNLRv2d   CF   E             WORKSTN
000700921109     D*
000800040607     D* passaggio dati a batch - FNLRV3R -
000900951114     D*
001000040607     D PARA02          DS
001100040607     D  P02dam                 1      8  0
001200040607     D  P02fgs                 9     11  0
001300040607     D  P02invdam             12     19  0
001400160920     D* TIPO zone consegna
001500160920     D  P02Tpm1               20     21
001600160920     D  p02Tpm2               22     23
001700951115     D*
001800911010     D WLBDAT          DS
001900941221     D  G02DAT                 1      8  0
002000941221     D  G02INV                 9     16  0
002100941221     D  G02ERR                17     17
002200941221     D  G02TGI                18     22  0
002300160920     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MESSAGGI ERRORE
002400040604     D CNCR80        E DS
002500040604     D KPJBA         E DS
002600030722     D UT§DSE0F      E DS
002700160920     D og150         E DS
002800000000     C*---------------------------------------------------------------*
002900911010     C* INDICATORI
003000911010     C*---------------------------------------------------------------*
003100951115     C*  28    - EMETTE CAMPO MESSAGGI
003200040607     C*  40    - ERRORE
003300911010     C*  90    - ERRORE GENERICO
003400911010     C*---------------------------------------------------------------*
003500000000     C     *ENTRY        PLIST
003600000000     C                   PARM                    KPJBA
003700941221     C*
003800000000     C                   Z-ADD     1             CODUT
003900911010     C                   CALL      'X§PARUT'
004000030722     C                   PARM                    UT§DSE0F
004100920901     C                   MOVEL     REC80         CNCR80
004200941221     C                   MOVEL     RAGUT         RSUT
004300900511     C*---------------------------------------------------------------*
004400911008     C                   EXSR      PULIZ
004500971223     C*
004600921123     C     FOR01         TAG
004700911008     C*
004800040607     C                   EXFMT     LRv2D01
004900040607     C*
005000040607     C                   SETOFF                                       409028
005100030722     C** F3 - FINE LAVORO
005200040604     C   KC              GOTO      FINE
005300921117     C*
005400040528     C* CONTROLLO FOGLIO ARRIVI
005500911008     C                   EXSR      CONTR1
005600930127     C* SE ERRORE
005700040607     c  nkf
005800040607     Cor 90              GOTO      FOR01
005900040528     c
006000030722     C* F6  - ELABORAZIONE
006100040607     c                   if        *inkf
006200040607     C                   eval      p02dam=v1cdam
006300040607     C                   eval      p02invdam=g02inv
006400160920     c                   if        *in02 or *in06
006500160920     c                   eval      p02tpm1=vidtpm
006600160920     c                   eval      p02tpm2=vidtpm2
006700160920     c                   else
006800160920     c                   clear                   p02tpm1
006900160920     c                   clear                   p02tpm2
007000160920     c                   endif
007100040607     C                   MOVEL     para02        KPJBU
007200040607     C                   MOVEL     'LRV3'        KCOAZ
007300040607     C                   CALL      'BCH10'
007400040607     C                   PARM                    KPJBA
007500040604     c                   endif
007600040604     c*
007700000000     C     FINE          TAG
007800960202     C*
007900000000     C                   SETON                                        LR
008000941216     C**************************************************************************
008100941216     C*    PULIZIA CAMPI
008200941216     C**************************************************************************
008300911008     C     PULIZ         BEGSR
008400941216     C*
008500920709     C* PRIMA VIDEATA
008600040607     C                   Z-ADD     SIMPOU        p02FGS
008700040607     C                   TIME                    w0140            14 0          ORA E DATA
008800040607     C                   MOVE      w0140         v1cdam                         GG/MM/AAAA
008900040528     c
009000040604     C                   TIME                    UTIME             6 0          ORA E DATA
009100160920
009200160920     c* Vedo se p.o. gestione abilitato al piking messaggerie
009300160920     c     simpou        chain     azorg01l
009400160920    2c                   if        %found(azorg01l)
009500160920     c                   movel     orgdf0        og150
009600160920    3c                   if        §ogpkm='S'
009700160920     c                   seton                                        02
009800160920    3C                   END
009900160920     c                   if        §ogturno='S'
010000160920     c                   seton                                        06
010100160920    3C                   END
010200160920     c
010300160920    2C                   END
010400040607     c*
010500911008     C                   ENDSR
010600960205     C**************************************************************************
010700040607     C*    CONTROLLI data arrivo  colli
010800960205     C**************************************************************************
010900960205     C     CONTR1        BEGSR
011000040607     C* data arrivo
011100040607     C*
011200040607     C                   Z-ADD     V1cdam        G02DAT
011300040607     C                   CLEAR                   G02ERR
011400040607     C                   CALL      'XSRDA8'
011500040607     C                   PARM                    WLBDAT
011600040607     C*
011700040607     C* Data errata
011800040607    1C     G02ERR        IFEQ      '1'
011900040607     C                   MOVEL     msg(1)        VIDMSG
012000040607     C                   SETON                                        409028
012100160920     c                   leavesr
012200040607     c                   else
012300040607     c                   z-add     g02dat        v1cdam
012400040607    1C                   ENDIF
012500160920
012600160920     c                   if        not *in06  and *in02
012700160920     c                   if        vidtpm='P1' or vidtpm='P2' or
012800160920     c                             vidtpm2='P1' or vidtpm2='P2'
012900160920     C                   SETON                                        902844
013000160920     C                   MOVEA     MSG(2)        vidmsg
013100160920     C                   leavesr
013200160920     c                   endif
013300160920     c                   endif
013400160920     c                   if        *in06  and not *in02
013500160920     c                   if        vidtpm='P ' or vidtpm='M ' or
013600160920     c                             vidtpm2='P ' or vidtpm2='M '
013700160920     C                   SETON                                        902844
013800160920     C                   MOVEA     MSG(2)        vidmsg
013900160920     C                   leavesr
014000160920     c                   endif
014100160920     c                   endif
014200160920     c* non posso mai accettare P +P1 o P+P2
014300160920     c                   if        vidtpm='P ' or  vidtpm2='P '
014400160920     c                   if        vidtpm='P1' or vidtpm='P2' or
014500160920     c                             vidtpm2='P1' or vidtpm2='P2'
014600160920     C                   SETON                                        902844
014700160920     C                   MOVEA     MSG(3)        vidmsg
014800160920     C                   leavesr
014900160920     c                   endif
015000160920     c                   endif
015100040607     C                   ENDSR
015200000606**  MSG
015300040607Data arrivo colli errata                                                      1
015400160920Tipo scelta zone di consegna non prevista                                          7
015500160920Scelta tipi  zone di consegna non corretta                                         8
